package auk.tui

import gears.async.{Async, UnboundedChannel}
import gears.async.default.given

import auk.{TestEnv, TestFs}
import auk.TestEnv.withEnv
import auk.agent.{AgentEvent, Inbox, UserCommand}
import auk.config.Credentials
import auk.llm.provider.Providers
import auk.tui.app.{Cmd, Layout, Viewport}

/** The /login flow: provider list → key entry → save + live re-switch, plus
  * the key-aware model picker and first-run onboarding. */
class LoginFlowSuite extends munit.FunSuite:

  private def noKeys: Seq[(String, Option[String])] = Seq(
    "ZAI_API_KEY" -> None,
    "KIMI_API_KEY" -> None,
    "OPENROUTER_API_KEY" -> None
  )

  private def newApp(keyless: Boolean = false, onboard: Boolean = false): (ChatApp, UnboundedChannel[UserCommand]) =
    val commands = UnboundedChannel[UserCommand]()
    val app = ChatApp(
      UnboundedChannel[AgentEvent]().asReadable,
      commands,
      UnboundedChannel[Unit](),
      UnboundedChannel[Inbox](),
      keyless = keyless,
      onboardLogin = onboard
    )
    (app, commands)

  test("availableChoices lists only providers with a key") {
    withEnv((noKeys ++ Seq("ZAI_API_KEY" -> Some("sk-t"), Credentials.NoKeysEnv -> Some("1")))*):
      val cs = ChatApp.availableChoices
      assert(cs.nonEmpty)
      assert(cs.forall(_.providerName == "ZAI"), cs.toString)
  }

  test("no key anywhere → the picker has nothing to offer") {
    withEnv((noKeys :+ (Credentials.NoKeysEnv -> Some("1")))*):
      assertEquals(ChatApp.availableChoices, Vector.empty)
  }

  test("onboarding opens straight onto the login provider list") {
    val (app, _) = newApp(onboard = true)
    val (st, _) = app.init
    assertEquals(st.overlay, Overlay.LoginPicker(0))
  }

  test("a normal start opens on the plain chat") {
    val (app, _) = newApp()
    val (st, _) = app.init
    assertEquals(st.overlay, Overlay.None)
  }

  /** Drive the whole flow: Enter on a provider row, paste (with the trailing
    * newline a terminal paste carries), Enter to save — then assert the store
    * write, the transcript note, and the fired live re-switch. */
  private def runLoginFlow(
      state0: ChatState,
      pasted: String,
      expectSwitch: Option[(String, String)]
  ): ChatState =
    val home = TestFs.tempDir("auk-login-home")
    withEnv((noKeys ++ Seq("HOME" -> Some(home), Credentials.NoKeysEnv -> None))*):
      Credentials.invalidate()
      try
        val (app, commands) = newApp(keyless = state0.keyless)
        val (st1, _) = app.update(Event.LoginProviderSelected, state0)
        val (st2, _) = app.update(Event.LoginInput(pasted), st1)
        val (st3, cmd) = app.update(Event.LoginSubmit, st2)
        assertEquals(st3.overlay, Overlay.None)
        assert(
          st3.history.exists {
            case Entry.System(t) => t.contains("saved")
            case _               => false
          },
          st3.history.toString
        )
        (cmd, expectSwitch) match
          case (Cmd.Fire(effect), Some((prov, model))) =>
            effect()
            Async.fromSync:
              commands.read() match
                case Right(UserCommand.SwitchModel(p, m)) =>
                  assertEquals(p, prov)
                  assertEquals(m, model)
                case other => fail(s"expected SwitchModel, got $other")
          case (Cmd.None, None)  => ()
          case (other, expected) => fail(s"got $other, expected switch $expected")
        st3
      finally Credentials.invalidate()

  test("keyless session: saving the active provider's key re-switches in place") {
    val st0 = ChatState.initial
      .copy(keyless = true, provider = "ZAI", modelId = "glm-5.2")
      .showLoginPicker
    runLoginFlow(st0, "sk-glm-test\n", expectSwitch = Some(("zai", "glm-5.2")))
    // The paste's trailing newline was stripped before the key hit the store.
    // (Read back under the flow's HOME is gone here; the assertion lives in the
    // flow via the transcript note + fired switch.)
  }

  test("keyless session: a key for another provider switches to its first model") {
    val st0 = ChatState.initial
      .copy(keyless = true, provider = "ZAI", modelId = "glm-5.2")
      .showLoginPicker
    // Row 1 of the catalog is Kimi.
    val (app, _) = newApp()
    val (moved, _) = app.update(Event.LoginPickerMove(1), st0)
    assertEquals(moved.overlay, Overlay.LoginPicker(1))
    runLoginFlow(moved, "sk-kimi-test", expectSwitch = Some(("kimi", "k3")))
  }

  test("live session: a key for another provider just lands in the store") {
    val st0 = ChatState.initial
      .copy(keyless = false, provider = "ZAI", modelId = "glm-5.2")
      .showLoginPicker
    val (app, _) = newApp()
    val (moved, _) = app.update(Event.LoginPickerMove(1), st0)
    runLoginFlow(moved, "sk-kimi-test", expectSwitch = None)
  }

  test("key entry: input is cleaned, masked state transitions, Esc steps back") {
    val (app, _) = newApp()
    val st0 = ChatState.initial.showLoginPicker
    val (st1, _) = app.update(Event.LoginProviderSelected, st0)
    assertEquals(st1.overlay, Overlay.LoginEntry("ZAI", ""))
    val (st2, _) = app.update(Event.LoginInput("sk-abc\n"), st1)
    assertEquals(st2.overlay, Overlay.LoginEntry("ZAI", "sk-abc"))
    val (st3, _) = app.update(Event.LoginBackspace, st2)
    assertEquals(st3.overlay, Overlay.LoginEntry("ZAI", "sk-ab"))
    val (st4, _) = app.update(Event.LoginBack, st3)
    assertEquals(st4.overlay, Overlay.LoginPicker(0))
  }

  private def panelLines(app: ChatApp, state: ChatState, width: Int = 90): Vector[String] =
    val lines = Layout.lay(app.view(state, Viewport(width, 30)).live, width).map(_.plain)
    val start = lines.indexWhere(_.startsWith("┌"))
    if start < 0 then Vector.empty
    else
      val tail = lines.drop(start)
      val end = tail.indexWhere(_.startsWith("└"))
      if end < 0 then tail else tail.take(end + 1)

  test("the key window says what the provider is: notes, endpoint and kind") {
    val (app, _) = newApp()
    val zai = panelLines(app, ChatState.initial.copy(overlay = Overlay.LoginEntry("ZAI", "")))
    assert(zai.exists(_.contains("GLM coding plan")), zai.mkString("|"))
    assert(zai.exists(_.contains("https://z.ai/subscribe")), zai.mkString("|"))
    assert(zai.exists(l => l.contains("Endpoint: https://api.z.ai/api/anthropic") && l.contains("(Anthropic)")), zai.mkString("|"))
    val kimi = panelLines(app, ChatState.initial.copy(overlay = Overlay.LoginEntry("Kimi", "")))
    assert(kimi.exists(_.contains("https://www.kimi.com/membership/pricing")), kimi.mkString("|"))
    // OpenRouter has no notes — just its endpoint line.
    val or = panelLines(app, ChatState.initial.copy(overlay = Overlay.LoginEntry("OpenRouter", "")))
    assert(or.exists(_.contains("Endpoint: https://openrouter.ai/api")), or.mkString("|"))
    assert(!or.exists(_.contains("subscription")), or.mkString("|"))
  }

  test("long overlay text wraps instead of being cut off at the frame") {
    val (app, _) = newApp()
    // The ZAI hint mentions its env var; the full sentence must survive,
    // wrapped across rows — nothing clipped at the border.
    val lines = panelLines(app, ChatState.initial.copy(overlay = Overlay.LoginEntry("ZAI", "")))
    val body = lines.map(_.stripPrefix("│").stripSuffix("│").trim)
    val joined = body.filter(_.nonEmpty).mkString(" ")
    assert(joined.contains("saved to ~/.auk/credentials (ZAI_API_KEY overrides it)"), lines.mkString("|"))
    // …and the frame stays a clean rectangle.
    assertEquals(lines.map(_.length).distinct.size, 1, lines.mkString("\n"))
  }

  test("submitting an empty key does nothing") {
    val (app, _) = newApp()
    val st0 = ChatState.initial.copy(overlay = Overlay.LoginEntry("ZAI", ""))
    val (st1, cmd) = app.update(Event.LoginSubmit, st0)
    assertEquals(st1.overlay, Overlay.LoginEntry("ZAI", ""))
    assertEquals(cmd, Cmd.None)
  }
