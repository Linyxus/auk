package auk.tui

import gears.async.{Async, UnboundedChannel}
import gears.async.default.given

import auk.{TestEnv, TestFs}
import auk.TestEnv.withEnv
import auk.agent.{AgentEvent, Inbox, UserCommand}
import auk.config.Credentials
import auk.llm.provider.Providers
import auk.tui.app.Cmd

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

  test("the custom-provider wizard: kind → URL → model → key, saved and switched onto") {
    val home = TestFs.tempDir("auk-login-home")
    withEnv((noKeys ++ Seq("HOME" -> Some(home), Credentials.NoKeysEnv -> None, "CUSTOM_API_KEY" -> None))*):
      Credentials.invalidate()
      try
        val (app, commands) = newApp(keyless = true)
        val st0 = ChatState.initial
          .copy(keyless = true, provider = "ZAI", modelId = "glm-5.2")
          .showLoginPicker
        // The add row sits one past the catalog (three builtins, no custom yet).
        val addIdx = 3
        val moved = (0 until addIdx).foldLeft(st0)((s, _) => app.update(Event.LoginPickerMove(1), s)._1)
        assertEquals(moved.overlay, Overlay.LoginPicker(addIdx))
        val (k0, _) = app.update(Event.LoginProviderSelected, moved)
        assertEquals(k0.overlay, Overlay.LoginCustomKind(0)) // anthropic, the recommended default
        val (u0, _) = app.update(Event.LoginProviderSelected, k0)
        assertEquals(u0.overlay, Overlay.LoginCustomUrl("anthropic", ""))
        val (u1, _) = app.update(Event.LoginInput("https://api.example.com/anthropic"), u0)
        val (m0, _) = app.update(Event.LoginSubmit, u1)
        assertEquals(m0.overlay, Overlay.LoginCustomModel("anthropic", "https://api.example.com/anthropic", ""))
        val (m1, _) = app.update(Event.LoginInput("glm-5.2"), m0)
        val (key0, _) = app.update(Event.LoginSubmit, m1)
        assertEquals(
          key0.overlay,
          Overlay.LoginCustomKey("anthropic", "https://api.example.com/anthropic", "glm-5.2", "")
        )
        // Esc walks back a step with the gathered inputs intact.
        val (backM, _) = app.update(Event.LoginBack, key0)
        assertEquals(backM.overlay, Overlay.LoginCustomModel("anthropic", "https://api.example.com/anthropic", "glm-5.2"))
        val (key1, _) = app.update(Event.LoginSubmit, backM)
        val (key2, _) = app.update(Event.LoginInput("sk-custom-1"), key1)
        val (done, cmd) = app.update(Event.LoginSubmit, key2)
        assertEquals(done.overlay, Overlay.None)
        // The provider is now in the catalog, keyed, with its one model.
        assertEquals(Providers.all.map(_.name), List("ZAI", "Kimi", "OpenRouter", "Custom"))
        assert(
          ChatApp.availableChoices.exists(c => c.providerName == "Custom" && c.modelId == "glm-5.2"),
          ChatApp.availableChoices.toString
        )
        cmd match
          case Cmd.Fire(effect) =>
            effect()
            Async.fromSync:
              commands.read() match
                case Right(UserCommand.SwitchModel(p, m)) =>
                  assertEquals(p, "custom")
                  assertEquals(m, "glm-5.2")
                case other => fail(s"expected SwitchModel, got $other")
          case other => fail(s"expected a fired switch, got $other")
      finally Credentials.invalidate()
  }

  test("submitting an empty key does nothing") {
    val (app, _) = newApp()
    val st0 = ChatState.initial.copy(overlay = Overlay.LoginEntry("ZAI", ""))
    val (st1, cmd) = app.update(Event.LoginSubmit, st0)
    assertEquals(st1.overlay, Overlay.LoginEntry("ZAI", ""))
    assertEquals(cmd, Cmd.None)
  }
