package auk.webui

import com.raquo.laminar.api.L.*

/** The thin Laminar binding: a pure `View -> HtmlElement` mapping, swapped
  * wholesale whenever the view signal changes. No branching logic lives here —
  * every decision is already a field on the [[View]] — so it needs no unit tests
  * beyond the pure view-model tests; it is verified visually via `startTestWebUI`.
  */
object WorkflowRender:

  /** The app root: re-renders the whole view subtree on each state change. */
  def app(view: Signal[View], onSelect: String => Unit): HtmlElement =
    div(cls := "app", child <-- view.map(v => renderView(v, onSelect)))

  private def renderView(v: View, onSelect: String => Unit): HtmlElement = v match
    case View.Empty(conn) =>
      div(cls := "empty", connBadge(conn), span(cls := "hint", "Waiting for a workflow…"))
    case View.Forest(fv, conn) =>
      div(cls := "forest",
        div(cls := "topbar", connBadge(conn), div(cls := "tabs", fv.tabs.map(renderTab(_, onSelect)))),
        div(cls := "sections", fv.sections.map(renderSection)),
        renderLogs(fv.logs)
      )

  private def renderTab(t: RunTab, onSelect: String => Unit): HtmlElement =
    div(
      cls := (if t.selected then "tab tab-selected" else "tab"),
      t.label,
      onClick --> (_ => onSelect(t.runId))
    )

  private def renderSection(s: GroupSection): HtmlElement =
    div(cls := "group",
      s.name.map(n => div(cls := "group-head", n)).getOrElse(emptyNode),
      div(cls := "nodes", s.nodes.map(renderNode))
    )

  private def renderNode(r: NodeRow): HtmlElement =
    div(
      cls := s"node ${StatusKind.cssClass(r.statusKind)}",
      dataAttr("node-id") := r.id,
      dataAttr("status") := StatusKind.name(r.statusKind),
      span(cls := "glyph", r.glyph),
      span(cls := "node-name", r.id),
      if r.tokensText.nonEmpty then span(cls := "tokens", r.tokensText) else emptyNode,
      if r.toolText.nonEmpty then span(cls := "tool", r.toolText) else emptyNode,
      r.summary.map(s => span(cls := "summary", s)).getOrElse(emptyNode)
    )

  private def renderLogs(logs: Vector[String]): HtmlElement =
    if logs.isEmpty then div(cls := "logs logs-empty")
    else div(cls := "logs", div(cls := "logs-head", "log"), logs.map(l => div(cls := "log", l)))

  private def connBadge(c: ConnStatus): HtmlElement =
    val (label, klass) = c match
      case ConnStatus.Connecting => ("connecting", "conn-connecting")
      case ConnStatus.Open       => ("live", "conn-open")
      case ConnStatus.Closed     => ("closed", "conn-closed")
      case ConnStatus.Error(m)   => (s"error: $m", "conn-error")
    div(cls := s"conn $klass", label)
