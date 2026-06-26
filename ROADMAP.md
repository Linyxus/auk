# Auk 🐧 — Roadmap

> **Progress:** 6/6 core tools implemented · 4/4 LLM providers integrated · 165+ tests passing

---

## UI/UX

- [ ] Display diff for "Edit" actions
- [ ] Tweak up/down-arrow's semantics for multiline inputs
- [ ] Allow input while agent is running, just disable sending
- [x] Chat transcript with You/Auk headers
- [x] Streaming text, thinking blocks, and tool call labels
- [x] Line editor (insert, delete, arrows, Ctrl+A/E/K/U/W, Home/End)
- [x] Input history (Up/Down arrows, draft stashing)
- [x] Spinner animation while waiting
- [x] Flicker-free rendering (DEC sync sequence)
- [x] Quit via Ctrl+Q
- [x] Tool-run progress events
- [x] Splitting line before and after the input box
- [x] Splitting line before and after each user message box
- [x] Repair scrolling
- [x] Multi-line input (Shift+Enter or similar)
- [x] Rewrite layoutz on our own for maximal performance — `auk.tui.render` (per-cell diff, static/live split, DEC-2026 atomic writes) + `auk.tui.app` (gears-based Elm runtime, DSL, layout)

## Configuration

- [x] Make things configurable

## Agent Engine

- [ ] Real-time steering
- [ ] Support compaction
- [ ] Parallel tool-calling
- [x] **Turn loop** with tool-use round-tripping (up to 8 rounds)
- [x] **Concurrent tool execution** via `Future`
- [x] **Streaming support** — live deltas from the LLM
- [x] **Tool progress events** (`ToolRunStart`/`ToolRunEnd`) for UI feedback
- [x] **Sub-agent delegation** — nested headless agent (non-recursive, up to 16 rounds)
- [x] **Token usage aggregation** across sub-agents
- [x] Setup the "event"-based context infrastructure
- [x] Refactor SubAgent tool to use the engine too: de-duplicate the logic
- [x] **Interrupt handling** — `UserCommand.Interrupt` is defined but not yet wired; cancels the current turn
- [x] **Tool-using loop limit** is hard-coded (8 turns); should be configurable (or simply unlimited)

## LLM Endpoints

- [x] **OpenRouter** (default) — via OpenAI Chat Completions API, supports thinking
- [x] **OpenAI** — via Responses API (new) and Chat Completions API (legacy)
- [x] **Anthropic** — via Messages API with extended thinking support
- [x] **Ollama** — via OpenAI Responses API compatibility layer
- [x] Streaming invoke on all production endpoints
- [ ] **ScriptedEndpoint** does not support `stream` (used in sub-agent tests)

## Scala Runtime

- [ ] Improve the usability of file-accessing interface
- [ ] Support workflow API
- [ ] Support subagent calls
- [x] **`read`** — file reading with `<n>@` line-numbered output (orientation only), offset/limit
- [x] **`edit`** — content-anchored: replace an exact, unique `oldText` snippet with `newText`
- [x] **`write`** — create a new file or overwrite an existing one (makes parent dirs)
- [x] **`bash`** — shell command execution with timeout, truncation, process tree kill
- [x] **`lib.memory`** — durable project memory (id + description + content), one Markdown file per memory under `.auk/memory/`; reached through the runtime library, not a tool (`overview`/`read`/`write`/`delete`)
- [x] **`sub_agent`** — nested agent with own tool loop, token aggregation, non-recursive
- [x] **File search / grep tool** — currently the model has to fall back to `bash grep`
- [x] **Directory listing tool** — currently the model has to fall back to `bash ls`
- [x] Edit anchors on content, not line numbers, so consecutive edits survive line shifts with no re-read
- [x] Verify the edit tool for creating new files (handled by the new `write` tool; `edit` points at it)

## Approvals & Safety

- [x] `ApprovalPolicy` trait (`AllowAll`, `DenyAll`)
- [x] Approval-gated tools (`edit`, `bash`)
- [x] Read-only tools (`read`) skip approval
- [ ] **Interactive approval** — prompt user in the TUI to allow/deny side-effecting operations
- [ ] **Diff preview** before applying edits

## Tool Framework

- [x] Typed `Tool` trait with derived `ToolInput` (schema + decoder)
- [x] `@desc` annotation for human-readable parameter descriptions
- [x] `RuntimeContext` with working directory and approval policy
- [x] `ToolResult` with metadata side-channel (exit codes, token counts)
- [x] JSON Schema derivation (primitives, nested case classes, Option, enum, Map, arrays)
- [x] `ToolRegistry` with dispatch, concurrent fan-out, and exception handling
- [x] `ToolBridge` — converts tools to wire format for LLM endpoints

## Testing

- [x] **Tool unit tests** — Read (25+), Edit (35+), Bash (30+), Memory (9), SubAgent (7)
- [x] **Tool framework tests** — ToolInput (16), Json (9), ToolRegistry (7)
- [x] **Message tests** — MessageSuite (4)
- [x] **ChatState tests** — 22 test cases for line editor, history, streaming state machine

## CLI & Tooling

- [x] **Configuration file** — model, endpoint, approval policy, etc.
- [x] `sbt "runMain auk.cli.chat"` — minimal stdin/stdout chat loop for debugging endpoints
- [x] `scripts/install.sh` — `sbt publishLocal` + `cs launch` installer
