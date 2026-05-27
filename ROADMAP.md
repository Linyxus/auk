# Auk 🐧 — Roadmap

> **Progress:** 6/6 core tools implemented · 4/4 LLM providers integrated · 165+ tests passing

---

## UI/UX

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
- [ ] Display diff for "Edit" actions
- [ ] Repair scrolling
- [ ] **Slash commands** (`/quit`, `/exit`, `/model`, etc.)
- [ ] Multi-line input (Shift+Enter or similar)
- [ ] Configurable keybindings

## Configuration

- [ ] Make things configurable

## Agent Engine

- [x] **Turn loop** with tool-use round-tripping (up to 8 rounds)
- [x] **Concurrent tool execution** via `Future`
- [x] **Streaming support** — live deltas from the LLM
- [x] **Tool progress events** (`ToolRunStart`/`ToolRunEnd`) for UI feedback
- [x] **Sub-agent delegation** — nested headless agent (non-recursive, up to 16 rounds)
- [x] **Token usage aggregation** across sub-agents
- [ ] **Interrupt handling** — `UserCommand.Interrupt` is defined but not yet wired; cancels the current turn
- [ ] **Tool-using loop limit** is hard-coded (8 turns); should be configurable (or simply unlimited)
- [ ] Real-time steering
- [ ] Setup the "event"-based context infrastructure
- [ ] Support compacting

## LLM Endpoints

- [x] **OpenRouter** (default) — via OpenAI Chat Completions API, supports thinking
- [x] **OpenAI** — via Responses API (new) and Chat Completions API (legacy)
- [x] **Anthropic** — via Messages API with extended thinking support
- [x] **Ollama** — via OpenAI Responses API compatibility layer
- [x] Streaming invoke on all production endpoints
- [ ] **ScriptedEndpoint** does not support `stream` (used in sub-agent tests)

## Runtime Tools

- [x] **`read`** — file reading with `@<n>>` line-numbered output, offset/limit
- [x] **`edit`** — line-based file editing with content verification and `...` wildcard
- [x] **`bash`** — shell command execution with timeout, truncation, process tree kill
- [x] **`write_memory` / `get_memory`** — persistent key-value project memory (`.auk/memory.json`)
- [x] **`sub_agent`** — nested agent with own tool loop, token aggregation, non-recursive
- [ ] **File search / grep tool** — currently the model has to fall back to `bash grep`
- [ ] **Directory listing tool** — currently the model has to fall back to `bash ls`
- [ ] Refactor the edit tool: let model specify line numbers
- [ ] Refactor the edit tool: if file is editted after the last read, it must be read again
- [ ] Verify the edit tool for creating new files

## Approvals & Safety

- [x] `ApprovalPolicy` trait (`AllowAll`, `DenyAll`)
- [x] Approval-gated tools (`edit`, `bash`, `write_memory`, `send_email`)
- [x] Read-only tools (`read`, `get_memory`) skip approval
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
- [ ] **Engine integration tests** — no full Engine + endpoint round-trip tests exist
- [ ] **TUI tests** — no tests for ChatApp's update/view logic

## CLI & Tooling

- [x] `sbt "runMain auk.cli.chat"` — minimal stdin/stdout chat loop for debugging endpoints
- [x] `scripts/install.sh` — `sbt publishLocal` + `cs launch` installer
- [ ] **Configuration file** — model, endpoint, approval policy, etc.
