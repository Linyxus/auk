# Auk 🐧

A coding agent for your terminal, written in Scala 3. Auk talks to an LLM,
streams its replies (and its reasoning) live, and lets the model read files,
edit them, run shell commands, remember things about your project, and delegate
work to sub-agents — all from a keyboard-driven TUI.


Auk compiles to **WebAssembly** (via Scala.js) and runs on the **Bun** runtime.
The LLM provider SDKs are the npm `openai` / `@anthropic-ai/sdk` packages.

## Dependencies

| Tool | Purpose | Required Version |
|------|---------|------------------|
| **Bun** | Runs the compiled Wasm + bundles | 1.3.x (Node 23+/26 also works) |
| **JDK** | Runs sbt (build only) | 17 or newer (tested on 21) |
| **coursier** (`cs`) | Installs the JDK + sbt | 2.1.x |
| **sbt** | Compiles Scala → Wasm | 1.12.x |

Install Bun from [bun.sh](https://bun.sh); install the JVM toolchain via
coursier:

### Install coursier (and optionally the JDK + sbt)

**macOS / Linux:**

```sh
# Install the `cs` launcher (Homebrew shown; see https://get-coursier.io for others)
brew install coursier/formulas/coursier

# Have coursier set up a JDK, sbt, and the PATH for you
cs setup
```

`cs setup` installs a JDK and common Scala tools, and adds coursier's `bin`
directory to your `PATH`. If you already manage your own JDK/sbt, you can skip
`cs setup` and just make sure `java`, `sbt`, and `cs` are on your `PATH`.

> Other install methods (curl, Windows, etc.) are documented at
> [get-coursier.io](https://get-coursier.io/docs/cli-installation).

---

## Configuration

Auk reads its credentials from the environment. Out of the box it uses
**OpenRouter**:

| Variable | Required | Default |
|----------|----------|---------|
| `OPENROUTER_API_KEY` | ✅ Yes | — |
| `OPENROUTER_BASE_URL` | ❌ No | `https://openrouter.ai/api/v1` |

Get a key at [openrouter.ai/keys](https://openrouter.ai/keys), then export it:

```sh
export OPENROUTER_API_KEY="sk-or-v1-..."
```

The default model is `deepseek/deepseek-v4-flash` (see
`src/main/scala/auk/Main.scala`).

Provider selection, model selection, thinking mode, and approval policy are
currently hard-coded in `Main.scala`. A configuration file is still on the
roadmap.


## Install & run

From the project root, run the installer once:

```sh
./scripts/install.sh
```

This does three things:

1. Installs the npm SDK dependencies (`bun install`).
2. Compiles the project to WebAssembly (`sbt fastLinkJS`).
3. Installs a small `auk` launcher onto your `PATH` that runs the linked build
   on Bun.

Then start the agent (in a real terminal):

```sh
auk
```

The day-to-day loop after editing Scala is just `sbt fastLinkJS` (or
`sbt ~fastLinkJS` to watch) and re-run `auk` — the launcher always runs the
latest linked output.

### Keybindings

| Key | Action |
|-----|--------|
| `Enter` | send the message |
| `←` / `→` | move the cursor |
| `Home` / `End` | jump to start / end of line |
| `Delete` | delete the character under the cursor |
| `Ctrl+A` / `Ctrl+E` | jump to start / end of line |
| `Ctrl+K` | kill to end of line |
| `Ctrl+U` | kill to start of line |
| `Ctrl+W` | delete the previous word |
| `↑` / `↓` | navigate input history |
| `Ctrl+C` | show key bindings |
| `Ctrl+C c` / `Ctrl+C q` | quit |
| `Ctrl+C r` | open the resume-session picker |
| `Ctrl+C n` | start a new session |
| `Ctrl+Q` | quit directly |

---

## Tools

The model invokes tools by name; Auk runs them in the working directory and
feeds the results back into the conversation. The bundled agent has:

| Tool | What it does | Approval |
|------|--------------|----------|
| `read` | Read a file as numbered lines (`<n>@ <content>`; numbers are for orientation), with optional offset/limit. Files ≤ 5 MB, output capped at 100 KB. An empty or missing file is reported (not an error) with a pointer to `write`. | — |
| `edit` | Replace an exact text snippet: `oldText` (copied verbatim, without the `<n>@ ` prefix) must match exactly once and is replaced by `newText` (empty deletes). Content-anchored, so it survives earlier edits that shift line numbers. Edits existing, non-empty files only. | ✅ |
| `write` | Create a new file (or overwrite an existing one) with the given content; makes any missing parent directories. | ✅ |
| `bash` | Run a shell command via `bash -c` with a timeout (default 2 min, max 10 min); merged stdout/stderr capped at 100 KB. | ✅ |
| `write_memory` | Save a project note under a key. | — |
| `get_memory` | Recall a note by key, or list all stored notes. | — |
| `sub_agent` | Hand a focused task to a nested agent (see below). | inherits |

Approval-gated tools consult the runtime approval policy, but the default TUI
currently uses `ApprovalPolicy.AllowAll`, so side-effecting actions are
auto-approved. Interactive approval prompts and edit diff previews are not wired
yet.

**Project memory.** `write_memory` / `get_memory` persist a small key-value
store as JSON at `.auk/memory.json` under the working directory, so notes
survive across sessions. Reads and writes are serialized, and a corrupt store
surfaces an error rather than being silently treated as empty.

**Sub-agents.** `sub_agent` spawns a headless agent that runs its own
tool-use loop to completion on a single self-contained prompt and returns one
final summary — useful for keeping a large exploration out of the main
conversation. It shares the caller's working directory and approval policy and
gets its own toolset (read/edit/write/bash + memory), but **not** the `sub_agent`
tool itself, so it can't recurse. It reports `rounds` and token usage as
metadata.

**Sessions.** The `auk.session` package provides append-only JSONL session logs
under `.auk/sessions`. The default TUI/engine path creates a fresh project
session, persists each completed step there, and has `Engine` replay those
events into model-facing history. Press `Ctrl+C r` to pick a saved session to
resume, or `Ctrl+C n` to start a fresh one.

---

### Project layout

```
src/main/scala/auk/
├── Main.scala            # entry point: wires endpoint + tools + TUI
├── agent/                # Engine (turn/tool loop) and UserCommand
├── llm/
│   ├── endpoint/         # Endpoint trait + Anthropic/OpenAI/OpenRouter/Ollama
│   └── tools/            # Tool framework: Tool, ToolInput, Schema, ToolResult, RuntimeContext
├── runtime/              # Concrete tools: Read, Edit, Write, Bash, Memory, SubAgent + ToolRegistry
├── session/              # Append-only session logs and replay primitives
├── tui/                  # terminal UI (ChatApp, Model, ChatTui)
│   ├── render/           # our rendering core: cell-grid diff, styles, Terminal
│   └── app/              # gears-based Elm runtime + view DSL + layout engine
└── utils/                # small helpers (Result)
```

## Development loop

After the one-time install, rebuilding is just a relink — the `auk` command
always runs your latest linked output, no reinstall needed:

```sh
sbt fastLinkJS     # recompile Scala → Wasm  (or `sbt ~fastLinkJS` to watch)
auk                # runs the new build on Bun
```

Or run the linked output directly (handy for scripting):

```sh
sbt fastLinkJS
bun run target/scala-3.8.3/auk-fastopt/main.js
```

Tests run on Node via sbt:

```sh
sbt test   # run the munit suite (294 tests at the time of writing)
```

> Note: `sbt run` is **not** the way to launch the TUI. Under sbt the process
> has no controlling terminal, so it falls back to a non-interactive headless
> terminal. Use `auk` (or `bun run …`) in a real terminal instead.

For a self-contained bundle (npm SDKs inlined + `.wasm` copied):

```sh
sbt fastLinkJS && bun run build   # → dist/main.js + dist/main.wasm
bun run dist/main.js              # or: bun run start
```

## Current limitations

- No configuration file yet; endpoint/model/approval defaults are in code.
- Interactive approvals and edit diff previews are not implemented yet.
- `UserCommand.Interrupt` exists, but the engine currently ignores it.
- File search and directory listing are not first-class tools yet; the model can
  still use `bash` for `rg`, `find`, or `ls`.
