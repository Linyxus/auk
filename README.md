# Auk

Auk is a coding agent written in Scala 3, compiled to WebAssembly (WasmGC + JSPI) through Scala.js, and shipped as a single self-contained executable. It puts a full-screen terminal UI in front of an LLM-driven agent whose central tool is a Scala REPL: rather than calling a fixed set of file and shell tools, the model acts by writing and running code — `eval_scala` evaluates Scala 3 in a persistent session powered by a compiler + interpreter built from the [scala3-js](https://github.com/Linyxus/scala3-js) fork, preloaded with a runtime library (`lib`) that reaches the file system, processes, sub-agents, and the project's own durable state.

## Features

- **Terminal UI.** A full-screen TUI built on Auk's own rendering engine (per-cell diffing, synchronized atomic writes): streaming replies with thinking blocks and tool-call labels, Markdown rendering, a line editor with input history, and fullscreen viewers for transcripts, workflow runs, and MCP servers. `Ctrl+C` opens the command palette; `auk --inline` opts into a hybrid inline mode.
- **Act by code.** One persistent REPL per session, with definitions accumulating across calls, so the agent builds up its own helpers as it works. The preloaded library exposes typed APIs for the file system (`lib.fs`), processes (`lib.shell`), durable memory (`lib.memory`), and past sessions (`lib.history`).
- **Sub-agent workflows.** `wf.start { ... }` fans a task out to a typed graph of disposable sub-agents — fan-out, dependency pipelines, joins, a final synthesis — while the host streams a live forest of the run to the TUI.
- **Agent team.** `lib.team` hires named, long-lived member agents that keep their own context across messages and reply asynchronously — delegation that is a continuing conversation, not a one-shot graph.
- **Refinement loops.** `lib.loop.start` begins durable, self-improving work: each generation is a worker agent improving on the last accepted state, a Scala checker written into the loop decides mechanically what counts as progress, and the whole history is a ledger under `.auk/loops/` — a loop survives the session that started it.
- **Durable skills.** When the agent gets a fiddly procedure working, it can crystallize the code as a skill: a tested Scala `object` stored under `.auk/skills/` and preloaded into every later session, so hard-won capability accumulates across conversations.
- **Project memory and history.** `lib.memory` keeps curated project notes (one Markdown file each under `.auk/memory/`); `lib.history` searches and reads the agent's own past conversations (`.auk/sessions/`).
- **Web dashboard.** Workflow runs and refinement loops lazily start a local HTTP+SSE server serving a live browser dashboard from assets embedded in the binary: the run forest with every sub-agent's transcript, and each loop's generation lineage — accepted work, abandoned branches, per-attempt checker reports, verdicts, and diffs (`o` in the workflows or loops window opens it). Set `AUK_NO_DASHBOARD=1` to opt out.
- **MCP servers.** Stdio MCP servers declared in `.auk/config` are spawned and discovered in the background; their tools appear as native model tools.
- **Models.** ZAI is the default provider, with Kimi and OpenRouter built in. Keys come from `ZAI_API_KEY`, `KIMI_API_KEY`, or `OPENROUTER_API_KEY`; switch provider or model at any time with the `/model` command.

## Installation

Prebuilt binaries are published for Apple Silicon (macOS arm64) via Homebrew:

```sh
brew tap linyxus/auk https://github.com/Linyxus/auk
brew trust linyxus/auk
brew install linyxus/auk/auk
```

On any other platform, build from source as described below.

## Build from source

Prerequisites: **sbt**, **Node.js 25+**, and **bun** — bun is used only for JS installs and packaging tooling; Auk itself runs on Node/V8.

The binary at `dist/auk` is a Node single-executable application embedding three vendored inputs alongside the app:

1. **The REPL worker** (`vendor/repl/`): the Scala 3 compiler and interpreter built to JS, from the [`scala3-js`](https://github.com/Linyxus/scala3-js) fork. Check the fork out and build its artifacts:

   ```sh
   git clone https://github.com/Linyxus/scala3-js.git ~/workspace/scala3-js
   cd ~/workspace/scala3-js
   sbt scala3-repl-json-sjs/fullLinkJS scala3-compiler-cli-sjs/packClasspath scala3-compiler-cli-sjs/packLinkerLibs
   ```

   Then, from the auk checkout, vendor them (re-run whenever the fork is rebuilt; `SCALA3_JS_HOME` defaults to `~/workspace/scala3-js`):

   ```sh
   SCALA3_JS_HOME=~/workspace/scala3-js sbt vendorRepl
   ```

2. **The auk runtime library** (`vendor/repl/library.bin`): the `library/` subproject preloaded into every REPL session. `sbt packageBinary` repacks it on every run; for dev and test runs, pack it explicitly:

   ```sh
   sbt packLibraryBin
   ```

3. **The web dashboard** (`vendor/webui/`): the browser bundle served by the host dashboard:

   ```sh
   sbt vendorWebUI
   ```

With the three inputs in place, produce the standalone binary at `./dist/auk`:

```sh
sbt packageBinary
```

## Usage

Set the API key for your provider (e.g. `ZAI_API_KEY` for the default), then run `auk` in a project directory. It boots into a fullscreen chat with the agent; `auk --inline` selects the inline display mode and `auk --version` prints the build version.

Type a message and press Enter to talk; the agent streams its reply and runs code as it goes. You can keep typing while it works — a message sent mid-turn is queued and steers the agent between rounds.

- `Ctrl+C` opens the command palette (a which-key menu): `c` quits (Ctrl+Q works anytime), `k` interrupts the current turn, `m` picks a model, `r` resumes an earlier session, `n` starts a new one, `w` shows workflow runs, `s` shows MCP servers.
- Every command also has a slash form (`/model`, `/resume`, `/mcp`, ...) — type `/` to browse them.
- The up/down arrows walk your input history; Shift+Enter (or Ctrl+J) inserts a newline into the message you are composing.

### Configuration

Configuration lives in `.auk/config` in the working directory; every section is optional. It selects the model and declares MCP servers:

```
[model]
provider = ZAI
id = glm-5.2

[mcp.servers.files]
command = npx
args = -y @modelcontextprotocol/server-filesystem /tmp
```

Each `[mcp.servers.<name>]` section declares one stdio MCP server: a required `command`, plus an optional `args` list and `env` overrides. Switching models with `/model` is persisted back to the `[model]` section.

## Development

- `sbt test` runs the full test suite (munit, across all subprojects); `sbt run` runs the agent from source.
- [AGENTS.md](AGENTS.md) and [CLAUDE.md](CLAUDE.md) hold contributor notes, [ROADMAP.md](ROADMAP.md) tracks what is done and what is next, and [build.sbt](build.sbt) is heavily commented — it documents the packaging pipeline in detail.
- The runtime library preloaded into REPL sessions is specified by [`auk.library.AukInterface`](library/src/main/scala/auk/library/AukInterface.scala) — the reference for everything `lib`, `wf`, and `team` can do.
- Repository layout in one breath: the root project is the agent, engine, and TUI (linked to Wasm); `library/` is the REPL runtime library; `webui/` the Laminar dashboard; `grep/` the search engine behind `lib.fs` grep/glob/walk; `snapshot/` the non-disruptive git snapshots loops record their states with; `workflow-protocol/` the shared host-to-browser protocol.
