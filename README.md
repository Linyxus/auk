# Auk 🐧

A coding agent.

## Installation

To install a prebuilt binary on Homebrew, do:
``` sh
brew tap linyxus/auk https://github.com/Linyxus/auk
brew trust linyxus/auk
brew install linyxus/auk/auk
```
Note: only Apple Silicon is supported for pre-built binaries. For other platforms, you have to build `auk` yourself.

## Build

Prerequisites: `sbt`, `node` 25+, and `bun` (used only for JS installs/tooling — `auk` itself runs on V8). Written in Scala 3 with Scala.js; the agent links to WebAssembly (WasmGC + JSPI).

The standalone binary is a Node single-executable application (SEA) that embeds three pieces of vendored content alongside the app:

- **The REPL worker** (`vendor/repl/`): the Scala 3 compiler + sjsir interpreter built to JS, from the [`scala3-js`](https://github.com/Linyxus/scala3-js) fork. Checkout the fork, build it, then vendor its artifacts:
  ``` sh
  git clone https://github.com/Linyxus/scala3-js.git ~/workspace/scala3-js
  cd ~/workspace/scala3-js
  sbt scala3-repl-json-sjs/fullLinkJS scala3-compiler-cli-sjs/packClasspath scala3-compiler-cli-sjs/packLinkerLibs
  ```
  Then, from the auk checkout, run `sbt vendorRepl` against that checkout:
  ``` sh
  SCALA3_JS_HOME=~/workspace/scala3-js sbt vendorRepl
  ```
  Re-run it whenever the fork is rebuilt. `SCALA3_JS_HOME` defaults to `~/workspace/scala3-js`.
- **The auk runtime library** (`vendor/repl/library.bin`): the `library` subproject that REPL sessions preload. Built and packed locally — `sbt packageBinary` repacks it on every run, but for dev/test runs you can pack it explicitly:
  ``` sh
  sbt packLibraryBin
  ```
- **The web dashboard** (`vendor/webui/`): the Laminar browser bundle served by the host workflow UI.
  ``` sh
  sbt vendorWebUI
  ```

With the three vendored inputs in place, produce the standalone binary at `./dist/auk`:
``` sh
sbt packageBinary
```

## Usage

Run `auk` to start the agent. Type message to interact with it. `Ctrl+C` opens up the command palette. To exit, either type `Ctrl+C c` or `Ctrl+Q`.
