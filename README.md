# Auk 🐧

A coding agent for your terminal.

Auk compiles to WebAssembly (via Scala.js) and runs on the V8 runtime.

## Installation

To install a prebuilt binary on Homebrew, do:
``` sh
brew tap linyxus/auk https://github.com/Linyxus/auk
brew trust linyxus/auk
brew install linyxus/auk/auk
```
Note: only Apple Silicon is supported for pre-built binaries. For other platforms, you have to build `auk` yourself.

## Build & Testing

This project uses `bun` and `node` for packaging, `sbt` for build management. It is written in Scala 3 with Scala.js.

To build a Node SEA at `./dist/auk`:
``` sh
sbt packageBinary
```

## Usage

Run `auk` to start the agent. Type message to interact with it. `Ctrl+C` opens up the command palette. To exit, either type `Ctrl+C c` or `Ctrl+Q`.

By default the chat runs fullscreen (the terminal's alternate screen): scroll the transcript with the mouse wheel or `PageUp`/`PageDown`, and scrolling back to the bottom re-follows the live tail. Because fullscreen enables mouse reporting, use `Shift`-drag (or your terminal's usual override) to select text. Run `auk --inline` for the classic inline mode, which prints the transcript into the terminal's native scrollback and leaves scrolling and selection to the terminal (no mouse reporting).

