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

By default the chat runs fullscreen (the terminal's alternate screen): scroll the transcript with the mouse wheel or `PageUp`/`PageDown`, and scrolling back to the bottom re-follows the live tail. Drag with the mouse to select transcript text; on release the selection is copied to your clipboard (via OSC 52 — iTerm2 needs "Applications may access clipboard" enabled under Preferences → General → Selection, and tmux needs `set-clipboard on`). A drag near the top or bottom edge auto-scrolls so a selection can run past one screen. If you'd rather use your terminal's own selection, `Shift`-drag (or your terminal's usual override) bypasses mouse reporting entirely. Run `auk --inline` for the classic inline mode, which prints the transcript into the terminal's native scrollback and leaves scrolling and selection to the terminal (no mouse reporting).

