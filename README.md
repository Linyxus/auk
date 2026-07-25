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

This project uses `bun` and `node` for packaging, `sbt` for build management. It is written in Scala 3 with Scala.js.

To build a Node SEA at `./dist/auk`:
``` sh
sbt packageBinary
```

## Usage

Run `auk` to start the agent. Type message to interact with it. `Ctrl+C` opens up the command palette. To exit, either type `Ctrl+C c` or `Ctrl+Q`.

