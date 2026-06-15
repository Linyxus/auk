# Auk 🐧

A coding agent for your terminal.

Auk compiles to WebAssembly (via Scala.js) and runs on the V8 runtime.

## Installation

``` sh
brew tap linyxus/auk https://github.com/Linyxus/auk
brew trust linyxus/auk
brew install linyxus/auk/auk
```

## Build & Testing

This project uses `bun` and `node` for packaging, `sbt` for build management. It is written in Scala 3 with Scala.js.

To build a Node SEA at `./dist/auk`:
``` sh
sbt packageBinary
```

