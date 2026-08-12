# Auk: Coding Agent in Scala 3

Auk is a coding agent.

## Building and Testing

The build is [Mill](https://mill-build.org); `./mill` bootstraps the pinned version
(`//| mill-version:` at the top of `build.mill`), so there is nothing to install.

Use `./mill __.test` to run the full test suite across every module. `./mill test`
runs only the root module's suite — unlike the old sbt build, `__.test` really does
include `library`, `grep`, `snapshot`, `webui`, `webui-dev` and `workflow-protocol`.

Run `./mill packLibraryBin` after editing `library/` or `grep/`: it repacks
`vendor/repl/library.bin`, which REPL sessions preload.

## Miscs

When testing, always use z.ai's `glm-5.2` as model.

