#!/usr/bin/env bash
#
# Workaround for a JavaScriptCore (Bun) bug: its native WebAssembly `js-string`
# builtins blank the last segment of `String.split` (e.g. "Auk".split("\n")
# yields [""] instead of ["Auk"]), which corrupts text all over the TUI. The
# Scala.js linker's loader requests those native builtins; this drops the
# request so the bundled JS polyfill is used instead, which is correct on JSC.
#
# Idempotent — a no-op once patched. Re-run after every `sbt fastLinkJS`.
# (Node/V8 is unaffected either way.)
#
set -euo pipefail
loader="${1:-target/scala-3.8.3/auk-fastopt/__loader.js}"
if [ -f "$loader" ]; then
  perl -0pi -e 's/builtins:\s*\["js-string"\]/builtins: []/' "$loader"
fi
