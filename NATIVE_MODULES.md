# NATIVE_MODULES — native companion JS modules for the scala3-js REPL

Read-only reconnaissance for "option 3": let library code evaluated in the
sjsir-interpreter call typed `@js.native @JSImport(...)` facades, with the `.js`
bytes shipped inside the existing `ClasspathBlob` archives (auk's `library.bin`).

Repos: fork `/Users/linyxus/Workspace/scala3-js` (branch `compiler-js`, HEAD
`9f7d0b7d1b`); auk `/Users/linyxus/Workspace/auk` + worktree
`/Users/linyxus/Workspace/auk-wt/auk-grep`. Interpreter artifact
**`be.doeraene:sjsir-interpreter_sjs1_2.13:0.10.0`** (note: `be.doeraene`, not
`org.scala-js` — `project/Build.scala:2664`); sources **are** published and were
read for this report (`…-0.10.0-sources.jar`).

---

## TL;DR verdicts

1. **`JSNativeLoadSpec.Import` is hard-unsupported today** — it throws
   `AssertionError("Imports are currently not supported")` at
   `Executor.scala:372-373`. There is **no module map, no config, no dynamic
   import, no require attempt**. The `Executor` takes only `(interpreter)`
   (`Executor.scala:22`) and `Interpreter` takes only `(semantics)`
   (`Interpreter.scala:15`), so there is no seam to pass a resolver without
   changing the artifact.

2. **But you do not need to patch the interpreter.**
   `JSNativeLoadSpec.ImportWithGlobalFallback` **already works** — it resolves
   the *global* branch (`Executor.scala:374-375`), and `@JSImport(module, name,
   globalFallback)` is public, standard Scala.js API
   (`JSImport.scala:72,83-84`) that the fork's backend already lowers to
   `ImportWithGlobalFallback` (`JSCodeGen.scala:5128`), **including dotted
   fallback paths** (`parseGlobalPath`, `JSCodeGen.scala:5093-5096`) and
   **including inherited member specs** (`JSCodeGen.scala:5086-5089`).
   So a typed facade resolves through a namespaced global registry with **zero
   interpreter changes and zero compiler changes**.

3. **All three native-load paths funnel through one function**, so one fix
   covers objects, classes and members: `loadJSModule` (NativeJSModuleClass,
   `Executor.scala:342-344`), `loadJSConstructor` (NativeJSClass,
   `Executor.scala:354-358`) and `SelectJSNativeMember` (`Nodes.scala:393-400`)
   all call `Executor.loadJSNativeLoadSpec` (`Executor.scala:366`).

4. **Recommended loading mechanism: `new js.Function("exports","module","require", code)`**
   with `//# sourceURL=<module>.js` appended — validated end-to-end on Node
   v26.3.1 (see "Proof" below). No temp files, no `vm`, identical behaviour in
   SEA and non-SEA. Code generation from strings is already load-bearing in this
   worker (`Executor.getJSGlobalRef` is literally `js.eval`, `Executor.scala:605-606`),
   so this adds no new capability requirement.

5. **Recommended naming convention: reserved archive prefix `js-modules/<moduleName>.js`.**
   `ExtraLib.fromArchive` (`ExtraLib.scala:33-36`) currently does a two-way
   partition on `.sjsir`; this becomes a three-way partition. Registry lives at
   `globalThis.__replJSModules`.

6. **What changes about option 3:** the in-archive requirement — not the
   `@JSImport` requirement — is what forces fork work. A facade with
   `globalFallback` works today; getting the bytes *out of `library.bin`* is the
   only thing the fork must learn. And there is a Tier-0 that needs **no fork
   change at all** (ship the bundle as a 5th vendored file, install it from
   auk's existing bootstrap shim). See "Recommended design" for the tiers.

---

## Q1 — sjsir-interpreter internals (the decisive question)

Sources: `cs fetch --sources be.doeraene:sjsir-interpreter_sjs1_2.13:0.10.0`
(published; extracted to a scratch dir for this report). 17 files under
`org/scalajs/sjsirinterpreter/core/`.

### The Import verdict

`Executor.loadJSNativeLoadSpec` — the single resolution point:

```scala
// Executor.scala:366-377
def loadJSNativeLoadSpec(loadSpec: JSNativeLoadSpec)(implicit pos: Position): Value = {
  loadSpec match {
    case JSNativeLoadSpec.Global(ref, path) =>
      path.foldLeft(getJSGlobalRef(ref)) { (prev, pathItem) =>
        prev.asInstanceOf[RawJSValue].jsPropertyGet(pathItem)
      }
    case JSNativeLoadSpec.Import(_, _) =>
      throw new AssertionError("Imports are currently not supported")
    case JSNativeLoadSpec.ImportWithGlobalFallback(_, globalSpec) =>
      loadJSNativeLoadSpec(globalSpec)
  }
}
```

- **`Import` → hard `AssertionError`.** Not a `require`, not a dynamic
  `import()`, not a lookup. It ignores both the module name and the path.
- **`ImportWithGlobalFallback` → resolves `globalSpec`** unconditionally. This is
  exactly how the standard scalajs-library classes work in this environment
  today; it is *already* a working escape hatch for user code.
- **`Global(ref, path)` → `getJSGlobalRef(ref)` then a fold of property gets.**
  And `getJSGlobalRef` is:

```scala
// Executor.scala:605-606
def getJSGlobalRef(name: String): Value =
  js.eval(name)
```

  `js.eval` is *indirect* eval → runs in **global scope**. So a `Global` ref
  resolves against `globalThis` only. (Verified on Node v26.3.1:
  `(0,eval)("require")` throws `ReferenceError` even though `typeof require ===
  "function"` in the CJS module scope — this asymmetry matters, see Q6.)

### All callers funnel through that one function

| IR construct | Node | Executor entry |
|---|---|---|
| `LoadJSModule(cls)` on a `@js.native @JSImport` **object** | `Nodes.scala:1050-1055` | `loadJSModule` → `NativeJSModuleClass` → `loadJSNativeLoadSpec(classDef.jsNativeLoadSpec.get)` (`Executor.scala:341-344`) |
| `LoadJSConstructor(cls)` on a `@js.native @JSImport` **class** | `Nodes.scala:1042-1048` | `loadJSConstructor` → `NativeJSClass` → same (`Executor.scala:354-358`) |
| `SelectJSNativeMember` (a `@js.native` **member** with its own spec) | `Nodes.scala:393-400` | `loadJSNativeLoadSpec(memberDef.jsNativeLoadSpec)` |

So **native class instantiation and namespace objects share one code path** —
there is no separate limitation for `new`-ing a `@JSImport` native class
(`js.Dynamic.newInstance` on the loaded constructor, `Executor.scala:348`).

Nothing inspects `jsNativeLoadSpec` at *registration* time: `ClassInfo.scala` has
no reference to it, and `Interpreter.loadIRFiles` (`Interpreter.scala:51-76`)
only builds `ClassInfo`s, runs static initializers and top-level exports. **The
`Import` failure is lazy — it fires at first use, not at load.**

### Where a module map would hook in

**The natural (upstream) hook is `Executor.loadJSNativeLoadSpec`, the `Import`
case at `Executor.scala:372-373`**, with a resolver threaded from the
`Interpreter` constructor (`Interpreter.scala:15`) through `Executor`'s
constructor (`Executor.scala:22`, `private[core] final class Executor(val
interpreter: Interpreter)`), i.e.:

```scala
case JSNativeLoadSpec.Import(module, path) =>
  path.foldLeft(interpreter.moduleResolver(module)) { (prev, item) =>
    prev.asInstanceOf[RawJSValue].jsPropertyGet(item) }
```

**But this requires forking/vendoring the `be.doeraene` artifact**, because
`Executor` is `final` and `private[core]`, and the method cannot be overridden.
(The fork already reaches *into* the interpreter's package from `repl-js/src` —
`repl-js/src/org/scalajs/sjsirinterpreter/core/EvalSupport.scala:1`, which even
reads the plain-`private` `classInfos` field through its Scala.js name mangling,
`EvalSupport.scala:36-41` — so package-level reach-in is precedented, but
*replacing a method body* is not possible that way.)

**Do not do this.** Two cheaper hooks exist — see Q4 / "Recommended design":

- **Tier 1 (no IR work):** use `ImportWithGlobalFallback` and register modules
  under a `globalThis` registry. Uses `Executor.scala:374-375` as-is.
- **Tier 2 (fork-side IR rewrite, ~50 LOC, still no interpreter change):**
  rewrite `Import(module, path)` → `Global("__replJSModules", module :: path)`
  when the IR is handed to the interpreter, i.e. in
  `InterpreterRunner.toIRFiles` (`InterpreterRunner.scala:36-39`) and
  `InterpreterRunner.registerEvalClasses` (`InterpreterRunner.scala:105-113`).
  Those two are the **only** places IR enters the interpreter.

Tier 2 is mechanically sound against IR 1.21.0:

- `ClassDef.apply` is public with all 14 params (`Trees.scala:1496-1517`), and
  `jsNativeLoadSpec` is field 8 (`Trees.scala:1481`); `optimizerHints` and `pos`
  are carried in the second/implicit parameter lists (`Trees.scala:1488-1490`).
- `JSNativeMemberDef` is a `sealed case class` (`Trees.scala:1585-1588`) → plain
  `.copy(jsNativeLoadSpec = …)`.
- `JSNativeLoadSpec.Global` requires `JSGlobalRef.isValidJSGlobalRefName`
  (`Trees.scala:1914-1917`, predicate at `Trees.scala:1194-1197`) —
  `__replJSModules` is a valid JS identifier and not reserved. Module names with
  hyphens/slashes (`auk-grep-engine`, `@scope/pkg`) are fine because they land in
  the **path** (property names), never in the ref.
- No re-serialization is needed: `IRFileImpl` is a public abstract class in
  `org.scalajs.linker.interface.unstable` (`IRFileImpl.scala:26-51`) with just
  `tree` and `entryPointsInfo` abstract, and `EntryPointsInfo.forClassDef` is
  public (`EntryPointsInfo.scala:24`). A `final class TreeIRFile(path, version,
  cd) extends IRFileImpl(path, version)` returning `Future.successful(cd)` slots
  straight in where `MemIRFileImpl` is used today
  (`InterpreterRunner.scala:36-39`). (`Serializers.serialize(OutputStream,
  ClassDef)` also exists at `Serializers.scala:63-64` if round-tripping were ever
  preferred — it is not.)

---

## Q2 — Fork wiring, and today's failure mode

### How the Interpreter is constructed

```scala
// InterpreterRunner.scala:34
private var interp = new Interpreter(Semantics.Defaults)
```

That is the whole config. `reset()` rebuilds it identically
(`InterpreterRunner.scala:53-56`). There is **no** notion of module loading
anywhere in `repl-js` / `repl-js-json` / `library-js`: grepping
`JSImport|LoadJSModule|moduleResolver|JSNativeLoadSpec` across those source roots
returns exactly one hit, and it is unrelated
(`compiler-js/src/java/security/MessageDigest.scala:4`).

IR enters the interpreter at exactly two places:

- `InterpreterRunner.toIRFiles` (`InterpreterRunner.scala:36-39`) — wraps bytes in
  `MemIRFileImpl(path, Version.Unversioned, bytes)`. Used by `loadAll()`
  (stdlib + every extra lib, `InterpreterRunner.scala:58-61`) and by
  `loadAndRun` (each line's fresh `.sjsir`, `InterpreterRunner.scala:86-91`).
- `InterpreterRunner.registerEvalClasses` (`InterpreterRunner.scala:105-113`) —
  the synchronous dynamic-`eval(...)` path, deserializes to `ClassDef` and calls
  `EvalSupport.registerClassDefs`.

### Today's failure mode for `@JSImport` — traced

No test or transcript exercises it: grepping `JSImport` under `repl-js/test`,
`repl-js/test-resources` and `repl-js-json` matches **only** files under
`target/` (build artifacts). So this is untested territory.

Precise path for `@js.native @JSImport("m", JSImport.Namespace) object F` used
from a REPL line:

1. The line **compiles fine** — the facade's `.tasty` is on the classpath via the
   extra lib's `cpDir` (`ExtraLib.scala:36`, `JSReplDriver.initCtx:71-74`), and
   the backend emits `Import("m", Nil)` into the facade's `.sjsir`
   (`JSCodeGen.scala:5124`; see Q3).
2. The facade's `ClassDef` (`kind = NativeJSModuleClass`) **loads fine** — no
   spec inspection at registration (`Interpreter.scala:51-76`).
3. The line's own IR contains `LoadJSModule(F$)`; running the wrapper
   (`JSReplDriver.runWrapper:211-224` → `InterpreterRunner.loadAndRun:86-91` →
   `runModuleInitializers`) evaluates `Nodes.LoadJSModule`
   (`Nodes.scala:1050-1055`) → `Executor.loadJSModule` (`Executor.scala:341-344`)
   → `Executor.loadJSNativeLoadSpec` → **`throw new AssertionError("Imports are
   currently not supported")`** (`Executor.scala:372-373`).
4. That `AssertionError` propagates out of the wrapper run, through
   `evalLineResult`/`ReplSession.eval`'s `recover`
   (`ReplSession.scala:77-…`), and surfaces to the JSONL client as a failed
   `eval` response whose error text is the bare string
   `"Imports are currently not supported"` — with **no** mention of the module
   name, the class, or the REPL line. Diagnostically opaque.

Note the failure is **at first use**, not at load: an unused `@JSImport` facade
in an extra lib is completely inert today.

---

## Q3 — Compiler side: no compiler work needed

`JSCodeGen.computeJSNativeLoadSpecOfInPhase`
(`compiler/src/dotty/tools/backend/sjs/JSCodeGen.scala`, body at 5070-5130) is
stock upstream logic:

```scala
// JSCodeGen.scala:5111-5129  (the @JSImport branch)
val module = annot.argumentConstantString(0).getOrElse { unexpected(…) }
val path = annot.argumentConstantString(1).fold {
  if (annot.arguments.sizeIs < 2) parsePath(sym.defaultJSName) else Nil
} { pathName => parsePath(pathName) }
val importSpec = Import(module, path)
annot.argumentConstantString(2).fold[js.JSNativeLoadSpec] {
  importSpec
} { globalPathName =>
  ImportWithGlobalFallback(importSpec, parseGlobalPath(globalPathName))
}
```

Consequences (all confirmed by reading, no build run):

- `@JSImport("m", JSImport.Namespace)` → **`Import("m", Nil)`**. `Namespace` is
  not a string literal so `argumentConstantString(1)` is `None`, and because the
  annotation has ≥ 2 arguments the `path` is `Nil` (`JSCodeGen.scala:5119-5123`).
- `@JSImport("m")` → `Import("m", parsePath(sym.defaultJSName))` — path derived
  from the declaration's name.
- `@JSImport("m", JSImport.Default)` → `JSImport.Default` is `final val Default =
  "default"` (`JSImport.scala:95`), a constant string → `Import("m",
  List("default"))`.
- **`@JSImport("m", name, globalFallback)` → `ImportWithGlobalFallback(Import(…),
  parseGlobalPath(globalFallback))`** (`JSCodeGen.scala:5128`), and
  `parseGlobalPath` splits on `.` (`JSCodeGen.scala:5093-5096`) — so
  `globalFallback = "__replJSModules.aukGrepEngine"` yields
  `Global("__replJSModules", List("aukGrepEngine"))`. **Dotted fallbacks work.**
- **Members inherit the owner's spec and extend its path**
  (`JSCodeGen.scala:5079-5090`), and the `ImportWithGlobalFallback` case is
  handled explicitly (`JSCodeGen.scala:5086-5089`): both the import path *and*
  the global path get `:+ jsName`. So one annotation on the facade object covers
  every member — `GrepEngineJS.search` becomes
  `ImportWithGlobalFallback(Import("auk-grep-engine", List("search")),
   Global("__replJSModules", List("aukGrepEngine", "search")))`.

The annotation is available to REPL lines and extra-lib sources: `JSImport` ships
in `scalajs-library` (`scala/scalajs/js/annotation/JSImport.scala`), which
`bundleLibs`/`packClasspath` bake into `classpath.bin`
(`project/Build.scala:1942, 2033`).

The REPL compiles with `-scalajs` and the full JS backend
(`JSReplDriver.baseSettings:63-64`), so nothing is special-cased away.

**Verdict: no compiler work. A REPL line compiled against a `@js.native
@JSImport` facade from an extra lib's `.tasty` produces IR whose execution hits
exactly the `Executor.loadJSNativeLoadSpec` paths in Q1.**

---

## Q4 — Archive plumbing and the loading mechanism

### Today

Archive format (`ClasspathBlob.scala:8-17`): `[4-byte BE index length][JSON index
{path: [offset, size]}][concatenated data]` — a **flat path index**, parsed by
`ClasspathBlob.loadEntries` (`ClasspathBlob.scala:24-50`).

`ExtraLib.fromArchive` does a two-way split:

```scala
// ExtraLib.scala:33-36
def fromArchive(name: String, buffer: ArrayBuffer): ExtraLib =
  val entries = ClasspathBlob.loadEntries(buffer)
  val (ir, cp) = entries.partition((path, _) => path.endsWith(".sjsir"))
  new ExtraLib(name, ClasspathBlob.dirFromEntries(s"(extra-lib $name)", cp), ir.toMap)
```

Everything that is not `.sjsir` becomes the compiler classpath `VirtualDirectory`
(`cpDir`). **A `.js` entry today silently lands in `cpDir`** — harmless to the
compiler (`VirtualDirectoryClassPath` ignores unknown extensions) but it would
also show up in `ExtraLib.shadowedPaths` collision warnings
(`ExtraLib.scala:41-49`).

### Naming convention — assessment

**Recommend the reserved prefix `js-modules/<moduleName>.js`**, with module name
= `path.stripPrefix("js-modules/").stripSuffix(".js")`.

- It needs no new format, no new header, no version field — the flat path index
  already carries arbitrary strings.
- It round-trips module names containing `/` (scoped packages: `js-modules/@ac
  me/pkg.js` → `@acme/pkg`) and `-` unchanged.
- It is self-describing when someone dumps the index.
- The prefix is reserved as a *namespace*, so a later `js-modules/.manifest.json`
  (for multi-file bundles, source maps, an explicit name→path map, or a
  `"main"`-style entry) can be added inside it **without breaking the
  convention**: "if `js-modules/.manifest.json` is present, it wins; otherwise
  derive names from paths."

A manifest-first design is more flexible but buys nothing for the single
self-contained-bundle case that motivates this, and it adds a second source of
truth. Start with the prefix.

### Where loading + registration happens in the worker boot

Boot chain today: `JsonMain.main` → `ReplBootstrap.createSessionFromEnv`
(`ReplBootstrap.scala:26-34`) → `loadExtraLibs` (`ReplBootstrap.scala:60-66`) →
`ReplSession.create` (`ReplSession.scala:208-215`) → `runner.loadLibrary(...)`
(`ReplSession.scala:213`).

**Register the JS modules in `ReplSession.create`, immediately before
`runner.loadLibrary` (`ReplSession.scala:213`).** Rationale: `loadIRFiles` runs
static initializers eagerly (`Interpreter.scala:73-74`), and a static initializer
*could* touch a `@JSImport` facade — so the registry must be populated before any
IR is loaded, not merely before the first line runs.

Two properties fall out for free:

- The registry lives on `globalThis`, so it **survives `:reset`** — which only
  rebuilds the `Interpreter` and reloads IR (`InterpreterRunner.reset:53-56`).
  No re-registration needed, and no double-execution of module top-level code.
- `ExtraLib` gains a third field (`jsModules: Map[String, String]`), threaded
  through `ReplBootstrap.loadExtraLibs` unchanged.

### How to load the bytes into a live namespace object — assessment

The worker is **`ModuleKind.NoModule`** (the fork's `DottyJSPlugin` only *offers*
`switchToESModules` as an opt-in autoImport value at
`project/DottyJSPlugin.scala:16-17`; `projectSettings` never applies it, and
`repl-js-json` sets only `ESFeatures` at `project/Build.scala:2697`). Confirmed
from the built artifact: `repl-js-json/…/scala3-repl-json-fastopt/main.js` opens
with `(function(){ 'use strict'; var $fileLevelThis = this;` and ends with
`}).call(this);` — the NoModule IIFE shape.

**Consequence: the worker's own compiled code has a real Node CJS `require` in
lexical scope.** Under `NoModule`, `js.Dynamic.global.require` compiles to a bare
`require` identifier, which Node's CJS module wrapper supplies. Verified in the
emitted bundle: `var fs = require("fs");` inside
`ReplBootstrap.readArrayBuffer` (source: `ReplBootstrap.scala:117-119`;
also `:92, :97, :101`). The interpreter itself uses `require("vm")` for the same
reason (`Interpreter.scala:80-90`).

Options, assessed:

| mechanism | verdict |
|---|---|
| **`new js.Function("exports","module","require", code)`** | **Recommended.** Pure language feature, no Node API, no disk, no cleanup, identical in SEA and non-SEA. Give the bundle a real name by appending `\n//# sourceURL=<module>.js` — V8 honours `sourceURL` for `Function`/`eval` bodies, so stack traces stay readable. |
| temp file + `require(path)` | Works (auk's `globalThis.require` is a `createRequire`'d real require), and gives native source-map support — but reintroduces a temp-dir/content-hash cache exactly like the SEA asset cache auk already had to build (`ReplArtifacts.extracted:101-126`). Not worth it. |
| `require("vm")` + `new vm.Script(code, {filename})` + `runInThisContext()` | Equivalent to option 1 but with a real filename for stack traces/source maps and no temp file. `vm` is provably reachable (`Interpreter.scala:82`). **Good upgrade path** if stack quality ever matters; not needed on day one. |
| dynamic `import()` | Rejected: async (the dynamic-`eval` path is synchronous, see RECON3 Q7/RISKS), and inside a SEA `import()` resolves only built-ins (auk documents exactly this at `ReplArtifacts.scala:135-140`). |

Code-generation-from-strings is **already** required by this stack — the
interpreter's `getJSGlobalRef` is `js.eval` (`Executor.scala:605-606`) and
`setJSGlobalRef` builds a `new js.Function` (`Executor.scala:608-612`) — so
option 1 adds no new runtime capability requirement, including under SEA.

What `require` to hand the bundle: `js.Dynamic.global.require` from fork code
resolves correctly in **both** hosting shapes — the fork's own tests
(`node main.js`, bare CJS `require` in module scope) and auk's bootstrap (which
sets `globalThis.require = createRequire(worker)` before `require`-ing the worker,
`ReplArtifacts.scala:141-149`).

### Proof (executed, Node v26.3.1)

A standalone script reproduced the exact resolution the interpreter performs —
`(0,eval)(ref)` followed by a fold of property gets — against a registry
populated by the `new Function` CJS wrapper:

```
named member  -> hit:foo:function          // Global("__replJSModules", ["auk-grep-engine","search"])
namespace obj -> [ 'search', 'version' ]   // Global("__replJSModules", ["auk-grep-engine"])
unknown module -> no JS module 'nope' registered (have: auk-grep-engine)
```

The bundle's own `require("node:fs")` resolved through the passed-in `require`.
The third line comes from wrapping the registry in a `Proxy` whose `get` trap
throws a named error for unregistered modules — **strongly recommended**, because
without it an unknown module yields `undefined` and then a bare V8 `TypeError:
Cannot read properties of undefined`.

---

## Q5 — Auk side

### Worker spawn

`auk/src/main/scala/auk/platform/js/ReplArtifacts.scala` owns the whole spawn
spec:

- Artifact names — `repl-worker.js`, `classpath.bin`, `linker-libs.bin`,
  `library.bin` — at `ReplArtifacts.scala:38-43`.
- Resolution order at `ReplArtifacts.scala:45-57`: `$AUK_REPL_DIR` → SEA assets
  (extracted to a content-tagged temp cache keyed by the `repl-manifest` asset,
  `:71-80`, `:101-126`) → `vendor/repl/` under cwd.
- argv: `[execPath, <bootstrap.mjs>] ++ ["--classpath", <dir>/library.bin]`
  (`ReplArtifacts.scala:64-69, 86-87`); in SEA, `[execPath, "--repl-worker",
  "--classpath", …]` (`:77-80`).
- env: `AUK_REPL_WORKER_JS`, `DOTTY_CLASSPATH_BIN`, `DOTTY_LINKER_LIBS_BIN`
  (`ReplArtifacts.scala:89-94`). Note `library.bin` travels by **`--classpath`
  argv**, not `DOTTY_EXTRA_LIBS_BIN`.
- The bootstrap shim (`ReplArtifacts.scala:141-149`) is the key existing hook:

```js
import { createRequire } from "node:module";
const worker = process.env.AUK_REPL_WORKER_JS;
const req = createRequire(worker);
globalThis.require = req;
req(worker);
```

  This is why auk's *interpreted* library can call
  `js.Dynamic.global.require("node:fs")` (`library/src/main/scala/auk/library/AukImpl.scala:9-12`)
  at all: inside the interpreter that lowers to `Executor.getJSGlobalRef("require")`
  = `js.eval("require")`, which needs `require` to be a **globalThis property** —
  and this shim makes it one. Same for
  `WorkflowClient.scala:29` and `TeamClient.scala:31` (`require("node:net")`).
  Its SEA twin is packageBinary's `entry.mjs` `--repl-worker` branch
  (`auk/build.sbt:426-433`), and there is a third copy for the library test jsEnv
  (`auk/build.sbt:148`: `globalThis.require = require;`).

**This shim is also the Tier-0 hook**: adding
`globalThis.__aukGrepEngine = req(enginePath)` before `req(worker)` would make a
`globalFallback` facade work with **zero fork changes** — at the cost of a 5th
vendored artifact rather than an in-archive one.

### Risk carried by that shim

Any new artifact must be added in **three** places that must stay in sync:
`ReplArtifacts.Files` (`:43`) + `BootstrapSource` (`:141-149`) + build.sbt's SEA
`entry.mjs` (`:426-433`), plus the SEA asset list. Putting the bytes *inside*
`library.bin` avoids all three — which is the real argument for option 3 over
Tier 0.

### `packLibraryBin` — adding an entry is a one-liner

`auk/build.sbt:313-344` (identical in the `auk-grep` worktree). It compiles
`library` **and `grep`** (`:315-316`), collects from **both** class directories
(`:320-323`) — `.tasty` (API for the REPL compiler), `.sjsir` (code for the
interpreter), and `.class` only where no `.tasty` sibling exists (`:327-336`) —
sorts by path, and writes `vendor/repl/library.bin` (`:339-341`).

The writer is `writeBinArchive(entries: Seq[(String, File)], out: File)`
(`build.sbt:74-98`) and emits exactly the `ClasspathBlob` format: 4-byte BE index
length (`:89-92`), UTF-8 JSON index `{"<rel>":[offset,size],…}` (`:76-86`), then
concatenated data (`:94`). Index keys are the paths relative to the class
directory, e.g. `auk/library/AukImpl.tasty`, `auk/grep/Matcher.sjsir`.

**So adding the bundle is: append one `("js-modules/auk-grep-engine.js",
<linkedFile>)` pair to `entries` before `writeBinArchive`.** No format change,
no index change, no worker-side format negotiation. (Precedent for a content tag
if one is later wanted: `vendorWebUI` at `build.sbt:362`.)

### `grep/` subproject — what has to change

```scala
// auk-grep worktree, build.sbt:138-153
lazy val grep = (project in file("grep"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    name := "auk-grep",
    scalaJSUseMainModuleInitializer := true,
    Compile / mainClass := Some("auk.grep.bench.Bench"),
    …)
```

- **No `scalaJSLinkerConfig`** → it inherits the Scala.js default
  **`ModuleKind.NoModule`**. The configs at `build.sbt:218-223` (root),
  `:608-609` and `:630-631` (`webui`, `webui-dev`) are all project-scoped, not
  `ThisBuild`, so neither `grep` nor `library` inherits them. Worth knowing: the
  root auk binary is a **WebAssembly** Scala.js build
  (`withExperimentalUseWebAssembly(true)`, ESModule, ES2017, `:218-223`) — the
  companion bundle must of course be plain JS, since it is loaded into the
  worker by `new Function`.
- **Only `library` depends on `grep`** (`build.sbt:163`); the root project does
  not. The engine exists solely to back the REPL library's grep/glob/walk API,
  so the facade is the single consumer and there is no third copy to reconcile.
- **`scalaJSUseMainModuleInitializer := true`** with the benchmark as `mainClass`
  (`:145-146`) — that is right for `grepBench` (`addCommandAlias("grepBench",
  "grep/run")`, `:155`) but **wrong for a companion bundle**: the linked output
  would run `Bench.main` on load.
- **No `@JSExportTopLevel` anywhere in `grep/`** — nothing is currently exposed
  to JS.

**Recommendation: do not repurpose `grep`; add a thin `grepEngine` project** that
`dependsOn(grep)`, holds a small `@JSExportTopLevel` surface, and links with
`scalaJSUseMainModuleInitializer := false`,
`ModuleKind.CommonJSModule`, `ESVersion.ES2018` (matching the fork's pin) and the
optimizer on. Under `CommonJSModule`, `@JSExportTopLevel("search")` emits
`exports.search = …`, which is exactly the namespace object the facade's
`JSImport.Namespace` resolves to. Have `packLibraryBin` assert the link produced
a **single** file and fail loudly otherwise (default `FewestModules` splitting
emits one file absent `js.dynamicImport`).

### The motivation, sharpened

`library.dependsOn(grep)` (`build.sbt:163`) and `packLibraryBin` already collects
`grep`'s `.sjsir` (`:322`). **The grep engine therefore already ships inside
`library.bin` today — as IR, executed by the interpreter.** Option 3 is not about
getting grep code into the worker; it is about getting it there as **linked JS**
so it runs at native speed instead of paying the interpreter tax.

Corollary worth deciding explicitly: once the library calls the facade instead of
`auk.grep.*`, grep's `.tasty`/`.sjsir` entries become dead weight in the archive
— unless REPL users are meant to keep calling `auk.grep` Scala APIs directly, in
which case keep them and accept that two copies of the engine ship.

---

## Q6 — Risks and surprises

- **`js.eval` runs in global scope, not module scope (MED, already mitigated).**
  Interpreted code cannot see the worker's lexical `require`; it only sees
  `globalThis`. Verified on Node v26.3.1: `typeof require === "function"` but
  `(0,eval)("require")` throws `ReferenceError`. Everything registered for
  interpreted code must be a **`globalThis` property**. Auk's bootstrap already
  establishes this pattern; the registry must follow it
  (`globalThis.__replJSModules = …`, matching how `InterpreterRunner.resetBridge`
  installs `globalThis.__replRenders`, `InterpreterRunner.scala:71-72`, and the
  comment there explaining why the bare-global form fails under Node's strict
  mode).

- **Unregistered module → opaque `TypeError` (LOW, mitigated by the Proxy).**
  After a Tier-2 rewrite, a module that was never registered resolves to
  `undefined` and then fails on the next property get. Wrap the registry in a
  `Proxy` (validated above) so the error names the module and lists what *is*
  registered.

- **Artifact generation trap (HIGH — the most likely way this breaks).**
  `repl-worker.js`, `classpath.bin`, `linker-libs.bin` and `library.bin` must be
  rebuilt coherently. A `library.bin` carrying `js-modules/` entries loaded by an
  **old** `repl-worker.js` fails *silently at pack time and loudly at first use*:
  the `.js` entries fall into `cpDir` (`ExtraLib.scala:35`, the non-`.sjsir`
  branch), nothing registers them, and the facade throws either
  `"Imports are currently not supported"` (plain `@JSImport`) or a
  `ReferenceError`/`TypeError` on the missing global (`globalFallback` form).
  Mitigation: bump the SEA `repl-manifest` content tag (`ReplArtifacts.scala:42,
  72-76`) whenever the worker changes, and have the worker fail loudly and
  by name when it sees `js-modules/` entries it cannot handle (i.e. land the
  worker-side recognition **before** shipping a `library.bin` that uses it).

- **Name collisions across archives (LOW).** `ExtraLib` is documented
  first-wins on both the classpath and interpreter sides
  (`ExtraLib.scala:18-21`). JS modules should follow the same rule, with a
  warning on stderr mirroring `ExtraLib.shadowedPaths`
  (`ReplSession.scala:211-212`).

- **`.js` entries currently pollute `cpDir` (LOW).** Until the three-way
  partition lands, a `.js` in an archive is a stray compiler-classpath file and a
  potential spurious "shadowed" warning.

- **Module top-level code runs once, at boot, outside the interpreter (MED,
  by design).** The bundle executes as real JS in the worker's realm — it does
  **not** go through the interpreter's heap, `Semantics`, or `Stack`. That is the
  entire point (it is why the grep engine would be fast), but it means: no
  interpreter stack frames in its exceptions, its state is invisible to `:reset`,
  and anything it throws surfaces as a raw JS exception. Boot cost is paid on
  every worker start.

- **CommonJS vs ESM (LOW, resolved).** The worker is NoModule/CJS-hosted, so a
  CommonJS bundle is the natural shape and `new Function`/`vm` load it directly.
  An ESM bundle would **not** work by this mechanism (no synchronous ESM
  evaluation) — so auk's grep bundle must link as `CommonJSModule` (or
  `NoModule` + a top-level export), not `ESModule`.

- **Interactions with the parked `linked-repl` branch: none expected.** That work
  branches at `JSReplDriver.runWrapper` (`JSReplDriver.scala:216`); this work
  touches `ExtraLib`/`ReplSession.create`/`InterpreterRunner.toIRFiles`. The one
  note for the future: a linked image would resolve `Import` specs through the
  *real* linker/module system, so a Tier-2 rewrite must not be applied on the
  linked path.

- **Not a risk: native class instantiation.** `new`-ing a `@js.native @JSImport`
  class goes through `loadJSConstructor` → the same
  `loadJSNativeLoadSpec` (`Executor.scala:354-358`), then
  `js.Dynamic.newInstance`. Namespace objects and constructors are equally
  supported.

---

## Recommended design

Four tiers; **recommend landing Tier 1, then Tier 2.**

### Tier 0 — no fork change (fallback / spike only)

Ship the bundle as a 5th vendored file; auk's bootstrap shim installs
`globalThis.__aukGrepEngine`; the facade uses
`@JSImport("auk-grep-engine", JSImport.Namespace, globalFallback = "__aukGrepEngine")`.
Works **today**, against the current `repl-worker.js`. Use this to de-risk the
grep engine itself before touching the fork. Cost: a 5th artifact to keep in sync
across three code paths (see Q5).

### Tier 1 — `.js` in the archive, resolved via the global fallback

Fork learns to carry and register JS modules; **no IR rewriting, no interpreter
change.** The facade keeps a `globalFallback`:

```scala
@js.native
@JSImport("auk-grep-engine", JSImport.Namespace,
          globalFallback = "__replJSModules.aukGrepEngine")
object GrepEngineJS extends js.Object:
  def search(pattern: String, root: String, opts: js.Object): js.Array[js.Object] = js.native
```

Resolution in the REPL: `ImportWithGlobalFallback` → `Executor.scala:374-375` →
`Global("__replJSModules", List("aukGrepEngine", "search"))` → works. Resolution
in a real Scala.js build of auk: the actual module import. **One facade, both
worlds.** Note the fallback path segment must be a JS identifier
(`aukGrepEngine`), while the module name may be anything (`auk-grep-engine`).

### Tier 2 — plain `@JSImport`, via a fork-side IR rewrite

Drop `globalFallback` from the facade; the worker rewrites
`Import(module, path)` → `Global("__replJSModules", module :: path)` as IR enters
the interpreter. ~50 LOC, confined to `repl-js`, no interpreter fork
(feasibility established in Q1). Leave `ImportWithGlobalFallback` **untouched**
so the standard library keeps its current, working behaviour.

### Tier 3 — patch the interpreter (not recommended)

Fork/vendor `be.doeraene:sjsir-interpreter` to thread a resolver into
`Executor.loadJSNativeLoadSpec`. Architecturally cleanest, but it means owning a
third-party artifact for a problem Tier 2 solves in 50 lines. Revisit only if the
rewrite proves leaky.

### Fixed choices across tiers

- **Registry:** `globalThis.__replJSModules`, a `Proxy` over a plain object whose
  `get` trap throws `no JS module '<name>' registered (have: …)`.
- **Archive convention:** `js-modules/<moduleName>.js`; `js-modules/.manifest.json`
  reserved for a future explicit index.
- **Loader:** `new js.Function("exports", "module", "require", code + "\n//# sourceURL=" + name + ".js\n")`,
  called with a fresh `{exports:{}}` module object and `js.Dynamic.global.require`.
- **Registration point:** `ReplSession.create`, before `runner.loadLibrary`
  (`ReplSession.scala:213`).
- **Precedence:** first-wins across archives, with a stderr warning, mirroring
  `ExtraLib.shadowedPaths` (`ReplSession.scala:211-212`).

---

## Implementation sketch

### FORK work (`/Users/linyxus/Workspace/scala3-js`, branch `compiler-js`)

1. **New `repl-js/src/dotty/tools/repl/JSModuleRegistry.scala`** — installs the
   `globalThis.__replJSModules` Proxy (idempotent), and
   `def register(name: String, code: String): Unit` doing the `new js.Function`
   CJS wrap. First-wins; returns whether it registered.
2. **`ExtraLib.scala:23-36`** — add `val jsModules: Map[String, String]` and make
   `fromArchive` a three-way partition: `.sjsir` → `sjsir`;
   `js-modules/*.js` → `jsModules` (key = strip prefix + suffix, decode UTF-8);
   rest → `cpDir`.
3. **`ReplSession.scala:208-215`** — before `runner.loadLibrary`, iterate
   `extraLibs.flatMap(_.jsModules)` in order, `JSModuleRegistry.register`, and
   warn on stderr for duplicates (next to the existing `shadowedPaths` warning).
4. **Tier 2 only — `InterpreterRunner.scala`**: add a `TreeIRFile extends
   IRFileImpl` and a `rewriteImports(cd: ClassDef): ClassDef` (rebuild via
   `ClassDef.apply` changing `jsNativeLoadSpec` + `jsNativeMembers.map(_.copy(...))`);
   apply it in `toIRFiles` (`:36-39`) and in `registerEvalClasses` (`:105-113`).
   Skip the rebuild entirely when no `Import` is present (cheap `exists` check)
   so the stdlib load path is untouched.
5. **Tests**: a new `repl-js/test-resources/eval/*.check` transcript plus an
   `ExtraLibsTests` case building a tiny archive with a `js-modules/` entry and
   evaluating a line that calls into it. Keep `sbt scala3-repl-json-sjs/test`
   (106 baseline) and `sbt scala3-repl-cli-sjs/test` green.
6. Re-emit `classpath.bin` / `linker-libs.bin` unchanged; only
   `repl-worker.js` (the `fullLinkJS`/`fastLinkJS` main) changes.

### AUK work (`/Users/linyxus/Workspace/auk`, grep work in the `auk-grep` worktree)

1. **New `grepEngine` project** in `build.sbt` (next to `grep` at `:138-153`):
   `.dependsOn(grep)`, `scalaJSUseMainModuleInitializer := false`,
   `scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule)
   .withESFeatures(_.withESVersion(ESVersion.ES2018)))`. Do **not** change
   `grep` itself — `grepBench` (`:155`) depends on its main-module-initializer
   setup.
2. **Export surface** — a small `auk/grep/js/GrepEngineExports.scala` in the new
   project with `@JSExportTopLevel` entry points (they become `exports.*` under
   CommonJSModule). Keep it deliberately narrow and JS-typed
   (`js.Array`/`js.Object`), since values cross the interpreter boundary as raw
   JS.
3. **Facade** in `library/` — `@js.native @JSImport("auk-grep-engine",
   JSImport.Namespace, globalFallback = "__replJSModules.aukGrepEngine")` for
   Tier 1; drop the `globalFallback` argument once Tier 2 lands.
4. **`packLibraryBin`** (`build.sbt:313-344`) — depend on
   `(grepEngine / Compile / fullLinkJS)`, assert it produced exactly one `.js`,
   and append `("js-modules/auk-grep-engine.js", thatFile)` to `entries`
   (`:327-336`) before `writeBinArchive` (`:341`). One pair; no format change.
5. **`vendorRepl`** (`build.sbt:301-310`) — re-vendor `repl-worker.js` from the
   fork build, and bump the SEA `repl-manifest` tag so extracted caches are
   invalidated (`ReplArtifacts.scala:42, 72-76`).
6. **Decide on the duplicate engine** — whether to keep packing `grep`'s
   `.tasty`/`.sjsir` (`build.sbt:322`) once the library routes through the
   facade.
7. **Ordering discipline** — land and vendor the fork worker **first**, then the
   `library.bin` that depends on it (see the generation trap in Q6).

Test topology reminder (auk memory `auk-test-topology`): root `sbt test` runs
neither `library/` nor `grep/` tests — run `sbt library/test` and `sbt grep/test`
too, and `packLibraryBin` after any `library/` **or** `grep/` edit.

---

## RISKS (severity)

- **HIGH — artifact generation coherence.** `library.bin` with `js-modules/` +
  old `repl-worker.js` = a confusing runtime failure. Land the worker first; bump
  `repl-manifest`; make the worker's failure message name the module.
- **MED — module code runs outside the interpreter.** Real JS in the worker
  realm: no interpreter stack frames, invisible to `:reset`, boot cost per worker
  start, and raw JS exceptions crossing back into interpreted code.
- **MED — `js.eval` global-scope asymmetry.** Anything interpreted code must see
  has to be a `globalThis` property; the lexical `require` the worker itself uses
  is invisible to it. Verified experimentally.
- **MED — Tier-2 IR rewrite must stay narrow.** Rewrite only `Import`, never
  `ImportWithGlobalFallback`; skip untouched ClassDefs; do not apply it on any
  future linked path.
- **LOW — bundle must be CommonJS.** An ESM bundle cannot be loaded
  synchronously by this mechanism.
- **LOW — name collisions / `.js` in `cpDir`.** Mirror `ExtraLib`'s first-wins
  rule and warn.
- **LOW — no existing test coverage.** `@JSImport` is entirely untested in the
  fork's REPL suites today; new transcripts are needed rather than adjusted.
