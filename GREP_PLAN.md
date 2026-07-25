# auk-grep optimization plan

Goal: make `lib.fs` search (grep / glob / walk) ripgrep-fast **without vendoring
ripgrep**. We build our own engine in the `grep/` subproject; rg serves as the
benchmark baseline and the correctness oracle, never as a dependency.

## Principles

- **Steal ripgrep's strategy, not its code.** rg is fast because it avoids
  work (ignore rules, early binary detection) and rejects files at byte level
  before doing anything expensive. Both strategies port cleanly to Node, where
  the primitives are already native-speed: `Buffer.indexOf` is memchr/SIMD
  under the hood, and V8's RegExp is a JIT'd engine. We do not write a regex
  engine and we do not hand-roll SIMD.
- **Every stage is benchmark-gated.** `sbt grepBench` (two cached corpora:
  *clean* = raw engine speed, *dirty* = work avoidance) runs before and after
  each stage; the numbers go in the commit message. A stage that doesn't move
  its target number doesn't merge.
- **rg is the oracle.** Match counts are cross-checked against rg on every
  bench run, and a differential test suite (stage 3) guards the risky matcher
  changes. Semantics stay pinned by `GrepSuite` + the `library` suites.
- **One writer per file, one engine.** No parallel variants kept alive except
  the per-line reference path, which survives as the fallback and the
  differential baseline.

## Status

- **Stage 0 — benchmark v2: DONE** (commit `97e9d82`).
- **Stage 1 — Buffer-first reads + binary sniff: DONE** (commit `4302732`).
  Dirty corpus 163/157/145 -> 104/112/103 ms; clean unchanged; counts exact.
- **Stage 2 — ignore-aware pruning: DONE** (commit `35cc5e9`).
  Dirty corpus 104/112/103 -> 17/20/16 ms and walk 4420 -> 362 files — from
  ~15-19x behind rg at stage 0 to within ~2x, and the pruned walk now beats
  `rg --files` wall-clock (2 vs 6 ms). Library grew walkAll/globAll/grepAll.
- **Stage 3 — differential harness: DONE** (commit `9f207d3`).
  DifferentialSuite: seeded trees + dialect-safe pattern AST vs rg as oracle,
  ~110 patterns in ~0.9 s, replayable failures, assume-skips without rg. The
  current per-line matcher agrees with rg on every generated and handpicked
  pattern — the baseline stages 4-5 must preserve.
- **Stage 4 — whole-content matching: DONE** (commit `c946047`), with one
  forced design change: `Pattern.MULTILINE` needs an ES2018 Scala.js linker
  target, and the REPL worker re-links library IR at the fork's own lower
  target — so the fast path uses a plain no-flags `Pattern` and bare `^`/`$`
  anchors are hazard-routed to the reference instead. No ES dependency,
  identical behavior in every environment. Clean corpus vs the engine
  baseline: rare 90 -> 30 ms, common 112 -> 64 ms, regex 77 -> 32 ms (ties
  rg -j1). Verified by the differential oracle incl. a 2400-pattern soak.
  Engine rule going forward: nothing that needs the worker's linker above
  its baked-in ES target (no MULTILINE, no ES2018+ regex features).
- Stage 5: not started. Its design is settled (see its section, plus the
  implementation notes below); it was assigned but no work landed.
- **Stage 4.5 — native engine bundle: DONE** (commits `4f1bf4d`, `3911bfb`).
  Decision 2026-07-24: the fork-side linked-REPL replacement was built to
  completion and then parked (branch `linked-repl` in the scala3-js fork;
  `~/Workspace/repl-link-spike/PARKED.md` has the full story) — its dynamic-
  eval restriction clashes with auk features and its ~2 s/line relink tax is
  the wrong trade. Production speed instead comes from executing THIS engine
  as pre-linked JS inside the REPL worker, called from the interpreted
  library over one JS-interop call per operation. Delivered 3-12x across the
  board; every row except the two match-volume-bound ones now runs within a
  few ms of the linked engine. See the stage 4.5 section for the table, the
  two missed targets, and why they are unreachable without the deferred lazy
  result type.
- **Stage 6 pre-work — interpreter-mode benchmark: DONE.**
  `sbt grepInterpBench/run` (the `grep-interp-bench/` subproject, or just
  `sbt grepInterpBench`) drives the same corpora and patterns through the
  *production* path: the packed `library.bin` executed by the REPL worker's
  sjsir interpreter, timed inside the evaluated snippet so worker compile time
  is excluded. It generates the corpora itself when they are absent, so it can
  run before `grepBench`, and any drift from its pinned match counts fails the
  command. Findings in the stage 6 section below.

Baseline established by stage 0 (M-series laptop, warm cache, medians):

| corpus | pattern | auk | rg | rg -j1 |
|--------|---------|-----|-----|--------|
| clean  | rare literal | 90 ms | 15 ms | 17 ms |
| clean  | common word  | 112 ms | 22 ms | 49 ms |
| clean  | regex        | 77 ms | 18 ms | 31 ms |
| dirty  | rare literal | 163 ms | 9 ms | 8 ms |
| dirty  | common word  | 157 ms | 10 ms | 14 ms |
| dirty  | regex        | 145 ms | 10 ms | 10 ms |
| dirty  | walk (files listed) | 10 ms / 4420 files | 5 ms / 361 files | — |

Reading the table: the clean-corpus gap (~4-6x) is engine speed — per-line
allocation churn and no prefilter (stages 4-5). The extra dirty-corpus gap
(~15-19x) is avoidable I/O — the engine reads 4420 files where rg reads 361
(stages 1-2). Real-world trees are far junkier than the 52 MB dirty corpus,
so the absolute wins scale up from these ratios.

## Stage 0 — benchmark v2 (DONE, `97e9d82`)

Added the *dirty* corpus: a 360-file real tree buried under gitignored junk
(4000 node_modules-style files, an 8 MB single-line min.js, a 32 MB NUL-marked
binary, a `.git` stub so rg honors the `.gitignore`). Junk is generated from a
word pool disjoint from all three benchmark patterns, so junk contributes zero
matches: both engines report **identical counts** while doing very different
work — the timing gap is a pure measurement of work avoidance, and the count
cross-check stays meaningful. Walk rows (`Walker.walk` vs `rg --files`)
quantify the pruning opportunity directly (4420 vs 361 files).

## Stage 1 — Buffer-first reads, early binary sniff

Replace `readFileSync(utf8)` in the search path with an fd-based read: open,
read the first 8 KB, sniff for a NUL byte, and close-and-skip binaries without
reading or decoding the rest. Text files read the remainder into one Buffer
and decode once. Today a 32 MB binary is fully read *and* fully UTF-8→UTF-16
decoded just to be discarded; after this stage it costs one small read.

- Semantics: unchanged. Same NUL rule (dir-wide search skips binaries;
  an explicitly-named file is always searched), no size cap (rg has none;
  parity is worth more than a cap once big files are cheap).
- Measure: dirty-corpus rows (the blob dominates); clean big.log unaffected.
- Verify: `GrepSuite` green, bench counts identical.

## Stage 2 — ignore-aware pruning (the headline stage)

`Walker` learns ignore rules:

- Always skip `.git` (by name), everywhere.
- Parse `.gitignore` per directory **from the search root down — never
  consulting parent directories or global config**. An explicitly-named root
  is therefore always searched, even if some outer tree ignores it: rg's
  `--no-ignore` gotcha (searching inside node_modules finds nothing) is
  deliberately not reproduced.
- Supported pattern subset: comments/blanks, `*` `**` `?` (the `Glob`
  translation exists), leading-`/` anchoring, trailing-`/` dir-only,
  `!` negation with last-match-wins. Exotica (`\#` escapes, `[]` classes)
  can land later; the subset covers real repos.
- Default **on** for `grep`/`glob`/`walk`, with an escape hatch. The exact
  library surface (an `ignored`-style parameter vs. `walkAll`/`grepAll`
  methods) is decided at implementation review; `AukInterface` docs must be
  updated either way (they are embedded in the agent's system prompt).
- Free riders: `realpathSync` only for symlinked dirs (regular dirs cannot
  form cycles — removes one syscall per directory), and a visitor-style walk
  core so matching starts during traversal instead of after building the
  full entry list.

- Measure: dirty walk row should collapse toward rg's 361 files; dirty grep
  rows toward their clean-corpus ratios. This is where "minutes on real
  trees" dies.
- Verify: new `GrepSuite` ignore cases (negation, anchoring, dir-only,
  nested files, ignored-root-still-searched, `.git` always skipped);
  `library` suites (which create no ignore files) stay green unchanged;
  `packLibraryBin` + root suite for the packed path.

## Stage 3 — differential harness

A dev-only suite (skipped when rg is absent): generate random small trees and
random patterns from a generator restricted to syntax that means the same
thing in both regex dialects; run engine vs `rg` and assert identical
`(path, line)` sets. Runs under `sbt grep/test` via an rg-availability
`assume`. This lands *before* the matcher changes because stages 4-5 are
where plausible-but-wrong is easiest to ship.

## Stage 4 — whole-content matching

Replace the per-line pipeline (split to `List[String]`, `zipWithIndex`, a
tuple and a fresh matcher per line) with one `Matcher.find()` loop over the
whole decoded content — same Scala.js `java.util.regex` emulation, so the
pattern dialect is unchanged — with `MULTILINE` added so `^`/`$` keep
per-line meaning, and line numbers computed by counting newlines up to match
offsets.

Correctness shape: a full-content match can span lines (`\s` matches `\n`),
so full-content matches are only **candidates** — each candidate's line is
confirmed by the existing per-line check before emitting. Patterns containing
hazardous constructs (explicit `\n`/`\r`, `\A` `\z` `\Z`, lookbehind) skip
the fast path entirely and use the per-line reference, which is kept.

- Measure: clean-corpus common-word and regex rows (allocation-bound today).
- Verify: stage 3 harness + `GrepSuite`; the reference path stays reachable
  for differential runs.

## Stage 4.5 — native engine bundle (DONE, `4f1bf4d` + `3911bfb`)

Production `lib.fs` runs the engine as `.sjsir` interpreted by the REPL worker
— 3-5x slower where native work dominates, ~45-50x where per-match interpreted
orchestration dominates (measured: `sbt grepInterpBench/run`). The fix: the
interpreted library calls a **pre-linked JS build of this same engine** over
the JS-interop boundary — the exact pattern `AukImpl` already uses for Node
natives, moved up from "one call per readdir" to "one call per search".

Design (settled in discussion; implementation decides details at review):

- **Build**: a `@JSExportTopLevel` facade in `grep/` (walk / walkAll / glob /
  globAll / grep ×2 / grepAll ×2 / searchFile, plus a content-hash version
  marker). New task `packGrepEngine`: `fastLinkJS` the grep project (optimizer
  ON, single-file CommonJS output) → `vendor/repl/grep-engine.js`, wired into
  `packLibraryBin` so the bundle and `library.bin` cannot drift (both sides
  check the hash marker at load).
- **Runtime plumbing**: the auk host passes the bundle path via env (e.g.
  `AUK_GREP_ENGINE`) when spawning the REPL worker (precedent:
  `AUK_TEAM_SOCK`); interpreted `AukImpl` lazily `require()`s it on first
  search op. ONE path — a missing or stale bundle is a loud error, not a
  silent fallback (no dual-engine drift). `library/test` sets the env itself and
  depends on `packGrepEngine`.
- **Marshalling**: the bundle returns plain JS arrays of `{path, line, text}`;
  `AukImpl` converts eagerly to `List[Match]` first (typical match counts make
  this sub-ms). If the interp bench's common-word row (110k matches) still
  hurts, upgrade to a lazy `Seq` view over the JS array (O(1) `length`,
  convert-on-access) — an `AukInterface` type-surface change, deferred until
  the numbers demand it.
- **Error fidelity**: the `grep: invalid regular expression ...` contract must
  cross the boundary byte-identically (pinned by GrepSuite + library suites).
- **Scope**: the corpus-scale ops (walk/glob/grep/searchFile). `Lines.split`
  and small pure helpers stay in-IR initially; route later if the bench says
  so. The engine SOURCE stays target-agnostic day one (no ES2018 features) —
  the bundle runs directly under Node, so the worker ES-target constraint no
  longer binds it, but lifting the stage-4 hazard-routing is a separate,
  later decision.
- **Acceptance**: the interpreter-mode bench rerun with the bundle active —
  target: dirty common word 549 → <40 ms, clean 3189 → <150 ms (eager
  conversion), counts exact on every row; `sbt grep/test` + `library/test` +
  `packLibraryBin` + root `sbt test` green; differential suite unaffected
  (engine source unchanged).
- **Risks**: artifact drift (hash handshake mitigates), bundle path resolution
  across dev/SEA spawn shapes (vendored file next to the other repl
  artifacts), and the standing rule — any `grep/` edit repacks BOTH artifacts.

After this stage, stage 5's prefilter and stage 6's re-baseline apply to the
mode users actually experience, 1:1.

### Result

Shipped as "option 3": the bundle rides **inside `library.bin`** rather than
as a fifth vendored artifact, so drift is structurally impossible (one task
builds both) and the hash handshake, the `AUK_GREP_ENGINE` env var and the
SEA/dev path resolution above all became unnecessary. See `NATIVE_MODULES.md`
for the recon this rests on. The fork carries the archive convention
(`js-modules/<name>.js` → `globalThis.__replJSModules.<name>`, fork branch
`native-js-modules`, `4547aa0135`, vendored here); auk carries the bundle, the
facade and the routing.

Interpreter-mode benchmark, before and after, same machine and same session
(medians; `linked` and `rg` from the same day's `sbt grepBench`):

| corpus | row | interp before | **interp after** | linked | rg | matches |
|--------|-----|--------------|------------------|--------|-----|---------|
| clean  | walk         | 36 ms   | **9 ms**   | 3 ms  | 6 ms  | 1205 |
| clean  | rare literal | 92 ms   | **30 ms**  | 30 ms | 16 ms | 12 |
| clean  | common word  | 3189 ms | **466 ms** | 71 ms | 23 ms | 110186 |
| clean  | regex        | 534 ms  | **44 ms**  | 33 ms | 18 ms | 2566 |
| dirty  | walk         | 16 ms   | **3 ms**   | 2 ms  | 5 ms  | 362 |
| dirty  | rare literal | 33 ms   | **8 ms**   | 8 ms  | 9 ms  | 6 |
| dirty  | common word  | 549 ms  | **77 ms**  | 13 ms | 11 ms | 18207 |
| dirty  | regex        | 91 ms   | **11 ms**  | 9 ms  | 10 ms | 424 |

Match counts are exact on every row (the suite asserts the grep rows against
its pins). Worker boot + preamble is unchanged at ~1.14 s.

Six of eight rows now sit within a few ms of the linked engine — on the two
rare-literal rows the interpreter tax is *gone*, exactly 0 ms. The two
common-word rows miss their targets (466 vs <150 ms, 77 vs <40 ms), and they
miss for one reason: **eagerly materializing the `List[Match]`**. Measured in
the worker for 110186 rows: a bare loop is 7 ms, reading all three fields off
each JS row is 97 ms, but allocating one 3-field object per row and consing it
into a `List` is **352 ms** on its own — against 396 ms for the shipped
conversion end to end. So the conversion runs within ~12% of the interpreter's
floor, the residual is ~89% allocation, and the marshalling shape is not the
lever: grouping rows per file or splitting them into parallel typed arrays
saves the 44 ms of field reads at best. (Cross-check from the bench itself: the
per-match cost is 3.9-4.0 µs on *both* corpora, which differ 2x in bytes and
4x in files — the time tracks match count, not scan work.)

The lever this stage deliberately left alone is the one the plan already named:
a lazy `Seq` view over the JS array, converting on access. That is an
`AukInterface` type-surface change; the numbers now argue for it, and it is the
only route below ~350 ms on the clean common-word row. Note the bench row
(`.length`) is its best case — an agent that actually reads 110k matches pays
the allocation either way.

### Implementation notes

- **`grepEngine` project** (`grep-engine/`), `dependsOn(grep)`, containing only
  `auk.grep.GrepEngineExports`. `grep` itself is untouched: `grepBench` needs
  its main-module-initializer setup, which a companion module must not have.
- **`fullLinkJS`**, not fastLink: Closure is clean under `CommonJSModule` +
  `ESVersion.ES2018` (tried first, worked). 584 KB single file; `packLibraryBin`
  asserts the output is exactly one `.js`. ES2018 costs nothing — the bundle
  runs directly under Node, so the worker's ES-target constraint (stage 4's
  MULTILINE finding) no longer binds it. Lifting the hazard routing stays a
  separate decision: the IR copy of the engine still runs under the old target.
- **Export surface**: `walk`/`walkAll`/`glob`/`globAll`/`grep`/`grepGlob`/
  `grepAll`/`grepAllGlob`/`grepFile` — JS has no overloading, so the two
  `search` arities get distinct names. Rows are `{path, dir}` and
  `{path, line, text}`; entry rows carry `dir` rather than being bare path
  strings, because the library needs the kind to pick `FsDirImpl`/`FsFileImpl`
  and splitting the list would lose walk order.
- **Error contract** crosses byte-identically. The bundle rethrows a Scala
  failure as `js.Error(message)` (a Scala `Throwable` would arrive in the
  interpreter as an opaque foreign object) and lets a raw Node error through
  untouched, keeping its `code`; the library re-raises whatever arrives as a
  `RuntimeException` with the same message. `grep: invalid regular expression
  '(unclosed': Unclosed group near index 9` reads identically on both sides.
- **`library/test`** links `NoModule`, where `@JSImport` compiles to the
  `globalFallback` read — so the suites exercise the real resolution path. Its
  prelude installs `globalThis.__replJSModules` (a plain object; the worker's
  Proxy only exists to name an unregistered module) and loads the bundle
  through the same CommonJS wrapper the worker uses. It cannot `require` it:
  this repo's `package.json` declares `"type": "module"`, so Node would read
  the linked `.js` as ESM and its `exports.*` assignments would vanish.
- **The IR copy of the engine still ships**, per plan — `Lines.split` stays
  in-IR for `FsFile.lines`, and `auk.grep.*` remains callable from evaluated
  code. That is ~1 MB of archive next to the 584 KB bundle; dropping it is a
  separate call about whether REPL users may call the engine directly.

## Stage 5 — literal prefilter

Extract a *required* literal from the pattern — conservatively: >= 3 chars,
mandatory position (not inside an alternation branch, optional group, or
class; bail on `(?i)`; when unsure, extract nothing). Encode it to UTF-8 and
`Buffer.indexOf` each file **before decoding**: no hit, no decode, no regex.
Hits proceed to the stage-4 matcher.

The extractor is the one component that can silently lie (a wrong "required"
literal drops matches), so it gets exhaustive unit tests plus a test-mode
assertion running prefiltered and unprefiltered engines side by side, on top
of the stage-3 oracle.

- Measure: clean-corpus rare-literal row — should become I/O-bound and land
  near `rg -j1`.

Settled implementation notes (from design review): `requiredLiteral` is one
linear top-level scan accumulating literal runs, longest run >= 3 wins.
Literal-yielding: plain chars and escaped specials. A `?`/`*`/`{0..}`
quantifier after a char drops that char and breaks the run; `+`/`{n>=1..}`
keeps it once, then breaks. Top-level `|` aborts. `(` skips the balanced
group (contributing nothing), EXCEPT any `(?` other than `(?:` aborts
entirely (inline flags change the rest of the pattern; lookaround and named
groups are not worth reasoning about). `[` skips the class. `.`, predefined
classes, `\b`, anchors, and `\n\r\t` escapes break the run; unrecognized
escapes abort. The needle is pre-encoded UTF-8 (`Buffer.from`), checked with
`buf.indexOf` on the raw content buffer before decoding — a false positive
from the needle straddling the content/garbage boundary of an allocUnsafe
buffer is harmless, false negatives are impossible. Prefilter applies on
both matcher paths (a required literal is required under either, anchored
included); `searchFile` skips it. Ship with a forced-prefilter-off hook for
exact equivalence tests, a ~12-case extractor pin table, and a differential
soak — the extractor is the one component that can lie silently.

## Stage 6 — re-baseline, decide on parallelism

**Pre-work landed: the interpreter-mode benchmark** (`sbt grepInterpBench/run`) —
because `grepBench` measures the *linked* engine under V8's JIT, while the
agent actually runs the engine as `.sjsir` interpreted by the REPL worker.
Same-machine medians (M-series, warm cache; interp / linked / rg).

**These are pre-4.5 numbers**, kept because they are what motivated that
stage; the `auk-interp` column is history now — see stage 4.5 for the
post-bundle table and for which of the conclusions below survived.

| corpus | row | auk-interp | auk linked | rg |
|--------|-----|-----------|------------|-----|
| clean  | walk         | 36 ms   | 3 ms  | 7 ms  |
| clean  | rare literal | 92 ms   | 27 ms | 16 ms |
| clean  | common word  | 3189 ms | 66 ms | 23 ms |
| clean  | regex        | 534 ms  | 32 ms | 19 ms |
| dirty  | walk         | 16 ms   | 2 ms  | 5 ms  |
| dirty  | rare literal | 33 ms   | 7 ms  | 10 ms |
| dirty  | common word  | 549 ms  | 12 ms | 11 ms |
| dirty  | regex        | 91 ms   | 8 ms  | 11 ms |

Worker boot + preamble is a ~1.1 s one-time cost on top. Reading the table:
the interpreter penalty is ~3-5x where native work dominates (rare literal:
native reads + one native regex scan, few matches) but ~45-50x where
per-match interpreted orchestration dominates (common word: 110k confirmed
lines ≈ ~28 µs of interpreted confirm + Match construction each). Production
is match-volume-bound, not scan-bound. Consequences for this stage:

- Relative stage-over-stage wins DID transfer (work avoidance reduces both
  native and interpreted work), but the absolute production gap to rg on
  match-heavy queries is ~50-140x, not the ~1-3x the linked bench shows.
- The biggest production lever is cutting interpreted work *per match /
  per file*, not raw scan speed: stage 5's skip-decode prefilter, and any
  trimming of per-match allocation in the hot loop, matter more than the
  linked rows suggest. Re-run this bench after stage 5.
- Worker-pool parallelism multiplies interpreted throughput too, but fix
  the 50x constant before adding cores to it.

After stage 5 the remaining gap to parallel rg is core count. A persistent
`worker_threads` pool behind an `Atomics.wait` sync facade is feasible (the
REPL worker is precedent) but is real complexity with its own failure modes.
Decision gate: add a monorepo-scale corpus tier (generated on demand, not in
the default bench) and only build the pool if that tier still hurts in
practice. `rg -j1` parity is the honest target for a single-threaded engine;
matching parallel rg is optional.

## Invariants that hold at every stage

- `sbt grep/test`, `sbt library/test`, `sbt packLibraryBin`, root `sbt test`
  all green before commit (the packed REPL path carries the engine twice —
  grep's IR and, since stage 4.5, its linked bundle — so a grep edit without
  repacking fails at runtime, not build time).
- Bench match counts identical to rg on every row, both corpora (walk rows
  exempt: their divergence is the datum until stage 2 converges it).
- Corpus bytes are sacred: generation is deterministic (seeded LCG, no
  clock, no randomness) and any change to generated bytes bumps the corpus
  tag. Beware invisible bytes in `Bench.scala` — the blob separator is a
  literal NUL escape; if git ever diffs the file as binary, hexdump it.
- The library API surface changes only in stage 2, and only additively.
