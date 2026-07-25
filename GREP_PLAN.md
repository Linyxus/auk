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
- **Stage 4.5 — native engine bundle: DONE** (commits `4f1bf4d`, `3911bfb`).
  Decision 2026-07-24: the fork-side linked-REPL replacement was built to
  completion and then parked (branch `linked-repl` in the scala3-js fork;
  `~/Workspace/repl-link-spike/PARKED.md` has the full story) — its dynamic-
  eval restriction clashes with auk features and its ~2 s/line relink tax is
  the wrong trade. Production speed instead comes from executing THIS engine
  as pre-linked JS inside the REPL worker, called from the interpreted
  library over one JS-interop call per operation. Delivered 3-12x across the
  board; every row except the two match-volume-bound ones now runs within a
  few ms of the linked engine. The two misses are marshalling-bound; the lazy
  result view that would address them is DEFERRED by explicit decision — see
  the section for the table, the floor measurement, and the revisit criteria.
- **Stage 5 — literal prefilter: DONE**, and it says something the plan did
  not expect: the clean rare-literal row went 29 -> 26 ms, not to rg -j1's
  18 ms, because **decode + regex was never that row's cost**. Measured on the
  clean corpus in plain Node: reading its 1205 files costs 15 ms (of which
  9.4 ms is open/fstat/close syscalls alone), the prefilter's own `indexOf`
  pass costs 3.5 ms, and the decode + regex the prefilter REMOVES was worth
  only 2.7 ms. The engine is now I/O- and syscall-bound on that corpus, which
  was the stage's stated goal — there was just far less to win than the
  target implied. Rows whose needle sits in nearly every file (clean common
  word "return", clean regex "handler_") would pay the extra scan for nothing,
  so the stage also ships an **adaptive give-up**: after a 16-file probe, a
  prefilter rejecting less than half of what it sees is dropped for the rest of
  that search (the measured break-even; it cannot change results, only skips).
  With it, those rows scan 16 files instead of 1202 and sit at or below their
  stage-4 timings. Dirty-corpus rows are flat throughout — stage 2's pruning
  already removed 92% of that tree. See the section for the full tables.
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

The only lever below ~350 ms on that row is the lazy `Seq` view over the JS
array, converting on access. **Decision (2026-07-25): deferred.** Reasons:
the bench row (`.length`) is the lazy view's best case — an agent that
actually iterates 110k matches pays the same allocation either way, so it
optimizes the benchmark more than agent behaviour; realistic result volumes
already sit at or near linked parity (regex row: 44 vs 33 ms); and it costs
an `AukInterface` type-surface change (`List` → `Seq`) that ripples into the
API text embedded in the system prompt. Revisit only if a real agent-usage
shape shows up that is dominated by `length`/`take`-style access to huge
result sets — that evidence, not this bench row, is the trigger. (Input to
stage 6's re-baseline.)

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

## Stage 5 — literal prefilter (DONE)

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
  near `rg -j1`. Run BOTH `sbt grepBench` and `sbt grepInterpBench/run`:
  post-4.5 the production numbers should move with the linked ones, and the
  interp bench's count pins double as the packed-path correctness check.

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

### Result

Shipped as designed; every settled rule above survived implementation. Two
guards were added on top, both about the extractor lying:

- **The U+FFFD guard.** Invalid UTF-8 decodes to U+FFFD, so a pattern
  containing that character can match decoded text whose raw bytes are
  nothing like the needle's — the one construction where a byte-level
  prefilter drops a real match. `encodeNeedle` refuses such a literal (and,
  generically, any literal that does not survive an encode/decode round trip,
  which also catches the unpaired surrogate a quantifier can leave behind).
  Pinned by a test whose file is the raw bytes `61 62 80 63 64`.
- **Dialect guards in the class and brace scanners.** The engine's reference
  regex is `java.util.regex` (Scala.js) while its fast path is a JS RegExp,
  and the two read `[]x]`, `[a[b]]`, `[a&&b]` and a non-quantifier `{` (a
  literal brace in JS, an error in Java) differently. Misreading where a class
  ENDS is the one way this scan could hand back a literal that is not
  required — the class's own contents would be read as literal text — so
  every shape the dialects disagree on abandons extraction instead.

Verification: `sbt grep/test` 45/45 (38 + 7 new: a 31-case extractor pin
table, the skip counter, the U+FFFD guard, the multi-byte needle, the 8 KB
boundary straddle, and a 39-search equivalence sweep). Differential soak at
200 patterns x 4 seeds + 10 handpicked = **810 patterns, 0 mismatches**, with
6992 files dropped by the prefilter under rg's eye. `library/test` 263/263,
root `sbt test` 987/982/5.

**The numbers, and why the target was the wrong target.** Same-session
`sbt grepBench`, prefilter off vs on (the same binary, via the test hook), rg
control rows from the same run:

| corpus | row | off | **on** | rg | rg -j1 | matches |
|--------|-----|-----|--------|-----|--------|---------|
| clean  | rare literal | 29 ms | **26 ms** | 15 ms | 18 ms | 12 |
| clean  | common word  | 65 ms | **65 ms** | 23 ms | 50 ms | 110186 |
| clean  | regex        | 32 ms | **35 ms** | 18 ms | 32 ms | 2566 |
| dirty  | rare literal | 7 ms  | **7 ms**  | 9 ms  | 7 ms  | 6 |
| dirty  | common word  | 13 ms | **12 ms** | 10 ms | 15 ms | 18207 |
| dirty  | regex        | 8 ms  | **8 ms**  | 10 ms | 11 ms | 424 |

Match counts are exact and unchanged on every row. The clean rare-literal row
was the stage target (30 -> ~18 ms, near `rg -j1`) and it missed. Direct
measurement of that corpus in plain Node, in the engine's exact read shape,
says why:

| what | cost |
|------|------|
| walk only | 1.5 ms |
| open + fstat + close, no reads at all | 9.4 ms |
| the full read path (1205 files, 24 MB) | 15.0 ms |
| + the prefilter's `Buffer.indexOf` pass | 18.5 ms |
| + decode and regex on hits (stage 5 total) | 18.4 ms |
| decode + regex on EVERY file (stage 4 total) | 21.1 ms |

So the prefilter removed 2.7 of the 21 ms it could see, and what remains is
**syscalls and bytes**, not matching: 9.4 ms of the row is open/fstat/close
before a single byte is read. The row is now I/O-bound — the stage's actual
goal — but the "near rg -j1" figure assumed decode + regex dominated it, and
they never did. (rg's 18 ms also includes ~5-10 ms of process start, so rg's
real search of this corpus is ~10 ms, under our 15 ms read floor.)

The cost side is equally clear: when the needle sits in nearly every file the
prefilter can never fire and its scan is pure overhead — clean regex
("handler_", planted in every file) 32 -> 35 ms, clean common word 65 -> 65 ms.
For a file it DOES skip the extra scan is nearly free, because the needle scan
replaces the full-buffer NUL scan the decode path would have run anyway.

Production (`sbt grepInterpBench/run`, same session, packed `library.bin`
before vs after) is unchanged within noise: clean rare literal medians 31/29/32
before, 32/33 after; clean regex 54/40/58 before, 47/45 after. That harness
takes 3 samples per cell on a ~30 ms row whose spread is +-3 ms, so it cannot
resolve a 3 ms effect — its value here was the count pins, which all held.
Stage 6 should raise its sample count if it wants to see deltas this small.

Artifact cost: the engine bundle grew 584224 -> 594238 bytes and `library.bin`
1643629 -> 1693457 (+3%), the extra being this code's IR. `Needle` and `Quant`
are plain classes rather than `case` classes for that reason — the generated
`Product` surface cost 23 KB of IR neither ever uses.

### The adaptive give-up (shipped in the same stage)

The regressions above are gone. A prefilter that is not rejecting files is
charging the search for nothing, so each search now judges its own: after a
16-file probe, a needle that has rejected less than half of what it has seen is
dropped for the remainder of THAT search. The counters live on the `Needle`,
which is built once per `kitFor` call, so the state is per-search by
construction and cannot leak between searches; and since giving up can only
stop the engine from SKIPPING a file, never from searching one, no give-up rule
can change a result.

**Half** is not a guess — it is the measured break-even. Scanning a megabyte
costs ~0.13 ms; rejecting one saves ~0.25 ms of NUL scan, decode and regex. The
prefilter pays exactly when it rejects more than half of what it sees.

**A rate is what works, and the first attempt taught us why.** The obvious rule
— give up only if the first N files ALL contain the needle — never fires on a
real tree: of the clean corpus's 1205 files, exactly ONE lacks `handler_` and
exactly one lacks `return`, and that single file (the corpus manifest) is
enough to mark the prefilter "useful" forever. Measured: with the
zero-rejection rule the regex row stayed at 34 ms. One README or LICENSE would
do the same to any real repo.

Mechanism, measured on the corpora — scans and skips per search, which is proof
independent of the timing noise:

| corpus | row | files scanned | files skipped | outcome |
|--------|-----|--------------|---------------|---------|
| clean | rare literal | 1202 | 1190 | runs all search: 99% rejected |
| clean | common word | 16 | 1 | gives up after the probe |
| clean | regex | 16 | 1 | gives up after the probe |
| dirty | rare literal | 362 | 356 | runs all search |
| dirty | common word | 16 | 2 | gives up |
| dirty | regex | 16 | 4 | gives up (25%, below break-even) |

Timing, same run, same rg control rows, give-up off vs on:

| corpus | row | give-up off | **on** | rg | rg -j1 |
|--------|-----|------------|--------|-----|--------|
| clean | rare literal | 27 ms | **25 ms** | 16 ms | 19 ms |
| clean | common word | 65 ms | **61 ms** | 23 ms | 50 ms |
| clean | regex | 34 ms | **32 ms** | 18 ms | 31 ms |
| dirty | rare literal | 7 ms | **7 ms** | 9 ms | 7 ms |
| dirty | common word | 12 ms | **12 ms** | 10 ms | 16 ms |
| dirty | regex | 9 ms | **8 ms** | 10 ms | 10 ms |

Every row is now at or below where stage 4 left it, and the rare-literal row
keeps its win. The two ubiquitous-needle rows recover 2-4 ms, at the edge of
this bench's +-2 ms noise on its own — which is why the scan counts above, not
the milliseconds, are the evidence that the mechanism fires.

### The keep decision

Recorded because it was close and the arithmetic should outlive the argument.
The prefilter is worth ~0.11 ms net per MB it rejects and costs ~0.13 ms per MB
it fails to reject, so it breaks even near 55% rejection — and the give-up now
enforces exactly that boundary at runtime, which is what turned "keep it and
accept a regression" into "keep it". Where it pays: a rare identifier over a
mid-size tree (the dominant agent-grep shape) rejects ~99%, and the saving
scales with tree size. Where it does nothing: junky real trees, because stage
2's pruning already removed 92% of the files before the prefilter sees them —
the dirty rows are flat by 0-1 ms in every table above, and that is worth
remembering before crediting this stage for the numbers users see.

**Left for stage 6**: the give-up is final for a search — it never re-samples,
so a tree whose first 16 files are unrepresentative pays for the rest of that
search. Re-sampling (scan 1 file in N while given up, and resume if the rate
recovers) is the obvious refinement and was not built, because it needs a
corpus that actually exhibits the problem to be tuned against. The larger lever
remains the read path: 9.4 ms of the clean rare-literal row is open/fstat/close
before a byte is read.

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

Worker boot + preamble is a ~1.1 s one-time cost on top. Reading the table
(at the time): the interpreter penalty was ~3-5x where native work dominates
but ~45-50x where per-match interpreted orchestration dominates — production
was match-volume-bound, not scan-bound. That finding is what motivated stage
4.5.

**Which of those conclusions survived stage 4.5:**

- "Relative wins transfer" — survived, and strengthened: with the bundle,
  production moves 1:1 with the linked bench, so `grepBench` is a faithful
  production predictor again and stage 5 needs no interp-specific reasoning.
  Qualified by stage 5: at 3 samples per cell this bench cannot RESOLVE a
  change smaller than a few ms, so "moves 1:1" is a claim about large deltas.
- "The 50x interpreted constant is the biggest lever" — RESOLVED by 4.5. The
  one surviving interpreted cost is the ~3.9 µs/match `List[Match]`
  materialization, which matters only on huge result sets and is addressable
  only by the deferred lazy view (see stage 4.5's decision + revisit
  criteria).
- "Fix the constant before adding cores" — done; the parallelism question is
  now purely about the remaining LINKED gap to parallel rg (clean common
  71 vs 23 ms).

What this stage does when it runs, post-stage-5:

- Re-baseline with both benches (`sbt grepBench`, `sbt grepInterpBench/run`)
  and refresh the tables in this file.
- Decide the worker pool: add a monorepo-scale corpus tier (generated on
  demand, not in the default bench) and only build a persistent
  `worker_threads` pool (behind an `Atomics.wait` sync facade; the REPL
  worker is precedent) if that tier still hurts in practice. `rg -j1` parity
  is the honest target for a single-threaded engine; matching parallel rg is
  optional.
- Close out the lazy-view question with stage-6 evidence: either a real
  agent-usage shape justified it, or it stays deferred.

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
