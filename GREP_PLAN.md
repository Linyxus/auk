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
- Stages 4-6: not started. Next: stage 4 (whole-content matching), guarded
  by the stage-3 harness.

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

## Stage 6 — re-baseline, decide on parallelism

After stage 5 the remaining gap to parallel rg is core count. A persistent
`worker_threads` pool behind an `Atomics.wait` sync facade is feasible (the
REPL worker is precedent) but is real complexity with its own failure modes.
Decision gate: add a monorepo-scale corpus tier (generated on demand, not in
the default bench) and only build the pool if that tier still hurts in
practice. `rg -j1` parity is the honest target for a single-threaded engine;
matching parallel rg is optional.

## Invariants that hold at every stage

- `sbt grep/test`, `sbt library/test`, `sbt packLibraryBin`, root `sbt test`
  all green before commit (the packed REPL path loads grep's IR — a grep edit
  without repacking fails at runtime, not build time).
- Bench match counts identical to rg on every row, both corpora (walk rows
  exempt: their divergence is the datum until stage 2 converges it).
- Corpus bytes are sacred: generation is deterministic (seeded LCG, no
  clock, no randomness) and any change to generated bytes bumps the corpus
  tag. Beware invisible bytes in `Bench.scala` — the blob separator is a
  literal NUL escape; if git ever diffs the file as binary, hexdump it.
- The library API surface changes only in stage 2, and only additively.
