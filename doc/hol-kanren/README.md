# Higher-order logic programming for `(aux kanren micro)`

This directory holds the prototype of `(aux kanren lambda)`: a module that brings λProlog's logic into
`(aux kanren micro)`, with ∀ and ⊃ goals, λ-tree syntax and higher-order pattern unification.

**The prototype is frozen at commit f84cf9c and superseded by `src/aux.kanren.lambda.scm`.** Its files are
kept as they were, because the document cites them; they are not built or tested by `src/Makefile`. The
module was ported onto the sources of 5f406fd in four commits:

| commit | what |
|---|---|
| 26b4708 Open (aux kanren micro) to extensions | the protocol of `micro.diff`, reworked: the slot `X`, the registry of `μkanren-extension` hooks, the invariants E1–E6, `test/microkanren-extension.scm` |
| 5be5f35 Add (aux kanren lambda) | `aux.kanren.lambda.scm` on that protocol, its sorted residual forms merged up to α, `test/microkanren-lambda*.scm` |
| d501822 Move the prefixes of D to N per binding in (aux kanren lambda) | a new `watch` hook, so that a binding moves only the disequalities it reaches, where the prototype walked every prefix of D |
| e97f7ef Read λ-terms in (aux kanren micro show) | `show.diff` replaced by a `μkanren-show-extension` registry in show, which the λ module fills |

The `README.md` at the root of the repository describes the module and the protocol as they now are, and
the sources' comments are the reference. Where the prototype and the document describe micro's hooks, the
post hook, `show.diff` or the costs of the migration of D, they describe f84cf9c, not `src/`.

The document that motivates and describes the prototype is a reading of Miller & Nadathur, *Programming with
Higher-Order Logic* (CUP 2012), and of the higher-order miniKanren literature, Ma & Friedman's λKanren paper
(miniKanren 2021) in particular, ending with the proposal. It is a 413-page `tufte-book` built with LuaLaTeX,
and lives in the Overleaf project `lambda-prolog-lambda-kanren` (working copy
`../overleaf/lambda-prolog-lambda-kanren`, next to this repository's checkout). It was last in this repository
at commit 09be875. The document cites this directory's paths as `doc/hol-kanren/prototype/`.

- Part I (chapters 2–8): the book.
- Part II (chapter 9): the literature.
- Part III (chapters 10–14): the proposal, in this order: micro as it is today, the design and its extension
  protocol, the module, the book's examples run on it, then limitations, costs and the merge.

## The prototype (`prototype/`, frozen at f84cf9c)

| file | what |
|---|---|
| `micro.diff` | the generic extension protocol for `src/aux.kanren.micro.scm`: a seventh state field `X`, a registry of hooks. It applies with `git apply` to the micro of f84cf9c, not to a later one. |
| `show.diff` | the edit to `src/aux.kanren.micro.show.scm`, which reads the new answer forms. It applies to the show of f84cf9c; `src/` now has show's own registry of extensions instead (e97f7ef). |
| `aux.kanren.micro.scm`, `aux.kanren.micro.show.scm` | the two modules with the diffs applied |
| `aux.kanren.lambda.scm` | the module `(aux kanren lambda)` |
| `test/` | the acceptance and regression suite (83 cases), the limits suite (6 cases) and the book's programs |
| `micro-tests-with-lambda/` | micro's own suites, with `(aux kanren lambda)` also imported |
| `examples.scm`, `examples.out` | the core examples of the book, each next to its λProlog original, and their output |
| `DESIGN.md` | the design document that chapters 11–14 are built on |
| `bench/` | the benchmark programs and their recorded outputs (`bench.out`, `bench-r3*.out`) |

Build and test, without touching `src/` or the installed eggs, on a checkout of f84cf9c:

    ./build.sh       # builds ./repo from ../../../src plus the edited modules here
    ./run-tests.sh   # 89 λ cases, plus micro's 125 cases run twice (with and without the λ layer); all pass

On the tree of the commit that adds this note, `build.sh` still builds and the 89 λ cases still pass, but
`run-tests.sh` also runs micro's suites of `../../../src/test` against the frozen micro, and those suites have
moved on: four of the six fail there.

Things to know before using the prototype:

- **Page numbers.** In the prototype's comments and in `DESIGN.md`, `p.N` is the *PDF* page of the book.
  The document's `[MN p. n]` references give the *printed* page, which is the PDF page minus 16.
- **Benchmarks.** `bench/run-bench.sh` expects the benchmark binaries compiled with `csc -O2 -d1`, plus a
  build of the unedited micro for the "before" column. The recorded figures are in `bench/*.out`.
- **Missing files.** The development probes that `DESIGN.md` mentions (`probe/`) are not included.
