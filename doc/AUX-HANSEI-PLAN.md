# Implementation plan for `(aux hansei)`, a CHICKEN 6 port of HANSEI

> **Status.** This plan is for completing `src/aux.hansei.scm`, the user's initial port of Oleg Kiselyov and Chung-chieh Shan's HANSEI, and its suite `src/test/hansei.scm`. It follows the form of `AUX-HAKARU10-PLAN.md`, and the citation and deviation conventions of `(aux hakaru10)`.
>
> The plan stands on its own. The implementer needs only this file, the upstream folder `upstream/hansei/`, and the build recipe in §11. No repository file other than this one was written while preparing it.
>
> **Revision 2** addresses three reviews (fidelity, idiom, oracles); every defect was checked against the sources and the build, and the outcome of each is in §15.
>
> Claims marked ✓ were re-run while the plan was written; the scratch scripts are listed in §14. Claims marked *[proto]* were reproduced by a reader's scratch prototype on top of the current `(aux hansei)`. Claims marked *[OCaml]* were reproduced by running upstream HANSEI under OCaml 4.14.2 with delimcc.

---

## 0. Sources, citation keys and conventions

**Citation keys.** All paths are relative to `aux.scm/upstream/hansei/`. Remote: `https://okmij.org/ftp/kakuritu/<file>`. The core files are tracked in git (commit 6b95bca); the other files are untracked.

| Key | File | What it is |
|---|---|---|
| T | ptypes.mli | the types `prob`, `dist`, `cdist`, `vc`, `pV`, `selector` |
| PI | probM.mli | the interface of the library (20 `val`s) |
| P | probM.ml | the core: memory, `reify0`, `dist`, `fail`, `reflect`, the distributions, the selectors, the samplers, `letlazy*`, `memo` |
| II | inference.mli | the inference interface |
| I | inference.ml | `explore`, `shallow_explore`, `first_success`, `bounded_explore`, the samplers, `statistics`, `normalize`, `timeit` |
| PMI / PM | pMap.mli / pMap.ml | persistent AVL map (ExtLib, LGPL-2.1, augmented with `insert_with`) |
| X | exactInfM.ml | exact-inference regression tests |
| SM | samplingM.ml | sampling regression tests |
| SL | slazy.ml | laziness, sharing, `memo` tests and the lazy-list kit |
| LP / LPI | lazy_permutations.ml / .mli | correlated memoization, lazy permutations |
| SO | sorted.ml | the sorted-list problem |
| N | nested.ml | nested inference |
| IN | incite.ml | bounded-rational theory of mind |
| PE | paper_examples.ml | the examples of the papers |
| M1 / M2 | music1a.ml / music2.ml | motivic development |
| NO | noisy-or/noisy_or.ml | noisy-or networks |
| BT | bloodtype/bloodtype.ml | blood types |
| HM | HMM/hmm.ml | hidden Markov model |
| BN / BA / BL / CB | blip/blip_naive.ml, blip_ailog.ml, blip.ml, colored_balls.ml | multitarget tracking, colored balls |
| LV | logic_vars.ml | the append relation, committed choice |
| DS | daily_schedule.ml | scheduling |
| Z | zebra.ml | the zebra puzzle |
| PZ | puzzle.ml | the U2 bridge puzzle |
| TI | type_inference.ml | reversible type inference |
| PS | parsec.ml | reversible parser combinators |
| PD | prolog_drawbacks.pro | the Prolog counterparts (doc only) |
| H | Hansei.html | the page (version April 2009 for the library, December 2010 for lazy permutations; last updated March 1, 2016) |
| K | kakuritu.html | the probabilistic-programming overview page |
| LG | logic-programming.html | "HANSEI as a Declarative Logic Programming Language" |
| DSL | dsl-paper.txt / .pdf | Kiselyov, Shan, "Embedded Probabilistic Programming", IFIP DSL 2009, LNCS 5658, pp. 360-384 |
| EPP | embedpp.txt / .pdf | Kiselyov, Shan, "Monolingual Probabilistic Programming Using Generalized Coroutines", UAI 2009 |
| NIPS | nips2008.txt / .pdf | Kiselyov, Shan, "Embedded Probabilistic Programming", NIPS 2008 workshop abstract |
| ML | ML2010.txt / .pdf | Kiselyov, Shan, "Probabilistic programming using first-class stores and first-class continuations", ML Workshop 2010 |
| RT | rethinking.txt / .pdf | Kiselyov, Kameyama, "Re-thinking Prolog: don't guess in a hurry" (JSSST 2014, in Japanese) |
| NII | NII2012.txt / .pdf | Kiselyov, "Non-deterministic choice in a conventional programming language", NII slides, December 18, 2012 |
| OR | OCaml 4.14.2 `stdlib/random.ml` (not in `upstream/`; 330 lines, sha256 `46683060…8e67`, from the oracle switch `opamroot/hansei/lib/ocaml/`) | `Random`: `full_init`, `bits`, `int`, `rawfloat` (two-draw in 4.14), the default state (added in WP2, §15) |
| OD | OCaml 4.14.2 `stdlib/digest.ml` | `Digest.string` (MD5) (added in WP2, §15) |

**Linked material (H, LG, K, recursively).** Every link of the three pages was classified. Pages fetched for this revision are in the scratchpad `linked/` (not in `upstream/`; ask before adding them, Q3).

| Link (from) | Status | Reason |
|---|---|---|
| `../continuations/index.html#reify-search` (H l.59) | consulted | Reifying a non-deterministic computation into a lazy `SearchTree` and traversing it with `dfs`, `bfs`, `iter_deepening`. It is the `cf.` for the user's `probcc-dfs` (`(new)` stays, plus `; cf. continuations/index.html#reify-search`). `bfs`/iterative deepening are not added: `probcc-first-success` (I:55-64) is HANSEI's breadth-first search. |
| `../continuations/shift-v-fork.html` (H l.46) | consulted | The lawn model in C and OCaml with `fork(2)`; the `Fork` `SimpleNonDet` of PZ:66-91 comes from it. Not ported (no `fork` in the library; §2 "Not ported"). Its lawn output `[(0.322, false); (0.2838, true)]` is H5/H12's oracle. |
| `../Computation/monads.html#lazy-sharing-nondet` (H l.39, 394; LG l.140) | consulted | Haskell lazy sharing non-determinism (Fischer, Kiselyov, Shan): background for `letlazy`; no definition to port. Cited in the "Laziness" doc section. |
| `StrangeLoop.pdf`, `http://menzies.us/pdf/07strange.pdf` (LG l.202, 208) | out of scope | Talk slides and a related paper; no code beyond LG's own. |
| `rethinking-talk.pdf` (LG l.60) | out of scope | Slides of RT; RT itself is used. |
| `../Prolog/Prolog.html`, `../lift-reduce/lift-reduce.html`, `../Computation/index.html`, `Hakaru10/…`, `shonan-113-report.pdf`, `index.html` | out of scope | Navigation, other projects (Hakaru10 is ported separately). |
| `Makefile` (H l.113) | out of scope | OCaml build only. |
| external papers (BLOG chapter, AILog, Jaeger's models, tr-12-07, ICLP07, SEP, Wikipedia Zebra, tls2009 abstract) | out of scope | Sources of the examples' models; the `.ml` files carry what is ported. |

**Paper pages.** The `.txt` extracts carry `=== page N ===` markers. Cite as `DSL p.15 l.636-658`. DSL sections:

| Section | txt line | Page |
|---|---|---|
| Introduction | l.21 | p.1 |
| Tagless final probabilistic language | l.267 | p.7 |
| Search trees, `explore` | l.367-425 | p.9-10 |
| Direct style: `dist`, `reify0` (shift/reset) | l.594-682 | p.14-16 |
| `reflect`, variable elimination | l.684-775 | p.16-18 |
| Bucket elimination | l.776-783 | p.18 |
| Importance sampling with look-ahead | l.785-856 | p.18-19 |
| Lazy evaluation | l.857-922 | p.19-21 |
| Conclusions | l.961 | p.22 |

For EPP: "Reifying a model into a search tree" l.381, p.4; "Importance sampling with look-ahead" l.547, p.6. Hansei.html anchors: `#implementation` (l.89), `#examples-tests` (l.116), `#colored-balls` (l.132), `#blood-type` (l.142), `#hmm` (l.150), `#noisy-or` (l.158), `#music` (l.167), `#blip` (l.180), `#sorted` (l.203), `#bounded-rationality` (l.233), `#all-different` (l.243).

**Source-citation rule (mandatory, as in `AUX-HAKARU10-PLAN.md` §0).**
- Every top-level definition carries `; cf. P:118-128` (or `I:…`, `DSL p.15 l.637`), naming what it transcribes.
- A definition with no upstream counterpart carries `; (new) …`. The user's own definitions are `(new)` too: `op/*`, `probcc-value`, `probcc-bernoulli`, `probcc-when`, `probcc-leaves`, `probcc-dfs`.
- Every departure from upstream carries `; deviation N (P:156-165): …`, with N from the register in §9.
- Every module begins with a `;;` header block: what it is, the files it mirrors with their URLs, the code version (April 2009; lazy permutations December 2010), the papers, and the contents.
- Section banners inside a module, as in `aux.hakaru10.exact.scm:186-188`:
  ```
  ;; ---------------------------------------------------------------------------------------
  ;; World-local memory (plan §5.2)
  ;; ---------------------------------------------------------------------------------------
  ```

**Coding style (the user's, from `src/aux.hansei.scm`; see the memory note "Port fidelity and idiom").**
- `(module (aux hansei …) * (import …) …)`, two-space indentation, `scheme-indent -T 2`.
- `λ`, `τ`, `let1`, `letrec` with λ helpers or named `let`, `let*` pipelines, `cond` with `else`, `match/first` / `λ1-match/first` with quasi-patterns `((V ,v) ,p)`, `define-syntax-rule`. A curried `define` where the user already uses one (`((probcc-reify depth) model)`).
- Single capital letters for local helpers (`A`, `F`, `N`) and `loop`.
- Public names are prefixed `probcc-`. Variants take a slash suffix (`probcc-reify/0`, `probcc-explore/shallow`). A macro taking a body sits next to a procedure taking a thunk, which gets the `/a` suffix (`probcc-reify/exact` and `probcc-reify/exact/a`).
- Delimited control **only** through `resetcc` / `letcc/shift` of `(aux continuation delimited)`, which is where upstream uses Delimcc. Never import `(chicken continuation)`, and never mix CHICKEN's native `shift`/`reset` with `resetcc` (memory note "Native shift/reset").
- The upstream folder is untracked apart from the core files. Do not commit the examples, papers or pages without asking the user.

---

## 1. Decisions

| # | Question | Decision | Why |
|---|---|---|---|
| D1 | Architecture of the core | Keep the user's direct `reify0` (DSL p.15 l.637: `reset (fun () -> pv_unit (m ()))`) where the `resetcc` body answers a `pV`, with no `pReq` and no `interp`. Add probM's memory protocol verbatim: the save, reset and restore of `thread_local` in `probcc-reify/0` (P:104-107); `curr_mem` captured by `dist` and `reflect`, and the save, set and restore around `k` (P:118-148). | The user chose the paper's form, and it is equivalent (§5.1). The memory protocol is what enables `letlazy`, `letlazy_nesting` and `memo` (P:59-75). |
| D2 | World-local memory | A module-level variable `probcc-thread-local` holding a **persistent** map from fixnum locations to values (`(aux hansei pmap)` with a fixnum order). Mutated only with `set!`, never through a parameter, a fluid or a hash table. | P:59-63: "The first-class storage is immutable … Only the pointer to it is mutable". A `parameterize`d or `set!`-shared cache leaks across worlds, because aux's shift re-enters dynamic-wind frames (`aux.continuation.delimited.scm:262-275`); probes `core/e2.scm` and `idiom/leak.scm` show the leak. |
| D3 | `probcc-explore` accumulator (bug B1) | Thread a persistent `(aux hansei pmap)` keyed by `pmap-compare` through the loop, exactly as I:16-28 threads `(ans, susp)`. | The srfi-69 table is shared by every world that resumes a captured explore loop (§4 C1, reproduced ✓). |
| D4 | Persistent map | New module `(aux hansei pmap)` (`src/aux.hansei.pmap.scm`): the PMI interface over an AVL tree with a comparison procedure, with `pmap-compare` as the default order. | Needed in three roles (value→mass collation, the priority queue of `bounded_explore`, memory and `memo` tables), as upstream uses PMap. aux has no persistent map: `aux.fds.*` holds a queue, sbral and union-find. |
| D5 | Output order of `probcc-explore` | Keep the user's contract, descending probability, but make ties deterministic: a **stable** sort (`(chicken sort) sort` on lists is a stable merge sort ✓) of upstream's own order, i.e. values in descending `pmap-compare` key order (`PM.foldi` consing, I:28) followed by the suspensions in reverse discovery order. | `(aux hakaru10 exact)` and the suites rely on the sorted output. The existing expected orders of tied entries (`test/hansei.scm:61-62, 76-77`) are exactly what this rule gives (§4 C5). |
| D6 | RNG | New module `(aux hansei random)` (`src/aux.hansei.random.scm`): a pure-Scheme MD5 and an **OCaml-`Random`-compatible** generator (lagged Fibonacci with the xor `bits` and the **three-draw** `rawfloat`), exposed as an `rng-source` of `(aux hakaru10 random)`. Like OCaml's stdlib, the module holds **one global state** `ocaml-random-global`. `(probcc-random-selector seed)` re-initialises that global state at creation (`Random.init seed`, P:221-222) and every such selector draws from it, so a selector created inside a model re-seeds the stream that the outer selector also uses, exactly as upstream. `(probcc-random-selector src)` with an explicit `rng-source` is the variant (deviation 18). | The global state is what upstream does, and replays need it: N:30-31 gives the recorded `0.5/0.5` only with a shared, re-seeded stream (`{0.6: 5, biased: 5}`); a fresh source per selector gives `{0.6: 6, biased: 4}` ✓ (`critic-fid/py/n30.py`). The three-draw generator replays every samplingM.ml assertion and every `random_selector` assertion of N, SL, PS and M1 *[OCaml, 3-draw build `crit-tests/hb3`]*; OCaml 4.14's two-draw `rawfloat` does not. Reusing the rng-source protocol lets users plug in stdgen, mrg32k3a or CHICKEN's generator. |
| D7 | Selector protocol | A selector is `(λ (choices) (list x ptotal))` over `((x p) …)`: one value, thing first, probability second, as the user's entries are. | It must return **one** value, because `dist_selector` returns through a captured continuation of one argument. The order matches `((V v) p)` and `(v p)`. |
| D8 | Exactness | New code keeps exact weights exact. Products and sums use `(op/times)` and `(op/plus)`, read once per call. Ratios (`p /. ptotal`, `p /. ns`) use the internal `probcc-ratio` = `(λ (m n) (if (zero? n) (/ (exact->inexact m) (exact->inexact n)) (/ m n)))`: exact stays exact, and a zero total gives `+nan.0`/`±inf.0` as upstream instead of raising (CHICKEN's `(/ 0 0.)` is exact `0`, hence both operands are converted). Only `probcc-normalize` goes through `op/divide`, which stays inexact by default. | This is the user's existing behaviour: `probcc-uniform` and `probcc-explore` are exact, and `probcc-normalize` is inexact. Flonum models stay bit-compatible with upstream, which replay needs. |
| D9 | Laziness | `probcc-letlazy` (P:321-330), `probcc-letlazy/nesting` (P:342-370, without the debug `printf`s), `probcc-memo` (P:466-479). Never `delay`/`force`/`δ`: built-in laziness is wrong under non-determinism (DSL p.21 l.912-922; SL:57-67 `tests3`). | Faithful. |
| D10 | Module layout | Five source modules (§2): `(aux hansei)` (core = P + I + T + utilities), `(aux hansei pmap)`, `(aux hansei random)`, `(aux hansei lazy)` (the lazy-list kit and LP), `(aux hansei parsec)` (PS). Examples stay in the tests. | It mirrors upstream's separate library files (PM, LP), the page calling parsec a "parser combinator library" (LG l.804, 993), and the `(aux hakaru10 …)` precedent. Users keep `(import (aux hansei))`. |
| D11 | Re-entrant `map` | Export `probcc-map`, a left-to-right map built by plain recursion. Library code that forces a thunk or calls user code inside a map uses it. Tests never use `map`, `vector-map` or `for-each` around stochastic code. | CHICKEN's `map` builds its result destructively (compiler-syntax rewrite; srfi-1 `map` too): re-entering a continuation captured inside `f` corrupts the list ✓ (§5.6). |
| D12 | csi operand hazard | Tests bind every choice with `let*`/`let1` before using it in **any call, quasiquote template or constructor (`list`, `vector`, …) with five or more elements**, whatever the position of the choice. A comment in each test file explains why, with the reproductions of §5.6. | Under `csi`, re-entering a continuation captured while evaluating the operands of a call with ≥5 operands accumulates arguments; this happens even with plain `call/cc` ✓ (§5.6). `` `(a b c d ,(probcc-coin 1/2)) ``, `(vector 'a 'b 'c 'd (probcc-coin 1/2))` and `(list (probcc-coin 1/2) 'a 'b 'c 'd)` are all affected ✓ (`crit-tests/op.scm`). Compiled code is correct. |
| D13 | `uniform` and `uniform_range` edge cases | **Keep the user's semantics** and register them as deviations: `probcc-uniform 1` makes a one-branch choice (commit 0af8975 removed the `n = 1` shortcut on purpose); `n ≤ 0` fails instead of raising; `probcc-uniform/range low high` is half-open `[low, high)` (the suite at `test/hansei.scm:158-167` and `test/travel.scm:26,73` rely on it). Tests that port `uniform_range a b` write `(probcc-uniform/range a (add1 b))`. | These are deliberate user choices. Both are listed in §13 as questions for the user. |
| D14 | `geometric` | Keep the user's `(probcc-geometric p s f)` (it returns the trace `(s f … f)`, same tree shape) and add the upstream integer form as `probcc-geometric/count`. | Faithful variant without breaking the user's API or `test/geometric`. |
| D15 | `normalize` | Keep `probcc-normalize` (list only, inexact): it is faithful to PE:22-24, upstream's own list-only `normalize` with the same fold (`; cf. PE:22-24`). Add `probcc-normalize/total` → `(list total normalized)` as I:299-301 returns. | `(aux hakaru10 exact)` imports `probcc-normalize`. |
| D16 | Sampler printing | `rejection_sample: done %d worlds` and `sample_importance: done %d worlds` print only when the parameter `probcc-verbose` is true (default `#f`). | The suites capture stdout. |
| D17 | Private prompt | Not added. Register that a user `resetcc` (or `yield`, `delimcc-either`, …) inside a model captures HANSEI choices. | aux has a single untagged delimiter. Upstream uses one global prompt too (DSL p.13 fn. 4); nested reification only needs the nearest delimiter, which already works ✓. A tagged-prompt facility would be a change to `(aux continuation delimited)`, outside this plan. |
| D18 | Tuples | OCaml tuples become lists: `(x rest)` from `probcc-uniform/either/uncons`, `(prev tail)` from `probcc-split-at`, `(low high)` from `probcc-explore/bounded`, `(x ptotal)` from selectors, `(v mean std)` from statistics. A parser result is a pair `(v . stream)`. | Readable, works with `match/first`, and passes through one-argument continuations. |
| D19 | OCaml evaluation order | Tests that port tuples, constructor arguments or function arguments that make choices sequence them with `let*` in **OCaml's right-to-left order**, with a comment. **`let x = e1 and y = e2 in` is sequenced e1 then e2** (left to right), e.g. `talarm` draws `earthquake` before `burglary` (SM:33, 38-40; X:31, 49). **The scrutinee of `match (e1, e2) with` is sequenced e1 then e2** too: OCaml compiles a tuple scrutinee without building the tuple and binds its components from the left (`let (a, b) = (e1, e2) in` stays right to left), e.g. LV:172 `bl_compare` and SO:88 `gte` (added in WP9, §15). Exact (`None`) oracles do not depend on it; depth-bounded ones and replays do. | Tuples and arguments: reproduced only right to left *[proto]* (logic reader). `let … and`: SM:38-40 gives the recorded `0.14/0.86` only left to right; `burglary` first gives `0.1/0.9` ✓ (`critic-fid/py/talarm.py`). |
| D20 | Statistical vs replay tests | Replays (OCaml source, fixed seeds, global state per D6) are bit-exact (`(⊦ equal? …)` on flonums). Records of SM, N, SL, PS and M1 are replayed (§8.2 R6, R9; §8.3 L17; §8.5 G13; §8.6). Records that do not replay (SO, IN, HM, CB, BT, BN, BL) get tolerance tests against the exact value in the long target, except IN: its records (IN:416-422) are the maxima of utilities estimated by nested sampling, which have no exact value for a mean to approach. Where upstream's code gives other values today under the three-draw OCaml (SO:177-200, IN:416-422), those are replayed bit for bit instead (amended in the final review, §15). | Every exact oracle is reproducible. With the three-draw generator, upstream's own assertions pass in samplingM, nested, slazy, parsec and music1a (0 exceptions; 10, 5, 4, 1 with two draws) and fail in sorted (5), incite (3), hmm (2), colored_balls (3) *[OCaml, `crit-tests/*.3draw.txt`]*. BT, BN and BL sampling records were not replayed and stay non-reproducible. |

---

## 2. Files, modules, dependencies

| File (under `src/`) | Module | Mirrors | Imports (besides `scheme`, `(chicken base)`) |
|---|---|---|---|
| `aux.hansei.pmap.scm` | `(aux hansei pmap)` | PM, PMI | `(chicken fixnum)`, `(only (chicken condition) make-property-condition make-composite-condition abort)`, `(only (chicken memory representation) record->vector)`, `(only (chicken keyword) keyword? keyword->string)`, `(aux base)`. **Explicit export list** (the §7 names), not `*`: the internal `height`, `node`, `bal`, `merge` must not leak (`merge` clashes with `(chicken sort)`'s, which `test/hansei.scm` imports whole). |
| `aux.hansei.random.scm` | `(aux hansei random)` | OCaml stdlib `Random` (`init`, `bits`, `float`, `int`) and `Digest.string` (MD5, RFC 1321) | `(chicken fixnum)`, `(chicken flonum)`, `(chicken bitwise)`, `(only (aux hakaru10 random) make-rng-source rng-source? rng-double! rng-integer!)`, `(aux base)` |
| `aux.hansei.scm` | `(aux hansei)` | T, P, I; DSL p.9-21; NIPS; LV:68-70, 386-391 (promoted); N:292-304, BL:456-460 (promoted) | `(only (scheme base) make-parameter)`, `(only (chicken sort) sort)`, `(only (chicken time) cpu-time)`, `(only (chicken format) printf)`, `(only (chicken flonum) fpsqrt)`, `(only (chicken fixnum) fx= fx< fx+)`, `(only (chicken memory representation) record-instance? record->vector)` (`probcc-structural=?`, WP4 review), `(only (chicken condition) make-property-condition make-composite-condition abort)`, `(only srfi-1 fold)`, `(aux base)`, `(aux continuation delimited)`, `(aux hansei pmap)`, `(aux hansei random)`, `(only (aux hakaru10 random) rng-source? rng-double!)` |
| `aux.hansei.lazy.scm` | `(aux hansei lazy)` | SL:325-355, M1:51-101, M2:113-119, LP, LPI | `(only (scheme base) exact-integer?)`, `(only (chicken condition) make-property-condition make-composite-condition abort)` (deviation 33, added in WP6), `(only srfi-1 append-reverse)`, `(aux base)`, `(aux hansei)` |
| `aux.hansei.parsec.scm` | `(aux hansei parsec)` | PS:22-290 (library part); LG `#parsec`, `#many` | `(aux base)`, `(aux hansei)` |

- The **removed imports** of `aux.hansei.scm` are `(chicken continuation)`, `(chicken pretty-print)`, `(chicken fixnum)`, `(aux continuation)` and `srfi-69`. `(scheme base)` is reduced to `(only (scheme base) make-parameter)`, which CHICKEN 6's `(chicken base)` does not export. The reduced list compiles *[proto]*.
- **Dependency graph** (no cycles): `hakaru10.random → base`; `hansei.pmap → base`; `hansei.random → hakaru10.random, base`; `hansei → pmap, random, continuation.delimited, base` (`aux.continuation` only transitively, through `continuation.delimited`); `hansei.lazy → hansei`; `hansei.parsec → hansei`; `hakaru10.exact → hansei` (unchanged).
- `pmap-compare` deliberately does not import `generic<?` from `(aux hakaru10 util)`. That order treats unknown types as equivalent, which would merge distinct keys (§6.1).

**Not ported**, each mentioned in a comment citing its lines:
- `from_option` / `read_answer` (P:28-38): the one-shot channel exists only to appease OCaml's type checker.
- The `pReq`/`interp` request loop (P:24, 97-108): fused into the answer type (deviation 2).
- The "simple-minded reify0" comment (P:81-90), `letlazy_old` and the `Needed` request (P:373-446).
- The commented-out experiments of I:200-274 (`observe_la`, `sample_explore_reify*`, the look-ahead counterexample).
- PM `enum` / `of_enum` (PMI:112-119, PM:201-233): commented out upstream ("We don't need enum"); `pmap->alist` / `alist->pmap` are (new) conveniences.
- The Unix `Fork` implementation of `SimpleNonDet` (PZ:66-91; `../continuations/shift-v-fork.html`).
- M2's sampling runs (M2:207-246, `sample_importance` with 2000 to 45000 samples): too slow for any suite; M2's lazy-list kit is M1's plus `lobserve` (§3.6).
- `string_of_list` (PS:41-47): `list->string` does it.

---

## 3. Inventory of upstream definitions

Status codes: **F** exists and is faithful (add the citation); **B** exists with a bug (fix, §4); **K** exists with a kept deviation (tag it, §9); **M** missing (add); **R** replaced or not ported (comment only). "Target" is the module or file; `core` is `(aux hansei)`.

### 3.1 ptypes.mli (T)

| Upstream | Cite | Scheme | Target | Status | Notes |
|---|---|---|---|---|---|
| `type prob = float` | T:4 | any number | core | K | Exact or inexact (deviation 1, D8) |
| `'a dist = (prob*'a) list` | T:6 | `((v p) …)`, the argument of `probcc-distribution` | core | K | Order swapped: value first (deviation 1) |
| `'a cdist` | T:7-8 | same shape | core | F | Need not sum to 1 |
| `'a vc = V of 'a \| C of (unit -> 'a pV)` | T:13-14 | `(V v)`, `(C thunk)` | core | F | Built by `probcc-value` / `probcc-τ` |
| `'a pV = (prob * 'a vc) list` | T:15 | `(((V v) p) ((C t) p) …)` | core | K | Probability second (deviation 1). Must not change: `aux.hakaru10.exact.scm:61, 190-262` and `test/hakaru10-exact.scm` depend on it. |
| `'a selector = 'a cdist -> prob * 'a` | T:20 | `(λ (choices) (list x ptotal))` | core | M | D7 |

### 3.2 probM.mli / probM.ml (P): the 20 exported values

| # | Upstream | Cite | Scheme | Target | Status | Implementation notes |
|---|---|---|---|---|---|---|
| 1 | `dist` | PI:15; P:118-128 | `probcc-distribution pairs` | core | **B** | Capture `curr` = `probcc-thread-local` **before** `letcc/shift`. Each branch is `(probcc-τ p (probcc-mem-resume curr k v))`. The `map` inside the shift body is safe, since it only builds thunks. Switch `λ1-match/non-overlapping` to `λ1-match/first` (§4 C11). |
| 2 | `fail` | PI:16; P:130 | `probcc-impossible` | core | F | `(probcc-distribution '())` = shift answering `'()` = `abort pp (Choice [])` |
| 3 | `reify0` | PI:20; P:97-108 | `probcc-reify/0 model` | core | **B** | §5.2: save the memory, `resetcc` with `(set! probcc-thread-local probcc-mem-empty)` as its first form, restore after the first segment returns |
| 4 | `flip` | PI:23; P:153 | `probcc-coin p` (via `probcc-bernoulli t f p`) | core | F | Order `(#t p) (#f 1-p)` as P:153 |
| 5 | `uniform` | PI:25; P:156-165 | `probcc-uniform n` | core | K | Order and masses as P:160-163 (value 0 gets `1 - pacc`). Deviation 7: `n = 1` makes a choice node, `n ≤ 0` fails, exact `1/n`. |
| 6 | `uniformly` | PI:27; P:168-170 | `probcc-uniform/either lst` | core | F | On a list (deviation 1). An empty list fails, through deviation 7. |
| 7 | `uniform_range` | PI:32; P:173-174 | `probcc-uniform/range low high` | core | K | Half-open, deviation 8 |
| 8 | `geometric` | PI:37; P:181-185 | `probcc-geometric p s f` (trace) | core | K | Deviation 9; see row 8b |
| 8b | `geometric` (int) | P:181-185 | `probcc-geometric/count p` | core | M | `(letrec ((loop (λ (n) (list (probcc-τ p (list (probcc-value 1 n))) (probcc-τ (subtract 1 p) (loop (add1 n))))))) (probcc-reflect (loop 0)))`, with `1` exact (upstream `1.0`) |
| 9 | `geometric_bounded` | PI:41; P:197-206 | `probcc-geometric/bounded n p` | core | M | `expi x n` is the right-nested product `x * (x * …)` with `expi x 0 = 1.` and `expi x 1 = x` (P:198-200). `norm = 1 - expi (1-p) (n+1)`. The pairs are `((0 p/norm) (1 (1-p)·p0) …)` with `pp_{i+1} = (1-p) * pp_i` in that operand order. Then `probcc-distribution`. Keep the operation order for bit-exact X:72-77. |
| 10 | `reflect` | PI:48; P:136-148 | `probcc-reflect choices` | core | **B** | Capture `curr` before the shift. `V` entries become `(probcc-τ p (probcc-mem-resume curr k v))`; `C` entries become `(probcc-τ p (make-choices (t)))` as now. Use `λ1-match/first`. |
| 11 | `observe` | PI:52; P:213-214 | `probcc-observe test thunk` | core | M | `(let1 (v (thunk)) (if (test v) v (probcc-impossible)))`. Keep `probcc-when` (F, `(new)` syntax). |
| 12 | `variable_elim` | PI:55; P:267-268 | `probcc-variable-elimination f` | core | F | Variadic (deviation 12) |
| 13 | `random_selector` | PI:60; P:221-233 | `probcc-random-selector seed-or-source` | core | M | §5.4: a seed re-initialises the global OCaml state (P:222) |
| 14 | `dist_selector` | PI:63; P:238-241 | `probcc-dist-selector choices` | core | M | `ptotal` is a left fold from `0` of `(plus pa p)`. Answer `(list (probcc-distribution (probcc-map (λ1-match/first ((,x ,p) (list x (probcc-ratio p ptotal)))) choices)) ptotal)`; all-zero choices give NaN masses as upstream, not a raise (D8). Needs C1 fixed. |
| 15 | `exact_reify` | PI:70; P:247-248 | `probcc-reify/exact/a thunk`; macro `probcc-reify/exact body …` | core | F | `(probcc-reify +inf.0)` |
| 16 | `sample_rejection` | PI:73; P:253-254 | `probcc-sample-rejection/a selector n thunk`; macro `probcc-sample-rejection selector n body …` | core | M | `(probcc-sample-dist/rejection selector n (probcc-reify/0 thunk))` |
| 17 | `sample_importance` | PI:80; P:256-262 | `probcc-sample-importance/a selector n thunk`; macro `probcc-sample-importance selector n body …` | core | M | `(probcc-sample-dist selector (probcc-sample-runner/times n) (probcc-explore/shallow 3 (probcc-reify/0 thunk)))` |
| 18 | `letlazy` | PI:88; P:321-330 | `probcc-letlazy thunk` | core | M | §5.3 |
| 19 | `letlazy_nesting` | PI:92; P:342-370 | `probcc-letlazy/nesting thunk` | core | M | §5.3. Drop the six `printf`s (P:347, 349, 351, 353, 359, 363), deviation 13. |
| 20 | `memo` | PI:97; P:449-479 | `probcc-memo f` | core | M | §5.3. Re-read the table after `(f x)` (P:474-476). |

**Internal definitions of P.**

| Upstream | Cite | Scheme | Status | Notes |
|---|---|---|---|---|
| `pReq`, `interp` | P:24, 99-102 | none | R | Deviation 2 |
| `pp` | P:26 | the nearest `resetcc` | K | Deviation 3 |
| `from_option`, `read_answer` | P:28-38 | none | R | |
| `Memory.newm` | P:46 | `probcc-mem-empty` = `(pmap-create fx-compare)` | M | `fx-compare` is a 3-way fixnum compare, defined in pmap |
| `Memory.genid`, `new_loc` | P:47-50 | `probcc-mem-new-loc` | M | Global counter, fresh fixnum. No channel (deviation 15). |
| `Memory.mref` | P:52-54 | `probcc-mem-ref loc mem [default-thunk]` | M | `pmap-find` (plain miss, then the default thunk in tail position, §3.4). A missing key raises `(exn hansei memory)` unless a default thunk is given (`Not_found`). |
| `Memory.mset` | P:55-56 | `probcc-mem-set loc v mem` → new memory | M | `pmap-add` |
| `thread_local` | P:59-64 | `probcc-thread-local` (module variable) | M | D2. Single-threaded, like upstream (deviation 15). |
| `mem_prompt` | P:66-75 | `probcc-mem-prompt thunk` | M | `(let* ((mem probcc-thread-local) (v (thunk))) (set! probcc-thread-local mem) v)`. No `dynamic-wind` (§5.2). |
| the branch wrapper in `dist`/`reflect` | P:123-127, 143-147 | `probcc-mem-resume curr k v` | M | `(let1 (mem probcc-thread-local) (set! probcc-thread-local curr) (let1 (r (k v)) (set! probcc-thread-local mem) r))`, named after the prototype's `resume/mem` |
| `'a memo_val = MV \| MT` | P:342 | `(MV v)` / `(MT thunk)` lists | M | Matched with `match/first` |
| simple reify0, `letlazy_old` | P:81-90, 373-446 | none | R | |
| design notes on letlazy | P:270-319 | doc text | — | Quote in the doc of the laziness tests |

### 3.3 inference.mli / inference.ml (I)

| Upstream | Cite | Scheme | Target | Status | Implementation notes |
|---|---|---|---|---|---|
| `explore` | II:10-13; I:16-28 | `probcc-explore maxdepth choices` | core | **B** | §5.5; corrections C1, C4-C6 |
| `nearly_one` | I:30 | `probcc-nearly-one` = `(- 1. 1e-7)` | core | M | |
| `shallow_explore` | II:17-20; I:35-53 | `probcc-explore/shallow maxdepth choices` | core | M | §6.2 |
| `first_success` | II:22-26; I:55-64 | `probcc-first-success choices` | core | M | `(match/first choices (() '()) ((((V ,_) ,_) . ,_) choices) ((((C ,t) ,pt) . ,rest) (probcc-first-success (append rest (probcc-map (λ1-match/first ((,slot ,p) (list slot (times pt p)))) (t))))))`. The head-`V` case returns the whole list. |
| `bounded_explore` | II:28-33; I:71-109 | `probcc-explore/bounded maxsize choices` → `(list low high)` | core | M | §6.3 |
| `rejection_sample_dist` | II:42-46; I:121-135 | `probcc-sample-dist/rejection selector n choices` | core | M | §6.4 |
| `sample_runner` | II:38-39; I:142-146 | `probcc-sample-runner/times n` → `(λ (z step) (list z* n))` | core | M | `(let loop ((z z) (i n)) (if (zero? i) (list z n) (loop (step z) (sub1 i))))`, as P:259-261 |
| `sample_dist` | II:48-52; I:137-198 | `probcc-sample-dist selector runner choices` | core | M | §6.5 |
| `statistics` | II:57-62; I:276-294 | `probcc-statistics seed1 seed2 sampler` → `((v mean std) …)` | core | M | §6.6; std through `fpsqrt` so a negative rounding gives `+nan.0` as upstream, not a complex |
| `normalize` | II:67-70; I:296-301 | `probcc-normalize/total` → `(list total normalized)` | core | M | §4 C7; D15. `probcc-normalize` (list only) is F, `; cf. PE:22-24`. |
| `timeit` | II:65; I:303-308 | `probcc-timeit thunk` | core | M | `cpu-time` (user + system ms) before and after; print `\nTime spent: ~a sec\n` (seconds as a flonum) to the current output port; return the value. Upstream `Sys.time` is processor time too. Upstream formats with `%g` (I:307); `~a` prints the full flonum (`%g` gives `1.23457e-05` and `0.123457`, `~a` gives `1.234567e-05` and `0.123456789`): deviation 28. |

### 3.4 pMap.mli / pMap.ml (PM) → `(aux hansei pmap)`

Representation: a record `pmap` with fields `cmp` and `tree`. A tree is `'()` (Empty) or a vector `#(l k v r h)` (Node). `cmp` is `(λ (a b) → negative, 0 or positive fixnum)`.

| Upstream | Cite | Scheme | Status | Notes |
|---|---|---|---|---|
| `height`, `make`, `bal` | PM:31-60 | internal `height`, `node`, `bal` | M | Rebalance when heights differ by more than 2 (PM:40, 51), as upstream: **not** by 1 |
| `merge`, `delete_find_min'`, `delete_find_max'` | PM:70-98 | internal | M | |
| `empty` | PMI:29; PM:101 | `pmap-empty` (cmp = `pmap-compare`) | M | |
| `create` | PMI:35; PM:100 | `pmap-create cmp` | M | |
| `is_empty` | PMI:32; PM:103-104 | `pmap-empty?` | M | |
| `add` | PMI:38; PM:106-118 | `pmap-add k v m` | M | On an equal key, the node stores the **new** key (PM:110) |
| `insert_with` | PMI:43-50; PM:120-132 | `pmap-insert-with f k v m` | M | Stores `(f new old)` (PM:124) |
| `find` | PMI:52; PM:134-142 | `pmap-find k m [default-thunk]` | M | A missing key raises `(exn pmap not-found)` unless a default thunk is given. The search returns a plain miss; the default thunk is then called **in tail position, outside any handler** (no `condition-case`, `handle-exceptions`, `with-exception-handler`): `probcc-letlazy` and `probcc-memo` pass thunks that shift (`(e)` calls `probcc-distribution`), and a shift under a handler captures a call/cc continuation (`aux.continuation.delimited.scm:277-282`). |
| `find_min` | PMI:56; PM:62-68 | `pmap-find-min m` → `(k . v)` | M | Raises on empty |
| `delete_find_min` / `delete_find_max` | PMI:61-62; PM:70-90 | `pmap-delete-find-min m` / `…-max` → `(list (k . v) m')` | M | Raises on empty (`invalid_arg`) |
| `remove` | PMI:72; PM:144-151 | `pmap-remove k m` | M | |
| `mem` / `exists` | PMI:76-80; PM:153-161 | `pmap-member? k m` | M | One name for both |
| `iter` | PMI:83; PM:163-167 | `pmap-for-each f m` (in key order) | M | |
| `map` / `mapi` | PMI:90-97; PM:169-185 | `pmap-map f m` / `pmap-map/key f m` | M | Left subtree first, then right (PM:173-174) |
| `fold` / `foldi` | PMI:101-108; PM:187-199 | `pmap-fold f m acc` / `pmap-foldi f m acc` | M | **Ascending** key order: `f` receives `(k v acc)`, the left subtree first |
| `enum` / `of_enum` | PMI:112-119; PM:201-233 | none; `pmap->alist` (ascending) / `alist->pmap alist [cmp]` are (new) | R | Commented out upstream ("We don't need enum"), not exported |
| OCaml `compare` | (PM:101) | `pmap-compare a b` | M | §6.1 |
| (int compare of `Memory.M`) | P:44 | `fx-compare` | M | `(fx- a b)` is safe for location ids |
| (float compare of the `bounded_explore` queue) | I:78 | `number-compare` | M | (new) `(cond ((< a b) -1) ((> a b) 1) (else 0))` on reals |
| (type test) | — | `pmap?` | M | (new) record predicate |

### 3.5 lazy_permutations.mli / .ml (LP) → `(aux hansei lazy)`

| Upstream | Cite | Scheme | Status | Notes |
|---|---|---|---|---|
| `uniformly_list` | LPI:7; LP:24 | `probcc-uniform/either` | F | Already in core; add `; cf. LP:24` to its comment |
| `split_at` | LPI:29-37; LP:139-146 | `probcc-split-at n lst` → `(list prev-reversed tail)` | M | Out of range gives `(list (reverse lst) '())` (LPI:35) |
| `uniformly_uncons` | LPI:11; LP:158-166 | `probcc-uniform/either/uncons lst` → `(list x rest)` | M | `'()` → `(probcc-impossible)`; `(x)` → `(list x '())` with **no** choice; otherwise `i = (probcc-uniform n)`, `(split-at i xs)`, `(list h (append-reverse prev next))` (srfi-1 `append-reverse` = `List.rev_append`) |
| `lazy_permutation` | LPI:13-16; LP:175-188 | `probcc-lazy-permutation lst` → list of thunks | M | Built with `probcc-letlazy` as upstream (non-nesting). Variable 0 is `(probcc-letlazy (τ (probcc-uniform/either/uncons lst)))`, each next one is uncons of `(cadr (prev))`, and the last is `(match/first (prev) ((,_ (,x)) x))`. `'()` gives `'()`; `(x)` gives `(list (τ x))`. Build the list with plain recursion. |
| `insert_shift` | LP:242-249 | `probcc-insert-shift i lst` → `(list j lst')` | M | Not in LPI; exported for the tests |
| `lazy_permutation_ins` | LPI:18-27; LP:272-287 | `probcc-lazy-permutation/ins n` | M | `0` gives `'()`, `1` gives `(list (τ 0))`, `n > 1` as LP:283-287; any `n` not a non-negative exact integer raises (deviation 33; upstream: match failure or a type error) |

### 3.6 slazy.ml (SL), music1a.ml (M1) and music2.ml (M2) lazy-list kit → `(aux hansei lazy)`

Representation: an llist is a thunk answering `'()` (LNil) or a pair `(head-thunk . tail-llist)` (LCons). The head is a thunk (SL:325-326, M1:51-52).

| Upstream | Cite | Scheme | Status | Notes |
|---|---|---|---|---|
| `type 'a lcons / 'a llist` | SL:325-326 | representation above | M | Example-local upstream; promoted to the library because SL, M1 and M2 share it |
| `nil` | SL:329 | `probcc-lnil` = `(τ '())` | M | |
| `cons h t` | SL:330 | `probcc-lcons h t` = `(τ (cons h t))` | M | |
| `lappend` | SL:332-336 | `probcc-lappend y z` | M | `(probcc-letlazy (τ (match/first (y) (() (z)) ((,h . ,t) (cons h (probcc-lappend t z))))))` |
| `lhead` | SL:338-339 | `probcc-lhead x` → the head thunk | M | `'()` → `(probcc-impossible)` |
| `ltail` | SL:342-343 | `probcc-ltail x` | M | Same |
| `lobserve` | M2:113-119 | `probcc-lobserve ll lst` | M | Compares a lazy list with a list: `(void)` when equal (`equal?` on forced heads, left to right), else `(probcc-impossible)` |
| `lreflect` | SL:346 | `probcc-lreflect x` = `(x)` | M | |
| `list_length_at_most` | SL:350-355 | `probcc-llength/at-most max l` | M | Example helper (Ken's test), small |
| `to_lcons` | M1:71-73 | `probcc-list->llist lst` | M | Heads `(τ h)` |
| `llength'` / `llength` | M1:78-83 | `probcc-llength l` | M | `llength'` stays internal |
| `lmap'` / `lmap` | M1:98-101 = M2:106-109 | `probcc-lmap f l` | M | `(probcc-letlazy (τ (match/first (l) (() '()) ((,h . ,t) (cons (probcc-letlazy (τ (f (h)))) (probcc-lmap f t))))))` |

Other definitions of SL, SO and M1 (`build_list`, `to_list`, `is_sorted`, `bit_list`, `gte`, `num_list`, `splitAt`, `split`, the PCFG, the note transforms) are examples. They go in the tests (§8.4).

### 3.7 nested.ml / paper_examples.ml / blip.ml utilities → core

| Upstream | Cite | Scheme | Status | Notes |
|---|---|---|---|---|
| `prob_of` | N:292-293 = PE:49-50 | `probcc-prob-of v pv` | M | Probability of the first entry `((V ,v*) ,p)` with `v*` equal to `v` under OCaml's `=` (`probcc-structural=?`: `(pmap-compare v* v)` is 0 and `v*` holds no NaN, so `-0.` is `0.` as in the tables and NaN is never found; WP4 review), else `0`. Upstream `List.find` raises a match failure on a `C` entry; skip `C` entries (deviation 23). |
| `at_least` | N:304 = PE:61 | `probcc-at-least prob v pv` | M | `(>= (probcc-prob-of v pv) prob)` |
| `map_dist` | BL:456-460 | `probcc-map-dist f pv` | M | Maps the values of a flat pV and re-collates them: left fold of `(pmap-insert-with plus (f v) p pm)` over the `V` entries, then `pmap-foldi` consing (ascending keys → descending list). A `C` entry raises `(exn hansei malformed)` (upstream: match failure). Needed by §8.6 BL `q3b_s0..s2`. |
| `argmax` | IN:147-156 | test-local in `hansei-examples.scm` | — | Example-specific (utility, action) tables |

### 3.8 Logic programming (LV, TI, LG) → core

| Upstream | Cite | Scheme | Status | Notes |
|---|---|---|---|---|
| `reify_part` | LV:68-70 = TI:60-63 (`run`); LG l.429 | `probcc-reify/part bound model` → `((v p) …)` | M | The `V` entries of `(probcc-explore bound (probcc-reify/0 model))`, as `(v p)` pairs in explore order. Upstream reverses them with `fold_left`; the order is not part of any oracle (deviation 5). |
| `soft_cut` | LV:386-391 = PS:156-161; LG l.1180-1184 | `probcc-soft-cut p q r` | M | `(let1 (t (probcc-first-success (probcc-reify/0 p))) (if (null? t) (r) (q (probcc-reflect t))))` |
| `WorldLocalMem.wref_new` | TI:121-126 (module not shipped) | `probcc-wref-new v` | M | Reconstructed: new loc, `mset` into the current memory, answer the loc (deviation 17) |
| `wref_get` | TI:124 | `probcc-wref-get r` | M | `mref` in the current memory |
| `wref_set` | TI:125 | `probcc-wref-set! r v` | M | `mset` into the current memory. Upstream argument order is `wref_set v r`; the Scheme order is target first (deviation 17). |
| `mustbe`, `exists` | DS:53; Z:49-55 | test-local | — | |

### 3.9 parsec.ml (PS) → `(aux hansei parsec)`

A stream is a thunk answering `'()` (Eof) or `(char . stream)`. A parser is `(λ (stream) (cons v stream'))`. The prefix is `parsec-`, as `(aux hakaru10 random)` uses `rng-`/`stdgen-`. `<*>` and `<$>` are valid identifiers (`aux.category.monad.scm:81, 87`; `aux.hakaru10.scm:76`), but `(aux category monad)` already exports them with other meanings, so reusing them would clash for any user importing both; only `<|>` is awkward (`|` quotes symbols; it could be written `\x7c;`).

| Upstream | Cite | Scheme | Notes |
|---|---|---|---|
| `letlazy = letlazy_nesting` | PS:22 | the module uses `probcc-letlazy/nesting` | "for the sake of 'many'" |
| `stream_of_string` | PS:30-35 | `parsec-stream-of-string s` | Deterministic |
| `string_of_stream` | PS:52-57 | `parsec-string-of-stream st` | Forces the stream |
| `stream_over` | PS:61-66 | `parsec-stream-over alphabet` (a list) | `(letrec ((loop (τ (if (probcc-coin 1/2) (let* ((t (probcc-letlazy/nesting loop)) (c (probcc-uniform/either alphabet))) (cons c t)) '())))) (probcc-letlazy/nesting loop))`. OCaml evaluates the constructor right to left: `letlazy loop` first, then `uniformly` (D19). |
| `eof` | PS:77-81 | `parsec-eof` | `(λ (st) (if (null? (st)) (cons (void) st) (probcc-impossible)))`; the unit value is `(void)` |
| `pure`, `empty` | PS:84, 87 | `parsec-pure x`, `parsec-empty` | |
| `p_sat`, `p_char` | PS:93-100 | `parsec-sat pred`, `parsec-char c` | `char=?` |
| `<\|>` | PS:106-108 | `parsec-or p1 p2` | `((probcc-uniform/either (list p1 p2)) st)`; binary, so nest it to the **left** like OCaml's infix (deviation 22) |
| `alt` | PS:111-113 | `parsec-alt parsers` (a list) | |
| `<*>`, `<$>`, `*>`, `<*` | PS:116-139 | `parsec-ap`, `parsec-fmap`, `parsec-right`, `parsec-left` | Sequenced with `let*` as written |
| `p_fix` | PS:142-144 | `parsec-fix f` | `(letrec ((p (λ (st) ((f p) st)))) p)` |
| `many` (with munch) | PS:156-171 | `parsec-many p` | via `probcc-soft-cut` |
| `many` (no munch, commented) | PS:146-151 | test-local contrast | |
| `many1` | PS:175-177 | `parsec-many1 p` | |
| `run_fwd` | PS:180-184 | `parsec-run/forward p string` | `(probcc-reify/exact …)`; fails unless the rest is Eof |
| `run_bwd` | PS:257-265 | `parsec-run/backward bound p stream-thunk` → pV of `((v string) …)` | `(probcc-explore bound (probcc-reify/0 …))`, keeps `C` nodes |
| `stream_len`, `stream_at_most_len` | PS:279-290 | `parsec-stream-len st n`, `parsec-stream-at-most-len st n` | Return `(void)` or fail |

The palindrome parsers, `digit`/`num`/`expression`/`term`/`factor` and `exp_stream` (PS:200-523) are examples. They go in `hansei-logic.scm`.

### 3.10 DSL paper definitions

| Upstream | Cite | Scheme | Status | Notes |
|---|---|---|---|---|
| `pv_unit` | DSL p.10 l.414-416 | `probcc-unit` | F | |
| `pv_bind` | DSL p.10 l.415-419 | `probcc-bind m f` | M | `(probcc-map (λ1-match/first (((V ,x) ,p) (probcc-τ p (f x))) (((C ,t) ,p) (probcc-τ p (probcc-bind (t) f)))) m)`. Small, documents the monad (DSL l.420-425). |
| `reify maxdepth` | DSL p.15 l.656-658 | `((probcc-reify depth) model)` | F | |
| bucket elimination | DSL p.18 l.776-783; NIPS l.69-75; EPP p.4 l.331-336 | `λ-probcc-bucket` | K | Global `λ-memo` (deviation 11) |
| `flips` / `trues` | DSL p.20 l.866-909 | tests | — | §8.1 |

### 3.11 New definitions with no upstream counterpart

| Scheme | Target | Why |
|---|---|---|
| `probcc-map f lst` | core | D11 |
| `probcc-verbose` (parameter, `#f`) | core | D16 |
| `probcc-mem-resume` | core | Names the wrapper of P:123-127 |
| `probcc-ratio` (internal, not exported) | core | D8 |
| `md5-digest`, `md5-hex`, `make-ocaml-random-state`, `ocaml-random-state?`, `ocaml-random-state-copy`, `ocaml-random-init!`, `ocaml-random-bits!`, `ocaml-random-float!`, `ocaml-random-int!`, `ocaml-random-global`, `make-ocaml-random-source` | random | D6, §5.4 |
| `pmap-compare`, `fx-compare`, `number-compare`, `pmap?`, `pmap->alist`, `alist->pmap` | pmap | §6.1, §3.4 |

### 3.12 Counts

Counted by upstream definition (the ptypes row counts the six types). I's `normalize` is counted as M (`probcc-normalize/total`); the existing list-only `probcc-normalize` is re-cited as faithful to PE:22-24 and not counted again. The added variant `probcc-geometric/count` (row 8b) is counted separately.

| Group | F | B | K | M | R |
|---|---|---|---|---|---|
| P exported (20) | 5 | 3 | 3 | 9 | 0 |
| P internal (12 rows) | 0 | 0 | 1 | 8 | 3 |
| I (11) | 0 | 1 | 0 | 10 | 0 |
| PM public (19, of which `enum`/`of_enum` are commented out upstream) | 0 | 0 | 0 | 17 | 2 |
| LP (6) | 1 | 0 | 0 | 5 | 0 |
| SL + M1 + M2 kit (12) | 0 | 0 | 0 | 12 | 0 |
| N/PE/BL utilities (3) | 0 | 0 | 0 | 3 | 0 |
| Logic core (5) | 0 | 0 | 0 | 5 | 0 |
| PS library (21) | 0 | 0 | 0 | 21 | 0 |
| DSL (4, without the examples) | 2 | 0 | 1 | 1 | 0 |
| ptypes (6) | 2 | 0 | 3 | 1 | 0 |
| **Total (119)** | **10** | **4** | **8** | **92**, plus the variant `probcc-geometric/count` = **93** | **5** |

Plus 3.11's (new) definitions.

---

## 4. Corrections and refactorings of the existing code

Each item names the evidence and the callers it affects. All keep the names and shapes used by `aux.hakaru10.exact.scm:61` (`probcc-τ`, `probcc-unit`, `probcc-explore`, `probcc-normalize`) and `test/hakaru10-exact.scm:20-22` (also `probcc-coin`, `probcc-distribution`, `probcc-when`, `probcc-reify/exact/a`).

| # | What | Evidence | Fix | Callers |
|---|---|---|---|---|
| **C1** | `probcc-explore` accumulates in a mutable srfi-69 table (`aux.hansei.scm:42-45`). A thunk forced by the explorer can choose in the **enclosing** model (`letlazy_nesting` asking the parent, P:358-369; `dist_selector` inside a nested sampler, P:238-241; any C thunk calling `dist`). Every outer world then resumes the same loop and table. | ✓ `(probcc-reify/exact (probcc-explore +inf.0 (list (probcc-value 1/2 'a) (probcc-τ 1/2 (probcc-unit (probcc-coin 1/2))))))` gives `(((V (((V a) 1/2) ((V #t) 1/2))) 1/2) ((V (((V a) 1/2) ((V #f) 1/2) ((V #t) 1/2))) 1/2))`. The `#t` leaks into the `#f` world. Upstream threads a persistent PMap (I:16-28). | §5.5: thread `(ans . susp)` with `pmap-insert-with` | none (same output contract) |
| **C2** | No world-local memory: `probcc-reify/0` (`:97`), `probcc-distribution` (`:56-57`) and `probcc-reflect` (`:59-65`) neither save nor restore memory. | P:97-148; a prerequisite for rows 18-20 of §3.2 | §5.2 | none |
| **C3** | Unused imports `(chicken continuation)` (a hazard: native `shift`/`reset`), `(chicken pretty-print)`, `(chicken fixnum)`, `(aux continuation)`; `srfi-69` becomes unused after C1. | Compiling without them works *[proto]* | Import list of §2 | none |
| **C4** | `probcc-explore` re-wraps a suspended branch, `(probcc-τ p*pt (t))` (`:40`), allocating a closure; upstream keeps `c` (I:26). | I:26 | `` `((C ,t) ,(times pt p)) `` | `test/geometric` reads the thunks from the result: unchanged |
| **C5** | Ties come out in srfi-69 hash order (`:45-47`). The existing tests depend on it by luck. | ✓ the two 0.24 entries of `test/procc/coin-model` and the 0.2857 pair of `…/when` | D5. The new order puts `((x #t) (y #f))` before `((x #f) (y #t))` and `(#t #f)` before `(#f #t)`, exactly as the suite expects. | none |
| **C6** | `maxdepth` must be a number; upstream has `None`. | I:23 | Accept `#f` as well: `(or (not maxdepth) (< depth maxdepth))` | `+inf.0` keeps working |
| **C7** | `probcc-normalize` sums with `foldr` (`:52`), whose float result can differ from upstream's left fold (I:300) in the last ulp. It divides `(exact->inexact (/ m n))`, which **raises** "division by zero" on a zero total; upstream gives `nan` (e.g. `(probcc-reify/exact (probcc-when (probcc-coin 0) 'x))` → `(((V x) 0))`). | I:299-301; probe `(probcc-normalize '(((V a) 0)))` raises *[proto]* | Left fold `(fold (λ (e acc) (plus (cadr e) acc)) 0 choices)` (`p +. acc` operand order, I:300, PE:23). Default `op/divide` = `(λ (m n) (if (zero? n) (/ (exact->inexact m) (exact->inexact n)) (exact->inexact (/ m n))))`: one correct rounding for exact weights (unchanged from today: converting both operands first would round twice, e.g. `1/10 ÷ 3/10` → 0.33333333333333337 instead of 0.33333333333333331 ✓), the same value as upstream for flonums, and `+nan.0` for 0/0 (converting both because CHICKEN's `(/ 0 0.)` is exact `0`). Read `op/*` once. Use `probcc-map`. | `aux.hakaru10.exact.scm:261` (list result unchanged); `test/hakaru10-exact.scm` compares at 1e-12: run it |
| **C8** | The `op/*` parameters are read inconsistently: per element in `probcc-dfs` (`:122`), at model run time elsewhere, and `probcc-uniform/range` uses raw `+`/`-`. | grep | Read each parameter once per call into a local, in every procedure. Keep the parameters (the user's pluggable arithmetic, tagged `(new)`). Raw `+` in `uniform/range` is right: it adds integers. Document: a parameter read inside a model takes its value **where the segment runs**, i.e. where the explorer forces the `C` thunk, not where `probcc-reify/0` was called: `(parameterize ((op/subtract f)) (probcc-reify/0 m))` explored outside the `parameterize` applies `f` only in the first segment. Bind the parameters around the explorer too. | none |
| **C9** | `probcc-dfs` forces thunks inside `map` (`:122-124`). If the tree's thunks shift to an enclosing reset (dfs used inside a model), the destructive `map` is re-entered. | D11, §5.6 | `probcc-map` for both maps; `(apply append …)` stays | `test/dfs` unchanged |
| **C10** | `probcc-leaves` binds unused `v` and `p` (`:116-117`), and diverges on infinite trees. | read | `((V ,_) ,_)`. Document "(new) the number of `V` leaves, forcing every thunk; diverges on infinite trees; inner reifications are invisible (DSL p.17 l.756-757 counts 1024 for `flips_xor 10`)". | none |
| **C11** | `λ1-match/non-overlapping` on the hot path of `probcc-distribution` and `probcc-reflect` builds a package list per element (`aux.base.scm:46-77`) for clauses that cannot overlap. | `aux.base.scm:46-77, 85-131` | `λ1-match/first`, which still raises on a malformed entry ("uncaught value") | none |
| **C12** | `aux.egg:60` `(extension aux.hansei)` has no `component-dependencies`. | `aux.egg:60-66` | §11 | — |
| **C13** | The test names `test/procc/…` (`test/hansei.scm:59, 74, 87, 131`) are typos for `test/probcc/…`. Lines 213-235 of the test file are blank. | read | Rename; drop the trailing blank lines | none (names only) |
| **C14** | No module header, citations or deviation tags. | §0 | WP10 | — |
| **C15** | `probcc-uniform/either` computes `(length lst)` and `list-ref`; upstream `uniformly` is an array index (P:168-170). | P:168-170 | Keep (faithful on lists); add `; cf. P:168-170, LP:24` | — |

**Kept unchanged, tagged as deviations (§9):** `probcc-uniform` (n = 1, n ≤ 0, exact 1/n), `probcc-uniform/range` (half-open), `probcc-geometric` (trace), `probcc-normalize` (list only, inexact), `λ-probcc-bucket` (global memo), `probcc-variable-elimination` (variadic), `probcc-when` (syntax), and the `((V v) p)` order.

---

## 5. Designs

### 5.1 Why the direct reifier is equivalent to P:97-128

- probM pushes `pp`, runs the thunk, and answers `Done` or `Choice [(p, thunk)]`. `interp` maps `Choice` lazily to `[(p, C (fun () -> interp (th ())))]` and `Done` to `[(1.0, V answer)]` (P:99-102).
- The port's `resetcc` body answers `(probcc-unit (model))`, which is `Done` already interpreted. The shift bodies answer `((C thunk) p)` lists whose thunks call `(k v)`, and `(k v)` re-enters a fresh `resetcc` (aux's `k` re-pushes a delimiter, `aux.continuation.delimited.scm:290-293`, as Delimcc's `shift0` does). It therefore answers the next interpreted segment.
- aux's `letcc/shift` is `shift`, not `shift0`: the body runs with the reset frame still current. No shift body in HANSEI shifts again (they only build lists of thunks), so the two coincide (deviation 4).
- `fail` = `abort pp (Choice [])` = a shift whose body answers `'()`.

### 5.2 World-local memory

```scheme
;; cf. P:44-46 newm (an int-keyed Map); deviation 15
(define probcc-mem-empty (pmap-create fx-compare))
;; cf. P:59-64; the pointer is mutable, the map is persistent (D2)
(define probcc-thread-local probcc-mem-empty)

;; cf. P:66-75
(define (probcc-mem-prompt thunk)
  (let* ((mem probcc-thread-local) (v (thunk)))
    (set! probcc-thread-local mem)
    v))

;; cf. P:123-127 and P:143-147: resume k in the memory of the choice point.
(define (probcc-mem-resume curr k v)
  (let1 (mem probcc-thread-local)
    (set! probcc-thread-local curr)
    (let1 (r (k v)) (set! probcc-thread-local mem) r)))

;; cf. P:97-108 and DSL p.15 l.637
(define (probcc-reify/0 model)
  (let* ((mem probcc-thread-local)
         (tree (resetcc (set! probcc-thread-local probcc-mem-empty)
                        (probcc-unit (model)))))
    (set! probcc-thread-local mem)
    tree))

;; cf. P:118-128 and DSL p.14 l.627-628
(define (probcc-distribution pairs)
  (let1 (curr probcc-thread-local)
    (letcc/shift k
      (map (λ1-match/first ((,v ,p) (probcc-τ p (probcc-mem-resume curr k v)))) pairs))))
```

- `probcc-reflect` captures `curr` the same way and wraps only the `V` case (P:136-148).
- **No `dynamic-wind`** around the `set!`s. aux's shift re-enters dynamic-wind frames on every resume, which would replay the "before" thunk with stale values. Upstream does not protect against exceptions either: an error inside a model leaves the memory pointer set, and the next `probcc-reify/0` saves and restores whatever is there. Document this.
- **Single-threaded**, like upstream (deviation 15). The delimcc frame is per srfi-18/CML thread, but this variable is not.
- **Inner reification starts from empty memory** (P:106). Hence:
  - `probcc-letlazy` variables of the outer world are re-evaluated inside (N:65-81, the documented pitfall).
  - `probcc-memo` created outside the reification it is used in raises (P:471 `Not_found`; here `(exn hansei memory)`, deviation 16).
  - `probcc-letlazy/nesting` asks the parent.

### 5.3 `letlazy`, `letlazy_nesting`, `memo`, wrefs

```scheme
;; cf. P:321-330
(define (probcc-letlazy e)
  (let1 (loc (probcc-mem-new-loc))
    (τ (probcc-mem-ref loc probcc-thread-local
         (τ (let1 (v (e))
              (set! probcc-thread-local (probcc-mem-set loc v probcc-thread-local))
              v))))))

;; cf. P:342-370; deviation 13: no printf
(define (probcc-letlazy/nesting e)
  (let1 (loc (probcc-mem-new-loc))
    (set! probcc-thread-local (probcc-mem-set loc `(MT ,e) probcc-thread-local))
    (letrec ((deref
               (τ (match/first (probcc-mem-ref loc probcc-thread-local (τ #f))
                    ((MV ,v) v)
                    ((MT ,e) (let1 (v (e))
                               (set! probcc-thread-local (probcc-mem-set loc `(MV ,v) probcc-thread-local))
                               v))
                    (#f (let1 (mem probcc-thread-local)   ; not found: ask the parent, P:358-369
                          (letcc/shift k
                            (list (probcc-τ 1 (let1 (v (deref))
                                                (probcc-mem-prompt
                                                  (τ (set! probcc-thread-local (probcc-mem-set loc `(MV ,v) mem))
                                                     (k v))))))))))))
      deref)))
```

- **`probcc-memo f`** (P:466-479). At creation, `mset` an **empty** `pmap-empty` table at a new loc in the current memory. `(deref x)`:
  1. Look `x` up in the table found at `loc` in the current memory (`pmap-compare` order; `mref` raises `(exn hansei memory)` with a message naming `probcc-memo` when the loc is missing).
  2. On a miss, `v = (f x)`, which may change memory.
  3. **Re-read** the table (P:474), `pmap-add x v`, `mset` the result, answer `v`.

  It is unary like upstream. Multi-argument callers pass a list as the key (`(m (list i t dir))`, BA).
- **wrefs** (deviation 17):
  - `(probcc-wref-new v)`: a new loc, `mset` v, answer the loc.
  - `(probcc-wref-get r)`: `mref`.
  - `(probcc-wref-set! r v)`: `mset` into `probcc-thread-local`.
- The probability `1` of the ask-parent branch is exact (upstream `1.0`). `1 * x = x` for any x.
- The thunk of the ask-parent branch is forced by the **inner** explorer, which runs in the dynamic extent of the outer model after the inner `resetcc` has returned. `probcc-thread-local` is then the outer world's memory, and a shift inside `(deref)` reaches the outer delimiter. This is upstream's "ask the parent" (P:332-340; ML p.1 l.82-91 "walk the chain of active delimited continuations"). It requires C1: the inner explore loop is captured by the outer shift *[proto]*.

### 5.4 RNG and selectors (D6, D7)

**`(aux hansei random)`** transcribes the generator that the Python oracle validated (§8.2):

- **MD5** `md5-digest` (string or bytevector → 16-byte bytevector). RFC 1321, little-endian words, 32-bit arithmetic in fixnums (64-bit CHICKEN): mask with `#xFFFFFFFF` after each `fx+`, rotate as `(fxior (fxand (fxshl x s) #xFFFFFFFF) (fxshr x (- 32 s)))`. Precompute the 64 constants `K[i] = floor(|sin(i+1)| · 2^32)` as literals (not with `sin` at load time).
- **State**: a record made by `(make-ocaml-random-state seed)`, a vector of 55 fixnums plus `idx`; `ocaml-random-state?`, `(ocaml-random-state-copy s)`. `(ocaml-random-init! s seed)` re-seeds `s` in place. Seeding is `full_init [|seed|]`:
  - `st[i] = i`, `accu = "x"`;
  - for `i` in `0 … 109` (`55 + max 55 1`): `accu = md5(accu ++ decimal(seed))`, as raw 16 bytes; then `st[i mod 55] = (st[i mod 55] xor le32(accu[0..3])) land #x3FFFFFFF`;
  - `idx = 0`. `decimal` of a negative seed has a leading `-`.
- **`ocaml-random-bits!`**: `idx = (idx+1) mod 55`; `cur = st[idx]`; `new = (st[(idx+24) mod 55] + (cur xor ((cur lsr 25) land #x1F))) land #x3FFFFFFF`; `st[idx] = new`; answer `new`.
- **`ocaml-random-float! s bound`** = `(((r0/2^30 + r1)/2^30 + r2)/2^30) * bound` in flonums, drawing r0, then r1, then r2.
- **`ocaml-random-int! s n`** (OCaml `Random.int`, for `draw-integer`): `r = bits; v = r mod n`; retry while `r - v > #x3FFFFFFF - n + 1`. R5 checks exact values.
- **`ocaml-random-global`**: the module-level state, OCaml's default `Random` state (initial seed irrelevant: every `probcc-random-selector seed` re-initialises it). Mutated in place, never rebound, never restored on backtracking, like upstream's.
- **`make-ocaml-random-source seed-or-state`** → `(make-rng-source 'ocaml state draw-double draw-integer copier)`; a seed makes a fresh state, a state is **shared** (not copied):
  - `draw-double` = `(λ (s lo hi) (fp+ lo (fp* (ocaml-random-float! s 1.) (fp- hi lo))))`. With `lo = 0.` this is bit-identical to `rawfloat *. hi`, because `0. + x = x` and `hi - 0. = hi`.
  - `draw-integer` = `lo + int(hi - lo + 1)`.
  - `copier` = vector copy.

**Selectors** (core):

```scheme
;; the source over the global state, as OCaml's Random module functions
(define probcc-random-global-source (make-ocaml-random-source ocaml-random-global))

;; cf. P:221-233; deviation 18: answers (x ptotal); an rng-source may replace the seed
(define (probcc-random-selector seed-or-source)
  (let1 (src (cond
               ((rng-source? seed-or-source) seed-or-source)
               (else (ocaml-random-init! ocaml-random-global seed-or-source)   ; Random.init, P:222
                     probcc-random-global-source)))
    (λ (choices)
      (let* ((plus (op/plus))
             (ptotal (fold (λ (e pa) (plus pa (cadr e))) 0 choices))   ; pa +. p, P:230
             (r (rng-double! src 0. ptotal)))                          ; one draw = three bits
        (let loop ((pcum 0) (rest choices))
          (match/first rest
            (() (probcc-signal 'selector "Choice selection: can't happen" choices))  ; P:224
            (((,x ,p) . ,rest*) (let1 (pcum* (plus pcum p))
                                  (if (< r pcum*) (list x ptotal) (loop pcum* rest*))))))))))
```

- The selector answers **ptotal**, not the chosen probability (P:227).
- An exact `0` start plus flonum weights gives the same flonums as upstream's `0.0 +. p`.
- `probcc-signal kind msg . irritants` is the internal raiser of `(exn hansei)` conditions (§7).
- The seeding happens **when the selector is created**, as upstream's `let () = Random.init randomseed in` (P:222). Creating a selector inside a model (N:22, 31, 45) re-seeds the stream the outer selector draws from; this is what makes N:30-31 replay.

**Draw discipline** (inference reader, needed for replay):
- exactly one `rng-double!` per selector call;
- rejection draws only at nodes with two or more entries;
- importance draws once in `toploop` per sample, then once per `loop` step whose look-ahead leaves at least one open thread (I:168-174), even a single one; there is no draw only for a singleton `V` or `C`, or when `cch` is empty. Only `make-threads` recurses without a draw on a single thread (I:192-193);
- no draws when pre-exploration solves the tree.

### 5.5 `probcc-explore` (C1, C4-C6; I:16-28)

```scheme
(define (probcc-explore maxdepth choices)
  (letrec ((times (op/times)) (plus (op/plus)) (greater (op/greater))
           (loop (λ (p depth down choices ans susp)          ; answers (ans . susp)
                   (match/first choices
                     (() (cons ans susp))
                     ((((V ,v) ,pt) . ,rest)
                      (loop p depth down rest (pmap-insert-with plus v (times pt p) ans) susp))
                     ((((C ,t) ,pt) . ,rest)
                      (cond
                        (down (let1 (down* (or (not maxdepth) (< depth maxdepth)))
                                (match/first (loop (times pt p) (add1 depth) down* (t) ans susp)
                                  ((,ans* . ,susp*) (loop p depth down rest ans* susp*)))))
                        (else (loop p depth down rest ans (cons `((C ,t) ,(times pt p)) susp)))))))))
    (match/first (loop 1 0 #t choices pmap-empty '())
      ((,ans . ,susp)
       (sort (pmap-foldi (λ (v p a) (cons (probcc-value p v) a)) ans susp)
             (λ (a b) (greater (cadr a) (cadr b))))))))
```

- Product order: `pt *. p` (I:21, 25-26).
- `insert_with (+.) v new` stores `new + old`. Floating addition is commutative, so this matches the old `(plus old new)` too.
- Verify ✓ against the suite: `test/procc/grass-model*` gives `0.2838` / `0.3219999999999999` bit-exact (`grass-check*.scm` *[proto]*).

### 5.6 Two CHICKEN hazards for stochastic code (D11, D12)

1. **`map` is not re-entrant.** ✓ `(probcc-reify/exact (map (λ (x) (probcc-coin 1/2)) '(1 2)))` gives `(((V (#t #t #f #f #t #f)) 1))` with the pmap core (WP10 review, 2026-10-16): the four worlds, each at 1/4 (`probcc-explore 0` shows four `C` branches), share one list built in place and collate into one entry; the srfi-69 core of the first revision gave three entries at 1/2, 1/4 and 1/4 of that value. With `foldr` it gives the right four entries at 1/4. `test/map/reentrant` checks the one-entry result.
   - `probcc-map` (core, `(new)`): `(if (null? l) '() (let* ((h (f (car l))) (t (probcc-map f (cdr l)))) (cons h t)))`, left to right as OCaml's `List.map`.
   - Re-entrant alternatives: srfi-1 `fold`, `filter`, `filter-map`, `append-map`, `list-tabulate`, `count`; `foldr`; explicit recursion. But srfi-1 `list-tabulate` and `filter-map` call `f` **from the last element to the first** ✓: `(probcc-reify/0 (τ (list-tabulate 2 (λ (i) (probcc-coin (if (= i 0) 1/3 1/5))))))` chooses element 1 first. That breaks D19 and every depth-bounded or replay oracle. So upstream `List.map` inside models (LP:183, 317; Z:167; BN:112-113; BL:209, 219) and `Array.init` (DS:101) are ported **only** with `probcc-map` (over `(iota n)`, then `list->vector`).
   - The library uses `map` only inside shift bodies that build thunks (`probcc-distribution`, `probcc-reflect`). Comment why each is safe.
2. **csi with ≥5 operands.** ✓ `(probcc-reify/exact (list 'a 'b 'c 'd (probcc-coin 1/2)))` gives `(((V (a b c d #t)) 1/2) ((V (a b c d #t #f)) 1/2))` under csi; 4 operands, or `let1` first, are correct. Quasiquote templates and constructors are calls too: `` `(a b c d ,(probcc-coin 1/2)) `` gives the same wrong result, `(vector 'a 'b 'c 'd (probcc-coin 1/2))` gives `#(a b c d #t #f)`, and `(list (probcc-coin 1/2) 'a 'b 'c 'd)` gives `(#t a b c d #f a b c d)` ✓ (`crit-tests/op.scm`). Plain `call/cc` reproduces it (`core/e5.scm`: `((a b c d 1 11 12) …)`). Compiled code is correct. Rule D12. Report it upstream as an optional chicken-core item (§13).

### 5.7 Nested inference: what works and why

- Inner `probcc-reify/0` pushes a fresh delimiter and empty memory. Inner `dist`s capture up to it.
- The inner explorer forces thunks in the outer model's extent. A shift *directly* inside such a thunk (ask-parent, `dist_selector` calling `dist`, a thunk calling an outer choice) reaches the outer delimiter and captures the inner explorer's or sampler's loop.
- Correctness therefore requires that **every** inference loop that can be captured threads persistent state: `probcc-explore`, `probcc-explore/shallow`, `probcc-sample-dist/rejection`, `probcc-sample-dist` and `probcc-first-success`. Only `probcc-statistics` (top level, no model inside) may use mutable state, and it uses pmap anyway.
- Without the fix, N:152-158 gives total mass > 1 *[proto]*.

---

## 6. Algorithms of the new inference procedures

### 6.1 `pmap-compare` (OCaml `compare` stand-in)

It answers a negative, zero or positive fixnum.

- **Type ranks**: boolean < number < char < string < symbol < keyword < `'()`/pair < vector < bytevector < record.
- **Within a type**:
  - `#f < #t`;
  - real numbers by `<`, with `=` giving 0, so `-0.` = `0.` as in OCaml; an exact number sorts before an equal inexact one (`1` vs `1.`, kept distinct, as `equal?` keeps them);
  - non-real numbers by `(real imag)`;
  - chars by `char<?`; strings by `string<?`; symbols and keywords by name;
  - `'()` before any pair; pairs by car, then cdr (so lists compare lexicographically, a prefix first);
  - vectors by length, then elementwise (OCaml compares blocks by size first);
  - bytevectors by length, then bytes;
  - records by `record->vector` (the `(chicken memory representation)` import used by `aux.hakaru10.exact.scm:56`).
- Anything else (procedures, ports, hash tables, …) raises `(exn pmap compare)`, as OCaml's `compare: functional value` does.
- `(void)` must be orderable because `parsec-eof` answers it and `bounded_explore` takes unit models: give it rank 0 (below booleans), equal to itself. OCaml's `()` is the constant 0.
- NaN: equal to itself and below every other number, like OCaml.
- Key order differs from OCaml for variants (Scheme symbols compare by name, OCaml by declaration). This only affects tie order (deviation 6).

### 6.2 `probcc-explore/shallow maxdepth choices` (I:35-53)

`loop pc depth ans acc list` answers `(ans . acc)`:

- `((V ,v) ,p)` → `ans := insert-with plus v (times p pc)`.
- An entry `c` when `depth ≥ maxdepth` → `acc := (cons c acc)`, unchanged with its local probability.
- `((C ,t) ,p)` → `(ans . ch) = loop (times pc p) (add1 depth) ans '() (t)`; `ptotal` = left fold of `(plus pa p)` over `ch` from `0`:
  - `(= ptotal 0)` → drop (includes `ch = '()`);
  - `(< ptotal probcc-nearly-one)` → `acc := (cons `((C ,(let1 (ch* (probcc-map (λ1-match/first ((,x ,q) (list x (/ q ptotal)))) ch)) (τ ch*))) ,(times p ptotal)) acc)`;
  - else `acc := (cons `((C ,(τ ch)) ,p) acc)`.
- Result: `(pmap-foldi (λ (v p a) (cons (probcc-value p v) a)) ans acc)`.

**Never sort**: `acc` and every `ch` stay reversed, because the selector's cumulative scan depends on this order (replay).

### 6.3 `probcc-explore/bounded maxsize choices` (I:71-109)

- The queue is `(pmap-create number-compare)` keyed by mass, with values LIFO lists of thunks: `(pmap-insert-with append (times pc p) (list t) q)` stores `(append (list t) old)`, as `(@)` does.
- `loop explore? pc low high q qsize list`:
  - a `V` entry adds `pe = (times pc p)` to `low` and `high`;
  - a `C` entry, when `explore?`, is queued with `qsize+1`; otherwise `high += (times pc p)`.
- `next`:
  - `qsize = 0` → `(list low high)`;
  - `qsize < maxsize` → `pmap-delete-find-max`, pop the head thunk, put the rest back with `pmap-add` if non-empty, `(loop #t p … (sub1 qsize) (t))`;
  - else the same with `pmap-delete-find-min` and `explore? = #f`.
- Initial call: `(loop #t 1 0 0 q 0 choices)`. Every `V` counts, whatever its value (I:73-75).
- Start `low` and `high` from exact `0`: `0 + x = x` reproduces upstream's `0. +. x` bit for bit for flonum x. `pc = 1` exact likewise. When no `V` is reached, `low` stays the exact `0` (upstream `0.`); H22 expects `(0 1.)`. Documented in the procedure's comment.

### 6.4 `probcc-sample-dist/rejection selector n choices` (I:121-135)

```
loop pcontrib ans ch:
  (((V ,v) ,p))  → (pmap-insert-with plus v (times p pcontrib) ans)
  ()             → ans
  (((C ,t) ,p))  → (loop (times p pcontrib) ans (t))         ; no draw
  ch             → (match/first (selector ch) ((,slot ,ptotal) (loop (times pcontrib ptotal) ans (list (list slot 1)))))
driver: n times from the same root `choices`, threading ans; then, when (probcc-verbose),
        printf "rejection_sample: done ~a worlds\n" n; answer
        (sort (pmap-foldi (λ (v p a) (cons (probcc-value (/ p n) v) a)) ans '()) …descending as explore)
```

### 6.5 `probcc-sample-dist selector runner choices` (I:148-198)

- **`look-ahead pcontrib (ans . acc) entry`**:
  - `((V ,v) ,p)` → add `(times p pcontrib)`;
  - `((C ,t) ,p)` → force `(t)`:
    - `'()` → unchanged;
    - `(((V ,v) ,p1))` → add `(times (times p p1) pcontrib)`;
    - otherwise `ch`: `ptotal` (left fold); if `(< ptotal probcc-nearly-one)`, push `` `(,(normalized ch) ,(times p ptotal)) `` (normalized with `probcc-ratio`, so a forced list summing to exact `0` gives NaN masses at weight 0, as upstream, not a raise), else `` `(,ch ,p) ``.
  - Fold over the entries **left to right**; `acc` ends up reversed.
- **`loop pcontrib ans ch`**:
  - a singleton `V` → add;
  - `'()` → ans;
  - a singleton `C` → `(loop (times p pcontrib) ans (t))`;
  - otherwise fold `look-ahead`. If `acc` is empty, answer ans. Else `(selector acc)` (one draw, over the **reversed** acc, no reverse) → `(x ptotal)`, then `(loop (times pcontrib ptotal) ans x)`, where `x` is a forced pV list.
- **`toploop pcontrib ans cch`**: `(selector cch)` → `(loop (times pcontrib ptotal) ans x)`.
- **`driver pcontrib vals cch`**:
  1. `(match/first (runner pmap-empty (λ (ans) (toploop pcontrib ans cch))) ((,ans ,ns) …))`;
  2. add `vals` with `pmap-foldi` in ascending key order: `(pmap-insert-with plus v (times ns p) ans)`;
  3. print when verbose (`sample_importance: done ~a worlds`);
  4. answer the entries `(probcc-value (/ p ns) v)`, sorted as explore.
- **`make-threads pcontrib ans ch`**: fold `look-ahead`:
  - no threads → the exact answers, **without division and without a draw**;
  - exactly one `(ch1 p)` → `(make-threads (times pcontrib p) ans ch1)`;
  - otherwise `(driver pcontrib ans (reverse cch))` (the `List.rev` of I:195).
- Initial call: `(make-threads 1 pmap-empty choices)`.

### 6.6 `probcc-statistics seed1 seed2 sampler` (I:281-294)

- For each seed in `seed1..seed2`, iterate over `(sampler seed)`, accumulating per value `Σp` and `Σp²` in a pmap (`(p_old + p, p2_old + p·p)` as I:287).
- A `C` entry raises (upstream: match failure).
- With `n = seed2 - seed1 + 1`, answer `(v (/ Σp n) (fpsqrt (exact->inexact (/ (- Σp² (/ (* Σp Σp) n)) n))))`: the population standard deviation, with operations in upstream order. Rounding often makes the variance slightly negative (SM:425-460 shows `nan` standard deviations); CHICKEN's generic `sqrt` would answer a complex (`(sqrt -1e-20)` → `0.0+1e-10i`), `fpsqrt` answers `+nan.0` as OCaml. The std is always a flonum (deviation 27). Callers comparing with it must handle NaN.
- The output is sorted by descending mean, ties in descending key order. Upstream order is unspecified (`Hashtbl.fold`), deviation 21.

---

## 7. Public API summary

**`(aux hansei)`.**
- Existing:
  - parameters `op/times op/plus op/subtract op/divide op/greater`;
  - macros `probcc-τ probcc-value probcc-when probcc-reify/exact λ-probcc-bucket`;
  - procedures `probcc-explore probcc-normalize probcc-distribution probcc-reflect probcc-impossible probcc-unit probcc-bernoulli probcc-coin probcc-uniform probcc-uniform/range probcc-uniform/either probcc-geometric probcc-reify/0 probcc-reify probcc-reify/exact/a probcc-variable-elimination probcc-leaves probcc-dfs`.
- New:
  - memory: `probcc-thread-local probcc-mem-empty probcc-mem-new-loc probcc-mem-ref probcc-mem-set probcc-mem-prompt probcc-mem-resume`;
  - laziness and world-local references: `probcc-letlazy probcc-letlazy/nesting probcc-memo probcc-wref-new probcc-wref-get probcc-wref-set!`;
  - distributions and evidence: `probcc-geometric/count probcc-geometric/bounded probcc-observe`;
  - selectors: `probcc-random-selector probcc-dist-selector`;
  - inference: `probcc-nearly-one probcc-explore/shallow probcc-first-success probcc-explore/bounded probcc-sample-dist/rejection probcc-sample-runner/times probcc-sample-dist probcc-statistics probcc-normalize/total probcc-timeit`;
  - samplers: `probcc-sample-rejection/a probcc-sample-importance/a` and the macros `probcc-sample-rejection probcc-sample-importance`;
  - logic programming: `probcc-reify/part probcc-soft-cut`;
  - utilities: `probcc-prob-of probcc-at-least probcc-map-dist probcc-map probcc-bind probcc-verbose`.

**`(aux hansei pmap)`.** `pmap-empty pmap-create pmap-empty? pmap-add pmap-insert-with pmap-find pmap-find-min pmap-delete-find-min pmap-delete-find-max pmap-remove pmap-member? pmap-for-each pmap-map pmap-map/key pmap-fold pmap-foldi pmap->alist alist->pmap pmap-compare fx-compare number-compare pmap?`

**`(aux hansei random)`.** `md5-digest md5-hex make-ocaml-random-state ocaml-random-state? ocaml-random-state-copy ocaml-random-init! ocaml-random-bits! ocaml-random-float! ocaml-random-int! ocaml-random-global make-ocaml-random-source` (`md5-hex` is `md5-digest` as 32 lowercase hex digits, for R1).

**`(aux hansei lazy)`.** `probcc-lnil probcc-lcons probcc-lappend probcc-lhead probcc-ltail probcc-lreflect probcc-llength/at-most probcc-list->llist probcc-llength probcc-lmap probcc-lobserve probcc-split-at probcc-uniform/either/uncons probcc-lazy-permutation probcc-insert-shift probcc-lazy-permutation/ins`

**`(aux hansei parsec)`.** `parsec-stream-of-string parsec-string-of-stream parsec-stream-over parsec-eof parsec-pure parsec-empty parsec-sat parsec-char parsec-or parsec-alt parsec-ap parsec-fmap parsec-right parsec-left parsec-fix parsec-many parsec-many1 parsec-run/forward parsec-run/backward parsec-stream-len parsec-stream-at-most-len`

Errors are composite conditions `(exn hansei)` with a `kind` property (`memory`, `selector`, `malformed`; `random` from `(aux hansei random)`, WP2), raised through the internal `probcc-signal` with `abort` from `(chicken condition)` (not continuable, as OCaml's `raise`; a handler that returns raises "exception handler returned"; changed from `signal` in the WP1 review, §15), and usable with `⊦raises`. There is no `uniform` kind: `n ≤ 0` fails (deviation 7). `(aux hansei pmap)` raises `(exn pmap)` with kinds `not-found`, `empty` and `compare` (`pmap-compare` lives there, so H2 expects `(exn pmap)`).

---

## 8. Test plan

**Conventions (all test files).**
- Assertions:
  - "exact" is `(⊦ equal? expected got)`, used for exact rationals and for **bit-exact** flonums;
  - "≈" is `⊦=` (1e-6 absolute, `aux.unittest.scm:135-148`);
  - "≈rel" is a test-local `(rel-approx? 1e-9)` predicate for probabilities below 1e-4;
  - "set" compares after `(canon pv)`, a test-local sort of the entries by the `write` string of the value. Every oracle with **tied masses whose order comes from a variant or constructor ordering** (OCaml declaration order vs `pmap-compare`'s name order, deviation 6), e.g. IN's `formv` lists and `argmax` with several maxima, is "set".
- **Quoted upstream lists are re-sorted.** Upstream output is in `PM.foldi` order, not by mass (X:72-77 and R6 list `V 7 … V 0`, ascending in mass). Every quoted upstream list is written in the Scheme order before an `equal?`/`⊦=` check: descending mass, ties in descending `pmap-compare` key order (D5). Where the plan's tables quote upstream order, the implementer re-sorts.
- **Bit-exact checks print 17 digits.** CHICKEN prints flonums with 15 significant digits (`(exact->inexact 3/7)` prints `0.428571428571429`, `test/hansei.scm:75`), so a one-ulp failure would print identical expected and got values. The replay and bit-exact suites set `(flonum-print-precision 17)` (from `(chicken flonum)`) at the top of the file, and restore nothing (the process ends).
- Models written with **exact rationals where upstream uses dyadic floats** (0.5, 0.25), asserted exact, e.g. `1/128` for `0.0078125`. Models with non-dyadic probabilities (0.3, 0.85, 0.9) keep **flonum literals as upstream** and are asserted ≈, or exact where marked "bit".
- D12 (`let*` before any call, quasiquote template or constructor with ≥5 elements), D11 (`probcc-map`, never `map`, `list-tabulate` or `filter-map`), D19 (right-to-left OCaml order for tuples and arguments; left to right for `let … and …`) apply to every model. Each test file carries a comment explaining D11 and D12 once, with the reproductions of §5.6.
- Provenance column: the upstream line of the oracle; ✓ / *[proto]* / *[OCaml]* as in the status note; "py" means the Python replay transcription (§8.2).
- Each case ends with a `` `(doc …) `` citing its source, in the style of `test/hansei.scm:70-72`.
- The epilogue of `test/hansei.scm:197-211` (the output port check) is kept in `hansei.scm` with its `test/parameterize` comment. Every new test file gets the same check with a generic comment: "a resume that fails to rewind its `dynamic-wind` frames would leave a test's string port as the current output port, hiding the summary; check that the port is back".

### 8.1 `src/test/hansei.scm` (suite `hanseitest`): core

The existing 8 cases stay, renamed per C13, with unchanged expected values (C5 makes their order deterministic).

| Id | Name | Check | Expected | Kind | Provenance |
|---|---|---|---|---|---|
| H1 | `test/pmap` | 200 `pmap-add` of `(remainder (* 37 i) 200)` → `pmap->alist` sorted; persistence (old map unchanged after add/remove); `insert-with` stores `(f new old)` (`(pmap-insert-with cons 'k 'n m)` with old `'o` → `(n . o)`); `delete-find-min/max` of `{1,5,9}`; the AVL invariant at every node, checked by a test-local walk over the record (`pmap?`, then the tree vector): stored height = 1 + max of the children's, and \|hl − hr\| ≤ 2 (upstream's tolerance, PM:40, 51; a 1.44·log2 n bound would not hold: the worst case is ≈ 1.81·log2 n) | as stated | exact | PM:106-199 (new test) |
| H1b | `test/pmap/api` | `pmap-find` hit, miss with a default thunk (answers its value), miss without (`(⊦raises (exn pmap) …)`); `pmap-find-min`; `pmap-member?`; `pmap-for-each` visits keys ascending (collect into a list); `pmap-map` / `pmap-map/key`; `pmap-fold` / `pmap-foldi` ascending; `alist->pmap` then `pmap->alist` round trip; `pmap-empty?` on `pmap-empty` and after an add; `pmap?` on a map and on `'()`; `fx-compare` and `number-compare` signs; a `pmap-create` with a custom descending comparison gives descending `pmap->alist`; `pmap-delete-find-min`/`-max` of `pmap-empty` raise `(exn pmap)` | as stated | exact | PMI, PM |
| H2 | `test/pmap/compare` | `(sort (list "b" 'a 2 #t #\c '(1) #(0) 1. 1) (λ (a b) (negative? (pmap-compare a b))))` → `(#t 1 1. 2 #\c "b" a (1) #(0))`; `(pmap-compare -0. 0.)` = 0; `(⊦raises (exn pmap) (pmap-compare car cdr))` | as stated | exact | §6.1 |
| H3 | `test/explore/persistent` | the C1 probe | `(((V (((V a) 1/2) ((V #t) 1/2))) 1/2) ((V (((V a) 1/2) ((V #f) 1/2))) 1/2))` | exact | ✓ before the fix; I:16-28 |
| H4 | `test/explore/order` | `(probcc-reify/exact (let* ((a (probcc-coin 1/2)) (b (probcc-coin 1/2))) (list a b)))` | `(((V (#t #t)) 1/4) ((V (#t #f)) 1/4) ((V (#f #t)) 1/4) ((V (#f #f)) 1/4))` | exact | D5 |
| H5 | `test/explore/depth` | lawn model (the existing grass model returning `rain`): `(map cadr (probcc-reify/0 …))` = `(0.3 0.7)` (written with `probcc-map`); `(probcc-explore 1 …)` = 8 `C` entries, multiset of masses `{0.135 0.015 0.135 0.015 0.315 0.035 0.315 0.035}`; `(probcc-explore 4 …)` and `(probcc-explore #f …)` = `0.2838` / `0.322`. C4 on a literal tree (the lawn tree cannot show it: the root's `C` entries are always forced, building fresh closures): ``(define inner `(((C ,t0) 1/2)))``, then `(eq? t0 (car (cdr (car (car (probcc-explore 0 `(((C ,(τ inner)) 1))))))))` is `#t` | as stated | ≈ / set | DSL p.13 l.540-546, p.15 l.654-656, p.17 l.714-719 *[proto]* |
| H6 | `test/explore/trees` | the five literal trees of DSL p.9 l.364-374 (including a `((C ,(τ '())) p)` failure node) through `(probcc-explore #f …)` | each `(((V #t) 0.6) ((V #f) 0.4))` | ≈ | DSL p.9 |
| H7 | `test/reflect/inverse` | `(probcc-reify/exact (probcc-reflect T))` for `T` = a flat table and for the lawn tree | equals `(probcc-explore #f T)` | ≈ | DSL p.16 l.694-700, 728-733; NIPS l.65-67 |
| H8 | `test/normalize` | `probcc-normalize/total` of PE grass-cloudy: `(0.6471 (((V #t) 0.7079276773296245) ((V #f) 0.29207232267037547)))`; `(probcc-normalize '(((V a) 0)))` has a NaN mass | as stated | ≈; `nan?` | I:299-301; PE:26-28 *[OCaml]* |
| H9 | `test/uniform/edges` | `(probcc-reify/exact (probcc-uniform 3))` = `(((V 2) 1/3) ((V 1) 1/3) ((V 0) 1/3))` (tied exact masses, so D5's descending key order; today's srfi-69 table happens to print `0 1 2`; upstream has no tie, value 0 gets `1 - 2p`); `(probcc-reify/0 (τ (probcc-uniform 1)))` is one `C` entry of mass 1 (deviation 7); `(probcc-reify/exact (probcc-uniform 0))` = `()` | as stated | exact | P:156-165; deviation 7 |
| H10 | `test/geometric/count` | `(probcc-explore 4 (probcc-reify/0 (τ (probcc-geometric/count 0.85))))`: V 3 0.00286875000000000107, V 2 0.0191250000000000031, V 1 0.1275, V 0 0.85, plus `C` masses {1.13906250000000092e-05, 6.45468750000000426e-05, 0.000430312500000000248} | as stated | bit (values), set | X:63-66 *[OCaml]*, py ✓ |
| H11 | `test/geometric/bounded` | `(probcc-reify/exact (probcc-geometric/bounded 7 0.85))` = X:72-77 (V 7 1.4523050597099037e-06 … V 0 0.850000217845759, re-sorted by descending mass per §8 conventions); `(… 1 0.85)` = `((V 0) 0.869565217391304324) ((V 1) 0.130434782608695676)`; `(… 3 0.85)` = BL:332-334 | as stated | bit | X:72-77 py ✓; BN:189-190; BL:332-334 |
| H12 | `test/observe` | lawn with `probcc-observe` | `0.2838` / `0.322` | ≈ | P:213-214; X:48-56 |
| H13 | `test/letlazy` | `(let* ((x (probcc-letlazy (τ (probcc-coin 1/2)))) (c (probcc-coin 1/3))) (list c (x) (x)))` → 4 entries `(c b b)` with masses `1/6 1/6 1/3 1/3`; SL `tests4` (SL:73-79, oracle SL:82-84, with a call counter showing no evaluation in the u = 0 world) | as stated | exact, set | P:321-330; SL:73-93 *[proto]* |
| H14 | `test/memo` | SL `test2m` (8 × 1/8, SL:704-709) and `test3m` (4 × 1/4, SL:726-729), written with `let*` (D12); PE `grass_model_memo` = `0.4581` / `0.189`; `(⊦raises (exn hansei) …)` for a memo created outside and used inside a nested reify | as stated | exact / ≈ | P:466-479; SL:694-729 *[proto]*; PE:33-45 *[OCaml]* |
| H15 | `test/nested` | N `testn11` (N:24-25), `testn21` and `testn21'` (N:40-41, 56-57), `testnl10` (pitfall, N:72-73: `(((V (((V #t) 3/4) ((V #f) 1/4))) 1))`), `testnl11`, `testnl12` (N:97-98, 111-112), `testnl13` (N:124-130) | as quoted, with exact halves | exact, set | N *[OCaml]* |
| H16 | `test/nested/inference-about-inference` | PE:63-67 → `(((V #t) 1))`; PE:78-82 and PE:85-89 (`letlazy/nesting`) → `(((V #t) 7/8) ((V #f) 1/8))` with `(probcc-sample-rejection/a probcc-dist-selector 2 coin)` | as stated | exact | PE *[OCaml]*; EPP p.4 l.345-379 |
| H17 | `test/nested/thresholds` | N:317-345: for p in −0.5 … 1.5 step 0.1 (use exact tenths), the four combinations of `exact` / `sample-rejection dist-selector 2` × `flip` / `letlazy/nesting` | `1 / 1/2 / 0` and `1 / 7/8 / 5/8 / 0` as N:324-333 | exact | N:307-345 *[OCaml]* |
| H18 | `test/dist-selector` | `tf2_nested 1` and `2` with `probcc-dist-selector` | N:152-153 `(((V (((V #f) 1))) 3/4) ((V (((V #t) 1))) 1/4))`; N:156-158 (3 entries: 1/16, 9/16, 3/8) | exact, set | N *[OCaml]* |
| H19 | `test/first-success` | `(probcc-first-success `(((C ,(τ '(((V b) 1)))) 1/2) ((V a) 1/2)))`; `'()` → `'()`; a tree with only failures → `'()` | `(((V a) 1/2) ((V b) 1/2))` | exact | I:60-64 |
| H20 | `test/soft-cut` | `(probcc-reify/exact (probcc-soft-cut (τ (probcc-coin 1/2)) (λ (x) (list 'q x)) (τ 'r)))`; with a failing `p` → `r` | `(((V (q #t)) 1/2) ((V (q #f)) 1/2))`; `(((V r) 1))` | exact | LV:386-391 |
| H21 | `test/explore/shallow` | for the lawn tree, geometric/bounded 7 and `dcoin_and 5`: `(probcc-explore #f (probcc-explore/shallow k T))` ≈ `(probcc-explore #f T)` for k = 0, 1, 3 | equal | ≈ | I:35-53 |
| H22 | `test/explore/bounded` | SM:80-82 → `(0.999494006159381776 0.999494006159381776)`; SM:469-480 on `(probcc-reify/0 (τ (unless (dcoin-and 10) (probcc-impossible))))` (upstream `if not (dcoin_and 10) then fail ()`, a unit model; the masses equal those of `dcoin_and_true 10`): maxsize 1 → `(0 1.)`, 3 → `(0 0.0499999999999999889)` (`low` stays the exact `0`, §6.3), 5 → `(9.76562499999997764e-14 9.76562499999997764e-14)` | as stated | bit (`equal?`) | py ✓ (`scratchpad/bounded_check.py`) |
| H23 | `test/sample/look-ahead-exact` | DSL p.20: the model `(probcc-when (trues 20 (flips 0.5 20)) 'ok)` with lazy flips; `(probcc-sample-importance/a (probcc-random-selector 17) 1 model)` → `(((V ok) 9.5367431640625e-07))`; the eager `flips 1/2 12` with `probcc-reify/exact` → `(((V ok) 1/4096))` | as stated | exact | DSL p.20 l.866-909; SL:623-655 |
| H24 | `test/sample/pre-explored` | `sample_importance` cases solved by pre-exploration (no draw), with a selector that raises if called: SM:19-21, 28-30, 41-43, 150-152 | the exact results: `0.5/0.5`; `0.25/0.75`; talarm `0.10872000000000001 / 0.8912800000000001`; tiblt2 `0.021 / 0.009000000000000001` | bit | SM, py ✓ |
| H25 | `test/sample/dist-selector-importance` | IN:424-426 asymmetric game argmax over `(probcc-sample-importance/a probcc-dist-selector 2 …)` (the model is defined in this test or shared through a small `define` at the top of the file) | `((-0. (F_notall)))`, compared with `=` on the utility (a single maximum, so no tie order) | exact | IN *[OCaml]* |
| H26 | `test/statistics` | `(probcc-statistics 1 3 (λ (s) (list (probcc-value (/ s 10) 'a))))` → `((a 1/5 0.081649658092772609))`; a constant sampler `(λ (s) (list (probcc-value 0.1 'a)))` over seeds 1..3 gives a std that is a flonum, `0.` or `+nan.0` (never a complex: `(flonum? std)`) | as stated | ≈; `flonum?` | I:281-294; deviation 27 |
| H27 | `test/map/reentrant` | `(probcc-reify/exact (probcc-map (λ (_) (probcc-coin 1/2)) '(1 2)))`; the doc shows the wrong result of CHICKEN `map` without asserting it | 4 entries × 1/4 | exact | §5.6 ✓ |
| H28 | `test/wref` | `(let* ((r (probcc-wref-new 0)) (c (probcc-coin 1/2))) (probcc-wref-set! r (if c 1 2)) (list c (probcc-wref-get r)))` | `(((V (#t 1)) 1/2) ((V (#f 2)) 1/2))` | exact | deviation 17 |
| H29 | `test/verbose` | `(with-output-to-string (τ (probcc-sample-rejection (probcc-random-selector 1) 10 (probcc-coin 1/2))))` is `""`; with `(parameterize ((probcc-verbose #t)) …)` it is `"rejection_sample: done 10 worlds\n"` | as stated | exact | I:131, deviation 14 |
| H30 | `test/variable-elimination` | `flips_xor 10` → 1/2, 1/2 and `(probcc-leaves (probcc-reify/0 …))` = 1024; `flips_xor' 10` (VE) → 1/2, 1/2; `λ-probcc-bucket` xor → 1/2, 1/2 | as stated | exact | DSL p.17 l.751-771; X:305-329 |
| H31 | `test/drunk-coin` | `(probcc-reify/exact (dcoin-and 10))` with flonum literals | `#t 9.76562499999997764e-14`, `#f 0.0526315789473632695` | bit (fall back to ≈rel 1e-12 if an ulp differs, and record it) | SM:236-238; DSL p.19 l.814-817 |
| H32 | `test/reify/part` | `(probcc-reify/part 3 a-list)` where `a-list` is RT l.340-346 | `((() 1/2) ((#f) 1/8) ((#t) 1/8))` (set) | exact | RT p.5 l.360-365 *[proto]* |
| H33 | `test/bind` | `f` answers a pV. Bind law: `(probcc-explore #f (probcc-bind (probcc-reify/0 m) f))` equals `(probcc-reify/exact (probcc-reflect (f (m))))`, for a coin and a two-coin model. Unit law: `(probcc-explore #f (probcc-bind (probcc-unit x) f))` equals `(probcc-explore #f (f x))` | equal | exact | DSL p.10 l.414-425 |
| H34 | `test/parameterize/memory` | mirrors the existing `test/parameterize` (`test/hansei.scm:182-186`) with the new memory: a `probcc-letlazy` variable forced inside `(parameterize ((p 1)) …)` in one world and outside in another; a nested `probcc-reify/exact` inside a `parameterize` within a model; a `probcc-memo` called inside a `parameterize`. Each answers its exact table, and the output-port epilogue still passes | exact tables | exact | §5.2; idiom review |
| H35 | `test/timeit-runner-memory` | `(with-output-to-string (τ (probcc-timeit (τ 42))))` starts with `"\nTime spent: "` and ends with `" sec\n"`, and the value is 42 (capture with a `let1` inside the thunk); `((probcc-sample-runner/times 3) 0 add1)` = `(3 3)`; `(probcc-mem-ref loc probcc-mem-empty (τ 'd))` = `d`; `(⊦raises (exn hansei) (probcc-mem-ref loc probcc-mem-empty))` | as stated | exact | I:142-146, 303-308; P:52-54 |
| H36 | `test/map-dist` | `(probcc-map-dist length '(((V (a)) 0.25) ((V (b)) 0.5) ((V (a b)) 0.25)))` = `(((V 2) 0.25) ((V 1) 0.75))`; a `C` entry raises `(exn hansei)` | as stated | exact | BL:456-460 |

Target runtime: under 3 s.

### 8.2 `src/test/hansei-random.scm` (suite `hansei-random`)

| Id | Check | Expected | Kind | Provenance |
|---|---|---|---|---|
| R1 | `md5-hex` of `""`, `"a"`, `"abc"`, `"message digest"`, `"abcdefghijklmnopqrstuvwxyz"`, `"A…Za…z0…9"`, `"1234567890"`×8 | `d41d8cd98f00b204e9800998ecf8427e`, `0cc175b9c0f1b6a831c399e269772661`, `900150983cd24fb0d6963f7d28e17f72`, `f96b697d7cb7938d525a2f31aaf161d0`, `c3fcd3d76192e4007dfb496cca67e13b`, `d174ab98d277d9f5a5611c2c9f419d9f`, `57edf4a22be3c955ac49da2e2107b67a` | exact | RFC 1321 A.5 ✓ |
| R2 | state after `make-ocaml-random-state` for seeds 1, 17, 0, −5: `st[0..4]` | 1: `613788242 197116386 1014951421 468579290 805446940`; 17: `43684900 826807282 853359337 631525392 682719058`; 0: `76722835 608589901 287828993 653199777 415478086`; −5: `640528891 982617058 790696909 229908377 1042402058` | exact | py ✓ |
| R3 | first five `bits` | 1: `111472438 155495486 1029890784 56915826 660037571`; 17: `230024973 197978033 495359752 1036278348 906260834`; 0: `218040752 178628190 911923154 33712936 1041571162`; −5: `638983422 1072006411 37028206 270829641 521483706` | exact | py ✓ |
| R4 | first three `(rng-double! src 0. 1.)` of a fresh source | 1: `0.9591605366625044 0.0477194009485728 0.6452449217046778`; 17: `0.4613397197652435 0.18637992054598515 0.699662755317079`; 0: `0.8492946197896828 0.6295131556410704 0.4331830841471257`; −5: `0.03448520507512965 0.9729778948106519 0.34460494547462833` | bit | py ✓ |
| R5 | `rng-source-copy` replays 1000 draws; `rng-integer!` stays in range; `ocaml-random-int!` after `make-ocaml-random-state`, five draws: seed 1, n 10 → `8 6 4 6 1`; seed 1, n 1000 → `438 486 784 826 571`; seed 17, n 10 → `3 3 2 8 4`; seed 17, n 1000 → `973 33 752 348 834`; `ocaml-random-init!` on a used state gives R3's first `bits` again | identical / as stated | exact | protocol; py (`scratchpad/rint.py`), cross-check with OCaml in WP2 (`crit-tests/r.ml` extended with `Random.int`) |
| R6 | **samplingM replays** with `probcc-random-selector`, flonum models (`tflip2_shared`, `tflip2_noshared`, `talarm`, `geometric/count 0.85`, `tiblt2` with symbols/lists for the objects, `dcoin_and 10`, `dcoin_and_true 10`) | see the table below | bit | SM, py ✓ |
| R7 | `probcc-random-selector` over `make-stdgen-source` and `make-chicken-random-source` (interop) and an explicit OCaml source; a seed selector created after another re-seeds the shared global state (two `(probcc-random-selector 1)` selectors, the second created after the first drew, draw the same first value) | answers `(x ptotal)`; selects the only positive-mass entry of `((a 0.) (b 1.))` | exact | D6 |
| R8 | selector on `'()` raises | `(⊦raises (exn hansei) …)` | — | P:224 |
| R9 | **Nested replays** (N), flonum models in upstream order | the upstream assertions of N:27-31 (`testn12`, the outer table `0.5/0.5` with inner `0.6/0.4`), N:43-50 (`testn22`), N:137 (`testnl13`), N:148-150, N:162-164, N:166-178 (`tf2_nested 3`: `0.42`, `0.18`, `0.4`), N:203-211 (`testnl14`), N:231-287 (`dcoin_and 10` under nested importance, e.g. N:232-233 = `0.052301965599999993` = SM:252), N:346-441 (the red-light `game` planning with risk: `noisy_or`, `being_caught`, `game 0.2` with 10 samples, 11 entries, and `game 0.5` with 50 samples, 5 entries; added in the final review, §15) | bit, written in the Scheme order | N *[OCaml 3-draw]*, `crit-tests/nested.3draw.txt` |

R6 table:

| SM lines | Call | Expected |
|---|---|---|
| 16-18 | rejection, seed 1, 100, `tflip2_shared` | `#t 0.48`, `#f 0.52` |
| 25-27 | rejection, seed 1, 100, `tflip2_noshared` | `0.24 / 0.76` |
| 38-40 | rejection, seed 1, 100, `talarm` | `0.14 / 0.86` |
| 41-43 | importance, seed 1, 100, `talarm` | `0.10872000000000001 / 0.8912800000000001` |
| 62-64 | rejection, seed 17, 100, geometric | `V 2 0.04`, `V 1 0.17`, `V 0 0.79` |
| 66-69 | importance, seed 17, 100, geometric | `V 4 0.0004500000000000001`, `V 3 0.002475000000000001`, `V 2 0.01957499999999998`, `V 1 0.1275`, `V 0 0.85` |
| 71-74 | importance, seed 17, 1000, geometric | `V 6 2.2500000000000005e-05`, `V 5 9.000000000000002e-05`, `V 4 0.0004725000000000003`, `V 3 0.002834999999999997`, `V 2 0.019080000000000406`, `V 1 0.1275`, `V 0 0.85` |
| 145-147 | rejection, seed 1, 100, `tiblt2` | `#f 0.02` |
| 243-246 | rejection, seed 17, 100, dcoin | `#f 0.05` |
| 247-250 | rejection, seed 17, 10000, dcoin | `#f 0.0514` |
| 252-255 | importance, seed 17, 100, dcoin | `#f 0.05230196559999999` |
| 259-262 | importance, seed 1, 5000, dcoin | `#t 6.999999999999984e-14`, `#f 0.052635478842930135` |
| 266-269 | importance, seed 17, 5000, dcoin | `#t 5.999999999999987e-14`, `#f 0.052641559436170146` |
| 398-401 | rejection, seed 1, 10000, `dcoin_and_true 10` | `()` |
| N:148-150 | `tf2_nested 100 (random_selector 1)` (flat) | `0.24 / 0.76` (same as SM:25-27) |
| N:162-164 | `sample_rejection (random_selector 1) 100 (fun () -> tf2_nested 1 dist_selector)` (nested) | `(((V (((V #f) 1.))) 0.76) ((V (((V #t) 1.))) 0.24))` |

Models use `let*` in upstream order: `drunk_coin` draws `x` then `lost` (SM:224-227), and `talarm` draws `earthquake` then `burglary` (`let … and …`, D19), as the Python oracle did. N's models create their selectors inside the model (`(probcc-random-selector 17)` evaluated per world), which re-seeds the global state (D6). **Triage order** if a replay fails:
1. R2-R4 (the RNG);
2. the selector scan and the ptotal fold;
3. the `acc` reversal and the `List.rev` of `make-threads`;
4. the entry order of `probcc-distribution`;
5. float operation order (§6).

Target runtime: under 5 s (two 10000-sample rejections and two 5000-sample importance runs, over a compiled library).

### 8.3 `src/test/hansei-lazy.scm` (suite `hansei-lazy`)

| Id | Check | Expected | Kind | Provenance |
|---|---|---|---|---|
| L1 | SL `tests1`, `tests2`, `tests4` with a call counter for `expensive_f`; `tests3` with CHICKEN's `delay`/`force` for OCaml's `lazy` (D9's evidence, added in the final review, §15) | SL:24-26 (4 × 1/8 + `None` 1/2), SL:44-48 (8 × 1/16 + 1/2), SL:82-84; counts 4 / 12 / per SL:85-93, comparing the counts as secondary oracles; `tests3`: `None` 1/2, `(11 11 #t)` 1/4, `(11 11 #f)` 1/4, 2 calls (wrong, as SL:63-70's is, differently) | exact | SL *[OCaml]* |
| L2 | SL `testl1`/`testl2`, `testl3`/`testl4`, `testl51`-`53`, `testl54` | SL:108-109 = 121-122; 153-156 = 173-176; 233-234 = 248-249 = 263-264; 280-281 | exact, set | SL *[OCaml]* |
| L3 | `test_ll_at_most` | `5 1/32, 4 1/32, 3 1/16, 2 1/8, 1 1/4, 0 1/2` | exact | SL:357-366 |
| L4 | PCFG `pcfg11`, `pcfg12`, `pcfg13`, and `pcfg` with `probcc-observe` | SL:387; 401; 451-454; 468 (`("flies" "flies") 0.048`, `("flies" "ants") 0.072`) | ≈ | SL:371-478 |
| L5 | `test1m` (32 × 1/32) | SL:675-690 | exact, set | SL:661-690 *[OCaml]* |
| L6 | `testd1`, `testd2` (lazy vs delayed) | `((2 2) 1/4) ((1 1) 1/2)`; `(2 1/2) (1 1/2)` | exact | SL:801-860 *[OCaml]* |
| L7 | `full_10ltree_obs 4` with `(probcc-sample-importance/a (probcc-random-selector 17) 1 …)`, the digits drawn with a test-local transcription of upstream's flonum `uniform` (P:156-165; WP6, §15); with `probcc-uniform` the answer is the exact `(((V 1) 1/10000))` | `(((V 1) 0.000100000000000000032))` (bit, see L17) | bit | SL:602-618 *[OCaml 3-draw]* |
| L8 | `probcc-split-at` | LP:149-152 four cases plus `(probcc-split-at 5 '(1 2 3))` = `((3 2 1) ())` | exact | LP, LPI:35 |
| L9 | `probcc-uniform/either/uncons '(1 2 3)` | `((3 (1 2)) 1/3) ((2 (1 3)) 1/3) ((1 (2 3)) 1/3)` | exact, set | LP:169-172 |
| L10 | `probcc-insert-shift` | the eight cases of LP:252-263 | exact | LP |
| L11 | `f1` (no memo, 16 × 1/16), `f2` (memo, 4 × 1/4), `f3` (memo + rejection, 2 × 1/4), `f4` (letlazy over permutations, 2 × 1/2) | LP:27-41, 44-52, 72-83, 101-107 | exact, set | LP *[OCaml]* |
| L12 | `probcc-lazy-permutation '(1 2 3 4)`, forced with `probcc-map` | the raw `probcc-reify/exact` output: 24 permutations × exact `1/24`; `probcc-normalize/total` of it: total exactly `1`, and the normalized masses ≈ `0.041666666666666664` (normalize is inexact, D8, D15) | exact (raw, total) / ≈ (normalized) | LP:190-217 *[OCaml]* (0.999999999999999667 in floats) |
| L13 | `f5` over `lazy-permutation '(1 2 3)` | 6 × 1/6: `(3 2 3 2) (3 1 3 1) (2 3 2 3) (2 1 2 1) (1 3 1 3) (1 2 1 2)` | exact, set | LP:219-229 |
| L14 | `probcc-lazy-permutation/ins 4` (24 × 1/24) and `f5` over `/ins 3` | LP:290-317, 319-330 | exact, set | LP *[OCaml]* |
| L15 | SO with test-local `build_list`/`to_list`/`is_sorted`/`gte`/`bit_list`/`num_list` (strict-head lazy list, SO:19-20): `bit_list 4` (16 × 1/16), sharing pairs (8 × 1/8), `gte` 2 bits (5/8, 3/8) and 3 bits (9/16, 7/16), `is_sorted (num_list 2 2)` 5/8, `(num_list 2 3)` 9/16, `(num_list 3 3)` 15/64 = `testp3_theory`, `(num_list 4 3)` 165/2048 = 0.08056640625 = `testp4_theory` | as stated | exact | SO:57-162 *[OCaml]* |
| L16 | `probcc-lappend`, `probcc-lmap`, `probcc-list->llist`, `probcc-llength`, `probcc-lhead` and `probcc-ltail` of a cons and of nil (fails: `()`), `probcc-lreflect`, `probcc-lobserve` (equal list → `(((V #<void>) 1))`, a different list or length → `()`) | small deterministic cases | exact | SL:329-346; M1:57-100; M2:113-119 |
| L17 | **SL replays**: SL:126-132 (`testl1`, `testl2`), SL:180-206 (`testl3`, `testl4`, rejection and importance), SL:432 (`pcfg12`), SL:482 (`pcfg`), SL:531-570 (`full_10tree_obs`), SL:603-618 (`full_10ltree_obs 4`, which supersedes L7's ≈rel), SL:633-652 | upstream's asserted values, bit, in the Scheme order | bit | SL *[OCaml 3-draw]*, `crit-tests/slazy.3draw.txt` |
| L18 | SL sprinkler `grass_model0`, `grass_model1` (memo over days; added in the final review, §15) | `(((V #t) 1.))`; `(((V #f) 0.322) ((V #t) 0.2838))` (upstream's pattern matches these floats exactly) | bit | SL:731-778 *[OCaml]* |

`(num_list 4 5)` (2^20 worlds, SO:164-171) was planned for the long suite; it takes 70 ms and is in L15 (`test/sorted`), with SO:177-183's sampling runs as upstream gives them under OCaml today (WP9, §15).

### 8.4 `src/test/hansei-examples.scm` (suite `hansei-examples`)

| Id | Check | Expected | Kind | Provenance |
|---|---|---|---|---|
| E1 | PE grass with cloudy; normalized | `#t 0.4581`, `#f 0.188999999999999974`; `0.707927677329624472 / 0.292072322670375473` | bit (WP8) | PE:9-28 *[OCaml]* |
| E2 | X basic: `tflip2_shared` (1/2, 1/2), `tflip2_noshared` (1/4, 3/4), `talarm` (`0.10872000000000001`/`0.8912800000000001`), `grass_model` (`0.2838`/`0.3219999999999999`), `uniform_range 1 8` as `(probcc-uniform/range 1 9)` (8 × 1/8) | as stated | exact / bit | X:15-62 *[OCaml]* |
| E3 | X IBAL tests `tfooibl1-3`, `tiblt2`, `tmusictr41`, `tmusictr42`, through `probcc-observe` | X:101-229 asserted values | bit (WP8) | X *[OCaml]* |
| E4 | `flips_and 10` with exact halves | `#t 1/1024`, `#f 1023/1024` (X: `0.0009765625 / 0.9990234375`) | exact | X:248-253 *[OCaml]* |
| E5 | DSL lawn normalized; NIPS two coins observed `x \|\| y` | `0.468471442720369724 / 0.53152855727963022` (the paper's, an earlier version; today's upstream gives `0.46847144272036984 / 0.53152855727963011`, WP8); 3 × 1/4 | ≈ and bit / exact | DSL p.5 l.194-197; NIPS l.24-27, 63 |
| E6 | NO `model_fwd`, `tq1m` (memo + `fixm`), `tq1`, `tq2`, `tq1l` (20 nodes; predecessor lists built as `insert_with (@)`, prepending) | `0.81441875000000008` ×3; `0.6864675`; `0.901446179820137083` | bit (WP8) | NO:59-220 *[OCaml]*; *[proto]* 127 ms |
| E7 | BT exact with VE: `test1exact`, `test2exact` | BT:255-260, 262-275 values | bit with a VE reflecting upstream's order, ≈rel 1e-9 with `probcc-variable-elimination` (WP8) | BT *[OCaml]*, *[proto]* 33 ms each |
| E8 | HM exact without VE, n = 10 (State8 … State1, HM:149-156) and VE model n = 10 (HM:281-296, normalized as well) | as quoted | bit (normalized in upstream's order; VE as E7), WP8 | HM *[proto]* 681 ms / 1 ms |
| E9 | HM call counters v1 n=4, v2 n=4, v3 n=4, v3 n=10 (a vector counter incremented by the transition) | `#(0 8 22 62 176 502 0 …)`, `#(0 8 22 62 176 0 …)`, `#(0 8 22 22 22 0 …)`, `#(0 8 22 22 22 20 20 22 22 22 22 0)` | exact | HM:196-278 *[proto]* |
| E10 | BN exact: `plane_state_0` (1/100), `geometric_bounded 1/2`, `(Some 1) 0` noisy (`0.0211504347826093099`), ideal (`0.00130434782608695684`), `(Some 1)` two blips (`()`), motion with VE (`0.00059257469565211177`) | as quoted | ≈ / ≈rel | BN:185-252 *[proto]* (≤ 80 ms each) |
| E11 | M1 exact with lazy lists (`probcc-lappend`/`probcc-lmap`): 7 entries (G 0.00015 … B 0.0075) and evidence 0.054 | M1:174-180 | ≈ | M1 *[proto]* (strict, 3 ms) |
| E12 | CB exact with `probcc-memo`, m = 3 blue observations, `probcc-uniform/range 1 9` | unnormalized `1 0.0325, 2 0.0240625, 3 0.02125, 4 0.01984375, 5 0.019, 6 0.0184375, 7 0.018035714285714287, 8 0.017734375` (the 1/8 prior included: for n = 1, (0.8³ + 0.2³)/2 · 1/8 = 0.0325) | ≈ | analytic (examples reader, exact fractions) |
| E13 | IN two-bit game: `exact_reify (agent_A_model ut1)` (IN:131-134), `argmax` (IN:158), the speaker table (IN:181-183), `sample_rejection dist_selector 1` and `2` (IN:185-205) | as quoted; normalize `-0.` with `(+ x 0.)` before comparing | ≈, set where masses tie | IN *[OCaml]* |
| E14 | IN some/all game `argmax` (IN:263, 273, 334-335, 337-339) and asymmetric utility (IN:376, 386, 409-414) | as quoted; IN:409-410 `[(-0., [F_notall]); (-0., [F_some; F_notall])]` comes out reversed (symbols order `F_all < F_none < F_notall < F_some`, OCaml `F_all < F_some < F_notall < F_none`, IN:212); the tied `exact_reify` tables keyed by `formv` lists (IN:306-310, 331-335) likewise | ≈, **set** for every multi-maximum argmax and tied table | IN *[OCaml]* |
| E15 | BA memo: `pos (1,0) = (3,3)` asked twice (0.01), (3,3) and (3,4) at t1 (0.0015), `anyblip1` (0.02882 twice), the conjunction (0.001919686) | as quoted | ≈ | BA:184-216 |
| E16 | BL lazy exact: lazy plane 0.01; one-plane ideal; two lazy planes `0.000200000000000000064`; `parm_n 3` two blips; three blips `1.72212182417348577e-08`; motion `0.000195652173913043515`, and the same value by SMC with `probcc-reflect` (BL:421-428) | as quoted | ≈rel 1e-9 | BL:264-428 |

Target: under 10 s. If E8's no-VE model or E16's SMC exceed 2 s each in csi, move them to the long suite.

### 8.5 `src/test/hansei-logic.scm` (suite `hansei-logic`)

Every oracle below was reproduced *[proto]* by the logic reader's `logic-proto/*.scm` with `letlazy/nesting` where noted. The values are exact rationals of the quoted floats.

| Id | Check | Expected | Provenance |
|---|---|---|---|
| G1 | LV blists (test-local `nil`, `cons`, `list_of_blist`, `append`, `a_boolean`, `a_blist`, `bl_compare`, `last`, `del`, `list_haslen`, `prog3`) using `probcc-letlazy/nesting` (LV:19) | LV:72-74, 89-98 (cheap, wrong on purpose), 114-117, 128-134, 141-150, 155-163, 183-189, 193-199, 205-217, 229-232, 238-245, 252-259, 287-296, 304-322, 338-349, 397-401, 404-424 (16 entries), 429-444 | LV *[OCaml]* |
| G2 | the plain-`letlazy` variant of `prog3` at `Some 9` gives wrong weights (assert they differ from G1's); never run it with `None` (diverges) | inequality | LV *[proto]* |
| G3 | RT `a_list` / cheap split at depth 15 (6 × 1/4096) | RT p.5 l.360-392 | *[proto]* 30 ms |
| G4 | DS `powerset '(1 2 3)` with srfi-1 `fold` (8 × 1/8) and `schedule_model` with `probcc-memo`, the vector built with `probcc-map` and `list->vector` | `#((Supermarket Shop) (Barber Walk) ())` (symbols) at exactly 1/8957952 | DS:39-48, 104-108 *[OCaml]* |
| G5 | PZ bridge with the HANSEI `SimpleNonDet` (`choose` = `probcc-uniform/either`, `[]` fails); traces as values | the set of the two traces of §5 of the logic report | PZ:94-183 *[proto]* (weights not asserted) |
| G6 | TI with `probcc-wref-*` and `probcc-geometric/count 0.1`: TI:141-199, 236-256, 262-280, and the comment-only oracles TI:66-77 (19 entries at depth 5) | as quoted | TI *[proto]* (TI does not run upstream) |
| G7 | PS palindromes with `parsec-or` (left-nested) and `parsec-alt`: PS:213-225, 238-251 | `()`, 1/64, 1/256, 1/32, 1/512; `()`, 1/25, 1/125, 1/25, 1/625 | PS *[OCaml]* |
| G8 | PS five-letter palindromes with `run/backward #f` and `stream-len 5` | 8 × 1/256000 | PS:295-309 *[proto]* |
| G9 | PS `many` table: PS:321-350, 369-371, 377-385, 389-395 (the two `C` nodes of 1/256 matched structurally, as `test/geometric` does) | as quoted | PS *[OCaml]* |
| G10 | PS `num "123"` → 123; `expression "10"` → 1/8; `"(10+5*2)/4"` → 5 at 1/65536 | as quoted | PS:435-466 |
| G11 | PS `factor` at `Some 14` over the balanced `exp_stream`: membership of `(9999 "9999")` and `(9998 "9998")` at 81/200000000 | membership | PS:525-529 *[proto]* 0.67 s |
| G12 | the nested-inference law of LG l.1212-1215 / RT l.911-918 on `parsec-many` over `parsec-stream-over` | both sides equal at depth 6 | LG |
| G13 | **PS replays**: PS:492-501 (`sample_importance (random_selector 17) 80` over `exp_stream`: `7139`, `5164` at `0.000390624999999999967`) | bit, Scheme order | PS *[OCaml 3-draw]*, `crit-tests/parsec.3draw.txt` |

`zebra` (Z:181-190: weight 1/199065600000, unique solution) is in the long suite (about 8 s with an alist store; 2.7 s with the pmap, WP9).

### 8.6 `src/test/hansei-long.scm` (suite `hansei-long`, `make test-hansei-long`)

- Z zebra.
- SO `num_list 4 5` (`0.04993438720703125`).
- BN two-plane cases (`1/5000`, `3.83795309168443282e-06`).
- BL `q3b_s0..s2` (384 worlds each; `probcc-map-dist` of the plane count: `1.72212182417348577e-08`, `5.81216115658552e-11`, `2.07608302413096791e-12`, BL:462-489).
- Replays (bit, 3-draw global RNG): M1:196-205 (`sample_importance (random_selector 17) 1000 main`, 7 entries, evidence 0.054); PS:504-512 (seed 37, 30000 samples), PS:531-546 (2000 samples, the balanced `exp_stream` of PS:515-523), PS:549-557 (30000 samples).
- Statistical tests with OCaml sources over seeds 1..20 via `probcc-statistics`, each mean within 4σ/√20 of the exact value and an absolute floor (a NaN std, §6.6, uses the floor alone):
  - BT, 1000 samples (±0.05);
  - HM, 2000 samples, normalized (±0.03);
  - CB, 10 blues, 2000 samples (P(n=1) ≈ 0.41196 ± 0.05; analytic posterior from the examples reader);
  - dcoin, 5000 samples (`#f` ≈ 0.0526 ± 10%);
  - SO `model n` (SO:119-123, 20-bit numbers), n = 2, 3, 4 with 100, 100, 200 samples (SO:185-200), against `invfact n` = 1/n! (± 10%);
  - BN one plane in motion, two blips, 1000 samples (BN:254-257), against the exact `0.00059257469565211177` (BN:246-250) (± 10%);
  - BL `qs3b_s0`, 1500 samples (BL:513-520), the number of planes against the exact `1.72212182417348577e-08` (± 10%).
  (The last three were added in the final review, §15.)

*WP9 placement (§15):* measured over the compiled library, SO `num_list 4 5` (70 ms), the BN two-plane cases (1.7 s), BL `q3b_s0..s2` (0.26 s), the M1 replay (12 ms) and PS:531-546 (0.14 s) are fast, so they went to the fast suites (`hansei-lazy.scm`, `hansei-examples.scm`, `hansei-logic.scm`), next to the models they share; `hansei-long.scm` keeps zebra, PS:504-512 and PS:549-557, and the four statistical tests (about 27 s in all).

### 8.7 Doc sections

Each file opens with a `(doc r)` in the style of `test/hansei.scm:9-57`, quoting with `cite/quote` and linking with `cite/a`:

| File | Doc sections |
|---|---|
| `hansei.scm` | Keep "Introduction" and "Implementation". Add: "Reification and reflection" (DSL p.14-17); "Exploration and its order" (I:11-28, D5); "World-local memory" (P:59-75, ML p.1 l.66-91); "Laziness" (DSL p.19-21, P:270-319); "Memoization" (P:449-464); "Nested inference" (EPP p.4 l.345-379, N:75-81); "Inference about inference" (PE:69-77); "Samplers and selectors" (DSL p.18-19, EPP p.6); "Two CHICKEN hazards" (§5.6); the **deviation register** (§9) as a table, as `test/hakaru10.scm:1309-1340` does. |
| `hansei-random.scm` | "Replaying upstream's records": the RNG, why three draws, provenance of the Python oracle (§10.3) |
| `hansei-lazy.scm` | H `#all-different` (l.243-end), SL commentary, H `#sorted` (l.203-232) |
| `hansei-examples.scm` | one section per H anchor (`#examples-tests`, `#colored-balls`, `#blood-type`, `#hmm`, `#noisy-or`, `#music`, `#blip`, `#bounded-rationality`) |
| `hansei-logic.scm` | LG `#vs-prolog`, `#logic-var`, `#append`, `#daily-schedule`, `#zebra`, `#parsec`, `#many`; Prolog counterparts quoted from PD:187-217; RT Figure 1 (l.285-308) |

Show model sources with `define/shown` where the hakaru10 suites do (`test/hakaru10-exact.scm:32-41`).

---

## 9. Deviation register

Each item carries `; deviation N (file:lines): …` at the definition.

1. **Representation** (T:4-15): entries `((V v) p)` / `((C t) p)` and pairs `(v p)` put the probability second. Weights may be exact; new code keeps them exact (D8). `op/*` are pluggable (new).
2. **Direct reifier** (P:24, 97-108): no `pReq`/`interp`/`read_answer`; the `resetcc` body answers the interpreted tree (§5.1).
3. **Single untagged delimiter** (P:26): a user `resetcc` inside a model captures HANSEI choices (D17).
4. **`shift` vs `shift0`** (P:120, 142, 361): equivalent here (§5.1).
5. **Output order** (I:28; LV:68-70): descending probability, stable over upstream's order (D5); `+inf.0` or `#f` for `None`; `reify/part` keeps explore order.
6. **Total order** (PM:101): `pmap-compare` orders symbols by name, not by declaration; raises on unorderable values (§6.1).
7. **`uniform`** (P:156-165): `n = 1` is a one-branch choice (user, commit 0af8975); `n ≤ 0` fails instead of raising; masses are exact `1/n`. A count that is not an integer (`5/2`, `2.5`, also through `uniform/range` with such bounds) raises `(exn hansei)` of kind `malformed` with `abort`, as deviation 33, where OCaml's `int` type rules it out and the loop would never stop; an integral flonum such as `2.` stays a count (added in the final review, §15).
8. **`uniform_range`** (P:173-174): half-open `[low, high)`.
9. **`geometric`** (P:181-185): `probcc-geometric p s f` returns the trace; `probcc-geometric/count` is upstream's.
10. **`normalize`** (I:299-301): exact weights are normalized to flonums with one rounding (`op/divide`). `probcc-normalize` (list only) is not a deviation: it is PE:22-24's `normalize`; `probcc-normalize/total` is I's.
11. **Bucket** (DSL p.18 l.776-783; NIPS l.71-72): `λ-probcc-bucket` uses the global `λ-memo`, shared by every inference as EPP p.4 l.331-336 asks; it is wrong if `f` reads world-local state.
12. **`variable_elim`** (P:267-268): variadic.
13. **`letlazy_nesting`** (P:347-363): no debugging output.
14. **Sampler output** (I:131, 186): printed only under `probcc-verbose`.
15. **Memory** (P:40-64): values stored directly in a fixnum-keyed pmap (no closure/channel cell); one global pointer, single-threaded.
16. **`memo`/`mref` errors** (P:471): `(exn hansei memory)` instead of `Not_found`; the table is ordered by `pmap-compare`.
17. **wrefs** (TI:121-126): `WorldLocalMem` is not shipped; reconstructed; `probcc-wref-set!` takes the reference first.
18. **Selectors** (P:221-233; T:20): answer `(x ptotal)` (a list); a seed re-initialises the global OCaml-compatible state as upstream; an explicit `rng-source` may be given instead of a seed (variant).
19. **Ratios** (P:205, 240, I:47, 132, 160, 187): `probcc-ratio`, exact stays exact; a zero total gives NaN/inf as upstream (D8). P:205 is `geometric_bounded`'s `p /. norm` (added in WP4, §15).
20. **`probcc-map`** (new): required instead of `map` around stochastic code (§5.6).
21. **`statistics` order** (I:292): sorted, not hash order.
22. **Parsec** (PS:59-66, 106-113): names prefixed `parsec-`; `parsec-or` binary, nested left in tests; `alt` and `stream_over` (`parsec-stream-over`, whose alphabet OCaml passes as an array) take a list; the unit value is `(void)` (widened in WP7, §15).
23. **`prob_of`** (N:292-293): skips `C` entries instead of failing.
24. **Lazy lists** (SL:325-326): `'()` / `(head-thunk . tail)`; promoted from example code to a library module; `probcc-lobserve` compares heads with `equal?`, where M2:116's `=` has `-0.` equal to `0.` and NaN unequal to itself (added in WP6, §15).
25. **Tuples** (D18): lists.
26. **Exact `1`** in place of `1.0` for deterministic branches (P:182, 361; I:128): identical for flonum weights. Likewise the exact `1` of `expi x 0` (P:198) and the exact `0` that `probcc-prob-of` answers for a missing value (N:293, PE:50 `0.`) (added in WP4, §15). Likewise `probcc-explore/bounded`'s `low`/`high` start at exact `0`, so `low` is `0`, not `0.`, when no leaf is reached (§6.3).
27. **`statistics`** (I:289-292): the std is computed with `fpsqrt` on a flonum, so it is always a flonum and a negative rounded variance gives `+nan.0` as OCaml (generic `sqrt` would give a complex); the mean keeps exact weights exact.
28. **`timeit`** (I:307): prints the seconds with `~a` (full flonum) instead of `%g`.
29. **`map_dist`** (BL:456-460): promoted from example code to the core; a `C` entry raises `(exn hansei malformed)`.
30. **pmap errors** (PM:66, 76, 85, 141): `(exn pmap)` conditions, kind `not-found` for `pmap-find` (`Not_found`), kind `empty` for `pmap-find-min` (`Not_found`) and `pmap-delete-find-min`/`-max` (`invalid_arg`); `pmap-find` takes an optional default thunk (added in WP1, §15). Raised with `abort`, not continuable, as OCaml's `raise`: a handler that returns gets "exception handler returned" instead of handing its value to the tree code (WP1 review, §15).

31. **RNG errors** (OR:91-94): `ocaml-random-int!` outside `[1, 2^30 - 1]`, a seed that is not an exact integer (or a vector of them), and non-integer bounds of `rng-integer!` raise `(exn hansei)` of kind `random` with `abort`, instead of `Invalid_argument` or a type error (added in WP2, §15).
32. **Three-draw `rawfloat`** (OR:167-172): `ocaml-random-float!` draws three words, `((r0/2^30 + r1)/2^30 + r2)/2^30`, as the OCaml that made HANSEI's records did; OCaml 4.14 draws two (D6, D20; added in WP2, §15).
33. **`lazy_permutation_ins` of an out-of-domain count** (LP:281-286): any count that is not a non-negative exact integer (negative, or a non-integer such as 2.5 or 0.5) raises `(exn hansei)` of kind `malformed` with `abort`, where upstream's `function` fails to match a negative `int` and its type rules out a non-integer (added in WP6, §15; widened from negative counts in the WP6 review).

**Kept as upstream, with a comment.** One global RNG state, re-seeded by every `random_selector seed` (P:221-222); `letlazy` is not nesting-safe (N:75-81); `memo` must be created inside the reification that uses it (P:466-471); `bounded_explore` counts every `V` (I:73-75); `random_selector` raises "can't happen" when `r` falls past the last entry; `first_success` breadth-first via `append`.

**Upstream discrepancies, for the docs only.**
- EPP p.4 says `biased` can be memoized "as usual" and used at the inner level; with P's `memo` that raises `Not_found`. Only `letlazy_nesting` works (§5.2).
- M1:203-205 re-sums the exact `r`, not the sample.
- PD:122 has a syntax typo (`R]).`).
- TI depends on the unreleased `WorldLocalMem`.
- With OCaml ≥ 4.x's three-draw `Random.float` (and the xor `bits`), the sampling records of SM, N, SL, PS and M1 replay bit for bit *[OCaml 3-draw]*. Those of SO (5 failing assertions), IN (3), HM (2) and CB (3) do not, and BT, BN, BL were not checked; they come from earlier code or generators.

---

## 10. Registration, building and provenance files

### 10.1 `src/aux.egg`

Replace line 60, `(extension aux.hansei)`, with:

```
  (extension aux.hansei.pmap (component-dependencies aux.base))
  (extension aux.hansei.random (component-dependencies aux.base aux.hakaru10.random))
  (extension aux.hansei (component-dependencies aux.base aux.continuation.delimited aux.hansei.pmap aux.hansei.random aux.hakaru10.random))
  (extension aux.hansei.lazy (component-dependencies aux.base aux.hansei))
  (extension aux.hansei.parsec (component-dependencies aux.base aux.hansei))
```

(`aux.continuation` is reached transitively through `aux.continuation.delimited`, matching C3's import list) and move `(extension aux.hakaru10.random …)` above them; the egg resolves the dependencies anyway, but keep the file in build order. `aux.hakaru10.exact` already lists `aux.hansei`. No new egg dependencies: srfi-1 is listed and srfi-69 is no longer needed by hansei.

### 10.2 `src/Makefile`

- `test:` after line 31 (`cd test && ${CSI} -s hansei.scm`): `hansei-random.scm`, `hansei-lazy.scm`, `hansei-examples.scm`, `hansei-logic.scm`.
- New target, added to `.PHONY`:
  ```
  # (aux hansei): zebra, the 2^20-world sorted list, and the statistical sampling tests.
  test-hansei-long:
  	cd test && ${CSI} -s hansei-long.scm
  ```
- `format:` after line 144: `scheme-indent -T 2` lines for `test/hansei-random.scm`, `test/hansei-lazy.scm`, `test/hansei-examples.scm`, `test/hansei-logic.scm`, `test/hansei-long.scm`. After line 172: lines for `aux.hansei.pmap.scm`, `aux.hansei.random.scm`, `aux.hansei.lazy.scm`, `aux.hansei.parsec.scm`.

### 10.3 Provenance files

Copy the Python replay transcription (scratch `plan/py/ocamlrandom.py`, `plan/py/hansei_py.py`, `plan/py/hansei_check.py`, and `bounded_check.py` and `rint.py` from the scratchpad root; `critic-fid/py/n30.py` and `talarm.py` as the D6/D19 evidence) into `src/test/hansei-python/`, as `src/test/hakaru10-python/` exists. Make `hansei_check.py` take its module directory from `__file__` instead of `argv[1]`, and keep the `xorvariant`/`ndraw` switches with a README line saying that `xorvariant=True, ndraw=3` is the one that replays SM. Run with `python3 -I`. Do not commit `src/test/testsuite-*.html` or generated `.import.scm` files.

### 10.4 Build and run (chicken-install is blocked here; memory note "aux egg build blockers")

```
SP=/data/bitcoin/sandbox/claude-1005/-data-bitcoin-mnocentini-home-Developer-working-copies-aux-scm/6908d606-8c7d-4297-b297-00a844b6d400/scratchpad
R=$SP/<wp-repo>   # a fresh directory per work package
MODS="aux.base aux.sxml aux.unittest aux.category.monad aux.continuation aux.continuation.delimited \
      aux.hakaru10.random aux.hansei.pmap aux.hansei.random aux.hansei aux.hansei.lazy aux.hansei.parsec" \
R=$R bash $SP/tools/build-hansei.sh
cd /data/bitcoin/mnocentini-home/Developer/working-copies/aux.scm/src/test
CHICKEN_REPOSITORY_PATH=$R:/home/mnocentini/.local/lib/chicken/12 csi -s hansei.scm
```

- Drop modules from `MODS` that do not exist yet in the current WP. `build-hansei.sh` copies each module to a temp dir and compiles with `csc -s -J -O3 -d1`.
- **Hakaru10 compatibility check** after any change to `aux.hansei.scm`: append `aux.category.state aux.stream aux.hakaru10.distribution aux.hakaru10.util aux.hakaru10.engine aux.hakaru10.exact aux.hakaru10` to `MODS`, then run `csi -s hakaru10-exact.scm` and `csi -s hakaru10.scm`. Both must still report `(failed 0)`.
- Without the scratch script, the per-module command is the one in `AUX-HAKARU10-PLAN.md` §11.

---

## 11. Work packages

Sequential, one agent each. Every WP ends with its **validation**: build per §10.4 into a fresh `R`, run the listed suites, and require `(failed 0)` with the runtime under the stated target. One commit per WP if the user asks for commits (imperative subject, the session's trailers).

| WP | Content | Files touched | Validation |
|---|---|---|---|
| **WP0** | Scaffolding: the egg and Makefile entries (§10.1-10.2); empty module skeletons with `;;` headers for `aux.hansei.pmap.scm`, `aux.hansei.random.scm`, `aux.hansei.lazy.scm`, `aux.hansei.parsec.scm`; test files with a `doc` stub and the epilogue | `src/aux.egg`, `src/Makefile`, new src stubs, new test stubs | All modules compile; `hansei.scm` still 8/0; the new suites run with 0 tests |
| **WP1** | `(aux hansei pmap)` (§3.4, §6.1) and H1-H2 | `aux.hansei.pmap.scm`, `test/hansei.scm` (H1-H2 only) | `hansei.scm` 10/0 |
| **WP2** | `(aux hansei random)` (§5.4, with `ocaml-random-global` and `ocaml-random-init!`), R1-R5 only (no selector yet: test `rng-double!` and `ocaml-random-int!` directly) | `aux.hansei.random.scm`, `test/hansei-random.scm` | `hansei-random.scm` passes; R4 bit-exact |
| **WP3** | Core refactor: header, imports (C3), memory (§5.2, C2), explore (§5.5; C1, C4-C6), normalize (C7) **and `probcc-normalize/total`**, `probcc-ratio`, `probcc-signal`, op reads (C8), dfs/leaves (C9-C10), match forms (C11), test renames (C13), `probcc-map`, `probcc-verbose`; H3-H9, H27 | `aux.hansei.scm`, `test/hansei.scm` | `hansei.scm` passes (8 old + new); **hakaru10-exact and hakaru10 suites pass**; the hansei part of `test/travel.scm` still runs (the file, the user's and unchanged, stops at travel.scm:5 on CHICKEN 6's `set-pseudo-random-seed!`, and past it at the non-hansei `match/non-overlapping` assertion of travel.scm:143; check a scratch copy seeded with `(string->utf8 "561")`) |
| **WP4** | Core laziness: `letlazy`, `letlazy/nesting`, `memo`, wrefs, `observe`, `geometric/count`, `geometric/bounded`, `prob-of`, `at-least`, `map-dist`; H10-H15, H28, H34, H36 | `aux.hansei.scm`, `test/hansei.scm` | `hansei.scm` passes |
| **WP5** | Core inference: `nearly-one`, `explore/shallow`, `first-success`, `explore/bounded`, selectors, `sample-dist/rejection`, runner, `sample-dist`, `sample-rejection(/a)`, `sample-importance(/a)`, `statistics`, `timeit`, `soft-cut`, `reify/part`, `bind`; H16-H26, H29-H33, H35; R6-R9 in `hansei-random.scm` | `aux.hansei.scm`, `test/hansei.scm`, `test/hansei-random.scm` | `hansei.scm` < 3 s; `hansei-random.scm` (R6, R9 bit-exact) < 5 s; hakaru10 suites still pass |
| **WP6** | `(aux hansei lazy)` and L1-L17 | `aux.hansei.lazy.scm`, `test/hansei-lazy.scm` | `hansei-lazy.scm` passes, < 5 s (move SL:633's 2000-sample replay to the long suite if needed) |
| **WP7** | `(aux hansei parsec)` and G7-G13 | `aux.hansei.parsec.scm`, `test/hansei-logic.scm` | parsec cases pass |
| **WP8** | Examples E1-E16 | `test/hansei-examples.scm` | passes, < 10 s (move slow cases to the long suite) |
| **WP9** | Logic G1-G6 and the long suite (§8.6, including the M1 and PS replays) | `test/hansei-logic.scm`, `test/hansei-long.scm` | `hansei-logic.scm` < 5 s; `hansei-long.scm` passes (record its runtime in its doc) |
| **WP10** | Citation pass (every definition `; cf.` or `(new)`, every deviation tagged and numbered per §9), doc sections (§8.7) and the deviation register doc in `hansei.scm`, the Python provenance (§10.3), `make format` equivalent `scheme-indent -T 2` on every touched file | all hansei files, `src/test/hansei-python/` | Full run of the five suites, the long suite, the hakaru10 suites and the hansei part of `test/travel.scm` (as WP3) |

**Replay-failure triage** (WP5): see §8.2.

**Per-WP rules.** Do not touch `src/aux.hakaru10*.scm` or their tests. Do not commit `upstream/hansei/` additions. Write scratch files under the scratchpad.

---

## 12. Runtime budget (csi, compiled library)

| Suite | Target | Dominant cost |
|---|---|---|
| `hansei.scm` | < 3 s | H17 thresholds (84 nested reifications), H22 |
| `hansei-random.scm` | < 5 s | R6 10000-sample rejections, 5000-sample importance; R9 nested 100-sample runs |
| `hansei-lazy.scm` | < 5 s | L15 `num_list 4 3`, L5 |
| `hansei-examples.scm` | < 10 s | E8 HMM no-VE (0.7 s), E16 |
| `hansei-logic.scm` | < 5 s | G11 (0.67 s), G5 (0.32 s) |
| `hansei-long.scm` | < 120 s | statistics (BT 6.8 s, CB 5.8 s, HM 2 s, dcoin 0.8 s), the 30000-sample parser replays (6.2 s), zebra (2.7 s); 27 s in all (WP9) |

---

## 13. Residual risks and questions for the user

- **Q1 (user decision):** `probcc-uniform/range` is half-open, against upstream's inclusive `uniform_range`. The plan keeps it (D13). If the user prefers upstream semantics, change it to `(+ low (probcc-uniform (add1 (- high low))))`, update `test/uniform/range` (1..8 at 1/8) and `test/travel.scm:26,73` (drop the `add1`).
- **Q2 (user decision):** `probcc-uniform 1` makes a choice node (commit 0af8975). Upstream returns 0 directly; only depth-bounded explorations notice the difference.
- **Q3:** committing `upstream/hansei/` examples and papers (no licence stated; pMap is LGPL-2.1 and already committed), and whether to add the three linked okmij pages fetched to the scratchpad `linked/` (§0). Ask before committing.
- **Q4 (user decision):** the global RNG state (D6) mirrors upstream and is needed for the N replays; the explicit `rng-source` argument remains for users who want isolated streams. Confirm that a module-level mutable RNG state is acceptable in `(aux hansei random)`.
- **Q5 (user decision):** names settled by this revision: `probcc-uniform/either/uncons` (upstream `uniformly_uncons`, after the user's `probcc-uniform/either` for `uniformly`), the `probcc-mem-…` stem (`probcc-mem-empty`, not `probcc-memory/empty`), `probcc-map-dist`, `probcc-lobserve`. Rename before WP3 if the user prefers others.
- **Q6 (open, user decision, WP10 review):** the `scheme-indent -T 2` pass of WP10 was not applied (§15 WP10: the tool here rewrites 19-469 lines per hansei file and misaligns vector literals, import specs and export lists). Either run `make format` with the user's own `scheme-indent` on the hansei files, or drop that part of WP10.
- **Risk: `pmap-compare` on model values.** A model returning a procedure, hash table or port now raises in `probcc-explore`, where the srfi-69 table accepted it. Upstream's `compare` raises too. Check the hakaru10 suites in WP3; if a value type there is not covered, extend the order rather than loosen it.
- **Risk: performance.** The pmap is slower than a hash table for very large supports. Measure the explore of `flips_xor 14` (16384 leaves, 34 ms before) and E8 in WP3. If the regression exceeds 2×, specialise `pmap-compare` for the common case of booleans, fixnums and symbols at the head of the cond.
- **Risk: replay drift.** If an R6 value drifts by one ulp, check operand grouping (§6) before anything else. Never loosen a count-based replay (0.48, 0.0514).
- **chicken-core item (optional, separate):** the csi re-entry fault with ≥5 operands (§5.6 2), reproduced by `core/e5.scm` with plain `call/cc`. It belongs to chicken-core's interpreter (`eval.scm`, argument evaluation for more than 4 operands). Report or fix it outside this plan; aux must not depend on the fix.
- **Memory notes worth adding** (by the user or the next session): the CHICKEN `map` re-entrancy hazard and the csi ≥5-operand fault (§5.6).

---

## 14. Scratch evidence used by this plan

All under the scratchpad `…/6908d606-…/scratchpad/`. They are ephemeral and not dependencies.

- `plan/v1.scm`: the ✓ checks of C1, the map hazard and the csi operand fault, against the current build.
- `plan/py/`: the Python replay oracle (`ocamlrandom.py`, `hansei_py.py`, `hansei_check.py`), re-run ✓ (all SM values in §8.2 R6). `bounded_check.py` (H22, X:72-77) and `rint.py` (R5's `Random.int` values) are at the **scratchpad root**, not in `plan/py/`.
- `critic-fid/`: `p1.scm`, `p2.scm`, `avl.py`, `py/h24.py`, `py/n30.py` (global vs fresh RNG, D6), `py/talarm.py` (`let … and` order, D19); run as `python3 -I critic-fid/py/n30.py <scratchpad>/plan/py`.
- `crit-tests/`: `a.scm`, `t2.scm`, `op.scm` (D12 variants), `avl.py`, `r.ml`, the three-draw upstream build `hb3/` and the `*.3draw.txt` / `*.2draw.txt` oracle runs (D20).
- `rev-chk.scm`: this revision's csi checks (`sqrt` of a negative, double rounding of `op/divide`, `(equal? 0 0.)`, `list-tabulate` order, quasiquote hazard, uniform 3 order today).
- `linked/`: the fetched linked pages (§0).
- `core/e1-e6.scm`, `core/proto.scm`: the core reader's probes and memory prototype (slazy test2m/test3m, nested letlazy).
- `logic-proto/*.scm`: the logic reader's prototype (logic_vars, daily_schedule, lazy permutations, zebra, parsec, type inference, bridge).
- `examples-probe/*.scm`: the examples reader's probes (HMM, noisy-or, bloodtype, blip_naive, music1a).
- `oracle-out/*.annot.clean.txt`: upstream HANSEI's outputs under OCaml 4.14.2 + delimcc 2023.06.17 (`tools/run-ocaml-hansei.sh`).

---

## 15. Review log (revision 2)

Each defect was verified first (✓ = reproduced in this revision: csi against `hrepo`, the Python oracle, or a read of the cited lines). "Fixed" names the sections changed.

**Fidelity review.**

| # | Verdict | Change |
|---|---|---|
| F1 importance draw rule | ✓ real (I:168-174 draws on any non-empty `cch`) | §5.4 draw discipline |
| F2 fresh source per selector | ✓ real (`n30.py`: global `{0.6: 5, biased: 5}`, fresh `{0.6: 6, biased: 4}`) | D6, §5.4 selector code, §7, deviation 18, R7, R9, Q4 |
| F3 H22 exact `0` | ✓ real (`(equal? 0 0.)` → `#f`) | §6.3, H22, deviation 26 |
| F4 complex std | ✓ real (`(sqrt -1e-20)` → `0.0+1e-10i`, `fpsqrt` → `+nan.0`) | §3.3, §6.6, H26, deviation 27, §8.6 |
| F5 `op/divide` double rounding; `p +. acc` | ✓ real (both) | C7. The suggested fix `(if (and (exact? n) (zero? n)) …)` was **not** taken as is: with `m` exact `0` and `n` = `0.`, `(exact->inexact (/ 0 0.))` is `0.0` in CHICKEN (`(/ 0 0.)` → exact `0`), not NaN; C7 uses `(if (zero? n) (/ (exact->inexact m) (exact->inexact n)) (exact->inexact (/ m n)))` |
| F6 `PI:` lines | ✓ real | §3.2 |
| F7 `let … and` order | ✓ real (`talarm.py`: 0.14/0.86 vs 0.1/0.9) | D19, §8 conventions, §8.2 |
| F8 argmax tie order | ✓ real (IN:212) | §8 conventions ("set"), E14 |
| F9 `map_dist`, `lobserve`, M2 | ✓ real | §2, §3.6, §3.7, §3.12, §7, H36, L16, §8.6, deviation 29 |
| F10 imports | ✓ real | §2 |
| F11 N:162 vs N:148-150 | ✓ real | R6 table |
| F12 H22 model | ✓ real (SM:469-480 is the unit model) | H22 |
| F13 `timeit` `%g` | ✓ real | §3.3, deviation 28 (formatting `%g` in Scheme would need a hand-written formatter; not worth it for a timing line) |
| F14 normalize is PE:22-24 | ✓ real | D15, §3.3, §3.12, deviation 10 |
| F15 `enum`/`of_enum` commented out | ✓ real (PMI:112-119, PM:201-233) | §2, §3.4, §3.12 |
| F16 zero totals raise | ✓ real | D8 (`probcc-ratio`), §3.2 row 14, §6.5, deviation 19 |
| F17 names | ✓ real | §3.11, §5.4, §7 |
| F18 linked material | ✓ real | §0 "Linked material" (three pages fetched and consulted) |

**Idiom review.**

| # | Verdict | Change |
|---|---|---|
| I1 H9 order | ✓ real (same as O1) | H9 |
| I2 H8 needs `normalize/total` in WP3 | ✓ real | WP3 now adds it |
| I3 R7/R8 need the selector | ✓ real | WP2 runs R1-R5; R6-R9 in WP5 |
| I4 `op/divide` | ✓ real (same as F5) | C7 |
| I5 `list-tabulate`/`filter-map` right to left | ✓ real (`rev-chk.scm`: element 1 chosen first) | §5.6, §8 conventions |
| I6 `<*>` rationale | ✓ real | §3.9 |
| I7 pmap exports internals | ✓ real | §2 (explicit export list) |
| I8 inconsistent conditions | ✓ real | §6.1, §7, §5.4 (`probcc-signal`), H2, R8; `uniform` kind dropped |
| I9 AVL height bound | ✓ real (tolerance 2 gives ≈ 1.81·log2 n) | H1 |
| I10 `pmap-find` default thunk | ✓ real risk | §3.4, §3.2 `Memory.mref` row |
| I11 parameters and memory | ✓ real (no test; force-site semantics) | C8, H34 |
| I12 epilogue comment | ✓ real | §8 conventions |
| I13 `uniformly/uncons`, `memory/empty` | ✓ real | renamed throughout to `probcc-uniform/either/uncons`, `probcc-mem-empty`; Q5 |
| I14 egg deps | ✓ real | §10.1 |
| I15 provenance path | ✓ real (`bounded_check.py` is at the scratchpad root) | §10.3, §14 |

**Oracles review.**

| # | Verdict | Change |
|---|---|---|
| O1 H9 | ✓ real | H9 |
| O2 H22 | ✓ real | H22, §6.3 |
| O3 `op/divide` | ✓ real (same as F5); the proposed fix is the one adopted | C7 |
| O4 L12 exact `1/24` after normalize | ✓ real | L12 |
| O5 N, SL, PS, M1 replays | ✓ real (`crit-tests/*.3draw.txt`: 0 exceptions) | D20, §9, R9, L7, L17, G13, §8.6 (M1 is now a replay, not a statistical test) |
| O6 N:162 | ✓ real (same as F11) | R6 table |
| O7 formv tie order | ✓ real | E13, E14, §8 conventions |
| O8 quoted upstream order | ✓ real | §8 conventions, H11 |
| O9 H5 `eq?` | ✓ real (the root's `C` entries are always forced) | H5 (literal tree, `maxdepth` 0) |
| O10 operand hazard in templates and `vector` | ✓ real (`rev-chk.scm` reproduced the quasiquote case) | D12, §5.6, §8 conventions |
| O11 WP2 R7/R8 | ✓ real (same as I3) | §11 |
| O12 coverage | ✓ real | H1b, H35, R5 (exact `Random.int` values from the oracle), L16 |
| O13 H33 ill-typed | ✓ real | H33 |
| O14 15-digit printing | ✓ real (`flonum-print-precision` is 15) | §8 conventions |

**Rejected.** None of the 47 items was rejected outright. Partially rejected: the literal fix text of F5 (see above), and F13's first option (formatting like `%g`), replaced by a registered deviation.

**2026-10-16, WP1 (`(aux hansei pmap)`, H1, H1b, H2).**

- **Validation count.** §11 says `hansei.scm` 10/0 after WP1, but revision 2 added H1b (`test/pmap/api`, O12) to the H1-H2 range, so WP1 ships three cases: the suite is **11/0** (8 old + H1, H1b, H2).
- **Imports (§2).** The pmap row lacks three imports the §6.1 order needs: `record-instance?` (to recognise records; `(only (chicken memory representation) record-instance? record->vector)`), `(only (chicken bytevector) bytevector? bytevector-length bytevector-u8-ref)` for bytevectors, and `(only (chicken format) fprintf)` for the record printer `#<pmap ((k . v) …)>`, as `(aux hakaru10 random)` prints its records. `(declare (local))` is used as in `(aux hakaru10 random)`.
- **Deviation 30 added to §9.** The pmap errors (conditions instead of `Not_found`/`invalid_arg`, and the optional default thunk of `pmap-find`) were in §3.4 and §7 but had no register number, which the citation rule needs. `pmap-find-min` raises kind `empty` (not `not-found`), like the two `delete-find` procedures: §7 says `empty` is the kind for an empty map.
- **`fx-compare`.** §3.4 suggests `(fx- a b)`; it is exported and used by `pmap-compare`'s fixnum fast path for arbitrary fixnums, where `fx-` overflows (e.g. `most-negative-fixnum` against 1). It is a three-way `fx<`/`fx>` comparison instead.
- **`pmap-compare` details beyond §6.1.** An `eq?` shortcut answers 0 first, as OCaml's `compare` does on physically equal values (`compare f f` is 0), so a procedure compares equal to itself and raises only against another value. Keywords are not `symbol?` in CHICKEN 6, so they rank separately as §6.1 says. Hash tables and promises are `record-instance?`, so they are ordered by their slots and raise only when the walk reaches a procedure slot; §6.1's "hash tables raise" holds only in that case. Non-real numbers compare by real part, then by imaginary part, with the real order (a real's imaginary part is exact 0).
- **`pmap-map` order.** §3.4 says "left subtree first, then right (PM:173-174)"; upstream also applies `f` to the node's value **after** both subtrees (post-order), and the port keeps that order.
- **Checks beyond the suite** (scratch `wp1/fuzz.scm`): 20000 random `pmap-add`/`pmap-remove` operations against a sorted alist (persistence, membership), all equal; 200000 `pmap-insert-with` calls on list keys take 274 ms under csi with the compiled module.
- **WP1 review fixes (2026-10-16).** (a) `pmap-signal` raised with `signal`, which is continuable: a handler that returned made `pmap-find` and the `delete-find` procedures answer its value, and made `pmap-compare` hand a non-fixnum to the `fx=`/`fx<` of the tree code (two procedures were inserted as keys). It now raises with `abort` (deviation 30), so it never returns, as OCaml's `raise`; `test/pmap/api` checks that a returning handler gets an error. **WP3's `probcc-signal` follows the same rule** (§2, §7). `(aux hakaru10 random)` keeps `signal` (out of scope). (b) The `pmap-remove` comment said "m itself when x is unbound"; like PM:144-151 it answers a fresh record with the same bindings. (c) Two new test lines over 100 columns were wrapped, and the expected alist of H1 is built with `iota`.
- **`test/travel.scm` already fails on CHICKEN 6**, before any hansei code: `(set-pseudo-random-seed! "561")` at travel.scm:5 raises "bad argument type - not a bytevector". The file is the user's and is unchanged in git; WP3's and WP10's "`test/travel.scm` still runs" check needs the user to fix that call first (e.g. `(set-pseudo-random-seed! (string->utf8 "561"))`), or runs the check after it.

**2026-10-16, WP2 (`(aux hansei random)`, R1-R5).**

- **Oracles re-checked against OCaml.** Every R1-R5 value of §8.2 was recomputed with OCaml 4.14.2 (scratch `wp2/r2.ml`, `r3.ml`, `r4.ml`, run with `tools/run-ocaml-hansei.sh -m script`; the states read through `Obj.magic`): R1's digests, R2's `st[0..4]`, R3's bits, R4's three-draw doubles and R5's `Random.int` values all agree with the plan. R4's literals are written with 17 digits (`0.95916053666250445`, not `0.9591605366625044`; the same doubles).
- **Trap for later replays: OCaml evaluates function arguments right to left.** `Printf.printf "%d %d %d" (Random.bits ()) (Random.bits ()) (Random.bits ())` prints the third draw first. A first version of the test that quoted such a line expected the draws reversed; the suite now quotes them in draw order. The same holds for any oracle line printing several draws (D19).
- **Citation keys.** §0 gains `OR` (OCaml 4.14.2 `stdlib/random.ml`, sha256 recorded) and `OD` (`digest.ml`); the module cites `OR:40-58` (`full_init`), `OR:76-83` (`bits`), `OR:86-94` (`int`), `OR:167-175` (`rawfloat`, `float`), `OR:197-213` (default state), RFC 1321 §3 for MD5.
- **Deviations 31 and 32 added to §9.** §5.4 does not say what `ocaml-random-int!` does on a bad bound, nor what a bad seed raises: they raise `(exn hansei)` kind `random` with `abort` (as deviation 30), registered as 31; §7 lists the kind. The three-draw `rawfloat` was a decision (D6) without a register number, which the citation rule needs: 32.
- **Beyond §5.4, all small and checked against OCaml.** (a) `make-ocaml-random-state` takes its seed as `#!optional`: without one it is OCaml's **default state** (OR:197-213, "the state you get with `init 27182818`"; the `land` filter it mentions is already in `full_init`), and `ocaml-random-global` starts as that state, so an un-seeded global draws `89809344 994326685 …` like OCaml's `Random.bits ()` (§5.4 said the initial seed is irrelevant; it costs nothing to match). (b) A seed may also be a vector of exact integers, which is `Random.full_init` (`#()` is `[| 0 |]`, OR:46); `#(1 2 3)` is checked. (c) The `draw-integer` of `make-ocaml-random-source` takes its bounds in either order, as the other sources of `(aux hakaru10 random)` do, and is limited to ranges of `2^30 - 1` integers (`Random.int`'s bound). (d) The module has an **explicit export list** (the §7 names), as `(aux hansei pmap)`, so the MD5 tables and word macros do not leak; the tests read a state's words through `record->vector`, as H1 reads the AVL tree.
- **Imports (§2).** The random row also needs `(only (scheme base) exact-integer? string->utf8 bytevector-append vector-copy vector-map)`, `(only (chicken bytevector) …)`, `(only (chicken condition) make-property-condition make-composite-condition abort)` and `(only (chicken format) fprintf)` (record printer); of `(aux hakaru10 random)` only `make-rng-source` is used (the tests import `rng-double! rng-integer! rng-source-copy`).
- **Word size.** As `(aux hakaru10 random)`, the word arithmetic is `fx` under `cond-expand` `64bit` and generic otherwise; the generic branch was checked by compiling the module with `csc -no-feature 64bit`: the suite passes (5/0) with it too.
- **Tests.** Five cases (`test/md5`, `test/ocaml/init`, `test/ocaml/bits`, `test/ocaml/float`, `test/ocaml/source`) cover R1-R5, plus the MD5 padding boundaries (55/56, 63/64, 119/120 bytes), seed 123456789, the default state, `full_init`, the 998th-1000th draws, error kinds, and that a source over `ocaml-random-global` shares (not copies) it. Validation: `hansei-random.scm` `((ran 5) (failed 0))` in 0.13 s; `hansei.scm` still 11/0; the three stub suites 0/0. Speed (csi, compiled module): 10^6 `ocaml-random-float!` in 0.46 s, 100 seedings in 27 ms.

**2026-10-16, WP3 (core refactor, H3-H9, H27).**

- **Export list.** `(aux hansei)` had `*`; §3.11 wants `probcc-ratio` internal, and `probcc-signal` and the location counter `probcc-mem-counter` must not leak either, so the module now has an **explicit export list** (the §7 names that exist after WP3), as `(aux hansei pmap)` and `(aux hansei random)`. **WP4 and WP5 must add their names to it.** `probcc-thread-local` is exported for reading only **by convention**: CHICKEN aliases the exported global, so an importer *can* `set!` it and the core then sees the new value (probe: a top-level `(set! probcc-thread-local 'clobbered)` is read back by `probcc-mem-prompt` and survives a `probcc-reify/exact`). Only the core's own `letlazy`, `memo` and wrefs may assign it; WP4 and WP5 must not rely on any encapsulation.
- **`probcc-signal` takes a location**, `(probcc-signal kind loc msg . args)`, like `pmap-signal` and `ocaml-random-signal`, so §5.4's call is `(probcc-signal 'selector 'probcc-random-selector "Choice selection: can't happen" choices)` in WP5. `probcc-mem-ref` raises kind `memory` with location `probcc-mem-ref`.
- **Imports (§2).** Only what WP3 uses: `(only (scheme base) make-parameter)`, `(chicken base)`, `(only (chicken sort) sort)`, `(only (chicken condition) make-property-condition make-composite-condition abort)`, `(only srfi-1 fold)`, `(aux base)`, `(aux continuation delimited)`, `(aux hansei pmap)`. WP5 adds `(aux hansei random)`, `(only (aux hakaru10 random) rng-source? rng-double!)`, `cpu-time`, `printf` and `fpsqrt` with the code that uses them. The removed imports are those of C3 (`(chicken continuation)`, `(chicken pretty-print)`, `(chicken fixnum)`, `(aux continuation)`, `srfi-69`).
- **`probcc-ratio`** is defined (deviation 19) but has no caller before WP5 (`dist_selector`, the samplers, `shallow_explore`).
- **`probcc-normalize`** is `(cadr (probcc-normalize/total choices))`: PE:22-24 and I:299-301 are the same fold and division.
- **`probcc-dfs`** reads `op/times` once for the whole traversal (an inner `letrec`), not once per recursive call (C8).
- **Citations corrected** against `dsl-paper.txt`: the lawn tree and its depth-1 exploration are DSL **p.12** l.539-546 (not p.13 l.540-546), `explore (Some 4)` is p.15 l.653-656, `gm_tree` p.16 l.712-719 (not p.17 l.714-719); the five trees of H6 straddle the page break, p.8-9 l.364-374; `explore` is p.9 l.393-408; `flip` p.4 l.161; `pv_unit`'s `1.0` is P:100.
- **Tests.** H3-H9 and H27 as §8.1 (names `test/explore/persistent`, `…/order`, `…/depth`, `…/trees`, `test/reflect/inverse`, `test/normalize`, `test/uniform/edges`, `test/map/reentrant`), plus: H5 checks that the depth-1 entries are all `C`; H8 checks that exact masses are rounded once (`1/10, 2/10` → `(3/10 (((V a) ,(exact->inexact 1/3)) …))`, deviation 10); H9 also checks `(probcc-reify/exact (probcc-uniform 1))` = `(((V 0) 1))` and that `(probcc-uniform/either '())` fails. The lawn model is a top-level `define` shared by H5 and H7, and the file carries the D11/D12/D19 comment (§8 conventions). C13 renames done, trailing blank lines dropped. Against the pre-WP3 core the new cases fail as expected (H3 and H6 on `#f` depth, H4 and H9 on hash order, the rest on missing names).
- **Validation.** Fresh `R` (`wp3/repo`) with the §10.4 modules plus the hakaru10 ones: `hansei.scm` `((ran 19) (failed 0))` in 0.19 s (8 old + H1, H1b, H2 + 8 new); `hansei-random.scm` 5/0 in 0.13 s; the three stubs 0/0; `hakaru10-exact.scm` 11/0 and 8/0 in 4.49 s; `hakaru10.scm` 13/0, 16/0, 7/0, 20/0, 9/0 in 7.28 s (4.48 s and 7.24 s before WP3). Performance (§13 risk): `flips_xor 14` exact, 70 ms (69 ms before); `probcc-uniform 20000` exact, 58 ms (60 ms).
- **`test/travel.scm`.** Unchanged in git, it still stops at travel.scm:5 (the WP1 note). A scratch copy with `(set-pseudo-random-seed! (string->utf8 "561"))` prints the same distribution under the old and the new core (only the order of tied entries differs, now deterministic, D5), then stops at travel.scm:143 under **both**: the top-level `(match/non-overlapping '(0 a b d) ((0 a b c) #t) (,,pair? #f))` has no matching clause (`,,pair?` compares with `eqv?`). Both are the user's to fix; no hansei code is involved.

**2026-10-16, WP4 (core laziness, H10-H15, H28, H34, H36).**

- **Definitions.** `probcc-observe` (P:213-214), `probcc-geometric/count` (P:181-185), `probcc-geometric/bounded` (P:187-206), `probcc-letlazy` (P:321-330), `probcc-letlazy/nesting` (P:342-370), `probcc-memo` (P:449-479), `probcc-wref-new`/`-get`/`-set!` (TI:121-126), `probcc-prob-of` (N:292-293), `probcc-at-least` (N:304), `probcc-map-dist` (BL:456-460), all added to the explicit export list and the module header. §5.3's code is used as written; `probcc-memo`'s missing table raises `(exn hansei memory)` with location `probcc-memo` (message "memo table not in this world's memory"), `probcc-wref-get` with location `probcc-mem-ref`. `probcc-at-least` compares with raw `>=` (upstream `>=`; `op/greater` is the strict order of the explorer's sort, so it is not used). `probcc-map-dist` folds with srfi-1 `fold` and raises kind `malformed` on a `C` entry, location `probcc-map-dist`.
- **`geometric_bounded` (§3.2 row 9).** Cited P:187-206 (the comment starts at 187). Row 9 did not say what `expi x 0` and `p /. norm` become under D8: the exact `1` (deviation 26) and `probcc-ratio` (deviation 19), so `(probcc-geometric/bounded 3 1/2)` is exactly `8/15 4/15 2/15 1/15` (checked in H11). Products and the subtraction read `op/times`/`op/subtract`. §9 deviations 19 and 26 now name P:205, P:198 and `prob_of`'s `0.` (N:293).
- **Trap for every later bit-exact oracle: CHICKEN's reader misrounds some decimal literals.** `1.13906250000000092e-05` (X:65) and its shortest form `1.139062500000001e-05` both read as `0x1.7e34b945308c2p-17`, one ulp above the double OCaml (and Python, and the computed mass) has, `…8c1p-17`; the 17-digit `1.1390625000000009e-05` reads correctly. The 21 other literals of H10-H14 read correctly. H10 writes that mass with 17 digits and a comment. Check a doubtful literal with `(= (inexact->exact LIT) N/D)` against Python's `Fraction(float('LIT'))` (scratch `wp4/p3.scm`).
- **`(flonum-print-precision 17)`** is now set at the top of `test/hansei.scm` (§8 conventions ask it for the bit-exact suites, and H10, H11, H12 and H14 are bit-exact); the old `⊦=` cases are unaffected.
- **Kinds stronger than §8.1.** H12 (`0.321999999999999897`/`0.2838`, X:55) and H14's `grass_model_memo` (`0.458100000000000063`/`0.189000000000000029`, PE:43) are asserted bit for bit (`equal?`), not ≈. H10's `C` masses are compared as a sorted list; H13-H15 are written in the D5 order with `equal?` (no "set" helper is needed: the order is deterministic and equals upstream's listing for the tied tables).
- **H15 extended.** Besides `testn11`, `testn21`, `testn21'`, `testnl10`-`testnl13`, a two-level case (an innermost reification asking its parent, which asks the outer world) is checked against upstream run under OCaml (scratch `wp4/n2.ml`, with a second OCaml-checked probe, a `memo` forced through `letlazy_nesting` from an inner reification, in `wp4/n2.scm`). `testn11` chooses among thunks with `probcc-distribution` and reifies the chosen one, as N:15-17.
- **One extra case, `test/prob-of`** (not in §8.1): `probcc-prob-of` on a table with a `C` entry (skipped, deviation 23) and a missing value (exact `0`), `probcc-at-least` both ways, and PE:62-66's exact `at_least 0.3 true (exact_reify coin)` → `(((V #t) 1))` (the exact half of H16; the `sample_rejection` halves stay in WP5).
- **H34** (`test/parameterize/memory`): a `probcc-letlazy` variable forced inside a `parameterize` in the `c = #t` world and outside in the other, then forced again outside; a nested `probcc-reify/exact` inside a `parameterize` reads the binding; a `probcc-memo` called inside a `parameterize` answers the same value outside; the parameter is back to `outer` afterwards and the epilogue's port check passes.
- **Validation.** Fresh `R` (`wp4/final`) with the §10.4 modules plus the hakaru10 ones: `hansei.scm` `((ran 29) (failed 0))` in 0.28 s (19 after WP3 + H10-H15, H28, H34, H36 + `test/prob-of`); `hansei-random.scm` 5/0 in 0.13 s; the three stubs 0/0 (0.07-0.08 s); `hakaru10-exact.scm` 11/0 and 8/0 in 4.50 s; `hakaru10.scm` 13/0, 16/0, 7/0, 20/0, 9/0 in 7.25 s (unchanged from WP3).

**2026-10-16, WP5 (core inference, H16-H26, H29-H33, H35, R6-R9).**

- **Definitions.** `probcc-random-selector` (P:221-233), `probcc-dist-selector` (P:238-241), `probcc-nearly-one` (I:30), `probcc-explore/shallow` (I:35-53), `probcc-first-success` (I:55-64), `probcc-explore/bounded` (I:71-109), `probcc-sample-dist/rejection` (I:121-135), `probcc-sample-runner/times` (I:142-146, P:259-261; curried, `((probcc-sample-runner/times n) z step)`), `probcc-sample-dist` (I:137-198), `probcc-sample-rejection/a` and the macro `probcc-sample-rejection` (P:253-254), `probcc-sample-importance/a` and the macro `probcc-sample-importance` (P:256-262), `probcc-statistics` (I:279-294), `probcc-timeit` (I:303-308), `probcc-bind` (DSL p.10 l.415-419), `probcc-reify/part` (LV:68-70), `probcc-soft-cut` (LV:386-391), all exported and listed in the module header. §5.4's and §6's code is used as written. Internal, not exported: `probcc-random-global-source` (§5.4), `probcc-pmap->pv` (the `PMap.foldi` consing of I:28, 53, 132, 187, 191, with a mass map for the division by the number of samples), `probcc-sort/mass` (D5's stable sort, now also used by `probcc-explore`, whose output is unchanged), `probcc-total` (the `ptotal` left fold of I:43, 158, P:230, 239) and `probcc-scale` (the `p /. ptotal` map of I:47, 161, P:240, through `probcc-ratio`). Imports added (§2): `cpu-time`, `printf`, `fpsqrt`, `(only (aux hansei random) ocaml-random-global ocaml-random-init! make-ocaml-random-source)`, `(only (aux hakaru10 random) rng-source? rng-double!)`.
- **`probcc-statistics`** reads `op/plus`, `op/times` and `op/subtract` once (C8) for `pold +. p`, `p2old +. p *. p` and the variance; §6.6 wrote raw operators. The divisions by `n` (a count) stay raw `/`. Every sample run by `probcc-sample-dist/rejection` and `probcc-sample-dist` uses `probcc-ratio` for `p /. ns` (deviation 19), so exact models give exact tables (H16-H18).
- **Citations corrected.** `pv_bind` is DSL p.10 **l.415-419** (`let rec pv_bind` is l.415), not l.417-421 (§3.10 fixed); the monad-law remark is l.420-425. `dist_selector` is P:238-241 (the comment starts at 235), the selector section P:218-241, `random_selector`'s `ptotal` fold P:230.
- **Oracles.** Every R6 value of §8.2 replays bit for bit with the first implementation, as do all the N records of R9 (recomputed with 17 digits under the three-draw OCaml build `crit-tests/hb3`, scratch `wp5/n.ml`, `HLIB=…/crit-tests/hb3 tools/run-ocaml-hansei.sh -m script`). H26's expected std is `0.081649658092772609` = `sqrt (1/150)`: §8.1's 15-digit `0.0816496580927726` is another double (fixed in the H26 row). **Reader trap again:** CHICKEN reads N:255/N:272's `4.9999999999999885e-11` (17 significant digits, OCaml's shortest form) one ulp low; R9 writes the 18-digit `4.99999999999998855e-11`, with a comment. Every flonum literal of `hansei.scm` and `hansei-random.scm` was checked against Python's `Fraction(float(lit))` (scratch `wp5/lits-check.scm`); only the two WP4 comment literals of X:65 misread.
- **Order of the nested models.** N:114-120 and N:183-190 are `let v = flip 0.5 in (v, coin_choice (), coin_choice ())`: `v` first (a `let`), then the tuple from the right (D19). A first R9 draft that drew the tuple's three components from the right failed on N:211, as D19 predicts.
- **Tests beyond §8.1-8.2.** H18 and H32 are asserted with `equal?` in the D5 order (deterministic), not "set". H19 adds a breadth-first case (`(((V #t) 1/6) ((V #f) 1/2))`); H21 also checks the unsorted shallow layout and the normalized thunk of a branch; H24 adds `tfooibl3` (SM:106-108) and `tmusictr42` (SM:200-202, one sample), also solved without a draw; R6 adds `tmusictr41` (SM:170-172, `[(0.8, f ()); (0.2, g ())]` built from the right, `g ()` first); H29 adds the `sample_importance: done` line; H25 compares the utility with `=` (`-0.`) and the form with `equal?`. R7 (`test/selector`) checks the shared re-seeded global stream, an explicit OCaml source (and that the global state is not moved by it), stdgen and CHICKEN sources (`make-chicken-random-source` takes a bytevector seed, `(string->utf8 "seed")`), `((a 0.) (b 1.))`, and an exact `(3 7/2)` answer; R8 also checks the kind `selector`. A mutation check (dropping I:195's `List.rev`) makes R6 and R9 fail, so the replays do test the draw discipline.
- **Validation.** Fresh `R` (`wp5/final`) with the §10.4 modules plus the hakaru10 ones: `hansei.scm` `((ran 46) (failed 0))` in 0.46 s (29 after WP4 + 17: H16-H26, H29-H33, H35); `hansei-random.scm` `((ran 9) (failed 0))` in 0.42 s (R1-R5 + `test/replay/sampling` R6, `test/selector` R7, `test/selector/raises` R8, `test/replay/nested` R9); the three stubs 0/0 (0.07-0.09 s); `hakaru10-exact.scm` 11/0 and 8/0 in 4.49 s; `hakaru10.scm` 13/0, 16/0, 7/0, 20/0, 9/0 in 7.51 s.

**2026-10-16, WP6 (`(aux hansei lazy)`, L1-L17).**

- **Definitions.** `probcc-lnil` (SL:329), `probcc-lcons` (SL:330), `probcc-lappend` (SL:332-336), `probcc-lhead` (SL:338-339), `probcc-ltail` (SL:342-343), `probcc-lreflect` (SL:346), `probcc-llength/at-most` (SL:350-355), `probcc-list->llist` (M1:71-73), `probcc-llength` (M1:78-83), `probcc-lmap` (M1:98-101 = M2:106-109; §2 said M1:51-84, which stops before `lmap`), `probcc-lobserve` (M2:113-119), `probcc-split-at` (LP:139-146), `probcc-uniform/either/uncons` (LP:158-166), `probcc-lazy-permutation` (LP:175-188), `probcc-insert-shift` (LP:242-249), `probcc-lazy-permutation/ins` (LP:272-287), as §3.5-3.6 wrote them. The module has an **explicit export list** (the §7 names), as the other hansei modules; the WP0 stub had `*`.
- **Deviation 33 added to §9.** §3.5 says a negative `n` of `probcc-lazy-permutation/ins` raises but not what: `(exn hansei)` kind `malformed` (the core's kind for an upstream match failure), with `abort` as `probcc-signal`, which is internal to the core, so the module builds the condition itself and imports `(chicken condition)` (§2 row updated). Deviation 24 now also records that `probcc-lobserve` compares with `equal?` (§3.6 said so without a number).
- **Plan gap: replays of models that draw with `uniform`.** L7 (and L17's SL:530 and SL:617-618) expect upstream's flonums, but `probcc-uniform`'s masses are exact (deviation 7, D13), so `full_10ltree_obs 4` gives the exact `1/10000` and no choice of `op/*` parameters changes that. The suite draws the digits of those replays with `ml-uniform`, a test-local transcription of P:156-165 (`1. /. n`, value 0 at `1. -. pacc`, `n = 1` answers 0), which replays `0.000100000000000000032` and `0.12` bit for bit; the same sample with `probcc-uniform` is also checked, `(((V 1) 1/10000))`. The other SL models take the mass of their fair choices as an argument: the exact `1/2` for the exact tables, `0.5` for the replays (L7 row of §8.3 updated). Every later replay of a model that calls `uniform` (SO's `model n` in the long suite, M1's notes) needs the same.
- **Kinds stronger than §8.3.** L4 (PCFG) is asserted bit for bit (`equal?` on upstream's `0.399999999999999967`, `0.0720000000000000084`, …), not ≈; L12's normalized masses are `equal?` to `(exact->inexact 1/24)` for all 24 entries; the "set" cases (L2, L5, L9, L11, L13, L14) are `equal?` in the D5 order, which for these values (integers, booleans, strings and lists of them) is upstream's own listing order. The `expensive_f` call counts, secondary oracles of L1, are also checked for L2 (testl1 .. testl54: 4 4 4 8 4 4 4 8), L6 (`testd1`: 4) and the L17 replays (10 4 10 4 10 8), all equal to the lines upstream prints under OCaml (`crit-tests/slazy.3draw.txt`).
- **Tests.** 15 cases: `test/llist/kit` (L16, plus the sharing of a stochastic `lmap` and of `lappend`'s cells), `test/lazy/sharing` (L1), `test/lazy/variables` (L2), `test/llist/at-most` (L3), `test/llist/pcfg` (L4), `test/memo/functions` (L5), `test/lazy/delayed` (L6), `test/split-at` (L8, plus the empty list), `test/uncons` (L9, plus no choice for one element), `test/insert-shift` (L10), `test/correlated` (L11), `test/lazy-permutation` (L12, L13, plus 3 leaves for `f5 0` alone), `test/lazy-permutation/ins` (L14, plus 0, 1 and the negative count), `test/sorted` (L15, with `testp3_theory`/`testp4_theory` computed by SO's `sum`), `test/replay/slazy` (L7 and L17: SL:126-211, 432, 482, 530, 617-618, 633-655, all bit for bit). `gte`'s pair `(l1 (), l2 ())` forces `l2` first (D19). SL:633's 2000-sample importance run stays in this suite (the whole suite takes 0.37 s). Every flonum literal of the file was checked against Python's `Fraction(float(lit))` (scratch `wp6/lits-check.scm`): none misreads. Mutations of the module (no `letlazy` in `lappend`, in `lmap`, in `lazy_permutation`; `append` for `rev_append` in `uncons`) each fail a case.
- **Validation.** Fresh `R` (`wp6/final`) with the §10.4 modules: `hansei-lazy.scm` `((ran 15) (failed 0))` in 0.37 s; `hansei.scm` 46/0 in 0.45 s and `hansei-random.scm` 9/0 in 0.42 s (unchanged; the core was not touched, so the hakaru10 check was not needed); the stubs `hansei-examples.scm` and `hansei-logic.scm` 0/0.

**2026-10-16, WP7 (`(aux hansei parsec)`, G7-G13).**

- **Definitions.** `parsec-stream-of-string` (PS:30-35), `parsec-string-of-stream` (PS:49-57), `parsec-stream-over` (PS:59-66), `parsec-stream-len` (PS:278-283), `parsec-stream-at-most-len` (PS:285-290), `parsec-eof` (PS:76-81), `parsec-pure` (PS:83-84), `parsec-empty` (PS:86-87), `parsec-sat` (PS:89-97), `parsec-char` (PS:99-100), `parsec-or` (PS:105-108), `parsec-alt` (PS:110-113), `parsec-ap` (PS:115-120), `parsec-fmap` (PS:122-125), `parsec-right` (PS:127-133), `parsec-left` (PS:135-139), `parsec-fix` (PS:141-144), `parsec-many` (PS:154-171, through the core's `probcc-soft-cut`), `parsec-many1` (PS:174-177), `parsec-run/forward` (PS:179-184), `parsec-run/backward` (PS:253-265), as §3.9 wrote them. The module has an **explicit export list** (the §7 names), as the other hansei modules; the WP0 stub had `*`. Imports are only `scheme`, `(chicken base)`, `(aux base)`, `(aux hansei)` (§2 row unchanged). `parsec-run/backward` takes `#f` for `None`, as `probcc-explore`. The parsers destructure `(v . stream)` with `match/first`; `parsec-pure`, `parsec-sat` and the combinators are curried `define`s, as the user's `((probcc-reify depth) model)`.
- **Operator precedence (not in §3.9).** OCaml's `*>` (first character `*`) binds tighter than `<*`, `<|>`, `<$>` and `<*>` (first character `<`, one level, left associative), so `p_char 'a' *> pali <* p_char 'a'` is `(parsec-left (parsec-right a pali) a)` and `f <$> p1 <*> p2 <*> p3` is `(parsec-ap (parsec-ap (parsec-fmap f p1) p2) p3)` with a curried `f`. The module comments say so; the suite's `pali`, `expression`, `term` and `factor` follow it (the logic reader's prototype nested `*>` the other way, which gives the same tables).
- **Plan gap: the G13 replay, as WP6's L7.** `exp_stream` draws its characters with `uniformly` over 15 characters, so `parsec-stream-over`'s exact masses (deviation 7) give the exact `1/2560` where upstream has `0.000390624999999999967`. The suite replays PS:492-501 bit for bit with a test-local `ml-stream-over` (`stream_over` with `flip 0.5` and the `ml-uniform` transcription of P:156-165, as in `hansei-lazy.scm`) and the module's parsers, whose binary choices are dyadic and so give OCaml's floats; the same run with `parsec-stream-over` is also checked: the same two expressions, at the exact `1/2560`. The literal was checked against Python's `Fraction(float(lit))`: it reads correctly. **WP9's PS replays (PS:504-557) need the same `ml-stream-over`** for the first `exp_stream`; the balanced one (PS:515-523) is test code and can call `ml-uniform` directly, with `flip 0.7` as the flonum `0.7`.
- **Oracles re-checked under OCaml** (scratch `wp7/o1.ml`, `tools/run-ocaml-hansei.sh -m script`, and `crit-tests/parsec.3draw.txt`): every G7-G10 and G13 value; G11's exploration has **17768 entries, 10100 of them values**, in OCaml and in Scheme (now asserted, besides the membership of `9999` and `9998` at `81/200000000`); G12's two sides at depth 6 (`(a a)` 1/2, `()` 3/8, `(a)` 3/32, two branches at 1/64, both sides, both systems); and the contrast cases with the commented-out `many` of PS:146-151 (`many/no-munch` in the suite): `(many0 ((a <* a) <|> a)) <* a` on `"aaa"` gives `(a)` 1/8 and `(a a)` 1/32, `many0 a <* a` gives `(a a)` 1/8, `many0 a` gives `(a a a)` 1/16. The "Previously" record of PS:352-359 (`[(0.25, (["aa"], 'a')); (0.125, (["a";"a"], 'a'))]`) is **not** what that `many0` gives (1/8 and 1/32): it came from an earlier `many`, not shipped, so it is not a test.
- **Tests beyond §8.5.** `test/parsec/kit` (the streams and primitive parsers, `parsec-stream-len`/`-at-most-len` both ways, `parsec-alt '()` fails, a `parsec-fix` counter at 1/16, and PS:272-273's depth-5 table of `parsec-stream-over`: 7 values and 16 branches at 1/128, compared through a test-local `shape` that writes `((C t) p)` as `(C p)`, the structural match §8.5 asks for G9); G8 also checks the values of PS:276's depth-10 generation (9 palindromes, `1/10` … `1/3200`); G10 adds `"7/4"` (1 at 1/64) and a failing `"1+"`. Cases: `test/parsec/kit`, `test/parsec/palindromes` (G7), `test/parsec/backward` (G8), `test/parsec/many` (G9), `test/parsec/expression` (G10), `test/parsec/factor` (G11), `test/parsec/nested` (G12), `test/replay/parsec` (G13). The doc of the suite gains the sections "Reversible parser combinators" (LG `#parsec`) and "Maximal munch, committed choice and nested inference" (LG `#many`); the other LG sections of §8.7 are WP9's. G11 takes 0.23 s (the plan's 0.67 s was the prototype's).
- **Deviation 22 widened in §9.** `parsec-stream-over` takes its alphabet as a list (PS:59-66 takes an array), as `parsec-alt` does; §3.9 said so without a number.
- **Mutation check.** With `probcc-letlazy` in place of `probcc-letlazy/nesting` in `parsec-stream-over`, the suite no longer terminates (killed after 300 s): the stream cells forced inside `parsec-many`'s soft-cut are drawn again in the inner world, as PS:22 warns.
- **Validation.** Fresh `R` (`wp7/final`) with the §10.4 modules: `hansei-logic.scm` `((ran 8) (failed 0))` in 0.48 s; `hansei.scm` 46/0 in 0.49 s, `hansei-random.scm` 9/0 in 0.45 s, `hansei-lazy.scm` 15/0 in 0.39 s, `hansei-examples.scm` 0/0 in 0.11 s (the core was not touched, so the hakaru10 check was not needed). The generated `src/test/testsuite-hansei*.html` were removed.

**2026-10-16, WP8 (examples, E1-E16).**

- **Files.** Only `src/test/hansei-examples.scm` (the WP0 stub filled in); no library module was touched, so the hakaru10 check was not needed. Imports added to the stub: `(only (scheme base) vector-copy)` (CHICKEN 6's `scheme` lacks it), `(only (chicken sort) sort)`, `(only (chicken port) with-output-to-string)`, `(only srfi-1 fold iota)`, `(aux hansei pmap)`.
- **Test-local definitions** (all `; cf.` or `(new)`): helpers `rel-approx?` (≈rel), `canon` ("set"; it sorts by the `write` string of the **whole entry**, so that it also canonicalizes lists of maxima `(u x)`, whose first element ties), `total` (M1:178's left fold), `accepted` (`probcc-leaves` of the tree: upstream's "reify: done; n accepted" counts), `upstream-order` and `variable-elimination/upstream` (below); the models of X, PE, DSL, NIPS (`tflip2-shared`, `tflip2-noshared`, `talarm`, `grass-model`, `grass-model/cloudy`, `two-coins`, `tfooibl1-3`, `tiblt2`, `tmusictr41-42`, `flips-and`, `flips-xor`, `flips-xor*`), NO (`no-predecessors`, `no-model-fwd`, `fixm`, `no-model/memo`, `no-model`), BT (`bt-chrom`, `bt-pchrom`, `bt-btype`, `bt-bloodtype`, `bt-pchr`, `bt-mchr`, `bt-bldtype`, the relations), HM (`hm-evolve`, `hm-observe`, `hm-evidence`, the counters, `hm-query/forward`, `/backward`, `/ve`), the blip dynamics shared by BN, BA, BL (`blip-xderiv`, `blip-yderiv`, `blip-new-dir`, `blip-pos0`, `blip-dir0`), BN (`bn-plane-state-0`, `bn-plane-fly`, `bn-blips-at-least(/ideal)`, `bn-model`, `bn-two-blips`), M1 (`m1-split-at`, `m1-split`, `m1-transpose1/5`, `m1-choose-op`, `m1-transform`, `m1-main`; M1's own `splitAt`, not LP's `split_at`), CB (`cb-observed-color`, `cb-model-nballs`, with the flip probability as an argument), IN (`in-utility`, `in-utility/asymmetric`, `in-language`, `in-language/some-all`, `in-an-action`, `in-agent-A-model`, `in-agent-A-model/some-all`, `in-argmax`, `in-inner-model`, `in-agent-S-model`, `in-forms`, `in-all-forms`, the hearer's inferencers), BA (`ba-one-of`, `ba-blip-model`, the functor instantiated inside the model), BL (`bl-lplane-state-0`, `bl-plane-fly`, `bl-force-plane`, `bl-lazy-plane`, `bl-lazy-state`, `bl-step`, `bl-blips-ideal`, the parameters, `bl-two-blips-ideal`). OCaml records and tuples are lists (deviation 25), `None`/`Some e` are `#f`/`(e)` as in `hansei-lazy.scm`, `fixm` uses `letrec` (the `let rec` NO:99-106 says OCaml rejects). D19 is applied to tuples, records, `::`, `+`, `=` and call arguments.
- **Cases (16).** `test/paper/grass-cloudy` (E1), `test/exact/basic` (E2), `test/exact/ibal` (E3), `test/exact/flips` (E4, plus `flips_xor` and `flips_xor'` of X:305-341), `test/paper/lawn` (E5), `test/noisy-or` (E6), `test/bloodtype` (E7), `test/hmm` (E8), `test/hmm/counters` (E9), `test/blip/naive` (E10), `test/music` (E11), `test/colored-balls` (E12), `test/incite/two-bit` (E13), `test/incite/some-all` (E14), `test/blip/ailog` (E15), `test/blip/lazy` (E16). The doc has the eight sections of §8.7 (one per H anchor, quoting H; the theory-of-mind quote is Shan's abstract).
- **Kinds stronger than §8.4** (rows E1, E3, E5-E8 updated). E1-E4, E6, E8 (without VE), E9, E11, E13, E14 are `equal?`, bit for bit; the accepted-world counts of upstream's comments are checked too (2, 3, 8, 14, 11, 1024, 4 for `flips_xor'`'s outer reification, 29 for M1). E10's `plane_state_0`, E15's twice-asked position and E16's lazy plane and two lazy planes are the exact `1/100` and `1/5000` (exact uniform masses, deviation 7; the plan quoted upstream's `0.000200000000000000064`). E12 is also run with the exact `4/5`, giving the analytic table as exact rationals (`13/400 … 227/12800`, re-derived with Python fractions), besides the plan's ≈ values with upstream's `0.8`. E14's tables and maxima keyed by forms are "set" as planned (IN:409-410 does come out reversed); the tables keyed by actions are in the same order in OCaml and by name, so `equal?`. `-0.` utilities are asserted as `-0.` (`equal?` distinguishes the sign), not normalized with `(+ x 0.)` as §8.4 suggested: they are upstream's.
- **Finding: deviation 5 and variable elimination.** BT and HM's VE model first gave masses a few ulps off upstream's. Verified cause: `probcc-variable-elimination` reflects `probcc-explore`'s table **sorted by mass** (deviation 5), so the outer model explores, and sums, the eliminated values in another order than upstream, whose `explore` answers in `PM.foldi` order. With a test-local `variable-elimination/upstream`, which reflects `(probcc-map-dist identity table)` (the table re-collated in upstream's descending-key order), BT's `test1exact`/`test2exact` and HM's `tq1ve_exact` are upstream's bit for bit; the suite checks both (`equal?` with the upstream-order VE, ≈rel 1e-9 with the library's). The same re-collation (`upstream-order`) makes the normalizations (`I:299-301`'s fold) and M1's evidence (`0.054` exactly, M1:178) bit-exact, since they sum in list order. Not a library change: D5 is the user's contract; WP10's deviation-5 doc may mention that replays of VE models need upstream's order.
- **Plan gap: DSL p.5's normalized lawn** (`0.468471442720369724 / 0.53152855727963022`) is not what today's upstream computes: under OCaml 4.14.2 `Inference.normalize (exact_reify grass_model)` gives `0.46847144272036984 / 0.53152855727963011` (scratch `wp8/dsl.ml`), as this port does, bit for bit. The suite checks the paper's values with ⊦= and today's with `equal?`.
- **Plan gap: E10's "`geometric_bounded 1/2`"** read as `geometric_bounded 2` (BN:208-210; `1` is already H11).
- **D20 checked further (3-draw build `crit-tests/hb3`).** BT:150-155 and BT:194-202 (`sample_importance (random_selector 17) 1000`) give `0.195630000000001358 …` and `0.250040000000001483 …`, not the records; BN's sampling records fail 7 assertions; BA:234-238 give `0.000762339606159999372` and `0.000776122190919999069`, not the records. So BA joins the non-replaying list; no replay of these files is in the suite. IN:136 (`sample_rejection (random_selector 17) 100`) gives `0.2/0.24/0.16/0.2` under the 3-draw build, not the record, as D20 says.
- **D11.** Models use `probcc-map` and named-let loops for `List.iter`; srfi-1 `fold` (re-entrant, left to right as `List.fold_left`) for the folds over plane states; plain `map` only for top-level deterministic tables (relations, the HMM transition table, `all_forms`).
- **Literals.** Every flonum literal of the file (154) was checked against Python's `Fraction(float(lit))` (scratch `wp8/lits.txt` with `wp6/lits-check.scm`): none misreads.
- **Mutation checks** (scratch `wp8/mut`): `fixm` without `probcc-memo` fails `test/noisy-or`; the lazy plane without `probcc-letlazy` fails `test/blip/lazy`; the hearer's state `s0` as a plain thunk fails both incite cases; `anyblip1` without memo, or `fixm` and `xpos0` both without memo, fail `test/blip/ailog` (the priors' memos alone keep `pos` consistent, as upstream's redundant memoization does).
- **Left for WP9 (long suite, §8.6):** BN's two-plane cases, BL's `q3b_s0..s2` (384 worlds each; a probe here took 7 ms for `q3b_s0`, so they may well fit a fast suite), the M1 replay, and the statistical tests.
- **Validation.** Fresh `R` (`wp8/final`) with the §10.4 modules: `hansei-examples.scm` `((ran 16) (failed 0))` in 1.61 s (target < 10 s; the slowest model, HM without VE at n = 10, takes about 0.55 s, so nothing moved to the long suite); `hansei.scm` 46/0 in 0.48 s, `hansei-random.scm` 9/0 in 0.43 s, `hansei-lazy.scm` 15/0 in 0.39 s, `hansei-logic.scm` 8/0 in 0.48 s, the `hansei-long.scm` stub 0/0 in 0.09 s. The generated `src/test/testsuite-hansei*.html` were removed (`testsuite-hakaru10-exact.html` and `testsuite-hakaru10-oracles.html`, from 13:46, are not this WP's and were left).

**2026-10-16, WP9 (logic G1-G6, the long suite §8.6).**

- **Files.** `src/test/hansei-logic.scm` (G1-G6 and their doc sections), `src/test/hansei-long.scm` (the WP0 stub filled in), and, for the reasons below, `src/test/hansei-lazy.scm`, `src/test/hansei-examples.scm` and the comment of the `test-hansei-long` target in `src/Makefile`. No library module was touched, so the hakaru10 check was not needed.
- **Plan error: D19 and the scrutinee of `match (e1, e2) with`.** OCaml does not build the tuple of a match scrutinee and binds its components **from the left**, so LV:172 `bl_compare` and SO:88 `gte` force `l1` first; `let (a, b) = (e1, e2) in` and tuples elsewhere stay right to left (checked under OCaml 4.14.2 bytecode, scratch `wp9/ord.ml`). Found because SO:177-183's importance sampling, run under OCaml with the three-draw build, gives `0.05482177734375` / `0.047857666015625`, which `hansei-lazy.scm`'s `so-gte` (written right to left per D19) did not reproduce (`0.0452…`, `0.0437…`); with `l1` first it does, sample for sample (`4491/81920`, `7841/163840`, the masses being dyadic). D19 amended; `so-gte` fixed (its exact tables, L15, do not depend on the order); the new LV port forces `l1` first. The records SO:177-183 themselves (`0.0445…`, `0.0504…`) still do not replay (D20).
- **Placement (§8.6 updated).** Measured over the compiled library, five cases planned for the long suite take well under 2 s and went next to the models they share, run by `make test`: SO `num_list 4 5` (70 ms; exact `6545/131072` = `testp5_theory`, plus the two OCaml-oracle sampling runs above) in `hansei-lazy.scm` `test/sorted`; BN's two-plane cases (`1/5000`, exact here, and `3.83795309168443282e-06` ≈rel 1e-9; 1.7 s) in `hansei-examples.scm` `test/blip/naive`; BL `q3b_s0..s2` (384 worlds each, the plane counts ≈rel 1e-9; 0.26 s, with a new `blip333-ideal`, BL:432-436) in `test/blip/lazy`; the M1 replay (M1:195-201, bit for bit, 12 ms) in `test/music`; PS:531-546 (2000 samples, bit for bit, 0.14 s) in `hansei-logic.scm` `test/replay/parsec`. The M1 replay needs no transcription of upstream's `uniform` (WP6's note expected one): `m1-split` draws `uniform 4`, whose exact `1/4` is the dyadic `0.25`. `hansei-long.scm` keeps zebra, PS:504-512 and PS:549-557 (30000 samples each) and the statistical tests.
- **Oracles.** LV, DS and Z: every upstream assertion holds under OCaml (`oracle-out/*.annot.clean.txt`). G2: §8.5 asked only that plain `letlazy` give weights different from G1's; the suite asserts the exact 17-answer table that upstream gives under OCaml with `letlazy = ProbM.letlazy` (scratch `wp9/lv2_letlazy.ml`). G3: the cheap split under OCaml at depths 12-25 (scratch `wp9/rt.ml`); the suite checks depth 15 and the paper's 25 (0.7 s). G5: the two traces with their exact weights, `1/6912` and `1/13824` (§8.5 said weights not asserted; OCaml gives `0.00014467592592592597` and `7.2337962962962986e-05`, scratch `wp9/pz.ml`). G6: TI does not run upstream (no `WorldLocalMem`); a reconstruction over ProbM's `Memory` and `thread_local` (`wref_new` = a new location set in `thread_local`, `wref_get` = `mref`, `wref_set` = `mset`; scratch build `wp9/hbw`) runs `type_inference.ml` with **every assertion holding** and reproduces the comment oracles TI:66-77 and TI:299-310; the suite checks all of TI's assertions and comment oracles, plus TI:258 (`Lx. x x`, cyclic types cut at depth 10), TI:260 (22 candidates at depth 10) and TI:282 (depth 7) from that run (sets compared programmatically, scratch `wp9/ti.out`).
- **Definitions (test-local, `; cf.` or `(new)`).** LV/RT: `bl-nil`, `bl-cons`, `blist->list`, `bl-append`, `bl-t3`, `bl-f2`, `bl-t3f2`, `a-boolean`, `a-blist/with` (the letlazy as an argument, for G2), `a-blist`, `bl-compare`, `bl-last`, `bl-del`, `bl-haslen`, `bl-prog3`, `bl-prog3/value`, `rt-a-list`; DS: `ds-powerset`, `mustbe`, `ds-schedule-model`; PZ: `pz-choose` (= `probcc-uniform/either`), `pz-u2`, `pz-speed`, `pz>=?` (declaration order), `pz-select-party`, `pz-elapsed-time`, `pz-without`, `pz-loop`; TI: `ti-var`, `ti-vf`, `ti-vx`, `ti-vz`, `ti-app`, `ti-lam`, `ti-num`, `ti-paren`, `ti-string-of-term`, `ti-a-term`, `ti-tp-same`, `ti-tp-v-same`, `ti-tp-pure`, `ti-int`, `ti-arr`, `ti-string-of-tp`, `ti-tp-memo` (over `probcc-wref-*`), `ti-a-tp`, `ti-new-tvar`, `ti-typeof`, `ti-infer`. `exp-stream/balanced` now takes the coin mass and the `uniformly` (`7/10 probcc-uniform/either` for G11, `0.7 ml-uniformly` for the PS:531 replay). Long suite: `statistics-misses`, `statistics/importance`, Z (`z-exists`, `z-to-the-right`, `z-to-the-left`, `z-next-to-dir`, `z-make-selector` over `probcc-lazy-permutation`, `z-model`), the parsec expression grammar with `ml-uniform`, `ml-stream-over`, `exp-stream`, `exp-stream/balanced`, `expression-model/value`, and the statistical models (BT without VE, `bt-*`; HM `hm-query1`; CB `cb-model-nballs` and `cb-posterior`, the closed-form posterior; SM `drunk-coin`, `dcoin-and`), copied from the other suites so that each suite stands alone. `None`/`Some l` are `#f`/`(l)`; D19 as amended; D12 respected (the zebra row and every five-element list bound with `let*`).
- **Cases.** `hansei-logic.scm` gains 10: `test/append` (LV:72-163), `test/append/backwards` (LV:183-259), `test/committed-choice` (LV:287-444), `test/committed-choice/letlazy` (G2), `test/cheap-nondeterminism` (G3), `test/daily-schedule` (G4, `1/8957952`), `test/bridge` (G5), `test/type-inference/types` (TI:141-199), `test/type-inference` (TI:236-263), `test/type-inference/backwards` (TI:66-77, 267-310). Its doc gains the sections of §8.7 not written in WP7: "Re-thinking Prolog" (LG `#vs-prolog`, RT Figure 1 l.285-308 mapped to the `probcc-` names), "No logic variables? No unification?", "The append relation" (with PD:187-217's `del`/`prog3` and Prolog's lost answers), "Solving scheduling problems", "Zebra puzzle". `hansei-long.scm`: `test/zebra` (unique solution at exactly `1/199065600000`, normalized `1.`; upstream's `5.023469650205764e-12` to a relative 1e-6), `test/replay/parsec/long` (PS:504-512 `(5 "05*1")` at `2.60416666666666626e-07`, PS:549-557 `(5 "03+2")` at `1.66666666666666666e-06`, bit for bit), `test/statistics/bloodtype`, `/hmm`, `/colored-balls`, `/drunk-coin`. The statistical check is: every value of the exact table has a mean within `max(floor, 4σ/√20)` of it (the floor alone when σ is NaN), the floors of §8.6. CB's exact posterior is computed in closed form with `q = 4/5`: `P(n = 1) = 0.41196368…` (§8.6's 0.41196 is that, rounded). The tests are deterministic (seeded); the means over seeds 1-20 are BT A 0.3154 / B 0.3291 / AB 0.1960 / O 0.1595, HM within 0.003 of the exact, CB P(1) 0.4120, dcoin `#f` 0.05262.
- **Reader trap again.** CHICKEN reads PS:546's `2.50000000000000215e-05` and its 17-digit form `2.5000000000000022e-05` one ulp high; the suite writes the 16-digit `2.500000000000002e-05`, the same double, with a comment. All 190 flonum literals of the four touched suites were checked against Python's `Fraction(float(lit))` (scratch `wp9/lits.txt` with `wp6/lits-check.scm`): none misreads.
- **Mutation checks** (scratch `wp9/mut`): `a-blist` or `a-boolean` without `letlazy` (the first does not finish in 300 s, the second fails `test/append/backwards`); `ti-tp-memo` without `probcc-wref-set!` fails the three TI cases; `pz>=?` always true fails `test/bridge`; DS without `probcc-memo` does not finish in 300 s; `a-blist` flipping the head only in the `Cons` world (not as OCaml's list literal does) fails `test/committed-choice` and `/letlazy`. Forcing `l2` first in `bl-compare`, or the head first in `rt-a-list`, changes no table these cases check (exact or exhaustive at their bounds).
- **Validation.** Fresh `R` (`wp9/final`) with the §10.4 modules: `hansei-logic.scm` `((ran 18) (failed 0))` in 1.84 s (target < 5 s); `hansei-long.scm` `((ran 6) (failed 0))` in 26.6 s (target < 120 s; recorded in its doc); `hansei.scm` 46/0 in 0.47 s, `hansei-random.scm` 9/0 in 0.43 s, `hansei-lazy.scm` 15/0 in 0.55 s, `hansei-examples.scm` 16/0 in 3.50 s. The generated `src/test/testsuite-hansei*.html` were removed.

**2026-10-16, WP10 (citation pass, doc sections, Python provenance, formatting).**

- **Citation pass.** Every top-level definition of the five modules and the six suites was checked by a script (scratch `wp10/audit.py`, `wp10/audit2.py`): a `; cf.` or `(new)` comment, or, for the test-local families written together (relations and their inverses, the parser grammar, the TI term constructors), the group comment above them, as the hakaru10 suites do. Fixed: `pmap-signal` and `ocaml-random-signal` opened with "deviation 30/31" but no `cf.`/`(new)` (now `; (new) deviation …`); five test-local definitions had none: `tuple-0101` (LP:36-41), `permutations-of-4` (`(new)`, LP:214-217's values), `p-a`/`p-b`/`to-unit` (PS:203-205), `expression-alphabet` (PS:473-474), `agent-S-model` (IN:389-404). All 33 register numbers are tagged in the sources (deviation 3, 9, 11, 12, 13, 20, 28, 29, 31, 32 once; deviation 25 fourteen times), and every tag was read against §9: none is misnumbered.
- **Doc sections (§8.7).** `hansei.scm` keeps "Introduction" and "Implementation" (with its source listing) and gains "Reification and reflection", "Exploration and its order", "World-local memory", "Laziness", "Memoization", "Nested inference", "Inference about inference", "Samplers and selectors", "Two CHICKEN hazards" and "Deviations": the register as an `ol` of 33 items numbered as §9 (the form of `test/hakaru10.scm:1309-1340`), then "kept as upstream" and the upstream discrepancies. Every quotation's lines were re-read in the `.txt` extracts; §8.7's own pointers are right, the quotations are at DSL p.15 l.676-679, p.17 l.728-730, p.19 l.837-842, p.21 l.912-915, EPP p.4 l.356-360 and l.374-379, ML p.1 l.82-83, P:456-463, PE:70-75. `hansei-lazy.scm`'s one section became three (H `#all-different`, the SL commentary, H `#sorted`, with a paragraph on LP and a quotation of H l.203-215); `hansei-random.scm` gains "why three draws" and the provenance of the Python oracle. `hansei-examples.scm` and `hansei-logic.scm` already had their §8.7 sections (WP8, WP9). `define/shown` was not added.
- **Plan gap: the two-draw example.** The value to quote for OCaml 4.14's two-draw float is `rej dcoin s17 10000` → `0.054` (`hansei_check.py`'s third block); `0.0501` is the three-draw generator **without** the xor of `bits`. Both texts say so.
- **Python provenance (§10.3).** `src/test/hansei-python/` holds `ocamlrandom.py`, `hansei_py.py`, `hansei_check.py`, `bounded_check.py`, `rint.py`, `n30.py`, `talarm.py` and a README in the form of `test/hakaru10-python/README.txt`. Changes from the scratch copies: **every** script, not only `hansei_check.py`, finds its siblings through `__file__` (since Python 3.11 `-I` no longer puts the script's directory on `sys.path`); `hansei_check.py`'s driver is under `if __name__ == '__main__':`, because `bounded_check.py` imports `dcoin_and` from it and ran its whole driver; `rint.py` imports `R` instead of finding the class through `inspect`. All seven re-run with `python3 -I -B` (no `__pycache__` in the tree) and print the values the plan quotes (SM block 1 all "exp" equal, `bounded_check` X:72-77 and H22, R5's `Random.int`s, N:30-31 `{0.6: 5, biased: 5}` vs `{0.6: 6, biased: 4}`, talarm `0.14/0.86` vs `0.1/0.9`).
- **Formatting: `scheme-indent -T 2` was not applied.** It is not installed here and is not a CHICKEN 6 egg; egg 0.7 (CHICKEN 5) was built in the scratchpad (`wp10/si/bld`), patched to count columns in bytes, as a CHICKEN 5 build does and as the user's committed files are aligned (`test/letcc.scm` is then a fixed point but for a trailing newline). The repository is not kept in its output: with the project's block forms passed as `-I let1 2 -I match/first 2 …` (scratch `wp10/si.sh`), only 4 of the 49 committed files of the `format` target are fixed points (HEAD's `test/hansei.scm`, the user's, is one but for a trailing newline), and the user's own committed `aux.hansei.scm` changes by 17 lines, where its hand alignment departs from the tool's. On the hansei files it would rewrite 19 to 469 lines each, and not only by the one-column `λ` offsets (the earlier WPs aligned by characters): it misaligns `'#(…)` vector literals, the continuations of `(only (chicken …) …)` import specs and the module export lists. So the pass was limited to what does not depend on the tool: no tab outside the user's doc strings, no trailing blank, one final newline, two-space indentation. Running `make format` with the user's own `scheme-indent` is left to the user.
- **Validation.** Fresh `R` (`wp10/repo`) with the §10.4 modules plus the hakaru10 ones: `hansei.scm` `((ran 46) (failed 0))` 0.50 s; `hansei-random.scm` 9/0 0.43 s; `hansei-lazy.scm` 15/0 0.54 s; `hansei-examples.scm` 16/0 3.58 s; `hansei-logic.scm` 18/0 1.84 s; `hansei-long.scm` 6/0 26.4 s; `hakaru10-exact.scm` 11/0 and 8/0 4.48 s; `hakaru10.scm` 13/0, 16/0, 7/0, 20/0, 9/0 7.27 s. `test/travel.scm` (the user's, unchanged) still stops at travel.scm:5 (WP1 note); a scratch copy with `(string->utf8 "561")` (and its import) prints its first distribution and stops at the top-level `match/non-overlapping` of travel.scm:143, as under the old core (WP3 note). The generated `src/test/testsuite-*.html` were moved to the scratchpad (`wp10/html`); no git command other than `status`/`show`/`diff` was run.

**2026-10-16, WP10 review fixes.**

- **The `map` hazard example (fixed).** Under the pmap core, `(probcc-reify/exact (map (λ (x) (probcc-coin 1/2)) '(1 2)))` answers `(((V (#t #t #f #f #t #f)) 1))`: `probcc-explore 0` shows four `C` branches at 1/4, whose worlds share one list built in place and collate into one entry. The "three entries/worlds" wording, left from the srfi-69 core of §5.6, was replaced in `aux.hansei.scm` (`probcc-map`'s comment), the header comments of the six suites, the "Two CHICKEN hazards" section and the doc of `test/map/reentrant`, and in §5.6. `test/map/reentrant` now also checks the one-entry result, so the doc cannot go stale.
- **`define/shown` (fixed, §8.7).** `test/hansei.scm` and `test/hansei-examples.scm` define `sources`, `define/shown` and `shown` as `test/hakaru10-exact.scm:32-41` does. Shown: `lawn` ("Reification and reflection"), `drunk-coin` and `dcoin-and` ("Samplers and selectors", `test/drunk-coin`), `agent-A-model` and `agent-S-model` (`test/sample/dist-selector-importance`); `grass-model` (`test/paper/lawn`), `flips-xor` and `flips-xor*` (`test/exact/flips`), `cb-observed-color` and `cb-model-nballs` (`test/colored-balls`). The other suites' models are test-local or already quoted in their doc sections.
- **`scheme-indent -T 2` (not applied, needs the user):** recorded as Q6 in §13.
- **Validation.** Fresh `R` (`wp10fix/final`) with the §10.4 and hakaru10 modules: `hansei.scm` 46/0 0.53 s; `hansei-random.scm` 9/0 0.44 s; `hansei-lazy.scm` 15/0 0.57 s; `hansei-examples.scm` 16/0 3.57 s; `hansei-logic.scm` 18/0 1.85 s; `hansei-long.scm` 6/0 26.8 s; `hakaru10-exact.scm` 11/0 and 8/0 4.48 s; `hakaru10.scm` 13/0, 16/0, 7/0, 20/0, 9/0 7.38 s. `test/travel.scm` still stops at travel.scm:5 (WP1 note). The generated HTML was moved to the scratchpad (`wp10fix/html`).

**2026-10-16, final review fixes.**

- **N:346-441, the red-light game (added).** `test/replay/nested` (`hansei-random.scm`) ports `police_monitoring`, `bad_luck`, `maxrisk`, `noisy_or` (unused by `game` upstream; checked on its own, `11/20`), `being_caught` and `game`, with the nested `sample_importance dist_selector 10`, and replays N:432-438 (11 entries) and N:440-441 (5 entries) bit for bit, in the Scheme order, with upstream's literals. R9 amended.
- **D20 vs §8.6 (fixed both ways).** SO's `model n` (SO:119-123) is in `hansei-lazy.scm` `test/sorted`, with SO:189-200 (n = 2, 3, 4) replayed as today's three-draw OCaml gives them (`51716307/104857600`, `55302254749/343597383680`, `97798819259/2748779069440`, the doubles `0.493205137252807624`, `0.160950744608999219`, `0.0355790031822834849`); IN:416-422 replay likewise in `hansei-examples.scm` `test/incite/some-all` (`-25.6944444444444429`, `-19.0034722222222214`, today's OCaml; the records came from another generator). `hansei-long.scm` gains `test/statistics/sorted` (n = 2, 3, 4 against 1/n!) and `test/statistics/blip` (BN 1000 samples against `0.00059257469565211177`; BL `qs3b_s0` 1500 samples against `1.72212182417348577e-08`), with copies of the SO, BN and BL models. D20 now says IN gets no tolerance test (its records are maxima of estimated utilities, with no exact value for a mean). §8.6 lists the new tests.
- **SL:731-778, the sprinkler (added, L18).** `grass-model` in `hansei-lazy.scm` `test/memo/functions`: `(((V #t) 1.))` and `(((V #f) 0.322) ((V #t) 0.2838))`. The review's `0.321999999999999897` is wrong: upstream's pattern `[(0.2838, V true); (0.322, V false)]` matches the floats exactly (`crit-tests/slazy.3draw.txt:315`), and the port gives exactly `0.322`.
- **SL:57-70 `tests3` (added to L1).** With CHICKEN's `delay`/`force`, the table is `None` 1/2, `(11 11 #t)` 1/4, `(11 11 #f)` 1/4 with 2 calls: the promise is shared by the worlds that split inside its first `force`, so the value 21 is lost; D9's rationale is now checked.
- **Comments in the core.** `probcc-letlazy/nesting` tags deviation 4 (P:361); `op/plus`/`op/subtract` are named in the shared comment, `op/greater` has its own (deviation 5), `probcc-mem-counter` and `probcc-mem-new-loc` have one each; `probcc-random-selector` says it raises "can't happen" when rounding puts `r` past the last cumulative mass.
- **`probcc-uniform` of a non-integer count (fixed).** `(probcc-uniform 5/2)` and `(probcc-uniform/range 0 2.5)` looped forever; a count that is not `integer?` now raises `(exn hansei)` of kind `malformed`; `2.` stays a count. Deviation 7 amended; `test/uniform/edges` checks it.
- **`test/travel.scm`.** Unchanged (the user's file). WP3's and WP10's validation now read "the hansei part of `test/travel.scm` still runs", checked on a scratch copy seeded with `(string->utf8 "561")`, which prints the `probcc-reify/exact` table and stops at travel.scm:143's non-hansei `match/non-overlapping`.
- **Validation.** Fresh `R` (`final-validate`) with the §10.4 and hakaru10 modules: `hansei.scm` 46/0 0.54 s; `hansei-random.scm` 9/0 0.46 s; `hansei-lazy.scm` 15/0 0.82 s; `hansei-examples.scm` 16/0 3.79 s; `hansei-logic.scm` 18/0 1.87 s; `hansei-long.scm` 8/0 38.9 s; `hakaru10-exact.scm` 11/0 and 8/0 4.44 s; `hakaru10.scm` 13/0, 16/0, 7/0, 20/0, 9/0 7.55 s. The generated HTML was moved to the scratchpad (`final-validate-html`).
