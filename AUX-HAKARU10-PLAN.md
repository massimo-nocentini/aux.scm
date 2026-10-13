# Final implementation plan for `(aux hakaru10)`, a CHICKEN 6 port of Hakaru10

> **Status.** This is the implementation plan that `src/aux.hakaru10*.scm` and `src/test/hakaru10*.scm` were written from; their `plan §N` references point here. Two things changed afterwards, and the code is the reference wherever the two disagree:
>
> - **Native delimited control instead of a construction monad.** A model is a direct-style Scheme program, and `dist`, `diracN`, `condition` and `if_` `shift` to the `reset` of the handler that runs it, using the native `shift`/`reset` of `(chicken continuation)`. The same model runs under three handlers: the trace-graph builder of the MH engine, exact enumeration into `(aux hansei)` search trees, and likelihood weighting. Each source file's header describes this.
> - **Review fixes.** Four review rounds added, among others, lazy values for inactive Dirac nodes, the construction error for SExps inside Scheme values, the rollback of a step left open by an error, and deviation 11. The deviation register in `src/test/hakaru10.scm` is the current one.
>
> The amendment that made the first change, cited as `PLAN-IDIOM` in the sources, is `AUX-HAKARU10-PLAN-IDIOM.md`; where the two plans disagree, the amendment wins. The Python transcription of §0 is in `src/test/hakaru10-python/`.

This plan is complete on its own. The implementer needs only this plan and the upstream folder `upstream/okmij-hakaru10/`. No repository file was modified while preparing it.

---

## 0. Sources, citation keys and conventions

**Citation keys.** All upstream paths are relative to `aux.scm/upstream/okmij-hakaru10/`.

| Key | File | Remote |
|---|---|---|
| S | Syntax.hs | https://okmij.org/ftp/kakuritu/Hakaru10/Syntax.hs |
| M | Metropolis.hs | …/Metropolis.hs |
| D | Distribution.hs | …/Distribution.hs |
| Mem | Memory.hs | …/Memory.hs |
| U | Util.hs | …/Util.hs |
| E | Examples1.hs | …/Examples1.hs |
| CB | ColoredBalls.hs | …/ColoredBalls.hs |
| B1 | Birds1.hs | …/Birds1.hs |
| BE | Bench.hs | …/Bench.hs |
| W | index.html | https://okmij.org/ftp/kakuritu/Hakaru10/index.html |
| P | design.txt / design.pdf | Kiselyov, "Probabilistic Programming Language and its Incremental Evaluation", APLAS 2016, LNCS 10017, DOI 10.1007/978-3-319-47958-3_19 |
| BMP | BirdMigrationProblem.txt / .pdf | PPAML Bird Migration problem statement, v6, Aug 2014 |
| R | random-1.0.1.1 `System/Random.hs` | https://hackage-content.haskell.org/package/random-1.0.1.1/src/System/Random.hs |

**Paper sections in P.** Each is given as txt line and PDF page.

| Section | Title | txt line | Page |
|---|---|---|---|
| Abstract | | l.8-23 | p.1 |
| §1 | Introduction | l.24 | p.1-4 |
| §2 | Hakaru10 by Example | l.189 | |
| §2.1 | Model Compositions and their Laws | l.202 | p.5-7 |
| §2.2 | Branching Models | l.328 | p.7-8 |
| §2.3 | Conditioning | l.367 | p.8-9 |
| §3 | Evaluation (Bayesian Linear Regression l.461; Population Estimation l.505) | l.452 | p.9-12 |
| §3.1 | Performance | l.617 | p.12-13 |
| §4 | Implementation | l.641 | p.13-14 |
| §4.1 | Design Overview | l.721 | p.14-15 |
| §4.2 | Incremental Recomputation | l.765 | p.15-16 |
| §4.3 | Special Treatment of the Dirac Distribution | l.801 | p.16-17 |
| §4.4 | Branching | l.844 | p.17 |
| §5 | Related Work | l.870 | |
| §6 | Conclusions and Future Work | l.903 | |
| Footnotes | 1 (l.241), 5 (l.566-569), 6 (l.776-777) | | |

**W anchors.** `#design` (l.23), `#implementation` (l.50), `#examples-tests` (l.69), `#colored-balls` (l.110), `#bird-migration` (l.120).

**Source-citation rule (mandatory).**
- Every top-level definition carries a comment `; cf. M:404-419` (or `S:…`, `D:…`, `P §4.4 p.17 l.844-869`) naming what it transcribes.
- Every departure from upstream carries `; deviation (D:298-304): …`, with the number from the register in §8.
- Every module begins with a header comment that lists the upstream files it mirrors, states the code version (November 2016; W updated 14 July 2018, W:52-53, 179), and gives the DOI.

**Upstream folder.** `upstream/okmij-hakaru10/` is untracked, and the upstream files carry no licence statement. Do **not** commit it without asking the user, and especially not the PDFs. Citations are file:line regardless.

**Provenance of the "replay" values.** Design 1 built a Python transcription of the engine with the random-1.0.1.1 StdGen. It reproduced the values marked *replay* in §9 bit-exactly, and the judges re-ran and confirmed most of them. Those values are therefore regression oracles for the Scheme port, which must use the same RNG, the same draw order and the same floating-point evaluation order. The prototype lives in an ephemeral scratchpad and is **not** a dependency of this plan.

---

## 1. Decisions (conflicts resolved)

| # | Question | Decision | Why |
|---|---|---|---|
| C1 | RNG | Default is a pure-Scheme **random-1.0.1.1 StdGen** behind a pluggable **rng-source** protocol. MRG32k3a and a `(chicken random)` adapter are optional extras (WP12). The fixnum-only `random-double` is **mandatory**, verified against a bignum reference. | Design 1 plus all three judges: exact replay of upstream records is the strongest oracle. The pluggable source comes from judge 3; the fixnum path avoids roughly 2^93-sized bignums per draw. |
| C2 | Default MH semantics on branch reactivation | `#:semantics 'upstream` is the default. `'extended` is opt-in. | Two of three judges. The user asked for a faithful, source-referenced port, and replay tests need upstream behaviour. The extended mode fixes the bias Design 3 found (§6.8) and is documented and tested. |
| C3 | File layout | **Six source files, six modules, six egg extensions** (§2). This is not one file holding six modules, and not one file holding two. | It lets the work packages run in parallel without file conflicts (§13) and follows the `aux.kanren.*` precedent. Memory.hs is folded into the engine module. |
| C4 | Node representation | Immutable node-*state* records `(val ll dst tstamp)` in a vector, plus static per-address description vectors (kind, role, parents, fn, update closure, scope, branch ranges). | Design 1's copy-on-write state matches `Mem.modify_ref` (Mem:36-37). Design 3's static descriptions enable the reference evaluator, the exact enumerator and the kernel test. |
| C5 | Rollback | A per-step undo journal. On reject, restore everything as upstream does, **including `now`**; the rng source is not restored. Queue deduplication uses a `queued` flag vector, not stamps. | Faithful to M:904-906. Design 2's "monotone now" argument is no longer needed. |
| C6 | Surface syntax | Keep upstream names: `dist`, `diracN`, `condition`, `if_`, `mcmC`. Add `model`, `define-model`, `if_*`, `lift`, `letlift`, `pair`, `collect` and `constant`. Keep `sexp-map` (= `lift`) and `sexp+ sexp- sexp* sexp/` as aliases. | Design 2's ergonomics plus Design 1's names. |
| C7 | `if_` test | Must be `boolean?`, checked at construction and at every entry update. It is not coerced to truthiness. | `SExp Bool` (S:275). Rejects Design 2's coercion. |
| C8 | `bern` log(1-p) | Plain `(log (- 1. p))`, as in D:56. | Bit-exact replay. A `log1p` path is not used. |
| C9 | Beta value outside [0,1], or a,b ≤ 0 | Raise an error as upstream does (D:331-337). | Faithful. Rejects Design 3's −∞. |
| C10 | Categorical with duplicate keys | Keep the upstream asymmetry: the sampler sums, the density takes the first match (D:98-102). Comment it. | Faithful. Rejects Design 3's merging. |
| C11 | Gamma bugs | Fix both: the sampler applies `scl` twice when shape < 1 (D:279-282), and the density formula is wrong (D:298-304). | Design 1. Neither fix changes any replay value. |
| C12 | `update_exit` failure (M:788-792) | No-op: return self unchanged, so tstamp is not bumped and nothing propagates. Add a dedicated test using Design 1's example. | All designs. Judge 1 found Design 3's example muddled. |
| C13 | Inactive-parent assertion (M:469-473) | In `'upstream` mode it is an assertion in every update. In `'extended` mode it is dropped. | Judges 1 and 3. |
| C14 | No active resampleable node | The step emits the same value, draws nothing, and counts as "accepted, no-op". | Upstream loops forever or indexes `[]` (M:962-966). No replay is affected. |
| C15 | `condition` name clash | The implementation imports only the procedures it needs from `(chicken condition)`. The docs tell users `(import (except (chicken condition) condition))`. | Design 3. |
| C16 | Errors | Composite conditions `(exn hakaru10)` with an extra `kind` property. | Design 2. Usable with `⊦raises`. |
| C17 | Name of the exact enumerator | `exact-posterior`, never `enumerate`, which would clash with aux.base.scm:402. | Design 3. |
| C18 | Evaluation order | Node creation order is load-bearing (topological order, P l.791-794, and replay). Never create nodes in procedure-argument positions or inside `map`; use `let*`, `for-each`, `fold` and srfi-1 `map-in-order`. Documented and used in all tests. | Design 3. Scheme argument order is unspecified. |
| C19 | Memoization | `memo/finite` (eager) plus a node-owner check. No lazy memo. | Design 2 and footnote 5 (P l.566-569). |
| C20 | tac1-3 | Assert that the three sequences are identical (S:370-371). The recorded 620/523/532 (S:346-368) are not asserted. | The prototype shows they come from an earlier engine revision. |
| C21 | `pdep_run` | Exact `equal?` (E:21-22 records True; the prototype confirms it). | Faithful. |
| C22 | Extended mode, error raised by an inactive node's update | Catch only `(exn hakaru10)` parameter errors and treat them as Δ = −∞, i.e. reject. Document that the extended target is then restricted to states where every inactive node has valid parameters. *Amended (review round 3):* the criterion is the engine's `deferrable?`, i.e. parameter errors and any non-hakaru10 Scheme error (such as `car` of `'()`), at an update and at construction alike, so that a partial lifted function rejects whether it is written inline or shared through a diracN (deviation 11); other `(exn hakaru10)` kinds propagate. | Judge 3's caveat. This is one more reason the mode is opt-in. |
| C23 | `mcmC§` stream | Built with aux.base `δ`, which is `delay` and so compatible with `(aux stream)`'s `take§`. No dependency on `(aux stream)`. | Avoids pulling in the continuation modules. |
| C24 | `momenta` result | A pair `(mean . var)`. | Works directly with `equal-approx?` on pairs. |

---

## 2. Files, modules, dependencies

| File (under `src/`) | Module | Mirrors | Imports (besides `scheme`, `(chicken base)`) |
|---|---|---|---|
| `aux.hakaru10.random.scm` | `(aux hakaru10 random)` | R (StdGen) | `(chicken fixnum)`, `(chicken flonum)`, `(chicken bitwise)` |
| `aux.hakaru10.distribution.scm` | `(aux hakaru10 distribution)` | D; M:46-105 (DistK); M:434-435 | `(aux hakaru10 random)`, `(chicken flonum)`, `(only (chicken condition) make-property-condition make-composite-condition signal)`, srfi-1, `(aux base)` |
| `aux.hakaru10.util.scm` | `(aux hakaru10 util)` | U | `(chicken sort)`, `(chicken flonum)`, `(chicken io)`, srfi-1, srfi-69 |
| `aux.hakaru10.engine.scm` | `(aux hakaru10 engine)` | Mem; M:110-977; the internals of S:84-279 | random, distribution, `(chicken fixnum)`, `(chicken flonum)`, `(only (chicken condition) …)`, srfi-1, srfi-4, `(aux base)` |
| `aux.hakaru10.exact.scm` | `(aux hakaru10 exact)` | (new) reference evaluator, exact posterior, kernel residual | engine, distribution, srfi-1, srfi-69, `(aux base)` |
| `aux.hakaru10.scm` | `(aux hakaru10)` | S (public syntax), re-exports | all of the above, `(aux base)` |

The internal modules export `*`, as `(aux lua internal)` does. The public module has the explicit export list in §3.

**Not ported**, but each is mentioned in a comment that cites its lines:
- M:1174-1415 (the old Wingate-style `Measure` code);
- D:127-146 (`choose`) and D:169-210 (Atkinson Poisson);
- `dirac` from D:45-51 as a user-level distribution (the engine has its own Dirac kind).

D:383-411 (Laplace and symmetric Dirichlet, commented out upstream) are optional extras in WP2b, marked as such.

**Test files** (under `src/test/`):

| File | Suites | In `make test` |
|---|---|---|
| `hakaru10-random.scm` | `hakaru10-random` | yes |
| `hakaru10-distribution.scm` | `hakaru10-distribution` | yes |
| `hakaru10-util.scm` | `hakaru10-util` | yes |
| `hakaru10.scm` | `hakaru10-language`, `hakaru10-replay`, `hakaru10-laws`, `hakaru10-statistics`, `hakaru10-semantics` (main doc) | yes |
| `hakaru10-exact.scm` | `hakaru10-exact` | yes |
| `hakaru10-examples.scm` | `hakaru10-examples` (colored balls, regression, birds synthetic) | yes |
| `hakaru10-bench.scm` | none (prints only) | no: `make bench-hakaru10` |
| `hakaru10-birds.scm` | `hakaru10-birds-real` (opt-in, real CSVs) | no: `make test-hakaru10-long` |

---

## 3. Public API: `(aux hakaru10)`

### 3.1 Models and construction

A **model** is a thunk run in a construction context. It is the Scheme form of `Model a = MCMCM (SExp a)` (S:82; P §4.1 l.737-747).
- Calling a model while a chain is being built creates nodes and returns an SExp.
- Calling it again creates fresh nodes. This keeps "no accidental sharing" (P l.178-179), and sharing comes only from binding a name (S:53-73).
- Haskell `do` becomes `let*`, and `return e` becomes the tail expression.
- Statements run only for their effect stay in the body, for example `(dist (condition c1 bern) b)` (P §2.3 l.422-437).

| Export | Signature | Semantics | Source |
|---|---|---|---|
| `model` | syntax `(model body …)` | `(τ body …)` | S:82 |
| `define-model` | syntax `(define-model (name arg …) body …)` / `(define-model name body …)` | `(define (name arg …) (model body …))` / `(define name (model body …))` | |
| `dist` | `(dist d)` with `d` a distk → SExp | A root node, never updated (`with_node_none`) | S:143-144, M:422-423 |
| | `(dist ctor arg …)` with `ctor` a procedure → SExp | Each `arg` is an SExp or a plain value (auto-Val, replacing `fromInteger`/`pure`, S:120-132). Any arity: upstream has only 1 and 2 (S:146-151). Always creates a **new** node. `ctor` may return a resampleable or an observed distk (S:143-151, `k` polymorphic). | S:137-151, S:218-273 |
| `diracN` | `(diracN e)` → SExp | A Val gives a fresh constant Dirac node. A node reference gives **the same reference**, shared and not copied. A lifted expression gives a Dirac node with an update. | S:161-169 |
| `condition` | `(condition obs d)` | A resampleable distk gives an observed distk (`conditioned`). A procedure gives `(λ args (condition obs (apply d args)))`, checked when applied. | S:174-193, M:434-435 |
| `if_` | syntax `(if_ test then-expr else-expr)` → SExp | `(if_* test (τ then-expr) (τ else-expr))` | S:275-279 |
| `if_*` | `(if_* test then-model else-model)` → SExp | Procedural form, for folds such as CB:85-99 | S:275-279, M:641-686 |
| `lift` | `(lift f e …)` → SExp | Applicative lifting (`<$>`, `<*>`, `liftA2`). If no `e` is an SExp, it returns `(f e …)` evaluated now in pure context, which is a Val (`fmap f (Val x) = Val (f x)`, S:100). Otherwise it fuses into one lifted expression whose leaves are the deduplicated union of the args' leaves (`fmap f (NT t g) = NT t (f . g)`, S:99-116). | S:99-116 |
| `letlift` | syntax `(letlift ((x e) …) body …)` | `(lift (λ (x …) body …) e …)` | |
| `pair` | `(pair a b)` | `(lift cons a b)` | S:134-135 |
| `collect` | `(collect sexps)` | `(apply lift list sexps)` (`sequenceA`); `(collect '())` is `'()` | B1:113-114, P l.503-504 |
| `constant` | `(constant x)` | An explicit Val, needed only to pass a procedure value | |
| `sexp?` | predicate | | |
| `sexp-map` | alias of `lift` | | |
| `sexp+ sexp- sexp* sexp/` | `(sexp+ e …)` = `(apply lift + es)` | Replaces the `Num`/`Fractional` instances | S:120-132 |
| `memo/finite` | `(memo/finite keys f)` → `(λ (key) sexp)` | Eagerly calls `(f k)` for every key, in order, at the call site. Unknown key → error. Must be called before any `if_` that uses it. It replaces stochastic memoization with pre-allocation. | CB:41-44, W:111, P §3 l.531-544, footnote 5 l.566-569 |

**No bind is exported.** SExp is not a monad (S:118; P §4.1 l.739-747).

### 3.2 Distributions (user level: resampleable distks)

| Export | Signature | Values | Source |
|---|---|---|---|
| `bern` | `(bern p)` | `#t`/`#f` | D:53-62, M:73 |
| `bern/flip` | `(bern/flip p)` | Same, with the upstream "always flip" kernel (fwd-bwd = 0, no draw). Upstream has it commented out; it is kept for the flip replays. | D:64-70 |
| `categorical` | `(categorical alist)`, with `alist` = `((value . weight) …)` and values compared with `equal?` | | D:87-105 |
| `uniformly` | `(uniformly list)` | | D:72-85 |
| `uniform` | `(uniform lo hi)` | flonum | D:107-117 |
| `normal` | `(normal mu sd)` | flonum | D:119-125, 148-166 |
| `gamma` | `(gamma shape scale)` | flonum (corrected, §8) | D:274-315 |
| `beta` | `(beta a b)` | flonum | D:317-350 |
| `poisson` | `(poisson lambda)` | exact integer | D:352-381 |
| `laplace`, `dirichlet/symmetric` | optional extras (WP2b) | | D:383-411, commented out upstream |
| `make-distribution` | `(make-distribution name #!key log-density sample (resample #f) (support #f) (params '()))` → resampleable distk | `log-density: x → flonum`; `sample: src → (values x ll)`; `resample: src x ll → (values x' ll' fwd-bwd)`, default independence proposal (D:29-32); `support: #f` or a thunk returning an alist `((x . p) …)` | D:14-41, D:18-19 (custom-proposal hook), M:953-954 TODO |
| `distribution-log-density` | `(distribution-log-density distk x)` | | |
| `distribution-sample` | `(distribution-sample distk src)` → `(values x ll)` | | |

All Double parameters are coerced with `exact->inexact` when the distribution is built. This makes the arithmetic match Haskell `Double`; for example the `1/3` literal must become `0.3333…`.

### 3.3 Sampling

| Export | Signature | Semantics | Source |
|---|---|---|---|
| `mcmC` | `(mcmC limit model #!key (seed 17) (rng #f) (semantics 'upstream) (check #f))` → list of `limit` values | §6.7. `rng` (an rng-source) overrides `seed`. `check`: `#f`, `#t` (every step) or n (the first n steps); see §6.9. | S:196-197, M:856-906 |
| `mcmC-fold` | `(mcmC-fold kons knil limit model #!key …)` | `(kons sample acc)` in chain order, constant memory | |
| `mcmC§` | `(mcmC§ model #!key …)` → infinite stream (promise of pair) | Haskell's lazy list (M:897-906) | |
| `model-graph` | `(model-graph model #!key (seed 17))` → alist `((nodes . n) (resampleable . r) (observed . o) (dirac . d) (inactive . i) (entries . e))` | Builds once and counts. Mirrors the debug print at M:860-867. | P §4.1 Fig. 1, P §3 l.495-497 |
| `exact-posterior` | `(exact-posterior model #!key (seed 17) (max-paths 1000000))` → `((value . prob) …)` sorted by value | §6.10. Error if an **active** resampleable node lacks finite support. | (new) |
| `make-stdgen-source` | re-exported from random | | R |

### 3.4 Utilities (re-exported from util)

| Export | Returns | Source |
|---|---|---|
| `hist` | `(hist xs)` → `((v . count) …)`, ascending by `generic<?` | U:11-12 |
| `frequency` | `(frequency pred xs)` → flonum | |
| `momenta` | `(momenta xs)` → `(mean . unbiased-var)` | U:16-22 |
| `tabulate` | `(tabulate step xs)` → `((bin . mass) …)` | U:40-45 |
| `tabulate/bins` | `(tabulate/bins nbins xs)` | `tabulate'`, U:28-37 |
| `kl` | `(kl p q)`; errors on a missing bin | U:51-56 |
| `plot` | `(plot h #!optional (path "/tmp/plot.dat"))` | U:58-65 |

**`generic<?` order.** Types rank boolean < number < char < string < symbol < pair/list < vector < other. Within a type: `#f < #t`; numbers by `<`; strings by `string<?`; symbols by name; lists lexicographically.

**Errors.** `(hakaru10-error? x)` is exported. All errors are composite `(exn hakaru10)` conditions; `hakaru10` has a `kind` property, one of:
- `conditioning`, `construction`, `dependency`, `parameter`, `branch`, `support`, `internal`.

Messages reuse the upstream strings where they exist, for example "dependency violation!" (S:242) and "normal: invalid parameters" (D:153).

---

## 4. Internal module APIs

### 4.1 `(aux hakaru10 random)`

**Pure StdGen layer (R).** `(define-record stdgen s1 s2)` holds fixnums.

| Procedure | Behaviour |
|---|---|
| `(mkStdGen n)` | `s = n & #x7fffffff` (bitwise; negative n via two's complement), `(q, s1) = divmod s 2147483562`, `s2 = q mod 2147483398` → `(stdgen (+ s1 1) (+ s2 1))`. Gives (18, 1) for 17. |
| `(stdgen-next g)` → `(values z g')` | Exact transcription of `stdNext`: `k = quot s1 53668`, `s1' = 40014*(s1 - k*53668) - k*12211`, add 2147483563 if negative; `k' = quot s2 52774`, `s2' = 40692*(s2 - k'*52774) - k'*3791`, add 2147483399 if negative; `z = s1'' - s2''`, add 2147483562 if `z < 1`. |
| `(stdgen-ival-integer l h g)` | `randomIvalInteger`: swap if `l>h`; `k = h-l+1`, `b = 2147483561`, `n = iLogBase b k` (`i<b → 1`, else `1 + iLogBase b (quotient i b)`); `acc = 1`, then n times `acc = x + acc*b`; result `l + (modulo acc k)`. Fixnum fast path when `k < b` (n = 1); exact-integer path otherwise. |
| `(stdgen-double g)` | `random :: Double`. `acc` from 3 `next` calls, kept mod 2^53 at each step (see below); value `(fp/ (fixnum->flonum acc) 9007199254740992.)`. |
| `(stdgen-double/reference g)` | The same through exact bignums: `acc` over 3 nexts, then `(modulo acc (expt 2 53))`. Used only by tests. |
| `(stdgen-range-double lo hi g)` | `randomRFloating`: swap if `lo>hi`; `(coef, g') = stdgen-double`; `(fp* 2. (fp+ (fp* .5 lo) (fp* coef (fp- (fp* .5 hi) (fp* .5 lo)))))`, in exactly this order. |

The `stdgen-double` value equals `mask53 .&. (minBound + acc mod 2^64)`, because 2^63 ≡ 0 mod 2^53.

**Mandatory fixnum mod-2^53 step.** `acc := (x + acc*b) mod 2^53`. Split `acc = hi*2^27 + lo` (lo < 2^27, hi < 2^26). Then `acc*b mod 2^53 = (((hi*b) mod 2^26)*2^27 + lo*b) mod 2^53`. Every intermediate is below 2^59, so it stays a fixnum on 64-bit.

**rng-source protocol.** `(define-record rng-source name state double! integer! copy)`.

| Procedure | Meaning |
|---|---|
| `(make-stdgen-source seed)` | `state` is a mutable stdgen |
| `(rng-double! src lo hi)` | `randomR (lo,hi) :: Double`, closed interval, for every Double draw in D and M |
| `(rng-integer! src lo hi)` | `randomR (lo,hi) :: Int`, inclusive (uniformly D:80, resample_trace M:962) |
| `(rng-source-copy src)` | |
| `(rng-source-state src)` | for debugging |

Optional (WP12):
- `(make-mrg32k3a-source seed)`: L'Ecuyer 1999, flonum arithmetic, 6-flonum state. `double!` maps u ∈ (0,1) to `lo + u*(hi-lo)`; `integer!` is `lo + floor(u*(h-l+1))`.
- `(make-chicken-random-source #!optional bytevector)`: rejects an all-zero seed (core bug A, §12) and clamps u to (0,1).

### 4.2 `(aux hakaru10 distribution)`

**Records.**
- `(define-record rdistr name params log-density sample resample support)` is `SomeRDistr` (D:34-41) as a record of closures.
- `(define-record distk kind rd value)` is `DistK` (M:46-60).
  - `kind` ∈ `resampleable`, `observed`, `dirac`.
  - For `dirac`, `rd` is `#f` and `value` holds θ. For `observed`, `value` holds the observation.

**Procedures.**
- Constructors: `bernS`, `bern/flipS`, `uniformlyS`, `categoricalS`, `uniformS`, `normalS`, `gammaS`, `betaS`, `poissonS` (M:70-95), which the public module re-exports as `bern` and so on (S:26-36). `diracS` is internal (M:93-95).
- `(conditioned obs dk)` (M:434-435) and `(condition obs d-or-ctor)` (S:174-193, with the checks of §7).
- `(sampleK dk src)` → `(values v ll)` (M:97-100). `(distinfo dk)` → `(values kind rd)` (M:102-105).
- `log-gamma`: AS 245 (D:212-272).
- `(libm-lgamma x)`, `(libm-erf x)`: `foreign-lambda double "lgamma"/"erf" double`. They are used only by tests (accuracy checks, erf bins). libchicken already links `-lm`.
- `make-distribution`, `distribution-log-density`, `distribution-sample`.
- `hakaru10-error`, `hakaru10-error?`. These live here because it is the lowest module that raises.

### 4.3 `(aux hakaru10 util)`

`hist`, `generic<?`, `frequency`, `momenta`, `tabulate`, `tabulate/bins`, `kl`, `plot` (§6.12).

### 4.4 `(aux hakaru10 engine)`, internal

- **Chain record and accessors** (§5.3).
- **Parameters.** `current-chain` (`#f` outside construction and inside pure functions). The scope stack is a chain field.
- **SExp.** `nref` record `(chain addr)`, `nt` record `(leaves fn)`, `sexp?`, `(sexp-value e)` (`eval_nctor`, S:201-208), `(sexp-leaves e)`, `(lift* f es)`.
- **Construction.**
  - `(pure-call thunk)`
  - `(create-node! ch role kind parents fn dk update)` (M:404-419, M:233-243)
  - `(add-dependency! ch parent child)` (M:246-250)
  - `(with-node-ctor! ch sexp df)` (S:218-273, M:451-526)
  - `(to-node! ch sexp)` (S:211-214)
  - `(ifnode! ch entry th-thunk el-thunk)` (M:641-686)
  - `(if-node! ch test th el)` (S:275-279)
- **MH.** `(modify-node! ch addr state')` (M:262-271), `(propagate! ch)` (M:293-317), `(resample-trace! ch)` (M:956-977), `(apply-proposal! ch addr v ll fb)`, `(transit! ch)` → accepted? (M:908-944).
- **Driver.** `(build-chain model src semantics)` → `(values ch result-addr)`; `(mcmc-fold kons knil limit model #!key …)` (M:856-906).
- **Journal.** `journal-begin!`, `journal-rollback!`, `journal-commit!`.
- **Hooks.** `chain-step-hook` (a procedure `(ch accepted? delta)`, or `#f`).
- **Raw accessors for `exact`.** `chain-node-state`, `chain-set-node-state!`, `chain-count`, and the static vectors.

### 4.5 `(aux hakaru10 exact)`, internal

`(reference-check ch)` (§6.9), `(exact-posterior* ch)` (§6.10), `(load-state! ch assignment)`, `(forced-step-log-alpha ch addr v)`, `(ext-states ch)`, `(kernel-residual model #!key (seed 17))` (§6.11).

---

## 5. Data structures

### 5.1 SExp (S:85-93)

- **Val**: any Scheme value that is not an `nref` or `nt` record and not a procedure. A procedure in an SExp position is an error ("a model where an SExp is expected; bind it with let* first"); use `(constant f)` to pass one deliberately.
- **NRef**: `(nref chain addr)`. The `chain` field is the **owner check**: using an nref whose chain is not `eq?` to the current chain is an error. That catches stale nodes from an earlier `mcmC`, from a discarded initial-retry build (M:882-892), or from a `memo/finite` table that outlived its build.
- **NT**: `(nt leaves fn)`, where `leaves` is a vector of addresses with no duplicates (first occurrence order) and `fn` takes their values positionally. It generalizes `NTree` with `NTLeaf`/`NTPair` to n-ary.
  - Fusion: `lift` over nt args rebuilds one nt with the union of leaves and a composed fn.
  - Tree shape has no effect on values, draws or tstamp checks, because the check uses the max over leaves (S:264-273, M:511).
  - The "cannot happen" cases (S:108-111, 115) disappear, because `lift` takes a plain Scheme procedure.

### 5.2 Node state and static description (M:120-143, Mem:31-99)

- **Node state** (immutable): `(define-record nstate val ll dst tstamp)`.
  - `dst` is the current rdistr, or `#f` for Dirac.
  - `tstamp` is the step at which `val` last changed. LL-only changes do not bump it (M:126-129).
- **Memory**: a growable vector `states`, indexed by address from 1, plus `count`. Addresses are consecutive, so creation order is topological order (Mem:59-63; P §4.2 l.791-794).
  - `NodeRef` is the address. `refresh_nref` (M:253-256, Mem:82-84) becomes a plain vector read; say so in a comment.
  - `unsafeCoerce` (Mem:5-13) is not needed.
- **Static vectors**, written only during construction:

| Vector | Content |
|---|---|
| `kind` | `resampleable`, `observed` or `dirac` (M:125) |
| `role` | `root` (no update), `plain`, `entry` or `exit` |
| `parents` | vector of addresses (the leaves) |
| `fn` | for `plain`/`entry`: `values → distk` (`df ∘ g`); for `root`: the constant distk |
| `update` | the closure (M:131-142); for an entry it is replaced once by `update-entry` at construction (M:679-685) |
| `scope` | list of `(entry-addr . polarity)`, innermost first |
| `branches` | for an entry: `((then-lo . then-hi) . (else-lo . else-hi))`; ranges are half-open `(lo, hi]` |
| `exit-info` | for an exit: `#(entry-addr th-addr el-addr)` |
| `deps` | list of child addresses with set semantics (M:170-172) |

### 5.3 Chain (MCState, M:165-197)

| Field | Content | Ref |
|---|---|---|
| `rng` | rng-source; the seed is never rolled back | M:166, M:906 |
| `now` | fixnum, starts at 1 | M:167, M:875 |
| `states`, `count` | memory | M:168 |
| static vectors | above | |
| `inactive` | fixnum vector of counts; 0 = absent | M:174-179 |
| `delayed` | u8 vector (`delayed_updates`), upstream mode only | M:181-183 |
| `llcum` | flonum: total LL during construction, Δ during a step | M:185-187, M:915 |
| `sampleable` | list, **prepended** during construction (M:239); frozen into vector `samp` with `samp[i] = (list-ref sampleable i)`, so index draws hit the node upstream's `!!` hits (M:962) | M:189-190 |
| `card-change` | fixnum, cumulative, never reset | M:193-194, M:911-914 |
| `added` | replaced by `count` snapshots: branch node sets are ranges `(count_before, count_after]` (M:650-663) | M:191-192 |
| `queue` | binary min-heap of fixnums plus a `queued` u8 vector: set semantics, smallest address first | M:273-295 |
| `journal` | `#f`, or a list of undo entries | M:904-906 |
| `scope` | current construction scope (list) | |
| `semantics` | `upstream` or `extended` | |
| `step-hook` | `#f` or a procedure | |
| `debug-msg` | dropped; use the `hakaru10-debug` parameter for tracing | M:196 |

**Journal entries.**
- `(state addr . old-nstate)`
- `(inactive addr . old-count)`
- `(delayed addr . old-flag)`

Saved scalars: `now`, `card-change`. `llcum` is reset at every step and is meaningless after a reject.

On reject, undo in reverse order and restore the scalars. On accept, drop the journal. `deps`, `samp` and the static vectors never change after construction.

---

## 6. Algorithms

### 6.1 Floating-point and draw-order discipline (needed for replay)

- Transcribe every Haskell Double expression with binary `fp+`, `fp-`, `fp*`, `fp/`, keeping Haskell associativity: `infixl 6 + -`, `infixl 7 * /`.
  - Unary minus binds like binary minus, so `-tau * sq` = `negate (tau*sq)`; numerically the same as `(-tau)*sq`.
  - `**` is `fpexpt` (libm `pow`), `log` is `fplog`, `exp` is `fpexp`, `sqrt` is `fpsqrt`, and `pi` is `3.141592653589793`.
- Draw only where upstream draws, in the same order. Observed and Dirac nodes draw nothing (M:97-100).
- Every random number goes through `rng-double!` / `rng-integer!`.

### 6.2 Distributions (D)

| Distribution | Construction | Sample | logDensity |
|---|---|---|---|
| `bern p` (D:53-62) | `lT = log p`, `lF = log (1. - p)` | `t = rng-double! 0 1`; `(t <= p) → (#t, lT)` else `(#f, lF)` | `x ? lT : lF` |

  `bern/flip` resample: `x' = (not x)`, `ll' = logDensity x'`, fwd-bwd `0.`, no draw (D:64-70).

| Distribution | Construction | Sample | logDensity |
|---|---|---|---|
| `uniformly lst` (D:72-85) | `'()` → error "uniformly: empty list" (D:77); `ll = -(log len)` | `i = rng-integer! 0 (len-1)`; `(vector-ref arr i)` | `ll` if `member` (by `equal?`), else −∞ |
| `categorical al` (D:87-105) | `ptotal` = left fold of `+` from 0. over weights; normalized `p/ptotal` | `p = rng-double! 0 1`; walk: if `p < p0` → `(x, log p0)`, else `p := p - p0` and continue. **Deviation 5:** if the list runs out, return the last `(x, log p_last)`. | `log` of the **first** `assoc`/`equal?` match, else −∞ (D:98). Keep the duplicate-key asymmetry and comment D:98-102. |
| `uniform lo hi` (D:107-117) | `l = log (/ 1. (- hi lo))` (`recip`) | `x = rng-double! lo hi` → `(x, l)` | `l` if `lo <= x <= hi`, else −∞ |

**`marsaglia`** (D:119-125). Draw `x = rng-double! -1 1`, then `y = rng-double! -1 1`. Compute `s = x*x + y*y` and `q = sqrt(((-2)*log s)/s)`. If `1 >= s && s > 0`, return `(x*q, y*q)`; otherwise loop.

**`normal-rng mu sd`** (D:142-146). If `sd > 0`, return `mu + sd*x`, using the first variate. Otherwise error "normal: invalid parameters" (`parameter`).

**`normal mu sd`** (D:148-166).
- `tau = 1/(sd*sd)`, `lf = log((tau/pi)/2)`.
- Density: `((-(tau*sq(x-mu))) + lf)/2`.
- Sample: `normal-rng`, with LL = logDensity.
- The constructor itself does not check `sd`; only sampling errors.

**`gamma-rng shape scl`** (D:274-296).
- Errors: `shape <= 0` gives "gamma: got a negative shape paramater", and `scl <= 0` the scale message (both `parameter`).
- If `shape < 1`: `(g1, …) = gamma-rng (shape+1) 1.` (**deviation 3**: upstream passes `scl`, then multiplies by `scl` again), then `w = rng-double! 0 1`, and return `scl * g1 * (w ** recip shape)`, i.e. `(scl*g1)*(w**(1/shape))`.
- Otherwise, Marsaglia–Tsang:
  1. `d = shape - 1/3` (with `1/3` as `(fp/ 1. 3.)`), `c = 1/sqrt(9*d)`.
  2. `v = normal-rng 1 c`, repeated **until** `v > 0`; the first draw counts.
  3. `x = (v-1)/c`, `sqr = x*x`, `v3 = (v*v)*v`, `u = rng-double! 0 1`.
  4. Accept if `u < 1 - 0.0331*(sqr*sqr)`, or else if `log u < 0.5*sqr + d*((1 - v3) + log v3)`.
  5. Accepted → `(scl*d)*v3`; otherwise restart from step 2.

**Gamma density.** **Deviation 2**, using the scale form:
- `−shape·log scl − x/scl + (shape−1)·log x − log-gamma shape`;
- −∞ for `x < 0`, and −∞ if `shape <= 0` or `scl <= 0` (the D:305 fallback).
- Upstream's `scl*log shape - scl*x + …` (D:298-304) matches neither parameterization.
- Sample: `(x, logDensity x)`.

**`beta-rng a b`** (D:317-328).
- If `a <= 1 && b <= 1` (Jöhnk): `u`, `v` from `rng-double! 0 1`; `x = u ** (1/a)`, `y = v ** (1/b)`. If `(x+y) <= 1`, return `x/(x+y)`; otherwise retry.
- Otherwise `ga = gamma-rng a 1`, `gb = gamma-rng b 1`, return `ga/(ga+gb)`.

**Beta density** (D:330-340).
- Error "beta: parameters must be positve" if `a<=0 || b<=0`.
- `factor = (lgamma(a+b) - lgamma a) - lgamma b`.
- For x outside [0,1], error "beta: value must be between 0 and 1".
- Otherwise `(factor + (a-1)*log x) + (b-1)*log(1-x)`.

**`poisson λ`** (D:352-381).
- `λ < 0` → error. `expl = exp(-λ)`.
- Density:
  - `x==0` → `-λ`
  - `λ==0` → −∞
  - `x==1` → `log λ - λ`
  - `x==2` → `log(λ*λ/2) - λ`
  - otherwise `(x*log λ - λ) - log-gamma(x+1)`
- Sample:
  - `λ==0` → `(0, 0.)`.
  - Otherwise `u = rng-double! 0 1`; loop `(p, k, acc)` starting at `(u, 1, 1.)`: while `p > expl`, draw `u'` and continue with `(p*u', k+1, acc*λ/k)`. Return `k-1`.
  - **Deviation 4:** the LL is `logDensity (k-1)`. Upstream returns the probability `acc*expl` (D:379).

**`log-gamma`** (D:212-272). A verbatim AS 245 transcription:
- branches at `x < 1.5`, `x < 4`, `x < 12`, `x > 3e6`, and Stirling with `alr2pi = 0.918938533204673`;
- the r1–r4 coefficients;
- +∞ for `x <= 0` or `x > 1e308`.

It is not libm, so replay stays exact.

**Default resample** (D:29-32): `(receive (x ll) (sample src) (values x ll (fp- ll llold)))`.

### 6.3 SExp and construction (S:137-279; M:233-526)

**`(pure-call thunk)`** runs `(parameterize ((current-chain #f)) (thunk))`. Every user function goes through it: lift bodies, ctors and `fn`. So `dist`, `diracN` and `if_` inside them raise "called outside model construction or inside a pure function" (`construction`). During MH steps `current-chain` is already `#f`.

**`create-node!`** (M:404-419, M:233-243):
1. `(values v ll) = (sampleK dk rng)`.
2. `addr = count+1`, `states[addr] = (nstate v ll rd now)`.
3. `llcum := llcum + ll`.
4. If the node is resampleable, prepend `addr` to `sampleable`.
5. Record the static fields, and `scope := chain.scope`.

**`add-dependency!`** (M:246-250). Add `child` to `deps[parent]` unless it is already there (IntSet union).

**Branch-escape check.** Unless the child's role is `exit`, require `scope(parent)` to be an `eq?` tail of `scope(child)`. Otherwise raise "node escapes its branch" (`branch`). This replaces Haskell's lexical guarantee.

**`with-node-ctor! ch e df`** (S:218-273; covers `with_node` M:451-489, `with_node2` M:492-526, and the general case):
- `e` is a Val → `create-node! root (pure-call (df e))`, with no update (S:219).
- `e` is an NRef → leaves `#(a)`, `fn = df`.
- `e` is an NT → leaves `e.leaves`, `fn = df ∘ e.fn`.
- Compute `dk = (pure-call (fn (leaf values)))`. Create the node with `role plain`, `kind = distk-kind dk`, and update `(doupdate)`. Then call `add-dependency!` from each leaf in order (S:257-261).

**`doupdate`** (S:233-253, M:462-489, M:500-526), called as `(λ (ch addr) …)` and returning `(values nstate' extra-addrs)`:
1. `ts = max` of the leaves' tstamps (`reeval_tree`, S:264-273).
2. In `'upstream` mode, if any leaf or self is inactive, raise "the parent … or the node itself are inactive" (`dependency`; M:469-473).
3. If `ts <= self.tstamp`, raise "dependency violation!" (`dependency`; S:241-243).
4. `dk = (pure-call (fn values))`. If its kind differs from `kind[addr]`, raise (`construction`); upstream kinds are static types.
5. Build the new state:
   - Dirac → `(nstate (distk-value dk) 0. #f now)`. tstamp is **always** bumped (M:482).
   - Resampleable or Observed → `(nstate self.val (log-density rd self.val) rd self.tstamp)`, keeping val and tstamp and **storing the new dst** (M:483-488: "!!! Don't forget to update the distribution").
6. Return extra `'()`.

**`to-node!`** (S:211-214). Val → `create-node! root (diracS v)`. NRef → its address. NT → `with-node-ctor! e diracS`.

**Public `dist`.**
- `(dist d)` with `d` a distk → `create-node! root d`.
- `(dist ctor a …)`:
  1. Validate that `ctor` is a procedure and that no arg is a procedure (model).
  2. Owner-check every nref.
  3. If every arg is a Val → `create-node! root (pure-call (ctor a …))`.
  4. Otherwise → `with-node-ctor!` of `(lift* list args)` with `df = (λ (vals) (check-distk (apply ctor vals)))`.
  5. Return an nref.

**`diracN`** (S:166-169). Val → `create-node! root (diracS v)`. NRef → returned as is. NT → `with-node-ctor! e diracS`.

### 6.4 Branches: `if_*` → `ifnode!` (S:275-279; M:641-686; P §4.4 p.17 l.844-869)

```
(if_* test th el):
  entry := with-node-ctor! test diracS        ; ALWAYS a new node (S:277); a constant test → root entry
  role[entry] := entry; check (boolean? val[entry])
  ifnode!(entry, th, el)

ifnode!(entry, th, el):
  if entry ∈ sampleable: error "Entry node for if should not be sampleable"       ; M:643-647
  tv := val[entry]
  c0 := count
  push (entry . (not tv)) on scope; inactive-nr := to-node!((if tv el th)); pop    ; inactive FIRST, M:650-651
  c1 := count
  deactivate!(c0, c1]                                                              ; M:659; may be empty (M:655-657)
  push (entry . tv); active-nr := to-node!((if tv th el)); pop                     ; M:661
  c2 := count
  th-nr, el-nr, th-range, el-range chosen by tv                                    ; M:665-666, 676-677
  exit := create-node! role exit, kind dirac, dk (diracS val[active-nr]),
          update (update-exit entry th-nr el-nr), exit-info                         ; M:665-667
  add-dependency! entry→exit, active-nr→exit, inactive-nr→exit                     ; M:670-673 (in this order)
  if update[entry] ≠ #f: update[entry] := (update-entry update[entry] th-range el-range)  ; M:679-685
     (a constant test has no update; Haskell's lazy `Just prev_upd` never fires)
  branches[entry] := (th-range . el-range)
  return exit
```

**`deactivate! (lo, hi]`** (M:696-717). Walk in ascending address order with a local `llc = 0.` and `k = 0`:
- Journal the old count, then increment it.
- On 0→1: `llc := llc + (observed ? ll : 0.)`; if resampleable, `k += 1`.

Afterwards `llcum := llcum - llc` and `card-change -= k`.

**`activate! (lo, hi]`** (M:724-751). Same shape. Test the count **before** decrementing; `updateLookupWithKey` returns the old value (M:733-746). On 1→0: `llc += observed ll`, `k += 1` if resampleable. Afterwards `llcum := llcum + llc`, `card-change += k`.

**`update-entry prev th-r el-r`** (M:760-765):
1. `old = val[self]`.
2. `(s', extra) = prev(...)`; check `(boolean? s'.val)`.
3. If `s'.val` is `eq?` to `old`, return `(s', extra)`.
4. Otherwise `switch-branches!` with `(newactive, newinactive) = s'.val ? (th-r, el-r) : (el-r, th-r)` (M:767-775):
   - `deactivate! newinactive`, **then** `activate! newactive`;
   - in `'upstream` mode, `newdeps` = addresses in `newactive` with `delayed` set; clear them (journaled);
   - return `(s', extra ∪ newdeps)`.

**`update-exit entry th el`** (M:778-794):
1. `active = val[entry] ? th : el`.
2. If `tstamp[entry] > self.tstamp` or `tstamp[active] > self.tstamp` → `(nstate val[active] 0. #f now)`.
3. **Otherwise return self unchanged (deviation 1).** Upstream fails at M:788-792. Trigger: `c ~ bern .5; d ~ normal 0 1; if_ c d (dist normal 5 1)` with c false. Resampling d enqueues the exit through `inactive_nr→exit` (M:673), but neither the entry nor the active result changed.

### 6.5 Propagation (M:262-317; P §4.2 p.15-16 l.765-800)

**`modify-node! addr s'`** (M:262-271):
1. Journal `(state addr . old)` when the journal is open, then store.
2. Δ contribution:
   - `'upstream`: `llcum := (llcum + s'.ll) - old.ll`.
   - `'extended`: the same, except **0** when `inactive[addr] > 0` and the kind is observed (§6.8).
3. Return `deps[addr]` if `old.tstamp ≠ s'.tstamp`, else `'()`.

```
propagate!:                                  ; M:293-317
  loop while queue nonempty:
    a := pop-min; queued[a] := 0
    if semantics = upstream and inactive[a] > 0:
        journal+set delayed[a] := 1; continue                 ; M:298-303
    if tstamp[a] >= now: error "dependency violation: updating the already updated"   ; M:307-308
    if update[a] = #f: continue                                ; M:311
    (s', extra) := update[a](ch, a)
        ; extended mode only, inactive a: an (exn hakaru10 parameter) raised here → llcum := -inf, clear queue, stop (C22)
    push-all extra; push-all (modify-node! a s')               ; M:313-317
```

Min-address order guarantees two things (M:273-288, M:319-379):
- each node is updated at most once per step, after all its updated parents;
- an entry switches branches before any of its branch nodes in the queue is processed.

### 6.6 One MH step: `transit!` (M:897-944) and `resample-trace!` (M:956-977)

```
transit!:
  journal-begin!; save now, card-change
  now += 1; llcum := 0.                                     ; M:915
  c-old := card-change; S := |samp|                         ; M:916-917
  if S + c-old = 0: commit; return 'noop                    ; deviation 6 (M:962-966)
  ; resample-trace!                                         ; M:956-977
  loop: i := rng-integer! 0 (S-1); a := samp[i]; if inactive[a] > 0 → loop   ; retry consumes draws, as M:966
  s := states[a]
  (v, ll', fb) := resample(s.dst, src, s.val, s.ll)         ; the node's own update is NOT run (M:643-645)
  apply-proposal!: push-all (modify-node! a (nstate v ll' s.dst now))       ; M:976
  propagate!                                                 ; M:921
  Δ := llcum; c-new := card-change
  if Δ is ±inf: reject                                       ; rejectedLL = isInfinite (M:853-854, 927)
  ratio := (Δ - fb) + (c-new = c-old ? 0. : (log (fp/ (exact->inexact (+ S c-old)) (exact->inexact (+ S c-new)))))   ; M:929-934
  accept := ratio >= 0. or (rng-double! 0 1) < (exp ratio)  ; u drawn ONLY when ratio < 0 (M:939-944); NaN → draws u, rejects
  accept → journal-commit!; reject → journal-rollback! (now, card-change, states, counts, flags restored; rng NOT)   ; M:904-906
  call step-hook (ch accepted Δ) if set
```

`S + card_change` is the number of active resampleable nodes. With the default kernel, `Δ − fb` cancels the resampled node's own prior term. Derivation of the cardinality term: M:1039-1089; merged-node justification: M:1091-1168.

### 6.7 Driver (M:856-906; S:196-197)

**`build-chain model src semantics`.** Make a fresh chain on `src` with `now = 1`. Then:
- `(parameterize ((current-chain ch)) (to-node! ch (model)))`, i.e. `m >>= to_node` (S:197);
- check that the model returned an SExp, not a procedure;
- freeze `samp` and clear `scope`.

**Initial retry** (M:882-891), transcribed literally:

```
go(limit, ch):
  limit = 0                 → (ch, 0)
  llcum(ch) is ±inf         → go(limit-1, build-chain again on the SAME src)
  else                      → (ch, limit)
```

If the returned limit is 0, the result is `'()` / `knil`.

**Emission** (M:897-906).
- Emit `val[result]`, then `transit!`. Repeat until `limit` values have been emitted, using the **original** `limit` (M:869).
- The first value is the initial trace, and a rejected step repeats the previous value.
- Run `limit−1` steps: the final step's value is never emitted.

**Deviation 7 (model with no resampleable nodes).** Every step is a no-op (deviation 6), so the chain is `limit` copies of the value.

`mcmC` = `(reverse (mcmc-fold cons '() …))`. `mcmC§` builds a lazy `(δ (cons v rest))` stream.

There is no burn-in or thinning; users `drop`, as upstream does (B1:202-205, E:203-205).

### 6.8 `'extended` semantics: the opt-in fix for the reactivation bias

**The bias** (Design 3 §0.1). In `'upstream` mode an inactive node's updates are deferred (M:298-303) and replayed on reactivation (M:767-775).
- For **Observed** nodes this is exact: activation first adds back the stale LL.
- For **Resampleable** nodes the replayed update adds `ll_now − ll_stale` to the Δ of the switching step. That charges LL changes from earlier steps, which were accepted without that term. M:1159-1165 argues these terms cancel, but the argument assumes reactivated nodes are fresh prior draws, while upstream reuses stale values.

**Counterexample `pswitch`.**

```
c ~ bern 0.5
m ~ uniformly '(0 1)
if_ c (y ~ categorical ((m . .9) (1-m . .1)))
      (diracN (lift identity m))
return c
```

The true answer is P(c) = 0.5. The exact stationary distribution of upstream's kernel gives P(c) = **0.377286**; Design 3's `exact.py` computed it and judges 1 and 2 re-ran it.

**Extended semantics** runs MH on the extended space where inactive Resampleable nodes are auxiliary variables with their prior. This is the merged-node argument made exact. Differences from `'upstream`:
1. Updates are never delayed: an inactive node's update runs.
2. Its LL change counts in Δ if it is Resampleable, and not if it is Observed.
3. Activation and deactivation still move only Observed weights (M:696-751).
4. The card term is unchanged (M:929-934).
5. The M:469-473 assertion is dropped.
6. The proposal still picks only active nodes (the M:966 retry).
7. C22 applies.

Upstream examples are unaffected in distribution, because their branch-local Resampleable nodes all have constant parameters. They are not bit-identical: the summation order differs.

### 6.9 Reference evaluator: `check:` (exact module, wired by the public `mcmC`)

`(reference-check ch)` recomputes from scratch in address order and compares with the incremental state:
- **Activity.** `count*(a) = |{(e . pol) ∈ scope[a] : val[e] ≠ pol}|`, using stored entry values. Require `inactive[a] = count*(a)` and `card-change = −#{inactive resampleable}`.
- **Values.** For every node that is active (every node, in `'extended` mode):
  - plain Dirac: val = fn(parent vals);
  - Resampleable/Observed: `equal?` of `rdistr-params` of dst against fn(parent vals), and ll within 1e-9;
  - exit: val = val[active result];
  - entry: boolean.
- **Δ check (`'extended` only).** The accepted Δ equals `L(after) − L(before)` within 1e-9, where `L` = Σ resampleable ll + Σ active observed ll. For a rejected step, the state equals the pre-step snapshot.

The public `mcmC` installs a `step-hook` that calls `reference-check` for the requested number of steps. It also snapshots the state before each checked step, for the rollback comparison.

### 6.10 `exact-posterior` (exact module)

1. Build once with seed 17 to obtain the static graph.
2. DFS over addresses 1..count with an environment of values and a weight. For each node:
   - **Activity** by scope from the current entry values. An inactive node is skipped (its value is unused).
   - **Root/plain resampleable:** `d = fn(parent vals)`. Branch over `(rdistr-support d)` with weight × p. No support → error (`support`).
   - **Observed:** weight × `exp(log-density d obs)`; prune at 0.
   - **Dirac/entry:** compute the value. **Exit:** the active result's value.
3. Accumulate `weight` by the result value in a srfi-69 `equal?` table, normalize, and sort with `generic<?`.

**Supports.**
- bern: `#t → p`, `#f → 1−p`.
- uniformly: each element → 1/len.
- categorical: each entry with its normalized p. These are the *sampling* semantics; duplicate keys are documented.
- Poisson, continuous distributions, and `make-distribution` without `support`: no support.

The result is the model's posterior, which is also the marginal of the extended target.

### 6.11 Kernel residual (exact module; tests only; `'extended`; finite supports)

1. `ext-states`: every assignment of all resampleable nodes, including inactive ones, from their supports. `max-states` defaults to 10^5.
2. For each state x, `load-state!` writes the values, then recomputes everything through §6.9 (Dirac values, dst, ll, counts, card) and sets `delayed` to 0. Set `π(x) ∝ exp L(x)`.
3. For each active resampleable `a` (probability `1/N(x)`) and each `(v . q)` in the support of its dst:
   - `forced-step-log-alpha` runs `journal-begin!`, `now += 1`, `apply-proposal!` with `fb = ll_v − ll_old` (D:29-32), `propagate!`, and computes `ratio`;
   - then it rolls back;
   - `T[x→x'] += (q/N)·min(1, e^ratio)` (0 if Δ is infinite or NaN), and the rest goes to `T[x→x]`.
4. Return `max_x |(πT)(x) − π(x)|`.

### 6.12 Utilities (U)

- **`momenta`** (U:16-22). `n = (exact->inexact (length xs))`. `av = (foldl + 0. xs)/n`, the Haskell `sum`. `σ² = (fold-right (λ (x acc) (fp+ (sq (fp- x av)) acc)) 0. xs) / (n − 1.)`.
- **`tabulate step xs`** (U:40-45). `q = 1/len`. Bin `= (fp* (exact->inexact (floor (/ x step))) step)`. Mass is accumulated as `q + old`, inserting in `fold-right` order. Return an alist sorted ascending.
- **`tabulate/bins`** (U:28-37). `step_raw = (mn = mx) ? 1 : (mx−mn)/nbins`; `step = 10 ** round(log step_raw / log 10)`, with Haskell `round` = banker's rounding, which equals Scheme `round`.
- **`kl p q`** (U:51-56). Fold over p's bins in ascending order: `sum + pi*log(pi/qi)`. A missing `qi` raises "Undefined KL divergence - q is 0 when p is not at point …".
- **`hist`** (U:11-12). Counts with `equal?`, sorted by `generic<?`.
- **`plot`** (U:58-65). Writes the header `x  y`, then `k v` lines.

---

## 7. Run-time checks replacing Haskell's types

| Haskell guarantee | Source | Scheme check (raised kind) |
|---|---|---|
| Observations are external (`CondV a = a`) | S:38-42, S:174-193; P §2.3 l.412-421, §4.1 l.751-759; E:194-201 | `condition` with an SExp `obs` raises "observation must be external to the model" (`conditioning`) |
| Only resampleable distributions can be conditioned: no double conditioning, no Dirac | S:180-186; P l.397-402; E:180-186 | kind check, at once for a distk and at application for a procedure (`conditioning`) |
| Condition distributions, not boolean formulas | S:43-48; P l.393-398, 653-673 | No boolean `observe` is exported |
| SExp is not a monad, so the graph is static | S:118; P l.739-747 | No bind is exported. `dist`/`diracN`/`if_` raise in pure context and outside `mcmC` (`construction`). |
| `Model` vs `SExp` | S:82 | A procedure in an SExp position is an error; use `constant` (`construction`) |
| `if_` test is `SExp Bool` | S:275 | `boolean?` at creation and in `update-entry` (`branch`) |
| Static kind of a node | M:46-60 | The ctor must return the same kind on every update (`construction`) |
| Node internals hidden | S:199-200 | `sexp-value` and the state accessors are not exported from `(aux hakaru10)` |
| Lexical scoping of branch nodes | (implicit) | branch-escape scope check (`branch`); owner check (`construction`) |
| Entry not sampleable | M:643-647 | kept in `ifnode!` (`internal`) |
| `Args` arity ≤ 2 | S:137-151 | Arity errors come from `apply`, wrapped with the ctor name; any arity is allowed |
| `Eq a`, `Show a`, `SomeRDistr`, GADTs, `unsafeCoerce` | S:28-31, M:123, D:34-41, M:46-60, Mem:5-13 | `equal?`; a debug printer only; closure records; the `kind` symbol; not needed |

---

## 8. Deviation register (each one carries a `; deviation N (file:lines)` comment)

1. **`update-exit`** returns self unchanged when neither the entry nor the active result is newer; upstream fails (M:788-792).
2. **Gamma density** uses the scale form; upstream's formula is wrong (D:298-304).
3. **`gamma-rng` with shape < 1** applies `scl` once (D:279-282).
4. **Poisson sample LL** is `logDensity k`, not the probability (D:379).
5. **Categorical sampling** returns the last entry if rounding exhausts the walk; upstream has no `[]` case (D:99-102).
6. **No active resampleable node:** the step is a no-op; upstream loops or crashes (M:962-966).
7. **Model with no random nodes:** constant chain (a consequence of 6).
8. **`'extended` semantics** (opt-in), §6.8; in that mode the M:469-473 assertion is dropped and C22 applies.
9. **Static checks become run-time errors**, in the style of Ścibior et al. [20] (P l.153-155, 419-421, 882-886).
10. **Extra checks** with no upstream counterpart: branch escape, owner, kind stability, and a boolean test.
11. **Deferred Dirac values** (M:99, M:120-129, S:100-116): a diracN value computed while its node is inactive, if it raises, is a deferred value (Haskell's thunk); a function applied to it in an inactive context is not called, so a function that ignores the argument still yields the deferred value, where upstream computes the real one. A strict reader forces it and raises the error again (in `'extended` mode an inactive one rejects, C22). Added in review round 1-2 (engine `lazy-value`, `lift/apply`, `build-branch!`; exact `forward-run`).

**Kept as upstream, with a comment.**
- The `'upstream` reactivation bias (M:1159-1165), explained by citing §6.8.
- The categorical duplicate-key asymmetry (D:98).
- Beta raises an error outside [0,1] (D:331-337).
- An inactive Observed node with −∞ LL gives NaN on deactivation.
- NaN Δ draws `u` and rejects.
- `initial_step` returns `[]` when retries run out (M:868).

**Paper errata, recorded in the docs only.**
- P p.14 l.695 says the LL of x = 0.1 under N(0,1) is −0.616; it is −0.9239385332046727.
- P l.360 writes `dist gamma (1/3)` for `gamma 3 (1/3)` (E:144).
- P l.655-658 is missing `dist`.
- P l.261 says `diracN` "has the alias return": in the code `return` creates no node (S:197, 211-214), and the two are equal only by the unit laws.

---

## 9. Test plan

**Conventions.**
- Seed is 17 unless stated, semantics `'upstream`.
- Assertions:
  - "exact" means `equal?` on counts or lists, or `=` on flonums;
  - "tol t" means `(⊦ (equal-approx? t) expected got)` (aux.unittest.scm:135-148);
  - "raises" means `(⊦raises (exn hakaru10) …)`.
- All test models are written with `let*` (C18) and flonum literals wherever Haskell has `Double`, for example `(- 1. x)`. Grass's `nor` must keep the Haskell order `1 - (nnot a x * nnot b y) * (1 - noise)`, with `(1 - strengthX)` computed first.
- **Replay** values were reproduced bit-exactly by Design 1's prototype; items marked ✓ were re-run by a judge.
- Statistical tolerances are about 4–5σ, inflated for autocorrelation. `HAKARU10_SEEDS=17,1,2,…` (opt-in, `make test-hakaru10-long`) re-runs the statistical suite over several seeds.

### 9.1 `hakaru10-random.scm`

| Id | Check | Expected | Kind | Source |
|---|---|---|---|---|
| R1 | `(mkStdGen 17)` | s1 18, s2 1 | exact | R `mkStdGen32` |
| R2 | 3× `stdgen-next` from `(mkStdGen 17)` | 679560, 1394521907, 656733876; generator after = (612660577, 2103410263) | exact | R `stdNext` |
| R3 | first `(rng-double! src 0. 1.)`, seed 17 | 0.2938995850243806 | exact | R `randomRFloating` |
| R4 | first `(rng-integer! src 0 9)`, seed 17 | 1 | exact | R `randomIvalInteger` |
| R5 | fixnum `stdgen-double` vs `stdgen-double/reference`, 100000 draws, seeds 17, 0, 1, 2147483647, −5 | identical | exact | §4.1 |
| R6 | `rng-source-copy` replays 1000 draws | identical | exact | |
| R7 | 10^5 draws stay in `[lo,hi]`; swapped bounds handled | | exact | R |
| R8 | `iLogBase`: k = 2^64 gives n = 3; k = 10 gives 1 | | exact | R |
| R9 (WP12) | MRG32k3a all-12345 state: 0.127011122046577, 0.318527565396794, 0.30918601558327 | freeze only after checking against L'Ecuyer's C reference | tol 1e-15 | L'Ecuyer 1999 |
| R10 (WP12) | chicken-random adapter rejects an all-zero seed | raises | | core bug A (§12) |

### 9.2 `hakaru10-distribution.scm`

| Id | Check | Expected | Kind | Source |
|---|---|---|---|---|
| D1 | normal 0 1 log-density at 0.1 | −0.9239385332046727 | tol 1e-15 | D:155-162; erratum P l.695 |
| D2 | bern 0.4: LL(#t), LL(#f) | −0.916290731874155, −0.5108256237659907 | tol 1e-15 | D:53-62 |
| D3 | uniform 10 20 at 15, 10, 20, 9.99 | −2.3025850929940455 (×3), −∞ | exact | D:107-117 |
| D4 | log-gamma at 0.3, 5.0, 7.5, 20.0 | 1.0957979947868837, 3.1780538330716963, 7.534364243376615, 39.33988418719949 (prototype) | tol 1e-14 | D:212-272 |
| D4b | log-gamma vs `libm-lgamma` at 0.3, 1.5, 3.9, 5, 7.5, 11.9, 20, 1e7 | agree | tol 1e-8 | D:212-219 |
| D4c | log-gamma at 0 and −1 | +∞ | exact | D:222 |
| D5 | corrected gamma(3, 1/3) at x = 1 | −0.397310314667487 (AS 245) | tol 1e-12 | deviation 2 |
| | closed form | −0.3973103145556162 | tol 1e-9 | |
| | x < 0 | −∞ | exact | |
| | shape 0 | −∞ | exact | D:305 |
| D6 | poisson 1 at k = 0..3 | −1, −1, −1.6931471805599454, −2.791759467549525 | tol 1e-12 | D:361-367 |
| | λ = 0: k = 0, k = 1 | −0.0, −∞ | exact | |
| | `(poisson -1)` | raises | | D:356 |
| D7 | categorical `'((a . 1) (b . 3))` | LL(a) = log .25, LL(b) = log .75, LL(c) = −∞ | tol 1e-15 | D:87-98 |
| | duplicate keys | density uses the first match | exact | |
| D8 | uniformly `'()` | raises "uniformly: empty list" | | D:77 |
| | uniformly `'(a b c)` | member −log 3, non-member −∞ | | |
| D9 | beta(2,2) at 0.5 | log 1.5 | tol 1e-9 | D:330-340 |
| | x = 1.5, a = 0 | raises | | |
| D10 | Σ exp ll over the support: bern .3, uniformly, categorical | 1 | tol 1e-12 | |
| | poisson 3.5, k ≤ 60 | 1 | tol 1e-9 | |
| | trapezoid ∫ of normal(1,2), corrected gamma(3,1/3), beta(2,5) | 1 | tol 1e-4 | |
| D11 | sampler moments, N = 20000 via `distribution-sample`: normal(10,.5) | mean 10 ±.03, var .25 ±.02 | | D:119-166 |
| | gamma(3,1/3) | mean 1 ±.03, var 1/3 ±.03 | | |
| | **gamma(0.5, 2)** | mean 1 ±.05, var 2 ±.25 (upstream's double `scl` would give mean 2) | | deviation 3 |
| | beta(2,5) | mean 2/7 ±.01 | | |
| | beta(.5,.5) (Jöhnk) | mean .5 ±.02, var .125 ±.01 | | |
| | poisson 3.5 | mean ±.06, var ±.15 | | |
| | uniform 10 20 | mean 15 ±.1 | | |
| | bern .3 | freq ±.02 | | |
| D12 | poisson sample LL = log-density of the sampled k | equal | exact | deviation 4 |
| D13 | normal sd 0: sampling raises "normal: invalid parameters"; gamma shape −1 sampling raises | | | D:146, D:277 |
| D14 | bern/flip resample from #t | #f, ll(#f), fb 0., source state unchanged | exact | D:64-70 |
| D15 | condition on a distk | kind `observed`, obs stored; `(condition 1. normal)` applied to (0 1) is observed | | M:434-435, S:180-193 |
| | `(condition #t (condition #f (bern .5)))` | raises | | P l.397-402 |
| | condition on diracS | raises | | |

### 9.3 `hakaru10-util.scm`

| Id | Check | Expected | Source |
|---|---|---|---|
| U1 | `(hist '(#t #f #t 2 1 2))` | `((#f . 1) (#t . 2) (1 . 1) (2 . 2))` | U:11-12 |
| U2 | `(momenta '(1. 2. 3. 4.))` | `(2.5 . 1.6666666666666667)` exact | U:16-22 |
| U3 | `(tabulate .5 '(0.1 0.2 0.7 -0.1))` | `((-0.5 . .25) (0. . .5) (0.5 . .25))` | U:40-45 |
| U4 | `tabulate/bins` step rounding to a power of 10 | | U:28-37 |
| U5 | `kl` of a histogram with itself is 0; a missing bin raises | | U:51-56 |
| U6 | `plot` to a temporary file writes the `x  y` header and sorted lines | | U:58-65 |

### 9.4 `hakaru10.scm`

**Suite `hakaru10-language`.**

| Id | Model / check | Expected | Source |
|---|---|---|---|
| L1 | `model-graph` grass | nodes 3, resampleable 2, observed 1, dirac 0 | P §4.1 Fig. 1 l.721-736; E:299-304 |
| L2 | `model-graph` Bayesian regression, k = 5, N = 500 synthetic (B1:106-114 helpers; P §3 l.461-504) | nodes 512, resampleable 11, observed 500, dirac 1 ("511 vertices" plus the result) | P l.495-497 |
| L3 | `model-graph` colored balls (maxBalls 8, 10 × `'blue`) | nodes 2114, resampleable 89 (exact); inactive 1994 (replay at seed 17) | CB:85-101 |
| L4 | `diracN` of an nref | the same address; `diracN` of a Val creates a node; `lift` deduplicates leaves | S:164-169 |
| L5 | raises: condition on an SExp; condition twice; dist outside a model; dist inside a lift fn (construction, and during `mcmC`); a model as a dist argument; non-boolean `if_` test; nref from a previous `mcmC`; branch escape via `set!`; ctor kind change; `memo/finite` unknown key; `ifnode!` with a sampleable entry (engine API) | all raise | S:38-51; E:180-201; M:643-647 |
| L6 | `(mcmC 5 (model 3))` | `'(3 3 3 3 3)` | deviation 7 |
| L7 | `(mcmC 10 (model (if_ #f (dist normal 0 1) 3)))` | ten 3s, terminates | deviation 6 |
| L8 | `mcmC§` | the first 100 forced elements equal `(mcmC 100 m)` | M:897-906 |
| L9 | `mcmC-fold` | equals `(fold …)` over `mcmC` | |

**Suite `hakaru10-replay`** (upstream semantics, seed 17, exact).

| Id | Model | N | Expected | Source |
|---|---|---|---|---|
| X1 | `pdep_run`: `(equal? (mcmC 100 (model (dist normal 11. .5))) (mcmC 100 pdep1))` | 100 | `#t` ✓ | E:16-22; P §2.1 l.231-249 |
| X2 | pdepR, pdepR' vs pnorm | 100 | `equal?` sequences | E:24-32; S:164-169 |
| X3 | grass (rain, sprinkler, `(dist (condition #t (nor .9 .8 .1)) rain sprinkler)`) | 20000 | 9363 `#t` (0.46815) ✓ | E:299-322; P §1 l.32-89, §2.3 l.438-451 |
| X4 | pcond1 | 10000 | `((10 . 2481) (11 . 2394) (20 . 2565) (21 . 2560))` ✓ | E:106-114 |
| X5 | pcond2 | 10000 | `((1 . 5000) (20 . 2321) (21 . 2679))` ✓ | E:116-124 |
| X6 | pcond3 | 10000 | `((1 . 5040) (20 . 2509) (21 . 2451))` ✓ ("new") | E:126-136 |
| X7 | pcond4 | 1000 | `((1 . 648) (20 . 174) (21 . 178))` ✓ | E:265-279 |
| | | 10000 | `((1 . 6697) (20 . 1641) (21 . 1662))` ✓ | |
| X8 | `(hist (mcmC 1000 (pcorr .1)))` | 1000 | `((#f . 419) (#t . 581))` ✓ | E:223-238 |
| X9 | girl | 100 | 77 `#t` ✓ | E:340-355 |
| X10 | post_bias 1 | 10 | all `#t` ✓ | E:256-263; P l.427-437 |
| X11 | `(momenta (drop (mcmC 10000 (biased-coin #t #f)) 1000))` | 10000 | `(0.49910027237934973 . 0.0497358737659099)` ✓ | E:174-210; P l.375-392 |
| X12 | KL(`tabulate .5` of `mcmC 5000 mixng`, `tabulate .5` of `(append (mcmC 2500 (model (dist normal 10 2))) (mcmC 2500 (model (dist gamma 3 (/ 1. 3.)))))`) | | 1.3896442111895212e-2 ✓ | E:159-164 |
| X13 | `(momenta (mcmC 1000 (phierN 10)))` | 1000 | `(3.4117556376053577 . 32.27027981263249)` ✓ | BE:14-24 |
| X14 | `cbr`: `(hist (mcmC 10000 (cballs-model (make-list 10 'blue))))` | 10000 | `((1 . 4265) (2 . 2203) (3 . 1244) (4 . 805) (5 . 486) (6 . 291) (7 . 565) (8 . 141))` | CB:101-103 |
| X15 | bern/flip variants: pbern | 10 | 4 `#t` ✓ | E:10-12; P p.5 l.217-218 |
| | ph2 | 10000 | 5809 ✓ | E:88-94 |
| | ph21 | 10000 | 2026 | E:96-102 |
| | KL(mixng, mixng'), 5000 each | | 1.1301558326703463e-2 ✓ | E:153-157 |
| X16 | tac1, tac2, tac3 | 1000 | identical sequences; 505 `#t` (prototype regression) | S:340-371 |

Notes on the replay suite:
- X14 was reported by the prototype but not re-run by a judge. If it alone fails, check node creation order (the `fold-right` chains, C18) before suspecting the engine.
- X16: upstream's 620/523/532 come from an older engine revision; cite S:346-368 in the doc.
- Documented but **not asserted** (doc text only):
  - phier_kl 0.7863/0.2362/0.03442/0.002157 (E:76-85; the prototype gives 0.78688/0.23811);
  - Bench 1e4 (BE:27-28);
  - niceex1 "All True" (S:288-289);
  - niceexmcr11/12 (S:297-301);
  - the older mixng KLs 2.276e-2 and 1.356e-2 (E:153-162).

**Suite `hakaru10-laws`** (P §2.1, §4.3).

| Id | Check | Expected | Source |
|---|---|---|---|
| A1 | left unit: `(let* ((x (diracN 5.))) (dist normal x 1.))` vs `(dist normal 5. 1.)`, 200 | `equal?` | P l.259-261 |
| A2 | diractest port: `x ~ bern .5`, result `(diracN (lift identity x))`, 10000 | P(#t) .5 ±.03 | M:1029-1037 |
| A3 | p2: `x ~ uniform 0 1`, result `(diracN (lift identity x))`, 10000 | ≥ 9990 distinct values (independence proposal always accepted) | P §4.3 l.812-825 |
| A4 | pmarg ≡ pnorm in distribution, 10000 | mean 10 ±.05, var .25 ±.05 | E:44-48; P l.273-281 |
| A5 | pjoin, 10000 | P(car) .4 ±.03, \|corr\| < .05 | E:36-42; P l.264-272 |
| A6 | `e >>= diracN` inside branches | covered by X16 | S:370-371 |

**Suite `hakaru10-statistics`** (seed 17; also in the seed sweep).

| Id | Model | N (burn-in) | Expected ± tol | Source |
|---|---|---|---|---|
| S1 | pbern (default kernel) | 10000 | .4 ±.025 | E:10-12 |
| S2 | pnorm | 10000 | mean 10 ±.03, var .25 ±.03 | E:14 |
| S3 | ph1 | 20000 | mean 0 ±.3, var 10 ±1.5 | E:65-67 |
| S4 | phierN 3 | 20000 | mean 0 ±.6, var 28 ±6 | E:56-57, BE:14-15 |
| S5 | phierN 10, KL against erf bins of N(0, √91) (step .5) | 10000 | < 0.35 (upstream 0.236, E:80) | E:69-85; P l.301-327 |
| S6 | ph2 / ph21 (default kernel) | 10000 | .58 / .2 ±.03 | E:88-102 |
| S7 | pcond1 | 10000 | .25 each ±.03 | E:106-114 |
| | pcond2, pcond3 | 10000 | {1 .5, 20 .25, 21 .25} ±.03 | E:116-136 |
| | pcond4 | 10000 | {1 2/3, 20 1/6, 21 1/6} ±.03 | E:265-282 |
| S8 | mixng, mixng' | 10000 | mean 5.5 ±.4, P(x>5) .5 ±.04 | E:139-151; P §2.2 l.331-366 |
| S9 | biased-coin #t #f / #f #f | 10000 (1000) | (.5 ±.02 . .05 ±.008) / (.25 ±.02 . .0375 ±.008) | E:174-221; P l.375-392 |
| S10 | nice_prog_mult_conditions #t #f / #f #f, sum/1000 | 1000 | .5 ±.05 / .25 ±.05 (upstream 499.8 / 257.29) | S:291-301 |
| S11 | niceex1 | 10000 | .25 ±.03 | S:282-289 |
| S12 | prog_mixture1 −2, tac10, tac11 | 100 | all `#t` | S:303-310, 373-397; M:882-892 |
| S13 | tac1 | 20000 | .5 ±.05 | S:340-348 |
| S14 | exif1 | 20000 | mean 13.5 ±.8, P(>10) .5 ±.05 | S:314-321 |
| | exif2 | 20000 | P(=10) .25 ±.04, P(=20) .25 ±.04, P(≤2) .5 ±.04 | S:323-337 |
| S15 | pcorr returning `(pair x y)` | 20000 | P(x=y) .9 ±.03, P(y) .5 ±.06 | E:223-238 |
| S16 | condhier | 20000 | mean 1/82 ±.15, var 81/82 ±.2 | E:242-251 |
| S17 | grass / girl | 20000 / 10000 | .4684714 ±.02 / .6644295 ±.03 | E:284-355 |
| S18 | mhex | 20000 | mean .345731 ±.1 | E:357-362; P §4 l.677-720 |
| S19 | post_bias 1 | 100 (drop 20) | all `#t` | E:256-263 |
| S20 | **if/shared-inactive**: `c ~ bern .5; d ~ normal 0 1; (if_ c d (dist normal 5. 1.))` | 2000 | no error; P(<2.5) .5 ±.05 | deviation 1, M:788-792 |

**Suite `hakaru10-semantics`.**

| Id | Check | Expected | Source |
|---|---|---|---|
| B1 | pswitch, `'upstream`, 40000 | P(c) .3773 ±.03 (documents the bias; exact kernel value 0.377286) | M:298-303, 767-775, 1159-1165 |
| B2 | pswitch, `'extended`, 40000 | .5 ±.02 | §6.8 |
| B3 | pcond1-4, mixng, colored balls (10000), `'extended` | same expectations as S7, S8, C1 | §6.8 |
| B4 | `check: 300` on phierN 3, mhex, exif2, pcond4, small colored balls, pswitch, small regression, both modes | no condition raised | §6.9 |
| B5 | extended mode, inactive node with an invalid parameter: `x ~ normal 0 1; (if_ (lift positive? x) (dist gamma x 1.) 0.)` | no error (C22); P(x>0) < .5 documented, not asserted | C22 |

### 9.5 `hakaru10-exact.scm` (suite `hakaru10-exact`)

| Id | Model | Expected | Kind | Source |
|---|---|---|---|---|
| E1 | `exact-posterior` grass | `((#f . 0.5315285572796303) (#t . 0.4684714427203697))` | tol 1e-12 | E:321-322 |
| E2 | girl | P(#t) 0.6644295302013423 | tol 1e-12 | E:340-355 |
| E3 | pcorr (pair) | P(x=y) .9, P(y) .5 | tol 1e-12 | E:223-238 |
| E4 | ph2 / ph21 | .58 / .2 | tol 1e-12 | E:88-102 |
| E5 | pcond1 / pcond2 / pcond4 | .25×4 / (.5 .25 .25) / (2/3 1/6 1/6) | tol 1e-12 | E:104-282 |
| E5b | pcond3 | raises `support` (an active normal) | | |
| E6 | post_bias 1 | P(#t) > 1 − 1e-12 | | E:256-263 |
| E7 | colored balls, maxBalls 3, 4 × `'blue` | (.45765164162493044 .29838619922092374 .24396215915414585) | tol 1e-12 | CB:85-101; closed form Σ_k C(n,k)2^-n p_k^4 |
| E8 | pswitch | P(c) .5 | tol 1e-12 | §6.8 |
| E9 | Hansei cross-check: grass, girl, pcond4 through `probcc-reify/exact` (as test/hansei.scm:87-154) | agrees with E1, E2, E5 | tol 1e-12 | W:111 (Hansei) |
| E10 | `kernel-residual` (`'extended`): grass, girl, pcorr, pcond1, pcond2, pcond4, post_bias, small colored balls, pswitch | < 1e-12 | | §6.11 |

The doc also records the full 8-ball closed form (not computable by enumeration):
`.4119636809525704 .2097286247545656 .12069197667405333 .08018476851324365 .05903181664195371 .04660379073990803 .038629862100510455 .03316547962319476`, together with Hansei's numbers (CB:110-147).

### 9.6 `hakaru10-examples.scm` (suite `hakaru10-examples`)

| Id | Check | Expected | Source |
|---|---|---|---|
| C1 | colored balls (8, 10 × `'blue`), 20000 | P(1) .412 ±.06; P(1) > P(2) > P(3) | CB:85-103; P §3 l.505-616 |
| C2 | birds `neighbors` on 4×4 | all 16 cells for every n | B1:87-93 |
| | `neighbors` 1 on 10×10 | 20 cells | |
| | column-major `divMod (n-1) 4`: 1 → (0,0), 4 → (0,3), 13 → (3,0), 16 → (3,3) | | BMP:106-122 |
| C3 | θ weights normalize through `categorical`; `birdpos` (unique positive cell, else `#f`) | | B1:77-80, 95-99 |
| | Poisson LL at λ ∈ {0,1} | per D:361-363 | |
| C4 | synthetic birds: gridDim 4, 2 years × 6 days, features from an rng-source seeded 17 with f4 = 2 on the diagonal (BMP:142-177), β_true = (1 2 .5 1); path simulated, observations = indicator counts; model as B1:50-74 with `(lift abs (dist normal 0. 10.))` priors; `mcmC-fold` 3000 | four finite, non-negative component means; finite initial LL | B1:50-74; W:122-168 |
| C5 | Bayesian regression: k = 2, 20 points from w = (1.5 −0.7), fixed xs, noise sd .5, priors as P l.476-504; 20000 (2000 burn-in) | posterior means within .4 of w | P §3 l.461-504; B1:106-114 |

### 9.7 Not in `make test`

**`hakaru10-bench.scm`** is compiled with `csc -O3`. It runs `./hakaru10-bench n nchain` and prints `(momenta (mcmC nchain (phierN n)))` plus elapsed milliseconds (BE:1-21). Reference values in comments:
- 1e3: exactly (3.4117556376053577, 32.27027981263249);
- 1e4: prototype (−3.151835923727823, 54.168535367816084), recorded (−3.1514, 54.18);
- BE:23-38 table, and P §3.1 l.617-640 (1e6: mean .22, var 93, 20 s).

**`hakaru10-birds.scm`** runs only when `HAKARU10_BIRDS_DIR` names a folder containing `onebird-observations.csv` and `onebird_features.csv` (B1:141-189).
- The CSV reader uses `(chicken string)` `string-split`; upstream's ReadCSV module and the data files are missing.
- It reports `main1 1000 n` and compares informationally with [4.3697, 6.5965, 5.7809, 6.5930] (B1:219-227).
- BMP:179-200's numbers are only a format example and must never be asserted.

---

## 10. Doc sections, in the style of `test/hansei.scm:9-57`

Each suite has `((doc r) …)` returning SXML with `structure/section`, `p`, `cite/a`, `cite/quote`, `code/scheme`, `code/lang "haskell" "…"`, and `displaymath`/`m` (aux.sxml.scm:82-215). Each test case ends with `` `(doc …) ``, showing:
- the Scheme model (`code/scheme`);
- the Haskell original (`code/lang "haskell"`, quoted verbatim from the cited lines);
- the citation and the expected value with its provenance (upstream record, exact, or prototype regression).

**`hakaru10.scm` (main doc).**
1. **Introduction.** `cite/a` W ("Embedded probabilistic domain-specific language Hakaru10"); `cite/quote "Oleg Kiselyov"` of the abstract (P l.8-23); `cite/a` the DOI and `design.pdf`. Explain that this is a port of the November 2016 code.
2. **Hakaru10 by example.** P §2 l.189-200, §2.1 l.202-327: pbern, pnorm, pdep1, pdepR, pjoin, pmarg, phier, with the laws (P l.250-281) as `displaymath`.
3. **Branching models.** P §2.2 l.328-366 (mixng, mixng', Hur et al. Fig. 2).
4. **Conditioning.** P §2.3 l.367-451: why one conditions on distributions (P l.393-398, §4 l.653-673), the static discipline turned into run-time checks (§7 table), grass (P l.438-451, W:80-95).
5. **Implementation.** §4.1 trace DAG (l.721-764); §4.2 incremental recomputation and creation order as topological order (l.765-800); §4.3 Dirac (l.801-843); §4.4 branching with inactivation counts (l.844-869). Map each to the Scheme procedures.
6. **Embedding notes.** Models are thunks; `let*` and evaluation order (C18); `condition` vs `chicken.condition#condition`; `memo/finite` (footnote 5).
7. **Findings and deviations.** The §8 register with file:line cites; the pswitch bias and the extended semantics (§6.8, M:1145-1168); the paper errata.
8. **Related work.** P §5 l.870-902 (Wingate et al. [21]; Hur et al. [10, 11]; Ścibior et al. [20]; the original Hakaru [24]).

**Other files.**
- `hakaru10-random.scm`: random-1.0.1.1 (`cite/a` R), why StdGen is ported (exact replay), the CHICKEN generator caveats (§12).
- `hakaru10-distribution.scm`: D:14-41 interface, D:93-96 normalization note, Marsaglia–Tsang, Jöhnk, Knuth, AS 245.
- `hakaru10-exact.scm`: oracle method, kernel residual, Hansei (`cite/a` https://okmij.org/ftp/kakuritu/Hansei.html).
- `hakaru10-examples.scm`: P §3 (l.452-616); BLOG chapter Ex 1.1 (`cite/a` http://people.csail.mit.edu/milch/papers/blog-chapter.pdf, CB:1-13); W `#colored-balls` (l.110-119); bird migration (W `#bird-migration` l.120-177, BMP:25-79, 103-200, 430-470).

---

## 11. Registration and running

**`src/aux.egg`**, after `(extension aux.hansei)` (line 59), in this order:

```
(extension aux.hakaru10.random)
(extension aux.hakaru10.distribution (component-dependencies aux.hakaru10.random))
(extension aux.hakaru10.util)
(extension aux.hakaru10.engine (component-dependencies aux.base aux.hakaru10.random aux.hakaru10.distribution))
(extension aux.hakaru10.exact (component-dependencies aux.hakaru10.engine))
(extension aux.hakaru10 (component-dependencies aux.base aux.hakaru10.engine aux.hakaru10.exact aux.hakaru10.util))
```

There are no new egg dependencies: srfi-1, srfi-69 and the srfi-4 core module are already available. The tests also use `aux.hansei` and `aux.unittest`.

**`src/Makefile`.**
- In `test:`, after `cd test && ${CSI} -s hansei.scm` (line 30): `hakaru10-random.scm`, `hakaru10-distribution.scm`, `hakaru10-util.scm`, `hakaru10.scm`, `hakaru10-exact.scm`, `hakaru10-examples.scm`.
- New targets:
  - `test-hakaru10-long:` runs `HAKARU10_SEEDS=17,1,2,3,4 csi -s hakaru10.scm` and `csi -s hakaru10-birds.scm`;
  - `bench-hakaru10:` runs `cd test && ${CSC} -O3 hakaru10-bench.scm && ./hakaru10-bench 10 10000`.
- `scheme-indent` lines for the six sources (next to line 148) and the eight test files (next to line 128).

**Running before the egg installs.** `chicken-install` is blocked here (see the memory note `aux-egg-build-blockers`).

```
R=<scratchpad>/h10repo; mkdir -p $R; cd src
for m in aux.base aux.sxml aux.unittest aux.continuation aux.continuation.delimited aux.hansei \
         aux.hakaru10.random aux.hakaru10.distribution aux.hakaru10.util aux.hakaru10.engine \
         aux.hakaru10.exact aux.hakaru10; do
  CSC_OPTIONS=-I/home/mnocentini/.local/include csc -s -J -O3 -d1 $m.scm -o $R/$m.so
  csc -s $(echo $m).import.scm -o $R/$m.import.so
done
cd test && CHICKEN_REPOSITORY_PATH=$R:/home/mnocentini/.local/lib/chicken/12 csi -s hakaru10.scm
```

Adjust the module list to whatever `aux.hansei`'s imports need. Generated `.import.scm` files and `testsuite-*.html` reports must not be committed.

**aux commits.** One commit per work package. Imperative subjects. End each message with the session's `Co-Authored-By:` and `Claude-Session:` trailers.

---

## 12. chicken-core work item (separate, optional; aux never depends on it)

**Rules.**
- Work in a clone, never in place: core's `config.make` is generated for `PREFIX=/home/mnocentini/.local`.
- Never install into `/home/mnocentini/.local`.
- The aux module stays pure Scheme on the installed 6.0.1pre1. There is no `cond-expand` on these changes in this project.

```
S=<scratchpad>; git clone /data/bitcoin/mnocentini-home/Developer/working-copies/chicken-core $S/cc
cd $S/cc && ./configure --prefix $S/prefix --chicken /home/mnocentini/.local/bin/chicken --c-compiler clang
env -u LD_LIBRARY_PATH -u CHICKEN_REPOSITORY_PATH make -j && make check && make install   # into $S/prefix only
# after editing c-platform.scm / lfa2.scm / types.db: rebuild with make CHICKEN=./chicken (README:131-151)
```

Take baselines under `env -u LD_LIBRARY_PATH -u CHICKEN_REPOSITORY_PATH` (IMPLEMENTATION-NOTES.md §11.7, l.2356-2373).

**Conventions.**
- Branch off `main`; one item per commit or branch.
- Each commit touches code, `types.db`, `manual/`, `NEWS` (6.1.0 section) and `tests/*.scm` with `runtests.sh` and `runtests-wasm.sh`, plus `distribution/manifest` for new files.
- Add an IMPLEMENTATION-NOTES §11.N entry with the commit hash and measurements.

| Prio | Item | Change | Test |
|---|---|---|---|
| 1 | **F. lfa2 typos block unboxing of `fpexpt`** (helps beta and gamma samplers) | `lfa2.scm:235` `"C_a_i_flonum_expr"` → `…_expt` (defined at chicken.h:1704); `lfa2.scm:220` `"C_u_i_flonum_finitepp"` → `…_finitep` (both since 79cf7427) | Compile `(let* ((a (fp* x 2.)) (b (fpexp a)) (c (fpexpt b 1.5))) …)` at `-O3 -debug o`; assert the unboxed-replacement count, or grep the C for `C_pow` on doubles; style of `tests/bytevector-guard-tests.scm` |
| 2 | **A. All-zero seed hangs `pseudo-random-integer`** | In `C_set_random_seed` (runtime.c:14298), expand the seed through SplitMix64 and never leave an all-zero state | `tests/random-tests.scm`: `(set-pseudo-random-seed! (make-bytevector 16 0))`, then `(pseudo-random-integer 100)` returns; the stream is not constant |
| 2 | **B. Seed shorter than one word** | runtime.c:14300 `nsu = 0` reads past the buffer; trailing `n mod 8` bytes are ignored. Handle partial words. Same commit as A. | Seeds of length 1..9 are deterministic and distinct |
| 3 | **E. Vacuous assert** | `tests/random-tests.scm:18` → `(assert (equal? n1 (nums 100)))` | Self |
| 4 | **C. Time-based startup seed** | runtime.c:892-898: seed from `C_random_bytes` unless `-:R` is given | Three concurrent `csi` give different first draws; `-:R` stays reproducible |
| 5 | **D. `pseudo-random-real` allocation on 32-bit/wasm** | extras.scm:459 `("C_a_i_random_real" 2)` → use the flonum word count (`words-per-flonum`, c-platform.scm:87) | wasm32 or 32-bit run if available; otherwise mark it analysis-only |
| 6 | **H. `fplog1p`, `fpexpm1`, `fplgamma`, `fperf`/`fperfc`** (optional `fptgamma`) | The 8-edit recipe of aa16d987/31027474: chicken.h:1007 aliases and :1702-1706 macros; library.scm:2304-2318 definitions (replacing the hidden `fllog1+` at :2842, :42); c-platform.scm:157-159 and :953-956 rewrites; types.db near :1324/:1336; lfa2.scm :181-185 and :234-236; manual; NEWS. **After F** (both edit lfa2.scm). | `(assert (fp= (fplog1p 1e-17) 1e-17))`, `(fplgamma 5.0)` ≈ `(log 24.)`, interpreted and `-specialize`d |
| 7 | **I. SRFI-27-style random-state objects** (optional) | Per-instance WELL512 state in a bytevector; `make-pseudo-random-state`, `-copy`, `-split`; optional STATE argument. **After A/B/C** (runtime.c, extras.scm). | Determinism from an integer seed, copy and replay, independence of a split |
| 8 | **G. Hyperbolic rewrites** | `+flonum-bindings+` and `rewrite` for `fpsinh` etc. (c-platform.scm:155-159). Low value. **After H** (same file). | Inlining at `-O2 -unsafe` |

**File conflicts inside core.**
- A, B and C share runtime.c, and also extras.scm and random-tests.scm with E and I: do them sequentially.
- F and H share lfa2.scm; H and G share c-platform.scm.
- F is independent of A/B/C/E.

---

## 13. Work packages

| WP | Content | Files touched (exclusive) | Depends on | Parallel with |
|---|---|---|---|---|
| **WP0** | Scaffolding: the six empty module skeletons (header comments, imports, `(module … *)`), the eight empty test files with a `doc` stub, aux.egg and Makefile entries (§11) | `aux.egg`, `Makefile`, the new files (stubs only) | none | none (do first; afterwards nobody else edits aux.egg or Makefile) |
| **WP1** | random module (§4.1, §6.1) and R1-R8 | `aux.hakaru10.random.scm`, `test/hakaru10-random.scm` | WP0 | WP2, WP3, WP13 |
| **WP2** | distribution module (§4.2, §6.2) with log-gamma, distk, condition and errors; D1-D15 | `aux.hakaru10.distribution.scm`, `test/hakaru10-distribution.scm` | WP0; codes against the WP1 protocol in §4.1, tests need WP1 | WP1, WP3, WP13 |
| **WP2b** (optional) | laplace and dirichlet/symmetric extras (D:383-411) | the distribution files | WP2 | WP4+ |
| **WP3** | util module (§6.12) and U1-U6 | `aux.hakaru10.util.scm`, `test/hakaru10-util.scm` | WP0 | WP1, WP2, WP4, WP13 |
| **WP4a** | engine without branches: memory, chain, journal, heap, SExp, construction, propagation, step, driver, owner and pure-context checks | `aux.hakaru10.engine.scm` | WP1, WP2 | WP3 |
| **WP4b** | engine branches: `ifnode!`, (de)activation, update-entry/exit, delayed updates, scope check | same file | WP4a | WP5 (after the WP4a API freeze) |
| **WP4c** | `'extended` semantics, C22, step hook | same file | WP4b | WP5, WP6 |
| **WP5** | public module: syntax, `dist`/`diracN`/`condition`/`if_`/`lift`/…, `mcmC*`, `model-graph`, `memo/finite`, `check:` wiring, re-exports | `aux.hakaru10.scm` | WP4a API (§4.4) | WP4b, WP4c, WP6 |
| **WP6** | exact module: reference-check, exact-posterior, load-state, kernel-residual | `aux.hakaru10.exact.scm` | WP4b API | WP4c, WP5 |
| **WP7** | main tests: language, replay, laws, statistics, semantics, plus the main doc (§10) | `test/hakaru10.scm` | WP5 (and WP4c for B1-B5) | WP8, WP9 |
| **WP8** | exact tests E1-E10 and doc | `test/hakaru10-exact.scm` | WP5, WP6 | WP7, WP9 |
| **WP9** | examples C1-C5, bench, birds-real, and their docs | `test/hakaru10-examples.scm`, `test/hakaru10-bench.scm`, `test/hakaru10-birds.scm` | WP5 | WP7, WP8 |
| **WP10** | Citation pass: every definition has a `cf.` comment, every deviation is tagged and numbered as in §8; `scheme-indent` all files | all hakaru10 files | WP1-WP9 | none (last) |
| **WP12** (optional) | MRG32k3a and `(chicken random)` sources; R9-R10 | random files | WP1 | WP4+ |
| **WP13** | chicken-core items (§12) | chicken-core clone only | none | everything in aux |

**Recommended sequence.** WP0 → {WP1, WP2, WP3, WP13} → WP4a → {WP4b, WP5} → {WP4c, WP6} → {WP7, WP8, WP9} → WP10.

**Validation milestones.** Within WP4a, use the prototype-backed replays as soon as the driver runs (via a temporary harness in the scratchpad, not in the repo): X1, X2, X11, X13, X8, X9. Within WP4b: X3-X7, X10, X12, X14-X16.

**Replay-failure triage order.**
1. RNG (R2-R5).
2. Draw order in the samplers (gamma `until`, Marsaglia retries, categorical walk).
3. The `samp` order (reversed creation list, M:239, M:962).
4. Float evaluation order.
5. Node creation order (C18).
6. Deactivation and activation summation order (M:696-751).

---

## 14. Residual risks and open items

- **X14 (cbr) and the inactive count of 1994 (L3)** come only from the prototype. If they fail while the other replays pass, check creation order first.
- **libm.** Replay assumes the libm used by GHC and by the prototype gives `log`/`exp`/`pow`/`sqrt` results identical to the libm CHICKEN links here. The Python prototype used this machine's libm, so it is expected to hold. If a single flonum replay drifts by an ulp, demote that test to tol 1e-12 and record the reason; never loosen count-based replays silently.
- **Runtime of `make test`.** About 140k replay steps plus about 300k statistical steps, compiled engine with interpreted model closures. Target: under 2 minutes. If it is slower, move S4, S5 and C1 to `test-hakaru10-long`.
- **Upstream folder.** Committing `upstream/okmij-hakaru10/` (no licence stated, third-party PDFs) is the user's decision.
- **Upstream sources that are absent.** Bird data, ReadCSV and the ground truth (B1:11, 141-189) are missing. Only synthetic tests are possible by default.