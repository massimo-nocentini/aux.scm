# PLAN-IDIOM: amendment to AUX-HAKARU10-PLAN.md (native shift/reset, aux abstractions, Scheme idiom)

> **Superseded on 2026-10-13** by AUX-HAKARU10-PLAN.md §15 (user decision: State-monad construction, as upstream). C23′ and C30 are restated there; everything else here (C18′, C25-C29, C31, §2-§6) is a historical record of the delimited-control design and no longer describes the code.

This amendment implements the user directive of 2026-10-12:

> "keep in mind to use chicken delim shift and reset from chicken-core and the abstractions in aux modules. please be more scheme-idiomatic as possible"

**Precedence.** Where this file and AUX-HAKARU10-PLAN.md disagree, this file wins. Everything it does not touch stays as in AUX-HAKARU10-PLAN.md, including:
- the `cf.` and `deviation N` citation rule (§0, §8);
- C18 (node-creation order);
- every replay value of §9.

**Abbreviations.**
- `SP` = the scratch directory used while implementing; it is not in the repository.
- `src/` = `aux.scm/src`.
- "the engine" = `src/aux.hakaru10.engine.scm` as of md5 `b772a4fb…` (1230 lines), which already has a first version of the effect protocol: `perform`, `handle-model`, `trace-builder`, `handle-if`, `effect-distk`.

---

## 0. What was validated before writing this

All of the following were run, nothing in `src/` was edited.

| What | Where | Result |
|---|---|---|
| Native `shift`/`reset` protocol micro-probe: a trampolined deep handler with a return clause; `if` answering a thunk; multi-shot enumeration into Hansei trees explored by `probcc-explore`/`probcc-normalize`; a one-shot weighted handler; `condition-case` across resumptions; `parameterize` re-established on resume | `SP/idiom-probe/p1.scm` (csi and `csc -O3`) | All as expected, interpreted and compiled. One-shot effect round trip: **363 ns** compiled, 3.1 µs interpreted. |
| The engine with the protocol changes of §2 (E-1…E-6 of §4.4) | `SP/idiom-probe/eng/aux.hakaru10.engine.scm`; diff in `SP/idiom-probe/engine-protocol.diff` (100 lines); built into `SP/repos/idiom` | See next row. |
| The replay harnesses re-run on that engine | `SP/idiom-probe/harness/{replay,effects,checks,wp4b,wp4c}.scm` | **0 failures**. Covers X1-X16 bit-exact; L3 graph counts 2114/89/1994; per-step invariants and exact rollbacks on 8 models × 2999 steps; `'extended` Δ checks; C22; B1. Timings unchanged (X14 669 → 697 ms, noise). |
| The exact and forward interpreters written against the §2 API | `SP/idiom-probe/p3.scm`, `p4.scm` | E1, E2, E5, E5b, E6, E7 (colored balls 3 × 4 blue: .45765164162493044 …), E8 pass at 1e-12. Likelihood weighting agrees with exact. `mcmC§` via `τ->§` equals `mcmC`. The **construction-order forward run reproduces the builder's initial trace**: value `equal?`, LL within 1e-9, on grass, pcond3, pcond4, pswitch, nested, colored balls 8 × 10 blue and phierN 10, in both semantics. |

---

## 1. Decisions (new or changed)

| # | Decision | Supersedes |
|---|---|---|
| **C25 Effects** | A model is a thunk that *performs* three operations through the native `shift` of `(chicken continuation)`: `(dist d args)`, `(diracN e)` and `(if test th el)`. An interpretation is a *handler* that `handle-model` installs under a native `reset`. Only the engine imports `(chicken continuation)`; every other module writes handlers against `handle-model`. | the "construction context" wording of PLAN §3.1, §4.4 |
| **C26 Not operations** | `condition`, `lift`, `memo/finite`, `pair`, `collect` and `letlift` are **not** operations. `condition` is a pure transformer of distributions (S:174-193); an observation is the `dist` operation on an *observed* distk, and each handler branches on `distk-kind`. `lift` is pure Applicative lifting (S:99-116); under a value handler every argument is a Val, so it evaluates at once. `memo/finite` is derived: an in-order map over the keys. | |
| **C27 `if` answers a thunk** | `(if_* test th el)` is `((perform 'if test th el))`: the handler resumes the model with *the computation that realizes the branch*, and the model calls it in its own context, under the same `reset`. The trace builder answers `(τ (build-if! …))`, which builds both branches (inactive first) inline. A value handler answers the selected branch thunk. One model run is therefore one `reset`, with no nested `handle-model`. | engine `handle-if`; nested `run-model!` per branch |
| **C28 Multi-shot purity** | A model must not mutate, across an operation, state that outlives it: no `set!` of an outer variable, no hash-table or vector filled between two `dist`s. Exact enumeration resumes continuations many times, and such state is shared between sibling paths. `SP/idiom-probe/p4.scm` shows an impure model answering lengths 2, 3, 5, 6 instead of 2. The builder and the forward sampler (one-shot) are unaffected. Documented in the main doc §6 and in the `exact-posterior` doc; `memo/finite`, `fold`, `map-in-order` and named `let` are safe. | |
| **C29 Static discipline** | The static discipline (§7: observation external, owner, branch escape, kind stability, non-model args) is checked by the **builder**; value handlers see plain values and cannot check it. `exact-posterior` therefore builds the model once first (`#:validate #t`, default), as PLAN §6.10 did. | |
| **C30 Names** | Scheme names everywhere. The Hakaru10 user API keeps its upstream names: `mcmC`, `dist`, `diracN`, `if_`, `condition`. Internal Haskell-isms are renamed (`bernS` → `bern`, `sampleK` → `distk-sample`, `mkStdGen` → `seed->stdgen`, …; §4); the upstream name stays in the `cf.` comment. | C6 (in part), PLAN §4.1-§4.2 names |
| **C23′ `mcmC§`** | `mcmC§` is `(τ->§ next)` from `(aux stream)` over the engine's generator thunk: a `δ` stream compatible with `take§`, `§->list`, `map§`, `car§`/`cdr§`. `(aux hakaru10)` imports `(only (aux stream) τ->§)`, and aux.egg adds `aux.stream` to its dependencies. A native-`shift` `yield` generator was considered and rejected: it adds a `reset`/`shift` per sample to a generator thunk that already exists. | C23 |
| **C18′** | Unchanged and extended: the same model code runs under every handler, so `let*`, `fold`, `map-in-order` and named `let` are required for node-making code. Not `map`: CHICKEN's `##sys#map` is `(cons (p x) (loop …))`, whose argument order is unspecified. Not srfi-1 `list-tabulate`: it calls `proc` from the **last** index down. | C18 |
| **C31 Monads** | `(aux category …)` monads are not used: shift/reset handlers replace the State/MCMCM monad (S:82), and SExp is deliberately not a monad (S:118). | |

---

## 2. Handler architecture

### 2.1 Protocol (engine, section "Effects and handlers")

```scheme
; (new) a request (op arg ...) and the model's continuation up to its handler's reset.
(define-record effect request k)

; (new) perform a request; outside a model or in pure context: `construction` error (deviation 9).
(define (perform . request)
  (unless (current-handler)
    (hakaru10-error 'construction (car request)
                    "called outside model construction or inside a pure function"))
  (shift k (make-effect request k)))

; (new) run the thunk `model` under `handler`, (λ (request resume) ...); `return` maps the
; model's value (inside the reset, outside the model's dynamic context).  A deep handler by
; trampoline: resume runs the model up to its next request and hands it to the handler again,
; so a one-shot handler runs in constant stack and a multi-shot one may call resume many times
; or lazily.  Hansei's probcc-reify (aux.hansei.scm) has this shape over aux's resetcc.
(define (handle-model model handler #!optional (return identity))
  (let run ((r (reset (return (parameterize ((current-handler handler)) (model))))))
    (if (effect? r)
        (handler (effect-request r) (λ (v) (run ((effect-k r) v))))
        r)))
```

**Properties** (verified in p1):
- `current-handler` is `#f` inside handlers and in `pure-call`. The `parameterize` is unwound when the model shifts out and rewound on every resumption.
- `condition-case` outside `handle-model` catches errors raised in a resumed segment or in a handler. A `condition-case` inside the model around a `dist` keeps working.
- `reset` delivers one value, so a handler that needs to return two (value and weight) keeps the second in a variable (§2.4).

**Operations.**

| Operation | Performed by | Request | The resume value the model expects |
|---|---|---|---|
| dist | `(dist d . args)`, after `check-not-model` on the args (S:137-151) | `(dist ,d ,args)`: `d` a distk or a constructor; `args` SExps/Vals | the choice: an nref (builder) or a value |
| diracN | `(diracN e)` (S:161-169) | `(diracN ,e)` | an nref or a value |
| if | `(if_* test th el)` = `((perform 'if test th el))` (S:275-279) | `(if ,test ,th ,el)`: `th`, `el` thunks | **a thunk**, called by the model (C27) |

Handlers dispatch with aux.base's `match/first`. An unknown request is an `internal` error.

**Shared helpers** (engine):
- `(effect-distk d args)`: unchanged.
- `(select-branch test th el)`: replaces `handle-if`. It checks that `(val-of test)` is a boolean (`branch`; deviation 9, C7) and answers `th` or `el`.
- `(value-handler on-dist #!key (on-if select-branch))`: the skeleton of every interpretation in which SExps are plain values:

```scheme
; cf. S:137-151, S:161-169, S:275-279 read over plain values: (dist d args) calls
; (on-dist dk resume) with the request's distk, (diracN e) resumes with e's value,
; (if test th el) resumes with the thunk (on-if test th el).
(define ((value-handler on-dist #!key (on-if select-branch)) request resume)
  (match/first request
    ((dist ,d ,args) (on-dist (effect-distk d args) resume))
    ((diracN ,e) (resume (val-of e)))
    ((if ,test ,th ,el) (resume (on-if test th el)))
    (else (hakaru10-error 'internal 'handle-model "unknown effect" request))))
```

### 2.2 (a) Trace builder (engine; owner: engine WP)

```scheme
; cf. S:82, S:196-197: build ch's trace graph; every request resumes at once.
(define ((trace-builder ch) request resume)
  (resume (match/first request
            ((dist ,d ,args) (build-dist! ch d args))
            ((diracN ,e) (to-node! ch e 'diracN))
            ((if ,test ,th ,el) (τ (build-if! ch test th el)))
            (else (hakaru10-error 'internal 'mcmC "unknown effect" request)))))

; cf. S:196-197 `m >>= to_node`.
(define (run-model! ch model)
  (handle-model model (trace-builder ch) (λ (e) (to-node! ch e))))
```

In `build-if!` the branches run inline, in the model's own segment:

```scheme
(ifnode! ch entry (τ (to-node! ch (th))) (τ (to-node! ch (el))))
```

`ifnode!`, `build-branch!` (scope push/pop), `deactivate!` and the exit creation are unchanged.

**Why C18 and every replay are preserved.** The builder is one-shot and sequential. The trampoline resumes the model exactly once per request, at once and in order. `build-if!` runs, in this order:
1. the entry, via `with-node-ctor!`;
2. the inactive branch, then `deactivate!`;
3. the active branch;
4. the exit, then the dependencies and the entry's `update-entry` wrap.

These are the same `create-node!` calls and the same draws as in the current engine and in upstream (M:641-686). MH steps never touch handlers. Measured: §0.

### 2.3 (b) Exact enumeration (exact module; owner: exact WP)

```scheme
; cf. Hansei's probcc-distribution (aux.hansei.scm; Kiselyov & Shan, "Embedded probabilistic
; programming"): a random choice is one ((C thunk) p) per support point (`distribution-support`,
; sampler order, p > 0); an observation one C node weighted by exp(log-density), or no node when
; that weight is 0.  Multi-shot.
(define exact-handler
  (value-handler
    (λ (dk resume)
      (if (eq? (distk-kind dk) 'observed)
          (let* ((obs (distk-value dk))
                 (w (fpexp (distribution-log-density dk obs))))
            (if (fp= w 0.) '() (list (probcc-τ w (resume obs)))))
          (filter-map (Λ ((,x . ,p)) (and (fp> p 0.) (probcc-τ p (resume x))))
                      (support-or-error dk))))))   ; `support` error when #f (E5b)

; (new) the model as a lazy Hansei search tree ((V v) p) / ((C thunk) p).
(define (model->search-tree model) (handle-model model exact-handler (o probcc-unit val-of)))

; (new; replaces PLAN §6.10) ((value . prob) ...) sorted by generic<?.
(define (exact-posterior model #!key (validate #t))
  (when validate (build-chain model (make-stdgen-source 17) 'upstream))   ; C29
  (sort (map (Λ (((V ,v) ,p)) (cons v p))
             (probcc-normalize (probcc-explore +inf.0 (model->search-tree model))))
        (λ (a b) (generic<? (car a) (car b)))))
```

- Only aux.hansei's **data-level** tools are imported: `(only (aux hansei) probcc-τ probcc-unit probcc-explore probcc-normalize)`. They work on lists and thunks and do not depend on delimcc, so native `shift`/`reset` and aux's `resetcc` are never mixed in one control context.
- The tree is a genuine Hansei tree: `probcc-explore` with a finite depth leaves `C` entries (p4).
- Continuous observations weigh by density, which is correct for posteriors over discrete latents (E6 post_bias).
- Only the selected branch of an `if` is explored, so the "active node without finite support" rule of PLAN §6.10 holds by construction.

### 2.4 (c) Forward sampler and likelihood weighting (exact module; owner: exact WP)

```scheme
; (new) (values value log-weight): log-weight sums the LLs of the choices drawn (distk-sample,
; same draw order as the builder) and of the observations weighed.  With #:construction-order
; #t an if_ first runs the branch not taken, drawing its choices (their LL counts) but not
; weighing its observations, as ifnode! builds it (M:650-661): the run then reproduces the trace
; builder's initial trace from the same source (value, and LL within 1e-9; deactivation is
; add-then-subtract, so not always bit-equal).
(define (forward-run model src #!key (construction-order #f)) ...)   ; one-shot; `skipping`
                                                                     ; counter set! in the thunk
; (new) n runs on one source, each (value . log-likelihood-of-the-observations).
(define (weighted-samples n model #!key (seed 17) (rng #f)) ...)
; (new) the normalized weighted histogram ((value . p) ...) by generic<?, weights
; exp(logw - max logw).
(define (likelihood-weighting n model #!key (seed 17) (rng #f)) ...)
; (new) build-chain and forward-run #:construction-order #t on two copies of one source; #t,
; 'skipped (first build rejected: llcum infinite, or no result in 'extended mode), else an
; `internal` error naming the first difference (value, or LL beyond 1e-9).
(define (construction-check model #!key (seed 17) (rng #f) (semantics 'upstream)) ...)
```

The reference sketch is in `SP/idiom-probe/p3.scm` (`forward-run`, `weighted-sample`, `likelihood-weighting`).

**`check:`** (PLAN §6.9, wired by the public `mcmC*`):
1. `construction-check` runs once before the chain, on a copy of the source.
2. Then the per-step `reference-check`, unchanged.

The construction check is the new independent oracle for creation order, draw order and LL accounting (C18; replay triage items 2, 3 and 5). It needs only the distribution module, not the engine's construction code.

**Rejected alternative.** A "trace replay" handler that re-runs the model while reading each choice from the chain was considered for `reference-check`. Under plain values, `diracN` of an nref (no node), `diracN` of an nt (a node) and a Val (a node) cannot be told apart, so address alignment would need the engine's scopes. `reference-check` stays static, as in PLAN §6.9.

### 2.5 Ownership and signatures

| Procedure | Module/file | Signature |
|---|---|---|
| `perform`, `handle-model`, `current-handler`, `pure-call`, effect record | engine | §2.1 |
| `dist`, `diracN`, `if_*`, `condition/external`, `lift*`, `constant`, `val-of`, `sexp?` | engine | as now, except `if_*` (C27) |
| `effect-distk`, `select-branch`, `value-handler` | engine | §2.1 |
| `trace-builder`, `run-model!`, `build-dist!`, `build-if!`, `build-chain`, `initial-step`, `mcmc-generator`, `mcmc-fold` | engine | §2.2; the driver is unchanged |
| `exact-handler`, `model->search-tree`, `exact-posterior` | exact | §2.3 |
| `forward-run`, `weighted-samples`, `likelihood-weighting`, `construction-check` | exact | §2.4 |
| `reference-check`, `load-state!`, `forced-step-log-alpha`, `ext-states`, `kernel-residual` | exact | PLAN §6.9, §6.11, unchanged |
| `model`, `define-model`, `if_`, `lift`, `letlift`, `pair`, `collect`, `condition`, `memo/finite`, `mcmC`, `mcmC-fold`, `mcmC§`, `model-graph`, re-exports | public `aux.hakaru10.scm` | §3 |

---

## 3. Public module (WP5) under the amendment

```scheme
(define-syntax-rule (model body ...) (τ body ...))                          ; S:82
(define-syntax define-model                                                 ; a model is a thunk
  (syntax-rules ()
    ((_ (name arg ...) body ...) (define ((name arg ...)) body ...))        ; curried define
    ((_ name body ...) (define (name) body ...))))
(define-syntax-rule (if_ test then else) (if_* test (τ then) (τ else)))     ; S:275-279
(define (lift f . es) (lift* f es))                                          ; S:99-116
(define-syntax-rule (letlift ((x e) ...) body ...) (lift (λ (x ...) body ...) e ...))
(define (pair a b) (lift cons a b))                                          ; S:134-135
(define (collect es) (if (null? es) '() (lift* list es)))                    ; B1:113-114
(define condition condition/external)                                        ; S:174-193
; CB:41-44, footnote 5: eager, in key order (map-in-order: C18′, multi-shot safe, C28);
; the table is built after the last request, so each resumption builds its own.
(define (memo/finite keys f)
  (let1 (table (alist->hash-table (map-in-order (λ (k) (cons k (f k))) keys) equal?))
    (λ (key) (hash-table-ref table key
               (τ (hakaru10-error 'construction 'memo/finite "unknown key" key))))))
(define (mcmC limit model . keys) (reverse (apply mcmC-fold cons '() limit model keys)))
(define (mcmC-fold kons knil limit model #!key (seed 17) (rng #f) (semantics 'upstream) (check #f))
  ...)   ; mcmc-fold, plus construction-check and the reference-check step hook when check
(define (mcmC§ model #!key (seed 17) (rng #f) (semantics 'upstream) (check #f) (retries 1000))
  (let1 (next (mcmc-generator retries model ...))     ; retries bounds the initial retry
    (if next (τ->§ next) '())))                       ; (M:882-891); '() = no trace
```

- Imports: drop `(only (chicken continuation) shift reset)`; add `(only (aux stream) τ->§)`, `srfi-69` and `(only srfi-1 map-in-order)`.
- New exports, in addition to PLAN §3: `exact-posterior` (new signature `(exact-posterior model #!key (validate #t))`), `model->search-tree`, `likelihood-weighting` and `weighted-samples`.
- `bern … poisson` and `laplace`, `dirichlet/symmetric` are re-exported **as named** by the distribution module (§4.2), with no renaming at the re-export.

---

## 4. Refactoring checklist for the written modules

**Rule.** No item may change a draw, a floating-point association or a creation order. Each package re-runs its own suite plus the harnesses (§4.5) before handing over.

### 4.1 `aux.hakaru10.random.scm` (WP1; also `test/hakaru10-random.scm`)

| # | Change | Note |
|---|---|---|
| R-1 | Rename `mkStdGen` → `seed->stdgen`; `iLogBase` → `ilog-base` | Keep `; cf. R:235-245 mkStdGen` and `; cf. R:464-469 iLogBase`. Tests R1, R5, R8 and `hakaru10-distribution.scm`'s one use (sed `\bmkStdGen\b`, `\biLogBase\b`). |
| R-2 | rng-source printer with `fprintf` from `(chicken format)`, like distribution's | cosmetic |
| — | Everything else stays | Already idiomatic: records, `define-syntax-rule` binders, named `let`, `cond-expand` fixnum path, `#!optional`/`#!key`. |

### 4.2 `aux.hakaru10.distribution.scm` (WP2; also `test/hakaru10-distribution.scm`)

| # | Change | Note |
|---|---|---|
| D-1 | Drop the Haskell `S` suffix: `bernS bern/flipS uniformlyS categoricalS uniformS normalS gammaS betaS poissonS laplaceS dirichlet/symmetricS diracS` → `bern bern/flip uniformly categorical uniform normal gamma beta poisson laplace dirichlet/symmetric dirac` | Comments become `; cf. M:73-74 bernS …`. `dirac` stays internal: the public module does not export it. No clash with `scheme`/`(chicken base)`. Mechanical rename with `\b…S\b`. |
| D-2 | `sampleK` → `distk-sample` (M:97-100); delete `distinfo` (M:102-105, unused outside one test; use `distk-kind`/`distk-rd`) | `distribution-sample` stays as the checked public wrapper. |
| D-3 | Header "Contents" and the engine-facing note | `distribution-support` is the exact handler's input (§2.3). |
| — | Keep | `let-flonums`, `fp-horner`, `make-resampleable`, closures-in-records, `case` on the kind symbol (a record per kind adds nothing), the `fold` in `categorical` (`acc + w`, the left fold of D:89). |

### 4.3 `aux.hakaru10.util.scm` (WP3)

No change is required: records are not needed, the folds are already named `foldl`/`foldr` in Haskell's direction, and `Λ` is already used.

**Do not** use aux.base's `foldr/avg`, `foldr/var`, `foldr/add`: they fold right from exact 0 and would change X11, X13 and U2 bit for bit.

### 4.4 `aux.hakaru10.engine.scm` (engine WP; after D-1/D-2)

| # | Change | Validated |
|---|---|---|
| E-1 | `(define-record effect request k)`; `(perform . request)` | p1, harness |
| E-2 | `handle-model` takes `#!optional (return identity)` (§2.1) | harness |
| E-3 | Handler signature `(λ (request resume))`, dispatch with `match/first`; `trace-builder` as in §2.2 | harness |
| E-4 | `if_*` = `((perform 'if test th el))`; the builder answers `(τ (build-if! ch test th el))`; `build-if!` runs the branches inline: `(τ (to-node! ch (th)))` | all replays |
| E-5 | Delete `handle-if`; add `select-branch` and `value-handler` (§2.1) | p3/p4 |
| E-6 | `run-model!` = `(handle-model model (trace-builder ch) (λ (e) (to-node! ch e)))` | harness |
| E-7 | Follow D-1/D-2: `diracS` → `dirac`, `sampleK` → `distk-sample` | mechanical |
| E-8 (optional) | One `(define-record branch entry then-result else-result then-range else-range)` per `if_`, stored in a single node field `branch` of the entry and of the exit; it replaces `branches` (pair of pairs) and `exit-info` (the `#(entry th el)` tuple vector). | Re-run `wp4b.scm` (it reads `node-exit-info`; adapt it). |
| E-9 | Rewrite the ENGINE API header block (l.32-148) for E-1…E-8. Comments: `select-branch` gets `; cf. S:275-279 … deviation 9 (S:275): boolean test`; `value-handler` gets `; (new)`. | |
| E-10 (optional) | Record printers with `fprintf` | cosmetic |
| — | Keep | The hot paths stay imperative where measured: the heap, `propagate!`, `move-activity!`, `leaves-tstamp`, `modify-node!`. Also the journal of undo thunks, `chain-fold`, `mcmc-generator` and `mcmc-fold`. |

The patched engine `SP/idiom-probe/eng/aux.hakaru10.engine.scm` already contains E-1…E-6; its diff is `SP/idiom-probe/engine-protocol.diff`. Only the `select-branch` comment there is stale.

### 4.5 Scratchpad harnesses

`SP/harness/effects.scm` uses the old `(op args resume)` signature and `handle-if`. The updated copy is `SP/idiom-probe/harness/effects.scm`. The other harnesses need only the D-1/E-7 renames.

---

## 5. Test plan changes

**New suite `hakaru10-handlers`.** It goes in `test/hakaru10-exact.scm` (WP8) and imports `(aux hakaru10)`, `(aux hakaru10 engine)`, `(aux hakaru10 exact)` and `(aux stream)`.

| Id | Check | Expected | Kind |
|---|---|---|---|
| H1 | **One model, three interpretations.** For grass, girl, pcorr (pair), ph2, ph21, pcond1, pcond2, pcond4, post_bias and colored balls 3 × 4 blue, compare `exact-posterior`, `likelihood-weighting` 20000 and the MCMC histogram over `mcmC` 20000. pswitch is run under `'extended` only. | LW and MCMC within .02 of exact. Exact values as E1-E8. | tol |
| H2 | **Construction-order forward run = initial trace.** `construction-check` on grass, pcond3, pcond4, pswitch, nested (`SP/harness/wp4b.scm`), colored balls 8 × 10 blue, phierN 10 and regression k=5 N=500, each in `'upstream` and `'extended`. | `#t` (value `equal?`, LL 1e-9) | exact |
| H3 | **Hansei format.** `(probcc-explore 0 (model->search-tree grass))` contains `C` entries; `(probcc-normalize (probcc-explore +inf.0 …))` equals `exact-posterior`. | | tol 1e-12 |
| H4 | **Errors under every handler.** `dist`, `diracN` and `if_` outside a model raise `construction`; `dist` inside a `lift` function raises `construction` under the builder, exact and forward handlers; a non-boolean `if_` test raises `branch` under each; an unknown request raises `internal`. | raises | |
| H5 | **Validation build (C29).** `exact-posterior` of `(let* ((x (dist bern .5))) (dist (condition x bern) .5))` | raises `conditioning` | |
| H6 | **Nested `if_` and `diracN` under value handlers.** A finite two-level `if_` model with `diracN` of an nref and of a lifted expression: exact vs MCMC 20000. | within .02 | tol |
| H7 | **Continuous model under LW.** pcond3: LW 20000 gives {1 .5, 20 .25, 21 .25} ±.02 (exact raises, E5b). mixng: weighted mean 5.5 ±.4. | | tol |
| H8 | **Multi-shot purity (C28).** `memo/finite` under exact gives E7. The impure model of p4 is shown in the doc only and **not asserted**. | | tol 1e-12 |

**Changed items.**

| Item | Change |
|---|---|
| L8 | `(§->list (take§ 100 (mcmC§ m)))` equals `(mcmC 100 m)`. Also: `(mcmC§ (model (dist (condition 5. uniform) 0 1)))` is `'()`; `car§`/`cdr§` work on the stream. |
| E1-E8 | Call `(exact-posterior model)`: no `#:seed`, no `#:max-paths`. E5b raises `support` from the exact handler. |
| E9 | Hansei-native programs (`probcc-reify/exact`, as in `test/hansei.scm:87-154`) for grass, girl and pcond4, compared with `exact-posterior`. They run in their own control context (aux delimcc), never inside a hakaru10 handler. |
| B4 | `check: 300` now also runs `construction-check` once per chain. |
| Main doc §6 "Embedding notes" | Adds: the three-operation protocol (C25-C27), handlers (C25), multi-shot purity (C28), and that the static discipline is checked by the builder (C29). |

**Runtime.** About +3 s for `hakaru10-handlers` (LW and MCMC at 20000 on about 10 small models), within the §14 target.

---

## 6. PLAN items superseded

- **§1** C23 (→ C23′), C18 (→ C18′), C6 (`define-model` is curried `define`; names per C30).
- **§2 table**
  - exact imports: drop `(chicken continuation)`; add `(chicken sort)`, `(chicken flonum)`, `(aux hakaru10 random)` and `(aux hakaru10 util)` (for `generic<?`).
  - public imports: drop `(chicken continuation)`; add `(only (aux stream) τ->§)`, `srfi-69`, `(only srfi-1 map-in-order)`.
- **§3.1** `memo/finite` definition (§3 here). §3.2 internal names (D-1). §3.3 `exact-posterior` signature, `mcmC§` keys (`#:retries`), new exports (§3).
- **§4.1-§4.4** names (R-1, D-1, D-2) and engine API (E-1…E-8). **§4.5** list: drop `exact-posterior*`; add §2.3/§2.4 procedures.
- **§6.10** replaced by §2.3. **§6.9** keeps `reference-check` and gains `construction-check` (§2.4).
- **§9.4** L8; **§9.5** E1-E9 calling convention, plus the `hakaru10-handlers` suite (§5).
- **§11 aux.egg**
  - `aux.hakaru10`: add `aux.stream` to its `component-dependencies`;
  - `aux.hakaru10.exact`: add `aux.hakaru10.random` and `aux.hakaru10.util`.
  - aux.egg is WP0's file; the orchestrator applies this.
- **§13** sequencing (§7 here).

---

## 7. Sequencing

1. **I-1** (WP1 owner): R-1, R-2.
2. **I-2** (WP2 owner): D-1…D-3. Runs in parallel with I-1.
3. **I-3** (engine owner): E-1…E-10, after I-2.
   - Gate: re-run `SP/idiom-probe/harness/*.scm` (renamed) with 0 failures, plus `SP/idiom-probe/p3.scm` and `p4.scm`.
4. **I-4** (WP5): the public module per §3, after I-3.
5. **I-5** (WP6): the exact module per §2.3-§2.4, after I-3. Runs in parallel with I-4.
6. **I-6**: the tests (WP7, WP8) per §5.
7. The WP10 citation pass, as planned.

**Risks.**
- Dependency weight: the `(aux stream)` import loads aux's delimcc modules. Load-time only; they are never used in a hakaru10 control context.
- Model impurity (C28) is a user-visible semantic rule.
- `construction-check` compares LL within 1e-9, not bit-exactly, because deactivation adds then subtracts.
