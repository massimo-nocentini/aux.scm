# Audit of `(aux base)` and `(aux kanren micro)`

Commit `f4e2475`, 2026-09-20. Targets: `src/aux.base.scm` (443 lines) and `src/aux.kanren.micro.scm` (622 lines). Every finding below was reproduced by an independent verifier on a private build; all suites pass at baseline (test 16, bootstrap 9, dmatch 6, microkanren 7, microkanren-aggregation 6, microkanren-untagged 7, microkanren-show 12).

**`(aux base)` verdict.** The matcher core is sound, but `match/non-overlapping` splices its scrutinee twice, which makes every nested unification in the engine exponential (depth-20 term: 2.5 s, fixed: 0 ms) and is the single largest defect found. `length/>?` returns the n-th element instead of a boolean and silently drops `#f` values in `(aux category ziplist)`. Vector patterns expand to 2^n code, so a 12-element `#(...)` pattern takes three minutes to compile. `define-macro` cannot be used from compiled code. The rest is API hygiene: an undocumented `=>` receiver form the engine depends on, a handful of helpers that duplicate srfi-1 / `(chicken base)` bindings, several exported but untested one-liners with edge-case bugs (`absent?` on compound terms, `foldr/var` on singletons, `,,x` with `eq?`), and a few cheap constant-factor wins.

**`(aux kanren micro)` verdict.** Unification, occurs check, disequalities and reification are correct on plain lists and atoms, but the type-tag / absento interplay is broken in three independent ways that together mean `absent°` + `symbol°` never yields a constraint (answers admit values the query forbids, and the quine tests enshrine the lost constraint). Any answer containing a vector crashes because vector-lib's `vector-map` passes the index. Reification fails past 24 free variables, `°->list/ground` re-checks constraints against placeholder symbols and depends on the caller's imports, a tuple disequality is reified as a conjunction (stronger than what the engine enforces), and `enumerate°` numbers answers backwards. The refactoring lens found six copies of the vector/record rule, two identical subsumption loops, duplicated aggregation folds and six positional state rebuilds; each proposed rewrite passes the suites.

## Method

Each module was examined through three lenses (errors, refactorings, optimizations) by separate finder agents against a scratch build of `src/` (csi/csc 6.0.1pre1). Findings were merged (duplicates and pure style nits dropped) and every survivor was handed to an independent verifier that re-ran the repro on the pristine build, applied the proposal to a private copy, rebuilt with `chicken-install` into a private repository and re-ran the suites. Optimizations were measured with compiled (`csc -O2 -d1`) programs against the baseline and patched `.so`, same binary, library swapped via `CHICKEN_REPOSITORY_PATH`.

| module | raw candidates | dropped at merge | verified | reproduced | refuted |
|---|---|---|---|---|---|
| `(aux base)` | 46 | 24 | 22 | 21 | 1 |
| `(aux kanren micro)` | 34 | 12 | 22 | 21 | 1 |

Severity is the verifier's, not the finder's; several were lowered where the end-to-end effect on the engine did not reproduce. "Suites" means the seven suites above unless stated otherwise.

## `(aux base)`

`src/aux.base.scm` is the prelude every other module imports (`(module (aux base) *`). It mixes four concerns: macro-definition helpers (`define-syntax-rule`, `define-macro-ir/er`, `define-macro`), two pattern matchers on one `match-pattern` syntax-rules core (`match/first` = first clause wins with `⊣` guards; `match/non-overlapping` = every clause tried, exactly one may match), a tiny documentation registry, and ~90 combinators, list/string helpers and statistics folds with Greek aliases. The engine reaches it on every hot path: sbral lookups through `λ-match/first`, `match/non-overlapping` in the pair case of unification, `exists` / `remove-duplicates` in the constraint store, `sort/lex<=?` four times per reified answer.

### Errors

#### B1. `match/non-overlapping` evaluates its scrutinee twice — **high** — `src/aux.base.scm:52`

```scheme
;; src/aux.base.scm:51-52
(define-syntax-rule (match/non-overlapping v (e ...) ...)
  (dmatch-run-a-thunk (quote v) v (dmatch-remexp v (e ...) ...)))
;; :58 — only the inner copy is let1-bound
((dmatch-remexp (rator rand ...) cls ...) (let1 (v (rator rand ...)) (dmatch-aux v cls ...)))
```
```
csi> (define c 0)
csi> (match/non-overlapping (begin (set! c (add1 c)) c) (,x (list x c)))
(2 2)                                  ; expected (1 1)
μkanren-state-unify, two distinct left-nested terms, pristine build:
depth 10: 2 ms  12: 10  14: 38  16: 155  18: 615  20: 2459 ms   (x4 per +2 levels)
```

Why: `v` is spliced once as the `value` argument of `dmatch-run-a-thunk` (error messages only) and once into `dmatch-remexp`. Side effects run twice, the value reported in a "no match" error is a different evaluation from the one matched, `src/aux.kanren.micro.scm:136` wraps the recursive car-unification in it (so unifying a left-nested term of depth d costs 2^d), and `:242` (`μkanren-subsumed?`) wraps `μkanren-state-unify/assoc` the same way, so every disequality subsumption check also unifies twice. `match/first` (`:93`) already binds once.

```scheme
(define-syntax-rule (match/non-overlapping v (e ...) ...)
  (let1 (v* v) (dmatch-run-a-thunk (quote v) v* (dmatch-aux v* (e ...) ...))))
;; then delete dmatch-remexp (src/aux.base.scm:56-59), now dead
```

Verdict: reproduced. Suites pass (plus hansei 6, fds.sbral 6). Measured: nested unification depth 10–20 all 0 ms after; flat unify 100 vars ×200 68 → 42 ms (−38%); `symbol°`/`≠°` over 8 vars ×2000 412 → 388 ms; `append°` on ground lists unchanged (search-dominated).

#### B2. `length/>?` returns the n-th element, not a boolean — **high** — `src/aux.base.scm:417`

```scheme
;; src/aux.base.scm:417-421
(define (length/>? lst n)
  (match/first (cons n lst)
    ((0 . (,v . _)) v)
    ((_ . ()) #f)
    ((_ . (_ . ,lst*)) (length/>? lst* (sub1 n)))))
```
```
csi> (list (length/>? '(#f 1) 0) (length/>? '(1 2) 0.0) (length/>? '(1) -1) (length/>? '(1 2 3) 1))
(#f #f #f 2)                          ; all three lengths ARE greater than n
csi> (import (aux category ziplist))
csi> (>>= (list 1 2) (lambda (x) (list #f x)))
(2)                                    ; the #f at index 0 vanished
```

Why: the first clause returns the element, so a `#f` element is indistinguishable from "list too short"; negative n returns `#f`; a flonum n never `equal?`s the literal `0`. The only consumer, `src/aux.category.ziplist.scm:16`, relies on truthiness via `=>`, so the ziplist monad loses `#f` values. A pair is also consed on every step.

```scheme
;; src/aux.base.scm:417-421
(define (length/>? lst n)   ; #t iff (length lst) > n, without walking past n
  (cond
    ((< n 0) #t)
    ((null? lst) #f)
    ((zero? n) #t)
    (else (length/>? (cdr lst) (sub1 n)))))

;; src/aux.category.ziplist.scm:16 — the receiver relied on the element being returned
((length/>? m* i) (cons i* (cons (list-ref m* i) acc)))
```

Verdict: reproduced. Suites pass (plus category-list 8). After the fix the ziplist bind gives `(#f 2)` and truthy-element results are byte-identical. No test covers ziplist; add `(>>= (list 1 2) (lambda (x) (list #f x)))` ⇒ `(#f 2)`. Side measurement: 1000-element walk ×20000, compiled: 855 → 389 ms.

#### B3. `define-macro` is unusable from compiled code — **medium** — `src/aux.base.scm:39`

```scheme
;; src/aux.base.scm:39
(let* ((inject (λ symbols (inject* (apply symbols->symbol/stripped-syntax symbols))))
  ...))
;; :44 — a runtime define, absent from the compiler process
(define (symbols->symbol/stripped-syntax . symbols) (apply symbol-append (map strip-syntax symbols)))
```
```
dm2.scm: (define-macro (m1 (inject (bi 'foo)) (compare (bl 'bar))) ((_ ,a) `(list ,a ',bi))) (write (m1 1))
csi -s dm2.scm  => (1 foo)
csc dm2.scm     => Error: during expansion of (m1 ...) - unbound variable: aux.base#symbols->symbol/stripped-syntax
```

Why: the transformer body runs at expansion time; a plain `define` only exists at runtime. `define-macro-ir` (used by `aux.category.monad`, `aux.anthropic`) works because `symbol-append` / `strip-syntax` are core bindings. No in-repo module uses `define-macro`, so the defect is latent, but the macro is exported.

```scheme
(define-syntax-rule (define-macro (name (inject (bi i) ...) (compare (bl l) ...)) (pattern body ...) ...)
  (define-macro-ir (name expr inject* compare)
    (let* ((inject (λ symbols (inject* (apply symbol-append (map strip-syntax symbols)))))
           (bi (inject i)) ...
           (bl (let1 (l* l) (λ (x) (compare x l*)))) ...)
      (match/first expr (pattern body ...) ...))))
;; symbols->symbol/stripped-syntax (:44) then has no in-repo caller; keep as a runtime utility or drop
```

Verdict: reproduced; after the fix `csc dm2.scm && ./dm2.out` prints `(1 foo)`. Suites pass. The interpreter-only suites cannot catch this class; consider one `csc`-compiled test.

#### B4. `absent?` never detects a compound subterm — **low** — `src/aux.base.scm:430`

```scheme
;; src/aux.base.scm:430-440 — null?/pair?/vector?/record-instance? are tested before equal?
(define (absent? v obj)
  (cond
    ((null? obj) #t)
    ((pair? obj) (and (absent? v (car obj)) (absent? v (cdr obj))))
    ((vector? obj) (let loop ((i 0)) (cond ((= i (vector-length obj)) #t) ...)))
    ((record-instance? obj) (absent? v (record->vector obj)))
    (else (not (equal? v obj)))))
```
```
csi> (list (absent? '(1 2) '(a (1 2))) (absent? (vector 1) (list (vector 1))) (absent? '() '()))
(#t #t #t)                             ; all three clearly occur
```

Why: compound `obj` is always descended before being compared, so `equal?` only ever sees leaves. In-repo consumers (`src/aux.kanren.micro.scm:207`, the untagged and show tests) pass symbol tags only, hence the suites do not notice. The verifier refuted the finder's speed claim for the naive fix (calling `equal?` at every node is 4% slower); dispatching once on the shape of `v` keeps the leaf-only fast path for atoms.

```scheme
(define (absent? v obj)
  (define (atom-absent? obj)      ; v is an atom: only leaves can be equal? to it
    (cond
      ((pair? obj) (and (atom-absent? (car obj)) (atom-absent? (cdr obj))))
      ((vector? obj) (let1 (n (vector-length obj))
                       (let loop ((i 0))
                         (cond
                           ((fx= i n) #t)
                           ((atom-absent? (vector-ref obj i)) (loop (fx+ i 1)))
                           (else #f)))))
      ((record-instance? obj) (atom-absent? (record->vector obj)))
      (else (not (equal? v obj)))))
  (define (struct-absent? obj)    ; v is compound (or ()): compare at every node
    (cond
      ((equal? v obj) #f)
      ((pair? obj) (and (struct-absent? (car obj)) (struct-absent? (cdr obj))))
      ((vector? obj) (let1 (n (vector-length obj))
                       (let loop ((i 0))
                         (cond
                           ((fx= i n) #t)
                           ((struct-absent? (vector-ref obj i)) (loop (fx+ i 1)))
                           (else #f)))))
      ((record-instance? obj) (struct-absent? (record->vector obj)))
      (else #t)))
  (if (or (null? v) (pair? v) (vector? v) (record-instance? v))
    (struct-absent? obj)
    (atom-absent? obj)))
```

Verdict: reproduced. Suites pass. Measured (`absent? 'closure` on a 200-element nested vector ×20000): baseline 837–856 ms, this variant 712–728 ms. Note `(absent? '() '(a b))` becomes `#f` since `'()` terminates the list; needs `(chicken fixnum)` if B-R1 removes it.

#### B5. `letport/string` with an input string returns a meaningless second value — **low** — `src/aux.base.scm:163`

```scheme
;; src/aux.base.scm:163-165
((_ (p instring) body ...) (let* ((v (void))
                                  (s (call-with-input-string instring (λ (p) (set! v (begin body ...))))))
                             (values v s)))
```
```
csi> (call-with-values (λ () (letport/string (p "(1 2)") (read p))) list)
((1 2) #<unspecified>)
```

Why: `s` is the value of `set!`. The out/else clauses return the captured string; this clause copies the shape without the content. No callers in `src/`.

```scheme
((_ (p instring) body ...) (call-with-input-string instring (λ (p) body ...)))
```

Verdict: reproduced. test/bootstrap/dmatch pass.

#### B6. `,,x` back-references compare with `eq?` — **low** — `src/aux.base.scm:120`

```scheme
;; src/aux.base.scm:120 vs :148
((match-pattern val (unquote (unquote var)) kt kf) (if (eq? var val) kt kf))
((match-pattern val lit kt kf) (if (equal? val (quote lit)) kt kf))
```
```
csi> (match/first (list 1.5 (list 1.5)) ((,x (,,x)) x))
Error: match/first: uncaught value   ; same for bignums and strings; fixnum 1 works (test/dmatch.scm:98)
```

Why: literal patterns and back-references disagree on any non-fixnum number and on strings. `equal?` is not an option: `src/aux.category.ziplist.scm:11` uses `,,m` to detect a circular list. `eqv?` is O(1), cycle-safe and fixes the numeric case.

```scheme
((match-pattern val (unquote (unquote var)) kt kf) (if (eqv? var val) kt kf))
```

Verdict: reproduced; flonum and bignum then match, strings still by identity, ziplist cycle detection still terminates. Suites pass (plus fds.sbral 6, which uses `,,` on fixnum sizes).

#### B7. `,@var` in a pattern silently means `,var` — **low** — `src/aux.base.scm:121`

```scheme
;; src/aux.base.scm:121
((match-pattern val (unquote-splicing var) kt kf) (match-pattern val (unquote var) kt kf))
;; :108-112, dead #| |# scratch block: (match/first '(1 2 3 4) ((1 ,@v 3 4) v))
```
```
csi> (match/first '(1 2 3 4) ((1 ,@v 3 4) v))
2                                      ; v binds one element; '(1 2 2 3 4) does not match
```

Why: accepting `unquote-splicing` suggests segment matching; the alias gives a single-element binding with no diagnostic. Deleting the line is NOT enough: `(unquote-splicing v)` is a pair and falls through to the `(x . y)` arm, matching nothing silently (verified).

```scheme
((match-pattern val (unquote-splicing var) kt kf)
  (syntax-error "match-pattern: splicing patterns are not supported" var))
;; also delete the scratch block :108-112 and the #; datum comments at :119, :125-127, :140-146
```

Verdict: reproduced; `,@v` now fails at expansion time, `,v` unaffected. Suites pass. No file in the repo uses `,@` inside a match pattern.

#### B8. `foldr/avg` on `'()` and `foldr/var` on a singleton raise division by zero — **low** — `src/aux.base.scm:354`

```scheme
;; src/aux.base.scm:354-358
(define (foldr/avg lst) (/ (foldr/add lst) (length lst)))
;; foldr/var divides by (sub1 (length lst)); foldr/stddev wraps it
```
```
csi> (foldr/var '(5))     => Error: (/) division by zero
window° with foldr/stddev as aggregate and a department of one row aborts the whole query
```

Why: sibling reducers accept `'()` (`foldr/max` ⇒ `-inf.0`, `foldr/add` ⇒ `0`). `window°`/`groupby°` never build an empty group, so the realistic crash is a singleton group under `foldr/var` / `foldr/stddev`. Raising may be intentional, hence low.

```scheme
(define (foldr/avg lst) (if (null? lst) +nan.0 (/ (foldr/add lst) (length lst))))
(define (foldr/var lst)
  (let1 (n (length lst))
    (if (< n 2)
      +nan.0
      (let1 (m (foldr/avg lst))
        (/ (foldr (λ (x acc) (let1 (d (- x m)) (+ (* d d) acc))) 0 lst) (sub1 n))))))
```

Verdict: reproduced. Suites pass; non-degenerate results unchanged. Test with `(nan? ...)` from `(chicken flonum)`, since `(= +nan.0 +nan.0)` is `#f`.

#### B9. Repeated pattern variables are shadowed, not compared — **low** — `src/aux.base.scm:122`

```scheme
;; src/aux.base.scm:122
((match-pattern val (unquote var) kt kf) (let1 (var val) kt))
```
```
csi> (match/first '(1 2) ((,x ,x) x))
2                                      ; matched although the positions differ
```

Why: each `,x` is an independent `let1`. The `,,x` form exists for this, but nothing documents linearity, and `,,x` itself has the `eq?` caveat of B6. A compile-time linearity check is not feasible in `syntax-rules` without threading a bound-variable list through every clause.

```scheme
;; comment above match-pattern (src/aux.base.scm:114):
; NOTE: pattern variables must be LINEAR: each `,x` unconditionally rebinds x, so `(,x ,x)`
; matches ANY 2-list and binds x to the last element. To test a position against an already
; bound x use `,,x`, which compares with eqv? (identity for strings and lists).
```

Verdict: reproduced (documentation-only proposal; suites not affected).

### Refactorings

#### B-R1. Six imports are unused or replaceable — **low** — `src/aux.base.scm:4`

```
src/aux.base.scm:4 imports (scheme base) (chicken continuation) (chicken fixnum) (chicken foreign)
(chicken module) vector-lib; grep finds no call/cc, fx*, foreign, export/reexport/functor, or
vector-lib procedure. The only (scheme base) binding used is open-input-string (:343).
```

Why: dead imports hide the real dependency surface of the foundational module. `subvector` and `delay-force` come from `(chicken base)`, not `scheme` (verified with csc), so that import stays. `vector-lib` remains an egg dependency because the engine imports it itself.

```scheme
(import scheme
        (chicken base)
        (chicken pretty-print)
        (chicken memory representation)
        (chicken sort)
        (chicken port)
        (chicken syntax)
        (chicken string)
        (only srfi-1 append-map iota)
        srfi-69)
;; src/aux.base.scm:343
(define (load/string str) (with-input-from-string str read))
```

Verdict: reproduced; whole-egg rebuild clean, all 16 base-dependent suites pass. Re-add `(chicken fixnum)` if B4 / B-O5 are adopted.

#### B-R2. Undocumented, untested `=>` receiver form in guarded `match/first` clauses — **medium** — `src/aux.base.scm:101`

```scheme
;; src/aux.base.scm:101-103 — `=>` works only because the guard lands inside cond; not in the literal list (:96)
((match-case-simple* val ((pattern ⊣ g) exp ...) clause ...)
  (let1 (fk (τ (match-case-simple* val clause ...)))
    (match-pattern val pattern (cond (g exp ...) (else (fk))) (fk))))
```
```
consumers: src/aux.kanren.micro.scm:348, :368, :394, e.g.
  ((((,α . _) . ,T*) ⊣ (μkanren-verify-T T* s)) => (μkanren-verify-T+ α T s))
rewriting the cond as (if g (begin exp ...) (fk)) breaks the egg build: Unknown identifier `=>'
```

Why: a real feature of the matcher exists only as an accident of expansion; `dmatch-aux` (`:64`) spells the same rule out explicitly. Anyone simplifying `match-case-simple*` breaks the engine.

```scheme
(define-syntax match-case-simple*
  (syntax-rules (else ⊣ =>)
    ...
    ; a guarded clause may also be ((pat ⊣ g) => receiver): receiver gets g's value (cond semantics)
    ((match-case-simple* val ((pattern ⊣ g) => receiver) clause ...)
      (let1 (fk (τ (match-case-simple* val clause ...)))
        (match-pattern val pattern (let1 (g* g) (if g* (receiver g*) (fk))) (fk))))
    ((match-case-simple* val ((pattern ⊣ g) exp ...) clause ...)
      (let1 (fk (τ (match-case-simple* val clause ...)))
        (match-pattern val pattern (cond (g exp ...) (else (fk))) (fk))))
    ...))

;; src/test/dmatch.scm, inside define-suite:
((test/match-first/receiver _)
  (⊦= 4 (match/first '(1 2) (((,a ,b) ⊣ (+ a b)) => (μ s (add1 s)))))
  (⊦= 'fallthrough (match/first '(1 2) (((,a ,b) ⊣ (memq 3 (list a b))) => car) (else 'fallthrough)))
  (⊦= 4 (match/first '(1 2) (((,a ,b) ⊣ (+ a b)) => add1) (else 'no))))
```

Verdict: reproduced. Suites pass (dmatch 6 → 7).

#### B-R3. `λ-curry` is an exact duplicate of `μ` — **low** — `src/aux.base.scm:336`

```scheme
;; src/aux.base.scm:336-340 vs :178-183: rule for rule the same nested unary lambdas;
;; μ additionally accepts a bare symbol. λ-curry is used only by define-curry (:341), which has no consumer.
```

```scheme
(define-syntax-rule (λ-curry formals body ...) (μ formals body ...))
(define-syntax-rule (define-curry (name arg ...) body ...) (define name (μ (arg ...) body ...)))
```

Verdict: reproduced; only behavioural change is that `λ-curry` now also accepts a bare symbol. Suites pass (plus category-list, stream, fds.sbral). Deleting both is equally valid.

#### B-R4. Seven combinators re-spell existing bindings — **low** — `src/aux.base.scm:320`

```
:320-321 K / K*         = (chicken base) constantly
:329 curry₁             = body of curry (:273) re-spelled instead of currying it
:154 Λ                  = single-clause λ-match/first (:151)
:250-253 letmap         = lettensor (:243-246) with append-map and the inner result wrapped in list
:268-271 mappair        = n-ary map over lst and (cdr lst) (core map stops at the shortest)
:360 foldr/concat       = srfi-1 concatenate;  :364-365 foldr/and, foldr/or = (not (memq #f ..)), (any identity ..)
```

Why: each pair is two copies kept in lockstep by hand. `foldr/add` and `foldr/times` are deliberately left as folds (`apply` fails at 1e6 arguments). `foldr/concat`'s only live caller (`src/aux.kanren.micro.show.scm:441`) passes a fresh `(map ...)`, so `concatenate`'s tail sharing is harmless.

```scheme
;; :17
(only srfi-1 append-map iota concatenate any)
;; :154
(define-syntax-rule (Λ pat body ...) (λ-match/first (pat body ...)))
;; :250-253
(define-syntax-rule (letmap ((x expr) ...) body ...) (lettensor append-map ((x expr) ...) (list (begin body ...))))
;; :268-271
(define (mappair f lst) (if (null? lst) '() (map f lst (cdr lst))))
;; :320-321
(define (K x) (constantly x))
(define K* constantly)
;; :329
(define curry₁ (μ (f g) (curry f g)))
;; :360, :364, :365
(define (foldr/concat lst) (concatenate lst))
(define (foldr/and lst) (not (memq #f lst)))
(define (foldr/or lst) (any identity lst))
```

Verdict: reproduced; differential test against the original definitions reports 0 mismatches on the edge cases (zero keeps, zero trailing args, dotted rest, 0–3 `letmap` bindings, 0/1/2/4-element `mappair`, nine boolean lists). Fifteen suites pass.

#### B-R5. About 45 exported bindings have no use anywhere in the repo — **low** — `src/aux.base.scm:2`

```
(module (aux base) * — token-exact grep over src/*.scm and src/test/*.scm finds no reference to:
define-macro, define-macro-er, letport/string, sub1!, δ!, define-τ, define-let, letassoc/cdr, curry,
memoize/call, memoize/arg, define-memo, boolean->P, boolean->01, indicator, pairwise-different?, K*, S*,
S⁺, Φ, Y, curry₁, snoc, snoc/λ, λ-curry, define-curry, display/pp, foldr/times, foldr/stddev, foldr/max,
foldr/min, foldr/and, foldr/or, not/✓, map/dotted, appender˱, appender˲, english-alphabet/*,
greek-alphabet/uppercase, enumerate; mappair only inside #; comments.
src/aux.kanren.micro.scm:66,99,442 locally rebind S*, so exported SKI names collide with ordinary locals.
```

Why: every one of these is public API with no test, and several are the defects above. An explicit export list cannot simply hide the helpers: CHICKEN 6 rejects an exported macro whose expansion references an unexported binding ("Unknown identifier `helper'", verified), so `dmatch-*`, `match-case-simple*`, `match-pattern`, `documentation-hash-table` must stay exported. Options: a test per binding, or move the combinator playground into `(aux base extra)`; `greek-alphabet/lowercase` must stay (used at `src/aux.kanren.micro.scm:43`).

```scheme
;; src/test/test.scm, before ((test/define-many _) — minimal coverage step, 16 -> 17 cases
((test/base/combinators _)
  (⊦= 7 ((K 7) 1 2))
  (⊦= '(1 2) (receive ((K* 1 2) 'x)))
  (⊦= 6 (((S* +) (λ (x) (* 2 x))) 2)))
```

Verdict: reproduced. Suites pass with the added case.

### Optimizations

#### B-O1. Vector patterns expand to 2^n code and allocate O(n²) subvectors — **high** — `src/aux.base.scm:123`

```scheme
;; src/aux.base.scm:123-134 — the continuation (rest of pattern + kt) is emitted in BOTH arms at every element
((match-pattern val #(x x* ...) kt kf)
  (cond
    ((and (vector? val) (> (vector-length val) 0))
      (let ((valx (vector-ref val 0)) (valy (subvector val 1)))
        (match-pattern valx x (match-pattern valy #(x* ...) kt kf) kf)))
    ((record-instance? val)
      (let* ((val* (record->vector val)) (valx (vector-ref val* 0)) (valy (subvector val* 1)))
        (match-pattern valx x (match-pattern valy #(x* ...) kt kf) kf)))
    (else kf)))
```
```
csc -O2 -d1 on one n-element #(,x0 ... ,x{n-1}) pattern, pristine:
n=4 0.67 s / 85 KB C;  n=6 1.88 s / 321 KB;  n=8 7.73 s / 1.37 MB;  n=10 35.7 s / 5.79 MB / 332 MB RSS
(finder: n=12 176 s / 21.7 MB / 888 MB RSS; n=16 past 10 min and 4.2 GB)
```

Why: any vector pattern past ~10 elements is effectively uncompilable and every vector/record match pays quadratic allocation. Normalising to a list keeps exact-length semantics (the terminal `()` arm rejects longer vectors) and leaves one place that knows how records are viewed.

```scheme
((match-pattern val #(x x* ...) kt kf)
  (let1 (val* (cond
                ((vector? val) (vector->list val))
                ((record-instance? val) (vector->list (record->vector val)))
                (else #f)))
    (if val* (match-pattern val* (x x* ...) kt kf) kf)))
;; the #; commented-out vector/record branches of the (x . y) arm at :140-146 can go at the same time
```

Verdict: reproduced. Suites pass (dmatch covers `#()`, `#(p)`, `#(,r 2)`, record `#(hello ,r ,e)`; plus hansei, stream, fds.sbral, category-list, letcc). Measured: compile n=8 7.73 s → 0.32 s / 17.9 KB; n=10 35.7 s → 0.33 s; n=16 and n=32 0.33 / 0.38 s. Runtime ×1e6: `#(4)` 260 → 106 ms, `#(8)` 550 → 170 ms, 3-field record 330 → 175 ms.

#### B-O2. `memoize/arg` and `λ-memo` do two hash lookups per hit — **medium** — `src/aux.base.scm:289`

```scheme
;; src/aux.base.scm:289-293 (λ-memo at :295-300 inlines the same algorithm)
(define (memoize/arg f)
  (let1 (memo (make-hash-table))
        (λ (arg)
            (unless (hash-table-exists? memo arg) (hash-table-set! memo arg (f arg)))
            (hash-table-ref memo arg))))
```

Why: every hit hashes the key twice (equal?-hash on the rest-args list for `λ-memo`); srfi-69's `hash-table-ref` fill thunk gives one lookup. The verifier found that aliasing `memoize/arg` to a variadic `memoize` (the finder's proposal) forfeits most of the gain — keep it as a dedicated 1-argument procedure.

```scheme
(define (memoize f)
  (let1 (memo (make-hash-table))
    (λ args
      (hash-table-ref memo args (τ (let1 (v (apply f args)) (hash-table-set! memo args v) v))))))

(define (memoize/arg f)
  (let1 (memo (make-hash-table))
    (λ (arg)
      (hash-table-ref memo arg (τ (let1 (v (f arg)) (hash-table-set! memo arg v) v))))))

(define-syntax-rule (λ-memo args body ...) (memoize (λ args body ...)))
(define-syntax-rule (define-memo (name arg ...) body ...) (define name (λ-memo (arg ...) body ...)))
```

Verdict: reproduced. Suites pass (plus hansei 6, the only consumer outside the suites, `src/aux.hansei.scm:130`; `test/procc/λ-memo` in test.scm exercises the macro directly and fib-memo 10 still counts 11 evaluations). Measured (csc -O3, 2e6 hits on 100 keys): `λ-memo` 645 → 382 ms (1.69×); `memoize/arg` 425 → 272 ms (1.55×) as a dedicated procedure, only 425 → 383 as an alias.

#### B-O3. sbral tree lookup/update take their arguments through `λ-match/first` — **medium** — `src/aux.fds.sbral.scm:41`

```scheme
;; src/aux.fds.sbral.scm:41-56: sbral-tree-lookup / sbral-tree-update are (λ-match/first ...),
;; i.e. (λ args (match/first args ...)) (src/aux.base.scm:151): a 3- or 4-element rest list consed
;; and destructured at every tree level of every μkanren-state-find / μkanren-update/sbral
;; (src/aux.kanren.micro.scm:55-61, :80-84, :104-114).
```

Why: this is the innermost loop of the substitution walk; the cost is created by the base macro, the fix lives in the dependency. Both procedures are module-internal, so the arity change has no external callers.

```scheme
(define (sbral-tree-lookup w i tree)
  (match/first tree
    (((,v . _) ⊣ (fx= i 0)) v)
    ((,v ,α ,β) (let1 (w/2 (quotient w 2))
                  (cond
                    ((<= i w/2) (sbral-tree-lookup w/2 (- i 1) α))
                    (else (sbral-tree-lookup w/2 (- i 1 w/2) β)))))
    (else (error "sbral-tree-lookup: not a valid sbral"))))

(define (sbral-tree-update w i y tree)
  (match/first tree
    (((_ . ,αβ) ⊣ (fx= i 0)) `(,y . ,αβ))
    ((,v ,α ,β)  (let1 (w/2 (quotient w 2))
                   (cond
                     ((<= i w/2) `(,v ,(sbral-tree-update w/2 (- i 1) y α) ,β))
                     (else `(,v ,α ,(sbral-tree-update w/2 (- i 1 w/2) y β))))))
    (else (error "sbral-tree-update: not a valid sbral"))))
```

Verdict: reproduced. Suites pass (plus fds.sbral 6). Measured (median of 3 interleaved rounds): `sbral-ref` 1000 refs ×2000 1659–1684 → 1335–1391 ms (−18%); `update/sbral` ×500 518–528 → 401–421 ms (−21%); engine `append°` 201 splits −10%; `member°` 1000 answers −3%. A plain `cond`/`car`/`caddr` version is a further ~30% (finder), at the cost of the module's style.

#### B-O4. `exists` builds its loop from `λ1-match/first` — **low** — `src/aux.base.scm:372`

```scheme
;; src/aux.base.scm:372-377: two clause attempts per element, each destructuring the pair and
;; allocating a failure thunk. Consumers: src/aux.kanren.micro.scm:279 (μkanren-ext-D), :319 (μkanren-subsume).
;; :379-384 prefix-with-respect-to is the same letrec+μ shape around a named let (no callers).
```

```scheme
(define (exists pred?)
  (μ lst
    (let E ((lst* lst))
      (cond
        ((null? lst*) #f)
        ((pred? (car lst*)) #t)
        (else (E (cdr lst*)))))))

(define ((prefix-with-respect-to s) s*)
  (let P ((s* s*))
    (if (or (null? s*) (eq? s* s)) '() (cons (car s*) (P (cdr s*))))))
```

Verdict: reproduced as a simplification. Suites pass. Measured: `exists` 1000 elements ×20000 555 → 268 ms (2.07×); `prefix-with-respect-to` 316 → 250 ms. End-to-end on a 15-`≠°` all-different query (720 solutions): 2405 → 2440 ms — no engine gain, constraint stores are a handful of entries long.

#### B-O5. `enumerate` mutates boxed closure variables — **low** — `src/aux.base.scm:413`

```scheme
;; src/aux.base.scm:413-416
(define (enumerate lst)
  (let ((index 0) (result '()))
    (for-each (λ (v) (push! (list index v) result) (add1! index)) lst)
    (reverse result)))
```

```scheme
(define (enumerate lst)
  (let E ((lst* lst) (index 0) (result '()))
    (cond
      ((null? lst*) (reverse result))
      (else (E (cdr lst*) (fx+ index 1) (cons (list index (car lst*)) result))))))
```

Verdict: reproduced. Suites pass. Measured (1000 elements ×20000): 681–690 → 259–267 ms (2.6×); 10 elements ×2e6: 418 → 245 ms. Do not use `(map list (iota (length lst)) lst)`: 1033–1054 ms, slower than the current code. No in-repo caller (`enumerate°` in the tests is the unrelated kanren macro).

#### B-O6. `sort/lex<=?` calls `->string` twice per comparison — **low** — `src/aux.base.scm:404`

```scheme
;; src/aux.base.scm:403-404 — 2·n·log n string-port conversions; four sorts per reified answer
;; (src/aux.kanren.micro.scm:188, :204, :206, :208)
(define (lex<=? x y) (string<=? (->string x) (->string y)))
(define (sort/lex<=? ls) (sort ls lex<=?))
```

```scheme
(define (lex<=? x y) (string<=? (->string x) (->string y)))
(define (sort/lex<=? ls)
  (let* ((decorated (map (λ (x) (cons (->string x) x)) ls))
         (sorted (sort decorated (λ (a b) (string<=? (car a) (car b))))))
    (map cdr sorted)))
```

Verdict: reproduced. Suites pass; ordering identical including `->string` collisions (`'a` vs `"a"`). Measured: n=3 358 → 373 ms (4% loss); n=10 834 → 549; n=100 2009 → 1042; n=1000 3027 → 1539 (1.97×). End-to-end reification (300 queries, 8 vars, 10 `≠°`, 8 `symbol°`, 3 `absent°`): 1640 → 1640 ms — the engine's sorted lists are tiny.

#### B-O7. `pairwise-different?` is O(n²) — **low** — `src/aux.base.scm:310`

```scheme
;; src/aux.base.scm:310-314: (member? (car lst) (cdr lst)) at every step. No caller in src/ or src/test/.
```

```scheme
;; O(n) with equal? semantics via srfi-69 (already imported). Slower than the linear scan below ~n=60.
(define (pairwise-different? lst)
  (let1 (seen (make-hash-table equal? equal?-hash))
    (let P ((lst* lst))
      (cond
        ((null? lst*) #t)
        ((hash-table-exists? seen (car lst*)) #f)
        (else (hash-table-set! seen (car lst*) #t) (P (cdr lst*)))))))
```

Verdict: reproduced. Suites pass. Measured per call: n=10 0.49 → 2.4 µs (5× slower); n=100 28.6 → 18.1 µs; n=1000 2.61 → 0.41 ms; n=10000 259 → 5.8 ms (45×). Hash-table initial size has no effect; a linear-prefix hybrid is not better.

## `(aux kanren micro)`

`src/aux.kanren.micro.scm` is a μKanren with a cKanren-style constraint store. A state is `(vars-count S D A T tags)`: `S` a skew-binary random-access list (`aux.fds.sbral`) indexed by variable, `D` disequalities (one list of `(var . term)` pairs = one disjunction), `A` type tags (`symbol°`, `number°`), `T` absento tags, `tags` the type tags seen so far. Working variables have index ≥ 0, reified ones negative indices printed as Greek letters. Goals map a state to an `(aux stream)` promise; `fresh°`/`and°`/`or°`/`cond°`/`if°`/`project°` are syntax over `freshª`/`andª`/`orª`; `groupby°`/`window°`/`set°`/`enumerate°` fold an answer stream into srfi-69 tables. Answers reify to `(λ (α β ...) constraint-form ... repr)`, which `°->list/ground` evaluates. Verification of the constraint findings used the stores directly (the module exports `*`).

### Errors

#### K1. Any answer containing a vector crashes: vector-lib's `vector-map` passes the index — **high** — `src/aux.kanren.micro.scm:147`

```scheme
;; src/aux.kanren.micro.scm:147 — A is the 1-ary walker (let A ((w v)) ...)
((vector? w*) (vector-map A w*))
```
```
csi> (vector-map (λ args args) #(a b))            => #((0 a) (1 b))          ; SRFI-43
csi> (°->list #f (fresh° (q) (=° q (vector 1 2))))
Error: bad argument count - received 2 but expected 1: #<procedure (A w)>
```

Why: `(chicken base)` has no `vector-map`; the module imports vector-lib (`:10`), whose `vector-map` is SRFI-43. `μkanren-state-find/value` is on the path of every projection and of `project°`/`groupby°`/`window°`/`set°`/`enumerate°`, so unification with vectors works but no answer holding a vector can be produced. The only vector test (`test/=°/structure/vector`) never has a vector in the answer. Note the current `μkanren-state-find/repr` at `:160` already uses `(map A (vector->list w*))`.

```scheme
((vector? w*) (list->vector (map A (vector->list w*))))
;; regression tests for src/test/microkanren.scm:
;; (⊦= '((λ () (vector 1 2))) (°->list #f (fresh° (q) (=° q (vector 1 2)))))
;; (⊦= '((λ (α) (vector 1 α))) (°->list #f (fresh° (q r) (=° q (vector 1 r)))))
```

Verdict: reproduced. Suites pass. (K-R1 below subsumes this line.)

#### K2. `absent°` is discarded when the same variable gets `symbol°` — **high** — `src/aux.kanren.micro.scm:294`

```scheme
;; src/aux.kanren.micro.scm:290-296
(let ((equal/α? (μ t (equal? (lhs t) α))) (tags (μkanren-state-tags s)))
  (match/first A
    (() (let1 (T* (remove equal/α? T)) `(,D . ,T*)))          ; drops every absento of α
    ((((,α* . ,tag) . _) ⊣ (and (equal? α* α) (member? tag tags))) ...)
    ...))
;; :397-406 μkanren-subsume-A stores tags* = (cons tag tags) in the NEW state but calls
;; μkanren-update-D/T with the OLD state s, so the guard never sees the tag being added.
```
```
csi> (°->list #f (fresh° (q) (absent° 'closure q) (symbol° q) (=° q 'closure)))
((λ () (quote closure)))                         ; expected ()
store after (absent° 'closure q)(symbol° q):  D () T () tags (μkanren-tag/sym)   ; absento gone
store after (symbol° q)(absent° 'closure q):  D (((_0 . #<μkanren-tag>)))        ; see K3
```

Why: when a variable acquires a type tag its absento entries must become disequalities (`symbol°`: α ≠ name) or be dropped only when vacuous (`number°`). The base case throws them away and the guard compares against the previous state's tags. Together with K3 this means `absent°` + `symbol°` never yields a constraint. The quine expectations in `src/test/microkanren-untagged.scm:103-131` and `src/test/microkanren-show.scm:138,153` enshrine the lost `(≠ α closure)`.

```scheme
;; src/aux.kanren.micro.scm:290-296 — keep α's absento entries, stop gating on the stale tags list
(define (μkanren-update-D/T α D A T s)
  (match/first A
    (() `(,D . ,T))
    ((((,α* . _) . _) ⊣ (equal? α* α)) (μkanren-update-D/T+ α '() D T s))
    ((_ . ,A*) (μkanren-update-D/T α D A* T s))))
;; MUST be applied together with K3 (μkanren-ext-D); alone it fixes nothing observable.
;; The state's `tags` field and tags* in μkanren-subsume-A (:397-406) then become dead.
```

Verdict: reproduced. Suites: with K2 + K3 applied all repro queries return `()`; microkanren 7 and aggregation 6 pass, untagged fails 1 and show fails 2 — exactly the three stale quine expectations, which must be updated in the same commit (see K3).

#### K3. `μkanren-ext-D` stores the tag record instead of its name — **high** — `src/aux.kanren.micro.scm:282`

```scheme
;; src/aux.kanren.micro.scm:275-282
(define (μkanren-ext-D α tag D s)
  ... (else (cons `((,α . ,tag)) D)))        ; tag is a μkanren-tag record, not the symbol
```
```
csi> (°->list #f (fresh° (q) (symbol° q) (absent° 'closure q) (=° q 'closure)))
((λ () (quote closure)))                         ; expected ()
csi> (°->list #f (fresh° (q) (symbol° q) (absent° 'closure q)))
((λ (α) (assert (every (μ v (symbol? v)) (list α))) α))   ; no (deny (equal? α 'closure))
```

Why: `μkanren-verify-D+` (`:386`) re-unifies the pair's rhs with the variable's value; a record never unifies with a symbol, so the constraint is treated as unviolable and dropped, and at projection `μkanren-subsume` (`:318`) asks `(symbol? <record>)` ⇒ `#f` and removes it as "subsumed by the type".

```scheme
(define (μkanren-ext-D α tag D s)
  (let* ((name (μkanren-tag-name tag))
         (E (λ1-match/first
              (((,α* . ,u)) (and (equal? α (μkanren-state-find α* s)) (equal? u name)))
              (else #f))))
    (if ((exists E) D) D (cons `((,α . ,name)) D))))
;; delete the now-dead clause at :313 in μkanren-subsumed-pr?:
;;   ((and (μkanren-tag? u*) (μkanren-tag? u) (μkanren-tag-equal? u u*)))
;; (μkanren-ext-D was the only producer of tag records inside D; grep: μkanren-tag? at 277/313/314 only)

;; test expectations that encode the buggy quine answer, each gaining the derived (≠ α closure):
;;   src/test/microkanren-untagged.scm:104 and :241 — insert (begin (deny (equal? α 'closure)))
;;     before (begin (deny (equal? α 'list)))
;;   src/test/microkanren-show.scm:138 — '((≠ α closure) (≠ α list) (≠ α quote) (symbol? α))
;;   src/test/microkanren-show.scm:153 — substring "constraints: `(≠ α closure)`, `(≠ α list)`"
;; regression: (⊦= '() (°->list #f (fresh° (q) (symbol° q) (absent° 'closure q) (=° q 'closure))))
```

Verdict: reproduced. Suites pass (32/32) once the three expectations are updated; the new quine answer is the canonical miniKanren one (`(=/= ((_.0 closure)) ((_.0 list)) ((_.0 quote))) (sym _.0)`).

#### K4. `absent°` does not look inside vectors and records — **high** — `src/aux.kanren.micro.scm:444`

```scheme
;; src/aux.kanren.micro.scm:444 — μkanren-absento+, vectors/records fall to the atom case
(,u* (if (and (symbol? u*) (equal? u* (μkanren-tag-name tag))) #f s))
;; :342-343 — μkanren-verify-T+
; perhaps we should also handle vectors and record-instances here ...
(,u (μ T₀ (and (μkanren-tag-pred? tag u) T₀)))
```
```
csi> (length (§->list ((fresh° (q) (absent° 'a q) (=° q (vector 'a))) μkanren-state-empty)))   => 1   ; expected 0
csi> (°->list #f (fresh° (q) (=° q (make-record-instance 'box 'a)) (absent° 'a q)))  => ((λ () (make-record-instance 'box 'a)))
csi> (absent? 'a (vector 'a))  => #f                               ; the reified assert disagrees with the engine
```

Why: unification (`:132-134`), the occurs check, `μkanren-anyvar?` (`:225-231`) and reification all descend into vectors and records, and the reified `(assert (absent? 'tag α))` descends too; only the two absento traversals stop, so the engine accepts states its own answer rejects.

```scheme
;; μkanren-verify-T+, before :343 (after the var and cons clauses — a working var is itself a record)
((,u ⊣ (vector? u)) (μkanren-verify-T+ (vector->list u) T s))
((,u ⊣ (record-instance? u)) (μkanren-verify-T+ (vector->list (record->vector u)) T s))
;; μkanren-absento+, before :444
((,u* ⊣ (vector? u*)) (μkanren-absento+ (vector->list u*) tag D A T s))
((,u* ⊣ (record-instance? u*)) (μkanren-absento+ (vector->list (record->vector u*)) tag D A T s))
```

Verdict: reproduced. Suites pass. `record->vector` puts the type name at index 0, so a tag equal to the record's type symbol counts as present — consistent with `absent?` and the unifier, worth a comment. Regression cases: `(absent° 'a q) (=° q (vector 'a))` ⇒ no answer; `(fresh° (q x) (=° q (vector x)) (absent° 'a q) (=° x 'a))` ⇒ none; `(=° q (vector 'b)) (absent° 'a q)` ⇒ one.

#### K5. Reification crashes past 24 free variables — **medium** — `src/aux.kanren.micro.scm:43`

```scheme
;; src/aux.kanren.micro.scm:39-43
(define (μkanren-var->symbol α) ... (else (vector-ref greek-alphabet/lowercase i)))
```
```
csi> (μkanren-var->symbol (make-μkanren-var -25))   => Error: out of range (#(α ... ω) 24)
csi> (display (make-μkanren-var -25))               => #<Error in printer of record type ...: out of range>
25 distinct fresh variables in one answer abort the whole stream; 23 work.
```

```scheme
(else (let* ((n (vector-length greek-alphabet/lowercase))
             (g (vector-ref greek-alphabet/lowercase (modulo i n)))
             (k (quotient i n)))
        (if (zero? k) g (symbol-append g (string->symbol (number->string k))))))
```

Verdict: reproduced; `(-1 -24 -25 -48 -49 -73)` ⇒ `(α ω α1 ω1 α2 α3)`, never colliding with working `_N` names. Suites pass.

#### K6. `°->list/ground` evaluates the answer λ with placeholder symbols in the caller's environment — **medium** — `src/aux.kanren.micro.scm:617`

```scheme
;; src/aux.kanren.micro.scm:617
(grounded (map (λ (expr) (let ((E (eval expr)) (args (cadr expr))) (apply E args))) sols))
```
```
csi> (°->list/ground (fresh° (q) (number° q)))   => Error: assertion failed: (every (μ v (number? v)) (list α))
csi> (°->list/ground (fresh° (q) (≠° q 'α)))     => Error: assertion failed: (not (equal? α (quote α)))
csi -q -e '(import (aux kanren micro))' -e '(print (°->list/ground (fresh° (q) (symbol° q))))'
Error: unbound variable: λ                       ; works only if the caller imported (aux base) and srfi-1
```

Why: the λ is applied to the variable *names* as values, so every constraint is re-checked against symbols; `eval` without an environment uses the interaction environment. The tests pass because every test file imports `(aux base)` and srfi-1. `src/aux.kanren.micro.show.scm:12` documents grounding as "silently drops every constraint".

```scheme
;; add (chicken eval) to the imports, replace :614-618 with
; the environment in which grounded answers are evaluated: the repr only needs `cons`, `vector`, `quote`
; and `make-record-instance`, all imported here. (`τ` delays the lookup to call time.)
(define μkanren-answer-environment (τ (module-environment 'aux.kanren.micro)))

(define (°->list grounded g)
  (let1 (sols (§->list (°->§ g)))
    (cond
      (grounded (map (λ (expr)
                       (let ((E (eval `(lambda ,(cadr expr) ,(last expr)) (μkanren-answer-environment)))
                             (args (cadr expr)))
                         (apply E args)))
                     sols))
      (else sols))))
```

Verdict: reproduced; all three cases return `(α)` under an engine-only import. Suites pass. A further step removes `eval`: move `μkanren-repr->datum` (`src/aux.kanren.micro.show.scm:41-49`) into the engine and ground with it.

#### K7. A tuple disequality is reified as a conjunction of denies — **medium** — `src/aux.kanren.micro.scm:183`

```scheme
;; src/aux.kanren.micro.scm:180-184 — one (deny (equal? ...)) per pair of d
(f (λ (d) `(begin ,@(group->deny-list d))))
```
```
csi> (°->list #f (fresh° (q p r) (≠° (list p r) '(1 2)) (=° q (list p r))))
((λ (α β) (begin (deny (equal? α 1)) (deny (equal? β 2))) (cons α (cons β '()))))
engine accepts p=1,r=3 and p=0,r=2, rejects only (1 2);  ((eval <λ>) 1 3) => assertion failed
```

Why: a `d` with several pairs means ¬(α=u ∧ β=v), which `μkanren-verify-D+` (`:386-389`) implements by re-unifying the whole list; the executable form asserts both inequalities, so any consumer that evaluates the λ rejects valid instances. The show layer already reads the group as a tuple `(≠ (β γ) (2 1))` (`src/aux.kanren.micro.show.scm:60-64`).

```scheme
(define (μkanren-drop-dot-D D s)
  (let* ((pair->equal (λ1-match/first ((,α . ,u) `(equal? ,(μkanren-var->symbol α) ,(μkanren-state-find/repr u s)))))
         (f (λ (d) (match/first (map pair->equal d)
                     ((,e) `(begin (deny ,e)))
                     (,es `(begin (deny (and ,@es))))))))
    (map f D)))

;; src/aux.kanren.micro.show.scm:60-64 — read the new shape
((μkanren-tagged? c 'begin)
  (let* ((denied (cadr (cadr c)))                                    ; (equal? α repr) | (and (equal? ...) ...)
         (equalities (if (μkanren-tagged? denied 'and) (cdr denied) (list denied)))
         (ls (map cadr equalities))
         (rs (map (o μkanren-repr->datum caddr) equalities)))
    (if (one? (length ls)) `(≠ ,(car ls) ,(car rs)) `(≠ ,ls ,rs))))
;; tests: src/test/microkanren-untagged.scm:174 and :178 -> (begin (deny (and (equal? β 2) (equal? γ 1)))) etc.;
;;        src/test/microkanren-show.scm:80-81 feed the `and` shape to μkanren-form->constraint
```

Verdict: reproduced; after the change `E(1 3)` ⇒ `(1 3)`, `E(0 2)` ⇒ `(0 2)`, `E(1 2)` raises. Suites pass with the test updates.

#### K8. `enumerate°` numbers answers from the end of the stream — **medium** — `src/aux.kanren.micro.scm:600`

```scheme
;; src/aux.kanren.micro.scm:598-602
(F (λ (s* H) (let1 (key (list i ...)) (add1! i) (cons (apply A key) H))))
(v (reverse (foldr§ F '() §)))
;; foldr§ (src/aux.stream.scm:64-68) is (f (car s) (foldr§ f init (cdr s))): F runs on the LAST state first
```
```
csi> (°->list/ground (fresh° (r) (fresh° (x) (enumerate° (c (λ (i k) (list i k))) over (x)
        from (or° (=° x 'first) (=° x 'second) (=° x 'third)) => (=° r c)))))
(((0 third) (1 second) (2 first)))
over empsalary° (stream order develop×5, personnel×2, sales×3): a full reversal
```

Why: the `reverse` shows the author expected stream order, but the mutable counter is bumped as `foldr§` unwinds. `src/test/microkanren-aggregation.scm:65-76` (`test/enumerate°`) enshrines the reversed numbering.

```scheme
;; :592-603 — replace the (i 0), F and (v (reverse (foldr§ F '() §))) bindings with
(keys (foldr§ (λ (s* H) (cons (list (μkanren-state-find/value k s*) ...) H)) '() §))
(v (map (λ (i key) (apply A i key)) (ι (length keys)) keys))
;; and flip test/enumerate° (src/test/microkanren-aggregation.scm:66-75) to
;; ((0 (develop)) (1 (develop)) (2 (develop)) (3 (develop)) (4 (develop)) (5 (personnel)) (6 (personnel)) (7 (sales)) (8 (sales)) (9 (sales)))
```

Verdict: reproduced; patched output `((0 first) (1 second) (2 third))`. Suites pass with the flipped expectation.

#### K9. `μkanren-subsumed-pr?` consults only the first A/T entry of a variable — **low** — `src/aux.kanren.micro.scm:311`

```scheme
;; src/aux.kanren.micro.scm:311 — T may hold several (α . tag) entries, assoc sees one
((,α . ,u)  (match/first (assoc α A/T) ...
  ...))
```
```
csi> (°->list #f (fresh° (q) (absent° 'a q) (absent° 'b q) (≠° q 'a)))
((λ (α) (begin (deny (equal? α 'a))) (assert (absent? 'b α)) (assert (absent? 'a α)) α))   ; redundant deny
csi> ... (≠° q 'b) ...  => no deny                    ; only the newest absento subsumes
```

Why: the answer form depends on the order the absentos were stated; not unsound, but non-canonical and order-sensitive for tests. `A` is unaffected (`μkanren-ext-A` rejects a second tag per variable).

```scheme
(define (μkanren-subsumed-pr? A/T)
  (λ1-match/first
    (((_ . ,α) ⊣ (μkanren-var-working? α)) #f)
    ((,α . ,u)  (any (λ1-match/first
                       (((,α* . ,u*) ⊣ (equal? α α*))
                         (cond
                           ((and (μkanren-tag? u*) (μkanren-tag? u) (μkanren-tag-equal? u u*)))
                           ((and (μkanren-tag? u*) (μkanren-tag-pred? u* u)) #f)
                           (else #t)))
                       (else #f))
                     A/T))))
```

Verdict: reproduced; all six order permutations then give the same answer with no deny. Suites pass. (Drop the tag-equal clause if K3 is applied.)

#### K10. Numeric unification uses `=`, reified constraints use `equal?` — **low** — `src/aux.kanren.micro.scm:123`

```scheme
;; src/aux.kanren.micro.scm:123
((and (number? u*) (number? v*) (= u* v*)) (return))
```
```
csi> (°->list #f (fresh° (q) (=° q 1) (=° q 1.0)))   => ((λ () 1))
csi> (°->list #f (fresh° (q) (≠° q 1) (=° q 1.0)))   => ()            ; engine: 1.0 violates q ≠ 1
csi> ((eval (car (°->list #f (fresh° (q) (≠° q 1))))) 1.0)  => 1.0    ; (deny (equal? α 1)) accepts 1.0
```

```scheme
((and (number? u*) (number? v*) (eqv? u* v*)) (return))
;; or fold it into the first clause: ((eqv? u* v*) (return)) replacing (eq? u* v*), and delete this line;
;; the string=? clause stays (eqv? on strings is identity)
```

Verdict: reproduced; both variants make the engine agree with its reified answers. Suites pass.

### Refactorings

#### K-R1. The vector/record rule is spelled six times; `find/value` and `find/repr` rebuild an `A*` closure per node — **medium** — `src/aux.kanren.micro.scm:132`

```
:92-93 occur?; :132-135 four unify clauses (vector×vector, record×record, vector×record, record×vector);
:141-149 and :151-154 (let ((A* (o (map/curry A) vector->list record->vector)) ...) at every node;
:176-177 reify; :229-230 μkanren-anyvar?
```

Why: every walker treats a vector or record as "the list of its slots"; the unify version needs four clauses because a record is first turned into a vector and only then into a list. One predicate and one converter make each site a single clause and remove the per-node allocation. The `find/value` rewrite also fixes K1.

```scheme
;; before μkanren-state-update (:86)
; A vector or a record instance is walked as the list of its slots (a record's first slot is its type tag).
(define (μkanren-structure? v) (or (vector? v) (record-instance? v)))
(define (μkanren-structure->list v) (vector->list (if (vector? v) v (record->vector v))))

;; occur? (:92-93), reify (:176-177), anyvar? (:229-230): one clause each
((μkanren-structure? v*) (occur? (μkanren-structure->list v*)))
((μkanren-structure? w*) (R (μkanren-structure->list w*) r c vars assocs))
((,v ⊣ (μkanren-structure? v)) (anyvar? (μkanren-structure->list v)))
;; unify, replacing :132-135 (working variables are handled at :124-131, so this clause sees only ground
;; vectors/records — and, as today, a reified var record, which never reaches unify in practice)
((and (μkanren-structure? u*) (μkanren-structure? v*))
  (μkanren-state-unify (μkanren-structure->list u*) (μkanren-structure->list v*) s assocs))
;; find/value (:141-149)
(define (μkanren-state-find/value v s)
  (let A ((w v))
    (let1 (w* (μkanren-state-find w s))
      (cond
        ((pair? w*) (cons (A (car w*)) (A (cdr w*))))
        ((vector? w*) (list->vector (map A (μkanren-structure->list w*))))
        ((and (record-instance? w*) (not (μkanren-var? w*))) (apply make-record-instance (map A (μkanren-structure->list w*))))
        (else w*)))))
;; find/repr (:151-162): drop A*, use let1, and
((record-instance? w*) (cons 'make-record-instance (map A (μkanren-structure->list w*))))
```

Verdict: reproduced; differential run of record×vector unification, occurs check and reification identical wherever baseline does not crash. Suites pass.

#### K-R2. `μkanren-rem-subsumed` and `μkanren-rem-subsumed-T` are the same loop; `μkanren-subsumed-T?` is `exists` — **medium** — `src/aux.kanren.micro.scm:218`

```
:218-223 and :246-251 differ only in the subsumption predicate; :211-216 is a hand-written (exists pred?)
```

```scheme
(define (μkanren-subsumed-T? x tag T)
  ((exists (λ1-match/first ((,y . ,tag*) (and (equal? x y) (μkanren-tag-equal? tag tag*))))) T))

; drops from a store every element subsumed by another one (before or after it).
(define ((μkanren-rem-subsumed/by subsumed?) L0)
  (let loop ((L L0) (L+ '()))
    (match/first L
      (() L+)
      (((,x . ,L*) ⊣ (or (subsumed? x L*) (subsumed? x L+))) (loop L* L+))
      ((,x . ,L*) (loop L* (cons x L+))))))

(define μkanren-rem-subsumed-T (μkanren-rem-subsumed/by (λ (t T) (μkanren-subsumed-T? (lhs t) (rhs t) T))))
;; keep μkanren-anyvar? and μkanren-subsumed? unchanged; replace the loop at :246-251 with
;; (must come after μkanren-subsumed?, it is evaluated at load time)
(define μkanren-rem-subsumed (μkanren-rem-subsumed/by μkanren-subsumed?))
```

Verdict: reproduced; 20,000 random stores give 0 mismatches between old and new `rem-subsumed-T`. Suites pass.

#### K-R3. `groupby°` and `window°` duplicate the hash-table fold verbatim — **medium** — `src/aux.kanren.micro.scm:550`

```
:550-557 and :564-571 are byte-identical; the key `(list (μkanren-state-find/value k s*) ...)` is at :553, :567, :572, :584, :599
```

Why: a 12-line block inside a macro template is expanded at every use site and cannot be tested on its own. `foldr§` forces promises, so the `δ` around `(g s)` in `groupby°` is unnecessary.

```scheme
; the values of `vars` in the state `s`, as a list (the key of a group).
(define (μkanren-state-find/values vars s) (map (λ (v) (μkanren-state-find/value v s)) vars))

; folds the stream `§` into a hash table `key → hash table (var → values)`, collecting the value of each of `vars`.
(define (μkanren-group§ key-of vars §)
  (foldr§ (λ (s* H)
            (hash-table-update!/default H (key-of s*)
              (λ (group)
                (for-each (λ (v) (hash-table-update!/default group v (λ (vs) (cons (μkanren-state-find/value v s*) vs)) '())) vars)
                group)
              (make-hash-table))
            H)
          (make-hash-table) §))

(define-syntax-rule (literal over from =>) (groupby° (((v* aggr) v) ...) over (k ...) from g => f ...)
  (λ (s)
      (let* ((ht (μkanren-group§ (λ (s*) (μkanren-state-find/values (list k ...) s*)) (list v ...) (g s)))
             (G (λ (key group folded) (or° (let ((v* (aggr (hash-table-ref group v))) ...) (receive (k ...) (apply values key) (and° f ...))) folded)))
             (g* (hash-table-fold ht G ✗°)))
        (δ (g* s)))))

(define-syntax-rule (literal over from =>) (window° (((v* aggr) v) ...) over (k ...) from g => f ...)
  (λ (s)
      (let* ((§ (δ (g s)))
             (key-of (λ (s*) (μkanren-state-find/values (list k ...) s*)))
             (ht (μkanren-group§ key-of (list v ...) §))
             (G (λ (s*) (let* ((group (hash-table-ref ht (key-of s*)))
                               (v* (aggr (hash-table-ref group v))) ...)
                          ((and° f ...) s*)))))
        (append-map§ G §))))
;; set° (:584):       (let1 (key (μkanren-state-find/values (list k ...) s*)) ...)
;; enumerate° (:599): (let1 (key (cons i (μkanren-state-find/values (list k ...) s*))) ...)
```

Verdict: reproduced. Suites pass. −26 template lines, +14 lines of ordinary procedures.

#### K-R4. `verify-D`, `verify-A`, `verify-T` share one right-fold-until-`#f` skeleton; `verify-T+` receives the whole store to read one tag — **medium** — `src/aux.kanren.micro.scm:331`

```
:345-349, :365-376, :391-395 share the (() '()) / (⊣ (recur rest)) => step / (else #f) shape;
:331-332 μkanren-verify-T+ extracts the tag from the head of the store passed in; :340 threads the store only for that.
Call sites: verify-T :348 :355; verify-A :368 :380; verify-D :394 :464; verify-T+ :340 :348 — all in-module.
```

```scheme
; a right fold over a store that stops at the first `#f` returned by `step`
; (the stores D, A and T are always proper lists, so no `else` clause is needed).
(define (μkanren-foldr/verify step L)
  (let F ((L L))
    (match/first L
      (() '())
      ((,x . ,L*) (cond ((F L*) => (μ acc (step x acc))) (else #f))))))

; NOTE: the module exports `*`, so this changes the signature of μkanren-verify-T+ from (α T s) to (α tag s)
(define (μkanren-verify-T+ α tag s)
  (match/first (μkanren-state-find α s)
    ((,α* ⊣ (μkanren-var-working? α*))  (μ T₀ (cond ((μkanren-ext-T+ α* tag T₀ s) => (μ T+ (append T+ T₀))) (else #f))))
    ((,au . ,du)  (μ T₀ (cond (((μkanren-verify-T+ au tag s) T₀) => (μkanren-verify-T+ du tag s)) (else #f))))
    (,u (μ T₀ (and (μkanren-tag-pred? tag u) T₀)))))     ; add the K4 vector/record clauses above this one

(define (μkanren-verify-T T s)
  (μkanren-foldr/verify (λ (t T₀) ((μkanren-verify-T+ (lhs t) (rhs t) s) T₀)) T))

(define (μkanren-verify-A+ α tag s)
  (μ A0
    (let1 (α* (μkanren-state-find α s))
      (cond
        ((μkanren-var-working? α*)  (cond ((μkanren-ext-A α* tag A0 s) => (μ A+ (append A+ A0))) (else #f)))
        (else (and (μkanren-tag-pred? tag α*) A0))))))

(define (μkanren-verify-A A s)
  (μkanren-foldr/verify (λ (a A0) ((μkanren-verify-A+ (lhs a) (rhs a) s) A0)) A))

(define (μkanren-verify-D D s)
  (μkanren-foldr/verify (λ (d D**) (μkanren-verify-D+ d D** s)) D))
```

Verdict: reproduced. Suites pass.

#### K-R5. State records are rebuilt field-by-field at six sites — **medium** — `src/aux.kanren.micro.scm:300`

```
make-μkanren-state at :100, :304, :404, :406, :455, :483 — every site copies six fields to change one or two
```

```scheme
;; after :51
; a copy of `s` where the given fields are replaced.
(define (μkanren-state-with s #!key
                            (vars-count (μkanren-state-vars-count s))
                            (S (μkanren-state-S s))
                            (D (μkanren-state-D s))
                            (A (μkanren-state-A s))
                            (T (μkanren-state-T s))
                            (tags (μkanren-state-tags s)))
  (make-μkanren-state vars-count S D A T tags))

;; :96-101 μkanren-state-update
(let* ((S* (μkanren-update/sbral α v (μkanren-state-S s)))
       (s* (μkanren-state-with s S: S*))
       (assocs* (cons `(,α . ,v) assocs)))
  `(,s* ,assocs*))
;; :298-306
(define (μkanren-subsume-T vars T+ D A T s)
  (match/first vars
    (() (μkanren-state-with s D: D A: A T: (append T+ T)))
    ((,α . ,vars*)  (match1/first ((,D* . ,T+*) (μkanren-update-D/T α D A T+ s))
                      (μkanren-subsume-T vars* T+* D* A T s)))))
;; :397-406
(define (μkanren-subsume-A tag vars D A s)
  (let1 (tags* (cons tag (μkanren-state-tags s)))
    (match/first vars
      (() (μkanren-state-with s D: D A: A tags: tags*))
      ((,α . _) (match1/first ((,D* . ,T*) (μkanren-update-D/T α D A (μkanren-state-T s) s))
                  (μkanren-state-with s D: D* A: A T: T* tags: tags*))))))
;; :455 freshª:  (s* (μkanren-state-with s vars-count: (add1 vc)))
;; :482-483 ≠°:  (s** (μkanren-state-with s D: (append D* D)))
```

Verdict: reproduced. Suites pass. Cost: CHICKEN parses `#!key` at runtime; measured 0–4% slower on an `append°`/`member°` benchmark, inside the ~7% run-to-run spread. If that matters, keep `freshª` (`:455`) and `μkanren-state-update` (`:100`) positional.

#### K-R6. `(module (aux kanren micro) *)` exports ~90 internals — **medium** — `src/aux.kanren.micro.scm:3`

```
Consumers: src/aux.kanren.micro.show.scm and src/test/microkanren*.scm only. A macro exported from a module
whose expansion references an unexported binding fails at the use site (`unbound variable: freshª`, verified
interpreted and compiled), so freshª andª orª ✓° ✗° =° and make-μkanren-tag must stay exported.
```

```scheme
(module (aux kanren micro)
  ( ; variables and states
    make-μkanren-var μkanren-var? μkanren-var-index μkanren-var-working? μkanren-var->symbol
    make-μkanren-state μkanren-state? μkanren-state-empty μkanren-state-vars-count μkanren-state-S μkanren-state-D μkanren-state-A μkanren-state-T μkanren-state-tags
    μkanren-state-match μkanren-state-find μkanren-state-find/value μkanren-state-find/repr μkanren-state-unify μkanren-project
    ; tags
    define-μkanren-tag make-μkanren-tag μkanren-tag? μkanren-tag-name μkanren-tag-def μkanren-tag-pred? μkanren-tag-equal? μkanren-tag/sym μkanren-tag/num μkanren-make-tag-A
    ; goals — freshª andª orª ✓° ✗° =° must stay exported: the exported macros expand to them at the importer's site
    ✓° ✗° freshª andª orª =° ≠° fresh° fresh°/record and° or° if° take° null° boolean° cons° symbol° number° absent° project° cond°
    ; aggregation (add μkanren-group§ μkanren-state-find/values only if K-R3 is applied — they do not exist today)
    groupby° window° set° enumerate°
    ; API
    define-relation °->§ °->list °->list/ground μkanren-run)
  ;; module body unchanged
  )
```
Only the `*` export specifier of the module form (`src/aux.kanren.micro.scm:1`) is replaced by this list; the module body is unchanged.

Verdict: reproduced (with the two non-existent names removed — the finder's list did not build). Suites pass; `μkanren-update-D/T+` becomes unbound for importers. Confidence moderate: every module of the egg uses `*` by choice.

#### K-R7. `μkanren-absento+` re-implements the tag predicate; `absent°` with a non-symbol tag fails silently — **low** — `src/aux.kanren.micro.scm:444`

```scheme
;; :444
(,u* (if (and (symbol? u*) (equal? u* (μkanren-tag-name tag))) #f s))
;; :537-538 the tag already carries (λ (v) (not (equal? tag v))); :343 and :424 use μkanren-tag-pred?
;; :532-534 (define (absent° tag u) (cond ((not (symbol? tag)) ✗°) ...
```
```
csi> (°->list #f (fresh° (q) (=° q '(1 2)) (absent° 5 q)))   => ()      ; but (absent? 5 '(1 2)) => #t
```

```scheme
;; :444
(,u* (and (μkanren-tag-pred? tag u*) s))
;; :532-541 — drop the (not (symbol? tag)) gate so any datum works like absent?
(define ((absent° tag u) s)
  (μkanren-state-match ((vc S D A T tags) s)
    (let* ((pred? (λ (v) (not (equal? tag v))))
           (tag* (make-μkanren-tag tag 'no-def pred?)))
      (cond
        ((μkanren-absento+ u tag* D A T s) => ✓°)
        (else (✗° s))))))
;; if the symbol-only restriction is intended, replace the silent ✗° with (error 'absent° "tag must be a symbol" tag)
```

Verdict: reproduced; numeric and string tags then work before and after unification. Suites pass.

#### K-R8. `μkanren-part`/`μkanren-partition*` is srfi-1 `partition` — **low** — `src/aux.kanren.micro.scm:190`

```
:190-201 two mutually recursive procedures with four accumulators; the only caller μkanren-form (:206)
immediately applies μkanren-sort-part, so intra-part variable order is unobservable.
```

```scheme
; groups the type store A by tag: `((tag . vars) ...)`.
(define (μkanren-partition* A)
  (match/first A
    (() '())
    (((_ . ,tag) . _)
      (receive (same others) (partition (λ1-match/first ((_ . ,tag*) (μkanren-tag-equal? tag tag*))) A)
        (cons `(,tag . ,(remove-duplicates (map lhs same))) (μkanren-partition* others))))))
```

Verdict: reproduced; after `μkanren-form`'s normalisation old and new agree on a synthetic store with duplicated variables and interleaved tags. Suites pass.

#### K-R9. State fields, variable indexing, `✓°`/`✗°`, the `fresh°` list form, `if°` and negative `take°` are undocumented — **low** — `src/aux.kanren.micro.scm:47`

```
:47 (define-record μkanren-state vars-count S D A T tags) with no comment; :34-37 negative-index encoding of reified
variables; :448-449 (define ✓° list) (define ✗° (K '())); :496 (fresh° α (β ...) body ...) binds α to (list β ...)
(test/microkanren.scm:38); :512-518 if° is a soft cut; :520 (take° -1 g) keeps every answer (microkanren-untagged.scm:198).
```

```scheme
;; :30
; A *working* variable has index i ≥ 0 (its slot in the substitution S, see `μkanren-var-index/sbral`);
; a *reified* one has index -1, -2, ... (allocated by `μkanren-state-reify`) and is printed as α, β, ...
; `μkanren-var-index>=0` maps both to a non-negative number via `(- (add1 i))`.
;; :47
; `vars-count` is the number of variables introduced so far; `S` is the substitution, an sbral indexed
; by variable with the most recent variable first; `D` the disequality store (a list of failed-unification
; prefixes, each a list of `(α . u)` pairs); `A` the type store and `T` the absento store (lists of
; `(α . tag)` pairs); `tags` the tags asserted so far through `A`. D, A and T are plain lists: the
; `empty/sbral` in `μkanren-state-empty` is just `'()`, only S is a real sbral.
;; :448
; A goal maps a state to a stream of states: `✓°` succeeds once with the given state, `✗°` fails.
; The `ª` suffix marks the applicative form of a goal constructor, `°` its syntactic form or a relation.
;; :492
; `(fresh° (α ...) g ...)` introduces fresh variables; `(fresh° α (β ...) g ...)` moreover binds α to the list `(β ...)`.
;; :512
; soft cut: if `g?` has at least one answer, every answer of `g?` is continued with `gt`, otherwise `gf` runs.
;; :520
; the first `n` answers of `g`; a negative `n` keeps all of them (`take§` in (aux stream) stops only when n reaches 0).
```

Verdict: reproduced (all six facts checked at runtime). Suites pass.

#### K-R10. Dead code: unused imports, `μkanren-var-reified?`, a constant `default` parameter, commented-out printers, `; ✓` markers — **low** — `src/aux.kanren.micro.scm:7`

```
:7 (chicken sort), :9 (chicken pretty-print): nothing in the module uses them (sort/lex<=? is from (aux base));
:35 μkanren-var-reified? has no caller in src/; :63 μkanren-var-extend/sbral's `default` is always
*μkanren-unbound* at its only call site (:81); :20, :49, :264 commented-out; 20 trailing `; ✓` between :275 and :419.
```

```scheme
(import scheme (chicken base) (chicken memory representation) srfi-1 srfi-69 vector-lib (aux base) (aux stream) (aux fds sbral))
;; delete :35; :63-69 ->
(define (μkanren-var-extend/sbral α S)
  (let ((i (μkanren-var-index>=0 α)))
    (let U ((l* (length/sbral S)) (S* S))
      (cond ((<= l* i) (U (add1 l*) (cons/sbral *μkanren-unbound* S*))) (else S*)))))
;; :81 -> (μkanren-var-extend/sbral α sbral); delete :20, :49, :264; sed -i 's/ *; ✓$//'
```

Verdict: reproduced (622 → 616 lines). Suites pass; the show module and tests import `(chicken sort)`/`(chicken pretty-print)` themselves.

### Optimizations

#### K-O1. `μkanren-var-index/sbral` and `μkanren-sbral-ref/var` return closures for a two-argument computation — **low** — `src/aux.kanren.micro.scm:55`

```scheme
;; src/aux.kanren.micro.scm:55-61 — curried, each applied exactly once (:82-83, :108/:112)
(define (μkanren-var-index/sbral sbral) (let1 (l (length/sbral sbral)) (μ α (- l 1 (μkanren-var-index>=0 α)))))
(define (μkanren-sbral-ref/var sbral) (let1 (index-of-var ...) (μ α (sbral-ref sbral (index-of-var α)))))
```

Why: two closure allocations per `μkanren-state-find` (every unification performs at least two). The verifier found that calling the two-argument helpers per hop recomputes `length/sbral` (O(log n)) and makes long variable chains 19% slower; keep one local closure in `μkanren-state-find` that reuses the computed `l`.

```scheme
; the position of the working variable α in the sbral `sbral` (which stores the most recent variable first).
(define (μkanren-var-index/sbral sbral α) (- (length/sbral sbral) 1 (μkanren-var-index>=0 α)))
(define (μkanren-sbral-ref/var sbral α) (sbral-ref sbral (μkanren-var-index/sbral sbral α)))

(define (μkanren-update/sbral α v sbral)
  (let1 (sbral* (μkanren-var-extend/sbral α sbral *μkanren-unbound*)) ; with K-R10 applied: (μkanren-var-extend/sbral α sbral)
    (update/sbral (μkanren-var-index/sbral sbral* α) v sbral*)))

(define (μkanren-state-find α s)
  (let* ((S (μkanren-state-S s))
         (l (length/sbral S))
         (var-deferred? (μ α (and (μkanren-var? α) (<= l (μkanren-var-index>=0 α)))))
         (ref-var (μ β (sbral-ref S (- l 1 (μkanren-var-index>=0 β))))))
    (let F ((β0 α) (β α))
      (cond
        ((var-deferred? β) β)
        ((μkanren-var-working? β) (F β (ref-var β)))
        ((μkanren-unbound? β) β0)
        (else β)))))
```

Verdict: reproduced. Suites pass. Measured (this variant vs same-toolchain control): peano300 253–257 → 203–208 ms (−19%); peano600 1055–1064 → 855; append120 75–77 → 60 (−21%); unify 2000 vars ×20 152–156 → 128 (−16%); 500-deep chain walk 933–950 → 927–934 (no regression; the finder's verbatim version was +19% there). Both exported names change arity; no other file references them.

## Suggested order of work

1. **B1** `match/non-overlapping` single evaluation — one-line macro fix, removes an exponential from the unifier; no test changes.
2. **K1 / K-R1** `vector-map` crash — adopt K-R1 (which contains the fix) or the one-liner; add the two vector regression tests.
3. **K2 + K3** absento/type-tag interplay — apply together, update the three quine expectations, drop the dead `:313` clause and the `tags` field afterwards; then **K4** (vectors/records in absento) and **K-R7** (tag predicate) touch the same code.
4. **B2** `length/>?` with the ziplist consumer; **K5** Greek overflow; **K6** `°->list/ground`; **K7** tuple disequality; **K8** `enumerate°` — each self-contained, each with a test update or addition.
5. **B-O1** vector patterns and **B-O3** sbral lookups — the two measurable performance items; **B-R2** (`=>` receiver) before any further matcher work; **B3** `define-macro` with a compiled-mode test.
6. Refactorings in the engine (K-R2, K-R3, K-R4, K-R5, K-R8, K-R10, K-R9 comments) and in base (B-R1, B-R3, B-R4), then K-R6 export list once the internal names have settled.
7. The remaining low items (B4–B9, K9, K10, B-O2, B-O4–B-O7, K-O1, B-R5) as time allows (K-O1 after K-R10: with K-R10 applied the `μkanren-var-extend/sbral` call in K-O1 becomes 2-ary, `(μkanren-var-extend/sbral α sbral)`); B-O7 and B-O6 are only worthwhile for external users.

## Appendix: refuted and dropped

Refuted by the verifier:

- `remove-duplicates` O(n²) (`src/aux.base.scm:390`): the microbenchmark holds (n=10000 340 → 7.6 ms) but instrumenting all 32 kanren cases shows 10,346 calls of length 0–4 only; the proposed `delete-duplicates` small branch is 2.4–3.4× slower on those, the hash branch is slower than the loop up to ~64 distinct elements and 3× slower on low-cardinality lists, and it relied on a boolean `length/>?` (B2). Not an improvement for the code that calls it.
- Dead `#!key unsafe` parameter of `μkanren-state-update` (`src/aux.kanren.micro.scm:86`): premise correct (no caller passes `unsafe:`), but the speedup is 8.7% only in a tight loop, 5% on `append°`, 0% on a `peano°` enumeration — under the bar; still a legitimate low-severity dead-code cleanup alongside K-R10.

Dropped at merge (`(aux base)`): match/non-overlapping double evaluation ×2 (merged into B1); length/>? consing and misleading name ×2 (B2); vector/record branch duplication in match-pattern (B-O1); absent? generic arithmetic (B4); memoize duplication (B-O2); remove-duplicates vs delete-duplicates (refuted item); exists / prefix-with-respect-to (B-O4); enumerate mutation (B-O5); dead `#|` and `#;` blocks (B7); K/K*, curry₁, Λ, letmap, mappair, foldr/concat-and-or (B-R4); trivial partial applications (snoc, cons/λ) — style; void?/not/✓/display/pp/one? simplifications — style; letassoc/cdr rule-head typo — cosmetic; define-documented's `(string->symbol "name")` — cosmetic; dmatch-run-a-thunk building its error message twice — cold path; macro definition order — readability; per-clause failure thunks in dmatch-aux — ~5%, within noise, superseded by B1.

Dropped at merge (`(aux kanren micro)`): `A*` closure per node (K-R1); enumerate° ordering from the refactoring lens (K8); °->list/ground import dependence (K6); absent° non-symbol tag (K-R7); unused imports, commented-out code and `; ✓` markers, μkanren-var-reified?, constant `default` (K-R10); `(remove-duplicates (map lhs A))` at three sites — naming nit; μkanren-drop-dot-T re-shaping pairs — one extra pass at reification; (aux base) remove-duplicates/exists vs srfi-1 — author's style choice; three conventions for returning several values — consistency nit.

## Status (2026-09-20)

All 43 findings except B2 were applied in nine batches (base-1-matcher, base-2-helpers, base-3-perf, base-4-hygiene, kanren-1-structure, kanren-2-absento, kanren-3-answers, kanren-4-refactor, kanren-5-exports), each located by content rather than by the line numbers above, followed by an independent review pass (one style-only remark, fixed) and a final rebuild. B2 is skipped by maintainer decision: in Scheme a predicate may return a value instead of `#t`, so `length/>?` keeps returning the n-th element and no other value-returning predicate (`exists`, `foldr/or`, `μkanren-subsumed-T?`, ...) was changed to return `#t`. Seven findings were applied with a deviation from the proposal as written (B-O7 hybrid, B-R1 import set, K-R5 partial, K-R6 extended export list, K-R10 without vector-lib, K4 through the K-R1 helpers, K-O1 single-closure variant); none changes the intent. The working tree touches ten files under `src/` (`git diff --stat`: 512 insertions, 347 deletions), all of them named by some batch; twenty test cases were added and the expectations of five existing cases were updated to the fixed behaviour (enumerate° order, the quine's `(≠ α closure)`, the `(deny (and ...))` shape). Private build `status` from the final tree is clean and every suite is green.

| id | status | note |
|---|---|---|
| B1 | applied | test added (`test/non-overlapping/single-evaluation` in dmatch) |
| B2 | skipped | maintainer decision: a predicate may return a value instead of `#t` |
| B3 | applied | verified with the dm2.scm probe under csc and csi (`(1 foo)`) |
| B4 | applied | test added (`test/absent?`) |
| B5 | applied | test added (`test/letport/string`) |
| B6 | applied | assertion added to `test/λ-match-first/unquote-unquote-pattern` (`,,x` against 1.5) |
| B7 | applied | `,@var` is now a `syntax-error`; dead `#;` branches removed with B-O1 |
| B8 | applied | test added (`test/foldr/avg&var`) |
| B9 | applied | documentation-only: linearity NOTE above `match-pattern` |
| B-R1 | applied with deviation | `(chicken fixnum)` kept for B4/B-O5; srfi-1 only-list extended with `concatenate any` for B-R4 |
| B-R2 | applied | test added (`test/match-first/receiver`) |
| B-R3 | applied | `λ-curry` is `μ`; asserted in `test/base/combinators` |
| B-R4 | applied | `foldr/or` keeps returning the first truthy element; regression asserts in `test/base/combinators` |
| B-R5 | applied | test added (`test/base/combinators`, fuller variant) |
| B-O1 | applied | vector arm delegates to the list arm; no test named by the report |
| B-O2 | applied | `memoize`, dedicated `memoize/arg`, `λ-memo` as proposed |
| B-O3 | applied | existing fds.sbral cases cover the rewritten tree procedures |
| B-O4 | applied | `exists` unchanged in return value; `prefix-with-respect-to` curried |
| B-O5 | applied | test added (`test/enumerate`) |
| B-O6 | applied | as proposed |
| B-O7 | applied with deviation | hybrid: linear scan up to 60 elements, srfi-69 table beyond (test added `test/pairwise-different?`) |
| K1 | applied | test added (`test/=°/structure/vector/answer`) |
| K2 | applied | test added (`test/absent°/symbol°`) |
| K3 | applied | quine expectations updated with `(≠ α closure)` in untagged and show suites |
| K4 | applied with deviation | vectors/records handled via the K-R1 helpers `μkanren-structure?`/`->list` (test added `test/absent°/structure`) |
| K5 | applied | test added (`test/var->symbol/overflow`, 26 fresh variables) |
| K6 | applied | evaluates in `(module-environment 'aux.kanren.micro)` (test added `test/ground/constrained`) |
| K7 | applied | `(deny (and ...))` shape; show reader and untagged/show expectations updated |
| K8 | applied | `test/enumerate°` expectation flipped to stream order |
| K9 | applied | test added (`test/absent°/subsumed`, order-independent) |
| K10 | applied | test added (`test/absent°/datum`) |
| K-R1 | applied | `vector-map` replaced by `(list->vector (map ...))` in the shared rule |
| K-R2 | applied | test added (`test/rem-subsumed/by`) |
| K-R3 | applied | test added (`test/find/values`); `enumerate°` keeps the K8 order |
| K-R4 | applied | test added (`test/foldr/verify`) |
| K-R5 | applied with deviation | `μkanren-state-with` only at subsume-T, subsume-A and `≠°`; `freshª`/`state-update` stay positional (test added `test/state-with`) |
| K-R6 | applied with deviation | export list extended with the K-R3 helpers and seven internals exercised by the test suite |
| K-R7 | applied | as proposed, together with K4 |
| K-R8 | applied | part variables now in store order, normalised by `μkanren-sort-part` (test added `test/partition*`) |
| K-R9 | applied | six comments added, first letter lowercased per house style |
| K-R10 | applied with deviation | `vector-lib` also dropped from the import since K-R1 left no reference to it |
| K-O1 | applied with deviation | report's single `ref-var` closure variant; both names became 2-ary |

Final suite counts, private build `status` from the working tree (`private-build.sh status`: BUILD OK; `run-suites.sh status`: ALL GREEN):

| suite | ran | failed |
|---|---|---|
| test | 22 | 0 |
| category-list | 8 | 0 |
| stream | 5 | 0 |
| letcc | 4 | 0 |
| letnondeterministic | 16 | 0 |
| delimcc | 16 | 0 |
| timsort | 9 | 0 |
| bootstrap | 9 | 0 |
| hansei | 6 | 0 |
| fds.queue | 3 | 0 |
| fds.sbral | 6 | 0 |
| dmatch | 8 | 0 |
| microkanren | 19 | 0 |
| microkanren-aggregation | 6 | 0 |
| microkanren-untagged | 7 | 0 |
| microkanren-show | 12 | 0 |
| anthropic | 110 | 0 |
| learning (csc-compiled) | 18 | 0 |
