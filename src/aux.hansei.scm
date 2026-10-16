;; (aux hansei) -- a CHICKEN port of Oleg Kiselyov and Chung-chieh Shan's HANSEI, the embedded
;; probabilistic domain-specific language (https://okmij.org/ftp/kakuritu/Hansei.html).
;;
;; Mirrors (paths relative to aux.scm/upstream/hansei/, citation keys as in the plan
;; AUX-HANSEI-PLAN.md §0):
;;   T   ptypes.mli (https://okmij.org/ftp/kakuritu/ptypes.mli): the search tree pV;
;;   PI  probM.mli and
;;   P   probM.ml (https://okmij.org/ftp/kakuritu/probM.ml): the reifier and the reflector,
;;       `dist`, `fail`, the distributions, the world-local memory;
;;   II  inference.mli and
;;   I   inference.ml (https://okmij.org/ftp/kakuritu/inference.ml): the exact, bounded and
;;       sampling inference procedures, `statistics`, `normalize`, `timeit`;
;;   PE  paper_examples.ml: the list-only `normalize`;
;;   N   nested.ml, BL blip/blip.ml: `prob_of`, `at_least`, `map_dist`, promoted;
;;   LV  logic_vars.ml, PS parsec.ml: `reify_part`, `soft_cut`, promoted.
;; The selectors draw from the OCaml-compatible generator of (aux hansei random), whose one
;; global state every `random_selector seed` re-initialises, as upstream (P:221-222, plan D6).
;; The reifier is the direct one of the paper, `reset (fun () -> pv_unit (m ()))` (DSL p.15
;; l.637), on the single delimiter of (aux continuation delimited), not probM's request loop
;; (plan D1, deviation 2); probM's memory protocol is kept as is, so that a captured
;; continuation also captures the world-local memory (P:59-75, plan §5.2).
;;
;; HANSEI code version: April 2009 (H, "Implementation"); web page last updated March 1, 2016.
;; Papers: O. Kiselyov, C. Shan, "Embedded Probabilistic Programming", IFIP DSL 2009, LNCS 5658,
;; pp. 360-384 (DSL = dsl-paper.txt / .pdf); "Monolingual Probabilistic Programming Using
;; Generalized Coroutines", UAI 2009 (EPP = embedpp.txt / .pdf); "Embedded Probabilistic
;; Programming", NIPS 2008 workshop (NIPS = nips2008.txt / .pdf); "Probabilistic programming
;; using first-class stores and first-class continuations", ML Workshop 2010 (ML).
;;
;; Contents:
;;   - the pluggable arithmetic of the weights op/times, op/plus, op/subtract, op/divide,
;;     op/greater, and the internal probcc-ratio and probcc-signal (the (exn hansei)
;;     conditions);
;;   - probcc-map, a map that may be re-entered (plan §5.6), and the parameter probcc-verbose;
;;   - the search tree (T): probcc-τ, probcc-value, probcc-unit;
;;   - the world-local memory (P:40-75): probcc-mem-empty, probcc-thread-local,
;;     probcc-mem-new-loc, probcc-mem-ref, probcc-mem-set, probcc-mem-prompt,
;;     probcc-mem-resume;
;;   - reification and reflection (P:97-148): probcc-reify/0, probcc-distribution,
;;     probcc-impossible, probcc-reflect;
;;   - distributions and evidence (P:153-214): probcc-bernoulli, probcc-coin, probcc-uniform,
;;     probcc-uniform/range, probcc-uniform/either, probcc-geometric, probcc-when,
;;     probcc-observe, probcc-geometric/count, probcc-geometric/bounded;
;;   - laziness, memoization and world-local references (P:321-370, 466-479; TI:121-126):
;;     probcc-letlazy, probcc-letlazy/nesting, probcc-memo, probcc-wref-new, probcc-wref-get,
;;     probcc-wref-set!;
;;   - exact inference (I:16-28, 296-301; P:247-248, 267-268): probcc-explore,
;;     probcc-normalize/total, probcc-normalize, probcc-reify, probcc-reify/exact/a,
;;     probcc-reify/exact, probcc-variable-elimination, λ-probcc-bucket;
;;   - tables (N:292-304, BL:456-460): probcc-prob-of, probcc-at-least, probcc-map-dist;
;;   - selectors (P:218-241): probcc-random-selector, probcc-dist-selector;
;;   - approximate inference (I:30-198; P:253-262): probcc-nearly-one, probcc-explore/shallow,
;;     probcc-first-success, probcc-explore/bounded, probcc-sample-dist/rejection,
;;     probcc-sample-runner/times, probcc-sample-dist, probcc-sample-rejection/a,
;;     probcc-sample-rejection, probcc-sample-importance/a, probcc-sample-importance;
;;   - utilities (I:276-308): probcc-statistics, probcc-timeit;
;;   - the search-tree monad and logic programming (DSL p.10 l.415-419; LV:68-70, 386-391):
;;     probcc-bind, probcc-reify/part, probcc-soft-cut;
;;   - (new) traversals: probcc-leaves, probcc-dfs.
;; Not ported (plan §2): `from_option` and `read_answer` (P:28-38), which only appease OCaml's
;; type checker; the `pReq` request type and its `interp` (P:24, 97-108), fused into the
;; answer of the reset (deviation 2); the "simple-minded reify0" of the comment at P:81-90;
;; `letlazy_old` and its `Needed` request (P:373-446); the commented-out experiments of
;; I:200-274 (`observe_la`, `sample_explore_reify*`).
;;
;; Two CHICKEN hazards for stochastic code (plan §5.6): `map` builds its result destructively,
;; so a continuation captured inside its procedure must not be resumed (use probcc-map); and
;; csi evaluates the operands of a call with five or more of them in a way that a resumed
;; continuation corrupts (bind every choice with let* first).  Compiled code has only the first.

(module (aux hansei) (op/times op/plus op/subtract op/divide op/greater
                      probcc-map probcc-verbose
                      probcc-τ probcc-value probcc-unit
                      probcc-mem-empty probcc-thread-local probcc-mem-new-loc probcc-mem-ref
                      probcc-mem-set probcc-mem-prompt probcc-mem-resume
                      probcc-reify/0 probcc-distribution probcc-impossible probcc-reflect
                      probcc-bernoulli probcc-coin probcc-uniform probcc-uniform/range
                      probcc-uniform/either probcc-geometric probcc-when
                      probcc-observe probcc-geometric/count probcc-geometric/bounded
                      probcc-letlazy probcc-letlazy/nesting probcc-memo
                      probcc-wref-new probcc-wref-get probcc-wref-set!
                      probcc-explore probcc-normalize/total probcc-normalize probcc-reify
                      probcc-reify/exact/a probcc-reify/exact probcc-variable-elimination
                      λ-probcc-bucket
                      probcc-prob-of probcc-at-least probcc-map-dist
                      probcc-random-selector probcc-dist-selector
                      probcc-nearly-one probcc-explore/shallow probcc-first-success
                      probcc-explore/bounded probcc-sample-dist/rejection
                      probcc-sample-runner/times probcc-sample-dist
                      probcc-sample-rejection/a probcc-sample-rejection
                      probcc-sample-importance/a probcc-sample-importance
                      probcc-statistics probcc-timeit
                      probcc-reify/part probcc-soft-cut probcc-bind
                      probcc-leaves probcc-dfs)

  (import scheme
          (only (scheme base) make-parameter)
          (chicken base)
          (only (chicken sort) sort)
          (only (chicken time) cpu-time)
          (only (chicken format) printf)
          (only (chicken flonum) fpsqrt)
          (only (chicken fixnum) fx= fx< fx+)
          (only (chicken condition) make-property-condition make-composite-condition abort)
          (only (chicken memory representation) record-instance? record->vector)
          (only srfi-1 fold)
          (aux base)
          (aux continuation delimited)
          (aux hansei pmap)
          (only (aux hansei random) ocaml-random-global ocaml-random-init!
                make-ocaml-random-source)
          (only (aux hakaru10 random) rng-source? rng-double!))

  ;; ---------------------------------------------------------------------------------------
  ;; Arithmetic of the weights (plan D8, C7, C8)
  ;; ---------------------------------------------------------------------------------------

  ; (new) deviation 1 (T:4): a weight is any number, exact or inexact, and the operations on
  ; weights, the product op/times, the sum op/plus and the difference op/subtract (OCaml's
  ; `*.`, `+.` and `-.`), are pluggable.  Each procedure reads a parameter once per call,
  ; where it is called (C8): a parameter read inside a model takes its value where the segment
  ; runs, that is where the explorer forces the `C` thunk, not where probcc-reify/0 was
  ; called; so bind the parameters around the explorer too.
  (define op/times (make-parameter *))
  (define op/plus (make-parameter +))
  (define op/subtract (make-parameter -))
  ; (new) deviation 10 (I:299-301): `p /. total` with one rounding of an exact ratio, the
  ; value of OCaml's for flonums, and NaN or an infinity for a zero total as OCaml, instead of
  ; raising: both operands are converted then, since CHICKEN's (/ 0 0.) is the exact 0.
  (define op/divide (make-parameter (λ (m n)
                                      (if (zero? n)
                                          (/ (exact->inexact m) (exact->inexact n))
                                          (exact->inexact (/ m n))))))
  ; (new) deviation 5 (I:28; D5): the order of the masses, by which probcc-explore and
  ; probcc-statistics sort their tables in descending mass; pluggable as the other operations.
  (define op/greater (make-parameter >))

  ; (new) deviation 19 (P:240; I:47, 132, 160, 187): `p /. ptotal`, where an exact ratio stays
  ; exact and a zero total gives NaN or an infinity as upstream (D8), instead of raising.  It
  ; is the ratio of the selectors and samplers (plan §6.2-6.5).
  (define (probcc-ratio m n) (if (zero? n) (/ (exact->inexact m) (exact->inexact n)) (/ m n)))

  ; (new) the composite (exn hansei) conditions whose hansei part carries a `kind` (plan §7):
  ; memory (P:52-54, `Not_found`), selector (P:224), malformed (a match failure upstream).
  ; Raised with `abort`, which is not continuable like OCaml's `raise` (as pmap-signal,
  ; deviation 30): a handler that returns raises "exception handler returned".
  (define (probcc-signal kind loc msg . args)
    (abort (make-composite-condition
             (make-property-condition 'exn 'location loc 'message msg 'arguments args)
             (make-property-condition 'hansei 'kind kind))))

  ;; ---------------------------------------------------------------------------------------
  ;; Utilities (plan D11, D16)
  ;; ---------------------------------------------------------------------------------------

  ; (new) deviation 20: OCaml's `List.map`, left to right, built by plain recursion so that a
  ; continuation captured inside `f` may be resumed; CHICKEN's `map` (and srfi-1's) builds its
  ; result destructively, and a resume corrupts it (plan §5.6):
  ;   (probcc-reify/exact (map (λ (x) (probcc-coin 1/2)) '(1 2)))
  ; gives the single entry (#t #t #f #f #t #f) at mass 1, the four worlds sharing one list
  ; built in place, instead of four entries at 1/4.
  (define (probcc-map f lst)
    (if (null? lst)
        '()
        (let* ((h (f (car lst)))
               (t (probcc-map f (cdr lst))))
          (cons h t))))

  ; (new) deviation 14 (I:131, 186): the samplers print their `done` lines only when true.
  (define probcc-verbose (make-parameter #f))

  ;; ---------------------------------------------------------------------------------------
  ;; The search tree (T:13-15; DSL p.9-10): ((V v) p) is a leaf, ((C thunk) p) a branch not
  ;; yet explored, deviation 1: the probability second
  ;; ---------------------------------------------------------------------------------------

  ; cf. T:13-15, the branch (p, C thunk) of the thunk with body `body`.
  (define-syntax-rule (probcc-τ p body ...) `((C ,(τ body ...)) ,p))
  ; cf. T:13-15, the leaf (p, V v).
  (define-syntax-rule (probcc-value p body ...) `((V ,(begin body ...)) ,p))
  ; cf. DSL p.10 l.414-416 `pv_unit`; deviation 26 (P:100): exact 1.
  (define (probcc-unit v) (list (probcc-value 1 v)))

  ;; ---------------------------------------------------------------------------------------
  ;; World-local memory (plan §5.2)
  ;; ---------------------------------------------------------------------------------------

  ; cf. P:44-46 `newm`, a map of int locations; deviation 15 (P:40-56): values are stored
  ; directly in a persistent map of fixnums (no closure cell, no channel).
  (define probcc-mem-empty (pmap-create fx-compare))

  ; cf. P:59-64 `thread_local`: "The first-class storage is immutable and pure functional.
  ; Only the pointer to it is mutable" (D2), and changed only with set!, never through a
  ; parameter or a dynamic-wind, which aux's shift enters again on every resume.  Upstream does
  ; not protect it against exceptions either: an error inside a model leaves it set, and the
  ; next probcc-reify/0 saves and restores whatever is there.  Deviation 15: one pointer for the
  ; whole program, as upstream (not one per srfi-18 or CML thread).
  (define probcc-thread-local probcc-mem-empty)

  ; cf. P:47-49 `genid`'s counter: the next location, from 0.
  (define probcc-mem-counter 0)

  ; cf. P:47-50 `genid`, `new_loc`: a fresh location; deviation 15: a fixnum, no channel.
  (define (probcc-mem-new-loc)
    (let1 (loc probcc-mem-counter)
      (set! probcc-mem-counter (add1 loc))
      loc))

  ; cf. P:52-54 `mref`; deviation 16 (P:53): a location missing in `mem` raises (exn hansei
  ; memory) instead of `Not_found`, unless the default thunk is given, which pmap-find calls in
  ; tail position, outside any handler, so that it may choose (plan §3.4).
  (define (probcc-mem-ref loc mem #!optional default)
    (pmap-find loc mem (or default
                           (τ (probcc-signal 'memory 'probcc-mem-ref "unbound location" loc)))))

  ; cf. P:55-56 `mset`: the memory `mem` with `v` at `loc`, a new map.
  (define (probcc-mem-set loc v mem) (pmap-add loc v mem))

  ; cf. P:66-75 `mem_prompt`: the memory as it was before `thunk`, whatever it did.
  (define (probcc-mem-prompt thunk)
    (let* ((mem probcc-thread-local)
           (v (thunk)))
      (set! probcc-thread-local mem)
      v))

  ; cf. P:123-127 and P:143-147: resume k with v in the memory `curr` of the choice point, then
  ; restore the memory of the caller.
  (define (probcc-mem-resume curr k v)
    (let1 (mem probcc-thread-local)
      (set! probcc-thread-local curr)
      (let1 (r (k v))
        (set! probcc-thread-local mem)
        r)))

  ;; ---------------------------------------------------------------------------------------
  ;; Reification and reflection (plan §5.1)
  ;; ---------------------------------------------------------------------------------------

  ; cf. P:97-108 and DSL p.15 l.637 `reify0`: the lazy search tree of `model`, a thunk; it runs
  ; in a fresh, empty memory (P:106), and the memory of the caller is restored when the first
  ; segment returns (P:104, 107).  Deviation 2 (P:24, 97-108): the resetcc's body answers the
  ; tree already interpreted, `Done` being (probcc-unit v), and every shift answers a list of
  ; branches (plan §5.1).  Deviation 3 (P:26): the nearest resetcc delimits, so a user's
  ; resetcc inside a model captures HANSEI's choices (D17).
  (define (probcc-reify/0 model)
    (let* ((mem probcc-thread-local)
           (tree (resetcc (set! probcc-thread-local probcc-mem-empty)
                          (probcc-unit (model)))))
      (set! probcc-thread-local mem)
      tree))

  ; cf. P:118-128 `dist` and DSL p.14 l.627-628: choose v of `pairs`, a list of (v p),
  ; deviation 1: the probability second.  The memory is read before the shift, and every branch
  ; resumes in it (P:119, 123-127).  Deviation 4 (P:120): `shift`, not `shift0`, which coincide
  ; since no shift body here shifts again (plan §5.1).  `map` is safe: it only builds thunks.
  (define (probcc-distribution pairs)
    (let1 (curr probcc-thread-local)
      (letcc/shift k
        (map (λ1-match/first ((,v ,p) (probcc-τ p (probcc-mem-resume curr k v)))) pairs))))

  ; cf. P:130 `fail` and DSL p.9: a choice among no values, the failure node '().
  (define (probcc-impossible) (probcc-distribution '()))

  ; cf. P:136-148 `reflect` and DSL p.16 l.694-700: the inverse of probcc-reify/0, a choice
  ; that follows the tree `choices`; a leaf resumes in the memory read before the shift, as
  ; probcc-distribution, a branch not yet explored stays lazy.  Deviation 4 (P:142) as
  ; probcc-distribution's.  `map` is safe: it only builds thunks.
  (define (probcc-reflect choices)
    (let1 (curr probcc-thread-local)
      (letcc/shift k
        (letrec ((make-choices (λ (pv) (map F pv)))
                 (F (λ1-match/first
                      (((V ,v) ,p) (probcc-τ p (probcc-mem-resume curr k v)))
                      (((C ,t) ,p) (probcc-τ p (make-choices (t)))))))
          (make-choices choices)))))

  ;; ---------------------------------------------------------------------------------------
  ;; Distributions (P:153-185)
  ;; ---------------------------------------------------------------------------------------

  ; (new) a choice of t with probability p and of f with 1 - p; cf. P:153 `flip` for its order.
  (define (probcc-bernoulli t f p) (probcc-distribution `((,t ,p) (,f ,((op/subtract) 1 p)))))

  ; cf. P:153 `flip`, DSL p.4 l.161.
  (define (probcc-coin p) (probcc-bernoulli #t #f p))

  ; cf. P:156-165 `uniform`: a choice of 0 .. n - 1, the value 0 getting 1 - (n - 1) p as
  ; upstream (P:161).  Deviation 7: masses are the exact 1/n; n = 1 makes a choice node of one
  ; branch (the user's, commit 0af8975), where upstream answers 0 directly; n <= 0 fails,
  ; where upstream raises; a count that is not an integer, such as 5/2 or 2.5, raises (exn
  ; hansei) of kind malformed, as probcc-lazy-permutation/ins (deviation 33), where OCaml's
  ; type rules it out (the loop below would step past zero and never stop); an integral
  ; flonum such as 2. is a count.
  (define (probcc-uniform n)
    (cond
      ((not (integer? n)) (probcc-signal 'malformed 'probcc-uniform "not an integer count" n))
      ((> n 0) (letrec ((p (/ 1 n))
                        (plus (op/plus))
                        (subtract (op/subtract))
                        (loop (λ (pacc acc i)
                                (if (zero? i)
                                    (probcc-distribution (cons `(,i ,(subtract 1 pacc)) acc))
                                    (loop (plus pacc p) (cons `(,i ,p) acc) (sub1 i))))))
                 (loop 0 '() (sub1 n))))
      (else (probcc-impossible))))

  ; cf. P:173-174 `uniform_range`; deviation 8: half open, low .. high - 1.  Raw `+` and `-`:
  ; it adds integers, not weights.
  (define (probcc-uniform/range low high) (+ low (probcc-uniform (- high low))))

  ; cf. P:168-170 `uniformly` and LP:24 `uniformly_list`: a uniform choice of an element of
  ; `lst`, a list (C15); deviation 7: the empty list fails.
  (define (probcc-uniform/either lst) (list-ref lst (probcc-uniform (length lst))))

  ; cf. P:181-185 `geometric`; deviation 9: the trace (s f ... f) of the trials, newest first,
  ; instead of their count, with the same tree.
  (define (probcc-geometric p s f)
    (letrec ((subtract (op/subtract))
             (loop (λ (n)
                     (list (probcc-τ p (probcc-unit (cons s n)))
                           (probcc-τ (subtract 1 p) (loop (cons f n)))))))
      (probcc-reflect (loop '()))))

  ; (new) the syntax of an evidence: the body when `test` holds, a failure otherwise (cf.
  ; P:213-214 `observe` and the `if … then … else fail ()` of the examples).
  (define-syntax-rule (probcc-when test body ...) (cond (test body ...) (else (probcc-impossible))))

  ; cf. P:213-214 `observe`: the value of `thunk` when it passes `test`, a failure otherwise.
  (define (probcc-observe test thunk)
    (let1 (v (thunk))
      (if (test v) v (probcc-impossible))))

  ; cf. P:181-185 `geometric`, upstream's: the number of failed trials before the first success,
  ; 0 with probability p, 1 with (1 - p) p, and so on; deviation 26 (P:182): the leaf has the
  ; exact mass 1.
  (define (probcc-geometric/count p)
    (letrec ((subtract (op/subtract))
             (loop (λ (n)
                     (list (probcc-τ p (probcc-unit n))
                           (probcc-τ (subtract 1 p) (loop (add1 n)))))))
      (probcc-reflect (loop 0))))

  ; cf. P:187-206 `geometric_bounded`: the geometric distribution of probcc-geometric/count
  ; restricted to 0 .. n and normalized by 1 - (1 - p)^(n + 1).  The operations are upstream's,
  ; in its order, so that flonum masses are bit for bit those of X:72-77: `expi` is the
  ; right-nested product x (x (... x)), and the mass of i + 1 is (1 - p) times that of i.
  ; Deviation 26 (P:198): `expi x 0` is the exact 1; deviation 19 (P:205): `p /. norm` is
  ; probcc-ratio, so exact masses stay exact.
  (define (probcc-geometric/bounded n p)
    (letrec ((times (op/times))
             (subtract (op/subtract))
             (expi (λ (x n)
                     (cond
                       ((= n 0) 1)
                       ((= n 1) x)
                       (else (times x (expi x (sub1 n)))))))
             (loop (λ (pp i)
                     (if (> i n)
                         '()
                         (cons (list i pp) (loop (times (subtract 1 p) pp) (add1 i)))))))
      (let1 (norm (subtract 1 (expi (subtract 1 p) (add1 n))))
        (probcc-distribution (loop (probcc-ratio p norm) 0)))))

  ;; ---------------------------------------------------------------------------------------
  ;; Laziness, memoization and world-local references (plan §5.3; P:270-370, 449-479)
  ;; ---------------------------------------------------------------------------------------

  ; cf. P:321-330 `letlazy`: a thunk whose body `e` is evaluated at most once per world, when
  ; the thunk is first called, its value kept at a fresh location of the world-local memory
  ; ("lazy let insertion at the point of reification", P:292-298).  It is not nesting-safe, as
  ; upstream: an inner reification starts with an empty memory (P:106), so a variable of the
  ; outer world is evaluated again, inside (N:65-81).  The thunk `e` is called in tail position
  ; of the miss, outside any handler, so it may choose (plan §3.4).
  (define (probcc-letlazy e)
    (let1 (loc (probcc-mem-new-loc))
      (τ (probcc-mem-ref loc probcc-thread-local
                         (τ (let1 (v (e))
                              (set! probcc-thread-local (probcc-mem-set loc v probcc-thread-local))
                              v))))))

  ; cf. P:342-370 `letlazy_nesting`, the nesting-safe probcc-letlazy: the unevaluated `(MT e)`
  ; is stored in the memory of its creation (P:342 `memo_val`, here the lists (MV v) and
  ; (MT e)), and a world that does not find the location, an inner reification, asks the
  ; parent: it suspends with a branch of mass 1 that the inner explorer forces in the extent of
  ; the outer model, where `deref` runs in the outer world's memory (and may choose there),
  ; then resumes in its own memory, noting the value there too (P:358-369; ML p.1 l.82-91).
  ; Deviation 13 (P:347-363): no debugging output; deviation 4 (P:361): `shift`, not `shift0`,
  ; as probcc-distribution's; deviation 26 (P:361): the exact mass 1.
  (define (probcc-letlazy/nesting e)
    (let1 (loc (probcc-mem-new-loc))
      (set! probcc-thread-local (probcc-mem-set loc `(MT ,e) probcc-thread-local))
      (letrec ((deref
                 (τ (match/first (probcc-mem-ref loc probcc-thread-local (τ #f))
                      ((MV ,v) v)
                      ((MT ,e) (let1 (v (e))
                                 (set! probcc-thread-local
                                   (probcc-mem-set loc `(MV ,v) probcc-thread-local))
                                 v))
                      (#f (let1 (mem probcc-thread-local)
                            (letcc/shift k
                              (list (probcc-τ 1 (let1 (v (deref))
                                                  (probcc-mem-prompt
                                                    (τ (set! probcc-thread-local
                                                         (probcc-mem-set loc `(MV ,v) mem))
                                                       (k v)))))))))))))
        deref)))

  ; cf. P:449-479 `memo`: the stochastic function f made a proper function in every world, its
  ; table of results kept in the world-local memory (P:466-469), so (deref x) gives the same
  ; value whenever it is applied to x in the same world; unary as upstream (pass a list for
  ; several arguments).  The table is read again after (f x), which may change the memory
  ; (P:474-476).  Deviation 16 (P:471): a memo created outside the reification it is used in
  ; raises (exn hansei memory) instead of `Not_found`; the table is ordered by pmap-compare.
  (define (probcc-memo f)
    (let1 (loc (probcc-mem-new-loc))
      (set! probcc-thread-local (probcc-mem-set loc pmap-empty probcc-thread-local))
      (letrec ((table (τ (probcc-mem-ref loc probcc-thread-local
                                         (τ (probcc-signal 'memory 'probcc-memo
                                                           "memo table not in this world's memory"
                                                           loc))))))
        (λ (x)
          (pmap-find x (table)
                     (τ (let* ((v (f x))
                               (table* (pmap-add x v (table))))
                          (set! probcc-thread-local (probcc-mem-set loc table* probcc-thread-local))
                          v)))))))

  ; cf. TI:121-126 `WorldLocalMem.wref_new`: a world-local reference holding v, a fresh location
  ; of the current memory; deviation 17: the module is not shipped upstream, reconstructed.
  (define (probcc-wref-new v)
    (let1 (loc (probcc-mem-new-loc))
      (set! probcc-thread-local (probcc-mem-set loc v probcc-thread-local))
      loc))

  ; cf. TI:124 `wref_get`: the value of the reference r in this world; deviation 17 as
  ; probcc-wref-new, and (exn hansei memory) when r is not in this world's memory.
  (define (probcc-wref-get r) (probcc-mem-ref r probcc-thread-local))

  ; cf. TI:125 `wref_set`: r holds v from now on, in this world only; deviation 17: the
  ; reference first, where upstream's `wref_set v r` takes the value first.
  (define (probcc-wref-set! r v)
    (set! probcc-thread-local (probcc-mem-set r v probcc-thread-local)))

  ;; ---------------------------------------------------------------------------------------
  ;; Exact inference (I:16-28, 296-301; P:247-248, 267-268; DSL p.9, 15-18)
  ;; ---------------------------------------------------------------------------------------

  ; (new) cf. I:28 `PMap.foldi (fun v p a -> (p,V v)::a) ans susp`: the leaves of the values
  ; collated in the map `ans`, each mass mapped by `mass` (the identity, or the division by the
  ; number of samples of I:132, 187), in descending pmap-compare order, before `tail`.
  (define (probcc-pmap->pv ans mass tail)
    (pmap-foldi (λ (v p a) (cons (probcc-value (mass p) v) a)) ans tail))

  ; (new) deviation 5 (I:28): the entries of `pv` by descending mass under op/greater, with a
  ; stable sort, so that tied entries keep upstream's order (D5).
  (define (probcc-sort/mass pv)
    (let1 (greater (op/greater))
      (sort pv (λ (a b) (greater (cadr a) (cadr b))))))

  ; (new) cf. I:43, 158; P:230, 239: `List.fold_left (fun pa (p,_) -> pa +. p) 0.0 ch`, the total
  ; mass of the entries of `ch`, a left fold from the exact 0 (deviation 26).
  (define (probcc-total plus ch) (fold (λ (e pa) (plus pa (cadr e))) 0 ch))

  ; (new) cf. I:47, 161; P:240: `List.map (fun (p,x) -> (p /. ptotal,x)) ch`, the entries of `ch`
  ; with their masses divided by ptotal; deviation 19: probcc-ratio, so exact masses stay exact
  ; and a zero total gives NaN masses as upstream.
  (define (probcc-scale ch ptotal)
    (probcc-map (λ1-match/first ((,x ,p) (list x (probcc-ratio p ptotal)))) ch))

  ; cf. I:16-28 `explore` and DSL p.9 l.393-408: the tree `choices` explored depth first to
  ; `maxdepth`, a number or #f for no bound (`None`, C6): the values found, collated with their
  ; masses, before the branches left unexplored.  The answer and the suspended branches are
  ; threaded through the loop in a persistent map and a list, as upstream (C1): a thunk forced
  ; here may choose in the enclosing model, whose every world then resumes this loop.  A
  ; suspended branch keeps its thunk (C4, I:26).  Deviation 5 (I:28): sorted by descending
  ; mass, with a stable sort of upstream's order, values in descending pmap-compare order then
  ; the branches in reverse discovery order (D5); deviation 6 (PM:101): pmap-compare stands for
  ; OCaml's `compare`.
  (define (probcc-explore maxdepth choices)
    (letrec ((times (op/times))
             (plus (op/plus))
             (loop (λ (p depth down choices ans susp)                 ; answers (ans . susp)
                     (match/first choices
                       (() (cons ans susp))
                       ((((V ,v) ,pt) . ,rest)
                        (loop p depth down rest (pmap-insert-with plus v (times pt p) ans) susp))
                       ((((C ,t) ,pt) . ,rest)
                        (cond
                          (down (let1 (down* (or (not maxdepth) (< depth maxdepth)))
                                  (match/first (loop (times pt p) (add1 depth) down* (t) ans susp)
                                    ((,ans* . ,susp*) (loop p depth down rest ans* susp*)))))
                          (else (let1 (susp* (cons `((C ,t) ,(times pt p)) susp))
                                  (loop p depth down rest ans susp*)))))))))
      (match/first (loop 1 0 #t choices pmap-empty '())
        ((,ans . ,susp) (probcc-sort/mass (probcc-pmap->pv ans identity susp))))))

  ; cf. I:296-301 `normalize`: (list total normalized), where `total` is the left fold of
  ; `p +. acc` from 0 (I:300) and every mass is divided by it with op/divide.  Deviation 10:
  ; exact weights become flonums with one rounding; a zero total gives NaN masses as upstream.
  ; Deviation 26 (I:300): the fold starts from the exact 0, so the total of '() is the exact
  ; 0, not OCaml's 0.0.
  (define (probcc-normalize/total choices)
    (let* ((plus (op/plus))
           (divide (op/divide))
           (total (fold (λ (e acc) (plus (cadr e) acc)) 0 choices)))
      (list total (probcc-map (λ1-match/first ((,slot ,p) (list slot (divide p total)))) choices))))

  ; cf. PE:22-24 `normalize`, the list only, the same fold and division as I's.
  (define (probcc-normalize choices) (cadr (probcc-normalize/total choices)))

  ; cf. DSL p.15 l.656-658 `reify`: the model explored to `depth` (a number or #f).
  (define ((probcc-reify depth) model) (probcc-explore depth (probcc-reify/0 model)))

  ; cf. P:247-248 `exact_reify`: the model explored with no bound (+inf.0 as `None`).
  (define probcc-reify/exact/a (probcc-reify +inf.0))

  ; (new) the syntax of probcc-reify/exact/a over a body.
  (define-syntax-rule (probcc-reify/exact body ...) (probcc-reify/exact/a (τ body ...)))

  ; cf. P:267-268 `variable_elim` and DSL p.17 l.761-771: f with its distribution inferred
  ; exactly at every call; deviation 12: variadic.
  (define (probcc-variable-elimination f)
    (λ args (probcc-reflect (probcc-reify/exact (apply f args)))))

  ; cf. DSL p.18 l.776-783, NIPS l.69-75 and EPP p.4 l.331-336, bucket elimination: the
  ; variable elimination of f memoized on its arguments; deviation 11: λ-memo's table is global,
  ; shared by every inference as EPP asks, so it is wrong when f reads world-local state.
  (define-syntax-rule (λ-probcc-bucket args body ...)
    (letrec ((F (λ args body ...))
             (B (λ-memo bargs (probcc-reify/exact (apply F bargs)))))
      (o probcc-reflect B)))

  ;; ---------------------------------------------------------------------------------------
  ;; Tables (N:292-304 = PE:49-61; BL:456-460)
  ;; ---------------------------------------------------------------------------------------

  ; (new) does x hold a NaN where pmap-compare looks, in a number (either part of a complex),
  ; a pair, a vector or the slots of a record?
  (define (probcc-nan-inside? x)
    (cond
      ((number? x) (or (nan? (real-part x)) (nan? (imag-part x))))
      ((pair? x) (or (probcc-nan-inside? (car x)) (probcc-nan-inside? (cdr x))))
      ((vector? x) (let loop ((i 0))
                     (and (fx< i (vector-length x))
                          (or (probcc-nan-inside? (vector-ref x i)) (loop (fx+ i 1))))))
      ((record-instance? x) (probcc-nan-inside? (record->vector x)))
      (else #f)))

  ; (new) OCaml's structural `=`: equal under pmap-compare, so -0. and 0. are equal, except
  ; that a NaN is never equal, not even to itself (`=` has no physical-equality shortcut), and
  ; values pmap-compare cannot order raise (exn pmap compare), as `=` raises on functional
  ; values; but an exact number and an equal inexact one differ, which OCaml cannot express.
  (define (probcc-structural=? a b)
    (and (fx= 0 (pmap-compare a b)) (not (probcc-nan-inside? a))))

  ; cf. N:292-293 = PE:49-50 `prob_of`: the mass of the first leaf of `pv` whose value is
  ; `=` to v (probcc-structural=?), else 0.  Deviation 23: a branch not yet explored is
  ; skipped, where upstream's pattern `(p, V v)` fails to match; deviation 26: the exact 0,
  ; not 0.0.
  (define (probcc-prob-of v pv)
    (let loop ((pv* pv))
      (match/first pv*
        (() 0)
        ((((V ,w) ,p) . ,rest) (if (probcc-structural=? w v) p (loop rest)))
        ((,_ . ,rest) (loop rest)))))

  ; cf. N:304 = PE:61 `at_least`: does `pv` give v a mass of at least prob?
  (define (probcc-at-least prob v pv) (>= (probcc-prob-of v pv) prob))

  ; cf. BL:456-460 `map_dist`: the flat table pv with f applied to its values, equal results
  ; collated by summing their masses (`p +. old`, PM:124), in upstream's order, descending
  ; pmap-compare order of the results (PM:187-199 `foldi` consing), not sorted by mass.
  ; Deviation 29: promoted from example code; a branch not yet explored raises (exn hansei
  ; malformed), where upstream's pattern fails to match.  srfi-1's fold may be re-entered.
  (define (probcc-map-dist f pv)
    (let* ((plus (op/plus))
           (pm (fold (λ (e pm)
                       (match/first e
                         (((V ,v) ,p) (pmap-insert-with plus (f v) p pm))
                         (else (probcc-signal 'malformed 'probcc-map-dist
                                              "a branch not yet explored" e))))
                     pmap-empty
                     pv)))
      (pmap-foldi (λ (v p a) (cons (probcc-value p v) a)) pm '())))

  ;; ---------------------------------------------------------------------------------------
  ;; Selectors (plan §5.4; P:218-241; T:20)
  ;; ---------------------------------------------------------------------------------------

  ; (new) the rng-source over the global state of (aux hansei random), as OCaml's `Random`
  ; functions draw from the global `Random` state.
  (define probcc-random-global-source (make-ocaml-random-source ocaml-random-global))

  ; cf. P:221-233 `random_selector`: a selector, which chooses an entry of `choices`, a list of
  ; (x p), at random with the probability of p over the total mass `ptotal`, and answers
  ; (x ptotal): ptotal, not p (P:227).  The seed re-initialises the global state when the
  ; selector is made (`Random.init`, P:222), so a selector made inside a model re-seeds the
  ; stream the outer selectors draw from, as upstream (N:30-31 replays only so, plan D6).  One
  ; draw per call, `Random.float ptotal` (P:231), even for '(), which then raises (P:224); as
  ; upstream, it also raises "can't happen" when rounding puts r past the last cumulative mass
  ; (P:224-232), kept as upstream (plan §9).  Deviation 18 (P:221-233; T:20): the answer is a
  ; list, thing first; an rng-source of (aux hakaru10 random) may replace the seed, and the
  ; selector then draws from it.
  (define (probcc-random-selector seed-or-source)
    (let1 (src (cond
                 ((rng-source? seed-or-source) seed-or-source)
                 (else (ocaml-random-init! ocaml-random-global seed-or-source)       ; P:222
                       probcc-random-global-source)))
      (λ (choices)
        (let* ((plus (op/plus))
               (ptotal (probcc-total plus choices))
               (r (rng-double! src 0. ptotal)))                                    ; 0 <= r < ptotal
          (let loop ((pcum 0)
                     (rest choices))
            (match/first rest
              (() (probcc-signal 'selector 'probcc-random-selector "Choice selection: can't happen"
                                 choices))
              (((,x ,p) . ,rest*) (let1 (pcum* (plus pcum p))
                                    (if (< r pcum*) (list x ptotal) (loop pcum* rest*))))))))))

  ; cf. P:238-241 `dist_selector`: a selector that chooses through the non-determinism of the
  ; enclosing reification, a probcc-distribution over the entries normalized by their total,
  ; for nested inference (N:145-178; PE:78-89).  Deviation 18 as probcc-random-selector's;
  ; deviation 19 (P:240): `p /. ptotal` is probcc-ratio, so exact masses stay exact.
  (define (probcc-dist-selector choices)
    (let* ((ptotal (probcc-total (op/plus) choices))
           (x (probcc-distribution (probcc-scale choices ptotal))))
      (list x ptotal)))

  ;; ---------------------------------------------------------------------------------------
  ;; Approximate inference (plan §6.2-6.5; I:30-198; P:253-262)
  ;; ---------------------------------------------------------------------------------------

  ; cf. I:30 `nearly_one`, for a robust comparison with 1.
  (define probcc-nearly-one (- 1. 1e-7))

  ; cf. I:35-53 `shallow_explore`: the tree `choices` explored to `maxdepth`, a number, without
  ; flattening it: the values found are collated, failures are dropped, and every branch at
  ; `maxdepth` is kept, a branch above it becoming a thunk that answers its explored subtree,
  ; normalized when its mass is below 1 (its mass then times the total).  Not sorted: the
  ; values in descending pmap-compare order, then the branches in reverse order, as upstream,
  ; since the selectors scan the entries in this order (plan §6.2).  Deviation 19 (I:47):
  ; `p /. ptotal` is probcc-ratio.
  (define (probcc-explore/shallow maxdepth choices)
    (letrec ((times (op/times))
             (plus (op/plus))
             (loop (λ (pc depth ans acc lst)                            ; answers (ans . acc)
                     (match/first lst
                       (() (cons ans acc))
                       ((((V ,v) ,p) . ,rest)
                        (loop pc depth (pmap-insert-with plus v (times p pc) ans) acc rest))
                       (((,c . ,rest) ⊣ (>= depth maxdepth)) (loop pc depth ans (cons c acc) rest))
                       ((((C ,t) ,p) . ,rest)
                        (match/first (loop (times pc p) (add1 depth) ans '() (t))
                          ((,ans* . ,ch)
                           (let* ((ptotal (probcc-total plus ch))
                                  (acc* (cond
                                          ((zero? ptotal) acc)
                                          ((< ptotal probcc-nearly-one)
                                           (let1 (ch* (probcc-scale ch ptotal))
                                             (cons `((C ,(τ ch*)) ,(times p ptotal)) acc)))
                                          (else (cons `((C ,(τ ch)) ,p) acc)))))
                             (loop pc depth ans* acc* rest)))))))))
      (match/first (loop 1 0 pmap-empty '() choices)
        ((,ans . ,acc) (probcc-pmap->pv ans identity acc)))))

  ; cf. I:55-64 `first_success`: the tree `choices` explored breadth first, as upstream's
  ; `rest @ …`, until its first entry is a leaf: that tree, or '() when it has no leaf.
  (define (probcc-first-success choices)
    (let1 (times (op/times))
      (let loop ((choices* choices))
        (match/first choices*
          (() '())
          ((((V ,_) ,_) . ,_) choices*)
          ((((C ,t) ,pt) . ,rest)
           (let1 (ch (probcc-map (λ1-match/first ((,slot ,p) (list slot (times pt p)))) (t)))
             (loop (append rest ch))))))))

  ; cf. I:71-109 `bounded_explore`: (list low high), bounds of the probability of evidence of
  ; `choices`, the tree of a model that answers (void) or fails (every leaf counts, whatever
  ; its value, I:73-75).  Breadth first, by mass: the branches wait in a queue, a persistent
  ; map from masses (number-compare, upstream's float compare) to lists of thunks, the last
  ; queued first (`(@)`, I:95); while it holds fewer than `maxsize` the heaviest is explored,
  ; otherwise the lightest is forced but its branches are not queued, their mass counting for
  ; `high` only.  Deviation 26 (I:108): low, high and the first mass are the exact 0, 0 and 1,
  ; so `low` is 0, not 0., when no leaf is reached (plan §6.3).
  (define (probcc-explore/bounded maxsize choices)
    (letrec ((times (op/times))
             (plus (op/plus))
             (loop (λ (explore? pc low high q qsize lst)
                     (match/first lst
                       (() (next low high q qsize))
                       ((((V ,_) ,p) . ,rest)
                        (let1 (pe (times pc p))
                          (loop explore? pc (plus low pe) (plus high pe) q qsize rest)))
                       ((((C ,t) ,p) . ,rest)
                        (if explore?
                            (let1 (q* (pmap-insert-with append (times pc p) (list t) q))
                              (loop explore? pc low high q* (add1 qsize) rest))
                            (loop explore? pc low (plus high (times pc p)) q qsize rest))))))
             (next (λ (low high q qsize)
                     (if (zero? qsize)
                         (list low high)
                         (let1 (explore? (< qsize maxsize))
                           (match/first ((if explore? pmap-delete-find-max pmap-delete-find-min) q)
                             (((,p ,t . ,ts) ,q*)
                              (loop explore? p low high (if (null? ts) q* (pmap-add p ts q*))
                                    (sub1 qsize) (t)))))))))
      (loop #t 1 0 0 (pmap-create number-compare) 0 choices)))

  ; cf. I:121-135 `rejection_sample_dist`: `n` samples of the tree `choices`, each a path from
  ; the root to a leaf chosen with `selector`, which is called only where there are two
  ; entries or more; the masses of the values reached are collated and divided by n.
  ; Deviation 14 (I:131): the `done` line is printed only under probcc-verbose; deviation 19
  ; (I:132): `p /. ns` is probcc-ratio; deviation 26 (I:128): the chosen entry has the exact
  ; mass 1; deviation 5 (I:132): sorted as probcc-explore.
  (define (probcc-sample-dist/rejection selector n choices)
    (letrec ((times (op/times))
             (plus (op/plus))
             (loop (λ (pcontrib ans ch)
                     (match/first ch
                       ((((V ,v) ,p)) (pmap-insert-with plus v (times p pcontrib) ans))
                       (() ans)
                       ((((C ,t) ,p)) (loop (times p pcontrib) ans (t)))
                       (else (match/first (selector ch)               ; one thread, at random
                               ((,th ,ptotal)
                                (loop (times pcontrib ptotal) ans (list (list th 1)))))))))
             (driver (λ (ans i) (if (zero? i) ans (driver (loop 1 ans choices) (sub1 i))))))
      (let1 (ans (driver pmap-empty n))
        (when (probcc-verbose) (printf "rejection_sample: done ~a worlds\n" n))
        (probcc-sort/mass (probcc-pmap->pv ans (λ (p) (probcc-ratio p n)) '())))))

  ; cf. I:142-146 `sample_runner` and P:259-261: the runner of `sample_dist` that applies `step`
  ; n times to z, answering (list z* n); curried, as the record of I:145-146.
  (define ((probcc-sample-runner/times n) z step)
    (let loop ((z* z)
               (i n))
      (if (zero? i) (list z* n) (loop (step z*) (sub1 i)))))

  ; cf. I:137-198 `sample_dist`, importance sampling with a one-step look-ahead (DSL p.18-19
  ; l.785-856; EPP p.6 l.547): every branch is forced once before choosing, so a leaf found
  ; one step ahead is collated at once with its exact mass, a failure is dropped, and the
  ; choice is among the open threads only, renormalized; the root is pre-explored until it
  ; splits into several threads, which `runner` samples, and a tree solved by the
  ; pre-exploration is answered exactly, without a draw.  The draws, the reversals and the
  ; operations are upstream's, in its order, so that seeded runs replay HANSEI's records bit
  ; for bit (plan §5.4, §6.5).  Deviations 14, 19 and 5 as probcc-sample-dist/rejection's
  ; (I:186, 160, 187).  srfi-1's fold may be re-entered, where a forced thunk chooses in an
  ; enclosing model (dist_selector, plan §5.7).
  (define (probcc-sample-dist selector runner choices)
    (letrec ((times (op/times))
             (plus (op/plus))
             (add (λ (v p ans) (pmap-insert-with plus v p ans)))
             (look-ahead (λ (pcontrib)                    ; I:150-163, a step of fold_left
                           (λ (e answers)
                             (let ((ans (car answers))
                                   (acc (cdr answers)))
                               (match/first e
                                 (((V ,v) ,p) (cons (add v (times p pcontrib) ans) acc))
                                 (((C ,t) ,p)
                                  (match/first (t)
                                    (() answers)
                                    ((((V ,v) ,p1))
                                     (cons (add v (times (times p p1) pcontrib) ans) acc))
                                    (,ch (let1 (ptotal (probcc-total plus ch))
                                           (cons ans (cons (if (< ptotal probcc-nearly-one)
                                                               (list (probcc-scale ch ptotal)
                                                                     (times p ptotal))
                                                               (list ch p))
                                                           acc)))))))))))
             (loop (λ (pcontrib ans ch)                   ; I:164-174
                     (match/first ch
                       ((((V ,v) ,p)) (add v (times p pcontrib) ans))
                       (() ans)
                       ((((C ,t) ,p)) (loop (times p pcontrib) ans (t)))
                       (else (match/first (fold (look-ahead pcontrib) (list ans) ch)
                               ((,ans*) ans*)
                               ((,ans* . ,cch)                    ; one thread, at random
                                (match/first (selector cch)
                                  ((,th ,ptotal) (loop (times pcontrib ptotal) ans* th)))))))))
             (toploop (λ (pcontrib ans cch)               ; I:175-177, cch pre-explored
                        (match/first (selector cch)
                          ((,th ,ptotal) (loop (times pcontrib ptotal) ans th)))))
             (driver (λ (pcontrib vals cch)               ; I:178-187
                       (match/first (runner pmap-empty (λ (ans) (toploop pcontrib ans cch)))
                         ((,ans ,ns)
                          (let1 (ans* (pmap-foldi (λ (v p ans) (add v (times ns p) ans)) vals ans))
                            (when (probcc-verbose)
                              (printf "sample_importance: done ~a worlds\n" ns))
                            (probcc-sort/mass
                              (probcc-pmap->pv ans* (λ (p) (probcc-ratio p ns)) '())))))))
             (make-threads (λ (pcontrib ans ch)           ; I:188-195, pre-explore the root
                             (match/first (fold (look-ahead pcontrib) (list ans) ch)
                               ((,ans*) (probcc-sort/mass (probcc-pmap->pv ans* identity '())))
                               ((,ans* (,ch1 ,p)) (make-threads (times pcontrib p) ans* ch1))
                               ; List.rev "for literal compatibility with an earlier version"
                               ((,ans* . ,cch) (driver pcontrib ans* (reverse cch)))))))
      (make-threads 1 pmap-empty choices)))

  ; cf. P:253-254 `sample_rejection`: `n` rejection samples of the model `thunk`.
  (define (probcc-sample-rejection/a selector n thunk)
    (probcc-sample-dist/rejection selector n (probcc-reify/0 thunk)))

  ; (new) the syntax of probcc-sample-rejection/a over a body.
  (define-syntax-rule (probcc-sample-rejection selector n body ...)
    (probcc-sample-rejection/a selector n (τ body ...)))

  ; cf. P:256-262 `sample_importance`: `n` importance samples of the model `thunk`, its tree
  ; first explored to depth 3 with probcc-explore/shallow.
  (define (probcc-sample-importance/a selector n thunk)
    (probcc-sample-dist selector (probcc-sample-runner/times n)
                        (probcc-explore/shallow 3 (probcc-reify/0 thunk))))

  ; (new) the syntax of probcc-sample-importance/a over a body.
  (define-syntax-rule (probcc-sample-importance selector n body ...)
    (probcc-sample-importance/a selector n (τ body ...)))

  ;; ---------------------------------------------------------------------------------------
  ;; Utilities (I:276-308)
  ;; ---------------------------------------------------------------------------------------

  ; cf. I:279-294 `statistics`: the mean and the population standard deviation of the mass of
  ; every value over the tables `(sampler seed)`, for seed from seed1 to seed2, as a list of
  ; (v mean std), in upstream's operation order.  Deviation 27 (I:293): the std is fpsqrt of a
  ; flonum, so a variance made slightly negative by rounding gives +nan.0 as OCaml (SM:425-463),
  ; not a complex; the mean keeps exact masses exact.  Deviation 21 (I:292): sorted by
  ; descending mean, ties in descending pmap-compare order, where upstream's is the hash
  ; table's.  A branch not yet explored raises (exn hansei malformed), where upstream's
  ; pattern `(p, V v)` fails to match.
  (define (probcc-statistics seed1 seed2 sampler)
    (let* ((times (op/times))
           (plus (op/plus))
           (subtract (op/subtract))
           (add (λ (new old) (list (plus (car old) (car new)) (plus (cadr old) (cadr new)))))
           (answers (let loop ((seed seed1)
                               (answers pmap-empty))
                      (if (> seed seed2)
                          answers
                          (loop (add1 seed)
                                (fold (λ (e answers)
                                        (match/first e
                                          (((V ,v) ,p)
                                           (pmap-insert-with add v (list p (times p p)) answers))
                                          (else (probcc-signal 'malformed 'probcc-statistics
                                                               "a branch not yet explored" e))))
                                      answers
                                      (sampler seed))))))
           (n (add1 (- seed2 seed1)))
           (stats (pmap-foldi (λ (v s a)
                                (match/first s
                                  ((,p ,p2)
                                   (let* ((variance (/ (subtract p2 (/ (times p p) n)) n))
                                          (std (fpsqrt (exact->inexact variance))))
                                     (cons (list v (/ p n) std) a)))))
                              answers
                              '())))
      (sort stats (let1 (greater (op/greater))
                    (λ (a b) (greater (cadr a) (cadr b)))))))

  ; cf. I:303-308 `timeit`: the value of `thunk`, after printing the processor time it took,
  ; user and system, in seconds (`Sys.time`).  Deviation 28 (I:307): printed with ~a, not %g.
  (define (probcc-timeit thunk)
    (let* ((seconds (λ () (call-with-values cpu-time (λ (user system) (/ (+ user system) 1000.)))))
           (start (seconds))
           (r (thunk)))
      (printf "\nTime spent: ~a sec\n" (- (seconds) start))
      r))

  ;; ---------------------------------------------------------------------------------------
  ;; The search-tree monad and logic programming (DSL p.10; LV; LG)
  ;; ---------------------------------------------------------------------------------------

  ; cf. DSL p.10 l.415-419 `pv_bind`: the tree `m` with every leaf x replaced by a branch to
  ; the tree (f x), lazily; with probcc-unit, a monad up to tree equivalence: explored with no
  ; bound, (probcc-bind (probcc-unit x) f) and (f x) are equal (DSL p.10 l.420-425).
  (define (probcc-bind m f)
    (probcc-map (λ1-match/first
                  (((V ,x) ,p) (probcc-τ p (f x)))
                  (((C ,t) ,p) (probcc-τ p (probcc-bind (t) f))))
                m))

  ; cf. LV:68-70 `reify_part` (= TI:60-63 `run`; LG l.429): the values of the model explored
  ; to `bound` (a number or #f), as (v p) pairs, the unexplored branches discarded.
  ; Deviation 5: in probcc-explore's order, where upstream's fold_left reverses it.
  (define (probcc-reify/part bound model)
    (foldr (λ (e acc)
             (match/first e
               (((V ,v) ,p) (cons (list v p) acc))
               (else acc)))
           '()
           (probcc-explore bound (probcc-reify/0 model))))

  ; cf. LV:386-391 = PS:156-161 `soft_cut` (LG l.1180-1184): Prolog's `p *-> q ; r`: when the
  ; model `p` has a leaf, reached breadth first, (q x) with x the choice that follows the tree
  ; probcc-first-success found, else (r).
  (define (probcc-soft-cut p q r)
    (let1 (t (probcc-first-success (probcc-reify/0 p)))
      (if (null? t) (r) (q (probcc-reflect t)))))

  ;; ---------------------------------------------------------------------------------------
  ;; Traversals (new)
  ;; ---------------------------------------------------------------------------------------

  ; (new) the number of `V` leaves of the tree `choices`, forcing every thunk (C10): it
  ; diverges on an infinite tree, and an inner reification is invisible to it (only the
  ; reflected table shows), e.g. it counts the 1024 leaves of `flips_xor 10` (DSL p.17
  ; l.756-757) but 4 for `flips_xor' 10`.  foldr may be re-entered, unlike `map`.
  (define (probcc-leaves choices)
    (let L ((choices* choices)
            (count 0))
      (let1 (F (λ (probpair acc)
                 (match/first probpair
                   (((V ,_) ,_) (add1 acc))
                   (((C ,t) ,_) (L (t) acc)))))
        (foldr F count choices*))))

  ; (new) the leaves of the tree `choices` in depth-first order, every thunk forced and the
  ; masses multiplied along the path, without collating equal values; cf.
  ; continuations/index.html#reify-search (H l.59), whose `dfs` traverses a reified search tree.
  ; op/times is read once (C8) and the maps are probcc-map, since a forced thunk may choose
  ; in an enclosing model (C9).
  (define (probcc-dfs choices)
    (letrec ((times (op/times))
             (D (λ (choices) (apply append (probcc-map M choices))))
             (M (λ1-match/first
                  (((C ,t) ,p) (D (probcc-map (λ1-match/first ((,slot ,pi) `(,slot ,(times p pi))))
                                              (t))))
                  (,probpair (list probpair)))))
      (D choices)))

  )
