;; (aux hakaru10 exact) -- exact evaluators that check (aux hakaru10), a CHICKEN port of
;; Oleg Kiselyov's Hakaru10 (https://okmij.org/ftp/kakuritu/Hakaru10/index.html).
;; Internal: (aux hakaru10) re-exports exact-posterior, model->search-tree,
;; likelihood-weighting and weighted-samples, and its `check:` runs construction-check and
;; reference-check.
;;
;; Mirrors no upstream file: it is new.  It checks the chain built by the engine, which mirrors
;;   M  Metropolis.hs (https://okmij.org/ftp/kakuritu/Hakaru10/Metropolis.hs) and
;;   S  Syntax.hs (https://okmij.org/ftp/kakuritu/Hakaru10/Syntax.hs),
;; against oracles that do not reuse the engine (user directive of 2026-10-12): a model is a
;; direct-style Scheme program performing the operations dist (an observation being the dist of
;; a `condition`ed distribution), diracN and if_, and each oracle is a handler installed with
;; the engine's `handle-model`, under the native `reset` of (chicken continuation):
;;   - exact enumeration: the model is resumed once per point of the finite support of every
;;     random choice (multi-shot), building a lazy search tree in Hansei's ((V v) p) / ((C thunk)
;;     p) format, explored and normalized with probcc-explore and probcc-normalize of (aux hansei)
;;     (https://okmij.org/ftp/kakuritu/Hansei.html);
;;   - a forward sampler (one-shot): likelihood weighting, and the construction-order run that
;;     `check:` compares with the trace builder's initial trace;
;; plus the reference evaluator (recompute every node from scratch and compare with the
;; incremental state) and the kernel residual of the 'extended semantics (plan §6.9-§6.11).
;; Only aux.hansei's data-level tools are used, which work on lists and thunks: native
;; shift/reset and aux's resetcc are never mixed within one control context.
;;
;; Hakaru10 code version: November 2016 (W:52-53); web page last updated July 14, 2018 (W:179).
;; Paper: O. Kiselyov, "Probabilistic Programming Language and its Incremental Evaluation",
;; APLAS 2016, LNCS 10017, DOI 10.1007/978-3-319-47958-3_19 (P = design.txt / design.pdf).
;;
;; Contents:
;;   - exact enumeration (PLAN-IDIOM §2.3): exact-handler, model->search-tree, exact-posterior;
;;   - forward sampling (PLAN-IDIOM §2.4): forward-run, weighted-samples, likelihood-weighting,
;;     construction-check;
;;   - the reference evaluator (plan §6.9): node-distk, recomputed-state, scope-inactivity,
;;     reference-check;
;;   - the kernel residual (plan §6.11): load-state!, ext-states, forced-step-log-alpha,
;;     kernel-residual.
;;
;; Runtime: exact enumeration is the one multi-shot interpretation, so it is the one that meets
;; the caveat of CHICKEN main (rev 55615603): a call/cc escape inside a resumed segment of the
;; native shift/reset returns to the `reset` the capture was made under, not to the resumption.
;; A model that sets up `guard`, `handle-exceptions` or `condition-case` around a `dist` then
;; makes exact-posterior, and the exploration of model->search-tree's tree, raise a
;; `construction` error (handle-model's check of the misrouted escape, see
;; native-multishot-escapes? in the engine, raised in the call that resumed the model) rather
;; than answer a wrong posterior.  The chicken-core branch delimcc-multishot fixes it (f11f304b,
;; b29737e8, 0d398032) and the same models enumerate there; this module runs on both.  The
;; forward sampler is one-shot and unaffected.
;; Models run here many times (exact enumeration resumes them once per support point), so they
;; must not mutate, across an operation, state that outlives it (PLAN-IDIOM C28); and since
;; SExps are plain values under these handlers, the static discipline is checked by building the
;; model with the trace builder (C29, validation-build).

(module (aux hakaru10 exact) *

  (import scheme
          (chicken base)
          (only srfi-1 filter-map count fold iota)
          srfi-69
          (aux base)
          (only (aux hansei) probcc-τ probcc-unit probcc-explore probcc-normalize)
          (only (aux hakaru10 random) make-stdgen-source rng-source-copy)
          (aux hakaru10 distribution)
          (only (aux hakaru10 util) histogram-sort)
          (aux hakaru10 engine))

  ;; ---------------------------------------------------------------------------------------
  ;; Helpers
  ;; ---------------------------------------------------------------------------------------

  ; (new, plan §6.9) whether two LLs agree within 1e-9, relative to the larger of 1 and |a|:
  ; equal infinities and two NaNs agree too.
  (define (ll-close? a b)
    (or (= a b)
        (and (nan? a) (nan? b))
        (<= (abs (- a b)) (* 1e-9 (max 1. (abs a))))))

  ; (new, plan §6.9) whether the value a check recomputed matches the incremental one: equal?,
  ; except that two procedures match and that a value too large to walk matches.  A lifted
  ; function or a diracN may answer a closure built at run time (check-lifted-value rejects
  ; only SExps), and recomputing it builds a fresh one, which equal? (eq? on procedures) would
  ; report as a difference: its consumers, which apply it, are checked themselves.  The walk
  ; stops, matching, after 100000 pairs and vector slots, so that a circular value (which
  ; equal? would walk forever, and which the engine accepts, check-no-sexp) ends the check.
  (define (same-value? a b)
    (let1 (budget 100000)
      (define (spent?) (set! budget (sub1 budget)) (negative? budget))
      (let walk ((a a) (b b))
        (cond
          ((eq? a b) #t)
          ((and (procedure? a) (procedure? b)) #t)
          ((and (pair? a) (pair? b))
           (or (spent?) (and (walk (car a) (car b)) (walk (cdr a) (cdr b)))))
          ((and (vector? a) (vector? b))
           (and (= (vector-length a) (vector-length b))
                (let loop ((i 0))
                  (or (= i (vector-length a))
                      (spent?)
                      (and (walk (vector-ref a i) (vector-ref b i)) (loop (add1 i)))))))
          (else (equal? a b))))))

  ; (new, plan §6.10) the finite support ((x . p) ...) of the distk of a random choice, in its
  ; sampler's order (distribution-support); a choice without one is a `support` error.
  (define (choice-support dk who)
    (or (distribution-support dk)
        (hakaru10-error 'support who "a random choice without a finite support" dk)))

  ;; ---------------------------------------------------------------------------------------
  ;; Exact enumeration (PLAN-IDIOM §2.3, replacing plan §6.10): a multi-shot handler
  ;; ---------------------------------------------------------------------------------------

  ; cf. Hansei's probcc-distribution (aux.hansei.scm; Kiselyov and Shan, "Embedded
  ; probabilistic programming", DSL 2009) and plan §6.10 step 2: the handler of exact
  ; enumeration, under which SExps are plain values (value-handler: diracN answers the value,
  ; if_ the branch its test selects, so a branch never taken is never run).  A random choice
  ; becomes one search-tree node ((C thunk) p) per support point x of probability p > 0, the
  ; thunk resuming the model with x: multi-shot, and lazy, since probcc-explore forces the
  ; thunks.  An observation becomes one node weighted by its likelihood exp(log-density), a
  ; density for a continuous distribution (right for posteriors over discrete choices), or no
  ; node at all when that is 0, pruning the path.
  (define exact-handler
    (value-handler
      (λ (dk resume)
        (if (eq? (distk-kind dk) 'observed)
            (let* ((obs (distk-value dk))
                   (w (exp (distribution-log-density dk obs))))
              (if (zero? w) '() (list (probcc-τ w (resume obs)))))
            (filter-map (Λ ((,x . ,p)) (and (positive? p) (probcc-τ p (resume x))))
                        (choice-support dk 'exact-posterior))))))

  ; cf. Hansei's probcc-reify/0 (aux.hansei.scm): the lazy search tree of model, a list of
  ; ((V v) p) and ((C thunk) p) entries, as probcc-explore and probcc-normalize take it: the
  ; model run under exact-handler, its value a leaf (probcc-unit).
  (define (model->search-tree model)
    (check-model model 'model->search-tree)
    (handle-model model exact-handler (o probcc-unit val-of)))

  ; (new, PLAN-IDIOM C29) build model with the trace builder from a stdgen seeded with 17
  ; (M:858) to check its static discipline, raising what the build raises: construction,
  ; conditioning, branch-escape and dependency errors.  The build is thrown away.  It runs in
  ; 'extended mode, whose build differs from 'upstream only by build-guard (deviation 8, C22):
  ; a `parameter` error raised in a branch the drawn values leave inactive (both branches are
  ; built, M:650-661) is a dynamic, value-dependent condition, which the enumeration never
  ; meets (exact-handler never runs an untaken branch), not a breach of the discipline, so it
  ; rejects the build instead of failing it.  A rejected build is retried on the same advanced
  ; source, as initial-step does (M:882-891), but only while builds are rejected (a complete
  ; build of LL -inf has checked all it can), at most 1000 builds in all; when none completes
  ; the validation is skipped, as every build met such a condition.  Hence the answer does not
  ; depend on what seed 17 happens to draw first (e.g. the order of a `uniformly` support).
  ; With 'upstream, as `mcmC` builds by default, such a model may fail at one seed only.
  (define (validation-build model)
    (let1 (src (make-stdgen-source 17))
      (let retry ((limit 1000))
        (when (positive? limit)
          (receive (ch result) (build-chain model src 'extended)
            (unless result (retry (sub1 limit))))))))

  ; cf. plan §6.10 (as amended by PLAN-IDIOM §2.3) and Hansei's probcc-reify/exact: the
  ; posterior of a model whose random choices, in the branches it takes, all have finite
  ; supports (else a `support` error), by exhaustive exploration of its search tree: the
  ; histogram ((value . probability) ...) sorted by generic<?, '() when no run has a positive
  ; weight.  With `validate` (PLAN-IDIOM C29) the trace builder first builds the model
  ; (validation-build), checking the static discipline that plain values cannot (observations
  ; external to the model, no node escaping its branch, ...).
  (define (exact-posterior model #!key (validate #t))
    (check-model model 'exact-posterior)
    (when validate (validation-build model))
    (histogram-sort
      (map (Λ (((V ,v) ,p)) (cons v p))
           (probcc-normalize (probcc-explore +inf.0 (model->search-tree model))))))

  ;; ---------------------------------------------------------------------------------------
  ;; Forward sampling (PLAN-IDIOM §2.4): a one-shot handler
  ;; ---------------------------------------------------------------------------------------

  ; (new, PLAN-IDIOM §2.4) run model once forward on the rng source src, under a one-shot
  ; handler that samples every `dist` with distk-sample (M:97-100 `sampleK`: a choice is drawn,
  ; an observation answers its value and LL without drawing), in the order the trace builder
  ; does (C18).  Answers (values value log-weight).  `weight` 'joint sums, in the order they
  ; happen, the LLs of the choices and of the observations, as the builder's llcum does
  ; (M:404-419); 'likelihood those of the observations only, the likelihood weight.
  ; With `construction-order` an if_ first runs the branch its test does not select, as ifnode!
  ; builds it (M:650-661), under the same handler except that its observations are not weighed
  ; (they are inactive: deactivate takes their LL back, M:659) and that it runs under
  ; lazy-lifting, as build-branch! builds an inactive branch (a partial lifted function there,
  ; e.g. car of the '() its test rules out, defers its error, cf. S:100, M:99; the
  ; parameterize sits inside the nested model, under its reset, so that resumptions re-enter
  ; it), and then the selected branch:
  ; the run then reproduces the builder's initial trace from the same source (see
  ; construction-check).  The weight is the run's own state, kept in a variable: a `reset`
  ; delivers one value.
  (define (forward-run model src #!key (weight 'joint) (construction-order #f))
    (unless (memq weight '(joint likelihood))
      (hakaru10-error 'construction 'forward-run "weight: must be 'joint or 'likelihood" weight))
    (let1 (log-weight 0.)
      (define (forward observing?)
        (value-handler
          (λ (dk resume)
            (receive (x ll) (distk-sample dk src)
              (when (if (eq? (distk-kind dk) 'observed) observing? (eq? weight 'joint))
                (set! log-weight (+ log-weight ll)))
              (resume x)))
          #:on-if (if construction-order
                      (λ (test th el)
                        (let* ((taken (select-branch test th el))
                               (untaken (if (eq? taken th) el th)))
                          (τ (handle-model (τ (parameterize ((lazy-lifting #t)) (untaken)))
                                           (forward #f))
                             (taken))))
                      select-branch)))
      (let1 (v (handle-model model (forward #t) val-of))
        (values v log-weight))))

  ; (new, PLAN-IDIOM §2.4) n forward runs of model, in order, on one rng source (`rng` as is,
  ; else a stdgen seeded with `seed`, M:858): the list of their (value . log-likelihood), the
  ; LL of the observations being the likelihood weight of the run.
  (define (weighted-samples n model #!key (seed 17) (rng #f))
    (check-model model 'weighted-samples)
    (let1 (src (or rng (make-stdgen-source seed)))
      (let loop ((i 0) (samples '()))
        (if (>= i n)
            (reverse samples)
            (receive (v ll) (forward-run model src #:weight 'likelihood)
              (loop (add1 i) (cons (cons v ll) samples)))))))

  ; (new, PLAN-IDIOM §2.4) the posterior of model estimated by likelihood weighting over n
  ; forward runs (weighted-samples, the same keys): the histogram ((value . probability) ...)
  ; sorted by generic<?, each run weighing exp(ll - max ll), for numerical stability; runs of
  ; zero likelihood count for nothing, and '() is the answer when every run has one.  It works
  ; for continuous choices too, where exact-posterior does not.
  (define (likelihood-weighting n model #!key (seed 17) (rng #f))
    (let* ((samples (filter-map (Λ ((,v . ,ll)) (and (finite? ll) (cons v ll)))
                                (weighted-samples n model #:seed seed #:rng rng)))
           (top (fold (λ (s m) (max (cdr s) m)) -inf.0 samples))
           (table (make-hash-table equal?)))
      (for-each (Λ ((,v . ,ll))
                  (hash-table-update!/default table v (λ (w) (+ w (exp (- ll top)))) 0.))
                samples)
      (let1 (total (hash-table-fold table (λ (v w acc) (+ acc w)) 0.))
        (histogram-sort
          (map (Λ ((,v . ,w)) (cons v (/ w total))) (hash-table->alist table))))))

  ; (new, PLAN-IDIOM §2.4; the first check of `check:`, plan §6.9) build the trace of model
  ; and run it forward in construction order (forward-run), each from its own copy of the rng
  ; source (`rng`, else a stdgen seeded with `seed`), and compare: the result's value
  ; (same-value?) and the builder's llcum with the run's joint log-weight, within 1e-9
  ; relative (not bit for bit: deactivation adds, then subtracts, the inactive observations'
  ; LL, M:659).  An oracle, independent of the engine's construction code, of the creation
  ; order (C18), the draw order and the LL accounting.  Answers #t; 'skipped when the build is
  ; one initial-step would retry (M:882-891): no result ('extended, C22) or an llcum that is
  ; not finite (an observation of LL -inf, NaN when it is inactive, plan §8); else raises an
  ; `internal` error.
  (define (construction-check model #!key (seed 17) (rng #f) (semantics 'upstream))
    (let1 (src (or rng (make-stdgen-source seed)))
      (receive (ch result) (build-chain model (rng-source-copy src) semantics)
        (let1 (llcum (chain-llcum ch))
          (if (not (and result (finite? llcum)))
              'skipped
              (receive (v ll) (forward-run model (rng-source-copy src) #:construction-order #t)
                (cond
                  ((not (same-value? v (node-value result)))
                   (hakaru10-error 'internal 'check
                                   "the construction-order run and the initial trace differ in value"
                                   v (node-value result)))
                  ((not (ll-close? ll llcum))
                   (hakaru10-error 'internal 'check
                                   "the construction-order run and the initial trace differ in LL"
                                   ll llcum))
                  (else #t))))))))

  ;; ---------------------------------------------------------------------------------------
  ;; The reference evaluator (plan §6.9)
  ;; ---------------------------------------------------------------------------------------

  ; (new, plan §6.9) whether n is a resampleable node (M:125 KResampleable).
  (define (resampleable? n) (eq? (node-kind n) 'resampleable))

  ; cf. S:201-208 `eval_nctor` and S:233-253 `doupdate`: the distk that node n's distribution
  ; function gives on its parents' current values, in pure context; a root's is its constant
  ; distk (M:421-423).
  (define (node-distk n)
    (let1 (fn (node-fn n))
      (if (distk? fn) fn (pure-call (τ (fn (leaf-values (node-parents n))))))))

  ; (new, plan §6.9, §6.11) the state of node n recomputed from its parents' current values, as
  ; its construction or its update gives it, with tstamp ts and, if n is resampleable, value v
  ; (by default its own: it is not a function of the parents):
  ;   - an exit: the value of the result its entry selects (M:778-794), LL 0;
  ;   - a Dirac node, entries included: the distk's value, LL 0 (M:52, M:55, M:482);
  ;   - an observed node: the observation and its LL (M:97-100, M:483-488);
  ;   - a resampleable node: v and its LL under the new distribution (M:483-488).
  ; deviation 10 (M:46-60): a distribution function that changes the kind of its node raises
  ; `construction`, as doupdate does.
  (define (recomputed-state n ts #!optional (v (node-value n)))
    (if (eq? (node-role n) 'exit)
        (make-nstate (node-value (branch-active-result (node-branch n))) 0. #f ts)
        (let* ((dk (node-distk n)) (kind (distk-kind dk)) (rd (distk-rd dk)))
          (unless (eq? kind (node-kind n))
            (hakaru10-error 'construction 'check
                            "a distribution constructor changed the kind of its node"
                            n (node-kind n) kind))
          (case kind
            ((dirac) (make-nstate (distk-value dk) 0. #f ts))
            ((observed) (let1 (obs (distk-value dk))
                          (make-nstate obs ((rdistr-log-density rd) obs) rd ts)))
            (else (make-nstate v ((rdistr-log-density rd) v) rd ts))))))

  ; (new, plan §6.9) the deactivation count node n must have (M:174-179, M:688-751): the
  ; number of the branches (entry . polarity) enclosing it (its scope) that the entry's current
  ; value does not select.
  (define (scope-inactivity n)
    (count (Λ ((,entry . ,polarity)) (not (eq? (node-value entry) polarity))) (node-scope n)))

  ; (new, plan §6.9) whether two rdistrs (or #f, a Dirac's) are the same distribution: the
  ; same name and equal? parameters.
  (define (same-rdistr? a b)
    (if (and a b)
        (and (eq? (rdistr-name a) (rdistr-name b)) (equal? (rdistr-params a) (rdistr-params b)))
        (eq? a b)))

  ; (new, plan §6.9) the reference evaluator of `check:`, run on the initial trace and after
  ; every checked step: recompute every node from its parents' values in address order and
  ; compare with the incremental state.  Parents come first (topological order, Mem:59-63), so
  ; checking each node against its parents' stored values is, by induction, recomputation from
  ; scratch.  Checked, for every node: the deactivation count (scope-inactivity); a delayed
  ; update only on an inactive node and never in 'extended mode (M:181-183, M:298-303); no
  ; pending weight and no queued update left over; no tstamp after now.  For every active node,
  ; and every node in 'extended mode, where no update is delayed (an inactive node's delayed
  ; update leaves it stale in 'upstream mode): the state against recomputed-state, value
  ; same-value?, distribution the same (same-rdistr?), LL within 1e-9.  For the chain: card
  ; change = minus the number of inactive resampleable nodes (M:193-194).  Answers #t, else
  ; raises an `internal` error at the first difference, naming the node's address.
  ; Kept as upstream (plan C10): a categorical with duplicate keys samples the LL of the entry
  ; it walks to, not the first match of its density, so such a node may fail the LL check.
  (define (reference-check ch)
    (define (fail msg n . args)
      (apply hakaru10-error 'internal 'check msg (node-addr n) args))
    (define (check-state n s)
      (let1 (s* (recomputed-state n (nstate-tstamp s) (nstate-val s)))
        (unless (same-value? (nstate-val s) (nstate-val s*))
          (fail "a value differs from its recomputation" n (nstate-val s) (nstate-val s*)))
        (unless (same-rdistr? (nstate-dst s) (nstate-dst s*))
          (fail "a distribution differs from its recomputation" n (nstate-dst s) (nstate-dst s*)))
        (unless (ll-close? (nstate-ll s) (nstate-ll s*))
          (fail "an LL differs from its recomputation" n (nstate-ll s) (nstate-ll s*)))))
    (let ((now (chain-now ch)) (every-node? (extended? ch)))
      (chain-fold
        (λ (n _)
          (let ((s (node-state n)) (inactive (node-inactive n)))
            (unless (= inactive (scope-inactivity n))
              (fail "a deactivation count differs from its recomputation" n inactive
                    (scope-inactivity n)))
            (when (and (node-delayed n) (or every-node? (zero? inactive)))
              (fail "an update is delayed on an active node or in 'extended mode" n))
            (when (or (node-pending n) (node-queued n))
              (fail "a node is still pending or queued after the step" n))
            (when (> (nstate-tstamp s) now)
              (fail "a tstamp is after now" n (nstate-tstamp s) now))
            (when (or every-node? (zero? inactive)) (check-state n s))))
        (void) ch)
      (let1 (card (- (count node-inactive? (chain-sampleable ch))))
        (unless (= (chain-card-change ch) card)
          (hakaru10-error 'internal 'check "the card change differs from its recomputation"
                          (chain-card-change ch) card)))
      #t))

  ;; ---------------------------------------------------------------------------------------
  ;; The kernel residual (plan §6.11; tests only; 'extended semantics; finite supports)
  ;; ---------------------------------------------------------------------------------------

  ; (new, plan §6.11) the resampleable nodes of ch in address order, the order of the values
  ; of an assignment (chain-sampleable is newest first, M:239).
  (define (resampleable-nodes ch) (reverse (chain-sampleable ch)))

  ; (new, plan §6.11 step 2) load into ch the assignment `xs`, the values of its resampleable
  ; nodes in address order (resampleable-nodes), and recompute every other field from scratch,
  ; in address order, as reference-check expects them: the deactivation counts
  ; (scope-inactivity), the states (recomputed-state, tstamp now), the card change; no update
  ; delayed or pending.  A node's count is set before its state is recomputed, because the
  ; recomputation reads it: a lazy diracN node defers its value's error only while it is
  ; inactive (deviation 11), e.g. guarded-car's car of '(); the entries that the count reads
  ; come earlier in address order, so their values are already the new assignment's.  Raw
  ; writes, outside any step: nothing is journaled.
  (define (load-state! ch xs)
    (let* ((now (chain-now ch))
           (rest (chain-fold
                   (λ (n xs)
                     (node-inactive-set! n (scope-inactivity n))
                     (node-state-set! n (if (resampleable? n)
                                            (if (pair? xs)
                                                (recomputed-state n now (car xs))
                                                (hakaru10-error 'construction 'load-state!
                                                                "too few values" n))
                                            (recomputed-state n now)))
                     (node-delayed-set! n #f)
                     (node-pending-set! n #f)
                     (if (resampleable? n) (cdr xs) xs))
                   xs ch)))
      (unless (null? rest)
        (hakaru10-error 'construction 'load-state! "too many values" rest))
      (chain-card-change-set! ch (- (count node-inactive? (chain-sampleable ch))))))

  ; (new, plan §6.11 step 1) the states of the extended space ('extended semantics, plan
  ; §6.8): every assignment of all of ch's resampleable nodes, the inactive ones included,
  ; each ranging over the support points of probability > 0 of the distribution its parents'
  ; values give it; the list of the value lists, as load-state! takes them.  A depth-first walk
  ; in address order, recomputing each node on its way as load-state! does (its deactivation
  ; count, then its state), so that ch is left with the last assignment's values; more than
  ; max-states assignments is a `support` error.
  (define (ext-states ch #!key (max-states 100000))
    (let ((now (chain-now ch)) (last (chain-count ch)) (found 0))
      (reverse
        (let walk ((a 1) (prefix '()) (states '()))
          (if (> a last)
              (begin
                (add1! found)
                (when (> found max-states)
                  (hakaru10-error 'support 'ext-states "more states than max-states" max-states))
                (cons (reverse prefix) states))
              (let1 (n (chain-node ch a))
                (node-inactive-set! n (scope-inactivity n))
                (if (resampleable? n)
                    (fold (Λ ((,x . ,p) ,states)
                            (if (positive? p)
                                (begin
                                  (node-state-set! n (recomputed-state n now x))
                                  (walk (add1 a) (cons x prefix) states))
                                states))
                          states
                          (choice-support (node-distk n) 'ext-states))
                    (begin
                      (node-state-set! n (recomputed-state n now))
                      (walk (add1 a) prefix states)))))))))

  ; cf. M:908-944 `transit` and M:956-977 `resample_trace` with the proposal forced: propose
  ; the value x for the active resampleable node n, with the independence kernel's fwd - bwd =
  ; ll_x - ll_old (D:29-32, M:972), propagate (M:921) and answer the log acceptance ratio
  ; (M:929-934), -inf for an infinite LL difference (M:927; a NaN ratio stays NaN, a
  ; rejection, M:939-944); then roll the step back (M:905-906), so ch is as before and nothing
  ; is drawn.
  (define (forced-step-log-alpha ch n x)
    (when (or (not (resampleable? n)) (node-inactive? n))
      (hakaru10-error 'internal 'forced-step-log-alpha "an active resampleable node is expected" n))
    (journal-begin! ch)
    (chain-now-set! ch (add1 (chain-now ch)))
    (chain-llcum-set! ch 0.)
    (let* ((card-old (chain-card-change ch))
           (s (node-state n))
           (ll ((rdistr-log-density (nstate-dst s)) x))
           (fb (apply-proposal! n x ll (- ll (nstate-ll s))))
           (delta (begin (propagate! ch) (chain-llcum ch)))
           (ratio (if (rejected-ll? delta)
                      -inf.0
                      (log-acceptance-ratio ch (vector-length (chain-samp ch)) card-old fb))))
      (journal-rollback! ch)
      ratio))

  ; (new, plan §6.11; Design 3 §5.9) how far the target of the 'extended semantics (plan §6.8)
  ; is from being stationary for the engine's own MH kernel (M:897-977): max over the states x
  ; of |(πT)(x) - π(x)|, where π(x) ∝ exp(chain-extended-ll) over ext-states (load-state!
  ; each), and T is the one-step kernel computed exactly from the real propagation and
  ; acceptance code (forced-step-log-alpha): an active resampleable node a chosen with
  ; probability 1/N(x) (resample_trace's retry, M:962-966), a support point (v . q) of its
  ; distribution proposed with probability q (the independence proposal of D:29-32, every
  ; built-in distribution with a support has it), accepted with probability min(1, e^ratio),
  ; the rest of the mass staying at x; a state with no active resampleable node stays put
  ; (deviation 6).  The model is built once (rng seeded with `seed`); every resampleable node
  ; must have a finite support.  Below 1e-12 the kernel leaves π invariant.
  ; `semantics` picks the kernel T: 'extended (the default) or 'upstream, whose step delays
  ; the updates of inactive nodes (M:298-303), so that their LL changes do not count in Δ.
  ; Upstream's chain is not a Markov chain on the assignments alone (the delayed updates are
  ; part of its state); load-state! starts each step from the state where no update is
  ; delayed, so the 'upstream residual is that of the one-step kernel from such states, and a
  ; positive one shows that upstream's kernel does not leave π invariant (pswitch: M:1159-1165
  ; and plan §6.8).
  (define (kernel-residual model #!key (seed 17) (max-states 100000) (semantics 'extended))
    (check-model model 'kernel-residual)
    (receive (ch result) (build-chain model (make-stdgen-source seed) semantics)
      (unless result
        (hakaru10-error 'construction 'kernel-residual "the model's first build was rejected"))
      (let* ((nodes (resampleable-nodes ch))
             (states (ext-states ch #:max-states max-states))
             (log-weights (map (λ (xs) (load-state! ch xs) (chain-extended-ll ch)) states))
             (top (fold max -inf.0 log-weights))
             (weights (map (λ (l) (exp (- l top))) log-weights))
             (z (fold + 0. weights))
             (π (alist->hash-table (map (λ (xs w) (cons xs (/ w z))) states weights) equal?))
             (πT (make-hash-table equal?)))
        (define (flow! xs mass) (hash-table-update!/default πT xs (λ (m) (+ m mass)) 0.))
        (hash-table-walk
          π
          (λ (xs p)
            (when (positive? p)
              (load-state! ch xs)
              (let* ((active (filter-map (λ (n i) (and (not (node-inactive? n)) (cons n i)))
                                         nodes (iota (length nodes))))
                     (pick (if (null? active) 0. (/ p (length active)))))
                (when (null? active) (flow! xs p))
                (for-each
                  (Λ ((,n . ,i))
                    (for-each
                      (Λ ((,x . ,q))
                        (when (positive? q)
                          (let* ((ratio (forced-step-log-alpha ch n x))
                                 (accept (if (nan? ratio) 0. (min 1. (exp ratio))))
                                 (mass (* pick q)))
                            (flow! (list-replace xs i x) (* mass accept))
                            (flow! xs (* mass (- 1. accept))))))
                      (choice-support (node-distk n) 'kernel-residual)))
                  active)))))
        (fold (λ (xs m) (max m (abs (- (hash-table-ref/default πT xs 0.)
                                       (hash-table-ref/default π xs 0.)))))
              0.
              (append (hash-table-keys π) (hash-table-keys πT))))))

  ; (new, plan §6.11) the list xs with its i-th element replaced by x.
  (define (list-replace xs i x)
    (if (zero? i) (cons x (cdr xs)) (cons (car xs) (list-replace (cdr xs) (sub1 i) x))))

  )
