;; (aux hakaru10 exact) -- exact evaluators that check (aux hakaru10), a CHICKEN port of
;; Oleg Kiselyov's Hakaru10 (https://okmij.org/ftp/kakuritu/Hakaru10/index.html).
;; Internal: (aux hakaru10) re-exports exact-posterior, model->search-tree,
;; likelihood-weighting and weighted-samples, and its `check:` runs construction-check and
;; reference-check.
;;
;; Mirrors no upstream file: it is new.  It checks the chain built by the engine, which mirrors
;;   M  Metropolis.hs (https://okmij.org/ftp/kakuritu/Hakaru10/Metropolis.hs) and
;;   S  Syntax.hs (https://okmij.org/ftp/kakuritu/Hakaru10/Syntax.hs),
;; with oracles that walk the trace graph of one build of the model (plan §6.10, C37), with no
;; delimited control:
;;   - exact enumeration into a lazy search tree in Hansei's ((V v) p) / ((C thunk) p) format,
;;     explored and normalized with the data-level probcc-explore and probcc-normalize of (aux
;;     hansei) (https://okmij.org/ftp/kakuritu/Hansei.html; C39);
;;   - forward sampling and likelihood weighting;
;;   - construction-check, the first check of `check:`: a rebuild and an address-order redraw;
;;   - the reference evaluator (plan §6.9), which recomputes every node from scratch and
;;     compares it with the incremental state, and the kernel residual of the 'extended
;;     semantics (plan §6.11).
;; What they share with the engine is the graph and the node functions only, none of the
;; incremental machinery (propagate!, the update queue, switch-branches!, the delayed and
;; pending updates, the journal, the acceptance ratio): so exact posteriors against MH test the
;; kernel and the incremental update, kernel-residual tests the kernel exactly, and
;; reference-check every incremental state.  The oracle of creation order that would not run
;; the construction code is lost: the replays, the redraw and the per-node signatures of the
;; tests stand for it.
;; The graph is that of an 'extended build (static-graph), so a model that every 'extended
;; build rejects raises a `construction` error here, even where mcmC samples it.  One build is
;; the whole graph because SExp is applicative, not a monad (S:118), and if_ builds both
;; branches (M:650-661); that assumes that no model branches in Scheme on a node or on a value
;; drawn at build time, which nothing enforces and only construction-check's rebuild catches.
;;
;; Hakaru10 code version: November 2016 (W:52-53); web page last updated July 14, 2018 (W:179).
;; Paper: O. Kiselyov, "Probabilistic Programming Language and its Incremental Evaluation",
;; APLAS 2016, LNCS 10017, DOI 10.1007/978-3-319-47958-3_19 (P = design.txt / design.pdf).
;;
;; Contents:
;;   - the static trace graph (plan §6.10, C37): build, static-graph, walk-active!,
;;     branch-starts, past-inactive-branches;
;;   - exact enumeration (plan §6.10): graph->search-tree, model->search-tree, exact-posterior;
;;   - forward sampling (plan §6.10): graph-forward, weighted-samples, likelihood-weighting;
;;   - the construction check (plan §6.9, C37): node-signature, same-built-value?,
;;     first-mismatch, construction-check, rebuild-check, redraw-check;
;;   - the reference evaluator (plan §6.9): node-distk, recomputed-state, scope-inactivity,
;;     reference-check;
;;   - the kernel residual (plan §6.11): load-state!, ext-states, forced-step-log-alpha,
;;     kernel-residual.

(module (aux hakaru10 exact) *

  (import scheme
          (chicken base)
          (only srfi-1 filter-map count fold iota any)
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

  ; (new, plan §6.9) whether two LLs agree within 1e-9, relative to the larger of 1 and
  ; `scale`, by default |a|: two NaNs agree too, and an infinity only with itself (an infinite
  ; |a| would scale the tolerance to +inf, so that -inf would agree with any LL).  A sum
  ; compares with the sum of its terms' magnitudes as scale, since its rounding error grows
  ; with them, not with the sum (redraw-check).
  (define (ll-close? a b #!optional (scale (abs a)))
    (or (= a b)
        (and (nan? a) (nan? b))
        (and (finite? a) (finite? b) (<= (abs (- a b)) (* 1e-9 (max 1. scale))))))

  ; (new, plan §6.9) whether the value a check recomputed matches the incremental one: equal?,
  ; except that two procedures match and that a value too large to walk matches.  A lifted
  ; function or a diracN may answer a closure built at run time (check-lifted-value rejects
  ; only SExps and Models), and recomputing it builds a fresh one, which equal? (eq? on
  ; procedures) would report as a difference: its consumers, which apply it, are checked
  ; themselves.  The walk stops, matching, after 100000 pairs and vector slots, so that a
  ; circular value (which equal? would walk forever, and which the engine accepts,
  ; check-no-sexp) ends the check.
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
  ;; The static trace graph (plan §6.10, C37)
  ;; ---------------------------------------------------------------------------------------

  ; (new) the trace of model built from src, on behalf of who: cf. S:197 `m >>= to_node` and
  ; M:872-880.  The only place this module runs a model: static-graph, construction-check and
  ; kernel-residual build through it.
  (define (build model src semantics who) (build-chain (model->trace model who) src semantics))

  ; (new, plan §6.10, C37) the trace graph of model: an 'extended build (C22: a parameter error
  ; in a branch the drawn values leave inactive rejects the build instead of failing it) from a
  ; private stdgen seeded with 17 (M:858), retried on the same advanced source while rejected,
  ; at most 1000 builds (M:882-891); a `construction` error when none completes.  SExp is
  ; applicative, not a monad (S:118), and if_ builds both branches (M:650-661), so one complete
  ; build is the whole graph, and it checks the static discipline (deviations 9, 10) as it goes.
  ; A complete build of LL -inf is the graph too (an observation impossible on that build).
  ; The graph is the whole model only if no model branches in Scheme on a node or on a value
  ; drawn at build time: unenforced, and caught only by construction-check's rebuild.
  (define (static-graph model who)
    (let1 (src (make-stdgen-source 17))
      (let retry ((builds 1000))
        (if (zero? builds)
            (hakaru10-error 'construction who "no complete build of the model in 1000 tries" model)
            (receive (ch result) (build model src 'extended who)
              (if result (values ch result) (retry (sub1 builds))))))))

  ; (new, plan §6.10, C37) whether n is active under the entry values now in its chain; its
  ; deactivation count is written first (the lazy diracN function reads it, as in load-state!).
  ; An inactive node is skipped by the walks below: no active node reads it (deviation 10's
  ; escape check), and an exit reads only the active result (M:781-794).
  (define (walk-active! n) (let1 (k (scope-inactivity n)) (node-inactive-set! n k) (zero? k)))

  ; (new, plan §6.10) the branches of the graph ch by the address where their nodes start: a
  ; vector whose slot a lists the (hi entry polarity) of every non-empty range (lo . hi) of a
  ; branch (M:650-668) with lo + 1 = a.  A branch's nodes are created together, at the
  ; addresses lo + 1 .. hi, each with (entry . polarity) in its scope (deviation 10).
  (define (branch-starts ch)
    (let1 (starts (make-vector (add1 (chain-count ch)) '()))
      (chain-fold
        (λ (n _)
          (when (eq? (node-role n) 'exit)
            (let1 (b (node-branch n))
              (for-each (Λ ((,lo . ,hi) ,polarity)
                          (when (< lo hi)
                            (vector-set! starts (add1 lo)
                                         (cons (list hi (branch-entry b) polarity)
                                               (vector-ref starts (add1 lo))))))
                        (list (branch-then-range b) (branch-else-range b))
                        '(#t #f)))))
        (void) ch)
      starts))

  ; (new, plan §6.10) the first address from a on that no branch starting there (branch-starts)
  ; leaves inactive under the entry values now in its chain.  The walks below jump over an
  ; inactive branch instead of skipping its nodes one by one (walk-active!), so a walk of a
  ; model with large inactive branches (colored balls) costs in proportion to its active
  ; nodes.  The nodes jumped over keep their last deactivation counts: nothing reads them.
  (define (past-inactive-branches starts a)
    (let1 (hi (and (< a (vector-length starts))
                   (any (Λ ((,hi ,entry ,polarity)) (and (not (eq? (node-value entry) polarity)) hi))
                        (vector-ref starts a))))
      (if hi (past-inactive-branches starts (add1 hi)) a)))

  ;; ---------------------------------------------------------------------------------------
  ;; Exact enumeration (plan §6.10, C37): a lazy search tree over the graph
  ;; ---------------------------------------------------------------------------------------

  ; (new, plan §6.10) cf. Hansei's probcc-reify/0 (aux.hansei.scm), the tree format: the lazy
  ; search tree of the graph ch: one ((C thunk) p) per support point of an active choice, one
  ; ((C thunk) w) per active observation (none when w = 0), (probcc-unit v) at the leaves.  A
  ; thunk may be forced in any order (probcc-explore to depth 0, H3), so it first restores
  ; `frame`, the (node . nstate) of its path's active nodes, newest first, tails shared (nstates
  ; are immutable, C5).  More than max-paths leaves reached is a `support` error; the count is
  ; the tree's, over all its explorations, so a tree built with max-paths is explored once.
  ; The walk visits addresses 1..count in order, creation and draw order (C18), recomputing
  ; each active node from its parents (recomputed-state): an observation is weighed by its
  ; likelihood exp(log-density), a density for a continuous distribution (right for posteriors
  ; over discrete choices); a choice without a finite support is a `support` error.
  (define (graph->search-tree ch result #!key (max-paths #f) (who 'exact-posterior))
    (let ((last (chain-count ch)) (now (chain-now ch)) (starts (branch-starts ch)) (paths 0))
      (define (restore! frame)
        (for-each (Λ ((,n . ,s)) (node-inactive-set! n 0) (node-state-set! n s)) frame))
      (let walk ((a (past-inactive-branches starts 1)) (frame '()))
        ; the walk on from a's node n, entered with the state s (stored before the next
        ; address is computed, since n may be the entry of the branches starting there).
        (define (enter n s)
          (node-state-set! n s)
          (walk (past-inactive-branches starts (add1 a)) (cons (cons n s) frame)))
        (if (> a last)
            (begin
              (add1! paths)
              (when (and max-paths (> paths max-paths))
                (hakaru10-error 'support who "more paths than max-paths" max-paths))
              (probcc-unit (force-value (node-value result))))
            (let1 (n (chain-node ch a))
              (cond
                ((not (walk-active! n)) (walk (past-inactive-branches starts (add1 a)) frame))
                ((resampleable? n)
                 (filter-map (Λ ((,x . ,p))
                               (and (positive? p)
                                    (probcc-τ p (restore! frame)
                                              (enter n (recomputed-state n now x)))))
                             (choice-support (node-distk n) who)))
                ((eq? (node-kind n) 'observed)
                 (let* ((s (recomputed-state n now)) (w (exp (nstate-ll s))))
                   (if (zero? w)
                       '()
                       (list (probcc-τ w (restore! frame) (enter n s))))))
                (else (enter n (recomputed-state n now)))))))))

  ; cf. Hansei's probcc-reify/0 (aux.hansei.scm): the lazy search tree of model, a list of
  ; ((V v) p) and ((C thunk) p) entries, as probcc-explore and probcc-normalize take it: the
  ; walk of its static graph (graph->search-tree).  With max-paths, the tree counts the leaves
  ; of all its explorations, and raises `support` past max-paths: explore it once.
  (define (model->search-tree model #!key (max-paths #f))
    (receive (ch result) (static-graph model 'model->search-tree)
      (graph->search-tree ch result #:max-paths max-paths #:who 'model->search-tree)))

  ; cf. plan §6.10 and Hansei's probcc-reify/exact: the posterior of a model whose random
  ; choices, in the branches it takes, all have finite supports (else a `support` error), by
  ; exhaustive exploration of the search tree of its static graph: the histogram ((value .
  ; probability) ...) sorted by generic<?, '() when no path has a positive weight.  The build
  ; of the graph checks the static discipline (observations external to the model, no node
  ; escaping its branch, ...); more than max-paths paths is a `support` error.
  (define (exact-posterior model #!key (max-paths #f))
    (receive (ch result) (static-graph model 'exact-posterior)
      (histogram-sort
        (map (Λ (((V ,v) ,p)) (cons v p))
             (probcc-normalize
               (probcc-explore +inf.0 (graph->search-tree ch result #:max-paths max-paths)))))))

  ;; ---------------------------------------------------------------------------------------
  ;; Forward sampling (plan §6.10, C37): walks of the graph
  ;; ---------------------------------------------------------------------------------------

  ; (new, plan §6.10) one forward walk of the graph ch on the rng source src, in address
  ; order: every active choice and observation sampled with distk-sample (M:97-100 `sampleK`:
  ; a choice is drawn, an observation answers its value and LL without drawing), the other
  ; active nodes recomputed.  Answers (values value log-likelihood), the sum of the active
  ; observations' LLs in address order, the likelihood weight.  The active nodes in address
  ; order are the choices and observations of a run of the model in its bind order (C35), so
  ; the draws are those of that run.  `starts` is (branch-starts ch), which a caller walking
  ; ch many times computes once.
  (define (graph-forward ch result src #!optional (starts (branch-starts ch)))
    (let ((last (chain-count ch)) (now (chain-now ch)))
      (let walk ((a (past-inactive-branches starts 1)) (ll 0.))
        (define (next) (past-inactive-branches starts (add1 a)))
        (if (> a last)
            (values (force-value (node-value result)) ll)
            (let1 (n (chain-node ch a))
              (cond
                ((not (walk-active! n)) (walk (next) ll))
                ((memq (node-kind n) '(resampleable observed))
                 (receive (x llx) (distk-sample (node-distk n) src)
                   (node-state-set! n (recomputed-state n now x))
                   (walk (next) (if (resampleable? n) ll (+ ll llx)))))
                (else (node-state-set! n (recomputed-state n now)) (walk (next) ll))))))))

  ; (new, plan §6.10) n forward walks of the static graph of model, in order, on one rng source
  ; (`rng` as is, else a stdgen seeded with `seed`, M:858): the list of their (value .
  ; log-likelihood), the LL of the observations being the likelihood weight of the run.
  (define (weighted-samples n model #!key (seed 17) (rng #f))
    (receive (ch result) (static-graph model 'weighted-samples)
      (let ((src (or rng (make-stdgen-source seed))) (starts (branch-starts ch)))
        (let loop ((i 0) (samples '()))
          (if (>= i n)
              (reverse samples)
              (receive (v ll) (graph-forward ch result src starts)
                (loop (add1 i) (cons (cons v ll) samples))))))))

  ; (new, plan §6.10) the posterior of model estimated by likelihood weighting over n
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

  ;; ---------------------------------------------------------------------------------------
  ;; The construction check (plan §6.9, C37): a rebuild and an address-order redraw
  ;; ---------------------------------------------------------------------------------------

  ; (new, plan §6.9) the shape of node n in its graph: its address, kind and role, its
  ; parents' addresses and its scope's (entry address . polarity).
  (define (node-signature n)
    (list (node-addr n) (node-kind n) (node-role n)
          (map node-addr (vector->list (node-parents n)))
          (map (Λ ((,entry . ,polarity)) (cons (node-addr entry) polarity)) (node-scope n))))

  ; (new, plan §6.9) whether the values of two builds' nodes match: same-value?, a deferred
  ; value (deviation 11) by its arguments, since its closure is fresh per build.
  (define (same-built-value? a b)
    (cond
      ((and (deferred? a) (deferred? b)) (same-value? (deferred-args a) (deferred-args b)))
      ((or (deferred? a) (deferred? b)) #f)
      (else (same-value? a b))))

  ; (new, plan §6.9) the first address at which the nodes of the chains ch and ch*, of as many
  ; nodes, are not `same?`, else #f.
  (define (first-mismatch same? ch ch*)
    (let loop ((a 1))
      (and (<= a (chain-count ch))
           (if (same? (chain-node ch a) (chain-node ch* a)) (loop (add1 a)) a))))

  ; (new, plan §6.9, C37; the first check of `check:`) build model, rebuild it, and redraw its
  ; choices in address order, each from its own copy of the rng source (`rng`, else a stdgen
  ; seeded with `seed`):
  ;   - the rebuild must give the same graph (node-signature, and the result's address), the
  ;     same values (same-built-value?) and the same LLs, llcum included, within 1e-9: a model
  ;     whose construction depends on state outlived by its runs (a counter) fails it;
  ;   - drawing every resampleable node, the inactive ones included, in address order from its
  ;     distk against the stored parents must give back every stored value, and the drawn LLs
  ;     plus those of the active observations must be llcum within 1e-9, relative to the sum
  ;     of the magnitudes of every LL that went into llcum: the draw order is the creation
  ;     order (C18), and the LL accounting holds.  In 'upstream mode that sum counts the
  ;     inactive observations' LLs too, and the comparison is not bit for bit: deactivation
  ;     adds, then subtracts, an inactive observation's LL (M:659), whose rounding error grows
  ;     with that LL.  In 'extended mode llcum is the extended LL (chain-extended-ll), whose
  ;     terms and order are the redraw's, and an inactive observation's LL, which never went
  ;     into it, does not widen the tolerance.
  ; Answers #t; 'skipped when the build is one initial-step would retry (M:882-891): no result
  ; ('extended, C22) or an llcum that is not finite (an observation of LL -inf, NaN when it is
  ; inactive, plan §8); else raises an `internal` error at the first difference.
  (define (construction-check model #!key (seed 17) (rng #f) (semantics 'upstream))
    (let1 (src (or rng (make-stdgen-source seed)))
      (receive (ch result) (build model (rng-source-copy src) semantics 'check)
        (if (not (and result (finite? (chain-llcum ch))))
            'skipped
            (receive (ch* result*) (build model (rng-source-copy src) semantics 'check)
              (rebuild-check ch result ch* result*)
              (redraw-check ch (rng-source-copy src))
              #t)))))

  ; (new, plan §6.9) the rebuild half of construction-check: ch and result against the
  ; rebuild ch* and result*.
  (define (rebuild-check ch result ch* result*)
    (define (differ what . args)
      (apply hakaru10-error 'internal 'check
             (string-append "a rebuild and the initial trace differ in " what) args))
    (unless (and result* (= (chain-count ch) (chain-count ch*))
                 (= (node-addr result) (node-addr result*)))
      (differ "graph" (chain-count ch) (and result* (chain-count ch*))))
    (cond
      ((first-mismatch (λ (n n*) (equal? (node-signature n) (node-signature n*))) ch ch*)
       => (λ (a) (differ "graph" a)))
      ((first-mismatch (λ (n n*) (same-built-value? (node-value n) (node-value n*))) ch ch*)
       => (λ (a) (differ "value" a (node-value (chain-node ch a)) (node-value (chain-node ch* a)))))
      ((first-mismatch (λ (n n*) (ll-close? (nstate-ll (node-state n)) (nstate-ll (node-state n*))))
                       ch ch*)
       => (λ (a) (differ "LL" a)))
      ((not (ll-close? (chain-llcum ch) (chain-llcum ch*)))
       (differ "LL" (chain-llcum ch) (chain-llcum ch*)))))

  ; (new, plan §6.9) the redraw half of construction-check: every resampleable node of ch
  ; drawn on src in address order.  The fold's accumulator is (ll . scale): the LL to compare
  ; with llcum, and the sum of the magnitudes of the LLs that went into llcum: every observed
  ; node's in 'upstream mode, the inactive ones added at their creation and subtracted at
  ; deactivation (M:659); only the active ones' in 'extended mode, whose llcum the build
  ; recomputes as chain-extended-ll.
  (define (redraw-check ch src)
    (define (differ what . args)
      (apply hakaru10-error 'internal 'check
             (string-append "an address-order redraw and the initial trace differ in " what) args))
    (match/first (chain-fold
                   (Λ (,n (,ll . ,scale))
                     (cond
                       ((resampleable? n)
                        (receive (x llx) (distk-sample (node-distk n) src)
                          (unless (same-value? x (node-value n))
                            (differ "value" (node-addr n) x (node-value n)))
                          (cons (+ ll llx) (+ scale (abs llx)))))
                       ((eq? (node-kind n) 'observed)
                        (let ((llo (nstate-ll (node-state n))) (inactive? (node-inactive? n)))
                          (cons (if inactive? ll (+ ll llo))
                                (if (and inactive? (extended? ch)) scale (+ scale (abs llo))))))
                       (else (cons ll scale))))
                   '(0. . 0.) ch)
      ((,ll . ,scale) (unless (ll-close? ll (chain-llcum ch) scale)
                        (differ "LL" ll (chain-llcum ch))))))

  ;; ---------------------------------------------------------------------------------------
  ;; The reference evaluator (plan §6.9)
  ;; ---------------------------------------------------------------------------------------

  ; (new, plan §6.9) whether n is a resampleable node (M:125 KResampleable).
  (define (resampleable? n) (eq? (node-kind n) 'resampleable))

  ; cf. S:201-208 `eval_nctor` and S:233-253 `doupdate`: the distk that node n's distribution
  ; function gives on its parents' current values; a root's is its constant distk (M:421-423).
  (define (node-distk n)
    (let1 (fn (node-fn n))
      (if (distk? fn) fn (fn (leaf-values (node-parents n))))))

  ; (new, plan §6.9, §6.11) the state of node n recomputed from its parents' current values, as
  ; its construction or its update gives it, with tstamp ts and, if n is resampleable, value v
  ; (by default its own: it is not a function of the parents):
  ;   - an exit: the value of the result its entry selects (M:778-794), LL 0, forced if the exit
  ;     is active, as update-exit does (deviation 11), so n's deactivation count must be set;
  ;   - a Dirac node, entries included: the distk's value, LL 0 (M:52, M:55, M:482);
  ;   - an observed node: the observation and its LL (M:97-100, M:483-488);
  ;   - a resampleable node: v and its LL under the new distribution (M:483-488).
  ; deviation 10 (M:46-60): a distribution function that changes the kind of its node raises
  ; `construction`, as doupdate does.
  (define (recomputed-state n ts #!optional (v (node-value n)))
    (if (eq? (node-role n) 'exit)
        (make-nstate (let1 (v (node-value (branch-active-result (node-branch n))))
                       (if (node-inactive? n) v (force-value v)))
                     0. #f ts)
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
  ; values give it; the list of the value lists, as load-state! takes them, one per combination
  ; of support points, so that an assignment is listed once per combination that gives it
  ; (twice for 1 of `uniformly '(1 1 2)`).  A depth-first walk in address order, recomputing
  ; each node on its way as load-state! does (its deactivation count, then its state), so that
  ; ch is left with the last assignment's values; more than max-states combinations is a
  ; `support` error.
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
  ; of |(πT)(x) - π(x)|, where π(x) ∝ exp(chain-extended-ll) summed over the combinations of
  ; ext-states that give x (load-state! each; a support with duplicate points, such as
  ; `uniformly '(1 1 2)`, gives 1 twice the weight of 2, as its sampler does), and T is the
  ; one-step kernel computed exactly from the real propagation and acceptance code
  ; (forced-step-log-alpha): an active resampleable node a chosen with probability 1/N(x)
  ; (resample_trace's retry, M:962-966), a support point (v . q) of its distribution proposed
  ; with probability q (the independence proposal of D:29-32, the default kernel;
  ; proposal-support), accepted with probability min(1, e^ratio), the rest of the mass staying
  ; at x; a state with no active resampleable node stays put (deviation 6).  The model is
  ; built once (rng seeded with `seed`); every resampleable node must have a finite support,
  ; and every active one the default kernel.  Below 1e-12 the kernel leaves π invariant.  A
  ; model where no state has a positive finite weight has no target π: a `construction` error.
  ; Kept as upstream (plan C10): a categorical with duplicate keys weighs each of a key's
  ; points by the density of its first weight, where its sampler gives the key the sum of its
  ; weights, so such a model has a positive residual.
  ; `semantics` picks the kernel T: 'extended (the default) or 'upstream, whose step delays
  ; the updates of inactive nodes (M:298-303), so that their LL changes do not count in Δ.
  ; Upstream's chain is not a Markov chain on the assignments alone (the delayed updates are
  ; part of its state); load-state! starts each step from the state where no update is
  ; delayed, so the 'upstream residual is that of the one-step kernel from such states, and a
  ; positive one shows that upstream's kernel does not leave π invariant (pswitch: M:1159-1165
  ; and plan §6.8).
  (define (kernel-residual model #!key (seed 17) (max-states 100000) (semantics 'extended))
    (receive (ch result) (build model (make-stdgen-source seed) semantics 'kernel-residual)
      (unless result
        (hakaru10-error 'construction 'kernel-residual "the model's first build was rejected"))
      (let* ((nodes (resampleable-nodes ch))
             (states (ext-states ch #:max-states max-states))
             (log-weights (map (λ (xs) (load-state! ch xs) (chain-extended-ll ch)) states))
             (top (let1 (top (fold max -inf.0 log-weights))
                    (if (finite? top)
                        top
                        (hakaru10-error 'construction 'kernel-residual
                                        "no state of the model has a positive finite weight"
                                        top))))
             (weights (map (λ (l) (exp (- l top))) log-weights))
             (z (fold + 0. weights))
             (π (make-hash-table equal?))
             (πT (make-hash-table equal?)))
        (define (flow! xs mass) (hash-table-update!/default πT xs (λ (m) (+ m mass)) 0.))
        (for-each (λ (xs w) (hash-table-update!/default π xs (λ (p) (+ p (/ w z))) 0.))
                  states weights)
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
                      (proposal-support n)))
                  active)))))
        (fold (λ (xs m) (max m (abs (- (hash-table-ref/default πT xs 0.)
                                       (hash-table-ref/default π xs 0.)))))
              0.
              (append (hash-table-keys π) (hash-table-keys πT))))))

  ; (new, plan §6.11) the support of the active resampleable node n as the kernel residual's T
  ; proposes it: the independence proposal of D:29-32 draws the support point (x . q) with
  ; probability q.  A distribution with another kernel, bern/flip's flip (D:64-70) or the
  ; resample: of make-distribution, is a `support` error: T would not be the engine's kernel.
  (define (proposal-support n)
    (let1 (dk (node-distk n))
      (unless (rdistr-independence? (distk-rd dk))
        (hakaru10-error 'support 'kernel-residual
                        "a random choice whose proposal is not the independence one" dk))
      (choice-support dk 'kernel-residual)))

  ; (new, plan §6.11) the list xs with its i-th element replaced by x.
  (define (list-replace xs i x)
    (if (zero? i) (cons x (cdr xs)) (cons (car xs) (list-replace (cdr xs) (sub1 i) x))))

  )
