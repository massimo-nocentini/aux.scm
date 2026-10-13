;; (aux hakaru10) -- a CHICKEN port of Oleg Kiselyov's Hakaru10, the embedded probabilistic
;; programming language with incremental Metropolis-Hastings (MCMC) inference
;; (https://okmij.org/ftp/kakuritu/Hakaru10/index.html).  This is the public module.
;;
;; Mirrors (paths relative to aux.scm/upstream/okmij-hakaru10/, citation keys as in the docs):
;;   S  Syntax.hs (https://okmij.org/ftp/kakuritu/Hakaru10/Syntax.hs): the user syntax, Model,
;;      SExp and its Functor/Applicative/Num instances, dist, diracN, condition, if_, mcmC;
;;   M  Metropolis.hs (https://okmij.org/ftp/kakuritu/Hakaru10/Metropolis.hs): the driver
;;      (M:856-906) behind mcmC, and the user-level distributions (M:70-95);
;;   D  Distribution.hs, re-exported through bern, bern/flip, categorical, uniformly, uniform,
;;      normal, gamma, beta, poisson (and the extras laplace, dirichlet/symmetric, D:383-411);
;;   U  Util.hs, re-exported: hist, momenta, tabulate, tabulate/bins, kl, plot.
;; The implementation lives in the internal modules (aux hakaru10 random), (aux hakaru10
;; distribution), (aux hakaru10 util), (aux hakaru10 engine) and (aux hakaru10 exact).
;; The examples Examples1.hs (E), ColoredBalls.hs (CB), Birds1.hs (B1) and Bench.hs (BE) are
;; ported as tests under test/hakaru10*.scm.
;;
;; Hakaru10 code version: November 2016 (W:52-53); web page last updated July 14, 2018 (W:179).
;; Paper: O. Kiselyov, "Probabilistic Programming Language and its Incremental Evaluation",
;; APLAS 2016, LNCS 10017, DOI 10.1007/978-3-319-47958-3_19 (P = design.txt / design.pdf).
;;
;; Embedding (user directive of 2026-10-12, PLAN-IDIOM C25-C29).  A model is a thunk, a
;; direct-style Scheme program that performs three operations, `dist` (an observation is the
;; `dist` of a `condition`ed distribution), `diracN` and `if_`; an interpretation is a handler,
;; which the engine's `handle-model` installs under the native `reset` of (chicken
;; continuation).  The same model runs under the MCMC trace builder (`mcmC`, `mcmC-fold`,
;; `mcmC§`, `model-graph`), under exact enumeration into a Hansei search tree
;; (`exact-posterior`, `model->search-tree`) and under a forward sampler (`weighted-samples`,
;; `likelihood-weighting`, and the construction check of `#:check`).  `lift`, `condition` and
;; `memo/finite` are not operations: they are pure.  `mcmC§` is an (aux stream) stream
;; (C23').
;;
;; Rules for models:
;;   - node creation order is load-bearing (topological order, P l.791-794, and the replay of
;;     upstream's runs): create nodes in sequence, with `let*`, `fold`, `map-in-order` or a
;;     named `let`, never in procedure-argument positions or inside `map` (plan C18, C18');
;;   - a model must not mutate, across an operation, state that outlives it (a `set!` of an
;;     outer variable, a table filled between two `dist`s): exact enumeration resumes the model
;;     many times and such state would be shared between its paths (C28);
;;   - the static discipline (external observations, no node of another run, no node escaping
;;     its branch, stable kinds) is checked by the trace builder; `exact-posterior` therefore
;;     builds the model once first (C29).
;; Runtime: the modules run on CHICKEN main (tested at rev 55615603) and on the chicken-core
;; branch delimcc-multishot alike.  One caveat on main up to rev 55615603, where a call/cc
;; escape inside a resumed segment of the native shift/reset returns to the `reset` that the
;; capture was made under instead of to the resumption: a model that sets up `guard`,
;; `handle-exceptions` or `condition-case` around an operation cannot be resumed many times.
;; `exact-posterior`, and the exploration of a tree that `model->search-tree` answered, then
;; raise a `construction` error (the engine's handle-model detects the misrouted escape and
;; raises it in the call that resumed the model, e.g. the probcc-explore that forces the
;; tree) instead of answering a wrong posterior.  The
;; branch delimcc-multishot fixes it (chicken-core f11f304b, b29737e8, 0d398032), and such
;; models enumerate there (case B7 of test/hakaru10.scm runs on both).  The one-shot
;; interpretations (mcmC, mcmC-fold, mcmC§, model-graph, the forward sampler) and models with
;; no handler set up around an operation are unaffected.
;; Users who also need (chicken condition) should import it as
;; (import (except (chicken condition) condition)): Hakaru10's `condition` is exported here
;; (plan C15).
;;
;; Contents: models and construction (model, define-model, dist, diracN, condition, if_, if_*,
;; lift, letlift, pair, collect, constant, sexp?, the sexp aliases, memo/finite); sampling
;; (mcmC, mcmC-fold, mcmC§, model-graph and the `#:check` wiring); and the re-exports of the
;; distributions, the rng sources, the exact oracles and the utilities.

(module (aux hakaru10) (model define-model dist diracN condition if_ if_* lift letlift pair collect
                        constant sexp? sexp-map sexp+ sexp- sexp* sexp/ memo/finite
                        bern bern/flip categorical uniformly uniform normal gamma beta poisson
                        laplace dirichlet/symmetric
                        make-distribution distribution-log-density distribution-sample
                        mcmC mcmC-fold mcmC§ model-graph
                        exact-posterior model->search-tree likelihood-weighting weighted-samples
                        make-stdgen-source make-mrg32k3a-source make-chicken-random-source
                        hist frequency momenta tabulate tabulate/bins kl plot
                        hakaru10-error? hakaru10-error-kind)

  (import scheme
          (only (scheme base) exact-integer?)
          (chicken base)
          (only srfi-1 map-in-order every)
          (only srfi-69 alist->hash-table hash-table-ref)
          (aux base)
          (only (aux stream) τ->§)
          (only (aux hakaru10 random)
                make-stdgen-source make-mrg32k3a-source make-chicken-random-source
                rng-source-copy)
          (only (aux hakaru10 distribution)
                hakaru10-error hakaru10-error? hakaru10-error-kind
                bern bern/flip categorical uniformly uniform normal gamma beta poisson
                laplace dirichlet/symmetric
                make-distribution distribution-log-density distribution-sample)
          (only (aux hakaru10 util) hist frequency momenta tabulate tabulate/bins kl plot)
          (only (aux hakaru10 engine)
                dist diracN if_* lift* constant sexp? condition/external
                build-chain mcmc-generator mcmc-fold extended?
                chain-count chain-fold chain-now chain-card-change chain-extended-ll
                chain-step-hook-set!
                node-kind node-role node-state node-inactive node-inactive? node-delayed
                node-pending)
          (only (aux hakaru10 exact)
                exact-posterior model->search-tree likelihood-weighting weighted-samples
                construction-check reference-check))

  ;; ---------------------------------------------------------------------------------------
  ;; Models and construction (S:82-279)
  ;; ---------------------------------------------------------------------------------------

  ; cf. S:82 `type Model a = MCMCM (SExp a)` (P §4.1 l.737-747): a model is a thunk, and each
  ; run of it under a handler is a fresh run (fresh nodes under the trace builder, P
  ; l.178-179); sharing comes only from binding a name (S:53-73).  Haskell's `do` becomes
  ; `let*` and `return e` the tail expression.
  (define-syntax-rule (model body ...) (τ body ...))

  ; (new) a named model, or a curried procedure answering one: (define-model (name arg ...)
  ; body ...) makes (name arg ...) a model, (define-model name body ...) makes name one.
  (define-syntax define-model
    (syntax-rules ()
      ((_ (name . formals) body ...) (define ((name . formals)) body ...))
      ((_ name body ...) (define (name) body ...))))

  ; cf. S:275-279 `if_ :: SExp Bool -> Model a -> Model a -> Model a`: the branches are
  ; expressions, made into models; if_* is the procedural form, for folds such as CB:85-99.
  (define-syntax-rule (if_ test then else) (if_* test (τ then) (τ else)))

  ; cf. S:174-193 `class Condition` (and M:434-435 `conditioned`): an observed distk from a
  ; resampleable one, or from a constructor, checked when applied; deviation 9 (S:38-42,
  ; S:180-183): the observation must be external to the model, not an SExp.
  (define condition condition/external)

  ; cf. S:99-116 `instance Functor SExp`, `instance Applicative SExp` (`<$>`, `<*>`, liftA2,
  ; ...): (lift f e ...) is (f e ...) evaluated now, in pure context, if no e is an SExp
  ; (`fmap f (Val x) = Val (f x)`, S:100), else one lifted expression over the union of the
  ; arguments' nodes (`fmap f (NT t g) = NT t (f . g)`, S:102).
  (define (lift f . es) (lift* f es))

  ; (new) the binding form of `lift`: (letlift ((x e) ...) body ...) is (lift (λ (x ...)
  ; body ...) e ...), except that the e's are evaluated in order, left to right, so that an e
  ; may create nodes (plan C18); as with `let`, no e sees an x.
  (define-syntax letlift
    (syntax-rules ()
      ((_ "ordered" () ((x t e) ...) body ...)
       (let* ((t e) ...) (lift (λ (x ...) body ...) t ...)))
      ((_ "ordered" ((x e) binding ...) (acc ...) body ...)
       (letlift "ordered" (binding ...) (acc ... (x t e)) body ...))
      ((_ ((x e) ...) body ...)
       (letlift "ordered" ((x e) ...) () body ...))))

  ; cf. S:134-135 `pair = liftA2 (,)`.
  (define (pair a b) (lift cons a b))

  ; cf. B1:113-114 and P l.503-504, `sequenceA` of a list of SExps: one SExp of the list of
  ; their values; (collect '()) is '().
  (define (collect es) (lift* list es))

  ; cf. S:99-116: `fmap`, the Functor name of `lift`.
  (define sexp-map lift)

  ; cf. S:120-132 `instance Num (SExp a)` and `instance Fractional (SExp a)` (`x + y = liftA2
  ; (+) x y`, ...); deviation 9 (S:120-132): any arity, and plain numbers are Vals, replacing
  ; `fromInteger` and `fromRational`.
  (define (sexp+ . es) (lift* + es))
  (define (sexp- . es) (lift* - es))      ; cf. S:123 `x - y`, S:125 `negate`
  (define (sexp* . es) (lift* * es))      ; cf. S:124 `x * y`
  (define (sexp/ . es) (lift* / es))      ; cf. S:130 `(/) = liftA2 (/)`, S:131 `recip`

  ; cf. CB:41-44 `balls_prior` (pre-allocation: `sequence . replicate n $ dist uniformly ...`),
  ; CB:162 (Hansei's `memo`, which Hakaru10 replaces), W:111 and P §3 l.531-544 with footnote 5
  ; (l.566-569): stochastic memoization replaced by pre-allocation.  (f key) runs eagerly for
  ; every key, in the keys' order (map-in-order: C18', and safe under multi-shot handlers,
  ; C28); the answer maps a key to its SExp, an unknown key being a `construction` error.  It
  ; must be called before the if_ that uses it, so that the nodes belong to no branch.  The
  ; table is built after f's last request, so each resumption of a multi-shot handler builds
  ; its own.
  (define (memo/finite keys f)
    (let1 (table (alist->hash-table (map-in-order (λ (key) (cons key (f key))) keys) equal?))
      (λ (key)
        (hash-table-ref table key
                        (τ (hakaru10-error 'construction 'memo/finite "unknown key" key))))))

  ;; ---------------------------------------------------------------------------------------
  ;; `#:check` (plan §6.9, PLAN-IDIOM §2.4): the oracles of (aux hakaru10 exact), wired into
  ;; the chain's build and step hooks.
  ;; ---------------------------------------------------------------------------------------

  ; (new, plan §6.9) what a step may change, taken before it: `now` and `card_change`, the
  ; extended LL (plan §6.8), and the dynamic fields of every node in address order.
  (define-record step-snapshot now card-change ll nodes)

  ; (new, plan §6.9, C5) the snapshot of ch's current state; node states are immutable and a
  ; rollback restores the very objects, so `eq?` compares them.
  (define (take-step-snapshot ch)
    (make-step-snapshot
      (chain-now ch) (chain-card-change ch) (chain-extended-ll ch)
      (chain-fold (λ (n acc)
                    (cons (list (node-state n) (node-inactive n) (node-delayed n) (node-pending n))
                          acc))
                  '() ch)))

  ; (new, plan C5 and §6.9) whether two snapshots record the same state.
  (define (same-state? a b)
    (and (= (step-snapshot-now a) (step-snapshot-now b))
         (= (step-snapshot-card-change a) (step-snapshot-card-change b))
         (every (λ (m n) (every eq? m n)) (step-snapshot-nodes a) (step-snapshot-nodes b))))

  ; (new, plan §6.9) the checks of one step against the snapshot `before`: a rejected step
  ; (M:904-906) must restore the state, `now` included (C5); in 'extended mode (plan §6.8) an
  ; accepted step's Δ must be the change of the extended LL, within 1e-9 relative to it.
  (define (check-step before ch accepted? delta)
    (let1 (after (take-step-snapshot ch))
      (cond
        ((not accepted?)
         (unless (same-state? before after)
           (hakaru10-error 'internal 'check "a rejected step did not restore the state"
                           (step-snapshot-now before))))
        ((extended? ch)
         (let* ((ll0 (step-snapshot-ll before))
                (expected (- (step-snapshot-ll after) ll0)))
           (unless (<= (abs (- delta expected)) (* 1e-9 (max 1. (abs ll0))))
             (hakaru10-error 'internal 'check "the step's delta is not the extended LL change"
                             (chain-now ch) delta expected)))))))

  ; (new, plan §6.9) the build and step hooks of `#:check`, #t (every step) or n (the first n
  ; steps): the reference evaluator on the initial trace and after each checked step, plus
  ; check-step against the snapshot taken before it; the step hook removes itself after the
  ; last checked step, so the rest of the chain runs at full speed.
  (define (check-hooks check)
    (let ((before #f) (steps 0))
      (define (more?) (or (eq? check #t) (< steps check)))
      (define (verify! ch)
        (reference-check ch)
        (if (more?) (set! before (take-step-snapshot ch)) (chain-step-hook-set! ch #f)))
      (values verify!
              (λ (ch accepted? delta)
                (check-step before ch accepted? delta)
                (add1! steps)
                (verify! ch)))))

  ; (new) apply `driver`, mcmc-fold or mcmc-generator curried over everything but the
  ; chain's keys, to them: the rng source (`rng` as is, else `seed`, M:858 `mkStdGen 17`),
  ; the semantics, and, with `check`, the hooks of check-hooks, once construction-check
  ; (PLAN-IDIOM §2.4) has compared the trace builder's first build with the construction-order
  ; forward run on a copy of the source.
  (define (with-chain-keys driver model seed rng semantics check)
    (unless (or (boolean? check) (and (exact-integer? check) (>= check 0)))
      (hakaru10-error 'construction 'mcmC "check: must be a boolean or a step count" check))
    (let1 (src (or rng (make-stdgen-source seed)))
      (if check
          (begin
            (construction-check model #:rng (rng-source-copy src) #:semantics semantics)
            (receive (build-hook step-hook) (check-hooks check)
              (driver #:rng src #:semantics semantics
                      #:build-hook build-hook #:step-hook step-hook)))
          (driver #:rng src #:semantics semantics))))

  ;; ---------------------------------------------------------------------------------------
  ;; Sampling (M:845-906, S:196-197)
  ;; ---------------------------------------------------------------------------------------

  ; cf. M:856-869 `mcmc` and S:196-197 `mcmC limit m = mcmc limit (m >>= to_node)`: fold
  ; (kons sample acc) over the first `limit` samples of the chain, in order, in constant
  ; memory; the first sample is the initial trace's, a rejected step repeats the previous one,
  ; and knil is the answer if no initial trace is found in `limit` builds (M:868, M:882-891).
  ; `rng` (an rng source) overrides `seed`; `semantics` is 'upstream or 'extended (plan §6.8,
  ; deviation 8); `check` is #f, #t or a step count (plan §6.9).
  (define (mcmC-fold kons knil limit model #!key (seed 17) (rng #f) (semantics 'upstream)
                     (check #f))
    (with-chain-keys (λ keys (apply mcmc-fold kons knil limit model keys))
                     model seed rng semantics check))

  ; cf. S:196-197 `mcmC :: Integer -> Model a -> [a]`: the list of the first `limit` samples;
  ; the keys are mcmC-fold's.
  (define (mcmC limit model . keys) (reverse (apply mcmC-fold cons '() limit model keys)))

  ; cf. M:897-906 `transition`, Haskell's lazy list of samples: the infinite (aux stream)
  ; stream of the chain's samples (τ->§ over the engine's generator, C23'), '() if no initial
  ; trace is found in `retries` builds (M:868, M:882-891).  The other keys are mcmC-fold's.
  (define (mcmC§ model #!key (seed 17) (rng #f) (semantics 'upstream) (check #f) (retries 1000))
    (let1 (next (with-chain-keys (λ keys (apply mcmc-generator retries model keys))
                                 model seed rng semantics check))
      (if next (τ->§ next) '())))

  ; cf. M:860-867, the "Trace constructed: ... nodes; ... inactive; ... sampleable nodes" debug
  ; print of `mcmc`, and the trace DAG of P §4.1 (Fig. 1, l.721-736) and P §3 l.493-495 ("511
  ; vertices"): build the model's trace once (no retry) and count its nodes, by kind, the
  ; inactive ones and the branch entries.
  (define (model-graph model #!key (seed 17) (rng #f) (semantics 'upstream))
    (receive (ch result) (build-chain model (or rng (make-stdgen-source seed)) semantics)
      (define (count-nodes node?) (chain-fold (λ (n k) (if (node? n) (add1 k) k)) 0 ch))
      (define ((kind? kind) n) (eq? (node-kind n) kind))
      `((nodes . ,(chain-count ch))
        (resampleable . ,(count-nodes (kind? 'resampleable)))
        (observed . ,(count-nodes (kind? 'observed)))
        (dirac . ,(count-nodes (kind? 'dirac)))
        (inactive . ,(count-nodes node-inactive?))
        (entries . ,(count-nodes (λ (n) (eq? (node-role n) 'entry)))))))

  )
