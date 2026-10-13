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
;; Embedding (plan §15, C32-C38).  A model is a computation of the State monad of (aux
;; category state), threading the chain (`Model a = MCMCM (SExp a)`, S:82; `MCMCM a = State
;; MCState a`, M:200), written with `do/monad`, `←` and `return` as in Syntax.hs.  `dist`,
;; `diracN` and `if_` are Models; `lift`, `pair`, `collect` and `condition` are pure.  Nodes are
;; created when a Model runs, in bind order (C35).  `mcmC`, `mcmC-fold`, `mcmC§` (an (aux
;; stream) stream, C23′) and `model-graph` run it under the MCMC engine; `exact-posterior`,
;; `model->search-tree`, `weighted-samples` and `likelihood-weighting` (new) walk the trace
;; graph of one build (C37).
;;
;; Rules for models:
;;   - a Model may run several times (the retries of the initial trace, the rebuild of
;;     `#:check`, the oracles' graph), so a Scheme effect that must happen once per run goes
;;     inside a bind, after it: the first clause of a top-level `do/monad` is evaluated once,
;;     when the definition is (C35);
;;   - `if_` delays its branches, which are Models built when the if_ runs (C36, deviation
;;     11);
;;   - the static discipline (external observations, no node of another run, no node escaping
;;     its branch, stable kinds) is checked by the build.
;; `<$>` and `<*>` are those of Models (fmap and ap of the State monad: B1:52 `abs <$> dist
;; normal 0 10`).  The SExp functor and applicative are `lift`, `sexp-map` and `pair`,
;; Haskell's same names at type SExp: S:286's `(&&) <$> e <*> d` is (lift (λ (ev dv) (and ev
;; dv)) e d).  MonadState (get, put, modify, ...) is not exported, as Syntax.hs exports none
;; (S:8-14).
;; Users who also need (chicken condition) should import it as
;; (import (except (chicken condition) condition)): Hakaru10's `condition` is exported here
;; (plan C15).
;;
;; Contents: models and construction (dist, diracN, condition, if_, lift, pair, collect, the
;; sexp aliases, and the monad vocabulary of upstream's models); sampling (mcmC, mcmC-fold,
;; mcmC§, model-graph and the `#:check` wiring); and the re-exports of the distributions, the
;; rng sources, the exact oracles and the utilities.

(module (aux hakaru10) (dist diracN condition if_ lift pair collect sexp-map sexp+ sexp- sexp* sexp/
                        return >>= >> ▷ do/monad <$> <*> fold/monad traverse/monad sequence/monad
                        for-each/monad replicate/monad
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
          (only srfi-1 every)
          (aux base)
          (only (aux stream) τ->§)
          (only (aux category state) return >>=)
          (only (aux category monad state) ▷ do/monad >> <$> <*> fold/monad traverse/monad
                sequence/monad for-each/monad replicate/monad)
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
                dist diracN if_* lazy-model model->trace pair collect lift* condition/external
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

  ; cf. S:275-279 `if_ :: SExp Bool -> Model a -> Model a -> Model a`: the branches are
  ; expressions whose values are Models, evaluated when the if_ builds each branch, inside its
  ; scope and, for an inactive one, under lazy lifting: the stand-in for Haskell's non-strict
  ; arguments (C36, deviation 11).  A branch that is not a Model raises `construction` then.
  ; if_ is syntax, so it is used inside a λ where a procedure is needed (CB:85-99's foldr).
  (define-syntax-rule (if_ test then else)
    (if_* test (lazy-model 'if_ (τ then)) (lazy-model 'if_ (τ else))))

  ; cf. S:174-193 `class Condition` (and M:434-435 `conditioned`): an observed distk from a
  ; resampleable one, or from a constructor, checked when applied; deviation 9 (S:38-42,
  ; S:180-183): the observation must be external to the model, neither an SExp nor a Model,
  ; nor a Scheme value that contains one.
  (define condition condition/external)

  ; cf. S:99-116 `instance Functor SExp`, `instance Applicative SExp` (`<$>`, `<*>`, liftA2,
  ; ...): (lift f e ...) is (f e ...) evaluated now if no e is an SExp (`fmap f (Val x) = Val
  ; (f x)`, S:100), else one lifted expression over the union of the arguments' nodes (`fmap
  ; f (NT t g) = NT t (f . g)`, S:102).
  (define (lift f . es) (lift* f es))

  ; cf. S:99-116: `fmap`, the Functor name of `lift`.
  (define sexp-map lift)

  ; cf. S:120-132 `instance Num (SExp a)` and `instance Fractional (SExp a)` (`x + y = liftA2
  ; (+) x y`, ...); deviation 9 (S:120-132): any arity, and plain numbers are Vals, replacing
  ; `fromInteger` and `fromRational`.
  (define (sexp+ . es) (lift* + es))
  (define (sexp- . es) (lift* - es))      ; cf. S:123 `x - y`, S:125 `negate`
  (define (sexp* . es) (lift* * es))      ; cf. S:124 `x * y`
  (define (sexp/ . es) (lift* / es))      ; cf. S:130 `(/) = liftA2 (/)`, S:131 `recip`

  ;; ---------------------------------------------------------------------------------------
  ;; `#:check` (plan §6.9, C37): the oracles of (aux hakaru10 exact), wired into the chain's
  ;; build and step hooks.
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
  ; (plan §6.9) has checked the model's build, on a copy of the source, against a rebuild and
  ; an address-order redraw (C37).
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
    (let1 (prog (model->trace model 'mcmC))
      (with-chain-keys (λ keys (apply mcmc-fold kons knil limit prog keys))
                       model seed rng semantics check)))

  ; cf. S:196-197 `mcmC :: Integer -> Model a -> [a]`: the list of the first `limit` samples;
  ; the keys are mcmC-fold's.
  (define (mcmC limit model . keys) (reverse (apply mcmC-fold cons '() limit model keys)))

  ; cf. M:897-906 `transition`, Haskell's lazy list of samples: the infinite (aux stream)
  ; stream of the chain's samples (τ->§ over the engine's generator, C23′), '() if no initial
  ; trace is found in `retries` builds (M:868, M:882-891).  The other keys are mcmC-fold's.
  (define (mcmC§ model #!key (seed 17) (rng #f) (semantics 'upstream) (check #f) (retries 1000))
    (let* ((prog (model->trace model 'mcmC))
           (next (with-chain-keys (λ keys (apply mcmc-generator retries prog keys))
                                  model seed rng semantics check)))
      (if next (τ->§ next) '())))

  ; cf. M:860-867, the "Trace constructed: ... nodes; ... inactive; ... sampleable nodes" debug
  ; print of `mcmc`, and the trace DAG of P §4.1 (Fig. 1, l.721-736) and P §3 l.493-495 ("511
  ; vertices"): build the model's trace once (no retry) and count its nodes, by kind, the
  ; inactive ones and the branch entries.
  (define (model-graph model #!key (seed 17) (rng #f) (semantics 'upstream))
    (receive (ch result) (build-chain (model->trace model 'model-graph)
                                      (or rng (make-stdgen-source seed)) semantics)
      (define (count-nodes node?) (chain-fold (λ (n k) (if (node? n) (add1 k) k)) 0 ch))
      (define ((kind? kind) n) (eq? (node-kind n) kind))
      `((nodes . ,(chain-count ch))
        (resampleable . ,(count-nodes (kind? 'resampleable)))
        (observed . ,(count-nodes (kind? 'observed)))
        (dirac . ,(count-nodes (kind? 'dirac)))
        (inactive . ,(count-nodes node-inactive?))
        (entries . ,(count-nodes (λ (n) (eq? (node-role n) 'entry)))))))

  )
