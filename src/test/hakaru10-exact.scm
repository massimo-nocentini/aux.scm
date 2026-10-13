; The suites for (aux hakaru10 exact), the exact oracles of (aux hakaru10).
;   - hakaru10-exact: the exact posteriors of the finite models of Examples1.hs and
;     ColoredBalls.hs (E1-E8), a cross-check with Hansei's own programs (E9) and the kernel
;     residual of the 'extended semantics (E10): cases E1-E10 of the plan (WP8, plan §9.5).
;   - hakaru10-handlers: one model, three interpretations (the trace builder of MCMC, exact
;     enumeration, forward sampling), cases H1-H8 of PLAN-IDIOM §5, and H9, an interpretation
;     written here with handle-model.
; Expected values are exact (closed forms, or the 'theoretical' values upstream records) unless
; a case says otherwise; statistical tolerances hold over the seeds 17 and 1-40 (H1).
; Models create their nodes in sequence, with let*, fold, map-in-order, for-each and named let,
; never two in the arguments of one call (plan C18), and are pure across their operations
; (PLAN-IDIOM C28): exact enumeration resumes them once per support point of every choice.

(import scheme
        (chicken base)
        (except (chicken condition) condition)
        srfi-1
        (aux base)
        (aux unittest)
        (only (aux hansei)
              probcc-coin probcc-distribution probcc-when probcc-reify/exact/a
              probcc-τ probcc-unit probcc-explore probcc-normalize)
        (aux hakaru10)
        (only (aux hakaru10 random) rng-double!)
        (only (aux hakaru10 util) histogram-sort)
        (only (aux hakaru10 distribution) distk-kind distk-value distribution-support)
        (only (aux hakaru10 engine) perform handle-model value-handler val-of)
        (only (aux hakaru10 exact) construction-check kernel-residual))

;; -------------------------------------------------------------------------------------------
;; Helpers
;; -------------------------------------------------------------------------------------------

; (new) the sources of the definitions made with define/shown, newest first, for the docs.
(define sources '())

; (new) (define/shown name definition): make the definition, and keep its source under name.
(define-syntax-rule (define/shown name definition)
  (begin definition (push! (cons 'name 'definition) sources)))

; (new) the sources of the definitions of the names, in order, one code/scheme element each
; (spliced with ,@ into a doc).
(define (shown . names) (map (λ (name) `(code/scheme ,(alist-ref name sources))) names))

; (new) the probability of the value v in a histogram ((value . probability) ...).
(define (probability v histogram) (alist-ref v histogram equal? 0.))

; (new) the probability of the values that satisfy pred.
(define (probability-of pred histogram)
  (fold (Λ ((,v . ,p) ,acc) (if (pred v) (+ acc p) acc)) 0. histogram))

; (new) the largest difference between the probabilities two histograms give a value.
(define (histogram-distance h k)
  (fold (λ (v d) (max d (abs (- (probability v h) (probability v k)))))
        0.
        (delete-duplicates (append (map car h) (map car k)))))

; (new) a ⊦ predicate: two histograms whose distance is below tol.
(define ((histograms-within tol) h k) (< (histogram-distance h k) tol))

; (new) the normalized histogram of the first n samples of model's chain (mcmC's keys).
(define (mcmc-posterior n model . keys)
  (map (Λ ((,v . ,k)) (cons v (/ k (exact->inexact n)))) (hist (apply mcmC n model keys))))

; (new) the histogram of a Hansei search tree (((V v) p) ...), normalized and sorted as
; exact-posterior sorts its own.
(define (hansei->histogram tree)
  (histogram-sort (map (Λ (((V ,v) ,p)) (cons v p)) (probcc-normalize tree))))

; (new) the kind of the (exn hakaru10) condition that thunk raises, or 'none.
(define (hakaru10-kind thunk)
  (condition-case (begin (thunk) 'none)
    (e (hakaru10) (hakaru10-error-kind e))))

; (new, PLAN-IDIOM §5 H4) the three interpretations of a model, by name: the trace builder of
; MCMC, exact enumeration (without the validation build, C29, so that the exact handler itself
; meets the model) and the forward sampler of likelihood weighting.
(define interpretations
  `((builder . ,(λ (m) (mcmC 1 m)))
    (exact . ,(λ (m) (exact-posterior m #:validate #f)))
    (forward . ,(λ (m) (likelihood-weighting 1 m)))))

; (new) the kind of the error model raises under each interpretation: ((name . kind) ...).
(define (kinds-under-interpretations model)
  (map (Λ ((,name . ,run)) (cons name (hakaru10-kind (τ (run model))))) interpretations))

;; -------------------------------------------------------------------------------------------
;; Models (Examples1.hs, ColoredBalls.hs, Bench.hs, Birds1.hs; plan §9)
;; -------------------------------------------------------------------------------------------

; cf. E:311-314 `nnot`, the noisy not.
(define/shown nnot (define (nnot p x) (if x p 1.)))

; cf. E:306-309 `nor`, the noisy or: a distribution constructor of two booleans, in Haskell's
; association, (1 - strengthX) first.
(define/shown nor
  (define ((nor strength-x strength-y noise) x y)
    (bern (- 1. (* (nnot (- 1. strength-x) x) (nnot (- 1. strength-y) y) (- 1. noise))))))

; cf. E:299-304 `grass` (P §1 l.32-89, §2.3 l.438-451, W:80-95).
(define/shown grass
  (define-model grass
    (let* ((rain (dist bern .3))
           (sprinkler (dist bern .5)))
      (dist (condition #t (nor .9 .8 .1)) rain sprinkler)
      rain)))

; cf. E:344-352 `girl`, the eldest daughter puzzle, and its `fuzzy2`.
(define/shown girl
  (define-model girl
    (define ((fuzzy2 pfuzz op) x y)
      (let1 (v (op x y)) (categorical `((,v . ,(- 1. pfuzz)) (,(not v) . ,pfuzz)))))
    (let* ((girl1 (dist bern .5))
           (girl2 (dist bern .5)))
      (dist (condition #t (fuzzy2 .01 (λ (x y) (or x y)))) girl1 girl2)
      girl1)))

; cf. E:226-235 `pcorr` (Yufei Cai's example), returning the pair (x . y) instead of y.
(define/shown pcorr/pair
  (define-model (pcorr/pair pfuzz)
    (define (xor y x) (not (eq? x y)))
    (define ((fuzzy2 pfuzz op) x y)
      (let1 (v (op x y)) (categorical `((,v . ,pfuzz) (,(not v) . ,(- 1. pfuzz))))))
    (let* ((x (dist bern .5))
           (y (dist bern .5)))
      (dist (condition #t (fuzzy2 pfuzz xor)) x y)
      (pair x y))))

; cf. E:88-90 `ph2`.
(define/shown ph2
  (define-model ph2
    (let* ((x (dist bern .2)))
      (dist bern (lift (λ (xv) (if xv .1 .7)) x)))))

; cf. E:96-99 `ph21`.
(define/shown ph21
  (define-model ph21
    (let* ((x (dist bern .2)))
      (dist bern (lift (λ (xv) (if xv .1 .7)) x))
      x)))

; cf. E:108 `(==1) <$> x`.
(define/shown one? (define (one? x) (lift (cut = <> 1) x)))

; cf. E:106-110 `pcond1`.
(define/shown pcond1
  (define-model pcond1
    (let* ((x (dist categorical '((1 . .5) (2 . .5)))))
      (if_ (one? x)
           (dist categorical '((10 . .5) (11 . .5)))
           (dist categorical '((20 . .5) (21 . .5)))))))

; cf. E:116-120 `pcond2`.
(define/shown pcond2
  (define-model pcond2
    (let* ((x (dist categorical '((1 . .5) (2 . .5)))))
      (if_ (one? x)
           x
           (dist categorical '((20 . .5) (21 . .5)))))))

; cf. E:126-132 `pcond3`.
(define/shown pcond3
  (define-model pcond3
    (let* ((x (dist categorical '((1 . .5) (2 . .5)))))
      (if_ (one? x)
           x
           (begin
             (dist normal 0. 1.)
             (dist categorical '((20 . .5) (21 . .5))))))))

; cf. E:265-271 `pcond4`.
(define/shown pcond4
  (define-model pcond4
    (let* ((x (dist categorical '((1 . .5) (2 . .5)))))
      (if_ (one? x)
           x
           (begin
             (dist (condition #t bern) .5)
             (dist categorical '((20 . .5) (21 . .5))))))))

; cf. E:256-260 `post_bias`.
(define/shown post-bias
  (define-model (post-bias c)
    (let* ((coin (dist bern .5)))
      (if_ coin
           (dist (condition c normal) 0. 1.)
           (dist (condition c normal) 100. 1.))
      coin)))

; cf. E:140-144 `mixng`, the mixing model of P §2.2 l.331-366.
(define/shown mixng
  (define-model mixng
    (let* ((x (dist normal 0. 1.)))
      (if_ (lift (cut > <> 0.) x)
           (dist normal 10. 2.)
           (dist gamma 3. (/ 1. 3.))))))

; (new, plan §6.8; Design 3 §0.1) the counterexample to upstream's reactivation: P(c) = 1/2,
; the stationary P(c) of upstream's kernel is 0.377286.
(define/shown pswitch
  (define-model pswitch
    (let* ((c (dist bern .5))
           (m (dist uniformly '(0 1))))
      (if_ c
           (dist categorical (lift (λ (mv) `((,mv . .9) (,(- 1 mv) . .1))) m))
           (diracN (lift identity m)))
      c)))

; (new, deviation 8) an inner if_ that tests the same choice as its outer one, with observations
; directly in its branches: when x moves from 1 to 3, the outer then branch activates the
; observation .3 (now pending) and the inner if_ deactivates it in the same step, which must
; subtract nothing from the log-likelihood.  P(x) = (.6 .3 .1) for x = 1, 2, 3.
(define/shown nested-dispatch
  (define-model nested-dispatch
    (let* ((x (dist uniformly '(1 2 3))))
      (if_ (lift (cut > <> 1) x)
           (if_ (lift (cut > <> 2) x)
                (dist (condition #t bern) .1)
                (dist (condition #t bern) .3))
           (dist (condition #t bern) .6))
      x)))

; (new) a coin whose declared support (.5/.5) disagrees with its density (.9/.1), so the forced
; independence proposal is not the one the acceptance ratio assumes: the positive control of
; E10, a model whose 'extended kernel does not leave its target invariant.
(define/shown liar
  (define (liar)
    (make-distribution 'liar
                       log-density: (λ (x) (log (if x .9 .1)))
                       sample: (λ (src) (let1 (x (< (rng-double! src 0. 1.) .9))
                                          (values x (log (if x .9 .1)))))
                       support: (τ '((#t . .5) (#f . .5))))))

; (new) the liar and a .3 coin, under a noisy observation of their disjunction.
(define/shown liar-or
  (define-model liar-or
    (let* ((a (dist liar))
           (b (dist bern .3)))
      (dist (condition #t bern) (lift (λ (av bv) (if (or av bv) .8 .2)) a b))
      a)))

; cf. CB:30-32 `opposite_color`.
(define/shown opposite-color
  (define (opposite-color color) (if (eq? color 'blue) 'green 'blue)))

; cf. CB:34-36 `observed_color`: an observed color is wrong with probability .2.
(define/shown observed-color
  (define (observed-color color)
    (categorical `((,color . .8) (,(opposite-color color) . .2)))))

; cf. CB:41-44 `balls_prior`, with the stochastic memoization of the balls' colors replaced by
; their pre-allocation (memo/finite; footnote 5, P l.566-569).
(define/shown balls-prior
  (define (balls-prior max-balls)
    (memo/finite (iota max-balls 1) (λ (i) (dist uniformly '(blue green))))))

; cf. CB:74-89 `balls_observe`: for each observation, in order (mapM_), a ball b is drawn among
; nballs and the observation conditioned on its color, through the chain of if_'s that
; `foldr observe (return (pure color)) [1..maxBalls]` builds.
(define/shown balls-observe
  (define (balls-observe max-balls balls obs nballs)
    (for-each (λ (color)
                (let* ((b (dist uniformly (iota nballs 1))))
                  (let observe ((i 1))
                    (if (> i max-balls)
                        color
                        (if_ (lift (cut = <> i) b)
                             (dist (condition color observed-color) (balls i))
                             (observe (add1 i)))))))
              obs)
    (void)))

; cf. CB:91-99 `cballs_model`, with maxBalls a parameter (CB:38-39 fixes it at 8).
(define/shown cballs-model
  (define-model (cballs-model max-balls obs)
    (let* ((balls (balls-prior max-balls))
           (nballs (dist uniformly (iota max-balls 1))))
      (let obs-number ((i 1))
        (if (> i max-balls)
            (void)
            (if_ (lift (cut = <> i) nballs)
                 (balls-observe max-balls balls obs i)
                 (obs-number (add1 i)))))
      nballs)))

; (new) the posterior of the number of balls in closed form: given m balls, k of them blue, a
; draw is seen blue with probability p_k = (.8k + .2(m - k))/m, so P(m | n blues) is
; proportional to sum_k C(m,k) 2^-m p_k^n, m = 1..max-balls under the uniform prior.
(define/shown cballs-closed-form
  (define (cballs-closed-form max-balls n)
    (define (binomial m k) (if (zero? k) 1 (/ (* m (binomial (sub1 m) (sub1 k))) k)))
    (define (likelihood m)
      (fold (λ (k acc)
              (+ acc (* (binomial m k) (expt 2. (- m))
                        (expt (/ (+ (* .8 k) (* .2 (- m k))) m) n))))
            0.
            (iota (add1 m))))
    (let* ((ls (map likelihood (iota max-balls 1)))
           (z (fold + 0. ls)))
      (map (cut / <> z) ls))))

; cf. BE:14-15 `phier n = (iterate (\m -> do {x<-m; dist normal x 3}) $ dist normal 0 1) !! n`.
(define/shown phier
  (define-model (phier n)
    (fold (λ (_ x) (dist normal x 3.)) (dist normal 0. 1.) (iota n))))

; cf. B1:103-107 `normals`: independent normals of the given means, created in order (mapM).
(define/shown normals
  (define (normals means sd) (map-in-order (cut dist normal <> sd) means)))

; cf. B1:110-111 `dot xs ays = foldr1 (+) $ zipWith (\x ay -> (*x) <$> ay) xs ays`.
(define/shown dot
  (define (dot xs ws) (reduce-right sexp+ 0. (map (λ (x w) (lift (cut * <> x) w)) xs ws))))

; cf. P §3 l.461-504, Bayesian linear regression (PPAML problem 4.1): data is a list of
; (xs . y), xs of length k.
(define/shown regression
  (define-model (regression k data)
    (let* ((w-mean (normals (make-list k 0.) 2.))
           (w (normals w-mean 1.))
           (noise-sd (lift (cut / 1. <>) (dist gamma .5 .5))))
      (for-each (Λ ((,xs . ,y)) (dist (condition y normal) (dot xs w) noise-sd)) data)
      (collect w))))

; (new) n synthetic regression points in k dimensions, deterministic: x_ij = sin(1.3i + .7j),
; y_i = x_i . w + .1cos(i), w = (1 -2 .5 3 -1 ...).
(define (regression-data k n)
  (let1 (w (list-tabulate k (λ (j) (list-ref '(1. -2. .5 3. -1.) (modulo j 5)))))
    (map (λ (i)
           (let1 (xs (map (λ (j) (sin (+ (* 1.3 i) (* .7 j)))) (iota k)))
             (cons xs (+ (fold + 0. (map * xs w)) (* .1 (cos i))))))
         (iota n))))

; cf. the `nested` model of the WP4b harness: two levels of if_, branch-local choices whose
; parameters depend on outer nodes, observations in both levels, diracN of a lifted expression.
(define/shown nested
  (define-model nested
    (let* ((c (dist bern .5))
           (d (dist bern .4))
           (z (dist normal 0. 1.)))
      (if_ c
           (let* ((e (dist bern (lift (λ (zv) (if (> zv 0.) .7 .2)) z))))
             (if_ (lift (λ (ev dv) (and ev dv)) e d)
                  (dist (condition .5 normal) z 1.)
                  (begin
                    (dist (condition #t bern) .3)
                    (diracN (lift (cut * 2. <>) z)))))
           (if_ d (dist normal z 2.) (dist gamma 2. 1.))))))

; (new, PLAN-IDIOM C29) a uniform choice over a list of lists, then one over the list drawn:
; when that is '(), the then branch is inactive and draws from `uniformly '()`, an error
; (D:76) that the validation build must not take for a breach of the discipline.
(define/shown nested-uniformly
  (define-model (nested-uniformly choices)
    (let* ((xs (dist uniformly choices)))
      (if_ (lift pair? xs) (dist uniformly xs) 'none))))

; (new, PLAN-IDIOM C29) nested-uniformly followed by an observation of a choice, ill-typed
; upstream (E:196-201): the validation build must find it whatever seed 17 draws first.
(define/shown nested-uniformly/ill-typed
  (define-model (nested-uniformly/ill-typed choices)
    (let* ((xs (dist uniformly choices)))
      (if_ (lift pair? xs) (dist uniformly xs) 'none)
      (let* ((y (dist bern .5)))
        (dist (condition y bern) .5)
        xs))))

; (new, PLAN-IDIOM §5 H6) a finite model with nested if_'s, diracN of a node of the enclosing
; scope and of a lifted expression, and an observation whose parameter depends on an outer
; node; its branch-local choices have constant parameters, so 'upstream is unbiased on it.
(define/shown two-levels
  (define-model two-levels
    (let* ((c (dist bern .4))
           (d (dist uniformly '(0 1 2))))
      (if_ c
           (let* ((e (dist bern .7)))
             (if_ e
                  (diracN d)
                  (diracN (lift (cut + 10 <>) d))))
           (let* ((y (dist categorical '((5 . .5) (6 . .5)))))
             (dist (condition #t bern) (lift (λ (yv dv) (if (= yv 5) .8 (/ (+ dv 1.) 4.))) y d))
             y)))))

; (new, PLAN-IDIOM C28) a model that breaks multi-shot purity: it mutates `seen`, which outlives
; its operations, so the paths of exact enumeration share it.
(define/shown impure
  (define-model impure
    (let1 (seen '())
      (let* ((a (dist bern .5)))
        (push! a seen)
        (let* ((b (dist bern .5)))
          (push! b seen)
          (length seen))))))

; (new, PLAN-IDIOM §2.3, H9) a user-defined interpretation: exact enumeration of the prior,
; which answers an observation with its value without weighing it.
(define/shown prior-handler
  (define prior-handler
    (value-handler
      (λ (dk resume)
        (if (eq? (distk-kind dk) 'observed)
            (resume (distk-value dk))
            (filter-map (Λ ((,x . ,p)) (and (positive? p) (probcc-τ p (resume x))))
                        (distribution-support dk)))))))

; (new, H9) the prior of a finite model, as exact-posterior answers a posterior.
(define/shown prior
  (define (prior model)
    (hansei->histogram
      (probcc-explore +inf.0 (handle-model model prior-handler (o probcc-unit val-of))))))

;; -------------------------------------------------------------------------------------------
;; Hansei's own programs (E9), in (aux hansei) over aux's delimcc: they never run inside a
;; hakaru10 handler.
;; -------------------------------------------------------------------------------------------

; cf. test/hansei.scm:87-100 (Kiselyov and Shan, "Embedded probabilistic programming", DSL
; 2009, §2): the grass model with the noisy-or spelled out as coin flips.
(define/shown hansei-grass
  (define (hansei-grass)
    (let* ((rain (probcc-coin .3))
           (sprinkler (probcc-coin .5))
           (grass-is-wet (or (and (probcc-coin .9) rain)
                             (and (probcc-coin .8) sprinkler)
                             (probcc-coin .1))))
      (probcc-when grass-is-wet rain))))

; cf. E:344-352 `girl` in Hansei: the soft observation of the fuzzy or is a coin of its weight.
(define/shown hansei-girl
  (define (hansei-girl)
    (let* ((girl1 (probcc-coin .5))
           (girl2 (probcc-coin .5)))
      (probcc-when (probcc-coin (if (or girl1 girl2) .99 .01)) girl1))))

; cf. E:265-282 `pcond4` in Hansei, whose exact answer upstream records at E:280-282.
(define/shown hansei-pcond4
  (define (hansei-pcond4)
    (let1 (x (probcc-distribution '((1 .5) (2 .5))))
      (if (= x 1)
          x
          (probcc-when (probcc-coin .5) (probcc-distribution '((20 .5) (21 .5))))))))

;; -------------------------------------------------------------------------------------------
;; Suite hakaru10-exact (plan §9.5, E1-E10)
;; -------------------------------------------------------------------------------------------

(define-suite hakaru10-exact

  ((doc r) `((structure/section "Exact oracles")
             (p "Hakaru10 infers by Markov chain Monte Carlo, so its answers are estimates.  To
                test them we need exact answers, and an oracle that does not share code with
                the engine it checks.  This page is about the oracles of "
                (code/inline "(aux hakaru10 exact)") ": exact enumeration, a reference
                evaluator, and the exact kernel of the 'extended semantics.  None of them is in
                upstream: they are new in the port.")
             (p "A model is a direct-style Scheme program that " (i "performs") " three
                operations: " (code/inline "dist") " (an observation is the "
                (code/inline "dist") " of a " (code/inline "condition") "ed distribution), "
                (code/inline "diracN") " and " (code/inline "if_") ".  What they mean is up to
                the " (i "handler") " that runs the model, which "
                (code/inline "handle-model") " installs under the native "
                (code/inline "reset") " of " (code/inline "(chicken continuation)")
                ": the handler receives the request and the model's continuation up to that "
                (code/inline "reset") ", and may resume it once, many times or never.  The
                trace builder of " (code/inline "mcmC") " resumes it once per operation and
                builds upstream's trace graph.  Exact enumeration resumes it once per point of
                the finite support of every random choice, and so builds a lazy search tree in
                the format of "
                (cite/a "https://okmij.org/ftp/kakuritu/Hansei.html"
                        "Embedded probabilistic domain-specific language HANSEI")
                ":")
             (code/lang "ocaml" "type 'a vc = V of 'a | C of (unit -> 'a pV) and 'a pV = (prob * 'a vc) list")
             (p "in Scheme the list " (code/inline "(((V v) p) ...)") " and "
                (code/inline "(((C thunk) p) ...)") ".  A choice becomes one "
                (code/inline "C") " entry per support point " (code/inline "(x . p)") " with "
                (code/inline "p > 0") ", whose thunk resumes the model with "
                (code/inline "x") ".  An observation becomes one entry weighted by its
                likelihood, or no entry at all when that likelihood is 0.  An "
                (code/inline "if_") " runs only the branch its test selects.  "
                (code/inline "probcc-explore") " and " (code/inline "probcc-normalize")
                " of " (code/inline "(aux hansei)") " then explore and normalize the tree.  Only
                Hansei's data-level tools are used, so the native " (code/inline "shift")
                "/" (code/inline "reset") " of the model and the delimited control of aux's "
                (code/inline "resetcc") " never meet in one control context.")
             (p "Under exact enumeration an SExp is a plain value, so "
                (code/inline "lift") " evaluates at once and no node exists.  The oracle
                therefore does not reuse the engine's construction or propagation code.  The
                same plain values hide the static discipline, such as observations external to
                the model or no node escaping its branch, so " (code/inline "exact-posterior")
                " first builds the model once with the trace builder (" (code/inline "#:validate #t")
                ", the default).  Continuous observations weigh by their density, which is right
                for posteriors over discrete choices.  A continuous " (i "choice")
                " on a path the enumeration takes is a " (code/inline "support") " error.")
             (p "Exact enumeration resumes a continuation many times, so a model must not
                mutate, across an operation, state that outlives the operation: sibling paths
                would share it (H8).  The " (code/inline "memo/finite") " table, folds,
                " (code/inline "map-in-order") " and named " (code/inline "let")
                " are all safe.")
             (p "The " (b "kernel residual") " (E10) checks the MH kernel itself, not one of
                its chains.  In 'extended mode the inactive resampleable nodes are auxiliary
                variables with their prior.  Over every assignment " (m "x")
                " of all resampleable nodes, inactive ones included, the target is "
                (m "π(x) ∝ exp L(x)") ", where " (m "L")
                " sums the LLs of every resampleable node and of every active observed node.
                The one-step kernel " (m "T") " is computed exactly from the engine's own
                propagation and acceptance code: each active resampleable node is picked with
                probability " (m "1/N(x)") ", each support point " (m "v")
                " of its distribution is proposed with its probability " (m "q")
                ", and the forced step is accepted with probability "
                (m "min(1, eᵅ)") " and then rolled back.  The residual is "
                (m "maxₓ |(πT)(x) − π(x)|") ", which is below "
                (m "10⁻¹²") " when " (m "π") " is stationary for " (m "T")
                ".  This is upstream's merged-node argument made exact (Metropolis.hs:1145-1168):")
             (code/lang "haskell" "So, when it comes to computing the acceptance ratio for a program
with branches, we take

p4 = do
     c <- bernN (Val 0.5)
     if_ c (do e1 e2 ...)
           (do e3 e4 e5 ...)

to be

p4' = do
      c <- bernN (Val 0.5)
      if_ c e_then e_else

That is, we merge (at least for the purpose of presentation) e1, e2 ...
into one node e_then, whose ll is the sum of ll of constituents. Ditto
for e_else.

When we switch the branches, we remove the ll of one branch from
llcum and add ll of the other branch. In our terms, we remove the ll
of the deactivated nodes and add ll of the activated nodes.

This algorithm is resilient with respect to adding replacing a node e
with e >>= diracN -- the desired behavior.")
             (p "In 'upstream mode the argument does not hold for resampleable nodes whose
                parameters change while they are inactive, as " (code/inline "pswitch")
                " shows (E8; the main suite's hakaru10-semantics).  'extended mode is the fix,
                and E10 shows that it leaves the target invariant.")
             (code/scheme/file "../aux.hakaru10.exact.scm")))

  ((test/grass _)
   (⊦ (equal-approx? 1e-12) '((#f . 0.5315285572796303) (#t . 0.4684714427203697))
     (exact-posterior grass))
   `(doc (p "E1.  The grass model: rain and a sprinkler may wet the grass, and the grass is
            observed wet.  The exact posterior of rain is upstream's 'theoretical' value.")
         ,@(shown 'nnot 'nor 'grass)
         (code/lang "haskell" "grass = do
  rain        <- dist bern 0.3
  sprinkler   <- dist bern 0.5
  --  dist (nor 0.9 0.8 0.1) rain sprinkler
  grass_is_wet <- dist (True `condition` nor 0.9 0.8 0.1) rain sprinkler
  return rain

-- noisy-or function
nor :: Double -> Double -> Double -> Bool -> Bool -> Distribution Bool
nor strengthX strengthY noise x y =
  bern $ 1 - nnot (1-strengthX) x * nnot (1-strengthY) y * (1-noise)

-- noisy not function
nnot :: Num a => a -> Bool -> a
nnot p True  = p
nnot p False = 1")
         (p "cf. Examples1.hs:299-314; the expected value is the comment at
            Examples1.hs:321-322:")
         (code/lang "haskell" "-- 0.46815 for 20000 -- with old resampling
-- 0.4684714427203697  -- theoretical")))

  ((test/girl _)
   (⊦ (equal-approx? 1e-12) '((#f . 0.33557046979865773) (#t . 0.6644295302013423))
     (exact-posterior girl))
   `(doc (p "E2.  The eldest daughter puzzle: a family has two kids, one of them a girl
            (observed through a fuzzy or that errs with probability .01); what is the chance
            the older is a girl?  Exactly " (m "0.495/0.745 = 0.6644295302013423") ".")
         ,@(shown 'girl)
         (code/lang "haskell" "girl = do
  girl1 <- dist bern 0.5
  girl2 <- dist bern 0.5
  dist (True `condition` fuzzy2 0.01 (||)) girl1 girl2
  return girl1
 where
   -- Creating a custom distribution
   fuzzy2 pfuzz op x y = let v = op x y in
     categorical [(v,1-pfuzz),(not v,pfuzz)]")
         (p "cf. Examples1.hs:340-355 (upstream records 77 " (code/inline "True")
            " in 100 samples).")))

  ((test/pcorr _)
   (let1 (posterior (exact-posterior (pcorr/pair .1)))
     (⊦ (equal-approx? 1e-12) .9 (probability-of (Λ ((,x . ,y)) (eq? x y)) posterior))
     (⊦ (equal-approx? 1e-12) .5 (probability-of cdr posterior))
     (⊦ (equal-approx? 1e-12) '(((#f . #f) . .45) ((#f . #t) . .05) ((#t . #f) . .05)
                                ((#t . #t) . .45))
       posterior))
   `(doc (p "E3.  Yufei Cai's example of correlated booleans: " (code/inline "x")
            " and " (code/inline "y") " agree with probability .9 a posteriori, and "
            (code/inline "y") " alone is a fair coin.  The model returns the pair, where
            upstream returns " (code/inline "y") ".")
         ,@(shown 'pcorr/pair)
         (code/lang "haskell" "pcorr pfuzz = do
  x <- dist bern 0.5
  y <- dist bern 0.5
  dist (True `condition` fuzzy2 pfuzz xor) x y
  return y
 where
   -- Creating a custom distribution
   fuzzy2 pfuzz op x y = let v = op x y in
     categorical [(v,pfuzz),(not v,1-pfuzz)]
   xor y x = not (x == y)")
         (p "cf. Examples1.hs:223-238.")))

  ((test/ph2 _)
   (⊦ (equal-approx? 1e-12) .58 (probability #t (exact-posterior ph2)))
   (⊦ (equal-approx? 1e-12) .2 (probability #t (exact-posterior ph21)))
   `(doc (p "E4.  A dependent coin: " (m "0.2 · 0.1 + 0.8 · 0.7 = 0.58")
            ", and its parent, with no observation, keeps its prior .2.")
         ,@(shown 'ph2 'ph21)
         (code/lang "haskell" "ph2 = do
  x <- dist bern 0.2
  dist bern ((\\xv -> if xv then 0.1 else 0.7) <$> x)
-- 0.2 * 0.1 + 0.8*0.7 = 0.58

ph21 = do
  x <- dist bern 0.2
  y <- dist bern ((\\xv -> if xv then 0.1 else 0.7) <$> x)
  return x")
         (p "cf. Examples1.hs:88-102 (upstream records 5809 and 2026 in 10000 samples).")))

  ((test/pcond _)
   (⊦ (equal-approx? 1e-12) '((10 . .25) (11 . .25) (20 . .25) (21 . .25))
     (exact-posterior pcond1))
   (⊦ (equal-approx? 1e-12) '((1 . .5) (20 . .25) (21 . .25)) (exact-posterior pcond2))
   (⊦ (equal-approx? 1e-12) `((1 . ,(/ 2. 3.)) (20 . ,(/ 1. 6.)) (21 . ,(/ 1. 6.)))
     (exact-posterior pcond4))
   `(doc (p "E5.  Mixtures by " (code/inline "if_") ": in " (code/inline "pcond2")
            " a branch returns the test's own node, and in " (code/inline "pcond4")
            " the else branch observes a fair coin " (code/inline "True")
            ", halving its weight.")
         ,@(shown 'one? 'pcond1 'pcond2 'pcond4)
         (code/lang "haskell" "pcond1 = do
  x <- dist categorical (pure [(1,0.5), (2,0.5)])
  if_ ((==1) <$> x)
     (dist categorical (pure [(10,0.5), (11,0.5)]))
     (dist categorical (pure [(20,0.5), (21,0.5)]))

pcond2 = do
  x <- dist categorical (pure [(1,0.5), (2,0.5)])
  if_ ((==1) <$> x)
     (return x)
     (dist categorical (pure [(20,0.5), (21,0.5)]))

pcond4 = do
  x <- dist categorical (pure [(1,0.5), (2,0.5)])
  if_ ((==1) <$> x)
     (return x)
     (do
       dist (True `condition` bern) 0.5
       dist categorical (pure [(20,0.5), (21,0.5)]))")
         (p "cf. Examples1.hs:104-124 and 265-282, where upstream records the exact answer
            of " (code/inline "pcond4") ":")
         (code/lang "haskell" "-- Exact:
--      [(0.166666666666666657, V 21); (0.166666666666666657, V 20);
--       (0.66666666666666663, V 1)])")))

  ((test/pcond3-support _)
   (⊦= 'support (hakaru10-kind (τ (exact-posterior pcond3))))
   (⊦= 'none (hakaru10-kind (τ (mcmC 10 pcond3))))
   `(doc (p "E5b.  " (code/inline "pcond3") " draws a normal in its else branch, so exact
            enumeration raises a " (code/inline "support") " error when it takes that
            branch.  The trace builder builds the model without complaint, so the error comes
            from the exact handler, not from the validation build.")
         ,@(shown 'pcond3)
         (code/lang "haskell" "pcond3 = do
  x <- dist categorical (pure [(1,0.5), (2,0.5)])
  if_ ((==1) <$> x)
     (return x)
     (do
       dist normal 0 1
       dist categorical (pure [(20,0.5), (21,0.5)]))")
         (p "cf. Examples1.hs:126-136.  Likelihood weighting gives its answer (H7).")))

  ((test/post-bias _)
   (⊦ equal? '((#t . 1.)) (exact-posterior (post-bias 1.)))
   (⊦ equal? '() (exact-posterior (model (dist (condition 5. uniform) 0. 1.))))
   `(doc (p "E6.  A coin selects which of two normals, centered at 0 and 100, the observation
            1 comes from.  The likelihood of the observation under N(100, 1) is "
            (m "e⁻⁴⁹⁰⁰·⁵/√(2π)") ", which underflows to 0, so the posterior of the
            coin is " (code/inline "#t") ", with probability 1.: an observation of weight 0
            prunes its path from the search tree, so " (code/inline "#f")
            " is not in the histogram at all.  A model whose every path has weight 0, here 5
            observed from the uniform on [0, 1], has the empty posterior "
            (code/inline "'()") ".")
         ,@(shown 'post-bias)
         (code/lang "haskell" "post_bias c = do
  coin <- dist bern 0.5
  if_ coin (dist (c `condition` normal) 0 1)
           (dist (c `condition` normal) 100 1)
  return coin

_ = mcmC 10 (post_bias 1)
-- All true")
         (p "cf. Examples1.hs:256-263; P l.427-437.")))

  ((test/colored-balls _)
   (let1 (expected '(.45765164162493044 .29838619922092374 .24396215915414585))
     (⊦ (equal-approx? 1e-12) (map cons '(1 2 3) expected)
       (exact-posterior (cballs-model 3 (make-list 4 'blue))))
     (⊦ (equal-approx? 1e-12) expected (cballs-closed-form 3 4)))
   (⊦ (equal-approx? 1e-12)
     '(.4119636809525704 .2097286247545656 .12069197667405333 .08018476851324365
       .05903181664195371 .04660379073990803 .038629862100510455 .03316547962319476)
     (cballs-closed-form 8 10))
   `(doc (p "E7.  The colored balls of BLOG (Milch et al., "
            (cite/a "http://people.csail.mit.edu/milch/papers/blog-chapter.pdf"
                    "BLOG: Probabilistic Models with Unknown Objects")
            ", example 1.1), quoted at ColoredBalls.hs:6-11:")
         (cite/quote "Brian Milch et al."
                     "An urn contains an unknown number of balls--say, a number chosen from
                     a Poisson or a uniform distributions. Balls are equally likely to be blue
                     or green. We draw some balls from the urn, observing the color of each
                     and replacing it. We cannot tell two identically colored balls apart;
                     furthermore, observed colors are wrong with probability 0.2.  How many
                     balls are in the urn? Was the same ball drawn twice?")
         (p "With at most 3 balls and 4 draws, all seen blue, the exact posterior of the
            number of balls agrees with its closed form: given " (m "m")
            " balls of which " (m "k") " are blue, a draw is seen blue with probability "
            (m "pₖ = (0.8k + 0.2(m − k))/m") ", so "
            (m "P(m | n blues) ∝ Σₖ C(m, k) 2⁻ᵐ pₖⁿ")
            ".  The 8-ball, 10-draw model of upstream has a trace of 2114 nodes and is out of
            reach of enumeration; its closed form is")
         (code/scheme ,(map cons (iota 8 1) (cballs-closed-form 8 10)))
         (p "next to Hansei's importance sampling, " (code/inline "t1")
            " and the average " (code/inline "t2") " (ColoredBalls.hs:110-147):")
         (code/lang "ocaml" "let
    [(0.031942788306915014, V 8); (0.0422088248548995876, V 7);
     (0.0481489259210673634, V 6); (0.0611501366775565255, V 5);
     (0.0843776503618006435, V 4); (0.123159075103266638, V 3);
     (0.203898291338526577, V 2); (0.405114307435967658, V 1)] =
 snd (Inference.normalize t1);;

let
  [(0.0318386565368576138, V 8); (0.0384083436381660176, V 7);
   (0.0445344709122082669, V 6); (0.055953476873295431, V 5);
   (0.0777907061915841, V 4); (0.123751456856916756, V 3);
   (0.211354336837841883, V 2); (0.416368552153129956, V 1)]
 = t2;;")
         (p "The model, with the number of balls a parameter and memoization replaced by
            pre-allocation (" (code/inline "memo/finite") "):")
         ,@(shown 'opposite-color 'observed-color 'balls-prior 'balls-observe 'cballs-model
                 'cballs-closed-form)
         (code/lang "haskell" "-- Distribution for the observing of a ball, with the failure rate 20%
observed_color color =
  categorical [(color,0.8),(opposite_color color,0.2)]

maxBalls :: Int
maxBalls = 8

-- Create the prior for the max amount of balls that may be in the urn
balls_prior n = do
  balls <- sequence . replicate n $ dist uniformly (pure [Blue,Green])
  return $ M.fromList $ zip [1..] balls

-- Submodel: Given balls color and the observations, perform conditioning
balls_observe :: M.IntMap (SExp Color) -> [Color] -> Int -> Model ()
balls_observe balls obs nballs = mapM_ observe_ball obs >> return (pure ())
 where
  -- The same ball can be drawn twice as we return the drawn balls
  -- into the urn. A ball keeps its true color, no matter how many
  -- times it is drawn.
   observe_ball color = do
        b <- dist uniformly (pure [1..nballs])
        -- It is more efficient to create if statements, rather than
        -- create an SExp(IntMap Color).
        let observe i =
              let true_color = balls ! i in
              if_ ((== i) <$> b) $
                dist (color `condition` observed_color) true_color
        foldr observe (return (pure color)) [1..maxBalls]

-- The model takes a list of observations as an argument
cballs_model :: [Color] -> Model Int
cballs_model obs = do
  balls  <- balls_prior maxBalls
  nballs <- dist uniformly (pure [1..maxBalls])
  let obs_number i =
        if_ ((== i) <$> nballs) $ balls_observe balls obs i
  foldr obs_number (return (pure())) [1..maxBalls]
  return nballs")
         (p "cf. ColoredBalls.hs:34-44, 74-99, without the first model that lines 46-72
            comment out; W " (code/inline "#colored-balls") " (l.110-119).")))

  ((test/pswitch _)
   (⊦ (equal-approx? 1e-12) '((#f . .5) (#t . .5)) (exact-posterior pswitch))
   `(doc (p "E8.  " (code/inline "pswitch") " is the counterexample to upstream's
            reactivation of branches (plan §6.8).  The branch-local "
            (code/inline "y") " depends on " (code/inline "m") ", which keeps changing while "
            (code/inline "y") " is inactive.  The answer is " (m "P(c) = 1/2")
            ", but the stationary distribution of upstream's kernel gives "
            (m "P(c) = 0.377286") ".  Upstream's argument is at Metropolis.hs:1145-1168.")
         ,@(shown 'pswitch)
         (code/lang "haskell" "-- (new, plan §6.8; no upstream source)
c ~ bern 0.5
m ~ uniformly [0,1]
if_ c (y ~ categorical [(m,.9),(1-m,.1)])
      (diracN (id <$> m))
return c")))

  ((test/hansei _)
   (⊦ (equal-approx? 1e-12) (exact-posterior grass)
     (hansei->histogram (probcc-reify/exact/a hansei-grass)))
   (⊦ (equal-approx? 1e-12) (exact-posterior girl)
     (hansei->histogram (probcc-reify/exact/a hansei-girl)))
   (⊦ (equal-approx? 1e-12) (exact-posterior pcond4)
     (hansei->histogram (probcc-reify/exact/a hansei-pcond4)))
   `(doc (p "E9.  The same posteriors from programs written in Hansei itself ("
            (code/inline "(aux hansei)") ", which runs on aux's own delimited control, "
            (code/inline "resetcc") ").  Hansei's grass is its canonical formulation, with the
            noisy-or spelled out as coin flips (test/hansei.scm:87-100).  The soft observation
            of " (code/inline "girl") " is a coin of the observation's likelihood, and "
            (code/inline "pcond4") " is a transcription.")
         ,@(shown 'hansei-grass 'hansei-girl 'hansei-pcond4)
         (p "Hansei is described in "
            (cite/a "https://okmij.org/ftp/kakuritu/Hansei.html"
                    "Embedded probabilistic domain-specific language HANSEI")
            " and in Kiselyov and Shan, " (i "Embedded Probabilistic Programming")
            ", DSL 2009, LNCS 5658; W:111 cites Hansei's implementation of the colored balls.")))

  ((test/kernel-residual _)
   (define models
     `((grass . ,grass) (girl . ,girl) (pcorr . ,(pcorr/pair .1)) (pcond1 . ,pcond1)
       (pcond2 . ,pcond2) (pcond4 . ,pcond4) (post-bias . ,(post-bias 1.))
       (colored-balls-3x2 . ,(cballs-model 3 '(blue blue))) (pswitch . ,pswitch)
       (nested-dispatch . ,nested-dispatch)))
   (⊦ equal? '()
     (filter-map (Λ ((,name . ,model))
                   (let1 (residual (kernel-residual model))
                     (and (not (< residual 1e-12)) (cons name residual))))
                 models))
   (⊦ = 0. (kernel-residual (model (if_ #f (dist bern .5) 0))))
   (⊦ < 1e-3 (kernel-residual liar-or))
   (⊦ < 1e-2 (kernel-residual pswitch #:semantics 'upstream))
   (⊦ > 1e-12 (kernel-residual pswitch #:semantics 'extended))
   `(doc (p "E10.  The 'extended semantics leaves its target invariant: the kernel residual "
            (m "maxₓ |(πT)(x) − π(x)|") " is below " (m "10⁻¹²")
            " on the finite models below.  The colored balls are cut to at most 3 balls and 2
            draws, whose extended space has 864 states.")
         (p (code/inline "nested-dispatch") " reaches the deactivation of an observation that
            the same step activated (deviation 8): subtracting its stale LL there would bias
            the kernel, with a residual of about .078.")
         ,@(shown 'nested-dispatch)
         (p "hakaru10.scm runs the kernel residual on four more models, next to their other
            checks: " (code/inline "act-update") " and " (code/inline "act-update/finite")
            " (case B4), whose step activates an observation and updates its parameters at
            once, and " (code/inline "guarded-car") " and " (code/inline "nested-guarded-car")
            " (case B6), whose inactive branches hold a deferred Dirac value.")
         (p "A state with no active resampleable node stays put (deviation 6): the residual
            is 0 on a model whose only choice sits in a branch never taken.")
         (p "As a positive control, the residual exceeds " (m "10⁻³") " (it is about .063)
            on a model whose " (code/inline "liar") " declares a support that disagrees with
            its density, so that its proposal is not the one the acceptance ratio assumes.")
         ,@(shown 'liar 'liar-or)
         (p "With " (code/inline "#:semantics 'upstream") " the kernel is upstream's, whose
            step delays the updates of inactive nodes (Metropolis.hs:298-303), run from the
            states where no update is delayed.  On " (code/inline "pswitch")
            " its residual is .05, while the extended kernel's is below "
            (m "10⁻¹²") ": the reactivation bias of E8, seen in one step.")
         (p "cf. plan §6.11; Design 3 §5.9.")))

  )

;; -------------------------------------------------------------------------------------------
;; Suite hakaru10-handlers (PLAN-IDIOM §5, H1-H8; H9 new)
;; -------------------------------------------------------------------------------------------

(define-suite hakaru10-handlers

  ((doc r) `((structure/section "One model, three interpretations")
             (p "A Hakaru10 model is a thunk that performs three operations through the native "
                (code/inline "shift") " of " (code/inline "(chicken continuation)") ":")
             (code/scheme (dist d args) (diracN e) (if test then-thunk else-thunk))
             (p "An interpretation is a " (i "handler") ", a procedure "
                (code/inline "(λ (request resume) ...)") " that " (code/inline "handle-model")
                " installs under a " (code/inline "reset") ".  The handler is deep, by
                trampoline: " (code/inline "resume") " runs the model up to its next request
                and hands that request to the handler again.  " (code/inline "if")
                " answers a " (i "thunk") ", which the model calls in its own context: the
                trace builder answers one that builds both branches, the inactive one first, as
                upstream's " (code/inline "ifnode") " does (Metropolis.hs:641-686), and a value
                handler answers the branch its test selects.  "
                (code/inline "condition") ", " (code/inline "lift") " and "
                (code/inline "memo/finite") " are not operations: conditioning makes an
                observed distribution (Syntax.hs:174-193), lifting is pure (Syntax.hs:99-116),
                and " (code/inline "memo/finite") " is an in-order map over its keys.")
             (p "The three interpretations are:")
             (ul (li (b "the trace builder") " of " (code/inline "mcmC")
                     ", one-shot: it builds upstream's trace graph, node by node and in
                     upstream's order, which incremental Metropolis-Hastings then updates.  It
                     is upstream's " (code/inline "Model a = MCMCM (SExp a)")
                     " (Syntax.hs:82), with the state monad replaced by a handler that holds the
                     chain;")
                 (li (b "exact enumeration") ", multi-shot: a lazy Hansei search tree (page
                     hakaru10-exact);")
                 (li (b "the forward sampler") ", one-shot: likelihood weighting, and the
                     construction-order run of " (code/inline "#:check") "."))
             (p "Under the last two an SExp is a plain value, so each is an oracle independent
                of the engine.  The tests below run the same models under all three.")))

  ((test/three-interpretations _)
   (define models
     `((grass ,grass) (girl ,girl) (pcorr ,(pcorr/pair .1) #:chain-samples 80000) (ph2 ,ph2)
       (ph21 ,ph21) (pcond1 ,pcond1) (pcond2 ,pcond2) (pcond4 ,pcond4)
       (post-bias ,(post-bias 1.))
       (colored-balls-3x4 ,(cballs-model 3 (make-list 4 'blue)) #:chain-samples 80000)
       (pswitch ,pswitch #:semantics extended)))
   (define (disagreement name model #!key (semantics 'upstream) (chain-samples 20000))
     (let ((exact (exact-posterior model))
           (weighted (likelihood-weighting 20000 model))
           (chain (mcmc-posterior chain-samples model #:semantics semantics)))
       (and (not (and ((histograms-within .02) exact weighted)
                      ((histograms-within .03) exact chain)))
            (list name exact weighted chain))))
   (⊦ equal? '() (filter-map (cut apply disagreement <>) models))
   `(doc (p "H1.  The same models under the three interpretations: exact enumeration, 20000
            runs of likelihood weighting, and an MCMC chain.  Likelihood weighting draws
            independent samples and lands within .016 of the exact posterior over the seeds
            17 and 1-40; the tolerance is .02.  The MCMC samples are autocorrelated, and the
            chain tolerance is .03.  Over the same seeds, 20000 samples land within .021 on
            all but two models, which mix slowly.  On "
            (code/inline "pcorr") " the observation favours " (m "x = y")
            " nine to one, so a single-site proposal from one likely state to the other must
            pass through an unlikely one.  On the colored
            balls the number of balls and the balls drawn are strongly correlated
            (ColoredBalls.hs:46-50).  At 20000 samples their distances average .019 and .021
            (standard deviations .014 and .010) and reach .070 and .045.  Both therefore run
            80000 samples, which land within .023 and .025 (means .010 and .011, standard
            deviations .006 and .005).  "
            (code/inline "pswitch") " runs under 'extended only, since upstream's semantics
            is biased on it (plan §6.8).")))

  ((test/construction-order _)
   (define models
     `((grass . ,grass) (pcond3 . ,pcond3) (pcond4 . ,pcond4) (pswitch . ,pswitch)
       (nested . ,nested) (colored-balls-8x10 . ,(cballs-model 8 (make-list 10 'blue)))
       (phier-10 . ,(phier 10)) (regression-5x500 . ,(regression 5 (regression-data 5 500)))))
   (⊦ equal?
     (map (λ (entry) (list (car entry) #t #t)) models)
     (map (Λ ((,name . ,model))
            (list name
                  (construction-check model #:semantics 'upstream)
                  (construction-check model #:semantics 'extended)))
          models))
   `(doc (p "H2.  The forward sampler, run in construction order on a copy of the same rng
            source, reproduces the trace builder's initial trace: the same value, and an LL
            within " (m "10⁻⁹") ", each run from its own copy of one rng source.  In
            construction order an " (code/inline "if_")
            " first runs the branch it does not take, drawing that branch's choices without
            weighing its observations, as " (code/inline "ifnode")
            " builds it (Metropolis.hs:650-661).  The LL is not bit-identical, because
            deactivation adds and then subtracts the inactive observations' LL.  The two runs
            make the same draws, so this oracle checks the node creation order (plan
            C18), the draw order and the LL accounting, without the engine's construction
            code.  " (code/inline "#:check") " runs this check once per chain.")
         ,@(shown 'nested 'phier 'normals 'dot 'regression)
         (code/lang "haskell" "phier n =
  (iterate (\\m -> do {x<-m; dist normal x 3}) $ dist normal 0 1) !! n")
         (p "cf. Bench.hs:14-15; the regression model of P §3 l.461-504 with Birds1.hs:100-111:")
         (code/lang "haskell" "normals :: [SExp Double] -> SExp Double -> MCMCM [SExp Double]
normals means std = mapM (\\m -> dist normal m std) means


dot :: [Double] -> [SExp Double] -> SExp Double
dot xs ays = foldr1 (+) $ zipWith (\\x ay -> (*x) <$> ay) xs ays")))

  ((test/hansei-format _)
   (let1 (shallow (probcc-explore 0 (model->search-tree grass)))
     (⊨ (any (λ (entry) (eq? 'C (caar entry))) shallow)))
   (⊦ (equal-approx? 1e-12) (exact-posterior grass)
     (hansei->histogram (probcc-explore +inf.0 (model->search-tree grass))))
   `(doc (p "H3.  " (code/inline "model->search-tree") " answers a genuine Hansei search tree.
            Explored to depth 0 it still has suspended " (code/inline "C")
            " entries, and explored fully and normalized it is the exact posterior:")
         (code/scheme ,(probcc-explore 0 (model->search-tree grass)))))

  ((test/errors _)
   (⊦= 'construction (hakaru10-kind (τ (dist bern .5))))
   (⊦= 'construction (hakaru10-kind (τ (diracN 3))))
   (⊦= 'construction (hakaru10-kind (τ (if_ #t 1 2))))
   (⊦ equal? '((builder . construction) (exact . construction) (forward . construction))
     (kinds-under-interpretations
       (model (let* ((x (dist bern .5))) (lift (λ (xv) (dist bern .5)) x)))))
   (⊦ equal? '((builder . branch) (exact . branch) (forward . branch))
     (kinds-under-interpretations
       (model (let* ((x (dist uniformly '(0 1)))) (if_ x 1 2)))))
   (⊦ equal? '((builder . internal) (exact . internal) (forward . internal))
     (kinds-under-interpretations (model (perform 'observe #t))))
   `(doc (p "H4.  The run-time checks that replace Haskell's types (plan §7) hold under every
            handler.  " (code/inline "dist") ", " (code/inline "diracN") " and "
            (code/inline "if_") " outside a model are " (code/inline "construction")
            " errors.  So is a " (code/inline "dist") " inside a lifted function, which runs
            in pure context.  A non-boolean test is a " (code/inline "branch")
            " error (" (code/inline "SExp Bool") ", Syntax.hs:275).  An unknown request is "
            (code/inline "internal") ".")))

  ((test/validation _)
   (define observes-a-choice (model (let* ((x (dist bern .5))) (dist (condition x bern) .5))))
   (⊦= 'conditioning (hakaru10-kind (τ (exact-posterior observes-a-choice))))
   (⊦= 'none (hakaru10-kind (τ (exact-posterior observes-a-choice #:validate #f))))
   (for-each (λ (choices)
               (⊦ (equal-approx? 1e-12) (list choices '((1 . .25) (2 . .25) (none . .5)))
                 (list choices (exact-posterior (nested-uniformly choices))))
               (⊦ equal? (list choices 'conditioning)
                  (list choices (hakaru10-kind (τ (exact-posterior
                                                     (nested-uniformly/ill-typed choices)))))))
             '(((1 2) ()) (() (1 2))))
   `(doc (p "H5.  Under a value handler a choice is a plain value, so conditioning on it looks
            legal.  Upstream's types reject it: observations are external to the model
            (Syntax.hs:38-42, P §2.3 l.412-421).  " (code/inline "exact-posterior")
            " therefore builds the model once with the trace builder first, which raises "
            (code/inline "conditioning") ".  " (code/inline "#:validate #f")
            " skips that build, and the program is then silently accepted.")
         (code/lang "haskell" "{-
biased_coin_ill_typed_too c1 c2 = do
  b <- dist bern 0.5
  dist (b  `condition` bern) 0.4
  return b
-}")
         (p "cf. Examples1.hs:196-201.")
         (p "The validation build is a build of the trace builder, which builds both branches
            of an " (code/inline "if_") ", so a branch the drawn values leave inactive may
            raise a parameter error that enumeration, taking only the selected branch, never
            meets.  It is a dynamic condition, not a breach of the discipline: the build runs
            in 'extended mode, where it rejects the build (plan C22), and is retried until one
            completes.  So the answer does not depend on what seed 17 draws first.  In "
            (code/inline "nested-uniformly") ", when xs is the empty list, the inactive then
            branch draws from " (code/inline "uniformly '()") ", an error; with the support
            of xs in either order the posterior is the same:")
         ,@(shown 'nested-uniformly)
         (p "and the retry still finds a breach that follows: "
            (code/inline "nested-uniformly/ill-typed")
            " observes a choice after the branch, and raises "
            (code/inline "conditioning") " with the support in either order, also when the
            first build is rejected.")
         ,@(shown 'nested-uniformly/ill-typed)))

  ((test/nested-branches _)
   (let1 (exact (exact-posterior two-levels))
     (⊦ (histograms-within .02) exact (mcmc-posterior 20000 two-levels))
     (⊦ (histograms-within .02) exact (mcmc-posterior 20000 two-levels #:semantics 'extended))
     (⊦ (histograms-within .02) exact (likelihood-weighting 20000 two-levels)))
   `(doc (p "H6.  Nested " (code/inline "if_") "s under the value handlers.  A branch answers "
            (code/inline "(diracN d)") " of a node of the enclosing scope, which the trace
            builder shares and does not copy (Syntax.hs:164-169), or the "
            (code/inline "diracN") " of a lifted expression.  The else branch observes a coin
            whose parameter depends on an outer node.  The MCMC chains, in both semantics,
            and likelihood weighting each run 20000 samples and agree with exact enumeration
            within .02:")
         ,@(shown 'two-levels)
         (code/scheme ,(exact-posterior two-levels))))

  ((test/continuous _)
   (⊦ (histograms-within .02) '((1 . .5) (20 . .25) (21 . .25))
     (likelihood-weighting 20000 pcond3))
   (⊦ (equal-approx? .4) 5.5
     (fold (Λ ((,x . ,p) ,mean) (+ mean (* x p))) 0. (likelihood-weighting 20000 mixng)))
   `(doc (p "H7.  Likelihood weighting needs no finite support.  "
            (code/inline "pcond3") " (E5b) gets its exact answer within .02, and the mean of "
            (code/inline "mixng") " is " (m "0.5 · 10 + 0.5 · 1 = 5.5")
            " within .4.")
         ,@(shown 'mixng)
         (code/lang "haskell" "mixng = do
  x <- dist normal 0 1
  if_ ((>0) <$> x)
           (dist normal 10 2)
           (dist gamma 3 (1/3))")
         (p "cf. Examples1.hs:139-144; P §2.2 l.331-366.")))

  ((test/multi-shot _)
   (⊦ (equal-approx? 1e-12) '(((#f . #f) . .25) ((#f . #t) . .25) ((#t . #f) . .25)
                              ((#t . #t) . .25))
     (exact-posterior (model (let* ((coins (memo/finite '(a b) (λ (_) (dist bern .5)))))
                               (pair (coins 'a) (coins 'b))))))
   (⊦ (equal-approx? 1e-12) (map cons '(1 2 3) (cballs-closed-form 3 4))
     (exact-posterior (cballs-model 3 (make-list 4 'blue))))
   `(doc (p "H8.  Exact enumeration resumes a model many times, so a model must be pure across
            its operations (PLAN-IDIOM C28).  " (code/inline "memo/finite")
            " is: each path builds its own table after its last choice, so two memoized coins
            are independent and E7 holds.  The model below is not pure, because "
            (code/inline "seen") " outlives the operations that mutate it, and enumeration
            answers lengths other than 2 (shown, not asserted):")
         ,@(shown 'impure)
         (code/scheme ,(exact-posterior impure #:validate #f))))

  ((test/new-interpretation _)
   (⊦ (equal-approx? 1e-12) '((#f . .7) (#t . .3)) (prior grass))
   (⊦ (equal-approx? 1e-12) '((1 . .5) (20 . .25) (21 . .25)) (prior pcond4))
   (⊦ (equal-approx? 1e-12) '((10 . .25) (11 . .25) (20 . .25) (21 . .25)) (prior pcond1))
   `(doc (p "H9.  The protocol is open: a fourth interpretation is a few lines over "
            (code/inline "handle-model") " and " (code/inline "value-handler")
            ".  The prior, by enumeration, is the exact handler with observations answered
            without being weighed: the rain of " (code/inline "grass")
            " gets back its prior .3, and the else branch of " (code/inline "pcond4")
            " its weight 1/2.")
         ,@(shown 'prior-handler 'prior)))

  )

(unittest/✓ hakaru10-exact)
(unittest/✓ hakaru10-handlers)
