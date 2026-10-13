; The main suites for (aux hakaru10), a CHICKEN port of Oleg Kiselyov's Hakaru10: the language,
; the replays of upstream's recorded runs, the laws of model composition, statistical checks,
; and the 'upstream and 'extended semantics of branches (plan §9.4, WP7).  The doc of
; hakaru10-language is the main documentation of the port (plan §10).
;
; Citation keys (paths relative to aux.scm/upstream/okmij-hakaru10/): S Syntax.hs, M
; Metropolis.hs, Mem Memory.hs, D Distribution.hs, U Util.hs, E Examples1.hs, CB
; ColoredBalls.hs, B1 Birds1.hs, BE Bench.hs, W index.html, BMP BirdMigrationProblem.txt, P
; design.txt (O. Kiselyov, "Probabilistic Programming Language and its Incremental Evaluation",
; APLAS 2016, LNCS 10017, DOI 10.1007/978-3-319-47958-3_19), and R random-1.0.1.1's
; System/Random.hs (hackage, 567 lines; not in upstream/).
; Hakaru10 code version: November 2016 (W:52-53); web page last updated July 14, 2018 (W:179).
;
; Expected values are upstream's records where they exist (E:..., S:..., CB:...), exact values
; otherwise, and, for the replays at seed 17, the values of the Python transcription of the
; engine over the random-1.0.1.1 StdGen (prototype regression values, plan §0).
; HAKARU10_SEEDS=17,1,2,... (make test-hakaru10-long) re-runs the statistical suite over seeds.
;
; Every model is written once, below, with `define/shown`, which also records its source for the
; docs (as in hakaru10-exact.scm and hakaru10-birds.scm).
; Haskell's `do` is `let*`, `return e` is the tail expression, and nodes are made in sequence:
; never in procedure-argument positions or inside `map` (plan C18, C18').

(import scheme
        (chicken base)
        (only (chicken format) format)
        (only (chicken process-context) get-environment-variable)
        (only (chicken string) string-split)
        (except (chicken condition) condition)
        srfi-1
        (aux base)
        (aux unittest)
        (only (aux stream) take§ §->list car§ cdr§)
        (only (aux hansei) probcc-explore)
        (aux hakaru10)
        (only (aux hakaru10 distribution) distribution-support distk-kind distk-value distk-rd)
        (only (aux hakaru10 exact) construction-check reference-check kernel-residual)
        (only (aux hakaru10 engine)
              handle-model value-handler build-chain node-parents new-chain run-model! ifnode!
              to-node! node? chain-node node-state node-state-set! make-nstate nstate-val
              nstate-ll nstate-dst nstate-tstamp chain-card-change-set! node-inactive-set!
              node-delayed-set! node-pending-set! mcmc-generator chain-fold node-update
              node-update-set! node-addr node-inactive? node-kind native-multishot-escapes?))

;; ---------------------------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------------------------

; (new) the sources of the definitions made with define/shown, newest first, for the docs.
(define sources '())

; (new) (define/shown name definition): make the definition, and keep its source under name.
(define-syntax-rule (define/shown name definition)
  (begin definition (push! (cons 'name 'definition) sources)))

; (new) the sources of the definitions of the names, in order, one code/scheme element each
; (spliced with ,@ into a doc).
(define (shown . names) (map (λ (name) `(code/scheme ,(alist-ref name sources))) names))

; (new) the seeds of the statistical suite: HAKARU10_SEEDS=17,1,2,... (make
; test-hakaru10-long), else 17 alone (M:858 `mkStdGen 17`).
(define seeds
  (let1 (s (get-environment-variable "HAKARU10_SEEDS"))
    (if s (map string->number (string-split s ", ")) '(17))))

; (new) assert that got is expected within tol, for the chain seeded with seed, which a failure
; reports.
(define (⊦≈ seed tol expected got)
  (⊦ (equal-approx? tol) `(seed ,seed ,expected) `(seed ,seed ,got)))

; (new) assert that got is below bound, for the chain seeded with seed.
(define (⊦< seed got bound)
  (⊦ (λ (g b) (< (caddr g) (caddr b))) `(seed ,seed ,got) `(seed ,seed ,bound)))

; (new) the `kind` of the (exn hakaru10) condition that thunk raises, or 'none.
(define (hakaru10-kind thunk)
  (condition-case (begin (thunk) 'none)
    (e (hakaru10) (hakaru10-error-kind e))))

; (new) what thunk raises: the `kind` of an (exn hakaru10) condition, 'exn for any other error
; (car of '(), say), or 'none.
(define (raised thunk)
  (condition-case (begin (thunk) 'none)
    (e (hakaru10) (hakaru10-error-kind e))
    (e (exn) 'exn)))

; (new) the message of the 'internal (exn hakaru10) condition that thunk raises, the oracles'
; reports; else the kind of the condition, or 'none.
(define (internal-message thunk)
  (condition-case (begin (thunk) 'none)
    (e (hakaru10) (if (eq? 'internal (hakaru10-error-kind e))
                      ((condition-property-accessor 'exn 'message) e)
                      (hakaru10-error-kind e)))))

; (new) the number of #t in a list of samples, as upstream's `length . filter id` (E:93).
(define (count-true xs) (count identity xs))

; (new) the histogram of xs normalized to proportions, ((v . p) ...) by generic<?.
(define (proportions xs)
  (let1 (n (exact->inexact (length xs)))
    (map (Λ ((,v . ,k)) (cons v (/ k n))) (hist xs))))

; (new) the mean of a list of samples (momenta's first component, U:16-22).
(define (mean xs) (car (momenta xs)))

; (new) the standard normal distribution function, by Abramowitz and Stegun 7.1.26 (absolute
; error below 1.5e-7): CHICKEN has no erf.
(define (Φ z)
  (let* ((x (/ (abs z) (sqrt 2.)))
         (t (/ 1. (+ 1. (* .3275911 x))))
         (poly (* t (+ .254829592 (* t (+ -.284496736 (* t (+ 1.421413741
                                                               (* t (+ -1.453152027
                                                                       (* t 1.061405429))))))))))
         (erf (- 1. (* poly (exp (- (* x x)))))))
    (/ (+ 1. (if (negative? z) (- erf) erf)) 2.)))

; (new) the mass of N(0, sd) in the bins [b, b + step) of the given bin starts, as tabulate's
; histograms are keyed (U:40-45): the exact q of the KL test of phier (E:69-85).
(define (normal-bins step sd bins)
  (map (λ (b) (cons b (- (Φ (/ (+ b step) sd)) (Φ (/ b sd))))) bins))

;; ---------------------------------------------------------------------------------------------
;; Models of Examples1.hs, Syntax.hs, Bench.hs, ColoredBalls.hs and P §3, and the new ones of
;; the plan (§6.8, §9.4).  A model is a thunk (S:82), so models are values: they compose by
;; calling one another, and a model that takes arguments is a procedure answering a model.
;; ---------------------------------------------------------------------------------------------

; cf. E:10 `pbern = dist bern 0.4`; the general form takes the coin, so that the replays of the
; records made with upstream's always-flip kernel can pass bern/flip (D:64-70, plan X15).
(define/shown pbern* (define-model (pbern* bern) (dist bern .4)))
(define pbern (pbern* bern))                ; E:10, with the default kernel

; cf. E:14 `pnorm = dist normal 10 0.5`.
(define/shown pnorm (define-model pnorm (dist normal 10. .5)))

; cf. E:16-18 pdep1: a normal and a Dirac node depending on it (P l.231-234).
(define/shown pdep1
  (define-model pdep1
    (let* ((x (pnorm)))
      (diracN (sexp+ x 1.)))))

; cf. E:24-26 pdepR, `do {x <- p; diracN x}` (P l.251-256).
(define/shown pdepR
  (define-model pdepR
    (let* ((x (pnorm)))
      (diracN x))))

; cf. E:30-32 pdepR', the same with `return x`.
(define/shown pdepR′
  (define-model pdepR′
    (let* ((x (pnorm)))
      x)))

; cf. E:36-39 pjoin (P l.265-268).
(define/shown pjoin
  (define-model pjoin
    (let* ((x (pbern))
           (y (pnorm)))
      (pair x y))))

; cf. E:43-46 pmarg (P l.274-277).
(define/shown pmarg
  (define-model pmarg
    (let* ((x (pbern))
           (y (pnorm)))
      y)))

; cf. BE:15 and E:63 `\m -> do {x<-m; dist normal x 3}`: the model transformer of phier.
(define/shown deeper (define ((deeper m)) (let* ((x (m))) (dist normal x 3.))))

; cf. BE:14-15 `phier n = (iterate (\m -> do {x<-m; dist normal x 3}) $ dist normal 0 1) !! n`
; and E:62-63 (n = 10; P l.300): the n-th iterate of `deeper` over the standard normal.
(define/shown phierN
  (define (phierN n) (fold (λ (_ m) (deeper m)) (model (dist normal 0. 1.)) (iota n))))

; cf. E:65-67 ph1.
(define/shown ph1
  (define-model ph1
    (let* ((x (dist normal 0. 1.)))
      (dist normal x 3.))))

; cf. E:88-91 ph2 (0.2 * 0.1 + 0.8 * 0.7 = 0.58); the general form takes the coin (X15).
(define/shown ph2*
  (define-model (ph2* bern)
    (let* ((x (dist bern .2)))
      (dist bern (lift (λ (xv) (if xv .1 .7)) x)))))
(define ph2 (ph2* bern))                    ; E:88-91, with the default kernel

; cf. E:96-99 ph21; the general form takes the coin (X15).
(define/shown ph21*
  (define-model (ph21* bern)
    (let* ((x (dist bern .2))
           (y (dist bern (lift (λ (xv) (if xv .1 .7)) x))))
      x)))
(define ph21 (ph21* bern))                  ; E:96-99, with the default kernel

; cf. E:108 `((==1) <$> x)`, the test of pcond1-4.
(define/shown is1 (define (is1 x) (lift (λ (xv) (= xv 1)) x)))

; cf. E:106-110 pcond1.
(define/shown pcond1
  (define-model pcond1
    (let* ((x (dist categorical '((1 . .5) (2 . .5)))))
      (if_ (is1 x)
           (dist categorical '((10 . .5) (11 . .5)))
           (dist categorical '((20 . .5) (21 . .5)))))))

; cf. E:116-120 pcond2.
(define/shown pcond2
  (define-model pcond2
    (let* ((x (dist categorical '((1 . .5) (2 . .5)))))
      (if_ (is1 x)
           x
           (dist categorical '((20 . .5) (21 . .5)))))))

; cf. E:126-132 pcond3.
(define/shown pcond3
  (define-model pcond3
    (let* ((x (dist categorical '((1 . .5) (2 . .5)))))
      (if_ (is1 x)
           x
           (begin
             (dist normal 0. 1.)
             (dist categorical '((20 . .5) (21 . .5))))))))

; cf. E:265-271 pcond4.
(define/shown pcond4
  (define-model pcond4
    (let* ((x (dist categorical '((1 . .5) (2 . .5)))))
      (if_ (is1 x)
           x
           (begin
             (dist (condition #t bern) .5)
             (dist categorical '((20 . .5) (21 . .5))))))))

; cf. E:140-144 mixng (P l.332-337).
(define/shown mixng
  (define-model mixng
    (let* ((x (dist normal 0. 1.)))
      (if_ (lift (λ (xv) (> xv 0.)) x)
           (dist normal 10. 2.)
           (dist gamma 3. (/ 1. 3.))))))

; cf. E:147-151 mixng' (P l.355-360); the general form takes the coin (X15).
(define/shown mixng′*
  (define-model (mixng′* bern)
    (let* ((x (dist bern .5)))
      (if_ x
           (dist normal 10. 2.)
           (dist gamma 3. (/ 1. 3.))))))
(define mixng′ (mixng′* bern))              ; E:147-151, with the default kernel

; cf. E:174-178 biased_coin (P l.379-383).
(define/shown biased-coin
  (define-model (biased-coin c1 c2)
    (let* ((b (dist beta 1. 1.)))
      (dist (condition c1 bern) b)
      (dist (condition c2 bern) b)
      b)))

; cf. E:226-235 pcorr, Yufei Cai's example (E:223-224); the general form answers (result x y),
; y upstream.
(define/shown pcorr*
  (define-model (pcorr* pfuzz result)
    (define ((fuzzy2 pfuzz op) x y)
      (let1 (v (op x y))
        (categorical `((,v . ,pfuzz) (,(not v) . ,(- 1. pfuzz))))))
    (define (xor y x) (not (eq? x y)))
    (let* ((x (dist bern .5))
           (y (dist bern .5)))
      (dist (condition #t (fuzzy2 pfuzz xor)) x y)
      (result x y))))
(define (pcorr pfuzz) (pcorr* pfuzz (λ (x y) y)))   ; E:226-235, answering y

; cf. E:242-246 phierN': n - 1 normal steps of sd 3 from start, then the observation obs.
(define/shown phierN′
  (define-model (phierN′ n start obs)
    (let loop ((i 1) (x start))
      (if (= i n)
          (dist (condition obs normal) x 3.)
          (loop (add1 i) (dist normal x 3.))))))

; cf. E:248-251 condhier.
(define/shown condhier
  (define-model condhier
    (let* ((x (dist normal 0. 1.)))
      ((phierN′ 9 x 1.))
      x)))

; cf. E:256-260 post_bias (P l.427-434).
(define/shown post-bias
  (define-model (post-bias c)
    (let* ((coin (dist bern .5)))
      (if_ coin
           (dist (condition c normal) 0. 1.)
           (dist (condition c normal) 100. 1.))
      coin)))

; cf. E:311-314 nnot, the noisy-not function of grass (P l.84-89).
(define/shown nnot (define (nnot p b) (if b p 1.)))

; cf. E:306-309 nor, the noisy-or function of grass (P l.81-83), in Haskell's association:
; 1 - ((nnot (1-strengthX) x * nnot (1-strengthY) y) * (1-noise)).
(define/shown nor
  (define ((nor strength-x strength-y noise) x y)
    (bern (- 1. (* (* (nnot (- 1. strength-x) x) (nnot (- 1. strength-y) y))
                   (- 1. noise))))))

; cf. E:299-304 grass (P l.73-80, l.440-447).
(define/shown grass
  (define-model grass
    (let* ((rain (dist bern .3))
           (sprinkler (dist bern .5)))
      (dist (condition #t (nor .9 .8 .1)) rain sprinkler)
      rain)))

; cf. E:302, commented out upstream, with P l.60-62: Fig. 1 without the observation, its
; unconditioned `dist (nor 0.9 0.8 0.1) rain sprinkler`, queried for the grass being wet
; ("60.6%"), so the wet node is the result; upstream's grass with E:302 in place of E:303
; still returns rain, whose prior is 0.3 (hakaru10-exact.scm).
(define/shown grass/wet
  (define-model grass/wet
    (let* ((rain (dist bern .3))
           (sprinkler (dist bern .5)))
      (dist (nor .9 .8 .1) rain sprinkler))))

; cf. E:284-296, commented out upstream: grass with the observation in the leaves of nested
; branches (conditioning inside branches, P l.181-183) instead of the noisy-or; obs is a
; procedure answering a model, as Haskell's `let obs = dist (True `condition` bern)`.  Its
; posterior differs from grass's: P(rain | wet) = 0.27/0.585 = 6/13.
(define/shown grass/if
  (define-model grass/if
    (let* ((rain (dist bern .3))
           (sprinkler (dist bern .5))
           (obs (λ (p) (dist (condition #t bern) p))))
      (if_ rain
           (obs .9)
           (if_ sprinkler (obs .8) (obs .1)))
      rain)))

; cf. E:340-352 girl, the eldest daughter puzzle.
(define/shown girl
  (define-model girl
    (define ((fuzzy2 pfuzz op) x y)
      (let1 (v (op x y))
        (categorical `((,v . ,(- 1. pfuzz)) (,(not v) . ,pfuzz)))))
    (let* ((girl1 (dist bern .5))
           (girl2 (dist bern .5)))
      (dist (condition #t (fuzzy2 .01 (λ (a b) (or a b)))) girl1 girl2)
      girl1)))

; cf. E:357-362 mhex (P l.677-686).
(define/shown mhex
  (define-model mhex
    (let* ((x (dist normal 0. 1.))
           (y (dist normal x 2.)))
      (if_ (lift (λ (xv) (> xv .5)) x)
           x
           (let* ((z (dist beta 1. 1.)))
             (sexp+ y z))))))

; cf. S:282-286 niceex1.
(define/shown niceex1
  (define-model niceex1
    (let* ((c (dist uniform 0. 1.))
           (d (dist bern .5))
           (e (dist bern c)))
      (diracN (lift (λ (ev dv) (and ev dv)) e d)))))

; cf. S:291-295 nice_prog_mult_conditions.
(define/shown nice-prog-mult-conditions
  (define-model (nice-prog-mult-conditions c1 c2)
    (let* ((b (dist beta 1. 1.)))
      (dist (condition c1 bern) b)
      (dist (condition c2 bern) b)
      (diracN b))))

; cf. S:303-307 prog_mixture1.
(define/shown prog-mixture1
  (define-model (prog-mixture1 cv)
    (let* ((c (dist bern .5)))
      (if_ c
           (dist (condition cv normal) 1. 1.)
           (dist (condition cv uniform) 0. 3.))
      c)))

; cf. S:314-319 exif1: `(fromIntegral . fromEnum) <$> c` is 1 for True.
(define/shown exif1
  (define-model exif1
    (let* ((c (dist bern .5))
           (d (if_ c
                   (dist normal (lift (λ (cv) (if cv 1. 0.)) c) 1.)
                   (diracN 5.)))
           (e (if_ (lift not c)
                   (dist normal 20. 1.)
                   (diracN d))))
      (diracN (sexp+ d e)))))

; cf. S:323-335 exif2, with its `where` clauses.
(define/shown exif2
  (define-model exif2
    (define (cl1 x)
      (let* ((d (dist bern .5)))
        (if_ d (cl3) (diracN x))))
    (define (cl3)
      (let* ((a (dist uniform 0. 1.))
             (b (dist uniform 0. 1.)))
        (diracN (sexp+ a b))))
    (let* ((c (dist bern .5)))
      (if_ c (cl1 10) (cl1 20)))))

; cf. S:340-345 tac1.
(define/shown tac1
  (define-model tac1
    (let* ((c (dist bern .5)))
      (if_ c
           (dist normal 0. 1.)
           (dist uniform 10. 20.))
      c)))

; cf. S:350-355 tac2.
(define/shown tac2
  (define-model tac2
    (let* ((c (dist bern .5)))
      (if_ c
           (begin (diracN #t) (dist normal 0. 1.))
           (dist uniform 10. 20.))
      c)))

; cf. S:360-365 tac3: `e >>= diracN` in the else branch.
(define/shown tac3
  (define-model tac3
    (let* ((c (dist bern .5)))
      (if_ c
           (begin (diracN #t) (dist normal 0. 1.))
           (diracN (dist uniform 10. 20.)))
      c)))

; cf. S:373-380 tac10.
(define/shown tac10
  (define-model tac10
    (let* ((c (dist bern .5)))
      (if_ c
           (dist (condition 0. normal) 0. 1.)
           (dist (condition 0. uniform) 10. 20.))
      c)))

; cf. S:385-394 tac11.
(define/shown tac11
  (define-model tac11
    (let* ((c (dist bern .5)))
      (if_ c
           (dist (condition 0. normal) 0. 1.)
           (let* ((d (diracN 10.)))
             (dist (condition 0. uniform) d 20.)))
      c)))

; (new) plan §6.8, the counterexample to upstream's reactivation (M:1159-1165): P(c) = 0.5,
; upstream's kernel converges to 0.377286.
(define/shown pswitch
  (define-model pswitch
    (let* ((c (dist bern .5))
           (m (dist uniformly '(0 1))))
      (if_ c
           (dist categorical (lift (λ (mv) `((,mv . .9) (,(- 1 mv) . .1))) m))
           (diracN (lift identity m)))
      c)))

; (new) deviation 1 (M:788-792): d is shared by the then branch and the rest of the model, so
; resampling d while c is false enqueues the exit, whose entry and active result did not change.
(define/shown shared-inactive
  (define-model shared-inactive
    (let* ((c (dist bern .5))
           (d (dist normal 0. 1.)))
      (if_ c d (dist normal 5. 1.)))))

; (new) plan C22: the inactive branch holds a gamma of a negative shape whenever x <= 0.
(define/shown invalid-inactive
  (define-model invalid-inactive
    (let* ((x (dist normal 0. 1.)))
      (if_ (lift positive? x) (dist gamma x 1.) 0.))))

; (new) plan C22 with a raising density: beta's log-density raises a parameter error for a
; shape a <= 0 (D:330-339), where gamma's answers -inf.
(define/shown invalid-inactive/beta
  (define-model invalid-inactive/beta
    (let* ((x (dist normal 0. 1.)))
      (if_ (lift positive? x) (dist beta x 1.) 0.))))

; (new) plan C22, the other side: the inner test of the else branch, inactive when c holds,
; is then 0, not a boolean, a branch error (deviation 9, S:275), not a parameter one.
(define/shown invalid-inactive/branch
  (define-model (invalid-inactive/branch p)
    (let* ((c (dist bern p))
           (x (dist bern .5)))
      (if_ c 3 (if_ (lift (λ (cv xv) (if cv 0 xv)) c x) 1 2))
      c)))

; (new) plan C22, the boundary: the observation is active, and its beta's first parameter is
; -1 whenever b is false, where beta's density raises a parameter error (D:330-339).  An active
; node's parameter error propagates in both semantics; only an inactive node's rejects.
(define/shown active-invalid
  (define-model active-invalid
    (let* ((b (dist bern .9)))
      (dist (condition .5 beta) (lift (λ (bv) (if bv 1. -1.)) b) 1.)
      b)))

; (new, deviation 8, M:724-751) an observation that the step activating it also updates: its
; categorical reads x, the choice whose proposal moves x from 2 (the observation inactive, its
; stale LL -inf) to 0 or 1.  In 'extended mode its weight must be added once, with its LL
; after the update, when propagation ends (settle-pending!), not at activation (-inf + inf)
; nor again by the update (counted twice).  P(x) = (.9 .3 1)/2.2 for x = 0, 1, 2.
(define/shown act-update
  (define-model act-update
    (let* ((x (dist uniformly '(0 1 2))))
      (if_ (lift (λ (v) (< v 2)) x)
           (dist (condition 0 categorical)
                 (lift (λ (v) (case v
                                ((0) '((0 . .9) (1 . .1)))
                                ((1) '((0 . .3) (1 . .7)))
                                (else '((1 . 1.)))))
                       x))
           'none)
      x)))
; (new) the same with a finite stale LL (log .5 at x = 2).
(define/shown act-update/finite
  (define-model act-update/finite
    (let* ((x (dist uniformly '(0 1 2))))
      (if_ (lift (λ (v) (< v 2)) x)
           (dist (condition 0 categorical)
                 (lift (λ (v) (case v
                                ((0) '((0 . .9) (1 . .1)))
                                ((1) '((0 . .3) (1 . .7)))
                                (else '((0 . .5) (1 . .5)))))
                       x))
           'none)
      x)))

; (new, deviation 8 and plan C22) proposing x = 0 from x = 2 activates the observation (its
; pending flag set) and then rejects the step mid-propagation, on the inactive `uniformly '()`
; of the second if_: the journal must restore the flag (set-pending!).  The extended target
; excludes x = 0 (its inactive node is invalid, as in invalid-inactive): P(x = 1) = .3/1.3.
(define/shown pending-abort
  (define-model pending-abort
    (let* ((x (dist uniformly '(0 1 2))))
      (if_ (lift (λ (v) (< v 2)) x)
           (dist (condition 0 categorical)
                 (lift (λ (v) `((0 . ,(if (= v 0) .9 .3)) (1 . ,(if (= v 0) .1 .7)))) x))
           'none)
      (if_ (lift (λ (v) (not (= v 0))) x)
           (dist uniformly (lift (λ (v) (if (= v 0) '() '(a b))) x))
           'z)
      x)))

; cf. B1:103-107 normals: independent normals of the given means and sd, made in order.
(define/shown normals (define (normals means std) (map-in-order (λ (m) (dist normal m std)) means)))

; cf. B1:110-111 `dot xs ays = foldr1 (+) $ zipWith (\x ay -> (*x) <$> ay) xs ays`: no node.
(define/shown dot
  (define (dot xs ays)
    (reduce-right sexp+ #f (map (λ (x ay) (lift (λ (a) (* a x)) ay)) xs ays))))

; cf. P §3 l.471-486, the Bayesian linear regression of PPAML problem 4.1, for the training
; points xsy, a list of (xs . y).
(define/shown regression
  (define-model (regression xsy)
    (let* ((mu (make-list (length (caar xsy)) 0.))
           (w-mean (normals mu 2.))
           (w (normals w-mean 1.))
           (noise-sd (lift (λ (g) (/ 1. g)) (dist gamma .5 .5))))
      (for-each (Λ ((,xs . ,y)) (dist (condition y normal) (dot xs w) noise-sd)) xsy)
      (collect w))))

; (new) n synthetic training points (xs . y) of dimension k for `regression`: deterministic xs
; in [-1, 1] and y = xs·w plus a small deterministic noise.
(define (regression-data k n #!optional (w '(1.5 -.7 .5 2. -1.)))
  (map (λ (i)
         (let1 (xs (map (λ (j) (sin (exact->inexact (+ (* 3 i) (* 7 j) 1)))) (iota k)))
           (cons xs (+ (fold + 0. (map * xs (take w k))) (* .1 (cos (exact->inexact i)))))))
       (iota n)))

; cf. CB:30-32 opposite_color.
(define/shown opposite-color (define (opposite-color color) (if (eq? color 'blue) 'green 'blue)))

; cf. CB:34-36 observed_color: an observation is wrong with probability 0.2.
(define/shown observed-color
  (define (observed-color color)
    (categorical `((,color . .8) (,(opposite-color color) . .2)))))

; cf. CB:38-39 maxBalls.
(define/shown max-balls (define max-balls 8))

; cf. CB:41-44 balls_prior: stochastic memoization replaced by pre-allocation (memo/finite,
; P footnote 5 l.566-569): one color per possible ball, made in order.
(define/shown balls-prior
  (define (balls-prior n)
    (memo/finite (iota n 1) (λ (i) (dist uniformly '(blue green))))))

; cf. CB:74-89 balls_observe: for each observed color, the ball drawn and `foldr observe (return
; (pure color)) [1..maxBalls]`, the chain of if_s conditioning on the drawn ball's color.
(define/shown balls-observe
  (define (balls-observe ball obs nballs)
    (for-each (λ (color)
                (let* ((b (dist uniformly (iota nballs 1))))
                  (let observe ((i 1))
                    (if (> i max-balls)
                        color
                        (if_ (lift (λ (bv) (= bv i)) b)
                             (dist (condition color observed-color) (ball i))
                             (observe (add1 i)))))))
              obs)
    '()))

; cf. CB:91-99 cballs_model, BLOG's urn (CB:1-13; P §3 l.505-616).
(define/shown cballs-model
  (define-model (cballs-model obs)
    (let* ((ball (balls-prior max-balls))
           (nballs (dist uniformly (iota max-balls 1))))
      (let obs-number ((i 1))
        (if (> i max-balls)
            '()
            (if_ (lift (λ (n) (= n i)) nballs)
                 (balls-observe ball obs i)
                 (obs-number (add1 i)))))
      nballs)))

; (new) PLAN-IDIOM C28: a model that mutates, across an operation, a variable shared by the
; continuations that exact enumeration resumes: for the docs only, never asserted.
(define/shown impure
  (define-model impure
    (let* ((seen '())
           (x (dist bern .5)))
      (push! x seen)
      (let* ((y (dist bern .5)))
        (push! y seen)
        (length seen)))))

; (new) a fourth interpretation, written here against the engine's handler protocol: the number
; of paths through a model with finite supports, resuming the model once per support point of
; each choice (multi-shot) and once per observation.
(define/shown count-paths
  (define (count-paths model)
    (handle-model model
                  (value-handler
                    (λ (dk resume)
                      (if (eq? (distk-kind dk) 'observed)
                          (resume (distk-value dk))
                          (fold (λ (xp n) (+ n (resume (car xp))))
                                0
                                (distribution-support dk)))))
                  (λ (value) 1))))

;; (new) the pair of two independent normals, written with letlift, whose e's create the nodes
;; left to right (plan C18), with let* and pair, and with let* in the other order.
(define/shown ordered-pair
  (define-model ordered-pair
    (letlift ((a (dist normal 0. 1.)) (b (dist normal 5. 1.))) (cons a b))))
(define/shown ordered-pair*
  (define-model ordered-pair*
    (let* ((a (dist normal 0. 1.)) (b (dist normal 5. 1.))) (pair a b))))
(define/shown swapped-pair
  (define-model swapped-pair
    (let* ((b (dist normal 5. 1.)) (a (dist normal 0. 1.))) (pair a b))))

; (new) a random choice whose values are procedures, and a Dirac node of one: value handlers
; box a procedure value with `constant`, as lift does, so that it stays a Val (S:105-116).
(define/shown procedure-choice
  (define-model procedure-choice
    (let* ((f (dist categorical (list (cons car .5) (cons cadr .5)))))
      (lift (λ (g) (g '(1 2))) f))))
(define/shown procedure-constant
  (define-model procedure-constant
    (let* ((c (dist bern .5))
           (f (diracN (constant car))))
      (lift (λ (g cv) (if cv (g '(1 2)) 0)) f c))))

; (new) a Dirac node whose value is a closure built at run time from a choice: recomputing it
; builds a fresh closure, which `#:check` must not take for a difference.
(define/shown procedure-built
  (define-model procedure-built
    (let* ((k (dist uniformly '(1 2 3)))
           (scale (diracN (lift (λ (kv) (λ (x) (* kv x))) k))))
      (lift (λ (f) (f 10)) scale))))

; (new) a Dirac node whose value is circular (srfi-1's circular-list), a legitimate Scheme
; value: the walks of the trace builder (deviation 9's check for SExps inside a value) and of
; `#:check` must end on it.
(define/shown circular-value
  (define-model circular-value
    (let* ((b (dist bern .5))
           (cyc (diracN (lift (λ (bv) (circular-list bv)) b))))
      (lift car cyc))))

; (new) cf. M:99 and M:120-129 (a Dirac node's value is lazy upstream): `car` guarded by an if_
; whose else branch is inactive exactly when xs is empty, where it applies car to '().
(define/shown guarded-car
  (define-model guarded-car
    (let* ((k (dist uniformly '(0 1 2)))
           (xs (diracN (lift (λ (kv) (list-tail '(a b) kv)) k)))
           (empty (diracN (lift null? xs))))
      (if_ empty 'none (diracN (lift car xs))))))

; (new) guarded-car with the support of k reversed: at seed 17 (and every seed tried) the
; initial trace has k = 2 in guarded-car, its car node built inactive, and k = 0 here, its car
; node built active, which an evaluator must not then apply strictly to the '() of k = 2.
(define/shown guarded-car/reversed
  (define-model guarded-car/reversed
    (let* ((k (dist uniformly '(2 1 0)))
           (xs (diracN (lift (λ (kv) (list-tail '(a b) kv)) k)))
           (empty (diracN (lift null? xs))))
      (if_ empty 'none (diracN (lift car xs))))))

; (new) the same with a constant test, so that the else branch is inactive forever, and with a
; test that is a lifted expression rather than a node.
(define/shown guarded-car/constant
  (define-model guarded-car/constant
    (let* ((xs '())
           (b (dist bern .5)))
      (if_ (lift null? xs) b (diracN (lift car xs))))))
(define/shown guarded-car/node
  (define-model guarded-car/node
    (let* ((b (dist bern .5))
           (xs (lift (λ (bv) (if bv '() '(1))) b)))
      (if_ (lift null? xs) 'none (diracN (lift car xs))))))

; (new) guarded-car with the car under a nested if_: when xs is empty, the inner if_ is inactive
; and, whenever c holds, its exit copies the deferred car of '() (deviation 11): an inactive
; exit copies the active result's value without forcing it, in 'extended mode too, where the
; inactive exit is updated.
(define/shown nested-guarded-car
  (define-model nested-guarded-car
    (let* ((k (dist uniformly '(0 1 2)))
           (xs (diracN (lift (λ (kv) (list-tail '(a b) kv)) k)))
           (c (dist bern .5)))
      (pair c (if_ (lift null? xs) 'none (if_ c (diracN (lift car xs)) 'other))))))

; (new) deviation 11 (M:99, M:120-129, S:100-116): h's value is deferred when xs is empty, and
; mu ignores it, the cdr of (pair h 1.).  Upstream builds the inactive branch, mu being snd
; (⊥, 1.0) = 1.0; here mu is the deferred value too, which the normal, a strict reader, forces.
(define/shown strict-reader
  (define-model (strict-reader xs)
    (let* ((c (dist bern .5)))
      (if_ (lift null? xs)
           c
           (let* ((h (diracN (lift car xs)))
                  (mu (diracN (lift cdr (pair h 1.)))))
             (dist normal mu 1.))))))
(define/shown strict-reader/node
  (define-model strict-reader/node
    (let* ((c (dist bern .5))
           (xs (lift (λ (cv) (if cv '() '(1))) c)))
      (if_ (lift null? xs)
           'empty
           (let* ((h (diracN (lift car xs)))
                  (mu (diracN (lift cdr (pair h 1.)))))
             (dist normal mu 1.))))))

; (new) models with a handler of their own around an operation, for the value handlers only
; (under the trace builder x and b are nodes, which `if` and `=` cannot test): the error is
; raised by the model after a choice and caught by the model.  Exact enumeration resumes the
; continuation of the choice once per support point, inside the handler.
(define/shown guarded-after-choice
  (define-model guarded-after-choice
    (handle-exceptions e 0.
      (let* ((x (dist bern .5)))
        (if x (error "guarded-after-choice") 1.)))))
(define/shown guarded-across-choices
  (define-model guarded-across-choices
    (let* ((b (dist uniformly '(0 1 2))))
      (handle-exceptions e 'caught
        (let* ((c (dist bern .5)))
          (when (and c (= b 0)) (error "guarded-across-choices"))
          (list b c))))))
; (new) the same guard inside a lifted function, around no operation.
(define/shown guarded-inside-lift
  (define-model guarded-inside-lift
    (let* ((b (dist uniformly '(0 1 2)))
           (c (dist bern .5)))
      (lift (λ (bv cv)
              (handle-exceptions e 'caught
                (if (and cv (= bv 0)) (error "guarded-inside-lift") (list bv cv))))
            b c))))

; (new) cf. P §4.2 l.771-776 pdep: z depends on y only, so resampling x, which changes y's LL
; but not its value, must not update z (P l.779-786).
(define/shown pdep
  (define-model pdep
    (let* ((x (dist normal 0. 1.))
           (y (dist normal x 1.))
           (z (dist normal y 1.)))
      (sexp+ x y z))))

; (new) cf. M:327-333 upp1 (the surface-syntax text, commented out upstream; M:335-340 is the
; same on the raw trace API) and M:387-397 upp2 (commented out upstream): d depends on a, b
; and c, which a's resampling all changes; d must be updated once, after b and c (M:342-361).
(define/shown upp1
  (define-model upp1
    (let* ((a (dist uniform 0. 1.))
           (b (diracN (sexp+ a 2.)))
           (c (diracN (sexp+ a 3.)))
           (d (dist uniform (sexp+ a b) c)))
      d)))
(define/shown upp2
  (define-model upp2
    (let* ((a (dist uniform 0. 1.))
           (b (diracN (sexp+ a 2.)))
           (c (diracN (sexp+ b 3.)))
           (d (dist uniform (sexp+ a b) c)))
      d)))

; (new) the updates that ran in each of the n - 1 steps of model's chain (mcmc-generator's
; keys), in the order they ran, as lists of node addresses: every node's update is wrapped,
; once the trace is built, by one that records its address, and the step hook collects them.
(define/shown update-traces
  (define (update-traces n model . keys)
    (let ((updated '()) (steps '()))
      (define (spy! ch)
        (chain-fold (λ (node _)
                      (let1 (update (node-update node))
                        (when update
                          (node-update-set! node
                                            (λ (n) (push! (node-addr n) updated) (update n))))))
                    (void) ch))
      (define (collect! ch accepted? delta)
        (push! (reverse updated) steps)
        (set! updated '()))
      (let1 (next (apply mcmc-generator n model #:build-hook spy! #:step-hook collect! keys))
        (do ((i 0 (add1 i))) ((= i n)) (next)))
      (reverse steps))))

;; ---------------------------------------------------------------------------------------------
;; Suite hakaru10-language, with the main documentation of the port (plan §10)
;; ---------------------------------------------------------------------------------------------

(define-suite hakaru10-language

  ((doc r)
   `((structure/section "Introduction")
     (p "Quoting from "
        (cite/a "https://okmij.org/ftp/kakuritu/Hakaru10/index.html"
                "Embedded probabilistic domain-specific language Hakaru10")
        ":"
        (cite/quote
          "Oleg Kiselyov"
          "This system description paper introduces the probabilistic programming
          language Hakaru10, for expressing, and performing inference on (general)
          graphical models. The language supports discrete and continuous
          distributions, mixture distributions and conditioning. Hakaru10 is a DSL
          embedded in Haskell and supports Monte-Carlo Markov Chain (MCMC) inference.
          Hakaru10 is designed to address two main challenges of probabilistic
          programming: performance and correctness. It implements the incremental
          Metropolis-Hastings method, avoiding all redundant computations. In the
          presence of conditional branches, efficiently maintaining dependencies and
          correctly computing the acceptance ratio are non-trivial problems, solved in
          Hakaru10. The implementation is unique in being explicitly designed to
          satisfy the common equational laws of probabilistic programs. Hakaru10 is
          typed; specifically, its type system statically prevents meaningless
          conditioning, enforcing that the values to condition upon must indeed come
          from outside the model."))
     (p "The paper is "
        (cite/a "https://doi.org/10.1007/978-3-319-47958-3_19"
                (i "Probabilistic Programming Language and its Incremental Evaluation")
                ", APLAS 2016, LNCS 10017")
        " (" (cite/a "https://okmij.org/ftp/kakuritu/Hakaru10/design.pdf" "design.pdf")
        ", abstract at design.txt l.8-23).  This is a port of the November 2016 code
        (index.html:52-53; the page was last updated on July 14, 2018) to CHICKEN Scheme 6:
        the modules "
        (code/inline "(aux hakaru10 random)") " (the random-1.0.1.1 "
        (code/inline "StdGen") " of GHC, so that upstream's recorded runs replay bit for bit), "
        (code/inline "(aux hakaru10 distribution)") " (Distribution.hs), "
        (code/inline "(aux hakaru10 util)") " (Util.hs), "
        (code/inline "(aux hakaru10 engine)") " (Metropolis.hs and Memory.hs, and the
        internals of Syntax.hs), "
        (code/inline "(aux hakaru10 exact)") " (new: exact enumeration, a forward sampler and
        a reference evaluator, the oracles of the tests) and the public "
        (code/inline "(aux hakaru10)") " (Syntax.hs).  Every definition carries a "
        (code/inline "cf.") " comment naming the upstream lines it transcribes, every
        departure a numbered " (code/inline "deviation") " comment (the register is in the
        section on findings below).  Citations read " (code/inline "M:404-419")
        " for Metropolis.hs lines 404-419, and likewise S Syntax.hs, Mem Memory.hs, D
        Distribution.hs, U Util.hs, E Examples1.hs, CB ColoredBalls.hs, B1 Birds1.hs, BE
        Bench.hs, W index.html, BMP BirdMigrationProblem.txt and P the paper's text
        (design.txt), all from "
        (cite/a "https://okmij.org/ftp/kakuritu/Hakaru10/" "okmij.org/ftp/kakuritu/Hakaru10")
        ", and R the System/Random.hs of "
        (cite/a "https://hackage-content.haskell.org/package/random-1.0.1.1/src/System/Random.hs"
                "random-1.0.1.1")
        " (hackage, 567 lines), whose StdGen GHC used for upstream's recorded runs.")
     (p "In Scheme a model is a thunk that " (i "performs") " three operations, "
        (code/inline "dist") ", " (code/inline "diracN") " and " (code/inline "if_")
        ", through the native " (code/inline "shift") " of "
        (code/inline "(chicken continuation)") ".  What they mean is up to the "
        (i "handler") " that runs the model under the matching " (code/inline "reset")
        ": the same grass model below is sampled by incremental Metropolis-Hastings ("
        (code/inline "mcmC") "), enumerated exactly into a Hansei search tree ("
        (code/inline "exact-posterior") ") and sampled forward with likelihood weights ("
        (code/inline "likelihood-weighting") ").")
     ,@(shown 'nnot 'nor 'grass)
     (code/scheme (((mcmC 10 grass) ,(mcmC 10 grass))
                   ((exact-posterior grass) ,(exact-posterior grass))
                   ((likelihood-weighting 2000 grass) ,(likelihood-weighting 2000 grass))))

     (structure/section "Hakaru10 by example")
     (p "The tutorial of P §2.1 (l.202-327), with each Haskell model next to its Scheme
        transcription in the cases of the replay and laws suites.  Haskell's "
        (code/inline "do") " becomes " (code/inline "let*") " and " (code/inline "return e")
        " the tail expression; a model is defined with " (code/inline "define-model")
        " (a thunk) or written in place with " (code/inline "model") ".  Quoting P l.204-219:")
     (cite/quote
       "Oleg Kiselyov"
       "The first, elementary program is pbern = dist bern 0.4 whose inferred type is Model
       Bool. The program represents the model consisting of a single Boolean ('Bernoulli')
       random variable, whose distribution is True with the probability 40% and False with
       the probability 60%. [...] The function mcmC, the interpreter of probabilistic
       programs, [...] performs the MH inference on a model and returns the list of samples
       from the model's distribution. In particular, mcmC 10 pbern produces a list of 10
       booleans, which indeed contains 4 True and 6 False values.")
     ,@(shown 'pbern* 'pnorm 'pdep1)
     (p "The Dirac distribution is the unit of model composition (P l.255-261, §4.3
        l.803-808), for every model " (m p) " and expression " (m e) ":")
     (displaymath (m "do {x ← p; diracN x} ≡ p,  do {x ← diracN e; p} ≡ let x = e in p"))
     (p "and independent variables marginalize (P l.273-281): " (code/inline "pmarg")
        " has the distribution of " (code/inline "pnorm") ".  The hierarchical model of Hur
        et al. [10, Fig. 3] (P l.283-300), on which STAN infers a wrong distribution, is the
        10th iterate of a model transformer; in Scheme, where models are values, it is a "
        (code/inline "fold") ":")
     ,@(shown 'deeper 'phierN)

     (structure/section "Branching models")
     (p "Quoting P §2.2 l.330-347:")
     (cite/quote
       "Oleg Kiselyov"
       "Hakaru10 can express not only 'straight-line' but also branching models. An example
       is a simple mixture model [mixng] whose distribution is the mixture of normal and gamma
       distributions, with the random variable x determining the proportion of the mixture.
       [...] The mixng program is the first betraying Model being not quite a monad. The
       variable x is not actually of the type Double, as one might have thought. [...] As
       should be apparent from the use of Applicative operator <$>, which is just fmap,
       Model a is something like M (A a) where M is a monad but A is an applicative.")
     (p "In Scheme the applicative is " (code/inline "lift") " (with " (code/inline "letlift")
        ", " (code/inline "pair") ", " (code/inline "collect") " and the "
        (code/inline "sexp+") " family for the Num instance, S:99-135), and the branch is "
        (code/inline "if_") ", whose test must be a boolean stochastic expression:")
     ,@(shown 'mixng 'mixng′*)
     (p "Both should give the same distribution, as should the interleaving of 2500 samples
        of each component (P l.361-366; replay case X12).")

     (structure/section "Conditioning")
     (p "Quoting P §2.3 l.393-398 and l.418-421:")
     (cite/quote
       "Oleg Kiselyov"
       "As biased coin code demonstrates, in Hakaru10 conditioning may only be applied to
       distributions. One may think that (c1 `condition` bern) creates a new distribution out
       of bern, with the singular value c1. Conditioning on arbitrary boolean formulas is
       fraught with peril, both theoretical and practical, degenerating MCMC algorithm into
       the inefficient rejection sampling. [...] Here, we attempted to condition bern on the
       random choice within the model. This is not allowed: observations must be external to
       the model. In [20], this semantic condition was a merely a coding convention, whose
       violation manifested as a run-time exception. In Hakaru10, the violation is a type
       error.")
     ,@(shown 'biased-coin)
     (p "Why distributions: the temperature model of P l.653-673 observes 25 from "
        (code/inline "normal tempr 0.1") ", an event of probability zero; what can be asked
        is how likely 25 is under that distribution, its density, which the observed node
        contributes to the trace's log-likelihood.  Scheme has no static types, so the
        static discipline of Syntax.hs becomes run-time checks (deviation 9, in the style of
        Ścibior et al. [20]), each an " (code/inline "(exn hakaru10)") " condition whose "
        (code/inline "kind") " property says what was violated (case "
        (code/inline "test/language/errors") "):")
     (ul (li "observations are external (S:38-42, S:174-193): " (code/inline "condition")
             " of a stochastic expression raises " (code/inline "conditioning") ";")
         (li "only resampleable distributions are conditioned, never twice (S:185-193,
             E:180-186): " (code/inline "conditioning") ";")
         (li "SExp is not a monad (S:118, P l.739-747): " (code/inline "dist") ", "
             (code/inline "diracN") " and " (code/inline "if_") " outside a model or inside a
             lifted function raise " (code/inline "construction") ";")
         (li "a model is not a value (S:82): a model (a thunk) in an argument position raises "
             (code/inline "construction") ", " (code/inline "constant") " passes a procedure
             as a value;")
         (li "the test of " (code/inline "if_") " is " (code/inline "SExp Bool")
             " (S:275): a non-boolean raises " (code/inline "branch") ";")
         (li "lexical scoping, implicit upstream (deviation 10): a node of another chain raises "
             (code/inline "construction") ", a node escaping its branch " (code/inline "branch")
             ";")
         (li "the static kind of a node (M:46-60; deviation 10): a constructor that changes
             kind raises " (code/inline "construction") "."))
     (p "The conditioned grass model of P §1 (l.73-89) and its posterior P(rain | wet) =
        0.4684714427203697 (E:322) close the tutorial; see the first figure above.")

     (structure/section "Implementation")
     (p "Quoting P §4.1 l.723-747:")
     (cite/quote
       "Oleg Kiselyov"
       "Hakaru10 represents the trace -- random variables of the model and their
       dependencies -- as a directed acyclic graph (DAG). Each node (vertex) in the graph
       stands for one random variable (whose value can be sampled and resampled) or an
       observed variable, whose value cannot be resampled. There are also computational
       nodes, representing 'samples' from the dirac distribution. [...] A Hakaru10 model has
       the type "
       (code/inline "type Model a = MCMCM (SExp a)")
       " It is a computation that constructs the trace graph. MCMCM is a monad, but SExp is not. It is an applicative [13]: it lets us
       construct new graph nodes, without looking at their values. [...] The fact that SExp
       is not a monad is significant: the current node values cannot influence the graph
       construction. Therefore, after the graph is built, its structure does not change. All
       dependencies among nodes can be computed once and for all.")
     (p "In " (code/inline "(aux hakaru10 engine)") " the trace is a " (code/inline "chain")
        ", a vector of " (code/inline "node") " records indexed by creation order (the address,
        replacing Memory.hs); a node's dynamic state is an immutable "
        (code/inline "nstate") " record (value, log-likelihood, distribution, timestamp)
        replaced as a whole, as " (code/inline "Mem.modify_ref") " does.  "
        (code/inline "Model a = MCMCM (SExp a)") ", a State monad threading the chain, is a
        thunk run under the " (code/inline "trace-builder") " handler, which holds the chain;
        an SExp is a node, an " (code/inline "nt") " (leaves and a function: S:85-116's "
        (code/inline "NT") ") or any other value (" (code/inline "Val") ").  For the grass
        model the graph has the three nodes of P's Fig. 1 (case "
        (code/inline "test/language/graph/grass") ").")
     (p "Incremental recomputation (P §4.2 l.765-800): resampling a node enqueues its
        dependents; " (code/inline "propagate!") " (M:293-317) pops the queue in increasing
        address order, a binary heap, because "
        (cite/quote
          "Oleg Kiselyov"
          "The fact that Hakaru10 models are acyclic and Hakaru10 programs are declarative
          (with no mutations) lets us avoid the topological sort and rely on the 'creation
          times' of trace nodes. A node can only depend on those constructed before it.")
        "Creation order is therefore load-bearing, and Scheme leaves the order of procedure
        arguments unspecified: models create nodes in sequence (plan C18; see the embedding
        notes).")
     (p "The Dirac distribution (P §4.3 l.801-843): a Dirac node is a pure computation,
        recomputed whenever a parent changes, its log-likelihood staying 0, so a proposal is
        extended to all dependent Dirac nodes and the unit laws hold by construction
        (laws suite).  " (code/inline "diracN") " of a node answers the node itself, shared
        (S:164-168).")
     (p "Branching (P §4.4 l.844-869): "
        (cite/quote
          "Oleg Kiselyov"
          "Hakaru10 avoids any modifications to the graph structure during the trace update.
          It compiles both if-branches when constructing the initial graph. [...] Updating
          the nodes in the inactive branch, whose values will be ultimately ignored, is not
          good for performance. Therefore, we mark the nodes in the non-current branch as
          inactive, and delay their updates until they are activated. Thus each node, along
          with its current value, LL and the distribution also keeps the inactivation count.")
        (code/inline "ifnode!") " (M:641-686) builds the entry (the test, always a new Dirac
        node), the inactive branch first, deactivates its nodes ("
        (code/inline "deactivate!") ", M:696-717, which takes the observed nodes' LL back),
        the active branch, and the exit node, whose value is the active branch's result ("
        (code/inline "update-exit") ", M:778-794).  When the entry's value changes, "
        (code/inline "update-entry") " (M:760-775) moves the counts and the observed LL
        between the branches and replays the updates delayed while a node was inactive.")
     (p "One MH step, " (code/inline "transit!") " (M:908-945): pick an active resampleable
        node uniformly (M:956-977), resample it from its own distribution, propagate, and
        accept with the log ratio of M:929-934, where " (m Δ) " is the change of the trace's
        log-likelihood accumulated by the updates, " (m S) " the number of resampleable
        nodes and " (m c) " and " (m c′) " the change of their active count before and after
        the step (the derivation is the note M:1039-1089):")
     (displaymath (m (log α = Δ − |(| fwd − bwd |)| + log)) (frac (m (S + c)) (m (S + c′))))
     (p "A rejected step restores the whole state from a journal of undo thunks, except the
        random generator (M:904-906).  The engine's source:")
     (code/scheme/file "../aux.hakaru10.engine.scm")

     (structure/section "Embedding notes")
     (ul (li (b "Models are thunks.") "  " (code/inline "(model body ...)") " is "
             (code/inline "(τ body ...)") "; running a model again under the trace builder
             makes fresh nodes, so there is no accidental sharing (P l.175-179) and sharing
             comes only from binding a name (S:53-73).  Models compose by calling one another
             ("
             (code/inline "((phierN′ 9 x 1.))") " in " (code/inline "condhier")
             ") and are values (" (code/inline "phierN") " folds a model transformer).")
         (li (b "Evaluation order.") "  Node creation order is the topological order of
             propagation and fixes the random draws of a replay: create nodes with "
             (code/inline "let*") ", " (code/inline "fold") ", " (code/inline "map-in-order")
             ", " (code/inline "for-each") " or a named " (code/inline "let") ", never in
             argument positions nor inside " (code/inline "map") " (CHICKEN's "
             (code/inline "map") " conses " (code/inline "(p x)") " onto the recursive call in
             unspecified order; srfi-1's " (code/inline "list-tabulate") " calls from the
             last index down).  " (code/inline "letlift") " and " (code/inline "memo/finite")
             " evaluate in order.")
         (li (b "Three operations and their handlers") " (PLAN-IDIOM C25-C27).  "
             (code/inline "(dist d arg ...)") " performs " (code/inline "(dist d args)")
             ", " (code/inline "(diracN e)") " performs " (code/inline "(diracN e)")
             " and " (code/inline "(if_* test th el)") " performs "
             (code/inline "(if test th el)") " and calls the thunk it is resumed with, so the
             branch runs in the model's own context, under the same "
             (code/inline "reset") ".  " (code/inline "condition") " is not an operation: it
             makes an observed distribution, which " (code/inline "dist") " performs; "
             (code/inline "lift") " is not one either: it is pure, and under a handler where
             stochastic expressions are plain values it evaluates at once.  "
             (code/inline "handle-model") " installs a handler "
             (code/inline "(λ (request resume) ...)") " under a native "
             (code/inline "reset") ", by trampoline: a one-shot handler (the trace builder,
             the forward sampler) runs in constant stack, a multi-shot one (exact
             enumeration, " (code/inline "count-paths") " in case "
             (code/inline "test/language/a-handler-of-your-own") ") resumes as often as it
             likes.  Hansei's " (code/inline "probcc-reify") " has the same shape over aux's
             delimited continuations; only Hansei's data-level tools ("
             (code/inline "probcc-explore") ", " (code/inline "probcc-normalize")
             ") are shared, so aux's " (code/inline "resetcc") " and the native "
             (code/inline "reset") " never meet in one control context.")
         (li (b "Multi-shot purity") " (C28).  A model must not mutate, across an operation,
             state that outlives it: exact enumeration resumes the continuation once per
             support point, and the siblings would share the mutation.  For instance "
             ,@(shown 'impure)
             " answers 2 on every run under " (code/inline "mcmC") " but not under "
             (code/inline "exact-posterior") ":"
             (code/scheme (((mcmC 5 impure) ,(mcmC 5 impure))
                           ((exact-posterior impure) ,(exact-posterior impure))))
             (code/inline "fold") ", "
             (code/inline "map-in-order") ", named " (code/inline "let") " and "
             (code/inline "memo/finite") " (whose table is built after its last operation) are
             safe.")
         (li (b "The static discipline is the builder's") " (C29).  Under the value handlers
             no node exists, so they cannot tell an observation computed inside the model
             from an external one; " (code/inline "exact-posterior") " therefore builds the
             model once with the trace builder before enumerating it.")
         (li (b (code/inline "condition") ".") "  " (code/inline "(chicken condition)")
             " exports a procedure of the same name: import it as "
             (code/inline "(except (chicken condition) condition)") ", as this file does.")
         (li (b "Stochastic memoization") " (P §3 l.531-544, footnote 5 l.566-569, CB:41-44)
             becomes pre-allocation: " (code/inline "(memo/finite keys f)") " calls "
             (code/inline "f") " eagerly for every key, in order, before the "
             (code/inline "if_") "s that use it, so its nodes belong to no branch.")
         (li (b "Sampling.") "  " (code/inline "mcmC") ", " (code/inline "mcmC-fold")
             " (constant memory) and " (code/inline "mcmC§") " (an " (code/inline "(aux stream)")
             " stream: " (code/inline "take§") ", " (code/inline "§->list") ", "
             (code/inline "car§") ", " (code/inline "cdr§") ") take the keys "
             (code/inline "#:seed") " (17, as M:858), " (code/inline "#:rng")
             " (an rng source, used as is), " (code/inline "#:semantics") " ("
             (code/inline "'upstream") " or " (code/inline "'extended")
             ", see the semantics suite) and " (code/inline "#:check")
             " (the reference evaluator on every step, or on the first n).  There is no
             burn-in nor thinning: " (code/inline "drop") " the samples, as upstream does."))

     (structure/section "Findings and deviations")
     (p "The deviations, each tagged in the source with its number:")
     (ol (li "the exit's update returns the node unchanged when neither the entry nor the
             active result is newer, where upstream fails (M:788-792; case S20 "
             (code/inline "if/shared-inactive") ");")
         (li "the gamma density uses the scale form; upstream's formula matches no
             parameterization (D:298-304);")
         (li "the gamma sampler with a shape below 1 applies the scale once, upstream twice
             (D:279-282);")
         (li "a Poisson sample's log-likelihood is its log-density, upstream returns the
             probability (D:379);")
         (li "a categorical walk exhausted by rounding returns the last entry; upstream has no
             case for the empty list (D:99-102);")
         (li "with no active resampleable node a step is a no-op; upstream loops or crashes
             (M:962-966; case L7);")
         (li "a model with no random node is a constant chain, a consequence of 6 (case L6);")
         (li "the opt-in " (code/inline "'extended") " semantics, which removes the
             reactivation bias of upstream's kernel (see the semantics suite);")
         (li "the static checks of Haskell's types become run-time errors (P l.153-155,
             l.419-421), among them the boolean test of " (code/inline "if_") " (S:275);")
         (li "extra checks with no upstream counterpart: branch escape, owner chain, kind
             stability (M:46-60), the parameter checks of beta, laplace and dirichlet
             (D:318-328, D:384-391, D:407-411), and a negative sample count, where upstream
             retries the build forever or answers an infinite list (M:885-891, M:897-898;
             case L5);")
         (li "strict propagation of a deferred value: the value of an inactive Dirac node
             whose computation raises is deferred, as upstream's lazy values are (M:99,
             M:120-129), but a function applied to it in an inactive branch is not called, so
             a strict reader of the result raises the deferred error where upstream computes
             a value that ignores it (S:100-116; case B6)."))
     (p "Kept as upstream, with a comment in the source: the reactivation bias in "
        (code/inline "'upstream") " mode (M:1159-1165), the categorical duplicate-key
        asymmetry (the sampler sums, the density takes the first match, D:98), beta raising
        an error outside [0, 1] (D:331-337), NaN on deactivating an observed node of LL
        -inf, a NaN ratio drawing u and rejecting, and the empty chain when the initial
        retries run out (M:868).")
     (p "Upstream's own records that this port does not reproduce, because they come from
        older revisions of the engine: tac1-3's 620/523/532 (S:346-368; the three chains are
        identical here: tac2 and tac3 as S:370-371 says they should be, and tac1 too, since a
        constant Dirac node draws nothing, X16), phier_kl's
        0.7863/0.2362/0.03442/0.002157 (E:76-85; the transcription gives 0.78688 and 0.23811
        for the first two), Bench's 1e4 line (BE:28-29, -3.151396208304257 and 54.17557680794696;
        -3.151835923727823 and 54.168535367816084 here and in the transcription), and the older mixng KLs 2.276e-2
        and 1.356e-2 (E:153-162).  Likewise, presumably from older revisions of the engine:
        the sums of 1000 samples of nice_prog_mult_conditions, 499.799988743311 and
        257.2928205920028 (S:297-301), are 502.706370138924 and 252.485601387305 here, although the chain of "
        (code/inline "(biased-coin #t #f)") ", identical to the first one's, replays E:208-210
        bit for bit (X11); niceex1's 7 samples are \"All True\" (S:288-289), here "
        (code/inline "(#t #t #t #t #f #f #f)") "; pcond3's first record (1,5011), (20,2507),
        (21,2482) (E:135), next to the \"new\" one that X6 replays; the \"incremental\"
        moments of test_multiple_conditions13, (0.2445212550806874, 3.651432172401049e-2)
        (E:219-220, commented out upstream, of a prog_multiple_conditions defined nowhere),
        here (0.251523121569678, 0.0379368207086475) for " (code/inline "(biased-coin #f #f)")
        "; and exgrass/exgrass' of the
        low-level node API, not ported, whose record is [0.9, 0.9, 0.9, 0.9, 0.8, 0.1, 0.1,
        0.1, 0.1, 0.8] (M:808-809, M:821-822), where their transcription into models (rain'
        and sprinkler' as " (code/inline "diracN") ") gives (0.9 0.9 0.9 0.8 0.8 0.1 0.1 0.8
        0.8 0.1) with the default kernel.  The \"original Metropolis\" moments of E:207-208
        and E:217-218 are those of the old code that M:1174-1415 keeps commented out \"for
        reference\", which is not ported.
        The records E:94 (5809), E:102 (2026), P l.217-218 (4 True, for E:10-12) and E:157
        (1.1301558326703463e-2) were made with the always-flip kernel of D:64-70, commented
        out upstream and kept here as " (code/inline "bern/flip") ".")
     (p "Paper errata: P l.695 gives the LL of x = 0.1 under N(0, 1) as -0.616, it is
        -0.9239385332046727; P l.363 writes " (code/inline "dist gamma (1/3)") " for "
        (code/inline "gamma 3 (1/3)") " (E:144); P l.655-658 lacks a " (code/inline "dist")
        "; P l.261 says diracN \"has the alias return\", but " (code/inline "return")
        " creates no node (S:197, S:211-214): the two agree only through the unit laws.")

     (structure/section "Related work")
     (p "From P §5 (l.870-902): Figaro [18], an embedded DSL in Scala that also builds a trace
        graph updated by MH, apparently without incremental evaluation; Infer.NET [15],
        which builds a graph but relies on expectation propagation; Ścibior et al. [20],
        Haskell and MCMC but a monad, so the construction is too dynamic for static
        conditioning checks (and it can express cyclic models); Anglican [22] and more
        advanced MCMC algorithms; Yang et al. [23], incremental MH by generating code from
        the initial trace; Hur et al. [10], a provably correct MH for an imperative
        language, whose examples (Fig. 2 and 3) are mixng and phier here; their Fig. 1,
        start_example (E:324-336, commented out upstream), conditions on the boolean formula "
        (code/inline "observe (x || y)") ", which Hakaru10 forbids by design (P l.393-398,
        S:43-49), and is not ported.  Hakaru10 is a rewrite of the original Hakaru [24],
        fixing the problems of the lightweight implementation of Wingate et al. [21] pointed
        out in [11] (P §1 l.137-143, 175-183): the unit law of Dirac, accidental sharing of
        random primitives, and conditioning inside branches.")
     (p "The exact oracle of these tests reuses "
        (cite/a "https://okmij.org/ftp/kakuritu/Hansei.html"
                "Hansei, the embedded probabilistic DSL of Kiselyov and Shan")
        " (aux.hansei.scm): its lazy search trees and their exploration.  The public module:")
     (code/scheme/file "../aux.hakaru10.scm")))

  ((test/language/graph/grass _)
   (⊦ equal?
      '((nodes . 3) (resampleable . 2) (observed . 1) (dirac . 0) (inactive . 0) (entries . 0))
      (model-graph grass))
   `(doc (p "L1: the trace graph of grass is P's Fig. 1 (P §4.1 l.734-736): two
            resampleable nodes, rain and sprinkler, and the observed wet grass; "
            (code/inline "model-graph") " counts what the debug print of M:860-867 shows.")))

  ((test/language/graph/regression _)
   (⊦ equal?
      '((nodes . 512) (resampleable . 11) (observed . 500) (dirac . 1) (inactive . 0)
        (entries . 0))
      (model-graph (regression (regression-data 5 500))))
   `(doc (p "L2: the Bayesian linear regression of P §3 (l.461-504) with five parameters and
            500 points: \"its graph has 511 vertices (there are five hundred observations and
            five parameters)\" (P l.493-495), here 512 with the Dirac node of the result, "
            (code/inline "collect w") ".  The noise's " (code/inline "(1/) <$>") " is a lifted
            expression, not a node.")
         ,@(shown 'normals 'dot 'regression)
         (code/lang "haskell" "-- normals means std
-- returns several independently distributed normal random variables
-- whose means are given by the list means, with the same std
normals :: [SExp Double] -> SExp Double -> MCMCM [SExp Double]
normals means std = mapM (\\m -> dist normal m std) means


dot :: [Double] -> [SExp Double] -> SExp Double
dot xs ays = foldr1 (+) $ zipWith (\\x ay -> (*x) <$> ay) xs ays

collect :: [SExp a] -> SExp [a]
collect = sequenceA")
         (p "(Birds1.hs:103-114.)  The model, as printed in P l.471-486 (re-indented, and with
            the underscores the PDF's text extraction drops restored):")
         (code/lang "haskell" "type RegrDatum = ([Double],Double) -- Xs and Y
model :: [RegrDatum] -> Model [Double]
model xsy = do
  let mu = replicate dimK 0
  w_mean   <- normals mu 2
  w        <- normals w_mean 1
  noise_sd <- (1/) <$> dist gamma 0.5 0.5
  let make_cond (xs,y) = dist (y `condition` normal) (dot xs w) noise_sd
  mapM make_cond xsy
  return $ collect w")))

  ((test/language/graph/colored-balls _)
   (let1 (graph (model-graph (cballs-model (make-list 10 'blue))))
     (⊦ equal? '(2114 89 1994)
        (map (λ (key) (alist-ref key graph)) '(nodes resampleable inactive))))
   `(doc (p "L3: the colored balls of BLOG (CB:1-13, P §3 l.505-616) with 8 possible balls
            and 10 blue draws: 2114 nodes, 89 resampleable (8 colors, the number of balls and
            one drawn ball per draw and per number of balls, 8 + 1 + 10 × 8), and 1994 of them
            inactive in the initial trace at seed 17 (prototype regression value).")
         ,@(shown 'opposite-color 'observed-color 'max-balls 'balls-prior 'balls-observe
                        'cballs-model)
         (code/lang "haskell" "-- Submodel: Given balls color and the observations, perform conditioning
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
         (p "(ColoredBalls.hs:74-99.)  " (code/inline "if_ test $ m") " is " (code/inline "if_")
            " short of its else branch, which the " (code/inline "foldr") " supplies: a chain
            of nested " (code/inline "if_") "s, a named " (code/inline "let") " here.")))

  ((test/language/applicative _)
   (⊦ equal? (mcmC 50 ordered-pair*) (mcmC 50 ordered-pair))
   (⊦≠ (mcmC 50 swapped-pair) (mcmC 50 ordered-pair))
   (⊦ equal? '(-3. .25 2 .5 4 #f) (list (sexp- 3.) (sexp/ 4.) (sexp- 3 1) (sexp/ 1. 2.)
                                       (sexp-map add1 3) (sexp? 3)))
   (⊦ equal? '((-3.) (.25)) (list (mcmC 1 (model (sexp- 3.))) (mcmC 1 (model (sexp/ 4.)))))
   (⊦ = 4 (sexp-map (λ (f v) (f v)) (constant add1) 3))
   (for-each (Λ ((,x ,x? ,nx? ,neg ,inv ,diff ,quot ,mapped ,applied))
               (⊦ equal? (list #t #t (- x) (/ x) (- 10. x) (/ x 2.) (- x) (- x))
                  (list x? nx? neg inv diff quot mapped applied)))
             (mcmC 20 (model (let* ((x (dist normal 0. 1.)))
                               (lift list x (sexp? x) (sexp? (sexp- x)) (sexp- x) (sexp/ x)
                                     (sexp- 10. x) (sexp/ x 2.) (sexp-map - x)
                                     (sexp-map (λ (f v) (f v)) (constant -) x))))))
   `(doc (p "The rest of the applicative surface (S:99-135).  " (code/inline "letlift")
            " evaluates its e's left to right, so " (code/inline "ordered-pair")
            " creates its nodes in the order of " (code/inline "ordered-pair*")
            " and replays it sample for sample, while " (code/inline "swapped-pair")
            ", the same distribution with the nodes made in the other order, draws other
            samples: node creation order is observable (plan C18).")
         ,@(shown 'ordered-pair 'ordered-pair* 'swapped-pair)
         (p (code/inline "sexp-") " and " (code/inline "sexp/") " of one argument are "
            (code/inline "negate") " and " (code/inline "recip") " (S:125, S:131), of two "
            (code/inline "-") " and " (code/inline "/") " (S:123, S:130); "
            (code/inline "sexp-map") " is " (code/inline "fmap") " (S:99-102), evaluated now
            over plain values; " (code/inline "sexp?") " holds of nodes and lifted
            expressions only (S:85-89); and " (code/inline "constant") " (S:105 "
            (code/inline "pure") ") passes a procedure as a value.")))

  ((test/language/sharing _)
   (⊦ equal? '(#t) (mcmC 1 (model (let* ((x (dist normal 0. 1.))) (eq? x (diracN x))))))
   (⊦ equal? '((nodes . 1) (resampleable . 0) (observed . 0) (dirac . 1) (inactive . 0)
               (entries . 0))
      (model-graph (model (diracN 3.))))
   (⊦ = 2 (alist-ref 'nodes (model-graph (model (let* ((x (dist normal 0. 1.)))
                                                   (diracN (sexp+ x x)))))))
   (receive (ch result) (build-chain (model (let* ((x (dist normal 0. 1.)))
                                              (diracN (lift + x x x))))
                                     (make-stdgen-source 17) 'upstream)
     (⊦ = 1 (vector-length (node-parents result))))
   `(doc (p "L4: " (code/inline "diracN") " of a node answers the node itself, shared rather
            than copied (S:161-169):")
         (code/lang "haskell" "-- NB: if x is just a NoderRef, (diracN x) returns this reference (shares)
-- rather than creating a copy of it!
diracN :: Show a => SExp a -> Model a
diracN (Val x)  = fmap NRef $ create_node Nothing (diracS x)
diracN x@NRef{} = return x
diracN x = fmap NRef $ with_node_ctor x diracS")
         (p "while " (code/inline "diracN") " of a value makes a constant Dirac node and of a
            lifted expression a Dirac node depending on its leaves, deduplicated: "
            (code/inline "(lift + x x x)") " has the single parent x (S:99-116; the engine's "
            (code/inline "nt") " keeps the leaves as a vector of distinct nodes).")))

  ((test/language/errors _)
   (define captured #f)
   (define cases
     `((condition-on-a-random-choice
         conditioning
         ,(τ (mcmC 3 (model (let* ((b (dist bern .5)))
                              (dist (condition b bern) .4)
                              b)))))
       (condition-twice conditioning ,(τ (condition #t (condition #t (bern .5)))))
       (condition-twice-applied
         conditioning
         ,(τ (mcmC 3 (model (let* ((b (dist beta 1. 1.)))
                              (dist (condition #t (condition #f bern)) b)
                              b)))))
       (dist-outside-a-model construction ,(τ (dist bern .5)))
       (diracN-outside-a-model construction ,(τ (diracN 1.)))
       (if_-outside-a-model construction ,(τ (if_ #t 1 2)))
       (dist-in-a-lifted-function
         construction
         ,(τ (mcmC 3 (model (let* ((x (dist normal 0. 1.)))
                              (lift (λ (xv) (dist normal xv 1.)) x))))))
       (dist-in-a-lifted-function-during-mcmC
         construction
         ,(τ (mcmC 200 (model (let* ((x (dist bern .5)))
                                (dist bern (lift (λ (xv) (if xv (begin (dist bern .5) .5) .5))
                                                 x)))))))
       (a-model-as-a-dist-argument construction ,(τ (mcmC 3 (model (dist normal pnorm 1.)))))
       (a-model-as-the-result construction ,(τ (mcmC 3 (model pnorm))))
       (not-a-model construction ,(τ (mcmC 3 42)))
       (not-a-model/exact construction ,(τ (exact-posterior 42)))
       (a-container-of-sexps-as-the-result
         construction
         ,(τ (mcmC 3 (model (let* ((x (dist bern .5))) (list x))))))
       (sexps-nested-in-the-result
         construction
         ,(τ (mcmC 3 (model (let* ((x (dist bern .5)) (y (dist bern .5))) (cons 1 (vector x y)))))))
       (an-sexp-inside-a-diracN-value
         construction
         ,(τ (mcmC 3 (model (let* ((x (dist bern .5))) (diracN (vector 1 (list x))))))))
       (ctor-not-a-distribution
         construction
         ,(τ (mcmC 3 (model (let* ((x (dist normal 0. 1.))) (dist (λ (m) 42) x))))))
       (negative-limit construction ,(τ (mcmC -1 (model (dist bern .5)))))
       (negative-limit/fold construction ,(τ (mcmC-fold cons '() -1 (model (dist bern .5)))))
       (negative-limit/stream construction ,(τ (mcmC§ (model (dist bern .5)) #:retries -1)))
       (check-not-a-boolean-or-a-count
         construction
         ,(τ (mcmC 3 (model (dist bern .5)) #:check 'yes)))
       (check-a-negative-count construction ,(τ (mcmC 3 (model (dist bern .5)) #:check -1)))
       (non-boolean-test branch ,(τ (mcmC 3 (model (if_ 1 1 2)))))
       (non-boolean-test-at-update
         branch
         ,(τ (mcmC 200 (model (let* ((x (dist categorical '((#t . .5) (#f . .25) (0 . .25)))))
                                (if_ (lift identity x) 1 2))))))
       (a-node-of-a-previous-chain
         construction
         ,(τ (mcmC 3 (model (let* ((x (dist normal 0. 1.))) (set! captured x) x)))
             (mcmC 3 (model (dist normal captured 1.)))))
       (branch-escape
         branch
         ,(τ (mcmC 3 (model (let* ((c (dist bern .5))
                                   (leak #f))
                              (if_ c (begin (set! leak (dist normal 0. 1.)) leak) 0.)
                              (dist normal leak 1.))))))
       (kind-change
         construction
         ,(τ (mcmC 200 (model (let* ((x (dist bern .5)))
                                (dist (λ (xv) (if xv (bern .3) (condition #t (bern .3)))) x))))))
       (memo/finite-unknown-key
         construction
         ,(τ (mcmC 3 (model (let* ((f (memo/finite '(1 2) (λ (k) (dist normal k 1.)))))
                              (f 3))))))
       (sampleable-entry
         internal
         ,(τ (let* ((ch (new-chain (make-stdgen-source 17) 'upstream))
                    (c (run-model! ch (model (dist bern .5)))))
               (ifnode! ch c (τ (to-node! ch 1)) (τ (to-node! ch 2))))))))
   (for-each (Λ ((,name ,kind ,thunk)) (⊦ equal? (list name kind) (list name (hakaru10-kind thunk))))
             cases)
   `(doc (p "L5: the static discipline of Syntax.hs as run-time errors (deviation 9; see the
            section on conditioning), each case naming the kind it raises.  Upstream's
            ill-typed examples are E:180-201:")
         (code/lang "haskell" "{- Type error
biased_coinX c1 c2 = do
  b <- dist beta 1 1
  dist (c1 `condition`  (c2 `condition` bern)) b
  dist (c2 `condition` bern) b
  return b
-}

{-
biased_coin_ill_typed c1 c2 = do
  b <- dist bern 0.5
  dist (c1 `condition` bern) b
  dist (c2 `condition` bern) b
  return b
-}

{-
biased_coin_ill_typed_too c1 c2 = do
  b <- dist bern 0.5
  dist (b  `condition` bern) 0.4
  return b
-}")
         (p "Without a counterpart in these examples: a Scheme container of SExps as the
            result, which Haskell's types reject and the engine would otherwise take for a
            constant (deviation 9), at any depth of pairs and vectors, and likewise inside the
            value of a " (code/inline "diracN") "; a distribution constructor that answers
            something other than a distribution (plan §7); a model that is not a thunk, for "
            (code/inline "mcmC") " and " (code/inline "exact-posterior")
            " alike (" (code/inline "check-model") "); a negative sample count, for each of "
            (code/inline "mcmC") ", " (code/inline "mcmC-fold") " and "
            (code/inline "mcmC§") " (deviation 10: upstream retries the build forever or
            answers an infinite list, M:885-891, M:897-898); and a " (code/inline "#:check")
            " that is neither a boolean nor a step count.")
         (p "The last case calls the engine's " (code/inline "ifnode!") " directly: \"Entry
            node for if should not be sampleable\" (M:643-647).  The " (code/inline "set!")
            "s are fine here, under the one-shot trace builder; models must not use them
            across operations (C28, embedding notes).")))

  ((test/language/constant-chain _)
   (⊦ equal? '(3 3 3 3 3) (mcmC 5 (model 3)))
   `(doc (p "L6, deviation 7: a model with no random node is a constant chain; upstream's
            step would pick from an empty list of resampleable nodes (M:962).")))

  ((test/language/no-active-resampleable _)
   (⊦ equal? (make-list 10 3) (mcmC 10 (model (if_ #f (dist normal 0. 1.) 3))))
   `(doc (p "L7, deviation 6: the only resampleable node is in the inactive branch, so no
            node can be proposed and every step is a no-op; upstream's "
            (code/inline "resample_trace") " retries forever (M:966).")))

  ((test/language/stream _)
   (let1 (s (mcmC§ pcond4))
     (⊦ equal? (mcmC 100 pcond4) (§->list (take§ 100 s)))
     (⊦ equal? (cadr (mcmC 2 pcond4)) (car§ (cdr§ s))))
   (⊦ equal? '() (mcmC§ (model (dist (condition 5. uniform) 0. 1.))))
   (let1 (rare (model (let* ((c (dist bern .1)))
                        (dist (condition #t bern) (lift (λ (cv) (if cv 1. 0.)) c))
                        c)))
     (⊦ equal? '() (mcmC 6 rare))
     (⊦ equal? (make-list 7 #t) (mcmC 7 rare))
     (⊦ equal? '() (mcmC§ rare #:retries 6))
     (⊦ equal? (make-list 3 #t) (§->list (take§ 3 (mcmC§ rare #:retries 7)))))
   `(doc (p "L8: " (code/inline "mcmC§") " is Haskell's lazy list of samples (M:897-906), an "
            (code/inline "(aux stream)") " stream made by " (code/inline "τ->§")
            " from the engine's generator: its first 100 elements are "
            (code/inline "(mcmC 100 pcond4)") ".  A model with no trace of finite
            log-likelihood gives the empty stream after " (code/inline "#:retries")
            " builds (1000 by default; upstream's " (code/inline "initial_step") " returns "
            (code/inline "[]") ", M:868, M:882-891).  The budget is exact, as upstream's
            \"repeat up to limit times\" (M:882-891): at seed 17 the model "
            (code/inline "rare") ", which observes " (code/inline "#t") " from "
            (code/inline "bern") " of 1 or 0 as its c is true or false, draws c false in its
            first six builds and true in the seventh, so six builds (" (code/inline "mcmC")
            "'s limit, or " (code/inline "#:retries") ") find no trace and seven do.")))

  ((test/language/fold _)
   (⊦ = (count-true (mcmC 2000 grass))
      (mcmC-fold (λ (rain n) (if rain (add1 n) n)) 0 2000 grass))
   (⊦ equal? (mcmC 100 pcond1 #:rng (make-stdgen-source 17)) (mcmC 100 pcond1))
   (⊦≠ (mcmC 100 pcond1 #:seed 18) (mcmC 100 pcond1))
   `(doc (p "L9: " (code/inline "mcmC-fold") " folds " (code/inline "(kons sample acc)")
            " over the samples in chain order, in constant memory, and agrees with a fold
            over " (code/inline "mcmC") "; " (code/inline "#:rng") " takes an rng source
            as is, " (code/inline "#:seed") " seeds a StdGen (17 by default, M:858).")))

  ((test/language/three-interpretations _)
   (let1 (exact (exact-posterior grass))
     (⊦ (equal-approx? 1e-12) '((#f . .5315285572796303) (#t . .4684714427203697)) exact)
     (⊦ (equal-approx? .02) exact (likelihood-weighting 20000 grass))
     (⊦ (equal-approx? .04) exact (proportions (mcmC 20000 grass))))
   `(doc (p "One model, three interpretations (PLAN-IDIOM C25): grass under exact
            enumeration (a multi-shot handler building a Hansei search tree, explored and
            normalized by " (code/inline "probcc-explore") " and "
            (code/inline "probcc-normalize") "), under likelihood weighting (a one-shot
            forward sampler) and under the incremental MH of the trace builder.  The exact
            posterior is upstream's \"theoretical\" 0.4684714427203697 (E:322); likelihood
            weighting agrees within 0.02, the Markov chain, autocorrelated, within 0.04.  The handler suite of hakaru10-exact.scm runs this check
            on ten models.")))

  ((test/language/a-handler-of-your-own _)
   (⊦ equal? '(4 4 3 4) (map count-paths (list grass pcond1 pcond4 ph2)))
   `(doc (p "A fourth interpretation, written in this file against the engine's protocol:
            the number of paths through a model whose choices have finite supports.  The
            handler resumes the model's continuation, captured by the native "
            (code/inline "shift") " of " (code/inline "(chicken continuation)")
            ", once per support point of each choice and adds up the answers; the return
            clause maps every completed run to 1.  Grass has the four paths of rain ×
            sprinkler, pcond1 the four of x × the branch's choice, pcond4 one for x = 1 and
            two for x = 2 (the observation resumes once), ph2 four.")
         ,@(shown 'count-paths)))

  ((test/language/procedure-values _)
   (⊦ (equal-approx? 1e-12) '((1 . .5) (2 . .5)) (exact-posterior procedure-choice))
   (⊦ (equal-approx? 1e-12) '((0 . .5) (1 . .5)) (exact-posterior procedure-constant))
   (⊦ equal? '((1 . 974) (2 . 1026)) (hist (mcmC 2000 procedure-choice)))
   (⊦ equal? '((0 . 1026) (1 . 974)) (hist (mcmC 2000 procedure-constant)))
   (⊦ equal? '(50 50) (list (length (mcmC 50 procedure-choice #:check #t))
                            (length (mcmC 50 procedure-constant #:check #t))))
   (⊦ equal? '((1 2) (0 1)) (list (map car (likelihood-weighting 100 procedure-choice))
                                  (map car (likelihood-weighting 100 procedure-constant))))
   (⊦ (equal-approx? 1e-12) (map (λ (v) (cons v (/ 1. 3.))) '(10 20 30))
      (exact-posterior procedure-built))
   (⊦ (equal-approx? 1e-12) '((#f . .5) (#t . .5)) (exact-posterior circular-value))
   (let1 (scales (model (let* ((k (dist uniformly '(1 2 3))))
                          (diracN (lift (λ (kv) (λ (x) (* kv x))) k)))))
     (⊦ equal? (map (λ (f) (f 10)) (mcmC 50 scales))
        (map (λ (f) (f 10)) (mcmC 50 scales #:check #t))))
   (for-each (λ (m)
               (⊦ equal? (mcmC 50 m) (mcmC 50 m #:check #t))
               (⊦ equal? (mcmC 50 m #:semantics 'extended)
                  (mcmC 50 m #:semantics 'extended #:check #t)))
             (list procedure-built circular-value))
   `(doc (p "L10: procedures as values.  A random choice may draw procedures, and "
            (code/inline "(diracN (constant car))") " makes a Dirac node of one.  Under the
            trace builder the node is the SExp; under the value handlers of exact enumeration
            and of the forward sampler, " (code/inline "resume") " boxes a procedure value
            with " (code/inline "constant") ", as " (code/inline "lift")
            " does, so that the model can pass it on to " (code/inline "lift")
            " (S:105-116 " (code/inline "Val f") ") rather than take it for a model
            (deviation 9).  Exact enumeration, the chain (with and without "
            (code/inline "#:check") ") and likelihood weighting agree; the counts at seed 17
            are regression values.")
         ,@(shown 'procedure-choice 'procedure-constant)
         (p "A lifted function may also answer a closure it builds, which "
            (code/inline "procedure-built") " keeps in a Dirac node: "
            (code/inline "#:check") " recomputes it as a fresh closure and takes two
            procedures for the same value (their consumers, which apply them, are checked
            themselves), as it does for a model that answers such closures.  A value may be circular: in " (code/inline "circular-value")
            " the Dirac node holds a circular list, which the trace builder's check for SExps
            inside a value (deviation 9) and the comparisons of " (code/inline "#:check")
            " walk to an end.  Both chains sample with " (code/inline "#:check")
            " what they sample without it, in both semantics, and agree with exact
            enumeration.")
         ,@(shown 'procedure-built 'circular-value))))

;; ---------------------------------------------------------------------------------------------
;; Suite hakaru10-replay: upstream's records, bit for bit (seed 17, 'upstream)
;; ---------------------------------------------------------------------------------------------

(define-suite hakaru10-replay

  ((doc r)
   `((structure/section "Replays")
     (p "Chains replayed bit-exactly against upstream's recorded runs (the comments of
        Examples1.hs, Syntax.hs and ColoredBalls.hs) and against the Python transcription of
        the engine (prototype regression values), with the random-1.0.1.1 "
        (code/inline "StdGen") " of " (code/inline "mkStdGen 17") " (M:858).  A replay
        holds only if the port draws exactly where upstream draws, in the same order,
        computes every Double expression in Haskell's association, and creates the nodes
        in the same order (plan §6.1, C18).  The triage order for a failing replay: the
        generator, the samplers' draw order, the order of the resampleable nodes (M:239,
        M:962), floating-point order, node creation order, (de)activation sums.")))

  ((test/replay/pdep _)
   (⊦ equal? (mcmC 100 (model (dist normal 11. .5))) (mcmC 100 pdep1))
   `(doc (p "X1, " (code/inline "pdep_run") ": literally the same samples (E:16-22; P
            l.248-249).")
         ,@(shown 'pnorm 'pdep1)
         (code/lang "haskell" "pnorm = dist normal 10 0.5

pdep1 = do
  x <- pnorm
  diracN (x+1)

-- Literally the same samples
pdep_run = mcmC 100 (dist normal 11 0.5) == mcmC 100 pdep1
-- True")))

  ((test/replay/pdepR _)
   (let1 (reference (mcmC 100 pnorm))
     (⊦ equal? reference (mcmC 100 pdepR))
     (⊦ equal? reference (mcmC 100 pdepR′))
     (⊦ < 10 (length (delete-duplicates reference))))
   `(doc (p "X2: the right unit law, sample for sample (E:24-32, P l.254-258): \"Even the
            sequences of samples for the two programs are identical.\"")
         ,@(shown 'pdepR 'pdepR′)
         (code/lang "haskell" "pdepR = do
  x <- pnorm
  diracN x

pdepR_run = mcmC 100 pdepR

pdepR' = do
  x <- pnorm
  return x")))

  ((test/replay/grass _)
   (⊦ = 9363 (count-true (mcmC 20000 grass)))
   `(doc (p "X3: 9363 rainy samples of 20000, 0.46815, upstream's \"0.46815 for 20000 -- with
            old resampling\" (E:321).")
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
nnot p False = 1

grass_run = let ns = 20000
                r  = mcmC ns grass
            in
             ((fromIntegral . length . filter id $ r) /
              (fromIntegral ns :: Double))
-- 0.46815 for 20000 -- with old resampling
-- 0.4684714427203697  -- theoretical")))

  ((test/replay/pcond1 _)
   (⊦ equal? '((10 . 246) (11 . 222) (20 . 240) (21 . 292)) (hist (mcmC 1000 pcond1)))
   (⊦ equal? '((10 . 2481) (11 . 2394) (20 . 2565) (21 . 2560)) (hist (mcmC 10000 pcond1)))
   `(doc (p "X4: both of upstream's records (E:112-114), of 1000 and 10000 samples (the
            first sums to 1000, though the code reads " (code/inline "mcmC 10000") ").")
         ,@(shown 'is1 'pcond1)
         (code/lang "haskell" "pcond1 = do
  x <- dist categorical (pure [(1,0.5), (2,0.5)])
  if_ ((==1) <$> x)
     (dist categorical (pure [(10,0.5), (11,0.5)]))
     (dist categorical (pure [(20,0.5), (21,0.5)]))

pcond1r = hist $ mcmC 10000 pcond1
-- fromList [(10,246),(11,222),(20,240),(21,292)]
-- fromList [(10,2481),(11,2394),(20,2565),(21,2560)]")))

  ((test/replay/pcond2 _)
   (⊦ equal? '((1 . 500) (20 . 214) (21 . 286)) (hist (mcmC 1000 pcond2)))
   (⊦ equal? '((1 . 5000) (20 . 2321) (21 . 2679)) (hist (mcmC 10000 pcond2)))
   `(doc (p "X5: both of upstream's records (E:122-124), of 1000 and 10000 samples (the
            first sums to 1000, though the code reads " (code/inline "mcmC 10000") ").")
         ,@(shown 'pcond2)
         (code/lang "haskell" "pcond2 = do
  x <- dist categorical (pure [(1,0.5), (2,0.5)])
  if_ ((==1) <$> x)
     (return x)
     (dist categorical (pure [(20,0.5), (21,0.5)]))

pcond2r = hist $ mcmC 10000 pcond2
-- fromList [(1,500),(20,214),(21,286)]
-- fromList [(1,5000),(20,2321),(21,2679)]")))

  ((test/replay/pcond3 _)
   (⊦ equal? '((1 . 5040) (20 . 2509) (21 . 2451)) (hist (mcmC 10000 pcond3)))
   `(doc (p "X6: upstream's \"new\" record (E:134-136).")
         ,@(shown 'pcond3)
         (code/lang "haskell" "pcond3 = do
  x <- dist categorical (pure [(1,0.5), (2,0.5)])
  if_ ((==1) <$> x)
     (return x)
     (do
       dist normal 0 1
       dist categorical (pure [(20,0.5), (21,0.5)]))

pcond3r = hist $ mcmC 10000 pcond3
-- fromList [(1,5011),(20,2507),(21,2482)]
-- fromList [(1,5040),(20,2509),(21,2451)] -- new")))

  ((test/replay/pcond4 _)
   (⊦ equal? '((1 . 648) (20 . 174) (21 . 178)) (hist (mcmC 1000 pcond4)))
   (⊦ equal? '((1 . 6697) (20 . 1641) (21 . 1662)) (hist (mcmC 10000 pcond4)))
   `(doc (p "X7: both of upstream's records (E:273-279), conditioning inside a branch.")
         ,@(shown 'pcond4)
         (code/lang "haskell" "pcond4 = do
  x <- dist categorical (pure [(1,0.5), (2,0.5)])
  if_ ((==1) <$> x)
     (return x)
     (do
       dist (True `condition` bern) 0.5
       dist categorical (pure [(20,0.5), (21,0.5)]))

_ = hist $ mcmC 1000 pcond4
{-
*Examples1> hist $ mcmC 1000 pcond4
fromList [(1,648),(20,174),(21,178)]
*Examples1> hist $ mcmC 10000 pcond4
fromList [(1,6697),(20,1641),(21,1662)]
-}
-- Exact:
--      [(0.166666666666666657, V 21); (0.166666666666666657, V 20);
--       (0.66666666666666663, V 1)])")))

  ((test/replay/pcorr _)
   (⊦ equal? '((#f . 419) (#t . 581)) (hist (mcmC 1000 (pcorr .1))))
   `(doc (p "X8: Yufei Cai's example (E:223-238; Figaro issue 548), a custom distribution
            conditioned on two random choices.")
         ,@(shown 'pcorr*)
         (code/lang "haskell" "pcorr pfuzz = do
  x <- dist bern 0.5
  y <- dist bern 0.5
  dist (True `condition` fuzzy2 pfuzz xor) x y
  return y
 where
   -- Creating a custom distribution
   fuzzy2 pfuzz op x y = let v = op x y in
     categorical [(v,pfuzz),(not v,1-pfuzz)]
   xor y x = not (x == y)

_ = hist $ mcmC 1000 (pcorr 0.1)
-- fromList [(False,419),(True,581)]")))

  ((test/replay/girl _)
   (⊦ = 77 (count-true (mcmC 100 girl)))
   `(doc (p "X9: the eldest daughter puzzle, \"77, without the bias\" (E:354-355).")
         ,@(shown 'girl)
         (code/lang "haskell" "girl = do
  girl1 <- dist bern 0.5
  girl2 <- dist bern 0.5
  dist (True `condition` fuzzy2 0.01 (||)) girl1 girl2
  return girl1
 where
   -- Creating a custom distribution
   fuzzy2 pfuzz op x y = let v = op x y in
     categorical [(v,1-pfuzz),(not v,pfuzz)]

_ = length . filter id $ mcmC 100 girl
-- 77, without the bias")))

  ((test/replay/post-bias _)
   (⊦ equal? (make-list 10 #t) (mcmC 10 (post-bias 1.)))
   (⊦ equal? (make-list 100 #t) (mcmC 100 (post-bias 1.)))
   `(doc (p "X10: \"All true\" of " (code/inline "mcmC 10") " (E:262-263) and \"all True
            samples\" of " (code/inline "mcmC 100") " (P l.435-437), both replayed: the
            observation in the branch not returned still weighs the coin.")
         ,@(shown 'post-bias)
         (code/lang "haskell" "post_bias c = do
  coin <- dist bern 0.5
  if_ coin (dist (c `condition` normal) 0 1)
           (dist (c `condition` normal) 100 1)
  return coin

_ = mcmC 10 (post_bias 1)
-- All true")))

  ((test/replay/biased-coin _)
   (⊦ equal? '(.49910027237934973 . .0497358737659099)
      (momenta (drop (mcmC 10000 (biased-coin #t #f)) 1000)))
   `(doc (p "X11: upstream's \"incremental\" record (E:203-210; P l.389-392: \"we estimate
            the average as 0.499\"), the posterior being beta 2 2 of mean 0.5 and variance
            0.05.")
         ,@(shown 'biased-coin)
         (code/lang "haskell" "biased_coin c1 c2 = do
  b <- dist beta 1 1
  dist (c1 `condition` bern) b
  dist (c2 `condition` bern) b
  return b")
         (code/lang "haskell" "biased_coin22 =
  momenta . drop 1000 $
   mcmC 10000 (biased_coin True False)
-- beta 2 2, av = 0.5, sigma2 = 5e-2
-- original Metropolis
-- (0.49433040918945736,5.013851472980666e-2)
-- incremental
-- (0.49910027237934973,4.97358737659099e-2)")))

  ((test/replay/mixng-interleaved _)
   (⊦ = 1.3896442111895212e-2
      (kl (tabulate .5 (mcmC 5000 mixng))
          (tabulate .5 (append (mcmC 2500 (model (dist normal 10. 2.)))
                               (mcmC 2500 (model (dist gamma 3. (/ 1. 3.))))))))
   `(doc (p "X12: the KL divergence of mixng against the two components' samples, upstream's
            \"now\" record (E:159-164; P l.361-364).")
         ,@(shown 'mixng)
         (code/lang "haskell" "mixng = do
  x <- dist normal 0 1
  if_ ((>0) <$> x)
           (dist normal 10 2)
           (dist gamma 3 (1/3))")
         (code/lang "haskell" "_  =
 let n = 5000 in kl (tabulate 0.5 (mcmC n mixng)) (tabulate 0.5 $ mcmC (n `div` 2) (dist normal 10 2) ++ mcmC (n `div` 2) (dist gamma 3 (1/3)))
{-
5000 samples 1.3563495645465825e-2
1.3896442111895212e-2 now
-}")))

  ((test/replay/phier _)
   (⊦ equal? '(3.4117556376053577 . 32.27027981263249) (momenta (mcmC 1000 (phierN 10))))
   `(doc (p "X13: Bench.hs's record for n = 10 and 1000 samples (BE:23-25).")
         ,@(shown 'deeper 'phierN)
         (code/lang "haskell" "phier n = 
  (iterate (\\m -> do {x<-m; dist normal x 3}) $ dist normal 0 1) !! n

main = do
  [n',nchain'] <- getArgs
  let n = read n' :: Int
  let nchain = read nchain' :: Integer
  print . momenta $ mcmC nchain (phier n)

{-
1000
(3.4117556376053577,32.27027981263249)")))

  ((test/replay/colored-balls _)
   (⊦ equal? '((1 . 4265) (2 . 2203) (3 . 1244) (4 . 805) (5 . 486) (6 . 291) (7 . 565) (8 . 141))
      (hist (mcmC 10000 (cballs-model (make-list 10 'blue)))))
   `(doc (p "X14: " (code/inline "cbr") ", upstream's record \"Good agreement\" (CB:101-104),
            10000 samples of a model of 2114 nodes, most of them in branches.  Its
            listing is with case " (code/inline "test/language/graph/colored-balls") ".")
         (code/lang "haskell" "cbr = hist $ mcmC 10000 (cballs_model (replicate 10 Blue))

-- fromList [(1,4265),(2,2203),(3,1244),(4,805),(5,486),(6,291),(7,565),(8,141)]
-- Good agreement")))

  ((test/replay/always-flip _)
   (let1 (bern bern/flip)
     (⊦ = 4 (count-true (mcmC 10 (pbern* bern))))
     (⊦ = 5809 (count-true (mcmC 10000 (ph2* bern))))
     (⊦ = 2026 (count-true (mcmC 10000 (ph21* bern))))
     (⊦ = 1.1301558326703463e-2
        (kl (tabulate .5 (mcmC 5000 mixng)) (tabulate .5 (mcmC 5000 (mixng′* bern))))))
   `(doc (p "X15: records made with the always-flip kernel of the Bernoulli distribution
            (D:64-70, commented out upstream and kept as " (code/inline "bern/flip")
            "): pbern's 4 True of 10 (P l.217-218), ph2's 5809 and ph21's 2026 (E:94, E:102)
            and the KL of mixng against mixng' (E:153-157).  The general forms of the models
            take the coin; " (code/inline "(let1 (bern bern/flip) ...)") " runs them as
            upstream did.")
         ,@(shown 'pbern* 'ph2* 'ph21* 'mixng′*)
         (code/lang "haskell" "-- Simple models
pbern = dist bern 0.4

pbern_run = mcmC 10 pbern")
         (code/lang "haskell" "ph2 = do
  x <- dist bern 0.2
  dist bern ((\\xv -> if xv then 0.1 else 0.7) <$> x)
-- 0.2 * 0.1 + 0.8*0.7 = 0.58

ph2r = length . filter id $ mcmC 10000 ph2
-- 5809

ph21 = do
  x <- dist bern 0.2
  y <- dist bern ((\\xv -> if xv then 0.1 else 0.7) <$> x)
  return x

ph21r = length . filter id $ mcmC 10000 ph21
-- 2026")
         (code/lang "haskell" "mixng' = do
  x <- dist bern 0.5
  if_ x
           (dist normal 10 2)
           (dist gamma 3 (1/3))

_  =
 let n = 5000 in kl (tabulate 0.5 (mcmC n mixng)) (tabulate 0.5 (mcmC n mixng'))
{-
5000 samples 2.275866271810348e-2
5000 samples 1.1301558326703463e-2 now")
         (code/lang "haskell" "-- resampling always flips, so fwd and bwd steps are 0
instance RDistribution Bern Bool where
{-
  resample d x _ g =
    let x' = not x in
    ((x', logDensity d x', g),0)
-}")
         (p "(Distribution.hs:64-70.)")))

  ((test/replay/tac _)
   (let ((t1 (mcmC 1000 tac1))
         (t2 (mcmC 1000 tac2))
         (t3 (mcmC 1000 tac3)))
     (⊦ equal? t1 t2)
     (⊦ equal? t2 t3)
     (⊦ = 505 (count-true t1)))
   `(doc (p "X16: \"tac2 and tac3 produce identical results, as expected: since diracN is
            unit, e >>= diracN is indeed the same as diracN\" (S:370-371); here tac1 too,
            since a constant Dirac node in a branch draws nothing.  Upstream's 620, 523 and
            532 (S:346-368) come from an older revision of the engine; 505 is the
            prototype's value.")
         ,@(shown 'tac1 'tac2 'tac3)
         (code/lang "haskell" "-- Programs from my messages about the problem with conditionals
tac1 = do
  c <- dist bern 0.5
  if_ c
    (dist normal 0 1)
    (dist uniform 10 20)
  return c

tac1_run = length . filter id $ mcmC 1000 tac1
-- 620 True

tac2 = do
  c <- dist bern 0.5
  if_ c
    (diracN (Val True) >> dist normal 0 1)
    (dist uniform 10 20)
  return c

tac2_run = length . filter id $ mcmC 1000 tac2
-- 523 True

tac3 = do
  c <- dist bern 0.5
  if_ c
    (diracN (Val True) >> dist normal 0 1)
    (dist uniform 10 20 >>= diracN)
  return c

tac3_run = length . filter id $ mcmC 1000 tac3
-- 532 True

-- tac2 and tac3 produce identical results, as expected:
-- since diracN is unit, e >>= diracN is indeed the same as diracN"))))

;; ---------------------------------------------------------------------------------------------
;; Suite hakaru10-laws: the laws of model composition (P §2.1, §4.3)
;; ---------------------------------------------------------------------------------------------

(define-suite hakaru10-laws

  ((doc r)
   `((structure/section "Laws")
     (p "The equational laws of model composition (P §2.1 l.250-281, §4.3 l.803-843):
        Dirac is the left and the right unit of composition, and an unused variable is
        marginalized.  Under the trace builder the right unit law holds sample for sample
        (X2, X16); the left unit law and marginalization hold in distribution.  Under exact
        enumeration, an independent oracle, they hold exactly.")
     (displaymath (m "do {x ← p; diracN x} ≡ p,  do {x ← diracN v; p} ≡ let x = v in p"))))

  ((test/laws/left-unit _)
   (⊦ equal? (mcmC 200 (model (dist normal 5. 1.)))
      (mcmC 200 (model (let* ((x (diracN 5.))) (dist normal x 1.)))))
   `(doc (p "A1: " (code/inline "do {x <- diracN 5; dist normal x 1}") " is "
            (code/inline "dist normal 5 1") " sample for sample: the constant Dirac node
            draws nothing and is never proposed (P l.259-261).")))

  ((test/laws/diractest _)
   (⊦ (equal-approx? .03) .5
      (frequency identity (mcmC 10000 (model (let* ((x (dist bern .5)))
                                               (diracN (lift identity x)))))))
   `(doc (p "A2: the port of " (code/inline "diractest") " (M:1029-1037): \"The result
            should show both True and False, in the same proportion\".  The Dirac node is
            recomputed with its parent, so the proposals to change x are accepted.")
         (code/lang "haskell" "{-
diractest = mcmC 5 $ do 
     a <- bernN (Val 0.5)
     diracN a
-}
diractest = mcmc 10 $
  with_node_none (bernS 0.5) `next` (\\a ->
  with_node a (\\a -> diracS a))
-- The result should show both True and False, in the same proportion")))

  ((test/laws/p2 _)
   (⊦ <= 9990 (length (hist (mcmC 10000 (model (let* ((x (dist uniform 0. 1.)))
                                                 (diracN (lift identity x))))))))
   `(doc (p "A3: P's " (code/inline "p2 = do {x <- dist uniform 0 1; diracN x}")
            " (§4.3 l.812-825), the example on which an MH that treats Dirac as an ordinary
            distribution never moves: here the independence proposal is always accepted (the
            uniform density is constant), so nearly every sample is new.")))

  ((test/laws/marginal _)
   (let1 (m (momenta (mcmC 10000 pmarg)))
     (⊦ (equal-approx? .05) '(10. . .25) m))
   `(doc (p "A4: pmarg has pnorm's distribution, N(10, 0.5²), though not its samples (P
            l.278-281; E:43-48).")
         ,@(shown 'pmarg)
         (code/lang "haskell" "pmarg = do
  x <- pbern
  y <- pnorm
  return y

pmarg_run = mcmC 100 pmarg")))

  ((test/laws/joint _)
   (let* ((samples (mcmC 10000 pjoin))
          (xs (map (λ (s) (if (car s) 1. 0.)) samples))
          (ys (map cdr samples))
          (mx (mean xs))
          (my (mean ys))
          (cov (mean (map (λ (x y) (* (- x mx) (- y my))) xs ys)))
          (corr (/ cov (sqrt (* (cdr (momenta xs)) (cdr (momenta ys)))))))
     (⊦ (equal-approx? .03) .4 mx)
     (⊦ < (abs corr) .05))
   `(doc (p "A5: pjoin samples the product of pbern and pnorm (P l.264-272; E:36-42):
            P(x) = 0.4 and x, y uncorrelated.")
         ,@(shown 'pjoin)
         (code/lang "haskell" "pjoin = do
  x <- pbern
  y <- pnorm
  return (pair x y)

_ = mcmC 100 pjoin")))

  ((test/laws/branch-unit _)
   (⊦ equal? (mcmC 1000 tac2) (mcmC 1000 tac3))
   `(doc (p "A6: " (code/inline "e >>= diracN") " inside a branch is " (code/inline "e")
            " (S:370-371), sample for sample; see X16.")))

  ((test/laws/exact _)
   (let ((coin (model (dist bern .4)))
         (pmarg/coin (model (let* ((x (dist bern .4))
                                   (y (dist bern .3)))
                              y))))
     (⊦ equal? (exact-posterior coin)
        (exact-posterior (model (let* ((x (dist bern .4))) (diracN x)))))
     (⊦ equal? (exact-posterior (model (dist bern .3)))
        (exact-posterior (model (let* ((p (diracN .3))) (dist bern p)))))
     (⊦ (equal-approx? 1e-15) (exact-posterior (model (dist bern .3)))
        (exact-posterior pmarg/coin))
     (⊦ (equal-approx? 1e-15) (exact-posterior ph2)
        (exact-posterior (model (let* ((x (dist bern .2))
                                       (q (diracN (lift (λ (xv) (if xv .1 .7)) x))))
                                  (dist bern q))))))
   `(doc (p "The laws under exact enumeration, where a model is its distribution: the right
            and left units, marginalization, and a Dirac node introduced for a lifted
            parameter (ph2, whose posterior is 0.58, E:91).  Enumeration does not reuse the
            engine: under its handler " (code/inline "diracN") " answers the value and "
            (code/inline "lift") " evaluates at once.")))

  )

;; ---------------------------------------------------------------------------------------------
;; Suite hakaru10-statistics: posterior estimates within statistical tolerances, over the
;; seeds of HAKARU10_SEEDS (17 alone by default)
;; ---------------------------------------------------------------------------------------------

(define-suite hakaru10-statistics

  ((doc r)
   `((structure/section "Statistics")
     (p "Posterior estimates of the examples, checked against exact or analytic values.
        Each tolerance is at least about 4 standard deviations of the estimate across the
        seeds 1 to 40, measured on this port, and more where a looser plan value is kept (the chains are autocorrelated, so the iid
        standard error underestimates the spread two- to fourfold); the averages over those
        seeds agree with the exact values, with no sign of bias.  The suite runs at seed 17
        by default; "
        (code/inline "HAKARU10_SEEDS=17,1,2,3,4") " ("
        (code/inline "make test-hakaru10-long") ") runs every case over the listed seeds,
        a failure reporting its seed.  Seeds this run: " (code ,(format "~a" seeds)) ".")))

  ((test/statistics/pbern _)
   (for-each (λ (seed) (⊦≈ seed .025 .4 (frequency identity (mcmC 10000 pbern #:seed seed))))
             seeds)
   `(doc (p "S1: pbern under the default kernel, an independence proposal (D:29-32).")))

  ((test/statistics/pnorm _)
   (for-each (λ (seed) (⊦≈ seed .03 '(10. . .25) (momenta (mcmC 10000 pnorm #:seed seed))))
             seeds)
   `(doc (p "S2: pnorm, N(10, 0.5²) (E:14).")))

  ((test/statistics/ph1 _)
   (for-each (λ (seed)
               (let1 (m (momenta (mcmC 20000 ph1 #:seed seed)))
                 (⊦≈ seed .3 0. (car m))
                 (⊦≈ seed 1.5 10. (cdr m))))
             seeds)
   `(doc (p "S3: ph1, x ~ N(0, 1) and N(x, 3²): mean 0, variance 1 + 9 (E:65-67).")
         ,@(shown 'ph1)))

  ((test/statistics/phier3 _)
   (for-each (λ (seed)
               (let1 (m (momenta (mcmC 20000 (phierN 3) #:seed seed)))
                 (⊦≈ seed 1.5 0. (car m))
                 (⊦≈ seed 8. 28. (cdr m))))
             seeds)
   `(doc (p "S4: phier with 3 steps: mean 0, variance 1 + 3 × 9 (BE:14-15).")))

  ((test/statistics/phier-kl _)
   (for-each (λ (seed)
               (let1 (p (tabulate .5 (mcmC 40000 (phierN 10) #:seed seed)))
                 (⊦< seed (kl p (normal-bins .5 (sqrt 91.) (map car p))) .25)))
             seeds)
   `(doc (p "S5: phier, Hur et al.'s Fig. 3, has the distribution N(0, √91) (P l.313-318),
            on which STAN goes wrong.  The KL divergence of 40000 samples, tabulated in bins
            of 0.5, from the exact bin masses is below 0.25 (over the seeds 1 to 40: mean
            0.080, standard deviation 0.038, at most 0.207; at 10000 samples the mean is 0.29
            but the spread reaches 0.98, too wide for a test).  Upstream's estimates, sample
            against sample (E:76-85):")
         (code/lang "haskell" "phier_kl n =
  kl (tabulate 0.5 (mcmC n phier))
     (tabulate 0.5 (mcmC n (dist normal 0 (sqrt <$> 91))))
     

_ =  phier_kl 10

{-
*Examples1> phier_kl 1000
0.7862745014080637
*Examples1> phier_kl 10000
0.23618257021796316
*Examples1> phier_kl 100000
3.441636543766045e-2
*Examples1> phier_kl 1000000
2.1573977909591603e-3
-}")
         (code/scheme ((kl (tabulate .5 (mcmC 40000 (phierN 10)))
                           (normal-bins .5 (sqrt 91.) bins))
                       ,(let1 (p (tabulate .5 (mcmC 40000 (phierN 10))))
                          (kl p (normal-bins .5 (sqrt 91.) (map car p))))))))

  ((test/statistics/ph2 _)
   (for-each (λ (seed)
               (⊦≈ seed .05 .58 (frequency identity (mcmC 10000 ph2 #:seed seed)))
               (⊦≈ seed .05 .2 (frequency identity (mcmC 10000 ph21 #:seed seed))))
             seeds)
   `(doc (p "S6: ph2 and ph21 under the default kernel: 0.2 × 0.1 + 0.8 × 0.7 = 0.58
            (E:91) and the prior 0.2 of x, since y is not observed.")))

  ((test/statistics/pcond _)
   (for-each (λ (seed)
               (let1 (run (λ (m) (proportions (mcmC 10000 m #:seed seed))))
                 (⊦≈ seed .03 '((10 . .25) (11 . .25) (20 . .25) (21 . .25)) (run pcond1))
                 (⊦≈ seed .04 '((1 . .5) (20 . .25) (21 . .25)) (run pcond2))
                 (⊦≈ seed .05 '((1 . .5) (20 . .25) (21 . .25)) (run pcond3))
                 (⊦≈ seed .05 `((1 . ,(/ 2. 3.)) (20 . ,(/ 1. 6.)) (21 . ,(/ 1. 6.)))
                     (run pcond4))))
             seeds)
   `(doc (p "S7: the branching models pcond1-4 (E:104-282): the exact posteriors, pcond4's
            given upstream as \"Exact: [(0.166666666666666657, V 21); (0.166666666666666657,
            V 20); (0.66666666666666663, V 1)]\" (E:280-282).  The tolerances are per model,
            at 4 to 5 standard deviations of P(10), P(1), P(1) and P(1) across the seeds 1 to
            40 (0.0066, 0.0079, 0.0121 and 0.0097): .03 (plan §9.4), .04, .05 and .05.")))

  ((test/statistics/mixng _)
   (for-each (λ (seed)
               (for-each (λ (m)
                           (let1 (xs (mcmC 10000 m #:seed seed))
                             (⊦≈ seed .4 5.5 (mean xs))
                             (⊦≈ seed .05 .5 (frequency (λ (x) (> x 5.)) xs))))
                         (list mixng mixng′)))
             seeds)
   `(doc (p "S8: the mixture of N(10, 2²) and gamma(3, 1/3), whose means are 10 and 1, in
            equal parts: mean 5.5, half the samples above 5 (E:139-151; P §2.2).")))

  ((test/statistics/biased-coin _)
   (for-each (λ (seed)
               (let ((tf (momenta (drop (mcmC 10000 (biased-coin #t #f) #:seed seed) 1000)))
                     (ff (momenta (drop (mcmC 10000 (biased-coin #f #f) #:seed seed) 1000))))
                 (⊦≈ seed .02 .5 (car tf))
                 (⊦≈ seed .008 .05 (cdr tf))
                 (⊦≈ seed .02 .25 (car ff))
                 (⊦≈ seed .008 .0375 (cdr ff))))
             seeds)
   `(doc (p "S9: the posteriors beta(2, 2), of mean 0.5 and variance 0.05 (E:206), and
            beta(1, 3), of mean 0.25 and variance 0.0375 (E:216), after 1000 samples of
            burn-in.  The \"incremental\" record of the commented-out
            test_multiple_conditions13 below is not reproduced: at seed 17 this chain gives
            (0.251523121569678 . 0.0379368207086475); see the findings of the language suite.")
         (code/lang "haskell" "{-
test_multiple_conditions13 =
  momenta . drop 1000 $
   mcmC 10000 (prog_multiple_conditions False False)
-- beta 1 3: 0.25, sigma2 = 3.75e-2
-- original Metropolis
-- (0.25144728141545775,3.782788408489756e-2)
-- incremental
-- (0.2445212550806874,3.651432172401049e-2)
-}")))

  ((test/statistics/nice-prog-mult-conditions _)
   (for-each (λ (seed)
               (let1 (average (λ (c1 c2) (/ (fold + 0. (mcmC 1000 (nice-prog-mult-conditions c1 c2)
                                                              #:seed seed))
                                            1000.)))
                 (⊦≈ seed .05 .5 (average #t #f))
                 (⊦≈ seed .05 .25 (average #f #f))))
             seeds)
   `(doc (p "S10: the biased coin of Syntax.hs, ending in " (code/inline "diracN b") ";
            upstream's sums of 1000 samples, 499.8 and 257.29 (S:297-301), come from an older
            revision of the engine: at seed 17 they are 502.706370138924 and
            252.485601387305 here, and are not asserted (see the findings of the language
            suite).")
         ,@(shown 'nice-prog-mult-conditions)
         (code/lang "haskell" "nice_prog_mult_conditions c1 c2 = do
  b <- dist beta 1 1
  dist (c1 `condition` bern) b
  dist (c2 `condition` bern) b
  diracN b

niceexmcr11 = sum $ mcmC 1000 (nice_prog_mult_conditions True False)
-- 499.799988743311

niceexmcr12 = sum $ mcmC 1000 (nice_prog_mult_conditions False False)
-- 257.2928205920028")))

  ((test/statistics/niceex1 _)
   (for-each (λ (seed) (⊦≈ seed .05 .25 (frequency identity (mcmC 10000 niceex1 #:seed seed))))
             seeds)
   `(doc (p "S11: P(e and d) = E[c] × 0.5 = 0.25 (S:282-289).  Upstream's 7 samples are \"All
            True\", presumably from an older revision of the engine: at seed 17 they are (#t
            #t #t #t #f #f #f) here, and are not asserted (see the findings of the language
            suite).")
         ,@(shown 'niceex1)
         (code/lang "haskell" "niceex1 = do
  c <- dist uniform 0 1
  d <- dist bern 0.5
  e <- dist bern c
  diracN ((&&) <$> e <*> d)

niceex1r = mcmC 7 niceex1
-- All True")))

  ((test/statistics/impossible-branches _)
   (for-each (λ (seed)
               (⊦ equal? (list seed (make-list 100 #t))
                  (list seed (mcmC 100 (prog-mixture1 -2.) #:seed seed)))
               (⊦ equal? (list seed (make-list 100 #t)) (list seed (mcmC 100 tac10 #:seed seed)))
               (⊦ equal? (list seed (make-list 100 #t)) (list seed (mcmC 100 tac11 #:seed seed))))
             seeds)
   (⊦ equal? (make-list 3 '((#t . 1.)))
      (map exact-posterior (list (prog-mixture1 -2.) tac10 tac11)))
   `(doc (p "S12: the else branches observe values outside the uniform's support (log
            likelihood -inf): an initial trace taking them is built again (M:882-891), and no
            step can switch to them (rejectedLL, M:853-854, M:927).  Exact enumeration
            prunes the paths of weight 0, so the three posteriors are exactly "
            (code/inline "((#t . 1.))") ".")
         ,@(shown 'prog-mixture1 'tac10 'tac11)
         (code/lang "haskell" "prog_mixture1 cv = do
  c <- dist bern 0.5
  if_ c (dist (cv `condition` normal)  1 1)
        (dist (cv `condition` uniform) 0 3)
  return c

prog_mixture1_run = mcmC 20 (prog_mixture1 (-2))
-- all true")
         (code/lang "haskell" "tac10 = do
  c <- dist bern 0.5
  if_ c
    -- then
    (dist (0 `condition` normal) 0 1)
    -- else
    (dist (0 `condition` uniform) 10 20)
  return c

tac10_run = length . filter id $ mcmC 100 tac10
-- 100  -- all True

tac11 = do
  c <- dist bern 0.5
  if_ c
    -- then
    (dist (0 `condition` normal) 0 1)
    -- else
    (do
        d <- diracN 10
        dist (0 `condition` uniform) d 20)
  return c

tac11_run = length . filter id $ mcmC 100 tac11
-- 100  -- all True")))

  ((test/statistics/tac1 _)
   (for-each (λ (seed) (⊦≈ seed .05 .5 (frequency identity (mcmC 20000 tac1 #:seed seed))))
             seeds)
   `(doc (p "S13: tac1's branch does not influence c, so P(c) = 0.5 (S:340-348).")))

  ((test/statistics/exif _)
   (for-each (λ (seed)
               (let1 (xs (mcmC 20000 exif1 #:seed seed))
                 (⊦≈ seed .8 13.5 (mean xs))
                 (⊦≈ seed .05 .5 (frequency (λ (x) (> x 10.)) xs)))
               (let1 (xs (mcmC 20000 exif2 #:seed seed))
                 (⊦≈ seed .04 .25 (frequency (λ (x) (eqv? x 10)) xs))
                 (⊦≈ seed .04 .25 (frequency (λ (x) (eqv? x 20)) xs))
                 (⊦≈ seed .04 .5 (frequency (λ (x) (<= x 2.)) xs))))
             seeds)
   `(doc (p "S14: exif1 is 2d with d ~ N(1, 1) when c holds, else 5 + N(20, 1): mean 13.5,
            half above 10; exif2 is 10 or 20 a quarter of the time each, else the sum of two
            uniforms (S:314-337).")
         ,@(shown 'exif1 'exif2)
         (code/lang "haskell" "exif1 = do
  c <- dist bern 0.5
  d <- if_ c (dist normal ((fromIntegral . fromEnum) <$> c) 1)
             (diracN 5)
  e <- if_ (not <$> c) (dist normal 20 1) (diracN d)
  diracN (d + e)

exif1r = mcmC 7 exif1

exif2 = do
  c <- dist bern 0.5
  if_ c (cl1 10) (cl1 20)
 where
   cl1 x = do
     d <- dist bern 0.5
     if_ d cl3 (diracN x)
     
   cl3 = do
     a <- dist uniform 0 1
     b <- dist uniform 0 1
     diracN (a + b)
     

exif2r = mcmC 17 exif2")))

  ((test/statistics/pcorr _)
   (for-each (λ (seed)
               (let1 (xys (mcmC 20000 (pcorr* .1 pair) #:seed seed))
                 (⊦≈ seed .03 .9 (frequency (Λ ((,x . ,y)) (eq? x y)) xys))
                 (⊦≈ seed .1 .5 (frequency cdr xys))))
             seeds)
   `(doc (p "S15: pcorr returning " (code/inline "(pair x y)") ": the observation makes x and
            y equal with probability 0.9, and y stays fair (E:223-238).")))

  ((test/statistics/condhier _)
   (for-each (λ (seed)
               (let1 (m (momenta (mcmC 20000 condhier #:seed seed)))
                 (⊦≈ seed .2 (/ 1. 82.) (car m))
                 (⊦≈ seed .25 (/ 81. 82.) (cdr m))))
             seeds)
   `(doc (p "S16: the conditioned hierarchical model (E:240-251): x ~ N(0, 1) observed as 1
            through nine steps of variance 9, so the posterior of x is N(1/82, 81/82).")
         ,@(shown 'phierN′ 'condhier)
         (code/lang "haskell" "phierN' :: Int -> SExp Double -> Double -> Model Double
phierN' n start obs =
  foldl (>>=) (pure start) $
  replicate (n-1) (\\x -> dist normal x 3) ++
  [(\\x -> dist (obs `condition` normal) x 3)]

condhier = do
  x <- dist normal 0 1
  phierN' 9 x 1
  return x")))

  ((test/statistics/grass-girl _)
   (for-each (λ (seed)
               (⊦≈ seed .04 .4684714 (frequency identity (mcmC 20000 grass #:seed seed)))
               (⊦≈ seed .05 .6644295 (frequency identity (mcmC 10000 girl #:seed seed)))
               (⊦≈ seed .04 (/ 6. 13.) (frequency identity (mcmC 20000 grass/if #:seed seed)))
               (⊦≈ seed .05 .6058 (frequency identity (mcmC 40000 grass/wet #:seed seed))))
             seeds)
   (⊦ (equal-approx? 1e-12) `((#f . ,(/ 7. 13.)) (#t . ,(/ 6. 13.))) (exact-posterior grass/if))
   (⊦ (equal-approx? 1e-12) '((#f . .3942) (#t . .6058)) (exact-posterior grass/wet))
   `(doc (p "S17: grass against \"0.4684714427203697 -- theoretical\" (E:322) and girl
            against its exact posterior 0.6644295302013423 (hakaru10-exact.scm).  The
            grass of E:284-296, commented out upstream, conditions in the leaves of nested
            branches; its posterior P(rain | wet) is 0.3 × 0.9 / (0.3 × 0.9 + 0.7 × (0.5 ×
            0.8 + 0.5 × 0.1)) = 6/13 = 0.4615, which exact enumeration gives and the chain
            approaches (standard deviation 0.0084 across the seeds 1 to 40).  Without the
            observation (E:302, commented out upstream), Fig. 1 gives \"the probability of
            the grass being wet is 60.6%\" (P l.60-62): " (code/inline "grass/wet")
            " returns the wet node, whose exact probability is 0.3 × 0.5 × 0.982 + 0.3 × 0.5
            × 0.91 + 0.7 × 0.5 × 0.82 + 0.7 × 0.5 × 0.1 = 0.6058.  A step redraws one of the
            three nodes, so the chain of the wet node is strongly autocorrelated: 40000
            samples are within 0.05 (0.5935 at seed 17, from 0.5928 to 0.6471 over the seeds
            1 to 10).")
         ,@(shown 'grass/if 'grass/wet)
         (code/lang "haskell" "{-
grass = do
  rain        <- dist bern 0.3
  sprinkler   <- dist bern 0.5
  let obs = dist (True `condition` bern)
  if_ rain
    (obs 0.9)
    (if_ sprinkler
        (obs 0.8)
        (obs 0.1))
  return rain
-}")))

  ((test/statistics/mhex _)
   (for-each (λ (seed) (⊦≈ seed .1 .345731 (mean (mcmC 20000 mhex #:seed seed)))) seeds)
   `(doc (p "S18: the example of P §4 (l.677-720) explaining MH; its mean is 0.345731 by
            numerical integration.")
         ,@(shown 'mhex)
         (code/lang "haskell" "mhex = do
  x <- dist normal 0 1
  y <- dist normal x 2
  if_ ((>0.5) <$> x)
        (return x)
        (do {z <- dist beta 1 1; return (y + z)})")))

  ((test/statistics/post-bias _)
   (for-each (λ (seed)
               (⊦ equal? (list seed (make-list 80 #t))
                  (list seed (drop (mcmC 100 (post-bias 1.) #:seed seed) 20))))
             seeds)
   `(doc (p "S19: post_bias with 20 samples of burn-in: the else branch's observation, at 99
            standard deviations, makes c = #f a trap the chain leaves at its first accepted
            proposal and never enters again (E:256-263).")))

  ((test/statistics/if/shared-inactive _)
   (for-each (λ (seed)
               (⊦≈ seed .06 .5 (frequency (λ (x) (< x 2.5))
                                          (mcmC 10000 shared-inactive #:seed seed))))
             seeds)
   `(doc (p "S20, deviation 1: d belongs to the rest of the model and is the then branch's
            result; when c is false, resampling d enqueues the exit through the inactive
            result (M:673), yet neither the entry nor the active result changed, and
            upstream fails:")
         (code/lang "haskell" "     -- the parent nodes must have been updated
     when (tstamp (Mem.dref entry) <= tstamp self &&
           tstamp nd <= tstamp self) $
       fail $ unwords [\"the parents of the exit node\", show (Mem.addr ns),
                       \"namely\", show [Mem.addr entry, Mem.addr ndr],
                       \"must have been updated\"]")
         (p "(Metropolis.hs:787-792.)  Here the exit keeps its state, and the chain samples
            the mixture of N(0, 1) and N(5, 1), half below 2.5.")
         ,@(shown 'shared-inactive))))

;; ---------------------------------------------------------------------------------------------
;; Suite hakaru10-semantics: 'upstream and 'extended semantics of branch reactivation
;; ---------------------------------------------------------------------------------------------

(define-suite hakaru10-semantics

  ((doc r)
   `((structure/section "Semantics of branches")
     (p "In upstream's kernel an inactive node's updates are delayed (M:298-303) and replayed
        when its branch is activated again (M:767-775).  For an observed node this is exact:
        activation adds back the LL that deactivation took.  For a resampleable node the
        replayed update adds " (m "ll(now) − ll(stale)") " to the Δ of the switching step: it
        charges the change of LL of steps already accepted without it.  The note M:1145-1168
        justifies the treatment of branches by merging a branch's nodes into one node:")
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
     (p "The argument assumes the reactivated nodes are fresh draws from their prior, while
        the kernel reuses their stale values.  The smallest counterexample, "
        (code/inline "pswitch") " (plan §6.8), has P(c) = 0.5; the exact stationary
        distribution of upstream's kernel gives P(c) = 0.377286.  The upstream examples are
        unaffected: their branch-local resampleable nodes have constant parameters.")
     ,@(shown 'pswitch)
     (p "The opt-in " (code/inline "#:semantics 'extended") " (deviation 8) runs MH on the
        space extended with the inactive resampleable nodes as auxiliary variables drawn
        from their prior, the merged-node argument made exact: updates are never delayed,
        an inactive resampleable node's LL change counts in Δ and an inactive observed
        node's does not, (de)activation still moves only observed weights, and the
        proposal, the retry and the cardinality term are upstream's.  An inactive node whose
        update raises a parameter error, or any other error that an inactive Dirac node would
        defer, rejects the step (plan C22).  The kernel residual of
        hakaru10-exact.scm shows that the extended kernel leaves the posterior invariant,
        exactly, on small models.")))

  ((test/semantics/pswitch/upstream _)
   (⊦ (equal-approx? .03) .3773 (frequency identity (mcmC 40000 pswitch)))
   `(doc (p "B1: upstream's bias, documented: P(c) near 0.3773 instead of 0.5.")))

  ((test/semantics/pswitch/extended _)
   (⊦ (equal-approx? .02) .5 (frequency identity (mcmC 40000 pswitch #:semantics 'extended)))
   `(doc (p "B2: the extended semantics samples the right posterior, P(c) = 0.5.")))

  ((test/semantics/extended/examples _)
   (let1 (run (λ (m) (proportions (mcmC 10000 m #:semantics 'extended))))
     (⊦ (equal-approx? .03) '((10 . .25) (11 . .25) (20 . .25) (21 . .25)) (run pcond1))
     (⊦ (equal-approx? .04) '((1 . .5) (20 . .25) (21 . .25)) (run pcond2))
     (⊦ (equal-approx? .05) '((1 . .5) (20 . .25) (21 . .25)) (run pcond3))
     (⊦ (equal-approx? .05) `((1 . ,(/ 2. 3.)) (20 . ,(/ 1. 6.)) (21 . ,(/ 1. 6.))) (run pcond4)))
   (let1 (xs (mcmC 10000 mixng #:semantics 'extended))
     (⊦ (equal-approx? .4) 5.5 (mean xs))
     (⊦ (equal-approx? .05) .5 (frequency (λ (x) (> x 5.)) xs)))
   (let1 (balls (proportions (mcmC 10000 (cballs-model (make-list 10 'blue))
                                   #:semantics 'extended #:seed 17)))
     (⊦ = .4265 (alist-ref 1 balls))
     (⊦ > (alist-ref 1 balls) (alist-ref 2 balls) (alist-ref 3 balls)))
   `(doc (p "B3: the extended semantics on upstream's branching examples gives the same
            posteriors as S7 and S8, within S7's tolerances.")
         (p "The colored balls line is a regression value at seed 17, not a statistical
            check: P(1) = 0.4265, with P(1) > P(2) > P(3), against the closed form 0.4119636809525704
            of hakaru10-exact.scm.  At 10000 extended samples the standard deviation of P(1)
            across seeds is about 0.05 (seeds 1 to 40: mean 0.4085, sd 0.0515), so a
            tolerance of 4 to 5 of them could not tell a biased sampler from a correct one.
            That the extended kernel leaves the colored balls' posterior invariant is shown
            exactly, by the kernel residual of hakaru10-exact.scm (E10), on the colored balls
            cut to 3 balls and 2 draws.")))

  ((test/semantics/check _)
   (for-each (λ (semantics)
               (for-each (Λ ((,name ,m))
                           (⊦ equal? (list name semantics 300)
                              (list name semantics
                                    (length (mcmC 300 m #:semantics semantics #:check #t)))))
                         `((phier3 ,(phierN 3))
                           (mhex ,mhex)
                           (exif2 ,exif2)
                           (pcond4 ,pcond4)
                           (colored-balls ,(cballs-model '(blue green blue)))
                           (pswitch ,pswitch)
                           (regression ,(regression (regression-data 2 10))))))
             '(upstream extended))
   (for-each (λ (semantics)
               (for-each (Λ ((,name ,m))
                           (⊦ equal? (list name semantics (mcmC 50 m #:semantics semantics))
                              (list name semantics (mcmC 50 m #:semantics semantics #:check #t))))
                         `((guarded-car/constant ,guarded-car/constant)
                           (guarded-car/node ,guarded-car/node))))
             '(upstream extended))
   (for-each (Λ ((,name ,m))
               (⊦ equal? (list name 2000)
                  (list name (length (mcmC 2000 m #:semantics 'extended #:check #t)))))
             `((act-update ,act-update)
               (act-update/finite ,act-update/finite)
               (pending-abort ,pending-abort)))
   (⊦ (equal-approx? .02) `((1 . ,(/ .3 1.3)) (2 . ,(/ 1. 1.3)))
      (proportions (mcmC 20000 pending-abort #:semantics 'extended)))
   (let1 (exact (exact-posterior act-update))
     (⊦ (equal-approx? 1e-12) `((0 . ,(/ .9 2.2)) (1 . ,(/ .3 2.2)) (2 . ,(/ 1. 2.2))) exact)
     (⊦ (equal-approx? .02) exact (proportions (mcmC 20000 act-update #:semantics 'extended))))
   (⊦ equal? '()
      (filter-map (Λ ((,name . ,m))
                    (let1 (residual (kernel-residual m))
                      (and (not (< residual 1e-12)) (cons name residual))))
                  `((act-update . ,act-update) (act-update/finite . ,act-update/finite))))
   `(doc (p "B4: " (code/inline "#:check #t") " on 300 samples of seven models, in both
            semantics, raises no condition: the construction-order forward run reproduces
            the initial trace (value, and LL within 1e-9), and after every step the
            reference evaluator recomputes every node from scratch (activity counts, Dirac
            values, distributions and LLs, exits) and finds the incremental state, a
            rejected step restores the state exactly, and in " (code/inline "'extended")
            " mode an accepted step's Δ is the change of the extended target's LL; the
            next case shows that each of these checks fails when it should.")
         (p "The construction-order run builds the branch its test does not select as the
            trace builder does, under lazy lifting, so a model whose inactive branch applies "
            (code/inline "car") " to the " (code/inline "'()") " its test rules out (a
            deferred Dirac value, M:99, M:120-129) passes the check, with a constant test and
            with a lifted one, in both semantics, and samples what it samples without "
            (code/inline "#:check") ":")
         ,@(shown 'guarded-car/constant 'guarded-car/node)
         (p "Three models reach the pending weight of the " (code/inline "'extended")
            " semantics (deviation 8), each checked on 2000 samples.  In "
            (code/inline "act-update") " and " (code/inline "act-update/finite")
            " a step that moves x from 2 activates the observation and updates its parameters
            at once: its weight is added once, with its new LL, when propagation ends, which
            the step check verifies against the change of the extended LL (the stale LL is
            -inf in the first, log .5 in the second).  In " (code/inline "pending-abort")
            " the step that activates the observation is then rejected mid-propagation by the
            inactive " (code/inline "uniformly '()") " of the second branch (plan C22): the
            journal restores the pending flag, which the reference evaluator finds clear and
            the step check finds restored.  Its chain, which the extended target keeps off x
            = 0 (as in " (code/inline "invalid-inactive") ", case B5), gives P(x = 1) = .3/1.3
            within .02 over 20000 samples.  The 'extended chain of "
            (code/inline "act-update") " gives its exact posterior, (.9 .3 1)/2.2, within .02
            over 20000 samples, and the kernel residual (case E10 of hakaru10-exact.scm) is below "
            (m "10⁻¹²") " on both " (code/inline "act-update") " models.  Counting the
            weight at activation, or again at the update, makes the residual of "
            (code/inline "act-update") " about .18, and its chain never leaves x = 2.")
         ,@(shown 'act-update 'act-update/finite 'pending-abort)))

  ((test/semantics/check/negative _)
   (let ((k 0) (j 0) (calls 0) (victim #f) (shifted #f) (tamper! #f))
     ; an LL that drifts with the number of calls of the log-density.
     (define (drifting)
       (let1 (d (normal 0. 1.))
         (make-distribution 'drifting
                            log-density: (λ (x)
                                           (add1! calls)
                                           (- (distribution-log-density d x) (* 1e-3 calls)))
                            sample: (λ (src) (distribution-sample d src)))))
     ; a normal whose sampler replaces victim's state by an equal copy, behind the journal.
     (define (unjournaled)
       (let1 (d (normal 0. 1.))
         (make-distribution 'unjournaled
                            log-density: (λ (x) (distribution-log-density d x))
                            sample: (λ (src)
                                      (when (node? victim)
                                        (let1 (s (node-state victim))
                                          (node-state-set! victim
                                                           (make-nstate (nstate-val s) (nstate-ll s)
                                                                        (nstate-dst s)
                                                                        (nstate-tstamp s)))))
                                      (distribution-sample d src)))))
     (define-model behind-the-journal
       (let* ((x (dist (condition .5 normal) 0. 1.)))
         (set! victim x)
         (let* ((s (dist unjournaled)))
           (dist (condition 3. normal) s .5)
           s)))
     ; replace the state of node n, or of the node at addr of ch, by (f val ll dst tstamp)
     ; of its fields, with the raw node-state-set!.
     (define (restate-node! n f)
       (let1 (s (node-state n))
         (node-state-set! n (f (nstate-val s) (nstate-ll s) (nstate-dst s) (nstate-tstamp s)))))
     (define (restate! ch addr f) (restate-node! (chain-node ch addr) f))
     ; the message of reference-check on the initial trace of shifted-normal, its x at
     ; address 1 and its Dirac node at 2, once corrupt! has changed it behind the engine's back.
     (define-model shifted-normal (let* ((x (dist normal 0. 1.))) (diracN (sexp+ x 1.))))
     (define (corrupted-message corrupt! #!optional (m shifted-normal) (semantics 'upstream))
       (internal-message
         (τ (receive (ch result) (build-chain m (make-stdgen-source 17) semantics)
              (corrupt! ch)
              (reference-check ch)))))
     ; add 1 to the stored LL of the first inactive resampleable node of ch.
     (define (shift-inactive-ll! ch)
       (restate-node! (chain-fold (λ (n found)
                                    (or found
                                        (and (node-inactive? n)
                                             (eq? (node-kind n) 'resampleable)
                                             n)))
                                  #f ch)
                      (λ (v ll d t) (make-nstate v (+ 1. ll) d t))))
     ; a normal whose sampler, once tamper! is armed, runs it once before drawing.
     (define (tampering)
       (let1 (d (normal 0. 1.))
         (make-distribution 'tampering
                            log-density: (λ (x) (distribution-log-density d x))
                            sample: (λ (src)
                                      (when tamper!
                                        (let1 (t! tamper!) (set! tamper! #f) (t!)))
                                      (distribution-sample d src)))))
     (define-model tampered
       (let* ((x (dist (condition .5 normal) 0. 1.)))
         (set! shifted x)
         (dist tampering)))
     ; the internal message of the 'extended chain of tampered under #:check check, read
     ; sample by sample for 20 steps, whose step k (from 1) adds 1 to the stored LL of x.
     (define (tampered-message check k)
       (set! tamper! #f)
       (internal-message
         (τ (let loop ((s (mcmC§ tampered #:semantics 'extended #:check check)) (i 0))
              (car§ s)
              (when (= i (sub1 k))
                (set! tamper! (τ (restate-node! shifted (λ (v ll d t) (make-nstate v (+ 1. ll) d t))))))
              (when (< i 20) (loop (cdr§ s) (add1 i)))))))
     (⊦ equal? "the construction-order run and the initial trace differ in value"
        (internal-message (τ (construction-check (model (add1! k) k)))))
     (⊦ equal? "the construction-order run and the initial trace differ in LL"
        (internal-message
          (τ (construction-check
               (model (add1! j) (dist (condition 0. normal) 0. (exact->inexact j)))))))
     (⊦ equal? "the construction-order run and the initial trace differ in value"
        (internal-message (τ (mcmC 10 (model (add1! k) k) #:check #t))))
     (⊦ equal? "the construction-order run and the initial trace differ in LL"
        (internal-message
          (τ (mcmC 10 (model (add1! j) (dist (condition 0. normal) 0. (exact->inexact j)))
                   #:check #t))))
     (⊦ equal? '("an LL differs from its recomputation" none)
        (map (λ (semantics) (corrupted-message shift-inactive-ll! tac1 semantics))
             '(extended upstream)))
     (for-each (Λ ((,expected ,corrupt!)) (⊦ equal? expected (corrupted-message corrupt!)))
               `(("an LL differs from its recomputation"
                  ,(λ (ch) (restate! ch 1 (λ (v ll d t) (make-nstate v (+ 1. ll) d t)))))
                 ("a value differs from its recomputation"
                  ,(λ (ch) (restate! ch 2 (λ (v ll d t) (make-nstate 99. ll d t)))))
                 ("a distribution differs from its recomputation"
                  ,(λ (ch) (restate! ch 1 (λ (v ll d t)
                                            (make-nstate v ll (distk-rd (normal 0. 2.)) t)))))
                 ("a tstamp is after now"
                  ,(λ (ch) (restate! ch 1 (λ (v ll d t) (make-nstate v ll d 99)))))
                 ("a deactivation count differs from its recomputation"
                  ,(λ (ch) (node-inactive-set! (chain-node ch 1) 1)))
                 ("an update is delayed on an active node or in 'extended mode"
                  ,(λ (ch) (node-delayed-set! (chain-node ch 1) #t)))
                 ("a node is still pending or queued after the step"
                  ,(λ (ch) (node-pending-set! (chain-node ch 1) #t)))
                 ("the card change differs from its recomputation"
                  ,(λ (ch) (chain-card-change-set! ch 1)))))
     (⊦ equal? "an LL differs from its recomputation"
        (internal-message (τ (mcmC 50 (model (dist drifting)) #:check #t))))
     (⊦ = 50 (length (mcmC 50 behind-the-journal)))
     (for-each (λ (semantics)
                 (⊦ equal? (list semantics "a rejected step did not restore the state")
                    (list semantics
                          (internal-message
                            (τ (mcmC 50 behind-the-journal #:check #t #:semantics semantics))))))
               '(upstream extended))
     (⊦ equal? "the step's delta is not the extended LL change" (tampered-message #t 3))
     (⊦ equal? "the step's delta is not the extended LL change" (tampered-message 5 5))
     (⊦ eq? 'none (tampered-message 5 6))
     (⊦ eq? 'none (tampered-message #f 3)))
   `(doc (p "B4′, the negative controls of B4: each oracle of " (code/inline "#:check")
            " reports a chain it should reject, with an " (code/inline "internal")
            " condition.  " (code/inline "construction-check") " (PLAN-IDIOM §2.4) catches a
            model whose two runs, the construction-order forward run and the trace builder's
            first build, differ in value (a counter answered as the result) or in LL (the
            same counter as an observation's standard deviation): both models break the
            multi-shot purity of C28 on purpose.  " (code/inline "mcmC")
            " runs it once per chain under " (code/inline "#:check")
            ", and reports both models the same way.")
         (p (code/inline "reference-check") " catches each of eight corruptions of the
            initial trace of " (code/inline "shifted-normal") ", x and the Dirac node x + 1,
            made behind the engine's back with its exported setters, each with its own
            message: an LL, a value, a distribution or a tstamp after now in a node's state,
            a deactivation count, a delayed update on an active node, a pending weight left
            over, and the chain's card change.  It also catches, through the wiring of "
            (code/inline "mcmC") ", a user distribution whose density drifts with the number
            of its calls.  In " (code/inline "'extended")
            " mode, where no update is delayed, it checks the inactive nodes' states too: the
            LL of the inactive branch's choice of " (code/inline "tac1")
            ", shifted by 1, is reported in an " (code/inline "'extended")
            " build and not in an " (code/inline "'upstream") " one, where an inactive node
            may hold a stale state.")
         (p "The step check catches a rejected step that did not restore the state: in "
            (code/inline "behind-the-journal") " the sampler of the proposed node replaces
            the state of the observed x by an equal copy with the raw "
            (code/inline "node-state-set!") ", which no journal entry undoes; the reference
            evaluator compares values and LLs and accepts the copy, the step check compares
            the state objects themselves (C5) and does not, in both semantics.  Without "
            (code/inline "#:check") " the chain runs.")
         (p "In " (code/inline "'extended") " mode it also catches an accepted step whose Δ
            is not the change of the extended LL: in " (code/inline "tampered")
            " the sampler of the only resampleable node, armed before step k, adds 1 to the
            stored LL of the observed x, which the step's Δ does not count; with no
            observation of the proposed node every step is accepted.  The step check runs
            before the reference evaluator, which would report the LL instead.  "
            (code/inline "#:check 5") " checks the first 5 steps only: it reports a
            corruption at step 5 and runs through one at step 6.")))

  ((test/semantics/check/escaped-error _)
   (let ((armed #f))
     ; the update of y raises once, the first time x is proposed true once armed.
     (define-model flaky
       (let* ((x (dist bern .1))
              (y (dist normal
                       (lift (λ (xv)
                               (if xv (if armed (begin (set! armed #f) (error "flaky")) 5.) 0.))
                             x)
                       1.)))
         (pair x y)))
     (let ((s (mcmC§ flaky #:check #t #:seed 3)))
       (set! armed #t)
       (let1 (reads (let loop ((i 0) (acc '()))
                      (if (= i 12)
                          (reverse acc)
                          (loop (add1 i)
                                (cons (condition-case (let1 (v (car§ s)) (set! s (cdr§ s)) v)
                                        (e (exn) ((condition-property-accessor 'exn 'message) e)))
                                      acc)))))
         (⊦ equal? '("flaky") (filter string? reads))
         (⊦ = 11 (count pair? reads)))))
   `(doc (p "B4″: an error that escapes a step leaves its journal open, and the next step
            first rolls it back (" (code/inline "journal-begin!") ", no upstream counterpart),
            so a reader that catches the error and reads on continues from the last accepted
            state.  In " (code/inline "flaky") " the update of y raises once, the first time
            x is proposed true after the build; at seed 3 that is the fifth read of the
            stream, and " (code/inline "#:check #t") " finds the state sound at every one of
            the 11 other reads.")))

  ((test/semantics/invalid-inactive _)
   (let1 (xs (mcmC 2000 invalid-inactive #:semantics 'extended))
     (⊦ = 2000 (length xs))
     (⊦ every positive? xs))
   (⊦ eq? 'parameter (hakaru10-kind (τ (mcmC 2000 invalid-inactive))))
   (let1 (xs (mcmC 2000 invalid-inactive/beta #:semantics 'extended))
     (⊦ = 2000 (length xs))
     (⊦ every positive? xs))
   (⊦ eq? 'parameter (hakaru10-kind (τ (mcmC 2000 invalid-inactive/beta))))
   (⊦ eq? 'branch (hakaru10-kind (τ (mcmC 1 (invalid-inactive/branch .5) #:semantics 'extended))))
   (⊦ eq? 'branch
      (hakaru10-kind (τ (mcmC 2000 (invalid-inactive/branch .05) #:semantics 'extended))))
   (⊦ eq? 'branch (hakaru10-kind (τ (mcmC 1 (invalid-inactive/branch .5)))))
   (let1 (xs (mcmC 2000 (invalid-inactive/branch .05)))
     (⊦ = 2000 (length xs))
     (⊦ any identity xs))
   (for-each (λ (semantics)
               (⊦ equal? (list semantics 'parameter 'parameter)
                  (list semantics
                        (hakaru10-kind (τ (mcmC 10 (model (dist normal 0. 0.))
                                                #:semantics semantics)))
                        (hakaru10-kind (τ (mcmC 2000 active-invalid #:semantics semantics))))))
             '(extended upstream))
   (⊦ = 1 (length (mcmC 1 active-invalid #:semantics 'extended)))
   `(doc (p "B5, plan C22: in extended mode an inactive node is updated, so the gamma of a
            negative shape in the branch not taken takes part in every step.  Its sampler
            raises a parameter error, which rejects the initial trace; its density answers
            -inf, so an update with x ≤ 0 rejects the step through Δ = -inf.  Beta's density
            raises the parameter error itself (D:330-339): in "
            (code/inline "invalid-inactive/beta") " the update that raises it rejects the
            step (plan C22's guarded update) instead of stopping the chain.  The extended
            target is then restricted to the states where every inactive node has
            valid parameters: here every state with x ≤ 0 holds the inactive "
            (code/inline "gamma x 1") ", so the chain stays where x > 0 and the result, the
            gamma sample, is always positive, where the model's posterior has P(x > 0) = 0.5.
            The extended semantics is opt-in for this reason, among others.  In "
            (code/inline "'upstream") " mode, at seed 17, the chain stops with the parameter
            error, raised when the inactive branch is built or updated with x ≤ 0 (at seeds
            15 to 19 too; at seed 20 the chain runs, delaying the inactive updates, and gives
            P(x > 0) = 0.3995); the beta model stops too.")
         (p "The other side of C22: of the " (code/inline "(exn hakaru10)")
            " errors only parameter errors reject.  Any other " (code/inline "(exn hakaru10)")
            " error of an inactive node propagates in extended mode, as upstream, at
            construction (the build guard) and at an update (the guarded update) alike.  An
            error that is not an " (code/inline "(exn hakaru10)") " one, such as "
            (code/inline "car") " of " (code/inline "'()") " in a lifted function, is one that
            an inactive Dirac node defers (M:99), and in extended mode it rejects too, raised
            by the node's own parameters or by forcing a deferred value (case B6).  In "
            (code/inline "invalid-inactive/branch")
            " the inner test of the else branch is 0, not a boolean, whenever c holds: with
            p = 0.5 the initial trace at seed 17 has c true and the build raises "
            (code/inline "branch") "; with p = 0.05 it starts with c false, and the first
            update that proposes c true raises " (code/inline "branch") " instead of
            rejecting the step, which would keep the chain at c = #f.  In "
            (code/inline "'upstream") " mode the build raises too, while the update, which
            leaves the inactive branch alone, runs the chain through c = #t.")
         (p "And of the nodes only the inactive ones reject: an active node's parameter error
            propagates in " (code/inline "'extended") " mode as in "
            (code/inline "'upstream") ", at construction (a normal of standard deviation 0)
            and at an update.  In " (code/inline "active-invalid")
            " the observation, always active, gets a beta whose first parameter is -1 when b
            is false: the initial trace at seed 17 has b true, and the first step that
            proposes b false raises " (code/inline "parameter") " instead of rejecting.")
         ,@(shown 'invalid-inactive 'invalid-inactive/beta 'invalid-inactive/branch
                  'active-invalid)))

  ((test/semantics/deferred _)
   (let1 (thirds (map (λ (v) (cons v (/ 1. 3.))) '(a b none)))
     (⊦ (equal-approx? 1e-12) thirds (exact-posterior guarded-car))
     (⊦ (equal-approx? 1e-12) thirds (exact-posterior guarded-car #:validate #f))
     (for-each (λ (semantics)
                 (⊦ (equal-approx? .02) thirds
                    (proportions (mcmC 6000 guarded-car #:semantics semantics)))
                 (⊦ equal? (mcmC 300 guarded-car #:semantics semantics)
                    (mcmC 300 guarded-car #:semantics semantics #:check #t)))
               '(upstream extended)))
   (⊦ equal? '(none none b none a) (mcmC 5 guarded-car))
   (let1 (exact (exact-posterior nested-guarded-car))
     (⊦ (equal-approx? 1e-12)
        (map cons '((#f . none) (#f . other) (#t . a) (#t . b) (#t . none))
             (map (λ (w) (/ w 6.)) '(1. 2. 1. 1. 1.)))
        exact)
     (for-each (λ (semantics)
                 (⊦ (equal-approx? .03) (list semantics exact)
                    (list semantics
                          (proportions (mcmC 20000 nested-guarded-car #:semantics semantics))))
                 (⊦ equal? (mcmC 300 nested-guarded-car #:semantics semantics)
                    (mcmC 300 nested-guarded-car #:semantics semantics #:check #t)))
               '(upstream extended)))
   (⊦ equal? '()
      (filter-map (Λ ((,name . ,m))
                    (let1 (residual (kernel-residual m))
                      (and (not (< residual 1e-12)) (cons name residual))))
                  `((guarded-car . ,guarded-car) (guarded-car/reversed . ,guarded-car/reversed)
                    (nested-guarded-car . ,nested-guarded-car))))
   (⊦ equal? '(a none) (list (car (mcmC 1 guarded-car/reversed)) (car (mcmC 1 guarded-car))))
   (⊦ equal? '(exn none exn)
      (map (λ (m) (raised (τ (mcmC 3 m))))
           (list (strict-reader '()) (strict-reader '(5)) strict-reader/node)))
   (⊦ equal? '() (mcmC 3 (strict-reader '()) #:semantics 'extended))
   (⊦ every flonum? (mcmC 300 strict-reader/node #:semantics 'extended))
   (let1 (inner #f)
     (mcmC 1 (model (let* ((c (dist bern .5)))
                      (if_ #t
                           c
                           (diracN (lift (λ (k)
                                           (set! inner
                                             (raised (τ (exact-posterior
                                                          (model (diracN (lift car '())))
                                                          #:validate #f))))
                                           k)
                                         1))))))
     (⊦ eq? 'exn inner))
   `(doc (p "B6: deferred Dirac values (M:99, M:120-129).  Upstream never forces the value of a
            Dirac node that nothing reads, so in " (code/inline "guarded-car")
            " the else branch, inactive exactly when xs is empty, applies " (code/inline "car")
            " to " (code/inline "'()") " harmlessly.  Here the value of an inactive diracN node
            whose computation raises is a deferred value, which only a strict reader forces.
            Exact enumeration, with and without the validation build, gives 1/3 to each of "
            (code/inline "a") ", " (code/inline "b") " and " (code/inline "none")
            "; 6000 samples of the chain land within .02 of it in both semantics, and "
            (code/inline "#:check") " finds every state sound; the first five samples at seed
            17 are a regression value.")
         ,@(shown 'guarded-car)
         (p "In " (code/inline "nested-guarded-car") " the car sits under a second "
            (code/inline "if_") ", on c: when xs is empty and c holds, the inner if_ is
            inactive and its exit copies the deferred value without forcing it, which an "
            (code/inline "'extended") " step, updating that inactive exit, must do too;
            forcing it there would reject every step to (#t . none), whose probability is
            1/6.  The chains of 20000 samples land within .03 of the exact posterior in both
            semantics, and " (code/inline "#:check") " finds every state sound.  The kernel
            residual of the 'extended semantics (hakaru10-exact.scm, E10) is below "
            (m "10⁻¹²") " on both models, and on " (code/inline "guarded-car/reversed")
            ", whose initial trace has the else branch active (a) where guarded-car's has it
            inactive (none).  The residual enumerates and loads every assignment of the
            extended space, the else branch's car of " (code/inline "'()")
            " included, which it must compute as lazily as the chain does, whichever
            assignment the node was last computed in.")
         ,@(shown 'nested-guarded-car 'guarded-car/reversed)
         (p "Deviation 11, strict propagation: " (code/inline "strict-reader")
            "'s mu is the cdr of a pair whose car is the deferred h, so upstream computes mu =
            1.0, while here mu is the deferred value itself and the normal, a strict reader,
            raises " (code/inline "car") "'s error, a plain Scheme one, both with a constant
            test (" (code/inline "'()") ") and with a lifted one; with a non-empty xs the
            branch is active and nothing is deferred.  In " (code/inline "'extended")
            " mode the error rejects instead (C22): every build of "
            (code/inline "(strict-reader '())") " is rejected, so the chain is empty, and "
            (code/inline "strict-reader/node") " never takes its then branch.")
         ,@(shown 'strict-reader 'strict-reader/node)
         (p "Lazy lifting does not leak: a model run from inside an inactive branch's lifted
            function, here an " (code/inline "exact-posterior") " of "
            (code/inline "(diracN (lift car '()))") ", starts without it and raises.")))

  ((test/semantics/escapes _)
   (let ((halves '((0. . .5) (1. . .5)))
         (sixths (map (λ (v) (cons v (/ 1. 6.))) '(caught (0 #f) (1 #f) (1 #t) (2 #f) (2 #t)))))
     (⊦ (equal-approx? 1e-12) sixths (exact-posterior guarded-inside-lift))
     (⊦ (equal-approx? .03) halves (likelihood-weighting 4000 guarded-after-choice))
     (⊦ (equal-approx? .03) sixths (likelihood-weighting 4000 guarded-across-choices))
     (if native-multishot-escapes?
         (begin
           (⊦ (equal-approx? 1e-12) halves (exact-posterior guarded-after-choice))
           (⊦ (equal-approx? 1e-12) sixths (exact-posterior guarded-across-choices)))
         (⊦ equal? '(construction construction)
            (map (λ (m) (raised (τ (exact-posterior m))))
                 (list guarded-after-choice guarded-across-choices))))
     (let* ((build-handled 0)
            (tree (condition-case (model->search-tree guarded-after-choice)
                    (e (exn) (add1! build-handled) '())))
            (explored (raised (τ (probcc-explore +inf.0 tree)))))
       (⊦ equal? (list (if native-multishot-escapes? 'none 'construction) 0)
          (list explored build-handled))))
   (let1 (stale #f)
     (mcmC 1 (model (let* ((x (dist bern .5))) (set! stale x) x)))
     (⊦ eq? 'construction
        (raised (τ (mcmC 2 (model (handle-exceptions e 0. (dist normal stale 1.))))))))
   `(doc (p "B7: handlers that a model sets up itself.  " (code/inline "guarded-after-choice")
            " and " (code/inline "guarded-across-choices") " raise, after a choice, an error
            that their own " (code/inline "handle-exceptions") " catches; they are written for
            the value handlers, where a choice is a plain value.  The forward sampler, one-shot,
            gives their posteriors (1/2 each, and 1/6 to each of six outcomes) within .03.  Exact
            enumeration resumes the continuation of a choice once per support point, and the
            handler's escape must return to the resumption it was raised in.  CHICKEN main
            (rev 55615603) sends it to the " (code/inline "reset") " of an earlier run
            instead, which would drop the rest of the enumeration and answer a normalized but
            wrong posterior: there " (code/inline "native-multishot-escapes?")
            " is false and " (code/inline "exact-posterior") " raises "
            (code/inline "construction") ".  So does the exploration of the tree that "
            (code/inline "model->search-tree") " answers, built in one call and explored in
            another: the error reaches the handler around the exploration, which resumed the
            model, and not the handler around the build, which has returned.  On the
            chicken-core branch delimcc-multishot the same calls answer the exact posteriors.  A guard around no operation, inside a
            lifted function as in " (code/inline "guarded-inside-lift")
            ", enumerates correctly on both.")
         ,@(shown 'guarded-after-choice 'guarded-across-choices 'guarded-inside-lift)
         (p "A handler runs outside the model: an error that the trace builder raises (here a
            node of another chain, " (code/inline "construction") ") reaches the caller of "
            (code/inline "mcmC") ", not the model's own " (code/inline "handle-exceptions")
            ".")))

  ((test/semantics/incremental _)
   (for-each (λ (semantics)
               (⊦ equal? (list semantics '((2 4) (3 4) (4)))
                  (list semantics
                        (map car (hist (update-traces 1000 pdep #:semantics semantics)))))
               (for-each (λ (m)
                           (⊦ equal? (list semantics '(() (2 3 4)))
                              (list semantics
                                    (map car (hist (update-traces 1000 m #:semantics semantics)))))
                           (⊦ = 300 (length (mcmC 300 m #:semantics semantics #:check #t))))
                         (list upp1 upp2)))
             '(upstream extended))
   `(doc (p "B8: incremental recomputation (P §4.2 l.765-800).  Every node's update is wrapped
            to record, step by step, which updates ran and in which order.  In "
            (code/inline "pdep") " resampling x updates y (whose LL changes, not its value) and
            the Dirac result, never z; resampling y updates z and the result; resampling z
            the result alone:")
         (cite/quote
           "Oleg Kiselyov"
           "Suppose MH proposes to resample x (and only x). Although the y sample keeps its old
           value, its distribution parameters, x specifically, changed. Therefore, the LL of
           the old y sample in the new distribution has to be recomputed. The variable z is
           not affected by the resampling proposal; also, the parameters of its distribution
           remain the same. Therefore, no update to the z node is needed.")
         ,@(shown 'pdep 'update-traces)
         (p "The propagation-order note of Metropolis.hs (M:319-397): in "
            (code/inline "upp1") " and " (code/inline "upp2") ", d depends on a, b and c,
            and resampling a updates b, c and d, each once, in this order, d last; resampling
            d updates nothing.  Both models also run under " (code/inline "#:check #t")
            ", whose reference evaluator recomputes every node after each step, in both
            semantics.")
         ,@(shown 'upp1 'upp2))))

(unittest/✓ hakaru10-language)
(unittest/✓ hakaru10-replay)
(unittest/✓ hakaru10-laws)
(unittest/✓ hakaru10-statistics)
(unittest/✓ hakaru10-semantics)
