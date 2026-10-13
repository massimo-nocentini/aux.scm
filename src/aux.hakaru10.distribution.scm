;; (aux hakaru10 distribution) -- the distributions of (aux hakaru10), a CHICKEN port of
;; Oleg Kiselyov's Hakaru10 (https://okmij.org/ftp/kakuritu/Hakaru10/index.html).
;;
;; Mirrors (paths relative to aux.scm/upstream/okmij-hakaru10/, citation keys as in the docs):
;;   D  Distribution.hs (https://okmij.org/ftp/kakuritu/Hakaru10/Distribution.hs): the
;;      Distribution/RDistribution classes and SomeRDistr (D:14-41), bern, uniformly,
;;      categorical, uniform, normal (Marsaglia polar), log-gamma (AS 245), gamma
;;      (Marsaglia-Tsang), beta (Joehnk), poisson (Knuth), and the extras laplace and symmetric
;;      dirichlet that upstream keeps commented out (D:383-411, plan WP2b);
;;   M  Metropolis.hs (https://okmij.org/ftp/kakuritu/Hakaru10/Metropolis.hs): DistK and its
;;      smart constructors (M:46-105), conditioned (M:434-435);
;;   S  Syntax.hs (https://okmij.org/ftp/kakuritu/Hakaru10/Syntax.hs): condition (S:174-193).
;; Not ported: D:127-146 (choose), D:169-210 (Atkinson Poisson), and dirac (D:45-51) as a
;; user-level distribution (the engine has its own Dirac kind).
;;
;; Hakaru10 code version: November 2016 (W:52-53); web page last updated July 14, 2018 (W:179).
;; Paper: O. Kiselyov, "Probabilistic Programming Language and its Incremental Evaluation",
;; APLAS 2016, LNCS 10017, DOI 10.1007/978-3-319-47958-3_19 (P = design.txt / design.pdf).
;;
;; Contents:
;;   - the re-export of the (exn hakaru10) error conditions of (aux hakaru10 random), raised by
;;     every module of (aux hakaru10);
;;   - the rdistr record (SomeRDistr: a record of closures) and the distk record (DistK);
;;   - log-gamma (AS 245) and, for the tests only, libm's lgamma;
;;   - the samplers (marsaglia, normal-rng, gamma-rng, beta-rng) and the distributions, each a
;;     procedure of its parameters answering a resampleable distk, so each is at once the
;;     distribution of D and its smart constructor of M:70-95 (whose Haskell names end in S:
;;     bernS, ...): bern, bern/flip, uniformly, categorical, uniform, normal, gamma, beta,
;;     poisson, and the extras laplace and dirichlet/symmetric; dirac (internal to the engine);
;;   - distk-sample (sampleK), conditioned and condition, the operations of Metropolis.hs and
;;     Syntax.hs on distks;
;;   - make-distribution (a user distribution), distribution-log-density, distribution-sample
;;     and distribution-support.
;;
;; A distribution is a value, not an effect: the handlers of (aux hakaru10 engine) give it
;; meaning.  The trace-graph builder and the forward sampler draw through `distk-sample`; the
;; exact enumerator resumes a random choice once per entry of `distribution-support`, the finite
;; support in the sampler's order (its handler's input, PLAN-IDIOM §2.3).
;;
;; Replay discipline (plan §6.1): every Double expression of Distribution.hs is transcribed with
;; binary fp+ fp- fp* fp/ in Haskell's association (`infixl 6 + -`, `infixl 7 * /`), `**` is
;; fpexpt, and the samplers draw from the rng source exactly where, and in the order in which,
;; upstream draws from its StdGen.  So the samples of the default stdgen source replay GHC's.
;; Every Double parameter is converted with exact->inexact when the distribution is built.
;;
;; Nothing is imported from (chicken condition): its `condition` would clash with
;; Hakaru10's (plan C15).

(module (aux hakaru10 distribution) *

  (import scheme
          (only (scheme base) exact-integer?)
          (chicken base)
          (chicken flonum)
          (chicken foreign)
          (only (chicken format) fprintf)
          (only (chicken module) reexport)
          (only srfi-1 every fold alist-copy make-list)
          (aux base)
          (aux hakaru10 random))

  ; No definition of this module is ever redefined, so the compiler may inline the record
  ; accessors and the helpers into the distributions, which the engine builds at every update.
  (declare (local))

  ;; ---------------------------------------------------------------------------------------
  ;; Errors (plan C16, §3.4): composite (exn hakaru10) conditions whose hakaru10 part carries a
  ;; `kind`, one of conditioning, construction, dependency, parameter, branch, support,
  ;; internal.  Upstream calls `error` (for example D:76, D:153, D:277-278, D:332, D:337,
  ;; D:356); the messages are upstream's strings, typos included.  They are defined in (aux
  ;; hakaru10 random), the lowest module that raises, and re-exported here for the modules
  ;; above.
  ;; ---------------------------------------------------------------------------------------

  (reexport (only (aux hakaru10 random) hakaru10-error hakaru10-error? hakaru10-error-kind))

  ; deviation 9 (D:107-166, D:306-381): Haskell's `Double` type becomes a run-time check; an
  ; exact real is converted to a flonum (plan §3.2: the `1/3` literal must become 0.333...).
  (define (->flonum who x)
    (cond
      ((flonum? x) x)
      ((real? x) (exact->inexact x))
      (else (hakaru10-error 'parameter who "a real number is expected" x))))

  ; (no upstream counterpart) rebinds each var to its value as a flonum, checked by ->flonum
  ; on behalf of `who`: the Double parameters of the data types of D (D:54, D:107, D:155, ...).
  (define-syntax-rule (let-flonums who (var ...) body ...)
    (let ((var (->flonum who var)) ...) body ...))

  ;; ---------------------------------------------------------------------------------------
  ;; SomeRDistr and DistK
  ;; ---------------------------------------------------------------------------------------

  ; cf. D:14-41: the classes Distribution (logDensity, sample) and RDistribution (resample)
  ; and the existential SomeRDistr, as a record of closures:
  ;   name         a symbol, for printing;
  ;   params       the list of the (converted) parameters, compared with equal? by the
  ;                reference evaluator (plan §6.9);
  ;   log-density  (λ (x) flonum), D:15;
  ;   sample       (λ (src) (values x ll)), D:16, src an rng source;
  ;   resample     (λ (src x ll) (values x' ll' fwd-bwd)), D:21-28: "This is the place to
  ;                specify custom proposals" (D:18-19);
  ;   support      #f, or a thunk answering the alist ((x . p) ...) of a finite support, in
  ;                the sampler's order (used by the exact enumerator, plan §6.10; new).
  (define-record rdistr name params log-density sample resample support)

  ; (debug printer only) #<rdistr normal (0. 1.)>.
  (set-record-printer! rdistr
    (λ (d port) (fprintf port "#<rdistr ~a ~s>" (rdistr-name d) (rdistr-params d))))

  ; cf. M:46-60 `data DistKind = KResampleable | KDirac | KObserved` and the GADT DistK: the
  ; type index becomes the symbol `kind`, one of resampleable, observed, dirac.  `rd` is the
  ; rdistr (#f for dirac); `value` is the observation of an observed distk and the theta of
  ; a dirac one (unused for a resampleable one).
  (define-record distk kind rd value)

  ; cf. M:62-65 `instance Show (DistK k a)`: #<distk resampleable normal (0. 1.)>,
  ; #<distk observed normal (0. 1.) 1.5>, #<distk dirac 3>.
  (set-record-printer! distk
    (λ (dk port)
      (let1 (rd (distk-rd dk))
        (fprintf port "#<distk ~a" (distk-kind dk))
        (when rd (fprintf port " ~a ~s" (rdistr-name rd) (rdistr-params rd)))
        (unless (eq? (distk-kind dk) 'resampleable) (fprintf port " ~s" (distk-value dk)))
        (display ">" port))))

  ; cf. D:29-32, the default `resample`: "By default, resample is really sample again", an
  ; independence proposal whose fwd - bwd is ll - llold.
  (define ((independence-resample sample) src x llold)
    (receive (x* ll) (sample src)
      (values x* ll (fp- ll llold))))

  ; cf. M:73-95 `Resampleable . Dist $ ...`: a resampleable distk over a new rdistr whose
  ; resample is the default one (D:29-32) unless one is given.
  (define (make-resampleable name params log-density sample #!optional (resample #f) (support #f))
    (make-distk 'resampleable
                (make-rdistr name params log-density sample
                             (or resample (independence-resample sample))
                             support)
                (void)))

  ; cf. D:163-164, D:312-313, D:347-348 `sample d g = let (x, g1) = rng g in (x, logDensity d
  ; x, g1)`: the sampler that draws x with (draw src) and answers it with its log-density.
  (define ((sample/log-density draw log-density) src)
    (let1 (x (draw src))
      (values x (log-density x))))

  ;; ---------------------------------------------------------------------------------------
  ;; Special functions
  ;; ---------------------------------------------------------------------------------------

  ; (no upstream counterpart) Horner's rule as AS 245 spells it out (D:234-245), in Haskell's
  ; association: (fp-horner x a b c ...) is ((a*x + b)*x + c)...; x must be a variable.
  (define-syntax fp-horner
    (syntax-rules ()
      ((_ x a) a)
      ((_ x a b c ...) (fp-horner x (fp+ (fp* a x) b) c ...))))

  ; (no upstream counterpart) the denominators of AS 245 (D:236, 239, 241, 245), whose leading
  ; coefficient is 1: (fp-horner/monic x b c ...) is ((x + b)*x + c)...
  (define-syntax-rule (fp-horner/monic x b c ...) (fp-horner x (fp+ x b) c ...))

  ; cf. D:212-272 `logGamma`, Macleod's Algorithm AS 245 ("an accuracy of 10-12 significant
  ; decimal digits, except for small regions around x = 1 and x = 2"), transcribed verbatim,
  ; branch by branch with the coefficients r1_4 ... r4_3 in Horner order, rather than libm's
  ; lgamma, so that replays stay exact.  +inf.0 for x <= 0 or x > 1e308 (D:229-232).
  (define (log-gamma x)
    (let-flonums 'log-gamma (x)
      (cond
        ((fp<= x 0.) +inf.0)
        ((fp> x 1e308) +inf.0)
        ((fp< x 1.5)
         (receive (a b c) (if (fp< x .5)
                              (values (fpneg (fplog x)) (fp+ x 1.) x)
                              (values 0. x (fp- x 1.)))
           (fp+ a (fp/ (fp* c (fp-horner b 3.13060547623 11.1667541262 -21.9698958928
                                         -24.4387534237 -2.66685511495))
                       (fp-horner/monic b 15.2346874070 31.4690115749 11.9400905721
                                        0.607771387771)))))
        ((fp< x 4.)
         (fp/ (fp* (fp- x 2.) (fp-horner x 4.16438922228 78.6994924154 137.519416416
                                         -142.046296688 -78.3359299449))
              (fp-horner/monic x 43.3400022514 263.505074721 313.399215894 47.0668766060)))
        ((fp< x 12.)
         (fp/ (fp-horner x -2.29660729780e3 -4.02621119975e4 2.74647644705e4 2.30661510616e5
                         -2.12159572323e5)
              (fp-horner/monic x -5.70691009324e2 -2.42357409629e4 -1.46025937511e5
                               -1.16328495004e5)))
        (else
          (let* ((y (fplog x))
                 (k (fp+ (fp- (fp* x (fp- y 1.)) (fp* .5 y)) 0.918938533204673)))
            (if (fp> x 3e6)
                k
                (let* ((x1 (fp/ 1. x))
                       (x2 (fp* x1 x1)))
                  (fp+ k (fp/ (fp* x1 (fp-horner x2 0.0692910599291889 0.4917317610505968
                                                 0.279195317918525))
                              (fp-horner/monic x2 6.012459259764103 3.350343815022304))))))))))

  ; (no upstream counterpart) libm's lgamma, for the tests only: accuracy checks of log-gamma
  ; (D:216-219).  libchicken links -lm.
  (define libm-lgamma (foreign-lambda double "lgamma" double))

  ;; ---------------------------------------------------------------------------------------
  ;; Samplers shared by several distributions
  ;; ---------------------------------------------------------------------------------------

  ; cf. D:119-125 `marsaglia`, the Marsaglia polar method: x then y from randomR (-1,1),
  ; s = x*x + y*y, q = sqrt ((-2) * log s / s), retried unless 1 >= s && s > 0.  Answers the
  ; first variate x*q only, the one normal_rng uses (D:151-152); y*q is never drawn again.
  (define (marsaglia src)
    (let* ((x (rng-double! src -1. 1.))
           (y (rng-double! src -1. 1.))
           (s (fp+ (fp* x x) (fp* y y))))
      (if (and (fp>= 1. s) (fp> s 0.))
          (fp* x (fpsqrt (fp/ (fp* -2. (fplog s)) s)))
          (marsaglia src))))

  ; cf. D:148-153 `normal_rng`: mu + sd * x when sd > 0, else "normal: invalid parameters".
  (define (normal-rng mu sd src)
    (if (fp> sd 0.)
        (fp+ mu (fp* sd (marsaglia src)))
        (hakaru10-error 'parameter 'normal "normal: invalid parameters" mu sd)))

  ; cf. D:283-296, the last clause of `gamma_rng`, Marsaglia and Tsang, "A Simple Method for
  ; Generating Gamma Variables", for shape >= 1: d = shape - 1/3, c = recip (sqrt (9*d)); v
  ; from normal_rng 1 c `until` v > 0 (the first draw counts); x = (v-1)/c, u from randomR
  ; (0,1); accept when u < 1 - 0.0331*(sqr*sqr) || log u < 0.5*sqr + d*(1 - v3 + log v3),
  ; answering scl*d*v3, else start again.
  (define (marsaglia-tsang shape scl src)
    (let* ((d (fp- shape (fp/ 1. 3.)))
           (c (fp/ 1. (fpsqrt (fp* 9. d)))))
      (let retry ()
        (let* ((v (let until ((v (normal-rng 1. c src)))
                    (if (fp> v 0.) v (until (normal-rng 1. c src)))))
               (x (fp/ (fp- v 1.) c))
               (sqr (fp* x x))
               (v3 (fp* (fp* v v) v))
               (u (rng-double! src 0. 1.)))
          (if (or (fp< u (fp- 1. (fp* 0.0331 (fp* sqr sqr))))
                  (fp< (fplog u) (fp+ (fp* .5 sqr) (fp* d (fp+ (fp- 1. v3) (fplog v3))))))
              (fp* (fp* scl d) v3)
              (retry))))))

  ; cf. D:274-282 `gamma_rng`: the parameter errors, the boost of a shape below 1 by a
  ; uniform w, then Marsaglia-Tsang.
  ; deviation 3 (D:279-282): for shape < 1 upstream draws gamma_rng (shape+1) scl and then
  ; multiplies by scl again, so the sample has scale scl^2; here the inner draw has scale 1,
  ; and scl * g * w ** recip shape is a Gamma(shape, scl) variate.  The draws are the same.
  (define (gamma-rng shape scl src)
    (cond
      ((fp<= shape 0.)
       (hakaru10-error 'parameter 'gamma "gamma: got a negative shape paramater" shape scl))
      ((fp<= scl 0.)
       (hakaru10-error 'parameter 'gamma "gamma: got a negative scale paramater" shape scl))
      ((fp< shape 1.)
       (let* ((g (gamma-rng (fp+ shape 1.) 1. src))
              (w (rng-double! src 0. 1.)))
         (fp* (fp* scl g) (fpexpt w (fp/ 1. shape)))))
      (else (marsaglia-tsang shape scl src))))

  ; cf. D:317-328 `beta_rng`: Joehnk's method when a <= 1 && b <= 1 (u then v from
  ; randomR (0,1), x = u ** recip a, y = v ** recip b, retried unless x + y <= 1, answering
  ; x / (x+y)); otherwise ga / (ga + gb) with ga = gamma_rng a 1 drawn before gb = gamma_rng b 1.
  ; deviation 10 (D:318-328, an extra parameter check): here sampling raises the parameter
  ; error of betaLogDensity (D:331-332) first, for a <= 0 or b <= 0, without drawing, so that
  ; the engine can reject such a state (plan C22).  Upstream differs by case:
  ;   - a < 0 or b < 0, both <= 1: the Joehnk loop does not end (u ** recip a >= 1, so
  ;     x + y > 1 at every retry) and that error is never reached;
  ;   - a = 0 or b = 0 (neither negative), both <= 1: it draws u and v and answers on the
  ;     first try (u ** recip 0 = 0 for u < 1), failing only when the log-likelihood forces
  ;     betaLogDensity, so it makes two draws that the port does not;
  ;   - a > 1 or b > 1: gamma_rng (D:277) raises gamma's error, after drawing ga (D:326) when
  ;     only b is out of range.
  (define (beta-rng a b src)
    (cond
      ((or (fp<= a 0.) (fp<= b 0.))
       (hakaru10-error 'parameter 'beta "beta: parameters must be positve" a b))
      ((and (fp<= a 1.) (fp<= b 1.))
       (let retry ()
         (let* ((u (rng-double! src 0. 1.))
                (v (rng-double! src 0. 1.))
                (x (fpexpt u (fp/ 1. a)))
                (y (fpexpt v (fp/ 1. b))))
           (if (fp<= (fp+ x y) 1.)
               (fp/ x (fp+ x y))
               (retry)))))
      (else
        (let* ((ga (gamma-rng a 1. src))
               (gb (gamma-rng b 1. src)))
          (fp/ ga (fp+ ga gb))))))

  ;; ---------------------------------------------------------------------------------------
  ;; The distributions, as resampleable distks (M:70-95)
  ;; ---------------------------------------------------------------------------------------

  ; cf. D:54-62 `bern p = Bern p (log p) (log (1-p))`: logDensity x = if x then lT else lF;
  ; sample draws t from randomR (0,1) and answers True when t <= p.  log (1-p) is computed
  ; plainly, not with log1p (plan C8), for bit-exact replay.  `kernel` maps the log-density
  ; to a resample procedure, or to #f for the default one.
  (define (bern-with name p kernel)
    (let-flonums name (p)
      (let* ((lT (fplog p))
             (lF (fplog (fp- 1. p)))
             (log-density (λ (x) (if x lT lF))))
        (make-resampleable name
                           (list p)
                           log-density
                           (λ (src) (if (fp<= (rng-double! src 0. 1.) p) (values #t lT) (values #f lF)))
                           (kernel log-density)
                           (τ `((#t . ,p) (#f . ,(fp- 1. p))))))))

  ; cf. M:73-74 `bernS d = Resampleable . Dist $ bern d`, with the default kernel: upstream's
  ; flipping kernel is commented out (D:65-70), so `instance RDistribution Bern Bool` keeps the
  ; independence proposal of D:29-32.
  (define (bern p) (bern-with 'bern p (K #f)))

  ; cf. D:64-70, the commented-out kernel "resampling always flips, so fwd and bwd steps are
  ; 0": x' = not x, its LL, fwd - bwd = 0, and no draw.
  (define ((flip-kernel log-density) src x ll)
    (let1 (x* (not x))
      (values x* (log-density x*) 0.)))

  ; cf. D:64-70: bern with the flipping kernel, kept for the flip replays (plan §3.2).
  (define (bern/flip p) (bern-with 'bern/flip p flip-kernel))

  ; cf. D:73-85 `uniformly`: "uniformly: empty list" for [] (D:76), ll = - log (fromIntegral
  ; len); logDensity x = ll when x `elem` the list (here by equal?), else minusInf; sample is
  ; the element at randomR (0, len-1) :: Int.  Also M:76-77 `uniformlyS`, its wrapper.
  (define (uniformly lst)
    (cond
      ((null? lst) (hakaru10-error 'parameter 'uniformly "uniformly: empty list"))
      ((not (list? lst)) (hakaru10-error 'parameter 'uniformly "a list is expected" lst))
      (else
        (let* ((len (exact->inexact (length lst)))
               (arr (list->vector lst))
               (hi (sub1 (vector-length arr)))
               (ll (fpneg (fplog len)))
               (p (fp/ 1. len)))
          (make-resampleable 'uniformly
                             (list lst)
                             (λ (x) (if (member x lst) ll -inf.0))
                             (λ (src) (values (vector-ref arr (rng-integer! src 0 hi)) ll))
                             #f
                             (τ (map (λ (x) (cons x p)) lst)))))))

  ; cf. D:99-103, the local `loop` of the categorical sampler: (x, log p0) at the first entry
  ; with p < p0, else on with p - p0.
  ; deviation 5 (D:99-103): when rounding exhausts the walk (no [] case upstream), the last
  ; entry is answered.
  (define (categorical-walk p alist)
    (let* ((entry (car alist))
           (p0 (cdr entry)))
      (if (or (fp< p p0) (null? (cdr alist)))
          (values (car entry) (fplog p0))
          (categorical-walk (fp- p p0) (cdr alist)))))

  ; cf. D:87-105 `categorical`: ptotal = sum of the weights (a left fold from 0), each weight
  ; normalized p/ptotal; logDensity is the log of the first `lookup` (here assoc, by equal?),
  ; else minusInf; sample draws p from randomR (0,1) and walks the list.  Also M:79-80
  ; `categoricalS`, its wrapper.
  ; Kept as upstream (plan C10): with duplicate keys, the sampler gives a key the sum of its
  ; weights, the density only its first weight (D:98-102); the support lists every entry.
  ; deviation 9 (D:88): the empty list is an error at construction, and each entry must be a
  ; pair with a real weight.
  (define (categorical alist)
    (unless (and (pair? alist) (list? alist) (every (λ (e) (and (pair? e) (real? (cdr e)))) alist))
      (hakaru10-error 'parameter 'categorical "a non-empty alist ((value . weight) ...) is expected"
                      alist))
    (let* ((weighted (map (λ (e) (cons (car e) (exact->inexact (cdr e)))) alist))
           (ptotal (fold (λ (e acc) (fp+ acc (cdr e))) 0. weighted))
           (normalized (map (λ (e) (cons (car e) (fp/ (cdr e) ptotal))) weighted)))
      (make-resampleable 'categorical
                         (list weighted)
                         (λ (x) (cond ((assoc x normalized) => (o fplog cdr)) (else -inf.0)))
                         (λ (src) (categorical-walk (rng-double! src 0. 1.) normalized))
                         #f
                         (τ (alist-copy normalized)))))

  ; cf. D:107-117 `uniform lo hi = Uniform lo hi (log (recip (hi - lo)))`: logDensity x = l
  ; when lo <= x && x <= hi, else minusInf; sample is randomR (lo, hi), with LL l.  Also
  ; M:82-83 `uniformS`, its wrapper.
  (define (uniform lo hi)
    (let-flonums 'uniform (lo hi)
      (let1 (l (fplog (fp/ 1. (fp- hi lo))))
        (make-resampleable 'uniform
                           (list lo hi)
                           (λ (x) (let-flonums 'uniform (x) (if (and (fp<= lo x) (fp<= x hi)) l -inf.0)))
                           (λ (src) (values (rng-double! src lo hi) l))))))

  ; cf. D:155-166 `normal mu sd = Normal mu sd tau (log (tau / pi / 2))` with
  ; tau = 1 / (sd*sd); logDensity x = (-tau * square (x - mu) + lf) / 2; sample is normal_rng
  ; with LL its logDensity.  The constructor does not check sd: only sampling fails
  ; (D:148-153).  Also M:85-86 `normalS`, its wrapper.
  (define (normal mu sd)
    (let-flonums 'normal (mu sd)
      (let* ((tau (fp/ 1. (fp* sd sd)))
             (lf (fplog (fp/ (fp/ tau 3.141592653589793) 2.)))
             (log-density (λ (x) (let-flonums 'normal (x)
                                   (let1 (y (fp- x mu))
                                     (fp/ (fp+ (fpneg (fp* tau (fp* y y))) lf) 2.))))))
        (make-resampleable 'normal
                           (list mu sd)
                           log-density
                           (sample/log-density (λ (src) (normal-rng mu sd src)) log-density)))))

  ; deviation 2 (D:298-304): upstream's `scl * log shape - scl * x + (shape-1) * log x -
  ; logGamma shape` is the density of no gamma parameterization; this is the scale form
  ; -shape*log scl - x/scl + (shape-1)*log x - log-gamma shape, which gamma_rng samples.  As
  ; upstream: minusInf for x < 0 (D:302), and for shape <= 0 or scl <= 0 (D:304).
  (define (gamma-log-density shape scl)
    (if (and (fp> shape 0.) (fp> scl 0.))
        (let ((shape-log-scl (fp* shape (fplog scl)))
              (log-g-shape (log-gamma shape))
              (shape-1 (fp- shape 1.)))
          (λ (x)
            (let-flonums 'gamma (x)
              (if (fp< x 0.)
                  -inf.0
                  (fp- (fp+ (fp- (fpneg shape-log-scl) (fp/ x scl)) (fp* shape-1 (fplog x)))
                       log-g-shape)))))
        (K -inf.0)))

  ; cf. D:306-315 `gamma shape scl`: sample is gamma_rng with LL its logDensity.  Also M:88-89
  ; `gammaS`, its wrapper.
  (define (gamma shape scl)
    (let-flonums 'gamma (shape scl)
      (let1 (log-density (gamma-log-density shape scl))
        (make-resampleable 'gamma
                           (list shape scl)
                           log-density
                           (sample/log-density (λ (src) (gamma-rng shape scl src)) log-density)))))

  ; cf. D:330-339 `betaLogDensity`: "beta: parameters must be positve" when a <= 0 || b <= 0,
  ; factor = logGamma (a+b) - logGamma a - logGamma b, "beta: value must be between 0 and 1"
  ; outside [0,1] (kept as upstream, plan C9), else factor + (a-1) * log x + (b-1) * log (1-x).
  ; Like upstream's lazy `error`, the parameter error is raised when a density is computed.
  (define (beta-log-density a b)
    (if (or (fp<= a 0.) (fp<= b 0.))
        (λ (x) (hakaru10-error 'parameter 'beta "beta: parameters must be positve" a b))
        (let ((factor (fp- (fp- (log-gamma (fp+ a b)) (log-gamma a)) (log-gamma b)))
              (a-1 (fp- a 1.))
              (b-1 (fp- b 1.)))
          (λ (x)
            (let-flonums 'beta (x)
              (if (or (fp< x 0.) (fp> x 1.))
                  (hakaru10-error 'support 'beta "beta: value must be between 0 and 1" x)
                  (fp+ (fp+ factor (fp* a-1 (fplog x))) (fp* b-1 (fplog (fp- 1. x))))))))))

  ; cf. D:341-350 `beta a b`: sample is beta_rng with LL its logDensity.  Also M:91-92
  ; `betaS`, its wrapper.
  (define (beta a b)
    (let-flonums 'beta (a b)
      (let1 (log-density (beta-log-density a b))
        (make-resampleable 'beta
                           (list a b)
                           log-density
                           (sample/log-density (λ (src) (beta-rng a b src)) log-density)))))

  ; cf. D:359-367, `logDensity (Poisson lambda expl)`: -lambda at 0; minusInf when lambda = 0;
  ; log lambda - lambda at 1; log (lambda*lambda/2) - lambda at 2; else x * log lambda - lambda
  ; - logGamma (x+1).
  ; deviation 9 (D:359): the Int type becomes a check: a non-integer has density minusInf, and
  ; an integral flonum counts as its integer.
  (define (poisson-log-density lam)
    (let1 (log-lam (fplog lam))
      (λ (x)
        (if (not (and (real? x) (integer? x)))
            -inf.0
            (let1 (x (inexact->exact x))
              (cond
                ((= x 0) (fpneg lam))
                ((fp= lam 0.) -inf.0)
                ((= x 1) (fp- log-lam lam))
                ((= x 2) (fp- (fplog (fp/ (fp* lam lam) 2.)) lam))
                (else (let1 (xf (exact->inexact x))
                        (fp- (fp- (fp* xf log-lam) lam) (log-gamma (fp+ xf 1.)))))))))))

  ; cf. D:369-379, Knuth's algorithm: u from randomR (0,1), and while p > expl draw u' and
  ; continue with p*u', k+1; answer k-1.  (acc, which only fed upstream's LL, is not
  ; computed: see deviation 4 at poisson.)
  (define (knuth-poisson expl src)
    (let loop ((p (rng-double! src 0. 1.)) (k 1))
      (if (fp> p expl)
          (loop (fp* p (rng-double! src 0. 1.)) (add1 k))
          (sub1 k))))

  ; cf. D:352-381 `poisson lambda`: an error unless lambda >= 0 (D:355-356), expl =
  ; exp (-lambda); sample is (0, 0) when lambda = 0, else Knuth's.  Also M:94-95 `poissonS`,
  ; its wrapper.
  ; deviation 4 (D:379): the LL of a sample is logDensity (k-1); upstream answers acc*expl, the
  ; probability itself, not its log.
  (define (poisson lam)
    (let-flonums 'poisson (lam)
      (unless (fp>= lam 0.)
        (hakaru10-error 'parameter 'poisson "In Poisson distribution, lambda cannot be negative"
                        lam))
      (let ((expl (fpexp (fpneg lam)))
            (log-density (poisson-log-density lam)))
        (make-resampleable 'poisson
                           (list lam)
                           log-density
                           (if (fp= lam 0.)
                               (λ (src) (values 0 0.))
                               (sample/log-density (λ (src) (knuth-poisson expl src))
                                                   log-density))))))

  ; cf. D:384-395, laplace (commented out upstream; optional extra, plan WP2b): sample draws u
  ; from randomR (0,1), answering mu + sd * log (u + u) when u < 0.5, else
  ; mu - sd * log (2 - u - u); logDensity x = - log (2*sd) - abs (x - mu) / sd.
  ; deviation 10 (D:384-391, an extra parameter check): sampling with sd <= 0 is a parameter
  ; error, as for normal (D:153).
  (define (laplace mu sd)
    (let-flonums 'laplace (mu sd)
      (let* ((log-2sd (fplog (fp* 2. sd)))
             (log-density (λ (x) (let-flonums 'laplace (x)
                                   (fp- (fpneg log-2sd) (fp/ (fpabs (fp- x mu)) sd)))))
             (draw (λ (src)
                     (unless (fp> sd 0.)
                       (hakaru10-error 'parameter 'laplace "laplace: invalid parameters" mu sd))
                     (let1 (u (rng-double! src 0. 1.))
                       (if (fp< u .5)
                           (fp+ mu (fp* sd (fplog (fp+ u u))))
                           (fp- mu (fp* sd (fplog (fp- (fp- 2. u) u)))))))))
        (make-resampleable 'laplace (list mu sd) log-density (sample/log-density draw log-density)))))

  ; cf. D:401-404, the local `gammas` of dirichlet_rng: n draws of gamma_rng a 1, recursing
  ; before drawing, so that the first variate drawn ends last in the list; answers the list and
  ; its total.
  (define (dirichlet-gammas n a src)
    (if (zero? n)
        (values '() 0.)
        (receive (xs total) (dirichlet-gammas (sub1 n) a src)
          (let1 (x (gamma-rng a 1. src))
            (values (cons x xs) (fp+ x total))))))

  ; cf. D:397-411, the symmetric dirichlet (commented out upstream; optional extra, plan WP2b),
  ; over lists of n flonums.  sample: the gammas, each divided by their total.  logDensity:
  ; "dirichlet: all values must be between 0 and 1" unless every x > 0, else the left fold of
  ; (a-1) * log y - logGamma a over the values, plus logGamma (sum of the alphas).
  ; deviation 10 (D:407-411, extra checks): a value that is not a list of n reals has density
  ; minusInf (upstream's zipWith would truncate); n must be a positive exact integer.
  (define (dirichlet/symmetric n a)
    (unless (and (exact-integer? n) (positive? n))
      (hakaru10-error 'parameter 'dirichlet/symmetric "a positive exact integer is expected" n))
    (let-flonums 'dirichlet/symmetric (a)
      (let* ((log-g-a (log-gamma a))
             (a-1 (fp- a 1.))
             (log-g-sum (log-gamma (fold fp+ 0. (make-list n a))))
             (log-term (λ (y acc) (fp+ acc (fp- (fp* a-1 (fplog (exact->inexact y))) log-g-a))))
             (log-density
               (λ (xs)
                 (cond
                   ((not (and (list? xs) (= (length xs) n) (every real? xs))) -inf.0)
                   ((not (every positive? xs))
                    (hakaru10-error 'support 'dirichlet/symmetric
                                    "dirichlet: all values must be between 0 and 1" xs))
                   (else (fp+ (fold log-term 0. xs) log-g-sum))))))
        (make-resampleable 'dirichlet/symmetric
                           (list n a)
                           log-density
                           (sample/log-density
                             (λ (src)
                               (receive (xs total) (dirichlet-gammas n a src)
                                 (map (λ (x) (fp/ x total)) xs)))
                             log-density)))))

  ; cf. M:70-71 `diracS = Dirac` (M:52-55: "Its value is fixed and its LogLikelihood is always
  ; 0").  Internal: the engine builds Dirac nodes with it; users write diracN.
  (define (dirac x) (make-distk 'dirac #f x))

  ;; ---------------------------------------------------------------------------------------
  ;; Operations on distks
  ;; ---------------------------------------------------------------------------------------

  ; cf. M:97-100 `sampleK`: (values x ll); a resampleable distk samples (drawing from src); a
  ; dirac one answers (x, 0) and an observed one (x, logDensity d x), neither drawing.
  (define (distk-sample dk src)
    (case (distk-kind dk)
      ((resampleable) ((rdistr-sample (distk-rd dk)) src))
      ((dirac) (values (distk-value dk) 0.))
      ((observed) (let1 (x (distk-value dk))
                    (values x ((rdistr-log-density (distk-rd dk)) x))))
      (else (hakaru10-error 'internal 'distk-sample "not a distk" dk))))

  ; cf. M:434-435 `conditioned obs (Resampleable d) = Observed d obs`; the GADT index made any
  ; other argument a type error (S:185-188), here a run-time one (deviation 9).
  (define (conditioned obs dk)
    (unless (distk? dk)
      (hakaru10-error 'conditioning 'condition "only distributions can be conditioned" dk))
    (case (distk-kind dk)
      ((resampleable) (make-distk 'observed (distk-rd dk) obs))
      ((observed) (hakaru10-error 'conditioning 'condition
                                  "an observed distribution cannot be conditioned again" obs dk))
      (else (hakaru10-error 'conditioning 'condition "a Dirac distribution cannot be conditioned"
                            obs dk))))

  ; cf. S:174-193 `class Condition`: on a resampleable distk it is `conditioned`; on a
  ; procedure (a distribution constructor) it is lifted, `condition v f = \x -> condition v
  ; (f x)`, for any arity and checked when applied.  Only resampleable distks can be
  ; conditioned: no double conditioning, no Dirac (S:185-188; P §2.3 l.397-402).
  ; deviation 9 (S:38-42, S:180-183): that observations are external to the model
  ; (`CondV a = a`, P l.412-421) is a type in Haskell; here `internal?`, a predicate on obs
  ; (#f for none), rejects them.  It is (aux hakaru10 engine)'s sexp?, which this module cannot
  ; import: (aux hakaru10)'s condition passes it.
  (define (condition obs d #!optional (internal? #f))
    (when (and internal? (internal? obs))
      (hakaru10-error 'conditioning 'condition "observation must be external to the model" obs))
    (if (procedure? d)
        (λ args (condition obs (apply d args)))
        (conditioned obs d)))

  ; cf. D:14-41: a user distribution, Resampleable . Dist of a new RDistribution instance
  ; (M:73-95), with a custom proposal if `resample` is given ("This is the place to specify
  ; custom proposals", D:18-19; the TODO of M:955).  `log-density` is (λ (x) flonum),
  ; `sample` (λ (src) (values x ll)), `resample` (λ (src x ll) (values x' ll' fwd-bwd))
  ; defaulting to the independence proposal of D:29-32, `support` #f or a thunk answering
  ; ((x . p) ...), `params` a list compared with equal? by the reference evaluator.
  (define (make-distribution name #!key log-density sample (resample #f) (support #f)
                             (params '()))
    (define (check ok? what)
      (unless ok? (hakaru10-error 'construction 'make-distribution what name)))
    (check (and (procedure? log-density) (procedure? sample))
           "log-density: and sample: procedures are required")
    (check (or (not resample) (procedure? resample)) "resample: must be a procedure or #f")
    (check (or (not support) (procedure? support)) "support: must be a thunk or #f")
    (make-resampleable name params log-density sample resample support))

  ; cf. D:15 `logDensity`, and D:49-50 for a dirac distk (0 at theta, by equal?, else
  ; minusInf).
  (define (distribution-log-density dk x)
    (unless (distk? dk) (hakaru10-error 'parameter 'distribution-log-density "not a distk" dk))
    (if (eq? (distk-kind dk) 'dirac)
        (if (equal? x (distk-value dk)) 0. -inf.0)
        ((rdistr-log-density (distk-rd dk)) x)))

  ; cf. D:16 `sample`, through distk-sample (M:97-100 `sampleK`): (values x ll).
  (define (distribution-sample dk src)
    (unless (distk? dk) (hakaru10-error 'parameter 'distribution-sample "not a distk" dk))
    (distk-sample dk src))

  ; (new, plan §6.10) the finite support ((x . p) ...) of a distk in its sampler's order, or #f:
  ; ((theta . 1.)) for a dirac distk, D:49-51.  The exact enumerator shifts over it.
  (define (distribution-support dk)
    (unless (distk? dk) (hakaru10-error 'parameter 'distribution-support "not a distk" dk))
    (if (eq? (distk-kind dk) 'dirac)
        (list (cons (distk-value dk) 1.))
        (let1 (support (rdistr-support (distk-rd dk)))
          (and support (support)))))

  )
