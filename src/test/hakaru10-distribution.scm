; The suite for (aux hakaru10 distribution): the distributions of Distribution.hs, the DistK
; smart constructors of Metropolis.hs and `condition`.  Cases D1-D15 of the plan (WP2), D16 (the
; samplers replay a transcription of upstream draw by draw) and D17-D18 (the optional extras
; laplace and symmetric dirichlet, WP2b), then make-distribution, distribution-support and the
; record printers.  Expected values are upstream's records where they
; exist, exact values otherwise, and the Python transcription of Distribution.hs over the
; random-1.0.1.1 StdGen for the replays (prototype regression values).

(import scheme
        (chicken base)
        (chicken flonum)
        (only (chicken format) format)
        (except (chicken condition) condition)
        srfi-1
        (aux base)
        (aux unittest)
        (aux hakaru10 random)
        (aux hakaru10 distribution))

; the `kind` of the (exn hakaru10) condition that thunk raises, or 'none.
(define (hakaru10-kind thunk)
  (condition-case (begin (thunk) 'none)
    (e (hakaru10) (hakaru10-error-kind e))))

; the message of the (exn hakaru10) condition that thunk raises, or #f.
(define (hakaru10-message thunk)
  (condition-case (begin (thunk) #f)
    (e (hakaru10) ((condition-property-accessor 'exn 'message) e))))

(define (ld d x) (distribution-log-density d x))

; n samples of the distk d from a stdgen source seeded with `seed`, in drawing order.
(define (samples d n #!optional (seed 17))
  (let1 (src (make-stdgen-source seed))
    (let loop ((i 0) (acc '()))
      (if (= i n)
          (reverse acc)
          (receive (x ll) (distribution-sample d src)
            (loop (add1 i) (cons x acc)))))))

; (mean . unbiased variance) of a list of reals, by the folds of (aux base).
(define (mean+var xs) (cons (foldr/avg xs) (foldr/var xs)))

; the trapezoid rule for exp (log-density of d) over [lo, hi] in n steps.
(define (trapezoid d lo hi n)
  (let* ((h (/ (- hi lo) n))
         (f (λ (i) (exp (ld d (+ lo (* i h)))))))
    (let loop ((i 1) (acc (/ (+ (f 0) (f n)) 2.)))
      (if (= i n) (* acc h) (loop (add1 i) (+ acc (f i)))))))

; the total probability of a distk's finite support.
(define (support-mass d) (foldr/add (map cdr (distribution-support d))))

; the seeds of a stdgen source, as a list.
(define (seeds src) (let1 (g (rng-source-state src)) (list (stdgen-s1 g) (stdgen-s2 g))))

; the first three samples of d from seed 17 and the seeds of the generator after 200 of them.
(define (replay d)
  (let* ((src (make-stdgen-source 17))
         (draw (λ () (receive (x ll) (distribution-sample d src) x)))
         (a (draw))
         (b (draw))
         (c (draw)))
    (do ((i 3 (add1 i))) ((= i 200)) (draw))
    (cons (list a b c) (seeds src))))

(define-suite hakaru10-distribution

  ((doc r) `((structure/section "Distributions")
             (p "The distributions of Hakaru10 "
                (cite/a "https://okmij.org/ftp/kakuritu/Hakaru10/Distribution.hs" "Distribution.hs")
                " are values of two type classes: a " (code/inline "Distribution")
                " has a log-density and a sampler, and a resampleable one (an "
                (code/inline "RDistribution") ") has a proposal kernel too, by default an
                independence proposal that samples again.  The existential "
                (code/inline "SomeRDistr") " hides the instance (Distribution.hs:14-41):")
             (code/lang "haskell" "class Distribution d a | d -> a where
  logDensity :: d -> a -> LogLikelihood
  sample     :: RandomGen g => d -> g -> (a, LogLikelihood, g)

-- re-sampleable distribution. This is the place to specify
-- custom proposals
class Distribution d a => RDistribution d a | d -> a where
  resample :: RandomGen g =>
     d ->
     a -> LogLikelihood -> -- old value and its LL
     g ->
     ((a,              -- new sample
       LogLikelihood,  -- its likelihood
       g),             -- new seed
      LogLikelihood)   -- (fwd - bwd) for the proposal step.
  -- By default, resample is really sample again
  resample d _ llold g =
    let s@(_,ll,_) = sample d g in
    (s,ll-llold)")
             (p "In Scheme " (code/inline "SomeRDistr") " is the record "
                (code/inline "rdistr") " of closures (" (code/inline "log-density") ", "
                (code/inline "sample") ", " (code/inline "resample") ", plus a finite "
                (code/inline "support") " for the exact enumerator), and the generator is an "
                (i "rng source") " that the sampler draws from in place.  The engine wraps a
                distribution in a " (code/inline "distk") " (Metropolis.hs:46-60), whose "
                (code/inline "kind") " is " (code/inline "resampleable") ", "
                (code/inline "observed") " (a conditioned distribution: its value is fixed, its
                log-likelihood follows its parameters) or " (code/inline "dirac")
                " (a deterministic node).  The user-level distributions are the smart
                constructors " (code/inline "bern") ", " (code/inline "normal")
                " and so on, upstream's " (code/inline "bernS") ", " (code/inline "normalS")
                ", ... (Metropolis.hs:70-95), which (aux hakaru10) re-exports as named.")
             (p "Log-densities are the true ones, normalization included, as upstream insists
                (Distribution.hs:93-96):")
             (code/lang "haskell" "-- Make sure we report the real LL rather than up to the constant
-- (which may depend on the argument to |categorical|).
-- When logDensity is called during the scoring of a proposal,
-- the parameters of the distribution have likely changed")
             (p "because the parameters of a node's distribution change when its parents are
                resampled, and the Metropolis-Hastings ratio compares log-likelihoods computed
                under different parameters.")
             (p "The samplers are the classical ones: Marsaglia's polar method for the normal
                distribution (Marsaglia and Bray, 1964); "
                (cite/a "https://doi.org/10.1145/358407.358414"
                        "G. Marsaglia and W. W. Tsang, A Simple Method for Generating Gamma
                        Variables, ACM TOMS 26(3), 2000")
                " for the gamma distribution; Joehnk's method (1964) for a beta distribution
                with both parameters at most 1, and the ratio of two gamma variates otherwise;
                Knuth's multiplication method (The Art of Computer Programming, vol. 2,
                §3.4.1) for the Poisson distribution.  The log-gamma function is Macleod's "
                (cite/a "http://people.sc.fsu.edu/~burkardt/f_src/asa245/asa245.html"
                        "Algorithm AS 245")
                " (Applied Statistics 38(2), 1989), transcribed rather than taken from libm,
                accurate to 10-12 significant digits.")
             (p "Every Double expression is transcribed in Haskell's association, and every
                sampler draws exactly where upstream draws: with the random-1.0.1.1 StdGen of
                (aux hakaru10 random), the samples are upstream's, bit for bit (D16).  Four
                upstream defects are fixed, each tagged in the source with its deviation
                number: the gamma density (deviation 2), the gamma sampler for a shape below 1
                (deviation 3), the log-likelihood of a Poisson sample (deviation 4) and the
                categorical walk exhausted by rounding (deviation 5).")
             (code/scheme/file "../aux.hakaru10.distribution.scm")))

  ((test/normal-density _)
   (⊦ (equal-approx? 1e-15) -0.9239385332046727 (ld (normal 0 1) 0.1))
   (⊦ (equal-approx? 1e-15) -0.9239385332046727 (ld (normal 0 1) 1/10))
   (⊦ (equal-approx? 1e-15) (- (+ (log 2.) (* .5 (log (* 2. 3.141592653589793)))))
     (ld (normal 1 2) 1.))
   `(doc (p "D1.  The log-density of the standard normal distribution at 0.1 is "
            (m "−½(0.01 + log 2π) = −0.9239385332046727")
            ".  The paper quotes " (m "-0.616") " for this LL (design.pdf p.14, design.txt
            l.695), an erratum.  The density keeps upstream's form:")
         (code/lang "haskell" "data Normal = Normal !Double !Double !Double !Double
normal :: Double -> Double -> Normal
normal mu sd = Normal mu sd tau (log (tau / pi / 2))
 where tau = 1 / (sd*sd)

instance Distribution Normal Double where
  logDensity (Normal mu sd tau lf) x =
   (-tau * square (x - mu) + lf) / 2 where square y = y * y")
         (p "cf. Distribution.hs:155-162.")))

  ((test/bern-density _)
   (let1 (d (bern .4))
     (⊦ (equal-approx? 1e-15) -0.916290731874155 (ld d #t))
     (⊦ (equal-approx? 1e-15) -0.5108256237659907 (ld d #f))
     (⊦ equal? '(bern (.4)) (list (rdistr-name (distk-rd d)) (rdistr-params (distk-rd d)))))
   (⊦ equal? '(.5) (rdistr-params (distk-rd (bern 1/2))))
   `(doc (p "D2.  " (code/inline "bern 0.4") " has the log-likelihoods "
            (m "log 0.4") " and " (m "log 0.6") ", computed once; " (m "log(1 − p)")
            " is computed plainly, as upstream does, not with " (code/inline "log1p")
            ", so that replays stay exact.  Exact parameters become flonums, as Haskell's "
            (code/inline "Double") " literals are:")
         (code/lang "haskell" "data Bern = Bern !Double !Double !Double
bern :: Double -> Bern
bern p = Bern p (log p) (log (1-p))

instance Distribution Bern Bool where
  logDensity (Bern _ lT lF) x = if x then lT else lF
  sample     (Bern p lT lF) g =
    let (t, g') = randomR (0, 1) g in
    if t <= p then (True, lT, g') else (False, lF, g')")
         (p "cf. Distribution.hs:53-62.")))

  ((test/uniform-density _)
   (let1 (d (uniform 10 20))
     (⊦ equal? (list -2.3025850929940455 -2.3025850929940455 -2.3025850929940455 -inf.0)
       (map (λ (x) (ld d x)) '(15 10 20 9.99))))
   `(doc (p "D3.  " (code/inline "uniform 10 20") " has the log-density "
            (m "log ⅒") " on the closed interval and " (m "−∞")
            " outside:")
         (code/lang "haskell" "uniform lo hi = Uniform lo hi (log (recip (hi - lo)))

instance Distribution Uniform Double where
  logDensity (Uniform lo hi l) x =
    if lo <= x && x <= hi then l else minusInf")
         (p "cf. Distribution.hs:107-117.")))

  ((test/log-gamma _)
   (⊦ (equal-approx? 1e-14) '(1.0957979947868837 3.1780538330716963 7.534364243376615
                              39.33988418719949)
     (map log-gamma '(.3 5. 7.5 20.)))
   (⊦ (equal-approx? 1e-8) (map libm-lgamma '(.3 1.5 3.9 5. 7.5 11.9 20. 1e7))
     (map log-gamma '(.3 1.5 3.9 5 7.5 11.9 20 1e7)))
   (⊦ equal? '(+inf.0 +inf.0 +inf.0) (map log-gamma '(0 -1. 1.5e308)))
   `(doc (p "D4.  " (code/inline "log-gamma") " is AS 245: at 0.3, 5, 7.5 and 20 it answers "
            "1.0957979947868837, 3.1780538330716963, 7.534364243376615 and 39.33988418719949
            (the prototype's values, through all four branches); it agrees with libm's "
            (code/inline "lgamma") " within " (m "10⁻⁸") " from 0.3 to " (m "10⁷")
            " (where only Stirling's leading terms are kept); it is " (m "+∞")
            " at 0, below 0 and above " (m "10³⁰⁸") ":")
         (code/lang "haskell" "logGamma :: Double -> Double
logGamma x
    | x <= 0    = m_pos_inf
    -- Handle positive infinity. logGamma overflows before 1e308 so
    -- it's safe
    | x > 1e308 = m_pos_inf
    -- Normal cases
    | x < 1.5   = a + c *
                  ((((r1_4 * b + r1_3) * b + r1_2) * b + r1_1) * b + r1_0) /
                  ((((b + r1_8) * b + r1_7) * b + r1_6) * b + r1_5)
    | x < 4     = (x - 2) *
                  ((((r2_4 * x + r2_3) * x + r2_2) * x + r2_1) * x + r2_0) /
                  ((((x + r2_8) * x + r2_7) * x + r2_6) * x + r2_5)
    | x < 12    = ((((r3_4 * x + r3_3) * x + r3_2) * x + r3_1) * x + r3_0) /
                  ((((x + r3_8) * x + r3_7) * x + r3_6) * x + r3_5)
    | x > 3e6   = k
    | otherwise = k + x1 *
                  ((r4_2 * x2 + r4_1) * x2 + r4_0) /
                  ((x2 + r4_4) * x2 + r4_3)")
         (p "cf. Distribution.hs:212-272 (D4, D4b, D4c).")))

  ((test/gamma-density _)
   (let1 (d (gamma 3 1/3))
     (⊦ (equal-approx? 1e-12) -0.397310314667487 (ld d 1))
     (⊦ (equal-approx? 1e-9) (- (+ (* 3. (log 3.)) (* 2. (log 1.))) 3. (log 2.)) (ld d 1.))
     (⊦ = -inf.0 (ld d -.5)))
   (⊦ = -inf.0 (ld (gamma 0 1) 1.))
   (⊦ = -inf.0 (ld (gamma 2 -1) 1.))
   (⊦ (equal-approx? 1e-12) (- (log .5) .5) (ld (gamma 1 2) 1.))
   (⊦ (equal-approx? 1e-9) (- (+ (* 3. (log 3.)) (* 2. (log 2.))) 6. (log 2.)) (ld (gamma 3 1/3) 2.))
   (⊦ (equal-approx? 1e-12) (- (- (log 2.)) 1.5) (ld (gamma 1 2) 3.))
   `(doc (p "D5, deviation 2.  Upstream's gamma log-density is "
            (m "θ log k − θx + (k − 1) log x − log Γ(k)")
            " for shape " (m "k") " and scale " (m "θ")
            ", the density of no gamma distribution (Distribution.hs:298-304):")
         (code/lang "haskell" "gammaLogDensity :: Double -> Double -> Double -> Double
gammaLogDensity shape scl | shape > 0 && scl > 0 =
  let logShape =  log shape
      logGShape = logGamma shape in
  \\x -> if x < 0 then minusInf else
          scl * logShape - scl * x + (shape - 1) * log x - logGShape
gammaLogDensity _ _ = \\_ -> minusInf")
         (p "The port uses the scale form, the one " (code/inline "gamma_rng") " samples:")
         (displaymath (m "log p(x) = −k log θ − x/θ + (k − 1) log x − log Γ(k)"))
         (p "so " (code/inline "gamma 3 (1/3)") " at 1 is "
            (m "3 log 3 − 3 − log 2 = −0.3973103145556162") " (closed form), "
            "-0.397310314667487 through AS 245; " (m "−∞") " below 0, and for a
            non-positive shape or scale, as upstream.  At 1, where "
            (m "log x = 0") ", the term " (m "(k − 1) log x") " vanishes, so the density is
            also checked away from 1: " (code/inline "gamma 3 (1/3)") " at 2 is "
            (m "3 log 3 + 2 log 2 − 6 − log 2") " (within " (m "10⁻⁹")
            ", through AS 245), and " (code/inline "gamma 1 2") " at 3 is "
            (m "−log 2 − 3/2") ".")))

  ((test/poisson-density _)
   (⊦ (equal-approx? 1e-12) '(-1. -1. -1.6931471805599454 -2.791759467549525)
     (map (λ (k) (ld (poisson 1) k)) '(0 1 2 3)))
   (⊦ eqv? -0. (ld (poisson 0) 0))
   (⊦ = -inf.0 (ld (poisson 0) 1))
   (⊦ equal? '(-inf.0 -inf.0) (map (λ (k) (ld (poisson 1) k)) '(1.5 -1)))
   (⊦ = -1.6931471805599454 (ld (poisson 1) 2.))
   (⊦ eq? 'parameter (hakaru10-kind (τ (poisson -1))))
   (⊦ equal? "In Poisson distribution, lambda cannot be negative"
     (hakaru10-message (τ (poisson -1))))
   `(doc (p "D6.  The Poisson log-density at " (m "k = 0, 1, 2, 3") " for "
            (m "λ = 1") ", through its special cases; " (m "λ = 0")
            " is the Dirac distribution at 0; a negative " (m "λ") " is an error when
            the distribution is built.  A non-integer has log-density " (m "−∞")
            " (Haskell's " (code/inline "Int") " type, deviation 9):")
         (code/lang "haskell" "poisson :: Double -> Poisson
poisson lambda | lambda >= 0 = Poisson lambda (exp(-lambda))
poisson lambda = error \"In Poisson distribution, lambda cannot be negative\"

-- if lambda = 0 then poisson(0) = dirac(0)
instance Distribution Poisson Int where
  -- density is (lambda ^ x) * exp(-lambda) / x!
  logDensity (Poisson lambda expl) x
    | x == 0      = -lambda
    | lambda == 0 = minusInf
    | x == 1      = log lambda - lambda
    | x == 2      = log (lambda*lambda/2) - lambda
    | otherwise   = fromIntegral x * log lambda - lambda
                     - logGamma (fromIntegral x + 1)")
         (p "cf. Distribution.hs:352-367.")))

  ((test/categorical-density _)
   (let1 (d (categorical '((a . 1) (b . 3))))
     (⊦ (equal-approx? 1e-15) (list (log .25) (log .75) -inf.0) (map (λ (x) (ld d x)) '(a b c))))
   (let1 (d (categorical '((a . 1) (b . 2) (a . 1))))
     (⊦ = (log .25) (ld d 'a))
     (⊦ equal? '((a . .25) (b . .5) (a . .25)) (distribution-support d)))
   (⊦ equal? '((x . 1.) (y . 3.)) (car (rdistr-params (distk-rd (categorical '((x . 1) (y . 3)))))))
   (⊦ eq? 'parameter (hakaru10-kind (τ (categorical '()))))
   (let1 (d (categorical '(((a) . 1) ("b" . 3))))
     (⊦ (equal-approx? 1e-15) (list (log .25) (log .75) -inf.0)
       (map (λ (x) (ld d x)) (list (list 'a) (string #\b) (list 'b)))))
   `(doc (p "D7.  " (code/inline "categorical [(a,1),(b,3)]") " normalizes its weights: "
            (m "log ¼") ", " (m "log ¾") ", and " (m "−∞")
            " for any other value; keys are compared with " (code/inline "equal?")
            ", Haskell's " (code/inline "Eq") ", so a freshly consed " (code/inline "(a)")
            " or string " (code/inline "\"b\"") " finds its entry.  With duplicate keys upstream is asymmetric, and so is the
            port (plan C10): the sampler gives a key the sum of its weights, the log-density
            only its first (" (code/inline "lookup") "); the support lists every entry, in
            the sampler's order:")
         (code/lang "haskell" "data Categorical a = Categorical ![(a,Double)]
categorical :: Eq a => [(a,Double)] -> Categorical a
categorical lst =
  let ptotal = sum $ map snd lst in
  Categorical $ ptotal `seq` map (\\ (i,p) -> (i,p/ptotal)) lst

instance Eq a => Distribution (Categorical a) a where
  logDensity (Categorical lst) x = maybe minusInf log $ (lookup x lst)
  sample     (Categorical lst) g = loop p lst
    where
      (p, g1) = randomR (0, 1) g
      loop p ((x,p0):t) | p < p0 = (x,log p0,g1)
                        | otherwise = loop (p-p0) t")
         (p "cf. Distribution.hs:87-91, 97-103.  The walk has no " (code/inline "[]")
            " case: when rounding exhausts it, the port answers the last entry (deviation 5).")))

  ((test/uniformly _)
   (⊦ eq? 'parameter (hakaru10-kind (τ (uniformly '()))))
   (⊦ equal? "uniformly: empty list" (hakaru10-message (τ (uniformly '()))))
   (let1 (d (uniformly '(a b c)))
     (⊦ = (- (log 3.)) (ld d 'b))
     (⊦ = -inf.0 (ld d 'z))
     (⊦ equal? '(a b c) (map car (distribution-support d))))
   (let1 (d (uniformly '((a) "b")))
     (⊦ equal? (list (- (log 2.)) (- (log 2.)) -inf.0)
       (map (λ (x) (ld d x)) (list (list 'a) (string #\b) (list 'b)))))
   `(doc (p "D8.  " (code/inline "uniformly") " on the empty list is an error; on "
            (code/inline "[a,b,c]") " every member has log-density " (m "−log 3")
            " (by " (code/inline "equal?") ", Haskell's " (code/inline "elem")
            ", so on " (code/inline "[(a),\"b\"]") " a freshly consed " (code/inline "(a)")
            " or string " (code/inline "\"b\"") " has " (m "−log 2") "), anything else "
            (m "−∞") ":")
         (code/lang "haskell" "uniformly :: Eq a => [a] -> Uniformly a
uniformly [] = error \"uniformly: empty list\"
uniformly lst = Uniformly (A.listArray (0,len-1) lst) (- log (fromIntegral len))
 where len = length lst

instance Eq a => Distribution (Uniformly a) a where
  logDensity (Uniformly a ll) x = if x `elem` a then ll else minusInf
  sample     (Uniformly a ll) g = let (i,g1) = randomR (A.bounds a) g in
                           (unsafeAt a i, ll, g1)")
         (p "cf. Distribution.hs:73-85.")))

  ((test/beta-density _)
   (⊦ (equal-approx? 1e-8) (log 1.5) (ld (beta 2 2) .5))
   (⊦ = (+ (+ (- (- (log-gamma 4.) (log-gamma 2.)) (log-gamma 2.)) (log .5)) (log .5))
     (ld (beta 2 2) .5))
   (⊦ eq? 'support (hakaru10-kind (τ (ld (beta 2 2) 1.5))))
   (⊦ equal? "beta: value must be between 0 and 1" (hakaru10-message (τ (ld (beta 2 2) 1.5))))
   (⊦ eq? 'parameter (hakaru10-kind (τ (ld (beta 0 2) .5))))
   (⊦ equal? "beta: parameters must be positve" (hakaru10-message (τ (ld (beta 0 2) .5))))
   `(doc (p "D9.  " (code/inline "beta 2 2") " at " (m "½") " is " (m "log 1.5")
            " within " (m "10⁻⁸") ", the accuracy of AS 245 at " (m "log Γ(4)")
            " (exactly " (m "log Γ(4) − 2 log Γ(2) + 2 log ½") " through log-gamma).  Upstream raises errors, and so does the port (plan C9), for a value outside "
            (m "[0,1]") " and for a non-positive parameter, both when a log-density is computed:")
         (code/lang "haskell" "betaLogDensity :: Double -> Double -> Double -> Double
betaLogDensity a b | a <= 0 || b <= 0 =
  error \"beta: parameters must be positve\"
betaLogDensity a b =
  let factor = logGamma (a + b)
               - logGamma a
               - logGamma b
  in \\x -> if x < 0 || x > 1 then error \"beta: value must be between 0 and 1\"
           else factor + (a - 1) * log x
                       + (b - 1) * log (1 - x)")
         (p "cf. Distribution.hs:330-339.")))

  ((test/normalization _)
   (⊦ (equal-approx? 1e-12) 1. (+ (exp (ld (bern .3) #t)) (exp (ld (bern .3) #f))))
   (⊦ (equal-approx? 1e-12) 1. (fold + 0. (map (λ (x) (exp (ld (uniformly '(a b c d e f g)) x)))
                                             '(a b c d e f g))))
   (⊦ (equal-approx? 1e-12) 1. (fold + 0. (map (λ (x) (exp (ld (categorical '((a . 1) (b . 3) (c . 2.5))) x)))
                                             '(a b c))))
   (⊦ (equal-approx? 1e-12) '(1. 1. 1.)
     (map support-mass (list (bern .3) (uniformly '(a b c d e f g))
                             (categorical '((a . 1) (b . 3) (c . 2.5))))))
   (⊦ (equal-approx? 1e-9) 1. (fold + 0. (map (λ (k) (exp (ld (poisson 3.5) k))) (iota 61))))
   (⊦ (equal-approx? 1e-4) '(1. 1. 1. 1.)
     (list (trapezoid (normal 1 2) -19. 21. 20000)
           (trapezoid (gamma 3 1/3) 0. 12. 12000)
           (trapezoid (gamma 2 1) 0. 40. 40000)
           (trapezoid (beta 2 5) 0. 1. 10000)))
   `(doc (p "D10.  The log-densities are normalized: the probabilities of " (code/inline "bern")
            ", " (code/inline "uniformly") " and " (code/inline "categorical")
            " sum to 1, as do their supports; the Poisson ones for " (m "λ = 3.5")
            " up to " (m "k = 60") "; and the trapezoid rule integrates the densities of "
            (code/inline "normal 1 2") ", the corrected " (code/inline "gamma 3 (1/3)")
            " and " (code/inline "gamma 2 1") ", and " (code/inline "beta 2 5")
            " to 1 within " (m "10⁻⁴") ".  Upstream's gamma density would fail this test.
            The second gamma has " (m "kθ = 2") ": a density with " (m "x^k")
            " in place of " (m "x^(k−1)") " integrates to " (m "kθ")
            ", which is 1 for the first.")))

  ((test/sampler-moments _)
   (let* ((within (λ (tol expected got) (⊦ (equal-approx? tol) expected got)))
          (n 20000))
     (let1 (mv (mean+var (samples (normal 10 .5) n)))
       (within .03 10. (car mv))
       (within .02 .25 (cdr mv)))
     (let1 (mv (mean+var (samples (gamma 3 1/3) n)))
       (within .03 1. (car mv))
       (within .03 (/ 1. 3.) (cdr mv)))
     (let1 (mv (mean+var (samples (gamma .5 2) n)))
       (within .05 1. (car mv))
       (within .25 2. (cdr mv)))
     (within .01 (/ 2. 7.) (car (mean+var (samples (beta 2 5) n))))
     (let1 (mv (mean+var (samples (beta .5 .5) n)))
       (within .02 .5 (car mv))
       (within .01 .125 (cdr mv)))
     (let1 (mv (mean+var (samples (poisson 3.5) n)))
       (within .06 3.5 (car mv))
       (within .15 3.5 (cdr mv)))
     (within .1 15. (car (mean+var (samples (uniform 10 20) n))))
     (within .02 .3 (/ (count identity (samples (bern .3) n)) (exact->inexact n))))
   `(doc (p "D11.  Moments of " (m "20000") " samples, seed 17, within about four standard
            errors: " (code/inline "normal 10 0.5") ", " (code/inline "gamma 3 (1/3)")
            " (mean 1, variance " (m "⅓") "), " (code/inline "beta 2 5") " (mean "
            (m "²⁄₇") "), " (code/inline "beta 0.5 0.5") " (Joehnk: mean "
            (m "½") ", variance " (m "⅛") "), " (code/inline "poisson 3.5")
            ", " (code/inline "uniform 10 20") " and " (code/inline "bern 0.3") ".")
         (p "Deviation 3: " (code/inline "gamma 0.5 2") " has mean 1 and variance 2.  For a
            shape below 1 upstream boosts the shape by 1 but keeps the scale, then multiplies
            by the scale again, so its samples have mean 2 and variance 8:")
         (code/lang "haskell" "gamma_rng shape scl g | shape <  1.0  = (gvar2, g2)
                      where (gvar1, g1) = gamma_rng (shape + 1) scl g
                            (w,  g2) = randomR (0,1) g1
                            gvar2 = scl * gvar1 * (w ** recip shape) ")
         (p "The port draws the boosted variate with scale 1: the same draws, a correct
            sample (Distribution.hs:279-282).")))

  ((test/poisson-sample-ll _)
   (let1 (src (make-stdgen-source 17))
     (⊨ (every (λ (i) (receive (k ll) (distribution-sample (poisson 3.5) src)
                        (= ll (ld (poisson 3.5) k))))
               (iota 1000))))
   (⊦ equal? '(0 0.) (receive (k ll) (distribution-sample (poisson 0) (make-stdgen-source 17))
                       (list k ll)))
   `(doc (p "D12, deviation 4.  The log-likelihood of a Poisson sample is the log-density of
            the sampled " (m "k") "; upstream's Knuth sampler answers the probability "
            (code/inline "acc*expl") " itself in the LL position:")
         (code/lang "haskell" "  -- Knuth algorithm
  sample (Poisson lambda expl) g
    | lambda == 0 = (0,0,g)
    | otherwise = let (u, g1) = randomR (0, 1) g
                  in loop u 1 1 g1
    where
      -- acc is lamda^(k-1)/(k-1)!
      loop p k acc g | p > expl =
           let (u, g1) = randomR (0, 1) g
           in loop (p*u) (k+1) (acc*lambda/(fromIntegral k)) g1
                     | otherwise = (k-1, acc*expl, g)")
         (p "cf. Distribution.hs:369-379.")))

  ((test/sampler-errors _)
   (⊦ eq? 'parameter (hakaru10-kind (τ (distribution-sample (normal 0 0) (make-stdgen-source)))))
   (⊦ equal? "normal: invalid parameters"
     (hakaru10-message (τ (distribution-sample (normal 0 0) (make-stdgen-source)))))
   (⊦ eq? 'parameter (hakaru10-kind (τ (distribution-sample (gamma -1 1) (make-stdgen-source)))))
   (⊦ equal? "gamma: got a negative shape paramater"
     (hakaru10-message (τ (distribution-sample (gamma -1 1) (make-stdgen-source)))))
   (⊦ equal? "gamma: got a negative scale paramater"
     (hakaru10-message (τ (distribution-sample (gamma 2 0) (make-stdgen-source)))))
   (⊦ eq? 'parameter (hakaru10-kind (τ (normal 'zero 1))))
   (let1 (src (make-stdgen-source 17))
     (⊦ equal? '(parameter parameter parameter)
       (map (λ (a b) (hakaru10-kind (τ (distribution-sample (beta a b) src)))) '(-.5 .5 0) '(1 -2 1)))
     (⊦ equal? '(18 1) (seeds src)))
   `(doc (p "D13.  Building " (code/inline "normal 0 0") " is fine, as upstream (only "
            (code/inline "normal_rng") " checks the deviation); sampling from it is an error, as is sampling
            from a gamma distribution with a non-positive shape or scale; the messages are
            upstream's, typos included.  A parameter that is not a real number is an error
            when the distribution is built (Haskell's " (code/inline "Double")
            " type, deviation 9).  Sampling a beta distribution with a non-positive
            parameter raises upstream's density error at once, without drawing; upstream's
            Joehnk loop never ends for a negative parameter (deviation 10):")
         (code/lang "haskell" "normal_rng mu sd g | sd > 0 = case marsaglia g of
                                ((x, _), g1) -> (mu + sd * x, g1)
normal_rng _ _ _ = error \"normal: invalid parameters\"

gamma_rng shape _   _ | shape <= 0.0  = error \"gamma: got a negative shape paramater\"
gamma_rng _     scl _ | scl <= 0.0  = error \"gamma: got a negative scale paramater\"

beta_rng a b g | a <= 1.0 && b <= 1.0 =
                 let (u, g1) = randomR (0.0, 1.0) g
                     (v, g2) = randomR (0.0, 1.0) g1
                     x = u ** (recip a)
                     y = v ** (recip b)
                 in  case (x+y) <= 1.0 of
                       True -> (x / (x + y), g2)
                       False -> beta_rng a b g2")
         (p "cf. Distribution.hs:148-153, 276-278, 317-325.")))

  ((test/bern-flip _)
   (let* ((d (bern/flip .3))
          (rd (distk-rd d))
          (src (make-stdgen-source 17))
          (before (seeds src)))
     (⊦ equal? (list #f (log .7) 0.)
       (receive (x ll fb) ((rdistr-resample rd) src #t (log .3)) (list x ll fb)))
     (⊦ equal? before (seeds src)))
   (let* ((rd (distk-rd (bern .3)))
          (src (make-stdgen-source 17)))
     (⊦ equal? (list #t (log .3) (- (log .3) (log .7)))
       (receive (x ll fb) ((rdistr-resample rd) src #f (log .7)) (list x ll fb))))
   `(doc (p "D14.  " (code/inline "bern/flip") " has upstream's commented-out kernel, which
            always flips the value, draws nothing and has " (m "fwd − bwd = 0")
            "; " (code/inline "bern") " keeps the default kernel, which samples again (here "
            (m "0.29389… ≤ 0.3") ", so " (code/inline "#t") ") and answers "
            (m "ll′ − ll") ":")
         (code/lang "haskell" "-- resampling always flips, so fwd and bwd steps are 0
instance RDistribution Bern Bool where
{-
  resample d x _ g =
    let x' = not x in
    ((x', logDensity d x', g),0)
-}")
         (p "cf. Distribution.hs:64-70 and 29-32.")))

  ((test/condition _)
   (let* ((d (normal 0 1))
          (o (condition 1.5 d)))
     (⊦ eq? 'observed (distk-kind o))
     (⊦ = 1.5 (distk-value o))
     (⊦ eq? (distk-rd d) (distk-rd o))
     (let1 (src (make-stdgen-source 17))
       (⊦ equal? (list 1.5 (ld d 1.5)) (receive (x ll) (distk-sample o src) (list x ll)))
       (⊦ equal? '(18 1) (seeds src))))
   (let1 (o ((condition 1. normal) 0 1))
     (⊦ eq? 'observed (distk-kind o))
     (⊦ equal? '(normal (0. 1.)) (list (rdistr-name (distk-rd o)) (rdistr-params (distk-rd o)))))
   (⊦ eq? 'observed (distk-kind (((condition 2 (λ (a) (λ (b) (uniform a b)))) 0) 3)))
   (⊦ eq? 'conditioning (hakaru10-kind (τ (condition #t (condition #f (bern .5))))))
   (⊦ eq? 'conditioning (hakaru10-kind (τ (condition 1 (dirac 1)))))
   (⊦ eq? 'conditioning (hakaru10-kind (τ ((condition 1 dirac) 1))))
   (⊦ eq? 'conditioning (hakaru10-kind (τ (condition 1 42))))
   (⊦ eq? 'conditioning (hakaru10-kind (τ (condition 'x (bern .5) symbol?))))
   (⊦ equal? "observation must be external to the model"
     (hakaru10-message (τ (condition 'x (bern .5) symbol?))))
   (let1 (d (dirac 3))
     (⊦ equal? '(dirac #f) (list (distk-kind d) (distk-rd d))))
   (⊦ equal? '(3 0.) (receive (x ll) (distk-sample (dirac 3) (make-stdgen-source)) (list x ll)))
   (⊦ equal? '(0. -inf.0) (list (ld (dirac 3) 3) (ld (dirac 3) 4)))
   `(doc (p "D15.  Conditioning a resampleable distk makes an observed one that keeps the
            distribution and stores the observation; sampling it answers the observation and
            its log-likelihood and draws nothing.  On a constructor, " (code/inline "condition")
            " is lifted (curried constructors included) and checked when applied.
            Conditioning twice, or a Dirac distribution, is a type error upstream (design.txt
            l.397-402) and a " (code/inline "conditioning") " error here; so is an
            observation that the predicate passed by (aux hakaru10)'s "
            (code/inline "condition") ", the engine's " (code/inline "internal-part")
            ", recognizes as part of the model (an SExp or a Model, at any depth of the
            observation):")
         (code/lang "haskell" "conditioned :: a -> DistK KResampleable a -> DistK KObserved a
conditioned obs (Resampleable d) = Observed d obs

instance Condition (DistK KResampleable a) where
  type CondR (DistK KResampleable a) = DistK KObserved a
  type CondV (DistK KResampleable a) = a
  condition = conditioned

instance Condition b => Condition (a -> b) where
  type CondR (a -> b) = a -> CondR b
  type CondV (a -> b) = CondV b
  condition v f = \\x -> condition v (f x)")
         (p "cf. Metropolis.hs:434-435, Syntax.hs:180-193, and " (code/inline "distk-sample")
            ", upstream's " (code/inline "sampleK") " (Metropolis.hs:97-100):")
         (code/lang "haskell" "sampleK :: DistK k a -> StdGen -> (a, LL, StdGen)
sampleK (Resampleable d) g = sample d g
sampleK (Dirac x) g = (x,0,g)
sampleK (Observed d x) g = (x,logDensity d x,g)")))

  ((test/replay _)
   (⊦ equal? '((#t #f #t) 2040924989 1624004889) (replay (bern .3)))
   (⊦ equal? '((b d c) 33947404 1403159235) (replay (uniformly '(a b c d e))))
   (⊦ equal? '((b b b) 2040924989 1624004889) (replay (categorical '((a . 1) (b . 3) (c . 2)))))
   (⊦ equal? '((12.938995850243806 13.273788866478 11.967275951981044) 2040924989 1624004889)
     (replay (uniform 10 20)))
   (⊦ equal? '((9.3961204294208418 9.4123224314548182 9.4444092264612465) 238574758 1847970949)
     (replay (normal 10 .5)))
   (⊦ equal? '((0.3802248764663857 1.174022508712856 0.69596540693107489) 1634784357 1431524588)
     (replay (gamma 3 1/3)))
   (⊦ equal? '((1.6615848336718226 5.8913466116211248 3.2990853790311712) 1634784357 1431524588)
     (replay (gamma 2.5 2)))
   (⊦ equal? '((0.23083928668339262 0.36462234149461792 5.9938773598408153) 507113220 1216787979)
     (replay (gamma .5 2)))
   (⊦ equal? '((0.085999760921585497 0.1526198274930278 0.32347514171580588) 470875069 467317093)
     (replay (beta 2 5)))
   (⊦ equal? '((0.44626827673318087 0.088053382595321181 0.076213585831562139) 368601130 810754135)
     (replay (beta .5 .5)))
   (⊦ equal? '((0.16035609821837615 0.29562975594083019 0.26019878567376903) 609399710 1534428535)
     (replay (beta .5 2)))
   (⊦ equal? '((2 3 8) 473751772 1482242513) (replay (poisson 3.5)))
   `(doc (p "D16.  The samplers replay upstream's draws: from " (code/inline "mkStdGen 17")
            ", the first three samples of each distribution and the generator after 200
            samples are those of a Python transcription of Distribution.hs over the
            random-1.0.1.1 StdGen (with deviation 3 for " (code/inline "gamma 0.5 2")
            "), bit for bit.  That transcription reproduced the sample records of
            Examples1.hs, so these values are regression oracles: a change in the draw
            order (Marsaglia's retries, the gamma " (code/inline "until") " loop, Joehnk's
            retries, the categorical walk) or in the floating-point association shows here
            first.")))

  ((test/laplace _)
   (let1 (d (laplace 1 2))
     (⊦ (equal-approx? 1e-15) (- (- (log 4.)) 1.5) (ld d 4.))
     (⊦ (equal-approx? 1e-4) 1. (trapezoid d -39. 41. 40000))
     (⊦ equal? '((-0.062739873920549138 0.15302015298687754 -0.86557618415695647)
                 2040924989 1624004889)
       (replay d))
     (let1 (mv (mean+var (samples d 20000)))
       (⊦ (equal-approx? .1) 1. (car mv))
       (⊦ (equal-approx? .6) 8. (cdr mv))))
   (⊦ eq? 'parameter (hakaru10-kind (τ (distribution-sample (laplace 0 0) (make-stdgen-source)))))
   `(doc (p "D17 (optional extra).  The Laplace distribution, commented out upstream, by
            inversion of one uniform draw: log-density "
            (m "−log 2b − |x − μ|/b") ", normalized, replayed against the
            transcription, with mean " (m "μ") " and variance " (m "2b²") ":")
         (code/lang "haskell" "laplace_rng :: (RandomGen g) => Double -> Double -> g -> (Double, g)
laplace_rng mu sd g = sample (randomR (0.0, 1.0) g)
   where sample (u, g1) = case u < 0.5 of
                            True  -> (mu + sd * log (u + u), g1)
                            False -> (mu - sd * log (2.0 - u - u), g1)

laplaceLogDensity :: Floating a => a -> a -> a -> a
laplaceLogDensity mu sd x = - log (2 * sd) - abs (x - mu) / sd")
         (p "cf. Distribution.hs:384-395.  Sampling with " (m "b ≤ 0")
            " is an error, as for the normal distribution (deviation 10).")))

  ((test/dirichlet _)
   (let1 (d (dirichlet/symmetric 3 2))
     (⊦ (equal-approx? 1e-8) (log 3.6) (ld d (list .2 .3 .5)))
     (⊦ = -inf.0 (ld d (list .5 .5)))
     (⊦ eq? 'support (hakaru10-kind (τ (ld d (list .5 .5 0.)))))
     (⊦ equal? '(((0.29568912068716002 0.57253866818349808 0.1317722111293419)) 1901800541 817484612)
       (let1 (r (replay d)) (cons (list (car (car r))) (cdr r))))
     (let* ((xs (samples d 20000))
            (mv1 (mean+var (map first xs)))
            (mv3 (mean+var (map third xs))))
       (⊨ (every (λ (v) (< (abs (- (fold + 0. v) 1.)) 1e-12)) xs))
       (⊦ (equal-approx? .01) (/ 1. 3.) (car mv1))
       (⊦ (equal-approx? .01) (/ 1. 3.) (car mv3))
       (⊦ (equal-approx? .003) (/ 8. 252.) (cdr mv1))))
   (⊦ (equal-approx? 1e-8) (log 2.) (ld (dirichlet/symmetric 3 1) (list .1 .2 .7)))
   `(doc (p "D18 (optional extra).  The symmetric Dirichlet distribution, commented out
            upstream, over lists of " (m "n") " flonums: " (m "n") " gamma variates
            normalized by their total.  Its log-density at " (m "(0.2, 0.3, 0.5)")
            " for " (m "n = 3, α = 2") " is " (m "log(Γ(6) · 0.2 · 0.3 · 0.5 / Γ(2)³) = log 3.6") "; the flat " (m "α = 1") " has density " (m "Γ(3) = 2")
            "; samples sum to 1, with component means " (m "⅓") " and variance "
            (m "α(nα − α) / ((nα)² (nα + 1)) = 8/252")
            ":")
         (code/lang "haskell" "dirichlet_rng :: (RandomGen g) => Int ->  Double -> g -> ([Double], g)
dirichlet_rng n' a g' = normalize (gammas g' n')
  where gammas g 0 = ([], 0, g)
        gammas g n = let (xs, total, g1) = gammas g (n-1)
                         ( x, g2) = gamma_rng a 1 g1
                     in ((x : xs), x+total, g2)
        normalize (b, total, h) = (map (/ total) b, h)

dirichletLogDensity :: [Double] -> [Double] -> Double
dirichletLogDensity a x | all (> 0) x = sum' (zipWith logTerm a x) + logGamma (sum a)
  where sum' = foldl' (+) 0
        logTerm b y = (b-1) * log y - logGamma b
dirichletLogDensity _ _ = error \"dirichlet: all values must be between 0 and 1\"")
         (p "cf. Distribution.hs:397-411.  The first gamma variate drawn ends last in the
            list.  A value that is not a list of " (m "n") " reals has log-density "
            (m "−∞") " (deviation 10).")))

  ((test/make-distribution _)
   (let* ((d (make-distribution 'exponential
                                log-density: (λ (x) (if (< x 0.) -inf.0 (- x)))
                                sample: (λ (src) (let1 (x (- (log (- 1. (rng-double! src 0. .9)))))
                                                   (values x (- x))))
                                params: '(1.)))
          (rd (distk-rd d))
          (src (make-stdgen-source 17)))
     (⊦ eq? 'resampleable (distk-kind d))
     (⊦ equal? '(exponential (1.)) (list (rdistr-name rd) (rdistr-params rd)))
     (⊦ = -2. (ld d 2.))
     (⊨ (not (distribution-support d)))
     (receive (x ll fb) ((rdistr-resample rd) src 3. -3.)
       (⊦ = fb (+ ll 3.))))
   (let* ((d (make-distribution 'coin
                                log-density: (λ (x) (log .5))
                                sample: (λ (src) (values (= 0 (rng-integer! src 0 1)) (log .5)))
                                resample: (λ (src x ll) (values (not x) ll 0.))
                                support: (τ '((#t . .5) (#f . .5)))))
          (rd (distk-rd d)))
     (⊦ equal? '(#f 0.) (receive (x ll fb) ((rdistr-resample rd) (make-stdgen-source) #t (log .5))
                         (list x fb)))
     (⊦ = 1. (support-mass d)))
   (⊦ eq? 'construction (hakaru10-kind (τ (make-distribution 'broken sample: (λ (src) 0)))))
   `(doc (p "make-distribution builds a user distribution, the Scheme form of a new "
            (code/inline "RDistribution") " instance (Distribution.hs:14-41): a log-density, a
            sampler that answers a value and its log-likelihood, and optionally a proposal
            kernel (\"This is the place to specify custom proposals\", Distribution.hs:18-19),
            which defaults to sampling again with " (m "fwd − bwd = ll′ − ll") " (Distribution.hs:29-32), and a finite support for
            the exact enumerator.")))

  ((test/support-and-printing _)
   (⊦ equal? '((#t . .25) (#f . .75)) (distribution-support (bern 1/4)))
   (⊦ equal? '((#t . .25) (#f . .75)) (distribution-support (condition #f (bern/flip .25))))
   (⊦ equal? '((7 . 1.)) (distribution-support (dirac 7)))
   (⊦ equal? '(#f #f #f) (map distribution-support (list (normal 0 1) (poisson 2) (beta 1 1))))
   (let1 (d (uniformly '(x y z)))
     (⊨ (every (λ (x) (pair? (assq x (distribution-support d)))) (samples d 200))))
   (⊦ eq? 'parameter (hakaru10-kind (τ (distribution-support 'normal))))
   (⊦ equal? '("#<distk resampleable normal (0.0 1.0)>" "#<distk observed normal (0.0 1.0) 1.5>"
               "#<distk dirac (a b)>" "#<rdistr categorical (((\"x\" . 1.0)))>")
     (map (λ (x) (format "~a" x))
          (list (normal 0 1) (condition 1.5 (normal 0 1)) (dirac '(a b))
                (distk-rd (categorical '(("x" . 1)))))))
   `(doc (p (code/inline "distribution-support") " answers the finite support of a distk, an
            alist " (code/inline "((x . p) ...)") " in the order its sampler walks the values,
            or " (code/inline "#f") " for a distribution whose support is infinite or
            continuous; an observed distk keeps the support of the distribution it conditions,
            and a Dirac distk is certain of its value (Distribution.hs:49-51).  It is what the
            exact interpretation of a model branches over: where the trace-graph builder draws
            one value with " (code/inline "distk-sample") ", the exact enumerator branches over
            the support of each active random choice of the static graph (plan §6.10), each
            value weighted by its probability.")
         (p "Distks and their distributions print with their kind, name and parameters, as
            Metropolis.hs:62-65 shows a " (code/inline "DistK") ".")))

  )

(unittest/✓ hakaru10-distribution)
