; The suite for the larger (aux hakaru10) examples: the population estimation of colored balls
; (ColoredBalls.hs; P §3 l.505-616), the bird migration model of Birds1.hs on synthetic data,
; and the Bayesian linear regression of the paper (P §3 l.461-504).  Cases C1-C5 of the plan
; (WP9, plan §9.6).
; Each model is a computation of the State monad, written with do/monad as ColoredBalls.hs and
; Birds1.hs write it with do (`Model a = MCMCM (SExp a)`, S:82; plan §15, C32): MCMC runs it,
; and the oracles of (aux hakaru10 exact) walk the trace graph of one of its builds (plan §15,
; C37), by exact enumeration and by forward sampling, which also simulates the synthetic data
; of C4 and C5 from the generative half of the models.  Expected values are exact (closed
; forms, least squares, graph counts) or statistical, checked at seed 17 with tolerances of
; about 4-5 sigma of the spread across seeds (C1's P(1) over seeds 1-20, C5's distance to least
; squares over seeds 1-12), or, where the chain mixes too slowly for that (C4), with a bound
; that holds on seeds 1-20 and fails for the prior-only model, a negative control; the
; synthetic data is simulated from seed 17 too.
; Models create their nodes when they run, in bind order (plan §15, C35; plan C18): do/monad
; top to bottom, and replicate/monad, traverse/monad, for-each/monad and fold/monad head first,
; upstream's sequence . replicate, mapM, mapM_/forM_ and foldM_; they have no Scheme effect,
; since a model may run several times (the oracles' build, the construction check's rebuild).
; C1′ is upstream's commented-out first colored-balls model (CB:46-72), C6 the plot query
; E:166-167 (E is Examples1.hs, CB ColoredBalls.hs, B1 Birds1.hs, S Syntax.hs, U Util.hs).

(import scheme
        (chicken base)
        srfi-1
        (aux base)
        (aux unittest)
        (only (chicken io) read-lines)
        (only (chicken file) create-temporary-file delete-file)
        (aux hakaru10)
        (only (aux hakaru10 exact) construction-check))

;; -------------------------------------------------------------------------------------------
;; Helpers
;; -------------------------------------------------------------------------------------------

; the sources of the definitions made with define/shown, newest first, for the docs.
(define sources '())

; (new) (define/shown name definition): make the definition, and keep its source under name.
(define-syntax-rule (define/shown name definition)
  (begin definition (push! (cons 'name 'definition) sources)))

; (new) the sources of the definitions of the names, as code/scheme shows them.
(define (shown . names) `(code/scheme ,@(map (cut alist-ref <> sources) names)))

; (new) the probability of the value v in a histogram ((value . probability) ...).
(define (probability v histogram) (alist-ref v histogram equal? 0.))

; (new) the largest difference between the probabilities two histograms give a value.
(define (histogram-distance h k)
  (fold (λ (v d) (max d (abs (- (probability v h) (probability v k)))))
        0.
        (delete-duplicates (append (map car h) (map car k)))))

; (new) a ⊦ predicate: two histograms whose distance is below tol.
(define ((histograms-within tol) h k) (< (histogram-distance h k) tol))

; (new) the normalized histogram of a chain, a non-empty list of samples.
(define (chain-posterior chain)
  (let1 (n (exact->inexact (length chain)))
    (map (Λ ((,v . ,k)) (cons v (/ k n))) (hist chain))))

; (new) the normalized histogram of the first n samples of model's chain (mcmC's keys).
(define (mcmc-posterior n model . keys) (chain-posterior (apply mcmC n model keys)))

; (new) the component-wise means of a non-empty list of equally long lists of numbers, by the
; fold of B1:208-209 (`foldl' (zipWith (+)) (repeat 0)`, then `map (/ length r)`).
(define (means rows)
  (let1 (sums (fold (λ (row acc) (map + row acc)) (map (K 0.) (car rows)) rows))
    (map (cut / <> (length rows)) sums)))

; (new) the value of one run of model by weighted-samples, the forward walk of the static graph
; (plan §6.10), on a stdgen source seeded with `seed`: the generative reading of the model,
; used to simulate data.
(define (forward-sample model #!key (seed 17))
  (car (car (weighted-samples 1 model #:seed seed))))

; (new) the circle constant, for the wind directions of synthetic-features.
(define pi (* 4. (atan 1.)))

;; -------------------------------------------------------------------------------------------
;; Colored balls (ColoredBalls.hs; P §3 l.505-616; Milch et al., BLOG chapter, Ex 1.1)
;; -------------------------------------------------------------------------------------------

; cf. CB:28-32 `data Color = Blue | Green` and `opposite_color`: the colors are the symbols
; blue and green.
(define/shown opposite-color
  (define (opposite-color color)
    (case color
      ((blue) 'green)
      ((green) 'blue))))

; cf. CB:34-36 `observed_color` (P l.527-530): the color seen of a ball of the given color,
; wrong with probability 0.2.
(define/shown observed-color
  (define (observed-color color)
    (categorical `((,color . .8) (,(opposite-color color) . .2)))))

; cf. CB:41-44 `balls_prior` (P l.531-544): the colors of balls 1..n, each uniformly blue or
; green, drawn in order by replicate/monad (`sequence . replicate n`); upstream's IntMap from
; the ball's index to its node (`M.fromList $ zip [1..] balls`) becomes an alist, read with
; alist-ref: the pre-allocation that replaces stochastic memoization (W:111, P footnote 5
; l.566-569).
(define/shown balls-prior
  (define (balls-prior n)
    (do/monad
      (← balls (replicate/monad n (dist uniformly '(blue green))))
      (return (map cons (iota n 1) balls)))))

; cf. CB:85-89, CB:96-98 and P l.554-561, 604-607: `foldr (\i rest -> if_ ((== i) <$> x)
; (branch i) rest) otherwise keys`, the nested if_ that dispatches on the value of the SExp x,
; the first key outermost; `otherwise` is a model.  The fold builds Models only, and if_ delays
; its branches (plan §15, C36), so it creates no node: the nodes are made when the outermost if_
; runs, in upstream's order.
(define/shown dispatch
  (define (dispatch x keys branch otherwise)
    (fold-right (λ (key rest) (if_ (lift (cut equal? <> key) x) (branch key) rest)) otherwise keys)))

; cf. CB:74-89 `balls_observe` (P l.563-594): for each observed color, in order (mapM_ is
; for-each/monad), a ball b is drawn among the nballs and the observation conditioned on b's
; true color, through the dispatch over the keys 1..maxBalls; balls beyond nballs are never
; drawn.
(define/shown balls-observe
  (define (balls-observe balls obs nballs max-balls)
    (>> (for-each/monad
          (λ (color)
            (do/monad
              (← b (dist uniformly (iota nballs 1)))
              (dispatch b (iota max-balls 1)
                        (λ (i) (dist (condition color observed-color) (alist-ref i balls)))
                        (return color))))
          obs)
        (return (void)))))

; cf. CB:91-99 `cballs_model` (P l.599-608): the colors of max-balls balls (CB:38-39 `maxBalls
; = 8`), the number of balls uniformly among 1..max-balls, and the observations dispatched on
; it; the model answers the number of balls.
(define/shown cballs-model
  (define (cballs-model obs #!optional (max-balls 8))
    (do/monad
      (← balls (balls-prior max-balls))
      (← nballs (dist uniformly (iota max-balls 1)))
      (dispatch nballs (iota max-balls 1)
                (λ (n) (balls-observe balls obs n max-balls))
                (return (void)))
      (return nballs))))

; cf. CB:51-71, upstream's first, commented-out `cballs_model`, which CB:46-49 explains away:
; each observation draws its ball b uniformly among 1..nballs, a support that depends on the
; random nballs (`(\n -> [1..n]) <$> nballs`, CB:60), and conditions the observation on b's
; true color through the dispatch over 1..maxBalls (CB:63-69; CB:66-68 inline
; `observed_color`).  The posterior is cballs-model's, but resampling nballs below some drawn
; b makes that b's log-density -inf, so single-site MH rejects it: nballs and the b's are
; strongly correlated, and the chain mixes badly.
(define/shown cballs-model/direct
  (define (cballs-model/direct obs #!optional (max-balls 8))
    (do/monad
      (← balls (balls-prior max-balls))
      (← nballs (dist uniformly (iota max-balls 1)))
      (for-each/monad
        (λ (color)
          (do/monad
            (← b (dist uniformly (lift (λ (n) (iota n 1)) nballs)))
            (dispatch b (iota max-balls 1)
                      (λ (i) (dist (condition color observed-color) (alist-ref i balls)))
                      (return color))))
        obs)
      (return nballs))))

; (new) the number of moves of a chain, the consecutive samples that differ.
(define (moves chain)
  (car (fold (Λ (,x (,n ,prev)) (list (if (equal? x prev) n (add1 n)) x))
             (list 0 (car chain))
             (cdr chain))))

; (new) the posterior of the number of balls in closed form, ((n . probability) ...): with n
; balls, k of them blue (probability C(n,k) 2^-n), a drawn ball looks blue with probability
; (.8k + .2(n-k))/n; the uniform prior over 1..max-balls cancels.
(define/shown cballs-closed-form
  (define (cballs-closed-form max-balls obs)
    (define (binomial n k) (fold (λ (i c) (/ (* c (- n i)) (add1 i))) 1 (iota k)))
    (define (seen color n k)
      (let1 (blue (/ (+ (* .8 k) (* .2 (- n k))) n))
        (if (eq? color 'blue) blue (- 1. blue))))
    (define (likelihood n)
      (fold (λ (k acc)
              (+ acc (* (binomial n k) (expt 2. (- n))
                        (fold (λ (color p) (* p (seen color n k))) 1. obs))))
            0.
            (iota (add1 n))))
    (let* ((ls (map likelihood (iota max-balls 1)))
           (z (fold + 0. ls)))
      (map (λ (n l) (cons n (/ l z))) (iota max-balls 1) ls))))

;; -------------------------------------------------------------------------------------------
;; Bird migration (Birds1.hs; W:120-177; BirdMigrationProblem.txt)
;; -------------------------------------------------------------------------------------------

; cf. B1:35-40 `gridDim = 4`, `nCells = gridDim * gridDim`, `nFeatures = 4`.
(define grid-dim 4)
(define n-cells (* grid-dim grid-dim))
(define n-features 4)

; cf. B1:90-91 `divMod (n-1) gridDim` and BMP:103-122 (Figure 1): cells are numbered from 1 in
; column-major (Matlab) order, cell 1 in the bottom-left corner; (column . row), from 0.
(define/shown cell->xy
  (define (cell->xy n #!optional (dim grid-dim))
    (let1 (m (sub1 n)) (cons (quotient m dim) (remainder m dim)))))

; cf. B1:90-92, the local `dist n m` of neighbors: the squared distance of two (x . y) cells.
(define/shown squared-distance
  (define squared-distance
    (Λ ((,ix . ,iy) (,jx . ,jy)) (+ (* (- ix jx) (- ix jx)) (* (- iy jy) (- iy jy))))))

; cf. B1:87-93 `neighbors`: the cells within d_max of n, n included, with `d_max = 18` the
; square of BMP:58-59's d_max = 3√2; on the 4x4 grid every cell is a neighbor of every cell.
(define/shown neighbors
  (define (neighbors n #!optional (dim grid-dim))
    (let1 (from (cell->xy n dim))
      (filter (λ (m) (<= (squared-distance from (cell->xy m dim)) 18)) (iota (* dim dim) 1)))))

; cf. B1:82-85 `phi`, BMP:64: the potential beta . x of a transition, the dot product of its
; features fs and the parameters bs, `sum $ zipWith (*) fs bs`.
(define/shown phi
  (define (phi fs bs) (fold + 0. (map * fs bs))))

; cf. B1:95-99 `theta`, BMP:66-72: exp phi, the unnormalized probability of a transition;
; categorical normalizes it over the neighbors (D:87-91).
(define/shown theta
  (define (theta fs bs) (exp (phi fs bs))))

; cf. B1:59 `trans n bs = [ (j,theta year day n j features bs) | j<-neighbors n]`: the weights
; of the categorical over the cells the bird may fly to from n on the night after `day`, where
; (features year day from to) answers the four features (B1:47 `Features`).
(define/shown transitions
  (define ((transitions features year day) n bs)
    (map (λ (j) (cons j (theta (features year day n j) bs))) (neighbors n))))

; cf. B1:76-80 `birdpos`: the cell where the bird was seen on that day, if it was seen in exactly
; one cell, else #f; (obs year day cell) answers the count (B1:42-44 `ObsData`).
(define/shown birdpos
  (define (birdpos obs year day)
    (match/first (filter (λ (i) (positive? (obs year day i))) (iota n-cells 1))
      ((,i) i)
      (else #f))))

; cf. B1:50-74 `model` (W:135-153): the four parameters with the prior |N(0,10)| (BMP:457-464),
; B1:52-55's `abs <$> dist normal 0 10` made four times by replicate/monad; then, each year
; (forM_, for-each/monad), the bird starts in cell 1 (B1:73) and flies each night (foldM_,
; fold/monad) to a cell drawn from the categorical of theta (B1:68), where every cell's count
; is a Poisson observation of the indicator of the bird being there (B1:69-71, BMP:62 and
; 134-138).  With `pin` (the default),
; the cell of a day where birdpos finds the bird is observed rather than drawn: the alternative
; B1:62-67 that upstream comments out.  The posterior is the same, since a Poisson of intensity
; 0 cannot count a bird (D:362), but B1:68's drawn cell must guess every observed day, so its
; initial trace has a finite LL with a negligible probability (M:882-891 retries the build).
(define/shown birds
  (define (birds features obs #!key (years 30) (days 20) (pin #t))
    (do/monad
      (← betas (replicate/monad n-features (<$> (cut lift abs <>) (dist normal 0. 10.))))
      (let bs (collect betas))
      (for-each/monad
        (λ (year)
          (fold/monad
            (λ (n day)
              (do/monad
                (let day′ (add1 day))
                (let trans (lift (transitions features year day) n bs))
                (let nreal (and pin (birdpos obs year day′)))
                (← n′ (dist (if nreal (condition nreal categorical) categorical) trans))
                (for-each/monad (λ (i) (dist (condition (obs year day′ i) poisson)
                                             (lift (λ (nv) (if (= i nv) 1 0)) n′)))
                                (iota n-cells 1))
                (return n′)))
            1 (iota (sub1 days) 1)))
        (iota years 1))
      (return bs))))

; (new) the generative half of B1:57-73, with known parameters bs: each year, the bird's path
; from cell 1, one cell per day (fold/monad threading the path, newest cell first); the model
; answers the list of the years' paths.  Run by the forward sampler, it simulates the flights
; of the synthetic data set.
(define/shown bird-flights
  (define (bird-flights features bs #!key (years 30) (days 20))
    (<$> collect
         (traverse/monad
           (λ (year)
             (<$> (λ (path) (collect (reverse path)))
                  (fold/monad (λ (path day)
                                (<$> (cut cons <> path)
                                     (dist categorical (lift (transitions features year day) (car path) bs))))
                              '(1)
                              (iota (sub1 days) 1))))
           (iota years 1)))))

; (new) synthetic features after BMP:159-177 and the covariates of the FAQ (BMP:430-443),
; (features year day from to) => (f1 f2 f3 f4): for from /= to, f1 = 4 lognpdf(|v|; 1, 1) of
; the distance |v| flown, f2 the cosine of v with the direction of the top-right cell (the
; destination; 0 from it), f3 the cosine of v with the night's wind, f4 = 0; for from = to,
; (0 0 0 2), selfT = 2.  The wind of each night is a direction drawn uniformly, in year and day
; order, from a stdgen source seeded with `seed`.
(define/shown synthetic-features
  (define (synthetic-features years days #!key (seed 17))
    (define (lognpdf x mu sigma)
      (/ (exp (- (/ (expt (- (log x) mu) 2) (* 2. sigma sigma)))) (* x sigma (sqrt (* 2. pi)))))
    (define (cosine u v)
      (let ((nu (sqrt (squared-distance u '(0 . 0)))) (nv (sqrt (squared-distance v '(0 . 0)))))
        (if (or (zero? nu) (zero? nv))
            0.
            (/ (+ (* (car u) (car v)) (* (cdr u) (cdr v))) (* nu nv)))))
    (define (vector-to p q) (cons (- (car q) (car p)) (- (cdr q) (cdr p))))
    (let* ((src (make-stdgen-source seed))
           (winds (list->vector
                    (map-in-order (λ (_)
                                    (receive (angle ll) (distribution-sample (uniform 0. (* 2. pi)) src)
                                      (cons (cos angle) (sin angle))))
                                  (iota (* years days)))))
           (top-right (cell->xy n-cells)))
      (memoize
        (λ (year day from to)
          (if (= from to)
              '(0. 0. 0. 2.)
              (let* ((p (cell->xy from))
                     (v (vector-to p (cell->xy to))))
                (list (* 4. (lognpdf (sqrt (squared-distance v '(0 . 0))) 1. 1.))
                      (cosine v (vector-to p top-right))
                      (cosine v (vector-ref winds (+ (* (sub1 year) days) (sub1 day))))
                      0.))))))))

; (new) the observations of a bird that flew `paths` (B1:42-44 `ObsData`): the count is 1 in
; its cell and 0 elsewhere, but 0 everywhere on the days `missed?`, as a watcher counts
; Poisson(1) = 0 birds with probability 1/e (BMP:465-470).
(define/shown indicator-counts
  (define ((indicator-counts paths missed?) year day cell)
    (if (and (not (missed? day)) (= cell (list-ref (list-ref paths (sub1 year)) (sub1 day))))
        1
        0)))

; (new) the parameters b1..b4 of the synthetic data (BMP:179-182).
(define β-true '(1. 2. .5 1.))

;; -------------------------------------------------------------------------------------------
;; Bayesian linear regression (P §3 l.461-504; PPAML problem 4.1; helpers of B1:103-114)
;; -------------------------------------------------------------------------------------------

; cf. B1:103-107 `normals means std = mapM (\m -> dist normal m std) means` (P l.499-502):
; independent normal nodes, one per mean, in order; mapM is traverse/monad.
(define/shown normals
  (define (normals means sd) (traverse/monad (λ (m) (dist normal m sd)) means)))

; cf. B1:110-111 `dot xs ays = foldr1 (+) $ zipWith (\x ay -> (*x) <$> ay) xs ays`: the dot
; product of plain numbers and SExps, one lifted expression; reduce-right is foldr1.
(define/shown dot
  (define (dot xs ays) (reduce-right sexp+ 0. (map sexp* ays xs))))

; cf. P §3 l.471-486 `model xsy`: data is a list of training points (xs . y), RegrDatum
; ([Double],Double); the weights have the prior N(w_mean, 1), w_mean ~ N(0, 2), the noise sd is
; 1/gamma(0.5, 0.5), each y is observed as normal around xs . w (mapM make_cond, whose results
; are dropped: for-each/monad), and the model answers the weights.
(define/shown regression
  (define (regression data)
    (do/monad
      (← w-mean (normals (make-list (length (car (car data))) 0.) 2.))
      (← w (normals w-mean 1.))
      (← inverse-noise-sd (dist gamma .5 .5))
      (let noise-sd (lift (cut / 1. <>) inverse-noise-sd))
      (for-each/monad (Λ ((,xs . ,y)) (dist (condition y normal) (dot xs w) noise-sd)) data)
      (return (collect w)))))

; (new) the generative reading of the regression with known weights w and noise sd: the
; training points (xs . y), y ~ N(xs . w, sd), for the given xss.
(define/shown regression-data
  (define (regression-data xss w sd)
    (<$> collect
         (traverse/monad (λ (xs) (<$> (cut lift (cut cons xs <>) <>) (dist normal (dot xs w) sd)))
                         xss))))

; (new) the least-squares estimate of w for two-dimensional xs, by the normal equations.
(define/shown least-squares/2
  (define (least-squares/2 data)
    (define (sum f) (fold (Λ ((,xs . ,y) ,acc) (+ acc (f xs y))) 0. data))
    (let* ((a (sum (λ (xs y) (* (first xs) (first xs)))))
           (b (sum (λ (xs y) (* (first xs) (second xs)))))
           (d (sum (λ (xs y) (* (second xs) (second xs)))))
           (u (sum (λ (xs y) (* (first xs) y))))
           (v (sum (λ (xs y) (* (second xs) y))))
           (det (- (* a d) (* b b))))
      (list (/ (- (* d u) (* b v)) det) (/ (- (* a v) (* b u)) det)))))

; (new) the twenty fixed points of C5: x1 from -1.8 to 2 by .2, x2 = sin i.
(define regression-xss (map (λ (i) (list (- (* i .2) 2.) (sin i))) (iota 20 1)))

;; -------------------------------------------------------------------------------------------
;; The suite
;; -------------------------------------------------------------------------------------------

(define-suite hakaru10-examples

  ((doc r) `((structure/section "Examples")
             (p "Section 3 of the paper "
                (cite/a "https://okmij.org/ftp/kakuritu/Hakaru10/design.pdf"
                        (i "Probabilistic Programming Language and its Incremental Evaluation"))
                " (APLAS 2016, "
                (cite/a "https://doi.org/10.1007/978-3-319-47958-3_19" "DOI 10.1007/978-3-319-47958-3_19")
                ") evaluates Hakaru10 on two realistic models; quoting design.txt l.454-460:"
                (cite/quote
                  "Oleg Kiselyov"
                  "The Hakaru10 tutorial might have given an impression that Hakaru10 tries so
                  hard to preclude problematic behavior, by restricting conditioning and models,
                  that one cannot do much interesting in it. In this section we briefly evaluate
                  the expressiveness of the language on two realistic models."))
             (p "The two models are the Bayesian linear regression of the PPAML small problems
                collection (problem 4.1) and the population estimation of Milch et al.'s
                colored balls.  To them the "
                (cite/a "https://okmij.org/ftp/kakuritu/Hakaru10/index.html" "Hakaru10 page")
                " adds the "
                (cite/a "https://okmij.org/ftp/kakuritu/Hakaru10/index.html#bird-migration"
                        "bird migration problem")
                " of the PPAML challenge, which these tests run on synthetic data (the real
                data run is "
                (code/inline "hakaru10-birds.scm") ", opt-in).  Upstream's "
                (cite/a "https://okmij.org/ftp/kakuritu/Hakaru10/ColoredBalls.hs" "ColoredBalls.hs")
                " and "
                (cite/a "https://okmij.org/ftp/kakuritu/Hakaru10/Birds1.hs" "Birds1.hs")
                " are transcribed definition by definition; the regression exists only in the
                paper, with its helpers in Birds1.hs.")
             (p "Each model is a computation of the State monad that threads the chain, as
                upstream's " (code/inline "Model a = MCMCM (SExp a)")
                " (Syntax.hs:82), written with " (code/inline "do/monad") " as the Haskell
                is written with " (code/inline "do") ": " (code/inline "dist") ", "
                (code/inline "diracN") " and " (code/inline "if_") " are models, and "
                (code/inline "mcmC") " runs one under the MCMC engine.  The oracles walk the
                trace graph of one build of the same model: exact enumeration ("
                (code/inline "exact-posterior") ") and the forward sampler ("
                (code/inline "likelihood-weighting") ", " (code/inline "weighted-samples")
                ").  So the same " (code/inline "cballs-model") " is checked by MCMC, by
                likelihood weighting and, on a smaller urn, by exact enumeration, against a
                closed form; and the generative half of the bird and the regression models, run
                forward from seed 17, simulates the data that their conditioned versions then
                explain.  The construction check rebuilds the model, which must give the same
                graph, values and log-likelihoods, and redraws its choices in address order,
                which must give back the values and the log-likelihood of the initial trace.")
             (p "Nodes are created when a model runs, in bind order: "
                (code/inline "do/monad") " top to bottom, and " (code/inline "replicate/monad")
                ", " (code/inline "traverse/monad") ", " (code/inline "for-each/monad") " and "
                (code/inline "fold/monad") " head first, upstream's "
                (code/inline "sequence . replicate") ", " (code/inline "mapM") ", "
                (code/inline "mapM_") " and " (code/inline "foldM_")
                ": creation order is the topological order of the trace (design.txt
                l.791-794).  A chain of " (code/inline "if_") " is built by "
                (code/inline "fold-right") " over models, which create no node until the
                outermost one runs, and " (code/inline "if_") " delays its branches until it
                builds them, which reproduces upstream's " (code/inline "foldr")
                " node for node: at 10000 samples the colored balls' histogram is upstream's
                own record (hakaru10-replay, X14).")))

  ((test/colored-balls _)
   (let* ((blue10 (make-list 10 'blue))
          (expected (cballs-closed-form 8 blue10))
          (exact (exact-posterior (cballs-model (make-list 4 'blue) 3)))
          (mcmc (mcmc-posterior 20000 (cballs-model blue10)))
          (lw (likelihood-weighting 10000 (cballs-model blue10))))
     (⊦ (equal-approx? 1e-12) (cballs-closed-form 3 (make-list 4 'blue)) exact)
     (⊦ (equal-approx? 1e-12) '(.4119636809525704 .2097286247545656 .12069197667405333
                                .08018476851324365 .05903181664195371 .04660379073990803
                                .038629862100510455 .03316547962319476)
       (map cdr expected))
     (⊦ (equal-approx? .13) (probability 1 expected) (probability 1 mcmc))
     (⊨ (> (probability 1 mcmc) (probability 2 mcmc) (probability 3 mcmc)))
     (⊦ (histograms-within .05) expected lw)
     `(doc (p "C1.  Population estimation, from Milch et al.'s "
              (cite/a "http://people.csail.mit.edu/milch/papers/blog-chapter.pdf"
                      "BLOG: Probabilistic Models with Unknown Objects")
              " (Introduction to Statistical Relational Learning, MIT Press, 2007, pp.
              373-398), example 1.1, as ColoredBalls.hs:1-13 quotes it:"
              (cite/quote
                "Milch et al."
                "An urn contains an unknown number of balls--say, a number chosen from a
                Poisson or a uniform distributions. Balls are equally likely to be blue or
                green. We draw some balls from the urn, observing the color of each and
                replacing it. We cannot tell two identically colored balls apart;
                furthermore, observed colors are wrong with probability 0.2.  How many balls
                are in the urn? Was the same ball drawn twice?"))
           (p "The " (cite/a "https://okmij.org/ftp/kakuritu/Hakaru10/index.html#colored-balls"
                             "Hakaru10 page")
              " (index.html:111) calls it an elegant example of a model with an indefinite
              number of objects whose real identity is unobservable, a simplified population
              estimation, which in Hansei makes the case for the memoization of stochastic
              functions; the paper (design.txt l.519-520) notes that it motivated BLOG,
              being hard to implement in many probabilistic languages.  Hakaru10 bounds the
              number of balls and pre-allocates one color per ball, which the unused branches
              marginalize (design.txt l.531-544); its footnote 5 (l.566-569) wishes for lazily
              instantiated branches instead.  Upstream's model, ColoredBalls.hs:28-44 and
              74-103:")
           (code/lang "haskell" "data Color = Blue | Green deriving (Eq, Show)

opposite_color :: Color -> Color
opposite_color Blue  = Green
opposite_color Green = Blue

-- Distribution for the observing of a ball, with the failure rate 20%
observed_color color =
  categorical [(color,0.8),(opposite_color color,0.2)]

maxBalls :: Int
maxBalls = 8

-- Create the prior for the max amount of balls that may be in the urn
balls_prior n = do
  balls <- sequence . replicate n $ dist uniformly (pure [Blue,Green])
  return $ M.fromList $ zip [1..] balls")
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
  return nballs

cbr = hist $ mcmC 10000 (cballs_model (replicate 10 Blue))

-- fromList [(1,4265),(2,2203),(3,1244),(4,805),(5,486),(6,291),(7,565),(8,141)]
-- Good agreement")
           (p "In Scheme, with " (code/inline "dispatch") " the two " (code/inline "foldr")
              "s of nested " (code/inline "if_") ":")
           ,(shown 'opposite-color 'observed-color 'balls-prior 'dispatch 'balls-observe
                   'cballs-model)
           (p "Given that ten balls were drawn and all looked blue, the posterior of the
              number of balls has a closed form: with " (m "n") " balls of which " (m "k")
              " are blue, a drawn ball looks blue with probability "
              (m "pₙ,ₖ = (0.8k + 0.2(n − k))/n") ", so")
           (displaymath (m "P(n | obs) ∝ Σₖ₌₀ⁿ C(n, k) 2⁻ⁿ pₙ,ₖ¹⁰,   n = 1, …, 8"))
           ,(shown 'cballs-closed-form)
           (p "which is " (code/scheme ,(map (Λ ((,n . ,p)) (list n p)) expected))
              "(Hansei's importance sampling gives .4164, .2114, .1238, .0778,
              .0560, .0445, .0384, .0318, ColoredBalls.hs:139-147, and BLOG's Fig. 1.7 agrees).
              The closed form is first checked against exact enumeration of the same "
              (code/inline "cballs-model") " on an urn of at most 3 balls with 4 blue
              observations, " (code/inline "(exact-posterior (cballs-model (make-list 4 'blue) 3))")
              ", within " (m "10⁻¹²") ": " (code/scheme ,exact)
              "; the enumerator walks the trace graph of one build, branching over the support
              of each active random choice, and jumps over the branches of the "
              (code/inline "dispatch") " that its tests leave inactive.  With 8
              balls and 10 observations enumeration is out of reach (the paths number "
              (m "2⁸ · 8¹¹") "), so the full model is checked by sampling:")
           (ul (li "MCMC, 20000 samples from seed 17: P(1) within .13 of the exact .41196 (over seeds
                   1-20 P(1) has mean .417 and sd .030, so .13 is about 4.3 sd), and
                   P(1) > P(2) > P(3): "
                   (code/scheme ,(map (Λ ((,n . ,p)) (list n p)) mcmc))
                   "Single-site MH mixes slowly here: CB:46-49 explains why upstream dispatches
                   on the number of balls first, since nballs and the drawn balls are strongly
                   correlated; C1′ runs the model it set aside.")
               (li "Likelihood weighting, 10000 forward runs of the same model weighted by their
                   observations: every probability within .05 of the closed form: "
                   (code/scheme ,(map (Λ ((,n . ,p)) (list n p)) lw))))
           (p "The paper reports 16 seconds for 10000 samples in GHCi bytecode (design.txt
              l.609-616), against Hansei's 13.4 s for 5000 importance samples and BLOG's 35
              s.  Hakaru10 has no counterpart of the second question, whether the same ball was
              drawn twice, which only Hansei answers (ColoredBalls.hs:154-185: a 25% chance that
              some ball was drawn only once)."))))

  ((test/colored-balls/direct _)
   (let* ((blue10 (make-list 10 'blue))
          (exact (exact-posterior (cballs-model/direct (make-list 4 'blue) 3)))
          (direct (mcmC 20000 (cballs-model/direct blue10)))
          (dispatched (mcmC 20000 (cballs-model blue10)))
          (posterior (chain-posterior direct)))
     (⊦ (equal-approx? 1e-12) (cballs-closed-form 3 (make-list 4 'blue)) exact)
     (⊦ equal? '((nodes . 269) (resampleable . 19) (observed . 80) (dirac . 170) (inactive . 164)
                 (entries . 80))
       (model-graph (cballs-model/direct blue10)))
     (⊨ (< (* 5 (moves direct)) (moves dispatched)))
     (⊨ (> (abs (- .4119636809525704 (probability 1 posterior))) .2))
     `(doc (p "C1′.  ColoredBalls.hs:46-72 keeps, commented out, a first version of "
              (code/inline "cballs_model") " in which every drawn ball is uniform among the "
              (code/inline "nballs") " balls, a support that depends on a random variable:")
           (code/lang "haskell" "{-
Problem with the following model: nballs and b below are strongly correlated.
Hence if nballs is drawn as 5 and b is drawn as, say, 3, all the
proposals to resample nbalss as 1 or 2 will be rejected.

-- The model takes an array of observations as an argument
cballs_model :: [Color] -> Model Int
cballs_model obs = do
  balls  <- balls_prior maxBalls
  nballs <- dist uniformly (pure [1..maxBalls])
  -- The same ball can be drawn twice as we return the drawn balls
  -- into the urn. A ball keeps its true color, no matter how many
  -- times it is drawn.
  let observe_ball color = do
        b <- dist uniformly ((\\n -> [1..n]) <$> nballs)
        -- It is more efficient to create if statements, rather than
        -- create an SExp(IntMap Color).
        let observe i =
              let true_color = balls ! i in
              if_ ((== i) <$> b) $
                dist (color `condition` categorical)
                     ((\\bc -> [(bc,0.8),(opposite_color bc,0.2)]) <$>
                      true_color)
        foldr observe (return (pure color)) [1..maxBalls]
  mapM_ observe_ball obs
  return nballs
-}")
           (p "In Scheme the support is " (code/inline "(lift (λ (n) (iota n 1)) nballs)")
              " and the inlined categorical is " (code/inline "observed-color") ":")
           ,(shown 'cballs-model/direct)
           (p "It is the same model: exact enumeration on the urn of at most 3 balls with 4
              blue observations gives the closed form within " (m "10⁻¹²") ", "
              (code/scheme ,exact) "and its graph is smaller, 19 resampleable nodes (8 colors,
              nballs and one ball per observation) against the 89 of C1, whose branches each
              draw their own balls.  But it mixes badly, for the reason CB:47-49 gives: a
              proposal of a smaller nballs makes some drawn ball's log-density "
              (m "−∞") " and is rejected, while a drawn ball may only move within the current
              nballs.  At 20000 samples from seed 17, nballs moves "
              (code/scheme ,(moves direct)) "times, against the " (code/scheme ,(moves dispatched))
              "of C1's chain (fewer than a fifth), and the posterior of the chain, "
              (code/scheme ,(map (Λ ((,n . ,p)) (list n p)) posterior))
              "misses P(1) = .41196 by more than .2.  Hence upstream's dispatch on the number of
              balls first (CB:91-99), whose branches redraw the balls whenever nballs changes."))))

  ((test/birds/neighbors _)
   (⊦ equal? '((0 . 0) (0 . 3) (3 . 0) (3 . 3)) (map cell->xy '(1 4 13 16)))
   (⊦ equal? '((0 . 0) (0 . 9) (9 . 0) (9 . 9)) (map (cut cell->xy <> 10) '(1 10 91 100)))
   (⊨ (every (λ (n) (equal? (iota 16 1) (neighbors n))) (iota 16 1)))
   (⊦ equal? '(1 2 3 4 5 11 12 13 14 15 21 22 23 24 31 32 33 34 41 42) (neighbors 1 10))
   (⊦ = 20 (length (neighbors 1 10)))
   `(doc (p "C2.  The grid of the "
            (cite/a "https://okmij.org/ftp/kakuritu/Hakaru10/BirdMigrationProblem.pdf"
                    "PPAML challenge problem: bird migration")
            " (Tom Dietterich and Shahed Sorower, version 6, August 30, 2014): cells are
            numbered in Matlab order, column major (BirdMigrationProblem.txt:103-122, 140), so
            that on the 4x4 grid of the 1-bird data set cell 1 is the bottom-left corner, 4 the
            top-left, 13 the bottom-right and 16 the top-right; " (code/inline "divMod (n-1) 4")
            " answers the column and the row.  A bird flies at most "
            (m "dₘₐₓ = 3√2 ≈ 4.2426") " units a night (l.58-59), which
            Birds1.hs:87-93 compares squared:")
         (code/lang "haskell" "neighbors :: CellNum -> [CellNum]
neighbors n = filter (\\i -> dist n i <= d_max) [1..nCells]
 where
  dist n m = let (ix,iy) = divMod (n-1) gridDim
                 (jx,jy) = divMod (m-1) gridDim
             in (ix-jx)^2+(iy-jy)^2
  d_max = 18")
         ,(shown 'cell->xy 'squared-distance 'neighbors)
         (p "The 4x4 grid's diagonal is " (m "3√2") " itself, so every cell neighbors
            every cell, itself included.  On a 10x10 grid (the 1000-birds data set,
            l.202-204) cell 1 reaches the 20 cells " (code/scheme ,(neighbors 1 10)))))

  ((test/birds/transitions _)
   (let* ((features (synthetic-features 1 2))
          (trans ((transitions features 1 1) 6 β-true))
          (d (categorical trans))
          (total (fold + 0. (map cdr trans)))
          (counts '(((1 1 6) . 1) ((1 2 6) . 2) ((1 3 3) . 1) ((1 3 9) . 1)))
          (obs (λ (year day cell) (alist-ref (list year day cell) counts equal? 0))))
     (⊦ equal? (neighbors 6) (map car trans))
     (⊦ (equal-approx? 1e-12) 1. (fold + 0. (map (λ (j) (exp (distribution-log-density d j))) (neighbors 6))))
     (⊦ (equal-approx? 1e-12) (map (λ (w) (/ (cdr w) total)) trans)
       (map (λ (j) (exp (distribution-log-density d j))) (neighbors 6)))
     (⊦ (equal-approx? 1e-12) (theta (features 1 1 6 6) β-true) (exp 2.))
     (⊦ equal? '(6 6 #f #f) (map (cut birdpos obs 1 <>) '(1 2 3 4)))
     (⊦ (equal-approx? 1e-15) '(-1. -1. -1.6931471805599454 -0. -inf.0 -inf.0)
       (map (Λ ((,λ . ,k)) (distribution-log-density (poisson λ) k))
            '((1 . 0) (1 . 1) (1 . 2) (0 . 0) (0 . 1) (0 . 2))))
     `(doc (p "C3.  The migration model is log-linear (BirdMigrationProblem.txt:32-33, 60-79):
              the potential of a flight from cell " (m "i") " to cell " (m "j")
              " is the dot product of the parameters with its four features, and the
              probability of the flight is its exponential, normalized over the cells within "
              (m "dₘₐₓ") ":")
           (displaymath (m "θₜ,ₜ₊₁(i, j) = ")
                        (frac (m "exp βᵀxₜ,ₜ₊₁(i, j)") (m "Σⱼ′ exp βᵀxₜ,ₜ₊₁(i, j′)")))
           (p "Birds1.hs computes the numerators, " (code/inline "phi") " (B1:82-85) and "
              (code/inline "theta") " (B1:95-99), and leaves the normalization to "
              (code/inline "categorical") " (Distribution.hs:87-91), which divides each weight
              by their sum; " (code/inline "birdpos") " (B1:76-80) finds the bird where exactly
              one cell counts it:")
           (code/lang "haskell" "-- where is the single bird on the day d of the year y
birdpos :: ObsData -> Year -> Day -> Maybe CellNum
birdpos obs y d = case [i | i <- [1..nCells], obs ! (y,d,i) > 0] of
  [i] -> Just i
  _   -> Nothing")
           (code/lang "haskell" "phi :: Year -> Day -> CellNum -> CellNum -> Features -> [Double] -> Double
phi y d from to features bs = sum $ zipWith (*) fs bs
  where
    fs = [ realToFrac $ features ! (y, d, from, to, i) | i <- [1..nFeatures]]")
           (code/lang "haskell" "-- multinomial (0; a,b,c) --> 0
-- multinomial (1,theta(i,1),,,theta(i,16))  -->
--     categorical [(1,theta(i,1)),(2,(theta(i,2))),... (16,theta(i,16))]
theta :: Year -> Day -> CellNum -> CellNum -> Features -> [Double] -> Double
theta y d from to features bs = exp $ phi y d from to features bs")
           ,(shown 'phi 'theta 'transitions 'birdpos)
           (p "The checks: the weights of the flights from cell 6 on the first night, under "
              (m "β = (1, 2, 0.5, 1)") ", come out of " (code/inline "categorical")
              " as probabilities summing to 1 within " (m "10⁻¹²")
              ", each its weight over the total; staying has the features (0 0 0 2), so its
              weight is " (m "e²") "; " (code/inline "birdpos") " finds the bird on days 1
              and 2 (a count of 2 in one cell still is one cell) and not on day 3 (two cells)
              or day 4 (none).")
           (p "Each count is a Poisson observation whose intensity is the number of birds in
              the cell, 0 or 1 with one bird (l.134-138, D:358-367): seeing no bird where it
              is has LL " (m "−1") ", seeing it once " (m "log 1 − 1 = −1")
              ", twice " (m "log(1/2) − 1") "; where it is not, seeing none has LL "
              (m "−0") " and seeing any has " (m "−∞") ", which pins the bird to the
              cell where it was seen:")
           (code/lang "haskell" "  logDensity (Poisson lambda expl) x
    | x == 0      = -lambda
    | lambda == 0 = minusInf
    | x == 1      = log lambda - lambda
    | x == 2      = log (lambda*lambda/2) - lambda")
           (p "cf. Distribution.hs:361-365."))))

  ((test/birds/synthetic _)
   (let* ((years 2)
          (days 6)
          (features (synthetic-features years days))
          (paths (forward-sample (bird-flights features β-true #:years years #:days days)))
          (obs (indicator-counts paths (λ (day) (zero? (modulo day 3)))))
          (birds/B1:68 (birds features obs #:years years #:days days #:pin #f))
          (model (birds features obs #:years years #:days days))
          (prior (birds features obs #:years 0 #:days days))
          (chain (λ (model)
                   (match/first (mcmC-fold (Λ (,bs (,sums ,prev ,repeats))
                                             (list (map + bs sums) bs
                                                   (if (equal? bs prev) (add1 repeats) repeats)))
                                           '((0. 0. 0. 0.) #f 0)
                                           3000
                                           model)
                     ((,sums ,last ,repeats) (list (map (cut / <> 3000.) sums) repeats)))))
          (bs+repeats (chain model))
          (bs (first bs+repeats))
          (repeats (second bs+repeats))
          (prior-bs (first (chain prior))))
     (⊦ equal? (make-list years days) (map length paths))
     (⊨ (every (λ (path) (= 1 (car path))) paths))
     (⊦ equal? (map (λ (path) (list (second path) #f (fourth path) (fifth path) #f)) paths)
       (map (λ (year) (map (cut birdpos obs year <>) (iota 5 2))) (iota years 1)))
     (⊦ equal? '((nodes . 175) (resampleable . 8) (observed . 166) (dirac . 1) (inactive . 0)
                 (entries . 0))
       (model-graph model))
     (⊦ eq? #t (construction-check model))
     (⊦ eq? 'skipped (construction-check birds/B1:68))
     (⊦ = 4 (length bs))
     (⊨ (every (λ (b) (and (finite? b) (>= b 0.))) bs))
     (⊨ (< repeats 2999))
     (⊨ (< (third bs) 3.))
     (⊨ (> (third prior-bs) 3.))
     `(doc (p "C4.  The bird migration problem, quoting the "
              (cite/a "https://okmij.org/ftp/kakuritu/Hakaru10/index.html#bird-migration"
                      "Hakaru10 page")
              " (index.html:122-128):"
              (cite/quote
                "Oleg Kiselyov"
                "Bird migration problem was one of the Challenge problems of the DARPA PPAML
                (Probabilistic Programming for Advanced Machine Learning) Program. The goal was
                to estimate the model parameters from the set of bird migration observations
                over the period of thirty years. The migration model is a log-linear model with
                four parameters that gives the probability of a bird flying from cell i to cell
                j under given conditions."))
           (p "The problem statement (BirdMigrationProblem.txt:19-79) models the population as
              a collective graphical model: every night each bird flies from cell "
              (m "i") " to cell " (m "j") " with probability " (m "θ(i, j)")
              ", and the birds counted in a cell are Poisson of the birds there.  Its first
              data set has one bird on a 4x4 grid for 30 years of 20 days: each year the bird
              starts in the bottom-left cell and heads for the top-right one, a fully observed
              input-output HMM (l.103-105); the task is to estimate "
              (m "β₁, …, β₄") " (l.179-182), whose prior may be the absolute
              value of a zero-mean Gaussian of sd 10, the features being coded so that every
              parameter should be non-negative (l.457-464).  Birds1.hs:50-74 is the model of
              Sasaki Shoichiro's 2018 bachelor thesis, which, the page says, closely reflects
              the specification:")
           (code/lang "haskell" "model :: Features -> ObsData -> Model [Double]
model features obs = do
  b1  <- abs <$> dist normal 0 10 -- see Statement of Problem, p9
  b2  <- abs <$> dist normal 0 10 -- see Statement of Problem, p9
  b3  <- abs <$> dist normal 0 10 -- see Statement of Problem, p9
  b4  <- abs <$> dist normal 0 10 -- see Statement of Problem, p9
  let bs = collect [b1,b2,b3,b4]
  forM_ [1..nYears] $ \\year -> do -- each year
    foldM_ (\\n day -> do -- one bird exists in cell n on each day(1..nDays-1)
      let trans n bs = [ (j,theta year day n j features bs) | j<-neighbors n]
      -- n' cell where the bird is on day+1
      let day' = day + 1
      {-
      n' <- case birdpos obs year day' of
          Just nreal -> dist (nreal `condition` categorical) $
                        liftA2 trans n bs
          Nothing    -> dist categorical $ liftA2 trans n bs
      -}
      n' <- dist categorical $ liftA2 trans n bs
      forM_ [1..nCells] $ \\i ->
        dist (fromIntegral (obs ! (year,day',i)) `condition` poisson)
           ((\\nv -> if i==nv then 1 else 0) <$> n')
      return n'
      ) (1 :: SExp Int) [1..nDays-1]
  return bs")
           ,(shown 'birds)
           (p (code/inline "forM_") " is " (code/inline "for-each/monad") ", "
              (code/inline "foldM_") " is " (code/inline "fold/monad")
              ", threading the bird's cell, " (code/inline "liftA2") " is "
              (code/inline "lift") ", and the four " (code/inline "abs <$> dist normal 0 10")
              " are made by " (code/inline "replicate/monad") ".  The test uses upstream's
              commented-out alternative
              (B1:62-67), " (code/inline "#:pin #t") ": on a day where the bird is seen its cell
              is observed, elsewhere it is drawn.  The posterior of "
              (m "β") " is the same, but B1:68 alone draws the cell of every day and needs
              all the observed ones guessed right for a finite initial trace: on the synthetic
              data below its build at seed 17 has an LL of " (m "−∞")
              ", which the construction check reports as " (code/inline "skipped") ".")
           (p "The synthetic data set has 2 years of 6 days.  Its features follow
              the covariates of the problem's FAQ (l.430-443), with a wind drawn each night from
              seed 17; its flights are simulated by running the generative half of the model,
              with " (m "β = (1, 2, 0.5, 1)") ", under the forward sampler:")
           ,(shown 'synthetic-features 'bird-flights 'indicator-counts)
           (code/scheme ,paths)
           (p "Every third day the watchers miss the bird (a Poisson(1) count is 0 with
              probability " (m "1/e") ", l.465-470), so that the cells of days 3 and 6 are
              latent; " (code/inline "birdpos") " recovers the others.  The model has "
              (m "4 + 2 · 5 · (1 + 16) + 1 = 175") " nodes: the 4 parameters and 4
              latent cells are resampleable, 6 cells and 160 counts observed, and the answer
              is a Dirac node.  The construction check passes: the initial trace's LL is
              finite, a rebuild gives the same graph, values and LLs, and redrawing the choices
              in address order from a copy of the source gives back their values and the LL.
              One "
              (code/inline "mcmC-fold") " over 3000 samples sums the "
              (m "β") " tuples and counts the consecutive ones that repeat, the steps
              whose proposal was rejected: " (code/scheme ,repeats) " of the 2999 do, so the
              chain moves, and the posterior means are " (code/scheme ,bs) "finite and
              non-negative, as the prior makes them.  Ten flights are little data, so they are
              not compared with " (m "β") ", and over seeds 1-20 the chain mixes too slowly
              for most of them to be bounded: " (m "β₄") " ranges from 1.3 to 14.7.  "
              (m "β₃") ", the weight of flying with the wind, is pinned by the data alone: it
              stays between 0.92 and 1.54 on every one of those seeds, so the test asks it to
              be below 3.  As a negative control, the same fold runs on the model with no
              year of data, " (code/inline "(birds features obs #:years 0 #:days days)")
              ", whose " (m "β") " are the four " (m "|N(0, 10)|")
              " nodes of the prior, of mean " (m "10 √(2/π) ≈ 7.98") ": its means are "
              (code/scheme ,prior-bs) "and its " (m "β₃")
              " is above 3, as it is, between 7.5 and 8.7, on every seed from 1 to 20.  The
              real data run, Birds1.hs:202-215, is "
              (code/inline "hakaru10-birds.scm") ".  Upstream's thesis run took 9 minutes for a
              million samples, against BLOG's 49 minutes for a thousand (index.html:163-167)."))))

  ((test/regression _)
   (let* ((w-true '(1.5 -.7))
          (data (forward-sample (regression-data regression-xss w-true .5)))
          (model (regression data))
          (samples (drop (mcmC 20000 model) 2000))
          (w (means samples)))
     (⊦ = 20 (length data))
     (⊦ equal? '((nodes . 26) (resampleable . 5) (observed . 20) (dirac . 1) (inactive . 0)
                 (entries . 0))
       (model-graph model))
     (⊦ eq? #t (construction-check model))
     (⊦ (equal-approx? .4) w-true w)
     (⊦ (equal-approx? .04) (least-squares/2 data) w)
     `(doc (p "C5.  Bayesian linear regression, problem 4.1 of the PPAML small problems
              collection, quoting design.txt l.464-470:"
              (cite/quote
                "Oleg Kiselyov"
                "Given the set of training points (x_ij, y_i), i = 1..N, j = 1..k and the
                generative model y_i = Σ_j x_ij * w_j + noise_i find the posterior distribution
                on w_j. In the conventional linear regression language, we are given N
                observations of y assumed to be a linear combination of the controlled
                quantities x with the parameters w; we have to estimate w.")
              "The model is typeset on page 10 of the paper (design.txt l.471-486), here in
              ASCII Haskell; its helpers, which the paper describes in l.496-504, are
              Birds1.hs:103-114:")
           (code/lang "haskell" "type RegrDatum = ([Double],Double) -- Xs and Y

model :: [RegrDatum] -> Model [Double]
model xsy = do
  let mu = replicate dimK 0
  w_mean   <- normals mu 2
  w        <- normals w_mean 1
  noise_sd <- (1/) <$> dist gamma 0.5 0.5
  let make_cond (xs,y) = dist (y `condition` normal) (dot xs w) noise_sd
  mapM make_cond xsy
  return $ collect w")
           (code/lang "haskell" "-- normals means std
-- returns several independently distributed normal random variables
-- whose means are given by the list means, with the same std
normals :: [SExp Double] -> SExp Double -> MCMCM [SExp Double]
normals means std = mapM (\\m -> dist normal m std) means


dot :: [Double] -> [SExp Double] -> SExp Double
dot xs ays = foldr1 (+) $ zipWith (\\x ay -> (*x) <$> ay) xs ays

collect :: [SExp a] -> SExp [a]
collect = sequenceA")
           (p "In Scheme " (code/inline "collect") " is exported by (aux hakaru10), "
              (code/inline "sexp+") " and " (code/inline "sexp*") " lift the arithmetic, "
              (code/inline "reduce-right") " is " (code/inline "foldr1") ", and "
              (code/inline "mapM") " is " (code/inline "traverse/monad") ", or "
              (code/inline "for-each/monad") " where its results are dropped:")
           ,(shown 'normals 'dot 'regression)
           (p "The paper's instance has five hundred points and five parameters, a graph of
              \"511 vertices\" (design.txt l.493-495; the hakaru10-language suite counts it).
              Here twenty points with two features are simulated from "
              (m "w = (1.5, −0.7)") " and noise sd 0.5 by the forward sampler, from the
              generative reading of the model with known weights:")
           ,(shown 'regression-data)
           (p "The graph has " (m "2k + 1 + N = 25") " vertices, plus the Dirac node of the
              answer; the construction check passes.  After 20000 samples, the first 2000
              dropped as Birds1.hs:205 drops its burn-in, the posterior means of "
              (m "w") " are " (code/scheme ,w) "within .4 of the weights that made the data,
              and within .04 of the least-squares estimate "
              (code/scheme ,(least-squares/2 data))
              "(over seeds 1-12 and 17 they differ from it by at most .021, mostly by less
              than .01, so .04 is about 4-5 times that spread), which the weak prior barely moves.  The noise sd, " (m "1/γ")
              " with " (m "γ ~ Γ(0.5, 0.5)") ": " (m "γ")
              " has mean 0.25, so its prior rarely proposes the " (m "γ = 2") " of the data's sd 0.5
              (" (m "P(γ > 2) ≈ 0.005") "), and the noise sd's posterior sits
              above 0.5: that widens the posterior of " (m "w") " but does not move its
              mean.")
           ,(shown 'least-squares/2))))

  ((test/plot-mixture _)
   (let* ((h (tabulate .2 (append (mcmC 5000 (dist normal 10. 2.))
                                  (mcmC 5000 (dist gamma 3. (/ 1. 3.))))))
          (path (plot h (create-temporary-file "dat")))
          (lines (with-input-from-file path read-lines)))
     (delete-file path)
     (⊦ = (add1 (length h)) (length lines))
     (⊦ equal? "x  y" (car lines))
     (⊦ (equal-approx? 1e-9) 1. (fold + 0. (map cdr h)))
     (⊦ (equal-approx? .2) 5.4 (fold (Λ ((,x . ,p) ,mean) (+ mean (* x p))) 0. h))
     `(doc (p "C6.  The only use of " (code/inline "plot") " upstream, Examples1.hs:166-167,
              writes for gnuplot the histogram, in bins of 0.2, of the mixture of
              Examples1.hs:159-164 (whose KL divergence from " (code/inline "mixng")
              " the hakaru10-replay suite replays, X12): 5000 samples of a normal of mean 10
              and sd 2 followed by 5000 of a gamma of shape 3 and scale 1/3:")
           (code/lang "haskell" "_ =
 let n = 10000 in plot (tabulate 0.2 $ mcmC (n `div` 2) (dist normal 10 2) ++ mcmC (n `div` 2) (dist gamma 3 (1/3)))")
           (p "Upstream writes /tmp/plot.dat; here " (code/inline "plot")
              "'s optional path is a temporary file, read back and deleted.  It has the header
              and one line per bin, "
              (code/scheme ,(add1 (length h)))
              "lines; the masses sum to 1, and the mean of the bins is "
              (code/scheme ,(fold (Λ ((,x . ,p) ,mean) (+ mean (* x p))) 0. h))
              "within .2 of the mixture's mean " (m "(10 + 1)/2 = 5.5")
              " less half a bin, since a bin is the floor of its samples (U:37).  The
              hakaru10-util suite checks the file's format on a toy histogram (cf. U:58-65)."))))

  )

(unittest/✓ hakaru10-examples)
