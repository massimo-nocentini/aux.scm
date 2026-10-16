; The long suite of (aux hansei), run by `make test-hansei-long`, not by `make test`: the zebra
; puzzle, the replays of parsec.ml's runs of 30000 samples, and the statistical tests of the
; importance sampler over seeds 1-20 (the sorted list and the planes added in the final review).
; Plan AUX-HANSEI-PLAN.md §8.6 (WP9); the other cases of §8.6 take under two seconds each and
; are in the fast suites: the sorted list of four numbers of five bits in hansei-lazy.scm, the
; two planes of blip_naive.ml, the three moving planes of blip.ml and the sampling replay of
; music1a.ml in hansei-examples.scm, the 2000 samples of parsec.ml in hansei-logic.scm.

; Two CHICKEN hazards for stochastic code (plan §5.6, D11 and D12), which every model of this
; file avoids:
;   - `map` is not re-entrant: CHICKEN's `map` (and srfi-1's) builds its result destructively, so
;     resuming a continuation captured inside its procedure corrupts the list, e.g.
;     (probcc-reify/exact (map (λ (x) (probcc-coin 1/2)) '(1 2))) answers the single
;     entry (#t #t #f #f #t #f) at mass 1, the four worlds sharing one list built in place.
;     Models use probcc-map, left to right as OCaml's List.map, never map, vector-map or
;     for-each; nor srfi-1's list-tabulate and filter-map, which are re-entrant but call their
;     procedure from the last element to the first.
;   - under csi, resuming a continuation captured while the operands of a call with five or more
;     operands are evaluated accumulates arguments, even with plain call/cc, e.g.
;     (probcc-reify/exact (list 'a 'b 'c 'd (probcc-coin 1/2))) answers (a b c d #t) and
;     (a b c d #t #f); quasiquote templates and constructors (list, vector) are calls too, and
;     the position of the choice does not matter.  Models bind every choice with let* or let1
;     before using it in such a call.  Compiled code is correct.
; OCaml evaluates the components of a tuple, the arguments of a constructor (`::` too) and of a
; call from right to left, and `let x = e1 and y = e2 in` and the scrutinee of `match (e1, e2)
; with` from left to right (D19): the ported models sequence them with let*, as their comments
; say.

(import scheme
        (chicken base)
        (chicken flonum)
        (only srfi-1 fold iota)
        (aux base)
        (aux unittest)
        (aux hansei)
        (aux hansei lazy)
        (aux hansei parsec))

; replays and bit-exact checks print 17 digits: with the default 15, a one-ulp failure would print
; identical expected and got values (plan §8 conventions).
(flonum-print-precision 17)

;; -----------------------------------------------------------------------------------------
;; Helpers
;; -----------------------------------------------------------------------------------------

; (new) the statistical check of plan §8.6: the statistics stats of probcc-statistics over the
; 20 seeds 1-20, a list of (v mean std), against the table expected of (v p): every value of
; expected has a mean within 4 std / √20 of p, or within the absolute tolerance floor if that is
; larger, or if std is NaN (§6.6); answers the values that fail, so '() when all pass.
(define (statistics-misses stats expected floor)
  (fold (λ (e misses)
          (match/first e
            ((,v ,p)
             (match/first (assoc v stats)
               ((,_ ,mean ,std)
                (let1 (tolerance (if (nan? std) floor (max floor (/ (* 4 std) (sqrt 20)))))
                  (if (<= (abs (- mean p)) tolerance) misses (cons (list v mean std) misses))))
               (#f (cons (list v 'missing) misses))))))
        '()
        expected))

; (new) the 20 seeds of plan §8.6: (probcc-statistics 1 20 …) of importance sampling of n
; worlds of thunk with (probcc-random-selector seed), each table mapped by f.
(define (statistics/importance n thunk f)
  (probcc-statistics 1 20 (λ (seed)
                            (let1 (selector (probcc-random-selector seed))
                              (f (probcc-sample-importance/a selector n thunk))))))

;; -----------------------------------------------------------------------------------------
;; Models of zebra.ml (Z)
;; -----------------------------------------------------------------------------------------

; cf. Z:49 `mustbe`: the proposition x holds in this world, or the world fails.
(define (mustbe x) (unless x (probcc-impossible)))

; cf. Z:52-55 `exists`: f holds for some element of the list, tried in order; a failure of f
; is a failure of the world, as upstream.
(define (z-exists f lst)
  (match/first lst
    (() (probcc-impossible))
    ((,h . ,t) (unless (f h) (z-exists f t)))))

; cf. Z:69-80 `to_the_right`, `to_the_left`: the house next to i, or a failure.
(define (z-to-the-right nhouses i)
  (let1 (j (add1 i))
    (if (>= j nhouses) (probcc-impossible) j)))

(define (z-to-the-left nhouses i)
  (let1 (j (sub1 i))
    (if (< j 0) (probcc-impossible) j)))

; cf. Z:85-87 `next_to_dir`: a house next to the given one, on a side chosen once.
(define (z-next-to-dir nhouses)
  (if (probcc-coin 1/2)
      (λ (i) (z-to-the-left nhouses i))
      (λ (i) (z-to-the-right nhouses i))))

; cf. Z:93-96 `make_selector`: the h-th element of a lazy permutation of lst, the all-different
; constraint (H #all-different).
(define (z-make-selector lst)
  (let1 (perm (list->vector (probcc-lazy-permutation lst)))
    (λ (h) ((vector-ref perm h)))))

; cf. Z:109-168 `zebra_model`: the puzzle stated directly, each attribute a function from the
; house to its value; `&&` from the left; the five attributes of a house from the right (D19)
; and bound before the list is made (D12).
(define (z-model)
  (let* ((nhouses 5)
         (houses '(0 1 2 3 4))
         (color (z-make-selector '(Red Green Ivory Yellow Blue)))
         (nat (z-make-selector '(EN SP UA NO JA)))
         (pet (z-make-selector '(Dog Snail Fox Horse Zebra)))
         (drink (z-make-selector '(Coffee Tea Milk OJ Water)))
         (smoke (z-make-selector '(OGold Kools Chest LStrike Parliaments))))
    (mustbe (eq? (nat 0) 'NO))
    (mustbe (eq? (drink 2) 'Milk))
    (z-exists (λ (h) (and (eq? (nat h) 'EN) (eq? (color h) 'Red))) houses)
    (z-exists (λ (h) (and (eq? (nat h) 'SP) (eq? (pet h) 'Dog))) houses)
    (z-exists (λ (h) (and (eq? (drink h) 'Coffee) (eq? (color h) 'Green))) houses)
    (z-exists (λ (h) (and (eq? (nat h) 'UA) (eq? (drink h) 'Tea))) houses)
    (z-exists (λ (hl) (let1 (hr (z-to-the-right nhouses hl))
                        (and (eq? (color hr) 'Green) (eq? (color hl) 'Ivory))))
              houses)
    (z-exists (λ (h) (and (eq? (smoke h) 'OGold) (eq? (pet h) 'Snail))) houses)
    (z-exists (λ (h) (and (eq? (smoke h) 'Kools) (eq? (color h) 'Yellow))) houses)
    (z-exists (let1 (next-of (z-next-to-dir nhouses))
                (λ (h) (and (eq? (smoke h) 'Chest) (eq? (pet (next-of h)) 'Fox))))
              houses)
    (z-exists (let1 (next-of (z-next-to-dir nhouses))
                (λ (h) (and (eq? (smoke h) 'Kools) (eq? (pet (next-of h)) 'Horse))))
              houses)
    (z-exists (λ (h) (and (eq? (smoke h) 'LStrike) (eq? (drink h) 'OJ))) houses)
    (z-exists (λ (h) (and (eq? (nat h) 'JA) (eq? (smoke h) 'Parliaments))) houses)
    (z-exists (let1 (next-of (z-next-to-dir nhouses))
                (λ (h) (and (eq? (nat h) 'NO) (eq? (color (next-of h)) 'Blue))))
              houses)
    (probcc-map (λ (h)
                  (let* ((s (smoke h))
                         (d (drink h))
                         (p (pet h))
                         (n (nat h))
                         (c (color h)))
                    (list c n p d s)))
                houses)))

;; -----------------------------------------------------------------------------------------
;; Models of parsec.ml (PS), as in the suite hansei-logic
;; -----------------------------------------------------------------------------------------

; cf. PS:430-433 `digit` and `num`.
(define digit
  (parsec-fmap (λ (c) (- (char->integer c) (char->integer #\0)))
               (parsec-sat (λ (x) (and (char>=? x #\0) (char<=? x #\9))))))

(define num (parsec-fmap (λ (ds) (fold (λ (d acc) (+ d (* acc 10))) 0 ds)) (parsec-many1 digit)))

; cf. PS:438-444 `plus_minus` and `prod_div`; OCaml's `/` truncates, as quotient.
(define plus-minus (parsec-or (parsec-fmap (λ (_) +) (parsec-char #\+))
                              (parsec-fmap (λ (_) -) (parsec-char #\-))))

(define prod-div (parsec-or (parsec-fmap (λ (_) *) (parsec-char #\*))
                            (parsec-fmap (λ (_) quotient) (parsec-char #\/))))

; cf. PS:446-459 `expression`, `term`, `factor`; `(fun t1 op t2 -> op t1 t2) <$> term <*>
; plus_minus <*> expression` associates to the left.
(define (((binary t1) op) t2) (op t1 t2))

(define (expression st)
  ((parsec-or (parsec-ap (parsec-ap (parsec-fmap binary term) plus-minus) expression) term) st))

(define (term st)
  ((parsec-or (parsec-ap (parsec-ap (parsec-fmap binary factor) prod-div) term) factor) st))

(define (factor st)
  ((parsec-or (parsec-left (parsec-right (parsec-char #\() expression) (parsec-char #\))) num) st))

; cf. P:156-165 `uniform` and P:168-170 `uniformly`, upstream's, with flonum masses: 1. /. n,
; the value 0 getting 1. -. pacc.  probcc-uniform's masses are the exact 1/n (deviation 7), so
; the replays draw the characters with these, to give upstream's flonums bit for bit.
(define (ml-uniform n)
  (cond
    ((= n 1) 0)
    (else (let1 (p (/ 1. n))
            (let loop ((pacc 0.) (acc '()) (i (sub1 n)))
              (if (zero? i)
                  (probcc-distribution (cons (list i (- 1. pacc)) acc))
                  (loop (+ pacc p) (cons (list i p) acc) (sub1 i))))))))

(define (ml-uniformly lst) (list-ref lst (ml-uniform (length lst))))

; cf. PS:59-66 `stream_over`, as parsec-stream-over but with `flip 0.5` and ml-uniformly.
(define (ml-stream-over alphabet)
  (letrec ((loop (τ (cond
                      ((probcc-coin 0.5) (let* ((t (probcc-letlazy/nesting loop))
                                                (c (ml-uniformly alphabet)))
                                           (cons c t)))
                      (else '())))))
    (probcc-letlazy/nesting loop)))

; cf. PS:468-476 `exp_stream`: four characters over the alphabet of the expressions.
(define (exp-stream)
  (let1 (st (ml-stream-over (string->list "0123456789+-*()")))
    (parsec-stream-len st 4)
    st))

; cf. PS:514-523, the more balanced `exp_stream`: an operator or a parenthesis with probability
; 0.7, a digit otherwise; each cell a probcc-letlazy/nesting, its tail made before its
; character (D19).
(define (exp-stream/balanced)
  (letrec ((ch (τ (if (probcc-coin 0.7)
                      (ml-uniformly (string->list "+-*()"))
                      (ml-uniformly (string->list "0123456789")))))
           (cell (λ (n)
                   (if (zero? n)
                       (τ '())
                       (probcc-letlazy/nesting (τ (let* ((t (cell (sub1 n)))
                                                         (c (ch)))
                                                    (cons c t))))))))
    (cell 4)))

; cf. PS:504-512 and PS:549-557, the model of the sampling runs that look for the expressions
; whose value is v: the expression must parse the whole stream, then have the value v.
(define ((expression-model/value stream v))
  (let1 (st (stream))
    (match/first (expression st)
      ((,v* . ,st*) (match/first (st*)
                      (() (if (= v* v) (list v* (parsec-string-of-stream st)) (probcc-impossible)))
                      (else (probcc-impossible)))))))

;; -----------------------------------------------------------------------------------------
;; Models of the statistical tests: bloodtype.ml (BT), HMM/hmm.ml (HM), colored_balls.ml (CB),
;; samplingM.ml (SM), as in the suites hansei-examples and hansei-random
;; -----------------------------------------------------------------------------------------

; cf. BT:21-48: the father and mother relations, (parent child), inverted, and `lookup`.
(define bt-father '((Uwe Kristian) (Uwe Annette) (Willhelm Uwe) (Alfred Marianne) (Kristian Linus)
                    (Kristian Mette) (Herbert Nadine)))
(define bt-mother '((Marianne Kristian) (Marianne Annette) (Anneliese Uwe) (Pauline Marianne)
                    (Margot Nadine) (Nadine Linus) (Nadine Mette)))
(define (bt-invert rel) (map (λ1-match/first ((,p1 ,p2) (list p2 p1))) rel))
(define bt-father-of (bt-invert bt-father))
(define bt-mother-of (bt-invert bt-mother))

(define (bt-lookup key rel)
  (let1 (e (assq key rel))
    (and e (cadr e))))

; cf. BT:71-78 `chrom`, `pchromf`, `mchromf`: a chromosome, a founder's at (0.3, 0.3).
(define (bt-chrom pa pb) (probcc-distribution `((GA ,pa) (GB ,pb) (GO ,(- 1. pa pb)))))

; cf. BT:82-105 `pchrom` = `mchrom`: the chromosome a parent passes, given the genotype.
(define (bt-pchrom g)
  (match/first g
    ((GA GA) (bt-chrom 0.9 0.05))
    ((GA GB) (bt-chrom 0.49 0.49))
    ((GA GO) (bt-chrom 0.49 0.02))
    ((GB GA) (bt-chrom 0.49 0.49))
    ((GB GB) (bt-chrom 0.05 0.9))
    ((GB GO) (bt-chrom 0.02 0.49))
    ((GO GA) (bt-chrom 0.49 0.02))
    ((GO GB) (bt-chrom 0.02 0.49))
    ((GO GO) (bt-chrom 0.05 0.05))))

; cf. BT:108-124 `btype` and `bloodtype`: the phenotype of a genotype.
(define (bt-btype pa pb pab)
  (probcc-distribution `((A ,pa) (B ,pb) (AB ,pab) (O ,(- 1. pa pb pab)))))

(define (bt-bloodtype g)
  (match/first g
    ((GA GA) (bt-btype 0.9 0.03 0.03))
    ((GA GB) (bt-btype 0.03 0.03 0.9))
    ((GA GO) (bt-btype 0.9 0.04 0.03))
    ((GB GA) (bt-btype 0.03 0.03 0.9))
    ((GB GB) (bt-btype 0.04 0.9 0.03))
    ((GB GO) (bt-btype 0.04 0.9 0.03))
    ((GO GA) (bt-btype 0.9 0.04 0.03))
    ((GO GB) (bt-btype 0.04 0.9 0.03))
    ((GO GO) (bt-btype 0.03 0.03 0.04))))

; cf. BT:133-144 `pchr`, `mchr`, `bldtype`, unconditioned and without variable elimination, as
; the sampled query `test1` (BT:146-147); the pairs of chromosomes from the right (D19).
(define (bt-pchr person)
  (match/first (bt-lookup person bt-father-of)
    (#f (bt-chrom 0.3 0.3))
    (,father (let* ((m (bt-mchr father))
                    (p (bt-pchr father)))
               (bt-pchrom (list p m))))))

(define (bt-mchr person)
  (match/first (bt-lookup person bt-mother-of)
    (#f (bt-chrom 0.3 0.3))
    (,mother (let* ((m (bt-mchr mother))
                    (p (bt-pchr mother)))
               (bt-pchrom (list p m))))))

(define ((bt-bldtype person))
  (let* ((m (bt-mchr person))
         (p (bt-pchr person)))
    (bt-bloodtype (list p m))))

; cf. HM:50-98: the hidden Markov model, the states 1 .. 8, uniform at the start, observed at
; time 5 (`query1`, HM:102-104).
(define hm-nstates 8)

(define (hm-evolve st)
  (probcc-distribution (cond
                         ((= st 1) `((1 0.7) (2 0.3)))
                         ((= st hm-nstates) `((,st 0.7) (,(sub1 st) 0.3)))
                         (else `((,st 0.4) (,(add1 st) 0.3) (,(sub1 st) 0.3))))))

(define hm-l-observation-prob '#(1.0 0.85714 0.71428 0.57142 0.42857 0.28571 0.14285 0.0))

(define (hm-observe st)
  (let1 (pl (vector-ref hm-l-observation-prob (sub1 st)))
    (cond
      ((= pl 0.0) 'R)
      ((= pl 1.0) 'L)
      (else (probcc-distribution `((L ,pl) (R ,(- 1. pl))))))))

(define ((hm-query1 n))
  (let loop ((i 1)
             (st (add1 (probcc-uniform hm-nstates))))
    (cond
      ((> i n) st)
      (else (when (and (= i 5) (not (eq? (hm-observe st) 'L))) (probcc-impossible))
            (loop (add1 i) (hm-evolve st))))))

; cf. CB:22-48 `observed_color` and `model_nballs`: 1 to 8 balls (`uniform_range 1 8`, half open
; here, deviation 8), each of a memoized color, the observed color right with probability q.
(define (cb-observed-color q c)
  (if (probcc-coin q)
      c
      (case c ((Blue) 'Green) ((Green) 'Blue))))

(define ((cb-model-nballs q obs))
  (let* ((nballs (probcc-uniform/range 1 9))
         (ball-color (probcc-memo (λ (b) (probcc-uniform/either '(Blue Green))))))
    (let loop ((obs obs))
      (unless (null? obs)
        (let1 (b (probcc-uniform/range 1 (add1 nballs)))
          (unless (eq? (cb-observed-color q (ball-color b)) (car obs)) (probcc-impossible)))
        (loop (cdr obs))))
    nballs))

; (new) the exact posterior of the number of balls after m observations that all look blue,
; with q the probability of seeing a color right: the prior is uniform, and n balls give the
; evidence 2^-n Σ_k C(n,k) ((q k + (1 - q) (n - k)) / n)^m, k the balls that are blue.
(define (cb-posterior q m)
  (letrec ((choose (λ (n k) (if (= k 0) 1 (/ (* n (choose (sub1 n) (sub1 k))) k))))
           (evidence (λ (n)
                       (let loop ((k 0) (acc 0))
                         (if (> k n)
                             (/ acc (expt 2 n))
                             (loop (add1 k)
                                   (+ acc (* (choose n k)
                                             (expt (/ (+ (* q k) (* (- 1 q) (- n k))) n) m)))))))))
    (let* ((ns '(1 2 3 4 5 6 7 8))
           (es (map evidence ns))
           (total (fold + 0 es)))
      (map (λ (n e) (list n (/ e total))) ns es))))

; cf. SM:224-233, the drunk coin, x drawn before lost, and `dcoin_and`.
(define (drunk-coin)
  (let* ((x (probcc-coin 0.5))
         (lost (probcc-coin 0.9)))
    (if lost (probcc-impossible) x)))

(define (dcoin-and n)
  (if (= n 1) (drunk-coin) (and (drunk-coin) (dcoin-and (sub1 n)))))

;; -----------------------------------------------------------------------------------------
;; Models of the statistical tests: sorted.ml (SO), as in the suite hansei-lazy
;; -----------------------------------------------------------------------------------------

; cf. SO:28-31 `build_list`: a list of n results of thunk, each tail lazy; OCaml builds the
; constructor from the right (D19), the letlazy before the head.
(define ((so-build-list n thunk))
  (cond
    ((= n 0) '())
    (else (let* ((t (probcc-letlazy (so-build-list (sub1 n) thunk)))
                 (h (thunk)))
            (cons h t)))))

; cf. SO:41-48 `is_sorted`: is l sorted under gte, `gte x y` being x >= y?
(define (so-is-sorted gte l)
  (letrec ((loop (λ (xprev c)
                   (match/first c
                     (() #t)
                     ((,x . ,l*) (and (gte x xprev) (loop x (l*))))))))
    (match/first (l)
      (() #t)
      ((,x . ,l*) (loop x (l*))))))

; cf. SO:54-55 `bit_list`: d random bits, the most significant first.
(define (so-bit-list d)
  (probcc-letlazy (so-build-list d (τ (probcc-distribution '((0 1/2) (1 1/2)))))))

; cf. SO:88-94 `gte`, on big-endian bit strings of equal length; the scrutinee (l1 (), l2 ())
; of a match is evaluated from the left, l1 first (D19, amended in WP9: OCaml does not build the
; tuple of a match scrutinee and binds its components in order).
(define (so-gte l1 l2)
  (let* ((c1 (l1))
         (c2 (l2)))
    (match/first (list c1 c2)
      ((() ()) #t)
      ((() ,_) #f)
      ((,_ ()) #t)
      (((1 . ,_) (0 . ,_)) #t)
      (((0 . ,_) (1 . ,_)) #f)
      (((,_ . ,t1) (,_ . ,t2)) (so-gte t1 t2)))))

; cf. SO:114 `num_list`: n numbers of d random bits.
(define (so-num-list n d) (so-build-list n (τ (so-bit-list d))))

; cf. SO:119-123 `model`: n numbers of 20 random bits, each uniform in 0 .. 2^20 - 1, sorted, or
; the world fails; the unit value is (void).
(define ((so-model n)) (if (so-is-sorted so-gte (so-num-list n 20)) (void) (probcc-impossible)))

;; -----------------------------------------------------------------------------------------
;; Models of the statistical tests: blip/blip_naive.ml (BN) and blip/blip.ml (BL), as in the
;; suite hansei-examples
;; -----------------------------------------------------------------------------------------

; cf. BN:59-81, BL:87-109 = BA:71-93 `xderiv`, `yderiv`, `new_dir`, which do not depend on the
; plane i and the time t they take.
(define (blip-xderiv d)
  (case d
    ((North South) (probcc-distribution '((-1 0.1) (0 0.8) (1 0.1))))
    ((East) (probcc-distribution '((0 0.2) (1 0.7) (2 0.1))))
    ((West) (probcc-distribution '((0 0.2) (-1 0.7) (2 0.1))))))

(define (blip-yderiv d)
  (case d
    ((North) (probcc-distribution '((0 0.2) (1 0.7) (2 0.1))))
    ((East West) (probcc-distribution '((-1 0.1) (0 0.8) (1 0.1))))
    ((South) (probcc-distribution '((0 0.2) (-1 0.7) (2 0.1))))))

(define (blip-new-dir d)
  (case d
    ((North) (probcc-distribution '((West 0.2) (North 0.6) (East 0.2))))
    ((East) (probcc-distribution '((North 0.2) (East 0.6) (South 0.2))))
    ((South) (probcc-distribution '((East 0.2) (South 0.6) (West 0.2))))
    ((West) (probcc-distribution '((South 0.2) (West 0.6) (North 0.2))))))

; cf. BN:50-54 = BL:78-82 `xpos0`, `ypos0`, `dir0`: probcc-uniform's masses are the exact 1/10
; (deviation 7), so the masses of these models agree with upstream's to a relative 1e-9 only.
(define (blip-pos0) (probcc-uniform 10))
(define (blip-dir0) (probcc-uniform/either '(North East South West)))

; cf. BN:94-95 `plane_state_0`: the plane state record {idx; pos; dir} is the list (i (x y) dir),
; its fields and the position from the right (D19).
(define (bn-plane-state-0 i)
  (let* ((dir (blip-dir0))
         (y (blip-pos0))
         (x (blip-pos0)))
    (list i (list x y) dir)))

; cf. BN:98-109 `plane_fly`: the new direction is drawn last, by the record construction.
(define (bn-plane-fly pstate)
  (match/first pstate
    ((,i (,x ,y) ,dir)
     (let* ((x* (blip-xderiv dir))
            (y* (blip-yderiv dir))
            (newx (+ x x*)))
       (when (or (< newx 0) (> newx 9)) (probcc-impossible))
       (let1 (newy (+ y y*))
         (when (or (< newy 0) (> newy 9)) (probcc-impossible))
         (let1 (dir* (blip-new-dir dir))
           (list i (list newx newy) dir*)))))))

; cf. BN:112-113 `planes_fly`.
(define (bn-planes-fly pstates) (probcc-map bn-plane-fly pstates))

; cf. BN:124-131 `blips_at_least`: every blip of the list is seen, a false alarm or a plane at
; its position detected; nothing is said of the other positions.
(define (bn-blips-at-least blips pstates)
  (let1 (check-blip (λ (blip)
                      (or (probcc-coin 0.02)
                          (fold (λ (ps acc)
                                  (or acc (and (equal? (cadr ps) blip) (probcc-coin 0.9))))
                                #f
                                pstates))))
    (let loop ((blips blips))
      (unless (null? blips)
        (unless (check-blip (car blips)) (probcc-impossible))
        (loop (cdr blips))))))

; cf. BN:156-171 `blip_HMM_model`, with variable elimination of the previous state: nplanes is
; #f (`None`) or the bound n (`Some n`); `plane_state_0 i :: state0 (pred i)` from the right.
(define ((bn-model nplanes ntimes evidence foutcome))
  (let1 (np (if nplanes (probcc-geometric/bounded nplanes 0.85) (probcc-geometric/count 0.85)))
    (letrec ((state0 (λ (i)
                       (if (= i 0)
                           '()
                           (let* ((rest (state0 (sub1 i)))
                                  (ps (bn-plane-state-0 i)))
                             (cons ps rest)))))
             (iter (λ (t)
                     (let1 (new-ps (if (= t 0)
                                       (state0 np)
                                       (let1 (ps ((probcc-variable-elimination iter) (sub1 t)))
                                         (bn-planes-fly ps))))
                       (evidence new-ps t)
                       new-ps))))
      (foutcome (iter ntimes)))))

; cf. BN:177-180 `two_blips`.
(define (bn-two-blips ps t)
  (case t
    ((0) (bn-blips-at-least '((3 3)) ps))
    ((1) (bn-blips-at-least '((3 4)) ps))
    (else (void))))

; cf. BL:126-130 `lplane_state_0`: the lazy plane state {idx; pos; dir}, the list (i (x y) dir) of
; thunks of probcc-letlazy, built from the right (D19).
(define (bl-lplane-state-0 i)
  (let* ((dir (probcc-letlazy blip-dir0))
         (y (probcc-letlazy blip-pos0))
         (x (probcc-letlazy blip-pos0)))
    (list i (list x y) dir)))

; cf. BL:133-146 `plane_fly`: the new coordinates and direction, each lazy; a coordinate off the
; screen fails when it is forced.
(define (bl-plane-fly pstate)
  (match/first pstate
    ((,i (,x ,y) ,dir)
     (let* ((newx (τ (let* ((x* (blip-xderiv (dir)))
                            (v (+ (x) x*)))
                       (if (or (< v 0) (> v 9)) (probcc-impossible) v))))
            (newy (τ (let* ((y* (blip-yderiv (dir)))
                            (v (+ (y) y*)))
                       (if (or (< v 0) (> v 9)) (probcc-impossible) v))))
            (dir* (probcc-letlazy (τ (blip-new-dir (dir)))))
            (ly (probcc-letlazy newy))
            (lx (probcc-letlazy newx)))
       (list i (list lx ly) dir*)))))

; cf. BL:178-181 `force_plane`: the strict plane state, its fields and position from the right.
(define (bl-force-plane lstate)
  (match/first lstate
    ((,i (,x ,y) ,dir)
     (let* ((d (dir))
            (yv (y))
            (xv (x)))
       (list i (list xv yv) d)))))

; cf. BL:205-220 `blip_HMM_step`: the system parameters (annihilate create) and the system state
; (cnt planes) evolve one time step, the evidence is checked and the strict state is answered.
; The planes are thunks of probcc-letlazy, the newest first.
(define (bl-step parms state timep evidence)
  (match/first (list parms state)
    (((,annihilate ,create) (,cnt ,planes))
     (let* ((planes (annihilate planes))
            (planes (probcc-map (λ (ps) (probcc-letlazy (τ (bl-plane-fly ps)))) planes))
            (np (create)))
       (match/first (if (= np 0)
                        (list cnt planes)
                        (let1 (new-cnt (+ cnt np))
                          (let st0loop ((acc planes)
                                        (i cnt))
                            (if (>= i new-cnt)
                                (list new-cnt acc)
                                (st0loop (cons (probcc-letlazy (τ (bl-lplane-state-0 i))) acc)
                                         (add1 i))))))
         ((,cnt ,planes)
          (evidence planes timep)
          (list cnt (probcc-map (λ (th) (bl-force-plane (th))) planes))))))))

; cf. BL:232-248 `blips_ideal`: the blips are all the blips on the screen, without noise: every
; plane is at a blip, forced coordinate by coordinate, and every blip has a plane.
(define (bl-blips-ideal blips pstates)
  (letrec ((observe (λ (plane-pos blips-obs)
                      (match/first blips-obs
                        (() (probcc-impossible))
                        ((((,x ,y) ,_) . ,rest)
                         (if (and (= x ((car plane-pos))) (= y ((cadr plane-pos))))
                             (cons (list (list x y) #t) rest)
                             (cons (car blips-obs) (observe plane-pos rest))))))))
    (let1 (blips-obs-post (fold (λ (psl blips-obs) (observe (cadr (psl)) blips-obs))
                                (probcc-map (λ (pos) (list pos #f)) blips)
                                pstates))
      (let loop ((bo blips-obs-post))
        (unless (null? bo)
          (unless (cadr (car bo)) (probcc-impossible))
          (loop (cdr bo)))))))

; cf. BL:251-261: a geometric number of planes appearing, and the initial state.
(define (bl-parm-n n) (list identity (τ (probcc-geometric/bounded n 0.85))))
(define bl-state0 '(0 ()))

; cf. BL:432-436 `blip333_ideal`: three blips, each one cell further along x at each step.
(define (blip333-ideal ps t)
  (case t
    ((0) (bl-blips-ideal '((3 5) (3 7) (3 9)) ps))
    ((1) (bl-blips-ideal '((4 5) (4 7) (4 9)) ps))
    ((2) (bl-blips-ideal '((5 5) (5 7) (5 9)) ps))
    (else (void))))

(define-suite hansei-long

  ((doc r)
   `((structure/section "Long tests")
     (p "The slow cases of the HANSEI suites: "
        (cite/a "https://okmij.org/ftp/kakuritu/logic-programming.html#zebra" "the zebra puzzle")
        ", the replays of upstream's longest sampling runs, of the parser combinators, and the
	statistical tests of importance sampling over the seeds 1-20. The whole suite takes
	about 40 s under csi over the compiled library (WP9 and the final review).")))

  ((test/zebra _)
   (define solution '((Yellow NO Fox Water Kools) (Blue UA Horse Tea Chest) (Red EN Snail Milk OGold)
                      (Ivory SP Dog OJ LStrike) (Green JA Zebra Coffee Parliaments)))
   (define table (probcc-reify/exact (z-model)))
   (⊦ equal? `(((V ,solution) 1/199065600000)) table)
   (⊦ equal? `(1/199065600000 (((V ,solution) 1.))) (probcc-normalize/total table))
   ; upstream's evidence, a product of floats, is the exact weight to a relative 1e-15.
   (⊦ (equal-approx? 1e-15) 1. (/ 5.023469650205764e-12 (exact->inexact 1/199065600000)))
   `(doc (p "The zebra puzzle (Z:1-190, LG " (code/inline "#zebra")
            "), stated directly: five attributes of five houses, each a lazy permutation, so
	    that the all-different constraint costs nothing to state and is enforced as the
	    attributes are asked for. The solution is unique, at weight 1/199065600000, (5!)^5
	    times the 2^3 sides of the three `next to' facts: upstream's "
            (code/inline "5.023469650205764e-12") " is the inverse of the size of the search
	    space, of which only a small part is generated. About 3 s.")))

  ((test/replay/parsec/long _)
   (⊦ equal? '(((V (5 "05*1")) 2.60416666666666626e-07))
      (probcc-sample-importance/a (probcc-random-selector 37) 30000
                                  (expression-model/value exp-stream 5)))
   (⊦ equal? '(((V (5 "03+2")) 1.66666666666666666e-06))
      (probcc-sample-importance/a (probcc-random-selector 17) 30000
                                  (expression-model/value exp-stream/balanced 5)))
   `(doc (p "The sampling records of PS:504-512 and PS:549-557, importance sampling of 30000
	    expressions of four characters whose value is 5, with "
            (code/inline "(probcc-random-selector 37)") " over the uniform streams and "
            (code/inline "(probcc-random-selector 17)") " over the balanced ones, replayed bit
	    for bit, the characters drawn with a transcription of upstream's "
            (code/inline "uniform") " (P:156-165). About 4 s and 2 s.")))

  ((test/statistics/bloodtype _)
   ; cf. BT:255-260 test1exact, by variable elimination (the suite hansei-examples).
   (⊦ equal? '()
      (statistics-misses (statistics/importance 1000 (bt-bldtype 'Linus) identity)
                         '((A 0.32180904495702678) (B 0.320852692775250137)
                           (AB 0.197860551516677169) (O 0.159477710751046636))
                         0.05))
   `(doc (p "Importance sampling of Linus' blood type (BT:146-147), 1000 worlds for each seed
	    1-20: the mean of every mass is within 4σ/√20, or 0.05, of the exact one.")))

  ((test/statistics/hmm _)
   ; cf. HM:158 tq1_exact', normalized in upstream's order (the suite hansei-examples).
   (⊦ equal? '()
      (statistics-misses (statistics/importance 2000 (hm-query1 10) probcc-normalize)
                         '((1 0.210857962344722732) (2 0.197112445175818218)
                           (3 0.172546876239070945) (4 0.141537960864701051)
                           (5 0.108461707082790779) (6 0.0774529825998843541)
                           (7 0.0528876908651939) (8 0.0391423748278179676))
                         0.03))
   `(doc (p "Importance sampling of the hidden state at time 10 of the HMM observed at time 5
	    (HM:102-117), 2000 worlds for each seed 1-20, normalized: every mean within
	    4σ/√20, or 0.03, of the exact posterior.")))

  ((test/statistics/colored-balls _)
   (define blues '(Blue Blue Blue Blue Blue Blue Blue Blue Blue Blue))
   (define posterior (cb-posterior 4/5 10))
   (⊦= 0.41196368 (cadr (assv 1 posterior)))
   (⊦ equal? '()
      (statistics-misses (statistics/importance 2000 (cb-model-nballs 0.8 blues) probcc-normalize)
                         (probcc-map (λ1-match/first ((,n ,p) (list n (exact->inexact p))))
                                     posterior)
                         0.05))
   `(doc (p "Importance sampling of the number of balls after ten draws that all look blue
	    (CB:50-63), 2000 worlds for each seed 1-20, normalized: every mean within 4σ/√20,
	    or 0.05, of the exact posterior, computed in closed form; P(n = 1) = 0.41196,
	    as Milch et al.'s Fig. 1.7 and CB's 10000 samples (0.405) suggest.")))

  ((test/statistics/drunk-coin _)
   (⊦ equal? '()
      (statistics-misses (statistics/importance 5000 (τ (dcoin-and 10)) identity)
                         '((#f 0.0526315789473632695))
                         (* 0.1 0.0526315789473632695)))
   `(doc (p "Importance sampling of " (code/inline "dcoin_and 10")
            " (SM:224-280), 5000 worlds for each seed 1-20: the mean mass of "
            (code/inline "false") " is within 4σ/√20, or 10%, of the exact "
            (code/inline "0.0526315789473632695") " (the probability of "
            (code/inline "true") ", 1e-13, is far below what 5000 samples can see).")))

  ((test/statistics/sorted _)
   ; cf. SO:185-200, invfact n, the probability that n uniform numbers are sorted.
   (define (misses n nsamples)
     (let1 (invfact (/ 1 (fold * 1 (iota n 1))))
       (statistics-misses (statistics/importance nsamples (so-model n) identity)
                          `((,(void) ,invfact))
                          (* 0.1 invfact))))
   (⊦ equal? '() (misses 2 100))
   (⊦ equal? '() (misses 3 100))
   (⊦ equal? '() (misses 4 200))
   `(doc (p "Importance sampling of the sorted-list model (SO:119-123), lists of 2, 3 and 4
	    numbers of 20 bits with 100, 100 and 200 worlds (SO:185-200) for each seed 1-20:
	    the mean probability of being sorted is within 4σ/√20, or 10%, of the exact 1/n!.
	    Upstream's records of these runs do not replay (plan D20); what upstream gives
	    under OCaml today is replayed in the suite hansei-lazy.")))

  ((test/statistics/blip _)
   (define (unit _) (void))
   (define (nplanes st) (length (cadr st)))
   ; cf. BN:246-257, one plane in motion and its two blips; exact 0.00059257469565211177.
   (⊦ equal? '()
      (statistics-misses (statistics/importance 1000 (bn-model 1 1 bn-two-blips unit) identity)
                         `((,(void) 0.00059257469565211177))
                         (* 0.1 0.00059257469565211177)))
   ; cf. BL:511-522, three lazy planes and three blips, the first step; exact
   ; 1.72212182417348577e-08 (BL:522, and the suite hansei-examples).
   (let1 (step0 (τ (bl-step (bl-parm-n 3) bl-state0 0 blip333-ideal)))
     (⊦ equal? '()
        (statistics-misses (statistics/importance 1500 step0
                                                  (λ (table) (probcc-map-dist nplanes table)))
                           '((3 1.72212182417348577e-08))
                           (* 0.1 1.72212182417348577e-08))))
   `(doc (p "Importance sampling of the planes, whose records (BN:254-257, BL:513-520) do not
	    replay (plan D20): 1000 worlds of one plane in motion seen at two blips, and 1500
	    worlds of the first step of three lazy planes seen at three blips, for each seed
	    1-20: every mean within 4σ/√20, or 10%, of the exact value that the suite
	    hansei-examples computes, " (code/inline "0.00059257469565211177") " and "
            (code/inline "1.72212182417348577e-08") ". About 7 s.")))

  )

; a resume that fails to rewind its `dynamic-wind` frames would leave a test's string port as
; the current output port, hiding the summary; check that the port is back.
(import (only (chicken base) exit))

(define stdout (current-output-port))

(let1 (r (unittest/✓ hansei-long))
  (unless (eq? stdout (current-output-port))
    (current-output-port stdout)
    (write (unittest/result-summary r))
    (newline)
    (display "FAIL: the suite ended with its output still captured by a test\n")
    (exit 1)))
