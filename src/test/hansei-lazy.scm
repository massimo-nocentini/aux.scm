; The suite for (aux hansei lazy): the lazy-list kit of slazy.ml, music1a.ml and music2.ml, lazy
; permutations (lazy_permutations.ml) and the sorted-list problem (sorted.ml).  Cases L1-L17 of
; the plan AUX-HANSEI-PLAN.md §8.3 (WP6; the sorted list of four numbers of five bits, WP9).
; Expected values are upstream's, re-sorted in the Scheme order (descending mass, ties in
; descending pmap-compare key order, plan D5); for the values of these models (integers,
; booleans, strings and lists of them) the tie order is upstream's own listing order.

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
; OCaml evaluates the components of a tuple, the elements of a list literal and the arguments of
; a call from right to left, and `let x = e1 and y = e2 in` and the components of the scrutinee
; of `match (e1, e2) with` from left to right (D19): the ported models sequence them with let*,
; as their comments say.

(import scheme
        (chicken base)
        (chicken flonum)
        (only srfi-1 iota)
        (aux base)
        (aux unittest)
        (aux hansei)
        (aux hansei lazy))

; replays and bit-exact checks print 17 digits: with the default 15, a one-ulp failure would print
; identical expected and got values (plan §8 conventions).
(flonum-print-precision 17)

;; -----------------------------------------------------------------------------------------
;; Models of slazy.ml (SL)
;; -----------------------------------------------------------------------------------------

; cf. SL:8 `expensive_f`, which prints a line where this one counts its calls, over all the
; worlds; (with-calls thunk) answers the value of thunk and that count, as (list v count).
(define expensive-calls 0)
(define (expensive-f x) (set! expensive-calls (add1 expensive-calls)) x)
(define (with-calls thunk)
  (set! expensive-calls 0)
  (let1 (v (thunk))
    (list v expensive-calls)))

; The models of SL take the mass h of their fair choices: the exact 1/2 for the exact tables,
; upstream's 0.5 for the replays, whose sampled masses are then flonums as upstream's.  `None`
; is #f and `Some t` is the list of t.

; cf. SL:15-20, the baseline: x is computed in every world, also where it is not needed.
(define ((tests1 h))
  (let* ((u (probcc-distribution `((0 ,h) (1 ,h))))
         (x (expensive-f (probcc-distribution `((,(+ u 10) ,h) (,(+ u 20) ,h))))))
    (cond
      ((= u 0) #f)
      (else (let1 (c (probcc-coin h))
              (list x x c))))))

; cf. SL:35-40, laziness by a thunk, which is not shared: the tuple from the right (D19).
(define ((tests2 h))
  (let* ((u (probcc-distribution `((0 ,h) (1 ,h))))
         (x (τ (expensive-f (probcc-distribution `((,(+ u 10) ,h) (,(+ u 20) ,h)))))))
    (cond
      ((= u 0) #f)
      (else (let* ((c (probcc-coin h))
                   (b (x))
                   (a (x)))
              (list a b c))))))

; cf. SL:57-61, laziness by OCaml's `lazy`, here CHICKEN's delay and force (plan D9: never in a
; model): the promise is shared by the worlds that split inside its first force, and the first
; of them to finish fixes its value for the others; the tuple from the right (D19).
(define ((tests3 h))
  (let* ((u (probcc-distribution `((0 ,h) (1 ,h))))
         (x (delay (expensive-f (probcc-distribution `((,(+ u 10) ,h) (,(+ u 20) ,h)))))))
    (cond
      ((= u 0) #f)
      (else (let* ((c (probcc-coin h))
                   (b (force x))
                   (a (force x)))
              (list a b c))))))

; cf. SL:73-79, probcc-letlazy: shared and lazy; the coin is flipped before x is forced (D19).
(define ((tests4 h))
  (let* ((u (probcc-distribution `((0 ,h) (1 ,h))))
         (x (probcc-letlazy
              (τ (expensive-f (probcc-distribution `((,(+ u 10) ,h) (,(+ u 20) ,h))))))))
    (cond
      ((= u 0) #f)
      (else (let* ((c (probcc-coin h))
                   (b (x))
                   (a (x)))
              (list a b c))))))

; cf. SL:99-103.
(define ((testl1 h))
  (let* ((u (probcc-distribution `((1 ,h) (2 ,h))))
         (x (expensive-f (probcc-distribution `((,(* 2 u) ,h) (,(* 3 u) ,h))))))
    (probcc-distribution `(((,u ,u) ,h) ((,x ,x) ,h)))))

; cf. SL:112-117: a choice of thunks, the chosen one called.
(define ((testl2 h))
  (let* ((u (probcc-distribution `((1 ,h) (2 ,h))))
         (x (probcc-letlazy
              (τ (expensive-f (probcc-distribution `((,(* 2 u) ,h) (,(* 3 u) ,h))))))))
    ((probcc-distribution `((,(τ (list u u)) ,h)
                            (,(τ (let* ((b (x)) (a (x))) (list a b))) ,h))))))

; cf. SL:143-148.
(define ((testl3 h))
  (let* ((u (probcc-distribution `((1 ,h) (2 ,h))))
         (x (expensive-f (probcc-distribution `((,(* 10 u) ,h) (,(* 100 u) ,h)))))
         (v (probcc-coin h)))
    (list u v x x)))

; cf. SL:161-168: x escapes the scope of its letlazy; forced after v is drawn (D19).
(define ((testl4 h))
  (let* ((u (probcc-distribution `((1 ,h) (2 ,h))))
         (x (probcc-letlazy
              (τ (expensive-f (probcc-distribution `((,(* 10 u) ,h) (,(* 100 u) ,h)))))))
         (v (probcc-coin h))
         (d (x))
         (c (x)))
    (list u v c d)))

; cf. SL:225-229: the list literal [(0.5, 2*u ()); (0.5, 3*u ())] from the right (D19).
(define ((testl51 h))
  (let* ((u (probcc-letlazy (τ (probcc-distribution `((1 ,h) (2 ,h))))))
         (x (let* ((b (* 3 (u)))
                   (a (* 2 (u))))
              (expensive-f (probcc-distribution `((,a ,h) (,b ,h)))))))
    ((probcc-distribution `((,(τ (let* ((b (u)) (a (u))) (list a b))) ,h)
                            (,(τ (list x x)) ,h))))))

; cf. SL:238-243: both pairs are built before the choice, the second first (D19).
(define ((testl52 h))
  (let* ((u (probcc-letlazy (τ (probcc-distribution `((1 ,h) (2 ,h))))))
         (x (probcc-letlazy (τ (let* ((b (* 3 (u)))
                                      (a (* 2 (u))))
                                 (expensive-f (probcc-distribution `((,a ,h) (,b ,h))))))))
         (q (let* ((b (x)) (a (x))) (list a b)))
         (p (let* ((b (u)) (a (u))) (list a b))))
    (probcc-distribution `((,p ,h) (,q ,h)))))

; cf. SL:253-259, without the `here` line.
(define ((testl53 h))
  (let* ((u (probcc-letlazy (τ (probcc-distribution `((1 ,h) (2 ,h))))))
         (x (probcc-letlazy (τ (let* ((b (* 3 (u)))
                                      (a (* 2 (u))))
                                 (expensive-f (probcc-distribution `((,a ,h) (,b ,h)))))))))
    ((probcc-distribution `((,(τ (let* ((b (u)) (a (u))) (list a b))) ,h)
                            (,(τ (let* ((b (x)) (a (x))) (list a b))) ,h))))))

; cf. SL:268-277, dependent lazy variables: the list literal is built from the right, so u is
; forced before the choice of a thunk; the tuples from the right too (D19).
(define ((testl54 h))
  (let* ((u (probcc-letlazy (τ (probcc-distribution `((1 ,h) (2 ,h))))))
         (x (probcc-letlazy (τ (expensive-f (let1 (u* (u))
                                              ((probcc-distribution `((,(τ u*) ,h) (,u ,h)))))))))
         (c (probcc-coin h)))
    (if c
        (let* ((e (x)) (d (u)) (b (x)) (a (u))) (list c a b d e))
        (let* ((e (u)) (d (x)) (b (u)) (a (x))) (list c a b d e)))))

; cf. SL:357-362, Ken Shan's test: a lazy list of unbounded length, of which
; probcc-llength/at-most forces at most 5 cells.
(define ((ll-count n))
  (probcc-distribution `((() 1/2) (,(cons (τ n) (probcc-letlazy (ll-count (add1 n)))) 1/2))))

; cf. SL:371-377, the PCFG of IBAL's Music TR: a noun phrase is a noun, or a noun followed by
; a noun phrase; the list literal from the right, so the lappend is built before the choice.
(define (pcfg-model)
  (letrec ((noun (τ (let1 (x (probcc-letlazy
                               (τ (probcc-distribution '(("flies" 0.4) ("ants" 0.6))))))
                      (cons x probcc-lnil))))
           (np (τ (let* ((np* (probcc-letlazy np))
                         (a (probcc-lappend noun np*)))
                    ((probcc-distribution `((,noun 0.7) (,a 0.3))))))))
    (np)))

; cf. SL:380-383.
(define (pcfg11)
  (let* ((np (pcfg-model))
         (b ((probcc-lhead (τ np))))
         (a ((probcc-lhead (τ np)))))
    (list a b)))

; cf. SL:394-397.
(define (pcfg12)
  (let* ((np (pcfg-model))
         (b (probcc-lreflect (probcc-lhead (probcc-ltail (τ np)))))
         (a (probcc-lreflect (probcc-lhead (probcc-ltail (τ np))))))
    (list a b)))

; cf. SL:441-446.
(define (pcfg13)
  (let* ((np (pcfg-model))
         (d (probcc-lreflect (probcc-lhead (τ np))))
         (c (probcc-lreflect (probcc-lhead (τ np))))
         (b (probcc-lreflect (probcc-lhead (probcc-ltail (τ np)))))
         (a (probcc-lreflect (probcc-lhead (probcc-ltail (τ np))))))
    (list a b c d)))

; cf. SL:459-464, with the evidence that the first word is "flies".
(define (pcfg)
  (let* ((x (probcc-observe (λ (x) (equal? "flies" (probcc-lreflect (probcc-lhead (τ x)))))
                            pcfg-model))
         (b (probcc-lreflect (probcc-lhead (probcc-ltail (τ x)))))
         (a (probcc-lreflect (probcc-lhead (τ x)))))
    (list a b)))

; cf. P:156-165 `uniform`, upstream's, with flonum masses: 1. /. n, the value 0 getting
; 1. -. pacc.  probcc-uniform's masses are the exact 1/n (deviation 7), so the replays of
; models that draw digits use this one to give upstream's flonums bit for bit.
(define (ml-uniform n)
  (cond
    ((= n 1) 0)
    (else (let1 (p (/ 1. n))
            (let loop ((pacc 0.) (acc '()) (i (sub1 n)))
              (if (zero? i)
                  (probcc-distribution (cons (list i (- 1. pacc)) acc))
                  (loop (+ pacc p) (cons (list i p) acc) (sub1 i))))))))

; cf. SL:510-526: n digits drawn whole, then compared with the observation of all ones.
(define (full-10tree uniform n)
  (cond
    ((= n 1) (list (uniform 10)))
    (else (let* ((x (uniform 10))
                 (xs (full-10tree uniform (sub1 n))))
            (cons x xs)))))

(define (ones n l)
  (cond
    ((and (= n 0) (null? l)) #t)
    ((and (pair? l) (eqv? 1 (car l))) (ones (sub1 n) (cdr l)))
    (else #f)))

(define ((full-10tree-obs uniform n))
  (if (ones n (full-10tree uniform n)) (void) (probcc-impossible)))

; cf. SL:580-600: the digits as a lazy list, compared as they are forced.
(define (full-10ltree uniform n)
  (cond
    ((= n 1) (let1 (x (probcc-letlazy (τ (uniform 10))))
               (cons x probcc-lnil)))
    (else (let* ((x (probcc-letlazy (τ (uniform 10))))
                 (xs (probcc-letlazy (τ (full-10ltree uniform (sub1 n))))))
            (cons x xs)))))

(define (lones n l)
  (match/first (l)
    (() (= n 0))
    ((,h . ,t) (and (= 1 (h)) (lones (sub1 n) t)))))

(define ((full-10ltree-obs uniform n))
  (let1 (t (full-10ltree uniform n))
    (if (lones n (τ t)) ((probcc-lhead (τ t))) (probcc-impossible))))

; cf. SL:623-634, n flips drawn whole, and SL:636-645 as a lazy list.
(define (flips p n)
  (cond
    ((= n 0) '())
    (else (let* ((x (probcc-coin p))
                 (xs (flips p (sub1 n))))
            (cons x xs)))))

(define (trues n xs)
  (if (= n 0) (null? xs) (and (car xs) (trues (sub1 n) (cdr xs)))))

(define (lflips p n)
  (cond
    ((= n 0) '())
    (else (let* ((x (probcc-letlazy (τ (probcc-coin p))))
                 (xs (probcc-letlazy (τ (lflips p (sub1 n))))))
            (cons x xs)))))

(define (ltrues n xs)
  (match/first xs
    (() (= n 0))
    ((,x . ,xs*) (and (x) (ltrues (sub1 n) (xs*))))))

; cf. SL:661-668 and SL:801-856: f and the tuples, from the right (D19).
(define (f-coin x) (probcc-distribution `((,x 1/2) (,(add1 x) 1/2))))

(define (test1m)
  (let1 (c (probcc-coin 1/2))
    (if c
        (let* ((e (f-coin 2)) (d (f-coin 2)) (b (f-coin 1)) (a (f-coin 1))) (list c a b d e))
        (let* ((e (f-coin 1)) (d (f-coin 1)) (b (f-coin 2)) (a (f-coin 2))) (list c a b d e)))))

; cf. SL:731-740, the sprinkler: the parameters (`let … and …`), the priors of rain and of the
; sprinkler on a day, and noisy-or, with flonum masses as upstream.
(define rain-str 0.9)
(define rain-prior 0.3)
(define sprinkler-str 0.8)
(define sprinkler-prior 0.5)
(define grass-baserate 0.1)
(define (rain day) (probcc-coin rain-prior))
(define (sprinkler day) (probcc-coin sprinkler-prior))
(define (noisy-or a astrength b bstrength baserate)
  (or (and (probcc-coin astrength) (a))
      (and (probcc-coin bstrength) (b))
      (probcc-coin baserate)))

; cf. SL:742-776, `grass_model0` and `grass_model1`: memoized rain, sprinkler and wet grass over
; days (`let rain = memo rain and sprinkler = memo sprinkler`, rain first, D19), then query
; answers the result for the memoized grass-is-wet and rain.
(define ((grass-model query))
  (let* ((rain (probcc-memo rain))
         (sprinkler (probcc-memo sprinkler))
         (grass-is-wet (probcc-memo (λ (day)
                                      (noisy-or (τ (rain day)) rain-str
                                                (τ (sprinkler day)) sprinkler-str
                                                grass-baserate)))))
    (query grass-is-wet rain)))

; cf. SL:801-809: x may fail, but only where it is forced.
(define (testd-u&x)
  (let* ((u (probcc-letlazy (τ (probcc-distribution '((1 1/2) (2 1/2))))))
         (x (probcc-letlazy
              (τ (probcc-observe (λ (u) (= u 1))
                                 (τ (expensive-f
                                      (let1 (u* (u))
                                        ((probcc-distribution `((,(τ u*) 1/2) (,u 1/2))))))))))))
    (list u x)))

(define (testd1)
  (match/first (testd-u&x)
    ((,u ,x) ((probcc-distribution `((,(τ (let* ((b (u)) (a (u))) (list a b))) 1/2)
                                     (,(τ (let* ((b (x)) (a (x))) (list a b))) 1/2)))))))

; cf. SL:848-856: x is never forced, so its failure is not seen.
(define (testd2)
  (match/first (testd-u&x)
    ((,u ,x) (let1 (z (cons u x))
               ((car z))))))

;; -----------------------------------------------------------------------------------------
;; Models of lazy_permutations.ml (LP)
;; -----------------------------------------------------------------------------------------

; cf. LP:36-41 `(f1 0, f1 1, f1 0, f1 1)`: the tuple of four applications of f, from the right
; (D19).
(define (tuple-0101 f)
  (let* ((d (f 1))
         (c (f 0))
         (b (f 1))
         (a (f 0)))
    (list a b c d)))

; cf. LP:37-40, 48-50: `function 0 -> uniformly_list [0;1] | 1 -> uniformly_list [0;1]`.
(define (f01 x)
  (match/first x
    (0 (probcc-uniform/either '(0 1)))
    (1 (probcc-uniform/either '(0 1)))))

; (new) the permutations of '(4 3 2 1), in descending lexicographic order, the 24 values of
; LP:214-217 in the D5 order.
(define permutations-of-4
  '((4 3 2 1) (4 3 1 2) (4 2 3 1) (4 2 1 3) (4 1 3 2) (4 1 2 3)
    (3 4 2 1) (3 4 1 2) (3 2 4 1) (3 2 1 4) (3 1 4 2) (3 1 2 4)
    (2 4 3 1) (2 4 1 3) (2 3 4 1) (2 3 1 4) (2 1 4 3) (2 1 3 4)
    (1 4 3 2) (1 4 2 3) (1 3 4 2) (1 3 2 4) (1 2 4 3) (1 2 3 4)))

;; -----------------------------------------------------------------------------------------
;; Models of sorted.ml (SO): its lazy lists have strict heads, (head . tail-llist) (SO:19-20).
;; -----------------------------------------------------------------------------------------

; cf. SO:28-31 `build_list`: a list of n results of thunk, each tail lazy; OCaml builds the
; constructor from the right (D19), the letlazy before the head.
(define ((so-build-list n thunk))
  (cond
    ((= n 0) '())
    (else (let* ((t (probcc-letlazy (so-build-list (sub1 n) thunk)))
                 (h (thunk)))
            (cons h t)))))

; cf. SO:34-36 `to_list`.
(define (so-to-list l)
  (match/first (l)
    (() '())
    ((,d . ,l*) (cons d (so-to-list l*)))))

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

; cf. SO:141-145 `sum`: the sum of body i for i from n to m.
(define (so-sum n m body)
  (cond
    ((= n m) (body n))
    ((> n m) 0)
    (else (+ (body n) (so-sum (add1 n) m body)))))

(define-suite hansei-lazy

  ((doc r)
   `((structure/section "Correlated memoization and lazy permutations")
     (p "Lazy lists, shared lazily-evaluated variables and memoized stochastic functions, after "
        (cite/a "https://okmij.org/ftp/kakuritu/Hansei.html#all-different" "Correlated memoization and lazy permutations")
        ", " (code/inline "slazy.ml") " and "
        (cite/a "https://okmij.org/ftp/kakuritu/Hansei.html#sorted" "Realistic Example: sorted list") ":"
        (cite/quote
          "Oleg Kiselyov"
          "Sampling of permutations, stochastic bijective functions, sequences of correlated
	  random variables, the all-different constraint -- are related to the same problem. A
	  problem with so many names must be important, and so must be its efficient solution."))
     (p "The goal is a sequence of N all-different random variables over 0 .. N-1, a stochastic
	bijection: the naive " (code/inline "f1") " of LP:36-41, which draws "
        (code/inline "uniformly_list [0;1]") " at every application, is not even a function
	(test " (code/inline "test/correlated") "); " (code/inline "probcc-memo")
        " makes it one, and " (code/inline "probcc-lazy-permutation") " (LP:127-229) and "
        (code/inline "probcc-lazy-permutation/ins") " (LP:266-330) correlate the memoized
	values, so that each one is drawn only when asked for, among the values still free.")
     (structure/section "Laziness and non-determinism")
     (p "Laziness and non-determinism make a tricky combination (SL:10-93): a variable bound with "
        (code/inline "let") " is computed in every world, also where it is not needed; a thunk
	is not shared; and the built-in " (code/inline "delay") " is shared across worlds, which
	gives a wrong distribution (DSL p.21 l.912-922). " (code/inline "probcc-letlazy")
        " evaluates its body at most once per world, when it is first needed (P:270-330).")
     (structure/section "Realistic example: sorted list")
     (p "Harik's problem (H " (code/inline "#sorted") ", SO:1-11) draws N numbers uniformly
	from 0 .. M-1 and asks for the probability that they are sorted; with numbers as lazy
	lists of bits, the comparison stops at the first differing bit, which is crucial:"
        (cite/quote
          "Oleg Kiselyov"
          "Harik's problem is ideal for a benchmark: it is easy to explain and to model. The
	  expected result is also known. Getting any useful estimate of the result is rather
	  difficult, especially without extensive fine-tuning of the inference algorithm.")
        "The exact probability for four numbers of five bits, over 2^20 worlds, and upstream's
	importance-sampling runs of it are checked in " (code/inline "test/sorted") ".")))

  ((test/llist/kit _)
   (define (to-list l)
     (match/first (l)
       (() '())
       ((,h . ,t) (let1 (x (h)) (cons x (to-list t))))))
   (define abc (probcc-list->llist '(a b c)))
   (⊦ equal? '() (to-list probcc-lnil))
   (⊦ equal? '(a b c) (to-list abc))
   (⊦ equal? '(x) (to-list (probcc-lcons (τ 'x) probcc-lnil)))
   (⊦ equal? 3 (probcc-llength abc))
   (⊦ equal? 0 (probcc-llength probcc-lnil))
   (⊦ equal? 2 (probcc-llength/at-most 2 abc))
   (⊦ equal? 3 (probcc-llength/at-most 7 abc))
   (⊦ equal? 0 (probcc-llength/at-most 0 abc))
   (⊦ equal? 'a (probcc-lreflect (probcc-lhead abc)))
   (⊦ equal? '(b c) (to-list (probcc-ltail abc)))
   (⊦ equal? 'z (probcc-lreflect (τ 'z)))
   (⊦ equal? '(((V (a b c d e)) 1))
      (probcc-reify/exact (to-list (probcc-lappend abc (probcc-list->llist '(d e))))))
   (⊦ equal? '(((V (a b c)) 1))
      (probcc-reify/exact (to-list (probcc-lappend probcc-lnil abc))))
   (⊦ equal? '(((V (10 20 30)) 1))
      (probcc-reify/exact (to-list (probcc-lmap (λ (x) (* 10 x))
                                                (probcc-list->llist '(1 2 3))))))
   (⊦ equal? '() (probcc-reify/exact (probcc-lhead probcc-lnil)))
   (⊦ equal? '() (probcc-reify/exact (probcc-ltail probcc-lnil)))
   ; a stochastic lmap is shared: the two traversals agree in every world.
   (⊦ equal? '(((V ((#t #t) (#t #t))) 1/4) ((V ((#t #f) (#t #f))) 1/4)
               ((V ((#f #t) (#f #t))) 1/4) ((V ((#f #f) (#f #f))) 1/4))
      (probcc-reify/exact (let* ((l (probcc-lmap (λ (_) (probcc-coin 1/2))
                                                 (probcc-list->llist '(1 2))))
                                 (b (to-list l))
                                 (a (to-list l)))
                            (list a b))))
   ; so are the cells of a lappend, whose second list, of a random length, is forced once.
   (⊦ equal? '(((V (a b)) 1/2) ((V (a)) 1/2))
      (probcc-reify/exact
        (let* ((l (probcc-lappend (probcc-list->llist '(a))
                                  (τ (if (probcc-coin 1/2) '() (cons (τ 'b) probcc-lnil)))))
               (b (to-list l))
               (a (to-list l)))
          (if (equal? a b) a 'unshared))))
   (⊦ equal? `(((V ,(void)) 1)) (probcc-reify/exact (probcc-lobserve abc '(a b c))))
   (⊦ equal? '() (probcc-reify/exact (probcc-lobserve abc '(a x c))))
   (⊦ equal? '() (probcc-reify/exact (probcc-lobserve abc '(a b))))
   (⊦ equal? '() (probcc-reify/exact (probcc-lobserve abc '(a b c d))))
   (⊦ equal? `(((V ,(void)) 1/4))
      (probcc-reify/exact
        (probcc-lobserve (probcc-lmap (λ (_) (probcc-coin 1/2)) (probcc-list->llist '(1 2)))
                         '(#t #f))))
   `(doc (p "The lazy-list kit of SL:325-355, M1:51-101 and M2:113-119, promoted to a library
	    (deviation 24): " (code/inline "'()") " and pairs " (code/inline "(head-thunk . tail)")
            " stand for " (code/inline "LNil") " and " (code/inline "LCons") "; "
            (code/inline "probcc-lappend") " and " (code/inline "probcc-lmap")
            " are built with " (code/inline "probcc-letlazy") ", so their cells are shared in a
	    world; " (code/inline "probcc-lobserve") " fails as soon as a head or the length
	    differs, forcing no more than that.")))

  ((test/lazy/sharing _)
   (⊦ equal? '((((V #f) 1/2) ((V (21 21 #t)) 1/8) ((V (21 21 #f)) 1/8) ((V (11 11 #t)) 1/8)
                ((V (11 11 #f)) 1/8))
               4)
      (with-calls (τ (probcc-reify/exact/a (tests1 1/2)))))
   (⊦ equal? '((((V #f) 1/2) ((V (21 21 #t)) 1/16) ((V (21 21 #f)) 1/16) ((V (21 11 #t)) 1/16)
                ((V (21 11 #f)) 1/16) ((V (11 21 #t)) 1/16) ((V (11 21 #f)) 1/16)
                ((V (11 11 #t)) 1/16) ((V (11 11 #f)) 1/16))
               12)
      (with-calls (τ (probcc-reify/exact/a (tests2 1/2)))))
   ; cf. SL:63-70: upstream's Lazy loses (21 21 false) and computes x 2 times; delay and force
   ; lose the value 21 altogether, also computed 2 times.
   (⊦ equal? '((((V #f) 1/2) ((V (11 11 #t)) 1/4) ((V (11 11 #f)) 1/4)) 2)
      (with-calls (τ (probcc-reify/exact/a (tests3 1/2)))))
   (⊦ equal? '((((V #f) 1/2) ((V (21 21 #t)) 1/8) ((V (21 21 #f)) 1/8) ((V (11 11 #t)) 1/8)
                ((V (11 11 #f)) 1/8))
               4)
      (with-calls (τ (probcc-reify/exact/a (tests4 1/2)))))
   `(doc (p "Call-by-need and non-determinism (SL:10-93): a strict " (code/inline "x")
            " is computed 4 times, also in the world " (code/inline "u = 0") " where it is not
	    needed; a thunk is not shared, so the two components may differ (12 calls); a
	    promise of " (code/inline "delay") " is shared too widely, by the worlds that split
	    inside its first " (code/inline "force") ", so the first world to finish fixes the
	    value for the others and the distribution is wrong (" (code/inline "tests3")
            ", SL:57-70, where OCaml's " (code/inline "Lazy") " is wrong too, differently); "
            (code/inline "probcc-letlazy") " gives the strict distribution and computes "
            (code/inline "x") " only where it is needed, 4 times: the world is split by the coin
	    before " (code/inline "x") " is forced (SL:85-93).")))

  ((test/lazy/variables _)
   (define table '(((V (2 2)) 3/8) ((V (1 1)) 1/4) ((V (6 6)) 1/8) ((V (4 4)) 1/8)
                   ((V (3 3)) 1/8)))
   (define table3 '(((V (2 #t 200 200)) 1/8) ((V (2 #t 20 20)) 1/8) ((V (2 #f 200 200)) 1/8)
                    ((V (2 #f 20 20)) 1/8) ((V (1 #t 100 100)) 1/8) ((V (1 #t 10 10)) 1/8)
                    ((V (1 #f 100 100)) 1/8) ((V (1 #f 10 10)) 1/8)))
   (⊦ equal? (list table 4) (with-calls (τ (probcc-reify/exact/a (testl1 1/2)))))
   (⊦ equal? (list table 4) (with-calls (τ (probcc-reify/exact/a (testl2 1/2)))))
   (⊦ equal? (list table3 4) (with-calls (τ (probcc-reify/exact/a (testl3 1/2)))))
   (⊦ equal? (list table3 8) (with-calls (τ (probcc-reify/exact/a (testl4 1/2)))))
   (⊦ equal? (list table 4) (with-calls (τ (probcc-reify/exact/a (testl51 1/2)))))
   (⊦ equal? (list table 4) (with-calls (τ (probcc-reify/exact/a (testl52 1/2)))))
   (⊦ equal? (list table 4) (with-calls (τ (probcc-reify/exact/a (testl53 1/2)))))
   (⊦ equal? '((((V (#t 2 2 2 2)) 1/4) ((V (#t 1 1 1 1)) 1/4) ((V (#f 2 2 2 2)) 1/4)
                ((V (#f 1 1 1 1)) 1/4))
               8)
      (with-calls (τ (probcc-reify/exact/a (testl54 1/2)))))
   `(doc (p "Lazy variables in " (code/inline "testl1") " .. " (code/inline "testl54")
            " (SL:99-284): the lazy and the strict models have the same distribution, and the
	    expensive function is called as often as upstream's prints its line; a variable that
	    escapes the scope of its " (code/inline "letlazy") ", forced after the world splits,
	    is computed in each world (8 calls in " (code/inline "testl4") ").")))

  ((test/llist/at-most _)
   (⊦ equal? '(((V 0) 1/2) ((V 1) 1/4) ((V 2) 1/8) ((V 3) 1/16) ((V 5) 1/32) ((V 4) 1/32))
      (probcc-reify/exact (probcc-llength/at-most 5 (probcc-letlazy (ll-count 0)))))
   `(doc (p "Ken Shan's test (SL:349-367): the length of a lazy list of unbounded length,
	    counting at most 5 cells, explores finitely many worlds.")))

  ((test/llist/pcfg _)
   (⊦ equal? '(((V ("ants" "ants")) 0.6) ((V ("flies" "flies")) 0.399999999999999967))
      (probcc-reify/exact/a pcfg11))
   (⊦ equal? '(((V ("ants" "ants")) 0.18) ((V ("flies" "flies")) 0.12))
      (probcc-reify/exact/a pcfg12))
   (⊦ equal? '(((V ("ants" "ants" "ants" "ants")) 0.108)
               ((V ("flies" "flies" "ants" "ants")) 0.0720000000000000084)
               ((V ("ants" "ants" "flies" "flies")) 0.072)
               ((V ("flies" "flies" "flies" "flies")) 0.048))
      (probcc-reify/exact/a pcfg13))
   (⊦ equal? '(((V ("flies" "ants")) 0.072) ((V ("flies" "flies")) 0.048))
      (probcc-reify/exact/a pcfg))
   `(doc (p "The probabilistic context-free grammar of IBAL's Music TR (SL:288-478): noun
	    phrases of unbounded length, as lazy lists that are forced only as far as the
	    question needs; the tables are upstream's bit for bit (SL:387, 401, 451-454, 468).")))

  ((test/memo/functions _)
   (⊦ equal? '(((V (#t 2 2 3 3)) 1/32) ((V (#t 2 2 3 2)) 1/32) ((V (#t 2 2 2 3)) 1/32)
               ((V (#t 2 2 2 2)) 1/32) ((V (#t 2 1 3 3)) 1/32) ((V (#t 2 1 3 2)) 1/32)
               ((V (#t 2 1 2 3)) 1/32) ((V (#t 2 1 2 2)) 1/32) ((V (#t 1 2 3 3)) 1/32)
               ((V (#t 1 2 3 2)) 1/32) ((V (#t 1 2 2 3)) 1/32) ((V (#t 1 2 2 2)) 1/32)
               ((V (#t 1 1 3 3)) 1/32) ((V (#t 1 1 3 2)) 1/32) ((V (#t 1 1 2 3)) 1/32)
               ((V (#t 1 1 2 2)) 1/32) ((V (#f 3 3 2 2)) 1/32) ((V (#f 3 3 2 1)) 1/32)
               ((V (#f 3 3 1 2)) 1/32) ((V (#f 3 3 1 1)) 1/32) ((V (#f 3 2 2 2)) 1/32)
               ((V (#f 3 2 2 1)) 1/32) ((V (#f 3 2 1 2)) 1/32) ((V (#f 3 2 1 1)) 1/32)
               ((V (#f 2 3 2 2)) 1/32) ((V (#f 2 3 2 1)) 1/32) ((V (#f 2 3 1 2)) 1/32)
               ((V (#f 2 3 1 1)) 1/32) ((V (#f 2 2 2 2)) 1/32) ((V (#f 2 2 2 1)) 1/32)
               ((V (#f 2 2 1 2)) 1/32) ((V (#f 2 2 1 1)) 1/32))
      (probcc-reify/exact (test1m)))
   ; cf. SL:742-763: the memoized grass-is-wet of a day is the same at each call; the
   ; operands of `=` from the right (D19).
   (⊦ equal? '(((V #t) 1.))
      (probcc-reify/exact/a (grass-model (λ (grass-is-wet rain)
                                           (let* ((b (grass-is-wet 2))
                                                  (a (grass-is-wet 2)))
                                             (equal? a b))))))
   ; cf. SL:765-778: did it rain on day 2, given that the grass is wet that day?  Bit for bit:
   ; upstream's pattern matches the floats 0.2838 and 0.322 exactly.
   (⊦ equal? '(((V #f) 0.322) ((V #t) 0.2838))
      (probcc-reify/exact/a (grass-model (λ (grass-is-wet rain)
                                           (probcc-observe (λ (_) (grass-is-wet 2))
                                                           (τ (rain 2)))))))
   `(doc (p "Without memoization, two applications of a stochastic function to the same
	    argument may give different results in the same world (" (code/inline "test1m")
            ", SL:661-692); " (code/inline "test2m") " and " (code/inline "test3m")
            " of " (code/inline "hansei.scm") " show " (code/inline "probcc-memo")
            ". The sprinkler of SL:731-778 memoizes rain, sprinkler and wet grass as functions
	    of the day: the grass of a day is wet or not once in a world, and given that it is
	    wet on day 2, it rained with the weights 0.2838 and 0.322, those of the lawn in the suite hansei.")))

  ((test/lazy/delayed _)
   (⊦ equal? '((((V (1 1)) 1/2) ((V (2 2)) 1/4)) 4)
      (with-calls (τ (probcc-reify/exact/a testd1))))
   (⊦ equal? '(((V 2) 1/2) ((V 1) 1/2)) (probcc-reify/exact/a testd2))
   `(doc (p "Lazy versus delayed evaluation (SL:782-860): an expression whose value is not
	    used is not evaluated, so its failure is not seen; lazy evaluation is sound only for
	    expressions that cannot fail, or when only the ratios of the final probabilities
	    matter. In " (code/inline "testd1") " the lazy " (code/inline "x")
            " fails in the worlds where it is forced, in " (code/inline "testd2")
            " it is never forced.")))

  ((test/split-at _)
   (⊦ equal? '(() (1 2 3)) (probcc-split-at 0 '(1 2 3)))
   (⊦ equal? '((1) (2 3)) (probcc-split-at 1 '(1 2 3)))
   (⊦ equal? '((2 1) (3)) (probcc-split-at 2 '(1 2 3)))
   (⊦ equal? '((3 2 1) ()) (probcc-split-at 3 '(1 2 3)))
   (⊦ equal? '((3 2 1) ()) (probcc-split-at 5 '(1 2 3)))
   (⊦ equal? '(() ()) (probcc-split-at 2 '()))
   `(doc (p (code/inline "probcc-split-at") " (cf. LP:139-153, LPI:32-37) answers the zipper of
	    a list at an element, the elements before it reversed; a list that is too short gives
	    an empty tail.")))

  ((test/uncons _)
   (⊦ equal? '(((V (3 (1 2))) 1/3) ((V (2 (1 3))) 1/3) ((V (1 (2 3))) 1/3))
      (probcc-reify/exact (probcc-uniform/either/uncons '(1 2 3))))
   (⊦ equal? '(((V (a ())) 1)) (probcc-reify/exact (probcc-uniform/either/uncons '(a))))
   (⊦ equal? #t (match/first (probcc-reify/0 (τ (probcc-uniform/either/uncons '(a))))
                  ((((V (a ())) 1)) #t)
                  (else #f)))
   (⊦ equal? '() (probcc-reify/exact (probcc-uniform/either/uncons '())))
   `(doc (p (code/inline "probcc-uniform/either/uncons") " (cf. LP:155-172) chooses an element
	    uniformly and answers it with the others; a one-element list makes no choice, and the
	    empty list fails.")))

  ((test/insert-shift _)
   (⊦ equal? '(0 (0)) (probcc-insert-shift 0 '()))
   (⊦ equal? '(1 (0 1)) (probcc-insert-shift 0 '(0)))
   (⊦ equal? '(0 (0 1)) (probcc-insert-shift 0 '(1)))
   (⊦ equal? '(2 (1 2)) (probcc-insert-shift 1 '(1)))
   (⊦ equal? '(3 (1 3)) (probcc-insert-shift 2 '(1)))
   (⊦ equal? '(2 (0 1 2)) (probcc-insert-shift 0 '(0 1)))
   (⊦ equal? '(0 (0 1 2)) (probcc-insert-shift 0 '(1 2)))
   (⊦ equal? '(3 (1 2 3)) (probcc-insert-shift 1 '(1 2)))
   `(doc (p (code/inline "probcc-insert-shift") " (cf. LP:233-264) inserts into a sorted list
	    the integer j given as j minus the number of smaller elements.")))

  ((test/correlated _)
   (⊦ equal? (map (λ (l) (list (list 'V l) 1/16))
                  '((1 1 1 1) (1 1 1 0) (1 1 0 1) (1 1 0 0) (1 0 1 1) (1 0 1 0) (1 0 0 1)
                    (1 0 0 0) (0 1 1 1) (0 1 1 0) (0 1 0 1) (0 1 0 0) (0 0 1 1) (0 0 1 0)
                    (0 0 0 1) (0 0 0 0)))
      (probcc-reify/exact (tuple-0101 f01)))
   (⊦ equal? '(((V (1 1 1 1)) 1/4) ((V (1 0 1 0)) 1/4) ((V (0 1 0 1)) 1/4) ((V (0 0 0 0)) 1/4))
      (probcc-reify/exact (tuple-0101 (probcc-memo f01))))
   ; `f2 0 == f2 1`: the operands of == from the right (D19).
   (⊦ equal? '(((V (1 0 1 0)) 1/4) ((V (0 1 0 1)) 1/4))
      (probcc-reify/exact (let* ((f2 (probcc-memo f01))
                                 (f3 (λ (x)
                                       (let* ((b (f2 1))
                                              (a (f2 0)))
                                         (when (eqv? a b) (probcc-impossible))
                                         (f2 x)))))
                            (tuple-0101 f3))))
   ; `List.nth (perms ())`: the permutation is forced when f4 is defined.
   (⊦ equal? '(((V (1 0 1 0)) 1/2) ((V (0 1 0 1)) 1/2))
      (probcc-reify/exact (let* ((perms (probcc-letlazy (τ (probcc-uniform/either '((0 1) (1 0))))))
                                 (p (perms)))
                            (tuple-0101 (λ (i) (list-ref p i))))))
   `(doc (p "Stochastic functions (LP:17-107, H " (code/inline "#all-different") "): "
            (code/inline "f1") " is not a function, the memoized " (code/inline "f2")
            " is; " (code/inline "f3") " rejects the worlds where " (code/inline "f2")
            " is not injective, wasting half of the choice space; " (code/inline "f4")
            " draws a whole permutation lazily.")))

  ((test/lazy-permutation _)
   (define raw (map (λ (l) (list (list 'V l) 1/24)) permutations-of-4))
   (define (f5-0101 perms) (tuple-0101 (λ (n) ((list-ref perms n)))))
   (⊦ equal? raw
      (probcc-reify/exact (probcc-map (λ (th) (th)) (probcc-lazy-permutation '(1 2 3 4)))))
   (⊦ equal? (list 1 (map (λ (e) (list (car e) (exact->inexact 1/24))) raw))
      (probcc-normalize/total
        (probcc-reify/exact (probcc-map (λ (th) (th)) (probcc-lazy-permutation '(1 2 3 4))))))
   (⊦ equal? '(((V (3 2 3 2)) 1/6) ((V (3 1 3 1)) 1/6) ((V (2 3 2 3)) 1/6) ((V (2 1 2 1)) 1/6)
               ((V (1 3 1 3)) 1/6) ((V (1 2 1 2)) 1/6))
      (probcc-reify/exact (f5-0101 (probcc-lazy-permutation '(1 2 3)))))
   ; f5 0 alone makes 3 choices, not 3! = 6.
   (⊦ equal? 3 (probcc-leaves (probcc-reify/0 (τ ((car (probcc-lazy-permutation '(1 2 3))))))))
   (⊦ equal? '() (probcc-lazy-permutation '()))
   (⊦ equal? '(((V (a)) 1))
      (probcc-reify/exact (probcc-map (λ (th) (th)) (probcc-lazy-permutation '(a)))))
   `(doc (p (code/inline "probcc-lazy-permutation") " (cf. LP:127-229) is a list of correlated
	    random variables, each choosing among the elements its predecessors left: the 24
	    permutations of four elements have the exact mass 1/24 (upstream's floats sum to
	    0.999999999999999667, LP:190), and the lazy " (code/inline "f5")
            " is an injective function that wastes no choice space.")))

  ((test/lazy-permutation/ins _)
   (define (f5-0101 perms) (tuple-0101 (λ (n) ((list-ref perms n)))))
   (⊦ equal? (map (λ (l) (list (list 'V (map sub1 l)) 1/24)) permutations-of-4)
      (probcc-reify/exact (probcc-map (λ (th) (th)) (probcc-lazy-permutation/ins 4))))
   (⊦ equal? '(((V (2 1 2 1)) 1/6) ((V (2 0 2 0)) 1/6) ((V (1 2 1 2)) 1/6) ((V (1 0 1 0)) 1/6)
               ((V (0 2 0 2)) 1/6) ((V (0 1 0 1)) 1/6))
      (probcc-reify/exact (f5-0101 (probcc-lazy-permutation/ins 3))))
   (⊦ equal? '() (probcc-lazy-permutation/ins 0))
   (⊦ equal? '(((V (0)) 1))
      (probcc-reify/exact (probcc-map (λ (th) (th)) (probcc-lazy-permutation/ins 1))))
   (⊦raises (exn hansei) (probcc-lazy-permutation/ins -1))
   (⊦raises (exn hansei) (probcc-lazy-permutation/ins 0.5))
   (⊦raises (exn hansei) (probcc-lazy-permutation/ins 2.5))
   `(doc (p (code/inline "probcc-lazy-permutation/ins") " (cf. LP:266-330) builds the same
	    sequences of all-different variables over 0 .. n-1 by insertion; a count that is
	    not a non-negative exact integer raises (deviation 33).")))

  ((test/sorted _)
   (⊦ equal? (map (λ (l) (list (list 'V l) 1/16))
                  '((1 1 1 1) (1 1 1 0) (1 1 0 1) (1 1 0 0) (1 0 1 1) (1 0 1 0) (1 0 0 1)
                    (1 0 0 0) (0 1 1 1) (0 1 1 0) (0 1 0 1) (0 1 0 0) (0 0 1 1) (0 0 1 0)
                    (0 0 0 1) (0 0 0 0)))
      (probcc-reify/exact (so-to-list (so-bit-list 4))))
   (⊦ equal? (map (λ (l) (list (list 'V (list l l)) 1/8))
                  '((1 1 1) (1 1 0) (1 0 1) (1 0 0) (0 1 1) (0 1 0) (0 0 1) (0 0 0)))
      (probcc-reify/exact (let* ((l (so-bit-list 3))
                                 (b (so-to-list l))
                                 (a (so-to-list l)))
                            (list a b))))
   (⊦ equal? '(((V #t) 5/8) ((V #f) 3/8))
      (probcc-reify/exact (let* ((l1 (so-bit-list 2)) (l2 (so-bit-list 2))) (so-gte l1 l2))))
   (⊦ equal? '(((V #t) 9/16) ((V #f) 7/16))
      (probcc-reify/exact (let* ((l1 (so-bit-list 3)) (l2 (so-bit-list 3))) (so-gte l1 l2))))
   (⊦ equal? '(((V #t) 5/8) ((V #f) 3/8))
      (probcc-reify/exact (so-is-sorted so-gte (so-num-list 2 2))))
   (⊦ equal? '(((V #t) 9/16) ((V #f) 7/16))
      (probcc-reify/exact (so-is-sorted so-gte (so-num-list 2 3))))
   (⊦ equal? '(((V #f) 49/64) ((V #t) 15/64))
      (probcc-reify/exact (so-is-sorted so-gte (so-num-list 3 3))))
   (⊦ equal? 15/64 (/ (so-sum 0 7 (λ (i) (so-sum i 7 (λ (j) (so-sum j 7 (λ (k) 1)))))) (* 64 8)))
   (⊦ equal? '(((V #f) 1883/2048) ((V #t) 165/2048))
      (probcc-reify/exact (so-is-sorted so-gte (so-num-list 4 3))))
   (⊦ equal? 165/2048
      (/ (so-sum 0 7 (λ (i0) (so-sum i0 7 (λ (i) (so-sum i 7 (λ (j) (so-sum j 7 (λ (k) 1))))))))
         (* 64 64)))
   ; cf. SO:164-171: four numbers of five bits, 2^20 worlds of which few are generated.
   (⊦ equal? '(((V #f) 124527/131072) ((V #t) 6545/131072))
      (probcc-reify/exact (so-is-sorted so-gte (so-num-list 4 5))))
   (⊦ equal? 6545/131072
      (/ (so-sum 0 31 (λ (i0) (so-sum i0 31 (λ (i) (so-sum i 31 (λ (j) (so-sum j 31 (λ (k) 1))))))))
         (* 32 32 32 32)))
   ; cf. SO:177-183: importance sampling of 100 and 200 worlds with (random_selector 17), as
   ; upstream computes them today under OCaml with the three-draw generator (0.05482177734375
   ; and 0.047857666015625); the records, 0.04451904296875 and 0.05048828125, came from
   ; another generator (D20).  The masses are dyadic, so exact here.
   (⊦ equal? '(((V #f) 77429/81920) ((V #t) 4491/81920))
      (probcc-sample-importance/a (probcc-random-selector 17) 100
                                  (τ (so-is-sorted so-gte (so-num-list 4 5)))))
   (⊦ equal? '(((V #f) 155999/163840) ((V #t) 7841/163840))
      (probcc-sample-importance/a (probcc-random-selector 17) 200
                                  (τ (so-is-sorted so-gte (so-num-list 4 5)))))
   ; cf. SO:185-200: the full model, lists of 2, 3 and 4 numbers of 20 bits, sampled with
   ; (random_selector 17) as upstream computes them today under OCaml with the three-draw
   ; generator (0.493205137252807624, 0.160950744608999219 and 0.0355790031822834849, the same
   ; doubles as these masses); the records, 0.507880630493164, 0.181672331919253333 and
   ; 0.0409626036065674284, came from another generator (D20).  The exact values are invfact n,
   ; 1/2, 1/6 and 1/24 (the statistical test of the suite hansei-long).
   (⊦ equal? `(((V ,(void)) 51716307/104857600))
      (probcc-sample-importance/a (probcc-random-selector 17) 100 (so-model 2)))
   (⊦ equal? `(((V ,(void)) 55302254749/343597383680))
      (probcc-sample-importance/a (probcc-random-selector 17) 100 (so-model 3)))
   (⊦ equal? `(((V ,(void)) 97798819259/2748779069440))
      (probcc-sample-importance/a (probcc-random-selector 17) 200 (so-model 4)))
   (⊦ equal? '(0.493205137252807624 0.160950744608999219 0.0355790031822834849)
      (probcc-map exact->inexact
                  (list 51716307/104857600 55302254749/343597383680 97798819259/2748779069440)))
   `(doc (p "Harik's sorted-list problem (SO:50-200): numbers are lazy lists of random bits,
	    so " (code/inline "gte") " forces only the bits up to the first difference; the
	    exact probabilities agree with the theory, " (code/inline "testp3_theory")
            " = 15/64, " (code/inline "testp4_theory") " = 165/2048 and "
            (code/inline "testp5_theory") " = 6545/131072 (SO:146-171), the last over 2^20
	    worlds of which only those that " (code/inline "gte") " looks at are generated.
	    Importance sampling with " (code/inline "(probcc-random-selector 17)")
            " gives what upstream gives under OCaml today, sample for sample, both for the
	    four numbers of five bits and for the full model of n numbers of 20 bits, whose
	    probability of being sorted is 1/n!.")))

  ((test/replay/slazy _)
   (define (rejection seed n thunk)
     (with-calls (τ (probcc-sample-rejection/a (probcc-random-selector seed) n thunk))))
   (define (importance seed n thunk)
     (with-calls (τ (probcc-sample-importance/a (probcc-random-selector seed) n thunk))))
   (⊦ equal? '((((V (2 2)) 0.4) ((V (4 4)) 0.2) ((V (3 3)) 0.2) ((V (6 6)) 0.1) ((V (1 1)) 0.1))
               10)
      (rejection 1 10 (testl1 0.5)))
   (⊦ equal? '((((V (2 2)) 0.5) ((V (1 1)) 0.2) ((V (6 6)) 0.1) ((V (4 4)) 0.1) ((V (3 3)) 0.1))
               4)
      (rejection 1 10 (testl2 0.5)))
   (⊦ equal? '((((V (2 #f 20 20)) 0.2) ((V (1 #f 100 100)) 0.2) ((V (1 #f 10 10)) 0.2)
                ((V (2 #t 200 200)) 0.1) ((V (2 #t 20 20)) 0.1) ((V (2 #f 200 200)) 0.1)
                ((V (1 #t 100 100)) 0.1))
               10)
      (rejection 1 10 (testl3 0.5)))
   (⊦ equal? '((((V (2 #t 200 200)) 0.125) ((V (2 #t 20 20)) 0.125) ((V (2 #f 200 200)) 0.125)
                ((V (2 #f 20 20)) 0.125) ((V (1 #t 100 100)) 0.125) ((V (1 #t 10 10)) 0.125)
                ((V (1 #f 100 100)) 0.125) ((V (1 #f 10 10)) 0.125))
               4)
      (importance 1 10 (testl3 0.5)))
   (⊦ equal? '((((V (2 #t 200 200)) 0.2) ((V (1 #t 100 100)) 0.2) ((V (1 #f 100 100)) 0.2)
                ((V (2 #t 20 20)) 0.1) ((V (2 #f 200 200)) 0.1) ((V (2 #f 20 20)) 0.1)
                ((V (1 #f 10 10)) 0.1))
               10)
      (rejection 1 10 (testl4 0.5)))
   (⊦ equal? '((((V (2 #t 200 200)) 0.125) ((V (2 #t 20 20)) 0.125) ((V (2 #f 200 200)) 0.125)
                ((V (2 #f 20 20)) 0.125) ((V (1 #t 100 100)) 0.125) ((V (1 #t 10 10)) 0.125)
                ((V (1 #f 100 100)) 0.125) ((V (1 #f 10 10)) 0.125))
               8)
      (importance 1 10 (testl4 0.5)))
   (⊦ equal? '(((V ("ants" "ants")) 0.18) ((V ("flies" "flies")) 0.12))
      (probcc-sample-importance/a (probcc-random-selector 1) 100 pcfg12))
   (⊦ equal? '(((V ("flies" "ants")) 0.0720000000000000501)
               ((V ("flies" "flies")) 0.0480000000000000357))
      (probcc-sample-importance/a (probcc-random-selector 1) 100 pcfg))
   (⊦ equal? `(((V ,(void)) 0.12))
      (probcc-sample-rejection/a (probcc-random-selector 17) 100 (full-10tree-obs ml-uniform 1)))
   (⊦ equal? '(((V 1) 0.000100000000000000032))
      (probcc-sample-importance/a (probcc-random-selector 17) 1 (full-10ltree-obs ml-uniform 4)))
   (⊦ equal? '(((V 1) 1/10000))
      (probcc-sample-importance/a (probcc-random-selector 17) 1
                                  (full-10ltree-obs probcc-uniform 4)))
   (⊦ equal? '()
      (probcc-sample-importance (probcc-random-selector 17) 2000
        (if (trues 20 (flips 0.5 20)) (void) (probcc-impossible))))
   (⊦ equal? `(((V ,(void)) ,(expt 2. -20)))
      (probcc-sample-importance (probcc-random-selector 17) 1
        (if (ltrues 20 (lflips 0.5 20)) (void) (probcc-impossible))))
   (⊦ equal? `(((V #t) ,(expt 2. -20)))
      (probcc-sample-importance (probcc-random-selector 17) 1
        (let1 (ts (lflips 0.5 20))
          (if (ltrues 20 ts) ((probcc-lhead (τ ts))) (probcc-impossible)))))
   `(doc (p "The sampling records of " (code/inline "slazy.ml") " (SL:125-211, 431-485, 530,
	    617-655), replayed bit for bit with " (code/inline "probcc-random-selector")
            ", with upstream's flonum masses (the digits through a transcription of upstream's "
            (code/inline "uniform") ", P:156-165, since " (code/inline "probcc-uniform")
            "'s are exact, deviation 7), and the calls of the expensive function counted as
	    upstream prints them: lazy variables are computed only in the sampled worlds that
	    need them. With lazy lists, importance sampling finds the exact probability of
	    all-ones digits and of 20 heads in one sample, where 2000 samples of the strict
	    lists find nothing (SL:617-655).")))

  )

; a resume that fails to rewind its `dynamic-wind` frames would leave a test's string port as
; the current output port, hiding the summary; check that the port is back.
(import (only (chicken base) exit))

(define stdout (current-output-port))

(let1 (r (unittest/✓ hansei-lazy))
  (unless (eq? stdout (current-output-port))
    (current-output-port stdout)
    (write (unittest/result-summary r))
    (newline)
    (display "FAIL: the suite ended with its output still captured by a test\n")
    (exit 1)))
