; The suite of HANSEI's example models over (aux hansei): the regression tests of exactInfM.ml,
; the papers' examples, colored balls, blood types, the hidden Markov model, noisy-or, motivic
; development, multitarget tracking and bounded-rational theory of mind.  Cases E1-E16 of the
; plan AUX-HANSEI-PLAN.md §8.4 (WP8), with the fast cases of §8.6 (WP9: the two planes of BN,
; the three moving planes of BL, the sampling replay of M1).  Expected values are upstream's,
; re-sorted in the Scheme order (descending mass, ties in descending pmap-compare key order, plan
; D5); a table whose tied values are ordered differently by OCaml's `compare` (variant
; declaration order) and by pmap-compare (symbol names, deviation 6) is compared as a set, after
; `canon`.

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
; OCaml evaluates the components of a tuple or a record, the arguments of a constructor (`::`
; too) and of a call, and the operands of `+` and `=`, from right to left, and `let x = e1 and
; y = e2 in` from left to right (D19): the ported models sequence them with let*, as their
; comments say.  Exact tables do not depend on that order; the bit-exact ones may, through the
; order in which masses are summed.

(import scheme
        (chicken base)
        (chicken flonum)
        (only (scheme base) vector-copy)
        (only (chicken sort) sort)
        (only (chicken port) with-output-to-string)
        (only srfi-1 fold iota)
        (aux base)
        (aux unittest)
        (aux hansei)
        (aux hansei pmap)
        (aux hansei lazy))

; replays and bit-exact checks print 17 digits: with the default 15, a one-ulp failure would print
; identical expected and got values (plan §8 conventions).
(flonum-print-precision 17)

;; -----------------------------------------------------------------------------------------
;; Helpers
;; -----------------------------------------------------------------------------------------

; (new) the sources of the definitions made with define/shown, newest first, for the docs (as
; test/hakaru10-exact.scm:32-41).
(define sources '())

; (new) (define/shown name definition): make the definition, and keep its source under name.
(define-syntax-rule (define/shown name definition)
  (begin definition (push! (cons 'name 'definition) sources)))

; (new) the sources of the definitions of the names, in order, one code/scheme element each
; (spliced with ,@ into a doc).
(define (shown . names) (map (λ (name) `(code/scheme ,(alist-ref name sources))) names))

; (new) ≈rel: are a and b equal up to the relative tolerance eps, numbers in the same positions
; of the same structure, everything else equal?; for masses far below the 1e-6 of ⊦=.
(define ((rel-approx? eps) a b)
  (cond
    ((and (real? a) (real? b)) (or (= a b) (<= (abs (- a b)) (* eps (max (abs a) (abs b))))))
    ((and (pair? a) (pair? b)) (and ((rel-approx? eps) (car a) (car b))
                                    ((rel-approx? eps) (cdr a) (cdr b))))
    (else (equal? a b))))

; (new) "set": the entries of a table, or of a list of maxima, sorted by their write string, so
; that two lists whose tied entries come in different orders compare equal (plan §8
; conventions).
(define (canon l)
  (let1 (key (λ (e) (with-output-to-string (τ (write e)))))
    (sort l (λ (a b) (string<? (key a) (key b))))))

; (new) the sum of the masses of a table, from its first entry to its last, as OCaml's
; `List.fold_left (fun z (p,_) -> z +. p) 0. r` (M1:178).
(define (total pv) (fold (λ (e z) (+ z (cadr e))) 0. pv))

; (new) the leaves of the tree of a thunk: upstream's "reify: done; n accepted" lines.
(define (accepted thunk) (probcc-leaves (probcc-reify/0 thunk)))

; (new) the table pv in upstream's own order, descending keys (PM `foldi`, I:28), not by mass
; (deviation 5): probcc-map-dist re-collates it so.
(define (upstream-order pv) (probcc-map-dist identity pv))

; (new) cf. P:267-268 `variable_elim`, whose reflected table is in upstream's order: the masses
; of a model that eliminates variables are then summed in upstream's order, so they are
; upstream's bit for bit; probcc-variable-elimination reflects the table sorted by mass.
(define ((variable-elimination/upstream f) . args)
  (probcc-reflect (upstream-order (probcc-reify/exact (apply f args)))))

;; -----------------------------------------------------------------------------------------
;; Models of exactInfM.ml (X), paper_examples.ml (PE), the DSL paper and the NIPS abstract
;; -----------------------------------------------------------------------------------------

; cf. X:15-17: one flip, shared.
(define (tflip2-shared)
  (let1 (v (probcc-coin 0.5))
    (and v v)))

; cf. X:23-24: two independent flips; `&&` does not flip the second when the first is false.
(define (tflip2-noshared) (and (probcc-coin 0.5) (probcc-coin 0.5)))

; cf. X:30-35: `let … and …` from the left, earthquake first (D19).
(define (talarm)
  (let* ((earthquake (probcc-coin 0.01))
         (burglary (probcc-coin 0.1)))
    (if earthquake
        (if burglary (probcc-coin 0.99) (probcc-coin 0.2))
        (if burglary (probcc-coin 0.98) (probcc-coin 0.01)))))

; cf. X:48-53 and DSL p.4 l.160-166, the lawn model: rain before sprinkler (D19).
(define/shown grass-model
  (define (grass-model)
    (let* ((rain (probcc-coin 0.3))
           (sprinkler (probcc-coin 0.5))
           (grass-is-wet (or (and (probcc-coin 0.9) rain)
                             (and (probcc-coin 0.8) sprinkler)
                             (probcc-coin 0.1))))
      (probcc-when grass-is-wet rain))))

; cf. PE:9-16, the extended lawn model: wet-roof is not used, but its flip splits the worlds.
(define (grass-model/cloudy)
  (let* ((cloudy (probcc-coin 0.5))
         (rain (probcc-coin (if cloudy 0.8 0.2)))
         (sprinkler (probcc-coin (if cloudy 0.1 0.5)))
         (wet-roof (and (probcc-coin 0.7) rain))
         (wet-grass (or (and (probcc-coin 0.9) rain) (and (probcc-coin 0.9) sprinkler))))
    (probcc-when wet-grass rain)))

; cf. NIPS l.24-27, the program (1): two fair coins, at least one of them true; the pair is a
; list (deviation 25).
(define (two-coins)
  (let* ((x (probcc-coin 1/2))
         (y (probcc-coin 1/2)))
    (probcc-when (or x y) (list x y))))

; cf. X:94-99 `tfooibl1`, IBAL's foo.ibl program 1.
(define (tfooibl1)
  (probcc-observe (λ1-match/first ((,_ ,z) (eq? z #t)))
                  (τ (let* ((x (probcc-coin 0.6))
                            (y x)
                            (z x))
                       (list y z)))))

; cf. X:118-123 `tfooibl2`, program 2: the argument pair of f from the right, so the choice of
; y comes before f's own (D19).
(define (tfooibl2)
  (let* ((x #\a)
         (f (λ1-match/first ((,x ,y) (if (probcc-distribution '((#t 0.2) (#f 0.8))) x y))))
         (y (probcc-distribution '((#\b 0.4) (#\c 0.6))))
         (a (f (list #\d y))))
    (list a #\e)))

; cf. X:140-147 `tfooibl3`, program 3; the method `mf` of the object is its body.
(define (tfooibl3)
  (probcc-observe (λ (x) (eq? x #t))
                  (τ (eqv? (if (probcc-distribution '((#t 0.1) (#f 0.9)))
                               (probcc-distribution '((#\a 0.7) (#\b 0.3)))
                               (probcc-distribution '((#\a 0.2) (#\b 0.8))))
                           #\a))))

; cf. X:171-184 `tiblt2`, IBAL's test2.ibl; an object with methods mz and mw is the list (z w).
(define (tiblt2)
  (let* ((x (probcc-observe (λ1-match/first ((,z ,_) (eqv? z #\a)))
                            (τ (probcc-distribution '(((#\a #\b) 0.01)
                                                      ((#\a #\c) 0.02)
                                                      ((#\d #\e) 0.97))))))
         (w (cadr x)))
    (cond
      ((eqv? w #\b) (probcc-distribution '((#t 0.9) (#f 0.1))))
      ((eqv? w #\c) (probcc-distribution '((#t 0.6) (#f 0.4))))
      (else (probcc-distribution '((#t 0.2) (#f 0.8)))))))

; cf. X:199-203 `tmusictr41`, of the Music TR, chapter 4: a choice of functions, then a call.
(define (tmusictr41)
  (let* ((f (τ (probcc-coin 0.1)))
         (g (τ (probcc-coin 0.3))))
    (probcc-observe (λ (x) (eq? x #t))
                    (τ ((probcc-distribution `((,f 0.8) (,g 0.2))))))))

; cf. X:219-225 `tmusictr42`: an object is the pair (p . q-thunk), since a method's body runs
; when the method is called, after the observation, as OCaml's `#mq`.
(define (tmusictr42)
  (let* ((f (τ (probcc-coin 0.01)))
         (r (probcc-observe (λ (r) (eqv? (car r) #\a))
                            (τ (if (f)
                                   (cons #\a (τ (probcc-coin 0.3)))
                                   (cons #\b (τ #t)))))))
    ((cdr r))))

; cf. X:248-252 `flips_and`: the and of n flips, the first flip first, so that a false one
; collapses the remaining worlds.
(define ((flips-and h n))
  (let loop ((n n))
    (if (= n 1)
        (probcc-coin h)
        (and (probcc-coin h) (loop (sub1 n))))))

; cf. X:305-312 `xor` and `flips_xor`: the arguments of xor from the right, the recursion
; before the flip (D19).
(define/shown flips-xor
  (define ((flips-xor n))
    (let loop ((n n))
      (if (= n 1)
          (probcc-coin 0.5)
          (let* ((r (loop (sub1 n)))
                 (x (probcc-coin 0.5)))
            (not (eq? x r)))))))

; cf. X:323-328 `flips_xor'`: the xor of the others reified and reflected, collapsed once.
(define/shown flips-xor*
  (define ((flips-xor* n))
    (let loop ((n n))
      (if (= n 1)
          (probcc-coin 0.5)
          (let* ((r (probcc-reflect (probcc-reify/exact (loop (sub1 n)))))
                 (x (probcc-coin 0.5)))
            (not (eq? x r)))))))

;; -----------------------------------------------------------------------------------------
;; Models of noisy-or/noisy_or.ml (NO)
;; -----------------------------------------------------------------------------------------

; cf. NO:35-41 and NO:189-193: the roots and edges (vf vt) of the small and of the large
; graph, the probabilities of NO:39-41 written in place.
(define no-roots '(0 4))
(define no-edges '((0 2) (0 1) (1 3) (2 3) (4 2) (4 3)))
(define no-roots/large '(0 1 2 3))
(define no-edges/large '((0 4) (0 5) (1 5) (1 6) (2 6) (3 7) (3 8) (8 13) (8 16) (12 13) (7 12)
                         (6 7) (5 6) (5 9) (4 10) (4 11) (9 10) (10 11) (11 18) (18 19) (11 19)
                         (17 19) (13 14) (14 15) (16 17) (13 16) (14 17) (17 18) (15 18) (9 12)
                         (9 15) (12 15)))

; cf. NO:80-82 `predecessors`: the map from a vertex to its predecessors, `insert_with (@)`, so
; that the predecessor of a later edge comes first.
(define (no-predecessors edges)
  (fold (λ (e m) (match/first e ((,vf ,vt) (pmap-insert-with append vt (list vf) m))))
        pmap-empty
        edges))

; cf. NO:50-57 `model_fwd`, the PRISM solution: the graph built in, from the roots forward;
; the pair (n0,n4) from the right (D19).
(define (no-model-fwd)
  (letrec ((noisy-or (λ (l)
                       (match/first l
                         (() #f)
                         ((,h . ,t) (or (if h (probcc-coin 0.8) (probcc-coin 0.1))
                                        (noisy-or t)))))))
    (let* ((n4 (probcc-coin 0.5))
           (n0 (probcc-coin 0.5))
           (n1 (noisy-or (list n0)))
           (n2 (noisy-or (list n0 n4)))
           (n3 (noisy-or (list n1 n2 n4))))
      n3)))

; cf. NO:108-112 `fixm`, the memoizing fixpoint combinator; Scheme's letrec allows the `let rec g
; = memo (fun v -> f g v)` that OCaml rejects (NO:99-106), so no reference cell is needed.
(define (fixm f)
  (letrec ((g (probcc-memo (λ (v) (f g v)))))
    g))

; cf. NO:122-137 `model`, backwards from the vertex v, the evidence a function answering #f
; (`None`) or the list of the observed value (`Some e`); the vertices shared by several paths are
; memoized with fixm.
(define (no-model/memo roots preds v evidence)
  ((fixm (λ (self v)
           (match/first (evidence v)
             ((,e) e)
             (#f (if (memv v roots)
                     (probcc-coin 0.5)
                     (let noisy-or ((ws (pmap-find v preds)))
                       (match/first ws
                         (() #f)
                         ((,w . ,ws*) (or (if (self w) (probcc-coin 0.8) (probcc-coin 0.1))
                                          (noisy-or ws*))))))))))
   v))

; cf. NO:148-164 = NO:199-214 `model`: the evidence is the memo table, a map from vertices to
; their values, threaded through the computation; `record` notes the value of v.
(define (no-model roots preds v evidence)
  (letrec ((loop (λ (evidence v)
                   (let1 (record (λ (evidence vl) (list vl (pmap-add v vl evidence))))
                     (cond
                       ((pmap-member? v evidence) (list (pmap-find v evidence) evidence))
                       ((memv v roots) (record evidence (probcc-coin 0.5)))
                       (else (let noisy-or ((ev evidence)
                                            (ws (pmap-find v preds)))
                               (match/first ws
                                 (() (record ev #f))
                                 ((,w . ,ws*)
                                  (match/first (loop ev w)
                                    ((,vlw ,ev*)
                                     (let1 (vl (if vlw (probcc-coin 0.8) (probcc-coin 0.1)))
                                       (if vl (record ev* vl) (noisy-or ev* ws*))))))))))))))
    (car (loop evidence v))))

;; -----------------------------------------------------------------------------------------
;; Models of bloodtype/bloodtype.ml (BT)
;; -----------------------------------------------------------------------------------------

; cf. BT:21-41: the father and mother relations, (parent child), and their inverses.
(define bt-father '((Uwe Kristian) (Uwe Annette) (Willhelm Uwe) (Alfred Marianne) (Kristian Linus)
                    (Kristian Mette) (Herbert Nadine)))
(define bt-mother '((Marianne Kristian) (Marianne Annette) (Anneliese Uwe) (Pauline Marianne)
                    (Margot Nadine) (Nadine Linus) (Nadine Mette)))
(define (bt-invert rel) (map (λ1-match/first ((,p1 ,p2) (list p2 p1))) rel))
(define bt-father-of (bt-invert bt-father))
(define bt-mother-of (bt-invert bt-mother))

; cf. BT:47-48 `lookup`: the value of key in the relation, #f for `None`.
(define (bt-lookup key rel)
  (let1 (e (assq key rel))
    (and e (cadr e))))

; cf. BT:71 `chrom`: a chromosome GA, GB or GO; `1. -. pa -. pb` from the left.
(define (bt-chrom pa pb) (probcc-distribution `((GA ,pa) (GB ,pb) (GO ,(- 1. pa pb)))))

; cf. BT:82-92 `pchrom`: the chromosome a father passes, given his genotype.
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

; cf. BT:95-105 `mchrom`, the mother's, the same table.
(define bt-mchrom bt-pchrom)

; cf. BT:108-109 `btype`.
(define (bt-btype pa pb pab)
  (probcc-distribution `((A ,pa) (B ,pb) (AB ,pab) (O ,(- 1. pa pb pab)))))

; cf. BT:114-124 `bloodtype`: the phenotype, a stochastic function of the genotype.
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

; cf. BT:232-250 `pchr` and `mchr`, with the constraints ctx = (pctx mctx) and variable
; elimination `ve` of the parents' chromosomes; the pair of the parent's chromosomes from the
; right, the mother's first (D19).  BT:77-78's founders choose with `chrom (0.3,0.3)`.
(define (bt-pchr ve ctx person)
  (or (bt-lookup person (car ctx))
      (let1 (father (bt-lookup person bt-father-of))
        (if father
            (let* ((m ((ve (λ (p) (bt-mchr ve ctx p))) father))
                   (p ((ve (λ (p) (bt-pchr ve ctx p))) father)))
              (bt-pchrom (list p m)))
            (bt-chrom 0.3 0.3)))))

(define (bt-mchr ve ctx person)
  (or (bt-lookup person (cadr ctx))
      (let1 (mother (bt-lookup person bt-mother-of))
        (if mother
            (let* ((m ((ve (λ (p) (bt-mchr ve ctx p))) mother))
                   (p ((ve (λ (p) (bt-pchr ve ctx p))) mother)))
              (bt-mchrom (list p m)))
            (bt-chrom 0.3 0.3)))))

; cf. BT:253 `bldtype` and BT:263-265: the blood type of person under the constraints ctx; the
; genotype from the right (D19).
(define ((bt-bldtype ve ctx person))
  (let* ((m (bt-mchr ve ctx person))
         (p (bt-pchr ve ctx person)))
    (bt-bloodtype (list p m))))

;; -----------------------------------------------------------------------------------------
;; Models of HMM/hmm.ml (HM)
;; -----------------------------------------------------------------------------------------

; cf. HM:50-70: the states `State 1` .. `State 8` are the integers 1 .. 8, and the transition
; table is indexed by `from_state`, i - 1.
(define hm-nstates 8)
(define hm-transition-prob
  (list->vector
    (map (λ (i)
           (let1 (st (add1 i))
             (cond
               ((= st 1) `((,st 0.7) (,(add1 st) 0.3)))
               ((= st hm-nstates) `((,st 0.7) (,(sub1 st) 0.3)))
               (else `((,st 0.4) (,(add1 st) 0.3) (,(sub1 st) 0.3))))))
         (iota hm-nstates))))

; cf. HM:76-77 `l_observation_prob`.
(define hm-l-observation-prob '#(1.0 0.85714 0.71428 0.57142 0.42857 0.28571 0.14285 0.0))

; cf. HM:80-81 `evolve`.
(define (hm-evolve st) (probcc-distribution (vector-ref hm-transition-prob (sub1 st))))

; cf. HM:83-86 `observe`: L or R.
(define (hm-observe st)
  (let1 (pl (vector-ref hm-l-observation-prob (sub1 st)))
    (cond
      ((= pl 0.0) 'R)
      ((= pl 1.0) 'L)
      (else (probcc-distribution `((L ,pl) (R ,(- 1. pl))))))))

; cf. HM:103-104: the evidence, L observed at time 5.
(define (hm-evidence st i)
  (when (= i 5)
    (unless (eq? (hm-observe st) 'L) (probcc-impossible))))

; cf. HM:191-194 `counters` and `add_counters`.
(define hm-counters (make-vector 12 0))
(define (hm-add-counters! i)
  (let1 (i* (if (>= i (vector-length hm-counters)) 0 i))
    (vector-set! hm-counters i* (add1 (vector-ref hm-counters i*)))))

; cf. HM:91-98 = HM:196-204 `do_evolve`, forwards, counting the calls of `iter`; the state is
; uniform at the start.
(define ((hm-query/forward n))
  (let loop ((i 1)
             (st (add1 (probcc-uniform hm-nstates))))
    (hm-add-counters! i)
    (cond
      ((> i n) st)
      (else (hm-evidence st i)
            (loop (add1 i) (hm-evolve st))))))

; cf. HM:225-236 `do_evolve`, backwards: the state at i from that at i - 1.
(define ((hm-query/backward n))
  (let iter ((i n))
    (let1 (new-st (if (= i 1) (add1 (probcc-uniform hm-nstates)) (hm-evolve (iter (sub1 i)))))
      (hm-evidence new-st i)
      (hm-add-counters! i)
      new-st)))

; cf. HM:251-262 `do_evolve`, backwards with variable elimination `ve` of the previous state.
(define ((hm-query/ve ve n))
  (letrec ((iter (λ (i)
                   (let1 (new-st (if (= i 1)
                                     (add1 (probcc-uniform hm-nstates))
                                     (hm-evolve ((ve iter) (sub1 i)))))
                     (hm-evidence new-st i)
                     (hm-add-counters! i)
                     new-st))))
    (iter n)))

; (new) the counters after the exact inference of thunk.
(define (hm-count thunk)
  (vector-fill! hm-counters 0)
  (probcc-reify/exact/a thunk)
  (vector-copy hm-counters))

;; -----------------------------------------------------------------------------------------
;; Models of blip/blip_naive.ml (BN): planes without laziness
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

; cf. BN:134-139 `blips_at_least_ideal`: the same without noise.
(define (bn-blips-at-least/ideal blips pstates)
  (let1 (check-blip (λ (blip) (fold (λ (ps acc) (or acc (equal? (cadr ps) blip))) #f pstates)))
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

;; -----------------------------------------------------------------------------------------
;; Models of music1a.ml (M1)
;; -----------------------------------------------------------------------------------------

; cf. M1:85-91 `splitAt`: the first n cells of the lazy list lst and the rest, (list x1 x2);
; deviation 25: the pair is a list.
(define (m1-split-at n lst)
  (if (= n 0)
      (list probcc-lnil lst)
      (match/first (lst)
        ((,h . ,t) (match/first (m1-split-at (sub1 n) t)
                     ((,x1 ,x2) (list (probcc-lcons h x1) x2)))))))

; cf. M1:93-96 `split`: at a uniformly chosen place.
(define (m1-split lst)
  (let1 (n (probcc-uniform (add1 (probcc-llength lst))))
    (m1-split-at n lst)))

; cf. M1:112-125 `transpose1`, the notes A .. Gsharp as symbols.
(define (m1-transpose1 n)
  (probcc-distribution
    (case n
      ((C) '((Csharp 0.3) (D 0.6) (Dsharp 0.1)))
      ((Csharp) '((D 0.4) (Dsharp 0.6)))
      ((D) '((Dsharp 0.3) (E 0.7)))
      ((Dsharp) '((E 0.7) (F 0.3)))
      ((E) '((F 0.6) (Fsharp 0.4)))
      ((F) '((Fsharp 0.3) (G 0.6) (Gsharp 0.1)))
      ((Fsharp) '((G 0.4) (Gsharp 0.6)))
      ((G) '((Gsharp 0.3) (A 0.6) (Asharp 0.1)))
      ((Gsharp) '((A 0.4) (Asharp 0.6)))
      ((A) '((Asharp 0.3) (B 0.7)))
      ((Asharp) '((B 0.7) (C 0.3)))
      ((B) '((C 0.6) (Csharp 0.4))))))

; cf. M1:128-143 `transpose5`.
(define (m1-transpose5 n)
  (probcc-distribution
    (case n
      ((C) '((F 0.3) (Fsharp 0.1) (G 0.55) (Gsharp 0.05)))
      ((Csharp) '((Fsharp 0.3) (G 0.4) (Gsharp 0.3)))
      ((D) '((G 0.3) (Gsharp 0.1) (A 0.55) (Asharp 0.05)))
      ((Dsharp) '((Gsharp 0.3) (A 0.4) (Asharp 0.3)))
      ((E) '((A 0.3) (Asharp 0.1) (B 0.55) (C 0.05)))
      ((F) '((Asharp 0.1) (B 0.2) (C 0.6) (Csharp 0.1)))
      ((Fsharp) '((B 0.3) (C 0.4) (Csharp 0.3)))
      ((G) '((C 0.3) (Csharp 0.1) (D 0.55) (Dsharp 0.05)))
      ((Gsharp) '((Csharp 0.3) (D 0.4) (Dsharp 0.3)))
      ((A) '((D 0.3) (Dsharp 0.1) (E 0.55) (F 0.05)))
      ((Asharp) '((Dsharp 0.3) (E 0.3) (F 0.4)))
      ((B) '((E 0.3) (F 0.3) (Fsharp 0.3) (G 0.1))))))

; cf. M1:151-154: an operation on a lazy list, chosen.
(define (m1-choose-op)
  (probcc-distribution `((,identity 0.5)
                         (,(λ (_) probcc-lnil) 0.2)
                         (,(λ (l) (probcc-lmap m1-transpose1 l)) 0.2)
                         (,(λ (l) (probcc-lmap m1-transpose5 l)) 0.1))))

; cf. M1:147-161 `transform`: split the input in two, an operation on each part; the arguments
; of lappend from the right (D19).
(define (m1-transform x)
  (match/first (x)
    (() probcc-lnil)
    (,input (let* ((f1 (m1-choose-op))
                   (f2 (m1-choose-op)))
              (match/first (m1-split (τ input))
                ((,z1 ,z2) (let* ((b (f2 z2))
                                  (a (f1 z1)))
                             (probcc-lappend a b))))))))

; cf. M1:164-169 `main`: the evidence that the result has three notes, the first Asharp; the
; second note.
(define (m1-main)
  (let* ((input (probcc-list->llist '(A B C)))
         (x (m1-transform input)))
    (unless (and (= (probcc-llength x) 3) (eq? (probcc-lreflect (probcc-lhead x)) 'Asharp))
      (probcc-impossible))
    (probcc-lreflect (probcc-lhead (probcc-ltail x)))))

;; -----------------------------------------------------------------------------------------
;; Models of blip/colored_balls.ml (CB)
;; -----------------------------------------------------------------------------------------

; cf. CB:22-28 `opposite_color` and `observed_color`: a color is read right with probability q,
; upstream's 0.8.
(define/shown cb-observed-color
  (define (cb-observed-color q c)
    (if (probcc-coin q)
        c
        (case c ((Blue) 'Green) ((Green) 'Blue)))))

; cf. CB:32-48 `nballs_prior` and `model_nballs`, the observations a list: `uniform_range 1 8`
; is (probcc-uniform/range 1 9), half open (deviation 8); every ball keeps its color, memoized.
(define/shown cb-model-nballs
  (define ((cb-model-nballs q obs))
    (let* ((nballs (probcc-uniform/range 1 9))
           (ball-color (probcc-memo (λ (b) (probcc-uniform/either '(Blue Green))))))
      (let loop ((obs obs))
        (unless (null? obs)
          (let1 (b (probcc-uniform/range 1 (add1 nballs)))
            (unless (eq? (cb-observed-color q (ball-color b)) (car obs)) (probcc-impossible)))
          (loop (cdr obs))))
      nballs)))

;; -----------------------------------------------------------------------------------------
;; Models of incite.ml (IN): bounded-rational theory of mind
;; -----------------------------------------------------------------------------------------

; The constructors of the variants statev and formv are symbols, an action `A n` is the list
; (A n), a pair (utility, x) is the list (u x) (deviation 25).  A state or a form is a function
; from a variable to a boolean.

; cf. IN:87-92 = IN:215-220 `utility`: -10 per piece of food bought too many or too few; the
; operands of `+` from the right (D19); `-10.0 *. 0.` is -0., as upstream's.
(define (in-utility state action)
  (match/first action
    ((A ,bought-food)
     (let* ((b1 (if (state 'S_b1) 2 0))
            (b0 (if (state 'S_b0) 1 0)))
       (* -10. (exact->inexact (abs (- bought-food (+ b0 b1)))))))))

; cf. IN:343-351 `utility`, asymmetric: buying too little costs ten times more.
(define (in-utility/asymmetric state action)
  (match/first action
    ((A ,bought-food)
     (let* ((b1 (if (state 'S_b1) 2 0))
            (b0 (if (state 'S_b0) 1 0))
            (diff (- bought-food (+ b0 b1))))
       (* -10. (exact->inexact (cond
                                 ((= diff 0) 0)
                                 ((> diff 0) diff)
                                 (else (* 10 (- diff))))))))))

; cf. IN:94-96 `language` of the two-bit game.
(define (in-language form state)
  (and (eq? (form 'F_b0) (state 'S_b0))
       (eq? (form 'F_b1) (state 'S_b1))))

; cf. IN:226-238 `language` of the some/all game: a conjunction of several form variables is
; understood only if S_conj holds, and each form variable constrains the headcount.
(define (in-language/some-all form state)
  (let1 (bit (λ (fv) (if (form fv) 1 0)))
    (and (if (> (+ (bit 'F_all) (bit 'F_some) (bit 'F_notall) (bit 'F_none)) 1) (state 'S_conj) #t)
         (let* ((b1 (if (state 'S_b1) 2 0))
                (b0 (if (state 'S_b0) 1 0))
                (people-came (+ b0 b1)))
           (and (if (form 'F_all) (= people-came 3) #t)
                (if (form 'F_some) (> people-came 0) #t)
                (if (form 'F_notall) (< people-came 3) #t)
                (if (form 'F_none) (= people-came 0) #t))))))

; cf. IN:101-102 `an_action`: planning as inference, every weight 1.
(define (in-an-action) (probcc-distribution '(((A 0) 1.) ((A 1) 1.) ((A 2) 1.) ((A 3) 1.))))

; cf. IN:109-116 `agent_A_model` of the two-bit game: the hearer, who interprets the form and
; buys food; the state variables are lazy.
(define ((in-agent-A-model form))
  (let* ((s0 (probcc-letlazy (τ (probcc-coin 0.5))))
         (s1 (probcc-letlazy (τ (probcc-coin 0.5))))
         (state (λ (sv) (case sv ((S_b0) (s0)) ((S_b1) (s1))))))
    (if (not (in-language form state))
        (probcc-impossible)
        (let* ((action (in-an-action))
               (u (in-utility state action)))
          (list u action)))))

; cf. IN:240-248 = IN:353-361 `agent_A_model` of the some/all game, over the utility of IN:215
; or IN:343.
(define (((in-agent-A-model/some-all utility) form))
  (let* ((s0 (probcc-letlazy (τ (probcc-coin 0.5))))
         (s1 (probcc-letlazy (τ (probcc-coin 0.5))))
         (sc (probcc-letlazy (τ (probcc-coin 0.5))))
         (state (λ (sv) (case sv ((S_b0) (s0)) ((S_b1) (s1)) ((S_conj) (sc))))))
    (if (not (in-language/some-all form state))
        (probcc-impossible)
        (let* ((action (in-an-action))
               (u (utility state action)))
          (list u action)))))

; cf. IN:147-156 `argmax`: the actions of the table `plans` of (u a) with the highest expected
; utility, as (u a) pairs, the expectations collated per action in a map (`(x1+.x2, y1+.y2)` of
; the new and the old pair) and folded in ascending order of the actions, so that the later of
; tied maxima comes first; '() for an empty table.
(define (in-argmax plans)
  (let1 (pl (fold (λ (e pm)
                    (match/first e
                      (((V (,u ,a)) ,p)
                       (pmap-insert-with (λ (new old)
                                           (match/first (list new old)
                                             (((,x1 ,y1) (,x2 ,y2)) (list (+ x1 x2) (+ y1 y2)))))
                                         a
                                         (list (* u p) p)
                                         pm))))
                  pmap-empty
                  plans))
    (pmap-foldi (λ (a up acc)
                  (match/first up
                    ((,u ,p) (let1 (u (/ u p))
                               (match/first acc
                                 (() (list (list u a)))
                                 (((,umax ,_) . ,_) (cond
                                                      ((> u umax) (list (list u a)))
                                                      ((= u umax) (cons (list u a) acc))
                                                      (else acc))))))))
                pl
                '())))

; cf. IN:160-161 `inner_model`: the hearer's best actions, by the inference procedure
; `inferencer`.
(define (in-inner-model inferencer agent-A form) (in-argmax (inferencer (agent-A form))))

; cf. IN:164-165 `agent_S_ideal_state`: two people come.
(define (in-agent-S-ideal-state sv) (eq? sv 'S_b1))

; cf. IN:167-179 = IN:292-303 = IN:392-403 `agent_S_model`: the speaker chooses the form
; variables to say among `forms`, each with weight 1, and predicts the hearer's action: one
; maximum is it, none is a uniform guess, several are equally likely.
(define ((in-agent-S-model forms agent-A utility inferencer))
  (let* ((formvs (probcc-distribution (probcc-map (λ (v) (list v 1.)) forms)))
         (form (λ (fv) (and (memq fv formvs) #t)))
         (action-A (match/first (in-inner-model inferencer agent-A form)
                     (((,_ ,x)) x)
                     (() (list 'A (probcc-uniform 4)))
                     (,lst (let1 (p (/ 1. (exact->inexact (length lst))))
                             (probcc-distribution
                               (probcc-map (λ1-match/first ((,_ ,x) (list x p))) lst))))))
         (u (utility in-agent-S-ideal-state action-A)))
    (list u formvs)))

; cf. IN:168-169: the forms of the two-bit game.
(define in-forms '((F_b1 F_b0) (F_b1) (F_b0) ()))

; cf. IN:279-290 `all_forms`: the sixteen subsets of the form variables of the some/all game,
; by the bits of 0 .. 15, in declaration order.
(define in-all-forms
  (map (λ (i)
         (append (if (odd? i) '(F_all) '())
                 (if (odd? (quotient i 2)) '(F_some) '())
                 (if (odd? (quotient i 4)) '(F_notall) '())
                 (if (odd? (quotient i 8)) '(F_none) '())))
       (iota 16)))

; (new) the inference procedures of the hearer: exact, and rejection or importance sampling
; with the dist_selector of nested inference.
(define in-exact probcc-reify/exact/a)
(define ((in-rejection n) m) (probcc-sample-rejection/a probcc-dist-selector n m))
(define ((in-importance n) m) (probcc-sample-importance/a probcc-dist-selector n m))

;; -----------------------------------------------------------------------------------------
;; Models of blip/blip_ailog.ml (BA): the AILog model, memoized
;; -----------------------------------------------------------------------------------------

; cf. BA:123-125 `one_of`: does f hold for one of n .. 1?
(define (ba-one-of f n)
  (and (> n 0) (or (f n) (ba-one-of f (sub1 n)))))

; cf. BA:127-178, the functor BLIPMODEL, instantiated inside the model (memo tables live in the
; world-local memory, P:466-471): (list pos anyblip1 anyblip), the memoized priors and dynamics
; of BA:62-93 keyed by the plane, or by (i t dir) (deviation 25: the tuple is a list).
; `uniformly [|0;…;9|]` is probcc-uniform/either, with exact masses (deviation 7).
(define (ba-blip-model)
  (let* ((xpos0 (probcc-memo (λ (i) (probcc-uniform/either '(0 1 2 3 4 5 6 7 8 9)))))
         (ypos0 (probcc-memo (λ (i) (probcc-uniform/either '(0 1 2 3 4 5 6 7 8 9)))))
         (dir0 (probcc-memo (λ (i) (blip-dir0))))
         (new-dir (probcc-memo (λ1-match/first ((,i ,t ,dir) (blip-new-dir dir)))))
         (xderiv (probcc-memo (λ1-match/first ((,i ,t ,dir) (blip-xderiv dir)))))
         (yderiv (probcc-memo (λ1-match/first ((,i ,t ,dir) (blip-yderiv dir)))))
         ; BA:136-139: the direction of the plane i at time t; the triple from the right.
         (direction (fixm (λ (self it)
                            (match/first it
                              ((,i ,t) (if (= t 0)
                                           (dir0 i)
                                           (let1 (d (self (list i (sub1 t))))
                                             (new-dir (list i (sub1 t) d)))))))))
         ; BA:142-156: the position of the plane i at time t.
         (pos (fixm (λ (self it)
                      (match/first it
                        ((,i ,t1)
                         (if (= t1 0)
                             (let* ((y (ypos0 i))
                                    (x (xpos0 i)))
                               (list x y))
                             (let* ((t (sub1 t1))
                                    (xy (self (list i t)))
                                    (dir (direction (list i t)))
                                    (x* (xderiv (list i t dir)))
                                    (y* (yderiv (list i t dir)))
                                    (newx (+ (car xy) x*)))
                               (when (or (< newx 0) (> newx 9)) (probcc-impossible))
                               (let1 (newy (+ (cadr xy) y*))
                                 (when (or (< newy 0) (> newy 9)) (probcc-impossible))
                                 (list newx newy)))))))))
         ; BA:163-165: a blip from the plane i, whose flip is not memoized.
         (blip-from (λ (i p t)
                      (let1 (p* (pos (list i t)))
                        (and (equal? p p*) (probcc-coin 0.9)))))
         ; BA:167-171: a blip at p at time t, from one plane or a false alarm.
         (anyblip1 (probcc-memo (λ1-match/first
                                  ((,p ,t) (or (probcc-coin 0.02)
                                               (ba-one-of (λ (i) (blip-from i p t)) 1))))))
         ; BA:173-177: the same for a lazy, geometric number of planes.
         (anyblip (let1 (nplanes (probcc-letlazy (τ (probcc-geometric/count 0.85))))
                    (probcc-memo (λ1-match/first ((,p ,t) (or (probcc-coin 0.02)
                                                              (ba-one-of (λ (i) (blip-from i p t))
                                                                         (nplanes)))))))))
    (list pos anyblip1 anyblip)))

;; -----------------------------------------------------------------------------------------
;; Models of blip/blip.ml (BL): lazy planes, the whole screen observed, sequential Monte-Carlo
;; -----------------------------------------------------------------------------------------

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

; cf. BL:184-192 `lazy_plane` and `lazy_state`: a strict system state (cnt planes) made lazy.
(define (bl-lazy-plane state)
  (match/first state
    ((,i (,x ,y) ,dir) (list i (list (τ x) (τ y)) (τ dir)))))

(define (bl-lazy-state sstate)
  (match/first sstate
    ((,cnt ,planes) (list cnt (probcc-map bl-lazy-plane planes)))))

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

; cf. BL:251-261: the parameters, no planes appearing or a geometric number of them, and the
; initial state.
(define bl-parm-no (list identity (τ 0)))
(define (bl-parm-n n) (list identity (τ (probcc-geometric/bounded n 0.85))))
(define bl-state0 '(0 ()))

; cf. BL:383-387 `two_blips_ideal`.
(define (bl-two-blips-ideal ps t)
  (case t
    ((0) (bl-blips-ideal '((3 3)) ps))
    ((1) (bl-blips-ideal '((3 4)) ps))
    (else (void))))

; cf. BL:432-436 `blip333_ideal`: three blips, each one cell further along x at each step.
(define (blip333-ideal ps t)
  (case t
    ((0) (bl-blips-ideal '((3 5) (3 7) (3 9)) ps))
    ((1) (bl-blips-ideal '((4 5) (4 7) (4 9)) ps))
    ((2) (bl-blips-ideal '((5 5) (5 7) (5 9)) ps))
    (else (void))))

;; -----------------------------------------------------------------------------------------
;; The suite
;; -----------------------------------------------------------------------------------------

(define-suite hansei-examples

  ((doc r)
   `((structure/section "Introductory examples and regression tests")
     (p "The example models of "
        (cite/a "https://okmij.org/ftp/kakuritu/Hansei.html#examples-tests" "Embedded probabilistic domain-specific language HANSEI")
        ", one section per example of the page, with upstream's own expected values: the lawn
	model of the papers, the regression tests of " (code/inline "exactInfM.ml") ", which
	liken HANSEI's syntax to IBAL's, and the early collapse of worlds:"
        (cite/quote
          "Oleg Kiselyov"
          "Naively, we would have thought we needed to evaluate 2^10 of possible worlds: each
	  (nested) flip splits the world. But as the result shows, we managed with only 10+1
	  worlds. The reason is that the algorithm is NOT tail recursive, and && is a _special_
	  form rather than a function."))
     (p "The models keep upstream's flonum probabilities, so that their tables are upstream's
	bit for bit where the masses are summed in upstream's order; where a model draws with "
        (code/inline "uniform") ", whose masses are the exact " (code/inline "1/n")
        " here (deviation 7), or reflects a table that " (code/inline "probcc-explore")
        " has sorted by mass (deviation 5), the masses agree to a relative 1e-9.")
     (structure/section "Colored balls")
     (p (cite/a "https://okmij.org/ftp/kakuritu/Hansei.html#colored-balls" "Example: colored balls") ":"
        (cite/quote
          "Oleg Kiselyov"
          "This is an elegant example of a model with indefinite number of objects whose `real'
	  identity is unobservable. The example is a simplified version of population
	  estimation. HANSEI's implementation of the example makes the case for memoization of
	  stochastic functions."))
     (structure/section "Blood types")
     (p (cite/a "https://okmij.org/ftp/kakuritu/Hansei.html#blood-type" "Example: blood types") ":"
        (cite/quote
          "Oleg Kiselyov"
          "The hidden variables are the presence of genes A and B in the chromosomes inherited
	  from the mother and the father. The observed variable is the blood type, which is a
	  stochastic function of the genotype. A typical query asks for the distribution of
	  blood types for a particular individual, possibly conditioned on the known genotype of
	  some of the ancestors."))
     (structure/section "Hidden Markov Model")
     (p (cite/a "https://okmij.org/ftp/kakuritu/Hansei.html#hmm" "Example: Hidden Markov Model (HMM)") ":"
        (cite/quote
          "Oleg Kiselyov"
          "A simple HMM model with a single hidden 8-state variable. Our implementation
	  demonstrates the investigation of time complexity of the inference -- in an
	  interactive session at the OCaml top level -- and changing the model by introducing
	  variable elimination, which speeds up inference exponentially."))
     (structure/section "Noisy OR")
     (p (cite/a "https://okmij.org/ftp/kakuritu/Hansei.html#noisy-or" "Example: Noisy OR") ":"
        (cite/quote
          "Oleg Kiselyov"
          "Although the graph does not have directed cycles, it does have many undirected ones.
	  As we work backwards, we may encounter a given node several times. Each time the value
	  of node's random attribute must be the same. We have to use memoization."))
     (structure/section "Motivic development in classical music")
     (p (cite/a "https://okmij.org/ftp/kakuritu/Hansei.html#music" "Realistic Example: Motivic development in classical music")
        ", the warm-up of " (code/inline "music1a.ml") ":"
        (cite/quote
          "Oleg Kiselyov"
          "Crucially this simplified model does not recursively subdivide and transform the
	  sequence. Therefore, the state space remains sufficiently small even for exact
	  inference. The lazy evaluation was essential."))
     (structure/section "Multitarget tracking")
     (p (cite/a "https://okmij.org/ftp/kakuritu/Hansei.html#blip" "Realistic Example: Multitarget tracking") ":"
        (cite/quote
          "Oleg Kiselyov"
          "We found it crucial to sample the state of an aircraft and the location of a false
	  alarm lazily. If the x-coordinate of the detected aircraft does not match the
	  x-coordinate of any observed blip, we immediately report the contradiction (of the
	  possible world with the evidence). We do not need to compute the y-coordinate of the
	  aircraft, let alone the locations of other aircrafts and false alarms."))
     (p "The ported models are the AILog one (" (code/inline "blip_ailog.ml")
        ", memoized), the naive one without laziness (" (code/inline "blip_naive.ml")
        ") and the lazy one with sequential Monte-Carlo (" (code/inline "blip.ml") ").")
     (structure/section "Bounded-rational theory of mind")
     (p (cite/a "https://okmij.org/ftp/kakuritu/Hansei.html#bounded-rationality" "Bounded-Rational Theory of Mind for Conversational Implicature") ":"
        (cite/quote
          "Chung-chieh Shan"
          "The payoff is that the same cognitive machinery that the hearer uses to decide what
	  to do, by interpreting utterances, can also be used by the speaker to decide what to
	  say, by predicting how potential utterances would be interpreted or misinterpreted by
	  the hearer."))
     (p "The speaker's model reifies the hearer's, exactly or by sampling, inside its own
	worlds (" (code/inline "incite.ml") "); the weights of the plans are 1, so the masses
	of the tables are expected utilities, not probabilities.")))

  ((test/paper/grass-cloudy _)
   (define table (probcc-reify/exact/a grass-model/cloudy))
   (⊦ equal? '(((V #t) 0.4581) ((V #f) 0.188999999999999974)) table)
   (⊦ equal? '(((V #t) 0.707927677329624472) ((V #f) 0.292072322670375473))
      (probcc-normalize table))
   `(doc (p "The extended lawn model of PE:9-28: whether it rained, given that the grass is wet,
	    with a cloudy sky as the common cause of rain and sprinkler; the flip of the wet
	    roof, whose value is not used, splits the worlds all the same.  The table and its
	    normalization are upstream's bit for bit.")))

  ((test/exact/basic _)
   (⊦ equal? '(((V #t) 0.5) ((V #f) 0.5)) (probcc-reify/exact/a tflip2-shared))
   (⊦ equal? 2 (accepted tflip2-shared))
   (⊦ equal? '(((V #f) 0.75) ((V #t) 0.25)) (probcc-reify/exact/a tflip2-noshared))
   (⊦ equal? 3 (accepted tflip2-noshared))
   (⊦ equal? '(((V #f) 0.891280000000000072) ((V #t) 0.108720000000000011))
      (probcc-reify/exact/a talarm))
   (⊦ equal? 8 (accepted talarm))
   (⊦ equal? '(((V #f) 0.321999999999999897) ((V #t) 0.2838)) (probcc-reify/exact/a grass-model))
   (⊦ equal? 14 (accepted grass-model))
   (⊦ equal? '(((V 8) 1/8) ((V 7) 1/8) ((V 6) 1/8) ((V 5) 1/8) ((V 4) 1/8) ((V 3) 1/8) ((V 2) 1/8)
               ((V 1) 1/8))
      (probcc-reify/exact (probcc-uniform/range 1 9)))
   `(doc (p "The basic tests of X:13-63: a shared flip and two independent ones, the alarm
	    network (whose " (code/inline "let … and …") " draws the earthquake first, D19),
	    the lawn model and " (code/inline "uniform_range 1 8") ", which is "
            (code/inline "(probcc-uniform/range 1 9)") " (deviation 8).  The tables are
	    upstream's bit for bit, and the number of leaves of each tree is the number of
	    worlds upstream's comments report as accepted.")))

  ((test/exact/ibal _)
   (⊦ equal? '(((V (#t #t)) 0.6)) (probcc-reify/exact/a tfooibl1))
   (⊦ equal? '(((V (#\c #\e)) 0.48) ((V (#\b #\e)) 0.320000000000000062) ((V (#\d #\e)) 0.2))
      (probcc-reify/exact/a tfooibl2))
   (⊦ equal? '(((V #t) 0.25)) (probcc-reify/exact/a tfooibl3))
   (⊦ equal? '(((V #t) 0.021) ((V #f) 0.00900000000000000105)) (probcc-reify/exact/a tiblt2))
   (⊦ equal? '(((V #t) 0.14)) (probcc-reify/exact/a tmusictr41))
   (⊦ equal? '(((V #f) 0.00699999999999999928) ((V #t) 0.003)) (probcc-reify/exact/a tmusictr42))
   `(doc (p "The tests of IBAL (X:81-231), whose syntax HANSEI likens: observations through "
            (code/inline "probcc-observe") ", records and objects as lists, the methods of an
	    object run when they are called; upstream's tables bit for bit.")))

  ((test/exact/flips _)
   (⊦ equal? '(((V #f) 1023/1024) ((V #t) 1/1024)) (probcc-reify/exact/a (flips-and 1/2 10)))
   (⊦ equal? '(((V #f) 0.9990234375) ((V #t) 0.0009765625))
      (probcc-reify/exact/a (flips-and 0.5 10)))
   (⊦ equal? 11 (accepted (flips-and 1/2 10)))
   (⊦ equal? '(((V #t) 0.5) ((V #f) 0.5)) (probcc-reify/exact/a (flips-xor 10)))
   (⊦ equal? 1024 (accepted (flips-xor 10)))
   (⊦ equal? '(((V #t) 0.5) ((V #f) 0.5)) (probcc-reify/exact/a (flips-xor* 10)))
   (⊦ equal? 4 (accepted (flips-xor* 10)))
   `(doc (p "Early collapse of worlds (X:236-347): the and of ten flips needs 11 worlds, not
	    2^10, because " (code/inline "and") " does not evaluate its second operand when the
	    first is false; the xor of ten flips needs all 1024, unless the xor of the others is
	    reified and reflected, collapsing its worlds into two, so that each level explores
	    4.")
         ,@(shown 'flips-xor 'flips-xor*)))

  ((test/paper/lawn _)
   (define normalized (probcc-normalize (probcc-reify/exact/a grass-model)))
   ; the paper's masses (DSL p.5 l.196-197), printed by an earlier version, and, bit for bit,
   ; those of today's upstream under OCaml 4.14.2, `normalize (exact_reify grass_model)`.
   (⊦= '(((V #f) 0.53152855727963022) ((V #t) 0.468471442720369724)) normalized)
   (⊦ equal? '(((V #f) 0.53152855727963011) ((V #t) 0.46847144272036984)) normalized)
   (⊦ equal? '(((V (#t #t)) 1/4) ((V (#t #f)) 1/4) ((V (#f #t)) 1/4))
      (probcc-reify/exact/a two-coins))
   `(doc (p "The lawn model of the DSL paper (DSL p.4 l.160-166), normalized as on p.5
	    l.194-197, whose 14 accepted worlds are those of " (code/inline "test/exact/basic")
            "; the paper's normalized masses were printed by an earlier version and differ
	    from today's upstream, which this port gives bit for bit, in the last two digits.
	    The two coins of the NIPS abstract (NIPS l.24-27, 63), at least one of them true,
	    with exact halves.")
         ,@(shown 'grass-model)))

  ((test/noisy-or _)
   (define preds (no-predecessors no-edges))
   (define on3 '(((V #t) 0.81441875000000008) ((V #f) 0.185581250000000031)))
   (⊦ equal? '(4 2 1) (pmap-find 3 preds))
   (⊦ equal? on3 (probcc-reify/exact/a no-model-fwd))
   (⊦ equal? on3 (probcc-reify/exact (no-model/memo no-roots preds 3 (λ (_) #f))))
   (⊦ equal? on3 (probcc-reify/exact (no-model no-roots preds 3 pmap-empty)))
   (⊦ equal? '(((V #t) 0.6864675) ((V #f) 0.313532500000000047))
      (probcc-reify/exact (no-model no-roots preds 3 (pmap-add 0 #f pmap-empty))))
   (⊦ equal? '(((V #t) 0.901446179820137083) ((V #f) 0.0985538201798756))
      (probcc-reify/exact (no-model no-roots/large (no-predecessors no-edges/large) 15 pmap-empty)))
   `(doc (p "Noisy-or networks (NO): the attribute of a vertex propagated from the roots,
	    forwards with the graph built in, or backwards from the vertex asked, where a vertex
	    reached by several paths must have one value per world: memoized with "
            (code/inline "fixm") " over " (code/inline "probcc-memo") ", or with the evidence
	    threaded as the memo table, which also holds the observations (" (code/inline "tq2")
            ", vertex 0 off).  The large graph has 20 vertices, 0 to 19 (NO:187 says 19);
	    every table is upstream's bit for bit.")))

  ((test/bloodtype _)
   (define test1exact '(((V A) 0.32180904495702678) ((V B) 0.320852692775250137)
                        ((V AB) 0.197860551516677169) ((V O) 0.159477710751046636)))
   (define test2exact '(((V A) 0.438689967666247682) ((V AB) 0.236503769769331712)
                        ((V B) 0.213085457329678429) ((V O) 0.111720805234742399)))
   (define uwe '(((Uwe GA)) ((Uwe GA))))
   (⊦ equal? test1exact
      (probcc-reify/exact/a (bt-bldtype variable-elimination/upstream '(() ()) 'Linus)))
   (⊦ equal? test2exact (probcc-reify/exact/a (bt-bldtype variable-elimination/upstream uwe 'Linus)))
   (⊦ (rel-approx? 1e-9) test1exact
      (probcc-reify/exact/a (bt-bldtype probcc-variable-elimination '(() ()) 'Linus)))
   (⊦ (rel-approx? 1e-9) test2exact
      (probcc-reify/exact/a (bt-bldtype probcc-variable-elimination uwe 'Linus)))
   `(doc (p "Blood types in a pedigree (BT:224-275), exactly, with variable elimination of the
	    parents' chromosomes: unconditionally, and given that Uwe's chromosomes are both A.
	    With the reflected tables in upstream's order the masses are upstream's bit for
	    bit; " (code/inline "probcc-variable-elimination") " reflects them sorted by mass
	    (deviation 5), so they are summed in another order and agree to a relative
	    1e-9.")))

  ((test/hmm _)
   (define forward (probcc-reify/exact/a (hm-query/forward 10)))
   (define ve (probcc-reify/exact/a (hm-query/ve variable-elimination/upstream 10)))
   (define ve/sorted (probcc-reify/exact/a (hm-query/ve probcc-variable-elimination 10)))
   (⊦ equal? '(((V 1) 0.10542819045498758) ((V 2) 0.0985554834162256921)
               ((V 3) 0.086272791068737309) ((V 4) 0.0707684496649872291)
               ((V 5) 0.0542304468099861214) ((V 6) 0.0387262008512519254)
               ((V 7) 0.0264436471037524448) ((V 8) 0.0195710406300005982))
      forward)
   ; normalized in upstream's order, State 8 first, which is the order of the sum (I:300).
   (⊦ equal? '(((V 8) 0.0391423748278179676) ((V 7) 0.0528876908651939)
               ((V 6) 0.0774529825998843541) ((V 5) 0.108461707082790779)
               ((V 4) 0.141537960864701051) ((V 3) 0.172546876239070945)
               ((V 2) 0.197112445175818218) ((V 1) 0.210857962344722732))
      (probcc-normalize (upstream-order forward)))
   (⊦ equal? '(((V 1) 0.107651107924999989) ((V 2) 0.100241383024999992)
               ((V 3) 0.0872119927625000096) ((V 4) 0.0710519301875)
               ((V 5) 0.0539469325375000058) ((V 6) 0.0377869824625000073)
               ((V 7) 0.02475776035) ((V 8) 0.0173481607499999975))
      ve)
   (⊦ equal? '(((V 8) 0.0346965817243629279) ((V 7) 0.0495158920691905202)
               ((V 6) 0.0755745317339880096) ((V 5) 0.107894674285057141)
               ((V 4) 0.142104926161946199) ((V 3) 0.174425293714702861)
               ((V 2) 0.200484269682022598) ((V 1) 0.215303830628729681))
      (probcc-normalize (upstream-order ve)))
   (⊦ (rel-approx? 1e-9) ve ve/sorted)
   `(doc (p "The hidden Markov model of HM, a random walk on 8 states observed at time 5,
	    exactly at time 10: without variable elimination (HM:147-167) the table is
	    upstream's bit for bit (" (code/inline "State i") " is " (code/inline "i")
            "); with it (HM:274-296), the model runs backwards and the previous state is
	    reified and reflected at each step, which makes the inference linear in the
	    number of steps; bit for bit when the reflected tables are in upstream's order, to
	    a relative 1e-9 with " (code/inline "probcc-variable-elimination") ".  The
	    normalized tables are computed in upstream's order, the order of the sum.")))

  ((test/hmm/counters _)
   (⊦ equal? '#(0 8 22 62 176 502 0 0 0 0 0 0) (hm-count (hm-query/forward 4)))
   (⊦ equal? '#(0 8 22 62 176 0 0 0 0 0 0 0) (hm-count (hm-query/backward 4)))
   (⊦ equal? '#(0 8 22 22 22 0 0 0 0 0 0 0)
      (hm-count (hm-query/ve probcc-variable-elimination 4)))
   (⊦ equal? '#(0 8 22 22 22 20 20 22 22 22 22 0)
      (hm-count (hm-query/ve probcc-variable-elimination 10)))
   `(doc (p "The investigation of complexity of HM:170-279, by counting the calls of the
	    worker at each step: the forward and the backward model grow by a factor of 3 per
	    step; with variable elimination the counts stay flat.")))

  ((test/blip/naive _)
   (define ((evidence0 check blips) ps t) (when (= t 0) (check blips ps)))
   (define (unit _) (void))
   (⊦ equal? `(((V ,(void)) 1/100))
      (probcc-reify/exact (if (equal? (cadr (bn-plane-state-0 1)) '(3 3))
                              (void)
                              (probcc-impossible))))
   (⊦ equal? '(((V 0) 0.852878464818763282) ((V 1) 0.127931769722814503)
               ((V 2) 0.0191897654584221797))
      (probcc-reify/exact (probcc-geometric/bounded 2 0.85)))
   (⊦ (rel-approx? 1e-9) `(((V ,(void)) 0.0211504347826093099))
      (probcc-reify/exact/a (bn-model 1 0 (evidence0 bn-blips-at-least '((3 3))) unit)))
   (⊦ (rel-approx? 1e-9) `(((V ,(void)) 0.00130434782608695684))
      (probcc-reify/exact/a (bn-model 1 0 (evidence0 bn-blips-at-least/ideal '((3 3))) unit)))
   (⊦ equal? '()
      (probcc-reify/exact/a (bn-model 1 0 (evidence0 bn-blips-at-least/ideal '((3 3) (3 5))) unit)))
   ; cf. BN:212-217: two planes at (3,3) and (3,5), either way round; the list from the right.
   (⊦ equal? `(((V ,(void)) 1/5000))
      (probcc-reify/exact (let* ((p2 (bn-plane-state-0 2))
                                 (p1 (bn-plane-state-0 1)))
                            (bn-blips-at-least/ideal '((3 3) (3 5)) (list p1 p2)))))
   ; cf. BN:227-232: two blips from at most two planes, so from two.
   (⊦ (rel-approx? 1e-9) '(((V 2) 3.83795309168443282e-06))
      (probcc-reify/exact/a
        (bn-model 2 0 (evidence0 bn-blips-at-least/ideal '((3 3) (3 5))) length)))
   (⊦ (rel-approx? 1e-9) `(((V ,(void)) 0.00059257469565211177))
      (probcc-reify/exact/a (bn-model 1 1 bn-two-blips unit)))
   `(doc (p "Airplanes without laziness (BN:183-252): a plane at (3,3) has probability 1/100,
	    exact here; a blip at (3,3) is a false alarm or the detection of the one plane of
	    the bounded geometric prior (BN:192: "
            (code/inline "0.02 +. 0.98 *. 0.00130434782608695684 *. 0.9") "); one plane cannot
	    make two blips, two planes make them with probability 1/5000 (either may be at
	    either blip; upstream " (code/inline "0.000199999999999999901")
            "), and two blips come from two planes of the prior with "
            (code/inline "0.0191897654584221797 · 2/10000") "; and the two blips of a moving
	    plane, with variable elimination of the previous state.  Without laziness the two
	    planes are drawn whole, 160000 worlds, which is as far as exact inference goes
	    (BN:238-245).")))

  ((test/music _)
   (define table (probcc-reify/exact/a m1-main))
   ; the evidence is summed in upstream's order (M1:178).
   (define table/upstream (upstream-order table))
   (define evidence (total table/upstream))
   (⊦ equal? '(((V C) 0.0270000000000000032) ((V Csharp) 0.018) ((V B) 0.0075)
               ((V Fsharp) 0.000450000000000000042) ((V F) 0.000450000000000000042)
               ((V E) 0.000450000000000000042) ((V G) 0.000150000000000000041))
      table)
   (⊦ equal? 0.054 evidence)
   (⊦ equal? '(((V G) 0.00277777777777777875) ((V Fsharp) 0.00833333333333333495)
               ((V F) 0.00833333333333333495) ((V E) 0.00833333333333333495)
               ((V Csharp) 0.333333333333333315) ((V C) 0.500000000000000111)
               ((V B) 0.138888888888888895))
      (probcc-map (λ1-match/first ((,v ,p) (list v (/ p evidence)))) table/upstream))
   (⊦ equal? 29 (accepted m1-main))
   ; cf. M1:195-201: importance sampling of 1000 worlds, replayed bit for bit.  m1-split draws
   ; with probcc-uniform 4, whose exact 1/4 is upstream's dyadic 0.25, so no transcription of
   ; upstream's uniform is needed.
   (⊦ equal? '(((V C) 0.0269730000000001947) ((V Csharp) 0.0179820000000001541)
               ((V B) 0.00785700000000001848) ((V Fsharp) 0.000607499999999999859)
               ((V F) 0.000607499999999999859) ((V E) 0.000607499999999999859)
               ((V G) 0.000202499999999999962))
      (probcc-sample-importance/a (probcc-random-selector 17) 1000 m1-main))
   `(doc (p "Motivic development, the warm-up (M1:147-191): the motive A B C is split at a
	    random place and each part transformed by an operation chosen at random; given that
	    the result has three notes, the first A sharp, the distribution of the second note.
	    The notes are lazy lists of " (code/inline "(aux hansei lazy)")
            ", forced only as far as the evidence asks; the table, the evidence 0.054 and the
	    normalized table are upstream's bit for bit, from 29 accepted worlds, and so is
	    importance sampling of 1000 worlds with " (code/inline "(probcc-random-selector 17)")
            " (M1:195-201; \"1000 samples is quite good, actually\").")))

  ((test/colored-balls _)
   (⊦ equal? '(((V 1) 13/400) ((V 2) 77/3200) ((V 3) 17/800) ((V 4) 127/6400) ((V 5) 19/1000)
               ((V 6) 59/3200) ((V 7) 101/5600) ((V 8) 227/12800))
      (probcc-reify/exact/a (cb-model-nballs 4/5 '(Blue Blue Blue))))
   (⊦= '(((V 1) 0.0325) ((V 2) 0.0240625) ((V 3) 0.02125) ((V 4) 0.01984375) ((V 5) 0.019)
         ((V 6) 0.0184375) ((V 7) 0.018035714285714287) ((V 8) 0.017734375))
       (probcc-reify/exact/a (cb-model-nballs 0.8 '(Blue Blue Blue))))
   `(doc (p "Colored balls (CB:32-48), exactly, for three draws that all look blue: the number
	    of balls in the urn, 1 to 8 a priori, with the color of every ball memoized.  With
	    the exact 4/5 the table is the analytic one, "
            (code/inline "1/8 · 1/2^n · Σ_colorings (Σ_b q(color b) / n)^3")
            "; with upstream's 0.8 it agrees to 1e-6.  Upstream only samples this model; the
	    sampling of ten draws is planned for the long suite.")
         ,@(shown 'cb-observed-color 'cb-model-nballs)))

  ((test/incite/two-bit _)
   (define (ut1 fv) (eq? fv 'F_b1))
   (define (speaker inferencer) (in-agent-S-model in-forms in-agent-A-model in-utility inferencer))
   (⊦ equal? '(((V (-0. (A 2))) 0.25) ((V (-10. (A 3))) 0.25) ((V (-10. (A 1))) 0.25)
               ((V (-20. (A 0))) 0.25))
      (probcc-reify/exact/a (in-agent-A-model ut1)))
   (⊦ equal? '((-0. (A 2))) (in-argmax (probcc-reify/exact/a (in-agent-A-model ut1))))
   (⊦ equal? '(((V (-0. (F_b1))) 1.) ((V (-10. (F_b1 F_b0))) 1.) ((V (-10. (F_b0))) 1.)
               ((V (-20. ())) 1.))
      (probcc-reify/exact/a (speaker in-exact)))
   (⊦ equal? '(((V (-10. (F_b1 F_b0))) 0.5) ((V (-10. (F_b1))) 0.5) ((V (-10. (F_b0))) 0.5)
               ((V (-10. ())) 0.5) ((V (-0. (F_b1 F_b0))) 0.25) ((V (-0. (F_b1))) 0.25)
               ((V (-0. (F_b0))) 0.25) ((V (-0. ())) 0.25) ((V (-20. (F_b1 F_b0))) 0.25)
               ((V (-20. (F_b1))) 0.25) ((V (-20. (F_b0))) 0.25) ((V (-20. ())) 0.25))
      (probcc-reify/exact/a (speaker (in-rejection 1))))
   (⊦ equal? '(((V (-10. (F_b1 F_b0))) 0.5078125) ((V (-10. (F_b1))) 0.5) ((V (-10. (F_b0))) 0.5)
               ((V (-10. ())) 0.4921875) ((V (-0. (F_b1))) 0.26171875) ((V (-20. ())) 0.26171875)
               ((V (-0. (F_b1 F_b0))) 0.25390625) ((V (-0. (F_b0))) 0.25) ((V (-20. (F_b0))) 0.25)
               ((V (-0. ())) 0.24609375) ((V (-20. (F_b1 F_b0))) 0.23828125)
               ((V (-20. (F_b1))) 0.23828125))
      (probcc-reify/exact/a (speaker (in-rejection 2))))
   `(doc (p "The two-bit game (IN:78-207): the hearer interprets the form F_b1 as two people
	    coming and buys two pieces of food; the speaker, who knows that two come, best says
	    F_b1 when the hearer infers exactly; if the hearer only draws one sample, what the
	    speaker says does not matter, with two samples it pays, by 1%, to tell the truth.
	    The utilities are " (code/inline "-0.") " where nothing is lost, as upstream's; the
	    tables are upstream's bit for bit, in the Scheme order (the order of the form
	    variables F_b0, F_b1 is the same in OCaml and by name).")))

  ((test/incite/some-all _)
   (define (ut1 fv) (eq? fv 'F_some))
   (define (ut2 fv) (and (memq fv '(F_some F_notall)) #t))
   (define agent-A (in-agent-A-model/some-all in-utility))
   (define agent-A/asymmetric (in-agent-A-model/some-all in-utility/asymmetric))
   (define (speaker agent-A utility inferencer)
     (in-agent-S-model in-all-forms agent-A utility inferencer))
   (⊦ equal? '(((V (-10. (A 2))) 0.5) ((V (-0. (A 3))) 0.25) ((V (-0. (A 2))) 0.25)
               ((V (-0. (A 1))) 0.25) ((V (-10. (A 3))) 0.25) ((V (-10. (A 1))) 0.25)
               ((V (-10. (A 0))) 0.25) ((V (-20. (A 3))) 0.25) ((V (-20. (A 1))) 0.25)
               ((V (-20. (A 0))) 0.25) ((V (-30. (A 0))) 0.25))
      (probcc-reify/exact/a (agent-A ut1)))
   (⊦ equal? '((-6.66666666666666696 (A 2))) (in-argmax (probcc-reify/exact/a (agent-A ut1))))
   (⊦ equal? '(((V (-0. (A 2))) 0.125) ((V (-0. (A 1))) 0.125) ((V (-10. (A 3))) 0.125)
               ((V (-10. (A 2))) 0.125) ((V (-10. (A 1))) 0.125) ((V (-10. (A 0))) 0.125)
               ((V (-20. (A 3))) 0.125) ((V (-20. (A 0))) 0.125))
      (probcc-reify/exact/a (agent-A ut2)))
   (⊦ equal? '((-5. (A 2)) (-5. (A 1))) (in-argmax (probcc-reify/exact/a (agent-A ut2))))
   (let1 (table (probcc-reify/exact/a (speaker agent-A in-utility in-exact)))
     (⊦ equal? (canon '(((V (-0. (F_some F_none))) 0.25) ((V (-0. (F_some F_notall F_none))) 0.25)
                        ((V (-0. (F_some F_notall))) 0.5) ((V (-0. (F_some))) 1.)
                        ((V (-0. (F_all F_none))) 0.25) ((V (-0. (F_all F_notall F_none))) 0.25)
                        ((V (-0. (F_all F_notall))) 0.25) ((V (-0. (F_all F_some F_none))) 0.25)
                        ((V (-0. (F_all F_some F_notall F_none))) 0.25)
                        ((V (-0. (F_all F_some F_notall))) 0.25) ((V (-0. ())) 0.5)
                        ((V (-10. (F_notall))) 1.) ((V (-10. (F_some F_none))) 0.5)
                        ((V (-10. (F_some F_notall F_none))) 0.5)
                        ((V (-10. (F_some F_notall))) 0.5) ((V (-10. (F_all F_none))) 0.5)
                        ((V (-10. (F_all F_notall F_none))) 0.5)
                        ((V (-10. (F_all F_notall))) 0.5) ((V (-10. (F_all F_some F_none))) 0.5)
                        ((V (-10. (F_all F_some F_notall F_none))) 0.5)
                        ((V (-10. (F_all F_some F_notall))) 0.5) ((V (-10. (F_all F_some))) 1.)
                        ((V (-10. (F_all))) 1.) ((V (-10. ())) 0.5) ((V (-20. (F_none))) 1.)
                        ((V (-20. (F_notall F_none))) 1.) ((V (-20. (F_some F_none))) 0.25)
                        ((V (-20. (F_some F_notall F_none))) 0.25)
                        ((V (-20. (F_all F_none))) 0.25)
                        ((V (-20. (F_all F_notall F_none))) 0.25)
                        ((V (-20. (F_all F_notall))) 0.25)
                        ((V (-20. (F_all F_some F_none))) 0.25)
                        ((V (-20. (F_all F_some F_notall F_none))) 0.25)
                        ((V (-20. (F_all F_some F_notall))) 0.25)))
        (canon table))
     (⊦ equal? '((-0. (F_some))) (in-argmax table)))
   (⊦ equal? '((-8.90625 (F_some)))
      (in-argmax (probcc-reify/exact/a (speaker agent-A in-utility (in-rejection 2)))))
   (⊦ equal? '(((V (-0. (A 3))) 0.25) ((V (-0. (A 2))) 0.25) ((V (-0. (A 1))) 0.25)
               ((V (-10. (A 3))) 0.25) ((V (-10. (A 2))) 0.25) ((V (-20. (A 3))) 0.25)
               ((V (-100. (A 2))) 0.25) ((V (-100. (A 1))) 0.25) ((V (-100. (A 0))) 0.25)
               ((V (-200. (A 1))) 0.25) ((V (-200. (A 0))) 0.25) ((V (-300. (A 0))) 0.25))
      (probcc-reify/exact/a (agent-A/asymmetric ut1)))
   (⊦ equal? '((-10. (A 3))) (in-argmax (probcc-reify/exact/a (agent-A/asymmetric ut1))))
   (⊦ equal? '(((V (-0. (A 2))) 0.125) ((V (-0. (A 1))) 0.125) ((V (-10. (A 3))) 0.125)
               ((V (-10. (A 2))) 0.125) ((V (-20. (A 3))) 0.125) ((V (-100. (A 1))) 0.125)
               ((V (-100. (A 0))) 0.125) ((V (-200. (A 0))) 0.125))
      (probcc-reify/exact/a (agent-A/asymmetric ut2)))
   (⊦ equal? '((-5. (A 2))) (in-argmax (probcc-reify/exact/a (agent-A/asymmetric ut2))))
   (⊦ equal? (canon '((-0. (F_notall)) (-0. (F_some F_notall))))
      (canon (in-argmax (probcc-reify/exact/a
                          (speaker agent-A/asymmetric in-utility/asymmetric in-exact)))))
   (⊦ equal? (canon '((-60.078125 (F_some)) (-60.078125 ())))
      (canon (in-argmax (probcc-reify/exact/a
                          (speaker agent-A/asymmetric in-utility/asymmetric (in-rejection 2))))))
   (⊦ equal? '((-0. (F_notall)))
      (in-argmax (probcc-reify/exact/a
                   (speaker agent-A/asymmetric in-utility/asymmetric (in-importance 2)))))
   ; cf. IN:416-422: the speaker sampled with (random_selector 17), 100 and 1000 worlds, the
   ; hearer by rejection of 10 worlds, as upstream computes them today under OCaml with the
   ; three-draw generator (-25.6944444444444429 and -19.0034722222222214, the same doubles); the
   ; records, -5.31250000000000089 and -15.9212239583333304, came from another generator (D20).
   (let1 (speaker (speaker agent-A/asymmetric in-utility/asymmetric (in-rejection 10)))
     (⊦ equal? '((-25.6944444444444429 (F_some)))
        (in-argmax (probcc-sample-importance/a (probcc-random-selector 17) 100 speaker)))
     (⊦ equal? '((-19.0034722222222214 (F_some)))
        (in-argmax (probcc-sample-importance/a (probcc-random-selector 17) 1000 speaker))))
   `(doc (p "The some/all game (IN:210-426): the forms are the sixteen subsets of all, some,
	    not all and none, and a conjunction is understood only when the hearer also guesses
	    the conjunction state.  With the symmetric utility the speaker best says some; with
	    the asymmetric one, where buying too little costs ten times more, not all, or some
	    and not all, when the hearer infers exactly, and some, or nothing, when the hearer
	    draws two samples.  OCaml orders the form variables by declaration, pmap-compare by
	    name (deviation 6), so the tables and maxima keyed by forms are compared as sets.
	    IN:416-422 sample the speaker with " (code/inline "random_selector")
            ": the records do not replay (plan D20), but what upstream gives under OCaml today
	    does, bit for bit.")))

  ((test/blip/ailog _)
   (define (query q)
     (probcc-reify/exact (match/first (ba-blip-model) ((,pos ,anyblip1 ,_) (q pos anyblip1)))))
   (⊦ equal? `(((V ,(void)) 1/100))
      (query (λ (pos _) (if (and (equal? (pos '(1 0)) '(3 3)) (equal? (pos '(1 0)) '(3 3)))
                            (void)
                            (probcc-impossible)))))
   (⊦ equal? '()
      (query (λ (pos _) (if (and (equal? (pos '(1 0)) '(3 3)) (equal? (pos '(1 0)) '(3 4)))
                            (void)
                            (probcc-impossible)))))
   (⊦ (rel-approx? 1e-9) `(((V ,(void)) 0.0015))
      (query (λ (pos _) (if (and (equal? (pos '(1 0)) '(3 3)) (equal? (pos '(1 1)) '(3 4)))
                            (void)
                            (probcc-impossible)))))
   (⊦ (rel-approx? 1e-9) '(((V #f) 0.971180000000001153) ((V #t) 0.0288200000000000019))
      (query (λ (_ anyblip1) (anyblip1 '((3 3) 0)))))
   (⊦ (rel-approx? 1e-9) '(((V #f) 0.971180000000001153) ((V #t) 0.0288200000000000019))
      (query (λ (_ anyblip1) (and (anyblip1 '((3 3) 0)) (anyblip1 '((3 3) 0))))))
   (⊦ (rel-approx? 1e-9) '(((V #f) 0.995959594000000892) ((V #t) 0.00191968600000000031))
      (query (λ (_ anyblip1) (and (anyblip1 '((3 3) 0)) (anyblip1 '((3 4) 1))))))
   `(doc (p "The AILog model of the blips (BA:181-216), where every prior and every step of
	    the dynamics is memoized, so that a fact observed once stays the same: the position
	    of a plane asked twice, at one and at two times, and the blips at one and two
	    times.  The memo tables live in the world-local memory, so the model is
	    instantiated inside the reification, as upstream's functor is.")))

  ((test/blip/lazy _)
   (define ((evidence0 blips) ps t) (when (= t 0) (bl-blips-ideal blips ps)))
   (define (count blips n)
     (probcc-reify/exact (car (bl-step (bl-parm-n n) bl-state0 0 (evidence0 blips)))))
   (⊦ equal? `(((V ,(void)) 1/100))
      (probcc-reify/exact (match/first (bl-lplane-state-0 1)
                            ((,_ (,x ,y) ,_) (if (and (= (x) 3) (= (y) 3))
                                                 (void)
                                                 (probcc-impossible))))))
   (⊦ (rel-approx? 1e-9) `(((V ,(void)) 0.00130434782608695684))
      (probcc-reify/exact (begin (bl-step (bl-parm-n 1) bl-state0 0 (evidence0 '((3 3)))) (void))))
   (⊦ equal? `(((V ,(void)) 1/5000))
      (probcc-reify/exact (let* ((p2 (probcc-letlazy (τ (bl-lplane-state-0 2))))
                                 (p1 (probcc-letlazy (τ (bl-lplane-state-0 1)))))
                            (bl-blips-ideal '((3 3) (3 5)) (list p1 p2)))))
   (⊦ equal? '()
      (probcc-reify/exact (begin
                            (bl-step (bl-parm-n 1) bl-state0 0 (evidence0 '((3 3) (3 5))))
                            (void))))
   (⊦ (rel-approx? 1e-9) '(((V 2) 3.83795309168443282e-06)) (count '((3 3) (3 5)) 2))
   (⊦ (rel-approx? 1e-9) '(((V 2) 3.8269373870522e-06) ((V 3) 1.72212182417348577e-08))
      (count '((3 3) (3 5)) 3))
   (⊦ (rel-approx? 1e-9) '(((V 3) 1.72212182417348577e-08)) (count '((3 3) (3 5) (3 7)) 3))
   (⊦ (rel-approx? 1e-9) `(((V ,(void)) 0.000195652173913043515))
      (probcc-reify/exact
        (let1 (st (bl-step (bl-parm-n 1) bl-state0 0 bl-two-blips-ideal))
          (bl-step bl-parm-no (bl-lazy-state st) 1 bl-two-blips-ideal)
          (void))))
   (let1 (q2b-s0 (probcc-reify/exact (bl-step (bl-parm-n 1) bl-state0 0 bl-two-blips-ideal)))
     (⊦ (rel-approx? 1e-9) `(((V ,(void)) 0.000195652173913043515))
        (probcc-reify/exact
          (let1 (st (bl-lazy-state (probcc-reflect q2b-s0)))
            (bl-step bl-parm-no st 1 bl-two-blips-ideal)
            (void)))))
   ; cf. BL:430-493: three planes in motion, step by step; 384 worlds at each step, and the
   ; distribution of the number of planes re-collated with probcc-map-dist (BL:462-466).
   (let* ((q3b-s0 (probcc-reify/exact (bl-step (bl-parm-n 3) bl-state0 0 blip333-ideal)))
          (q3b-s1 (probcc-reify/exact (bl-step bl-parm-no (bl-lazy-state (probcc-reflect q3b-s0))
                                               1 blip333-ideal)))
          (q3b-s2 (probcc-reify/exact (bl-step bl-parm-no (bl-lazy-state (probcc-reflect q3b-s1))
                                               2 blip333-ideal)))
          (nplanes (λ (st) (length (cadr st)))))
     (⊦ equal? '(384 384 384) (probcc-map length (list q3b-s0 q3b-s1 q3b-s2)))
     (⊦ (rel-approx? 1e-9) '(((V 3) 1.72212182417348577e-08)) (probcc-map-dist nplanes q3b-s0))
     (⊦ (rel-approx? 1e-9) '(((V 3) 5.81216115658552e-11)) (probcc-map-dist nplanes q3b-s1))
     (⊦ (rel-approx? 1e-9) '(((V 3) 2.07608302413096791e-12)) (probcc-map-dist nplanes q3b-s2)))
   `(doc (p "Lazy planes and the whole screen observed (BL:263-428): a coordinate is drawn
	    only when an observation compares it, so two and three planes are within reach of
	    exact inference: two blips may come from two planes, or from three when two are
	    in one cell, and three blips from three planes with probability "
            (code/inline "6 · 0.01^3 · 0.00287020304028914744") ".  The motion of one plane over
	    two steps is inferred in one query, and step by step, by reflecting the exact
	    state of the first step into the second (sequential Monte-Carlo, BL:419-428), with
	    the same result; so are three moving planes over three steps (BL:430-493), whose
	    number of worlds stays 384, since one motion only is consistent with the blips.")))

  )

; a resume that fails to rewind its `dynamic-wind` frames would leave a test's string port as
; the current output port, hiding the summary; check that the port is back.
(import (only (chicken base) exit))

(define stdout (current-output-port))

(let1 (r (unittest/✓ hansei-examples))
  (unless (eq? stdout (current-output-port))
    (current-output-port stdout)
    (write (unittest/result-summary r))
    (newline)
    (display "FAIL: the suite ended with its output still captured by a test\n")
    (exit 1)))
