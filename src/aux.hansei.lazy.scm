;; (aux hansei lazy) -- lazy lists and lazy permutations for (aux hansei), a CHICKEN port of
;; Oleg Kiselyov and Chung-chieh Shan's HANSEI (https://okmij.org/ftp/kakuritu/Hansei.html).
;;
;; Mirrors (paths relative to aux.scm/upstream/hansei/, citation keys as in the plan
;; AUX-HANSEI-PLAN.md §0):
;;   SL   slazy.ml (https://okmij.org/ftp/kakuritu/slazy.ml), the lazy-list kit of SL:325-355;
;;   M1   music1a.ml (https://okmij.org/ftp/kakuritu/music1a.ml), to_lcons, llength, lmap
;;        (M1:51-101);
;;   M2   music2.ml (https://okmij.org/ftp/kakuritu/music2.ml), lobserve (M2:113-119);
;;   LP   lazy_permutations.ml and LPI lazy_permutations.mli
;;        (https://okmij.org/ftp/kakuritu/lazy_permutations.ml): correlated memoization, lazy
;;        permutations, split_at, uniformly_uncons, insert_shift (H, `#all-different`).
;; Upstream's lazy-list kit is example code, shared by SL, M1 and M2; it is promoted to a library
;; module here (plan D10, deviation 24).  A lazy list is a thunk answering '() or a pair
;; (head-thunk . tail-llist), built with probcc-letlazy, never with delay/force (DSL p.21
;; l.912-922).
;;
;; HANSEI code version: April 2009 (H, "Implementation"); lazy permutations December 2010; web
;; page last updated March 1, 2016.
;; Papers: O. Kiselyov, C. Shan, "Embedded Probabilistic Programming", IFIP DSL 2009, LNCS 5658,
;; pp. 360-384 (DSL = dsl-paper.txt / .pdf), §"Lazy evaluation" (p.19-21).
;;
;; Contents (plan §3.5, §3.6):
;;   - the lazy-list kit: probcc-lnil, probcc-lcons, probcc-lappend, probcc-lhead, probcc-ltail,
;;     probcc-lreflect, probcc-llength/at-most, probcc-list->llist, probcc-llength, probcc-lmap,
;;     probcc-lobserve;
;;   - lazy permutations: probcc-split-at, probcc-uniform/either/uncons,
;;     probcc-lazy-permutation, probcc-insert-shift, probcc-lazy-permutation/ins.
;; LP's `uniformly_list` (LP:24, LPI:7) is probcc-uniform/either of (aux hansei).  The other
;; definitions of SL, M1 and M2 (`splitAt`, `split`, the PCFG, the notes and their transforms)
;; and of sorted.ml (its strict-head lazy lists, `build_list`, `is_sorted`, `bit_list`) are
;; examples, ported in the tests (plan §3.6).

(module (aux hansei lazy) (probcc-lnil probcc-lcons probcc-lappend probcc-lhead probcc-ltail
                           probcc-lreflect probcc-llength/at-most probcc-list->llist
                           probcc-llength probcc-lmap probcc-lobserve
                           probcc-split-at probcc-uniform/either/uncons probcc-lazy-permutation
                           probcc-insert-shift probcc-lazy-permutation/ins)

  (import scheme
          (chicken base)
          (only (scheme base) exact-integer?)
          (only (chicken condition) make-property-condition make-composite-condition abort)
          (only srfi-1 append-reverse)
          (aux base)
          (aux hansei))

  ;; ---------------------------------------------------------------------------------------
  ;; Lazy lists (SL:325-355; M1:51-101; M2:113-119): an llist is a thunk answering '()
  ;; (`LNil`) or a pair (head-thunk . tail-llist) (`LCons`); the head is a thunk too.
  ;; ---------------------------------------------------------------------------------------

  ; cf. SL:329 = M1:54 `nil`: the empty lazy list.  Deviation 24 (SL:325-326): '() and pairs
  ; stand for `LNil` and `LCons`, and the kit is a library module.
  (define probcc-lnil (τ '()))

  ; cf. SL:330 = M1:55 `cons`: the lazy list of head thunk h and tail llist t.
  (define (probcc-lcons h t) (τ (cons h t)))

  ; cf. SL:332-336 = M1:57-61 `lappend`: y then z, each cell forced at most once per world
  ; (probcc-letlazy), and only when it is needed.
  (define (probcc-lappend y z)
    (probcc-letlazy (τ (match/first (y)
                         (() (z))
                         ((,h . ,t) (cons h (probcc-lappend t z)))))))

  ; cf. SL:338-339 = M1:63-64 `lhead`: the head thunk of x; the empty list fails.
  (define (probcc-lhead x)
    (match/first (x)
      ((,h . ,_) h)
      (() (probcc-impossible))))

  ; cf. SL:342-343 = M1:67-68 `ltail`: the tail llist of x; the empty list fails.
  (define (probcc-ltail x)
    (match/first (x)
      ((,_ . ,t) t)
      (() (probcc-impossible))))

  ; cf. SL:346 = M1:76 `lreflect`: the value of the thunk x.
  (define (probcc-lreflect x) (x))

  ; cf. SL:350-355 `list_length_at_most`: the length of l, counting at most max cells, so that
  ; an infinite (or long) list is forced only up to there; Ken Shan's example.
  (define (probcc-llength/at-most max l)
    (cond
      ((<= max 0) 0)
      (else (match/first (l)
              (() 0)
              ((,_ . ,t) (add1 (probcc-llength/at-most (sub1 max) t)))))))

  ; cf. M1:71-73 `to_lcons`: the lazy list of the elements of lst, each head the thunk of its
  ; element.
  (define (probcc-list->llist lst)
    (match/first lst
      (() probcc-lnil)
      ((,h . ,t) (probcc-lcons (τ h) (probcc-list->llist t)))))

  ; cf. M1:78-83 `llength'` and `llength`: the number of cells of l, every tail forced (the
  ; heads are not); `loop` is `llength'`, on a forced cell.
  (define (probcc-llength l)
    (let loop ((c (l)))
      (match/first c
        (() 0)
        ((,_ . ,t) (add1 (loop (t)))))))

  ; cf. M1:98-101 `lmap'` and `lmap` (= M2:106-109): the lazy list of f applied to the heads of
  ; l, each new head and cell forced at most once per world (probcc-letlazy).
  (define (probcc-lmap f l)
    (probcc-letlazy (τ (match/first (l)
                         (() '())
                         ((,h . ,t) (cons (probcc-letlazy (τ (f (h)))) (probcc-lmap f t)))))))

  ; cf. M2:113-119 `lobserve`: the evidence that the lazy list ll has the elements of the list
  ; lst, compared left to right as their cells are forced: (void) when it does, a failure as
  ; soon as a head or the length differs.  Deviation 24: heads compare with equal?, where
  ; OCaml's `=` has -0. equal to 0. and a NaN different from itself.
  (define (probcc-lobserve ll lst)
    (match/first (ll)
      (() (if (null? lst) (void) (probcc-impossible)))
      ((,h . ,t) (if (and (pair? lst) (equal? (h) (car lst)))
                     (probcc-lobserve t (cdr lst))
                     (probcc-impossible)))))

  ;; ---------------------------------------------------------------------------------------
  ;; Correlated memoization and lazy permutations (LP; LPI; H `#all-different`)
  ;; ---------------------------------------------------------------------------------------

  ; cf. LP:139-146 `split_at` (LPI:29-37): the zipper of lst at its n-th element, (list prev
  ; tail): the first n elements in reverse order and the rest, whose head is the n-th element
  ; counting from 0; a list shorter than n gives (list (reverse lst) '()).  Deviation 25: the
  ; pair is a list.
  (define (probcc-split-at n lst)
    (let loop ((acc '()) (n n) (lst lst))
      (cond
        ((zero? n) (list acc lst))
        ((null? lst) (list acc '()))
        (else (loop (cons (car lst) acc) (sub1 n) (cdr lst))))))

  ; cf. LP:158-166 `uniformly_uncons` (LPI:11): an element of lst chosen uniformly, and the
  ; others, (list x rest), the elements before x reversed in front of those after it
  ; (`List.rev_append`); a one-element list makes no choice, the empty list fails.
  ; Deviation 25: the pair is a list.
  (define (probcc-uniform/either/uncons lst)
    (match/first lst
      (() (probcc-impossible))
      ((,x) (list x '()))
      (else (let1 (i (probcc-uniform (length lst)))
              (match/first (probcc-split-at i lst)
                ((,prev (,h . ,next)) (list h (append-reverse prev next))))))))

  ; cf. LP:175-188 `lazy_permutation` (LPI:13-16): a permutation of lst as a list of thunks,
  ; correlated random variables: the i-th thunk chooses, when first forced, among the elements
  ; not chosen by the variables before it, which it forces.  Each variable is a probcc-letlazy
  ; (not nesting-safe, as upstream) holding (list x rest), the next one unconsing `rest`; the
  ; last takes the one element left.  Forcing the i-th of n elements makes only n (n - 1) ...
  ; (n - i) choices, where a permutation drawn whole makes n! (LP:109-124).
  (define (probcc-lazy-permutation lst)
    (letrec ((loop (λ (prev-var lst)
                     (match/first lst
                       ((,_) (list (τ (match/first (prev-var) ((,_ (,x)) x)))))
                       ((,_ . ,t)
                        (let1 (new-var (probcc-letlazy
                                         (τ (probcc-uniform/either/uncons (cadr (prev-var))))))
                          (cons (τ (car (new-var))) (loop new-var t))))))))
      (match/first lst
        (() '())
        ((,x) (list (τ x)))
        ((,_ . ,t) (let1 (var0 (probcc-letlazy (τ (probcc-uniform/either/uncons lst))))
                     (cons (τ (car (var0))) (loop var0 t)))))))

  ; cf. LP:242-249 `insert_shift`: given a sorted list lst of distinct integers and i, the
  ; integer j such that i is j minus the number of elements of lst below j, and lst with j
  ; inserted, (list j lst').  Not in LPI; exported for the tests.  Deviation 25: the pair is
  ; a list.
  (define (probcc-insert-shift i lst)
    (let loop ((acc '()) (i i) (lst lst))
      (match/first lst
        (() (list i (reverse (cons i acc))))
        (((,h . ,_) ⊣ (> h i)) (list i (append-reverse acc (cons i lst))))
        ((,h . ,t) (loop (cons h acc) (add1 i) t)))))

  ; cf. LP:272-287 `lazy_permutation_ins` (LPI:18-27): n all-different random variables over
  ; 0 .. n - 1, as thunks, by insertion where probcc-lazy-permutation selects: the k-th
  ; variable (from 0) chooses i among n - k values, then inserts it with probcc-insert-shift
  ; into the sorted values of the variables before it, which it forces.  Deviation 33
  ; (LP:281-286): an n that is not a non-negative exact integer raises (exn hansei
  ; malformed), where upstream fails to match (negative) or does not type check.
  (define (probcc-lazy-permutation/ins n)
    (letrec ((loop (λ (prev-var n)
                     (cond
                       ((= n 1) (list (τ (car (probcc-insert-shift 0 (cadr (prev-var)))))))
                       (else (let1 (new-var (probcc-letlazy
                                              (τ (let1 (i (probcc-uniform n))
                                                   (probcc-insert-shift i (cadr (prev-var)))))))
                               (cons (τ (car (new-var))) (loop new-var (sub1 n)))))))))
      (cond
        ((not (and (exact-integer? n) (>= n 0)))
         (abort (make-composite-condition
                  (make-property-condition 'exn 'location 'probcc-lazy-permutation/ins
                                           'message "not a non-negative exact integer number of variables"
                                           'arguments (list n))
                  (make-property-condition 'hansei 'kind 'malformed))))
        ((= n 0) '())
        ((= n 1) (list (τ 0)))
        (else (let1 (var0 (probcc-letlazy (τ (let1 (i (probcc-uniform n)) (list i (list i))))))
                (cons (τ (car (var0))) (loop var0 (sub1 n))))))))

  )
