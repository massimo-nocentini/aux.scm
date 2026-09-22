
(import (aux unittest) (aux timsort) scheme (chicken sort) (chicken syntax) srfi-1)

(define n 1000000)
(define r (iota n))

(define-suite timsort-suite


  ((doc r) (let ((letcc-expr '(letcc k body ...)))
             `((p "Here is a test suite for the " (code/inline "timsort") " function, which implements the Timsort algorithm, a hybrid sorting algorithm derived from merge sort and insertion sort. It is designed to perform well on many kinds of real-world data. The implementation follows the specifications outlined in "
                  (cite/a "https://github.com/python/cpython/blob/main/Objects/listobject.c" "CPython: listobject.c")
                  (cite/a "https://github.com/python/cpython/blob/main/Objects/listsort.txt" "CPython: listsort.txt")
                  " that we extracted in "
                  (cite/a "https://github.com/massimo-nocentini/timsort.c" "Github: timsort.c")
                  " and we compare it against the built-in " (code/inline "sort") " function "
                  (cite/a "https://srfi.schemers.org/srfi-95/srfi-95.html" "SRFI 95") 
                  (cite/a "https://en.wikipedia.org/wiki/Sorting_algorithm" "Wikipedia: Sorting algorithm")
                  ". For the sake of completeness, we report their implementation of the " (code/inline "sort!") 
                  " function, which is a wrapper around the " (code/inline "merge!") " function, which merges two sorted sequences. The " 
                  (code/inline "sort!") " function sorts a sequence in place using a provided comparison function " (code/inline "less?") "."
                  (code/scheme
                    (define (sort! seq less?)
                      (define (step n)
                        (cond
                          ((> n 2)
                           (let* ((j (quotient n 2))
                                  (a (step j))
                                  (k (- n j))
                                  (b (step k)))
                             (merge! a b less?)))
                          ((= n 2)
                           (let ((x (car seq))
                                 (y (cadr seq))
                                 (p seq))
                             (set! seq (cddr seq))
                             (if (less? y x) (begin
                                               (set-car! p y)
                                               (set-car! (cdr p) x)))
                             (set-cdr! (cdr p) '())
                             p))
                          ((= n 1)
                           (let ((p seq))
                             (set! seq (cdr seq))
                             (set-cdr! p '())
                             p))
                          (else
                            '()) ))
                      (if (vector? seq)
                          (let ((n (vector-length seq))
                                (vec seq))
                            (set! seq (vector->list seq))
                            (do ((p (step n) (cdr p))
                                 (i 0 (+ i 1)))
                                ((null? p) vec)
                                (vector-set! vec i (car p)) ))
                          ;; otherwise, assume it is a list
                          (step (length seq)) )))                  
                  (code/scheme
                    (define (merge! a b less?)
                      (define (loop r a b)
                        (if (less? (car b) (car a))
                            (begin
                              (set-cdr! r b)
                              (if (null? (cdr b))
                                  (set-cdr! b a)
                                  (loop b a (cdr b)) ))
                            ;; (car a) <= (car b)
                            (begin
                              (set-cdr! r a)
                              (if (null? (cdr a))
                                  (set-cdr! a b)
                                  (loop a (cdr a) b)) )) )
                      (cond
                        ((null? a) b)
                        ((null? b) a)
                        ((less? (car b) (car a))
                         (if (null? (cdr b))
                             (set-cdr! b a)
                             (loop b a (cdr b)))
                         b)
                        (else ; (car a) <= (car b)
                          (if (null? (cdr a))
                              (set-cdr! a b)
                              (loop a (cdr a) b))
                          a))))            ))))

  ((test/simple _)
   (⊦= '(1 2 3 4 5) (timsort '(5 4 3 2 1)))
   (⊦= '(1 2 3 4 5) (timsort '(1 2 3 4 5)))
   (⊦= '(1 2 3 4 5) (timsort '(3 2 1 5 4)))
   (⊦= '(1) (timsort '(1)))
   (⊦= '() (timsort '())))

  ((test/iota _)
   (⊦= r (timsort (iota n (sub1 n) -1)))
   `(doc (p "Here we test the timsort function with a large list of integers from 0 to " ,n " elements long. The list is sorted in descending order, and we expect the result to be a list sorted in ascending order.")))

  ((test/iota/sort _)
   (⊦= r (sort (iota n (sub1 n) -1) <)))

  ((test/timtros/iota _)
   (⊦= (iota n (sub1 n) -1) (timtros r)))

  ((test/tros/iota _)
   (⊦= (iota n (sub1 n) -1) (reverse (sort r <))))

  ((test/already-sorted _)
   (⊦= r (timsort r)))

  ((test/already-sorted/sort _)
   (⊦= r (sort r <)))

  ((test/already-sorted/primitive _)
   (⊦= r (timsort/primitive r)))

  ((test/primitive _)
   (⊦= (sort '(5 4 3 2 1) <) (timsort/primitive '(5 4 3 2 1)))
   (⊦= '(1.1 2.1 3.1 4.1 5.1) (timsort/primitive '(5.1 4.1 3.1 2.1 1.1)))
   (⊦= '(hello world) (timsort/primitive '(world hello))))

  ((test/vector _)
   (⊦= '#(1 2 3 4 5) (timsort/vector '#(5 4 3 2 1)))
   (⊦= '#(5 4 3 2 1) (timtros/vector '#(1 2 3 4 5)))
   (⊦= '#(1) (timsort/vector '#(1)))
   (⊦= '#() (timsort/vector '#()))
   ;; the non-destructive form must leave its argument alone …
   (let ((v (vector 3 1 2)))
     (⊦= '#(1 2 3) (timsort/vector v))
     (⊦= '#(3 1 2) v))
   ;; … and the destructive one must sort, and return, that very object.
   (let ((v (vector 3 1 2)))
     (⊨ (eq? v (begin (timsort/vector! v) v)))
     (⊦= '#(1 2 3) v))
   `(doc (p "The vector entry points sort a vector without going through a list: "
            (code/inline "timsort/vector") " returns a fresh vector and leaves its "
            "argument untouched, while " (code/inline "timsort/vector!") " sorts in "
            "place and returns the vector it was given.")))

  ((test/vector/primitive _)
   (⊦= '#("apple" "fig" "pear") (timsort/primitive/vector '#("pear" "apple" "fig")))
   (⊦= '#(a m z) (timsort/primitive/vector '#(z a m)))
   (⊦= '#(#\a #\b #\z) (timsort/primitive/vector '#(#\z #\b #\a)))
   (⊦= '#(#f #t) (timsort/primitive/vector '#(#t #f))))

  ((test/key _)
   ;; Ordered by the key, and STABLE: equal keys keep their input order, which
   ;; the (key . position) decoration below makes observable.
   (let ((recs (list (cons 1 'a) (cons 0 'b) (cons 1 'c) (cons 0 'd))))
     (⊦= (list (cons 0 'b) (cons 0 'd) (cons 1 'a) (cons 1 'c))
         (timsort/key recs car))
     ;; descending keys, ties still in INPUT order
     (⊦= (list (cons 1 'a) (cons 1 'c) (cons 0 'b) (cons 0 'd))
         (timtros/key recs car)))
   ;; the key runs exactly once per element, in input order
   (let* ((seen '())
          (recs (list (cons 3 'x) (cons 1 'y) (cons 2 'z)))
          (spy (lambda (r) (set! seen (cons r seen)) (car r))))
     (timsort/key recs spy)
     (⊦= recs (reverse seen)))
   `(doc (p "A key is applied exactly once per element rather than twice per "
            "comparison, so the ordering is decided entirely in C and the sort "
            "makes no call back into Scheme at all. A key can only express a "
            "total preorder; an arbitrary comparator still needs "
            (code/inline "timsort/gen") ".")))

  ((test/vector/key _)
   (let ((v (vector (cons 1 'a) (cons 0 'b) (cons 1 'c))))
     (⊦= (vector (cons 0 'b) (cons 1 'a) (cons 1 'c)) (timsort/vector/key v car))
     (⊦= (vector (cons 1 'a) (cons 0 'b) (cons 1 'c)) v)))

  )

(unittest/✓ timsort-suite)