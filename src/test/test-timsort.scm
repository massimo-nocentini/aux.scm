
(import unittest aux scheme (chicken sort) (chicken syntax) srfi-1)

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
                  ". For the sake of completeness, we report their implementation of the " (code/inline "sort!") " function, which is a wrapper around the " (code/inline "merge!") " function, which merges two sorted sequences. The " (code/inline "sort!") " function sorts a sequence in place using a provided comparison function " (code/inline "less?") "."
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
   #;(⊦= '(1.1 2.1 3.1 4.1 5.1) (timsort/primitive '(5.1 4.1 3.1 2.1 1.1)))
   (⊦= '(hello world) (timsort/primitive '(world hello))))

  )

(unittest/✓ timsort-suite)