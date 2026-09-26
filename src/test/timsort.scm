
(import (aux unittest) (aux timsort) scheme (chicken sort) (chicken syntax) (chicken condition) (only (scheme base) guard raise raise-continuable)
        (rename (only (scheme base) with-exception-handler) (with-exception-handler r7rs-with-exception-handler))
        srfi-1)

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

  ;; A comparator that catches what a NESTED sort's comparator raised.  Had
  ;; the raise escaped through the inner sort's C frames, the runtime would
  ;; resume that abandoned sort when the outer callback returns: its error
  ;; resurfaced outside the handler that caught it and killed the outer sort,
  ;; or, once the inner comparator stopped raising, the process panicked with
  ;; "callback returned twice".
  ((test/nested-raise _)
   (let* ((seen '())
          (inner (timsort/gen (lambda (a b) (error "boom")) #f #f #f #t TIMSORT_USE_COMPARATOR))
          (outer (timsort/gen (lambda (a b)
                                (handle-exceptions e
                                  (set! seen (cons (get-condition-property e 'exn 'message) seen))
                                  (inner '(3 1 2)))
                                (< a b))
                              #f #f #f #t TIMSORT_USE_COMPARATOR)))
     (⊦= '(1 2 5 7 9) (outer '(5 2 9 1 7)))
     (⊨ (and (pair? seen) (every (lambda (m) (equal? m "boom")) seen)))
     ;; one inner raise per outer comparison, and each handler body ran once
     (⊦= (length seen)
         (let ((n 0))
           ((timsort/gen (lambda (a b) (set! n (add1 n)) (< a b)) #f #f #f #t TIMSORT_USE_COMPARATOR) '(5 2 9 1 7))
           n)))
   (let* ((count 0)
          (inner (timsort/gen (lambda (a b) (set! count (add1 count)) (if (= count 1) (abort 'once) (< a b)))
                              #f #f #f #t TIMSORT_USE_COMPARATOR))
          (outer (timsort/gen (lambda (a b) (handle-exceptions e e (inner '(3 1 2))) (< a b))
                              #f #f #f #t TIMSORT_USE_COMPARATOR)))
     (⊦= '(1 2 5 7 9) (outer '(5 2 9 1 7))))
   (let* ((inner (timsort/vector/gen (lambda (a b) (abort 'inner)) #t #f #f #t TIMSORT_USE_COMPARATOR))
          (v (vector 3 1 2))
          (outer (timsort/vector/gen (lambda (a b) (handle-exceptions e e (inner v)) (< a b))
                                     #f #f #f #t TIMSORT_USE_COMPARATOR)))
     (⊦= '#(1 2 5 7 9) (outer (vector 5 2 9 1 7)))
     ;; a failed in-place sort writes nothing back
     (⊦= '#(3 1 2) v))
   ;; uncaught, the inner raise still reaches whoever called the outer sort
   (let* ((inner (timsort/gen (lambda (a b) (abort 'inner)) #f #f #f #t TIMSORT_USE_COMPARATOR))
          (outer (timsort/gen (lambda (a b) (inner '(3 1 2)) (< a b)) #f #f #f #t TIMSORT_USE_COMPARATOR)))
     (⊦= 'inner (handle-exceptions e e (outer '(5 2 9 1 7)))))
   ;; a handler that returns resumes a continuable raise in the nested sort's
   ;; comparator, R7RS's and CHICKEN's alike, exactly as in a top-level sort
   (let* ((got '())
          (sorted '())
          (inner (timsort/gen (lambda (a b) (set! got (cons (raise-continuable 'c) got)) (< a b))
                              #f #f #f #t TIMSORT_USE_COMPARATOR))
          (outer (timsort/gen (lambda (a b)
                                (set! sorted (cons (with-exception-handler (lambda (e) 'resumed) (lambda () (inner '(3 1 2))))
                                                   sorted))
                                (< a b))
                              #f #f #f #t TIMSORT_USE_COMPARATOR)))
     (⊦= '(1 2 5 7 9) (outer '(5 2 9 1 7)))
     (⊨ (and (pair? sorted) (every (lambda (l) (equal? l '(1 2 3))) sorted)))
     (⊨ (and (pair? got) (every (lambda (x) (eq? x 'resumed)) got))))
   (let* ((inner (timsort/vector/gen (lambda (a b) (+ 1 (signal 'c)) (< a b)) #f #f #f #t TIMSORT_USE_COMPARATOR))
          (outer (timsort/gen (lambda (a b)
                                (with-exception-handler (lambda (e) 0) (lambda () (inner (vector 3 1 2))))
                                (< a b))
                              #f #f #f #t TIMSORT_USE_COMPARATOR)))
     (⊦= '(1 2 5 7 9) (outer '(5 2 9 1 7))))
   ;; a `guard' whose clauses do not match re-raises, from inside its
   ;; handler, to the next handler out: here another `guard'
   (let* ((seen '())
          (inner (timsort/gen (lambda (a b) (raise 'x)) #f #f #f #t TIMSORT_USE_COMPARATOR))
          (outer (timsort/gen (lambda (a b)
                                (set! seen (cons (guard (e (#t (list 'outer e)))
                                                   (guard (e ((eq? e 'never) 'no)) (inner '(3 1 2))))
                                                 seen))
                                (< a b))
                              #f #f #f #t TIMSORT_USE_COMPARATOR)))
     (⊦= '(1 2 5 7 9) (outer '(5 2 9 1 7)))
     (⊨ (and (pair? seen) (every (lambda (x) (equal? x '(outer x))) seen))))
   ;; A handler reached from a nested sort's comparator is entered once, with
   ;; the handlers around it exactly as in a top-level sort, and what it does
   ;; there -- escaping, raising again, returning -- comes out the same.  So
   ;; each scenario runs at top level, nested in one comparison of an outer
   ;; sort, and both again after a caught top-level raise, which leaves an
   ;; abandoned frame on the runtime's callback continuation stack for good
   ;; and must not make later top-level sorts look nested.
   (let* ((cmp (lambda (lt?) (timsort/gen lt? #f #f #f #t TIMSORT_USE_COMPARATOR)))
          (nested (lambda (thunk)
                    (let ((res 'unset))
                      ((cmp (lambda (a b) (when (eq? res 'unset) (set! res (thunk))) (< a b))) '(3 1 2))
                      res)))
          (message (lambda (e) (if (condition? e) (get-condition-property e 'exn 'message) e)))
          ;; CHICKEN's handler, escaping through a continuation
          (escaping (lambda ()
                      (let ((calls 0))
                        (list (call-with-current-continuation
                                (lambda (k)
                                  (with-exception-handler
                                    (lambda (e) (set! calls (add1 calls)) (k (message e)))
                                    (lambda () ((cmp (lambda (a b) (error "x"))) '(3 1 2))))))
                              calls))))
          ;; R7RS's handler raising a wrapped condition to the `guard' around
          ;; it: it runs with the guard's handler installed, not its own
          (wrapping (lambda ()
                      (let ((calls 0))
                        (guard (e (#t (list e calls)))
                          (r7rs-with-exception-handler
                            (lambda (e) (set! calls (add1 calls)) (if (> calls 5) (raise 'gave-up) (raise (list 'wrapped e))))
                            (lambda () ((cmp (lambda (a b) (raise 'x))) '(3 1 2))))))))
          ;; R7RS's handler returning from a non-continuable raise: the next
          ;; handler out gets the secondary error, the comparator is not resumed
          (returning (lambda ()
                       (guard (e (#t (list 'outer (message e))))
                         (r7rs-with-exception-handler
                           (lambda (e) 'ret)
                           (lambda () ((cmp (lambda (a b) (raise 'x))) '(3 1 2)))))))
          (expected (list '("x" 1) '((wrapped x) 1) '(outer "exception handler returned")))
          (scenarios (list escaping wrapping returning)))
     (⊦= expected (map (lambda (s) (s)) scenarios))
     (⊦= expected (map nested scenarios))
     (⊦= 'caught (handle-exceptions e 'caught ((cmp (lambda (a b) (error "boom"))) '(2 1))))
     (⊦= expected (map (lambda (s) (s)) scenarios))
     (⊦= expected (map nested scenarios)))
   ;; a nested sort's comparator resumed through a continuation captured in
   ;; it, after the sort has answered, gets an error rather than answering
   ;; to a sort that no longer waits for it
   (let* ((msgs '())
          (outer (timsort/gen (lambda (a b)
                                (set! msgs
                                  (cons (handle-exceptions e (get-condition-property e 'exn 'message)
                                          (let* ((saved #f)
                                                 (l ((timsort/gen (lambda (x y)
                                                                    (call-with-current-continuation
                                                                      (lambda (k) (unless saved (set! saved k))))
                                                                    (< x y))
                                                                  #f #f #f #t TIMSORT_USE_COMPARATOR)
                                                     '(3 1 2))))
                                            (if (procedure? saved)
                                                (let ((k saved)) (set! saved 'done) (k #f))
                                                l)))
                                        msgs))
                                (< a b))
                              #f #f #f #t TIMSORT_USE_COMPARATOR)))
     (⊦= '(1 2 5 7 9) (outer '(5 2 9 1 7)))
     (⊨ (and (pair? msgs)
             (every (lambda (m) (equal? m "a comparator was resumed after it had answered or its nested sort had failed"))
                    msgs))))
   (⊦= 0 (timsort-depth))
   (⊦= '(1 2 3) ((timsort/gen < #f #f #f #t TIMSORT_USE_COMPARATOR) '(3 1 2))))

  ;; A NESTED sort's comparator that escapes through a continuation captured
  ;; in the enclosing comparator, with no raise involved.  Escaping through
  ;; the inner sort's C frames, it panicked the process with "callback
  ;; returned twice" when the outer callback returned.
  ((test/nested-escape _)
   (let* ((got '())
          (winds '())
          (outer (timsort/gen (lambda (a b)
                                (set! got
                                  (cons (call-with-current-continuation
                                          (lambda (k)
                                            (dynamic-wind
                                              (lambda () (set! winds (cons 'in winds)))
                                              (lambda ()
                                                ((timsort/gen (lambda (x y)
                                                                (dynamic-wind
                                                                  (lambda () (set! winds (cons 'cin winds)))
                                                                  (lambda () (k 'esc))
                                                                  (lambda () (set! winds (cons 'cout winds)))))
                                                              #f #f #f #t TIMSORT_USE_COMPARATOR)
                                                 '(3 1 2)))
                                              (lambda () (set! winds (cons 'out winds))))))
                                        got))
                                (< a b))
                              #f #f #f #t TIMSORT_USE_COMPARATOR)))
     (⊦= '(1 2 5 7 9) (outer '(5 2 9 1 7)))
     (⊨ (and (pair? got) (every (lambda (x) (eq? x 'esc)) got)))
     ;; every wind of the caller's, inside the inner comparator or around the
     ;; inner sort, ran once per escape
     (⊦= (map (lambda (w) (length got)) '(in cin cout out))
         (map (lambda (w) (count (lambda (x) (eq? x w)) winds)) '(in cin cout out))))
   ;; three sorts deep, escaping from the innermost comparator to the outermost
   (let ((outer (timsort/gen (lambda (a b)
                               (call-with-current-continuation
                                 (lambda (k)
                                   ((timsort/gen (lambda (c d)
                                                   ((timsort/gen (lambda (e f) (k 'deep)) #f #f #f #t TIMSORT_USE_COMPARATOR)
                                                    '(3 1 2))
                                                   (< c d))
                                                 #f #f #f #t TIMSORT_USE_COMPARATOR)
                                    '(9 8 7))))
                               (< a b))
                             #f #f #f #t TIMSORT_USE_COMPARATOR)))
     (⊦= '(1 2 5 7 9) (outer '(5 2 9 1 7))))
   ;; a failed in-place inner sort writes nothing back
   (let* ((v (vector 3 1 2))
          (outer (timsort/gen (lambda (a b)
                                (call-with-current-continuation
                                  (lambda (k)
                                    ((timsort/vector/gen (lambda (x y) (k 0)) #t #f #f #t TIMSORT_USE_COMPARATOR) v)))
                                (< a b))
                              #f #f #f #t TIMSORT_USE_COMPARATOR)))
     (⊦= '(1 2 5 7 9) (outer '(5 2 9 1 7)))
     (⊦= '#(3 1 2) v))
   ;; an escape past the outer sort as well reaches its target
   (⊦= 'out
       (call-with-current-continuation
         (lambda (k)
           ((timsort/gen (lambda (a b)
                           ((timsort/gen (lambda (x y) (k 'out)) #f #f #f #t TIMSORT_USE_COMPARATOR) '(3 1 2))
                           (< a b))
                         #f #f #f #t TIMSORT_USE_COMPARATOR)
            '(5 2 9 1 7)))))
   (⊦= 0 (timsort-depth))
   (⊦= '(1 2 3) ((timsort/gen < #f #f #f #t TIMSORT_USE_COMPARATOR) '(3 1 2))))

  )

(unittest/✓ timsort-suite)