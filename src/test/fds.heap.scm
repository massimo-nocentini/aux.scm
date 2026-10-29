; Tests of (aux fds heap), the persistent pairing heap.

(import (aux unittest) (aux base) (aux fds heap)
        (chicken random) (chicken bytevector) (chicken sort) (chicken condition)
        (only srfi-1 list-tabulate))

; the message of the error raised by thunk, or 'not-raised: like ⊦raises it requires a condition, and it also exposes the message
(define (error-message thunk)
  (condition-case (begin (thunk) 'not-raised)
    (e (exn) ((condition-property-accessor 'exn 'message) e))))

; the order of (aux search) frontiers: by priority, then by serial number.
(define (entry<? a b)
  (or (< (vector-ref a 0) (vector-ref b 0))
      (and (= (vector-ref a 0) (vector-ref b 0)) (< (vector-ref a 1) (vector-ref b 1)))))

(define-suite heap-suite

  ((test-empty _)
   (let ((h (make-fds-heap <)))
     (⊨ (fds-heap? h))
     (⊭ (fds-heap? '()))
     (⊨ (fds-heap-empty? h))
     (⊦= 0 (fds-heap-size h))
     (⊨ (eq? < (fds-heap-less? h)))
     (⊦= '() (fds-heap->list h))
     (⊦= "fds-heap-min: empty heap" (error-message (λ () (fds-heap-min h))))
     (⊦= "fds-heap-delete-min: empty heap" (error-message (λ () (fds-heap-delete-min h))))
     (⊦= "fds-heap-pop: empty heap" (error-message (λ () (fds-heap-pop h))))
     (⊦= "fds-heap-min: empty heap"
         (error-message (λ () (fds-heap-min (fds-heap-delete-min (fds-heap-insert 1 h))))))))

  ((test-sort _)
   (⊦= '(1 2 3 5 7 8 9) (fds-heap->list (list->fds-heap < '(5 3 8 1 9 2 7))))
   (⊦= '(1 1 2 2) (fds-heap->list (list->fds-heap < '(2 1 2 1))))
   (⊦= '(9 8 7 5 3 2 1) (fds-heap->list (list->fds-heap > '(5 3 8 1 9 2 7))))
   (let ((h (list->fds-heap < '(5 3 8 1 9 2 7))))
     (⊦= 7 (fds-heap-size h))
     (⊦= 1 (fds-heap-min h))
     (let-values (((m rest) (fds-heap-pop h)))
       (⊦= 1 m)
       (⊦= 6 (fds-heap-size rest))
       (⊦= 2 (fds-heap-min rest)))))

  ((test-heap-sort-property _)
   (set-pseudo-random-seed! (string->utf8 "aux-fds-heap"))
   (do ((i 0 (add1 i))) ((= i 200))
     (let ((lst (list-tabulate (pseudo-random-integer 301) (λ (_) (pseudo-random-integer 1000)))))
       (let ((h (list->fds-heap < lst)))
         (⊦= (length lst) (fds-heap-size h))
         (⊦= (sort lst <) (fds-heap->list h))))))

  ((test-persistence _)
   (let* ((h₀ (list->fds-heap < '(6 2 9 4)))
          (h₁ (fds-heap-insert 4 h₀)))
     (⊦= '(2 4 6 9) (fds-heap->list h₀))
     (⊦= '(2 4 4 6 9) (fds-heap->list h₁))
     (⊦= 4 (fds-heap-size h₀))
     (⊦= 5 (fds-heap-size h₁))
     (let ((a (fds-heap-delete-min h₁))
           (b (fds-heap-delete-min h₁)))
       (⊦= (fds-heap->list a) (fds-heap->list b))
       (⊦= '(4 4 6 9) (fds-heap->list a))
       (⊦= '(2 4 4 6 9) (fds-heap->list h₁)))))

  ((test-merge _)
   (let ((h (fds-heap-merge (list->fds-heap < '(1 4 7)) (list->fds-heap < '(2 5 8)))))
     (⊦= '(1 2 4 5 7 8) (fds-heap->list h))
     (⊦= 6 (fds-heap-size h)))
   (⊦= '(3) (fds-heap->list (fds-heap-merge (make-fds-heap <) (list->fds-heap < '(3)))))
   (⊦= "fds-heap-merge: different orders"
       (error-message (λ () (fds-heap-merge (list->fds-heap < '(1)) (list->fds-heap > '(2)))))))

  ((test-entries _)
   ; equal priorities pop in serial order, whatever the insertion order
   (let ((h (list->fds-heap entry<? (list #(3 0 a) #(1 4 b) #(1 2 c) #(2 1 d) #(1 3 e) #(3 5 f)))))
     (⊦= '(c e b d a f) (map (λ (e) (vector-ref e 2)) (fds-heap->list h)))))

  )

(unittest/✓ heap-suite)
