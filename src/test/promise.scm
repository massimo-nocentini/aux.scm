
(import (only srfi-1 car+cdr) (aux unittest) (aux base))

(define-suite promise-suite

  ; from on-scheme promise-test, group "`delay` and `make-promise` syntactic and functional
  ; abstractions, respectively".
  ((test/delay-and-make-promise _)
   (let* ((count (let1 (counter 0)
                   (λ ()
                     (set! counter (add1 counter))
                     counter)))
          (delayed_count (delay (count))))
     (⊦= 1 (force delayed_count))
     (⊦= 1 (force delayed_count))
     (⊦= 1 (force delayed_count))
     (⊦= 1 (force delayed_count))
     (⊦= 1 (force delayed_count))
     (let1 (promised_count (make-promise (count)))   ; `make-promise` evaluates its argument now
       (⊦= 2 (force promised_count))
       (⊦= 2 (force promised_count))
       (let1 (promised_ccount (make-promise (count)))
         (⊦= 2 (force promised_count))
         (⊦= 3 (force promised_ccount))
         (⊦= 4 (count))))))

  ; from on-scheme learning-test, group DELAY-FORCE.
  ((test/promise? _)
   (⊨ (promise? (delay (+ 3 4))))
   (⊨ (promise? (delay-force (+ 3 4)))))

  ((test/delay-force-filter _)
   ; `stream-filter/stackfull` is defined, as in the original, but never run: it would use
   ; unbounded memory because of `(delay (force ...))`.
   (letrec ((stream-filter/tailcall (λ (p? s)
                                      (delay-force
                                        (let1 (s-mature (force s))
                                          (if (null? s-mature)
                                            (delay '())
                                            (let-values (((h t) (car+cdr s-mature)))
                                              (if (p? h)
                                                (delay (cons h (stream-filter/tailcall p? t)))
                                                (stream-filter/tailcall p? t))))))))
            (stream-filter/stackfull (λ (p? s)
                                       (delay
                                         (force
                                           (let1 (s-mature (force s))
                                             (if (null? s-mature)
                                               (delay '())
                                               (let-values (((h t) (car+cdr s-mature)))
                                                 (if (p? h)
                                                   (delay (cons h (stream-filter/stackfull p? t)))
                                                   (stream-filter/stackfull p? t)))))))))
            (from (λ (n)
                    (delay-force (cons n (from (+ n 1))))))
            (large-number 10000))
     (⊦= large-number (car (force (stream-filter/tailcall
                                    (λ (n) (= n large-number))
                                    (from 0)))))))

  ((test/force-non-promises _)
   (⊨ (procedure? (force (τ 3))))
   (⊦= '3 (force (make-promise 3))))

  )

(unittest/✓ promise-suite)
