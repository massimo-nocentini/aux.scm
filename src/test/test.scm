
(import unittest aux (chicken sort))

(define-suite auxtest

  ((test/let1 _)
   (let1 (a 1) (⊦= a 1)))

  ((test/letcar&cdr _)
   (letcar&cdr (((a d) (cons 1 '()))
                ((aa dd) (cons 2 3)))
               (⊦= a 1) (⊦= d '()) (⊦= aa 2) (⊦= dd 3)))

  ((test/λ _)
   (⊦= 5 ((λ (x) (+ x 2)) 3))
   (⊦= 5 ((λ (x) ((λ (y) (+ x y)) 2)) 3))
   (⊦= 5 ((λ (x y) (+ x y)) 2 3)))

  ((test/letmaptensor _)
   (⊦= '((((2 a #t) (2 a #t)) ((2 1 #t) (2 1 #f)))
           (((3 a #f) (3 a #t)) ((3 2 #f) (3 2 #f)))
           (((4 a #t) (4 a #t)) ((4 3 #t) (4 3 #f))))
         (letmaptensor ((x (list 1 2 3))
                        (y `(a ,x))
                        (z (list (odd? x) (symbol? y))))
                       (list (add1 x) y z))))

  ((test/letmap _)
   (⊦= '((2 a #t)
           (2 a #t)
           (2 1 #t)
           (2 1 #f)
           (3 a #f)
           (3 a #t)
           (3 2 #f)
           (3 2 #f)
           (4 a #t)
           (4 a #t)
           (4 3 #t)
           (4 3 #f))
         (letmap ((x (list 1 2 3))
                  (y `(a ,x))
                  (z (list (odd? x) (symbol? y))))
                 (list (add1 x) y z))))

  ((test/stream/nats _)
   (let1 (nats (take§ 20 (nats§ 0)))
         (⊦= '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19) (§->list nats))))

  ((test/stream/fibs _)
   (let1 (fibs (take§ 20 (fibs§ 0 1)))
         (⊦= '(0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181) (§->list fibs))))

  ((test/stream/ones _)
   (let1 (ones (take§ 20 (const§ 1)))
         (⊦= '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) (§->list ones))))

  ((test/stream/primes _)
   (let1 (primes (take§ 20 primes§))
         (⊦= '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71) (§->list primes))))

  ((test/stream/ones+τ _)
   (let1 (ones (take§ 20 (thunk§ 1)))
         (⊦= '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) (§->list ones))))

  ((test/procc/λ-memo _)

   (define count 0)
   (define fib (λ (n) 
                   (add1! count)
                   (cond
                     ((< n 2) n) 
                     (else (let1 (m (sub1 n)) 
                                 (+ (fib m) (fib (sub1 m))))))))
   (let1 (v (fib 10))
         (⊦= 55 v)
         (⊦= 177 count))

   (set! count 0)

   (define fib-memo
     (λ-memo (n) 
              (add1! count)
              (cond
                ((< n 2) n) 
                (else (let1 (m (sub1 n)) 
                            (+ (fib-memo m) (fib-memo (sub1 m))))))))
   (let1 (v (fib-memo 10))
         (⊦= 55 v)
         (⊦= 11 count)))

  )

(unittest/✓ auxtest)






