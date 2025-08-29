
(import unittest (aux base) (chicken sort))

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




















