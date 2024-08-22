
(import unittest aux)

(define-suite auxtest

    ((test/let1 _)
        (let1 (a 1) (⊦= a 1)))

    ((test/letcar&cdr _)
        (letcar&cdr (((a d) (cons 1 '()))
                     ((aa dd) (cons 2 3)))
            (⊦= a 1) (⊦= d '()) (⊦= aa 2) (⊦= dd 3)))

    ((test/letcc/multiarg _)
        (⊦= 'a (letcc k (k 'a))))

    ((test/letcc* _)
        (⊦= 3 (letcc* ⤶ ((v (+ 1 (⤶ 1)))
                         (w (+ 2 v)))
                (+ 4 w)))
        (⊦= 5 (letcc* ⤶ ((v (+ 1 (⤶ 1)))
                         (w (+ 2 (⤶ (+ 3 v)))))
                (+ 1 w)))
        (⊦= 3 (letcc* ⤶ ((v (+ 2 (⤶ 2))))
                (+ 1 v)))
        (⊦= 3 (letcc* ⤶ ((v (+ 2 (⤶ 2))))
                (⤶ (+ 1 v))))
        (⊦= 2 (letcc* ⤶ ((v (+ 2 (⤶ 2))))
                (+ 1 (⤶ v))))
        (⊦= 6 (letcc* ⤶ ((v (+ 2 (⤶ 2))))
                (+ 1 (⤶ (+ 4 v))))))

    ((test/trycc1 _)
        (⊦= 5 (trycc 
                (✗ 
                  (+ 1 (✗))
                  (+ 2 3))
                (cons 3 '()))))

    ((test/trycc2 _)
        (⊦= 3 (trycc 
                (✗
                  (+ 1 2)
                  (+ 2 (✗)))
                (cons 3 '()))))

    ((test/trycc3 _)
        (⊦= '(3) (trycc 
                   (✗
                      (+ 1 (✗))
                      (+ 2 (✗)))
                   (cons 3 '()))))

    ((test/λ _)
        (⊦= 5 ((λ (x) (+ x 2)) 3))
        (⊦= 5 ((λ (x) ((λ (y) (+ x y)) 2)) 3))
        (⊦= 5 ((λ (x y) (+ x y)) 2 3)))

    ((test/letmap _)
        (⊦= '((((2 a #t) (2 a #t)) ((2 1 #t) (2 1 #f)))
              (((3 a #f) (3 a #t)) ((3 2 #f) (3 2 #f)))
              (((4 a #t) (4 a #t)) ((4 3 #t) (4 3 #f))))
          (letmap ((x (list 1 2 3))
                   (y `(a ,x))
                   (z (list (odd? x) (symbol? y))))
            (list (add1 x) y z))))
         
)

(unittest/✓ auxtest)