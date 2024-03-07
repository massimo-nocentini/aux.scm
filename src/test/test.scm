
(import unittest aux)

(define-sut auxtest

    ((test/let1 _)
        (let1 (a 1) (⊦= a 1)))

    ((test/letcar&cdr _)
        (letcar&cdr (((a d) (cons 1 '()))
                     ((aa dd) (cons 2 3)))
            (⊦= a 1) (⊦= d '()) (⊦= aa 2) (⊦= dd 3)))

    ((test/trycc _)
        (⊦= 3 (trycc ⤶
                (v (+ 1 (⤶ 1)))
                (w (+ 2 v))
                (else w)))
        (⊦= 1 (trycc ⤶
                (v (+ 1 (⤶ 1)))
                (w (+ 2 (⤶ v)))
                (else w)))
        (⊦= 3 (trycc ⤶
                (v (+ 2 (⤶ 2)))
                (else (⤶ (+ 1 v))))))

    #;(end))

(unittest/✓-sut auxtest)