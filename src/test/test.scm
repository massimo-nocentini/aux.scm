
(import unittest aux)

(define-sut auxtest

    ((test/let1 _)  
        (let1 (a 1) (⊦= a 1)))

    ((test/letcar&cdr _) 
        (letcar&cdr (((a d) (cons 1 '()))
                     ((aa dd) (cons 2 3)))
            (⊦= a 1) (⊦= d '()) (⊦= aa 2) (⊦= dd 3)))

    #;(end))

(unittest/✓-sut auxtest)