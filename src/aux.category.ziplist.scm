

(module (aux category ziplist) *

  (import scheme (chicken base) (aux base))

  ; Monad instance
  (define (return x) (list/circular x))
  (define (>>= m g)
    (match/first m
      ((,l . ,,m) (g l))
      (else (reverse (cdr (foldl  (λ-match/first
                                    (((,i . ,acc) ,v) (let ((m* (g v))
                                                            (i* (add1 i)))
                                                        (cond
                                                          ((length/>? m* i) => (λ (v*) (cons i* (cons v* acc))))
                                                          (else (cons i* acc))))))
                    `(0 . ()) m))))))

  (define (fail . args) '())
)

(import (aux category monad))

(module (aux category monad ziplist) = ((aux category monad) (aux category ziplist)))

#|
(import  (aux category ziplist) (aux category monad ziplist))

(define l (do/monad
  (← x ,3)
  (← y ,4)
  (← z ,5)
  ,(+ x y z)))

  l

(do/monad
  (← x '(1 2 3))
  (← y '(a b))
  (← z '(c c*))
  (← r '(d e f g))
  ,(list x y z r))

(<*> (list add1 sub1 sub1) (list 1 2 3)) ; => (2 1 2)

|#
