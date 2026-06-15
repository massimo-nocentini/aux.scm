

(module (aux category ziplist) *

  (import scheme (chicken base) (aux base))

  ; Monad instance
  (define (return x) (list/circular x))
  (define (>>= m g)
    (match/first m
      ((,l . ,,m) (g l))
      (else (reverse (cdr (foldl  (λ-match/first
                                    (((,i . ,acc) ,v) (let1 (m* (g v))
                                                        (cond
                                                          ((length/>? m* i) => (λ (v*) (cons (add1 i) (cons v* acc))))
                                                          (else (cons (add1 i) acc))))))
                    `(0 . ()) m))))))

  (define (fail . args) '())
)

#|

(import (aux category monad))

(module (aux category monad ziplist) = ((aux category monad) (aux category ziplist)))

(import (aux category monad ziplist))

(do/monad)

(do/monad
  (← x '(1 2 3))
  (← y '(a b))
  (← z '(c c*))
  (← r '(d e f g))
  ,(list x y z r))

(<*> (list add1 sub1 sub1) (list 1 2 3)) ; => (2 3 4)

|#