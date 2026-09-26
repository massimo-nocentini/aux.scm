

(module (aux category ziplist) *

  (import scheme (chicken base) (aux base))

  ; the pair at index i of lst, #f when lst is shorter: a pair is never #f, so a
  ; stored #f is not mistaken for absence (the witness of `length/>?` is that
  ; same tail pair). It stops at i, so it terminates on the circular lists built by `return`.
  (define (list-tail/or-false lst i)
    (cond
      ((null? lst) #f)
      ((zero? i) lst)
      (else (list-tail/or-false (cdr lst) (sub1 i)))))

  ; Monad instance
  (define (return x) (list/circular x))
  (define (>>= m g)
    (match/first m
      ((,l . ,,m) (g l))
      (else (reverse (cdr (foldl  (λ-match/first
                                    (((,i . ,acc) ,v) (let ((m* (g v))
                                                            (i* (add1 i)))
                                                        (cond
                                                          ((list-tail/or-false m* i) => (λ (t) (cons i* (cons (car t) acc))))
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
