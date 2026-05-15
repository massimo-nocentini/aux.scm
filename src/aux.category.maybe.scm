
(module (aux category maybe) *

  (import scheme (chicken base) (aux base))

  (define (return . args) `(just . ,(car args)))
  (define (>>= m f)
    (match/first m
      ((just . ,v) (f v))
      (else '())))
  (define (fail . args) '())

)

(import (aux category monad))

(module (aux category monad maybe) = ((aux category monad) (aux category maybe)))

