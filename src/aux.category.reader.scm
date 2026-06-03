

(module (aux category reader) *

  (import scheme (chicken base) (aux base))

  #|

  instance Monad ((->) r) where
    return x = \_ -> x
    h >>= f = \w -> f (h w) w

  |#

  ; Monad instance
  (define (return v) (λ (v*) v))

  (define (>>= m f)
    (λ (v)
      (let* ((v* (m v))
             (f* (f v*)))
        (f* v))))

  (define (fail . args) (λ (v) (error "Reader monad does not support failure" `((on ,v) (args . ,args)))))

)

(import (aux category monad))

(module (aux category monad reader) = ((aux category monad) (aux category reader)))

#|

(import scheme (chicken base) (aux category reader) (aux category monad reader))

(define combine-stuff 
  (do/monad
    (let a (λ (v) (* 2 v)))
    (let b (λ (v) (+ 10 v)))
    (return (+ a b))))

(combine-stuff 3)
(combine-stuff 5)

|#