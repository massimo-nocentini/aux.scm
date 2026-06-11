

(module (aux category identity) *

  (import scheme (chicken base) (aux base))

  ; Monad instance
  (define (return v) v)
  (define (>>= m f) (f m))
  (define (fail . args) (error "Identity monad does not support failure" args))

)

(import (aux category monad))

(module (aux category monad identity) = ((aux category monad) (aux category identity)))

