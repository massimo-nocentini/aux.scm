

(module (aux category list) *

  (import scheme (chicken base) (only srfi-1 append-map) (aux base))

  ; Monad instance
  (define return list)
  (define (>>= m f) (append-map f m))
  (define (fail . args) '())

  ; Monoid instance
  (define mempty '())
  (define (mappend l1 l2) (append l1 l2))

  (define (mlog x) `((got ,x)))

  (define fmap map)

  (define (<*> flst lst) (append-map (λ (f) (map f lst)) flst))

)

(import (aux category monad) (aux category writer))

(module (aux category monad list) = ((aux category monad) (aux category list)))
(module (aux category writer list) = ((aux category writer) (aux category list)))
(module (aux category monad writer list) = ((aux category monad) (aux category writer list)))