

(module (aux category list) *

  (import scheme (chicken base) (only srfi-1 append-map) (aux base))

  ; Monad instance
  (define (return v) (list v))
  (define (>>= m f) (append-map f m))
  (define (fail . args) '())

  ; Monoid instance
  (define mempty '())
  (define (mappend l1 l2) (append l1 l2))

  (define (mlog x) (list x))

  (define map/functor map)

  (define (map/applicative flst lst) (append-map (λ (f) (map f lst)) flst))

  (define ⊕₀/monad '())
  (define (⊕/monad m1 m2) (append m1 m2))
)

(import (aux category monad) (aux category monad plus) (aux category writer))

(module (aux category monad list) = ((aux category monad) (aux category list)))
(module (aux category monad plus list) = ((aux category monad plus) (aux category monad list) (aux category list)))
(module (aux category writer list) = ((aux category writer) (aux category list)))
(module (aux category monad writer list) = ((aux category monad) (aux category writer list)))