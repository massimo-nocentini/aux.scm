

(module (aux category list) *

  (import scheme (chicken base) (only srfi-1 append-map) (aux base))

  ; Monad instance
  (define return list)
  (define (>>= m f) (append-map f m))
  (define (fail . args) '())

  ; Monoid instance
  (define mempty '())
  (define (mappend l1 l2) (append l1 l2))

)
