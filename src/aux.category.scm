
(functor ((aux category monad) (M (return >>= fail)))

  (do/monad >> return >>= fail)

  #|

  class Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b
    (>>) :: m a -> m b -> m b
    x >> y = x >>= \_ -> y
    fail :: String -> m a
    fail msg = error msg

  |#

  (import
    scheme 
    (chicken base)
    (aux base)
    M)

  (define-syntax do/monad
    (syntax-rules (let let*)
      ((do/monad) (fail (void)))
      ((do/monad (let var expr) body body* ...) (>>= expr (λ (var) (do/monad body body* ...))))
      ((do/monad (let* var expr ...) body body* ...) (do/monad (let var (return expr ...)) body body* ...))
      ((do/monad (let* expr ...)) (return expr ...))
      ((do/monad expr) expr)
      ((do/monad expr body ...) (>>= expr (λ (_) (do/monad body ...))))))

  (define (>> m1 m2) (>>= m1 (λ (_) m2)))

)

(module (aux category list) *

  (import scheme (chicken base) srfi-1 (aux base))

  (define return list)
  (define (>>= m f) (append-map f m))
  (define (fail . args) '())

)

(module (aux category maybe) *

  (import scheme (chicken base) (aux base))

  (define (return . args) `(just . ,(car args)))
  (define (>>= m f)
    (match/first m
      ((just . ,v) (f v))
      (else '())))
  (define (fail . args) '())

)

(module (aux category monad list) = ((aux category monad) (aux category list)))
(module (aux category monad maybe) = ((aux category monad) (aux category maybe)))

(import (aux category monad list))

(do/monad
  (let x (return 1))
  (let y (return x))
  (return (+ x y)))

(do/monad
  (let* x 1)
  (let* y 2)
  (let* x y))

(do/monad
  (let x (list 3 4 5))
  (let y (list x (- x)))
  (return (+ x y)))

(do/monad
  (let* x 3 4 5)
  (let* y x (- x))
  (let* (+ x y) (- x y)))

(do/monad
  (let x (list 1 2))
  (let y (list 'a 'b))
  (let* (list x y) (list y x) 'separator))

(do/monad
  (let x (ι 10))
  (let y (ι 10))
  (let* (list x y)))

(do/monad
  (let x (ι 10))
  (let y (ι 10))
  (let* x y))

(import (aux category monad maybe))

(do/monad
  (let* x 3 4)
  (let* y (- x))
  (let* y))

