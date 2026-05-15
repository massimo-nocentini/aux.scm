
(functor ((aux category monad) (M (return >>= fail)))

  *

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
    (prefix M M:))

  (define-syntax do/monad
    (syntax-rules (let match)
      ((do/monad) (M:fail (void)))
      ((do/monad (match pat expr) body body* ...) (M:>>= expr (λ1-match/first
                                                                (pat (do/monad body body* ...))
                                                                (else (do/monad)))))
      ((do/monad (let var expr) body body* ...) (M:>>= expr (λ (var) (do/monad body body* ...))))
      ((do/monad expr) expr)
      ((do/monad expr body body* ...) (do/monad (let _ expr) body body* ...))))

  (define (>> m1 m2) (M:>>= m1 (λ (_) m2)))

)

#|

(import (aux category list))
(import (aux category monad list))

(do/monad
  (let x (return 1))
  (let y (return x))
  (return (+ x y)))

(do/monad
  (match* ,x 1)
  (match* ,y 2)
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

|#