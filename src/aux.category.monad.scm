
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

  (define-many (return >>= fail) (M:return M:>>= M:fail))

  (define-syntax do/monad
    (syntax-rules (let match unquote)
      ((do/monad (let var (unquote expr)) body body* ...) (do/monad (let var (M:return expr)) body body* ...))
      ((do/monad (let var expr) body body* ...) (M:>>= expr (λ (var) (do/monad body body* ...))))
      ((do/monad (match pat (unquote expr)) body body* ...) (do/monad (match pat (M:return expr)) body body* ...))
      ((do/monad (match pat expr) body body* ...) (M:>>= expr (λ1-match/first
                                                                (pat (do/monad body body* ...))
                                                                (else (do/monad)))))
      ((do/monad (unquote expr)) (M:return expr))
      ((do/monad expr) expr)
      ((do/monad) (M:fail (void)))))

  (define (>> m1 m2) (M:>>= m1 (λ (_) m2)))

)

#|

(import (aux category))
(import (aux category list))
(import (aux category monad list))

(do/monad
  (let x (return 1))
  (let y (return x))
  (return (+ x y)))

(do/monad
  (match ,x ,1)
  (match ,y ,2)
  ,(list x y))

(do/monad
  (let x (list 3 4 5))
  (let y (list x (- x)))
  ,(+ x y))

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