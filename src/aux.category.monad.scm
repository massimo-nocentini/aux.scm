
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

  (define-syntax ▷ 
    (syntax-rules ()
      ((▷ p) (M:>>= p M:return))
      ((▷ p q) (M:>>= p q))
      ((▷ p q r ...) (▷ p (λ (x) (▷ (q x) r ...))))))

  (define-syntax do/monad
    (syntax-rules (let match unquote)
      ((do/monad (let var (unquote expr)) body body* ...) (do/monad (let var (M:return expr)) body body* ...))
      ((do/monad (let var expr) body body* ...) (▷ expr (λ (var) (do/monad body body* ...))))
      ((do/monad (match pat (unquote expr)) body body* ...) (do/monad (match pat (M:return expr)) body body* ...))
      ((do/monad (match pat expr) body body* ...) (▷ expr
                                                    (λ1-match/first
                                                      (pat (do/monad body body* ...))
                                                      (else (do/monad)))))
      ((do/monad (unquote expr)) (do/monad (M:return expr)))
      ((do/monad (unquote expr) body ...) (do/monad (let _ (unquote expr)) body ...))
      ((do/monad expr) expr)
      ((do/monad expr body ...) (do/monad (let _ expr) body ...))
      ((do/monad) (M:fail (void)))))

  (define (>> m1 m2) (▷ m1 (λ (_) m2)))

  (define (fmap f mx) 
    (do/monad
      (let x mx)
      ,(f x)))

  (define (<*> mf mx)
    (do/monad
      (let f mf)
      (let x mx)
      ,(f x)))

  (define-syntax-rule (◇ f f* ...) (λ (x) (▷ (f x) f* ...))) ; Kleisli composition

  (define (concat mm)
    (do/monad
      (let m mm)
      m))

)

#|

(import (aux category list))
(import (aux category monad list))

(<*> (list add1 sub1) (list 1 2 3))

(concat (list (list 1 2) (list 3 4) (list 5)))

(fmap add1 (list 1 2 3))

(do/monad
  (let x ,1)
  (let y ,x)
  ,(+ x y))

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
  (let x ,(ι 10))
  (let y ,(ι 10))
  (list x y))

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