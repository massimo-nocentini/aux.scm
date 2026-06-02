
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

  (define (map/monad f mx)
    (do/monad
      (let x mx)
      (M:return (f x))))

  (define (<*> mf mx)
    (do/monad
      (let f mf)
      (let x mx)
      (M:return (f x))))

  (define-syntax-rule (◇ f f* ...) (λ (x) (▷ (f x) f* ...))) ; Kleisli composition

  ; flatten/monad :: (Monad m) => m (m a) -> m a
  (define (flatten/monad mm)
    (do/monad
      (let m mm)
      m))

  ; list->monad :: (Monad m) => [a] -> m [a]
  (define (list->monad lst)
    (match/first lst
      (() (M:return '()))
      ((,x . ,xs) (do/monad 
                    (let xs* (list->monad xs))
                    (M:return (cons x xs*))))))

  ; monoids->monad :: (Monoid n), (Monad m) => [n] -> m n
  (define (monoids->monad mempty mappend)
    (letrec ((R (λ (lst) 
                  (match/first lst
                    (() (M:return mempty))
                    ((,x . ,xs) (do/monad 
                                  (let x* (R xs))
                                  (M:return (mappend x x*))))))))
      R))

  ; foldM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
  (define (fold/monad f init lst)
    (match/first lst
      (() (M:return init))
      ((,x . ,xs) (do/monad
                    (let acc (f init x))
                    (foldM f acc xs)))))

  ; filterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
  (define (filter/monad f lst)
    (match/first lst
      (() (M:return '()))
      ((,x . ,xs) (do/monad
                    (let b (f x))
                    (if b
                      (do/monad 
                        (let xs* (filterM f xs))
                        (M:return (cons x xs*)))
                      (filterM f xs))))))

)

#|

(import (aux category list))
(import (aux category monad list))

(foldM (λ (acc x) (list (+ acc x) (- acc x))) 0 '(1 2 3 4 5))

(list->monad '(1 2 3))
(define (powerset lst) (filterM (λ (_) (list #t #f)) lst))

(powerset '(1 2 3 4 5 6 7 8 9 10))

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

(callcc
(do/monad
  (let x ,(ι 10))
  (let y ,(ι 10))
  ,(list x y))
)

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