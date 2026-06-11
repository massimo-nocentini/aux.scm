
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

  (import-for-syntax (aux base))

  (define-syntax ▷ 
    (syntax-rules ()
      ((▷ p) (▷ p M:return))
      ((▷ p q) (M:>>= p q))
      ((▷ p q r ...) (▷ p (λ (x) (▷ (q x) r ...))))))

  (define-syntax do/monad
    (syntax-rules (let ← unquote)
      ((do/monad (← (pat ...) (unquote expr)) body body* ...) (do/monad (← (pat ...) (M:return expr)) body body* ...))
      ((do/monad (← (pat ...) expr) body body* ...) (▷ expr (λ1-match/first ((pat ...) (do/monad body body* ...)))))
      ((do/monad (← var (unquote expr)) body body* ...) (do/monad (← var (M:return expr)) body body* ...))
      ((do/monad (← var expr) body body* ...) (▷ expr (λ (var) (do/monad body body* ...))))
      ((do/monad (let (p ...) expr) body body* ...) (match/first (((p ...) expr) (do/monad body body* ...))))
      ((do/monad (let var expr) body body* ...) (let1 (var expr) (do/monad body body* ...)))
      ((do/monad (unquote expr)) (do/monad (M:return expr)))
      ((do/monad (unquote expr) body ...) (do/monad (← _ (unquote expr)) body ...))
      ((do/monad expr) expr)
      ((do/monad expr body ...) (do/monad (← _ expr) body ...))
      ((do/monad) (M:fail (void)))))

  (define (>> m1 m2) (▷ m1 (λ (_) m2)))

  (define (map/monad f mx)
    (do/monad
      (← x mx)
      ,(f x)))

  (define-macro-ir (lift/monad expr inject compare)
    (let* ((f (cadr expr))
           (ms (cddr expr))
           (vars '())
           (ss (map (lambda (m) (let ((v (gensym))) (set! vars (cons v vars)) `(← ,v ,m))) ms))
           (f* (list 'unquote (cons f (reverse vars)))))
      `(do/monad ,@ss ,f*)))

  (define (<*> mf mx)
    (do/monad
      (← f mf)
      (← x mx)
      ,(f x)))
  
  (define (<$> f m) (<*> (M:return f) m))

  (define (o/monad . f*) (λ (x) (foldr (λ (f m) (▷ m f)) (M:return x) f*))) ; Kleisli composition
  (define (◇ . f*) (o/monad (reverse f*))) ; Reverse Kleisli composition

  ; flatten/monad :: (Monad m) => m (m a) -> m a
  (define (flatten/monad mm)
    (do/monad
      (← m mm)
      m))

  ; list->monad :: (Monad m) => [a] -> m [a]
  (define (list->monad lst)
    (match/first lst
      (() (M:return '()))
      ((,x . ,xs) (do/monad 
                    (← xs* (list->monad xs))
                    ,(cons x xs*)))))

  ; monoids->monad :: (Monoid n), (Monad m) => [n] -> m n
  (define (monoids->monad mempty mappend)
    (letrec ((R (λ (lst) 
                  (match/first lst
                    (() (M:return mempty))
                    ((,x . ,xs) (do/monad 
                                  (← x* (R xs))
                                  ,(mappend x x*)))))))
      R))

  ; foldM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
  (define (fold/monad f init lst)
    (match/first lst
      (() (M:return init))
      ((,x . ,xs) (do/monad
                    (← acc (f init x))
                    (fold/monad f acc xs)))))

  ; filterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
  (define (filter/monad f lst)
    (match/first lst
      (() (M:return '()))
      ((,x . ,xs) (do/monad
                    (← b (f x))
                    (if b
                      (do/monad (← xs* (filter/monad f xs)) ,(cons x xs*))
                      (filter/monad f xs))))))

  ; zip :: (Monad m) => m a -> m b -> m (a, b)
  (define (zip/monad m1 m2) (lift/monad cons m1 m2))
  (define (zip-with/monad f m1 m2) (lift/monad f m1 m2))

  ; fork :: (a -> m b, a -> m c) -> m a -> m (b, c)
  (define ((fork/monad f g) m)
    (do/monad
      (← x m)
      (← y (f x))
      (← z (g x))
      ,(cons y z)))

  ; cross :: (Monad m) => (a -> m c) -> (b -> m d) -> m (a, b) -> m (c, d)
  (define ((cross/monad f g) m)
    (do/monad
      (← p m)
      (← x (f (car p)))
      (← y (g (cdr p)))
      ,(cons x y)))

  ; ; out :: (Monad m) => m a -> a
  ; (define (out/monad m)
  ;   (do/monad
  ;     (← x m)
  ;     x))

)

(functor ((aux category monad plus) (M0 (filter/monad)) (M (return ⊕₀/monad ⊕/monad)))

  *

  (import
    scheme 
    (chicken base)
    (aux base)
    (prefix M0 M0:)
    (prefix M M:))

  (define (return/⊕ . args) (foldr (λ (arg m) (M:⊕/monad (M:return arg) m)) M:⊕₀/monad args))

  (define (guard/monad b) (if b (M:return (void)) M:⊕₀/monad))

  ; powerset/monad :: (MonadPlus m) => [a] -> m [a]
  (define (powerset/monad lst) (M0:filter/monad (μ _ (return/⊕ #f #t)) lst))

)