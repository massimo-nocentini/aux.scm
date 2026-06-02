
(functor ((aux category writer) (M (mappend mempty mlog)))

  *

  #|
  
  newtype Writer w a = Writer { runWriter :: (a, w) }
  instance (Monoid w) => Monad (Writer w) where
    return x = Writer (x, mempty)
    (Writer (x, v)) >>= f = let (Writer (y, v')) = f x in Writer (y, v `mappend` v')
  
  |#

  (import scheme (chicken base) (chicken pretty-print) (aux base) (prefix M M:))

  (define-record category-writer log value)

  (set-record-printer! category-writer
    (λ (w port)
      (let1 (expr `(writer (log ,(category-writer-log w)) (value ,(category-writer-value w))))
        (pretty-print expr port))))

  (define (return v) (make-category-writer M:mempty v))

  (define (>>= m f)
    (match/first m
      (#(_ ,w ,v) (match/first (f v)
                    (#(_ ,w* ,v*) (make-category-writer (M:mappend w w*) v*))))))

  (define (tell . args) (make-category-writer (foldr (λ (v m) (M:mappend (M:mlog v) m)) M:mempty args) (void)))

  (define (fail . args) (error "Writer monad does not support failure"))

)

#|

(import (aux category list))
(import (aux category monad list))
(import (only (aux category writer list) tell))
(import (aux category monad writer list))

(import (aux category difflist))
(import (aux category monad difflist))
(import (only (aux category writer difflist) tell category-writer-log))
(import (aux category monad writer difflist))

(map return '(1 2 3))

(define (final-count-down n)
  (match/first n
    (0 (do/monad (tell 0)))
    (else (do/monad
            (final-count-down (- n 1))
            (tell n)))))

(define (final-count-down n)
  (match/first n
    (0 (do/monad #;(tell 0) ,0))
    (else (do/monad
            (final-count-down (- n 1))
            #;(tell n)
            ,n))))

(callcc (final-count-down 500000))
(category-writer-log (final-count-down 500000))

(do/monad 
  (let a ,(make-category-difflist (λ (xs) (append '(1 2 3) xs))))
  (let b ,(make-category-difflist (λ (xs) (append '(4 5 6) xs))))
  (let c ,(make-category-difflist (λ (xs) (append '(7 8) xs))))
  ,(list a b c))

(do/monad
  (let a (list->difflist '(1 2 3)))
  (let b (list->difflist '(4 5 6)))
  (let c (list->difflist '(7 8)))
  (return (list a b c)))

(do/monad
  (let x (writer/log 1))
  (let y (writer/log 2))
  ,(+ x y))

(do/monad
  (let x ,1)
  (tell `(x ,x))
  (let y ,2)
  ,(+ x y))

(define (gcd a b)
  (match/first b
    (0 (do/monad ,a))
    (else (do/monad
            ; (let _ (tell `(Calculating gcd of ,a and ,b)))
            (tell `(Calculating gcd of ,a and ,b))
            (gcd b (modulo a b))))))

(gcd 8 3)

|#