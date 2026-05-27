
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
      (let1 (expr `((log ,(category-writer-log w)) (value ,(category-writer-value w))))
        (pretty-print expr port))))

  (define (return v) (make-category-writer (M:mlog v) v))

  (define (>>= m f)
    (match/first m
      (#(_ ,w ,v) (match/first (f v)
                    (#(_ ,w* ,v*) (make-category-writer (M:mappend w w*) v*))))))

  (define (fail . args) (error "Writer monad does not support failure"))

)

#|

(import (aux category list))
(import (aux category monad list))
(import (aux category monad writer list))

(do/monad
  (let x (writer/log 1))
  (let y (writer/log 2))
  ,(+ x y))

(do/monad
  (let x ,1)
  (let y ,2)
  ,(+ x y))

(define (gcd a b)
  (match/first b
    (0 (do/monad ,a))
    (else (do/monad
            ; (let _ (tell `(Calculating gcd of ,a and ,b)))
            (unquote `(Calculating gcd of ,a and ,b))
            (gcd b (modulo a b))))))

(gcd 8 3)

|#