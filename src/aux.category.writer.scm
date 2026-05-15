
(functor ((aux category writer) (M (mappend mempty)))

  *

  #|
  
  newtype Writer w a = Writer { runWriter :: (a, w) }
  instance (Monoid w) => Monad (Writer w) where
    return x = Writer (x, mempty)
    (Writer (x, v)) >>= f = let (Writer (y, v')) = f x in Writer (y, v `mappend` v')
  
  |#

  (import scheme (chicken base) (aux base) (prefix M M:))

  (define (return v) `(writer ,M:mempty ,v))
  (define (>>= m f)
    (match/first m
      ((writer ,w ,v) (match/first (f v)
                        ((writer ,w* ,v*) `(writer ,(M:mappend w w*) ,v*))))))
  (define (fail . args) (error "Writer monad does not support failure"))

)

#|

(import (aux category))

(import (prefix (aux category list) list/) )
(import (prefix (aux category writer list) writer-list/) )
(import (aux category monad writer list))

(do/monad
  (let x (list/writer/log 1))
  (let y (list/writer/log 2))
  (writer-list/return (+ x y)))

|#