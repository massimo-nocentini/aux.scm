

(module (aux category prob) *

  (import scheme (chicken base) (only srfi-1 append-map find) (aux base))

  #|

  newtype Prob a = Prob { getProb :: [(a, Rational)] } deriving Show

  instance Monad Prob where
    return x = Prob [(x,1%1)]
    m >>= f = flatten (fmap f m)
    fail _ = Prob []

  flatten :: Prob (Prob a) -> Prob a
  flatten (Prob xs) = Prob $ concat $ map multAll xs
    where multAll (Prob innerxs, p) = map (\(x, r) -> (x, p*r)) innerxs

  |#

  ; Monad instance
  (define (return v) (list (cons v 1.0)))

  (define (>>= m f) 
    (consolidate 
      (append-map (λ1-match/first 
                    ((,v . ,p) (map (λ1-match/first 
                                      ((,v* . ,p*) (cons v* (* p p*))))
                                    (f v)))) 
                  m)))

  (define (fail . args) '())

  (define (coin/tf p) (list (cons #t p) (cons #f (- 1 p))))

  ; Consolidate outcomes that share the same value by summing their
  ; probabilities, so that a distribution like ((#f . 0.25) (#f . 0.25))
  ; collapses to ((#f . 0.5)).  Preserves first-occurrence order.
  (define (consolidate m)
    (let loop ((m m) (acc '()))
      (if (null? m)
          (reverse acc)
          (let* ((vp (car m))
                 (v  (car vp))
                 (p  (cdr vp))
                 (cell (find (λ1-match/first ((,v* . _) (equal? v v*))) acc)))
            (if cell
                (begin
                  (set-cdr! cell (+ (cdr cell) p))
                  (loop (cdr m) acc))
                (loop (cdr m) (cons (cons v p) acc)))))))

)

(import (aux category monad))

(module (aux category monad prob) = ((aux category monad) (aux category prob)))

#|

(import scheme (chicken base) srfi-1 (aux category prob) (aux category monad prob))

(do/monad
  (let x (coin/tf 0.5))
  (let y (coin/tf 0.3))
  (return (every (λ (v) v) (list x y))))

(consolidate
  (do/monad
    (let x (coin 0.5))
    (let y (coin 0.5))
    (let z (coin 0.5))
    (return (every (λ (v) v) (list x y z)))))
; => ((#t . 0.125) (#f . 0.875))

(map/monad - (list (cons 1 0.5) (cons 2 0.3) (cons 3 0.2)))

|#