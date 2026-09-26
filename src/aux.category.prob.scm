

(module (aux category prob) *

  (import scheme
          (chicken base)
          (chicken fixnum)
          (only srfi-1 append-map find) 
          (only srfi-69 make-hash-table equal?-hash hash-table-ref/default hash-table-set!) 
          (aux base))

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
  (define (return v) (list (cons v 1))) ; exact 1 keeps exact distributions exact, floats stay floats

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
  ; collapses to ((#f . 0.5)).  Preserves first-occurrence order and never
  ; mutates m: the summed cells are fresh pairs.
  ;
  ; hybrid, as `pairwise-different?` in (aux base): scanning the cells found so far wins
  ; while they are fewer than ~60, then an equal?-keyed srfi-69 table from value to cell
  ; takes over, so a bind costs O(pairs) instead of O(pairs × distinct values).
  (define (consolidate m)
    (let loop ((m m) (acc '()) (n 0) (cells #f))
      (cond
        ((null? m) (reverse acc))
        ((and (not cells) (fx> n 60))
          (let1 (cells (make-hash-table equal? equal?-hash))
            (for-each (λ (cell) (hash-table-set! cells (car cell) cell)) acc)
            (loop m acc n cells)))
        (else
          (let* ((vp (car m))
                 (v  (car vp))
                 (p  (cdr vp))
                 (cell (if cells
                         (hash-table-ref/default cells v #f)
                         (find (λ1-match/first ((,v* . _) (equal? v v*))) acc))))
            (cond
              (cell (set-cdr! cell (+ (cdr cell) p))
                    (loop (cdr m) acc n cells))
              (else (let1 (cell (cons v p))
                      (when cells (hash-table-set! cells v cell))
                      (loop (cdr m) (cons cell acc) (fx+ n 1) cells)))))))))

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