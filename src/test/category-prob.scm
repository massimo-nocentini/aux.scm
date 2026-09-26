
(import (only srfi-1 iota) (aux unittest) (aux base) (aux category prob) (aux category monad prob))

(define-suite category-prob-suite

  ((doc r) `((structure/section "Introduction")
             (p "This section tests the probability monad, where a value of type (Prob a) is a distribution: an association list of outcomes paired with their probabilities.  Binding multiplies the probabilities along each path and consolidates outcomes that share the same value by summing their probabilities.")

             (structure/section "Implementation")

             (code/scheme/file "../aux.category.prob.scm")))

  ; ⊦= compares numbers approximately, so exactness is checked with equal?
  ((test/return _)
      (⊨ (equal? '((a . 1)) (return 'a)))
      (let ((d '((a . 1/2) (b . 1/2))))
        (⊨ (equal? d (>>= d return))))
      `(doc "Return is the certain distribution, with the exact probability 1 so that the right identity law "
            (code "(>>= m return) = m") " holds for exact distributions too."))

  ((test/exact _)
      (let ((d (do/monad
                 (← x (coin/tf 1/3))
                 (← y (coin/tf 1/7))
                 ,(and x y))))
        (⊨ (equal? '((#t . 1/21) (#f . 20/21)) d))
        (⊨ (exact? (apply + (map cdr d)))))
      `(doc "Distributions built from exact rationals stay exact through binds, and their total probability is exactly 1."))

  ((test/consolidate _)
      (let* ((up (map (λ (i) (cons i 1/300)) (iota 100)))
             (down (map (λ (i) (cons (- 99 i) 2/300)) (iota 100)))
             (m (append up down))
             (m* (map (λ (vp) (cons (car vp) (cdr vp))) m)))
        (⊨ (equal? '((#f . 0.5) (#t . 0.5)) (consolidate '((#f . 0.25) (#t . 0.5) (#f . 0.25)))))
        (⊨ (equal? (map (λ (i) (cons i 1/100)) (iota 100)) (consolidate m)))
        (⊨ (equal? m* m)))
      `(doc (code consolidate) " sums the probabilities of outcomes sharing a value, in first-occurrence order and without mutating its argument; "
            "past about 60 distinct values it finds the summed cells through a hash table instead of scanning them."))

)

(unittest/✓ category-prob-suite)
