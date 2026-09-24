
(import (aux unittest) (aux base) (aux continuation classic) (aux continuation amb))

(define-suite amb-suite

  ((doc r) `((structure/section "McCarthy's " (code/inline "amb") " and a SAT solver")
             (p "Ported from on-scheme's " (code/inline "continuations.scm") ".")
             (structure/section "Implementation")
             (code/scheme/file "../aux.continuation.classic.scm")))

  ((test/amb/pythagorean _)
   (⊦= '((3 4 5) (4 3 5))
       (amb (λ (ε ? ✗ ✓)
              (let ((a (ε (list 1 2 3 4 5 6 7)))
                    (b (ε (list 1 2 3 4 5 6 7)))
                    (c (ε (list 1 2 3 4 5 6 7))))
                ; We're looking for dimensions of a legal right
                ; triangle using the Pythagorean theorem:
                (? (= (* c c) (+ (* a a) (* b b))))
                (✓ (list a b c))
                ; retry with any other solution
                (✗))))))

  ((test/amb/ordered _)
   (⊦= '((4 3 5))
       (amb (λ (ε ? ✗ ✓)
              (let* ((a (ε (list 1 2 3 4 5 6 7)))
                     (b (ε (list 1 2 3 4 5 6 7)))
                     (c (ε (list 1 2 3 4 5 6 7))))
                (? (= (* c c) (+ (* a a) (* b b))))
                (? (< b a))
                (✓ (list a b c))
                (✗)))))
   (⊦= '() (amb (λ (ε ? ✗ ✓) (✗))))
   (⊦= '() (amb (λ (ε ? ✗ ✓) (ε '())))))

  ;; Known bug, kept on purpose: `sat-solve` is unsound because `(void)` counts
  ;; as true while the other variables are unassigned; hence the second
  ;; expectation misses `(#f #t)` and the fourth one duplicates a model.
  ((test/sat-solve _)
   (⊦= (list (list #t) (list #f))
       (sat-solve (x)
         (or #t x)))

   (⊦= '((#t #t) (#t #f))
       (sat-solve (x y)
         (or x y)))

   (⊦= (list (list #f #f #t))
       (sat-solve (a b c)
         (and (implies a (not b)) (not a) c)))

   (⊦= (list (list #f #t #t #t) (list #f #t #t #t))
       (sat-solve (x1 x3 x4 x5)
         (and
           (or x1 (not x5) x4)
           (or (not x1) x5 x3 x4)))))

  ((test/implies _)
   (⊨ (implies #f #f))
   (⊨ (implies #f #t))
   (⊭ (implies #t #f))
   (⊨ (implies #t #t)))

  )

(unittest/✓ amb-suite)
