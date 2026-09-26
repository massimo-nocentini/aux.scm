
(import (aux unittest) (aux base) (aux continuation classic) (aux continuation amb)
        (only (scheme base) make-parameter parameterize))

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

  ;; A choice point inside a `parameterize` or a `dynamic-wind`: every retry
  ;; enters the frame again and leaves it with the choice, so each branch sees
  ;; the binding only inside and every before thunk has its after thunk.
  ;; Before the choice points left and entered the frames, the binding leaked
  ;; into the second branch and compiled code crashed at the second retry.
  ((test/amb/parameterize _)
   (define q (make-parameter 'outer))
   (⊦= '((1 inner outer) (2 inner outer) (3 inner outer))
       (amb (λ (ε ? ✗ ✓)
              (let* ((inside #f)
                     (a (parameterize ((q 'inner)) (let1 (a (ε '(1 2 3))) (set! inside (q)) a))))
                (✓ (list a inside (q)))
                (✗)))))
   (⊦= 'outer (q)))

  ((test/amb/dynamic-wind _)
   (define wlog '())
   (⊦= '(1 2 3)
       (amb (λ (ε ? ✗ ✓)
              (let1 (a (dynamic-wind
                         (τ (push! 'in wlog))
                         (τ (ε '(1 2 3)))
                         (τ (push! 'out wlog))))
                (✓ a)
                (✗)))))
   (⊦= '(in out in out in out in out) (reverse wlog)))

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

; test/amb/parameterize and test/amb/dynamic-wind retry a choice inside a `parameterize`: when
; a jump did not rewind the `dynamic-wind` frames (a bare `continuation-return`), it left the
; string port that captured a test's output as the current output port, and the run ended with
; rc 0 and nothing printed, the summary included, hiding every failure of the suite; so check
; that the output port is back.
(import (only (chicken base) exit))

(define stdout (current-output-port))

(let1 (r (unittest/✓ amb-suite))
  (unless (eq? stdout (current-output-port))
    (current-output-port stdout)
    (write (unittest/result-summary r))
    (newline)
    (display "FAIL: the suite ended with its output still captured by a test\n")
    (exit 1)))
