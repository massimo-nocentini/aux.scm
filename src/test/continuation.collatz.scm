
(import (aux unittest) (aux base) (aux continuation) (only (scheme base) call/cc))

;; Ported from on-scheme's src/collatz.scm and tests/collatz-test.scm; the
;; definitions resemble this article of prof. Kozen:
;; http://www.cs.cornell.edu/courses/cs3110/2011sp/recitations/rec26-cps/cps.htm
;; The `letcc` is (aux continuation)'s one.

(define collatz
  (λ (x)
    (cond
      ((equal? x 1)   (list 1))
      ((even? x)      (cons x (collatz (/ x 2))))
      (else           (cons x (collatz (+ 1 (* 3 x))))))))

(define collatz&co
  (λ (x)
    (letrec ((C (λ (x col) ; `col` stands for `collector`
                  (cond
                    ((equal? x 1)   (col (list 1)))
                    ((even? x)      (C (/ x 2) (λ (lst) (col (cons x lst)))))
                    (else           (C (+ 1 (* 3 x)) (λ (lst) (col (cons x lst)))))))))
      (C x identity))))

(define collatz&co-abridged
  (λ (x)
    (letcc hop
      (letrec ((C (λ (x col)
                    (cond
                      ((equal? x 1)   (hop (col (list 1))))
                      ((even? x)      (C (/ x 2) (λ (lst) (col (cons x lst)))))
                      (else           (C (+ 1 (* 3 x)) (λ (lst) (col (cons x lst)))))))))
        (C x identity)))))

(define collatz&co-abridged2
  (λ (x)
    (letrec ((C (λ (x col)
                  (cond
                    ((equal? x 1)   (col (list 1)))
                    ((even? x)      (C (/ x 2) (λ (lst) (col (cons x lst)))))
                    (else           (C (+ 1 (* 3 x)) (λ (lst) (col (cons x lst)))))))))
      (letcc hop (C x hop)))))

(define collatz&cc
  (λ (x)
    (letrec ((C (λ (x)
                  (λ (cont) ; `cont` stands for `continuation`
                    (cond
                      ((equal? x 1)   (cont (list 1)))
                      ((even? x)      (cons x (call/cc (C (/ x 2)))))
                      (else           (cons x (call/cc (C (+ 1 (* 3 x)))))))))))
      (call/cc (C x)))))

(define collatz&cc-abridged
  (λ (x)
    (call/cc (λ (hop)
               (letrec ((C (λ (x)
                             (λ (cont)
                               (cond
                                 ((equal? x 1)   (hop (cont (list 1))))
                                 ((even? x)      (cons x (call/cc (C (/ x 2)))))
                                 (else           (cons x (call/cc (C (+ 1 (* 3 x)))))))))))
                 (call/cc (C x)))))))

(define-suite collatz-suite

  ((doc r) `((structure/section "Collatz 3x+1 problem")
             (p "Direct, continuation-passing and " (code/inline "call/cc") " versions, after "
                (cite/a "http://www.cs.cornell.edu/courses/cs3110/2011sp/recitations/rec26-cps/cps.htm"
                        "Kozen's recitation on CPS") ".")))

  ((test/collatz _)
   (let ((expected '(51 154 77 232 116 58 29 88 44 22 11 34 17 52 26 13 40 20 10 5 16 8 4 2 1)))
     (⊦= expected (collatz 51))
     (⊦= expected (collatz&co 51))
     (⊦= expected (collatz&co-abridged 51))
     (⊦= expected (collatz&co-abridged2 51))
     (⊦= expected (collatz&cc 51))
     (⊦= expected (collatz&cc-abridged 51))))

  )

(unittest/✓ collatz-suite)
