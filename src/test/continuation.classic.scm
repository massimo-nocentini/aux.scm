(import (aux unittest) (aux base) (aux continuation) (aux continuation classic) (chicken continuation) (chicken port)
        (only (scheme base) open-output-string get-output-string))

(define (eternity) (eternity))

(define-suite continuation-classic-suite

  ((doc r) `((structure/section "Classic continuation operators")
             (p "Ported from on-scheme's " (code/inline "continuations.scm")
                ", after " (cite/a "https://mitpress.mit.edu/9780262561006/the-seasoned-schemer/"
                                   "The Seasoned Schemer") ".")
             (structure/section "Implementation")
             (code/scheme/file "../aux.continuation.classic.scm")))

  ;; The original plain `letcc` is (aux continuation)'s `letcc`; the same
  ;; expectations are checked on `letcc/raw` too.
  ((test/letcc _)
   (⊦= 1 (letcc hop 1))
   (⊦= 1 (letcc hop (hop 1)))
   (⊦= 1 (letcc hop (add1 (hop 1))))
   (⊦= 10 (+ 9 (letcc hop (add1 (hop 1)))))
   (⊦= 1 (letcc/raw (raw hop) 1))
   (⊦= 1 (letcc/raw (raw hop) (hop 1)))
   (⊦= 1 (letcc/raw (raw hop) (add1 (hop 1))))
   (⊦= 10 (+ 9 (letcc/raw (raw hop) (add1 (hop 1))))))

  ((test/trycc/named _)
   (let ((identity-applicative&return (λ (x skip) x))
         (identity-applicative&skip (λ (x skip) (skip 'discard))))

     (⊦= 1 (trycc/named
             (skip (identity-applicative&return 1 skip))
             (else (eternity))))

     (⊦= 2 (trycc/named
             (skip (identity-applicative&skip 1 skip))
             (else 2)))

     (⊦= '(1)
         (trycc/named
           (skip1 (list 1))
           (skip2 'useless)
           (else 'even-more-useless)))

     (⊦= '(2)
         (trycc/named
           (skip1 (list 1 (skip1 'discard)))
           (skip2 (list 2))
           (else 'useless)))

     (⊦= '(3)
         (trycc/named
           (skip1 (list 1 (skip1 'discard)))
           (skip2 (list 2 (skip2 'discard)))
           (else (list 3))))))

  ((test/trycc/named/=> _)
   (⊦= '(discard discard)
       (trycc/named
         (skip1 (list 1 (skip1 'discard)))
         (skip2 (list 2 (skip2 'discard)))
         (else => identity)))
   (⊦= '(1)
       (trycc/named
         (skip1 (list 1))
         (skip2 (list 2 (skip2 'discard)))
         (else => identity))))

  ((test/letcc/output _)
   (⊦= "I got here\nThis string was passed to the continuation.\n"
       (with-output-to-string
         (τ
           (display
             (letcc cont
               (display "I got here\n")
               (cont "This string was passed to the continuation.\n")
               (display "...but not here")))))))

  ;; `set/cc!` stores a procedure: the first `(start)` re-enters the `set!`,
  ;; which stores the unspecified value passed by that call, so the message
  ;; is displayed twice and only the second `(start)` raises an error. The
  ;; original `test-error` does not capture the output; here the port does.
  ((test/set/cc! _)
   (let1 (port (open-output-string))
     (⊨ (condition-case (begin
                          (let ((start #f))
                            (unless start
                              (set/cc! start))
                            (display "Going to invoke `(start)`\n" port)
                            (start))
                          #f)
          ((exn) #t)))
     (⊦= "Going to invoke `(start)`\nGoing to invoke `(start)`\n"
         (get-output-string port))))

  ;; Known quirk: the `(number? add1)` clause is dead code, see the module.
  ((test/cond/cc _)
   (⊦= 3 (cond/cc
           (number? add1)
           (else (λ (cont) (+ 2 (cont 3)))))))

  ((test/letcc/raw _)
   (⊦= 5
       (let ((start #f))
         (letcc/raw (cont λ)
           (unless start (set! start λ))
           (cond
             ((continuation? cont) (λ 3))
             ((number? cont) (add1 cont))
             (else (error "e")))
           (start) (λ 7)) 5)))

  ((test/escapecc _)
   (let ((leftmost (λ (l)
                     (escapecc
                       (hop (let L ((ll l))
                              (cond
                                ((null? ll) '(no symbol here))
                                ((symbol? (car ll)) (hop (car ll)))
                                (else (begin (L (car ll)) (L (cdr ll)))))))
                       (else l)))))
     (⊦= 'a (leftmost '(((a) b) (c d))))
     (⊦= 'a (leftmost '(((a) ()) () (e))))
     (⊦= '((() ())) (leftmost '((() ())))))
   (⊦= 3 (escapecc (out (+ 1 (out 3))) (=> add1)))
   (⊦= 5 (escapecc (out (+ 1 3)) (=> add1)))
   (⊦= 'done (escapecc (out (+ 1 3)) (else 'done))))

  ((test/apply/cc _)
   (⊦= '(1 2 3 4) (apply/cc (λ (a b c d k) (list a b c d)) 1 2 '(3 4)))
   (⊦= 'escaped (apply/cc (λ (a b k) (cons a (k 'escaped))) '(1 2)))
   (⊦= '(1 . 2) (apply/cc (λ (a b k) (cons a b)) 1 '(2)))
   (⊦= 'no (apply/cc (λ (k) (+ 1 (k 'no))) '())))

  ((test/continuation->λ _)
   (⊦= '(1 2) (call-with-values
                (τ (continuation-capture (λ (raw) ((continuation->λ raw) 1 2))))
                list))
   (⊦= 3 (+ 1 (continuation-capture (λ (raw) (* 10 ((continuation->λ raw) 2)))))))

  ((test/current-continuation _)
   (⊨ (continuation? (current-continuation/cont)))
   (⊨ (procedure? (current-continuation/λ)))
   (⊦= '(0 1 2 3)
       (let ((n 3) (acc '()))
         (let1 (k (current-continuation/λ))
           (push! n acc)
           (unless (zero? n)
             (set! n (sub1 n))
             (k k)))
         acc))
   (⊦= '(0 1 2 3)
       (let ((n 3) (acc '()))
         (let1 (c (current-continuation/cont))
           (push! n acc)
           (unless (zero? n)
             (set! n (sub1 n))
             (continuation-return c c)))
         acc)))

  ((test/letcc/escaper _)
   (⊦= 10 (letcc/escaper escaper (+ ((escaper *) 5 2) 3)))
   (⊦= 15 (letcc/escaper escaper (+ ((escaper (λ (x) (- ((escaper *) x 3) 7))) 5) 4)))
   (⊦= -1 (letcc/escaper escaper ((escaper add1) ((escaper sub1) 0))))
   (⊦= '(3) (letcc/escaper escaper
              (let ((escape-cons (escaper cons)))
                (escape-cons 1 (escape-cons 2 (escape-cons 3 '()))))))
   (⊦= '(a b d) (letcc/escaper escaper
                  (let ((receiver (escaper (λ (proc) (cons 'c (proc (list 'd)))))))
                    (cons 'a (cons 'b (call-with-current-continuation receiver)))))))

  )

(unittest/✓ continuation-classic-suite)
