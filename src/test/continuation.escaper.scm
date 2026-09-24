
(import (aux unittest)
        (aux base)
        (aux continuation classic)
        (only (scheme base) call/cc open-output-string get-output-string)
        (chicken port)
        (chicken format)
        (chicken irregex)
        (chicken random))

;; Ported from on-scheme's introduction-to-continuations-tests.scm, which
;; follows chapter 16 of Springer and Friedman, "Scheme and the Art of
;; Programming". The original `tester` forms only displayed the expected and
;; actual values; here each one is an assertion. The global `make-escaper`,
;; which captured the continuation of a top-level form, is replaced by the
;; scoped `letcc/escaper`, and every example that re-enters a continuation
;; is written with an explicit phase counter inside a single case.
;; Printed procedure names, such as `#<procedure (display/return x)>` or
;; `#<procedure (continuation . results1901)>`, are normalised to
;; `#<procedure>` because they depend on the compiler.

(define (procedure-names/normalise s) (irregex-replace/all "#<procedure[^>]*>" s "#<procedure>"))

;; receivers
(define receiver-1 (λ (proc) (proc (list 1))))
(define receiver-2 (λ (proc) (proc (list (proc (list 2))))))
(define receiver-3 (λ (proc) (proc (list (proc (list 3 proc))))))

;; `display/return` and `answer-maker` write on `port` so that the output of
;; a re-entered continuation can be inspected without re-entering the
;; dynamic extent of a `with-output-to-string`.
(define (display/return/port port) (λ (x) (display x port) x))
(define (answer-maker/port port) (λ (x) (cons 'answer-is ((display/return/port port) x))))

(define-suite escaper-suite

  ((doc r) `((structure/section "Escape procedures and " (code/inline "call/cc"))
             (p "Exercises of chapter 16 of Springer and Friedman, "
                (i "Scheme and the Art of Programming") ", on contexts, escape procedures "
                "and " (code/inline "call/cc") ".")))

  ;; A context is a procedure of one variable ▢; applied to the value of the
  ;; subexpression, it gives the value of the whole expression.
  ((test/contexts _)
   (⊦= (+ 3 (* 4 (+ 5 6)))
       ((λ (▢) (+ 3 (* 4 ▢))) (+ 5 6)))
   (⊦= 47 ((λ (▢) (+ 3 (* 4 ▢))) (+ 5 6)))
   (⊦= (letrec ((sum+n (λ (n)
                         (if (zero? n)
                           1
                           (+ (add1 n) (sum+n (sub1 n)))))))
         (* 10 (sum+n 5)))
       ((λ (▢) (* 10 (+ 6 (+ 5 (+ ▢ (+ 3 (+ 2 1))))))) (add1 3)))
   (⊦= 210 ((λ (▢) (* 10 (+ 6 (+ 5 (+ ▢ (+ 3 (+ 2 1))))))) (add1 3)))
   ;; contexts may even maintain state
   (let* ((out (open-output-string))
          (v (begin
               (display 0 out)
               (let ((n 1))
                 (if (zero? n)
                   (display (+ 3 (* 4 (+ 5 6))) out)
                   (display (* (+ (* 3 4) 5) 2) out))
                 (set! n (+ n 2))
                 n))))
     (⊦= "034" (get-output-string out))
     (⊦= 3 v))
   (let* ((out (open-output-string))
          (n 1)
          (context (λ (▢) (begin
                            (display 0 out)
                            (display (* (+ ▢ 5) 2) out)
                            (set! n (+ n 2))
                            n))))
     (⊦= 3 (context (* 3 4)))
     (⊦= 5 (context (* 3 4)))
     (⊦= "034034" (get-output-string out))))

  ((test/escaper/first _)
   (⊦= 10 (letcc/escaper escaper
            (let ((escape-* (escaper *)))
              (+ (escape-* 5 2) 3)))))

  ((test/escaper _)
   (⊦= 10 (letcc/escaper escaper (+ ((escaper *) 5 2) 3)))
   (⊦= 8 (letcc/escaper escaper
           (+ ((escaper (λ (x) (- (* x 3) 7))) 5)
              4)))
   (⊦= 8 (letcc/escaper escaper
           (+ ((escaper (λ (x) ((escaper -) (* x 3) 7))) 5)
              4)))
   (⊦= 15 ((λ (x) (* x 3)) 5))
   (⊦= 15 (letcc/escaper escaper
            (+ ((escaper (λ (x) (- ((escaper *) x 3) 7))) 5)
               4)))
   (⊦= 15 (letcc/escaper escaper
            (+ ((escaper (λ (x) ((escaper -) ((escaper *) x 3) 7))) 5)
               4))))

  ;; an escape invocation abandons its context: `(f (e expr))` = `(e expr)`
  ((test/escaper/abandons _)
   (⊦= 8 (letcc/escaper escaper
           (/ (+ ((escaper (λ (x) (- (* x 3) 7))) 5)
                 4)
              2)))
   ; 16.4.1
   (⊦= -1 (letcc/escaper escaper ((escaper add1) ((escaper sub1) 0))))
   ; 16.4.2
   (⊦= '(3) (letcc/escaper escaper
              (let ((escape-cons (escaper cons)))
                (escape-cons 1 (escape-cons 2 (escape-cons 3 '()))))))
   ; 16.6.1
   (⊦= "reset invoked"
       (with-output-to-string
         (τ (letcc/escaper escaper
              (let ((reset (λ ()
                             (let ((escape (escaper (λ () (display "reset invoked")))))
                               (escape)))))
                (cons 1 (reset)))))))
   (⊨ (void? (letcc/escaper escaper
               (let ((reset (λ ()
                              (let ((escape (escaper (λ () (void)))))
                                (escape)))))
                 (cons 1 (reset))))))
   ; 16.6.2
   (⊦= '(2) (letcc/escaper escaper
              (let ((reset (λ () ((escaper (λ () '(2)))))))
                (cons 1 (reset))))))

  ;; manually building contexts and their escaping procedures.
  ((test/receiver/escaper _)
   (⊦= 27 (letcc/escaper escaper
            (let ((receiver (λ (continuation) 6))
                  (context (λ (▢) (+ 3 (* 4 ▢)))))
              (+ 3 (* 4 (receiver (escaper context)))))))
   (⊦= 27 (letcc/escaper escaper
            (let ((receiver (λ (continuation) (continuation 6)))
                  (context (λ (▢) (+ 3 (* 4 ▢)))))
              (+ 3 (* 4 (receiver (escaper context)))))))
   ;; `receiver` escapes the context `(λ (▢) (+ 2 ▢))`, not the one denoted
   ;; by `context`, which is the one we actually escape toward.
   (⊦= 27 (letcc/escaper escaper
            (let ((receiver (λ (continuation) (+ 2 (continuation 6))))
                  (context (λ (▢) (+ 3 (* 4 ▢)))))
              (+ 3 (* 4 (receiver (escaper context))))))))

  ;; contexts and their escaping procedures are provided by `call/cc` directly.
  ((test/receiver/call/cc _)
   (⊦= 27 (let ((receiver (λ (continuation) 6)))
            (+ 3 (* 4 (call/cc receiver)))))
   (⊦= 27 (let ((receiver (λ (continuation) (continuation 6))))
            (+ 3 (* 4 (call/cc receiver)))))
   (⊦= 27 (let ((receiver (λ (continuation) (+ 2 (continuation 6)))))
            (+ 3 (* 4 (call/cc receiver))))))

  ;; the quoted `sexp` bindings of the original, which only documented the
  ;; expressions, are omitted.
  ((test/16.9 _)
   ; 16.9.1
   (⊦= -22 (letcc/escaper escaper
             (let ((context (λ (▢) (- 3 (* 5 ▢))))
                   (receiver (λ (continuation) (continuation 5))))
               (- 3 (* 5 (receiver (escaper context)))))))
   (⊦= -22 (let ((receiver (λ (continuation) (continuation 5))))
             (- 3 (* 5 (call/cc receiver)))))
   ; 16.9.2
   (⊦= -22 (letcc/escaper escaper
             (let ((context (λ (▢) (- 3 (* 5 ▢))))
                   (receiver (λ (continuation) 5)))
               (- 3 (* 5 (receiver (escaper context)))))))
   (⊦= -22 (let ((receiver (λ (continuation) 5)))
             (- 3 (* 5 (call/cc receiver)))))
   ; 16.9.3
   (⊦= -22 (letcc/escaper escaper
             (let ((context (λ (▢) (- 3 (* 5 ▢))))
                   (receiver (λ (continuation) (+ 1000 (continuation 5)))))
               (- 3 (* 5 (receiver (escaper context)))))))
   (⊦= -22 (let ((receiver (λ (continuation) (+ 1000 (continuation 5)))))
             (- 3 (* 5 (call/cc receiver))))))

  ((test/experiment/first _)
   (let* ((port (open-output-string))
          (call (λ (receiver) (receiver (display/return/port port))))
          (result (void)))
     (set! result ((answer-maker/port port) (call receiver-1)))
     (⊦= "(1)(1)" (get-output-string port))
     (⊦= '(answer-is 1) result))
   (let* ((port (open-output-string))
          (result/cc (void)))
     (set! result/cc ((answer-maker/port port) (call/cc receiver-1)))
     (⊦= "(1)" (get-output-string port))
     (⊦= '(answer-is 1) result/cc)))

  ((test/experiment/second _)
   (let* ((port (open-output-string))
          (call (λ (receiver) (receiver (display/return/port port))))
          (result (void)))
     (set! result ((answer-maker/port port) (call receiver-2)))
     (⊦= "(2)((2))((2))" (get-output-string port))
     (⊦= '(answer-is (2)) result))
   (let* ((port (open-output-string))
          (result/cc (void)))
     (set! result/cc ((answer-maker/port port) (call/cc receiver-2)))
     (⊦= "(2)" (get-output-string port))
     (⊦= '(answer-is 2) result/cc)))

  ((test/experiment/third _)
   (let* ((port (open-output-string))
          (display/return (display/return/port port))
          (call (λ (receiver) (receiver display/return)))
          (result (void)))
     (set! result ((answer-maker/port port) (call receiver-3)))
     (⊦= "(3 #<procedure>)((3 #<procedure>))((3 #<procedure>))"
         (procedure-names/normalise (get-output-string port)))
     (⊦= `(answer-is (3 ,display/return)) result)
     (let1 (mark (string-length (get-output-string port)))
       (⊦= '(1000) ((cadr (cadr result)) (list 1000)))
       (⊦= "(1000)" (substring (get-output-string port) mark)))
     (⊦= `(answer-is (3 ,display/return)) result)))

  ;; re-entering the continuation of `(call/cc receiver-3)` executes again
  ;; the rest of this case, hence the `phase` counter; the pending
  ;; `answer-maker` writes on the same port, so `mark` separates the outputs.
  ((test/experiment/third/call/cc _)
   (let* ((phase 0)
          (port (open-output-string))
          (mark 0)
          (result/cc (void)))
     (set! result/cc ((answer-maker/port port) (call/cc receiver-3)))
     (set! phase (add1 phase))
     (cond
       ((= phase 1)
        (⊦= "(3 #<procedure>)" (procedure-names/normalise (get-output-string port)))
        (⊦= `(answer-is 3 ,(caddr result/cc)) result/cc)
        (⊨ (procedure? (caddr result/cc)))
        (set! mark (string-length (get-output-string port)))
        ((caddr result/cc) (list 1000))
        (⊭ 'unreachable))
       (else
        (⊦= 2 phase)
        (⊦= "(1000)" (substring (get-output-string port) mark))
        (⊦= '(answer-is 1000) result/cc)))))

  ;; the fourth experiment, as requested by exercise 16.15.
  ((test/experiment/fourth _)
   (let* ((port (open-output-string))
          (display/return (display/return/port port))
          (call (λ (receiver) (receiver display/return)))
          (result (void)))
     (⊦= 'done (begin
                 (set! result ((answer-maker/port port) (call receiver-3)))
                 'done))
     (⊦= "(3 #<procedure>)((3 #<procedure>))((3 #<procedure>))"
         (procedure-names/normalise (get-output-string port)))
     (⊦= `(answer-is (3 ,display/return)) result)
     (let1 (mark (string-length (get-output-string port)))
       (⊦= '(1000) ((cadr (cadr result)) (list 1000)))
       (⊦= "(1000)" (substring (get-output-string port) mark)))
     (⊦= `(answer-is (3 ,display/return)) result)))

  ((test/experiment/fourth/call/cc _)
   (let* ((phase 0)
          (port (open-output-string))
          (mark 0)
          (result/cc (void)))
     (let1 (v (begin
                (set! result/cc ((answer-maker/port port) (call/cc receiver-3)))
                'done))
       (set! phase (add1 phase))
       (⊦= 'done v)
       (cond
         ((= phase 1)
          (⊦= "(3 #<procedure>)" (procedure-names/normalise (get-output-string port)))
          (⊦= `(answer-is 3 ,(caddr result/cc)) result/cc)
          (set! mark (string-length (get-output-string port)))
          ((caddr result/cc) (list 1000))
          (⊭ 'unreachable))
         (else
          (⊦= 2 phase)
          (⊦= "(1000)" (substring (get-output-string port) mark))
          (⊦= '(answer-is 1000) result/cc))))))

  ;; 16.14, with `pseudo-random-integer` in place of `random`; each
  ;; expression is sampled many times and its value must be in the set.
  ((test/16.14 _)
   (let ((receiver (λ (continuation)
                     (if (zero? (pseudo-random-integer 2))
                       (+ 1000 6)
                       (continuation 6)))))
     (do ((i 0 (add1 i))) ((= i 32))
       ;; actual in {72, 8072}, with uniform probability
       (⊨ (member? (* (+ (call/cc receiver) 3) 8) '(72 8072)))
       ;; actual in {144, 8144, 16144}, with probabilities 1/4, 1/2, 1/4
       (⊨ (member? (+
                     (* (+ (call/cc receiver) 3) 8)
                     (* (+ (call/cc receiver) 3) 8))
                   '(144 8144 16144)))))
   (let ((receiver (λ (continuation)
                     (continuation ; useless
                       (if (zero? (continuation (pseudo-random-integer 2)))
                         (+ 1000 6)
                         6)))))
     (do ((i 0 (add1 i))) ((= i 32))
       ;; actual in {48, 56, 64}, with probabilities 1/4, 1/2, 1/4
       (⊨ (member? (+
                     (* (+ (call/cc receiver) 3) 8)
                     (* (+ (call/cc receiver) 3) 8))
                   '(48 56 64))))))

  ;; 16.16: re-entering `deep` completes again `(cons 1000 ▢)`, hence the
  ;; `phase` counter; each run collects both results.
  ((test/16.16 _)
   (let* ((deep (void))
          (map-sub1 (letrec ((M (λ (ls)
                                  (if (null? ls)
                                    (call/cc (λ (continuation)
                                               (set! deep continuation)
                                               '()))
                                    (cons (sub1 (car ls)) (M (cdr ls)))))))
                      M))
          (run (λ (ls)
                 (let ((phase 0) (results '()))
                   (let1 (v (cons 1000 (map-sub1 ls)))
                     (push! v results)
                     (set! phase (add1 phase))
                     (when (= phase 1) (cons 2000 (deep '(a b c)))))
                   (reverse results)))))
     (⊦= '((1000) (1000 a b c)) (run '()))
     (⊦= '((1000 -1) (1000 -1 a b c)) (run '(0)))
     (⊦= '((1000 0 -1) (1000 0 -1 a b c)) (run '(1 0)))
     (⊦= '((1000 4 3 2 1 0 -1) (1000 4 3 2 1 0 -1 a b c)) (run '(5 4 3 2 1 0)))))

  ;; the original applied the top-level `*escaper/thunk*` to a thunk, which
  ;; is the same as applying the escape version of that thunk.
  ((test/escaper/thunk _)
   (⊦= 7 (letcc/escaper escaper ((escaper (τ (add1 6))))))
   (⊦= 7 (letcc/escaper escaper (+ 5 ((escaper (τ (add1 6))))))))

  ((test/escaper/receiver _)
   (⊦= '(a b d)
       (letcc/escaper escaper
         (let ((receiver (escaper
                           (λ (proc)
                             (cons 'c (proc (list 'd)))))))
           (cons 'a (cons 'b (call/cc receiver))))))
   (⊦= '(c d)
       (letcc/escaper escaper
         (let ((receiver (escaper
                           (λ (proc)
                             '(c d)))))
           (cons 'a (cons 'b (call/cc receiver))))))
   ;; `make-new-escaper` built a second, independent escaper.
   (⊦= '(c d)
       (letcc/escaper new-escaper
         (let ((receiver (new-escaper
                           (λ (proc)
                             '(c d)))))
           (cons 'a (cons 'b (call/cc receiver)))))))

  ((test/countdown _)
   (let* ((countdown (λ (n attempt)
                       (format #t "~%This string will appear only once~%")
                       (let* ((message (λ (direction value)
                                         (format #t "\t~Aing `attempt` function with value ~A~%" direction value)
                                         value))
                              (pair (message 'exit (attempt (message 'enter n))))
                              (v (car pair))
                              (returner (cadr pair)))
                         (format #t "\tnon-negative number: ~A~%" v)
                         (if (positive? v)
                           (returner (list (sub1 v) returner))
                           (format #t "Blastoff")))))
          (v (void)))
     (⊦= "
This string will appear only once
\tentering `attempt` function with value 3
\texiting `attempt` function with value (3 #<procedure>)
\tnon-negative number: 3
"
         (procedure-names/normalise
           (with-output-to-string
             (τ (set! v (let ((attempt (λ (n)
                                         (let ((receiver (λ (proc) (list n proc))))
                                           (receiver (λ (x) x))))))
                          (countdown 3 attempt)))))))
     ;; the original displayed this value as `(2 #<procedure (? x)>)`
     (⊦= 2 (car v))
     (⊨ (procedure? (cadr v)))
     (⊦= "
This string will appear only once
\tentering `attempt` function with value 3
\texiting `attempt` function with value (3 #<procedure>)
\tnon-negative number: 3
\texiting `attempt` function with value (2 #<procedure>)
\tnon-negative number: 2
\texiting `attempt` function with value (1 #<procedure>)
\tnon-negative number: 1
\texiting `attempt` function with value (0 #<procedure>)
\tnon-negative number: 0
Blastoff"
         (procedure-names/normalise
           (with-output-to-string
             (τ (let ((attempt (λ (n)
                                 (let ((receiver (λ (proc) (list n proc))))
                                   (call/cc receiver)))))
                  (countdown 3 attempt))))))))

  ((test/three-phases _)
   (⊦= "beginbeginmiddlebeginend"
       (with-output-to-string
         (τ (let ((receiver
                    (λ (continuation)
                      (continuation continuation))) ; equivalent to just returning `continuation`
                  (three-phases
                    (λ (continuation)
                      (display 'begin)
                      (call/cc continuation)
                      (display 'middle)
                      (call/cc continuation)
                      (display 'end))))
              (three-phases (call/cc receiver)))))))

  )

(unittest/✓ escaper-suite)
