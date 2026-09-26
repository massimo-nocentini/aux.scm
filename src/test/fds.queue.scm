
(import (aux unittest) (aux base) (aux fds queue) (chicken condition))

; the message of the error raised by thunk, or 'not-raised: like ⊦raises it requires a condition, and it also exposes the message
(define (error-message thunk)
  (condition-case (begin (thunk) 'not-raised)
    (e (exn) ((condition-property-accessor 'exn 'message) e))))

(define-suite queue-suite

  ((test-letqueue _)
   (⊦= 2 (cadr '(1 2)))
   (letqueue ((q '())
              (w '(a))
              (e '(a b))
              (r '(a b c))
              (t '(a b c d)))
             (⊦= '(0 ()) q) (⊨ (fds-queue-empty? q))
             (⊦= '(1 (a)) w) (⊭ (fds-queue-empty? w))
             (⊦= '(2 (a) b) e) (⊭ (fds-queue-empty? e))
             (⊦= '(3 (a) c b) r) (⊭ (fds-queue-empty? r))
             (⊦= '(4 (a) d c b) t) (⊭ (fds-queue-empty? t))))

  ((test-cons-cdr _)
   (letqueue ((q '()))
             (⊦= '(0 ()) q)


             (set! q (fds-queue-cons 'a q))
             (⊦= '(1 (a)) q)
             (⊭ (fds-queue-empty? q))
             (⊦= 'a (fds-queue-car q))

             (set! q (fds-queue-cons 'b q))
             (⊦= '(2 (a) b) q)
             (⊭ (fds-queue-empty? q))
             (⊦= 'a (fds-queue-car q))

             (set! q (fds-queue-cons 'c q))
             (⊦= '(3 (a) c b) q)
             (⊭ (fds-queue-empty? q))
             (⊦= 'a (fds-queue-car q))

             (set! q (fds-queue-cdr q))
             (⊦= '(2 (b c)) q)
             (⊭ (fds-queue-empty? q))
             (⊦= 'b (fds-queue-car q))

             (set! q (fds-queue-cdr q))
             (⊦= '(1 (c)) q)
             (⊭ (fds-queue-empty? q))
             (⊦= 'c (fds-queue-car q))

             (set! q (fds-queue-cdr q))
             (⊦= '(0 ()) q)
             (⊨ (fds-queue-empty? q))))

  ((test-empty-car _)
   (letqueue ((q '()))
             (⊦= "fds-queue-car: empty queue" (error-message (λ () (fds-queue-car q))))
             (⊦= "fds-queue-cdr: empty queue" (error-message (λ () (fds-queue-cdr q))))
             (⊦= "fds-queue-car: empty queue" (error-message (λ () (fds-queue-car (fds-queue-cdr (fds-queue-cons 'a q))))))))
  )

(unittest/✓ queue-suite)