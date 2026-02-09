
(module (aux continuation) *

  (import scheme 
          (chicken base)
          (chicken continuation)
          (aux base))

  (define-syntax-rule (letcc/call hop body ...) (call-with-current-continuation (λ (hop) body ...)))

  (define-syntax-rule (letcc hop body ...)
    (continuation-capture (λ (cont) 
                            (let1 (hop (λ (arg) (continuation-return cont arg)))
                              body ...))))

  (define (callcc f) (letcc k (f k)))

  (define-syntax-rule (letcc* next ((v exp) ...) body ...)
    (letcc success
      (let* ((v (letcc next (success exp))) ...
             (next success))
        body ...)))

  (define-syntax-rule (literal else) 
    (trycc (next exp ...) (else body ...))
      (letcc* hop ((v (let1 (next (τ (hop (void)))) exp)) ...)
        body ...))

  )