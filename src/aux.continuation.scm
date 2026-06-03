
(module (aux continuation) *

  (import scheme 
          (chicken base)
          (chicken continuation)
          (aux base))

  (define-syntax-rule (letcc/call hop body ...) (call-with-current-continuation (λ (hop) body ...)))

  (define-syntax-rule (letcc hop body ...)
    (continuation-capture (λ (cont) 
                            (let1 (hop (λ args (continuation-graft cont (τ (apply values args)))))
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


(module (aux category continuation) *

  (import scheme (chicken base) (chicken pretty-print) (aux base) (aux continuation))

  (define-record category-continuation k)

  (set-record-printer! category-continuation
    (λ (cont port) (pretty-print `(cont ,(callcc (category-continuation-k cont))) port)))

  (define (return v) (make-category-continuation (λ (k) (k v))))

  (define (>>= m f) 
    (make-category-continuation (λ (k) 
                                  (let1 (km (category-continuation-k m))
                                    (km (λ (v) (>>= (f v) (apply-to k))))))))

  (define (fail . args) (error "Continuation monad does not support failure"))
)

(import (aux category monad))

(module (aux category monad continuation) = ((aux category monad) (aux category continuation)))

#|

(import (aux continuation))
(import (aux category continuation))
(import (aux category monad continuation))

(+ 4 (callcc (category-continuation-k (do/monad
         (let x ,1)
         (let y ,2)
         ,3
         ,(+ x x y)))))

|#