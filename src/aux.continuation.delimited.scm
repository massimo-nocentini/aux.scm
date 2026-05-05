
(module (aux continuation delimited) *

  (import scheme 
          (chicken base)
          (aux base)
          (aux continuation))

  (define-syntax-rule (resetcc body ...) (delimcc-reset (τ body ...)))
  (define-syntax-rule (resetcc+null body ...) (resetcc body ... '()))
  (define-syntax define-resetcc
    (syntax-rules ()
      ((define-resetcc (def arg ...) body ...) (define def (λ (arg ...) (resetcc body ...))))
      ((define-resetcc def body ...) (define def (resetcc body ...)))))
  
  (define-syntax-rule (letcc/shift k body ...) (delimcc-shift (λ (k) body ...)))
  (define (callcc/shift f) (letcc/shift k (f k)))
  (define-syntax-rule (λ-shift args body ...) (letcc/shift k (λ args (let1 (x (begin body ...)) (k x)))))
  (define-syntax-rule (τ-shift body ...) (λ-shift () body ...))

  (define *meta-continuation* identity) ; or: (λ (v) (error "Missing enclosing resetcc" v)) for a more strict experience.
  (define (delimcc-abort thunk) (*meta-continuation* (thunk)))
  
  (define (delimcc-reset thunk)
    (let1 (mc *meta-continuation*)
      (letcc k
        (set! *meta-continuation* (λ (v) (set! *meta-continuation* mc) (k v)))
        (delimcc-abort thunk))))

  (define (delimcc-shift f) (letcc k (delimcc-abort (τ (f (λ (v) (resetcc (k v))))))))

  (define (delimcc-extract) (letcc/shift k k))
  (define (delimcc-discard v) (letcc/shift _ v))
  (define (delimcc-either lst) (letcc/shift k (map k lst)))

  (define (yield v) (letcc/shift k (cons v (k (void)))))
  (define (yield/extract v) (letcc/shift k (cons v k)))

  (define-syntax delimcc-foldr
    (syntax-rules ()
      ((delimcc-fold bexpr ((each acc) fbody ...) body ...)
       (let* ((witness (gensym))
              (b bexpr)
              (f (λ (each acc) fbody ...)))
         (define (L r)
           (cond
             ((eq? r witness) b)
             (else (f (car r) (L (let1 (k (cdr r)) (k (void))))))))
         (L (resetcc body ... witness))))))
  )