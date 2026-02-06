
(module (aux continuation) *

  (import scheme 
          (chicken base)
          (chicken continuation)
          (aux base))

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

  (define-syntax-rule (letcc/call hop body ...) (call-with-current-continuation (λ (hop) body ...)))

  (define-syntax-rule (resetcc body ...) (callcc (delimcc-reset (τ body ...))))
  (define-syntax-rule (define-resetcc def body ...) (define def (resetcc body ...)))
  (define-syntax-rule (letcc/shift k body ...) (callcc (delimcc-shift (λ (k) body ...))))
  (define-syntax-rule (delimcc-τ body ...) (letcc/shift k (τ (let1 (x (begin body ...)) (k x)))))

  (define (callcc/shift f) (letcc/shift k (f k)))
  
  ;(let1 (cont (λ (v) (error "Missing enclosing resetcc" v)))
  (define *delimcc-base-cont* (make-parameter (λ (v) v)))
  (define (delimcc-abort v) ((*delimcc-base-cont*) v))
  (define (delimcc-cont-push! k) (*delimcc-base-cont* k))
  (define (delimcc-cont-pop!) (*delimcc-base-cont*))
  (define ((delimcc-reset t) k)
    (let* ((s (delimcc-cont-pop!))
           (s* (λ (r) (delimcc-cont-push! s) (k r))))
      (delimcc-cont-push! s*)
      (delimcc-abort (t))))
  (define ((delimcc-shift h) k) (delimcc-abort (h (λ (v) (resetcc (k v))))))

  (define (delimcc-extract) (letcc/shift k k))
  (define (delimcc-discard v) (letcc/shift _ v))
  (define-syntax-rule (delimcc-λ args body ...) (letcc/shift k (λ args (let1 (x (begin body ...)) (k x)))))
  (define (delimcc-either lst) (letcc/shift k (map k lst)))

  (define-syntax-rule (resetcc+null body ...) (resetcc body ... '()))
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




















































