
(module (aux continuation) *

  (import scheme 
          (chicken base)
          (chicken continuation)
          (aux base))

  (define-syntax letcc
    (syntax-rules ()
      ((letcc hop body ...) (continuation-capture (lambda (cont) 
                                                    (let1 (hop (lambda (arg) (continuation-return cont arg)))
                                                          body ...))))))

  (define (callcc f) (letcc k (f k)))

  (define-syntax resetcc (syntax-rules () ((resetcc body ...) (callcc (delimcc-reset (τ body ...))))))
  (define-syntax letcc/shift (syntax-rules () ((letcc/shift k body ...) (callcc (delimcc-shift (lambda (k) body ...))))))
  (define (callshiftcc f) (letcc/shift k (f k)))
  (define-values (delimcc-reset delimcc-shift)
    (let1 (cont (lambda (v) v)) ;(let1 (cont (lambda (v) (error "Missing enclosing resetcc" v)))
          (define (delimcc-abort v) (cont v))
          (define (delimcc-cont-push! k) (set! cont k))
          (define (delimcc-cont-pop!) cont)
          (define ((resetk t) k)
            (let1 (s (delimcc-cont-pop!))
                  (delimcc-cont-push! (lambda (r)
                                        (delimcc-cont-push! s)
                                        (k r)))
                  (delimcc-abort (t))))
          (define ((shiftk h) k) (delimcc-abort (h (lambda (v) (resetcc (k v))))))
          (values resetk shiftk)))

  (define-syntax define-resetcc (syntax-rules () ((define-resetcc def body ...) (define def (resetcc body ...)))))
  (define (delimcc-extract) (letcc/shift k k))
  (define (delimcc-discard v) (letcc/shift _ v))
  (define-syntax delimcc-τ 
    (syntax-rules () 
      ((delimcc-τ body ...) (letcc/shift k (τ (let1 (x (begin body ...)) (k x)))))))
  (define-syntax delimcc-lambda 
    (syntax-rules () 
      ((delimcc-lambda args body ...) (letcc/shift k (lambda args (let1 (x (begin body ...)) (k x)))))))
  (define (delimcc-either lst) (letcc/shift k (map k lst)))

  (define-syntax resetcc+null (syntax-rules () ((resetcc+null body ...) (resetcc body ... '()))))
  (define (yield v) (letcc/shift k (cons v (k (void)))))
  (define (yield/extract v) (letcc/shift k (cons v k)))


  (define-syntax delimcc-foldr
    (syntax-rules ()
      ((delimcc-fold bexpr ((each acc) fbody ...) body ...)
       (let* ((witness (gensym))
              (b bexpr)
              (f (lambda (each acc) fbody ...)))
         (define (L r)
           (cond
             ((eq? r witness) b)
             (else (f (car r) (L (let1 (k (cdr r)) (k (void))))))))
         (L (resetcc body ... witness))))))

  (define-syntax letcc*
    (syntax-rules ()
      ((letcc* next ((v exp ...) ...) body ...)
       (letcc success
              (let* ((v (letcc next (success (begin exp ...)))) ...
                     (next success))
                body ...)))))

  (define-syntax trycc
    (syntax-rules (else)
      ((trycc (next exp ...) (else body ...))
       (letcc* hop ((v (let1 (next (τ (hop (void)))) exp)) ...)
               body ...))))


  )






















