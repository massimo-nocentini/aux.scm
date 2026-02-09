
(module (aux stream) *

  (import scheme 
          (chicken base)
          (aux base)
          (aux continuation)
          (aux continuation delimited)
  )

  (define-syntax cons§ (syntax-rules () ((_ a d) (delay (cons a d)))))
  (define-syntax yield§ (syntax-rules () ((yield§ body) (letcc/shift k (cons§ body (k (void)))))))
  (define (yield§ª v) (yield§ v))

  (define (map§/yielded f t)
    (cond
      ((null? t) '())
      (else (letcar&cdr (((v k) t))
                        (cons§ (f v) (map§/yielded f (k (void))))))))

  #;(define-syntax delimcc-foldr§
    (syntax-rules ()
      ((delimcc-fold body ...)
       (let1 (witness (gensym))
             (define (L r)
               (cond
                 ((eq? r witness) '())
                 (else (cons§ (car r) (L (let1 (k (cdr r)) (k (void))))))))
             (L (resetcc body ... witness))))))

  (define (§->list s)
    (cond
      ((promise? s) (§->list (force s)))
      ((pair? s) (cons (car s) (§->list (cdr s))))
      (else s)))

  (define (take§ n s)
    (cond
      ((zero? n) '())
      ((promise? s) (delay (take§ n (force s))))
      ((pair? s) (cons§ (car s) (take§ (sub1 n) (cdr s))))
      (else s)))

  (define (map§ f s)
    (cond
      ((promise? s) (delay (map§ f (force s))))
      ((pair? s) (cons§ (f (car s)) (map§ f (cdr s))))
      (else s)))

  (define (filter§ p s)
    (cond
      ((promise? s) (delay (filter§ p (force s))))
      ((pair? s) (letcar&cdr (((a d) s)) (if (p a) (cons§ a (filter§ p d)) (delay (filter§ p d)))))
      (else s)))

  (define (zip§ f r s)
    (cond
      ((and (promise? r) (promise? s)) (delay (zip§ f (force r) (force s))))
      ((promise? r) (delay (zip§ f (force r) s)))
      ((promise? s) (delay (zip§ f r (force s))))
      ((and (pair? r) (pair? s)) (cons§ (f (car r) (car s)) (zip§ f (cdr r) (cdr s))))
      (else '())))

  (define (foldr§ f init s)
    (cond
      ((promise? s) (foldr§ f init (force s)))
      ((pair? s) (f (car s) (foldr§ f init (cdr s))))
      (else init)))

  (define (cdr§ s)
    (cond
      ((promise? s) (delay (cdr§ (force s))))
      (else (cdr s))))

  (define (car§ s)
    (cond
      ((promise? s) (car§ (force s)))
      (else (car s))))

  (define append§/interleaved/2
    (λ (s1 s2)
        (cond
          ((null? s1) s2)
          ((promise? s1) (delay (append§/interleaved/2 s2 (force s1))))
          (else (cons§ (car s1) (append§/interleaved/2 (cdr s1) s2))))))

  (define append§/interleaved
    (lambda list-of-choices
      (cond
        ((null? list-of-choices) '())
        (else (letcar&cdr (((choices rest-of-choices) list-of-choices))
                          (cond
                            ((null? choices) (apply append§/interleaved rest-of-choices))
                            ((promise? choices) (delay (apply append§/interleaved (append rest-of-choices (list (force choices))))))
                            ((pair? choices) (cons§ (car choices) (apply append§/interleaved (append rest-of-choices (list (cdr choices))))))
                            (else '())))))))

  (define (append-map§ f $)
    (cond
      ((null? $) '())
      ((promise? $) (delay (append-map§ f (force $))))
      (else (append§/interleaved/2 (delay (f (car $))) (append-map§ f (cdr $))))))

  (define (const§ s) (rec N (cons§ s N)))
  (define (nats§ s) (rec N (cons§ s (map§ add1 N))))
  (define ℕ (nats§ 0))
  (define (gfibs§ f m n) (rec F (cons§ m (cons§ n (zip§ f F (cdr§ F))))))
  (define (fibs§ m n) (gfibs§ + m n))
  (define primes§
    (let P ((s (nats§ 2)))
      (let1 (p (car§ s)) 
            (cons§ p (P (filter§ (lambda (n) (not (zero? (modulo n p)))) (cdr§ s)))))))

  (define 1§ (const§ 1))

  (define (τ->§ t) (cons§ (t) (τ->§ t)))
  (define-syntax thunk§ (syntax-rules () ((thunk§ body ...) (τ->§ (τ body ...)))))

  (define (stop§ pred? §)
    (cond
      ((promise? §) (delay (stop§ pred? (force §))))
      ((or (null? §) (pred? (car §))) '())
      (else (cons§ (car §) (stop§ pred? (cdr§ §))))))

  )



















































