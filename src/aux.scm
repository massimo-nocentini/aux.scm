
(module aux *

  (import scheme (chicken base) (chicken continuation))

  (define-syntax push! (syntax-rules () ((push! val var) (begin (set! var (cons val var)) (void)))))
  (define-syntax pop! (syntax-rules () ((pop! var) (let ((a (car var))) (set! var (cdr var)) a))))
  (define-syntax append-right! (syntax-rules () ((append-right! lst another ... var) (begin (set! var (append var lst another ...)) (void)))))
  (define-syntax sub1! (syntax-rules () ((sub1! var) (begin (set! var (sub1 var)) (void)))))

  (define-syntax λ (syntax-rules () ((λ formals body ...) (lambda formals body ...))))
  (define-syntax thunk (syntax-rules () ((thunk body ...) (lambda () body ...))))
  (define-syntax letgensym (syntax-rules () ((letgensym (var ...) body ...) (let ((var (gensym)) ...) body ...))))

  (define-syntax cons§ (syntax-rules () ((_ a d) (delay (cons a d)))))
  (define-syntax let1 (syntax-rules () ((let1 (var val) body ...) (let ((var val)) body ...))))

  (define-syntax letcc
   (syntax-rules ()
    ((letcc hop body ...) (continuation-capture (lambda (cont) 
                                                 (let1 (hop (lambda (arg) (continuation-return cont arg)))
                                                  body ...))))))

  (define (callcc f) (letcc k (f k)))
  
  (define-syntax resetcc (syntax-rules () ((resetcc body ...) (callcc (delimcc-reset (thunk body ...))))))
  (define-syntax letshiftcc (syntax-rules () ((letshiftcc k body ...) (callcc (delimcc-shift (lambda (k) body ...))))))
  
  (define-values (delimcc-reset delimcc-shift)
   (let1 (cont (lambda (v) (error "Missing enclosing resetcc" v)))
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
  (define (delimcc-extract) (letshiftcc k k))
  (define (delimcc-discard v) (letshiftcc _ v))
  (define (delimcc-cons v) (letshiftcc k (cons v k)))
  (define-syntax delimcc-thunk (syntax-rules () ((delimcc-thunk body ...) (letshiftcc k (thunk (let1 (x (begin body ...)) (k x)))))))
  (define-syntax delimcc-lambda (syntax-rules () ((delimcc-lambda args body ...) (letshiftcc k (lambda args (let1 (x (begin body ...)) (k x)))))))
  (define (delimcc-either lst) (letshiftcc k (map k lst)))

  (define-syntax resetcc+null (syntax-rules () ((resetcc+null body ...) (resetcc body ... '()))))
  (define (yield v) (letshiftcc k (cons v (k (void)))))
  (define-syntax yield§ (syntax-rules () ((yield§ body) (letshiftcc k (cons§ body (k (void)))))))

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
  
  (define-syntax delimcc-foldr§
   (syntax-rules ()
    ((delimcc-fold body ...)
         (let1 (witness (gensym))
          (define (L r)
           (cond
            ((eq? r witness) '())
            (else (cons§ (car r) (L (let1 (k (cdr r)) (k (void))))))))
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
       (letcc* hop ((v (let1 (next (thunk (hop (void)))) exp)) ...)
         body ...))))

  (define-syntax letcar&cdr
    (syntax-rules ()
      ((letcar&cdr () body ...) (begin body ...))        
      ((letcar&cdr (((a d) expr) ((aa dd) eexpr) ...) body ...)
       (let* ((x expr) (a (car x)) (d (cdr x)))
         (letcar&cdr (((aa dd) eexpr) ...) body ...)))))

  (define-syntax define-let
    (syntax-rules ()
     ((define-let ((v e) ...) (name formal ...) body ...)
      (define name (let ((v e) ...) (lambda (formal ...) body ...))))))

  (define-syntax lettensor
    (syntax-rules ()
        ((lettensor f () body ...) (begin body ...))
        ((lettensor f ((x expr) (xx exprr) ...) body ...) 
         (f (lambda (x) (lettensor f ((xx exprr) ...) body ...)) expr))))

  (define-syntax letmaptensor
    (syntax-rules () ((letmaptensor ((x expr) ...) body ...) (lettensor map ((x expr) ...) body ...))))

  (define-syntax letmap
    (syntax-rules ()
        ((letmap () body ...) (list (begin body ...)))
        ((letmap ((x expr) (xx exprr) ...) body ...) 
         (apply append (map (lambda (x) (letmap ((xx exprr) ...) body ...)) expr)))))

  (define (member? v lst) (pair? (member v lst)))

  (define-syntax letassoc 
   (syntax-rules (else) 
    ((letassoc (searchexpr lstexpr) (else body ...))
     (let1 (p (assoc searchexpr lstexpr))
      (if (pair? p) (cadr p) (begin body ...))))))

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

  (define (cdr§ s)
    (cond
     ((promise? s) (cdr§ (force s)))
     ((pair? s) (cdr s))
     (else (void))))
  
  (define (car§ s)
    (cond
     ((promise? s) (car§ (force s)))
     ((pair? s) (car s))
     (else (void))))

  (define (const§ s) (rec N (cons§ s N)))
  (define (nats§ s) (rec N (cons§ s (map§ add1 N))))
  (define (gfibs§ f m n) (rec F (cons§ m (cons§ n (zip§ f F (cdr§ F))))))
  (define (fibs§ m n) (gfibs§ + m n))
  (define primes§
   (let P ((s (nats§ 2)))
    (let1 (p (car§ s)) 
     (cons§ p (P (filter§ (lambda (n) (not (zero? (modulo n p)))) (cdr§ s)))))))

  (define (thunk->§ t) (cons§ (t) (thunk->§ t)))
  (define-syntax thunk§ (syntax-rules () ((thunk§ body ...) (thunk->§ (thunk body ...)))))

  (define (stop§ pred? §)
    (cond
     ((promise? §) (delay (stop§ pred? (force §))))
     ((or (null? §) (pred? (car §))) '())
     (else (cons§ (car §) (stop§ pred? (cdr§ §))))))

  (define ((nondeterministic system) cc)
        (letrec ((pool '())
                 (values '())
                 (fail (thunk
                         (cond 
                          ((null? pool) (cc (reverse values)))
                          (else (let1 (t (pop! pool))
                                 (cond
                                  ((procedure? t) (t))
                                  (else (fail))))))))
                 (chooseD (lambda list-of-choices
                           (¶ (pair? list-of-choices))
                           (letcar&cdr (((choices rest-of-choices) list-of-choices))
                            (cond
                             ((null? choices) (apply chooseD rest-of-choices))
                             ((promise? choices) (apply chooseD (append rest-of-choices (list (force choices)))))
                             ((pair? choices) (letcc kk
                                               (push! (thunk (kk (apply chooseD (append rest-of-choices (list (cdr choices)))))) pool)
                                               (car choices)))
                             (else (fail))))))
                 (chooseB (lambda (choices)
                           (cond
                            ((promise? choices) (letcc kk
                                                  (append-right! (list (thunk (kk (chooseB (force choices))))) pool)
                                                  (chooseB '())))
                            ((pair? choices) (letcc kk
                                              (append-right! (list (thunk (kk (car choices))) (thunk (kk (chooseB (cdr choices))))) pool)
                                              (chooseB '())))
                            (else (fail)))))
                 (markD (thunk (letgensym (flag) (push! flag pool) flag)))
                 (cutD (lambda (flag)
                        (cond 
                         ((null? pool) (void))
                         (else (let1 (a (pop! pool))
                                (if (eq? a flag) (void) (cutD flag)))))))
                 (¶ (lambda (b) (unless b (fail)))))

          (let1 (v (system chooseD chooseB ¶ markD cutD))
           (push! v values)
           (yield§ v)
           (fail))))

  (define-syntax letnondeterministic§
    (syntax-rules ()
      ((_ (chooseD chooseB asserter markD cutD) body ...)
       (resetcc+null (callcc (nondeterministic (λ (chooseD chooseB asserter markD cutD) body ...)))))))

  (define-syntax letnondeterministic
    (syntax-rules ()
     ((_ (chooseD chooseB asserter markD cutD) body ...)
      (letnondeterministic -1 (chooseD chooseB asserter markD cutD) body ...))
     ((_ nr (chooseD chooseB asserter markD cutD) body ...)
      (§->list (take§ nr (letnondeterministic§ (chooseD chooseB asserter markD cutD) body ...))))))

  (define (memoize f)
   (let ((called #f) (memo (void)))
    (λ args
     (unless called 
      (set! memo (apply f args))
      (set! called #t))
     memo)))

  (define-syntax lambdamemo
   (syntax-rules ()
    ((_ args body ...) (memoize (lambda args body ...)))))

  (define ((boolean->P prob) bool) (if bool prob (- 1 prob)))
  (define boolean->01 (boolean->P 1))

)