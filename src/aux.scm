
(module aux *

  (import scheme (chicken base) (chicken continuation) (chicken pretty-print) (chicken fixnum))

  (define-syntax push! (syntax-rules () ((push! val var) (begin (set! var (cons val var)) (void)))))
  (define-syntax pop! (syntax-rules () ((pop! var) (let ((a (car var))) (set! var (cdr var)) a))))
  (define-syntax append-right! (syntax-rules () ((append-right! lst another ... var) (begin (set! var (append var lst another ...)) (void)))))
  (define-syntax sub1! (syntax-rules () ((sub1! var) (begin (set! var (sub1 var)) (void)))))

  (define-syntax λ (syntax-rules () ((λ formals body ...) (lambda formals body ...))))
  (define-syntax τ (syntax-rules () ((τ body ...) (lambda () body ...))))
  (define-syntax letgensym (syntax-rules () ((letgensym (var ...) body ...) (let ((var (gensym)) ...) body ...))))

  (define-syntax cons§ (syntax-rules () ((_ a d) (delay (cons a d)))))
  (define-syntax let1 (syntax-rules () ((let1 (var val) body ...) (let ((var val)) body ...))))

  (define-syntax letcc
   (syntax-rules ()
    ((letcc hop body ...) (continuation-capture (lambda (cont) 
                                                 (let1 (hop (lambda (arg) (continuation-return cont arg)))
                                                  body ...))))))

  (define (callcc f) (letcc k (f k)))
  
  (define-syntax resetcc (syntax-rules () ((resetcc body ...) (callcc (delimcc-reset (τ body ...))))))
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
  (define-syntax delimcc-τ (syntax-rules () ((delimcc-τ body ...) (letshiftcc k (τ (let1 (x (begin body ...)) (k x)))))))
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
       (letcc* hop ((v (let1 (next (τ (hop (void)))) exp)) ...)
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

  (define interleave§
   (lambda list-of-choices
    (cond
     ((null? list-of-choices) '())
     (else (letcar&cdr (((choices rest-of-choices) list-of-choices))
            (cond
             ((null? choices) (apply interleave§ rest-of-choices))
             ((promise? choices) (delay (apply interleave§ (append rest-of-choices (list (force choices))))))
             ((pair? choices) (cons§ (car choices) (apply interleave§ (append rest-of-choices (list (cdr choices))))))
             (else '())))))))

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

  (define ((nondeterministic name system) cc)
        (letrec ((pool '())
                 (stats (cons 0 1)) ; `car` counts accepted, `cdr` counts tried.
                 (fail (τ
                         (cond 
                          ((null? pool) (pretty-print `(,name
                                                        ((tried ,(cdr stats))
                                                         (accepted ,(car stats)) 
                                                         (ratio ,(exact->inexact (/ (car stats) (cdr stats)))))))
                                        (cc stats))
                          (else (let1 (t (pop! pool))
                                 (cond
                                  ((procedure? t) (set-cdr! stats (add1 (cdr stats))) (t))
                                  (else (fail))))))))
                 (chooseD (lambda (choices #!optional (continue? (λ (v) #t)))
                             (cond
                              ((null? choices) (fail))
                              ((promise? choices) (chooseD (force choices) continue?))
                              ((pair? choices) (letcc kk
                                                (let1 (v (car choices))
                                                 (push! (τ (if (continue? v) (kk (chooseD (cdr choices) continue?)) (fail))) pool)
                                                 (push! (τ (kk v)) pool))
                                                (fail)))
                              (else (fail)))))
                 (chooseB (lambda (choices)
                           (cond
                            ((promise? choices) (letcc kk
                                                  (append-right! (list (τ (kk (chooseB (force choices))))) pool)
                                                  (chooseB '())))
                            ((pair? choices) (letcc kk
                                              (append-right! (list (τ (kk (car choices))) (τ (kk (chooseB (cdr choices))))) pool)
                                              (chooseB '())))
                            (else (fail)))))
                 (markD (τ (letgensym (flag) (push! flag pool) flag)))
                 (cutD (lambda (flag)
                        (cond 
                         ((null? pool) (void))
                         (else (let1 (a (pop! pool))
                                (if (eq? a flag) (void) (cutD flag)))))))
                 (⊦ (lambda (bool) (unless bool (fail)))))

          (let1 (v (system chooseD chooseB ⊦ markD cutD))
           (set-car! stats (add1 (car stats)))
           (yield§ v)
           (fail))))

  (define-syntax letnondeterministic§
    (syntax-rules ()
      ((letnondeterministic§ (chooseD chooseB asserter markD cutD) body ...)
       (letnondeterministic§ (gensym) (chooseD chooseB asserter markD cutD) body ...))
      ((letnondeterministic§ name (chooseD chooseB asserter markD cutD) body ...)
       (resetcc+null (callcc (nondeterministic name (λ (chooseD chooseB asserter markD cutD) body ...)))))))

  (define-syntax letnondeterministic
    (syntax-rules ()
     ((_ (chooseD chooseB asserter markD cutD) body ...)
      (letnondeterministic -1 (chooseD chooseB asserter markD cutD) body ...))
     ((_ nr (chooseD chooseB asserter markD cutD) body ...)
      (letnondeterministic nr (gensym) (chooseD chooseB asserter markD cutD) body ...))
     ((_ nr name (chooseD chooseB asserter markD cutD) body ...)
      (§->list (take§ nr (letnondeterministic§ name (chooseD chooseB asserter markD cutD) body ...))))))

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
  
  (define ((indicator set) v) (member? v set))
  
  (define (pairwise-different? lst)
   (cond
    ((null? lst) #t)  ; Empty list, all elements are trivially different
    ((member? (car lst) (cdr lst)) #f)  ; First element is found in the rest of the list
    (else (pairwise-different? (cdr lst)))))  ; Recur on the rest of the list

)