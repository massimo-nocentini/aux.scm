
(module aux *

  (import scheme (chicken base) (chicken continuation) (chicken condition))

  (define-syntax push! (syntax-rules () ((push! val var) (begin (set! var (cons val var)) (void)))))
  (define-syntax pop! (syntax-rules () ((pop! var) (let ((a (car var))) (set! var (cdr var)) a))))
  (define-syntax append! (syntax-rules () ((append! lst another ... var) (begin (set! var (append var lst another ...)) (void)))))
  (define-syntax sub1! (syntax-rules () ((sub1! var) (begin (set! var (sub1 var)) (void)))))

  (define-syntax λ (syntax-rules () ((λ arg body ...) (lambda arg body ...))))
  (define-syntax thunk (syntax-rules () ((thunk body ...) (lambda () body ...))))
  (define-syntax λ0 (syntax-rules () ((λ0 body ...) (lambda () body ...))))
  (define-syntax λ1 (syntax-rules () ((λ1 arg body ...) (lambda (arg) body ...))))
  (define-syntax letgensym (syntax-rules () ((letgensym (var ...) body ...) (let ((var (gensym)) ...) body ...))))

  (define-syntax cons§
   (syntax-rules ()
    ((_ a d) (delay (cons a d)))))

  (define-syntax let1 
    (syntax-rules ()
      ((let1 (var val) body ...) (let ((var val)) body ...))))

  (define-syntax letcc
    (syntax-rules ()
      ((letcc (var ...) hop body ...) (letcc hop (begin (set! var hop) ...) body ...))
      ((letcc hop body ...) 
       (continuation-capture (lambda (cont) 
                              (let1 (hop (lambda (arg) (continuation-return cont arg))) 
                               body ...))))))

  (define (callcc f) (letcc k (f k)))
  (define-syntax definecc (syntax-rules () ((definecc k var body ...) (define var (letcc k body ...)))))

  (define delimcc-cont (lambda (v) (void)))
  (define (delimcc-abort v) (delimcc-cont v))
  (define (delimcc-cont-set! f) (set! delimcc-cont f))

  (define ((delimcc-reset t) k)
                     (let1 (m delimcc-cont)
                      (delimcc-cont-set! (lambda (r) 
                                          (delimcc-cont-set! m) 
                                          (k r)))
                      (delimcc-abort (t))))
  (define-syntax reset (syntax-rules () ((reset body ...) (callcc (delimcc-reset (thunk body ...))))))
  
  (define ((delimcc-shift h) k) (delimcc-abort (h (lambda (v) (reset (k v))))))
  (define-syntax letshiftcc (syntax-rules () ((letshiftcc k body ...) (callcc (delimcc-shift (lambda (k) body ...))))))
  (define (delimcc-extract) (letshiftcc k k))
  (define (delimcc-discard v) (letshiftcc _ v))
  (define (delimcc-cons v) (letshiftcc k (cons v k)))

  (define-syntax resetnull (syntax-rules () ((_ body ...) (reset body ... '()))))
  (define (yield x) (letshiftcc k (cons x (k (void)))))
  (define (yield§ x) (letshiftcc k (cons§ x (k (void)))))

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
       (letcc* hop ((v (let1 (next (λ0 (hop (void)))) exp)) ...)
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

  (define ((nondeterministic system R nr) cc)
        (letrec ((nremaining nr)
                 (pool '())
                 (values '())
                 (fail (thunk
                         (cond 
                          ((or (zero? nremaining) (null? pool)) (cc (reverse values)))
                          (else (let1 (t (pop! pool))
                                 (cond
                                  ((procedure? t) (t))
                                  (else (fail))))))))
                 (chooseD (lambda list-of-choices
                           (¶ (pair? list-of-choices))
                           (letcar&cdr (((choices rest-of-choices) list-of-choices))
                            (cond
                             ((null? choices) (apply chooseD rest-of-choices))
                             ((promise? choices) (apply chooseD `(,@rest-of-choices ,(force choices))))
                             ((pair? choices) (letcc kk
                                               (push! (thunk (kk (apply chooseD `(,@rest-of-choices ,(cdr choices))))) pool)
                                               (car choices)))
                             (else (fail))))))
                 (chooseB (lambda (choices)
                           (cond
                            ((promise? choices) (letcc kk
                                                  (append! (list (thunk (kk (chooseB (force choices))))) pool)
                                                  (chooseB '())))
                            ((pair? choices) (letcc kk
                                              (append! (list (thunk (kk (car choices))) (thunk (kk (chooseB (cdr choices))))) pool)
                                              (chooseB '())))
                            (else (fail)))))
                 (markD (thunk (letgensym (flag) (push! flag pool) flag)))
                 (cutD (lambda (flag)
                        (cond 
                         ((null? pool) (void))
                         (else (let1 (a (pop! pool))
                                (if (eq? a flag) (void) (cutD flag)))))))
                 (¶ (lambda (b) (unless b (fail)))))

          (let1 (v (system chooseD chooseB fail markD cutD ¶))
           (push! v values) 
           (sub1! nremaining)
           (R values fail))))

  (define-syntax letnondeterministic
    (syntax-rules ()
      ((_ ((chooseD chooseB fail markD cutD asserter) body ...) ((arg next) lbody ...))
       (letnondeterministic -1 ((chooseD chooseB fail markD cutD asserter) body ...) ((arg next) lbody ...)))
      ((_ (chooseD chooseB fail markD cutD asserter) body ...)
       (letnondeterministic -1 (chooseD chooseB fail markD cutD asserter) body ...))
      ((_ nr ((chooseD chooseB fail markD cutD asserter) body ...) ((arg next) lbody ...))
       (callcc (nondeterministic (lambda (chooseD chooseB fail markD cutD asserter) body ...) (lambda (arg next) lbody ...) nr)))
      ((_ nr (chooseD chooseB fail markD cutD asserter) body ...) 
       (letnondeterministic nr ((chooseD chooseB fail markD cutD asserter) body ...) ((arg next) (next))))))

  (define-syntax define-nondeterministic
   (syntax-rules ()
    ((_ (q (chooseD chooseB fail markD cutD asserter)) body ...)
     (begin
      (define q (void))
      (letnondeterministic
                  ((chooseD chooseB fail markD cutD asserter) (set! q fail) body ...)
                  ((v next) v))))))

  (define (memoize f)
   (let ((called #f) (memo (void)))
    (lambda args
     (if called 
      memo 
      (begin 
       (set! memo (apply f args))
       (set! called #t)
       memo)))))

  (define-syntax lambdamemo
   (syntax-rules ()
    ((_ args body ...) (memoize (lambda args body ...)))))

)