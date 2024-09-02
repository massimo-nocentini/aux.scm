
(module aux *

  (import scheme (chicken base) (chicken continuation) (chicken condition))

  (define-syntax let1 
    (syntax-rules ()
      ((let1 (var val) body ...) (let ((var val)) body ...))))

  (define-syntax letcc 
    (syntax-rules ()
      ((letcc (var ...) hop body ...) (letcc hop (begin (set! var hop) ...) body ...))
      ((letcc hop body ...) 
       (continuation-capture (lambda (cont)
                              (let ((hop (lambda (arg) (continuation-return cont arg))))
                                body ...))))))
                          
  (define (callcc f) (letcc k (f k)))

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

  (define-syntax λ (syntax-rules () ((λ arg body ...) (lambda arg body ...))))
  (define-syntax thunk (syntax-rules () ((thunk body ...) (lambda () body ...))))
  (define-syntax λ0 (syntax-rules () ((λ0 body ...) (lambda () body ...))))
  (define-syntax λ1 (syntax-rules () ((λ1 arg body ...) (lambda (arg) body ...))))
  (define-syntax letgensym (syntax-rules () ((letgensym (var ...) body ...) (let ((var (gensym)) ...) body ...))))

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

  (define-syntax push! (syntax-rules () ((push! val var) (begin (set! var (cons val var)) (void)))))
  (define-syntax pop! (syntax-rules () ((pop! var) (let ((a (car var))) (set! var (cdr var)) a))))
  (define-syntax append! (syntax-rules () ((append! lst another ... var) (begin (set! var (append var lst another ...)) (void)))))
  (define-syntax sub1! (syntax-rules () ((sub1! var) (begin (set! var (sub1 var)) (void)))))

  (define (member? v lst) (pair? (member v lst)))

  (define-syntax letassoc 
   (syntax-rules (else) 
    ((letassoc (searchexpr lstexpr) (else body ...))
     (let1 (p (assoc searchexpr lstexpr))
      (if (pair? p) (cadr p) (begin body ...))))))

  (define-syntax letnondeterministic 
    (syntax-rules ()
     ((_ ((chooseD chooseB)) body ...) (letnondeterministic (chooseD chooseB fail markD cutD) body ...))
     ((_ (choose) body ...) (letnondeterministic (choose chooseB fail markD cutD) body ...))
     ((_ ((chooseD chooseB) fail) body ...) (letnondeterministic (chooseD chooseB fail markD cutD) body ...))
     ((_ ((chooseD chooseB) fail markD cutD) body ...) (letnondeterministic (chooseD chooseB fail markD cutD) body ...))
     ((_ (choose fail) body ...) (letnondeterministic (choose chooseB fail markD cutD) body ...))
     ((_ (choose fail markD cutD) body ...) (letnondeterministic (choose chooseB fail markD cutD) body ...))
     ((_ (chooseD chooseB fail markD cutD) body ...)
      (letcc cc
        (letrec ((nremaining -1)
                 (pool '())
                 (values '())
                 (fail (thunk
                         (if (or (zero? nremaining) (null? pool))
                           (cc (reverse values)) 
                           (let1 (t (pop! pool))
                             (cond 
                              ((symbol? t) (fail))
                              ((procedure? t) (t))
                              (else
                               (push! (gensym) pool)
                               (set-cdr! pool '())
                               (fail)))))))
                 (chooseD (lambda (choices)
                           (if (null? choices)
                             (fail)
                             (letcc kk
                               (push! (thunk (kk (chooseD (cdr choices)))) pool)
                               (car choices)))))
                 (chooseB (lambda (choices)
                           (letcc kk
                            (append! (map (lambda (c) (thunk (kk c))) choices) pool)
                            (fail))))
                 (markD (thunk (letgensym (flag) (push! flag pool) flag)))
                 (markB (thunk (letgensym (flag) 
                                (let1 (witness (list flag)) 
                                 (append! (list witness) pool)
                                 witness))))
                 (cutD (lambda (flag)
                        (if (null? pool)
                          (void)
                          (let1 (a (pop! pool))
                            (if (eq? a flag) (void) (cutD flag))))))
                 (cutB (lambda (flag)
                        (unless (null? pool) (set-cdr! pool '()))
                        (void)
                        #;(letrec ((A (lambda (lst)
                                     (cond
                                      ((or (null? lst) (null? (cdr lst))) (void))
                                      ((eq? (cadr lst) flag) (display 'found) (set-cdr! lst '()) (void))
                                      (else (A (cdr lst)))))))
                          (A pool)))))
          
          (push! (let () body ...) values)
          (sub1! nremaining)
          (fail))))))

  
)