
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

  (define-syntax letmap
    (syntax-rules ()
        ((letmap () body ...) (begin body ...))
        ((letmap ((x expr) (xx exprr) ...) body ...) 
         (map (lambda (x) (letmap ((xx exprr) ...) body ...)) expr))))

  (define-syntax letmapflat
    (syntax-rules ()
        ((letmapflat () body ...) (list (begin body ...)))
        ((letmapflat ((x expr) (xx exprr) ...) body ...) 
         (apply append (map (lambda (x) (letmapflat ((xx exprr) ...) body ...)) expr)))))

  (define-syntax push! (syntax-rules () ((push! val var) (begin (set! var (cons val var)) (void)))))
  (define-syntax pop! (syntax-rules () ((pop! var) (let ((a (car var))) (set! var (cdr var)) a))))
  (define-syntax append! (syntax-rules () ((append! lst another ... var) (begin (set! var (append var lst another ...)) (void)))))

  (define (member? v lst) (pair? (member v lst)))

  (define-syntax letassoc 
   (syntax-rules (else) 
    ((letassoc (searchexpr lstexpr) (else body ...))
     (let1 (p (assoc searchexpr lstexpr))
      (if (pair? p) (cadr p) (begin body ...))))))

  (define-syntax letnondeterministic 
    (syntax-rules ()
     ((_ (chooseD chooseB fail markD markB cut) body ...)
      (letcc cc
        (letrec ((pool '())
                 (values '())
                 (fail (thunk
                         (if (null? pool) 
                           (cc (reverse values)) 
                           (let1 (t (pop! pool))
                             (if (symbol? t) (fail) (t))))))
                 (chooseD (lambda (choices)
                           (if (null? choices)
                             (fail)
                             (letcc kk
                               (push! (thunk (kk (chooseD (cdr choices)))) pool)
                               (car choices)))))
                 (markD (thunk (letgensym (flag) (push! flag pool) flag)))
                 (chooseB (lambda (choices)
                           (letcc kk
                            (append! (map (lambda (c) (thunk (kk c))) choices) pool)
                            (fail))))
                 (markB (thunk (letgensym (flag) (append! (list flag) pool) flag)))
                 (cut (lambda (flag)
                        (if (null? pool)
                          (void)
                          (let1 (a (pop! pool))
                            (if (eq? a flag) (void) (cut flag)))))))
          (push! (let () body ...) values)
          (fail))))))

  
)