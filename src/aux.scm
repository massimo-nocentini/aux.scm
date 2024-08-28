
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

  (define-syntax push! (syntax-rules () ((push! val var) (begin (set! var (cons val var)) var))))
  (define-syntax pop! (syntax-rules () ((pop! var) (let ((a (car var))) (set! var (cdr var)) a))))
  (define-syntax append! (syntax-rules () ((append! lst ... var) (set! var (append var lst ...)))))
  
)