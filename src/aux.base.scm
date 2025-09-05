
(module (aux base) *

  (import scheme 
          (chicken base) 
          (chicken continuation) 
          (chicken pretty-print) 
          (chicken fixnum) 
          (chicken sort) 
          (chicken port) 
          (chicken foreign)
          srfi-1
          srfi-69)

  (define-syntax letport/string 
    (syntax-rules (out else) 
      ((_ (p out) body ...) (let* ((v (void))
                                   (s (call-with-output-string (λ (p) (set! v (begin body ...))))))
                              (values v s)))
      ((_ (p instring) body ...) (let* ((v (void))
                                        (s (call-with-input-string instring (λ (p) (set! v (begin body ...))))))
                                   (values v s)))
      ((_ else body ...) (let* ((v (void))
                                (s (with-error-output-to-string (τ (set! v (begin body ...))))))
                           (values v s)))))

  (define-syntax push! 
    (syntax-rules () 
      ((push! val var) (begin (set! var (cons val var)) (void)))))

  (define-syntax pop! (syntax-rules () ((pop! var) (let ((a (car var))) (set! var (cdr var)) a))))
  (define-syntax append-right! 
    (syntax-rules () 
      ((append-right! lst another ... var) (begin (set! var (append var lst another ...)) (void)))))
  (define-syntax add1! (syntax-rules () ((_ var) (begin (set! var (add1 var)) (void)))))
  (define-syntax sub1! (syntax-rules () ((_ var) (begin (set! var (sub1 var)) (void)))))

  (define-syntax λ (syntax-rules () ((λ formals body ...) (lambda formals body ...))))
  (define-syntax λ/_ (syntax-rules () ((λ/_ body ...) (λ formals body ...))))
  (define-syntax τ (syntax-rules () ((τ body ...) (λ () body ...))))
  (define-syntax define-τ (syntax-rules () ((define-τ name body ...) (define name (τ body ...)))))
  (define-syntax letgensym (syntax-rules () ((letgensym (var ...) body ...) (let ((var (gensym)) ...) body ...))))

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

  (define (mappair f lst)
    (cond
      ((or (null? lst) (null? (cdr lst))) '())
      (else (cons (f (car lst) (cadr lst)) (mappair f (cdr lst))))))

  (define (curry f g)
    (λ args (apply f (cons g args)))) 

  (define-syntax let1 
    (syntax-rules () 
      ((let1 (var val) body ...) (let ((var val)) body ...))
      ((let1 var body ...) (let1 (var (void)) body ...))))

  (define (foldr/yielded f t init)
    (cond
      ((null? t) init)
      (else (letcar&cdr (((v k) t))
                        (f v (foldr/yielded f (k (void)) init))))))

  (define (memoize/call f)
    (let ((called #f) (memo (void)))
      (λ args
          (unless called 
            (set! memo (apply f args))
            (set! called #t))
          memo)))

  (define (memoize/arg f)
    (let1 (memo (make-hash-table))
          (λ (arg)
              (unless (hash-table-exists? memo arg) (hash-table-set! memo arg (f arg)))
              (hash-table-ref memo arg))))

  (define-syntax λ-memo
    (syntax-rules ()
      ((_ args body ...) (let ((memo (make-hash-table))
                               (f (λ args body ...)))
                           (λ vargs
                               (unless (hash-table-exists? memo vargs) 
                                 (hash-table-set! memo vargs (apply f vargs)))
                               (hash-table-ref memo vargs))))))
  (define-syntax define-memo
    (syntax-rules ()
      ((_ (name arg ...) body ...) (define name (λ-memo (arg ...) body ...)))))

  (define ((boolean->P prob) bool) (if bool prob (- 1 prob)))

  (define boolean->01 (boolean->P 1))

  (define ((indicator set) v) (member? v set))

  (define (pairwise-different? lst)
    (cond
      ((null? lst) #t)  ; Empty list, all elements are trivially different
      ((member? (car lst) (cdr lst)) #f)  ; First element is found in the rest of the list
      (else (pairwise-different? (cdr lst)))))  ; Recur on the rest of the list


  )


































