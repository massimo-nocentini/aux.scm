
(import scheme (chicken base) (chicken memory representation) (aux base) (aux unittest) (aux match))

(define-suite dmatch-suite

  ((doc r) `((structure/section "Implementation")
             (code/scheme/file "../aux.match.scm")))

  ((test/base-non-overlapping _)
      (⊦= 'empty (match/non-overlapping '() (() 'empty)))
      (⊦= 'empty (match/non-overlapping #() (() 'empty)))
      (⊦= '() (match/non-overlapping '() (,r r)))
      (⊦= #() (match/non-overlapping #() (,r r)))
      (⊦= 'p (match/non-overlapping '(p) ((,r) r)))
      (⊦= #t (match/non-overlapping #(p) ((p) #t)))
      (⊦= 'p (match/non-overlapping #(p) ((,r) r)))
      (⊦= 3 (match/non-overlapping #(3 2) ((,r 2) r)))
      (⊦= '(3 #(2)) (match/non-overlapping #(3 2) ((,r . ,s) (list r s))))
      (⊦= '(3 2 #()) (match/non-overlapping #(3 2) ((,r ,s . ,t) (list r s t))))
      (⊦⧳ ((exn)) (match/non-overlapping #(3 2) ((,r 2 ,t) r)))
      (⊦= 3 (match/non-overlapping #(3 2) ((,r ,e) r)))
      (⊦= 3 (match/non-overlapping (make-record-instance 'hello 3 2) ((hello ,r ,e) r)))
  )

  ((test/h-non-overlapping _)

    (define (h x y)
      (match/non-overlapping (cons x y)
        ((,a . ,b)  (and (number? a) (number? b)) ⇒ (* a b))
        ((,a ,b ,c) (and (number? a) (number? b) (number? c)) ⇒ (+ a b c))))

    (⊦= '(12 8) (list (h 3 4) (apply h '(1 (3 4)))))

  )

  ((test/h-overlapping _)

    (define (w x y)
      (match/non-overlapping (cons x y)
        ((,a . ,b) (and (number? a) (number? b)) ⇒ (* a b))
        ((,a . ,b) (+ a b))
        ((,a ,b ,c) (and (number? a) (number? b) (number? c)) ⇒ (+ a b c))))

    (⊦⧳ ((exn)) (list (w 3 4) (apply w '(1 (3 4)))))
    
  )

  ((test/meta-circular-interpreter _)

    (define (lookup x env)
      (cond
        ((assq x env) => cdr)
        (else (error "Can't find " x))))

    (define (int code env)
      (match/non-overlapping code
        ((quote ,x) x)
        ((let (,x ,e) ,body) (symbol? x) ⇒ (let ((xv (int e env))) (int body (cons (cons x xv) env))))
        ((τ ,body) (τ (int body env)))
        ((λ ,argl ,body) (symbol? argl) ⇒ (λ arglv (int body (cons (cons argl arglv) env))))
        ((λ (,x) ,body) (symbol? x) ⇒ (λ (xv) (int body (cons (cons x xv) env))))
        ((,op . ,args) (not (or (eq? op 'quote) (eq? op 'let) (eq? op 'τ) (eq? op 'λ)))
                          ⇒ (let ((opv (int op env))
                                  (argvs (map (λ (c) (int c env)) args)))
                              (apply opv argvs)))
        (,x (symbol? x) ⇒ (lookup x env))
        (,x (or (number? x) (string? x)) ⇒ x)))

    (define env0 (interaction-environment/symbols '(+ - display identity)))

    (define-syntax-rule (dint body) (int (quote body) env0))

    (⊦ equal? 1 (dint 1))
    (⊦ equal? 'x (dint 'x))
    (⊦ void? (dint (display 'x)))
    (⊦ equal? 'x (dint (identity 'x)))
    (⊦⧳ ((exn)) (dint (display x))) ; error: unbound x
    (⊦ equal? 6 (dint (let (x (+ 1 2 3)) x)))
    (⊦ equal? 1 (let1 (t (dint (τ 1))) (t)))
    (⊦ equal? 1 ((dint (λ (x) x)) 1))
    (⊦ equal? '(1 2 3) ((dint (λ x x)) 1 2 3))
    (⊦ equal? 5 (((dint (λ (x) (λ (y) (+ x y)))) 2) 3))
    
  )

)

(unittest/✓ dmatch-suite)