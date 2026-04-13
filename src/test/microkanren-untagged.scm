
(import scheme (chicken base) (chicken memory representation) (aux base) (aux unittest) (aux kanren micro))

(define-suite untagged-suite

  ((test-λ-calculus-interpreter _)

    (define (lookup x env)
      (match/non-overlapping env
        (() (error 'lookup "unbound variable" x))
        ((((,y . ,v) . ,rest) ⊣ (equal? x y)) v)
        ((((,y . ,v) . ,rest) ⊣ (not (equal? x y))) (lookup x rest))))

    (define (not-in-env? x env)
      (match/non-overlapping env
        (() #t)
        ((((,y . ,v) . ,rest) ⊣ (equal? x y)) #f)
        ((((,y . ,v) . ,rest) ⊣ (not (equal? x y))) (not-in-env? x rest))))

    (define rator?
      (let1 (op-names '(lambda quote list))
        (λ (x env)
          (not (and (symbol? x) (memq x op-names) (not-in-env? x env))))))

    (define (eval-exp exp env)
      (match/non-overlapping exp
        (((,rator ,rand) ⊣ (rator? rator env))  (let ((proc (eval-exp rator env))
                                                      (arg (eval-exp rand env)))
                                                  (match/non-overlapping proc
                                                    ((closure ,x ,body ,env2) (eval-exp body `((,x . ,arg) . ,env2))))))
        (((λ (,x) ,body) ⊣ (and (symbol? x) (not-in-env? 'λ env))) `(closure ,x ,body ,env))
        (((quote ,v) ⊣ (not-in-env? 'quote env)) v)
        (((list . ,a*) ⊣ (not-in-env? 'list env)) (map (λ (e) (eval-exp e env)) a*))
        ((,x ⊣ (symbol? x)) (lookup x env))))

    (define-syntax-rule (eval/env0 body) (eval-exp (quote body) (interaction-environment/symbols '())))

    (⊦ equal? `(closure z z ()) (eval/env0 (((λ (x) (λ (y) x)) (λ (z) z)) (λ (a) a))))
    (⊦ equal? `(closure y x ((x . (closure z z ())))) (eval/env0 ((λ (x) (λ (y) x)) (λ (z) z))))
    (⊦ equal? `(closure y y ()) (eval/env0 ((λ (x) x) (λ (y) y))))
    (⊦ equal? `(closure y y ()) (eval/env0 ((quote (closure x x ())) (λ (y) y))))

  )

  ((test/eval-exp° _)

    (define (proper-list° exp env val)
      (cond°
        ((=° '() exp) (=° '() val))
        ((fresh° (a d v-a v-d)
          (=° `(,a . ,d) exp)
          (=° `(,v-a . ,v-d) val)
          (eval-exp° a env v-a)
          (proper-list° d env v-d)))))

    (define (not-in-env° x env)
      (cond°
        ((=° '() env))
        ((fresh° (y v rest)
          (=° `((,y . ,v) . ,rest) env)
          (≠° y x)
          (not-in-env° x rest)))))

    (define (lookup° x env t)
      (cond°
        ((=° '() env) ✗°)
        ((fresh° (y v rest)
          (=° `((,y . ,v) . ,rest) env) 
          (=° y x)
          (=° v t)))
        ((fresh° (y v rest)
          (=° `((,y . ,v) . ,rest) env)
          (≠° y x)
          (lookup° x rest t)))))

    (define (eval-exp° exp env val)
      (cond°
        ((fresh° (v) 
          (=° `(quote ,v) exp) 
          (not-in-env° 'quote env) 
          (absent° 'closure v) 
          (=° v val)))
        ((fresh° (a*)
          (=° `(list . ,a*) exp)
          (not-in-env° 'list env)
          (absent° 'closure a*)
          (proper-list° a* env val)))
        ((symbol° exp) (lookup° exp env val))
        ((fresh° (rator rand x body envˆ a)
            (=° `(,rator ,rand) exp)
            (eval-exp° rator env `(closure ,x ,body ,envˆ))
            (eval-exp° rand env a)
            (eval-exp° body `((,x . ,a) . ,envˆ) val)))
        ((fresh° (x body)
          (=° `(λ (,x) ,body) exp)
          (symbol° x)
          (not-in-env° 'λ env)
          (=° `(closure ,x ,body ,env) val)))))
  
    (⊦= '((λ (α) (deny (equal? α 1)) α)) 
      (°->list #f (take° 5 (fresh° (q e v) (eval-exp° e '() v) (=° `(,e → ,v) q)))))
  
  )

)

(unittest/✓ untagged-suite)