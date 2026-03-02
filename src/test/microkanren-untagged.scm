
(import scheme (chicken base) (chicken memory representation) (aux base) (aux unittest) (aux match))

(define-suite untagged-suite

  #;((doc r) `((structure/section "Implementation")
             (code/scheme/file "../aux.match.scm")))

  ((test-λ-calculus-interpreter _)

    (define (lookup x env)
      (match/non-overlapping env
        (() (error 'lookup "unbound variable" x))
        (((,,x . ,v) . ,rest) v)
        (((,,,x . ,v) . ,rest) (lookup x rest))))

    (define (not-in-env? x env)
      (match/non-overlapping env
        (() #t)
        (((,,x . ,v) . ,rest) #f)
        (((,,,x . ,v) . ,rest) (not-in-env? x rest))))

    (define rator?
      (let1 (op-names '(lambda quote list))
        (λ (x env)
          (not (and (symbol? x) (memq x op-names) (not-in-env? x env))))))

    (define (eval-exp exp env)
      (match/non-overlapping exp
        ((,rator ,rand) (rator? rator env) ⇒ (let ((proc (eval-exp rator env))
                                                   (arg (eval-exp rand env)))
                                               (match/non-overlapping proc
                                                ((closure ,x ,body ,env2) (eval-exp body `((,x . ,arg) . ,env2))))))
        ((λ (,x) ,body) (and (symbol? x) (not-in-env? 'λ env)) ⇒ `(closure ,x ,body ,env))
        ((quote ,v) (not-in-env? 'quote env) ⇒ v)
        ((list . ,a∗) (not-in-env? 'list env) ⇒ (map (λ (e) (eval-exp e env)) a∗))
        (,x (symbol? x) ⇒ (lookup x env))))

    (define-syntax-rule (eval/env0 body) (eval-exp (quote body) (interaction-environment/symbols '())))

    (⊦ equal? `(closure z z ()) (eval/env0 (((λ (x) (λ (y) x)) (λ (z) z)) (λ (a) a))))
    (⊦ equal? `(closure y x ((x . (closure z z ())))) (eval/env0 ((λ (x) (λ (y) x)) (λ (z) z))))
    (⊦ equal? `(closure y y ()) (eval/env0 ((λ (x) x) (λ (y) y))))
    (⊦ equal? `(closure y y ()) (eval/env0 ((quote (closure x x ())) (λ (y) y))))

  )

)

(unittest/✓ untagged-suite)