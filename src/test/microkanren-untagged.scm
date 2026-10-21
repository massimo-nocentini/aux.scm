
(import scheme (chicken base) (chicken memory representation) srfi-1
  (aux base) (aux unittest) (aux kanren micro) (only (aux kanren arith) build-num pos° minus° *°))

; Byrd, Holk and Friedman, "miniKanren, Live and Untagged: Quine Generation via Relational Interpreters", 2012.

; the Scheme interpreter of section 4.
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

; the relational interpreter of section 4.
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
  (fresh° (y v rest)
    (=° `((,y . ,v) . ,rest) env)
    (cond°
      ((=° y x) (=° v t))
      ((≠° y x) (lookup° x rest t)))))

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

; the relational interpreter of section 3, for the call-by-value λ-calculus only, with the paper's clause order.
(define (eval-λ° exp env val)
  (cond°
    ((fresh° (rator rand x body envˆ a)
        (=° `(,rator ,rand) exp)
        (eval-λ° rator env `(closure ,x ,body ,envˆ))
        (eval-λ° rand env a)
        (eval-λ° body `((,x . ,a) . ,envˆ) val)))
    ((fresh° (x body)
      (=° `(λ (,x) ,body) exp)
      (symbol° x)
      (=° `(closure ,x ,body ,env) val)
      (not-in-env° 'λ env)))
    ((symbol° exp) (lookup° exp env val))))

; the relational interpreter of appendix A: an uncurried Scheme with numbers (the little-endian bit
; lists of appendix B, in (aux kanren arith)), booleans, pairs, sub1, zero?, *, cons, car, cdr, not and if.
(define eval-exp/extended°
  (let ()

    (define (proper-list° exp env val)
      (cond°
        ((=° '() exp) (=° '() val))
        ((fresh° (a d v-a v-d)
          (=° `(,a . ,d) exp)
          (=° `(,v-a . ,v-d) val)
          (eval-exp° a env v-a)
          (proper-list° d env v-d)))))

    (define (eval-exp° exp env val)
      (cond°
        ((fresh° (v)
          (=° `(quote ,v) exp)
          (not-in-env° 'quote env)
          (absent° 'closure v)
          (absent° 'int-val v)
          (=° v val)))
        ((fresh° (a*)
          (=° `(list . ,a*) exp)
          (not-in-env° 'list env)
          (absent° 'closure a*)
          (absent° 'int-val a*)
          (proper-list° a* env val)))
        ((prim-exp° exp env val))
        ((symbol° exp) (lookup° exp env val))
        ((fresh° (rator x* rands body envˆ a* res)
          (=° `(,rator . ,rands) exp)
          (eval-exp° rator env `(closure ,x* ,body ,envˆ))
          (proper-list° rands env a*)
          (ext-env*° x* a* envˆ res)
          (eval-exp° body res val)))
        ((fresh° (x* body)
          (=° `(λ ,x* ,body) exp)
          (not-in-env° 'λ env)
          (=° `(closure ,x* ,body ,env) val)))))

    (define (ext-env*° x* a* env out)
      (cond°
        ((=° '() x*) (=° '() a*) (=° env out))
        ((fresh° (x a dx* da* env2)
          (=° `(,x . ,dx*) x*)
          (=° `(,a . ,da*) a*)
          (=° `((,x . ,a) . ,env) env2)
          (ext-env*° dx* da* env2 out)))))

    (define (prim-exp° exp env val)
      (cond°
        ((boolean-prim° exp env val))
        ((number-prim° exp env val))
        ((sub1-prim° exp env val))
        ((zero?-prim° exp env val))
        ((*-prim° exp env val))
        ((cons-prim° exp env val))
        ((car-prim° exp env val))
        ((cdr-prim° exp env val))
        ((not-prim° exp env val))
        ((if-prim° exp env val))))

    (define (boolean-prim° exp env val)
      (cond°
        ((=° #t exp) (=° #t val))
        ((=° #f exp) (=° #f val))))

    (define (number-prim° exp env val)
      (fresh° (n)
        (=° `(int-exp ,n) exp)
        (=° `(int-val ,n) val)
        (not-in-env° 'int-exp env)))

    (define (sub1-prim° exp env val)
      (fresh° (e n n-1)
        (=° `(sub1 ,e) exp)
        (=° `(int-val ,n-1) val)
        (not-in-env° 'sub1 env)
        (eval-exp° e env `(int-val ,n))
        (minus° n '(1) n-1)))

    (define (zero?-prim° exp env val)
      (fresh° (e n)
        (=° `(zero? ,e) exp)
        (cond°
          ((=° '() n) (=° #t val))
          ((pos° n) (=° #f val)))
        (not-in-env° 'zero? env)
        (eval-exp° e env `(int-val ,n))))

    (define (*-prim° exp env val)
      (fresh° (e1 e2 n1 n2 n3)
        (=° `(* ,e1 ,e2) exp)
        (=° `(int-val ,n3) val)
        (not-in-env° '* env)
        (eval-exp° e1 env `(int-val ,n1))
        (eval-exp° e2 env `(int-val ,n2))
        (*° n1 n2 n3)))

    (define (cons-prim° exp env val)
      (fresh° (a d v-a v-d)
        (=° `(cons ,a ,d) exp)
        (=° `(,v-a . ,v-d) val)
        (absent° 'closure val)
        (absent° 'int-val val)
        (not-in-env° 'cons env)
        (eval-exp° a env v-a)
        (eval-exp° d env v-d)))

    (define (car-prim° exp env val)
      (fresh° (p d)
        (=° `(car ,p) exp)
        (≠° 'int-val val)
        (≠° 'closure val)
        (not-in-env° 'car env)
        (eval-exp° p env `(,val . ,d))))

    (define (cdr-prim° exp env val)
      (fresh° (p a)
        (=° `(cdr ,p) exp)
        (≠° 'int-val a)
        (≠° 'closure a)
        (not-in-env° 'cdr env)
        (eval-exp° p env `(,a . ,val))))

    (define (not-prim° exp env val)
      (fresh° (e b)
        (=° `(not ,e) exp)
        (cond°
          ((=° #t b) (=° #f val))
          ((=° #f b) (=° #t val)))
        (not-in-env° 'not env)
        (eval-exp° e env b)))

    (define (if-prim° exp env val)
      (fresh° (e1 e2 e3 t)
        (=° `(if ,e1 ,e2 ,e3) exp)
        (not-in-env° 'if env)
        (eval-exp° e1 env t)
        (cond°
          ((=° #t t) (eval-exp° e2 env val))
          ((=° #f t) (eval-exp° e3 env val)))))

    eval-exp°))

; section 5: a quine evaluates to itself, a twine is a pair of distinct programs evaluating to each other.
(define (quine° q) (eval-exp° q '() q))

(define (twine° x)
  (fresh° (p q)
    (≠° p q)
    (eval-exp° p '() q)
    (eval-exp° q '() p)
    (=° `(,p ,q) x)))

; the quine of section 1, with λ for lambda.
(define quinec '((λ (x) (list x (list (quote quote) x))) (quote (λ (x) (list x (list (quote quote) x))))))

; the constraint clauses of a reified answer (λ (α ...) clause ... term), without its formals and its term.
(define (reified-constraints A) (drop-right (cddr A) 1))

(define-suite microkanren-untagged-suite

  ((test-λ-calculus-interpreter _)

    (define-syntax-rule (eval/env0 body) (eval-exp (quote body) (interaction-environment/symbols '())))

    (⊦ equal? `(closure z z ()) (eval/env0 (((λ (x) (λ (y) x)) (λ (z) z)) (λ (a) a))))
    (⊦ equal? `(closure y x ((x . (closure z z ())))) (eval/env0 ((λ (x) (λ (y) x)) (λ (z) z))))
    (⊦ equal? `(closure y y ()) (eval/env0 ((λ (x) x) (λ (y) y))))
    (⊦ equal? `(closure y y ()) (eval/env0 ((quote (closure x x ())) (λ (y) y))))

  )

  ((test/eval-exp° _)

    (⊦= '(bar) (°->list #t (fresh° (q) (lookup° 'y (list (cons 'x  'foo) (cons 'y  'bar)) q))))
    (⊦= '() (°->list #t (fresh° (q) (lookup° 'w (list (cons 'x 'foo) (cons 'y 'bar)) q))))

    (let* ((Q (car (°->list #f (take° 1 (fresh° (q) (eval-exp° q '() q))))))
           (λQ (eval Q))
           (quine (λQ 'α)))
      (⊦= 
       '(λ (α)
            (begin (deny (equal? α 'closure)))
            (begin (deny (equal? α 'list)))
            (begin (deny (equal? α 'quote)))
            (assert (every (μ v (symbol? v)) (list α)))
            (cons (cons 'λ
                        (cons (cons α '())
                              (cons (cons 'list
                                          (cons α
                                                (cons (cons 'list
                                                            (cons (cons 'quote (cons 'quote '()))
                                                                  (cons α '())))
                                                      '())))
                                    '())))
                  (cons (cons 'quote
                              (cons (cons 'λ
                                          (cons (cons α '())
                                                (cons (cons 'list
                                                            (cons α
                                                                  (cons (cons 'list
                                                                              (cons (cons 'quote (cons 'quote '()))
                                                                                    (cons α '())))
                                                                        '())))
                                                      '())))
                                    '()))
                        '())))
                Q)
      (⊦= '((λ (α) (list α (list (quote quote) α))) (quote (λ (α) (list α (list (quote quote) α))))) quine)
      (⊦= quine (eval quine)))

    (⊦= 
     '((λ (α)
          (assert (absent? 'closure α))
          (cons (cons 'quote (cons α '())) (cons '→ (cons α '()))))
      (λ () (cons (cons 'list '()) (cons '→ (cons '() '()))))
      (λ (α β)
          (assert (every (μ v (symbol? v)) (list α)))
          (cons (cons 'λ (cons (cons α '()) (cons β '())))
                (cons '→
                      (cons (cons 'closure (cons α (cons β (cons '() '()))))
                            '()))))
      (λ (α)
          (assert (absent? 'closure α))
          (cons (cons 'list (cons (cons 'quote (cons α '())) '()))
                (cons '→ (cons (cons α '()) '()))))
      (λ ()
          (cons (cons 'list (cons (cons 'list '()) '()))
                (cons '→ (cons (cons '() '()) '()))))
      (λ (α β)
          (assert (absent? 'closure α))
          (assert (absent? 'closure β))
          (cons (cons 'list
                      (cons (cons 'quote (cons α '()))
                            (cons (cons 'quote (cons β '())) '())))
                (cons '→ (cons (cons α (cons β '())) '())))))
      (°->list #f (take° 6 (fresh° (q e v) (eval-exp° e '() v) (=° `(,e → ,v) q)))))
  
  )

  ; sections 3 and 4: shadowing of λ, quote and list, and the closure tag.

  ((test/eval-exp/shadowing _)
    (⊦= '(closure x x ()) (eval-exp '((λ (λ) (λ λ)) (λ (x) x)) '()))
    (⊦= '(closure y y ()) (eval-exp '((λ (quote) (quote quote)) (λ (y) y)) '()))
    (⊦= 'a (eval-exp '((λ (list) (list (quote a))) (λ (x) x)) '()))
    (⊦= '(a) (eval-exp '((λ (y) (list (quote a))) (λ (x) x)) '()))
  )

  ((test/eval-exp°/agrees-with-eval-exp _)
    (let1 (programs '(((λ (λ) (λ λ)) (λ (x) x))
                      ((λ (quote) (quote quote)) (λ (y) y))
                      ((λ (quote) (quote (λ (x) x))) (λ (y) y))
                      ((λ (list) (list (quote a))) (λ (x) x))
                      ((λ (y) (list (quote a))) (λ (x) x))
                      ((λ (list) (list list)) (λ (z) z))
                      (((λ (x) (λ (y) x)) (λ (z) z)) (λ (a) a))))
      (⊦= (map (λ (p) (list (eval-exp p '()))) programs)
          (map (λ (p) (°->list #t (fresh° (q) (eval-exp° p '() q)))) programs)))
  )

  ((test/eval-λ°/forward-pairs _)
    (⊦= '((λ (α β)
              (assert (every (μ v (symbol? v)) (list α)))
              (cons (cons 'λ (cons (cons α '()) (cons β '())))
                    (cons '→ (cons (cons 'closure (cons α (cons β (cons '() '())))) '()))))
          (λ (α β γ)
              (assert (every (μ v (symbol? v)) (list α β)))
              (cons (cons (cons 'λ (cons (cons α '()) (cons α '())))
                          (cons (cons 'λ (cons (cons β '()) (cons γ '()))) '()))
                    (cons '→ (cons (cons 'closure (cons β (cons γ (cons '() '())))) '()))))
          (λ (α β γ δ ε)
              (begin (deny (equal? α 'λ)))
              (assert (every (μ v (symbol? v)) (list α β δ)))
              (cons (cons (cons 'λ (cons (cons α '()) (cons (cons 'λ (cons (cons β '()) (cons γ '()))) '())))
                          (cons (cons 'λ (cons (cons δ '()) (cons ε '()))) '()))
                    (cons '→
                          (cons (cons 'closure
                                      (cons β
                                            (cons γ
                                                  (cons (cons (cons α (cons 'closure (cons δ (cons ε (cons '() '())))))
                                                              '())
                                                        '()))))
                                '()))))
          (λ (α β)
              (assert (every (μ v (symbol? v)) (list α β)))
              (cons (cons (cons 'λ (cons (cons α '()) (cons (cons α (cons α '())) '())))
                          (cons (cons 'λ (cons (cons β '()) (cons β '()))) '()))
                    (cons '→ (cons (cons 'closure (cons β (cons β (cons '() '())))) '()))))
          (λ (α β γ δ)
              (begin (deny (equal? β 'λ)))
              (assert (every (μ v (symbol? v)) (list α β γ)))
              (cons (cons (cons 'λ (cons (cons α '()) (cons (cons α (cons α '())) '())))
                          (cons (cons 'λ (cons (cons β '()) (cons (cons 'λ (cons (cons γ '()) (cons δ '()))) '())))
                                '()))
                    (cons '→
                          (cons (cons 'closure
                                      (cons γ
                                            (cons δ
                                                  (cons (cons (cons β
                                                                    (cons 'closure
                                                                          (cons β
                                                                                (cons (cons 'λ (cons (cons γ '()) (cons δ '())))
                                                                                      (cons '() '())))))
                                                              '())
                                                        '()))))
                                '())))))
        (°->list #f (take° 5 (fresh° (q e v) (eval-λ° e '() v) (=° `(,e → ,v) q)))))
  )

  ((test/eval-λ°/backwards _)
    (⊦= '(((λ (x) (λ (y) x)) (λ (z) z))
          ((λ (x) (x (λ (y) x))) (λ (z) z))
          ((λ (x) (λ (y) x)) ((λ (α) α) (λ (z) z)))
          (((λ (α) α) (λ (x) (λ (y) x))) (λ (z) z))
          ((λ (α) α) ((λ (x) (λ (y) x)) (λ (z) z))))
        (°->list #t (take° 5 (fresh° (q) (eval-λ° q '() '(closure y x ((x . (closure z z ())))))))))
    (⊦= '(() () ((assert (every (μ v (symbol? v)) (list α)))) ((assert (every (μ v (symbol? v)) (list α)))) ((assert (every (μ v (symbol? v)) (list α)))))
        (map reified-constraints (°->list #f (take° 5 (fresh° (q) (eval-λ° q '() '(closure y x ((x . (closure z z ()))))))))))
    ; with quote and list the same query keeps its first two answers (the third takes minutes).
    (⊦= '(((λ (x) (λ (y) x)) (λ (z) z))
          ((λ (x) (x (λ (y) x))) (λ (z) z)))
        (°->list #t (take° 2 (fresh° (q) (eval-exp° q '() '(closure y x ((x . (closure z z ())))))))))
  )

  ((test/eval-exp°/shadowed-λ _)
    (⊦= '((closure λ (λ λ) ())) (°->list #t (fresh° (q) (eval-exp° '(λ (λ) (λ λ)) '() q))))
    (⊦= '((closure x x ())) (°->list #t (fresh° (q) (eval-exp° '((λ (λ) (λ λ)) (λ (x) x)) '() q))))
    (⊦= '() (°->list #t (fresh° (q) (eval-exp° '((λ (λ) (λ (x) x)) (λ (y) y)) '() q))))
  )

  ((test/eval-exp°/shadowed-quote _)
    (⊦= '((closure y y ())) (°->list #t (fresh° (q) (eval-exp° '((λ (quote) (quote quote)) (λ (y) y)) '() q))))
    (⊦= '((closure x x ((quote . (closure y y ())))))
        (°->list #t (fresh° (q) (eval-exp° '((λ (quote) (quote (λ (x) x))) (λ (y) y)) '() q))))
    (⊦= '((closure x x ((quote . (closure y y ())))))
        (°->list #t (fresh° (q) (eval-exp° '(quote (λ (x) x)) '((quote . (closure y y ()))) q))))
    (⊦= '() (°->list #t (fresh° (q) (eval-exp° '(quote (λ (x) x)) '((quote . foo)) q))))
    (⊦= '((λ (x) x)) (°->list #t (fresh° (q) (eval-exp° '(quote (λ (x) x)) '() q))))
    (⊦= '(quote) (°->list #t (fresh° (q) (eval-exp° '(quote quote) '() q))))
  )

  ((test/eval-exp°/shadowed-list _)
    (⊦= '(a) (°->list #t (fresh° (q) (eval-exp° '((λ (list) (list (quote a))) (λ (x) x)) '() q))))
    (⊦= '((a)) (°->list #t (fresh° (q) (eval-exp° '((λ (y) (list (quote a))) (λ (x) x)) '() q))))
    (⊦= '((closure z z ())) (°->list #t (fresh° (q) (eval-exp° '((λ (list) (list list)) (λ (z) z)) '() q))))
  )

  ((test/eval-exp°/unguarded-overlap _)

    ; eval-exp° without its three not-in-env° guards: application overlaps the quote and list clauses, and a shadowed λ still builds a closure.
    (define (proper-list/unguarded° exp env val)
      (cond°
        ((=° '() exp) (=° '() val))
        ((fresh° (a d v-a v-d)
          (=° `(,a . ,d) exp)
          (=° `(,v-a . ,v-d) val)
          (eval-exp/unguarded° a env v-a)
          (proper-list/unguarded° d env v-d)))))

    (define (eval-exp/unguarded° exp env val)
      (cond°
        ((fresh° (v)
          (=° `(quote ,v) exp)
          (absent° 'closure v)
          (=° v val)))
        ((fresh° (a*)
          (=° `(list . ,a*) exp)
          (absent° 'closure a*)
          (proper-list/unguarded° a* env val)))
        ((symbol° exp) (lookup° exp env val))
        ((fresh° (rator rand x body envˆ a)
            (=° `(,rator ,rand) exp)
            (eval-exp/unguarded° rator env `(closure ,x ,body ,envˆ))
            (eval-exp/unguarded° rand env a)
            (eval-exp/unguarded° body `((,x . ,a) . ,envˆ) val)))
        ((fresh° (x body)
          (=° `(λ (,x) ,body) exp)
          (symbol° x)
          (=° `(closure ,x ,body ,env) val)))))

    (⊦= '((a) a) (°->list #t (fresh° (q) (eval-exp/unguarded° '((λ (list) (list (quote a))) (λ (x) x)) '() q))))
    (⊦= (list 'quote '(closure y y ()))
        (°->list #t (fresh° (q) (eval-exp/unguarded° '((λ (quote) (quote quote)) (λ (y) y)) '() q))))
    (⊦= '((closure x x ((λ . (closure y y ())))))
        (°->list #t (fresh° (q) (eval-exp/unguarded° '((λ (λ) (λ (x) x)) (λ (y) y)) '() q))))
  )

  ((test/absent°/closure-tag _)
    (⊦= '() (°->list #t (fresh° (q) (eval-exp° '((quote (closure x x ())) (λ (y) y)) '() q))))
    (⊦= '() (°->list #t (fresh° (q) (eval-exp° '(quote (closure x x ())) '() q))))
    (⊦= '() (°->list #f (fresh° (q) (eval-exp° `(quote ,q) '() '(closure x x ())))))
    (⊦= '((λ () (cons 'λ (cons (cons 'x '()) (cons 'x '()))))
          (λ (α)
              (begin (deny (equal? α 'closure)))
              (assert (every (μ v (symbol? v)) (list α)))
              (cons (cons 'λ (cons (cons α '()) (cons α '())))
                    (cons (cons 'λ (cons (cons 'x '()) (cons 'x '()))) '())))
          (λ (α)
              (begin (deny (equal? α 'closure)))
              (assert (every (μ v (symbol? v)) (list α)))
              (cons (cons 'λ (cons (cons α '()) (cons (cons α (cons α '())) '())))
                    (cons (cons 'λ (cons (cons 'x '()) (cons 'x '()))) '()))))
        (°->list #f (take° 3 (fresh° (q) (eval-exp° `(list ,q) '() '((closure x x ())))))))
  )

  ((test/eval-exp°/backwards-shadowed-quote _)
    (let1 (answers (°->list #f (take° 6 (fresh° (q) (eval-exp° `((λ (quote) ,q) (λ (x) x)) '() '(closure x x ()))))))
      (⊦= '(quote
            (quote quote)
            ((λ (α) α) quote)
            ((λ (α) quote) quote)
            ((λ (α) quote) (list))
            ((λ (α) (α α)) quote))
          (map (λ (A) (apply (eval A) (cadr A))) answers))
      (⊦= '(() () ((assert (every (μ v (symbol? v)) (list α))))
            ((begin (deny (equal? α 'quote))) (assert (every (μ v (symbol? v)) (list α))))
            ((begin (deny (equal? α 'quote))) (assert (every (μ v (symbol? v)) (list α))))
            ((assert (every (μ v (symbol? v)) (list α)))))
          (map reified-constraints answers)))
  )

  ((test/eval-exp°/backwards-shadowed-list _)
    (let1 (programs (°->list #t (take° 10 (fresh° (p q) (=° `((λ (list) ,q) (λ (x) x)) p) (eval-exp° p '() '(a))))))
      (⊦= '(((λ (list) '(a)) (λ (x) x))
            ((λ (list) (list '(a))) (λ (x) x))
            ((λ (list) ((λ (α) '(a)) 'β)) (λ (x) x))
            ((λ (list) ((λ (α) α) '(a))) (λ (x) x))
            ((λ (list) ((λ (α) '(a)) list)) (λ (x) x))
            ((λ (list) ((λ (α) (list '(a))) 'β)) (λ (x) x))
            ((λ (list) ((λ (α) (list α)) '(a))) (λ (x) x))
            ((λ (list) ((λ (α) '(a)) (λ (β) γ))) (λ (x) x))
            ((λ (list) ((λ (α) ((λ (β) '(a)) 'γ)) 'δ)) (λ (x) x))
            ((λ (list) ((λ (α) (α '(a))) list)) (λ (x) x)))
          programs)
      (⊦= (make-list 10 '(a)) (map eval programs)))
  )

  ; section 6: untagged answers run unchanged in Scheme.
  ((test/eval-exp°/untagged-answers-run-in-scheme _)
    (let1 (programs (°->list #t (take° 30 (fresh° (q) (eval-exp° q '() '(I love you))))))
      (⊦= '('(I love you)
            ((λ (α) '(I love you)) 'β)
            ((λ (α) α) '(I love you))
            (list 'I 'love 'you)
            ((λ (α) '(I love you)) (list))
            ((λ (α) '(I love you)) (λ (β) γ)))
          (take programs 6))
      (⊦= (make-list 30 '(I love you)) (map eval programs))
      (⊦= (make-list 30 '(I love you)) (map (λ (p) (eval-exp p '())) programs)))
  )

  ; sections 1 and 5: quines.

  ((test/quinec/scheme _)
    (⊦= quinec (eval quinec))
  )

  ((test/quinec/forward _)
    (⊦= (list quinec) (μkanren-run (v -1 #t) (eval-exp° quinec '() v)))
    (⊦= (list quinec) (μkanren-run (q -1 #t) (=° q quinec) (quine° q)))
  )

  ((test/quine°/α-equivalent-to-quinec _)
    ; footnote 1: the run 1 quine is α-equivalent to quinec.
    (let* ((Q (car (°->list #f (take° 1 (fresh° (q) (quine° q))))))
           (λQ (eval Q)))
      (⊦= quinec (λQ 'x))
      (⊦= (λQ 'y) (eval (λQ 'y)))
      (⊦= (list (λQ 'y)) (μkanren-run (v -1 #t) (eval-exp° (λQ 'y) '() v)))
      (⊦raises (exn) (λQ 'closure))
      (⊦raises (exn) (λQ 'list))
      (⊦raises (exn) (λQ 'quote)))
  )

  ((test/quine°/disequalities-are-needed _)
    (let* ((Q (car (°->list #f (take° 1 (fresh° (q) (quine° q))))))
           (λQ! (eval `(λ (α) ,(last Q)))))   ; the body of Q alone, without its constraint clauses
      (⊦= '() (μkanren-run (v -1 #t) (eval-exp° (λQ! 'closure) '() v)))
      (⊦= '() (μkanren-run (v -1 #t) (eval-exp° (λQ! 'list) '() v)))
      (⊦= '() (μkanren-run (v -1 #t) (eval-exp° (λQ! 'quote) '() v)))
      ; shadowing λ is harmless, α ≠ λ is not required.
      (⊦= '((λ (λ) (list λ (list (quote quote) λ))) (quote (λ (λ) (list λ (list (quote quote) λ))))) (λQ! 'λ))
      (⊦= (list (λQ! 'λ)) (μkanren-run (v -1 #t) (eval-exp° (λQ! 'λ) '() v)))
      (⊦= (λQ! 'λ) (eval (λQ! 'λ))))
  )

  ((test/quine°/no-literal-quines _)
    (⊦= 5 (eval 5))
    (⊦= #t (eval #t))
    (⊦= '() (μkanren-run (v -1 #t) (eval-exp° 5 '() v)))
    (⊦= '() (μkanren-run (v -1 #t) (eval-exp° #t '() v)))
    (⊦= '() (μkanren-run (q -1 #t) (symbol° q) (quine° q)))
    (⊦= '() (μkanren-run (q -1 #t) (fresh° (v) (=° `(quote ,v) q) (quine° q))))
    (⊦= '() (μkanren-run (q -1 #t) (fresh° (x b) (=° `(λ (,x) ,b) q) (quine° q))))
  )

  ((test/quine°/second _)
    (⊦= '(λ (α β γ)
            (begin (deny (equal? α 'closure)))
            (begin (deny (equal? α 'list)))
            (begin (deny (equal? α 'quote)))
            (begin (deny (equal? α 'λ)))
            (begin (deny (equal? β 'closure)))
            (begin (deny (equal? β α)))
            (assert (every (μ v (symbol? v)) (list α β)))
            (assert (absent? 'closure γ))
            (cons (cons 'λ
                        (cons (cons α '())
                              (cons (cons 'list
                                          (cons (cons (cons 'λ (cons (cons β '()) (cons α '())))
                                                      (cons (cons 'quote (cons γ '())) '()))
                                                (cons (cons 'list
                                                            (cons (cons 'quote (cons 'quote '()))
                                                                  (cons α '())))
                                                      '())))
                                    '())))
                  (cons (cons 'quote
                              (cons (cons 'λ
                                          (cons (cons α '())
                                                (cons (cons 'list
                                                            (cons (cons (cons 'λ (cons (cons β '()) (cons α '())))
                                                                        (cons (cons 'quote (cons γ '())) '()))
                                                                  (cons (cons 'list
                                                                              (cons (cons 'quote (cons 'quote '()))
                                                                                    (cons α '())))
                                                                        '())))
                                                      '())))
                                    '()))
                        '())))
        (cadr (°->list #f (take° 2 (fresh° (q) (quine° q))))))
  )

  ((test/quine°/ten _)
    (let1 (qs (μkanren-run (q 10 #t) (quine° q)))
      (⊦= '(((λ (α) (list α (list (quote quote) α))) (quote (λ (α) (list α (list (quote quote) α)))))
            ((λ (α) (list ((λ (β) α) (quote γ)) (list (quote quote) α))) (quote (λ (α) (list ((λ (β) α) (quote γ)) (list (quote quote) α)))))
            ((λ (α) (list α (list ((λ (β) (quote quote)) (quote γ)) α))) (quote (λ (α) (list α (list ((λ (β) (quote quote)) (quote γ)) α)))))
            ((λ (α) (list (list (quote λ) (quote (α)) α) (list (quote quote) α))) (quote (list (list (quote λ) (quote (α)) α) (list (quote quote) α))))
            ((λ (α) ((λ (β) (list α (list (quote quote) α))) (quote γ))) (quote (λ (α) ((λ (β) (list α (list (quote quote) α))) (quote γ)))))
            ((λ (α) (list α (list ((λ (β) β) (quote quote)) α))) (quote (λ (α) (list α (list ((λ (β) β) (quote quote)) α)))))
            ((λ (α) (list α ((λ (β) (list (quote quote) α)) (quote γ)))) (quote (λ (α) (list α ((λ (β) (list (quote quote) α)) (quote γ))))))
            ((λ (α) (list α (list (quote quote) ((λ (β) α) (quote γ))))) (quote (λ (α) (list α (list (quote quote) ((λ (β) α) (quote γ)))))))
            ((λ (α) ((λ (β) (list β (list (quote quote) β))) α)) (quote (λ (α) ((λ (β) (list β (list (quote quote) β))) α))))
            ((λ (α) (list α ((λ (β) (list (quote quote) β)) α))) (quote (λ (α) (list α ((λ (β) (list (quote quote) β)) α))))))
          qs)
      (⊦= 10 (length (delete-duplicates qs)))
      (⊦ every (λ (p) (equal? p (eval p))) qs)
      (⊦ every (λ (p) (equal? (list p) (μkanren-run (v -1 #t) (eval-exp° p '() v)))) qs))
  )

  ((test/quine°/fixed-variable _)
    ; footnote 1, literally: fixing the bound variable to x, run 1 gives quinec itself.
    (⊦= (list quinec) (μkanren-run (q 1 #t) (fresh° (b r) (=° `((λ (x) ,b) ,r) q) (quine° q))))
  )

  ((test/quine°/application-body _)
    (⊦= '((λ (α β γ)
             (begin (deny (equal? α 'closure)))
             (begin (deny (equal? α 'list)))
             (begin (deny (equal? α 'quote)))
             (begin (deny (equal? α 'λ)))
             (begin (deny (equal? β 'closure)))
             (begin (deny (equal? β 'list)))
             (begin (deny (equal? β 'quote)))
             (begin (deny (equal? β α)))
             (assert (every (μ v (symbol? v)) (list α β)))
             (assert (absent? 'closure γ))
             (cons (cons 'λ
                         (cons (cons α '())
                               (cons (cons (cons 'λ
                                                 (cons (cons β '())
                                                       (cons (cons 'list
                                                                   (cons α
                                                                         (cons (cons 'list
                                                                                     (cons (cons 'quote (cons 'quote '()))
                                                                                           (cons α '())))
                                                                               '())))
                                                             '())))
                                           (cons (cons 'quote (cons γ '())) '()))
                                     '())))
                   (cons (cons 'quote
                               (cons (cons 'λ
                                           (cons (cons α '())
                                                 (cons (cons (cons 'λ
                                                                   (cons (cons β '())
                                                                         (cons (cons 'list
                                                                                     (cons α
                                                                                           (cons (cons 'list
                                                                                                       (cons (cons 'quote (cons 'quote '()))
                                                                                                             (cons α '())))
                                                                                                 '())))
                                                                               '())))
                                                             (cons (cons 'quote (cons γ '())) '()))
                                                       '())))
                                     '()))
                         '()))))
        (°->list #f (take° 1 (fresh° (q x f y r) (=° `((λ (,x) (,f ,y)) ,r) q) (quine° q)))))
  )

  ((test/quine°/operand-hole _)
    ; the only operand, yet run 2 diverges.
    (⊦= '((quote (λ (x) (list x (list (quote quote) x)))))
        (μkanren-run (r 1 #t) (quine° `((λ (x) (list x (list (quote quote) x))) ,r))))
  )

  ((test/quine°/body-hole _)
    (⊦= '((λ ()
             (cons 'list
                   (cons 'x
                         (cons (cons 'list
                                     (cons (cons 'quote (cons 'quote '()))
                                           (cons 'x '())))
                               '()))))
          (λ (α β)
             (begin (deny (equal? α 'closure)))
             (begin (deny (equal? α 'x)))
             (assert (every (μ v (symbol? v)) (list α)))
             (assert (absent? 'closure β))
             (cons 'list
                   (cons (cons (cons 'λ (cons (cons α '()) (cons 'x '())))
                               (cons (cons 'quote (cons β '())) '()))
                         (cons (cons 'list
                                     (cons (cons 'quote (cons 'quote '()))
                                           (cons 'x '())))
                               '()))))
          (λ (α β)
             (begin (deny (equal? α 'closure)))
             (begin (deny (equal? α 'quote)))
             (assert (every (μ v (symbol? v)) (list α)))
             (assert (absent? 'closure β))
             (cons 'list
                   (cons 'x
                         (cons (cons 'list
                                     (cons (cons (cons 'λ
                                                       (cons (cons α '())
                                                             (cons (cons 'quote (cons 'quote '())) '())))
                                                 (cons (cons 'quote (cons β '())) '()))
                                           (cons 'x '())))
                               '())))))
        (°->list #f (take° 3 (fresh° (b) (quine° `((λ (x) ,b) (quote (λ (x) ,b))))))))
  )

  ((test/quine°/quote-hole _)
    (⊦= '((λ () (cons 'quote (cons 'quote '())))
          (λ (α β)
             (begin (deny (equal? α 'closure)))
             (begin (deny (equal? α 'quote)))
             (assert (every (μ v (symbol? v)) (list α)))
             (assert (absent? 'closure β))
             (cons (cons 'λ
                         (cons (cons α '()) (cons (cons 'quote (cons 'quote '())) '())))
                   (cons (cons 'quote (cons β '())) '())))
          (λ (α)
             (begin (deny (equal? α 'closure)))
             (assert (every (μ v (symbol? v)) (list α)))
             (cons (cons 'λ (cons (cons α '()) (cons α '())))
                   (cons (cons 'quote (cons 'quote '())) '()))))
        (°->list #f (take° 3 (fresh° (h) (quine° `((λ (x) (list x (list ,h x))) (quote (λ (x) (list x (list ,h x))))))))))
  )

  ; section 5: twines.

  ((test/twine°/run1 _)
    (let* ((X (car (°->list #f (take° 1 (fresh° (x) (twine° x))))))
           (twine ((eval X) 'α))
           (p (car twine))
           (q (cadr twine)))
      (⊦=
       '(λ (α)
          (begin (deny (equal? α 'closure)))
          (begin (deny (equal? α 'list)))
          (begin (deny (equal? α 'quote)))
          (assert (every (μ v (symbol? v)) (list α)))
          (cons (cons 'quote
                      (cons (cons (cons 'λ
                                        (cons (cons α '())
                                              (cons (cons 'list
                                                          (cons (cons 'quote (cons 'quote '()))
                                                                (cons (cons 'list
                                                                            (cons α
                                                                                  (cons (cons 'list
                                                                                              (cons (cons 'quote (cons 'quote '()))
                                                                                                    (cons α '())))
                                                                                        '())))
                                                                      '())))
                                                    '())))
                                  (cons (cons 'quote
                                              (cons (cons 'λ
                                                          (cons (cons α '())
                                                                (cons (cons 'list
                                                                            (cons (cons 'quote (cons 'quote '()))
                                                                                  (cons (cons 'list
                                                                                              (cons α
                                                                                                    (cons (cons 'list
                                                                                                                (cons (cons 'quote (cons 'quote '()))
                                                                                                                      (cons α '())))
                                                                                                          '())))
                                                                                        '())))
                                                                      '())))
                                                    '()))
                                        '()))
                            '()))
                (cons (cons (cons 'λ
                                  (cons (cons α '())
                                        (cons (cons 'list
                                                    (cons (cons 'quote (cons 'quote '()))
                                                          (cons (cons 'list
                                                                      (cons α
                                                                            (cons (cons 'list
                                                                                        (cons (cons 'quote (cons 'quote '()))
                                                                                              (cons α '())))
                                                                                  '())))
                                                                '())))
                                              '())))
                            (cons (cons 'quote
                                        (cons (cons 'λ
                                                    (cons (cons α '())
                                                          (cons (cons 'list
                                                                      (cons (cons 'quote (cons 'quote '()))
                                                                            (cons (cons 'list
                                                                                        (cons α
                                                                                              (cons (cons 'list
                                                                                                          (cons (cons 'quote (cons 'quote '()))
                                                                                                                (cons α '())))
                                                                                                    '())))
                                                                                  '())))
                                                                '())))
                                              '()))
                                  '()))
                      '())))
       X)
      (⊦= '((quote ((λ (α) (list (quote quote) (list α (list (quote quote) α)))) (quote (λ (α) (list (quote quote) (list α (list (quote quote) α)))))))
            ((λ (α) (list (quote quote) (list α (list (quote quote) α)))) (quote (λ (α) (list (quote quote) (list α (list (quote quote) α)))))))
          twine)
      (⊦ equal? q (eval p))
      (⊦ equal? p (eval q))
      (⊦≠ p q)
      (⊦= `(quote ,q) p))
  )

  ((test/twine°/forwards _)

    (define p '(quote ((λ (α) (list (quote quote) (list α (list (quote quote) α)))) (quote (λ (α) (list (quote quote) (list α (list (quote quote) α))))))))
    (define q '((λ (α) (list (quote quote) (list α (list (quote quote) α)))) (quote (λ (α) (list (quote quote) (list α (list (quote quote) α)))))))

    (⊦= (list q) (°->list #t (fresh° (v) (eval-exp° p '() v))))
    (⊦= (list p) (°->list #t (fresh° (v) (eval-exp° q '() v))))
    (⊦= (list q) (°->list #t (fresh° (r) (eval-exp° p '() r) (eval-exp° r '() p))))
    (⊦= (list p) (°->list #t (take° 1 (fresh° (e) (eval-exp° e '() q)))))
  )

  ((test/twine°/quine _)

    (define quine '((λ (α) (list α (list (quote quote) α))) (quote (λ (α) (list α (list (quote quote) α))))))

    ; every quine is trivially a twine, hence the (≠° p q) of twine°.
    (⊦= (list quine) (°->list #t (fresh° (q) (eval-exp° quine '() q) (eval-exp° q '() quine))))
    (⊦= '() (°->list #t (fresh° (q) (≠° quine q) (eval-exp° quine '() q) (eval-exp° q '() quine))))
    (let* ((X (car (°->list #f (take° 1 (fresh° (x p q) (=° p q) (eval-exp° p '() q) (eval-exp° q '() p) (=° `(,p ,q) x))))))
           (twine ((eval X) 'α)))
      (⊦= '((begin (deny (equal? α 'closure)))
            (begin (deny (equal? α 'list)))
            (begin (deny (equal? α 'quote)))
            (assert (every (μ v (symbol? v)) (list α))))
          (reified-constraints X))
      (⊦= (list quine quine) twine))
  )

  ((test/twine°/run2 _)
    (let* ((X (cadr (°->list #f (take° 2 (fresh° (x) (twine° x))))))
           (twine ((eval X) 'α 'β 'γ))
           (p (car twine))
           (q (cadr twine)))
      (⊦= '((begin (deny (equal? α 'closure)))
            (begin (deny (equal? α 'list)))
            (begin (deny (equal? α 'quote)))
            (begin (deny (equal? α 'λ)))
            (begin (deny (equal? β 'closure)))
            (begin (deny (equal? β α)))
            (assert (every (μ v (symbol? v)) (list α β)))
            (assert (absent? 'closure γ)))
          (reified-constraints X))
      (⊦= '((quote ((λ (α) (list (quote quote) (list ((λ (β) α) (quote γ)) (list (quote quote) α)))) (quote (λ (α) (list (quote quote) (list ((λ (β) α) (quote γ)) (list (quote quote) α)))))))
            ((λ (α) (list (quote quote) (list ((λ (β) α) (quote γ)) (list (quote quote) α)))) (quote (λ (α) (list (quote quote) (list ((λ (β) α) (quote γ)) (list (quote quote) α)))))))
          twine)
      (⊦ equal? q (eval p))
      (⊦ equal? p (eval q))
      (⊦≠ p q))
  )

  ; section 5: thrines.

  ((test/thrine°/search _)
    (let* ((Q (car (°->list #f (take° 1 (fresh° (x p q r)
                                          (≠° p q) (≠° q r) (≠° r p)
                                          (eval-exp° p '() q) (eval-exp° q '() r) (eval-exp° r '() p)
                                          (=° `(,p ,q ,r) x))))))
           (λQ (eval Q))
           (thrine (λQ 'α))
           (p (first thrine)) (q (second thrine)) (r (third thrine)))
      (⊦= '(λ (α)
              (begin (deny (equal? α 'closure)))
              (begin (deny (equal? α 'list)))
              (begin (deny (equal? α 'quote)))
              (assert (every (μ v (symbol? v)) (list α))))
          (drop-right Q 1))
      (⊦= '((quote (quote ((λ (α) (list 'quote (list 'quote (list α (list 'quote α)))))
                           '(λ (α) (list 'quote (list 'quote (list α (list 'quote α))))))))
            (quote ((λ (α) (list 'quote (list 'quote (list α (list 'quote α)))))
                    '(λ (α) (list 'quote (list 'quote (list α (list 'quote α)))))))
            ((λ (α) (list 'quote (list 'quote (list α (list 'quote α)))))
             '(λ (α) (list 'quote (list 'quote (list α (list 'quote α)))))))
          thrine)
      (⊦ equal? q (eval p))
      (⊦ equal? r (eval q))
      (⊦ equal? p (eval r))
      (⊦= '(#f #f #f) (list (equal? p q) (equal? q r) (equal? r p)))
      ; neither quines nor twines: the cycle has length exactly 3.
      (⊦= '(#f #f #f) (map (λ (e) (equal? e (eval e))) thrine))
      (⊦= '(#f #f #f) (map (λ (e) (equal? e (eval (eval e)))) thrine))
      (let* ((thrine/x (λQ 'x)) (p/x (first thrine/x)) (q/x (second thrine/x)) (r/x (third thrine/x)))
        (⊦= '((λ (x) (list 'quote (list 'quote (list x (list 'quote x)))))
              '(λ (x) (list 'quote (list 'quote (list x (list 'quote x))))))
            r/x)
        (⊦= (list q/x r/x p/x) (map eval thrine/x))))
  )

  ((test/thrine°/ground _)
    (let* ((R '(λ (α) (list 'quote (list 'quote (list α (list 'quote α))))))
           (r `(,R ',R))
           (q `',r)
           (p `',q))
      (⊦= (list q) (°->list #t (fresh° (v) (eval-exp° p '() v))))
      (⊦= (list r) (°->list #t (fresh° (v) (eval-exp° q '() v))))
      (⊦= (list p) (°->list #t (fresh° (v) (eval-exp° r '() v))))
      (⊦= '() (°->list #t (fresh° (v) (eval-exp° p '() p))))
      (⊦= '() (°->list #t (fresh° (v) (eval-exp° q '() q))))
      (⊦= '() (°->list #t (fresh° (v) (eval-exp° r '() r)))))
  )

  ; appendix A: the extended interpreter.

  ((test/eval-exp/extended°/fact5 _)
    ; the factorial of five through the Poorman's Y combinator is 120 = (0 0 0 1 1 1 1).
    (define rel-fact5
      `((λ (f)
          ((f f) (int-exp ,(build-num 5))))
        (λ (f)
          (λ (n)
            (if (zero? n)
                (int-exp ,(build-num 1))
                (* n ((f f) (sub1 n))))))))
    (⊦= '((int-val (0 0 0 1 1 1 1))) (μkanren-run (q -1 #t) (eval-exp/extended° rel-fact5 '() q)))
  )

  ((test/eval-exp/extended°/forward _)
    (⊦= '((closure (x y) x ())) (μkanren-run (q -1 #t) (eval-exp/extended° '(λ (x y) x) '() q)))
    (⊦= '((b . a)) (μkanren-run (q -1 #t) (eval-exp/extended° '((λ (x y) (cons y x)) (quote a) (quote b)) '() q)))
    (⊦= '((#t)) (μkanren-run (q -1 #t) (eval-exp/extended° '(cdr (cons #f (cons #t (quote ())))) '() q)))
    (⊦= '(yes) (μkanren-run (q -1 #t) (eval-exp/extended° '(if (zero? (sub1 (int-exp (1)))) (quote yes) (quote no)) '() q)))
    (⊦= '(#t) (μkanren-run (q -1 #t) (eval-exp/extended° '(not (zero? (int-exp (1)))) '() q)))
    (⊦= '(((int-val (1)) #t)) (μkanren-run (q -1 #t) (eval-exp/extended° '(list (int-exp (1)) #t) '() q)))
  )

  ((test/eval-exp/extended°/guards _)
    (⊦= '(#t) (μkanren-run (q -1 #t) (eval-exp/extended° '((λ (not) (not #t)) (λ (x) x)) '() q)))
    (⊦= '() (μkanren-run (q -1 #t) (eval-exp/extended° '(quote (int-val (1))) '() q)))
    (⊦= '() (μkanren-run (q -1 #t) (eval-exp/extended° '(cons (int-exp (1)) #t) '() q)))
    (⊦= '() (μkanren-run (q -1 #t) (eval-exp/extended° '(car (int-exp (1))) '() q)))
    (⊦= '() (μkanren-run (q -1 #t) (eval-exp/extended° '(sub1 (int-exp ())) '() q)))
  )

  ((test/eval-exp/extended°/backward-arithmetic _)
    (⊦= '((0 1)) (μkanren-run (q -1 #t) (eval-exp/extended° `(* (int-exp ,q) (int-exp (1 1))) '() '(int-val (0 1 1)))))
    (⊦= '(((1) (0 1 1)) ((0 1 1) (1)) ((0 1) (1 1)) ((1 1) (0 1)))
        (μkanren-run (q -1 #t) (fresh° (a b) (=° q (list a b)) (eval-exp/extended° `(* (int-exp ,a) (int-exp ,b)) '() '(int-val (0 1 1))))))
    (⊦= '((1 1 1)) (μkanren-run (q -1 #t) (eval-exp/extended° `(sub1 (int-exp ,q)) '() '(int-val (0 1 1)))))
  )

  ((test/eval-exp/extended°/six _)
    ; the paper's run12: its first 11 answers come first here too, in the same order, but its 12th arrives 15th.
    (let1 (six (μkanren-run (q 15 #t) (eval-exp/extended° q '() `(int-val ,(build-num 6)))))
      (⊦= '((int-exp (0 1 1))
            ((λ () (int-exp (0 1 1))))
            (sub1 (int-exp (1 1 1)))
            ((λ (α) (int-exp (0 1 1))) (quote β))
            (* (int-exp (1)) (int-exp (0 1 1)))
            (* (int-exp (0 1 1)) (int-exp (1)))
            (* (int-exp (0 1)) (int-exp (1 1)))
            ((λ (α) (int-exp (0 1 1))) (list))
            (car (list (int-exp (0 1 1))))
            ((λ () ((λ () (int-exp (0 1 1))))))
            (sub1 ((λ () (int-exp (1 1 1)))))
            (car (list (int-exp (0 1 1)) (quote α))))
          (take six 12))
      (⊦= '(* (int-exp (0 1)) (int-exp (1 1))) (list-ref six 6))   ; "the 7th value in this list"
      (⊦= '((λ () (sub1 (int-exp (1 1 1))))) (list-ref six 14)))
    (⊦= '(λ (α β)
            (begin (deny (equal? α 'int-exp)))
            (assert (absent? 'closure β))
            (assert (absent? 'int-val β))
            (cons (cons 'λ
                        (cons (cons α '())
                              (cons (cons 'int-exp (cons (cons 0 (cons 1 (cons 1 '()))) '())) '())))
                  (cons (cons 'quote (cons β '())) '())))
        (list-ref (°->list #f (take° 4 (fresh° (q) (eval-exp/extended° q '() `(int-val ,(build-num 6)))))) 3))
  )

  ((test/eval-exp/extended°/boolean-quines _)
    (⊦= '((λ () #t) (λ () #f)) (°->list #f (take° 2 (fresh° (q) (eval-exp/extended° q '() q)))))
    (let1 (booleans (μkanren-run (q 2 #t) (eval-exp/extended° q '() q)))
      (⊦= '(#t #f) booleans)
      (⊦= booleans (map eval booleans)))
  )

  ((test/eval-exp/extended°/quines _)
    (let* ((Qs (°->list #f (take° 5 (fresh° (q) (eval-exp/extended° q '() q)))))
           (quines (map (λ (Q) (apply (eval Q) (take '(α β) (length (cadr Q))))) Qs)))
      (⊦= '(λ (α)
              (begin (deny (equal? α 'closure)))
              (begin (deny (equal? α 'int-val)))
              (begin (deny (equal? α 'list)))
              (begin (deny (equal? α 'quote)))
              (assert (every (μ v (symbol? v)) (list α)))
              (cons (cons 'λ
                          (cons (cons α '())
                                (cons (cons 'list
                                            (cons α
                                                  (cons (cons 'list
                                                              (cons (cons 'quote (cons 'quote '()))
                                                                    (cons α '())))
                                                        '())))
                                      '())))
                    (cons (cons 'quote
                                (cons (cons 'λ
                                            (cons (cons α '())
                                                  (cons (cons 'list
                                                              (cons α
                                                                    (cons (cons 'list
                                                                                (cons (cons 'quote (cons 'quote '()))
                                                                                      (cons α '())))
                                                                          '())))
                                                        '())))
                                      '()))
                          '())))
          (list-ref Qs 2))
      (⊦= '(λ (α β)
              (begin (deny (equal? α 'car)))
              (begin (deny (equal? α 'closure)))
              (begin (deny (equal? α 'int-val)))
              (begin (deny (equal? α 'list)))
              (begin (deny (equal? α 'quote)))
              (assert (every (μ v (symbol? v)) (list α)))
              (assert (absent? 'closure β))
              (assert (absent? 'int-val β))
              (cons (cons 'λ
                          (cons (cons α '())
                                (cons (cons 'list
                                            (cons α
                                                  (cons (cons 'list
                                                              (cons (cons 'car
                                                                          (cons (cons 'quote (cons (cons 'quote β) '()))
                                                                                '()))
                                                                    (cons α '())))
                                                        '())))
                                      '())))
                    (cons (cons 'quote
                                (cons (cons 'λ
                                            (cons (cons α '())
                                                  (cons (cons 'list
                                                              (cons α
                                                                    (cons (cons 'list
                                                                                (cons (cons 'car
                                                                                            (cons (cons 'quote (cons (cons 'quote β) '()))
                                                                                                  '()))
                                                                                      (cons α '())))
                                                                          '())))
                                                        '())))
                                      '()))
                          '())))
          (list-ref Qs 3))
      (⊦= '(λ (α)
              (begin (deny (equal? α 'closure)))
              (begin (deny (equal? α 'int-val)))
              (begin (deny (equal? α 'list)))
              (begin (deny (equal? α 'quote)))
              (assert (every (μ v (symbol? v)) (list α)))
              (cons (cons 'λ
                          (cons (cons α '())
                                (cons (cons 'list
                                            (cons (cons 'list
                                                        (cons (cons 'quote (cons 'λ '()))
                                                              (cons (cons 'quote (cons (cons α '()) '()))
                                                                    (cons α '()))))
                                                  (cons (cons 'list
                                                              (cons (cons 'quote (cons 'quote '()))
                                                                    (cons α '())))
                                                        '())))
                                      '())))
                    (cons (cons 'quote
                                (cons (cons 'list
                                            (cons (cons 'list
                                                        (cons (cons 'quote (cons 'λ '()))
                                                              (cons (cons 'quote (cons (cons α '()) '()))
                                                                    (cons α '()))))
                                                  (cons (cons 'list
                                                              (cons (cons 'quote (cons 'quote '()))
                                                                    (cons α '())))
                                                        '())))
                                      '()))
                          '())))
          (list-ref Qs 4))
      (⊦= '(#t
            #f
            ((λ (α) (list α (list (quote quote) α)))
             (quote (λ (α) (list α (list (quote quote) α)))))
            ((λ (α) (list α (list (car (quote (quote . β))) α)))
             (quote (λ (α) (list α (list (car (quote (quote . β))) α)))))
            ((λ (α) (list (list (quote λ) (quote (α)) α) (list (quote quote) α)))
             (quote (list (list (quote λ) (quote (α)) α) (list (quote quote) α)))))
          quines)
      (⊦ every (λ (quine) (equal? quine (eval quine))) quines))
  )

  ((test/symbol° _)
    (⊦= '((λ (α) (assert (every (μ v (symbol? v)) (list α))) α)) (°->list #f (fresh° (s) (symbol° s))))
    (⊦= '((λ (α) α)) (°->list #f (fresh° (s r) (symbol° r))))
  )
  
  ((test/≠° _)
    (⊦= '() (°->list #f (fresh° (s) (≠° (+ 2 3) 5))))
    (⊦= '((λ (α) α)) (°->list #f (fresh° (s) (≠° (* 2 3) 5))))
    (⊦= '((λ (α) (begin (deny (equal? α 5))) (begin (deny (equal? α 6))) α)) (°->list #f (fresh° (q x) (≠° 5 q) (=° x q) (≠° 6 x))))
    (⊦= '((λ (α) α)) (°->list #f (fresh° (q y z) (≠° (cons y z) q))))
    (⊦= '((λ (α β γ) (begin (deny (equal? α (cons β γ)))) (cons α (cons β (cons γ '()))))) (°->list #f (fresh° (q x y z) (≠° (cons y z) x) (=° (list x y z) q))))
    (⊦= '((λ (α) (begin (deny (equal? α 6))) (cons (cons 5 α) (cons 5 (cons α (quote ())))))) (°->list #f (fresh° (q x y z) (=° (cons y z) x) (≠° (cons 5 6) x) (=° 5 y) (=° (list x y z) q))))
    (⊦= '((λ (α) (cons (cons 6 α) (cons 6 (cons α (quote ())))))) (°->list #f (fresh° (q x y z) (=° (cons y z) x) (≠° (cons 5 6) x) (=° 6 y) (=° (list x y z) q))))
    (⊦= '((λ (α β γ) (begin (deny (equal? α 5))) (begin (deny (equal? α 6))) (begin (deny (and (equal? β 2) (equal? γ 1)))) (cons α (cons β (cons γ (quote ())))))) (°->list #f (fresh° (q x y z) (≠° 5 x) (≠° 6 x) (≠° (list y 1) (list 2 z)) (=° (list x y z) q))))
    (⊦= '((λ (α) (begin (deny (equal? α 1))) α)) (°->list #f (fresh° (s) (≠° s 1))))
    (⊦= '() (°->list #f (fresh° (s) (≠° s 1) (=° s 1))))
    (⊦= '((λ (α) (begin (deny (equal? α (cons 'a (cons 'b '()))))) α)) (°->list #f (fresh° (s) (≠° s '(a b)))))
    (⊦= '((λ (α β) (begin (deny (and (equal? α 1) (equal? β 2)))) (cons α (cons β (quote ())))))
        (°->list #f (fresh° (q p r) (≠° (list p r) '(1 2)) (=° q (list p r)))))
    (⊦= '((λ (α) (begin (deny (equal? α 2))) (cons 1 (cons α (quote ())))))
        (°->list #f (fresh° (q p r) (≠° (list p r) '(1 2)) (=° p 1) (=° q (list p r)))))
    (⊦= '() (°->list #f (fresh° (q p r) (≠° (list p r) '(1 2)) (=° p 1) (=° r 2) (=° q (list p r)))))
  )

  ((test/rember°/naive _)

    (define-relation (rember° x ls out)
      (cond° 
        ((=° '() ls) (=° '() out))
        ((fresh° (a d) (=° `(,a . ,d) ls) (=° a x) (=° d out)))
        ((fresh° (a d res)
                (=° `(,a . ,d) ls)
                (=° `(,a . ,res) out)
                (rember° x d res)))))

    (⊦= '((a c b d)) (μkanren-run (q 1 #t) (rember° 'b '(a b c b d) q)))
    (⊦= '((a b c)) (μkanren-run (q 1 #t) (rember° 'd '(a b c) q)))
    (⊦= '((a c b d) (a b c d) (a b c b d)) (μkanren-run (q -1 #t) (rember° 'b '(a b c b d) q)))
    (⊦= '(α) (μkanren-run (q -1 #t) (rember° 'b '(b) '(b))))
  )

  ((test/rember°/fixed _)

    (define-relation (rember° x ls out)
      (cond° 
        ((=° '() ls) (=° '() out))
        ((fresh° (a d) (=° `(,a . ,d) ls) (=° a x) (=° d out)))
        ((fresh° (a d res)
                (=° `(,a . ,d) ls)
                (≠° a x)
                (=° `(,a . ,res) out)
                (rember° x d res)))))

    (⊦= '((a c b d)) (μkanren-run (q -1 #t) (rember° 'b '(a b c b d) q)))
    (⊦= '() (μkanren-run (q -1 #t) (rember° 'b '(b) '(b))))
    (⊦= '(  (λ () (cons (quote a) (cons (cons (quote b) (cons (quote c) (quote ()))) (quote ()))))
            (λ () (cons (quote b) (cons (cons (quote a) (cons (quote c) (quote ()))) (quote ())))) 
            (λ () (cons (quote c) (cons (cons (quote a) (cons (quote b) (quote ()))) (quote ())))) 
            (λ (α) (begin (deny (equal? α (quote a)))) (begin (deny (equal? α (quote b)))) (begin (deny (equal? α (quote c)))) (cons α (cons (cons (quote a) (cons (quote b) (cons (quote c) (quote ())))) (quote ()))))) 
      (°->list #f (fresh° (q x out) (rember° x '(a b c) out) (=° (list x out) q))))
  )

  ((test/absent° _)
    (⊦= '((λ (α β) (assert (absent? (quote panda) α)) (assert (absent? (quote panda) β)) (cons (quote jackal) (cons (cons α (cons (quote leopard) (cons β (quote ())))) (quote ())))))
        (°->list #f (fresh° (q x y) (=° `(jackal (,y leopard ,x)) q) (absent° 'panda q))))
    
  )

)

(unittest/✓ microkanren-untagged-suite)

#|
(define Q '(λ (α) (begin (deny (equal? α (quote closure)))) (begin (deny (equal? α (quote list)))) (begin (deny (equal? α (quote quote)))) (assert (every (μ v (symbol? v)) (list α))) (cons (cons (quote λ) (cons (cons α (quote ())) (cons (cons (quote list) (cons α (cons (cons (quote list) (cons (cons (quote quote) (cons (quote quote) (quote ()))) (cons α (quote ())))) (quote ())))) (quote ())))) (cons (cons (quote quote) (cons (cons (quote λ) (cons (cons α (quote ())) (cons (cons (quote list) (cons α (cons (cons (quote list) (cons (cons (quote quote) (cons (quote quote) (quote ()))) (cons α (quote ())))) (quote ())))) (quote ())))) (quote ()))) (quote ())))))
(equal? ((eval Q ) 'α) Q)

(pp Q)

(define Q
(λ (α)
    (begin (deny (equal? α 'closure)))
    (begin (deny (equal? α 'list)))
    (begin (deny (equal? α 'quote)))
    (assert (every (μ v (symbol? v)) (list α)))
    (cons (cons 'λ
                (cons (cons α '())
                      (cons (cons 'list
                                  (cons α
                                        (cons (cons 'list
                                                    (cons (cons 'quote (cons 'quote '()))
                                                          (cons α '())))
                                              '())))
                            '())))
          (cons (cons 'quote
                      (cons (cons 'λ
                                  (cons (cons α '())
                                        (cons (cons 'list
                                                    (cons α
                                                          (cons (cons 'list
                                                                      (cons (cons 'quote (cons 'quote '()))
                                                                            (cons α '())))
                                                                '())))
                                              '())))
                            '()))
                '())))
)

(Q 'α) -> ((λ (α) (list α (list (quote quote) α))) (quote (λ (α) (list α (list (quote quote) α)))))
       -> ((λ (α) (list α (list (quote quote) α))) (quote (λ (α) (list α (list (quote quote) α)))))

|#