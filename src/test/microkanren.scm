
(import 
  scheme (chicken base) (chicken condition) (chicken memory representation) (chicken sort)
  srfi-1
  (aux unittest) (aux base) (aux stream) (aux kanren micro) (aux fds sbral))

; The following is a sample database of employees and their salaries in various departments.
; taken from https://www.postgresql.org/docs/current/tutorial-window.html.
(define-relation (empsalary° depname empno salary)
  (or°  (and° (=° depname 'develop) (=° empno 7)  (=° salary 4200))
        (and° (=° depname 'develop) (=° empno 8)  (=° salary 6000))
        (and° (=° depname 'develop) (=° empno 9)  (=° salary 4500))
        (and° (=° depname 'develop) (=° empno 10) (=° salary 5200))
        (and° (=° depname 'develop) (=° empno 11) (=° salary 5200))
        (and° (=° depname 'personnel) (=° empno 2)  (=° salary 3900))
        (and° (=° depname 'personnel) (=° empno 5)  (=° salary 3500))
        (and° (=° depname 'sales)     (=° empno 1)  (=° salary 5000))
        (and° (=° depname 'sales)     (=° empno 3)  (=° salary 4800))
        (and° (=° depname 'sales)     (=° empno 4)  (=° salary 4800))))

(define-suite microkanren-suite

  ((test/find _)
   (let* ((v1 (make-μkanren-var 0))
          (v2 (make-μkanren-var 1))
          (s0 μkanren-state-empty)
          (s1 (make-μkanren-state 1 (cons/sbral 'a (μkanren-state-S s0)) empty/sbral empty/sbral empty/sbral '()))
          (s2 (make-μkanren-state 2 (cons/sbral v1 (μkanren-state-S s1)) empty/sbral empty/sbral empty/sbral '())))
     (⊦= 5 (μkanren-state-find 5 s2))
     (⊦= 'a (μkanren-state-find v1 s2))
     (⊦= 'a (μkanren-state-find v2 s2))))

  ((test/find/values _)
   (let* ((v1 (make-μkanren-var 0))
          (v2 (make-μkanren-var 1))
          (s0 μkanren-state-empty)
          (s1 (make-μkanren-state 1 (cons/sbral 'a (μkanren-state-S s0)) empty/sbral empty/sbral empty/sbral '()))
          (s2 (make-μkanren-state 2 (cons/sbral v1 (μkanren-state-S s1)) empty/sbral empty/sbral empty/sbral '())))
     (⊦= 1 (μkanren-var-index/sbral (μkanren-state-S s2) v1))
     (⊦= 0 (μkanren-var-index/sbral (μkanren-state-S s2) v2))
     (⊦= 'a (μkanren-sbral-ref/var (μkanren-state-S s2) v1))
     (⊦= v1 (μkanren-sbral-ref/var (μkanren-state-S s2) v2))
     (⊦= '(a a 5) (μkanren-state-find/values (list v1 v2 5) s2))))

  ((test/state-with _)
   (let* ((s (make-μkanren-state 3 empty/sbral '((d)) '((a)) '((t)) '(tag)))
          (s* (μkanren-state-with s D: '() tags: '())))
     (⊦= 3 (μkanren-state-vars-count s*))
     (⊦= '() (μkanren-state-D s*))
     (⊦= '((a)) (μkanren-state-A s*))
     (⊦= '((t)) (μkanren-state-T s*))
     (⊦= '() (μkanren-state-tags s*))
     (⊦= '((d)) (μkanren-state-D (μkanren-state-with s)))))

  ((test/state-unify/stores _)
   ; a goal that binds through the exported unifier leaves the stores to the next =° that changes the state,
   ; which verifies them as a whole, whether the goal answers the unifier's state or only its substitution.
   (define ((raw=° u v) s) (let1 (r (μkanren-state-unify u v s '())) (if r (list (car r)) '())))
   (define ((rawS=° u v) s) (let1 (r (μkanren-state-unify u v s '())) (if r (list (μkanren-state-with s S: (μkanren-state-S (car r)))) '())))
   (⊦= '() (°->list #f (fresh° (q) (fresh° (x y) (=° q (list x y)) (number° x) (raw=° x 'a) (=° y 1)))))
   (⊦= '() (°->list #f (fresh° (q) (fresh° (x y) (=° q (list x y)) (absent° 'a x) (raw=° x 'a) (=° y 1)))))
   (⊦= '() (°->list #f (fresh° (q) (fresh° (x y) (=° q (list x y)) (number° x) (rawS=° x 'a) (=° y 1)))))
   (⊦= '() (°->list #f (fresh° (q) (fresh° (x y) (=° q (list x y)) (absent° 'a x) (rawS=° x 'a) (=° y 1))))))

  ((test/rem-subsumed/by _)
   (⊦= '(2 3 1) ((μkanren-rem-subsumed/by member?) '(1 2 1 3 2)))
   (⊦= '() ((μkanren-rem-subsumed/by member?) '()))
   ; keyed, an element is tried only against those whose key is among its keys: the same answer when they
   ; hold every element that could subsume it, as the identity does for `member?`
   (⊦= '(2 3 1) ((μkanren-rem-subsumed/by member? identity list) '(1 2 1 3 2)))
   (⊦= '(6) ((μkanren-rem-subsumed/by (λ (x L) (pair? L))) '(1 2 3 4 6)))
   (⊦= '(6 3) ((μkanren-rem-subsumed/by (λ (x L) (pair? L)) (μ x (modulo x 2)) (μ x (list (modulo x 2)))) '(1 2 3 4 6))))

  ((test/foldr/verify _)
   (define (step x acc) (and (odd? x) (cons x acc)))
   (⊦= '(1 3 5) (μkanren-foldr/verify step '(1 3 5)))
   (⊦= #f (μkanren-foldr/verify step '(1 2 5)))
   (⊦= '() (μkanren-foldr/verify step '()))
   ; μkanren-verify-T+ takes the tag itself, not the store it was read from
   (⊦= '() ((μkanren-verify-T+ 'x μkanren-tag/sym μkanren-state-empty) '()))
   (⊦= #f ((μkanren-verify-T+ 5 μkanren-tag/sym μkanren-state-empty) '())))

  ((test/partition* _)
   (let1 (A (list (cons 'a μkanren-tag/sym) (cons 'b μkanren-tag/num) (cons 'a μkanren-tag/sym) (cons 'c μkanren-tag/sym)))
     (⊦= `((,μkanren-tag/sym a c) (,μkanren-tag/num b)) (μkanren-partition* A)))
   (⊦= '((λ (α β γ) (assert (every (μ v (symbol? v)) (list α γ))) (assert (every (μ v (number? v)) (list β))) (cons α (cons β (cons γ '())))))
       (°->list #f (fresh° (q a b c) (=° q (list a b c)) (symbol° a) (number° b) (symbol° c)))))

  ((test/=° _) (⊦= '(α) (°->list/ground (=° 'z 'z))))


  ((test/sharing _)
   (⊦= '(((α z) z (α β)))
         (°->list/ground (fresh° r ← (n q x)
                           (=° q 'z)
                           (fresh° (w r) (=° n (list w q)) (=° x (list w r)))))))

  ((test/fresh°/typo _)
    ; the destructuring form needs its `←`, so a goal in the place of the variables is an expansion error
    (⊦= 'syntax-error (condition-case (eval '(μkanren-run (q 3 #f) (fresh° x (null° x) (=° q x)))) ((exn syntax) 'syntax-error)))
    (⊦= '(()) (μkanren-run (q 3 #t) (fresh° (x) (null° x) (=° q x)))))

  ((test/define-relation/recursive _)
    ; a relation is delayed, so its recursion needs no `fresh°` to stop at construction
    (define-relation (ones° x) (or° (=° x 1) (ones° x)))
    (⊦= '(1 1 1) (°->list/ground (take° 3 (fresh° (q) (ones° q))))))

  ((test/peano° _)
   (define-relation (peano° n) (or° (=° n 'z) (fresh° (r) (=° n `(s ,r)) (peano° r))))
   (define-relation (church° n) (fresh° (b) (=° n `(λ (s) (λ (z) ,b))) (peano° b)))
   (⊦= '(z (s z)
             (s (s z))
             (s (s (s z)))
             (s (s (s (s z))))
             (s (s (s (s (s z)))))
             (s (s (s (s (s (s z))))))
             (s (s (s (s (s (s (s z)))))))
             (s (s (s (s (s (s (s (s z))))))))
             (s (s (s (s (s (s (s (s (s z))))))))))
         (°->list/ground (take° 10 (fresh° (n) (peano° n)))))

   (⊦= '((λ (s) (λ (z) z))
           (λ (s) (λ (z) (s z)))
           (λ (s) (λ (z) (s (s z))))
           (λ (s) (λ (z) (s (s (s z)))))
           (λ (s) (λ (z) (s (s (s (s z))))))
           (λ (s) (λ (z) (s (s (s (s (s z)))))))
           (λ (s) (λ (z) (s (s (s (s (s (s z))))))))
           (λ (s) (λ (z) (s (s (s (s (s (s (s z)))))))))
           (λ (s) (λ (z) (s (s (s (s (s (s (s (s z))))))))))
           (λ (s) (λ (z) (s (s (s (s (s (s (s (s (s z))))))))))))
         (°->list/ground (take° 10 (fresh° (n) (church° n))))))

  ((test/append° _)

   (define-relation (append° r s rs)
     (cond°
       ((null° r) (=° s rs))
       ((fresh° (a d c) (cons° a d r) (append° d s c) (cons° a c rs)))))
  
   (⊦ equal?
      '((λ (α) α) (λ (α β) (cons α β)) (λ (α β γ) (cons α (cons β γ))) (λ (α β γ δ) (cons α (cons β (cons γ δ)))) (λ (α β γ δ ε) (cons α (cons β (cons γ (cons δ ε))))) (λ (α β γ δ ε ζ) (cons α (cons β (cons γ (cons δ (cons ε ζ)))))) (λ (α β γ δ ε ζ η) (cons α (cons β (cons γ (cons δ (cons ε (cons ζ η))))))) (λ (α β γ δ ε ζ η θ) (cons α (cons β (cons γ (cons δ (cons ε (cons ζ (cons η θ)))))))) (λ (α β γ δ ε ζ η θ ι) (cons α (cons β (cons γ (cons δ (cons ε (cons ζ (cons η (cons θ ι))))))))) (λ (α β γ δ ε ζ η θ ι κ) (cons α (cons β (cons γ (cons δ (cons ε (cons ζ (cons η (cons θ (cons ι κ)))))))))))
      (μkanren-run (l 10 #f) (fresh° (a d) (append° a d l))))
  )
  
  ((test/=°/nan _)
    ; unification is reflexive on NaN too, although CHICKEN's (eqv? +nan.0 +nan.0) is #f
    (⊦= 1 (length (μkanren-run (q 2 #f) (=° q (/ 0. 0.)) (=° q (/ 0. 0.)))))
    (⊦= 1 (length (μkanren-run (q 2 #f) (=° q (list 1 (/ 0. 0.))) (=° q (list 1 (/ 0. 0.))))))
    (⊦= '() (μkanren-run (q 2 #f) (=° q (/ 0. 0.)) (≠° q (/ 0. 0.))))
    (⊦= '() (μkanren-run (q 2 #f) (=° q (/ 0. 0.)) (=° q 1.)))
    ; absent° finds a NaN tag in a NaN, bare or nested, in both goal orders, as it finds a NaN inside a compound tag
    (⊦= '() (μkanren-run (q 2 #f) (absent° (/ 0. 0.) q) (=° q (/ 0. 0.))))
    (⊦= '() (μkanren-run (q 2 #f) (=° q (/ 0. 0.)) (absent° (/ 0. 0.) q)))
    (⊦= '() (μkanren-run (q 2 #f) (absent° (/ 0. 0.) q) (=° q (list 1 (/ 0. 0.)))))
    (⊦= '() (μkanren-run (q 2 #f) (absent° (list (/ 0. 0.)) q) (=° q (list (/ 0. 0.)))))
    (⊦= 1 (length (μkanren-run (q 2 #f) (absent° (/ 0. 0.) q) (=° q (list 1. 2)))))
    ; a complex number with a NaN part equals only one with the same parts, although nan? is #t on both
    (let1 (c (λ (s) (string->number s)))
      (⊦= 1 (length (μkanren-run (q 2 #f) (=° q (c "+nan.0+1i")) (=° q (c "+nan.0+1i")))))
      (⊦= '() (μkanren-run (q 2 #f) (=° q (c "+nan.0+1i")) (=° q (c "+nan.0+2i"))))
      (⊦= '() (μkanren-run (q 2 #f) (=° q (c "+nan.0+1i")) (=° q (c "1+nan.0i"))))
      (⊦= '() (μkanren-run (q 2 #f) (=° q (c "+nan.0+1i")) (=° q (/ 0. 0.))))
      (⊦= '() (μkanren-run (q 2 #f) (=° q (c "+nan.0+1i")) (≠° q (c "+nan.0+1i"))))
      (⊦= 1 (length (μkanren-run (q 2 #f) (=° q (c "+nan.0+1i")) (≠° q (c "7+nan.0i")))))))

  ((test/≠°/verify _)
    ; a disequality is verified again when =° binds one of its variables, after any other unification
    (⊦= '() (°->list #f (fresh° (q x y) (≠° x y) (=° q 1) (=° x 5) (=° y 5))))
    (⊦= '() (°->list #f (fresh° (q x y) (≠° (list x y) '(1 2)) (=° q 0) (=° x 1) (=° y 2))))
    (⊦= '() (°->list #f (fresh° (q x y z) (≠° x (list y)) (=° q 0) (=° y z) (=° x (list z)))))
    ; one satisfied deep in its terms, by the occurs check, is dropped from the answer
    (⊦= '((λ (α) α)) (°->list #f (fresh° (q x b) (=° q b) (≠° b (list x)) (=° x b))))
    (⊦= '((λ (α) (begin (deny (equal? α (cons 1 '())))) (cons α (cons 1 '()))))
        (°->list #f (fresh° (q b x) (=° q (list b x)) (≠° b (list x)) (=° x 1)))))

  ((test/=°/structure _)
    (define-record person name age)
    (define p (make-person 'alice 30))
    (⊦= #t (record-instance? p))
    (⊦= `((record ,p)) (°->list/ground (fresh° (r) (=° r (list 'record p)))))
    (⊦= `((record ,(make-person 'α 30))) (°->list/ground (fresh° (r a) (=° r (list 'record (make-person a 30)))))))

  ((test/=°/structure/vector _)
    (define-record person name age)
    (define p (make-person 'alice 30))
    (⊦= #t (record-instance? p))
    (⊦= #(person alice 30) (record->vector p))
    (⊦= '((person alice 30)) (°->list/ground (fresh° r ← (t n a) (=° `#(,t ,n ,a) p)))))

  ((test/=°/structure/vector/answer _)
    (⊦= '((λ () (vector 1 2))) (°->list #f (fresh° (q) (=° q (vector 1 2)))))
    (⊦= '((λ (α) (vector 1 α))) (°->list #f (fresh° (q r) (=° q (vector 1 r))))))

  ((test/var->symbol/overflow _)
    (⊦= '((α β γ δ ε ζ η θ ι κ λ μ ν ξ ο π ρ σ τ υ φ χ ψ ω α1 β1))
        (°->list/ground (fresh° r ← (a b c d e f g h i j k l m n o p q s t u v w x y z aa)))))

  ((test/ground/constrained _)
    ; grounding drops the constraints instead of re-checking them against the placeholder symbols
    (⊦= '(α) (°->list/ground (fresh° (q) (number° q))))
    (⊦= '(α) (°->list/ground (fresh° (q) (≠° q 'α))))
    (⊦= '(α) (°->list/ground (fresh° (q) (symbol° q)))))

  ((test/absent°/symbol° _)
    ; a type tag turns the absento of the same variable into a disequality, in either order
    (⊦= '() (°->list #f (fresh° (q) (symbol° q) (absent° 'closure q) (=° q 'closure))))
    (⊦= '() (°->list #f (fresh° (q) (absent° 'closure q) (symbol° q) (=° q 'closure))))
    (⊦= '((λ (α) (begin (deny (equal? α 'closure))) (assert (every (μ v (symbol? v)) (list α))) α))
        (°->list #f (fresh° (q) (symbol° q) (absent° 'closure q))))
    ; among other keys of A: only the key equal to the absento's variable converts it
    (⊦= '((λ (α β γ) (begin (deny (equal? β 'a))) (assert (every (μ v (symbol? v)) (list α β γ))) (cons α (cons β (cons γ '())))))
        (°->list #f (fresh° (q x y z) (=° q (list x y z)) (symbol° x) (symbol° y) (symbol° z) (absent° 'a y)))))

  ((test/≠°/subsumed _)
    ; a type tag subsumes a prefix of D through any of its pairs, the first or a later one
    (⊦= '((λ (α β) (assert (every (μ v (symbol? v)) (list α β))) (cons α (cons β '()))))
        (°->list #f (fresh° (q x y) (=° q (list x y)) (symbol° x) (symbol° y) (≠° (list x y) (list 1 2)))))
    (⊦= '((λ (α β) (assert (every (μ v (symbol? v)) (list α β))) (cons α (cons β '()))))
        (°->list #f (fresh° (q x y) (=° q (list x y)) (symbol° x) (symbol° y) (≠° (list x y) (list 'a 2))))))

  ((test/≠°/subsumed/keyed _)
    ; a prefix of D subsumes another one through its variable of largest index, which is not the left side
    ; of its first pair, whatever the order of the disequalities and of their pairs
    (⊦= '((λ (α β γ) (begin (deny (equal? α β))) (cons α (cons β (cons γ '())))))
        (°->list #f (fresh° (q x y z) (=° q (list x y z)) (≠° x y) (≠° (list x z) (list y 1)))))
    (⊦= '((λ (α β γ) (begin (deny (equal? α β))) (cons α (cons β (cons γ '())))))
        (°->list #f (fresh° (q x y z) (=° q (list x y z)) (≠° (list z x) (list 1 y)) (≠° y x))))
    (⊦= '((λ (α β) (begin (deny (equal? β 2))) (cons α (cons β '()))))
        (°->list #f (fresh° (q x y) (=° q (list x y)) (≠° (list x y) (list 1 2)) (≠° y 2))))
    ; every prefix shares x, introduced after the variables it must differ from, and is keyed on another one
    (⊦= '((λ (α β γ δ) (begin (deny (equal? α δ))) (begin (deny (equal? β δ))) (cons α (cons β (cons γ (cons δ '()))))))
        (°->list #f (fresh° (q a b c x) (=° q (list a b c x)) (≠° x a) (≠° x b) (≠° (list x c) (list b 1)))))
    (⊦= '((λ (α β γ δ) (begin (deny (equal? α δ))) (begin (deny (equal? β δ))) (begin (deny (equal? γ δ)))
            (cons α (cons β (cons γ (cons δ '()))))))
        (°->list #f (fresh° (q a b c) (fresh° (x) (=° q (list a b c x)) (≠° x a) (≠° (list c x) (list 1 b)) (≠° x b) (≠° x c))))))

  ((test/stores/order _)
    ; an answer does not depend on the order in which its constraints were posted, nor on what a record holds
    (⊦= (°->list #f (fresh° (q) (absent° 'a q) (absent° '(b) q) (absent° 'c q)))
        (°->list #f (fresh° (q) (absent° 'c q) (absent° '(b) q) (absent° 'a q))))
    (⊦= (°->list #f (fresh° (q) (≠° q (make-record-instance 'box 1)) (≠° q (make-record-instance 'box 2))))
        (°->list #f (fresh° (q) (≠° q (make-record-instance 'box 2)) (≠° q (make-record-instance 'box 1)))))
    (⊦= (°->list #f (fresh° (q x y) (=° q (list x y)) (≠° (list x y) '(1 2)) (≠° (list y x) '(3 4))))
        (°->list #f (fresh° (q x y) (=° q (list x y)) (≠° (list y x) '(3 4)) (≠° (list x y) '(1 2))))))

  ((test/=°/hands-on _)
    ; =° hands the constraints of a variable it binds on to the variable's walk, whatever it is
    (⊦= '() (°->list #f (fresh° (q x y) (symbol° x) (=° x y) (=° y 5))))
    (⊦= '() (°->list #f (fresh° (q x y) (symbol° x) (number° y) (=° x y))))
    (⊦= '() (°->list #f (fresh° (q x y) (≠° x 1) (=° x y) (=° y 1))))
    (⊦= '() (°->list #f (fresh° (q x y) (absent° 'z x) (=° x (list y)) (=° y 'z))))
    (⊦= '() (°->list #f (fresh° (q x y z) (absent° '(1 2) (cons x y)) (=° y (list z)) (=° x 1) (=° z 2))))
    ; an absento handed on to a variable with a type tag becomes a disequality
    (⊦= '((λ (α) (begin (deny (equal? α 'a))) (assert (every (μ v (symbol? v)) (list α))) α))
        (°->list #f (fresh° (q) (fresh° (x) (absent° 'a x) (symbol° q) (=° x q)))))
    (⊦= '((λ (α) (begin (deny (equal? α 'a))) (assert (every (μ v (symbol? v)) (list α))) α))
        (°->list #f (fresh° (q) (fresh° (x) (absent° 'a q) (symbol° x) (=° x q))))))

  ((test/absent°/structure _)
    (⊦= '() (°->list #f (fresh° (q) (absent° 'a q) (=° q (vector 'a)))))
    (⊦= '() (°->list #f (fresh° (q x) (=° q (vector x)) (absent° 'a q) (=° x 'a))))
    (⊦= '((λ () (vector 'b))) (°->list #f (fresh° (q) (=° q (vector 'b)) (absent° 'a q))))
    (⊦= '() (°->list #f (fresh° (q) (=° q (make-record-instance 'box 'a)) (absent° 'a q)))))

  ((test/absent°/subsumed _)
    ; a disequality subsumed by any absento of the same variable is dropped, whatever the order
    (⊦= (°->list #f (fresh° (q) (absent° 'a q) (absent° 'b q)))
        (°->list #f (fresh° (q) (absent° 'a q) (absent° 'b q) (≠° q 'a))))
    (⊦= (°->list #f (fresh° (q) (absent° 'a q) (absent° 'b q)))
        (°->list #f (fresh° (q) (absent° 'a q) (absent° 'b q) (≠° q 'b)))))

  ((test/absent°/datum _)
    ; any datum is a valid tag, as for absent?
    (⊦= '((λ () (cons 1 (cons 2 '())))) (°->list #f (fresh° (q) (=° q '(1 2)) (absent° 5 q))))
    (⊦= '() (°->list #f (fresh° (q) (=° q '(1 2)) (absent° 2 q))))
    (⊦= '() (°->list #f (fresh° (q) (absent° 5 q) (=° q '(1 5))))))

  ((test/absent°/compound _)
    ; a compound tag is compared with every node of the term, not only with its leaves, as for absent?
    (⊦= '() (°->list #f (fresh° (q) (absent° '(1 2) q) (=° q '(1 2)))))
    (⊦= '() (°->list #f (fresh° (q) (=° q '(1 2)) (absent° '(1 2) q))))
    (⊦= '() (°->list #f (fresh° (q) (=° q '(0 (1 2) 3)) (absent° '(1 2) q))))
    (⊦= '() (°->list #f (fresh° (q) (=° q '(0 1 2)) (absent° '(1 2) q))))
    (⊦= '((λ () (cons 0 (cons 1 (cons 2 (cons 3 '())))))) (°->list #f (fresh° (q) (=° q '(0 1 2 3)) (absent° '(1 2) q))))
    (⊦= '() (°->list #f (fresh° (q) (=° q (list (vector 1 2))) (absent° (vector 1 2) q))))
    ; a structure is walked slot by slot: its slot list is not a term, nor are its tails
    (⊦= '((λ () (vector 0 1 2))) (°->list #f (fresh° (q) (=° q (vector 0 1 2)) (absent° '(1 2) q))))
    (⊦= '((λ () (vector 1))) (°->list #f (fresh° (q) (=° q (vector 1)) (absent° '() q))))
    ; a term that may still become the tag is watched whole, whatever the order of the goals
    (⊦= '() (°->list #f (fresh° (q x) (=° q (cons 1 x)) (absent° '(1 2) q) (=° x '(2)))))
    (⊦= '() (°->list #f (fresh° (q x) (absent° '(1 2) q) (=° q (cons 1 x)) (=° x '(2)))))
    (⊦= '() (°->list #f (fresh° (q x) (absent° '(1 2) q) (=° q (list 0 x)) (=° x '(1 2)))))
    (⊦= '((λ () (cons 1 (cons 3 '())))) (°->list #f (fresh° (q x) (=° q (cons 1 x)) (absent° '(1 2) q) (=° x '(3)))))
    (⊦= '((λ (α) (assert (absent? '(1 2) (cons 1 α))) (cons 1 α))) (°->list #f (fresh° (q x) (=° q (cons 1 x)) (absent° '(1 2) q))))
    ; a watched term holding a variable hidden from the answer still constrains the answer's variables through its parts
    (⊦= '((λ (α) (assert (absent? '(1 2) α)) α)) (°->list #f (fresh° (q) (fresh° (z) (absent° '(1 2) (cons q z))))))
    (⊦= '((λ (α) (assert (absent? '(1 2) α)) α)) (°->list #f (fresh° (q) (fresh° (z) (absent° '(1 2) (list q z))))))
    (⊦= '((λ (α) (assert (absent? '(()) α)) α)) (°->list #f (fresh° (q) (fresh° (z) (absent° '(()) (cons q z))))))
    (⊦= '((λ (α) (assert (absent? '(1 2) (cons 1 α))) α)) (°->list #f (fresh° (q) (fresh° (z) (absent° '(1 2) (list (cons 1 q) z))))))
    (⊦= '((λ (α) (assert (absent? '(1 2) α)) α)) (°->list #f (fresh° (q) (fresh° (z) (absent° '(1 2) (vector z q))))))
    (⊦= '((λ (α) α)) (°->list #f (fresh° (q) (fresh° (z) (absent° '(1 2) (cons z z))))))
    (⊦= '() (°->list #f (fresh° (q) (fresh° (z) (absent° '(1 2) (cons q z)) (=° q '(1 2)))))))

)

(unittest/✓ microkanren-suite)