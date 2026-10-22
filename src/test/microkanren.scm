
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

; a third type tag, defined as a user would, through the exported API.
(define-μkanren-tag (string-tag v) (string? v))
(define string° (μkanren-make-tag-A string-tag))

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

  ((test/≠°/hidden _)
    ; a disequality holding a variable hidden from the answer is dropped from it, also when it holds that variable
    ; through another, bound after or before it was posted
    (⊦= '((λ (α) α)) (°->list #f (fresh° (q) (fresh° (e f) (≠° q (list e)) (=° e (cons 1 f))))))
    (⊦= '((λ (α) α)) (°->list #f (fresh° (q) (fresh° (e f) (=° e (cons 1 f)) (≠° q (list e))))))
    (⊦= '((λ (α) (begin (deny (equal? α (cons (cons 1 2) '())))) α))
        (°->list #f (fresh° (q) (fresh° (e f) (≠° q (list e)) (=° e (cons 1 f)) (=° f 2))))))

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

  ((test/=°/occurs _)
    ; the occurs check walks a value through S, the slots of a vector or a record and chains of variables, whether
    ; the bound variable is older or newer than those that S holds, in the window of M or below it; a part of a
    ; value of S is bound without the check only to a variable that no value of S holds.
    (define-record box v)
    (⊦= '() (°->list #f (fresh° (q x y) (=° y (list x)) (=° x (vector y)) (=° q 'bad))))
    (⊦= '() (°->list #f (fresh° (q x y) (=° y (list x)) (=° x (make-box y)) (=° q 'bad))))
    (⊦= '() (°->list #f (fresh° (q x y) (=° x (list y)) (=° y x) (=° q 'bad))))
    (⊦= '() (°->list #f (fresh° (q x y z) (=° x (list z)) (=° z y) (=° y x) (=° q 'bad))))
    (⊦= '() (°->list #f (fresh° (q x a b c d e) (=° x (list a b c d e)) (=° a x) (=° q 'bad))))
    (⊦= '() (°->list #f (fresh° (q x a b c d e) (=° x (list a b c d e)) (=° e x) (=° q 'bad))))
    (⊦= '() (°->list #f (fresh° (q x y l d) (=° l (list 1 2 x)) (=° l (cons y d)) (=° x d) (=° q 'bad))))
    (⊦= '() (°->list #f (fresh° (q l a d) (=° l (list 1 a)) (=° l (cons 1 d)) (=° a d) (=° q 'bad))))
    (⊦= '() (°->list #f (fresh° (q x) (=° x (list x)) (=° q 'bad))))
    (⊦= '((λ () (cons 2 (cons 3 '())))) (°->list #f (fresh° (q l a) (=° l '(1 2 3)) (=° l (cons a q)))))
    (⊦= '((λ (α) (cons α '()))) (°->list #f (fresh° (q x a d) (=° x (vector a (list d))) (=° x (vector a q))))))

  ((test/=°/occurs/state _)
    ; a substitution given from outside, here `_1 ↦ (_0)` and `_0 ↦ _2`, tells nothing of the variables its values
    ; hold, so every binding is checked.
    (let* ((v0 (make-μkanren-var 0)) (v1 (make-μkanren-var 1))
           (S (cons/sbral (list v0) (cons/sbral (make-μkanren-var 2) empty/sbral)))
           (s (make-μkanren-state 2 S '() '() '() '())))
      (⊦= '() (§->list ((=° v0 v1) s)))
      (⊦= '() (§->list ((=° v0 v1) (μkanren-state-with μkanren-state-empty vars-count: 2 S: S))))
      (⊦= 1 (length (§->list ((=° v0 'a) s))))))

  ((test/andª/chain _)
    ; a chain of andª nested to the left, as fold builds it, has the stream of the nest of append-map§, promise by
    ; promise, also when its inner stream has many answers, and when the chain or a prefix of it is applied to
    ; many states or extended in two ways.
    (define ((andª/plain f g) s) (append-map§ g (δ (f s))))
    (define-relation (nat° n) (or° (=° n 'z) (fresh° (m) (=° n (list 's m)) (nat° m))))
    (define (steps g k)
      (let L (($ ((fresh° (q) (g q)) μkanren-state-empty)) (k k))
        (cond
          ((zero? k) '())
          ((null? $) '(end))
          ((promise? $) (cons '• (L (force $) (sub1 k))))
          (else (cons (μkanren-state-find/value (make-μkanren-var 0) (car $)) (L (cdr $) (sub1 k)))))))
    (define (chain and f gs) (fold (λ (g f) (and f g)) f gs))
    (define (gs q) (list (or° (=° q 'z) (nat° q)) (or° (nat° q) (=° q '(s z))) ✓° (or° ✓° (nat° q))))
    (⊦= (steps (λ (q) (chain andª/plain (nat° q) (gs q))) 300) (steps (λ (q) (chain andª (nat° q) (gs q))) 300))
    (⊦= (steps (λ (q) (andª/plain (nat° q) (chain andª/plain ✓° (gs q)))) 300)
        (steps (λ (q) (andª (nat° q) (chain andª ✓° (gs q)))) 300))
    (⊦= (steps (λ (q) (let1 (c (chain andª/plain (nat° q) (gs q))) (orª (andª/plain c (=° q 'z)) (andª/plain c ✓°)))) 300)
        (steps (λ (q) (let1 (c (chain andª (nat° q) (gs q))) (orª (andª c (=° q 'z)) (andª c ✓°)))) 300))
    ; and a chain of 4000 goals nested to the left, each binding a fresh variable, runs; its cost is left to the benchmarks.
    (⊦= '((λ () 'ok)) (°->list #f (fresh° (q) (fold (λ (i g) (and° g (fresh° (v) (=° v i)))) (=° q 'ok) (iota 4000))))))

  ((test/var->symbol/overflow _)
    (⊦= '((α β γ δ ε ζ η θ ι κ ν ξ ο π ρ σ τ υ φ χ ψ ω α1 β1 γ1 δ1))
        (°->list/ground (fresh° r ← (a b c d e f g h i j k l m n o p q s t u v w x y z aa))))
    ; λ and μ are left out, since the answer binds its variables with λ and asserts their types with μ: an answer
    ; with twelve variables and a type tag applies.
    (⊦= '((0 1 2 3 4 5 6 7 8 9 10 a))
        (map (λ (a) (apply (eval a) (append (iota 11) '(a))))
             (°->list #f (fresh° (q) (fresh° (a b c d e f g h i j k l) (=° q (list a b c d e f g h i j k l)) (symbol° l)))))))

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

  ((test/≠°/subsumed/disjoint _)
    ; two disjoint type tags subsume a prefix of D with a pair between their variables, typed before or after
    ; the disequality, on either side of the pair, through any pair of the prefix, or handed on by =°
    (⊦= '((λ (α β) (assert (every (μ v (symbol? v)) (list α))) (assert (every (μ v (number? v)) (list β))) (cons α (cons β '()))))
        (°->list #f (fresh° (q x y) (=° q (list x y)) (symbol° x) (number° y) (≠° x y))))
    (⊦= '((λ (α β) (assert (every (μ v (symbol? v)) (list α))) (assert (every (μ v (number? v)) (list β))) (cons α (cons β '()))))
        (°->list #f (fresh° (q x y) (=° q (list x y)) (≠° y x) (number° y) (symbol° x))))
    (⊦= '((λ (α β γ δ) (assert (every (μ v (symbol? v)) (list α))) (assert (every (μ v (number? v)) (list β))) (cons α (cons β (cons γ (cons δ '()))))))
        (°->list #f (fresh° (q x y z w) (=° q (list x y z w)) (symbol° x) (number° y) (≠° (list z x) (list w y)))))
    (⊦= '((λ (α β) (assert (every (μ v (symbol? v)) (list α))) (assert (every (μ v (number? v)) (list β))) (cons α (cons β '()))))
        (°->list #f (fresh° (q x y) (fresh° (z) (=° q (list x y)) (≠° x z) (symbol° x) (number° z) (=° y z)))))
    ; and so they do when the prefix joins their variables through a third one, which no pair of it relates to
    ; both, also next to the prefix of the pair that subsumes it
    (⊦= '((λ (α β γ) (assert (every (μ v (symbol? v)) (list α))) (assert (every (μ v (number? v)) (list β))) (cons α (cons β (cons γ '())))))
        (°->list #f (fresh° (q x y z) (=° q (list x y z)) (symbol° x) (number° y) (≠° (list x y) (list z z)))))
    (⊦= '((λ (α β γ) (assert (every (μ v (symbol? v)) (list α))) (assert (every (μ v (number? v)) (list β))) (cons α (cons β (cons γ '())))))
        (°->list #f (fresh° (q x y z) (=° q (list x y z)) (symbol° x) (number° y) (≠° x y) (≠° (list x y) (list z z)))))
    ; a tag of the user's under the name of a library tag is that tag, as `μkanren-post-A` takes it, whichever
    ; of the two the variable took first
    (let ((sym° (μkanren-make-tag-A (make-μkanren-tag (μkanren-tag-name μkanren-tag/sym) '(μ v (symbol? v)) symbol?)))
          (answer '((λ (α β) (assert (every (μ v (symbol? v)) (list α))) (assert (every (μ v (number? v)) (list β))) (cons α (cons β '()))))))
      (⊦= answer (°->list #f (fresh° (q x y) (=° q (list x y)) (sym° x) (number° y) (≠° x y))))
      (⊦= answer (°->list #f (fresh° (q x y) (=° q (list x y)) (sym° x) (symbol° x) (number° y) (≠° x y))))
      (⊦= answer (°->list #f (fresh° (q x y) (=° q (list x y)) (symbol° x) (sym° x) (number° y) (≠° x y))))))

  ((test/≠°/subsumed/user _)
    ; the same tag keeps a prefix between its variables, and so does a tag of the user's, even one disjoint from the
    ; others as `define-μkanren-tag` asks, since nothing checks it: string° keeps its deny against symbol° and number°
    (⊦= '((λ (α β) (begin (deny (equal? α β))) (assert (every (μ v (symbol? v)) (list α β))) (cons α (cons β '()))))
        (°->list #f (fresh° (q x y) (=° q (list x y)) (symbol° x) (symbol° y) (≠° x y))))
    (⊦= '((λ (α β) (begin (deny (equal? α β))) (assert (every (μ v (string? v)) (list α))) (assert (every (μ v (symbol? v)) (list β)))
            (cons α (cons β '()))))
        (°->list #f (fresh° (q x y) (=° q (list x y)) (string° x) (symbol° y) (≠° x y))))
    (⊦= '((λ (α β) (begin (deny (equal? α β))) (assert (every (μ v (string? v)) (list α))) (assert (every (μ v (number? v)) (list β)))
            (cons α (cons β '()))))
        (°->list #f (fresh° (q x y) (=° q (list x y)) (string° x) (number° y) (≠° y x)))))

  ((test/stores/order _)
    ; an answer does not depend on the order in which its constraints were posted, nor on what a record holds
    (⊦= (°->list #f (fresh° (q) (absent° 'a q) (absent° '(b) q) (absent° 'c q)))
        (°->list #f (fresh° (q) (absent° 'c q) (absent° '(b) q) (absent° 'a q))))
    (⊦= (°->list #f (fresh° (q) (≠° q (make-record-instance 'box 1)) (≠° q (make-record-instance 'box 2))))
        (°->list #f (fresh° (q) (≠° q (make-record-instance 'box 2)) (≠° q (make-record-instance 'box 1)))))
    (⊦= (°->list #f (fresh° (q x y) (=° q (list x y)) (≠° (list x y) '(1 2)) (≠° (list y x) '(3 4))))
        (°->list #f (fresh° (q x y) (=° q (list x y)) (≠° (list y x) '(3 4)) (≠° (list x y) '(1 2)))))
    ; nor on data that `display` prints alike
    (⊦= (°->list #f (fresh° (q) (≠° q 1) (≠° q "1")))
        (°->list #f (fresh° (q) (≠° q "1") (≠° q 1))))
    (⊦= (°->list #f (fresh° (q) (absent° 'a q) (absent° "a" q) (absent° #\a q)))
        (°->list #f (fresh° (q) (absent° #\a q) (absent° "a" q) (absent° 'a q)))))

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

  ((test/absent°/variable-tag _)
    ; a tag with variables is walked whenever the constraint is posted, so it is the datum it is bound to, before or after
    (⊦= '() (°->list #f (fresh° (q) (fresh° (t) (=° t 'a) (absent° t q) (=° q 'a)))))
    (⊦= '() (°->list #f (fresh° (q) (fresh° (t) (absent° t q) (=° t 'a) (=° q 'a)))))
    (⊦= '() (°->list #f (fresh° (q) (fresh° (t) (=° q '(b a)) (absent° t q) (=° t 'a)))))
    (⊦= '() (°->list #f (fresh° (q) (fresh° (t u) (absent° t q) (=° t u) (=° u 'a) (=° q 'a)))))
    (⊦= '() (°->list #f (fresh° (q) (fresh° (x) (absent° (list 1 x) q) (=° q '(0 1 2)) (=° x 2)))))
    (⊦= (°->list #f (fresh° (q) (absent° 'a q))) (°->list #f (fresh° (q) (fresh° (t) (absent° t q) (=° t 'a)))))
    (⊦= (°->list #f (fresh° (q) (absent° 'a q))) (°->list #f (fresh° (q) (fresh° (t u) (absent° t q) (=° t u) (=° u 'a)))))
    (⊦= (°->list #f (fresh° (q) (absent° '(1 2) q))) (°->list #f (fresh° (q) (fresh° (x) (absent° (list 1 x) q) (=° x 2)))))
    ; the answer is the same in either order once the datum is ground before a compound node of the term is taken
    ; apart, as an atomic datum always is; otherwise that node is printed as the disequalities of its nodes
    (⊦= (°->list #f (fresh° (q) (fresh° (r t) (=° t 'a) (absent° t q) (=° q (cons 1 r)))))
        (°->list #f (fresh° (q) (fresh° (r t) (absent° t q) (=° q (cons 1 r)) (=° t 'a)))))
    (⊦= '((λ (α) (assert (absent? '(1 . 5) (cons 1 α))) (cons 1 α)))
        (°->list #f (fresh° (q) (fresh° (r t) (=° t '(1 . 5)) (absent° t q) (=° q (cons 1 r))))))
    (⊦= '((λ (α) (begin (deny (equal? α 5))) (assert (absent? '(1 . 5) α)) (cons 1 α)))
        (°->list #f (fresh° (q) (fresh° (r t) (absent° t q) (=° q (cons 1 r)) (=° t '(1 . 5))))))
    ; unbound, it differs from every node of the term, as faster-miniKanren's absento has it
    (⊦= '() (°->list #f (fresh° (q t) (absent° t q) (=° q t))))
    (⊦= '() (°->list #f (fresh° (q) (fresh° (t u) (absent° t u) (=° u t)))))
    (⊦= '() (°->list #f (fresh° (q t) (absent° t q) (=° q (list 1 t)))))
    (⊦= '() (°->list #f (fresh° (q) (fresh° (x) (absent° x q) (=° q (/ 0. 0.)) (=° x (/ 0. 0.))))))
    (⊦= '((λ (α β) (assert (absent? α β)) (cons α (cons β '()))))
        (°->list #f (fresh° (q t u) (=° q (list t u)) (absent° t u))))
    (⊦= '((λ (α β) (assert (absent? (cons 1 (cons α '())) β)) (cons α (cons β '()))))
        (°->list #f (fresh° (q x u) (=° q (list x u)) (absent° (list 1 x) u))))
    (⊦= '((λ (α) (begin (deny (equal? α 1))) (begin (deny (equal? α 2))) (begin (deny (equal? α (cons 1 2)))) (cons α (cons (cons 1 2) '()))))
        (°->list #f (fresh° (q t u) (=° q (list t u)) (absent° t u) (=° u '(1 . 2)))))
    ; it is printed after a ground datum of the same variable, even one that prints alike, in either order
    (⊦= '((λ (α β) (assert (absent? 'β α)) (assert (absent? β α)) (cons α (cons β '()))))
        (°->list #f (fresh° (q x y) (=° q (list x y)) (absent° 'β x) (absent° y x))))
    (⊦= (°->list #f (fresh° (q x y) (=° q (list x y)) (absent° 'β x) (absent° y x)))
        (°->list #f (fresh° (q x y) (=° q (list x y)) (absent° y x) (absent° 'β x))))
    (⊦= (°->list #f (fresh° (q x y) (=° q (list x y)) (absent° '(β) x) (absent° (list y) x)))
        (°->list #f (fresh° (q x y) (=° q (list x y)) (absent° (list y) x) (absent° '(β) x))))
    ; the answer runs: its clause evaluates the datum
    (let1 (answer (eval (car (°->list #f (fresh° (q t u) (=° q (list t u)) (absent° t u))))))
      (⊦= '(a (b c)) (answer 'a '(b c)))
      (⊦= 'failed (condition-case (answer 'a '(b a)) (_ () 'failed))))
    ; on a variable with a type tag it is a disequality, in either order and through =° (I3)
    (⊦= '((λ (α β) (begin (deny (equal? α β))) (assert (every (μ v (symbol? v)) (list β))) (cons α (cons β '()))))
        (°->list #f (fresh° (q t u) (=° q (list t u)) (absent° t u) (symbol° u))))
    (⊦= '((λ (α β) (begin (deny (equal? α β))) (assert (every (μ v (symbol? v)) (list β))) (cons α (cons β '()))))
        (°->list #f (fresh° (q t u) (=° q (list t u)) (symbol° u) (absent° t u))))
    (⊦= '((λ (α β) (begin (deny (equal? α β))) (assert (every (μ v (symbol? v)) (list β))) (cons α (cons β '()))))
        (°->list #f (fresh° (q t u) (=° q (list t u)) (fresh° (x) (absent° t x) (symbol° u) (=° x u)))))
    (⊦= '((λ (α) (begin (deny (equal? α 'a))) (assert (every (μ v (symbol? v)) (list α))) (cons 'a (cons α '()))))
        (°->list #f (fresh° (q t u) (=° q (list t u)) (fresh° (x) (absent° x u) (symbol° u) (=° x t) (=° t 'a)))))
    ; a datum holding the term can never occur in it, and one holding a hidden variable can always be kept out of it,
    ; unless the term holds that variable too
    (⊦= '((λ (α) α)) (°->list #f (fresh° (q) (absent° (list q) q))))
    (⊦= '((λ (α) α)) (°->list #f (fresh° (q) (fresh° (t) (absent° t q)))))
    (⊦= '((λ (α) α)) (°->list #f (fresh° (q) (fresh° (h) (absent° (list h) q)))))
    (⊦= '((λ (α) (begin (deny (equal? α 1))) α)) (°->list #f (fresh° (q) (fresh° (h) (absent° (cons 1 h) (cons q h))))))
    ; of the disequalities it posts on the nodes of the term, those holding a hidden variable, here through e, are dropped
    (⊦= '((λ (α) (begin (deny (equal? α 1))) (begin (deny (equal? α '()))) α))
        (°->list #f (fresh° (q) (fresh° (e f) (=° e (cons 1 f)) (absent° q (list e)))))))

  ((test/string° _)
    ; a third type tag, alone, twice, and at reification
    (⊦= '((λ (α) (assert (every (μ v (string? v)) (list α))) α)) (°->list #f (fresh° (q) (string° q))))
    (⊦= (°->list #f (fresh° (q) (string° q))) (°->list #f (fresh° (q) (string° q) (string° q))))
    (⊦= '(α) (°->list/ground (fresh° (q) (string° q))))
    ; disjoint from symbol° and number°, in either order, and from the values of other types
    (⊦= '() (°->list #f (fresh° (q) (string° q) (symbol° q))))
    (⊦= '() (°->list #f (fresh° (q) (symbol° q) (string° q))))
    (⊦= '() (°->list #f (fresh° (q) (number° q) (string° q))))
    (⊦= '() (°->list #f (fresh° (q) (string° q) (=° q 'a))))
    (⊦= '() (°->list #f (fresh° (q) (=° q 5) (string° q))))
    (⊦= '((λ () "a")) (°->list #f (fresh° (q) (string° q) (=° q "a"))))
    (⊦= '((λ () "a")) (°->list #f (fresh° (q) (=° q "a") (string° q))))
    ; on a compound or a bound term
    (⊦= '() (°->list #f (fresh° (q) (string° (list q)))))
    (⊦= '((λ () (cons "b" '()))) (°->list #f (fresh° (q) (fresh° (x) (=° q (list x)) (string° x) (=° x "b")))))
    (⊦= '() (°->list #f (fresh° (q) (fresh° (x) (=° q (list x)) (string° x) (=° x 1)))))
    ; with the two built-in tags: one clause per tag
    (⊦= '((λ (α β γ) (assert (every (μ v (string? v)) (list α))) (assert (every (μ v (symbol? v)) (list β))) (assert (every (μ v (number? v)) (list γ)))
            (cons α (cons β (cons γ '())))))
        (°->list #f (fresh° (q x y z) (=° q (list x y z)) (number° z) (symbol° y) (string° x))))
    ; ≠°: a disequality that the tag implies is dropped (I4), in either order, and one that it does not is kept
    (⊦= '((λ (α) (assert (every (μ v (string? v)) (list α))) α)) (°->list #f (fresh° (q) (string° q) (≠° q 'a))))
    (⊦= '((λ (α) (assert (every (μ v (string? v)) (list α))) α)) (°->list #f (fresh° (q) (≠° q 'a) (string° q))))
    (⊦= '((λ (α) (begin (deny (equal? α "a"))) (assert (every (μ v (string? v)) (list α))) α)) (°->list #f (fresh° (q) (≠° q "a") (string° q))))
    (⊦= '() (°->list #f (fresh° (q) (string° q) (≠° q "a") (=° q "a"))))
    ; absent°: on a string variable the absento becomes a disequality (I3), which the tag drops if it implies it
    (⊦= '((λ (α) (begin (deny (equal? α "a"))) (assert (every (μ v (string? v)) (list α))) α)) (°->list #f (fresh° (q) (absent° "a" q) (string° q))))
    (⊦= '((λ (α) (begin (deny (equal? α "a"))) (assert (every (μ v (string? v)) (list α))) α)) (°->list #f (fresh° (q) (string° q) (absent° "a" q))))
    (⊦= '((λ (α) (assert (every (μ v (string? v)) (list α))) α)) (°->list #f (fresh° (q) (absent° 'a q) (string° q))))
    (⊦= '((λ (α) (assert (every (μ v (string? v)) (list α))) α)) (°->list #f (fresh° (q) (absent° '(1) q) (string° q))))
    (⊦= '() (°->list #f (fresh° (q) (string° q) (absent° "a" q) (=° q "a"))))
    (⊦= '() (°->list #f (fresh° (q) (fresh° (x) (string° x) (absent° "a" q) (=° q (list 1 x)) (=° x "a")))))
    (⊦= '((λ () (cons 1 (cons "b" '())))) (°->list #f (fresh° (q) (fresh° (x) (string° x) (absent° "a" q) (=° q (list 1 x)) (=° x "b")))))
    ; =° hands it on, and the answer drops it on a hidden variable
    (⊦= '() (°->list #f (fresh° (q) (fresh° (x y) (string° x) (=° x y) (symbol° y)))))
    (⊦= '() (°->list #f (fresh° (q) (fresh° (x y) (string° x) (symbol° y) (=° x y)))))
    (⊦= '((λ (α) (assert (every (μ v (string? v)) (list α))) α)) (°->list #f (fresh° (q) (fresh° (x) (string° x) (=° x q)))))
    (⊦= '((λ (α) α)) (°->list #f (fresh° (q) (fresh° (x) (string° x)))))
    ; the answer runs: its clause holds on a string and fails on a symbol
    (let1 (answer (eval (car (°->list #f (fresh° (q) (string° q))))))
      (⊦= "zz" (answer "zz"))
      (⊦= 'failed (condition-case (answer 'zz) (_ () 'failed)))))

)

(unittest/✓ microkanren-suite)