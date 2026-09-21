
(import 
  scheme (chicken base) (chicken memory representation) (chicken sort)
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

  ((test/rem-subsumed/by _)
   (⊦= '(2 3 1) ((μkanren-rem-subsumed/by member?) '(1 2 1 3 2)))
   (⊦= '() ((μkanren-rem-subsumed/by member?) '())))

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
         (°->list/ground (fresh° r (n q x)
                           (=° q 'z)
                           (fresh° (w r) (=° n (list w q)) (=° x (list w r)))))))

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
    (⊦= '((person alice 30)) (°->list/ground (fresh° r (t n a) (=° `#(,t ,n ,a) p)))))

  ((test/=°/structure/vector/answer _)
    (⊦= '((λ () (vector 1 2))) (°->list #f (fresh° (q) (=° q (vector 1 2)))))
    (⊦= '((λ (α) (vector 1 α))) (°->list #f (fresh° (q r) (=° q (vector 1 r))))))

  ((test/var->symbol/overflow _)
    (⊦= '((α β γ δ ε ζ η θ ι κ λ μ ν ξ ο π ρ σ τ υ φ χ ψ ω α1 β1))
        (°->list/ground (fresh° r (a b c d e f g h i j k l m n o p q s t u v w x y z aa)))))

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
        (°->list #f (fresh° (q) (symbol° q) (absent° 'closure q)))))

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

)

(unittest/✓ microkanren-suite)