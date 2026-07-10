
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

  

)

(unittest/✓ microkanren-suite)