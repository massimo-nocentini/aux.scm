
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
  
  ((test/project° _)
    (⊦= '(4700 6500 5000 5700 5700 4400 4000 5500 5300 5300)
        (°->list/ground (fresh° (r) (fresh° (d e s) (empsalary° d e s) (project° ((s* s)) (=° r (+ s* 500))))))))

  ((test/groupby°/empty _)
   (⊦= '(47100) (°->list/ground (fresh° (r) (fresh° (d e s) (groupby° (((s* foldr/add) s)) over () from (empsalary° d e s) => (=° r s*))))))
   `(doc (p "The following table has been kept from the PostgreSQL documentation example on " (i "window functions "  )
            (cite/a "https://www.postgresql.org/docs/current/tutorial-window.html" "PostgreSQL Window Functions")
            ":"
            (code/pre #<<END
  depname  | empno | salary |          avg
-----------+-------+--------+-----------------------
 develop   |    11 |   5200 | 5020.0000000000000000
 develop   |     7 |   4200 | 5020.0000000000000000
 develop   |     9 |   4500 | 5020.0000000000000000
 develop   |     8 |   6000 | 5020.0000000000000000
 develop   |    10 |   5200 | 5020.0000000000000000
 personnel |     5 |   3500 | 3700.0000000000000000
 personnel |     2 |   3900 | 3700.0000000000000000
 sales     |     3 |   4800 | 4866.6666666666666667
 sales     |     1 |   5000 | 4866.6666666666666667
 sales     |     4 |   4800 | 4866.6666666666666667
(10 rows)
END
)
   "with respect to the following query:"
   (code/lang pgsql "SELECT depname, empno, salary, avg(salary) OVER (PARTITION BY depname) FROM empsalary;" )
   )))

  ((test/groupby°/one-column _)
    (⊦= '((personnel 7400) (sales 14600) (develop 25100))
      (sort
        (°->list/ground (fresh° (r) (fresh° (d e s) (groupby° (((s* foldr/add) s)) over (d) from (empsalary° d e s) => (=° r `(,d ,s*))))))
        (λ (a b) (< (cadr a) (cadr b))))))
  
  ((test/set° _)
    (⊦= '((personnel 2) (sales 3) (develop 5))
      (sort
        (°->list/ground (fresh° (r) (fresh° (d e s) (set° (c (λ (k v) (add1 v)) 0) over ((d* d)) from (empsalary° d e s) => (=° r `(,d* ,c))))))
        (λ (a b) (< (cadr a) (cadr b))))))

  ((test/enumerate° _)
    (⊦= '(((0 (sales))
            (1 (sales))
            (2 (sales))
            (3 (personnel))
            (4 (personnel))
            (5 (develop))
            (6 (develop))
            (7 (develop))
            (8 (develop))
            (9 (develop))))
        (°->list/ground (fresh° (r) (fresh° (d e s) (enumerate° (c (λ (i k) (list i (list k)))) over (d) from (empsalary° d e s) => (=° r c)))))))

  ((test/window° _)
   
   (⊦= '((develop 7 4200 5020)
           (develop 8 6000 5020)
           (develop 9 4500 5020)
           (develop 10 5200 5020)
           (develop 11 5200 5020)
           (personnel 2 3900 3700)
           (personnel 5 3500 3700)
           (sales 1 5000 14600/3)
           (sales 3 4800 14600/3)
           (sales 4 4800 14600/3))
      (°->list/ground 
        (fresh° (r) 
          (fresh° (d e s) 
            (window° (((s* foldr/avg) s)) over (d) from (empsalary° d e s) 
              => (=° r `(,d ,e ,s ,s*)))))))

    (⊦= '((develop 7 4200 4710)
           (develop 8 6000 4710)
           (develop 9 4500 4710)
           (develop 10 5200 4710)
           (develop 11 5200 4710)
           (personnel 2 3900 4710)
           (personnel 5 3500 4710)
           (sales 1 5000 4710)
           (sales 3 4800 4710)
           (sales 4 4800 4710))
      (°->list/ground (fresh° (r) (fresh° (d e s) (window° (((s* foldr/avg) s)) over () from (empsalary° d e s) => (=° r `(,d ,e ,s ,s*))))))))

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
    (⊦= '((λ (α β γ) (begin (deny (equal? α 5))) (begin (deny (equal? α 6))) (begin (deny (equal? β 2)) (deny (equal? γ 1))) (cons α (cons β (cons γ (quote ())))))) (°->list #f (fresh° (q x y z) (≠° 5 x) (≠° 6 x) (≠° (list y 1) (list 2 z)) (=° (list x y z) q))))
    (⊦= '((λ (α) (begin (deny (equal? α 1))) α)) (°->list #f (fresh° (s) (≠° s 1))))
    (⊦= '() (°->list #f (fresh° (s) (≠° s 1) (=° s 1))))
    (⊦= '((λ (α) (begin (deny (equal? α (cons 'a (cons 'b '()))))) α)) (°->list #f (fresh° (s) (≠° s '(a b)))))
    (⊦= '((λ (α β) (begin (deny (equal? α 1)) (deny (equal? β 2))) (cons α (cons β (quote ())))))
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
    (⊦= '((a b c)) (μkanren-run (q -1 #t) (fresh° (x out) (rember° x '(a b c) out) (=° (list x out) q))))
  )

  ((test/absent° _)
    (⊦= '((λ (α β) (assert (absent? (quote panda) α)) (assert (absent? (quote panda) β)) (cons (quote jackal) (cons (cons α (cons (quote leopard) (cons β (quote ())))) (quote ())))))
        (°->list #f (fresh° (q x y) (=° `(jackal (,y leopard ,x)) q) (absent° 'panda q))))
    
  )

)

(unittest/✓ microkanren-suite)