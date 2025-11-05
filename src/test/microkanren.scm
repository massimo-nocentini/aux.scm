
(import (aux unittest) (aux base) (aux stream) (aux kanren micro) (aux fds sbral))

; The following is a sample database of employees and their salaries in various departments.
; taken from https://www.postgresql.org/docs/current/tutorial-window.html.
(define-relation (empsalary° depname empno salary)
  (or° (and° (=° depname 'develop) (=° empno 7)  (=° salary 4200))
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
   (let* ((v1 (µkanren-var 0))
          (v2 (µkanren-var 1))
          (s0 µkanren-state-empty)
          (s1 (list (cons/sbral 'a (µkanren-state-substitution s0)) 1))
          (s2 (list (cons/sbral v1 (µkanren-state-substitution s1)) 2)))
     (⊦= 5 (µkanren-state-find 5 s2))
     (⊦= 'a (µkanren-state-find v1 s2))
     (⊦= 'a (µkanren-state-find v2 s2))))

  ((test/=° _) (⊦= '((λ () `#t)) (§->list (°->§ (=° 'z 'z)))))

  ((test/sharing _)
   (⊦= '((λ (_0 _1) `((,_0 z) z (,_0 ,_1))))
         (§->list (°->§ (fresh° r (n q x)
                           (=° q 'z)
                           (fresh° (w r) (=° n (list w q)) (=° x (list w r))))))))

  ((test/peano° _)
   (define-relation (peano° n) (or° (=° n 'z) (fresh° (r) (=° n `(s ,r)) (peano° r))))
   (define-relation (church° n) (fresh° (b) (=° n `(λ (s) (λ (z) ,b))) (peano° b)))
   (⊦= '((λ () `z)
           (λ () `(s z))
           (λ () `(s (s z)))
           (λ () `(s (s (s z))))
           (λ () `(s (s (s (s z)))))
           (λ () `(s (s (s (s (s z))))))
           (λ () `(s (s (s (s (s (s z)))))))
           (λ () `(s (s (s (s (s (s (s z))))))))
           (λ () `(s (s (s (s (s (s (s (s z)))))))))
           (λ () `(s (s (s (s (s (s (s (s (s z)))))))))))
         (§->list (take§ 10 (°->§ (fresh° (n) (peano° n))))))

   (⊦= '((λ () `(λ (s) (λ (z) z)))
           (λ () `(λ (s) (λ (z) (s z))))
           (λ () `(λ (s) (λ (z) (s (s z)))))
           (λ () `(λ (s) (λ (z) (s (s (s z))))))
           (λ () `(λ (s) (λ (z) (s (s (s (s z)))))))
           (λ () `(λ (s) (λ (z) (s (s (s (s (s z))))))))
           (λ () `(λ (s) (λ (z) (s (s (s (s (s (s z)))))))))
           (λ () `(λ (s) (λ (z) (s (s (s (s (s (s (s z))))))))))
           (λ () `(λ (s) (λ (z) (s (s (s (s (s (s (s (s z)))))))))))
           (λ () `(λ (s) (λ (z) (s (s (s (s (s (s (s (s (s z)))))))))))))
         (§->list (take§ 10 (°->§ (fresh° (n) (church° n)))))))

  ((test/append° _)
   (define-relation (append° r s rs)
     (cond°
       ((null° r) (=° s rs))
       ((fresh° (a d c) (cons° a d r) (append° d s c) (cons° a c rs)))))
   (⊦= '((λ (_0) `,_0)
           (λ (_0 _1) `(,_0 unquote _1))
           (λ (_0 _1 _2) `(,_0 ,_1 unquote _2))
           (λ (_0 _1 _2 _3) `(,_0 ,_1 ,_2 unquote _3))
           (λ (_0 _1 _2 _3 _4) `(,_0 ,_1 ,_2 ,_3 unquote _4))
           (λ (_0 _1 _2 _3 _4 _5) `(,_0 ,_1 ,_2 ,_3 ,_4 unquote _5))
           (λ (_0 _1 _2 _3 _4 _5 _6) `(,_0 ,_1 ,_2 ,_3 ,_4 ,_5 unquote _6))
           (λ (_0 _1 _2 _3 _4 _5 _6 _7)
               `(,_0 ,_1 ,_2 ,_3 ,_4 ,_5 ,_6 unquote _7))
           (λ (_0 _1 _2 _3 _4 _5 _6 _7 _8)
               `(,_0 ,_1 ,_2 ,_3 ,_4 ,_5 ,_6 ,_7 unquote _8))
           (λ (_0 _1 _2 _3 _4 _5 _6 _7 _8 _9)
               `(,_0 ,_1 ,_2 ,_3 ,_4 ,_5 ,_6 ,_7 ,_8 unquote _9)))
         (§->list (take§ 10 (°->§ (fresh° (l) (fresh° (a d) (append° a d l))))))))

  ((test/project° _)
    (⊦= '((λ () `4700)
           (λ () `6500)
           (λ () `5000)
           (λ () `5700)
           (λ () `5700)
           (λ () `4400)
           (λ () `4000)
           (λ () `5500)
           (λ () `5300)
           (λ () `5300))
   (§->list (°->§ (fresh° (r) (fresh° (d e s) (empsalary° d e s) (project° ((s* s)) (=° r (+ s* 500)))))))))

  ((test/groupby°/empty _)
   (⊦= '((λ () `47100)) (§->list (°->§ (fresh° (r) (fresh° (d e s) (groupby° (((s* foldr/add) s)) over () from (empsalary° d e s) => (=° r s*)))))))
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
    (⊦= '((λ () `(develop 25100))
            (λ () `(sales 14600))
            (λ () `(personnel 7400)))
          (§->list (°->§ (fresh° (r) (fresh° (d e s) (groupby° (((s* foldr/add) s)) over (d) from (empsalary° d e s) => (=° r `(,d ,s*)))))))))
  
  ((test/set° _)
    (⊦= '((λ () `(sales 3))
            (λ () `(personnel 2))
            (λ () `(develop 5)))
        (§->list (°->§ (fresh° (r) (fresh° (d e s) (set° (c (λ (k v) (add1 v)) 0) over ((d* d)) from (empsalary° d e s) => (=° r `(,d* ,c)))))))))

  ((test/enumerate° _)
    (⊦= '((λ ()
                `((0 (sales))
                    (1 (sales))
                    (2 (sales))
                    (3 (personnel))
                    (4 (personnel))
                    (5 (develop))
                    (6 (develop))
                    (7 (develop))
                    (8 (develop))
                    (9 (develop)))))
        (§->list (°->§ (fresh° (r) (fresh° (d e s) (enumerate° (c (λ (i k) (list i (list k)))) over (d) from (empsalary° d e s) => (=° r c))))))))

  ((test/window° _)
   
   (⊦= '((λ () `(develop 7 4200 5020))
           (λ () `(develop 8 6000 5020))
           (λ () `(develop 9 4500 5020))
           (λ () `(develop 10 5200 5020))
           (λ () `(develop 11 5200 5020))
           (λ () `(personnel 2 3900 3700))
           (λ () `(personnel 5 3500 3700))
           (λ () `(sales 1 5000 14600/3))
           (λ () `(sales 3 4800 14600/3))
           (λ () `(sales 4 4800 14600/3)))
      (§->list 
        (°->§ (fresh° (r) 
          (fresh° (d e s) 
            (window° (((s* foldr/avg) s)) over (d) from (empsalary° d e s) 
              => (=° r `(,d ,e ,s ,s*))))))))

    (⊦= '((λ () `(develop 7 4200 4710))
           (λ () `(develop 8 6000 4710))
           (λ () `(develop 9 4500 4710))
           (λ () `(develop 10 5200 4710))
           (λ () `(develop 11 5200 4710))
           (λ () `(personnel 2 3900 4710))
           (λ () `(personnel 5 3500 4710))
           (λ () `(sales 1 5000 4710))
           (λ () `(sales 3 4800 4710))
           (λ () `(sales 4 4800 4710)))
      (§->list (°->§ (fresh° (r) (fresh° (d e s) (window° (((s* foldr/avg) s)) over () from (empsalary° d e s) => (=° r `(,d ,e ,s ,s*))))))))
   
   )

  )

(unittest/✓ microkanren-suite)

















