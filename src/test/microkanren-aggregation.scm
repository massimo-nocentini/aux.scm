
(import 
  scheme (chicken base) (chicken memory representation) (chicken sort)
  srfi-1 srfi-69
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

(define-suite microkanren-aggregation-suite


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

  ; the groups come in the order of the first state of their key, here the order of the rows, since each body answers at once.
  ((test/groupby°/one-column _)
    (⊦= '((develop 25100) (personnel 7400) (sales 14600))
        (°->list/ground (fresh° (r) (fresh° (d e s) (groupby° (((s* foldr/add) s)) over (d) from (empsalary° d e s) => (=° r `(,d ,s*))))))))

  ; and the values of a group in the order of its states.
  ((test/groupby°/first-state _)
    (⊦= '((4200 (7)) (6000 (8)) (4500 (9)) (5200 (10 11)) (3900 (2)) (3500 (5)) (5000 (1)) (4800 (3 4)))
        (°->list/ground (fresh° (r) (fresh° (d e s) (groupby° (((es (λ (l) l)) e)) over (s) from (empsalary° d e s) => (=° r `(,s ,es))))))))

  ; the first state of a key, not the last: b, a, b, c, a groups as b, a, c.
  ((test/groupby°/interleaved _)
    (⊦= '((b 2) (a 2) (c 1))
        (°->list/ground (fresh° (r) (fresh° (x) (groupby° (((n length) x)) over (x) from (or° (=° x 'b) (=° x 'a) (=° x 'b) (=° x 'c) (=° x 'a)) => (=° r `(,x ,n))))))))

  ; srfi-69 salts each table with a draw of rand(), so the tables built before must not move the order.
  ((test/groupby°/salt _)
    (let1 (answers (map (λ (n)
                          (for-each (λ (i) (make-hash-table)) (iota n))
                          (°->list/ground (fresh° (r) (fresh° (d e s) (groupby° (((es (λ (l) l)) e)) over (s) from (empsalary° d e s) => (=° r `(,s ,es)))))))
                        (iota 4)))
      (⊨ (every (λ (a) (equal? a (car answers))) answers))))

  ; a variable aggregated twice gives each aggregation the values of its own column, once each.
  ((test/groupby°/same-column _)
    (⊦= '((develop 25100 5) (personnel 7400 2) (sales 14600 3))
        (°->list/ground (fresh° (r) (fresh° (d e s) (groupby° (((s* foldr/add) s) ((n length) s)) over (d) from (empsalary° d e s) => (=° r `(,d ,s* ,n))))))))

  ; the aggregated value can be a term, not only a variable.
  ((test/groupby°/term _)
    (⊦= '((develop ((7 4200) (8 6000) (9 4500) (10 5200) (11 5200))) (personnel ((2 3900) (5 3500))) (sales ((1 5000) (3 4800) (4 4800))))
        (°->list/ground (fresh° (r) (fresh° (d e s) (groupby° (((es (λ (l) l)) `(,e ,s))) over (d) from (empsalary° d e s) => (=° r `(,d ,es))))))))
  
  ((test/set° _)
    (⊦= '((develop 5) (personnel 2) (sales 3))
        (°->list/ground (fresh° (r) (fresh° (d e s) (set° (c (λ (k v) (add1 v)) 0) over ((d* d)) from (empsalary° d e s) => (=° r `(,d* ,c))))))))

  ((test/set°/interleaved _)
    (⊦= '((b 2) (a 2) (c 1))
        (°->list/ground (fresh° (r) (fresh° (x) (set° (n (λ (k v) (add1 v)) 0) over ((x* x)) from (or° (=° x 'b) (=° x 'a) (=° x 'b) (=° x 'c) (=° x 'a)) => (=° r `(,x* ,n))))))))

  ((test/set°/salt _)
    (let1 (answers (map (λ (n)
                          (for-each (λ (i) (make-hash-table)) (iota n))
                          (°->list/ground (fresh° (r) (fresh° (d e s) (set° (c (λ (k v) (add1 v)) 0) over ((s* s)) from (empsalary° d e s) => (=° r `(,s* ,c)))))))
                        (iota 4)))
      (⊨ (every (λ (a) (equal? a (car answers))) answers))))

  ((test/enumerate° _)
    (⊦= '(((0 (develop))
            (1 (develop))
            (2 (develop))
            (3 (develop))
            (4 (develop))
            (5 (personnel))
            (6 (personnel))
            (7 (sales))
            (8 (sales))
            (9 (sales))))
        (°->list/ground (fresh° (r) (fresh° (d e s) (enumerate° (c (λ (i k) (list i (list k)))) over (d) from (empsalary° d e s) => (=° r c)))))))

  ((test/aggregation/fresh-in-body _)
    ; the keys and values hold the fresh variable x of the from goal, and the body of set°, groupby° and
    ; enumerate°, which runs on the outer state, makes y fresh: y is another variable than x, as in window°.
    (⊦= '((λ (α) (cons (cons α (quote ())) (cons 1 (cons (quote b) (quote ()))))))
        (°->list #f (fresh° (q) (fresh° (k) (set° (n (λ (k acc) (add1 acc)) 0) over ((k* k)) from (fresh° (x) (=° k (list x)))
                                              => (fresh° (y) (=° y 'b) (=° q (list k* n y))))))))
    (⊦= '((λ (α) (cons (cons α (quote ())) (cons (cons α (quote ())) (cons (quote b) (quote ()))))))
        (°->list #f (fresh° (q) (fresh° (k e) (groupby° (((es (λ (l) l)) e)) over (k) from (fresh° (x) (=° k (list x)) (=° e x))
                                                => (fresh° (y) (=° y 'b) (=° q (list k es y))))))))
    (⊦= '((λ (α) (cons (cons α (quote ())) (cons (cons α (quote ())) (cons (quote b) (quote ()))))))
        (°->list #f (fresh° (q) (fresh° (k e) (window° (((es (λ (l) l)) e)) over (k) from (fresh° (x) (=° k (list x)) (=° e x))
                                                => (fresh° (y) (=° y 'b) (=° q (list k es y))))))))
    (⊦= '((λ (α) (cons (cons (cons 0 (cons (cons α (quote ())) (quote ()))) (quote ())) (cons (quote b) (quote ())))))
        (°->list #f (fresh° (q) (fresh° (k) (enumerate° (v (λ (i k) (list i k))) over (k) from (fresh° (x) (=° k (list x)))
                                              => (fresh° (y) (=° y 'b) (=° q (list v y)))))))))

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

  ((test/window°/same-column _)
    (⊦= '((develop 7 25100 5)
           (develop 8 25100 5)
           (develop 9 25100 5)
           (develop 10 25100 5)
           (develop 11 25100 5)
           (personnel 2 7400 2)
           (personnel 5 7400 2)
           (sales 1 14600 3)
           (sales 3 14600 3)
           (sales 4 14600 3))
      (°->list/ground (fresh° (r) (fresh° (d e s) (window° (((s* foldr/add) s) ((n length) s)) over (d) from (empsalary° d e s) => (=° r `(,d ,e ,s* ,n))))))))

  ((test/window°/term _)
    (⊦= '((7 (7 8 9 10 11))
           (8 (7 8 9 10 11))
           (9 (7 8 9 10 11))
           (10 (7 8 9 10 11))
           (11 (7 8 9 10 11))
           (2 (2 5))
           (5 (2 5))
           (1 (1 3 4))
           (3 (1 3 4))
           (4 (1 3 4)))
      (°->list/ground (fresh° (r) (fresh° (d e s) (window° (((es (λ (l) (map car l))) `(,e ,s))) over (d) from (empsalary° d e s) => (=° r `(,e ,es))))))))

)

(unittest/✓ microkanren-aggregation-suite)