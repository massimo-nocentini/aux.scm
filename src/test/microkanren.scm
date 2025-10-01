
(import (aux unittest) (aux base) (aux stream) (aux kanren micro) (aux fds sbral))

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

  ((test/=° _) (⊦= '((λ () `())) (§->list (°->§ () (=° 'z 'z)))))

  ((test/sharing _) 
   (⊦= '((λ (_0 _1) `((,_0 z) z (,_0 ,_1)))) 
         (§->list (°->§ (n q x) 
                           (=° q 'z) 
                           (fresh° (w r) (=° n (list w q)) (=° x (list w r)))))))

  ((test/peano° _)
   (define-relation (peano° n) (or° (=° n 'z) (fresh° (r) (=° n `(s ,r)) (peano° r))))
   (define-relation (church° n) (fresh° (b) (=° n `(λ (s) (λ (z) ,b))) (peano° b)))
   (⊦= '((λ () `(z))
           (λ () `((s z)))
           (λ () `((s (s z))))
           (λ () `((s (s (s z)))))
           (λ () `((s (s (s (s z))))))
           (λ () `((s (s (s (s (s z)))))))
           (λ () `((s (s (s (s (s (s z))))))))
           (λ () `((s (s (s (s (s (s (s z)))))))))
           (λ () `((s (s (s (s (s (s (s (s z))))))))))
           (λ () `((s (s (s (s (s (s (s (s (s z))))))))))))
         (§->list (take§ 10 (°->§ (n) (peano° n)))))

   (⊦= '((λ () `((λ (s) (λ (z) z))))
           (λ () `((λ (s) (λ (z) (s z)))))
           (λ () `((λ (s) (λ (z) (s (s z))))))
           (λ () `((λ (s) (λ (z) (s (s (s z)))))))
           (λ () `((λ (s) (λ (z) (s (s (s (s z))))))))
           (λ () `((λ (s) (λ (z) (s (s (s (s (s z)))))))))
           (λ () `((λ (s) (λ (z) (s (s (s (s (s (s z))))))))))
           (λ () `((λ (s) (λ (z) (s (s (s (s (s (s (s z)))))))))))
           (λ () `((λ (s) (λ (z) (s (s (s (s (s (s (s (s z))))))))))))
           (λ () `((λ (s) (λ (z) (s (s (s (s (s (s (s (s (s z))))))))))))))
         (§->list (take§ 10 (°->§ (n) (church° n))))))

  ((test/append° _)
   (define-relation (append° r s rs)
     (cond°
       ((null° r) (=° s rs))
       ((fresh° (a d c) (cons° a d r) (append° d s c) (cons° a c rs)))))
   (⊦= '((λ (_0) `(,_0))
           (λ (_0 _1) `((,_0 unquote _1)))
           (λ (_0 _1 _2) `((,_0 ,_1 unquote _2)))
           (λ (_0 _1 _2 _3) `((,_0 ,_1 ,_2 unquote _3)))
           (λ (_0 _1 _2 _3 _4) `((,_0 ,_1 ,_2 ,_3 unquote _4)))
           (λ (_0 _1 _2 _3 _4 _5) `((,_0 ,_1 ,_2 ,_3 ,_4 unquote _5)))
           (λ (_0 _1 _2 _3 _4 _5 _6) `((,_0 ,_1 ,_2 ,_3 ,_4 ,_5 unquote _6)))
           (λ (_0 _1 _2 _3 _4 _5 _6 _7)
               `((,_0 ,_1 ,_2 ,_3 ,_4 ,_5 ,_6 unquote _7)))
           (λ (_0 _1 _2 _3 _4 _5 _6 _7 _8)
               `((,_0 ,_1 ,_2 ,_3 ,_4 ,_5 ,_6 ,_7 unquote _8)))
           (λ (_0 _1 _2 _3 _4 _5 _6 _7 _8 _9)
               `((,_0 ,_1 ,_2 ,_3 ,_4 ,_5 ,_6 ,_7 ,_8 unquote _9))))
         (§->list (take§ 10 (°->§ (l) (fresh° (a d) (append° a d l)))))))

  )

(unittest/✓ microkanren-suite)
















