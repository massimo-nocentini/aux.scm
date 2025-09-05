
(import (aux unittest) (aux base) (aux stream) (aux kanren micro) (fds sbral))

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
   (define-relation (peano° n)
     (or°
       (=° n 'z)
       (fresh° (r) (=° n `(s ,r)) (peano° r))))
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
         (§->list (take§ 10 (°->§ (n) (peano° n))))))


  )

(unittest/✓ microkanren-suite)