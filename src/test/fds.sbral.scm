
(import (aux unittest) (aux base) (aux fds sbral) (chicken base) srfi-1)

(define-suite sbral-suite

  ((test-cons _)

   (let1 (q empty/sbral) 
         (set! q (cons/sbral 'a q))
         (⊦= '((1 a)) q)
         (⊦= 'a (car/sbral q))
         (⊦= '() (cdr/sbral q))

         (set! q (cons/sbral 'b q))
         (⊦= '((1 b) (1 a)) q)
         (⊦= 'b (car/sbral q))
         (⊦= '((1 a)) (cdr/sbral q))

         (set! q (cons/sbral 'c q))
         (⊦= '((3 c (b) (a))) q)
         (⊦= 'c (car/sbral q))
         (⊦= '((1 b) (1 a)) (cdr/sbral q))

         (set! q (cons/sbral 'd q))
         (⊦= '((1 d) (3 c (b) (a))) q)
         (⊦= 'd (car/sbral q))
         (⊦= '((3 c (b) (a))) (cdr/sbral q))

         (set! q (cons/sbral 'e q))
         (⊦= '((1 e) (1 d) (3 c (b) (a))) q)
         (⊦= 'e (car/sbral q))
         (⊦= '((1 d) (3 c (b) (a))) (cdr/sbral q))

         (set! q (cons/sbral 'f q))
         (⊦= '((3 f (e) (d)) (3 c (b) (a))) q)
         (⊦= 'f (car/sbral q))
         (⊦= '((1 e) (1 d) (3 c (b) (a))) (cdr/sbral q))

         (set! q (cons/sbral 'g q))
         (⊦= '((7 g (f (e) (d)) (c (b) (a)))) q)
         (⊦= 'g (car/sbral q))
         (⊦= '((3 f (e) (d)) (3 c (b) (a))) (cdr/sbral q))))

  ((test-ref _)

   (let1 (q (list->sbral '(a b c d e f g)))
         (⊦= 'a (sbral-ref q 0))
         (⊦= 'b (sbral-ref q 1))
         (⊦= 'c (sbral-ref q 2))
         (⊦= 'd (sbral-ref q 3))
         (⊦= 'e (sbral-ref q 4))
         (⊦= 'f (sbral-ref q 5))
         (⊦= 'g (sbral-ref q 6))
         ))

  ((test-list->sbral _)

   (let* ((l '(a b c d e f g))
          (ll (iota 100)))     
     (⊦= '((7 a (b (c) (d)) (e (f) (g)))) (list->sbral l))
     (⊦= '((3 0 (1) (2))
             (3 3 (4) (5))
             (31
               6
               (7
                 (8 (9 (10) (11)) (12 (13) (14)))
                 (15 (16 (17) (18)) (19 (20) (21))))
               (22
                 (23 (24 (25) (26)) (27 (28) (29)))
                 (30 (31 (32) (33)) (34 (35) (36)))))
             (63
               37
               (38
                 (39
                   (40 (41 (42) (43)) (44 (45) (46)))
                   (47 (48 (49) (50)) (51 (52) (53))))
                 (54
                   (55 (56 (57) (58)) (59 (60) (61)))
                   (62 (63 (64) (65)) (66 (67) (68)))))
               (69
                 (70
                   (71 (72 (73) (74)) (75 (76) (77)))
                   (78 (79 (80) (81)) (82 (83) (84))))
                 (85
                   (86 (87 (88) (89)) (90 (91) (92)))
                   (93 (94 (95) (96)) (97 (98) (99))))))) (list->sbral ll))))


  ((test-length _)

   (let* ((l '(a b c d e f g))
          (q (list->sbral l)))              
     (⊦= (length l) (length/sbral q))))

  ((test-update _)
   (let1 (q (list->sbral '(a b c d e f g)))         
         (⊦= '((7 a (b (c) (d)) (e (f) (g)))) q)
         (⊦= '((7 a (b (c) (d)) (hello (f) (g)))) (update/sbral 4 'hello q))))

  ((test-identity _)
   (let1 (l '(a b c d e f g))
         (⊦= l (sbral->list (list->sbral l)))))

  )

(unittest/✓ sbral-suite)