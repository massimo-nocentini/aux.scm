
(import (aux unittest) (aux base) (aux fds sbral) (chicken base) (chicken condition) srfi-1)

; the message of the error raised by thunk, or 'not-raised: ⊦⧳ tolerates a condition, this requires it
(define (error-message thunk)
  (condition-case (begin (thunk) 'not-raised)
    (e (exn) ((condition-property-accessor 'exn 'message) e))))

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

        ;  (⊦ eq? (cdr (cdr/sbral (cons/sbral 'g q))) (cdr q))
         (set! q (cons/sbral 'g q))
         (⊦= '((7 g (f (e) (d)) (c (b) (a)))) q)
         (⊦= 'g (car/sbral q))
         (⊦= '((3 f (e) (d)) (3 c (b) (a))) (cdr/sbral q))))

  ((test-cdr-invalid _)
   (⊦= "cdr/sbral: not a valid sbral" (error-message (λ () (cdr/sbral empty/sbral))))
   (⊦= "cdr/sbral: not a valid sbral" (error-message (λ () (cdr/sbral 42))))
   (⊦= "car/sbral: not a valid sbral" (error-message (λ () (car/sbral empty/sbral)))))

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

  ((test-update-out-of-bounds _)
   (let1 (q (list->sbral '(a b c d e)))
         (⊦= '((1 a) (1 b) (3 c (d) (hello))) (update/sbral 4 'hello q))
         (⊦= "update/sbral: index out of bounds" (error-message (λ () (update/sbral 5 'x q))))
         (⊦= "update/sbral: index out of bounds" (error-message (λ () (update/sbral -1 'x q))))
         (⊦= "update/sbral: index out of bounds" (error-message (λ () (update/sbral 0 'x empty/sbral))))))

  ((test-identity _)
   (let1 (l '(a b c d e f g))
         (⊦= l (sbral->list (list->sbral l))))
   (do ((n 0 (add1 n))) ((> n 64))
     (⊦= (iota n) (sbral->list (list->sbral (iota n))))))

  ((test-foldr _)
   (let1 (q (list->sbral '(a b c d e f g h i j)))      ; trees of sizes 3 and 7
         (⊦= '((0 . a) (1 . b) (2 . c) (3 . d) (4 . e) (5 . f) (6 . g) (7 . h) (8 . i) (9 . j))
             (foldr/sbral (λ (i each out) (cons (cons i each) out)) '() q))
         ; `f` is called last index first, as `foldr` does
         (let1 (seen '())
               (foldr/sbral (λ (i each out) (set! seen (cons i seen))) (void) q)
               (⊦= (iota 10) seen))
         (⊦= '((0 a) (1 b) (2 c) (3 d) (4 e) (5 f) (6 g) (7 h) (8 i) (9 j))
             (sbral->list (map/sbral (λ (i each) (list i each)) q)))
         (⊦= '(b d f h j) (sbral->list (filter/sbral (λ (i each) (odd? i)) q)))
         (⊦= 0 (foldr/sbral (λ (i each out) (add1 out)) 0 empty/sbral))))

  ((test-exists _)
   (let1 (q (list->sbral '(a b c d e f g h i j)))
         (⊭ (exists?/sbral (λ (i each) #f) q))
         (⊭ (exists?/sbral (λ (i each) #t) empty/sbral))
         ; the value of the first hit from the end, and `pred?` is not called after it
         (let1 (calls 0)
               (⊦= '(7 h) (exists?/sbral (λ (i each) (set! calls (add1 calls)) (and (< i 8) (list i each))) q))
               (⊦= 3 calls))))

  )

(unittest/✓ sbral-suite)