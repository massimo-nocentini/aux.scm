

(import unittest aux (chicken sort))

(define-suite auxtest

    ((test/let1 _)
        (let1 (a 1) (⊦= a 1)))

    ((test/letcar&cdr _)
        (letcar&cdr (((a d) (cons 1 '()))
                     ((aa dd) (cons 2 3)))
            (⊦= a 1) (⊦= d '()) (⊦= aa 2) (⊦= dd 3)))

    ((test/letcc/multiarg _)
        (⊦= 'a (letcc k (k 'a))))

    ((test/letcc/delimcc _)

        (⊦= 21 (letdelimcc (shift reset yield)
                (+ 1 (* 2 (shift k (k (k 10)))))))
        
        (⊦= 41 (letdelimcc (shift reset yield)
                (+ 1 (reset (* 2 (shift k (k (k 10))))))))

        (⊦= 15 (letdelimcc (shift reset yield)
                (+ 10 (reset (+ 2 3)))))
       
        (⊦= 13 (letdelimcc (shift reset yield)
                (+ 10 (reset (+ 2 (shift k 3))))))

        (⊦= 15 (letdelimcc (shift reset yield)
                (+ 10 (reset (+ 2 (shift k (k 3)))))))

        (⊦= 115 (letdelimcc (shift reset yield)
                (+ 10 (reset (+ 2 (shift k (+ 100 (k 3))))))))
      
        (⊦= 117 (letdelimcc (shift reset yield)
                (+ 10 (reset (+ 2 (shift k (+ 100 (k (k 3))))))))))

     ((test/letcc/delimcc+yield _)

        (⊦= '(1) (letdelimcc (shift reset yield)
                (reset
                 (begin 
                  (yield 1)
                  '()))))

        (⊦= '(1 2) (letdelimcc (shift reset yield)
                (reset
                 (begin 
                  (yield 1)
                  (yield 2)
                  '()))))

                )

    ((test/letcc/delimcc+monad _)

        (letdelimcc (shift reset yield)

                    (define (reflect meaning) (shift k (extend k meaning)))
                    (define (reify t) (reset (eta (t))))
                    (define (eta x) (list x))
                    (define (extend f l) (apply append (map f l)))

                    (define-syntax amb (syntax-rules () ((amb v ...) (reflect (append (reify (thunk v)) ...)))))

                    (⊦= '(1 2 3) (reify (thunk (amb 1 2 3))))
                    (⊦= '(8 9 9 10) (reify (thunk (+ (amb 1 2) 3 (amb 4 5)))))
                    (⊦= '(31 51) (reify (thunk (+ 1 (letcc k (* 10 (amb 3 (k 4))))))))))
      
    ((test/letcc* _)
        (⊦= 3 (letcc* ⤶ ((v (+ 1 (⤶ 1)))
                         (w (+ 2 v)))
                (+ 4 w)))
        (⊦= 5 (letcc* ⤶ ((v (+ 1 (⤶ 1)))
                         (w (+ 2 (⤶ (+ 3 v)))))
                (+ 1 w)))
        (⊦= 3 (letcc* ⤶ ((v (+ 2 (⤶ 2))))
                (+ 1 v)))
        (⊦= 3 (letcc* ⤶ ((v (+ 2 (⤶ 2))))
                (⤶ (+ 1 v))))
        (⊦= 2 (letcc* ⤶ ((v (+ 2 (⤶ 2))))
                (+ 1 (⤶ v))))
        (⊦= 6 (letcc* ⤶ ((v (+ 2 (⤶ 2))))
                (+ 1 (⤶ (+ 4 v))))))

    ((test/trycc1 _)
        (⊦= 5 (trycc 
                (✗ 
                  (+ 1 (✗))
                  (+ 2 3))
                (else (cons 3 '())))))

    ((test/trycc2 _)
        (⊦= 3 (trycc 
                (✗
                  (+ 1 2)
                  (+ 2 (✗)))
                (else (cons 3 '())))))

    ((test/trycc3 _)
        (⊦= '(3) (trycc 
                   (✗
                      (+ 1 (✗))
                      (+ 2 (✗)))
                   (else (cons 3 '())))))

    ((test/λ _)
        (⊦= 5 ((λ (x) (+ x 2)) 3))
        (⊦= 5 ((λ (x) ((λ (y) (+ x y)) 2)) 3))
        (⊦= 5 ((λ (x y) (+ x y)) 2 3)))

    ((test/letmaptensor _)
        (⊦= '((((2 a #t) (2 a #t)) ((2 1 #t) (2 1 #f)))
              (((3 a #f) (3 a #t)) ((3 2 #f) (3 2 #f)))
              (((4 a #t) (4 a #t)) ((4 3 #t) (4 3 #f))))
          (letmaptensor ((x (list 1 2 3))
                   (y `(a ,x))
                   (z (list (odd? x) (symbol? y))))
            (list (add1 x) y z))))

    ((test/letmap _)
        (⊦= '((2 a #t)
           (2 a #t)
           (2 1 #t)
           (2 1 #f)
           (3 a #f)
           (3 a #t)
           (3 2 #f)
           (3 2 #f)
           (4 a #t)
           (4 a #t)
           (4 3 #t)
           (4 3 #f))
          (letmap ((x (list 1 2 3))
                       (y `(a ,x))
                       (z (list (odd? x) (symbol? y))))
            (list (add1 x) y z))))

    #;((test/letcc/save _)
        (let* ((frozen (void)))
           (display (append '(the letcc returned) (list (letcc k (set! frozen k) 'a))))
           #;(⊦= '(the letcc returned a) (frozen 3))
           (frozen 3)
           #;(⊦= '(the letcc returned 3) x)))

    #;((test/letcc/share _)
        (display (let ((l '())
              (i 0)
              (froz1 (void)) 
              (froz2 (void)))
           (letcc (froz1 froz2) k)
           (set! i (add1 i))
           (push! i l)
           (froz1 '()))
           l))


    ((test/letcc/dfs _)
    
        (begin
        
                (define t1 '(a (b (d h)) (c e (f i) g))) 
                (define t2 '(1 (2 (3 6 7) 4 5)))

                (letrec ((*saved* '())
                         (col '())
                         (witness (gensym))
                         (dft-node (lambda (tree)
                                    (cond
                                     ((null? tree) (restart))
                                     ((not (pair? tree)) tree)
                                     (else (letcc cc
                                            (push! (thunk (cc (dft-node (cdr tree)))) *saved*)
                                            (dft-node (car tree)))))))
                        (restart (thunk
                                  (if (null? *saved*) 
                                   witness 
                                   (let1 (cont (pop! *saved*))
                                    (cont)))))
                        (dft-comb (lambda (another)
                                   (lambda (tree)
                                    (let1 (node1 (dft-node tree))
                                     (if (eq? node1 witness) witness (list node1 (dft-node another)))))))
                        (dft2 (lambda (v)
                                (if (eq? v witness) 
                                 (reverse col)
                                 (begin
                                  (push! v col)
                                  (restart))))))

                 (⊦= '(a b d h c e f i g) (dft2 (dft-node t1)))
                 
                 (set! col '()) ; reset the collection to the empty state

                 (⊦= '((a 1)
           (a 2)
           (a 3)
           (a 6)
           (a 7)
           (a 4)
           (a 5)
           (b 1)
           (b 2)
           (b 3)
           (b 6)
           (b 7)
           (b 4)
           (b 5)
           (d 1)
           (d 2)
           (d 3)
           (d 6)
           (d 7)
           (d 4)
           (d 5)
           (h 1)
           (h 2)
           (h 3)
           (h 6)
           (h 7)
           (h 4)
           (h 5)
           (c 1)
           (c 2)
           (c 3)
           (c 6)
           (c 7)
           (c 4)
           (c 5)
           (e 1)
           (e 2)
           (e 3)
           (e 6)
           (e 7)
           (e 4)
           (e 5)
           (f 1)
           (f 2)
           (f 3)
           (f 6)
           (f 7)
           (f 4)
           (f 5)
           (i 1)
           (i 2)
           (i 3)
           (i 6)
           (i 7)
           (i 4)
           (i 5)
           (g 1)
           (g 2)
           (g 3)
           (g 6)
           (g 7)
           (g 4)
           (g 5))  (dft2 ((dft-comb t2) t1))))
        )
    )

    ((test/letnondeterministic/choose-null? _)
      (⊦= '() (letnondeterministic (? ¿ † • ! ¶) (? '()))))

    ((test/letnondeterministic/choose _)
       (⊦= '(1 2 3) (letnondeterministic (? ¿ † • ! ¶) (? '(1 2 3)))))

    ((test/letnondeterministic/choose§ _)
       (⊦= '(1 2 3) (letnondeterministic (? ¿ † • ! ¶) (? (cons§ 1 (cons§ 2 (cons§ 3 '())))))))

    ((test/letnondeterministic/choose-rec _)
       (⊦= '(0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181)
           (letnondeterministic
            ((? ¿ † • ! ¶) (? (fibs§ 0 1)))
            ((v next) (if (< (car v) 3000) (next) (reverse v))))))

    #|

   (import unittest aux (chicken sort))

      (define-nondeterministic (q (? ¿ † • ! ¶))
       (? (map§ (lambda (v) (list 'a v)) (fibs§ 0 1)) 
          (map§ (lambda (v) (list 'b v)) (fibs§ 0 1))
          (map§ (lambda (v) (list 'c v)) (fibs§ 0 1))))

      (define-nondeterministic (q (? ¿ † • ! ¶))
       (? '((a 1) (a 2) (a 3)) '((b 1) (b 2))))

      (add1 (q))

    |#

    ((test/letnondeterministic/choose+nested _)
       (⊦= '(1 4 3 1 5 3) (letnondeterministic (? ¿ † • ! ¶) (? `(1 ,(? '(4 5)) 3)))))

    ((test/letnondeterministic/choose+nested+¿ _)
       (⊦= '(1 4 3 1 5 3) (letnondeterministic (? ¿ † • ! ¶) (? `(1 ,(¿ '(4 5)) 3)))))

    ((test/letnondeterministic/choose+double _)
       (⊦= '(5 6 6 7 7 8) (letnondeterministic (? ¿ † • ! ¶) (+ (? '(1 2 3)) (? '(4 5))))))

    ((test/letnondeterministic/choose+fair+fibs _)
       (⊦= '((a 0)
           (b 0)
           (c 0)
           (a 1)
           (b 1)
           (c 1)
           (a 1)
           (b 1)
           (c 1)
           (a 2)
           (b 2)
           (c 2)
           (a 3)
           (b 3)
           (c 3)
           (a 5)
           (b 5)
           (c 5)
           (a 8)
           (b 8)
           (c 8)) 
        (letnondeterministic 21 (? ¿ † • ! ¶)
          (? (map§ (lambda (v) (list 'a v)) (fibs§ 0 1)) 
             (map§ (lambda (v) (list 'b v)) (fibs§ 0 1))
             (map§ (lambda (v) (list 'c v)) (fibs§ 0 1))))))

    ((test/letnondeterministic/choose+fair _)
       (⊦= '((a 1) (b 1) (a 2) (b 2) (a 3)) (letnondeterministic (? ¿ † • ! ¶) (? '((a 1) (a 2) (a 3)) '((b 1) (b 2))))))

    ((test/letnondeterministic/odd _)
       (⊦= '(1 3)
           (letnondeterministic (? ¿ † • ! ¶)
             (let1 (v (? '(1 2 3))) 
               (¶ (odd? v)) 
               v))))

    ((test/letnondeterministic/parlor _)
    
       (⊦= '((1 1 5)
           (1 2 4)
           (1 3 3)
           (1 4 2)
           (1 5 1)
           (2 1 4)
           (2 2 3)
           (2 3 2)
           (2 4 1)
           (3 1 3)
           (3 2 2)
           (3 3 1)
           (4 1 2)
           (4 2 1)
           (5 1 1)) (letnondeterministic (? ¿ † • ! ¶)
        
                (define (two-numbers)
                  (list (? '(1 2 3 4 5)) (? '(1 2 3 4 5)) (? '(1 2 3 4 5))))

                (define (parlor sum)
                  (let ((nums (two-numbers)))
                    (¶ (eq? (apply + nums) sum))
                    nums))
                
                (parlor 7)
                 
        )))

        ((test/letnondeterministic/parlor+sorted _)
    
       (⊦= '((1 1 5) (1 2 4) (1 3 3) (2 2 3)) 
         (letnondeterministic (? ¿ † • ! ¶)
        
                (define (two-numbers)
                  (list (? '(1 2 3 4 5)) (? '(1 2 3 4 5)) (? '(1 2 3 4 5))))

                (define (parlor sum)
                  (let ((nums (two-numbers)))
                    (¶ (and (eq? (apply + nums) sum) (sorted? nums <)))
                    nums))
                
                (parlor 7)
                 
        )))

         ((test/letnondeterministic/coin _)

           (⊦= '(((la 1 1) (la 1 2))
           ((la 1 1) (la 1 2) (la 2 1) (la 2 2) (ny 1 1))
           ((la 1 1)
            (la 1 2)
            (la 2 1)
            (la 2 2)
            (ny 1 1)
            (ny 1 2)
            (ny 2 1)
            (ny 2 2)
            (bos 1 1)
            (bos 1 2)
            (bos 2 1)
            (bos 2 2)))
             (letnondeterministic (? ¿ † • ! ¶)

                (define (coin? x)
                  (member? x '((la 1 2) (ny 1 1) (bos 2 2))))

                  (let* ((*paths* '())
                         (attempts '())
                         (city (? '(la ny bos)))
                         (store (? '(1 2)))
                         (box (? '(1 2)))
                         (triple (list city store box)))
                  (push! triple attempts)
                  (¶ (coin? triple))
                  (reverse attempts)))))

        ((test/letnondeterministic/coin+cut _)

           (⊦= '(((la 1 1) (la 1 2))
           ((la 1 1) (la 1 2) (ny 1 1))
           ((la 1 1)
            (la 1 2)
            (ny 1 1)
            (bos 1 1)
            (bos 1 2)
            (bos 2 1)
            (bos 2 2)))
             (letnondeterministic (? ¿ † • ! ¶)

                (define (coin? x)
                  (member? x '((la 1 2) (ny 1 1) (bos 2 2))))

                  (let* ((*paths* '())
                         (attempts '())
                         (city (? '(la ny bos)))
                         (flag (•))
                         (store (? '(1 2)))
                         (_ (•))
                         (box (? '(1 2)))
                         (triple (list city store box)))
                  (push! triple attempts)
                  (¶ (coin? triple))
                  (! flag)
                  (reverse attempts)))))

        #;((test/letnondeterministic/coin+cut/bfs _)

           (⊦= '(((la 1 1) (la 1 2))
           ((la 1 1) (la 1 2) (ny 1 1))
           ((la 1 1)
            (la 1 2)
            (ny 1 1)
            (bos 1 1)
            (bos 1 2)
            (bos 2 1)
            (bos 2 2)))
             (letnondeterministic pool (? ¿ † • ! ¶)

                (define (coin? x)
                  (member? x '((la 1 2) (ny 1 1) (bos 2 2))))

                  (let* ((*paths* '())
                         (attempts '())
                         (city (¿ '(la ny bos)))
                         ;(flag (‡))
                         (store (¿ '(1 2)))
                         (box (¿ '(1 2)))
                         (triple (list city store box)))
                  (push! triple attempts)
                  (if (coin? triple) (begin #;(¡ flag) (‡) (reverse attempts)) (†))))))

         ((test/letnondeterministic/graph+cycles/bfs _)

                (⊦= '((a b c a) (a b c e a) (a b d e a))
                (letnondeterministic (? ¿ † • ! ¶)

                        (define (neighbors node) ; our graph, with cycles.
                         (letassoc 
                          (node '(
                                (a (b))
                                (b (c d))
                                (c (a e))
                                (d (e))
                                (e (a))
                                ))
                          (else '())))

                        #;(define (path node1 node2 seen)
                         (let1 (hood (neighbors node1))
                          (cond
                           ((null? hood) (†))
                           ((pair? (member node2 hood)) (list node2))
                           (else (let1 (n (? hood)) (cons n (path n node2 (cons node1 seen))))))))

                        #;(define (path node1 node2 seen)
                         (let1 (hood (neighbors node1))
                          (cond
                           ((member? node2 hood) (list node2))
                           (else (let1 (n (? hood)) (cons n (path n node2 (cons node1 seen))))))))

                        #;(define (path node1 node2 seen)
                          (if (member? node1 seen)
                           (list)
                           (let1 (n (? (neighbors node1))) 
                            (cons n (path n node2 (cons node1 seen))))))

                        (define (path node1 node2 seen)
                           (¶ (not (member? node1 seen)))
                           (let1 (n (? (neighbors node1)))
                             (if (eq? n node2)
                              (list node2)
                              (cons n (path n node2 (cons node1 seen))))))

                        (let* ((source 'a)
                               (p (path source 'a '())))
                         (cons source p))
                )))

        ((test/stream/fibs _)
         (let1 (fibs (take§ 20 (rec F (cons§ 0 (cons§ 1 (zip§ + F (cdr§ F)))))))
                (⊦= '(0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181) (§->list fibs))))

        ((test/stream/ones _)
         (let1 (ones (take§ 20 (rec O (cons§ 1 O))))
                (⊦= '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) (§->list ones))))
)

(unittest/✓ auxtest)

