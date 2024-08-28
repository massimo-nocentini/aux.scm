

(import unittest aux)

(define-suite auxtest

    ((test/let1 _)
        (let1 (a 1) (⊦= a 1)))

    ((test/letcar&cdr _)
        (letcar&cdr (((a d) (cons 1 '()))
                     ((aa dd) (cons 2 3)))
            (⊦= a 1) (⊦= d '()) (⊦= aa 2) (⊦= dd 3)))

    ((test/letcc/multiarg _)
        (⊦= 'a (letcc k (k 'a))))

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

    ((test/letmap _)
        (⊦= '((((2 a #t) (2 a #t)) ((2 1 #t) (2 1 #f)))
              (((3 a #f) (3 a #t)) ((3 2 #f) (3 2 #f)))
              (((4 a #t) (4 a #t)) ((4 3 #t) (4 3 #f))))
          (letmap ((x (list 1 2 3))
                   (y `(a ,x))
                   (z (list (odd? x) (symbol? y))))
            (list (add1 x) y z))))

    ((test/letmapflat _)
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
          (letmapflat ((x (list 1 2 3))
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
                                  (if (null? *saved*) witness (let1 (cont (pop! *saved*)) (cont)))))
                        (dft-comb (lambda (another recv)
                                   (lambda (tree)
                                    (let ((node1 (dft-node tree)))
                                     (if (eq? node1 witness) witness (recv node1 (dft-node another)))))))
                        (dft2 (lambda (visitor tree f)
                                (let ((node (visitor tree)))
                                 (cond
                                  ((eq? node witness) (void))
                                   (else (f node) (restart)))))))

                 (define (pusher v) (push! v col))

                 (⊦= (void) (dft2 dft-node t1 pusher))
                 
                 (⊦= '(a b d h c e f i g) (reverse col))

                 (set! col '()) ; reset the collection to the empty state

                 (⊦= (void) (dft2 (dft-comb t2 list) t1 pusher))
                 
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
           (g 5)) (reverse col))
                 )

        
                #;(⊦= 'a (dft-node t1))
                #;(⊦= 'b (restart))
                
        )
    )
)

(unittest/✓ auxtest)

