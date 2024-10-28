

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
        #;(⊦= 10 (letshiftcc k 10))
        #;(⊦= 21 (+ 1 (* 2 (letshiftcc k (k (k 10))))))
        (⊦= 41 (+ 1 (resetcc (* 2 (letshiftcc k (k (k 10)))))))
        (⊦= 15 (+ 10 (resetcc (+ 2 3))))
        (⊦= 13 (+ 10 (resetcc (+ 2 (letshiftcc k 3)))))
        (⊦= 15 (+ 10 (resetcc (+ 2 (letshiftcc k (k 3))))))
        (⊦= 115 (+ 10 (resetcc (+ 2 (letshiftcc k (+ 100 (k 3)))))))
        (⊦= 117 (+ 10 (resetcc (+ 2 (letshiftcc k (+ 100 (k (k 3))))))))
        (⊦= 117 (resetcc (+ 10 (resetcc (+ 2 (letshiftcc k (+ 100 (k (k 3))))))))))

     ((test/letcc/delimcc+asai+tutorial _)
        ; Tests according to the tutorial at http://pllab.is.ocha.ac.jp/~asai/cw2011tutorial/main-e.pdf by Kenichi Asai.

        (⊦= 10 (resetcc (sub1 (+ 3 (letshiftcc k (* 5 2))))))
        (⊦= 9 (sub1 (resetcc (+ 3 (letshiftcc k (* 5 2))))))
        (⊦= 'hello (resetcc (sub1 (+ 3 (letshiftcc k 'hello)))))
        
        (define (prod lst)
         (cond
          ((null? lst) 1))
          ((zero? (car lst)) (delimcc-discard 'zero))
          (else (* (car lst) (prod (cdr lst)))))
        
        (⊦= 'zero (resetcc (prod '(2 3 0 5))))

        (define-resetcc f (sub1 (+ 3 (letshiftcc k k))))
        (⊦= 12 (f 10))

        (define-resetcc g (sub1 (+ 3 (delimcc-extract))))
        (⊦= 12 (g 10))

        (define (appender lst)
         (cond
          ((null? lst) (delimcc-extract))
          (else (cons (car lst) (appender (cdr lst))))))

        (define-resetcc A (appender '(1 2 3)))
        (⊦= '(1 2 3 4 5 6) (A '(4 5 6)))

        (define (walk f tree)
         (cond
          ((null? tree) (void))
          (else 
           (walk f (car tree))
           (f (cadr tree))
           (walk f (caddr tree)))))

        (⊦= '(1 2 3) (§->list (resetcc+null (walk (lambda (v) (yield§ v)) '((() 1 ()) 2 (() 3 ()))))))

        (⊦= 600 (delimcc-foldr 100 ((each prod) (* each prod))
                (walk delimcc-cons '((() 1 ()) 2 (() 3 ())))))

        (define-resetcc a (append (delimcc-τ '(hello)) '(world)))
        (⊦= '(hello world) (a))

        (define-resetcc p (append '(hello) (delimcc-lambda (x) (list x)) '(world)))
        (⊦= '(hello 4 world) (p 4))

        (⊦= '(1 3 3) (resetcc (delimcc-either `(1 ,(add1 2) 3))))

        (⊦= '(((p #t) (q #f))) 
           (let1 (sols '())
            (resetcc
                      (let ((p (delimcc-either '(#t #f)))
                            (q (delimcc-either '(#t #f))))
                       (when (and (or p q) (or p (not q)) (or (not p) (not q)))
                        (push! `((p ,p) (q ,q)) sols))))
            sols))
     )

     ((test/letcc/delimcc+yield _)
        (⊦= '(1) (resetcc+null (yield 1)))
        (⊦= '(1 2) (resetcc+null (yield 1) (yield 2))))

    ((test/letcc/delimcc+yield§ _)
        (⊦= '(1) (§->list (resetcc+null (yield§ 1))))
        (⊦= '(1 2) (§->list (resetcc+null (yield§ 1) (yield§ 2)))))

    ((test/letcc/delimcc+monad _)

                    (define (reflect meaning) (letshiftcc k (extend k meaning)))
                    (define (reify* t) (resetcc (eta (t))))
                    (define (eta x) (list x))
                    (define (extend f l) (apply append (map f l)))

                    (define-syntax reify (syntax-rules () ((reify body ...) (reify* (τ body ...)))))
                    (define-syntax amb (syntax-rules () ((amb v ...) (reflect (append (reify v) ...)))))

                    (⊦= '(1 2 3) (reify (amb 1 2 3)))
                    (⊦= '(8 9 9 10) (reify (+ (amb 1 2) 3 (amb 4 5))))
                    (⊦= '(31 51) (reify (+ 1 (letcc k (* 10 (amb 3 (k 4)))))))

                    (define (www)
                     (define (f x) (+ x (amb 6 4 2 8) (amb 2 4 5 4 1)))
                     (reify (f (f (amb 0 2 3 4 5 32)))))

                    (⊦= '(16 18 19 18 15 14 16 17 16 13 12 14 15 14 11 18 20 21 20 17 18 20 21 20 17 16 18 19 18 15 14 16 17 16 13 20 22 23 22 19 19 21 22 21 18 17 19 20 19 16 15 17 18 17 14 21 23 24 23 20 18 20 21 20 17 16 18 19 18 15 14 16 17 16 13 20 22 23 22 19 15 17 18 17 14 13 15 16 15 12 11 13 14 13 10 17 19 20 19 16 14 16 17 16 13 12 14 15 14 11 10 12 13 12 9 16 18 19 18 15 16 18 19 18 15 14 16 17 16 13 12 14 15 14 11 18 20 21 20 17 17 19 20 19 16 15 17 18 17 14 13 15 16 15 12 19 21 22 21 18 16 18 19 18 15 14 16 17 16 13 12 14 15 14 11 18 20 21 20 17 13 15 16 15 12 11 13 14 13 10 9 11 12 11 8 15 17 18 17 14 12 14 15 14 11 10 12 13 12 9 8 10 11 10 7 14 16 17 16 13 14 16 17 16 13 12 14 15 14 11 10 12 13 12 9 16 18 19 18 15 15 17 18 17 14 13 15 16 15 12 11 13 14 13 10 17 19 20 19 16 14 16 17 16 13 12 14 15 14 11 10 12 13 12 9 16 18 19 18 15 11 13 14 13 10 9 11 12 11 8 7 9 10 9 6 13 15 16 15 12 18 20 21 20 17 16 18 19 18 15 14 16 17 16 13 20 22 23 22 19 20 22 23 22 19 18 20 21 20 17 16 18 19 18 15 22 24 25 24 21 21 23 24 23 20 19 21 22 21 18 17 19 20 19 16 23 25 26 25 22 20 22 23 22 19 18 20 21 20 17 16 18 19 18 15 22 24 25 24 21 17 19 20 19 16 15 17 18 17 14 13 15 16 15 12 19 21 22 21 18 18 20 21 20 17 16 18 19 18 15 14 16 17 16 13 20 22 23 22 19 20 22 23 22 19 18 20 21 20 17 16 18 19 18 15 22 24 25 24 21 21 23 24 23 20 19 21 22 21 18 17 19 20 19 16 23 25 26 25 22 20 22 23 22 19 18 20 21 20 17 16 18 19 18 15 22 24 25 24 21 17 19 20 19 16 15 17 18 17 14 13 15 16 15 12 19 21 22 21 18 16 18 19 18 15 14 16 17 16 13 12 14 15 14 11 18 20 21 20 17 18 20 21 20 17 16 18 19 18 15 14 16 17 16 13 20 22 23 22 19 19 21 22 21 18 17 19 20 19 16 15 17 18 17 14 21 23 24 23 20 18 20 21 20 17 16 18 19 18 15 14 16 17 16 13 20 22 23 22 19 15 17 18 17 14 13 15 16 15 12 11 13 14 13 10 17 19 20 19 16 14 16 17 16 13 12 14 15 14 11 10 12 13 12 9 16 18 19 18 15 16 18 19 18 15 14 16 17 16 13 12 14 15 14 11 18 20 21 20 17 17 19 20 19 16 15 17 18 17 14 13 15 16 15 12 19 21 22 21 18 16 18 19 18 15 14 16 17 16 13 12 14 15 14 11 18 20 21 20 17 13 15 16 15 12 11 13 14 13 10 9 11 12 11 8 15 17 18 17 14 20 22 23 22 19 18 20 21 20 17 16 18 19 18 15 22 24 25 24 21 22 24 25 24 21 20 22 23 22 19 18 20 21 20 17 24 26 27 26 23 23 25 26 25 22 21 23 24 23 20 19 21 22 21 18 25 27 28 27 24 22 24 25 24 21 20 22 23 22 19 18 20 21 20 17 24 26 27 26 23 19 21 22 21 18 17 19 20 19 16 15 17 18 17 14 21 23 24 23 20 19 21 22 21 18 17 19 20 19 16 15 17 18 17 14 21 23 24 23 20 21 23 24 23 20 19 21 22 21 18 17 19 20 19 16 23 25 26 25 22 22 24 25 24 21 20 22 23 22 19 18 20 21 20 17 24 26 27 26 23 21 23 24 23 20 19 21 22 21 18 17 19 20 19 16 23 25 26 25 22 18 20 21 20 17 16 18 19 18 15 14 16 17 16 13 20 22 23 22 19 17 19 20 19 16 15 17 18 17 14 13 15 16 15 12 19 21 22 21 18 19 21 22 21 18 17 19 20 19 16 15 17 18 17 14 21 23 24 23 20 20 22 23 22 19 18 20 21 20 17 16 18 19 18 15 22 24 25 24 21 19 21 22 21 18 17 19 20 19 16 15 17 18 17 14 21 23 24 23 20 16 18 19 18 15 14 16 17 16 13 12 14 15 14 11 18 20 21 20 17 15 17 18 17 14 13 15 16 15 12 11 13 14 13 10 17 19 20 19 16 17 19 20 19 16 15 17 18 17 14 13 15 16 15 12 19 21 22 21 18 18 20 21 20 17 16 18 19 18 15 14 16 17 16 13 20 22 23 22 19 17 19 20 19 16 15 17 18 17 14 13 15 16 15 12 19 21 22 21 18 14 16 17 16 13 12 14 15 14 11 10 12 13 12 9 16 18 19 18 15 21 23 24 23 20 19 21 22 21 18 17 19 20 19 16 23 25 26 25 22 23 25 26 25 22 21 23 24 23 20 19 21 22 21 18 25 27 28 27 24 24 26 27 26 23 22 24 25 24 21 20 22 23 22 19 26 28 29 28 25 23 25 26 25 22 21 23 24 23 20 19 21 22 21 18 25 27 28 27 24 20 22 23 22 19 18 20 21 20 17 16 18 19 18 15 22 24 25 24 21 20 22 23 22 19 18 20 21 20 17 16 18 19 18 15 22 24 25 24 21 22 24 25 24 21 20 22 23 22 19 18 20 21 20 17 24 26 27 26 23 23 25 26 25 22 21 23 24 23 20 19 21 22 21 18 25 27 28 27 24 22 24 25 24 21 20 22 23 22 19 18 20 21 20 17 24 26 27 26 23 19 21 22 21 18 17 19 20 19 16 15 17 18 17 14 21 23 24 23 20 18 20 21 20 17 16 18 19 18 15 14 16 17 16 13 20 22 23 22 19 20 22 23 22 19 18 20 21 20 17 16 18 19 18 15 22 24 25 24 21 21 23 24 23 20 19 21 22 21 18 17 19 20 19 16 23 25 26 25 22 20 22 23 22 19 18 20 21 20 17 16 18 19 18 15 22 24 25 24 21 17 19 20 19 16 15 17 18 17 14 13 15 16 15 12 19 21 22 21 18 16 18 19 18 15 14 16 17 16 13 12 14 15 14 11 18 20 21 20 17 18 20 21 20 17 16 18 19 18 15 14 16 17 16 13 20 22 23 22 19 19 21 22 21 18 17 19 20 19 16 15 17 18 17 14 21 23 24 23 20 18 20 21 20 17 16 18 19 18 15 14 16 17 16 13 20 22 23 22 19 15 17 18 17 14 13 15 16 15 12 11 13 14 13 10 17 19 20 19 16 22 24 25 24 21 20 22 23 22 19 18 20 21 20 17 24 26 27 26 23 24 26 27 26 23 22 24 25 24 21 20 22 23 22 19 26 28 29 28 25 25 27 28 27 24 23 25 26 25 22 21 23 24 23 20 27 29 30 29 26 24 26 27 26 23 22 24 25 24 21 20 22 23 22 19 26 28 29 28 25 21 23 24 23 20 19 21 22 21 18 17 19 20 19 16 23 25 26 25 22 21 23 24 23 20 19 21 22 21 18 17 19 20 19 16 23 25 26 25 22 23 25 26 25 22 21 23 24 23 20 19 21 22 21 18 25 27 28 27 24 24 26 27 26 23 22 24 25 24 21 20 22 23 22 19 26 28 29 28 25 23 25 26 25 22 21 23 24 23 20 19 21 22 21 18 25 27 28 27 24 20 22 23 22 19 18 20 21 20 17 16 18 19 18 15 22 24 25 24 21 19 21 22 21 18 17 19 20 19 16 15 17 18 17 14 21 23 24 23 20 21 23 24 23 20 19 21 22 21 18 17 19 20 19 16 23 25 26 25 22 22 24 25 24 21 20 22 23 22 19 18 20 21 20 17 24 26 27 26 23 21 23 24 23 20 19 21 22 21 18 17 19 20 19 16 23 25 26 25 22 18 20 21 20 17 16 18 19 18 15 14 16 17 16 13 20 22 23 22 19 17 19 20 19 16 15 17 18 17 14 13 15 16 15 12 19 21 22 21 18 19 21 22 21 18 17 19 20 19 16 15 17 18 17 14 21 23 24 23 20 20 22 23 22 19 18 20 21 20 17 16 18 19 18 15 22 24 25 24 21 19 21 22 21 18 17 19 20 19 16 15 17 18 17 14 21 23 24 23 20 16 18 19 18 15 14 16 17 16 13 12 14 15 14 11 18 20 21 20 17 23 25 26 25 22 21 23 24 23 20 19 21 22 21 18 25 27 28 27 24 25 27 28 27 24 23 25 26 25 22 21 23 24 23 20 27 29 30 29 26 26 28 29 28 25 24 26 27 26 23 22 24 25 24 21 28 30 31 30 27 25 27 28 27 24 23 25 26 25 22 21 23 24 23 20 27 29 30 29 26 22 24 25 24 21 20 22 23 22 19 18 20 21 20 17 24 26 27 26 23 48 50 51 50 47 46 48 49 48 45 44 46 47 46 43 50 52 53 52 49 50 52 53 52 49 48 50 51 50 47 46 48 49 48 45 52 54 55 54 51 51 53 54 53 50 49 51 52 51 48 47 49 50 49 46 53 55 56 55 52 50 52 53 52 49 48 50 51 50 47 46 48 49 48 45 52 54 55 54 51 47 49 50 49 46 45 47 48 47 44 43 45 46 45 42 49 51 52 51 48 46 48 49 48 45 44 46 47 46 43 42 44 45 44 41 48 50 51 50 47 48 50 51 50 47 46 48 49 48 45 44 46 47 46 43 50 52 53 52 49 49 51 52 51 48 47 49 50 49 46 45 47 48 47 44 51 53 54 53 50 48 50 51 50 47 46 48 49 48 45 44 46 47 46 43 50 52 53 52 49 45 47 48 47 44 43 45 46 45 42 41 43 44 43 40 47 49 50 49 46 44 46 47 46 43 42 44 45 44 41 40 42 43 42 39 46 48 49 48 45 46 48 49 48 45 44 46 47 46 43 42 44 45 44 41 48 50 51 50 47 47 49 50 49 46 45 47 48 47 44 43 45 46 45 42 49 51 52 51 48 46 48 49 48 45 44 46 47 46 43 42 44 45 44 41 48 50 51 50 47 43 45 46 45 42 41 43 44 43 40 39 41 42 41 38 45 47 48 47 44 50 52 53 52 49 48 50 51 50 47 46 48 49 48 45 52 54 55 54 51 52 54 55 54 51 50 52 53 52 49 48 50 51 50 47 54 56 57 56 53 53 55 56 55 52 51 53 54 53 50 49 51 52 51 48 55 57 58 57 54 52 54 55 54 51 50 52 53 52 49 48 50 51 50 47 54 56 57 56 53 49 51 52 51 48 47 49 50 49 46 45 47 48 47 44 51 53 54 53 50) 
                     (www))

                    (define (wwww)
                     (define (f x) (+ x (amb 6 4 2 8) (amb 2 4 5 4 1)))
                     (reify (f (f (f (amb 0 2 3 4 5 32))))))

                    (⊦= 48000 (length (wwww)))

                    )
      
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
                                            (push! (τ (cc (dft-node (cdr tree)))) *saved*)
                                            (dft-node (car tree)))))))
                        (restart (τ
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
      (⊦= '() (letnondeterministic (? ¿ ⊦ • !) (? '()))))

    ((test/letnondeterministic/choose _)
       (⊦= '(1 2 3) (letnondeterministic (? ¿ ⊦ • !) (? '(1 2 3)))))

    ((test/letnondeterministic/choose§ _)
       (⊦= '(1 2 3) (letnondeterministic (? ¿ ⊦ • !) (? (cons§ 1 (cons§ 2 (cons§ 3 '())))))))

    ((test/letnondeterministic/choose-rec _)
       (⊦= '(0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584)
           (§->list
            (stop§ (λ (v) (> v 3000))
             (letnondeterministic§ (? ¿ ⊦ • !) (? (fibs§ 0 1)))))))

    ((test/letnondeterministic/choose+nested _)
       (⊦= '(1 4 3 1 5 3) (letnondeterministic (? ¿ ⊦ • !) (? `(1 ,(? '(4 5)) 3)))))

    ((test/letnondeterministic/choose+nested+¿ _)
       (⊦= '(1 4 3 1 5 3) (letnondeterministic (? ¿ ⊦ • !) (? `(1 ,(¿ '(4 5)) 3)))))

    ((test/letnondeterministic/choose+double _)
       (⊦= '(5 6 6 7 7 8) (letnondeterministic (? ¿ ⊦ • !) (+ (? '(1 2 3)) (? '(4 5))))))

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
        (letnondeterministic 21 (? ¿ ⊦ • !)
          (? (interleave§ 
              (map§ (lambda (v) (list 'a v)) (fibs§ 0 1)) 
              (map§ (lambda (v) (list 'b v)) (fibs§ 0 1))
              (map§ (lambda (v) (list 'c v)) (fibs§ 0 1)))))))

    ((test/letnondeterministic/choose+fair _)
       (⊦= '((a 1) (b 1) (a 2) (b 2) (a 3)) (letnondeterministic (? ¿ ⊦ • !) (? (interleave§ '((a 1) (a 2) (a 3)) '((b 1) (b 2)))))))

    ((test/letnondeterministic/odd _)
       (⊦= '(1 3)
           (letnondeterministic (? ¿ ⊦ • !)
             (let1 (v (? '(1 2 3))) 
               (⊦ (odd? v)) 
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
           (5 1 1)) (letnondeterministic (? ¿ ⊦ • !)
        
                (define (two-numbers)
                  (list (? '(1 2 3 4 5)) (? '(1 2 3 4 5)) (? '(1 2 3 4 5))))

                (define (parlor sum)
                  (let ((nums (two-numbers)))
                    (⊦ (eq? (apply + nums) sum))
                    nums))
                
                (parlor 7)
                 
        )))

        ((test/letnondeterministic/parlor+sorted _)
    
       (⊦= '((1 1 5) (1 2 4) (1 3 3) (2 2 3)) 
         (letnondeterministic (? ¿ ⊦ • !)
        
                (define (two-numbers)
                  (list (? '(1 2 3 4 5)) (? '(1 2 3 4 5)) (? '(1 2 3 4 5))))

                (define (parlor sum)
                  (let ((nums (two-numbers)))
                    (⊦ (and (eq? (apply + nums) sum) (sorted? nums <)))
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
             (letnondeterministic (? ¿ ⊦ • !)

                (define (coin? x)
                  (member? x '((la 1 2) (ny 1 1) (bos 2 2))))

                  (let* ((*paths* '())
                         (attempts '())
                         (city (? '(la ny bos)))
                         (store (? '(1 2)))
                         (box (? '(1 2)))
                         (triple (list city store box)))
                  (push! triple attempts)
                  (⊦ (coin? triple))
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
             (letnondeterministic (? ¿ ⊦ • !)

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
                  (⊦ (coin? triple))
                  (! flag)
                  (reverse attempts)))))

         ((test/letnondeterministic/graph+cycles/bfs _)

                (⊦= '((a b c a) (a b c e a) (a b d e a))
                (letnondeterministic (? ¿ ⊦ • !)

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
                           ((null? hood) (⊦))
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
                           (⊦ (not (member? node1 seen)))
                           (let1 (n (? (neighbors node1)))
                             (if (eq? n node2)
                              (list node2)
                              (cons n (path n node2 (cons node1 seen))))))

                        (let* ((source 'a)
                               (p (path source 'a '())))
                         (cons source p))
                )))

        ((test/letnondeterministic/temporal-graphs _)

            (define graph-simple `(
                                (a ((b (1 2 3))))
                                (b ((c (2 5)) (d ,ℕ)))
                                (c ((a (2 3 4 5 6)) (e ,ℕ)))
                                (d ((e (3 4 5))))
                                (e ((a ,ℕ)))
                                ))

            (define (V g) (map car g))

                (⊦=  '(
           ((a b) ((a 1) (b 2)))
           ((a b) ((a 1) (b 3)))
           ((a c) ((a 1) (b 2) (c 5)))
           ((a c) ((a 1) (b 3) (c 5)))
           ((a d) ((a 1) (b 2) (d 3)))
           ((a d) ((a 1) (b 2) (d 4)))
           ((a d) ((a 1) (b 2) (d 5)))
           ((a d) ((a 1) (b 2) (d 6)))
           ((a d) ((a 1) (b 2) (d 7)))
           ((a d) ((a 1) (b 3) (d 4)))
           ((a d) ((a 1) (b 3) (d 5)))
           ((a d) ((a 1) (b 3) (d 6)))
           ((a d) ((a 1) (b 3) (d 7)))
           ((a e) ((a 1) (b 2) (c 5) (e 6)))
           ((a e) ((a 1) (b 2) (c 5) (e 7)))
           ((a e) ((a 1) (b 2) (d 3) (e 4)))
           ((a e) ((a 1) (b 2) (d 3) (e 5)))
           ((a e) ((a 1) (b 2) (d 4) (e 5)))
           ((a e) ((a 1) (b 3) (c 5) (e 6)))
           ((a e) ((a 1) (b 3) (c 5) (e 7)))
           ((a e) ((a 1) (b 3) (d 4) (e 5)))
           ((b a) ((b 1) (c 2) (a 3)))
           ((b a) ((b 1) (c 2) (a 4)))
           ((b a) ((b 1) (c 2) (a 5)))
           ((b a) ((b 1) (c 2) (a 6)))
           ((b a) ((b 1) (c 2) (e 3) (a 4)))
           ((b a) ((b 1) (c 2) (e 3) (a 5)))
           ((b a) ((b 1) (c 2) (e 3) (a 6)))
           ((b a) ((b 1) (c 2) (e 3) (a 7)))
           ((b a) ((b 1) (c 2) (e 4) (a 5)))
           ((b a) ((b 1) (c 2) (e 4) (a 6)))
           ((b a) ((b 1) (c 2) (e 4) (a 7)))
           ((b a) ((b 1) (c 2) (e 5) (a 6)))
           ((b a) ((b 1) (c 2) (e 5) (a 7)))
           ((b a) ((b 1) (c 2) (e 6) (a 7)))
           ((b a) ((b 1) (c 5) (a 6)))
           ((b a) ((b 1) (c 5) (e 6) (a 7)))
           ((b a) ((b 1) (d 2) (e 3) (a 4)))
           ((b a) ((b 1) (d 2) (e 3) (a 5)))
           ((b a) ((b 1) (d 2) (e 3) (a 6)))
           ((b a) ((b 1) (d 2) (e 3) (a 7)))
           ((b a) ((b 1) (d 2) (e 4) (a 5)))
           ((b a) ((b 1) (d 2) (e 4) (a 6)))
           ((b a) ((b 1) (d 2) (e 4) (a 7)))
           ((b a) ((b 1) (d 2) (e 5) (a 6)))
           ((b a) ((b 1) (d 2) (e 5) (a 7)))
           ((b a) ((b 1) (d 3) (e 4) (a 5)))
           ((b a) ((b 1) (d 3) (e 4) (a 6)))
           ((b a) ((b 1) (d 3) (e 4) (a 7)))
           ((b a) ((b 1) (d 3) (e 5) (a 6)))
           ((b a) ((b 1) (d 3) (e 5) (a 7)))
           ((b a) ((b 1) (d 4) (e 5) (a 6)))
           ((b a) ((b 1) (d 4) (e 5) (a 7)))
           ((b c) ((b 1) (c 2)))
           ((b c) ((b 1) (c 5)))
           ((b d) ((b 1) (d 2)))
           ((b d) ((b 1) (d 3)))
           ((b d) ((b 1) (d 4)))
           ((b d) ((b 1) (d 5)))
           ((b d) ((b 1) (d 6)))
           ((b d) ((b 1) (d 7)))
           ((b e) ((b 1) (c 2) (e 3)))
           ((b e) ((b 1) (c 2) (e 4)))
           ((b e) ((b 1) (c 2) (e 5)))
           ((b e) ((b 1) (c 2) (e 6)))
           ((b e) ((b 1) (c 2) (e 7)))
           ((b e) ((b 1) (c 5) (e 6)))
           ((b e) ((b 1) (c 5) (e 7)))
           ((b e) ((b 1) (d 2) (e 3)))
           ((b e) ((b 1) (d 2) (e 4)))
           ((b e) ((b 1) (d 2) (e 5)))
           ((b e) ((b 1) (d 3) (e 4)))
           ((b e) ((b 1) (d 3) (e 5)))
           ((b e) ((b 1) (d 4) (e 5)))
           ((c a) ((c 1) (a 2)))
           ((c a) ((c 1) (a 3)))
           ((c a) ((c 1) (a 4)))
           ((c a) ((c 1) (a 5)))
           ((c a) ((c 1) (a 6)))
           ((c a) ((c 1) (e 2) (a 3)))
           ((c a) ((c 1) (e 2) (a 4)))
           ((c a) ((c 1) (e 2) (a 5)))
           ((c a) ((c 1) (e 2) (a 6)))
           ((c a) ((c 1) (e 2) (a 7)))
           ((c a) ((c 1) (e 3) (a 4)))
           ((c a) ((c 1) (e 3) (a 5)))
           ((c a) ((c 1) (e 3) (a 6)))
           ((c a) ((c 1) (e 3) (a 7)))
           ((c a) ((c 1) (e 4) (a 5)))
           ((c a) ((c 1) (e 4) (a 6)))
           ((c a) ((c 1) (e 4) (a 7)))
           ((c a) ((c 1) (e 5) (a 6)))
           ((c a) ((c 1) (e 5) (a 7)))
           ((c a) ((c 1) (e 6) (a 7)))
           ((c b) ((c 1) (a 2) (b 3)))
           ((c d) ((c 1) (a 2) (b 3) (d 4)))
           ((c d) ((c 1) (a 2) (b 3) (d 5)))
           ((c d) ((c 1) (a 2) (b 3) (d 6)))
           ((c d) ((c 1) (a 2) (b 3) (d 7)))
           ((c e) ((c 1) (a 2) (b 3) (d 4) (e 5)))
           ((c e) ((c 1) (e 2)))
           ((c e) ((c 1) (e 3)))
           ((c e) ((c 1) (e 4)))
           ((c e) ((c 1) (e 5)))
           ((c e) ((c 1) (e 6)))
           ((c e) ((c 1) (e 7)))
           ((d a) ((d 1) (e 3) (a 4)))
           ((d a) ((d 1) (e 3) (a 5)))
           ((d a) ((d 1) (e 3) (a 6)))
           ((d a) ((d 1) (e 3) (a 7)))
           ((d a) ((d 1) (e 4) (a 5)))
           ((d a) ((d 1) (e 4) (a 6)))
           ((d a) ((d 1) (e 4) (a 7)))
           ((d a) ((d 1) (e 5) (a 6)))
           ((d a) ((d 1) (e 5) (a 7)))
           ((d e) ((d 1) (e 3)))
           ((d e) ((d 1) (e 4)))
           ((d e) ((d 1) (e 5)))
           ((e a) ((e 1) (a 2)))
           ((e a) ((e 1) (a 3)))
           ((e a) ((e 1) (a 4)))
           ((e a) ((e 1) (a 5)))
           ((e a) ((e 1) (a 6)))
           ((e a) ((e 1) (a 7)))
           ((e b) ((e 1) (a 2) (b 3)))
           ((e c) ((e 1) (a 2) (b 3) (c 5)))
           ((e d) ((e 1) (a 2) (b 3) (d 4)))
           ((e d) ((e 1) (a 2) (b 3) (d 5)))
           ((e d) ((e 1) (a 2) (b 3) (d 6)))
           ((e d) ((e 1) (a 2) (b 3) (d 7))))
                (letnondeterministic -1 (? ¿ ⊦ • !)

                        (define (path p node1 node2 seen maxlen maxt)
                           (let* ((lastpair (car p))
                                  (l (caddr lastpair))
                                  (_ (⊦ (< l (add1 maxlen))))
                                  (pair (? (letassoc (node1 graph-simple) (else '()))))
                                  (n (car pair))
                                  (_ (⊦ (not (member? n seen))))
                                  (t₁ (? (cadr pair) (λ (t) (< t maxt))))
                                  (t₀ (cadr lastpair))
                                  (_ (⊦ (> t₁ t₀))) ; simulating the fact that transit takes some time > 0.
                                  (newp (cons (list n t₁ (add1 l)) p)))
                             (cond
                              ((eq? n node2) (reverse newp))
                              (else (path newp n node2 (cons node1 seen) maxlen maxt)))))

                        (let* ((vertices (V graph-simple))
                               (source (? vertices))
                               (destination (? vertices))
                               (p (path (list (list source 1 1)) source destination '() 15 10)))

                         #;(list (list source destination) (map (λ (triple) (list (car triple) (cadr triple))) p)) ; each path
                         #;(length p) ; path length
                         #;(? (map (λ (triple) (car triple)) p)) ; vertex usage
                         #;(? (map (λ (triple) (list (car triple) (cadr triple))) p)) ; vertex visiting, wrt time.
                         #;(? (mappair (λ (r s) (list (car r) (car s))) p)) ; edge usage
                         #;(? (mappair (λ (r s) (list (car s) (- (cadr s) (cadr r)))) p)) ; vertex waiting
                         (list source destination) ; path "weight"
                         ))))

        ((test/stream/nats _)
         (let1 (nats (take§ 20 (nats§ 0)))
                (⊦= '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19) (§->list nats))))

        ((test/stream/fibs _)
         (let1 (fibs (take§ 20 (fibs§ 0 1)))
                (⊦= '(0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181) (§->list fibs))))

        ((test/stream/ones _)
         (let1 (ones (take§ 20 (const§ 1)))
                (⊦= '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) (§->list ones))))

        ((test/stream/primes _)
         (let1 (primes (take§ 20 primes§))
                (⊦= '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71) (§->list primes))))

        ((test/stream/ones+τ _)
         (let1 (ones (take§ 20 (thunk§ 1)))
                (⊦= '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) (§->list ones))))

        ((test/procc/grass-model _)

          (define grass-model
           (probcc-model
            (let* ((rain (probcc-coin 0.3))
                   (sprinkler (probcc-coin 0.5))
                   (grass-is-wet (or (and (probcc-coin 0.9) rain)
                                     (and (probcc-coin 0.8) sprinkler)
                                     (probcc-coin 0.1))))
             grass-is-wet
             #;(probcc-when grass-is-wet rain))))
             
          (⊦= '(((V #f) 0.53152855727963) ((V #t) 0.46847144272037)) (probcc-explore +inf.0 grass-model)))

         ((test/procc/flip-xor-model _)

         (define leaves 0)

      (define flipxor-model
       (probcc-model
        (let loop ((p 0.6) (n 10))
         (set! leaves (add1 leaves))
         (cond
          ((equal? 1 n) (probcc-coin p))
          (else (not (equal? (probcc-coin (- 1 p)) (loop p (sub1 n)))))))))
         
         (probcc-explore +inf.0 flipxor-model)
   
         (⊦= 1023 leaves))

      ((test/procc/flip-xor-model/bucket _)

   (define leaves 0)
  (define p 0.5)

(define flipxor-model
 (probcc-model
  (let loop ((n 10))
   (set! leaves (add1 leaves))
   (cond
    ((equal? 1 n) (probcc-coin p))
    (else (not (equal? (probcc-coin (- 1 p)) 
               (probcc-reflect (probcc-explore +inf.0 (probcc-model (loop (sub1 n))))))))))))
   
   (probcc-explore +inf.0 flipxor-model)

   (⊦= 38 leaves))
)

(unittest/✓ auxtest)

