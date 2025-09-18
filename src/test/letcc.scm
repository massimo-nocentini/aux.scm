
(import (aux unittest) (aux base) (aux continuation))

(define-suite letcc-suite


  ((doc r) (let ((letcc-expr '(letcc k body ...)))
             `((structure/section "Call with current continuation: the " (code/inline "letcc") " macro")
               (p "The fundamental macro " (code/inline "letcc") " in the form "
                  (code/scheme ,letcc-expr) " binds " (code/inline k) " to the " (i "current continuation ") 
                  (cite/a "https://docs.scheme.org/schintro/schintro_141.html")
                  (cite/a "https://en.wikipedia.org/wiki/Call-with-current-continuation") 
                  (cite/a "https://ds26gte.github.io/tyscheme/index-Z-H-15.html")
                  (cite/a "https://matt.might.net/articles/programming-with-continuations--exceptions-backtracking-search-threads-generators-coroutines/"
                          "Continuations by example")
                  " in " (code/inline body ...) " expressions; for the sake of clarity, it expands to"
                  (code/scheme/expand ,letcc-expr)
                  "where " (code/inline "continuation-capture") " and " (code/inline "continuation-return") 
                  " are defined in " (cite/a "https://wiki.call-cc.org/man/5/Module%20(chicken%20continuation)" 
                                             "Module " (code/inline "(chicken continuation)")) 
                  " and based on " (cite/a "http://www.iro.umontreal.ca/~feeley/papers/FeeleySW01.pdf" 
                                           "A Better API for First-Class Continuations") 
                  " by Marc Feeley, respectively."))))

  ((test/letcc/multiarg _)
   (⊦= 'a (letcc k (k 'a))))

  ((test/letcc* _)
   (⊦= '(1) (letcc* ⤶ ((v (cons 1 '()))
                           (w (cons 2 v)))
                      (cons 4 w)))
   (⊦= '(2 1) (letcc* ⤶ ((v (cons 1 (⤶ '(1))))
                             (w (cons 2 v)))
                        (cons 4 w)))
   (⊦= '(4 3 1) (letcc* ⤶ ((v (cons 1 (⤶ '(1))))
                               (w (cons 2 (⤶ (cons 3 v)))))
                          (cons 4 w)))
   (⊦= '(1 2) (letcc* ⤶ ((v (cons 3 (⤶ '(2)))))
                        (cons 1 v)))
   (⊦= '(3 2) (letcc* ⤶ ((v (cons 1 (⤶ '(2)))))
                        (⤶ (cons 3 v))))
   (⊦= '(2) (letcc* ⤶ ((v (cons 3 (⤶ '(2)))))
                      (cons 1 (⤶ v))))
   (⊦= '(4 2) (letcc* ⤶ ((v (cons 3 (⤶ '(2)))))
                        (cons 1 (⤶ (cons 4 v)))))
   `(doc (p "The frame in "
            (cite/a "https://mitpress.mit.edu/9780262561006/the-seasoned-schemer/" 
                    "The Seasoned Schemer: page 89, see " 
                    (code/inline "rember1*") " definition.")
            " is of inspiration of the " (code/inline "letcc*") " macro.")
         (code/scheme/expand (letcc* ⤶ ((v vexpr)) body ...))))

  ((test/trycc _)
   (⊦= 5 (trycc
             (✗
               (+ 1 (✗))
               (+ 2 3))
             (else (cons 3 '()))))
   (⊦= 3 (trycc
             (✗
               (+ 1 2)
               (+ 2 (✗)))
             (else (cons 3 '()))))
   (⊦= '(3) (trycc
                (✗
                  (+ 1 (✗))
                  (+ 2 (✗)))
                (else (cons 3 '())))))

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
             (g 5))  (dft2 ((dft-comb t2) t1)))))


  )

(unittest/✓ letcc-suite)











































