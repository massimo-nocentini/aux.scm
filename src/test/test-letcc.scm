
(import unittest aux scheme (chicken sort) (chicken syntax))

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
						   "A Better API for First-Class Continuations") ", by Marc Feeley."))))

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



  )

(unittest/✓ letcc-suite)




