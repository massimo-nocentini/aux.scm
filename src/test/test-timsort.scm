
(import unittest aux scheme (chicken sort) (chicken syntax) srfi-1)

(define n 1000000)
(define r (iota n))

(define-suite timsort-suite


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

  ((test/simple _)
   (⊦= '(1 2 3 4 5) (timsort '(5 4 3 2 1)))
   (⊦= '(1 2 3 4 5) (timsort '(1 2 3 4 5)))
   (⊦= '(1 2 3 4 5) (timsort '(3 2 1 5 4)))
   (⊦= '(1) (timsort '(1)))
   (⊦= '() (timsort '())))

  ((test/iota _)
    (⊦= r (timsort (iota n (sub1 n) -1))))

  ((test/iota/sort _)
    (⊦= r (sort (iota n (sub1 n) -1) <)))

  ((test/timtros/iota _)
    (⊦= (iota n (sub1 n) -1) (timtros r)))

  ((test/tros/iota _)
    (⊦= (iota n (sub1 n) -1) (reverse (sort r <))))

  ((test/already-sorted _)
    (⊦= r (timsort r)))

  ((test/already-sorted/sort _)
    (⊦= r (sort r <)))
  
  ((test/already-sorted/primitive _)
    (⊦= r (timsort/primitive r)))

  ((test/primitive _)
    (⊦= (sort '(5 4 3 2 1) <) (timsort/primitive '(5 4 3 2 1)))
    #;(⊦= '(1.1 2.1 3.1 4.1 5.1) (timsort/primitive '(5.1 4.1 3.1 2.1 1.1)))
    (⊦= '(hello world) (timsort/primitive '(world hello))))

)

(unittest/✓ timsort-suite)