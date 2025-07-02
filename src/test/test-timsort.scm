
(import unittest aux scheme (chicken sort) (chicken syntax) srfi-1)

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

  ((test/already-sorted _)
    (let* ((n 10000000)
           (r (iota n)))
      (⊦= r (timsort r))))

  ((test/already-sorted/sort _)
    (let* ((n 10000000)
           (r (iota n)))
      (⊦= r (sort r <))))

  ((test/iota _)
    (let1 (n 1000000)
      (⊦= (iota n) (timsort (iota n (sub1 n) -1)))))

  ((test/iota/sort _)
    (let1 (n 1000000)
      (⊦= (iota n) (sort (iota n (sub1 n) -1) <))))

  ((test/timtros/iota _)
    (let1 (n 1000000)
      (⊦= (iota n (sub1 n) -1) (timtros (iota n)))))

  ((test/tros/iota _)
    (let1 (n 1000000)
      (⊦= (iota n (sub1 n) -1) (reverse (sort (iota n) <)))))

)

(unittest/✓ timsort-suite)