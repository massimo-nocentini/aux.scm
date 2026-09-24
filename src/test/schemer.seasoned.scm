(import (aux unittest) (aux base) (aux continuation) (aux continuation classic) (aux schemer seasoned) (chicken port))

(define-suite seasoned-schemer-suite

  ((doc r) `((structure/section "The Seasoned Schemer")
             (p "Ported from on-scheme's " (code/inline "seasoned-schemer.scm")
                ", after " (cite/a "https://mitpress.mit.edu/9780262561006/the-seasoned-schemer/"
                                   "The Seasoned Schemer") ".")
             (structure/section "Implementation")
             (code/scheme/file "../aux.schemer.seasoned.scm")))

  ((test/multi-insert*&co _)
   (let ((sexp '((((orange fish)) apple anchovy) apple (orange)))
         (sexp-expected '((((orange tuna fish)) apple tuna anchovy) apple (orange tuna)))
         (identity-abridged (λ args args)))
     ; should be tail-recursive by Chicken Scheme
     (⊦= `(,sexp-expected 1 2)
         (multi-insert*&co 'tuna 'anchovy 'orange sexp identity-abridged))
     ; use `letcc` to return abruptly in the collector if no TCO is performed
     (⊦= `(,sexp-expected 1 2)
         (letcc hop (multi-insert*&co 'tuna 'anchovy 'orange sexp (λ args (hop args)))))))

  ((test/two-in-a-row? _)
   ; natural unwind of recursion stack
   (⊦= #f (two-in-a-row? '()))
   (⊦= #f (two-in-a-row? '(j f r e k s i)))
   (⊦= #t (two-in-a-row? '(j f r e k s s)))
   ; discard recursion stack hopping with letcc
   (⊦= #f (two-in-a-row?&hop '()))
   (⊦= #f (two-in-a-row?&hop '(j f r e k s i)))
   (⊦= #t (two-in-a-row?&hop '(j f r e k s s))))

  ; the original nested group "intersect", a dependency of `intersect+all`.
  ((test/intersect _)
   (⊦= '() (intersect '(1 2 3) '()))
   (⊦= '(2 3) (intersect '(1 2 3) '(2 3 4))))

  ((test/intersect+all _)
   (⊦= '(3) (intersect+all '((3 mango and) (3 kiwis and) (3 hamburgers))))
   ; empty intersection, empty set present
   (⊦= '() (intersect+all '((3 steaks and) (no food and) () (3 diet hamburgers))))
   ; empty intersection, empty set present in last position
   (⊦= '() (intersect+all '((3 steaks and) (no food and) (3 diet hamburgers) ())))
   ; empty intersection
   (⊦= '() (intersect+all '((3 steaks and) (no food and) (three baked potatoes) (3 diet hamburgers)))))

  ((test/comb-upto-last _)
   ; rember-upto-last, three atom occurrences
   (⊦= '(e f) (comb-upto-last 'a -1 '(a b a d a e f)))
   ; rember-upto-last, no atom occurrences
   (⊦= '(a b c d a e f) (comb-upto-last 'r -1 '(a b c d a e f)))
   ; empty prefix when atom is in `car` position
   (⊦= '() (comb-upto-last 'a 1 '(a b c d a e f a g h)))
   ; sublist between the start and first atom occurrences
   (⊦= '(b c d) (comb-upto-last 'a 1 '(b c d a e f a g h)))
   ; sublist between first and second atom occurrences
   (⊦= '(e f) (comb-upto-last 'a 2 '(b c d a e f a g h)))
   ; suffix as done by `rember-upto-last`
   (⊦= '(g h) (comb-upto-last 'a 3 '(b c d a e f a g h)))
   ; suffix as done by `rember-upto-last`, again
   (⊦= '(g h) (comb-upto-last 'a 50 '(b c d a e f a g h))))

  ((test/leftmost _)
   (let1 (leftmost leftmost/escape)
     ; there is at least one atom
     (⊦= 'a (leftmost '(((a)) b (c))))
     ; no atom in the very first car sexp
     (⊦= 'b (leftmost '((() ((() (())))) b (c))))
     ; no atom at all
     (let1 (sexp (quote ((() ((() (())))) (((() ()))) ())))
       (⊦= sexp (leftmost sexp)))))

  ((test/rember1* _)
   ; `rember1*/try` prints its skip reasons, collected here to keep the log clean.
   (let1 (R (λ args (let1 (result #f)
                      (with-output-to-string (τ (set! result (apply rember1*/try args))))
                      result)))
     ; atom present
     (⊦= '((delicious) (food)) (R 'more '((delicious) more (food))))
     ; atom not present
     (⊦= '((orange) more (fruits)) (R 'pizza '((orange) more (fruits))))))

  ; New cases: the leftmost ones on all four variants.
  ((test/leftmost/variants _)
   (let1 (sexp (quote ((() ((() (())))) (((() ()))) ())))
     (for-each (λ (leftmost)
                 (⊦= 'a (leftmost '(((a)) b (c))))
                 (⊦= 'b (leftmost '((() ((() (())))) b (c))))
                 (⊦= sexp (leftmost sexp)))
               (list leftmost/awkward leftmost/awkward+letcc leftmost/escape+explicit leftmost/escape))))

  ; New cases: rember1* on both variants, plus the printed skip reasons.
  ((test/rember1*/variants _)
   (⊦= '((delicious) (food)) (rember1*/letcc 'more '((delicious) more (food))))
   (⊦= '((orange) more (fruits)) (rember1*/letcc 'pizza '((orange) more (fruits))))
   (⊦= '((a) x) (rember1*/letcc 'x '((a x) x)))
   (⊦= '((a) x) (rember1*/try 'x '((a x) x)))
   (⊦= "(more not present in (delicious))"
       (with-output-to-string (τ (rember1*/try 'more '((delicious) more (food))))))
   (⊦= (string-append "(pizza not present in (orange))"
                      "(pizza not present in (fruits))"
                      "(pizza not present in ((orange) more (fruits)))"
                      "((pizza not present in ((orange) more (fruits))))")
       (with-output-to-string (τ (rember1*/try 'pizza '((orange) more (fruits)))))))

  ; New cases: `intersect/letrec` (the original `intersect-old`) and the edges of `intersect+all`.
  ((test/intersect/letrec _)
   (⊦= '() (intersect/letrec '(1 2 3) '()))
   (⊦= '(2 3) (intersect/letrec '(1 2 3) '(2 3 4)))
   (⊦= '() (intersect+all '()))
   (⊦= '(a b) (intersect+all '((a b))))
   (⊦= '() (intersect+all '(() ()))))

  )

(unittest/✓ seasoned-schemer-suite)
