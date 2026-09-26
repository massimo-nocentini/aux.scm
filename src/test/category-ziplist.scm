(import (aux unittest) (aux base) (aux category ziplist) (aux category monad ziplist))

(define-suite category-ziplist-suite

  ((doc r) `((structure/section "Introduction")
             (p "This section tests the ziplist monad, whose bind zips rather than nests: the i-th element of "
                (code "(>>= m g)") " is the i-th element of " (code "(g v)") ", where " (code "v") " is the i-th element of " (code "m") ".  "
                "A pure value is the circular list that repeats it, so it zips with lists of any length.")

             (structure/section "Implementation")

             (code/scheme/file "../aux.category.ziplist.scm")))

  ((test/pure _)
      (let ((l (do/monad (← x ,3) (← y ,4) (← z ,5) ,(+ x y z))))
        (⊨ (eq? l (cdr l)))
        (⊦= 12 (car l))
        (⊦= '(12 13 14) (do/monad (← x l) (← y '(0 1 2)) ,(+ x y))))
      `(doc "A pure value repeats forever as a one-element circular list, so " (code write) " never returns on it; "
            "zipping it with a finite list gives a finite result."))

  ((test/diagonal _)
      (⊦= '((1 a) (2 b)) (do/monad (← x '(1 2 3)) (← y '(a b)) ,(list x y)))
      (⊦= '(2 1 2) (<*> (list add1 sub1 sub1) (list 1 2 3)))
      `(doc "Binding zips the lists, truncating at the shortest one."))

  ((test/false-elements _)
      (⊦= '(#f #t #f) (do/monad (← x '(1 2 3)) ,(even? x)))
      (⊦= '(#f #f) (>>= '(1 2) (λ (v) (list #f #f))))
      `(doc "A " (code "#f") " element is a value like any other, so predicates zip too."))

  ((test/truncation _)
      (⊦= '(x) (>>= '(1 2 3) (λ (v) (if (= v 2) '(a) '(x y z w)))))
      (⊦= '((1 a c d) (2 b c* e)) (do/monad (← x '(1 2 3)) (← y '(a b)) (← z '(c c*)) (← r '(d e f g)) ,(list x y z r)))
      (⊦= '(1) (do/monad (← (,a . ,b) '((1 . 2) 3 (4 . 5))) ,a))
      `(doc "The diagonal stops at the first inner list too short to reach it, rather than skipping it and sliding the later elements out of their index; "
            "a value that does not match the pattern of a " (code ←) " clause goes to " (code fail) ", the empty list, so the diagonal stops there too."))

  ((test/associativity _)
      (let* ((table '#((1 2 0) (2) (0 1 2 1) (3 0 3 2 1)))
             (f (λ (v) (vector-ref table v)))
             (g (λ (v) (reverse (vector-ref table v)))))
        (for-each (λ (m)
                    (let ((lhs (>>= (>>= m f) g))
                          (rhs (>>= m (λ (x) (>>= (f x) g)))))
                      ; `map` stops at the shorter list, so this compares the common prefix
                      (⊦= (map (λ (l r) l) lhs rhs) (map (λ (l r) r) lhs rhs))))
                  '((0 1 2 3) (2 0 1) (0 2 0 1) (1 0 2) (3 3 3 0) (2 2 2) (3 0)))
        (⊦= '(1 2 3) (>>= (>>= '(3 3 3) f) g))
        (⊦= '(1 2 3) (>>= '(3 3 3) (λ (x) (>>= (f x) g)))))
      `(doc "Both sides of the associativity law agree index by index.  On ragged inner lists one side may stop earlier: "
            "the inner bind on the right truncates at the first index any of its inner lists misses, even when the left side never "
            "looks at that index, which is why finite zip lists admit no lawful monad (Haskell's ZipList is only an Applicative)."))
)

(unittest/✓ category-ziplist-suite)
