(import (aux unittest) (aux base) (aux category ziplist) (aux category monad ziplist))

(define-suite category-ziplist-suite

  ((doc r) `((structure/section "Introduction")
             (p "This section tests the ziplist monad, whose bind zips rather than nests: the i-th element of "
                (code "(>>= m g)") " is the i-th element of " (code "(g v)") ", where " (code "v") " is the i-th element of " (code "m") ".  "
                "A pure value is the circular list that repeats it, so it zips with lists of any length.")

             (structure/section "Implementation")

             (code/scheme/file "../aux.category.ziplist.scm")))

  ((test/diagonal _)
      (⊦= '((1 a) (2 b)) (do/monad (← x '(1 2 3)) (← y '(a b)) ,(list x y)))
      (⊦= '(2 1 2) (<*> (list add1 sub1 sub1) (list 1 2 3)))
      `(doc "Binding zips the lists, truncating at the shortest one."))

  ((test/false-elements _)
      (⊦= '(#f #t #f) (do/monad (← x '(1 2 3)) ,(even? x)))
      (⊦= '(#f #f) (>>= '(1 2) (λ (v) (list #f #f))))
      `(doc "A " (code "#f") " element is a value like any other, so predicates zip too."))
)

(unittest/✓ category-ziplist-suite)
