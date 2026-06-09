
(import (aux unittest) (aux base) (aux stream) (aux category list) (aux category monad list) (aux category monad plus list))

(define-suite category-list-suite

  ((doc r) `((structure/section "Introduction")
             (p "This section tests the list monad, which is a monad that represents non-deterministic computations.  A value of type (list a) represents a non-deterministic computation that can produce any of the values in the list.  The monad operations allow us to combine these computations in various ways, such as sequencing them with >>= or combining them with ⊕/monad.")

             (structure/section "Implementation")

             (p "The list monad is implemented as follows, which defines the monad operations for lists.  The return operation creates a singleton list, and the >>= operation uses append-map to apply a function to each element of the list and concatenate the results.  The ⊕/monad operation is defined as append, which combines two lists of computations.")

             (code/scheme/file "../aux.category.list.scm")))

  ((test/empty _)
      (⊦= '() (do/monad ))
      `(doc "Empty list is the identity of list monad."))

  ((test/return _)
      (⊦= '(4) (do/monad ,4))
      `(doc "Return lifts a value into the list monad, which creates a singleton list."))

  ((test/two-returns _)
      (⊦= '(2) 
      (do/monad
            (← x ,1)
            (let y x)
            ,(+ x y)))
      `(doc "Multiple return statements in a do/monad block are implicitly combined with >>=, so the above is equivalent to "
      (code "(return 1) >>= (λ x (let y x) (return (+ x y))).")))

  ((test/return/⊕ _)
      (⊦= '(((1 4) 5 -3)
           ((1 5) 6 -4)
           ((1 6) 7 -5)
           ((2 4) 6 -2)
           ((2 5) 7 -3)
           ((2 6) 8 -4)
           ((3 4) 7 -1)
           ((3 5) 8 -2)
           ((3 6) 9 -3)) 
      (do/monad
            (← x (return/⊕ 1 2 3))
            (← y (return/⊕ 4 5 6))
            (return/⊕ `((,x ,y) ,(+ x y) ,(- x y)))))
      `(doc "The " (code return/⊕) " function lifts a value into the list monad, but it also allows you to specify multiple values to lift, "
            "which are combined with ⊕/monad (which is append for lists).  So the above is equivalent to "
            (code "(return/⊕ 1 2 3) >>= (λ x (return/⊕ 4 5 6) >>= (λ y (return/⊕ (+ x y) (- x y)))).")))

  ((test/fmap _)
      (⊦= '(2 3 4) (map/monad add1 (return/⊕ 1 2 3)))
      `(doc "The " (code map/monad) " function is the functor operation for the list monad, which applies a function to each value in the list.  So the above is equivalent to "
      (code "(map add1 (return/⊕ 1 2 3)).")))

  ((test/<*> _)
      (⊦= '(2 3 4 0 1 2) (<*> (return/⊕ add1 sub1) (return/⊕ 1 2 3)))
      `(doc "The " (code <*>) " function is the applicative functor operation.  It takes a list of functions and a list of values, and applies each function to each value, returning the combined results.  So the above is equivalent to "))

  ((test/lift/monad _)
      (⊦= '(5 6 7 6 7 8 7 8 9) (lift/monad + (return/⊕ 1 2 3) (return/⊕ 4 5 6)))
      `(doc "The " (code lift/monad) " function is a convenient way to lift a multi-argument function into the monad.  It takes a function and multiple monadic values, and applies the function to the values inside the monad.  So the above is equivalent to "
      (code "(return/⊕ 1 2 3) >>= (λ x (return/⊕ 4 5 6) >>= (λ y (return (+ x y))))).")))


  ((test/powerset _)
      (⊦= '(() (5) (4) (4 5) (3) (3 5) (3 4) (3 4 5) (2) (2 5) (2 4) (2 4 5) (2 3) (2 3 5) (2 3 4) (2 3 4 5) (1) (1 5) (1 4) (1 4 5) (1 3) (1 3 5) (1 3 4) (1 3 4 5) (1 2) (1 2 5) (1 2 4) (1 2 4 5) (1 2 3) (1 2 3 5) (1 2 3 4) (1 2 3 4 5)) (powerset/monad '(1 2 3 4 5)))
      `(doc "The " (code powerset/monad) " function generates the powerset of a list, which is the set of all subsets of the list.  It does this by using " (code filter/monad) " to filter the list with a predicate that non-deterministically returns true or false for each element, effectively including or excluding each element in the subsets.  So the above is equivalent to "
      (code "(filterM (λ (_) (return/⊕ #f #t)) '(1 2 3 4 5)).")))
)

(unittest/✓ category-list-suite)