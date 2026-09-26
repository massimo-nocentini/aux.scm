
(import (chicken condition) (aux unittest) (aux base) (aux category maybe) (aux category monad maybe))

(define-suite category-maybe-suite

  ((doc r) `((structure/section "Introduction")
             (p "This section tests the maybe monad, where " (code "(just . v)") " is a computation that succeeded with the value " (code v) " and the empty list is a computation that failed, short-circuiting every later bind.")

             (structure/section "Implementation")

             (code/scheme/file "../aux.category.maybe.scm")))

  ((test/bind _)
      (⊦= '(just . 3) (do/monad (← x ,1) (← y (return 2)) ,(+ x y)))
      (⊦= '() (do/monad (← x ,1) (← y (fail)) ,(+ x y)))
      `(doc "A failure anywhere in the block makes the whole block fail."))

  ((test/return _)
      (let ((arity-error? (λ args (condition-case (begin (apply return args) #f) (e (exn arity) #t)))))
        (⊨ (arity-error? 1 2 3))
        (⊨ (arity-error?)))
      `(doc (code return) " takes exactly one value, like in every other monad instance."))

)

(unittest/✓ category-maybe-suite)
