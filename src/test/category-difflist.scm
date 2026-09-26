(import (aux unittest) (aux base) (aux category difflist) (aux category monad difflist))

(define-suite category-difflist-suite

  ((doc r) `((structure/section "Introduction")
             (p "This section tests difference lists: a difflist is a function that prepends its content to the list it receives, "
                "so appending two of them is function composition, in constant time, and " (code difflist->list) " applies the result to " (code "'()") ".")

             (structure/section "Implementation")

             (code/scheme/file "../aux.category.difflist.scm")))

  ((test/monoid _)
      (⊦= '(1 2 3 4 5) (difflist->list (mappend (list->difflist '(1 2 3)) (list->difflist '(4 5)))))
      (⊦= '(1 2) (difflist->list (mappend mempty (list->difflist '(1 2)))))
      (⊦= '(1 2) (difflist->list (mappend (list->difflist '(1 2)) mempty)))
      `(doc "Appending is composition, and " (code mempty) " is the identity."))

  ((test/fail _)
      (⊦= '() (difflist->list (fail)))
      (⊦= '(1 2) (difflist->list (mappend (fail) (list->difflist '(1 2)))))
      (⊦= '(1 3) (difflist->list (>>= (list->difflist '(1 2 3)) (λ (x) (if (= x 2) (fail) (return x))))))
      (⊦= '(1 3 9 9) (difflist->list (mappend (>>= (list->difflist '(1 2 3)) (λ (x) (if (= x 2) (fail) (return x))))
                                              (list->difflist '(9 9)))))
      `(doc "A failure is the empty difflist, so it drops only its own branch, as " (code "'()") " does in the list monad."))
)

(unittest/✓ category-difflist-suite)
