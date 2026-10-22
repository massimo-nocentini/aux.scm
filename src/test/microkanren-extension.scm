; The dispatch of the hooks of (aux kanren micro) to a registered extension. The registry is global and an
; extension cannot be taken back, so this suite runs in its own process: the other suites of micro register none.

(import scheme (chicken base) srfi-1 (aux unittest) (aux base) (aux kanren micro))

; an extension with a `post` hook only: every binding to `(twin u v)` that =° makes unifies u with v too, as an
; extension wakes a constraint suspended on the variable bound. The bindings it adds must have their constraints
; handed on (E3), which `μkanren-post-unify` does after the hooks.
(define (twin-post s assocs)
  (fold (λ (a r)
          (let1 (v (rhs a))
            (if (and r (pair? v) (eq? (car v) 'twin)) (μkanren-unify (cadr v) (caddr v) (car r) (cadr r)) r)))
        (list s assocs)
        assocs))

; registered twice under the same name, the second replaces the first, which would fail every binding.
(μkanren-extension-register! (make-μkanren-extension 'twin post: (λ (s assocs) #f)))
(μkanren-extension-register! (make-μkanren-extension 'twin post: twin-post))

(define-suite microkanren-extension-suite

  ((test/extension/post _)
    (⊦= '((λ () (cons 'twin (cons 'k (cons 'k '())))))
        (°->list #f (fresh° (q) (fresh° (y) (=° q (list 'twin y 'k))))))
    (⊦= '((λ () 1)) (°->list #f (fresh° (q) (=° q 1))))
    (⊦= '((λ () (cons 'twin (cons 'j (cons 'j '())))))
        (°->list #f (fresh° (q) (fresh° (y) (≠° y 'k) (symbol° y) (=° q (list 'twin y 'j))))))
    ; a binding by the hook that fails fails the =°.
    (⊦= '() (°->list #f (fresh° (q) (=° q (list 'twin 1 2))))))

  ; the constraints of y, which only the hook binds, are handed on after it, one store each.
  ((test/extension/post/D _)
    (⊦= '() (°->list #f (fresh° (q) (fresh° (y) (≠° y 'k) (=° q (list 'twin y 'k)))))))

  ((test/extension/post/A _)
    (⊦= '() (°->list #f (fresh° (q) (fresh° (y) (symbol° y) (=° q (list 'twin y 1)))))))

  ((test/extension/post/T _)
    (⊦= '() (°->list #f (fresh° (q) (fresh° (y) (absent° 'a y) (=° q (list 'twin y '(a))))))))

)

(unittest/✓ microkanren-extension-suite)
