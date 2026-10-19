
(import
  scheme (chicken base) srfi-1
  (aux unittest) (aux base) (aux stream) (aux kanren micro) (aux kanren lambda))

; what the layer does not do, or does differently from λProlog: each row of DESIGN.md §11 pinned by a case.
(define stash #f)

(define-suite microkanren-lambda-limits-suite

  ((doc r) `((p "Known limits of (aux kanren lambda), each pinned so that a change of behaviour is noticed.")))

  ((test/limits/host-escape _)
   ; an eigenvariable smuggled out by the host (project° and set!) is policed against older variables, but a
   ; variable of a later scope at the same depth may still take it: levels guard logic variables only.
   (⊦= '() (°->list #f (fresh° (q) (∀° (x) (project° ((v x)) (begin (set! stash v) ✓°))) (λ (s) ((=° q stash) s)))))
   (⊦= '((λ () 'leaked)) (°->list #f (fresh° (q) (∀° (x) (project° ((v x)) (begin (set! stash v) ✓°)))
                                         (∀° (y) (fresh° (z) (=° z stash) (=° q 'leaked)))))))

  ((test/limits/no-huet _)
   ; equations outside L_λ are suspended, never enumerated: an answer may be conditional on residuals that
   ; are jointly unsatisfiable.
   (⊦= '((λ (α) (assert (≐ (@ α 'a) 'b)) (assert (≐ (@ α 'a) 'c)) α))
       (°->list #f (fresh° (q F) (=° q F) (=° (@ F 'a) 'b) (=° (@ F 'a) 'c)))))

  ((test/limits/untyped _)
   ; λ-tree application and list data are distinct: `(@ g a)` is not `(g a)`; η holds only at abstractions.
   (⊦= '() (°->list #f (fresh° (q) (=° (@ 'g 'a) '(g a)))))
   (⊦= 'g (μkanren-datum (ƛ (x) (@ 'g x)))))

  ((test/limits/closed-with-∀ _)
   ; the constraints that cannot be closed by an abstraction keep the eigenvariable, and print under ∀.
   (⊦= '((λ (α) (∀ (x) (assert (every (μ v (symbol? v)) (list (@ α x))))) α))
       (°->list #f (fresh° (q F) (=° q F) (∀° (x) (symbol° (@ F x))))))
   (⊦= '((ƛ (z) k)) (°->list/datum (fresh° (q F) (=° q F) (∀° (x) (symbol° (@ F x))) (=° F (ƛ (z) 'k)))))
   (⊦= '((λ (α) (∀ (x) (assert (absent? x (@ α x)))) α)) (°->list #f (fresh° (q F) (=° q F) (∀° (x) (absent° x (@ F x))))))
   (⊦= '() (°->list #f (fresh° (q F) (=° q F) (∀° (x) (absent° x (@ F x))) (=° F (ƛ (z) z))))))

  ((test/limits/absent-projection _)
   ; an absento on a flexible term is kept whole in an answer, even when one of its arguments is out of
   ; the answer, which then prints as a working variable: c ∉ α Y for some Y is no absento on α alone
   ; (α := λz. z c with Y := λv.b satisfies it). With its head out of the answer, it is dropped.
   (⊦= '((λ (α) (assert (absent? 'c (@ α _1))) α)) (°->list #f (fresh° (q) (fresh° (Y) (absent° 'c (@ q Y))))))
   (⊦= '((λ (α) α)) (°->list #f (fresh° (q) (fresh° (F) (absent° 'c (@ F q)))))))

  ((test/limits/project _)
   ; project° sees β-normal values, but they are records: host code must use the term API.
   (⊦= '((saw (g a))) (°->list/datum (fresh° (q F) (=° F (ƛ (z) `(g ,z))) (project° ((v (@ F 'a))) (=° q (list 'saw v))))))
   (⊦= '(#t) (°->list/datum (fresh° (q F) (=° F (ƛ (z) z)) (project° ((v F)) (=° q (μkanren-lam? v))))))))

(unittest/✓ microkanren-lambda-limits-suite)
