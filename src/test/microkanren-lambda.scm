
(import
  scheme (chicken base) (chicken sort) (chicken string) (chicken condition) (chicken memory representation)
  srfi-1
  (aux unittest) (aux base) (aux stream) (aux kanren micro) (aux kanren lambda))

(include "microkanren-lambda-programs.scm")

; a sorted copy of the answers, to compare a set of answers whose order interleaving decides.
(define (answers-sorted l) (sort l (λ (a b) (string<? (->string a) (->string b)))))

; the message of the error that `thunk` raises, or #f.
(define (error-message thunk)
  (handle-exceptions exn ((condition-property-accessor 'exn 'message #f) exn) (begin (thunk) #f)))

(define (permutations l) (if (null? l) '(()) (append-map (λ (x) (map (λ (p) (cons x p)) (permutations (delete x l)))) l)))

; the distinct `(answers g)` over every order of the goals `g ...`, which share the variables `v ...`.
(define-syntax in-every-order
  (syntax-rules ()
    ((_ answers (v ...) g ...)
     (delete-duplicates
       (map (λ (p) (answers (fresh° (v ...) (let1 (gs (list g ...)) (fold-right andª ✓° (map (λ (i) (list-ref gs i)) p))))))
            (permutations (iota (length '(g ...)))))))))

(define (answers/datum g) (answers-sorted (°->list/datum g)))

; `l` as a list of `n` fresh pairs, ending in `tail`.
(define (chain° l n tail) (if (zero? n) (=° l tail) (fresh° (a d) (=° l (cons a d)) (chain° d (sub1 n) tail))))

(define-suite microkanren-lambda-suite

  ((doc r) `((p "λProlog's universal and hypothetical goals, λ-tree syntax and higher-order pattern unification "
                "over (aux kanren micro), after Miller & Nadathur, Programming with Higher-Order Logic (CUP 2012). "
                "Page numbers refer to the book.")))

  ;; A1 ∀ scope ---------------------------------------------------------------------------------------

  ((test/A1/exists-forall _)
   ; ∃Y∀x. Y = x is not provable (p.101): the older Y may not take the eigenvariable.
   (⊦= '() (°->list #f (fresh° (q) (∀° (x) (=° q x)))))
   (⊦= '() (°->list #f (fresh° (q) (∀° (x) (=° q `(f ,x)))))))

  ((test/A1/forall-exists _)
   (⊦= '((λ () 'ok)) (°->list #f (fresh° (q) (∀° (x) (fresh° (y) (=° y x))) (=° q 'ok)))))

  ((test/A1/forall-forall _)
   (⊦= '() (°->list #f (fresh° (q) (∀° (x y) (=° x y)))))
   (⊦= '((λ () 'ok)) (°->list #f (fresh° (q) (∀° (x) (=° x x)) (=° q 'ok)))))

  ((test/A1/alias _)
   ; the escape through a younger variable is caught whatever the order of the bindings and their kind.
   (⊦= '() (°->list #f (fresh° (q) (∀° (x) (fresh° (y) (=° q (list y)) (=° y x))))))
   (⊦= '() (°->list #f (fresh° (q) (∀° (x) (fresh° (y) (=° y x) (=° q (list y)))))))
   (⊦= '() (°->list #f (fresh° (q) (∀° (x) (fresh° (y z) (=° q (list y)) (=° y z) (=° z x))))))
   (⊦= '() (°->list #f (fresh° (q) (∀° (x) (fresh° (y) (=° y q) (=° y x))))))
   (⊦= '() (°->list #f (fresh° (q) (∀° (x) (=° (vector q) (vector x)))))))

  ((test/A1/alias-under-binder _)
   ; q := λz. k (g z), g younger than c: g is lowered to q's level, so it cannot become λw.c later.
   (⊦= '() (°->list #f (fresh° (q) (∀° (c) (fresh° (g) (=° q (ƛ (z) `(k ,(@ g z)))) (=° g (ƛ (w) c)))))))
   (⊦= '((ƛ (z) (k (z . z)))) (°->list/datum (fresh° (q) (∀° (c) (fresh° (g) (=° q (ƛ (z) `(k ,(@ g z)))) (=° g (ƛ (w) (cons w w)))))))))

  ((test/A1/library-relations _)
   ; micro's own relations are scope-safe: every binding of `=°` goes through the scope check.
   (⊦= '() (°->list #f (fresh° (q) (∀° (x) (cons° x '() q)))))
   (⊦= '((λ () (cons 'a (quote ())))) (°->list #f (fresh° (q) (∀° (x) (cons° 'a '() q))))))

  ((test/A1/unrelated _)
   ; an answer found inside ∀ keeps no trace of the eigenvariable.
   (⊦= '((λ (α) (cons 'f (cons α '())))) (°->list #f (fresh° (q) (∀° (x) (fresh° (z) (=° q `(f ,z)))))))
   (⊦= '((λ () 'ok)) (°->list #f (fresh° (q) (∀° (x) (fresh° (z) (=° z x) (=° q 'ok)))))))

  ((test/A1/levels-reach-clauses _)
   ; Nadathur, Jayaraman & Kwon 1995 p.16: with ∀z(q z ⊃ p (d z)), the goal ∃x∀y(q y ⊃ p x) must fail: the
   ; clause variable Z bound inside X is lowered to X's level, so it cannot take y.
   (define-dynamic-relation (q° z))
   (define-dynamic-relation (p° x) (fresh° (z) (=° x `(d ,z)) (q° z)))
   (⊦= '() (°->list #f (fresh° (x) (∀° (y) (⊃° ((q° y)) (p° x))))))
   (⊦= '((λ () 'ok)) (°->list #f (fresh° (q) (∀° (y) (fresh° (x) (⊃° ((q° y)) (p° x)))) (=° q 'ok)))))

  ((test/A1/hypotheses _)
   ; neither direction of a hypothetical clause lets an eigenvariable out.
   (define-dynamic-relation (h° x))
   (⊦= '() (°->list #f (fresh° (q) (∀° (x) (⊃° ((h° x)) (h° q))))))
   (⊦= '() (°->list #f (fresh° (q) (∀° (x) (⊃° ((h° q)) (h° x))))))
   (⊦= '() (°->list #f (fresh° (q) (⊃° (((h° q) ← ✓°)) (∀° (x) (h° x))))))
   (⊦= '(ok) (°->list/datum (fresh° (q) (∀° (x) (⊃° ((h° x)) (fresh° (y) (h° y) (=° q 'ok))))))))

  ;; A2 ⊃ hypothetical clauses ------------------------------------------------------------------------

  ((test/A2/scope _)
   (define-dynamic-relation (h° x))
   (⊦= '(a) (°->list/datum (fresh° (q) (⊃° ((h° 'a)) (h° q)))))
   (⊦= '() (°->list/datum (fresh° (q) (⊃° ((h° 'a)) ✓°) (h° q))))
   (⊦= '() (°->list/datum (fresh° (q) (⊃° ((h° 'a) (h° 'b)) (h° q)) (fresh° (z) (h° z))))))

  ((test/A2/order _)
   ; p.94: `p 2 => p 3 => p X` gives 3, 2, 1 and `(p 2 & p 3) => p X` gives 2, 3, 1, the program being `p 1`.
   (define-dynamic-relation (p° x) (=° x 1))
   (⊦= '(3 2 1) (°->list/datum (fresh° (q) (⊃° ((p° 2)) (⊃° ((p° 3)) (p° q))))))
   (⊦= '(2 3 1) (°->list/datum (fresh° (q) (⊃° ((p° 2) (p° 3)) (p° q))))))

  ((test/A2/branches _)
   (define-dynamic-relation (h° x))
   (⊦= '(a) (°->list/datum (fresh° (q) (or° (⊃° ((h° 'a)) (h° q)) (h° q)))))
   (⊦= '(a b) (°->list/datum (fresh° (q) (or° (⊃° ((h° 'a)) (h° q)) (⊃° ((h° 'b)) (h° q)))))))

  ((test/A2/shared-and-renamed _)
   (define-dynamic-relation (h° x))
   ; the free variable y of the clause is the context's: bound by the first use, seen by the query.
   (⊦= '(7) (°->list/datum (fresh° (q y) (⊃° ((h° y)) (fresh° (u) (h° u) (=° u 7))) (=° q y))))
   (⊦= '() (°->list/datum (fresh° (q y) (⊃° ((h° `(,y ,y))) (h° '(1 1)) (h° '(2 2))))))
   ; the clause's own variable z is renamed at each use, in either clause syntax.
   (⊦= '(ok) (°->list/datum (fresh° (q) (⊃° ((∀ (z) (h° `(,z ,z)))) (h° '(1 1)) (h° '(2 2))) (=° q 'ok))))
   (⊦= '((1 1)) (°->list/datum (fresh° (q) (⊃° ((∀ (z) ((h° `(,z ,z)) ← (=° z 1)))) (fresh° (w) (h° w) (=° q w))))))
   (⊦= '((1 1)) (°->list/datum (fresh° (q) (⊃° ((∀ (z) (← (h° `(,z ,z)) (=° z 1)))) (fresh° (w) (h° w) (=° q w)))))))

  ((test/A2/static-relations _)
   ; only a dynamic relation takes hypotheses; assuming a clause of any other one is an error, not a no-op.
   (define-relation (r° x) (=° x 1))
   (⊦= "not a dynamic relation, see define-dynamic-relation"
       (error-message (λ () (°->list #f (fresh° (q) (⊃° ((r° 2)) (r° q))))))))

  ((test/A2/courses _)
   ; §3.2.2, Figure 3.1, the book's answers (p.96-97).
   (⊦= '(210) (°->list/datum (fresh° (q) (⊃° ((fact° `(finished dana ,q))) (false°)))))
   (⊦= '((dana 301)) (°->list/datum (fresh° (q x y) (=° q (list x y))
                                      (⊃° ((fact° `(finished ,x ,y))) (fact° `(graduates ,x)) (fact° `(cs_major ,x))))))
   ; the book shows the first answer, 250 and 301; both orders are proofs, each found twice.
   (⊦= '((250 301) (250 301) (301 250) (301 250))
       (answers-sorted (°->list/datum (fresh° (q x y) (=° q (list x y))
                                        (⊃° ((fact° `(finished kim ,x)))
                                          (⊃° ((fact° `(finished kim ,y)))
                                            (fact° '(graduates kim)) (fact° '(cs_major kim))))))))
   (⊦= '(solved) (°->list/datum (fresh° (q) (⊃° ((fact° '(finished kim 250))) (⊃° ((fact° '(finished kim 301))) (false°))) (=° q 'solved))))
   (⊦= '() (°->list/datum (fresh° (q) (fact° '(graduates dana)) (=° q 'yes)))))

  ((test/A2/jars _)
   ; p.98-99: sterile J :- pi x\ bug x => in x J => dead x.
   (define-dynamic-relation (bug° b))
   (define-dynamic-relation (in° b j))
   (define-relation (heated° j) (=° j 'j))
   (define-dynamic-relation (dead° b) (fresh° (j) (heated° j) (in° b j) (bug° b)))
   (define-relation (sterile° j) (∀° (x) (⊃° ((bug° x)) (⊃° ((in° x j)) (dead° x)))))
   (⊦= '(j) (°->list/datum (fresh° (q) (sterile° q)))))

  ((test/A2/reverse _)
   ; Figure 3.4: the clause `rev nil K` shares K with the goal, the other one has its own X, L, K.
   (define-dynamic-relation (rev° l k))
   (define-relation (reverse° l k)
     (⊃° ((rev° '() k) (∀ (X L K) ((rev° `(,X . ,L) K) ← (rev° L `(,X . ,K))))) (rev° l '())))
   (⊦= '((3 2 1)) (°->list/datum (fresh° (q) (reverse° '(1 2 3) q))))
   (⊦= '((3 2 1)) (°->list/datum (take° 1 (fresh° (q) (reverse° q '(1 2 3)))))))

  ;; A3 ∀ and ⊃ together: type inference -----------------------------------------------------------

  ((test/A3/typeof _)
   (⊦= '((arr α α)) (°->list/datum (fresh° (q) (typeof° `(abs ,(ƛ (x) x)) q))))
   (⊦= '((arr α (arr β α))) (°->list/datum (fresh° (q) (typeof° `(abs ,(ƛ (x) `(abs ,(ƛ (y) x)))) q))))
   (⊦= '((arr (arr α α) (arr α α)))
       (°->list/datum (fresh° (q) (typeof° `(abs ,(ƛ (f) `(abs ,(ƛ (x) `(app ,f (app ,f ,x)))))) q))))
   ; p.207: S has type (α → β → γ) → (α → β) → (α → γ).
   (⊦= '((arr (arr α (arr β γ)) (arr (arr α β) (arr α γ))))
       (°->list/datum (fresh° (q) (typeof° `(abs ,(ƛ (x) `(abs ,(ƛ (y) `(abs ,(ƛ (z) `(app (app ,x ,z) (app ,y ,z)))))))) q))))
   (⊦= '((λ (α) (cons 'arr (cons α (cons α '())))))
       (°->list #f (fresh° (q) (typeof° `(abs ,(ƛ (x) x)) q)))))

  ((test/A3/self-application _)
   ; λx.x x has no simple type: B = arr B A fails the occurs check.
   (⊦= '() (°->list/datum (fresh° (q) (typeof° `(abs ,(ƛ (x) `(app ,x ,x))) q)))))

  ((test/A3/backwards _)
   ; an unknown term: the search enumerates (term, type) pairs.
   (⊦= '(((abs (ƛ (x) x)) (arr α α))
         ((abs (ƛ (x) (abs (ƛ (x1) x1)))) (arr α (arr β β)))
         ((abs (ƛ (x) (abs (ƛ (x1) x)))) (arr α (arr β α)))
         ((app (abs (ƛ (x) x)) (abs (ƛ (x) x))) (arr α α)))
       (°->list/datum (take° 4 (fresh° (q m t) (=° q (list m t)) (typeof° m t)))))
   ; and the terms of a given type.
   (⊦= '((abs (ƛ (x) x)) (abs (ƛ (x) (app (abs (ƛ (x1) x1)) x))))
       (°->list/datum (take° 2 (fresh° (q) (typeof° q '(arr i i)))))))

  ;; A4 object-level substitution -----------------------------------------------------------------

  ((test/A4/cbn _)
   ; p.199: (λx λw.w) Ω evaluates by name to λw.w; K I (λj. j j) to I.
   (let* ((ω `(abs ,(ƛ (x) `(app ,x ,x)))) (Ω `(app ,ω ,ω)))
     (⊦= '((abs (ƛ (w) w))) (°->list/datum (fresh° (q) (cbn° `(app (abs ,(ƛ (x) `(abs ,(ƛ (w) w)))) ,Ω) q)))))
   (⊦= '((abs (ƛ (i) i)))
       (°->list/datum (fresh° (q) (cbn° `(app (app (abs ,(ƛ (x) `(abs ,(ƛ (y) x)))) (abs ,(ƛ (i) i))) (abs ,(ƛ (j) `(app ,j ,j)))) q)))))

  ((test/A4/beta _)
   ; subst R N (R N), the book's non-L_λ clause (p.241), used dynamically: R instantiated before, or later.
   (⊦= '((app (abs (ƛ (y) y)) (abs (ƛ (y) y))))
       (°->list/datum (fresh° (q r) (=° r (ƛ (x) `(app ,x ,x))) (=° q (@ r `(abs ,(ƛ (y) y)))))))
   (⊦= '((app (abs (ƛ (y) y)) (abs (ƛ (y) y))))
       (°->list/datum (fresh° (q r) (=° q (@ r `(abs ,(ƛ (y) y)))) (=° r (ƛ (x) `(app ,x ,x))))))
   ; β at construction: (λx.λy.x y) (λz.c z) is λy.c y, i.e. c.
   (⊦= 'c (μkanren-datum (@ (ƛ (x) (ƛ (y) (@ x y))) (ƛ (z) (@ 'c z))))))

  ((test/A4/copy _)
   ; p.215: copy (abs x\ abs y\ app y x) M gives M = abs v\ abs u\ app u v; the binders of M are made by
   ; the unifier, named after the eigenvariable x of copy's clause.
   (⊦= '((abs (ƛ (x) (abs (ƛ (x1) (app x1 x))))))
       (°->list/datum (fresh° (q) (copy° `(abs ,(ƛ (x) `(abs ,(ƛ (y) `(app ,y ,x))))) q))))
   (⊦= '((app (abs (ƛ (y) y)) (abs (ƛ (y) y))))
       (°->list/datum (fresh° (q) (subst° (ƛ (x) `(app ,x ,x)) `(abs ,(ƛ (y) y)) q)))))

  ((test/A4/subst-formulas _)
   (⊦= '((and (p (f a)) (all (ƛ (y) (q (f a) y)))))
       (°->list/datum (fresh° (q) (substform° (ƛ (x) `(and (p ,x) (all ,(ƛ (y) `(q ,x ,y))))) '(f a) q))))
   ; p.219, existential generalization: the book's four answers, in its order.
   (⊦= '((ƛ (x) (and (p x) (q a x)))
         (ƛ (x) (and (p x) (q a (f a))))
         (ƛ (x) (and (p (f a)) (q a x)))
         (ƛ (x) (and (p (f a)) (q a (f a)))))
       (°->list/datum (fresh° (q) (substform° q '(f a) '(and (p (f a)) (q a (f a))))))))

  ;; A5 L_λ pattern unification ------------------------------------------------------------------

  ((test/A5/flex-rigid _)
   ; F x = g x x gives F = λx.g x x; λx.F x = λx.c gives F = λx.c.
   (⊦= '((ƛ (x) (g x x))) (°->list/datum (fresh° (q) (∀° (x) (=° (@ q x) `(g ,x ,x))))))
   (⊦= '((ƛ (x) c)) (°->list/datum (fresh° (q) (=° (ƛ (x) (@ q x)) (ƛ (x) 'c)))))
   (⊦= '((ƛ (y x) (g y))) (°->list/datum (fresh° (q) (=° (ƛ (x y) (@ q y x)) (ƛ (a b) `(g ,b)))))))

  ((test/A5/flex-flex _)
   ; F x y = G y x, and the same head F x y = F y x (p.238).
   (⊦= '(((ƛ (x y) (@ α y x)) α))
       (°->list/datum (fresh° (q f g) (=° q (list f g)) (∀° (x y) (=° (@ f x y) (@ g y x))))))
   (⊦= '((ƛ (x y) α)) (°->list/datum (fresh° (q) (∀° (x y) (=° (@ q x y) (@ q y x))))))
   (⊦= '((ƛ (x) α)) (°->list/datum (fresh° (q) (∀° (x y) (=° (@ q x) (@ q y))))))
   ; p.129: λxλy.G x = λxλy.H y gives G, H := λw.T.
   (⊦= '(((ƛ (x) α) (ƛ (y) α))) (°->list/datum (fresh° (q g h) (=° q (list g h)) (=° (ƛ (x y) (@ g x)) (ƛ (x y) (@ h y)))))))

  ((test/A5/pruning _)
   ; F x = G y under binders x, y: neither may depend on its argument.
   (⊦= '(((ƛ (x) α) (ƛ (y) α))) (°->list/datum (fresh° (q f g) (=° q (list f g)) (∀° (x y) (=° (@ f x) (@ g y))))))
   ; Example 8.H (p.239-240): U := λxλy. g (V' y), V := λyλw. V' y.
   (⊦= '(((ƛ (x y) (g (@ α y))) (ƛ (y w) (@ α y))))
       (°->list/datum (fresh° (q u v) (=° q (list u v)) (∀° (w x y) (=° `(f ,(@ u x y)) `(f (g ,(@ v y w)))))))))

  ((test/A5/scope-and-occurs _)
   (⊦= '() (°->list #f (fresh° (q) (∀° (x) (=° q x)))))                                  ; ∃F∀x. F = x
   (⊦= '((ƛ (x) x)) (°->list/datum (fresh° (q) (∀° (x) (=° (@ q x) x)))))               ; ∃F∀x. F x = x
   (⊦= '() (°->list #f (fresh° (q) (∀° (w x y) (=° (@ q x y) `(g ,w))))))               ; Example 8.J
   (⊦= '() (°->list #f (fresh° (q) (∀° (w x y) (=° (@ q x y) `(g ,(@ q y w)))))))       ; Example 8.I
   (⊦= '() (°->list #f (fresh° (q) (=° q `(f ,q)))))                                    ; Example 8.G
   (⊦= '() (°->list #f (fresh° (q) (=° q (ƛ (x) `(g ,(@ q x))))))))                      ; F = λx.g (F x)

  ((test/A5/delayed-raising _)
   ; Example 8.K (p.240-241): ∃U∀x∀y∀z∃V. U x = f V gives U := λx. f (V' x).
   (⊦= '((ƛ (x) (f (@ α x)))) (°->list/datum (fresh° (q) (∀° (x y z) (fresh° (v) (=° (@ q x) `(f ,v)))))))
   (⊦= '((ƛ (x) (g (@ α x)))) (°->list/datum (fresh° (q) (∀° (x) (fresh° (G) (∀° (y) (=° (@ q x) `(g ,(@ G y))))))))))

  ((test/A5/non-pattern _)
   ; F (g x) = h (g x) is no pattern: suspended, reported closed over x at the end of its scope.
   (⊦= '((λ (α) (assert (≐ (ƛ (x) (@ α (cons 'g (cons x '())))) (ƛ (x) (cons 'h (cons x '()))))) α))
       (°->list #f (fresh° (q) (∀° (x) (=° (@ q `(g ,x)) `(h ,x))))))
   ; woken by a later binding of F, to success or failure.
   (⊦= '((ƛ (z) (h z))) (°->list/datum (fresh° (q) (∀° (x) (=° (@ q `(g ,x)) `(h (g ,x)))) (=° q (ƛ (z) `(h ,z))))))
   (⊦= '() (°->list/datum (fresh° (q) (∀° (x) (=° (@ q `(g ,x)) `(h (g ,x)))) (=° q (ƛ (z) 'k)))))
   ; suspended below a binder of the unifier, no `∀°` at all: waking it may not let y capture that binder.
   (⊦= '() (°->list #f (fresh° (q f y) (=° q y) (=° (ƛ (x) (@ f `(g ,x))) (ƛ (x) `(h ,x))) (=° f (ƛ (z) y)))))
   ; Example 8.M (p.224): F x x = x has two incomparable unifiers, so it stays suspended.
   (⊦= '((λ (α) (assert (≐ (ƛ (x) (@ α x x)) (ƛ (x) x))) α)) (°->list #f (fresh° (q) (∀° (x) (=° (@ q x x) x)))))
   (⊦= '((ƛ (a b) b)) (°->list/datum (fresh° (q) (∀° (x) (=° (@ q x x) x)) (=° q (ƛ (a b) b)))))
   ; Example 8.A (p.229): F a = g a a with F inside a's scope: F is raised over a at the end of the scope.
   (⊦= '((λ (α) (assert (≐ (ƛ (a) (@ α a a)) (ƛ (a) (cons 'g (cons a (cons a '())))))) 'ok))
       (°->list #f (fresh° (q) (∀° (a) (fresh° (f) (=° (@ f a) `(g ,a ,a)))) (=° q 'ok)))))

  ((test/A5/non-pattern-pruning _)
   ; ∃q∃F∀x. q = F x x: an argument that is an eigenvariable q may not see is pruned, pattern or not.
   (⊦= '((λ (α) α)) (°->list #f (fresh° (q F) (∀° (x) (=° q (@ F x x))))))
   (⊦= '(c) (°->list/datum (fresh° (q F) (∀° (x) (=° q (@ F x x))) (=° F (ƛ (a b) 'c)))))
   (⊦= '() (°->list/datum (fresh° (q F) (∀° (x) (=° q (@ F x x))) (=° F (ƛ (a b) a)))))
   ; ∃F∀x∃G. F x = G x: G is raised, then F x = G' x x is solved, F := λx. G' x x (fully general).
   (⊦= '((λ (α) (ƛ (x) (@ α x x)))) (°->list #f (fresh° (q) (∀° (x) (fresh° (G) (=° (@ q x) (@ G x)))))))
   (⊦= '((ƛ (x) x)) (°->list/datum (fresh° (q) (∀° (x) (fresh° (G) (=° (@ q x) (@ G x)) (=° G (ƛ (z) x))))))))

  ((test/A5/soft-occurs _)
   ; F x = G (F x) holds with G := λz.z for any F: the occurrence of F in G's argument suspends, not fails.
   (⊦= '((λ (α β) (assert (≐ α (ƛ (x) (@ β (@ α x))))) (cons α (cons β '()))))
       (°->list #f (fresh° (q F G) (=° q (list F G)) (∀° (x) (=° (@ F x) (@ G (@ F x)))))))
   (⊦= '(((ƛ (x) a) (ƛ (y) a)))
       (°->list/datum (fresh° (q F G) (=° q (list F G)) (∀° (x) (=° (@ F x) (@ G (@ F x)))) (=° G (ƛ (y) 'a)))))
   (⊦= '(((ƛ (x) (h c)) (ƛ (y) c)))
       (°->list/datum (fresh° (q F G) (=° q (list F G)) (∀° (x) (=° (@ F x) `(h ,(@ G (@ F x))))) (=° G (ƛ (y) 'c))))))

  ((test/A5/eta _)
   ; terms are η-short, built so by `ƛ` and by the unifier.
   (⊦= 'g (μkanren-datum (ƛ (x) (@ 'g x))))
   (⊦= '(ƛ (x) (g x)) (μkanren-datum (ƛ (x) `(g ,x))))
   (⊦= '(α) (°->list/datum (fresh° (q) (=° (ƛ (x) (@ q x)) q))))
   (⊦= '((ƛ (x) (g x))) (°->list/datum (fresh° (q) (=° (ƛ (x) (@ q x)) (ƛ (x) `(g ,x))))))
   (⊦= '((α α)) (°->list/datum (fresh° (q f g) (=° q (list f g)) (∀° (x) (=° (@ f x) (@ g x))))))
   (⊦= '((α α)) (°->list/datum (fresh° (q f g) (=° q (list f g)) (∀° (x) (=° (@ f x) (@ g x))) (=° f g))))
   ; F x = G x y makes F := G' (not λx.G' x), so F = H later is no occurs failure once G' is H.
   (⊦= '((α (ƛ (x y) (@ α x)))) (°->list/datum (fresh° (q f g h) (=° q (list f g)) (∀° (x y) (=° (@ f x) (@ g x y)))
                                                 (=° g (ƛ (a b) (@ h a))) (=° f h)))))

  ((test/A5/trivial-residuals _)
   ; F x x = G x x, then F = G: the suspended equation becomes α-equal and is dropped, not reported.
   (⊦= '((λ (α) (cons α (cons α '()))))
       (°->list #f (fresh° (q F G) (=° q (list F G)) (∀° (x) (=° (@ F x x) (@ G x x))) (=° F G)))))

  ;; A6 Ma & Friedman 2021 ------------------------------------------------------------------------

  ((test/A6/alpha _)
   (⊦= '((λ (α) α)) (°->list #f (fresh° (q) (=° (ƛ (a) a) (ƛ (b) b)))))
   (⊦= #t (equal? (ƛ (x) `(f ,x)) (ƛ (x) `(f ,x))))
   (⊦= #t (≐ (ƛ (a) a) (ƛ (b) b))))

  ((test/A6/scope _)
   ; the paper answers d0 (grafting); λProlog fails: ∃X∀a. X = a.
   (⊦= '() (°->list #f (fresh° (q) (=° (ƛ (a) q) (ƛ (b) b)))))
   (⊦= '() (°->list #f (fresh° (q) (=° (ƛ (a) q) (ƛ (b) b)) (=° (ƛ (c) q) (ƛ (d) d)))))
   ; their Example 3: neither X nor Y may depend on the binders, so X = Y.
   (⊦= '((α α)) (°->list/datum (fresh° (q X Y) (=° q (list X Y)) (=° (ƛ (a b) X) (ƛ (c d) Y))))))

  ((test/A6/true=false _)
   ; λa.λa.X = λc.λd.c must fail: the inner a shadows the outer one, X can be neither.
   (⊦= '() (°->list #f (fresh° (q) (=° (ƛ (a) (ƛ (a) q)) (ƛ (c) (ƛ (d) c))))))
   (⊦= '() (°->list #f (fresh° (q) (=° (ƛ (a) (ƛ (a) a)) (ƛ (c) (ƛ (d) c))))))
   (⊦= '((λ () 'ok)) (°->list #f (fresh° (q) (=° (ƛ (a) (ƛ (a) a)) (ƛ (c) (ƛ (d) d))) (=° q 'ok))))
   (⊦= '() (°->list #f (fresh° (q) (=° (ƛ (x y) (@ x y)) (ƛ (a a) (@ a a))))))
   ; shadowing printed apart: λa.λa.a is λt.λf.f.
   (⊦= '((ƛ (a a1) a1)) (°->list/datum (fresh° (q) (=° q (ƛ (a) (ƛ (a) a)))))))

  ((test/A6/assume _)
   ; the paper's (assume (== 'apple 'orange) (== 'apple X)) on a dynamic equality: the assumption first.
   (define-dynamic-relation (eq° a b) (=° a b))
   (⊦= '(orange apple) (°->list/datum (fresh° (q) (⊃° ((eq° 'apple 'orange)) (eq° 'apple q))))))

  ((test/A6/all _)
   ; (run 1 F (all (A) (== A (F A)))) gives λx.x.
   (⊦= '((ƛ (a) a)) (°->list/datum (fresh° (q) (∀° (a) (=° a (@ q a)))))))

  ;; A7 more book examples ------------------------------------------------------------------------

  ((test/A7/de-bruijn _)
   (define tm `(abs ,(ƛ (x) `(app ,x (abs ,(ƛ (y) `(app ,x (abs ,(ƛ (w) `(app ,w ,x))))))))))
   ; p.209, both directions.
   (⊦= '((ab (ap (deb 1) (ab (ap (deb 2) (ab (ap (deb 1) (deb 3))))))))
       (°->list/datum (fresh° (q) (trans° 1 tm q))))
   (⊦= '((abs (ƛ (c) (app c (abs (ƛ (c1) (app c (abs (ƛ (c2) (app c2 c))))))))))
       (°->list/datum (fresh° (q) (trans° 1 q '(ab (ap (deb 1) (ab (ap (deb 2) (ab (ap (deb 1) (deb 3)))))))))))
   ; p.209-210, the trace.
   (⊦= '((ab (ab (ab (deb 2)))))
       (°->list/datum (fresh° (q) (trans° 1 `(abs ,(ƛ (x) `(abs ,(ƛ (y) `(abs ,(ƛ (z) y)))))) q)))))

  ((test/A7/prenex _)
   ; p.212: the unique prenex form.
   (⊦= '((all (ƛ (x) (all (ƛ (x1) (or (and (and (p x) (q x x1)) (p (f x))) (p a)))))))
       (°->list/datum (fresh° (q) (prenex° `(or (all ,(ƛ (x) `(and (and (p ,x) (all ,(ƛ (y) `(q ,x ,y)))) (p (f ,x))))) (p a)) q))))
   ; p.213: the five prenex forms (the book's answers 1, 3, 2, 4, 5, in this order).
   (⊦= '((all (ƛ (x) (all (ƛ (x1) (and (q x x) (q x x1))))))
         (all (ƛ (x) (all (ƛ (x1) (and (q x1 x1) (q x x1))))))
         (all (ƛ (x) (all (ƛ (x1) (all (ƛ (x2) (and (q x x) (q x1 x2))))))))
         (all (ƛ (x) (all (ƛ (x1) (all (ƛ (x2) (and (q x1 x1) (q x x2))))))))
         (all (ƛ (x) (all (ƛ (x1) (all (ƛ (x2) (and (q x2 x2) (q x x1)))))))))
       (°->list/datum (fresh° (q) (prenex° `(and (all ,(ƛ (x) `(q ,x ,x))) (all ,(ƛ (z) `(all ,(ƛ (y) `(q ,z ,y)))))) q)))))

  ((test/A7/mappred _)
   ; Figure 5.4 (p.143): mappred age (ned::bob::sue::nil) L, and backwards.
   (⊦= '((23 23 24)) (°->list/datum (fresh° (q) (mappred° age° '(ned bob sue) q))))
   (⊦= '((bob sue) (ned sue)) (°->list/datum (fresh° (q) (mappred° age° q '(23 24)))))
   ; p.145: mappred (x\y\ age y x) (23::24::nil) K.
   (⊦= '((bob sue) (ned sue)) (°->list/datum (fresh° (q) (mappred° (λ (x y) (age° y x)) '(23 24) q)))))

  ((test/A7/palindrome _)
   ; Figure 5.11 (p.158): destructuring a functional difference list waits for non-patterns to wake.
   (⊦= '(solved) (°->list/datum (fresh° (q) (palindrome° `(fdl ,(ƛ (x) `(a b c b a . ,x)))) (=° q 'solved))))
   (⊦= '() (°->list/datum (fresh° (q) (palindrome° `(fdl ,(ƛ (x) `(a b c . ,x)))) (=° q 'solved))))
   ; the book's first nine answers, in its order.
   (⊦= '((ƛ (x) x) (ƛ (x) (b a . x)) (ƛ (x) (a b a . x)) (ƛ (x) (α a b a . x)) (ƛ (x) (α α a b a . x))
         (ƛ (x) (α β α a b a . x)) (ƛ (x) (α β β α a b a . x)) (ƛ (x) (α β γ β α a b a . x)) (ƛ (x) (α β γ γ β α a b a . x)))
       (°->list/datum (take° 9 (fresh° (q) (palindrome° `(fdl ,(ƛ (x) `(a b a . ,(@ q x))))))))))

  ((test/A7/cps _)
   ; Figure 10.6 (p.273): ftrans ((abs u\u) @ (abs u\u)) F, the book's term, administrative redexes and all.
   (⊦= '((abs (ƛ (k) (app (abs (ƛ (k1) (app k1 (abs (ƛ (k2) (abs (ƛ (x) (app (abs (ƛ (k3) (app k3 x))) k2))))))))
                          (abs (ƛ (m) (app (abs (ƛ (k1) (app k1 (abs (ƛ (k2) (abs (ƛ (x) (app (abs (ƛ (k3) (app k3 x))) k2))))))))
                                           (abs (ƛ (n) (app (app m k) n))))))))))
       (°->list/datum (fresh° (q) (ftrans° `(app (abs ,(ƛ (u) u)) (abs ,(ƛ (u) u))) q)))))

  ;; A8 the stores of micro ------------------------------------------------------------------------

  ((test/A8/diseq-lambda _)
   ; ≠° is decided modulo α, β and η.
   (⊦= '() (°->list #f (fresh° (q) (≠° q (ƛ (x) x)) (=° q (ƛ (y) y)))))
   (⊦= '() (°->list #f (fresh° (q) (≠° (ƛ (x) x) (ƛ (y) y)))))
   (⊦= '((λ (α) (begin (deny (equal? α (ƛ (x) x)))) α)) (°->list #f (fresh° (q) (≠° q (ƛ (x) x)))))
   (⊦= '((ƛ (y) c)) (°->list/datum (fresh° (q) (≠° q (ƛ (x) x)) (=° q (ƛ (y) 'c)))))
   (⊦= '() (°->list #f (fresh° (q f) (=° q f) (≠° (ƛ (x) (@ f x)) (ƛ (x) x)) (=° f (ƛ (y) y)))))
   (⊦= '((ƛ (y) 5)) (°->list/datum (fresh° (q f) (=° q f) (≠° (ƛ (x) (@ f x)) (ƛ (x) x)) (=° f (ƛ (y) 5)))))
   (⊦= '() (°->list #f (fresh° (q f g) (=° q (list f g)) (≠° (ƛ (x) (@ f x)) (ƛ (x) (@ g x))) (=° g f)))))

  ((test/A8/diseq-flex _)
   ; a disequality whose trial only suspends is printed as the equation it denies.
   (⊦= '((λ (α) (begin (deny (≐ (@ α 'a) 'a))) α)) (°->list #f (fresh° (q F) (=° q F) (≠° (@ F 'a) 'a))))
   (⊦= '() (°->list #f (fresh° (q F) (=° q F) (≠° (@ F 'a) 'a) (=° F (ƛ (z) z)))))
   (⊦= '((ƛ (z) b)) (°->list/datum (fresh° (q F) (=° q F) (≠° (@ F 'a) 'a) (=° F (ƛ (z) 'b))))))

  ((test/A8/diseq-flex-flex _)
   ; λxλy.F x ≠ λxλy.G y is ¬∃H. F = λx.H ∧ G = λy.H: the variable that the unifier creates is existential.
   (⊦= '((λ (α β) (begin (deny (∃ (_3) (and (equal? α (ƛ (x) _3)) (equal? β (ƛ (y) _3)))))) (cons α (cons β '()))))
       (°->list #f (fresh° (q f g) (=° q (list f g)) (≠° (ƛ (x y) (@ f x)) (ƛ (x y) (@ g y))))))
   (⊦= '() (°->list #f (fresh° (q f g) (=° q (list f g)) (≠° (ƛ (x y) (@ f x)) (ƛ (x y) (@ g y)))
                          (=° f (ƛ (z) 'a)) (=° g (ƛ (z) 'a)))))
   (⊦= '(((ƛ (z) a) (ƛ (z) b))) (°->list/datum (fresh° (q f g) (=° q (list f g)) (≠° (ƛ (x y) (@ f x)) (ƛ (x y) (@ g y)))
                                                 (=° f (ƛ (z) 'a)) (=° g (ƛ (z) 'b)))))
   ; the same disequality born first-order in D, on two variables later bound to such λ-terms.
   (⊦= '() (°->list #f (fresh° (q a b f g) (=° q (list f g)) (≠° a b)
                          (=° a (ƛ (x y) (@ f x))) (=° b (ƛ (x y) (@ g y))) (=° f (ƛ (z) 'a)) (=° g (ƛ (z) 'a)))))
   ; a tuple disequality migrates whole.
   (⊦= '() (°->list #f (fresh° (q r F) (=° q (list r)) (≠° (list r 'z) (list 'a 'z)) (=° r (@ F 'b)) (=° F (ƛ (u) 'a)))))
   (⊦= '((a c)) (°->list/datum (fresh° (q r s F) (=° q (list r s)) (≠° (list r s) '(a b)) (=° r (@ F 'b)) (=° F (ƛ (u) 'a)) (=° s 'c)))))

  ((test/A8/diseq-eigen _)
   ; an eigenvariable is a scoped constant (GENERIC's new name): no older variable can ever be it, so q ≠ x
   ; holds and vanishes; a younger one can.
   (⊦= '((λ (α) α)) (°->list #f (fresh° (q) (∀° (x) (≠° q x)))))
   (⊦= '() (°->list #f (fresh° (q) (∀° (x) (fresh° (y) (≠° y x) (=° y x))))))
   (⊦= '((λ (α) (cons α '()))) (°->list #f (fresh° (q) (∀° (x) (fresh° (y) (≠° y x) (=° q (list y)))))))
   (⊦= '((λ () 'ok)) (°->list #f (fresh° (q) (∀° (x) (≠° x 'a)) (=° q 'ok)))))

  ((test/A8/diseq-scope-exit _)
   ; ∀x. F x ≠ x is closed at the end of the scope into F ≠ λx.x, whichever comes first, binding or ≠.
   (⊦= '((λ (α) (begin (deny (equal? α (ƛ (x) x)))) α)) (°->list #f (fresh° (q F) (=° q F) (∀° (x) (≠° (@ F x) x)))))
   (⊦= '((ƛ (z) c)) (°->list/datum (fresh° (q F) (=° q F) (∀° (x) (≠° (@ F x) x)) (=° F (ƛ (z) 'c)))))
   (⊦= '((ƛ (z) c)) (°->list/datum (fresh° (q F) (=° q F) (=° F (ƛ (z) 'c)) (∀° (x) (≠° (@ F x) x)))))
   (⊦= '() (°->list/datum (fresh° (q F) (=° q F) (∀° (x) (≠° (@ F x) x)) (=° F (ƛ (z) z)))))
   (⊦= '((ƛ (a b) c)) (°->list/datum (fresh° (q F) (=° q F) (∀° (x) (≠° (@ F x x) x)) (=° F (ƛ (a b) 'c)))))
   (⊦= '() (°->list/datum (fresh° (q F) (=° q F) (∀° (x) (≠° (@ F x x) x)) (=° F (ƛ (a b) a))))))

  ((test/A8/diseq-extensional _)
   ; the other reading of ∀ under ≠°, on request: an eigenvariable stands for any term of its universe.
   (parameterize ((μkanren-λ-reading 'extensional))
     (⊦= '() (°->list #f (fresh° (q) (∀° (x) (≠° q x)))))                                 ; x could be q
     (⊦= '() (°->list #f (fresh° (q) (∀° (x) (≠° x 'a)) (=° q 'ok))))                     ; x could be a
     (⊦= '() (°->list #f (fresh° (q) (∀° (x) (fresh° (y) (≠° y x) (=° y 'a))))))          ; ∀x∃y. y ≠ x ∧ y = a
     (⊦= '((λ () 'ok)) (°->list #f (fresh° (q) (∀° (x) (fresh° (y) (≠° y x) (=° y `(f ,x)))) (=° q 'ok))))
     ; ∀x. F x ≠ x, kept closed with ∀, and violated by F := λz.c whichever comes first.
     (⊦= '((λ (α) (∀ (x) (begin (deny (≐ (@ α x) x)))) α)) (°->list #f (fresh° (q F) (=° q F) (∀° (x) (≠° (@ F x) x)))))
     (⊦= '() (°->list #f (fresh° (q F) (=° q F) (∀° (x) (≠° (@ F x) x)) (=° F (ƛ (z) 'c)))))
     (⊦= '() (°->list #f (fresh° (q F) (=° q F) (=° F (ƛ (z) 'c)) (∀° (x) (≠° (@ F x) x)))))
     (⊦= '((ƛ (z) (g z))) (°->list/datum (fresh° (q F) (=° q F) (∀° (x) (≠° (@ F x) x)) (=° F (ƛ (z) `(g ,z))))))
     ; ∀x. q ≠ (f x): q may be anything but an f-term.
     (⊦= '((λ (α) (begin (deny (∃ (_2) (equal? α (cons 'f (cons _2 '())))))) α)) (°->list #f (fresh° (q) (∀° (x) (≠° q `(f ,x))))))
     (⊦= '() (°->list #f (fresh° (q) (∀° (x) (≠° q `(f ,x))) (=° q '(f a)))))))

  ((test/A8/absent _)
   (⊦= '((λ () 'ok)) (°->list #f (fresh° (q) (absent° 'a (ƛ (x) (cons x 'b))) (=° q 'ok))))
   (⊦= '() (°->list #f (fresh° (q) (absent° 'b (ƛ (x) (cons x 'b))) (=° q 'ok))))
   ; neither binder names nor de Bruijn indices are data.
   (⊦= '((λ () 'ok)) (°->list #f (fresh° (q) (absent° 'x (ƛ (x) x)) (absent° 0 (ƛ (x) x)) (=° q 'ok))))
   (⊦= '((λ () 'ok)) (°->list #f (fresh° (q) (∀° (x) (absent° 'a x)) (=° q 'ok))))
   (⊦= '() (°->list/datum (fresh° (q) (absent° 'a q) (=° q (ƛ (x) (cons x 'a))))))
   (⊦= '() (°->list/datum (fresh° (q) (absent° 'a q) (=° q (ƛ (z) `(,z a))))))
   (⊦= '() (°->list/datum (fresh° (q) (absent° 'a q) (∀° (y) (=° (@ q y) `(g a ,y))))))
   ; an eigenvariable may be the datum.
   (⊦= '() (°->list #f (fresh° (q) (∀° (x) (fresh° (y) (absent° x y) (=° y (list x))))))))

  ((test/A8/absent-flexible _)
   ; absent c (F c) waits for F: F := λz.b drops the argument, so c is absent; F := λz.z keeps it.
   (⊦= '((λ (α) (assert (absent? 'c (@ α 'c))) α)) (°->list #f (fresh° (q F) (=° q F) (absent° 'c (@ F 'c)))))
   (⊦= '((ƛ (z) b)) (°->list/datum (fresh° (q F) (=° q F) (absent° 'c (@ F 'c)) (=° F (ƛ (z) 'b)))))
   (⊦= '() (°->list/datum (fresh° (q F) (=° q F) (absent° 'c (@ F 'c)) (=° F (ƛ (z) z)))))
   ; a datum present outside the flexible part is found at once; below a binder the watched part stays closed.
   (⊦= '() (°->list #f (fresh° (q F) (=° q F) (absent° 'c (ƛ (x) `(,(@ F x) c))))))
   (⊦= '((λ (α) (assert (absent? 'c α)) α)) (°->list #f (fresh° (q F) (=° q F) (absent° 'c (ƛ (x) `(d ,(@ F x)))))))
   ; ∀x. absent a (F x): closed at the end of the scope, i.e. absent a F.
   (⊦= '((λ (α) (assert (absent? 'a α)) α)) (°->list #f (fresh° (q F) (=° q F) (∀° (x) (absent° 'a (@ F x))))))
   (⊦= '((ƛ (z) z)) (°->list/datum (fresh° (q F) (=° q F) (∀° (x) (absent° 'a (@ F x))) (=° F (ƛ (z) z))))))

  ((test/A8/types _)
   ; an eigenvariable is no symbol nor number, and neither is an abstraction.
   (⊦= '() (°->list #f (fresh° (q) (∀° (x) (symbol° x)) (=° q 'ok))))
   (⊦= '() (°->list #f (fresh° (q) (symbol° q) (=° q (ƛ (x) x)))))
   ; a type on a flexible term waits for its head, before or after the binding.
   (⊦= '((λ (α) (assert (every (μ v (symbol? v)) (list (@ α 'a)))) (@ α 'a)))
       (°->list #f (fresh° (q F) (=° q (@ F 'a)) (symbol° q))))
   (⊦= '(a) (°->list/datum (fresh° (q F) (=° q (@ F 'a)) (symbol° q) (=° F (ƛ (z) z)))))
   (⊦= '(a) (°->list/datum (fresh° (q F) (symbol° q) (=° q (@ F 'a)) (=° F (ƛ (z) z)))))
   (⊦= '() (°->list/datum (fresh° (q F) (symbol° q) (=° q (@ F 'a)) (=° F (ƛ (z) 5)))))
   (⊦= '(5) (°->list/datum (fresh° (q F) (number° q) (=° q (@ F 'a)) (=° F (ƛ (z) 5))))))

  ((test/A8/reify _)
   ; λ-terms print with their binder names, merged; flexible applications with `@`; eigenvariables never.
   (⊦= '((λ (α) (cons (cons 'abs (cons (ƛ (x) x) '())) (cons (cons 'arr (cons α (cons α '()))) '()))))
       (°->list #f (take° 1 (fresh° (q m t) (=° q (list m t)) (typeof° m t)))))
   (⊦= '((ƛ (x1 x2) (x2 . x))) (°->list/datum (fresh° (q) (=° q (ƛ (x) (ƛ (x) (cons x 'x)))))))
   (⊦= '((ƛ (x1) (x1 x))) (°->list/datum (fresh° (q) (∀° (x) (=° (@ q x) (list x 'x))))))
   (⊦= '((ƛ (f x) (@ f (@ f x)))) (°->list/datum (fresh° (q) (=° q (ƛ (f x) (@ f (@ f x)))))))
   ; ground answers are live terms, to be used again; a term prints as its datum.
   (⊦= '((g b b)) (°->list/datum (fresh° (q) (=° q (@ (car (°->list/ground (fresh° (a) (=° a (ƛ (x) `(g ,x ,x)))))) 'b)))))
   (⊦= "(ƛ (x y) (x . y))" (->string (ƛ (x y) (cons x y)))))

  ((test/A8/aggregation _)
   ; λ-terms built alike are equal?, so they group.
   (⊦= '(((ƛ (x) c) 1) ((ƛ (x) x) 2))
       (sort (°->list/datum (fresh° (q t m) (groupby° (((n length) m)) over (t) from (and° (or° (=° t (ƛ (x) x)) (=° t (ƛ (x) x)) (=° t (ƛ (x) 'c))) (=° m 1))
                                               => (=° q (list t n)))))
             (λ (a b) (string<? (->string a) (->string b))))))

  ;; R regressions of the final review ------------------------------------------------------------------

  ((test/R/D-migration-deep _)
   ; a disequality of D whose inner variable is bound to an eigenvariable moves to N: the readings and the
   ; order of the goals no longer matter, and no eigenvariable reaches an answer.
   (parameterize ((μkanren-λ-reading 'extensional))
     (⊦= '() (°->list #f (fresh° (q) (∀° (x) (fresh° (Y) (≠° q (list Y)) (=° Y x))) (=° q '(a)))))
     (⊦= '() (°->list #f (fresh° (q) (∀° (x) (fresh° (Y) (=° Y x) (≠° q (list Y)))) (=° q '(a)))))
     (⊦= '() (°->list #f (fresh° (q) (∀° (x) (fresh° (Y) (≠° q (list Y)) (=° Y `(f ,x)))) (=° q '((f a)))))))
   (⊦= '((λ (α) α)) (°->list #f (fresh° (q) (∀° (x) (fresh° (Y) (≠° q (list Y)) (=° Y x)))))))

  ((test/R/D-migration-crash _)
   ; the same through a flexible term, with no ∀: micro's D never meets a λ-term.
   (⊦= '((λ () (cons 'b '()))) (°->list #f (fresh° (q F Y) (≠° q (list Y)) (=° Y (@ F 'a)) (=° q '(b)))))
   (⊦= '((λ (α) α)) (°->list #f (fresh° (q F Y) (≠° q (list Y)) (=° Y (@ F 'a)))))
   (⊦= '() (°->list #f (fresh° (q F Y) (≠° q (list Y)) (=° Y (@ F 'a)) (=° F (ƛ (z) 'b)) (=° q '(b))))))

  ((test/R/D-empty-prefix _)
   (⊦= '((λ () (cons 'b '()))) (°->list #f (fresh° (q) (fresh° (r F) (≠° q (list r)) (=° r (@ F 'a)) (=° q (list 'b))))))
   (⊦= '() (°->list #f (fresh° (q) (fresh° (r F) (≠° q (list r)) (=° r (@ F 'a)) (=° q (list 'b)) (=° F (ƛ (z) 'b))))))
   (⊦= '((λ () (cons 'b '()))) (°->list #f (fresh° (q) (fresh° (r G) (≠° q (list r)) (=° r (ƛ (x) (@ G x x))) (=° q (list 'b)))))))

  ((test/R/D-eigen-leak _)
   ; p = (q), r = x, p ≠ (r) is q ≠ x, read as the direct ∀x. q ≠ x is.
   (define (via-D q) (∀° (x) (fresh° (p r) (≠° p (list r)) (=° r x) (=° p (list q)))))
   (⊦= '((λ (α) α)) (°->list #f (fresh° (q) (via-D q))))
   (⊦= '() (parameterize ((μkanren-λ-reading 'extensional)) (°->list #f (fresh° (q) (via-D q))))))

  ((test/R/absent-entailed _)
   ; x is newer than every variable of the term: no instance can hold it.
   (⊦= '((λ (α) α)) (°->list #f (fresh° (q) (∀° (x) (absent° x q)))))
   (⊦= '((λ (α) α)) (°->list #f (fresh° (q) (∀° (x) (fresh° (Y) (absent° x Y) (=° q Y))))))
   (⊦= '() (°->list #f (fresh° (q) (∀° (x) (fresh° (Y) (absent° x Y) (=° Y (list q x))))))))

  ((test/R/non-strict-arguments _)
   ; G may drop its argument: nothing below it is pruned or lowered, in either order.
   (⊦= '(k) (°->list/datum (fresh° (q G) (∀° (y) (fresh° (Y) (=° q (@ G Y)) (=° Y y))) (=° G (ƛ (a) 'k)))))
   (⊦= '(k) (°->list/datum (fresh° (q G) (∀° (y) (fresh° (Y) (=° Y y) (=° q (@ G Y)))) (=° G (ƛ (a) 'k)))))
   (⊦= '(k) (°->list/datum (fresh° (q G H) (∀° (y) (=° q (@ G (@ H y)))) (=° G (ƛ (a) 'k)) (=° H (ƛ (a) a)))))
   (⊦= '(k) (°->list/datum (fresh° (q G H) (∀° (y) (=° q (@ G (@ H y y)))) (=° G (ƛ (a) 'k)) (=° H (ƛ (a b) a)))))
   ; undecided, the binding of q is taken back into an equation, closed over y at ∀ exit.
   (⊦= '((λ (α β γ) (assert (≐ (ƛ (y) α) (ƛ (y) (@ β (@ γ y))))) α)) (°->list #f (fresh° (q G H) (∀° (y) (=° q (@ G (@ H y)))))))
   ; a rigid occurrence of y in the argument still prunes it.
   (⊦= '((λ (α) α)) (°->list #f (fresh° (q G) (∀° (y) (=° q (@ G `(f ,y)))))))
   (⊦= '() (°->list/datum (fresh° (q G) (∀° (y) (=° q (@ G `(f ,y)))) (=° G (ƛ (a) a))))))

  ((test/R/bound-head-argument _)
   ; z is applied by G: y in (λz. z y) is no strict occurrence.
   (⊦= '(k) (°->list/datum (fresh° (q G) (∀° (y) (=° q (@ G (ƛ (z) (@ z y))))) (=° G (ƛ (a) (@ a (ƛ (w) 'k)))))))
   (⊦= '((λ (α β) (assert (≐ (ƛ (y) α) (ƛ (y) (@ β (ƛ (z) (@ z y)))))) α)) (°->list #f (fresh° (q G) (∀° (y) (=° q (@ G (ƛ (z) (@ z y)))))))))

  ((test/R/same-head-occurrence _)
   ; F a = F y holds with any constant F: suspended whatever the side, and solved once F is known.
   (⊦= '((λ (α) (assert (≐ α (ƛ (y) (@ α 'a)))) α)) (°->list #f (fresh° (q) (∀° (y) (=° (@ q 'a) (@ q y))))))
   (⊦= '((λ (α) (assert (≐ α (ƛ (y) (@ α 'a)))) α)) (°->list #f (fresh° (q) (∀° (y) (=° (@ q y) (@ q 'a))))))
   (⊦= '((ƛ (z) c)) (°->list/datum (fresh° (q) (∀° (y) (=° (@ q 'a) (@ q y))) (=° q (ƛ (z) 'c)))))
   (⊦= '() (°->list/datum (fresh° (q) (∀° (y) (=° (@ q 'a) (@ q y))) (=° q (ƛ (z) z)))))
   (⊦= '((ƛ (z) c)) (°->list/datum (fresh° (q) (∀° (y) (=° `(f ,(@ q (@ q 'a))) `(f ,(@ q y)))) (=° q (ƛ (z) 'c)))))
   (⊦= '((λ (α) (assert (≐ α (ƛ (w) (@ α 'a)))) α)) (°->list #f (fresh° (q) (=° q (ƛ (w) (@ q 'a))))))
   ; below a rigid constructor it is an occurs failure.
   (⊦= '() (°->list #f (fresh° (q) (=° q (ƛ (w) `(g ,(@ q 'a))))))))

  ((test/R/occurs-modulo-beta _)
   (⊦= '((λ (α) α)) (°->list #f (fresh° (q F) (=° F (ƛ (z) z)) (=° q (@ F q)))))
   (⊦= '((λ (α) α)) (°->list #f (fresh° (q F) (=° F (ƛ (z) z)) (=° (@ F q) q))))
   (⊦= '((λ (α) (cons (ƛ (z) z) (cons α '())))) (°->list #f (fresh° (q F Y) (=° q (list F Y)) (=° Y (@ F Y)) (=° F (ƛ (z) z)))))
   (⊦= '((ƛ (a b) b)) (°->list/datum (fresh° (q F) (∀° (x) (fresh° (Y) (=° (@ F x Y) Y) (=° F (ƛ (a b) b)) (=° Y 'a))) (=° q F)))))

  ((test/R/project-absent _)
   ; an absento on a flexible term whose head is out of the answer is dropped: that head may be constant.
   (⊦= '((λ (α) α)) (°->list #f (fresh° (q) (fresh° (F) (absent° 'c (@ F q))))))
   (⊦= '(c) (°->list/datum (fresh° (q) (fresh° (F) (absent° 'c (@ F q)) (=° q 'c)))))
   (⊦= '((λ (α) α)) (°->list #f (fresh° (q) (fresh° (G) (absent° 'c (ƛ (x) (@ G x q))))))))

  ((test/R/ground-hints _)
   ; binders are renamed apart from the operators of the rebuilding code too, so ground answers stay live.
   (⊦= '((ƛ (cons1) (a cons1))) (map μkanren-datum (°->list/ground (fresh° (q) (=° q (ƛ (cons) (list 'a cons)))))))
   (⊦= '(a b) (μkanren-datum (@ (car (°->list/ground (fresh° (q) (=° q (ƛ (cons) (list 'a cons)))))) 'b)))
   (⊦= '#(a b) (@ (car (°->list/ground (fresh° (q) (=° q (μkanren-abstract 'vector (λ (x) (vector 'a x))))))) 'b))
   (⊦= '((ƛ (quote1) (a quote1))) (map μkanren-datum (°->list/ground (fresh° (q) (=° q (μkanren-abstract 'quote (λ (x) (list 'a x)))))))))

  ((test/R/hypotheses-per-relation _)
   ; Γ has one entry per assumed relation, however many clauses it holds (slot 4 of the extension's record).
   (define-dynamic-relation (p° x))
   (define (nest n g) (if (zero? n) g (⊃° ((p° n)) (nest (sub1 n) g))))
   (define size #f)
   (°->list #f (nest 50 (λ (s) (set! size (length (record-instance-slot (μkanren-state-X s) 4))) (list s))))
   (⊦= 1 size)
   (⊦= 50 (length (°->list/datum (fresh° (q) (nest 50 (p° q)))))))

  ;; R2 the second adversarial review ------------------------------------------------------------------

  ((test/R2/raise-closes-constraints _)
   ; r2-0-raise-unclosed-eigen-print, r2-2-discharge-T-eigen-leak: the raise Y := Y' x at the exit of ∀x
   ; brings x into an absento of T and into a prefix of D moving to N; they are discharged in turn.
   (⊦= '((λ (α β) (assert (absent? 'k β)) (assert (≐ (ƛ (x) (@ α (@ β x))) (ƛ (x) x))) α))
       (°->list #f (fresh° (G) (∀° (x) (fresh° (Y) (absent° 'k Y) (=° (@ G Y) x))))))
   (⊦= '((λ (α β) (assert (absent? 'k β)) (assert (≐ (ƛ (x) (@ α (@ β x))) (ƛ (x) x))) α))
       (°->list #f (fresh° (G) (∀° (x) (fresh° (Y) (=° (@ G Y) x) (absent° 'k Y))))))
   (⊦= '((λ (α β γ) (assert (absent? 'd γ)) (assert (≐ (ƛ (x) (@ β (@ γ x) x)) (ƛ (x) 'b))) α))
       (°->list #f (fresh° (q G) (∀° (x) (fresh° (Y) (absent° 'd Y) (=° (@ G Y x) 'b))))))
   (⊦= '((λ (α β γ δ) (assert (≐ (ƛ (x) (@ α (@ γ x))) (ƛ (x) x)))
                      (assert (≐ (ƛ (x) (@ β (@ γ x) (@ γ x))) (ƛ (x) (cons 'f (cons (@ δ x) '())))))
                      (begin (deny (equal? γ δ))) (cons α (cons β '()))))
       (°->list #f (fresh° (q G H) (=° q (list G H)) (∀° (x) (fresh° (Y W) (≠° W Y) (=° (@ H Y Y) `(f ,W)) (=° (@ G Y) x))))))
   ; a closed equation is retried: ∀x. F x = g F is F = λx. g F, an occurs failure.
   (⊦= '() (°->list #f (fresh° (q F) (∀° (x) (=° (@ F x) `(g ,F)))))))

  ((test/R2/answer-inside-forall _)
   ; r2-0-answer-var-inside-forall: an answer created inside ∀x that holds x cannot be told without x.
   (define message "the answer holds the eigenvariable of a ∀° it was created in: create the variable it reifies outside every ∀°")
   (⊦= message (error-message (λ () (°->list #f (∀° (x) (fresh° (q) (=° q x)))))))
   (⊦= message (error-message (λ () (°->list/ground (∀° (x) (fresh° (q) (=° q `(f ,x))))))))
   (⊦= '((λ () 'a)) (°->list #f (∀° (x) (fresh° (q) (=° q 'a))))))

  ((test/R2/eta-after-binding _)
   ; r2-1-eta-after-instantiation: λx. F (G x) is F once G := λz.z, whichever comes first.
   (⊦= '((λ (α) α)) (°->list #f (fresh° (F G) (=° G (ƛ (z) z)) (=° F (ƛ (x) (@ F (@ G x)))))))
   (⊦= '((λ (α) α)) (°->list #f (fresh° (F G) (=° F (ƛ (x) (@ F (@ G x)))) (=° G (ƛ (z) z)))))
   (⊦= '((λ (α) α)) (°->list #f (fresh° (F G H) (=° G (ƛ (z) z)) (=° H (ƛ (x) (@ F (@ G x)))) (=° F H))))
   (⊦= '() (°->list #f (fresh° (F G) (=° G (ƛ (z) z)) (≠° F (ƛ (x) (@ F (@ G x)))))))
   (⊦= '((λ () (ƛ (c) c))) (°->list #f (fresh° (F G) (=° G (ƛ (z) z)) (∀° (c) (=° (@ F (ƛ (y) (@ G (@ c y)))) c)))))
   (⊦= '(α) (°->list/datum (fresh° (q F G) (=° q (ƛ (x) (@ F (@ G x)))) (=° G (ƛ (z) z))))))

  ((test/R2/occurs-non-pattern-arguments _)
   ; r2-1-rigid-occurs-nonpattern-args: F b̄ below a rigid constructor may be smaller than F ā when b̄ may
   ; reduce or differ in number: suspended, and solved in either order.
   (⊦= '((λ (α) (assert (≐ α (ƛ (x) (@ x (@ α (ƛ (z) z)))))) α))
       (°->list #f (fresh° (F) (∀° (x) (=° (@ F x) (@ x (@ F (ƛ (z) z))))))))
   (⊦= '((λ () (ƛ (a) (@ a 'c)))) (°->list #f (fresh° (F) (=° F (ƛ (a) (@ a 'c))) (∀° (x) (=° (@ F x) (@ x (@ F (ƛ (z) z))))))))
   (⊦= '((λ () (ƛ (a) (@ a 'c)))) (°->list #f (fresh° (F) (∀° (x) (=° (@ F x) (@ x (@ F (ƛ (z) z))))) (=° F (ƛ (a) (@ a 'c))))))
   (⊦= '((λ () (ƛ (w) w))) (°->list #f (fresh° (F) (∀° (x y) (=° (@ F x y) (@ x (@ F y)))) (=° F (ƛ (w) w)))))
   (⊦= '((λ () (ƛ (w) w))) (°->list #f (fresh° (F) (∀° (x) (=° (@ F x) (ƛ (w) (@ x (@ F w))))) (=° F (ƛ (w) w)))))
   ; rigid arguments as many as ā and the binders above: an occurs failure still.
   (⊦= '() (°->list #f (fresh° (q) (=° q (ƛ (w) `(g ,(@ q '(a b))))))))
   (⊦= '() (°->list #f (fresh° (q) (∀° (x) (=° (@ q x) `(g ,(@ q 'a))))))))

  ((test/R2/extensional-absent-order _)
   ; r2-2-ext-diseq-absent-subsumption: under the extensional reading Y ≠ a stays in N, where no absento of
   ; T absorbs it: ∀x∃Y. Y ≠ a ∧ absent a Y ∧ Y = x fails in every order.
   (parameterize ((μkanren-λ-reading 'extensional))
     (⊦= '() (°->list #f (fresh° (q) (∀° (x) (fresh° (Y) (≠° Y 'a) (=° Y x) (absent° 'a Y))))))
     (⊦= '() (°->list #f (fresh° (q) (∀° (x) (fresh° (Y) (absent° 'a Y) (=° Y x) (≠° Y 'a))))))
     (⊦= '() (°->list #f (fresh° (q) (∀° (x) (fresh° (Y) (≠° Y 'a) (absent° 'a Y) (=° Y x))))))
     (⊦= '() (°->list #f (fresh° (q) (∀° (x) (fresh° (Y) (absent° 'a Y) (≠° Y 'a) (=° Y x))))))
     (⊦= '((λ (α β) (begin (deny (equal? α β))) (cons α (cons β '())))) (°->list #f (fresh° (q a b) (∀° (x) (≠° a b)) (=° q (list a b)))))))

  ((test/R2/type-projection _)
   ; r2-2-E-residual-unreachable-head: a type on a term whose head is out of the answer is dropped, as an
   ; absento is, unless two different types wait on that head; a type of A entails a disequality with a λ-term.
   (⊦= '((λ (α) α)) (°->list #f (fresh° (q F) (symbol° (@ F q)))))
   (⊦= '((λ (α) α)) (°->list #f (fresh° (q F) (∀° (x) (symbol° (@ F x))))))
   (⊦= '((λ (α) α)) (°->list #f (fresh° (q F) (symbol° (@ F q)) (absent° 'c (@ F q)))))
   (⊦= '((λ (α) (assert (every (μ v (number? v)) (list (@ _1 'b)))) (assert (every (μ v (symbol? v)) (list (@ _1 α)))) α))
       (°->list #f (fresh° (q F) (symbol° (@ F q)) (number° (@ F 'b)))))
   (⊦= '((λ (α) (assert (every (μ v (symbol? v)) (list α))) α)) (°->list #f (fresh° (q) (symbol° q) (≠° q (ƛ (x) x))))))

  ;; R3 the third adversarial review -------------------------------------------------------------------

  ((test/R3/non-pattern-functional-arguments _)
   ; r3-0-nonpattern-abstraction-arg-elim, r3-1-lower-head-of-nonpattern, r3-1-prune-consumed-argument: G
   ; may apply an argument that is, or may become, an abstraction, and drop another argument or the
   ; eigenvariables of its own body; so G b̄ is neither lowered nor pruned then, and the check waits.
   (⊦= '((λ () 'k)) (°->list #f (fresh° (q) (∀° (x) (fresh° (G) (=° q (@ G (ƛ (w) 'k))) (=° G (ƛ (y) (@ y x))))))))
   (⊦= '((λ () 'k)) (°->list #f (fresh° (q) (∀° (x) (fresh° (G) (=° G (ƛ (y) (@ y x))) (=° q (@ G (ƛ (w) 'k))))))))
   (⊦= '((λ () 'k)) (°->list #f (fresh° (q) (∀° (x) (fresh° (G H) (=° q (@ G H)) (=° H (ƛ (w) 'k)) (=° G (ƛ (y) (@ y x))))))))
   (⊦= '((λ () 'k)) (°->list #f (fresh° (q G) (∀° (x) (=° q (@ G `(f ,x) (ƛ (v) 'k)))) (=° G (ƛ (a b) (@ b a))))))
   (⊦= '((λ () 'k)) (°->list #f (fresh° (q G) (=° G (ƛ (a b) (@ b a))) (∀° (x) (=° q (@ G `(f ,x) (ƛ (v) 'k)))))))
   (⊦= '((λ () 'k)) (°->list #f (fresh° (q G) (∀° (c) (=° q (@ G c (ƛ (w) 'k)))) (=° G (ƛ (y z) (@ z y))))))
   (⊦= '((λ () 'k)) (°->list #f (fresh° (q G Z) (∀° (c) (=° q (@ G c Z))) (=° G (ƛ (y z) (@ z y))) (=° Z (ƛ (w) 'k)))))
   ; the trial of ≠° uses the same check: the violated disequalities fail.
   (⊦= '() (°->list #f (fresh° (q) (∀° (x) (fresh° (G) (≠° (list (ƛ (w) q) G) (list (ƛ (w) (@ G (ƛ (v) 'k))) (ƛ (y) (@ y x))))
                                                        (=° G (ƛ (y) (@ y x))) (=° q 'k))))))
   (⊦= '() (°->list #f (fresh° (q G Z) (∀° (c) (≠° (list (ƛ (w) q) G) (list (ƛ (w) (@ G c Z)) (ƛ (y z) (@ z y)))))
                               (=° G (ƛ (y z) (@ z y))) (=° Z (ƛ (w) 'k)) (=° q 'k))))
   ; with no argument that may be an abstraction, pruning stays: ∃q∃F∀x. q = F x x.
   (⊦= '((λ (α) α)) (°->list #f (fresh° (q F) (∀° (x) (=° q (@ F x x)))))))

  ((test/R3/diseq-unreachable-argument _)
   ; r3-0-N-unreachable-dropped: ∃Y. G ≠ F Y is F ≠ λz.G, so an unreachable variable in an argument of a
   ; flexible term keeps its disequality; it prints as a working variable, as in limits/absent-projection.
   (⊦= '((λ (α β) (begin (deny (equal? β (@ α _3)))) (cons α (cons β '()))))
       (°->list #f (fresh° (q) (fresh° (F G Y) (=° q (list F G)) (≠° G (@ F Y))))))
   (⊦= '() (°->list #f (fresh° (q) (fresh° (F G Y) (=° q (list F G)) (≠° G (@ F Y)) (=° F (ƛ (z) 'k)) (=° G 'k)))))
   (⊦= '((λ (α β) (begin (deny (≐ (@ α _3) β))) (cons α (cons β '()))))
       (°->list #f (fresh° (q) (fresh° (F G) (=° q (list F G)) (∀° (x) (fresh° (Y) (≠° (@ F Y) G)))))))
   ; an unreachable head, or a variable outside every argument, is still dropped as by micro.
   (⊦= '((λ (α) α)) (°->list #f (fresh° (q) (fresh° (F) (≠° q (@ F 'a))))))
   (⊦= '((λ (α) α)) (°->list #f (fresh° (q) (fresh° (Y) (≠° q (ƛ (x) `(f ,Y)))))))
   (parameterize ((μkanren-λ-reading 'extensional))
     (⊦= '((λ (α) (∀ (x) (begin (deny (≐ (@ α _1) x)))) α)) (°->list #f (fresh° (F) (∀° (x) (fresh° (Y) (≠° (@ F Y) x))))))
     (⊦= '() (°->list #f (fresh° (F) (∀° (x) (fresh° (Y) (≠° (@ F Y) x))) (=° F (ƛ (z) 'k)))))))

  ((test/R3/absento-of-eigenvariable-under-type _)
   ; r3-0-derived-D-eigen, r3-2-T-to-D-under-type: under a type, micro turns an absento into a prefix of D;
   ; one whose datum holds a λ-term or an eigenvariable stays in T, so D never holds one.
   (define-μkanren-tag (μkanren-tag/nonnum v) (not (number? v)))
   (define nonnum° (μkanren-make-tag-A μkanren-tag/nonnum))
   (define nonnum-form '(assert (every (μ v (not (number? v))) (list α))))
   (⊦= `((λ (α) ,nonnum-form α)) (°->list #f (fresh° (q) (∀° (x) (absent° x q) (nonnum° q)))))
   (⊦= `((λ (α) ,nonnum-form α)) (°->list #f (fresh° (q) (∀° (x) (absent° x q)) (nonnum° q))))
   (⊦= `((λ (α) ,nonnum-form α)) (°->list #f (fresh° (q) (∀° (x) (absent° (list x) q) (nonnum° q)))))
   (⊦= '() (°->list #f (fresh° (q) (∀° (x) (absent° x q) (nonnum° q) (=° q x)))))
   (⊦= '(()) (let1 (D #f) (°->list #f (fresh° (q) (∀° (x) (absent° x q) (symbol° q)) (λ (s) (set! D (μkanren-state-D s)) (list s)))) (list D)))
   (⊦= '((λ (α) (assert (every (μ v (symbol? v)) (list (@ α 'a)))) α))
       (°->list #f (fresh° (F) (fresh° (q) (∀° (x) (absent° x q) (symbol° q)) (=° q (@ F 'a))))))
   (⊦= '((λ () 'a)) (°->list #f (fresh° (q G) (absent° (ƛ (z) (@ G z z)) q) (symbol° q) (=° q 'a))))
   (⊦= '((λ () 'a)) (°->list #f (fresh° (q G) (symbol° q) (absent° (ƛ (z) (@ G z z)) q) (=° q 'a))))
   (parameterize ((μkanren-λ-reading 'extensional))
     (⊦= '((λ () (ƛ (z) 'k))) (°->list #f (fresh° (F) (∀° (x) (fresh° (q) (absent° x q) (symbol° q) (=° q (@ F 'a)) (=° F (ƛ (z) 'k)))))))
     (⊦= '((λ () (ƛ (z) 'k))) (°->list #f (fresh° (F) (∀° (x) (fresh° (q) (absent° x q) (=° q (@ F 'a)) (=° F (ƛ (z) 'k)) (symbol° q))))))))

  ((test/R3/user-types _)
   ; r3-0-user-tag-lambda, r3-2-user-tag-on-lambda: a type of the user's decides λ-terms and eigenvariables
   ; by its predicate, as micro decides any other record; symbol° and number° hold of none. nonnum holds pairs
   ; too, so it is outside micro's contract of types, atoms only: these cases pin its predicate alone.
   (define-μkanren-tag (μkanren-tag/nonnum v) (not (number? v)))
   (define nonnum° (μkanren-make-tag-A μkanren-tag/nonnum))
   (⊦= '((λ () (ƛ (z) z))) (°->list #f (fresh° (q) (nonnum° q) (=° q (ƛ (z) z)))))
   (⊦= '((λ () 'ok)) (°->list #f (fresh° (q) (∀° (x) (nonnum° x)) (=° q 'ok))))
   (⊦= '((λ () (@ 'g 'a))) (°->list #f (fresh° (q) (=° q (@ 'g 'a)) (nonnum° q))))
   (⊦= '((λ (α) α)) (°->list #f (fresh° (q) (∀° (x) (fresh° (Y) (nonnum° Y) (=° Y x))))))
   (⊦= '((λ (α) (assert (every (μ v (not (number? v))) (list α))) (begin (deny (equal? α (ƛ (x) x)))) α))
       (°->list #f (fresh° (q) (nonnum° q) (≠° q (ƛ (x) x)))))
   (⊦= '() (°->list #f (fresh° (q) (symbol° q) (=° q (ƛ (z) z))))))

  ((test/R3/eta-short-occurs _)
   ; r3-1-occurs-eta-short: F y below the constant g is the η-short λz. F y z; below a root that no
   ; projection of F can produce, a rigid occurrence of F fails whatever its number of arguments.
   (⊦= '() (°->list #f (fresh° (F) (=° F (ƛ (x y) (@ 'g (@ F y)))))))
   (⊦= '() (°->list #f (fresh° (F) (∀° (x) (=° (@ F x) (ƛ (y) (@ 'g (@ F y))))))))
   (⊦= '() (°->list #f (fresh° (F) (=° F (ƛ (x y) (@ 'g (ƛ (z) (@ F z y))))))))
   (⊦= '() (°->list #f (fresh° (F) (∀° (x) (=° (@ F x) `(g ,(@ F 'a 'b))))))))

  ((test/R3/absento-datum-reduces _)
   ; r3-2-absent-datum-reduces-to-atom: an atom is checked against a datum that may still become it by
   ; unification, modulo βη, as a compound already was.
   (⊦= '() (°->list #f (fresh° (q G) (=° G (ƛ (z) 'a)) (absent° (@ G 'b) q) (=° q 'a))))
   (⊦= '() (°->list #f (fresh° (q G) (absent° (@ G 'b) q) (=° q 'a) (=° G (ƛ (z) 'a)))))
   (⊦= '() (°->list #f (fresh° (q G) (=° G (ƛ (u v) (@ 'a u))) (absent° (ƛ (z) (@ G z z)) q) (=° q 'a))))
   ; micro keeps an absento whose datum holds a variable as disequalities of that datum with the parts of the
   ; term (faster-miniKanren's absento): against the atom 'a, the disequality of the λ layer, read as absence.
   (⊦= '((λ (α) (begin (deny (≐ (@ α 'b) 'a))) α)) (°->list #f (fresh° (q) (fresh° (G X) (=° q G) (absent° (@ G 'b) X) (=° X 'a)))))
   (⊦= '() (°->list #f (fresh° (q D) (absent° D q) (=° q 'a) (=° D 'a)))))

  ((test/R3/type-conflict-on-flexible-term _)
   ; r3-2-type-conflict-on-flex: two different types on one flexible term fail, in every order.
   (⊦= '() (°->list #f (fresh° (q F) (symbol° q) (number° q) (=° q (@ F 'a)))))
   (⊦= '() (°->list #f (fresh° (q F) (=° q (@ F 'a)) (symbol° q) (number° q))))
   (⊦= '() (°->list #f (fresh° (q F) (symbol° q) (=° q (@ F 'a)) (number° q))))
   (⊦= '() (°->list/ground (fresh° (r q F) (=° r 'ok) (=° q (@ F 'a)) (symbol° q) (number° q))))
   (⊦= '() (°->list #f (fresh° (q F) (symbol° (@ F q)) (number° (@ F q)))))
   (⊦= '() (°->list #f (fresh° (q r F X Y) (=° q (@ F X)) (=° r (@ F Y)) (symbol° q) (number° r) (=° X Y)))))

  ((test/R3/first-order-cost _)
   ; r3-2-perf-A-D-nonempty: with D or A non-empty, the migration walks D and A, not every new binding
   ; through S; doc/hol-kanren/prototype/bench/bench-A.scm measures it. Here, that it still moves what it must.
   (⊦= '() (°->list #f (fresh° (q Y Z) (symbol° Z) (≠° q (list Y)) (=° Y (ƛ (x) x)) (=° q (list (ƛ (y) y))))))
   (⊦= '((λ () (cons (ƛ (x) x) '()))) (°->list #f (fresh° (q Y) (≠° q (list Y)) (symbol° Y) (=° q (list (ƛ (x) x))))))
   (⊦= '() (°->list #f (fresh° (q Y Z) (≠° q (list Y)) (=° Y (list Z)) (=° Z (ƛ (x) x)) (=° q (list (list (ƛ (y) y))))))))

  ;; R4 occurs checks under ∀, handover in every goal order, discharge at the exit of ∀, aggregation, sorted forms

  ((test/R4/occurs-under-forall _)
   ; a cycle closes through a variable lowered, or raised, by the scope check, or through β; micro's occurs
   ; check spares the variables M admits, so a binding that the λ layer takes back must leave M admitting it.
   (⊦= '() (°->list/datum (fresh° (q) (∀° (x) (fresh° (G) (=° q (list G)) (=° G q))))))
   (⊦= '() (°->list/datum (fresh° (q) (∀° (x) (fresh° (G) (=° G q) (=° q (list G)))))))
   (⊦= '() (°->list/datum (fresh° (q r) (=° q (list r)) (∀° (x) (fresh° (G) (=° q (list G)) (=° r (list G)))))))
   (⊦= '((a)) (°->list/datum (fresh° (q) (∀° (x) (fresh° (G) (=° q (list G)) (=° G 'a))))))
   (⊦= '() (°->list/datum (fresh° (q) (∀° (x) (fresh° (t) (chain° q 300 t) (=° t (list q)))))))
   (⊦= '() (°->list/datum (fresh° (q a b) (=° q (list a b)) (∀° (x) (fresh° (G) (=° a (list G)) (=° G b) (=° b q))))))
   ; X := F G is taken back into E by the scope check, G being a variable of a higher level in an argument of a
   ; non-pattern, while Y := (g X) of the same =° keeps X in S: forgotten, X must be admitted by M again, else
   ; X := (g X) skips the occurs check (with M left as it was, these do not terminate).
   (⊦= '() (°->list/datum (fresh° (q X F) (∀° (x) (fresh° (Y G) (=° (cons X Y) (cons (@ F G) (list 'g X))) (=° X Y))) (=° q 'bad))))
   (⊦= '() (°->list/datum (fresh° (q X F) (∀° (x) (fresh° (Y G) (=° (cons X Y) (cons (@ F G) (list 'g X))) (=° Y X))) (=° q 'bad))))
   ; λz.F z is F, so q := F, and then F := λw.(a F).
   (⊦= '() (°->list/datum (fresh° (q F) (=° q (ƛ (z) (@ F z))) (=° F (ƛ (w) (list 'a q))))))
   ; the eigenvariable escapes through a lowered variable, a long list or a β-reduction.
   (⊦= '() (°->list/datum (fresh° (q) (∀° (x) (fresh° (G) (=° q (list G)) (=° G x))))))
   (⊦= '() (°->list/datum (fresh° (q) (∀° (x) (fresh° (t) (chain° q 200 t) (=° t x))))))
   (⊦= '() (°->list/datum (fresh° (q F) (∀° (x) (=° q (list (@ F x)))) (=° F (ƛ (z) z)))))
   (⊦= '((a)) (°->list/datum (fresh° (q F) (∀° (x) (=° q (list (@ F x)))) (=° F (ƛ (z) 'a))))))

  ((test/R4/handover _)
   ; the constraints of a variable bound to a λ-term, or to a flexible term that a later binding reduces, are
   ; handed on in every order of the goals.
   (⊦= '(()) (in-every-order answers/datum (q F G) (≠° q G) (=° q (@ F 'a)) (=° G 'a) (=° F (ƛ (z) z))))
   (⊦= '(()) (in-every-order answers/datum (q F G) (≠° (list q G) '(a b)) (=° q (@ F 'a)) (=° G 'b) (=° F (ƛ (z) z))))
   (⊦= '(()) (in-every-order answers/datum (q F Y) (absent° (list Y) q) (=° Y (@ F 'a)) (=° q '((a))) (=° F (ƛ (z) z))))
   (⊦= '((((a)))) (in-every-order answers/datum (q F Y) (absent° (list Y) q) (=° Y (@ F 'a)) (=° q '((a))) (=° F (ƛ (z) 'b))))
   ; q := λx.F x is q := F, and F := λz.G z is F := G: q is G's abstraction, named after its binder.
   (⊦= '(()) (in-every-order answers/datum (q F G) (absent° 'c q) (=° q (ƛ (x) (@ F x))) (=° F (ƛ (z) (@ G z))) (=° G (ƛ (w) 'c))))
   (⊦= '(((ƛ (w) w))) (in-every-order answers/datum (q F G) (absent° 'c q) (=° q (ƛ (x) (@ F x))) (=° F (ƛ (z) (@ G z))) (=° G (ƛ (w) w))))
   (⊦= '(()) (in-every-order answers/datum (q F Y) (symbol° q) (absent° Y q) (=° q (@ F 'b)) (=° Y 'b) (=° F (ƛ (z) z))))
   ; at the exit of ∀x, Z := q x and the absento of x in Z is one of x in q x, which q's binding then decides.
   (⊦= '(()) (in-every-order answers/datum (q) (∀° (x) (fresh° (Z) (absent° x Z) (=° (@ q x) Z))) (=° q (ƛ (z) z))))
   (⊦= '(((ƛ (z) a))) (in-every-order answers/datum (q) (∀° (x) (fresh° (Z) (absent° x Z) (=° (@ q x) Z))) (=° q (ƛ (z) 'a))))
   (⊦= '(()) (in-every-order answers/datum (q) (∀° (x) (fresh° (Z) (absent° x Z) (=° (@ q x) Z))) (=° q (ƛ (z) (list 'a z)))))
   (⊦= '(()) (in-every-order answers/datum (q F) (∀° (x) (fresh° (Z) (≠° Z 'a) (absent° 'k Z) (=° (@ F x) Z))) (=° F (ƛ (z) 'a))))
   (⊦= '((ok)) (in-every-order answers/datum (q F) (∀° (x) (fresh° (Z) (≠° Z 'a) (absent° 'k Z) (=° (@ F x) Z))) (=° F (ƛ (z) 'b)) (=° q 'ok)))
   ; two exits leave an absento each in E, which the binding of G, through F = G, wakes: G y = (y) holds y.
   (⊦= '(()) (in-every-order answers/datum (q F G) (∀° (x) (fresh° (Z) (absent° x Z) (=° (@ F x) Z))) (∀° (y) (fresh° (Z) (absent° y Z) (=° (@ G y) Z)))
                             (=° F G) (=° G (ƛ (u) (list u)))))
   (⊦= '((ok)) (in-every-order answers/datum (q F G) (∀° (x) (fresh° (Z) (absent° x Z) (=° (@ F x) Z))) (∀° (y) (fresh° (Z) (absent° y Z) (=° (@ G y) Z)))
                               (=° F G) (=° G (ƛ (u) '(b))) (=° q 'ok))))

  ((test/R4/discharge _)
   ; an absento whose datum holds the eigenvariable moves to E at the exit of its ∀°, and stays there until
   ; the bindings decide it.
   (⊦= '() (°->list/datum (fresh° (r q V) (=° r (list q V)) (∀° (x) (fresh° (Y) (absent° (list x V) Y) (=° (@ q x) Y))) (=° q (ƛ (z) (list z V))))))
   (⊦= '() (°->list/datum (fresh° (r q V) (=° r (list q V)) (∀° (x) (fresh° (Y) (absent° (list x V) Y) (=° (@ q x) Y))) (=° q (ƛ (z) (list (list z 'w)))) (=° V 'w))))
   (⊦= '(((ƛ (z) ((z w))) u))
       (°->list/datum (fresh° (r q V) (=° r (list q V)) (∀° (x) (fresh° (Y) (absent° (list x V) Y) (=° (@ q x) Y))) (=° q (ƛ (z) (list (list z 'w)))) (=° V 'u))))
   (⊦= '() (°->list/datum (fresh° (r q V) (=° r (list q V)) (=° V 'w) (∀° (x) (fresh° (Y) (absent° (list x V) Y) (=° (@ q x) Y))) (=° q (ƛ (z) (list (list z 'w)))))))
   ; Y := (A B) inside the ∀°, so q's binding is made by the unifier, which names its binder after x.
   (⊦= '(((ƛ (x) (k (x w))) u))
       (°->list/datum (fresh° (r q V) (=° r (list q V)) (∀° (x) (fresh° (Y A B) (absent° (list x V) Y) (=° Y (list A B)) (=° (@ q x) Y)))
                        (=° q (ƛ (z) (list 'k (list z 'w)))) (=° V 'u))))
   (⊦= '() (°->list/datum (fresh° (q) (∀° (x) (∀° (y) (fresh° (Y) (absent° x Y) (=° (@ q x y) Y)))) (=° q (ƛ (a b) a)))))
   (⊦= '((ƛ (a b) b)) (°->list/datum (fresh° (q) (∀° (x) (∀° (y) (fresh° (Y) (absent° x Y) (=° (@ q x y) Y)))) (=° q (ƛ (a b) b)))))
   (⊦= '() (°->list/datum (fresh° (r q V) (=° r (list q V)) (∀° (x) (≠° (@ q x) (list x V))) (=° q (ƛ (z) (list z 'w))) (=° V 'w))))
   (⊦= '(((ƛ (z) (z w)) u)) (°->list/datum (fresh° (r q V) (=° r (list q V)) (∀° (x) (≠° (@ q x) (list x V))) (=° q (ƛ (z) (list z 'w))) (=° V 'u))))
   (⊦= '() (°->list/datum (fresh° (q) (∀° (x) (fresh° (Y) (absent° x Y) (=° (@ q x) Y))) (∀° (y) (fresh° (Y) (absent° 'k Y) (=° (@ q y) Y))) (=° q (ƛ (z) (list 'k))))))
   (⊦= '((ƛ (z) (m))) (°->list/datum (fresh° (q) (∀° (x) (fresh° (Y) (absent° x Y) (=° (@ q x) Y))) (∀° (y) (fresh° (Y) (absent° 'k Y) (=° (@ q y) Y))) (=° q (ƛ (z) (list 'm))))))
   (⊦= '() (°->list/datum (fresh° (q) (∀° (x) (fresh° (Y) (absent° x Y) (=° (@ q x) Y))) (∀° (y) (fresh° (Y) (absent° 'k Y) (=° (@ q y) Y))) (=° q (ƛ (z) (list z)))))))

  ((test/R4/aggregation _)
   ; the bodies of set°, groupby°, enumerate° and window° run past the variables of their from goal, so a
   ; variable of the from goal is older than the eigenvariables of a ∀° in the body.
   (define cnt (λ (k acc) (add1 acc)))
   (define-dynamic-relation (h° x))
   (⊦= '() (°->list/datum (fresh° (q k) (set° (n cnt 0) over ((k* k)) from (∀° (x) (=° k x)) => (=° q (list k* n))))))
   (⊦= '(ok) (°->list/datum (fresh° (q) (∀° (x) (fresh° (k) (set° (n cnt 0) over ((k* k)) from (or° (=° k x) (=° k 'a) (=° k x)) => (=° n 2) (=° k* x))))
                              (=° q 'ok))))
   (⊦= '(ok) (°->list/datum (fresh° (q k) (∀° (x) (set° (n cnt 0) over ((k* k)) from (fresh° (z) (=° k (list z))) => (=° k* (list x)) (=° q 'ok))))))
   (⊦= '() (°->list/datum (fresh° (q k) (∀° (x) (set° (n cnt 0) over ((k* k)) from (fresh° (z) (=° k (list z))) => (=° k* (list x)) (=° k k*) (=° q 'ok))))))
   (⊦= '() (°->list/datum (fresh° (q k) (∀° (x) (set° (n cnt 0) over ((k* k)) from (fresh° (z) (=° k (list z))) => (=° q k*) (=° k* (list x)))))))
   (⊦= '((1 1) (2 1)) (°->list/datum (fresh° (q k) (set° (n cnt 0) over ((k* k)) from (⊃° ((h° 1) (h° 2)) (h° k)) => (=° q (list k* n))))))
   (⊦= '(a) (°->list/datum (fresh° (q k) (⊃° ((h° 'a)) (set° (n cnt 0) over ((k* k)) from (=° k 1) => (h° q))))))
   (⊦= '() (°->list/datum (fresh° (q k) (set° (n cnt 0) over ((k* k)) from (⊃° ((h° 'b)) (=° k 1)) => (h° q)))))
   (⊦= '() (°->list/datum (fresh° (q) (∀° (x) (fresh° (k e) (groupby° (((es (λ (l) l)) e)) over (k) from (and° (=° k 1) (=° e x)) => (=° q es)))))))
   ; a ∀y in the body may not bind a variable of the from goal to y.
   (⊦= '() (°->list/datum (fresh° (q k) (set° (n cnt 0) over ((k* k)) from (fresh° (z) (=° k (list z))) => (∀° (y) (fresh° (w) (=° w y) (=° k* (list w)))) (=° q 'ok)))))
   (⊦= '() (°->list/datum (fresh° (q k) (set° (n cnt 0) over ((k* k)) from (∀° (u) (fresh° (z) (=° k (list z)))) => (∀° (y) (=° k* (list y))) (=° q 'ok)))))
   (⊦= '() (°->list/datum (fresh° (q k) (enumerate° (v (λ (i k) k)) over (k) from (fresh° (z) (=° k (list z))) => (∀° (y) (=° v (list (list y)))) (=° q 'ok)))))
   (⊦= '() (°->list/datum (fresh° (q k e) (groupby° (((es (λ (l) l)) e)) over (k) from (fresh° (z) (=° k 1) (=° e z)) => (∀° (y) (=° es (list y))) (=° q 'ok)))))
   (⊦= '() (°->list/datum (fresh° (q k e) (window° (((es (λ (l) l)) e)) over (k) from (fresh° (z) (=° k 1) (=° e z)) => (∀° (y) (=° es (list y))) (=° q 'ok)))))
   (⊦= '(ok) (°->list/datum (fresh° (q k) (∀° (x) (set° (n cnt 0) over ((k* k)) from (fresh° (z) (=° k (list z))) => (=° k* (list x)))) (=° q 'ok)))))

  ((test/R4/goal-order _)
   ; E and N are sorted when printed, as micro sorts D and T: the very same answer in every order of the goals.
   (⊦= '(((λ (α β γ) (assert (absent? 'c α)) (begin (deny (equal? β (ƛ (x) x)))) (begin (deny (≐ (cons α '()) (@ β 'a))))
                      (cons (cons α '()) (cons β (cons α (cons γ '())))))))
       (in-every-order (λ (g) (°->list #f g)) (r q F G Y) (=° r (list q F G Y))
                       (≠° q (@ F 'a)) (absent° 'c q) (=° q (list G)) (∀° (x) (≠° (@ F x) x))))
   (⊦= '(((λ (α β γ δ) (begin (deny (equal? α 'a))) (begin (deny (equal? β (ƛ (x) x)))) (begin (deny (equal? β (ƛ (y) 'c))))
                      (begin (deny (equal? γ (ƛ (z) z)))) (cons α (cons β (cons γ (cons δ '())))))))
       (in-every-order (λ (g) (°->list #f g)) (r q F G Y) (=° r (list q F G Y))
                       (∀° (x) (≠° (@ F x) x)) (∀° (y) (≠° (@ F y) 'c)) (≠° G (ƛ (z) z)) (≠° q 'a)))
   (⊦= '(((λ (α β γ) (assert (absent? 'k α)) (begin (deny (equal? α (ƛ (x) 'a)))) (begin (deny (equal? β (@ α 'b))))
                    (cons 'ok (cons α (cons β (cons γ '())))))))
       (in-every-order (λ (g) (°->list #f g)) (r q F G Y) (=° r (list q F G Y))
                       (∀° (x) (fresh° (Z) (≠° Z 'a) (absent° 'k Z) (=° (@ F x) Z))) (=° q 'ok) (≠° G (@ F 'b)))))

  ((test/R4/same-constraint _)
   ; a constraint posted twice, or twice up to the names of its binders, is printed once: E and N are
   ; conjunctions, the first in sort order stands for the others.
   (⊦= '((λ (α) (begin (deny (equal? α (ƛ (x) x)))) α))
       (°->list #f (fresh° (q) (≠° q (ƛ (x) x)) (≠° q (ƛ (x) x)))))
   (⊦= '((λ (α) (begin (deny (equal? α (ƛ (x) x)))) α))
       (°->list #f (fresh° (q F) (=° q F) (≠° q (ƛ (x) x)) (≠° F (ƛ (y) y)))))
   (⊦= '((λ (α) (begin (deny (equal? α (ƛ (x) x)))) α))
       (°->list #f (fresh° (q F) (=° q F) (∀° (y) (≠° (@ F y) y)) (∀° (x) (≠° (@ F x) x)))))
   (⊦= '((λ (α) (begin (deny (≐ (@ α 'a) 'b))) (cons α (cons α '()))))
       (°->list #f (fresh° (q F G) (=° q (list F G)) (≠° (@ F 'a) 'b) (≠° (@ G 'a) 'b) (=° F G))))
   (⊦= '((λ (α) (∀ (x) (assert (absent? x (@ α x)))) (cons α (cons α '()))))
       (°->list #f (fresh° (q F G) (=° q (list F G)) (=° F G)
                     (∀° (x) (fresh° (Z) (absent° x Z) (=° (@ F x) Z))) (∀° (y) (fresh° (Z) (absent° y Z) (=° (@ G y) Z))))))
   ; the names of the binders count only where they are bound: λx.x and λy.'x stay apart, as do λx.λy.x
   ; and λx.λy.y, while λy.λx.y is λa.λb.a.
   (⊦= '((λ (α) (begin (deny (equal? α (ƛ (x) x)))) (begin (deny (equal? α (ƛ (y) 'x)))) α))
       (°->list #f (fresh° (q) (≠° q (ƛ (x) x)) (≠° q (ƛ (y) 'x)))))
   (⊦= '((λ (α) (begin (deny (equal? α (ƛ (a b) a)))) (begin (deny (equal? α (ƛ (x y) y)))) α))
       (°->list #f (fresh° (q) (≠° q (ƛ (x y) y)) (≠° q (ƛ (y x) y)) (≠° q (ƛ (a b) a)))))
   ; 1 and "1" display alike: their written forms tell them apart, and order them in every order of the goals.
   (⊦= '(((λ (α) (begin (deny (equal? α (ƛ (x) "1")))) (begin (deny (equal? α (ƛ (x) 1)))) α)))
       (in-every-order (λ (g) (°->list #f g)) (q) (≠° q (ƛ (x) 1)) (≠° q (ƛ (x) "1")))))

  ;; A9 micro alone is unchanged: `make test-microkanren-with-lambda` runs micro's own suites with this module loaded.

  ((test/A9/first-order _)
   (define-relation (appendo r s rs)
     (cond° ((null° r) (=° s rs)) ((fresh° (a d c) (cons° a d r) (cons° a c rs) (appendo d s c)))))
   (⊦= '((() (1 2 3)) ((1) (2 3)) ((1 2) (3)) ((1 2 3) ()))
       (μkanren-run (q -1 #t) (fresh° (x y) (appendo x y '(1 2 3)) (=° q (list x y)))))
   (⊦= '((λ (α β) (begin (deny (equal? α 'a))) (assert (every (μ v (symbol? v)) (list α))) (assert (absent? 'z β)) (cons α (cons β '()))))
       (°->list #f (fresh° (q x y) (=° q (list x y)) (≠° x 'a) (symbol° x) (absent° 'z y))))))

(unittest/✓ microkanren-lambda-suite)
