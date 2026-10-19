
;;; The core examples of Miller & Nadathur, "Programming with Higher-Order Logic" (CUP 2012), in
;;; (aux kanren lambda), each beside its λProlog original. Run with
;;;   CHICKEN_REPOSITORY_PATH=$PWD/repo:/home/mnocentini/.local/lib/chicken/12 csi -s examples.scm > examples.out
;;; Every `(show "label" query)` prints the label and the answers, as data (`°->list/datum`), as micro's
;;; code (`°->list #f`), or αKanren-style with their constraints (`°->pretty`, from (aux kanren micro show)).
;;;
;;; The dictionary, λProlog → here:
;;;   x\ t                 (ƛ (x) t)               object binder by a Scheme binder (λ-tree syntax)
;;;   (F t)                (@ F t)                 application of a variable or an abstraction
;;;   pi x\ G              (∀° (x) G)              GENERIC
;;;   D => G               (⊃° (D) G)              AUGMENT, for a relation made by define-dynamic-relation
;;;   pi X\ H :- B         (∀ (X) (H ← B))         a hypothetical clause with its own variables
;;;   sigma X\ G           (fresh° (X) G)
;;;   G1, G2   G1 ; G2     (and° G1 G2)  (or° G1 G2)
;;; Object-level syntax is Scheme data: `abs M` is `(abs ,M), `app M N` is `(app ,M ,N).

(import scheme (chicken base) (chicken pretty-print) (chicken sort) (chicken string)
        (aux base) (aux stream) (aux kanren micro) (aux kanren micro show) (aux kanren lambda))

(include "test/programs.scm")

(define-syntax show
  (syntax-rules ()
    ((_ label e) (begin (display ";; ") (display label) (newline) (pretty-print e) (newline)))))

(define (sorted l) (sort l (λ (a b) (string<? (->string a) (->string b)))))

;;; ----------------------------------------------------------------------------------------------
;;; 1. λ-tree syntax (§7.1, §4.1): terms modulo α, β at construction, binders printed apart

(show "λx.λy.(x . y), as data and as micro's rebuilding code"
      (list (°->list/datum (fresh° (q) (=° q (ƛ (x y) (cons x y)))))
            (°->list #f (fresh° (q) (=° q (ƛ (x y) (cons x y)))))))

(show "α-equivalence: λa.a = λb.b, and the β-redex (λx.g x x) a"
      (list (°->list #f (fresh° (q) (=° (ƛ (a) a) (ƛ (b) b))))
            (°->list/datum (fresh° (q f) (=° f (ƛ (x) `(g ,x ,x))) (=° q (@ f 'a))))))

(show "shadowing, λa.λa.a, printed λa.λa1.a1 (Ma–Friedman's \"true = false\" hazard)"
      (°->list/datum (fresh° (q) (=° q (ƛ (a) (ƛ (a) a))))))

;;; ----------------------------------------------------------------------------------------------
;;; 2. Universal goals and scope (§3.3, p.101)

;; ?- pi x\ (Y = x).        no                ∃Y∀x. Y = x
;; ?- pi x\ sigma Y\ Y = x.  yes               ∀x∃Y. Y = x
(show "∃Y∀x. Y = x  and  ∀x∃Y. Y = x"
      (list (°->list #f (fresh° (q) (∀° (x) (=° q x))))
            (°->list #f (fresh° (q) (∀° (x) (fresh° (y) (=° y x))) (=° q 'ok)))))

(show "escape through an alias (q = (y), y = x), and through micro's cons°: both fail"
      (list (°->list #f (fresh° (q) (∀° (x) (fresh° (y) (=° q (list y)) (=° y x)))))
            (°->list #f (fresh° (q) (∀° (x) (cons° x '() q))))))

;;; ----------------------------------------------------------------------------------------------
;;; 3. Hypothetical goals (§3.2, p.94-104)

;; p 1.
;; ?- p 2 => p 3 => p X.          X = 3; X = 2; X = 1
;; ?- (p 2 & p 3) => p X.         X = 2; X = 3; X = 1
(define-dynamic-relation (p° x) (=° x 1))
(show "clause order under augmentation (p.94)"
      (list (°->list/datum (fresh° (q) (⊃° ((p° 2)) (⊃° ((p° 3)) (p° q)))))
            (°->list/datum (fresh° (q) (⊃° ((p° 2) (p° 3)) (p° q))))))

;; Figure 3.1, the course database (p.96-97):
;; ?- fact (finished dana X) => false.                                     X = 210
;; ?- fact (finished X Y) => (fact (graduates X), fact (cs_major X)).      X = dana, Y = 301
;; ?- fact (finished kim X) => fact (finished kim Y) =>
;;      (fact (graduates kim), fact (cs_major kim)).                        X = 250, Y = 301
;; ?- fact (finished kim 250) => fact (finished kim 301) => false.          solved
(show "courses: which course would make Dana's record inconsistent"
      (°->list/datum (fresh° (q) (⊃° ((fact° `(finished dana ,q))) (false°)))))
(show "courses: who graduates in CS after one more course, and which"
      (°->list/datum (fresh° (q x y) (=° q (list x y)) (⊃° ((fact° `(finished ,x ,y))) (fact° `(graduates ,x)) (fact° `(cs_major ,x))))))
(show "courses: Kim's two courses (each order is a proof, each found twice; the book shows the first)"
      (sorted (°->list/datum (fresh° (q x y) (=° q (list x y))
                               (⊃° ((fact° `(finished kim ,x))) (⊃° ((fact° `(finished kim ,y))) (fact° '(graduates kim)) (fact° '(cs_major kim))))))))
(show "courses: no consistent way for Kim"
      (°->list/datum (fresh° (q) (⊃° ((fact° '(finished kim 250))) (⊃° ((fact° '(finished kim 301))) (false°))) (=° q 'solved))))

;; p.98-99:  sterile J :- pi x\ bug x => in x J => dead x.
;;           dead B :- heated J, in B J, bug B.        heated j.
(define-dynamic-relation (bug° b))
(define-dynamic-relation (in° b j))
(define-relation (heated° j) (=° j 'j))
(define-dynamic-relation (dead° b) (fresh° (j) (heated° j) (in° b j) (bug° b)))
(define-relation (sterile° j) (∀° (x) (⊃° ((bug° x)) (⊃° ((in° x j)) (dead° x)))))
(show "jars and bugs: ?- sterile Q." (°->list/datum (fresh° (q) (sterile° q))))

;; Figure 3.4 (p.104):
;; reverse L K :- (rev nil K & (pi X\ pi L\ pi K\ rev (X::L) K :- rev L (X::K))) => rev L nil.
;; the free K of `rev nil K` is the caller's, the clause's own X, L, K are renamed at each use.
(define-dynamic-relation (rev° l k))
(define-relation (reverse° l k)
  (⊃° ((rev° '() k) (∀ (X L K) ((rev° `(,X . ,L) K) ← (rev° L `(,X . ,K))))) (rev° l '())))
(show "reverse with assumed clauses, forwards and backwards"
      (list (°->list/datum (fresh° (q) (reverse° '(1 2 3) q)))
            (°->list/datum (take° 1 (fresh° (q) (reverse° q '(1 2 3)))))))

;;; ----------------------------------------------------------------------------------------------
;;; 4. Type inference: ∀ and ⊃ together (§7.4.3, p.207)

;; typeof (app M N) A :- typeof M (arr B A), typeof N B.
;; typeof (abs M) (arr A B) :- pi x\ typeof x A => typeof (M x) B.
;; ?- typeof (abs x\ abs y\ abs z\ app (app x z) (app y z)) Ty.
;; Ty = arr (arr T1 (arr T2 T3)) (arr (arr T1 T2) (arr T1 T3))
;; ?- typeof (abs x\x) Ty.          Ty = arr T1 T1
;; ?- typeof (abs x\ app x x) Ty.   no
(show "typeof S, I and λx.x x"
      (list (°->list/datum (fresh° (q) (typeof° `(abs ,(ƛ (x) `(abs ,(ƛ (y) `(abs ,(ƛ (z) `(app (app ,x ,z) (app ,y ,z)))))))) q)))
            (°->list/datum (fresh° (q) (typeof° `(abs ,(ƛ (x) x)) q)))
            (°->list/datum (fresh° (q) (typeof° `(abs ,(ƛ (x) `(app ,x ,x))) q)))))
(show "typeof backwards: the first terms of type i → i (interleaving finds them where DFS loops)"
      (°->list/datum (take° 3 (fresh° (q) (typeof° q '(arr i i))))))

;;; ----------------------------------------------------------------------------------------------
;;; 5. Object-level substitution (§7.2, §7.6)

;; cbn (abs R) (abs R).   cbn (app M N) V :- cbn M (abs R), cbn (R N) V.
;; ?- cbn (app (abs x\ abs w\w) (app (abs x\ app x x) (abs x\ app x x))) V.     V = abs w\w   (p.199)
(show "call by name: (λx.λw.w) Ω"
      (let* ((ω `(abs ,(ƛ (x) `(app ,x ,x)))) (Ω `(app ,ω ,ω)))
        (°->list/datum (fresh° (q) (cbn° `(app (abs ,(ƛ (x) `(abs ,(ƛ (w) w)))) ,Ω) q)))))

;; Figure 7.10:  copy (abs M) (abs N) :- pi x\ copy x x => copy (M x) (N x).
;;               subst M T S :- pi x\ copy x T => copy (M x) S.
;; ?- copy (abs x\ abs y\ app y x) M.        M = abs v\ abs u\ app u v   (p.215)
(show "copy (p.215), and subst (x\\ app x x) (abs y\\ y)"
      (list (°->list/datum (fresh° (q) (copy° `(abs ,(ƛ (x) `(abs ,(ƛ (y) `(app ,y ,x))))) q)))
            (°->list/datum (fresh° (q) (subst° (ƛ (x) `(app ,x ,x)) `(abs ,(ƛ (y) y)) q)))))

;; Figure 7.11, existential generalization (p.218-219):
;; ?- subst M (f a) ((p (f a)) && (q a (f a))).
;; M = x\ (p x) && (q a x);  M = x\ (p x) && (q a (f a));  M = x\ (p (f a)) && (q a x);  M = x\ (p (f a)) && (q a (f a))
(show "existential generalization: the book's four answers, in its order"
      (°->list/datum (fresh° (q) (substform° q '(f a) '(and (p (f a)) (q a (f a)))))))

;;; ----------------------------------------------------------------------------------------------
;;; 6. De Bruijn translation (§7.4.4, p.209) and prenex normal form (§7.5, p.212-213)

;; trans D (abs M) (ab P) :- pi c\ depth D c => (E is D + 1, trans E (M c) P).
;; trans D (app M N) (ap P Q) :- trans D M P, trans D N Q.
;; trans D X (deb E) :- depth N X, E is (D - N).
;; ?- trans 1 (abs x\ app x (abs y\ app x (abs w\ app w x))) D.
;; D = ab (ap (deb 1) (ab (ap (deb 2) (ab (ap (deb 1) (deb 3))))))
;; ?- trans 1 P (ab (ap (deb 1) (ab (ap (deb 2) (ab (ap (deb 1) (deb 3))))))).
;; P = abs W1\ app W1 (abs W2\ app W1 (abs W3\ app W3 W1))
(show "de Bruijn, both directions"
      (list (°->list/datum (fresh° (q) (trans° 1 `(abs ,(ƛ (x) `(app ,x (abs ,(ƛ (y) `(app ,x (abs ,(ƛ (w) `(app ,w ,x))))))))) q)))
            (°->list/datum (fresh° (q) (trans° 1 q '(ab (ap (deb 1) (ab (ap (deb 2) (ab (ap (deb 1) (deb 3))))))))))))

;; ?- prenex ((all x\ q x x) && (all z\ all y\ q z y)) P.       five answers (p.213)
(show "prenex is a relation: the book's five answers (here in the order 1 3 2 4 5)"
      (°->list/datum (fresh° (q) (prenex° `(and (all ,(ƛ (x) `(q ,x ,x))) (all ,(ƛ (z) `(all ,(ƛ (y) `(q ,z ,y)))))) q))))

;;; ----------------------------------------------------------------------------------------------
;;; 7. Higher-order pattern unification (§8.3)

;; ?- pi x\ F x = g x x.                     F = x\ g x x
;; ?- pi x\ pi y\ F x y = G y x.             F = x\ y\ H y x,  G = H
;; ?- pi x\ pi y\ F x y = F y x.             F = x\ y\ H
(show "flex-rigid and flex-flex"
      (list (°->list/datum (fresh° (q) (∀° (x) (=° (@ q x) `(g ,x ,x)))))
            (°->list/datum (fresh° (q f g) (=° q (list f g)) (∀° (x y) (=° (@ f x y) (@ g y x)))))
            (°->list/datum (fresh° (q) (∀° (x y) (=° (@ q x y) (@ q y x)))))))

;; Example 8.H (p.239-240): ?- pi w\ pi x\ pi y\ f (U x y) = f (g (V y w)).
;; U = x\ y\ g (V' y),  V = y\ w\ V' y
(show "Example 8.H, pruning: (U V)" (°->list/datum (fresh° (q u v) (=° q (list u v)) (∀° (w x y) (=° `(f ,(@ u x y)) `(f (g ,(@ v y w))))))))

;; Example 8.K (p.240-241): ?- pi x\ pi y\ pi z\ sigma V\ U x = f V.      U = x\ f (V' x)
(show "Example 8.K, delayed raising" (°->list/datum (fresh° (q) (∀° (x y z) (fresh° (v) (=° (@ q x) `(f ,v)))))))

;; Examples 8.G, 8.I, 8.J: X = f X;  U x y = g (U y w);  U x y = g w.      no, no, no
(show "Examples 8.G (occurs), 8.I (occurs under a flexible head), 8.J (scope)"
      (list (°->list #f (fresh° (q) (=° q `(f ,q))))
            (°->list #f (fresh° (q) (∀° (w x y) (=° (@ q x y) `(g ,(@ q y w))))))
            (°->list #f (fresh° (q) (∀° (w x y) (=° (@ q x y) `(g ,w)))))))

;; ∃F∀x. F = x fails; ∃F∀x. F x = x gives F = λz.z.
(show "scope: F = x versus F x = x"
      (list (°->list #f (fresh° (q) (∀° (x) (=° q x))))
            (°->list/datum (fresh° (q) (∀° (x) (=° (@ q x) x))))))

;; Example 8.M (p.224): pi x\ F x x = x has two incomparable unifiers: it waits, and is reported.
;; Example 8.A (p.229): F a = g a a, four unifiers: likewise (no Huet enumeration).
(show "non-patterns: 8.M and 8.A, suspended; 8.M woken by F := λa.λb.b"
      (list (°->pretty (fresh° (q) (∀° (x) (=° (@ q x x) x))))
            (°->pretty (fresh° (q) (=° (@ q 'a) '(g a a))))
            (°->list/datum (fresh° (q) (∀° (x) (=° (@ q x x) x)) (=° q (ƛ (a b) b))))))

;; Dynamic L_λ (p.241-242): G may drop its argument, so nothing inside it is pruned or lowered. The
;; equation ∃q∃G∃H∀y. q = G (H y) waits, closed over y at ∀ exit, and is solved by G := λa.k. Likewise
;; pi y\ F a = F y holds with any constant F: it waits, then F := λz.c solves it, and F := λz.z does not.
(show "non-strict occurrences: q = G (H y), and F a = F y"
      (list (°->pretty (fresh° (q G H) (∀° (y) (=° q (@ G (@ H y))))))
            (°->list/datum (fresh° (q G H) (∀° (y) (=° q (@ G (@ H y)))) (=° G (ƛ (a) 'k))))
            (°->pretty (fresh° (q) (∀° (y) (=° (@ q 'a) (@ q y)))))
            (°->list/datum (fresh° (q) (∀° (y) (=° (@ q 'a) (@ q y))) (=° q (ƛ (z) 'c))))
            (°->list/datum (fresh° (q) (∀° (y) (=° (@ q 'a) (@ q y))) (=° q (ƛ (z) z))))))

;;; ----------------------------------------------------------------------------------------------
;;; 8. Higher-order programming (§5.3, §5.8.2)

;; mappred P nil nil.   mappred P (X::L) (Y::K) :- P X Y, mappred P L K.
;; ?- mappred age (ned::bob::sue::nil) L.        L = (23::23::24::nil)
;; ?- mappred (x\y\ age y x) (23::24::nil) K.    K = (bob::sue::nil); K = (ned::sue::nil)
(show "mappred (p.143, p.145)"
      (list (°->list/datum (fresh° (q) (mappred° age° '(ned bob sue) q)))
            (°->list/datum (fresh° (q) (mappred° (λ (x y) (age° y x)) '(23 24) q)))))

;; Figure 5.11: palindrome (fdl x\x).  palindrome (fdl x\ Y::x).
;;              palindrome (fdl x\ Y::(F (Y::x))) :- palindrome (fdl F).
;; ?- palindrome (fdl x\ a::b::c::b::a::x).     solved
;; ?- palindrome (fdl x\ a::b::a::(F x)).       F = x\ x; F = x\ b::a::x; F = x\ a::b::a::x; F = x\ Y::a::b::a::x; ...
(show "functional difference lists: palindromes (p.158), the book's first nine answers in its order"
      (list (°->list/datum (fresh° (q) (palindrome° `(fdl ,(ƛ (x) `(a b c b a . ,x)))) (=° q 'solved)))
            (°->list/datum (take° 9 (fresh° (q) (palindrome° `(fdl ,(ƛ (x) `(a b a . ,(@ q x))))))))))

;;; ----------------------------------------------------------------------------------------------
;;; 9. Fischer's CPS transformation (Figure 10.6, p.273)

;; ftrans (abs V) (abs k\ k @ U) :- phi (abs V) U.
;; ftrans (M @ N) (abs k\ P @ (abs m\ Q @ (abs n\ m @ k @ n))) :- ftrans M P, ftrans N Q.
;; phi (abs M) (abs k\ abs x\ (P x) @ k) :- pi x\ pi y\ ftrans x (abs k\ k @ y) => ftrans (M x) (P y).
;; ?- ftrans ((abs u\u) @ (abs u\u)) F.
;; F = (abs W1\ (abs (W2\ W2 @ abs W3\ abs W4\ abs (W5\ W5 @ W4) @ W3)) @
;;      (abs W2\ (abs (W3\ W3 @ abs W4\ abs W5\ abs (W6\ W6 @ W5) @ W4)) @ (abs W3\ W2 @ W1 @ W3)))
(show "ftrans ((abs u\\u) @ (abs u\\u)) F" (°->list/datum (fresh° (q) (ftrans° `(app (abs ,(ƛ (u) u)) (abs ,(ƛ (u) u))) q))))

;;; ----------------------------------------------------------------------------------------------
;;; 10. Ma & Friedman 2021: λProlog's answers

(show "λa.X = λb.b fails (the paper: X = d0); λa.λa.X = λc.λd.c fails; ∀A. A = F A gives F = λA.A"
      (list (°->list #f (fresh° (q) (=° (ƛ (a) q) (ƛ (b) b))))
            (°->list #f (fresh° (q) (=° (ƛ (a) (ƛ (a) q)) (ƛ (c) (ƛ (d) c)))))
            (°->list/datum (fresh° (q) (∀° (a) (=° a (@ q a)))))))

;;; ----------------------------------------------------------------------------------------------
;;; 11. micro's stores on λ-terms (no λProlog counterpart)

(show "≠ modulo αβη, flex-flex with an existential, a flexible absento, a type on a flexible term"
      (list (°->pretty (fresh° (q) (≠° q (ƛ (x) x))))
            (°->pretty (fresh° (q f g) (=° q (list f g)) (≠° (ƛ (x y) (@ f x)) (ƛ (x y) (@ g y)))))
            (°->pretty (fresh° (q F) (=° q F) (absent° 'c (@ F 'c))))
            (°->list/datum (fresh° (q F) (=° q F) (absent° 'c (@ F 'c)) (=° F (ƛ (z) 'b))))
            (°->list/datum (fresh° (q F) (=° q (@ F 'a)) (symbol° q) (=° F (ƛ (z) z))))))

(show "∀x. q ≠ x, and ∀x. F x ≠ x, under GENERIC's reading (default) and the extensional one"
      (list (°->pretty (fresh° (q) (∀° (x) (≠° q x))))
            (°->pretty (fresh° (q F) (=° q F) (∀° (x) (≠° (@ F x) x))))
            (parameterize ((μkanren-λ-reading 'extensional))
              (list (°->pretty (fresh° (q) (∀° (x) (≠° q x))))
                    (°->pretty (fresh° (q F) (=° q F) (∀° (x) (≠° (@ F x) x))))))))

(show "βη modulo later bindings: λx.F (G x) is F once G := λz.z; an absento on a variable raised at ∀ exit is closed"
      (list (°->list #f (fresh° (F G) (=° F (ƛ (x) (@ F (@ G x)))) (=° G (ƛ (z) z))))
            (°->list #f (fresh° (F G) (=° G (ƛ (z) z)) (∀° (c) (=° (@ F (ƛ (y) (@ G (@ c y)))) c))))
            (°->pretty (fresh° (G) (∀° (x) (fresh° (Y) (absent° 'k Y) (=° (@ G Y) x)))))))

(show "a non-pattern whose argument may be an abstraction waits: ∃q∀x∃G. q = G (λw.k) ∧ G = λy. y x, in both orders; F y below g is η-short and fails the occurs check"
      (list (°->list/datum (fresh° (q) (∀° (x) (fresh° (G) (=° q (@ G (ƛ (w) 'k))) (=° G (ƛ (y) (@ y x)))))))
            (°->list/datum (fresh° (q) (∀° (x) (fresh° (G) (=° G (ƛ (y) (@ y x))) (=° q (@ G (ƛ (w) 'k)))))))
            (°->pretty (fresh° (q G) (∀° (c) (=° q (@ G c (ƛ (w) 'k))))))
            (°->list #f (fresh° (F) (=° F (ƛ (x y) (@ 'g (@ F y))))))))

(show "∃Y. G ≠ F Y keeps its disequality, F may be λz.G; a datum that reduces to an atom; a type of the user's on a λ-term"
      (list (°->pretty (fresh° (q) (fresh° (F G Y) (=° q (list F G)) (≠° G (@ F Y)))))
            (°->list #f (fresh° (q G) (absent° (@ G 'b) q) (=° q 'a) (=° G (ƛ (z) 'a))))
            (let ()
              (define-μkanren-tag (μkanren-tag/nonnum v) (not (number? v)))
              (°->list/datum (fresh° (q) ((μkanren-make-tag-A μkanren-tag/nonnum) q) (=° q (ƛ (z) z)))))))
