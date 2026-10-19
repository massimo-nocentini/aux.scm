
; the book's programs used by the suite and by examples.scm, in the surface syntax of (aux kanren lambda).
; Object-level terms are lists, `(abs ,R)` with R a meta-level abstraction (λ-tree syntax, book §7.1).

; Figure 3.1, the course database (p.96): every predicate may be assumed, so `fact°` is dynamic.
(define-dynamic-relation (fact° e)
  (fresh° (x)
    (cond°
      ((=° e '(finished kim 102))) ((=° e '(finished dana 101)))
      ((=° e '(finished kim 210))) ((=° e '(finished dana 250)))
      ((=° e `(cs_major ,x))
       (or° (fact° `(finished ,x 101)) (fact° `(finished ,x 102)))
       (fact° `(finished ,x 250))
       (fact° `(finished ,x 301)))
      ((=° e `(graduates ,x))
       (or° (fact° `(finished ,x 101)) (fact° `(finished ,x 102)))
       (or° (fact° `(finished ,x 210)) (fact° `(finished ,x 250)))
       (fact° `(finished ,x 301))))))

(define-dynamic-relation (false°) (fresh° (x) (fact° `(finished ,x 210)) (fact° `(finished ,x 250))))

; §7.4.3, type inference:  typeof (abs M) (arr A B) :- pi x\ typeof x A => typeof (M x) B.
(define-dynamic-relation (typeof° m t)
  (cond°
    ((fresh° (M N B) (=° m `(app ,M ,N)) (typeof° M `(arr ,B ,t)) (typeof° N B)))
    ((fresh° (R A B) (=° m `(abs ,R)) (=° t `(arr ,A ,B))
       (∀° (x) (⊃° ((typeof° x A)) (typeof° (@ R x) B)))))))

; Figure 7.3, call-by-name evaluation: β of the object language is β of the meta-language.
(define-relation (cbn° m v)
  (cond°
    ((fresh° (R) (=° m `(abs ,R)) (=° v m)))
    ((fresh° (M N R) (=° m `(app ,M ,N)) (cbn° M `(abs ,R)) (cbn° (@ R N) v)))))

; Figure 7.10, copy and subst on untyped λ-terms.
(define-dynamic-relation (copy° m n)
  (cond°
    ((fresh° (M N P Q) (=° m `(app ,M ,N)) (=° n `(app ,P ,Q)) (copy° M P) (copy° N Q)))
    ((fresh° (M N) (=° m `(abs ,M)) (=° n `(abs ,N))
       (∀° (x) (⊃° ((copy° x x)) (copy° (@ M x) (@ N x))))))))
(define-relation (subst° m t s) (∀° (x) (⊃° ((copy° x t)) (copy° (@ m x) s))))

; Figure 7.11, copy and subst on first-order formulas.
(define-dynamic-relation (copyterm° t s)
  (cond°
    ((=° t 'a) (=° s 'a))
    ((fresh° (x u) (=° t `(f ,x)) (=° s `(f ,u)) (copyterm° x u)))
    ((fresh° (x y u v) (=° t `(g ,x ,y)) (=° s `(g ,u ,v)) (copyterm° x u) (copyterm° y v)))))
(define-relation (copyform° b d)
  (cond°
    ((fresh° (B C D E op) (=° b `(,op ,B ,C)) (=° d `(,op ,D ,E)) (or° (=° op 'and) (=° op 'or) (=° op 'imp))
       (copyform° B D) (copyform° C E)))
    ((fresh° (B D op) (=° b `(,op ,B)) (=° d `(,op ,D)) (or° (=° op 'all) (=° op 'some))
       (∀° (y) (⊃° ((copyterm° y y)) (copyform° (@ B y) (@ D y))))))
    ((fresh° (x u) (=° b `(p ,x)) (=° d `(p ,u)) (copyterm° x u)))
    ((fresh° (x y u v) (=° b `(q ,x ,y)) (=° d `(q ,u ,v)) (copyterm° x u) (copyterm° y v)))))
(define-relation (substform° m t n) (∀° (x) (⊃° ((copyterm° x t)) (copyform° (@ m x) n))))

; §7.4.4, translation to and from de Bruijn syntax, with the `depth` assumptions.
(define-dynamic-relation (depth° n x))
(define-relation (trans° d t b)
  (cond°
    ((fresh° (M P) (=° t `(abs ,M)) (=° b `(ab ,P))
       (∀° (c) (⊃° ((depth° d c)) (fresh° (e) (project° ((d* d)) (=° e (+ d* 1))) (trans° e (@ M c) P))))))
    ((fresh° (M N P R) (=° t `(app ,M ,N)) (=° b `(ap ,P ,R)) (trans° d M P) (trans° d N R)))
    ((fresh° (n e) (=° b `(deb ,e)) (depth° n t) (project° ((d* d) (n* n)) (=° e (- d* n*)))))))

; Figure 7.8, prenex normal form of formulas in negation normal form.
(define-relation (atom° a) (fresh° (x y) (or° (=° a `(p ,x)) (=° a `(q ,x ,y)))))
(define-relation (quantfree° b)
  (cond°
    ((atom° b))
    ((fresh° (c) (=° b `(neg ,c)) (quantfree° c)))
    ((fresh° (op c d) (=° b `(,op ,c ,d)) (or° (=° op 'and) (=° op 'or) (=° op 'imp)) (quantfree° c) (quantfree° d)))))
(define-relation (prenex° b d)
  (cond°
    ((atom° b) (=° d b))
    ((fresh° (a) (=° b `(neg ,a)) (atom° a) (=° d b)))
    ((fresh° (op B C U V) (=° b `(,op ,B ,C)) (or° (=° op 'and) (=° op 'or)) (prenex° B U) (prenex° C V) (merge° `(,op ,U ,V) d)))
    ((fresh° (q B D) (=° b `(,q ,B)) (=° d `(,q ,D)) (or° (=° q 'all) (=° q 'some)) (∀° (x) (prenex° (@ B x) (@ D x)))))))
(define-relation (merge° b d)
  (cond°
    ((fresh° (B C D) (=° b `(and (all ,B) (all ,C))) (=° d `(all ,D))
       (∀° (x) (merge° `(and ,(@ B x) ,(@ C x)) (@ D x)))))
    ((fresh° (q B C D) (=° b `(and (,q ,B) ,C)) (=° d `(,q ,D)) (or° (=° q 'all) (=° q 'some))
       (∀° (x) (merge° `(and ,(@ B x) ,C) (@ D x)))))
    ((fresh° (q B C D) (=° b `(and ,B (,q ,C))) (=° d `(,q ,D)) (or° (=° q 'all) (=° q 'some))
       (∀° (x) (merge° `(and ,B ,(@ C x)) (@ D x)))))
    ((fresh° (B C D) (=° b `(or (some ,B) (some ,C))) (=° d `(some ,D))
       (∀° (x) (merge° `(or ,(@ B x) ,(@ C x)) (@ D x)))))
    ((fresh° (q B C D) (=° b `(or (,q ,B) ,C)) (=° d `(,q ,D)) (or° (=° q 'all) (=° q 'some))
       (∀° (x) (merge° `(or ,(@ B x) ,C) (@ D x)))))
    ((fresh° (q B C D) (=° b `(or ,B (,q ,C))) (=° d `(,q ,D)) (or° (=° q 'all) (=° q 'some))
       (∀° (x) (merge° `(or ,B ,(@ C x)) (@ D x)))))
    ((quantfree° b) (=° d b))))

; §5.3, Figures 5.3-5.4 (p.143): mappred with a relation argument, a Scheme closure here.
(define-relation (age° n a) (cond° ((=° n 'bob) (=° a 23)) ((=° n 'sue) (=° a 24)) ((=° n 'ned) (=° a 23))))
(define-relation (mappred° p l k)
  (cond°
    ((null° l) (null° k))
    ((fresh° (x xs y ys) (=° l `(,x . ,xs)) (=° k `(,y . ,ys)) (p x y) (mappred° p xs ys)))))

; §5.8.2, Figure 5.11 (p.158): a functional difference list is an abstraction over its tail, and the
; palindrome clauses destructure one by unification, `x\ Y::(F (Y::x))` being no pattern: it waits.
(define-relation (palindrome° f)
  (cond°
    ((=° f `(fdl ,(ƛ (x) x))))
    ((fresh° (Y) (=° f `(fdl ,(ƛ (x) `(,Y . ,x))))))
    ((fresh° (Y F) (=° f `(fdl ,(ƛ (x) `(,Y . ,(@ F `(,Y . ,x)))))) (palindrome° `(fdl ,F))))))

; §10.3.2, Figure 10.6 (p.273): Fischer's CPS transformation; object application `M @ N` is `(app M N)`.
; There is no clause for variables: going under a binder assumes ftrans x (abs k\ k @ y).
(define-dynamic-relation (ftrans° m f)
  (cond°
    ((fresh° (V U) (=° m `(abs ,V)) (=° f `(abs ,(ƛ (k) `(app ,k ,U)))) (phi° m U)))
    ((fresh° (M N P Q) (=° m `(app ,M ,N)) (ftrans° M P) (ftrans° N Q)
       (=° f `(abs ,(ƛ (k) `(app ,P (abs ,(ƛ (m) `(app ,Q (abs ,(ƛ (n) `(app (app ,m ,k) ,n))))))))))))))
(define-relation (phi° m u)
  (fresh° (M P) (=° m `(abs ,M)) (=° u `(abs ,(ƛ (k) `(abs ,(ƛ (x) `(app ,(@ P x) ,k))))))
    (∀° (x y) (⊃° ((ftrans° x `(abs ,(ƛ (k) `(app ,k ,y))))) (ftrans° (@ M x) (@ P y))))))
