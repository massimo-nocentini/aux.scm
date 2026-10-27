;;; (aux pie alpha) -- α-equivalence of Pie's Core expressions.
;;;
;;; A port of alpha.rkt of Pie, the language of "The Little Typer" by Daniel P. Friedman and David Thrane
;;; Christiansen (https://github.com/the-little-typer/pie, commit 2c89553). Pie is distributed under the
;;; GNU AGPL-3.0, and so is this file, which is derived from it.
;;;
;;; The file is ported whole and line for line, with the upstream names. Only α-equiv? is exported, as
;;; upstream provides only it (cf. alpha.rkt:5); bind, α-equiv-aux and α-equiv-aux* stay internal. The
;;; module+ test block of alpha.rkt:100-141 lives in the pie-alpha-suite of src/test/pie-units.scm.
;;;
;;; Two expressions are compared by walking them together and giving each binder the depth at which it
;;; occurs, its level. Two bound variables are the same when their binders have the same level, two free
;;; variables when they are the same name. Two expressions (the Absurd _) are always the same, which is
;;; the η rule for Absurd that read-back relies on, and two holes are the same when they come from the same
;;; source location, compared with equal? on their srcloc lists.
;;;
;;; Racket's match* over the two expressions becomes a match/first of (aux base) over (list e1 e2), with
;;; the clauses in upstream order; #:when becomes a ⊣ guard and the catch-all (_ _) the last clause. Typed
;;; Racket signatures are kept as comments; Typed Racket only checks them, so nothing at run time depends
;;; on them.

(module (aux pie alpha) (α-equiv?)

  (import scheme
          (chicken base)
          (aux base)
          (aux pie basics))

  ;;; ==================== PUBLIC INTERFACE ====================

  ;; α-equiv? : Core Core -> Boolean, cf. alpha.rkt:9
  (define (α-equiv? e1 e2)
    (α-equiv-aux 0 '() '() e1 e2))

  ;;; ==================== HELPERS ====================

  ;; Bindings = (Listof (Pair Symbol Natural)), each bound name with the level of its binder,
  ;; cf. alpha.rkt:17

  ;; bind : Bindings Symbol Natural -> Bindings, cf. alpha.rkt:19
  (define (bind b x lvl)
    (cons (cons x lvl)
          b))

  ;; α-equiv-aux : Natural Bindings Bindings Core Core -> Boolean, cf. alpha.rkt:24
  ;; lvl is the level that the next binder gets; b1 and b2 hold the binders around e1 and e2.
  (define (α-equiv-aux lvl b1 b2 e1 e2)
    (match/first (list e1 e2)
      (((,x ,y) ⊣ (and (symbol? x) (symbol? y)))
       (cond
         ((and (var-name? x) (var-name? y))
          (let ((x-binding (assv x b1))
                (y-binding (assv y b2)))
            (match/first (list x-binding y-binding)
              ;; Both bound
              (((_ . ,lvl-x) (_ . ,lvl-y))
               (= lvl-x lvl-y))
              ;; Both free
              ((#f #f) (eqv? x y))
              ;; One bound, one free
              ((_ _) #f))))
         ;; Constructor equality (e.g. zero ≡ zero)
         ((and (not (var-name? x)) (not (var-name? y)))
          (eqv? x y))
         ;; one constructor, one var (e.g. zero /≡ x)
         (else #f)))
      ;; Atoms must be the same atom, cf. alpha.rkt:46
      (((quote ,a) (quote ,b))
       (eqv? a b))
      ;; cf. alpha.rkt:49
      (((Π ((,x ,A1)) ,B1) (Π ((,y ,A2)) ,B2))
       (and (α-equiv-aux lvl b1 b2 A1 A2)
            (α-equiv-aux (add1 lvl)
                         (bind b1 x lvl)
                         (bind b2 y lvl)
                         B1
                         B2)))
      ;; cf. alpha.rkt:56
      (((Σ ((,x ,A1)) ,D1) (Σ ((,y ,A2)) ,D2))
       (and (α-equiv-aux lvl b1 b2 A1 A2)
            (α-equiv-aux (add1 lvl)
                         (bind b1 x lvl)
                         (bind b2 y lvl)
                         D1
                         D2)))
      ;; cf. alpha.rkt:63
      (((λ (,x) ,body1) (λ (,y) ,body2))
       (α-equiv-aux (add1 lvl)
                    (bind b1 x lvl)
                    (bind b2 y lvl)
                    body1
                    body2))
      ;; η for Absurd relies on read-back inserting an annotation, cf. alpha.rkt:69
      (((the Absurd _) (the Absurd _)) #t)
      ;; Non-binding keywords, cf. alpha.rkt:71
      ((((,kw1 . ,args1) (,kw2 . ,args2))
        ⊣ (and (symbol? kw1) (symbol? kw2)
               (not (or (eqv? kw1 'λ) (eqv? kw1 'Π) (eqv? kw1 'Σ) (eqv? kw1 'TODO)))
               (not (or (eqv? kw2 'λ) (eqv? kw2 'Π) (eqv? kw2 'Σ) (eqv? kw2 'TODO)))
               (not (var-name? kw1)) (not (var-name? kw2))))
       (and (eqv? kw1 kw2)
            (α-equiv-aux* lvl b1 b2 args1 args2)))
      ;; Holes from the same location are equal, cf. alpha.rkt:80
      (((TODO ,loc1 ,t1) (TODO ,loc2 ,t2))
       (and (equal? loc1 loc2)
            (α-equiv-aux lvl b1 b2 t1 t2)))
      ;; Function application, cf. alpha.rkt:84
      (((,f ,arg1) (,g ,arg2))
       (and (α-equiv-aux lvl b1 b2 f g)
            (α-equiv-aux lvl b1 b2 arg1 arg2)))
      ((_ _) #f)))

  ;; α-equiv-aux* : Natural Bindings Bindings (Listof Core) (Listof Core) -> Boolean, cf. alpha.rkt:91
  ;; The arguments of two keywords, pairwise; lists of different lengths are never equivalent.
  (define (α-equiv-aux* lvl b1 b2 args1 args2)
    (cond
      ((and (pair? args1) (pair? args2))
       (and (α-equiv-aux lvl b1 b2 (car args1) (car args2))
            (α-equiv-aux* lvl b1 b2 (cdr args1) (cdr args2))))
      ((and (null? args1) (null? args2)) #t)
      (else #f))))
