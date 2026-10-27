;;; (aux pie typechecker) -- type checking of Pie's expressions and definitions.
;;;
;;; A port of typechecker.rkt of Pie, the language of "The Little Typer" by Daniel P. Friedman and David
;;; Thrane Christiansen (https://github.com/the-little-typer/pie, commit 2c89553). Pie is distributed under
;;; the GNU AGPL-3.0, and so is this file, which is derived from it.
;;;
;;; The file is ported whole and in upstream order. It holds the info hook, renamings, the three judgments
;;; of the elaborator (is-type, synth and check), the sameness of types and of values, claims and
;;; definitions, atoms, and the helper make-app. Upstream provides everything it defines (cf.
;;; typechecker.rkt:9), and so does this module. The module+ test block of typechecker.rkt:874-889 lives in
;;; the pie-atom-suite of src/test/pie-units.scm.
;;;
;;; Elaboration turns the Src of the parser into Core. Each judgment matches on (src-stx e), with the
;;; clauses in upstream order, and answers a Perhaps of basics, a go of the result or a stop with a
;;; location and a message. go-on of basics chains Perhaps values as upstream does. Racket's match becomes
;;; match/first of (aux base). A quasi pattern `(Π ((,(binder x-loc x) ,A)) ,B) becomes
;;; (Π ((#(binder ,x-loc ,x) ,A)) ,B), a struct pattern (PI x A c) becomes #(PI ,x ,A ,c), the quoted
;;; symbol 'U the bare symbol U, #:when a ⊣ guard, a match* over two values a match/first on their list
;;; after a let* that computes them left to right, and a catch-all variable such as non-PI the pattern
;;; ,non-PI. The [else ...] clause of check is a pattern variable named else in Racket, which matches
;;; anything, so it is the else clause of match/first here.
;;;
;;; The view (!! p) of normalize.rkt:86, which matches p against (now v), is used here for the length of a
;;; Vec type. As in (aux pie normalize), a clause that uses it becomes a guard on (now len), followed in the
;;; body by a match1/first on (now len) when the body needs the parts. The guard runs only after the rest
;;; of the pattern matched, while Racket forces the length when its matcher reaches it, and the next clause
;;; forces the same length in both, so the difference cannot be seen.
;;;
;;; The application clause (list rator rand0 rands ... rand) of typechecker.rkt:616 has no counterpart in
;;; (aux base) patterns, which have no ellipsis. It becomes the pattern (,rator ,rand0 ,r1 . ,rs) of a list
;;; of at least three elements, with a guard that checks that the list is proper and that the arguments
;;; between rand0 and the last one are Src, as #:when does upstream, and a let that splits off the last
;;; argument as rand.
;;;
;;; Values are inspected with now exactly where upstream calls now, because the tests of tests.rkt compare
;;; contexts down to the state of their DELAY boxes. Messages are copied verbatim, typos and spacing
;;; included ("is aready used.", "Not List: ", and the symbol vecnil at the head of two messages). The
;;; require/typed of locations.rkt becomes a plain import of (aux pie locations) (cf. typechecker.rkt:12).
;;; Typed Racket signatures are kept as comments; Typed Racket only checks them, so nothing at run time
;;; depends on them.

(module (aux pie typechecker)
  ( ; reporting information
    pie-info-hook send-pie-info
    ; renamings
    rename extend-renaming
    ; the judgments
    is-type synth check same-type convert
    ; claims and definitions
    not-used get-claim add-claim remove-claim add-def
    ; atoms
    atom-ok? all-ok-atom
    ; helpers
    make-app)

  (import scheme
          (chicken base)
          (only (scheme base) make-parameter)
          (only srfi-1 every drop-right last)
          (aux base)
          (aux pie basics)
          (aux pie normalize)
          (aux pie alpha)
          (only (aux pie locations) location-for-info? not-for-info)
          (only (aux pie racket) racket-char-alphabetic?))

  ;;; ==================== REPORTING INFORMATION ====================

  ;; The info hook is a procedure that the type checker calls to report information about the type of an
  ;; expression. It is called with the position in the source file that the information is about, and the
  ;; information, which is one of
  ;;
  ;;   'definition                the position is a defined name; the slides use it to print defined
  ;;                              names in the fonts of The Little Typer
  ;;   (binding-site TYPE)        the position binds a variable of type TYPE, for the tooltips of DrRacket
  ;;                              and for the slides
  ;;   (is-type TYPE)             the position is the type TYPE
  ;;   (has-type TYPE)            the position is an expression of type TYPE, found by checking or by
  ;;                              synthesis, for the tooltips of DrRacket and for the slides
  ;;   (TODO Γ TYPE)              the position is a TODO that should have the type TYPE in the context Γ,
  ;;                              a Serializable-Ctx, for the tooltips and the todo list of DrRacket, for
  ;;                              the slides, and for the hole reports of main
  ;;
  ;; cf. typechecker.rkt:18-47

  ;; pie-info-hook : (Parameterof (Loc Info -> Void)), by default a hook that does nothing,
  ;; cf. typechecker.rkt:48
  (define pie-info-hook
    (make-parameter (λ (where what) (void))))

  ;; send-pie-info : Loc Info -> Void, calls the hook only when the location is meant for information,
  ;; cf. typechecker.rkt:58
  (define (send-pie-info where what)
    (when (location-for-info? where)
      ((pie-info-hook) where what)))

  ;;; ==================== RENAMINGS ====================

  ;; The elaborator gives each entry of Γ a distinct name, so that users can find more easily the mistakes
  ;; that shadowing causes. Shadowed bindings are renamed, and the renamings are tracked during
  ;; elaboration, cf. typechecker.rkt:72-75
  ;;
  ;; Renaming = (Listof (Pair Symbol Symbol)), cf. typechecker.rkt:76

  ;; rename : Renaming Symbol -> Symbol, cf. typechecker.rkt:78
  (define (rename r x)
    (match/first (assv x r)
      (#f x)
      ((_ . ,y) y)))

  ;; extend-renaming : Renaming Symbol Symbol -> Renaming, cf. typechecker.rkt:84
  (define (extend-renaming r from to)
    (cons (cons from to) r))

  ;;; ==================== Γ ⊢ e type ↝ c ====================

  ;; is-type : Ctx Renaming Src -> (Perhaps Core), checks that in is a type and elaborates it. Desugared
  ;; forms are rebuilt with (src-loc in), so their parts still send information, cf. typechecker.rkt:91
  (define (is-type Γ r in)
    (define the-type
      (match/first (src-stx in)
        (U (go 'U))
        (Nat (go 'Nat))
        ((-> ,A ,B)
         (let1 (x (fresh-binder Γ B 'x))
           (go-on ((,A-out (is-type Γ r A))
                   (,B-out (is-type (bind-free Γ
                                               x
                                               (val-in-ctx Γ A-out))
                                    r
                                    B)))
             (go `(Π ((,x ,A-out)) ,B-out)))))
        ((-> ,A ,B ,C . ,C*)
         (let1 (x (fresh-binder Γ (make-app B C C*) 'x))
           (go-on ((,A-out (is-type Γ r A))
                   (,t-out (is-type (bind-free Γ x (val-in-ctx Γ A-out))
                                    r
                                    (@ (src-loc in)
                                       `(-> ,B ,C . ,C*)))))
             (go `(Π ((,x ,A-out)) ,t-out)))))
        ((Π ((#(binder ,x-loc ,x) ,A)) ,B)
         (let1 (y (fresh Γ x))
           (go-on ((,A-out (is-type Γ r A))
                   (,A-outv (go (val-in-ctx Γ A-out)))
                   (,B-out (is-type (bind-free Γ y A-outv) (extend-renaming r x y) B)))
             (begin ((pie-info-hook) x-loc `(binding-site ,A-out))
                    (go `(Π ((,y ,A-out)) ,B-out))))))
        ((Π ((#(binder ,x-loc ,x) ,A) (,y ,A1) . ,more) ,B)
         (let1 (z (fresh Γ x))
           (go-on ((,A-out (is-type Γ r A))
                   (,A-outv (go (val-in-ctx Γ A-out)))
                   (,B-out (is-type (bind-free Γ z A-outv)
                                    (extend-renaming r x z)
                                    (@ (src-loc in)
                                       `(Π ((,y ,A1) . ,more) ,B)))))
             (begin ((pie-info-hook) x-loc `(binding-site ,A-out))
                    (go `(Π ((,z ,A-out)) ,B-out))))))
        (Atom
         (go 'Atom))
        ((Pair ,A ,D)
         (let1 (x (fresh-binder Γ D 'x))
           (go-on ((,A-out (is-type Γ r A))
                   (,D-out (is-type (bind-free Γ x (val-in-ctx Γ A-out))
                                    r
                                    D)))
             (go `(Σ ((,x ,A-out)) ,D-out)))))
        ((Σ ((#(binder ,x-loc ,x) ,A)) ,D)
         (let1 (y (fresh Γ x))
           (go-on ((,A-out (is-type Γ r A))
                   (,A-outv (go (val-in-ctx Γ A-out)))
                   (,D-out (is-type (bind-free Γ y A-outv)
                                    (extend-renaming r x y)
                                    D)))
             (begin ((pie-info-hook) x-loc `(binding-site ,A-out))
                    (go `(Σ ((,y ,A-out)) ,D-out))))))
        ((Σ ((#(binder ,x-loc ,x) ,A) (,y ,A1) . ,more) ,D)
         (let1 (z (fresh Γ x))
           (go-on ((,A-out (is-type Γ r A))
                   (,A-outv (go (val-in-ctx Γ A-out)))
                   (,D-out (is-type (bind-free Γ z A-outv)
                                    (extend-renaming r x z)
                                    (@ (src-loc in)
                                       `(Σ ((,y ,A1) . ,more) ,D)))))
             (begin ((pie-info-hook) x-loc `(binding-site ,A-out))
                    (go `(Σ ((,z ,A-out)) ,D-out))))))
        (Trivial (go 'Trivial))
        ((List ,E)
         (go-on ((,E-out (is-type Γ r E)))
           (go `(List ,E-out))))
        (Absurd (go 'Absurd))
        ((= ,A ,from ,to)
         (go-on ((,A-out (is-type Γ r A))
                 (,Av (go (val-in-ctx Γ A-out)))
                 (,from-out (check Γ r from Av))
                 (,to-out (check Γ r to Av)))
           (go `(= ,A-out ,from-out ,to-out))))
        ((Vec ,E ,len)
         (go-on ((,E-out (is-type Γ r E))
                 (,len-out (check Γ r len 'NAT)))
           (go `(Vec ,E-out ,len-out))))
        ((Either ,L ,R)
         (go-on ((,L-out (is-type Γ r L))
                 (,R-out (is-type Γ r R)))
           (go `(Either ,L-out ,R-out))))
        (,other
         (match/first (check Γ r (@ (src-loc in) other) 'UNIVERSE)
           (#(go ,t-out)
            (go t-out))
           (#(stop ,where ,why)
            (cond
              ((and (symbol? other) (var-name? other))
               (go-on ((,other-tv (var-type Γ (src-loc in) other)))
                 (stop (src-loc in)
                       `("Expected" U
                                    "but given"
                                    ,(read-back-type Γ other-tv)))))
              (else
               (stop (src-loc in) `("Not a type")))))))))
    (go-on ((,t the-type))
      (begin (send-pie-info (src-loc in) `(is-type ,t))
             (go t))))

  ;;; ==================== Γ ⊢ e synth ↝ (the c c) ====================

  ;; synth : Ctx Renaming Src -> (Perhaps (the Core Core)), finds the type of e and elaborates it. Desugared
  ;; forms are rebuilt with (not-for-info (src-loc e)), so that they send no information of their own,
  ;; cf. typechecker.rkt:200
  (define (synth Γ r e)
    (define the-expr
      (match/first (src-stx e)
        (Nat (go '(the U Nat)))
        (U (stop (src-loc e)
                 '(U
                   "is a type, but it does not have a type.")))
        ((-> ,A ,B)
         (let1 (z (fresh-binder Γ B 'x))
           (go-on ((,A-out (check Γ r A 'UNIVERSE))
                   (,B-out (check (bind-free Γ z (val-in-ctx Γ A-out))
                                  r
                                  B
                                  'UNIVERSE)))
             (go `(the U (Π ((,z ,A-out)) ,B-out))))))
        ((-> ,A ,B ,C . ,C*)
         (let1 (z (fresh-binder Γ (make-app B C C*) 'x))
           (go-on ((,A-out (check Γ r A 'UNIVERSE))
                   (,t-out (check (bind-free Γ z (val-in-ctx Γ A-out))
                                  r
                                  (@ (not-for-info (src-loc e))
                                     `(-> ,B ,C . ,C*))
                                  'UNIVERSE)))
             (go `(the U (Π ((,z ,A-out)) ,t-out))))))
        ((Π ((#(binder ,x-loc ,x) ,A)) ,B)
         (let1 (x^ (fresh Γ x))
           (go-on ((,A-out (check Γ r A 'UNIVERSE))
                   (,B-out (check (bind-free Γ x^
                                             (val-in-ctx Γ A-out))
                                  (extend-renaming r x x^)
                                  B
                                  'UNIVERSE)))
             (begin ((pie-info-hook) x-loc `(binding-site ,A-out))
                    (go `(the U (Π ((,x^ ,A-out)) ,B-out)))))))
        ((Π ((#(binder ,x-loc ,x) ,A) (,y ,A1) . ,more) ,B)
         (let1 (x^ (fresh Γ x))
           (go-on ((,A-out (check Γ r A 'UNIVERSE))
                   (,B-out (check (bind-free Γ x^ (val-in-ctx Γ A-out))
                                  (extend-renaming r x x^)
                                  (@ (not-for-info (src-loc e))
                                     `(Π ((,y ,A1) . ,more) ,B))
                                  'UNIVERSE)))
             (begin ((pie-info-hook) x-loc `(binding-site ,A-out))
                    (go `(the U (Π ((,x^ ,A-out)) ,B-out)))))))
        (zero (go '(the Nat zero)))
        ((add1 ,n)
         (go-on ((,n-out (check Γ r n 'NAT)))
           (go `(the Nat (add1 ,n-out)))))
        ((which-Nat ,tgt ,b ,s)
         (go-on ((,tgt-out (check Γ r tgt 'NAT))
                 ((the ,b-t-out ,b-out)
                  (synth Γ r b))
                 (,s-out
                  (check Γ
                         r
                         s
                         (let1 (n-1 (fresh Γ 'n-1))
                           (PI n-1 'NAT (FO-CLOS (ctx->env Γ) n-1 b-t-out))))))
           (go `(the ,b-t-out
                     (which-Nat ,tgt-out (the ,b-t-out ,b-out) ,s-out)))))
        ((iter-Nat ,tgt ,b ,s)
         (go-on ((,tgt-out (check Γ r tgt 'NAT))
                 ((the ,b-t-out ,b-out)
                  (synth Γ r b))
                 (,s-out
                  (check Γ
                         r
                         s
                         (let1 (old (fresh Γ 'old))
                           (val-in-ctx Γ `(Π ((,old ,b-t-out))
                                            ,b-t-out))))))
           (go `(the ,b-t-out
                     (iter-Nat ,tgt-out (the ,b-t-out ,b-out) ,s-out)))))
        ((rec-Nat ,tgt ,b ,s)
         (go-on ((,tgt-out (check Γ r tgt 'NAT))
                 ((the ,b-t-out ,b-out)
                  (synth Γ r b))
                 (,s-out
                  (check Γ
                         r
                         s
                         (let ((n-1 (fresh Γ 'n-1))
                               (old (fresh Γ 'old)))
                           (val-in-ctx Γ `(Π ((,n-1 Nat))
                                            (Π ((,old ,b-t-out))
                                              ,b-t-out)))))))
           (go `(the ,b-t-out
                     (rec-Nat ,tgt-out (the ,b-t-out ,b-out) ,s-out)))))
        ((ind-Nat ,tgt ,mot ,b ,s)
         (go-on ((,tgt-out (check Γ r tgt 'NAT))
                 (,mot-out (check Γ r mot (PI 'n 'NAT (HO-CLOS (λ (n) 'UNIVERSE)))))
                 (,mot-val (go (val-in-ctx Γ mot-out)))
                 (,b-out (check Γ r b (do-ap mot-val 'ZERO)))
                 (,s-out (check
                          Γ
                          r
                          s
                          (Π-type ((n-1 'NAT)
                                   (ih (do-ap mot-val n-1)))
                                  (do-ap mot-val (ADD1 n-1))))))
           (go `(the (,mot-out ,tgt-out)
                     (ind-Nat ,tgt-out ,mot-out ,b-out ,s-out)))))
        (Atom (go '(the U Atom)))
        ((Pair ,A ,D)
         (let1 (a (fresh Γ 'a))
           (go-on ((,A-out (check Γ r A 'UNIVERSE))
                   (,D-out (check (bind-free Γ a (val-in-ctx Γ A-out))
                                  r
                                  D
                                  'UNIVERSE)))
             (go `(the U (Σ ((,a ,A-out)) ,D-out))))))
        ((Σ ((#(binder ,x-loc ,x) ,A)) ,D)
         (let1 (x^ (fresh Γ x))
           (go-on ((,A-out (check Γ r A 'UNIVERSE))
                   (,D-out (check (bind-free Γ
                                             x^
                                             (val-in-ctx Γ A-out))
                                  (extend-renaming r x x^)
                                  D
                                  'UNIVERSE)))
             (begin ((pie-info-hook) x-loc `(binding-site ,A-out))
                    (go `(the U (Σ ((,x^ ,A-out)) ,D-out)))))))
        ((Σ ((#(binder ,x-loc ,x) ,A) (,y ,A1) . ,more) ,D)
         (let1 (x^ (fresh Γ x))
           (go-on ((,A-out (check Γ r A 'UNIVERSE))
                   (,D-out (check (bind-free Γ x^ (val-in-ctx Γ A-out))
                                  (extend-renaming r x x^)
                                  (@ (not-for-info (src-loc e))
                                     `(Σ ((,y ,A1) . ,more) ,D))
                                  'UNIVERSE)))
             (begin ((pie-info-hook) x-loc `(binding-site ,A-out))
                    (go `(the U (Σ ((,x^ ,A-out)) ,D-out)))))))
        ((car ,p)
         (go-on (((the ,p-t ,p-out) (synth Γ r p)))
           (match/first (val-in-ctx Γ p-t)
             (#(SIGMA ,x ,A ,clos)
              (go `(the ,(read-back-type Γ A) (car ,p-out))))
             (,non-Sigma
              (stop (src-loc e)
                    `("Not a pair type:" ,(read-back-type Γ non-Sigma)))))))
        ((cdr ,p)
         (go-on (((the ,p-t ,p-out) (synth Γ r p)))
           (match/first (val-in-ctx Γ p-t)
             (#(SIGMA ,x ,A ,c)
              (go `(the ,(read-back-type Γ
                                         (val-of-closure c (do-car (val-in-ctx Γ p-out))))
                        (cdr ,p-out))))
             (,non-Sigma
              (stop (src-loc e)
                    `("Not a pair type:" ,(read-back-type Γ non-Sigma)))))))
        ((quote ,a)
         (if (atom-ok? a)
             (go `(the Atom (quote ,a)))
             (stop (src-loc e) '("Atoms consist of letters and hyphens."))))
        (Trivial
         (go '(the U Trivial)))
        (sole
         (go '(the Trivial sole)))
        ((ind-List ,tgt ,mot ,b ,s)
         (go-on (((the ,tgt-t ,tgt-out) (synth Γ r tgt)))
           (match/first (val-in-ctx Γ tgt-t)
             (#(LIST ,E)
              (go-on ((,mot-out (check
                                 Γ
                                 r
                                 mot
                                 (PI 'xs (LIST E) (FO-CLOS (ctx->env Γ) 'xs 'U))))
                      (,mot-val (go (val-in-ctx Γ mot-out)))
                      (,b-out (check Γ r b (do-ap mot-val 'NIL)))
                      (,s-out
                       (check Γ
                              r
                              s
                              (Π-type ((e E)
                                       (es (LIST E))
                                       (ih (do-ap mot-val es)))
                                      (do-ap mot-val (|LIST::| e es))))))
                (go `(the (,mot-out ,tgt-out)
                          (ind-List ,tgt-out
                                    ,mot-out
                                    ,b-out
                                    ,s-out)))))
             (,other (stop (src-loc e)
                           `("Not List: "
                             ,(read-back-type Γ other)))))))
        ((rec-List ,tgt ,b ,s)
         (go-on (((the ,tgt-t ,tgt-out) (synth Γ r tgt)))
           (match/first (val-in-ctx Γ tgt-t)
             (#(LIST ,E)
              (go-on (((the ,b-t-out ,b-out) (synth Γ r b))
                      (,b-t-val (go (val-in-ctx Γ b-t-out)))
                      (,s-out (check
                               Γ
                               r
                               s
                               (Π-type ((e E)
                                        (es (LIST E))
                                        (ih b-t-val))
                                       b-t-val))))
                (go `(the ,b-t-out
                          (rec-List ,tgt-out
                                    (the ,b-t-out ,b-out)
                                    ,s-out)))))
             (,other (stop (src-loc e)
                           `("Not List: "
                             ,(read-back-type Γ other)))))))
        ((List ,E)
         (go-on ((,E-out (check Γ r E 'UNIVERSE)))
           (go `(the U (List ,E-out)))))
        ((|::| ,e ,es)
         (go-on (((the ,E ,e-out) (synth Γ r e))
                 (,es-out (check Γ r es (val-in-ctx Γ `(List ,E)))))
           (go `(the (List ,E) (|::| ,e-out ,es-out)))))
        (Absurd
         (go '(the U Absurd)))
        ((ind-Absurd ,tgt ,mot)
         (go-on ((,tgt-out (check Γ r tgt 'ABSURD))
                 (,mot-out (check Γ r mot 'UNIVERSE)))
           (go `(the ,mot-out (ind-Absurd ,tgt-out ,mot-out)))))
        ((= ,A ,from ,to)
         (go-on ((,A-out (check Γ r A 'UNIVERSE))
                 (,A-v (go (val-in-ctx Γ A-out)))
                 (,from-out (check Γ r from A-v))
                 (,to-out (check Γ r to A-v)))
           (go `(the U (= ,A-out ,from-out ,to-out)))))
        ((replace ,tgt ,mot ,b)
         (go-on (((the ,tgt-t-out ,tgt-out) (synth Γ r tgt)))
           (match/first (val-in-ctx Γ tgt-t-out)
             (#(EQUAL ,Av ,fromv ,tov)
              (go-on ((,mot-out (check Γ
                                       r
                                       mot
                                       (Π-type ((x Av)) 'UNIVERSE)))
                      (,b-out (check Γ r b (do-ap (val-in-ctx Γ mot-out)
                                                  fromv))))
                (go `(the ,(read-back-type Γ (do-ap (val-in-ctx Γ mot-out)
                                                    tov))
                          (replace ,tgt-out ,mot-out ,b-out)))))
             (,non-equal
              (stop
               (src-loc e)
               `("Expected an expression with = type, but the type was"
                 ,tgt-t-out))))))
        ((trans ,p1 ,p2)
         (go-on (((the ,p1-t-out ,p1-out) (synth Γ r p1))
                 ((the ,p2-t-out ,p2-out) (synth Γ r p2)))
           (let* ((p1-t-v (val-in-ctx Γ p1-t-out))
                  (p2-t-v (val-in-ctx Γ p2-t-out)))
             (match/first (list p1-t-v p2-t-v)
               ((#(EQUAL ,Av ,from-v ,mid-v) #(EQUAL ,Bv ,mid-v2 ,to-v))
                (go-on ((_ (same-type Γ (src-loc e) Av Bv))
                        (_ (convert Γ (src-loc e) Av mid-v mid-v2)))
                  (go `(the ,(read-back-type Γ (EQUAL Av from-v to-v))
                            (trans ,p1-out ,p2-out)))))
               ((,non=1 ,non=2)
                (stop (src-loc e)
                      `("Expected =, got"
                        ,(read-back-type Γ non=1) "and"
                        ,(read-back-type Γ non=2))))))))
        ((cong ,p ,f)
         (go-on (((the ,p-t-out ,p-out) (synth Γ r p))
                 ((the ,f-t-out ,f-out) (synth Γ r f)))
           (match/first (val-in-ctx Γ p-t-out)
             (#(EQUAL ,Av ,from-v ,to-v)
              (match/first (val-in-ctx Γ f-t-out)
                (#(PI ,x ,Bv ,c)
                 (go-on ((_ (same-type Γ (src-loc e) Av Bv))
                         (,Cv (go (val-of-closure c from-v)))
                         (,f-v (go (val-in-ctx Γ f-out))))
                   (go `(the (= ,(read-back-type Γ Cv)
                                ,(read-back Γ Cv (do-ap f-v from-v))
                                ,(read-back Γ Cv (do-ap f-v to-v)))
                             (cong ,p-out ,(read-back-type Γ Cv) ,f-out)))))
                (,non-Pi
                 (stop (src-loc e) `("Expected a function, got" ,(read-back-type Γ non-Pi))))))
             (,non= (stop (src-loc e) `("Expected =, got" ,(read-back-type Γ non=)))))))
        ((symm ,p)
         (go-on (((the ,p-t-out ,p-out) (synth Γ r p)))
           (match/first (val-in-ctx Γ p-t-out)
             (#(EQUAL ,Av ,from-v ,to-v)
              (go `(the ,(read-back-type Γ (EQUAL Av to-v from-v))
                        (symm ,p-out))))
             (,non=
              (stop (src-loc e)
                    `("Expected =, got" ,(read-back-type Γ non=)))))))
        ((ind-= ,tgt ,mot ,base)
         (go-on (((the ,tgt-t ,tgt-out) (synth Γ r tgt)))
           (match/first (val-in-ctx Γ tgt-t)
             (#(EQUAL ,Av ,from-v ,to-v)
              (go-on ((,mot-out (check Γ r mot (Π-type ((to Av)
                                                        (p (EQUAL Av from-v to)))
                                                       'UNIVERSE)))
                      (,mot-v (go (val-in-ctx Γ mot-out)))
                      (,base-out (check Γ r base (do-ap (do-ap mot-v from-v)
                                                        (SAME from-v)))))
                (go `(the ,(read-back-type Γ (do-ap (do-ap mot-v to-v)
                                                    (val-in-ctx Γ tgt-out)))
                          (ind-= ,tgt-out
                                 ,mot-out
                                 ,base-out)))))
             (,non= (stop (src-loc e) `("Expected evidence of equality, got "
                                        ,(read-back-type Γ non=)))))))
        ((Vec ,E ,len)
         (go-on ((,E-out (check Γ r E 'UNIVERSE))
                 (,len-out (check Γ r len 'NAT)))
           (go `(the U (Vec ,E-out ,len-out)))))
        ((head ,es)
         (go-on (((the ,es-type-out ,es-out)
                  (synth Γ r es)))
           (match/first (now (val-in-ctx Γ es-type-out))
             ;; (VEC Ev (!! (ADD1 len-1))), see the header
             ((#(VEC ,Ev ,len) ⊣ (ADD1? (now len)))
              (go `(the ,(read-back-type Γ Ev)
                        (head ,es-out))))
             (#(VEC ,Ev ,non-add1)
              (stop
               (src-loc e)
               `("Expected a Vec with add1 at the top of the length, got"
                 ,(read-back Γ 'NAT non-add1))))
             (,non-Vec
              (stop (src-loc e)
                    `("Expected a Vec, got"
                      ,(read-back-type Γ non-Vec)))))))
        ((tail ,es)
         (go-on (((the ,es-type-out ,es-out)
                  (synth Γ r es)))
           (match/first (now (val-in-ctx Γ es-type-out))
             ;; (VEC Ev (!! (ADD1 len-1))), see the header
             ((#(VEC ,Ev ,len) ⊣ (ADD1? (now len)))
              (match1/first (#(ADD1 ,len-1) (now len))
                (go `(the (Vec ,(read-back-type Γ Ev)
                               ,(read-back Γ 'NAT len-1))
                          (tail ,es-out)))))
             (#(VEC ,Ev ,non-add1)
              (stop
               (src-loc e)
               `("Expected a Vec with add1 at the top of the length, got"
                 ,(read-back Γ 'NAT non-add1))))
             (,non-Vec
              (stop (src-loc e)
                    `("Expected a Vec, got"
                      ,(read-back-type Γ non-Vec)))))))
        ((ind-Vec ,len ,vec ,mot ,b ,s)
         (go-on ((,len-out (check Γ r len 'NAT))
                 (,len-v (go (val-in-ctx Γ len-out)))
                 ((the ,vec-t ,vec-out) (synth Γ r vec)))
           (match/first (val-in-ctx Γ vec-t)
             (#(VEC ,Ev ,len2-v)
              (go-on ((_ (convert Γ (src-loc vec) 'NAT len-v len2-v))
                      (,mot-out (check
                                 Γ
                                 r
                                 mot
                                 (Π-type ((k 'NAT)
                                          (es (VEC Ev k)))
                                         'UNIVERSE)))
                      (,mot-val (go (val-in-ctx Γ mot-out)))
                      (,b-out (check Γ
                                     r
                                     b
                                     (do-ap (do-ap mot-val 'ZERO) 'VECNIL)))
                      (,s-out (check
                               Γ
                               r
                               s
                               (ind-Vec-step-type Ev mot-val))))
                (go `(the ((,mot-out ,len-out) ,vec-out)
                          (ind-Vec ,len-out
                                   ,vec-out
                                   ,mot-out
                                   ,b-out
                                   ,s-out)))))
             (,non-Vec
              (stop (src-loc e)
                    `("Expected a Vec, got"
                      ,(read-back-type Γ non-Vec)))))))
        ((Either ,L ,R)
         (go-on ((,L-out (check Γ r L 'UNIVERSE))
                 (,R-out (check Γ r R 'UNIVERSE)))
           (go `(the U (Either ,L-out ,R-out)))))
        ((ind-Either ,tgt ,mot ,on-left ,on-right)
         (go-on (((the ,tgt-t ,tgt-out) (synth Γ r tgt)))
           (match/first (val-in-ctx Γ tgt-t)
             (#(EITHER ,Lv ,Rv)
              (go-on ((,mot-out (check Γ r mot (Π-type ((x (EITHER Lv Rv))) 'UNIVERSE)))
                      (,mot-val (go (val-in-ctx Γ mot-out)))
                      (,l-out (check Γ r on-left (Π-type ((x Lv)) (do-ap mot-val (LEFT x)))))
                      (,r-out (check Γ r on-right (Π-type ((x Rv)) (do-ap mot-val (RIGHT x))))))
                (go `(the (,mot-out ,tgt-out)
                          (ind-Either ,tgt-out ,mot-out ,l-out ,r-out)))))
             (,non-Either
              (stop (src-loc e)
                    `("Expected an Either, but got a"
                      ,(read-back-type Γ non-Either)))))))
        ((the ,t ,e)
         (go-on ((,t-out (is-type Γ r t))
                 (,e-out (check Γ r e (val-in-ctx Γ t-out))))
           (go `(the ,t-out ,e-out))))
        ;; Γ ⊢ f synth ↝ (the (Π ((x A)) B) f')
        ;; Γ ⊢ a ∈ A ↝ a'
        ;; ----------------------------------------
        ;; Γ ⊢ (f a) synth ↝ (the B[a'/x] (f' a'))
        (((,rator ,rand) ⊣ (src? rator))
         (go-on (((the ,rator-t ,rator-out)
                  (synth Γ r rator)))
           (match/first (val-in-ctx Γ rator-t)
             (#(PI ,x ,A ,c)
              (go-on ((,rand-out (check Γ r rand A)))
                (go `(the ,(read-back-type
                            Γ
                            (val-of-closure c (val-in-ctx Γ rand-out)))
                          (,rator-out ,rand-out)))))
             (,non-PI (stop (src-loc e)
                            `("Not a function type:" ,(read-back-type Γ non-PI)))))))
        ;; Γ ⊢ (f x y ...) synth ↝ (the (Π ((x A)) B) app')
        ;; Γ ⊢ z ∈ A ↝ z'
        ;; ---------------------------------------------------
        ;; Γ ⊢ (f x y ... z) synth ↝ (the B[z'/x] (app' z'))
        ;;
        ;; The pattern (list rator rand0 rands ... rand) of upstream, see the header.
        (((,rator ,rand0 ,r1 . ,rs) ⊣ (and (src? rator)
                                         (list? rs)
                                         (every src? (drop-right (cons r1 rs) 1))))
         (let ((rands (drop-right (cons r1 rs) 1))
               (rand (last (cons r1 rs))))
           (go-on (((the ,app0-t ,app0)
                    (synth Γ r (@ (not-for-info (src-loc e)) (cons rator (cons rand0 rands))))))
             (match/first (val-in-ctx Γ app0-t)
               (#(PI ,x ,A ,c)
                (go-on ((,rand-out (check Γ r rand A)))
                  (go `(the ,(read-back-type
                              Γ
                              (val-of-closure c (val-in-ctx Γ rand-out)))
                            (,app0 ,rand-out)))))
               (,non-PI (stop (src-loc e)
                              `("Not a function type:" ,(read-back-type Γ non-PI))))))))
        (,x
         (cond ((and (symbol? x) (var-name? x))
                (let1 (real-x (rename r x))
                  (go-on ((,x-tv (var-type Γ (src-loc e) real-x)))
                    (begin (match/first (assv real-x Γ)
                             ((_ . #(def _ _))
                              (send-pie-info (src-loc e) 'definition))
                             (else (void)))
                           (go `(the ,(read-back-type Γ x-tv) ,real-x))))))
               ((number? x)
                ;; no else, as upstream, since the parser makes only natural literals
                (cond ((zero? x)
                       (go '(the Nat zero)))
                      ((positive? x)
                       (go-on ((,n-1-out (check Γ
                                                r
                                                (@ (src-loc e) (sub1 x))
                                                'NAT)))
                         (go `(the Nat (add1 ,n-1-out)))))))
               (else
                (stop (src-loc e)
                      '("Can't determine a type")))))))
    (go-on (((the ,ty ,out) the-expr))
      (begin (send-pie-info (src-loc e) `(has-type ,ty))
             the-expr)))

  ;;; ==================== Γ ⊢ e ∈ e ↝ c ====================

  ;; check : Ctx Renaming Src Value -> (Perhaps Core), checks that e has the type tv and elaborates it. The
  ;; forms that need their type, the constructors, look at (now tv); every other form is synthesized and
  ;; its type compared with tv, cf. typechecker.rkt:658
  (define (check Γ r e tv)
    (define out
      (match/first (src-stx e)
        ((λ (#(binder ,x-loc ,x)) ,b)
         (match/first (now tv)
           (#(PI ,y ,A ,c)
            (let1 (x^ (fresh Γ x))
              (go-on ((,b-out (check (bind-free Γ x^ A)
                                     (extend-renaming r x x^)
                                     b
                                     (val-of-closure c (NEU A (N-var x^))))))
                (begin ((pie-info-hook) x-loc `(binding-site ,(read-back-type Γ A)))
                       (go `(λ (,x^) ,b-out))))))
           (,non-PI
            (stop (src-loc e)
                  `("Not a function type:"
                    ,(read-back-type Γ non-PI))))))
        ((λ (,x ,y . ,xs) ,b)
         (check Γ
                r
                (@ (src-loc e)
                   `(λ (,x)
                      ,(@ (not-for-info (src-loc e))
                          `(λ (,y . ,xs) ,b))))
                tv))
        ((cons ,a ,d)
         (match/first (now tv)
           (#(SIGMA ,x ,A ,c)
            (go-on ((,a-out (check Γ r a A))
                    (,d-out (check Γ
                                   r
                                   d
                                   (val-of-closure c (val-in-ctx Γ a-out)))))
              (go `(cons ,a-out ,d-out))))
           (,non-Sigma
            (stop (src-loc e)
                  `("cons requires a Pair or Σ type, but was used as a"
                    ,(read-back-type Γ non-Sigma))))))
        (nil
         (match/first (now tv)
           (#(LIST ,E)
            (go 'nil))
           (,non-List
            (stop (src-loc e)
                  `("nil requires a List type, but was used as a"
                    ,(read-back-type Γ non-List))))))
        ((same ,c)
         (match/first (now tv)
           (#(EQUAL ,Av ,fromv ,tov)
            (go-on ((,c-out (check Γ r c Av))
                    (,v (go (val-in-ctx Γ c-out)))
                    (_ (convert Γ (src-loc c) Av fromv v))
                    (_ (convert Γ (src-loc c) Av tov v)))
              (go `(same ,c-out))))
           (,non-=
            (stop (src-loc e)
                  `("same requires an = type, but was used as a"
                    ,(read-back-type Γ non-=))))))
        (vecnil
         (match/first (now tv)
           ;; (VEC Ev (!! 'ZERO)), see the header
           ((#(VEC ,Ev ,len) ⊣ (eq? (now len) 'ZERO))
            (go 'vecnil))
           (#(VEC ,Ev ,non-zero)
            (stop (src-loc e)
                  `(vecnil
                    "requires that the length be zero, not"
                    ,(read-back Γ 'NAT non-zero))))
           (,non-Vec
            (stop (src-loc e)
                  `(vecnil
                    "must be a Vec, but was used in a"
                    ,(read-back-type Γ non-Vec)
                    "context.")))))
        ((|vec::| ,h ,t)
         (match/first (now tv)
           ;; (VEC Ev (!! (ADD1 len-1))), see the header
           ((#(VEC ,Ev ,len) ⊣ (ADD1? (now len)))
            (match1/first (#(ADD1 ,len-1) (now len))
              (go-on ((,h-out (check Γ r h Ev))
                      (,t-out (check Γ r t (VEC Ev len-1))))
                (go `(|vec::| ,h-out ,t-out)))))
           (#(VEC ,Ev ,non-add1)
            (stop (src-loc e)
                  `("vec:: requires that the length have add1 on top, not"
                    ,(read-back Γ 'NAT non-add1))))
           (,non-Vec
            (stop (src-loc e)
                  `("vec:: must be a Vec, but was used in a"
                    ,(read-back-type Γ non-Vec)
                    "context.")))))
        ((left ,l)
         (match/first (now tv)
           (#(EITHER ,Lv ,Rv)
            (go-on ((,l-out (check Γ r l Lv)))
              (go `(left ,l-out))))
           (,non-Either
            (stop (src-loc e)
                  `("left constructs an Either, but it was used where a"
                    ,(read-back-type Γ non-Either)
                    "was expected.")))))
        ((right ,rght)
         (match/first (now tv)
           (#(EITHER ,Lv ,Rv)
            (go-on ((,r-out (check Γ r rght Rv)))
              (go `(right ,r-out))))
           (,non-Either
            (stop (src-loc e)
                  `("right constructs an Either, but it was used where a"
                    ,(read-back-type Γ non-Either)
                    "was expected.")))))
        (TODO
         ;; the context is read back even when the hook will not be called, as upstream
         (let1 (ty (read-back-type Γ tv))
           (begin (send-pie-info (src-loc e) `(TODO ,(read-back-ctx Γ) ,ty))
                  (go `(TODO ,(location->srcloc (src-loc e)) ,ty)))))
        (else (go-on (((the ,t-out ,e-out) (synth Γ r e))
                      (_ (same-type Γ (src-loc e) (val-in-ctx Γ t-out) tv)))
                (go e-out)))))
    (go-on ((,ok out))
      (begin (send-pie-info (src-loc e) `(has-type ,(read-back-type Γ tv)))
             out)))

  ;;; ==================== Γ ⊢ c ≡ c type ====================

  ;; same-type : Ctx Loc Value Value -> (Perhaps Void). Racket's let evaluates its bindings left to right,
  ;; which let* makes explicit here, cf. typechecker.rkt:782
  (define (same-type Γ where given expected)
    (let* ((given-e (read-back-type Γ given))
           (expected-e (read-back-type Γ expected)))
      (if (α-equiv? given-e expected-e)
          (go (void))
          (stop where
                `("Expected" ,(read-back-type Γ expected)
                             "but given" ,(read-back-type Γ given))))))

  ;;; ==================== Γ ⊢ c ≡ c : c ====================

  ;; convert : Ctx Loc Value Value Value -> (Perhaps Void), whether av and bv are the same value of the
  ;; type tv. let* as in same-type, cf. typechecker.rkt:795
  (define (convert Γ where tv av bv)
    (let* ((a (read-back Γ tv av))
           (b (read-back Γ tv bv)))
      (if (α-equiv? a b)
          (go (void))
          (stop where
                `("The expressions"
                  ,(read-back Γ tv av)
                  "and"
                  ,(read-back Γ tv bv)
                  "are not the same"
                  ,(read-back-type Γ tv))))))

  ;;; ==================== CLAIMS AND DEFINITIONS ====================

  ;; not-used : Ctx Loc Symbol -> (Perhaps #t), cf. typechecker.rkt:812
  (define (not-used Γ where x)
    (if (pair? (assv x Γ))
        (stop where `("The name" ,x "is aready used."))
        (go #t)))

  ;; get-claim : Ctx Loc Symbol -> (Perhaps Value), the type claimed for x, which must not be defined yet,
  ;; cf. typechecker.rkt:818
  (define (get-claim Γ where x)
    (match/first Γ
      (() (stop where `("No claim:" ,x)))
      ((((,y . #(def _ _)) . ,Γ-next) ⊣ (eqv? x y))
       (stop where `("The name" ,x "is already defined.")))
      ((((,y . #(claim ,tv)) . ,Γ-next) ⊣ (eqv? x y))
       (go tv))
      ((,not-the-claim . ,Γ-next)
       (get-claim Γ-next where x))))

  ;; add-claim : Ctx Symbol Loc Src -> (Perhaps Ctx), cf. typechecker.rkt:831
  (define (add-claim Γ f f-loc ty)
    (go-on ((_ (not-used Γ f-loc f))
            (,ty-out (is-type Γ '() ty)))
      (go (cons (cons f (claim (val-in-ctx Γ ty-out)))
                Γ))))

  ;; remove-claim : Symbol Ctx -> Ctx, Γ without the claim of x. As upstream, an entry for x that is not a
  ;; claim matches no clause, cf. typechecker.rkt:838
  (define (remove-claim x Γ)
    (match/first Γ
      (() '())
      ((((,y . #(claim ,ty)) . ,Γ-next) ⊣ (eqv? x y))
       (remove-claim x Γ-next))
      ((((,y . ,b) . ,Γ-next) ⊣ (not (eqv? x y)))
       (cons (cons y b) (remove-claim x Γ-next)))))

  ;; add-def : Ctx Symbol Loc Src -> (Perhaps Ctx), cf. typechecker.rkt:849
  (define (add-def Γ f f-loc expr)
    (go-on ((,tv (get-claim Γ f-loc f))
            (,expr-out (check Γ '() expr tv)))
      (go (bind-val (remove-claim f Γ) f tv (val-in-ctx Γ expr-out)))))

  ;;; ==================== ATOMS ====================

  ;; atom-ok? : Symbol -> Boolean, whether a is made of letters and hyphens, cf. typechecker.rkt:856
  (define (atom-ok? a)
    (all-ok-atom (string->list (symbol->string a))))

  ;; all-ok-atom : (Listof Char) -> Boolean, cf. typechecker.rkt:860
  ;; Racket's char-alphabetic? is racket-char-alphabetic?, which knows the letters of Unicode 15.1 as
  ;; Racket 8.15 does; CHICKEN's knows those of Unicode 17.
  (define (all-ok-atom cs)
    (cond
      ((null? cs) #t)
      ((or (racket-char-alphabetic? (car cs))
           (eqv? (car cs) #\-))
       (all-ok-atom (cdr cs)))
      (else #f)))

  ;;; ==================== HELPERS ====================

  ;; make-app : Src Src (Listof Src) -> Src, concocts a function application in source syntax, the
  ;; (list* a b cs) of upstream, cf. typechecker.rkt:869
  (define (make-app a b cs)
    (@ (src-loc a) (cons a (cons b cs)))))
