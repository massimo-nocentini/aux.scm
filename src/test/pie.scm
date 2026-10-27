;;; The tests of (aux pie), the port of Pie, the language of "The Little Typer" by Daniel P. Friedman and
;;; David Thrane Christiansen (https://github.com/the-little-typer/pie, commit 2c89553).
;;;
;;; A port of tests.rkt of Pie. Pie is distributed under the GNU AGPL-3.0, and so is this file, which is
;;; derived from it.
;;;
;;; Every check of tests.rkt is one case of pie-suite, in upstream order, named test/L<line>/<name> after
;;; the line where the check starts, with a "cf. tests.rkt:<line>" comment. There are 121 of them, 90
;;; check-equal? and 31 check-stop-message-equal?, as many as `raco test tests.rkt` reports. The comments
;;; of tests.rkt are kept above their cases. The translation is mechanical.
;;;   - (check-equal? actual expected) is (⊦= expected actual), and ⊦= compares the structs of (aux pie
;;;     basics) and (aux pie racket) structurally, as check-equal? compares Racket's transparent structs.
;;;   - (check-stop-message-equal? e msg) is (⊦= msg (stop->message e)).
;;;   - A syntax template #'(...) is (string->stx "(...)"), and (parse-pie #'(...)) is (string->src
;;;     "(...)"). The source text is kept as written upstream, so CHICKEN's reader never sees :: and vec::
;;;     (it would read them as keywords) and the brackets of [E U] reach the Pie reader. Only the column
;;;     where each program starts has moved.
;;;   - The for/fold over parse-pie-decl of tests.rkt:731, 773, 1041, 1327 and 1583 is decls->ctx.
;;;   - Expected Core is quoted data, with |::| and |vec::| written with bars. Expected values use the
;;;     struct constructors with their upstream names, and a box literal '#&X is (box 'X).
;;;   - The definition and export of foo at tests.rkt:19-21 is not a test and is left out.
;;; The locations of the syntax objects point into the made-up source "tests.rkt", as Racket's point into
;;; the real one. No check compares locations, but check-same and the parser need them.

(import scheme
        (chicken base)
        (only (chicken format) format)
        (aux base)
        (aux unittest)
        (only (aux pie racket) box)
        (only (aux pie syntax) string->syntax)
        (only (aux pie locations) syntax->location)
        (only (aux pie basics) go stop init-ctx ctx->env def ADD1 DELAY DELAY-CLOS EQUAL SAME PI LAM FO-CLOS)
        (only (aux pie normalize) read-back-ctx val-of)
        (only (aux pie typechecker) add-claim add-def)
        (only (aux pie parser) parse-pie parse-pie-decl)
        (only (aux pie rep) rep norm-type check-same))

;;; ==================== HELPERS ====================

; string->stx : string -> syntax, the syntax object that the template #'s denotes in tests.rkt.
(define (string->stx s) (string->syntax s "tests.rkt"))

; string->src : string -> Src, upstream's (parse-pie #'s).
(define (string->src s) (parse-pie (string->stx s)))

; stop->message : (Perhaps A) -> Message, the message of a stop, which check-stop-message-equal?
; compares. A go raises not-stop as upstream. cf. tests.rkt:23
(define (stop->message p)
  (match/first p
    (#(go _) (error 'not-stop))
    (#(stop _ ,m) m)))

; decls->ctx : string ... -> Ctx, the context that the declarations build from init-ctx. All of them
; are parsed first, as the map of upstream does, then added one after the other, and a stop raises
; "Nope: MESSAGE". cf. the for/fold of tests.rkt:731
(define (decls->ctx . strings)
  (foldl (λ (st d)
           (match/first d
             ((claim ,x ,loc ,t)
              (match/first (add-claim st x loc t)
                (#(go ,new-st) new-st)
                (#(stop _ ,msg) (error (format "Nope: ~a" msg)))))
             ((definition ,x ,loc ,v)
              (match/first (add-def st x loc v)
                (#(go ,new-st) new-st)
                (#(stop _ ,msg) (error (format "Nope: ~a" msg)))))))
         init-ctx
         (map (λ (s) (parse-pie-decl (string->stx s))) strings)))

;;; ==================== TESTS ====================

(define-suite pie-suite

  ((doc r) `((p "The tests of Pie's tests.rkt (https://github.com/the-little-typer/pie, commit 2c89553), "
                "one case per check and in upstream order, on the port (aux pie). They type check and "
                "normalize expressions with rep and norm-type, compare the messages of the errors, run "
                "check-same, and build contexts with add-claim and add-def, comparing the values in them "
                "down to which DELAY boxes are already forced.")))

  ((test/L29/val-of-the-zero _)
   ; cf. tests.rkt:29
   (⊦= 'ZERO
       (val-of (ctx->env init-ctx) '(the Nat zero))))

  ((test/L31/rep-identity _)
   ; cf. tests.rkt:31
   (⊦= (go '(the (Π ((x Nat)) Nat)
                 (λ (my-var) my-var)))
       (rep init-ctx
            (string->src "(the (-> Nat Nat)
                       (λ (my-var)
                         my-var))"))))

  ((test/L39/rep-repeated-binder _)
   ; cf. tests.rkt:39
   (⊦= (go '(the (Π ((x Nat)) (Π ((x₁ Nat)) Nat)) (λ (x) (λ (x₁) x₁))))
       (rep init-ctx (string->src "(the (-> Nat Nat Nat) (lambda (x x) x))"))))

  ((test/L43/rep-renamed-binder _)
   ; cf. tests.rkt:43
   (⊦= (go '(the (Π ((x Nat)) Nat) (λ (z) z)))
       (rep init-ctx (string->src "(the (-> Nat Nat) (λ (z) z))"))))

  ((test/L47/which-Nat-add1 _)
   ; cf. tests.rkt:47
   (⊦= (go '(the Nat zero))
       (rep init-ctx (string->src "(which-Nat 1 2 (lambda (x) x))"))))

  ((test/L51/which-Nat-zero _)
   ; cf. tests.rkt:51
   (⊦= (go '(the Nat (add1 (add1 zero))))
       (rep init-ctx (string->src "(which-Nat 0 2 (lambda (x) x))"))))

  ((test/L55/stop-lambda-too-few-binders _)
   ; cf. tests.rkt:55
   (⊦= '("Expected" (Π ((x₁ Nat)) Nat) "but given" Nat)
       (stop->message (rep init-ctx (string->src "(the (-> Nat Nat Nat) (lambda (x) x))")))))

  ((test/L59/stop-not-a-function-type _)
   ; cf. tests.rkt:59
   (⊦= '("Not a function type:" Nat)
       (stop->message (rep init-ctx
                           (string->src "(the (-> (-> Nat Nat) Nat)
                                              (lambda (f x) (f x)))")))))

  ((test/L64/rep-apply-argument _)
   ; cf. tests.rkt:64
   (⊦= (go '(the (Π ((x (Π ((x Nat)) Nat))) (Π ((x₁ Nat)) Nat)) (λ (f) (λ (x) (f x)))))
       (rep init-ctx (string->src "(the (-> (-> Nat Nat) Nat Nat) (lambda (f x) (f x)))"))))

  ((test/L67/which-Nat-neutral-step _)
   ; cf. tests.rkt:67
   (⊦= (go
        '(the (Π ((x Nat))
                (Π ((x₁ (Π ((x₁ Nat))
                          Nat)))
                  Nat))
              (λ (x)
                (λ (f)
                  (f (add1 zero))))))
       (rep init-ctx (string->src "(the (-> Nat (-> Nat Nat) Nat)
                               (lambda (x f)
                                 (which-Nat 2 x f)))"))))

  ((test/L78/which-Nat-neutral-target _)
   ; cf. tests.rkt:78
   (⊦= (go
        '(the (Π ((x Nat)) (Π ((x₁ (Π ((x₁ Nat)) Nat))) Nat))
              (λ (x) (λ (f) (which-Nat x (the Nat (add1 (add1 zero))) (λ (n) (f n)))))))
       (rep init-ctx (string->src "(the (-> Nat (-> Nat Nat) Nat)
                               (lambda (x f)
                                 (which-Nat x (add1 (add1 zero)) f)))"))))

  ((test/L85/stop-U-has-no-type _)
   ; cf. tests.rkt:85
   (⊦= '(U "is a type, but it does not have a type.")
       (stop->message (rep init-ctx (string->src "U")))))

  ((test/L88/rep-Pi-U _)
   ; cf. tests.rkt:88
   (⊦= (go '(the (Π ((A U)) U) (λ (B) B)))
       (rep init-ctx (string->src "(the (Pi ((A U)) U) (lambda (B) B))"))))

  ((test/L91/rep-Pi-two-binders _)
   ; cf. tests.rkt:91
   (⊦= (go '(the (Π ((A U)) (Π ((a A)) A)) (λ (B) (λ (b) b))))
       (rep init-ctx (string->src "(the (Pi ((A U) (a A)) A) (lambda (B b) b))"))))

  ((test/L94/ind-Nat-closed _)
   ; cf. tests.rkt:94
   (⊦= (go '(the Nat (add1 (add1 (add1 zero)))))
       (rep init-ctx
            (string->src "(ind-Nat (add1 (add1 zero))
                           (lambda (x) Nat)
                           (add1 zero)
                           (lambda (n-1 ih)
                             (add1 ih)))"))))

  ((test/L101/ind-Nat-neutral _)
   ; cf. tests.rkt:101
   (⊦= (go '(the (Π ((x Nat)) (Π ((x₁ Nat)) Nat))
                 (λ (x)
                   (λ (y)
                     (ind-Nat x
                              (λ (x₁) Nat)
                              y
                              (λ (n-1)
                                (λ (ih)
                                  (add1 ih))))))))
       (rep init-ctx (string->src "(the (-> Nat Nat Nat)
                                      (lambda (x y)
                                        (ind-Nat x
                                                 (lambda (x) Nat)
                                                 y
                                                 (lambda (n-1 ih)
                                                   (add1 ih)))))"))))

  ((test/L118/the-U-arrow _)
   ; cf. tests.rkt:118
   (⊦= (go '(the U (Π ((x Nat)) Nat)))
       (rep init-ctx (string->src "(the U (-> Nat Nat))"))))

  ((test/L120/stop-U-in-U _)
   ; cf. tests.rkt:120
   (⊦= '(U "is a type, but it does not have a type.")
       (stop->message (rep init-ctx (string->src "(the U (-> U U))")))))

  ((test/L122/arrow-five-Nats _)
   ; cf. tests.rkt:122
   (⊦= (go
        '(the U
              (Π ((x Nat))
                (Π ((x₁ Nat))
                  (Π ((x₂ Nat))
                    (Π ((x₃ Nat))
                      Nat))))))
       (rep init-ctx (string->src "(-> Nat Nat Nat Nat Nat)"))))

  ((test/L130/Pi-two-binders-is-a-type _)
   ; cf. tests.rkt:130
   (⊦= (go '(the U (Π ((x Nat)) (Π ((y Nat)) Nat))))
       (rep init-ctx (string->src "(Π ((x Nat) (y Nat)) Nat)"))))

  ((test/L133/stop-not-a-type _)
   ; cf. tests.rkt:133
   (⊦= '("Not a type")
       (stop->message (rep init-ctx (string->src "(the zero zero)")))))

  ((test/L135/stop-expected-U _)
   ; cf. tests.rkt:135
   (⊦= '("Expected" U "but given" Nat)
       (stop->message (rep init-ctx (string->src "(the (-> Nat U) (lambda (x) x))")))))

  ((test/L137/stop-zero-applied _)
   ; cf. tests.rkt:137
   (⊦= '("Not a function type:" Nat)
       (stop->message (rep init-ctx (string->src "(zero zero)")))))

  ((test/L139/stop-expected-U-again _)
   ; cf. tests.rkt:139
   (⊦= '("Expected" U "but given" Nat)
       (stop->message (rep init-ctx (string->src "(the (-> Nat U) (lambda (x) x))")))))

  ((test/L141/stop-unknown-variable _)
   ; cf. tests.rkt:141
   (⊦= '("Unknown variable" x)
       (stop->message (rep init-ctx (string->src "x")))))

  ((test/L143/norm-type-Nat _)
   ; cf. tests.rkt:143
   (⊦= (go 'Nat)
       (norm-type init-ctx (string->src "Nat"))))

  ((test/L145/stop-norm-type-Pi-body _)
   ; cf. tests.rkt:145
   (⊦= '("Expected" U "but given" Nat)
       (stop->message (norm-type init-ctx (string->src "(Π ((x Nat)) x)")))))

  ((test/L147/stop-norm-type-unknown-variable _)
   ; cf. tests.rkt:147
   (⊦= '("Unknown variable" x)
       (stop->message (norm-type init-ctx (string->src "x")))))

  ((test/L149/quoted-atom _)
   ; cf. tests.rkt:149
   (⊦= (go '(the Atom 'a))
       (rep init-ctx (string->src "'a"))))

  ((test/L151/the-Atom-quoted _)
   ; cf. tests.rkt:151
   (⊦= (go '(the Atom 'a))
       (rep init-ctx (string->src "(the Atom 'a)"))))

  ((test/L153/Atom-is-a-type _)
   ; cf. tests.rkt:153
   (⊦= (go '(the U Atom))
       (rep init-ctx (string->src "Atom"))))

  ((test/L155/Pair-is-Sigma _)
   ; cf. tests.rkt:155
   (⊦= (go '(the U (Σ ((a Atom)) Atom)))
       (rep init-ctx (string->src "(Pair Atom Atom)"))))

  ((test/L157/Sigma-two-binders _)
   ; cf. tests.rkt:157
   (⊦= (go '(the U (Σ ((x Nat)) (Σ ((y Atom)) Nat))))
       (rep init-ctx (string->src "(Σ ((x Nat) (y Atom)) Nat)"))))

  ((test/L159/cons-of-atoms _)
   ; cf. tests.rkt:159
   (⊦= (go '(the (Σ ((x Atom)) Atom) (cons 'olive 'oil)))
       (rep init-ctx (string->src "(the (Pair Atom Atom) (cons 'olive 'oil))"))))

  ((test/L161/car-of-pair _)
   ; cf. tests.rkt:161
   (⊦= (go '(the Atom 'olive))
       (rep init-ctx (string->src "(car (the (Pair Atom Atom) (cons 'olive 'oil)))"))))

  ((test/L163/cdr-of-pair _)
   ; cf. tests.rkt:163
   (⊦= (go '(the Atom 'oil))
       (rep init-ctx (string->src "(cdr (the (Pair Atom Atom) (cons 'olive 'oil)))"))))

  ((test/L165/dependent-cdr _)
   ; cf. tests.rkt:165
   (⊦= (go
        '(the
          (Π ((f (Π ((x Nat)) U)))
            (Π ((p (Σ ((n Nat)) (f n))))
              (f (car p))))
          (λ (f) (λ (p) (cdr p)))))
       (rep init-ctx
            (string->src "(the (Π ((f (-> Nat U))
                           (p (Σ ((n Nat))
                                (f n))))
                         (f (car p)))
                       (λ (f p)
                         (cdr p)))"))))

  ((test/L178/norm-type-Sigma _)
   ; cf. tests.rkt:178
   (⊦= (go '(Σ ((x Nat)) (Σ ((y Nat)) Nat)))
       (norm-type init-ctx (string->src "(Σ ((x Nat) (y Nat)) Nat)"))))

  ((test/L180/stop-car-of-zero _)
   ; cf. tests.rkt:180
   (⊦= '("Not a pair type:" Nat)
       (stop->message (rep init-ctx (string->src "(car zero)")))))

  ((test/L182/stop-cdr-of-zero _)
   ; cf. tests.rkt:182
   (⊦= '("Not a pair type:" Nat)
       (stop->message (rep init-ctx (string->src "(cdr zero)")))))

  ((test/L184/Trivial-is-a-type _)
   ; cf. tests.rkt:184
   (⊦= (go '(the U Trivial))
       (rep init-ctx (string->src "Trivial"))))

  ((test/L186/sole _)
   ; cf. tests.rkt:186
   (⊦= (go '(the Trivial sole))
       (rep init-ctx (string->src "sole"))))

  ((test/L188/eta-Trivial-pair _)
   ; cf. tests.rkt:188
   (⊦= (go
        '(the (Π ((x Trivial))
                (Σ ((x₁ Trivial)) Trivial))
              (λ (x)
                (cons sole sole))))
       (rep init-ctx (string->src "(the (-> Trivial
                                   (Pair Trivial Trivial))
                               (lambda (x)
                                 (cons x x)))"))))

  ((test/L197/nil _)
   ; cf. tests.rkt:197
   (⊦= (go '(the (List Atom) nil))
       (rep init-ctx (string->src "(the (List Atom) nil)"))))

  ((test/L199/list-cons _)
   ; cf. tests.rkt:199
   (⊦= (go '(the (List Atom) (|::| 'foo nil)))
       (rep init-ctx (string->src "(the (List Atom) (:: 'foo nil))"))))

  ((test/L201/ind-List-length _)
   ; cf. tests.rkt:201
   (⊦= (go '(the Nat (add1 (add1 zero))))
       (rep init-ctx (string->src "(ind-List (:: 'lakrids
                                        (:: 'salmiak nil))
                                    (λ (_) Nat)
                                    zero
                                    (lambda (x y z)
                                      (add1 z)))"))))

  ((test/L208/ind-List-sum _)
   ; cf. tests.rkt:208
   (⊦= (go '(the Nat (add1 (add1 (add1 (add1 (add1 zero)))))))
       (rep init-ctx
            (string->src "(ind-List (:: (add1 (add1 (add1 zero)))
                                (:: (add1 (add1 zero))
                                    nil))
                            (λ (_)
                              Nat)
                            zero
                            (lambda (x y z)
                              (ind-Nat x
                                       (lambda (n)
                                         Nat)
                                       z
                                       (lambda (_ q)
                                         (add1 q)))))"))))

  ((test/L223/ind-List-neutral _)
   ; cf. tests.rkt:223
   (⊦= (go
        '(the
          (Π ((x (List Nat)))
            Nat)
          (λ (ns)
            (ind-List
             ns
             (λ (_) Nat)
             zero
             (λ (x)
               (λ (y) (λ (z) (ind-Nat x (λ (n) Nat) z (λ (_) (λ (q) (add1 q)))))))))))
       (rep init-ctx
            (string->src "(the (-> (List Nat) Nat)
                       (lambda (ns)
                         (ind-List
                          ns
                          (λ (_) Nat)
                          zero
                          (lambda (x y z)
                            (ind-Nat
                             x
                             (lambda (n)
                               Nat)
                             z
                             (lambda (_ q)
                               (add1 q)))))))"))))

  ((test/L249/stop-nil-not-a-List _)
   ; cf. tests.rkt:249
   (⊦= '("nil requires a List type, but was used as a" Atom)
       (stop->message (rep init-ctx (string->src "(the Atom nil)")))))

  ((test/L252/stop-ind-List-not-a-List _)
   ; cf. tests.rkt:252
   (⊦= '("Not List: " Atom)
       (stop->message (rep init-ctx (string->src "(ind-List
                                          'brunsviger
                                          (λ (_) Nat)
                                          zero
                                          (lambda (x y z)
                                            (ind-Nat x
                                                     (lambda (n) Nat)
                                                     z
                                                     (lambda (_ q) (add1 q)))))")))))

  ((test/L263/rec-List-append _)
   ; cf. tests.rkt:263
   (⊦= (go
        '(the
          (Π ((E U)) (Π ((x (List E))) (Π ((x₁ (List E))) (List E))))
          (λ (E)
            (λ (xs)
              (λ (ys)
                (rec-List
                 xs
                 (the (List E) ys)
                 (λ (h) (λ (t) (λ (res) (|::| h res))))))))))
       (rep init-ctx (string->src "(the (Pi ((E U))
                                   (-> (List E) (List E)
                                       (List E)))
                               (lambda (E)
                                 (lambda (xs ys)
                                   (rec-List xs
                                             ys
                                             (lambda (h t res)
                                               (:: h res))))))"))))

  ((test/L283/rec-List-closed _)
   ; cf. tests.rkt:283
   (⊦= (go '(the (List Atom)
                 (|::| 'æbler (|::| 'blommer (|::| 'jordbær (|::| 'pærer nil))))))
       (rep init-ctx (string->src "(rec-List (:: 'æbler (:: 'blommer nil))
                                    (:: 'jordbær (:: 'pærer nil))
                                    (lambda (h t res) (:: h res)))"))))

  ((test/L288/stop-rec-List-not-a-List _)
   ; cf. tests.rkt:288
   (⊦= '("Not List: " U)
       (stop->message (rep init-ctx (string->src "(rec-List Nat
                                                   (:: 'jordbær (:: 'pære nil))
                                                   (lambda (h t res)
                                                     (:: h res)))")))))

  ((test/L293/ind-Absurd _)
   ; cf. tests.rkt:293
   (⊦= (go '(the (Π ((x Absurd)) Nat) (λ (x) (ind-Absurd (the Absurd x) Nat))))
       (rep init-ctx (string->src "(the (-> Absurd Nat) (lambda (x) (ind-Absurd x Nat)))"))))

  ((test/L295/Absurd-is-a-type _)
   ; cf. tests.rkt:295
   (⊦= (go '(the U Absurd))
       (rep init-ctx (string->src "Absurd"))))

  ((test/L297/check-same-Absurd-functions _)
   ; cf. tests.rkt:297
   (⊦= (go (void))
       (check-same init-ctx
                   (syntax->location (string->stx "here"))
                   (string->src "(-> Absurd Absurd Absurd)")
                   (string->src "(lambda (x y) x)")
                   (string->src "(lambda (x y) y)"))))

  ((test/L303/stop-check-same-numbers _)
   ; cf. tests.rkt:303
   (⊦= '("The expressions" zero "and" (add1 zero) "are not the same" Nat)
       (stop->message (check-same init-ctx
                                  (syntax->location (string->stx "here"))
                                  (string->src "Nat")
                                  (string->src "0")
                                  (string->src "1")))))

  ((test/L309/eta-Absurd-identity _)
   ; cf. tests.rkt:309
   (⊦= (go '(the (Π ((x Absurd)) Absurd) (λ (x) (the Absurd x))))
       (rep init-ctx (string->src "(the (-> Absurd Absurd) (lambda (x) x))"))))

  ((test/L311/eta-Absurd-first _)
   ; cf. tests.rkt:311
   (⊦= (go '(the (Π ((x Absurd)) (Π ((x₁ Absurd)) Absurd)) (λ (x) (λ (y) (the Absurd x)))))
       (rep init-ctx (string->src "(the (-> Absurd Absurd Absurd) (lambda (x y) x))"))))

  ((test/L313/stop-cons-no-type _)
   ; cf. tests.rkt:313
   (⊦= '("Can't determine a type")
       (stop->message (rep init-ctx (string->src "(cons 1 2)")))))

  ((test/L315/same-zero _)
   ; cf. tests.rkt:315
   (⊦= (go '(the (= Nat zero zero) (same zero)))
       (rep init-ctx (string->src "(the (= Nat 0 0) (same 0))"))))

  ((test/L317/same-polymorphic _)
   ; cf. tests.rkt:317
   (⊦= (go '(the (Π ((A U))
                   (Π ((a A))
                     (= A a a)))
                 (λ (A)
                   (λ (a)
                     (same a)))))
       (rep init-ctx (string->src "(the (Pi ((A U)
                                    (a A))
                                   (= A a a))
                               (lambda (A a)
                                 (same a)))"))))

  ((test/L328/same-Trivial-eta _)
   ; cf. tests.rkt:328
   (⊦= (go '(the (Π ((x Trivial))
                   (Π ((y Trivial))
                     (= Trivial sole sole)))
                 (λ (x)
                   (λ (y)
                     (same sole)))))
       (rep init-ctx (string->src "(the (Pi ((x Trivial)
                                    (y Trivial))
                                   (= Trivial x y))
                               (lambda (x y)
                                 (same sole)))"))))

  ((test/L340/same-functions-from-Absurd _)
   ; cf. tests.rkt:340
   (⊦= (go
        '(the
          (Π ((x (Π ((x Absurd)) Trivial)))
            (Π ((y (Π ((x₁ Absurd)) Trivial)))
              (= (Π ((x₁ Absurd)) Trivial)
                 (λ (x₁) sole)
                 (λ (x₁) sole))))
          (λ (f)
            (λ (g)
              (same (λ (x) sole))))))
       (rep init-ctx
            (string->src "(the (Pi ((x (-> Absurd Trivial))
                            (y (-> Absurd Trivial)))
                           (= (-> Absurd Trivial)
                              x
                              y))
                       (lambda (f g)
                         (same (lambda (x)
                                 (ind-Absurd x Trivial)))))"))))

  ((test/L359/ind-Absurd-equality _)
   ; cf. tests.rkt:359
   (⊦= (go
        '(the
          (Π ((x (Π ((x Trivial)) Absurd)))
            (Π ((y (Π ((x₁ Trivial)) Absurd)))
              (= (Π ((x₁ Trivial)) Absurd)
                 (λ (x₁) (the Absurd (x sole)))
                 (λ (x₁) (the Absurd (y sole))))))
          (λ (f)
            (λ (g)
              (ind-Absurd
               (the Absurd (f sole))
               (= (Π ((x Trivial)) Absurd)
                  (λ (x) (the Absurd (f sole)))
                  (λ (x) (the Absurd (g sole)))))))))
       (rep init-ctx (string->src "(the (Pi ((x (-> Trivial Absurd)) (y (-> Trivial Absurd))) (= (-> Trivial Absurd) x y)) (lambda (f g) (ind-Absurd (f sole) (= (-> Trivial Absurd) f g))))"))))

  ((test/L375/same-Absurd-neutrals _)
   ; cf. tests.rkt:375
   (⊦= (go
        '(the (Π ((x (Π ((x Trivial))
                       Absurd)))
                (Π ((y (Π ((x₁ Trivial))
                         Absurd)))
                  (= Absurd
                     (the Absurd (x sole))
                     (the Absurd (y sole)))))
              (λ (f)
                (λ (g)
                  (same (the Absurd (f sole)))))))
       (rep init-ctx (string->src "(the (Pi ((x (-> Trivial Absurd))
                                    (y (-> Trivial Absurd)))
                                           (= Absurd
                                              (the Absurd (x sole))
                                              (the Absurd (y sole))))
                               (lambda (f g)
                                 (same (f sole))))"))))

  ((test/L393/same-functions-to-Absurd _)
   ; cf. tests.rkt:393
   (⊦= (go
        '(the
          (Π ((x (Π ((x Trivial)) Absurd)))
            (Π ((y (Π ((x₁ Trivial)) Absurd)))
              (= (Π ((x₁ Trivial)) Absurd)
                 (λ (x₁) (the Absurd (x sole)))
                 (λ (x₁) (the Absurd (y sole))))))
          (λ (f)
            (λ (g)
              (same (λ (x₁) (the Absurd (f sole))))))))
       (rep init-ctx
            (string->src "(the (Pi ((x (-> Trivial Absurd))
                            (y (-> Trivial Absurd)))
                           (= (-> Trivial Absurd) x y))
                       (lambda (f g)
                         (same f)))"))))

  ((test/L410/replace-symmetry _)
   ; cf. tests.rkt:410
   (⊦= (go
        '(the (Π ((n Nat))
                (Π ((m Nat))
                  (Π ((x (= Nat n m)))
                    (= Nat m n))))
              (λ (n)
                (λ (m)
                  (λ (n=m)
                    (replace n=m
                             (λ (k)
                               (= Nat k n))
                             (same n)))))))
       (rep init-ctx (string->src "(the (Pi ((n Nat)
                                    (m Nat))
                                   (-> (= Nat n m)
                                       (= Nat m n)))
                               (lambda (n m n=m)
                                 (replace n=m
                                          (lambda (k)
                                            (= Nat k n))
                                          (same n))))"))))

  ((test/L432/replace-closed _)
   ; cf. tests.rkt:432
   (⊦= (go
        '(the (= Nat
                 (add1 (add1 (add1 (add1 zero))))
                 (add1 (add1 (add1 (add1 zero)))))
              (same (add1 (add1 (add1 (add1 zero)))))))
       (rep init-ctx (string->src "(replace (the (= Nat 4 4) (same 4))
                                   (lambda (k)
                                     (= Nat k 4))
                                   (same 4))"))))

  ((test/L442/stop-replace-not-equality _)
   ; cf. tests.rkt:442
   (⊦= '("Expected an expression with = type, but the type was" Nat)
       (stop->message (rep init-ctx (string->src "(replace 4 5 6)")))))

  ((test/L445/stop-same-not-equality _)
   ; cf. tests.rkt:445
   (⊦= '("same requires an = type, but was used as a" Nat)
       (stop->message (rep init-ctx (string->src "(the Nat (same 3))")))))

  ((test/L449/vec-literal _)
   ; cf. tests.rkt:449
   (⊦= (go
        '(the (Vec Atom (add1 (add1 (add1 zero))))
              (|vec::| 'råkost
                     (|vec::| 'rødbeder
                            (|vec::| 'grønkål
                                   vecnil)))))
       (rep init-ctx (string->src "(the (Vec Atom 3)
                               (vec:: 'råkost
                                      (vec:: 'rødbeder
                                             (vec:: 'grønkål
                                                    vecnil))))"))))

  ((test/L462/head _)
   ; cf. tests.rkt:462
   (⊦= (go '(the Atom 'rosenkål))
       (rep init-ctx (string->src "(head (the (Vec Atom 1) (vec:: 'rosenkål vecnil)))"))))

  ((test/L465/tail _)
   ; cf. tests.rkt:465
   (⊦= (go '(the (Vec Atom zero) vecnil))
       (rep init-ctx (string->src "(tail (the (Vec Atom 1) (vec:: 'rosenkål vecnil)))"))))

  ((test/L468/head-tail-neutral _)
   ; cf. tests.rkt:468
   (⊦= (go
        '(the
          (Π ((A U))
            (Π ((len Nat))
              (Π ((x (Vec A (add1 len))))
                (Vec A (add1 len)))))
          (λ (A)
            (λ (len)
              (λ (xs)
                (|vec::| (head xs)
                       (tail xs)))))))
       (rep init-ctx (string->src "(the (Pi ((A U) (len Nat)) (-> (Vec A (add1 len)) (Vec A (add1 len)))) (lambda (A len) (lambda (xs) (vec:: (head xs) (tail xs)))))"))))

  ((test/L481/stop-vecnil-nonzero-length _)
   ; cf. tests.rkt:481
   (⊦= '(vecnil "requires that the length be zero, not" (add1 (add1 zero)))
       (stop->message (rep init-ctx (string->src "(the (Vec Atom 2) vecnil)")))))

  ((test/L485/stop-vecnil-not-a-Vec _)
   ; cf. tests.rkt:485
   (⊦= '(vecnil "must be a Vec, but was used in a" Nat "context.")
       (stop->message (rep init-ctx (string->src "(the Nat vecnil)")))))

  ((test/L489/stop-vec-cons-neutral-length _)
   ; cf. tests.rkt:489
   (⊦= '("vec:: requires that the length have add1 on top, not" k)
       (stop->message (rep init-ctx (string->src "(the (Pi ((k Nat)) (Vec Atom k)) (lambda (k) (vec:: 'hey vecnil)))")))))

  ((test/L492/stop-vec-cons-zero-length _)
   ; cf. tests.rkt:492
   (⊦= '("vec:: requires that the length have add1 on top, not" zero)
       (stop->message (rep init-ctx (string->src "(the (Vec Atom 0) (vec:: 'øllebrød vecnil))")))))

  ((test/L495/stop-vec-cons-not-a-Vec _)
   ; cf. tests.rkt:495
   (⊦= '("vec:: must be a Vec, but was used in a" Nat "context.")
       (stop->message (rep init-ctx (string->src "(the Nat (vec:: 3 2))")))))

  ((test/L498/Vec-is-a-type _)
   ; cf. tests.rkt:498
   (⊦= (go '(the U (Vec Nat (add1 (add1 zero)))))
       (rep init-ctx (string->src "(Vec Nat 2)"))))

  ((test/L501/stop-head-of-empty _)
   ; cf. tests.rkt:501
   (⊦= '("Expected a Vec with add1 at the top of the length, got" zero)
       (stop->message (rep init-ctx (string->src "(head (the (Vec Atom 0) vecnil))")))))

  ((test/L504/stop-head-not-a-Vec _)
   ; cf. tests.rkt:504
   (⊦= '("Expected a Vec, got" Nat)
       (stop->message (rep init-ctx (string->src "(head zero)")))))

  ((test/L507/stop-tail-of-empty _)
   ; cf. tests.rkt:507
   (⊦= '("Expected a Vec with add1 at the top of the length, got" zero)
       (stop->message (rep init-ctx (string->src "(tail (the (Vec Atom 0) vecnil))")))))

  ((test/L510/stop-tail-not-a-Vec _)
   ; cf. tests.rkt:510
   (⊦= '("Expected a Vec, got" Nat)
       (stop->message (rep init-ctx (string->src "(tail zero)")))))

  ((test/L513/ind-Vec-empty _)
   ; cf. tests.rkt:513
   (⊦= (go '(the Nat zero))
       (rep init-ctx (string->src "(ind-Vec 0
                                   (the (Vec Nat 0) vecnil)
                                   (lambda (k xs) Nat)
                                   0
                                   (lambda (k e es ih)
                                     (add1 ih)))"))))

  ((test/L520/ind-Vec-two _)
   ; cf. tests.rkt:520
   (⊦= (go '(the Nat (add1 (add1 zero))))
       (rep init-ctx (string->src "(ind-Vec 2
                                          (the (Vec Nat 2)
                                               (vec:: 1
                                                      (vec:: 3
                                                             vecnil)))
                                          (lambda (k xs) Nat)
                                          0
                                          (lambda (k e es ih)
                                            (add1 ih)))"))))

  ((test/L531/ind-Vec-neutral _)
   ; cf. tests.rkt:531
   (⊦= (go
        '(the
          (Π ((k Nat)) (Π ((xs (Vec Nat k))) Nat))
          (λ (k)
            (λ (xs)
              (ind-Vec k
                       xs
                       (λ (k₁) (λ (xs₁) Nat))
                       zero
                       (λ (k₁)
                         (λ (e)
                           (λ (es)
                             (λ (ih)
                               (add1 ih))))))))))
       (rep init-ctx
            (string->src "(the (Pi ((k Nat) (xs (Vec Nat k))) Nat)
                             (lambda (k xs)
                               (ind-Vec k
                                        xs
                                        (lambda (k xs)
                                          Nat)
                                        0
                                        (lambda (k e es ih)
                                          (add1 ih)))))"))))

  ((test/L556/Either-left _)
   ; cf. tests.rkt:556
   (⊦= (go '(the (Either Nat Atom) (left (add1 (add1 (add1 (add1 zero)))))))
       (rep init-ctx
            (string->src "(the (Either Nat Atom) (left 4))"))))

  ((test/L559/Either-right _)
   ; cf. tests.rkt:559
   (⊦= (go '(the (Either Nat Atom) (right 'løgsuppe)))
       (rep init-ctx
            (string->src "(the (Either Nat Atom) (right 'løgsuppe))"))))

  ((test/L562/Either-is-a-type _)
   ; cf. tests.rkt:562
   (⊦= (go '(the U (Either Nat Atom)))
       (rep init-ctx (string->src "(Either Nat Atom)"))))

  ((test/L564/ind-Either-swap _)
   ; cf. tests.rkt:564
   (⊦= (go
        '(the (Π ((x (Either Nat Atom))) (Either Atom Nat))
              (λ (x)
                (ind-Either
                 x
                 (λ (x₁) (Either Atom Nat))
                 (λ (x₁) (right x₁))
                 (λ (x₁) (left x₁))))))
       (rep init-ctx (string->src "(the (-> (Either Nat Atom) (Either Atom Nat))
                               (lambda (x)
                                 (ind-Either x
                                             (lambda (x) (Either Atom Nat))
                                             (lambda (x) (right x))
                                             (lambda (x) (left x)))))"))))

  ((test/L579/ind-Vec-neutral-vector _)
   ; cf. tests.rkt:579
   (⊦= (go
        '(the (Π ((x (Vec Nat (add1 (add1 zero)))))
                Nat)
              (λ (xs)
                (ind-Vec
                 (add1 (add1 zero))
                 xs
                 (λ (k)
                   (λ (xs₁) Nat))
                 zero
                 (λ (k)
                   (λ (x)
                     (λ (xs₁)
                       (λ (ih)
                         (add1 ih)))))))))
       (rep init-ctx (string->src "(the (-> (Vec Nat 2) Nat)
                                      (lambda (xs)
                                        (ind-Vec 2
                                                 xs
                                                 (lambda (k xs)
                                                   Nat)
                                                 0
                                                 (lambda (k x xs ih)
                                                   (add1 ih)))))"))))

  ((test/L604/ind-Vec-neutral-length _)
   ; cf. tests.rkt:604
   (⊦= (go
        '(the
          (Π ((n Nat)) (Π ((x (Vec Nat n))) Nat))
          (λ (n)
            (λ (xs)
              (ind-Vec
               n
               xs
               (λ (k) (λ (xs₁) Nat))
               zero
               (λ (k)
                 (λ (x)
                   (λ (xs₁)
                     (λ (ih)
                       (add1 ih))))))))))
       (rep init-ctx
            (string->src "(the (Pi ((n Nat)) (-> (Vec Nat n) Nat))
                       (lambda (n xs)
                         (ind-Vec n
                                  xs
                                  (lambda (k xs) Nat)
                                  0
                                  (lambda (k x xs ih) (add1 ih)))))"))))

  ((test/L627/symm-closed _)
   ; cf. tests.rkt:627
   (⊦= (go
        '(the
          (= Nat (add1 (add1 zero)) (add1 (add1 zero)))
          (same (add1 (add1 zero)))))
       (rep init-ctx (string->src "(symm (the (= Nat 2 2) (same 2)))"))))

  ((test/L633/symm-neutral _)
   ; cf. tests.rkt:633
   (⊦= (go
        '(the
          (Π ((x (= Nat
                    (add1 (add1 zero))
                    (add1 (add1 (add1 (add1 (add1 zero))))))))
            (= Nat
               (add1 (add1 (add1 (add1 (add1 zero)))))
               (add1 (add1 zero))))
          (λ (x)
            (symm x))))
       (rep init-ctx (string->src "(the (-> (= Nat 2 5) (= Nat 5 2)) (lambda (x) (symm x)))"))))

  ((test/L644/trans-neutral _)
   ; cf. tests.rkt:644
   (⊦= (go
        '(the (Π ((x (= Nat (add1 (add1 zero)) (add1 (add1 (add1 (add1 (add1 zero))))))))
                (Π ((x₁ (= Nat
                           (add1 (add1 (add1 (add1 (add1 zero)))))
                           (add1 (add1 (add1 zero))))))
                  (= Nat (add1 (add1 zero)) (add1 (add1 (add1 zero))))))
              (λ (x) (λ (y) (trans x y)))))
       (rep init-ctx (string->src "(the (-> (= Nat 2 5) (= Nat 5 3) (= Nat 2 3))
                               (lambda (x y) (trans x y)))"))))

  ((test/L653/trans-same-left _)
   ; cf. tests.rkt:653
   (⊦= (go
        '(the
          (Π ((x (=
                  Nat
                  (add1 (add1 (add1 (add1 (add1 zero)))))
                  (add1 (add1 (add1 zero))))))
            (= Nat (add1 (add1 (add1 (add1 (add1 zero))))) (add1 (add1 (add1 zero)))))
          (λ (y)
            (trans (same (add1 (add1 (add1 (add1 (add1 zero))))))
                   y))))
       (rep init-ctx (string->src "(the (-> (= Nat 5 3) (= Nat 5 3)) (lambda (y) (trans (the (= Nat 5 5) (same 5)) y)))"))))

  ((test/L664/trans-same-right _)
   ; cf. tests.rkt:664
   (⊦= (go
        '(the
          (Π ((x (= Nat
                    (add1 (add1 (add1 (add1 (add1 zero)))))
                    (add1 (add1 (add1 zero))))))
            (= Nat (add1 (add1 (add1 (add1 (add1 zero))))) (add1 (add1 (add1 zero)))))
          (λ (y)
            (trans y (same (add1 (add1 (add1 zero))))))))
       (rep init-ctx
            (string->src "(the (-> (= Nat 5 3)
                           (= Nat 5 3))
                       (lambda (y)
                         (trans y (the (= Nat 3 3) (same 3)))))"))))

  ((test/L677/trans-closed _)
   ; cf. tests.rkt:677
   (⊦= (go
        '(the
          (= Nat
             (add1 (add1 (add1 (add1 (add1 zero)))))
             (add1 (add1 (add1 (add1 (add1 zero))))))
          (same (add1 (add1 (add1 (add1 (add1 zero))))))))
       (rep init-ctx (string->src "(trans (the (= Nat 5 5) (same 5)) (the (= Nat 5 5) (same 5)))"))))

  ((test/L684/iter-Nat-closed _)
   ; cf. tests.rkt:684
   (⊦= (go '(the Nat (add1 (add1 (add1 (add1 (add1 zero)))))))
       (rep init-ctx (string->src "(iter-Nat 2
                                    3
                                    (λ (ih)
                                      (add1 ih)))"))))

  ((test/L689/iter-Nat-neutral _)
   ; cf. tests.rkt:689
   (⊦= (go
        '(the
          (Π ((x Nat))
            (Π ((x₁ Nat))
              Nat))
          (λ (x)
            (λ (y)
              (iter-Nat x
                        (the Nat y)
                        (λ (n-1)
                          (add1 n-1)))))))
       (rep init-ctx
            (string->src "(the (-> Nat Nat
                           Nat)
                       (lambda (x y)
                         (iter-Nat x
                                   y
                                   (λ (n-1)
                                     (add1 n-1)))))"))))

  ((test/L708/rec-Nat-neutral _)
   ; cf. tests.rkt:708
   (⊦= (go
        '(the
          (Π ((x Nat))
            (Π ((x₁ Nat))
              Nat))
          (λ (x)
            (λ (y)
              (rec-Nat x
                       (the Nat y)
                       (λ (n-1)
                         (λ (ih)
                           (add1 ih))))))))
       (rep init-ctx
            (string->src "(the (-> Nat Nat Nat)
                       (lambda (x y)
                         (rec-Nat x
                                  y
                                  (λ (n-1 ih)
                                    (add1 ih)))))"))))

  ((test/L728/rec-Nat-closed _)
   ; cf. tests.rkt:728
   (⊦= (go '(the Nat (add1 (add1 (add1 (add1 (add1 zero)))))))
       (rep init-ctx (string->src "(rec-Nat 2 3 (λ (n-1 ih) (add1 ih)))"))))

  ((test/L731/ctx-four-eight _)
   ; cf. tests.rkt:731
   (⊦= (list
        (cons
         'eight
         (def
           'NAT
           (ADD1
            (DELAY
             (box
              (DELAY-CLOS
               (list
                (cons
                 'four
                 (ADD1 (DELAY (box
                               (DELAY-CLOS '() '(add1 (add1 (add1 zero)))))))))
               '(add1 (add1 (add1 four)))))))))
        (cons
         'four
         (def 'NAT (ADD1 (DELAY (box
                                 (DELAY-CLOS '() '(add1 (add1 (add1 zero))))))))))
       (decls->ctx "(claim four
                           Nat)"
                   "(define four
                      4)"
                   "(claim eight
                           Nat)"
                   "(define eight
                      (add1 (add1 (add1 (add1 four)))))")))

  ; Check call-by-need relative to the previous test
  ((test/L773/ctx-call-by-need _)
   ; cf. tests.rkt:773
   (⊦= (list
        (cons
         'go
         (def
           (EQUAL
            (DELAY (box 'NAT))
            (DELAY
             (box
              (ADD1
               (DELAY
                (box
                 (ADD1
                  (DELAY
                   (box
                    (ADD1
                     (DELAY
                      (box
                       (ADD1
                        (DELAY
                         (box
                          (ADD1
                           (DELAY
                            (box
                             (ADD1
                              (DELAY
                               (box
                                (ADD1
                                 (DELAY
                                  (box (ADD1 (DELAY (box 'ZERO))))))))))))))))))))))))))
            (DELAY
             (box
              (ADD1
               (DELAY
                (box
                 (ADD1
                  (DELAY
                   (box
                    (ADD1
                     (DELAY
                      (box
                       (ADD1
                        (DELAY
                         (box
                          (ADD1
                           (DELAY
                            (box
                             (ADD1
                              (DELAY
                               (box
                                (ADD1
                                 (DELAY
                                  (box (ADD1 (DELAY (box 'ZERO)))))))))))))))))))))))))))
           (SAME
            (DELAY
             (box
              (DELAY-CLOS
               (list
                (cons
                 'force
                 (LAM
                  'n
                  (FO-CLOS
                   (list
                    (cons
                     'eight
                     (ADD1
                      (DELAY
                       (box
                        (ADD1
                         (DELAY
                          (box
                           (ADD1
                            (DELAY
                             (box
                              (ADD1
                               (DELAY
                                (box
                                 (ADD1
                                  (DELAY
                                   (box
                                    (ADD1
                                     (DELAY
                                      (box
                                       (ADD1
                                        (DELAY
                                         (box
                                          (ADD1 (DELAY (box 'ZERO)))))))))))))))))))))))))
                    (cons
                     'four
                     (ADD1
                      (DELAY
                       (box
                        (ADD1
                         (DELAY
                          (box (ADD1 (DELAY (box (ADD1 (DELAY (box 'ZERO))))))))))))))
                   'n
                   '(iter-Nat n (the Nat zero) (λ (k) (add1 k))))))
                (cons
                 'eight
                 (ADD1
                  (DELAY
                   (box
                    (ADD1
                     (DELAY
                      (box
                       (ADD1
                        (DELAY
                         (box
                          (ADD1
                           (DELAY
                            (box
                             (ADD1
                              (DELAY
                               (box
                                (ADD1
                                 (DELAY
                                  (box
                                   (ADD1
                                    (DELAY
                                     (box (ADD1 (DELAY (box 'ZERO)))))))))))))))))))))))))
                (cons
                 'four
                 (ADD1
                  (DELAY
                   (box
                    (ADD1
                     (DELAY (box (ADD1 (DELAY (box (ADD1 (DELAY (box 'ZERO))))))))))))))
               'eight))))))
        (cons
         'force
         (def
           (PI
            'x
            (DELAY (box 'NAT))
            (FO-CLOS
             (list
              (cons
               'eight
               (ADD1
                (DELAY
                 (box
                  (ADD1
                   (DELAY
                    (box
                     (ADD1
                      (DELAY
                       (box
                        (ADD1
                         (DELAY
                          (box
                           (ADD1
                            (DELAY
                             (box
                              (ADD1
                               (DELAY
                                (box
                                 (ADD1
                                  (DELAY
                                   (box (ADD1 (DELAY (box 'ZERO)))))))))))))))))))))))))
              (cons
               'four
               (ADD1
                (DELAY
                 (box
                  (ADD1 (DELAY (box (ADD1 (DELAY (box (ADD1 (DELAY (box 'ZERO))))))))))))))
             'x
             'Nat))
           (LAM
            'n
            (FO-CLOS
             (list
              (cons
               'eight
               (ADD1
                (DELAY
                 (box
                  (ADD1
                   (DELAY
                    (box
                     (ADD1
                      (DELAY
                       (box
                        (ADD1
                         (DELAY
                          (box
                           (ADD1
                            (DELAY
                             (box
                              (ADD1
                               (DELAY
                                (box
                                 (ADD1
                                  (DELAY
                                   (box (ADD1 (DELAY (box 'ZERO)))))))))))))))))))))))))
              (cons
               'four
               (ADD1
                (DELAY
                 (box
                  (ADD1 (DELAY (box (ADD1 (DELAY (box (ADD1 (DELAY (box 'ZERO))))))))))))))
             'n
             '(iter-Nat n (the Nat zero) (λ (k) (add1 k)))))))
        (cons
         'eight
         (def
           'NAT
           (ADD1
            (DELAY
             (box
              (ADD1
               (DELAY
                (box
                 (ADD1
                  (DELAY
                   (box
                    (ADD1
                     (DELAY
                      (box
                       (ADD1
                        (DELAY
                         (box
                          (ADD1
                           (DELAY
                            (box
                             (ADD1
                              (DELAY (box (ADD1 (DELAY (box 'ZERO))))))))))))))))))))))))))
        (cons
         'four
         (def
           'NAT
           (ADD1
            (DELAY
             (box
              (ADD1 (DELAY (box (ADD1 (DELAY (box (ADD1 (DELAY (box 'ZERO)))))))))))))))
       (decls->ctx "(claim four
                           Nat)"
                   "(define four
                      4)"
                   "(claim eight
                           Nat)"
                   "(define eight
                      (add1 (add1 (add1 (add1 four)))))"
                   "(claim force (-> Nat Nat))"
                   "(define force
                      (lambda (n)
                        (iter-Nat n
                                  zero
                                  (lambda (k)
                                    (add1 k)))))"
                   "(claim go (= Nat eight (force eight)))"
                   "(define go (same eight))")))

  ; Check thunk sharing between values in the context
  ((test/L1040/ctx-thunk-sharing _)
   ; cf. tests.rkt:1040
   (⊦= (list
        (cons 'four≥three (def 'TRIVIAL 'SOLE))
        (cons
         '≥-three
         (def
           (PI
            'x
            (DELAY (box 'NAT))
            (FO-CLOS
             (list
              (cons
               'four
               (ADD1
                (DELAY
                 (box
                  (ADD1
                   (DELAY
                    (box (ADD1 (DELAY (box (DELAY-CLOS '() '(add1 zero))))))))))))
              (cons
               'three
               (ADD1
                (DELAY (box (ADD1 (DELAY (box (DELAY-CLOS '() '(add1 zero))))))))))
             'x
             'U))
           (LAM
            'n
            (FO-CLOS
             (list
              (cons
               'four
               (ADD1
                (DELAY
                 (box
                  (ADD1
                   (DELAY
                    (box (ADD1 (DELAY (box (DELAY-CLOS '() '(add1 zero))))))))))))
              (cons
               'three
               (ADD1
                (DELAY (box (ADD1 (DELAY (box (DELAY-CLOS '() '(add1 zero))))))))))
             'n
             '(which-Nat
               n
               (the U Absurd)
               (λ (n-1)
                 (which-Nat
                  n-1
                  (the U Absurd)
                  (λ (n-2) (which-Nat n-2 (the U Absurd) (λ (n-3) Trivial))))))))))
        (cons
         'four
         (def
           'NAT
           (ADD1
            (DELAY
             (box
              (ADD1
               (DELAY (box (ADD1 (DELAY (box (DELAY-CLOS '() '(add1 zero)))))))))))))
        (cons
         'three
         (def
           'NAT
           (ADD1 (DELAY (box (ADD1 (DELAY (box (DELAY-CLOS '() '(add1 zero)))))))))))
       (decls->ctx "(claim three
                           Nat)"
                   "(define three
                      3)"
                   "(claim four
                           Nat)"
                   "(define four
                      (add1 three))"
                   "(claim ≥-three (-> Nat U))"
                   "(define ≥-three
                      (lambda (n)
                        (which-Nat n
                                   Absurd
                                   (lambda (n-1)
                                     (which-Nat n-1
                                                Absurd
                                                (lambda (n-2)
                                                  (which-Nat n-2
                                                             Absurd
                                                             (lambda (n-3)
                                                               Trivial))))))))"
                   "(claim four≥three
                           (≥-three four))"
                   "(define four≥three sole)")))

  ((test/L1143/ind-Either-applied _)
   ; cf. tests.rkt:1143
   (⊦= (go '(the (Either Trivial Nat) (right (add1 (add1 zero)))))
       (rep init-ctx
           (string->src "((the (Pi ((A U) (B U))
                           (-> (Either A B)
                               (Either B A)))
                       (lambda (A B e)
                         (ind-Either e
                                     (lambda (_) (Either B A))
                                     (lambda (x) (right x))
                                     (lambda (x) (left x)))))
                  Nat Trivial (left 2))"))))

  ((test/L1156/ind-Either-partial _)
   ; cf. tests.rkt:1156
   (⊦= (go
       '(the
         (Π ((B U)) (Π ((x (Either Nat B))) (Either B Nat)))
         (λ (B)
           (λ (e)
             (ind-Either
              e
              (λ (_) (Either B Nat))
              (λ (x) (right x))
              (λ (x) (left x)))))))
       (rep init-ctx
           (string->src "((the (Pi ((A U) (B U))
                           (-> (Either A B)
                               (Either B A)))
                       (lambda (A B e)
                         (ind-Either e
                                     (lambda (_) (Either B A))
                                     (lambda (x) (right x))
                                     (lambda (x) (left x)))))
                  Nat)"))))

  ((test/L1178/cong-closed _)
   ; cf. tests.rkt:1178
   (⊦= (go
        '(the (= Nat (add1 (add1 (add1 zero))) (add1 (add1 (add1 zero))))
              (same (add1 (add1 (add1 zero))))))
       (rep init-ctx (string->src "(cong (the (= Nat 2 2) (same 2))
                                (the (-> Nat Nat) (lambda (x) (add1 x))))"))))

  ((test/L1184/cong-neutral _)
   ; cf. tests.rkt:1184
   (⊦= (go '(the (Π ((x Nat))
                   (Π ((y Nat))
                     (Π ((p (= Nat x y)))
                       (Π ((f (Π ((x₁ Nat)) (List Atom)))) (= (List Atom) (f x) (f y))))))
                 (λ (x) (λ (y) (λ (p) (λ (f) (cong p (List Atom) (λ (x₁) (f x₁)))))))))
       (rep init-ctx (string->src "(the (Pi ((x Nat)
                                    (y Nat)
                                    (p (= Nat x y))
                                    (f (-> Nat (List Atom))))
                                   (= (List Atom) (f x) (f y)))
                               (lambda (x y p f)
                                 (cong p f)))"))))

  ((test/L1197/ind-=-transitivity _)
   ; cf. tests.rkt:1197
   (⊦= (go
        '(the
          (Π
              ((A U))
            (Π
                ((x A))
              (Π
                  ((y A))
                (Π ((p1 (= A x y))) (Π ((z A)) (Π ((p2 (= A y z))) (= A x z)))))))
          (λ (A)
            (λ (x)
              (λ (y)
                (λ (p1)
                  (λ (z)
                    (λ (p2)
                      (((ind-=
                         p1
                         (λ (w) (λ (p) (Π ((z₁ A)) (Π ((x₁ (= A w z₁))) (= A x z₁)))))                                (λ (z₁) (λ (eq2) eq2)))
                        z)
                       p2)))))))))
       (rep init-ctx (string->src "(the (Pi ((A U)
                                             (x A)
                                             (y A)
                                             (p1 (= A x y))
                                             (z A)
                                             (p2 (= A y z)))
                                            (= A x z))
                                        (λ (A x y p1)
                                          (ind-= p1
                                                 (λ (w)
                                                   (λ (p)
                                                     (Pi ((z A))
                                                         (-> (= A w z)
                                                             (= A x z)))))
                                                 (λ (z eq2)
                                                   eq2))))"))))

  ((test/L1234/stop-ind-=-wrong-motive _)
   ; cf. tests.rkt:1234
   (⊦= '("Expected"
         (Π ((z A)) (Π ((p2 (= A y z))) (= A x z)))
         "but given"
         (Π ((z A)) (Π ((x₁ (= A x z))) (= A y z))))
       (stop->message (rep init-ctx (string->src "(the (Pi ((A U)
                                                            (x A)
                                                            (y A)
                                                            (p1 (= A x y))
                                                            (z A)
                                                            (p2 (= A y z)))
                                                           (= A x z))
                                                       (λ (A x y p1)
                                                         (ind-= p1
                                                                (λ (w)
                                                                  (λ (p)
                                                                    (Pi ((z A))
                                                                        (-> (= A x z)
                                                                            (= A w z)))))
                                                                (λ (z eq2)
                                                                  eq2))))")))))

  ((test/L1255/stop-ind-=-not-equality _)
   ; cf. tests.rkt:1255
   (⊦= '("Expected evidence of equality, got " Nat)
       (stop->message (rep init-ctx (string->src "(the (Pi ((A U)
                                                            (x A)
                                                            (y A)
                                                            (p1 (= A x y))
                                                            (z A)
                                                            (p2 (= A y z)))
                                                           (= A x z))
                                                       (λ (A x y p1)
                                                         (ind-= 33
                                                                (λ (w)
                                                                  (λ (p)
                                                                    (Pi ((z A))
                                                                        (-> (= A x z)
                                                                            (= A w z)))))
                                                                (λ (z eq2)
                                                                  eq2))))")))))

  ((test/L1273/iter-Nat-higher-order _)
   ; cf. tests.rkt:1273
   (⊦= (go
        '(the
          (Π ((x Nat)) (Π ((x₁ Nat)) Nat))
          (λ (n)
            (λ (x₁)
              ((iter-Nat
                n
                (the (Π ((x Nat)) Nat) (λ (m) zero))
                (λ (times-of-n-1)
                  (λ (m) (iter-Nat m (the Nat (times-of-n-1 m)) (λ (x) (add1 x))))))
               x₁)))))
       (rep init-ctx
            (string->src "(the (-> Nat Nat Nat)
                               (lambda (n)
                                 (iter-Nat n
                                           (the (-> Nat Nat)
                                                (lambda (m)
                                                  zero))
                                           (lambda (times-of-n-1)
                                             (lambda (m)
                                               (iter-Nat m
                                                         (times-of-n-1 m)
                                                         (lambda (x)
                                                           (add1 x))))))))"))))

  ((test/L1299/lambda-repeated-binders _)
   ; cf. tests.rkt:1299
   (⊦= (go
        '(the
          (Π ((A U)) (Π ((D U)) (Π ((x A)) (Π ((x₁ D)) (Σ ((x₂ A)) D)))))
          (λ (x) (λ (x₁) (λ (x₂) (λ (y) (cons x₂ y)))))))
       (rep init-ctx (string->src "(the (Π ((A U) (D U)) (-> A D (Pair A D))) (λ (x x x y) (cons x y)))"))))

  ((test/L1306/lambda-repeated-binders-applied _)
   ; cf. tests.rkt:1306
   (⊦= (go
       '(the
         (Σ ((x₂ Nat)) Atom)
         (cons (add1 (add1 (add1 (add1 (add1 zero))))) 'five)))
       (rep init-ctx (string->src "((the (Π ((A U) (D U)) (-> A D (Pair A D))) (λ (x x x y) (cons x y))) Nat Atom 5 'five)"))))

  ((test/L1313/lambda-repeated-binders-partial _)
   ; cf. tests.rkt:1313
   (⊦= (go
       '(the
         (Π ((x₁ Atom)) (Σ ((x₂ Nat)) Atom))
         (λ (y) (cons (add1 (add1 (add1 (add1 (add1 zero))))) y))))
       (rep init-ctx (string->src "((the (Π ((A U) (D U)) (-> A D (Pair A D))) (λ (x x x y) (cons x y))) Nat Atom 5)"))))

  ; This is a regression test for issue #14, from
  ; https://github.com/the-little-typer/pie/issues/14
  ;
  ; Sorry for the length, I couldn't minimize it further.
  ((test/L1325/issue-14 _)
   ; cf. tests.rkt:1325
   (⊦= '((foo
          (def (Π ((X U))
                 (Π ((Y U))
                   (Π ((f (Π ((x X)) Y)))
                     (Π ((j Nat))
                       (Π ((v (Vec X j)))
                         (= (Vec Y (iter-Nat j (the Nat zero) (λ (x) (add1 x))))
                            (ind-Vec
                             j
                             (ind-Vec
                              j
                              v
                              (λ (n₁) (λ (v₁) (Vec Y n₁)))
                              vecnil
                              (λ (?) (λ (x) (λ (_) (λ (qwerty) (|vec::| (f x) qwerty))))))
                             (λ (z)
                               (λ (v₁) (Vec Y (iter-Nat z (the Nat zero) (λ (x) (add1 x))))))
                             vecnil
                             (λ (k) (λ (h) (λ (t) (λ (q) (|vec::| h q))))))
                            (ind-Vec
                             (iter-Nat j (the Nat zero) (λ (x) (add1 x)))
                             (ind-Vec
                              j
                              v
                              (λ (z)
                                (λ (v₁) (Vec X (iter-Nat z (the Nat zero) (λ (x) (add1 x))))))
                              vecnil
                              (λ (k) (λ (h) (λ (t) (λ (q) (|vec::| h q))))))
                             (λ (n₁) (λ (v₁) (Vec Y n₁)))
                             vecnil
                             (λ (?) (λ (x) (λ (_) (λ (qwerty) (|vec::| (f x) qwerty))))))))))))
            (λ (X)
              (λ (Y)
                (λ (f)
                  (λ (j)
                    (λ (v)
                      (ind-Vec
                       j
                       v
                       (λ (j₁)
                         (λ (v₁)
                           (=
                            (Vec Y (iter-Nat j₁ (the Nat zero) (λ (x) (add1 x))))
                            (ind-Vec
                             j₁
                             (ind-Vec
                              j₁
                              v₁
                              (λ (n₁) (λ (v₂) (Vec Y n₁)))
                              vecnil
                              (λ (?)
                                (λ (x) (λ (_) (λ (qwerty) (|vec::| (f x) qwerty))))))
                             (λ (z)
                               (λ (v₂)
                                 (Vec Y (iter-Nat z (the Nat zero) (λ (x) (add1 x))))))
                             vecnil
                             (λ (k) (λ (h) (λ (t) (λ (q) (|vec::| h q))))))
                            (ind-Vec
                             (iter-Nat j₁ (the Nat zero) (λ (x) (add1 x)))
                             (ind-Vec
                              j₁
                              v₁
                              (λ (z)
                                (λ (v₂)
                                  (Vec
                                   X
                                   (iter-Nat z (the Nat zero) (λ (x) (add1 x))))))
                              vecnil
                              (λ (k) (λ (h) (λ (t) (λ (q) (|vec::| h q))))))
                             (λ (n₁) (λ (v₂) (Vec Y n₁)))
                             vecnil
                             (λ (?)
                               (λ (x) (λ (_) (λ (qwerty) (|vec::| (f x) qwerty)))))))))
                       (same vecnil)
                       (λ (m)
                         (λ (x)
                           (λ (xs)
                             (λ (IH)
                               (trans
                                (same
                                 (|vec::|
                                  (f x)
                                  (ind-Vec
                                   m
                                   (ind-Vec
                                    m
                                    xs
                                    (λ (n₁) (λ (v₁) (Vec Y n₁)))
                                    vecnil
                                    (λ (?)
                                      (λ (x₁)
                                        (λ (_) (λ (qwerty) (|vec::| (f x₁) qwerty))))))
                                   (λ (z)
                                     (λ (v₁)
                                       (Vec
                                        Y
                                        (iter-Nat
                                         z
                                         (the Nat zero)
                                         (λ (x₁) (add1 x₁))))))
                                   vecnil
                                   (λ (k) (λ (h) (λ (t) (λ (q) (|vec::| h q))))))))
                                (cong
                                 IH
                                 (Vec
                                  Y
                                  (add1
                                   (iter-Nat m (the Nat zero) (λ (x₁) (add1 x₁)))))
                                 (λ (tl) (|vec::| (f x) tl))))))))))))))))
         (map-vec
          (def (Π ((A U))
                 (Π ((B U))
                   (Π ((n Nat))
                     (Π ((x (Π ((x A))
                              B)))
                       (Π ((x₁ (Vec A n)))
                         (Vec B n))))))
            (λ (A)
              (λ (B)
                (λ (n)
                  (λ (f)
                    (λ (v)
                      (ind-Vec
                       n
                       v
                       (λ (n₁) (λ (v₁) (Vec B n₁)))
                       vecnil
                       (λ (?)
                         (λ (x) (λ (_) (λ (qwerty) (|vec::| (f x) qwerty)))))))))))))
         (append-vec
          (def (Π ((E U))
                 (Π ((n Nat))
                   (Π ((m Nat))
                     (Π ((x (Vec E n)))
                       (Π ((x₁ (Vec E m)))
                         (Vec E (iter-Nat n (the Nat m) (λ (x₂) (add1 x₂)))))))))
            (λ (E)
              (λ (n)
                (λ (m)
                  (λ (xs)
                    (λ (ys)
                      (ind-Vec
                       n
                       xs
                       (λ (z)
                         (λ (v) (Vec E (iter-Nat z (the Nat m) (λ (x) (add1 x))))))
                       ys
                       (λ (k) (λ (h) (λ (t) (λ (q) (|vec::| h q)))))))))))))
         (* (def
              (Π ((x Nat)) (Π ((x₁ Nat)) Nat))
              (λ (x)
                (λ (y)
                  (iter-Nat
                   x
                   (the Nat zero)
                   (λ (y₁) (iter-Nat y (the Nat y₁) (λ (x₁) (add1 x₁)))))))))
         (+ (def
              (Π ((x Nat)) (Π ((x₁ Nat)) Nat))
              (λ (x) (λ (y) (iter-Nat x (the Nat y) (λ (x₁) (add1 x₁)))))))
         (succ (def (Π ((x Nat)) Nat) (λ (x) (add1 x)))))
       (read-back-ctx
        (decls->ctx "(claim succ (-> Nat Nat))"
                    "(define succ (λ (x) (add1 x)))"
                    "(claim + (-> Nat Nat Nat))"
                    "(define + (λ (x y) (iter-Nat x y succ)))"
                    "(claim * (-> Nat Nat Nat))"
                    "(define * (λ (x y) (iter-Nat x 0 (+ y))))"
                    "(claim append-vec
                            (Π ([E U]
                                [n Nat]
                                [m Nat])
                              (-> (Vec E n) (Vec E m)
                                  (Vec E (+ n m)))))"
                    "(define append-vec
                       (λ (E n m xs ys)
                         (ind-Vec n xs
                                  (λ (z v) (Vec E (+ z m)))
                                  ys
                                  (λ (k h t q)
                                    (vec:: h q)))))"
                    "(claim map-vec
                            (Π ((A U)
                                (B U)
                                [n Nat])
                              (-> (-> A B)
                                  (Vec A n)
                                  (Vec B n))))"
                    "(define map-vec
                       (λ (A B n f v)
                         (ind-Vec n v
                                  (λ (n v) (Vec B n))
                                  vecnil
                                  (λ (? x _ qwerty)
                                    (vec:: (f x) qwerty)))))"
                    "(claim foo
                            (Π ((X U)
                                (Y U)
                                (f (-> X Y))
                                (j Nat)
                                (v (Vec X j)))
                              (= (Vec Y (+ j 0))
                                 (append-vec Y j 0
                                             (map-vec X Y j f v)
                                             (map-vec X Y 0 f vecnil))
                                 (map-vec X Y (+ j 0) f
                                          (append-vec X j 0 v vecnil)))))"
                    "(define foo
                       (λ (X Y f j v)
                         (ind-Vec j v
                                  (λ (j v)
                                    (= (Vec Y (+ j 0))
                                       (append-vec Y j 0
                                                   (map-vec X Y j f v)
                                                   (map-vec X Y 0 f vecnil))
                                       (map-vec X Y (+ j 0) f
                                                (append-vec X j 0 v vecnil))))
                                  (same (map-vec X Y 0 f vecnil))
                                  (λ (m x xs IH)
                                    (trans (the (= (Vec Y (add1 (+ m 0)))
                                                   (append-vec Y (add1 m) 0
                                                               (map-vec X Y (add1 m) f
                                                                        (vec:: x xs))
                                                               (map-vec X Y 0 f vecnil))
                                                   (vec:: (f x)
                                                          (append-vec Y m 0
                                                                      (map-vec X Y m f xs)
                                                                      (map-vec X Y 0 f vecnil))))
                                                (same (vec:: (f x)
                                                             (append-vec Y m 0
                                                                         (map-vec X Y m f xs)
                                                                         (map-vec X Y 0 f vecnil)))))
                                           (cong IH (the (-> (Vec Y (+ m 0))
                                                             (Vec Y (add1 (+ m 0))))
                                                         (λ (tl)
                                                           (vec:: (f x) tl)))))))))"))))

  ; Regression test for issue #20, where the evaluation rule for ind-= was wrong.
  ((test/L1581/issue-20 _)
   ; cf. tests.rkt:1581
   (⊦= '((my-J-works
          (def
            (=
             (= Nat (add1 (add1 zero)) (add1 (add1 zero)))
             (same (add1 (add1 zero)))
             (same (add1 (add1 zero))))
            (same (same (add1 (add1 zero))))))
         (my-J
          (def
            (Π ((A U))
              (Π ((x A))
                (Π ((C (Π ((y A)) (Π ((x₁ (= A x y))) U))))
                  (Π ((y A))
                    (Π ((eq (= A x y))) (Π ((x₁ ((C x) (same x)))) ((C y) eq)))))))
            (λ (A)
              (λ (x)
                (λ (C)
                  (λ (y)
                    (λ (eq)
                      (λ (b)
                        (ind-= eq
                               (λ (to)
                                 (λ (p)
                                   ((C to) p)))
                               b))))))))))
       (read-back-ctx
        (decls->ctx "(claim my-J
                            (Π ((A U) (x A) (C (Π ((y A)) (-> (= A x y) U))))
                              (Π ((y A)
                                  (eq (= A x y)))
                                (-> (C x (same x))
                                    (C y eq)))))"
                    "(define my-J
                       (λ (A x C)
                         (λ (y eq b)
                           (ind-= eq C b))))"
                    "(claim my-J-works
                            (= (= Nat 2 2)
                               (my-J Nat 2 (λ (z eq) (= Nat 2 z))
                                     2 (same 2) (same 2))
                               (same 2)))"
                    ; The conversion check would have crashed here
                    ; due to a bad implementation of ind-=
                    "(define my-J-works
                       (same (same 2)))"))))

  ; Check that ∏ works as a way of writing Π
  ((test/L1642/Prod-as-Pi _)
   ; cf. tests.rkt:1642
   (⊦= (go '(the (Either Trivial Nat) (right (add1 (add1 zero)))))
       (rep init-ctx
           (string->src "((the (∏ ((A U) (B U))
                           (-> (Either A B)
                               (Either B A)))
                       (lambda (A B e)
                         (ind-Either e
                                     (lambda (_) (Either B A))
                                     (lambda (x) (right x))
                                     (lambda (x) (left x)))))
                  Nat Trivial (left 2))")))))

(unittest/✓ pie-suite)
