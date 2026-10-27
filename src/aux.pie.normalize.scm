;;; (aux pie normalize) -- normalization by evaluation, with call-by-need evaluation.
;;;
;;; A port of normalize.rkt of Pie, the language of "The Little Typer" by Daniel P. Friedman and David Thrane
;;; Christiansen (https://github.com/the-little-typer/pie, commit 2c89553). Pie is distributed under the
;;; GNU AGPL-3.0, and so is this file, which is derived from it.
;;;
;;; The file is ported whole and in upstream order. It holds call-by-need evaluation (later, undelay, now),
;;; the Π-type macro, the eliminators do-X and the evaluator val-of, the serialization of contexts, the
;;; read-back of types, values and neutral expressions, and two general helpers. Upstream provides
;;; everything it defines (cf. normalize.rkt:10), and so does this module, except the match expander !!,
;;; which has no counterpart in (aux base) patterns.
;;;
;;; Racket's match becomes match/first of (aux base), clause by clause in upstream order. A struct pattern
;;; (PI x A c) becomes #(PI ,x ,A ,c), the quoted symbol 'ZERO the bare symbol ZERO, and a match* over two
;;; values a match/first on their list, the two values being forced by a let* first, so that they are
;;; forced left to right as in Racket.
;;;
;;; The view (!! p) of normalize.rkt:86 matches p against (now v). A clause that uses it becomes a guard on
;;; (now v), followed in the body by a match1/first on (now v), which costs nothing more, because now keeps
;;; the value that it finds in the box of the DELAY. For instance (NEU (!! (PI x A c)) ne) is
;;;
;;;   ((#(NEU ,T ,ne) ⊣ (PI? (now T))) (match1/first (#(PI ,x ,A ,c) (now T)) ...))
;;;
;;; The guard runs only after the whole pattern of the clause matched, while Racket forces the value when
;;; the matcher reaches it, so a clause that fails on a later part has already forced it in Racket and not
;;; here. Forcing only fills a box with the value that it would hold later anyway, and the next clause that
;;; looks at the same part forces it in both, so the difference cannot be seen in what Pie computes or
;;; prints. The places where upstream demands a value, every `later` and every `now`, are the same here,
;;; because the tests of tests.rkt compare contexts down to the state of their DELAY boxes.
;;;
;;; Like upstream, the module imports (aux pie locations) without using it (cf. normalize.rkt:9).

(module (aux pie normalize)
  ( ; call-by-need evaluation
    later undelay now
    ; nested Π types
    Π-type
    ; the evaluator
    do-ap do-which-Nat do-iter-Nat do-rec-Nat do-ind-Nat do-car do-cdr do-ind-List do-rec-List
    do-ind-Absurd do-replace do-trans do-cong do-symm do-ind-= do-head do-tail ind-Vec-step-type do-ind-Vec
    do-ind-Either val-of
    ; context serialization
    read-back-ctx val-of-ctx
    ; normalization
    read-back-type read-back read-back-neutral
    ; general-purpose helpers
    val-of-closure val-in-ctx)

  (import scheme
          (chicken base)
          (chicken format)
          (aux base)
          (aux pie racket)
          (aux pie basics)
          ; unused, as upstream's require of location->srcloc, cf. normalize.rkt:9
          (aux pie locations))

  ;;; ==================== CALL-BY-NEED EVALUATION ====================

  ;; Pie is a total language, so every program terminates, and because the steps of evaluation are
  ;; deterministic, any order of evaluation gives the same answers. Strict evaluation would take many more
  ;; steps for many useful programs, though. zerop of chapter 3 of The Little Typer answers 'nil when the
  ;; value of its argument has add1 at the top and 't when it is zero, so a strict (zerop (double 10000))
  ;; would first find that (double 10000) is 20000, in 10000 steps, while a lazy one needs a single step to
  ;; see the add1 at the top, cf. normalize.rkt:15-28
  ;;
  ;; Pie evaluates by need, so that two expressions that use the same expression, such as a definition,
  ;; share its evaluation steps. A DELAY of basics is a value that stands for evaluation not done yet, to be
  ;; done on demand. Its box holds a DELAY-CLOS, an expression with its environment, until some operator
  ;; demands the value, and the value itself from then on, so that the work is shared, cf. normalize.rkt:30-46

  ;; later : Env Core -> Value, a DELAY of a DELAY-CLOS that delays the evaluation of expr,
  ;; cf. normalize.rkt:48-52
  (define (later ρ expr)
    (DELAY (box (DELAY-CLOS ρ expr))))

  ;; undelay : DELAY-CLOS -> Value, the value of the expression in a DELAY-CLOS, found by the evaluator,
  ;; cf. normalize.rkt:54-60
  (define (undelay c)
    (match1/first (#(DELAY-CLOS ,ρ ,expr) c)
      (now (val-of ρ expr))))

  ;; now : Value -> Value, the actual value that a DELAY stands for. A DELAY-CLOS in the box is computed
  ;; by undelay and replaces itself in the box, anything else there has been computed already. now must be
  ;; used whenever a value is inspected to see its form, because that requires the delayed steps to be
  ;; carried out. It is the hottest procedure of Pie, so it tests DELAY? instead of matching a pattern on
  ;; the record, cf. normalize.rkt:62-79
  (define (now v)
    (if (DELAY? v)
        (let* ((b (DELAY-val v))
               (v (unbox b)))
          (if (DELAY-CLOS? v)
              (let1 (the-value (undelay v))
                (set-box! b the-value)
                the-value)
              v))
        v))

  ;; !! is now inside a pattern, convenient when a part of a value below the top must be inspected, such as
  ;; the length in the Vec type of vecnil, which must be precisely zero. It becomes a guard and a nested
  ;; match, see the header, cf. normalize.rkt:81-91

  ;;; ==================== HELPER FOR CONSTRUCTING NESTED Π TYPES ====================

  ;; (Π-type ((x arg-t) ...) ret) is the value of a Π type whose argument types and result are values
  ;; built by Scheme code, with a higher-order closure for each binder, so that each x is bound in the
  ;; argument types that follow it and in ret, cf. normalize.rkt:97-102
  (define-syntax Π-type
    (syntax-rules ()
      ((_ () ret) ret)
      ((_ ((x arg-t) b ...) ret)
       (PI 'x arg-t (HO-CLOS (λ (x) (Π-type (b ...) ret)))))))

  ;;; ==================== THE EVALUATOR ====================

  ;; The procedures whose names begin with do- implement the eliminators of the same names,
  ;; cf. normalize.rkt:107-108

  ;; do-ap : Value Value -> Value, cf. normalize.rkt:110
  (define (do-ap rator-v rand-v)
    (match/first (now rator-v)
      (#(LAM ,x ,c)
       (val-of-closure c rand-v))
      ((#(NEU ,T ,ne) ⊣ (PI? (now T)))
       (match1/first (#(PI ,x ,A ,c) (now T))
         (NEU (val-of-closure c rand-v)
              (N-ap ne (THE A rand-v)))))))

  ;; do-which-Nat : Value Value Value Value -> Value, cf. normalize.rkt:120
  (define (do-which-Nat tgt-v b-tv b-v s-v)
    (match/first (now tgt-v)
      (ZERO b-v)
      (#(ADD1 ,n-1v)
       (do-ap s-v n-1v))
      ((#(NEU ,T ,ne) ⊣ (eq? (now T) 'NAT))
       (NEU b-tv
            (N-which-Nat ne
                         (THE b-tv b-v)
                         (THE (Π-type ((n 'NAT)) b-tv)
                              s-v))))))

  ;; do-iter-Nat : Value Value Value Value -> Value, cf. normalize.rkt:133
  (define (do-iter-Nat tgt-v b-tv b-v s-v)
    (match/first (now tgt-v)
      (ZERO b-v)
      (#(ADD1 ,n-1v)
       (do-ap s-v (do-iter-Nat n-1v b-tv b-v s-v)))
      ((#(NEU ,T ,ne) ⊣ (eq? (now T) 'NAT))
       (NEU b-tv
            (N-iter-Nat ne
                        (THE b-tv b-v)
                        (THE (Π-type ((n b-tv)) b-tv)
                             s-v))))))

  ;; do-rec-Nat : Value Value Value Value -> Value, cf. normalize.rkt:146
  (define (do-rec-Nat tgt-v b-tv b-v s-v)
    (match/first (now tgt-v)
      (ZERO b-v)
      (#(ADD1 ,n-1v)
       (do-ap (do-ap s-v n-1v)
              (do-rec-Nat n-1v b-tv b-v s-v)))
      ((#(NEU ,T ,ne) ⊣ (eq? (now T) 'NAT))
       (NEU b-tv
            (N-rec-Nat ne
                       (THE b-tv b-v)
                       (THE (Π-type ((n-1 'NAT)
                                     (ih b-tv))
                                    b-tv)
                            s-v))))))

  ;; do-ind-Nat : Value Value Value Value -> Value, cf. normalize.rkt:164
  (define (do-ind-Nat tgt-v mot-v b-v s-v)
    (match/first (now tgt-v)
      (ZERO b-v)
      (#(ADD1 ,n-1v)
       (do-ap (do-ap s-v n-1v)
              (do-ind-Nat n-1v mot-v b-v s-v)))
      ((#(NEU ,T ,ne) ⊣ (eq? (now T) 'NAT))
       (NEU (do-ap mot-v tgt-v)
            (N-ind-Nat
             ne
             (THE (Π-type ((x 'NAT)) 'UNIVERSE)
                  mot-v)
             (THE (do-ap mot-v 'ZERO) b-v)
             (THE (Π-type ((n-1 'NAT)
                           (ih (do-ap mot-v n-1)))
                          (do-ap mot-v (ADD1 n-1)))
                  s-v))))))

  ;; do-car : Value -> Value, cf. normalize.rkt:183
  (define (do-car p-v)
    (match/first (now p-v)
      (#(CONS ,a ,d) a)
      ((#(NEU ,T ,ne) ⊣ (SIGMA? (now T)))
       (match1/first (#(SIGMA ,x ,A ,c) (now T))
         (NEU A (N-car ne))))))

  ;; do-cdr : Value -> Value, cf. normalize.rkt:190
  (define (do-cdr p-v)
    (match/first (now p-v)
      (#(CONS ,a ,d)
       d)
      ((#(NEU ,T ,ne) ⊣ (SIGMA? (now T)))
       (match1/first (#(SIGMA ,x ,A ,c) (now T))
         (NEU (val-of-closure c (do-car p-v))
              (N-cdr ne))))))

  ;; do-ind-List : Value Value Value Value -> Value, cf. normalize.rkt:199
  (define (do-ind-List tgt-v mot-v b-v s-v)
    (match/first (now tgt-v)
      (NIL b-v)
      (#(|LIST::| ,h ,t)
       (do-ap (do-ap (do-ap s-v h) t)
              (do-ind-List t mot-v b-v s-v)))
      ((#(NEU ,T ,ne) ⊣ (LIST? (now T)))
       (match1/first (#(LIST ,E) (now T))
         (let1 (mot-tv (Π-type ((xs (LIST E))) 'UNIVERSE))
           (NEU (do-ap mot-v tgt-v)
                (N-ind-List
                 ne
                 (THE mot-tv mot-v)
                 (THE (do-ap mot-v 'NIL) b-v)
                 (THE (Π-type ((h E)
                               (t (LIST E))
                               (ih (do-ap mot-v t)))
                              (do-ap mot-v (|LIST::| h t)))
                      s-v))))))))

  ;; do-rec-List : Value Value Value Value -> Value, cf. normalize.rkt:220
  (define (do-rec-List tgt-v b-tv b-v s-v)
    (match/first (now tgt-v)
      (NIL b-v)
      (#(|LIST::| ,h ,t)
       (do-ap (do-ap (do-ap s-v h) t)
              (do-rec-List t b-tv b-v s-v)))
      ((#(NEU ,T ,ne) ⊣ (LIST? (now T)))
       (match1/first (#(LIST ,E) (now T))
         (NEU b-tv
              (N-rec-List
               ne
               (THE b-tv b-v)
               (THE (Π-type ((h E)
                             (t (LIST E))
                             (ih b-tv))
                            b-tv)
                    s-v)))))))

  ;; do-ind-Absurd : Value Value -> Value. Upstream's pattern (!! ABSURD) binds the pattern variable ABSURD
  ;; to the forced type instead of comparing it with the symbol, so the clause takes a neutral of any type,
  ;; after forcing the type. The port keeps both, cf. normalize.rkt:238
  (define (do-ind-Absurd tgt-v mot-v)
    (match/first (now tgt-v)
      (#(NEU ,T ,ne)
       (let1 (ABSURD (now T))
         (NEU mot-v
              (N-ind-Absurd ne (THE 'UNIVERSE mot-v)))))))

  ;; do-replace : Value Value Value -> Value, cf. normalize.rkt:245
  (define (do-replace tgt-v mot-v b-v)
    (match/first (now tgt-v)
      (#(SAME ,v)
       b-v)
      ((#(NEU ,T ,ne) ⊣ (EQUAL? (now T)))
       (match1/first (#(EQUAL ,A-v ,from-v ,to-v) (now T))
         (NEU (do-ap mot-v to-v)
              (N-replace ne
                         (THE (Π-type ((x A-v)) 'UNIVERSE)
                              mot-v)
                         (THE (do-ap mot-v from-v)
                              b-v)))))))

  ;; do-trans : Value Value -> Value, cf. normalize.rkt:259
  (define (do-trans tgt-1v tgt-2v)
    (let* ((tgt-1 (now tgt-1v))
           (tgt-2 (now tgt-2v)))
      (match/first (list tgt-1 tgt-2)
        ((#(SAME ,v) #(SAME _))
         (SAME v))
        (((#(SAME ,from-v) #(NEU ,T2 ,ne2)) ⊣ (EQUAL? (now T2)))
         (match1/first (#(EQUAL ,A-v _ ,to-v) (now T2))
           (NEU (EQUAL A-v from-v to-v)
                (N-trans2 (THE (EQUAL A-v from-v from-v) (SAME from-v))
                          ne2))))
        (((#(NEU ,T1 ,ne1) #(SAME ,to-v)) ⊣ (EQUAL? (now T1)))
         (match1/first (#(EQUAL ,A-v ,from-v _) (now T1))
           (NEU (EQUAL A-v from-v to-v)
                (N-trans1 ne1 (THE (EQUAL A-v to-v to-v) (SAME to-v))))))
        (((#(NEU ,T1 ,ne1) #(NEU ,T2 ,ne2)) ⊣ (and (EQUAL? (now T1)) (EQUAL? (now T2))))
         (match1/first ((#(EQUAL ,A-v ,from-v _) #(EQUAL _ _ ,to-v)) (list (now T1) (now T2)))
           (NEU (EQUAL A-v from-v to-v)
                (N-trans12 ne1 ne2)))))))

  ;; do-cong : Value Value Value -> Value, cf. normalize.rkt:275
  (define (do-cong tgt-v B-v fun-v)
    (match/first (now tgt-v)
      (#(SAME ,v)
       (SAME (do-ap fun-v v)))
      ((#(NEU ,T ,ne) ⊣ (EQUAL? (now T)))
       (match1/first (#(EQUAL ,A-v ,from-v ,to-v) (now T))
         (NEU (EQUAL B-v (do-ap fun-v from-v) (do-ap fun-v to-v))
              (N-cong ne (THE (Π-type ((x A-v)) B-v) fun-v)))))))

  ;; do-symm : Value -> Value, cf. normalize.rkt:284
  (define (do-symm tgt-v)
    (match/first (now tgt-v)
      (#(SAME ,v) (SAME v))
      ((#(NEU ,T ,ne) ⊣ (EQUAL? (now T)))
       (match1/first (#(EQUAL ,A-v ,from-v ,to-v) (now T))
         (NEU (EQUAL A-v to-v from-v)
              (N-symm ne))))))

  ;; do-ind-= : Value Value Value -> Value, cf. normalize.rkt:293
  (define (do-ind-= tgt-v motive-v base-v)
    (match/first (now tgt-v)
      (#(SAME ,v) base-v)
      ((#(NEU ,T ,ne) ⊣ (EQUAL? (now T)))
       (match1/first (#(EQUAL ,A ,from ,to) (now T))
         (NEU (do-ap (do-ap motive-v to) tgt-v)
              (N-ind-= ne
                       (THE (Π-type ((to A)
                                     (p (EQUAL A from to)))
                                    'UNIVERSE)
                            motive-v)
                       (THE (do-ap (do-ap motive-v from)
                                   (SAME from))
                            base-v)))))))

  ;; do-head : Value -> Value. The pattern (NEU (!! (VEC Ev (!! (ADD1 len-1v)))) ne) forces the type and
  ;; then its length, in this order, cf. normalize.rkt:308
  (define (do-head tgt-v)
    (match/first (now tgt-v)
      (#(|VEC::| ,hv ,tv) hv)
      ((#(NEU ,T ,ne) ⊣ (and (VEC? (now T)) (ADD1? (now (VEC-length (now T))))))
       (match1/first (#(VEC ,Ev ,len) (now T))
         (match1/first (#(ADD1 ,len-1v) (now len))
           (NEU Ev (N-head ne)))))))

  ;; do-tail : Value -> Value, cf. normalize.rkt:316
  (define (do-tail tgt-v)
    (match/first (now tgt-v)
      (#(|VEC::| ,hv ,tv) tv)
      ((#(NEU ,T ,ne) ⊣ (and (VEC? (now T)) (ADD1? (now (VEC-length (now T))))))
       (match1/first (#(VEC ,Ev ,len) (now T))
         (match1/first (#(ADD1 ,len-1v) (now len))
           (NEU (VEC Ev len-1v) (N-tail ne)))))))

  ;; ind-Vec-step-type : Value Value -> Value, the type of the step of ind-Vec, cf. normalize.rkt:323
  (define (ind-Vec-step-type Ev mot-v)
    (Π-type ((k 'NAT)
             (e Ev)
             (es (VEC Ev k))
             (ih (do-ap (do-ap mot-v k) es)))
            (do-ap (do-ap mot-v (ADD1 k)) (|VEC::| e es))))

  ;; do-ind-Vec : Value Value Value Value Value -> Value. In the fourth clause the pattern variable len-v
  ;; shadows the argument len-v with its forced value, which the body then uses, while the third clause
  ;; names the neutral length len and uses the argument len-v, cf. normalize.rkt:331
  (define (do-ind-Vec len-v vec-v mot-v b-v s-v)
    (let* ((len-v* (now len-v))
           (vec-v* (now vec-v)))
      (match/first (list len-v* vec-v*)
        ((ZERO VECNIL) b-v)
        ((#(ADD1 ,len-1-v) #(|VEC::| ,h ,t))
         (do-ap (do-ap (do-ap (do-ap s-v len-1-v) h) (do-tail vec-v))
                (do-ind-Vec len-1-v t mot-v b-v s-v)))
        (((#(NEU ,Tl ,len) #(NEU ,Tv ,ne)) ⊣ (and (eq? (now Tl) 'NAT) (VEC? (now Tv))))
         (match1/first (#(VEC ,Ev _) (now Tv))
           (NEU (do-ap (do-ap mot-v len-v) vec-v)
                (N-ind-Vec12 len
                             ne
                             (THE (Π-type ((k 'NAT)
                                           (es (VEC Ev k)))
                                          'UNIVERSE)
                                  mot-v)
                             (THE (do-ap (do-ap mot-v 'ZERO) 'VECNIL) b-v)
                             (THE (ind-Vec-step-type Ev mot-v)
                                  s-v)))))
        (((,len-v #(NEU ,Tv ,ne)) ⊣ (VEC? (now Tv)))
         (match1/first (#(VEC ,Ev _) (now Tv))
           (NEU (do-ap (do-ap mot-v len-v) vec-v)
                (N-ind-Vec2 (THE 'NAT len-v)
                            ne
                            (THE (Π-type ((k 'NAT)
                                          (es (VEC Ev k)))
                                         'UNIVERSE)
                                 mot-v)
                            (THE (do-ap (do-ap mot-v 'ZERO) 'VECNIL)
                                 b-v)
                            (THE (ind-Vec-step-type Ev mot-v) s-v))))))))

  ;; do-ind-Either : Value Value Value Value -> Value, cf. normalize.rkt:361
  (define (do-ind-Either tgt mot l r)
    (match/first (now tgt)
      (#(LEFT ,x)
       (do-ap l x))
      (#(RIGHT ,x)
       (do-ap r x))
      ((#(NEU ,T ,ne) ⊣ (EITHER? (now T)))
       (match1/first (#(EITHER ,Lv ,Rv) (now T))
         (let1 (mot-tv (Π-type ((x (EITHER Lv Rv))) 'UNIVERSE))
           (NEU (do-ap mot tgt)
                (N-ind-Either ne
                              (THE mot-tv mot)
                              (THE (Π-type ((x Lv))
                                           (do-ap mot (LEFT x)))
                                   l)
                              (THE (Π-type ((x Rv))
                                           (do-ap mot (RIGHT x)))
                                   r))))))))

  ;; val-of : Env Core -> Value, the main evaluator. Instead of calling itself on the parts of e, it
  ;; delays them with later, all but the outermost constructor or type constructor. The two laters of the
  ;; TODO clause are separate, as upstream, cf. normalize.rkt:380-484
  (define (val-of ρ e)
    (match/first e
      ((the ,t ,expr) (val-of ρ expr))
      (U 'UNIVERSE)
      (Nat 'NAT)
      (zero 'ZERO)
      ((add1 ,n) (ADD1 (later ρ n)))
      ((Π ((,x ,A)) ,B)
       (let1 (A-v (later ρ A))
         (PI x A-v (FO-CLOS ρ x B))))
      ((λ (,x) ,b)
       (LAM x (FO-CLOS ρ x b)))
      ((which-Nat ,tgt (the ,b-t ,b) ,s)
       (do-which-Nat (later ρ tgt)
                     (later ρ b-t)
                     (later ρ b)
                     (later ρ s)))
      ((iter-Nat ,tgt (the ,b-t ,b) ,s)
       (do-iter-Nat (later ρ tgt)
                    (later ρ b-t)
                    (later ρ b)
                    (later ρ s)))
      ((rec-Nat ,tgt (the ,b-t ,b) ,s)
       (do-rec-Nat (later ρ tgt)
                   (later ρ b-t)
                   (later ρ b)
                   (later ρ s)))
      ((ind-Nat ,tgt ,mot ,b ,s)
       (do-ind-Nat (later ρ tgt)
                   (later ρ mot)
                   (later ρ b)
                   (later ρ s)))
      (Atom 'ATOM)
      ((Σ ((,x ,A)) ,D)
       (let1 (A-v (later ρ A))
         (SIGMA x A-v (FO-CLOS ρ x D))))
      ((cons ,a ,d) (CONS (later ρ a) (later ρ d)))
      ((car ,p) (do-car (later ρ p)))
      ((cdr ,p) (do-cdr (later ρ p)))
      (((quote ,a) ⊣ (symbol? a)) (QUOTE a))
      (Trivial 'TRIVIAL)
      (sole 'SOLE)
      (nil 'NIL)
      ((|::| ,h ,t) (|LIST::| (later ρ h) (later ρ t)))
      ((List ,E) (LIST (later ρ E)))
      ((ind-List ,tgt ,mot ,b ,s)
       (do-ind-List (later ρ tgt)
                    (later ρ mot)
                    (later ρ b)
                    (later ρ s)))
      ((rec-List ,tgt (the ,b-t ,b) ,s)
       (do-rec-List (later ρ tgt)
                    (later ρ b-t)
                    (later ρ b)
                    (later ρ s)))
      (Absurd 'ABSURD)
      ((ind-Absurd ,tgt ,mot)
       (do-ind-Absurd (later ρ tgt) (later ρ mot)))
      ((= ,A ,from ,to)
       (EQUAL (later ρ A) (later ρ from) (later ρ to)))
      ((same ,e)
       (SAME (later ρ e)))
      ((replace ,tgt ,mot ,b)
       (do-replace (later ρ tgt) (later ρ mot) (later ρ b)))
      ((trans ,p1 ,p2)
       (do-trans (later ρ p1) (later ρ p2)))
      ((cong ,p1 ,p2 ,p3)
       (do-cong (later ρ p1) (later ρ p2) (later ρ p3)))
      ((symm ,p)
       (do-symm (later ρ p)))
      ((ind-= ,tgt ,mot ,b)
       (do-ind-= (later ρ tgt) (later ρ mot) (later ρ b)))
      ((Vec ,E ,len)
       (VEC (later ρ E) (later ρ len)))
      (vecnil 'VECNIL)
      ((|vec::| ,h ,t) (|VEC::| (later ρ h) (later ρ t)))
      ((head ,es) (do-head (later ρ es)))
      ((tail ,es) (do-tail (later ρ es)))
      ((ind-Vec ,len ,es ,mot ,b ,s)
       (do-ind-Vec (later ρ len)
                   (later ρ es)
                   (later ρ mot)
                   (later ρ b)
                   (later ρ s)))
      ((Either ,L ,R) (EITHER (later ρ L) (later ρ R)))
      ((left ,l) (LEFT (later ρ l)))
      ((right ,r) (RIGHT (later ρ r)))
      ((ind-Either ,tgt ,mot ,l ,r)
       (do-ind-Either (later ρ tgt)
                      (later ρ mot)
                      (later ρ l)
                      (later ρ r)))
      ((,rator ,rand)
       (do-ap (later ρ rator) (later ρ rand)))
      ((TODO ,where ,type)
       (NEU (later ρ type) (N-TODO where (later ρ type))))
      (,x
       (if (and (symbol? x) (var-name? x))
           (var-val ρ x)
           (error (format "No evaluator for ~a" x))))))

  ;;; ==================== CONTEXT SERIALIZATION AND DESERIALIZATION ====================

  ;; Both the typechecker and the REPL need to serialize contexts, which hold values, into pure
  ;; S-expressions, simple data that can be saved to disk or sent over a network. The drawback of the
  ;; approach is that it loses laziness, because serializing a context evaluates every value in it
  ;; strictly, which may be slow for values that take long to compute, cf. normalize.rkt:489-497

  ;; read-back-ctx : Ctx -> Serializable-Ctx, cf. normalize.rkt:499
  (define (read-back-ctx Γ)
    (match/first Γ
      (()
       '())
      (((,x . #(free ,t)) . ,Γ-next)
       (cons (list x (list 'free (read-back-type Γ-next t)))
             (read-back-ctx Γ-next)))
      (((,x . #(def ,t ,v)) . ,Γ-next)
       (cons (list x (list 'def (read-back-type Γ-next t) (read-back Γ-next t v)))
             (read-back-ctx Γ-next)))
      (((,x . #(claim ,t)) . ,Γ-next)
       (cons (list x (list 'claim (read-back-type Γ-next t)))
             (read-back-ctx Γ-next)))))

  ;; val-of-ctx : Serializable-Ctx -> Ctx, cf. normalize.rkt:514
  (define (val-of-ctx ctx-list)
    (match/first ctx-list
      (() '())
      (((,x ,b) . ,ctx-tail)
       (let1 (Γ (val-of-ctx ctx-tail))
         (cons (cons x
                     (match/first b
                       ((free ,t) (free (val-in-ctx Γ t)))
                       ((def ,t ,e) (def (val-in-ctx Γ t) (val-in-ctx Γ e)))
                       ((claim ,t) (claim (val-in-ctx Γ t)))))
               Γ)))))

  ;;; ==================== NORMALIZATION ====================

  ;; read-back-type : Ctx Value -> Core, the Core syntax of the value of a type. Read-back types are
  ;; compared for sameness with α-equiv?. In the last clause UNIVERSE is a pattern variable upstream, not
  ;; the symbol, so the clause takes a neutral of any type, and so does the port, cf. normalize.rkt:529-562
  (define (read-back-type Γ tv)
    (match/first (now tv)
      (UNIVERSE 'U)
      (NAT 'Nat)
      (#(PI ,x ,A ,c)
       (let ((A-e (read-back-type Γ A))
             (x^ (fresh Γ x)))
         `(Π ((,x^ ,A-e))
            ,(let1 (Γ/x^ (bind-free Γ x^ A))
               (read-back-type Γ/x^ (val-of-closure c (NEU A (N-var x^))))))))
      (ATOM 'Atom)
      (#(SIGMA ,x ,A ,c)
       (let ((A-e (read-back-type Γ A))
             (x^ (fresh Γ x)))
         `(Σ ((,x^ ,A-e))
            ,(let1 (Γ/x^ (bind-free Γ x^ A))
               (read-back-type Γ/x^ (val-of-closure c (NEU A (N-var x^))))))))
      (TRIVIAL 'Trivial)
      (#(LIST ,E) `(List ,(read-back-type Γ E)))
      (ABSURD 'Absurd)
      (#(EQUAL ,Av ,fromv ,tov)
       `(= ,(read-back-type Γ Av)
           ,(read-back Γ Av fromv)
           ,(read-back Γ Av tov)))
      (#(VEC ,Ev ,lenv)
       `(Vec ,(read-back-type Γ Ev) ,(read-back Γ 'NAT lenv)))
      (#(EITHER ,Lv ,Rv)
       `(Either ,(read-back-type Γ Lv) ,(read-back-type Γ Rv)))
      (#(NEU ,UNIVERSE ,ne)
       (read-back-neutral Γ ne))))

  ;; read-back : Ctx Value Value -> Core, the Core syntax of a value. The type determines the process,
  ;; which is what allows η-expansion. The first clause names the forced value v, like the argument that
  ;; it shadows, and the PI clause names it f, cf. normalize.rkt:564-611
  (define (read-back Γ tv v)
    (let* ((tv* (now tv))
           (v* (now v)))
      (match/first (list tv* v*)
        ((UNIVERSE ,v) (read-back-type Γ v))
        ((NAT ZERO) 'zero)
        ((NAT #(ADD1 ,n-1))
         `(add1 ,(read-back Γ 'NAT n-1)))
        ((#(PI ,x ,A ,c) ,f)
         (let1 (y (match/first f
                    (#(LAM ,y _) y)
                    (_ x)))
           (let1 (x^ (fresh Γ y))
             `(λ (,x^)
                ,(read-back
                  (bind-free Γ x^ A)
                  (val-of-closure c (NEU A (N-var x^)))
                  (do-ap f (NEU A (N-var x^))))))))
        ((#(SIGMA ,x ,A ,c) ,p-v)
         (let1 (the-car (do-car p-v))
           `(cons ,(read-back Γ A the-car)
                  ,(read-back Γ
                              (val-of-closure c the-car)
                              (do-cdr p-v)))))
        ((ATOM #(QUOTE ,a))
         `(quote ,a))
        ((TRIVIAL _) 'sole) ; η-expansion
        ((#(LIST ,E) NIL) 'nil)
        ((#(LIST ,E) #(|LIST::| ,h ,t))
         `(|::| ,(read-back Γ E h) ,(read-back Γ (LIST E) t)))
        ((ABSURD #(NEU _ ,ne))
         ;; This type annotation is half of the η law. See the implementation of α-equiv? for the other
         ;; half.
         `(the Absurd ,(read-back-neutral Γ ne)))
        ((#(EQUAL ,Av _ _) #(SAME ,v))
         `(same ,(read-back Γ Av v)))
        (((#(VEC ,Ev ,len) _) ⊣ (eq? (now len) 'ZERO)) 'vecnil)
        (((#(VEC ,Ev ,len) #(|VEC::| ,h ,t)) ⊣ (ADD1? (now len)))
         (match1/first (#(ADD1 ,len-1v) (now len))
           `(|vec::| ,(read-back Γ Ev h)
                     ,(read-back Γ (VEC Ev len-1v) t))))
        ((#(EITHER ,Lv ,Rv) #(LEFT ,lv))
         `(left ,(read-back Γ Lv lv)))
        ((#(EITHER ,Lv ,Rv) #(RIGHT ,rv))
         `(right ,(read-back Γ Rv rv)))
        ((_ #(NEU _ ,ne))
         (read-back-neutral Γ ne)))))

  ;; read-back-neutral : Ctx Neutral -> Core, the Core syntax of a neutral expression. The type does not
  ;; determine the process, because read-back has done the reading back driven by types by the time that it
  ;; calls read-back-neutral, cf. normalize.rkt:613-711
  (define (read-back-neutral Γ ne)
    (match/first ne
      (#(N-which-Nat ,tgt #(THE ,b-tv ,b-v) #(THE ,s-tv ,s-v))
       `(which-Nat ,(read-back-neutral Γ tgt)
                   (the ,(read-back-type Γ b-tv)
                        ,(read-back Γ b-tv b-v))
                   ,(read-back Γ s-tv s-v)))
      (#(N-iter-Nat ,tgt #(THE ,b-tv ,b-v) #(THE ,s-tv ,s-v))
       `(iter-Nat ,(read-back-neutral Γ tgt)
                  (the ,(read-back-type Γ b-tv)
                       ,(read-back Γ b-tv b-v))
                  ,(read-back Γ s-tv s-v)))
      (#(N-rec-Nat ,tgt #(THE ,b-tv ,b-v) #(THE ,s-tv ,s-v))
       `(rec-Nat ,(read-back-neutral Γ tgt)
                 (the ,(read-back-type Γ b-tv)
                      ,(read-back Γ b-tv b-v))
                 ,(read-back Γ s-tv s-v)))
      (#(N-ind-Nat ,tgt
                   #(THE ,mot-tv ,mot-v)
                   #(THE ,b-tv ,b-v)
                   #(THE ,s-tv ,s-v))
       `(ind-Nat ,(read-back-neutral Γ tgt)
                 ,(read-back Γ mot-tv mot-v)
                 ,(read-back Γ b-tv b-v)
                 ,(read-back Γ s-tv s-v)))
      (#(N-car ,tgt)
       `(car ,(read-back-neutral Γ tgt)))
      (#(N-cdr ,tgt)
       `(cdr ,(read-back-neutral Γ tgt)))
      (#(N-ind-List ,tgt #(THE ,mot-t ,mot) #(THE ,b-t ,b) #(THE ,s-t ,s))
       `(ind-List ,(read-back-neutral Γ tgt)
                  ,(read-back Γ mot-t mot)
                  ,(read-back Γ b-t b)
                  ,(read-back Γ s-t s)))
      (#(N-rec-List ,tgt #(THE ,b-t ,b) #(THE ,s-t ,s))
       `(rec-List ,(read-back-neutral Γ tgt)
                  (the ,(read-back-type Γ b-t)
                       ,(read-back Γ b-t b))
                  ,(read-back Γ s-t s)))
      (#(N-ind-Absurd ,tgt #(THE ,tv ,ttv))
       ;; Here's some Absurd η. The rest is in α-equiv?.
       `(ind-Absurd (the Absurd ,(read-back-neutral Γ tgt))
                    ,(read-back Γ tv ttv)))
      (#(N-replace ,tgt #(THE ,mot-tv ,mot-v) #(THE ,b-tv ,b-v))
       `(replace ,(read-back-neutral Γ tgt)
                 ,(read-back Γ mot-tv mot-v)
                 ,(read-back Γ b-tv b-v)))
      (#(N-trans12 ,p1 ,p2)
       `(trans ,(read-back-neutral Γ p1) ,(read-back-neutral Γ p2)))
      (#(N-trans1 ,ne #(THE ,t ,v))
       `(trans ,(read-back-neutral Γ ne) ,(read-back Γ t v)))
      (#(N-trans2 #(THE ,t ,v) ,ne)
       `(trans ,(read-back Γ t v) ,(read-back-neutral Γ ne)))
      (#(N-cong ,ne #(THE #(PI ,y ,Av ,c) ,v))
       `(cong ,(read-back-neutral Γ ne)
              ,(read-back-type Γ (val-of-closure c 'ABSURD))
              ,(read-back Γ (PI y Av c) v)))
      (#(N-symm ,ne)
       `(symm ,(read-back-neutral Γ ne)))
      (#(N-ind-= ,ne #(THE ,mot-t ,mot) #(THE ,b-t ,b))
       `(ind-= ,(read-back-neutral Γ ne)
               ,(read-back Γ mot-t mot)
               ,(read-back Γ b-t b)))
      (#(N-head ,ne)
       `(head ,(read-back-neutral Γ ne)))
      (#(N-tail ,ne)
       `(tail ,(read-back-neutral Γ ne)))
      (#(N-ind-Vec1 ,len #(THE ,es-t ,es-v) #(THE ,mot-t ,mot) #(THE ,b-t ,b) #(THE ,s-t ,s))
       `(ind-Vec ,(read-back-neutral Γ len)
                 ,(read-back Γ es-t es-v)
                 ,(read-back Γ mot-t mot)
                 ,(read-back Γ b-t b)
                 ,(read-back Γ s-t s)))
      (#(N-ind-Vec2 #(THE ,len-t ,len-v) ,es #(THE ,mot-t ,mot) #(THE ,b-t ,b) #(THE ,s-t ,s))
       `(ind-Vec ,(read-back Γ len-t len-v)
                 ,(read-back-neutral Γ es)
                 ,(read-back Γ mot-t mot)
                 ,(read-back Γ b-t b)
                 ,(read-back Γ s-t s)))
      (#(N-ind-Vec12 ,len ,es #(THE ,mot-t ,mot) #(THE ,b-t ,b) #(THE ,s-t ,s))
       `(ind-Vec ,(read-back-neutral Γ len)
                 ,(read-back-neutral Γ es)
                 ,(read-back Γ mot-t mot)
                 ,(read-back Γ b-t b)
                 ,(read-back Γ s-t s)))
      (#(N-ind-Either ,tgt #(THE ,mot-tv ,mot-v) #(THE ,l-tv ,l-v) #(THE ,r-tv ,r-v))
       `(ind-Either ,(read-back-neutral Γ tgt)
                    ,(read-back Γ mot-tv mot-v)
                    ,(read-back Γ l-tv l-v)
                    ,(read-back Γ r-tv r-v)))
      (#(N-ap ,tgt #(THE ,arg-tv ,arg-v))
       `(,(read-back-neutral Γ tgt)
         ,(read-back Γ arg-tv arg-v)))
      (#(N-var ,x) x)
      (#(N-TODO ,where ,tyv) `(TODO ,where ,(read-back-type Γ tyv)))))

  ;;; ==================== GENERAL-PURPOSE HELPERS ====================

  ;; val-of-closure : Closure Value -> Value, the value of a closure given a value for its free variable.
  ;; It does not apply to a DELAY-CLOS, whose laziness closure has no free variable and is only a delayed
  ;; computation, cf. normalize.rkt:716-725
  (define (val-of-closure c v)
    (match/first c
      (#(FO-CLOS ,ρ ,x ,e)
       (val-of (extend-env ρ x v) e))
      (#(HO-CLOS ,fun) (fun v))))

  ;; val-in-ctx : Ctx Core -> Value, the value of an expression in the environment of a context,
  ;; cf. normalize.rkt:727-731
  (define (val-in-ctx Γ e)
    (val-of (ctx->env Γ) e)))
