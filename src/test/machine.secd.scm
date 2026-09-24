
(import (aux unittest) (aux base) (aux commons) (aux machine env) (aux machine secd)
        (only srfi-1 last) (chicken format) (chicken condition) (chicken irregex))

; Printed procedures depend on the CHICKEN build (`#<procedure (commons#² x394)>` on CHICKEN 5):
; the original expected strings are kept verbatim and both sides are normalised.
(define normalise
  (λ (str)
    (irregex-replace/all "#<procedure[^>]*>" str "#<procedure>")))

; the strict form of the old `test-error`: it fails when nothing is raised.
(define-syntax ⊨/error
  (syntax-rules ()
    ((_ expr) (⊨ (condition-case (begin expr #f) ((exn) #t))))))

(define v 'x)
(define e (Comb
            (Lambda v
              (Comb
                (Comb
                  (Comb
                    (Id 'p)
                    (Comb
                      (Comb
                        (Id '*)
                        (Comb
                          (Id '²)
                          (Id v)))
                      (Id 'a)))
                  (Comb
                    (Comb
                      (Id '*)
                      (Id v))
                    (Id 'b)))
                (Id 'c)))
            (Id 'd)))

; the environment of the first block of the original test.
(define make-E/first
  (τ ((extend E₀)
      `(a . ,1) `(b . ,2) `(c . ,3) `(d . ,4) `(o . ,-24)
      `(zero . 0) `(one . 1) `(two . 2) `(three . 3)
      `(p . ,(λ (x) (λ (y) (λ (z) (+ x y z)))))
      `(+ . ,(λ (x) (λ (y) (+ x y))))
      `(- . ,(λ (x) (λ (y) (- x y))))
      `(* . ,(λ (x) (λ (y) (* x y))))
      `(² . ,²) `(null? . ,null?) `(pair? . ,pair?))))

; the environment of the second block; environments are tabled, so each case that prints one
; builds its own, as the original did once.
(define make-E
  (τ ((extend E₀)
      `(² . ,²)
      `(+ . ,(λ (x) (λ (y) (+ x y))))
      `(- . ,(λ (x) (λ (y) (- x y))))
      `(three . 3)
      `(two . 2)
      `(* . ,*)
      `(add1 . ,add1)
      `(null? . ,null?)
      `(cdr . ,cdr)
      `(zero . 0)
      `(ones . ,(list 1 1 1))
      `(one . 1))))

(define control (curryfy '((λ (f x) (f (f x))) ² three)))
(define control₁ (curryfy '((λ (f) (λ (x) (f (f x)))) ² three)))
(define control₂ (curryfy '((λ (f) (λ (x) (f (f x)))) (λ (x) (* x x)) three)))
(define length₀ '(λ (L)
                   (λ (l)
                     (cond
                       ((null? l) zero)
                       (else (add1 (L (cdr l))))))))
(define Y₀ (let1 (h '(λ (g) (f (λ (x) ((g g) x)))))
             `(λ (f) (,h ,h))))
(define Y/length₀ (curryfy `(Y ,length₀)))
(define Y₁ (curryfy `((Y ,length₀) ones)))
(define s₁ (status-init '() ((○ compile expression->de-bruijn) control₁)))
(define s₁⁺ (status-init '() ((○ compile⁺ expression->de-bruijn) control₁)))
(define s₂⁺ (status-init '() ((○ compile⁺ expression->de-bruijn) control₂)))
(define Y⁺ (status-init '() ((○ compile⁺ expression->de-bruijn) Y/length₀)))
(define Y₁⁺ (status-init '() ((○ compile⁺ expression->de-bruijn) Y₁)))

; each step is printed, as in the original, to stdout that the unittest captures.
(define F
  (λ (→)
    (λ (s)
      (format #t "~a\n" s)
      (→ s))))

(define →/interpreted* (rtc (F →/interpreted)))
(define →/compiled*
  (λ (s) ((rtc (F (→/compiled (make-E)))) s)))
(define →/compiled⁺*
  (λ (s) ((rtc (F (→/compiled⁺ (make-E)))) s)))

(define final
  (λ (s)
    (list
      ((○ car status-S) s)
      (status-E s)
      (status-C s)
      (status-D s))))

; from on-scheme SECD-machine-test: its 49 live checks.
(define-suite secd-machine-suite

  ((doc r) `((structure/section "The SECD machine")
             (p "Ported from on-scheme's " (code/inline "SECD-machine.scm") ".")
             (structure/section "Implementation")
             (code/scheme/file "../aux.machine.secd.scm")))

  ((test/printer-and-curryfy _)
   (⊦= "((λ (x) (((p ((* (² x)) a)) ((* x) b)) c)) d)" (to-string e))
   (⊦= (Lambda 'x (Id 'y)) (curryfy '(λ (x) y)))
   (⊦= (Lambda 'x (Comb (Id 'y) (Id 'z))) (curryfy '(λ (x) (y z))))
   (⊦= e (curryfy `((λ (,v) (p (* (² ,v) a) (* ,v b) c)) d))))

  ((test/lambda-arith _)
   (⊦= 27 ((λ (x a b c) (+ (* (² x) a) (* x b) c)) 4 1 2 3))
   (let1 (p (λ (x) (λ (y) (λ (z) (+ x y z)))))
     (⊦= 27 ((λ (x a b c) (((p (((curry₁ *) (² x)) a)) (((curry₁ *) x) b)) c)) 4 1 2 3))))

  ((test/value-env _)
   (let1 (E (make-E/first))
     (⊦= 27 ((value E) e))

     (⊦= "((λ (x) (λ (y) (((p ((* (² x)) a)) ((* x) b)) y))) d)"
         ((○ to-string curryfy) `((λ (,v y) (p (* (² ,v) a) (* ,v b) y)) d)))

     (⊨ ((○ procedure? (value E) curryfy)
         `((λ (,v y) (p (* (² ,v) a) (* ,v b) y)) d)))

     (let1 (e₁ `((λ (,v y) (p (* (² ,v) a) (* ,v b) y)) d o))
       (⊦= "(((λ (x) (λ (y) (((p ((* (² x)) a)) ((* x) b)) y))) d) o)"
           ((○ to-string curryfy) e₁))
       (⊦= 0 ((○ (value E) curryfy) e₁)))

     (let1 (e₂ '(cond
                  ((null? l) zero)
                  ((pair? l) one)
                  (else two)))
       (⊦= "(if (null? l) zero (if (pair? l) one two))"
           ((○ to-string curryfy) e₂))
       (⊦= 0 ((○ (value ((extend E) `(l . ,(list)))) curryfy) e₂))
       (⊦= 1 ((○ (value ((extend E) `(l . ,(list 1)))) curryfy) e₂))
       (⊦= 2 ((○ (value ((extend E) `(l . ,3))) curryfy) e₂)))))

  ((test/Y-combinator _)
   (⊦= 3 ((Y (λ (L)
               (λ (l)
                 (cond/λ l
                   (null? (K 0))
                   (else (○ add1 L cdr))))))
          '(1 1 1))))

  ((test/status-init-printer _)
   (let1 (E (make-E))
     (⊨ (equal? control control₁))
     (⊦= "(S ())\n(E ())\n(C ((((λ (f) (λ (x) (f (f x)))) ²) three)))\n(D #<unspecified>)"
         (to-string (status-init E (list control))))))

  ; the hidden hash tables are folded in an unspecified order: the alist is sorted with
  ; `sort/lex<=?` (which gives the original order here) and the procedure name normalised.
  ((test/interpreted-final _)
   (let* ((E (make-E))
          (s₀ (status-init E (list control))))
     (⊦= `(81 ,(normalise "(((three) . 3) ((²) . #<procedure (commons#² x394)>))") ,'() ,(void))
         (let1 (s (last (→/interpreted* s₀)))
           (list
             ((○ car status-S) s)
             ((○ normalise to-string sort/lex<=? E->alist status-E) s)
             (status-C s)
             (status-D s))))
     ; new, structural: the procedure hidden by the normalisation is `²` itself.
     (let1 (s (last (→/interpreted* (status-init (make-E) (list control)))))
       (⊦= ² (cdr (assoc '(²) (E->alist (status-E s))))))))

  ((test/de-bruijn _)
   (let1 (t '(λ (x) ((g x) (λ (y) ((λ (z) (x y z)) (f x))))))
     (⊦= "(λ ((g 0) (λ ((λ ((2 1) 0)) (f 1)))))"
         ((○ to-string expression->de-bruijn curryfy) t)))
   (⊦= "(λ (f) ((λ (g) (f (λ (x) ((g g) x)))) (λ (g) (f (λ (x) ((g g) x))))))"
       (to-string Y₀))
   (⊦= "(λ ((λ (1 (λ ((1 1) 0)))) (λ (1 (λ ((1 1) 0))))))"
       ((○ to-string expression->de-bruijn curryfy) 'Y)))

  ((test/value₊ _)
   (let* ((E ((extend E₀) `(three . 3) `(four . 4) `(+ . ,(curry₁ +))))
          (t (curryfy `((λ (p x) (p x three)) + four))))
     (⊦= 7 ((○ (value₊ E) expression->de-bruijn) t))))

  ((test/compile _)
   (⊦= "((Load three) (Load ²) (Closure ((Closure ((Position 0) (Position 1) Apply (Position 1) Apply)))) Apply Apply)"
       ((○ to-string compile expression->de-bruijn) control))
   (⊦= "((Load three) (Load ²) Enter (Closure ((Position 0) (Position&Apply 1) (Position&Apply 1))) Exit Apply)"
       ((○ to-string compile⁺ expression->de-bruijn) control₁))
   (⊦= "((Load three) (Closure ((Position 0) (Position 0) (Load *) Apply Apply)) Enter (Closure ((Position 0) (Position&Apply 1) (Position&Apply 1))) Exit Apply)"
       ((○ to-string compile⁺ expression->de-bruijn) control₂))
   (⊦= "((Closure ((Closure ((Closure ((Position 0) (Position 1) (Position&Apply 1) Apply)) (Position&Apply 1))) Enter (Closure ((Position 0) (Position 1) (Position&Apply 1) Apply)) (Position&Apply 1) Exit)))"
       ((○ to-string compile⁺ expression->de-bruijn curryfy) 'Y)))

  ((test/compiled-runs _)
   (⊦= `(81 ,'() ,'() ,(void)) (final (last (→/compiled* s₁))))
   (⊦= `(81 ,'() ,'() ,(void)) (final (last (→/compiled⁺* s₁))))
   (⊦= `(81 ,'() ,'() ,(void)) (final (last (→/compiled⁺* s₁⁺))))
   (⊦= "[((Position 0) (Load null?) Apply (Test ((Position 0) (Load cdr) Apply (Position&Apply 1) (Load add1) Apply)) (Load zero)) ([((Position 0) (Position 1) (Position&Apply 1) Apply) ([((Closure ((Position 0) (Position 1) (Position&Apply 1) Apply)) (Position&Apply 1)) ([((Closure ((Position 0) (Load null?) Apply (Test ((Position 0) (Load cdr) Apply (Position&Apply 1) (Load add1) Apply)) (Load zero)))) ()])] [((Closure ((Position 0) (Load null?) Apply (Test ((Position 0) (Load cdr) Apply (Position&Apply 1) (Load add1) Apply)) (Load zero)))) ()])])]"
       ((○ to-string car status-S last →/compiled⁺*) Y⁺)))

  ((test/Y-length _)
   (⊦= `(3 ,'() ,'() ,(void))
       (let* ((s₁ (status-init '() ((○ compile expression->de-bruijn) Y₁)))
              (s (last (→/compiled* s₁))))
         (final s)))
   (⊦= 3 ((○ car status-S last →/compiled⁺*) Y₁⁺)))

  ((test/J-operator _)
   (define E (make-E))

   (let1 (J-term '(J three))
     (⊦= "(J three)" ((○ to-string curryfy) J-term))
     (⊨/error ((○ (value E) curryfy) J-term))
     (⊦= "<[3 #<unspecified>]>"
         (let1 (t ((○ compile⁺ expression->de-bruijn curryfy) J-term))
           ((○ to-string car status-S last →/compiled⁺*) (status-init '() t)))))

   (let1 (J-term '((λ (L) (² two)) (J (λ (z) z))))
     (⊦= "((λ (L) (² two)) (J (λ (z) z)))"
         ((○ to-string curryfy) J-term))
     (⊨/error ((○ (value E) curryfy) J-term))
     (⊦= 4 (let1 (t ((○ compile⁺ expression->de-bruijn curryfy) J-term))
             ((○ car status-S last →/compiled⁺*) (status-init '() t)))))

   (let1 (J-term '((λ (L) (² (L two))) (J (λ (z) z))))
     (⊦= "((λ (L) (² (L two))) (J (λ (z) z)))"
         ((○ to-string curryfy) J-term))
     (⊨/error ((○ (value E) curryfy) J-term))
     (⊦= 2 (let1 (t ((○ compile⁺ expression->de-bruijn curryfy) J-term))
             ((○ car status-S last →/compiled⁺*) (status-init '() t)))))

   (let1 (J-term '((λ (x y) (+ ((λ (L) (² (L x)))
                                (J (λ (z) (- (² z) (² two)))))
                              y))
                   three two))
     (⊦= "(((λ (x) (λ (y) ((+ ((λ (L) (² (L x))) (J (λ (z) ((- (² z)) (² two)))))) y))) three) two)"
         ((○ to-string curryfy) J-term))
     (⊨/error ((○ (value E) curryfy) J-term))
     ; KNOWN DIVERGENCE, kept and reported: the original expected 7, that is `L` returning from
     ; the `(λ (L) ...)` redex. But `(J ...)` is the operand of that redex, so `Jump` runs in the
     ; body of `(λ (y) ...)` *before* `Enter`, and the dump it captures is the one of the
     ; `(λ (y) ...)` call; the machine, ported clause by clause, thus returns `(L x)` = 9 - 4 = 5
     ; from the whole application, skipping `(+ _ y)`. This is a bug in the original
     ; *expectation*, not in the port: the original on-scheme sources (commons, continuations,
     ; SECD-machine, built unmodified but for three CHICKEN 6 shims that do not touch the
     ; machine: dropping the unused `test` import, importing the string ports from (scheme base)
     ; and a `define-record-printer` macro over `set-record-printer!`) answer 5 on this very
     ; term, and 4, 2 and 5 on the neighbouring J-terms, so the original `(test 7 ...)` at
     ; tests/SECD-machine-test.scm:231 was failing there too. It counts as 1 of the 49 original
     ; assertions altered (7 -> 5), not as passing. The original assertion, verbatim:
     #;(⊦= 7 (let1 (t ((○ compile⁺ expression->de-bruijn curryfy) J-term))
               ((○ car status-S last →/compiled⁺*) (status-init '() t))))
     (let1 (t ((○ compile⁺ expression->de-bruijn curryfy) J-term))
       (⊦= 5 ((○ car status-S last →/compiled⁺*) (status-init '() t)))))

   (let1 (J-term '((λ (x y L) (+ (² (L x)) y))
                   three two (J (λ (z) (- (² z) (² two))))))
     (⊦= "((((λ (x) (λ (y) (λ (L) ((+ (² (L x))) y)))) three) two) (J (λ (z) ((- (² z)) (² two)))))"
         ((○ to-string curryfy) J-term))
     (⊨/error ((○ (value E) curryfy) J-term))
     (⊦= 5 (let1 (t ((○ compile⁺ expression->de-bruijn curryfy) J-term))
             ((○ car status-S last →/compiled⁺*) (status-init '() t)))))

   (let1 (J-term '((λ (L x y) (+ (² (L x)) y))
                   (J (λ (z) (- (² z) (² two)))) three two))
     (⊨/error ((○ (value E) curryfy) J-term))))

  ; new: the `curryfy` clauses that the matchable version got from ellipses, and its errors.
  ((test/curryfy-clauses _)
   (⊦= "(((f a) b) c)" ((○ to-string curryfy) '(f a b c)))
   (⊦= "(λ (x) (λ (y) (λ (z) x)))" ((○ to-string curryfy) '(λ (x y z) x)))
   (⊦= "(if p a b)" ((○ to-string curryfy) '(cond (p a) (else b))))
   (⊨/error (curryfy '(cond (p a))))
   (⊨/error (curryfy '(f)))
   (⊨/error (curryfy '()))
   (⊨/error (curryfy 3))
   (⊨/error (Lambda "x" (Id 'y)))
   (⊨/error ((value₊ E₀) (J₊))))

  )

(unittest/✓ secd-machine-suite)
