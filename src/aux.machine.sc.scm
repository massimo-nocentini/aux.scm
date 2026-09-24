
; (aux machine sc): the SC machine, a stack and control machine for curried combinations
; (Landin; see also Danvy's "A rational deconstruction of Landin's SECD machine").
;
; A `combination` is either `(Id identifier)` or `(Comb rator rand)`; `curryfy` turns an
; s-expression such as `'(p a b)` into the left-nested `((p a) b)`. The machine comes in two
; flavours over the same `status` record (a stack S and a control list C):
; `→/interpreted` runs combinations directly, while `→/compiled` runs the `instruction`s
; that `compile` produces. Both are one-step transitions that answer the *same* status object
; when C is empty, which is the fixed point that `rtc` in (aux commons) detects with `eq?`.
;
; The names here (Id, Comb, Load, Apply, curryfy, value, compile, status, ...) are also
; exported by (aux machine secd): never import both unprefixed.
;
; Each variant of the old `datatype`s is a record under a `%`-prefixed name (`%Id`, `%Comb`, ...)
; paired with a checking constructor procedure named after the variant (`Id`, `Comb`, ...), because
; rebinding the record name would break `set-record-printer!`. Like every aux module this one
; exports `*`, so the raw record API (`make-%Id`, `%Id-identifier-set!`, ...) and `%check` are
; exported too, and they are also shared with (aux machine secd): they are private by
; convention only, and client code should use the variant constructors, which check the
; field types, and never build or mutate the `%` records directly.

(module (aux machine sc) *

  (import scheme
          (chicken base)
          (chicken format)
          (aux base)
          (aux commons)
          (aux machine env))

  (define %check
    (λ (who pred? v)
      (cond
        ((pred? v) v)
        (else (error who "wrong field type" v)))))

  ; combinations -----------------------------------------------------------------------------------

  (define-record %Id identifier)
  (define-record %Comb rator rand)

  (define Id (λ (identifier) (make-%Id (%check 'Id symbol? identifier))))
  (define Id? %Id?)
  (define Id-identifier %Id-identifier)

  (define combination? (λ (c) (or (%Id? c) (%Comb? c))))

  (define Comb (λ (rator rand) (make-%Comb (%check 'Comb combination? rator)
                                           (%check 'Comb combination? rand))))
  (define Comb? %Comb?)
  (define Comb-rator %Comb-rator)
  (define Comb-rand %Comb-rand)

  (set-record-printer! %Id (λ (c out) (format out "~a" (%Id-identifier c))))
  (set-record-printer! %Comb (λ (c out) (format out "(~a ~a)" (%Comb-rator c) (%Comb-rand c))))

  (define curryfy (foldl1
                    (λ (acc c)
                      (cond
                        ((list? c) (Comb acc (curryfy c)))
                        (else (Comb acc (Id c)))))
                    H₀: (λ (i) (Id i))))

  (define value
    (λ (E)
      (rec V (λ (c)
               (cond
                 ((%Id? c) (E (%Id-identifier c)))
                 ((%Comb? c) ((V (%Comb-rator c)) (V (%Comb-rand c))))
                 (else (error 'value "not a combination" c)))))))

  ; status ------------------------------------------------------------------------------------------

  (define-record status S C)

  (define dbind/status
    (λ (recv)
      (λ (s)
        (recv s (status-S s) (status-C s)))))

  (set-record-printer! status
    (λ (s out)
      (let1 (P (dbind/status (λ (s S C)
                               (format out "(~a ~a)" S C))))
        (P s))))

  (define →/interpreted
    (let* ((sym/apply (gensym 'apply))
           (is-apply? (=to? sym/apply)))
      (λ (E)
        (dbind/status
          (λ (s S C)
            (cond
              ((null? C) s) ; fixed-point termination condition
              (else (match1/first ((,C₀ . ,C₊) C)
                      (cond
                        ((is-apply? C₀) (match1/first ((,f ,y . ,S₊) S)
                                          (make-status (cons (f y) S₊) C₊)))
                        ((%Id? C₀) (let1 (stack (cons (E (%Id-identifier C₀)) S))
                                     (make-status stack C₊)))
                        ((%Comb? C₀) (let* ((cmds (list (%Comb-rand C₀) (%Comb-rator C₀) sym/apply))
                                            (control (append cmds C₊)))
                                       (make-status S control)))
                        (else (error '→/interpreted "not a combination" C₀)))))))))))

  ; instructions ------------------------------------------------------------------------------------

  (define-record %Load selector)
  (define-record %Apply)

  (define Load (λ (selector) (make-%Load (%check 'Load procedure? selector))))
  (define Load? %Load?)
  (define Load-selector %Load-selector)
  (define Apply make-%Apply)
  (define Apply? %Apply?)

  (define instruction? (λ (i) (or (%Load? i) (%Apply? i))))

  (set-record-printer! %Load (λ (i out) (format out "(Load ~a)" ((%Load-selector i) (gensym)))))
  (set-record-printer! %Apply (λ (i out) (format out "Apply")))

  (define compile
    (λ (E)
      (rec C (λ (c)
               (cond
                 ((%Id? c) (list (Load (K (E (%Id-identifier c))))))
                 ((%Comb? c) (append (C (%Comb-rand c)) (C (%Comb-rator c)) (list (Apply))))
                 (else (error 'compile "not a combination" c)))))))

  (define →/compiled
    (λ (E)
      (dbind/status
        (λ (s S C)
          (cond
            ((null? C) s)
            (else (letcar&cdr (((C₀ C₊) C))
                    (cond
                      ((%Load? C₀) (let1 (stack (cons ((%Load-selector C₀) E) S))
                                     (make-status stack C₊)))
                      ((%Apply? C₀) (match1/first ((,f ,y . ,S₊) S)
                                      (make-status (cons (f y) S₊) C₊)))
                      (else (error '→/compiled "not an instruction" C₀))))))))))

  )
