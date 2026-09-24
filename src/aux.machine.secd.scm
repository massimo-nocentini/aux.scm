
; (aux machine secd): Landin's SECD machine, with its interpreted, compiled and optimised
; flavours and the J operator (see Danvy, "A rational deconstruction of Landin's SECD machine",
; and Danvy and Millikin, "A rational deconstruction of Landin's J operator").
;
; - `expression`s are `Id`, `Lambda`, `Comb`, `If` and `J`; `curryfy` desugars an s-expression
;   into them: it expands `Y` into a Y-combinator term, rewrites `cond` into nested `If`s,
;   curries multi-argument `λ`s and turns n-ary applications into left-nested `Comb`s.
; - `value` is a direct evaluator; it does not implement `J` and raises an error on it.
; - `status` is the S E C D record; `→/interpreted` is the one-step SECD transition over
;   expressions, whose dump bottom is `(void)`.
; - `expression->de-bruijn` gives `de-bruijn` terms (`Id₋` free, `Id₊` bound by index, `Lambda₊`,
;   `Comb₊`, `If₊`, `J₊`); `value₊` evaluates them.
; - `compile` and the optimising `compile⁺` (Enter/Exit for a direct redex, Position&Apply for a
;   variable in operator position) give `instruction`s run by `→/compiled` and `→/compiled⁺`;
;   the latter implements J: `Jump` pushes a `status-appender` of the dump, applying it builds a
;   `program-closure`.
;
; Every transition answers the *same* status object at the end, the fixed point that `rtc` in
; (aux commons) detects with `eq?`.
;
; The names here (Id, Comb, Load, Apply, curryfy, value, compile, status, ...) are also
; exported by (aux machine sc): never import both unprefixed.
;
; Each variant of the old `datatype`s is a record under a `%`-prefixed name (`%Id`, `%Lambda`, ...)
; paired with a checking constructor procedure named after the variant (`Id`, `Lambda`, ...), because
; rebinding the record name would break `set-record-printer!`. Like every aux module this one
; exports `*`, so the raw record API (`make-%Id`, `%Id-identifier-set!`, ...) and `%check` are
; exported too, and they are also shared with (aux machine sc): they are private by
; convention only, and client code should use the variant constructors, which check the
; field types, and never build or mutate the `%` records directly.

(module (aux machine secd) *

  (import scheme
          (chicken base)
          (chicken format)
          (chicken string)
          (only srfi-1 list-index last drop-right)
          (aux base)
          (aux commons)
          (aux machine env))

  (define %check
    (λ (who pred? v)
      (cond
        ((pred? v) v)
        (else (error who "wrong field type" v)))))

  ; expressions -------------------------------------------------------------------------------------

  (define-record %Id identifier)
  (define-record %Lambda var body)
  (define-record %Comb rator rand)
  (define-record %If question answer otherwise)
  (define-record %J)

  (define expression?
    (λ (e) (or (%Id? e) (%Lambda? e) (%Comb? e) (%If? e) (%J? e))))

  (define Id (λ (identifier) (make-%Id (%check 'Id symbol? identifier))))
  (define Lambda (λ (var body) (make-%Lambda (%check 'Lambda symbol? var)
                                             (%check 'Lambda expression? body))))
  (define Comb (λ (rator rand) (make-%Comb (%check 'Comb expression? rator)
                                           (%check 'Comb expression? rand))))
  (define If (λ (q a o) (make-%If (%check 'If expression? q)
                                  (%check 'If expression? a)
                                  (%check 'If expression? o))))
  (define J make-%J)

  (define-many (Id? Lambda? Comb? If? J?) (%Id? %Lambda? %Comb? %If? %J?))

  (set-record-printer! %Id (λ (e out) (format out "~a" (%Id-identifier e))))
  (set-record-printer! %Lambda (λ (e out) (format out "(λ (~a) ~a)" (%Lambda-var e) (%Lambda-body e))))
  (set-record-printer! %Comb (λ (e out) (format out "(~a ~a)" (%Comb-rator e) (%Comb-rand e))))
  (set-record-printer! %If (λ (e out) (format out "(if ~a ~a ~a)"
                                        (%If-question e) (%If-answer e) (%If-otherwise e))))
  (set-record-printer! %J (λ (e out) (format out "J")))

  (define curryfy
    (λ (sexp)
      (let ((is-Y? (=to? 'Y))
            (is-J? (=to? 'J)))
        (cond
          ((is-J? sexp) (J))
          ((is-Y? sexp) (let* ((f (gensym))
                               (h `(λ (g) (,f (λ (x) ((g g) x)))))
                               (Y `(λ (,f) (,h ,h))))
                          (curryfy Y)))
          ((symbol? sexp) (Id sexp))
          ((list? sexp) (match/first sexp
                          ((cond (,question ,answer) (else ,otherwise))
                           (If (curryfy question)
                               (curryfy answer)
                               (curryfy otherwise)))
                          ((cond (,question ,answer) . ,clauses)
                           (If (curryfy question)
                               (curryfy answer)
                               (curryfy `(cond ,@clauses))))
                          ((λ (,x) ,body) (Lambda x (curryfy body)))
                          (((λ (,x . ,ys) ,body) ⊣ (pair? ys)) (Lambda x (curryfy `(λ ,ys ,body))))
                          ((,rator ,rand) (Comb (curryfy rator) (curryfy rand)))
                          (((,rator ,rand . ,rands) ⊣ (pair? rands))
                           (Comb (curryfy (drop-right sexp 1)) (curryfy (last sexp))))
                          (else (error "match error for" sexp))))
          (else (error "cond error for:" sexp))))))

  (define value
    (λ (E)
      (λ (e)
        (letrec ((V (λ (e)
                      (cond
                        ((%Id? e) (E (%Id-identifier e)))
                        ((%Lambda? e) (let ((var (%Lambda-var e)) (body (%Lambda-body e)))
                                        (λ (x)
                                          (let1 (E₁ ((extend E) `(,var . ,x)))
                                            ((value E₁) body)))))
                        ((%Comb? e) ((V (%Comb-rator e)) (V (%Comb-rand e))))
                        ((%If? e) (if (V (%If-question e)) (V (%If-answer e)) (V (%If-otherwise e))))
                        ((%J? e) (error "((value E) J) not implemented"))
                        (else (error 'value "not an expression" e))))))
          (V e)))))

  ; status ------------------------------------------------------------------------------------------

  (define-record status S E C D)

  (define status-init
    (λ (E C)
      (make-status '() E C (void))))

  (define dbind/status
    (λ (recv)
      (λ (s)
        (recv s (status-S s) (status-E s) (status-C s) (status-D s)))))

  (define status-printer
    (λ (indent)
      (dbind/status
        (λ (s S E C D)
          (let* ((indents (make-string indent #\space))
                 (L (fmap (λ (e)
                            (string-append
                              (make-string (+ indent 4) #\space)
                              (to-string e)))))
                 (P (λ (sym l #!key (indents indents))
                      (match/first l
                        (() (format #f "~a(~a ~a)" indents sym '()))
                        ((,l₀) (format #f "~a(~a (~a))" indents sym l₀))
                        ((,l₀ . ,l₊) (let1 (rest (string-intersperse (L l₊) "\n"))
                                       (format #f "~a(~a (~a\n~a))" indents sym l₀ rest))))))
                 (Ss (P "S" S indents: ""))
                 (Es (P "E" (if (list? E) E (E->alist E))))
                 (Cs (P "C" C))
                 (Ds (cond/λ D
                       (void? (K (format #f "~a(D ~a)" indents D)))
                       (else (K (format #f "~a(~a)"
                                  indents ((status-printer (add1 indent)) D)))))))
            (format #f "~a\n~a\n~a\n~a" Ss Es Cs Ds))))))

  (set-record-printer! status
    (λ (s out)
      (format out "~a" ((status-printer 0) s))))

  (define-record closure C₁ var E)

  (define dbind/closure
    (λ (recv)
      (λ (c)
        (recv c (closure-C₁ c) (closure-var c) (closure-E c)))))

  (set-record-printer! closure
    (λ (c out)
      (let1 (P (dbind/closure (λ (_ C₁ var E)
                                (format out "[~a ~a ~a]" C₁ var E))))
        (P c))))

  (define →/interpreted
    (let* ((sym/apply (gensym 'apply))
           (is-apply? (=to? sym/apply)))
      (dbind/status
        (λ (s S E C D)
          (cond
            ((and (null? C) (void? D)) s) ; termination condition for fixed-point
            ((null? C)
             (let1 (extend-dump (dbind/status
                                  (λ (_ S₁ E₁ C₁ D₁)
                                    (let1 (S₂ (cons (car S) S₁))
                                      (make-status S₂ E₁ C₁ D₁)))))
               (extend-dump D)))
            (else (match1/first ((,C₀ . ,C₊) C)
                    (cond
                      ((is-apply? C₀) (match1/first ((,f ,y . ,S₊) S)
                                        (cond/λ f
                                          (closure? (dbind/closure
                                                      (λ (_ C₁ j E₁)
                                                        (let ((S₂ '())
                                                              (E₂ ((extend E₁) `(,j . ,y)))
                                                              (C₂ (list C₁))
                                                              (D₂ (make-status S₊ E C₊ D)))
                                                          (make-status S₂ E₂ C₂ D₂)))))
                                          (else (K (make-status (cons (f y) S₊) E C₊ D))))))
                      ((%Id? C₀) (let1 (S₁ (cons (E (%Id-identifier C₀)) S))
                                   (make-status S₁ E C₊ D)))
                      ((%Lambda? C₀) (let1 (c (make-closure (%Lambda-body C₀) (%Lambda-var C₀) E))
                                       (make-status (cons c S) E C₊ D)))
                      ((%Comb? C₀) (let* ((cmds (list (%Comb-rand C₀) (%Comb-rator C₀) sym/apply))
                                          (C₁ (append cmds C₊)))
                                     (make-status S E C₁ D)))
                      ; as with the old `cases`, the machine has no rule for `If` and `J`.
                      (else (error '→/interpreted "no transition for" C₀))))))))))

  ; de Bruijn terms ---------------------------------------------------------------------------------

  (define-record %Id₋ id)
  (define-record %Id₊ index)
  (define-record %Lambda₊ body)
  (define-record %Comb₊ rator rand)
  (define-record %If₊ question answer otherwise)
  (define-record %J₊)

  (define de-bruijn?
    (λ (e) (or (%Id₋? e) (%Id₊? e) (%Lambda₊? e) (%Comb₊? e) (%If₊? e) (%J₊? e))))

  (define Id₋ (λ (id) (make-%Id₋ (%check 'Id₋ symbol? id))))
  (define Id₊ (λ (index) (make-%Id₊ (%check 'Id₊ number? index))))
  (define Lambda₊ (λ (body) (make-%Lambda₊ (%check 'Lambda₊ de-bruijn? body))))
  (define Comb₊ (λ (rator rand) (make-%Comb₊ (%check 'Comb₊ de-bruijn? rator)
                                             (%check 'Comb₊ de-bruijn? rand))))
  (define If₊ (λ (q a o) (make-%If₊ (%check 'If₊ de-bruijn? q)
                                    (%check 'If₊ de-bruijn? a)
                                    (%check 'If₊ de-bruijn? o))))
  (define J₊ make-%J₊)

  (define-many (Id₋? Id₊? Lambda₊? Comb₊? If₊? J₊?) (%Id₋? %Id₊? %Lambda₊? %Comb₊? %If₊? %J₊?))

  (set-record-printer! %Id₋ (λ (e out) (format out "~a" (%Id₋-id e))))
  (set-record-printer! %Id₊ (λ (e out) (format out "~a" (%Id₊-index e))))
  (set-record-printer! %Lambda₊ (λ (e out) (format out "(λ ~a)" (%Lambda₊-body e))))
  (set-record-printer! %Comb₊ (λ (e out) (format out "(~a ~a)" (%Comb₊-rator e) (%Comb₊-rand e))))
  (set-record-printer! %If₊ (λ (e out) (format out "(if ~a ~a ~a)"
                                         (%If₊-question e) (%If₊-answer e) (%If₊-otherwise e))))
  (set-record-printer! %J₊ (λ (e out) (format out "J")))

  (define expression->de-bruijn
    (λ (e)
      (letrec ((deB (λ (e F)
                      (let1 (position (λ (v) (list-index (=to? v) F)))
                        (cond
                          ((%Id? e) (let1 (i (%Id-identifier e))
                                      (cond/λ (position i)
                                        (number? (λ (p) (Id₊ p)))
                                        (else (K (Id₋ i))))))
                          ((%Lambda? e) (Lambda₊ (deB (%Lambda-body e) (cons (%Lambda-var e) F))))
                          ((%Comb? e) (Comb₊ (deB (%Comb-rator e) F) (deB (%Comb-rand e) F)))
                          ((%If? e) (If₊ (deB (%If-question e) F)
                                         (deB (%If-answer e) F)
                                         (deB (%If-otherwise e) F)))
                          ((%J? e) (J₊))
                          (else (error 'expression->de-bruijn "not an expression" e)))))))
        (deB e '()))))

  (define value₊
    (λ (E)
      (λ (e)
        (letrec ((deB (λ (e F)
                        (cond
                          ((%Id₋? e) (E (%Id₋-id e)))
                          ((%Id₊? e) (list-ref F (%Id₊-index e)))
                          ((%Lambda₊? e) (λ (y) (deB (%Lambda₊-body e) (cons y F))))
                          ((%Comb₊? e) ((deB (%Comb₊-rator e) F) (deB (%Comb₊-rand e) F)))
                          ((%If₊? e) (if (deB (%If₊-question e) F)
                                       (deB (%If₊-answer e) F)
                                       (deB (%If₊-otherwise e) F)))
                          ((%J₊? e) (error "((value₊ E) J) not implemented"))
                          (else (error 'value₊ "not a de Bruijn term" e))))))
          (deB e '())))))

  ; instructions ------------------------------------------------------------------------------------

  (define-record %Load selector)
  (define-record %Apply)
  (define-record %Position index)
  (define-record %Position&Apply index)
  (define-record %Closure control) ; [instruction], precisely.
  (define-record %Enter)
  (define-record %Exit)
  (define-record %Test control) ; [instruction], precisely.
  (define-record %Jump)

  (define instruction?
    (λ (i) (or (%Load? i) (%Apply? i) (%Position? i) (%Position&Apply? i) (%Closure? i)
               (%Enter? i) (%Exit? i) (%Test? i) (%Jump? i))))

  (define Load (λ (selector) (make-%Load (%check 'Load procedure? selector))))
  (define Apply make-%Apply)
  (define Position (λ (index) (make-%Position (%check 'Position number? index))))
  (define Position&Apply (λ (index) (make-%Position&Apply (%check 'Position&Apply number? index))))
  (define Closure (λ (control) (make-%Closure (%check 'Closure list? control))))
  (define Enter make-%Enter)
  (define Exit make-%Exit)
  (define Test (λ (control) (make-%Test (%check 'Test list? control))))
  (define Jump make-%Jump)

  (define-many (Load? Apply? Position? Position&Apply? Closure? Enter? Exit? Test? Jump?)
               (%Load? %Apply? %Position? %Position&Apply? %Closure? %Enter? %Exit? %Test? %Jump?))

  (set-record-printer! %Load (λ (i out) (format out "(Load ~a)" ((%Load-selector i) identity))))
  (set-record-printer! %Apply (λ (i out) (format out "Apply")))
  (set-record-printer! %Enter (λ (i out) (format out "Enter")))
  (set-record-printer! %Exit (λ (i out) (format out "Exit")))
  (set-record-printer! %Test (λ (i out) (format out "(Test ~a)" (%Test-control i))))
  (set-record-printer! %Position (λ (i out) (format out "(Position ~a)" (%Position-index i))))
  (set-record-printer! %Position&Apply (λ (i out) (format out "(Position&Apply ~a)" (%Position&Apply-index i))))
  (set-record-printer! %Closure (λ (i out) (format out "(Closure ~a)" (%Closure-control i))))
  (set-record-printer! %Jump (λ (i out) (format out "Jump")))

  (define compile
    (rec C (λ (e)
             (cond
               ((%Id₋? e) (list (Load ($ (%Id₋-id e)))))
               ((%Id₊? e) (list (Position (%Id₊-index e))))
               ((%Lambda₊? e) (list (Closure (C (%Lambda₊-body e)))))
               ((%Comb₊? e) `(,@(C (%Comb₊-rand e)) ,@(C (%Comb₊-rator e)) ,(Apply)))
               ((%If₊? e) `(,@(C (%If₊-question e)) ,(Test (C (%If₊-otherwise e))) ,@(C (%If₊-answer e))))
               ((%J₊? e) (list (Jump)))
               (else (error 'compile "not a de Bruijn term" e))))))

  (define-record closure₊ C E)

  (define dbind/closure₊
    (λ (recv)
      (λ (c)
        (recv c (closure₊-C c) (closure₊-E c)))))

  (set-record-printer! closure₊
    (λ (c out)
      (format out "[~a ~a]" (closure₊-C c) (closure₊-E c))))

  (define-record program-closure body D)

  (define dbind/program-closure
    (λ (recv)
      (λ (pc)
        (recv pc (program-closure-body pc) (program-closure-D pc)))))

  (set-record-printer! program-closure
    (λ (pc out)
      (format out "<[~a ~a]>" (program-closure-body pc) (program-closure-D pc))))

  (define →/compiled
    (λ (E₀)
      (dbind/status
        (λ (s S E C D)
          (cond
            ((and (null? C) (void? D)) s) ; termination condition for fixed-point
            ((null? C)
             (let1 (extend-dump (dbind/status
                                  (λ (_ S₁ E₁ C₁ D₁)
                                    (let1 (S₂ (cons (car S) S₁))
                                      (make-status S₂ E₁ C₁ D₁)))))
               (extend-dump D)))
            (else (match1/first ((,C₀ . ,C₊) C)
                    (cond
                      ((%Load? C₀) (let1 (S₁ (cons ((%Load-selector C₀) E₀) S))
                                     (make-status S₁ E C₊ D)))
                      ((%Position? C₀) (let1 (S₁ (cons (list-ref E (%Position-index C₀)) S))
                                         (make-status S₁ E C₊ D)))
                      ((%Closure? C₀) (let1 (c₊ (make-closure₊ (%Closure-control C₀) E))
                                        (make-status (cons c₊ S) E C₊ D)))
                      ((%Test? C₀) (match1/first ((,boolean . ,S₊) S)
                                     (cond
                                       (boolean (make-status S₊ E C₊ D))
                                       (else (make-status S₊ E (%Test-control C₀) D)))))
                      ((%Apply? C₀) (match1/first ((,f ,y . ,S₊) S)
                                      (cond/λ f
                                        (closure₊? (dbind/closure₊
                                                     (λ (_ C₁ E₁)
                                                       (let ((S₂ '())
                                                             (E₂ (cons y E₁))
                                                             (C₂ (identity C₁))
                                                             (D₂ (make-status S₊ E C₊ D)))
                                                         (make-status S₂ E₂ C₂ D₂)))))
                                        (else (K (make-status (cons (f y) S₊) E C₊ D))))))
                      ; as with the old `cases`, this machine has no rule for the other
                      ; instructions, which only `compile⁺` emits.
                      (else (error '→/compiled "no transition for" C₀))))))))))

  (define compile⁺
    (rec C (λ (e)
             (cond
               ((%Id₋? e) (list (Load ($ (%Id₋-id e)))))
               ((%Id₊? e) (list (Position (%Id₊-index e))))
               ((%Lambda₊? e) (list (Closure (C (%Lambda₊-body e)))))
               ((%Comb₊? e) (let ((rator (%Comb₊-rator e)) (rand (%Comb₊-rand e)))
                              (cond
                                ((%Lambda₊? rator) `(,@(C rand) ,(Enter) ,@(C (%Lambda₊-body rator)) ,(Exit)))
                                ((%Id₊? rator) `(,@(C rand) ,(Position&Apply (%Id₊-index rator))))
                                (else `(,@(C rand) ,@(C rator) ,(Apply))))))
               ((%If₊? e) `(,@(C (%If₊-question e)) ,(Test (C (%If₊-otherwise e))) ,@(C (%If₊-answer e))))
               ((%J₊? e) (list (Jump)))
               (else (error 'compile⁺ "not a de Bruijn term" e))))))

  (define-record status-appender status)

  (define dbind/status-appender
    (λ (recv)
      (λ (sa)
        (recv sa (status-appender-status sa)))))

  (set-record-printer! status-appender
    (λ (sa out)
      (format out "<|~a|>" (status-appender-status sa))))

  (define →/compiled⁺
    (let ((PA (λ (f y S E C D)
                (cond/λ f
                  (closure₊? (dbind/closure₊
                               (λ (_ C₁ E₁)
                                 (let ((S₂ '())
                                       (E₂ (cons y E₁))
                                       (C₂ (identity C₁))
                                       (D₂ (make-status S E C D)))
                                   (make-status S₂ E₂ C₂ D₂)))))
                  (program-closure? (dbind/program-closure
                                      (λ (_ body D₀)
                                        (cond/λ D₀
                                          (void? (K (let ((S₁ `(,body ,y . ,S))
                                                          (C₁ (cons (Apply) '())))
                                                      (make-status S₁ E C₁ D))))
                                          (status? (dbind/status
                                                     (λ (_ S₁ E₁ C₁ D₁)
                                                       (let ((S₂ `(,body ,y . ,S₁))
                                                             (E₂ (identity E₁))
                                                             (C₂ (cons (Apply) C₁))
                                                             (D₂ (identity D₁)))
                                                         (make-status S₂ E₂ C₂ D₂)))))
                                          (else error)))))
                  (status-appender? (dbind/status-appender
                                      (λ (_ D₀)
                                        (let1 (pc (make-program-closure y D₀))
                                          (make-status (cons pc S) E C D)))))
                  (else (K (make-status (cons (f y) S) E C D)))))))
      (λ (E₀)
        (dbind/status
          (λ (s S E C D)
            (cond
              ((and (null? C) (void? D)) s) ; termination condition for fixed-point
              ((null? C)
               (let1 (extend-dump (dbind/status
                                    (λ (_ S₁ E₁ C₁ D₁)
                                      (let1 (S₂ (cons (car S) S₁))
                                        (make-status S₂ E₁ C₁ D₁)))))
                 (extend-dump D)))
              (else (match1/first ((,C₀ . ,C₊) C)
                      (cond
                        ((%Load? C₀) (let1 (S₁ (cons ((%Load-selector C₀) E₀) S))
                                       (make-status S₁ E C₊ D)))
                        ((%Position? C₀) (let1 (S₁ (cons (list-ref E (%Position-index C₀)) S))
                                           (make-status S₁ E C₊ D)))
                        ((%Closure? C₀) (let1 (c₊ (make-closure₊ (%Closure-control C₀) E))
                                          (make-status (cons c₊ S) E C₊ D)))
                        ((%Enter? C₀) (match1/first ((,s₀ . ,S₊) S)
                                        (let1 (D₁ (make-status S₊ '() '() D))
                                          (make-status '() (cons s₀ E) C₊ D₁))))
                        ((%Exit? C₀) (let ((S₁ (cons (car S) (status-S D)))
                                           (E₁ (cdr E))
                                           (C₁ C₊)
                                           (D₁ (status-D D)))
                                       (make-status S₁ E₁ C₁ D₁)))
                        ((%Test? C₀) (match1/first ((,boolean . ,S₊) S)
                                       (cond
                                         (boolean (make-status S₊ E C₊ D))
                                         (else (make-status S₊ E (%Test-control C₀) D)))))
                        ((%Position&Apply? C₀) (let1 (f (list-ref E (%Position&Apply-index C₀)))
                                                 (match1/first ((,y . ,S₊) S)
                                                   (PA f y S₊ E C₊ D))))
                        ((%Apply? C₀) (match1/first ((,f ,y . ,S₊) S)
                                        (PA f y S₊ E C₊ D)))
                        ((%Jump? C₀) (let1 (S₁ (cons (make-status-appender D) S))
                                       (make-status S₁ E C₊ D)))
                        (else (error '→/compiled⁺ "not an instruction" C₀)))))))))))

  )
