; (aux kanren unionfind): a small miniKanren whose substitution is a *union-find*, ported from
; the old `on-scheme` repository (microkanren.scm), together with (aux kanren unionfind reasoned),
; the relations of "The Reasoned Schemer" written on top of it (reasoned-schemer.scm).
;
; It is a teaching implementation distinct from (aux kanren micro), not a variant of it:
;   - a state is a `status` record holding a union-find `≡` (from (aux fds unionfind)) and a `depth`,
;     the number of disjunctions taken so far, which `deepening` uses as a filter (not a bound);
;   - goals are `λ§` abstractions (lambda + `δ!`) returning (aux stream sicp) streams, so the order of
;     the answers is the one fixed by `stream:>>=` and `stream:§` (interleaving) or `stream:append`;
;   - `run` returns a stream of `(term depth)` lists, `run/with-symbols` a list of plain symbols
;     where reified variables read as ▢₀, ▢₁, …;
;   - four if-then-else flavours: `if°/¦` (plain append, unfair), `if°/§` (interleaving, the usual
;     conde), `if°/!` (soft cut, conda) and `if°/!!` (first answer only, condu), each with its `cond°/X`.
;
; Differences from the original:
;   - the º suffix (U+00BA) becomes ° (U+00B0), as everywhere in aux;
;   - `Λ` becomes `λ§` (the (aux base) `Λ` is a matching lambda);
;   - the `variable` datatype becomes two records, `variable/working` (made by `V`) and
;     `variable/reified` (made by `R`), sharing one printer; `V` and `R` keep the field checks of
;     the datatype (`symbol?`, and `number?` for the index of `R`) and signal an error otherwise;
;   - the union-find is keyed on these variable *records*, not on their bare symbols (a deliberate
;     departure from the port brief): srfi-69 `equal?-hash` hashes records by their slots, hence by
;     the variable's symbol, while bare-symbol keys would make a variable and a user term that is
;     the same symbol indistinguishable (`(≡ q 'a)` would alias q with a variable named `a`);
;   - `unify` binds a variable to a non-variable term *directly* and uses union by rank only
;     between two variables: the original used union by rank for both, so after `(≡ v w)` the root
;     had rank 1 and a later `(≡ v 3)` made the constant the child, that is `(∧ (≡ v w) (≡ v 3))`
;     reified to ((▢₀ ▢₀)) and `(∧ (≡ v w) (≡ v 3) (≡ v 4))` succeeded;
;   - API change: the exported `unify` takes the union-find itself as a first argument,
;     `((unify U ∪ ↑) v w)` instead of the original `((unify ∪ ↑) v w)`, because binding a variable
;     directly to a term (the fix above) writes into the table of `U`, which `∪` and `↑` do not expose;
;   - `unify` compares strings with `string=?` (the original used only `eqv?`);
;   - in `run/with-symbols` the count is an *expression*: `+inf.0` (aux `∞`) means all answers,
;     any other number `n` the first `n` of them (the original matched `∞` as a syntax literal,
;     which fails as soon as `∞` is bound, as (aux base) does); `#t` still asks for the first
;     answer only (or #f) and `↓` still keeps the `(term depth)` pairs;
;   - the typo `condº/∞` in the pattern of `condº/¦` is fixed.
;
; Limits kept from the original: no occurs check (`(≡ v (list 1 v))` makes the reification loop),
; vectors and records are not decomposed, and every `≡` copies the whole union-find.

(module (aux kanren unionfind)
  ( ; states
    make-status status? status-≡ status-depth status-copy ε
    ; variables
    variable? V R V/gensym variable->symbol
    variable/working? variable/working-s variable/reified? variable/reified-r variable/reified-n
    ; goals -- ✓ ✗ ∧₂ fresh₁ if°/X must stay exported: the exported macros expand to them
    ✓ ✗ ∧₂ ∧ ∨ fresh₁ fresh ≡ unify
    ; reification -- V/gensym reify/var ε are expanded to by `run`
    R/unionfind-size reify/status walk* reify/var
    ; running -- get/variable->symbol and run/with-symbols/taker are expanded to by `run/with-symbols`
    run get/variable->symbol run/with-symbols/taker run/with-symbols deepening
    ; conditionals
    if°/¦ if°/§ if°/! if°/!! cond° cond°/¦ cond°/§ cond°/! cond°/!!)

  (import scheme
          (chicken base)
          (chicken port)
          (only srfi-69 hash-table-set!)
          (aux base)
          (only (aux commons) ○ symbol∼subscripts number->symbol)
          (only (aux stream sicp)
                λ§ stream:empty stream:singleton stream:>>= stream:repeat stream:map stream:append
                stream:§ stream:null? stream:car stream:dest/car+cdr stream:->list list○take
                stream:iterative-deepening)
          (aux fds unionfind))

  ; states -----------------------------------------------------------------------------------------

  (define-record status ≡ depth)

  (set-record-printer! status
    (λ (s out)
      (display (status-≡ s) out)
      (display ((○ number->string status-depth) s) out)))

  (define status-copy
    (λ (s)
      (make-status
        ((○ unionfind-copy status-≡) s)
        ((○ identity status-depth) s))))

  (define ε
    (λ ()
      (make-status (unionfind-empty) 0)))

  ; variables --------------------------------------------------------------------------------------

  (define-record variable/working s)    ; for *working* logic variables
  (define-record variable/reified r n)  ; for *reified* logic variables

  ; checking constructors: the original `(define-datatype variable variable? (V (s symbol?))
  ; (R (r symbol?) (n number?)))` rejected ill-typed fields, so do these ones (as in (aux variable)).
  (define V
    (λ (s)
      (if (symbol? s)
        (make-variable/working s)
        (error 'V "not a symbol" s))))

  (define R
    (λ (r n)
      (cond
        ((not (symbol? r)) (error 'R "not a symbol" r))
        ((not (number? n)) (error 'R "not a number" n))
        (else (make-variable/reified r n)))))

  (define variable?
    (λ (v)
      (or (variable/working? v) (variable/reified? v))))

  (define variable-printer
    (λ (v out)
      (let1 (subscripts&display (○ (display/port out) symbol∼subscripts))
        (cond
          ((variable/working? v) (subscripts&display (variable/working-s v)))
          (else (subscripts&display (symbol-append
                                      (variable/reified-r v)
                                      (number->symbol (variable/reified-n v)))))))))

  (set-record-printer! variable/working variable-printer)
  (set-record-printer! variable/reified variable-printer)

  (define variable->symbol
    (λ (v)
      (cond
        ((variable? v) (string->symbol
                         (with-output-to-string (λ () (display v)))))
        ((pair? v) (cons
                     (variable->symbol (car v))
                     (variable->symbol (cdr v))))
        (else v))))

  ; goals ------------------------------------------------------------------------------------------

  (define ✓ stream:singleton)
  (define ✗ (λ§ (s) stream:empty))

  (define ∧₂
    (λ (g₁ g₂)
      (λ§ (s)
        (stream:>>= (g₁ s) (stream:repeat g₂)))))

  (define-syntax ∧
    (syntax-rules ()
      ((∧) ✓)
      ((∧ g) g)
      ((∧ g₀ g ...) (∧₂ g₀ (∧ g ...)))))

  (define ∨
    (λ (m+)
      (λ goals
        (λ§ (s)
          (let1 (r (make-status (status-≡ s) ((○ add1 status-depth) s)))
            (apply m+ (map (λ (g) (g r)) goals)))))))

  (define V/gensym
    (λ ()
      (V (gensym 'V))))

  (define fresh₁
    (λ (recv)
      (λ§ (s)
        ((recv (V/gensym)) s))))

  (define-syntax fresh
    (syntax-rules ()
      ((fresh () g) (λ§ (s) (g s))) ; delaying `g` application
      ((fresh (v₀ v ...) g) (fresh₁ (lambda (v₀) (fresh (v ...) g))))))

  ; reification ------------------------------------------------------------------------------------

  (define R/unionfind-size
    (λ (U)
      (R '▢ (unionfind-edges U))))

  (define reify/status
    (λ (v s)
      (let1 (U (status-≡ s))
        (unionfind-accessors U
          (λ (↑ ↑! ∪ →)
            (letrec ((R/S (λ (v)
                            (let1 (v₀ (↑ v))
                              (cond
                                ((variable? v₀) (∪ v₀ (R/unionfind-size U)))
                                ((pair? v₀) (begin
                                              (R/S (car v₀))
                                              (R/S (cdr v₀))))
                                (else 'useless))))))
              (begin (R/S v) s)))))))

  (define walk*
    (λ (↑)
      (letrec ((W* (λ (v)
                     (let1 (v₀ (↑ v))
                       (cond
                         ((variable? v₀) v₀)
                         ((pair? v₀) (cons (W* (car v₀)) (W* (cdr v₀))))
                         (else v₀))))))
        W*)))

  (define reify/var
    (λ (v)
      (λ (s)
        (let* ((v₀ ((walk* (unionfind-↑ (status-≡ s))) v))
               (s₀ (reify/status v₀ (ε))))
          (values
            ((walk* (unionfind-↑! (status-≡ s₀))) v₀)
            (status-depth s))))))

  ; unification ------------------------------------------------------------------------------------

  ; `U` is the union-find to update in place, `∪` its union by rank and `↑` its find.
  (define unify
    (λ (U ∪ ↑)
      (let1 (bind! (λ (var term) (hash-table-set! (unionfind-π U) var term)))
        (letrec ((U* (λ (v w)
                       (let ((v₀ (↑ v))
                             (w₀ (↑ w)))
                         (cond
                           ((eqv? v₀ w₀) #t)
                           ((and (variable? v₀) (variable? w₀)) (begin (∪ v₀ w₀) #t))
                           ((variable? v₀) (begin (bind! v₀ w₀) #t))
                           ((variable? w₀) (begin (bind! w₀ v₀) #t))
                           ((and (string? v₀) (string? w₀)) (string=? v₀ w₀))
                           ((and (pair? v₀) (pair? w₀))
                            (and
                              (U* (car v₀) (car w₀))
                              (U* (cdr v₀) (cdr w₀))))
                           (else #f))))))
          U*))))

  (define ≡
    (λ (v w)
      (λ§ (s)
        (let1 (S (status-copy s))
          (unionfind-accessors (status-≡ S)
            (λ (↑ ↑! ∪ →)
              (cond
                (((unify (status-≡ S) ∪ ↑) v w) (stream:singleton S))
                (else stream:empty))))))))

  ; running ----------------------------------------------------------------------------------------

  (define-syntax run
    (syntax-rules ()
      ((run taut) (run (q) (∧ taut (≡ q #t))))
      ((run (q) g) (run q g))
      ((run (v₀ v ...) g) (run q (fresh (v₀ v ...)
                                   (∧ g (≡ q (list v₀ v ...))))))
      ((run q g) (let ((q (V/gensym)))
                   ((stream:map (reify/var q)) (g (ε)))))))

  (define get/variable->symbol
    (λ (get)
      (○ get (stream:map variable->symbol))))

  ; `+inf.0` (that is aux `∞`) asks for every answer, a finite `n` for the first `n` ones.
  (define run/with-symbols/taker
    (λ (n)
      (cond
        ((and (number? n) (= n +inf.0)) stream:->list)
        (else (list○take n)))))

  (define-syntax run/with-symbols
    (syntax-rules (↓)
      ((run/with-symbols ↓ n sexp ...)
       ((get/variable->symbol (run/with-symbols/taker n)) (run sexp ...)))
      ((run/with-symbols #t sexp ...)
       ((lambda (α) (if (stream:null? α) #f (stream:car α))) (run sexp ...)))
      ((run/with-symbols n sexp ...)
       (map car (run/with-symbols ↓ n sexp ...)))))

  ; `deepening` *filters* the answers by depth, it does not bound the search: on an infinite stream
  ; of answers none of which is within `depth`, asking for all of them never terminates.
  (define deepening
    (λ (depth)
      (λ (g)
        (λ§ (s)
          ((stream:iterative-deepening 0 depth status-depth) (g s))))))

  ; conditionals -----------------------------------------------------------------------------------

  (define if°/¦
    (λ (question answer otherwise)
      ((∨ stream:append) (∧ question answer) otherwise)))

  (define if°/§
    (λ (question answer otherwise)
      (λ§ (s)
        (let1 (g ((∨ stream:§) (∧ question answer) otherwise))
          (g s)))))

  (define if°/!
    (λ (question answer otherwise)
      (λ§ (s)
        (let1 (α (question s))
          (cond
            ((stream:null? α) (otherwise s))
            (else (stream:>>= α (stream:repeat answer))))))))

  (define if°/!!
    (λ (question answer otherwise)
      (λ§ (s)
        (let1 (α (question s))
          (stream:dest/car+cdr α
            ((α₀ α₊) (answer α₀))
            (else (otherwise s)))))))

  (define-syntax cond°
    (syntax-rules ()
      ((cond° if° (question answer ...))
       (if° question (∧ answer ...) ✗))
      ((cond° if° (question answer ...) otherwise ...)
       (if° question (∧ answer ...) (cond° if° otherwise ...)))))

  (define-syntax cond°/¦
    (syntax-rules ()
      ((cond°/¦ sexp ...) (cond° if°/¦ sexp ...))))

  (define-syntax cond°/§
    (syntax-rules ()
      ((cond°/§ sexp ...) (cond° if°/§ sexp ...))))

  (define-syntax cond°/!
    (syntax-rules ()
      ((cond°/! sexp ...) (cond° if°/! sexp ...))))

  (define-syntax cond°/!!
    (syntax-rules ()
      ((cond°/!! sexp ...) (cond° if°/!! sexp ...))))

  )

; (aux kanren unionfind reasoned): relations from "The Reasoned Schemer", over (aux kanren unionfind).
;
; WARNING: do not import this module, or (aux kanren unionfind), together with any of the following
; without a `prefix` or `rename`, since they export the same names over incompatible values and
; CHICKEN 6 silently keeps the binding of the module imported last (no warning is printed):
;   - (aux kanren micro): null°, cons° and cond° (the latter from (aux kanren unionfind));
;   - (aux kanren arith): car°, cdr° and append°;
;   - (aux variable): V, fresh₁, fresh and variable? (from (aux kanren unionfind));
;   - (aux machine sc) and (aux machine secd): make-status and status? (from (aux kanren unionfind)).
; For example `(import (prefix (aux kanren unionfind) uf:))` or `(import (rename (aux variable) (V var:V)))`.
;
; The four broken drafts of the original (stacksort_º, stacksort__º, 2stacksort_º and 2stacksort__º,
; which called stacksortº/2stacksortº with the wrong arity) are dropped.
(module (aux kanren unionfind reasoned) *

  (import scheme
          (chicken base)
          (aux base)
          (only (aux commons) sub2)
          (aux kanren unionfind))

  (define null°
    (λ (l)
      (≡ l '())))

  (define cons°
    (λ (a d p)
      (≡ (cons a d) p)))

  (define car°
    (λ (p a)
      (fresh (d)
        (cons° a d p))))

  (define cdr°
    (λ (p d)
      (fresh (a)
        (cons° a d p))))

  (define pair°
    (λ (p)
      (fresh (a d)
        (cons° a d p))))

  (define tea-cup°
    (λ (v)
      (cond°/§
        ((≡ v 'tea))
        ((≡ v 'cup)))))

  (define split-pea°
    (λ (v w)
      (cond°/§
        ((≡ v 'split) (≡ w 'pea))
        ((≡ v 'red) (≡ w 'bean)))))

  (define split-pea₁°
    (λ (v w)
      (cond°/§
        ((tea-cup° v) (tea-cup° v))
        ((≡ v #f) (tea-cup° w)))))

  (define list°
    (λ (l)
      (cond°/§
        ((null° l))
        ((fresh (d)
           (∧ (cdr° l d) (list° d)))))))

  (define append°
    (λ (x y z)
      (cond°/§
        ((null° x) (≡ z y))
        ((fresh (x₀ x₊ w)
           (∧
             (cons° x₀ x₊ x)
             (cons° x₀ w z)
             (append° x₊ y w)))))))

  (define any°
    (λ (g)
      (cond°/§
        (g)
        ((fresh () (any° g))))))

  (define always° (any° ✓))
  (define never° (any° ✗))

  (define dyck°
    (λ (α)
      (cond°/§
        ((null° α))
        ((fresh (β γ) (∧
                        (dyck° β)
                        (dyck° γ)
                        (append° `(○ . ,β) `(● . ,γ) α)))))))

  ; `fibonacci°` and `tartaglia°` recur in Scheme on `depth`, they are not relational in it.
  (define fibonacci°
    (λ (depth n α)
      (cond
        ((zero? depth) (≡ α (list n)))
        (else (fresh (β γ)
                (∧
                  (fibonacci° (sub1 depth) (sub1 n) β)
                  (fibonacci° (sub1 depth) (sub2 n) γ)
                  (append° β γ α)))))))

  (define tartaglia°
    (λ (depth n k α)
      (cond
        ((zero? depth) (≡ α (list (list n k))))
        (else (fresh (β γ)
                (∧
                  (tartaglia° (sub1 depth) (sub1 n) (sub1 k) β)
                  (tartaglia° (sub1 depth) (sub1 n) k γ)
                  (append° β γ α)))))))

  ; stack-sorting: `R` is the permutation that sorts to `I` through one stack, `P` the path of
  ; pushes #\) and pops #\( (reversed).
  (define stacksort°
    (λ (P R I)
      (letrec ((ss° (λ (O S I path)
                      (cond°/§
                        ((null° S) (null° I) (≡ R O) (≡ P path))
                        ((fresh (a d s) (∧
                                          (cons° a d I)
                                          (cons° a S s)
                                          (ss° O s d (cons #\) path)))))
                        ((fresh (a d o) (∧
                                          (cons° a d S)
                                          (cons° a O o)
                                          (ss° o d I (cons #\( path)))))))))
        (ss° '() '() I '()))))

  ; the same, through two stacks in series; #\- moves an item from the first stack to the second.
  (define 2stacksort°
    (λ (P R I)
      (letrec ((2ss° (λ (O S₂ S₁ I path)
                       (cond°/§
                         ((null° S₂) (null° S₁) (null° I) (≡ R O) (≡ P path))
                         ((fresh (a d s) (∧
                                           (cons° a d I)
                                           (cons° a S₁ s)
                                           (2ss° O S₂ s d (cons #\) path)))))
                         ((fresh (a d s) (∧
                                           (cons° a d S₁)
                                           (cons° a S₂ s)
                                           (2ss° O s d I (cons #\- path)))))
                         ((fresh (a d o) (∧
                                           (cons° a d S₂)
                                           (cons° a O o)
                                           (2ss° o d S₁ I (cons #\( path)))))))))
        (2ss° '() '() '() I '()))))

  )
