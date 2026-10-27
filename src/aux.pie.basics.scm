;;; (aux pie basics) -- the datatypes that the rest of Pie is written in.
;;;
;;; A port of basics.rkt of Pie, the language of "The Little Typer" by Daniel P. Friedman and David Thrane
;;; Christiansen (https://github.com/the-little-typer/pie, commit 2c89553). Pie is distributed under the
;;; GNU AGPL-3.0, and so is this file, which is derived from it.
;;;
;;; The file is ported whole and in upstream order. It holds source locations, keywords, the abstract syntax
;;; of programs (Src), Core, values, neutral expressions, normal forms, the Perhaps type with go, stop and
;;; go-on, the recognition of variable names, contexts, run-time environments, closures and fresh names.
;;; Upstream provides everything it defines (cf. basics.rkt:11-17), and so does this module, except the
;;; mutators that define-struct of (aux pie racket) also makes, because Racket's structs here are immutable.
;;; Like upstream, it passes on location?, syntax->location and location->srcloc of (aux pie locations).
;;;
;;; Every Typed Racket struct is a define-struct whose constructor keeps the upstream name, (PI x A c),
;;; (go v), (@ loc stx), with the predicate NAME? and the accessors NAME-FIELD, so a pattern of (aux base)
;;; such as #(PI ,x ,A ,c) matches an instance and equal? compares two of them field by field, like the
;;; transparent structs of upstream. A #:type-name becomes a plain define (src? is @?, Norm? is THE?).
;;; The value atoms 'UNIVERSE 'NAT 'ZERO and the others stay symbols. CHICKEN reads a symbol that ends in
;;; a colon as a keyword, so ::, vec::, LIST:: and VEC:: are written with bars.
;;;
;;; The Typed Racket unions (Pie-Keyword, Src-Stx, Core, Value, Neutral, Message, Perhaps, Ctx, Binder,
;;; Serializable-Ctx, Env, Closure) are comment blocks under their sections, with the upstream grammar.
;;; Typed Racket only checks them statically and adds contracts at the borders with untyped modules, so
;;; nothing that Pie prints depends on them. The two predicates that upstream derives from a type with
;;; define-predicate are written out by hand. Neutral? tests for one of the N- structs, and
;;; serializable-ctx? checks the whole shape of a serializable context, Core included, as Typed Racket's
;;; generated predicate does (checked against Racket on 33 contexts).
;;;
;;; Not ported is define-here-and-for-syntax of basics.rkt:366, which defines var-name? for Racket's
;;; macros too. The port has one phase, and var-name? is a plain define.

(module (aux pie basics)
  ( ; abstract syntax
    @ @? @-loc @-stx src-loc src-stx src?
    binder binder? binder-loc binder-var
    ; values
    DELAY-CLOS DELAY-CLOS? DELAY-CLOS-env DELAY-CLOS-expr
    DELAY DELAY? DELAY-val
    QUOTE QUOTE? QUOTE-name
    ADD1 ADD1? ADD1-smaller
    PI PI? PI-arg-name PI-arg-type PI-result-type
    LAM LAM? LAM-arg-name LAM-body
    SIGMA SIGMA? SIGMA-car-name SIGMA-car-type SIGMA-cdr-type
    CONS CONS? CONS-car CONS-cdr
    |LIST::| LIST::? LIST::-head LIST::-tail
    LIST LIST? LIST-entry-type
    EQUAL EQUAL? EQUAL-type EQUAL-from EQUAL-to
    SAME SAME? SAME-value
    VEC VEC? VEC-entry-type VEC-length
    |VEC::| VEC::? VEC::-head VEC::-tail
    EITHER EITHER? EITHER-left-type EITHER-right-type
    LEFT LEFT? LEFT-value
    RIGHT RIGHT? RIGHT-value
    NEU NEU? NEU-type NEU-neutral
    ; neutral expressions
    N-var N-var? N-var-name
    N-TODO N-TODO? N-TODO-where N-TODO-type
    N-which-Nat N-which-Nat? N-which-Nat-target N-which-Nat-base N-which-Nat-step
    N-iter-Nat N-iter-Nat? N-iter-Nat-target N-iter-Nat-base N-iter-Nat-step
    N-rec-Nat N-rec-Nat? N-rec-Nat-target N-rec-Nat-base N-rec-Nat-step
    N-ind-Nat N-ind-Nat? N-ind-Nat-target N-ind-Nat-motive N-ind-Nat-base N-ind-Nat-step
    N-car N-car? N-car-target
    N-cdr N-cdr? N-cdr-target
    N-rec-List N-rec-List? N-rec-List-target N-rec-List-base N-rec-List-step
    N-ind-List N-ind-List? N-ind-List-target N-ind-List-motive N-ind-List-base N-ind-List-step
    N-ind-Absurd N-ind-Absurd? N-ind-Absurd-target N-ind-Absurd-motive
    N-replace N-replace? N-replace-target N-replace-motive N-replace-base
    N-trans1 N-trans1? N-trans1-target1 N-trans1-target2
    N-trans2 N-trans2? N-trans2-target1 N-trans2-target2
    N-trans12 N-trans12? N-trans12-target1 N-trans12-target2
    N-cong N-cong? N-cong-target N-cong-function
    N-symm N-symm? N-symm-target
    N-ind-= N-ind-=? N-ind-=-target N-ind-=-motive N-ind-=-base
    N-head N-head? N-head-target
    N-tail N-tail? N-tail-target
    N-ind-Vec1 N-ind-Vec1? N-ind-Vec1-target1 N-ind-Vec1-target2 N-ind-Vec1-motive N-ind-Vec1-base
    N-ind-Vec1-step
    N-ind-Vec2 N-ind-Vec2? N-ind-Vec2-target1 N-ind-Vec2-target2 N-ind-Vec2-motive N-ind-Vec2-base
    N-ind-Vec2-step
    N-ind-Vec12 N-ind-Vec12? N-ind-Vec12-target1 N-ind-Vec12-target2 N-ind-Vec12-motive N-ind-Vec12-base
    N-ind-Vec12-step
    N-ind-Either N-ind-Either? N-ind-Either-target N-ind-Either-motive N-ind-Either-base-left
    N-ind-Either-base-right
    N-ap N-ap? N-ap-rator N-ap-rand
    Neutral?
    ; normal forms
    THE THE? THE-type THE-value Norm?
    ; error handling
    go go? go-result stop stop? stop-where stop-message go-on
    ; variable names
    var-name?
    ; contexts
    claim claim? claim-type def def? def-type def-value free free? free-type
    var-type binder-type init-ctx bind-free bind-val serializable-ctx?
    ; run-time environments
    ctx->env extend-env var-val
    ; closures
    FO-CLOS FO-CLOS? FO-CLOS-env FO-CLOS-var FO-CLOS-expr HO-CLOS HO-CLOS? HO-CLOS-proc
    ; fresh names
    fresh fresh-binder names-only occurring-names occurring-binder-names)

  (import scheme
          (chicken base)
          (only (scheme base) exact-integer?)
          (chicken format)
          (only (chicken module) reexport)
          (aux base)
          (aux pie racket)
          (aux pie fresh)
          (aux pie locations))

  ;;; ==================== SOURCE LOCATIONS ====================

  ;; Srcloc = (List String Integer Integer Integer Integer), the list (source line column position span),
  ;; cf. basics.rkt:22
  ;;
  ;; Precise-Loc is the location struct of (aux pie locations), opaque upstream, of which basics passes on
  ;; the predicate and two conversions, cf. basics.rkt:13-16 and 24-29. Loc is Precise-Loc. TODO compares
  ;; Locs through their Srclocs when alpha.rkt checks Core, so a Loc must not be a trivial value,
  ;; cf. basics.rkt:32-34
  (reexport (only (aux pie locations) location? syntax->location location->srcloc))

  ;;; ==================== KEYWORDS ====================

  ;; Pie-Keyword, cf. basics.rkt:39
  ;;   U
  ;;   Nat zero add1 which-Nat iter-Nat rec-Nat ind-Nat
  ;;   -> → Π λ Pi ∏ lambda
  ;;   quote Atom
  ;;   car cdr cons Σ Sigma Pair
  ;;   Trivial sole
  ;;   List :: nil rec-List ind-List
  ;;   Absurd ind-Absurd
  ;;   = same replace trans cong symm ind-=
  ;;   Vec vecnil vec:: head tail ind-Vec
  ;;   Either left right ind-Either
  ;;   TODO the

  ;;; ==================== ABSTRACT SYNTAX OF HIGH-LEVEL PROGRAMS ====================

  ;; @ : Loc Src-Stx -> Src, which associates a source location with a Pie expression or declaration, so
  ;; that the feedback can be precise, cf. basics.rkt:59
  (define-struct @ loc stx)

  ;; src-loc : Src -> Loc, cf. basics.rkt:64
  (define src-loc @-loc)

  ;; src-stx : Src -> Src-Stx, cf. basics.rkt:67
  (define src-stx @-stx)

  ;; src? : Any -> Boolean, cf. basics.rkt:70
  (define src? @?)

  ;; binder : Loc Symbol -> Binding-Site, cf. basics.rkt:73
  (define-struct binder loc var)

  ;; Typed-Binder = (List Binding-Site Src), cf. basics.rkt:75

  ;; Src-Stx, the S-expressions of The Little Typer, each sub-expression a Src, cf. basics.rkt:83
  ;;   (the Src Src) | U | Nat | zero | Symbol | Atom | (quote Symbol)
  ;;   (add1 Src) | (which-Nat Src Src Src) | (iter-Nat Src Src Src) | (rec-Nat Src Src Src)
  ;;   (ind-Nat Src Src Src Src)
  ;;   (-> Src Src Src ...) | (Π (Typed-Binder Typed-Binder ...) Src) | (λ (Binding-Site Binding-Site ...) Src)
  ;;   (Σ (Typed-Binder Typed-Binder ...) Src) | (Pair Src Src) | (cons Src Src) | (car Src) | (cdr Src)
  ;;   Trivial | sole | nil | Natural | (:: Src Src) | (List Src)
  ;;   (rec-List Src Src Src) | (ind-List Src Src Src Src)
  ;;   Absurd | (ind-Absurd Src Src)
  ;;   (= Src Src Src) | (same Src) | (replace Src Src Src) | (trans Src Src) | (cong Src Src) | (symm Src)
  ;;   (ind-= Src Src Src)
  ;;   (Vec Src Src) | vecnil | (vec:: Src Src) | (head Src) | (tail Src) | (ind-Vec Src Src Src Src Src)
  ;;   (Either Src Src) | (left Src) | (right Src) | (ind-Either Src Src Src Src)
  ;;   TODO | (Src Src Src ...)

  ;; Core, the result of elaborating a Pie expression, without source positions, cf. basics.rkt:138
  ;;   (the Core Core) | U | Nat | zero | Symbol | (add1 Core)
  ;;   (which-Nat Core (the Core Core) Core) | (iter-Nat Core (the Core Core) Core)
  ;;   (rec-Nat Core (the Core Core) Core) | (ind-Nat Core Core Core Core)
  ;;   (Π ((Symbol Core)) Core) | (λ (Symbol) Core) | Atom | (quote Symbol)
  ;;   (Σ ((Symbol Core)) Core) | (cons Core Core) | (car Core) | (cdr Core)
  ;;   (:: Core Core) | nil | (List Core) | (rec-List Core (the Core Core) Core) | (ind-List Core Core Core Core)
  ;;   Absurd | (ind-Absurd Core Core)
  ;;   (= Core Core Core) | (same Core) | (replace Core Core Core) | (trans Core Core)
  ;;   (cong Core Core Core), whose extra expression is the type found by synth | (symm Core)
  ;;   (ind-= Core Core Core)
  ;;   (Vec Core Core) | (vec:: Core Core) | vecnil | (head Core) | (tail Core)
  ;;   (ind-Vec Core Core Core Core Core)
  ;;   (Either Core Core) | (left Core) | (right Core) | (ind-Either Core Core Core Core)
  ;;   (TODO Srcloc Core) | (Core Core)

  ;;; ==================== VALUES ====================

  ;; Type checking compares the normal forms of expressions, which depend on their types, because the
  ;; types with η-rules (Π, Σ, Trivial and Absurd) constrain them. Normalization evaluates an expression
  ;; to a value and reads the value back into the syntax of its normal form (normalize.rkt). These
  ;; are the values, written in ALL-CAPS, cf. basics.rkt:187-199.
  ;;
  ;; Laziness is a closure that binds no variable, see "Call-by-need" in normalize.rkt, cf. basics.rkt:201

  ;; DELAY-CLOS : Env Core -> DELAY-CLOS, cf. basics.rkt:204
  (define-struct DELAY-CLOS env expr)
  ;; DELAY : (Boxof (U DELAY-CLOS Value)) -> DELAY, cf. basics.rkt:205
  (define-struct DELAY val)

  ;; QUOTE : Symbol -> Value, cf. basics.rkt:207
  (define-struct QUOTE name)
  ;; ADD1 : Value -> Value, cf. basics.rkt:208
  (define-struct ADD1 smaller)
  ;; PI : Symbol Value Closure -> Value, cf. basics.rkt:209
  (define-struct PI arg-name arg-type result-type)
  ;; LAM : Symbol Closure -> Value, cf. basics.rkt:213
  (define-struct LAM arg-name body)
  ;; SIGMA : Symbol Value Closure -> Value, cf. basics.rkt:214
  (define-struct SIGMA car-name car-type cdr-type)
  ;; CONS : Value Value -> Value, cf. basics.rkt:218
  (define-struct CONS car cdr)
  ;; LIST:: : Value Value -> Value, cf. basics.rkt:219
  (define-struct |LIST::| head tail)
  ;; LIST : Value -> Value, cf. basics.rkt:220
  (define-struct LIST entry-type)
  ;; EQUAL : Value Value Value -> Value, cf. basics.rkt:221
  (define-struct EQUAL type from to)
  ;; SAME : Value -> Value, cf. basics.rkt:223
  (define-struct SAME value)
  ;; VEC : Value Value -> Value, cf. basics.rkt:224
  (define-struct VEC entry-type length)
  ;; VEC:: : Value Value -> Value, cf. basics.rkt:225
  (define-struct |VEC::| head tail)
  ;; EITHER : Value Value -> Value, cf. basics.rkt:226
  (define-struct EITHER left-type right-type)
  ;; LEFT : Value -> Value, cf. basics.rkt:227
  (define-struct LEFT value)
  ;; RIGHT : Value -> Value, cf. basics.rkt:228
  (define-struct RIGHT value)
  ;; NEU : Value Neutral -> Value, a neutral expression together with its type, cf. basics.rkt:229
  (define-struct NEU type neutral)

  ;; Value, cf. basics.rkt:230
  ;;   UNIVERSE | NAT | ZERO | ADD1 | QUOTE | ATOM | PI | LAM | SIGMA | CONS | TRIVIAL | SOLE
  ;;   LIST | LIST:: | NIL | ABSURD | EQUAL | SAME | VEC | VECNIL | VEC:: | EITHER | LEFT | RIGHT
  ;;   NEU | DELAY
  ;; where the names without fields are the symbols 'UNIVERSE, 'NAT and so on.

  ;;; ==================== NEUTRAL EXPRESSIONS ====================

  ;; The structs ensure that no expression that is not neutral can be represented, cf. basics.rkt:246.
  ;; A target is a Neutral and the other fields are Norms, as the field names say.

  ;; N-var : Symbol -> Neutral, cf. basics.rkt:249
  (define-struct N-var name)
  ;; N-TODO : Srcloc Value -> Neutral, cf. basics.rkt:250
  (define-struct N-TODO where type)
  ;; N-which-Nat : Neutral Norm Norm -> Neutral, cf. basics.rkt:251
  (define-struct N-which-Nat target base step)
  ;; N-iter-Nat : Neutral Norm Norm -> Neutral, cf. basics.rkt:252
  (define-struct N-iter-Nat target base step)
  ;; N-rec-Nat : Neutral Norm Norm -> Neutral, cf. basics.rkt:253
  (define-struct N-rec-Nat target base step)
  ;; N-ind-Nat : Neutral Norm Norm Norm -> Neutral, cf. basics.rkt:254
  (define-struct N-ind-Nat target motive base step)
  ;; N-car : Neutral -> Neutral, cf. basics.rkt:259
  (define-struct N-car target)
  ;; N-cdr : Neutral -> Neutral, cf. basics.rkt:260
  (define-struct N-cdr target)
  ;; N-rec-List : Neutral Norm Norm -> Neutral, cf. basics.rkt:261
  (define-struct N-rec-List target base step)
  ;; N-ind-List : Neutral Norm Norm Norm -> Neutral, cf. basics.rkt:262
  (define-struct N-ind-List target motive base step)
  ;; N-ind-Absurd : Neutral Norm -> Neutral, cf. basics.rkt:267
  (define-struct N-ind-Absurd target motive)
  ;; N-replace : Neutral Norm Norm -> Neutral, cf. basics.rkt:268
  (define-struct N-replace target motive base)
  ;; N-trans1 : Neutral Norm -> Neutral, cf. basics.rkt:269
  (define-struct N-trans1 target1 target2)
  ;; N-trans2 : Norm Neutral -> Neutral, cf. basics.rkt:270
  (define-struct N-trans2 target1 target2)
  ;; N-trans12 : Neutral Neutral -> Neutral, cf. basics.rkt:271
  (define-struct N-trans12 target1 target2)
  ;; N-cong : Neutral Norm -> Neutral. The function holds enough to find the type of the result again, so
  ;; two fields suffice, cf. basics.rkt:272
  (define-struct N-cong target function)
  ;; N-symm : Neutral -> Neutral, cf. basics.rkt:274
  (define-struct N-symm target)
  ;; N-ind-= : Neutral Norm Norm -> Neutral, cf. basics.rkt:275
  (define-struct N-ind-= target motive base)
  ;; N-head : Neutral -> Neutral, cf. basics.rkt:276
  (define-struct N-head target)
  ;; N-tail : Neutral -> Neutral, cf. basics.rkt:277
  (define-struct N-tail target)
  ;; N-ind-Vec1 : Neutral Norm Norm Norm Norm -> Neutral, cf. basics.rkt:278
  (define-struct N-ind-Vec1 target1 target2 motive base step)
  ;; N-ind-Vec2 : Norm Neutral Norm Norm Norm -> Neutral, cf. basics.rkt:284
  (define-struct N-ind-Vec2 target1 target2 motive base step)
  ;; N-ind-Vec12 : Neutral Neutral Norm Norm Norm -> Neutral, cf. basics.rkt:290
  (define-struct N-ind-Vec12 target1 target2 motive base step)
  ;; N-ind-Either : Neutral Norm Norm Norm -> Neutral, cf. basics.rkt:296
  (define-struct N-ind-Either target motive base-left base-right)
  ;; N-ap : Neutral Norm -> Neutral, cf. basics.rkt:301
  (define-struct N-ap rator rand)

  ;; Neutral, cf. basics.rkt:303
  ;;   N-var | N-TODO | N-which-Nat | N-iter-Nat | N-rec-Nat | N-ind-Nat | N-car | N-cdr
  ;;   N-rec-List | N-ind-List | N-ind-Absurd
  ;;   N-replace | N-trans1 | N-trans2 | N-trans12 | N-cong | N-symm | N-ind-=
  ;;   N-head | N-tail | N-ind-Vec1 | N-ind-Vec2 | N-ind-Vec12 | N-ind-Either | N-ap

  ;; Neutral? : Any -> Boolean, whether x is one of the structs of Neutral, cf. basics.rkt:314
  (define (Neutral? x)
    (or (N-var? x) (N-TODO? x)
        (N-which-Nat? x) (N-iter-Nat? x) (N-rec-Nat? x) (N-ind-Nat? x)
        (N-car? x) (N-cdr? x)
        (N-rec-List? x) (N-ind-List? x)
        (N-ind-Absurd? x)
        (N-replace? x) (N-trans1? x) (N-trans2? x) (N-trans12? x) (N-cong? x) (N-symm? x) (N-ind-=? x)
        (N-head? x) (N-tail? x) (N-ind-Vec1? x) (N-ind-Vec2? x) (N-ind-Vec12? x)
        (N-ind-Either? x)
        (N-ap? x)))

  ;;; ==================== NORMAL FORMS ====================

  ;; THE : Value Value -> Norm, a type value and a value of that type, which read-back turns into the
  ;; syntax of a normal form, cf. basics.rkt:316-320
  (define-struct THE type value)

  ;; Norm? : Any -> Boolean, cf. basics.rkt:322
  (define Norm? THE?)

  ;;; ==================== ERROR HANDLING ====================

  ;; Message = (Listof (U String Core)), text shown to the user mixed with Core expressions, cf. basics.rkt:329
  ;;
  ;; (Perhaps α) = (U (go α) stop), the result of an operation that can fail, cf. basics.rkt:334

  ;; go : α -> (Perhaps α), a successful result, cf. basics.rkt:338
  (define-struct go result)

  ;; stop : Loc Message -> (Perhaps α), an error message, cf. basics.rkt:341
  (define-struct stop where message)

  ;; (go-on ((PAT EXPR) ...) BODY) is like let*, except that the first EXPR whose value is a stop becomes
  ;; the value of the whole form. Otherwise the result of each go is matched against PAT, a pattern of
  ;; (aux base) match/first such as ,x or (the ,t ,e), and BODY runs with every PAT bound. A value that is
  ;; neither a go nor a stop, or a go whose result does not match PAT, raises the error of match/first, as
  ;; Racket's match raises its own, cf. basics.rkt:343-354
  (define-syntax go-on
    (syntax-rules ()
      ((go-on () e) e)
      ((go-on ((p0 b0) (p b) ...) e)
       (match/first b0
         (#(go p0) (go-on ((p b) ...) e))
         (#(stop ,where ,msg) (stop where msg))))))

  ;;; ==================== RECOGNIZING VARIABLE NAMES ====================

  ;; Upstream defines var-name? for its macros and for its programs with define-here-and-for-syntax, because
  ;; the parser needs it at expansion time. The port has a single phase, so var-name? is a plain define,
  ;; cf. basics.rkt:359-368

  ;; var-name? : Symbol -> Boolean, #t for every symbol that is not a Pie keyword and #f for the keywords,
  ;; cf. basics.rkt:370-376
  (define (var-name? x)
    (not (or (eqv? x 'U) (eqv? x 'Nat) (eqv? x 'zero)
             (eqv? x 'add1) (eqv? x 'which-Nat) (eqv? x 'ind-Nat)
             (eqv? x 'rec-Nat) (eqv? x 'iter-Nat)
             (eqv? x '->) (eqv? x '→) (eqv? x 'Π) (eqv? x 'Pi) (eqv? x '∏) (eqv? x 'λ) (eqv? x 'lambda)
             (eqv? x 'quote) (eqv? x 'Atom) (eqv? x 'Σ) (eqv? x 'Sigma) (eqv? x 'Pair)
             (eqv? x 'cons) (eqv? x 'car) (eqv? x 'cdr)
             (eqv? x 'Trivial) (eqv? x 'sole)
             (eqv? x '|::|) (eqv? x 'nil) (eqv? x 'List)
             (eqv? x 'rec-List) (eqv? x 'ind-List)
             (eqv? x 'Absurd) (eqv? x 'ind-Absurd)
             (eqv? x '=) (eqv? x 'same) (eqv? x 'replace)
             (eqv? x 'symm) (eqv? x 'trans) (eqv? x 'cong) (eqv? x 'ind-=)
             (eqv? x 'Vec) (eqv? x '|vec::|) (eqv? x 'vecnil)
             (eqv? x 'head) (eqv? x 'tail) (eqv? x 'ind-Vec)
             (eqv? x 'Either) (eqv? x 'left) (eqv? x 'right)
             (eqv? x 'ind-Either) (eqv? x 'the)
             (eqv? x 'TODO))))

  ;;; ==================== CONTEXTS ====================

  ;; Ctx = (Listof (Pair Symbol Binder)), which maps free variable names to binders, cf. basics.rkt:400
  ;;
  ;; Binder = (U Def Free Claim), cf. basics.rkt:405-411. A free binder is a variable bound in a larger
  ;; context by λ, Π or Σ, a def binder a name bound by define, and a claim binder binds no name but
  ;; reserves it for a later define and records the type that the define will use.

  ;; claim : Value -> Binder, cf. basics.rkt:413
  (define-struct claim type)
  ;; def : Value Value -> Binder, cf. basics.rkt:414
  (define-struct def type value)
  ;; free : Value -> Binder, cf. basics.rkt:415
  (define-struct free type)

  ;; var-type : Ctx Loc Symbol -> (Perhaps Value), the type of the closest binder of x that is not a claim,
  ;; cf. basics.rkt:418-421
  (define (var-type Γ where x)
    (match/first Γ
      (() (stop where `("Unknown variable" ,x)))
      (((,y . #(claim ,tv)) . ,Γ-next)
       (var-type Γ-next where x))
      (((,y . ,b) . ,Γ-next)
       (if (eqv? x y)
           (go (binder-type b))
           (var-type Γ-next where x)))))

  ;; binder-type : Binder -> Value, cf. basics.rkt:431
  (define (binder-type b)
    (match/first b
      (#(claim ,tv) tv)
      (#(def ,tv ,v) tv)
      (#(free ,tv) tv)))

  ;; init-ctx : Ctx, the starting context, which is empty, cf. basics.rkt:438
  (define init-ctx '())

  ;; bind-free : Ctx Symbol Value -> Ctx. Racket's (error 'bind-free FORMAT x Γ) prints
  ;; "bind-free: MESSAGE", CHICKEN's (error 'bind-free MESSAGE) prints "(bind-free) MESSAGE",
  ;; cf. basics.rkt:442
  (define (bind-free Γ x tv)
    (if (assv x Γ)
        (error 'bind-free (format "~a is already bound in ~a" x Γ))
        (cons (cons x (free tv)) Γ)))

  ;; bind-val : Ctx Symbol Value Value -> Ctx, cf. basics.rkt:448
  (define (bind-val Γ x tv v)
    (cons (cons x (def tv v)) Γ))

  ;; Serializable-Ctx, see the comments of normalize.rkt, cf. basics.rkt:453-458
  ;;   (Listof (List Symbol (U (List 'free Core) (List 'def Core Core) (List 'claim Core))))

  ;; serializable-ctx? : Any -> Boolean, the predicate that Typed Racket derives from Serializable-Ctx. It
  ;; checks every entry and every Core expression in it, cf. basics.rkt:460
  (define (serializable-ctx? x)
    (and (list? x)
         (every-element? (λ (entry)
                           (match/first entry
                             (((,y (free ,t)) ⊣ (symbol? y)) (core? t))
                             (((,y (def ,t ,v)) ⊣ (symbol? y)) (and (core? t) (core? v)))
                             (((,y (claim ,t)) ⊣ (symbol? y)) (core? t))
                             (else #f)))
                         x)))

  ;; every-element? : (Any -> Boolean) (Listof Any) -> Boolean, whether p holds for every element of xs.
  (define (every-element? p xs)
    (or (null? xs) (and (p (car xs)) (every-element? p (cdr xs)))))

  ;; core? : Any -> Boolean, the part of serializable-ctx? that checks the type Core of basics.rkt:138. The
  ;; branches of the union may overlap, the last one, (Core Core), with all the others of two elements,
  ;; so a clause whose guard fails passes the value on to the next clauses, as a union does.
  (define (core? x)
    (or (symbol? x)
        (match/first x
          (((the ,t ,e) ⊣ (and (core? t) (core? e))) #t)
          (((add1 ,n) ⊣ (core? n)) #t)
          (((which-Nat ,tgt ,b ,s) ⊣ (and (core? tgt) (the-core? b) (core? s))) #t)
          (((iter-Nat ,tgt ,b ,s) ⊣ (and (core? tgt) (the-core? b) (core? s))) #t)
          (((rec-Nat ,tgt ,b ,s) ⊣ (and (core? tgt) (the-core? b) (core? s))) #t)
          (((ind-Nat . ,args) ⊣ (cores? 4 args)) #t)
          (((Π ((,y ,A)) ,B) ⊣ (and (symbol? y) (core? A) (core? B))) #t)
          (((λ (,y) ,b) ⊣ (and (symbol? y) (core? b))) #t)
          (((quote ,y) ⊣ (symbol? y)) #t)
          (((Σ ((,y ,A)) ,D) ⊣ (and (symbol? y) (core? A) (core? D))) #t)
          (((cons . ,args) ⊣ (cores? 2 args)) #t)
          (((car . ,args) ⊣ (cores? 1 args)) #t)
          (((cdr . ,args) ⊣ (cores? 1 args)) #t)
          (((|::| . ,args) ⊣ (cores? 2 args)) #t)
          (((List . ,args) ⊣ (cores? 1 args)) #t)
          (((rec-List ,tgt ,b ,s) ⊣ (and (core? tgt) (the-core? b) (core? s))) #t)
          (((ind-List . ,args) ⊣ (cores? 4 args)) #t)
          (((ind-Absurd . ,args) ⊣ (cores? 2 args)) #t)
          (((= . ,args) ⊣ (cores? 3 args)) #t)
          (((same . ,args) ⊣ (cores? 1 args)) #t)
          (((replace . ,args) ⊣ (cores? 3 args)) #t)
          (((trans . ,args) ⊣ (cores? 2 args)) #t)
          (((cong . ,args) ⊣ (cores? 3 args)) #t)
          (((symm . ,args) ⊣ (cores? 1 args)) #t)
          (((ind-= . ,args) ⊣ (cores? 3 args)) #t)
          (((Vec . ,args) ⊣ (cores? 2 args)) #t)
          (((|vec::| . ,args) ⊣ (cores? 2 args)) #t)
          (((head . ,args) ⊣ (cores? 1 args)) #t)
          (((tail . ,args) ⊣ (cores? 1 args)) #t)
          (((ind-Vec . ,args) ⊣ (cores? 5 args)) #t)
          (((Either . ,args) ⊣ (cores? 2 args)) #t)
          (((left . ,args) ⊣ (cores? 1 args)) #t)
          (((right . ,args) ⊣ (cores? 1 args)) #t)
          (((ind-Either . ,args) ⊣ (cores? 4 args)) #t)
          (((TODO ,where ,t) ⊣ (and (srcloc? where) (core? t))) #t)
          (((,f ,a) ⊣ (and (core? f) (core? a))) #t)
          (else #f))))

  ;; cores? : Integer Any -> Boolean, whether xs is a list of exactly n Core expressions.
  (define (cores? n xs)
    (cond
      ((zero? n) (null? xs))
      ((pair? xs) (and (core? (car xs)) (cores? (sub1 n) (cdr xs))))
      (else #f)))

  ;; the-core? : Any -> Boolean, whether x is (the Core Core).
  (define (the-core? x)
    (match/first x
      ((the ,t ,e) (and (core? t) (core? e)))
      (else #f)))

  ;; srcloc? : Any -> Boolean, whether x is a Srcloc, a string and four exact integers, cf. basics.rkt:22
  (define (srcloc? x)
    (match/first x
      ((,s ,l ,c ,p ,n) (and (string? s) (exact-integer? l) (exact-integer? c) (exact-integer? p)
                             (exact-integer? n)))
      (else #f)))

  ;;; ==================== RUN-TIME ENVIRONMENTS ====================

  ;; Env = (Listof (Pair Symbol Value)), a value for each variable, cf. basics.rkt:467-469

  ;; ctx->env : Ctx -> Env. Normalizing an expression with free variables needs a value for each, which
  ;; is the variable itself as a neutral expression, cf. basics.rkt:471-477
  (define (ctx->env Γ)
    (match/first Γ
      (((,x . #(def ,tv ,v)) . ,Γ-next)
       (cons (cons x v)
             (ctx->env Γ-next)))
      (((,x . #(free ,tv)) . ,Γ-next)
       (cons (cons x (NEU tv (N-var x)))
             (ctx->env Γ-next)))
      (((,x . #(claim ,tv)) . ,Γ-next)
       (ctx->env Γ-next))
      (() '())))

  ;; extend-env : Env Symbol Value -> Env, cf. basics.rkt:490
  (define (extend-env ρ x v) (cons (cons x v) ρ))

  ;; var-val : Env Symbol -> Value, looked up with assv, the usual Lisp way, cf. basics.rkt:494-496
  (define (var-val ρ x)
    (match/first (assv x ρ)
      ((,y . ,v) v)
      (#f (error (format "Variable ~a not in\n\tρ: ~a\n" x ρ)))))

  ;;; ==================== CLOSURES ====================

  ;; Closure = (U FO-CLOS HO-CLOS). Pie has two kinds of closures, for different purposes, and
  ;; val-of-closure of normalize.rkt finds the value of either given a value for its variable,
  ;; cf. basics.rkt:504-512

  ;; FO-CLOS : Env Symbol Core -> Closure, a first-order closure, an environment for the free variables of
  ;; an expression. Most closures of Pie are first order, and they are easy to debug because their parts
  ;; can be seen, but making one out of values would need a read-back first, cf. basics.rkt:514-521
  (define-struct FO-CLOS env var expr)

  ;; HO-CLOS : (Value -> Value) -> Closure, a higher-order closure, which is a procedure of the host. It is
  ;; the convenient kind for closures built from values, such as the types of steps when type checking and
  ;; the type annotations of THE and NEU when evaluating, cf. basics.rkt:523-529
  (define-struct HO-CLOS proc)

  ;;; ==================== FINDING FRESH NAMES ====================

  ;; fresh : Ctx Symbol -> Symbol, a name close to x that the context does not use, the judgment
  ;; Γ ⊢ fresh ↝ x of The Little Typer with a suggestion so that elaborated code keeps the user's names,
  ;; cf. basics.rkt:534-542
  (define (fresh Γ x)
    (freshen (names-only Γ) x))

  ;; fresh-binder : Ctx Src Symbol -> Symbol, a name that neither the context nor expr uses, for a binder
  ;; that must not capture a free variable of expr, cf. basics.rkt:545-550
  (define (fresh-binder Γ expr x)
    (freshen (append (names-only Γ) (occurring-names expr)) x))

  ;; names-only : Ctx -> (Listof Symbol), the names that a context describes, cf. basics.rkt:554-556
  (define (names-only Γ)
    (cond
      ((null? Γ) '())
      (else (cons (car (car Γ)) (names-only (cdr Γ))))))

  ;; occurring-names : Src -> (Listof Symbol), every name in expr. The free ones would be enough, but
  ;; avoiding the bound ones too makes the binders of desugared code differ more from the program as
  ;; written, which helps to read the internals, cf. basics.rkt:562-567
  (define (occurring-names expr)
    (match/first (src-stx expr)
      ((the ,t ,e)
       (append (occurring-names t) (occurring-names e)))
      ((quote ,x)
       '())
      ((add1 ,n)
       (occurring-names n))
      ((which-Nat ,tgt ,base ,step)
       (append (occurring-names tgt) (occurring-names base) (occurring-names step)))
      ((iter-Nat ,tgt ,base ,step)
       (append (occurring-names tgt) (occurring-names base) (occurring-names step)))
      ((rec-Nat ,tgt ,base ,step)
       (append (occurring-names tgt) (occurring-names base) (occurring-names step)))
      ((ind-Nat ,tgt ,mot ,base ,step)
       (append (occurring-names tgt) (occurring-names mot) (occurring-names base) (occurring-names step)))
      ((-> ,t0 . ,ts)
       (append (occurring-names t0) (apply append (map occurring-names ts))))
      ((Π ,bindings ,t)
       (append (apply append (map occurring-binder-names bindings)) (occurring-names t)))
      ((λ ,bindings ,t)
       (append (map binder-var bindings) (occurring-names t)))
      ((Σ ,bindings ,t)
       (append (apply append (map occurring-binder-names bindings)) (occurring-names t)))
      ((Pair ,A ,D)
       (append (occurring-names A) (occurring-names D)))
      ((cons ,A ,D)
       (append (occurring-names A) (occurring-names D)))
      ((car ,p)
       (occurring-names p))
      ((cdr ,p)
       (occurring-names p))
      ((|::| ,A ,D)
       (append (occurring-names A) (occurring-names D)))
      ((List ,E)
       (occurring-names E))
      ((rec-List ,tgt ,base ,step)
       (append (occurring-names tgt) (occurring-names base) (occurring-names step)))
      ((ind-List ,tgt ,mot ,base ,step)
       (append (occurring-names tgt) (occurring-names mot) (occurring-names base) (occurring-names step)))
      ((ind-Absurd ,tgt ,mot)
       (append (occurring-names tgt) (occurring-names mot)))
      ((= ,A ,from ,to)
       (append (occurring-names A) (occurring-names from) (occurring-names to)))
      ((same ,e)
       (occurring-names e))
      ((replace ,tgt ,mot ,base)
       (append (occurring-names tgt) (occurring-names mot) (occurring-names base)))
      ((trans ,tgt1 ,tgt2)
       (append (occurring-names tgt1) (occurring-names tgt2)))
      ((cong ,tgt ,f)
       (append (occurring-names tgt) (occurring-names f)))
      ((symm ,tgt)
       (occurring-names tgt))
      ((ind-= ,tgt ,mot ,base)
       (append (occurring-names tgt) (occurring-names mot) (occurring-names base)))
      ((Vec ,E ,len)
       (append (occurring-names E) (occurring-names len)))
      ((|vec::| ,hd ,tl)
       (append (occurring-names hd) (occurring-names tl)))
      ((head ,tgt)
       (occurring-names tgt))
      ((tail ,tgt)
       (occurring-names tgt))
      ((ind-Vec ,len ,tgt ,mot ,base ,step)
       (append (occurring-names len) (occurring-names tgt)
               (occurring-names mot)
               (occurring-names base) (occurring-names step)))
      ((Either ,A ,B)
       (append (occurring-names A) (occurring-names B)))
      ((left ,e)
       (occurring-names e))
      ((right ,e)
       (occurring-names e))
      ((ind-Either ,tgt ,mot ,l ,r)
       (append (occurring-names tgt) (occurring-names mot) (occurring-names l) (occurring-names r)))
      (((,f ,arg0 . ,args) ⊣ (src? f))
       (append (occurring-names f) (occurring-names arg0) (apply append (map occurring-names args))))
      (,x
       (if (and (symbol? x) (var-name? x))
           (list x)
           '()))))

  ;; occurring-binder-names : Typed-Binder -> (Listof Symbol), cf. basics.rkt:651
  (define (occurring-binder-names b)
    (match1/first ((#(binder ,where ,x) ,t) b)
      (cons x (occurring-names t)))))
