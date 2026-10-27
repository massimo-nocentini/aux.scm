;;; (aux pie parser) -- from the concrete syntax of Pie to the abstract syntax of basics.
;;;
;;; A port of parser.rkt of Pie, the language of "The Little Typer" by Daniel P. Friedman and David Thrane
;;; Christiansen (https://github.com/the-little-typer/pie, commit 2c89553). Pie is distributed under the
;;; GNU AGPL-3.0, and so is this file, which is derived from it.
;;;
;;; The parsers turn the syntax objects that (aux pie syntax) reads into Src, the source-annotated input
;;; ASTs of basics, where every node is an @ that carries the location of the syntax it came from. They are
;;; written with the syntax-parse of (aux pie syntax-parse), with the clauses, the cuts and the
;;; descriptions of upstream in the same order, because when a program is malformed the error that Pie
;;; prints is syntax-parse's report on these very patterns. The two descriptions of check-same that are
;;; computed from the type are dynamic, as upstream, and they change the report. A check-same that is too
;;; short says "expected more terms" without "starting with".
;;;
;;; Upstream provides everything it defines (cf. parser.rkt:12), and so does this module. In a clause body a
;;; pattern variable is the syntax object it matched and a variable under `...` the list of the syntax
;;; objects it matched, so upstream's #'x is x, (syntax->list #'(x0 x ...)) is (cons x0 x) and #'x.name is
;;; x. Racket evaluates the arguments of a call from left to right, and so does CHICKEN, so with two
;;; malformed sub-terms the first one is reported, as upstream.
;;;
;;; ==================== THE BINDERS ====================
;;; pie->binders and pie-decl->binders (parser.rkt:433-795) are macros. main.rkt:203 puts one
;;; (pie-decl->binders form) for each form into the module that Racket expands after the elaboration, so
;;; that DrRacket can draw binding arrows. They turn a claim into a module-level definition of
;;; internal-claim-binding-x, a definition of x into one of x, and every λ, Π and Σ into a let* of its
;;; names. That expansion prints nothing when it succeeds, but it can fail, because the names are the
;;; user's own identifiers and Racket treats them as such
;;;   - a definition of internal-claim-binding-y next to a claim of y defines the same name twice;
;;;   - a name #%datum, bound by a definition or by a binder, takes the place of the #%datum of the module
;;;     language (main.rkt:217) around the literals in its scope, and then a literal number is an error.
;;; The port keeps both macros as procedures that give one step of their expansion. Their result is a
;;; Form, an S-expression in which the identifiers of the templates (void, let*, define, quote, the kw:
;;; keywords of pie-info.rkt and pie->binders itself) are plain symbols and the user's syntax stays the
;;; syntax object it was. The two kinds never bind each other, as with Racket's scopes, the templates'
;;; identifiers belonging to parser.rkt and the user's to the module. A (pie->binders stx) inside a Form is
;;; a use of the macro that is not expanded yet, as #'(pie->binders e) is upstream. The module expansion of
;;; (aux pie) walks the Forms. add-disappeared (parser.rkt:426) only adds the syntax properties of
;;; check-syntax, so it is left out, and so is the location that syntax/loc gives to each template.
;;;
;;; Not ported are
;;;   - the copy of pie-id inside begin-for-syntax (parser.rkt:18-21), which serves Racket's macros only;
;;;     the port has one phase;
;;;   - the commented-out srcloc of syntax->srcloc (parser.rkt:25-30);
;;;   - the require of typechecker.rkt, of which the parser uses nothing (cf. parser.rkt:6); dropping it
;;;     keeps the parser below the typechecker. The kw: identifiers of pie-info.rkt (parser.rkt:7) are
;;;     symbols of the Forms.

(module (aux pie parser)
  (pie-id syntax->srcloc binding-site typed-binders
   make-U make--> make-Nat make-zero make-add1 make-lambda make-Pi make-Sigma make-ap make-Atom make-Trivial
   make-sole make-List make-Vec make-Either make-nil |make-::| |make-vec::| make-vecnil make-Absurd make-Pair
   make-cons make-the make-ind-Absurd make-trans make-cong make-ind-= make-which-Nat make-iter-Nat
   make-rec-Nat make-ind-Nat make-rec-List make-ind-List make-ind-Either make-ind-Vec make-= make-replace
   make-symm make-head make-tail make-same make-left make-right make-car make-cdr make-quote make-var-ref
   make-nat-literal make-TODO
   parse-pie parse-pie-decl
   pie->binders pie-decl->binders)

  (import scheme
          (chicken base)
          (chicken format)
          (aux base)
          (aux pie racket)
          (aux pie syntax)
          (aux pie syntax-parse)
          (aux pie locations)
          (aux pie basics))

  ;;; ==================== NAMES AND LOCATIONS ====================

  ;; pie-id : Class, an identifier that var-name? accepts, that is, not a Pie keyword, cf. parser.rkt:14
  (define pie-id
    (make-syntax-class "valid Pie name" 'id (λ (name) (var-name? (syntax->datum name)))))

  ;; syntax->srcloc : Syntax -> Loc, despite its name the location of the syntax, cf. parser.rkt:23
  (define (syntax->srcloc stx)
    (syntax->location stx))

  ;; binding-site : Identifier -> Binding-Site, cf. parser.rkt:32
  (define (binding-site id)
    (binder (syntax->srcloc id) (syntax->datum id)))

  ;;; ==================== CONSTRUCTORS ====================
  ;;
  ;; Each (make-X loc args ...) builds the Src of one form, located where the syntax loc was read,
  ;; cf. parser.rkt:35-232.

  ;; cf. parser.rkt:35
  (define (make-U loc)
    (@ (syntax->srcloc loc) 'U))

  ;; make--> : Syntax Src Src (Listof Src) -> Src, cf. parser.rkt:39
  (define (make--> loc A B Cs)
    (@ (syntax->srcloc loc)
       `(-> ,A ,B . ,Cs)))

  ;; cf. parser.rkt:44
  (define (make-Nat loc)
    (@ (syntax->srcloc loc) 'Nat))

  ;; cf. parser.rkt:48
  (define (make-zero loc)
    (@ (syntax->srcloc loc) 'zero))

  ;; cf. parser.rkt:52
  (define (make-add1 loc n)
    (@ (syntax->srcloc loc) `(add1 ,n)))

  ;; make-lambda : Syntax (Listof Binding-Site) Src -> Src, cf. parser.rkt:56
  (define (make-lambda loc xs body)
    (@ (syntax->srcloc loc) `(λ ,xs ,body)))

  ;; make-Pi : Syntax Typed-Binders Src -> Src, cf. parser.rkt:60
  (define (make-Pi loc args body)
    (@ (syntax->srcloc loc) `(Π ,args ,body)))

  ;; cf. parser.rkt:64
  (define (make-Sigma loc args body)
    (@ (syntax->srcloc loc) `(Σ ,args ,body)))

  ;; typed-binders : Typed-Binder (Listof Typed-Binder) -> (Listof Typed-Binder), a non-empty list of
  ;; (list Binding-Site Src), cf. parser.rkt:68
  (define (typed-binders A Bs)
    (cons A Bs))

  ;; make-ap : Syntax Src Src (Listof Src) -> Src, cf. parser.rkt:72
  (define (make-ap loc rator rand0 rands)
    (@ (syntax->srcloc loc)
       `(,rator ,rand0 . ,rands)))

  ;; cf. parser.rkt:77
  (define (make-Atom loc)
    (@ (syntax->srcloc loc) 'Atom))

  ;; cf. parser.rkt:81
  (define (make-Trivial loc)
    (@ (syntax->srcloc loc) 'Trivial))

  ;; cf. parser.rkt:85
  (define (make-sole loc)
    (@ (syntax->srcloc loc) 'sole))

  ;; cf. parser.rkt:89
  (define (make-List loc E)
    (@ (syntax->srcloc loc) `(List ,E)))

  ;; cf. parser.rkt:93
  (define (make-Vec loc E len)
    (@ (syntax->srcloc loc) `(Vec ,E ,len)))

  ;; cf. parser.rkt:97
  (define (make-Either loc L R)
    (@ (syntax->srcloc loc) `(Either ,L ,R)))

  ;; cf. parser.rkt:101
  (define (make-nil loc)
    (@ (syntax->srcloc loc) 'nil))

  ;; cf. parser.rkt:105
  (define (|make-::| loc x xs)
    (@ (syntax->srcloc loc) `(|::| ,x ,xs)))

  ;; cf. parser.rkt:109
  (define (|make-vec::| loc x xs)
    (@ (syntax->srcloc loc) `(|vec::| ,x ,xs)))

  ;; cf. parser.rkt:113
  (define (make-vecnil loc)
    (@ (syntax->srcloc loc) 'vecnil))

  ;; cf. parser.rkt:117
  (define (make-Absurd loc)
    (@ (syntax->srcloc loc) 'Absurd))

  ;; cf. parser.rkt:121
  (define (make-Pair loc A B)
    (@ (syntax->srcloc loc) `(Pair ,A ,B)))

  ;; cf. parser.rkt:125
  (define (make-cons loc a d)
    (@ (syntax->srcloc loc) `(cons ,a ,d)))

  ;; cf. parser.rkt:129
  (define (make-the loc a d)
    (@ (syntax->srcloc loc) `(the ,a ,d)))

  ;; cf. parser.rkt:133
  (define (make-ind-Absurd loc a d)
    (@ (syntax->srcloc loc) `(ind-Absurd ,a ,d)))

  ;; cf. parser.rkt:137
  (define (make-trans loc p1 p2)
    (@ (syntax->srcloc loc) `(trans ,p1 ,p2)))

  ;; cf. parser.rkt:141
  (define (make-cong loc p1 p2)
    (@ (syntax->srcloc loc) `(cong ,p1 ,p2)))

  ;; cf. parser.rkt:144
  (define (make-ind-= loc tgt mot base)
    (@ (syntax->srcloc loc) `(ind-= ,tgt ,mot ,base)))

  ;; cf. parser.rkt:147
  (define (make-which-Nat loc e1 e2 e3)
    (@ (syntax->srcloc loc) `(which-Nat ,e1 ,e2 ,e3)))

  ;; cf. parser.rkt:151
  (define (make-iter-Nat loc e1 e2 e3)
    (@ (syntax->srcloc loc) `(iter-Nat ,e1 ,e2 ,e3)))

  ;; cf. parser.rkt:155
  (define (make-rec-Nat loc e1 e2 e3)
    (@ (syntax->srcloc loc) `(rec-Nat ,e1 ,e2 ,e3)))

  ;; cf. parser.rkt:159
  (define (make-ind-Nat loc e1 e2 e3 e4)
    (@ (syntax->srcloc loc) `(ind-Nat ,e1 ,e2 ,e3 ,e4)))

  ;; cf. parser.rkt:163
  (define (make-rec-List loc e1 e2 e3)
    (@ (syntax->srcloc loc) `(rec-List ,e1 ,e2 ,e3)))

  ;; cf. parser.rkt:167
  (define (make-ind-List loc e1 e2 e3 e4)
    (@ (syntax->srcloc loc) `(ind-List ,e1 ,e2 ,e3 ,e4)))

  ;; cf. parser.rkt:171
  (define (make-ind-Either loc e1 e2 e3 e4)
    (@ (syntax->srcloc loc) `(ind-Either ,e1 ,e2 ,e3 ,e4)))

  ;; cf. parser.rkt:175
  (define (make-ind-Vec loc e1 e2 e3 e4 e5)
    (@ (syntax->srcloc loc) `(ind-Vec ,e1 ,e2 ,e3 ,e4 ,e5)))

  ;; cf. parser.rkt:179
  (define (make-= loc e1 e2 e3)
    (@ (syntax->srcloc loc) `(= ,e1 ,e2 ,e3)))

  ;; cf. parser.rkt:183
  (define (make-replace loc e1 e2 e3)
    (@ (syntax->srcloc loc) `(replace ,e1 ,e2 ,e3)))

  ;; cf. parser.rkt:187
  (define (make-symm loc e)
    (@ (syntax->srcloc loc) `(symm ,e)))

  ;; cf. parser.rkt:191
  (define (make-head loc e)
    (@ (syntax->srcloc loc) `(head ,e)))

  ;; cf. parser.rkt:195
  (define (make-tail loc e)
    (@ (syntax->srcloc loc) `(tail ,e)))

  ;; cf. parser.rkt:199
  (define (make-same loc e)
    (@ (syntax->srcloc loc) `(same ,e)))

  ;; cf. parser.rkt:203
  (define (make-left loc e)
    (@ (syntax->srcloc loc) `(left ,e)))

  ;; cf. parser.rkt:207
  (define (make-right loc e)
    (@ (syntax->srcloc loc) `(right ,e)))

  ;; cf. parser.rkt:211
  (define (make-car loc e)
    (@ (syntax->srcloc loc) `(car ,e)))

  ;; cf. parser.rkt:215
  (define (make-cdr loc e)
    (@ (syntax->srcloc loc) `(cdr ,e)))

  ;; make-quote : Syntax Symbol -> Src, cf. parser.rkt:219
  (define (make-quote loc a)
    (@ (syntax->srcloc loc) `(quote ,a)))

  ;; make-var-ref : Syntax Symbol -> Src, cf. parser.rkt:223
  (define (make-var-ref loc a)
    (@ (syntax->srcloc loc) a))

  ;; make-nat-literal : Syntax Natural -> Src, cf. parser.rkt:227
  (define (make-nat-literal loc n)
    (@ (syntax->srcloc loc) n))

  ;; cf. parser.rkt:231
  (define (make-TODO loc)
    (@ (syntax->srcloc loc) 'TODO))

  ;;; ==================== PARSERS ====================

  ;; The templates #'(Π more ...) of parser.rkt:282 and 284 are syntax objects that Racket reads from
  ;; parser.rkt itself, so the rewritten Pi and ∏ forms are located there, and an error about them
  ;; reports parser.rkt:282:18 or 284:18. The port builds the same syntax, with the positions and spans
  ;; measured by reading parser.rkt with Racket, that is the list (Π more ...) and, one column to its right,
  ;; its identifier Π. The template of Sigma (parser.rkt:297) goes through syntax/loc, which gives the list
  ;; the location of the form being parsed, so only its identifier Σ comes from parser.rkt.
  (define (template-Π line column position more)
    (datum->syntax #f
                   (cons (datum->syntax #f 'Π (list (pie-upstream-parser-source) line (+ column 1) (+ position 1) 1))
                         more)
                   (list (pie-upstream-parser-source) line column position 12)))

  (define (template/loc-Σ stx more)
    (datum->syntax #f
                   (cons (datum->syntax #f 'Σ (list (pie-upstream-parser-source) 297 33 6911 1)) more)
                   stx))

  ;; parse-pie : Syntax -> Src, cf. parser.rkt:235
  (define (parse-pie stx)
    (syntax-parse stx
      #:datum-literals (U
                        Nat zero add1
                        which-Nat iter-Nat rec-Nat ind-Nat
                        → -> Pi Π ∏ the lambda λ Atom quote
                        cons car cdr Sigma Σ Pair
                        Trivial sole
                        List |::| nil ind-List rec-List
                        Absurd ind-Absurd
                        = same replace symm trans cong ind-=
                        head tail Vec |vec::| vecnil ind-Vec
                        Either left right ind-Either
                        TODO)
      (U
        (make-U stx))
      (Nat
        (make-Nat stx))
      ((→ ~! A B C ...)
        (make--> stx
                 (parse-pie A)
                 (parse-pie B)
                 (map parse-pie C)))
      ((-> ~! A B C ...)
        (make--> stx
                 (parse-pie A)
                 (parse-pie B)
                 (map parse-pie C)))
      (zero
        (make-zero stx))
      ((add1 ~! n)
        (make-add1 stx (parse-pie n)))
      ((lambda ~! ((~describe "argument name" x0:pie-id)
                   (~describe "argument name" x:pie-id)
                   ...)
         b)
        (make-lambda stx
                     (map binding-site (cons x0 x))
                     (parse-pie b)))
      ((λ ~! ((~describe "argument name" x0:pie-id)
              (~describe "argument name" x:pie-id)
              ...)
         b)
        (make-lambda stx
                     (map binding-site (cons x0 x))
                     (parse-pie b)))
      ((Pi ~! more ...)
        (parse-pie (template-Π 282 18 6228 more)))
      ((∏ ~! more ...)
        (parse-pie (template-Π 284 18 6282 more)))
      ((Π ~! (~describe "argument names and types"
                        ((x0:pie-id A0) (x:pie-id A) ...))
          (~describe "result type" B))
        (make-Pi stx
                 (typed-binders (list (binding-site x0) (parse-pie A0))
                                (map (λ (x A)
                                       (list (binding-site x)
                                             (parse-pie A)))
                                     x A))
                 (parse-pie B)))
      ((Sigma ~! more ...)
        (parse-pie (template/loc-Σ stx more)))
      ((Σ ~! (~describe "car names and types"
                        ((x0:pie-id A0) (x:pie-id A) ...))
          (~describe "cdr type" B))
        (make-Sigma stx
                    (typed-binders (list (binding-site x0) (parse-pie A0))
                                   (map (λ (x A)
                                          (list (binding-site x)
                                                (parse-pie A)))
                                        x A))
                    (parse-pie B)))
      ((Pair ~! A D)
        (make-Pair stx (parse-pie A) (parse-pie D)))
      ((cons ~! a d)
        (make-cons stx (parse-pie a) (parse-pie d)))
      ((car ~! p)
        (make-car stx (parse-pie p)))
      ((cdr ~! p)
        (make-cdr stx (parse-pie p)))
      ((the ~! t e)
        (make-the stx (parse-pie t) (parse-pie e)))
      ((which-Nat ~! tgt b s)
        (make-which-Nat stx (parse-pie tgt) (parse-pie b) (parse-pie s)))
      ((rec-Nat ~! tgt b s)
        (make-rec-Nat stx (parse-pie tgt) (parse-pie b) (parse-pie s)))
      ((iter-Nat ~! tgt b s)
        (make-iter-Nat stx (parse-pie tgt) (parse-pie b) (parse-pie s)))
      ((ind-Nat ~! tgt mot b s)
        (make-ind-Nat stx (parse-pie tgt) (parse-pie mot) (parse-pie b) (parse-pie s)))
      (Atom
        (make-Atom stx))
      ((quote ~! a:id)
        (make-quote stx (syntax->datum a)))
      (Trivial
        (make-Trivial stx))
      (sole
        (make-sole stx))
      ((List ~! E)
        (make-List stx (parse-pie E)))
      (nil
        (make-nil stx))
      ((|::| ~! e es)
        (|make-::| stx (parse-pie e) (parse-pie es)))
      ((ind-List ~! tgt mot b s)
        (make-ind-List stx (parse-pie tgt) (parse-pie mot) (parse-pie b) (parse-pie s)))
      ((rec-List ~! tgt b s)
        (make-rec-List stx (parse-pie tgt) (parse-pie b) (parse-pie s)))
      (x:pie-id
        (make-var-ref stx (syntax->datum x)))
      (Absurd
        (make-Absurd stx))
      ((ind-Absurd ~! tgt mot)
        (make-ind-Absurd stx (parse-pie tgt) (parse-pie mot)))
      (n:nat
        (make-nat-literal stx (syntax->datum n)))
      ((= ~! A from to)
        (make-= stx (parse-pie A) (parse-pie from) (parse-pie to)))
      ((same ~! e)
        (make-same stx (parse-pie e)))
      ((replace ~! tgt mot b)
        (make-replace stx (parse-pie tgt) (parse-pie mot) (parse-pie b)))
      ((trans ~! p1 p2)
        (make-trans stx (parse-pie p1) (parse-pie p2)))
      ((cong ~! p1 p2)
        (make-cong stx (parse-pie p1) (parse-pie p2)))
      ((ind-= ~! (~describe "target" tgt) (~describe "motive" mot) (~describe "base" base))
        (make-ind-= stx (parse-pie tgt) (parse-pie mot) (parse-pie base)))
      ((symm ~! p)
        (make-symm stx (parse-pie p)))
      ((Vec ~! E len)
        (make-Vec stx (parse-pie E) (parse-pie len)))
      (vecnil
        (make-vecnil stx))
      ((|vec::| ~! h t)
        (|make-vec::| stx (parse-pie h) (parse-pie t)))
      ((head ~! es)
        (make-head stx (parse-pie es)))
      ((tail ~! es)
        (make-tail stx (parse-pie es)))
      ((ind-Vec ~! k xs m b s)
        (make-ind-Vec stx (parse-pie k) (parse-pie xs) (parse-pie m) (parse-pie b) (parse-pie s)))
      ((Either ~! L R)
        (make-Either stx (parse-pie L) (parse-pie R)))
      ((left ~! l)
        (make-left stx (parse-pie l)))
      ((right ~! r)
        (make-right stx (parse-pie r)))
      ((ind-Either ~! tgt mot l r)
        (make-ind-Either stx (parse-pie tgt) (parse-pie mot) (parse-pie l) (parse-pie r)))
      ((~describe "TODO" TODO)
        (make-TODO stx))
      ((~describe "function application"
                  ((~describe "function" rator)
                   (~describe "first argument" rand0)
                   (~describe "more arguments" rand) ...))
        (make-ap stx (parse-pie rator) (parse-pie rand0) (map parse-pie rand)))))

  ;; parse-pie-decl : Syntax -> Decl, where a Decl is one of
  ;;   (claim Symbol Loc Src)
  ;;   (definition Symbol Loc Src)
  ;;   (check-same Loc Src Src Src)
  ;;   (expression Src)
  ;; The descriptions of the two expressions of check-same are computed from its type, cf. parser.rkt:395
  (define (parse-pie-decl stx)
    (syntax-parse stx
      #:datum-literals (claim define check-same)
      ((~describe "claim"
                  (claim ~!
                         (~describe "name" x:pie-id)
                         (~describe "type" type)))
        `(claim ,(syntax->datum x)
                ,(syntax->srcloc x)
                ,(parse-pie type)))
      ((~describe "definition"
                  (define ~!
                    (~describe "name" x:pie-id)
                    (~describe "definiens" e)))
        `(definition
           ,(syntax->datum x)
           ,(syntax->srcloc x)
           ,(parse-pie e)))
      ((~describe "sameness check"
                  (check-same ~!
                              (~describe "type" type)
                              (~describe (format "first ~a" (syntax->datum type)) e1)
                              (~describe (format "second ~a" (syntax->datum type)) e2)))
        `(check-same ,(syntax->srcloc stx)
                     ,(parse-pie type)
                     ,(parse-pie e1)
                     ,(parse-pie e2)))
      ((~describe "expression" e)
        `(expression ,(parse-pie e)))))

  ;;; ==================== BINDERS ====================

  ;; binders-of : Syntax -> Form, #'(pie->binders e), a use of the macro on e that the expansion meets later
  (define (binders-of e)
    `(pie->binders ,e))

  ;; pie->binders : Syntax -> Form, one step of the expansion of (pie->binders stx), cf. parser.rkt:433.
  ;; A keyword becomes the kw: keyword of pie-info.rkt, which expands into (void) and its arguments into
  ;; (void arg ...); the names of λ, Π and Σ are bound by a let*, a variable stays the user's identifier and
  ;; a number goes inside (void n) as it was written, so its #%datum is the one in the scope of the user's
  ;; names. The input has already been parsed by parse-pie, so no clause fails and the last one is never
  ;; reached by a Pie program.
  (define (pie->binders stx)
    (syntax-parse stx
      #:datum-literals (U
                        Nat zero add1
                        which-Nat iter-Nat rec-Nat ind-Nat
                        → -> Pi Π ∏ the lambda λ Atom quote
                        cons car cdr Sigma Σ Pair
                        Trivial sole
                        List |::| nil ind-List rec-List
                        Absurd ind-Absurd
                        = same replace symm trans cong ind-=
                        head tail Vec |vec::| vecnil ind-Vec
                        Either left right ind-Either
                        TODO)
      (U
        'kw:U)
      (Nat
        'kw:Nat)
      ((→ ~! A B C ...)
        `(kw:→ ,(binders-of A) ,(binders-of B) ,@(map binders-of C)))
      ((-> ~! A B C ...)
        `(kw:-> ,(binders-of A) ,(binders-of B) ,@(map binders-of C)))
      (zero
        'kw:zero)
      ((add1 ~! n)
        `(kw:add1 ,(binders-of n)))
      ((lambda ~! (x0:id x:id ...) b)
        `(void kw:lambda (let* ((,x0 (void)) ,@(map (λ (x) `(,x (void))) x))
                           ,(binders-of b))))
      ((λ ~! (x0:id x:id ...) b)
        `(void kw:λ (let* ((,x0 (void)) ,@(map (λ (x) `(,x (void))) x))
                      ,(binders-of b))))
      ((Pi ~! ((x0:id A0) (x:id A) ...) B)
        `(void kw:Pi ,(typed-let* x0 A0 x A B)))
      ((Π ~! ((x0:id A0) (x:id A) ...) B)
        `(void kw:Π ,(typed-let* x0 A0 x A B)))
      ((∏ ~! ((x0:id A0) (x:id A) ...) B)
        `(void kw:∏ ,(typed-let* x0 A0 x A B)))
      ((Sigma ~! ((x0:id A0) (x:id A) ...) B)
        `(void kw:Sigma ,(typed-let* x0 A0 x A B)))
      ((Σ ~! ((x0:id A0) (x:id A) ...) B)
        `(void kw:Σ ,(typed-let* x0 A0 x A B)))
      ((Pair ~! A D)
        `(void kw:Pair ,(binders-of A) ,(binders-of D)))
      ((cons ~! a d)
        `(void kw:cons ,(binders-of a) ,(binders-of d)))
      ((car ~! p)
        `(void kw:car ,(binders-of p)))
      ((cdr ~! p)
        `(void kw:cdr ,(binders-of p)))
      ((the ~! t e)
        `(void kw:the ,(binders-of t) ,(binders-of e)))
      ((which-Nat ~! tgt b s)
        `(void kw:which-Nat ,(binders-of tgt) ,(binders-of b) ,(binders-of s)))
      ((rec-Nat ~! tgt b s)
        `(void kw:rec-Nat ,(binders-of tgt) ,(binders-of b) ,(binders-of s)))
      ((iter-Nat ~! tgt b s)
        `(void kw:iter-Nat ,(binders-of tgt) ,(binders-of b) ,(binders-of s)))
      ((ind-Nat ~! tgt mot b s)
        `(void kw:ind-Nat ,(binders-of tgt) ,(binders-of mot) ,(binders-of b) ,(binders-of s)))
      (Atom
        'kw:Atom)
      ((quote ~! a:id)
        `(void kw:quote (quote ,a)))
      (Trivial
        'kw:Trivial)
      (sole
        'kw:sole)
      ((List ~! E)
        `(void kw:List ,(binders-of E)))
      (nil
        'kw:nil)
      ((|::| ~! a d)
        `(void |kw:::| ,(binders-of a) ,(binders-of d)))
      ((ind-List ~! tgt mot b s)
        `(void kw:ind-List ,(binders-of tgt) ,(binders-of mot) ,(binders-of b) ,(binders-of s)))
      ((rec-List ~! tgt b s)
        `(void kw:rec-List ,(binders-of tgt) ,(binders-of b) ,(binders-of s)))
      (x:pie-id
        stx)
      (Absurd
        'kw:Absurd)
      ((ind-Absurd ~! tgt mot)
        `(void kw:ind-Absurd ,(binders-of tgt) ,(binders-of mot)))
      (n:nat
        `(void ,n))
      ((= ~! A from to)
        `(void kw:= ,(binders-of A) ,(binders-of from) ,(binders-of to)))
      ((same ~! e)
        `(void kw:same ,(binders-of e)))
      ((replace ~! tgt mot b)
        `(void kw:replace ,(binders-of tgt) ,(binders-of mot) ,(binders-of b)))
      ((trans ~! a d)
        `(void kw:trans ,(binders-of a) ,(binders-of d)))
      ((cong ~! a d)
        `(void kw:cong ,(binders-of a) ,(binders-of d)))
      ((symm ~! p)
        `(void kw:symm ,(binders-of p)))
      ((ind-= ~! tgt mot base)
        `(void kw:ind-= ,(binders-of tgt) ,(binders-of mot) ,(binders-of base)))
      ((Vec ~! E len)
        `(void kw:Vec ,(binders-of E) ,(binders-of len)))
      (vecnil
        'kw:vecnil)
      ((|vec::| ~! a d)
        `(void |kw:vec::| ,(binders-of a) ,(binders-of d)))
      ((head ~! p)
        `(void kw:head ,(binders-of p)))
      ((tail ~! p)
        `(void kw:tail ,(binders-of p)))
      ((ind-Vec ~! k tgt mot b s)
        `(void kw:ind-Vec ,(binders-of k) ,(binders-of tgt) ,(binders-of mot) ,(binders-of b) ,(binders-of s)))
      ((Either ~! L R)
        `(void kw:Either ,(binders-of L) ,(binders-of R)))
      ((left ~! p)
        `(void kw:left ,(binders-of p)))
      ((right ~! p)
        `(void kw:right ,(binders-of p)))
      ((ind-Either ~! tgt mot l r)
        `(void kw:ind-Either ,(binders-of tgt) ,(binders-of mot) ,(binders-of l) ,(binders-of r)))
      ((~describe "TODO" TODO)
        'kw:TODO)
      ((~describe "application" (rator rand0 rand ...))
        `(void ,(binders-of rator) ,(binders-of rand0) ,@(map binders-of rand)))
      (_ '(void))))

  ;; typed-let* : Identifier Syntax (Listof Identifier) (Listof Syntax) Syntax -> Form, the
  ;; (let* ([x0 A0*] [x A*] ...) B*) of the Π and Σ clauses, where each name is bound in the types after it
  ;; and in B, cf. parser.rkt:503-534
  (define (typed-let* x0 A0 xs As B)
    `(let* ((,x0 ,(binders-of A0)) ,@(map (λ (x A) `(,x ,(binders-of A))) xs As))
       ,(binders-of B)))

  ;; format-id : Identifier String Identifier [Syntax] -> Identifier, racket/syntax's format-id on one
  ;; identifier, the name that fmt makes of id's, with the lexical context of lctx and the location of
  ;; source, none without it, cf. parser.rkt:769 and 781
  (define (format-id lctx fmt id #!optional (source #f))
    (datum->syntax lctx (string->symbol (format fmt (syntax-e id))) source))

  ;; pie-decl->binders : Syntax -> Form, one step of the expansion of (pie-decl->binders stx), cf.
  ;; parser.rkt:757. A claim of x defines internal-claim-binding-x, located at x; a definition of x defines
  ;; x, using internal-claim-binding-x; a sameness check and an expression are expressions of the module.
  ;; The internal names are made in the context of x, so they are names of the user's.
  (define (pie-decl->binders stx)
    (syntax-parse stx
      #:datum-literals (claim define check-same)
      ((~describe "claim"
                  (claim ~!
                         (~describe "name" x:id)
                         (~describe "type" type)))
        `(define ,(format-id x "internal-claim-binding-~a" x x)
           (void kw:claim ,(binders-of type))))
      ((~describe "definition"
                  (define ~!
                    (~describe "name" x:id)
                    (~describe "definiens" e)))
        `(define ,x
           (void kw:define ,(format-id x "internal-claim-binding-~a" x) ,(binders-of e))))
      ((~describe "sameness check"
                  (check-same ~!
                              (~describe "type" type)
                              (~describe (format "first ~a" (syntax->datum type)) e1)
                              (~describe (format "second ~a" (syntax->datum type)) e2)))
        `(void kw:check-same ,(binders-of type) ,(binders-of e1) ,(binders-of e2)))
      ((~describe "expression" e)
        (binders-of e)))))
