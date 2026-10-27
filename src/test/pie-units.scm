;;; The unit tests of (aux pie), the port of Pie, the language of "The Little Typer" by Daniel P. Friedman
;;; and David Thrane Christiansen (https://github.com/the-little-typer/pie, commit 2c89553). Pie is
;;; distributed under the GNU AGPL-3.0, and so is this file, which is derived from it.
;;;
;;; Three suites port the module+ test blocks of upstream, one case per check and in upstream order.
;;;   - pie-fresh-suite, the 10 checks of fresh.rkt:107-126.
;;;   - pie-alpha-suite, the 27 checks of alpha.rkt:100-141.
;;;   - pie-atom-suite, the 11 checks of typechecker.rkt:874-889.
;;; check-eqv?, check-true and check-false become (⊦= expected actual), (⊨ actual) and (⊭ actual).
;;;
;;; pie-substrate-suite tests what the port adds under Pie, the modules (aux pie racket) and (aux pie
;;; syntax) that stand for Racket itself, and the dump and restore of (aux pie serialization). Its expected
;;; values were printed by Racket 8.15 for the same inputs, that is the locations that read-syntax gives on
;;; a port with line counting, Racket's read-error and #lang messages with the hints that the indentation
;;; gives, the UTF-8 decoding of bytes->string/utf-8, what write prints with print-reader-abbreviations off,
;;; string-split, ~a padding, printf and datum->syntax, the values that the reader makes for byte strings,
;;; regexps, hash tables (in the order of Racket CS's tables), prefab structs and extflonums, number->string
;;; on flonums and char-alphabetic? of Unicode 15.1.

(import scheme
        (chicken base)
        (only (chicken condition) condition-case get-condition-property)
        (only (chicken port) with-output-to-string)
        (only (chicken keyword) string->keyword)
        (aux base)
        (aux unittest)
        (only (aux pie fresh) freshen)
        (only (scheme base) bytevector)
        (only (aux pie racket) define-struct box box? unbox racket-write->string racket-string-split
              racket-pad-left racket-printf racket-exn? racket-exn-message racket-number->string
              racket-char-alphabetic? racket-bytes->string/utf-8)
        (only (aux pie syntax) syntax? syntax-e syntax-line syntax-column syntax-position syntax-span
              syntax-srcloc syntax->datum syntax->list datum->syntax open-syntax-input read-syntax
              read-language string->syntax)
        (only (aux pie locations) syntax->location location->srcloc)
        (only (aux pie alpha) α-equiv?)
        (only (aux pie typechecker) atom-ok?)
        (only (aux pie serialization) dump restore))

;;; ==================== HELPERS ====================

; string->stx : string -> syntax, the datum that read-syntax reads from s, with source "t".
(define (string->stx s) (string->syntax s "t"))

; stx-locations : syntax -> tree, the (line column position span) of a syntax object and, for a list,
; of each element in turn, a dotted tail coming after the symbol |.|, as the Racket probe prints them.
(define (stx-locations x)
  (let1 (here (list (syntax-line x) (syntax-column x) (syntax-position x) (syntax-span x)))
    (if (pair? (syntax-e x))
      (cons here (let loop ((e (syntax-e x)))
                   (cond
                     ((null? e) '())
                     ((pair? e) (cons (stx-locations (car e)) (loop (cdr e))))
                     ((syntax? e) (list '|.| (stx-locations e)))
                     (else '()))))
      here)))

; read-error : string -> string, the message of the read error that reading s raises, or 'no-error.
(define (read-error s)
  (condition-case (begin (string->stx s) 'no-error)
    (e (exn) (if (racket-exn? e 'exn:fail:read) (racket-exn-message e) (raise e)))))

; language : string -> symbol, what read-language answers for a port over s named "string", or the
; message of the read error it raises.
(define (language s)
  (condition-case (read-language (open-syntax-input "string" s))
    (e (exn) (if (racket-exn? e 'exn:fail:read) (racket-exn-message e) (raise e)))))

; Two structs, for the tests of define-struct.
(define-struct point x y)
(define-struct segment from to)

;;; ==================== FRESH ====================

(define-suite pie-fresh-suite

  ((doc r) `((p "The checks of fresh.rkt:107-126 on freshen of (aux pie fresh).")))

  ((test/L109/x _) (⊦= 'x₁ (freshen '(x) 'x)))                     ; cf. fresh.rkt:109
  ((test/L111/x-x1-x2 _) (⊦= 'x₃ (freshen '(x x₁ x₂) 'x)))         ; cf. fresh.rkt:111
  ((test/L113/unused _) (⊦= 'y (freshen '(x x1 x2) 'y)))           ; cf. fresh.rkt:113
  ((test/L115/r2d _) (⊦= 'r2d₂ (freshen '(r2d r2d₀ r2d₁) 'r2d)))   ; cf. fresh.rkt:115
  ((test/L117/empty _) (⊦= 'A (freshen '() 'A)))                   ; cf. fresh.rkt:117
  ((test/L119/x1 _) (⊦= 'x₂ (freshen '(x₁) 'x₁)))                  ; cf. fresh.rkt:119
  ((test/L120/x1-unused _) (⊦= 'x₁ (freshen '() 'x₁)))             ; cf. fresh.rkt:120

  ((test/L121/subscript-only _)                                    ; cf. fresh.rkt:121
   (⊦= (string->symbol "₉₉") (freshen '() (string->symbol "₉₉"))))

  ((test/L123/subscript-only-used _)                               ; cf. fresh.rkt:123
   (⊦= 'x₉₉ (freshen (list (string->symbol "₉₉")) (string->symbol "₉₉"))))

  ((test/L125/subscript-only-x99-used _)                           ; cf. fresh.rkt:125
   (⊦= 'x₁₀₀ (freshen (list (string->symbol "₉₉") 'x₉₉) (string->symbol "₉₉")))))

;;; ==================== ALPHA ====================

(define here (location->srcloc (syntax->location (string->syntax "here"))))
(define there (location->srcloc (syntax->location (string->syntax "  there"))))

(define-suite pie-alpha-suite

  ((doc r) `((p "The checks of alpha.rkt:100-141 on α-equiv? of (aux pie alpha).")))

  ((test/L102/id _) (⊨ (α-equiv? '(λ (x) x) '(λ (x) x))))                               ; cf. alpha.rkt:102
  ((test/L103/id-renamed _) (⊨ (α-equiv? '(λ (x) x) '(λ (y) y))))                       ; cf. alpha.rkt:103
  ((test/L104/const _) (⊨ (α-equiv? '(λ (x) (λ (y) x)) '(λ (x) (λ (y) x)))))            ; cf. alpha.rkt:104
  ((test/L105/const-renamed _) (⊨ (α-equiv? '(λ (x) (λ (y) x)) '(λ (y) (λ (z) y)))))    ; cf. alpha.rkt:105
  ((test/L106/const-vs-inner _) (⊭ (α-equiv? '(λ (x) (λ (y) x)) '(λ (y) (λ (z) z)))))   ; cf. alpha.rkt:106
  ((test/L107/const-vs-shadow _) (⊭ (α-equiv? '(λ (x) (λ (y) x)) '(λ (y) (λ (x) x)))))  ; cf. alpha.rkt:107
  ((test/L108/bound-vs-free _) (⊭ (α-equiv? '(λ (x) x) '(λ (y) x))))                    ; cf. alpha.rkt:108

  ((test/L110/free _) (⊨ (α-equiv? 'x 'x)))                                             ; cf. alpha.rkt:110
  ((test/L111/free-differ _) (⊭ (α-equiv? 'x 'y)))                                      ; cf. alpha.rkt:111

  ((test/L113/app _) (⊨ (α-equiv? '(f x) '(f x))))                                      ; cf. alpha.rkt:113
  ((test/L114/app-differ _) (⊭ (α-equiv? '(f x) '(g x))))                               ; cf. alpha.rkt:114
  ((test/L115/app-bound _) (⊨ (α-equiv? '(λ (f) (f x)) '(λ (g) (g x)))))                ; cf. alpha.rkt:115

  ((test/L117/zero _) (⊨ (α-equiv? 'zero 'zero)))                                       ; cf. alpha.rkt:117
  ((test/L118/add1 _) (⊨ (α-equiv? '(add1 zero) '(add1 zero))))                         ; cf. alpha.rkt:118

  ((test/L120/quote _) (⊨ (α-equiv? ''rugbrød ''rugbrød)))                              ; cf. alpha.rkt:120
  ((test/L121/quote-differ _) (⊭ (α-equiv? ''rugbrød ''rundstykker)))                   ; cf. alpha.rkt:121

  ((test/L123/sigma _)                                                                  ; cf. alpha.rkt:123
   (⊨ (α-equiv? '(Σ ((half Nat)) (= Nat n (double half)))
                '(Σ ((blurgh Nat)) (= Nat n (double blurgh))))))
  ((test/L125/sigma-differ _)                                                           ; cf. alpha.rkt:125
   (⊭ (α-equiv? '(Σ ((half Nat)) (= Nat n (double half)))
                '(Σ ((half Nat)) (= Nat n (twice half))))))

  ((test/L128/absurd _) (⊨ (α-equiv? '(the Absurd x) '(the Absurd x))))                 ; cf. alpha.rkt:128
  ((test/L129/absurd-eta _) (⊨ (α-equiv? '(the Absurd x) '(the Absurd y))))             ; cf. alpha.rkt:129
  ((test/L130/absurd-eta-app _)                                                         ; cf. alpha.rkt:130
   (⊨ (α-equiv? '(the Absurd x) '(the Absurd (find-the-proof x)))))

  ((test/L134/todo _) (⊨ (α-equiv? `(TODO ,here Nat) `(TODO ,here Nat))))               ; cf. alpha.rkt:134
  ((test/L135/todo-differ _) (⊭ (α-equiv? `(TODO ,here Nat) `(TODO ,there Nat))))       ; cf. alpha.rkt:135

  ((test/L137/keyword-vs-free _) (⊭ (α-equiv? 'zero 'naught)))                          ; cf. alpha.rkt:137

  ((test/L139/pi _) (⊨ (α-equiv? '(Π ((n Nat)) (= Nat n n)) '(Π ((m Nat)) (= Nat m m)))))       ; cf. alpha.rkt:139
  ((test/L140/pi-free _) (⊭ (α-equiv? '(Π ((n Nat)) (= Nat n n)) '(Π ((m Nat)) (= Nat n n)))))  ; cf. alpha.rkt:140
  ((test/L141/pi-vs-sigma _) (⊭ (α-equiv? '(Π ((n Nat)) (= Nat n n)) '(Σ ((m Nat)) (= Nat m m))))))  ; cf. alpha.rkt:141

;;; ==================== ATOMS ====================

(define-suite pie-atom-suite

  ((doc r) `((p "The checks of typechecker.rkt:874-889 on atom-ok? of (aux pie typechecker).")))

  ((test/L877/food _) (⊨ (atom-ok? 'food)))              ; cf. typechecker.rkt:877
  ((test/L878/dashes _) (⊨ (atom-ok? 'food---)))         ; cf. typechecker.rkt:878
  ((test/L879/oe _) (⊨ (atom-ok? 'œ)))                   ; cf. typechecker.rkt:879
  ((test/L880/rugbrod _) (⊨ (atom-ok? 'rugbrød)))        ; cf. typechecker.rkt:880
  ((test/L881/devanagari _) (⊨ (atom-ok? 'देवनागरी)))     ; cf. typechecker.rkt:881
  ((test/L882/nihongo _) (⊨ (atom-ok? '日本語)))          ; cf. typechecker.rkt:882
  ((test/L883/atOm _) (⊨ (atom-ok? 'atØm)))              ; cf. typechecker.rkt:883
  ((test/L884/lambda _) (⊨ (atom-ok? 'λ)))               ; cf. typechecker.rkt:884
  ((test/L885/lambda-word _) (⊨ (atom-ok? 'λάμβδα)))     ; cf. typechecker.rkt:885
  ((test/L888/digit _) (⊭ (atom-ok? 'at0m)))             ; cf. typechecker.rkt:888
  ((test/L889/canoe _) (⊭ (atom-ok? '🛶))))              ; cf. typechecker.rkt:889

;;; ==================== SUBSTRATE ====================

(define-suite pie-substrate-suite

  ((doc r) `((p "The Racket behaviour that the port rebuilds under Pie, compared with what Racket 8.15 "
                "prints for the same inputs: the reader with its locations and errors, #lang, write, "
                "string-split, ~a, printf, datum->syntax, structs and the dump of contexts.")))

  ((test/reader/positions _)
   ; a tab moves the column to the next multiple of 8, and CRLF is one line break and one position.
   (⊦= '((1 0 1 10) (1 1 2 1) (1 8 4 1) (2 0 6 1) (2 3 9 1))
       (stx-locations (string->stx "(a\tb\r\nc  d)"))))

  ((test/reader/quote _)
   ; the quote symbol gets the location of the prefix, and the list spans both.
   (⊦= '((2 2 4 2) (2 2 4 1) (2 3 5 1)) (stx-locations (string->stx "\n  'x")))
   (⊦= '((1 0 1 7) (1 0 1 2) ((1 2 3 5) (1 3 4 1) (1 5 6 1))) (stx-locations (string->stx "#'(f x)")))
   (⊦= '(syntax (f x)) (syntax->datum (string->stx "#'(f x)"))))

  ((test/reader/dotted _)
   ; a dotted tail stays a syntax object, and syntax->list flattens it; infix dot moves the operator.
   (⊦= '((1 0 1 8) (1 1 2 1) |.| (1 5 6 2)) (stx-locations (string->stx "(1 . ())")))
   (⊨ (syntax? (cdr (syntax-e (string->stx "(1 . ())")))))
   (⊦= '(1) (map syntax->datum (syntax->list (string->stx "(1 . ())"))))
   (⊦= '(add1 1 2) (syntax->datum (string->stx "(1 . add1 . 2)"))))

  ((test/reader/keywords-and-boxes _)
   (⊦= (list 'the 'Nat (string->keyword "kw")) (syntax->datum (string->stx "(the Nat #:kw)")))
   (let1 (b (syntax-e (string->stx "#&3")))
     (⊦= '(#t #t 3) (list (box? b) (syntax? (unbox b)) (syntax->datum (unbox b))))))

  ((test/reader/data _)
   ; Racket's numbers, and symbols that CHICKEN's own reader would read otherwise.
   (⊦= (list 16 1 3/2 10.0 1.0 1+2i 1/2 '|::| '|vec::| '|a b| 'x₁)
       (map (o syntax->datum string->stx) '("#x10" "+1" "#e1.5" "1#" "1f0" "1+2i" "1/2" "::" "vec::" "|a b|" "x₁"))))

  ((test/reader/errors _)
   (⊦= '("t:1:0: read-syntax: expected a `)` to close `(`"
         "t:1:2: read-syntax: expected `)` to close preceding `(`, found instead `]`"
         "t:1:0: read-syntax: unexpected `)`"
         "t:1:0: read-syntax: expected a closing `\"`"
         "t:1:0: read-syntax: bad syntax `#<`"
         "t:1:1: read-syntax: illegal use of `.`"
         "t:1:0: read-syntax: expected a `]` to close `[`"
         "t:1:0: read-syntax: expected a `}` to close `{`"
         "t:1:3: read-syntax: illegal use of `.`")
       (map read-error '("(a" "(a]" ")" "\"abc" "#<x>" "(. a)" "[a" "{a" "(a . b c)"))))

  ((test/reader/indentation _)
   ; the opener of a wrong closer is on its line when the sequence spans lines and "preceding" otherwise,
   ; the closer is "missing" when an enclosing sequence expects the one found, and a line less indented
   ; than the lines before it is the hint, which a closer where a datum should start gets too. The datum
   ; of a #; between the elements counts for the enclosing sequence, the keys and values of a hash table
   ; count for the table, and U+FEFF is whitespace.
   (⊦= '("t:2:0: read-syntax: expected `)` to close preceding `(`, found instead `]`"
         "t:1:0: read-syntax: expected a `)` to close `(`"
         "t:1:3: read-syntax: missing `)` to close preceding `(`, found instead `]`"
         "t:4:5: read-syntax: expected `)` to close `(` on line 2, found instead `]`"
         "t:3:0: read-syntax: unexpected `)`\n  possible cause: indentation suggests a missing `)` before line 2"
         "t:1:5: read-syntax: expected `)` to close preceding `(`, found instead `]`"
         "t:3:6: read-syntax: expected `)` to close `(` on line 1, found instead `]`\n  possible cause: indentation suggests a missing `)` before line 2"
         "t:2:2: read-syntax: expected `)` to close `(` on line 1, found instead `]`\n  possible cause: indentation suggests a missing `)` before line 2"
         "t:1:0: read-syntax: expected a `)` to close `(`\n  possible cause: indentation suggests a missing `)` before line 3")
       (map read-error (list "(a\n]" "(the Nat\n  (add1\n0)" "[(a]" "(a\n  (b\n#;c\n    d]" "(a\nb '\n)" "(a . ]"
                             "#hash((a . 1)\n(b\n . 2) ]" (string-append "(a\n" (string #\xFEFF) "b]")
                             "(the Nat\n  0\n(add1\n  zero)"))))

  ((test/reader/utf-8 _)
   ; each byte where a UTF-8 encoding fails gives one U+FFFD, a cut, overlong or surrogate encoding too.
   (⊦= (list (string #\a #\xFFFD #\b) (string #\f #\xFFFD #\xFFFD #\space #\x) (make-string 3 #\xFFFD)
             (make-string 2 #\xFFFD) (make-string 3 #\xFFFD) (string #\x20AC))
       (map (λ (b) (racket-bytes->string/utf-8 b #\xFFFD))
            (list (bytevector 97 255 98) (bytevector 102 226 128 32 120) (bytevector 237 160 128) (bytevector 192 128)
                  (bytevector 240 159 152) (bytevector 226 130 172)))))

  ((test/reader/lang _)
   ; #lang needs one space and a name that ends the line or the file; #!pie works as #lang pie, and
   ; text that is not #lang gives #f.
   (⊦= '(pie
         "string:1:0: read-syntax: expected a single space after `#lang`"
         "string:1:0: read-syntax: expected only alphanumeric, `-`, `+`, `_`, or `/` characters for `#lang`, found `(`"
         pie #f #f)
       (map language '("#lang pie\n(x)" "#lang  pie" "#lang pie(the Nat 2)" "#!pie\n" "(x)" ""))))

  ((test/racket-write _)
   ; Racket's write with print-reader-abbreviations off. An extflonum such as 1t0 needs bars, but it is
   ; never part of a complex number, so 1t0+1i is a plain symbol.
   (⊦= '("::" "vec::" "|a b|" "a\\|b" "|1|" "|+i|" "#%x" "#:kw" "#&1" "\"s\\n\"" "#\\nul" "1t0+1i" "|1t0|"
         "(quote a)" "(1 . 2)" "#(1 x)" "||" "|.|" "|#x|" "a#b" "|a;b|")
       (map racket-write->string
            (list '|::| '|vec::| '|a b| (string->symbol "a|b") '|1| '|+i| '|#%x| (string->keyword "kw") (box 1)
                  "s\n" (integer->char 0) (string->symbol "1t0+1i") (string->symbol "1t0") ''a '(1 . 2)
                  (vector 1 'x) (string->symbol "") '|.| (string->symbol "#x") (string->symbol "a#b") '|a;b|))))

  ((test/reader/values _)
   ; the values that Racket's reader makes and the parser reports as bad syntax, as write prints them, with
   ; the keys of a hash table sorted when they are all symbols, a later pair replacing an earlier one with the
   ; same key, two list keys never eq?, a prefab key normalized, an extflonum without its radix prefix.
   (⊦= '("#\"by\\0tes\\377\\n\"" "#rx\"a+\"" "#px#\"a\\\\d\"" "#hash((a . 2) (m . 3) (z . 1))" "#hasheq((\"a\" . 2))"
         "#hasheq(((1) . 1) ((1) . 2))" "#hash((a . (x y)))" "#s((p q 1) 1 2)" "#s(p 1)" "1at0" "+inf.t")
       (map (λ (s) (racket-write->string (syntax->datum (string->stx s))))
            '("#\"by\\0tes\\377\\n\"" "#rx\"a+\"" "#px#\"a\\\\d\"" "#hash((z . 1) (a . 2) (m . 3))"
              "#hasheq((\"a\" . 1) (\"a\" . 2))" "#hasheq(((1) . 1) ((1) . 2))" "#hash([a . (x y)])"
              "#s((p 1 q 1) 1 2)" "#s[(p 1 #()) 1]" "#x1at0" "+inf.t"))))

  ((test/reader/hash-order _)
   ; keys of mixed kinds print in the order of Racket CS's HAMT, which follows their hash codes.
   (⊦= '("#hash((5.5 . e) (\"c\" . 3) (#\\d . 4) (1 . a) (b . 2))"
         "#hasheqv((#\\a . e) (x . d) (0 . c) (16 . a) (32 . b))"
         "#hash((#t . e) (#(3) . b) (() . d) (#\"x\" . f) (#&4 . c) ((1 2) . a))")
       (map (λ (s) (racket-write->string (syntax->datum (string->stx s))))
            '("#hash((1 . a) (b . 2) (\"c\" . 3) (#\\d . 4) (5.5 . e))"
              "#hasheqv((16 . a) (32 . b) (0 . c) (x . d) (#\\a . e))"
              "#hash(((1 2) . a) (#(3) . b) (#&4 . c) (() . d) (#t . e) (#\"x\" . f))"))))

  ((test/reader/value-errors _)
   ; Racket's regexp and prefab checks, byte strings, hash pairs and extflonums under a prefix.
   (⊦= '("t:1:0: read-syntax: missing closing parenthesis in pattern\n  pattern: \"(\""
         "t:1:0: read-syntax: `*`, `+`, or `{...}` operand could be empty\n  pattern: \"(a*)\\\\1*\""
         "t:1:0: read-syntax: lookbehind pattern does not match a bounded length\n  pattern: \"(?<=a+b)\""
         "t:1:0: read-syntax: character `λ` is out of range in byte string"
         "t:1:0: read-syntax: unknown escape sequence `\\u` in byte string"
         "t:1:9: read-syntax: expected `.` and value for hash"
         "t:1:0: read-syntax: expected `l` after `#hasha`"
         "t:1:0: read-syntax: cannot read mutable `#s` form as syntax"
         "t:1:0: read-syntax: mismatch between structure description and number of provided field values in `#s` form"
         "t:1:0: read-syntax: cannot convert extflonum to exact in `#e1t0`"
         "t:1:0: read-syntax: cannot combine extflonum `1t0` into a complex number"
         "t:1:0: read-syntax: misplaced `+` in `#x1t0+1i`")
       (map read-error '("#rx\"(\"" "#px\"(a*)\\\\1*\"" "#rx\"(?<=a+b)\"" "#\"λ\"" "#\"\\u41\"" "#hash((a 1))" "#hasha()"
                         "#s((p #(0)) 1)" "#s((p 3) 1 2)" "#e1t0" "#x1@1t0" "#x1t0+1i"))))

  ((test/racket-number->string _)
   ; the shortest digits that read back, an exponent without padding (fresh.rkt:76 prints them so).
   (⊦= '("0.12345678912345678" "1e-7" "1e+21" "0.1" "123.456" "5e-324" "1.7976931348623157e+308" "-2.5e-5")
       (map racket-number->string '(0.12345678912345678 1e-7 1e21 0.1 123.456 5e-324 1.7976931348623157e308 -2.5e-5)))
   (⊦= '|c.₁e-₇| (freshen '(|c.₀₀₀₀₀₀₁|) '|c.₀₀₀₀₀₀₁|)))

  ((test/racket-char-alphabetic? _)
   ; Unicode 15.1, where U+0363 and the Garay letter U+10D4E became Alphabetic only later, U+2B739 is the last
   ; letter of CJK Extension C there.
   (⊦= '(#t #t #f #f #t #f #f)
       (map (o racket-char-alphabetic? integer->char) '(#x61 #x3BB #x363 #x10D4E #x2B739 #x2B73A #x2D))))

  ((test/racket-string-split _)
   (⊦= '(("a" "" "b") ("" "a" "") ("" "") () ("abc"))
       (map (λ (s) (racket-string-split s "\n")) '("a\n\nb\n" "\n\na\n\n" "\n\n\n" "" "abc"))))

  ((test/racket-pad-left _)
   (⊦= '("   x" "long") (list (racket-pad-left "x" 4) (racket-pad-left "long" 2))))

  ((test/racket-printf _)
   ; ~n and ~% are newlines and ~~ is a tilde; any other directive raises before anything is printed.
   (⊦= "a\nb~c\n" (with-output-to-string (τ (racket-printf "a~nb~~c~%"))))
   (⊦= '("" raised)
       (let* ((r #f)
              (out (with-output-to-string (τ (set! r (condition-case (racket-printf "x~a") (e (exn) 'raised)))))))
         (list out r))))

  ((test/datum->syntax _)
   ; the location given to datum->syntax goes to the sub-parts too.
   (let1 (s (datum->syntax #f '(a b) (list "f" 1 2 3 4)))
     (⊦= '(("f" 1 2 3 4) (1 1)) (list (syntax-srcloc s) (map syntax-line (syntax->list s))))))

  ((test/define-struct _)
   ; instances compare structurally with equal?, as Racket's transparent structs, and match/first
   ; patterns name them by their unqualified tag. The pattern has no _, because the formal _ of the
   ; case binds it, and match/first then compares with that variable instead of ignoring the part.
   (⊨ (equal? (segment (point 1 2) (point 3 4)) (segment (point 1 2) (point 3 4))))
   (⊭ (equal? (point 1 2) (point 2 1)))
   (⊭ (equal? (point 1 2) (segment 1 2)))
   (⊦= '(1 2 #t #f) (list (point-x (point 1 2)) (point-y (point 1 2)) (point? (point 1 2)) (point? (segment 1 2))))
   (⊦= 4 (match/first (segment (point 1 2) (point 3 4)) (#(segment ,from #(point ,x ,y)) y))))

  ((test/dump-restore _)
   ; a serializable context with what CHICKEN's reader could get wrong, keywords made of colons,
   ; symbols that look like numbers and a TODO srcloc, comes back equal; anything else is not a context.
   (let1 (Γ `((|+i| (def Nat (add1 zero)))
              (xs (free (List Atom)))
              (v (free (Vec Nat (add1 zero))))
              (ys (def (List Nat) (|::| zero nil)))
              (w (def (Vec Nat (add1 zero)) (|vec::| zero vecnil)))
              (|a b| (claim (TODO ,here Nat)))))
     (⊦= Γ (restore (dump Γ))))
   (⊦= "Invalid deserialized context: 42"
       (condition-case (restore "42")
         (e (exn) (get-condition-property e 'exn 'message))))))

(unittest/✓ pie-fresh-suite)
(unittest/✓ pie-alpha-suite)
(unittest/✓ pie-atom-suite)
(unittest/✓ pie-substrate-suite)
