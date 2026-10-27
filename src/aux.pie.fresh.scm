;;; (aux pie fresh) -- fresh names for the binders of Pie.
;;;
;;; A port of fresh.rkt of Pie, the language of "The Little Typer" by Daniel P. Friedman and David Thrane
;;; Christiansen (https://github.com/the-little-typer/pie, commit 2c89553). Pie is distributed under the
;;; GNU AGPL-3.0, and so is this file, which is derived from it.
;;;
;;; The file is ported whole and line for line, with the upstream names. Only `freshen` is exported, as
;;; upstream provides only it (cf. fresh.rkt:4). The module+ test block of fresh.rkt:107-126 lives in the
;;; pie-fresh-suite of src/test/pie-units.scm.
;;;
;;; A name is split into a base string and a number written with the subscript digits ₀ … ₉ at its end, so
;;; x₁₂ splits into ("x" . 12) and r2d into ("r2d" . 1), because a name without a subscript counts as
;;; numbered 1. The upstream quirks are kept on purpose, and they are
;;;   - split-name-aux converts the substring that starts AT the last non-subscript character, so x₁ gives
;;;     ("x" . 1) only because (string->number "x1") is #f and safe-string->number answers 1 then, while
;;;     a1₂ gives ("a1" . 12) and a.₅ gives ("a." . 0.5), which unsplits as a.₀.₅;
;;;   - a name made of subscripts only, such as ₉₉, gets the base "x";
;;;   - the empty symbol raises an error from string-ref when it has to be freshened, since split-name-aux
;;;     starts at index -1. It is Racket's contract violation, through racket-string-ref of (aux pie racket),
;;;     so that `racket FILE.pie` reports it as Racket does.
;;; CHICKEN 6 has the full numeric tower, so the numbers read from odd names are those of Racket. They print
;;; back with racket-number->string of (aux pie racket), because CHICKEN's number->string writes a flonum with
;;; 15 significant digits and pads its exponent, so d.₀.₁₂₃₄₅₆₇₈₉₁₂₃₄₅₆₇₈ and c.₁e-₇ would come back changed.
;;;
;;; Typed Racket signatures are kept as comments; Typed Racket only checks them, so nothing at run time
;;; depends on them.

(module (aux pie fresh) (freshen)

  (import scheme
          (chicken base)
          (only (chicken string) string-translate*)
          (aux base)
          (only (aux pie racket) racket-number->string racket-string-ref))

  ;;; ==================== FRESHEN ====================

  ;; freshen : (Listof Symbol) Symbol -> Symbol, cf. fresh.rkt:6
  ;; x itself when x is not used, otherwise the first numbered variant of x that is not used.
  (define (freshen used x)
    (if (memv x used)
        (let1 (split (split-name x))
          (freshen-aux used split))
        x))

  ;; freshen-aux : (Listof Symbol) (Pair String Number) -> Symbol, cf. fresh.rkt:13
  (define (freshen-aux used split)
    (let1 (joined (unsplit-name split))
      (if (memv joined used)
          (freshen-aux used (next-split-name split))
          joined)))

  ;; next-split-name : (Pair String Number) -> (Pair String Number), cf. fresh.rkt:20
  (define (next-split-name split)
    (cons (car split) (add1 (cdr split))))

  ;; unsplit-name : (Pair String Number) -> Symbol, cf. fresh.rkt:24
  (define (unsplit-name split)
    (string->symbol
     (string-append (car split) (number->subscript-string (cdr split)))))

  ;;; ==================== SUBSCRIPTS ====================

  ;; string-replace : String String String -> String, the racket/string procedure that fresh.rkt:3 requires.
  ;; It replaces every occurrence of from in str by to. Here from is always one character, so CHICKEN's
  ;; string-translate* with a single pair gives the same string.
  (define (string-replace str from to)
    (string-translate* str (list (cons from to))))

  ;; string-replace* : String (Listof (Pair String String)) -> String, cf. fresh.rkt:29
  ;; The replacements apply one after the other, from the first pair to the last.
  (define (string-replace* str replacements)
    (cond
      ((null? replacements) str)
      (else
       (let ((from (car (car replacements)))
             (to (cdr (car replacements))))
         (string-replace* (string-replace str from to)
                          (cdr replacements))))))

  ;; subscript-replacements : (Listof (Pair String String)), cf. fresh.rkt:39
  (define subscript-replacements
    '(("0" . "₀")
      ("1" . "₁")
      ("2" . "₂")
      ("3" . "₃")
      ("4" . "₄")
      ("5" . "₅")
      ("6" . "₆")
      ("7" . "₇")
      ("8" . "₈")
      ("9" . "₉")))

  ;; non-subscripts : (Listof (Pair String String)), cf. fresh.rkt:52
  (define non-subscripts
    '(("₀" . "0")
      ("₁" . "1")
      ("₂" . "2")
      ("₃" . "3")
      ("₄" . "4")
      ("₅" . "5")
      ("₆" . "6")
      ("₇" . "7")
      ("₈" . "8")
      ("₉" . "9")))

  ;; subscript-digit? : Char -> Boolean, cf. fresh.rkt:66
  (define (subscript-digit? c)
    (if (assoc (string c) non-subscripts)
        #t
        #f))

  ;; subscript->number : String -> Number, cf. fresh.rkt:72
  (define (subscript->number str)
    (safe-string->number (string-replace* str non-subscripts)))

  ;; number->subscript-string : Number -> String, cf. fresh.rkt:76
  ;; Racket's number->string is racket-number->string.
  (define (number->subscript-string n)
    (string-replace* (racket-number->string n) subscript-replacements))

  ;;; ==================== SPLITTING NAMES ====================

  ;; split-name : Symbol -> (Pair String Number), cf. fresh.rkt:80
  (define (split-name name)
    (let1 (str (symbol->string name))
      (split-name-aux str (sub1 (string-length str)))))

  ;; split-name-aux : String Integer -> (Pair String Number), cf. fresh.rkt:85
  ;; It walks back from index i over the subscript digits. The last branch converts the substring that
  ;; starts at the non-subscript character i, not after it, as upstream does. Racket's string-ref is
  ;; racket-string-ref.
  (define (split-name-aux str i)
    (cond
      ((zero? i)
       (cond
         ((subscript-digit? (racket-string-ref str i))
          (cons "x" (subscript->number str)))
         (else (cons (string (racket-string-ref str i))
                     (subscript->number (substring str i (string-length str)))))))
      ((subscript-digit? (racket-string-ref str i))
       (split-name-aux str (sub1 i)))
      (else (cons
             (substring str 0 (add1 i))
             (subscript->number (substring str i (string-length str)))))))

  ;; safe-string->number : String -> Number, cf. fresh.rkt:100
  ;; 1 when str does not read as a number.
  (define (safe-string->number str)
    (let1 (num (string->number str))
      (if (eqv? num #f)
          1
          num))))
