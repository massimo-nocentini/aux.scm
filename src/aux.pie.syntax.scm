;;; (aux pie syntax) -- Racket's syntax objects, and the part of Racket's reader that Pie programs reach.
;;;
;;; Pie, the language of "The Little Typer" by Daniel P. Friedman and David Thrane Christiansen
;;; (https://github.com/the-little-typer/pie, commit 2c89553), reads a program with Racket's `read-syntax`
;;; (the module reader of main.rkt:250) and works on the syntax objects it gets. The parser matches them with
;;; syntax-parse (parser.rkt:235-423), and every error and every TODO reports the file, line and column kept
;;; in them (parser.rkt:23-33, locations.rkt:19-23). This module has no upstream Pie file. It gives the port
;;; located syntax objects and a reader that builds them, because CHICKEN's reader neither keeps positions nor
;;; reads Racket's syntax (it reads `::` and `vec::` as keywords, for one). It holds no Pie code, but it
;;; belongs to the port and is distributed under the GNU AGPL-3.0 like the rest of it.
;;;
;;; ==================== SYNTAX OBJECTS ====================
;;; A syntax object is the struct (stx e source line column position span) and e is one of
;;;   - a symbol, a number, a string, a character, a boolean, a CHICKEN keyword (for Racket's #:kw) or '();
;;;   - a byte string, a regexp, a hash table whose values are syntax objects, a prefab struct whose fields
;;;     are syntax objects, or an extflonum, the values of the reader of (aux pie racket);
;;;   - a proper list of syntax objects;
;;;   - a chain of pairs of syntax objects whose last cdr is a syntax object, as the reader makes for the dotted
;;;     notation, which keeps its tail as one syntax object, so (1 . (2 3)) has the tail #'(2 3), and (1 . ())
;;;     the tail #'(), as in Racket;
;;;   - a vector of syntax objects, or a box (of (aux pie racket)) holding one.
;;; source is the string that names the input, the complete path of a Pie file; line is 1-based, column
;;; 0-based in characters, position 1-based in characters and span in characters. As Racket counts them,
;;; LF, CR and CRLF are each one line break, CRLF takes ONE position, and a tab moves the column to the next
;;; multiple of 8. The API is Racket's, with the names of racket/base.
;;;
;;; ==================== THE READER ====================
;;; It imitates the reader of Racket 8.15 (racket/src/expander/read/, the modules read.rkt, sequence.rkt,
;;; indentation.rkt, quote.rkt, string.rkt, char.rkt, constant.rkt, symbol-or-number.rkt, number.rkt,
;;; vector.rkt, box.rkt, here-string.rkt, whitespace.rkt and extension.rkt) as it reads the body of a
;;; `#lang` module, that is with read-accept-reader #t and read-accept-lang #f, together with the `#lang`
;;; line that comes before.
;;; The oracle is what Racket reads, and the error texts and locations were measured one by one with it.
;;; An error about an unclosed sequence or a wrong closer ends with the hint that Racket draws from the
;;; indentation of the lines, "possible cause: indentation suggests a missing `)` before line N".
;;; Read are
;;;   - lists ( ) [ ] { } with matching closers, the dotted notation and the infix dot, (a . op . b) being
;;;     (op a b);
;;;   - ' ` , ,@ #' #` #, #,@, which give (quote x), (quasiquote x), (unquote x), (unquote-splicing x),
;;;     (syntax x), (quasisyntax x), (unsyntax x) and (unsyntax-splicing x), the symbol located at the prefix;
;;;   - the comments ; #| |# (nested) #; and #! followed by a space or a slash (a line comment that a
;;;     backslash at the end of the line continues);
;;;   - strings with every Racket escape, here strings #<<, characters #\ with Racket's names, #\uXXXX,
;;;     #\UXXXXXXXX and #\ooo, #t #f #true #false, #( #[ #{ vectors and #N( vectors of a given length, #& boxes,
;;;     #:kw keywords, #% symbols, #ci and #cs, symbols with | | and \ escapes, case-sensitive;
;;;   - numbers in the whole grammar of the Racket Reference (section 1.3.3), with the prefixes #x #o #b #d
;;;     #e #i, the # digit placeholders, the exponent markers e d f s l, fractions, decimals, ±inf.0, ±nan.0
;;;     and complex numbers in both notations, and Racket's errors for a malformed number after a # prefix.
;;;     A decimal is rounded to the nearest flonum by exact arithmetic, as Racket does;
;;;   - the values that no Pie term is made of, which the parser reports as bad syntax (parser.rkt:235-394),
;;;     byte strings #"...", hash tables #hash #hasheq #hasheqv #hashalw, prefab structs #s, regexps #rx #px
;;;     #rx# #px#, whose pattern is checked as Racket's regexp checks it, and extflonums (1t0, +inf.t), which
;;;     Racket CS reads as their text (read/hash.rkt, struct.rkt, regexp.rkt and number.rkt). Their values
;;;     are those of (aux pie racket).
;;; Not read is #reader, which loads a reader. It raises the read error "`#reader` is not supported by this
;;; port", where Racket would load the reader it names. The forms that Racket's read-syntax
;;; refuses in a module body raise Racket's own error. They are #lang and #! (not enabled), graph labels #0=
;;; and #0#, #fl( and #fx( vectors, and #~ compiled code. Unlike Racket, the reader works on a string, so special
;;; (non-character) input from a port never occurs.
;;;
;;; A read error raises (raise-racket-exn 'exn:fail:read MESSAGE '()), where MESSAGE is the location, the
;;; source made relative to the current directory as Racket prints it (srcloc->string/relative), then
;;; ": read-syntax: " and Racket's text. It carries no srclocs, because Racket's error display prints none
;;; for it, its location being in the message already.

(module (aux pie syntax)
  ( ; syntax objects
    syntax? syntax-e syntax-source syntax-line syntax-column syntax-position syntax-span syntax-srcloc
    syntax->datum syntax->list identifier? datum->syntax
    ; the reader
    open-syntax-input read-syntax read-language string->syntax)

  (import scheme
          (chicken base)
          (only (scheme base) open-input-string make-bytevector bytevector-u8-set!)
          (only (chicken string) ->string string-intersperse)
          (chicken keyword)
          (only (scheme char) char-foldcase string-foldcase)
          (only (chicken bitwise) integer-length)
          (only srfi-1 any last append-reverse)
          (only srfi-13 string-index)
          (aux base)
          (aux pie racket))

  ;;; ==================== SYNTAX OBJECTS ====================

  ;; The syntax objects of racket/base, made by the reader and by datum->syntax. The struct is named stx
  ;; because `syntax` is a binding of CHICKEN.
  (define-struct stx e source line column position span)

  ;; syntax? : Any -> Boolean, and the accessors of racket/base, cf. parser.rkt:26-30, locations.rkt:19-23
  (define syntax? stx?)
  (define syntax-e stx-e)
  (define syntax-source stx-source)
  (define syntax-line stx-line)
  (define syntax-column stx-column)
  (define syntax-position stx-position)
  (define syntax-span stx-span)

  ;; syntax-srcloc : Syntax -> (U Srcloc #f), the Srcloc of basics.rkt:22 in the format of
  ;; location->srcloc (locations.rkt:19), so a syntax object without a source gives the source "#f". It is
  ;; #f for a syntax object without any location.
  (define (syntax-srcloc s)
    (and (or (syntax-source s) (syntax-line s) (syntax-position s))
         (list (->string (syntax-source s)) (syntax-line s) (syntax-column s) (syntax-position s) (syntax-span s))))

  ;; syntax->datum : Syntax -> Any, which takes off every syntax object, in vectors, boxes, the values of
  ;; hash tables and the fields of prefab structs too, cf. parser.rkt:16, 330, 402.
  (define (syntax->datum v)
    (let1 (e (if (syntax? v) (syntax-e v) v))
      (cond
        ((pair? e) (cons (syntax->datum (car e)) (syntax->datum (cdr e))))
        ((vector? e) (list->vector (map syntax->datum (vector->list e))))
        ((box? e) (box (syntax->datum (unbox e))))
        ((racket-hash? e) (make-racket-hash (racket-hash-kind e)
                                            (map (λ (p) (cons (car p) (syntax->datum (cdr p)))) (racket-hash-pairs e))))
        ((racket-prefab? e) (racket-prefab (racket-prefab-key e) (map syntax->datum (racket-prefab-fields e))))
        (else e))))

  ;; syntax->list : Syntax -> (U (Listof Syntax) #f), the elements of a syntax list, through the dotted tails
  ;; that are lists, so #'(1 . (2 3)) gives three elements; #f when s is not a list, cf. parser.rkt:257, 290.
  (define (syntax->list s)
    (let loop ((e (syntax-e s)) (elements '()))
      (cond
        ((null? e) (reverse elements))
        ((pair? e) (loop (cdr e) (cons (car e) elements)))
        ((syntax? e) (loop (syntax-e e) elements))
        (else #f))))

  ;; identifier? : Any -> Boolean, a syntax object that holds a symbol.
  (define (identifier? v) (and (syntax? v) (symbol? (syntax-e v))))

  ;; datum->syntax : Any Any [Location] -> Syntax, which wraps every part of v that is not a syntax object
  ;; yet and keeps the others. The lexical context ctxt means nothing in the port. Every new syntax object
  ;; gets the location srcloc, which is #f for none, a syntax object whose location is copied, or a Srcloc as
  ;; a list or a vector of five. The parser rewrites Pi, ∏ and Sigma with it (cf. parser.rkt:282-297), as
  ;; Racket's #'(Π more ...) and syntax/loc do.
  (define (datum->syntax ctxt v #!optional (srcloc #f))
    (let1 (location (cond
                      ((not srcloc) '(#f #f #f #f #f))
                      ((syntax? srcloc) (list (syntax-source srcloc) (syntax-line srcloc) (syntax-column srcloc)
                                              (syntax-position srcloc) (syntax-span srcloc)))
                      ((vector? srcloc) (vector->list srcloc))
                      (else srcloc)))
      (define (wrap e) (apply stx e location))
      (define (convert v)
        (cond
          ((syntax? v) v)
          ((pair? v) (wrap (convert-pairs v)))
          ((vector? v) (wrap (list->vector (map convert (vector->list v)))))
          ((box? v) (wrap (box (convert (unbox v)))))
          ((racket-hash? v) (wrap (make-racket-hash (racket-hash-kind v)
                                                    (map (λ (p) (cons (car p) (convert (cdr p)))) (racket-hash-pairs v)))))
          ((racket-prefab? v) (wrap (racket-prefab (racket-prefab-key v) (map convert (racket-prefab-fields v)))))
          (else (wrap v))))
      (define (convert-pairs v)
        (cond
          ((null? v) '())
          ((pair? v) (cons (convert (car v)) (convert-pairs (cdr v))))
          (else (convert v))))
      (convert v)))

  ;;; ==================== INPUT ====================

  ;; A sport is a string being read, with the location of the next character as Racket's line counting
  ;; gives it, whether symbols are case-folded (inside #ci), and the indentations of the sequences being read,
  ;; the innermost first (see INDENTATION below).
  (define-record sport source chars size offset line column position fold-case? indentations)

  ;; open-syntax-input : (U String #f) String -> Sport, the whole text of a source, cf. main.rkt:250, where
  ;; Racket opens the file with line counting on. The characters are read through a string port, because
  ;; string->list on a CHICKEN 6 string takes time quadratic in its length (4 s for 38 KB, measured), while
  ;; read-char takes linear time.
  (define (open-syntax-input source text)
    (let1 (chars (let1 (port (open-input-string text))
                   (let loop ((chars '()))
                     (let1 (c (read-char port))
                       (if (eof-object? c) (list->vector (reverse chars)) (loop (cons c chars)))))))
      (make-sport source chars (vector-length chars) 0 1 0 1 #f '())))

  ;; The character k places after the next one, or #f at the end.
  (define (peek-at sp #!optional (k 0))
    (let1 (i (+ (sport-offset sp) k))
      (and (< i (sport-size sp)) (vector-ref (sport-chars sp) i))))

  ;; Reads the next character and moves the location past it. A LF right after a CR belongs to the same line
  ;; break and takes no position of its own.
  (define (consume! sp)
    (let* ((i (sport-offset sp))
           (c (vector-ref (sport-chars sp) i)))
      (sport-offset-set! sp (add1 i))
      (cond
        ((and (char=? c #\newline) (> i 0) (char=? (vector-ref (sport-chars sp) (sub1 i)) #\return)) (void))
        ((or (char=? c #\newline) (char=? c #\return))
          (sport-line-set! sp (add1 (sport-line sp)))
          (sport-column-set! sp 0)
          (sport-position-set! sp (add1 (sport-position sp))))
        ((char=? c #\tab)
          (sport-column-set! sp (let1 (column (sport-column sp)) (+ column (- 8 (modulo column 8)))))
          (sport-position-set! sp (add1 (sport-position sp))))
        (else
          (sport-column-set! sp (add1 (sport-column sp)))
          (sport-position-set! sp (add1 (sport-position sp)))))
      c))

  (define (consume-n! sp n) (unless (zero? n) (consume! sp) (consume-n! sp (sub1 n))))

  ;; The location of the next character, (line column position).
  (define (here sp) (list (sport-line sp) (sport-column sp) (sport-position sp)))

  (define (sport-save sp) (vector (sport-offset sp) (sport-line sp) (sport-column sp) (sport-position sp)))

  (define (sport-restore! sp saved)
    (sport-offset-set! sp (vector-ref saved 0))
    (sport-line-set! sp (vector-ref saved 1))
    (sport-column-set! sp (vector-ref saved 2))
    (sport-position-set! sp (vector-ref saved 3)))

  ;; The syntax object of e, from the location start up to the next character.
  (define (located sp e start)
    (match1/first ((,line ,column ,position) start)
      (stx e (sport-source sp) line column position (- (sport-position sp) position))))

  ;;; ==================== ERRORS ====================

  ;; read-error : Sport (U Location #f) String -> Nothing, Racket's reader-error. The location is the one of
  ;; the datum or of the character that the text speaks about; without one, only the source is printed.
  (define (read-error sp location text)
    (raise-racket-exn
      'exn:fail:read
      (string-append (srcloc->string/relative (cons (or (sport-source sp) "string")
                                                    (append (or location '(#f #f #f)) '(#f))))
                     ": read-syntax: " text)
      '()))

  (define (bad-syntax sp start text) (read-error sp start (string-append "bad syntax `" text "`")))

  (define (unsupported sp start what) (read-error sp start (string-append "`" what "` is not supported by this port")))

  (define not-enabled-reason
    "\n  possible reason: not allowed again inside a module that already starts `#lang`, or not enabled for interactive evaluation")

  ;;; ==================== INDENTATION ====================

  ;; Racket's reader watches the indentation of the lines inside every open sequence, so that an error about a
  ;; missing or wrong closer can guess where the closer was forgotten, cf. read/indentation.rkt of the
  ;; expander. Each sequence pushes an indentation record when its opener has been read and pops it when its
  ;; closer has been read (sequence.rkt:28-31, the field indentations of read-config), and the start of every
  ;; datum that is not a closer, and every dot of a sequence, updates the innermost record (main.rkt:193-195,
  ;; sequence.rkt:62 and :92). Upstream keeps the stack in the reader's config. Here it is the field
  ;; indentations of the sport, which follows the same nesting, because each config with a longer stack is
  ;; only used while its sequence is being read. Upstream's field suspicious-quote is never set or read in
  ;; Racket 8.15, so it is left out.
  (define-record-type <indentation>
    (indentation closer suspicious-closer multiline? start-line last-line suspicious-line max-indent)
    indentation?
    (closer indentation-closer)                    ; the closer that the sequence expects
    (suspicious-closer indentation-suspicious-closer set-indentation-suspicious-closer!) ; the expected closer when the suspicious line was found
    (multiline? indentation-multiline? set-indentation-multiline?!) ; whether a datum or a dot starts on a later line than the opener
    (start-line indentation-start-line)            ; the line of the opener
    (last-line indentation-last-line set-indentation-last-line!) ; the last line whose indentation was checked
    (suspicious-line indentation-suspicious-line set-indentation-suspicious-line!) ; #f, or the first line less indented than the lines before it
    (max-indent indentation-max-indent set-indentation-max-indent!)) ; the largest indentation of a line since the opener

  ;; The record of a sequence whose opener has just been read; at first a line must be indented by more than
  ;; the column right after the opener, cf. indentation.rkt:21-30.
  (define (make-indentation closer sp)
    (indentation closer #f #f (sport-line sp) (sport-line sp) #f (add1 (sport-column sp))))

  ;; Runs (thunk) with indt on top of the stack of the sport and pops it on return, cf. sequence.rkt:28-31.
  ;; A read error ends the whole read, so it needs no popping.
  (define (with-indentation sp indt thunk)
    (let1 (outer (sport-indentations sp))
      (sport-indentations-set! sp (cons indt outer))
      (begin1 (thunk) (sport-indentations-set! sp outer))))

  ;; Racket's sequence skips whitespace and comments between its elements with the config it was called with,
  ;; whose stack does not hold its own record (seq-config at sequence.rkt:48, :75 and :95), so the datum of a #;
  ;; there counts for the enclosing sequence.
  (define (skip-whitespace-and-comments/outer! sp context)
    (let1 (indts (sport-indentations sp))
      (sport-indentations-set! sp (cdr indts))
      (skip-whitespace-and-comments! sp context)
      (sport-indentations-set! sp indts)))

  ;; The first datum on a line later than the last one checked. A line at least as indented as the most
  ;; indented line so far raises that indentation, and the first line that is less indented is suspicious,
  ;; cf. indentation.rkt:32-51.
  (define (track-indentation! sp line column)
    (let1 (indts (sport-indentations sp))
      (unless (null? indts)
        (let1 (indt (car indts))
          (when (> line (indentation-last-line indt))
            (set-indentation-last-line! indt line)
            (set-indentation-multiline?! indt #t)
            (cond
              ((>= column (indentation-max-indent indt)) (set-indentation-max-indent! indt column))
              ((not (indentation-suspicious-line indt))
                (set-indentation-suspicious-closer! indt (indentation-closer indt))
                (set-indentation-suspicious-line! indt line))))))))

  ;; The hint that ends an error about the sequence of indt, or "", cf. indentation.rkt:53-60. Upstream takes
  ;; the config and uses the top of its stack, which is indt at each of the calls.
  (define (indentation-possible-cause indt)
    (if (indentation-suspicious-line indt)
      (string-append "\n  possible cause: indentation suggests a missing `" (string (indentation-suspicious-closer indt))
                     "` before line " (number->string (indentation-suspicious-line indt)))
      ""))

  ;; The text of the error for the closer c where a datum should start, cf. indentation.rkt:62-97. Outside
  ;; any sequence, or when c is the closer of the innermost one, c is unexpected. Otherwise that sequence
  ;; expected its own closer, which is "missing" when an enclosing sequence expects c. The opener is placed
  ;; on its line when the sequence spans lines, and is "preceding" otherwise.
  (define (indentation-unexpected-closer-message sp c)
    (let1 (indts (sport-indentations sp))
      (if (null? indts)
        (string-append "unexpected `" (string c) "`")
        (let* ((indt (car indts))
               (closer (indentation-closer indt))
               (opener (string #\` (closer->opener closer) #\`)))
          (string-append
            (if (char=? c closer)
              (string-append "unexpected `" (string c) "`")
              (string-append (if (memv c (map indentation-closer (cdr indts))) "missing" "expected")
                             " `" (string closer) "` to close "
                             (if (indentation-multiline? indt)
                               (string-append opener " on line " (number->string (indentation-start-line indt)))
                               (string-append "preceding " opener))
                             ", found instead `" (string c) "`"))
            (indentation-possible-cause indt))))))

  ;;; ==================== CHARACTER CLASSES ====================

  ;; A delimiter ends a symbol or a number. It is whitespace, one of ( ) [ ] { } " , ' ` ; the byte order
  ;; mark U+FEFF or the end, cf. read/delimiter.rkt:9-31.
  (define (delimiter? c)
    (or (not c) (char-whitespace? c) (and (memv c '(#\( #\) #\[ #\] #\{ #\} #\" #\, #\' #\` #\; #\xFEFF)) #t)))

  (define (closer? c) (and (memv c '(#\) #\] #\})) #t))

  (define (closer-of opener) (case opener ((#\() #\)) ((#\[) #\]) ((#\{) #\})))

  ;; cf. closer->opener in read/closer.rkt
  (define (closer->opener closer) (case closer ((#\)) #\() ((#\]) #\[) ((#\}) #\{)))

  (define (ascii-digit? c) (and c (char<=? #\0 c #\9)))

  (define (octal-digit? c) (and c (char<=? #\0 c #\7)))

  (define (hex-digit? c) (and c (or (ascii-digit? c) (char<=? #\a (char-downcase c) #\f))))

  (define (ascii-alphanumeric? c) (and c (or (ascii-digit? c) (char<=? #\a (char-downcase c) #\z))))

  ;; The characters of a language name after #lang or #!.
  (define (lang-name-char? c) (and c (or (ascii-alphanumeric? c) (and (memv c '(#\- #\+ #\_ #\/)) #t))))

  ;; A dot that stands alone, followed by a delimiter.
  (define (dot-ahead? sp) (and (eqv? (peek-at sp) #\.) (delimiter? (peek-at sp 1))))

  ;; Reads at most max characters satisfying ok? and answers them as a string.
  (define (read-while! sp ok? #!optional (max #f))
    (let loop ((n 0) (chars '()))
      (if (and (or (not max) (< n max)) (ok? (peek-at sp)))
        (loop (add1 n) (cons (consume! sp) chars))
        (list->string (reverse chars)))))

  ;;; ==================== WHITESPACE AND COMMENTS ====================

  ;; Skips whitespace and the comments ; #| |# #; and #! followed by a space or a slash. context is the
  ;; location of the datum being read around them, where a #; that finds the end reports. The byte order mark
  ;; U+FEFF counts as whitespace, cf. read/whitespace.rkt:37-40.
  (define (skip-whitespace-and-comments! sp context)
    (let loop ()
      (let1 (c (peek-at sp))
        (cond
          ((not c) (void))
          ((or (char-whitespace? c) (char=? c #\xFEFF)) (consume! sp) (loop))
          ((char=? c #\;) (skip-line-comment! sp) (loop))
          ((char=? c #\#)
            (case (peek-at sp 1)
              ((#\|) (skip-block-comment! sp) (loop))
              ((#\;) (consume-n! sp 2)
                     (read-element sp context
                                   (τ (read-error sp context "expected a commented-out element for `#;`, but found end-of-file")))
                     (loop))
              ((#\!) (when (memv (peek-at sp 2) '(#\space #\/)) (skip-hash-bang-comment! sp) (loop)))
              (else (void))))
          (else (void))))))

  ;; ; up to the next linefeed; a carriage return does not end it (checked with Racket).
  (define (skip-line-comment! sp)
    (let loop ()
      (let1 (c (peek-at sp))
        (unless (or (not c) (char=? c #\newline)) (consume! sp) (loop)))))

  ;; #| up to the matching |#, nesting. An unterminated comment reports at its |.
  (define (skip-block-comment! sp)
    (consume! sp)
    (let1 (bar (here sp))
      (consume! sp)
      (let loop ((depth 1))
        (let ((c (peek-at sp)) (next (peek-at sp 1)))
          (cond
            ((not c) (read-error sp bar "end of file in `#|` comment"))
            ((and (char=? c #\|) (eqv? next #\#)) (consume-n! sp 2) (unless (= depth 1) (loop (sub1 depth))))
            ((and (char=? c #\#) (eqv? next #\|)) (consume-n! sp 2) (loop (add1 depth)))
            (else (consume! sp) (loop depth)))))))

  ;; #! followed by a space or a slash, up to a linefeed that no backslash comes right before. As for ;, a
  ;; carriage return is no line end here, so a backslash before CRLF does not continue the comment.
  (define (skip-hash-bang-comment! sp)
    (consume-n! sp 2)
    (let loop ((previous #\!))
      (let1 (c (peek-at sp))
        (cond
          ((not c) (void))
          ((char=? c #\newline) (when (char=? previous #\\) (consume! sp) (loop c)))
          (else (consume! sp) (loop c))))))

  ;;; ==================== DATA ====================

  ;; read-syntax : Sport -> (U Syntax EOF), the next datum, or the end of file, cf. main.rkt:250, where the
  ;; module reader reads the body of the module datum by datum.
  (define (read-syntax sp) (read-element sp #f (τ #!eof)))

  ;; The next datum after whitespace and comments, in the context of the datum at the location context.
  ;; At the end it answers (on-eof); a closer here was not expected, cf. main.rkt:224-245.
  (define (read-element sp context on-eof)
    (skip-whitespace-and-comments! sp context)
    (let1 (c (peek-at sp))
      (cond
        ((not c) (on-eof))
        ((closer? c) (read-error sp (here sp) (indentation-unexpected-closer-message sp c)))
        (else (read-datum sp context on-eof)))))

  ;; The datum that starts at the next character, which is neither whitespace nor a closer. Its start counts
  ;; for the indentation of the innermost sequence, cf. main.rkt:193-195.
  (define (read-datum sp context on-eof)
    (let ((start (here sp))
          (c (peek-at sp)))
      (track-indentation! sp (sport-line sp) (sport-column sp))
      (case c
        ((#\( #\[ #\{) (consume! sp) (read-sequence sp start c #f #f))
        ((#\") (consume! sp) (read-string-literal sp start))
        ((#\') (read-quoted sp start 'quote 1 "quoting \"'\""))
        ((#\`) (read-quoted sp start 'quasiquote 1 "quasiquoting \"`\""))
        ((#\,) (if (eqv? (peek-at sp 1) #\@)
                 (read-quoted sp start 'unquote-splicing 2 "unquoting `,@`")
                 (read-quoted sp start 'unquote 1 "unquoting `,`")))
        ((#\#) (consume! sp) (read-dispatch sp start context on-eof))
        (else (read-symbol-or-number sp start "" 'symbol-or-number)))))

  ;; What follows a #, which is consumed.
  (define (read-dispatch sp start context on-eof)
    (let1 (c (peek-at sp))
      (case c
        ((#f) (bad-syntax sp start "#"))
        ((#\( #\[ #\{) (consume! sp) (read-sequence sp start c #t #f))
        ((#\') (read-quoted sp start 'syntax 1 "quoting #'"))
        ((#\`) (read-quoted sp start 'quasisyntax 1 "quasiquoting #`"))
        ((#\,) (if (eqv? (peek-at sp 1) #\@)
                 (read-quoted sp start 'unsyntax-splicing 2 "unquoting #,@")
                 (read-quoted sp start 'unsyntax 1 "unquoting #,")))
        ((#\\) (consume! sp) (read-character sp start))
        ((#\") (consume! sp) (read-string-literal sp start 'bytes))
        ((#\<) (consume! sp)
               (if (eqv? (peek-at sp) #\<)
                 (begin (consume! sp) (read-here-string sp start))
                 (bad-syntax sp start "#<")))
        ((#\%) (consume! sp) (read-symbol-or-number sp start "#%" 'symbol))
        ((#\:) (consume! sp) (read-symbol-or-number sp start "" 'keyword))
        ((#\t #\T) (consume! sp) (read-delimited-constant sp start c (char=? c #\t) '(#\r #\u #\e) #t))
        ((#\f #\F) (consume! sp)
                   (case (peek-at sp)
                     ((#\x #\X) (read-error sp start "literal fxvectors not allowed"))
                     ((#\l #\L) (read-error sp start "literal flvectors not allowed"))
                     (else (read-delimited-constant sp start c (char=? c #\f) '(#\a #\l #\s #\e) #f))))
        ((#\e #\E #\i #\I #\d #\D #\b #\B #\o #\O #\x #\X) (read-symbol-or-number sp start "#" 'number))
        ((#\c #\C) (consume! sp)
                   (case (peek-at sp)
                     ((#\s #\S) (consume! sp) (read-with-case sp #f context on-eof))
                     ((#\i #\I) (consume! sp) (read-with-case sp #t context on-eof))
                     (else (read-error sp start (string-append "expected `s', `S`, `i`, or `I` after `#" (string c) "`")))))
        ((#\&) (consume! sp) (read-box sp start))
        ((#\!) (consume! sp) (read-hash-bang sp start))
        ((#\h #\H) (consume! sp) (read-hash sp start c))
        ((#\r) (consume! sp)
               (case (peek-at sp)
                 ((#\x) (consume! sp) (read-regexp sp start "#rx" #f))
                 ((#\e) (expect-word! sp start "#r" "eader")
                        (skip-whitespace-and-comments! sp start)
                        (if (peek-at sp)
                          (unsupported sp start "#reader")
                          (read-error sp start "expected a datum after `#reader`, found end-of-file")))
                 (else (bad-syntax-after sp start "#r"))))
        ((#\p) (consume! sp)
               (if (eqv? (peek-at sp) #\x)
                 (begin (consume! sp) (read-regexp sp start "#px" #t))
                 (bad-syntax-after sp start "#p")))
        ((#\s) (consume! sp) (read-struct sp start))
        ((#\l) (consume! sp)
               (expect-word! sp start "#l" "ang")
               (if (eqv? (peek-at sp) #\space)
                 (read-error sp start (string-append "`#lang` not enabled" not-enabled-reason))
                 (read-error sp start "expected a single space after `#lang`")))
        ((#\~) (read-error sp start "`#~` compiled expressions not enabled"))
        (else (if (ascii-digit? c)
                (read-vector-or-graph sp start)
                (begin (consume! sp) (bad-syntax sp start (string #\# c))))))))

  ;; Reads the characters of word one by one after the text read so far; the first one that differs is read
  ;; too and reported, unless it is a delimiter, as Racket's #reader and #lang do.
  (define (expect-word! sp start read-so-far word)
    (let loop ((i 0) (text read-so-far))
      (when (< i (string-length word))
        (let1 (c (peek-at sp))
          (cond
            ((eqv? c (string-ref word i)) (consume! sp) (loop (add1 i) (string-append text (string c))))
            ((delimiter? c) (bad-syntax sp start text))
            (else (consume! sp) (bad-syntax sp start (string-append text (string c)))))))))

  ;; #ci and #cs read the next datum with case-folding on or off.
  (define (read-with-case sp fold? context on-eof)
    (let1 (outer (sport-fold-case? sp))
      (sport-fold-case?-set! sp fold?)
      (begin1 (read-element sp context on-eof)
              (sport-fold-case?-set! sp outer))))

  ;; #! that does not start a comment. #!NAME is a #lang line, which the body of a module may not have.
  (define (read-hash-bang sp start)
    (let1 (c (peek-at sp))
      (cond
        ((not c) (bad-syntax sp start "#!"))
        ((lang-name-char? c) (read-error sp start (string-append "`#!` not enabled" not-enabled-reason)))
        (else (consume! sp) (bad-syntax sp start (string #\# #\! c))))))

  ;; The bad syntax of what was read and the next character, which is consumed, as #r and #p report it.
  (define (bad-syntax-after sp start read-so-far)
    (let1 (c (peek-at sp))
      (if c
        (begin (consume! sp) (bad-syntax sp start (string-append read-so-far (string c))))
        (bad-syntax sp start read-so-far))))

  ;;; ==================== HASH TABLES, REGEXPS AND PREFAB STRUCTS ====================

  ;; #hash( #hasheq( #hasheqv( #hashalw( and their [ and { variants, after the # and the h, which is init-c,
  ;; cf. read-hash in read/hash.rkt of the expander. Every letter is read with its case, an upper-case one
  ;; too, and the error texts quote what was read, the character that does not fit included.
  (define (read-hash sp start init-c)
    (let1 (accum (list init-c #\#))                                ; reversed
      (define (accumulated) (list->string (reverse accum)))
      (define (next-char!) (let1 (c (peek-at sp)) (when c (consume! sp)) c))
      (define (get-next! expect-c)
        (let1 (c (next-char!))
          (unless (and c (char=? (char-downcase c) expect-c))
            (read-error sp start (string-append "expected `" (string expect-c) "` after `" (accumulated) "`")))
          (set! accum (cons c accum))))
      (get-next! #\a)
      (get-next! #\s)
      (get-next! #\h)
      (let loop ((mode 'equal))
        (let1 (c (next-char!))
          (case c
            ((#\( #\[ #\{) (located sp (make-racket-hash mode (read-hash-pairs sp start c)) start))
            ((#\e #\E) (set! accum (cons c accum))
                       (get-next! #\q)
                       (loop 'eq))
            ((#\v #\V) (set! accum (cons c accum))
                       (if (eq? mode 'eq) (loop 'eqv) (bad-syntax sp start (accumulated))))
            ((#\a #\A) (set! accum (cons c accum))
                       (get-next! #\l)
                       (get-next! #\w)
                       (if (eq? mode 'equal) (loop 'equal-always) (bad-syntax sp start (accumulated))))
            (else (when c (set! accum (cons c accum)))
                  (bad-syntax sp start (accumulated))))))))

  ;; The pairs of a hash table up to the closer of opener, which is consumed, as read-unwrapped-sequence of
  ;; read/sequence.rkt reads them with #:dot-mode #f, so that a dot after the first element is illegal. The
  ;; keys and the values count for the indentation of the hash table, the openers of the pairs do not.
  (define (read-hash-pairs sp start opener)
    (let1 (closer (closer-of opener))
      (with-indentation sp (make-indentation closer sp)
        (τ (let loop ((pairs '()) (first? #t))
             (skip-whitespace-and-comments/outer! sp start)
             (cond
               ((eqv? (peek-at sp) closer) (consume! sp) (reverse pairs))
               ((and (not first?) (dot-ahead? sp)) (read-error sp (here sp) "illegal use of `.`"))
               (else (loop (cons (read-hash-pair sp start opener closer) pairs) #f))))))))

  ;; One (key . value) between ( ) [ ] or { }, cf. make-read-one-key+value in read/hash.rkt. The key is a
  ;; datum, the value a syntax object. An element that is no pair reports where it starts, and the end of
  ;; the input reports at the hash table. The key and the value may be anything, the end of the input too,
  ;; which then reports as the missing dot or closer.
  (define (read-hash-pair sp start opener closer)
    (let ((c (peek-at sp))
          (location (here sp)))
      (cond
        ((not c) (read-error sp start (string-append "expected `" (string closer) "` to close `" (string opener) "`")))
        ((closer? c) (read-error sp location (indentation-unexpected-closer-message sp c)))
        ((memv c '(#\( #\[ #\{))
          (consume! sp)
          (let* ((pair-closer (closer-of c))
                 (key (read-element sp start (τ #!eof))))
            (skip-whitespace-and-comments! sp start)
            (unless (dot-ahead? sp) (read-error sp (here sp) "expected `.` and value for hash"))
            (consume! sp)
            (let1 (value (read-element sp start (τ #!eof)))
              (skip-whitespace-and-comments! sp start)
              (unless (eqv? (peek-at sp) pair-closer)
                (read-error sp (here sp) (string-append "expected `" (string pair-closer) "` after value within a hash")))
              (consume! sp)
              (cons (syntax->datum key) value))))
        (else (read-error sp location "expected `(`, `[`, or `{` to start a hash pair")))))

  ;; #rx and #px after their x, followed by a string or by # and a byte string, cf. read/regexp.rkt. The
  ;; pattern is checked as Racket's regexp and pregexp check it (racket-regexp-error of (aux pie racket)),
  ;; whose error the reader reports with the pattern, as catch-and-reraise-as-reader (read/error.rkt:38)
  ;; reports the exn:fail of raise-arguments-error without its "regexp: ".
  (define (read-regexp sp start prefix px?)
    (let1 (c3 (peek-at sp))
      (when c3 (consume! sp))
      (case c3
        ((#\") (make-regexp sp start (syntax-e (read-string-literal sp start)) px?))
        ((#\#) (let1 (c4 (peek-at sp))
                 (when c4 (consume! sp))
                 (if (eqv? c4 #\")
                   (make-regexp sp start (syntax-e (read-string-literal sp start 'bytes)) px?)
                   (read-error sp start (string-append "expected `\"` after `" prefix "#`")))))
        (else (read-error sp start (string-append "expected `\"` or `#` after `" prefix "`"))))))

  (define (make-regexp sp start source px?)
    (let1 (problem (racket-regexp-error source px?))
      (if problem
        (read-error sp start (string-append problem "\n  pattern: " (racket-truncate (racket-write->string source) 256)))
        (located sp (racket-regexp source px?) start))))

  ;; #s( #s[ #s{ after the # and the s, a prefab struct, cf. read-struct in read/struct.rkt. The elements are
  ;; read as a list, dots included, whose first element is the key, a datum, and the others the fields. A
  ;; dotted tail makes the fields an improper list, which fits no key. The key is checked as
  ;; prefab-key->struct-type checks it (rumble/prefab.ss:11), and read-syntax refuses mutable fields.
  (define (read-struct sp start)
    (let1 (c (peek-at sp))
      (unless (memv c '(#\( #\[ #\{)) (read-error sp start "expected `(`, `[`, or `{` after `#s`"))
      (consume! sp)
      (let1 (seq (syntax-e (read-sequence sp start c #f #f)))
        (when (null? seq) (read-error sp start "missing structure description in `#s` form"))
        (let* ((key (syntax->datum (car seq)))
               (fields (cdr seq))
               (count (let loop ((fields fields) (n 0))
                        (cond ((null? fields) n) ((pair? fields) (loop (cdr fields) (add1 n))) (else #f)))))
          (unless (prefab-key? key) (read-error sp start "invalid structure description in `#s` form"))
          (let1 (normalized (and count
                                 (prefab-key-compatible-count? key count)
                                 (let1 (normalized (normalize-prefab-key key #f))
                                   (and (prefab-key? normalized) (prefab-key-compatible-count? normalized count) normalized))))
            (unless normalized
              (read-error sp start "mismatch between structure description and number of provided field values in `#s` form"))
            (unless (prefab-key-all-fields-immutable? key) (read-error sp start "cannot read mutable `#s` form as syntax"))
            (located sp (racket-prefab normalized fields) start))))))

  ;;; ==================== SEQUENCES ====================

  ;; A list or a vector up to the closer of opener, which is consumed; start is the location of the opener,
  ;; or of the # of a vector, cf. read-unwrapped-sequence in read/sequence.rkt. A vector has no dots, and
  ;; declared is #f or the length of a #N( vector. A list may end with a dotted tail, or have an infix dot,
  ;; (a ... . op . b ...) giving (op a ... b ...). The errors are reported where Racket reports them. An
  ;; unclosed sequence reports at its opener, a wrong closer and a misplaced dot where they are, and a dotted
  ;; tail followed by anything but the closer at its dot. The sequence has its own indentation record (see
  ;; INDENTATION), so that an unclosed sequence and a wrong closer can hint at the line where the closer is
  ;; missing.
  (define (read-sequence sp start opener vector? declared)
    (let* ((closer (closer-of opener))
           (indt (make-indentation closer sp))
           (unclosed (τ (read-error sp start (string-append "expected a `" (string closer) "` to close `" (string opener) "`"
                                                            (indentation-possible-cause indt))))))
      (define (finish elements infix)
        (let1 (items (if infix (cons infix (reverse elements)) (reverse elements)))
          (if vector?
            (finish-vector items)
            (located sp items start))))
      (define (finish-vector items)
        (let1 (count (length items))
          (cond
            ((not declared) (located sp (list->vector items) start))
            ((> count declared)
              (read-error sp start (string-append "vector length " (number->string declared) " is too small, "
                                                  (number->string count) " values provided")))
            (else
              (let1 (v (make-vector declared (if (null? items) (located sp 0 start) (last items))))
                (let fill ((i 0) (items items))
                  (unless (null? items) (vector-set! v i (car items)) (fill (add1 i) (cdr items))))
                (located sp v start))))))
      (define (illegal-dot location) (read-error sp location "illegal use of `.`"))
      (with-indentation sp indt
        (τ (let loop ((elements '()) (infix #f))
             (skip-whitespace-and-comments/outer! sp start)
             (let1 (c (peek-at sp))
               (cond
                 ((not c) (unclosed))
                 ((closer? c)
                   (if (char=? c closer)
                     (begin (consume! sp) (finish elements infix))
                     (read-error sp (here sp) (indentation-unexpected-closer-message sp c))))
                 ((dot-ahead? sp)
                   (let1 (dot (here sp))
                     (track-indentation! sp (sport-line sp) (sport-column sp))      ; cf. sequence.rkt:62
                     (consume! sp)
                     (when (or vector? infix (null? elements)) (illegal-dot dot))
                     (let1 (tail (read-after-dot sp start unclosed))
                       (skip-whitespace-and-comments/outer! sp start)
                       (cond
                         ((eqv? (peek-at sp) closer) (consume! sp) (located sp (append-reverse elements tail) start))
                         ((dot-ahead? sp)
                           (consume! sp)
                           (track-indentation! sp (sport-line sp) (sport-column sp)) ; after the dot, cf. sequence.rkt:91-92
                           (skip-whitespace-and-comments/outer! sp start)
                           (let1 (c (peek-at sp))
                             ;; the closer right after the second dot is illegal, any other closer is a wrong one
                             (if (eqv? c closer) (illegal-dot dot) (loop elements tail))))
                         (else (illegal-dot dot))))))
                 (else (loop (cons (read-datum sp start unclosed) elements) infix)))))))))

  ;; The datum after a dot, which may be neither the end, nor a closer, nor another dot.
  (define (read-after-dot sp start unclosed)
    (skip-whitespace-and-comments! sp start)
    (let1 (c (peek-at sp))
      (cond
        ((not c) (unclosed))
        ((closer? c) (read-error sp (here sp) (indentation-unexpected-closer-message sp c)))
        ((dot-ahead? sp) (read-error sp (here sp) "illegal use of `.`"))
        (else (read-datum sp start unclosed)))))

  ;; #N( #N[ #N{, a vector of length N whose missing elements repeat the last one, or 0 when it is empty; the
  ;; graph labels #N= and #N# are refused by read-syntax.
  (define (read-vector-or-graph sp start)
    (let* ((digits (read-while! sp ascii-digit?))
           (c (peek-at sp)))
      (case c
        ((#\( #\[ #\{) (consume! sp) (read-sequence sp start c #t (string->number digits)))
        ((#\=) (read-error sp start "`#...=` forms not enabled for `read-syntax` mode"))
        ((#\#) (read-error sp start "`#...#` forms not enabled for `read-syntax` mode"))
        ((#f) (bad-syntax sp start (string-append "#" digits "#<eof>")))
        (else (consume! sp) (bad-syntax sp start (string-append "#" digits (string c)))))))

  ;;; ==================== QUOTES AND BOXES ====================

  ;; ' ` , ,@ #' #` #, #,@ and the datum after them. The n characters left of the prefix are consumed, and
  ;; the prefix becomes the symbol head, located where the prefix is. The datum must follow; description
  ;; names the prefix in the error.
  (define (read-quoted sp start head n description)
    (consume-n! sp n)
    (let* ((tag (located sp head start))
           (datum (read-element sp start (τ (read-error sp start (string-append "expected an element for " description
                                                                                 ", found end-of-file"))))))
      (located sp (list tag datum) start)))

  ;; #& and the datum in the box.
  (define (read-box sp start)
    (let1 (datum (read-element sp start (τ (read-error sp start "expected an element for `#&` box, found end-of-file"))))
      (located sp (box datum) start)))

  ;;; ==================== STRINGS ====================

  ;; A string after its opening ", with the escapes \a \b \t \n \v \f \r \e \" \' \\, \ooo (octal, up to 255),
  ;; \xHH, \uHHHH (a surrogate pair of two of them makes one character), \UHHHHHHHH, and a backslash before
  ;; a line break, which removes it. Every error reports at the opening ", or at the # of a byte string. In
  ;; the mode bytes, the one of #"...", the string is a byte string, whose characters must be bytes and
  ;; which has no \u and \U, cf. read-string in read/string.rkt.
  (define (read-string-literal sp start #!optional (mode 'string))
    (let1 (mode-name (if (eq? mode 'bytes) "byte string" "string"))
      (let loop ((chars '()))
        (let1 (c (peek-at sp))
          (cond
            ((not c) (read-error sp start "expected a closing `\"`"))
            ((char=? c #\")
              (consume! sp)
              (located sp (if (eq? mode 'bytes) (chars->bytevector (reverse chars)) (list->string (reverse chars))) start))
            ((char=? c #\\) (consume! sp) (loop (read-escape sp start chars mode-name)))
            ((and (eq? mode 'bytes) (> (char->integer c) 255))
              (read-error sp start (string-append "character `" (string c) "` is out of range in byte string")))
            (else (consume! sp) (loop (cons c chars))))))))

  (define (chars->bytevector chars)
    (let1 (bv (make-bytevector (length chars)))
      (let loop ((i 0) (chars chars))
        (if (null? chars) bv (begin (bytevector-u8-set! bv i (char->integer (car chars))) (loop (add1 i) (cdr chars)))))))

  ;; The escape after a backslash, consed onto chars. mode-name is "string" or "byte string".
  (define (read-escape sp start chars mode-name)
    (let1 (c (peek-at sp))
      (unless c (read-error sp start "expected a closing `\"`"))
      (consume! sp)
      (when (and (memv c '(#\u #\U)) (string=? mode-name "byte string"))
        (read-error sp start (string-append "unknown escape sequence `\\" (string c) "` in byte string")))
      (case c
        ((#\a) (cons (integer->char 7) chars))
        ((#\b) (cons (integer->char 8) chars))
        ((#\t) (cons #\tab chars))
        ((#\n) (cons #\newline chars))
        ((#\v) (cons (integer->char 11) chars))
        ((#\f) (cons (integer->char 12) chars))
        ((#\r) (cons #\return chars))
        ((#\e) (cons (integer->char 27) chars))
        ((#\" #\' #\\) (cons c chars))
        ((#\newline) chars)
        ((#\return) (when (eqv? (peek-at sp) #\newline) (consume! sp)) chars)
        ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7)
          (let* ((digits (string-append (string c) (read-while! sp octal-digit? 2)))
                 (code (string->number digits 8)))
            (if (> code 255)
              (read-error sp start (string-append "escape sequence `\\" digits "` is out of range in " mode-name))
              (cons (integer->char code) chars))))
        ((#\x) (cons (integer->char (read-hex-escape sp start "x" 2)) chars))
        ((#\u)
          (let* ((digits (read-while! sp hex-digit? 4))
                 (code (hex-escape-code sp start "u" digits)))
            (if (<= #xD800 code #xDFFF)
              (cons (read-surrogate-pair sp start code (string-append "\\u" digits)) chars)
              (cons (integer->char code) chars))))
        ((#\U)
          (let* ((digits (read-while! sp hex-digit? 8))
                 (code (hex-escape-code sp start "U" digits)))
            (if (or (> code #x10FFFF) (<= #xD800 code #xDFFF))
              (read-error sp start (string-append "escape sequence `\\U" digits "` is out of range in string"))
              (cons (integer->char code) chars))))
        (else (read-error sp start (string-append "unknown escape sequence `\\" (string c) "` in " mode-name))))))

  (define (read-hex-escape sp start letter max)
    (hex-escape-code sp start letter (read-while! sp hex-digit? max)))

  (define (hex-escape-code sp start letter digits)
    (if (string=? digits "")
      (read-error sp start (string-append "no hex digit following `\\" letter "`"))
      (string->number digits 16)))

  ;; After \uHHHH in the surrogate range, a high surrogate must be followed by \u and a low one, which make one
  ;; character; anything else is reported with the text read so far.
  (define (read-surrogate-pair sp start code text)
    (let1 (bad (λ (more) (read-error sp start (string-append "bad or incomplete surrogate-style encoding at `" text more "`"))))
      (let1 (c1 (peek-at sp))
        (unless c1 (read-error sp start "expected a closing `\"`"))
        (consume! sp)
        (unless (char=? c1 #\\) (bad (string c1)))
        (let1 (c2 (peek-at sp))
          (unless c2 (read-error sp start "expected a closing `\"`"))
          (consume! sp)
          (unless (char=? c2 #\u) (bad (string #\\ c2)))
          (let* ((digits (read-while! sp hex-digit? 4))
                 (low (and (> (string-length digits) 0) (string->number digits 16))))
            (if (and (<= code #xDBFF) low (<= #xDC00 low #xDFFF))
              (integer->char (+ #x10000 (* (- code #xD800) #x400) (- low #xDC00)))
              (bad (string-append "\\u" digits))))))))

  ;; #<<TERMINATOR, then the lines up to one that is TERMINATOR, joined by newlines.
  (define (read-here-string sp start)
    (define (read-line!)
      (let loop ((chars '()))
        (let1 (c (peek-at sp))
          (cond
            ((not c) (values (list->string (reverse chars)) #f))
            ((char=? c #\newline) (consume! sp) (values (list->string (reverse chars)) #t))
            (else (consume! sp) (loop (cons c chars)))))))
    (receive (terminator newline?) (read-line!)
      (unless newline? (read-error sp start "found end-of-file after `#<<` and before a newline"))
      (let loop ((lines '()))
        (receive (line newline?) (read-line!)
          (cond
            ((string=? line terminator) (located sp (string-intersperse (reverse lines) "\n") start))
            ((not newline?) (read-error sp start (string-append "found end-of-file before terminating `" terminator "`")))
            (else (loop (cons line lines))))))))

  ;;; ==================== CHARACTERS AND CONSTANTS ====================

  ;; The names of characters after #\, case-insensitive.
  (define character-names
    `(("nul" . ,(integer->char 0)) ("null" . ,(integer->char 0)) ("backspace" . ,(integer->char 8)) ("tab" . #\tab)
      ("newline" . #\newline) ("linefeed" . #\newline) ("vtab" . ,(integer->char 11)) ("page" . ,(integer->char 12))
      ("return" . #\return) ("space" . #\space) ("rubout" . ,(integer->char 127))))

  ;; A character after #\ is three octal digits up to 255, u and up to 4 hex digits, U and up to 8, a name of
  ;; two letters or more, or any single character. The u, the U and a single letter may be followed by
  ;; anything, which belongs to the next datum.
  (define (read-character sp start)
    (let1 (c (peek-at sp))
      (unless c (read-error sp start "expected a character after `#\\`"))
      (consume! sp)
      (let1 (bad (λ (text) (read-error sp start (string-append "bad character constant `#\\" text "`"))))
        (located sp
                 (cond
                   ((and (octal-digit? c) (octal-digit? (peek-at sp)))
                     (let1 (digits (string-append (string c) (read-while! sp octal-digit? 2)))
                       (if (and (= (string-length digits) 3) (<= (string->number digits 8) 255))
                         (integer->char (string->number digits 8))
                         (bad digits))))
                   ((memv c '(#\u #\U))
                     (let1 (digits (read-while! sp hex-digit? (if (char=? c #\u) 4 8)))
                       (if (string=? digits "")
                         c
                         (let1 (code (string->number digits 16))
                           (if (or (< code #xD800) (< #xDFFF code #x110000))
                             (integer->char code)
                             (bad (string-append "u" digits)))))))
                   ((and (racket-char-alphabetic? c) (let1 (next (peek-at sp)) (and next (racket-char-alphabetic? next))))
                     (let1 (name (string-foldcase (string-append (string c) (read-while! sp (λ (c) (and c (racket-char-alphabetic? c)))))))
                       (cond
                         ((assoc name character-names) => cdr)
                         (else (bad name)))))
                   (else c))
                 start))))

  ;; #t #f #true #false, which a delimiter must end; the first character that does not fit is reported with
  ;; what was read. Only a lower-case #t or #f may go on to #true or #false.
  (define (read-delimited-constant sp start first can-match? rest value)
    (let1 (text (λ (chars) (list->string (cons #\# (reverse chars)))))
      (if (delimiter? (peek-at sp))
        (located sp value start)
        (let loop ((rest rest) (chars (list first)))
          (let1 (c (peek-at sp))
            (cond
              ((delimiter? c) (if (null? rest) (located sp value start) (bad-syntax sp start (text chars))))
              ((and can-match? (pair? rest) (char=? c (car rest))) (consume! sp) (loop (cdr rest) (cons c chars)))
              (else (consume! sp) (bad-syntax sp start (text (cons c chars))))))))))

  ;;; ==================== SYMBOLS AND KEYWORDS ====================

  ;; A symbol, a number, a keyword (mode 'keyword, after #:) or a #% symbol (mode 'symbol), up to a delimiter,
  ;; after the characters of prefix. Between bars and after a backslash a character stands for itself, and
  ;; such a quoted token is never a number nor a dot. Mode 'number is a token that starts with a # prefix,
  ;; which must be a number. Inside #ci the characters that are not quoted are case-folded.
  (define (read-symbol-or-number sp start prefix mode)
    (let1 (end-of-file (λ (c) (read-error sp start (string-append "end-of-file following `" c "` in "
                                                                  (if (eq? mode 'keyword) "keyword" "symbol")))))
      (let loop ((chars (reverse (string->list prefix))) (quoted? #f))
        (let1 (c (peek-at sp))
          (cond
            ((delimiter? c) (finish-token sp start (list->string (reverse chars)) mode quoted?))
            ((char=? c #\|)
              (consume! sp)
              (let bars ((chars chars))
                (let1 (c (peek-at sp))
                  (cond
                    ((not c) (end-of-file "|"))
                    ((char=? c #\|) (consume! sp) (loop chars #t))
                    (else (consume! sp) (bars (cons c chars)))))))
            ((char=? c #\\)
              (consume! sp)
              (unless (peek-at sp) (end-of-file "\\"))
              (loop (cons (consume! sp) chars) #t))
            (else (consume! sp) (loop (cons (if (sport-fold-case? sp) (char-foldcase c) c) chars) quoted?)))))))

  (define (finish-token sp start name mode quoted?)
    (case mode
      ((keyword) (located sp (string->keyword name) start))
      ((symbol) (located sp (string->symbol name) start))
      (else
        (cond
          (quoted? (located sp (string->symbol name) start))
          ((string=? name ".") (read-error sp start "illegal use of `.`"))
          (else
            (let1 (n (parse-number name))
              (cond
                ((or (number? n) (racket-extflonum? n)) (located sp n start))
                ((eq? (car n) 'error) (read-error sp start (cdr n)))
                ((eq? mode 'number) (read-error sp start (cdr n)))
                (else (located sp (string->symbol name) start)))))))))

  ;;; ==================== NUMBERS ====================
  ;;; parse-number answers the number that a token denotes in Racket's grammar, or an extflonum of (aux pie
  ;;; racket), or (fail . TEXT) when the token is no number (a symbol, unless it starts with a # prefix, where
  ;;; TEXT is Racket's error), or (error . TEXT) when it is a number that Racket refuses (a zero
  ;;; denominator). A real is parsed into a part, the vector
  ;;; #(sign numerator denominator exponent inexact? special), whose value is
  ;;; sign * numerator/denominator * radix^exponent, or the flonum special for ±inf.0 and +nan.0; the parts
  ;;; are combined only at the end, so that the exactness prefix applies to all of them.

  ;; Binds each x to the value of its expression, unless the value is a failure, which is answered at once.
  (define-syntax let-parsed
    (syntax-rules ()
      ((_ () body ...) (let () body ...))
      ((_ ((x e) more ...) body ...) (let1 (x e) (if (pair? x) x (let-parsed (more ...) body ...))))))

  (define (parse-number token)
    (let1 (n (string-length token))
      (let loop ((i 0) (radix #f) (exactness #f))
        (if (and (< (add1 i) n) (char=? (string-ref token i) #\#))
          (let1 (c (char-downcase (string-ref token (add1 i))))
            (cond
              ((memv c '(#\e #\i))
                (if exactness
                  (cons 'fail (string-append "misplaced exactness specification at `" (substring token i n) "`"))
                  (loop (+ i 2) radix (if (char=? c #\e) 'exact 'inexact))))
              ((assv c '((#\x . 16) (#\o . 8) (#\b . 2) (#\d . 10)))
                => (λ (entry)
                     (if radix
                       (cons 'fail (string-append "misplaced radix specification at `" (substring token i n) "`"))
                       (loop (+ i 2) (cdr entry) exactness))))
              (else (cons 'fail "bad digit `#`"))))
          (let1 (body (substring token i n))
            (cond
              ((string=? body "") (cons 'fail "no digits"))
              ((string=? body ".") (cons 'fail "illegal use of `.`"))
              (else (parse-complex body (or radix 10) exactness token))))))))

  (define (in-token what token) (cons 'fail (string-append what " in `" token "`")))

  ;; A real, real@real, or [real]±[ureal]i. An extflonum is a number only as a whole token, which the
  ;; exactness prefixes cannot convert. Racket reads a complex number from left to right, so an extflonum
  ;; before the + - or @ makes that character misplaced, and one after it cannot be combined, a message that
  ;; prints the real part, or the error that it makes, as number.rkt:369 of the expander does, or the
  ;; extflonum itself when there is no real part or in the polar form (checked with Racket 8.15).
  (define (parse-complex body radix exactness token)
    (let1 (n (string-length body))
      (cond
        ((memv (string-ref body (sub1 n)) '(#\i #\I))
          (let1 (k (imaginary-sign-index body radix))
            (if (not k)
              (in-token "misplaced `i`" token)
              (let-parsed ((re (if (zero? k)
                                 (vector 1 0 1 0 #f #f)
                                 (no-extflonum-before (parse-real (substring body 0 k) radix token) (string-ref body k) token))))
                (let1 (im (let1 (s (substring body k (sub1 n)))
                            (if (= (string-length s) 1)
                              (vector (if (char=? (string-ref s 0) #\-) -1 1) 1 1 0 #f #f)
                              (parse-real s radix token))))
                  (if (extflonum-part? im)
                    (cannot-combine (if (zero? k) (cdr im) (part-value re radix exactness token)))
                    (let-parsed ((im im)
                                 (x (part-value re radix exactness token))
                                 (y (part-value im radix exactness token)))
                      (cond
                        ((and (exact? y) (zero? y)) x)
                        ((or (inexact? x) (inexact? y)) (make-rectangular (->flonum x) (->flonum y)))
                        (else (make-rectangular x y))))))))))
        ((string-index body #\@)
          => (λ (k)
               (cond
                 ((zero? k) (cons 'fail "bad digit `@`"))
                 ((= k (sub1 n)) (in-token "misplaced `@`" token))
                 (else
                   (let-parsed ((m (no-extflonum-before (parse-real (substring body 0 k) radix token) #\@ token)))
                     (let1 (a (parse-real (substring body (add1 k) n) radix token))
                       (if (extflonum-part? a)
                         (cannot-combine (cdr a))
                         (let-parsed ((a a)
                                      (r (part-value m radix exactness token))
                                      (θ (part-value a radix exactness token)))
                           (cond
                             ((and (exact? θ) (zero? θ)) r)
                             ((and (exact? r) (zero? r)) 0)
                             (else (let* ((r (->flonum r)) (θ (->flonum θ))
                                          (z (make-rectangular (* r (cos θ)) (* r (sin θ)))))
                                     (if (eq? exactness 'exact) (inexact->exact z) z))))))))))))
        (else (let1 (x (parse-real body radix token))
                (cond
                  ((not (extflonum-part? x)) (let-parsed ((x x)) (part-value x radix exactness token)))
                  ((eq? exactness 'exact) (in-token "cannot convert extflonum to exact" token))
                  ((eq? exactness 'inexact) (in-token "cannot convert extflonum to inexact" token))
                  (else (racket-extflonum (cdr x)))))))))

  ;; parse-real answers (extflonum . TEXT) for an extflonum, TEXT being the part that names it.
  (define (extflonum-part? x) (and (pair? x) (eq? (car x) 'extflonum)))

  ;; The part before the character c of a complex number, which an extflonum may not be.
  (define (no-extflonum-before part c token)
    (if (extflonum-part? part) (in-token (string-append "misplaced `" (string c) "`") token) part))

  ;; real is the extflonum, a number or a failure.
  (define (cannot-combine real)
    (cons 'fail (string-append "cannot combine extflonum `"
                               (cond ((string? real) real) ((number? real) (racket-number->string real)) (else (cdr real)))
                               "` into a complex number")))

  ;; The index of the sign that starts the imaginary part of body, which ends with i. It is the last + or - that does
  ;; not follow an exponent marker (the f of +inf.f is no marker), or #f.
  (define (imaginary-sign-index body radix)
    (let loop ((k (- (string-length body) 2)))
      (cond
        ((< k 0) #f)
        ((and (memv (string-ref body k) '(#\+ #\-))
              (or (zero? k)
                  (not (exponent-marker? (string-ref body (sub1 k)) radix))
                  (special-suffix? body k)))
          k)
        (else (loop (sub1 k))))))

  (define (special-suffix? body k)
    (and (>= k 5) (any (λ (special) (string-ci=? special (substring body (- k 5) k))) '("inf.f" "nan.f" "inf.t" "nan.t"))))

  ;; e d f s l t mark an exponent in radix 2, 8 and 10; in radix 16, where e d f are digits, only s l t.
  (define (exponent-marker? c radix)
    (and (memv (char-downcase c) (if (= radix 16) '(#\s #\l #\t) '(#\e #\d #\f #\s #\l #\t))) #t))

  (define (digit-value c radix)
    (let1 (d (cond
               ((ascii-digit? c) (- (char->integer c) 48))
               ((char<=? #\a (char-downcase c) #\f) (+ 10 (- (char->integer (char-downcase c)) 97)))
               (else #f)))
      (and d (< d radix) d)))

  ;; [sign] ureal, or ±inf.0 ±nan.0 (and .f), or (extflonum . s) for the extflonums ±inf.t ±nan.t and those
  ;; with the exponent marker t.
  (define (parse-real s radix token)
    (let* ((signed? (memv (string-ref s 0) '(#\+ #\-)))
           (sign (if (char=? (string-ref s 0) #\-) -1 1))
           (u (if signed? (substring s 1 (string-length s)) s)))
      (cond
        ((and signed? (any (λ (special) (string-ci=? u special)) '("inf.0" "inf.f")))
          (vector sign 0 1 0 #t (if (< sign 0) -inf.0 +inf.0)))
        ((and signed? (any (λ (special) (string-ci=? u special)) '("nan.0" "nan.f")))
          (vector sign 0 1 0 #t +nan.0))
        ((and signed? (any (λ (special) (string-ci=? u special)) '("inf.t" "nan.t"))) (cons 'extflonum s))
        ((string=? u "") (in-token "no digits" token))
        (else (let1 (x (parse-ureal u sign radix token))
                (if (extflonum-part? x) (cons 'extflonum s) x))))))

  ;; mantissa [marker exponent]
  (define (parse-ureal u sign radix token)
    (let1 (m (let loop ((i 0))
               (cond
                 ((= i (string-length u)) #f)
                 ((exponent-marker? (string-ref u i) radix) i)
                 (else (loop (add1 i))))))
      (let-parsed ((mantissa (parse-mantissa (if m (substring u 0 m) u) radix token))
                   (exponent (if m (parse-exponent (substring u (add1 m) (string-length u)) radix token) 0)))
        (if (and m (char-ci=? (string-ref u m) #\t))
          (cons 'extflonum u)
          (vector sign (vector-ref mantissa 0) (vector-ref mantissa 1) exponent
                  (or (and m #t) (vector-ref mantissa 2)) #f)))))

  ;; [sign] digits, in the radix.
  (define (parse-exponent e radix token)
    (let* ((n (string-length e))
           (signed? (and (> n 0) (memv (string-ref e 0) '(#\+ #\-))))
           (digits (if signed? (substring e 1 n) e)))
      (if (string=? digits "")
        (cons 'fail (string-append "empty exponent `" token "`"))
        (let loop ((i 0) (value 0))
          (if (= i (string-length digits))
            (if (and signed? (char=? (string-ref e 0) #\-)) (- value) value)
            (let1 (d (digit-value (string-ref digits i) radix))
              (if d
                (loop (add1 i) (+ (* value radix) d))
                (cons 'fail (string-append "bad digit `" (string (string-ref digits i)) "`")))))))))

  ;; digits#, digits#/digits#, digits#.#*, [digits].digits#, where digits# is some digits followed by some #
  ;; placeholders that count as 0 and make the number inexact. Answers #(numerator denominator inexact?).
  (define (parse-mantissa s radix token)
    (let1 (n (string-length s))
      (let loop ((i 0) (part 'integer) (numerator 0) (digits 0) (scale 1) (denominator 0) (denominator-digits 0)
                 (hash? #f) (inexact? #f))
        (if (= i n)
          (cond
            ((and (eq? part 'denominator) (zero? denominator-digits)) (in-token "missing digits after `/`" token))
            ((zero? digits) (in-token "no digits" token))
            (else (vector numerator (* scale (if (eq? part 'denominator) denominator 1)) inexact?)))
          (let* ((c (string-ref s i))
                 (d (digit-value c radix)))
            (cond
              (d (cond
                   (hash? (in-token "misplaced `#`" token))
                   ((eq? part 'denominator)
                     (loop (add1 i) part numerator digits scale (+ (* denominator radix) d) (add1 denominator-digits) #f inexact?))
                   (else
                     (loop (add1 i) part (+ (* numerator radix) d) (add1 digits)
                           (if (eq? part 'fraction) (* scale radix) scale) denominator denominator-digits #f inexact?))))
              ((char=? c #\#)
                (cond
                  ((eq? part 'denominator)
                    (if (zero? denominator-digits)
                      (cons 'fail "bad digit `#`")
                      (loop (add1 i) part numerator digits scale (* denominator radix) (add1 denominator-digits) #t #t)))
                  ((zero? digits) (cons 'fail "bad digit `#`"))
                  (else (loop (add1 i) part (* numerator radix) (add1 digits)
                              (if (eq? part 'fraction) (* scale radix) scale) denominator denominator-digits #t #t))))
              ((char=? c #\.)
                (case part
                  ((denominator) (in-token "decimal points and fractions cannot be mixed" token))
                  ((fraction) (in-token "misplaced `.`" token))
                  (else (loop (add1 i) 'fraction numerator digits scale denominator denominator-digits hash? #t))))
              ((char=? c #\/)
                (case part
                  ((fraction) (in-token "decimal points and fractions cannot be mixed" token))
                  ((denominator) (in-token "misplaced `/`" token))
                  (else (if (zero? digits)
                          (cons 'fail "bad digit `/`")
                          (loop (add1 i) 'denominator numerator digits scale 0 0 #f inexact?)))))
              ((memv c '(#\+ #\-))
                (in-token (if (= i (sub1 n)) "no digits" "missing `i` for complex number") token))
              ((memv c '(#\i #\I)) (in-token "misplaced `i`" token))
              ((char=? c #\@) (in-token "misplaced `@`" token))
              (else (cons 'fail (string-append "bad digit `" (string c) "`")))))))))

  ;; The number of a part under the exactness prefix. It is exact when #e says so or nothing
  ;; made it inexact.
  (define (part-value p radix exactness token)
    (match1/first (#(,sign ,numerator ,denominator ,exponent ,inexact? ,special) p)
      (cond
        (special (if (eq? exactness 'exact)
                   (cons 'fail (string-append "no exact representation for " (racket-write->string special)))
                   special))
        ((zero? denominator) (cons 'error (string-append "division by zero in `" token "`")))
        ((or (eq? exactness 'exact) (and (not exactness) (not inexact?)))
          (* sign (/ numerator denominator) (expt radix exponent)))
        (else
          ;; Far beyond the flonums, the exact value is not worth computing.
          (let1 (bits (+ (- (integer-length numerator) (integer-length denominator))
                         (* exponent (/ (log radix) (log 2)))))
            (let1 (magnitude (cond
                               ((zero? numerator) 0.0)
                               ((> bits 1100) +inf.0)
                               ((< bits -1200) 0.0)
                               (else (rational->flonum (* (/ numerator denominator) (expt radix exponent))))))
              (if (< sign 0) (- magnitude) magnitude)))))))

  (define (->flonum x) (if (exact? x) (if (< x 0) (- (rational->flonum (- x))) (rational->flonum x)) x))

  ;; The flonum nearest to the exact non-negative rational r, ties to even, by exact integer arithmetic, as
  ;; Racket rounds a decimal it reads. q is r scaled by 2^-k to 53 bits (fewer for a subnormal).
  (define (rational->flonum r)
    (if (zero? r)
      0.0
      (let* ((n (numerator r))
             (d (denominator r)))
        (let loop ((k (max (- (integer-length n) (integer-length d) 54) -1074)))
          (let* ((num (if (< k 0) (* n (expt 2 (- k))) n))
                 (den (if (< k 0) d (* d (expt 2 k))))
                 (q (quotient num den))
                 (twice-remainder (* 2 (remainder num den))))
            (if (>= q (expt 2 53))
              (loop (add1 k))
              (let1 (q (cond
                         ((> twice-remainder den) (add1 q))
                         ((< twice-remainder den) q)
                         ((odd? q) (add1 q))
                         (else q)))
                (* (exact->inexact q) (expt 2.0 k)))))))))

  ;;; ==================== #LANG ====================

  ;; read-language : Sport -> (U Symbol #f), the language that a module file declares, cf. main.rkt:250. After
  ;; whitespace and comments, which may be #; datum comments and #! comment lines, it reads `#lang NAME`, with
  ;; exactly one space, or `#!NAME`, and answers NAME as a symbol; a name is made of ASCII letters, digits and
  ;; - + _ /, and ends at whitespace or at the end. Errors report at the #. When the first text is no such
  ;; line, it answers #f and leaves the input where it was.
  (define (read-language sp)
    (let1 (saved (sport-save sp))
      (skip-whitespace-and-comments! sp #f)
      (let ((start (here sp))
            (c (peek-at sp 1))
            (c2 (peek-at sp 2)))
        (cond
          ((and (eqv? (peek-at sp) #\#) (string=? (lookahead sp 5) "#lang"))
            (consume-n! sp 5)
            (unless (eqv? (peek-at sp) #\space) (read-error sp start "expected a single space after `#lang`"))
            (consume! sp)
            (let1 (next (peek-at sp))
              (cond
                ((not next)
                  (read-error sp start "expected a non-empty sequence of alphanumeric, `-`, `+`, `_`, or `/` after `#lang `"))
                ((char-whitespace? next) (read-error sp start "expected a single space after `#lang`"))
                (else (read-language-name sp start "#lang")))))
          ((and (eqv? (peek-at sp) #\#) (eqv? c #\!))
            (cond
              ((not c2) (consume-n! sp 2) (bad-syntax sp start "#!"))
              ((lang-name-char? c2) (consume-n! sp 2) (read-language-name sp start "#!"))
              (else (consume-n! sp 3) (bad-syntax sp start (string #\# #\! c2)))))
          (else (sport-restore! sp saved) #f)))))

  ;; The next n characters, fewer at the end.
  (define (lookahead sp n)
    (let loop ((k 0) (chars '()))
      (let1 (c (and (< k n) (peek-at sp k)))
        (if c (loop (add1 k) (cons c chars)) (list->string (reverse chars))))))

  (define (read-language-name sp start what)
    (let loop ((chars '()))
      (let1 (c (peek-at sp))
        (cond
          ((or (not c) (char-whitespace? c))
            (let* ((name (list->string (reverse chars)))
                   (n (string-length name)))
              (cond
                ((char=? (string-ref name 0) #\/)
                  (read-error sp start (string-append "expected a name that does not start `/` after `" what "`")))
                ((char=? (string-ref name (sub1 n)) #\/)
                  (read-error sp start (string-append "expected a name that does not end `/` after `" what "`")))
                (else (string->symbol name)))))
          ((lang-name-char? c) (consume! sp) (loop (cons c chars)))
          (else (read-error sp start (string-append "expected only alphanumeric, `-`, `+`, `_`, or `/` characters for `"
                                                    what "`, found `" (string c) "`")))))))

  ;;; ==================== STRINGS AS INPUT ====================

  ;; string->syntax : String [String] -> Syntax, the one datum that s holds, read from the source "string" by
  ;; default; the tests use it for the #'(...) of tests.rkt.
  (define (string->syntax s #!optional (source "string"))
    (let* ((sp (open-syntax-input source s))
           (datum (read-syntax sp)))
      (when (eof-object? datum) (error 'string->syntax "no datum in" s))
      (unless (eof-object? (read-syntax sp)) (error 'string->syntax "more than one datum in" s))
      datum)))
