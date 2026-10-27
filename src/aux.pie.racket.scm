;;; (aux pie racket) -- the parts of Racket itself that the port of Pie relies on.
;;;
;;; Pie, the language of "The Little Typer" by Daniel P. Friedman and David Thrane Christiansen
;;; (https://github.com/the-little-typer/pie, commit 2c89553), is written in Racket and leans on Racket's
;;; runtime in ways that CHICKEN does not share. This module has no upstream Pie file. It collects that
;;; platform behaviour in one place, so that the modules ported from Pie can follow upstream line by line.
;;; It holds no Pie code, but it belongs to the port and is distributed under the GNU AGPL-3.0 like the
;;; rest of it.
;;;
;;; Every piece imitates a Racket 8.15 source, and the oracle for each is what `racket` prints. They are
;;;   - structs        Racket's transparent `struct`, with a constructor, a predicate, accessors and
;;;                    mutators. CHICKEN's define-record tags are qualified by the module (m.x#PI), so an
;;;                    (aux base) pattern #(PI ,x ,A ,c) could not name them, and matchable's ($ PI ...)
;;;                    fails across modules. `define-struct` tags an instance with the bare name instead,
;;;                    which also makes CHICKEN's equal? compare two instances field by field, as Racket's
;;;                    equal? compares transparent structs. The price is that a tag names one type in the
;;;                    whole process, so another record made with the same bare tag would pass for it;
;;;   - boxes          racket/base `box`, `unbox`, `set-box!`, which CHICKEN lacks;
;;;   - reader values  the byte strings, regexps, hash tables, prefab structs and extflonums that Racket's
;;;                    reader makes, with the checks of prefab keys (racket/src/cs/rumble/prefab.ss) and of
;;;                    regexps (racket/src/regexp, its parser and validate.rkt) that the reader runs;
;;;   - write          the default printer of racket/src/io/print with print-reader-abbreviations #f, for
;;;                    the terms that syntax errors quote. CHICKEN's write bars :: and vec::, and writes
;;;                    numbers, characters and string escapes its own way. A hash table prints in the order
;;;                    of Racket CS's HAMT (rumble/hamt-stencil.ss), which follows the hash codes of the keys
;;;                    (rumble/hash-code.ss and the hash functions of Chez Scheme);
;;;   - numbers        Racket's number->string, which writes a flonum with the shortest digits that read
;;;                    back to it (CHICKEN's writes 15 significant digits) and an exponent without padding;
;;;   - letters        Racket's char-alphabetic?, the Alphabetic property of Unicode 15.1. CHICKEN 6 knows
;;;                    Unicode 17, where more code points are letters;
;;;   - printf         racket/src/io/format/printf.rkt for a format string given no arguments;
;;;   - string-split   racket/string string-split (collects/racket/string.rkt:129-167) with a string
;;;                    separator and #:trim? #t. CHICKEN's string-split drops the inner empty fields;
;;;   - string-ref     racket/base string-ref, whose bad index is a Racket contract error;
;;;   - UTF-8          the decoding of racket/src/io/string/utf-8-decode.rkt, which turns each byte that
;;;                    belongs to no valid encoding into an error character, as a file port does when it
;;;                    reads characters. CHICKEN 6 keeps such a byte in the string instead;
;;;   - ~a             racket/format ~a with #:align 'right, #:min-width and #:left-pad-string " ";
;;;   - srclocs        syntax/srcloc's source-location->string (collects/syntax/srcloc.rkt:173), and the
;;;                    srcloc->string of racket/base that the error display handler uses;
;;;   - exceptions     exn:fail:pie, exn:fail:syntax and exn:fail:read as CHICKEN composite conditions,
;;;                    and the default error display handler that prints them, minus its "context...:"
;;;                    block (Racket's stack trace), together with the error-print-width truncation of
;;;                    the values in a message.
;;;
;;; A Srcloc is the list (source line column position span) of basics.rkt:22, where source is a string.
;;;
;;; Not imitated are structs other than boxes in racket-write (they never reach it, because syntax->datum
;;; gives only symbols, numbers, strings, characters, booleans, keywords, lists, vectors, boxes and the byte
;;; strings, regexps, hash tables, prefab structs and extflonums of the reader), the printf directives that
;;; take arguments (Pie calls printf on a name only, cf. pretty.rkt:85 and 107), the order of a hash table
;;; with a key that Racket CS hashes by eq-hash-code, which the port prints in the order in which the keys
;;; were read (see THE ORDER OF A HASH TABLE), and the Unicode general categories. Racket writes a character
;;; of a string as \uXXXX when it is neither graphic nor blank, which needs the categories Cc, Cf, Co, Cn,
;;; Zl and Zp; CHICKEN has no table of them, so `racket-char-graphic?` lists the control, format, private
;;; use, separator and noncharacter code points, and an unassigned code point outside that list counts as
;;; graphic (Racket writes "\u0378", the port the character itself).

(module (aux pie racket)
  ( ; structs and boxes
    (define-struct struct-contract-violation) box box? unbox set-box!
    ; printing
    racket-write racket-write->string racket-number->string racket-truncate racket-printf racket-string-split
    racket-string-ref racket-pad-left racket-bytes->string/utf-8
    ; characters
    racket-char-alphabetic?
    ; the values of the reader
    racket-regexp racket-regexp? racket-regexp-source racket-regexp-px? racket-regexp-error
    racket-hash? racket-hash-kind racket-hash-pairs make-racket-hash
    racket-prefab racket-prefab? racket-prefab-key racket-prefab-fields
    prefab-key? prefab-key-compatible-count? normalize-prefab-key prefab-key-all-fields-immutable?
    racket-extflonum racket-extflonum? racket-extflonum-text
    ; the upstream package and source locations
    pie-upstream-directory pie-upstream-parser-source
    source-location->string source-location->prefix srcloc->string/relative racket-current-directory
    ; exceptions
    raise-racket-exn racket-exn? racket-exn-kind racket-exn-message racket-exn-srclocs display-racket-exn)

  (import scheme
          (chicken base)
          (only (scheme base) make-parameter exact-integer? bytevector? bytevector-length bytevector-u8-ref open-input-string)
          (only (chicken bitwise) bitwise-and bitwise-ior bitwise-xor arithmetic-shift integer-length)
          (only (chicken sort) sort)
          (only (scheme char) string-downcase string-upcase)
          (only (chicken port) call-with-output-string)
          (chicken memory representation)
          (chicken string)
          (chicken format)
          (chicken keyword)
          (chicken condition)
          (chicken pathname)
          (chicken file)
          (only (chicken process-context) current-directory get-environment-variable)
          (only (chicken file posix) file-stat)
          (only srfi-1 filter append-map any)
          (only srfi-13 string-index string-prefix? string-suffix?)
          (aux base))

  ;; The transformer of define-struct runs in every importer, so what it calls is imported for syntax too.
  (import-for-syntax scheme
                     (chicken base)
                     (chicken syntax)
                     (only srfi-1 iota)
                     (aux base))

  ;;; ==================== STRUCTS ====================

  ;; (define-struct NAME field ...) defines the constructor (NAME field ...), the predicate NAME?, the
  ;; accessor NAME-FIELD and the mutator set-NAME-FIELD! of every field, as Racket's
  ;; (struct NAME (field ...) #:transparent #:mutable) does under the constructor name of a Typed Racket
  ;; struct (cf. basics.rkt:59-71, 203-312). An instance is a CHICKEN record instance whose tag is the
  ;; symbol NAME itself, so (record->vector v) is #(NAME field-value ...) and the (aux base) pattern
  ;; #(NAME ,p ...) matches it. An accessor or a mutator given another value raises an error, as
  ;; Racket's do.
  (define-macro (define-struct (inject) (compare))
    ((_ ,name . ,fields)
      (let* ((tag (inject name))
             (indices (iota (length fields)))
             (accessor (λ (field) (inject name '- field)))
             (mutator (λ (field) (inject 'set- name '- field '!))))
        `(begin
           (define (,tag ,@fields) (make-record-instance (quote ,tag) ,@fields))
           (define (,(inject name '?) v) (record-instance? v (quote ,tag)))
           ,@(map (λ (field i)
                    `(define (,(accessor field) v)
                       (if (record-instance? v (quote ,tag))
                         (record-instance-slot v ,i)
                         (struct-contract-violation (quote ,(accessor field)) (quote ,tag) v))))
                  fields indices)
           ,@(map (λ (field i)
                    `(define (,(mutator field) v x)
                       (if (record-instance? v (quote ,tag))
                         (record-instance-slot-set! v ,i x)
                         (struct-contract-violation (quote ,(mutator field)) (quote ,tag) v))))
                  fields indices)
           (void)))))

  ;; The error of a struct accessor applied to another value, worded after Racket's contract violation.
  (define (struct-contract-violation who name v)
    (error who (format "contract violation\n  expected: ~a?\n  given: ~s" name v)))

  ;;; ==================== BOXES ====================

  ;; Racket's mutable boxes, a struct of one field; write prints them #&V. Pie keeps a DELAY-CLOS or its
  ;; value in one (cf. basics.rkt:205).
  (define-struct box value)
  (define unbox box-value)
  (define set-box! set-box-value!)

  ;;; ==================== VALUES OF THE READER ====================

  ;; Racket's reader makes values that no Pie term is made of. parser.rkt:235-394 matches none of them and
  ;; reports them as bad syntax, printing them after in:. The port's reader makes them too, as
  ;;   - byte strings, which are CHICKEN bytevectors;
  ;;   - (racket-regexp source px?), a regexp of #rx or #px. Its source is a string, or a byte string for the
  ;;     byte regexps of #rx# and #px#;
  ;;   - (racket-hash kind pairs), an immutable hash table of the kind equal, eq, eqv or equal-always. Its
  ;;     pairs (key . value) have distinct keys, the keys being plain data and the values syntax objects;
  ;;   - (racket-prefab key fields), a prefab struct whose key is normalized;
  ;;   - (racket-extflonum text), an extflonum. Racket CS has no extflonum arithmetic and reads one as the
  ;;     text that names it, the number without its # prefixes (1t0 for #x1t0), and prints that text back.
  (define-struct racket-regexp source px?)
  (define-struct racket-hash kind pairs)
  (define-struct racket-prefab key fields)
  (define-struct racket-extflonum text)

  ;; make-racket-hash : Symbol (Listof (Pair Any Any)) -> Racket-Hash, make-immutable-hash, or its eq, eqv or
  ;; equal-always variant, on the pairs that the reader read (cf. read/hash.rkt of the expander). A later pair
  ;; replaces an earlier one with the same key and takes its place. Two keys are the same when they are
  ;; equal?, for every kind, because the reader interns the literals it reads (datum-intern-literal), so two
  ;; equal strings or numbers are also eq?. The exception is two pairs, vectors, boxes, hash tables or prefab
  ;; structs, which are never eq? nor eqv?, so #hasheq(((1) . a) ((1) . b)) keeps both pairs (checked).
  (define (make-racket-hash kind pairs)
    (let1 (same-key? (if (memq kind '(eq eqv))
                       (λ (a b) (and (equal? a b) (not (compound-datum? a))))
                       equal?))
      (racket-hash kind
                   (let loop ((pairs pairs) (table '()))
                     (if (null? pairs)
                       (reverse table)
                       (let1 (p (car pairs))
                         (loop (cdr pairs)
                               (if (any (λ (q) (same-key? (car q) (car p))) table)
                                 (map (λ (q) (if (same-key? (car q) (car p)) p q)) table)
                                 (cons p table)))))))))

  (define (compound-datum? v)
    (or (pair? v) (vector? v) (box? v) (racket-hash? v) (racket-prefab? v)))

  ;; The prefab keys of racket/src/cs/rumble/prefab.ss and collects/racket/prefab.rkt, which the reader of
  ;; #s(KEY field ...) checks (cf. read/struct.rkt of the expander). A key is a symbol, or a list of a
  ;; name, an optional field count, an optional (auto-count auto-value), an optional vector of the indices of
  ;; the mutable fields, and then the same for each parent, whose field count must be there.

  ;; prefab-key? : Any -> Boolean, cf. rumble/prefab.ss:25
  (define (prefab-key? k)
    (or (symbol? k)
        (and (pair? k)
             (symbol? (car k))
             (let* ((k (cdr k))                 ; skip the name
                    (field-count (and (pair? k) (natural? (car k)) (car k)))
                    (k (if field-count (cdr k) k)))
               (let loop ((field-count field-count) (k k))
                 (or (null? k)
                     (and (pair? k)
                          (let* ((auto? (and (pair? (car k)) (pair? (cdar k)) (null? (cddar k)) (natural? (caar k))))
                                 (auto-count (if auto? (caar k) 0))
                                 (k (if auto? (cdr k) k)))
                            (or (null? k)
                                (and (pair? k)
                                     (let1 (k (if (vector? (car k))
                                                (and (mutable-indices? (car k) field-count auto-count) (cdr k))
                                                k))
                                       (and k
                                            (or (null? k)
                                                ;; a parent, a name with a field count
                                                (and (pair? k)
                                                     (symbol? (car k))
                                                     (pair? (cdr k))
                                                     (natural? (cadr k))
                                                     (loop (cadr k) (cddr k))))))))))))))))

  ;; The indices of a vector of mutable fields are naturals, distinct, and in range when the count is known.
  (define (mutable-indices? vec field-count auto-count)
    (let loop ((positions (vector->list vec)) (seen '()))
      (or (null? positions)
          (let1 (pos (car positions))
            (and (natural? pos)
                 (or (not field-count) (< pos (+ field-count auto-count)))
                 (not (memv pos seen))
                 (loop (cdr positions) (cons pos seen)))))))

  (define (natural? v) (and (exact-integer? v) (>= v 0)))

  ;; prefab-key-compatible-count? : Prefab-Key Natural -> Boolean, whether a valid key fits the given number
  ;; of fields, cf. rumble/prefab.ss:84
  (define (prefab-key-compatible-count? k total-field-count)
    (define (field-count-after-name+count k)
      (cond
        ((null? k) 0)
        ((pair? (car k)) (+ (caar k) (field-count-after-name+count+auto (cdr k))))
        (else (field-count-after-name+count+auto k))))
    (define (field-count-after-name+count+auto k)
      (cond
        ((null? k) 0)
        ((vector? (car k)) (if (null? (cdr k)) 0 (field-count (cdr k))))
        (else (field-count k))))
    (define (field-count k)                   ; k has a symbol and a count
      (+ (cadr k) (field-count-after-name+count (cddr k))))
    (cond
      ((symbol? k) #t)
      ((null? (cdr k)) #t)
      ;; the information must match exactly
      ((exact-integer? (cadr k)) (= total-field-count (+ (cadr k) (field-count-after-name+count (cddr k)))))
      (else
        (let1 (n (field-count-after-name+count (cdr k)))
          (and (>= total-field-count n)
               ;; the first vector of mutable fields, if any, must be in range for the field count
               (let* ((k (cdr k))
                      (auto (and (pair? (car k)) (car k)))
                      (k (if auto (cdr k) k)))
                 (or (null? k)
                     (not (vector? (car k)))
                     (let1 (n (- total-field-count (if auto (car auto) 0)))
                       (not (any (λ (m) (not (and (natural? m) (< m n)))) (vector->list (car k))))))))))))

  ;; normalize-prefab-key : Prefab-Key Boolean -> Prefab-Key, the normalized, compact key, without the first
  ;; field count unless keep-count? (the parents keep theirs), a zero auto count or an empty vector, and with
  ;; the mutable indices sorted, cf. rumble/prefab.ss:144
  (define (normalize-prefab-key k keep-count?)
    (if (symbol? k)
      k
      (let* ((name (car k))
             (k (cdr k))
             (count (and (pair? k) (natural? (car k)) (car k)))
             (k (if count (cdr k) k))
             (auto (and (pair? k) (pair? (car k)) (car k)))
             (k (if auto (cdr k) k))
             (mutables (and (pair? k) (vector? (car k)) (car k)))
             (k (if mutables (cdr k) k))
             (norm-auto (and auto (not (eqv? 0 (car auto))) auto))
             (norm-mutables (and mutables (> (vector-length mutables) 0)
                                 (list->vector (sort (vector->list mutables) <))))
             (r (if (null? k) '() (normalize-prefab-key k #t)))
             (r (if norm-mutables (cons norm-mutables r) r))
             (r (if norm-auto (cons norm-auto r) r)))
        (if keep-count?
          (cons name (cons count r))
          (if (null? r) name (cons name r))))))

  ;; prefab-key-all-fields-immutable? : Prefab-Key -> Boolean, whether no field of the key, its auto fields
  ;; and those of its parents included, is mutable, cf. collects/racket/prefab.rkt:17
  (define (prefab-key-all-fields-immutable? k)
    (or (symbol? k)
        (null? k)
        (let* ((rk (cdr k))                                            ; skip the name
               (rk (if (and (pair? rk) (exact-integer? (car rk))) (cdr rk) rk))   ; skip the count
               (rk (if (and (pair? rk) (pair? (car rk)))
                     (if (zero? (caar rk))
                       (cdr rk)                                        ; skip a zero auto count
                       (cons '#(1) (cdr rk)))                          ; an auto field is mutable
                     rk)))
          (if (and (pair? rk) (vector? (car rk)))
            (and (zero? (vector-length (car rk))) (prefab-key-all-fields-immutable? (cdr rk)))
            (prefab-key-all-fields-immutable? rk)))))

  ;;; ==================== WRITE ====================

  ;; racket-write : Any [Output-Port] -> Void, Racket's write with print-reader-abbreviations #f, so
  ;; (quote a) prints in full. It is how the at:, within: and in: lines of a syntax error show a term
  ;; (cf. runtime-report.rkt:739-780 in collects/syntax/parse/private).
  (define (racket-write v #!optional (port (current-output-port)))
    (cond
      ((null? v) (display "()" port))
      ((pair? v) (display "(" port)
                 (racket-write (car v) port)
                 (let loop ((rest (cdr v)))
                   (cond
                     ((null? rest) (void))
                     ((pair? rest) (display " " port) (racket-write (car rest) port) (loop (cdr rest)))
                     (else (display " . " port) (racket-write rest port))))
                 (display ")" port))
      ((vector? v) (display "#" port) (racket-write (vector->list v) port))
      ((box? v) (display "#&" port) (racket-write (unbox v) port))
      ((bytevector? v) (display (racket-bytes-literal v) port))
      ((racket-regexp? v) (display (if (racket-regexp-px? v) "#px" "#rx") port) (racket-write (racket-regexp-source v) port))
      ((racket-hash? v) (racket-write-hash v port))
      ((racket-prefab? v) (display "#s" port) (racket-write (cons (racket-prefab-key v) (racket-prefab-fields v)) port))
      ((racket-extflonum? v) (display (racket-extflonum-text v) port))
      ((keyword? v) (display "#:" port) (display (racket-symbol-name (keyword->string v) #t) port))
      ((symbol? v) (display (racket-symbol-name (symbol->string v) #f) port))
      ((string? v) (display (racket-string-literal v) port))
      ((char? v) (display (racket-char-literal v) port))
      ((boolean? v) (display (if v "#t" "#f") port))
      ((number? v) (display (racket-number->string v) port))
      (else (write v port))))

  ;; racket-write->string : Any -> String
  (define (racket-write->string v) (call-with-output-string (λ (port) (racket-write v port))))

  ;; How Racket writes a byte string, with the escapes \a \b \t \n \v \f \r \e \" \\, the other bytes from 32
  ;; to 126 as themselves, and any other byte in octal, padded to three digits when an octal digit comes next
  ;; (#"\0001", #"\08", #"\377", checked with Racket 8.15).
  (define (racket-bytes-literal bv)
    (let1 (n (bytevector-length bv))
      (call-with-output-string
        (λ (port)
          (display "#\"" port)
          (let loop ((i 0))
            (when (< i n)
              (let* ((b (bytevector-u8-ref bv i))
                     (escape (assv b '((7 . "\\a") (8 . "\\b") (9 . "\\t") (10 . "\\n") (11 . "\\v") (12 . "\\f")
                                       (13 . "\\r") (27 . "\\e") (34 . "\\\"") (92 . "\\\\")))))
                (cond
                  (escape (display (cdr escape) port))
                  ((<= 32 b 126) (write-char (integer->char b) port))
                  (else (let ((octal (number->string b 8))
                              (next (and (< (add1 i) n) (bytevector-u8-ref bv (add1 i)))))
                          (display "\\" port)
                          (when (and next (<= 48 next 55)) (display (make-string (- 3 (string-length octal)) #\0) port))
                          (display octal port))))
                (loop (add1 i)))))
          (display "\"" port)))))

  ;; How Racket writes a hash table, #hash((key . value) ...) with the prefix of its kind, cf. print-hash in
  ;; racket/src/io/print/hash.rkt:7-31. The pairs come in the order of try-sort (hash.rkt:35-58), sorted by
  ;; key when the keys are all reals, all symbols, all keywords, all strings or all byte strings, and
  ;; otherwise in the order of the table that hash-keys walks (see THE ORDER OF A HASH TABLE below).
  (define (racket-write-hash h port)
    (display (case (racket-hash-kind h) ((equal) "#hash(") ((eq) "#hasheq(") ((eqv) "#hasheqv(") (else "#hashalw(")) port)
    (let loop ((pairs (try-sort (racket-hash-table-order h))) (first? #t))
      (unless (null? pairs)
        (unless first? (display " " port))
        (display "(" port)
        (racket-write (caar pairs) port)
        (display " . " port)
        (racket-write (cdar pairs) port)
        (display ")" port)
        (loop (cdr pairs) #f)))
    (display ")" port))

  (define (try-sort pairs)
    (let1 (sorted (λ (ok? less?)
                    (and (not (any (λ (p) (not (ok? (car p)))) pairs))
                         (sort pairs (λ (a b) (less? (car a) (car b)))))))
      (or (sorted real? <)
          (sorted symbol? (λ (a b) (string<? (symbol->string a) (symbol->string b))))
          (sorted keyword? (λ (a b) (string<? (keyword->string a) (keyword->string b))))
          (sorted string? string<?)
          (sorted bytevector? bytes<?)
          pairs)))

  ;; Racket's bytes<?, the lexicographic order of the bytes.
  (define (bytes<? a b)
    (let ((n (bytevector-length a)) (m (bytevector-length b)))
      (let loop ((i 0))
        (cond
          ((= i m) #f)
          ((= i n) #t)
          ((< (bytevector-u8-ref a i) (bytevector-u8-ref b i)) #t)
          ((> (bytevector-u8-ref a i) (bytevector-u8-ref b i)) #f)
          (else (loop (add1 i)))))))

  ;;; ==================== THE ORDER OF A HASH TABLE ====================

  ;; Racket CS keeps an immutable hash table in a HAMT whose nodes are stencil vectors of 16 entries
  ;; (racket/src/cs/rumble/hamt-stencil.ss), and hash-keys (collects/racket/private/hash.rkt:22-32) walks it
  ;; by position, through bnode-entry-at-position (hamt-stencil.ss:946-982). A node holds first the keys that
  ;; are alone in their entry, then the child nodes, each group in the order of the entries, and the entry of
  ;; a key at depth d is the 4 bits of its hash code from bit 4d up (bnode-mask, hamt-stencil.ss:115-116). Two
  ;; keys that share an entry go down into a child node (node-merge, hamt-stencil.ss:334-358), and two keys
  ;; with the same hash code end in a collision node that keeps them in the order of insertion. The shape of
  ;; the trie depends only on the hash codes, so the order of the table is computed here from them, without
  ;; building it. The hash codes are those of rumble/hash-code.ss over the primitives of Chez Scheme
  ;; (s/newhash.ss, c/intern.c, and the $flhash of s/cpprim.ss:4623), for the keys whose hash code does not
  ;; depend on the history of the process. A string, a pair or a boolean in a #hasheq, a keyword, a regexp,
  ;; a prefab struct, a hash table or an extflonum is hashed by eq-hash-code, which numbers objects in the
  ;; order they are first hashed (hash-code.ss:20-53), so a table with such a key keeps the order in which
  ;; the keys were read.

  ;; racket-hash-table-order : Racket-Hash -> (Listof (Pair Any Any)), the pairs of h in the order of
  ;; hash-keys
  (define (racket-hash-table-order h)
    (let* ((pairs (racket-hash-pairs h))
           (key-hash-code (case (racket-hash-kind h)
                            ((eq) eq-hash-code)
                            ((eqv) eqv-hash-code)
                            ((equal) equal-hash-code)
                            (else equal-always-hash-code)))
           (codes (map (λ (p) (key-hash-code (car p))) pairs)))
      (if (memq #f codes)
        pairs
        (hamt-order (map cons codes pairs) 0))))

  ;; The pairs of the entries (hash-code . pair), given in the order of insertion, as a node of the HAMT at
  ;; the given shift holds them. Past the 60 bits of a hash code (HASHCODE-BITS, hamt-stencil.ss:332) all
  ;; the hash codes are equal and make a collision node.
  (define (hamt-order entries shift)
    (if (> shift 60)
      (map cdr entries)
      (let1 (groups (make-vector 16 '()))
        (for-each (λ (e) (let1 (bit (bitwise-and (fxsrl (car e) shift) 15))
                           (vector-set! groups bit (cons e (vector-ref groups bit)))))
                  (reverse entries))
        (let1 (groups (vector->list groups))
          (append (map (λ (g) (cdar g)) (filter (λ (g) (and (pair? g) (null? (cdr g)))) groups))
                  (append-map (λ (g) (hamt-order g (+ shift 4))) (filter (λ (g) (and (pair? g) (pair? (cdr g)))) groups)))))))

  ;; Racket CS fixnums have 61 bits, from -2^60 to 2^60 - 1, and the /wraparound operations of Chez Scheme
  ;; wrap around them. fxsrl shifts the 61 bits of the two's complement logically.
  (define fixnum-modulus 2305843009213693952)                   ; 2^61
  (define fixnum-half 1152921504606846976)                      ; 2^60
  (define racket-most-positive-fixnum 1152921504606846975)      ; 2^60 - 1

  (define (fxwrap n) (- (modulo (+ n fixnum-half) fixnum-modulus) fixnum-half))
  (define (fx+/wraparound a b) (fxwrap (+ a b)))
  (define (fxsll/wraparound a k) (fxwrap (arithmetic-shift a k)))
  (define (fxsrl a k) (arithmetic-shift (modulo a fixnum-modulus) (- k)))

  ;; The mixing step of Chez Scheme's hash functions, `update` in newhash.ss:1147-1149; mix-hash-code of
  ;; hash-code.ss:55-57 is (update hc 1).
  (define (update hc k)
    (let1 (hc2 (fx+/wraparound hc (fxsll/wraparound (fx+/wraparound hc k) 10)))
      (bitwise-xor hc2 (fxsrl hc2 6))))

  (define (mix-hash-code hc) (update hc 1))

  (define (hcabs hc) (if (< hc 0) (- -1 hc) hc))               ; cf. newhash.ss:1145

  ;; cf. hash-code.ss:335-338, where ->fx leaves the fixnums that these hash codes are as they are
  (define (hash-code-combine hc v)
    (let1 (mxhc (mix-hash-code hc))
      (fx+/wraparound mxhc (mix-hash-code (fx+/wraparound mxhc v)))))

  ;; eq-hash-code, eqv-hash-code and equal-hash-code of hash-code.ss:33-53, :94-98 and :102-103, or #f for a
  ;; key whose hash code depends on the history of the process
  (define (eq-hash-code x)
    (cond
      ((keyword? x) #f)                                           ; a CHICKEN keyword is also a symbol
      ((symbol? x) (symbol-hash x))
      ((number? x) (number-hash x))
      ((char? x) (char->integer x))
      (else #f)))

  (define eqv-hash-code eq-hash-code)

  (define (equal-hash-code x) (and (hashable? x #t) (receive (hc burn) (equal-hash-loop x 0 0) hc)))

  ;; equal-always-hash-code of hash-code.ss:105-106 hashes the immutable data as equal-hash-code does and
  ;; a mutable string or byte string by eq-hash-code. The reader interns the strings and byte strings that
  ;; are keys, which makes them immutable, but not those inside a key, a list for one (checked with Racket).
  (define (equal-always-hash-code x)
    (and (or (string? x) (bytevector? x) (hashable? x #f)) (equal-hash-code x)))

  ;; The data whose equal-hash-code the port computes, with strings and byte strings when strings? is true.
  (define (hashable? x strings?)
    (cond
      ((keyword? x) #f)
      ((or (boolean? x) (null? x) (number? x) (char? x) (symbol? x)) #t)
      ((or (string? x) (bytevector? x)) strings?)
      ((pair? x) (and (hashable? (car x) strings?) (hashable? (cdr x) strings?)))
      ((vector? x) (not (any (λ (y) (not (hashable? y strings?))) (vector->list x))))
      ((box? x) (hashable? (unbox x) strings?))
      (else #f)))

  ;; cf. equal-hash-loop in hash-code.ss:148-187, with recur #f. burn counts the work done, and hashing stops
  ;; once it passes MAX-HASH-BURN, 128.
  (define (equal-hash-loop x burn hc)
    (cond
      ((> burn 128) (values hc burn))
      ((boolean? x) (values (fx+/wraparound hc (if x #x0ace0120 #x0cafe121)) burn))
      ((null? x) (values (fx+/wraparound hc #x0cabd122) burn))
      ((number? x) (values (fx+/wraparound hc (number-hash x)) burn))
      ((char? x) (values (fx+/wraparound hc (char->integer x)) burn))
      ((symbol? x) (values (fx+/wraparound hc (symbol-hash x)) burn))
      ((string? x) (values (fx+/wraparound hc (string-hash x)) burn))
      ((bytevector? x) (values (fx+/wraparound hc (bytevector-equal-hash x)) burn))
      ((box? x) (equal-hash-loop (unbox x) (add1 burn) (mix-hash-code (fx+/wraparound hc 1))))
      ((pair? x)
        (receive (hc0 burn) (equal-hash-loop (car x) (+ burn 2) 0)
          (let ((hc (hash-code-combine hc hc0))
                (r (cdr x)))
            (if (and (pair? r) (list? r))
              (equal-hash-loop r (- burn 2) hc)        ; a list goes on without burning for its cdrs
              (equal-hash-loop r burn hc)))))
      (else                                            ; a vector
        (let1 (len (vector-length x))
          (if (zero? len)
            (values (fx+/wraparound hc 1) burn)
            (let loop ((i 0) (burn burn) (hc hc))
              (if (= i len)
                (values hc burn)
                (receive (hc0 burn) (equal-hash-loop (vector-ref x i) (+ burn 2) 0)
                  (loop (add1 i) burn (fx+/wraparound (mix-hash-code hc) hc0))))))))))

  ;; number-hash of hash-code.ss:73-92, from Chez Scheme's newhash.ss
  (define (number-hash z)
    (cond
      ((and (exact-integer? z) (<= (- fixnum-half) z racket-most-positive-fixnum))
        (if (< z 0) (bitwise-and z racket-most-positive-fixnum) z))
      ((exact-integer? z)                              ; a bignum, hashed 60 bits at a time
        (let1 (len (integer-length z))
          (let loop ((i 0) (hc 0))
            (if (>= i len)
              hc
              (let1 (next-i (+ i 60))
                (loop (if (< next-i 6100) next-i (max next-i (+ i (quotient (- len i) 2))))
                      (fx+/wraparound (bitwise-and (arithmetic-shift z (- i)) (sub1 (arithmetic-shift 1 (- next-i i))))
                                      (mix-hash-code hc))))))))
      ((flonum? z) (flonum-hash z))
      ((and (exact? z) (real? z)) (number-hash (+ (* (numerator z) 5) (denominator z))))
      (else (bitwise-and (bitwise-xor (- -1 (number-hash (real-part z))) (number-hash (imag-part z)))
                         racket-most-positive-fixnum))))

  ;; $flhash, inlined by s/cpprim.ss:4623-4636, is the 64 bits of the IEEE double shifted right by one and
  ;; read as a fixnum, which drops the 3 low tag bits, so the bits shifted right by four; #xfa1e for +nan.0
  (define (flonum-hash x)
    (if (not (= x x)) #xfa1e (arithmetic-shift (flonum-bits x) -4)))

  ;; The bits of a double, computed exactly from its value.
  (define (flonum-bits x)
    (let1 (sign (if (or (< x 0) (eqv? x -0.0)) (arithmetic-shift 1 63) 0))
      (cond
        ((zero? x) sign)
        ((not (finite? x)) (+ sign (arithmetic-shift 2047 52)))
        (else
          (let* ((q (inexact->exact (abs x)))
                 (e (let1 (e (- (integer-length (numerator q)) (integer-length (denominator q))))
                      (if (< q (expt 2 e)) (sub1 e) e))))          ; 2^e <= q < 2^(e+1)
            (if (>= e -1022)
              (+ sign (arithmetic-shift (+ e 1023) 52) (- (* (/ q (expt 2 e)) (expt 2 52)) (expt 2 52)))
              (+ sign (* q (expt 2 1074)))))))))                       ; subnormal

  ;; The code points of a string, read through a string port (see open-syntax-input in (aux pie syntax)).
  (define (string-codes s)
    (let1 (port (open-input-string s))
      (let loop ((codes '()))
        (let1 (c (read-char port))
          (if (eof-object? c) (list->vector (reverse codes)) (loop (cons (char->integer c) codes)))))))

  ;; symbol-hash, the hash code that Chez Scheme gives a symbol when it interns it, SYM_HASH_LOOP of
  ;; c/intern.c:103-119 on unsigned 64-bit integers over the code points of its name
  (define (symbol-hash sym)
    (let1 (codes (string-codes (symbol->string sym)))
      (let loop ((i 0) (h (+ (vector-length codes) 401887359)))
        (if (= i (vector-length codes))
          (bitwise-and h racket-most-positive-fixnum)
          (let* ((h (modulo (+ h (vector-ref codes i)) 18446744073709551616))
                 (h (modulo (+ h (arithmetic-shift h 10)) 18446744073709551616))
                 (h (bitwise-xor h (arithmetic-shift h -6))))
            (loop (add1 i) h))))))

  ;; string-hash and bytevector-hash of newhash.ss:1151-1188, which hash all the elements of a sequence of
  ;; at most 16 and otherwise the first 5, the last 5 and every (n/16)th from the sixth on
  (define (sequence-hash codes base)
    (let* ((n (vector-length codes))
           (step (λ (hc i) (update hc (vector-ref codes i)))))
      (if (<= n 16)
        (let loop ((i 0) (hc base)) (if (= i n) (hcabs hc) (loop (add1 i) (step hc i))))
        (let* ((hc (let loop ((i 0) (hc base)) (if (= i 5) hc (loop (add1 i) (step hc i)))))
               (hc (let loop ((i (- n 5)) (hc hc)) (if (= i n) hc (loop (add1 i) (step hc i)))))
               (stride (arithmetic-shift n -4)))
          (let loop ((i 5) (hc hc)) (if (>= i n) (hcabs hc) (loop (+ i stride) (step hc i))))))))

  (define (string-hash s) (sequence-hash (string-codes s) 523658599))

  ;; Chez Scheme's equal-hash of a byte string, newhash.ss:1248 and :1269-1270
  (define (bytevector-equal-hash bv)
    (let1 (codes (let1 (v (make-vector (bytevector-length bv)))
                   (let loop ((i 0)) (when (< i (bytevector-length bv)) (vector-set! v i (bytevector-u8-ref bv i)) (loop (add1 i))))
                   v))
      (hcabs (update 523658599 (sequence-hash codes 440697712)))))

  ;; Racket's char-whitespace? is the Unicode White_Space property, as CHICKEN 6's is.
  (define (racket-delimiter? c) (or (char-whitespace? c) (memv c '(#\( #\) #\[ #\] #\{ #\} #\" #\, #\' #\` #\; #\\))))

  ;; How Racket writes the name of a symbol, or of a keyword after its #: (checked with Racket 8.15). The
  ;; name needs quoting when it contains whitespace or a delimiter, when it starts with # but not with #%,
  ;; and, for a symbol only, when it is empty, is ".", or reads as a number. A name with a | in it has every
  ;; such character, and a leading #, escaped by a backslash (a\|b, a\|b\ c, \#a\|); any other name that
  ;; needs quoting is written between bars, where a backslash stands for itself (|a b|, |1|, |a\b|).
  (define (racket-symbol-name name keyword?)
    (let* ((n (string-length name))
           (hash-start? (and (> n 0) (char=? (string-ref name 0) #\#)
                             (not (and (> n 1) (char=? (string-ref name 1) #\%))))))
      (cond
        ((string-index name (λ (c) (char=? c #\|)))
          (call-with-output-string
            (λ (port)
              (let loop ((i 0))
                (when (< i n)
                  (let1 (c (string-ref name i))
                    (when (or (char=? c #\|) (racket-delimiter? c) (and (= i 0) hash-start?)) (write-char #\\ port))
                    (write-char c port)
                    (loop (add1 i))))))))
        ((or hash-start?
             (string=? name ".")
             (string-index name racket-delimiter?)
             (and (not keyword?) (or (string=? name "") (racket-number-string? name))))
          (string-append "|" name "|"))
        (else name))))

  ;; racket-number-string? : String -> Boolean, whether Racket's reader reads the string as a number in
  ;; decimal mode, after the grammar of the Racket Reference, section 1.3.3 "Reading Numbers". That is a
  ;; real, or real@real, or [real]±[ureal]i, where a real is [sign]ureal or ±inf.0 ±nan.0 (and the .f and .t
  ;; variants), and a ureal is digits#, digits#/digits#, [digits].digits# or digits#.#*, each with an
  ;; optional exponent of marker e s f d l or t. digits# is some digits followed by some # placeholders. A
  ;; string that starts with # never reaches it, so the radix and exactness prefixes are left out. "1/0"
  ;; counts, because Racket reads it as a number and fails there, and "1t0" counts, being an extflonum. An
  ;; extflonum is never part of a complex number, so the marker t and the specials .t make "1t0+1i",
  ;; "1+1t0i", "1t0@1" and "+inf.t+1i" symbols (checked with Racket 8.15); extflonum? records them.
  (define (racket-number-string? s)
    (let* ((s (string-downcase s))
           (n (string-length s))
           (extflonum? #f))
      (define (at i) (and (< i n) (string-ref s i)))
      (define (sign? i) (memv (at i) '(#\+ #\-)))
      (define (digit? i) (let1 (c (at i)) (and c (char<=? #\0 c #\9))))
      ;; Each scanner answers the index after what it read, or #f.
      (define (digits i) (and (digit? i) (let loop ((i i)) (if (digit? i) (loop (add1 i)) i))))
      (define (hashes i) (if (eqv? (at i) #\#) (hashes (add1 i)) i))
      (define (digits# i) (let1 (j (digits i)) (and j (hashes j))))
      (define (exponent i)
        (if (memv (at i) '(#\e #\s #\f #\d #\l #\t))
          (let* ((j (if (sign? (add1 i)) (+ i 2) (add1 i)))
                 (k (digits j)))
            (when (and k (eqv? (at i) #\t)) (set! extflonum? #t))
            (or k i))
          i))
      (define (simple i)
        (let1 (j (digits# i))
          (cond
            ((and j (eqv? (at j) #\/)) (digits# (add1 j)))
            ((and j (eqv? (at j) #\.)) (let1 (k (add1 j)) (if (= j (digits i)) (or (digits# k) k) (hashes k))))
            (j j)
            ((eqv? (at i) #\.) (digits# (add1 i)))
            (else #f))))
      (define (ureal i)
        (let1 (j (simple i))
          (and j (exponent j))))
      (define (special i)
        (and (sign? i)
             (let1 (word (and (<= (+ i 6) n) (substring s (add1 i) (+ i 6))))
               (and (member word '("inf.0" "inf.f" "inf.t" "nan.0" "nan.f" "nan.t"))
                    (begin (when (eqv? (string-ref word 4) #\t) (set! extflonum? #t)) (+ i 6))))))
      (define (real i) (or (special i) (ureal (if (sign? i) (add1 i) i))))
      (define (whole? j) (eqv? j n))
      (define (imaginary i)
        (and (sign? i)
             (let1 (j (or (special i) (ureal (add1 i)) (add1 i)))
               (and (eqv? (at j) #\i) (whole? (add1 j))))))
      (let1 (j (real 0))
        (cond
          ((whole? j) #t)
          ((imaginary 0) (not extflonum?))
          ((not j) #f)
          ((eqv? (at j) #\@) (and (whole? (real (add1 j))) (not extflonum?)))
          (else (and (imaginary j) (not extflonum?)))))))

  ;; How Racket writes a string, with the escapes \a \b \t \n \v \f \r \e \" \\, and \uXXXX (\UXXXXXXXX above
  ;; #xFFFF) for any other character that is neither graphic nor blank ("a\tb\u0001", checked).
  (define (racket-string-literal s)
    (call-with-output-string
      (λ (port)
        (write-char #\" port)
        (for-each
          (λ (c)
            (let1 (escape (assv c '((#\x7 . "\\a") (#\x8 . "\\b") (#\x9 . "\\t") (#\xA . "\\n") (#\xB . "\\v")
                                    (#\xC . "\\f") (#\xD . "\\r") (#\x1B . "\\e") (#\" . "\\\"") (#\\ . "\\\\"))))
              (cond
                (escape (display (cdr escape) port))
                ((or (racket-char-graphic? c) (racket-char-blank? c)) (write-char c port))
                (else (display (racket-char-hex c "\\") port)))))
          (string->list s))
        (write-char #\" port))))

  ;; How Racket writes a character, by the names #\nul #\backspace #\tab #\newline #\vtab #\page #\return
  ;; #\space #\rubout, a graphic character as itself (#\λ), and any other as #\uXXXX, so #  too.
  (define (racket-char-literal c)
    (let1 (name (assv c '((#\x0 . "nul") (#\x8 . "backspace") (#\x9 . "tab") (#\xA . "newline") (#\xB . "vtab")
                          (#\xC . "page") (#\xD . "return") (#\x20 . "space") (#\x7F . "rubout"))))
      (cond
        (name (string-append "#\\" (cdr name)))
        ((racket-char-graphic? c) (string-append "#\\" (string c)))
        (else (racket-char-hex c "#\\")))))

  ;; prefix + uXXXX, or UXXXXXXXX above #xFFFF, in upper-case hexadecimal.
  (define (racket-char-hex c prefix)
    (let* ((code (char->integer c))
           (width (if (> code #xFFFF) 8 4))
           (hex (string-upcase (number->string code 16))))
      (string-append prefix (if (> code #xFFFF) "U" "u") (make-string (- width (string-length hex)) #\0) hex)))

  ;; Racket's char-blank?, the category Zs and the tab.
  (define (racket-char-blank? c)
    (let1 (code (char->integer c))
      (or (= code 9) (= code #x20) (= code #xA0) (= code #x1680) (<= #x2000 code #x200A)
          (= code #x202F) (= code #x205F) (= code #x3000))))

  ;; Racket's char-graphic?, every category but the separators Z* and the others C*. The format characters
  ;; Cf are those of Unicode 15; an unassigned code point outside the noncharacters counts as graphic.
  (define (racket-char-graphic? c)
    (let1 (code (char->integer c))
      (not (or (<= code #x20) (<= #x7F code #xA0)
               (racket-char-blank? c) (= code #x2028) (= code #x2029)
               (any (λ (range) (<= (car range) code (cdr range)))
                     '((#xAD . #xAD) (#x600 . #x605) (#x61C . #x61C) (#x6DD . #x6DD) (#x70F . #x70F)
                       (#x890 . #x891) (#x8E2 . #x8E2) (#x180E . #x180E) (#x200B . #x200F) (#x202A . #x202E)
                       (#x2060 . #x2064) (#x2066 . #x206F) (#xFEFF . #xFEFF) (#xFFF9 . #xFFFB)
                       (#x110BD . #x110BD) (#x110CD . #x110CD) (#x13430 . #x1343F) (#x1BCA0 . #x1BCA3)
                       (#x1D173 . #x1D17A) (#xE0001 . #xE0001) (#xE0020 . #xE007F)
                       (#xD800 . #xDFFF) (#xE000 . #xF8FF) (#xF0000 . #xFFFFD) (#x100000 . #x10FFFD)
                       (#xFDD0 . #xFDEF)))
               (>= (modulo code #x10000) #xFFFE)))))

  ;; racket-char-alphabetic? : Char -> Boolean, Racket's char-alphabetic?, the Alphabetic property of the
  ;; Unicode 15.1 tables of Racket 8.15. CHICKEN 6's char-alphabetic? follows Unicode 17, which made 21867
  ;; more code points Alphabetic and none less (every code point compared with Racket). They are the ranges
  ;; of unicode-16+17-letters, which the predicate takes off CHICKEN's. Among them are the combining Latin
  ;; small letters U+0363..U+036F and the scripts that Unicode 16 and 17 added, Garay U+10D4A.. for one.
  (define (racket-char-alphabetic? c)
    (and (char-alphabetic? c)
         (let1 (code (char->integer c))
           (not (and (>= code #x363)
                     (any (λ (range) (<= (car range) code (cdr range))) unicode-16+17-letters))))))

  (define unicode-16+17-letters
    '((#x363 . #x36F) (#x558 . #x558) (#x58B . #x58C) (#x5C8 . #x5C9) (#x88F . #x88F) (#x897 . #x897)
      (#xC5C . #xC5C) (#xCDC . #xCDC) (#x1C89 . #x1C8A) (#x1DD3 . #x1DE6) (#x208F . #x208F) (#x209D . #x209F)
      (#xA7CB . #xA7CF) (#xA7D2 . #xA7D2) (#xA7D4 . #xA7D4) (#xA7DA . #xA7DD) (#xA7E2 . #xA7E2)
      (#xA7F1 . #xA7F1) (#xAB6C . #xAB6D) (#x105C0 . #x105F3) (#x107BB . #x107BF) (#x10940 . #x10959)
      (#x10D4A . #x10D65) (#x10D69 . #x10D69) (#x10D6F . #x10D85) (#x10EC2 . #x10EC7) (#x10ECB . #x10ECD)
      (#x10ED9 . #x10EEE) (#x10EF3 . #x10EF3) (#x10EF5 . #x10EF5) (#x10EF7 . #x10EF8) (#x10EFA . #x10EFC)
      (#x11380 . #x11389) (#x1138B . #x1138B) (#x1138E . #x1138E) (#x11390 . #x113B5) (#x113B7 . #x113C0)
      (#x113C2 . #x113C2) (#x113C5 . #x113C5) (#x113C7 . #x113CA) (#x113CC . #x113CD) (#x113D1 . #x113D1)
      (#x113D3 . #x113D3) (#x11B0A . #x11B0A) (#x11B60 . #x11B67) (#x11BC0 . #x11BE0) (#x11DB0 . #x11DDB)
      (#x11DF0 . #x11DF1) (#x1246F . #x1246F) (#x12475 . #x1247F) (#x12550 . #x12686) (#x13460 . #x143FA)
      (#x16100 . #x1612E) (#x16D40 . #x16D6C) (#x16EA0 . #x16EB8) (#x16EBB . #x16ED3) (#x16FF2 . #x16FF6)
      (#x187F8 . #x187FF) (#x18CD6 . #x18CDA) (#x18CFF . #x18CFF) (#x18D09 . #x18D20) (#x18D80 . #x18DF2)
      (#x18E00 . #x19191) (#x191A0 . #x191D2) (#x1B123 . #x1B128) (#x1B168 . #x1B168) (#x1D6A6 . #x1D6A6)
      (#x1DF1F . #x1DF24) (#x1DF2B . #x1DF81) (#x1DF90 . #x1DF96) (#x1DFCD . #x1DFFF) (#x1E5D0 . #x1E5ED)
      (#x1E5F0 . #x1E5F0) (#x1E6C0 . #x1E6DE) (#x1E6E0 . #x1E6F5) (#x1E6FE . #x1E6FF) (#x2B73A . #x2B73F)
      (#x2B81E . #x2B81E) (#x2CEA2 . #x2CEAD) (#x323B0 . #x33479) (#x3D000 . #x3FC3F)))

  ;; racket-number->string : Number -> String, how Racket writes a number, which is also what its
  ;; number->string gives in radix 10. Exact numbers print as in CHICKEN. A flonum prints its shortest digits
  ;; that read back to it, in positional notation when its decimal exponent e is between -4 and
  ;; max(13, digits + 2), otherwise as d.ddde±x (1000.0, 1e+14, 1e-5, 12345678901234567000.0, measured
  ;; with Racket 8.15). A complex number prints its two parts.
  (define (racket-number->string x)
    (cond
      ((not (real? x))
        (let ((re (racket-number->string (real-part x)))
              (im (racket-number->string (imag-part x))))
          (string-append re (if (memv (string-ref im 0) '(#\+ #\-)) "" "+") im "i")))
      ((exact? x) (number->string x))
      ((nan? x) "+nan.0")
      ((infinite? x) (if (> x 0) "+inf.0" "-inf.0"))
      ((zero? x) (if (eqv? x -0.0) "-0.0" "0.0"))
      ((< x 0) (string-append "-" (racket-number->string (- x))))
      (else (receive (digits e) (shortest-digits x)
              (let1 (n (string-length digits))
                (cond
                  ((not (<= -4 e (max 13 (+ n 2))))
                    (string-append (substring digits 0 1)
                                   (if (> n 1) (string-append "." (substring digits 1 n)) "")
                                   "e" (if (> e 0) "+" "") (number->string e)))
                  ((>= e (sub1 n)) (string-append digits (make-string (- e (sub1 n)) #\0) ".0"))
                  ((>= e 0) (string-append (substring digits 0 (add1 e)) "." (substring digits (add1 e) n)))
                  (else (string-append "0." (make-string (- (- e) 1) #\0) digits))))))))

  ;; The shortest digit string d1 d2 ... dn and the exponent e such that d1.d2...dn × 10^e reads back to
  ;; the positive flonum x, and among those of n digits the one closest to x, the upper one on a tie
  ;; (1118696272789288.3 for 4474785091157153/4, as Racket prints it). Everything is exact. A candidate
  ;; reads back to x when it lies in the interval of the reals that round to x, whose ends belong to it
  ;; when the mantissa of x is even (round half to even). CHICKEN's exact->inexact is not used, because it
  ;; does not always round a large rational correctly.
  (define (shortest-digits x)
    (let* ((r (inexact->exact x))
           (e (let loop ((e (inexact->exact (floor (/ (log x) (log 10))))))
                (cond ((> (expt 10 e) r) (loop (sub1 e))) ((<= (expt 10 (add1 e)) r) (loop (add1 e))) (else e))))
           (k (let loop ((k (inexact->exact (floor (/ (log x) (log 2))))))
                (cond ((> (expt 2 k) r) (loop (sub1 k))) ((<= (expt 2 (add1 k)) r) (loop (add1 k))) (else k))))
           (ulp (expt 2 (max (- k 52) -1074)))
           (mantissa (/ r ulp))
           (low (- r (if (and (= mantissa (expt 2 52)) (> k -1022)) (/ ulp 4) (/ ulp 2))))
           (high (+ r (/ ulp 2)))
           (reads-back? (λ (c) (or (< low c high) (and (even? mantissa) (or (= c low) (= c high)))))))
      (let loop ((p 1))
        (let* ((scale (expt 10 (- e (sub1 p))))
               (m (floor (+ (/ r scale) 1/2))))
          (if (or (reads-back? (* m scale)) (>= p 17))
            (let1 (digits (number->string m))
              (if (> (string-length digits) p)   ; m rounded up to 10^p
                (values (strip-trailing-zeros digits) (add1 e))
                (values (strip-trailing-zeros digits) e)))
            (loop (add1 p)))))))

  (define (strip-trailing-zeros digits)
    (let loop ((n (string-length digits)))
      (if (and (> n 1) (char=? (string-ref digits (sub1 n)) #\0)) (loop (sub1 n)) (substring digits 0 n))))

  ;; racket-truncate : String Natural -> String, how Racket's error messages cut a printed value down to
  ;; error-print-width, keeping the first width - 3 characters and "...". Syntax errors print at:, within: and in:
  ;; with a width of 256, and the terms of a parsing context with 50 (oracle parse-q101, parse-q102).
  (define (racket-truncate s width)
    (if (> (string-length s) width) (string-append (substring s 0 (- width 3)) "...") s))

  ;;; ==================== REGEXPS ====================

  ;; racket-regexp-error : (U String Bytevector) Boolean -> (U String #f), the message of the error that
  ;; Racket's regexp or byte-regexp (pregexp or byte-pregexp when px?) raises for the pattern p, or #f when
  ;; Racket accepts p. make-regexp (racket/src/regexp/match/regexp.rkt:46) can fail in two steps, the parser
  ;; (regexp/parse/main.rkt with range.rkt, class.rkt and unicode.rkt) and the checks of
  ;; analyze/validate.rkt, and the port has both, which escape with the message as upstream's regexp-error
  ;; aborts to its prompt (common/error.rkt). The parser builds only what the checks look at, which is
  ;;   - char, any matcher of one character (a literal, a range, `.`, `\d`), as rx-range and rx-alts of
  ;;     parse/ast.rkt merge them;
  ;;   - unicode, a \p{..} category, which matches one to four bytes;
  ;;   - the anchors empty, start and end, word-boundary and not-word-boundary, which match nothing;
  ;;   - the lists (sequence rx ...), (alts rx rx), (group rx number), (repeat rx min max), (maybe rx),
  ;;     (conditional test rx rx), (lookahead rx), (lookbehind rx), (cut rx) and (reference n), after the
  ;;     rx: structs of parse/ast.rkt.
  ;; The case-insensitive and multi-line modes change no check, so the parser reads them without keeping
  ;; them. The checks run on the pattern before a string regexp is converted to bytes (match/regexp.rkt:53),
  ;; which changes lengths but neither whether one is zero nor whether one is unbounded, the only facts that
  ;; the checks report on.
  (define (racket-regexp-error p px?)
    (call-with-current-continuation
      (λ (return)
        (let* ((s (if (string? p)
                    (list->vector (map char->integer (string->list p)))
                    (let1 (v (make-vector (bytevector-length p)))
                      (let loop ((i 0)) (when (< i (vector-length v)) (vector-set! v i (bytevector-u8-ref p i)) (loop (add1 i))))
                      v)))
               (n (vector-length s))
               (group-number 0))

          (define (parse-error message) (return message))

          ;; The character at pos, or #f at the end, the dispatch of chyte-case/eos (parse/chyte-case.rkt).
          (define (at pos) (and (< pos n) (integer->char (vector-ref s pos))))

          (define (ascii-letter? c) (and c (or (char<=? #\a c #\z) (char<=? #\A c #\Z))))
          (define (ascii-digit? c) (and c (char<=? #\0 c #\9)))

          ;; parse : -> Rx, cf. parse/main.rkt:15
          (define (parse)
            (receive (rx pos) (parse-regexp 0)
              (if (eqv? (at pos) #\)) (parse-error "unmatched `)` in pattern") rx)))

          ;; parse-regexp : Natural [Procedure] -> (values Rx Natural), cf. parse/main.rkt:28, which reads the
          ;; alternative after a | with again.
          (define (parse-regexp pos #!optional (again parse-regexp))
            (receive (rxs pos2) (parse-pces pos)
              (if (eqv? (at pos2) #\|)
                (receive (rx pos3) (again (add1 pos2))
                  (values (list 'alts (cons 'sequence rxs) rx) pos3))
                (values (cons 'sequence rxs) pos2))))

          ;; cf. parse/main.rkt:39
          (define (parse-regexp/maybe-empty pos)
            (if (eqv? (at pos) #\))
              (values 'empty pos)
              (parse-regexp pos parse-regexp/maybe-empty)))

          ;; parse-pces : Natural -> (values (Listof Rx) Natural), cf. parse/main.rkt:48
          (define (parse-pces pos)
            (if (= pos n)
              (values '() pos)
              (receive (rx pos2) (parse-pce pos)
                (if (memv (at pos2) '(#f #\| #\)))
                  (values (list rx) pos2)
                  (receive (rxs pos3) (parse-pces pos2)
                    (values (cons rx rxs) pos3))))))

          ;; parse-pce : Natural -> (values Rx Natural), an atom and its repetition, cf. parse/main.rkt:65
          (define (parse-pce pos)
            (receive (rx pos2) (parse-atom pos)
              (case (at pos2)
                ((#\*) (values (list 'repeat rx 0 'inf) (parse-non-greedy (add1 pos2))))
                ((#\+) (values (list 'repeat rx 1 'inf) (parse-non-greedy (add1 pos2))))
                ((#\?) (values (list 'maybe rx) (parse-non-greedy (add1 pos2))))
                ((#\{)
                  (if px?
                    (receive (n1 pos3) (parse-integer 0 (add1 pos2))
                      (case (at pos3)
                        ((#\,)
                          (receive (n2 pos4) (parse-integer 0 (add1 pos3))
                            (if (eqv? (at pos4) #\})
                              (values (list 'repeat rx n1 (if (= pos4 (add1 pos3)) 'inf n2)) (parse-non-greedy (add1 pos4)))
                              (parse-error "expected digit or `}` to end repetition specification started with `{`"))))
                        ((#\}) (values (list 'repeat rx n1 n1) (parse-non-greedy (add1 pos3))))
                        (else (parse-error "expected digit, `,`, or `}' for repetition specification started with `{`"))))
                    (values rx pos2)))
                (else (values rx pos2)))))

          ;; cf. parse/main.rkt:104
          (define (parse-non-greedy pos)
            (if (eqv? (at pos) #\?) (check-not-nested (add1 pos)) (check-not-nested pos)))

          ;; cf. parse/main.rkt:112
          (define (check-not-nested pos)
            (let1 (c (at pos))
              (case c
                ((#\? #\* #\+) (parse-error (string-append "nested `" (string c) "` in pattern")))
                ((#\{) (when px? (parse-error "nested `{` in pattern")))
                (else (void)))
              pos))

          ;; parse-integer : Natural Natural -> (values Natural Natural), cf. parse/main.rkt:282
          (define (parse-integer k pos)
            (if (ascii-digit? (at pos))
              (parse-integer (+ (* k 10) (- (vector-ref s pos) 48)) (add1 pos))
              (values k pos)))

          ;; parse-atom : Natural -> (values Rx Natural), at a character, cf. parse/main.rkt:126
          (define (parse-atom pos)
            (case (at pos)
              ((#\|) (values 'empty pos))
              ((#\() (parse-parenthesized-atom (add1 pos)))
              ((#\[) (values 'char (parse-range/not (add1 pos))))
              ((#\.) (values 'char (add1 pos)))
              ((#\^) (values 'start (add1 pos)))
              ((#\$) (values 'end (add1 pos)))
              (else (parse-literal pos))))

          ;; cf. parse/main.rkt:155
          (define (parse-parenthesized-atom pos)
            (case (at pos)
              ((#f) (missing-closing-error))
              ((#\?)
                (let1 (pos2 (add1 pos))
                  (case (at pos2)
                    ((#f) (bad-?-sequence-error))
                    ((#\>) (receive (rx pos3) (parse-regexp/maybe-empty (add1 pos2))
                             (values (list 'cut rx) (check-close-paren pos3))))
                    ((#\() (parse-conditional (add1 pos2)))
                    ((#\i #\s #\m #\- #\:)
                      (let1 (pos3 (parse-mode pos2))
                        (if (eqv? (at pos3) #\:)
                          (receive (rx pos4) (parse-regexp/maybe-empty (add1 pos3))
                            (values rx (check-close-paren pos4)))
                          (parse-error (string-append "expected `:` or another mode after `(?` and a mode sequence;\n"
                                                      " a mode is `i`, `-i`, `m`, `-m`, `s`, or `-s`")))))
                    (else (parse-look pos2)))))
              (else
                (let1 (number group-number)
                  (set! group-number (add1 group-number))
                  (receive (rx pos2) (parse-regexp/maybe-empty pos)
                    (values (list 'group rx number) (check-close-paren pos2)))))))

          ;; cf. parse/main.rkt:194. Racket 8.15 reports (?(? at the end as a bad (? sequence too.
          (define (parse-look pos2)
            (case (at pos2)
              ((#\= #\!) (receive (rx pos3) (parse-regexp/maybe-empty (add1 pos2))
                           (values (list 'lookahead rx) (check-close-paren pos3))))
              ((#\<) (let1 (pos2+ (add1 pos2))
                       (case (at pos2+)
                         ((#\= #\!) (receive (rx pos3) (parse-regexp/maybe-empty (add1 pos2+))
                                      (values (list 'lookbehind rx) (check-close-paren pos3))))
                         (else (bad-?-sequence-error)))))
              (else (bad-?-sequence-error))))

          ;; cf. parse/main.rkt:228
          (define (parse-conditional pos)
            (receive (test pos2) (parse-test pos)
              (receive (pces pos3) (parse-pces pos2)
                (case (at pos3)
                  ((#f) (missing-closing-error))
                  ((#\|)
                    (receive (pces2 pos4) (if (eqv? (at (add1 pos3)) #\))
                                            (values '() (add1 pos3))   ; an empty second alternative
                                            (parse-pces (add1 pos3)))
                      (case (at pos4)
                        ((#f) (missing-closing-error))
                        ((#\)) (values (list 'conditional test (cons 'sequence pces) (cons 'sequence pces2)) (add1 pos4)))
                        (else (parse-error "expected `)` to close `(?(...)...` after second branch")))))
                  (else (values (list 'conditional test (cons 'sequence pces) 'empty) (add1 pos3)))))))

          ;; cf. parse/main.rkt:261
          (define (parse-test pos)
            (case (at pos)
              ((#f) (missing-closing-error))
              ((#\?) (parse-look (add1 pos)))
              (else
                (if (ascii-digit? (at pos))
                  (receive (k pos3) (parse-integer 0 pos)
                    (unless (eqv? (at pos3) #\)) (parse-error "expected `)` after `(?(` followed by digits"))
                    (values (list 'reference k) (add1 pos3)))
                  (parse-error "expected `(?=`, `(?!`, `(?<`, or digit after `(?(`")))))

          ;; cf. parse/main.rkt:296, never at (, [, ., ^, $ or |
          (define (parse-literal pos)
            (let1 (c (at pos))
              (case c
                ((#\* #\+ #\?) (parse-error (string-append "`" (string c) "` follows nothing in pattern")))
                ((#\{) (if px? (parse-error "`{` follows nothing in pattern") (values 'char (add1 pos))))
                ((#\\) (parse-backslash-literal (add1 pos)))
                ((#\)) (parse-error "unmatched `)` in pattern"))
                ((#\] #\}) (if px?
                             (parse-error (string-append "unmatched `" (string c) "` in pattern"))
                             (values 'char (add1 pos))))
                (else (values 'char (add1 pos))))))

          ;; cf. parse/main.rkt:328. A backslash at the end matches the nul character.
          (define (parse-backslash-literal pos2)
            (let1 (c2 (at pos2))
              (cond
                ((not c2) (values 'char pos2))
                ((and px? (ascii-digit? c2))
                  (receive (k pos3) (parse-integer 0 pos2) (values (list 'reference k) pos3)))
                ((and px? (ascii-letter? c2))
                  (case c2
                    ((#\p #\P) (parse-unicode-categories c2 (add1 pos2)))
                    ((#\b) (values 'word-boundary (add1 pos2)))
                    ((#\B) (values 'not-word-boundary (add1 pos2)))
                    (else (if (class-letter? c2) (values 'char (add1 pos2)) (parse-error "illegal alphabetic escape")))))
                (else (values 'char (add1 pos2))))))

          ;; The letters of parse-class, \d \D \w \W \s \S, cf. parse/class.rkt:10
          (define (class-letter? c) (and (memv c '(#\d #\D #\w #\W #\s #\S)) #t))

          ;; parse-mode : Natural -> Natural, the modes i s m -i -s -m, cf. parse/main.rkt:362
          (define (parse-mode pos)
            (case (at pos)
              ((#\i #\s #\m) (parse-mode (add1 pos)))
              ((#\-) (if (memv (at (add1 pos)) '(#\i #\s #\m)) (parse-mode (+ pos 2)) pos))
              (else pos)))

          (define (check-close-paren pos)
            (unless (eqv? (at pos) #\)) (parse-error "expected a closing `)`"))
            (add1 pos))

          (define (missing-closing-error) (parse-error "missing closing parenthesis in pattern"))

          (define (bad-?-sequence-error)
            (parse-error "expected `:`, `=`, `!`, `<=`, `<!`, `i`, `-i`, `m`, `-m`, `s`, or `-s` after `(?`"))

          ;; The ranges [...] answer the position after them, cf. parse/range.rkt:15. span-from is the
          ;; character that may start a span a-z, must-span-from the one that a hyphen has started.
          (define (parse-range/not pos)
            (case (at pos)
              ((#f) (missing-square-closing-error))
              ((#\^) (parse-range (add1 pos)))
              (else (parse-range pos))))

          ;; cf. parse/range.rkt:27, where a first ] or - stands for itself
          (define (parse-range pos)
            (case (at pos)
              ((#f) (missing-square-closing-error))
              ((#\] #\-) (parse-range-rest (add1 pos) #f #f))
              (else (parse-range-rest pos #f #f))))

          ;; cf. parse/range.rkt:40
          (define (parse-range-rest pos span-from must-span-from)
            (case (at pos)
              ((#f) (missing-square-closing-error))
              ((#\]) (add1 pos))
              ((#\-)
                (let1 (pos2 (add1 pos))
                  (case (at pos2)
                    ((#f) (missing-square-closing-error))
                    ((#\]) (if must-span-from (misplaced-hyphen-error) (add1 pos2)))
                    (else (if span-from (parse-range-rest pos2 #f span-from) (misplaced-hyphen-error))))))
              ((#\\)
                (if px?
                  (let1 (pos2 (add1 pos))
                    (if (= pos2 n)
                      (parse-error "escaping backslash at end pattern (within square brackets)")
                      (let1 (c (at pos2))
                        (cond
                          ((and (ascii-letter? c) must-span-from) (misplaced-hyphen-error))
                          ((ascii-letter? c) (if (class-letter? c)
                                               (parse-range-rest (add1 pos2) #f #f)
                                               (parse-error "illegal alphabetic escape")))
                          (else (parse-range-rest/span (vector-ref s pos2) (add1 pos2) span-from must-span-from))))))
                  (parse-range-rest/span (vector-ref s pos) (add1 pos) span-from must-span-from)))
              ((#\[)
                (let1 (pos2 (and px? (not must-span-from) (parse-posix-char-class (add1 pos))))
                  (if pos2
                    (parse-range-rest pos2 #f #f)
                    (parse-range-rest/span (vector-ref s pos) (add1 pos) span-from must-span-from))))
              (else (parse-range-rest/span (vector-ref s pos) (add1 pos) span-from must-span-from))))

          ;; cf. parse/range.rkt:114
          (define (parse-range-rest/span c pos span-from must-span-from)
            (cond
              ((not must-span-from) (parse-range-rest pos c #f))
              ((> must-span-from c) (parse-error "invalid range within square brackets in pattern"))
              (else (parse-range-rest pos #f #f))))

          (define (missing-square-closing-error) (parse-error "missing closing square bracket in pattern"))

          (define (misplaced-hyphen-error) (parse-error "misplaced hyphen within square brackets in pattern"))

          ;; parse-posix-char-class : Natural -> (U Natural #f), [:alpha:] and the others after the [, the
          ;; position after their ], or #f for none, cf. parse/class.rkt:46
          (define (parse-posix-char-class pos)
            (and (eqv? (at pos) #\:)
                 (let loop ((pos (add1 pos)) (letters '()))
                   (let1 (c (at pos))
                     (cond
                       ((not c) #f)
                       ((char<=? #\a c #\z) (loop (add1 pos) (cons c letters)))
                       ((and (char=? c #\:) (eqv? (at (add1 pos)) #\]))
                         (and (member (list->string (reverse letters))
                                      '("alpha" "upper" "lower" "digit" "xdigit" "alnum" "word" "blank" "space"
                                        "graph" "print" "cntrl" "ascii"))
                              (+ pos 2)))
                       (else #f))))))

          ;; \p{..} and \P{..}, cf. parse/unicode.rkt:11
          (define (parse-unicode-categories p-c pos)
            (if (eqv? (at pos) #\{)
              (let loop ((pos (if (eqv? (at (add1 pos)) #\^) (+ pos 2) (add1 pos))) (chars '()))
                (case (at pos)
                  ((#f) (parse-error (string-append "missing `}` to close `\\" (string p-c) "{`")))
                  ((#\}) (let1 (name (list->string (reverse chars)))
                           (if (member name '("Ll" "Lu" "Lt" "Lm" "L&" "Lo" "L" "Nd" "Nl" "No" "N" "Ps" "Pe" "Pi"
                                              "Pf" "Pc" "Pd" "Po" "P" "Mn" "Mc" "Me" "M" "Sc" "Sk" "Sm" "So" "S"
                                              "Zl" "Zp" "Zs" "Z" "Cc" "Cf" "Cs" "Cn" "Co" "C" "."))
                             (values 'unicode (add1 pos))
                             (parse-error (string-append "unrecognized property name in `\\" (string p-c) "{}`: `" name "`")))))
                  (else (loop (add1 pos) (cons (at pos) chars)))))
              (parse-error (string-append "expected `{` after `\\" (string p-c) "`"))))

          ;; validate : Rx Natural -> Void, cf. analyze/validate.rkt:9. walk answers the minimum and maximum
          ;; length that rx matches, a maximum being a natural or inf (+inf.0 upstream, where (* 0 +inf.0) is
          ;; 0). group-sizes maps a group number to its minimum length; depends-sizes and must-sizes are the
          ;; sets of the group numbers that the references inside a repetition point to, keyed as upstream keys
          ;; them, n - 1 for the reference \n.
          (define (validate rx num-groups)
            (let ((group-sizes '()) (depends-sizes '()) (must-sizes '()))
              (define (might-be-empty-error) (parse-error "`*`, `+`, or `{...}` operand could be empty"))
              (define (inf+ a b) (if (or (eq? a 'inf) (eq? b 'inf)) 'inf (+ a b)))
              (define (inf* a b) (cond ((or (eqv? a 0) (eqv? b 0)) 0) ((or (eq? a 'inf) (eq? b 'inf)) 'inf) (else (* a b))))
              (define (inf-max a b) (if (or (eq? a 'inf) (eq? b 'inf)) 'inf (max a b)))
              (define (union a b) (append a (filter (λ (k) (not (memv k a))) b)))
              (define (walk rx)
                (match/first rx
                  (char (values 1 1))
                  (unicode (values 1 4))
                  ((sequence . ,rxs)
                    (let loop ((rxs rxs) (lo 0) (hi 0))
                      (if (null? rxs)
                        (values lo hi)
                        (receive (lo1 hi1) (walk (car rxs)) (loop (cdr rxs) (+ lo lo1) (inf+ hi hi1))))))
                  ((alts ,rx1 ,rx2)
                    (receive (lo1 hi1) (walk rx1)
                      (receive (lo2 hi2) (walk rx2) (values (min lo1 lo2) (inf-max hi1 hi2)))))
                  ((group ,rx ,number)
                    (receive (lo hi) (walk rx)
                      (set! group-sizes (cons (cons number lo) group-sizes))
                      (values lo hi)))
                  ((repeat ,rx ,rmin ,rmax)
                    (let1 (old-depends-sizes depends-sizes)
                      (set! depends-sizes '())
                      (receive (lo hi) (walk rx)
                        (when (zero? lo) (might-be-empty-error))
                        (set! must-sizes (union must-sizes depends-sizes))
                        (set! depends-sizes (union old-depends-sizes depends-sizes))
                        (values (* lo rmin) (inf* hi rmax)))))
                  ((maybe ,rx) (receive (lo hi) (walk rx) (values 0 hi)))
                  ((conditional ,test ,rx1 ,rx2)
                    (receive (lo0 hi0) (walk test)
                      (receive (lo1 hi1) (walk rx1)
                        (receive (lo2 hi2) (walk rx2) (values (min lo1 lo2) (inf-max hi1 hi2))))))
                  ((lookahead ,rx) (receive (lo hi) (walk rx) (values 0 0)))
                  ((lookbehind ,rx)
                    (receive (lo hi) (walk rx)
                      (when (eq? hi 'inf) (parse-error "lookbehind pattern does not match a bounded length"))
                      (values 0 0)))
                  ((cut ,rx) (walk rx))
                  ((reference ,k)
                    (unless (<= k num-groups) (parse-error "backreference number is larger than the highest-numbered cluster"))
                    (let1 (min-size (assv k group-sizes))
                      (if min-size
                        (values (cdr min-size) 'inf)
                        (begin (set! depends-sizes (union depends-sizes (list (sub1 k)))) (values 1 'inf)))))
                  (else (values 0 0))))                 ; empty, start, end and the word boundaries
              (receive (lo hi) (walk rx)
                (for-each (λ (k) (unless (let1 (size (assv k group-sizes)) (and size (positive? (cdr size))))
                                   (might-be-empty-error)))
                          must-sizes))))

          (validate (parse) group-number)
          #f))))

  ;;; ==================== PRINTF ====================

  ;; racket-printf : String -> Void, Racket's (printf format) with no arguments, cf. pretty.rkt:85 and 107,
  ;; where Pie prints a binder's name as a format string. The whole string is checked before anything is
  ;; printed. ~~ prints ~, ~n and ~% (in either case) print a newline, ~ followed by whitespace skips the
  ;; whitespace up to the first other character or up to the second end of line (\n, \r or \r\n), and any
  ;; other directive raises an error, the ones that take an argument because there is none (oracle
  ;; printf-tilde).
  (define (racket-printf format-string)
    (let* ((n (string-length format-string))
           (at (λ (i) (and (< i n) (string-ref format-string i))))
           (ill-formed (λ (explanation)
                         (error (string-append "printf: ill-formed pattern string\n  explanation: " explanation)))))
      ;; The index after the whitespace that ~ skips from i.
      (define (skip-whitespace i newlines)
        (let1 (c (at i))
          (cond
            ((not (and c (char-whitespace? c))) i)
            ((memv c '(#\newline #\return))
              (if (= newlines 1)
                i
                (skip-whitespace (if (and (eqv? c #\return) (eqv? (at (add1 i)) #\newline)) (+ i 2) (add1 i)) 1)))
            (else (skip-whitespace (add1 i) newlines)))))
      ;; The pieces to print, after checking the whole string.
      (define pieces
        (let loop ((i 0) (pieces '()) (arguments 0))
          (let1 (c (at i))
            (cond
              ((not c) (if (> arguments 0)
                         (error (format "printf: format string requires ~a arguments, given 0" arguments))
                         (reverse pieces)))
              ((not (char=? c #\~)) (loop (add1 i) (cons (string c) pieces) arguments))
              (else
                (let1 (d (at (add1 i)))
                  (cond
                    ((not d) (ill-formed "cannot end in `~`"))
                    ((char=? d #\~) (loop (+ i 2) (cons "~" pieces) arguments))
                    ((memv d '(#\n #\N #\%)) (loop (+ i 2) (cons "\n" pieces) arguments))
                    ((char-whitespace? d) (loop (skip-whitespace (add1 i) 0) pieces arguments))
                    ((memv (char-downcase d) '(#\a #\s #\v #\e #\c #\b #\o #\x))
                      (loop (+ i 2) pieces (add1 arguments)))
                    ((char=? d #\.)
                      (if (memv (at (+ i 2)) '(#\a #\A #\s #\S #\v #\V))
                        (loop (+ i 3) pieces (add1 arguments))
                        (ill-formed "tag `~.` not followed by `a`, `s`, or `v`")))
                    (else (ill-formed (string-append "tag `~" (string d) "` not allowed"))))))))))
      (for-each display pieces)))

  ;;; ==================== STRINGS ====================

  ;; racket-string-split : String String -> (Listof String), racket/string's (string-split str sep) with
  ;; #:trim? #t and #:repeat? #f, cf. show-goal.rkt:63 and 70. One sep is trimmed from each end, then the
  ;; rest is cut at every sep, keeping the empty fields between two of them, and an empty rest gives no
  ;; field at all ((racket-string-split "a\n\nb\n" "\n") => ("a" "" "b"), "\n\na\n\n" => ("" "a" ""),
  ;; checked with Racket).
  (define (racket-string-split str sep)
    (let* ((n (string-length str))
           (k (string-length sep))
           (start (if (and (>= n k) (string=? (substring str 0 k) sep)) k 0))
           (end (if (and (>= (- n start) k) (string=? (substring str (- n k) n) sep)) (- n k) n))
           (rest (substring str start end)))
      (if (string=? rest "")
        '()
        (let loop ((i 0) (fields '()))
          (let1 (j (substring-index sep rest i))
            (if j
              (loop (+ j k) (cons (substring rest i j) fields))
              (reverse (cons (substring rest i) fields))))))))

  ;; racket-string-ref : String Integer -> Char, racket/base's string-ref, cf. fresh.rkt:89-94. An index
  ;; that is not a natural number or lies past the end raises Racket's exn:fail:contract, worded as Racket
  ;; CS words it (checked with Racket 8.15), where CHICKEN's string-ref signals its own bounds error. The
  ;; values are written whole, without the error-print-width cut.
  (define (racket-string-ref str k)
    (cond
      ((not (and (exact-integer? k) (>= k 0)))
        (raise-racket-exn 'exn:fail:contract
                          (string-append "string-ref: contract violation\n"
                                         "  expected: exact-nonnegative-integer?\n"
                                         "  given: " (racket-write->string k) "\n"
                                         "  argument position: 2nd\n"
                                         "  first argument...:\n"
                                         "   " (racket-write->string str))
                          '()))
      ((>= k (string-length str))
        (raise-racket-exn 'exn:fail:contract
                          (if (zero? (string-length str))
                            (string-append "string-ref: index is out of range for empty string\n"
                                           "  index: " (racket-write->string k) "\n"
                                           "  string: " (racket-write->string str))
                            (string-append "string-ref: index is out of range\n"
                                           "  index: " (racket-write->string k) "\n"
                                           "  valid range: [0, " (racket-write->string (sub1 (string-length str))) "]\n"
                                           "  string: " (racket-write->string str)))
                          '()))
      (else (string-ref str k))))

  ;; racket-bytes->string/utf-8 : Bytevector Char -> String, racket/base's (bytes->string/utf-8 bv err-char),
  ;; which decodes as utf-8-decode! does with #:error-char err-char and #:abort-mode 'error
  ;; (io/string/utf-8-decode.rkt:43-206). Racket's file ports decode the same way, with #\uFFFD, when the
  ;; reader reads characters from them. An encoding that fails puts err-char in place of its first byte and
  ;; starts again at the next byte, so a bad sequence of n bytes gives n of them, and the bytes of #"a\342\200b"
  ;; give "a\uFFFD\uFFFDb". It fails on a byte that cannot start or continue an encoding, on an overlong
  ;; encoding, on a surrogate, on a code point above #x10FFFF, and on an encoding cut by the end of the bytes.
  ;; base is where the encoding being read starts, accum its bits so far and remaining the number of
  ;; continuation bytes it still needs, as in upstream.
  (define (racket-bytes->string/utf-8 bv err-char)
    (let1 (n (bytevector-length bv))
      (call-with-output-string
        (λ (out)
          (let loop ((i 0) (base 0) (accum 0) (remaining 0))
            (define (complete code) (write-char (integer->char code) out) (loop (add1 i) (add1 i) 0 0))
            (define (encoding-failure) (write-char err-char out) (loop (add1 base) (add1 base) 0 0))
            (define (start-encoding bits remaining) (loop (add1 i) i bits remaining))
            (define (continue-encoding bits remaining) (loop (add1 i) base bits remaining))
            (if (= i n)
              (unless (zero? remaining) (encoding-failure))
              (let1 (b (bytevector-u8-ref bv i))
                (cond
                  ((< b 128) (if (zero? remaining) (complete b) (encoding-failure)))
                  ((= #b10000000 (bitwise-and b #b11000000))         ; a continuation byte
                    (if (zero? remaining)
                      (encoding-failure)
                      (let1 (next (bitwise-ior (arithmetic-shift accum 6) (bitwise-and b #b111111)))
                        (cond
                          ((= remaining 1)
                            (if (and (> next 127) (<= next #x10FFFF) (not (<= #xD800 next #xDFFF)))
                              (complete next)
                              (encoding-failure)))
                          ((and (= remaining 2) (<= next #b11111)) (encoding-failure)) ; a shorter encoding would do
                          ((and (= remaining 3) (<= next #b1111)) (encoding-failure))
                          (else (continue-encoding next (sub1 remaining)))))))
                  ((not (zero? remaining)) (encoding-failure))      ; a new encoding inside another one
                  ((= #b11000000 (bitwise-and b #b11100000))
                    (let1 (bits (bitwise-and b #b11111))
                      (if (zero? bits) (encoding-failure) (start-encoding bits 1))))
                  ((= #b11100000 (bitwise-and b #b11110000)) (start-encoding (bitwise-and b #b1111) 2))
                  ((= #b11110000 (bitwise-and b #b11111000)) (start-encoding (bitwise-and b #b111) 3))
                  (else (encoding-failure))))))))))           ; five- and six-byte encodings

  ;; racket-pad-left : String Natural -> String, racket/format's
  ;; (~a s #:align 'right #:min-width width #:left-pad-string " "), cf. show-goal.rkt:32. A longer string
  ;; is kept whole.
  (define (racket-pad-left s width)
    (let1 (n (string-length s))
      (if (< n width) (string-append (make-string (- width n) #\space) s) s)))

  ;;; ==================== SOURCE LOCATIONS ====================

  ;; The directory of the upstream pie package as the oracle prints it. The Pi and ∏ rewrites of
  ;; parser.rkt:282 and 284 are located in upstream's parser.rkt, which the port names
  ;; (pie-upstream-parser-source).
  (define pie-upstream-directory (make-parameter "<pie>"))

  (define (pie-upstream-parser-source) (string-append (pie-upstream-directory) "/parser.rkt"))

  ;; source-location->string : Srcloc -> String, syntax/srcloc's good-string (srcloc.rkt:173), as
  ;; runtime-report.rkt and main.rkt use it. The source is the full path, except that a file of the
  ;; upstream package prints as <pkgs>/pie/FILE, as path->relative-string/library abbreviates the paths of
  ;; installed packages. Then :LINE:COL, or ::POS-END without a line, or nothing.
  (define (source-location->string srcloc)
    (match/first srcloc
      ((,source ,line ,column ,position ,span)
        (string-append
          (cond
            ((not (string? source)) (if source (format "~a" source) ""))
            ((string-prefix? (string-append (pie-upstream-directory) "/") source)
              (string-append "<pkgs>/pie/" (substring source (add1 (string-length (pie-upstream-directory))))))
            (else source))
          (cond
            ((and line column) (format ":~a:~a" line column))
            (line (format ":~a" line))
            ((and position span) (format "::~a-~a" position (+ position span)))
            (position (format "::~a" position))
            (else ""))))))

  ;; source-location->prefix : Srcloc -> String, the same followed by ": ", or "" when that is empty,
  ;; cf. main.rkt:172.
  (define (source-location->prefix srcloc)
    (let1 (s (source-location->string srcloc)) (if (string=? s "") "" (string-append s ": "))))

  ;; srcloc->string/relative : Srcloc -> String, racket/base's srcloc->string on the srcloc that Racket's
  ;; error display prints under location...:, and that the reader puts at the start of its messages. A
  ;; source that names an existing file becomes a path, as the prop:exn:srclocs of pie-err.rkt:12-15
  ;; makes it, and Racket prints a path relative to the current directory when it lies inside it; any
  ;; other source, such as "<pie>/parser.rkt", prints as it is. Then :LINE:COL, or ::POS, or nothing.
  (define (srcloc->string/relative srcloc)
    (match/first srcloc
      ((,source ,line ,column ,position ,span)
        (string-append
          (let1 (cwd (let1 (d (racket-current-directory)) (if (string-suffix? "/" d) d (string-append d "/"))))
            (if (and (string? source)
                     (file-exists? source)
                     (not (directory-exists? source))
                     (string-prefix? cwd source))
              (substring source (string-length cwd))
              (format "~a" source)))
          (cond
            ((and line column) (format ":~a:~a" line column))
            (position (format "::~a" position))
            (else ""))))))

  ;; racket-current-directory : -> String, the current directory as Racket CS starts with it. That is $PWD
  ;; when it is an absolute path naming the same directory as the physical path of getcwd, so that a
  ;; directory reached through a symbolic link keeps the name it was reached by, and the physical path
  ;; otherwise, when $PWD is unset, relative or names another directory (checked against Racket 8.15).
  (define (racket-current-directory)
    (let ((physical (current-directory))
          (logical (get-environment-variable "PWD")))
      (if (and logical
               (absolute-pathname? logical)
               (directory-exists? logical)
               (same-file? logical physical))
        logical
        physical)))

  ;; same-file? : String String -> Boolean, whether the two paths name the same file, that is the same
  ;; device and the same inode
  (define (same-file? a b)
    (let ((sa (file-stat a))
          (sb (file-stat b)))
      (and (= (vector-ref sa 0) (vector-ref sb 0))
           (= (vector-ref sa 9) (vector-ref sb 9)))))

  ;;; ==================== EXCEPTIONS ====================

  ;; raise-racket-exn : Symbol String (Listof Srcloc) -> Nothing, raises the Racket exception of kind
  ;; exn:fail:pie (pie-err.rkt:9), exn:fail:syntax (raised by syntax-parse and raise-syntax-error) or
  ;; exn:fail:read (raised by read-syntax). It is a CHICKEN composite condition of kinds exn, with the
  ;; message, racket, with the kind and the srclocs of prop:exn:srclocs, and the kind itself.
  (define (raise-racket-exn kind message srclocs)
    (abort (make-composite-condition
             (make-property-condition 'exn 'message message 'location #f 'arguments '())
             (make-property-condition 'racket 'kind kind 'srclocs srclocs)
             (make-property-condition kind))))

  ;; racket-exn? : Any [Symbol] -> Boolean, whether obj is a condition raised by raise-racket-exn, of the
  ;; given kind when there is one.
  (define (racket-exn? obj #!optional (kind #f))
    (and (condition? obj)
         ((condition-predicate 'racket) obj)
         (or (not kind) (eq? kind (racket-exn-kind obj)))))

  (define (racket-exn-kind c) (get-condition-property c 'racket 'kind))
  (define (racket-exn-message c) (get-condition-property c 'exn 'message))
  (define (racket-exn-srclocs c) (get-condition-property c 'racket 'srclocs))

  ;; display-racket-exn : Condition [Output-Port] -> Void, Racket's default error display handler without
  ;; the "context...:" block. It prints the message, then, when there are srclocs, "  location...:" and one line
  ;; per srcloc. A read error has no srclocs, its location being already in the message.
  (define (display-racket-exn c #!optional (port (current-error-port)))
    (display (racket-exn-message c) port)
    (let1 (srclocs (racket-exn-srclocs c))
      (unless (null? srclocs)
        (display "\n  location...:" port)
        (for-each (λ (srcloc) (display "\n   " port) (display (srcloc->string/relative srcloc) port)) srclocs)))
    (newline port)))
