
(module (aux simdjson) *

  (import
    scheme 
    (chicken base) 
    (chicken bytevector)
    (chicken fixnum)
    (chicken foreign) 
    (chicken pretty-print)
    (only (scheme base) string-for-each)
    srfi-1
    (aux base))

  #>

  #include <stdio.h>
  #include <stdlib.h>
  #include <string.h>
  #include "chicken-simdjson.h"

  <#

  

  (define-foreign-variable simdjson-type-signed-integer int "CHICKEN_SIMDJSON_TYPE_SIGNED_INTEGER")
  (define-foreign-variable simdjson-type-unsigned-integer int "CHICKEN_SIMDJSON_TYPE_UNSIGNED_INTEGER")
  (define-foreign-variable simdjson-type-floating-point-number int "CHICKEN_SIMDJSON_TYPE_FLOATING_POINT_NUMBER")
  (define-foreign-variable simdjson-type-string int "CHICKEN_SIMDJSON_TYPE_STRING")
  (define-foreign-variable simdjson-type-array int "CHICKEN_SIMDJSON_TYPE_ARRAY")
  (define-foreign-variable simdjson-type-object int "CHICKEN_SIMDJSON_TYPE_OBJECT")
  (define-foreign-variable simdjson-type-null int "CHICKEN_SIMDJSON_TYPE_NULL")
  (define-foreign-variable simdjson-type-boolean int "CHICKEN_SIMDJSON_TYPE_BOOLEAN")
  (define-foreign-variable simdjson-type-unknown int "CHICKEN_SIMDJSON_TYPE_UNKNOWN")
  (define-foreign-variable simdjson-type-error int "CHICKEN_SIMDJSON_TYPE_ERROR")

  (define (simdjson-signed-integer? t) (= t simdjson-type-signed-integer))
  (define (simdjson-unsigned-integer? t) (= t simdjson-type-unsigned-integer))
  (define (simdjson-floating-point-number? t) (= t simdjson-type-floating-point-number))
  (define (simdjson-string? t) (= t simdjson-type-string))
  (define (simdjson-array? t) (= t simdjson-type-array))
  (define (simdjson-object? t) (= t simdjson-type-object))
  (define (simdjson-null? t) (= t simdjson-type-null))
  (define (simdjson-boolean? t) (= t simdjson-type-boolean))
  (define (simdjson-unknown? t) (= t simdjson-type-unknown))
  (define (simdjson-error? t) (= t simdjson-type-error))

  (define simdjson-get-version (foreign-lambda (const c-string) "chicken_simdjson_get_version"))

  (define simdjson-get-type (foreign-lambda int "chicken_simdjson_get_type" (c-pointer "chicken_simdjson_dom_element_t")))
  (define simdjson-get-signed-integer (foreign-lambda integer64 "chicken_simdjson_get_signed_integer" (c-pointer "chicken_simdjson_dom_element_t")))
  (define simdjson-get-unsigned-integer (foreign-lambda unsigned-integer64 "chicken_simdjson_get_unsigned_integer" (c-pointer "chicken_simdjson_dom_element_t")))  
  (define simdjson-get-floating-point-number (foreign-lambda double "chicken_simdjson_get_floating_point_number" (c-pointer "chicken_simdjson_dom_element_t")))
  (define simdjson-get-boolean (foreign-lambda scheme-object "chicken_simdjson_get_boolean" (c-pointer "chicken_simdjson_dom_element_t")))
  (define simdjson-get-c-string (foreign-lambda c-string "chicken_simdjson_get_string" (c-pointer "chicken_simdjson_dom_element_t")))
  (define simdjson-get-string-length (foreign-lambda size_t "chicken_simdjson_get_string_length" (c-pointer "chicken_simdjson_dom_element_t")))
  (define simdjson-get-string-copy! (foreign-lambda void "chicken_simdjson_get_string_copy" (c-pointer "chicken_simdjson_dom_element_t") bytevector))
  (define simdjson-get-array-length (foreign-lambda size_t "chicken_simdjson_get_array_length" (c-pointer "chicken_simdjson_dom_element_t")))
  (define simdjson-get-array-ref (foreign-lambda (c-pointer "chicken_simdjson_dom_element_t") "chicken_simdjson_get_array_ref" (c-pointer "chicken_simdjson_dom_element_t") size_t))
  (define simdjson-get-object-length (foreign-lambda size_t "chicken_simdjson_get_object_length" (c-pointer "chicken_simdjson_dom_element_t")))
  (define simdjson-get-object-ref-c-key (foreign-lambda c-string "chicken_simdjson_get_object_ref_key" (c-pointer "chicken_simdjson_dom_element_t") size_t))
  (define simdjson-get-object-ref-key-length (foreign-lambda size_t "chicken_simdjson_get_object_ref_key_length" (c-pointer "chicken_simdjson_dom_element_t") size_t))
  (define simdjson-get-object-ref-key-copy! (foreign-lambda void "chicken_simdjson_get_object_ref_key_copy" (c-pointer "chicken_simdjson_dom_element_t") size_t bytevector))
  (define simdjson-get-object-ref-value (foreign-lambda (c-pointer "chicken_simdjson_dom_element_t") "chicken_simdjson_get_object_ref_value" (c-pointer "chicken_simdjson_dom_element_t") size_t))

  ; A JSON string may hold \u0000, where a c-string return value would stop.  So the C getters
  ; answer NULL, that is #f, for such a string, whose bytes are then copied by length and the
  ; buffer made a string in place, as the c-string conversion itself does: simdjson has already
  ; validated them as UTF-8.  Every other string keeps the one-call c-string path.
  (define (simdjson-get-string w)
    (or (simdjson-get-c-string w)
        (let* ((n (simdjson-get-string-length w)) (bv (make-bytevector (add1 n) 0)))
          (simdjson-get-string-copy! w bv)
          (##sys#buffer->string! bv n))))

  (define (simdjson-get-object-ref-key w i)
    (string->symbol
      (or (simdjson-get-object-ref-c-key w i)
          (let* ((n (simdjson-get-object-ref-key-length w i)) (bv (make-bytevector (add1 n) 0)))
            (simdjson-get-object-ref-key-copy! w i bv)
            (##sys#buffer->string! bv n)))))

  (define simdjson-free (foreign-lambda void "chicken_simdjson_free" (c-pointer "chicken_simdjson_dom_element_t")))

  (define simdjson-parse-ondemand-callback (foreign-lambda (c-pointer "chicken_simdjson_dom_element_t") "chicken_simdjson_parse_ondemand_callback" (const c-string) size_t))
  (define simdjson-load-ondemand-callback (foreign-lambda (c-pointer "chicken_simdjson_dom_element_t") "chicken_simdjson_load_ondemand_callback" (const c-string)))

  ; the callback wants the length of the text in BYTES, where string-length counts characters;
  ; CHICKEN refuses to marshal a string holding a NUL as a c-string, so strlen is exact
  (define simdjson-parse-ondemand-string
    (foreign-lambda* (c-pointer "chicken_simdjson_dom_element_t") (((const c-string) str))
      "C_return(chicken_simdjson_parse_ondemand_callback(str, strlen(str)));"))

  (define (simdjson->scheme w)
    (let1 (t (simdjson-get-type w))
          (cond
            ((simdjson-signed-integer? t) (simdjson-get-signed-integer w)) 
            ((simdjson-unsigned-integer? t) (simdjson-get-unsigned-integer w))
            ((simdjson-floating-point-number? t) (simdjson-get-floating-point-number w))
            ((simdjson-string? t) (simdjson-get-string w))
            ((simdjson-array? t) (let* ((n (simdjson-get-array-length w)) (vec (make-vector n)))
                                   (let loop ((i 0))
                                     (cond
                                       ((< i n) (vector-set! vec i (simdjson->scheme (simdjson-get-array-ref w i))) (loop (add1 i)))
                                       (else vec)))))
            ((simdjson-object? t) (let1 (n (simdjson-get-object-length w))
                                        (let loop ((i (sub1 n)) (lst '()))
                                          (cond
                                            ((>= i 0) (let* ((key (simdjson-get-object-ref-key w i))
                                                             (w* (simdjson-get-object-ref-value w i))
                                                             (key* (list key (simdjson->scheme w*)))
                                                             (lst* (cons key* lst)))
                                                        (loop (sub1 i) lst*)))
                                            (else lst)))))
            ((simdjson-null? t) (void))
            ((simdjson-boolean? t) (simdjson-get-boolean w))
            ((simdjson-error? t) (error 'simdjson (simdjson-get-string w)))
            ; the C side gives every node it builds a known type, or throws; w stays out of the
            ; irritants, since simdjson->scheme/free has freed the tree by the time a handler runs
            (else (error 'simdjson "unknown simdjson node type" t)))))

  ; converts the malloc'd tree a callback returns and frees it, also when the conversion raises
  ; (the error node of a rejected document)
  (define (simdjson->scheme/free dom)
    (dynamic-wind void (λ () (simdjson->scheme dom)) (λ () (simdjson-free dom))))

  (define (simdjson-load/ondemand filename)
    (simdjson->scheme/free (simdjson-load-ondemand-callback filename)))

  (define simdjson-load simdjson-load/ondemand)

  (define (simdjson-parse/ondemand str)
    (simdjson->scheme/free (simdjson-parse-ondemand-string str)))

  ; The callback wants BYTES and `string-length` answers in CHARACTERS.  A CHICKEN 6 string is
  ; UTF-8 inside, so the byte count is the length of its UTF-8 copy: one primitive copy instead of
  ; a walk summing per-codepoint widths, which cost ~30 ms on a 1 MB body.
  (define (json-utf8-length s) (bytevector-length (string->utf8 s)))

  ; the JSON validator ----------------------------------------------------------------------
  ;
  ; Sniffing for a leading `{` or `[` is not enough to tell whether a string is a well-formed JSON
  ; document, and callers that need a total predicate before handing a body to simdjson (or that
  ; restrict the root to an object or array, the default when `scalar?` is #f) use `json-well-formed?`.

  (define (json-char/digit? c) (and (char>=? c #\0) (char<=? c #\9)))

  (define (json-char/hex? c)
    (or (json-char/digit? c)
        (and (char>=? c #\a) (char<=? c #\f))
        (and (char>=? c #\A) (char<=? c #\F))))

  ; The JSON number grammar is not enough.  simdjson reads a literal with a `.` or an exponent as
  ; a double and raises NUMBER_ERROR when it rounds to infinity -- `1e309`, `-1e999` -- and also
  ; when the exponent field has twenty or more digits, leading zeros included, whatever its value.
  ; A literal without either is read as an integer, never as a double, and never overflows here.
  ;
  ; `string->number` cannot decide the rounding: it is not correctly rounded on this CHICKEN --
  ; it answers +inf.0 for 1.7976931348623158e308, which simdjson reads as the largest finite
  ; double -- and on a huge exponent it builds the exact power of ten.  So the magnitude is
  ; decided on the digits instead.  Writing the value as 0.DDD * 10^q with D1 non-zero, it
  ; overflows exactly when it reaches 2^1024 - 2^970, the midpoint between the largest double
  ; and 2^1024 (the midpoint rounds up, to even); that bound has 309 digits, so only q = 309 needs
  ; the digits themselves, and there a string comparison of the two digit runs is the numeric one.
  (define json-overflow-digits (number->string (- (expt 2 1024) (expt 2 970))))

  ; `s[m, b)` is a literal that already matched the number grammar, less its sign, with a
  ; fraction or an exponent: its integer digits end at `dot` and its exponent marker is at `e`
  ; (`e` = `b` without one, `dot` = `e` without a fraction).  It runs on every such literal of
  ; every body, so it allocates nothing on the way to its answer and uses fixnum arithmetic:
  ; the generic `min`, `*` and `+` tripled its cost, measured.
  (define (json-finite? s m dot e b)
    (if (and (fx= e b) (fx< (fx- dot m) 309))
        #t                                                  ; no exponent, few integer digits
        (let* ((sign (and (fx< e b) (let1 (c (string-ref s (fx+ e 1)))
                                      (and (or (char=? c #\+) (char=? c #\-)) c))))
               (x (cond (sign (fx+ e 2)) ((fx< e b) (fx+ e 1)) (else b)))   ; exponent digits
               (f (let F ((k m))                            ; the first significant digit
                    (if (and (fx< k e)
                             (let1 (c (string-ref s k)) (or (char=? c #\0) (char=? c #\.))))
                        (F (fx+ k 1))
                        k))))
          (cond
            ((fx> (fx- b x) 19) #f)                         ; simdjson gives up on the exponent
            ((fx>= f e) #t)                                 ; every digit is zero
            (else
              ; the exponent saturates far above any string length, so it stays a fixnum
              (let1 (q (fx+ (if (fx< f dot) (fx- dot f) (fx- (fx+ dot 1) f))
                            (let X ((k x) (v 0))
                              (cond
                                ((fx< k b)
                                 (X (fx+ k 1)
                                    (if (fx< v 100000000000000)
                                        (fx+ (fx* 10 v) (fx- (char->integer (string-ref s k)) 48))
                                        v)))
                                ((eqv? sign #\-) (fxneg v))
                                (else v)))))
                (or (fx< q 309)
                    (and (fx= q 309)
                         (string<? (list->string (remove (λ (c) (char=? c #\.))
                                                         (string->list (substring s f e))))
                                   json-overflow-digits)))))))))

  ; An integer literal -- no fraction, no exponent -- never overflows a double, but one beyond 64
  ; bits, below -2^63 or above 2^64 - 1, is simdjson's big_integer, which the binding refuses with
  ; BIGINT_ERROR.  JSON forbids leading zeros, so digit runs of equal length compare as strings
  ; exactly as they do as numbers.  `s[m, b)` are the digits.
  (define (json-integer-fits? s negative? m b)
    (let* ((bound (if negative? "9223372036854775808" "18446744073709551615"))
           (k (string-length bound))
           (n (fx- b m)))
      (or (fx< n k)
          (and (fx= n k) (not (string<? bound (substring s m b)))))))

  ; simdjson's binding visits the document by C++ recursion, one frame per level, and refuses
  ; nesting past simdjson's default of 1024 with DEPTH_ERROR, while this validator, riding
  ; CHICKEN's stack, would pass a million.  So nesting is bounded here, below simdjson's 1024.
  (define json-max-depth 512)

  (define (json-well-formed? s #!optional (scalar? #f))
    (and
      (string? s)
      (let* ((n (string-length s))
             (at (λ (i) (and (< i n) (string-ref s i)))))
        (letrec
          ; JSON whitespace is exactly these four.  NOT char-whitespace?, which is Unicode-aware
          ; here: it passed a form feed, VT, NBSP and U+2028, all of which simdjson answers with
          ; TAPE_ERROR.
          ((skip (λ (i) (if (and (< i n)
                                 (memv (string-ref s i) (list #\space #\tab #\newline #\return)))
                            (skip (add1 i))
                            i)))
           (word (λ (i w) (let1 (m (string-length w))
                            (and (<= (+ i m) n) (string=? w (substring s i (+ i m))) (+ i m)))))
           (digits (λ (i) (let D ((k i))
                            (if (and (< k n) (json-char/digit? (string-ref s k)))
                                (D (add1 k))
                                (and (> k i) k)))))
           (hex4 (λ (j) (and (<= (+ j 4) n)
                             (let1 (h (substring s j (+ j 4)))
                               (and (every json-char/hex? (string->list h))
                                    (string->number h 16))))))
           (number
             (λ (i)
               (let* ((m (if (eqv? #\- (at i)) (add1 i) i))
                      (dot (cond
                             ((eqv? #\0 (at m)) (add1 m))
                             ((and (at m) (json-char/digit? (at m))) (digits m))
                             (else #f))))
                 (and dot
                      (let1 (e (if (eqv? #\. (at dot)) (digits (add1 dot)) dot))
                        (and e
                             (let1 (j (let1 (c (at e))
                                        (if (memv c '(#\e #\E))
                                            (digits (if (memv (at (add1 e)) '(#\+ #\-))
                                                        (+ e 2)
                                                        (add1 e)))
                                            e)))
                               (and j
                                    (if (fx= dot j)
                                        (json-integer-fits? s (fx> m i) m j)
                                        (json-finite? s m dot e j))
                                    j))))))))
           (text
             (λ (i)
               (and (eqv? #\" (at i))
                    (let S ((k (add1 i)))
                      (let1 (c (at k))
                        (cond
                          ((not c) #f)
                          ((char=? c #\") (add1 k))
                          ((char=? c #\\)
                           (let1 (e (at (add1 k)))
                             (cond
                               ((not e) #f)
                               ((memv e (list #\" #\\ #\/ #\b #\f #\n #\r #\t)) (S (+ k 2)))
                               ; simdjson raises STRING_ERROR on a surrogate escape that is
                               ; not a high one followed by a low one, so pairing is checked
                               ((char=? e #\u)
                                (let1 (u (hex4 (+ k 2)))
                                  (cond
                                    ((not u) #f)
                                    ((<= #xDC00 u #xDFFF) #f)            ; a lone low surrogate
                                    ((<= #xD800 u #xDBFF)
                                     (and (eqv? #\\ (at (+ k 6)))
                                          (eqv? #\u (at (+ k 7)))
                                          (let1 (v (hex4 (+ k 8)))
                                            (and v (<= #xDC00 v #xDFFF) (S (+ k 12))))))
                                    (else (S (+ k 6))))))
                               (else #f))))
                          ((char<? c #\space) #f)          ; a raw control byte is not legal JSON
                          ; CHICKEN decodes each byte of invalid UTF-8 to #xDC00 + byte, a
                          ; surrogate no valid UTF-8 can yield, while the FFI hands simdjson the
                          ; original bytes -- UTF8_ERROR.  This one test covers every such byte.
                          ; Two 2-argument comparisons, which csc inlines: the 3-argument
                          ; char<=? is a variadic call per character, ~22 ms per MB of body.
                          ((and (char>=? c #\xD800) (char<=? c #\xDFFF)) #f)
                          (else (S (add1 k)))))))))
           ; `d` counts the containers enclosing position i
           (value
             (λ (i d)
               (let1 (i (skip i))
                 (let1 (c (at i))
                   (cond
                     ((not c) #f)
                     ((and (memv c (list #\{ #\[)) (>= d json-max-depth)) #f)
                     ((char=? c #\{) (object (add1 i) (add1 d)))
                     ((char=? c #\[) (array (add1 i) (add1 d)))
                     ((char=? c #\") (text i))
                     ((char=? c #\t) (word i "true"))
                     ((char=? c #\f) (word i "false"))
                     ((char=? c #\n) (word i "null"))
                     (else (number i)))))))
           (array
             (λ (i d)
               (let1 (i (skip i))
                 (if (eqv? #\] (at i))
                     (add1 i)
                     (let A ((i i))
                       (let1 (j (value i d))
                         (and j
                              (let1 (j (skip j))
                                (cond
                                  ((eqv? #\, (at j)) (A (add1 j)))
                                  ((eqv? #\] (at j)) (add1 j))
                                  (else #f))))))))))
           (object
             (λ (i d)
               (let1 (i (skip i))
                 (if (eqv? #\} (at i))
                     (add1 i)
                     (let O ((i i))
                       (let1 (k (text (skip i)))
                         (and k
                              (let1 (k (skip k))
                                (and (eqv? #\: (at k))
                                     (let1 (v (value (add1 k) d))
                                       (and v
                                            (let1 (v (skip v))
                                              (cond
                                                ((eqv? #\, (at v)) (O (add1 v)))
                                                ((eqv? #\} (at v)) (add1 v))
                                                (else #f)))))))))))))))
          (let1 (i (skip 0))
            (and (or scalar? (memv (at i) (list #\{ #\[)))
                 (let1 (j (value i 0))
                   (and j (= n (skip j))))))))))

  ; True when the UTF-8 bytes of a string can go out between quotes as they are: valid UTF-8
  ; holding neither the quote, the backslash nor a control character.  A surrogate code point
  ; (#xD800-#xDFFF, which integer->char accepts, and which CHICKEN makes of every byte of invalid
  ; UTF-8 it reads, keeping that byte as the character's encoding) fails the test, since what it
  ; encodes to is no UTF-8 at all, and a JSON parser rejects it.  A C loop: string-ref would walk
  ; the string from its start at every index, and a Scheme loop over the bytes costs more.
  (define utf8/json-plain?
    (foreign-lambda* bool ((bytevector b) (size_t n))
      "const unsigned char *s = b; size_t i = 0;
       while (i < n) {
         unsigned char c = s[i], lo = 0x80, hi = 0xBF; size_t k;
         if (c < 0x80) {
           if (c < 0x20 || c == '\"' || c == '\\\\') C_return(0);
           i++; continue;
         }
         if (c >= 0xC2 && c <= 0xDF) k = 1;
         else if (c >= 0xE0 && c <= 0xEF) { k = 2; if (c == 0xE0) lo = 0xA0; else if (c == 0xED) hi = 0x9F; }
         else if (c >= 0xF0 && c <= 0xF4) { k = 3; if (c == 0xF0) lo = 0x90; else if (c == 0xF4) hi = 0x8F; }
         else C_return(0);
         if (n - i <= k || s[i + 1] < lo || s[i + 1] > hi) C_return(0);
         for (size_t j = 2; j <= k; j++) if (s[i + j] < 0x80 || s[i + j] > 0xBF) C_return(0);
         i += k + 1;
       }
       C_return(1);"))

  ; JSON escapes only the quote, the backslash and the control characters, spelling the latter
  ; \uXXXX where Scheme `write` would emit its own \x01; syntax, and has no spelling for a
  ; surrogate code point, which is an error, as a number JSON cannot spell is; everything else
  ; goes out as is, and so does, in one piece, the common string that utf8/json-plain? passes.
  ; The procedure given to string-for-each returns its character because types.db declares it
  ; (#!rest char -> char): returning display's unspecified value draws a scrutinizer warning.
  (define (write-string/json s port)
    (write-char #\" port)
    (if (let1 (bytes (string->utf8 s)) (utf8/json-plain? bytes (bytevector-length bytes)))
        (display s port)
        (string-for-each
          (λ (c)
            (let1 (u (char->integer c))
              (cond
                ((char=? c #\") (display "\\\"" port))
                ((char=? c #\\) (display "\\\\" port))
                ((= u 8) (display "\\b" port))
                ((= u 9) (display "\\t" port))
                ((= u 10) (display "\\n" port))
                ((= u 12) (display "\\f" port))
                ((= u 13) (display "\\r" port))
                ((< u #x20) (display (string-append (if (< u #x10) "\\u000" "\\u00") (number->string u 16)) port))
                ((and (>= u #xD800) (<= u #xDFFF)) (error "JSON strings cannot hold surrogate code points" s))
                (else (write-char c port))))
            c)
          s))
    (write-char #\" port))

  ; A flonum as the fewest of 15, 16 or 17 significant digits that read back as the same double
  ; (17 always do).  display prints flonum-print-precision digits, 15 by default: too few for
  ; 0.30000000000000004, written 0.3, and for the largest double, written 1.79769313486232e+308,
  ; which is out of range and so no JSON number at all.  Where 15 digits do, the text is the
  ; same as display's, and an integral flonum keeps its ".0" as there, lest it read back as an
  ; exact integer.  The caller has ruled out infinities and NaN.
  (define flonum->string/json
    (foreign-lambda* c-string ((double x))
      "static char buffer[32]; int p;
       for (p = 15; p <= 17; p++) {
         snprintf(buffer, sizeof buffer - 2, \"%.*g\", p, x);
         if (p == 17 || strtod(buffer, NULL) == x) break;
       }
       if (strpbrk(buffer, \".eE\") == NULL) strcat(buffer, \".0\");
       C_return(buffer);"))

  ; JSON has neither ratios nor infinities and NaN: an exact non-integer goes out as the nearest
  ; flonum, and what is still not a finite real is an error
  (define (write-number/json n port)
    (let1 (x (if (and (exact? n) (not (integer? n))) (exact->inexact n) n))
      (cond
        ((not (and (real? x) (finite? x))) (error "JSON numbers must be finite reals" n))
        ((flonum? x) (display (flonum->string/json x) port))
        (else (display x port)))))

  (define (write-json w port)
    (let J ((v w))
      (cond
        ((number? v) (write-number/json v port))
        ((null? v) (display "{}" port))
        ((string? v) (write-string/json v port))
        ((symbol? v) (write-string/json (symbol->string v) port))
        ((char? v) (write-string/json (string v) port))
        ((boolean? v) (display (if v "true" "false") port))
        ((void? v) (display "null" port))
        ((vector? v) (begin (display "[" port)
                       (let loop ((i 0))
                         (when (< i (vector-length v))
                           (when (> i 0) (display "," port))
                           (J (vector-ref v i))
                           (loop (add1 i))))
                       (display "]" port)))
        ((pair? v) (begin (display "{" port)
                     (let loop ((lst v) (first? #t))
                       (when (pair? lst)
                         (when (not first?) (display "," port))
                         (let1 (keyvalue (car lst))
                           (unless (and (pair? keyvalue) (pair? (cdr keyvalue)) (null? (cddr keyvalue)))
                             (error "a JSON object entry must be a two-element list" keyvalue))
                           (let ((key (car keyvalue))
                                 (val (cadr keyvalue)))
                             (if (or (string? key) (symbol? key))
                                 (begin
                                   (write-string/json (if (symbol? key) (symbol->string key) key) port)
                                   (display ":" port)
                                   (J val))
                                 (error "a JSON object key must be a symbol or a string" keyvalue))))
                         (loop (cdr lst) #f)))
                     (display "}" port)))
        (else (error "cannot be represented as JSON" v)))))

  (define (->string/json w)
    (letport/output-string port (write-json w port)))

  (define json-escape         write-string/json)
  (define json-flonum->string flonum->string/json)
  (define json-emit           write-json)
  (define json-write          ->string/json)
  (define json-load           simdjson-load/ondemand)

  ; Gated parser: validates `s` with `json-well-formed?` (requiring an object or array root by
  ; default, or allowing a scalar root when `scalar?` is #t) and passes the UTF-8 byte count to
  ; `simdjson-parse-ondemand-callback`.
  (define (json-parse s #!optional (scalar? #f))
    (unless (json-well-formed? s scalar?)
      (error 'json-parse "not a well-formed JSON document" s))
    (simdjson->scheme/free (simdjson-parse-ondemand-callback s (json-utf8-length s))))

  ; object accessors and functional updates -------------------------------------------------
  ;
  ; simdjson decodes an object to a list of two-element (symbol value) lists, so the accessor is
  ; assq-then-cadr.  `alist-ref` from (chicken base) is a trap -- it returns the CDR, i.e. the
  ; one-element list (value).  `letassoc` from (aux base) would work but uses `assoc` and demands
  ; an `else` clause, so `ref` and `has?` would disagree on the comparator; keys are interned
  ; symbols, so `assq` is both correct and faster.

  (define (json-ref obj key #!optional (default (void)))
    (let1 (kv (and (pair? obj) (assq key obj)))
      (if (pair? kv) (cadr kv) default)))

  (define (json-has? obj key) (and (pair? obj) (pair? (assq key obj)) #t))

  (define (json-null? v) (void? v))

  (define (json-vector->list v) (if (vector? v) (vector->list v) '()))

  ; A key is compared by NAME, not with eq?: the encoder takes a string key as readily as a
  ; symbol, and "model" is not eq? to model -- nor to another "model" -- so an update spelled
  ; with strings would otherwise be appended as a SECOND key.  A hit keeps the key it found, so
  ; that setting "model" on a decoded object leaves the symbol `model` in place for `json-ref`
  ; and `json-has?`, which use `assq`.
  (define (json-key=? a b)
    (or (eq? a b)
        (equal? (if (symbol? a) (symbol->string a) a) (if (symbol? b) (symbol->string b) b))))

  (define (json-set obj key val)
    (let L ((o obj) (acc '()) (hit #f))
      (cond
        ((not (pair? o)) (reverse (if hit acc (cons (list key val) acc))))
        ((json-key=? key (car (car o)))
         (L (cdr o) (cons (list (car (car o)) val) acc) #t))
        (else (L (cdr o) (cons (car o) acc) hit)))))

  ; `foldl` in CHICKEN passes the ACCUMULATOR first -- (λ (o kv) ...) is the order.
  (define (json-merge obj patch)
    (if (pair? patch)
        (foldl (λ (o kv) (json-set o (car kv) (cadr kv))) obj patch)
        obj))

  )










