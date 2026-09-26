
(module (aux simdjson) *

  (import
    scheme 
    (chicken base) 
    (chicken bytevector)
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

  (define (->string/json w)
    (letport/output-string port
                           (let J ((v w))
                             (cond
                               ((number? v) (write-number/json v port))
                               ((null? v) (display "{}" port))
                               ((string? v) (write-string/json v port))
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
                                                (let* ((keyvalue (car lst))
                                                       (key (car keyvalue))
                                                       (val (cadr keyvalue)))
                                                  (if (or (string? key) (symbol? key))
                                                      (begin
                                                        (write-string/json (if (symbol? key) (symbol->string key) key) port)
                                                        (display ":" port)
                                                        (J val))
                                                      (error "JSON object keys must be strings")))
                                                (loop (cdr lst) #f)))
                                            (display "}" port)))
                               (else (warning "Cannot convert to JSON" v) (display "\"<unknown>\"" port))))))

  )










