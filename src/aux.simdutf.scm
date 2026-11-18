(module (aux simdutf) (simdutf-get-version simdutf-get-implementation
                       simdutf-error? simdutf-error-code simdutf-error-position
                       simdutf-valid-utf8? simdutf-valid-ascii? simdutf-valid-utf16le? simdutf-valid-utf16be?
                       simdutf-valid-utf32?
                       simdutf-validate-utf8 simdutf-validate-ascii simdutf-validate-utf16le
                       simdutf-validate-utf16be simdutf-validate-utf32
                       simdutf-count-utf8 simdutf-count-utf16le simdutf-count-utf16be
                       simdutf-autodetect-encoding simdutf-detect-encodings
                       simdutf-utf8->utf16le simdutf-utf8->utf16be simdutf-utf8->utf32 simdutf-utf8->latin1
                       simdutf-utf16le->utf8 simdutf-utf16be->utf8 simdutf-utf16le->utf32
                       simdutf-utf16be->utf32 simdutf-utf16le->latin1 simdutf-utf16be->latin1
                       simdutf-utf32->utf8 simdutf-utf32->utf16le simdutf-utf32->utf16be simdutf-utf32->latin1
                       simdutf-latin1->utf8 simdutf-latin1->utf16le simdutf-latin1->utf16be
                       simdutf-latin1->utf32
                       simdutf-utf8->utf16le/replacement simdutf-utf8->utf16be/replacement
                       simdutf-utf16le->utf8/replacement simdutf-utf16be->utf8/replacement
                       simdutf-to-well-formed-utf16le simdutf-to-well-formed-utf16be
                       simdutf-binary->base64 simdutf-base64->binary)

  (import
    scheme
    (chicken base)
    (chicken bitwise)
    (chicken bytevector)
    (chicken condition)
    (chicken foreign)
    (only (scheme base) bytevector-copy)
    (only srfi-1 find filter)
    (aux base))

  (foreign-declare "#include \"chicken-simdutf.h\"")

  ; The bytes of a bytevector, typed as the code units of the encoding at hand.  A bytevector's
  ; contents are word-aligned, so they can be read as char16_t and char32_t; the procedures below
  ; hand them to C only within a single foreign call, during which no GC can move them.
  (define-foreign-type u8* (scheme-pointer "char"))
  (define-foreign-type u16* (scheme-pointer "char16_t"))
  (define-foreign-type u32* (scheme-pointer "char32_t"))
  (define-foreign-type count* (scheme-pointer "size_t"))

  (define simdutf-get-version (foreign-lambda c-string "chicken_simdutf_get_version"))
  (define simdutf-get-implementation (foreign-lambda c-string "chicken_simdutf_get_implementation"))

  ; errors ------------------------------------------------------------------------------------
  ;
  ; simdutf_error_code, in the order of simdutf_c.h, which is that of simdutf::error_code.
  (define simdutf-error-codes
    '#(success header-bits too-short too-long overlong too-large surrogate invalid-base64-character
       base64-input-remainder base64-extra-bits output-buffer-too-small other))

  ; The struct results of simdutf_c.h come back through a cell: the foreign call returns the error
  ; code and writes the count, a position or a length, into a bytevector passed along with it.
  ; A let-location would do the same, but its location is valid only while no GC can move it.
  (define size_t-size (foreign-value "sizeof(size_t)" int))
  (define (make-count-cell) (make-bytevector size_t-size 0))
  (define count-cell-ref (foreign-lambda* size_t ((count* cell)) "C_return(*cell);"))

  (define (call/result f . args)
    (let* ((cell (make-count-cell))
           (code (apply f (append args (list cell)))))
      (values (vector-ref simdutf-error-codes code) (count-cell-ref cell))))

  ; An invalid input raises a composite condition (exn simdutf) whose simdutf part carries the
  ; error code, a symbol of simdutf-error-codes, and its position, counted as simdutf counts it:
  ; in code units of the input (bytes, char16_t or char32_t), or in characters of base64 text.
  (define (simdutf-raise who code position)
    (abort
      (make-composite-condition
        (make-property-condition 'exn 'location who
                                 'message (string-append "invalid input, " (symbol->string code)
                                                         " at position " (number->string position))
                                 'arguments (list code position))
        (make-property-condition 'simdutf 'code code 'position position))))

  (define simdutf-error? (condition-predicate 'simdutf))
  (define simdutf-error-code (condition-property-accessor 'simdutf 'code))
  (define simdutf-error-position (condition-property-accessor 'simdutf 'position))

  (define (check who code count)
    (unless (eq? code 'success) (simdutf-raise who code count)))

  ; inputs ------------------------------------------------------------------------------------
  ;
  ; A UTF-8 (or Latin-1, ASCII, base64, binary) input is a bytevector, or a string, which stands
  ; for its UTF-8 encoding.  A UTF-16 or UTF-32 input is a bytevector whose length is a multiple of
  ; its code unit; `units` answers that length in code units.
  (define (bytes who x)
    (cond
      ((bytevector? x) x)
      ((string? x) (string->utf8 x))
      (else (error who "neither a bytevector nor a string" x))))

  (define (units who bv size)
    (unless (bytevector? bv) (error who "not a bytevector" bv))
    (let1 (n (bytevector-length bv))
      (unless (zero? (remainder n size))
        (error who (string-append "the length in bytes is not a multiple of " (number->string size)) n))
      (quotient n size)))

  ; the first k bytes of bv, bv itself when it holds no more
  (define (prefix bv k)
    (if (= k (bytevector-length bv)) bv (bytevector-copy bv 0 k)))

  ; foreign entry points ------------------------------------------------------------------------
  ;
  ; (f in n count) or (f in n out count) for the functions of simdutf_c.h that answer a
  ; simdutf_result: the error code, with the count written into the cell.
  (define-syntax define-result
    (syntax-rules ()
      ((_ name cfun in-type)
       (define name
         (foreign-lambda* int ((in-type in) (size_t n) (count* count))
           "simdutf_result r = " cfun "(in, n); *count = r.count; C_return(r.error);")))
      ((_ name cfun in-type out-type)
       (define name
         (foreign-lambda* int ((in-type in) (size_t n) (out-type out) (count* count))
           "simdutf_result r = " cfun "(in, n, out); *count = r.count; C_return(r.error);")))))

  (define-result validate-utf8/errors "simdutf_validate_utf8_with_errors" u8*)
  (define-result validate-ascii/errors "simdutf_validate_ascii_with_errors" u8*)
  (define-result validate-utf16le/errors "simdutf_validate_utf16le_with_errors" u16*)
  (define-result validate-utf16be/errors "simdutf_validate_utf16be_with_errors" u16*)
  (define-result validate-utf32/errors "simdutf_validate_utf32_with_errors" u32*)

  (define-result convert-utf8->latin1/errors "simdutf_convert_utf8_to_latin1_with_errors" u8* u8*)
  (define-result convert-utf16le->latin1/errors "simdutf_convert_utf16le_to_latin1_with_errors" u16* u8*)
  (define-result convert-utf16be->latin1/errors "simdutf_convert_utf16be_to_latin1_with_errors" u16* u8*)
  (define-result convert-utf32->latin1/errors "simdutf_convert_utf32_to_latin1_with_errors" u32* u8*)

  (define-result utf8-length-from-utf16le/replacement* "simdutf_utf8_length_from_utf16le_with_replacement" u16*)
  (define-result utf8-length-from-utf16be/replacement* "simdutf_utf8_length_from_utf16be_with_replacement" u16*)

  ; the count of a simdutf_utf8_to_utf16_result, the char16_t of the conversion with replacement
  (define utf16-length-from-utf8/replacement
    (foreign-lambda* size_t ((u8* in) (size_t n))
      "C_return(simdutf_utf16_length_from_utf8_with_replacement(in, n).count);"))

  ; the count of utf8-length-from-utf16*/replacement*, whose error only tells whether a surrogate,
  ; paired or not, was met on the way
  (define ((length/replacement f) in n) (receive (_ count) (call/result f in n) count))
  (define utf8-length-from-utf16le/replacement (length/replacement utf8-length-from-utf16le/replacement*))
  (define utf8-length-from-utf16be/replacement (length/replacement utf8-length-from-utf16be/replacement*))

  (define valid-utf8 (foreign-lambda bool "simdutf_validate_utf8" u8* size_t))
  (define valid-ascii (foreign-lambda bool "simdutf_validate_ascii" u8* size_t))
  (define valid-utf16le (foreign-lambda bool "simdutf_validate_utf16le" u16* size_t))
  (define valid-utf16be (foreign-lambda bool "simdutf_validate_utf16be" u16* size_t))
  (define valid-utf32 (foreign-lambda bool "simdutf_validate_utf32" u32* size_t))

  (define count-utf8 (foreign-lambda size_t "simdutf_count_utf8" u8* size_t))
  (define count-utf16le (foreign-lambda size_t "simdutf_count_utf16le" u16* size_t))
  (define count-utf16be (foreign-lambda size_t "simdutf_count_utf16be" u16* size_t))

  (define autodetect-encoding (foreign-lambda int "simdutf_autodetect_encoding" u8* size_t))
  (define detect-encodings (foreign-lambda int "simdutf_detect_encodings" u8* size_t))

  (define utf16-length-from-utf8 (foreign-lambda size_t "simdutf_utf16_length_from_utf8" u8* size_t))
  (define utf32-length-from-utf8 (foreign-lambda size_t "simdutf_utf32_length_from_utf8" u8* size_t))
  (define utf8-length-from-utf16le (foreign-lambda size_t "simdutf_utf8_length_from_utf16le" u16* size_t))
  (define utf8-length-from-utf16be (foreign-lambda size_t "simdutf_utf8_length_from_utf16be" u16* size_t))
  (define utf8-length-from-utf32 (foreign-lambda size_t "simdutf_utf8_length_from_utf32" u32* size_t))
  (define utf16-length-from-utf32 (foreign-lambda size_t "chicken_simdutf_utf16_length_from_utf32" u32* size_t))
  (define utf8-length-from-latin1 (foreign-lambda size_t "simdutf_utf8_length_from_latin1" u8* size_t))

  (define convert-valid-utf8->utf16le (foreign-lambda size_t "simdutf_convert_valid_utf8_to_utf16le" u8* size_t u16*))
  (define convert-valid-utf8->utf16be (foreign-lambda size_t "simdutf_convert_valid_utf8_to_utf16be" u8* size_t u16*))
  (define convert-valid-utf8->utf32 (foreign-lambda size_t "simdutf_convert_valid_utf8_to_utf32" u8* size_t u32*))
  (define convert-valid-utf16le->utf8 (foreign-lambda size_t "simdutf_convert_valid_utf16le_to_utf8" u16* size_t u8*))
  (define convert-valid-utf16be->utf8 (foreign-lambda size_t "simdutf_convert_valid_utf16be_to_utf8" u16* size_t u8*))
  (define convert-valid-utf16le->utf32 (foreign-lambda size_t "simdutf_convert_valid_utf16le_to_utf32" u16* size_t u32*))
  (define convert-valid-utf16be->utf32 (foreign-lambda size_t "simdutf_convert_valid_utf16be_to_utf32" u16* size_t u32*))
  (define convert-valid-utf32->utf8 (foreign-lambda size_t "simdutf_convert_valid_utf32_to_utf8" u32* size_t u8*))
  ; simdutf_c.h has no convert_valid_utf32_to_utf16*: these validate again, on input already valid
  (define convert-utf32->utf16le (foreign-lambda size_t "simdutf_convert_utf32_to_utf16le" u32* size_t u16*))
  (define convert-utf32->utf16be (foreign-lambda size_t "simdutf_convert_utf32_to_utf16be" u32* size_t u16*))
  (define convert-latin1->utf8 (foreign-lambda size_t "simdutf_convert_latin1_to_utf8" u8* size_t u8*))
  (define convert-latin1->utf16le (foreign-lambda size_t "simdutf_convert_latin1_to_utf16le" u8* size_t u16*))
  (define convert-latin1->utf16be (foreign-lambda size_t "simdutf_convert_latin1_to_utf16be" u8* size_t u16*))
  (define convert-latin1->utf32 (foreign-lambda size_t "simdutf_convert_latin1_to_utf32" u8* size_t u32*))

  (define convert-utf8->utf16le/replacement (foreign-lambda size_t "simdutf_convert_utf8_to_utf16le_with_replacement" u8* size_t u16*))
  (define convert-utf8->utf16be/replacement (foreign-lambda size_t "simdutf_convert_utf8_to_utf16be_with_replacement" u8* size_t u16*))
  (define convert-utf16le->utf8/replacement (foreign-lambda size_t "simdutf_convert_utf16le_to_utf8_with_replacement" u16* size_t u8*))
  (define convert-utf16be->utf8/replacement (foreign-lambda size_t "simdutf_convert_utf16be_to_utf8_with_replacement" u16* size_t u8*))

  (define to-well-formed-utf16le (foreign-lambda void "simdutf_to_well_formed_utf16le" u16* size_t u16*))
  (define to-well-formed-utf16be (foreign-lambda void "simdutf_to_well_formed_utf16be" u16* size_t u16*))

  ; validation --------------------------------------------------------------------------------
  ;
  ; The predicates answer #t when the whole input is valid: a UTF-16 or UTF-32 bytevector whose
  ; length is no multiple of its code unit is not.  `simdutf-validate-*` answer two values, the
  ; error code and, on success, the code units of the input, otherwise the position of the error.
  (define ((valid? size f) x)
    (if (= size 1)
        (let1 (bv (bytes 'simdutf-valid? x)) (f bv (bytevector-length bv)))
        (and (bytevector? x)
             (zero? (remainder (bytevector-length x) size))
             (f x (quotient (bytevector-length x) size)))))

  (define simdutf-valid-utf8? (valid? 1 valid-utf8))
  (define simdutf-valid-ascii? (valid? 1 valid-ascii))
  (define simdutf-valid-utf16le? (valid? 2 valid-utf16le))
  (define simdutf-valid-utf16be? (valid? 2 valid-utf16be))
  (define simdutf-valid-utf32? (valid? 4 valid-utf32))

  (define (simdutf-validate-utf8 x)
    (let1 (bv (bytes 'simdutf-validate-utf8 x)) (call/result validate-utf8/errors bv (bytevector-length bv))))

  (define (simdutf-validate-ascii x)
    (let1 (bv (bytes 'simdutf-validate-ascii x)) (call/result validate-ascii/errors bv (bytevector-length bv))))

  (define (simdutf-validate-utf16le bv)
    (call/result validate-utf16le/errors bv (units 'simdutf-validate-utf16le bv 2)))

  (define (simdutf-validate-utf16be bv)
    (call/result validate-utf16be/errors bv (units 'simdutf-validate-utf16be bv 2)))

  (define (simdutf-validate-utf32 bv)
    (call/result validate-utf32/errors bv (units 'simdutf-validate-utf32 bv 4)))

  ; counting and detection -------------------------------------------------------------------
  ;
  ; The code points of VALID input: simdutf does not check it, and on invalid input the count
  ; is meaningless, though harmless.
  (define (simdutf-count-utf8 x)
    (let1 (bv (bytes 'simdutf-count-utf8 x)) (count-utf8 bv (bytevector-length bv))))

  (define (simdutf-count-utf16le bv) (count-utf16le bv (units 'simdutf-count-utf16le bv 2)))
  (define (simdutf-count-utf16be bv) (count-utf16be bv (units 'simdutf-count-utf16be bv 2)))

  ; simdutf_encoding_type, a bit each
  (define simdutf-encodings '((utf8 . 1) (utf16le . 2) (utf16be . 4) (utf32le . 8) (utf32be . 16)))

  ; the most likely encoding of the bytes, from a BOM or else by validation, #f when none fits
  (define (simdutf-autodetect-encoding x)
    (let* ((bv (bytes 'simdutf-autodetect-encoding x))
           (e (autodetect-encoding bv (bytevector-length bv))))
      (let1 (p (find (λ (p) (= (cdr p) e)) simdutf-encodings))
        (and p (car p)))))

  ; every encoding the bytes are valid in, among utf8, utf16le, utf32le (and a BOM's one)
  (define (simdutf-detect-encodings x)
    (let* ((bv (bytes 'simdutf-detect-encodings x))
           (mask (detect-encodings bv (bytevector-length bv))))
      (map car (filter (λ (p) (not (zero? (bitwise-and mask (cdr p))))) simdutf-encodings))))

  ; transcoding ------------------------------------------------------------------------------
  ;
  ; Every transcoder takes a bytevector (a string too, for UTF-8 and Latin-1 input, see `bytes`)
  ; and answers a fresh bytevector, or raises a simdutf condition on invalid input.  UTF-32 is in
  ; the byte order of the machine, as in simdutf, UTF-32LE on x86-64 and arm64.
  ;
  ; Most go in two passes: validation, which locates an error, then the conversion of valid input
  ; into a buffer of the exact size, which simdutf computes from the input.
  (define (transcode who in in-size validate out-size out-length convert)
    (let* ((in (if (= in-size 1) (bytes who in) in))
           (n (units who in in-size)))
      (receive (code count) (call/result validate in n)
        (check who code count)
        (let1 (out (make-bytevector (* out-size (out-length in n))))
          (convert in n out)
          out))))

  ; To Latin-1 in one pass, converting with errors into a buffer of a code unit per code unit of
  ; the input, which no Latin-1 output can outgrow, then cut to the bytes written.
  (define (transcode->latin1 who in in-size convert/errors)
    (let* ((in (if (= in-size 1) (bytes who in) in))
           (n (units who in in-size))
           (out (make-bytevector n)))
      (receive (code count) (call/result convert/errors in n out)
        (check who code count)
        (prefix out count))))

  ; Latin-1 is always valid, and so is any input of a conversion with replacement
  (define (always-valid in n count) 0)

  (define (simdutf-utf8->utf16le s)
    (transcode 'simdutf-utf8->utf16le s 1 validate-utf8/errors 2 utf16-length-from-utf8 convert-valid-utf8->utf16le))
  (define (simdutf-utf8->utf16be s)
    (transcode 'simdutf-utf8->utf16be s 1 validate-utf8/errors 2 utf16-length-from-utf8 convert-valid-utf8->utf16be))
  (define (simdutf-utf8->utf32 s)
    (transcode 'simdutf-utf8->utf32 s 1 validate-utf8/errors 4 utf32-length-from-utf8 convert-valid-utf8->utf32))
  (define (simdutf-utf8->latin1 s)
    (transcode->latin1 'simdutf-utf8->latin1 s 1 convert-utf8->latin1/errors))

  (define (simdutf-utf16le->utf8 bv)
    (transcode 'simdutf-utf16le->utf8 bv 2 validate-utf16le/errors 1 utf8-length-from-utf16le convert-valid-utf16le->utf8))
  (define (simdutf-utf16be->utf8 bv)
    (transcode 'simdutf-utf16be->utf8 bv 2 validate-utf16be/errors 1 utf8-length-from-utf16be convert-valid-utf16be->utf8))
  ; the UTF-32 code units of valid UTF-16 are its code points
  (define (simdutf-utf16le->utf32 bv)
    (transcode 'simdutf-utf16le->utf32 bv 2 validate-utf16le/errors 4 count-utf16le convert-valid-utf16le->utf32))
  (define (simdutf-utf16be->utf32 bv)
    (transcode 'simdutf-utf16be->utf32 bv 2 validate-utf16be/errors 4 count-utf16be convert-valid-utf16be->utf32))
  (define (simdutf-utf16le->latin1 bv)
    (transcode->latin1 'simdutf-utf16le->latin1 bv 2 convert-utf16le->latin1/errors))
  (define (simdutf-utf16be->latin1 bv)
    (transcode->latin1 'simdutf-utf16be->latin1 bv 2 convert-utf16be->latin1/errors))

  (define (simdutf-utf32->utf8 bv)
    (transcode 'simdutf-utf32->utf8 bv 4 validate-utf32/errors 1 utf8-length-from-utf32 convert-valid-utf32->utf8))
  (define (simdutf-utf32->utf16le bv)
    (transcode 'simdutf-utf32->utf16le bv 4 validate-utf32/errors 2 utf16-length-from-utf32 convert-utf32->utf16le))
  (define (simdutf-utf32->utf16be bv)
    (transcode 'simdutf-utf32->utf16be bv 4 validate-utf32/errors 2 utf16-length-from-utf32 convert-utf32->utf16be))
  (define (simdutf-utf32->latin1 bv)
    (transcode->latin1 'simdutf-utf32->latin1 bv 4 convert-utf32->latin1/errors))

  (define (simdutf-latin1->utf8 s)
    (transcode 'simdutf-latin1->utf8 s 1 always-valid 1 utf8-length-from-latin1 convert-latin1->utf8))
  (define (simdutf-latin1->utf16le s)
    (transcode 'simdutf-latin1->utf16le s 1 always-valid 2 (λ (in n) n) convert-latin1->utf16le))
  (define (simdutf-latin1->utf16be s)
    (transcode 'simdutf-latin1->utf16be s 1 always-valid 2 (λ (in n) n) convert-latin1->utf16be))
  (define (simdutf-latin1->utf32 s)
    (transcode 'simdutf-latin1->utf32 s 1 always-valid 4 (λ (in n) n) convert-latin1->utf32))

  ; Transcoding that never fails: each maximal ill-formed subsequence of UTF-8, or unpaired
  ; surrogate of UTF-16, becomes U+FFFD, as the WHATWG Encoding Standard and `TextDecoder` do.
  (define (simdutf-utf8->utf16le/replacement s)
    (transcode 'simdutf-utf8->utf16le/replacement s 1 always-valid 2 utf16-length-from-utf8/replacement convert-utf8->utf16le/replacement))
  (define (simdutf-utf8->utf16be/replacement s)
    (transcode 'simdutf-utf8->utf16be/replacement s 1 always-valid 2 utf16-length-from-utf8/replacement convert-utf8->utf16be/replacement))
  (define (simdutf-utf16le->utf8/replacement bv)
    (transcode 'simdutf-utf16le->utf8/replacement bv 2 always-valid 1 utf8-length-from-utf16le/replacement convert-utf16le->utf8/replacement))
  (define (simdutf-utf16be->utf8/replacement bv)
    (transcode 'simdutf-utf16be->utf8/replacement bv 2 always-valid 1 utf8-length-from-utf16be/replacement convert-utf16be->utf8/replacement))

  ; the same UTF-16, each unpaired surrogate replaced by U+FFFD
  (define (simdutf-to-well-formed-utf16le bv)
    (let* ((n (units 'simdutf-to-well-formed-utf16le bv 2)) (out (make-bytevector (* 2 n))))
      (to-well-formed-utf16le bv n out)
      out))
  (define (simdutf-to-well-formed-utf16be bv)
    (let* ((n (units 'simdutf-to-well-formed-utf16be bv 2)) (out (make-bytevector (* 2 n))))
      (to-well-formed-utf16be bv n out)
      out))

  ; base64 -----------------------------------------------------------------------------------
  ;
  ; simdutf_base64_options and simdutf_last_chunk_handling_options, by name
  (define base64-options
    '((default . 0) (url . 1) (default-no-padding . 2) (url-with-padding . 3)
      (default-accept-garbage . 4) (url-accept-garbage . 5) (default-or-url . 8)
      (default-or-url-accept-garbage . 12)))

  (define base64-last-chunk-options
    '((loose . 0) (strict . 1) (stop-before-partial . 2) (only-full-chunks . 3)))

  (define (option who table name)
    (let1 (p (assq name table))
      (if p (cdr p) (error who "unknown option" name (map car table)))))

  (define base64-length-from-binary (foreign-lambda size_t "simdutf_base64_length_from_binary" size_t int))
  (define base64-length-from-binary/lines (foreign-lambda size_t "simdutf_base64_length_from_binary_with_lines" size_t int size_t))
  (define binary->base64
    (foreign-lambda* size_t ((u8* in) (size_t n) (u8* out) (int options))
      "C_return(simdutf_binary_to_base64(in, n, out, (simdutf_base64_options) options));"))
  (define binary->base64/lines
    (foreign-lambda* size_t ((u8* in) (size_t n) (u8* out) (size_t line_length) (int options))
      "C_return(simdutf_binary_to_base64_with_lines(in, n, out, line_length, (simdutf_base64_options) options));"))

  (define maximal-binary-length-from-base64 (foreign-lambda size_t "simdutf_maximal_binary_length_from_base64" u8* size_t))
  (define base64->binary
    (foreign-lambda* int ((u8* in) (size_t n) (u8* out) (int options) (int last_chunk) (count* count))
      "simdutf_result r = simdutf_base64_to_binary(in, n, out, (simdutf_base64_options) options,
                                                   (simdutf_last_chunk_handling_options) last_chunk);
       *count = r.count;
       C_return(r.error);"))

  ; The base64 text of the bytes (or of a string's UTF-8), in the alphabet `options` names:
  ; default, url, default-no-padding or url-with-padding (url omits the padding, as simdutf does).
  ; A line-length breaks the text with a newline after every that many characters (MIME's 76).
  (define (simdutf-binary->base64 x #!optional (options 'default) line-length)
    (let* ((who 'simdutf-binary->base64)
           (bv (bytes who x))
           (n (bytevector-length bv))
           (o (option who base64-options options))
           (out (make-bytevector (if line-length
                                     (base64-length-from-binary/lines n o line-length)
                                     (base64-length-from-binary n o)))))
      (if line-length
          (binary->base64/lines bv n out line-length o)
          (binary->base64 bv n out o))
      (utf8->string out)))

  ; The bytes base64 text (a string, or its bytes) stands for, raising a simdutf condition at the
  ; first offending character.  `options` adds the *-accept-garbage and default-or-url variants to
  ; the alphabets above, which skip what is not base64 or accept either alphabet; `last-chunk`, one
  ; of loose, strict, stop-before-partial or only-full-chunks, is the handling of a final partial
  ; chunk, whose details are simdutf's (and the TC39 proposal's, `Uint8Array.fromBase64`).
  ; ASCII whitespace is skipped in every case.
  (define (simdutf-base64->binary x #!optional (options 'default) (last-chunk 'loose))
    (let* ((who 'simdutf-base64->binary)
           (bv (bytes who x))
           (n (bytevector-length bv))
           (out (make-bytevector (maximal-binary-length-from-base64 bv n))))
      (receive (code count) (call/result base64->binary bv n out
                                         (option who base64-options options)
                                         (option who base64-last-chunk-options last-chunk))
        (check who code count)
        (prefix out count))))

  )
