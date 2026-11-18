; Tests of (aux simdutf), the binding of simdutf: validation, transcoding between UTF-8, UTF-16,
; UTF-32 and Latin-1, encoding detection, and base64, checked against small reference encoders
; written here in plain Scheme.

(import (aux unittest) (aux base) (aux simdutf)
        (chicken random) (chicken bytevector) (chicken condition) (chicken platform) (chicken bitwise) (chicken port)
        (only srfi-1 list-tabulate append-map))

; the code, position and message of the simdutf condition raised by thunk, or 'not-raised
(define (simdutf-failure thunk)
  (condition-case (begin (thunk) 'not-raised)
    (e (simdutf) (list (simdutf-error-code e) (simdutf-error-position e)))))

(define (error-message thunk)
  (condition-case (begin (thunk) 'not-raised)
    (e (exn) ((condition-property-accessor 'exn 'message) e))))

(define (validate f x) (call-with-values (λ () (f x)) list))

; reference encoders, a code point at a time
(define (u16-units cp)
  (if (< cp #x10000)
      (list cp)
      (let1 (v (- cp #x10000))
        (list (+ #xD800 (arithmetic-shift v -10)) (+ #xDC00 (bitwise-and v #x3FF))))))

(define (le16 u) (list (bitwise-and u 255) (arithmetic-shift u -8)))
(define (be16 u) (reverse (le16 u)))
(define (le32 u) (list-tabulate 4 (λ (i) (bitwise-and (arithmetic-shift u (* -8 i)) 255))))
(define (ne32 u) (if (eq? (machine-byte-order) 'little-endian) (le32 u) (reverse (le32 u))))

(define (codepoints s) (map char->integer (string->list s)))
(define (ref-utf16le s) (apply bytevector (append-map le16 (append-map u16-units (codepoints s)))))
(define (ref-utf16be s) (apply bytevector (append-map be16 (append-map u16-units (codepoints s)))))
(define (ref-utf32 s) (apply bytevector (append-map ne32 (codepoints s))))

; A random scalar value, from every UTF-8 length class, never a surrogate.  Nor one whose low 16
; bits are DC80..DCFF: CHICKEN reads a byte b of invalid UTF-8 as the character #xDC00 + b, and a
; string made of characters (string, list->string) encodes every such one, in any plane, U+1DC80
; and the CJK Extension F U+2DC80 too, back to the byte b alone, which is no UTF-8.
(define (random-codepoint)
  (let1 (c (case (pseudo-random-integer 4)
             ((0) (pseudo-random-integer #x80))
             ((1) (+ #x80 (pseudo-random-integer (- #x800 #x80))))
             ((2) (+ #x800 (pseudo-random-integer (- #x10000 #x800))))
             (else (+ #x10000 (pseudo-random-integer (- #x110000 #x10000))))))
    (if (or (<= #xD800 c #xDFFF) (<= #xDC80 (bitwise-and c #xFFFF) #xDCFF)) #xFFFD c)))

(define (random-string n) (list->string (list-tabulate n (λ (_) (integer->char (random-codepoint))))))

(define (random-bytes n) (apply bytevector (list-tabulate n (λ (_) (pseudo-random-integer 256)))))

(define sample "héllo, 世界 🙂 ∀x. x ⊦ x")

(define-suite simdutf-suite

  ((test/version _)
   (let ((v (simdutf-get-version)) (impl (simdutf-get-implementation)))
     (⊦= "9.4.0" v)
     (⊨ (string? impl))
     `(doc (p "The binding embeds " (code/inline "simdutf") " " (code/inline ,v)
              (cite/a "https://github.com/simdutf/simdutf" (code/inline "simdutf") " : Unicode at gigabytes per second")
              ", whose kernel for this CPU is " (code/inline ,impl) "."))))

  ((test/validation _)
   (⊨ (simdutf-valid-utf8? sample))
   (⊨ (simdutf-valid-utf8? (string->utf8 sample)))
   (⊨ (simdutf-valid-utf8? ""))
   (⊨ (simdutf-valid-ascii? "plain text"))
   (⊭ (simdutf-valid-ascii? sample))
   (⊨ (simdutf-valid-utf16le? (ref-utf16le sample)))
   (⊨ (simdutf-valid-utf16be? (ref-utf16be sample)))
   (⊨ (simdutf-valid-utf32? (ref-utf32 sample)))
   ; a dangling byte is no valid UTF-16 or UTF-32
   (⊭ (simdutf-valid-utf16le? (bytevector 65 0 66)))
   (⊭ (simdutf-valid-utf32? (bytevector 65 0 0)))
   (⊭ (simdutf-valid-utf16le? "AB"))
   ; CHICKEN reads a byte of invalid UTF-8 as #xDC00 + byte, which a string encodes back to the byte
   (⊦= (bytevector 65 #xFF 66) (string->utf8 (string #\A (integer->char #xDCFF) #\B)))
   (⊭ (simdutf-valid-utf8? (string #\A (integer->char #xDCFF) #\B)))
   (⊦= '(success 14) (validate simdutf-validate-utf8 "héllo, 世界"))          ; bytes, not characters
   (⊦= '(success 3) (validate simdutf-validate-utf32 (ref-utf32 "a🙂b")))
   (⊦= '(success 4) (validate simdutf-validate-utf16le (ref-utf16le "a🙂b"))))

  ((test/validation-errors _)
   ; every UTF-8 error, at the code unit where it starts
   (⊦= '(header-bits 1) (validate simdutf-validate-utf8 (bytevector 65 #xFF 66)))
   (⊦= '(too-short 2) (validate simdutf-validate-utf8 (bytevector 65 66 #xC3)))
   (⊦= '(too-long 0) (validate simdutf-validate-utf8 (bytevector #x80 65)))
   (⊦= '(overlong 1) (validate simdutf-validate-utf8 (bytevector 65 #xC0 #x80)))
   (⊦= '(too-large 0) (validate simdutf-validate-utf8 (bytevector #xF4 #x90 #x80 #x80)))
   (⊦= '(surrogate 0) (validate simdutf-validate-utf8 (bytevector #xED #xA0 #x80)))
   (⊦= '(too-large 3) (validate simdutf-validate-ascii (bytevector 65 66 67 #xC3 #xA9)))
   ; a lone low surrogate, a high one at the end
   (⊦= '(surrogate 1) (validate simdutf-validate-utf16le (bytevector 65 0 0 #xDC 66 0)))
   (⊦= '(surrogate 1) (validate simdutf-validate-utf16be (bytevector 0 65 #xD8 0)))
   (⊦= '(too-large 1) (validate simdutf-validate-utf32 (apply bytevector (append (ne32 65) (ne32 #x110000)))))
   (⊦= '(surrogate 0) (validate simdutf-validate-utf32 (apply bytevector (ne32 #xD800))))
   (⊦= "the length in bytes is not a multiple of 2"
       (error-message (λ () (simdutf-validate-utf16le (bytevector 1 2 3))))))

  ((test/transcoding _)
   (⊦= (ref-utf16le sample) (simdutf-utf8->utf16le sample))
   (⊦= (ref-utf16be sample) (simdutf-utf8->utf16be sample))
   (⊦= (ref-utf32 sample) (simdutf-utf8->utf32 sample))
   (⊦= (string->utf8 sample) (simdutf-utf16le->utf8 (ref-utf16le sample)))
   (⊦= (string->utf8 sample) (simdutf-utf16be->utf8 (ref-utf16be sample)))
   (⊦= (string->utf8 sample) (simdutf-utf32->utf8 (ref-utf32 sample)))
   (⊦= (ref-utf32 sample) (simdutf-utf16le->utf32 (ref-utf16le sample)))
   (⊦= (ref-utf32 sample) (simdutf-utf16be->utf32 (ref-utf16be sample)))
   (⊦= (ref-utf16le sample) (simdutf-utf32->utf16le (ref-utf32 sample)))
   (⊦= (ref-utf16be sample) (simdutf-utf32->utf16be (ref-utf32 sample)))
   (⊦= (bytevector) (simdutf-utf8->utf16le ""))
   (⊦= (bytevector) (simdutf-utf16le->utf8 (bytevector)))
   `(doc (p "A string stands for its UTF-8 bytes, and every transcoder answers a fresh bytevector: "
            (code/inline "(simdutf-utf8->utf16le \"a🙂\")") " is "
            (code/inline ,(with-output-to-string (λ () (write (simdutf-utf8->utf16le "a🙂"))))) ".")))

  ((test/latin1 _)
   (let ((latin1 (bytevector 104 #xE9 108 108 #xF6 #xFF)))         ; "héllöÿ"
     (⊦= latin1 (simdutf-utf8->latin1 "héllöÿ"))
     (⊦= (string->utf8 "héllöÿ") (simdutf-latin1->utf8 latin1))
     (⊦= (ref-utf16le "héllöÿ") (simdutf-latin1->utf16le latin1))
     (⊦= (ref-utf16be "héllöÿ") (simdutf-latin1->utf16be latin1))
     (⊦= (ref-utf32 "héllöÿ") (simdutf-latin1->utf32 latin1))
     (⊦= latin1 (simdutf-utf16le->latin1 (ref-utf16le "héllöÿ")))
     (⊦= latin1 (simdutf-utf16be->latin1 (ref-utf16be "héllöÿ")))
     (⊦= latin1 (simdutf-utf32->latin1 (ref-utf32 "héllöÿ"))))
   ; a code point beyond U+FF has no Latin-1 byte
   (⊦= '(too-large 1) (simdutf-failure (λ () (simdutf-utf8->latin1 "a世"))))
   (⊦= '(too-large 2) (simdutf-failure (λ () (simdutf-utf16le->latin1 (ref-utf16le "ab€")))))
   (⊦= '(too-large 0) (simdutf-failure (λ () (simdutf-utf32->latin1 (ref-utf32 "🙂"))))))

  ((test/transcoding-errors _)
   (⊦= '(header-bits 1) (simdutf-failure (λ () (simdutf-utf8->utf16le (bytevector 65 #xFF 66)))))
   (⊦= '(too-short 1) (simdutf-failure (λ () (simdutf-utf8->utf32 (bytevector 65 #xE4 #xB8)))))
   (⊦= '(surrogate 1) (simdutf-failure (λ () (simdutf-utf16le->utf8 (bytevector 65 0 0 #xDC)))))
   (⊦= '(surrogate 0) (simdutf-failure (λ () (simdutf-utf16be->utf32 (bytevector #xD8 0)))))
   (⊦= '(too-large 0) (simdutf-failure (λ () (simdutf-utf32->utf16le (apply bytevector (ne32 #x110000))))))
   (⊦= "invalid input, header-bits at position 1"
       (error-message (λ () (simdutf-utf8->utf16le (bytevector 65 #xFF 66)))))
   (⊦= "neither a bytevector nor a string" (error-message (λ () (simdutf-utf8->utf16le 'symbol))))
   (⊦= "not a bytevector" (error-message (λ () (simdutf-utf16le->utf8 "a string")))))

  ((test/round-trips _)
   (set-pseudo-random-seed! (string->utf8 "aux-simdutf"))
   (do ((i 0 (add1 i))) ((= i 200))
     (let* ((s (random-string (pseudo-random-integer 200))) (u8 (string->utf8 s)))
       (⊦= (ref-utf16le s) (simdutf-utf8->utf16le s))
       (⊦= (ref-utf32 s) (simdutf-utf8->utf32 s))
       (⊦= (string-length s) (simdutf-count-utf8 s))
       (⊦= (string-length s) (simdutf-count-utf16le (ref-utf16le s)))
       (⊦= (string-length s) (simdutf-count-utf16be (ref-utf16be s)))
       (⊦= u8 (simdutf-utf16be->utf8 (simdutf-utf8->utf16be s)))
       (⊦= u8 (simdutf-utf32->utf8 (simdutf-utf16le->utf32 (simdutf-utf32->utf16le (simdutf-utf8->utf32 s)))))))
   ; long enough for every SIMD kernel, with the error far from the start
   (let* ((s (random-string 100000)) (u8 (string->utf8 s)) (n (bytevector-length u8)))
     (⊦= u8 (simdutf-utf16le->utf8 (simdutf-utf8->utf16le u8)))
     (⊦= (string-length s) (simdutf-count-utf8 u8))
     (let1 (broken (bytevector-append u8 (bytevector #xFF) u8))
       (⊦= `(header-bits ,n) (validate simdutf-validate-utf8 broken)))))

  ((test/replacement _)
   ; each maximal ill-formed subsequence of UTF-8 becomes one U+FFFD
   (⊦= (ref-utf16le "A\xFFFD;B") (simdutf-utf8->utf16le/replacement (bytevector 65 #xFF 66)))
   (⊦= (ref-utf16be "A\xFFFD;B") (simdutf-utf8->utf16be/replacement (bytevector 65 #xE4 #xB8 66)))
   (⊦= (ref-utf16le "\xFFFD;\xFFFD;") (simdutf-utf8->utf16le/replacement (bytevector #xC0 #x80)))
   (⊦= (ref-utf16le sample) (simdutf-utf8->utf16le/replacement sample))
   ; each unpaired surrogate of UTF-16 too
   (⊦= "A\xFFFD;B" (utf8->string (simdutf-utf16le->utf8/replacement (bytevector 65 0 0 #xD8 66 0))))
   (⊦= "A\xFFFD;" (utf8->string (simdutf-utf16be->utf8/replacement (bytevector 0 65 #xDC 0))))
   (⊦= (string->utf8 sample) (simdutf-utf16le->utf8/replacement (ref-utf16le sample)))
   (⊦= (ref-utf16le "A\xFFFD;B") (simdutf-to-well-formed-utf16le (bytevector 65 0 0 #xD8 66 0)))
   (⊦= (ref-utf16be "A\xFFFD;B") (simdutf-to-well-formed-utf16be (bytevector 0 65 #xDC 0 0 66)))
   (⊦= (ref-utf16le sample) (simdutf-to-well-formed-utf16le (ref-utf16le sample))))

  ((test/detection _)
   (⊦= 'utf8 (simdutf-autodetect-encoding sample))
   (⊦= 'utf16le (simdutf-autodetect-encoding (bytevector #xFF #xFE 65 0)))
   (⊦= 'utf16be (simdutf-autodetect-encoding (bytevector #xFE #xFF 0 65)))
   (⊦= 'utf8 (simdutf-autodetect-encoding (bytevector #xEF #xBB #xBF 65)))
   (⊦= #f (simdutf-autodetect-encoding (bytevector 65 #xFF 66)))
   ; four ASCII bytes are valid UTF-8 and UTF-16LE, not UTF-32
   (⊦= '(utf8 utf16le) (simdutf-detect-encodings "abcd"))
   (⊦= '(utf8 utf16le utf32le) (simdutf-detect-encodings (bytevector 65 0 0 0))))

  ((test/base64 _)
   ; RFC 4648, section 10
   (for-each (λ (plain encoded)
               (⊦= encoded (simdutf-binary->base64 plain))
               (⊦= (string->utf8 plain) (simdutf-base64->binary encoded)))
             '("" "f" "fo" "foo" "foob" "fooba" "foobar")
             '("" "Zg==" "Zm8=" "Zm9v" "Zm9vYg==" "Zm9vYmE=" "Zm9vYmFy"))
   (⊦= "-_8" (simdutf-binary->base64 (bytevector 251 255) 'url))
   (⊦= "+/8=" (simdutf-binary->base64 (bytevector 251 255)))
   (⊦= "+/8" (simdutf-binary->base64 (bytevector 251 255) 'default-no-padding))
   (⊦= "-_8=" (simdutf-binary->base64 (bytevector 251 255) 'url-with-padding))
   (⊦= "AAAA\nAAAA\nAA==" (simdutf-binary->base64 (make-bytevector 7 0) 'default 4))
   (⊦= (bytevector 251 255) (simdutf-base64->binary "-_8" 'url))
   (⊦= (bytevector 251 255) (simdutf-base64->binary "-_8" 'default-or-url))
   ; whitespace is skipped, garbage only when asked
   (⊦= (string->utf8 "foobar") (simdutf-base64->binary " Zm9v\nYmFy\r\n"))
   (⊦= '(invalid-base64-character 3) (simdutf-failure (λ () (simdutf-base64->binary "Zm9*vYmFy"))))
   (⊦= (string->utf8 "foobar") (simdutf-base64->binary "Zm9*vYmFy" 'default-accept-garbage))
   (⊦= '(invalid-base64-character 0) (simdutf-failure (λ () (simdutf-base64->binary "-_8"))))
   ; the final chunk
   (⊦= (string->utf8 "fo") (simdutf-base64->binary "Zm8"))
   (⊦= '(base64-input-remainder 3) (simdutf-failure (λ () (simdutf-base64->binary "Zm8" 'default 'strict))))
   (⊦= (string->utf8 "foo") (simdutf-base64->binary "Zm9vYm" 'default 'stop-before-partial))
   (⊦= '(base64-input-remainder 1) (simdutf-failure (λ () (simdutf-base64->binary "Z"))))
   (⊦= "unknown option" (error-message (λ () (simdutf-binary->base64 "x" 'hex))))
   (set-pseudo-random-seed! (string->utf8 "aux-simdutf-base64"))
   (do ((i 0 (add1 i))) ((= i 200))
     (let ((bv (random-bytes (pseudo-random-integer 300))))
       (⊦= bv (simdutf-base64->binary (simdutf-binary->base64 bv)))
       (⊦= bv (simdutf-base64->binary (simdutf-binary->base64 bv 'url) 'url))
       (⊦= bv (simdutf-base64->binary (simdutf-binary->base64 bv 'default 76))))))

  )

(unittest/✓ simdutf-suite)
