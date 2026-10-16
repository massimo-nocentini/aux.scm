;; (aux hansei random) -- the random number generator of (aux hansei), a CHICKEN port of Oleg
;; Kiselyov and Chung-chieh Shan's HANSEI (https://okmij.org/ftp/kakuritu/Hansei.html).
;;
;; Mirrors OCaml's standard library, which HANSEI's `random_selector` draws from (P:221-233):
;;   OR  stdlib/random.ml of OCaml 4.14.2 (https://github.com/ocaml/ocaml/blob/4.14/stdlib/
;;       random.ml; not in upstream/: OR line numbers refer to that file, 330 lines, sha256
;;       46683060...8e67; (c) 1996 INRIA, LGPL-2.1 with the OCaml linking exception): `init`,
;;       `full_init`, `bits`, `int`, `float`, the default state; the lagged Fibonacci generator
;;       F(55, 24, +) of 30-bit words with the xor `bits`.  `rawfloat` is the three-draw one of
;;       the OCaml releases HANSEI's records were made with (90 bits), not OCaml 4.14's two-draw
;;       one (OR:167-172, 60 bits): only the former replays them (plan D6, D20);
;;   OD  stdlib/digest.ml of OCaml 4.14.2: `Digest.string`, MD5 (R. Rivest, RFC 1321,
;;       https://www.rfc-editor.org/rfc/rfc1321), which `full_init` uses to seed the state.
;; It is the generator that replays HANSEI's recorded samples bit for bit (samplingM.ml, nested.ml,
;; slazy.ml, parsec.ml, music1a.ml; plan D6, D20).  Like OCaml's stdlib, the module holds one
;; global state, `ocaml-random-global`, which every `(probcc-random-selector seed)` of (aux
;; hansei) re-initialises (`Random.init seed`, P:221-222); and it exposes a state as an
;; rng-source of (aux hakaru10 random), so the selectors accept any other source too.
;;
;; HANSEI code version: April 2009 (H, "Implementation"); web page last updated March 1, 2016.
;; Papers: O. Kiselyov, C. Shan, "Embedded Probabilistic Programming", IFIP DSL 2009, LNCS 5658,
;; pp. 360-384 (DSL = dsl-paper.txt / .pdf), §"Importance sampling with look-ahead" (p.18-19).
;;
;; Contents (plan §5.4):
;;   - the (exn hansei) condition of kind `random`, raised on a bad seed or bound;
;;   - MD5: md5-digest, md5-hex;
;;   - the OCaml state: make-ocaml-random-state, ocaml-random-state?, ocaml-random-state-copy,
;;     ocaml-random-init!, ocaml-random-bits!, ocaml-random-float!, ocaml-random-int!;
;;   - the global state ocaml-random-global, OCaml's default state, mutated in place as
;;     upstream's;
;;   - the rng-source adapter make-ocaml-random-source.
;; Every word of the generator is below 2^31, every word of MD5 below 2^35 and every
;; shift of one in rotl32 below 2^55: fixnums on a 64-bit CHICKEN, where the arithmetic is the
;; `fx` one; on other word sizes it is the generic one, as in (aux hakaru10 random).

(module (aux hansei random) (md5-digest md5-hex make-ocaml-random-state ocaml-random-state?
                             ocaml-random-state-copy ocaml-random-init! ocaml-random-bits!
                             ocaml-random-float! ocaml-random-int! ocaml-random-global
                             make-ocaml-random-source)

  (import scheme
          (only (scheme base) exact-integer? string->utf8 bytevector-append vector-copy vector-map)
          (chicken base)
          (chicken fixnum)
          (chicken flonum)
          (chicken bitwise)
          (only (chicken format) fprintf)
          (only (chicken condition) make-property-condition make-composite-condition abort)
          (only (chicken bytevector) bytevector? bytevector-length bytevector-u8-ref
                                     make-bytevector bytevector-u8-set! bytevector-copy!)
          (only (aux hakaru10 random) make-rng-source)
          (aux base))

  ; No definition of this module is ever redefined, so the compiler may open-code the record
  ; accessors and inline the word operations into the loops.
  (declare (local))

  ;; ---------------------------------------------------------------------------------------
  ;; Errors (plan §7): composite (exn hansei) conditions of kind `random`, for OCaml's
  ;; `invalid_arg "Random.int"` (OR:91-94) and for a seed that is no integer (a type error
  ;; in OCaml).
  ;; ---------------------------------------------------------------------------------------

  ; (new) deviation 31 (OR:91-94): a condition instead of OCaml's `Invalid_argument`, raised with
  ; `abort`, which is not continuable like OCaml's `raise` (as pmap-signal, deviation 30).
  (define (ocaml-random-signal loc msg . args)
    (abort (make-composite-condition
             (make-property-condition 'exn 'location loc 'message msg 'arguments args)
             (make-property-condition 'hansei 'kind 'random))))

  ;; ---------------------------------------------------------------------------------------
  ;; Word arithmetic: `fx` on 64-bit CHICKEN, generic elsewhere (see the header).
  ;; ---------------------------------------------------------------------------------------

  ; (new) defines each `name` as the syntax (name a b) => (op a b), as (aux hakaru10 random).
  (define-syntax-rule (define-binary-syntax (name op) ...)
    (begin (define-syntax-rule (name a b) (op a b)) ...))

  ; (new) the operators of MD5 and of the generator below.
  (cond-expand
    (64bit (define-binary-syntax (w+ fx+) (w- fx-) (wand fxand) (wior fxior) (wxor fxxor)
                                 (wshl fxshl) (wshr fxshr) (wmod fxmod) (w= fx=) (w> fx>)))
    (else (define-binary-syntax (w+ +) (w- -) (wand bitwise-and) (wior bitwise-ior)
                                (wxor bitwise-xor) (wshl arithmetic-shift)
                                (wshr (λ (a b) (arithmetic-shift a (- b)))) (wmod modulo)
                                (w= =) (w> >))))

  ;; ---------------------------------------------------------------------------------------
  ;; MD5 (RFC 1321 §3; OD's `Digest.string` is `caml_md5_string`, the same algorithm).
  ;; ---------------------------------------------------------------------------------------

  ; cf. RFC 1321 §3.4: T[i] = floor(2^32 |sin(i + 1)|), the table of the 64 steps, as literals.
  (define md5-K
    '#(#xd76aa478 #xe8c7b756 #x242070db #xc1bdceee #xf57c0faf #x4787c62a #xa8304613 #xfd469501
       #x698098d8 #x8b44f7af #xffff5bb1 #x895cd7be #x6b901122 #xfd987193 #xa679438e #x49b40821
       #xf61e2562 #xc040b340 #x265e5a51 #xe9b6c7aa #xd62f105d #x02441453 #xd8a1e681 #xe7d3fbc8
       #x21e1cde6 #xc33707d6 #xf4d50d87 #x455a14ed #xa9e3e905 #xfcefa3f8 #x676f02d9 #x8d2a4c8a
       #xfffa3942 #x8771f681 #x6d9d6122 #xfde5380c #xa4beea44 #x4bdecfa9 #xf6bb4b60 #xbebfbc70
       #x289b7ec6 #xeaa127fa #xd4ef3085 #x04881d05 #xd9d4d039 #xe6db99e5 #x1fa27cf8 #xc4ac5665
       #xf4292244 #x432aff97 #xab9423a7 #xfc93a039 #x655b59c3 #x8f0ccc92 #xffeff47d #x85845dd1
       #x6fa87e4f #xfe2ce6e0 #xa3014314 #x4e0811a1 #xf7537e82 #xbd3af235 #x2ad7d2bb #xeb86d391))

  ; cf. RFC 1321 §3.4: the rotation amounts of the four rounds, four per round, in step order.
  (define md5-S '#(7 12 17 22 5 9 14 20 4 11 16 23 6 10 15 21))

  ; (new) the 32-bit truncation of x.
  (define-syntax-rule (w32 x) (wand x #xFFFFFFFF))

  ; cf. RFC 1321 §3.4: x <<< s on a 32-bit word x.
  (define-syntax-rule (rotl32 x s)
    (let ((x* x) (s* s))
      (wior (w32 (wshl x* s*)) (wshr x* (w- 32 s*)))))

  ; (new) the 32-bit little-endian word at byte i of the bytevector bv.
  (define (le32 bv i)
    (wior (wior (bytevector-u8-ref bv i)
                (wshl (bytevector-u8-ref bv (fx+ i 1)) 8))
          (wior (wshl (bytevector-u8-ref bv (fx+ i 2)) 16)
                (wshl (bytevector-u8-ref bv (fx+ i 3)) 24))))

  ; (new) stores the 32-bit word w little-endian at byte i of bv.
  (define (le32-set! bv i w)
    (let loop ((j 0) (w w))
      (unless (fx= j 4)
        (bytevector-u8-set! bv (fx+ i j) (wand w 255))
        (loop (fx+ j 1) (wshr w 8)))))

  ; cf. RFC 1321 §3.1-3.2: the message bv padded with the byte #x80, then zeros up to 56
  ; modulo 64 bytes, then its length in bits as a 64-bit little-endian integer.
  (define (md5-pad bv)
    (let* ((n (bytevector-length bv))
           (padded (make-bytevector (fx* 64 (fx+ (fx/ (fx+ n 8) 64) 1)) 0))
           (len (bytevector-length padded))
           (bits (* 8 n)))
      (bytevector-copy! padded 0 bv)
      (bytevector-u8-set! padded n #x80)
      (let loop ((j 0))
        (unless (fx= j 8)
          (bytevector-u8-set! padded (fx+ (fx- len 8) j)
                              (bitwise-and (arithmetic-shift bits (* -8 j)) 255))
          (loop (fx+ j 1))))
      padded))

  ; cf. RFC 1321 §3.4: the 64 steps over the 16-word block at byte `at` of bv, from the
  ; buffer (a b c d); answers the buffer added to the step results, as (values a b c d).  Step
  ; i of round r = i/16 uses F, G, H or I and the word g of the block: F(b,c,d) = bc | ~b d,
  ; G = bd | c ~d, H = b ^ c ^ d, I = c ^ (b | ~d); g = i, 5i+1, 3i+5, 7i mod 16.
  (define (md5-block bv at a0 b0 c0 d0)
    (let loop ((i 0) (a a0) (b b0) (c c0) (d d0))
      (cond
        ((fx= i 64) (values (w32 (w+ a0 a)) (w32 (w+ b0 b)) (w32 (w+ c0 c)) (w32 (w+ d0 d))))
        (else
         (let* ((r (fxshr i 4))
                (f (case r
                     ((0) (wior (wand b c) (wand (wxor b #xFFFFFFFF) d)))
                     ((1) (wior (wand b d) (wand c (wxor d #xFFFFFFFF))))
                     ((2) (wxor b (wxor c d)))
                     (else (wxor c (wior b (wxor d #xFFFFFFFF))))))
                (g (case r
                     ((0) i)
                     ((1) (fxand (fx+ (fx* 5 i) 1) 15))
                     ((2) (fxand (fx+ (fx* 3 i) 5) 15))
                     (else (fxand (fx* 7 i) 15))))
                (s (vector-ref md5-S (fx+ (fx* r 4) (fxand i 3))))
                (x (w32 (w+ (w+ a f) (w+ (vector-ref md5-K i) (le32 bv (fx+ at (fx* 4 g))))))))
           (loop (fx+ i 1) d (w32 (w+ b (rotl32 x s))) b c))))))

  ; cf. OD `Digest.string` and RFC 1321 §3: the 16-byte MD5 digest of a bytevector or of a
  ; string's UTF-8 bytes (an OCaml string is its bytes).  The buffer starts as RFC 1321 §3.3's
  ; A, B, C, D and is output low-order byte first (§3.5).
  (define (md5-digest msg)
    (let* ((bv (if (string? msg) (string->utf8 msg) msg))
           (padded (md5-pad bv))
           (len (bytevector-length padded)))
      (let loop ((at 0) (a #x67452301) (b #xefcdab89) (c #x98badcfe) (d #x10325476))
        (cond
          ((fx= at len) (let1 (out (make-bytevector 16 0))
                          (le32-set! out 0 a)
                          (le32-set! out 4 b)
                          (le32-set! out 8 c)
                          (le32-set! out 12 d)
                          out))
          (else (let-values (((a* b* c* d*) (md5-block padded at a b c d)))
                  (loop (fx+ at 64) a* b* c* d*)))))))

  ; cf. OD `Digest.to_hex`: md5-digest as 32 lowercase hexadecimal digits.
  (define (md5-hex msg)
    (let ((d (md5-digest msg))
          (digits "0123456789abcdef"))
      (let loop ((i 15) (acc '()))
        (if (fx< i 0)
            (list->string acc)
            (let1 (byte (bytevector-u8-ref d i))
              (loop (fx- i 1) (cons (string-ref digits (fxshr byte 4))
                                    (cons (string-ref digits (fxand byte 15)) acc))))))))

  ;; ---------------------------------------------------------------------------------------
  ;; The OCaml state (OR:30-175).
  ;; ---------------------------------------------------------------------------------------

  ; cf. OR:32 `type t = { st : int array; mutable idx : int }`: 55 words below 2^30 and the
  ; index of the last one drawn.
  (define-record-type ocaml-random-state
    (ocaml-random-state/raw st idx)
    ocaml-random-state?
    (st ocaml-random-state-st)
    (idx ocaml-random-state-idx ocaml-random-state-idx-set!))

  ; (debug printer only) #<ocaml-random-state idx st[0] ...>, the index and the first word.
  (set-record-printer! ocaml-random-state
    (λ (s port)
      (fprintf port "#<ocaml-random-state ~a ~a ...>" (ocaml-random-state-idx s)
               (vector-ref (ocaml-random-state-st s) 0))))

  ; cf. OR:69-72 `copy`: a state that replays s.
  (define (ocaml-random-state-copy s)
    (ocaml-random-state/raw (vector-copy (ocaml-random-state-st s)) (ocaml-random-state-idx s)))

  ; cf. OR:40-58 `full_init`, transcribed loop by loop: st[i] = i; then, for i from 0 to 54 +
  ; max 55 l, accu := MD5(accu ^ decimal seed[i mod l]) starting from "x", and st[i mod 55] :=
  ; (st[i mod 55] xor the little-endian first word of accu) land #x3FFFFFFF; idx = 0.  `seed`
  ; is an exact integer, which is `init` (OR:228, `full_init [| seed |]`), or a vector of exact
  ; integers, which is `full_init`; the empty vector is [| 0 |] (OR:46).  It answers s.
  (define (ocaml-random-init! s seed)
    (let* ((seeds (cond
                    ((exact-integer? seed) (vector seed))
                    ((and (vector? seed) (fx= (vector-length seed) 0)) (vector 0))
                    ((and (vector? seed) (every-exact-integer? seed)) seed)
                    (else (ocaml-random-signal 'ocaml-random-init!
                                               "seed must be an exact integer or a vector of them"
                                               seed))))
           (decimals (vector-map (λ (x) (string->utf8 (number->string x))) seeds))  ; Int.to_string
           (l (vector-length seeds))
           (st (ocaml-random-state-st s)))
      (let loop ((i 0))
        (unless (fx= i 55)
          (vector-set! st i i)
          (loop (fx+ i 1))))
      (let loop ((i 0) (accu (string->utf8 "x")))
        (unless (fx> i (fx+ 54 (fxmax 55 l)))
          (let* ((j (fxmod i 55))
                 (accu* (md5-digest (bytevector-append accu (vector-ref decimals (fxmod i l))))))
            (vector-set! st j (wand (wxor (vector-ref st j) (le32 accu* 0)) #x3FFFFFFF))
            (loop (fx+ i 1) accu*))))
      (ocaml-random-state-idx-set! s 0)
      s))

  ; (new) whether every element of the vector v is an exact integer.
  (define (every-exact-integer? v)
    (let loop ((i 0))
      (or (fx= i (vector-length v))
          (and (exact-integer? (vector-ref v i)) (loop (fx+ i 1))))))

  ; cf. OR:61-64 `make`: a fresh state seeded as `ocaml-random-init!` does.  Without a seed, it
  ; is OCaml's default state, the one a program that never seeds draws from (OR:197-213: "the
  ; state you get with [init 27182818]"; the `land 0x3FFFFFFF` filter it mentions is already
  ; applied by OR:56).
  (define (make-ocaml-random-state #!optional (seed 27182818))
    (ocaml-random-init! (ocaml-random-state/raw (make-vector 55 0) 0) seed))

  ; cf. OR:76-83 `bits`: 30 random bits, 0 <= x < 2^30.  idx := (idx + 1) mod 55; the new word
  ; is st[(idx + 24) mod 55] + (cur xor ((cur lsr 25) land #x1F)) land #x3FFFFFFF, stored at
  ; idx and answered.
  (define (ocaml-random-bits! s)
    (let* ((st (ocaml-random-state-st s))
           (idx (wmod (w+ (ocaml-random-state-idx s) 1) 55))
           (cur (vector-ref st idx))
           (new (wand (w+ (vector-ref st (wmod (w+ idx 24) 55))
                          (wxor cur (wand (wshr cur 25) #x1F)))
                      #x3FFFFFFF)))
      (ocaml-random-state-idx-set! s idx)
      (vector-set! st idx new)
      new))

  ; cf. OR:86-94 `intaux` and `int`: a uniform integer in [0, n), for 0 < n <= 2^30 - 1.  r :=
  ; bits; v := r mod n; drawn again while r - v > #x3FFFFFFF - n + 1, which rejects the last,
  ; incomplete period of n in [0, 2^30).
  (define (ocaml-random-int! s n)
    (unless (and (fixnum? n) (fx> n 0) (fx<= n #x3FFFFFFF))
      (ocaml-random-signal 'ocaml-random-int! "Random.int: bound must be in [1, 2^30 - 1]" n))
    (let loop ()
      (let* ((r (ocaml-random-bits! s))
             (v (wmod r n)))
        (if (w> (w- r v) (w+ (w- #x3FFFFFFF n) 1)) (loop) v))))

  ; cf. OR:167-175 `rawfloat` and `float`; deviation 32 (OR:167-172): the three-draw rawfloat
  ; of the OCaml that made HANSEI's records, ((r0 / 2^30 + r1) / 2^30 + r2) / 2^30 with r0, r1,
  ; r2 drawn in this order, times `bound`; OCaml 4.14 draws two words, (r1 / 2^30 + r2) / 2^30,
  ; and replays none of the records (plan D6, D20).  `bound` is any real.
  (define (ocaml-random-float! s bound)
    (let* ((scale 1073741824.)                                    ; 2^30
           (r0 (exact->inexact (ocaml-random-bits! s)))
           (r1 (exact->inexact (ocaml-random-bits! s)))
           (r2 (exact->inexact (ocaml-random-bits! s))))
      (fp* (fp/ (fp+ (fp/ (fp+ (fp/ r0 scale) r1) scale) r2) scale) (exact->inexact bound))))

  ; cf. OR:197-213 `default` and OR:215-234 (`Random.bits ()` and the other functions of the
  ; module draw from it): the global state, OCaml's default one until the first
  ; `ocaml-random-init!`.  Mutated in place, never rebound, and never restored on
  ; backtracking, like upstream's: every `(probcc-random-selector seed)` re-initialises it
  ; (`Random.init`, OR:228, P:222).  Kept as upstream (plan Q4).
  (define ocaml-random-global (make-ocaml-random-state))

  ;; ---------------------------------------------------------------------------------------
  ;; The rng-source adapter: an OCaml state as a source of (aux hakaru10 random), so that the
  ;; selectors of (aux hansei) draw from it with `rng-double!`.
  ;; ---------------------------------------------------------------------------------------

  ; (new, plan §5.4) a source over an OCaml state: `seed-or-state` is an exact integer or a
  ; vector of them, which makes a fresh state (ocaml-random-init!), or a state, which is
  ; shared, not copied, so that a source over ocaml-random-global draws from the global state,
  ; as OCaml's `Random.float` does.  Doubles are lo + rawfloat * (hi - lo), bit-identical to
  ; `Random.float hi` when lo is 0. (0. + x = x and hi - 0. = hi); integers are lo +
  ; `Random.int (hi - lo + 1)`, the bounds in either order, for ranges of at most 2^30 - 1
  ; integers; `rng-source-copy` copies the state.
  (define (make-ocaml-random-source seed-or-state)
    (make-rng-source 'ocaml
                     (if (ocaml-random-state? seed-or-state)
                         seed-or-state
                         (make-ocaml-random-state seed-or-state))
                     (λ (s lo hi) (fp+ lo (fp* (ocaml-random-float! s 1.) (fp- hi lo))))
                     (λ (s lo hi)
                       (unless (and (exact-integer? lo) (exact-integer? hi))
                         (ocaml-random-signal 'rng-integer! "bounds must be exact integers" lo hi))
                       (let1 (l (min lo hi))
                         (+ l (ocaml-random-int! s (add1 (- (max lo hi) l))))))
                     ocaml-random-state-copy))

  )
