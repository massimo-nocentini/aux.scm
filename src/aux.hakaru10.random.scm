;; (aux hakaru10 random) -- the random number generators of (aux hakaru10), a CHICKEN port of
;; Oleg Kiselyov's Hakaru10 (https://okmij.org/ftp/kakuritu/Hakaru10/index.html).
;;
;; Mirrors (paths relative to aux.scm/upstream/okmij-hakaru10/, citation keys as in the docs):
;;   R  random-1.0.1.1 System/Random.hs (StdGen: mkStdGen, stdNext, randomIvalInteger,
;;      randomRFloating), https://hackage-content.haskell.org/package/random-1.0.1.1/src/
;;      System/Random.hs
;;      (not in upstream/: R line numbers refer to that file, 567 lines, sha256 fd5c9241...c671;
;;      (c) The University of Glasgow 2001, BSD-style licence).
;; It is the generator GHC used for Hakaru10's recorded runs: Metropolis.hs seeds `mkStdGen 17`
;; (M:858) and threads the StdGen through the chain state (M:166, M:204-210), so porting it lets
;; the tests replay upstream's samples exactly.
;;
;; Hakaru10 code version: November 2016 (W:52-53); web page last updated July 14, 2018 (W:179).
;; Paper: O. Kiselyov, "Probabilistic Programming Language and its Incremental Evaluation",
;; APLAS 2016, LNCS 10017, DOI 10.1007/978-3-319-47958-3_19 (P = design.txt / design.pdf).
;;
;; Contents:
;;   - the StdGen layer (R): the stdgen record, seed->stdgen (mkStdGen), ilog-base (iLogBase)
;;     and, for each of stdNext, randomIvalInteger, `random :: Double` and randomRFloating, an
;;     in-place procedure (`!`, which the engine uses through a source) and its pure twin
;;     answering (values x g'), as Haskell does.  `stdgen-double!` is fixnum-only (acc kept
;;     mod 2^53); its bignum oracle `stdgen-double/reference` serves the tests;
;;   - the rng-source protocol (plan §4.1): make-stdgen-source, rng-double!, rng-integer!,
;;     rng-source-copy, rng-source-state;
;;   - two optional sources (plan WP12), not used by any replay: make-mrg32k3a-source
;;     (L'Ecuyer 1999) and make-chicken-random-source, an adapter over (chicken random);
;;   - the (exn hakaru10) error conditions (plan C16): hakaru10-error, hakaru10-error? and
;;     hakaru10-error-kind, raised by every module of (aux hakaru10) and re-exported by
;;     (aux hakaru10 distribution); this module raises only the `parameter` kind.

(module (aux hakaru10 random) *

  (import scheme
          (only (scheme base) exact-integer?)
          (chicken base)
          (chicken fixnum)
          (chicken flonum)
          (chicken bitwise)
          (only (chicken format) fprintf)
          (only (chicken condition) make-property-condition make-composite-condition signal
                                    condition-predicate condition-property-accessor)
          (only (chicken bytevector) bytevector? bytevector-length make-bytevector bytevector-copy!)
          (only (chicken random) set-pseudo-random-seed! pseudo-random-integer pseudo-random-real)
          (only srfi-1 every split-at append-map)
          srfi-4
          (aux base))

  ; No definition of this module is ever redefined, so the compiler may open-code the record
  ; accessors where the draws call them (about 20% faster draws at the egg's -O2).
  (declare (local))

  ;; ---------------------------------------------------------------------------------------
  ;; Errors (plan C16, §3.4): composite (exn hakaru10) conditions whose hakaru10 part carries a
  ;; `kind`, one of conditioning, construction, dependency, parameter, branch, support,
  ;; internal.  They are defined here, the lowest module that raises, and (aux hakaru10
  ;; distribution) re-exports them.
  ;; ---------------------------------------------------------------------------------------

  ; cf. plan C16 and §3.4 "Errors"; replaces Haskell's `error` (e.g. D:153).  It never returns.
  (define (hakaru10-error kind loc msg . args)
    (signal (make-composite-condition
              (make-property-condition 'exn 'location loc 'message msg 'arguments args)
              (make-property-condition 'hakaru10 'kind kind))))

  ; cf. plan §3.4: whether x is a condition raised by (aux hakaru10).
  (define hakaru10-error? (condition-predicate 'hakaru10))

  ; cf. plan C16: the kind of a (exn hakaru10) condition.
  (define hakaru10-error-kind (condition-property-accessor 'hakaru10 'kind))

  ; (no upstream counterpart) whether lo and hi are both exact integers, as the bounds of an
  ; Int draw must be; otherwise a `parameter` error located at `who`.
  (define (check-integer-bounds who lo hi)
    (unless (and (exact-integer? lo) (exact-integer? hi))
      (hakaru10-error 'parameter who "bounds must be exact integers" lo hi)))

  ;; ---------------------------------------------------------------------------------------
  ;; The StdGen of random-1.0.1.1 (R).
  ;;
  ;; Every intermediate of stdNext stays below 2^32 (40014 * 53667 < 2^31) and the mod-2^53
  ;; step of `stdgen-double!` below 2^59: fixnums on a 64-bit CHICKEN, where the stdgen
  ;; arithmetic below is the `fx` one; on other word sizes it is the generic one.
  ;; ---------------------------------------------------------------------------------------

  ; (no upstream counterpart) defines each `name` as the syntax (name a b) => (op a b).
  (define-syntax-rule (define-binary-syntax (name op) ...)
    (begin (define-syntax-rule (name a b) (op a b)) ...))

  ; cf. R:474-486 (stdNext, on Int32) and R:427 (the n = 1 case of randomIvalInteger): the
  ; operators of their transcriptions below, `fx` where the values above are fixnums.
  (cond-expand
    (64bit (define-binary-syntax (stdgen+ fx+) (stdgen- fx-) (stdgen* fx*) (stdgen-quot fx/)
                                 (stdgen-mod fxmod) (stdgen< fx<)))
    (else (define-binary-syntax (stdgen+ +) (stdgen- -) (stdgen* *) (stdgen-quot quotient)
                                (stdgen-mod modulo) (stdgen< <))))

  ; cf. R:191-192 `data StdGen = StdGen Int32 Int32`; the two fields hold fixnums in
  ; [1, 2147483562] and [1, 2147483398].  The `!` procedures below mutate a stdgen in place;
  ; their pure twins copy it first.
  (define-record stdgen s1 s2)

  ; cf. R:203-207 `instance Show StdGen` ("s1 s2"), shown as #<stdgen s1 s2>.
  (set-record-printer! stdgen
    (λ (g port) (fprintf port "#<stdgen ~a ~a>" (stdgen-s1 g) (stdgen-s2 g))))

  ; (no upstream counterpart: StdGen is an immutable Haskell value) a fresh copy of g.
  (define (stdgen-copy g) (make-stdgen (stdgen-s1 g) (stdgen-s2 g)))

  ; (no upstream counterpart) defines the pure (name arg ... g), which answers (values x g')
  ; and leaves g untouched, as a Haskell StdGen is, from its in-place twin (name! arg ... g).
  (define-syntax-rule (define-stdgen/pure (name arg ...) name!)
    (define (name arg ... g)
      (let1 (g* (stdgen-copy g))
        (let1 (x (name! arg ... g*))
          (values x g*)))))

  ; cf. R:430-431 `b = 2147483561`, which randomIvalInteger bakes in ("ERROR: b here (2^31-87)
  ; represents a baked-in assumption about genRange"; stdRange is (0, 2147483562), R:471-472).
  (define stdgen-b 2147483561)

  ; cf. R:235-245 `mkStdGen` and `mkStdGen32`: `fromIntegral` to Int32, then `.&. maxBound`,
  ; which on two's complement is the same as masking the integer itself with #x7fffffff.
  (define (seed->stdgen n)
    (unless (exact-integer? n)
      (hakaru10-error 'parameter 'seed->stdgen "seed must be an exact integer" n))
    (let* ((s (bitwise-and n #x7fffffff))
           (q (quotient s 2147483562))
           (s1 (remainder s 2147483562))
           (s2 (remainder q 2147483398)))
      (make-stdgen (+ s1 1) (+ s2 1))))

  ; cf. R:474-486 `stdNext`, transcribed line by line, with s1* for Haskell's s1' (`quot`
  ; truncates, as fx/ and quotient do): binds z to the output of a StdGen whose seeds are seed1
  ; and seed2, and next1 and next2 to its next seeds (Haskell's s1'' and s2''), in body.  A
  ; binding form, so that the draws below thread the seeds through local variables, as Haskell
  ; threads the StdGen, and store them back into the record once per draw.
  (define-syntax-rule (let-stdnext ((z next1 next2) (seed1 seed2)) body ...)
    (let* ((s1 seed1)
           (s2 seed2)
           (k (stdgen-quot s1 53668))
           (s1* (stdgen- (stdgen* 40014 (stdgen- s1 (stdgen* k 53668))) (stdgen* k 12211)))
           (next1 (if (stdgen< s1* 0) (stdgen+ s1* 2147483563) s1*))
           (k* (stdgen-quot s2 52774))
           (s2* (stdgen- (stdgen* 40692 (stdgen- s2 (stdgen* k* 52774))) (stdgen* k* 3791)))
           (next2 (if (stdgen< s2* 0) (stdgen+ s2* 2147483399) s2*))
           (z* (stdgen- next1 next2))
           (z (if (stdgen< z* 1) (stdgen+ z* 2147483562) z*)))
      body ...))

  ; cf. R:434-441, the local `f` of randomIvalInteger: folds the next n >= 0 outputs z of the
  ; stdgen g into acc, which starts as init and becomes step at each output, and leaves in g
  ; the seeds that follow them.  A binding form for z and acc, as `do` is for its variables.
  (define-syntax-rule (stdgen-fold! g n (z acc) step init)
    (let1 (gen g)
      (let loop ((i n) (acc init) (s1 (stdgen-s1 gen)) (s2 (stdgen-s2 gen)))
        (if (eq? i 0)
            (begin (stdgen-s1-set! gen s1) (stdgen-s2-set! gen s2) acc)
            (let-stdnext ((z next1 next2) (s1 s2))
              (loop (sub1 i) step next1 next2))))))

  ; cf. R:474-486 `stdNext`, in place: one output of g.
  (define-syntax-rule (stdnext! g) (stdgen-fold! g 1 (z _) z #f))

  ; cf. R:474-486 `stdNext`, in place, as a procedure.
  (define (stdgen-next! g) (stdnext! g))

  ; cf. R:194-196 `next = stdNext` (instance RandomGen StdGen): (values z g').
  (define-stdgen/pure (stdgen-next) stdgen-next!)

  ; cf. R:464-469 `iLogBase b i = if i < b then 1 else 1 + iLogBase b (i `div` b)`, the
  ; number of `next` calls randomIvalInteger makes for a range of size i (2 at i = b, as
  ; R:466-467 notes).
  (define (ilog-base b i)
    (if (< i b) 1 (add1 (ilog-base b (quotient i b)))))

  ; cf. R:424-441 `randomIvalInteger` (random-1.0.1.1, before random-1.1's `magtgt`): swap if l
  ; > h, then n = iLogBase b k nexts folded as acc := x + acc*b starting from acc = 1, and the
  ; result is l + acc `mod` k.  A range of size k < b needs one next only (n = 1) and acc = x +
  ; b < 2^32: that is the fixnum fast path; any other range goes through exact integers, as
  ; upstream's Integer arithmetic does.
  (define (stdgen-ival-integer! l h g)
    (check-integer-bounds 'stdgen-ival-integer l h)
    (if (> l h)
        (stdgen-ival-integer! h l g)
        (let1 (k (add1 (- h l)))
          (+ l (if (< k stdgen-b)
                   (stdgen-mod (stdgen+ (stdnext! g) stdgen-b) k)
                   (modulo (stdgen-fold! g (ilog-base stdgen-b k) (z acc) (+ z (* acc stdgen-b)) 1)
                           k))))))

  ; cf. R:305, R:420-422 `randomR (l,h) :: Int` = `randomIvalIntegral`: (values x g').
  (define-stdgen/pure (stdgen-ival-integer l h) stdgen-ival-integer!)

  ; cf. plan §4.1 "Mandatory fixnum mod-2^53 step": (x + acc*b) mod 2^53 for acc < 2^53, with
  ; acc = hi*2^27 + lo (lo < 2^27, hi < 2^26), so that acc*b mod 2^53 is
  ; (((hi*b) mod 2^26)*2^27 + lo*b) mod 2^53 and every intermediate is below 2^59.
  (cond-expand
    (64bit
      (define-inline (stdgen-acc53 x acc)
        (let* ((hi (fxshr acc 27))
               (lo (fxand acc #x7ffffff))
               (hib (fxshl (fxand (fx* hi stdgen-b) #x3ffffff) 27)))
          (fxand (fx+ x (fx+ hib (fx* lo stdgen-b))) #x1fffffffffffff))))
    (else
      ; the same step through generic arithmetic, where 2^59 is no fixnum.
      (define-inline (stdgen-acc53 x acc)
        (bitwise-and (+ x (* acc stdgen-b)) #x1fffffffffffff))))

  ; cf. R:368-378 `instance Random Double`, `random`: an Int64 from `randomBounded` (R:309,
  ; R:417-418), that is randomIvalInteger over (minBound, maxBound), so k = 2^64 and n =
  ; iLogBase b 2^64 = 3 nexts; then `mask53 .&. x` over 2^53.  Since the lower bound -2^63 and
  ; the modulus 2^64 are both multiples of 2^53, mask53 .&. (-2^63 + acc mod 2^64) is acc mod
  ; 2^53, which `stdgen-acc53` maintains at every step of the fold.  The value is
  ; (acc mod 2^53) / 2^53, both exact in a double.
  (define (stdgen-double! g)
    (fp/ (exact->inexact (stdgen-fold! g 3 (z acc) (stdgen-acc53 z acc) 1))
         9007199254740992.))

  ; cf. R:368-378 `random :: Double`: (values d g').
  (define-stdgen/pure (stdgen-double) stdgen-double!)

  ; cf. R:368-378 `instance Random Double` and R:417-418 `randomBounded` read literally, through
  ; exact integers: randomIvalInteger over (-2^63, 2^63 - 1) (bignums, three nexts), the Int64
  ; result masked with mask53 = 2^53 - 1 and divided by twoto53.  Tests only: it is the oracle
  ; of `stdgen-double`.
  (define (stdgen-double/reference g)
    (let*-values (((twoto53) (expt 2 53))
                  ((x g*) (stdgen-ival-integer (- (expt 2 63)) (sub1 (expt 2 63)) g)))
      (values (/ (exact->inexact (bitwise-and x (sub1 twoto53))) (exact->inexact twoto53)) g*)))

  ; cf. R:361-366 `randomRFloating`: swap if l > h, then
  ; 2.0 * (0.5*l + coef * (0.5*h - 0.5*l)), in this association (Haskell `infixl 6 + -`,
  ; `infixl 7 * /`), which "avoids overflow".  The interval is closed: coef is in
  ; [0, 1 - 2^-53], and the rounding may reach h.
  (define (stdgen-range-double! lo hi g)
    (if (fp> lo hi)
        (stdgen-range-double! hi lo g)
        (let1 (coef (stdgen-double! g))
          (fp* 2. (fp+ (fp* .5 lo) (fp* coef (fp- (fp* .5 hi) (fp* .5 lo))))))))

  ; cf. R:368-369 `randomR (l,h) :: Double` = randomRFloating: (values d g').
  (define-stdgen/pure (stdgen-range-double lo hi) stdgen-range-double!)

  ;; ---------------------------------------------------------------------------------------
  ;; The rng-source protocol (plan §4.1): a mutable random source, so that the engine draws
  ;; with `rng-double!` and `rng-integer!` instead of threading a pure generator.  Upstream
  ;; threads the StdGen through the chain state (M:166, `randomS` at M:204-210) and never rolls
  ;; it back on rejection (M:904-906); a source that only moves forward is the same thing.
  ;; ---------------------------------------------------------------------------------------

  ; cf. M:166 (`seed :: StdGen` of MCState) and M:204-210 (randomS).  `draw-double` is
  ; (λ (state lo hi) flonum) for flonum bounds, `draw-integer` is (λ (state lo hi) integer)
  ; for exact-integer bounds, and `copier` is (λ (state) state'), a state that replays this one.
  (define-record rng-source name state draw-double draw-integer copier)

  ; (debug printer only) #<rng-source name state>.
  (set-record-printer! rng-source
    (λ (src port)
      (fprintf port "#<rng-source ~a ~s>" (rng-source-name src) (rng-source-state src))))

  ; cf. R:368-369 `randomR (lo,hi) :: Double` as Hakaru10 calls it through randomS (M:204-210):
  ; every Double draw of D and M, for example uniform (D:107-117), bern (D:53-62) and the
  ; acceptance test (M:939-944).  The bounds may be any reals; they are converted to flonums.
  (define (rng-double! src lo hi)
    ((rng-source-draw-double src) (rng-source-state src) (exact->inexact lo) (exact->inexact hi)))

  ; cf. R:305 `randomR (lo,hi) :: Int`, inclusive, as Hakaru10 calls it in uniformly (D:82) and in
  ; resample_trace (M:962).
  (define (rng-integer! src lo hi)
    ((rng-source-draw-integer src) (rng-source-state src) lo hi))

  ; (no upstream counterpart: a StdGen value is copied by sharing it) a source with a copy of
  ; src's state, which replays the draws src makes from now on.
  (define (rng-source-copy src)
    (make-rng-source (rng-source-name src)
                     ((rng-source-copier src) (rng-source-state src))
                     (rng-source-draw-double src)
                     (rng-source-draw-integer src)
                     (rng-source-copier src)))

  ; cf. M:858 (`mkStdGen 17`): the default source of (aux hakaru10).  `seed` is an exact integer
  ; (default 17) or a stdgen, which is copied.  `rng-source-state` answers the live stdgen.
  (define (make-stdgen-source #!optional (seed 17))
    (make-rng-source 'stdgen
                     (if (stdgen? seed) (stdgen-copy seed) (seed->stdgen seed))
                     (λ (g lo hi) (stdgen-range-double! lo hi g))
                     (λ (g lo hi) (stdgen-ival-integer! lo hi g))
                     stdgen-copy))

  ;; ---------------------------------------------------------------------------------------
  ;; Optional sources (plan WP12).  Neither replays upstream; both are for users who want a
  ;; longer period or CHICKEN's own generator.  Both map a u in (0,1) onto a range by
  ;; `unit->range` and `unit->integer`.
  ;; ---------------------------------------------------------------------------------------

  ; (new, plan §4.1 WP12) lo + u*(hi-lo) for u in (0,1), the bounds in either order.
  (define (unit->range u lo hi)
    (if (fp> lo hi) (unit->range u hi lo) (fp+ lo (fp* u (fp- hi lo)))))

  ; (new, plan §4.1 WP12) lo + floor(u*(hi-lo+1)) for u in (0,1), at most hi (the product may
  ; round up to hi-lo+1), the exact-integer bounds in either order.
  (define (unit->integer u lo hi)
    (if (> lo hi)
        (unit->integer u hi lo)
        (min hi (+ lo (inexact->exact (fpfloor (fp* u (exact->inexact (add1 (- hi lo))))))))))

  ; cf. P. L'Ecuyer, "Good parameters and implementations for combined multiple recursive random
  ; number generators", Operations Research 47(1):159-164, 1999, Figure 1 (MRG32k3a, the
  ; floating-point implementation): moduli, multipliers and the normalization 1/(m1+1).
  (define mrg32k3a-m1 4294967087.)
  (define mrg32k3a-m2 4294944443.)
  (define mrg32k3a-norm 2.328306549295727688e-10)

  ; cf. L'Ecuyer 1999, Fig. 1 `MRG32k3a()`: one component, p := (a*x - b*y) mod m (x is s11
  ; and y s10 for the first, x is s22 and y s20 for the second), with `k = p / m` the C cast to
  ; long, so fptruncate; every product stays below 2^53, so the flonum arithmetic is exact.
  ; Answers p in [0, m).
  (define-inline (mrg32k3a-component a x b y m)
    (let* ((p (fp- (fp* a x) (fp* b y)))
           (p (fp- p (fp* (fptruncate (fp/ p m)) m))))
      (if (fp< p 0.) (fp+ p m) p)))

  ; cf. L'Ecuyer 1999, Fig. 1 `MRG32k3a()`, statement by statement.  s is an f64vector
  ; (s10 s11 s12 s20 s21 s22), shifted in place; the answer u is in (0,1).
  (define (mrg32k3a-next! s)
    (let ((p1 (mrg32k3a-component 1403580. (f64vector-ref s 1) 810728. (f64vector-ref s 0)
                                  mrg32k3a-m1))
          (p2 (mrg32k3a-component 527612. (f64vector-ref s 5) 1370589. (f64vector-ref s 3)
                                  mrg32k3a-m2)))
      (f64vector-set! s 0 (f64vector-ref s 1))
      (f64vector-set! s 1 (f64vector-ref s 2))
      (f64vector-set! s 2 p1)
      (f64vector-set! s 3 (f64vector-ref s 4))
      (f64vector-set! s 4 (f64vector-ref s 5))
      (f64vector-set! s 5 p2)
      (fp* (if (fp<= p1 p2) (fp+ (fp- p1 p2) mrg32k3a-m1) (fp- p1 p2)) mrg32k3a-norm)))

  ; cf. L'Ecuyer, Simard, Chen, Kelton, "An object-oriented random-number package with many long
  ; streams and substreams", Operations Research 50(6), 2002, `CheckSeed`: each component in
  ; [0, m), neither triple all zero.  An exact-integer seed is spread over the six words by the
  ; LCG x := (69069 x + 1) mod 2^32 (Marsaglia's constant), each reduced modulo its m; a triple
  ; that comes out all zero is replaced by 12345s, L'Ecuyer's default.  Six integers (a list, a
  ; vector or an f64vector) are taken as they are.
  (define (mrg32k3a-seed->state seed)
    (define moduli '(4294967087 4294967087 4294967087 4294944443 4294944443 4294944443))
    (define (all-zero? triple) (every zero? triple))
    (define (component? x m) (and (real? x) (integer? x) (<= 0 x) (< x m)))
    (define (spread seed)
      (let loop ((moduli moduli) (x (modulo seed 4294967296)))
        (if (null? moduli)
            '()
            (let1 (x* (modulo (add1 (* 69069 x)) 4294967296))
              (cons (modulo x* (car moduli)) (loop (cdr moduli) x*))))))
    (define (->state words) (list->f64vector (map exact->inexact words)))
    (define seed-words
      (cond ((list? seed) seed)
            ((vector? seed) (vector->list seed))
            ((f64vector? seed) (f64vector->list seed))
            (else #f)))
    (cond
      ((exact-integer? seed)
       (let-values (((t1 t2) (split-at (spread seed) 3)))
         (->state (append-map (λ (t) (if (all-zero? t) '(12345 12345 12345) t)) (list t1 t2)))))
      ((and seed-words (= (length seed-words) 6) (every component? seed-words moduli))
       (let-values (((t1 t2) (split-at seed-words 3)))
         (when (or (all-zero? t1) (all-zero? t2))
           (hakaru10-error 'parameter 'make-mrg32k3a-source "a seed triple is all zero" seed))
         (->state seed-words)))
      (else (hakaru10-error 'parameter 'make-mrg32k3a-source
                       "seed must be an exact integer or six integers in [0,m1)^3 x [0,m2)^3"
                       seed))))

  ; (new, plan §4.1 WP12) the MRG32k3a source.  `seed` is an exact integer (default 17) or six
  ; integers (list, vector or f64vector), for example L'Ecuyer's all-12345 default state.
  ; Doubles are lo + u*(hi-lo) with u in (0,1); integers are lo + floor(u*(hi-lo+1)), at most
  ; hi.  u carries about 32 random bits, so ranges wider than 2^32 are not all reachable.
  (define (make-mrg32k3a-source #!optional (seed 17))
    (make-rng-source 'mrg32k3a
                     (mrg32k3a-seed->state seed)
                     (λ (s lo hi) (unit->range (mrg32k3a-next! s) lo hi))
                     (λ (s lo hi)
                       (check-integer-bounds 'rng-integer! lo hi)
                       (unit->integer (mrg32k3a-next! s) lo hi))
                     (λ (s) (list->f64vector (f64vector->list s)))))

  ; (new, plan §4.1 WP12 and §12 bugs A, B) the seed (chicken random) receives: a bytevector
  ; zero-padded to a whole number of 8-byte words, because C_set_random_seed reads whole words
  ; only (it ignores trailing bytes and, for fewer than 8, reads past the buffer); an all-zero
  ; seed is rejected, because the all-zero WELL512 state is a fixed point and
  ; `pseudo-random-integer` then never returns (random_uniform's `do r = random_word(); while
  ; (r < min);` retry loop).  Cf. chicken-core main rev 55615603, runtime.c:14179 (that loop)
  ; and runtime.c:14296ff (C_set_random_seed); the chicken-core branch hakaru10-numerics
  ; fixes both in random_seed, but the adapter targets main and keeps the workaround.
  (define (chicken-random-seed seed)
    (unless (bytevector? seed)
      (hakaru10-error 'parameter 'make-chicken-random-source "seed must be a bytevector" seed))
    (when (every zero? (u8vector->list seed))
      (hakaru10-error 'parameter 'make-chicken-random-source
                 "an all-zero seed would hang (chicken random)" seed))
    (let* ((n (bytevector-length seed))
           (padded (make-bytevector (* 8 (max 1 (quotient (+ n 7) 8))) 0)))
      (bytevector-copy! padded 0 seed)
      padded))

  ; (new, plan §4.1 WP12) a u in (0,1) from (chicken random): pseudo-random-real answers [0,1]
  ; (1.0 with probability about 2^-54), so the two endpoints are redrawn.
  (define (chicken-random-unit)
    (let1 (u (pseudo-random-real))
      (if (and (fp< 0. u) (fp< u 1.)) u (chicken-random-unit))))

  ; (new, plan §4.1 WP12) an adapter over (chicken random), whose WELL512 state is global: all
  ; such sources share it, `rng-source-copy` cannot replay it and raises, and a seed (a
  ; bytevector, see chicken-random-seed) reseeds the whole process.  Without a seed the current
  ; global state is used; (random-bytes) gives a seed from OS entropy.  Doubles map a u in
  ; (0,1); integers come from pseudo-random-integer, exactly uniform on [lo, hi].
  (define (make-chicken-random-source #!optional seed)
    (when seed (set-pseudo-random-seed! (chicken-random-seed seed)))
    (make-rng-source 'chicken-random
                     'global
                     (λ (_ lo hi) (unit->range (chicken-random-unit) lo hi))
                     (λ (_ lo hi)
                       (check-integer-bounds 'rng-integer! lo hi)
                       (let1 (l (min lo hi))
                         (+ l (pseudo-random-integer (add1 (- (max lo hi) l))))))
                     (λ (_) (hakaru10-error 'parameter 'rng-source-copy
                                       "the global state of (chicken random) cannot be copied"))))

  )
