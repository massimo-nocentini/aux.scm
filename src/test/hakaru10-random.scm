; The suite for (aux hakaru10 random): the random-1.0.1.1 StdGen that GHC used for Hakaru10's
; recorded runs, and the rng-source protocol of (aux hakaru10).  Cases R1-R10 of the plan (WP1,
; WP12).  The StdGen values come from random-1.0.1.1 itself, read through a bignum transcription
; (the oracle `stdgen-double/reference`, and the Python transcription that replayed upstream's
; records); the MRG32k3a values come from L'Ecuyer's C code compiled on this machine.

(import scheme
        (chicken base)
        (chicken condition)
        (chicken bytevector)
        srfi-4
        (aux base)
        (aux unittest)
        (aux hakaru10 random))

; the `kind` of the (exn hakaru10) condition that thunk raises, or 'none.
(define (hakaru10-kind thunk)
  (condition-case (begin (thunk) 'none)
    (e (hakaru10) (hakaru10-error-kind e))))

; how many of n successive draws of the fixnum stdgen-double and of the bignum reference differ,
; in value or in the generator they answer, starting from (seed->stdgen seed).
(define (stdgen-double-mismatches seed n)
  (let loop ((i 0) (g (seed->stdgen seed)) (bad 0))
    (if (= i n)
        bad
        (let*-values (((d g1) (stdgen-double g))
                      ((r g2) (stdgen-double/reference g)))
          (loop (add1 i)
                g1
                (if (and (eqv? d r)
                         (= (stdgen-s1 g1) (stdgen-s1 g2))
                         (= (stdgen-s2 g1) (stdgen-s2 g2)))
                    bad
                    (add1 bad)))))))

; n draws from src, alternating a double in [-1,1] and an integer in [0,99], in a list.
(define (draws src n)
  (let loop ((i 0) (acc '()))
    (if (= i n)
        (reverse acc)
        (let* ((d (rng-double! src -1. 1.))
               (k (rng-integer! src 0 99)))
          (loop (add1 i) (cons k (cons d acc)))))))

; whether n draws of (draw) all satisfy ok?
(define (all-draws? n draw ok?)
  (let loop ((i 0))
    (or (= i n) (and (ok? (draw)) (loop (add1 i))))))

(define-suite hakaru10-random

  ((doc r) `((structure/section "Random numbers")
             (p "Hakaru10 "
                (cite/a "https://okmij.org/ftp/kakuritu/Hakaru10/index.html"
                        "Embedded probabilistic domain-specific language Hakaru10")
                " draws its random numbers from the " (code/inline "StdGen")
                " of the Haskell package "
                (cite/a "https://hackage-content.haskell.org/package/random-1.0.1.1/src/System/Random.hs"
                        "random-1.0.1.1")
                ", seeded with " (code/inline "mkStdGen 17")
                " (Metropolis.hs:858) and threaded through the chain state (Metropolis.hs:166,
                204-210). Porting that generator, bit for bit, lets the tests of (aux hakaru10)
                replay upstream's chains exactly: the samples recorded in the Haskell sources are
                regression oracles, not merely statistical targets.")
             (p "The generator is Lennart Augustsson's transliteration of L'Ecuyer's portable
                combined generator for 32-bit computers: two multiplicative congruential
                generators with moduli " (m "2147483563") " and " (m "2147483399") ", computed with
                Schrage's method so that no product exceeds " (m "2³¹") ".  Each "
                (code/inline "next") " answers " (m "z ∈ [1, 2147483562]") ".  An "
                (code/inline "Int") " in a range of size " (m "k") " folds "
                (m "n = ⌈log k / log b⌉") " of them, " (m "b = 2147483561") ", as")
             (displaymath (m "acc₀ = 1,   accᵢ₊₁ = zᵢ₊₁ + b · accᵢ,   x = l + (accₙ mod k)"))
             (p "and a " (code/inline "Double") " is an " (code/inline "Int64")
                " drawn this way over
                the whole 64-bit range, " (m "n = 3") ", masked to its 53 low bits and divided by "
                (m "2⁵³") ".  Upstream's " (code/inline "Integer") " arithmetic makes "
                (m "acc₃") " a number of about 93 bits; since " (m "2⁶³") " and " (m "2⁶⁴")
                " are multiples of " (m "2⁵³") ", only")
             (displaymath (m "acc₃ mod 2⁵³,   (z + b · acc) mod 2⁵³")
                          (m " = (z + ((h b) mod 2²⁶) · 2²⁷ + ℓ b) mod 2⁵³,   acc = h · 2²⁷ + ℓ"))
             (p "matters, and the port keeps every intermediate below " (m "2⁵⁹")
                ", a fixnum on a 64-bit CHICKEN, instead of allocating bignums at each draw.  The
                bignum reading of the Haskell code is kept as the oracle "
                (code/inline "stdgen-double/reference") ".")
             (p "The engine never threads a generator: it draws from an " (i "rng source") ", a
                record holding a mutable state and the procedures " (code/inline "rng-double!")
                " (Haskell's " (code/inline "randomR (lo,hi) :: Double") ", a closed interval) and "
                (code/inline "rng-integer!") " (" (code/inline "randomR (lo,hi) :: Int")
                ", inclusive).  Upstream never rolls the generator back when a proposal is
                rejected (Metropolis.hs:904-906), so a source that only moves forward is
                faithful; " (code/inline "rng-source-copy") " replays one.")
             (p "Two optional sources replay nothing upstream: L'Ecuyer's MRG32k3a "
                (cite/a "https://pubsonline.informs.org/doi/10.1287/opre.47.1.159"
                        "Good parameters and implementations for combined multiple recursive
                        random number generators, Operations Research 47(1), 1999")
                ", period " (m "≈ 2¹⁹¹") ", and an adapter over CHICKEN's "
                (code/inline "(chicken random)") ".  The latter has caveats: its WELL512 state is
                global to the process, so the adapter cannot be copied; an all-zero seed is a
                fixed point of the generator and " (code/inline "pseudo-random-integer")
                " then never returns (the retry loop of " (code/inline "random_uniform")
                " in runtime.c, chicken-core main rev 55615603), so the adapter rejects it; a
                seed is read in whole 8-byte words only (" (code/inline "C_set_random_seed")
                ", same file and revision), so the adapter pads it; "
                (code/inline "pseudo-random-real") " can answer " (code/inline "1.0")
                ", which the adapter redraws; and the startup seed comes from the clock.")
             (code/scheme/file "../aux.hakaru10.random.scm")))

  ((test/seed->stdgen _)
   (let1 (g (seed->stdgen 17))
     (⊦ equal? '(18 1) (list (stdgen-s1 g) (stdgen-s2 g))))
   (⊦ equal? '((1 1) (2 1) (86 2) (82 2) (18 1))
     (map (λ (n) (let1 (g (seed->stdgen n)) (list (stdgen-s1 g) (stdgen-s2 g))))
       (list 0 1 2147483647 -5 (+ (expt 2 40) 17))))
   (⊦ eq? 'parameter (hakaru10-kind (τ (seed->stdgen 1.5))))
   `(doc (p "R1.  " (code/inline "(seed->stdgen 17)") ", upstream's " (code/inline "mkStdGen 17")
            ", is the generator " (code/inline "18 1")
            ".  The seed is cut to 32 bits and masked to 31, as the conversion to "
            (code/inline "Int32") " does; so " (m "2⁴⁰ + 17") " seeds like 17 and "
            (m "−5") " like " (m "2³¹ − 5") ":")
         (code/lang "haskell" "mkStdGen :: Int -> StdGen -- why not Integer ?
mkStdGen s = mkStdGen32 $ fromIntegral s

mkStdGen32 :: Int32 -> StdGen
mkStdGen32 sMaybeNegative = StdGen (s1+1) (s2+1)
      where
	-- We want a non-negative number, but we can't just take the abs
	-- of sMaybeNegative as -minBound == minBound.
	s       = sMaybeNegative .&. maxBound
	(q, s1) = s `divMod` 2147483562
	s2      = q `mod` 2147483398")
         (p "cf. R:235-245.")))

  ((test/stdNext _)
   (let*-values (((a g) (stdgen-next (seed->stdgen 17)))
                 ((b g) (stdgen-next g))
                 ((c g) (stdgen-next g)))
     (⊦ equal? '(679560 1394521907 656733876) (list a b c))
     (⊦ equal? '(612660577 2103410263) (list (stdgen-s1 g) (stdgen-s2 g))))
   (let1 (g (seed->stdgen 17))
     (stdgen-next g)
     (⊦ equal? '(18 1) (list (stdgen-s1 g) (stdgen-s2 g)))
     (⊦ equal? '(679560 1394521907 656733876)
       (let* ((a (stdgen-next! g))
              (b (stdgen-next! g))
              (c (stdgen-next! g)))
         (list a b c)))
     (⊦ equal? '(612660577 2103410263) (list (stdgen-s1 g) (stdgen-s2 g))))
   `(doc (p "R2.  The first three outputs of " (code/inline "mkStdGen 17") " and the generator
            they leave, " (code/inline "612660577 2103410263") "; " (code/inline "stdgen-next")
            " is pure and leaves its argument untouched, while its in-place twin "
            (code/inline "stdgen-next!") ", the one the sources use, steps it:")
         (code/lang "haskell" "stdNext :: StdGen -> (Int, StdGen)
-- Returns values in the range stdRange
stdNext (StdGen s1 s2) = (fromIntegral z', StdGen s1'' s2'')
	where	z'   = if z < 1 then z + 2147483562 else z
		z    = s1'' - s2''

		k    = s1 `quot` 53668
		s1'  = 40014 * (s1 - k * 53668) - k * 12211
		s1'' = if s1' < 0 then s1' + 2147483563 else s1'

		k'   = s2 `quot` 52774
		s2'  = 40692 * (s2 - k' * 52774) - k' * 3791
		s2'' = if s2' < 0 then s2' + 2147483399 else s2'")
         (p "cf. R:474-486.")))

  ((test/randomR-double _)
   (⊦ = 0.2938995850243806 (rng-double! (make-stdgen-source 17) 0. 1.))
   (⊦ = 0.2938995850243806 (rng-double! (make-stdgen-source) 0 1))
   (⊦ = -1.8100773861098114 (rng-double! (make-stdgen-source 17) -3.5 2.25))
   (let*-values (((d g) (stdgen-double (seed->stdgen 17))))
     (⊦ = 0.2938995850243806 d)
     (⊦ equal? '(612660577 2103410263) (list (stdgen-s1 g) (stdgen-s2 g))))
   `(doc (p "R3.  The first " (code/inline "randomR (0,1) :: Double") " of seed 17 is "
            (code/inline "0.2938995850243806") ", after three " (code/inline "next")
            "s; the scaling keeps Haskell's association, " (code/inline "-1.8100773861098114")
            " for the range " (m "[−3.5, 2.25]") ":")
         (code/lang "haskell" "randomRFloating (l,h) g
    | l>h       = randomRFloating (h,l) g
    | otherwise = let (coef,g') = random g in
		  (2.0 * (0.5*l + coef * (0.5*h - 0.5*l)), g')  -- avoid overflow

instance Random Double where
  randomR = randomRFloating
  random rng     =
    case random rng of
      (x,rng') ->
          -- We use 53 bits of randomness corresponding to the 53 bit significand:
          ((fromIntegral (mask53 .&. (x::Int64)) :: Double)
	   /  fromIntegral twoto53, rng')
   where
    twoto53 = (2::Int64) ^ (53::Int64)
    mask53 = twoto53 - 1")
         (p "cf. R:361-378; the values are those of the Python transcription that replayed
            upstream's records.")))

  ((test/randomR-int _)
   (⊦ = 1 (rng-integer! (make-stdgen-source 17) 0 9))
   (⊦ = 1 (rng-integer! (make-stdgen-source 17) 9 0))
   (let*-values (((v g) (stdgen-ival-integer (- (expt 10 30)) (expt 10 30) (seed->stdgen 17))))
     (⊦ = -471988287518935644257797793060 v)
     (⊦ equal? '(1475456433 1872071452) (list (stdgen-s1 g) (stdgen-s2 g))))
   (let1 (src (make-stdgen-source 17))
     (⊦ = 5 (rng-integer! src 5 5))
     (let1 (g (rng-source-state src))
       (⊦ equal? '(720252 40692) (list (stdgen-s1 g) (stdgen-s2 g)))))
   (⊦ eq? 'parameter (hakaru10-kind (τ (rng-integer! (make-stdgen-source) 0. 9))))
   `(doc (p "R4.  The first " (code/inline "randomR (0,9) :: Int") " of seed 17 is "
            (code/inline "1") ", that is " (m "(679560 + b) mod 10") "; a range wider than "
            (m "b") " folds several outputs through exact integers (here "
            (m "2 · 10³⁰ + 1")
            ", four outputs), and the one-point range " (m "[5,5]") " still consumes one:")
         (code/lang "haskell" "randomIvalInteger :: (RandomGen g, Num a) => (Integer, Integer) -> g -> (a, g)
randomIvalInteger (l,h) rng
 | l > h     = randomIvalInteger (h,l) rng
 | otherwise = case (f n 1 rng) of (v, rng') -> (fromInteger (l + v `mod` k), rng')
     where
       k = h - l + 1
       -- ERROR: b here (2^31-87) represents a baked-in assumption about genRange:
       b = 2147483561
       n = iLogBase b k

       -- Here we loop until we've generated enough randomness to cover the range:
       f 0 acc g = (acc, g)
       f n' acc g =
          let
	   (x,g')   = next g
	  in
          -- We shift over the random bits generated thusfar (* b) and add in the new ones.
	  f (n' - 1) (fromIntegral x + acc * b) g'")
         (p "cf. R:424-441, used by uniformly (D:82) and by the choice of the node to resample
            (M:962).")))

  ((test/fixnum-double-vs-reference _)
   (⊦ equal? '(0 0 0 0 0)
     (map (λ (seed) (stdgen-double-mismatches seed 100000)) '(17 0 1 2147483647 -5)))
   `(doc (p "R5.  The fixnum " (code/inline "stdgen-double") ", which keeps " (m "acc")
            " modulo " (m "2⁵³") " at each step, agrees with the bignum reading of the Haskell
            code, value and generator, on " (m "10⁵") " successive draws from each of the seeds
            17, 0, 1, " (m "2³¹ − 1") " and " (m "−5") ".")))

  ((test/copy-replays _)
   (let* ((src (make-stdgen-source 17))
          (_ (draws src 10))
          (copy (rng-source-copy src))
          (a (draws src 500))
          (b (draws copy 500)))
     (⊦ equal? a b)
     (⊦ equal? (draws (make-stdgen-source 17) 10)
       (draws (make-stdgen-source (seed->stdgen 17)) 10)))
   `(doc (p "R6.  A copy of a source replays its next 1000 draws (500 doubles and 500
            integers, interleaved), and the copy is independent of the original.")))

  ((test/bounds _)
   (let1 (src (make-stdgen-source 17))
     (⊨ (all-draws? 100000 (τ (rng-double! src -2.5 4.)) (λ (x) (and (<= -2.5 x) (<= x 4.)))))
     (⊨ (all-draws? 1000 (τ (rng-double! src 4. -2.5)) (λ (x) (and (<= -2.5 x) (<= x 4.)))))
     (⊨ (all-draws? 1000 (τ (rng-integer! src 9 -3)) (λ (k) (and (<= -3 k) (<= k 9)))))
     (⊦ = 1. (rng-double! src 1. 1.)))
   (let*-values (((a _) (stdgen-range-double -2.5 4. (seed->stdgen 17)))
                 ((b _) (stdgen-range-double 4. -2.5 (seed->stdgen 17))))
     (⊦ = a b))
   `(doc (p "R7.  " (m "10⁵") " doubles stay in the closed interval " (m "[lo, hi]")
            "; swapped bounds give the same draw as ordered ones, for doubles and integers alike,
            as the first guard of " (code/inline "randomRFloating") " and "
            (code/inline "randomIvalInteger") " does.")))

  ((test/ilog-base _)
   (⊦ equal? '(1 2 3 3) (map (λ (k) (ilog-base 2147483561 k))
                          (list 10 2147483561 (expt 2147483561 2) (expt 2 64))))
   (let*-values (((x g) (stdgen-ival-integer 0 2147483560 (seed->stdgen 17)))
                 ((z1 g1) (stdgen-next (seed->stdgen 17)))
                 ((z2 g2) (stdgen-next g1)))
     (⊦ = (modulo (+ z2 (* (+ z1 (* 1 2147483561)) 2147483561)) 2147483561) x)
     (⊦ equal? (list (stdgen-s1 g2) (stdgen-s2 g2)) (list (stdgen-s1 g) (stdgen-s2 g))))
   `(doc (p "R8.  The number of outputs a range needs: one for " (m "k = 10") ", three for the "
            (m "2⁶⁴") " of an " (code/inline "Int64") "; the comment upstream notes the
            off-by-one at " (m "k = b") ", where a range of exactly " (m "b")
            " values consumes two outputs, folded as " (m "acc₂ = z₂ + b (z₁ + b)") ":")
         (code/lang "haskell" "-- Perform an expensive logarithm on arbitrary-size integers by repeated division.
--
-- (NOTE: This actually returns ceiling(log(i) base b) except with an
--  incorrect result at iLogBase b b = 2.)
iLogBase :: Integer -> Integer -> Integer
iLogBase b i = if i < b then 1 else 1 + iLogBase b (i `div` b)")
         (p "cf. R:464-469.")))

  ((test/mrg32k3a _)
   (let1 (src (make-mrg32k3a-source '(12345 12345 12345 12345 12345 12345)))
     (⊦ equal? '(0.12701112204657714 0.3185275653967945 0.30918601558327008)
       (let* ((a (rng-double! src 0. 1.))
              (b (rng-double! src 0. 1.))
              (c (rng-double! src 0. 1.)))
         (list a b c)))
     (⊨ (all-draws? 997 (τ (rng-double! src 0. 1.)) (λ (u) (and (< 0. u) (< u 1.)))))
     (⊦ equal? (f64vector 4239718941. 899640195. 1411745448. 2768972929. 343921931. 1471537888.)
       (rng-source-state src)))
   (let* ((src (make-mrg32k3a-source 17))
          (copy (rng-source-copy src)))
     (⊦ equal? (draws src 100) (draws copy 100))
     (⊨ (all-draws? 10000 (τ (rng-integer! src 9 0)) (λ (k) (and (<= 0 k) (<= k 9)))))
     (⊨ (all-draws? 10000 (τ (rng-double! src 3. -1.)) (λ (x) (and (<= -1. x) (<= x 3.))))))
   (⊦ equal? '(parameter parameter parameter)
     (map (λ (seed) (hakaru10-kind (τ (make-mrg32k3a-source seed))))
       (list '(0 0 0 1 1 1) '(1 1 4294967087 1 1 1) 'seventeen)))
   `(doc (p "R9 (optional source).  MRG32k3a from L'Ecuyer's default state, all components "
            (code/inline "12345") ": the first three outputs and the state after 1000 of them
            are those of the C code of the paper (Fig. 1), compiled here, bit for bit; a copy
            replays, and integers and swapped bounds stay in range.  A seed with an all-zero
            triple, a component out of range, or of the wrong type is a "
            (code/inline "parameter") " error.")))

  ((test/chicken-random _)
   (⊦ equal? '(parameter parameter parameter)
     (map (λ (seed) (hakaru10-kind (τ (make-chicken-random-source seed))))
       (list (make-bytevector 16 0) (bytevector 0) "17")))
   (let* ((a (draws (make-chicken-random-source (bytevector 17)) 5))
          (b (draws (make-chicken-random-source (bytevector 17 0 0 0 0 0 0 0)) 5))
          (src (make-chicken-random-source (bytevector 1 2 3 4 5 6 7 8 9))))
     (⊦ equal? a b)
     (⊨ (all-draws? 1000 (τ (rng-double! src 2. 0.)) (λ (x) (and (< 0. x) (< x 2.)))))
     (⊨ (all-draws? 1000 (τ (rng-integer! src (expt 2 70) (+ (expt 2 70) 3)))
                    (λ (k) (and (<= (expt 2 70) k) (<= k (+ (expt 2 70) 3))))))
     (⊦ eq? 'parameter (hakaru10-kind (τ (rng-source-copy src)))))
   `(doc (p "R10 (optional source).  The adapter over " (code/inline "(chicken random)")
            " rejects an all-zero seed, which would make " (code/inline "pseudo-random-integer")
            " loop forever, whatever its length; it pads a short seed to whole words, so "
            (code/inline "#u8(17)") " and " (code/inline "#u8(17 0 0 0 0 0 0 0)")
            " give the same stream; and it cannot be copied, its state being global.")))

  )

(unittest/✓ hakaru10-random)
