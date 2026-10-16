; The suite for (aux hansei random): MD5, OCaml's `Random` generator and the selectors of (aux
; hansei) over it, which replay HANSEI's recorded samples bit for bit (samplingM.ml, nested.ml).
; Cases R1-R9 of the plan AUX-HANSEI-PLAN.md §8.2 (WP2, WP5).  The expected values come from
; RFC 1321, from the Python transcription of OCaml's generator (plan §10.3), and from upstream
; HANSEI run under OCaml with the three-draw `Random.float` (plan D6, D20).

; Two CHICKEN hazards for stochastic code (plan §5.6, D11 and D12), which every model of this
; file avoids:
;   - `map` is not re-entrant: CHICKEN's `map` (and srfi-1's) builds its result destructively, so
;     resuming a continuation captured inside its procedure corrupts the list, e.g.
;     (probcc-reify/exact (map (λ (x) (probcc-coin 1/2)) '(1 2))) answers the single
;     entry (#t #t #f #f #t #f) at mass 1, the four worlds sharing one list built in place.
;     Models use probcc-map, left to right as OCaml's List.map, never map, vector-map or
;     for-each; nor srfi-1's list-tabulate and filter-map, which are re-entrant but call their
;     procedure from the last element to the first.
;   - under csi, resuming a continuation captured while the operands of a call with five or more
;     operands are evaluated accumulates arguments, even with plain call/cc, e.g.
;     (probcc-reify/exact (list 'a 'b 'c 'd (probcc-coin 1/2))) answers (a b c d #t) and
;     (a b c d #t #f); quasiquote templates and constructors (list, vector) are calls too, and
;     the position of the choice does not matter.  Models bind every choice with let* or let1
;     before using it in such a call.  Compiled code is correct.

(import scheme
        (chicken base)
        (chicken flonum)
        (only (chicken memory representation) record->vector)
        (only (chicken sort) sort)
        (only srfi-1 delete-duplicates make-list iota)
        (aux base)
        (aux unittest)
        (aux hansei)
        (only (scheme base) string->utf8)
        (only (chicken condition) condition-property-accessor)
        (only (aux hakaru10 random) rng-double! rng-integer! rng-source-copy make-stdgen-source
              make-chicken-random-source)
        (aux hansei random))

; replays and bit-exact checks print 17 digits: with the default 15, a one-ulp failure would print
; identical expected and got values (plan §8 conventions).
(flonum-print-precision 17)

; The models of samplingM.ml and nested.ml, with flonum masses as upstream; choices are
; sequenced in OCaml's order (D19): `let x = e1 in let y = e2` and `let … and …` from the left,
; tuples and arguments from the right.

; cf. SM:13-15, shared flips.
(define (tflip2-shared) (let1 (v (probcc-coin 0.5)) (and v v)))

; cf. SM:23-24 = N:142-143, independent flips.
(define (tflip2-noshared) (and (probcc-coin 0.5) (probcc-coin 0.5)))

; cf. SM:32-37: `let earthquake = flip 0.01 and burglary = flip 0.1`, earthquake first.
(define (talarm)
  (let* ((earthquake (probcc-coin 0.01))
         (burglary (probcc-coin 0.1)))
    (if earthquake
        (if burglary (probcc-coin 0.99) (probcc-coin 0.2))
        (if burglary (probcc-coin 0.98) (probcc-coin 0.01)))))

; cf. SM:129-142: the objects are lists (mz mw).
(define (tiblt2)
  (let* ((x (probcc-observe (λ (r) (eq? (car r) 'a))
                            (τ (probcc-distribution '(((a b) 0.01) ((a c) 0.02) ((d e) 0.97))))))
         (y (case (cadr x)
              ((b) (probcc-distribution '((#t 0.9) (#f 0.1))))
              ((c) (probcc-distribution '((#t 0.6) (#f 0.4))))
              (else (probcc-distribution '((#t 0.2) (#f 0.8)))))))
    y))

; cf. SM:164-168: the list [(0.8, f ()); (0.2, g ())] is built from the right, g () first.
(define (tmusictr41)
  (probcc-observe (λ (x) (eq? x #t))
                  (τ (let* ((b (probcc-coin 0.3))
                            (a (probcc-coin 0.1)))
                       (probcc-distribution `((,a 0.8) (,b 0.2)))))))

; cf. SM:224-233 = N:216-225, the drunk coin: x drawn before lost.
(define (drunk-coin)
  (let* ((x (probcc-coin 0.5))
         (lost (probcc-coin 0.9)))
    (if lost (probcc-impossible) x)))

(define (dcoin-and n)
  (if (= n 1) (drunk-coin) (and (drunk-coin) (dcoin-and (sub1 n)))))

(define-suite hansei-random

  ((doc r)
   `((structure/section "Replaying upstream's records")
     (p "HANSEI's samplers draw from OCaml's " (code/inline "Random") " module, through "
        (code/inline "random_selector seed") ", which re-initialises its global state (P:221-222). "
        (code/inline "(aux hansei random)") " transcribes that generator, the lagged Fibonacci
        generator of 55 words seeded with MD5 " (cite/a "https://www.rfc-editor.org/rfc/rfc1321" "RFC 1321")
        ", with the xor " (code/inline "bits") " and the three-draw " (code/inline "Random.float")
        " of the OCaml releases HANSEI's records were made with (OCaml 4.14 draws two words), the
        only variant that replays the records of " (code/inline "samplingM.ml") " and "
        (code/inline "nested.ml") " bit for bit; see the plan, D6 and D20, and "
        (cite/a "https://okmij.org/ftp/kakuritu/Hansei.html" "Embedded probabilistic domain-specific language HANSEI") ".")
     (p "Why three draws: OCaml 4.14's " (code/inline "Random.float") " (OR:167-172) draws two
        30-bit words, " (code/inline "(r1 /. 2^30 +. r2) /. 2^30") ", and with it the rejection
        sampling of " (code/inline "dcoin_and 10") " over 10000 samples, seed 17, gives "
        (code/inline "0.054") " where SM records " (code/inline "0.0514") "; three words, "
        (code/inline "((r0 /. 2^30 +. r1) /. 2^30 +. r2) /. 2^30") ", replay every assertion
        of " (code/inline "samplingM.ml") ", " (code/inline "nested.ml") ", "
        (code/inline "slazy.ml") ", " (code/inline "parsec.ml") " and "
        (code/inline "music1a.ml") " under an OCaml build of HANSEI patched so (deviation 32).
        The global state is upstream's too: a selector made with a seed re-initialises the one
        state every selector draws from, which N:30-31 needs (D6).")
     (p "The expected values come from three sources: upstream's own assertions; runs of
        upstream HANSEI under OCaml 4.14.2 with delimcc, two-draw and three-draw; and a Python
        transcription of the samplers over OCaml's generator, written while planning the port
        and kept in " (code/inline "test/hansei-python/") " (its README says how to run it under "
        (code/inline "python3 -I") "; it is not part of " (code/inline "make test") ").")))

  ((test/md5 _)
   (⊦ equal? "d41d8cd98f00b204e9800998ecf8427e" (md5-hex ""))
   (⊦ equal? "0cc175b9c0f1b6a831c399e269772661" (md5-hex "a"))
   (⊦ equal? "900150983cd24fb0d6963f7d28e17f72" (md5-hex "abc"))
   (⊦ equal? "f96b697d7cb7938d525a2f31aaf161d0" (md5-hex "message digest"))
   (⊦ equal? "c3fcd3d76192e4007dfb496cca67e13b" (md5-hex "abcdefghijklmnopqrstuvwxyz"))
   (⊦ equal? "d174ab98d277d9f5a5611c2c9f419d9f"
      (md5-hex "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"))
   (⊦ equal? "57edf4a22be3c955ac49da2e2107b67a"
      (md5-hex (apply string-append (make-list 8 "1234567890"))))
   ; the padding boundaries: 55 and 56 bytes (one block or two), 63 and 64, 119 and 120.
   (⊦ equal? "04364420e25c512fd958a70738aa8f72" (md5-hex (make-string 55 #\x)))
   (⊦ equal? "668a72d5ba17f08e62dabcafad6db14b" (md5-hex (make-string 56 #\x)))
   (⊦ equal? "7dc2ca208106a2f703567bdff99d8981" (md5-hex (make-string 63 #\x)))
   (⊦ equal? "c1bb4f81d892b2d57947682aeb252456" (md5-hex (make-string 64 #\x)))
   (⊦ equal? "b2ea3db8221fa9ae01ee0adf42970ba0" (md5-hex (make-string 119 #\y)))
   (⊦ equal? "723628a6315542e744e4bc6e2f9c84d2" (md5-hex (make-string 120 #\y)))
   (⊦ equal? #u8(#x90 #x01 #x50 #x98 #x3c #xd2 #x4f #xb0 #xd6 #x96 #x3f #x7d #x28 #xe1 #x7f #x72)
      (md5-digest #u8(97 98 99)))
   `(doc (p (code/inline "md5-digest") " is OCaml's " (code/inline "Digest.string") ", which "
            (code/inline "Random.full_init") " uses to seed the state: the test suite of "
            (cite/a "https://www.rfc-editor.org/rfc/rfc1321" "RFC 1321") " (A.5), and messages of
	    55 to 120 bytes around the padding boundaries, whose digests come from OCaml 4.14.2.")))

  ((test/ocaml/init _)
   ; the 55 words and the index of a state, through the record's slots #(tag st idx), since
   ; OCaml's state is abstract and the module exports no accessor.
   (define (words s) (vector-ref (record->vector s) 1))
   (define (idx s) (vector-ref (record->vector s) 2))
   (define (first-words s) (list (vector-ref (words s) 0) (vector-ref (words s) 1)
                                 (vector-ref (words s) 2) (vector-ref (words s) 3)
                                 (vector-ref (words s) 4)))
   (define s1 (make-ocaml-random-state 1))
   (⊦ equal? '(613788242 197116386 1014951421 468579290 805446940) (first-words s1))
   (⊦ equal? '(145365742 323272762 161958640 284739705 672753100)
      (list (vector-ref (words s1) 50) (vector-ref (words s1) 51) (vector-ref (words s1) 52)
            (vector-ref (words s1) 53) (vector-ref (words s1) 54)))
   (⊦ equal? 0 (idx s1))
   (⊦ equal? '(43684900 826807282 853359337 631525392 682719058)
      (first-words (make-ocaml-random-state 17)))
   (⊦ equal? '(76722835 608589901 287828993 653199777 415478086)
      (first-words (make-ocaml-random-state 0)))
   (⊦ equal? '(640528891 982617058 790696909 229908377 1042402058)
      (first-words (make-ocaml-random-state -5)))
   (⊦ equal? '(972831248 467146799 206477288 695949822 606390225)
      (first-words (make-ocaml-random-state 123456789)))
   ; OCaml's default state (OR:197-213), the first words of its literal array.
   (⊦ equal? '(987910699 495797812 364182224 414272206 318284740)
      (first-words (make-ocaml-random-state)))
   (⊦ equal? #t (ocaml-random-state? ocaml-random-global))
   (⊦ equal? #f (ocaml-random-state? (words s1)))
   (⊦raises (exn hansei) (make-ocaml-random-state 'seed))
   (⊦raises (exn hansei) (make-ocaml-random-state 1.))
   (⊦raises (exn hansei) (make-ocaml-random-state #(1 a)))
   `(doc (p (code/inline "make-ocaml-random-state seed") " is OCaml's "
            (code/inline "Random.State.make [| seed |]") " (OR:40-64): the first and last words
	    after seeding, as OCaml 4.14.2 prints them through " (code/inline "Obj.magic")
            " (and as the Python transcription of the plan, §10.3, computes them). Without a seed
	    it is OCaml's default state, the one " (code/inline "Random.bits ()") " draws from in a
	    program that never seeds; a seed that is not an exact integer, or a vector of them,
	    raises " (code/inline "(exn hansei)") " of kind " (code/inline "random") ".")))

  ((test/ocaml/bits _)
   ; n draws of (f), left to right, in a list (srfi-1's list-tabulate calls f from the last
   ; element to the first).
   (define (draws n f) (if (zero? n) '() (let1 (x (f)) (cons x (draws (sub1 n) f)))))
   (define (bits s) (draws 5 (τ (ocaml-random-bits! s))))
   (⊦ equal? '(111472438 155495486 1029890784 56915826 660037571)
      (bits (make-ocaml-random-state 1)))
   (⊦ equal? '(230024973 197978033 495359752 1036278348 906260834)
      (bits (make-ocaml-random-state 17)))
   (⊦ equal? '(218040752 178628190 911923154 33712936 1041571162)
      (bits (make-ocaml-random-state 0)))
   (⊦ equal? '(638983422 1072006411 37028206 270829641 521483706)
      (bits (make-ocaml-random-state -5)))
   (⊦ equal? '(1047213900 619322777 232370739 831219731 841638859)
      (bits (make-ocaml-random-state 123456789)))
   (⊦ equal? '(89809344 994326685 290180182 366831641 332180439)
      (bits (make-ocaml-random-state)))
   ; Random.full_init: [| |] is [| 0 |] (OR:46), and a longer seed.
   (⊦ equal? '(218040752 178628190 911923154 33712936 1041571162)
      (bits (make-ocaml-random-state #())))
   (⊦ equal? '(311998834 678888426 938209723 870508417 248299610)
      (bits (make-ocaml-random-state #(1 2 3))))
   ; the 998th to 1000th draws, after the 55 words have been replaced many times (OCaml printed
   ; them with one printf, whose arguments it evaluates from right to left: 1044869830 is the
   ; 1000th).
   (⊦ equal? '(586659661 422753529 1044869830)
      (let1 (s (make-ocaml-random-state 17))
        (draws 997 (τ (ocaml-random-bits! s)))
        (draws 3 (τ (ocaml-random-bits! s)))))
   `(doc (p (code/inline "ocaml-random-bits!") " is OCaml's " (code/inline "Random.State.bits")
            " (OR:76-83), the xor variant of the lagged Fibonacci step: the first five draws after "
            (code/inline "Random.init") " for several seeds, after "
            (code/inline "Random.full_init") ", and from the default state, as OCaml 4.14.2 draws
	    them.")))

  ((test/ocaml/float _)
   (define (doubles seed)
     (let* ((src (make-ocaml-random-source seed))
            (a (rng-double! src 0. 1.))
            (b (rng-double! src 0. 1.))
            (c (rng-double! src 0. 1.)))
       (list a b c)))
   (⊦ equal? '(0.95916053666250445 0.047719400948572799 0.64524492170467784) (doubles 1))
   (⊦ equal? '(0.46133971976524352 0.18637992054598515 0.699662755317079) (doubles 17))
   (⊦ equal? '(0.84929461978968279 0.62951315564107035 0.4331830841471257) (doubles 0))
   (⊦ equal? '(0.03448520507512965 0.97297789481065189 0.34460494547462833) (doubles -5))
   ; Random.float bound = rawfloat *. bound (OR:175), drawn in the order 1e-3, 0.3, 2.5.
   (⊦ equal? '(0.00046133971976524351 0.055913976163795544 1.7491568882926976)
      (let* ((s (make-ocaml-random-state 17))
             (a (ocaml-random-float! s 1e-3))
             (b (ocaml-random-float! s 0.3))
             (c (ocaml-random-float! s 2.5)))
        (list a b c)))
   ; exact bounds are converted, as rng-double! does.
   (⊦ equal? 0.95916053666250445 (rng-double! (make-ocaml-random-source 1) 0 1))
   `(doc (p "The doubles of a fresh source are OCaml's three-draw " (code/inline "rawfloat")
            ", " (code/inline "((r0 / 2^30 + r1) / 2^30 + r2) / 2^30") ", bit for bit (OCaml
	    4.14.2 running the three-draw formula over " (code/inline "Random.bits") "): the one
	    variant that replays HANSEI's records (plan D6, D20).")))

  ((test/ocaml/source _)
   (define (draws n f) (if (zero? n) '() (let1 (x (f)) (cons x (draws (sub1 n) f)))))
   (define (ints seed n) (let1 (s (make-ocaml-random-state seed))
                           (draws 5 (τ (ocaml-random-int! s n)))))
   (define src (make-ocaml-random-source 17))
   (define copy (rng-source-copy src))
   (define used (make-ocaml-random-state 17))
   (define global-copy (ocaml-random-state-copy ocaml-random-global))
   (define global-source (make-ocaml-random-source ocaml-random-global))
   ; a copy replays 1000 draws of either kind.
   (⊦ equal? (draws 1000 (τ (rng-double! src 0. 1.))) (draws 1000 (τ (rng-double! copy 0. 1.))))
   (⊦ equal? (draws 1000 (τ (rng-integer! src 0 99))) (draws 1000 (τ (rng-integer! copy 0 99))))
   ; integers stay in range, bounds in either order, and every value is reached.
   (⊦ equal? '(-3 -2 -1 0 1 2 3 4)
      (sort (delete-duplicates (draws 1000 (τ (rng-integer! src 4 -3)))) <))
   (⊦ equal? '(9 7 5 7 2)
      (let1 (s (make-ocaml-random-source 1)) (draws 5 (τ (rng-integer! s 1 10)))))
   ; Random.int, values from OCaml 4.14.2.
   (⊦ equal? '(8 6 4 6 1) (ints 1 10))
   (⊦ equal? '(438 486 784 826 571) (ints 1 1000))
   (⊦ equal? '(3 3 2 8 4) (ints 17 10))
   (⊦ equal? '(973 33 752 348 834) (ints 17 1000))
   (⊦ equal? '(0 6 6 2 4) (ints 1 7))
   (⊦ equal? '(230024973 197978033 495359752 1036278348 906260834) (ints 17 #x3FFFFFFF))
   (⊦raises (exn hansei) (ocaml-random-int! used 0))
   (⊦raises (exn hansei) (ocaml-random-int! used #x40000000))
   (⊦raises (exn hansei) (rng-integer! src 0 1.))
   ; ocaml-random-init! on a used state starts over, and answers the state.
   (⊦ equal? '(230024973 197978033 495359752 1036278348 906260834)
      (begin (draws 100 (τ (ocaml-random-bits! used)))
             (let1 (s (ocaml-random-init! used 17))
               (draws 5 (τ (ocaml-random-bits! s))))))
   ; a source over a state shares it: drawing from the source over ocaml-random-global
   ; advances the global state as a Random.float would.
   (⊦ equal? (ocaml-random-float! global-copy 1.) (rng-double! global-source 0. 1.))
   (⊦ equal? (ocaml-random-bits! global-copy) (ocaml-random-bits! ocaml-random-global))
   `(doc (p (code/inline "make-ocaml-random-source") " makes an OCaml state an rng-source of "
            (code/inline "(aux hakaru10 random)") ": " (code/inline "rng-source-copy")
            " replays it, " (code/inline "rng-integer!") " is " (code/inline "lo + Random.int (hi -
	    lo + 1)") " (OR:86-94, values from OCaml 4.14.2), and a source over a given state, such
	    as " (code/inline "ocaml-random-global") ", draws from that state, not from a copy.")))

  ((test/replay/sampling _)
   (⊦ equal? '(((V #f) 0.52000000000000002) ((V #t) 0.47999999999999998))
      (probcc-sample-rejection/a (probcc-random-selector 1) 100 tflip2-shared))
   (⊦ equal? '(((V #f) 0.76000000000000001) ((V #t) 0.23999999999999999))
      (probcc-sample-rejection/a (probcc-random-selector 1) 100 tflip2-noshared))
   (⊦ equal? '(((V #f) 0.85999999999999999) ((V #t) 0.14000000000000001))
      (probcc-sample-rejection/a (probcc-random-selector 1) 100 talarm))
   (⊦ equal? '(((V #f) 0.89128000000000007) ((V #t) 0.10872000000000001))
      (probcc-sample-importance/a (probcc-random-selector 1) 100 talarm))
   (⊦ equal? '(((V 0) 0.79000000000000004) ((V 1) 0.17000000000000001) ((V 2) 0.040000000000000001))
      (probcc-sample-rejection (probcc-random-selector 17) 100 (probcc-geometric/count 0.85)))
   (⊦ equal? '(((V 0) 0.84999999999999998) ((V 1) 0.1275) ((V 2) 0.019574999999999981)
               ((V 3) 0.0024750000000000011) ((V 4) 0.0004500000000000001))
      (probcc-sample-importance (probcc-random-selector 17) 100 (probcc-geometric/count 0.85)))
   (⊦ equal? '(((V 0) 0.84999999999999998) ((V 1) 0.1275) ((V 2) 0.019080000000000406)
               ((V 3) 0.0028349999999999968) ((V 4) 0.00047250000000000032)
               ((V 5) 9.0000000000000019e-05) ((V 6) 2.2500000000000005e-05))
      (probcc-sample-importance (probcc-random-selector 17) 1000 (probcc-geometric/count 0.85)))
   (⊦ equal? '(((V #f) 0.02)) (probcc-sample-rejection/a (probcc-random-selector 1) 100 tiblt2))
   (⊦ equal? '(((V #t) 0.14000000000000001))
      (probcc-sample-importance/a (probcc-random-selector 1) 100 tmusictr41))
   (⊦ equal? '(((V #f) 0.050000000000000003))
      (probcc-sample-rejection (probcc-random-selector 17) 100 (dcoin-and 10)))
   (⊦ equal? '(((V #f) 0.051400000000000001))
      (probcc-sample-rejection (probcc-random-selector 17) 10000 (dcoin-and 10)))
   (⊦ equal? '(((V #f) 0.052301965599999993))
      (probcc-sample-importance (probcc-random-selector 17) 100 (dcoin-and 10)))
   (⊦ equal? '(((V #f) 0.052635478842930135) ((V #t) 6.9999999999999841e-14))
      (probcc-sample-importance (probcc-random-selector 1) 5000 (dcoin-and 10)))
   (⊦ equal? '(((V #f) 0.052641559436170146) ((V #t) 5.9999999999999871e-14))
      (probcc-sample-importance (probcc-random-selector 17) 5000 (dcoin-and 10)))
   (⊦ equal? '()
      (probcc-sample-rejection (probcc-random-selector 1) 10000
        (or (dcoin-and 10) (probcc-impossible))))
   `(doc (p "The sampling records of " (code/inline "samplingM.ml") " (SM:16-269, 398-401),
	    replayed bit for bit with " (code/inline "probcc-random-selector") ": rejection and
	    importance sampling of shared and independent flips, the burglar alarm, the geometric
	    distribution, the observation of " (code/inline "tiblt2") " and of the Music TR, and
	    the drunk coin, whose " (code/inline "dcoin_and_true 10") " rejection sampling
	    accepts no world in 10000. The tables are in the order of " (code/inline "probcc-explore")
            " (deviation 5), and the models draw in OCaml's order (D19).")))

  ((test/selector _)
   (define (draws n f) (if (zero? n) '() (let1 (x (f)) (cons x (draws (sub1 n) f)))))
   (define choices (probcc-map (λ (i) (list i 1/10)) (iota 10)))
   (define (picks selector) (draws 5 (τ (car (selector choices)))))
   ; a seed re-seeds the global state when the selector is made: a second selector of the same
   ; seed starts over, and the first one then draws from where the second left the state.
   (define s1 (probcc-random-selector 1))
   (define first-picks (picks s1))
   (define s2 (probcc-random-selector 1))
   (⊦ equal? first-picks (picks s2))
   (⊦ equal? (begin (probcc-random-selector 1) first-picks) (picks s1))
   ; an explicit source draws from it alone: a copy replays it, and the global state is unmoved.
   (define src (make-ocaml-random-source 1))
   (define src-copy (rng-source-copy src))
   (define global (ocaml-random-state-copy ocaml-random-global))
   (⊦ equal? first-picks (picks (probcc-random-selector src)))
   (⊦ equal? (ocaml-random-bits! global) (ocaml-random-bits! ocaml-random-global))
   (⊦ equal? first-picks (picks (probcc-random-selector src-copy)))
   (⊦ equal? (picks (probcc-random-selector (make-stdgen-source 3)))
      (picks (probcc-random-selector (make-stdgen-source 3))))
   (⊦ equal? (picks (probcc-random-selector (make-chicken-random-source (string->utf8 "seed"))))
      (picks (probcc-random-selector (make-chicken-random-source (string->utf8 "seed")))))
   ; the answer is the chosen thing and the total mass; an entry of mass 0 is never chosen.
   (⊦ equal? '(b 1.) ((probcc-random-selector 1) '((a 0.) (b 1.))))
   (⊦ equal? '((b 1.) (b 1.) (b 1.))
      (let1 (s (probcc-random-selector (make-stdgen-source 7)))
        (draws 3 (τ (s '((a 0.) (b 1.) (c 0.)))))))
   (⊦ equal? '(3 7/2) (let1 (s (probcc-random-selector 17)) (s '((x 1/2) (3 3)))))
   `(doc (p (code/inline "probcc-random-selector") " (cf. P:221-233) chooses an entry with the
	    probability of its mass and answers it with the total mass (deviation 18). Its seed
	    re-initialises OCaml's global " (code/inline "Random") " state, as upstream's "
            (code/inline "Random.init") ", so selectors made with the same seed share one
	    stream; an rng-source of " (code/inline "(aux hakaru10 random)")
            " (OCaml's, the StdGen of Haskell, CHICKEN's) may replace the seed.")))

  ((test/selector/raises _)
   (⊦raises (exn hansei) ((probcc-random-selector 1) '()))
   (⊦ equal? 'selector
      (handle-exceptions e ((condition-property-accessor 'hansei 'kind) e)
        ((probcc-random-selector 1) '())))
   `(doc (p "A selector given no entries raises " (code/inline "(exn hansei)") " of kind "
            (code/inline "selector") ", upstream's " (code/inline "Choice selection: can't
	    happen") " (P:224).")))

  ((test/replay/nested _)
   (define (testn12)
     (let1 (coin (probcc-distribution `((,(τ (probcc-coin 0.5)) 0.5) (,(τ #t) 0.5))))
       (probcc-sample-rejection/a (probcc-random-selector 17) 10 coin)))
   (define (testn22)
     (let1 (coin-choice (probcc-coin 0.5))
       (probcc-sample-rejection (probcc-random-selector 17) 10 (or (probcc-coin 0.5) coin-choice))))
   ; cf. N:114-120, 183-190: `let v = flip 0.5 in (v, coin_choice (), coin_choice ())`, v
   ; first, then the components of the tuples from the right (D19).
   (define (testnl13/14 inferencer)
     (τ (let* ((coin-choice (probcc-letlazy/nesting (τ (probcc-coin 0.5))))
               (v1 (inferencer (τ (let* ((v (probcc-coin 0.5))
                                         (c (coin-choice))
                                         (b (coin-choice)))
                                    (list v b c)))))
               (b (coin-choice))
               (a (coin-choice)))
          (list a b v1))))
   (define (tf2-nested n selector) (probcc-sample-rejection/a selector n tflip2-noshared))
   (define (dcoin-and-true n) (or (dcoin-and n) (probcc-impossible)))
   (define (nested-rejection n model) (probcc-sample-rejection/a probcc-dist-selector n model))
   (define (nested-importance n model) (probcc-sample-importance/a probcc-dist-selector n model))
   ; cf. N:388-394, the parameters of the red-light game: n = 5 intersections.
   (define police-monitoring '#(0.1 0.2 0.1 0.2 0.2))
   (define bad-luck 0.1)
   (define maxrisk 0.2)
   ; cf. N:398-401, defined upstream but not used by `game`.
   (define (noisy-or a astrength b bstrength baserate)
     (or (and (probcc-coin astrength) (a))
         (and (probcc-coin bstrength) (b))
         (probcc-coin baserate)))
   ; cf. N:404-407, the chance of being caught at intersection i at time-step t.
   (define ((being-caught i t))
     (or (and (probcc-coin 0.5) (probcc-coin (vector-ref police-monitoring i)))
         (and (probcc-coin 0.3) (if (< t 3) (probcc-coin 0.2) (probcc-coin 0.4)))
         (probcc-coin bad-luck)))
   ; cf. N:409-424: planning with risk, a cheat on red is too risky when a nested 10-sample
   ; estimate of being caught reaches maxrisk.
   (define ((game cheatingp))
     (let loop ((t 0) (pos 0))
       (cond
         ((>= pos 5) t)
         ((probcc-coin 0.5)
          (cond
            ((not (probcc-coin cheatingp)) (loop (add1 t) pos))
            ((probcc-at-least maxrisk #t (nested-importance 10 (being-caught pos t)))
             (probcc-impossible))
            (else (loop (add1 t) (add1 pos)))))
         (else (loop (add1 t) (add1 pos))))))
   (define coin-tables '(((V (((V #t) 1))) 0.5)
                         ((V (((V #t) 0.59999999999999998) ((V #f) 0.40000000000000002))) 0.5)))
   (⊦ equal? coin-tables (probcc-reify/exact/a testn12))
   (⊦ equal? coin-tables (probcc-sample-rejection/a (probcc-random-selector 17) 10 testn12))
   (⊦ equal? '(((V (((V #t) 1.))) 0.5)
               ((V (((V #t) 0.59999999999999998) ((V #f) 0.40000000000000002))) 0.5))
      (probcc-reify/exact/a testn22))
   (⊦ equal? '(((V (#t #t (((V (#t #t #t)) 0.5) ((V (#f #t #t)) 0.5)))) 0.59999999999999998)
               ((V (#f #f (((V (#t #f #f)) 0.5) ((V (#f #f #f)) 0.5)))) 0.40000000000000002))
      (probcc-sample-rejection/a (probcc-random-selector 17) 10 (testnl13/14 probcc-reify/exact/a)))
   (⊦ equal? '(((V #f) 0.76000000000000001) ((V #t) 0.23999999999999999))
      (tf2-nested 100 (probcc-random-selector 1)))
   (⊦ equal? '(((V (((V #f) 1.))) 0.76000000000000001) ((V (((V #t) 1.))) 0.23999999999999999))
      (probcc-sample-rejection (probcc-random-selector 1) 100 (tf2-nested 1 probcc-dist-selector)))
   (⊦ equal? '(((V (((V #f) 1.))) 0.41999999999999998)
               ((V (((V #f) 0.66666666666666663) ((V #t) 0.33333333333333331))) 0.40000000000000002)
               ((V (((V #t) 0.66666666666666663) ((V #f) 0.33333333333333331)))
                0.17999999999999999))
      (probcc-sample-rejection (probcc-random-selector 1) 100 (tf2-nested 3 probcc-dist-selector)))
   (⊦ equal? '(((V (#f #f (((V (#f #f #f)) 0.69999999999999996)
                           ((V (#t #f #f)) 0.29999999999999999))))
                0.29999999999999999)
               ((V (#t #t (((V (#t #t #t)) 0.5) ((V (#f #t #t)) 0.5)))) 0.20000000000000001)
               ((V (#t #t (((V (#t #t #t)) 0.69999999999999996)
                           ((V (#f #t #t)) 0.29999999999999999))))
                0.10000000000000001)
               ((V (#f #f (((V (#t #f #f)) 0.69999999999999996)
                           ((V (#f #f #f)) 0.29999999999999999))))
                0.10000000000000001)
               ((V (#f #f (((V (#t #f #f)) 0.5) ((V (#f #f #f)) 0.5)))) 0.10000000000000001)
               ((V (#f #f (((V (#f #f #f)) 0.80000000000000004)
                           ((V (#t #f #f)) 0.20000000000000001))))
                0.10000000000000001)
               ((V (#f #f (((V (#f #f #f)) 0.59999999999999998)
                           ((V (#t #f #f)) 0.40000000000000002))))
                0.10000000000000001))
      (probcc-sample-rejection/a (probcc-random-selector 17) 10
                                 (testnl13/14 (λ (m) (nested-rejection 10 m)))))
   (⊦ equal? '(((V ()) 0.9476980344) ((V (((V #f) 1.))) 0.052301965599999993))
      (probcc-sample-importance (probcc-random-selector 17) 100
        (nested-rejection 1 (τ (dcoin-and 10)))))
   ; 4.99999999999998855e-11 has 18 digits: CHICKEN's reader misrounds the shortest form
   ; 4.9999999999999885e-11 by one ulp (the trap of the WP4 note in the plan, §15).
   (⊦ equal? '(((V (((V #f) 0.054999999999999986))) 0.5) ((V (((V #f) 0.050499999999999989))) 0.25)
               ((V (((V #f) 0.05004999999999999))) 0.125)
               ((V (((V #f) 0.050004999999999987))) 0.0625)
               ((V (((V #f) 0.050000499999999989))) 0.03125)
               ((V (((V #f) 0.05000004999999999))) 0.015625)
               ((V (((V #f) 0.050000004999999986))) 0.0078125)
               ((V (((V #f) 0.050000000499999989))) 0.00390625)
               ((V (((V #f) 0.050000000049999986))) 0.001953125)
               ((V (((V #f) 0.049999999999999989) ((V #t) 4.99999999999998855e-11))) 0.001953125))
      (probcc-sample-importance (probcc-random-selector 17) 100
        (nested-importance 1 (τ (dcoin-and 10)))))
   (⊦ equal? '(((V ()) 0.99999999999999989))
      (probcc-sample-importance (probcc-random-selector 1) 100
        (nested-rejection 1 (τ (dcoin-and-true 10)))))
   (⊦ equal? '(((V ()) 0.998046875) ((V (((V #t) 4.99999999999998855e-11))) 0.001953125))
      (probcc-sample-importance (probcc-random-selector 1) 100
        (nested-importance 1 (τ (dcoin-and-true 10)))))
   (⊦ equal? '(((V ()) 0.88826171875000004) ((V (((V #t) 9.9999999999999776e-13))) 0.10154296875)
               ((V (((V #t) 1.9999999999999955e-12))) 0.01017578125)
               ((V (((V #t) 2.9999999999999933e-12))) 1.953125e-05))
      (probcc-sample-importance (probcc-random-selector 1) 100
        (nested-importance 50 (τ (dcoin-and-true 10)))))
   (⊦ equal? '(((V #t) 11/20) ((V #f) 9/20))
      (probcc-reify/exact (noisy-or (τ #t) 1/2 (τ #f) 1/2 1/10)))
   ; cf. N:432-438 and N:440-441, the red-light game.
   (⊦ equal? '(((V 9) 0.10625) ((V 6) 0.09999999999999995) ((V 13) 0.053515625) ((V 10) 0.053125)
               ((V 7) 0.049999999999999975) ((V 14) 0.0267578125) ((V 11) 0.0265625)
               ((V 8) 0.0249999999999999875) ((V 12) 0.01328125) ((V 15) 0.01259765625)
               ((V 16) 4.88281249999999485e-05))
      (probcc-sample-importance/a (probcc-random-selector 17) 10 (game 0.2)))
   (⊦ equal? '(((V 7) 0.0525) ((V 5) 0.04) ((V 6) 0.025) ((V 8) 0.01625) ((V 9) 0.010625))
      (probcc-sample-importance/a (probcc-random-selector 17) 50 (game 0.5)))
   `(doc (p "The records of " (code/inline "nested.ml") " (N:20-287, 346-441), replayed bit for bit:
	    a sampler inside a model that makes its own " (code/inline "random_selector 17")
            " re-seeds the global state, which the outer sampler then draws from (plan D6);
	    nested rejection and importance sampling with " (code/inline "probcc-dist-selector")
            " give the distribution of the inner estimates, with the inner failures as empty
	    tables. Exact masses of 1 are the exact 1 where upstream has 1. (deviation 26), and
	    the tables are in the order of " (code/inline "probcc-explore") " (deviation 5). The
	    red-light game (N:346-441) plans with risk: a cheat on a red light fails the world
	    when a nested 10-sample estimate of the chance of being caught reaches "
            (code/inline "maxrisk") ".")))

  )

; a resume that fails to rewind its `dynamic-wind` frames would leave a test's string port as
; the current output port, hiding the summary; check that the port is back.
(import (only (chicken base) exit))

(define stdout (current-output-port))

(let1 (r (unittest/✓ hansei-random))
  (unless (eq? stdout (current-output-port))
    (current-output-port stdout)
    (write (unittest/result-summary r))
    (newline)
    (display "FAIL: the suite ended with its output still captured by a test\n")
    (exit 1)))
