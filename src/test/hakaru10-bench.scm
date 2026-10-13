; The benchmark of Bench.hs (BE:1-38) and of the paper's §3.1 (design.txt l.617-640):
;   ./hakaru10-bench n nchain
; prints the moments (mean . variance) of the nchain samples of (phier n), a chain of n+1
; normal nodes whose last one has the distribution N(0, 1 + 9n), and the milliseconds taken.
; Compiled with csc -O3 by `make bench-hakaru10` (which runs ./hakaru10-bench 10 10000), not
; part of `make test`; it asserts nothing (WP9, plan §9.7).  Without arguments it runs n = 10,
; nchain = 10000.
;
; Reference values, n = 10 (BE:23-38, Haskell, upstream's StdGen seeded with 17):
;   nchain 1000     (3.4117556376053577,32.27027981263249)   0.03 s MUT
;   nchain 10000    (-3.151396208304257,54.17557680794696)   0.20 s MUT
;   nchain 100000   (-0.7051036715134076,84.35841806407852)  2.02 s MUT
;   nchain 1000000  (0.2246675771924041,93.50779949355687)   19.84 s MUT
; This port reproduces the record of 1000 exactly (the replay X13 of hakaru10.scm).  Beyond,
; it gives what the Python transcription of the November 2016 engine gives, bit for bit:
; (-3.151835923727823 . 54.168535367816084), (0.7178938906152538 . 97.97806557757613) and
; (-0.25726322883691294 . 97.59985845502847); upstream's longer records, close at 10000 and
; apart after, presumably come from another revision of the code.  Compiled, 10^6 samples take about
; 4.4 s here.
; The paper (P §3.1, l.623-640) reports for 10^6 samples: original Hakaru mean 0.39, variance
; 87, 72 s; Hakaru10 mean 0.22, variance 93, 20 s (Intel Core i3 1.8GHz, GHC 7.8.3 -O2).  The
; exact variance is 1 + 9 * 10 = 91 (P l.623-624).

(import scheme
        (only (scheme base) exact-integer?)
        (chicken base)
        (chicken process-context)
        (only (chicken time) cpu-time current-process-milliseconds)
        (aux base)
        (aux hakaru10))

; cf. BE:14-15 `phier n = (iterate (\m -> do {x<-m; dist normal x 3}) $ dist normal 0 1) !! n`
; (E:62-63 phier, P §2.1 l.301-327): x0 ~ N(0, 1), then n times x ~ N(x, 3).  The named let is
; `iterate ... !! n` over models: it nests n do/monad around x0's dist and creates no node;
; x0's node is created first, when the outermost model runs (plan §15, C35).
(define (phier n)
  (let loop ((i n) (m (dist normal 0 1)))
    (if (zero? i) m (loop (sub1 i) (do/monad (← x m) (dist normal x 3))))))

; (new) the values recorded for the arguments (n nchain), BE:23-38, or #f.
(define (recorded n nchain)
  (and (= n 10)
       (alist-ref nchain '((1000 3.4117556376053577 . 32.27027981263249)
                           (10000 -3.151396208304257 . 54.17557680794696)
                           (100000 -0.7051036715134076 . 84.35841806407852)
                           (1000000 .2246675771924041 . 93.50779949355687)))))

; cf. BE:17-21 `main`: [n', nchain'] <- getArgs; print . momenta $ mcmC nchain (phier n), with
; the CPU and wall-clock milliseconds of the run (BE:4-5 runs it under GHCRTS="-tstderr").
(define (main args)
  (match/first (map string->number args)
    (((,n ,nchain) ⊣ (and (exact-integer? n) (exact-integer? nchain) (>= n 0) (> nchain 0)))
     (receive (user0 system0) (cpu-time)
       (let* ((wall0 (current-process-milliseconds))
              (m (momenta (mcmC nchain (phier n))))
              (wall (- (current-process-milliseconds) wall0)))
         (receive (user system) (cpu-time)
           (write `(hakaru10-bench (n ,n) (nchain ,nchain) (momenta ,m)
                                   (cpu-ms ,(- (+ user system) user0 system0)) (wall-ms ,wall)
                                   (recorded ,(recorded n nchain))))
           (newline)))))
    (() (main '("10" "10000")))
    (else (display "usage: hakaru10-bench n nchain" (current-error-port))
          (newline (current-error-port))
          (exit 2))))

(main (command-line-arguments))
