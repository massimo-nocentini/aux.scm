;; (aux hakaru10 util) -- utilities to summarize and plot the samples of (aux hakaru10), a
;; CHICKEN port of Oleg Kiselyov's Hakaru10 (https://okmij.org/ftp/kakuritu/Hakaru10/index.html).
;;
;; Mirrors (paths relative to aux.scm/upstream/okmij-hakaru10/, citation keys as in the docs):
;;   U  Util.hs (https://okmij.org/ftp/kakuritu/Hakaru10/Util.hs): hist (U:11-12), momenta
;;      (U:16-22), tabulate' (U:28-37), tabulate (U:40-45), kl (U:51-56), plot (U:58-65).
;;
;; Hakaru10 code version: November 2016 (W:52-53); web page last updated July 14, 2018 (W:179).
;; Paper: O. Kiselyov, "Probabilistic Programming Language and its Incremental Evaluation",
;; APLAS 2016, LNCS 10017, DOI 10.1007/978-3-319-47958-3_19 (P = design.txt / design.pdf).
;;
;; Contents: generic<?, hist, frequency, momenta, tabulate, tabulate/bins, kl, plot.
;;
;; A histogram is an alist ((key . mass) ...) sorted ascending by generic<?, standing for
;; Data.Map's `M.Map k v` (U:25), whose `M.toList` is ascending too; it is accumulated in a
;; srfi-69 table and sorted once at the end.  The floating-point folds go in the direction of
;; the Haskell ones (foldl for `sum` and for M.foldlWithKey', foldr for the variance and for the
;; insertions), so that the values recorded upstream and by the replay prototype come out bit
;; for bit (plan §6.12).  kl raises the (exn hakaru10) condition of hakaru10-error (plan C16).

(module (aux hakaru10 util) *

  (import scheme
          (chicken base)
          (chicken sort)
          (chicken flonum)
          (only (chicken string) ->string)
          (only srfi-1 count)
          srfi-69
          (aux base)
          (only (aux hakaru10 distribution) hakaru10-error))

  ;; ---------------------------------------------------------------------------------------
  ;; Ordering and histograms
  ;; ---------------------------------------------------------------------------------------

  ; cf. U:11 (the `Ord t` constraint of hist) and U:25, U:52 (the key order of Data.Map).
  ; Haskell's Ord is resolved by the type; here one total preorder over Scheme values stands for
  ; it, plan §3.4 and §7 (`Eq a`, `Show a`).  Each entry pairs a type predicate with the order
  ; inside that type, and the types rank in the order of the list: boolean < number < char <
  ; string < symbol < list < vector < anything else (all equivalent).  Within a type: #f < #t;
  ; real numbers by <, an exact number before an equal inexact one, non-real ones by their
  ; (real imaginary) parts; characters by char<?; strings by string<?; symbols by name; lists
  ; lexicographically, a proper prefix first (the cdr of an improper list is compared as a value);
  ; vectors as the lists of their elements.
  (define generic-orders
    (list (cons boolean? (λ (a b) (and (not a) b)))
          (cons number? (λ (a b)
                          (cond
                            ((not (and (real? a) (real? b)))
                             (generic<? (list (real-part a) (imag-part a))
                                        (list (real-part b) (imag-part b))))
                            ((= a b) (and (exact? a) (inexact? b)))
                            (else (< a b)))))
          (cons char? char<?)
          (cons string? string<?)
          (cons symbol? (λ (a b) (string<? (symbol->string a) (symbol->string b))))
          (cons (λ (v) (or (null? v) (pair? v)))
                (λ (a b)
                  (cond
                    ((null? a) (pair? b))
                    ((null? b) #f)
                    ((generic<? (car a) (car b)) #t)
                    ((generic<? (car b) (car a)) #f)
                    (else (generic<? (cdr a) (cdr b))))))
          (cons vector? (λ (a b) (generic<? (vector->list a) (vector->list b))))))

  ; cf. U:11 (Ord t), see generic-orders: the rank of v's type and the order inside it.
  (define (generic-order v)
    (let loop ((rank 0) (orders generic-orders))
      (cond
        ((null? orders) (values rank (λ (a b) #f)))
        (((caar orders) v) (values rank (cdar orders)))
        (else (loop (add1 rank) (cdr orders))))))

  ; cf. U:11 (Ord t), see generic-orders.
  (define (generic<? a b)
    (let-values (((ra less?) (generic-order a))
                 ((rb _) (generic-order b)))
      (or (< ra rb)
          (and (= ra rb) (less? a b)))))

  ; cf. U:25 (M.toList is ascending): the pairs of an alist sorted by key, with generic<?.
  (define (histogram-sort alist)
    (sort alist (λ (p q) (generic<? (car p) (car q)))))

  ; cf. U:25: a histogram from the table that accumulated it.
  (define (hash-table->histogram table)
    (histogram-sort (hash-table->alist table)))

  ; cf. U:11-12 hist = foldr (\e -> M.insertWith' (+) e 1) M.empty.
  ; Counts the occurrences of each value, keys compared with equal?, and returns the histogram
  ; ((v . count) ...).  The fold direction does not matter for exact counts.
  (define (hist xs)
    (let1 (table (make-hash-table equal?))
      (for-each (λ (x) (hash-table-update!/default table x add1 0)) xs)
      (hash-table->histogram table)))

  ; (new; plan §3.4) the fraction of xs that satisfy pred, as a flonum: the shape of the
  ; upstream records "419 False / 581 True" (E:237-238) read as a probability.  An empty xs
  ; gives +nan.0, as 0/0 does for momenta.
  (define (frequency pred xs)
    (fp/ (exact->inexact (count pred xs)) (exact->inexact (length xs))))

  ;; ---------------------------------------------------------------------------------------
  ;; Moments and tabulation of Double samples
  ;; ---------------------------------------------------------------------------------------

  ; cf. U:16-22 momenta: (av, sigma2) where n = fromIntegral (length lst), av = sum lst / n
  ; and sigma2 = foldr (\x acc -> sqr (x - av) + acc) 0 lst / (n - 1).
  ; The list `sum` is a left fold from 0, the variance a right fold, as written.  Returns the
  ; pair (mean . unbiased-variance), plan C24.  Exact numbers are taken as flonums.
  (define (momenta xs)
    (let* ((xs (map exact->inexact xs))
           (n (exact->inexact (length xs)))
           (av (fp/ (foldl fp+ 0. xs) n))
           (sqr (λ (x) (fp* x x))))
      (cons av (fp/ (foldr (λ (x acc) (fp+ (sqr (fp- x av)) acc)) 0. xs)
                    (fp- n 1.)))))

  ; cf. U:37, U:45 to_bin x = fromInteger (floor (x / step)) * step.
  ; Haskell's floor goes through an Integer, so a bin is never -0.0 (for a positive step): the
  ; flonum floor is mapped back from -0.0 to 0.0 to agree with it (Data.Map would merge the two
  ; keys anyway, since compare 0.0 (-0.0) is EQ, while equal? tells them apart).
  (define (to-bin step x)
    (let1 (b (fpfloor (fp/ x step)))
      (fp* (if (fp= b 0.) 0. b) step)))

  ; cf. U:40-45 tabulate step lst = foldr (\x -> M.insertWith' (+) (to_bin x) quantum) M.empty lst
  ; with quantum = 1 / fromIntegral (length lst).  insertWith' f k new stores (f new old), that
  ; is quantum + old, and the elements are inserted from the right end of the list: the first
  ; insertion of a bin stores quantum + 0., the same flonum as quantum.  Returns the histogram
  ; ((bin . mass) ...).
  (define (tabulate step xs)
    (let* ((step (exact->inexact step))
           (quantum (fp/ 1. (exact->inexact (length xs))))
           (insert! (λ (x table)
                      (hash-table-update!/default table (to-bin step (exact->inexact x))
                                                  (λ (old) (fp+ quantum old)) 0.)
                      table)))
      (hash-table->histogram (foldr insert! (make-hash-table equal?) xs))))

  ; cf. U:28-37 tabulate' nbins lst, where nbins is only a hint: step_raw is 1 when
  ; minimum lst == maximum lst, else (mx - mn) / fromIntegral nbins, and the bin size is
  ; step = 10.0 ** (fromInteger . round $ log step_raw / log 10); the fold is tabulate's.
  ; Haskell's round rounds half to even, as Scheme's round does (not fpround, which rounds half
  ; away from zero); ** is pow, that is fpexpt.
  (define (tabulate/bins nbins xs)
    (let* ((ys (map exact->inexact xs))
           (mn (foldl fpmin (car ys) (cdr ys)))
           (mx (foldl fpmax (car ys) (cdr ys)))
           (step-raw (if (fp= mn mx) 1. (fp/ (fp- mx mn) (exact->inexact nbins)))))
      (tabulate (fpexpt 10. (round (fp/ (fplog step-raw) (fplog 10.)))) ys)))

  ;; ---------------------------------------------------------------------------------------
  ;; Comparing and plotting histograms
  ;; ---------------------------------------------------------------------------------------

  ; cf. U:51-56 kl p q = M.foldlWithKey' f 0 p, f sum i pi = sum + pi * log (pi / qi), where a
  ; key i of p missing from q is an error (q is 0 where p is not).  KL(p|q), p being the
  ; `theoretical' distribution (U:47-50), folded in ascending key order.  The bins of q need not
  ; be sorted; of a repeated key the first binding counts (alist->hash-table keeps it), as assoc
  ; would.  The error is the (exn hakaru10) condition of kind support (plan C16) with upstream's
  ; message (U:55-56); keys are compared with equal? (tabulate never makes a -0.0 bin).
  (define (kl p q)
    (let1 (q (alist->hash-table q equal?))
      (foldl (Λ (,sum (,i . ,pi))
               (let1 (qi (hash-table-ref q i (τ (hakaru10-error 'support 'kl
                                                   (string-append
                                                     "Undefined KL divergence - q is 0 when p is not at point "
                                                     (->string i))
                                                   i))))
                 (+ sum (* pi (log (/ pi qi))))))
             0.
             (histogram-sort p))))

  ; cf. U:58-65 plot dat: writes the file fname = "/tmp/plot.dat" (here the optional path) with
  ; the header line "x  y" and then one line "k v" per bin, ascending (M.toList).  Numbers are
  ; printed by display rather than by Haskell's show (1e-05 rather than 1.0e-5); gnuplot reads
  ; both.  Returns the path.
  (define (plot h #!optional (path "/tmp/plot.dat"))
    (with-output-to-file path
      (τ (print "x  y")
         (for-each (Λ ((,k . ,v)) (print k " " v)) (histogram-sort h))))
    path)

  )
