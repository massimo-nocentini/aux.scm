; The suite for (aux hakaru10 util): histograms, moments, tabulation, KL divergence and plot
; files, after Util.hs.  Cases U1-U6 of the plan (WP3); every expected value is exact (equal?),
; computed by hand or by an IEEE-754 transcription of the Haskell folds.

(import scheme
        (chicken base)
        (chicken io)
        (chicken file)
        (chicken condition)
        (aux base)
        (aux unittest)
        (aux hakaru10 util))

(define-suite hakaru10-util

  ((doc r) `((structure/section "Utilities")
             (p "Helpers to summarize the samples of a chain, after "
                (cite/a "https://okmij.org/ftp/kakuritu/Hakaru10/Util.hs" "Util.hs")
                " of "
                (cite/a "https://okmij.org/ftp/kakuritu/Hakaru10/index.html"
                        "Embedded probabilistic domain-specific language Hakaru10")
                ", November 2016.  A chain of " (code/inline "mcmC") " is a list of samples;
                these procedures reduce it to a histogram, to its first two moments, or to the
                Kullback-Leibler divergence from another histogram, which is how the paper
                compares a model with its exact distribution (P §2.1 p.6-7, l.282-327).")
             (p "A histogram is an alist " (code/inline "((key . mass) ...)") " sorted
                ascending, standing for Haskell's " (code/inline "type Histogram = M.Map Double Double")
                " (U:25), whose " (code/inline "M.toList") " is ascending too.  Where Haskell
                orders keys by the " (code/inline "Ord") " class, the port uses one total
                order over Scheme values, " (code/inline "generic<?") ": booleans, then numbers,
                characters, strings, symbols, lists and vectors, each ordered in the obvious way
                (" (code/inline "#f") " before " (code/inline "#t") ", lists lexicographically,
                vectors as the lists of their elements).")
             (p "The floating-point folds are those of the Haskell code, in the same direction,
                so that the KL divergences and moments recorded upstream are reproduced bit for
                bit: " (code/inline "sum") " is a left fold, the variance and the insertions of "
                (code/inline "tabulate") " are right folds, " (code/inline "kl") " folds the keys
                of " (m "p") " in ascending order.  The moments are")
             (displaymath (m "x̄ = (1/n) Σᵢ₌₁ⁿ xᵢ,   σ² = (1/(n − 1)) Σᵢ₌₁ⁿ (xᵢ − x̄)²"))
             (p "and the divergence of " (m "q") " from the " (i "theoretical") " " (m "p")
                " (U:47-50) is")
             (displaymath (m "KL(p ‖ q) = Σᵢ pᵢ log (pᵢ / qᵢ)"))
             (p "which is undefined, and raises an " (code/inline "(exn hakaru10)") " condition of
                kind " (code/inline "support") ", where " (m "qᵢ = 0 < pᵢ") ".")
             (code/scheme/file "../aux.hakaru10.util.scm")))

  ((test/hist _)
   (⊦ equal? '((#f . 1) (#t . 2) (1 . 1) (2 . 2)) (hist '(#t #f #t 2 1 2)))
   (⊦ equal? '() (hist '()))
   (⊦ equal? '((#\a . 1) ("a" . 2) (a . 1) (b . 1) (() . 1) ((1) . 1) ((1 2) . 1) ((2) . 1) (#(1) . 1))
     (hist '(b (2) #(1) "a" (1 2) a () "a" (1) #\a)))
   `(doc (p "U1.  " (code/inline "hist") " counts the occurrences of each value (with "
            (code/inline "equal?") ") and sorts them with " (code/inline "generic<?") ":")
         (code/lang "haskell" "-- Very simple histogram, for a discrete distribution
hist :: Ord t => [t] -> M.Map t Int
hist = foldr (\\e -> M.insertWith' (+) e 1) M.empty")
         (p "cf. U:11-12; the second check exercises the order across types.")))

  ((test/generic<? _)
   (⊨ (generic<? #f #t))
   (⊭ (generic<? #t #f))
   (⊨ (generic<? #t -1))
   (⊨ (generic<? 1 1.5))
   (⊨ (generic<? 1 1.))
   (⊭ (generic<? 1. 1))
   (⊭ (generic<? 2 2))
   (⊨ (generic<? 'z '()))
   (⊨ (generic<? '(1 2) '(1 2 0)))
   (⊨ (generic<? '(1 2) '(1 3)))
   (⊨ (generic<? '(9) #(0)))
   (⊨ (generic<? #(1 2) #(1 2 0)))
   (⊭ (generic<? #(1 3) #(1 2 0)))
   `(doc (p "The order standing for Haskell's " (code/inline "Ord") " (U:11): an exact number
            precedes an equal inexact one, a proper prefix precedes the longer list or vector.")))

  ((test/frequency _)
   (⊦ equal? .75 (frequency even? '(2 4 5 6)))
   (⊦ equal? 0. (frequency symbol? '(1 2)))
   `(doc (p (code/inline "frequency") " is the fraction of the samples satisfying a predicate, as
            a flonum: the probability of an event estimated from a chain.")))

  ((test/momenta _)
   (⊦ equal? '(2.5 . 1.6666666666666667) (momenta '(1. 2. 3. 4.)))
   (⊦ equal? '(2.5 . 1.6666666666666667) (momenta '(1 2 3 4)))
   ; the variance folded from the left would give 0.0728702: the right fold is pinned.
   (⊦ equal? '(0.2602 . 0.07287019999999998) (momenta '(.1 .2 .3 .7 .001)))
   `(doc (p "U2.  " (code/inline "momenta") " returns the pair " (code/inline "(mean . variance)")
            " with the unbiased variance, dividing by " (m "n − 1") ":")
         (code/lang "haskell" "-- compute the first two momenta
momenta :: [Double] -> (Double,Double)
momenta lst = (av,sigma2)
  where
    n   = fromIntegral $ length lst
    av  = sum lst / n
    sigma2 = (foldr (\\x acc -> sqr (x - av) + acc) 0 lst) / (n - 1)
    sqr x = x * x")
         (p "cf. U:16-22; exact numbers are taken as flonums.  The last check pins the
            direction of the fold of the squares: folded from the left they would sum to "
            (code/inline 0.0728702) " rather than " (code/inline 0.07287019999999998) ".")))

  ((test/tabulate _)
   (⊦ equal? '((-0.5 . .25) (0. . .5) (0.5 . .25)) (tabulate .5 '(0.1 0.2 0.7 -0.1)))
   (⊦ equal? '((0. . .5) (1. . .5)) (tabulate 1 '(-0. 0. 1.5 1.)))
   `(doc (p "U3.  " (code/inline "tabulate") " puts each sample in the bin "
            (m "⌊x / step⌋ · step") " with mass " (m "1/n") ":")
         (code/lang "haskell" "-- Tabulate with the given bin size
tabulate :: Double -> [Double] -> Histogram
tabulate step lst =
  foldr (\\x -> M.insertWith' (+) (to_bin x) quantum) M.empty lst
 where
   quantum = 1 / fromIntegral (length lst)
   to_bin x = fromInteger (floor (x / step)) * step")
         (p "cf. U:40-45.  Since " (code/inline "floor") " goes through an " (code/inline "Integer")
            ", the bin of " (code/inline "-0.") " is " (code/inline "0.") ", as the second check
            shows.")))

  ((test/tabulate/bins _)
   (⊦ equal? '((0. . 0.09090909090909091) (0.1 . 0.09090909090909091) (0.2 . 0.18181818181818182)
               (0.4 . 0.09090909090909091) (0.5 . 0.18181818181818182)
               (0.6000000000000001 . 0.09090909090909091) (0.8 . 0.09090909090909091)
               (0.9 . 0.09090909090909091) (1. . 0.09090909090909091))
     (tabulate/bins 10 '(0. .1 .2 .3 .4 .5 .6 .7 .8 .9 1.)))
   (⊦ equal? '((0. . 0.9090909090909093) (10. . 0.09090909090909091))
     (tabulate/bins 3 '(0. 1. 2. 3. 4. 5. 6. 7. 8. 9. 10.)))
   (⊦ equal? '((0. . 0.3333333333333333) (200. . 0.3333333333333333) (1000. . 0.3333333333333333))
     (tabulate/bins 4 '(0. 250. 1000.)))
   (⊦ equal? '((3. . 1.)) (tabulate/bins 5 '(3. 3.)))
   ; log 316.22776601683785 / log 10 is 2.5 exactly: round to even gives the step 100, where
   ; rounding half away from zero (fpround) would give 1000 and the single bin 0.
   (⊦ equal? '((0. . .5) (300. . .5)) (tabulate/bins 1 '(0. 316.22776601683785)))
   `(doc (p "U4.  " (code/inline "tabulate/bins") ", that is " (code/inline "tabulate'") ", takes
            only a hint of the number of bins: the bin size is rounded to a power of ten.")
         (code/lang "haskell" "-- The argument is the _hint_ to the number of bins
tabulate' :: Int -> [Double] -> Histogram
tabulate' nbins lst =
  foldr (\\x -> M.insertWith' (+) (to_bin x) quantum) M.empty lst
 where
   mn = minimum lst
   mx = maximum lst
   quantum = 1 / fromIntegral (length lst)
   step_raw = if mn == mx then 1 else (mx - mn) / fromIntegral nbins
   step  = 10.0 ** (fromInteger . round $ log step_raw / log 10)
   to_bin x = fromInteger (floor (x / step)) * step")
         (p "cf. U:28-37.  The first check has step " (code/inline 0.1) " (the ratio of logarithms
            is " (code/inline -0.9999999999999998) "), and the floating-point bins show it: "
            (code/inline ".3") " falls in the bin " (code/inline ".2") ".  The last check pins
            Haskell's " (code/inline "round") ", which rounds half to even.")))

  ((test/kl _)
   (define p (tabulate .5 '(0.1 0.2 0.7 -0.1)))
   (⊦ equal? 0. (kl p p))
   (⊦ equal? 0.14384103622589042 (kl '((0. . .5) (1. . .5)) '((1. . .75) (0. . .25) (2. . 0.))))
   (⊦ equal? 0.6931471805599453 (kl '((0. . 1.)) '((0. . .5) (0. . .25))))
   (⊦raises (exn hakaru10) (kl p '((0. . .5) (0.5 . .5))))
   (⊦ equal? '(support . "Undefined KL divergence - q is 0 when p is not at point -0.5")
     (condition-case (kl p '((0. . .5) (0.5 . .5)))
       (c (hakaru10) (cons (get-condition-property c 'hakaru10 'kind)
                           (get-condition-property c 'exn 'message)))))
   `(doc (p "U5.  " (code/inline "kl") " is the Kullback-Leibler divergence " (m "KL(p ‖ q)")
            "; the bins of " (m "q") " need not be sorted, those absent from " (m "p")
            " do not count, and of a repeated bin of " (m "q") " the first one counts, as with "
            (code/inline "assoc") ":")
         (code/lang "haskell" "kl :: Histogram -> Histogram -> Double
kl p q = M.foldlWithKey' f 0 p
 where f sum i pi | Just qi <- M.lookup i q = sum + pi * log (pi / qi)
                  | otherwise =
         error $ \"Undefined KL divergence - q is 0 when p is not at point \" ++
                 show i")
         (p "cf. U:51-56; the expected value " (code/inline 0.14384103622589042) " is "
            (m "½ log 2 + ½ log ⅔") " summed in this order.")))

  ((test/plot _)
   (define path (create-temporary-file "dat"))
   (⊦ equal? path (plot '((0.5 . .25) (-0.5 . .25) (0. . .5)) path))
   (⊦ equal? '("x  y" "-0.5 0.25" "0.0 0.5" "0.5 0.25") (with-input-from-file path read-lines))
   (delete-file path)
   `(doc (p "U6.  " (code/inline "plot") " writes a histogram as a two-column file for gnuplot,
            by default " (code/inline "/tmp/plot.dat") ", sorting it first:")
         (code/lang "haskell" "plot :: Histogram -> IO ()
plot dat = do
  withFile fname WriteMode write_data
 where
   fname = \"/tmp/plot.dat\"
   write_data h = do
     hPutStrLn h \"x  y\"
     mapM_ (\\ (k,v) -> hPutStrLn h $ unwords [show k,show v]) $ M.toList dat")
         (p "cf. U:58-65; upstream plots the histogram of a mixture's components at
            E:166-167:")
         (code/lang "haskell" "_ =
 let n = 10000 in plot (tabulate 0.2 $ mcmC (n `div` 2) (dist normal 10 2) ++ mcmC (n `div` 2) (dist gamma 3 (1/3)))")))

  )

(unittest/✓ hakaru10-util)
