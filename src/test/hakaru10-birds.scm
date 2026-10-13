; The bird migration model of Birds1.hs on the real data of the PPAML challenge problem
; (Birds1.hs:116-227; BirdMigrationProblem.txt, data set 1, "1-bird"): it runs only when
; HAKARU10_BIRDS_DIR names a folder with onebird-observations.csv and onebird_features.csv, the
; files that Birds1.hs reads and that upstream does not distribute (nor its ReadCSV module,
; B1:11).  Run by `make test-hakaru10-long`, not part of `make test` (WP9, plan §9.7).
; HAKARU10_BIRDS_N (default 10000) and HAKARU10_BIRDS_BURN (default 1000) are the arguments n
; and burn of `main1 burn n` (B1:202-215); HAKARU10_BIRDS_PIN=0 runs Birds1.hs:68 as upstream
; does, drawing the bird's cell on every day instead of observing it where it was seen.
; The features are rounded to single precision when loaded, as upstream's `Float` arrays hold
; them (B1:47, B1:168, B1:178) before `phi` widens them back with `realToFrac` (B1:85); since
; string->number reads a double first, a decimal within 2^-53 of a single-precision midpoint may
; round differently from a direct `read :: Float` (double rounding; ReadCSV is absent, B1:11).
; The means are reported, never compared with a ground truth: upstream's run gives
; [4.3697, 6.5965, 5.7809, 6.5930] (B1:219-227), and the numbers of BirdMigrationProblem.txt
; l.191-195 are only an example of the output format.
; The model is the one of hakaru10-examples.scm (cases C2-C4), repeated here so that each test
; script stands alone.

(import scheme
        (only (scheme base) exact-integer?)
        (chicken base)
        (only (chicken io) read-lines)
        (only (chicken string) string-split)
        (chicken process-context)
        (only srfi-4 f32vector f32vector-ref)
        (except (chicken condition) condition)
        srfi-1
        (aux base)
        (aux unittest)
        (aux hakaru10))

;; -------------------------------------------------------------------------------------------
;; Helpers
;; -------------------------------------------------------------------------------------------

; the sources of the definitions made with define/shown, newest first, for the docs.
(define sources '())

; (new) (define/shown name definition): make the definition, and keep its source under name.
(define-syntax-rule (define/shown name definition)
  (begin definition (push! (cons 'name 'definition) sources)))

; (new) the sources of the definitions of the names, in order, one code/scheme element each
; (spliced with ,@ into a doc).
(define (shown . names) (map (λ (name) `(code/scheme ,(alist-ref name sources))) names))

; (new) the number in the environment variable `name`, else `default`.
(define (environment-number name default)
  (or (and-let* ((s (get-environment-variable name))) (string->number s)) default))

;; -------------------------------------------------------------------------------------------
;; The model (Birds1.hs:28-114), as in hakaru10-examples.scm
;; -------------------------------------------------------------------------------------------

; cf. B1:35-40 `gridDim = 4`, `nCells = gridDim * gridDim`, `nFeatures = 4`.
(define grid-dim 4)
(define n-cells (* grid-dim grid-dim))
(define n-features 4)

; cf. B1:90-91 `divMod (n-1) gridDim` and BMP:103-122: column-major cells, (column . row).
(define (cell->xy n) (let1 (m (sub1 n)) (cons (quotient m grid-dim) (remainder m grid-dim))))

; cf. B1:90-92, the local `dist n m` of neighbors: the squared distance of two cells.
(define squared-distance
  (Λ ((,ix . ,iy) (,jx . ,jy)) (+ (* (- ix jx) (- ix jx)) (* (- iy jy) (- iy jy)))))

; cf. B1:87-93 `neighbors`, `d_max = 18` (BMP:58-59, d_max = 3√2): all 16 cells on 4x4.
(define (neighbors n)
  (let1 (from (cell->xy n))
    (filter (λ (m) (<= (squared-distance from (cell->xy m)) 18)) (iota n-cells 1))))

; cf. B1:82-85 `phi` and B1:95-99 `theta = exp . phi` (BMP:60-79).
(define (theta fs bs) (exp (fold + 0. (map * fs bs))))

; cf. B1:59 `trans n bs = [ (j,theta year day n j features bs) | j<-neighbors n]`.
(define ((transitions features year day) n bs)
  (map (λ (j) (cons j (theta (features year day n j) bs))) (neighbors n)))

; cf. B1:76-80 `birdpos`: the one cell that counts birds that day, else #f.
(define (birdpos obs year day)
  (match/first (filter (λ (i) (positive? (obs year day i))) (iota n-cells 1))
    ((,i) i)
    (else #f)))

; cf. B1:50-74 `model` (W:135-153); with `pin`, the alternative B1:62-67 observes the bird's
; cell where birdpos finds it (see hakaru10-examples.scm, C4).
(define/shown birds
  (define-model (birds features obs #!key (years 30) (days 20) (pin #t))
    (let1 (bs (collect (map-in-order (λ (_) (let1 (b (dist normal 0. 10.)) (lift abs b)))
                                     (iota n-features))))
      (for-each
        (λ (year)
          (fold (λ (day n)
                  (let* ((day′ (add1 day))
                         (trans (lift (transitions features year day) n bs))
                         (nreal (and pin (birdpos obs year day′)))
                         (n′ (dist (if nreal (condition nreal categorical) categorical) trans)))
                    (for-each (λ (i)
                                (dist (condition (obs year day′ i) poisson)
                                      (lift (λ (nv) (if (= i nv) 1 0)) n′)))
                              (iota n-cells 1))
                    n′))
                1
                (iota (sub1 days) 1)))
        (iota years 1))
      bs)))

;; -------------------------------------------------------------------------------------------
;; Loading the data (Birds1.hs:116-197)
;; -------------------------------------------------------------------------------------------

; (new) B1:44-47's `UArray`s: a table of the given extents, indexed from 1, as nested vectors;
; `fill` is shared by the cells, which are replaced, never mutated.
(define/shown make-table
  (define (make-table extents fill)
    (if (null? extents)
        fill
        (list->vector (map-in-order (λ (_) (make-table (cdr extents) fill)) (iota (car extents)))))))

; (new) the entry of the table at the indices.
(define/shown table-ref
  (define (table-ref table . indices) (fold (λ (i t) (vector-ref t (sub1 i))) table indices)))

; (new) replace the entry of the table at the indices by x.
(define/shown table-set!
  (define (table-set! table indices x)
    (vector-set! (apply table-ref table (drop-right indices 1)) (sub1 (last indices)) x)))

; cf. B1:144 and B1:176 (`hGetLine h -- skip the header line`) and B1:148-150, 184 (`readCSV`,
; the missing ReadCSV module): the rows of a CSV file after its header, as lists of numbers.
(define/shown read-csv
  (define (read-csv path)
    (filter-map (λ (line)
                  (let1 (fields (string-split line ", \t\r"))
                    (and (pair? fields) (map string->number fields))))
                (cdr (call-with-input-file path read-lines)))))

; cf. B1:141-155 `read_obs` and BMP:124-140: rows (year day count1 ... count16); answers the
; procedure (obs year day cell), the years and the days (B1:29-30: 30 and 20).
(define/shown read-observations
  (define (read-observations path)
    (let* ((rows (read-csv path))
           (years (fold max 0 (map first rows)))
           (days (fold max 0 (map second rows)))
           (table (make-table (list years days n-cells) 0)))
      (for-each (Λ ((,year ,day . ,counts))
                  (for-each (λ (cell count) (table-set! table (list year day cell) count))
                            (iota n-cells 1) counts))
                rows)
      (values (λ (year day cell) (table-ref table year day cell)) years days))))

; cf. B1:47, B1:168 and B1:178 (`Float`) and B1:85 (`realToFrac $ features ! ...`): the number x
; rounded to single precision, through a one-element f32vector, and widened back to a double.
(define/shown single
  (define (single x) (f32vector-ref (f32vector (exact->inexact x)) 0)))

; cf. B1:170-189 `read_features` and BMP:142-177: rows (year day from to f1 f2 f3 f4), days
; 1..days-1, entries absent from the file being 0 (B1:177 `newArray bounds 0`); answers the
; procedure (features year day from to) => (f1 f2 f3 f4), each one `single`.
(define/shown read-features
  (define (read-features path years days)
    (let1 (table (make-table (list years (sub1 days) n-cells n-cells) '(0. 0. 0. 0.)))
      (for-each (Λ ((,year ,day ,from ,to . ,fs))
                  (table-set! table (list year day from to) (map single fs)))
                (read-csv path))
      (λ (year day from to) (table-ref table year day from to)))))

; (new) the data set of HAKARU10_BIRDS_DIR, loaded when first forced: the alist ((obs . proc)
; (features . proc) (years . n) (days . n)).
(define data
  (δ (let1 (dir (get-environment-variable "HAKARU10_BIRDS_DIR"))
       (receive (obs years days) (read-observations (string-append dir "/onebird-observations.csv"))
         `((obs . ,obs)
           (features . ,(read-features (string-append dir "/onebird_features.csv") years days))
           (years . ,years)
           (days . ,days))))))

; (new) the entry `key` of the data set.
(define (data-ref key) (alist-ref key (force data)))

;; -------------------------------------------------------------------------------------------
;; Main program (Birds1.hs:199-215)
;; -------------------------------------------------------------------------------------------

; cf. B1:202-209 `main1 burn n`: the means of b1..b4 over the samples of a chain of n, the
; first burn dropped (`drop burn $ mcmC n`), by the fold of B1:208-209, in constant memory;
; '() if no sample is kept, as when no initial trace is found in n builds (M:868, M:882-891).
(define/shown main1
  (define (main1 burn n #!key (pin #t))
    (let* ((model (birds (data-ref 'features) (data-ref 'obs)
                         #:years (data-ref 'years) #:days (data-ref 'days) #:pin pin))
           (acc (mcmC-fold (Λ (,bs (,i . ,sums)) (cons (add1 i) (if (< i burn) sums (map + bs sums))))
                           (cons 0 (make-list n-features 0.))
                           n
                           model))
           (kept (- (car acc) burn)))
      (if (positive? kept) (map (cut / <> kept) (cdr acc)) '()))))

(define-suite hakaru10-birds-real

  ((doc r) `((structure/section "Bird migration, real data")
             (p "The "
                (cite/a "https://okmij.org/ftp/kakuritu/Hakaru10/index.html#bird-migration"
                        "bird migration")
                " model of "
                (cite/a "https://okmij.org/ftp/kakuritu/Hakaru10/Birds1.hs" "Birds1.hs")
                " on the 1-bird data set of the "
                (cite/a "https://okmij.org/ftp/kakuritu/Hakaru10/BirdMigrationProblem.pdf"
                        "PPAML challenge problem: bird migration")
                " (Tom Dietterich and Shahed Sorower, version 6, August 30, 2014; l.103-105):"
                (cite/quote
                  "Dietterich and Sorower"
                  "The data consist of observations of a single bird traversing a 4x4 grid
                  (Figure 1) over a period of 30 years. Each year, the bird starts in the lower
                  left cell and attempts to migrate to the upper right cell. In effect, we are
                  observing 30 state sequences, so this is a fully-observed IOHMM."))
             (p "The task (l.179-200) is to estimate the four parameters of the log-linear
                transition model, "
                (m "P(j | i) ∝ exp[β₁ f₁(i, j) + β₂ f₂(i, j) + β₃ f₃(i, j) + β₄ f₄(i, j)]")
                ", scored by the squared error.  Upstream reads two CSV files with a ReadCSV
                module that it does not distribute, nor the files: this suite runs when "
                (code/inline "HAKARU10_BIRDS_DIR") " names the folder that holds them, "
                (code/inline "onebird-observations.csv") " (year, day, the counts of cells 1 to
                16, l.124-140) and " (code/inline "onebird_features.csv")
                " (year, day, from, to, f1 to f4, days 1 to 19, l.142-177), each with a header
                line.  " (code/inline "HAKARU10_BIRDS_N") " and "
                (code/inline "HAKARU10_BIRDS_BURN") " set the chain's length and burn-in
                (default 10000 and 1000).")
             (p "The model is Birds1.hs:50-74, as in hakaru10-examples.scm (C4): the cell of
                a day where the bird was seen in exactly one cell is observed (the alternative
                Birds1.hs:62-67, which upstream comments out), unless "
                (code/inline "HAKARU10_BIRDS_PIN=0") " asks for Birds1.hs:68, where every cell
                is drawn and the initial trace must guess all the observed ones.")
             ,@(shown 'birds)))

  ((test/birds-real/data _)
   (let ((obs (data-ref 'obs))
         (features (data-ref 'features))
         (years (data-ref 'years))
         (days (data-ref 'days)))
     (define (every-index? pred extents)
       (let loop ((extents extents) (indices '()))
         (if (null? extents)
             (apply pred (reverse indices))
             (every (λ (i) (loop (cdr extents) (cons i indices))) (iota (car extents) 1)))))
     (⊨ (every-index? (λ (y d i) (let1 (c (obs y d i)) (and (exact-integer? c) (>= c 0))))
                      (list years days n-cells)))
     (⊨ (every-index? (λ (y d i) (= 2. (fourth (features y d i i)))) (list years (sub1 days) n-cells)))
     `(doc (p "The data set has " ,years " years of " ,days " days; every count is a
              non-negative integer, and the fourth feature is 2 where the bird stays (l.173-174:
              \"Encodes whether FromCell == ToCell.  Contains 2 if true\").  The entries that
              Birds1.hs:157-163 and 191-197 print:")
           (code/scheme
             ,(map (Λ ((,y ,d ,i)) `((,y ,d ,i) ,(obs y d i)))
                   `((1 1 1) (1 1 16) (,years ,days 1) (,years ,days 16) (,years ,(quotient days 2) 16))))
           (code/scheme
             ,(map (Λ ((,y ,d ,i ,j)) `((,y ,d ,i ,j) ,(features y d i j)))
                   `((1 1 1 1) (1 1 16 16) (,years ,(sub1 days) 1 1) (,years ,(sub1 days) 16 15))))
           (p "Upstream's loaders, Birds1.hs:141-155 and 170-189:")
           (code/lang "haskell" "-- Load the bird observations
read_obs :: IO ObsData
read_obs = withFile \"onebird-observations.csv\" ReadMode go
  where
    go h = hGetLine h >> loop h []         -- skip the header line
    loop h acc = hIsEOF h >>= \\b ->
      if b then return (array ((1,1,1),(nYears,nDays,nCells)) acc)
      else do
      Rec (y :+ d :+
           x1 :+ x2 :+ x3 :+ x4 :+ x5 :+ x6 :+ x7 :+ x8 :+
           x9 :+ x10 :+ x11 :+ x12 :+ x13 :+ x14 :+ x15 :+ x16) <- readCSV h
      let rows = [((y,d,1),x1),((y,d,2),x2),((y,d,3),x3),((y,d,4),x4),
                  ((y,d,5),x5),((y,d,6),x6),((y,d,7),x7),((y,d,8),x8),
                  ((y,d,9),x9),((y,d,10),x10),((y,d,11),x11),((y,d,12),x12),
                  ((y,d,13),x13),((y,d,14),x14),((y,d,15),x15),((y,d,16),x16)]
      loop h (rows ++ acc)")
           (code/lang "haskell" "read_features :: IO Features
read_features = withFile \"onebird_features.csv\" ReadMode go
  where
    bounds = ((1,1,1,1,1),
              (nYears,nDays-1,nCells,nCells,nFeatures))
    go h = do
      hGetLine h        -- skip the header line
      arr <- newArray bounds 0 ::
        IO (IOUArray (Year,Day,CellNum,CellNum,FeatureIdx) Float)
      loop h arr

    loop h arr = hIsEOF h >>= \\b ->
      if b then freeze arr
      else do
      Rec (y :+ d :+ cfrom :+ cto :+ f1 :+ f2 :+ f3 :+ f4) <- readCSV h
      writeArray arr (y,d,cfrom,cto,1) f1
      writeArray arr (y,d,cfrom,cto,2) f2
      writeArray arr (y,d,cfrom,cto,3) f3
      writeArray arr (y,d,cfrom,cto,4) f4
      loop h arr")
           (p "In Scheme the arrays are tables of nested vectors, read with "
              (code/inline "read-lines") " and " (code/inline "string-split") ".  The
              features are " (code/inline "Float") "s upstream, widened to "
              (code/inline "Double") " by " (code/inline "realToFrac")
              " in Birds1.hs:85, so each one is rounded to single precision by "
              (code/inline "single") " when loaded (a double first, then a single: in rare
              cases this double rounding differs from a direct " (code/inline "read :: Float")
              "):")
           ,@(shown 'make-table 'table-ref 'table-set! 'read-csv 'read-observations 'single
                   'read-features))))

  ((test/birds-real/main1 _)
   (let* ((n (environment-number "HAKARU10_BIRDS_N" 10000))
          (burn (environment-number "HAKARU10_BIRDS_BURN" 1000))
          (pin (not (equal? "0" (get-environment-variable "HAKARU10_BIRDS_PIN"))))
          (bs (main1 burn n #:pin pin))
          (upstream '(4.3696541828670865 6.5965237230656415 5.780875501909224 6.593019768389584)))
     (print `(main1 ,burn ,n (pin ,pin) ,bs (upstream ,upstream)))
     (⊦ = n-features (length bs))
     (⊨ (every (λ (b) (and (finite? b) (>= b 0.))) bs))
     `(doc (p "The posterior means of " (m "β₁, …, β₄") " after " ,n
              " samples, the first " ,burn " dropped: " (code/scheme ,bs)
              "They are finite and non-negative, as the prior "
              (m "|𝒩(0, 10)|") " makes them (l.457-464), and are reported, not
              asserted: upstream's own run of a million samples, and the old Hakaru's, gave
              (Birds1.hs:219-227)")
           (code/lang "haskell" "-- Results from the old Hakaru
-- b1,b2,b3,b4
-- 3.3779743775135875,4.66617475773739,4.1061948941490245,5.593322825143362
--
-- time ./Birds1 1000 1000000
-- [4.3696541828670865,6.5965237230656415,5.780875501909224,6.593019768389584]
-- real 2m34.868s
-- user 2m34.456s
-- sys  0m0.387s")
           (p "while the problem statement's " (code/inline "2.099555 4.561738 2.966087
              3.723159") " (l.189-195) only shows the format of the answer.  Upstream's main
              program, Birds1.hs:202-215, with the fold done by " (code/inline "mcmC-fold")
              " in constant memory:")
           (code/lang "haskell" "main1 burn n = do
  obs      <- read_obs
  features <- read_features
  let r = drop burn $ mcmC n $ model features obs
--  return $ (momenta (map head r), momenta (map (head . tail) r))
--  return $ map (/(fromIntegral $ length r)) $ foldr1 (zipWith (+)) r
  return . map (/(fromIntegral $ length r)) .
           foldl' (zipWith (+)) (repeat 0) $ r

main = do
  [burn',n'] <- getArgs
  let burn = read burn' :: Int
  let    n = read n' :: Integer
  main1 burn n >>= print")
           ,@(shown 'main1))))

  )

(if (get-environment-variable "HAKARU10_BIRDS_DIR")
    (unittest/✓ hakaru10-birds-real)
    (print "hakaru10-birds: HAKARU10_BIRDS_DIR is not set, skipped"))
