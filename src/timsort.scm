;;; timsort.scm --- an implementation of the timsort algorithm for Chicken Scheme
;;;
;;; This is an original implementation of the timsort algorithm design
;;; (natural run detection, minrun sizing, binary-insertion-sort extension
;;; of short runs, and a run-stack merge policy that preserves the classic
;;; timsort length invariants). It is written from scratch, in Scheme idiom,
;;; and is NOT a translation of any specific existing source file. For
;;; clarity it omits some of the low-level performance work found in
;;; production implementations (e.g. CPython's "galloping mode" during
;;; merges, and the newer "powersort" merge-policy variant); the classic
;;; merge-invariant policy is used instead.
;;;
;;; Sorts a vector in place, ascending, using a user-supplied `less?`.

(import scheme
        (chicken base)
        (chicken random))

;; Minimal local stand-ins for srfi-1's take/drop, to avoid an external
;; egg dependency.
(define (take lst k)
  (if (= k 0) '() (cons (car lst) (take (cdr lst) (- k 1)))))

(define (drop lst k)
  (if (= k 0) lst (drop (cdr lst) (- k 1))))

;; --- minimum run length --------------------------------------------------

;; Classic timsort trick: shrink n by halving until it's below 64,
;; remembering whether any 1-bit was shifted out along the way.
(define (compute-min-run n0)
  (let loop ((n n0) (extra 0))
    (if (>= n 64)
        (loop (quotient n 2) (if (odd? n) 1 extra))
        (+ n extra))))

;; --- in-place vector sort --------------------------------------------------

(define (timsort! vec less?)
  (define n (vector-length vec))
  (define min-run (compute-min-run n))

  (define (reverse-range! lo hi)     ; reverse vec[lo, hi] inclusive
    (let loop ((i lo) (j hi))
      (when (< i j)
        (let ((tmp (vector-ref vec i)))
          (vector-set! vec i (vector-ref vec j))
          (vector-set! vec j tmp))
        (loop (+ i 1) (- j 1)))))

  ;; Identify the natural run starting at lo (bound: hi, exclusive).
  ;; A "run" is a maximal ascending (non-strict) or strictly descending
  ;; stretch. Descending runs are reversed in place so every run coming
  ;; out of this function is ascending. Returns the run's length.
  (define (count-run lo hi)
    (if (>= (+ lo 1) hi)
        1
        (if (less? (vector-ref vec (+ lo 1)) (vector-ref vec lo))
            (let loop ((i (+ lo 2)))
              (if (and (< i hi) (less? (vector-ref vec i) (vector-ref vec (- i 1))))
                  (loop (+ i 1))
                  (begin (reverse-range! lo (- i 1)) (- i lo))))
            (let loop ((i (+ lo 2)))
              (if (and (< i hi) (not (less? (vector-ref vec i) (vector-ref vec (- i 1)))))
                  (loop (+ i 1))
                  (- i lo))))))

  ;; Binary insertion sort of vec[lo, hi), given that vec[lo, start) is
  ;; already known to be sorted. Used to pad short natural runs up to
  ;; min-run length.
  (define (binary-insertion-sort! lo hi start)
    (let loop ((i (max start (+ lo 1))))
      (when (< i hi)
        (let ((pivot (vector-ref vec i)))
          (let bsearch ((left lo) (right i))
            (if (< left right)
                (let ((mid (quotient (+ left right) 2)))
                  (if (less? pivot (vector-ref vec mid))
                      (bsearch left mid)
                      (bsearch (+ mid 1) right)))
                (begin
                  (let shift ((j i))
                    (when (> j left)
                      (vector-set! vec j (vector-ref vec (- j 1)))
                      (shift (- j 1))))
                  (vector-set! vec left pivot)))))
        (loop (+ i 1)))))

  ;; Merge adjacent sorted runs vec[lo,mid) and vec[mid,hi) using a
  ;; temporary buffer sized to the smaller of the two runs.
  (define (merge-runs! lo mid hi)
    (let ((left-len (- mid lo))
          (right-len (- hi mid)))
      (if (<= left-len right-len)
          (let ((tmp (make-vector left-len)))
            (do ((i 0 (+ i 1))) ((= i left-len))
              (vector-set! tmp i (vector-ref vec (+ lo i))))
            (let loop ((i 0) (j mid) (k lo))
              (cond
                ((= i left-len) #t)
                ((= j hi)
                 (do ((ii i (+ ii 1)) (kk k (+ kk 1))) ((= ii left-len))
                   (vector-set! vec kk (vector-ref tmp ii))))
                ((less? (vector-ref vec j) (vector-ref tmp i))
                 (vector-set! vec k (vector-ref vec j))
                 (loop i (+ j 1) (+ k 1)))
                (else
                 (vector-set! vec k (vector-ref tmp i))
                 (loop (+ i 1) j (+ k 1))))))
          (let ((tmp (make-vector right-len)))
            (do ((i 0 (+ i 1))) ((= i right-len))
              (vector-set! tmp i (vector-ref vec (+ mid i))))
            (let loop ((i (- left-len 1)) (j (- right-len 1)) (k (- hi 1)))
              (cond
                ((< j 0) #t)
                ((< i 0)
                 (do ((jj j (- jj 1)) (kk k (- kk 1))) ((< jj 0))
                   (vector-set! vec kk (vector-ref tmp jj))))
                ((less? (vector-ref tmp j) (vector-ref vec (+ lo i)))
                 (vector-set! vec k (vector-ref vec (+ lo i)))
                 (loop (- i 1) j (- k 1)))
                (else
                 (vector-set! vec k (vector-ref tmp j))
                 (loop i (- j 1) (- k 1)))))))))

  ;; Pending runs, held as (start . length) pairs, most-recent run first.
  (define run-stack '())

  (define (push-run! start len)
    (set! run-stack (cons (cons start len) run-stack)))

  ;; Merge the run at position idx in run-stack with the one right
  ;; after it (idx 0 is the most recently pushed / rightmost run).
  (define (merge-at! idx)
    (let* ((before (take run-stack idx))
           (rest   (drop run-stack idx))
           (run2   (car rest))
           (run1   (cadr rest))
           (after  (cddr rest))
           (start1 (car run1)) (len1 (cdr run1))
           (start2 (car run2)) (len2 (cdr run2))
           (merged (cons start1 (+ len1 len2))))
      (merge-runs! start1 start2 (+ start2 len2))
      (set! run-stack (append before (cons merged after)))))

  (define (run-len k) (cdr (list-ref run-stack k)))

  ;; Restore the two classic timsort invariants over the run stack:
  ;;   len(run[i+2]) > len(run[i+1]) + len(run[i])
  ;;   len(run[i+1]) > len(run[i])
  ;; merging as needed, working from the top of the stack downward.
  (define (merge-collapse!)
    (let loop ()
      (let ((depth (length run-stack)))
        (cond
          ((< depth 2) #t)
          ((and (>= depth 3) (<= (run-len 2) (+ (run-len 1) (run-len 0))))
           (if (< (run-len 2) (run-len 0)) (merge-at! 1) (merge-at! 0))
           (loop))
          ((<= (run-len 1) (run-len 0))
           (merge-at! 0)
           (loop))
          (else #t)))))

  (define (merge-force-collapse!)
    (let loop ()
      (when (>= (length run-stack) 2)
        (if (and (>= (length run-stack) 3) (< (run-len 2) (run-len 0)))
            (merge-at! 1)
            (merge-at! 0))
        (loop))))

  (let loop ((lo 0))
    (if (< lo n)
        (let* ((natural-len (count-run lo n))
               (force-len   (min min-run (- n lo))))
          (if (< natural-len force-len)
              (begin
                (binary-insertion-sort! lo (+ lo force-len) (+ lo natural-len))
                (push-run! lo force-len)
                (merge-collapse!)
                (loop (+ lo force-len)))
              (begin
                (push-run! lo natural-len)
                (merge-collapse!)
                (loop (+ lo natural-len)))))
        (merge-force-collapse!)))
  vec)

(define (copy-vector v)
  (let* ((len (vector-length v))
         (out (make-vector len)))
    (do ((i 0 (+ i 1))) ((= i len) out)
      (vector-set! out i (vector-ref v i)))))

;; Convenience: returns a freshly sorted copy, leaving vec untouched.
(define (timsort vec #!optional (less? <))
  (timsort! (copy-vector vec) less?))

;; --- quick self-check --------------------------------------------------

(define (sorted? v less?)
  (let loop ((i 1))
    (or (>= i (vector-length v))
        (and (not (less? (vector-ref v i) (vector-ref v (- i 1))))
             (loop (+ i 1))))))

(let* ((sizes (list 0 1 2 5 30 63 64 65 200 1000 5000))
       (ok #t))
  (for-each
   (lambda (size)
     (let ((v (make-vector size)))
       (do ((i 0 (+ i 1))) ((= i size))
         (vector-set! v i (pseudo-random-integer 10000)))
       (let ((sorted (timsort v <)))
         (unless (sorted? sorted <)
           (set! ok #f)
           (print "FAILED for size " size)))))
   sizes)
  (print (if ok "all self-checks passed" "self-check FAILURE")))
