;;; a-res-slice.scm --- weighted sampling without replacement over an
;;; in-memory vector, using quickselect instead of a reservoir heap.
;;;
;;; Same underlying algorithm as a-res.scm (Efraimidis & Spirakis,
;;; "Weighted random sampling with a reservoir", IPL 97(5), 2006,
;;; Algorithm A: assign each item a key u^(1/w) and keep the items
;;; with the m largest keys), but selection is done differently: since
;;; the whole population is already in memory (a vector, not a stream),
;;; there's no need to pay the O(log m) heap-maintenance cost per item.
;;; Instead all n keys are computed up front, then a single quickselect
;;; partition finds the m largest in expected O(n) time and O(1) extra
;;; space (beyond the key array), with no particular order guaranteed
;;; among the m selected items or the n-m rejected ones.
;;;
;;; This mirrors the strategy Rust's `rand` crate uses for
;;; SliceRandom::choose_multiple_weighted, which is documented as
;;; being built on the same Efraimidis-Spirakis "Algorithm A" but
;;; choosing a partition/quickselect step (via slice_partition_at_index
;;; on nightly, or a stable equivalent) rather than a heap, since it
;;; already has the full slice available. This is an original
;;; implementation of that same idea, not a transliteration of rand's
;;; Rust source.

(import scheme
        (chicken base)
        (chicken random))

(define (copy-vector v)
  (let* ((len (vector-length v)) (out (make-vector len)))
    (do ((i 0 (+ i 1))) ((= i len) out)
      (vector-set! out i (vector-ref v i)))))

;; log-space key, same rationale as in a-res.scm: log(u)/w is
;; numerically nicer than u^(1/w) and preserves the same ordering.
(define (a-res-key weight)
  (/ (log (pseudo-random-real)) weight))

(define (swap-both! keys items i j)
  (let ((tk (vector-ref keys i)) (ti (vector-ref items i)))
    (vector-set! keys i (vector-ref keys j))
    (vector-set! items i (vector-ref items j))
    (vector-set! keys j tk)
    (vector-set! items j ti)))

;; Lomuto partition of keys[lo..hi] (inclusive) around keys[pivot-index],
;; moving items in lock-step. Returns the pivot's final index; everything
;; left of it has a smaller key, everything right has a key >= it.
(define (partition! keys items lo hi pivot-index)
  (let ((pivot-val (vector-ref keys pivot-index)))
    (swap-both! keys items pivot-index hi)
    (let loop ((i lo) (store lo))
      (if (< i hi)
          (if (< (vector-ref keys i) pivot-val)
              (begin (swap-both! keys items i store) (loop (+ i 1) (+ store 1)))
              (loop (+ i 1) store))
          (begin (swap-both! keys items store hi) store)))))

;; Quickselect with a random pivot (expected O(n), avoids worst-case
;; O(n^2) on already-sorted or adversarial input). After this call,
;; keys[k] holds the k-th smallest key in [lo,hi], with everything
;; smaller to its left and everything >= to its right.
(define (quickselect! keys items lo hi k)
  (let loop ((lo lo) (hi hi))
    (when (< lo hi)
      (let* ((pivot-index (+ lo (pseudo-random-integer (+ (- hi lo) 1))))
             (p (partition! keys items lo hi pivot-index)))
        (cond
          ((= p k) 'done)
          ((< k p) (loop lo (- p 1)))
          (else (loop (+ p 1) hi)))))))

;; items and weights are parallel vectors of equal length. Returns a
;; fresh vector of m items, chosen without replacement with probability
;; proportional to weight; order is unspecified, matching rand's
;; documented behaviour for choose_multiple_weighted.
(define (a-res/slice items weights m)
  (let* ((n (vector-length items)))
    (if (>= m n)
        (copy-vector items)
        (let ((keys (make-vector n))
              (items (copy-vector items)))   ; don't mutate caller's vector
          (do ((i 0 (+ i 1))) ((= i n))
            (vector-set! keys i (a-res-key (vector-ref weights i))))
          (quickselect! keys items 0 (- n 1) (- n m))
          (let ((result (make-vector m)))
            (do ((i 0 (+ i 1))) ((= i m) result)
              (vector-set! result i (vector-ref items (+ (- n m) i)))))))))

;; ---------------------------------------------------------------------
;; Self-check
;; ---------------------------------------------------------------------

(define (run-self-check)
  (define items   #('a 'b 'c 'd 'e))
  (define weights #(1.0 2.0 3.0 4.0 5.0))
  (define trials 20000)
  (define m 2)
  (define counts (map (lambda (x) (cons x 0)) (vector->list items)))
  (define (bump! key)
    (let ((cell (assq key counts)))
      (set-cdr! cell (+ (cdr cell) 1))))
  (do ((i 0 (+ i 1))) ((= i trials))
    (let ((sample (a-res/slice items weights m)))
      (do ((j 0 (+ j 1))) ((= j m))
        (bump! (vector-ref sample j)))))
  (print "empirical inclusion frequencies over " trials " trials, m=" m ":")
  (do ((i 0 (+ i 1))) ((= i (vector-length items)))
    (let ((x (vector-ref items i)))
      (print "  " x " (weight " (vector-ref weights i) "): "
             (exact->inexact (/ (cdr (assq x counts)) trials)))))
  (print "sample: " (a-res/slice items weights m))
  (print "sample size == m? " (= (vector-length (a-res/slice items weights m)) m)))

(run-self-check)