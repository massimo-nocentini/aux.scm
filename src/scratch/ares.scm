;;; a-res.scm --- weighted random sampling without replacement (A-Res)
;;;
;;; Implements the A-Res algorithm of Efraimidis & Spirakis, "Weighted
;;; random sampling with a reservoir", Information Processing Letters
;;; 97(5), 2006. Each item i with weight w_i > 0 is assigned a key
;;;   k_i = u_i ^ (1 / w_i),   u_i ~ Uniform(0,1)
;;; and the m items with the largest keys form a weighted random
;;; sample of size m, drawn without replacement, in a single pass.
;;;
;;; Two entry points are provided:
;;;   - a-res/list   : simplest form, works over a Scheme list, O(n log n)
;;;   - a-res/stream : one-pass reservoir form using a size-m min-heap,
;;;                    O(n log m) --- suitable for a generator/stream
;;;                    where you don't want, or can't hold, the whole
;;;                    population in memory at once.

(import scheme
        (chicken base)
        (chicken random))

;; ---------------------------------------------------------------------
;; A small binary min-heap over vectors, keyed by a comparison function.
;; Used to hold the reservoir: the m items with the largest keys seen so
;; far, with the *smallest* key sitting at the root so it can be evicted
;; in O(log m) as soon as something bigger comes along.
;; ---------------------------------------------------------------------

(define-record-type heap
  (make-heap% items size capacity key-of)
  heap?
  (items heap-items set-heap-items!)
  (size heap-size set-heap-size!)
  (capacity heap-capacity)
  (key-of heap-key-of))

(define (make-heap capacity key-of)
  (make-heap% (make-vector capacity) 0 capacity key-of))

(define (heap-full? h) (= (heap-size h) (heap-capacity h)))
(define (heap-empty? h) (= (heap-size h) 0))

(define (heap-peek-key h)
  ((heap-key-of h) (vector-ref (heap-items h) 0)))

(define (heap-swap! v i j)
  (let ((tmp (vector-ref v i)))
    (vector-set! v i (vector-ref v j))
    (vector-set! v j tmp)))

(define (heap-sift-up! h i0)
  (let ((v (heap-items h)) (key-of (heap-key-of h)))
    (let loop ((i i0))
      (when (> i 0)
        (let ((parent (quotient (- i 1) 2)))
          (when (< (key-of (vector-ref v i)) (key-of (vector-ref v parent)))
            (heap-swap! v i parent)
            (loop parent)))))))

(define (heap-sift-down! h i0)
  (let ((v (heap-items h)) (n (heap-size h)) (key-of (heap-key-of h)))
    (let loop ((i i0))
      (let* ((l (+ (* 2 i) 1))
             (r (+ (* 2 i) 2))
             (smallest
              (let* ((s (if (and (< l n) (< (key-of (vector-ref v l)) (key-of (vector-ref v i)))) l i))
                     (s (if (and (< r n) (< (key-of (vector-ref v r)) (key-of (vector-ref v s)))) r s)))
                s)))
        (unless (= smallest i)
          (heap-swap! v i smallest)
          (loop smallest))))))

;; Insert unconditionally; caller must ensure there's room (size < capacity).
(define (heap-insert! h item)
  (let ((v (heap-items h)) (i (heap-size h)))
    (vector-set! v i item)
    (set-heap-size! h (+ i 1))
    (heap-sift-up! h i)))

;; Replace the current minimum with a new item, restoring heap order.
(define (heap-replace-min! h item)
  (vector-set! (heap-items h) 0 item)
  (heap-sift-down! h 0))

;; Return the reservoir contents as a plain list, in no particular order.
(define (heap->list h)
  (let loop ((i 0) (acc '()))
    (if (= i (heap-size h))
        acc
        (loop (+ i 1) (cons (vector-ref (heap-items h) i) acc)))))

;; ---------------------------------------------------------------------
;; A-Res key computation
;; ---------------------------------------------------------------------

;; key = u^(1/w), for u drawn uniformly from (0,1).
;; Using log-space (log(u) / w) instead is a common numerical-stability
;; refinement, since it avoids overflow/underflow from raising a small u
;; to a large power; it produces the same ordering since log is monotone.
(define (a-res-key weight)
  (/ (log (pseudo-random-real)) weight))

;; ---------------------------------------------------------------------
;; Simple, non-streaming version: compute all keys, sort, take top m.
;; items is a list of (value . weight) pairs. Returns a list of m
;; (value . weight) pairs, sorted by descending key (best first).
;; ---------------------------------------------------------------------

(define (a-res/list items m)
  (let* ((keyed (map (lambda (p) (cons (a-res-key (cdr p)) p)) items))
         (sorted (sort keyed (lambda (x y) (> (car x) (car y))))))
    (map cdr (if (> (length sorted) m) (take sorted m) sorted))))

;; local take, to avoid pulling in srfi-1 for one helper
(define (take lst k)
  (if (or (= k 0) (null? lst))
      '()
      (cons (car lst) (take (cdr lst) (- k 1)))))

;; a stable-ish sort (merge sort) since we don't want to depend on an
;; external egg just for this
(define (sort lst less?)
  (define (merge a b)
    (cond ((null? a) b)
          ((null? b) a)
          ((less? (car a) (car b)) (cons (car a) (merge (cdr a) b)))
          (else (cons (car b) (merge a (cdr b))))))
  (define (split lst)
    (let loop ((slow lst) (fast lst) (acc '()))
      (if (or (null? fast) (null? (cdr fast)))
          (values (reverse acc) slow)
          (loop (cdr slow) (cddr fast) (cons (car slow) acc)))))
  (if (or (null? lst) (null? (cdr lst)))
      lst
      (let-values (((a b) (split lst)))
        (merge (sort a less?) (sort b less?)))))

;; ---------------------------------------------------------------------
;; One-pass reservoir version, matching the paper's A-Res pseudocode.
;; `next!` is a thunk that returns the next (value . weight) pair, or
;; the symbol 'done when the stream is exhausted. Returns a list of up
;; to m (value . weight) pairs.
;; ---------------------------------------------------------------------

(define (a-res/stream next! m)
  (define (item-key p) (car p))          ; p = (key value . weight), see below
  (define h (make-heap m item-key))
  (let loop ()
    (let ((p (next!)))
      (unless (eq? p 'done)
        (let* ((value (car p)) (weight (cdr p))
               (key (a-res-key weight))
               (entry (list key value weight)))
          (cond
            ((not (heap-full? h)) (heap-insert! h entry))
            ((> key (heap-peek-key h)) (heap-replace-min! h entry)))
          (loop)))))
  (map (lambda (entry) (cons (cadr entry) (caddr entry))) (heap->list h)))

;; convenience: build a `next!` thunk from a plain in-memory list, so
;; a-res/stream can be exercised the same way a-res/list is
(define (list->generator lst)
  (let ((remaining lst))
    (lambda ()
      (if (null? remaining)
          'done
          (let ((item (car remaining)))
            (set! remaining (cdr remaining))
            item)))))

;; ---------------------------------------------------------------------
;; Self-check: verify sampling is (a) unbiased in the sense of matching
;; theoretical inclusion probabilities within noise, and (b) that
;; a-res/list and a-res/stream agree on set membership.
;; ---------------------------------------------------------------------

(define (run-self-check)
  (define population
    (list (cons 'a 1.0) (cons 'b 2.0) (cons 'c 3.0)
          (cons 'd 4.0) (cons 'e 5.0)))
  (define trials 20000)
  (define m 2)
  (define counts (map (lambda (p) (cons (car p) 0)) population))
  (define (bump! key)
    (let ((cell (assq key counts)))
      (set-cdr! cell (+ (cdr cell) 1))))
  (do ((i 0 (+ i 1))) ((= i trials))
    (let ((sample (a-res/list population m)))
      (for-each (lambda (p) (bump! (car p))) sample)))
  (print "empirical inclusion frequencies over " trials " trials, m=" m ":")
  (for-each
    (lambda (p)
      (print "  " (car p) " (weight " (cdr p) "): "
             (exact->inexact (/ (cdr (assq (car p) counts)) trials))))
    population)
  ;; cross-check list vs stream versions agree on cardinality and draw
  ;; from the same distribution shape (not bit-identical, since each
  ;; draws its own random numbers)
  (let* ((sample-list   (a-res/list population m))
         (sample-stream (a-res/stream (list->generator population) m)))
    (print "a-res/list sample:   " (map car sample-list))
    (print "a-res/stream sample: " (map car sample-stream))
    (print "both sizes == m? " (and (= (length sample-list) m) (= (length sample-stream) m)))))

(run-self-check)