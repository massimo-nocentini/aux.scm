; (aux fds heap): persistent min-heaps, as pairing heaps (Fredman, Sedgewick, Sleator and Tarjan,
; "The pairing heap: a new form of self-adjusting heap", Algorithmica 1, 1986), purely functional as
; in Okasaki, "Purely Functional Data Structures", 1998, §5.5.
;
; A heap is a record of its order `less?` (a strict weak order fixed when the heap is made), its size
; and a tree: '() for the empty heap, or (cons element subtrees), where `subtrees` is a list of
; non-empty trees whose elements are all not less than `element` (the heap order). The minimum is
; therefore the root. Like its sibling (aux fds queue), and unlike (aux fds unionfind), the structure
; is persistent: no operation changes a heap, they all return a new one sharing most of the old.
;
; Choices:
;   - a pairing heap rather than a leftist heap, because its typical use, the frontier of a search,
;     is linear (an old version is never popped again), and there pairing heaps are faster;
;   - the bounds, O(1) `fds-heap-insert` and `fds-heap-merge`, O(log n) `fds-heap-delete-min`, are
;     amortized, so they hold for linear use only: popping the same old version again and again
;     redoes its pairing pass every time;
;   - `merge-pairs` is Okasaki's two-pass `mergePairs` written as two loops, left to right and
;     then back, so that a long list of subtrees never makes a deep recursion;
;   - on ties `link` keeps its first argument at the root; callers that need a deterministic order
;     among equal priorities should make the keys distinct (as (aux search) does with a serial).

(module (aux fds heap) (make-fds-heap fds-heap? fds-heap-less? fds-heap-empty? fds-heap-size fds-heap-min
                        fds-heap-insert fds-heap-delete-min fds-heap-pop fds-heap-merge
                        list->fds-heap fds-heap->list)

  (import scheme (chicken base) (aux base))

  (define-record-type fds-heap
    (heap less? size tree)
    fds-heap?
    (less? fds-heap-less?)
    (size fds-heap-size)
    (tree fds-heap-tree))

  (define (make-fds-heap less?) (heap less? 0 '()))

  (define (fds-heap-empty? h) (null? (fds-heap-tree h)))

  ; trees ------------------------------------------------------------------------------------------

  ; the tree of the non-empty trees a and b: the greater root becomes the first subtree of the other.
  (define (link less? a b)
    (if (less? (car b) (car a))
      (cons (car b) (cons a (cdr b)))
      (cons (car a) (cons b (cdr a)))))

  (define (meld less? a b)
    (cond
      ((null? a) b)
      ((null? b) a)
      (else (link less? a b))))

  ; the tree of the list of subtrees ts: link them in pairs left to right, then link the pairs
  ; right to left, each into the tree of the pairs on its right.
  (define (merge-pairs less? ts)
    (let pair ((ts ts) (pairs '())) ; pairs: the linked pairs so far, the last one first
      (cond
        ((null? ts) (fold-back less? pairs))
        ((null? (cdr ts)) (fold-back less? (cons (car ts) pairs)))
        (else (pair (cddr ts) (cons (link less? (car ts) (cadr ts)) pairs))))))

  (define (fold-back less? pairs)
    (if (null? pairs)
      '()
      (let loop ((pairs (cdr pairs)) (t (car pairs)))
        (if (null? pairs)
          t
          (loop (cdr pairs) (link less? (car pairs) t))))))

  ; heaps ------------------------------------------------------------------------------------------

  (define (fds-heap-min h)
    (if (fds-heap-empty? h)
      (error "fds-heap-min: empty heap")
      (car (fds-heap-tree h))))

  (define (fds-heap-insert x h)
    (let ((less? (fds-heap-less? h)))
      (heap less? (add1 (fds-heap-size h)) (meld less? (list x) (fds-heap-tree h)))))

  (define (fds-heap-delete-min h)
    (if (fds-heap-empty? h)
      (error "fds-heap-delete-min: empty heap")
      (let ((less? (fds-heap-less? h)))
        (heap less? (sub1 (fds-heap-size h)) (merge-pairs less? (cdr (fds-heap-tree h)))))))

  (define (fds-heap-pop h)
    (if (fds-heap-empty? h)
      (error "fds-heap-pop: empty heap")
      (values (fds-heap-min h) (fds-heap-delete-min h))))

  (define (fds-heap-merge h₁ h₂)
    (let ((less? (fds-heap-less? h₁)))
      (unless (eq? less? (fds-heap-less? h₂)) (error "fds-heap-merge: different orders"))
      (heap less?
            (+ (fds-heap-size h₁) (fds-heap-size h₂))
            (meld less? (fds-heap-tree h₁) (fds-heap-tree h₂)))))

  (define (list->fds-heap less? lst)
    (let loop ((lst lst) (h (make-fds-heap less?)))
      (if (null? lst) h (loop (cdr lst) (fds-heap-insert (car lst) h)))))

  ; the elements in ascending order; h itself is unchanged, as always.
  (define (fds-heap->list h)
    (let loop ((h h) (acc '()))
      (if (fds-heap-empty? h)
        (reverse acc)
        (loop (fds-heap-delete-min h) (cons (fds-heap-min h) acc)))))

  )
