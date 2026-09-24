;; Union-find (disjoint sets) with union by rank and optional path compression.
;;
;; Ported from on-scheme's `unionfind.scm`.
;;
;; NOTE: unlike its persistent siblings (aux fds queue) and (aux fds sbral),
;; this structure is MUTABLE: a `unionfind` record holds two srfi-69
;; `equal?`-hash-tables, `π` mapping each node to its parent and `rank`
;; mapping each node to its rank. `unionfind-≡` and `unionfind-↑!` update
;; them in place; use `unionfind-copy` (O(n)) to obtain an independent copy.
;;
;; Public API:
;; - `unionfind-empty` creates an empty structure.
;; - `unionfind-new` creates an empty U and calls `(recv U ↑ ↑! ≡ →)`.
;; - `unionfind-accessors` calls `(recv ↑ ↑! ≡ →)` on an existing U.
;; - `unionfind-↑` finds a root without compression, `unionfind-↑!` with full
;;   path compression; both return x itself when x is absent.
;; - `unionfind-≡` returns a binary union procedure (union by rank, unseen
;;   nodes are inserted with rank 0; its result is unspecified).
;; - `unionfind-★` returns a rank lookup (an error for absent nodes).
;; - `unionfind-keys`, `unionfind-size`, `unionfind-edges` (non self-loop
;;   parent links), `unionfind-copy`, `unionfind-walk`,
;;   `unionfind-walk/without-loops`, `unionfind->alist` (node . parent) pairs
;;   in hash order, `unionfind->→+★` (((node . rank) (parent . rank)) ...).
(module (aux fds unionfind) *

  (import scheme (chicken base) (only srfi-1 car+cdr) srfi-69 (aux base))

  (define-record unionfind π rank)

  (set-record-printer! unionfind
    (λ (U out)
      (for-each ; no need to produce output
        (λ (e)
          (let-values (((a d) (car+cdr e)))
            (display `(,a -> ,d) out)
            (newline out)))
        (unionfind->alist U))))

  (define (unionfind-walk U recv res)
    (hash-table-walk (unionfind-π U) recv)
    (res))

  (define (unionfind-walk/without-loops U recv res)
    (unionfind-walk U
                    (λ (k v) (unless (equal? k v) (recv k v)))
                    res))

  (define (unionfind-empty) (make-unionfind (make-hash-table) (make-hash-table)))

  (define unionfind-keys (compose hash-table-keys unionfind-π))

  (define unionfind-size (compose hash-table-size unionfind-π))

  (define (unionfind-edges U)
    (let1 (n 0)
      (unionfind-walk/without-loops U
                                    (λ (k v) (set! n (add1 n)))
                                    (λ () n))))

  (define (unionfind-copy U)
    (make-unionfind
      (hash-table-copy (unionfind-π U))
      (hash-table-copy (unionfind-rank U))))

  (define (unionfind-↑ U)
    (letrec ((π (unionfind-π U))
             (↑ (λ (x)
                  (cond
                    ((hash-table-exists? π x) (let1 (y (hash-table-ref π x))
                                                (if (equal? x y) x (↑ y))))
                    (else x)))))
      ↑))

  (define (unionfind-↑! U)
    (letrec ((π (unionfind-π U))
             (↑ (λ (x)
                  (cond
                    ((hash-table-exists? π x) (let1 (y (hash-table-ref π x))
                                                (unless (equal? x y) (hash-table-set! π x (↑ y)))
                                                (hash-table-ref π x)))
                    (else x)))))
      ↑))

  (define ((unionfind-★ U) y) (hash-table-ref (unionfind-rank U) y))

  (define (unionfind-≡ U)
    (let* ((π       (unionfind-π U))
           (rank    (unionfind-rank U))
           (in?     (λ (x) (hash-table-exists? π x)))
           (insert! (λ (x)
                      (hash-table-set! π x x)
                      (hash-table-set! rank x 0)))
           (↑       (unionfind-↑ U))
           (★       (unionfind-★ U))
           (≡ (λ (x y) ; the return is undefined
                (unless (in? x) (insert! x))
                (unless (in? y) (insert! y))
                (let ((root-x (↑ x))
                      (root-y (↑ y)))
                  (unless (equal? root-x root-y)
                    (cond
                      ((> (★ root-x) (★ root-y)) (hash-table-set! π root-y root-x))
                      (else (hash-table-set! π root-x root-y)
                            (when (equal? (★ root-x) (★ root-y))
                              (hash-table-update! rank root-y add1)))))))))
      ≡))

  (define (unionfind-accessors U recv)
    (let ((↑  (unionfind-↑ U))
          (↑! (unionfind-↑! U))
          (≡  (unionfind-≡ U))
          (→  (λ () (unionfind->alist U))))
      (recv ↑ ↑! ≡ →)))

  (define (unionfind-new recv)
    (let ((U (unionfind-empty))
          (R (curry₁ recv)))
      (unionfind-accessors U (R U))))

  (define (unionfind->alist U)
    (let* ((π (unionfind-π U))
           (C (λ (k) (cons k (hash-table-ref π k)))))
      (map C (unionfind-keys U))))

  (define (unionfind->→+★ U)
    (let* ((★ (unionfind-★ U))
           (C (λ (k)
                (let-values (((a d) (car+cdr k)))
                  (cons (cons a (★ a)) (list (cons d (★ d))))))))
      (map C (unionfind->alist U))))

  )
