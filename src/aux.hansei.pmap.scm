;; (aux hansei pmap) -- persistent maps for (aux hansei), a CHICKEN port of Oleg Kiselyov and
;; Chung-chieh Shan's HANSEI (https://okmij.org/ftp/kakuritu/Hansei.html).
;;
;; Mirrors (paths relative to aux.scm/upstream/hansei/, citation keys as in the plan
;; AUX-HANSEI-PLAN.md §0):
;;   PMI  pMap.mli (https://okmij.org/ftp/kakuritu/pMap.mli) and
;;   PM   pMap.ml (https://okmij.org/ftp/kakuritu/pMap.ml),
;; ExtLib's polymorphic maps, (c) 1996-2003 Xavier Leroy, Nicolas Cannasse, Markus Mottl,
;; LGPL-2.1 with the OCaml linking exception, as augmented by HANSEI with `insert_with`.
;; A persistent AVL tree with its own comparison procedure: HANSEI uses it to collate values
;; with their masses in `explore` (I:16-28), as the priority queue of `bounded_explore`
;; (I:71-109), and as the world-local memory and the `memo` tables (P:40-64, 466-479); so does
;; (aux hansei) (plan D3, D4).  OCaml's polymorphic `compare`, the default order of PM:101, is
;; `pmap-compare` (plan §6.1).
;;
;; HANSEI code version: April 2009 (H, "Implementation"); web page last updated March 1, 2016.
;; Papers: O. Kiselyov, C. Shan, "Embedded Probabilistic Programming", IFIP DSL 2009, LNCS 5658,
;; pp. 360-384 (DSL = dsl-paper.txt / .pdf); "Probabilistic programming using first-class stores
;; and first-class continuations", ML Workshop 2010 (ML = ML2010.txt / .pdf).
;;
;; Contents (plan §3.4, §6.1):
;;   - the (exn pmap) error conditions, kinds `not-found`, `empty` and `compare`;
;;   - the orders: pmap-compare (OCaml `compare`), fx-compare, number-compare;
;;   - the pmap record (a comparison procedure and an AVL tree of #(l k v r h) nodes, '() being
;;     the empty tree) and the internal height, node, bal, merge, delete-find-min/tree,
;;     delete-find-max/tree (PM:31-98), not exported;
;;   - the PMI interface: pmap-empty, pmap-create, pmap-empty?, pmap-add, pmap-insert-with,
;;     pmap-find, pmap-find-min, pmap-delete-find-min, pmap-delete-find-max, pmap-remove,
;;     pmap-member?, pmap-for-each, pmap-map, pmap-map/key, pmap-fold, pmap-foldi;
;;   - (new) pmap->alist, alist->pmap, pmap?.
;; PM's `enum` and `of_enum` (PMI:112-119, PM:201-233) are commented out upstream ("We don't
;; need enum") and not ported; pmap->alist and alist->pmap stand in for them.

(module (aux hansei pmap) (pmap-empty pmap-create pmap-empty? pmap-add pmap-insert-with
                           pmap-find pmap-find-min pmap-delete-find-min pmap-delete-find-max
                           pmap-remove pmap-member? pmap-for-each pmap-map pmap-map/key
                           pmap-fold pmap-foldi pmap->alist alist->pmap pmap-compare
                           fx-compare number-compare pmap?)

  (import scheme
          (chicken base)
          (chicken fixnum)
          (only (chicken format) fprintf)
          (only (chicken condition) make-property-condition make-composite-condition abort)
          (only (chicken memory representation) record-instance? record->vector)
          (only (chicken keyword) keyword? keyword->string)
          (only (chicken bytevector) bytevector? bytevector-length bytevector-u8-ref)
          (aux base))

  ; No definition of this module is ever redefined, so the compiler may open-code the record
  ; accessors and inline the tree helpers into the loops of the interface.
  (declare (local))

  ;; ---------------------------------------------------------------------------------------
  ;; Errors (plan §7): composite (exn pmap) conditions whose pmap part carries a `kind`, one
  ;; of not-found (PM:141, `Not_found`), empty (PM:66, 76, 85: `Not_found` and
  ;; `invalid_arg` on the empty map) and compare (OCaml's `compare: functional value`).
  ;; ---------------------------------------------------------------------------------------

  ; (new) deviation 30 (PM:66, 76, 85, 141): a condition instead of an OCaml exception, raised with
  ; `abort`, which is not continuable like OCaml's `raise`: it never returns, and a handler
  ; that returns raises "exception handler returned" instead of handing its value to the tree.
  (define (pmap-signal kind loc msg . args)
    (abort (make-composite-condition
              (make-property-condition 'exn 'location loc 'message msg 'arguments args)
              (make-property-condition 'pmap 'kind kind))))

  ;; ---------------------------------------------------------------------------------------
  ;; Orders (plan §6.1): a comparison answers a negative, zero or positive fixnum, as OCaml's.
  ;; ---------------------------------------------------------------------------------------

  ; (new, plan §3.4) the three-way order of fixnums, the `int` compare of P:44's Memory.M; a
  ; comparison rather than (fx- a b), which would overflow on fixnums far apart.
  (define (fx-compare a b) (cond ((fx< a b) -1) ((fx> a b) 1) (else 0)))

  ; (new, plan §3.4) the three-way order of real numbers, the float compare of the queue of
  ; `bounded_explore` (I:78); it does not order NaN (see pmap-compare).
  (define (number-compare a b) (cond ((< a b) -1) ((> a b) 1) (else 0)))

  ; (new) the rank of x's type in pmap-compare's order, #f for a type it cannot order.  (void)
  ; is OCaml's `()`, the constant 0, below every other value (plan §6.1): parsec-eof answers
  ; it and bounded_explore takes unit models.
  (define (pmap-rank x)
    (cond
      ((eq? x (void)) 0)
      ((boolean? x) 1)
      ((number? x) 2)
      ((char? x) 3)
      ((string? x) 4)
      ((symbol? x) 5)
      ((keyword? x) 6)
      ((or (null? x) (pair? x)) 7)
      ((vector? x) 8)
      ((bytevector? x) 9)
      ((record-instance? x) 10)
      (else #f)))

  ; (new) the order of reals within pmap-compare: by <, = giving 0 so that -0. and 0. are
  ; equal, as in OCaml; NaN equal to itself and below every other number, as OCaml's
  ; `compare`; an exact number before an equal inexact one, which `equal?` keeps distinct.
  (define (pmap-compare/real a b)
    (cond
      ((nan? a) (if (nan? b) 0 -1))
      ((nan? b) 1)
      ((< a b) -1)
      ((> a b) 1)
      ((eq? (exact? a) (exact? b)) 0)
      ((exact? a) -1)
      (else 1)))

  ; (new) the order of numbers within pmap-compare: reals as above, other numbers by their
  ; real parts, then by their imaginary parts.
  (define (pmap-compare/number a b)
    (cond
      ((and (real? a) (real? b)) (pmap-compare/real a b))
      (else (let1 (c (pmap-compare/real (real-part a) (real-part b)))
              (if (fx= c 0) (pmap-compare/real (imag-part a) (imag-part b)) c)))))

  ; (new) the three-way order of strings.
  (define (pmap-compare/string a b) (cond ((string<? a b) -1) ((string=? a b) 0) (else 1)))

  ; (new) the order of vectors and bytevectors within pmap-compare: by length first, as OCaml
  ; compares blocks by size, then elementwise with C over the elements answered by ref.
  (define (pmap-compare/sequence a b len ref C)
    (let ((la (len a))
          (lb (len b)))
      (cond
        ((fx< la lb) -1)
        ((fx> la lb) 1)
        (else (let loop ((i 0))
                (cond
                  ((fx= i la) 0)
                  (else (let1 (c (C (ref a i) (ref b i)))
                          (if (fx= c 0) (loop (fx+ i 1)) c)))))))))

  ; cf. PM:101 (`compare`, the default order of `empty`); deviation 6 (PM:101): a total order
  ; standing in for OCaml's polymorphic `compare` (plan §6.1).  Types rank (void) < booleans
  ; < numbers < chars < strings < symbols < keywords < '() and pairs < vectors < bytevectors
  ; < records; within a type, #f < #t, numbers as pmap-compare/number, chars by char<?,
  ; strings by string<?, symbols and keywords by name (OCaml orders variants by declaration),
  ; '() before any pair, pairs by car then cdr (lists lexicographically, a prefix first),
  ; vectors and bytevectors by length then elements, records as the vectors of their slots
  ; (type tag first).  eq? values are equal, as OCaml's physical-equality shortcut makes them;
  ; anything else (procedures, ports, ...) raises (exn pmap compare), as OCaml's `compare:
  ; functional value` does.  The fixnum case comes first, the commonest key.
  (define (pmap-compare a b)
    (cond
      ((eq? a b) 0)
      ((and (fixnum? a) (fixnum? b)) (fx-compare a b))
      (else
        (let ((ra (pmap-rank a))
              (rb (pmap-rank b)))
          (cond
            ((not (and ra rb)) (pmap-signal 'compare 'pmap-compare "cannot compare" a b))
            ((fx< ra rb) -1)
            ((fx> ra rb) 1)
            (else
              (case ra
                ((0) 0)
                ((1) (if a 1 -1))
                ((2) (pmap-compare/number a b))
                ((3) (cond ((char<? a b) -1) ((char=? a b) 0) (else 1)))
                ((4) (pmap-compare/string a b))
                ((5) (pmap-compare/string (symbol->string a) (symbol->string b)))
                ((6) (pmap-compare/string (keyword->string a) (keyword->string b)))
                ((7) (cond
                       ((null? a) -1)
                       ((null? b) 1)
                       (else (let1 (c (pmap-compare (car a) (car b)))
                               (if (fx= c 0) (pmap-compare (cdr a) (cdr b)) c)))))
                ((8) (pmap-compare/sequence a b vector-length vector-ref pmap-compare))
                ((9) (pmap-compare/sequence a b bytevector-length bytevector-u8-ref fx-compare))
                (else (pmap-compare/sequence (record->vector a) (record->vector b)
                                             vector-length vector-ref pmap-compare)))))))))

  ;; ---------------------------------------------------------------------------------------
  ;; AVL trees (PM:21-98): '() is `Empty`, the vector #(l k v r h) is `Node (l, k, v, r, h)`.
  ;; ---------------------------------------------------------------------------------------

  ; (new) binds l, k, v, r and h to the fields of the node t in body, as OCaml's pattern
  ; `Node (l, k, v, r, h)` does.
  (define-syntax-rule (let-node ((l k v r h) t) body ...)
    (let1 (n t)
      (let ((l (vector-ref n 0))
            (k (vector-ref n 1))
            (v (vector-ref n 2))
            (r (vector-ref n 3))
            (h (vector-ref n 4)))
        body ...)))

  ; cf. PM:31-33 `height`.
  (define (height t) (if (null? t) 0 (vector-ref t 4)))

  ; cf. PM:35 `make`, named node here: `make` is the stem of the record constructors.
  (define (node l k v r) (vector l k v r (fx+ (fxmax (height l) (height r)) 1)))

  ; cf. PM:37-60 `bal`: rebalances when the heights of l and r differ by more than 2, not by
  ; 1, as upstream.  The `Empty -> assert false` cases of PM:47-48, 49, 57-58, 59 cannot
  ; happen: hl > hr + 2 makes l a node of height at least 3, whose child lr is a node when
  ; height ll < height lr; symmetrically for r.
  (define (bal l k v r)
    (let ((hl (height l))
          (hr (height r)))
      (cond
        ((fx> hl (fx+ hr 2))
          (let-node ((ll lk lv lr lh) l)
            (cond
              ((fx>= (height ll) (height lr)) (node ll lk lv (node lr k v r)))
              (else (let-node ((lrl lrk lrv lrr lrh) lr)
                      (node (node ll lk lv lrl) lrk lrv (node lrr k v r)))))))
        ((fx> hr (fx+ hl 2))
          (let-node ((rl rk rv rr rh) r)
            (cond
              ((fx>= (height rr) (height rl)) (node (node l k v rl) rk rv rr))
              (else (let-node ((rll rlk rlv rlr rlh) rl)
                      (node (node l k v rll) rlk rlv (node rlr rk rv rr)))))))
        (else (vector l k v r (fx+ (fxmax hl hr) 1))))))

  ; cf. PM:70-75 `delete_find_min'`; deviation 25 (PM:70): answers the list ((k . v) t'),
  ; not a tuple.  The empty tree raises (exn pmap empty) at loc (deviation 30).
  (define (delete-find-min/tree t loc)
    (cond
      ((null? t) (pmap-signal 'empty loc "empty map"))
      (else (let-node ((l k v r h) t)
              (cond
                ((null? l) (list (cons k v) r))
                (else (match/first (delete-find-min/tree l loc)
                        ((,b ,l*) (list b (bal l* k v r))))))))))

  ; cf. PM:81-86 `delete_find_max'`; deviation 25 (PM:81): answers the list ((k . v) t').
  (define (delete-find-max/tree t loc)
    (cond
      ((null? t) (pmap-signal 'empty loc "empty map"))
      (else (let-node ((l k v r h) t)
              (cond
                ((null? r) (list (cons k v) l))
                (else (match/first (delete-find-max/tree r loc)
                        ((,b ,r*) (list b (bal l k v r*))))))))))

  ; cf. PM:92-98 `merge`, of two trees whose keys are all ordered, those of t1 first.
  (define (merge t1 t2)
    (cond
      ((null? t1) t2)
      ((null? t2) t1)
      (else (match/first (delete-find-min/tree t2 'merge)
              (((,k . ,v) ,t2*) (bal t1 k v t2*))))))

  ;; ---------------------------------------------------------------------------------------
  ;; Maps (PMI; PM:100-199): a record holding the comparison procedure and the tree.
  ;; ---------------------------------------------------------------------------------------

  ; cf. PM:25-29 `type ('k, 'v) t = { cmp; map }`; pmap? is (new).
  (define-record pmap cmp tree)

  ; (new) shows a map as #<pmap ((k . v) ...)>, its bindings in ascending key order.
  (set-record-printer! pmap (λ (m port) (fprintf port "#<pmap ~s>" (pmap->alist m))))

  ; cf. PMI:35; PM:100 `create`.
  (define (pmap-create cmp) (make-pmap cmp '()))

  ; cf. PMI:29; PM:101 `empty`, ordered by pmap-compare.
  (define pmap-empty (pmap-create pmap-compare))

  ; cf. PMI:32; PM:103-104 `is_empty`.
  (define (pmap-empty? m) (null? (pmap-tree m)))

  ; cf. PMI:38; PM:106-118 `add`: on an equal key the node stores the new key x and keeps its
  ; height, without rebalancing (PM:110).
  (define (pmap-add x d m)
    (let1 (cmp (pmap-cmp m))
      (letrec ((loop (λ (t)
                       (cond
                         ((null? t) (vector '() x d '() 1))
                         (else (let-node ((l k v r h) t)
                                 (let1 (c (cmp x k))
                                   (cond
                                     ((fx= c 0) (vector l x d r h))
                                     ((fx< c 0) (bal (loop l) k v r))
                                     (else (bal l k v (loop r)))))))))))
        (make-pmap cmp (loop (pmap-tree m))))))

  ; cf. PMI:43-50; PM:120-132 `insert_with`: on an equal key the node stores the new key x and
  ; (f d v), the new value first (PM:124); otherwise it adds d.
  (define (pmap-insert-with f x d m)
    (let1 (cmp (pmap-cmp m))
      (letrec ((loop (λ (t)
                       (cond
                         ((null? t) (vector '() x d '() 1))
                         (else (let-node ((l k v r h) t)
                                 (let1 (c (cmp x k))
                                   (cond
                                     ((fx= c 0) (vector l x (f d v) r h))
                                     ((fx< c 0) (bal (loop l) k v r))
                                     (else (bal l k v (loop r)))))))))))
        (make-pmap cmp (loop (pmap-tree m))))))

  ; (new) the value a search answers for a missing key: a fresh pair, eq? to nothing else.
  (define pmap-miss (list 'pmap-miss))

  ; cf. PMI:52; PM:134-142 `find`; deviation 30 (PM:141): a missing key raises (exn pmap
  ; not-found), unless the optional default thunk is given, which is then called and its
  ; value answered.  The search answers a plain miss and the thunk is called in tail
  ; position, outside any handler (plan §3.4): (aux hansei)'s letlazy and memo pass thunks
  ; that choose, and a shift under a handler would capture a call/cc continuation.
  (define (pmap-find x m #!optional default)
    (let* ((cmp (pmap-cmp m))
           (found (let loop ((t (pmap-tree m)))
                    (cond
                      ((null? t) pmap-miss)
                      (else (let-node ((l k v r h) t)
                              (let1 (c (cmp x k))
                                (cond
                                  ((fx< c 0) (loop l))
                                  ((fx> c 0) (loop r))
                                  (else v)))))))))
      (cond
        ((not (eq? found pmap-miss)) found)
        (default (default))
        (else (pmap-signal 'not-found 'pmap-find "key not found" x)))))

  ; cf. PMI:56; PM:62-68 `find_min`: the binding (k . v) of the least key; deviation 25
  ; (PM:63): a pair, not a tuple; deviation 30 (PM:66): the empty map raises (exn pmap
  ; empty).
  (define (pmap-find-min m)
    (let loop ((t (pmap-tree m)))
      (cond
        ((null? t) (pmap-signal 'empty 'pmap-find-min "empty map"))
        (else (let-node ((l k v r h) t)
                (if (null? l) (cons k v) (loop l)))))))

  ; cf. PMI:61; PM:77-79 `delete_find_min`: the list ((k . v) m') of the binding of the least
  ; key and the map without it; deviation 25 (PM:77): a list, not a tuple; deviation 30
  ; (PM:76): the empty map raises (exn pmap empty).
  (define (pmap-delete-find-min m)
    (match/first (delete-find-min/tree (pmap-tree m) 'pmap-delete-find-min)
      ((,b ,t) (list b (make-pmap (pmap-cmp m) t)))))

  ; cf. PMI:62; PM:88-90 `delete_find_max`, as pmap-delete-find-min for the greatest key.
  (define (pmap-delete-find-max m)
    (match/first (delete-find-max/tree (pmap-tree m) 'pmap-delete-find-max)
      ((,b ,t) (list b (make-pmap (pmap-cmp m) t)))))

  ; cf. PMI:72; PM:144-151 `remove`: a map with the same bindings as m when x is unbound (a
  ; fresh record, as upstream).
  (define (pmap-remove x m)
    (let1 (cmp (pmap-cmp m))
      (letrec ((loop (λ (t)
                       (cond
                         ((null? t) '())
                         (else (let-node ((l k v r h) t)
                                 (let1 (c (cmp x k))
                                   (cond
                                     ((fx= c 0) (merge l r))
                                     ((fx< c 0) (bal (loop l) k v r))
                                     (else (bal l k v (loop r)))))))))))
        (make-pmap cmp (loop (pmap-tree m))))))

  ; cf. PMI:76-80; PM:153-161 `mem` and `exists`, one name for both.
  (define (pmap-member? x m)
    (let1 (cmp (pmap-cmp m))
      (let loop ((t (pmap-tree m)))
        (cond
          ((null? t) #f)
          (else (let-node ((l k v r h) t)
                  (let1 (c (cmp x k))
                    (or (fx= c 0) (loop (if (fx< c 0) l r))))))))))

  ; cf. PMI:83; PM:163-167 `iter`: (f k v) for each binding, in ascending key order.
  (define (pmap-for-each f m)
    (let loop ((t (pmap-tree m)))
      (unless (null? t)
        (let-node ((l k v r h) t)
          (loop l)
          (f k v)
          (loop r)))))

  ; (new) the tree of map with g applied to each node, after its left and then its right
  ; subtree (PM:173-175 and 182-184 bind l and r first, then build the node), as upstream.
  (define (pmap-map/tree g m)
    (letrec ((loop (λ (t)
                     (cond
                       ((null? t) '())
                       (else (let-node ((l k v r h) t)
                               (let* ((l* (loop l))
                                      (r* (loop r)))
                                 (vector l* k (g k v) r* h))))))))
      (make-pmap (pmap-cmp m) (loop (pmap-tree m)))))

  ; cf. PMI:90; PM:169-176 `map`: (f v) for each value, the keys and the shape unchanged.
  (define (pmap-map f m) (pmap-map/tree (λ (k v) (f v)) m))

  ; cf. PMI:97; PM:178-185 `mapi`: (f k v) for each binding.
  (define (pmap-map/key f m) (pmap-map/tree f m))

  ; cf. PMI:101; PM:187-192 `fold`: (f v acc) over the values, in ascending key order.
  (define (pmap-fold f m acc)
    (let loop ((acc acc)
               (t (pmap-tree m)))
      (cond
        ((null? t) acc)
        (else (let-node ((l k v r h) t)
                (loop (f v (loop acc l)) r))))))

  ; cf. PMI:108; PM:194-199 `foldi`: (f k v acc) over the bindings, in ascending key order;
  ; consing (aux hansei)'s explore answers a list in descending key order (I:28).
  (define (pmap-foldi f m acc)
    (let loop ((acc acc)
               (t (pmap-tree m)))
      (cond
        ((null? t) acc)
        (else (let-node ((l k v r h) t)
                (loop (f k v (loop acc l)) r))))))

  ; (new; cf. PMI:112-113, PM:202-229 `enum`, commented out upstream) the bindings of m as an
  ; alist ((k . v) ...) in ascending key order.
  (define (pmap->alist m)
    (let loop ((t (pmap-tree m))
               (acc '()))
      (cond
        ((null? t) acc)
        (else (let-node ((l k v r h) t)
                (loop l (cons (cons k v) (loop r acc))))))))

  ; (new; cf. PMI:116-119, PM:231-232 `of_enum`, commented out upstream) the map of the
  ; bindings of alist added from left to right, so a later binding of a key wins; ordered by
  ; cmp, pmap-compare by default as `?(cmp = compare)`.
  (define (alist->pmap alist #!optional (cmp pmap-compare))
    (let loop ((alist alist)
               (m (pmap-create cmp)))
      (match/first alist
        (() m)
        (((,k . ,v) . ,rest) (loop rest (pmap-add k v m))))))

  )
