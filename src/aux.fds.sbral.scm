
;; Skew binary random-access list.
;;
;; The structure is stored as a list of weighted trees. Each element of the
;; outer list has shape `(size root-subtree)`, where `size` is the number of
;; values stored in the tree and the tree is either a leaf `(value)` or a node
;; `(value left right)`. The front of the sequence is always at the root of the
;; first tree, which gives constant-time `cons/sbral`, `car/sbral`, and
;; `cdr/sbral`, while indexed access and updates walk a tree by weight.
;;
;; Public API:
;; - `empty/sbral` creates an empty sequence.
;; - `cons/sbral`, `car/sbral`, and `cdr/sbral` behave like their list
;;   counterparts on the front of the sequence.
;; - `sbral-ref` and `update/sbral` provide positional lookup and replacement.
;; - `list->sbral` and `sbral->list` convert to and from ordinary lists.
;; - `length/sbral`, `map/sbral`, `filter/sbral`, `exists?/sbral`, and
(module (aux fds sbral) *

  (import scheme (chicken base) (chicken fixnum) (aux base))

  (define empty/sbral '())

  (define (cons/sbral v sbral)
    (match/first sbral
      ((((,x . ,xtree) (,y . ,ytree) . ,sbral*) ⊣ (= x y)) `((,(+ 1 x y) ,v ,xtree ,ytree) . ,sbral*))
      (else `((1 ,v) . ,sbral))))

  (define (sbral-tree-leaf? tree) (null? (cdr tree)))
  (define (sbral-tree-node? tree) (pair? (cdr tree)))
  (define (sbral-tree-value tree) (car tree))
  (define (sbral-tree-left tree) (cadr tree))
  (define (sbral-tree-right tree) (caddr tree))

  (define (car/sbral sbral)
    (match1/first ((,size . ,tree) (car sbral))
      (cond
        ((or (and (= size 1) (sbral-tree-leaf? tree)) (sbral-tree-node? tree)) (sbral-tree-value tree))
        (else (error "car/sbral: not a valid sbral")))))

  (define (cdr/sbral sbral)
    (match1/first (((,size . ,tree) . ,sbral*) sbral)
      (cond
        ((and (= size 1) (sbral-tree-leaf? tree)) sbral*)
        ((sbral-tree-node? tree) (let* ((w (quotient size 2))
                                        (f (cons w (sbral-tree-left tree)))
                                        (s (cons w (sbral-tree-right tree))))
                                    `(,f ,s . ,sbral*)))
        (else (error "cdr/sbral: not a valid sbral")))))

  (define (sbral-tree-lookup w i tree)
    (cond
      ((and (= w 1) (= i 0) (sbral-tree-leaf? tree)) (sbral-tree-value tree))
      ((and (= i 0) (sbral-tree-node? tree)) (sbral-tree-value tree))
      ((sbral-tree-node? tree) (let1 (whalf (quotient w 2))
                                     (cond
                                       ((<= i whalf) (sbral-tree-lookup whalf (- i 1) (sbral-tree-left tree)))
                                       (else (sbral-tree-lookup whalf (- i 1 whalf) (sbral-tree-right tree))))))
      (else (error "sbral-tree-lookup: not a valid sbral"))))

  (define (sbral-tree-update w i y tree)
    (cond
      ((and (= w 1) (= i 0) (sbral-tree-leaf? tree)) (list y))
      ((and (= i 0) (sbral-tree-node? tree)) (list y (sbral-tree-left tree) (sbral-tree-right tree)))
      ((sbral-tree-node? tree) (let1 (whalf (quotient w 2))
                                     (cond
                                       ((<= i whalf) (list (sbral-tree-value tree)
                                                           (sbral-tree-update whalf (- i 1) y (sbral-tree-left tree))
                                                           (sbral-tree-right tree)))
                                       (else (list (sbral-tree-value tree)
                                                   (sbral-tree-left tree)
                                                   (sbral-tree-update whalf (- i 1 whalf) y (sbral-tree-right tree)))))))
      (else (error "sbral-tree-update: not a valid sbral"))))

  (define (sbral-ref sbral i)
    (match/non-overlapping sbral
      ((((,size . ,tree) . _) ⊣ (< -1 i size)) (sbral-tree-lookup size i tree))
      ((((,size . _) . ,sbral*) ⊣ (<= size i)) (sbral-ref sbral* (- i size)))))

  (define (update/sbral i y sbral)
    (match1/first (((,size . ,tree) . ,sbral*) sbral)
      (cond
        ((< -1 i size) `((,size . ,(sbral-tree-update size i y tree)) . ,sbral*))
        (else `(,(car sbral) . ,(update/sbral (- i size) y sbral*))))))

  (define (foldr/sbral f init sbral)
    (let loop ((l (sub1 (length/sbral sbral))) (out init))
      (if (< l 0) out (loop (sub1 l) (f l (sbral-ref sbral l) out)))))

  (define (length/sbral sbral) (foldr (λ (each l) (+ (car each) l)) 0 sbral))
  (define (list->sbral lst) (foldr cons/sbral empty/sbral lst))
  (define (sbral->list sbral) (foldr/sbral (λ (i each lst) (cons each lst)) '() sbral))
  
  (define (map/sbral f sbral)
    (foldr/sbral (λ (i each sbral*) (cons/sbral (f i each) sbral*)) empty/sbral sbral))
  
  (define (filter/sbral pred? sbral)
    (foldr/sbral (λ (i each sbral*) (if (pred? i each) (cons/sbral each sbral*) sbral*)) empty/sbral sbral))
  
  (define (exists?/sbral pred? sbral)
    (foldr/sbral (λ (i each exists) (or exists (pred? i each))) #f sbral))

)