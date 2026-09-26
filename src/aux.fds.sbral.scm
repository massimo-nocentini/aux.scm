
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
      (((,x . ,xtree) (,,x . ,ytree) . ,sbral*) `((,(+ 1 x x) ,v ,xtree ,ytree) . ,sbral*))
      (else `((1 ,v) . ,sbral))))

  (define (car/sbral sbral) 
    (match/first sbral 
      (((_ ,v . _) . _) v)
      (else (error "car/sbral: not a valid sbral"))))

  (define (cdr/sbral sbral)
    (match/first sbral
      (((1 _) . ,sbral*) sbral*)
      (((,s _ ,α ,β) . ,sbral*) (let1 (w (quotient s 2)) `((,w . ,α) (,w . ,β) . ,sbral*)))
      (else (error "cdr/sbral: not a valid sbral"))))

  ; fixed-arity procedures: this is the innermost loop of the substitution walk, so the
  ; rest list that `λ-match/first` would cons and destructure at every level is avoided.
  ; Sizes and indices are fixnums by construction, hence the unchecked `fx` arithmetic:
  ; the public entries `sbral-ref` and `update/sbral` check that the caller's index is one.
  (define (sbral-tree-lookup w i tree)
    (match/first tree
      (((,v . _) ⊣ (fx= i 0)) v)
      ((,v ,α ,β) (let1 (w/2 (fxshr w 1))
                    (cond
                      ((fx<= i w/2) (sbral-tree-lookup w/2 (fx- i 1) α))
                      (else (sbral-tree-lookup w/2 (fx- (fx- i 1) w/2) β)))))
      (else (error "sbral-tree-lookup: not a valid sbral"))))

  (define (sbral-tree-update w i y tree)
    (match/first tree
      (((_ . ,αβ) ⊣ (fx= i 0)) `(,y . ,αβ))
      ((,v ,α ,β) (let1 (w/2 (fxshr w 1))
                    (cond
                      ((fx<= i w/2) `(,v ,(sbral-tree-update w/2 (fx- i 1) y α) ,β))
                      (else `(,v ,α ,(sbral-tree-update w/2 (fx- (fx- i 1) w/2) y β))))))
      (else (error "sbral-tree-update: not a valid sbral"))))

  (define (sbral-ref sbral i)
    (match/first sbral
      ((((,size . ,tree) . _) ⊣ (and (fixnum? i) (fx>= i 0) (fx< i size))) (sbral-tree-lookup size i tree))
      ((((,size . _) . ,sbral*) ⊣ (and (fixnum? i) (fx<= size i))) (sbral-ref sbral* (fx- i size)))
      (else (error "sbral-ref: index out of bounds" i))))

  (define (update/sbral i y sbral)
    (match/first sbral
      ((((,size . ,tree) . ,sbral*) ⊣ (and (fixnum? i) (fx>= i 0) (fx< i size)))
       `((,size . ,(sbral-tree-update size i y tree)) . ,sbral*))
      (((,entry . ,sbral*) ⊣ (and (fixnum? i) (fx<= (car entry) i))) `(,entry . ,(update/sbral (fx- i (car entry)) y sbral*)))
      (else (error "update/sbral: index out of bounds" i))))

  ; folds the tree of weight w whose root sits at index i, right subtree first, so that
  ; `f` sees the indices in decreasing order: O(w) instead of one `sbral-ref` per element.
  (define (sbral-tree-foldr f w i tree out)
    (match/first tree
      ((,v) (f i v out))
      ((,v ,α ,β) (let1 (w/2 (fxshr w 1))
                    (f i v (sbral-tree-foldr f w/2 (fx+ i 1) α (sbral-tree-foldr f w/2 (fx+ (fx+ i 1) w/2) β out)))))
      (else (error "sbral-tree-foldr: not a valid sbral"))))

  (define (foldr/sbral f init sbral)
    (let loop ((sbral sbral) (i 0))
      (match/first sbral
        (() init)
        (((,size . ,tree) . ,sbral*) (sbral-tree-foldr f size i tree (loop sbral* (fx+ i size))))
        (else (error "foldr/sbral: not a valid sbral")))))

  (define (length/sbral sbral) (foldr (λ (each l) (+ (car each) l)) 0 sbral))
  (define (list->sbral lst) (foldr cons/sbral empty/sbral lst))
  (define (sbral->list sbral) (foldr/sbral (λ (i each lst) (cons each lst)) '() sbral))
  
  (define (map/sbral f sbral)
    (foldr/sbral (λ (i each sbral*) (cons/sbral (f i each) sbral*)) empty/sbral sbral))
  
  (define (filter/sbral pred? sbral)
    (foldr/sbral (λ (i each sbral*) (if (pred? i each) (cons/sbral each sbral*) sbral*)) empty/sbral sbral))
  
  ; the first truthy `(pred? i each)` in the fold's order (last index first); the walk stops there.
  (define (exists?/sbral pred? sbral)
    (call-with-current-continuation
      (λ (return)
        (foldr/sbral (λ (i each _) (let1 (x (pred? i each)) (if x (return x) #f))) #f sbral))))

)