
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

  (define (car/sbral sbral) 
    (match/first sbral 
      (((_ ,v . _) . _) v)
      (else (error "car/sbral: not a valid sbral"))))

  (define (cdr/sbral sbral)
    (match/first sbral
      (((1 _) . ,sbral*) sbral*)
      (((,s _ ,α ,β) . ,sbral*) (let1 (w (quotient s 2)) `((,w . ,α) (,w . ,β) . ,sbral*)))
      (else error "cdr/sbral: not a valid sbral")))

  (define sbral-tree-lookup
    (λ-match/first
      ((_   0 (,v . _)) v)
      ((,w ,i (,v ,α ,β)) (let1 (w/2 (quotient w 2))
                            (cond
                              ((<= i w/2) (sbral-tree-lookup w/2 (- i 1) α))
                              (else (sbral-tree-lookup w/2 (- i 1 w/2) β)))))
      (else (error "sbral-tree-lookup: not a valid sbral"))))

  (define sbral-tree-update
    (λ-match/first
      ((_   0 ,y (_ . ,αβ)) `(,y . ,αβ))
      ((,w ,i ,y (,v ,α ,β))  (let1 (w/2 (quotient w 2))
                                (cond
                                  ((<= i w/2) `(,v ,(sbral-tree-update w/2 (- i 1) y α) ,β))
                                  (else `(,v ,α ,(sbral-tree-update w/2 (- i 1 w/2) y β))))))
      (else (error "sbral-tree-update: not a valid sbral"))))    

  (define (sbral-ref sbral i)
    (match/first sbral
      ((((,size . ,tree) . _) ⊣ (< -1 i size)) (sbral-tree-lookup size i tree))
      ((((,size . _) . ,sbral*) ⊣ (<= size i)) (sbral-ref sbral* (- i size)))
      (else (error "sbral-ref: index out of bounds" i))))

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