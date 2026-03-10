
(import scheme (chicken base) (chicken sort) (chicken random) (chicken pretty-print) (chicken string) (chicken sort) 
    srfi-1 srfi-69 (aux base) (aux unittest) (aux hansei) (aux match))

(set-pseudo-random-seed! "561")

(define max-τ 11)
(define *people* 3)
(define *τ* 3)
(define V '(a b c))

#;(define (travel t remaining path)
    (if (zero? remaining)
        (reverse path)
        (let* ((t* (probcc-uniform/range t max-τ))
               (vi (probcc-uniform (length V)))
               (v (list-ref V vi)))
            (probcc-when (not (eq? v (car path))) (travel t* (sub1 remaining) (cons v path)))
        )))

(define (V-for-trajectory n V lst)
  (match/non-overlapping V
    (() (zero? n) ⇒ (reverse lst))
    ((,v) (V-for-trajectory 0 '() (cons (list v n) lst)))
    ((,v . ,v*) (not (null? v*)) ⇒ (let* ((n* (probcc-uniform (add1 n)))
                                          (lst* (cons `(,v ,n*) lst)))
                                        (V-for-trajectory (- n n*) v* lst*)))))

(pp (probcc-normalize (probcc-reify/exact (V-for-trajectory *people* V '()))))

#;(define (lmerge l0 l1)
    (let L ((l0 l0) (l1 l1) (out '()))
        (match/non-overlapping l0
            (() (reverse out))
            (((_ 0) . ,ll) (L ll l1 out))
            (((,u ,up) . ,ll)
                (> up 0) ⇒ (match/non-overlapping (car l1)
                                ((_ 0) (L l0 (cdr l1) out))
                                ((,v ,vp) 
                                    (> vp 0) ⇒ (let* ((m (min up vp))
                                                      (w (probcc-uniform/range 1 (add1 m))))
                                                    (L  (cons `(,u ,(- up w)) ll)
                                                        (cons `(,v ,(- vp w)) (cdr l1))
                                                        (cons `(,u ,w ,v) out)))))))))

(define (lmerge l0 l1)
    (let L ((l0 l0) (l1 l1) (out '()))
        (match/non-overlapping l0
            (() (reverse out))
            (((_ 0) . ,ll) (L ll l1 out))
            (((,u ,up) . ,ll)
                (> up 0) ⇒ (let* ((w (probcc-uniform (length l1)))
                                  (prefix (take l1 w)))
                                (match1/non-overlapping (((,v ,vp) . ,suffix) (drop l1 w))
                                    (let1 (m (probcc-uniform/range (if (eq? u v) up 1) (add1 up)))
                                        (let1 (edge `(,u ,m ,v))
                                            (probcc-when (and (or (null? out) (not (equal? edge (car out)))) (<= m vp))
                                                (L  (cons `(,u ,(- up m)) ll)
                                                    (append prefix (cons `(,v ,(- vp m)) suffix))
                                                    (cons edge out)))))))))))

(define (layer r n V l lst lkeep?)
    (probcc-when (lkeep? (- *τ* r) l)
        (cond
            ((one? r) (reverse lst))
            (else (let* ((l* (V-for-trajectory n V '()))
                        (ll* (lmerge l l*))
                        (lst* (cons ll* lst))) 
                    (layer (sub1 r) n V l* lst* lkeep?))))))

(define (cons/λ x) (λ (y) (cons x y)))
(define (snoc/λ y) (λ (x) (cons x y)))

(define (splash tup multi) 
    (match1/non-overlapping ((,s ,n ,d) tup)
        (let1 (p (list s d)) (map (λ_ p) (iota (if multi n 1))))))

(define (⊗ r1 r2)
    (append-map (λ (re)
                    (match/non-overlapping re 
                        ((,s ,d) (map (cons/λ s) (filter (λ (se) (eq? d (car se))) r2))))) 
                r1))

(splash '(a 3 b) #t)

(⊗ (splash '(a 1 b) #f) (splash '(b 2 c) #t))
(append-map (λ (l) (splash l #t)) '((a 2 b) (b 1 b)))
(⊗ (append-map (λ (l) (splash l #f)) '((a 1 b) (b 1 b))) (splash '(b 2 c) #t))

;(: L [edge] -> [[edge]] -> [[edge]])
(define (L l0 ll)
    (cond
        ((null? ll) (list (append-map (λ (l) (splash l #t)) l0)))
        (else (let* ((paths (L (car ll) (cdr ll)))
                     (p (probcc-uniform/either paths))
                     (paths* (map (λ (l) (⊗ (splash l #f) p)) l0))
                     #;(paths* (map (λ (e) (⊗ (splash e #f) p)) l0)))
                #;(list l0 p)
                (let1 (lst (probcc-uniform/either paths*))
                    (probcc-when (= *people* (length lst)) lst))))))

 (define (lkeep? r l)
            (let1 (l* (cons r l))
              (cond
                ((equal? l* '(0 (a 1) (b 2) (c 0))) #t)
                ((equal? l* '(1 (a 0) (b 2) (c 1))) #t)
                ((equal? l* '(2 (a 2) (b 1) (c 0))) #t)
                (else #f))))

(match/non-overlapping '(0 a b d)
    ((0 a b c) #t)
    (,,pair? #f))

(pp (probcc-normalize (probcc-reify/exact

        (let* ((l (V-for-trajectory *people* V '()))
               (list-of-edges (layer *τ* *people* V l '() lkeep?)))
             list-of-edges
        ;    (L (car list-of-edges) (cdr list-of-edges))
            ))

))

'(((a 2 a)) ((a 1 a) (a 1 b)) ((a 1 a) (b 1 b)))
; I expect the following from the recursive call:
'(((a 1 a) (a 1 a)) ((a 1 b) (b 1 b)))
; and the final call should produce:
'(((a 2 a) (a 1 a) (a 1 a)) ((a 2 a) (a 1 b) (b 1 b)))

'(((a 2 a) (b 1 a)) ((a 1 a) (a 2 b))) ||| il caso base (quello piu' a destra) produce '((a a) (a b) (a b))
; --------------------------
'((a a) (b b) (a b))
'((a a) (a b) (b a))
'(((a a a) (a a b) (b a b))
  ((a a b) (a a b) (b a a)))


(define (travel t remaining path)
    (if (or (zero? remaining) (>= t max-τ))
        (reverse path)
        (let* (#;(t* (probcc-uniform/range (add1 t) max-τ))
               (t* (add1 t))
               (vi (probcc-uniform (length V)))
               (v (list-ref V vi)))
            (probcc-when (not (eq? v (second (car path)))) (travel t* (sub1 remaining) (cons (list t* v) path)))
        )))

(pp
    (probcc-normalize (probcc-reify/exact (travel -1 5 (list (list -1 (gensym))))))
)

(pp
    (probcc-normalize (probcc-reify/exact (probcc-uniform/range 4 5)))
)

(foldr (λ (v lst) (cons v lst)) '() '(1 2 3))