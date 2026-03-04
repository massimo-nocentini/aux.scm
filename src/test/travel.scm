(import scheme (chicken base) (chicken sort) (chicken random) (chicken pretty-print) (chicken string) (chicken sort) 
    srfi-1 srfi-69 (aux base) (aux unittest) (aux hansei) (aux match))

(set-pseudo-random-seed! "561")

(define max-τ 10)
(define V '(a b c))

(define (travel t remaining path)
    (if (zero? remaining)
        (reverse path)
        (let* ((t* (probcc-uniform/range t max-τ))
               (vi (probcc-uniform (length V)))
               (v (list-ref V vi)))
            (probcc-when (not (eq? v (car path))) (travel t* (sub1 remaining) (cons v path)))
        )))

(define (V-for-trajectory n V lst)
  (cond 
    ((null? V) (reverse lst))
    ((null? (cdr V)) (V-for-trajectory 0 (cdr V) (cons (list (car V) n) lst)))
    (else (let* ((n* (probcc-uniform (add1 n)))
                 (lst* (cons `(,(car V) ,n*) lst)))
            (V-for-trajectory (- n n*) (cdr V) lst*)))))

(pp (probcc-normalize (probcc-reify/exact (V-for-trajectory 5 V '()))))

(define (lmerge l0 l1)
    (let L ((l0 l0) (l1 l1) (out '()))
        (match/non-overlapping l0
            (() (reverse out))
            (((,u 0) . ,ll) (L ll l1 out))
            (((,u ,up) . ,ll)
                (> up 0) ⇒ (match/non-overlapping (car l1)
                                ((,v ,vp) (let* ((m (min up vp))
                                                 (w (probcc-uniform/range (if (and (eq? u v) (> m 0)) m 1) m)))
                                            (L  (cons `(,u ,(- up w)) ll)
                                                (if (equal? w vp) (cdr l1) l1)
                                                (cons `(,u ,w ,v) out)))))))))

(define (layer r n V l lst)
    (cond
        ((one? r) (reverse lst))
        (else (let* ((l* (V-for-trajectory n V '()))
                     (ll* (lmerge l l*))
                     (lst* (cons ll* lst))) 
                (layer (sub1 r) n V l* lst*)))))

(define *people* 2)
(define *layers* 4)

;(: L [edge] -> [[edge]] -> [[edge]])
(define (L l0 ll)
    (cond
        ((null? ll) (map list l0))
        (else (let1 (paths (L (car ll) (cdr ll)))
                (append-map (λ (x) (let1 (ps (filter (λ (p) (and (eq? (third x) (caar p)) (<= (second x) (cadar p)))) paths))
                                            (map (λ (p) (cons x p)) ps)))
                    l0))
                )))

(pp 
(probcc-normalize (probcc-reify/exact
    
        (let* ((l (V-for-trajectory *people* V '()))
               (list-of-edges (layer *layers* *people* V l '())))
            ; list-of-edges
            (L (car list-of-edges) (cdr list-of-edges))
            )))
)

'(((a 2 a)) ((a 1 a) (a 1 b)) ((a 1 a) (b 1 b)))
; I expect the following from the recursive call:
'(((a 1 a) (a 1 a)) ((a 1 b) (b 1 b)))
; and the final call should produce:
'(((a 2 a) (a 1 a) (a 1 a)) ((a 2 a) (a 1 b) (b 1 b)))

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
    (probcc-normalize (probcc-reify/exact (probcc-uniform/range 4 4)))
)

(foldr (λ (v lst) (cons v lst)) '() '(1 2 3))