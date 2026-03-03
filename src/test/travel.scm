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
    (let L ((l l0) (out '()))
        (cond
          ((null? l) (reverse out))
          (else (match/non-overlapping l
                    (((,u 0) . ,ll) (L ll out))
                    (((,u ,up) . ,ll) (> up 0) 
                        ⇒ (let1 (vv (probcc-uniform/either l1))
                            (match/non-overlapping vv
                                ((,v ,vp) (let1 (w (probcc-uniform/range 1 (min up vp)))
                                            (L  (cons `(,u ,(- up w)) ll) 
                                                (cons `(,u (→ ,w) ,v) out))))))))))))

(define (layer r n V l lst)
    (cond
        ((one? r) (reverse lst))
        (else (let* ((l* (V-for-trajectory n V '()))
                     (ll* (lmerge l l*))
                     (lst* (cons ll* lst))) 
                (layer (sub1 r) n V l* lst*)))))


(pp 
(probcc-normalize (probcc-reify/exact
    ; ((probcc-reify 5)
        (let* ((l (V-for-trajectory 5 V '()))
            (lst (layer 2 5 V l '())))
            lst
        )
        ; )
))
)

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
    (probcc-normalize (probcc-reify/exact (probcc-uniform/range 0 4)))
)

(foldr (λ (v lst) (cons v lst)) '() '(1 2 3))