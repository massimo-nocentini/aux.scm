
(import scheme (chicken base) (chicken sort) (chicken random) (chicken pretty-print) (chicken string) (chicken sort) 
    srfi-1 srfi-69 (aux base) (aux unittest) (aux hansei))

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
  (match/first (cons n V)
    ((0) (reverse lst))
    ((0 ,v . ,V*) (V-for-trajectory 0 V* (cons (list v 0) lst)))
    ((_ ,v) (V-for-trajectory 0 '() (cons (list v n) lst)))
    ((_ ,v . ,V*) (let* ((n* (probcc-uniform/range 0 (add1 n)))
                         (lst* (cons `(,v ,n*) lst)))
                    (V-for-trajectory (- n n*) V* lst*)))))

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

#;(define (lmerge l0 l1)
    (let L ((l0 l0) (l1 l1) (out '()))
        (match/non-overlapping l0
            (() (reverse out))
            (((_ 0) . ,ll) (L ll l1 out))
            (((,u ,up) . ,ll)
                (> up 0) ⇒ (let1 (w (probcc-uniform (length l1)))
                                (match/non-overlapping (drop l1 w)
                                    (((_ 0) . ,suffix) (L l0 suffix out))
                                    (((,v ,vp) . ,suffix) 
                                        (> vp 0) ⇒ (let* ((m* (min up vp))
                                                          (m (probcc-uniform/range (if (eq? u v) m* 1) (add1 m*)))
                                                          (edge `(,u ,m ,v)))
                                                    (probcc-when (or (null? out) (not (equal? edge (car out))))
                                                        (L  (cons `(,u ,(- up m)) ll)
                                                            (append (take l1 w) (cons `(,v ,(- vp m)) suffix))
                                                            (cons edge out)))))))))))


(define (lmerge l0 l1)
    (let L ((l0 l0) (l1 l1) (out '()))
        (match/first l0
            (() (reverse out))
            (((_ 0) . ,ll) (L ll l1 out))
            (((,u ,up) . ,ll) (let1 (w (probcc-uniform (length l1)))
                                (match1/first (((,v ,vp) . ,suffix) (drop l1 w))
                                    (let* ((m (probcc-uniform/range (if (eq? u v) up 1) (add1 up)))
                                           (edge `(,u ,m ,v)))
                                        (probcc-when (and (<= m vp) (or (null? out) (not (equal? edge (car out)))))
                                            (L  (cons `(,u ,(- up m)) ll)
                                                (append (take l1 w) (cons `(,v ,(- vp m)) suffix))
                                                (cons edge out))))))))))


(define (layer r n V l lst lkeep? ekeep?)
    (let1 (r* (- *τ* r))
        (probcc-when (lkeep? r* l)
            (cond
                ((one? r) (reverse lst))
                (else (let* ((l* (V-for-trajectory n V '()))
                            (ll* (lmerge l l*))
                            (lst* (cons ll* lst))) 
                        (probcc-when (ekeep? r* ll*)
                            (layer (sub1 r) n V l* lst* lkeep? ekeep?))))))))

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

(define (M edge edges out)
    (match/first (list edge edges)
        (((_ 0 _) _) (cons out edges))
        ((_ ((_ 0 _) . ,edges*)) (M edge edges* out))
        (((,s ,n ,d) ((,ss ,nn ,dd) . ,edges*))
            (equal? d ss) ⇒ (M `(,s ,(sub1 n) ,d) (cons `(,ss ,(sub1 nn) ,dd) edges*) (cons `(,s ,d ,dd) out)))
        ((_ (,edge* . ,edges*)) (M edge (append edges* (list edge*)) out))))

(M '(a 1 c) '((b 2 a) (c 1 b)) '())
(match1/non-overlapping ((((,a . ,a*))) '(((b c)))) a*)

#|
(((V (((a 1 c) (b 2 b)) ((b 2 a) (c 1 b)))) 0.642857142857143)
 ((V (((a 1 c) (b 2 b)) ((b 1 a) (b 1 b) (c 1 a)))) 0.214285714285714)
 ((V (((a 1 b) (b 1 c) (b 1 b)) ((b 2 a) (c 1 b)))) 0.107142857142857)
 ((V (((a 1 b) (b 1 c) (b 1 b)) ((b 1 a) (b 1 b) (c 1 a))))
|#

(define (L l0 ll bin)
    (cond
        ((null? ll) l0)
        (else (let1 (edges (L (car ll) (cdr ll) bin))
            (foldr  (λ (edge edges*) (match1/first ((,out . ,edges**) (M edge edges* '())) (push! out bin) edges**))
                    edges l0)))))

 (define (lkeep? r l)
            (let1 (l* (cons r l))
              (cond
                ((equal? l* '(0 (a 1) (b 2) (c 0))) #t)
                ((equal? l* '(1 (a 0) (b 2) (c 1))) #t)
                ((equal? l* '(2 (a 2) (b 1) (c 0))) #t)
                (else #f))))

 (define (ekeep? r edges) #t)

(match/non-overlapping '(0 a b d)
    ((0 a b c) #t)
    (,,pair? #f))

(define (back-home? l0 l1) (equal? (map (λ (l) (take l 2)) l0) (map (o reverse cdr) l1)))

(pp (probcc-normalize (probcc-reify/exact

        (let* ((l (V-for-trajectory *people* V '()))
               (list-of-edges (layer *τ* *people* V l '() lkeep? ekeep?))
               (bin '()))
               list-of-edges
            #;(probcc-when #t #;(back-home? (first list-of-edges) (last list-of-edges))
                list-of-edges)
        ;    (L (car list-of-edges) (cdr list-of-edges) bin)
        ;    bin
            ))
))

#|

(((V ((a c b) (b b a) (b b a))) 0.642857142857143)
 ((V ((a c a) (b b b) (b b a))) 0.214285714285714)
 ((V ((a b a) (b c b) (b b a))) 0.107142857142857)
 ((V ((a b a) (b c a) (b b a))) 0.0357142857142857))

|#

(match/first '(0 a b d)
    (((0 a b . ,c) as all) (append c all))
    (else #f))

(match/first '(0 a b d)
    ((,c as all) (cons c all))
    (else #f))

(match/first #(0 a b d)
    (((0 a b ,c) as all) (list c all))
    (else #f))


(match/first '(0 a b d)
    ((#(0 a b ,c) as all) (list c all))
    (else #f))

(match/first #(0 a b d)
    (((0 a b . ,c) as all) (list c all))
    (else #f))

(define-record re a b c)

(match/first (make-re 0 1 2)
    ((re ,a ,b ,c) (list b))
    (else #f))

(match/first #(0 a b d)
    ((#(0 a c ,c)) (list c all))
    )

'#,(re 0 1 2)