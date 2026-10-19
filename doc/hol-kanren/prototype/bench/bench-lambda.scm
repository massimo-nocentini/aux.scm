(import scheme (chicken base) (chicken time) srfi-1 (aux base) (aux stream) (aux kanren micro) (aux kanren lambda))

(define-dynamic-relation (typeof° m t)
  (cond° ((fresh° (M N B) (=° m `(app ,M ,N)) (typeof° M `(arr ,B ,t)) (typeof° N B)))
         ((fresh° (R A B) (=° m `(abs ,R)) (=° t `(arr ,A ,B)) (∀° (x) (⊃° ((typeof° x A)) (typeof° (@ R x) B)))))))
(define-dynamic-relation (copy° m n)
  (cond° ((fresh° (M N P Q) (=° m `(app ,M ,N)) (=° n `(app ,P ,Q)) (copy° M P) (copy° N Q)))
         ((fresh° (M N) (=° m `(abs ,M)) (=° n `(abs ,N)) (∀° (x) (⊃° ((copy° x x)) (copy° (@ M x) (@ N x))))))))

; the Church numeral n, λf.λx. f (f ... (f x)); n nested binders λx1 ... λxn. x1, both in λ-tree syntax.
(define (church n) `(abs ,(ƛ (f) `(abs ,(ƛ (x) (let L ((k n)) (if (zero? k) x `(app ,f ,(L (sub1 k))))))))))
(define (nest n) (let L ((k n) (acc '())) (if (zero? k) (car (reverse acc)) `(abs ,(μkanren-abstract 'x (λ (x) (L (sub1 k) (cons x acc))))))))

(define (timeit label thunk)
  (let loop ((k 0) (best 1e9) (r #f))
    (if (= k 3) (begin (print label "\t" r "\t" best " ms"))
        (let* ((t0 (current-process-milliseconds)) (r (thunk)) (t1 (current-process-milliseconds)))
          (loop (add1 k) (min best (- t1 t0)) r)))))

(define (RUN f) (°->list #f (fresh° (q) (f q))))
(timeit "typeof church 400      " (λ () (length (RUN (λ (q) (typeof° (church 400) q))))))
(timeit "copy church 400        " (λ () (length (RUN (λ (q) (copy° (church 400) q))))))
(timeit "typeof nest 150        " (λ () (length (RUN (λ (q) (typeof° (nest 150) q))))))
(timeit "copy nest 150          " (λ () (length (RUN (λ (q) (copy° (nest 150) q))))))
(timeit "typeof backwards 30    " (λ () (length (°->list #f (take° 30 (fresh° (q m t) (=° q (list m t)) (typeof° m t)))))))
(timeit "F x1..x50 = g x50..x1  "
        (λ () (length (°->list/datum (fresh° (q)
                (let loop ((i 50) (xs '()))
                  (if (zero? i) (=° (μkanren-apply q xs) (cons 'g (reverse xs)))
                      (∀ª 'x (λ (x) (loop (sub1 i) (cons x xs)))))))))))
