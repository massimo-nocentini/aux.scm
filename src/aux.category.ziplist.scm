

(module (aux category ziplist) *

  (import scheme (chicken base) (only srfi-1 take every iota) (aux base))

  ; the pair at index i of lst, #f when lst is shorter: a pair is never #f, so a
  ; stored #f is not mistaken for absence (the witness of `length/>?` is that
  ; same tail pair). It stops at i, so it terminates on the circular lists built by `return`.
  (define (list-tail/or-false lst i)
    (cond
      ((null? lst) #f)
      ((zero? i) lst)
      (else (list-tail/or-false (cdr lst) (sub1 i)))))

  ; the length of the prefix and of the cycle of a circular list, as a pair;
  ; #f for a proper list (Floyd's tortoise and hare).
  (define (list-rho lst)
    (let meet ((t lst) (h lst))
      (cond
        ((or (null? h) (null? (cdr h))) #f)
        ((eq? (cdr t) (cddr h))
         (let* ((t (cdr t))
                (start (let S ((a lst) (b t) (n 0)) (if (eq? a b) n (S (cdr a) (cdr b) (add1 n)))))
                (period (let P ((b (cdr t)) (n 1)) (if (eq? b t) n (P (cdr b) (add1 n))))))
           (cons start period)))
        (else (meet (cdr t) (cddr h))))))

  ; the diagonal of g over a circular m whose rho is (start . period), with g
  ; applied once per pair of m. Walking the cycle, the diagonal stops at the first
  ; (g v) too short to reach it, as it surely does when one of the cycle is
  ; finite; when they are all circular, past the longest prefix among m and them
  ; it repeats with the lcm of the periods, so one period is built and closed.
  (define (>>=/circular m start period g)
    (let* ((images (list->vector (map g (take m (+ start period)))))
           (at (λ (i) (vector-ref images (if (< i start) i (+ start (modulo (- i start) period))))))
           (rhos (map (λ (k) (list-rho (vector-ref images k))) (iota period start)))
           (from (and (every pair? rhos) (apply max start (map car rhos))))
           (cycle (and from (apply lcm period (map cdr rhos))))
           (bound (and from (+ from cycle)))
           (d (let D ((i 0) (acc '()))
                (cond
                  ((eqv? i bound) (reverse acc))
                  ((list-tail/or-false (at i) i) => (λ (t) (D (add1 i) (cons (car t) acc))))
                  (else (reverse acc))))))
      (when (and bound (= (length d) bound))
        (set-cdr! (list-tail d (sub1 bound)) (list-tail d from)))
      d))

  ; Monad instance
  ;
  ; `return` repeats its value forever, as a one-element circular list, so that a
  ; pure value zips with a list of any length. A do/monad whose clauses are all
  ; pure therefore yields a circular list too: `write`, `display` and `print` never
  ; return on it (only the csi REPL prints it, as #0=(12 . #0#)). Look at such a
  ; value through srfi-1's `take`, or zip it with a finite list first.
  (define (return x) (list/circular x))
  (define (>>= m g)
    (match/first m
      ((,l . ,,m) (g l))
      ; the diagonal stops at the first (g v) too short to reach it, as zipWith
      ; does: skipping it would slide the later elements out of their index.
      (else (match/first (list-rho m)
              ((,start . ,period) (>>=/circular m start period g))
              (else (let D ((m* m) (i 0) (acc '()))
                      (cond
                        ((null? m*) (reverse acc))
                        ((list-tail/or-false (g (car m*)) i) => (λ (t) (D (cdr m*) (add1 i) (cons (car t) acc))))
                        (else (reverse acc)))))))))

  (define (fail . args) '())
)

(import (aux category monad))

(module (aux category monad ziplist) = ((aux category monad) (aux category ziplist)))

#|
(import  (aux category ziplist) (aux category monad ziplist) (only srfi-1 take))

(define l (do/monad
  (← x ,3)
  (← y ,4)
  (← z ,5)
  ,(+ x y z)))

(take l 3) ; => (12 12 12), since `l` is circular: (write l) never returns

(do/monad
  (← x '(1 2 3))
  (← y '(a b))
  (← z '(c c*))
  (← r '(d e f g))
  ,(list x y z r))

(<*> (list add1 sub1 sub1) (list 1 2 3)) ; => (2 1 2)

|#
