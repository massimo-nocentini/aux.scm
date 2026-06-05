

(module (aux category difflist) *

  (import scheme (chicken base) (chicken pretty-print) (only srfi-1 append-map) (aux base))

  (define-record category-difflist f)

  (set-record-printer! category-difflist
    (λ (d port)
      (let ((f (category-difflist-f d))
            (D (display/port port)))
        (D "⸨")
        (map (μ v (D " ") (D v)) (f '()))
        (D " ⸩"))))

  ; Monad instance
  (define (return x) (make-category-difflist (λ (x*) (cons x x*))))
  (define (>>= m g)
    (let* ((f (category-difflist-f m))
           (xs (map g (f '()))))
      (make-category-difflist (λ (xs*) (foldr (λ (m* lst) ((category-difflist-f m*) lst)) xs* xs)))))

  (define (fail . args) (make-category-difflist (λ (xs) '())))

  ; Monoid instance
  (define mempty (make-category-difflist (λ (lst) lst)))
  (define (mappend l1 l2) (make-category-difflist (o (category-difflist-f l1) (category-difflist-f l2))))

  (define (mlog x) (make-category-difflist (λ (xs) (cons `(got ,x) xs))))

  (define (list->difflist l1) (make-category-difflist (λ (l2) (append l1 l2))))
  (define (difflist->list dl) ((category-difflist-f dl) '()))

)

(import (aux category monad) (aux category writer))

(module (aux category monad difflist) = ((aux category monad) (aux category difflist)))
(module (aux category writer difflist) = ((aux category writer) (aux category difflist)))
(module (aux category monad writer difflist) = ((aux category monad) (aux category writer difflist)))