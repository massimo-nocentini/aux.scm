
(module (aux hansei) *

  (import scheme 
          (chicken base) 
          (chicken continuation) 
          (chicken pretty-print) 
          (chicken fixnum) 
          (chicken sort) 
          srfi-69
          (aux base)
          (aux match)
          (aux continuation)
          (aux continuation delimited))

  (define op/times (make-parameter *))
  (define op/plus (make-parameter +))
  (define op/subtract (make-parameter -))
  (define op/divide (make-parameter (λ (m n) (exact->inexact (/ m n)))))
  (define op/greater (make-parameter >))

  (define-syntax-rule (probcc-τ p body ...) `((C ,(τ body ...)) ,p))
  (define-syntax-rule (probcc-value p body ...) `((V ,(begin body ...)) ,p))

  (define (probcc-explore maxdepth choices)
    (letrec ((times (op/times))
             (plus (op/plus))
             (loop (λ (p depth down choices ans susp)
                       (match/non-overlapping choices
                         (() susp)
                         (((,slot ,pt) . ,rest)
                            (let* ((p*pt (times p pt))
                                   (A (λ (w) (plus w p*pt))))
                              (match/non-overlapping slot
                                ((V ,v) (hash-table-update!/default ans v A 0)
                                        (loop p depth down rest ans susp))
                                ((C ,t) (cond
                                          (down (loop p depth down rest ans
                                                  (loop p*pt (add1 depth) (< depth maxdepth) (t) ans susp)))
                                          (else (let1 (s (cons (probcc-τ p*pt (t)) susp))
                                                  (loop p depth down rest ans s))))))))))))
      (let* ((ans (make-hash-table))
             (susp (loop 1 0 #t choices ans '()))
             (f (λ (v p l) (cons (probcc-value p v) l)))
             (folded (hash-table-fold ans f susp))
             (greater (op/greater)))
        (sort folded (λ (a b) (greater (cadr a) (cadr b)))))))

  #;(define (probcc-next-value choices)
    (match/non-overlapping choices
      (() '())
      ((((V ,v) ,pt) . ,rest) choices)
      ((((C ,t) ,pt) . ,rest) (let1 (times (op/times))
                                (probcc-next-value
                                  (append rest (letmap ((pair (t)))
                                                  (match1/non-overlapping ((,slot ,p) pair)
                                                    `(,slot ,(times p pt))))))))))

  (define (probcc-next-value choices)
    (cond
      ((null? choices) '())
      (else (match1/non-overlapping ((,slot ,pt) (car choices)) 
                             (match/non-overlapping slot
                                              ((V ,v) choices)
                                              ((C ,t) (let1 (times (op/times))
                                                           (probcc-next-value
                                                             (append (cdr choices)
                                                                     (letmap ((pair (t)))
                                                                             (match1/non-overlapping ((,slot ,p) pair)
                                                                                              `(,slot ,(times p pt)))))))))))))


  (define (probcc-normalize choices)
    (let* ((divide (op/divide))
           (plus (op/plus))
           (tot (foldr (λ (each t) (plus t (cadr each))) 0 choices))
           (N (λ (each) (list (car each) (divide (cadr each) tot)))))
      (map N choices)))

  (define (probcc-distribution distribution)
    (letcc/shift k 
      (letmap ((pair distribution))
        (match1/non-overlapping ((,v ,p) pair) (probcc-τ p (k v))))))

  (define (probcc-reflect choices)
    (letcc/shift k 
      (letrec ((make-choices (λ (pv) (map f pv)))
               (f (λ/non-overlapping
                    (((V ,v) ,p) (probcc-τ p (k v)))
                    (((C ,t) ,p) (probcc-τ p (make-choices (t)))))))
        (make-choices choices))))

  ; Events and random variables.
  (define (probcc-impossible) (probcc-distribution '()))
  (define (probcc-unit v) (list (probcc-value 1 v)))
  (define (probcc-bernoulli t f p) (probcc-distribution `((,t ,p) (,f ,((op/subtract) 1 p)))))
  (define (probcc-coin p) (probcc-bernoulli #t #f p))
  (define (probcc-uniform n)
    (cond
      ((> n 0) (letrec ((p (/ 1 n))
                        (plus (op/plus))
                        (subtract (op/subtract))
                        (loop (λ (pacc acc i)
                                  (if (zero? i)
                                      (probcc-distribution (cons `(,i ,(subtract 1 pacc)) acc))
                                      (loop (plus pacc p) (cons `(,i ,p) acc) (sub1 i))))))
                 (loop 0 '() (sub1 n))))
      (else (probcc-impossible))))

  (define (probcc-uniform/range low high)
    (+ low (probcc-uniform (add1 (- high low)))))
  
  (define (probcc-uniform/either lst) (list-ref lst (probcc-uniform (length lst))))

  (define (probcc-geometric p s f)
    (letrec ((subtract (op/subtract))
             (loop (λ (n)
                       (list (probcc-τ p (list (probcc-value 1 (cons s n))))
                             (probcc-τ (subtract 1 p) (loop (cons f n)))))))
      (probcc-reflect (loop '()))))

  (define-syntax probcc-when
    (syntax-rules ()
      ((_ test body ...) (cond
                           (test body ...) 
                           (else (probcc-impossible))))))

  (define (probcc-reify/0 model) (resetcc (probcc-unit (model))))
  (define ((probcc-reify depth) model) (probcc-explore depth (probcc-reify/0 model)))
  (define probcc-reify/exact/a (probcc-reify +inf.0))

  (define-syntax probcc-reify/exact
    (syntax-rules ()
      ((_ body ...) (probcc-reify/exact/a (τ body ...)))))

  (define (probcc-variable-elimination f)
    (λ args (probcc-reflect (probcc-reify/exact (apply f args)))))

  (define-syntax λ-probcc-bucket
    (syntax-rules ()
      ((_ args body ...) (letrec ((f (λ args body ...))
                                  (bucket (λ-memo bargs (probcc-reify/exact (apply f bargs)))))
                           (o probcc-reflect bucket)))))

  (define (probcc-leaves choices)
    (let L ((choices* choices) 
            (count 0))
      (let1 (F (λ (probpair acc) 
                  (match/non-overlapping probpair
                    (((V ,v) ,p) (add1 acc))
                    (((C ,t) ,p) (L (t) acc)))))
        (foldr F count choices*))))

  (define (probcc-dfs choices)
    (letmap ((probpair choices))
      (match/non-overlapping probpair
        (((V ,v) ,p) (list probpair))
        (((C ,t) ,p) (apply append (probcc-dfs (letmap ((inner (t)))
                                                (match1/non-overlapping ((,slot ,pi) inner)
                                                  (list slot ((op/times) p pi))))))))))


  )