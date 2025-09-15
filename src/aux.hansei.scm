
; use parameters to generalize constants and arithmetic operations

(module (aux hansei) *

  (import scheme 
          (chicken base) 
          (chicken continuation) 
          (chicken pretty-print) 
          (chicken fixnum) 
          (chicken sort) 
          srfi-69
          (aux base)
          (aux continuation))

  (define op/times (make-parameter *))
  (define op/plus (make-parameter +))
  (define op/subtract (make-parameter -))
  (define op/divide (make-parameter (λ (m n) (exact->inexact (/ m n)))))
  (define op/greater (make-parameter >))

  (define-syntax probcc-τ (syntax-rules () ((_ p body ...) `((C ,(τ body ...)) ,p))))
  (define (probcc-value p v) `((V ,v) ,p))

  (define-syntax let1/probccpair
    (syntax-rules () 
      ((_ ((slot p) probpairexpr) body ...) (let* ((probpair probpairexpr)
                                                   (slot (car probpair))
                                                   (p (cadr probpair)))
                                              body ...))))

  (define-syntax cond/probccslot
    (syntax-rules (V C)
      ((_ slotexpr 
          ((V v) vbody ...)
          ((C t) cbody ...))
       (let* ((slot slotexpr)
              (flag (car slot))
              (payload (cadr slot)))
         (cond
           ((equal? flag 'V) (let1 (v payload) vbody ...))
           ((equal? flag 'C) (let1 (t payload) cbody ...))
           (else (error `(not a probability slot ,slot))))))))

  (define (probcc-explore maxdepth choices)
    (letrec ((times (op/times))
             (plus (op/plus))
             (loop (λ (p depth down choices ans susp)
                       (cond
                         ((null? choices) susp)
                         (else (let1/probccpair ((slot pt) (car choices)) 
                                                (let* ((p*pt (times p pt))
                                                       (A (λ (w) (plus w p*pt)))
                                                       (rest (cdr choices)))
                                                  (cond/probccslot slot
                                                                   ((V v) (hash-table-update!/default ans v A 0)
                                                                    (loop p depth down rest ans susp))
                                                                   ((C t) (cond
                                                                            (down (loop p depth down rest ans
                                                                                        (loop p*pt (add1 depth) (< depth maxdepth) (t) ans susp)))
                                                                            (else (let1 (s (cons (probcc-τ p*pt (t)) susp))
                                                                                        (loop p depth down rest ans s)))))))))))))
      (let* ((ans (make-hash-table))
             (susp (loop 1 0 #t choices ans '()))
             (f (λ (v p l) (cons (probcc-value p v) l)))
             (folded (hash-table-fold ans f susp))
             (greater (op/greater)))
        (sort folded (λ (a b) (greater (cadr a) (cadr b)))))))

  (define (probcc-next-value choices)
    (cond
      ((null? choices) '())
      (else (let1/probccpair ((slot pt) (car choices)) 
                             (cond/probccslot slot
                                              ((V v) choices)
                                              ((C t) (let1 (times (op/times))
                                                           (probcc-next-value
                                                             (append (cdr choices)
                                                                     (letmap ((pair (t)))
                                                                             (let1/probccpair ((slot p) pair)
                                                                                              `(,slot ,(times p pt)))))))))))))

  (define (probcc-normalize choices)
    (let* ((divide (op/divide))
           (plus (op/plus))
           (tot (foldr (λ (each t) (plus t (cadr each))) 0 choices))
           (N (λ (each) (list (car each) (divide (cadr each) tot)))))
      (map N choices)))

  (define (probcc-distribution distribution)
    (letcc/shift k (letmap ((pair distribution))
                           (letcar&cdr (((v p) pair))
                                       (probcc-τ (car p) (k v))))))

  (define (probcc-reflect choices)
    (letcc/shift k (letrec ((make-choices (λ (pv) (map f pv)))
                            (f (λ (probpair)
                                   (let1/probccpair ((slot p) probpair)
                                                    (cond/probccslot slot
                                                                     ((V v) (probcc-τ p (k v)))
                                                                     ((C t) (probcc-τ p (make-choices (t)))))))))
                     (make-choices choices))))

  ; Events and random variables.
  (define (probcc-impossible) (probcc-distribution '()))
  (define (probcc-unit v) (list (probcc-value 1 v)))
  (define (probcc-bernoulli t f p) (probcc-distribution `((,t ,p) (,f ,((op/subtract) 1 p)))))
  (define (probcc-coin p) (probcc-bernoulli #t #f p))
  (define (probcc-uniform n)
    (cond
      ((equal? n 1) 0)
      ((> n 1) (letrec ((p (/ 1 n))
                        (plus (op/plus))
                        (subtract (op/subtract))
                        (loop (λ (pacc acc i)
                                  (if (zero? i)
                                      (probcc-distribution (cons `(,i ,(subtract 1 pacc)) acc))
                                      (loop (plus pacc p) (cons `(,i ,p) acc) (sub1 i))))))
                 (loop 0 '() (sub1 n))))
      (else (error `(non-positive count ,n)))))

  (define (probcc-uniform/range low high)
    (+ low (probcc-uniform (add1 (- high low)))))

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
    (letrec ((L (λ (choices count)
                    (let1 (F (λ (probpair acc) 
                                 (let1/probccpair ((slot p) probpair)
                                                  (cond/probccslot slot
                                                                   ((V v) (add1 acc))
                                                                   ((C t) (L (t) acc))))))
                          (foldr F count choices)))))
      (L choices 0)))

  (define (probcc-dfs choices)
    (letmap ((probpair choices))
            (let1/probccpair ((slot p) probpair)
                             (cond/probccslot slot
                                              ((V v) (list probpair))
                                              ((C t) (apply append (probcc-dfs (letmap ((inner (t)))
                                                                                       (let1/probccpair ((slot pi) inner)
                                                                                                        (list slot ((op/times) p pi)))))))))))


  )









