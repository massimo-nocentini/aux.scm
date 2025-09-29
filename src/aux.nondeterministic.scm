
(module (aux nondeterministic) *

  (import scheme 
          (chicken base)
          (chicken sort)
          srfi-69
          (aux base)
          (aux continuation)
          (aux stream))

  (define ((nondeterministic system) cc)
    (letrec ((pool '())
             (stats (cons 0 0)) ; `car` counts accepted, `cdr` counts tried.
             (P (make-hash-table))
             (describe (τ
                         (letcar&cdr (((accepted tried) stats))
                                     `((left ,(length pool))
                                       (tried ,tried)
                                       (accepted ,accepted)
                                       (ratio ,(exact->inexact (/ accepted tried)))
                                       (distribution ,(sort (hash-table-map P (λ (value count) 
                                                                                  (list value (exact->inexact (/ count accepted)))))
                                                            (λ (a b) (> (cadr a) (cadr b)))))))))
             (fail (τ
                     (cond
                       ((null? pool) (cc (describe)))
                       (else (let1 (t (pop! pool))
                                   (cond
                                     ((procedure? t) (set-cdr! stats (add1 (cdr stats))) (t))
                                     (else (fail))))))))
             (chooseD (lambda (choices #!optional (continue? (λ (v) #t)))
                        (cond
                          ((null? choices) (fail))
                          ((promise? choices) (chooseD (force choices) continue?))
                          ((pair? choices) (letcc kk
                                                  (let1 (v (car choices))
                                                        (when (continue? v) 
                                                          (push! (τ (kk (chooseD (cdr choices) continue?))) pool))
                                                        (push! (τ (kk v)) pool))
                                                  (fail)))
                          (else (fail)))))
             (chooseB (lambda (choices)
                        (cond
                          ((promise? choices) (letcc kk
                                                     (append-right! (list (τ (kk (chooseB (force choices))))) pool)
                                                     (chooseB '())))
                          ((pair? choices) (letcc kk
                                                  (append-right! (list (τ (kk (car choices))) 
                                                                       (τ (kk (chooseB (cdr choices))))) 
                                                                 pool)
                                                  (chooseB '())))
                          (else (fail)))))
             (markD (τ (letgensym (flag) (push! flag pool) flag)))
             (cutD (lambda (flag)
                     (cond
                       ((null? pool) (void))
                       (else (let1 (a (pop! pool))
                                   (if (eq? a flag) (void) (cutD flag)))))))
             (⊦ (lambda (bool) (unless bool (fail)))))

      (set-cdr! stats (add1 (cdr stats)))
      (let1 (v (system chooseD chooseB ⊦ markD cutD))
            (set-car! stats (add1 (car stats)))
            (hash-table-update!/default P v add1 0)
            (yield§ v)
            (fail))))

  (define-syntax letnondeterministic§
    (syntax-rules ()
      ((letnondeterministic§ (chooseD chooseB asserter markD cutD) body ...)
       (resetcc+null
         (pretty-print (callcc (nondeterministic (λ (chooseD chooseB asserter markD cutD) body ...))))))))

  (define-syntax letnondeterministic
    (syntax-rules ()
      ((_ (chooseD chooseB asserter markD cutD) body ...)
       (letnondeterministic -1 (chooseD chooseB asserter markD cutD) body ...))
      ((_ nr (chooseD chooseB asserter markD cutD) body ...)
       (§->list (take§ nr (letnondeterministic§ (chooseD chooseB asserter markD cutD) body ...))))))

  )

















































