
(module (aux match) *

  (import scheme 
          (chicken base)
          (chicken string)
          (chicken memory representation)
          (aux base))

  (define-syntax match/non-overlapping
    (syntax-rules ()
      ((match/non-overlapping v (e ...) ...) (match/non-overlapping v "" (e ...) ...))
      ((match/non-overlapping v name (e ...) ...) (dmatch-run-a-thunk (quote v) v name (dmatch-remexp v (e ...) ...)))))

  (define-record dmatch-pkg clause thunk)
  (define-syntax-rule (dmatch-pkg pat g e ...) (make-dmatch-pkg (quote (pat g e ...)) (τ e ...)))

  (define-syntax dmatch-remexp
    (syntax-rules ()
      ((dmatch-remexp (rator rand ...) cls ...) (let1 (v (rator rand ...)) (dmatch-aux v cls ...)))
      ((dmatch-remexp v cls ...) (dmatch-aux v cls ...))))

  (define-syntax dmatch-aux
    (syntax-rules (guard)
      ((dmatch-aux v) '())
      ((dmatch-aux v (pat (guard g ...) e0 e ...) cls ...)
        (let1 (fk (τ (dmatch-aux v cls ...)))
          (dmatch-ppat v pat
            (cond 
              ((not (and g ...)) (fk))
              (else (cons (dmatch-pkg pat (guard g ...) e0 e ...) (fk))))
            (fk))))
      ((dmatch-aux v (pat e0 e ...) cls ...)
        (let1 (fk (τ (dmatch-aux v cls ...)))
          (dmatch-ppat v pat
            (cons (dmatch-pkg pat (guard ) e0 e ...) (fk))
            (fk))))))

  (define-syntax dmatch-ppat
    (syntax-rules (unquote)
      ((dmatch-ppat vv () kt kf) (let1 (v vv) (if (or (null? v) (and (vector? v) (zero? (vector-length v)))) kt kf)))
      ((dmatch-ppat vv (unquote v) kt kf) (let1 (v vv) kt))
      ((dmatch-ppat vv (x . y) kt kf)
        (let1 (v vv)
          (cond
            ((pair? v) (let ((vx (car v)) 
                             (vy (cdr v))) 
                        (dmatch-ppat vx x (dmatch-ppat vy y kt kf) kf)))
            ((and (vector? v) (> (vector-length v) 0)) (let ((vx (vector-ref v 0))
                                                             (vy (subvector v 1)))
                                                          (dmatch-ppat vx x (dmatch-ppat vy y kt kf) kf)))
            ((record-instance? v) (let* ((r (record->vector v))
                                         (vx (vector-ref r 0))
                                         (vy (subvector r 1)))
                                    (dmatch-ppat vx x (dmatch-ppat vy y kt kf) kf)))
            (else kf))))
      ((dmatch-ppat v lit kt kf) (if (equal? v (quote lit)) kt kf))))

  (define (dmatch-run-a-thunk v-expr v name pkg∗)
    (cond
      ((null? pkg∗) (error 'match/non-overlapping `((message ,name) 
                                                    (reason "no match found") 
                                                    (expr ,v-expr) 
                                                    (value ,v))))
      ((null? (cdr pkg∗)) ((dmatch-pkg-thunk (car pkg∗))))
      (else (error 'match/non-overlapping `((expr ,v-expr) 
                                            (value ,v) 
                                            (message ,name) 
                                            (reason "overlapping match") 
                                            (ambiguities ,(map dmatch-pkg-clause pkg∗)))))))

)