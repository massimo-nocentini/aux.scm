

(module (aux kanren micro) *

  (import scheme 
          (chicken base)
          (aux base)
          (aux stream)
          (fds sbral)
          )

  ; stuff for variables
  (define µkanren-var-metaid (gensym 'µkanren-var-))
  (define µkanren-var-unbound (gensym 'µkanren-void-))
  (define (µkanren-var i) (list µkanren-var-metaid i))
  (define (µkanren-var? v) (and (pair? v) (eq? (car v) µkanren-var-metaid)))
  (define (µkanren-var-index v) (cadr v))
  (define (µkanren-unbound? v) (eq? v µkanren-var-unbound))

  (define µkanren-state-empty (list empty/sbral 0))
  (define (µkanren-state-substitution s) (car s))
  (define (µkanren-state-counter s) (cadr s))
  (define (µkanren-state-find v s)
    (let ((size (µkanren-state-counter s))
          (subst (µkanren-state-substitution s)))
      (let F ((p (void)) (w v))
        (cond
          ((µkanren-var? w) (F w (sbral-ref subst (- size 1 (µkanren-var-index w)))))
          ((µkanren-unbound? w) p)
          (else w)))))

  (define (µkanren-state-set var v s)
    (define (occur? w)
      (cond
        ((µkanren-var? w) (equal? w var))
        ((pair? w) (or (occur? (µkanren-state-find (car w) s)) (occur? (µkanren-state-find (cdr w) s))))
        (else #f)))
    (cond
      #;((occur? v) #f)
      (else
        (let ((size (µkanren-state-counter s))
              (subst (µkanren-state-substitution s))
              (i (µkanren-var-index var)))
          (list (update/sbral i v subst) size)))))

  (define (µkanren-state-unify u v s)
    (let ((u* (µkanren-state-find u s))
          (v* (µkanren-state-find v s)))
      (cond
        ((eq? u* v*) s)
        ;((and (µkanren-var? u) (µkanren-var? u)) think about which of the two should reference the other
        ((µkanren-var? u*) (µkanren-state-set u* v* s))
        ((µkanren-var? v*) (µkanren-state-set v* u* s))
        ((and (pair? u*) (pair? v*)) (let1 (ss (µkanren-state-unify (car u*) (car v*) s))
                                           (and (pair? ss) (µkanren-state-unify (cdr u*) (cdr v*) ss))))
        (else #f))))

  (define (µkanren-apply-subst v s)
    (let A ((w v))
      (let1 (w* (µkanren-state-find w s))
            (cond
              ((µkanren-var? w*) w*)
              ((pair? w*) (cons (A (car w*)) (A (cdr w*))))
              (else w*)))))

  (define (µkanren-build-r v s)
    (let ((c (µkanren-state-counter s))
	  (subst (µkanren-state-substitution s)))
      (cond
	((µkanren-var? v) (list (cons/sbral (+ c (length/sbral subst)) subst) (add1 c)))
	((pair? v) (µkanren-build-r (cdr v) (µkanren-build-r (car v) s)))
	(else s))))

  (define ((µkanren-project w) s)
    (let* ((w* (µkanren-apply-subst w s))
           (s* (µkanren-build-r w* (list empty/sbral 0)))
           (w** (µkanren-apply-subst w* s*))
           (s** (µkanren-build-r w** µkanren-state-empty)))
      (µkanren-apply-subst w** s**)))

  (define (✓° s) (cons§ s '()))
  (define (✗° s) '())

  (define ((µkanren-goal/fresh° f) s)
    (let* ((i (µkanren-state-counter s))
           (var (µkanren-var i))
           (goal (f var)))
      (goal (list (cons/sbral µkanren-var-unbound (µkanren-state-substitution s)) (add1 i)))))

  (define ((=° u v) s)
    (let1 (snew (µkanren-state-unify u v s))
          (if (pair? snew) (cons§ snew '()) '())))

  (define ((µkanren-goal/or° f g) s) (append§/interleaved (delay (f s)) (delay (g s))))
  (define ((µkanren-goal/and° f g) s) (append-map§ g (delay (f s))))

  (define-syntax fresh°
    (syntax-rules ()
      ((fresh° () body ...) (and° body ...))
      ((fresh° (v w ...) body ...) (µkanren-goal/fresh° (λ (v) (fresh° (w ...) body ...))))))

  (define-syntax and°
    (syntax-rules ()
      ((and°) ✓°)
      ((and° g) g)
      ((and° g1 g2 g* ...) (and° (µkanren-goal/and° g1 g2) g* ...))))

  (define-syntax or°
    (syntax-rules ()
      ((or°) ✗°)
      ((or° g) g)
      ((or° g1 g2 g* ...) (µkanren-goal/or° g1 (or° g2 g* ...)))))

  (define-syntax define-relation
    (syntax-rules ()
      ((define-relation (name arg ...) g ...) (define ((name arg ...) s) ((and° g ...) s)))))

  (define-syntax solutions§
    (syntax-rules ()
      ((solutions§ (var ...) g ...) (let1 (main (fresh° (q) (fresh° (var ...) g ... (=° q (list var ...)))))
                                       (map§ (µkanren-project (µkanren-var 0)) (main µkanren-state-empty))))
      ((solutions§ var g ...) (map§ car (solutions§ (var) g ...)))))

  )


