

(module (aux kanren micro) *

  (import scheme 
          (chicken base)
          (chicken memory representation)
          srfi-1 srfi-69
          (aux base)
          (aux stream)
          (aux fds sbral))

  ; stuff for variables
  (define µkanren-var-metaid (gensym 'µkanren-var-))
  (define µkanren-var-unbound (gensym 'µkanren-void-))
  (define (µkanren-var i) (list µkanren-var-metaid i))
  (define (µkanren-var? v) (and (pair? v) (eq? (car v) µkanren-var-metaid)))
  (define µkanren-var-index cadr)
  (define (µkanren-unbound? v) (eq? v µkanren-var-unbound))

  (define µkanren-state-empty (list empty/sbral 0))
  (define µkanren-state-substitution car)
  (define µkanren-state-counter cadr)
  (define (µkanren-var-index/state v s) (- (µkanren-state-counter s) 1 (µkanren-var-index v)))

  (define (µkanren-state-find v s)
    (let ((size (µkanren-state-counter s))
          (subst (µkanren-state-substitution s)))
      (let F ((p (void)) (w v))
        (cond
          ((µkanren-var? w) (let1 (i (µkanren-var-index/state w s))
                              (F w (sbral-ref subst i))))
          ((µkanren-unbound? w) p)
          (else w)))))

  (define (µkanren-state-set var v s)
    (define (occur? w)
      (cond
        ((µkanren-var? w) (equal? w var))
        ((pair? w) (or (occur? (µkanren-state-find (car w) s))
                       (occur? (µkanren-state-find (cdr w) s))))
        (else #f)))
    (cond
      ((occur? v) #f)
      (else
        (let ((size (µkanren-state-counter s))
              (subst (µkanren-state-substitution s))
              (i (µkanren-var-index/state var s)))
          (list (update/sbral i v subst) size)))))

  (define (µkanren-state-unify u v s)
    (let ((u* (µkanren-state-find u s))
          (v* (µkanren-state-find v s)))
      (cond
        ((eq? u* v*) s)
        ((and (string? u*) (string? v*) (string=? u* v*)) s)
        ((and (number? u*) (number? v*) (= u* v*)) s)
        ((and (vector? u*) (vector? v*) (= (vector-length u*) (vector-length v*)))
          (let1 (F (λ (ss i) (and (pair? ss) (µkanren-state-unify (vector-ref u* i) (vector-ref v* i) ss))))
            (foldl F s (iota (vector-length u*)))))
        ((and (record-instance? u*) (record-instance? v*)) (µkanren-state-unify (record->vector u*) (record->vector v*) s))
        ((and (vector? u*) (record-instance? v*)) (µkanren-state-unify u* (record->vector v*) s))
        ((and (record-instance? u*) (vector? v*)) (µkanren-state-unify (record->vector u*) v* s))
        ;((and (µkanren-var? u) (µkanren-var? u)) think about which of the two should reference the other
        ((µkanren-var? u*) (µkanren-state-set u* v* s))
        ((µkanren-var? v*) (µkanren-state-set v* u* s))
        ((and (pair? u*) (pair? v*)) (let1 (ss (µkanren-state-unify (car u*) (car v*) s))
                                           (and (pair? ss) (µkanren-state-unify (cdr u*) (cdr v*) ss))))
        (else #f))))

  (define (µkanren-state-find* v s)
    (let A ((w v))
      (let1 (w* (µkanren-state-find w s))
            (cond
              ((µkanren-var? w*) w*)
              ((pair? w*) (cons (A (car w*)) (A (cdr w*))))
              (else w*)))))

  (define (µkanren-state-reify v s)
    (let R ((w v) (r s) (c 0) (vars '()))
      (let1 (w* (µkanren-state-find w r))
            (cond
              ((µkanren-var? w*) (let1 (new-var (string->symbol (string-append "_" (number->string c))))
                                        (values
                                          (list (update/sbral (µkanren-var-index/state w* r) 
                                                              (list 'unquote new-var)
                                                              (µkanren-state-substitution r))
                                                (µkanren-state-counter r))
                                          (add1 c)
                                          (cons new-var vars))))
              ((pair? w*) (let-values (((r* c* vars*) (R (car w*) r c vars)))
                            (R (cdr w*) r* c* vars*)))
              ;((record-instance? w*) )
              (else (values r c vars))))))

  (define ((µkanren-project w) s)
    (let1 (w* (cond 
                ((< 0 (µkanren-state-counter s)) (µkanren-state-find* w s))
                (else #t) ; for tautology when there is no variable
                ))
      (let-values (((s* c vars-reversed) (µkanren-state-reify w* s)))
        (let ((quotation 'quasiquote)
              (vars (reverse vars-reversed))
              (repr (µkanren-state-find* w s*)))
          (cond
            ((or (number? repr) (string? repr) (symbol? repr) (boolean? repr)) `(λ ,vars ,repr))
            ; ((record-instance? repr) `(λ ,vars (make-record-instance ,@(vector->list (record->vector repr)))))
            (else `(λ ,vars (,quotation ,repr))))))))

  ; goals --------------------------------------------------------------------------

  (define (✓° s) (cons§ s '()))
  (define (✗° s) '())

  (define ((µkanren-goal/fresh° f) s)
    (let* ((i (µkanren-state-counter s))
           (goal (f (µkanren-var i))))
      (delay (goal (list (cons/sbral µkanren-var-unbound (µkanren-state-substitution s))
                         (add1 i))))))

  (define ((=° u v) s)
    (let* ((s* (µkanren-state-unify u v s))
           (g (cond ((pair? s*) ✓°) (else ✗°))))
      (delay (g s*))))

  (define ((µkanren-goal/or° f g) s) (append§/interleaved/2 (delay (f s)) (delay (g s))))
  (define ((µkanren-goal/and° f g) s) (append-map§ g (delay (f s))))

  (define-syntax fresh°
    (syntax-rules ()
      ((fresh° () body ...) (and° body ...))
      ((fresh° (v w ...) body ...) (µkanren-goal/fresh° (λ (v) (fresh° (w ...) body ...))))
      ((fresh° r (v ...) body ...) (fresh° (r) (fresh° (v ...) (=° r (list v ...)) body ...)))))

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

  (define ((if° g? gt gf) s)
    (let L ((§ (g? s)))
      (cond
        ((null? §) (delay (gf s)))
        ((promise? §) (delay (L (force §))))
        (else (append-map§ gt §)))))

  (define (null° l) (=° l '()))
  (define (cons° a d c) (=° c (cons a d)))
  (define-syntax-rule (project° ((v* v) ...) g ...) (λ (s) (let ((v* (µkanren-state-find* v s)) ...) (delay ((and° g ...) s)))))
  (define-syntax-rule (cond° (g ...) ...) (or° (and° g ...) ...))

  (define-syntax-rule (define-relation (name arg ...) g ...) (define ((name arg ...) s) (delay ((and° g ...) s))))

  (define (°->§ . goals)
    (let* ((g (foldl µkanren-goal/and° ✓° goals))
           (§ (delay (g µkanren-state-empty)))
           (P (µkanren-project (µkanren-var 0))))
      (map§ P §)))

  (define (°->list . goals) 
    (cond
      ((null? goals) '())
      ((number? (car goals)) (§->list (take§ (car goals) (apply °->§ (cdr goals)))))
      (else (§->list (take§ +inf.0 (apply °->§ goals))))))

  (define-syntax-rule (literal over from =>) (groupby° (((v* aggr) v) ...) over (k ...) from g => f ...)
    (λ (s)
        (let* ((§ (delay (g s))) ; the stream to fold over
               (F (λ (s* H)
                      (hash-table-update!/default H  
                                                  (list (µkanren-state-find* k s*) ...) ; key
                                                  (λ (group) (hash-table-update!/default group v (λ (vs) (cons (µkanren-state-find* v s*) vs)) '()) ... group) ; value
                                                  (make-hash-table))
                      H))
               (ht (foldr§ F (make-hash-table) §))
               (G (λ (key group folded) (or° (let ((v* (aggr (hash-table-ref group v))) ...) (receive (k ...) (apply values key) (and° f ...))) folded)))
               (g* (hash-table-fold ht G ✗°)))
          (delay (g* s)))))

  (define-syntax-rule (literal over from =>) (window° (((v* aggr) v) ...) over (k ...) from g => f ...)
    (λ (s)
        (let* ((§ (delay (g s))) ; the stream to fold over
               (F (λ (s* H)
                      (hash-table-update!/default H  
                                                  (list (µkanren-state-find* k s*) ...) ; key
                                                  (λ (group) (hash-table-update!/default group v (λ (vs) (cons (µkanren-state-find* v s*) vs)) '()) ... group) ; value
                                                  (make-hash-table))
                      H))
               (ht (foldr§ F (make-hash-table) §))
               (G (λ (s*) (let* ((group (hash-table-ref ht (list (µkanren-state-find* k s*) ...)))
                                 (v* (aggr (hash-table-ref group v))) ...)
                            ((and° f ...) s*)))))
          (append-map§ G §))))

  (define-syntax-rule (literal over from =>) 
    (set° (v aggr init₀) over ((k* k) ...) from g => f ...)
      (λ (s)
        (let* ((§ (delay (g s))) ; the stream to fold over
              (A aggr) ; to evaluate `aggr` only once
              (init init₀) ; to evaluate `init` only once
              (F (λ (s* H)
                    (let1 (key (list (µkanren-state-find* k s*) ...))
                      (hash-table-update!/default H key (λ (u) (apply A `(,@key ,u))) init))
                    H))
              (ht (foldr§ F (make-hash-table) §))
              (G (λ (key v folded) (or° (receive (k* ...) (apply values key) (and° f ...)) folded)))
              (g* (hash-table-fold ht G ✗°)))
          (delay (g* s)))))

  (define-syntax-rule (literal over from =>) 
    (enumerate° (v aggr) over (k ...) from g => f ...)
      (λ (s)
        (let* ((§ (delay (g s))) ; the stream to fold over
               (A aggr) ; to evaluate `aggr` only once
               (i 0)
               (F (λ (s* H)
                   (let1 (key (list i (µkanren-state-find* k s*) ...))
                     (add1! i)
                     (cons (apply A key) H))))
               (v (reverse (foldr§ F '() §))))
           (delay ((and° f ...) s)))))


  )





















