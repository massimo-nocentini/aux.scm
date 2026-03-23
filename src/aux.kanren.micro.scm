

(module (aux kanren micro) *

  (import scheme 
          (chicken base)
          (chicken memory representation)
          srfi-1 srfi-69 srfi-133
          (aux base)
          (aux stream)
          (aux fds sbral))
  
  (define-record μkanren-unbound)

  (define-record μkanren-tag name def pred)

  (define-syntax define-μkanren-tag
    (syntax-rules ()
      ((_ (name s) body) (define name (make-μkanren-tag (gensym 'name) '(μ s body) (μ s body))))
      ((_ (name s) body ...) (define-μkanren-tag (name s) (begin body ...)))))
  
  (define (μkanren-tag-hold? tag v) (let1 (pred? (μkanren-tag-pred tag)) (pred? v)))
  (define (μkanren-tag-equal? tag1 tag2) (equal? (μkanren-tag-name tag1) (μkanren-tag-name tag2)))

  (define *μkanren-unbound* (make-μkanren-unbound))

  ; variables --------------------------------------------------------------------

  (define-record μkanren-var index)

  (define (μkanren-var-working? α) (and (μkanren-var? α) (<= 0 (μkanren-var-index α))))

  (define (μkanren-var-index/✓ α) 
    (let1 (i (μkanren-var-index α))
      (if (μkanren-var-working? α) i (- (add1 i)))))

  (define (μkanren-var->symbol α)
    (string->symbol 
      (string-append
        (if (μkanren-var-working? α) "▢" "_")
        (number->string (μkanren-var-index/✓ α)))))

  ; state ------------------------------------------------------------------------

  (define-record μkanren-state vars-count S D A T tags)

  (define μkanren-state-empty (make-μkanren-state 0 empty/sbral empty/sbral empty/sbral empty/sbral '()))

  (define (μkanren-var-index/sbral sbral)
    (let1 (l (length/sbral sbral))
      (μ α (- l 1 (μkanren-var-index/✓ α)))))

  (define (μkanren-var-deferred? sbral)
    (let1 (l (length/sbral sbral))
      (μ α (and (μkanren-var? α) (<= l (μkanren-var-index/✓ α))))))

  (define (μkanren-sbral-ref/var sbral)
    (let1 (index-of-var (μkanren-var-index/sbral sbral))
      (μ α (sbral-ref sbral (index-of-var α)))))

  (define (μkanren-var-extend/sbral α S default)
    (let ((i (μkanren-var-index/✓ α))
          (l (length/sbral S)))
      (let U ((l* l) (S* S))
        (cond
          ((<= l* i) (U (add1 l*) (cons/sbral default S*)))
          (else S*)))))

  (define-syntax λ°
    (syntax-rules (:)
      ((λ° (s : vc S D A T tags) body ...) (λ (s)
                                            (let ((vc (μkanren-state-vars-count s))
                                                  (S (μkanren-state-S s))
                                                  (D (μkanren-state-D s))
                                                  (A (μkanren-state-A s))
                                                  (T (μkanren-state-T s))
                                                  (tags (μkanren-state-tags s)))
                                              body ...)))
      ((λ° (s) body ...) (λ° (s : vc S D A T tags) body ...))))

  (define-syntax-rule (μkanren-state-match ((vc S D A T tags) s) body ...)
    (let ((vc (μkanren-state-vars-count s))
          (S (μkanren-state-S s))
          (D (μkanren-state-D s))
          (A (μkanren-state-A s))
          (T (μkanren-state-T s))
          (tags (μkanren-state-tags s)))
      body ...))

  (define (μkanren-update/sbral α v sbral)
    (let* ((sbral*        (μkanren-var-extend/sbral α sbral *μkanren-unbound*))
           (index-of-var  (μkanren-var-index/sbral sbral*))
           (i             (index-of-var α)))
      (update/sbral i v sbral*)))

  (define (μkanren-state-update α v s)
    (μkanren-state-match ((vc S D A T tags) s)
      (let1 (S* (μkanren-update/sbral α v S))
        (make-μkanren-state vc S* D A T tags))))

  (define (μkanren-state-occur? w v s)
    (let occur? ((v* v))
      (cond
        ((μkanren-var? v*) (equal? w v*))
        ((pair? v*) (or (occur? (μkanren-state-find (car v*) s)) (occur? (μkanren-state-find (cdr v*) s))))
        ((vector? v*) (vector-fold (λ (found e) (or found (occur? (μkanren-state-find e s)))) #f v*))
        ((record-instance? v*) (occur? (record->vector v*)))
        (else #f))))

  (define (μkanren-state-update/✓ α β s) (and (not (μkanren-state-occur? α β s)) (μkanren-state-update α β s)))

  (define (μkanren-state-find α s)
    (let* ((S (μkanren-state-S s))
           (deferred? (μkanren-var-deferred? S))
           (ref-var (μkanren-sbral-ref/var S)))
      (let F ((β0 α) (β α))
        (cond
          ((deferred? β) β)
          ((μkanren-var-working? β) (F β (ref-var β)))
          ((μkanren-unbound? β) β0)
          (else β)))))

  (define (μkanren-state-unify u v s)
    (let ((u* (μkanren-state-find u s))
          (v* (μkanren-state-find v s)))
      (cond
        ((eq? u* v*) s)
        ((and (μkanren-var? u*) (μkanren-var? v*)) 
          (if (< (μkanren-var-index/✓ u*) (μkanren-var-index/✓ v*))
            (μkanren-state-update/✓ u* v* s)
            (μkanren-state-update/✓ v* u* s)))
        ((μkanren-var? u*) (μkanren-state-update/✓ u* v* s))
        ((μkanren-var? v*) (μkanren-state-update/✓ v* u* s))
        ((and (string? u*) (string? v*) (string=? u* v*)) s)
        ((and (number? u*) (number? v*) (= u* v*)) s)
        ((and (vector? u*) (vector? v*) (= (vector-length u*) (vector-length v*)))
          (let1 (F (λ (s* i) (and (μkanren-state? s*) (μkanren-state-unify (vector-ref u* i) (vector-ref v* i) s*))))
            (foldl F s (iota (vector-length u*)))))
        ((and (record-instance? u*) (record-instance? v*)) (μkanren-state-unify (record->vector u*) (record->vector v*) s))
        ((and (vector? u*) (record-instance? v*)) (μkanren-state-unify u* (record->vector v*) s))
        ((and (record-instance? u*) (vector? v*)) (μkanren-state-unify (record->vector u*) v* s))
        ((and (pair? u*) (pair? v*)) 
          (let1 (s* (μkanren-state-unify (car u*) (car v*) s)) 
            (and (μkanren-state? s*) (μkanren-state-unify (cdr u*) (cdr v*) s*))))
        (else #f))))

  (define (μkanren-state-find* v s)
    (let A ((w v))
      (let1 (w* (μkanren-state-find w s))
        (cond
          ((μkanren-var? w*) w*)
          ((pair? w*) (cons (A (car w*)) (A (cdr w*))))
          ((vector? w*) (vector-map A w*))
          ((record-instance? w*) (let* ((vec (record->vector w*))
                                        (F (λ (lst e) (cons (A e) lst)))
                                        (type+args (vector-fold-right F '() vec)))
                                  (apply make-record-instance type+args)))
          (else w*)))))

  (define (μkanren-state-find*/repr v s)
    (let A ((w v))
      (let1 (w* (μkanren-state-find w s))
        (cond
          ((μkanren-var? w*) (μkanren-var->symbol w*))
          ((symbol? w*) (list 'quote w*))
          ((null? w*) (list 'quote '()))
          ((pair? w*) (list 'cons (A (car w*)) (A (cdr w*))))
          ((vector? w*) (cons 'vector (map A (vector->list w*))))
          ((record-instance? w*) (cons  'make-record-instance 
                                        (vector-fold-right (λ (lst e) (cons (A e) lst)) '() (record->vector w*))))
          (else w*)))))

  (define (μkanren-state-reify v s)
    (let R ((w v) (r s) (c -1) (vars '()))
      (let1 (w* (μkanren-state-find w r))
            (cond
              ((μkanren-var-working? w*) (let* ((v* (make-μkanren-var c))
                                                (r* (μkanren-state-update w* v* r))
                                                (c* (sub1 c))
                                                (vars* (cons v* vars)))
                                           (R (void) r* c* vars*)))
              ((pair? w*) (match1/first ((,r* ,c* ,vars*) (R (car w*) r c vars))
                            (R (cdr w*) r* c* vars*)))
              ((vector? w*) (let loop ((i 0) (r* r) (c* c) (vars* vars))
                              (cond
                                ((= i (vector-length w*)) (list r* c* vars*))
                                (else (match1/first ((,r** ,c** ,vars**) (R (vector-ref w* i) r* c* vars*))
                                        (loop (add1 i) r** c** vars**))))))
              ((record-instance? w*) (R (record->vector w*) r c vars))
              (else (list r c vars))))))

  (define ((μkanren-project w) s)
    (let1 (w* (if (null? (μkanren-state-S s)) #t (μkanren-state-find* w s))) ; for tautology when there is no variable in the substitution.
      (match1/first ((,s* _ ,vars-reversed) (μkanren-state-reify w* s))
        (let* ((vars (reverse vars-reversed))
               (vars* (map μkanren-var->symbol vars))
               (repr (μkanren-state-find*/repr w* s*)))
          `(λ ,vars* ,repr)))))

  (define (((μkanren-make-tag-A tag) u) state)
    (let1 (u* (μkanren-state-find u state))
      (cond
        ((μkanren-var? u*) (cond
                              ((μkanren-make-tag-A+ u* tag state) => ✓°)
                              (else (✗° state))))
        #;((pair? u*) (✗° state))
        ((μkanren-tag-hold? tag u*) (✓° state))
        (else (✗° state)))))

  (define (μkanren-make-tag-A+ α tag state)
    (match/non-overlapping (ext-A α tag state)
      (() state)
      (#t (let* ((A   (μkanren-state-A state))
                 (D   (μkanren-state-D state))
                 (A*  (μkanren-update/sbral α tag A))
                 (D*  (μkanren-subsume α A* D)))
            (μkanren-subsume-A α tag D* A* state)))
      (#f #f)))

  (define (ext-A α tag state)
    (let1 (f  (λ (i tag* failed)
                (or failed
                  (let1 (α* (make-μkanren-var i))
                    (cond
                      ((equal? α (μkanren-state-find α* state)) (if (μkanren-tag-equal? tag* tag) '() #t))
                      (else failed))))))
      (not/✓ (foldr/sbral f #f (μkanren-state-A state)))))

  (define (μkanren-subsume α A/T D)
    (let1 (f  (λ (i each sbral)
                (let1 (α* (make-μkanren-var i))
                  (cond
                    ((equal? α α*) sbral)
                    (else (μkanren-update/sbral α* each sbral))))))
      (foldr/sbral f empty/sbral D)))

  (define (μkanren-subsume-A α tag D A state)
    (match1/first ((,D* ,T*) (μkanren-update-D/T α D A state))
      (make-μkanren-state
        (μkanren-state-vars-count state)
        (μkanren-state-S state) D* A T*
        (cons tag (μkanren-state-tags state)))))

  (define (μkanren-update-D/T α D A state)
    (let* ((tags (μkanren-state-tags state))
           (done? (λ (i tag done)
                    (or done
                      (let1 (α* (make-μkanren-var i))
                        (cond
                          ((and (equal? α α*) (member? tag tags)) #t)
                          (else done))))))
           (F (λ (i each sbral)
                (let1 (α* (make-μkanren-var i))
                  (cond
                    ((equal? α α*) sbral)
                    (else (μkanren-update/sbral α* each sbral)))))))
      (if (foldr/sbral done? #f A)
        (μkanren-update-D/T+ α D empty/sbral state)
        (let* ((T (μkanren-state-T state))
               (T* (foldr/sbral F empty/sbral T)))
              (list D T*)))))

  (define (μkanren-update-D/T+ α D T state)
    (let1 (F  (λ (i each D&T)
                (let ((α* (make-μkanren-var i))
                      (D (car D&T))
                      (T (cadr D&T)))
                  (if (equal? α α*)
                    (let1 (D* (ext-D α each D state)) (list D* T))
                    (let1 (T* (μkanren-update/sbral α* each T)) (list D T*))))))
      (foldr/sbral F (list D T) (μkanren-state-T state))))

  (define (ext-D α tag D state)
    (let1 (exists?  (λ (i d) 
                      (let1 (α* (make-μkanren-var i))
                        (match/first d
                          (((,tag*)) (and (equal? α α*) (μkanren-tag? tag*) (μkanren-tag-equal? tag tag*)))
                          (else #f)))))
      (if (exists?/sbral exists? D) D (μkanren-update/sbral α (list tag) D))))

  ; goals --------------------------------------------------------------------------

  (define ✓° list)
  (define (✗° s) '())

  (define (freshª f) ; ª means "applicative", so `freshª` is a *function* that consumes a function and returns a goal.
    (λ° (s : vc S D A T tags)
      (let* ((g  (f (make-μkanren-var vc)))
             (s* (make-μkanren-state (add1 vc) S D A T tags)))
        (delay (g s*)))))

  (define ((=° u v) s)
    (cond 
      ((μkanren-state-unify u v s) => (μkanren-post-=° s))
      (else (✗° s))))

  (define ((μkanren-post-=° s) s*) (✓° s*))
    ; (cond
    ;   ((eq? s s*) (✓° s))
    ;   ((μkanren-verify-D s s*) => (μ D (cond
    ;                                     ((μkanren-post-verify-D D s s*) => ✓°)
    ;                                     (else (✗° s)))))
    ;   (else (✗° s))))

  #;(define (μkanren-verify-D s s*)
    (let1 (F  (λ (i each D*)
                (and D*
                  (cond
                    ((μkanren-unify* i each s*) => (λ (s**) (if (eq? s* s**) #f (cons ...))))
                    (else D*)))))
      (foldr/sbral F empty/sbral (μkanren-state-D s))))

  (define ((orª f g) s) (append§/interleaved/2 (delay (f s)) (delay (g s))))
  (define ((andª f g) s) (append-map§ g (delay (f s))))

  (define-syntax fresh°
    (syntax-rules ()
      ((fresh° () body ...) (and° body ...))
      ((fresh° (α β ...) body ...) (freshª (λ (α) (fresh° (β ...) body ...))))
      ((fresh° α (β ...) body ...) (fresh° (α) (fresh° (β ...) (=° α (list β ...)) body ...)))))

  (define-syntax-rule (fresh°/record α (t β ...) body ...)
    (fresh° (α β ...) (=° (make-record-instance t β ...) α) body ...))

  (define-syntax and°
    (syntax-rules ()
      ((and°) ✓°)
      ((and° g) g)
      ((and° g1 g2 g* ...) (and° (andª g1 g2) g* ...))))

  (define-syntax or°
    (syntax-rules ()
      ((or°) ✗°)
      ((or° g g* ...) (orª g (or° g* ...)))))

  (define ((if° g? gt gf) s)
    (define (L §)
      (cond
        ((null? §) (delay (gf s)))
        ((promise? §) (delay (L (force §))))
        (else (append-map§ gt §))))
    (delay (L (g? s))))

  (define (take° n g) (μ s (take§ n (delay (g s)))))

  (define (null° l) (=° l '()))
  (define (boolean° v) (or° (=° v #t) (=° v #f)))
  (define (cons° a d c) (=° c (cons a d)))

  (define-μkanren-tag (tag:sym s) (symbol? s))
  (define-μkanren-tag (tag:num s) (number? s))
  
  (define symbol° (μkanren-make-tag-A tag:sym))
  (define number° (μkanren-make-tag-A tag:num))

  (define-syntax-rule (project° ((v α) ...) g ...) 
    (μ s (let* ((v (μkanren-state-find* α s)) ...) (delay ((and° g ...) s)))))
  
  (define-syntax-rule (cond° (g ...) ...) (or° (and° g ...) ...))

  (define-syntax-rule (literal over from =>) (groupby° (((v* aggr) v) ...) over (k ...) from g => f ...)
    (λ (s)
        (let* ((§ (delay (g s))) ; the stream to fold over
               (F (λ (s* H)
                      (hash-table-update!/default H  
                                                  (list (μkanren-state-find* k s*) ...) ; key
                                                  (λ (group) (hash-table-update!/default group v (λ (vs) (cons (μkanren-state-find* v s*) vs)) '()) ... group) ; value
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
                                                  (list (μkanren-state-find* k s*) ...) ; key
                                                  (λ (group) (hash-table-update!/default group v (λ (vs) (cons (μkanren-state-find* v s*) vs)) '()) ... group) ; value
                                                  (make-hash-table))
                      H))
               (ht (foldr§ F (make-hash-table) §))
               (G (λ (s*) (let* ((group (hash-table-ref ht (list (μkanren-state-find* k s*) ...)))
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
                    (let1 (key (list (μkanren-state-find* k s*) ...))
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
                   (let1 (key (list i (μkanren-state-find* k s*) ...))
                     (add1! i)
                     (cons (apply A key) H))))
               (v (reverse (foldr§ F '() §))))
           (delay ((and° f ...) s)))))

  ; API ------------------------------------------------------------------------

  (define-syntax-rule (define-relation (name arg ...) g ...) (define (name arg ...) (and° g ...)))

  (define (°->§ g)
    (let* ((§ (delay (g μkanren-state-empty)))
           (P (μkanren-project (make-μkanren-var 0))))
      (map§ P §)))

  (define (°->list grounded g)
    (let1 (sols (§->list (°->§ g)))
      (cond
        (grounded (map (λ (expr) (let ((E (eval expr)) (args (cadr expr))) (apply E args))) sols))
        (else sols))))

  (define (°->list/ground g) (°->list #t g))

  (define-syntax-rule (μkanren-run (v n grounded) g ...) (°->list grounded (take° n (fresh° (v) g ...))))
)