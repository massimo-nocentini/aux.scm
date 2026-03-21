

(module (aux kanren micro) *

  (import scheme 
          (chicken base)
          (chicken memory representation)
          srfi-1 srfi-69 srfi-133
          (aux base)
          (aux stream)
          (aux fds sbral))
  
  (define-record µkanren-unbound)

  (define-record µkanren-tag name def pred)

  (define-syntax define-µkanren-tag
    (syntax-rules ()
      ((_ (name s) body) (define name (make-µkanren-tag (gensym 'name) '(μ s body) (μ s body))))
      ((_ (name s) body ...) (define-µkanren-tag (name s) (begin body ...)))))
  
  (define (µkanren-tag-hold? tag v) (let1 (pred? (µkanren-tag-pred tag)) (pred? v)))
  (define (µkanren-tag-equal? tag1 tag2) (equal? (µkanren-tag-name tag1) (µkanren-tag-name tag2)))

  (define *µkanren-unbound* (make-µkanren-unbound))

  ; variables --------------------------------------------------------------------

  (define-record µkanren-var index)

  (define (µkanren-var-working? α) (and (µkanren-var? α) (<= 0 (µkanren-var-index α))))

  (define (µkanren-var-index/✓ α) 
    (let1 (i (µkanren-var-index α))
      (if (µkanren-var-working? α) i (- (add1 i)))))

  (define (µkanren-var->symbol α)
    (string->symbol 
      (string-append
        (if (µkanren-var-working? α) "▢" "_")
        (number->string (µkanren-var-index/✓ α)))))

  ; state ------------------------------------------------------------------------

  (define-record µkanren-state vars-count S D A T tags)

  (define µkanren-state-empty (make-µkanren-state 0 empty/sbral empty/sbral empty/sbral empty/sbral '()))

  (define (µkanren-var-index/sbral sbral)
    (let1 (l (length/sbral sbral))
      (μ α (- l 1 (µkanren-var-index/✓ α)))))

  (define (µkanren-var-deferred? sbral)
    (let1 (l (length/sbral sbral))
      (μ α (and (µkanren-var? α) (<= l (µkanren-var-index/✓ α))))))

  (define (µkanren-sbral-ref/var sbral)
    (let1 (index-of-var (µkanren-var-index/sbral sbral))
      (μ α (sbral-ref sbral (index-of-var α)))))

  (define (µkanren-var-extend/sbral α S default)
    (let ((i (µkanren-var-index/✓ α))
          (l (length/sbral S)))
      (let U ((l* l) (S* S))
        (cond
          ((<= l* i) (U (add1 l*) (cons/sbral default S*)))
          (else S*)))))

  (define-syntax λ°
    (syntax-rules (:)
      ((λ° (s : vc S D A T tags) body ...) (λ (s)
                                            (let ((vc (µkanren-state-vars-count s))
                                                  (S (µkanren-state-S s))
                                                  (D (µkanren-state-D s))
                                                  (A (µkanren-state-A s))
                                                  (T (µkanren-state-T s))
                                                  (tags (µkanren-state-tags s)))
                                              body ...)))
      ((λ° (s) body ...) (λ° (s : vc S D A T tags) body ...))))

  (define-syntax-rule (μkanren-state-match ((vc S D A T tags) s) body ...)
    (let ((vc (µkanren-state-vars-count s))
          (S (µkanren-state-S s))
          (D (µkanren-state-D s))
          (A (µkanren-state-A s))
          (T (µkanren-state-T s))
          (tags (µkanren-state-tags s)))
      body ...))

  (define (µkanren-update/sbral α v sbral)
    (let* ((sbral*        (µkanren-var-extend/sbral α sbral *µkanren-unbound*))
           (index-of-var  (µkanren-var-index/sbral sbral*))
           (i             (index-of-var α)))
      (update/sbral i v sbral*)))

  (define (µkanren-state-update α v s)
    (μkanren-state-match ((vc S D A T tags) s)
      (let1 (S* (µkanren-update/sbral α v S))
        (make-µkanren-state vc S* D A T tags))))

  (define (µkanren-state-occur? w v s)
    (let occur? ((v* v))
      (cond
        ((µkanren-var? v*) (equal? w v*))
        ((pair? v*) (or (occur? (µkanren-state-find (car v*) s)) (occur? (µkanren-state-find (cdr v*) s))))
        ((vector? v*) (vector-fold (λ (found e) (or found (occur? (µkanren-state-find e s)))) #f v*))
        ((record-instance? v*) (occur? (record->vector v*)))
        (else #f))))

  (define (µkanren-state-update/✓ α β s) (and (not (µkanren-state-occur? α β s)) (µkanren-state-update α β s)))

  (define (µkanren-state-find α s)
    (let* ((S (µkanren-state-S s))
           (deferred? (µkanren-var-deferred? S))
           (ref-var (µkanren-sbral-ref/var S)))
      (let F ((β0 α) (β α))
        (cond
          ((deferred? β) β)
          ((µkanren-var-working? β) (F β (ref-var β)))
          ((µkanren-unbound? β) β0)
          (else β)))))

  (define (µkanren-state-unify u v s)
    (let ((u* (µkanren-state-find u s))
          (v* (µkanren-state-find v s)))
      (cond
        ((eq? u* v*) s)
        ((and (µkanren-var? u*) (µkanren-var? v*)) 
          (if (< (µkanren-var-index/✓ u*) (µkanren-var-index/✓ v*))
            (µkanren-state-update/✓ u* v* s)
            (µkanren-state-update/✓ v* u* s)))
        ((µkanren-var? u*) (µkanren-state-update/✓ u* v* s))
        ((µkanren-var? v*) (µkanren-state-update/✓ v* u* s))
        ((and (string? u*) (string? v*) (string=? u* v*)) s)
        ((and (number? u*) (number? v*) (= u* v*)) s)
        ((and (vector? u*) (vector? v*) (= (vector-length u*) (vector-length v*)))
          (let1 (F (λ (s* i) (and (µkanren-state? s*) (µkanren-state-unify (vector-ref u* i) (vector-ref v* i) s*))))
            (foldl F s (iota (vector-length u*)))))
        ((and (record-instance? u*) (record-instance? v*)) (µkanren-state-unify (record->vector u*) (record->vector v*) s))
        ((and (vector? u*) (record-instance? v*)) (µkanren-state-unify u* (record->vector v*) s))
        ((and (record-instance? u*) (vector? v*)) (µkanren-state-unify (record->vector u*) v* s))
        ((and (pair? u*) (pair? v*)) 
          (let1 (s* (µkanren-state-unify (car u*) (car v*) s)) 
            (and (µkanren-state? s*) (µkanren-state-unify (cdr u*) (cdr v*) s*))))
        (else #f))))

  (define (µkanren-state-find* v s)
    (let A ((w v))
      (let1 (w* (µkanren-state-find w s))
        (cond
          ((µkanren-var? w*) w*)
          ((pair? w*) (cons (A (car w*)) (A (cdr w*))))
          ((vector? w*) (vector-map A w*))
          ((record-instance? w*) (let* ((vec (record->vector w*))
                                        (F (λ (lst e) (cons (A e) lst)))
                                        (type+args (vector-fold-right F '() vec)))
                                  (apply make-record-instance type+args)))
          (else w*)))))

  (define (µkanren-state-find*/repr v s)
    (let A ((w v))
      (let1 (w* (µkanren-state-find w s))
        (cond
          ((µkanren-var? w*) (µkanren-var->symbol w*))
          ((symbol? w*) (list 'quote w*))
          ((null? w*) (list 'quote '()))
          ((pair? w*) (list 'cons (A (car w*)) (A (cdr w*))))
          ((vector? w*) (cons 'vector (map A (vector->list w*))))
          ((record-instance? w*) (cons  'make-record-instance 
                                        (vector-fold-right (λ (lst e) (cons (A e) lst)) '() (record->vector w*))))
          (else w*)))))

  (define (µkanren-state-reify v s)
    (let R ((w v) (r s) (c -1) (vars '()))
      (let1 (w* (µkanren-state-find w r))
            (cond
              ((µkanren-var-working? w*) (let* ((v* (make-µkanren-var c))
                                                (r* (µkanren-state-update w* v* r))
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

  (define ((µkanren-project w) s)
    (let1 (w* (if (null? (µkanren-state-S s)) #t (µkanren-state-find* w s))) ; for tautology when there is no variable in the substitution.
      (match1/first ((,s* _ ,vars-reversed) (µkanren-state-reify w* s))
        (let* ((vars (reverse vars-reversed))
               (vars* (map µkanren-var->symbol vars))
               (repr (µkanren-state-find*/repr w* s*)))
          `(λ ,vars* ,repr)))))

  (define (((µkanren-make-tag-A tag) u) state)
    (let1 (u* (µkanren-state-find u state))
      (cond
        ((µkanren-var? u*) (cond
                              ((µkanren-make-tag-A+ u* tag state) => ✓°)
                              (else (✗° state))))
        #;((pair? u*) (✗° state))
        ((µkanren-tag-hold? tag u*) (✓° state))
        (else (✗° state)))))

  (define (µkanren-make-tag-A+ α tag state)
    (match/non-overlapping (ext-A α tag state)
      (() state)
      (#t (let* ((A   (µkanren-state-A state))
                 (D   (µkanren-state-D state))
                 (A*  (µkanren-update/sbral α tag A))
                 (D*  (µkanren-subsume α A* D)))
            (µkanren-subsume-A α tag D* A* state)))
      (#f #f)))

  (define (ext-A α tag state)
    (let1 (f  (λ (i tag* failed)
                (or failed
                  (let1 (α* (make-µkanren-var i))
                    (cond
                      ((equal? α (µkanren-state-find α* state)) (if (µkanren-tag-equal? tag* tag) '() #t))
                      (else failed))))))
      (not/✓ (foldr/sbral f #f (µkanren-state-A state)))))

  (define (µkanren-subsume α A/T D)
    #;(remp (λ (d) (exists (subsumed-pr? A/T ) d)) D)
    D)

  (define (µkanren-subsume-A α tag D A state)
    (match1/first ((,D* ,T*) (µkanren-update-D/T α D A state))
      (make-µkanren-state
        (µkanren-state-vars-count state)
        (µkanren-state-S state) D* A T*
        (cons tag (µkanren-state-tags state)))))

  (define (µkanren-update-D/T α D A state)
    (let* ((tags (µkanren-state-tags state))
           (done? (λ (i tag done)
                    (or done
                      (let1 (α* (make-µkanren-var i))
                        (cond
                          ((and (equal? α α*) (member? tag tags)) #t)
                          (else done))))))
           (F (λ (i each sbral)
                (let1 (α* (make-µkanren-var i))
                  (cond
                    ((equal? α α*) sbral)
                    (else (µkanren-update/sbral α* each sbral)))))))
      (if (foldr/sbral done? #f A)
        (µkanren-update-D/T+ α D empty/sbral state)
        (let* ((T (µkanren-state-T state))
               (T* (foldr/sbral F empty/sbral T)))
              (list D T*)))))

  (define (µkanren-update-D/T+ α D T state)
    (let1 (F  (λ (i each D&T)
                (let ((α* (make-µkanren-var i))
                      (D (car D&T))
                      (T (cadr D&T)))
                  (if (equal? α α*)
                    (let1 (D* (ext-D α each D state)) (list D* T))
                    (let1 (T* (µkanren-update/sbral α* each T)) (list D T*))))))
      (foldr/sbral F (list D T) (µkanren-state-T state))))

  (define (ext-D α tag D state)
    (let1 (exists?  (λ (i d) 
                      (let1 (α* (make-µkanren-var i))
                        (match/first d
                          (((,tag*)) (and (equal? α α*) (µkanren-tag? tag*) (µkanren-tag-equal? tag tag*)))
                          (else #f)))))
      (if (exists?/sbral exists? D) D (µkanren-update/sbral α (list tag) D))))

  ; goals --------------------------------------------------------------------------

  (define ✓° list)
  (define (✗° s) '())

  (define (freshª f) ; ª means "applicative", so `freshª` is a *function* that consumes a function and returns a goal.
    (λ° (s : vc S D A T tags)
      (let* ((g  (f (make-µkanren-var vc)))
             (s* (make-µkanren-state (add1 vc) S D A T tags)))
        (delay (g s*)))))

  (define ((=° u v) s)
    (let* ((s* (µkanren-state-unify u v s))
           (g (if (µkanren-state? s*) ✓° ✗°)))
      (delay (g s*))))

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

  (define-µkanren-tag (tag:sym s) (symbol? s))
  (define-µkanren-tag (tag:num s) (number? s))
  
  (define symbol° (µkanren-make-tag-A tag:sym))
  (define number° (µkanren-make-tag-A tag:num))

  (define-syntax-rule (project° ((v α) ...) g ...) 
    (μ s (let* ((v (µkanren-state-find* α s)) ...) (delay ((and° g ...) s)))))
  
  (define-syntax-rule (cond° (g ...) ...) (or° (and° g ...) ...))

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

  ; API ------------------------------------------------------------------------

  (define-syntax-rule (define-relation (name arg ...) g ...) (define (name arg ...) (and° g ...)))

  (define (°->§ g)
    (let* ((§ (delay (g µkanren-state-empty)))
           (P (µkanren-project (make-µkanren-var 0))))
      (map§ P §)))

  (define (°->list grounded g)
    (let1 (sols (§->list (°->§ g)))
      (cond
        (grounded (map (λ (expr) (let ((E (eval expr)) (args (cadr expr))) (apply E args))) sols))
        (else sols))))

  (define (°->list/ground g) (°->list #t g))

  (define-syntax-rule (µkanren-run (v n grounded) g ...) (°->list grounded (take° n (fresh° (v) g ...))))
)