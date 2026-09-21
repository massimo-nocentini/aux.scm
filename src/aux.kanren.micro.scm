

(module (aux kanren micro)
  ( ; variables and states
    make-μkanren-var μkanren-var? μkanren-var-index μkanren-var-working? μkanren-var->symbol
    make-μkanren-state μkanren-state? μkanren-state-empty μkanren-state-vars-count μkanren-state-S μkanren-state-D μkanren-state-A μkanren-state-T μkanren-state-tags
    μkanren-state-with μkanren-state-match μkanren-state-find μkanren-state-find/value μkanren-state-find/values μkanren-state-find/repr μkanren-state-unify μkanren-project
    ; tags
    define-μkanren-tag make-μkanren-tag μkanren-tag? μkanren-tag-name μkanren-tag-def μkanren-tag-pred? μkanren-tag-equal? μkanren-tag/sym μkanren-tag/num μkanren-make-tag-A
    ; goals — freshª andª orª ✓° ✗° =° must stay exported: the exported macros expand to them at the importer's site
    ✓° ✗° freshª andª orª =° ≠° fresh° fresh°/record and° or° if° take° null° boolean° cons° symbol° number° absent° project° cond°
    ; aggregation — μkanren-group§ is expanded to by groupby° and window°
    μkanren-group§ groupby° window° set° enumerate°
    ; api
    define-relation °->§ °->list °->list/ground μkanren-run
    ; internals exercised directly by the unit tests
    μkanren-var-index/sbral μkanren-sbral-ref/var μkanren-rem-subsumed/by μkanren-partition* μkanren-foldr/verify μkanren-verify-T+)

  (import scheme 
          (chicken base)
          (chicken memory representation)
          (chicken eval)
          srfi-1 srfi-69
          (aux base)
          (aux stream)
          (aux fds sbral))
  
  (define-record μkanren-unbound) ; this implicitly defines the predicate μkanren-unbound? and the constructor make-μkanren-unbound.
  (define *μkanren-unbound* (make-μkanren-unbound))

  (define-record μkanren-tag name def pred)

  (define-syntax-rule (define-μkanren-tag (name s) body ...)
    (define name (make-μkanren-tag (gensym 'name) '(μ s body ...) (μ s body ...))))
  
  (define (μkanren-tag-pred? tag v) (let1 (pred? (μkanren-tag-pred tag)) (pred? v)))
  (define (μkanren-tag-equal? tag1 tag2) (equal? (μkanren-tag-name tag1) (μkanren-tag-name tag2)))

  ; variables --------------------------------------------------------------------

  (define-record μkanren-var index)

  (set-record-printer! μkanren-var (λ (α port) (display (μkanren-var->symbol α) port)))

  ; a *working* variable has index i ≥ 0 (its slot in the substitution S, see `μkanren-var-index/sbral`);
  ; a *reified* one has index -1, -2, ... (allocated by `μkanren-state-reify`) and is printed as α, β, ...
  ; `μkanren-var-index>=0` maps both to a non-negative number via `(- (add1 i))`.
  (define (μkanren-var-working? α) (and (μkanren-var? α) (<= 0 (μkanren-var-index α))))

  (define (μkanren-var-index>=0 α) (let1 (i (μkanren-var-index α)) (if (μkanren-var-working? α) i (- (add1 i)))))

  (define (μkanren-var->symbol α)
    (let1 (i (μkanren-var-index>=0 α))
      (cond 
        ((μkanren-var-working? α) (string->symbol (string-append "_" (number->string i))))
        (else (let* ((n (vector-length greek-alphabet/lowercase))
                     (g (vector-ref greek-alphabet/lowercase (modulo i n)))
                     (k (quotient i n)))
                (if (zero? k) g (symbol-append g (string->symbol (number->string k)))))))))

  ; state ------------------------------------------------------------------------

  ; `vars-count` is the number of variables introduced so far; `S` is the substitution, an sbral indexed
  ; by variable with the most recent variable first; `D` the disequality store (a list of failed-unification
  ; prefixes, each a list of `(α . u)` pairs); `A` the type store and `T` the absento store (lists of
  ; `(α . tag)` pairs); `tags` the tags asserted so far through `A`. D, A and T are plain lists: the
  ; `empty/sbral` in `μkanren-state-empty` is just `'()`, only S is a real sbral.
  (define-record μkanren-state vars-count S D A T tags)

  (define μkanren-state-empty (make-μkanren-state 0 empty/sbral empty/sbral empty/sbral empty/sbral '()))

  (define (μkanren-state-equal? s s*) (eq? s s*))

  ; a copy of `s` where the given fields are replaced.
  (define (μkanren-state-with s #!key
                              (vars-count (μkanren-state-vars-count s))
                              (S (μkanren-state-S s))
                              (D (μkanren-state-D s))
                              (A (μkanren-state-A s))
                              (T (μkanren-state-T s))
                              (tags (μkanren-state-tags s)))
    (make-μkanren-state vars-count S D A T tags))

  ; the position of the working variable α in the sbral `sbral` (which stores the most recent variable first).
  (define (μkanren-var-index/sbral sbral α) (- (length/sbral sbral) 1 (μkanren-var-index>=0 α)))
  (define (μkanren-sbral-ref/var sbral α) (sbral-ref sbral (μkanren-var-index/sbral sbral α)))

  (define (μkanren-var-extend/sbral α S)
    (let ((i (μkanren-var-index>=0 α)))
      (let U ((l* (length/sbral S)) (S* S))
        (cond
          ((<= l* i) (U (add1 l*) (cons/sbral *μkanren-unbound* S*)))
          (else S*)))))

  (define-syntax-rule (μkanren-state-match ((vc S D A T tags) s) body ...)
    (let ((vc (μkanren-state-vars-count s))
          (S (μkanren-state-S s))
          (D (μkanren-state-D s))
          (A (μkanren-state-A s))
          (T (μkanren-state-T s))
          (tags (μkanren-state-tags s)))
      body ...))

  (define (μkanren-update/sbral α v sbral)
    (let1 (sbral* (μkanren-var-extend/sbral α sbral))
      (update/sbral (μkanren-var-index/sbral sbral* α) v sbral*)))

  ; a vector or a record instance is walked as the list of its slots (a record's first slot is its type tag).
  (define (μkanren-structure? v) (or (vector? v) (record-instance? v)))
  (define (μkanren-structure->list v) (vector->list (if (vector? v) v (record->vector v))))

  (define (μkanren-state-update α v s assocs #!key (unsafe #f))
    
    (define (occur? v*)
      (cond
        ((μkanren-var? v*) (equal? α v*))
        ((pair? v*) (or (occur? (μkanren-state-find (car v*) s)) (occur? (μkanren-state-find (cdr v*) s))))
        ((μkanren-structure? v*) (occur? (μkanren-structure->list v*)))
        (else #f)))
    
    (and
      (or unsafe (not (occur? v)))
      (μkanren-state-match ((vc S D A T tags) s)
        (let* ((S* (μkanren-update/sbral α v S))
               (s* (make-μkanren-state vc S* D A T tags))
               (assocs* (cons `(,α . ,v) assocs)))
          `(,s* ,assocs*)))))

  (define (μkanren-state-find α s)
    (let* ((S (μkanren-state-S s))
           (l (length/sbral S))
           (var-deferred? (μ α (and (μkanren-var? α) (<= l (μkanren-var-index>=0 α)))))
           (ref-var (μ β (sbral-ref S (- l 1 (μkanren-var-index>=0 β))))))
      (let F ((β0 α) (β α))
        (cond
          ((var-deferred? β) β)
          ((μkanren-var-working? β) (F β (ref-var β)))
          ((μkanren-unbound? β) β0)
          (else β)))))

  (define (μkanren-state-unify u v s assocs)
    (let ((return (τ `(,s ,assocs)))
          (u* (μkanren-state-find u s))
          (v* (μkanren-state-find v s)))
      (cond
        ((eq? u* v*) (return))
        ((and (string? u*) (string? v*) (string=? u* v*)) (return))
        ((and (number? u*) (number? v*) (eqv? u* v*)) (return))
        ((and (μkanren-var-working? u*) (μkanren-var-working? v*))
          (let ((i (μkanren-var-index>=0 u*)) (j (μkanren-var-index>=0 v*)))
            (cond
              ((= i j) (return))
              ((< i j) (μkanren-state-update u* v* s assocs))
              (else (μkanren-state-update v* u* s assocs)))))
        ((μkanren-var-working? u*) (μkanren-state-update u* v* s assocs))
        ((μkanren-var-working? v*) (μkanren-state-update v* u* s assocs))
        ((and (μkanren-structure? u*) (μkanren-structure? v*))
          (μkanren-state-unify (μkanren-structure->list u*) (μkanren-structure->list v*) s assocs))
        ((and (pair? u*) (pair? v*))  (match/non-overlapping (μkanren-state-unify (car u*) (car v*) s assocs)
                                        ((,s* ,assocs*) (μkanren-state-unify (cdr u*) (cdr v*) s* assocs*))
                                        (#f #f)))
        (else #f))))

  (define (μkanren-state-find/value v s)
    (let A ((w v))
      (let1 (w* (μkanren-state-find w s))
        (cond
          ((pair? w*) (cons (A (car w*)) (A (cdr w*))))
          ((vector? w*) (list->vector (map A (μkanren-structure->list w*))))
          ((and (record-instance? w*) (not (μkanren-var? w*))) (apply make-record-instance (map A (μkanren-structure->list w*))))
          (else w*)))))

  (define (μkanren-state-find/repr v s)
    (let A ((w v))
      (let1 (w* (μkanren-state-find w s))
        (cond
          ((μkanren-var? w*) (μkanren-var->symbol w*))
          ((symbol? w*) (list 'quote w*))
          ((null? w*) (list 'quote '()))
          ((pair? w*) (list 'cons (A (car w*)) (A (cdr w*))))
          ((vector? w*) (cons 'vector (map A (μkanren-structure->list w*))))
          ((record-instance? w*) (cons 'make-record-instance (map A (μkanren-structure->list w*))))
          (else w*)))))

  (define (μkanren-state-reify v s)
    (let R ((w v) (r s) (c -1) (vars '()) (assocs '()))
      (let1 (w* (μkanren-state-find w r))
        (cond
          ((μkanren-var-working? w*)  (let* ((v* (make-μkanren-var c))
                                             (r*+assocs* (μkanren-state-update w* v* r assocs))
                                             (r* (car r*+assocs*))
                                             (assocs* (cadr r*+assocs*))
                                             (c* (sub1 c))
                                             (vars* (cons v* vars)))
                                        (R (void) r* c* vars* assocs*))) ; `(void)` will stop the recursion in the next call.
          ((pair? w*) (match1/first ((,r* ,c* ,vars* ,assocs*) (R (car w*) r c vars assocs)) (R (cdr w*) r* c* vars* assocs*)))
          ((μkanren-structure? w*) (R (μkanren-structure->list w*) r c vars assocs))
          (else (list r c vars assocs))))))

  (define (μkanren-drop-dot-D D s)
    (let* ((pair->equal (λ1-match/first ((,α . ,u) `(equal? ,(μkanren-var->symbol α) ,(μkanren-state-find/repr u s)))))
           (f (λ (d) (match/first (map pair->equal d)
                       ((,e) `(begin (deny ,e)))
                       (,es `(begin (deny (and ,@es))))))))
      (map f D)))
  
  (define μkanren-drop-dot-T (map/curry (λ1-match/first ((,α . ,tag) `(,tag ,α)))))

  (define μkanren-sort-part (λ1-match/first ((,tag . ,vars) `(,tag . ,(sort/lex<=? vars)))))

  ; groups the type store A by tag: `((tag . vars) ...)`.
  (define (μkanren-partition* A)
    (match/first A
      (() '())
      (((_ . ,tag) . _)
        (receive (same others) (partition (λ1-match/first ((_ . ,tag*) (μkanren-tag-equal? tag tag*))) A)
          (cons `(,tag . ,(remove-duplicates (map lhs same))) (μkanren-partition* others))))))

  (define (μkanren-form v D A T s)
    (let* ((fd (μkanren-drop-dot-D (sort/lex<=? (map sort/lex<=? D)) s))
           (FA (λ1-match/first ((,tag . ,vars) `(assert (every ,(μkanren-tag-def tag) (list ,@(map μkanren-var->symbol vars)))))))
           (fa (map FA (sort/lex<=? (map μkanren-sort-part (μkanren-partition* A)))))
           (FT (λ1-match/first ((,tag ,var) `(assert (absent? (quote ,(μkanren-tag-name tag)) ,(μkanren-var->symbol var))))))
           (ft (map FT (μkanren-drop-dot-T (sort/lex<=? T)))))
        `(,@fd ,@fa ,@ft ,v)))

  (define (μkanren-subsumed-T? x tag T)
    ((exists (λ1-match/first ((,y . ,tag*) (and (equal? x y) (μkanren-tag-equal? tag tag*))))) T))

  ; drops from a store every element subsumed by another one (before or after it).
  (define ((μkanren-rem-subsumed/by subsumed?) L0)
    (let loop ((L L0) (L+ '()))
      (match/first L
        (() L+)
        (((,x . ,L*) ⊣ (or (subsumed? x L*) (subsumed? x L+))) (loop L* L+))
        ((,x . ,L*) (loop L* (cons x L+))))))

  (define μkanren-rem-subsumed-T (μkanren-rem-subsumed/by (λ (t T) (μkanren-subsumed-T? (lhs t) (rhs t) T))))

  (define (μkanren-anyvar? s)
    (define anyvar? (λ1-match/first
                      ((,a . ,d) (or (anyvar? a) (anyvar? d)))
                      ((,α ⊣ (μkanren-var-working? α)) (μkanren-var-working? (μkanren-state-find α s)))
                      ((,v ⊣ (μkanren-structure? v)) (anyvar? (μkanren-structure->list v)))
                      (else #f)))
    anyvar?)

  (define (μkanren-subsumed? d D)
    (let* ((f (λ-match/non-overlapping
                (((,α . ,u) (,s* ,assocs*)) (μkanren-state-update α u s* assocs*))
                ((_ #f) #f)))
           (s+assocs (foldr f `(,μkanren-state-empty ()) d))
           (s (car s+assocs)))
      (match/first D
        (() #f)
        ((,d* . ,D*)  (match/non-overlapping (μkanren-state-unify/assoc d* s '())
                        ((,s* ,d**) (or (μkanren-state-equal? s s*) (μkanren-subsumed? d D*)))
                        (#f (μkanren-subsumed? d D*)))))))

  ; it must come after `μkanren-subsumed?`, being evaluated at load time.
  (define μkanren-rem-subsumed (μkanren-rem-subsumed/by μkanren-subsumed?))

  (define ((μkanren-project w) s)
    (let1 (w* (μkanren-state-find/value w s))
      (match1/first ((,s* ,c ,vars-reversed ,assocs) (μkanren-state-reify w* s))
        (let* ((R     (λ1-match/first ((,α . _) (μkanren-var-working? (μkanren-state-find α s*)))))
               (vars  (reverse vars-reversed))
               (vars* (map μkanren-var->symbol vars))
               (w**   (μkanren-state-find/repr w* s*))
               (D     (μkanren-state-D s*))
               (A     (μkanren-state-A s*))
               (T     (μkanren-state-T s*))
               (D*    (μkanren-rem-subsumed (remove (μkanren-anyvar? s*) D))) ; needs more revision the stuff with T.
               (A*    (remove R A))
               (T*    (remove R T))
               (D**   (μkanren-state-find/value (μkanren-subsume A* D*) s*))
               (A**   (μkanren-state-find/value A* s*))
               (T**   (μkanren-rem-subsumed-T (μkanren-state-find/value T* s*)))
               (body  (μkanren-form w** D** A** T** s*)))
          `(λ ,vars* ,@body)))))

  ; constraints -------------------------------------------------------------------

  (define (μkanren-ext-D α tag D s)
    (let* ((name (μkanren-tag-name tag))
           (E (λ1-match/first
                (((,α* . ,u)) (and (equal? α (μkanren-state-find α* s)) (equal? u name)))
                (else #f)))
           (exists? (exists E)))
      (cond
        ((exists? D) D)
        (else (cons `((,α . ,name)) D)))))

  (define (μkanren-update-D/T+ α T+ D T s)
    (match/first T
      (() `(,D . ,T+))
      ((((,α* . ,tag) . ,T*) ⊣ (equal? α α*)) (let1 (D* (μkanren-ext-D α tag D s)) (μkanren-update-D/T+ α T+ D* T* s)))
      ((,t . ,T*) (let1 (T+* (cons t T+)) (μkanren-update-D/T+ α T+* D T* s)))))

  (define (μkanren-update-D/T α D A T s)
    (match/first A
      (() `(,D . ,T))
      ((((,α* . _) . _) ⊣ (equal? α* α)) (μkanren-update-D/T+ α '() D T s))
      ((_ . ,A*) (μkanren-update-D/T α D A* T s))))

  (define (μkanren-subsume-T vars T+ D A T s)
    (match/first vars
      (() (μkanren-state-with s D: D A: A T: (append T+ T)))
      ((,α . ,vars*)  (match1/first ((,D* . ,T+*) (μkanren-update-D/T α D A T+ s))
                        (μkanren-subsume-T vars* T+* D* A T s)))))

  (define (μkanren-subsumed-pr? A/T)
    (λ1-match/first
      (((_ . ,α) ⊣ (μkanren-var-working? α)) #f)
      ((,α . ,u)  (any (λ1-match/first
                         (((,α* . ,u*) ⊣ (equal? α α*))
                           (cond
                             ((and (μkanren-tag? u*) (μkanren-tag-pred? u* u)) #f)
                             (else #t)))
                         (else #f))
                       A/T))))

  (define (μkanren-subsume A-or-T D)
    (remove (exists (μkanren-subsumed-pr? A-or-T)) D))

  (define (μkanren-verify-T/post D A s)
    (let1 (vars (remove-duplicates (map lhs A)))
      (μ T (μkanren-subsume-T vars T (μkanren-subsume T D) A '() s))))

  (define (μkanren-ext-T+ α tag T s)
    (match/first T
      (() `((,α . ,tag)))
      ((((,α* . ,tag*) . _) ⊣ (and (equal? (μkanren-state-find α* s) α) (μkanren-tag-equal? tag tag*))) '())
      ((_ . ,T*) (μkanren-ext-T+ α tag T* s))))

  ; a right fold over a store that stops at the first `#f` returned by `step`
  ; (the stores D, A and T are always proper lists, so no `else` clause is needed).
  (define (μkanren-foldr/verify step L)
    (let F ((L L))
      (match/first L
        (() '())
        ((,x . ,L*) (cond ((F L*) => (μ acc (step x acc))) (else #f))))))

  (define (μkanren-verify-T+ α tag s)
    (match/first (μkanren-state-find α s)
      ((,α* ⊣ (μkanren-var-working? α*))  (μ T₀
                                    (cond
                                      ((μkanren-ext-T+ α* tag T₀ s) => (μ T+ (append T+ T₀)))
                                      (else #f))))
      ((,au . ,du)  (μ T₀
                      (cond
                        (((μkanren-verify-T+ au tag s) T₀) => (μkanren-verify-T+ du tag s))
                        (else #f))))
      ; a record's slots start with its type name, so a tag equal to that name counts as present, as in absent?.
      ((,u ⊣ (μkanren-structure? u)) (μkanren-verify-T+ (μkanren-structure->list u) tag s))
      (,u (μ T₀ (and (μkanren-tag-pred? tag u) T₀)))))

  (define (μkanren-verify-T T s)
    (μkanren-foldr/verify (λ (t T₀) ((μkanren-verify-T+ (lhs t) (rhs t) s) T₀)) T))

  (define (μkanren-verify-A/post D T s)
    (λ (A)
      (let1 (D* (μkanren-subsume A D))
        (cond 
          ((μkanren-verify-T T s) => (μkanren-verify-T/post D* A s))
          (else #f)))))

  (define (μkanren-ext-A α tag A0 s)
    (let L ((A A0))
      (match/first A
        (() `((,α . ,tag)))
        ((((,α* . ,tag*) . _) ⊣ (equal? (μkanren-state-find α* s) α)) (if (μkanren-tag-equal? tag tag*) '() #f))
        ((_ . ,A*) (L A*)))))

  (define (μkanren-verify-A+ α tag s)
    (μ A0
      (let1 (α* (μkanren-state-find α s))
        (cond
          ((μkanren-var-working? α*)  (cond
                                        ((μkanren-ext-A α* tag A0 s) => (μ A+ (append A+ A0)))
                                        (else #f)))
          (else (and (μkanren-tag-pred? tag α*) A0))))))

  (define (μkanren-verify-A A s)
    (μkanren-foldr/verify (λ (a A0) ((μkanren-verify-A+ (lhs a) (rhs a) s) A0)) A))

  (define (μkanren-verify-D/post D A T s)
    (cond
      ((μkanren-verify-A A s) => (μkanren-verify-A/post D T s))
      (else #f)))

  (define (μkanren-state-unify/assoc associations s assocs)
    (μkanren-state-unify (map lhs associations) (map rhs associations) s assocs))

  (define (μkanren-verify-D+ d D s)
    (cond
      ((μkanren-state-unify/assoc d s '()) => (λ1-match/first ((,s* ,d*) (if (μkanren-state-equal? s s*) #f (cons d* D)))))
      (else D)))

  (define (μkanren-verify-D D s)
    (μkanren-foldr/verify (λ (d D**) (μkanren-verify-D+ d D** s)) D))

  (define (μkanren-subsume-A tag vars D A s)
    (let1 (tags* (cons tag (μkanren-state-tags s)))
      (match/first vars
        (() (μkanren-state-with s D: D A: A tags: tags*))
        ((,α . _) (match1/first ((,D* . ,T*) (μkanren-update-D/T α D A (μkanren-state-T s) s))
                    (μkanren-state-with s D: D* A: A T: T* tags: tags*))))))

  (define (μkanren-make-tag-A+ α tag s)
    (let1 (A (μkanren-state-A s))
      (match/first (μkanren-ext-A α tag A s)
        (#f #f)
        (() s)
        (,A+  (let* ((D   (μkanren-state-D s))
                     (D*  (μkanren-subsume A+ D))
                     (A*  (append A+ A))
                     (vars (remove-duplicates (map lhs A*))))
                (μkanren-subsume-A tag vars D* A* s))))))

  (define (((μkanren-make-tag-A tag) u) s)
    (match/first (μkanren-state-find u s)
      ((,α ⊣ (μkanren-var-working? α))  (cond
                                          ((μkanren-make-tag-A+ α tag s) => ✓°)
                                          (else (✗° s))))
      ((,u* ⊣ (μkanren-tag-pred? tag u*)) (✓° s))
      (else (✗° s))))

  (define (μkanren-ext-T α tag T s)
    (match/first T
      (() `((,α . ,tag)))
      ((((,α* . ,tag*) . ,T*) ⊣ (equal? (μkanren-state-find α* s) α))
        (if (μkanren-tag-equal? tag tag*) '() (μkanren-ext-T α tag T* s)))
      ((((_ . ,tag*) . ,T*) ⊣ (μkanren-tag-equal? tag tag*)) (μkanren-ext-T+ α tag T* s))
      ((_ . ,T*) (μkanren-ext-T α tag T* s))))

  (define (μkanren-absento+ u tag D A T s)
      (match/first (μkanren-state-find u s)
        ((,α ⊣ (μkanren-var-working? α)) (let1 (T+ (μkanren-ext-T α tag T s))
                                            (if (null? T+) s (let ((D* (μkanren-subsume T+ D))
                                                                   (vars (remove-duplicates (map lhs A))))
                                                              (μkanren-subsume-T vars T+ D* A T s)))))
        ((,au . ,du) (let1 (s* (μkanren-absento+ au tag D A T s))
                        (and s* (μkanren-state-match ((vc* S* D* A* T* tags*) s*)
                                  (μkanren-absento+ du tag D* A* T* s*)))))
        ((,u* ⊣ (μkanren-structure? u*)) (μkanren-absento+ (μkanren-structure->list u*) tag D A T s))
        (,u* (and (μkanren-tag-pred? tag u*) s))))
  
  ; goals --------------------------------------------------------------------------

  ; a goal maps a state to a stream of states: `✓°` succeeds once with the given state, `✗°` fails.
  ; the `ª` suffix marks the applicative form of a goal constructor, `°` its syntactic form or a relation.
  (define ✓° list)
  (define ✗° (K '()))

  (define ((freshª f) s) ; ª means "applicative", so `freshª` is a *function* that consumes a function and returns a goal.
    (μkanren-state-match ((vc S D A T tags) s)
      (let* ((α   (make-μkanren-var vc))
             (g   (f α))
             (s*  (make-μkanren-state (add1 vc) S D A T tags)))
        (δ (g s*)))))

  (define ((=° u v) s)
    (μkanren-state-match ((vc S D A T tags) s)
      (define post°
        (λ1-match/first
          ((,s* _)  (cond
                      ((μkanren-state-equal? s s*) (✓° s*))
                      ((μkanren-verify-D D s*) => (μ D*
                                                    (cond
                                                      ((μkanren-verify-D/post D* A T s*) => ✓°)
                                                      (else (✗° s*)))))
                      (else (✗° s*))))))
      (cond
        ((μkanren-state-unify u v s '()) => post°)
        (else (✗° s)))))

  (define ((≠° u v) s)
    (μkanren-state-match ((vc S D A T tags) s)
      (define post°
        (λ1-match/first
          ((,s* ,d) (cond
                      ((μkanren-state-equal? s s*) (✗° s*))
                      (else (let* ((D*  (list d))
                                   (D*  (μkanren-subsume A D*))
                                   (D*  (μkanren-subsume T D*))
                                   (s** (μkanren-state-with s D: (append D* D))))
                              (✓° s**)))))))
      (cond
        ((μkanren-state-unify u v s '()) => post°)
        (else (✓° s)))))

  (define ((orª f g) s) (append§/interleaved/2 (δ (f s)) (δ (g s))))
  (define ((andª f g) s) (append-map§ g (δ (f s))))

  ; `(fresh° (α ...) g ...)` introduces fresh variables; `(fresh° α (β ...) g ...)` moreover binds α to the list `(β ...)`.
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

  ; soft cut: if `g?` has at least one answer, every answer of `g?` is continued with `gt`, otherwise `gf` runs.
  (define ((if° g? gt gf) s)
    (define (L §)
      (cond
        ((null? §) (δ (gf s)))
        ((promise? §) (δ (L (force §))))
        (else (append-map§ gt §))))
    (δ (L (g? s))))

  ; the first `n` answers of `g`; a negative `n` keeps all of them (`take§` in (aux stream) stops only when n reaches 0).
  (define (take° n g) (μ s (take§ n (δ (g s)))))

  (define (null° l) (=° l '()))
  (define (boolean° v) (or° (=° v #t) (=° v #f)))
  (define (cons° a d c) (=° c (cons a d)))

  (define-μkanren-tag (μkanren-tag/sym v) (symbol? v))
  (define-μkanren-tag (μkanren-tag/num v) (number? v))
  
  (define symbol° (μkanren-make-tag-A μkanren-tag/sym))
  (define number° (μkanren-make-tag-A μkanren-tag/num))

  (define ((absent° tag u) s)
    (μkanren-state-match ((vc S D A T tags) s)
      (let* ((pred? (λ (v) (not (equal? tag v))))
             (tag* (make-μkanren-tag tag 'no-def pred?)))
        (cond
          ((μkanren-absento+ u tag* D A T s) => ✓°)
          (else (✗° s))))))

  (define-syntax-rule (project° ((v α) ...) g ...)
    (μ s (let* ((v (μkanren-state-find/value α s)) ...) (δ ((and° g ...) s)))))
  
  (define-syntax-rule (cond° (g ...) ...) (or° (and° g ...) ...))

  ; the values of `vars` in the state `s`, as a list (the key of a group).
  (define (μkanren-state-find/values vars s) (map (λ (v) (μkanren-state-find/value v s)) vars))

  ; folds the stream `§` into a hash table `key → hash table (var → values)`, collecting the value of each of `vars`.
  (define (μkanren-group§ key-of vars §)
    (foldr§ (λ (s* H)
              (hash-table-update!/default H (key-of s*)
                (λ (group)
                  (for-each (λ (v) (hash-table-update!/default group v (λ (vs) (cons (μkanren-state-find/value v s*) vs)) '())) vars)
                  group)
                (make-hash-table))
              H)
            (make-hash-table) §))

  (define-syntax-rule (literal over from =>) (groupby° (((v* aggr) v) ...) over (k ...) from g => f ...)
    (λ (s)
        (let* ((ht (μkanren-group§ (λ (s*) (μkanren-state-find/values (list k ...) s*)) (list v ...) (g s)))
               (G (λ (key group folded) (or° (let ((v* (aggr (hash-table-ref group v))) ...) (receive (k ...) (apply values key) (and° f ...))) folded)))
               (g* (hash-table-fold ht G ✗°)))
          (δ (g* s)))))

  (define-syntax-rule (literal over from =>) (window° (((v* aggr) v) ...) over (k ...) from g => f ...)
    (λ (s)
        (let* ((§ (δ (g s))) ; the stream to fold over
               (key-of (λ (s*) (μkanren-state-find/values (list k ...) s*)))
               (ht (μkanren-group§ key-of (list v ...) §))
               (G (λ (s*) (let* ((group (hash-table-ref ht (key-of s*)))
                                 (v* (aggr (hash-table-ref group v))) ...)
                            ((and° f ...) s*)))))
          (append-map§ G §))))

  (define-syntax-rule (literal over from =>) 
    (set° (v aggr init₀) over ((k* k) ...) from g => f ...)
      (λ (s)
        (let* ((§ (δ (g s))) ; the stream to fold over
              (A aggr) ; to evaluate `aggr` only once
              (init init₀) ; to evaluate `init` only once
              (F (λ (s* H)
                    (let1 (key (μkanren-state-find/values (list k ...) s*))
                      (hash-table-update!/default H key (λ (u) (apply A `(,@key ,u))) init))
                    H))
              (ht (foldr§ F (make-hash-table) §))
              (G (λ (key v folded) (or° (receive (k* ...) (apply values key) (and° f ...)) folded)))
              (g* (hash-table-fold ht G ✗°)))
          (δ (g* s)))))

  (define-syntax-rule (literal over from =>) 
    (enumerate° (v aggr) over (k ...) from g => f ...)
      (λ (s)
        (let* ((§ (δ (g s))) ; the stream to fold over
               (A aggr) ; to evaluate `aggr` only once
               (keys (foldr§ (λ (s* H) (cons (μkanren-state-find/values (list k ...) s*) H)) '() §))
               (v (map (λ (i key) (apply A i key)) (ι (length keys)) keys)))
           (δ ((and° f ...) s)))))

  ; API ------------------------------------------------------------------------

  (define-syntax-rule (define-relation (name arg ...) g ...) (define (name arg ...) (and° g ...)))

  (define (°->§ g)
    (let* ((§ (δ (g μkanren-state-empty)))
           (P (μkanren-project (make-μkanren-var 0))))
      (map§ P §)))

  ; the environment in which grounded answers are evaluated: the repr only needs `cons`, `vector`, `quote`
  ; and `make-record-instance`, all imported here. (`τ` delays the lookup to call time.)
  (define μkanren-answer-environment (τ (module-environment 'aux.kanren.micro)))

  (define (°->list grounded g)
    (let1 (sols (§->list (°->§ g)))
      (cond
        (grounded (map (λ (expr)
                         (let ((E (eval `(lambda ,(cadr expr) ,(last expr)) (μkanren-answer-environment)))
                               (args (cadr expr)))
                           (apply E args)))
                       sols))
        (else sols))))

  (define (°->list/ground g) (°->list #t g))

  (define-syntax-rule (μkanren-run (v n grounded) g ...) (°->list grounded (take° n (fresh° (v) g ...))))
)