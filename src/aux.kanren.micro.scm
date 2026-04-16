

(module (aux kanren micro) *

  (import scheme 
          (chicken base)
          (chicken sort)
          (chicken memory representation)
          (chicken pretty-print)
          srfi-1 srfi-69 vector-lib
          (aux base)
          (aux stream)
          (aux fds sbral))
  
  (define-record μkanren-unbound) ; this implicitly defines the predicate μkanren-unbound? and the constructor make-μkanren-unbound.
  (define *μkanren-unbound* (make-μkanren-unbound))

  (define-record μkanren-tag name def pred)

  ; (set-record-printer! μkanren-tag (λ (tag port) (pretty-print (μkanren-tag-def tag) port)))

  (define-syntax-rule (define-μkanren-tag (name s) body ...)
    (define name (make-μkanren-tag (gensym 'name) '(μ s body ...) (μ s body ...))))
  
  (define (μkanren-tag-pred? tag v) (let1 (pred? (μkanren-tag-pred tag)) (pred? v)))
  (define (μkanren-tag-equal? tag1 tag2) (equal? (μkanren-tag-name tag1) (μkanren-tag-name tag2)))

  ; variables --------------------------------------------------------------------

  (define-record μkanren-var index)

  (set-record-printer! μkanren-var (λ (α port) (display (μkanren-var->symbol α) port)))

  (define (μkanren-var-working? α) (and (μkanren-var? α) (<= 0 (μkanren-var-index α))))
  (define (μkanren-var-reified? α) (and (μkanren-var? α) (> 0 (μkanren-var-index α))))

  (define (μkanren-var-index>=0 α) (let1 (i (μkanren-var-index α)) (if (μkanren-var-working? α) i (- (add1 i)))))

  (define (μkanren-var->symbol α)
    (let1 (i (μkanren-var-index>=0 α))
      (cond 
        ((μkanren-var-working? α) (string->symbol (string-append "_" (number->string i))))
        (else (vector-ref greek-alphabet/lowercase i)))))

  ; state ------------------------------------------------------------------------

  (define-record μkanren-state vars-count S D A T tags)

  ; (set-record-printer! μkanren-state (λ (s port) (display (->string/pretty-print `((vars count ,(μkanren-state-vars-count s)) (S ,(sbral->list (μkanren-state-S s))) (D ,(μkanren-state-D s)))) port)))

  (define μkanren-state-empty (make-μkanren-state 0 empty/sbral empty/sbral empty/sbral empty/sbral '()))

  (define (μkanren-state-equal-S? s s*) (eq? s s*))

  (define (μkanren-var-index/sbral sbral)
    (let1 (l (length/sbral sbral))
      (μ α (- l 1 (μkanren-var-index>=0 α)))))

  (define (μkanren-sbral-ref/var sbral)
    (let1 (index-of-var (μkanren-var-index/sbral sbral))
      (μ α (sbral-ref sbral (index-of-var α)))))

  (define (μkanren-var-extend/sbral α S default)
    (let ((i (μkanren-var-index>=0 α))
          (l (length/sbral S)))
      (let U ((l* l) (S* S))
        (cond
          ((<= l* i) (U (add1 l*) (cons/sbral default S*)))
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
    (let* ((sbral*        (μkanren-var-extend/sbral α sbral *μkanren-unbound*))
           (index-of-var  (μkanren-var-index/sbral sbral*))
           (i             (index-of-var α)))
      (update/sbral i v sbral*)))

  (define (μkanren-state-update α v s #!key (unsafe #f))
    
    (define (occur? v*)
      (cond
        ((μkanren-var? v*) (equal? α v*))
        ((pair? v*) (or (occur? (μkanren-state-find (car v*) s)) (occur? (μkanren-state-find (cdr v*) s))))
        ((vector? v*) (occur? (vector->list v*)))
        ((record-instance? v*) (occur? (record->vector v*)))
        (else #f)))
    
    (and
      (or unsafe (not (occur? v)))
      (μkanren-state-match ((vc S D A T tags) s)
        (let1 (S* (μkanren-update/sbral α v S))
          (make-μkanren-state vc S* D A T tags)))))

  (define (μkanren-state-find α s)
    (let* ((S (μkanren-state-S s))
           (l (length/sbral S))
           (var-deferred? (μ α (and (μkanren-var? α) (<= l (μkanren-var-index>=0 α)))))
           (ref-var (μkanren-sbral-ref/var S)))
      (let F ((β0 α) (β α))
        (cond
          ((var-deferred? β) β)
          ((μkanren-var-working? β) (F β (ref-var β)))
          ((μkanren-unbound? β) β0)
          (else β)))))

  (define (μkanren-state-unify u v s)
    (let ((u* (μkanren-state-find u s)) (v* (μkanren-state-find v s)))
      (cond
        ((eq? u* v*) s)
        ((and (string? u*) (string? v*) (string=? u* v*)) s)
        ((and (number? u*) (number? v*) (= u* v*)) s)
        ((and (μkanren-var-working? u*) (μkanren-var-working? v*)) 
          (if (< (μkanren-var-index>=0 u*) (μkanren-var-index>=0 v*))
            (μkanren-state-update u* v* s)
            (μkanren-state-update v* u* s)))
        ((μkanren-var-working? u*) (μkanren-state-update u* v* s))
        ((μkanren-var-working? v*) (μkanren-state-update v* u* s))
        ((and (vector? u*) (vector? v*)) (μkanren-state-unify (vector->list u*) (vector->list v*) s))
        ((and (record-instance? u*) (record-instance? v*)) (μkanren-state-unify (record->vector u*) (record->vector v*) s))
        ((and (vector? u*) (record-instance? v*)) (μkanren-state-unify u* (record->vector v*) s))
        ((and (record-instance? u*) (vector? v*)) (μkanren-state-unify (record->vector u*) v* s))
        ((and (pair? u*) (pair? v*)) 
          (let1 (s* (μkanren-state-unify (car u*) (car v*) s)) 
            (and (μkanren-state? s*) (μkanren-state-unify (cdr u*) (cdr v*) s*))))
        (else #f))))

  (define (μkanren-state-find/value v s)
    (let A ((w v))
      (let ((A* (o (map/curry A) vector->list record->vector))
            (w* (μkanren-state-find w s)))
        (cond
          ((pair? w*) (cons (A (car w*)) (A (cdr w*))))
          ((vector? w*) (vector-map A w*))
          ((and (record-instance? w*) (not (μkanren-var? w*))) (apply make-record-instance (A* w*)))
          (else w*)))))

  (define (μkanren-state-find/repr v s)
    (let A ((w v))
      (let ((A* (o (map/curry A) vector->list record->vector))
            (w* (μkanren-state-find w s)))
        (cond
          ((μkanren-var? w*) (μkanren-var->symbol w*))
          ((symbol? w*) (list 'quote w*))
          ((null? w*) (list 'quote '()))
          ((pair? w*) (list 'cons (A (car w*)) (A (cdr w*))))
          ((vector? w*) (cons 'vector (map A (vector->list w*))))
          ((record-instance? w*) (cons 'make-record-instance (A* w*)))
          (else w*)))))

  (define (μkanren-state-reify v s)
    (let R ((w v) (r s) (c -1) (vars '()))
      (let1 (w* (μkanren-state-find w r))
        (cond
          ((μkanren-var-working? w*)  (let* ((v* (make-μkanren-var c))
                                            (r* (μkanren-state-update w* v* r))
                                            (c* (sub1 c))
                                            (vars* (cons v* vars)))
                                        (R (void) r* c* vars*))) ; `(void)` will stop the recursion in the next call.
          ((pair? w*) (match1/first ((,r* ,c* ,vars*) (R (car w*) r c vars)) (R (cdr w*) r* c* vars*)))
          ((vector? w*) (R (vector->list w*) r c vars))
          ((record-instance? w*) (R (record->vector w*) r c vars))
          (else (list r c vars))))))

  (define (μkanren-drop-dot-D D s)
    (let* ((pair->deny (λ1-match/first ((,α . ,u) `(deny (equal? ,(μkanren-var->symbol α) ,(μkanren-state-find/repr u s))))))
           (group->deny-list (map/curry pair->deny)))
      (map (λ (d) `(begin ,@(group->deny-list d))) D)))
  
  (define μkanren-drop-dot-T (map/curry (λ1-match/first ((,α . ,tag) `(,tag ,α)))))

  (define μkanren-sort-part (λ1-match/first ((,tag . ,vars) `(,tag . ,(sort/lex<=? vars)))))

  (define (μkanren-part tag A vars A0)
    (match/first A
      (() (cons `(,tag . ,vars) (μkanren-partition* A0)))
      ((((,α . ,tag*) . ,A*) ⊣ (μkanren-tag-equal? tag tag*))
        (let1 (vars* (if (member? α vars) vars (cons α vars)))
          (μkanren-part tag A* vars* A0)))
      ((,a . ,A*) (let1 (A1 (cons a A0)) (μkanren-part tag A* vars A1)))))

  (define (μkanren-partition* A)
    (match/first A
      (() '())
      (((_ . ,tag) . _) (μkanren-part tag A '() '()))))

  (define (μkanren-form v D A T s)
    (let* ((fd (μkanren-drop-dot-D (sort/lex<=? (map sort/lex<=? D)) s))
           (FA (λ1-match/first ((,tag . ,vars) `(assert (every ,(μkanren-tag-def tag) (list ,@(map μkanren-var->symbol vars)))))))
           (fa (map FA (sort/lex<=? (map μkanren-sort-part (μkanren-partition* A)))))
           (FT (λ1-match/first ((,tag ,var) `(assert (absent? (quote ,(μkanren-tag-name tag)) ,(μkanren-var->symbol var))))))
           (ft (map FT (μkanren-drop-dot-T (sort/lex<=? T)))))
        `(,@fd ,@fa ,@ft ,v)))

  (define (μkanren-subsumed-T? x tag T)
    (match/first T
      (() #f)
      (((,y . ,tag*) . ,T*) (or 
                              (and (equal? x y) (μkanren-tag-equal? tag tag*))
                              (μkanren-subsumed-T? x tag T*)))))

  (define (μkanren-rem-subsumed-T T0)
      (let loop ((T T0) (Tˆ '()))
        (match/first T
          (() Tˆ)
          ((((,x . ,tag) . ,T*) ⊣ (or (μkanren-subsumed-T? x tag T*) (μkanren-subsumed-T? x tag Tˆ))) (loop T* Tˆ))
          ((,t . ,T*) (loop T* (cons t Tˆ))))))

  (define (μkanren-anyvar? s)
    (define anyvar? (λ1-match/first
                      ((,a . ,d) (or (anyvar? a) (anyvar? d)))
                      ((,α ⊣ (μkanren-var-working? α)) (μkanren-var-working? (μkanren-state-find α s)))
                      ((,v ⊣ (vector? v)) (anyvar? (vector->list v)))
                      ((,r ⊣ (record-instance? r)) (anyvar? (record->vector r)))
                      (else #f)))
    anyvar?)

  (define (μkanren-subsumed? d D)
    (let* ((f (λ (p s) (match/first p ((,α . ,u) (μkanren-state-update α u s)))))
           (s (foldr f μkanren-state-empty d)))
      (match/first D
        (() #f)
        ((,d* . ,D*)  (let1 (d** (μkanren-state-unify/assoc d* s))
                        (or (and d** (equal? d** d)) (μkanren-subsumed? d D*)))))))

  (define (μkanren-rem-subsumed D0)
    (let loop ((D D0) (D+ '()))
      (match/first D 
        (() D+)
        (((,d . ,D*) ⊣ (or (μkanren-subsumed? d D*) (μkanren-subsumed? d D+))) (loop D* D+))
        ((,d . ,D*) (loop D* (cons d D+))))))

  (define ((μkanren-project w) s)
    (let1 (w* (μkanren-state-find/value w s))
      (match1/first ((,s* _ ,vars-reversed) (μkanren-state-reify w* s))
        (let* ((R     (λ1-match/first ((,α . _) (μkanren-var-working? (μkanren-state-find α s*)))))
               (vars  (reverse vars-reversed))
               (vars* (map μkanren-var->symbol vars))
               (w**   (μkanren-state-find/repr w* s*))
               (D     (μkanren-state-D s*))
               (A     (μkanren-state-A s*))
               (T     (μkanren-state-T s*))
               #;(D*    (μkanren-rem-subsumed (remove (μkanren-anyvar? s*) D))) ; needs more revision the stuff with T.
               (D*    (remove (μkanren-anyvar? s*) D))
               (A*    (remove R A))
               (T*    (remove R T))
               (D**   (μkanren-state-find/value (μkanren-subsume A* D*) s*))
               (A**   (μkanren-state-find/value A* s*))
               (T**   (μkanren-rem-subsumed-T (μkanren-state-find/value T* s*)))
               (body  (μkanren-form w** D** A** T** s*)))
          `(λ ,vars* ,@body)))))

  ; constraints -------------------------------------------------------------------

  (define (μkanren-ext-D α tag D s) ; ✓
    (let1 (exists? (exists (λ1-match/first
                            (((,α* . ,tag*))  (and 
                                                (equal? α (μkanren-state-find α* s))
                                                (μkanren-tag? tag*)
                                                (μkanren-tag-equal? tag tag*)))
                            (else #f))))
      (cond
        ((exists? D) D)
        (else (cons `((,α . ,tag)) D)))))

  (define (μkanren-update-D/T+ α T+ D T s) ; ✓
    (match/first T
      (() `(,D . ,T+))
      ((((,α* . ,tag) . ,T*) ⊣ (equal? α α*)) (let1 (D* (μkanren-ext-D α tag D s)) (μkanren-update-D/T+ α T+ D* T* s)))
      ((,t . ,T*) (let1 (T+* (cons t T+)) (μkanren-update-D/T+ α T+* D T* s)))))

  (define (μkanren-update-D/T α D A T s) ; ✓
    (let ((equal/α? (μ t (equal? (lhs t) α)))
          (tags (μkanren-state-tags s)))
      (match/first A
        (() (let1 (T* (remove equal/α? T)) `(,D . ,T*)))
        ((((,α* . ,tag) . _) ⊣ (and (equal? α* α) (member? tag tags))) (μkanren-update-D/T+ α '() D T s))
        ((_ . ,A*) (μkanren-update-D/T α D A* T s)))))

  (define (μkanren-subsume-T vars T+ D A T s) ; ✓
    (match/first vars
      (() (let* ((T* (append T+ T))
                 (vc (μkanren-state-vars-count s))
                 (S (μkanren-state-S s))
                 (tags (μkanren-state-tags s)))
            (make-μkanren-state vc S D A T* tags)))
      ((,α . ,vars*)  (match1/first ((,D* . ,T+*) (μkanren-update-D/T α D A T+ s))
                        (μkanren-subsume-T vars* T+* D* A T s)))))

  (define (μkanren-subsumed-pr? A/T) ; ✓
    (λ1-match/first
      (((_ . ,α) ⊣ (μkanren-var-working? α)) #f)
      ((,α . ,u)  (match/first (assoc α A/T)
                    ((_ . ,u*)  (cond
                                  ((and (μkanren-tag? u*) (μkanren-tag? u) (μkanren-tag-equal? u u*)))
                                  ((and (μkanren-tag? u*) (μkanren-tag-pred? u* u)) #f)
                                  (else #t)))
                    (else #f)))))

  (define (μkanren-subsume A-or-T D) ; ✓
    (remove (exists (μkanren-subsumed-pr? A-or-T)) D))

  (define (μkanren-verify-T/post D A s) ; ✓
    (let1 (vars (remove-duplicates (map lhs A)))
      (μ T (μkanren-subsume-T vars T (μkanren-subsume T D) A '() s))))

  (define (μkanren-ext-T+ α tag T s) ; ✓
    (match/first T
      (() `((,α . ,tag)))
      ((((,α* . ,tag*) . _) ⊣ (and (equal? (μkanren-state-find α* s) α) (μkanren-tag-equal? tag tag*))) '())
      ((_ . ,T*) (μkanren-ext-T+ α tag T* s))))

  (define (μkanren-verify-T+ α T s) ; ✓
    (match1/first (((_ . ,tag) . _) T)
      (match/first (μkanren-state-find α s)
        ((,α* ⊣ (μkanren-var-working? α*))  (μ T₀
                                      (cond
                                        ((μkanren-ext-T+ α* tag T₀ s) => (μ T+ (append T+ T₀)))
                                        (else #f))))
        ((,au . ,du)  (μ T₀
                        (cond
                          (((μkanren-verify-T+ au T s) T₀) => (μkanren-verify-T+ du T s))
                          (else #f))))
        ; perhaps we should also handle vectors and record-instances here, but for now we only support tags on variables and conses.
        (,u (μ T₀ (and (μkanren-tag-pred? tag u) T₀))))))

  (define (μkanren-verify-T T s) ; ✓
    (match/first T
      (() '())
      ((((,α . _) . ,T*) ⊣ (μkanren-verify-T T* s)) => (μkanren-verify-T+ α T s))
      (else #f)))

  (define (μkanren-verify-A/post D T s) ; ✓
    (λ (A)
      (let1 (D* (μkanren-subsume A D))
        (cond 
          ((μkanren-verify-T T s) => (μkanren-verify-T/post D* A s))
          (else #f)))))

  (define (μkanren-ext-A α tag A0 s) ; ✓
    (let L ((A A0))
      (match/first A
        (() `((,α . ,tag)))
        ((((,α* . ,tag*) . _) ⊣ (equal? (μkanren-state-find α* s) α)) (if (μkanren-tag-equal? tag tag*) '() #f))
        ((_ . ,A*) (L A*)))))

  (define (μkanren-verify-A A s) ; ✓
    (match/first A
      (() '())
      ((((,α . ,tag) . ,A*) ⊣ (μkanren-verify-A A* s)) =>
          (μ A0
            (let1 (α* (μkanren-state-find α s))
              (cond
                ((μkanren-var-working? α*)  (cond
                                              ((μkanren-ext-A α* tag A0 s) => (μ A+ (append A+ A0)))
                                              (else #f)))
                (else (and (μkanren-tag-pred? tag α*) A0))))))
      (else #f)))

  (define (μkanren-verify-D/post D A T s) ; ✓
    (cond
      ((μkanren-verify-A A s) => (μkanren-verify-A/post D T s))
      (else #f)))

  (define (μkanren-state-unify/assoc associations s) ; ✓
    (μkanren-state-unify (map lhs associations) (map rhs associations) s))

  (define (μkanren-prefix-sbral->list S* S) ; ✓
    (let* (#;(l (length/sbral S))
           (l* (length/sbral S*))
           (M (λ (i each lst)
                (cond
                  ;((and (< i l) (eq? each (sbral-ref S i))) lst)
                  ((μkanren-unbound? each) lst)
                  (else (let1 (α (make-μkanren-var (- l* i 1))) (cons `(,α . ,each) lst)))))))
      (foldr/sbral M '() S*)))

  (define (μkanren-verify-D+ d D s) ; ✓
    (cond
      ((μkanren-state-unify/assoc d s) => (μ s*
                                            (cond
                                              ((μkanren-state-equal-S? s s*) #f)
                                              (else (let* ((S (μkanren-state-S s))
                                                           (S* (μkanren-state-S s*))
                                                           (d* (μkanren-prefix-sbral->list S* S)))
                                                      (cons d* D))))))
      (else D)))

  (define (μkanren-verify-D D s) ; ✓
    (match/first D
      (() '())
      (((,d . ,D*) ⊣ (μkanren-verify-D D* s)) => (μ D** (μkanren-verify-D+ d D** s)))
      (else #f)))

  (define (μkanren-subsume-A tag vars D A s) ; ✓
    (let* ((vc (μkanren-state-vars-count s))
           (S (μkanren-state-S s))
           (T (μkanren-state-T s))
           (tags (μkanren-state-tags s))
           (tags* (cons tag tags)))
    (match/first vars
      (() (make-μkanren-state vc S D A T tags*))
      ((,α . _) (match1/first ((,D* . ,T*) (μkanren-update-D/T α D A T s))
                  (make-μkanren-state vc S D* A T* tags*))))))

  (define (μkanren-make-tag-A+ α tag s) ; ✓
    (let1 (A (μkanren-state-A s))
      (match/first (μkanren-ext-A α tag A s)
        (#f #f)
        (() s)
        (,A+  (let* ((D   (μkanren-state-D s))
                     (D*  (μkanren-subsume A+ D))
                     (A*  (append A+ A))
                     (vars (remove-duplicates (map lhs A*))))
                (μkanren-subsume-A tag vars D* A* s))))))

  (define (((μkanren-make-tag-A tag) u) s) ; ✓
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
        (,u* (if (and (symbol? u*) (equal? u* (μkanren-tag-name tag))) #f s))))
  
  ; goals --------------------------------------------------------------------------

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
      (define (post° s*)
        (cond
          ((μkanren-state-equal-S? s s*) (✓° s*))
          ((μkanren-verify-D D s*) => (μ D*
                                        (cond
                                          ((μkanren-verify-D/post D* A T s*) => ✓°)
                                          (else (✗° s*)))))
          (else (✗° s*))))
      (cond
        ((μkanren-state-unify u v s) => post°)
        (else (✗° s)))))

  (define ((≠° u v) s)
    (μkanren-state-match ((vc S D A T tags) s)
      (define (post° s*)
        (cond
          ((μkanren-state-equal-S? s s*) (✗° s*))
          (else (let* ((S*  (μkanren-state-S s*))
                       (d   (μkanren-prefix-sbral->list S* S))
                       (_ (display/pp d))
                       (D*  (list d))
                       (D*  (μkanren-subsume A D*))
                       (D*  (μkanren-subsume T D*))
                       (D*  (append D* D))
                       (s** (make-μkanren-state vc S D* A T tags)))
                  (✓° s**)))))
      (cond
        ((μkanren-state-unify u v s) => post°)
        (else (✓° s)))))

  (define ((orª f g) s) (append§/interleaved/2 (δ (f s)) (δ (g s))))
  (define ((andª f g) s) (append-map§ g (δ (f s))))

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
        ((null? §) (δ (gf s)))
        ((promise? §) (δ (L (force §))))
        (else (append-map§ gt §))))
    (δ (L (g? s))))

  (define (take° n g) (μ s (take§ n (δ (g s)))))

  (define (null° l) (=° l '()))
  (define (boolean° v) (or° (=° v #t) (=° v #f)))
  (define (cons° a d c) (=° c (cons a d)))

  (define-μkanren-tag (μkanren-tag/sym v) (symbol? v))
  (define-μkanren-tag (μkanren-tag/num v) (number? v))
  
  (define symbol° (μkanren-make-tag-A μkanren-tag/sym))
  (define number° (μkanren-make-tag-A μkanren-tag/num))

  (define (absent° tag u)
    (cond
      ((not (symbol? tag)) ✗°)
      (else (λ (s)
              (μkanren-state-match ((vc S D A T tags) s)
                (let* ((pred? (λ (v) (not (equal? tag v))))
                       (tag* (make-μkanren-tag tag 'no-def pred?)))
                  (cond
                    ((μkanren-absento+ u tag* D A T s) => ✓°)
                    (else (✗° s)))))))))

  (define-syntax-rule (project° ((v α) ...) g ...)
    (μ s (let* ((v (μkanren-state-find/value α s)) ...) (δ ((and° g ...) s)))))
  
  (define-syntax-rule (cond° (g ...) ...) (or° (and° g ...) ...))

  (define-syntax-rule (literal over from =>) (groupby° (((v* aggr) v) ...) over (k ...) from g => f ...)
    (λ (s)
        (let* ((§ (δ (g s))) ; the stream to fold over
               (F (λ (s* H)
                      (hash-table-update!/default H  
                                                  (list (μkanren-state-find/value k s*) ...) ; key
                                                  (λ (group) (hash-table-update!/default group v (λ (vs) (cons (μkanren-state-find/value v s*) vs)) '()) ... group) ; value
                                                  (make-hash-table))
                      H))
               (ht (foldr§ F (make-hash-table) §))
               (G (λ (key group folded) (or° (let ((v* (aggr (hash-table-ref group v))) ...) (receive (k ...) (apply values key) (and° f ...))) folded)))
               (g* (hash-table-fold ht G ✗°)))
          (δ (g* s)))))

  (define-syntax-rule (literal over from =>) (window° (((v* aggr) v) ...) over (k ...) from g => f ...)
    (λ (s)
        (let* ((§ (δ (g s))) ; the stream to fold over
               (F (λ (s* H)
                      (hash-table-update!/default H  
                                                  (list (μkanren-state-find/value k s*) ...) ; key
                                                  (λ (group) (hash-table-update!/default group v (λ (vs) (cons (μkanren-state-find/value v s*) vs)) '()) ... group) ; value
                                                  (make-hash-table))
                      H))
               (ht (foldr§ F (make-hash-table) §))
               (G (λ (s*) (let* ((group (hash-table-ref ht (list (μkanren-state-find/value k s*) ...)))
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
                    (let1 (key (list (μkanren-state-find/value k s*) ...))
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
               (i 0)
               (F (λ (s* H)
                   (let1 (key (list i (μkanren-state-find/value k s*) ...))
                     (add1! i)
                     (cons (apply A key) H))))
               (v (reverse (foldr§ F '() §))))
           (δ ((and° f ...) s)))))

  ; API ------------------------------------------------------------------------

  (define-syntax-rule (define-relation (name arg ...) g ...) (define (name arg ...) (and° g ...)))

  (define (°->§ g)
    (let* ((§ (δ (g μkanren-state-empty)))
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