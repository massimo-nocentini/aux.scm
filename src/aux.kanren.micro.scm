

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
          (chicken fixnum)
          (only (chicken sort) sort)
          (only (chicken string) ->string)
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
  ; `(α . tag)` pairs, where T's α may also be a compound term, see `μkanren-tag-unify`); `tags` the tags
  ; asserted so far through `A`. D, A and T are plain lists; besides S, only the marks `M` below are an sbral.
  ;
  ; `M` marks the variables the stores mention, so that a =° binding none of them can skip verifying the
  ; stores (see `μkanren-quiet?`): an sbral indexed like S (see `sbral-ref/back`, a missing slot reads 0) of bitmasks, where
  ; - `μkanren-mark/D` marks the left side, or the right side when a variable, of a pair of some prefix of D,
  ;   that is, a variable whose binding makes `μkanren-verify-D/touched` verify that prefix again;
  ; - `μkanren-mark/A` marks a key of A;
  ; - `μkanren-mark/T` marks a variable key of T, and every variable reachable from a compound key of T
  ;   or from the compound datum of its tag (both walked by `μkanren-tag-unify`).
  ; `M` may be #f, which is no knowledge at all: such a state verifies its stores as a whole at every =°.
  ; `dirty` is #f only when no prefix of D is subsumed by A or T: a conversion of T into D (see
  ; `μkanren-ext-D`) leaves a prefix like `((α . 5))` under `(symbol° α)`, which the next =° removes.
  ; Past the quiet test, the marks spare the searches of the stores for a variable they do not mention, see
  ; `μkanren-make-tag-A+`, `μkanren-absento+`, `μkanren-subsume` and `μkanren-subsume-T`; within one
  ; verification, `μkanren-verify-A` and `μkanren-verify-T` find the keys of the store they rebuild in a table.
  ;
  ; The stores keep these invariants, on which skipping their verification rests:
  ; I1. the keys of A are distinct unbound working variables;
  ; I2. the left sides of the pairs of D are unbound, and their right sides are non-variables or unbound;
  ; I3. the variable keys of T are unbound and none is a key of A, and no two entries of T have equal
  ;     keys and equal tags;
  ; I4. M over-approximates: marks only grow along a branch, and a stale mark costs time, not answers;
  ; I5. when `dirty` is #f, neither A nor T subsumes a prefix of D;
  ; I6. the answer of `μkanren-tag-unify` on a compound key of T changes only when a T-marked variable is bound.
  (define-record-type μkanren-state
    (make-μkanren-state/M vars-count S D A T tags M dirty)
    μkanren-state?
    (vars-count μkanren-state-vars-count) (S μkanren-state-S) (D μkanren-state-D) (A μkanren-state-A)
    (T μkanren-state-T) (tags μkanren-state-tags) (M μkanren-state-M) (dirty μkanren-state-dirty))

  ; a state built by hand knows nothing about its stores (M is #f, see above).
  (define (make-μkanren-state vars-count S D A T tags) (make-μkanren-state/M vars-count S D A T tags #f #t))

  (define μkanren-state-empty (make-μkanren-state/M 0 empty/sbral '() '() '() '() empty/sbral #f))

  ; a copy of `s` with the given `vars-count` and `S`, which keeps the marks: only `freshª` and the
  ; unification inside =° call it, and =° verifies the stores of its answer, see `μkanren-state-unify`.
  (define (μkanren-state-with/S s vc S)
    (make-μkanren-state/M vc S (μkanren-state-D s) (μkanren-state-A s) (μkanren-state-T s) (μkanren-state-tags s)
                          (μkanren-state-M s) (μkanren-state-dirty s)))

  (define (μkanren-state-equal? s s*) (eq? s s*))

  (define μkanren-state-keep (list 'keep)) ; the default of the keys of `μkanren-state-with`, unique under `eq?`.

  ; a copy of `s` where the given fields are replaced. A new substitution or new stores given without `M:`
  ; may break the marks, so the copy drops them, unless `M:` is given too, as it is by every call in here.
  (define (μkanren-state-with s #!key
                              (vars-count (μkanren-state-vars-count s))
                              (S μkanren-state-keep)
                              (D μkanren-state-keep)
                              (A μkanren-state-keep)
                              (T μkanren-state-keep)
                              (tags (μkanren-state-tags s))
                              (M μkanren-state-keep)
                              (dirty μkanren-state-keep))
    (let* ((kept? (λ (v) (eq? v μkanren-state-keep)))
           (field (λ (v get) (if (kept? v) (get s) v)))
           (blind? (and (kept? M) (not (and (kept? S) (kept? D) (kept? A) (kept? T)))))
           (M* (if blind? #f (field M μkanren-state-M))))
      (make-μkanren-state/M vars-count (field S μkanren-state-S) (field D μkanren-state-D) (field A μkanren-state-A)
                            (field T μkanren-state-T) tags M*
                            (cond ((not (kept? dirty)) dirty) (blind? #t) (else (μkanren-state-dirty s))))))

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

  ; marks, see the comment of the state record.
  (define μkanren-mark/D 1)
  (define μkanren-mark/A 2)
  (define μkanren-mark/T 4)

  ; the marks of the working variable α.
  (define (μkanren-marks M α) (sbral-ref/back M (μkanren-var-index α) 0))

  ; whether the marks `m` of a variable hold `bit`.
  (define (μkanren-mark? m bit) (not (fx= 0 (fxand m bit))))

  ; whether the working variable α may hold the mark `bit`: always so when M is #f, which knows nothing.
  (define (μkanren-marked? M α bit) (or (not M) (μkanren-mark? (μkanren-marks M α) bit)))

  (define (μkanren-mark M α bit)
    (cond
      ((or (not M) (not (μkanren-var-working? α))) M)
      (else (let1 (m (μkanren-marks M α))
              (if (fx= bit (fxand m bit)) M (update/sbral/back (μkanren-var-index α) (fxior m bit) 0 M))))))

  ; marks the variables of the prefix `d` of D, as `μkanren-verify-D/touched` reads them.
  (define (μkanren-mark-prefix M d)
    (fold (λ (p M) (let1 (M* (μkanren-mark M (lhs p) μkanren-mark/D))
                     (if (μkanren-var? (rhs p)) (μkanren-mark M* (rhs p) μkanren-mark/D) M*)))
          M d))

  ; marks the prefixes that `D+` holds in front of its tail `D`.
  (define (μkanren-mark-prefixes M D+ D)
    (let F ((L D+) (M M))
      (if (eq? L D) M (F (cdr L) (μkanren-mark-prefix M (car L))))))

  ; marks every unbound variable reachable from the term `u` in `s`, as a key of T.
  (define (μkanren-mark-T M u s)
    (let* ((S (μkanren-state-S s)) (l (length/sbral S)))
      (let R ((w u) (M M))
        (let1 (w* (μkanren-state-find/SL w S l))
          (cond
            ((μkanren-var? w*) (μkanren-mark M w* μkanren-mark/T))
            ((pair? w*) (R (cdr w*) (R (car w*) M)))
            ((μkanren-structure? w*) (fold R M (μkanren-structure->list w*)))
            (else M))))))

  ; a vector or a record instance is walked as the list of its slots (a record's first slot is its type tag).
  (define (μkanren-structure? v) (or (vector? v) (record-instance? v)))
  (define (μkanren-structure->list v) (vector->list (if (vector? v) v (record->vector v))))

  (define (μkanren-state-update α v s assocs #!key (unsafe #f))
    
    (define (occur? v*)
      (cond
        ((μkanren-var? v*) (equal? α v*))
        ((μkanren-compound? v*)
          (let* ((S (μkanren-state-S s)) (l (length/sbral S)))
            (let O ((w v*))
              (cond
                ((μkanren-var? w) (equal? α w))
                ((pair? w) (or (O (μkanren-state-find/SL (car w) S l)) (O (μkanren-state-find/SL (cdr w) S l))))
                ((μkanren-structure? w) (O (μkanren-structure->list w)))
                (else #f)))))
        (else #f)))
    
    (and
      (or unsafe (not (occur? v)))
      (let* ((S* (μkanren-update/sbral α v (μkanren-state-S s)))
             (s* (μkanren-state-with/S s (μkanren-state-vars-count s) S*))
             (assocs* (cons `(,α . ,v) assocs)))
        `(,s* ,assocs*))))

  ; the walk of α in the substitution S of length l: a variable beyond S (deferred) or unbound is
  ; answered as is. `(length/sbral S)` folds over the whole spine, so a walk over every node of a
  ; term computes it once and calls this, rather than `μkanren-state-find`, on each node.
  (define (μkanren-state-find/SL α S l)
    (let F ((β0 α) (β α))
      (cond
        ((and (μkanren-var? β) (<= l (μkanren-var-index>=0 β))) β)
        ((μkanren-var-working? β) (F β (sbral-ref S (- l 1 (μkanren-var-index>=0 β)))))
        ((μkanren-unbound? β) β0)
        (else β))))

  (define (μkanren-state-find α s)
    (let1 (S (μkanren-state-S s)) (μkanren-state-find/SL α S (length/sbral S))))

  ; `eqv?`, but reflexive on NaN: CHICKEN's `eqv?` compares flonums with `=`, so a NaN would never unify with an
  ; equal NaN. Only two flonum NaNs are equal (`nan?` is #t for a complex number with a NaN part too, so it alone
  ; would equate distinct complex numbers); a non-real number is compared part by part.
  (define (μkanren-number-eqv? u v)
    (or (eqv? u v)
        (and (flonum? u) (flonum? v) (nan? u) (nan? v))
        (and (not (real? u)) (not (real? v))
             (μkanren-number-eqv? (real-part u) (real-part v))
             (μkanren-number-eqv? (imag-part u) (imag-part v)))))

  ; the unification inside the module: its answer keeps the marks of `s`, which only =° may rely on,
  ; since it verifies the stores right after.
  (define (μkanren-unify u v s assocs)
    (let ((return (τ `(,s ,assocs)))
          (u* (μkanren-state-find u s))
          (v* (μkanren-state-find v s)))
      (cond
        ((eq? u* v*) (return))
        ((and (string? u*) (string? v*) (string=? u* v*)) (return))
        ((and (number? u*) (number? v*) (μkanren-number-eqv? u* v*)) (return))
        ((and (μkanren-var-working? u*) (μkanren-var-working? v*))
          (let ((i (μkanren-var-index>=0 u*)) (j (μkanren-var-index>=0 v*)))
            (cond
              ((= i j) (return))
              ((< i j) (μkanren-state-update u* v* s assocs))
              (else (μkanren-state-update v* u* s assocs)))))
        ((μkanren-var-working? u*) (μkanren-state-update u* v* s assocs))
        ((μkanren-var-working? v*) (μkanren-state-update v* u* s assocs))
        ((and (μkanren-structure? u*) (μkanren-structure? v*))
          (μkanren-unify (μkanren-structure->list u*) (μkanren-structure->list v*) s assocs))
        ; the hottest path: a unification answers `(s* assocs*)` or #f, no need for a checked match.
        ((and (pair? u*) (pair? v*))  (let1 (r (μkanren-unify (car u*) (car v*) s assocs))
                                        (and r (μkanren-unify (cdr u*) (cdr v*) (car r) (cadr r)))))
        (else #f))))

  ; the exported unification: a goal may hand its answer on without verifying the stores, so the
  ; answer drops the marks, as `μkanren-state-with` does for a new S, and its stores are verified as
  ; a whole at the next =° that changes it.
  (define (μkanren-state-unify u v s assocs)
    (match/first (μkanren-unify u v s assocs)
      ((,s* ,assocs*) (if (μkanren-state-equal? s s*) `(,s* ,assocs*) `(,(μkanren-state-with s* M: #f dirty: #t) ,assocs*)))
      (#f #f)))

  (define (μkanren-state-find/value v s)
    (let* ((S (μkanren-state-S s)) (l (length/sbral S)))
      (let A ((w v))
        (let1 (w* (μkanren-state-find/SL w S l))
          (cond
            ((pair? w*) (cons (A (car w*)) (A (cdr w*))))
            ((vector? w*) (list->vector (map A (μkanren-structure->list w*))))
            ((and (record-instance? w*) (not (μkanren-var? w*))) (apply make-record-instance (map A (μkanren-structure->list w*))))
            (else w*))))))

  (define (μkanren-state-find/repr v s)
    (let* ((S (μkanren-state-S s)) (l (length/sbral S)))
      (let A ((w v))
        (let1 (w* (μkanren-state-find/SL w S l))
          (cond
            ((μkanren-var? w*) (μkanren-var->symbol w*))
            ((symbol? w*) (list 'quote w*))
            ((null? w*) (list 'quote '()))
            ((pair? w*) (list 'cons (A (car w*)) (A (cdr w*))))
            ((vector? w*) (cons 'vector (map A (μkanren-structure->list w*))))
            ((record-instance? w*) (cons 'make-record-instance (map A (μkanren-structure->list w*))))
            (else w*))))))

  ; `l` is the length of the substitution of `r`, recomputed only when reifying a variable changes `r`.
  (define (μkanren-state-reify v s)
    (let R ((w v) (r s) (l (length/sbral (μkanren-state-S s))) (c -1) (vars '()) (assocs '()))
      (let1 (w* (μkanren-state-find/SL w (μkanren-state-S r) l))
        (cond
          ((μkanren-var-working? w*)  (let* ((v* (make-μkanren-var c))
                                             (r*+assocs* (μkanren-state-update w* v* r assocs))
                                             (r* (car r*+assocs*))
                                             (assocs* (cadr r*+assocs*))
                                             (c* (sub1 c))
                                             (vars* (cons v* vars)))
                                        (list r* c* vars* assocs*)))
          ((pair? w*) (match1/first ((,r* ,c* ,vars* ,assocs*) (R (car w*) r l c vars assocs))
                        (let1 (l* (if (eq? r* r) l (length/sbral (μkanren-state-S r*))))
                          (R (cdr w*) r* l* c* vars* assocs*))))
          ((μkanren-structure? w*) (R (μkanren-structure->list w*) r l c vars assocs))
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
    (let* ((fd (μkanren-drop-dot-D (μkanren-sort-D D s) s))
           (FA (λ1-match/first ((,tag . ,vars) `(assert (every ,(μkanren-tag-def tag) (list ,@(map μkanren-var->symbol vars)))))))
           (fa (map FA (sort/lex<=? (map μkanren-sort-part (μkanren-partition* A)))))
           (FT (λ1-match/first ((,tag ,u) `(assert (absent? (quote ,(μkanren-tag-name tag)) ,(μkanren-state-find/repr u s))))))
           (ft (map FT (μkanren-drop-dot-T (μkanren-sort-T T s)))))
        `(,@fd ,@fa ,@ft ,v)))

  ; sorts L as `sort/lex<=?` does, breaking its ties by the printed `(repr x)`, which spells out what `->string`
  ; leaves out: the slots of a record, and the datum of a tag.
  (define (μkanren-sort/repr repr L)
    (let ((decorated (map (λ (x) (list (->string x) (->string (repr x)) x)) L))
          (<=? (λ (a b) (or (string<? (car a) (car b)) (and (string=? (car a) (car b)) (string<=? (cadr a) (cadr b)))))))
      (map caddr (sort decorated <=?))))

  ; the answers do not depend on the order of the stores: the pairs of each prefix of D and the prefixes are
  ; sorted, before `μkanren-rem-subsumed` chooses which of two prefixes that subsume each other to keep as
  ; much as when they are printed, and so is T, by key and then by the datum of the tag.
  (define (μkanren-sort-D D s)
    (let1 (R (λ1-match/first ((,α . ,u) `(,α . ,(μkanren-state-find/repr u s)))))
      (μkanren-sort/repr (λ (d) (map R d)) (map (λ (d) (μkanren-sort/repr R d)) D))))

  (define (μkanren-sort-T T s)
    (μkanren-sort/repr (λ1-match/first ((,u . ,tag) `(,(μkanren-state-find/repr u s) . ,(μkanren-tag-name tag)))) T))

  (define (μkanren-subsumed-T? x tag T)
    ((exists (λ1-match/first ((,y . ,tag*) (and (equal? x y) (μkanren-tag-equal? tag tag*))))) T))

  ; a table keyed by the index of a variable, or by #f. srfi-69 salts every table built on one of its own hash
  ; functions with a draw of libc's rand(), so each such table would move the salt of every table built after
  ; it, and with it the order in which set° and groupby° fold theirs into answers: the tables of the stores
  ; hash with a procedure of their own, which draws nothing, and leave that order as it is without them.
  (define (μkanren-index-hash k bound) (if (fixnum? k) (modulo k bound) 0))
  (define (μkanren-index-table) (make-hash-table eqv? μkanren-index-hash))

  ; drops from a store every element subsumed by another one (before or after it). `subsumed?` must be an
  ; `any` of pure trials over its list, and given `key` and `keys` an element y is tried for x only when
  ; `(key y)` is one of `(keys x)`, which must hold whenever y subsumes x: see `μkanren-rem-subsumed/keyed`,
  ; spared for fewer than two elements, which have nothing to try but empty lists.
  (define ((μkanren-rem-subsumed/by subsumed? #!optional key keys) L0)
    (if (and key (pair? L0) (pair? (cdr L0)))
      (μkanren-rem-subsumed/keyed subsumed? key keys L0)
      (let loop ((L L0) (L+ '()))
        (match/first L
          (() L+)
          (((,x . ,L*) ⊣ (or (subsumed? x L*) (subsumed? x L+))) (loop L* L+))
          ((,x . ,L*) (loop L* (cons x L+)))))))

  ; the loop of `μkanren-rem-subsumed/by`, where the element x at position p is tried against those of L*
  ; (the positions after p) and of L+ (the positions before p that are `kept`) whose key is one of `(keys x)`
  ; only, found through a table from each key to its positions, in one call of `subsumed?` in place of two:
  ; being an `any`, it answers the same on them as on the whole of L* and L+, the others failing anyway.
  (define (μkanren-rem-subsumed/keyed subsumed? key keys L0)
    (let* ((V (list->vector L0))
           (kept (make-vector (vector-length V) #f))
           (H (μkanren-index-table))
           (others (λ (x p) (append-map (λ (k) (filter-map (λ (q) (and (not (fx= q p)) (or (fx> q p) (vector-ref kept q)) (vector-ref V q)))
                                                           (hash-table-ref/default H k '())))
                                        (keys x)))))
      (let K ((q (fx- (vector-length V) 1)))
        (unless (fx< q 0)
          (hash-table-update!/default H (key (vector-ref V q)) (λ (qs) (cons q qs)) '())
          (K (fx- q 1))))
      (let loop ((L L0) (p 0) (L+ '()))
        (match/first L
          (() L+)
          (((,x . ,L*) ⊣ (subsumed? x (others x p))) (loop L* (fx+ p 1) L+))
          ((,x . ,L*) (vector-set! kept p #t) (loop L* (fx+ p 1) (cons x L+)))))))

  (define μkanren-rem-subsumed-T (μkanren-rem-subsumed/by (λ (t T) (μkanren-subsumed-T? (lhs t) (rhs t) T))))

  (define (μkanren-anyvar? s)
    (define anyvar? (λ1-match/first
                      ((,a . ,d) (or (anyvar? a) (anyvar? d)))
                      ((,α ⊣ (μkanren-var-working? α)) (μkanren-var-working? (μkanren-state-find α s)))
                      ((,v ⊣ (μkanren-structure? v)) (anyvar? (μkanren-structure->list v)))
                      (else #f)))
    anyvar?)

  ; projects the absento store T, already walked in `s`, on the reified variables: a variable key that is still a
  ; working variable is unreachable from the answer, so its constraint is dropped; a compound key holding such a
  ; variable is broken into its parts instead, since the unreachable variable can always be chosen to keep the
  ; compound apart from the tag datum, while the constraints its parts put on the reachable variables must stay.
  ; A part with no variable at all is dropped: it could never be the datum, being part of a term that still could.
  (define (μkanren-project-T T s)
    (let1 (anyvar? (μkanren-anyvar? s))
      (define (P u tag)
        (cond
          ((μkanren-var? u) (if (anyvar? u) '() `((,u . ,tag))))
          ((not (anyvar? u)) (if (μkanren-hasvar? u) `((,u . ,tag)) '()))
          ((μkanren-compound? u) (append-map (λ (v) (P v tag)) (μkanren-compound->list u)))
          (else '())))
      (append-map (λ1-match/first ((,u . ,tag) (P u tag))) T)))

  ; whether the term `u` holds some variable, working or reified.
  (define (μkanren-hasvar? u)
    (cond
      ((μkanren-var? u) #t)
      ((pair? u) (or (μkanren-hasvar? (car u)) (μkanren-hasvar? (cdr u))))
      ((μkanren-structure? u) (any μkanren-hasvar? (μkanren-structure->list u)))
      (else #f)))

  ; the distinct indices of the variables the term `u` holds, found as `μkanren-hasvar?` finds them.
  (define (μkanren-var-indices u)
    (let I ((w u) (is '()))
      (cond
        ((μkanren-var? w) (let1 (i (μkanren-var-index w)) (if (memv i is) is (cons i is))))
        ((pair? w) (I (cdr w) (I (car w) is)))
        ((μkanren-structure? w) (fold I is (μkanren-structure->list w)))
        (else is))))

  ; whether the equations of some prefix of D already hold in the state built from the equations of `d`,
  ; so that `d` adds nothing; that state is built once, not once per prefix.
  (define (μkanren-subsumed? d D)
    (let* ((f (λ-match/non-overlapping
                (((,α . ,u) (,s* ,assocs*)) (μkanren-state-update α u s* assocs*))
                ((_ #f) #f)))
           (s+assocs (foldr f `(,μkanren-state-empty ()) d))
           (s (car s+assocs)))
      (any (λ (d*) (match/non-overlapping (μkanren-state-unify/assoc d* s '())
                     ((,s* _) (μkanren-state-equal? s s*))
                     (#f #f)))
           D)))

  ; `μkanren-subsumed?` finds that a prefix d* holds in the state s_d built from d only if every variable of d*
  ; occurs in d. Since s_d binds variables of d only, to terms of d, a variable γ of d* that d lacks is unbound
  ; in s_d and no binding reaches it; the left side of each pair of d* being a variable (I2), either γ is one
  ; of them, and the trial binds it, or it sits inside a term whose walk holds γ, unlike the walk of the other
  ; side, and the trial binds some variable or fails: either way it does not answer s_d itself. So d* needs
  ; trying for d only when its key variable occurs in d, whichever variable of d* the key is: it is the one
  ; occurring in the fewest prefixes of D (the first of `μkanren-var-indices` among equals), since a variable
  ; that many prefixes share, as the left side of their first pair in `((_1 . _k))` for each k or as x in
  ; `((_k . x))` when x comes after the variables it must differ from, would put them all in one bucket.
  ; A prefix without variables, which I2 rules out, would key on -1, which every prefix lists (a reified
  ; variable has index -1 too, but an index shared by two variables only adds trials).
  (define (μkanren-prefix-key/rarest D)
    (let1 (C (μkanren-index-table))
      (for-each (λ (d) (for-each (λ (i) (hash-table-update!/default C i add1 0)) (μkanren-var-indices d))) D)
      (let1 (count (λ (i) (hash-table-ref/default C i 0)))
        (λ (d) (fold (λ (i k) (if (or (fx= k -1) (fx< (count i) (count k))) i k)) -1 (μkanren-var-indices d))))))

  (define (μkanren-prefix-keys d) (cons -1 (μkanren-var-indices d)))

  ; the counts of the keys are spared for fewer than two prefixes, as the table of `μkanren-rem-subsumed/keyed`.
  (define (μkanren-rem-subsumed D)
    (if (and (pair? D) (pair? (cdr D)))
      ((μkanren-rem-subsumed/by μkanren-subsumed? (μkanren-prefix-key/rarest D) μkanren-prefix-keys) D)
      ((μkanren-rem-subsumed/by μkanren-subsumed?) D)))

  (define ((μkanren-project w) s)
    (let1 (w* (μkanren-state-find/value w s))
      (match1/first ((,s* ,c ,vars-reversed ,assocs) (μkanren-state-reify w* s))
        (let* ((R     (λ1-match/first ((,α . _) (μkanren-var-working? (μkanren-state-find α s*)))))
               (vars  (reverse vars-reversed))
               (vars* (map μkanren-var->symbol vars))
               (w**   (μkanren-state-find/repr w* s*))
               (D     (μkanren-verify-D (μkanren-state-D s) s))
               (A     (μkanren-state-A s*))
               (T     (μkanren-state-T s*))
               (D*    (μkanren-rem-subsumed (μkanren-sort-D (remove (μkanren-anyvar? s*) D) s*))) ; needs more revision the stuff with T.
               (A*    (remove R A))
               (D**   (μkanren-state-find/value (μkanren-subsume A* D*) s*))
               (A**   (μkanren-state-find/value A* s*))
               (T**   (μkanren-rem-subsumed-T (μkanren-project-T (μkanren-state-find/value T s*) s*)))
               (body  (μkanren-form w** D** A** T** s*)))
          `(λ ,vars* ,@body)))))

  ; constraints -------------------------------------------------------------------

  ; D extended with the prefix `((α . name))` for the absento tag `tag` of α, unless D holds it already. The
  ; search is spared when `scan?` is #f, which tells that no prefix of D has α on its left side.
  (define (μkanren-ext-D α tag D s #!optional (scan? #t))
    (let* ((name (μkanren-tag-name tag))
           (E (λ1-match/first
                (((,α* . ,u)) (and (equal? α (μkanren-state-find α* s)) (equal? u name)))
                (else #f)))
           (exists? (exists E)))
      (cond
        ((and scan? (exists? D)) D)
        (else (cons `((,α . ,name)) D)))))

  ; turns the entries of T keyed on α, a key of A, into disequalities: it answers `(D* . T*)`, where T* is
  ; T without them, in its order. `scan?` is for `μkanren-ext-D`, and holds once a prefix is added.
  (define (μkanren-update-D/T+ α T+ D T s #!optional (scan? #t))
    (match/first T
      (() `(,D . ,(reverse T+)))
      ((((,α* . ,tag) . ,T*) ⊣ (equal? α α*)) (let1 (D* (μkanren-ext-D α tag D s scan?))
                                                 (μkanren-update-D/T+ α T+ D* T* s (or scan? (not (eq? D* D))))))
      ((,t . ,T*) (let1 (T+* (cons t T+)) (μkanren-update-D/T+ α T+* D T* s scan?)))))

  ; turns the entries of T+ keyed on a key of A into disequalities, for each key α in `vars`, all keys of A.
  ; An α with no T mark keys no entry of T+ (I3, I4) and is skipped. An α with no D mark is on the left
  ; side of no prefix of D, and `μkanren-ext-D` needs not search for it. A conversion that adds a prefix
  ; to D marks it in `M` and leaves D dirty, see the state record. The answer has the A of `s`, unless `A:`
  ; gives the one just rebuilt.
  (define (μkanren-subsume-T vars T+ D T M s #!key (A μkanren-state-keep))
    (let U ((vars vars) (T+ T+) (D D) (M M) (dirty (μkanren-state-dirty s)))
      (match/first vars
        (() (μkanren-state-with s D: D A: A T: (append T+ T) M: M dirty: dirty))
        (((,α . ,vars*) ⊣ (not (μkanren-marked? M α μkanren-mark/T))) (U vars* T+ D M dirty))
        ((,α . ,vars*)  (match1/first ((,D* . ,T+*) (μkanren-update-D/T+ α '() D T+ s (μkanren-marked? M α μkanren-mark/D)))
                          (if (eq? D* D)
                              (U vars* T+* D M dirty)
                              (U vars* T+* D* (μkanren-mark-prefixes M D* D) #t)))))))

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

  ; a table from the index of each variable key of the store `L` (A or T) to the entries of `L` keyed on it.
  (define (μkanren-store-table L)
    (let1 (H (μkanren-index-table))
      (for-each (λ (e) (when (μkanren-var? (lhs e))
                         (hash-table-update!/default H (μkanren-var-index (lhs e)) (λ (es) (cons e es)) '())))
                L)
      H))

  ; D without the prefixes that A-or-T subsumes: the verification of a =° that is not quiet calls it on the
  ; whole D, `≠°` on its new prefix only, so an empty store (the usual case) costs nothing. Given the marks
  ; `M`, a pair of D whose right side is no working variable is tested only when the variable on its left
  ; side has the mark `bit` (I4), and against the entries of A-or-T keyed on that variable only, which a
  ; table holds from the second such test on (the first one, often the only one, scans A-or-T instead,
  ; for less than building the table): the others cannot match it under `equal?`.
  (define (μkanren-subsume A-or-T D #!optional M bit)
    (cond
      ((null? A-or-T) D)
      ((not M) (remove (exists (μkanren-subsumed-pr? A-or-T)) D))
      (else (let* ((H (delay (μkanren-store-table A-or-T)))
                   (scanned #f)
                   (entries (λ (α) (cond
                                     (scanned (hash-table-ref/default (force H) (μkanren-var-index α) '()))
                                     (else (set! scanned #t) A-or-T))))
                   (subsumed? (λ (p) (let1 (α (lhs p))
                                       (cond
                                         ((μkanren-var-working? (rhs p)) #f)
                                         ((not (μkanren-var? α)) ((μkanren-subsumed-pr? A-or-T) p))
                                         ((and (μkanren-var-working? α) (not (μkanren-marked? M α bit))) #f)
                                         (else ((μkanren-subsumed-pr? (entries α)) p)))))))
              (remove (exists subsumed?) D)))))

  ; the keys of A, just rebuilt by `μkanren-verify-A`, are distinct. An empty T, just rebuilt by
  ; `μkanren-verify-T`, subsumes nothing and has nothing to convert.
  (define (μkanren-verify-T/post D A s)
    (let1 (M (μkanren-state-M s))
      (μ T (if (null? T)
               (μkanren-state-with s D: D A: A T: '() M: M dirty: (μkanren-state-dirty s))
               (μkanren-subsume-T (map lhs A) T (μkanren-subsume T D M μkanren-mark/T) '() M s A: A)))))

  ; the absento store T holds `(α . tag)` pairs where α is a variable or, for a compound tag datum, a compound
  ; term that may still become that datum: such a term is kept whole, since walking into its parts would lose
  ; sight of it, and it is walked into only once it can no longer unify with the datum.
  (define (μkanren-compound? v) (or (pair? v) (μkanren-structure? v)))

  ; the parts of a compound: a structure yields its slots one by one (its slot list is no term by itself, whose
  ; tails could be mistaken for the datum); a record's slots start with its type name, so a tag equal to that name
  ; counts as present, as in absent?.
  (define (μkanren-compound->list v) (if (pair? v) (list (car v) (cdr v)) (μkanren-structure->list v)))

  ; whether the compound `u` already is the datum of the absento tag `tag` (`'present`), could become it by binding
  ; some of its variables (`'maybe`), or never can (`#f`, always so for an atomic datum, found only at the leaves).
  (define (μkanren-tag-unify u tag s)
    (let1 (datum (μkanren-tag-name tag))
      (and (μkanren-compound? datum)
           (match/first (μkanren-unify u datum s '())
             (#f #f)
             ((,s* _) (if (μkanren-state-equal? s s*) 'present 'maybe))))))

  (define (μkanren-ext-T+ α tag T s)
    (match/first T
      (() `((,α . ,tag)))
      ((((,α* . ,tag*) . _) ⊣ (and (equal? (μkanren-state-find α* s) α) (μkanren-tag-equal? tag tag*))) '())
      ((_ . ,T*) (μkanren-ext-T+ α tag T* s))))

  ; a right fold over a store, from `init` (the empty store by default), that stops at the first `#f`
  ; returned by `step` (the stores D, A and T are always proper lists, so no `else` clause is needed).
  (define (μkanren-foldr/verify step L #!optional (init '()))
    (let F ((L L))
      (match/first L
        (() init)
        ((,x . ,L*) (cond ((F L*) => (μ acc (step x acc))) (else #f))))))

  ; `ext`, which extends the store T₀ rebuilt so far as `μkanren-ext-T+` does, is given by `μkanren-verify-T`.
  (define (μkanren-verify-T+ α tag s #!optional (ext μkanren-ext-T+))
    (define ((watch u) T₀) (append (ext u tag T₀ s) T₀))
    (define ((verify* us) T₀) (fold (λ (u T) (and T ((μkanren-verify-T+ u tag s ext) T))) T₀ us))
    (match/first (μkanren-state-find α s)
      ((,α* ⊣ (μkanren-var-working? α*)) (watch α*))
      ((,u ⊣ (μkanren-compound? u))  (case (μkanren-tag-unify u tag s)
                                        ((present) (K #f))
                                        ((maybe) (watch u))
                                        (else (verify* (μkanren-compound->list u)))))
      (,u (μ T₀ (and (μkanren-tag-pred? tag u) T₀)))))

  ; T is rebuilt from the right, each key replaced by its walk in `s`. The keys of the store T₀ rebuilt so far
  ; are walks themselves (a compound walks to itself), so the search of `μkanren-ext-T+` for an entry of
  ; T₀ with the key u and the same tag needs only look, with the same `equal?` and `μkanren-tag-equal?`,
  ; at the entries in `keys`, a table from the index of a variable key, or #f for every compound one, to
  ; the entries of T₀ under it, filled as T₀ grows.
  (define (μkanren-verify-T T s)
    (define keys (and (pair? T) (μkanren-index-table)))
    (define (ext u tag T₀ s)
      (let* ((k (and (μkanren-var? u) (μkanren-var-index u)))
             (us (hash-table-ref/default keys k '())))
        (cond
          ((any (λ1-match/first ((,u* . ,tag*) (and (equal? u* u) (μkanren-tag-equal? tag tag*)))) us) '())
          (else (hash-table-set! keys k (cons `(,u . ,tag) us)) `((,u . ,tag))))))
    (μkanren-foldr/verify (λ (t T₀) ((μkanren-verify-T+ (lhs t) (rhs t) s ext) T₀)) T))

  (define (μkanren-verify-A/post D T s)
    (λ (A)
      (let1 (D* (μkanren-subsume A D (μkanren-state-M s) μkanren-mark/A))
        (cond 
          ((μkanren-verify-T T s) => (μkanren-verify-T/post D* A s))
          (else #f)))))

  (define (μkanren-ext-A α tag A0 s)
    (let L ((A A0))
      (match/first A
        (() `((,α . ,tag)))
        ((((,α* . ,tag*) . _) ⊣ (equal? (μkanren-state-find α* s) α)) (if (μkanren-tag-equal? tag tag*) '() #f))
        ((_ . ,A*) (L A*)))))

  ; A is rebuilt from the right, each key replaced by its walk in `s`, as `μkanren-ext-A` would extend it. The
  ; keys of the store A0 built so far are themselves walks in `s`, so the search of `μkanren-ext-A` for a key
  ; whose walk is α* is a lookup of the index of α* in `keys`, a table from the index of each key of A0 to its
  ; tag: the same tag leaves A0 as it is, so the right-most entry of a key survives, a different one fails.
  (define (μkanren-verify-A A s)
    (define keys (and (pair? A) (μkanren-index-table)))
    (define (step a A0)
      (let ((α* (μkanren-state-find (lhs a) s)) (tag (rhs a)))
        (cond
          ((not (μkanren-var-working? α*)) (and (μkanren-tag-pred? tag α*) A0))
          ((hash-table-ref/default keys (μkanren-var-index α*) #f) => (λ (tag*) (and (μkanren-tag-equal? tag tag*) A0)))
          (else (hash-table-set! keys (μkanren-var-index α*) tag) (cons `(,α* . ,tag) A0)))))
    (μkanren-foldr/verify step A))

  (define (μkanren-verify-D/post D A T s)
    (cond
      ((μkanren-verify-A A s) => (μkanren-verify-A/post D T s))
      (else #f)))

  (define (μkanren-state-unify/assoc associations s assocs)
    (fold (λ (p r) (and r (μkanren-unify (lhs p) (rhs p) (car r) (cadr r)))) `(,s ,assocs) associations))

  (define (μkanren-verify-D+ d D s)
    (cond
      ((μkanren-state-unify/assoc d s '()) => (λ1-match/first ((,s* ,d*) (if (μkanren-state-equal? s s*) #f (cons d* D)))))
      (else D)))

  (define (μkanren-verify-D D s)
    (μkanren-foldr/verify (λ (d D**) (μkanren-verify-D+ d D** s)) D))

  ; `assocs` are the bindings the unification that led to `s` just made. A prefix of D is keyed on
  ; unbound variables, each paired with a term that is no variable or an unbound one: the trial
  ; unification of a prefix none of whose variables was just bound would still bind its first key,
  ; so it cannot have become violated, and only the prefixes that mention one of them are verified
  ; again. A prefix kept as it is may have become satisfied, by the occurs check deep in its terms:
  ; `μkanren-project` drops those with a last `μkanren-verify-D`. It answers `(D* . rebuilt)`, where
  ; `rebuilt` are the prefixes of D* that the verification made anew, to be marked.
  (define (μkanren-verify-D/touched D s assocs)
    (let* ((bound (map (o μkanren-var-index lhs) assocs))
           (bound? (λ (u) (and (μkanren-var? u) (memv (μkanren-var-index u) bound))))
           (touched? (λ (d) (any (λ (p) (or (bound? (lhs p)) (bound? (rhs p)))) d)))
           (step (λ (d r) (cond
                            ((not (touched? d)) (cons (cons d (car r)) (cdr r)))
                            ((μkanren-verify-D+ d (car r) s) => (μ D** (if (eq? D** (car r)) r (cons D** (cons (car D**) (cdr r))))))
                            (else #f)))))
      (if (any touched? D) (μkanren-foldr/verify step D (cons '() '())) (cons D '()))))

  ; the marks of `s*`, which a =° on `s` reached through `assocs`, for the verification of its stores:
  ; the prefixes `rebuilt` by `μkanren-verify-D/touched`, the walk of a key of A just bound, which
  ; `μkanren-verify-A` makes a key in its place, and the variables reachable from a T-marked variable
  ; just bound, which `μkanren-verify-T` may make keys of T or find inside a compound key (I6).
  (define (μkanren-remark s s* assocs rebuilt)
    (define M0 (μkanren-state-M s))
    (define (R p M)
      (let* ((α (lhs p))
             (m (μkanren-marks M0 α))
             (A? (μkanren-mark? m μkanren-mark/A))
             (T? (μkanren-mark? m μkanren-mark/T)))
        (if (or A? T?)
            (let* ((u (μkanren-state-find α s*)) ; walked only for a variable that A or T mentions
                   (M (if (and A? (μkanren-var? u)) (μkanren-mark M u μkanren-mark/A) M)))
              (if T? (μkanren-mark-T M u s*) M))
            M)))
    (and M0 (fold R (fold (λ (d M) (μkanren-mark-prefix M d)) M0 rebuilt) assocs)))

  ; whether the =° that took `s` to a new state through `assocs` leaves the stores as they are: D is clean
  ; and no store mentions a variable just bound. By I1–I6, the verification `μkanren-verify-D/post` would
  ; then rebuild D, A and T as they are, so the new state keeps them.
  (define (μkanren-quiet? s assocs)
    (let1 (M (μkanren-state-M s))
      (and M (not (μkanren-state-dirty s)) (every (λ (p) (fx= 0 (μkanren-marks M (lhs p)))) assocs))))

  ; α is the key just added to A, one more key than `s` has, whose absento tags become disequalities
  ; (`μkanren-update-D/T+`). An α with no T mark is no key of T (I3, I4), and then D and T stay as they are.
  (define (μkanren-subsume-A tag α D A s)
    (match1/first ((,D* . ,T*) (let ((M (μkanren-state-M s)) (T (μkanren-state-T s)))
                                 (if (μkanren-marked? M α μkanren-mark/T)
                                     (μkanren-update-D/T+ α '() D T s (μkanren-marked? M α μkanren-mark/D))
                                     `(,D . ,T))))
      (let1 (M (μkanren-mark (μkanren-state-M s) α μkanren-mark/A))
        (μkanren-state-with s D: D* A: A T: T* tags: (cons tag (μkanren-state-tags s))
                            M: (μkanren-mark-prefixes M D* D) dirty: (or (μkanren-state-dirty s) (not (eq? D* D)))))))

  ; the unbound α is a key of A only if it has the A mark, and the left side of a pair of D only if it has
  ; the D mark (I1, I2, I4): without them, neither the search of `μkanren-ext-A` nor `μkanren-subsume` can
  ; find it, and both are spared.
  (define (μkanren-make-tag-A+ α tag s)
    (let ((A (μkanren-state-A s)) (M (μkanren-state-M s)))
      (match/first (if (μkanren-marked? M α μkanren-mark/A) (μkanren-ext-A α tag A s) `((,α . ,tag)))
        (#f #f)
        (() s)
        (,A+  (let* ((D   (μkanren-state-D s))
                     (D*  (if (μkanren-marked? M α μkanren-mark/D) (μkanren-subsume A+ D) D))
                     (A*  (append A+ A)))
                (μkanren-subsume-A tag α D* A* s))))))

  (define (((μkanren-make-tag-A tag) u) s)
    (match/first (μkanren-state-find u s)
      ((,α ⊣ (μkanren-var-working? α))  (cond
                                          ((μkanren-make-tag-A+ α tag s) => ✓°)
                                          (else (✗° s))))
      ((,u* ⊣ (μkanren-tag-pred? tag u*)) (✓° s))
      (else (✗° s))))

  ; the stores of `s` are read only by `watch`, so a ground `u` costs nothing more than its walk. A variable
  ; α is a key of T, and the left side of a pair of D, only if it has the T mark, and the D mark (I2–I4):
  ; without them, the search of `μkanren-ext-T+` and `μkanren-subsume` are spared. The new entry T+ is a
  ; single one, so the only key of A that `μkanren-subsume-T` needs is the first one equal to α, which
  ; converts it: there is none for a compound α, nor for a variable with no A mark (I1, I4), but in a
  ; state built by hand, which has no marks.
  (define (μkanren-absento+ u tag s)
      (define (watch α)
        (let* ((D (μkanren-state-D s)) (T (μkanren-state-T s)) (M0 (μkanren-state-M s))
               (var? (μkanren-var? α))
               (T+ (if (and var? (not (μkanren-marked? M0 α μkanren-mark/T))) `((,α . ,tag)) (μkanren-ext-T+ α tag T s))))
          (if (null? T+) s (let* ((D* (if (and var? (not (μkanren-marked? M0 α μkanren-mark/D))) D (μkanren-subsume T+ D)))
                                  (vars (cond
                                          ((and M0 (not (and var? (μkanren-marked? M0 α μkanren-mark/A)))) '())
                                          ((assoc α (μkanren-state-A s)) => (λ1-match/first ((,β . _) `(,β))))
                                          (else '())))
                                  (datum (μkanren-tag-name tag))
                                  (M (μkanren-mark-T M0 α s))
                                  ; the datum of a compound tag is walked by `μkanren-tag-unify` too.
                                  (M (if (μkanren-compound? datum) (μkanren-mark-T M datum s) M)))
                             (μkanren-subsume-T vars T+ D* T M s)))))
      (define (absento* us s*)
        (match/first us
          (() s*)
          ((,u . ,us*) (let1 (s** (μkanren-absento+ u tag s*))
                         (and s** (absento* us* s**))))))
      (match/first (μkanren-state-find u s)
        ((,α ⊣ (μkanren-var-working? α)) (watch α))
        ((,u* ⊣ (μkanren-compound? u*))  (case (μkanren-tag-unify u* tag s)
                                            ((present) #f)
                                            ((maybe) (watch u*))
                                            (else (absento* (μkanren-compound->list u*) s))))
        (,u* (and (μkanren-tag-pred? tag u*) s))))
  
  ; goals --------------------------------------------------------------------------

  ; a goal maps a state to a stream of states: `✓°` succeeds once with the given state, `✗°` fails.
  ; the `ª` suffix marks the applicative form of a goal constructor, `°` its syntactic form or a relation.
  (define ✓° list)
  (define ✗° (K '()))

  (define ((freshª f) s) ; ª means "applicative", so `freshª` is a *function* that consumes a function and returns a goal.
    (let* ((vc  (μkanren-state-vars-count s))
           (α   (make-μkanren-var vc))
           (g   (f α))
           (s*  (μkanren-state-with/S s (add1 vc) (μkanren-state-S s))))
      (δ (g s*))))

  (define ((=° u v) s)
    (define post°
      (λ1-match/first
        ((,s* ,assocs)  (cond
                          ((μkanren-state-equal? s s*) (✓° s*))
                          ((μkanren-quiet? s assocs) (✓° s*))
                          ((μkanren-verify-D/touched (μkanren-state-D s) s* assocs)
                           => (λ1-match/first
                                ((,D* . ,rebuilt)
                                 (let1 (s** (μkanren-state-with s* M: (μkanren-remark s s* assocs rebuilt) dirty: #f))
                                   (cond
                                     ((μkanren-verify-D/post D* (μkanren-state-A s) (μkanren-state-T s) s**) => ✓°)
                                     (else (✗° s*)))))))
                          (else (✗° s*))))))
    (cond
      ((μkanren-unify u v s '()) => post°)
      (else (✗° s))))

  (define ((≠° u v) s)
    (define post°
      (λ1-match/first
        ((,s* ,d) (cond
                    ((μkanren-state-equal? s s*) (✗° s*))
                    (else (let* ((D   (μkanren-state-D s))
                                 (M   (μkanren-state-M s))
                                 (D*  (list d))
                                 (D*  (μkanren-subsume (μkanren-state-A s) D* M μkanren-mark/A))
                                 (D*  (μkanren-subsume (μkanren-state-T s) D* M μkanren-mark/T))
                                 (D+  (append D* D)))
                            (✓° (μkanren-state-with s D: D+ M: (μkanren-mark-prefixes M D+ D)))))))))
    (cond
      ((μkanren-unify u v s '()) => post°)
      (else (✓° s))))

  (define ((orª f g) s) (append§/interleaved/2 (δ (f s)) (δ (g s))))
  (define ((andª f g) s) (append-map§ g (δ (f s))))

  ; `(fresh° (α ...) g ...)` introduces fresh variables; `(fresh° α ← (β ...) g ...)` moreover binds α to the list `(β ...)`.
  ; the `←` keeps the latter apart from the typo `(fresh° x (g x) ...)`, which is an expansion error instead of a goal
  ; that silently takes `(g x)` for the names of fresh variables.
  (define-syntax fresh°
    (syntax-rules (←)
      ((fresh° () body ...) (and° body ...))
      ((fresh° (α β ...) body ...) (freshª (λ (α) (fresh° (β ...) body ...))))
      ((fresh° α ← (β ...) body ...) (fresh° (α) (fresh° (β ...) (=° α (list β ...)) body ...)))))

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
    ; an atomic tag is found at the leaves: compare numbers as unification does, so a NaN tag is found in a NaN.
    (let* ((pred? (λ (v) (not (if (and (number? tag) (number? v)) (μkanren-number-eqv? tag v) (equal? tag v)))))
           (tag* (make-μkanren-tag tag 'no-def pred?)))
      (cond
        ((μkanren-absento+ u tag* s) => ✓°)
        (else (✗° s)))))

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

  ; a relation is inverse-η-delayed: calling it builds a goal that builds its body only when applied to
  ; a state, and then suspends, so a recursive relation without `fresh°` does not loop at construction.
  (define-syntax-rule (define-relation (name arg ...) g ...) (define (name arg ...) (λ (s) (δ ((and° g ...) s)))))

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