

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
  ; by variable with the most recent variable first; `tags` the type tags asserted so far.
  ;
  ; The constraints are kept by variable, as faster-miniKanren keeps them: `C` is an sbral indexed like S
  ; (see `sbral-ref/back`, a missing slot reads `μkanren-c-empty`) of the records of the variables, each with
  ; - `A`, the type tag of the variable, if any;
  ; - `T`, the absento tags keyed on the variable;
  ; - `D`, the ids of the prefixes of the disequality store with the variable on either side of a pair;
  ; - `t`, whether the variable is reachable from a compound key of T, or from the compound datum of its tag.
  ; `P` is an sbral of the prefixes by id, each a list of `(α . u)` pairs or #f once dropped, and `Tc` the list
  ; of the `(u . tag)` entries of T keyed on a compound u, see `μkanren-tag-unify`. The disequality store D, the
  ; type store A and the absento store T, which `μkanren-state-D`, `μkanren-state-A` and `μkanren-state-T`
  ; answer as lists of prefixes, of `(α . tag)` and of `(u . tag)` pairs, are read from them.
  ;
  ; The records keep these invariants:
  ; I1. a variable with a record is unbound: =° hands the constraints of a variable it binds on, see `μkanren-repost`;
  ; I2. the left sides of the pairs of a prefix are unbound, and their right sides are non-variables or unbound;
  ; I3. a variable with a type tag has no absento tags, which become disequalities, see `μkanren-convert-T`;
  ; I4. no tag of a variable subsumes a prefix with the variable on the left side of a pair;
  ; I5. the answer of `μkanren-tag-unify` on a compound key of T changes only when a variable with `t` is bound.
  ; An id in `D` may be of a dropped prefix, and `t` may be stale: both cost time, not answers.
  ;
  ; `raw` is #f, or the list `(D A T)` of stores given as lists, by `make-μkanren-state` or `μkanren-state-with`,
  ; or left unverified by the exported `μkanren-state-unify`: the next goal verifies and indexes them first.
  (define-record-type μkanren-state
    (make-μkanren-state/C vars-count S C P Tc tags raw)
    μkanren-state?
    (vars-count μkanren-state-vars-count) (S μkanren-state-S) (C μkanren-state-C) (P μkanren-state-P)
    (Tc μkanren-state-Tc) (tags μkanren-state-tags) (raw μkanren-state-raw))

  (define (make-μkanren-state vars-count S D A T tags)
    (make-μkanren-state/C vars-count S empty/sbral empty/sbral '() tags (list D A T)))

  (define μkanren-state-empty (make-μkanren-state/C 0 empty/sbral empty/sbral empty/sbral '() '() #f))

  ; a copy of `s` with the given `vars-count` and `S`, which keeps the stores: only `freshª` and the
  ; unification inside =° call it, and =° hands on the constraints of the variables it binds.
  (define (μkanren-state-with/S s vc S)
    (make-μkanren-state/C vc S (μkanren-state-C s) (μkanren-state-P s) (μkanren-state-Tc s) (μkanren-state-tags s)
                          (μkanren-state-raw s)))

  (define (μkanren-state-with/stores s C P Tc)
    (make-μkanren-state/C (μkanren-state-vars-count s) (μkanren-state-S s) C P Tc (μkanren-state-tags s) #f))

  (define (μkanren-state-equal? s s*) (eq? s s*))

  (define μkanren-state-keep (list 'keep)) ; the default of the keys of `μkanren-state-with`, unique under `eq?`.

  ; a copy of `s` where the given fields are replaced. Given a new substitution or a store, the copy holds its
  ; stores as lists (`raw`), since they may no longer hold: the next goal verifies them as a whole.
  (define (μkanren-state-with s #!key
                              (vars-count (μkanren-state-vars-count s))
                              (S μkanren-state-keep)
                              (D μkanren-state-keep)
                              (A μkanren-state-keep)
                              (T μkanren-state-keep)
                              (tags (μkanren-state-tags s)))
    (let* ((kept? (λ (v) (eq? v μkanren-state-keep)))
           (field (λ (v get) (if (kept? v) (get s) v))))
      (if (and (kept? S) (kept? D) (kept? A) (kept? T))
          (make-μkanren-state/C vars-count (μkanren-state-S s) (μkanren-state-C s) (μkanren-state-P s) (μkanren-state-Tc s)
                                tags (μkanren-state-raw s))
          (make-μkanren-state/C vars-count (field S μkanren-state-S) empty/sbral empty/sbral '() tags
                                (list (field D μkanren-state-D) (field A μkanren-state-A) (field T μkanren-state-T))))))

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

  ; the records of the variables, see the comment of the state record.
  (define-record-type μkanren-c
    (make-μkanren-c A T D t)
    μkanren-c?
    (A μkanren-c-A) (T μkanren-c-T) (D μkanren-c-D) (t μkanren-c-t))

  (define μkanren-c-empty (make-μkanren-c #f '() '() #f))

  (define (μkanren-c-empty? c)
    (or (eq? c μkanren-c-empty)
        (and (not (μkanren-c-A c)) (null? (μkanren-c-T c)) (null? (μkanren-c-D c)) (not (μkanren-c-t c)))))

  (define (μkanren-c-with c #!key (A (μkanren-c-A c)) (T (μkanren-c-T c)) (D (μkanren-c-D c)) (t (μkanren-c-t c)))
    (make-μkanren-c A T D t))

  ; the record of the working variable α in `s`, and `s` with the record `c` in its place.
  (define (μkanren-c-ref s α) (sbral-ref/back (μkanren-state-C s) (μkanren-var-index α) μkanren-c-empty))
  (define (μkanren-c-set s α c)
    (μkanren-state-with/stores s (update/sbral/back (μkanren-var-index α) c μkanren-c-empty (μkanren-state-C s))
                               (μkanren-state-P s) (μkanren-state-Tc s)))

  ; folds `f` over the variables with a record and their records.
  (define (μkanren-fold-C f init s)
    (let* ((C (μkanren-state-C s)) (l (length/sbral C)))
      (foldr/sbral (λ (i c acc) (if (μkanren-c-empty? c) acc (f (make-μkanren-var (- l 1 i)) c acc))) init C)))

  ; the stores as lists, see the comment of the state record: D with its newest prefix first.
  (define (μkanren-state-D s)
    (match/first (μkanren-state-raw s)
      ((,D _ _) D)
      (#f (foldr/sbral (λ (i d D) (if d (cons d D) D)) '() (μkanren-state-P s)))))

  (define (μkanren-state-A s)
    (match/first (μkanren-state-raw s)
      ((_ ,A _) A)
      (#f (μkanren-fold-C (λ (α c A) (if (μkanren-c-A c) (cons `(,α . ,(μkanren-c-A c)) A) A)) '() s))))

  (define (μkanren-state-T s)
    (match/first (μkanren-state-raw s)
      ((_ _ ,T) T)
      (#f (μkanren-fold-C (λ (α c T) (append (map (λ (tag) `(,α . ,tag)) (μkanren-c-T c)) T)) (μkanren-state-Tc s) s))))

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

  ; the unification inside the module: its answer keeps the stores of `s`, which only =° may rely on,
  ; since it hands on the constraints of the variables it binds.
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

  ; the exported unification: a goal may hand its answer on without the constraints of the variables it
  ; binds, so the answer holds its stores as lists, which the next goal verifies as a whole.
  (define (μkanren-state-unify u v s assocs)
    (match/first (μkanren-unify u v s assocs)
      ((,s* ,assocs*) (if (μkanren-state-equal? s s*) `(,s* ,assocs*) `(,(μkanren-state-with s* S: (μkanren-state-S s*)) ,assocs*)))
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
  ; leaves out: the slots of a record, and the datum of a tag; and those ties by the written `(repr x)`, since
  ; `->string` displays 1 and "1" alike, and so 'a, "a" and #\a.
  (define (μkanren-sort/repr repr L)
    (let* ((decorated (map (λ (x) (let1 (r (repr x)) (list (->string x) (->string r) r x))) L))
           (written (λ (a) (->string/write (caddr a))))
           (<=? (λ (a b) (or (string<? (car a) (car b))
                             (and (string=? (car a) (car b))
                                  (or (string<? (cadr a) (cadr b))
                                      (and (string=? (cadr a) (cadr b)) (string<=? (written a) (written b)))))))))
      (map cadddr (sort decorated <=?))))

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

  ; whether a term holds a working variable once walked in `s` all the way down, one that the reification of the
  ; answer left unreachable: a variable bound to a compound is walked into, since the right side of a prefix may
  ; hold variables bound after the prefix was posted (I2 is about its top only).
  (define (μkanren-anyvar? s)
    (define anyvar? (λ1-match/first
                      ((,a . ,d) (or (anyvar? a) (anyvar? d)))
                      ((,α ⊣ (μkanren-var-working? α)) (let1 (α* (μkanren-state-find α s))
                                                         (if (μkanren-var? α*) (μkanren-var-working? α*) (anyvar? α*))))
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

  ; D without the prefixes that A-or-T subsumes, for `μkanren-project`.
  (define (μkanren-subsume A-or-T D)
    (if (null? A-or-T) D (remove (exists (μkanren-subsumed-pr? A-or-T)) D)))

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

  ; `ext` extends the store T₀ rebuilt so far, as `μkanren-ext-T+` does by default.
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

  (define (μkanren-state-unify/assoc associations s assocs)
    (fold (λ (p r) (and r (μkanren-unify (lhs p) (rhs p) (car r) (cadr r)))) `(,s ,assocs) associations))

  (define (μkanren-verify-D+ d D s)
    (cond
      ((μkanren-state-unify/assoc d s '()) => (λ1-match/first ((,s* ,d*) (if (μkanren-state-equal? s s*) #f (cons d* D)))))
      (else D)))

  (define (μkanren-verify-D D s)
    (μkanren-foldr/verify (λ (d D**) (μkanren-verify-D+ d D** s)) D))

  ; a left fold of `f` over L from the state `s`, which stops at the first #f.
  (define (μkanren-fold/and f s L)
    (let F ((L L) (s s))
      (if (or (not s) (null? L)) s (F (cdr L) (f (car L) s)))))

  ; the distinct indices of the variables a prefix is listed under: the left sides of its pairs, and the right
  ; sides that are variables.
  (define (μkanren-prefix-indices d)
    (let1 (add (λ (α js) (let1 (j (μkanren-var-index α)) (if (memv j js) js (cons j js)))))
      (fold (λ (p js) (let1 (js (add (lhs p) js)) (if (μkanren-var-working? (rhs p)) (add (rhs p) js) js))) '() d)))

  ; whether the tag `tag` of α rules the pair `(α . u)` out, as `μkanren-subsumed-pr?` finds it.
  (define (μkanren-tag-denies? tag u) (not (or (μkanren-var-working? u) (μkanren-tag-pred? tag u))))

  ; whether the tags of the variable on the left side of some pair of the prefix d rule that pair out (I4).
  (define (μkanren-prefix-subsumed? d s)
    (any (λ (p) (let1 (c (μkanren-c-ref s (lhs p)))
                  (or (and (μkanren-c-A c) (μkanren-tag-denies? (μkanren-c-A c) (rhs p)))
                      (any (λ (tag) (μkanren-tag-denies? tag (rhs p))) (μkanren-c-T c)))))
         d))

  (define (μkanren-prefix-ref s i) (sbral-ref/back (μkanren-state-P s) i #f))

  (define (μkanren-drop-prefix s i)
    (μkanren-state-with/stores s (μkanren-state-C s) (update/sbral/back i #f #f (μkanren-state-P s)) (μkanren-state-Tc s)))

  ; `s` with the new prefix d under the next id, which the records of its variables list, unless a tag subsumes it.
  (define (μkanren-add-prefix d s)
    (if (μkanren-prefix-subsumed? d s)
        s
        (let* ((P (μkanren-state-P s))
               (i (length/sbral P))
               (L (λ (j C) (let1 (c (sbral-ref/back C j μkanren-c-empty))
                             (update/sbral/back j (make-μkanren-c (μkanren-c-A c) (μkanren-c-T c) (cons i (μkanren-c-D c)) (μkanren-c-t c))
                                                μkanren-c-empty C)))))
          (μkanren-state-with/stores s (fold L (μkanren-state-C s) (μkanren-prefix-indices d)) (cons/sbral d P) (μkanren-state-Tc s)))))

  ; `(ids . s*)`: the ids of the prefixes listing α that are not dropped, and `s*`, where the record of α
  ; lists them only.
  (define (μkanren-prefixes s α)
    (let* ((c (μkanren-c-ref s α))
           (ids (μkanren-c-D c))
           (live (filter (λ (i) (μkanren-prefix-ref s i)) ids)))
      (cons live (if (fx= (length live) (length ids)) s (μkanren-c-set s α (μkanren-c-with c D: live))))))

  ; drops the prefixes listing α that the new tag `tag` of α subsumes (I4).
  (define (μkanren-subsume-prefixes α tag s)
    (match1/first ((,ids . ,s) (μkanren-prefixes s α))
      (let1 (denied? (λ1-match/first ((,β . ,u) (and (equal? β α) (μkanren-tag-denies? tag u)))))
        (fold (λ (i s) (if (any denied? (μkanren-prefix-ref s i)) (μkanren-drop-prefix s i) s)) s ids))))

  ; the prefix `((α . datum))` for the absento tag `tag` of α, which has a type tag (I3), unless it is there.
  (define (μkanren-ext-D α tag s)
    (let1 (name (μkanren-tag-name tag))
      (match1/first ((,ids . ,s) (μkanren-prefixes s α))
        (let1 (same? (λ (i) (match/first (μkanren-prefix-ref s i)
                              (((,β . ,u)) (and (equal? β α) (equal? u name)))
                              (else #f))))
          (if (any same? ids) s (μkanren-add-prefix `((,α . ,name)) s))))))

  ; turns the absento tags of α, which has just taken a type tag, into disequalities (I3).
  (define (μkanren-convert-T α s)
    (let1 (c (μkanren-c-ref s α))
      (fold (λ (tag s) (μkanren-ext-D α tag s)) (μkanren-c-set s α (μkanren-c-with c T: '())) (μkanren-c-T c))))

  ; gives the working variable α the type tag `tag`: a different one fails, the same one leaves `s` as it is.
  (define (μkanren-post-A α tag s)
    (let1 (c (μkanren-c-ref s α))
      (cond
        ((μkanren-c-A c) => (λ (tag*) (and (μkanren-tag-equal? tag tag*) s)))
        (else (let1 (s (μkanren-subsume-prefixes α tag (μkanren-c-set s α (μkanren-c-with c A: tag))))
                (if (null? (μkanren-c-T c)) s (μkanren-convert-T α s)))))))

  ; the type tag `tag` on the term u: its walk takes it if a working variable, and must satisfy it otherwise.
  (define (μkanren-post-type u tag s)
    (let1 (u* (μkanren-state-find u s))
      (cond
        ((μkanren-var-working? u*) (μkanren-post-A u* tag s))
        ((μkanren-tag-pred? tag u*) s)
        (else #f))))

  ; marks with `t` every unbound variable reachable from the term u in `s` (I5).
  (define (μkanren-mark-t u s)
    (let* ((S (μkanren-state-S s)) (l (length/sbral S)))
      (let R ((w u) (s s))
        (let1 (w* (μkanren-state-find/SL w S l))
          (cond
            ((μkanren-var-working? w*) (let1 (c (μkanren-c-ref s w*)) (if (μkanren-c-t c) s (μkanren-c-set s w* (μkanren-c-with c t: #t)))))
            ((pair? w*) (R (cdr w*) (R (car w*) s)))
            ((μkanren-structure? w*) (fold R s (μkanren-structure->list w*)))
            (else s))))))

  ; the absento tag `tag` on the term u, as `μkanren-verify-T+` verifies it: a variable keys an entry of T,
  ; which becomes a disequality at once if the variable has a type tag (I3), and so does a compound term that
  ; could still become the datum of the tag, kept whole, while the parts of one that cannot are verified.
  (define (μkanren-absento+ u tag s)
      (define (watch α)
        (if (μkanren-var? α)
            (let1 (c (μkanren-c-ref s α))
              (cond
                ((any (λ (tag*) (μkanren-tag-equal? tag tag*)) (μkanren-c-T c)) s)
                ((μkanren-c-A c) (μkanren-ext-D α tag (μkanren-subsume-prefixes α tag s)))
                (else (μkanren-subsume-prefixes α tag (μkanren-c-set s α (μkanren-c-with c T: (cons tag (μkanren-c-T c))))))))
            (let1 (Tc (μkanren-state-Tc s))
              (if (any (λ1-match/first ((,u* . ,tag*) (and (equal? u* α) (μkanren-tag-equal? tag tag*)))) Tc)
                  s
                  (let* ((s (μkanren-state-with/stores s (μkanren-state-C s) (μkanren-state-P s) (cons `(,α . ,tag) Tc)))
                         (s (μkanren-mark-t α s))
                         (datum (μkanren-tag-name tag)))
                    ; the datum of a compound tag is walked by `μkanren-tag-unify` too.
                    (if (μkanren-compound? datum) (μkanren-mark-t datum s) s))))))
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

  ; the prefix d posted again in `s`, where some of its variables may have been bound: `s` when one of its
  ; equations can no longer hold, #f when they all hold, else `s` with the bindings they still need.
  (define (μkanren-repost-prefix d s)
    (match/first (μkanren-state-unify/assoc d s '())
      (#f s)
      ((,s* ,d*) (and (not (μkanren-state-equal? s s*)) (μkanren-add-prefix d* s)))))

  ; `s`, where β is bound, with the record of β emptied (I1) and the prefixes with the ids `ids` it listed
  ; posted again, as `μkanren-repost-prefix` does: all are tried first, so that a violated one fails before
  ; any is dropped, and the others are dropped in one pass, those whose equations still need bindings
  ; then added as such.
  (define (μkanren-repost-prefixes β ids s)
    (let* ((P (μkanren-state-P s))
           (l (length/sbral P)))
      (let R ((ids ids) (P* P) (ds '()))
        (match/first ids
          (() (let1 (C (update/sbral/back (μkanren-var-index β) μkanren-c-empty μkanren-c-empty (μkanren-state-C s)))
                (fold μkanren-add-prefix (μkanren-state-with/stores s C P* (μkanren-state-Tc s)) ds)))
          ((,i . ,ids*) (let1 (d (sbral-ref P (- l 1 i)))
                          (if d
                              (match/first (μkanren-state-unify/assoc d s '())
                                (#f (R ids* (update/sbral (- l 1 i) #f P*) ds))
                                ((,s* ,d*) (and (not (μkanren-state-equal? s s*)) (R ids* (update/sbral (- l 1 i) #f P*) (cons d* ds)))))
                              (R ids* P* ds))))))))

  (define (μkanren-repost-Tc s)
    (μkanren-fold/and (λ (t s) (μkanren-absento+ (lhs t) (rhs t) s))
                      (μkanren-state-with/stores s (μkanren-state-C s) (μkanren-state-P s) '())
                      (μkanren-state-Tc s)))

  ; hands on the constraints of the variables that the unification leading to `s` bound, `assocs`: the record
  ; of each is emptied (I1), and its prefixes, its type tag and its absento tags are posted again in `s`, on
  ; its walk; the compound keys of T are verified again when one of them is marked with `t` (I5). A variable
  ; without a record costs nothing more than its lookup.
  (define (μkanren-repost s assocs)
    (let R ((assocs assocs) (s s) (Tc? #f))
      (cond
        ((not s) #f)
        ((null? assocs) (if Tc? (μkanren-repost-Tc s) s))
        (else (let* ((β (lhs (car assocs))) (c (μkanren-c-ref s β)))
                (if (μkanren-c-empty? c)
                    (R (cdr assocs) s Tc?)
                    (let* ((s (μkanren-repost-prefixes β (μkanren-c-D c) s))
                           (s (if (and s (μkanren-c-A c)) (μkanren-post-type β (μkanren-c-A c) s) s))
                           (s (μkanren-fold/and (λ (tag s) (μkanren-absento+ β tag s)) s (μkanren-c-T c))))
                      (R (cdr assocs) s (or Tc? (μkanren-c-t c))))))))))

  ; `s` with its stores indexed: given as lists (`raw`), they are posted anew on the substitution of `s`, so
  ; that the answer is #f when they no longer hold.
  (define (μkanren-state-index s)
    (match/first (μkanren-state-raw s)
      (#f s)
      ((,D ,A ,T) (let* ((s (make-μkanren-state/C (μkanren-state-vars-count s) (μkanren-state-S s) empty/sbral empty/sbral '()
                                                   (μkanren-state-tags s) #f))
                         (s (μkanren-fold/and (λ (a s) (μkanren-post-type (lhs a) (rhs a) s)) s A))
                         (s (μkanren-fold/and (λ (t s) (μkanren-absento+ (lhs t) (rhs t) s)) s T)))
                    (μkanren-fold/and μkanren-repost-prefix s D)))))

  (define (((μkanren-make-tag-A tag) u) s0)
    (let1 (s (μkanren-state-index s0))
      (match/first (and s (μkanren-post-type u tag s))
        (#f (✗° s0))
        (,s* (✓° (if (μkanren-state-equal? s s*) s* (μkanren-state-with s* tags: (cons tag (μkanren-state-tags s*)))))))))
  
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
                          ((if (μkanren-state-raw s*) (μkanren-state-index s*) (μkanren-repost s* assocs)) => ✓°)
                          (else (✗° s*))))))
    (cond
      ((μkanren-unify u v s '()) => post°)
      (else (✗° s))))

  (define ((≠° u v) s0)
    (let1 (s (μkanren-state-index s0))
      (cond
        ((not s) (✗° s0))
        ((μkanren-unify u v s '()) => (λ1-match/first ((,s* ,d) (if (μkanren-state-equal? s s*) (✗° s) (✓° (μkanren-add-prefix d s))))))
        (else (✓° s)))))

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
        ((let1 (s* (μkanren-state-index s)) (and s* (μkanren-absento+ u tag* s*))) => ✓°)
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