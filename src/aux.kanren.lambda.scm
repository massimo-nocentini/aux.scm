
;;; (aux kanren lambda) -- λProlog's higher-order hereditary Harrop formulas over (aux kanren micro).
;;;
;;; Miller & Nadathur, "Programming with Higher-Order Logic" (CUP 2012): λ-tree syntax, universal goals
;;; (GENERIC, `∀°`), hypothetical goals (AUGMENT, `⊃°`) over dynamic relations, and higher-order pattern
;;; unification (L_λ, ch. 8) with universe levels (Nadathur, Jayaraman & Kwon 1995; Nadathur & Linnell 2005)
;;; in place of raising; equations outside L_λ wait until they become patterns (the book's "dynamic L_λ",
;;; p.241-242) and are reported as residual constraints otherwise.
;;;
;;; The module is a client of micro's extension protocol: it registers one `μkanren-extension` and keeps its
;;; per-branch data in the state's `X` slot, all of it (micro has one slot for every extension, so no other
;;; extension that keeps data there can be loaded with this one), so micro's own `=°`, `≠°`, `absent°`,
;;; `symbol°`, `°->list` and every relation built on them work on λ-terms, and are scope-safe under `∀°`.
;;; Its hooks unify with micro's `μkanren-unify`, which keeps the stores indexed: `=°` hands on the
;;; constraints of every binding they make, after the `post` hook.
;;;
;;; The extension is registered when the module is loaded, for the whole process and for good: micro's
;;; registry is global and takes nothing back, so every query run after the import, a first-order one too,
;;; goes through the hooks. Its answers stay the same, micro's own suites pass with the module loaded
;;; (`make test-microkanren-with-lambda`), but every binding pays the `post` hook, which walks the value of
;;; each new binding of a variable that a prefix of D reaches, see `μkanren-λ-post`. It registers too how
;;; (aux kanren micro show) reads its answers, see `μkanren-λ-show-extension`.
;;;
;;; ==================== TERMS ====================
;;;   abstraction     `μkanren-lam`    (hint body), locally nameless: the body refers to its binder by the de
;;;                                    Bruijn `μkanren-bvar` 0; `hint`, an interned symbol, only names the
;;;                                    binder when printing, so terms built alike are `equal?`;
;;;   application     `μkanren-app`    (head args): flexible when the head walks to an unbound variable, a
;;;                                    β-redex when it walks to an abstraction (reduced on the fly, see
;;;                                    `μkanren-hnorm`), rigid otherwise;
;;;   eigenvariable   `μkanren-eigen`  (name level id), a scoped constant of GENERIC; `id`, an uninterned
;;;                                    symbol, is its identity;
;;;   bound variable  `μkanren-bvar`   (index), only inside the body of an abstraction.
;;; First-order structure (`(list 'abs M)`, records, vectors) is plain Scheme data, as in micro.
;;;
;;; ==================== LEVELS ====================
;;; The state carries a universe level `depth`, raised by `∀°` for the extent of its body; an eigenvariable
;;; gets level depth+1, a logic variable the depth at its creation. A variable of level ℓ may be bound only
;;; to terms whose eigenvariables have level ≤ ℓ, and the unbound variables of a level above ℓ in such a
;;; term are lowered to ℓ (bound to a fresh variable of level ℓ raised over the eigenvariables they could
;;; see, book p.241). The eigenvariables that the unifier opens under binders have level +∞. The level of
;;; variable i is kept in an sbral `L` indexed like S and filled lazily: the variables created since `L`
;;; was last extended have the current depth, so `L` is extended exactly when the depth changes.

(module (aux kanren lambda)
  ( ; terms
    ƛ @ ≐ μkanren-abstract μkanren-apply μkanren-lam? μkanren-app? μkanren-eigen? μkanren-bvar? μkanren-λ-term?
    μkanren-lam-hint μkanren-eigen-name μkanren-eigen-level μkanren-hnorm μkanren-α=?
    ; goals; `μkanren-hyps°`, `μkanren-clause` & co. are expanded to by `define-dynamic-relation` and `⊃°`
    ∀° ∀ª ⊃° ⊃ª define-dynamic-relation μkanren-dynamic μkanren-dynamic? μkanren-hyps° μkanren-clause μkanren-clause/vars μkanren-clause*
    ; answers
    °->list/datum μkanren-datum μkanren-λ-show-extension
    ; engine
    μkanren-λ-reading μkanren-λ-depth μkanren-λ-level μkanren-λ-delayed μkanren-λ-disequalities μkanren-λ-extension)

  (import scheme
          (chicken base)
          (chicken eval)
          (only (scheme base) make-parameter)
          (only (chicken sort) sort)
          (only (chicken string) ->string conc string-intersperse)
          (chicken memory representation)
          srfi-1
          (aux base)
          (aux stream)
          (aux fds sbral)
          (aux kanren micro)
          (only (aux kanren micro show) make-μkanren-show-extension μkanren-show-extension-register!))

  ;;; ==================== TERMS ====================

  (define-record μkanren-lam hint body)
  (define-record μkanren-app head args)
  (define-record μkanren-eigen name level id)
  (define-record μkanren-bvar index)

  (define (μkanren-λ-term? u) (or (μkanren-lam? u) (μkanren-app? u) (μkanren-eigen? u) (μkanren-bvar? u)))

  (define (μkanren-eigen* name level) (make-μkanren-eigen name level (string->uninterned-symbol (symbol->string name))))
  (define (μkanren-eigen=? e e*) (eq? (μkanren-eigen-id e) (μkanren-eigen-id e*)))

  ; the first bound variables are shared.
  (define μkanren-bvars (list->vector (map make-μkanren-bvar (iota 64))))
  (define (μkanren-bvar* i) (if (< i 64) (vector-ref μkanren-bvars i) (make-μkanren-bvar i)))

  (define (μkanren-other-record? t) (and (record-instance? t) (not (μkanren-var? t)) (not (μkanren-λ-term? t))))

  ; `t`, a pair, vector or foreign record, rebuilt with `F` on its parts: the very same object when no part changes.
  (define (μkanren-rebuild t F)
    (cond
      ((pair? t) (let ((a (F (car t))) (d (F (cdr t)))) (if (and (eq? a (car t)) (eq? d (cdr t))) t (cons a d))))
      ((vector? t) (let* ((l (vector->list t)) (l* (map F l))) (if (every eq? l l*) t (list->vector l*))))
      ((μkanren-other-record? t)
        (let* ((l (vector->list (record->vector t))) (l* (cons (car l) (map F (cdr l)))))
          (if (every eq? l l*) t (apply make-record-instance l*))))
      (else t)))

  (define (μkanren-lam/body t b) (if (eq? b (μkanren-lam-body t)) t (make-μkanren-lam (μkanren-lam-hint t) b)))

  (define (μkanren-app/parts t h args)
    (if (and (eq? h (μkanren-app-head t)) (every eq? args (μkanren-app-args t))) t (make-μkanren-app h args)))

  ; the de Bruijn operations never walk through the substitution: a term bound to a variable is locally
  ; closed, it has no loose bound variable.

  ; `t` with its loose bound variables of index ≥ c shifted by d.
  (define (μkanren-shift t d c)
    (let S ((t t) (c c))
      (cond
        ((μkanren-bvar? t) (let1 (i (μkanren-bvar-index t)) (if (<= c i) (μkanren-bvar* (+ i d)) t)))
        ((μkanren-lam? t) (μkanren-lam/body t (S (μkanren-lam-body t) (add1 c))))
        ((μkanren-app? t) (μkanren-app/parts t (S (μkanren-app-head t) c) (map (λ (a) (S a c)) (μkanren-app-args t))))
        ((or (μkanren-eigen? t) (μkanren-var? t)) t)
        (else (μkanren-rebuild t (λ (u) (S u c)))))))

  ; whether `t` has no loose bound variable.
  (define (μkanren-closed? t)
    (let C ((t t) (k 0))
      (cond
        ((μkanren-bvar? t) (< (μkanren-bvar-index t) k))
        ((μkanren-lam? t) (C (μkanren-lam-body t) (add1 k)))
        ((μkanren-app? t) (and (C (μkanren-app-head t) k) (every (λ (a) (C a k)) (μkanren-app-args t))))
        ((or (μkanren-eigen? t) (μkanren-var? t)) #t)
        ((pair? t) (and (C (car t) k) (C (cdr t) k)))
        ((vector? t) (every (λ (a) (C a k)) (vector->list t)))
        ((μkanren-other-record? t) (every (λ (a) (C a k)) (cdr (vector->list (record->vector t)))))
        (else #t))))

  ; the body `t` of an abstraction with its bound variable replaced by `u`; `u` is shifted under binders
  ; only when it has loose bound variables itself, which is decided once.
  (define (μkanren-instantiate t u)
    (let ((closed 'unknown))
      (define (u/k k)
        (when (eq? closed 'unknown) (set! closed (μkanren-closed? u)))
        (if closed u (μkanren-shift u k 0)))
      (let I ((t t) (k 0))
        (cond
          ((μkanren-bvar? t) (let1 (i (μkanren-bvar-index t))
                               (cond
                                 ((< i k) t)
                                 ((= i k) (if (zero? k) u (u/k k)))
                                 (else (μkanren-bvar* (sub1 i))))))
          ((μkanren-lam? t) (μkanren-lam/body t (I (μkanren-lam-body t) (add1 k))))
          ((μkanren-app? t) (μkanren-app/parts t (I (μkanren-app-head t) k) (map (λ (a) (I a k)) (μkanren-app-args t))))
          ((or (μkanren-eigen? t) (μkanren-var? t)) t)
          (else (μkanren-rebuild t (λ (v) (I v k))))))))

  ; `t` with the eigenvariable `e` turned into the bound variable of a new outermost binder.
  (define (μkanren-close t e)
    (let C ((t t) (k 0))
      (cond
        ((μkanren-eigen? t) (if (μkanren-eigen=? t e) (μkanren-bvar* k) t))
        ((μkanren-lam? t) (μkanren-lam/body t (C (μkanren-lam-body t) (add1 k))))
        ((μkanren-app? t) (μkanren-app/parts t (C (μkanren-app-head t) k) (map (λ (a) (C a k)) (μkanren-app-args t))))
        ((or (μkanren-bvar? t) (μkanren-var? t)) t)
        (else (μkanren-rebuild t (λ (v) (C v k)))))))

  (define (μkanren-beta f args)
    (cond
      ((null? args) f)
      ((μkanren-lam? f) (μkanren-beta (μkanren-instantiate (μkanren-lam-body f) (car args)) (cdr args)))
      ((μkanren-app? f) (make-μkanren-app (μkanren-app-head f) (append (μkanren-app-args f) args)))
      (else (make-μkanren-app f args))))

  ; whether the loose bound variable 0 occurs in `t`.
  (define (μkanren-free0? t)
    (let F ((t t) (k 0))
      (cond
        ((μkanren-bvar? t) (= (μkanren-bvar-index t) k))
        ((μkanren-lam? t) (F (μkanren-lam-body t) (add1 k)))
        ((μkanren-app? t) (or (F (μkanren-app-head t) k) (any (λ (a) (F a k)) (μkanren-app-args t))))
        ((or (μkanren-eigen? t) (μkanren-var? t)) #f)
        ((pair? t) (or (F (car t) k) (F (cdr t) k)))
        ((vector? t) (any (λ (a) (F a k)) (vector->list t)))
        ((μkanren-other-record? t) (any (λ (a) (F a k)) (cdr (vector->list (record->vector t)))))
        (else #f))))

  ; the abstraction of body `b`, built η-short: `λx.(t x)` is `t` when x is not free in t. Every abstraction
  ; is built so, by `ƛ` and by the unifier, so that `λx.(F x) = F` never reaches micro's occurs check, which
  ; binds a variable before any extension sees the equation.
  (define (μkanren-lam/η hint b)
    (if (μkanren-η-redex? b μkanren-free0?) (μkanren-η b) (make-μkanren-lam hint b)))

  ; whether the body `b` of an abstraction is `h ā x`, x its bound variable and not free in `h ā`, as told
  ; by `free0?`.
  (define (μkanren-η-redex? b free0?)
    (and (μkanren-app? b)
         (let1 (args (μkanren-app-args b))
           (and (μkanren-bvar? (last args)) (zero? (μkanren-bvar-index (last args)))
                (not (free0? (μkanren-app-head b)))
                (not (any free0? (drop-right args 1)))))))

  ; the contraction of the η-redex whose body is `b`.
  (define (μkanren-η b) (μkanren-shift (μkanren-beta (μkanren-app-head b) (drop-right (μkanren-app-args b) 1)) -1 1))

  ; the abstraction of the term built by `f` from a placeholder eigenvariable; `name` names the binder.
  (define (μkanren-abstract name f)
    (let1 (p (μkanren-eigen* name +inf.0))
      (μkanren-lam/η name (μkanren-close (f p) p))))

  ; `(ƛ (x y) body)` is λx.λy.body: `x`, `y` are bound to placeholders while `body` is built, so the Scheme
  ; binders give the object binders, shadowing included.
  (define-syntax ƛ
    (syntax-rules ()
      ((_ () body) body)
      ((_ (x y ...) body) (μkanren-abstract 'x (λ (x) (ƛ (y ...) body))))))

  ; application, β-reduced at once when `f` is already an abstraction.
  (define (@ f . args) (μkanren-beta f args))
  (define (μkanren-apply f args) (μkanren-beta f args))

  ; α-equivalence of two deep values (binder hints ignored).
  (define (μkanren-α=? u v)
    (let E ((u u) (v v))
      (cond
        ((eq? u v) #t)
        ((μkanren-lam? u) (and (μkanren-lam? v) (E (μkanren-lam-body u) (μkanren-lam-body v))))
        ((μkanren-app? u) (and (μkanren-app? v) (E (μkanren-app-head u) (μkanren-app-head v))
                               (= (length (μkanren-app-args u)) (length (μkanren-app-args v)))
                               (every E (μkanren-app-args u) (μkanren-app-args v))))
        ((μkanren-eigen? u) (and (μkanren-eigen? v) (μkanren-eigen=? u v)))
        ((μkanren-bvar? u) (and (μkanren-bvar? v) (= (μkanren-bvar-index u) (μkanren-bvar-index v))))
        ((μkanren-var? u) (and (μkanren-var? v) (= (μkanren-var-index u) (μkanren-var-index v))))
        ((pair? u) (and (pair? v) (E (car u) (car v)) (E (cdr u) (cdr v))))
        ((vector? u) (and (vector? v) (E (vector->list u) (vector->list v))))
        ((μkanren-other-record? u) (and (μkanren-other-record? v) (E (record->vector u) (record->vector v))))
        (else (equal? u v)))))

  ;;; ==================== THE EXTENSION'S SLOT ====================

  ; `depth` the universe level, `top` the highest one reached so far, `L`/`Ln` the levels of the variables
  ; below `Ln`, `Γ` the hypothetical clauses as an alist `(relation . clauses)`, most recent first, `E` the
  ; suspended constraints, `N` the disequalities that involve λ-terms or eigenvariables, `W` an sbral indexed
  ; like S of the ids of the prefixes of D that reach each variable where micro's records do not list them,
  ; below a compound side or through a binding, see `μkanren-migrate`.
  (define-record μkanren-hh depth top L Ln Γ E N W)

  (define μkanren-hh0 (make-μkanren-hh 0 0 empty/sbral 0 '() '() '() empty/sbral))

  (define (μkanren-hh s) (let1 (x (μkanren-state-X s)) (if (null? x) μkanren-hh0 x)))

  (define (μkanren-hh-with x #!key (depth (μkanren-hh-depth x)) (top (μkanren-hh-top x)) (L (μkanren-hh-L x)) (Ln (μkanren-hh-Ln x))
                                   (Γ (μkanren-hh-Γ x)) (E (μkanren-hh-E x)) (N (μkanren-hh-N x)) (W (μkanren-hh-W x)))
    (make-μkanren-hh depth top L Ln Γ E N W))

  (define (μkanren-with s x) (μkanren-state-with s X: x))
  (define (μkanren-with-E s E) (μkanren-with s (μkanren-hh-with (μkanren-hh s) E: E)))
  (define (μkanren-push-E s e) (μkanren-with-E s (cons e (μkanren-hh-E (μkanren-hh s)))))

  (define (μkanren-λ-depth s) (μkanren-hh-depth (μkanren-hh s)))
  (define (μkanren-λ-delayed s) (μkanren-hh-E (μkanren-hh s)))
  (define (μkanren-λ-disequalities s) (μkanren-hh-N (μkanren-hh s)))

  ; `L` extended with the current depth up to `vc`.
  (define (μkanren-hh-pad x vc)
    (let loop ((L (μkanren-hh-L x)) (n (μkanren-hh-Ln x)))
      (cond
        ((< n vc) (loop (cons/sbral (μkanren-hh-depth x) L) (add1 n)))
        ((= n (μkanren-hh-Ln x)) x)
        (else (μkanren-hh-with x L: L Ln: n)))))

  (define (μkanren-level α s)
    (let* ((x (μkanren-hh s)) (i (μkanren-var-index α)) (n (μkanren-hh-Ln x)))
      (cond
        ((< i 0) 0)
        ((< i n) (sbral-ref (μkanren-hh-L x) (- n 1 i)))
        (else (μkanren-hh-depth x)))))

  (define μkanren-λ-level μkanren-level)

  ; whether some variable or eigenvariable may have a level above 0: until a `∀°`, none has.
  (define (μkanren-levels? s) (let1 (x (μkanren-hh s)) (or (positive? (μkanren-hh-depth x)) (positive? (μkanren-hh-Ln x)))))

  ; a fresh variable of level ℓ, and the state that knows it.
  (define (μkanren-fresh s ℓ)
    (let* ((vc (μkanren-state-vars-count s))
           (x  (μkanren-hh-pad (μkanren-hh s) vc))
           (x* (μkanren-hh-with x L: (cons/sbral ℓ (μkanren-hh-L x)) Ln: (add1 vc))))
      (values (make-μkanren-var vc) (μkanren-state-with s vars-count: (add1 vc) X: x*))))

  ;;; ==================== NORMALISATION ====================

  ; the head normal form of `t` in `s`: walked, with β-redexes at the head reduced and nested applications
  ; flattened. Under the pattern restriction every reduction is a β₀ renaming; `@` on instantiated heads
  ; (dynamic L_λ, book p.241) makes it full β, which may not terminate on untyped terms.
  ; An abstraction is built η-short, but binding a variable inside it may make it an η-redex, λx.F (G x)
  ; with G := λz.z: such a redex at the head is contracted too, so that terms compare modulo βη (p.236).
  (define (μkanren-hnorm t s)
    (let1 (t* (μkanren-state-find t s))
      (cond
        ((μkanren-app? t*)
          (let ((h (μkanren-hnorm (μkanren-app-head t*) s)) (args (μkanren-app-args t*)))
            (cond
              ((null? args) h)
              ((μkanren-lam? h) (μkanren-hnorm (μkanren-beta h args) s))
              ((μkanren-app? h) (make-μkanren-app (μkanren-app-head h) (append (μkanren-app-args h) args)))
              ((eq? h (μkanren-app-head t*)) t*)
              (else (make-μkanren-app h args)))))
        ((and (μkanren-lam? t*) (μkanren-app? (μkanren-state-find (μkanren-lam-body t*) s)))
          (let1 (b (μkanren-hnorm (μkanren-lam-body t*) s))
            (if (and (μkanren-app? b)
                     (μkanren-η-redex? (μkanren-app/parts b (μkanren-app-head b) (append (drop-right (μkanren-app-args b) 1)
                                                                                         (list (μkanren-hnorm (last (μkanren-app-args b)) s))))
                                       ; a part bound since may have lost its occurrence of x: its value tells.
                                       (λ (u) (and (μkanren-free0? u) (μkanren-free0? (μkanren-state-find/value u s))))))
                (μkanren-hnorm (μkanren-η b) s)
                t*)))
        (else t*))))

  (define (μkanren-flex? t) (and (μkanren-app? t) (μkanren-var-working? (μkanren-app-head t))))

  ; whether the walked arguments `as` of a variable of level ℓ are a pattern: distinct eigenvariables, each
  ; quantified inside the scope of the variable, i.e. of a level above ℓ (book p.236, Miller's L_λ).
  (define (μkanren-pattern? as ℓ)
    (let loop ((as as) (seen '()))
      (cond
        ((null? as) #t)
        ((and (μkanren-eigen? (car as)) (< ℓ (μkanren-eigen-level (car as))) (not (memq (μkanren-eigen-id (car as)) seen)))
          (loop (cdr as) (cons (μkanren-eigen-id (car as)) seen)))
        (else #f))))

  (define (μkanren-var=? α β) (= (μkanren-var-index α) (μkanren-var-index β)))

  (define (μkanren-lams hints t) (fold-right μkanren-lam/η t hints))

  ;;; ==================== VARIABLE ELIMINATION ====================

  ; whether binding a variable of level ℓ to `t` asks for no work, the first-order fast path of the scope
  ; check: `t` holds no eigenvariable nor unbound variable of a level above ℓ, and no application whose head
  ; is a variable or an abstraction (variable elimination might prune, lower or reduce it). The walk
  ; allocates nothing, like micro's occurs check.
  (define (μkanren-plain? t ℓ s)
    (let P ((t t))
      (cond
        ((μkanren-var? t) (let1 (t* (μkanren-state-find t s))
                            (if (μkanren-var? t*) (or (not (μkanren-var-working? t*)) (<= (μkanren-level t* s) ℓ)) (P t*))))
        ((pair? t) (and (P (car t)) (P (cdr t))))
        ((μkanren-eigen? t) (<= (μkanren-eigen-level t) ℓ))
        ((μkanren-bvar? t) #t)
        ((μkanren-lam? t) (P (μkanren-lam-body t)))
        ((μkanren-app? t) (let1 (h (μkanren-state-find (μkanren-app-head t) s))
                            (and (or (μkanren-eigen? h) (μkanren-bvar? h)) (P h) (every P (μkanren-app-args t)))))
        ((vector? t) (every P (vector->list t)))
        ((μkanren-other-record? t) (every P (cdr (vector->list (record->vector t)))))
        (else #t))))

  ; book p.239, with levels in place of the prefix. For `F ā = t`, F of level ℓ and ā a pattern, the body t'
  ; of the binding F := λā.t', walking t once: an eigenvariable of ā becomes its bound variable, one of level
  ; ≤ ℓ stays, any other fails the scope check (Example 8.J); F b̄ itself fails the occurs check (8.I) below
  ; a rigid constructor when its instance cannot be smaller than that of F ā, b̄ being rigid terms (no
  ; variable, flexible term nor abstraction, so no β) as many as ā and the binders on top of t (η), or in
  ; any number when no instance of F can be a projection, the root of t being a constant, an eigenvariable
  ; not in ā or a pair (`F = λxy.g (F y)`, η-short), and suspends the equation otherwise: `F x = x (F (λz.z))` holds with
  ; F := λa. a c, `F x y = x (F y)` with F := λw.w, and `F = λw.F a`, with only binders above, with
  ; F := λw.c; an unbound Y of a level above ℓ is lowered to ℓ, raised over the eigenvariables of ā it
  ; could see (delayed raising, 8.K); a flexible G b̄ that is a pattern is pruned of the arguments F may not
  ; see (8.H) and lowered likewise. Only strict occurrences are pruned or lowered (Dowek, Hardin, Kirchner &
  ; Pfenning): in an argument of a flexible term that is no pattern, which G may drop, nothing is bound; the
  ; argument is pruned when it holds, rigidly, an eigenvariable that F may not see (every instance of it
  ; would hold it), and any other trouble in it suspends the whole equation; and when some argument is, or
  ; may become, an abstraction, which G may apply to drop another argument or the eigenvariables of its own
  ; body, G is neither pruned nor lowered: the equation is suspended. A bound variable of such an
  ; argument is no rigid head either, since G may apply the argument. F is #f to check a binding `X := t`
  ; just made by micro, with ā empty. Answers `(t' s* assocs*)`, #f when no unifier exists, or `'delay`
  ; (the caller then suspends the equation and drops `s*`).
  (define (μkanren-elim ℓ F ā t s assocs)
    (define n (length ā))
    (define (position e) (and (μkanren-eigen? e) (list-index (λ (a) (μkanren-eigen=? a e)) ā)))
    (define (bvar-of i k) (μkanren-bvar* (+ k (- n 1 i))))
    (call-with-current-continuation
      (λ (return)
        (define (bind α v s as fail) (or (μkanren-unify α v s as) (fail 'bind)))
        (define (local? b k) (and (μkanren-bvar? b) (< (μkanren-bvar-index b) k)))
        ; whether the head-normal `b` is, or may become, an abstraction.
        (define (functional? b) (or (μkanren-lam? b) (μkanren-var-working? b) (μkanren-flex? b)))
        ; the binders on top of `t`: `F ā = λȳ.t'` is `F ā ȳ = t'`, by η.
        (define (binders t) (let1 (t* (μkanren-hnorm t s)) (if (μkanren-lam? t*) (add1 (binders (μkanren-lam-body t*))) 0)))
        ; the context `m` of a subterm: `'top` below binders only, `'rigid` below a rigid constructor, `'firm`
        ; below one whose root no instance of F can project onto (a constant, an eigenvariable not in ā, a
        ; pair: no bound variable of F's instance can head it), the local depth k0 at which an argument of
        ; a non-pattern was entered, on a rigid path from it, or `'weak` below a flexible head inside such an
        ; argument. No binding is made in the last two.
        (define (soft? m) (not (memq m '(top rigid firm))))
        (define (rigid m root) (if (eq? m 'top) root m))
        (define (E t k s as fail m)
          (let1 (t* (μkanren-hnorm t s))
            (cond
              ((μkanren-var-working? t*) (E/var t* k s as fail m))
              ((μkanren-eigen? t*)  (cond
                                      ((position t*) => (λ (i) (values (bvar-of i k) s as)))
                                      ((<= (μkanren-eigen-level t*) ℓ) (values t* s as))
                                      (else (fail (if (eq? m 'weak) 'soft 'scope)))))
              ((μkanren-bvar? t*) (values t* s as))
              ((μkanren-lam? t*) (receive (b s as) (E (μkanren-lam-body t*) (add1 k) s as fail m)
                                   (values (μkanren-lam/body t* b) s as)))
              ((μkanren-flex? t*) (if (soft? m)
                                      (E/flex/soft (μkanren-app-head t*) (μkanren-app-args t*) k s as fail)
                                      (E/flex (μkanren-app-head t*) (μkanren-app-args t*) k s as fail m)))
              ((μkanren-app? t*) (let* ((h (μkanren-app-head t*))
                                        (root (if (or (μkanren-bvar? h) (position h)) 'rigid 'firm))
                                        (m* (if (and (number? m) (μkanren-bvar? h) (< (μkanren-bvar-index h) (- k m))) 'weak (rigid m root))))
                                   (receive (h s as) (E h k s as fail (rigid m root))
                                     (receive (args s as) (E* (μkanren-app-args t*) k s as fail m*)
                                       (values (make-μkanren-app h args) s as)))))
              ((pair? t*) (receive (a s as) (E (car t*) k s as fail (rigid m 'firm))
                            (receive (d s as) (E (cdr t*) k s as fail (rigid m 'firm))
                              (values (if (and (eq? a (car t*)) (eq? d (cdr t*))) t* (cons a d)) s as))))
              ((vector? t*) (receive (l s as) (E* (vector->list t*) k s as fail (rigid m 'firm)) (values (list->vector l) s as)))
              ((μkanren-other-record? t*)
                (let1 (l (vector->list (record->vector t*)))
                  (receive (l* s as) (E* (cdr l) k s as fail (rigid m 'firm)) (values (apply make-record-instance (car l) l*) s as))))
              (else (values t* s as)))))
        (define (E* ts k s as fail m)
          (if (null? ts)
              (values '() s as)
              (receive (a s as) (E (car ts) k s as fail m)
                (receive (d s as) (E* (cdr ts) k s as fail m)
                  (values (cons a d) s as)))))
        (define (E/var Y k s as fail m)
          (let1 (ℓY (μkanren-level Y s))
            (cond
              ; F itself, applied to nothing: as large as F ā only when ā is empty.
              ((and F (μkanren-var=? Y F)) (if (zero? n) (fail 'occurs) (return 'delay)))
              ((<= ℓY ℓ) (values Y s as))
              ((soft? m) (fail 'soft))
              (else (let1 (extra (filter (λ (a) (<= (μkanren-eigen-level a) ℓY)) ā))
                      (receive (Y* s) (μkanren-fresh s ℓ)
                        (match1/first ((,s ,as) (bind Y (μkanren-beta Y* extra) s as fail))
                          (values (μkanren-beta Y* (map (λ (a) (bvar-of (position a) k)) extra)) s as))))))))
        ; a flexible term inside an argument of a non-pattern: kept as it is when it asks for no work.
        (define (E/flex/soft G bs k s as fail)
          (cond
            ((and F (μkanren-var=? G F)) (fail 'occurs))
            ((< ℓ (μkanren-level G s)) (fail 'soft))
            (else (receive (args s as) (E* bs k s as (λ (why) (fail 'soft)) 'weak)
                    (values (make-μkanren-app G args) s as)))))
        ; G b̄ becomes G' applied to the kept arguments and to `extra`, the eigenvariables of ā that G could
        ; see but G' cannot, G := λȳ. G' ȳ|kept extra, when something is dropped or G is lowered.
        (define (E/flex G bs k s as fail ctx)
          (let* ((bs (map (λ (b) (μkanren-hnorm b s)) bs))
                 (ℓG (μkanren-level G s))
                 (m (length bs))
                 (extra (if (< ℓ ℓG) (filter (λ (a) (<= (μkanren-eigen-level a) ℓG)) ā) '()))
                 ; a pattern in t: distinct, each local or an eigenvariable G may not see.
                 (pattern? (let loop ((bs bs) (seen '()))
                             (cond
                               ((null? bs) #t)
                               ((or (local? (car bs) k) (and (μkanren-eigen? (car bs)) (< ℓG (μkanren-eigen-level (car bs)))))
                                 (and (not (member (car bs) seen)) (loop (cdr bs) (cons (car bs) seen))))
                               (else #f)))))
            (when (and F (μkanren-var=? G F))
              (if (and (not (eq? ctx 'top)) (or (eq? ctx 'firm) (= m (+ n (binders t)))) (not (any functional? bs)))
                  (fail 'occurs)
                  (return 'delay)))
            (receive (kept s as)
              (if pattern?
                  (values (filter-map (λ (b j) (cond
                                                 ((local? b k) (cons b j))
                                                 ((position b) => (λ (i) (cons (bvar-of i k) j)))
                                                 ((<= (μkanren-eigen-level b) ℓ) (cons b j))
                                                 (else #f)))
                                      bs (ι m))
                          s as)
                  (E/args bs k s as))
              (cond
                ((and (= (length kept) m) (null? extra) (<= ℓG ℓ)) (values (make-μkanren-app G (map car kept)) s as))
                ; G may apply an argument that is, or may become, an abstraction to another one and drop it, or
                ; drop the eigenvariables of its own body: neither pruning nor lowering is strict then.
                ((and (not pattern?) (any functional? bs)) (return 'delay))
                (else
                  (receive (G* s) (μkanren-fresh s (min ℓ ℓG))
                    (let* ((hints (map (λ (b) (if (μkanren-eigen? b) (μkanren-eigen-name b) 'z)) bs))
                           (body (μkanren-beta G* (append (map (λ (p) (μkanren-bvar* (- m 1 (cdr p)))) kept) extra))))
                      (match1/first ((,s ,as) (bind G (μkanren-lams hints body) s as fail))
                        (values (μkanren-beta G* (append (map car kept) (map (λ (a) (bvar-of (position a) k)) extra)))
                                s as)))))))))
        ; the arguments of a flexible term that is no pattern: each is walked on its own, binding nothing, and
        ; pruned when it fails by scope on a rigid path.
        (define (E/args bs k s as)
          (let loop ((bs bs) (j 0) (kept '()))
            (if (null? bs)
                (values (reverse kept) s as)
                (match/first (call-with-current-continuation
                               (λ (k*) (receive (b s* as*) (E (car bs) k s as (λ (why) (k* why)) k) (list b))))
                  ((,b) (loop (cdr bs) (add1 j) (cons (cons b j) kept)))
                  (scope (loop (cdr bs) (add1 j) kept))
                  (_ (return 'delay))))))
        (receive (t* s* as*) (E t 0 s assocs (λ (why) (return #f)) 'top)
          (list t* s* as*)))))

  ;;; ==================== UNIFICATION ====================

  ; an eigenvariable for a binder that the unifier descends under: level +∞, so that no variable may
  ; capture it, except through its own pattern arguments.
  (define (μkanren-local-eigen hint) (μkanren-eigen* hint +inf.0))

  ; the eigenvariables of level +∞ in `t`, opened by the unifier under its binders.
  (define (μkanren-locals t)
    (let L ((t t) (acc '()))
      (cond
        ((μkanren-eigen? t) (if (and (= +inf.0 (μkanren-eigen-level t)) (not (any (λ (e) (μkanren-eigen=? e t)) acc))) (cons t acc) acc))
        ((μkanren-lam? t) (L (μkanren-lam-body t) acc))
        ((μkanren-app? t) (fold L (L (μkanren-app-head t) acc) (μkanren-app-args t)))
        ((pair? t) (L (cdr t) (L (car t) acc)))
        ((vector? t) (fold L acc (vector->list t)))
        ((μkanren-other-record? t) (fold L acc (cdr (vector->list (record->vector t)))))
        (else acc))))

  ; `t` closed over the eigenvariables `es`, the first one outermost.
  (define (μkanren-close* t es)
    (fold-right (λ (e t) (μkanren-lam/η (μkanren-eigen-name e) (μkanren-close t e))) t es))

  ; `u = v` suspended, closed over the binders the unifier had opened (`λL.u = λL.v`), unless trivially true.
  (define (μkanren-delay u v s assocs)
    (let ((u* (μkanren-state-find/value u s)) (v* (μkanren-state-find/value v s)))
      (if (μkanren-α=? u* v*)
          (list s assocs)
          (let1 (L (μkanren-locals (cons u* v*)))
            (list (μkanren-push-E s `(= ,(μkanren-close* u* L) ,(μkanren-close* v* L))) assocs)))))

  ; the bindings that `assocs*` has on top of `assocs`.
  (define (μkanren-new assocs* assocs)
    (let loop ((p assocs*) (new '()))
      (if (or (eq? p assocs) (null? p)) new (loop (cdr p) (cons (car p) new)))))

  ; `as` without the pairs `taken`, sharing the tail below the last of them.
  (define (μkanren-drop as taken)
    (if (null? taken)
        as
        (let D ((as as))
          (cond
            ((null? as) '())
            ((memq (car as) taken) (D (cdr as)))
            (else (let1 (d (D (cdr as))) (if (eq? d (cdr as)) as (cons (car as) d))))))))

  ; the scope check, eager, on the bindings `new` that micro just made: each `X := t` goes through variable
  ; elimination at the level of X, which fails on a captured eigenvariable and lowers or prunes the variables
  ; of t; when t holds a flexible term that only a later binding can decide, the binding is taken back into
  ; the suspended equation `X = t`, so that no eigenvariable sits in a binding it may not be in.
  (define (μkanren-enforce s new assocs)
    (let loop ((new new) (s s) (as assocs) (taken '()))
      (match/first new
        (() (list s (μkanren-drop as taken)))
        ((,p . ,new*) (let* ((α (car p)) (t (cdr p)) (ℓ (μkanren-level α s)))
                        (if (μkanren-plain? t ℓ s)
                            (loop new* s as taken)
                            (match/first (μkanren-elim ℓ #f '() t s as)
                              (#f #f)
                              (delay (match1/first ((,s _) (μkanren-delay α t (μkanren-state-forget α s) as))
                                       (loop new* s as (cons p taken))))
                              ((_ ,s* ,as*) (loop new* s* as* taken)))))))))

  ; a unification `r` made below a binder opened with a +∞ eigenvariable: its new bindings are checked at
  ; once, since the eigenvariable is local to this very unification.
  (define (μkanren-scoped r assocs)
    (and r (μkanren-enforce (car r) (μkanren-new (cadr r) assocs) (cadr r))))

  (define (μkanren-λ-unify u v s assocs)
    (let ((u* (μkanren-hnorm u s)) (v* (μkanren-hnorm v s)))
      (cond
        ((or (μkanren-var-working? u*) (μkanren-var-working? v*)) (μkanren-unify u* v* s assocs))
        ((μkanren-lam? u*) (if (μkanren-lam? v*) (μkanren-unify/ξ u* v* s assocs) (μkanren-unify/η u* v* s assocs)))
        ((μkanren-lam? v*) (μkanren-unify/η v* u* s assocs))
        ((μkanren-flex? u*) (if (μkanren-flex? v*) (μkanren-unify/flex-flex u* v* s assocs) (μkanren-unify/flex-rigid u* v* s assocs)))
        ((μkanren-flex? v*) (μkanren-unify/flex-rigid v* u* s assocs))
        ((and (μkanren-app? u*) (μkanren-app? v*) (= (length (μkanren-app-args u*)) (length (μkanren-app-args v*))))
          (let1 (r (μkanren-unify (μkanren-app-head u*) (μkanren-app-head v*) s assocs))
            (and r (μkanren-unify (μkanren-app-args u*) (μkanren-app-args v*) (car r) (cadr r)))))
        ((and (μkanren-eigen? u*) (μkanren-eigen? v*)) (and (μkanren-eigen=? u* v*) (list s assocs)))
        ((and (μkanren-bvar? u*) (μkanren-bvar? v*)) (and (= (μkanren-bvar-index u*) (μkanren-bvar-index v*)) (list s assocs)))
        ((or (μkanren-λ-term? u*) (μkanren-λ-term? v*)) #f)
        (else (μkanren-unify u* v* s assocs)))))

  ; ξ: both sides are abstractions, opened with the same new eigenvariable (Miller's ξ, Nipkow's Abs-Abs).
  (define (μkanren-unify/ξ u v s assocs)
    (let1 (c (μkanren-local-eigen (μkanren-lam-hint u)))
      (μkanren-scoped (μkanren-unify (μkanren-instantiate (μkanren-lam-body u) c) (μkanren-instantiate (μkanren-lam-body v) c) s assocs) assocs)))

  ; η: `λx.b = t` is `b = t x` (untyped: only when one side is an abstraction, Miller §9.3).
  (define (μkanren-unify/η l t s assocs)
    (let1 (c (μkanren-local-eigen (μkanren-lam-hint l)))
      (μkanren-scoped (μkanren-unify (μkanren-instantiate (μkanren-lam-body l) c) (μkanren-beta t (list c)) s assocs) assocs)))

  (define (μkanren-unify/flex-flex u v s assocs)
    (let* ((F (μkanren-app-head u)) (G (μkanren-app-head v))
           (as (map (λ (a) (μkanren-hnorm a s)) (μkanren-app-args u)))
           (bs (map (λ (b) (μkanren-hnorm b s)) (μkanren-app-args v)))
           (ℓ (μkanren-level F s)))
      (cond
        ((not (μkanren-pattern? as ℓ))
          (if (μkanren-pattern? bs (μkanren-level G s)) (μkanren-unify/flex-rigid v u s assocs) (μkanren-delay u v s assocs)))
        ((μkanren-var=? F G)
          (cond
            ((not (μkanren-pattern? bs ℓ)) (μkanren-delay u v s assocs))
            ((not (= (length as) (length bs))) #f)
            ((every μkanren-eigen=? as bs) (list s assocs))
            ; same head: keep the positions where the arguments agree (book p.238).
            (else (receive (H s) (μkanren-fresh s ℓ)
                    (let* ((m (length as))
                           (same (filter-map (λ (a b j) (and (μkanren-eigen=? a b) (μkanren-bvar* (- m 1 j)))) as bs (ι m))))
                      (μkanren-unify F (μkanren-lams (map μkanren-eigen-name as) (μkanren-beta H same)) s assocs))))))
        ; different heads: variable elimination of F prunes G as needed (p.238).
        (else (μkanren-unify/flex-rigid u v s assocs)))))

  (define (μkanren-unify/flex-rigid u t s assocs)
    (let* ((F (μkanren-app-head u))
           (as (map (λ (a) (μkanren-hnorm a s)) (μkanren-app-args u)))
           (ℓ (μkanren-level F s)))
      (if (not (μkanren-pattern? as ℓ))
          (μkanren-delay u t s assocs)
          (match/first (μkanren-elim ℓ F as t s assocs)
            (#f #f)
            (delay (μkanren-delay u t s assocs))
            ((,t* ,s* ,as*) (μkanren-unify F (μkanren-lams (map μkanren-eigen-name as) t*) s* as*))))))

  ; micro's occurs check failed on `α := t`: when α occurs inside a λ-term, the equation is `F = t` with no
  ; arguments, modulo β (`q = F q` holds when F is λz.z), decided by variable elimination, which suspends it
  ; when the occurrence is in the argument of a flexible term (`F = λx.G (F x)` holds with G := λz.z).
  (define (μkanren-λ-occurs α t s assocs)
    (and (μkanren-has-term? t s)
         (let1 (t* (μkanren-hnorm t s))
           (if (and (μkanren-var? t*) (μkanren-var=? t* α))
               (list s assocs)
               (match/first (μkanren-elim (μkanren-level α s) α '() t s assocs)
                 (#f #f)
                 (delay (μkanren-delay α t s assocs))
                 ((,t* ,s* ,as*) (μkanren-unify α t* s* as*)))))))

  ;;; ==================== SUSPENDED CONSTRAINTS ====================

  ; E holds `(= u v)` (an equation outside L_λ, or a binding whose scope check a non-pattern decides),
  ; `(tag tag u)` (a type on a flexible term) and `(absent d u)` (an absento whose datum outlived its
  ; eigenvariable's scope). Each is retried when something is bound: `'fail`, or `(s* assocs*)` where it is
  ; either solved or suspended again.

  ; the type `tag` on `u`: a variable takes it in A, a flexible term waits, anything else is checked by the
  ; type's predicate, as micro checks every other record: `symbol?` and `number?` hold of no λ-term nor
  ; eigenvariable, a type of the user's may. Two different types on the same flexible term fail at once,
  ; as on a variable of A: micro's types are disjoint.
  ; micro asks a type to hold atoms only, see `define-μkanren-tag`. A λ-term is no atom, nor is an
  ; eigenvariable, which a variable of a lower level may never take, so a type within that contract holds of
  ; neither. A type outside it, one that holds compound values, is decided here by its predicate as micro
  ; decides it on a pair, and what micro derives from a type (I3, the constraints on hidden variables that
  ; answers drop) is as unwarranted for it as it is on pairs: the layer adds no rule of types of its own.
  (define (μkanren-λ-tag u tag s)
    (let1 (h (μkanren-hnorm u s))
      (cond
        ((μkanren-var-working? h) (match/first (((μkanren-make-tag-A tag) h) s) (() #f) ((,s*) s*)))
        ((μkanren-flex? h)
          (let1 (v (μkanren-state-find/value h s))
            (let loop ((E (μkanren-hh-E (μkanren-hh s))))
              (match/first E
                (() (μkanren-push-E s `(tag ,tag ,u)))
                (((tag ,tag* ,u*) . ,E*) (if (μkanren-α=? v (μkanren-state-find/value u* s))
                                             (and (μkanren-tag-equal? tag tag*) s)
                                             (loop E*)))
                ((_ . ,E*) (loop E*))))))
        (else (and (μkanren-tag-pred? tag h) s)))))

  ; micro's absento of the datum `d` in `u`: `#f` when violated, `'holds` when the stores already entail it,
  ; posting it then answering the very state, or else `'pending`. (micro may keep an absento as disequalities
  ; and tags of the variables of its datum, besides T: only the state as a whole tells that it adds nothing.)
  (define (μkanren-λ-absent d u s)
    (match/first ((absent° d u) s)
      (() #f)
      ((,s*) (if (eq? s* s) 'holds 'pending))))

  ; whether the absento of the datum `d` in `u` holds for good by levels: `d` holds an eigenvariable c that
  ; `u` does not, and every variable of `u` has a level below ℓ(c), so no instance of `u` can hold c.
  (define (μkanren-absent-entailed? d u s)
    (let ((u* (μkanren-state-find/value u s)) (d* (μkanren-state-find/value d s)))
      (let1 (us (filter μkanren-var-working? (μkanren-vars u*)))
        (any (λ (c) (and (finite? (μkanren-eigen-level c))
                         (not (any (λ (e) (μkanren-eigen=? e c)) (μkanren-eigens u*)))
                         (every (λ (α) (< (μkanren-level α s) (μkanren-eigen-level c))) us)))
             (μkanren-eigens d*)))))

  ; a suspended `α = t`, α unbound: α is bound only once the scope check of the binding passes, so that a
  ; check still undecided binds nothing and the wake-up ends.
  (define (μkanren-retry/bind α t e s as)
    (let1 (ℓ (μkanren-level α s))
      (if (μkanren-plain? t ℓ s)
          (or (μkanren-unify α t s as) 'fail)
          (match/first (μkanren-elim ℓ α '() t s as)
            (#f 'fail)
            (delay (list (μkanren-push-E s e) as))
            ((,t* ,s* ,as*) (or (μkanren-unify α t* s* as*) 'fail))))))

  (define (μkanren-retry e s as)
    (match/first e
      ((= ,u ,v) (let ((u* (μkanren-hnorm u s)) (v* (μkanren-hnorm v s)))
                   (cond
                     ((and (μkanren-var-working? u*) (not (μkanren-var? v*))) (μkanren-retry/bind u* v* e s as))
                     ((and (μkanren-var-working? v*) (not (μkanren-var? u*))) (μkanren-retry/bind v* u* e s as))
                     (else (or (μkanren-unify u v s as) 'fail)))))
      ((tag ,tag ,u) (match/first (μkanren-λ-tag u tag s)
                       (#f 'fail)
                       (,s* (list s* as))))
      ((absent ,d ,u) (case (μkanren-λ-absent d u s)
                        ((#f) 'fail)
                        ((holds) (list s as))
                        (else (list (if (μkanren-absent-entailed? d u s) s (μkanren-push-E s e)) as))))))

  (define (μkanren-wake s assocs)
    (let1 (E (reverse (μkanren-hh-E (μkanren-hh s))))
      (let loop ((E E) (s (μkanren-with-E s '())) (as assocs))
        (match/first E
          (() (list s as))
          ((,e . ,E*) (match/first (μkanren-retry e s as)
                        (fail #f)
                        ((,s* ,as*) (loop E* s* as*))))))))

  ; the bindings `new` made by `=°` are checked for scope (when levels exist), then the suspended
  ; constraints are retried, as long as that binds something more. A variable of the highest level reached
  ; sees every eigenvariable and variable there is, so its bindings need no check (those made below a binder
  ; of the unifier were checked there, by `μkanren-scoped`).
  (define (μkanren-settle s new assocs #!optional (woken #f))
    (let1 (r (if (and (pair? new) (or woken (μkanren-levels? s)))
                 (let1 (top (μkanren-hh-top (μkanren-hh s)))
                   (μkanren-enforce s (remove (λ (p) (<= top (μkanren-level (car p) s))) new) assocs))
                 (list s assocs)))
      (and r
           (let ((s1 (car r)) (as1 (cadr r)))
             (if (or (null? (μkanren-hh-E (μkanren-hh s1))) (and (null? new) (eq? as1 assocs)))
                 r
                 (match/first (μkanren-wake s1 as1)
                   (#f #f)
                   ((,s2 ,as2) (if (eq? as2 as1) (list s2 as2) (μkanren-settle s2 (μkanren-new as2 as1) as2 #t)))))))))

  ;;; ==================== DISEQUALITY ====================

  ; whether `u` holds a λ-term or an eigenvariable, walked in `s`.
  (define (μkanren-has-term? u s)
    (let H ((u u))
      (cond
        ((μkanren-var? u) (let1 (u* (μkanren-state-find u s)) (and (not (μkanren-var? u*)) (H u*))))
        ((pair? u) (or (H (car u)) (H (cdr u))))
        ((μkanren-λ-term? u) #t)
        ((vector? u) (any H (vector->list u)))
        ((μkanren-other-record? u) (any H (cdr (vector->list (record->vector u)))))
        (else #f))))

  ; how `≠°` reads an eigenvariable: `'generic`, λProlog's GENERIC, a new name distinct from every other term
  ; (`∀x. q ≠ x` holds, as `pi x\ not (q = x)` succeeds), or `'extensional`, any term of its universe
  ; (`∀x. q ≠ x` fails, x could be q). A disequality keeps the reading in force when it was posted.
  (define μkanren-λ-reading (make-parameter 'generic))

  ; `u`, `v` with their eigenvariables replaced by new variables of the same level, for the extensional
  ; reading ¬∃x̄. u = v; a variable Y that could see some x of x̄ may depend on it, so it is first raised for
  ; the trial, Y := Y' x̄|Y. Answers `(values u* v* s* Ys)`, Ys the raised Y', which stand for the old ones.
  (define (μkanren-instances u v s)
    (let* ((u* (μkanren-state-find/value u s)) (v* (μkanren-state-find/value v s))
           (es (filter (λ (e) (finite? (μkanren-eigen-level e))) (μkanren-eigens (list u* v*)))))
      (if (null? es)
          (values u v s '())
          (let* ((ℓ0 (apply min (map μkanren-eigen-level es)))
                 (Ys (delete-duplicates (filter (λ (α) (and (μkanren-var-working? α) (<= ℓ0 (μkanren-level α s)))) (μkanren-vars (list u* v*)))
                                        μkanren-var=?)))
            (let raise ((Ys Ys) (s s) (Ys* '()))
              (if (pair? Ys)
                  (receive (Y* s) (μkanren-fresh s (μkanren-level (car Ys) s))
                    (let1 (seen (filter (λ (e) (<= (μkanren-eigen-level e) (μkanren-level (car Ys) s))) es))
                      (raise (cdr Ys) (car (μkanren-unify (car Ys) (make-μkanren-app Y* seen) s '())) (cons Y* Ys*))))
                  (let loop ((es es) (u (μkanren-state-find/value u s)) (v (μkanren-state-find/value v s)) (s s))
                    (if (null? es)
                        (values u v s Ys*)
                        (receive (Z s) (μkanren-fresh s (μkanren-eigen-level (car es)))
                          (loop (cdr es)
                                (μkanren-instantiate (μkanren-close u (car es)) Z)
                                (μkanren-instantiate (μkanren-close v (car es)) Z)
                                s))))))))))

  ; the trial unification of a disequality `u ≠ v`: `'unsat` when u = v has no unifier (the disequality holds
  ; for good), `'entailed` when it holds without binding any variable older than the trial and without
  ; suspending anything (the disequality is violated), else the bindings of older variables, in the trial
  ; state. The variables a trial creates by pruning or flex-flex, or for the eigenvariables of an extensional
  ; disequality, are existential inside the negation, so they are told apart by their index, not stored as
  ; they would be in a prefix of D; a trial that binds older variables only to distinct such variables,
  ; unbound, is entailed too.
  (define (μkanren-trial u v s #!optional (reading 'generic))
    (let ((vc (μkanren-state-vars-count s)) (E0 (μkanren-hh-E (μkanren-hh s))))
      (receive (u v s Ys) (if (eq? reading 'extensional) (μkanren-instances u v s) (values u v s '()))
        (match/first (μkanren-unify u v s '())
          (#f 'unsat)
          ((,s1 ,as1) (match/first (if (and (pair? as1) (μkanren-levels? s1)) (μkanren-enforce s1 (μkanren-new as1 '()) as1) (list s1 as1))
                        (#f 'unsat)
                        (((,s* _) ⊣ (any (λ (a) (let1 (h (μkanren-hnorm (car a) s*))
                                                  (and (μkanren-λ-term? h) (not (μkanren-flex? h)) (not (μkanren-tag-pred? (cdr a) h)))))
                                         (μkanren-state-A s*)))
                          ; a type of A that holds of no rigid λ-term nor eigenvariable it is bound to: u = v contradicts it.
                          'unsat)
                        ((,s* ,as*) (let* ((Y? (λ (α) (any (λ (Y) (μkanren-var=? α Y)) Ys)))
                                           (old (filter (λ (p) (or (< (μkanren-var-index (car p)) vc) (Y? (car p)))) as*))
                                           (ends (map (λ (p) (μkanren-state-find (car p) s*)) old))
                                           (absorbed (every (λ (Z) (and (μkanren-var-working? Z) (<= vc (μkanren-var-index Z)) (not (Y? Z)))) ends)))
                                      (if (and absorbed (= (length ends) (length (delete-duplicates ends μkanren-var=?)))
                                               (eq? (μkanren-hh-E (μkanren-hh s*)) E0))
                                          'entailed
                                          (list old s*))))))))))

  ; N holds `(reading u . v)`. Every disequality of N is tried again: dropped when it holds for good, a
  ; failure when violated.
  (define (μkanren-verify-N s)
    (let loop ((N (μkanren-hh-N (μkanren-hh s))) (N* '()) (dropped #f))
      (match/first N
        (() (if dropped (μkanren-with s (μkanren-hh-with (μkanren-hh s) N: (reverse N*))) s))
        (((,r ,u . ,v) . ,N**) (match/first (μkanren-trial u v s r)
                                 (unsat (loop N** N* #t))
                                 (entailed #f)
                                 (_ (loop N** (cons (car N) N*) dropped)))))))

  ; a first-order disequality posted under the extensional reading once levels exist is kept in N too, with
  ; its reading: in D, micro would let an absento of T absorb it, and absence reads an eigenvariable as GENERIC
  ; does, so `Y ≠ a ∧ absent a Y ∧ Y = x` would hold in some orders only. So does a disequality that micro
  ; derives from an absento (`absento`), whatever the reading in force.
  (define (μkanren-λ-diseq u v s #!optional absento)
    (and (or (μkanren-has-term? u s) (μkanren-has-term? v s) (and (μkanren-levels? s) (eq? (μkanren-λ-reading) 'extensional)))
         (let1 (r (if absento 'generic (μkanren-λ-reading)))
           (match/first (μkanren-trial u v s r)
             (unsat s)
             (entailed 'fail)
             (_ (μkanren-with s (μkanren-hh-with (μkanren-hh s) N: (cons `(,r ,u . ,v) (μkanren-hh-N (μkanren-hh s))))))))))

  ;;; ==================== AFTER A UNIFICATION ====================

  ; whether the new bindings `as` may hold a λ-term or an eigenvariable, walked in `s`: `#f` only when they
  ; hold none, looking at a few nodes only, since first-order search often binds long ground data at each
  ; step (the tails of append°); past them `'unknown`, and the migration looks at the prefixes they reach.
  (define (μkanren-new-terms? as s)
    (let1 (n 64)
      (call-with-current-continuation
        (λ (k)
          (any (λ (p) (let H ((u (cdr p)))
                        (set! n (sub1 n))
                        (cond
                          ((negative? n) (k 'unknown))
                          ((μkanren-var? u) (let1 (u* (μkanren-state-find u s)) (and (not (μkanren-var? u*)) (H u*))))
                          ((pair? u) (or (H (car u)) (H (cdr u))))
                          ((μkanren-λ-term? u) #t)
                          ((vector? u) (any H (vector->list u)))
                          ((μkanren-other-record? u) (any H (cdr (vector->list (record->vector u)))))
                          (else #f))))
               as)))))

  ; the prefix d of D as the tuple disequality it stands for in N, read as disequalities posted now are.
  (define (μkanren-prefix->N d) `(,(μkanren-λ-reading) ,(map car d) . ,(map cdr d)))

  ; `Ys` with the unbound variables that the term u reaches, walked in `s`, or `(k #t)` as soon as it holds a
  ; λ-term or an eigenvariable.
  (define (μkanren-reach u Ys s k)
    (let H ((u u) (Ys Ys))
      (cond
        ((μkanren-var? u) (let1 (u* (μkanren-state-find u s))
                            (cond
                              ((not (μkanren-var? u*)) (H u* Ys))
                              ((μkanren-var-working? u*) (cons u* Ys))
                              (else Ys))))
        ((pair? u) (H (cdr u) (H (car u) Ys)))
        ((μkanren-λ-term? u) (k #t))
        ((vector? u) (fold H Ys (vector->list u)))
        ((μkanren-other-record? u) (fold H Ys (cdr (vector->list (record->vector u)))))
        (else Ys))))

  ; `#t` when the term u holds a λ-term or an eigenvariable, walked in `s`, else the unbound variables it reaches.
  (define (μkanren-term-reach u s) (call-with-current-continuation (λ (k) (μkanren-reach u '() s k))))

  ; `#t` when the prefix d of D holds a λ-term or an eigenvariable, walked in `s`, else the unbound variables
  ; it reaches that the records of micro do not list, below the left and right sides of its pairs (the records
  ; list those sides that are variables).
  (define (μkanren-prefix-reach d s)
    (call-with-current-continuation
      (λ (k)
        (let1 (side (λ (u Ys) (let1 (u* (μkanren-state-find u s)) (if (μkanren-var? u*) Ys (μkanren-reach u* Ys s k)))))
          (fold (λ (p Ys) (side (cdr p) (side (car p) Ys))) '() d)))))

  ; `W` where each of the variables `Ys` lists the ids `is` too, which a variable that lists none shares.
  (define (μkanren-W+ W Ys is)
    (fold (λ (Y W) (let* ((j (μkanren-var-index Y)) (js (sbral-ref/back W j '())))
                     (if (null? js)
                         (update/sbral/back j is '() W)
                         (let1 (js* (fold (λ (i js) (if (memv i js) js (cons i js))) js is))
                           (if (eq? js* js) W (update/sbral/back j js* '() W))))))
          W Ys))

  ; micro's `watch` hook: the variables below the right sides of the new prefix d with the id i, which are
  ; unbound (I2) when its right sides are, go to `W`.
  (define (μkanren-λ-watch d i s)
    (if (every (λ (p) (let1 (u (cdr p)) (not (or (pair? u) (vector? u) (μkanren-other-record? u))))) d)
        s
        (let1 (Ys (μkanren-prefix-reach d s))
          (if (or (eq? Ys #t) (null? Ys))
              s
              (let1 (x (μkanren-hh s)) (μkanren-with s (μkanren-hh-with x W: (μkanren-W+ (μkanren-hh-W x) Ys (list i)))))))))

  ; the prefixes of D that now involve λ-terms or eigenvariables, anywhere below their variables, move to N
  ; as the tuple disequality they stand for, before micro hands D on: D never holds one, so micro's plain
  ; trial never meets a λ-term nor an eigenvariable. (A type of A on a variable just bound to a λ-term goes
  ; to `μkanren-λ-tag`, micro's `tag` hook, when micro hands the type on.)
  ; Since D held none before, only a prefix that reaches a variable just bound in `assocs` can hold one now:
  ; one that micro lists under it, as a side of a pair, or one that `W` lists under it. The first are looked
  ; at only when `terms?`, the bindings maybe holding a λ-term, since micro posts them again right after,
  ; telling `μkanren-λ-watch` of them. Every binding is in `assocs` (E2), so such a prefix holds one now
  ; exactly when the value of a binding that reaches it does: that value alone is walked, once for all the
  ; prefixes listed under its variable, never the prefixes themselves, which a recursion that extends the
  ; open tail of a prefix one pair at a time would walk again at each step. When the value holds none, the
  ; prefixes that `W` lists and that are not dropped stay in D and now reach its variables, which go to `W`
  ; under their ids. When it holds one, each prefix is walked once, to move: the ids in `W` of a state indexed
  ; again under new ids may be of other prefixes, or of dropped ones, which cost time, not answers. The
  ; prefixes move in the order of D, newest first, as `μkanren-migrate/D` moves them.
  (define (μkanren-migrate s assocs terms?)
    (if (and (not terms?) (null? (μkanren-hh-W (μkanren-hh s)))) s (μkanren-migrate/ids s assocs terms?)))

  (define (μkanren-migrate/ids s assocs terms?)
    (let1 (x (μkanren-hh s))
      (let loop ((as assocs) (W (μkanren-hh-W x)) (is '()))
        (match/first as
          (() (μkanren-migrate/moves s x W (fold-right (λ (i is) (if (and (pair? is) (= i (car is))) is (cons i is))) '() (sort is >))))
          ((,p . ,as*) (let* ((α (car p))
                              (listed (if terms? (μkanren-state-prefix-ids α s) '()))
                              (watched (sbral-ref/back (μkanren-hh-W x) (μkanren-var-index α) '())))
                         (if (and (null? listed) (null? watched))
                             (loop as* W is)
                             (let1 (r (μkanren-term-reach (cdr p) s))
                               (cond
                                 ((eq? r #t) (loop as* W (append listed watched is)))
                                 ((or (null? r) (null? watched)) (loop as* W is))
                                 (else (let1 (live (filter (λ (i) (μkanren-state-prefix i s)) watched))
                                         (loop as* (if (null? live) W (μkanren-W+ W r live)) is))))))))))))

  ; the prefixes with the ids `is`, newest first, that hold a λ-term or an eigenvariable move to N.
  (define (μkanren-migrate/moves s x W is)
    (let loop ((is is) (s s) (W W) (moved '()))
      (match/first is
        (() (if (and (null? moved) (eq? W (μkanren-hh-W x)))
                s
                (μkanren-with s (μkanren-hh-with x W: W N: (append (reverse moved) (μkanren-hh-N x))))))
        ((,i . ,is*) (let* ((d (μkanren-state-prefix i s))
                            (r (and d (μkanren-prefix-reach d s))))
                       (cond
                         ((not d) (loop is* s W moved))
                         ((eq? r #t) (loop is* (μkanren-state-drop-prefix i s) W (cons (μkanren-prefix->N d) moved)))
                         (else (loop is* s (μkanren-W+ W r (list i)) moved))))))))

  ; the same over the whole of D, for a state whose stores are given as lists, which micro indexes again
  ; right after the hook.
  (define (μkanren-migrate/D s)
    (let* ((D (μkanren-state-D s))
           (moves? (λ (d) (μkanren-has-term? d s))))
      (if (any moves? D)
          (let1 (x (μkanren-hh s))
            (μkanren-with (μkanren-state-remove-D moves? s)
                          (μkanren-hh-with x N: (append (map μkanren-prefix->N (filter moves? D)) (μkanren-hh-N x)))))
          s)))

  ; `μkanren-state-no-D?` spares every look at D and at `W` when D never held a prefix; when the bindings hold
  ; no λ-term, as `μkanren-new-terms?` tells, only the values bound to the variables that `W` lists are walked,
  ; and none when `W` is empty. A state whose stores are lists has no records: D is walked whole, as micro
  ; indexes it again right after the hook.
  (define (μkanren-λ-post s assocs)
    (match/first (μkanren-settle s assocs assocs)
      (#f #f)
      ((,s1 ,as1) (let* ((s2 (cond
                               ((or (null? as1) (μkanren-state-no-D? s1)) s1)
                               ((μkanren-state-indexed? s1) (μkanren-migrate s1 as1 (μkanren-new-terms? as1 s1)))
                               ((or (not (μkanren-new-terms? as1 s1)) (null? (μkanren-state-D s1))) s1)
                               (else (μkanren-migrate/D s1))))
                         (s3 (and s2 (if (null? (μkanren-hh-N (μkanren-hh s2))) s2 (μkanren-verify-N s2)))))
                    (and s3 (list s3 as1))))))

  ;;; ==================== ABSENT° ====================

  ; the parts of a λ-term in which `absent°` looks for its datum, or `'stuck` for a flexible term, which is
  ; watched whole. Below a binder the parts that mention it are kept closed, `λx.(a . b)` being looked into
  ; as `λx.a` and `λx.b`, so a watched term never has a loose bound variable.
  ; When `projecting` an answer's absento (`s` reified), a flexible term whose head is no variable of the
  ; answer has no part left to watch, since that head may be a constant function.
  (define (μkanren-λ-parts u s #!optional projecting)
    (define (flex? t) (and (μkanren-app? t) (if projecting (μkanren-var? (μkanren-app-head t)) (μkanren-flex? t))))
    (define (stuck t) (if (and projecting (μkanren-var-working? (μkanren-app-head t))) '() 'stuck))
    (let1 (h (μkanren-hnorm u s))
      (cond
        ((flex? h) (stuck h))
        ((or (μkanren-eigen? h) (μkanren-bvar? h)) '())
        ((μkanren-app? h) (cons (μkanren-app-head h) (μkanren-app-args h)))
        ((μkanren-lam? h)
          (let1 (b (μkanren-hnorm (μkanren-lam-body h) s))
            (cond
              ((flex? b) (stuck b))
              ((μkanren-bvar? b) '())
              ((not (μkanren-free0? b)) (list (μkanren-shift b -1 1)))
              (else (let1 (ps (cond
                                ((μkanren-lam? b) (μkanren-λ-parts b s projecting))
                                ((μkanren-app? b) (cons (μkanren-app-head b) (μkanren-app-args b)))
                                ((pair? b) (list (car b) (cdr b)))
                                ((vector? b) (vector->list b))
                                ((μkanren-other-record? b) (cdr (vector->list (record->vector b))))
                                (else (list b))))
                      (if (eq? ps 'stuck)
                          'stuck
                          (map (λ (p) (if (μkanren-free0? p) (μkanren-lam/η (μkanren-lam-hint h) p) (μkanren-shift p -1 1))) ps)))))))
        (else (list h)))))

  ;;; ==================== REIFICATION ====================

  ; the deep value of a λ-term: β-normal, and η-short like the terms built.
  (define (μkanren-λ-value u A s)
    (cond
      ((μkanren-lam? u) (let1 (b (A (μkanren-lam-body u))) (if (μkanren-η-redex? b μkanren-free0?) (μkanren-η b) (μkanren-lam/body u b))))
      ((μkanren-app? u) (let ((h (A (μkanren-app-head u))) (args (map A (μkanren-app-args u))))
                          (cond
                            ((μkanren-lam? h) (A (μkanren-beta h args)))
                            ((μkanren-app? h) (make-μkanren-app (μkanren-app-head h) (append (μkanren-app-args h) args)))
                            (else (make-μkanren-app h args)))))
      (else u)))

  ; a binder's occurrence, or an eigenvariable's, while printing.
  (define-record μkanren-name symbol)

  (define (μkanren-symbols v)
    (let S ((v v) (acc '()))
      (cond
        ((symbol? v) (cons v acc))
        ((μkanren-var? v) (cons (μkanren-var->symbol v) acc))
        ((μkanren-name? v) (cons (μkanren-name-symbol v) acc))
        ((pair? v) (S (car v) (S (cdr v) acc)))
        ((vector? v) (fold S acc (vector->list v)))
        ((μkanren-lam? v) (S (μkanren-lam-body v) acc))
        ((μkanren-app? v) (fold S acc (cons (μkanren-app-head v) (μkanren-app-args v))))
        ((μkanren-other-record? v) (fold S acc (cdr (vector->list (record->vector v)))))
        (else acc))))

  ; a name based on `base`, not among `taken`.
  (define (μkanren-name* base taken)
    (let1 (b (symbol->string base))
      (let loop ((i 0))
        (let1 (n (if (zero? i) base (string->symbol (string-append b (number->string i)))))
          (if (memq n taken) (loop (add1 i)) n)))))

  ; the rebuilding expression of the deep value `v`: `(ƛ (x y) body)` for nested abstractions, merged, and
  ; `(@ h a ...)` for an application; the rest as micro prints it. A binder is named after its hint,
  ; renamed apart from the binders around it, from every symbol of its body and from the operators of the
  ; expression itself, so that neither capture nor shadowing can mislead the reader (Ma–Friedman's
  ; "true = false") nor change what a ground answer evaluates to.
  (define μkanren-print-operators '(cons quote vector make-record-instance ƛ @ eigen bvar))

  (define (μkanren-print v scope)
    (let R ((v v) (scope scope))
      (cond
        ((μkanren-lam? v)
          (let1 (taken (append scope μkanren-print-operators (μkanren-symbols v)))
            (let loop ((t v) (names '()))
              (if (μkanren-lam? t)
                  (let1 (x (μkanren-name* (μkanren-lam-hint t) (append names taken)))
                    (loop (μkanren-instantiate (μkanren-lam-body t) (make-μkanren-name x)) (cons x names)))
                  `(ƛ ,(reverse names) ,(R t (append names scope)))))))
        ((μkanren-app? v) `(@ ,@(map (λ (w) (R w scope)) (cons (μkanren-app-head v) (μkanren-app-args v)))))
        ((μkanren-name? v) (μkanren-name-symbol v))
        ((μkanren-eigen? v) `(eigen ,(μkanren-eigen-name v)))
        ((μkanren-bvar? v) `(bvar ,(μkanren-bvar-index v)))
        ((μkanren-var? v) (μkanren-var->symbol v))
        ((symbol? v) `',v)
        ((null? v) ''())
        ((pair? v) `(cons ,(R (car v) scope) ,(R (cdr v) scope)))
        ((vector? v) `(vector ,@(map (λ (w) (R w scope)) (vector->list v))))
        ((record-instance? v) `(make-record-instance ,@(map (λ (w) (R w scope)) (vector->list (record->vector v)))))
        (else v))))

  ; every constraint that holds an eigenvariable past its scope is closed, by ƛ or ∀, so only the answer
  ; itself can hold one, when the variable it reifies is created inside a `∀°`: since λProlog reports no
  ; binding for a variable quantified inside `pi x\`, nor can this answer be told without x.
  (define (μkanren-λ-repr u A s)
    (let1 (v (μkanren-state-find/value u s))
      (match/first (filter (λ (e) (finite? (μkanren-eigen-level e))) (μkanren-eigens v))
        (() (μkanren-print v '()))
        ((,e . _) (error '°->list "the answer holds the eigenvariable of a ∀° it was created in: create the variable it reifies outside every ∀°"
                         (μkanren-eigen-name e))))))

  ; a rebuilding expression as data: `(cons 'a (cons x '()))` is `(a x)`, binders and applications stay.
  (define (μkanren-repr->datum r)
    (let D ((r r))
      (cond
        ((not (pair? r)) r)
        ((eq? (car r) 'quote) (cadr r))
        ((eq? (car r) 'cons) (cons (D (cadr r)) (D (caddr r))))
        ((eq? (car r) 'vector) (list->vector (map D (cdr r))))
        ((eq? (car r) 'ƛ) `(ƛ ,(cadr r) ,(D (caddr r))))
        ((eq? (car r) '@) `(@ ,@(map D (cdr r))))
        (else (map D r)))))

  ; a term as data, printed without a state.
  (define (μkanren-datum t) (μkanren-repr->datum (μkanren-print t '())))

  (set-record-printer! μkanren-lam (λ (t port) (write (μkanren-datum t) port)))
  (set-record-printer! μkanren-app (λ (t port) (write (μkanren-datum t) port)))
  (set-record-printer! μkanren-eigen (λ (t port) (write `(eigen ,(μkanren-eigen-name t)) port)))

  ; the eigenvariables in `t`, outermost first.
  (define (μkanren-eigens t)
    (reverse
      (let L ((t t) (acc '()))
        (cond
          ((μkanren-eigen? t) (if (any (λ (e) (μkanren-eigen=? e t)) acc) acc (cons t acc)))
          ((μkanren-lam? t) (L (μkanren-lam-body t) acc))
          ((μkanren-app? t) (fold L (L (μkanren-app-head t) acc) (μkanren-app-args t)))
          ((pair? t) (L (cdr t) (L (car t) acc)))
          ((vector? t) (fold L acc (vector->list t)))
          ((μkanren-other-record? t) (fold L acc (cdr (vector->list (record->vector t)))))
          (else acc)))))

  ; `ts` printed in `s*` with their eigenvariables named apart, as `(reprs names)`.
  (define (μkanren-render ts s*)
    (let* ((vs (map (λ (t) (μkanren-state-find/value t s*)) ts))
           (es (μkanren-eigens vs))
           (taken (μkanren-symbols vs))
           (names (fold (λ (e ns) (cons (μkanren-name* (μkanren-eigen-name e) (append ns taken)) ns)) '() es))
           (names (reverse names))
           (vs* (fold (λ (e n vs) (map (λ (v) (μkanren-instantiate (μkanren-close v e) (make-μkanren-name n))) vs)) vs es names)))
      (list (map (λ (v) (μkanren-print v names)) vs*) names)))

  (define (μkanren-closed-form names form) (if (null? names) form `(∀ ,names ,form)))

  ; the terms of the suspended equations, whose variables an answer names; a type or an absento names none,
  ; as micro's T does not: its term is printed only when its head is in the answer anyway.
  (define (μkanren-λ-terms s)
    (append-map (λ (e) (match/first e
                         ((= ,u ,v) (list u v))
                         (_ '())))
                (reverse (μkanren-hh-E (μkanren-hh s)))))

  ; the variable at the head of the term `u` that a type or an absento of E watches.
  (define (μkanren-watched-head u s)
    (let1 (h (μkanren-hnorm u s))
      (cond
        ((μkanren-var-working? h) h)
        ((μkanren-flex? h) (μkanren-app-head h))
        (else #f))))

  ; the types and absentos of E that an answer drops: their term's head is out of the answer `s*`, so it may
  ; be a constant function, `λz̄.k` for a k of the type and none of the data, as micro's projection of T
  ; drops such an absento; unless two different types wait on that head, which no constant satisfies.
  (define (μkanren-λ-dropped E s s*)
    (let* ((out (filter-map (λ (e) (match/first e
                                     ((= . _) #f)
                                     ((,_ ,_ ,u) (let1 (h (μkanren-watched-head u s))
                                                   (and h (μkanren-var-working? (μkanren-state-find h s*)) (cons h e))))))
                            E))
           (clash? (λ (h) (let1 (tags (filter-map (λ1-match/first ((,h* tag ,tag ,_) (and (μkanren-var=? h h*) tag)) (_ #f)) out))
                            (any (λ (tag) (not (μkanren-tag-equal? tag (car tags)))) tags)))))
      (filter-map (λ1-match/first ((,h . ,e) (and (not (clash? h)) e))) out)))

  ; the printed form `f` up to α, as a key for `equal?`: the names bound by `ƛ`, `∀` and `∃` become their
  ; positions in scope, any other symbol stays itself, quoted data is left alone, and every node is tagged,
  ; so that two forms have the same key exactly when they differ only in the names of their binders.
  (define (μkanren-form-α-key f)
    (let K ((f f) (scope '()))
      (cond
        ((symbol? f) (let1 (i (list-index (λ (x) (eq? x f)) scope)) (if i (cons 'bound i) (cons 'free f))))
        ((and (pair? f) (eq? (car f) 'quote)) (cons 'quote f))
        ((and (pair? f) (memq (car f) '(ƛ ∀ ∃)) (list? f) (= 3 (length f)) (list? (cadr f)) (every symbol? (cadr f)))
          (list (car f) (length (cadr f)) (K (caddr f) (append (reverse (cadr f)) scope))))
        ((pair? f) (cons* 'pair (K (car f) scope) (K (cdr f) scope)))
        (else (cons 'atom f)))))

  ; `forms`, printed already, sorted as micro sorts D and T: by `->string`, ties by the written form, which
  ; tells 1 from "1"; so that an answer does not depend on the order in which E and N were filled. A form
  ; that says what an earlier one says, up to the names of its binders, is dropped: two disequalities of N
  ; that became the same one, or the same one posted with two binder hints.
  (define (μkanren-sort-forms forms)
    (let* ((decorated (map (λ (f) (cons* (->string f) (->string/write f) f)) forms))
           (sorted (sort decorated (λ (a b) (or (string<? (car a) (car b))
                                                (and (string=? (car a) (car b)) (string<=? (cadr a) (cadr b))))))))
      (let loop ((l (map cddr sorted)) (keys '()) (kept '()))
        (match/first l
          (() (reverse kept))
          ((,f . ,l*) (let1 (k (μkanren-form-α-key f))
                        (if (member k keys) (loop l* keys kept) (loop l* (cons k keys) (cons f kept)))))))))

  ; the suspended constraints, as `(assert (≐ u v))` & co., closed with `∀` over the eigenvariables that
  ; outlived their scope; then the disequalities of N as micro prints D: their trial mgu on the older
  ; variables, `∃` over the variables the trial creates, or the equation itself when the trial only suspends.
  ; Each group is sorted, see `μkanren-sort-forms`.
  (define (μkanren-λ-forms s s*)
    (let* ((x (μkanren-hh s))
           (vc (μkanren-state-vars-count s))
           (E (reverse (μkanren-hh-E x)))
           (E (let1 (dropped (μkanren-λ-dropped E s s*)) (remove (λ (e) (memq e dropped)) E)))
           (E (map (λ (e)
                     (match/first e
                       ((= ,u ,v) (match1/first (((,ru ,rv) ,ns) (μkanren-render (list u v) s*)) (μkanren-closed-form ns `(assert (≐ ,ru ,rv)))))
                       ((tag ,tag ,u) (match1/first (((,ru) ,ns) (μkanren-render (list u) s*))
                                        (μkanren-closed-form ns `(assert (every ,(μkanren-tag-def tag) (list ,ru))))))
                       ((absent ,d ,u) (match1/first (((,rd ,ru) ,ns) (μkanren-render (list d u) s*))
                                         (μkanren-closed-form ns `(assert (absent? ,rd ,ru)))))))
                   E))
           (unreachable? (λ (α) (and (< (μkanren-var-index α) vc) (μkanren-var-working? (μkanren-state-find α s*)))))
           (vars-of μkanren-vars)
           ; the variables that occur strictly, outside every argument of a flexible term: micro drops a
           ; disequality over an unreachable variable, which can always be chosen to keep its sides apart, but
           ; one in an argument of `F` cannot when F is a constant function, `∃Y. G ≠ F Y` being `F ≠ λz.G`.
           (strict-vars-of (λ (v) (let V ((v v)) (cond
                                                   ((μkanren-var? v) (list v))
                                                   ((pair? v) (append (V (car v)) (V (cdr v))))
                                                   ((μkanren-lam? v) (V (μkanren-lam-body v)))
                                                   ((μkanren-app? v) (if (μkanren-var? (μkanren-app-head v))
                                                                         (list (μkanren-app-head v))
                                                                         (append-map V (cons (μkanren-app-head v) (μkanren-app-args v)))))
                                                   ((vector? v) (append-map V (vector->list v)))
                                                   ((μkanren-other-record? v) (append-map V (cdr (vector->list (record->vector v)))))
                                                   (else '())))))
           (N (filter-map
                (λ (n)
                  (match1/first ((,r ,u . ,v) n)
                    (match/first (μkanren-trial u v s r)
                      ((,old ,s1)
                        (let* ((rhs (map (λ (q) (μkanren-state-find/value (cdr q) s1)) old))
                               (vars (append (append-map vars-of rhs)
                                             (if (null? old) (append-map vars-of (list (μkanren-state-find/value u s) (μkanren-state-find/value v s))) '())))
                               (strict (append (append-map strict-vars-of rhs)
                                               (if (null? old) (append-map strict-vars-of (list (μkanren-state-find/value u s) (μkanren-state-find/value v s))) '())))
                               (fresh (remove-duplicates (map μkanren-var->symbol (filter (λ (α) (<= vc (μkanren-var-index α))) vars)))))
                          (and (not (any unreachable? (map car old)))
                               (not (any unreachable? (filter (λ (α) (< (μkanren-var-index α) vc)) strict)))
                               (if (null? old)
                                   ; only suspended: the equation it denies, closed over the eigenvariables of an extensional one.
                                   (match1/first (((,ru ,rv) ,ns) (μkanren-render (list u v) s*))
                                     (μkanren-closed-form ns `(begin (deny (≐ ,ru ,rv)))))
                                   (let* ((eqs (map (λ (q v) `(equal? ,(μkanren-print (μkanren-state-find/value (car q) s*) '())
                                                                       ,(μkanren-print (μkanren-state-find/value v s*) '())))
                                                    old rhs))
                                          (e (if (= 1 (length eqs)) (car eqs) `(and ,@eqs))))
                                     `(begin (deny ,(if (null? fresh) e `(∃ ,fresh ,e)))))))))
                      (else #f))))
                (reverse (μkanren-hh-N x)))))
      (append (μkanren-sort-forms E) (μkanren-sort-forms N))))

  (define μkanren-λ-environment (τ (module-environment 'aux.kanren.lambda)))

  (define μkanren-λ-extension
    (make-μkanren-extension 'λProlog
      term?: μkanren-λ-term? atom?: (λ (u) (or (μkanren-eigen? u) (μkanren-bvar? u))) unify: μkanren-λ-unify occurs: μkanren-λ-occurs post: μkanren-λ-post diseq: μkanren-λ-diseq
      watch: μkanren-λ-watch
      tag: μkanren-λ-tag parts: μkanren-λ-parts value: μkanren-λ-value repr: μkanren-λ-repr
      terms: μkanren-λ-terms forms: μkanren-λ-forms environment: μkanren-λ-environment))

  (μkanren-extension-register! μkanren-λ-extension)

  ; the answers of `g` as data: `(ƛ (x) (g x))` rather than records, constraints dropped as by `°->list/ground`.
  (define (°->list/datum g) (map (λ (a) (μkanren-repr->datum (last a))) (°->list #f g)))

  ; how (aux kanren micro show) reads the answers: `(ƛ (x ...) body)` and `(@ h a ...)` stay as they are
  ; printed, as by `°->list/datum`, and the cells view draws them as nodes ⟦ƛ x ...⟧, with a node per
  ; binder, and ⟦@⟧; `(assert (≐ u v))` is the suspended equation `(= u v)`, `(begin (deny (≐ u v)))` its
  ; denial `(≠ u v)`, a disequality denied under `∃ x̄` is `(∀ x̄ (≠ ...))`, and `(∀ (c ...) form)` closes
  ; a constraint over the eigenvariables that outlived their scope.
  (define μkanren-λ-show-extension
    (make-μkanren-show-extension 'λProlog
      datum: (λ (r D) (match/first r
                        ((ƛ ,xs ,body) `(ƛ ,xs ,(D body)))
                        ((@ . ,rs) `(@ ,@(map D rs)))
                        (_ #f)))
      constraint: (λ (c C R) (match/first c
                               ((assert (≐ ,u ,v)) `(= ,(R u) ,(R v)))
                               ((begin (deny (≐ ,u ,v))) `(≠ ,(R u) ,(R v)))
                               ((begin (deny (∃ ,xs ,e))) `(∀ ,xs ,(C `(begin (deny ,e)))))   ; ¬∃x̄.e is ∀x̄.¬e
                               ((∀ ,xs ,c*) `(∀ ,xs ,(C c*)))
                               (_ #f)))
      cell: (λ (v) (match/first v
                     (((ƛ ,xs ,body) ⊣ (and (list? xs) (every symbol? xs)))
                       `(,(conc "ƛ " (string-intersperse (map ->string xs) " ")) ,xs ("body" . ,body)))
                     (((@ ,h . ,as) ⊣ (list? as)) `("@" () ("head" . ,h) ,@(map (λ (j a) (cons (->string j) a)) (ι (length as) 1) as)))
                     (_ #f)))))

  (μkanren-show-extension-register! μkanren-λ-show-extension)

  ; α-βη-equality of closed terms, for the residual forms `(assert (≐ u v))` to evaluate.
  (define (≐ u v)
    (match/first (μkanren-state-unify u v μkanren-state-empty '())
      (#f #f)
      ((,s _) (eq? s μkanren-state-empty))))

  ;;; ==================== GENERIC: ∀ ====================

  ; leaving the scope of the eigenvariable c, back at depth d: a suspended equation or a disequality of N that
  ; mentions c is the constraint ∀c(u = v), resp. its negation under GENERIC's reading of c as a new name:
  ; every variable in it that could see c is raised, Y := Y' c with Y' of level d (Miller's raising, a
  ; bijection on solutions), and the constraint is closed into λc.u = λc.v, equivalent by ξ; the bindings
  ; are then checked and the stores verified as by `=°`. An absento of micro whose term mentions c is
  ; closed likewise, one whose datum is c moves to E, unless the levels say that its term can never hold c;
  ; the other suspended constraints keep c, and print closed with `∀`. The raise bindings make c appear in
  ; more constraints, an absento of T on a raised Y or a prefix of D that moves to N: they are discharged
  ; in turn, until no raise is left.
  (define (μkanren-discharge s c d)
    (let* ((x (μkanren-hh s))
           (mentions? (λ (t) (any (λ (e) (μkanren-eigen=? e c)) (μkanren-eigens (μkanren-state-find/value t s)))))
           (E (μkanren-hh-E x))
           (N (μkanren-hh-N x))
           (T (μkanren-state-T s))
           (E-eqs (filter (λ (e) (and (eq? (car e) '=) (mentions? (cdr e)))) E))
           (N-c (filter (λ (n) (and (eq? (car n) 'generic) (mentions? (cdr n)))) N))
           (T-c (filter (λ (t) (or (mentions? (car t)) (mentions? (μkanren-tag-name (cdr t))))) T)))
      (if (and (null? E-eqs) (null? N-c) (null? T-c))
          s
          (let loop ((Ys (delete-duplicates
                           (filter (λ (α) (and (μkanren-var-working? α) (<= (μkanren-eigen-level c) (μkanren-level α s))))
                                   (append-map (λ (t) (μkanren-vars (μkanren-state-find/value t s))) (append (map cdr E-eqs) (map cdr N-c))))
                           μkanren-var=?))
                     (s s) (as '()))
            (if (pair? Ys)
                (receive (Y* s) (μkanren-fresh s d)
                  (match/first (μkanren-unify (car Ys) (make-μkanren-app Y* (list c)) s as)
                    (#f #f)
                    ((,s ,as) (loop (cdr Ys) s as))))
                (let* ((close (λ (t) (μkanren-lam/η (μkanren-eigen-name c) (μkanren-close (μkanren-state-find/value t s) c))))
                       (E* (map (λ (e) (if (memq e E-eqs) `(= ,(close (cadr e)) ,(close (caddr e))) e)) E))
                       (N* (map (λ (n) (if (memq n N-c) `(generic ,(close (cadr n)) . ,(close (cddr n))) n)) N))
                       (moved (filter (λ (t) (mentions? (μkanren-tag-name (cdr t)))) T-c))
                       (T* (filter-map (λ (t) (cond
                                                ((memq t moved) #f)
                                                ((memq t T-c) (cons (close (car t)) (cdr t)))
                                                (else t)))
                                       T))
                       (E** (append (filter-map (λ (t) (let1 (d (μkanren-tag-name (cdr t)))
                                                         (and (not (μkanren-absent-entailed? d (car t) s)) `(absent ,d ,(car t)))))
                                                moved)
                                    E*))
                       ; T is given as a list only when it changes: micro indexes such a state again as a whole.
                       (x* (μkanren-hh-with (μkanren-hh s) E: E** N: N*))
                       (s (if (null? T-c) (μkanren-with s x*) (μkanren-state-with s T: T* X: x*)))
                       (s (if (null? N-c) s (μkanren-verify-N s))))
                  (cond
                    ((not s) #f)
                    ; a closed equation may be decided now, `F x = g F` being `F = λx.g F`.
                    ((null? as) (if (null? E-eqs)
                                    s
                                    (match/first (μkanren-wake s '())
                                      (#f #f)
                                      ((,s* ()) s*)
                                      ((,s* ,as*) (cond ((μkanren-post-unify s* as*) => (λ (s**) (μkanren-discharge s** c d))) (else #f))))))
                    ((μkanren-post-unify s as) => (λ (s*) (μkanren-discharge s* c d)))
                    (else #f))))))))

  (define (μkanren-vars v)
    (cond
      ((μkanren-var? v) (list v))
      ((pair? v) (append (μkanren-vars (car v)) (μkanren-vars (cdr v))))
      ((μkanren-lam? v) (μkanren-vars (μkanren-lam-body v)))
      ((μkanren-app? v) (append-map μkanren-vars (cons (μkanren-app-head v) (μkanren-app-args v))))
      ((vector? v) (append-map μkanren-vars (vector->list v)))
      ((μkanren-other-record? v) (append-map μkanren-vars (cdr (vector->list (record->vector v)))))
      (else '())))

  ; GENERIC (book p.98): prove `(f c)` for a new eigenvariable c one universe above the current one. The
  ; depth is raised for the body only; every answer state of the body gets it back (λProlog's universe
  ; register, Nadathur et al. 1995), the levels of the variables created inside being recorded first, and
  ; its constraints on c discharged.
  (define ((∀ª name f) s)
    (let* ((x  (μkanren-hh s))
           (d  (μkanren-hh-depth x))
           (x* (μkanren-hh-with (μkanren-hh-pad x (μkanren-state-vars-count s)) depth: (add1 d) top: (max (add1 d) (μkanren-hh-top x))))
           (c  (μkanren-eigen* name (add1 d))))
      (append-map§ (λ (s1)
                     (let1 (s2 (μkanren-with s1 (μkanren-hh-with (μkanren-hh-pad (μkanren-hh s1) (μkanren-state-vars-count s1)) depth: d)))
                       (match/first (μkanren-discharge s2 c d)
                         (#f (✗° s2))
                         (,s3 (✓° s3)))))
                   (δ ((f c) (μkanren-with s x*))))))

  (define-syntax ∀°
    (syntax-rules ()
      ((_ () g ...) (and° g ...))
      ((_ (x y ...) g ...) (∀ª 'x (λ (x) (∀° (y ...) g ...))))))

  ;;; ==================== AUGMENT: ⊃ ====================

  ; AUGMENT (book p.93-94): prove `g` with the clauses in front of the program, for the extent of g only.
  ; A clause is `(relation . procedure)`, the procedure taking the list of arguments to a goal: its free logic
  ; variables are those of the closure, shared with the context, its own ∀-variables are made by a `fresh°`
  ; inside, so renamed at each use (book p.102-104, Nadathur et al. 1995 p.14).
  (define ((⊃ª clauses g) s)
    (let* ((x (μkanren-hh s))
           (Γ (μkanren-hh-Γ x))
           (Γ* (fold-right (λ (c Γ*) (let1 (old (cond ((assq (car c) Γ*) => cdr) (else '())))
                                       (cons (cons (car c) (cons (cdr c) old)) (if (null? old) Γ* (alist-delete (car c) Γ* eq?)))))
                           Γ clauses)))
      (append-map§ (λ (s1) (✓° (μkanren-with s1 (μkanren-hh-with (μkanren-hh s1) Γ: Γ))))
                   (δ (g (μkanren-with s (μkanren-hh-with x Γ: Γ*)))))))

  ; `(⊃° (D ...) g ...)`, a clause D being a fact `(rel t ...)`, a rule `((rel t ...) ← g ...)` or
  ; `(← (rel t ...) g ...)`, or `(∀ (v ...) D)` for its own variables.
  (define-syntax ⊃°
    (syntax-rules ()
      ((_ (D ...) g ...) (⊃ª (list (μkanren-clause D) ...) (and° g ...)))))

  (define-syntax μkanren-clause
    (syntax-rules ()
      ((_ D) (μkanren-clause/vars () D))))

  (define-syntax μkanren-clause/vars
    (syntax-rules (∀ ←)
      ((_ (v ...) (∀ (w ...) D)) (μkanren-clause/vars (v ... w ...) D))
      ((_ (v ...) (← (rel t ...) g ...)) (μkanren-clause/vars (v ...) ((rel t ...) ← g ...)))
      ((_ (v ...) ((rel t ...) ← g ...)) (μkanren-clause* rel (λ (args) (fresh° (v ...) (=° args (list t ...)) g ...))))
      ((_ (v ...) (rel t ...)) (μkanren-clause* rel (λ (args) (fresh° (v ...) (=° args (list t ...))))))))

  ; only a dynamic relation takes hypotheses: assuming a clause for any other would be silently ignored.
  (define (μkanren-clause* rel clause)
    (if (μkanren-dynamic? rel) (cons rel clause) (error '⊃° "not a dynamic relation, see define-dynamic-relation" rel)))

  (define (μkanren-dynamic f) (extend-procedure f 'μkanren-dynamic))
  (define (μkanren-dynamic? f) (and (extended-procedure? f) (eq? (procedure-data f) 'μkanren-dynamic)))

  ; backchaining on the hypotheses about the relation `key`, most recent first, then on its static clauses
  ; `static` (a goal, or #f): one flat disjunction, so that when every clause answers at once the answers
  ; come in λProlog's order (p.94), interleaving deciding otherwise.
  (define ((μkanren-hyps° key args static) s)
    (let* ((gs (map (λ (c) (c args)) (cond ((assq key (μkanren-hh-Γ (μkanren-hh s))) => cdr) (else '()))))
           (gs (if static (append gs (list static)) gs)))
      (if (null? gs) '() ((reduce-right orª #f gs) s))))

  ; a relation that hypotheses can extend, whose static clauses are the goals `g ...`; with no `g` it has
  ; none (in λProlog every predicate is dynamic, here only those defined so).
  (define-syntax define-dynamic-relation
    (syntax-rules ()
      ((_ (name arg ...)) (define name (μkanren-dynamic (λ (arg ...) (λ (s) (δ ((μkanren-hyps° name (list arg ...) #f) s)))))))
      ((_ (name arg ...) g ...) (define name (μkanren-dynamic (λ (arg ...) (λ (s) (δ ((μkanren-hyps° name (list arg ...) (and° g ...)) s)))))))))

  )
