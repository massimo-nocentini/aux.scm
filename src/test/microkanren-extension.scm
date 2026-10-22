; The dispatch of the hooks of (aux kanren micro) to a registered extension. The registry is global and an
; extension cannot be taken back, so this suite runs in its own process: the other suites of micro register none.
; It also holds the case that writes the slot X, which the other suites must not: they run with (aux kanren
; lambda) loaded too, see `make test-microkanren-with-lambda`, and its extension owns X.

(import scheme (chicken base) srfi-1 (aux unittest) (aux base) (aux stream) (aux kanren micro))

; an extension with a `post` hook only: every binding to `(twin u v)` that =° makes unifies u with v too, as an
; extension wakes a constraint suspended on the variable bound. The bindings it adds must have their constraints
; handed on (E3), which `μkanren-post-unify` does after the hooks.
(define (twin-post s assocs)
  (fold (λ (a r)
          (let1 (v (rhs a))
            (if (and r (pair? v) (eq? (car v) 'twin)) (μkanren-unify (cadr v) (caddr v) (car r) (cadr r)) r)))
        (list s assocs)
        assocs))

; registered twice under the same name, the second replaces the first, which would fail every binding.
(μkanren-extension-register! (make-μkanren-extension 'twin post: (λ (s assocs) #f)))
(μkanren-extension-register! (make-μkanren-extension 'twin post: twin-post))

; an extension with a `watch` hook only, which records each prefix that D takes, as its id and its right sides.
(define told '())
(μkanren-extension-register! (make-μkanren-extension 'watcher watch: (λ (d i s) (set! told (cons (list i (map rhs d)) told)) s)))

(define-suite microkanren-extension-suite

  ((test/extension/post _)
    (⊦= '((λ () (cons 'twin (cons 'k (cons 'k '())))))
        (°->list #f (fresh° (q) (fresh° (y) (=° q (list 'twin y 'k))))))
    (⊦= '((λ () 1)) (°->list #f (fresh° (q) (=° q 1))))
    (⊦= '((λ () (cons 'twin (cons 'j (cons 'j '())))))
        (°->list #f (fresh° (q) (fresh° (y) (≠° y 'k) (symbol° y) (=° q (list 'twin y 'j))))))
    ; a binding by the hook that fails fails the =°.
    (⊦= '() (°->list #f (fresh° (q) (=° q (list 'twin 1 2))))))

  ; the constraints of y, which only the hook binds, are handed on after it, one store each.
  ((test/extension/post/D _)
    (⊦= '() (°->list #f (fresh° (q) (fresh° (y) (≠° y 'k) (=° q (list 'twin y 'k)))))))

  ((test/extension/post/A _)
    (⊦= '() (°->list #f (fresh° (q) (fresh° (y) (symbol° y) (=° q (list 'twin y 1)))))))

  ((test/extension/post/T _)
    (⊦= '() (°->list #f (fresh° (q) (fresh° (y) (absent° 'a y) (=° q (list 'twin y '(a))))))))

  ; twin keeps nothing in X.
  ((test/extension/state _)
    ; the slot X of the extensions rides along every goal, and `μkanren-state-remove-D` drops prefixes of D by
    ; their own content, keeping the others; `μkanren-state-no-D?` is #t of an indexed state whose P never held
    ; a prefix, and of a raw state whose list D is empty. A raw state, whose stores are lists, comes of a copy
    ; that replaces S, here by the same S, so that M is kept: X goes along it, and along the indexing by the next
    ; goal that binds, and D is read and pruned as the list.
    (define ((mark° m) s) (list (μkanren-state-with s X: m)))
    (define ((raw° m) s) (list (μkanren-state-with s S: (μkanren-state-S s) X: m)))
    (define ((X° out) s) (set-car! out (μkanren-state-X s)) (list s))
    (define ((remove-D° v) s) (list (μkanren-state-remove-D (λ (d) (equal? (cdar d) v)) s)))
    (define ((no-D?° out) s) (set-car! out (μkanren-state-no-D? s)) (list s))
    (let ((x (list 'unset)) (none (list 'unset)) (some (list 'unset)))
      (⊦= '((λ (α) (begin (deny (equal? α 2))) α))
          (°->list #f (fresh° (q) (no-D?° none) (mark° '(m)) (≠° q 1) (fresh° (r) (=° r q)) (≠° q 2) (remove-D° 1)
                              (no-D?° some) (X° x))))
      (⊦= '(m) (car x))
      (⊦= #t (car none))
      (⊦= #f (car some))
      (⊦= '() (μkanren-state-X μkanren-state-empty)))
    (let ((x (list 'unset)) (raw-X (list 'unset)) (some (list 'unset)) (none (list 'unset)))
      (⊦= '((λ (α) (begin (deny (equal? α 2))) α))
          (°->list #f (fresh° (q) (≠° q 1) (≠° q 2) (raw° '(r)) (X° raw-X) (no-D?° some) (remove-D° 1)
                              (fresh° (r) (=° r q)) (X° x))))
      (⊦= '(r) (car raw-X))
      (⊦= #f (car some))
      (⊦= '(r) (car x))
      (⊦= '((λ () 1)) (°->list #f (fresh° (q) (≠° q 1) (raw° '(r)) (remove-D° 1) (no-D?° none) (=° q 1))))
      (⊦= #t (car none)))
    ; a state made from outside carries the X it is given, raw and then indexed.
    (let1 (s (make-μkanren-state 0 (μkanren-state-S μkanren-state-empty) '() '() '() '() '(k)))
      (⊦= '(k) (μkanren-state-X s))
      (⊦= '((k)) (map μkanren-state-X (§->list ((fresh° (q) (≠° q 1) (=° q 2)) s))))))

  ; the records read by id, for an extension that looks only at the prefixes that a binding reaches: `watch` is
  ; told of each prefix that D takes, under its id, which the records list under the variables of its sides,
  ; newest first, and keep listing once the prefix is dropped and reads #f. A prefix that a type tag subsumes is
  ; not taken; a raw state is not indexed, and the next goal indexes it again, telling its prefixes anew.
  ((test/extension/watch _)
    (define ((read° q out) s)
      (set-car! out (list (μkanren-state-indexed? s) (μkanren-state-prefix-ids q s)
                          (let1 (d (μkanren-state-prefix 0 s)) (and d (map rhs d)))))
      (list s))
    (define ((drop° i) s) (list (μkanren-state-drop-prefix i s)))
    (define ((raw° m) s) (list (μkanren-state-with s S: (μkanren-state-S s) X: m)))
    (let ((before (list 'unset)) (after (list 'unset)))
      (set! told '())
      (⊦= '((λ (α) (begin (deny (equal? α 2))) α))
          (°->list #f (fresh° (q) (≠° q 1) (≠° q 2) (read° q before) (drop° 0) (read° q after))))
      (⊦= '((0 (1)) (1 (2))) (reverse told))
      (⊦= '(#t (1 0) (1)) (car before))
      (⊦= '(#t (1 0) #f) (car after)))
    (set! told '())
    (⊦= '((λ (α) (assert (every (μ v (symbol? v)) (list α))) α)) (°->list #f (fresh° (q) (symbol° q) (≠° q 1))))
    (⊦= '() told)
    (let1 (raw (list 'unset))
      (⊦= '((λ (α) (begin (deny (equal? α 1))) (begin (deny (equal? α 2))) α))
          (°->list #f (fresh° (q) (≠° q 1) (raw° '()) (read° q raw) (≠° q 2))))
      (⊦= #f (car (car raw)))
      (⊦= '((0 (1)) (0 (1)) (1 (2))) (reverse told))))

)

(unittest/✓ microkanren-extension-suite)
