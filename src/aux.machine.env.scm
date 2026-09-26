
; (aux machine env): the curried, tabled environments of the old `on-scheme` commons, used by
; the SC and SECD machines in (aux machine sc) and (aux machine secd).
;
; An environment is a procedure `E` from an identifier to its value; `(void)` means unbound.
; `((extend E) '(x . 1) '(y . 2) ...)` returns a new environment that answers the given
; associations and falls back to `E` for everything else. Every environment made by `extend`
; is a tabled procedure (see (aux tabling)): lookups are cached in a hidden hash table, and
; `E->alist` collects the merge of all the hidden tables of the chain.
;
; The hidden tables compare the identifiers with `equal?`, so an environment extended with a
; `same?:` other than `equal?` (e.g. `eq?`), and every extension of it, is not tabled.
;
; `E₀` is the empty environment, `E⁺` is an environment that falls back to `eval` for any
; atom, answering `(void)` on error.

(module (aux machine env) *

  (import scheme
          (chicken base)
          (chicken condition)
          (only (chicken memory representation) extend-procedure procedure-data)
          srfi-69
          (aux base)
          (aux commons)
          (aux tabling))

  ; the hidden tables of `letrec-tabled` are `equal?` ones, so they may only cache lookups that
  ; cannot tell `equal?` identifiers apart: an environment whose `same?` is not `equal?`, or
  ; that extends such an environment, is marked `untabled` (as its procedure data) and caches
  ; nothing; its hidden table is the one of its closest tabled ancestor.
  (define extend
    (λ (E #!key (same? equal?))
      (let* ((lookup (λ (p E z)
                       (letcar&cdr (((x y) p))
                         (cond
                           ((same? x z) y)
                           (else (E z))))))
             (extend₁
               (cond
                 ((and (eq? same? equal?) (not (eq? (procedure-data E) 'untabled)))
                  (λ (p E)
                    (letrec-tabled ((E₁ H (lambda (z)
                                            (cond
                                              ((eq? tabled/get-hidden-hash-table z)
                                               (hash-table-merge (H) (E z fresh: #t store: #f)))
                                              (else (lookup p E z))))))
                      E₁)))
                 (else (λ (p E)
                         (extend-procedure
                           (λ (z #!key (fresh (void)) (store (void)))
                             (cond
                               ((eq? tabled/get-hidden-hash-table z) (E z fresh: #t store: #f))
                               (else (lookup p E z))))
                           'untabled))))))
        (λ assocs
          (foldr extend₁ E assocs)))))

  (define E₀
    (λ (z #!key (fresh (void)) (store (void)))
      (cond
        ((eq? tabled/get-hidden-hash-table z) (make-hash-table))
        (else (void)))))

  ; the original used `with-exception-handler` with a handler that returns, which hangs on
  ; CHICKEN 6 because returning from the handler of an `error` re-signals; `handle-exceptions`
  ; escapes to its body instead.
  (define E⁺
    (λ (z #!key (fresh (void)) (store (void)))
      (cond
        ((eq? tabled/get-hidden-hash-table z) (make-hash-table))
        ((atom? z) (handle-exceptions _ (void) (eval z)))
        (else (void)))))

  (define E->alist
    (λ (E)
      (hash-table-fold
        (E tabled/get-hidden-hash-table fresh: #t store: #f)
        (λ (k v acc) (cons `(,k . ,v) acc))
        '())))

  (define E-null? (=to? E₀ same?: eq?))

  )
