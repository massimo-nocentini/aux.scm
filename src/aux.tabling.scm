
; (aux tabling): memoization and tabled definitions, from the old `on-scheme` commons.
;
; `memoize!` is *not* (aux base) `memoize`: it patches the procedure object in place with
; `mutate-procedure!`, so recursive calls that go through the original binding (for example
; a `letrec`-bound `F`) are memoized too; `memoize` would return a new wrapper instead and
; leave the recursion exponential.
;
; A procedure made by `define-tabled`, `letrec-tabled` or `lambda-tabled` takes two extra
; keyword arguments: `fresh:` (when true, recompute even if the arguments are in the table)
; and `store:` (when false, do not save the computed value).

(module (aux tabling) *

  (import scheme
          (chicken base)
          (chicken memory representation)
          srfi-69
          (aux base))

  ; a table keeps the value of a call as it is, and the values of a call that returns zero or
  ; several of them in a `tabled/values` box, so that a table read directly (as through `λH`)
  ; holds plain values in the usual case, and no value is lost in the others.
  (define-record tabled/values list)

  (define (tabled/values->entry . vs) (if (and (pair? vs) (null? (cdr vs))) (car vs) (make-tabled/values vs)))
  (define (tabled/entry->values v) (if (tabled/values? v) (apply values (tabled/values-list v)) v))

  (define hash-table-ref/store
    (λ (H)
      (λ (key missing)
        (tabled/entry->values
          (cond
            ((hash-table-exists? H key) (hash-table-ref H key))
            (else (let1 (v (call-with-values (τ (apply missing key)) tabled/values->entry))
                    (begin1 v (hash-table-set! H key v)))))))))

  (define hash-table-ref/maybe
    (λ (H key)
      (cond
        ((hash-table-exists? H key) (values #t (hash-table-ref H key)))
        (else (values #f (void))))))

  (define memoize!
    (λ (f #!key (H (make-hash-table test: equal?)))
      (let1 (↑ (hash-table-ref/store H))
        (mutate-procedure! f (λ (f)
                               (λ args
                                 (↑ args f)))))))

  ; a sentinel: a tabled procedure may answer it with its hidden table (see `λH` below).
  (define tabled/get-hidden-hash-table (gensym))

  ; `(define-tabled name λH (lambda (arg ...) body ...))` also binds `λH`, in the scope of
  ; `body`, to a thunk that returns the hidden hash table.
  (define-syntax define-tabled
    (syntax-rules (lambda)
      ((define-tabled name (lambda (args ...) body ...))
       (define-tabled name λH-ignored (lambda (args ...) body ...)))
      ((define-tabled name λH (lambda (args ...) body ...))
       (define name
         (let* ((H (make-hash-table test: equal?))
                (λH (τ H)))
           (letrec ((name (lambda (args ... #!key (fresh #f) (store #t))
                            (let ((k `(,args ...)))
                              (let-values (((found v) (hash-table-ref/maybe H k)))
                                (unless (and found (not fresh))
                                  (set! v (call-with-values (τ body ...) tabled/values->entry))
                                  (when store (hash-table-set! H k v)))
                                (tabled/entry->values v))))))
             name))))))

  (define-syntax letrec-tabled
    (syntax-rules (lambda)
      ((letrec-tabled ((name (lambda (args ...) λ-body ...)) ...) body ...)
       (letrec-tabled ((name λH-ignored (lambda (args ...) λ-body ...)) ...) body ...))
      ((letrec-tabled ((name H (lambda (args ...) λ-body ...)) ...) body ...)
       (let ()  ; to limit the scope of tabled definitions;
                ; on the contrary, `begin` doesn't limit their scope.
         (define-tabled name H (lambda (args ...) λ-body ...)) ...
         (begin body ...)))))

  ; `(lambda-tabled (arg ...) body ...)` or `(lambda-tabled λH → (arg ...) body ...)`.
  ; The clause order differs from the original, whose catch-all first clause matched the
  ; `λH →` form too and so never terminated its expansion.
  (define-syntax lambda-tabled
    (syntax-rules (→)
      ((lambda-tabled H → (args ...) body ...) (letrec-tabled ((bind H (lambda (args ...) body ...))) bind))
      ((lambda-tabled (args ...) body ...) (lambda-tabled λH-ignored → (args ...) body ...))))

  )
