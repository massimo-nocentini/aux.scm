
; (aux variable): distinct logic variables introduced by `fresh`, from the old `on-scheme`
; promise.scm (renamed, because that file has nothing to do with promises).
;
; A `variable` record prints as its symbol with subscript digits, so `(V 'g123)` shows `g₁₂₃`.
; Records compare with `equal?` slot by slot, so `(equal? (V 'a) (V 'a))` holds.
; Not to be confused with `fresh°` and `freshª` of (aux kanren micro).
; (aux kanren unionfind) also exports `V`, `fresh₁`, `fresh` and `variable?`, with different
; values: import both only with a prefix or rename, e.g. `(rename (aux variable) (V var:V))`.

(module (aux variable) *

  (import scheme
          (chicken base)
          (aux base)
          (aux commons))

  (define-record variable s)

  ; checking constructor: the original `(define-datatype variable (V (s symbol?)))` rejected
  ; any non-symbol field, so does this one.
  (define V
    (λ (s)
      (if (symbol? s)
        (make-variable s)
        (error 'V "not a symbol" s))))

  (set-record-printer! variable
    (λ (v out)
      (display (symbol∼subscripts (variable-s v)) out)))

  ; functional abstraction for introducing _distinct_ `variable` objects.
  (define fresh₁
    (λ (recv)
      (recv (V (gensym)))))

  ; syntactic sugar on top of `fresh₁` to allow arbitrary arity.
  (define-syntax fresh
    (syntax-rules ()
      ((fresh (v) body ...) (fresh₁ (λ (v) body ...)))
      ((fresh (v w ...) body ...) (fresh₁ (λ (v) (fresh (w ...) body ...))))))

  )
