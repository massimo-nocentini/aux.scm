
(import (chicken port) (chicken condition) (aux unittest) (aux base) (aux commons) (aux variable))

(define subscript-digits (string->list "₀₁₂₃₄₅₆₇₈₉"))

; `g` followed by one or more subscript digits, as `fresh₁` names print.
(define (gensym-subscripted? str)
  (let1 (cs (string->list str))
    (and (pair? cs)
         (pair? (cdr cs))
         (char=? #\g (car cs))
         (every/subscript? (cdr cs)))))

(define (every/subscript? cs)
  (or (null? cs) (and (member? (car cs) subscript-digits) (every/subscript? (cdr cs)))))

(define (display->string v) (with-output-to-string (τ (display v))))

; from on-scheme promise-test, group "subscripting symbols and variables introduction via `fresh`".
(define-suite variable-suite

  ((test/symbol∼subscripts _)
   (⊦= 'g₁₂₃ (symbol∼subscripts 'g123)))

  ((test/variable-printer _)
   (⊦= "g₁₂₃" (display->string (V 'g123)))
   (⊦= "(g₁ hello)" (display->string (list (V 'g1) (V 'hello)))))

  ; The original expected "g₁" and then "g₂" from two `(fresh₁ display)`, and "(g₃ g₄ g₅)" from
  ; `(fresh (v w z) (display (list v w z)))`: those literal names depend on the global gensym
  ; counter, which csi and (aux unittest) (one gensym per case) advance before the test runs,
  ; so the assertions are kept structurally: `g` plus subscript digits, the variable's own
  ; subscripted symbol, and pairwise distinct names.
  ((test/fresh₁ _)
   (let ((g₁ (with-output-to-string (τ (fresh₁ display))))
         (g₂ (with-output-to-string (τ (fresh₁ display)))))
     (⊨ (gensym-subscripted? g₁))
     (⊨ (gensym-subscripted? g₂))
     (⊭ (equal? g₁ g₂)))
   (fresh₁ (λ (v)
             (⊨ (variable? v))
             (⊦= (symbol->string (symbol∼subscripts (variable-s v))) (display->string v)))))

  ((test/fresh _)
   (let1 (vs (fresh (v w z) (list v w z)))
     (⊦= 3 (length vs))
     (⊨ (pairwise-different? vs))
     (let1 (str (with-output-to-string (τ (display vs))))
       (⊦= (string-append "(" (display->string (car vs))
                          " " (display->string (cadr vs))
                          " " (display->string (caddr vs)) ")")
           str)
       (⊨ (and (gensym-subscripted? (display->string (car vs)))
               (gensym-subscripted? (display->string (cadr vs)))
               (gensym-subscripted? (display->string (caddr vs))))))))

  ((test/variable-equal? _)
   (let ((one (V (gensym)))
         (two (V 'hello))
         (three (V 'hello)))
     (⊦= #f (equal? one two))
     (⊦= #f (equal? one three))
     (⊦= #t (equal? two three))))

  ; the original `define-datatype` field predicate `symbol?` rejects non-symbols.
  ((test/V-rejects-non-symbols _)
   (⊦= 'rejected (condition-case (begin (V 42) 'accepted) ((exn) 'rejected)))
   (⊦= 'rejected (condition-case (begin (V "g1") 'accepted) ((exn) 'rejected))))

  )

(unittest/✓ variable-suite)
