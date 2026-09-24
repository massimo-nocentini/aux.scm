
(import (aux unittest) (aux base))

; from on-scheme ackermann-test: the Ackermann function on unary numbers, as lists; the
; unused `φ` and `AA` macros and the `ackermann-expander` REPL demo are not ported, and neither
; are the `tabling` macro (before/ref/store/else) and the `restart` continuation hook, which
; exist only to support that demo.
(define-suite ackermann-suite

  ((test/unary-ackermann _)
   (define A
     (λ (α β)
       (match/first (list α β)
         ((() ,ns) (cons '● ns))
         (((,m . ,ms) ()) (A ms '(●)))
         (((,m . ,ms) (,n . ,ns)) (A ms (A (cons m ms) ns))))))
   (⊦= '(● ● ● ● ● ● ● ● ●) (A '(a a) '(a a a))))

  )

(unittest/✓ ackermann-suite)
