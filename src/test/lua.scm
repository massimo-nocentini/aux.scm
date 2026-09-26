
; not in the Makefile `test` target: it needs the Lua headers and library, like (aux lua).
(import (chicken gc) (only (chicken memory) pointer?) (aux unittest) (aux base) (aux lua))

(define-suite lua-suite

  ((test/make-empty _)
   (let1 (L (lua-make-empty))
     (⊨ (luaR? L))
     (⊨ (pointer? (luaR-state L)))))

  ; closing is idempotent, and the finalizer does not close a closed state again.
  ((test/close-twice _)
   (let1 (L (lua-make-empty))
     (lua-close L)
     (⊦= #f (luaR-state L))
     (lua-close L)
     (⊦= #f (luaR-state L))
     (set! L #f)
     (gc #t)))

  )

(unittest/✓ lua-suite)
