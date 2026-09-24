
(import (aux unittest) (aux base) (aux machine env))

; E->alist follows the hash-table-fold order of srfi-69, which is unspecified: both sides are
; compared after `sort/lex<=?`, the expected alists are the original ones, verbatim.
(define ⊦=/alist
  (λ (expected actual)
    (⊦= (sort/lex<=? expected) (sort/lex<=? actual))))

; from on-scheme SC-machine-test, the 10 checks about the commons environments.
(define-suite machine-env-suite

  ((doc r) `((structure/section "Tabled environments")
             (p "Ported from on-scheme's " (code/inline "commons.scm") "; the checks come from "
                (code/inline "SC-machine-test.scm") ".")
             (structure/section "Implementation")
             (code/scheme/file "../aux.machine.env.scm")))

  ((test/extend-lookup _)
   (define E₁ ((extend E₀) '(a . 3)))
   (define E₂ ((extend E₁) '(b . 4)))
   (⊦= 3 (E₂ 'a))
   (⊦= 4 (E₂ 'b))
   (⊨ (void? (E₂ 'c)))
   (⊦= 3 (E₂ 'a)) ; memoized
   (⊦=/alist `(((c) . ,(void)) ((a) . 3) ((b) . 4)) (E->alist E₂))

   (define E₃ ((extend E₂) '(b . 0)))

   (⊦=/alist `(((c) . ,(void)) ((a) . 3) ((b) . 4)) (E->alist E₃))
   (⊦= 0 (E₃ 'b)) ; invalidate the cache
   (⊦=/alist `(((c) . ,(void)) ((a) . 3) ((b) . 0)) (E->alist E₃)))

  ((test/E-null? _)
   (define E₁ ((extend E₀) '(a . 3)))
   (⊨ (E-null? E₀))
   (⊭ (E-null? E₁)))

  ; new: `E⁺` was only used by commented-out SECD tests.
  ((test/E⁺ _)
   (define E ((extend E⁺) '(a . 3)))
   (⊦= 3 (E 'a))
   (⊦= 7 (E 7))
   (⊦= car (E 'car))
   (⊨ (void? (E 'an-unbound-identifier)))
   (⊨ (void? (E '(car x)))))

  ; new: extending with several associations at once, and the `same?:` keyword.
  ((test/extend-many _)
   (define E ((extend E₀) '(a . 1) '(b . 2) '(a . 3)))
   (⊦= 1 (E 'a)) ; the leftmost association wins
   (⊦= 2 (E 'b))
   (define E/eq ((extend E₀ same?: eq?) (cons "s" 1)))
   (⊨ (void? (E/eq (string #\s))))
   (define E/equal ((extend E₀) (cons "s" 1)))
   (⊦= 1 (E/equal (string #\s))))

  )

(unittest/✓ machine-env-suite)
