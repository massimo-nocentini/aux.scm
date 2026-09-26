
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

  ; new: the hidden tables are `equal?` ones, so a lookup under a finer `same?:` must not be
  ; cached, in the environment itself nor in its extensions, else a miss on an `equal?` alias
  ; hides the binding, or a hit answers for the alias.
  ((test/extend-same?-untabled _)
   (define s "s")
   (define E/miss-first ((extend E₀ same?: eq?) (cons s 1)))
   (⊨ (void? (E/miss-first (string #\s))))
   (⊦= 1 (E/miss-first s))
   (define E/hit-first ((extend E₀ same?: eq?) (cons s 1)))
   (⊦= 1 (E/hit-first s))
   (⊨ (void? (E/hit-first (string #\s))))
   (define E/child ((extend ((extend E₀ same?: eq?) (cons s 1))) '(a . 2)))
   (⊨ (void? (E/child (string #\s))))
   (⊦= 1 (E/child s))
   (⊦= 2 (E/child 'a))
   (⊦=/alist '() (E->alist E/child))
   ; the tabled ancestors of an untabled environment still cache, and `E->alist` shows them.
   (define E/tabled ((extend E₀) '(b . 3)))
   (define E/over ((extend E/tabled same?: eq?) (cons s 1)))
   (⊦= 3 (E/over 'b))
   (⊦=/alist '(((b) . 3)) (E->alist E/over)))

  )

(unittest/✓ machine-env-suite)
