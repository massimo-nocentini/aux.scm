
; Tests of the `matchable` egg itself, from on-scheme learning-test group MATCHABLE; it is not
; a dependency of aux, so this file stays out of the `test` target.
(import matchable (aux unittest) (aux base))

(define-suite learning-matchable-suite

  ((test/multiple-matches _)
   (⊦= 10 (match '(1 2 2 2 2 2 3)
            ((1 x ... 3) (apply + x)))))

  ((test/multiple-matches-literal _)
   (⊦= 2 (match '(1 1 1 1 1 2 3)
           ((1 ... x 3) x))))

  ((test/match-inside-a-string _)
   (⊦= 8 (match '(1 2 3)
           ((1 x 3) (* x x x)))))

  ((test/declarative _)
   (⊦= 8 (let ((x 3))
           (match '(1 3 2 3)
             ((1 x y 3) (* y y y))))))

  ; the original title is "declarative SHOULD RISE AN ERROR", but it expects 8: a pattern
  ; variable shadows the lexical `x`, whatever its value.
  ((test/declarative-shadowing _)
   (⊦= 8 (let ((x 4))
           (match '(1 3 2 3)
             ((1 x y 3) (* y y y))))))

  ((test/quote-in-pattern _)
   (⊦= 'a-match (match '(1 x 3)
                  ((1 'x 3) 'a-match))))

  ((test/matching-a-quoted-symbol _)
   (⊦= 'x (match '(1 x 3)
            ((1 y 3) y))))

  ((test/quoted-head-and-ellipsis _)
   (⊦= '(b c)
       (match '(a b c)
         (('a x ...) x)))
   (⊦= 'else (match '(a b c)
               (('b x ...) x)
               (else 'else))))

  ((test/nested-ellipsis _)
   (⊦= '(λ (b c) d)
       (let1 (l '(a (b c) d))
         (match l (('a (x ...) y) `(λ (,@x) ,y))))))

  )

(unittest/✓ learning-matchable-suite)
