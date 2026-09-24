
(import (chicken sort) srfi-69 (aux unittest) (aux base) (aux commons))

(define-suite commons-suite

  ; from on-scheme learning-test, group MAPPING (the three plain `values` asserts
  ; are in test/learning-chicken.scm).
  ((test/map-values _)
   ; a single-value context keeps the first value only.
   (⊦= '(1 2 3) (map (λ (i) (values (add1 i) i)) '(0 1 2)))
   (⊦= '(1 2 3)
       ((map/call-with-values
          (λ (i) (values (add1 i) i))
          (λ (more less) more))
        '(0 1 2)))
   (⊦= '((1 1) (3 3))
       ((map/values (λ (p) (values (add1 (car p)) (cadr p))))
        '((0 1) (2 3)))))

  ((test/tuple-pred _)
   (⊦= #t ((tuple/pred? <) '(1 2) '(2 3) '(3 4)))
   (⊦= #f ((tuple/pred? <) '(1 5) '(2 3) '(3 4)))
   (⊦= #f ((tuple/pred? <) '(1 5) '(2) '(3 4))))

  ((test/remove-duplicates/last _)
   (⊦= '(a b c d e) (remove-duplicates/last (reverse '(a b c d e))))
   (⊦= '(e c d a b) (remove-duplicates/last '(a b a a c d c e e)))
   ; while the (aux base) one keeps the first occurrences in order.
   (⊦= '(a b c d e) (remove-duplicates '(a b a a c d c e e))))

  ; new cases, for the helpers that no on-scheme test covered.
  ((test/identity*-collect-values _)
   (⊦= '(hello world) (call-with-values (τ (values 'hello 'world)) identity*))
   (⊦= 'hello (identity* 'hello))
   (⊦= '() (identity*))
   (⊦= '(1 2) (collect-values (τ (values 1 2))))
   (⊦= 1 (collect-values (τ (values 1)))))

  ((test/symbol∼subscripts _)
   (⊦= 'g₁₂₃ (symbol∼subscripts 'g123))
   (⊦= 'x₀₉ (symbol∼subscripts 'x09))
   (⊦= 'hello (symbol∼subscripts 'hello))
   (⊦= 'α₁ (symbol∼subscripts 'α1))
   (⊦= "₇" (hash-table-ref subscripts #\7)))

  ((test/rtc _)
   (let1 (→ (λ (n) (if (< n 5) (add1 n) n)))
     (⊦= '(0 1 2 3 4 5) ((rtc →) 0))
     (⊦= '(5) ((rtc →) 5))))

  ((test/group _)
   (⊦= '((#f 5 3 1) (#t 4 2))
       (sort/lex<=? ((group even? (λ (p) (cons (car p) (cdr p)))) '(1 2 3 4 5)))))

  ((test/foldl1 _)
   (⊦= '() ((foldl1 +) '()))
   (⊦= 10 ((foldl1 +) '(1 2 3 4)))
   (⊦= 20 ((foldl1 + H₀: (λ (x) (* 11 x))) '(1 2 3 4)))
   (⊦= 8 ((foldl1/lshift + H₀: (λ (x) (values x cddr))) '(1 2 3 4))))

  ((test/within? _)
   (⊨ ((within? 1 3) 1))
   (⊨ ((within? 1 3) 2))
   (⊨ ((within? 1 3) 3))
   (⊭ ((within? 1 3) 4))
   (⊭ ((within? 1 3) 0)))

  ((test/map/tree _)
   (⊦= '(2 (3 4) ((5)) 6) ((map/tree add1) '(1 (2 3) ((4)) 5))))

  ((test/map/with-index _)
   (⊦= '((10 a) (11 b) (12 c)) ((map/with-index (λ (i) (λ (x) (list i x))) 10) '(a b c))))

  ((test/accumulator _)
   (let1 (A (accumulator + 0))
     (⊦= 1 (A 1))
     (⊦= 3 (A 2))
     (⊦= 6 (A 3)))
   (let1 (A (accumulator cons '()))
     (A 'a)
     (⊦= '(b a) (A 'b))))

  ((test/curried-helpers _)
   (⊦= '(2 3) ((fmap add1) '(1 2)))
   (⊦= 6 ((fapply +) '(1 2 3)))
   (⊦= '(2 4) ((ffilter even?) '(1 2 3 4)))
   (⊦= '((c . 1) (a . 2) (b . 3)) ((fsort cdr <) '((a . 2) (b . 3) (c . 1))))
   (⊦= 'b ((flist-ref '(a b c)) 1))
   (⊦= 'c ((fvector-ref #(a b c)) 2))
   (⊨ ((equals-to? '(a)) '(a)))
   (⊨ ((=to? '(a)) '(a)))
   (⊭ ((=to? (list 'a) same?: eq?) (list 'a)))
   (⊦= 'num (cond/λ 3 (symbol? (K 'sym)) (number? (K 'num)) (else identity)))
   (⊦= "s" (cond/λ "s" (symbol? (K 'sym)) (number? (K 'num)) (else identity))))

  ((test/numeric-and-strings _)
   (⊦= 3 (sub2 5))
   (⊨ (≠ 'a 'b))
   (⊭ (≠ '(a) '(a)))
   (⊦= 1/4 (⁻¹ 4))
   (⊦= 16 (² 4))
   (⊦= '|42| (number->symbol 42))
   (⊦= "(1 a)" (to-string '(1 "a")))
   (call+stdout
     (τ (display "hello") 'result)
     (λ (r s)
       (⊦= 'result r)
       (⊦= "hello" s))))

  )

(unittest/✓ commons-suite)
