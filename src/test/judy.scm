
(import scheme (chicken base) srfi-1 (aux unittest) (aux base) (aux judy) (aux judyL))

(define-suite judy-suite

  ((test/empty _)
   (let1 (a (judy-array-empty)) 
    (⊦ = 0 (judy-array-size a))
    (⊦ = 0 (judy-array-bytes a))))

((test/set!-ref _)
 (let1 (a (judy-array-empty)) 
  (judy-array-set! a 'key '(zero "hello"))
  (⊦ = 1 (judy-array-size a))
  (⊦ = 24 (judy-array-bytes a))
  (⊦ (λ (v z) (not (eq? v z))) '(zero "hello") (judy-array-ref a 'key))
  (⊦ equal? '(zero "hello") (judy-array-ref a 'key))
  (⊦ equal? 'missing (judy-array-ref/default a 0 (λ_ 'missing)))))

((test/alist->judy-array _)
  (let1 (a (alist->judy-array '((0 zero) (1 one) (2 two) (10000 ten-thousand))))
    (⊦ = 4 (judy-array-size a))
    (⊦ = 88 (judy-array-bytes a))
    (⊦ equal? 'zero (judy-array-ref a 0))
    (⊦ equal? 'one (judy-array-ref a 1))
    (⊦ equal? 'two (judy-array-ref a 2))
    (⊦ equal? 'ten-thousand (judy-array-ref a 10000))
    (⊦ equal? 'missing (judy-array-ref/default a 3 (λ_ 'missing)))))

((test/list->judy-array _)
  (let1 (a (list->judy-array '(zero one two three four five six seven eight nine ten)))
    (⊦ = 11 (judy-array-size a))
    (⊦ = 184 (judy-array-bytes a))
    (⊦ equal? 'zero (judy-array-ref a 0))
    (⊦ equal? 'five (judy-array-ref a 5))
    (⊦ equal? 'ten (judy-array-ref a 10))
    (⊦ equal? 'missing (judy-array-ref/default a 11 (λ_ 'missing)))))

((test/judy-array->alist _)
  (let1 (alist '((0 zero) (1 one) (2 two) (10000 ten-thousand)))
    (let1 (a (alist->judy-array alist))
      (⊦ equal? alist (judy-array->alist a)))))

((test/list->judy-array/iota _)
  (let* ((n 100000) (I (iota n)))
    (let1 (a (list->judy-array I))
      (⊦ = n (judy-array-size a))
      (for-each (λ (i) (⊦ = i (judy-array-ref a i))) I))))

((test/judy-array-walk _)
  (let* ((n 50000) (I (iota n)))
    (let1 (a (list->judy-array I))
      (let1 (collected '())
        (judy-array-walk/backward a (λ (i v) (push! v collected)))
        (⊦ equal? I collected)
        (set! collected '())
        (judy-array-walk/forward a (λ (i v) (push! v collected)))
        (⊦ equal? I (reverse collected))))))

)

(unittest/✓ judy-suite)




#|

(define a (alist->judy-array '((0 "zero") (1 "one") (2 "two") (10000 "ten thousand"))))
(define a (alist->judy-array '((0 'zero) (1 'one) (2 'two) (10000 'ten-thousand))))
(judy-array-free a)
(judy-array->alist a)
(judy-array-walk a (λ (i v) (when (= (modulo i 10000) 0) (display (list i v)))))

|#














































