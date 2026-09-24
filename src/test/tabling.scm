
(import (only srfi-1 iota append-map) srfi-69 (aux unittest) (aux base) (aux tabling))

; from on-scheme learning-test, group TABLING; `time` reports go to stderr, captured by the unittest.
(define-suite tabling-suite

  ((test/naive-fibonacci _)
   (define F
     (λ (i)
       (cond
         ((< i 2) i)
         (else (+ (F (- i 1)) (F (- i 2)))))))
   (⊦= 0 (F 0))
   (⊦= 1 (F 1))
   (⊦= '(0 1 1 2 3 5 8 13 21) (map F (iota 9)))
   (time (⊦= 1346269 (F 31))))

  ((test/define-tabled-fibonacci _)
   (define-tabled F₀
     (lambda (i)
       (cond
         ((zero? i) 0)
         ((one? i) 1)
         (else (+ (F₀ (- i 1)) (F₀ (- i 2)))))))
   (⊦= 0 (F₀ 0))
   (⊦= 1 (F₀ 1))
   (⊦= '(0 1 1 2 3 5 8 13 21) (map F₀ (iota 9)))
   (time (⊦= 1346269 (F₀ 31))))

  ((test/memoize!-letrec _)
   ; `memoize!` patches `F` in place, so the recursion through the `letrec` binding is
   ; memoized too; (aux base) `memoize` would not terminate here.
   (define fibonacci
     (letrec ((F (λ (i)
                   (cond
                     ((< i 2) i)
                     (else (+ (F (- i 1)) (F (- i 2))))))))
       (memoize! F)))
   (time (⊦= 354224848179261915075 (fibonacci 100))))

  ((test/memoize!-define _)
   (define F₁
     (memoize! (λ (i)
                 (cond
                   ((< i 2) i)
                   (else (+ (F₁ (- i 1)) (F₁ (- i 2))))))))
   (time (⊦= 1346269 (F₁ 31))))

  ((test/pascal _)
   (define-tabled pascal
     (lambda (n k)
       (cond
         ((and (zero? n) (zero? k)) 1)
         ((zero? n) 0)
         ((zero? k) (pascal (sub1 n) 0))
         (else (+ (pascal (sub1 n) (sub1 k)) (pascal (sub1 n) k))))))
   (define Riordan-array
     (λ (recurrence)
       (λ (m)
         (map (λ (n)
                (append-map (λ (k) (list (recurrence n k)))
                            (iota (add1 n))))
              (iota m)))))
   (⊦= 1 (pascal 0 0))
   (⊦= 2 (pascal 2 1))
   (⊦= 1 (pascal 2 2))
   (⊦= 3 (pascal 3 2))
   (⊦= 100891344545564193334812497256 (pascal 100 50))
   (⊦= '((1)
         (1 1)
         (1 2 1)
         (1 3 3 1)
         (1 4 6 4 1)
         (1 5 10 10 5 1)
         (1 6 15 20 15 6 1)
         (1 7 21 35 35 21 7 1)
         (1 8 28 56 70 56 28 8 1)
         (1 9 36 84 126 126 84 36 9 1)
         (1 10 45 120 210 252 210 120 45 10 1)
         (1 11 55 165 330 462 462 330 165 55 11 1)
         (1 12 66 220 495 792 924 792 495 220 66 12 1)
         (1 13 78 286 715 1287 1716 1716 1287 715 286 78 13 1)
         (1 14 91 364 1001 2002 3003 3432 3003 2002 1001 364 91 14 1)
         (1 15 105 455 1365 3003 5005 6435 6435 5005 3003 1365 455 105 15 1)
         (1 16 120 560 1820 4368 8008 11440 12870 11440 8008 4368 1820 560 120 16 1)
         (1 17 136 680 2380 6188 12376 19448 24310 24310 19448 12376 6188 2380 680 136 17 1)
         (1 18 153 816 3060 8568 18564 31824 43758 48620 43758 31824 18564 8568 3060 816 153 18 1)
         (1 19 171 969 3876 11628 27132 50388 75582 92378 92378 75582 50388 27132 11628 3876 969 171 19 1))
       ((Riordan-array pascal) 20)))

  ((test/catalan _)
   (define-tabled catalan
     (lambda (n k)
       (cond
         ((and (zero? n) (zero? k)) 1)
         ((zero? n) 0)
         ((zero? k) (apply + (map (λ (j) (catalan (sub1 n) j)) (iota n))))
         (else (apply + (map (λ (j) (catalan (sub1 n) j)) (iota n (sub1 k))))))))
   (define Riordan-array
     (λ (recurrence)
       (λ (m)
         (map (λ (n)
                (append-map (λ (k) (list (recurrence n k)))
                            (iota (add1 n))))
              (iota m)))))
   (⊦= '((1)
         (1 1)
         (2 2 1)
         (5 5 3 1)
         (14 14 9 4 1)
         (42 42 28 14 5 1)
         (132 132 90 48 20 6 1)
         (429 429 297 165 75 27 7 1)
         (1430 1430 1001 572 275 110 35 8 1)
         (4862 4862 3432 2002 1001 429 154 44 9 1)
         (16796 16796 11934 7072 3640 1638 637 208 54 10 1)
         (58786 58786 41990 25194 13260 6188 2548 910 273 65 11 1)
         (208012 208012 149226 90440 48450 23256 9996 3808 1260 350 77 12 1)
         (742900 742900 534888 326876 177650 87210 38760 15504 5508 1700 440 90 13 1)
         (2674440 2674440 1931540 1188640 653752 326876 149226 62016 23256 7752 2244 544 104 14 1)
         (9694845 9694845 7020405 4345965 2414425 1225785 572033 245157 95931 33915 10659 2907 663 119 15 1)
         (35357670 35357670 25662825 15967980 8947575 4601610 2187185 961400 389367 144210 48279 14364 3705 798 135 16 1)
         (129644790 129644790 94287120 58929450 33266625 17298645 8351070 3749460 1562275 600875 211508 67298 19019 4655 950 152 17 1)
         (477638700 477638700 347993910 218349120 124062000 65132550 31865925 14567280 6216210 2466750 904475 303600 92092 24794 5775 1120 170 18 1)
         (1767263190 1767263190 1289624490 811985790 463991880 245642760 121580760 56448210 24582285 10015005 3798795 1332045 427570 123970 31878 7084 1309 189 19 1))
       ((Riordan-array catalan) 20)))

  ((test/ackermann _)
   (define-tabled ackermann
     (lambda (m n)
       (cond
         ((zero? m) (add1 n))
         ((zero? n) (ackermann (sub1 m) 1))
         (else (ackermann (sub1 m) (ackermann m (sub1 n)))))))
   (⊦= 7 (ackermann 2 2))
   (⊦= 125 (ackermann 3 4)))

  ; new cases, for what no on-scheme test covered.
  ((test/fresh-and-store-keywords _)
   (define calls 0)
   (define-tabled sq λH
     (lambda (x)
       (cond
         ((eq? x tabled/get-hidden-hash-table) (λH)) ; `λH` is visible in the body only
         (else (set! calls (add1 calls))
               (* x x)))))
   (⊦= 9 (sq 3))
   (⊦= 1 calls)
   (⊦= 9 (sq 3))
   (⊦= 1 calls)                       ; answered by the table
   (⊦= 9 (sq 3 fresh: #t))
   (⊦= 2 calls)                       ; recomputed
   (⊦= 16 (sq 4 store: #f))
   (⊦= 3 calls)
   (⊦= 16 (sq 4))
   (⊦= 4 calls)                       ; 4 was not stored
   (⊦= '((3) (4)) (sort/lex<=? (hash-table-keys (sq tabled/get-hidden-hash-table store: #f)))))

  ((test/letrec-tabled _)
   (⊦= '(#t #f 832040)
       (letrec-tabled ((ev? (lambda (n) (if (zero? n) #t (od? (sub1 n)))))
                       (od? (lambda (n) (if (zero? n) #f (ev? (sub1 n)))))
                       (fib (lambda (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))))
         (list (ev? 10) (od? 10) (fib 30))))
   (⊦= '(1 ((0) (2)))
       (letrec-tabled ((f H (lambda (x) (if (eq? x tabled/get-hidden-hash-table) (H) (add1 x)))))
         (f 0) (f 2)
         (list (f 0) (sort/lex<=? (hash-table-keys (f tabled/get-hidden-hash-table store: #f)))))))

  ((test/lambda-tabled _)
   (let1 (f (lambda-tabled (x y) (+ x y)))
     (⊦= 3 (f 1 2))
     (⊦= 3 (f 1 2 fresh: #t)))
   (let1 (f (lambda-tabled H → (x) (list x (hash-table-size (H)))))
     (⊦= '(a 0) (f 'a))
     (⊦= '(a 0) (f 'a))
     (⊦= '(b 1) (f 'b))))

  ((test/hash-table-ref/store-and-maybe _)
   (let* ((H (make-hash-table test: equal?))
          (↑ (hash-table-ref/store H)))
     (⊦= 3 (↑ '(1 2) +))
     (⊦= 3 (↑ '(1 2) (λ _ 'unused)))
     (⊦= '(#t 3) (call-with-values (τ (hash-table-ref/maybe H '(1 2))) list))
     (⊦= '(#f) (call-with-values (τ (hash-table-ref/maybe H '(3))) (λ (found v) (list found))))
     (⊨ (void? (call-with-values (τ (hash-table-ref/maybe H '(3))) (λ (found v) v))))))

  )

(unittest/✓ tabling-suite)
