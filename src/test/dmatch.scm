
(import scheme (chicken base) (chicken memory representation) (aux base) (aux unittest) (aux match))

(define-suite dmatch-suite

  #;((doc r) (let ((letcc-expr '(letcc k body ...)))
             `((structure/section "Implementation")
               (code/scheme/file "../aux.match.scm"))))

  ((test/base _)
      (⊦= 'empty (match/non-overlapping '() (() 'empty)))
      (⊦= 'empty (match/non-overlapping #() (() 'empty)))
      (⊦= '() (match/non-overlapping '() (,r r)))
      (⊦= #() (match/non-overlapping #() (,r r)))
      (⊦= 'p (match/non-overlapping '(p) ((,r) r)))
      (⊦= #t (match/non-overlapping #(p) ((p) #t)))
      (⊦= 'p (match/non-overlapping #(p) ((,r) r)))
      (⊦= 3 (match/non-overlapping #(3 2) ((,r 2) r)))
      (⊦= '(3 #(2)) (match/non-overlapping #(3 2) ((,r . ,s) (list r s))))
      (⊦⧳ ((exn)) (match/non-overlapping #(3 2) ((,r 2 ,t) r)))
      (⊦= 3 (match/non-overlapping #(3 2) ((,r ,e) r)))
      (⊦= 3 (match/non-overlapping (make-record-instance 'hello 3 2) ((hello ,r ,e) r))))

  ((test/h _)
      (define h
            (lambda (x y)
                  (match/non-overlapping `(,x . ,y) "h function, example"
                  ((,a . ,b) (guard (number? a) (number? b)) (* a b))
                  ((,a ,b ,c) (guard (number? a) (number? b) (number? c)) (+ a b c)))))
      (⊦= '(12 8) (list (h 3 4) (apply h '(1 (3 4))))))

  ((test/h-wrong _)
      (define h
            (lambda (x y)
                  (match/non-overlapping `(,x . ,y) "h function, example"
                  ((,a . ,b) (guard (number? a) (number? b)) (* a b))
                  ((,a . ,b) (+ a b))
                  ((,a ,b ,c) (guard (number? a) (number? b) (number? c)) (+ a b c)))))
      (⊦⧳ ((exn)) (list (h 3 4) (apply h '(1 (3 4))))))

  )

(unittest/✓ dmatch-suite)