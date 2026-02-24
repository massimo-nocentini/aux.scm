
(import scheme (chicken base) (chicken memory representation) (aux base) (aux unittest) (aux match))

(define-suite dmatch-suite

  #;((doc r) (let ((letcc-expr '(letcc k body ...)))
             `((structure/section "Implementation")
               (code/scheme/file "../aux.match.scm"))))

  ((test/base _)
      (⊦= 'empty (match/guarded '() (() 'empty)))
      (⊦= 'empty (match/guarded #() (() 'empty)))
      (⊦= '() (match/guarded '() (,r r)))
      (⊦= #() (match/guarded #() (,r r)))
      (⊦= 'p (match/guarded '(p) ((,r) r)))
      (⊦= #t (match/guarded #(p) ((p) #t)))
      (⊦= 'p (match/guarded #(p) ((,r) r)))
      (⊦= 3 (match/guarded #(3 2) ((,r 2) r)))
      (⊦= '(3 #(2)) (match/guarded #(3 2) ((,r . ,s) (list r s))))
      (⊦⧳ ((exn)) (match/guarded #(3 2) ((,r 2 ,t) r)))
      (⊦= 3 (match/guarded #(3 2) ((,r ,e) r)))
      (⊦= 3 (match/guarded (make-record-instance 'hello 3 2) ((hello ,r ,e) r))))

  ((test/h _)
      (define h
            (lambda (x y)
                  (match/guarded `(,x . ,y) "h function, example"
                  ((,a . ,b) (guard (number? a) (number? b)) (* a b))
                  ((,a ,b ,c) (guard (number? a) (number? b) (number? c)) (+ a b c)))))
      (⊦= '(12 8) (list (h 3 4) (apply h '(1 (3 4))))))

  ((test/h-wrong _)
      (define h
            (lambda (x y)
                  (match/guarded `(,x . ,y) "h function, example"
                  ((,a . ,b) (guard (number? a) (number? b)) (* a b))
                  ((,a . ,b) (+ a b))
                  ((,a ,b ,c) (guard (number? a) (number? b) (number? c)) (+ a b c)))))
      (⊦⧳ ((exn)) (list (h 3 4) (apply h '(1 (3 4))))))

  )

(unittest/✓ dmatch-suite)