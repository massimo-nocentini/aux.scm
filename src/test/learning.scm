

(import (aux unittest) scheme (chicken base) (chicken pretty-print) (chicken condition) (chicken foreign) (chicken gc))


(define-suite learning-suite

  ((test-alist-ref _) 
   (let ((alst '((a 3) (b 2))))
     (⊦= '(3) (alist-ref 'a alst))
     (⊦= '(2) (alist-ref 'b alst))
     (⊦= #f (alist-ref 'c alst))))

  ((test/len _) (let ((my-strlen (foreign-lambda* int 
                                                  ((scheme-object cons)) 
                                                  "C_return(C_header_size(cons));")))
                  (⊦= 2 (my-strlen (cons 1 '())))
                  (⊦= 2 (my-strlen (cons 1 (cons 2 (cons 3 '())))))
                  (⊦= 11 (my-strlen "hello world"))))

  ((test/c-apply _) 
   (let ((witness (gensym))
         (my-strlen (foreign-safe-lambda* scheme-object ((scheme-object f)) 
                                          #<<END
					  C_word res;
					  int s = CHICKEN_apply(f, C_SCHEME_END_OF_LIST, &res);
					  printf("code: %d\n", s);
					  C_return (res);
END
                                          )))
     (⊦= witness (car (my-strlen (lambda () (list witness 4)))))))

  ((test-null-eq? _) (⊨ (eq? '() '())))
  #;((test-null-car _) (⊦⧳ ((exn)) (car (list))))
  )

(unittest/✓ learning-suite)











