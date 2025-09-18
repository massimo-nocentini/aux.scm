

(import
  (aux base) (aux unittest) 
  scheme (chicken base) (chicken pretty-print) (chicken condition) (chicken foreign) (chicken gc))


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

  ((test/unquote _) 
   (let1 (a '(3)) (⊦= (cons 1 a)  `(1 unquote a)))
   `(doc (p "Very interesting test about quasiquotation " 
            (cite/a "https://wiki.call-cc.org/man/5/Module%20scheme#quasiquotation" 
                    "Quasiquotation, Chicken Scheme manual.") 
            " : it shows how " (code/inline "unquote")
            " can be used in a quasiquotation pattern in the " (code/inline "cdr") " slot.")))

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


















