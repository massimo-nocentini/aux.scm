

(import
  (aux base) (aux unittest) 
  scheme (chicken base) (chicken pretty-print) (chicken condition) (chicken foreign) (chicken gc))

#>

C_word C_my_allocate_string (C_word C_k)
{
 char* str = "hello world";
 int length = strlen(str);
 C_word* ptr = C_alloc (C_SIZEOF_STRING (length));
 C_word res = C_string (&ptr, length, str);
 C_kontinue (C_k, res);
 }

extern int callout(int, int, int);

extern C_word C_list_walk(C_word l, C_word p)
{
 if (l == C_SCHEME_END_OF_LIST)
 {
  //C_save(C_SCHEME_END_OF_LIST);
  //C_return(C_callback (p, 1));
  C_return(C_SCHEME_END_OF_LIST);
  }

 C_word cdr = C_list_walk(C_i_cdr (l), p);
 C_word *ptr = C_alloc(C_SIZEOF_PAIR);
 C_word res = C_a_pair(&ptr, C_i_car(l), cdr);
 C_save(res);
 C_return(C_callback (p, 1));
 }

<#

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

  ((test/c_callback _) 
   (let ((witness (gensym))
         (my-strlen (foreign-safe-lambda* scheme-object ((scheme-object f))
                                          #<<END
					  C_word res = C_callback(f, 0);
					  printf("from within a safe lambda\n");
					  C_return (res);
END
                                          )))
     (⊦= witness (car (my-strlen (lambda () (list witness 4)))))))

  ((test/foreign-safe-lambda*/allocate_string/inline _) 
   (let1 (allocate_string (foreign-safe-lambda* scheme-object () 
                                                #<<END
                            char* str = "hello world";
                            C_word length = strlen(str);
                            C_word* ptr = C_alloc (C_SIZEOF_STRING (length));
                            C_word res = C_string (&ptr, length, str);
                            C_return (res);
END
                                                ))
         (⊦= "hello world" (allocate_string))))

  ((test/foreign-primitive/allocate-string _) 
   (let1 (allocate_string (foreign-primitive scheme-object () "C_my_allocate_string(C_k);"))
         (⊦= "hello world" (allocate_string)))
   `(doc (p "An example of " (code/inline "foreign-primitive") 
            " " (cite/a "https://wiki.call-cc.org/man/5/Module%20(chicken%20foreign)#foreign-primitive" 
                        (code/inline "foreign-primitive") ", Chicken Scheme manual.") 
            " that allocates a string in C and returns it to Scheme. "
            "The C code is inlined in the Scheme source code.")))

  ((test/foreign-safe-lambda/list-walk _) 
   (let1 (list-walk (foreign-safe-lambda scheme-object "C_list_walk" scheme-object scheme-object))
         (⊦= '(1 2 3) (list-walk '(1 2 3) identity))))

  ((test/foreign/callout-callin _) 
   (define callout (foreign-safe-lambda int "callout" int int int))
   (define-external (callin (scheme-object xyz)) int
     (print "This is 'callin': " xyz)
     123)
   (⊦= 123 (callout 1 2 3))
   `(doc (p "This test shows how to call out to C code that in turn calls back into Scheme code. "
            "The C code is in the file " (code/inline "bar.c") 
            ", and the function called from Scheme is " (code/inline "callout") 
            ". The function that is called back from C into Scheme is " (code/inline "callin") 
            ". Taken from the Chicken Scheme manual " 
            (cite/a "https://wiki.call-cc.org/man/5/C%20interface#an-example-for-simple-calls-to-foreign-code-involving-callbacks"
                    "An example for simple calls to foreign code involving callbacks, Chicken Scheme manual.") ".")))


  ((test-null-eq? _) (⊨ (eq? '() '())))
  #;((test-null-car _) (⊦⧳ ((exn)) (car (list))))
  )

(unittest/✓ learning-suite)



























