
(import (chicken port) (chicken sort) (only (scheme base) open-output-string get-output-string) srfi-69 (aux unittest) (aux base) (aux commons))

; the strict version of the old `test-error`: fails when `expr` raises nothing.
(define-syntax-rule (⊦error expr) (⊨ (condition-case (begin expr #f) ((exn) #t))))

; from on-scheme learning-test: tests of the CHICKEN language itself.
(define-suite learning-chicken-suite

  ((test/booleans _)
   (⊦= 'fail (if #f 'succeed 'fail))
   (⊦= 'succeed (if '() 'succeed 'fail))
   (⊦= 'succeed (if `(,#f ,#f) 'succeed 'fail))
   (⊦= 'else (cond
               ((and #t #f) => (λ (y) #t))
               (else 'else))))

  ((test/hash-table _)
   (let1 (H (make-hash-table))
     (hash-table-set! H 'hello 'world)
     (⊦= #t (hash-table-exists? H 'hello)))
   (let1 (H (make-hash-table))
     (hash-table-set! H 'hello 'world)
     (hash-table-set! H 'hello 'new-world)
     (⊦= 'new-world (hash-table-ref H 'hello)))
   (⊦= '(3 4 5)
       (sort
         (hash-table-fold
           (alist->hash-table '((a . 3) (b . 4) (c . 5)))
           (λ (k v acc) (cons v acc)) '())
         <)))

  ((test/output-ports _)
   (let* ((str-port (open-output-string))
          (result (with-output-to-port str-port
                    (λ ()
                      (display "hello world")
                      "succeed"))))
     (⊦= "hello world" (get-output-string str-port))
     (⊦= "succeed" result))
   (call+stdout
     (λ ()
       (display "hello world")  ; to be redirected into a collecting string
       '(hello world))          ; to be used as return value
     (λ (r s)
       (⊨ (and (equal? '(hello world) r) (equal? "hello world" s)))))
   (⊦= "succeed" (with-output-to-string (λ ()
                                          (display 'succeed)
                                          #t))) ; `with-output-to-string` discards the return value
   (⊦= "hello-world" (call-with-output-string (λ (port)
                                                (display 'hello-world port)
                                                #t)))) ; `call-with-output-string` discards the return value

  ; from group MAPPING: implementation-specific behaviour of `values` in a one-value context.
  ((test/multiple-values _)
   (⊦= '(hello world) (call-with-values (λ () (values 'hello 'world)) identity*))
   (⊦= 'succeed (values 'succeed))
   (⊦= 'fail (values 'fail 'succeed)))

  ((test/keyword-arguments _)
   (define-syntax add-key-args
     (syntax-rules ()
       ((_ name) (define name (lambda (#!key (hello 0)) (add1 hello))))))
   (add-key-args f)
   (define g
     (lambda (#!key (hello 0))
       (add1 hello)))
   (⊦= 1 (f))
   (⊦= 4 (f hello: 3))
   (⊦= 1 (f hello₁: 3))
   (⊦error (identity 0 hello: 3))
   (⊦= 1 (g))
   (⊦= 4 (g hello: 3))
   (⊦= 1 (g hello₁: 3)))

  ((test/eval-ignores-lexical-scope _)
   (let1 (x 4)
     (⊦error (eval 'x))))

  )

(unittest/✓ learning-chicken-suite)
