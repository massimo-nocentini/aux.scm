

#>

#include <Judy.h>

<#

(module (aux judy l) *

  (import
    scheme 
    (chicken base) 
    (chicken foreign))


  (define JI (foreign-primitive ((c-pointer PJLArray) (scheme-object Index) (scheme-object Value)) #<<TAG

Word_t * PValue;                    // pointer to array element value
JLI(PValue, PJLArray, Index);
C_word res = C_SCHEME_UNDEFINED;
if (PValue == PJERR) res = C_fix(PJERR); 
else {
  *PValue = Value;  // store new value
  C_word *ptr = C_alloc(C_SIZEOF_POINTER);
  res = C_mpointer(&ptr, PJLArray);
}               

C_kontinue (C_k, res);

TAG
))

  (define JG (foreign-primitive ((c-pointer PJLArray) (scheme-object Index)) #<<TAG

Word_t * PValue;                    // pointer to array element value
JLG(PValue, PJLArray, Index);
C_word res = C_SCHEME_UNDEFINED;
C_word flag = C_SCHEME_FALSE;
if (PValue != NULL) {  
  flag = C_SCHEME_TRUE;
  res = *PValue;
}               

C_word av[4] = { C_SCHEME_UNDEFINED, C_k, flag, res };
C_values(4, av);

TAG
))

  (define JC (foreign-primitive ((c-pointer PJLArray) (scheme-object from) (scheme-object to)) #<<TAG

Word_t PValue;                    // pointer to array element value
JLC(PValue, PJLArray, from, to);

C_kontinue (C_k, C_fix(PValue));

TAG
))


  (define JMU (foreign-primitive ((c-pointer PJLArray)) #<<TAG

Word_t PValue;                    // pointer to array element value
JLMU(PValue, PJLArray);

C_kontinue (C_k, C_fix(PValue));

TAG
))

  (define JFA (foreign-primitive ((c-pointer PJLArray)) #<<TAG

Word_t PValue;                    // pointer to array element value
JLFA(PValue, PJLArray);

C_word *ptr = C_alloc(C_SIZEOF_POINTER);
C_word res = C_mpointer(&ptr, PJLArray);

C_word av[4] = { C_SCHEME_UNDEFINED, C_k, res, C_fix(PValue) };
C_values(4, av);

TAG
))

 (define JW (foreign-safe-lambda* scheme-object ((c-pointer PJLArray) (scheme-object walk)) #<<TAG

Word_t Index = 0;
Word_t *PValue;

JLF(PValue, PJLArray, Index);

while (PValue != NULL)
{    
    C_word v = *PValue;
    C_save(v);
    C_save(Index);
    C_callback(walk, 2);

    JLN(PValue, PJLArray, Index);
}

C_return (C_SCHEME_UNDEFINED);

TAG
))


 (define JWr (foreign-safe-lambda* scheme-object ((c-pointer PJLArray) (scheme-object walk)) #<<TAG

Word_t Index = -1;
Word_t *PValue;

JLL(PValue, PJLArray, Index);

while (PValue != NULL)
{
    C_word v = *PValue;
    C_save(v);
    C_save(Index);    
    C_callback(walk, 2);

    JLP(PValue, PJLArray, Index);
}

C_return (C_SCHEME_UNDEFINED);

TAG
))


)


(functor ((aux judy) (J (JI JG JC JMU JFA JW JWr))) *

  (import scheme (chicken base) (chicken foreign) (chicken gc) (chicken memory) (chicken pretty-print) (aux base) J)

  (define-record judy-array handle)

  (define-record-printer judy-array
    (λ (ja port)
      (pretty-print `((handle ,(judy-array-handle ja)) 
                      (bytes ,(judy-array-memory-used-in-bytes ja))
                      (size ,(judy-array-size ja))
                      (alist ,(judy-array->alist ja))) port)))

  (define make-null-pointer (foreign-primitive () #<<TAG
  
  C_word *ptr = C_alloc(C_SIZEOF_POINTER);
  C_word p = C_mpointer(&ptr, NULL);
  C_kontinue (C_k, p);

TAG
))

  (define (judy-array-empty) 
    (let1 (ja (make-judy-array (make-null-pointer)))
      (set-finalizer! ja judy-array-free)
      ja))

  (define (judy-array-set! ja index value)
    (let1 (res (JI (judy-array-handle ja) index value))
      (cond
        ((pointer? res) (judy-array-handle-set! ja res))
        (else (error "judy-array-set! failed with error code" res)))))

  (define (judy-array-ref/default ja index default) 
    (let-values (((flag res) (JG (judy-array-handle ja) index)))
      (if flag res (default index))))

  (define (judy-array-ref ja index) 
    (judy-array-ref/default ja index (λ (i) (error "judy-array-ref: index not found" i))))

  (define (judy-array-size ja) (JC (judy-array-handle ja) 0 -1))

  (define (judy-array-memory-used-in-bytes ja) (JMU (judy-array-handle ja)))

  (define (judy-array-free ja) 
    (let-values (((handle bytes) (JFA (judy-array-handle ja))))
      (judy-array-handle-set! ja handle)
      bytes))

  (define (alist->judy-array alist)
    (let1 (ja (judy-array-empty))
      (for-each (λ (pair) (judy-array-set! ja (car pair) (cadr pair))) alist)
      ja))

  (define (list->judy-array lst)
    (let ((ja (judy-array-empty))
          (i 0))
        (for-each (λ (v) (judy-array-set! ja i v) (add1! i)) lst)
        ja))

  (define (judy-array->alist ja)
    (let1 (result '())
      (judy-array-walk-reverse ja (λ (index value) (push! (list index value) result)))
      result))

  (define (judy-array-walk ja w) (JW (judy-array-handle ja) w))
  (define (judy-array-walk-reverse ja w) (JWr (judy-array-handle ja) w))
)

(module (aux judyL) = ((aux judy) (aux judy l)))


#|

(import srfi-1 (aux base) (aux judy) (aux judyL))
(define a (alist->judy-array '((0 "zero") (1 "one") (2 "two") (10000 "ten thousand"))))
(define a (alist->judy-array '((0 'zero) (1 'one) (2 'two) (10000 'ten-thousand))))
(judy-array-free a)
(judy-array-ref a 0)
(judy-array->alist a)
(judy-array-memory-used-in-bytes a)
(judy-array-walk a (λ (i v) (when (= (modulo i 10000) 0) (display (list i v)))))
|#



