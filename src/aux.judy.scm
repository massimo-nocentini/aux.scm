

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
  
)

(functor ((aux judy) (J (JI JG))) *

  (import scheme (chicken base) (chicken foreign) (chicken memory) (chicken pretty-print) (aux base) J)

  (define-record judy-array handle)

  (define-record-printer judy-array
    (λ (ja port)
      (pretty-print `(handle ,(judy-array-handle ja)) port)))

  (define make-null-pointer (foreign-primitive () #<<TAG
  
  C_word *ptr = C_alloc(C_SIZEOF_POINTER);
  C_word p = C_mpointer(&ptr, NULL);
  C_kontinue (C_k, p);

TAG
))

  (define (judy-array-empty) (make-judy-array (make-null-pointer)))
  (define (judy-array-set! ja index value) (let1 (res (JI (judy-array-handle ja) index value))
                                            (cond
                                              ((pointer? res) (judy-array-handle-set! ja res))
                                              (else (error "judy-array-set! failed with error code" res)))))
  (define (judy-array-ref ja index) (let-values (((flag res) (JG (judy-array-handle ja) index)))
                                 (cond
                                   (flag res)
                                   (else #f))))

  )

(module (aux judyL) = ((aux judy) (aux judy l)))






