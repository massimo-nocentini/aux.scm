

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


)


(functor ((aux judy) (J (JI JG JC JMU JFA))) *

  (import scheme (chicken base) (chicken foreign) (chicken gc) (chicken memory) (chicken pretty-print) (aux base) J)

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
)

(module (aux judyL) = ((aux judy) (aux judy l)))






