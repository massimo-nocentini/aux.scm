

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

Word_t Value;                    // pointer to array element value
JLMU(Value, PJLArray);

C_kontinue (C_k, C_fix(Value));

TAG
))

  (define JFA (foreign-primitive ((c-pointer PJLArray)) #<<TAG

Word_t Value;                    // pointer to array element value
JLFA(Value, PJLArray);

C_word *ptr = C_alloc(C_SIZEOF_POINTER);
C_word res = C_mpointer(&ptr, PJLArray);

C_word av[4] = { C_SCHEME_UNDEFINED, C_k, res, C_fix(Value) };
C_values(4, av);

TAG
))


  (define JF (foreign-primitive ((c-pointer PJLArray) (scheme-object k)) #<<TAG

Word_t Index = k;
Word_t *PValue;                    // pointer to array element value
JLF(PValue, PJLArray, Index);

C_word key = C_SCHEME_UNDEFINED;
C_word value = C_SCHEME_UNDEFINED;
C_word handle = C_SCHEME_UNDEFINED;
C_word flag = C_SCHEME_FALSE;
if (PValue != NULL) {  
  flag = C_SCHEME_TRUE;
  key = Index;
  value = *PValue;
  C_word *ptr = C_alloc(C_SIZEOF_POINTER);
  handle = C_mpointer(&ptr, PValue);
}               

C_word av[6] = { C_SCHEME_UNDEFINED, C_k, flag, key, value, handle };
C_values(6, av);

TAG
))


  (define JN (foreign-primitive ((c-pointer PJLArray) (scheme-object k) (c-pointer h)) #<<TAG

Word_t Index = k;
Word_t *PValue = h;                    // pointer to array element value
JLN(PValue, PJLArray, Index);

C_word key = C_SCHEME_UNDEFINED;
C_word value = C_SCHEME_UNDEFINED;
C_word handle = C_SCHEME_UNDEFINED;
C_word flag = C_SCHEME_FALSE;
if (PValue != NULL) {  
  flag = C_SCHEME_TRUE;
  key = Index;
  value = *PValue;
  C_word *ptr = C_alloc(C_SIZEOF_POINTER);
  handle = C_mpointer(&ptr, PValue);
}               

C_word av[6] = { C_SCHEME_UNDEFINED, C_k, flag, key, value, handle };
C_values(6, av);

TAG
))


  (define JL (foreign-primitive ((c-pointer PJLArray) (scheme-object k)) #<<TAG

Word_t Index = k;
Word_t *PValue;                    // pointer to array element value
JLL(PValue, PJLArray, Index);

C_word key = C_SCHEME_UNDEFINED;
C_word value = C_SCHEME_UNDEFINED;
C_word handle = C_SCHEME_UNDEFINED;
C_word flag = C_SCHEME_FALSE;
if (PValue != NULL) {  
  flag = C_SCHEME_TRUE;
  key = Index;
  value = *PValue;
  C_word *ptr = C_alloc(C_SIZEOF_POINTER);
  handle = C_mpointer(&ptr, PValue);
}               

C_word av[6] = { C_SCHEME_UNDEFINED, C_k, flag, key, value, handle };
C_values(6, av);

TAG
))


  (define JP (foreign-primitive ((c-pointer PJLArray) (scheme-object k) (c-pointer h)) #<<TAG

Word_t Index = k;
Word_t *PValue = h;                    // pointer to array element value
JLP(PValue, PJLArray, Index);

C_word key = C_SCHEME_UNDEFINED;
C_word value = C_SCHEME_UNDEFINED;
C_word handle = C_SCHEME_UNDEFINED;
C_word flag = C_SCHEME_FALSE;
if (PValue != NULL) {  
  flag = C_SCHEME_TRUE;
  key = Index;
  value = *PValue;
  C_word *ptr = C_alloc(C_SIZEOF_POINTER);
  handle = C_mpointer(&ptr, PValue);
}               

C_word av[6] = { C_SCHEME_UNDEFINED, C_k, flag, key, value, handle };
C_values(6, av);

TAG
))

)


(functor ((aux judy) (J (JI JG JC JMU JFA JF JN JL JP))) *

  (import scheme (chicken base) (chicken foreign) (chicken gc) (chicken memory) (chicken pretty-print) (aux base) J)

  (define-record judy-array handle)

  (define-record-printer judy-array
    (pretty-printer/port (λ (ja) 
                          `((handle ,(judy-array-handle ja)) 
                            (bytes ,(judy-array-bytes ja))
                            (size ,(judy-array-size ja))
                            (alist ,(judy-array->alist ja))))))

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

  (define (judy-array-bytes ja) (JMU (judy-array-handle ja)))

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
      (judy-array-walk/backward ja (λ (index value) (push! (list index value) result)))
      result))

  (define ((judy-array-walk init init_key next) ja w) 
    (let1 (ja-handle (judy-array-handle ja))
      (let-values (((flag key value handle) (init ja-handle init_key)))
        (let loop ((flag flag) (key key) (value value) (handle handle))
          (when flag
            (w key value)
            (let-values (((flag key value handle) (next ja-handle key handle)))
              (loop flag key value handle)))))))
    
  (define judy-array-walk/forward (judy-array-walk JF 0 JN))
  (define judy-array-walk/backward (judy-array-walk JL -1 JP))
)

(module (aux judyL) = ((aux judy) (aux judy l)))

