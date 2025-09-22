
(module (aux simdjson) *

  (import
    scheme 
    (chicken base) 
    (chicken foreign) 
    srfi-1
    (aux base))

  #>
  #include "chicken-simdjson.h"
  extern C_word c(C_word l);
  <#

  

  ; (define sc (foreign-safe-lambda scheme-object "c" scheme-object))

  ; (define-external (c_rec (scheme-object l)) scheme-object (sc l))

  #;(define simdjson-parse (foreign-primitive scheme-object (((const c-string) filename) (scheme-object l) (scheme-object p))
      "parse_json(C_k, filename, l, p);"))
   
(define simdjson-parse (foreign-safe-lambda scheme-object "parse_json" scheme-object (const c-string) scheme-object scheme-object))
   

  )


 











#|

(import (aux base) (aux simdjson))

#;(sc '(1 2 3))

(define stack '())

(simdjson-parse #t "twitter.json" '(1 2 3) (λ (x) #;(display x) #;(push! x stack) (list x)))

stack







|#