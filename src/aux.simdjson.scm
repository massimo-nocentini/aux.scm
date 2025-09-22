
(module (aux simdjson) *

  (import
    scheme 
    (chicken base) 
    (chicken foreign) 
    srfi-1
    (aux base))

  #>
  #include "chicken-simdjson.h"
  <#

  (define-external (c_rec (scheme-object l)) scheme-object #f)

  (define simdjson-parse (foreign-primitive scheme-object (((const c-string) filename) (scheme-object l))
      "parse_json(C_k, filename, l);"))
   
  )














#|

(import (aux simdjson))
(simdjson-parse "twitter.json" '(1 2 3))








|#