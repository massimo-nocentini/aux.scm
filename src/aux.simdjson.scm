
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

  (define simdjson-parse (foreign-primitive scheme-object (((const c-string) filename))
      "parse_json(C_k, filename);"
  ))
   
  )














#;(import (aux simdjson))








#;(simdjson-parse "twitter.json")
