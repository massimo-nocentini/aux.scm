
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

  (define (simdjson-parse filename)
    (let ((P (foreign-safe-lambda scheme-object "chicken_simdjson_load" (const c-string) scheme-object scheme-object scheme-object))
          (callback-object list))
    (P filename callback-object identity vector-set!)))


  )














#|

(import (aux base) (aux simdjson))

#;(sc '(1 2 3))

(define stack '())

(simdjson-parse "twitter.json")

stack







|#