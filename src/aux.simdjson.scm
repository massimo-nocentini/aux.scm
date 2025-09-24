
(module (aux simdjson) *

  (import
    scheme 
    (chicken base) 
    (chicken foreign) 
    (chicken pretty-print)
    srfi-1
    (aux base))

  #>

  #include "chicken-simdjson.h"

  <#

  (define (simdjson-load filename)
    (let1 (P (foreign-safe-lambda scheme-object "chicken_simdjson_load" 
                                  (const c-string) 
                                  scheme-object 
                                  scheme-object
                                  scheme-object
                                  scheme-object 
                                  scheme-object
                                  scheme-object))
          (P filename list identity make-vector cons vector-set! reverse)))

  (define (simdjson-load/ondemand filename)
    (let1 (P (foreign-safe-lambda scheme-object "chicken_simdjson_load_ondemand" 
                                  (const c-string) 
                                  scheme-object 
                                  scheme-object
                                  scheme-object
                                  scheme-object 
                                  scheme-object
                                  scheme-object))
          (P filename list identity make-vector cons vector-set! reverse)))
 

  )





