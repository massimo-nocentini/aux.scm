
(module (aux libc) *

  (import
    scheme 
    (chicken base) 
    (chicken foreign))

  #>

  #include "math.h"

  <#

  (define libc-fma (foreign-lambda double "fma" double double double))
  
  )









