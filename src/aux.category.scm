

(module (aux category) *

  (import scheme (chicken base) (aux base))

  (define (writer/log x) `(writer ((got ,x)) ,x))

)
