
(import (aux category monad) (aux category writer) (aux category list))

(module (aux category) *

  (import scheme (chicken base) (aux base))

  (define (writer-log/list x) `(writer ((got ,x)) ,x))
  (define-syntax-rule (do/monad/writer (bind var expr) ...) (do/monad (bind var (writer-log/list expr)) ...))

)

(module (aux category monad list) = ((aux category monad) (aux category list)))
(module (aux category writer list) = ((aux category writer) (aux category list)))
(module (aux category monad writer list) = ((aux category monad) (aux category writer list)))
