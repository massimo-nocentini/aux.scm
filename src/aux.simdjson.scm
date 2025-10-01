
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
          (P filename list identity make-vector cons (λ (v i x) (vector-set! v i x) v) reverse)))

  (define (simdjson-load/ondemand filename)
    (let1 (P (foreign-safe-lambda scheme-object "chicken_simdjson_load_ondemand" 
                                  (const c-string) 
                                  scheme-object 
                                  scheme-object
                                  scheme-object
                                  scheme-object 
                                  scheme-object
                                  scheme-object))
          (P filename list identity make-vector cons (λ (v i x) (vector-set! v i x) v) reverse)))

  (define (simdjson-parse/ondemand str)
    (let1 (P (foreign-safe-lambda scheme-object "chicken_simdjson_parse_ondemand" 
                                  (const c-string) 
                                  size_t 
                                  scheme-object 
                                  scheme-object
                                  scheme-object
                                  scheme-object 
                                  scheme-object
                                  scheme-object))
          (P str (string-length str) list identity make-vector cons (λ (v i x) (vector-set! v i x) v) reverse)))

  (define (->string/json w)
    (letport/output-string port
                           (let J ((v w))
                             (cond
                               ((number? v) (display v port))
                               ((string? v) (write v port))
                               ((boolean? v) (display (if v "true" "false") port))
                               ((void? v) (display "null" port))
                               ((vector? v) (begin (display "[" port)
                                              (let loop ((i 0))
                                                (when (< i (vector-length v))
                                                  (when (> i 0) (display "," port))
                                                  (J (vector-ref v i))
                                                  (loop (add1 i))))
                                              (display "]" port)))
                               ((pair? v) (begin (display "{" port)
                                            (let loop ((lst v) (first? #t))
                                              (when (pair? lst)
                                                (when (not first?) (display "," port))
                                                (let* ((keyvalue (car lst))
                                                       (key (car keyvalue))
                                                       (val (cadr keyvalue)))
                                                  (if (or (string? key) (symbol? key))
                                                      (begin
                                                        (display "\"" port)
                                                        (display key port)
                                                        (display "\":" port)
                                                        (J val))
                                                      (error "JSON object keys must be strings")))
                                                (loop (cdr lst) #f)))
                                            (display "}" port)))
                               (else (error "Cannot convert to JSON" v))))))

  )









