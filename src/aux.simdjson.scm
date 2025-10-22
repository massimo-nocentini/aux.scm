
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

  (define-foreign-variable simdjson-type-signed-integer int "CHICKEN_SIMDJSON_TYPE_SIGNED_INTEGER")
  (define-foreign-variable simdjson-type-unsigned-integer int "CHICKEN_SIMDJSON_TYPE_UNSIGNED_INTEGER")
  (define-foreign-variable simdjson-type-floating-point-number int "CHICKEN_SIMDJSON_TYPE_FLOATING_POINT_NUMBER")
  (define-foreign-variable simdjson-type-string int "CHICKEN_SIMDJSON_TYPE_STRING")
  (define-foreign-variable simdjson-type-array int "CHICKEN_SIMDJSON_TYPE_ARRAY")
  (define-foreign-variable simdjson-type-object int "CHICKEN_SIMDJSON_TYPE_OBJECT")
  (define-foreign-variable simdjson-type-null int "CHICKEN_SIMDJSON_TYPE_NULL")
  (define-foreign-variable simdjson-type-boolean int "CHICKEN_SIMDJSON_TYPE_BOOLEAN")
  (define-foreign-variable simdjson-type-unknown int "CHICKEN_SIMDJSON_TYPE_UNKNOWN")

  (define (simdjson-signed-integer? t) (= t simdjson-type-signed-integer))
  (define (simdjson-unsigned-integer? t) (= t simdjson-type-unsigned-integer))
  (define (simdjson-floating-point-number? t) (= t simdjson-type-floating-point-number))
  (define (simdjson-string? t) (= t simdjson-type-string))
  (define (simdjson-array? t) (= t simdjson-type-array))
  (define (simdjson-object? t) (= t simdjson-type-object))
  (define (simdjson-null? t) (= t simdjson-type-null))
  (define (simdjson-boolean? t) (= t simdjson-type-boolean))
  (define (simdjson-unknown? t) (= t simdjson-type-unknown))

  (define simdjson-get-signed-integer (foreign-lambda integer64 "chicken_simdjson_get_signed_integer" c-pointer))
  (define simdjson-get-unsigned-integer (foreign-lambda size_t "chicken_simdjson_get_unsigned_integer" c-pointer))
  (define simdjson-get-floating-point-number (foreign-lambda double "chicken_simdjson_get_floating_point_number" c-pointer))
  (define simdjson-get-boolean (foreign-lambda scheme-object "chicken_simdjson_get_boolean" c-pointer))
  (define simdjson-get-string (foreign-primitive ((c-pointer ptr)) "chicken_simdjson_get_string(ptr, C_k);"))
  (define simdjson-get-array (foreign-safe-lambda* void ((c-pointer ptr) (scheme-object g)) "chicken_simdjson_get_array(ptr, g, C_k);"))
  (define simdjson-get-object (foreign-safe-lambda* void ((c-pointer ptr) (scheme-object g)) "chicken_simdjson_get_object(ptr, g, C_k);"))
  (define simdjson-get-type (foreign-lambda int "chicken_simdjson_get_type" c-pointer))
  (define simdjson-parse-ondemand-callback (foreign-safe-lambda* void (((const c-string) str) (size_t l) (scheme-object f)) "chicken_simdjson_parse_ondemand_callback(str, l, f, C_k);"))
  (define simdjson-load-ondemand-callback (foreign-safe-lambda* void (((const c-string) filename) (scheme-object f)) "chicken_simdjson_load_ondemand_callback(filename, f, C_k);"))
  
  (define (simdjson->scheme w)
    (let1 (t (simdjson-get-type w))
      (cond
        ((simdjson-signed-integer? t) (simdjson-get-signed-integer w)) 
        ((simdjson-unsigned-integer? t) (simdjson-get-unsigned-integer w))
        ((simdjson-floating-point-number? t) (simdjson-get-floating-point-number w))
        ((simdjson-string? t) (simdjson-get-string w))
        ((simdjson-array? t) (simdjson-get-array w simdjson->scheme))
        ((simdjson-object? t) (reverse (simdjson-get-object w simdjson->scheme)))
        ((simdjson-null? t) (void))
        ((simdjson-boolean? t) (simdjson-get-boolean w))
        (else (error "Unknown simdjson type" t w)))))

  #;(define (simdjson-load filename)
    (let1 (P (foreign-safe-lambda scheme-object "chicken_simdjson_load" 
                                  (const c-string) 
                                  scheme-object 
                                  scheme-object
                                  scheme-object
                                  scheme-object 
                                  scheme-object
                                  scheme-object))
          (P filename list identity make-vector cons (λ (v i x) (vector-set! v i x) v) reverse)))

  (define (simdjson-load filename) (simdjson-load-ondemand-callback filename simdjson->scheme))

  #;(define (simdjson-load/ondemand filename)
    (let1 (P (foreign-safe-lambda scheme-object "chicken_simdjson_load_ondemand" 
                                  (const c-string) 
                                  scheme-object 
                                  scheme-object
                                  scheme-object
                                  scheme-object 
                                  scheme-object
                                  scheme-object))
          (P filename list identity make-vector cons (λ (v i x) (vector-set! v i x) v) reverse)))

  #;(define (simdjson-parse/ondemand str)
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

  (define (simdjson-load/ondemand filename) (simdjson-load-ondemand-callback filename simdjson->scheme))
  (define (simdjson-parse/ondemand str) (simdjson-parse-ondemand-callback str (string-length str) simdjson->scheme))

  (define (->string/json w)
    (letport/output-string port
                           (let J ((v w))
                             (cond
                               ((number? v) (display v port))
                               ((null? v) (display "{}" port))
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
                               (else (warning "Cannot convert to JSON" v) (display "\"<unknown>\"" port))))))

  )









