
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

  (define-foreign-variable chicken-simdjson-type-unknown int "CHICKEN_SIMDJSON_TYPE_UNKNOWN")
  (define-foreign-variable chicken-simdjson-type-signed-integer int "CHICKEN_SIMDJSON_TYPE_SIGNED_INTEGER")
  (define-foreign-variable chicken-simdjson-type-unsigned-integer int "CHICKEN_SIMDJSON_TYPE_UNSIGNED_INTEGER")
  (define-foreign-variable chicken-simdjson-type-floating-point-number int "CHICKEN_SIMDJSON_TYPE_FLOATING_POINT_NUMBER")
  (define-foreign-variable chicken-simdjson-type-string int "CHICKEN_SIMDJSON_TYPE_STRING")
  (define-foreign-variable chicken-simdjson-type-array int "CHICKEN_SIMDJSON_TYPE_ARRAY")
  (define-foreign-variable chicken-simdjson-type-object int "CHICKEN_SIMDJSON_TYPE_OBJECT")
  (define-foreign-variable chicken-simdjson-type-null int "CHICKEN_SIMDJSON_TYPE_NULL")
  (define-foreign-variable chicken-simdjson-type-boolean int "CHICKEN_SIMDJSON_TYPE_BOOLEAN")

  ; (define simdjson-signed-integer? (foreign-safe-lambda scheme-object "chicken_simdjson_is_signed_integer" c-pointer))
  ; (define simdjson-unsigned-integer? (foreign-safe-lambda scheme-object "chicken_simdjson_is_unsigned_integer" c-pointer))
  ; (define simdjson-floating-point-number? (foreign-safe-lambda scheme-object "chicken_simdjson_is_floating_point_number" c-pointer))
  ; (define simdjson-string? (foreign-safe-lambda scheme-object "chicken_simdjson_is_string" c-pointer))
  ; (define simdjson-array? (foreign-safe-lambda scheme-object "chicken_simdjson_is_array" c-pointer))
  ; (define simdjson-object? (foreign-safe-lambda scheme-object "chicken_simdjson_is_object" c-pointer))
  ; (define simdjson-null? (foreign-safe-lambda scheme-object "chicken_simdjson_is_null" c-pointer))
  ; (define simdjson-boolean? (foreign-safe-lambda scheme-object "chicken_simdjson_is_boolean" c-pointer))
  ; (define simdjson-unknown? (foreign-safe-lambda scheme-object "chicken_simdjson_is_unknown" c-pointer))

  (define (simdjson-signed-integer? t) (= t chicken-simdjson-type-signed-integer))
  (define (simdjson-unsigned-integer? t) (= t chicken-simdjson-type-unsigned-integer))
  (define (simdjson-floating-point-number? t) (= t chicken-simdjson-type-floating-point-number))
  (define (simdjson-string? t) (= t chicken-simdjson-type-string))
  (define (simdjson-array? t) (= t chicken-simdjson-type-array))
  (define (simdjson-object? t) (= t chicken-simdjson-type-object))
  (define (simdjson-null? t) (= t chicken-simdjson-type-null))
  (define (simdjson-boolean? t) (= t chicken-simdjson-type-boolean))
  (define (simdjson-unknown? t) (= t chicken-simdjson-type-unknown))

  (define simdjson-get-signed-integer (foreign-lambda integer64 "chicken_simdjson_get_signed_integer" c-pointer))
  (define simdjson-get-unsigned-integer (foreign-lambda unsigned-integer64 "chicken_simdjson_get_unsigned_integer" c-pointer))
  (define simdjson-get-floating-point-number (foreign-lambda double "chicken_simdjson_get_floating_point_number" c-pointer))
  (define simdjson-get-boolean (foreign-lambda scheme-object "chicken_simdjson_get_boolean" c-pointer))
  (define simdjson-get-string (foreign-lambda (const c-string) "chicken_simdjson_get_string" c-pointer))
  ; (define simdjson-get-array-count-elements (foreign-safe-lambda size_t "chicken_simdjson_get_array_count_elements" c-pointer))
  (define simdjson-get-array (foreign-safe-lambda scheme-object "chicken_simdjson_get_array" c-pointer scheme-object))
  (define simdjson-get-object (foreign-safe-lambda scheme-object "chicken_simdjson_get_object" c-pointer scheme-object))
  (define simdjson-parse-ondemand-callback (foreign-safe-lambda scheme-object "chicken_simdjson_parse_ondemand_callback" c-string size_t scheme-object))
  (define simdjson-get-raw-json-string (foreign-lambda (const c-string) "chicken_simdjson_get_raw_json_string" c-pointer))
  (define simdjson-get-type (foreign-lambda int "chicken_simdjson_get_type" c-pointer))

  (define (simdjson->scheme w)
    (let1 (t (simdjson-get-type w))
      (cond
        ((simdjson-signed-integer? t) (simdjson-get-signed-integer w)) 
        ((simdjson-unsigned-integer? t) (simdjson-get-unsigned-integer w))
        ((simdjson-floating-point-number? t) (simdjson-get-floating-point-number w))
        ((simdjson-string? t) (simdjson-get-string w))
        ((simdjson-array? t) (simdjson-get-array w (λ (vec i w*) (vector-set! vec i (simdjson->scheme w*)) vec)))
        ((simdjson-object? t) (reverse (simdjson-get-object w (λ (lst k w*) (cons (list k (simdjson->scheme w*)) lst)))))
        ((simdjson-null? t) (void))
        ((simdjson-boolean? t) (simdjson-get-boolean w))
        ((simdjson-unknown? t) (warning "Unknown simdjson type handled" (simdjson-get-raw-json-string w)) w)
        (else (warning "Unknown simdjson type" #;(simdjson-get-raw-json-string w)) 'error))))

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









