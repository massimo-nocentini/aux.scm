
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

  (define simdjson-get-type (foreign-lambda int "chicken_simdjson_get_type" (c-pointer "chicken_simdjson_dom_element_t")))
  (define simdjson-get-signed-integer (foreign-lambda integer64 "chicken_simdjson_get_signed_integer" (c-pointer "chicken_simdjson_dom_element_t")))
  (define simdjson-get-unsigned-integer (foreign-lambda unsigned-integer64 "chicken_simdjson_get_unsigned_integer" (c-pointer "chicken_simdjson_dom_element_t")))  
  (define simdjson-get-floating-point-number (foreign-lambda double "chicken_simdjson_get_floating_point_number" (c-pointer "chicken_simdjson_dom_element_t")))
  (define simdjson-get-boolean (foreign-lambda scheme-object "chicken_simdjson_get_boolean" (c-pointer "chicken_simdjson_dom_element_t")))
  (define simdjson-get-string (foreign-lambda c-string "chicken_simdjson_get_string" (c-pointer "chicken_simdjson_dom_element_t")))
  (define simdjson-get-array-length (foreign-lambda size_t "chicken_simdjson_get_array_length" (c-pointer "chicken_simdjson_dom_element_t")))
  (define simdjson-get-array-ref (foreign-lambda (c-pointer "chicken_simdjson_dom_element_t") "chicken_simdjson_get_array_ref" (c-pointer "chicken_simdjson_dom_element_t") size_t))
  (define simdjson-get-object-length (foreign-lambda size_t "chicken_simdjson_get_object_length" (c-pointer "chicken_simdjson_dom_element_t")))
  (define simdjson-get-object-ref-key (foreign-lambda symbol "chicken_simdjson_get_object_ref_key" (c-pointer "chicken_simdjson_dom_element_t") size_t))
  (define simdjson-get-object-ref-value (foreign-lambda (c-pointer "chicken_simdjson_dom_element_t") "chicken_simdjson_get_object_ref_value" (c-pointer "chicken_simdjson_dom_element_t") size_t))

  (define simdjson-free (foreign-lambda void "chicken_simdjson_free" (c-pointer "chicken_simdjson_dom_element_t")))

  (define simdjson-parse-ondemand-callback (foreign-lambda (c-pointer "chicken_simdjson_dom_element_t") "chicken_simdjson_parse_ondemand_callback" (const c-string) size_t))
  (define simdjson-load-ondemand-callback (foreign-lambda (c-pointer "chicken_simdjson_dom_element_t") "chicken_simdjson_load_ondemand_callback" (const c-string)))
  
  (define (simdjson->scheme w)
    (let1 (t (simdjson-get-type w))
      (cond
        ((simdjson-signed-integer? t) (simdjson-get-signed-integer w)) 
        ((simdjson-unsigned-integer? t) (simdjson-get-unsigned-integer w))
        ((simdjson-floating-point-number? t) (simdjson-get-floating-point-number w))
        ((simdjson-string? t) (simdjson-get-string w))
        ((simdjson-array? t) (let* ((n (simdjson-get-array-length w)) (vec (make-vector n)))
                                (let loop ((i 0))
                                  (cond
                                    ((< i n) (vector-set! vec i (simdjson->scheme (simdjson-get-array-ref w i))) (loop (add1 i)))
                                    (else vec)))))
        ((simdjson-object? t) (let1 (n (simdjson-get-object-length w))
                                (let loop ((i (sub1 n)) (lst '()))
                                  (cond
                                    ((>= i 0) (let* ((key (simdjson-get-object-ref-key w i))
                                                     (w* (simdjson-get-object-ref-value w i))
                                                     (key* (list key (simdjson->scheme w*)))
                                                     (lst* (cons key* lst)))
                                                (loop (sub1 i) lst*)))
                                    (else lst)))))
        ((simdjson-null? t) (void))
        ((simdjson-boolean? t) (simdjson-get-boolean w))
        (else (error "Unknown simdjson type" t w)))))

  (define (simdjson-load filename) (simdjson->scheme (simdjson-load-ondemand-callback filename)))

  (define (simdjson-load/ondemand filename) 
    (let* ((dom (simdjson-load-ondemand-callback filename))
           (v (simdjson->scheme dom)))
      (simdjson-free dom)
      v))        

  (define (simdjson-parse/ondemand str) 
    (let* ((dom (simdjson-parse-ondemand-callback str (string-length str)))
           (v (simdjson->scheme dom)))
      (simdjson-free dom)
      v))

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









