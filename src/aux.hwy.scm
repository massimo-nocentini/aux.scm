
(module (aux hwy) *

  (import
    scheme 
    (chicken base) 
    (chicken foreign) 
    (chicken pretty-print)
    srfi-1 srfi-4
    (aux base))

  #>

  #include "chicken-hwy.h"

  <#
  
  (define chicken-hwy-vqsort-int32 (foreign-lambda void "chicken_hwy_vqsort_int32" s32vector size_t bool))
  (define chicken-hwy-vqsort-uint32 (foreign-lambda void "chicken_hwy_vqsort_uint32" u32vector size_t bool))
  (define chicken-hwy-vqsort-int64 (foreign-lambda void "chicken_hwy_vqsort_int64" s64vector size_t bool))
  (define chicken-hwy-vqsort-uint64 (foreign-lambda void "chicken_hwy_vqsort_uint64" u64vector size_t bool))
  (define chicken-hwy-vqsort-f32 (foreign-lambda void "chicken_hwy_vqsort_f32" f32vector size_t bool))
  (define chicken-hwy-vqsort-f64 (foreign-lambda void "chicken_hwy_vqsort_f64" f64vector size_t bool))
  
  (define (vqsort! vec ascending?)
    (cond 
      ((s32vector? vec) (chicken-hwy-vqsort-int32 vec (s32vector-length vec) ascending?))
      ((u32vector? vec) (chicken-hwy-vqsort-uint32 vec (u32vector-length vec) ascending?))
      ((s64vector? vec) (chicken-hwy-vqsort-int64 vec (s64vector-length vec) ascending?))
      ((u64vector? vec) (chicken-hwy-vqsort-uint64 vec (u64vector-length vec) ascending?))
      ((f32vector? vec) (chicken-hwy-vqsort-f32 vec (f32vector-length vec) ascending?))
      ((f64vector? vec) (chicken-hwy-vqsort-f64 vec (f64vector-length vec) ascending?))
      (else (error "vqsort!: unsupported vector type" vec)))
    (void))

  )





#|

(import (chicken base) (chicken memory) (chicken blob) (aux base) (aux hwy) srfi-1 srfi-4 (chicken sort))
(define lst '(5 3 8 1 4 7 -2 6))
(display lst) 
(define v #s64(5 3 8 1 4 7 -2 6))
(define vi (iota 100000000))
(define v (list->s64vector vi))
,t (vqsort! v #f)
,t (sort vi >)
v
(define sorted-desc (vqsort! #f64(5.0 3 8 1 4 7 -2 6) #f))
(display sorted-desc)
(newline)
(define sorted-desc (vqsort! #s64(5 3 8 1 4 7 -2 6) #f))
(display sorted-desc)


|#




