
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
  
  (define chicken-hwy-vqsort-int32-asc (foreign-lambda void "chicken_hwy_vqsort_int32_asc" s32vector size_t))
  (define chicken-hwy-vqsort-int32-desc (foreign-lambda void "chicken_hwy_vqsort_int32_desc" s32vector size_t))
  (define chicken-hwy-vqsort-uint32-asc (foreign-lambda void "chicken_hwy_vqsort_uint32_asc" u32vector size_t))
  (define chicken-hwy-vqsort-uint32-desc (foreign-lambda void "chicken_hwy_vqsort_uint32_desc" u32vector size_t))
  (define chicken-hwy-vqsort-int64-asc (foreign-lambda void "chicken_hwy_vqsort_int64_asc" s64vector size_t))
  (define chicken-hwy-vqsort-int64-desc (foreign-lambda void "chicken_hwy_vqsort_int64_desc" s64vector size_t))
  (define chicken-hwy-vqsort-uint64-asc (foreign-lambda void "chicken_hwy_vqsort_uint64_asc" u64vector size_t))
  (define chicken-hwy-vqsort-uint64-desc (foreign-lambda void "chicken_hwy_vqsort_uint64_desc" u64vector size_t))
  (define chicken-hwy-vqsort-f32-asc (foreign-lambda void "chicken_hwy_vqsort_f32_asc" f32vector size_t))
  (define chicken-hwy-vqsort-f32-desc (foreign-lambda void "chicken_hwy_vqsort_f32_desc" f32vector size_t))
  (define chicken-hwy-vqsort-f64-asc (foreign-lambda void "chicken_hwy_vqsort_f64_asc" f64vector size_t))
  (define chicken-hwy-vqsort-f64-desc (foreign-lambda void "chicken_hwy_vqsort_f64_desc" f64vector size_t))
  
  (define (vqsort! vec ascending?)
    (cond 
      ((s32vector? vec)
       (let ((len (s32vector-length vec)))
         (if ascending?
             (chicken-hwy-vqsort-int32-asc vec len)
             (chicken-hwy-vqsort-int32-desc vec len))))
      ((u32vector? vec)
       (let ((len (u32vector-length vec)))
         (if ascending?
             (chicken-hwy-vqsort-uint32-asc vec len)
             (chicken-hwy-vqsort-uint32-desc vec len))))
      ((s64vector? vec)
       (let ((len (s64vector-length vec)))
         (if ascending?
             (chicken-hwy-vqsort-int64-asc vec len)
             (chicken-hwy-vqsort-int64-desc vec len))))
      ((u64vector? vec)
       (let ((len (u64vector-length vec)))
         (if ascending?
             (chicken-hwy-vqsort-uint64-asc vec len)
             (chicken-hwy-vqsort-uint64-desc vec len))))
      ((f32vector? vec)
       (let ((len (f32vector-length vec)))
         (if ascending?
             (chicken-hwy-vqsort-f32-asc vec len)
             (chicken-hwy-vqsort-f32-desc vec len))))
      ((f64vector? vec)
       (let ((len (f64vector-length vec)))
         (if ascending?
             (chicken-hwy-vqsort-f64-asc vec len)
             (chicken-hwy-vqsort-f64-desc vec len))))
      (else (error "vqsort: unsupported vector " vec)))
    (void))

  )





#|

(import (chicken base) (chicken memory) (chicken blob) (aux base) (aux hwy) srfi-4)
(define lst '(5 3 8 1 4 7 -2 6))
(display lst) (vqsort! 4 #f)
(define sorted-desc (vqsort! #f64(5.0 3 8 1 4 7 -2 6) #f))
(display sorted-desc)
(newline)
(define sorted-desc (vqsort! #s64(5 3 8 1 4 7 -2 6) #f))
(display sorted-desc)


|#




