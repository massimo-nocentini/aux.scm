
(module (aux timsort) *

  (import scheme 
          (chicken base) 
          (chicken foreign) 
          (aux base))

  (foreign-declare "#include \"chicken-timsort.h\"")

  (define timsort-foreign 
    (foreign-safe-lambda scheme-object "C_timsort" scheme-object size_t scheme-object scheme-object scheme-object bool bool bool bool int))

  (define timsort-depth (foreign-lambda size_t "C_timsort_depth"))
  (define timsort-unwind (foreign-lambda void "C_timsort_unwind" size_t))

  ;; The elements are handed to C inside a vector: that vector is the only
  ;; place they live while the sort runs, so the collector keeps tracing them
  ;; and C never has to cache a C_word of its own.  See chicken-timsort.c.
  (define ((timsort/gen lt? inplace reverse use-insertion-sort be-unpredictable-on-random-data comparator_type) lst) 
    (let* ((elements (list->vector lst))
           (size (vector-length elements))
           (new-lst (if inplace lst (vector->list elements)))
           (depth (timsort-depth)))
      ;; A comparator that raises, or that escapes through a continuation
      ;; captured outside the sort, never lets C_timsort return and drop the
      ;; roots holding `elements'; they are dropped here instead.
      (or (dynamic-wind
            void
            (lambda () 
              (timsort-foreign lst size lt? new-lst elements inplace reverse use-insertion-sort be-unpredictable-on-random-data comparator_type))
            (lambda () (timsort-unwind depth)))
          (error 'timsort "cannot allocate the working array" size))))

  (define TIMSORT_USE_COMPARATOR (foreign-value "TIMSORT_USE_COMPARATOR" int))
  (define TIMSORT_USE_LESS_THAN (foreign-value "TIMSORT_USE_LESS_THAN" int))

  (define timsort (timsort/gen < #f #f #f #t TIMSORT_USE_COMPARATOR))
  (define timsort! (timsort/gen < #t #f #f #t TIMSORT_USE_COMPARATOR))
  (define timtros (timsort/gen < #f #t #f #t TIMSORT_USE_COMPARATOR))
  (define timtros! (timsort/gen < #t #t #f #t TIMSORT_USE_COMPARATOR))

  (define timsort/primitive (timsort/gen < #f #f #f #t TIMSORT_USE_LESS_THAN))
  (define timsort/primitive! (timsort/gen < #t #f #f #t TIMSORT_USE_LESS_THAN))
  (define timtros/primitive (timsort/gen < #f #t #f #t TIMSORT_USE_LESS_THAN))
  (define timtros/primitive! (timsort/gen < #t #t #f #t TIMSORT_USE_LESS_THAN))

  )



















































