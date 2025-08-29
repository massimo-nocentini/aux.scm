
(module (aux timsort) *

  (import scheme 
          (chicken base) 
          (chicken foreign) 
          (aux base))

  (foreign-declare "#include \"chicken-timsort.h\"")

  (define timsort-foreign 
    (foreign-safe-lambda scheme-object "C_timsort" scheme-object size_t scheme-object scheme-object bool bool bool bool int))

  (define ((timsort/gen lt? inplace reverse use-insertion-sort be-unpredictable-on-random-data comparator_type) lst) 
    (let* ((size 0)
           (new-lst (map (λ (each) (add1! size)) lst)))
      (timsort-foreign lst size lt? new-lst inplace reverse use-insertion-sort be-unpredictable-on-random-data comparator_type)))

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





















