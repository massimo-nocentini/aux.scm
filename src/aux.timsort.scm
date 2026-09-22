
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

  ;; The output buffer only has to be `size' fresh pairs: C overwrites every
  ;; car of it before it is returned, and on the two paths where it is not
  ;; returned (an allocation failure, or a comparator escaping non-locally) it
  ;; is thrown away unread.  Copying the elements into it with `vector->list'
  ;; first, which is what this used to do, is that much pure waste.
  (define (%timsort-fresh-list size)
    (let loop ((i size) (acc '()))
      (if (eq? i 0) acc (loop (- i 1) (cons #f acc)))))

  ;; The elements are handed to C inside a vector: that vector is the only
  ;; place they live while the sort runs, so the collector keeps tracing them
  ;; and C never has to cache a C_word of its own.  See chicken-timsort.c.
  (define ((timsort/gen lt? inplace reverse use-insertion-sort be-unpredictable-on-random-data comparator_type) lst)
    (let* ((elements (list->vector lst))
           (size (vector-length elements)))
      (if (< size 2)
          ;; Fewer than two elements: no comparison is possible, so there is
          ;; nothing for C to do and nothing that can escape.  Returning here
          ;; skips the foreign call, the buffer and the `dynamic-wind'.
          (if inplace (void) (if (null? lst) '() (list (car lst))))
          (timsort/call lt? inplace reverse use-insertion-sort be-unpredictable-on-random-data comparator_type lst elements size))))

  (define (timsort/call lt? inplace reverse use-insertion-sort be-unpredictable-on-random-data comparator_type lst elements size)
    (let* ((new-lst (if inplace lst (%timsort-fresh-list size)))
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
  (define TIMSORT_USE_NUMBER_LESS_THAN (foreign-value "TIMSORT_USE_NUMBER_LESS_THAN" int))

  ;; `<' itself, evaluated in C whenever both operands are numbers and handed
  ;; to the `<' on the left otherwise -- which is where the "bad argument
  ;; type" error comes from, exactly as when every comparison went through it.
  (define timsort (timsort/gen < #f #f #f #t TIMSORT_USE_NUMBER_LESS_THAN))
  (define timsort! (timsort/gen < #t #f #f #t TIMSORT_USE_NUMBER_LESS_THAN))
  (define timtros (timsort/gen < #f #t #f #t TIMSORT_USE_NUMBER_LESS_THAN))
  (define timtros! (timsort/gen < #t #t #f #t TIMSORT_USE_NUMBER_LESS_THAN))

  ;; The same, widened past `<' to the other classes C can order without
  ;; allocating: strings and symbols by their UTF-8 bytes, characters by code
  ;; point, #f before #t.
  (define timsort/primitive (timsort/gen < #f #f #f #t TIMSORT_USE_LESS_THAN))
  (define timsort/primitive! (timsort/gen < #t #f #f #t TIMSORT_USE_LESS_THAN))
  (define timtros/primitive (timsort/gen < #f #t #f #t TIMSORT_USE_LESS_THAN))
  (define timtros/primitive! (timsort/gen < #t #t #f #t TIMSORT_USE_LESS_THAN))

  )
