
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

  ;; ======================================================================
  ;; NATIVE VECTOR API
  ;;
  ;; No list is built, walked or marshalled: the caller's vector is handed
  ;; to C as-is and IS the working representation.  `src' is the GC root the
  ;; objects live in; `dst' is what C writes into and may be `src' itself,
  ;; in which case C applies the permutation in place, cycle by cycle, with
  ;; no second vector.
  ;; ======================================================================

  (define timsort-vector-foreign
    (foreign-safe-lambda scheme-object "C_timsort_vector"
      scheme-object size_t scheme-object scheme-object scheme-object bool bool bool int))

  (define (%vector-copy v n)
    (let ((w (make-vector n)))
      (let loop ((i 0)) (if (eq? i n) w (begin (vector-set! w i (vector-ref v i)) (loop (+ i 1)))))))

  ;; `n' is passed in, and the failure message mentions only `n', SO THAT
  ;; NOTHING AFTER THE FOREIGN CALL REFERS TO `src'.  Anything the code after
  ;; the call still needs is captured by that call's continuation, and when a
  ;; comparator escapes, the abandoned continuation of the `C_callback' keeps
  ;; that capture alive for the life of the process: writing
  ;; `(vector-length src)' in the error message, which is the obvious thing to
  ;; write, retains the caller's whole vector -- elements included -- on every
  ;; escape: measured at 4,812,436 bytes per escaped sort of 300 records
  ;; carrying a 2000-word payload each, against 428 bytes when only `n' is
  ;; named.  The list path is accidentally free of this because its own
  ;; message mentions a fixnum it already had.  This is NOT the GC-safety
  ;; rule; see the note at the top of chicken-timsort.c.
  (define (%timsort/vector-call lt? src dst keys n reverse use-insertion-sort be-unpredictable-on-random-data comparator_type)
    (let ((depth (timsort-depth)))
      ;; Same escape unwind as the list path: a comparator that raises or
      ;; escapes never lets C drop the roots holding `src', `dst' and `keys'.
      (or (dynamic-wind
            void
            (lambda ()
              (timsort-vector-foreign src n lt? dst keys
                                      reverse use-insertion-sort
                                      be-unpredictable-on-random-data comparator_type))
            (lambda () (timsort-unwind depth)))
          (error 'timsort/vector "cannot allocate the working array" n))))

  ;; A `!' variant sorts `v' in place and RETURNS it, the way `sort!' does; the
  ;; others return a FRESH vector and leave `v' untouched.  Sorting in place
  ;; allocates nothing in the Scheme heap at all: C applies the permutation to
  ;; `v' one cycle at a time, with no second vector and no mark array.
  (define ((timsort/vector/gen lt? inplace reverse use-insertion-sort be-unpredictable-on-random-data comparator_type) v)
    (let ((n (vector-length v)))
      (if (< n 2)
          (if inplace v (%vector-copy v n))
          (%timsort/vector-call lt? v (if inplace v (make-vector n)) #f n
                                reverse use-insertion-sort be-unpredictable-on-random-data comparator_type))))

  ;; Decorate-sort-undecorate.  `key' is applied EXACTLY ONCE per element, in
  ;; index order, in Scheme, before C is entered at all; the sort then orders
  ;; the KEY vector and permutes the elements alongside it.  So a sort by a
  ;; computed key costs n ordinary Scheme calls and, whenever the keys all
  ;; belong to one class C can order, ZERO `C_callback's -- instead of ~n log n
  ;; of them, each of which forces a minor collection.
  ;;
  ;; Stable: equal keys keep input order, and `reverse' is a stable descending
  ;; sort rather than a flipped comparator.  `key' is applied for every length,
  ;; including 0 and 1, only in the sense that a sort of fewer than two
  ;; elements applies it zero times -- a caller must not count applications to
  ;; detect a singleton.  A `key' that mutates `v' is undefined.  Because every
  ;; key is computed before the first comparison, a `key' that raises raises
  ;; earlier, and possibly on a different element, than a comparison-driven
  ;; sort would.
  (define ((timsort/vector/key/gen lt? inplace reverse use-insertion-sort be-unpredictable-on-random-data comparator_type) v key)
    (let ((n (vector-length v)))
      (if (< n 2)
          (if inplace v (%vector-copy v n))
          (let ((keys (make-vector n)))
            (let loop ((i 0))
              (unless (eq? i n) (vector-set! keys i (key (vector-ref v i))) (loop (+ i 1))))
            (%timsort/vector-call lt? v (if inplace v (make-vector n)) keys n
                                  reverse use-insertion-sort be-unpredictable-on-random-data comparator_type)))))

  (define timsort/vector  (timsort/vector/gen < #f #f #f #t TIMSORT_USE_NUMBER_LESS_THAN))
  (define timsort/vector! (timsort/vector/gen < #t #f #f #t TIMSORT_USE_NUMBER_LESS_THAN))
  (define timtros/vector  (timsort/vector/gen < #f #t #f #t TIMSORT_USE_NUMBER_LESS_THAN))
  (define timtros/vector! (timsort/vector/gen < #t #t #f #t TIMSORT_USE_NUMBER_LESS_THAN))

  (define timsort/primitive/vector  (timsort/vector/gen < #f #f #f #t TIMSORT_USE_LESS_THAN))
  (define timsort/primitive/vector! (timsort/vector/gen < #t #f #f #t TIMSORT_USE_LESS_THAN))
  (define timtros/primitive/vector  (timsort/vector/gen < #f #t #f #t TIMSORT_USE_LESS_THAN))
  (define timtros/primitive/vector! (timsort/vector/gen < #t #t #f #t TIMSORT_USE_LESS_THAN))

  (define timsort/vector/key  (timsort/vector/key/gen < #f #f #f #t TIMSORT_USE_NUMBER_LESS_THAN))
  (define timsort/vector/key! (timsort/vector/key/gen < #t #f #f #t TIMSORT_USE_NUMBER_LESS_THAN))
  (define timtros/vector/key  (timsort/vector/key/gen < #f #t #f #t TIMSORT_USE_NUMBER_LESS_THAN))
  (define timtros/vector/key! (timsort/vector/key/gen < #t #t #f #t TIMSORT_USE_NUMBER_LESS_THAN))

  (define timsort/primitive/vector/key  (timsort/vector/key/gen < #f #f #f #t TIMSORT_USE_LESS_THAN))
  (define timsort/primitive/vector/key! (timsort/vector/key/gen < #t #f #f #t TIMSORT_USE_LESS_THAN))
  (define timtros/primitive/vector/key  (timsort/vector/key/gen < #f #t #f #t TIMSORT_USE_LESS_THAN))
  (define timtros/primitive/vector/key! (timsort/vector/key/gen < #t #t #f #t TIMSORT_USE_LESS_THAN))

  )
