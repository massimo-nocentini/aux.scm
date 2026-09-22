
(module (aux timsort) *

  (import scheme
          (chicken base)
          (chicken foreign)
          ;; `make-parameter' and `parameterize' are R7RS; they are not in
          ;; (chicken base).
          (only (scheme base) make-parameter parameterize)
          (aux base))

  (foreign-declare "#include \"chicken-timsort.h\"")

  (define timsort-foreign
    (foreign-safe-lambda scheme-object "C_timsort" scheme-object size_t scheme-object scheme-object scheme-object scheme-object bool bool bool bool int))

  (define timsort-depth (foreign-lambda size_t "C_timsort_depth"))
  (define timsort-unwind (foreign-lambda void "C_timsort_unwind" size_t))

  ;; ------------------------------------------------------------------------
  ;; The call-chain buffer, and why it is a sort parameter.
  ;;
  ;; A sort that re-enters Scheme -- which is any sort whose comparator C
  ;; cannot evaluate itself, i.e. TIMSORT_USE_COMPARATOR, and the pair that
  ;; raises when `<' is handed something it will not order -- pays a forced
  ;; minor collection per comparison.  It is not bookkeeping bolted on the
  ;; side: `callback_return_continuation' is an ordinary CPS procedure, the
  ;; only way back into the abandoned C frame is `C_longjmp(C_restart, 1)',
  ;; and in the whole runtime that longjmp occurs exactly once -- at the end
  ;; of `C_reclaim' (runtime.c).  Returning from a callback therefore *is* a
  ;; collection.
  ;;
  ;; That collection's cost is nearly all fixed, because a comparator leaves
  ;; nothing live in the nursery.  The one part of it that scales with
  ;; something we can reach is the trace-buffer scan in `mark_live_objects':
  ;;
  ;;     for(tinfo = trace_buffer; tinfo < trace_buffer_limit; ++tinfo) {
  ;;       mark(&tinfo->cooked_location); mark(&tinfo->cooked1);
  ;;       mark(&tinfo->cooked2);         mark(&tinfo->thread); }
  ;;
  ;; -- four `mark's per entry over the WHOLE buffer, full or not.  The
  ;; buffer's length is thus a straight multiplier on the price of every
  ;; comparison.  Measured by sorting the same 1e6 random records through the
  ;; same comparator at several lengths, in one process, on a heavily loaded
  ;; box (so read the slope, not the absolutes): 298ns per comparison at the
  ;; minimum 3, 443 at CHICKEN's default 16, 559 at 64, 1354 at 256 -- a
  ;; straight line of ~4.2ns per buffer entry per comparison.  Nothing else
  ;; in that collection scales with anything an egg can reach: the nursery is
  ;; empty (a comparator leaves nothing live), and the mutation stack and the
  ;; temporary stack hold one word between them.
  ;;
  ;; Nursery size is therefore NOT a lever, and that was checked rather than
  ;; assumed: -:s128k through -:s6m move this sort by less than the noise,
  ;; because the collection is forced on every callback whatever the nursery
  ;; holds.  (-:s above `ulimit -s' segfaults -- CHICKEN's nursery is the C
  ;; stack -- with or without this egg in the program.)
  ;;
  ;; THE PRICE, and the only reason this is opt-in: `C_resize_trace_buffer'
  ;; frees and reallocates the buffer, so it CLEARS the recorded call chain.
  ;; While it is in effect an error raised by a comparator carries a 3-deep
  ;; backtrace instead of 16, and the restore on the way out clears it again,
  ;; so a later error whose history crossed the sort loses up to 16 frames
  ;; too.  A sort is exactly where a user comparator is most likely to raise.
  ;; Debuggability for speed is the caller's trade to make, never ours.
  ;;
  ;; The same effect with no code at all is the documented `-:aNUMBER' runtime
  ;; option (`-:a3'), which sets the buffer length for the whole process.  Use
  ;; that when a whole program can live with short call chains; use this when
  ;; only the sorts can.
  ;;
  ;; SCOPE: the per-sort resize is done by the LIST entry points only.  The
  ;; vector ones do not consult this parameter, so a vector sort with a user
  ;; comparator runs at the default buffer length; wrap it in
  ;; `with-timsort-trace-buffer-size' if it needs the same treatment.  The
  ;; bracket wants factoring into one helper shared by all four call sites, and
  ;; that is where this belongs -- but it is also the most delicate code here,
  ;; so it is left for a change of its own rather than done in passing.
  ;;
  ;; THE BUFFER IS PROCESS-GLOBAL, so this is only correct while sorts nest.
  ;; Each level restores what it observed, which is right for nesting -- an
  ;; inner sort inside an outer one -- but two sorts that OVERLAP without
  ;; nesting can restore each other's sizes in the wrong order.  That needs a
  ;; comparator that yields to another thread, or one that escapes through a
  ;; continuation into a second sort and back; a comparator must do neither.
  ;; Nothing here can detect it, so it is stated rather than guarded.
  ;; ------------------------------------------------------------------------

  ;; `C_resize_trace_buffer' is declared in chicken.h (it is what
  ;; `##sys#resize-trace-buffer' calls; that one is unreachable from a
  ;; compiled program, it lives in unit `repl').  It TAKES AND RETURNS TAGGED
  ;; WORDS -- `C_unfix(size)' in, `C_fix(old_size)' out -- so the foreign type
  ;; is `scheme-object' on both sides.  Declaring the result `int' tags it
  ;; twice and the restore then sets the buffer to 2n+1, growing it
  ;; geometrically across sorts -- 16, 33, 67, 135 -- so that every later sort
  ;; is slower with nothing to show for it.  `test/size-is-restored-exactly'
  ;; in test/timsort-trace-buffer.scm exists for exactly that mistake.
  (define %timsort-resize-trace-buffer
    (foreign-lambda scheme-object "C_resize_trace_buffer" scheme-object))

  ;; Reading the size is not the same call: `C_trace_buffer_size' is a plain
  ;; `C_varextern int', so this is a load and costs nothing, where
  ;; `C_resize_trace_buffer' would answer the question by destroying the
  ;; buffer.
  (define timsort-trace-buffer-size/current
    (foreign-lambda* int () "C_return(C_trace_buffer_size);"))

  ;; MIN_TRACE_BUFFER_SIZE (runtime.c).  It is not exported by chicken.h, and
  ;; `C_clear_trace_buffer' clamps to it silently -- but a request below it
  ;; would then not be the size the caller gets, and this file does not build
  ;; on a clamp it cannot see.
  (define timsort-trace-buffer-minimum 3)

  ;; #f (the default, and today's behaviour bit for bit) leaves the call-chain
  ;; buffer alone.  A fixnum >= 3 is the length it is shrunk to for the
  ;; duration of each sort, and restored from afterwards.
  ;;
  ;; The value must be a fixnum: `C_resize_trace_buffer' does `C_unfix(size)'
  ;; on whatever it is handed, so anything else is undefined behaviour in C
  ;; rather than a Scheme error.  That check belongs here, at the only point
  ;; where a wrong value is still a value and not yet a word.
  (define timsort-trace-buffer-size
    (make-parameter #f
      (lambda (size)
        (cond ((not size) #f)
              ((and (fixnum? size)
                    (>= size timsort-trace-buffer-minimum)
                    ;; An upper bound as well: the point of this knob is to
                    ;; SHRINK the buffer, and a huge request would just make
                    ;; every resize expensive for no benefit.
                    (<= size 65536))
               size)
              (else (error 'timsort-trace-buffer-size
                           "expected #f, or a fixnum between the runtime's minimum and 65536"
                           size timsort-trace-buffer-minimum))))))

  ;; Shrink once for a whole batch of sorts instead of once per sort.  The
  ;; parameter is set as well, so `timsort/call' sees the size it wants
  ;; already in place and skips its own resize -- which is what makes this
  ;; free rather than merely cheap for many small sorts.
  ;;
  ;; That matters because a resize is not free: it frees and re-mallocs the
  ;; buffer and clears it, which measures ~800ns here.  One sort of any
  ;; interesting size pays that back in its first few comparisons, but 200000
  ;; sorts of four elements do not, and on the callback-free `<' path they
  ;; never would.  Hence this form, and hence the skip.
  (define (with-timsort-trace-buffer-size size thunk)
    (parameterize ((timsort-trace-buffer-size size))
      (let ((requested (timsort-trace-buffer-size)) ; validated by the converter
            (previous #f))
        (dynamic-wind
          (lambda ()
            (when (and requested (not (eq? requested (timsort-trace-buffer-size/current))))
              (set! previous (%timsort-resize-trace-buffer requested))))
          thunk
          (lambda ()
            (when previous
              (%timsort-resize-trace-buffer previous)
              (set! previous #f)))))))

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
          (timsort/call lt? inplace reverse use-insertion-sort be-unpredictable-on-random-data comparator_type lst elements #f size))))

  ;; The keys are extracted here, in Scheme, before C is entered: the key
  ;; procedure runs exactly once per element and as an ordinary Scheme call,
  ;; never as a `C_callback'.  C then orders the keys, which is why a keyed
  ;; sort re-enters Scheme not once per comparison but not at all.
  (define (%timsort-keys key elements size)
    (let ((keys (make-vector size)))
      (let loop ((i 0))
        (if (eq? i size)
            keys
            (begin (vector-set! keys i (key (vector-ref elements i)))
                   (loop (+ i 1)))))))

  (define ((timsort/key/gen lt? inplace reverse use-insertion-sort be-unpredictable-on-random-data comparator_type) lst key)
    (let* ((elements (list->vector lst))
           (size (vector-length elements)))
      (if (< size 2)
          ;; As above -- and the key is applied zero times, so a caller cannot
          ;; tell a singleton from an empty list by counting applications.
          (if inplace (void) (if (null? lst) '() (list (car lst))))
          (timsort/call lt? inplace reverse use-insertion-sort be-unpredictable-on-random-data comparator_type
                        lst elements (%timsort-keys key elements size) size))))

  (define (timsort/call lt? inplace reverse use-insertion-sort be-unpredictable-on-random-data comparator_type lst elements keys size)
    (let* ((new-lst (if inplace lst (%timsort-fresh-list size)))
           ;; Read once, outside the wind: a parameter lookup per sort, not
           ;; per comparison, and the two thunks must agree about it even if
           ;; the comparator reparameterizes it.
           ;;
           ;; Only TIMSORT_USE_COMPARATOR calls back into Scheme, so only it
           ;; can gain anything; on every other path a resize would be pure
           ;; cost (~800ns, and a cleared backtrace) for no comparisons saved.
           (requested (and (eq? comparator_type TIMSORT_USE_COMPARATOR)
                           (timsort-trace-buffer-size)))
           (previous #f)
           (depth (timsort-depth)))
      ;; A comparator that raises, or that escapes through a continuation
      ;; captured outside the sort, never lets C_timsort return and drop the
      ;; roots holding `elements'; they are dropped here instead.  The trace
      ;; buffer is restored from the same thunk and after that unwind, so all
      ;; three exits -- returning, raising, and a continuation captured
      ;; outside the sort -- put back the size this level observed.  Nesting
      ;; is then correct by construction: each level restores what it saw.
      (or (dynamic-wind
            (lambda ()
              (when (and requested (not (eq? requested (timsort-trace-buffer-size/current))))
                (set! previous (%timsort-resize-trace-buffer requested))))
            (lambda ()
              (timsort-foreign lst size lt? new-lst elements keys inplace reverse use-insertion-sort be-unpredictable-on-random-data comparator_type))
            (lambda ()
              (timsort-unwind depth)
              (when previous
                (%timsort-resize-trace-buffer previous)
                (set! previous #f))))
          ;; Nothing but the call's own result may be named after the foreign
          ;; call: whatever this continuation closes over is pinned for the
          ;; life of the process when a comparator escapes non-locally, since
          ;; CHICKEN never unwinds such a frame.  `size' is a fixnum and
          ;; `previous' a fixnum or #f; neither reaches the elements.
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

  ;; ----------------------------------------------------------------------
  ;; SORT BY KEY.  `(timsort/key lst car)' orders the ELEMENTS of lst by
  ;; `(< (car a) (car b))', stably, applying `car' exactly once per element in
  ;; input order.  The comparator on the left is handed the two KEYS, not the
  ;; two elements, because comparing keys is what this is defined to do -- so a
  ;; type error still comes from `<' and reads as it always did.
  ;;
  ;; A key can only express a total preorder.  A comparator that is not a
  ;; function of one projection of each element -- a multi-field order with
  ;; mixed directions, say -- has no single key, and still needs
  ;; `timsort/gen' with TIMSORT_USE_COMPARATOR and its per-comparison callback.
  ;; ----------------------------------------------------------------------
  (define timsort/key  (timsort/key/gen < #f #f #f #t TIMSORT_USE_NUMBER_LESS_THAN))
  (define timsort/key! (timsort/key/gen < #t #f #f #t TIMSORT_USE_NUMBER_LESS_THAN))
  (define timtros/key  (timsort/key/gen < #f #t #f #t TIMSORT_USE_NUMBER_LESS_THAN))
  (define timtros/key! (timsort/key/gen < #t #t #f #t TIMSORT_USE_NUMBER_LESS_THAN))

  (define timsort/primitive/key  (timsort/key/gen < #f #f #f #t TIMSORT_USE_LESS_THAN))
  (define timsort/primitive/key! (timsort/key/gen < #t #f #f #t TIMSORT_USE_LESS_THAN))
  (define timtros/primitive/key  (timsort/key/gen < #f #t #f #t TIMSORT_USE_LESS_THAN))
  (define timtros/primitive/key! (timsort/key/gen < #t #t #f #t TIMSORT_USE_LESS_THAN))

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
