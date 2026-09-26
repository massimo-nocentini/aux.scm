
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
  (define timsort-fail (foreign-lambda void "C_timsort_fail" size_t))

  ;; ------------------------------------------------------------------------
  ;; An exit out of a sort that itself runs inside another sort's comparator.
  ;;
  ;; A comparator that raises, or that escapes through a continuation, leaves
  ;; through the sort's C frames, which the runtime never unwinds.  That is
  ;; harmless while no C frame is waiting for a callback of its own to return
  ;; -- the exit just abandons ours -- but not when the sort runs inside
  ;; another sort's comparator, and the handler that catches the raise, or the
  ;; continuation the comparator escapes to, is inside that comparator too.
  ;; Saved callback frames are strictly LIFO: when the outer callback returns,
  ;; the runtime resumes the ABANDONED inner `C_callback' instead and hands it
  ;; the outer result.  The inner sort then either raises again, outside the
  ;; handler that already caught it, or completes into its finished
  ;; continuation and the outer callback returns twice -- "[panic] callback
  ;; returned twice".
  ;;
  ;; So a sort started while another is in flight (`timsort-depth' is not 0,
  ;; which can only mean that we are in that sort's comparator) wraps its
  ;; comparator in `%timsort-catching', which stops such an exit before it
  ;; leaves the callback, fails the sort, and completes the exit from the sort
  ;; call, in the caller's own continuation, where it is safe.
  ;;
  ;; Nesting is judged by this module's own count of the sorts in flight, and
  ;; not by the runtime's `##sys#callback-continuation-stack': that stack is
  ;; never popped when a callback is left by an exit, so after one caught raise
  ;; out of any top-level sort it would stay non-empty for good.  The count
  ;; is exact: an outermost sort's exit unwinds it to 0 (see `timsort/call'),
  ;; and a nested one, wrapped, always returns through C.  The price is that a
  ;; sort run inside some OTHER library's callback is not wrapped and keeps
  ;; the runtime's own behaviour.
  ;;
  ;; Raises need nothing of their own.  The wrapper installs no exception
  ;; handler: whatever the comparator raises, with whatever primitive, reaches
  ;; its handlers exactly as it would in a top-level sort, is entered once,
  ;; and a handler that RETURNS resumes the raise -- or not -- exactly as it
  ;; would there, since a handler's return is not an exit.  Only an ESCAPE is
  ;; an exit, whether the comparator invokes a continuation itself or a
  ;; handler does (`handle-exceptions', `condition-case' and `guard' all
  ;; escape).  It meets the after thunk of a `dynamic-wind' around the
  ;; comparator once the winds inside the comparator are unwound and before
  ;; any outside it are.  That thunk captures the rest of the escape as a
  ;; continuation, and the wrapper fails the sort through `C_timsort_fail' and
  ;; returns normally from the callback; the sort takes its error path (roots
  ;; released, working array freed, nothing written back) and answers #f, and
  ;; the sort call then invokes the captured continuation: the escape goes on
  ;; from where it was stopped and reaches its target.  The winds between the
  ;; comparator and the sort call -- the sort's own, which restores the
  ;; call-chain buffer -- are unwound, re-entered by that invocation and
  ;; unwound again, so theirs run twice; no wind of the caller's does.  An
  ;; escape out of a sort nested several comparators deep is stopped and
  ;; resumed that way once per nested sort it leaves.
  ;;
  ;; What cannot be kept is a RESUMPTION of the comparator once its sort has
  ;; failed, or once it has answered: a continuation captured inside it and
  ;; invoked afterwards.  The usual way to make one is a `guard' none of
  ;; whose clauses matches: it re-raises, with `raise-continuable', from
  ;; inside the handler it has already escaped from.  That re-raise reaches
  ;; the next handler out as usual; if it escapes, fine -- the wind lets any
  ;; exit through once the sort is gone.  If it returns, the comparator would
  ;; answer to a sort that no longer waits for it, so an error is raised
  ;; instead, at the point where it would answer.  In a sort not nested in
  ;; another nothing detects such a resumption, and a comparator must not
  ;; make one.
  ;;
  ;; A sort not nested in another pays nothing: only nested ones pay a
  ;; `call/cc' and a `dynamic-wind' per comparison.
  ;;
  ;; `escape' is #f for a sort not nested in another, else a one-slot box:
  ;; #f until an escape fails the sort, then the rest of that escape.  The
  ;; box itself is what the wrapper's `call/cc' answers on an escape, so it
  ;; cannot be mistaken for anything the comparator answers.
  (define (%timsort-escape depth)
    (and (> depth 0) (vector #f)))

  (define ((%timsort-catching lt? escape depth) a b)
    ;; True once the comparator has answered or has escaped: any later exit
    ;; through the wind is let through, and any later answer refused.
    (let* ((left #f)
           (r (call-with-current-continuation
                (lambda (stop)
                  (dynamic-wind
                    void
                    (lambda ()
                      (let ((v (lt? a b)))
                        (when left
                          (error 'timsort "a comparator was resumed after it had answered or its nested sort had failed" a b))
                        (set! left #t)
                        v))
                    (lambda ()
                      (unless left
                        (set! left #t)
                        ;; Invoked later from the sort call, this returns
                        ;; from the thunk and the escape goes on.
                        (call-with-current-continuation
                          (lambda (rest)
                            (vector-set! escape 0 rest)
                            (timsort-fail depth)
                            (stop escape))))))))))
      ;; An escape answers #f to a sort that `timsort-fail' has already
      ;; failed; C ignores the answer.
      (if (eq? r escape) #f r)))

  ;; What a sort whose foreign call answered #f does: complete the escape
  ;; that failed it, if one did, else raise the allocation failure that #f
  ;; otherwise means.
  (define (%timsort-failed escape who n)
    (let ((rest (and escape (vector-ref escape 0))))
      (if rest
          (rest #f) ; never returns: the escape reaches its target from here
          (error who "cannot allocate the working array" n))))

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
  ;; continuation into a second sort and is then re-entered, which the note
  ;; on nested sorts above forbids too; a comparator must do neither.
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
           (depth (timsort-depth))
           (escape (%timsort-escape depth)))
      ;; A comparator that raises, or that escapes through a continuation
      ;; captured outside the sort, never lets C_timsort return and drop the
      ;; roots holding `elements'; they are dropped here instead.  (Unless
      ;; the sort is nested in another: then `%timsort-catching' fails it,
      ;; C_timsort returns #f, and `%timsort-failed' completes the exit
      ;; after this wind has been left.)  The trace buffer is restored from
      ;; the same thunk and after that unwind, so all three exits --
      ;; returning, raising, and a continuation captured outside the sort --
      ;; put back the size this level observed.  Nesting is then correct by
      ;; construction: each level restores what it saw.
      (or (dynamic-wind
            (lambda ()
              (when (and requested (not (eq? requested (timsort-trace-buffer-size/current))))
                (set! previous (%timsort-resize-trace-buffer requested))))
            (lambda ()
              (timsort-foreign lst size (if escape (%timsort-catching lt? escape depth) lt?)
                               new-lst elements keys inplace reverse use-insertion-sort be-unpredictable-on-random-data comparator_type))
            (lambda ()
              (timsort-unwind depth)
              (when previous
                (%timsort-resize-trace-buffer previous)
                (set! previous #f))))
          ;; Nothing but the call's own result may be named after the foreign
          ;; call: whatever this continuation closes over is pinned for the
          ;; life of the process when a comparator escapes non-locally, since
          ;; CHICKEN never unwinds such a frame.  `size' is a fixnum,
          ;; `previous' a fixnum or #f, and `escape' #f whenever the sort
          ;; can be abandoned that way: only a sort not nested in another can.
          (%timsort-failed escape 'timsort size))))

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
    (let* ((depth (timsort-depth))
           (escape (%timsort-escape depth)))
      ;; Same escape unwind as the list path: a comparator that raises or
      ;; escapes never lets C drop the roots holding `src', `dst' and `keys'.
      ;; And the same catching of an escape when the sort is nested in another.
      (or (dynamic-wind
            void
            (lambda ()
              (timsort-vector-foreign src n (if escape (%timsort-catching lt? escape depth) lt?) dst keys
                                      reverse use-insertion-sort
                                      be-unpredictable-on-random-data comparator_type))
            (lambda () (timsort-unwind depth)))
          (%timsort-failed escape 'timsort/vector n))))

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
