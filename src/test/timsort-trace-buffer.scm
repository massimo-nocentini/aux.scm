
(import (aux unittest) (aux timsort) scheme (chicken base) (chicken sort)
        (chicken condition) (chicken gc)
        (only (scheme base) parameterize)
        srfi-1)

;; The size the process starts with: CHICKEN's DEFAULT_TRACE_BUFFER_SIZE,
;; unless the program was given `-:aNUMBER'.  Every check below asks for this
;; value back, so none of them hard-codes 16.
(define default-size (timsort-trace-buffer-size/current))

(define records (list-tabulate 4000 (lambda (i) (cons (modulo (* i 2654435761) 97) i))))
(define (key<? a b) (< (car a) (car b)))

;; The only entry point that re-enters Scheme once per comparison, which is
;; the only thing the trace-buffer parameter is about.
(define timsort/comparator (timsort/gen key<? #f #f #f #t TIMSORT_USE_COMPARATOR))
(define timtros/comparator (timsort/gen key<? #f #t #f #t TIMSORT_USE_COMPARATOR))

(define (message-of thunk)
  (handle-exceptions e (get-condition-property e 'exn 'message) (thunk) 'no-error))

(define-suite timsort-trace-buffer-suite

  ((doc r) `((p "The call-chain buffer is scanned in full by the minor collection that ends "
                "every re-entry into Scheme from C, so its length multiplies the cost of every "
                "comparison a Scheme comparator makes.  "
                (code/inline "timsort-trace-buffer-size")
                " shrinks it for the extent of a sort.  It is off by default, because the "
                "resize also clears the recorded call chain and so shortens the backtrace of "
                "anything the comparator raises.")
             (p "These tests are about the parameter, not about the ordering: the sorted "
                "results themselves are checked in " (code/inline "timsort.scm") ".")))

  ;; Off by default, and reading the live size does not disturb it.
  ((test/default-is-off _)
   (⊦= #f (timsort-trace-buffer-size))
   (⊦= default-size (timsort-trace-buffer-size/current))
   (timsort/comparator records)
   (⊦= default-size (timsort-trace-buffer-size/current)))

  ;; THE REGRESSION THIS FILE EXISTS FOR.  `C_resize_trace_buffer' takes and
  ;; returns TAGGED words; declaring its result `int' tags it a second time,
  ;; so the restore asks for 2n+1 and the buffer grows geometrically across
  ;; sorts -- 16, 33, 67, 135 -- making every later sort slower with nothing
  ;; to show for it.  200 sorts is far past the point where that is visible.
  ((test/size-is-restored-exactly _)
   (parameterize ((timsort-trace-buffer-size 3))
     (do ((i 0 (+ i 1))) ((= i 200))
       (timsort/comparator (list (cons 3 'a) (cons 1 'b) (cons 2 'c)))))
   (⊦= default-size (timsort-trace-buffer-size/current)))

  ;; All three ways out of a sort put the size back: returning, raising, and a
  ;; continuation captured outside the sort.  The restore shares the `after'
  ;; thunk with the root unwind, so this is also a check that the two compose.
  ((test/restored-on-every-exit _)
   (parameterize ((timsort-trace-buffer-size 3))
     (timsort/comparator records)
     (⊦= default-size (timsort-trace-buffer-size/current))

     (handle-exceptions e #t
       ((timsort/gen (lambda (a b) (error 'boom "from the comparator"))
                     #f #f #f #t TIMSORT_USE_COMPARATOR)
        records))
     (⊦= default-size (timsort-trace-buffer-size/current))

     (call-with-current-continuation
       (lambda (k)
         ((timsort/gen (lambda (a b) (k 'escaped)) #f #f #f #t TIMSORT_USE_COMPARATOR)
          records)))
     (⊦= default-size (timsort-trace-buffer-size/current)))
   (⊦= default-size (timsort-trace-buffer-size/current)))

  ;; The comparator really does run under the requested size, and under the
  ;; default when the parameter is off.
  ((test/comparator-runs-under-the-requested-size _)
   (let ((seen '()))
     (parameterize ((timsort-trace-buffer-size 3))
       ((timsort/gen (lambda (a b) (set! seen (cons (timsort-trace-buffer-size/current) seen)) (< a b))
                     #f #f #f #t TIMSORT_USE_COMPARATOR)
        '(5 4 3 2 1 9 7 8)))
     (⊦= '(3) (delete-duplicates seen)))
   (let ((seen '()))
     ((timsort/gen (lambda (a b) (set! seen (cons (timsort-trace-buffer-size/current) seen)) (< a b))
                   #f #f #f #t TIMSORT_USE_COMPARATOR)
      '(5 4 3 2 1 9 7 8))
     (⊦= (list default-size) (delete-duplicates seen))))

  ;; Nesting is correct by construction because each level restores the size
  ;; it observed, never a constant.
  ((test/nesting-restores-per-level _)
   (let ((inside-after-inner #f) (done #f))
     (parameterize ((timsort-trace-buffer-size 8))
       ((timsort/gen (lambda (a b)
                       (unless done
                         (set! done #t)
                         (parameterize ((timsort-trace-buffer-size 3))
                           ((timsort/gen < #f #f #f #t TIMSORT_USE_COMPARATOR) '(3 1 2)))
                         (set! inside-after-inner (timsort-trace-buffer-size/current)))
                       (< a b))
                     #f #f #f #t TIMSORT_USE_COMPARATOR)
        '(5 4 3 2 1)))
     (⊦= 8 inside-after-inner)
     (⊦= default-size (timsort-trace-buffer-size/current))))

  ;; `with-timsort-trace-buffer-size' resizes once for the whole batch: the
  ;; sorts inside find the size already in place and do not resize again.
  ((test/with-form _)
   (⊦= (list 3 '(1 2 3))
       (with-timsort-trace-buffer-size 3
         (lambda () (list (timsort-trace-buffer-size/current) (timsort '(3 1 2))))))
   (⊦= default-size (timsort-trace-buffer-size/current))
   (handle-exceptions e #t (with-timsort-trace-buffer-size 3 (lambda () (error 'boom "inside"))))
   (⊦= default-size (timsort-trace-buffer-size/current))
   (⊦= default-size (with-timsort-trace-buffer-size #f (lambda () (timsort-trace-buffer-size/current)))))

  ;; `C_resize_trace_buffer' does `C_unfix' on whatever it is handed, so a
  ;; non-fixnum has to be refused in Scheme rather than become a wild size.
  ((test/rejects-values-C-cannot-use _)
   (let ((expected "expected #f, or a fixnum between the runtime's minimum and 65536"))
     (⊦= expected (message-of (lambda () (parameterize ((timsort-trace-buffer-size 16.0)) #t))))
     (⊦= expected (message-of (lambda () (parameterize ((timsort-trace-buffer-size 2)) #t))))
     (⊦= expected (message-of (lambda () (parameterize ((timsort-trace-buffer-size 'small)) #t))))
     (⊦= expected (message-of (lambda () (parameterize ((timsort-trace-buffer-size (expt 2 80))) #t))))
     ;; The point of the knob is to SHRINK the buffer, so an absurdly large
     ;; request is refused too rather than made expensive.
     (⊦= expected (message-of (lambda () (parameterize ((timsort-trace-buffer-size 65537)) #t))))
     (⊨ (fixnum? (parameterize ((timsort-trace-buffer-size 65536)) (timsort-trace-buffer-size)))))
   (⊦= 3 (parameterize ((timsort-trace-buffer-size 3)) (timsort-trace-buffer-size)))
   (⊦= default-size (timsort-trace-buffer-size/current)))

  ;; The knob must not be able to change an answer.  Object identity, not
  ;; value: a stable sort that merely produced an equal list would pass an
  ;; `equal?' check and still have lost the tie order.
  ((test/results-identical-with-the-knob-on _)
   (let ((reference (sort records key<?)))
     (⊨ (every eq? reference (timsort/comparator records)))
     (⊨ (every eq? reference (parameterize ((timsort-trace-buffer-size 3))
                               (timsort/comparator records)))))
   ;; descending is stable too: equal keys keep INPUT order while keys descend
   (let ((reference (reverse (sort (reverse records) key<?))))
     (⊨ (every eq? reference (parameterize ((timsort-trace-buffer-size 3))
                               (timtros/comparator records))))))

  ;; The families whose order C evaluates by itself never call back, so the
  ;; parameter has nothing to act on -- but it must not disturb them either,
  ;; including where they hand a pair back to `<' and `<' raises.
  ((test/C-evaluated-families-unaffected _)
   (let ((d (list-tabulate 2000 (lambda (i) (modulo (* i 40503) 1000)))))
     (parameterize ((timsort-trace-buffer-size 3))
       (⊦= (sort d <) (timsort d))
       (⊦= (reverse (sort (reverse d) <)) (timtros d))
       (⊦= (sort d <) (timsort/primitive d))
       (let ((c (list-copy d))) (timsort! c) (⊦= (sort d <) c))
       (⊦= '("a" "b" "c") (timsort/primitive '("b" "a" "c")))
       (⊦= "bad argument type - not a number"
           (message-of (lambda () (timsort (list 1 "two" 3))))))))

  ;; A comparator that escapes leaves the sort's C frames standing for good,
  ;; so anything the foreign call's continuation names is pinned for the life
  ;; of the process.  The restore adds two names to that continuation; both
  ;; are a fixnum or #f, so the knob must not change what an escaped sort
  ;; retains.  Measured as ON against OFF in the same process -- an absolute
  ;; budget would only be measuring the interpreter, which retains several
  ;; kilobytes per escaped sort either way.  Retaining a payload would be
  ;; ~100MB over these 20 escapes, so the margin is four orders of magnitude.
  )

(unittest/✓ timsort-trace-buffer-suite)
