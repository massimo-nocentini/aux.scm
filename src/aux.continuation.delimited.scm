
(module (aux continuation delimited) *

  (import scheme
          (only (scheme base) make-parameter)
          (chicken base)
          (only (chicken fixnum) fx+ fx- fxmax)
          srfi-1
          (aux base)
          (aux continuation))

  (define-syntax-rule (resetcc body ...) (delimcc-reset (τ body ...)))
  (define-syntax-rule (resetcc+null body ...) (resetcc body ... '()))
  (define-syntax define-resetcc
    (syntax-rules ()
      ((define-resetcc (def arg ...) body ...) (define def (λ (arg ...) (resetcc body ...))))
      ((define-resetcc def body ...) (define def (resetcc body ...)))))

  (define-syntax-rule (letcc/shift k body ...) (delimcc-shift (λ (k) body ...)))
  (define (callcc/shift f) (letcc/shift k (f k)))
  (define-syntax-rule (λ-shift args body ...) (letcc/shift k (λ args (let1 (x (begin body ...)) (k x)))))
  (define-syntax-rule (τ-shift body ...) (λ-shift () body ...))

  (define (delimcc-missing-reset . args)
    (warning "Missing enclosing resetcc, called with args" args)
    (apply values args))

  ; A resetcc: `k` returns from it, `parent` is the frame current outside of it, `outer` the
  ; exception handlers outside of it (see `delimcc-handlers-push`) and `winds` the
  ; `##sys#dynamic-winds` of its body; `signalling` counts the calls to handlers outside of it,
  ; for conditions raised in its body, whose dynamic extent control is in, and `state` is `in`,
  ; `escaping` (control left such a call other than by its return, see `delimcc-signal`), `left`
  ; (control left the body while such a call was pending or escaping, see `delimcc-reset`) or
  ; `popped`.  `host` is the nearest frame below it
  ; whose outer handler was running when it was pushed (#f if none): its body runs within that
  ; call, so it is left with the host.  `escaped` is #f or, when control escaped from such a call
  ; for a condition raised outside the body, the winds of the raise, see `delimcc-signal`.  The
  ; root frame stands for no resetcc.
  (define-record delimcc-frame k parent outer winds signalling state host escaped)
  (define delimcc-root-frame (make-delimcc-frame #f #f #f #f 0 'in #f #f))

  ; `##sys#current-exception-handler` is a stack of handlers, the current one at its car, since
  ; CHICKEN 6.0.1 (core commit d63e474b): `signal` and `abort` pop it while the handler they call
  ; runs, and storing a bare procedure there makes the next `current-exception-handler` spin.  In
  ; CHICKEN 6.0.0 it is the handler itself, which stays current while it runs.
  (define (delimcc-handlers-push handler handlers)
    (if (pair? handlers) (cons handler handlers) handler))

  ; Call the current handler of `handlers` on `c` as `signal` calls it.
  (define (delimcc-handlers-call handlers c)
    (cond
      ((pair? handlers) (let1 (saved #f)
                          (##sys#dynamic-wind
                            (τ (set! saved ##sys#current-exception-handler)
                               (set! ##sys#current-exception-handler (cdr handlers)))
                            (τ ((car handlers) c))
                            (τ (set! ##sys#current-exception-handler saved)))))
      (else (handlers c))))

  ; The `##sys#dynamic-winds` of a raise, from within its handler.  Since CHICKEN 6.0.1 `signal`
  ; and `raise-continuable` call the handler inside a `dynamic-wind` frame of their own, which
  ; reinstates it on the way out: its before thunk is `##sys#pop-exception-handler!`, by which it
  ; is left out (CHICKEN 6.0.0 has neither the frame nor the procedure).
  (define delimcc-pop-exception-handler
    (and (##sys#symbol-has-toplevel-binding? '##sys#pop-exception-handler!)
         (##sys#slot '##sys#pop-exception-handler! 0)))

  (define (delimcc-raise-winds)
    (let1 (ws ##sys#dynamic-winds)
      (if (and delimcc-pop-exception-handler (pair? ws) (eq? (caar ws) delimcc-pop-exception-handler))
        (cdr ws)
        ws)))

  ; Unchecked reads of the fields that every resetcc, shift and return reads.
  (define-inline (delimcc-frame-k/fast f) (##sys#slot f 1))
  (define-inline (delimcc-frame-parent/fast f) (##sys#slot f 2))
  (define-inline (delimcc-frame-winds/fast f) (##sys#slot f 4))
  (define-inline (delimcc-frame-signalling/fast f) (##sys#slot f 5))
  (define-inline (delimcc-frame-state/fast f) (##sys#slot f 6))
  (define-inline (delimcc-frame-outer/fast f) (##sys#slot f 3))
  (define-inline (delimcc-frame-signalling-set!/fast f n) (##sys#setislot f 5 n))
  (define-inline (delimcc-frame-state-set!/fast f s) (##sys#setslot f 6 s))
  (define-inline (delimcc-frame-host/fast f) (##sys#slot f 7))

  ; Whether `frame` has been left for good: left by a condition, escaped by one raised outside its
  ; body to a place outside the context of the raise, or pushed while the outer handler of a frame
  ; so left was running.
  (define-inline (delimcc-frame-gone?/fast frame)
    (or (eq? (delimcc-frame-state/fast frame) 'left)
        (let1 (host (delimcc-frame-host/fast frame))
          (and host (memq (delimcc-frame-state/fast host) '(left popped)) #t))
        (and (eq? (delimcc-frame-state/fast frame) 'escaping) (delimcc-frame-escaped-away? frame))))

  (define (delimcc-frame-escaped-away? frame)
    (let1 (winds (delimcc-frame-escaped frame))
      (and winds (eq? 0 (delimcc-frame-signalling/fast frame)) (not (delimcc-within? winds)))))

  (define (delimcc-within? base)
    (let loop ((ws ##sys#dynamic-winds))
      (cond
        ((eq? ws base) #t)
        ((pair? ws) (loop (cdr ws)))
        (else #f))))

  ; The current frame is kept in the cdr of a cell, one per srfi-18 thread, so that a thread never
  ; delivers its shifts to another thread's resetcc, and, in the srfi-18 thread that runs (aux cml),
  ; one per CML thread: CML runs all its threads within that srfi-18 thread, switching among them
  ; by call/cc.  The cell of a srfi-18 thread is kept in a parameter, whose value CHICKEN keeps per
  ; srfi-18 thread, with the thread owning it in its car: a new thread starts with a copy of its
  ; creator's parameters, not inside the creator's resetcc.  The cell of a CML thread is a thread
  ; property of (aux cml), so that it goes with the thread when the thread dies (in a resetcc or
  ; not) and with the session; its `get` is made at the first use, by name, so that this module
  ; does not load (aux cml), and it is used only in the srfi-18 thread that runs `run-cml`, which
  ; (aux cml) keeps in `%host-thread`: the current CML thread is one global, which other srfi-18
  ; threads see change under them.  AUX-CML.md ("Delimited continuations") records these two
  ; names that this module reads; test/delimcc/cml fails if they stop resolving.  The
  ; frame is only ever mutated, never parameterized nor kept by a `dynamic-wind`: a continuation
  ; captured by `letcc` inside a resetcc may be resumed from within a later resumption of one of
  ; its shift continuations, and then it must still return to the resetcc of that resumption.
  (define delimcc-meta-continuation-thread-cell (make-parameter #f))
  (define delimcc-meta-continuation-cml-cell #f)

  ; The frames that control left while a handler outside of them was running, and the ones pushed
  ; within such a handler, are left for good once a resetcc, shift, return or condition of the
  ; thread asks for the current frame.
  (define (delimcc-settle! cell)
    (let1 (frame (cdr cell))
      (when (delimcc-frame-gone?/fast frame)
        (delimcc-frame-state-set! frame 'popped)
        (delimcc-frame-signalling-set! frame 0)
        (set-cdr! cell (delimcc-frame-parent frame))
        (delimcc-settle! cell)))
    cell)

  (define (delimcc-meta-continuation-cell)
    (let1 (cell (delimcc-meta-continuation-cell/raw))
      (if (delimcc-frame-gone?/fast (cdr cell)) (delimcc-settle! cell) cell)))

  (define (delimcc-meta-continuation-cell/raw)
    (let1 (host 'aux.cml#%host-thread)
      (cond
        ((and (##sys#symbol-has-toplevel-binding? host) (eq? (##sys#slot host 0) ##sys#current-thread))
         (unless delimcc-meta-continuation-cml-cell
           (receive (clear! get peek put!) ((##sys#slot 'aux.cml#make-thread-property 0) (τ (cons 'cml delimcc-root-frame)))
             (set! delimcc-meta-continuation-cml-cell get)))
         (delimcc-meta-continuation-cml-cell))
        (else (let1 (cell (delimcc-meta-continuation-thread-cell))
                (if (and cell (eq? (car cell) ##sys#current-thread))
                  cell
                  (let1 (cell (cons ##sys#current-thread delimcc-root-frame))
                    (delimcc-meta-continuation-thread-cell cell)
                    cell)))))))

  ; Return from the resetcc of `frame`, the current one in `cell`.
  (define (delimcc-escape cell frame args)
    (cond
      ((eq? frame delimcc-root-frame) (apply delimcc-missing-reset args))
      (else (set-cdr! cell (delimcc-frame-parent/fast frame))
            (unless (eq? 0 (delimcc-frame-signalling/fast frame)) (delimcc-frame-signalling-set! frame 0))
            (apply (delimcc-frame-k/fast frame) args))))

  ; Give the values of a resetcc body to the resetcc current now, which is not the lexically
  ; enclosing one when the body is part of a resumed shift continuation (in this thread or in
  ; another one).
  (define (delimcc-return args)
    (let1 (cell (delimcc-meta-continuation-cell))
      (delimcc-escape cell (cdr cell) args)))

  ; The handler of a resetcc body, for a condition raised in the body of the current frame: it
  ; calls the handler outside that resetcc, which runs where the condition was raised, inside the
  ; resetcc (so a shift there goes to it, as a handler in Racket does).  The frame and the outer
  ; handler are the ones current when the condition is raised, not the ones of the resetcc that
  ; installed this handler: the body of a shift continuation resumed elsewhere signals to the
  ; handler around the resume.  While that outer handler runs, a condition that reaches this
  ; handler again (CHICKEN 6.0.0's `signal` leaves it current, and the outer handler may be the
  ; one of an enclosing resetcc) belongs to the frames outside, so frames whose outer handler is
  ; running are skipped.  The call is counted by a `dynamic-wind` frame, so that the count follows control in and out of
  ; the call: a continuation escaping from the handler (`handle-exceptions`, or any other) leaves
  ; the call, a continuation captured in it (a CML thread switch) enters it again.  Leaving it
  ; other than by its return marks the frame `escaping`, for the after thunk of the resetcc, which
  ; runs next if the escape leaves the body too; an escape that lands in the body is found out
  ; by the next condition, or by the before thunk when control comes back into the body.  The
  ; frame may be current while control is outside its body, whose after thunk then never runs: a
  ; continuation captured by call/cc in a shift continuation, taken within a resumption of it
  ; (a `handle-exceptions` returning), brings back the context of the original resetcc with the
  ; frame of the resume current.  Then the frame records in `escaped` the winds of the raise,
  ; and it is left for good once control is found outside of them.
  ;   aux.egg compiles this module with `-no-trace` (CHICKEN has no declaration for it; a build
  ; by hand should pass it too), listing the default `-O2 -d1` with it since a component's
  ; `csc-options` replace them: otherwise the calls of this handler fill the call history that
  ; an uncaught error in a resetcc body prints, and its `<--` points here, not at the failing call.
  (define (delimcc-signal c outer)
    (let loop ((frame (cdr (delimcc-meta-continuation-cell))))
      (cond
        ((eq? frame delimcc-root-frame) (delimcc-handlers-call outer c))
        ((not (eq? 0 (delimcc-frame-signalling/fast frame))) (loop (delimcc-frame-parent/fast frame)))
        (else (when (eq? (delimcc-frame-state/fast frame) 'escaping) (delimcc-frame-state-set!/fast frame 'in))
              (let1 (returned #f)
                (##sys#dynamic-wind
                  (τ (set! returned #f)
                     (delimcc-frame-escaped-set! frame #f)
                     (delimcc-frame-signalling-set!/fast frame (fx+ (delimcc-frame-signalling/fast frame) 1)))
                  (τ (receive vs (delimcc-handlers-call (delimcc-frame-outer/fast frame) c) (set! returned #t) (apply values vs)))
                  (τ (delimcc-frame-signalling-set!/fast frame (fxmax 0 (fx- (delimcc-frame-signalling/fast frame) 1)))
                     (unless (or returned (not (eq? (delimcc-frame-state/fast frame) 'in)))
                       (delimcc-frame-state-set!/fast frame 'escaping)
                       (unless (delimcc-within? (delimcc-frame-winds/fast frame))
                         (delimcc-frame-escaped-set! frame (delimcc-raise-winds)))))))))))

  ; The body runs in a `dynamic-wind` frame that installs the resetcc's handler, as
  ; `with-exception-handler` does, and records its `##sys#dynamic-winds`; it never returns
  ; through it: when it belongs to a resumed shift continuation that frame is not in the current
  ; winds, whose resetcc is the one `delimcc-return` escapes to.  Leaving the body does not leave
  ; the resetcc, as resumed shift continuations and `letcc` continuations re-enter bodies
  ; legitimately; but leaving it while a handler outside of it runs, or by an escape from such a
  ; handler, is a condition escaping it (`handle-exceptions`, `condition-case`, `guard`), so then
  ; the frame is marked `left`; it is popped, with the frames pushed within that handler, at the
  ; next use of the thread's frame, unless control comes back into the body first (a CML thread
  ; switched out and in again while the handler blocks, or a `guard` that re-raises in the
  ; handler's context); a body re-entered after the pop becomes current again.
  (define (delimcc-reset thunk)
    (let1 (cell (delimcc-meta-continuation-cell))
      (letcc k
        (let* ((parent (cdr cell))
               (outer ##sys#current-exception-handler)
               (host (if (eq? 0 (delimcc-frame-signalling/fast parent)) (delimcc-frame-host/fast parent) parent))
               (frame (make-delimcc-frame k parent outer #f 0 'in host #f))
               (handler (λ (c) (delimcc-signal c outer))))
          (set-cdr! cell frame)
          (##sys#dynamic-wind
            (τ (set! ##sys#current-exception-handler (delimcc-handlers-push handler outer))
               (case (delimcc-frame-state/fast frame)
                 ((left escaping) (delimcc-frame-state-set! frame 'in))
                 ((popped) (delimcc-frame-state-set! frame 'in)
                           (let1 (cell (delimcc-meta-continuation-cell))
                             (when (eq? (cdr cell) parent) (set-cdr! cell frame))))))
            (τ (delimcc-frame-winds-set! frame ##sys#dynamic-winds)
               (receive args (thunk) (delimcc-return args)))
            (τ (set! ##sys#current-exception-handler outer)
               (when (or (not (eq? 0 (delimcc-frame-signalling/fast frame)))
                         (eq? (delimcc-frame-state/fast frame) 'escaping))
                 (delimcc-frame-state-set! frame 'left))))))))

  ; The `dynamic-wind` frames of the current winds above `base`, outermost first, leaving them
  ; after thunk by after thunk, innermost first; and entering them again on top of the current
  ; winds, before thunk by before thunk.
  (define (delimcc-unwind! base)
    (let loop ((segment '()))
      (let1 (ws ##sys#dynamic-winds)
        (cond
          ((eq? ws base) segment)
          (else (set! ##sys#dynamic-winds (cdr ws))
                ((cdar ws))
                (loop (cons (car ws) segment)))))))

  (define (delimcc-rewind! segment)
    (unless (null? segment)
      ((caar segment))
      (set! ##sys#dynamic-winds (cons (car segment) ##sys#dynamic-winds))
      (delimcc-rewind! (cdr segment))))

  ; A shift continuation holds the `dynamic-wind` frames between the shift and its resetcc and
  ; nothing outside: the shift leaves them before calling `f`, which runs in the resetcc's
  ; context, and every resume enters them again on top of the context of the resume, so that the
  ; resumed code sees the parameters and signals to the handlers in force there.  The raw
  ; continuation primitive jumps without touching the winds, which are spliced by hand.  When the
  ; resetcc's winds are not a tail of the current ones (no resetcc at all, or a `letcc`
  ; continuation of an older resumption brought the context of that one back) the whole context of
  ; the shift is captured, by `letcc`.
  ;   A limitation: a resume from within a resumption of the same continuation enters its frames
  ; again while they are still active.  A `dynamic-wind` frame just runs its before thunk once
  ; more, but CHICKEN's `parameterize` frame swaps the parameter's value with the one it saved, so
  ; the nested resumption sees the value the parameter had where the first resumption entered the
  ; frame, not the binding of the `parameterize`.  Sequential resumes, as hansei, amb, generators
  ; and `(k (k x))` make, are not affected.
  ;   Only the continuation of the shift itself is delimited: a continuation captured by call/cc
  ; inside the segment, as `handle-exceptions`, `condition-case` and `guard` do (the first one
  ; even returns through it), carries the whole context of its capture.  Taken within a resumed
  ; continuation, it brings that context back: the parameters and the `dynamic-wind` frames around
  ; the original resetcc are entered again (their before thunks run) and those of the resume are
  ; left, for the rest of the resumed code, and its later shifts capture their whole context.
  ; Values still go to the resetcc of the resume, and conditions to the handlers around it.
  (define (delimcc-shift f)
    (let* ((cell (delimcc-meta-continuation-cell))
           (base (delimcc-frame-winds/fast (cdr cell))))
      (define (return vs)
        (when (delimcc-frame-gone?/fast (cdr cell)) (delimcc-settle! cell))
        (delimcc-escape cell (cdr cell) vs))
      (if (delimcc-within? base)
        (##sys#call-with-current-continuation
          (λ (c)
            (let1 (segment (delimcc-unwind! base))
              (receive vs (f (λ args (resetcc (delimcc-rewind! segment) (apply c args)))) (return vs)))))
        (letcc k
          (receive vs (f (λ args (resetcc (apply k args)))) (return vs))))))

  (define (delimcc-extract) (letcc/shift k k))
  (define (delimcc-discard . args) (letcc/shift _ (apply values args)))
  (define (delimcc-either lst) (letcc/shift k (map k lst)))
  (define (delimcc-either/map f lst) (letcc/shift k (map (λ (v) (let1 (v* (f v)) (k v*))) lst)))
  (define (delimcc-either/append f lst) (letcc/shift k (append-map (λ (v) (let1 (v* (f v)) (k v*))) lst)))
  (define (delimcc-either/filter pred? lst) (letcc/shift k (filter-map (λ (v) (let1 (v* (pred? v)) (and v* (k v*)))) lst)))
  (define (delimcc-compose . fns) (letcc/shift k (apply compose (cons k fns))))
  (define (delimcc-state-get) (letcc/shift k (λ (state) ((k state) state))))
  (define (delimcc-state-put v) (letcc/shift k (λ (state) ((k state) v))))
  (define-syntax-rule (delimcc-state-monad init body ...) 
    (let* ((t (τ body ...))
           (R (resetcc (let1 (result (t)) (λ (state) result)))))
      (R init)))
  (define (yield v) (letcc/shift k (cons v (k (void)))))
  (define (yield/extract v) (letcc/shift k (cons v k)))

  (define-syntax delimcc-foldr
    (syntax-rules ()
      ((delimcc-foldr bexpr ((each acc) fbody ...) body ...)
       (let* ((witness (gensym))
              (b bexpr)
              (f (λ (each acc) fbody ...)))
         (define (L r)
           (cond
             ((eq? r witness) b)
             (else (f (car r) (L (let1 (k (cdr r)) (k (void))))))))
         (L (resetcc body ... witness))))))
)

#|
(import (aux continuation delimited))
(resetcc (delimcc-either/filter (lambda (x) (and (number? x) (* x x))) '(a 1 b 3 c 7)))
(resetcc (cons 'a (delimcc-either/append (lambda (x) (list x (- x))) '(1 3 8))))
(letcc k (map k '(a 1 b 3 c 7)))

(define (tick . ticks) (letcc/shift k (λ (state) ((k state) (apply + state ticks)))))

(delimcc-state-monad 0
  (tick 1)
  (tick 1)
  (let1 (a (delimcc-state-get))
    (tick 1)
    (- (delimcc-state-get) a)))

(delimcc-state-monad 0 (- (begin (tick 1) (delimcc-state-get)) (begin (tick 1) (delimcc-state-get))))

(delimcc-state-monad 0
  (delimcc-state-put 3)
  (+ (begin
      (delimcc-state-get)
      (+ (delimcc-state-get) (delimcc-state-put 4))
      (delimcc-state-get))
    (delimcc-state-get)))

  (letcc k (k 1 2))
|#
