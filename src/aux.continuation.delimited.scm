
(module (aux continuation delimited) *

  (import scheme
          (only (scheme base) make-parameter)
          (chicken base)
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

  ; A resetcc: `escape` returns to it, `parent` is the frame current outside of it and `winds` is
  ; the `##sys#dynamic-winds` of its body.  The root frame stands for no resetcc at all.
  (define-record delimcc-frame escape parent winds)
  (define delimcc-root-frame (make-delimcc-frame delimcc-missing-reset #f #f))

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

  (define (delimcc-meta-continuation-cell)
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

  (define (delimcc-current-frame) (cdr (delimcc-meta-continuation-cell)))

  ; Give the values of a resetcc body to the resetcc current now, which is not the lexically
  ; enclosing one when the body is part of a resumed shift continuation (in this thread or in
  ; another one).
  (define (delimcc-return args) (apply (delimcc-frame-escape (delimcc-current-frame)) args))

  (define (delimcc-reset thunk)
    (let1 (cell (delimcc-meta-continuation-cell))
      (letcc k
        (let* ((parent (cdr cell))
               (frame (make-delimcc-frame (λ args (set-cdr! cell parent) (apply k args))
                                          parent
                                          ##sys#dynamic-winds)))
          (set-cdr! cell frame)
          (receive args (thunk) (delimcc-return args))))))

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

  (define (delimcc-within? base)
    (let loop ((ws ##sys#dynamic-winds))
      (cond
        ((eq? ws base) #t)
        ((pair? ws) (loop (cdr ws)))
        (else #f))))

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
  ; Values still go to the resetcc of the resume.
  (define (delimcc-shift f)
    (let1 (cell (delimcc-meta-continuation-cell))
      (define (return vs) (apply (delimcc-frame-escape (cdr cell)) vs))
      (let1 (base (delimcc-frame-winds (cdr cell)))
        (if (delimcc-within? base)
          (##sys#call-with-current-continuation
            (λ (c)
              (let1 (segment (delimcc-unwind! base))
                (receive vs (f (λ args (resetcc (delimcc-rewind! segment) (apply c args)))) (return vs)))))
          (letcc k
            (receive vs (f (λ args (resetcc (apply k args)))) (return vs)))))))

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
      ((delimcc-fold bexpr ((each acc) fbody ...) body ...)
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
