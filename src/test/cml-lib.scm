; The suites for the cml-lib layer of (aux cml): Multicast, SimpleRPC and TraceCML.  As in cml.scm,
; every case runs complete `run-cml` sessions and asserts outside of them, on what the threads
; recorded with `note!`.  No case depends on timing, threads are ordered by the scheduler only, but
; for one scaling check with a generous margin (test/watch/many-threads).

(import scheme
        (only (scheme base) parameterize)
        (only (chicken time) current-process-milliseconds)
        (except (chicken base) guard)
        (chicken condition)
        (chicken port)
        (chicken file)
        (chicken io)
        (chicken sort)
        (chicken string)
        (only srfi-1 iota filter every any make-list)
        (aux base)
        (aux unittest)
        (aux cml))

; helpers ---------------------------------------------------------------------------------------

; ⊦⧳ tolerates a condition, this one requires it
(define-syntax-rule (⊦raises (kind ...) body ...)
  (⊦= 'raised (condition-case (begin body ... 'not-raised) ((kind ...) 'raised))))

; the value of body, or 'raised when a condition of the given kinds escapes it
(define-syntax-rule (raised-or (kind ...) body ...)
  (condition-case (begin body ...) ((kind ...) 'raised)))

; run thunk under CML, returning (status . log) where log is what (note! x) collected, in order
(define cml-test/log '())
(define (note! x) (set! cml-test/log (cons x cml-test/log)))
(define (run/log thunk . args)
  (set! cml-test/log '())
  (let1 (status (apply run-cml thunk args))
    (cons status (reverse cml-test/log))))

; run thunk, stopping the run with its value
(define (run/value thunk) (run-cml (τ (cml/shutdown (thunk)))))

; the message of a condition
(define (message-of e) ((condition-property-accessor 'exn 'message) e))

; wait for every thread in tids
(define (join-all tids) (for-each (λ (t) (sync (join-evt t))) tids))

(define-suite cml-multicast-suite

  ((doc r) `((structure/section "Concurrent ML library: Multicast")
             (p "Asynchronous one-to-many channels, a port of " (code/inline "Multicast") " from cml-lib.")))

  ((test/predicates _)
   (let1 (mc (make-multicast-channel))
     (⊨ (multicast-channel? mc))
     (⊭ (multicast-channel? (make-channel)))
     (⊦= "#<multicast-channel>" (->string mc))
     (⊦= '(#t #f) (run/value (τ (let1 (p (multicast-port mc)) (list (multicast-port? p) (multicast-port? mc))))))
     (⊦raises (exn cml not-running) (multicast-port mc))           ; a port spawns its tee thread
     (multicast! mc 'nobody-listens)))                               ; but multicasting never blocks

  ((test/independent-speeds _)
   ; three ports, each read at its own pace, see the whole stream in order
   (let1 (got (run/value (τ (let* ((mc (make-multicast-channel))
                                   (ps (map (λ (i) (multicast-port mc)) (iota 3)))
                                   (logs (make-vector 3 '()))
                                   (reader (λ (i p pause)
                                             (spawn (τ (let loop ((n 6))
                                                         (unless (zero? n)
                                                           (for-each (λ (j) (cml/yield)) (iota pause))
                                                           (vector-set! logs i (cons (multicast-recv p) (vector-ref logs i)))
                                                           (loop (sub1 n)))))))))
                              (let1 (ts (list (reader 0 (car ps) 0) (reader 1 (cadr ps) 3)))
                                (for-each (λ (v) (multicast! mc v)) '(1 2 3 4 5 6))   ; the sender never waits
                                (let1 (late (reader 2 (caddr ps) 1))                   ; starts reading afterwards
                                  (join-all (cons late ts))
                                  (map reverse (vector->list logs))))))))
     (⊦= '((1 2 3 4 5 6) (1 2 3 4 5 6) (1 2 3 4 5 6)) got)))

  ((test/new-port-sees-later-messages _)
   (⊦= '((a b c) (c) (d))
       (run/value (τ (let* ((mc (make-multicast-channel))
                            (p1 (multicast-port mc)))
                       (multicast! mc 'a)
                       (multicast! mc 'b)
                       (let1 (p2 (multicast-port mc))
                         (multicast! mc 'c)
                         (let1 (p3 (multicast-port mc))
                           (multicast! mc 'd)
                           (list (map (λ (i) (multicast-recv p1)) '(1 2 3))
                                 (list (multicast-recv p2))
                                 (list (multicast-recv p3))))))))))

  ((test/copy-port _)
   ; the copy starts at the original port's read position, not at "now"
   (⊦= '(1 (2 3 4) (2 3 4) 5)
       (run/value (τ (let* ((mc (make-multicast-channel))
                            (p (multicast-port mc)))
                       (for-each (λ (v) (multicast! mc v)) '(1 2 3))
                       (let1 (first (multicast-recv p))
                         (let1 (c (multicast-copy-port p))            ; p has consumed 1 only
                           (multicast! mc 4)
                           (let* ((from-c (map (λ (i) (multicast-recv c)) '(1 2 3)))
                                  (from-p (map (λ (i) (multicast-recv p)) '(1 2 3)))
                                  (cc (multicast-copy-port (multicast-copy-port c)))) ; nothing unread: now
                             (multicast! mc 5)
                             (list first from-c from-p (multicast-recv cc)))))))))
   ; a copy made before anything was read sees everything its original will see
   (⊦= '((x y) (x y))
       (run/value (τ (let* ((mc (make-multicast-channel))
                            (p (multicast-port mc))
                            (c (multicast-copy-port p)))
                       (multicast! mc 'x)
                       (multicast! mc 'y)
                       (list (list (multicast-recv p) (multicast-recv p))
                             (list (multicast-recv c) (multicast-recv c))))))))

  ((test/recv-evt _)
   (⊦= '(timeout (got 7) (got 8) timeout)
       (cdr (run/log (τ (let* ((mc (make-multicast-channel))
                               (p (multicast-port mc))
                               (poll (τ (note! (sync/timeout (wrap (multicast-recv-evt p) (λ (v) (list 'got v)))
                                                             0.01 'timeout)))))
                          (poll)
                          (multicast! mc 7)
                          (multicast! mc 8)
                          (poll)
                          (note! (list 'got (sync (multicast-recv-evt p))))
                          (poll)
                          (cml/shutdown)))))))

  ((test/shared-port _)
   ; two readers of one port share its messages: each message is delivered once
   (let1 (got (run/value (τ (let* ((mc (make-multicast-channel))
                                   (p (multicast-port mc))
                                   (acc '())
                                   (ts (map (λ (i) (spawn (τ (let loop ((n 4))
                                                               (unless (zero? n)
                                                                 (set! acc (cons (multicast-recv p) acc))
                                                                 (loop (sub1 n)))))))
                                            '(1 2))))
                              (for-each (λ (v) (multicast! mc v)) (iota 8))
                              (join-all ts)
                              acc))))
     (⊦= (iota 8) (sort got <))))

  ((test/many-messages _)
   (⊦= (iota 500)
       (run/value (τ (let* ((mc (make-multicast-channel))
                            (p (multicast-port mc)))
                       (spawn (τ (for-each (λ (v) (multicast! mc v)) (iota 500))))
                       (map (λ (i) (multicast-recv p)) (iota 500)))))))

  )

(define-suite cml-rpc-suite

  ((doc r) `((structure/section "Concurrent ML library: SimpleRPC")
             (p "Server-side RPC entry points, a port of " (code/inline "SimpleRPC")
                ": every maker returns " (code/inline "(values call entry-evt)") ".")))

  ((test/rpc _)
   (⊦= '((c1 1) (c1 9) (c2 4) served-3)
       (cdr (run/log (τ (receive (call entry-evt) (make-rpc (λ (x) (* x x)))
                          (let* ((server (spawn (τ (for-each (λ (i) (sync entry-evt)) '(1 2 3)))))
                                 (c1-log '())
                                 (c2-log '())
                                 (c1 (spawn (τ (set! c1-log (list (call 1) (call 3))))))
                                 (c2 (spawn (τ (set! c2-log (list (call 2)))))))
                            (join-all (list c1 c2 server))
                            (for-each (λ (v) (note! (list 'c1 v))) c1-log)
                            (for-each (λ (v) (note! (list 'c2 v))) c2-log)
                            (note! 'served-3)
                            (cml/shutdown))))))))

  ((test/rpc/fifo _)
   ; requests queued before the server looks are served in mailbox order
   (⊦= '(a b c d)
       (run/value (τ (receive (call entry-evt) (make-rpc (λ (x) (note! x) x))
                       (set! cml-test/log '())
                       (let1 (ts (map (λ (x) (spawn (τ (call x)))) '(a b c d)))
                         (for-each (λ (i) (sync entry-evt)) (iota 4))
                         (join-all ts)
                         (reverse cml-test/log)))))))

  ((test/rpc/in _)
   ; the state is given to the entry event and passed to f unchanged
   (⊦= '((1 . 100) (2 . 100) (3 . 200))
       (run/value (τ (receive (call entry-evt) (make-rpc/in (λ (x st) (cons x st)))
                       (spawn (τ (sync (entry-evt 100)) (sync (entry-evt 100)) (sync (entry-evt 200))))
                       (map call '(1 2 3)))))))

  ((test/rpc/out _)
   ; f computes the new state, which is the entry event's value
   (⊦= '(((ack a) (ack b)) (b a))
       (run/value (τ (receive (call entry-evt) (make-rpc/out (λ (x) (values (list 'ack x) x)))
                       (let* ((states '())
                              (server (spawn (τ (for-each (λ (i) (set! states (cons (sync entry-evt) states))) '(1 2))))))
                         (let1 (replies (map call '(a b)))
                           (sync (join-evt server))
                           (list replies states))))))))

  ((test/rpc/in-out _)
   ; an accumulator threaded through the server loop
   (⊦= '((1 3 6 10) 10)
       (run/value (τ (receive (call entry-evt) (make-rpc/in-out (λ (x acc) (let1 (s (+ x acc)) (values s s))))
                       (let* ((final #f)
                              (server (spawn (τ (let loop ((acc 0) (n 4))
                                                  (if (zero? n) (set! final acc) (loop (sync (entry-evt acc)) (sub1 n))))))))
                         (let1 (replies (map call '(1 2 3 4)))
                           (sync (join-evt server))
                           (list replies final))))))))

  ((test/rpc/state _)
   (⊦= '((1 2 3) (count-1 count-2 count-3))
       (run/value (τ (receive (call entry-evt) (make-rpc/state 0 (λ (x n) (values (add1 n) (add1 n))))
                       (let* ((states '())
                              (server (spawn (τ (for-each (λ (i) (set! states (cons (sync entry-evt) states))) '(1 2 3))))))
                         (let1 (replies (map call '(x y z)))
                           (sync (join-evt server))
                           (list replies (map (λ (n) (string->symbol (string-append "count-" (number->string n))))
                                              (reverse states))))))))))

  ((test/rpc/state/shared _)
   ; the state is read and written when a request is served, so server threads sharing the entry
   ; event never lose an update
   (⊦= '(1 2 3 3)
       (run/value (τ (receive (call entry-evt) (make-rpc/state 0 (λ (x st) (values (+ st x) (+ st x))))
                       (for-each (λ (i) (spawn (τ (let loop () (sync entry-evt) (loop))))) '(1 2))
                       (let1 (replies (map call '(1 1 1)))
                         (append replies (list (call 0))))))))
   ; with concurrent callers too: the new state is stored before the caller is answered (the put
   ; of the answer switches to the caller, and may preempt the server), so no other server thread
   ; serves a request from the old state meanwhile.  Every reply of a counter is distinct
   (for-each (λ (quantum)
               (let1 (replies (run-cml (τ (receive (call entry-evt) (make-rpc/state 0 (λ (x st) (values (add1 st) (add1 st))))
                                             (for-each (λ (i) (spawn (τ (let loop () (sync entry-evt) (loop))))) (iota 5))
                                             (let ((done (make-mailbox)) (replies '()))
                                               (for-each (λ (i) (spawn (τ (for-each (λ (j) (push! (call j) replies)) (iota 40))
                                                                          (mailbox-send! done #t))))
                                                         (iota 5))
                                               (for-each (λ (i) (mailbox-recv done)) (iota 5))
                                               (cml/shutdown replies))))
                                       #:quantum quantum))
                 (⊦= (iota 200 1) (sort replies <))))
             '(1 3 64)))

  ((test/rpc/values _)
   ; f returning no value answers (void); with a state, anything but a result and a new state is
   ; answered with (exn cml rpc) and the server goes on with its old state
   (⊦= (list (void) 'raised 5 'raised 7)
       (run/value (τ (receive (call entry-evt) (make-rpc (λ (x) (values)))
                       (receive (call2 entry2) (make-rpc/in-out (λ (x st) (if (zero? x) st (values (+ x st) (+ x st)))))
                         (receive (call3 entry3) (make-rpc/state 0 (λ (x st)
                                                                     (if (zero? x) (values) (values (+ x st) (+ x st)))))
                           (spawn (τ (let loop () (sync entry-evt) (loop))))
                           (spawn (τ (let loop ((st 0)) (loop (sync (entry2 st))))))
                           (spawn (τ (let loop () (sync entry3) (loop))))
                           (list (call 1)
                                 (raised-or (exn cml rpc) (call2 0))
                                 (call2 5)
                                 (raised-or (exn cml rpc) (call3 0))
                                 (call3 7)))))))))

  ((test/rpc/select-loop _)
   ; entry events mix with other events in the server's select
   (⊦= '((get 0) (get 5) stopped)
       (cdr (run/log (τ (receive (call-get get-evt) (make-rpc/in (λ (ignored st) st))
                          (receive (call-add add-evt) (make-rpc/in-out (λ (x st) (values (void) (+ x st))))
                            (let* ((stop (make-channel))
                                   (server (spawn (τ (let loop ((st 0))
                                                       (select/case
                                                         ((get-evt st) () (loop st))
                                                         ((add-evt st) (new) (loop new))
                                                         ((recv-evt stop) () (note! 'stopped))))))))
                              (note! (list 'get (call-get #f)))
                              (call-add 2)
                              (call-add 3)
                              (note! (list 'get (call-get #f)))
                              (send stop #t)
                              (sync (join-evt server))
                              (cml/shutdown)))))))))

  ((test/rpc/exceptions _)
   ; f's exception reaches the caller and the server goes on
   (⊦= '((raised "no zero" 0) (ok 1/2) (raised "no zero" 0) (ok 1/4) served)
       (cdr (run/log (τ (receive (call entry-evt) (make-rpc (λ (x) (if (zero? x) (error "no zero" x) (/ 1 x))))
                          (let1 (server (spawn (τ (for-each (λ (i) (sync entry-evt)) (iota 4)))))
                            (for-each (λ (x)
                                        (note! (condition-case (list 'ok (call x))
                                                 (e (exn) (list 'raised (message-of e) x)))))
                                      '(0 2 0 4))
                            (sync (join-evt server))
                            (note! 'served)
                            (cml/shutdown)))))))
   ; a non-condition object is delivered as it is
   (⊦= '(caught boom)
       (run/value (τ (receive (call entry-evt) (make-rpc (λ (x) (abort x)))
                       (spawn (τ (sync entry-evt)))
                       (handle-exceptions e (list 'caught e) (call 'boom)))))))

  ((test/rpc/exceptions/state _)
   ; in-out keeps the old state when f raises; out re-raises in the server as well
   (⊦= '((1 raised 3) 3)
       (run/value (τ (receive (call entry-evt) (make-rpc/in-out (λ (x acc) (when (negative? x) (error "negative" x))
                                                                (values (+ x acc) (+ x acc))))
                       (let* ((final #f)
                              (server (spawn (τ (let loop ((acc 0) (n 3))
                                                  (if (zero? n) (set! final acc) (loop (sync (entry-evt acc)) (sub1 n))))))))
                         (let1 (replies (map (λ (x) (condition-case (call x) ((exn) 'raised))) '(1 -5 2)))
                           (sync (join-evt server))
                           (list replies final)))))))
   (⊦= '((server raised "odd") raised)
       (cdr (run/log (τ (receive (call entry-evt) (make-rpc/out (λ (x) (error "odd" x)))
                          (let1 (server (spawn (τ (condition-case (sync entry-evt)
                                                    (e (exn) (note! (list 'server 'raised (message-of e))))))))
                            (let1 (r (condition-case (call 1) ((exn) 'raised)))
                              (sync (join-evt server))
                              (note! r))
                            (cml/shutdown))))))))

  )

(define-suite cml-trace-suite

  ((doc r) `((structure/section "Concurrent ML library: TraceCML")
             (p "Trace modules, thread watching and uncaught exception handlers, a port of "
                (code/inline "TraceCML") " with its bugs fixed.")))

  ((test/modules _)
   (let* ((a (trace-module trace-module/root "lib-a"))
          (b (trace-module a "b"))
          (c (trace-module b "c")))
     (⊨ (trace-module? a))
     (⊭ (trace-module? "/lib-a/"))
     (⊭ (trace-module? (make-channel)))
     (⊦= "/" (trace-module-name trace-module/root))
     (⊦= "/lib-a/" (trace-module-name a))
     (⊦= "/lib-a/b/c/" (trace-module-name c))
     (⊦= "#<trace-module /lib-a/b/>" (->string b))
     (⊦= "/ThreadWatcher/" (trace-module-name trace-watcher))
     (⊨ (eq? b (trace-module a "b")))                              ; idempotent
     (⊨ (eq? b (trace-module "/lib-a/" "b")))                      ; a parent can be named
     (⊨ (eq? c (trace-module-of "/lib-a/b/c/")))
     (⊨ (eq? c (trace-module-of "lib-a//b/c")))                    ; empty arcs are ignored
     (⊨ (eq? trace-module/root (trace-module-of "/")))
     (⊨ (eq? trace-module/root (trace-module-of "")))
     (⊦raises (exn cml no-such-module) (trace-module-of "/lib-a/nope/"))
     (⊦raises (exn cml trace) (trace-module a "x/y"))
     (⊦raises (exn cml trace) (trace-module a ""))
     ; inside CML too, the error is the caller's (ML's trace server died, blocking the caller)
     (⊦= '(raised ok) (run/value (τ (list (raised-or (exn cml no-such-module) (trace-module-of "/nope/"))
                                          (and (eq? c (trace-module-of "/lib-a/b/c")) 'ok)))))))

  ((test/on-off-hierarchy _)
   (let* ((a (trace-module trace-module/root "lib-h"))
          (b (trace-module a "b"))
          (c (trace-module b "c"))
          (d (trace-module a "d"))
          (flags (τ (map tracing? (list a b c d)))))
     (⊦= '(#f #f #f #f) (flags))
     (trace-on! b)
     (⊦= '(#f #t #t #f) (flags))
     (⊨ (tracing? (trace-module c "new")))                          ; a new child inherits its parent's flag
     (⊭ (tracing? (trace-module d "new")))
     (trace-on! a)
     (⊦= '(#t #t #t #t) (flags))
     (trace-off! b)
     (⊦= '(#t #f #f #t) (flags))
     (trace-off! "/lib-h/")
     (trace-on-only! b)                                               ; not its descendants
     (⊦= '(#f #t #f #f) (flags))
     ; status: pre-order, children newest first as in ML
     (⊦= '(("/lib-h/" . #f) ("/lib-h/d/" . #f) ("/lib-h/d/new/" . #f) ("/lib-h/b/" . #t)
           ("/lib-h/b/c/" . #f) ("/lib-h/b/c/new/" . #f))
         (map (λ (p) (cons (trace-module-name (car p)) (cdr p))) (trace-status a)))
     (⊨ (tracing? trace-watcher))                                     ; on at load time
     (trace-off! a)))

  ((test/trace-output _)
   (let* ((m (trace-module trace-module/root "lib-out"))
          (calls 0)
          (msg (τ (set! calls (add1 calls)) (list "x=" 1 " " 'y "\n"))))
     (⊦= "" (with-output-to-string (τ (trace m msg))))
     (⊦= 0 calls)                                                     ; thunk not even called when off
     (dynamic-wind
       (τ (trace-on! m))
       (τ (⊦= "x=1 y\n" (with-output-to-string (τ (trace m msg))))   ; 'out, also outside CML
          (⊦= "x=1 y\n" (with-error-output-to-string (τ (parameterize ((trace-to 'err)) (trace m msg)))))
          (⊦= "" (with-output-to-string (τ (parameterize ((trace-to 'null)) (trace m msg)))))
          (⊦= "plain" (call-with-output-string (λ (port) (parameterize ((trace-to port)) (trace m (τ "plain"))))))
          ; a long list is concatenated without apply (CHICKEN's apply hangs on some 32000 arguments)
          (⊦= 50000 (string-length (call-with-output-string
                                     (λ (port) (parameterize ((trace-to port)) (trace m (τ (make-list 50000 "a"))))))))
          (⊦= 3 calls)                                                ; 'null drops the output, as ML
          (⊦raises (exn cml trace) (trace-to 42))
          ; inside threads, ordered by the scheduler
          (⊦= "t1\nt2\nmain\n"
              (with-output-to-string
                (τ (run-cml (τ (join-all (map (λ (s) (spawn (τ (trace m (τ (list s "\n")))))) '("t1" "t2")))
                               (trace m (τ '("main\n")))
                               (cml/shutdown))))))
          ; channel and mailbox destinations, only while running
          (⊦= '("to-ch 1" "to-mb 2")
              (run/value (τ (let ((ch (make-channel)) (mb (make-mailbox)))
                              (spawn (τ (parameterize ((trace-to ch)) (trace m (τ '("to-ch " 1)))))) ; blocks until received
                              (parameterize ((trace-to mb)) (trace m (τ '("to-mb " 2))))
                              (list (recv ch) (mailbox-recv mb))))))
          (let1 (mb (make-mailbox)) (parameterize ((trace-to mb)) (trace m msg))))   ; dropped, no error
       (τ (trace-off! m)))))

  ((test/trace-file _)
   ; a fresh temporary file, removed whatever happens: concurrent runs of the suite do not share it
   (let* ((m (trace-module trace-module/root "lib-file"))
          (file (create-temporary-file ".txt")))
     (dynamic-wind
       void
       (τ (trace-on! m)
          (parameterize ((trace-to file))
            (run-cml (τ (trace m (τ '("one\n"))) (trace m (τ '("two\n"))) (cml/shutdown)))
            ; closed at shutdown, then reopened to append by the next run
            (run-cml (τ (trace m (τ '("three\n"))) (cml/shutdown))))
          (⊦= "one\ntwo\nthree\n" (with-input-from-file file (τ (read-string #f))))
          ; a file that cannot be opened redirects to stdout, with a warning
          (let* ((err #f)
                 (out (with-output-to-string
                        (τ (set! err (with-error-output-to-string
                                       (τ (parameterize ((trace-to "/nonexistent-dir/x/trace.txt"))
                                            (trace m (τ '("lost\n")))
                                            (trace m (τ '("found\n")))))))))))
            (⊦= "lost\nfound\n" out)
            (⊨ (and (substring-index "unable to open" err) #t))))
       (τ (trace-close-files!)
          (trace-off! m)
          (delete-file* file)))))

  ((test/trace-file-closed-after-the-servers _)
   ; the shutdown functions of logged servers run before the trace files are closed: one that
   ; traces to a file does not leave it open after the session (as ML's tracerStop, a server
   ; logged first, is shut down last)
   (let* ((m (trace-module trace-module/root "lib-file-servers"))
          (file (create-temporary-file ".txt")))
     (dynamic-wind
       void
       (τ (trace-on! m)
          (cml/log-server! "trace-test-server" void (τ (trace m (τ '("server shut\n")))))
          (parameterize ((trace-to file))
            (run-cml (τ (trace m (τ '("in run\n"))) (cml/shutdown))))
          (⊦= '() %trace-files)
          (⊦= "in run\nserver shut\n" (with-input-from-file file (τ (read-string #f)))))
       (τ (cml/unlog-server! "trace-test-server")
          (trace-close-files!)
          (trace-off! m)
          (delete-file* file)))))

  ((test/watch _)
   (let1 (out (with-output-to-string
                (τ (⊦= '(#t #f done)
                       (cdr (run/log (τ (let* ((c (make-channel))
                                               (t (spawn (τ (recv c)))))
                                          (watch "worker" t)
                                          (note! (watched? t))
                                          (send c 'die)
                                          (cml/yield)
                                          (note! (watched? t))
                                          (note! 'done)
                                          (cml/shutdown)))))))))
     (⊦= "WARNING!  Watched thread worker [000003] has died.\n" out)))

  ((test/watch/many-threads _)
   ; a watch is kept on the thread itself: watching n threads, and their deaths, cost O(n) (a list
   ; of the watches made each watch and each death O(n): 16000 threads took 30 s compiled).  Ten
   ; times as many threads must not take thirty times as long
   (define (cost n)
     (let1 (t0 (current-process-milliseconds))
       (⊦= "" (with-output-to-string
                (τ (run-cml (τ (let* ((iv (make-ivar))
                                      (ts (map (λ (i) (spawn (τ (ivar-get iv)))) (iota n))))
                                 (for-each (λ (t) (watch "w" t)) ts)
                                 (unless (every watched? ts) (print "not watched"))
                                 (ivar-put! iv #t)
                                 (join-all ts)
                                 (cml/yield)
                                 (when (any watched? ts) (print "still watched"))
                                 (cml/shutdown)))))))
       (- (current-process-milliseconds) t0)))
   (dynamic-wind
     (τ (trace-off! trace-watcher))
     (τ (let* ((small (max 20 (cost 400)))
               (large (cost 4000)))
          (⊨ (< large (* 30 small)))))
     (τ (trace-on! trace-watcher))))

  ((test/unwatch _)
   ; unwatching, even right after watching (ML's race), silences the watch
   (⊦= "" (with-output-to-string
            (τ (run-cml (τ (let* ((c (make-channel))
                                  (t1 (spawn (τ (recv c))))
                                  (t2 (spawn (τ (recv c)))))
                             (watch "t1" t1)
                             (cml/yield)
                             (unwatch t1)
                             (watch "t2" t2)
                             (unwatch t2)
                             (unwatch t2)                             ; unwatching twice is harmless
                             (send c 1) (send c 2)
                             (cml/sleep 0)
                             (cml/yield)
                             (cml/shutdown)))))))
   ; the watcher off: no message (and on again even if the assertion fails)
   (dynamic-wind
     (τ (trace-off! trace-watcher))
     (τ (⊦= "" (with-output-to-string (τ (run-cml (τ (watch "quiet" (spawn void)) (cml/yield) (cml/shutdown)))))))
     (τ (trace-on! trace-watcher))))

  ((test/watch/after-death _)
   ; ML's watcher server blocked forever after the first death: here later watches still work
   (let1 (out (with-output-to-string
                (τ (⊦= '(#f #f #f)
                       (run/value (τ (let* ((ts (map (λ (i) (spawn (τ (cml/yield)))) '(1 2 3)))
                                            (c (make-channel))
                                            (u (spawn (τ (recv c)))))
                                       (for-each (λ (t n) (watch n t)) ts '("a" "b" "c"))
                                       (watch "u" u)
                                       (watch "u-again" u)             ; replaces the previous watch
                                       (join-all ts)
                                       (cml/yield)
                                       (unwatch (car ts))              ; already gone
                                       (send c 'go)
                                       (sync (join-evt u))
                                       (cml/yield)
                                       (map watched? (list (car ts) u (cadr ts))))))))))
     (⊦= '("a" "b" "c" "u-again")
         (map (λ (l) (cadddr (string-split l " "))) (filter (λ (l) (substring-index "WARNING" l)) (string-split out "\n"))))))

  ((test/watch/exception _)
   ; a thread dying of an exception is reported as well
   (let* ((err #f)
          (out (with-output-to-string
                 (τ (set! err (with-error-output-to-string
                                (τ (run-cml (τ (let1 (t (spawn (τ (cml/yield) (error "crash"))))
                                                 (watch "crasher" t)
                                                 (sync (join-evt t))
                                                 (cml/yield)
                                                 (cml/shutdown)))))))))))
     (⊨ (and (substring-index "Watched thread crasher" out) #t))
     (⊨ (and (substring-index "crash" err) #t))))

  ((test/uncaught/default _)
   (⊨ (eq? trace-exn-handler (default-exn-handler)))
   (let1 (err (with-error-output-to-string
                (τ (⊦= '(joined) (cdr (run/log (τ (sync (join-evt (spawn (τ (error "kaboom" 7)))))
                                                  (note! 'joined)
                                                  (cml/shutdown))))))))
     (⊨ (and (substring-index "kaboom" err) #t))
     (⊨ (and (substring-index "[000003]" err) #t)))
   ; the default handler of the registry, called directly
   (let1 (err (with-error-output-to-string
                (τ (uncaught-default-handler (current-tid) (make-property-condition 'exn 'message "direct")))))
     (⊨ (and (substring-index "[-000001] died of an uncaught exception" err) #t))
     (⊨ (and (substring-index "direct" err) #t))))

  ((test/uncaught/handlers _)
   (dynamic-wind
     void
     (τ (add-uncaught-handler! (λ (tid e) (and (condition? e) ((condition-predicate 'type) e) (note! (list 'h1 (tid->string tid))) #t)))
        (add-uncaught-handler! (λ (tid e) (and (symbol? e) (note! (list 'h2 e)) #t)))
        (add-uncaught-handler! (λ (tid e) (and (eq? e 'bad-handler) (error "handler fails"))))
        (set-uncaught-handler! (λ (tid e) (note! (list 'default (if (symbol? e) e (message-of e))))))
        ; newest first; the default when nobody handles it or a handler raises; all before join
        (⊦= '((h2 sym) j1 (h1 "[000004]") j2 (default "other") j3 (default bad-handler) j4)
            (cdr (run/log (τ (let1 (die (λ (thunk j) (sync (join-evt (spawn thunk))) (note! j)))
                               (die (τ (abort 'sym)) 'j1)
                               (die (τ (car 1)) 'j2)
                               (die (τ (error "other")) 'j3)
                               (die (τ (abort 'bad-handler)) 'j4)
                               (cml/shutdown))))))
        ; exceptions of the default one are swallowed as well
        (set-uncaught-handler! (λ (tid e) (error "default fails")))
        (⊦= '(alive) (cdr (run/log (τ (sync (join-evt (spawn (τ (error "x"))))) (note! 'alive) (cml/shutdown)))))
        ; parameterizing default-exn-handler bypasses the registry
        (⊦= '(own) (cdr (run/log (τ (parameterize ((default-exn-handler (λ (e) (note! 'own))))
                                      (sync (join-evt (spawn (τ (abort 'sym))))))
                                    (cml/shutdown))))))
     reset-uncaught-handlers!)
   ; reset: the default message again, no handlers
   (let1 (err (with-error-output-to-string (τ (run-cml (τ (sync (join-evt (spawn (τ (error "after reset")))))
                                                          (cml/shutdown))))))
     (⊨ (and (substring-index "after reset" err) #t))))

  )

(unittest/✓ cml-multicast-suite)
(unittest/✓ cml-rpc-suite)
(unittest/✓ cml-trace-suite)
