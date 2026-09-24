; The suites for (aux cml), the Concurrent ML core.  Every case runs one or more complete
; `run-cml` sessions and asserts OUTSIDE of them: an assertion failing inside a CML thread would
; only reach that thread's exception handler, so threads record what they observe in plain lists
; and the assertions look at those lists once `run-cml` has returned.  Timing-dependent cases use
; generous margins, everything else is deterministic (the scheduler is).

(import scheme
        (only (scheme base) parameterize)
        (except (chicken base) guard)
        (chicken condition)
        (only (chicken time) current-process-milliseconds)
        (chicken port)
        (chicken process)
        (chicken file posix)
        (chicken bytevector)
        (chicken io)
        (chicken sort)
        (chicken string)
        (only srfi-1 iota filter count last make-list append-map)
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

(define-suite cml-events-suite

  ((doc r) `((structure/section "Concurrent ML: events, sync and select")
             (p "A port of SML/NJ " (code/inline "CML") " (John Reppy) to CHICKEN on first-class continuations, "
                "see the header of " (code/inline "aux.cml.scm") " for the design.")))

  ((test/version _)
   (⊦= "Concurrent ML (aux cml)" (cdr (assq 'system cml/version)))
   (⊦= '(1 0 10) (cdr (assq 'version-id cml/version)))
   (⊦= "Concurrent ML (aux cml), Version 1.0.10, September 15, 1997" cml/banner))

  ((test/run-status _)
   (⊦= 'failure (run-cml (τ (sync never-evt))))           ; deadlock
   (⊦= 'failure (run-cml (τ (void))))                     ; all threads done: deadlock too, as in ML
   (⊦= 'done (run-cml (τ (cml/shutdown 'done))))
   (⊦= 'success (run-cml (τ (cml/shutdown))))
   (⊦= 'late (run-cml (τ (spawn (τ (cml/sleep 0.01) (cml/shutdown 'late)))
                         (sync never-evt))))
   (⊭ (cml-running?))
   (⊦= '(#t) (cdr (run/log (τ (note! (cml-running?)) (cml/shutdown))))))

  ((test/not-running _)
   (⊦raises (exn cml not-running) (sync (always-evt 1)))
   (⊦raises (exn cml not-running) (spawn void))
   (⊦raises (exn cml not-running) (recv (make-channel)))
   (⊦raises (exn cml not-running) (cml/shutdown))
   (⊦= '(raised) (cdr (run/log (τ (note! (raised-or (exn cml running) (run-cml void))) (cml/shutdown)))))
   ; a failed call leaves nothing behind that a later call could resume
   (let1 (ch (make-channel))
     (⊦raises (exn cml not-running) (recv ch))
     (⊦raises (exn cml not-running) (send ch 42))
     (⊦raises (exn cml not-running) (send-poll ch 1))
     (⊦raises (exn cml not-running) (recv-poll ch)))
   (let ((iv (make-ivar)) (mv (make-mvar)) (mb (make-mailbox)))
     (⊦raises (exn cml not-running) (ivar-get iv))
     (ivar-put! iv 1)                                       ; puts work, and wake nobody
     (⊦= '(1) (ivar-get-poll iv))
     (⊦raises (exn cml not-running) (mvar-take! mv))
     (⊦raises (exn cml not-running) (mvar-get mv))
     (⊦raises (exn cml not-running) (mvar-swap! mv 2))
     (mvar-put! mv 3)
     (⊦= '(3) (mvar-get-poll mv))
     (⊦raises (exn cml not-running) (mailbox-recv mb))
     (⊦raises (exn cml not-running) (mailbox-send! mb 4))
     (⊦= '() (mailbox-recv-poll mb)))
   (⊦= 'ok (run/value (τ 'ok))))                          ; state is sane after all of this

  ((test/run-cml/escape _)
   ; leaving run-cml through a continuation ends the session and CML can run again
   (⊦= 'escaped (call-with-current-continuation (λ (k) (run-cml (τ (spawn (τ (k 'escaped))) (sync never-evt))))))
   (⊭ (cml-running?))
   (⊦= 'ok (run/value (τ 'ok))))

  ((test/internal-names _)
   ; the scheduler hooks for later layers are exported with a `%`, generic names stay free for clients
   (define (bound? name) (condition-case (begin (eval name) #t) (ignored () #f)))
   (⊦= '(#f #f #f #f #t #t #t #t)
       (map bound? '(atomic-begin atomic-end atomic-dispatch dispatch
                     %atomic-begin %atomic-end %atomic-dispatch %dispatch))))

  ((test/rendezvous/receiver-first _)
   (⊦= '((got x) (state non-atomic) sent)
       (cdr (run/log (τ (let1 (ch (make-channel))
                          (spawn (τ (note! (list 'got (recv ch)))    ; child first: blocks in recv
                                    (note! (list 'state %atomic-state))))
                          (send ch 'x)                               ; the receiver runs at once
                          (note! 'sent)
                          (cml/yield)
                          (cml/shutdown)))))))

  ((test/rendezvous/sender-first _)
   (⊦= '((got y) sent)
       (cdr (run/log (τ (let* ((ch (make-channel))
                               (t (spawn (τ (send ch 'y) (note! 'sent)))))
                          (note! (list 'got (recv ch)))              ; the receiver continues first
                          (sync (join-evt t))
                          (cml/shutdown)))))))

  ((test/rendezvous/events _)
   (⊦= '(1 2 3 (sent-evt))
       (cdr (run/log (τ (let1 (ch (make-channel))
                          (spawn (τ (send ch 1) (sync (send-evt ch 2)) (sync (send-evt ch 3)) (note! '(sent-evt))))
                          (note! (sync (recv-evt ch)))
                          (note! (recv ch))
                          (cml/yield)
                          (note! (sync (recv-evt ch)))
                          (cml/yield)
                          (cml/shutdown)))))))

  ((test/polls _)
   (⊦= '(#f () got-a #t (b) #f)
       (cdr (run/log (τ (let1 (ch (make-channel))
                          (note! (send-poll ch 'a))                  ; nobody is waiting
                          (note! (recv-poll ch))
                          (spawn (τ (recv ch) (note! 'got-a)))       ; a receiver blocks
                          (note! (send-poll ch 'a))
                          (spawn (τ (send ch 'b)))                   ; a sender blocks
                          (note! (recv-poll ch))
                          (note! (send-poll ch 'c))                  ; polls never meet each other
                          (cml/shutdown)))))))

  ((test/polls/tick _)
   ; every poll is a clock tick, the read-only ones too: a thread busy-waiting on a poll is
   ; preempted, so the thread that fills the variable runs (it never did for ivar/mvar-get-poll)
   (define (spins poll make put!)
     (run/value (τ (let1 (v (make))
                     (spawn (τ (cml/yield) (put! v 'done)))
                     (let loop ((n 0))
                       (cond ((pair? (poll v)) 'filled) ((< n 100000) (loop (add1 n))) (else 'starved)))))))
   (⊦= 'filled (spins ivar-get-poll make-ivar ivar-put!))
   (⊦= 'filled (spins mvar-get-poll make-mvar mvar-put!))
   (⊦= 'filled (spins mvar-take-poll make-mvar mvar-put!))
   (⊦= 'filled (spins mailbox-recv-poll make-mailbox mailbox-send!))
   ; and they still work outside run-cml
   (let1 (iv (make-ivar))
     (⊦= '() (ivar-get-poll iv))
     (ivar-put! iv 1)
     (⊦= '(1) (ivar-get-poll iv))
     (⊦= '(2) (mvar-get-poll (make-mvar 2)))))

  ((test/choose/many-channels _)
   (let* ((n 12)
          (got (run/value (τ (let1 (chs (map (λ (i) (make-channel)) (iota n)))
                               (for-each (λ (ch i) (spawn (τ (send ch i)))) chs (iota n))
                               (let loop ((k n) (acc '()))
                                 (if (zero? k) acc (loop (sub1 k) (cons (select* (map recv-evt chs)) acc)))))))))
     (⊦= (iota n) (sort got <))))

  ((test/choose/structure _)
   (let* ((c (make-channel))
          (e (choose (always-evt 1) (choose (recv-evt c) never-evt) (guard (τ never-evt)))))
     (⊨ (event? e))
     (⊨ (event? never-evt))
     (⊭ (event? 42))
     (⊦= 'choose (%event-tag e))
     (⊦= 'base (%event-tag (choose (always-evt 1) (recv-evt c) never-evt)))
     (⊦= 2 (length (%event-payload (choose (always-evt 1) (recv-evt c) never-evt))))
     (⊦= 7 (run/value (τ (sync (wrap (choose never-evt (always-evt 6)) add1)))))
     (⊦= 'failure (run-cml (τ (sync (choose)))))
     ; choose* takes a list
     (⊦= 'base (%event-tag (choose* '())))
     (⊦= 2 (run/value (τ (sync (choose* (list never-evt (always-evt 2)))))))
     (⊦= 'failure (run-cml (τ (sync (choose* '())))))))

  ((test/select/case _)
   (⊦= '(one 3 unit (4 5) none)
       (cdr (run/log (τ (let ((c (make-channel)) (d (make-channel)))
                          (spawn (τ (send c 1)))
                          (note! (select/case ((recv-evt c) (x) (if (= x 1) 'one 'other))
                                              ((recv-evt d) (x) 'from-d)))
                          (note! (select/case ((always-evt 2) => add1)))
                          (note! (select/case ((timeout-evt 0) () 'unit)))
                          (note! (select/case ((always-evt 4) xs (cons 4 (list 5)))))
                          (note! (sync/timeout (recv-evt d) 0.01 'none))
                          (cml/shutdown)))))))

  ((test/guard/per-sync _)
   (⊦= '(1 2 3 3)
       (cdr (run/log (τ (let* ((n 0)
                               (e (guard (τ (set! n (add1 n)) (always-evt n)))))
                          (note! (sync e))
                          (note! (sync e))
                          (note! (select e never-evt))
                          (note! n)
                          (cml/shutdown)))))))

  ((test/guard/exceptions-not-handled _)
   (⊦= '(raised handled)
       (cdr (run/log (τ (note! (raised-or (exn) (sync (wrap-handler (guard (τ (error "in guard"))) (λ (e) 'handled)))))
                        (note! (sync (wrap-handler (guard (τ (wrap (always-evt 0) (λ (x) (error "in wrap")))))
                                                   (λ (e) 'handled))))
                        (cml/shutdown))))))

  ((test/wrap-handler/nesting _)
   (define (boom x) (error "boom"))
   (⊦= '(inner outer 1 raised (msg "boom"))
       (cdr (run/log (τ (note! (sync (wrap-handler (wrap-handler (wrap (always-evt 1) boom) (λ (e) 'inner))
                                                   (λ (e) 'outer))))
                        (note! (sync (wrap-handler (wrap-handler (wrap (always-evt 1) boom) (λ (e) (error "again")))
                                                   (λ (e) 'outer))))
                        (note! (sync (wrap-handler (always-evt 1) (λ (e) 'unused))))
                        ; a wrap applied after the handler is not protected by it
                        (note! (raised-or (exn) (sync (wrap (wrap-handler (always-evt 1) (λ (e) 'no)) boom))))
                        (note! (sync (wrap-handler (wrap (always-evt 1) boom)
                                                   (λ (e) (list 'msg ((condition-property-accessor 'exn 'message) e))))))
                        (cml/shutdown))))))

  ((test/wrap-handler/own-branch-only _)
   ; a handler covers its own event: when a blocked select commits on another branch whose wrap
   ; raises, the condition escapes the sync (event.sml runs the block functions nested, so there
   ; the handler of an earlier branch would catch it), for sync/choose and with-nack groups too
   (define (boom v) (error "boom"))
   (define (handled e) 'c1-handler)
   (⊦= '(raised raised raised raised)
       (cdr (run/log (τ (let ((c1 (make-channel)) (c2 (make-channel)))
                          (define (try thunk)
                            (spawn (τ (cml/yield) (send c2 'x)))       ; sends once the sync is blocked
                            (note! (raised-or (exn) (thunk))))
                          (try (τ (select (wrap-handler (recv-evt c1) handled) (wrap (recv-evt c2) boom))))
                          (try (τ (sync (choose (wrap-handler (recv-evt c1) handled) (wrap (recv-evt c2) boom)))))
                          (try (τ (sync (choose (wrap (recv-evt c2) boom) (wrap-handler (recv-evt c1) handled)))))
                          (try (τ (select (wrap-handler (with-nack (λ (nack) (recv-evt c1))) handled)
                                          (wrap (recv-evt c2) boom))))
                          (cml/shutdown)))))))

  ((test/with-nack/losers-only _)
   (let1 (log (cdr (run/log (τ (define (branch i)
                                 (with-nack (λ (nack) (spawn (τ (sync nack) (note! (list 'nack i)))) never-evt)))
                               (note! (select (branch 1) (branch 2) (always-evt 'win) (branch 3)))
                               (cml/sleep 0.01)
                               (cml/shutdown)))))
     (⊦= 'win (car log))
     (⊦= '(1 2 3) (sort (map cadr (cdr log)) <))))

  ((test/with-nack/chosen-no-nack _)
   (⊦= '(a)
       (cdr (run/log (τ (note! (select (with-nack (λ (nack) (spawn (τ (sync nack) (note! 'nacked))) (always-evt 'a)))
                                       never-evt))
                        (cml/sleep 0.01)
                        (cml/shutdown))))))

  ((test/with-nack/blocked _)
   ; both branches block; a later send on c2 commits the choice, the c1 branch gets its nack and its
   ; registration in c1's queue is stale, so a later send-poll on c1 finds nobody
   (⊦= '((got 2) nack #f)
       (cdr (run/log (τ (let ((c1 (make-channel)) (c2 (make-channel)))
                          (spawn (τ (note! (list 'got (select (with-nack (λ (nack) (spawn (τ (sync nack) (note! 'nack)))
                                                                              (recv-evt c1)))
                                                              (recv-evt c2))))))
                          (send c2 2)
                          (cml/sleep 0.01)
                          (note! (send-poll c1 1))
                          (cml/shutdown)))))))

  ((test/with-nack/nested _)
   ; the outer nack covers the inner branches: it fires only if none of them is chosen
   (⊦= '((got b) inner-a)
       (cdr (run/log (τ (let1 (c (make-channel))
                          (spawn (τ (send c 'b)))
                          (note! (list 'got
                                       (select (with-nack (λ (outer)
                                                            (spawn (τ (sync outer) (note! 'outer)))
                                                            (choose (with-nack (λ (a) (spawn (τ (sync a) (note! 'inner-a)))
                                                                                         never-evt))
                                                                    (recv-evt c))))
                                               never-evt)))
                          (cml/sleep 0.01)
                          (cml/shutdown)))))))

  ((test/with-nack/forcing-raises _)
   ; a guard that raises abandons the sync, and the nacks made before it are set (not in ML, where
   ; their servers wait forever); a continuable condition whose handler returns abandons nothing
   (define (branch tag) (with-nack (λ (nack) (spawn (τ (sync nack) (note! (list 'nacked tag)))) never-evt)))
   (⊦= '(raised (nacked 1))
       (cdr (run/log (τ (note! (raised-or (exn) (select (branch 1) (guard (τ (error "guard failed"))))))
                        (cml/sleep 0.01)
                        (cml/shutdown)))))
   (⊦= '(1 (nacked 2))
       (cdr (run/log (τ (note! (with-exception-handler
                                 (λ (e) 'ignored)
                                 (τ (select (branch 2) (guard (τ (signal 'warning) (always-evt 1)))))))
                        (cml/sleep 0.01)
                        (cml/shutdown))))))

  ((test/with-nack/partner-waits-on-the-nack _)
   ; the server behind a nack also offers the partner of the other, enabled, branch: the nacks fire
   ; only after the chosen branch has committed, so the server completes that rendezvous (event.sml
   ; fires them first: the server was cancelled and then dequeued anyway, and the run never ended)
   (define (client other-evt server-evt)
     (sync (choose (with-nack (λ (nack)
                                (spawn (τ (note! (list 'server (select (wrap server-evt (λ ignored 'done))
                                                                       (wrap nack (λ ignored 'nacked)))
                                                       (tid->string (current-tid))))))
                                never-evt))
                   other-evt)))
   (⊦= '((client hello "[000002]") (server done "[000003]"))
       (cdr (run/log (τ (let1 (ch (make-channel))
                          (note! (list 'client (client (recv-evt ch) (send-evt ch 'hello)) (tid->string (current-tid))))
                          (cml/sleep 0.01)
                          (cml/shutdown))))))
   (⊦= '((server done "[000003]") (client sent "[000002]"))
       (cdr (run/log (τ (let1 (ch (make-channel))
                          (note! (list 'client
                                       (client (wrap (send-evt ch 'hello) (λ ignored 'sent)) (recv-evt ch))
                                       (tid->string (current-tid))))
                          (cml/sleep 0.01)
                          (cml/shutdown))))))
   ; the enabled barrier wait completes the round with the server that also waits on the nack
   (⊦= '((client 1 "[000002]") (server done "[000003]"))
       (cdr (run/log (τ (let* ((b (make-barrier 0 add1)) (e1 (barrier-enroll b)) (e2 (barrier-enroll b)))
                          (note! (list 'client
                                       (client (barrier-wait-evt e1) (barrier-wait-evt e2))
                                       (tid->string (current-tid))))
                          (cml/sleep 0.01)
                          (cml/shutdown)))))))

  ((test/with-nack/forcing-escapes _)
   ; the nacks made so far are set however the forcing is left for good: a guard escaping with a
   ; continuation, or its thread exiting, abandons the sync as a raise does
   (define (branch tag) (with-nack (λ (nack) (spawn (τ (sync nack) (note! (list 'nacked tag)))) never-evt)))
   (⊦= '(escaped (nacked 1))
       (cdr (run/log (τ (note! (call-with-current-continuation
                                 (λ (k) (select (branch 1) (guard (τ (k 'escaped)))))))
                        (cml/sleep 0.01)
                        (cml/shutdown)))))
   (⊦= '((nacked 2))
       (cdr (run/log (τ (spawn (τ (select (branch 2) (guard (τ (cml/exit))))))
                        (cml/sleep 0.01)
                        (cml/shutdown)))))
   ; a guard that blocks is not an exit: the sync goes on and the other branch is nacked once
   (⊦= '(x (nacked 3))
       (cdr (run/log (τ (let1 (ch (make-channel))
                          (spawn (τ (cml/sleep 0.01) (send ch 'x)))
                          (note! (select (branch 3) (guard (τ (always-evt (recv ch))))))
                          (cml/sleep 0.01)
                          (cml/shutdown)))))))

  ((test/with-nack/forcing-escapes-then-reentered _)
   ; a guard that escaped with a continuation set the nacks made so far; re-entering it (backtracking)
   ; finishes the forcing with that nack already set, and the sync must go on: setting it again
   ; raised "cvar already set" in the thread that woke the sync (here the timeout poller), which
   ; aborted the whole run and every other thread with it
   (let* ((saved #f) (first? #t)
          (r (run/log (τ (spawn (τ (let loop ((i 0)) (cml/sleep 0.02) (note! 'tick) (when (< i 3) (loop (add1 i))))))
                         (let1 (r (call-with-current-continuation
                                    (λ (out)
                                      (select (with-nack (λ (n) (spawn (τ (sync n) (note! 'nacked))) never-evt))
                                              (guard (τ (call-with-current-continuation (λ (k) (set! saved k)))
                                                        (if first?
                                                          (begin (set! first? #f) (out 'escaped))
                                                          (wrap (timeout-evt 0.01) (λ ignored 'timeout)))))))))
                           (note! r)
                           (if (eq? r 'escaped)
                             (saved #f)
                             (begin (cml/sleep 0.2) (cml/shutdown 'ok))))))))
     (⊦= 'ok (car r))
     (⊦= '(escaped nacked timeout) (filter (λ (x) (not (eq? x 'tick))) (cdr r)))
     (⊦= 4 (count (λ (x) (eq? x 'tick)) (cdr r)))))

  ((test/with-nack/forcing-thread-killed-while-switched-out _)
   ; a guard that blocks, then its thread killed by a dynamic-wind after thunk that raises while it
   ; is switched out: the sync is abandoned for good (its frame is unwound by the dying thread's
   ; handler before the thread is marked dead), so the nack made before is set
   (define (killed-in-guard)
     (select (with-nack (λ (n) (spawn (τ (sync n) (note! '(nacked 1)))) never-evt))
             (guard (τ (dynamic-wind void
                                     (τ (cml/sleep 0.01) never-evt)
                                     (τ (error "after thunk raised")))))))
   (⊦= '(died (nacked 1) joined)
       (cdr (run/log (τ (sync (join-evt (parameterize ((default-exn-handler (λ (e) (note! 'died)))) (spawn killed-in-guard))))
                        (note! 'joined)
                        (cml/sleep 0.05)
                        (cml/shutdown))))))

  ((test/with-nack/set-when-sync-returns _)
   ; a sync that commits on an enabled event has set the nacks of the other branches when it
   ; returns (as ML's chkCVars): a poll right after sees them, and the nacks of consecutive syncs
   ; are set in order
   (⊦= '(other nacked)
       (cdr (run/log (τ (let1 (nack #f)
                          (note! (select (with-nack (λ (n) (set! nack n) never-evt)) (always-evt 'other)))
                          (note! (sync/timeout (wrap nack (λ ignored 'nacked)) 0 'not-yet))
                          (cml/shutdown))))))
   (⊦= '((nacked 1) (nacked 2))
       (filter pair?
               (cdr (run/log (τ (define (branch i) (with-nack (λ (n) (spawn (τ (sync n) (note! (list 'nacked i)))) never-evt)))
                                (note! (select (branch 1) (always-evt 'a)))
                                (note! (select (branch 2) (always-evt 'b)))
                                (cml/sleep 0.01)
                                (cml/shutdown))))))
   ; a server blocked on the next request and on the nack of a sync that then commits elsewhere is
   ; woken by the nack, before that syncing thread can send it a request directly
   (⊦= '(abandoned (served 2))
       (cdr (run/log (τ (let ((req (make-channel)) (go (make-mvar)) (nack #f))
                          (spawn (τ (mvar-take! go)
                                    (note! (select (wrap (recv-evt req) (λ (x) (list 'served x)))
                                                   (wrap nack (λ ignored 'abandoned))))
                                    (note! (list 'served (recv req)))))
                          (cml/yield)
                          (select (with-nack (λ (n) (set! nack n) (mvar-put! go #t) never-evt)) (always-evt 'other))
                          (send req 2)
                          (cml/sleep 0.01)
                          (cml/shutdown)))))))

  ((test/with-nack/preempted-handler _)
   ; the handler of a continuable condition raised while forcing uses CML operations and is
   ; preempted (quantum 1): a preemption is not an exit, so the nacks are not set early, the
   ; chosen with-nack branch is not nacked, and a sync that commits later does not raise
   (define warning? (condition-predicate 'warning))
   (define (handler mb) (λ (c) (mailbox-send! mb 'log) (mailbox-recv mb) 'continue))
   (⊦= '(x not-nacked)
       (cdr (run/log (τ (let ((mb (make-mailbox)) (nacked #f))
                          (note! (with-exception-handler
                                   (handler mb)
                                   (τ (sync (choose (with-nack (λ (n) (spawn (τ (sync n) (set! nacked #t))) (always-evt 'x)))
                                                    (guard (τ (signal (make-property-condition 'warning)) never-evt)))))))
                          (cml/yield) (cml/yield)
                          (note! (if nacked 'nacked 'not-nacked))
                          (cml/shutdown)))
                     quantum: 1)))
   (⊦= '(msg)
       (cdr (run/log (τ (let ((mb (make-mailbox)) (ch (make-channel)))
                          (spawn (τ (cml/sleep 0.02) (send ch 'msg)))
                          (note! (condition-case
                                   (with-exception-handler
                                     (handler mb)
                                     (τ (sync (choose (with-nack (λ (n) never-evt))
                                                      (guard (τ (signal (make-property-condition 'warning)) (recv-evt ch)))))))
                                   (e () (list 'raised (get-condition-property e 'exn 'message)))))
                          (cml/shutdown)))
                     quantum: 1))))

  ((test/with-nack/poll-raises _)
   ; a poll that raises (a bad argument) abandons the sync after its nacks are set
   (⊦= '(raised (nacked 1))
       (cdr (run/log (τ (note! (raised-or (exn) (select (with-nack (λ (nack) (spawn (τ (sync nack) (note! '(nacked 1)))) never-evt))
                                                       (recv-evt 'not-a-channel))))
                        (cml/sleep 0.01)
                        (cml/shutdown))))))

  ((test/sync/continuable-conditions _)
   ; a condition signalled (continuably) in a wrap function reaches the handler around the sync,
   ; whose value goes back to the wrap function, however the sync ends: alone, enabled at poll time,
   ; or blocked on several branches (it was re-raised with `abort` there)
   (define (try blocked? two?)
     (run/value (τ (let ((c (make-channel)) (c2 (make-channel)))
                     (spawn (τ (unless blocked? (send c 1))))
                     (if blocked? (spawn (τ (cml/sleep 0.01) (send c 1))) (cml/yield))
                     (call-with-current-continuation
                       (λ (out)
                         (with-exception-handler
                           (λ (e) (if (eq? e 'oops) 42 (out 'raised)))
                           (τ (let1 (e (wrap (recv-evt c) (λ (v) (+ v (signal 'oops)))))
                                (sync (if two? (choose e (recv-evt c2)) e)))))))))))
   (⊦= '(43 43 43 43) (list (try #f #f) (try #f #t) (try #t #f) (try #t #t))))

  ((test/wrap/loops-do-not-nest _)
   ; a server loop that recurses from the wrap function of a blocked select (or of a with-nack
   ; event) runs in the dynamic context of the sync itself, not nested in the block functions of
   ; its branches: no dynamic-wind frame (nor continuation frame) piles up per blocked iteration
   (define (growth n make-evt)
     (run/value (τ (let1 (ch (make-channel))
                     (spawn (τ (let loop () (cml/yield) (send ch 1) (loop))))
                     (let loop ((k 0) (w0 #f))
                       (let1 (w (length ##sys#dynamic-winds))
                         (if (= k n)
                           (- w w0)
                           (sync (wrap (make-evt ch) (λ ignored (loop (add1 k) (or w0 w))))))))))))
   (⊦= 0 (growth 300 (λ (ch) (choose (recv-evt ch) (timeout-evt 10)))))
   (⊦= 0 (growth 300 (λ (ch) (with-nack (λ (nack) (recv-evt ch))))))
   (⊦= 0 (growth 300 (λ (ch) (choose (with-nack (λ (nack) (recv-evt ch))) (timeout-evt 10))))))

  ((test/wrap-handler/blocked-branch _)
   ; a blocked branch's handler still sees what its own event raises once committed (a barrier
   ; whose update fails), and its wrap functions still run in order after the commit
   (⊦= '((a raised) (b handled))
       (cdr (run/log (τ (let* ((b (make-barrier 0 (λ (x) (error "update failed"))))
                               (ea (barrier-enroll b)) (eb (barrier-enroll b)))
                          (spawn (τ (cml/sleep 0.01) (note! (list 'a (raised-or (exn) (barrier-wait ea))))))
                          (note! (list 'b (select (wrap-handler (wrap (barrier-wait-evt eb) (λ (x) 'wrapped))
                                                                (λ (e) 'handled))
                                                  (recv-evt (make-channel)))))
                          (cml/yield)
                          (cml/shutdown))))))
   (⊦= '(3 5)
       (run/value (τ (let ((ch (make-channel)) (ch2 (make-channel)))
                       (spawn (τ (cml/sleep 0.01) (send ch 1)))
                       (list (select (wrap (wrap (recv-evt ch) add1) add1) (recv-evt ch2))
                             (begin (spawn (τ (cml/sleep 0.01) (send ch 4)))
                                    (sync (wrap (choose (recv-evt ch2) (recv-evt ch)) add1)))))))))

  ((test/priorities/prio-0 _)
   ; critic C: a fresh mvar-init get event polls with priority 0, next to a fixed priority event (2,
   ; the number of enabled events): the always event wins while the mvar's priority is 0 and 1,
   ; again on the tie at 2 (the tie-break counter), and the bumped mvar wins from then on
   (⊦= (append '(a a a) (make-list 17 5))
       (run/value (τ (let1 (mv (make-mvar 5))
                       (map (λ (i) (select (mvar-get-evt mv) (always-evt 'a))) (iota 20)))))))

  ((test/priorities/reset _)
   ; iGetEvt's doFn resets the dynamic priority, mTakeEvt's does not
   (⊦= '(1 2)
       (run/value (τ (let ((iv (make-ivar)) (mv (make-mvar 'x)))
                       (ivar-put! iv 'v)
                       (sync (ivar-get-evt iv))
                       (sync (ivar-get-evt iv))
                       (sync (mvar-take-evt mv))
                       (mvar-put! mv 'y)
                       (sync (mvar-take-evt mv))
                       (list (%cell-priority iv) (%cell-priority mv)))))))

  ((test/priorities/tie-break _)
   ; always events tie at priority n: the counter based tie-break reaches every branch
   (let1 (got (run/value (τ (map (λ (i) (select (always-evt 'a) (always-evt 'b) (always-evt 'c))) (iota 30)))))
     (⊦= '(10 10 10) (map (λ (x) (count (λ (y) (eq? x y)) got)) '(a b c)))))

  )

(define-suite cml-threads-suite

  ((test/tid _)
   (⊦= "[-000001]" (tid->string (current-tid)))
   (let1 (r (run/value (τ (let* ((me (current-tid))
                                 (t (spawn void)))
                            (list (tid? me) (tid? 1) (tid=? me me) (tid=? me t) (tid<? me t) (tid<? t me) (tid<? me me)
                                  (tid-compare t me) (tid-compare me me) (tid-compare me t)
                                  (= (add1 (tid-hash me)) (tid-hash t))
                                  (string=? (tid->string me) (tid->string me)) (string-length (tid->string me)))))))
     (⊦= '(#t #f #t #f #t #f #f 1 0 -1 #t #t 8) r)))

  ((test/runs-are-independent _)
   (let ((a (run/value (τ (list (tid->string (current-tid)) (tid->string (spawn void))))))
         (b (run/value (τ (list (tid->string (current-tid)) (tid->string (spawn void)))))))
     (⊦= a b)
     (⊦= "[000002]" (car a))))                             ; tids 0 and 1 ran the two standard cleaners

  ((test/spawn/child-first _)
   (⊦= '(child parent (arg 42))
       (cdr (run/log (τ (spawn (τ (note! 'child)))
                        (note! 'parent)
                        (spawn/call (λ (x) (note! (list 'arg x))) 42)
                        (cml/shutdown))))))

  ((test/spawn/not-a-procedure _)
   ; a bad argument raises in the caller, as for every other operation, instead of killing the child
   (⊦= '(raised raised)
       (run/value (τ (map (λ (thunk) (condition-case (begin (thunk) (cml/yield) 'no-raise) (e (exn) 'raised)))
                          (list (τ (spawn 42)) (τ (spawn/call 42 1))))))))

  ((test/join/normal-exit-exception _)
   (parameterize ((default-exn-handler (λ (e) (note! 'handler))))
     (⊦= '(handler joined-normal joined-exit joined-exn)
         (cdr (run/log (τ (let ((t1 (spawn (τ (cml/sleep 0.01))))
                                (t2 (spawn (τ (cml/sleep 0.01) (cml/exit) (note! 'not-here))))
                                (t3 (spawn (τ (error "die")))))
                            (sync (join-evt t1)) (note! 'joined-normal)
                            (sync (join-evt t2)) (note! 'joined-exit)
                            (sync (join-evt t3)) (note! 'joined-exn)
                            (sync (join-evt t3))                    ; stays enabled
                            (cml/shutdown))))))))

  ((test/join/fifo _)
   (⊦= '(w1 w2 w3 w4)
       (cdr (run/log (τ (let* ((c (make-channel))
                               (t (spawn (τ (recv c)))))
                          (for-each (λ (w) (spawn (τ (sync (join-evt t)) (note! w)))) '(w1 w2 w3 w4))
                          (send c 'go)
                          (cml/sleep 0.01)
                          (cml/shutdown)))))))

  ((test/default-exn-handler _)
   (let1 (err (with-error-output-to-string
                (τ (⊦= '(after) (cdr (run/log (τ (spawn (τ (error "kaboom" 1 2))) (note! 'after) (cml/shutdown))))))))
     (⊨ (and (substring-index "kaboom" err) #t))
     (⊨ (and (substring-index "[000003]" err) #t)))
   ; exceptions raised by a handler are swallowed
   (parameterize ((default-exn-handler (λ (e) (error "handler fails too"))))
     (⊦= '(still-running) (cdr (run/log (τ (spawn (τ (car '()))) (note! 'still-running) (cml/shutdown)))))))

  ((test/dynamic-wind/after-raises _)
   ; an after thunk raising while its thread is switched out kills that thread (its handler runs as
   ; that thread), the switch goes on to its target and the dead thread is never resumed
   (parameterize ((default-exn-handler (λ (e) (note! (list 'died (tid->string (current-tid)))))))
     (⊦= '(done (died "[000003]") joined)
         (run/log (τ (let1 (a (spawn (τ (dynamic-wind void
                                                     (τ (cml/sleep 0.02) (note! 'resumed-after-death))
                                                     (τ (error "after"))))))
                       (sync (join-evt a))
                       (note! 'joined)
                       (cml/sleep 0.05)
                       (cml/shutdown 'done)))))
     ; the switch can be a shutdown
     (⊦= '(bye (died "[000003]"))
         (run/log (τ (spawn (τ (dynamic-wind void (τ (cml/shutdown 'bye)) (τ (error "after")))))
                     (sync never-evt))))))

  ((test/dynamic-wind/winders-at-every-switch _)
   ; documented in the header: the winders of a thread run at every switch out of/into it, an after
   ; thunk as the thread being switched to (here the root context of run-cml, which is the dummy tid
   ; and spawned main, [000002]), a before thunk as the thread itself
   (define (who) (tid->string (current-tid)))
   (⊦= '((in "[000003]") (out "[-000001]") (in "[000003]") (body x) (out "[000003]"))
       (cdr (run/log (τ (let1 (c (make-channel))
                          (let1 (a (spawn (τ (dynamic-wind (τ (note! (list 'in (who))))
                                                           (τ (note! (list 'body (recv c))))
                                                           (τ (note! (list 'out (who))))))))
                            (send c 'x)
                            (sync (join-evt a))
                            (cml/shutdown))))))))

  ((test/dynamic-wind/dead-receiver-not-resumed _)
   ; a receiver whose after thunk raises while it switches to its blocked sender dies there: the
   ; sender goes on, and the dead thread never runs again (the message is lost with it)
   (define (try do-recv)
     (parameterize ((default-exn-handler (λ (e) (note! 'died))))
       (cdr (run/log (τ (let ((ch (make-channel)) (armed #f))
                          (spawn (τ (send ch 'hello) (note! 'sender-resumed)))
                          (let1 (r (spawn (τ (dynamic-wind
                                               void
                                               (τ (set! armed #t) (note! (list 'zombie (do-recv ch))))
                                               (τ (when armed (set! armed #f) (error "after thunk raised")))))))
                            (sync (join-evt r))
                            (note! 'joined)
                            (cml/sleep 0.01)
                            (cml/shutdown))))))))
   (⊦= '(died sender-resumed joined) (try recv))
   (⊦= '(died sender-resumed joined) (try (λ (ch) (sync (recv-evt ch)))))
   (⊦= '(died sender-resumed joined) (try (λ (ch) (car (recv-poll ch))))))

  ((test/dynamic-wind/dead-sender-does-not-strand-receiver _)
   ; the mirror case: a blocked sender whose before thunk raises as a receiver switches to it dies
   ; there, before taking the receiver's continuation; the receiver is not left blocked on nothing,
   ; it receives again (recv and a committed recv-evt wait for the next sender, recv-poll finds none)
   (define (try do-recv)
     (parameterize ((default-exn-handler (λ (e) (note! 'died))))
       (run/log (τ (let ((ch (make-channel)) (first #t))
                     (spawn (τ (dynamic-wind (τ (if first (set! first #f) (error "before thunk raised")))
                                             (τ (send ch 42) (note! 'zombie))
                                             void)))
                     (spawn (τ (cml/sleep 0.02) (send ch 43) (note! 'second-sent)))
                     (cml/yield)
                     (note! (list 'got (do-recv ch)))
                     (cml/sleep 0.03)
                     (cml/shutdown 'ok))))))
   (⊦= '(ok died (got 43) second-sent) (try recv))
   (⊦= '(ok died (got 43) second-sent) (try (λ (ch) (sync (recv-evt ch)))))
   (⊦= '(ok died (got 43) second-sent) (try (λ (ch) (sync (choose (recv-evt ch) (recv-evt (make-channel)))))))
   (⊦= '(ok died (got ())) (try recv-poll)))

  ((test/dynamic-wind/dead-reader-does-not-strand-syncvar _)
   ; the first reader of an empty ivar or mvar dies as the put switches to it (a before thunk that
   ; raises): the relay to the other readers (or the take) was its job, so they are woken with the
   ; value still in the variable instead of waiting for good on a full variable
   (define (try make read)
     (parameterize ((default-exn-handler (λ (e) (note! 'died))))
       (cdr (run/log (τ (let ((v (make)) (n 0))
                          (spawn (τ (dynamic-wind (τ (set! n (add1 n)) (when (= n 2) (error "before thunk raised")))
                                                  (τ (note! (list 'zombie (read v))))
                                                  void)))
                          (spawn (τ (note! (list 'second (read v)))))
                          (cml/sleep 0.01)
                          (if (ivar? v) (ivar-put! v 42) (mvar-put! v 42))
                          (note! (sync/timeout (wrap (timeout-evt 10) (λ ignored 'never)) 0.2 'stranded))
                          (note! (if (ivar? v) (ivar-get-poll v) (mvar-get-poll v)))
                          (cml/shutdown)))))))
   (⊦= '(died (second 42) stranded (42)) (try make-ivar ivar-get))
   (⊦= '(died (second 42) stranded (42)) (try make-ivar (λ (v) (sync (ivar-get-evt v)))))
   (⊦= '(died (second 42) stranded (42)) (try make-mvar mvar-get))
   (⊦= '(died (second 42) stranded (42)) (try make-mvar (λ (v) (sync (mvar-get-evt v)))))
   (⊦= '(died (second 42) stranded ()) (try make-mvar mvar-take!))
   (⊦= '(died (second 42) stranded ()) (try make-mvar (λ (v) (sync (choose (mvar-take-evt v) never-evt))))))

  ((test/dynamic-wind/dead-reader-relays-the-current-value _)
   ; the next reader is handed the value the mvar holds when it is woken, not the one it held when
   ; the first reader died: whoever runs in between may take or replace it.  Here the putter polls
   ; it back and puts another value: every value put is taken exactly once, by the putter's poll,
   ; the second reader or the final poll (a stale value was handed to the second reader, so that
   ; 42 was taken twice and 43 lost)
   (define (try take)
     (parameterize ((default-exn-handler (λ (e) (note! 'died))))
       (cdr (run/log (τ (let ((v (make-mvar)) (n 0))
                          (spawn (τ (dynamic-wind (τ (set! n (add1 n)) (when (= n 2) (error "before thunk raised")))
                                                  (τ (note! (list 'zombie (take v))))
                                                  void)))
                          (spawn (τ (note! (list 'took (list (take v))))))
                          (cml/sleep 0.01)
                          (mvar-put! v 42)
                          (let1 (p (mvar-take-poll v))
                            (note! (list 'took p))
                            (mvar-put! v 43))
                          (cml/sleep 0.02)
                          (note! (list 'took (mvar-take-poll v)))
                          (cml/shutdown)))))))
   (for-each (λ (take)
               (let1 (log (try take))
                 (⊦= 'died (car log))
                 (⊦= '(42 43) (sort (append-map cadr (cdr log)) <))))
             (list mvar-take! (λ (v) (sync (mvar-take-evt v))))))

  ((test/dynamic-wind/dead-receiver-does-not-lose-mailbox-message _)
   ; a mailbox receiver that dies as mailbox-send! hands it the message (a before thunk that raises)
   ; does not take the message with it: the next receiver gets it, or the mailbox keeps it
   (define (try recv2)
     (parameterize ((default-exn-handler (λ (e) (note! 'died))))
       (cdr (run/log (τ (let ((mb (make-mailbox)) (n 0))
                          (spawn (τ (dynamic-wind (τ (set! n (add1 n)) (when (= n 2) (error "before thunk raised")))
                                                  (τ (note! (list 'zombie (mailbox-recv mb))))
                                                  void)))
                          (when recv2 (spawn (τ (note! (list 'second (recv2 mb))))))
                          (cml/sleep 0.01)
                          (mailbox-send! mb 42)
                          (mailbox-send! mb 43)
                          (cml/sleep 0.02)
                          (note! (list 'poll (mailbox-recv-poll mb)))
                          (cml/shutdown)))))))
   (⊦= '(died (second 42) (poll (43))) (try mailbox-recv))
   (⊦= '(died (second 42) (poll (43))) (try (λ (mb) (sync (choose (mailbox-recv-evt mb) never-evt)))))
   (⊦= '(died (poll (42))) (try #f)))

  ((test/mailbox/receiver-dying-later-keeps-its-message _)
   ; a receiver that dies of an ordinary condition after it got its message (no switch in between)
   ; has consumed it, as in ML: the message is not handed to the next receiver, nor put back in the
   ; mailbox (the switch that resumed the receiver was once taken for one still in progress)
   (define (try recv)
     (parameterize ((default-exn-handler (λ (e) (note! 'died))))
       (cdr (run/log (τ (let1 (mb (make-mailbox))
                          (spawn (τ (let1 (j (recv mb)) (note! (list 'w1 j)) (error "worker 1 crashes on" j))))
                          (spawn (τ (let loop () (note! (list 'w2 (recv mb))) (loop))))
                          (cml/yield)
                          (mailbox-send! mb 'job-1)
                          (cml/sleep 0.02)
                          (note! (list 'poll (mailbox-recv-poll mb)))
                          (mailbox-send! mb 'job-2)
                          (cml/sleep 0.02)
                          (cml/shutdown)))))))
   (⊦= '((w1 job-1) died (poll ()) (w2 job-2)) (try mailbox-recv))
   (⊦= '((w1 job-1) died (poll ()) (w2 job-2)) (try (λ (mb) (sync (mailbox-recv-evt mb)))))
   (⊦= '((w1 job-1) died (poll ()) (w2 job-2))
       (try (λ (mb) (sync (wrap (choose (mailbox-recv-evt mb) (timeout-evt 10)) (λ (j) j))))))
   ; a pool of workers selecting over the mailbox and a timeout: one bad job kills one worker only
   (⊦= '((w bad) died (w good))
       (parameterize ((default-exn-handler (λ (e) (note! 'died))))
         (cdr (run/log (τ (let1 (mb (make-mailbox))
                            (for-each (λ (i) (spawn (τ (let loop ()
                                                         (select (wrap (mailbox-recv-evt mb)
                                                                       (λ (j) (note! (list 'w j)) (when (eq? j 'bad) (error "bad job"))))
                                                                 (timeout-evt 1))
                                                         (loop)))))
                                      (iota 3))
                            (cml/yield)
                            (mailbox-send! mb 'bad)
                            (cml/sleep 0.02)
                            (mailbox-send! mb 'good)
                            (cml/sleep 0.02)
                            (cml/shutdown))))))))

  ((test/dynamic-wind/dead-waiter-sets-its-nacks _)
   ; a thread blocked in a sync with nacks that dies as the partner that committed switches to it
   ; (a before thunk that raises) never runs the sync's cleanup: its nacks are set all the same,
   ; so a server waiting on one is not left waiting for good.  Likewise for a thread that dies
   ; while switched out as it blocks (an after thunk that raises)
   (define (try make-evt mode)
     (parameterize ((default-exn-handler (λ (e) (note! 'died))))
       (cdr (run/log (τ (let ((n 0) (nk #f))
                          (receive (evt fire!) (make-evt)
                            (spawn (τ (dynamic-wind (τ (set! n (add1 n)) (when (and (eq? mode 'in) (= n 2)) (error "before")))
                                                    (τ (note! (list 'zombie (sync (choose evt (with-nack (λ (nack) (set! nk nack) never-evt)))))))
                                                    (τ (when (and (eq? mode 'out) (= n 1)) (error "after"))))))
                            (cml/sleep 0.01)
                            (fire!)
                            (cml/sleep 0.01)
                            (note! (list 'nack (sync/timeout (wrap nk (λ ignored 'set)) 0.2 'unset)))
                            (cml/shutdown))))))))
   (define (ivar) (let1 (iv (make-ivar)) (values (ivar-get-evt iv) (τ (ivar-put! iv 1)))))
   (define (chan) (let1 (ch (make-channel)) (values (recv-evt ch) (τ (spawn (τ (send ch 1)))))))
   (define (mbox) (let1 (mb (make-mailbox)) (values (mailbox-recv-evt mb) (τ (mailbox-send! mb 1)))))
   (define (join) (let1 (iv (make-ivar)) (values (join-evt (spawn (τ (ivar-get iv)))) (τ (ivar-put! iv 1)))))
   (for-each (λ (make-evt)
               (⊦= '(died (nack set)) (try make-evt 'in))
               (⊦= '(died (nack set)) (try make-evt 'out)))
             (list ivar chan mbox join)))

  ((test/dynamic-wind/handler-may-exit-or-block _)
   ; the handler of a thread that dies of an after thunk raising as it is switched out runs after
   ; the switch it interrupted has been queued: a handler that exits its thread (cml/exit) or
   ; blocks does not strand the thread switched to (b here, which then never ran again)
   (define (try handler)
     (cdr (run/log (τ (let* ((b (spawn (τ (cml/yield) (note! 'b-ran))))
                             (a (parameterize ((default-exn-handler handler))
                                  (spawn (τ (dynamic-wind void (τ (cml/yield) (note! 'a-zombie)) (τ (error "after raised"))))))))
                        (note! (list 'joined (sync/timeout (wrap (join-evt b) (λ ignored 'b)) 0.3 'never)))
                        (note! (list 'a-joined (sync/timeout (wrap (join-evt a) (λ ignored 'a)) 0.3 'never)))
                        (cml/shutdown))))))
   (⊦= '(a-died b-ran (joined b) (a-joined a)) (try (λ (e) (note! 'a-died))))
   (⊦= '(a-died b-ran (joined b) (a-joined a)) (try (λ (e) (note! 'a-died) (cml/exit))))
   (⊦= '(a-died b-ran (joined b) handler-woke (a-joined a))
       (try (λ (e) (note! 'a-died) (cml/sleep 0.05) (note! 'handler-woke)))))

  ((test/dynamic-wind/dead-thread-in-ready-queue _)
   ; a thread that dies while its entry sits in a ready queue (an after thunk raising as it yields
   ; or is preempted) is skipped by the dispatcher, never resumed
   (define (try body)
     (let1 (dead #f)
       (parameterize ((default-exn-handler (λ (e) (set! dead #t) (note! 'died))))
         (run/log (τ (let1 (a (spawn (τ (let1 (armed #t)
                                          (dynamic-wind void
                                                        (τ (body (τ dead)))
                                                        (τ (when armed (set! armed #f) (error "after"))))))))
                       (sync (join-evt a))
                       (note! 'joined)
                       (cml/sleep 0.02)
                       (cml/shutdown 'done)))))))
   ; switched out by a yield
   (⊦= '(done died joined) (try (λ (dead?) (cml/yield) (note! 'resumed-after-death))))
   ; switched out by a preemption (every sync is a clock tick)
   (⊦= '(done died joined)
       (try (λ (dead?) (let loop () (sync (always-evt 1)) (if (dead?) (note! 'resumed-after-death) (loop)))))))

  ((test/yield _)
   (⊦= '(a1 b1 a2 b2 a3 b3)
       (cdr (run/log (τ (let1 (t (spawn (τ (for-each (λ (x) (note! x) (cml/yield)) '(a1 a2 a3)))))
                          (for-each (λ (x) (note! x) (cml/yield)) '(b1 b2 b3))
                          (cml/shutdown)))))))

  ((test/thread-properties _)
   (⊦= '((main) () init (init) (child) () (main))
       (cdr (run/log (τ (receive (clear! get peek put!) (make-thread-property (τ 'init))
                          (put! 'main)
                          (note! (peek))
                          (sync (join-evt (spawn (τ (note! (peek))
                                                    (note! (get))
                                                    (note! (peek))
                                                    (put! 'child)
                                                    (note! (peek))
                                                    (clear!)
                                                    (note! (peek))))))
                          (note! (peek))
                          (cml/shutdown)))))))

  ((test/thread-property/lazy-init _)
   (⊦= '(() 1 1 (1) 2)
       (cdr (run/log (τ (let1 (n 0)
                          (receive (clear! get peek put!) (make-thread-property (τ (set! n (add1 n)) n))
                            (note! (peek))
                            (note! (get))
                            (note! (get))
                            (note! (peek))
                            (sync (join-evt (spawn (τ (note! (get))))))
                            (cml/shutdown))))))))

  ((test/thread-flag _)
   ; a flag is per thread: the child starts unset and its own set leaves the parent's flag alone
   (⊦= '(#f #t (child #f #t) #t #f #t)
       (cdr (run/log (τ (receive (get set) (make-thread-flag)
                          (note! (get))
                          (set #t) (set #t)
                          (note! (get))
                          (sync (join-evt (spawn (τ (let1 (before (get))
                                                      (set #t)
                                                      (note! (list 'child before (get))))))))
                          (note! (get))
                          (set #f)
                          (note! (get))
                          (set #t)
                          (note! (get))
                          (cml/shutdown)))))))

  ((test/fairness/two-producers _)
   (let1 (got (run/value (τ (let1 (c (make-channel))
                              (for-each (λ (tag) (spawn (τ (let loop () (send c tag) (loop))))) '(p q))
                              (map (λ (i) (recv c)) (iota 20))))))
     (⊦= 10 (count (λ (x) (eq? x 'p)) got))
     (⊦= (map (λ (i) (if (even? i) 'p 'q)) (iota 20)) got)))

  ((test/fairness/preemption _)
   ; two compute-bound threads that only poll: without the quantum tick the first would finish
   ; before the second starts
   (let1 (log (cdr (run/log (τ (let1 (c (make-channel))
                                 (define (worker tag)
                                   (τ (let loop ((i 0))
                                        (when (< i 400)
                                          (recv-poll c)
                                          (when (zero? (remainder i 100)) (note! tag))
                                          (loop (add1 i))))))
                                 (let ((a (spawn (worker 'a))) (b (spawn (worker 'b))))
                                   (sync (join-evt a)) (sync (join-evt b))
                                   (cml/shutdown))))
                            quantum: 16)))
     (⊦= 8 (length log))
     (⊭ (equal? '(a a a a b b b b) log))))

  ((test/fairness/yield-promotes _)
   ; a thread looping on cml/yield (or a mailbox producer, which yields on a non-empty mailbox) is
   ; preempted at its quantum as ML's timer would: a thread demoted to rdyQ2 is promoted and runs
   (⊦= 'worker-done
       (run-cml (τ (let1 (done #f)
                     (spawn (τ (let loop ((i 0)) (if (= i 300) (set! done #t) (begin (sync (always-evt i)) (loop (add1 i)))))))
                     (let loop ((n 0))
                       (cond (done (cml/shutdown 'worker-done))
                             ((< n 200000) (cml/yield) (loop (add1 n)))
                             (else (cml/shutdown 'starved))))))))
   (let1 (sent (run/value (τ (let ((mb (make-mailbox)) (sent 0))
                               (spawn (τ (let loop () (when (< sent 200000) (mailbox-send! mb sent) (set! sent (add1 sent)) (loop)))))
                               (let loop ((got 0)) (when (< got 2000) (mailbox-recv mb) (loop (add1 got))))
                               sent))))
     (⊨ (< sent 3000))))

  ((test/fairness/no-phase-lock _)
   ; ticks are deterministic: a loop that spawns a child and receives from a busy sender is always
   ; preempted in the fresh (unmarked) child; a thread is promoted at every preemption, so the
   ; children do not pile up in rdyQ2 while the loop runs
   (let1 (r (run/value (τ (let ((ch (make-channel)) (done 0))
                            (spawn (τ (let loop () (send ch 1) (loop))))
                            (let loop ((i 0))
                              (when (< i 5000) (spawn (τ (set! done (add1 done)))) (recv ch) (loop (add1 i))))
                            (list done (length (%queue-front %rdy-q2)) (length (%queue-rear %rdy-q2)))))))
     (⊨ (>= (car r) 4998))
     (⊨ (<= (+ (cadr r) (caddr r)) 2))))

  ((test/fairness/channel-ping-pong-ticks _)
   ; two threads trading messages over channels hand off to each other without leaving the atomic
   ; region otherwise: each hand-off is a clock tick, so the timeouts, the descriptors and rdyQ2
   ; are still served (they were starved for good: the sleeper never woke)
   (define (ping-pong send! recv! server)
     (run-cml (τ (let ((a (make-channel)) (b (make-channel)) (n 0))
                   (spawn (τ (cml/sleep 0.01) (cml/shutdown (list 'woke (< n 100000)))))
                   (spawn (τ (server a b)))
                   (let loop () (set! n (add1 n)) (when (> n 100000) (cml/shutdown 'starved)) (send! a 1) (recv! b) (loop))))))
   (⊦= '(woke #t) (ping-pong send recv (λ (a b) (let loop () (send b (recv a)) (loop)))))
   (⊦= '(woke #t) (ping-pong (λ (ch v) (sync (send-evt ch v))) (λ (ch) (sync (recv-evt ch)))
                             (λ (a b) (let loop () (sync (send-evt b (sync (choose (recv-evt a) (recv-evt a))))) (loop)))))
   ; a timeout-evt of another thread, and a thread in rdyQ2
   (⊦= '(timed-out #t)
       (run-cml (τ (let ((a (make-channel)) (b (make-channel)) (n 0))
                     (spawn (τ (cml/shutdown (list (sync/timeout (recv-evt (make-channel)) 0.01 'timed-out) (< n 100000)))))
                     (spawn (τ (let loop () (send b (recv a)) (loop))))
                     (let loop () (set! n (add1 n)) (when (> n 100000) (cml/shutdown 'starved)) (send a 1) (recv b) (loop))))))
   (⊦= '(ran #t)
       (run-cml (τ (let ((a (make-channel)) (b (make-channel)) (n 0))
                     (spawn (τ (let loop () (send b (recv a)) (loop))))
                     ; compute-bound: demoted to rdyQ2 at its first preemption, it needs many promotions
                     (spawn (τ (let loop ((i 0))
                                 (if (< i 2000) (begin (sync (always-evt i)) (loop (add1 i))) (cml/shutdown (list 'ran (< n 100000)))))))
                     (let loop () (set! n (add1 n)) (when (> n 100000) (cml/shutdown 'starved)) (send a 1) (recv b) (loop)))))))

  )

(define-suite cml-time-suite

  ((test/timeouts/order _)
   (⊦= '(b c a)
       (cdr (run/log (τ (for-each (λ (tag secs) (spawn (τ (cml/sleep secs) (note! tag)))) '(a b c) '(0.06 0.02 0.04))
                        (cml/sleep 0.1)
                        (cml/shutdown))))))

  ((test/timeouts/duration _)
   (let1 (dt (run/value (τ (let1 (t0 (cml/now)) (cml/sleep 0.05) (- (cml/now) t0)))))
     (⊨ (>= dt 0.049))
     (⊨ (< dt 1.0))))

  ((test/timeouts/zero-and-past _)
   (⊦= '(zero past later)
       (cdr (run/log (τ (sync (timeout-evt 0)) (note! 'zero)
                        (sync (at-time-evt (- (cml/now) 10))) (note! 'past)
                        (sync (at-time-evt (+ (cml/now) 0.02))) (note! 'later)
                        (cml/shutdown))))))

  ((test/timeouts/lifo-ties _)
   ; two waits on the same deadline: the one blocked later wakes first
   (⊦= '(second first)
       (cdr (run/log (τ (let1 (t (+ (cml/now) 0.03))
                          (spawn (τ (sync (at-time-evt t)) (note! 'first)))
                          (spawn (τ (sync (at-time-evt t)) (note! 'second)))
                          (cml/sleep 0.06)
                          (cml/shutdown)))))))

  ((test/sync/timeout _)
   (⊦= '(none (got 7) timeout #f)
       (cdr (run/log (τ (let1 (c (make-channel))
                          (note! (sync/timeout (recv-evt c) 0.01 'none))
                          (spawn (τ (send c 7)))
                          (note! (list 'got (sync/timeout (recv-evt c) 1)))
                          (note! (select (wrap (timeout-evt 0.01) (λ (x) 'timeout)) (recv-evt c)))
                          (note! (sync/timeout never-evt 0.01))                ; the default default is #f
                          (cml/shutdown)))))))

  ((test/sync/timeout/zero-is-a-poll _)
   ; with 0 (or less) seconds sync/timeout polls: an enabled event is always chosen, the default
   ; comes only when nothing is enabled (an expired timeout-evt would compete with the event under
   ; select's priorities, and usually win)
   (⊦= '(42 (1 1 1 1) (7 7 7) (x x) none none d)
       (cdr (run/log (τ (let ((ch (make-channel)) (iv (make-ivar)))
                          (spawn (τ (send ch 42)))
                          (cml/yield)
                          (note! (sync/timeout (recv-evt ch) 0 'none))
                          (note! (map (λ (i) (sync/timeout (always-evt 1) 0 'd)) (iota 4)))
                          (ivar-put! iv 7)
                          (note! (map (λ (i) (sync/timeout (ivar-get-evt iv) 0 'd)) (iota 3)))
                          (note! (map (λ (i) (sync/timeout (choose (recv-evt ch) (always-evt 'x)) -1 'd)) (iota 2)))
                          (note! (sync/timeout (recv-evt ch) 0 'none))
                          (note! (sync/timeout (choose (recv-evt ch) never-evt) 0 'none))
                          (note! (sync/timeout (timeout-evt 0.5) 0 'd))
                          (cml/shutdown)))))))

  ((test/timeouts/yield-loop _)
   ; a yield is a clock tick: a thread busy-waiting with cml/yield does not keep timeouts from firing
   (⊦= 'timer-fired
       (run-cml (τ (spawn (τ (let loop ((n 0))
                               (if (< n 1000000) (begin (cml/yield) (loop (add1 n))) (cml/shutdown 'starved)))))
                   (cml/sleep 0.02)
                   (cml/shutdown 'timer-fired))))
   (⊦= 'filled
       (run/value (τ (let1 (iv (make-ivar))
                       (spawn (τ (cml/sleep 0.02) (ivar-put! iv 'filled)))
                       (let loop ((n 0))
                         (cond
                           ((pair? (ivar-get-poll iv)) (car (ivar-get-poll iv)))
                           ((< n 1000000) (cml/yield) (loop (add1 n)))
                           (else 'starved))))))))

  ((test/timeouts/many-sleepers _)
   ; the pending timeouts are a heap: blocking n sleepers is O(n log n) and a CML operation does not
   ; pay for the ones that are not due (a sorted list, scanned at every preemption tick, made both
   ; O(n) per operation, and blocking n sleepers O(n^2)).  Ten times as many sleepers and operations
   ; must not take a hundred times as long
   (define (cost n)
     (let1 (t0 (current-process-milliseconds))
       (⊦= 'ok (run/value (τ (for-each (λ (i) (spawn (τ (cml/sleep (+ 3600 i))))) (iota n))
                             (let1 (ch (make-channel))
                               (spawn (τ (let loop () (send ch 1) (loop))))
                               (do ((i 0 (add1 i))) ((= i (* 4 n)) 'ok) (recv ch))))))
       (- (current-process-milliseconds) t0)))
   (let* ((small (max 20 (cost 300)))
          (large (cost 3000)))
     (⊨ (< large (* 30 small)))))

  ((test/timeouts/losing-timeouts-are-dropped _)
   ; a loop whose syncs block with a timeout and commit elsewhere leaves a stale entry each time:
   ; they are purged now and then, so the heap stays bounded
   (let1 (n (run/value (τ (let1 (ch (make-channel))
                            (spawn (τ (let loop () (cml/yield) (send ch 1) (loop))))
                            (do ((i 0 (add1 i))) ((= i 500)) (select (recv-evt ch) (timeout-evt 1000)))
                            %time-n))))
     (⊨ (<= n (+ (* 2 %clean-budget-min) 2)))))

  ((test/timeouts/cancelled-wait-not-woken _)
   ; the timeout registered by a sync that was committed elsewhere is stale and never wakes it
   (⊦= '(got after)
       (cdr (run/log (τ (let1 (c (make-channel))
                          (spawn (τ (sync (choose (wrap (timeout-evt 0.02) (λ (x) (note! 'timed-out)))
                                                  (wrap (recv-evt c) (λ (x) (note! 'got)))))))
                          (send c 'x)
                          (cml/sleep 0.05)
                          (note! 'after)
                          (cml/shutdown)))))))

  )

(define-suite cml-syncvar-suite

  ((test/ivar _)
   (⊦= '(() (r3 1) (1) 1 1 (r1 1) (r2 1) raised)
       (cdr (run/log (τ (let1 (iv (make-ivar))
                          (for-each (λ (tag) (spawn (τ (note! (list tag (ivar-get iv)))))) '(r1 r2 r3))
                          (note! (ivar-get-poll iv))
                          (ivar-put! iv 1)
                          (note! (ivar-get-poll iv))
                          (note! (ivar-get iv))
                          (note! (sync (ivar-get-evt iv)))
                          (cml/yield)
                          (note! (raised-or (exn cml put) (ivar-put! iv 2)))
                          (cml/shutdown)))))))

  ((test/ivar/predicates _)
   (let ((iv (make-ivar)) (mv (make-mvar)))
     (⊨ (ivar? iv)) (⊭ (ivar? mv)) (⊨ (mvar? mv)) (⊭ (mvar? iv))
     (⊨ (ivar=? iv iv)) (⊭ (ivar=? iv (make-ivar))) (⊨ (mvar=? mv mv)) (⊭ (mvar=? mv (make-mvar)))
     (⊨ (result? (make-result))) (⊭ (result? iv))
     (⊦raises (exn cml put) (ivar-put! iv 1) (ivar-put! iv 2))))

  ((test/ivar-mvar-kinds _)
   ; ivars and mvars are one record: every operation checks the kind, so an mvar take or swap
   ; cannot empty or overwrite an ivar (which a later ivar-put! would then write again), nor an
   ; ivar operation act on an mvar; the error is an ordinary one, raised in the caller
   (define (kind-errors ops)
     (map (λ (op) (condition-case (begin (op) 'no-raise) (e (exn cml) 'cml-raised) (e (exn) 'raised))) ops))
   (⊦= '((raised raised raised raised raised raised raised raised raised raised)
         (raised raised raised raised raised)
         (1 () (1)))
       (run/value (τ (let ((iv (make-ivar)) (mv (make-mvar)))
                       (ivar-put! iv 1)
                       (list (kind-errors (list (τ (mvar-take! iv)) (τ (mvar-take-evt iv)) (τ (mvar-take-poll iv))
                                                (τ (mvar-swap! iv 2)) (τ (mvar-swap-evt iv 2)) (τ (mvar-put! iv 2))
                                                (τ (mvar-get iv)) (τ (mvar-get-evt iv)) (τ (mvar-get-poll iv))
                                                (τ (sync (mvar-take-evt iv)))))
                             (kind-errors (list (τ (ivar-put! mv 1)) (τ (ivar-get mv)) (τ (ivar-get-evt mv))
                                                (τ (ivar-get-poll mv)) (τ (sync (ivar-get-evt mv)))))
                             (list (ivar-get iv) (mvar-take-poll mv) (ivar-get-poll iv)))))))
   (⊦raises (exn) (mvar-put! (make-ivar) 1))                   ; outside run-cml too
   (⊦raises (exn) (ivar-put! (make-mvar) 1)))

  ((test/condition-predicates _)
   (let ((put (condition-case (let1 (iv (make-ivar)) (ivar-put! iv 1) (ivar-put! iv 2)) (e () e)))
         (not-running (condition-case (sync (always-evt 1)) (e () e)))
         (other (condition-case (car '()) (e () e))))
     (⊨ (cml-condition? put)) (⊨ (cml-put-condition? put)) (⊭ (cml-not-running-condition? put))
     (⊨ (cml-condition? not-running)) (⊨ (cml-not-running-condition? not-running)) (⊭ (cml-put-condition? not-running))
     (⊭ (cml-condition? other)) (⊭ (cml-put-condition? other))))

  ((test/mvar _)
   (⊦= '(1 () 2 (2) 2 3 4 raised (taker 5) (swapped 6 7) (7))
       (cdr (run/log (τ (let1 (mv (make-mvar 1))
                          (note! (mvar-take! mv))
                          (note! (mvar-take-poll mv))
                          (mvar-put! mv 2)
                          (note! (mvar-get mv))
                          (note! (mvar-get-poll mv))
                          (note! (mvar-swap! mv 3))
                          (note! (sync (mvar-swap-evt mv 4)))
                          (note! (sync (mvar-take-evt mv)))
                          (mvar-put! mv 5)
                          (note! (raised-or (exn cml put) (mvar-put! mv 6)))
                          (spawn (τ (note! (list 'taker (mvar-take! mv)))))
                          (spawn (τ (note! (list 'swapped (mvar-swap! mv 7) (sync (mvar-get-evt mv))))))
                          (mvar-put! mv 6)
                          (cml/yield)
                          (note! (mvar-get-poll mv))
                          (cml/shutdown)))))))

  ((test/mvar/relay-chain _)
   ; readers [get A, get B, take C, get D]: a put reaches A, B and C, D stays blocked until the next put
   (⊦= '((c x) (a x) (b x) blocked (d y))
       (cdr (run/log (τ (let1 (mv (make-mvar))
                          (spawn (τ (note! (list 'a (mvar-get mv)))))
                          (spawn (τ (note! (list 'b (sync (mvar-get-evt mv))))))
                          (spawn (τ (note! (list 'c (mvar-take! mv)))))
                          (spawn (τ (note! (list 'd (mvar-get mv)))))
                          (mvar-put! mv 'x)
                          (cml/sleep 0.01)
                          (note! 'blocked)
                          (mvar-put! mv 'y)
                          (cml/sleep 0.01)
                          (cml/shutdown)))))))

  ((test/losing-waiters-are-dropped _)
   ; a select loop over variables that stay empty and the join-evt of a thread that never dies,
   ; against a busy channel, does not pile up the waiters of its lost syncs (ML keeps them until the
   ; next put, i.e. forever here, each one holding a continuation); they are dropped by a full clean
   ; every %clean-budget-min additions or so, so at most that many (plus the live one) stay
   (define (size q) (+ (length (%queue-front q)) (length (%queue-rear q))))
   (define bound (+ %clean-budget-min 2))
   (let1 (r (run/value (τ (let ((iv (make-ivar)) (mv (make-mvar)) (t (spawn (τ (sync never-evt)))) (ch (make-channel)) (blocked 0))
                            (spawn (τ (let loop () (cml/yield) (send ch 1) (loop))))
                            (for-each (λ (i) (select (ivar-get-evt iv) (mvar-take-evt mv) (mvar-get-evt mv) (join-evt t)
                                                     (recv-evt ch)))
                                      (iota 200))
                            (list (size (%cell-read-q iv)) (size (%cell-read-q mv)) (length (%cvar-state (%tid-dead t))))))))
     (⊨ (<= (car r) bound))
     (⊨ (<= (cadr r) bound))
     (⊨ (<= (caddr r) bound))))

  ((test/many-waiters-block-in-linear-time _)
   ; blocking n threads on one ivar, one mvar (evt), the join-evt of one thread or one mailbox (evt)
   ; costs O(1) amortized each: the stale waiters are dropped by a full clean only now and then (a
   ; full clean at every block made this O(n^2): about 3 s for 5000 compiled ivar waiters).  Ten
   ; times as many waiters must not take a hundred times as long
   (define (blocking-time n)
     (let1 (t0 (current-process-milliseconds))
       (⊦= (* 4 n)
           (run/value (τ (let* ((iv (make-ivar)) (mv (make-mvar)) (mb (make-mailbox)) (out (make-channel))
                                (stop (make-channel)) (t (spawn (τ (recv stop)))))
                           (for-each (λ (i)
                                       (spawn (τ (send out (ivar-get iv))))
                                       (spawn (τ (send out (sync (mvar-get-evt mv)))))
                                       (spawn (τ (sync (join-evt t)) (send out 1)))
                                       (spawn (τ (send out (sync (mailbox-recv-evt mb))))))
                                     (iota n))
                           (ivar-put! iv 1)
                           (mvar-put! mv 1)
                           (send stop #t)
                           (for-each (λ (i) (mailbox-send! mb 1)) (iota n))
                           (let loop ((i 0) (sum 0))
                             (if (= i (* 4 n)) sum (loop (add1 i) (+ sum (recv out)))))))))
       (- (current-process-milliseconds) t0)))
   (let* ((small (max 20 (blocking-time 200)))
          (large (blocking-time 2000)))
     (⊨ (< large (* 30 small)))))

  ((test/many-ready-threads-die-in-linear-time _)
   ; n threads woken together that die of an uncaught condition one after the other cost O(n): a
   ; death scans the ready queues only when the thread died in the middle of a switch (scanning
   ; them at every death made this O(n^2): about 2 s for 4000 compiled threads, against 30 ms)
   (define (dying-time n)
     (let1 (t0 (current-process-milliseconds))
       (⊦= n
           (parameterize ((default-exn-handler void))
             (run/value (τ (let* ((gate (make-ivar))
                                  (ts (map (λ (i) (spawn (τ (ivar-get gate) (error "die")))) (iota n))))
                             (ivar-put! gate #t)
                             (for-each (λ (t) (sync (join-evt t))) ts)
                             (length ts))))))
       (- (current-process-milliseconds) t0)))
   (let* ((small (max 20 (dying-time 300)))
          (large (dying-time 3000)))
     (⊨ (< large (* 30 small)))))

  ((test/result _)
   (⊦= '(1 1 raised raised "boom")
       (cdr (run/log (τ (let ((r (make-result)) (s (make-result)))
                          (spawn (τ (note! (result-get r))))
                          (result-put! r 1)
                          (note! (sync (result-get-evt r)))
                          (result-put-exn! s (make-property-condition 'exn 'message "boom"))
                          (note! (raised-or (exn) (result-get s)))
                          (note! (raised-or (exn cml put) (result-put! s 2)))
                          (note! (condition-case (sync (result-get-evt s))
                                   (e (exn) ((condition-property-accessor 'exn 'message) e))))
                          (cml/shutdown)))))))

  )

(define-suite cml-mailbox-suite

  ((test/mailbox/ordering _)
   (⊦= (append (iota 50) '(()))
       (cdr (run/log (τ (let1 (mb (make-mailbox))
                          (for-each (λ (i) (mailbox-send! mb i)) (iota 50))     ; never blocks
                          (for-each (λ (i) (note! (mailbox-recv mb))) (iota 25))
                          (for-each (λ (i) (note! (sync (mailbox-recv-evt mb)))) (iota 25))
                          (note! (mailbox-recv-poll mb))
                          (cml/shutdown)))))))

  ((test/mailbox/blocking-recv _)
   (⊦= '((r1 a) (r2 b) sent (1) timeout)
       (cdr (run/log (τ (let1 (mb (make-mailbox))
                          (spawn (τ (note! (list 'r1 (mailbox-recv mb)))))
                          (spawn (τ (note! (list 'r2 (sync (mailbox-recv-evt mb))))))
                          (mailbox-send! mb 'a)
                          (mailbox-send! mb 'b)
                          (note! 'sent)
                          (cml/sleep 0.01)
                          (mailbox-send! mb 1)
                          (note! (mailbox-recv-poll mb))
                          (note! (sync/timeout (mailbox-recv-evt mb) 0.01 'timeout))
                          (cml/shutdown)))))))

  ((test/mailbox/predicates _)
   (let1 (mb (make-mailbox))
     (⊨ (mailbox? mb)) (⊭ (mailbox? (make-channel))) (⊨ (mailbox=? mb mb)) (⊭ (mailbox=? mb (make-mailbox)))
     (⊨ (channel? (make-channel))) (⊭ (channel=? (make-channel) (make-channel)))
     (let1 (c (make-channel)) (⊨ (channel=? c c)))))

  )

(define-suite cml-barrier-suite

  ((test/barrier/rounds-in-linear-time _)
   ; a round of n waiters costs O(n) (ML keeps a counter), not O(n) per arrival to count the
   ; arrived threads (O(n^3) per round, about 1.4 s for 1000 threads compiled): 6 times as many
   ; threads must not take 15 times as long
   (define (rounds-time n)
     (let1 (t0 (current-process-milliseconds))
       (⊦= 3 (run/value (τ (let* ((b (make-barrier 0 add1))
                                  (es (map (λ (i) (barrier-enroll b)) (iota n)))
                                  (ts (map (λ (e) (spawn (τ (barrier-wait e) (barrier-wait e) (barrier-wait e)))) es)))
                             (for-each (λ (t) (sync (join-evt t))) ts)
                             (barrier-value (car es))))))
       (- (current-process-milliseconds) t0)))
   (let* ((small (max 20 (rounds-time 100)))
          (large (rounds-time 600)))
     (⊨ (< large (* 15 small)))))

  ((test/barrier/rounds _)
   (let1 (log (cdr (run/log (τ (let* ((b (make-barrier 0 add1))
                                      (es (map (λ (i) (barrier-enroll b)) (iota 3)))
                                      (ts (map (λ (e tag)
                                                 (spawn (τ (for-each (λ (i) (note! (list tag (barrier-wait e)))) (iota 4)))))
                                               es '(x y z))))
                                 (for-each (λ (t) (sync (join-evt t))) ts)
                                 (note! (barrier-value (car es)))
                                 (cml/shutdown))))))
     (⊦= 4 (last log))
     (⊦= '(1 2 3 4) (map cadr (filter (λ (x) (and (pair? x) (eq? 'x (car x)))) log)))
     (⊦= '(1 2 3 4) (map cadr (filter (λ (x) (and (pair? x) (eq? 'z (car x)))) log)))
     (⊦= 13 (length log))))

  ((test/barrier/wake-order _)
   ; a completed round wakes its waiters oldest first, a deliberate departure from barrier.sml
   ; (whose List.app over its newest-first list wakes them newest first)
   (⊦= '(1 2 3)
       (run/value (τ (let* ((b (make-barrier 0 add1))
                            (es (map (λ (i) (barrier-enroll b)) (iota 4)))
                            (mb (make-mailbox)))
                       (for-each (λ (e i) (spawn (τ (barrier-wait e) (mailbox-send! mb i)))) (cdr es) '(1 2 3))
                       (cml/yield)
                       (barrier-wait (car es))
                       (map (λ (i) (mailbox-recv mb)) '(1 2 3)))))))

  ((test/barrier/resign-completes-round _)
   (⊦= '((a 1) (b 1) resigned-again raised raised)
       (cdr (run/log (τ (let* ((b (make-barrier 0 add1))
                               (ea (barrier-enroll b)) (eb (barrier-enroll b)) (ec (barrier-enroll b)))
                          (spawn (τ (note! (list 'a (barrier-wait ea)))))
                          (spawn (τ (note! (list 'b (barrier-wait eb)))))
                          (barrier-resign ec)                          ; the two waiters now complete
                          (cml/sleep 0.01)
                          (barrier-resign ec)
                          (note! 'resigned-again)
                          (note! (raised-or (exn cml barrier) (barrier-wait ec)))
                          ; ea waits alone (eb enrolled but idle) and a second wait on it is refused
                          (spawn (τ (barrier-wait ea)))
                          (note! (raised-or (exn cml barrier) (sync (barrier-wait-evt ea))))
                          (cml/shutdown)))))))

  ((test/barrier/update-raises _)
   (⊦= '((b raised) (a raised) 5 (b 6) (a 6))
       (cdr (run/log (τ (let* ((b (make-barrier 5 (let1 (n 0) (λ (x) (set! n (add1 n)) (if (= n 1) (error "no") (add1 x))))))
                               (ea (barrier-enroll b)) (eb (barrier-enroll b)))
                          (spawn (τ (note! (list 'a (raised-or (exn) (barrier-wait ea))))))
                          (note! (list 'b (raised-or (exn) (barrier-wait eb))))  ; the last arrival goes on
                          (cml/yield)
                          (note! (barrier-value ea))                    ; unchanged
                          (spawn (τ (note! (list 'a (barrier-wait ea)))))
                          (note! (list 'b (barrier-wait eb)))
                          (cml/yield)
                          (cml/shutdown)))))))

  ((test/barrier/doc-example _)
   ; the barrier.mldoc clock: the parent enrolls before spawning so that no round completes early,
   ; then resigns; each child waits until the clock reaches 5
   (⊦= '(5 5)
       (cdr (run/log (τ (let* ((clock (make-barrier 0 add1))
                               (parent (barrier-enroll clock))
                               (spawn-child (τ (let1 (e (barrier-enroll clock))
                                                 (spawn (τ (let loop ()
                                                             (if (= 5 (barrier-wait e)) (note! (barrier-value e)) (loop))))))))
                               (ts (list (spawn-child) (spawn-child))))
                          (barrier-resign parent)
                          (for-each (λ (t) (sync (join-evt t))) ts)
                          (cml/shutdown)))))))

  ((test/barrier/predicates _)
   (let* ((b (make-barrier 0 add1)) (e (barrier-enroll b)))
     (⊨ (barrier? b)) (⊭ (barrier? e)) (⊨ (enrollment? e)) (⊭ (enrollment? b))))

  ((test/barrier/one-sync-counts-once _)
   ; a sync is one arrival: two waits on one enrollment in a select raise, as a second Barrier.wait
   ; does in ML, and a sync on the waits of two enrollments does not complete a round by itself
   (define (three) (let1 (b (make-barrier 0 add1)) (list b (barrier-enroll b) (barrier-enroll b) (barrier-enroll b))))
   (⊦= '((a raised) timeout 0)
       (cdr (run/log (τ (let* ((l (three)) (e1 (cadr l)) (e2 (caddr l)))
                          (spawn (τ (note! (list 'a (raised-or (exn cml barrier)
                                                      (select (barrier-wait-evt e1) (barrier-wait-evt e1)))))))
                          (note! (sync/timeout (barrier-wait-evt e2) 0.02 'timeout))
                          (note! (barrier-value e1))
                          (cml/shutdown))))))
   (⊦= '(u-blocked 0)
       (cdr (run/log (τ (let* ((l (three)) (e1 (cadr l)) (e2 (caddr l)) (e3 (cadddr l)))
                          (spawn (τ (note! (list 't (select (barrier-wait-evt e1) (barrier-wait-evt e2))))))
                          (let1 (u (spawn (τ (note! (list 'u (barrier-wait e3))))))
                            (note! (sync/timeout (wrap (join-evt u) (λ ignored 'u-done)) 0.02 'u-blocked))
                            (note! (barrier-value e1))
                            (cml/shutdown))))))))

  ((test/barrier/wait-evt-in-choice _)
   ; a wait that loses a choice does not count as arrived
   (⊦= '(timeout (a 1) (b 1))
       (cdr (run/log (τ (let* ((bar (make-barrier 0 add1)) (ea (barrier-enroll bar)) (eb (barrier-enroll bar)))
                          (note! (sync/timeout (barrier-wait-evt ea) 0.01 'timeout))
                          (spawn (τ (note! (list 'b (barrier-wait eb)))))
                          (note! (list 'a (barrier-wait ea)))
                          (cml/yield)
                          (cml/shutdown)))))))

  )

(define-suite cml-cleanup-suite

  ((test/cleaners _)
   ; the registrations are undone even when an assertion fails, so that no later case sees them
   (set! cml-test/log '())
   (dynamic-wind
     void
     (τ (⊦= '() (cml/add-cleaner! "t1" '(at-init at-shutdown) (λ (w) (note! (list 't1 w)))))
        (⊦= '() (cml/add-cleaner! "t2" 'at-init (λ (w) (note! (list 't2 w)))))
        (⊦= '() (cml/add-cleaner! "t3" 'at-shutdown (λ (w) (note! (list 't3 w)))))
        (⊦= 'x (run-cml (τ (note! 'main) (cml/shutdown 'x))))
        ; at-init in registration order, at-shutdown in reverse
        (⊦= '((t1 at-init) (t2 at-init) main (t3 at-shutdown) (t1 at-shutdown)) (reverse cml-test/log))
        (let1 (old (cml/add-cleaner! "t2" 'at-shutdown (λ (w) (void))))
          (⊦= 1 (length old))
          (⊦= '(at-init) (car (car old))))
        (⊦= 1 (length (cml/remove-cleaner! "t1")))
        (⊦= 1 (length (cml/remove-cleaner! "t2")))
        (⊦= 1 (length (cml/remove-cleaner! "t3")))
        (⊦= '() (cml/remove-cleaner! "t3"))
        ; 'all is every when, cml/at-all; run-cml triggers at-init and at-shutdown only
        (⊦= '(at-exit at-shutdown at-init at-init-fn) cml/at-all)
        (⊦= '() (cml/add-cleaner! "t4" 'all (λ (w) (note! (list 't4 w)))))
        (set! cml-test/log '())
        (⊦= 'y (run-cml (τ (note! 'main) (cml/shutdown 'y))))
        (⊦= '((t4 at-init) main (t4 at-shutdown)) (reverse cml-test/log))
        (⊦= (list cml/at-all) (map car (cml/remove-cleaner! "t4"))))
     (τ (for-each cml/remove-cleaner! '("t1" "t2" "t3" "t4"))))
   (⊦= "[000002]" (run/value (τ (tid->string (current-tid))))))

  ((test/logged-channel _)
   ; Channel.resetChan at run boundaries: a logged channel is emptied and its priority is back to 1,
   ; an unlogged one keeps its stale entries (harmless, see test/stale-across-runs); a logged mailbox
   ; forgets its messages
   (let ((ch (make-channel)) (control (make-channel)) (mb (make-mailbox)))
     (define (state c) (list (%channel-priority c) (%q-empty? (%channel-out-q c))))
     (dynamic-wind
       (τ (cml/log-channel! "test-ch" ch) (cml/log-mailbox! "test-mb" mb))
       (τ (⊦= '(first x x)
              (run/log (τ (for-each (λ (c) (spawn (τ (send c 'stale)))) (list ch control))
                          ; a recv polled with a sender waiting but losing the select bumps the priority
                          (note! (select (recv-evt ch) (always-evt 'x)))
                          (note! (select (recv-evt control) (always-evt 'x)))
                          (mailbox-send! mb 'old)
                          (cml/shutdown 'first))))
          (⊦= '((1 #t) (2 #f)) (list (state ch) (state control)))
          (⊦= '(() ()) (run/value (τ (list (recv-poll ch) (mailbox-recv-poll mb))))))
       (τ (cml/unlog-channel! "test-ch") (cml/unlog-mailbox! "test-mb")))
     (⊦raises (exn cml unlog) (cml/unlog-channel! "test-ch"))))

  ((test/unlog-all _)
   (let1 (mb (make-mailbox))
     (cml/log-mailbox! "test-mb-all" mb)
     (cml/unlog-all!)
     (⊦raises (exn cml unlog) (cml/unlog-mailbox! "test-mb-all"))
     (run-cml (τ (mailbox-send! mb 'kept) (cml/shutdown)))       ; no longer reset at the run boundary
     (⊦= '(kept) (run/value (τ (mailbox-recv-poll mb))))))

  ((test/stale-across-runs _)
   ; even unlogged, what a previous run left blocked in a channel or an ivar is ignored
   (let ((ch (make-channel)) (iv (make-ivar)))
     (⊦= 'first (run-cml (τ (spawn (τ (send ch 'stale))) (spawn (τ (ivar-get iv))) (cml/shutdown 'first))))
     (⊦= '(() #f ok) (run/value (τ (ivar-put! iv 'ok) (list (recv-poll ch) (send-poll ch 1) (ivar-get iv)))))))

  ((test/stale-waiters-of-blocking-operations-are-dropped _)
   ; the blocking send, recv and mailbox-recv drop the stale waiters, those left by previous runs
   ; included, as they add theirs (as the events do): a global channel or mailbox that every run
   ; only receives (or only sends) on keeps at most a few of them, not one per run, each holding a
   ; dead thread's continuation
   (define (size q) (+ (length (%queue-front q)) (length (%queue-rear q))))
   (define (mb-size mb) (let1 (st (%mailbox-state mb)) (if (eq? 'empty (car st)) (+ (length (cadr st)) (length (cddr st))) 0)))
   (define bound (* 2 (+ %clean-budget-min 2)))
   (let ((in (make-channel)) (out (make-channel)) (mb (make-mailbox)))
     (for-each (λ (i) (run-cml (τ (spawn (τ (recv in))) (spawn (τ (send out i))) (spawn (τ (mailbox-recv mb)))
                                  (cml/yield) (cml/shutdown))))
               (iota 200))
     (⊨ (<= (size (%channel-in-q in)) bound))
     (⊨ (<= (size (%channel-out-q out)) bound))
     (⊨ (<= (mb-size mb) bound))))

  ((test/bad-arguments-raise-at-registration _)
   ; a cleaner that is not a procedure, or a bad when, raises when it is registered: the cleaners
   ; run in run-cml's own context, where it made every later run-cml raise before running its
   ; thunk; a logged item of the wrong type raises too: the resets of the logged channels and
   ; mailboxes run in one cleaner thread, which it killed at every start and shutdown, skipping
   ; the items after it (a mailbox logged later kept its stale messages); likewise for a server
   (define (raises? thunk) (condition-case (begin (thunk) 'no-raise) (e (exn) 'raised)))
   (let1 (mb (make-mailbox))
     (dynamic-wind
       void
       (τ (⊦= '(raised raised raised raised raised raised raised raised)
              (map raises? (list (τ (cml/add-cleaner! "test-bad" 'at-init 42))
                                 (τ (cml/add-cleaner! "test-bad" 'bogus void))
                                 (τ (cml/add-cleaner! "test-bad" '(at-init bogus) void))
                                 (τ (cml/add-cleaner! "test-bad" 42 void))
                                 (τ (cml/log-channel! "test-bad" (make-mailbox)))
                                 (τ (cml/log-mailbox! "test-bad" (make-channel)))
                                 (τ (cml/log-server! "test-bad" 42 void))
                                 (τ (cml/log-server! "test-bad" void 42)))))
          (⊦= '() (cml/remove-cleaner! "test-bad"))
          (map (λ (unlog) (⊦raises (exn cml unlog) (unlog "test-bad")))
               (list cml/unlog-channel! cml/unlog-mailbox! cml/unlog-server!))
          (cml/log-mailbox! "test-mb-after" mb)
          (⊦= 'ok (run-cml (τ (mailbox-send! mb 'stale) (cml/shutdown 'ok))))
          (⊦= '(ran ()) (run/value (τ (list 'ran (mailbox-recv-poll mb))))))
       (τ (condition-case (cml/unlog-mailbox! "test-mb-after") (e () (void)))
          (cml/remove-cleaner! "test-bad")
          (for-each (λ (unlog) (condition-case (unlog "test-bad") (e () (void))))
                    (list cml/unlog-channel! cml/unlog-mailbox! cml/unlog-server!))))))

  ((test/registration-lock-given-back-by-a-dying-thread _)
   ; the lock of the CleanUp registry is an internal hold: a thread that dies holding it (its after
   ; thunk raising as it is preempted right after the take, with quantum 1) gives it back, or every
   ; later registration would block and the shutdown cleaners of the run would never run
   (set! cml-test/log '())
   (dynamic-wind
     (τ (cml/add-cleaner! "test-probe" 'at-shutdown (λ (w) (note! 'shutdown-cleaner))))
     (τ (parameterize ((default-exn-handler (λ (e) (note! 'died))))
          (⊦= 'ok (run-cml (τ (let1 (t (spawn (τ (let1 (armed #t)
                                                   (dynamic-wind
                                                     void
                                                     (τ (cml/add-cleaner! "test-dying" 'at-shutdown void))
                                                     (τ (when armed (set! armed #f) (error "after thunk"))))))))
                                (sync (join-evt t))
                                (note! (sync/timeout (wrap (join-evt (spawn (τ (cml/log-channel! "test-dying-ch" (make-channel)))))
                                                           (λ ignored 'logged))
                                                     0.5 'blocked))
                                (note! 'shutdown)
                                (cml/shutdown 'ok)))
                           quantum: 1)))
        (⊦= '(died logged shutdown shutdown-cleaner) (reverse cml-test/log)))
     (τ (for-each cml/remove-cleaner! '("test-probe" "test-dying"))
        (condition-case (cml/unlog-channel! "test-dying-ch") (e () (void))))))

  ((test/logged-server _)
   (set! cml-test/log '())
   (dynamic-wind
     (τ (cml/log-server! "srv" (τ (note! 'start)) (τ (note! 'stop))))
     (τ (run-cml (τ (note! 'main) (cml/shutdown))))
     (τ (cml/unlog-server! "srv")))
   (run-cml (τ (cml/shutdown)))
   (⊦= '(start main stop) (reverse cml-test/log)))

  ((test/debug _)
   (⊦= "" (with-error-output-to-string (τ (cml/debug "quiet"))))
   (⊦= "[-000001] hello 1\n" (with-error-output-to-string (τ (parameterize ((cml/debug? #t)) (cml/debug "hello" 1))))))

  )

(define-suite cml-os-suite

  ((test/io-evt/pipe _)
   (receive (in out) (create-pipe)
     (let1 (log (cdr (run/log (τ (spawn (τ (cml/sleep 0.02) (note! 'writing) (file-write out (string->utf8 "hello\n"))))
                                 (note! (list 'ready (= in (sync (io-evt in 'input)))))
                                 (note! (read-line (open-input-file* in)))
                                 (note! (list 'out (= out (sync (io-evt out 'output)))))
                                 (cml/shutdown)))))
       (⊦= '(writing (ready #t) "hello" (out #t)) log))
     (file-close out)))

  ((test/poll-evt _)
   (receive (in1 out1) (create-pipe)
     (receive (in2 out2) (create-pipe)
       (let1 (log (cdr (run/log (τ (note! (sync/timeout (poll-evt (list in1 'input) (list in2 'input)) 0.02 'nothing))
                                   (spawn (τ (cml/sleep 0.01) (file-write out2 (string->utf8 "x"))))
                                   (note! (sync (poll-evt (list in1 'input) (list in2 'input))))
                                   (file-write out1 (string->utf8 "y"))
                                   (note! (length (sync (poll-evt (list in1 'input) (list in2 'input)))))
                                   (cml/shutdown)))))
         (⊦= `(nothing ((,in2 input)) 2) log))
       (⊦= `((,in1 input) (,in2 input)) (run/value (τ (sync (poll-evt* (list (list in1 'input) (list in2 'input)))))))
       (⊨ (eq? never-evt (poll-evt* '())))
       (for-each file-close (list in1 out1 in2 out2)))))

  ((test/io-evt/yield-loop _)
   ; a thread busy-waiting with cml/yield does not keep descriptors from being polled
   (receive (in out) (create-pipe)
     (⊦= 'io-ready
         (run/value (τ (let1 (iv (make-ivar))
                         (spawn (τ (sync (io-evt in 'input)) (ivar-put! iv 'io-ready)))   ; blocks: no data yet
                         (file-write out (string->utf8 "x"))
                         (let loop ((n 0))
                           (cond
                             ((pair? (ivar-get-poll iv)) (car (ivar-get-poll iv)))
                             ((< n 1000000) (cml/yield) (loop (add1 n)))
                             (else 'starved)))))))
     (for-each file-close (list in out))))

  ((test/io-evt/idle-waiters-do-not-slow-others _)
   ; descriptors are polled at a preemption tick only once 2 ms (or ten times the last poll) have
   ; passed since the last poll, not at every tick: 2000 threads idle on a descriptor (a select over
   ; 2000 specs at every tick made this some ten times slower) leave channel hand-offs as fast
   (define (hand-off-time n)
     (receive (r w) (create-pipe)
       (let1 (t (run-cml (τ (for-each (λ (i) (spawn (τ (sync (io-evt r 'input))))) (iota n))
                            (cml/sleep 0.02)
                            (let ((ch (make-channel)) (t0 (current-process-milliseconds)))
                              (spawn (τ (let loop ((i 0)) (when (< i 20000) (send ch i) (loop (add1 i))))))
                              (let loop ((i 0)) (when (< i 20000) (recv ch) (loop (add1 i))))
                              (cml/shutdown (- (current-process-milliseconds) t0))))))
         (file-close r)
         (file-close w)
         t)))
   (let* ((none (max 50 (hand-off-time 0)))
          (idle (hand-off-time 2000)))
     (⊨ (< idle (* 4 none)))))

  ((test/process-evt _)
   (let1 (log (cdr (run/log (τ (let* ((p1 (process-run "true"))
                                      (p2 (process-run "sh" '("-c" "exit 3")))
                                      (e1 (process-evt p1)))
                                 (note! (eq? e1 (process-evt p1)))           ; memoized per pid
                                 (note! (sync (process-evt p2)))
                                 (note! (sync e1))
                                 (note! (sync e1))
                                 (cml/shutdown))))))
     (⊦= '(#t (#t 3) (#t 0) (#t 0)) log))
   ; the memo keeps the pids still running only: it does not grow with every child of a run (and a
   ; numeric pid reused by the kernel later is waited for afresh); a reaped numeric pid cannot be
   ; waited for again, while a process object keeps its status
   (⊦= '(0 (#t 0) (#t 0) (#t 0) raised)
       (cdr (run/log (τ (for-each (λ (i) (sync (process-evt (process-run "true")))) (iota 20))
                        (note! (length %proc-memo))
                        (let ((p (process-run "true")) (q (process-run "true")))
                          (note! (sync (process-evt p)))
                          (note! (sync (process-evt p)))
                          (note! (sync (process-evt (process-id q))))
                          (note! (raised-or (exn) (sync (process-evt (process-id q))))))
                        (cml/shutdown))))))

  ((test/process-evt/bad-pid _)
   ; a pid that is neither a positive exact integer nor a process object raises when the event is
   ; made, in the caller (it was queued and failed at sync with a misleading ECHILD)
   (⊦= '(raised raised raised raised)
       (run/value (τ (map (λ (pid) (condition-case (begin (process-evt pid) 'no-raise) (e (exn) 'raised)))
                          (list 'y "x" 0 -1))))))

  ((test/process-evt/keeps-run-alive _)
   ; a pending child process is something to wait for: no deadlock while it runs
   (⊦= '(done (#t 0))
       (cdr (run/log (τ (let1 (p (process-run "sh" '("-c" "sleep 0.05")))
                          (spawn (τ (note! (sync (process-evt p)))))
                          (note! 'done)))))))

  )

(define-suite cml-examples-suite

  ((doc r) `((structure/section "Classic CML examples")))

  ((test/sieve _)
   (define (counter n) (let1 (ch (make-channel)) (spawn (τ (let loop ((i n)) (send ch i) (loop (add1 i))))) ch))
   (define (sift p in)
     (let1 (out (make-channel))
       (spawn (τ (let loop () (let1 (i (recv in)) (unless (zero? (remainder i p)) (send out i))) (loop))))
       out))
   (define (primes)
     (let1 (ch (make-channel))
       (spawn (τ (let loop ((in (counter 2))) (let1 (p (recv in)) (send ch p) (loop (sift p in))))))
       ch))
   (⊦= '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47)
       (run/value (τ (let1 (ps (primes)) (map (λ (i) (recv ps)) (iota 15)))))))

  ((test/server-with-nack _)
   ; a server replying to requests; a client that gives up (its timeout wins) sends a nack and
   ; the server, instead of blocking forever on the reply, notices it
   (define (make-server)
     (let1 (req (make-channel))
       (spawn (τ (let loop ((n 0))
                   (let* ((r (recv req)) (reply (car r)) (nack (cdr r)))
                     (select (wrap (send-evt reply n) (λ (x) (note! (list 'replied n))))
                             (wrap nack (λ (x) (note! (list 'abandoned n)))))
                     (loop (add1 n))))))
       (λ () (with-nack (λ (nack)
                          (let1 (reply (make-channel))
                            (spawn (τ (send req (cons reply nack))))
                            (recv-evt reply)))))))
   (⊦= '((got 0) (replied 0) gave-up (abandoned 1) (replied 2) (got 2))
       (cdr (run/log (τ (let1 (request-evt (make-server))
                          (note! (list 'got (sync (request-evt))))
                          (note! (select (request-evt) (wrap (timeout-evt 0) (λ (x) 'gave-up))))
                          (cml/sleep 0.01)
                          (let1 (v (sync (request-evt))) (cml/yield) (note! (list 'got v)))
                          (cml/shutdown)))))))

  )

(unittest/✓ cml-events-suite)
(unittest/✓ cml-threads-suite)
(unittest/✓ cml-time-suite)
(unittest/✓ cml-syncvar-suite)
(unittest/✓ cml-mailbox-suite)
(unittest/✓ cml-barrier-suite)
(unittest/✓ cml-cleanup-suite)
(unittest/✓ cml-os-suite)
(unittest/✓ cml-examples-suite)
