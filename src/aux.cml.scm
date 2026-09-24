;; (aux cml) -- Concurrent ML for CHICKEN Scheme, a port of SML/NJ CML (smlnj/libraries/cml).
;;
;; The file has three parts, in this order:
;;   1. the core (src/core-cml, util/result.sml, Unix/proc-manager.sml, glue/*): scheduler, events,
;;      threads, channels, timeouts, IO manager, SyncVar, Mailbox, Barrier, CleanUp and RunCML;
;;   2. the IO / OS layer (Unix/os-process.sml, IO/new-text-io-fn.sml, IO/chan-io-fn.sml,
;;      Sockets/cml-socket.sml): port events, channel ports, system-evt, cml/execute and tcp events;
;;   3. cml-lib (cml-lib/*.sml): Multicast, SimpleRPC and TraceCML.
;; Each of the last two starts with its own design notes and deviations from ML.
;;
;; Design of the core, with the ML origin of each piece:
;;
;; - threads are first-class continuations (`%letcc/call`, i.e. plain call/cc, so that dynamic-wind
;;   frames, parameterize and exception handlers are saved and restored per thread on every switch);
;;   there is no srfi-18 underneath.  A new thread does NOT run in its parent's dynamic context: it is
;;   started from an "isolation" continuation captured once by `run-cml` (ML's `isolate`), so it only
;;   inherits the dynamic context of the `run-cml` call itself (e.g. `current-output-port` as seen by
;;   `run-cml`).  User `dynamic-wind` frames established inside a thread have their after/before
;;   thunks run each time that thread is switched out/in (standard call/cc semantics), see below.
;; - scheduler.sml: two FIFO ready queues rdyQ1 (primary) and rdyQ2 (compute-bound threads), a
;;   current-thread register and a single, NON-nesting atomic flag with the two states
;;   non-atomic / atomic (ML's third state, signal-pending, is set only by its SIGALRM handler,
;;   which has no counterpart here) and the atomicBegin / atomicEnd / atomicDispatch /
;;   atomicSwitchTo discipline.  Preemption is cooperative: every `%atomic-end` (i.e. every CML
;;   operation, the read-only polls ivar-get-poll and mvar-get-poll included) and every
;;   `%atomic-switch-to` (the hand-off of a channel rendezvous) is a clock tick; when
;;   `quantum` ticks have elapsed the running thread is preempted as ML's SIGALRM handler does (a
;;   thread marked `done-comm` is unmarked and goes to the rear of rdyQ1, an unmarked one is demoted
;;   to rdyQ2) except that one thread is promoted from rdyQ2 at every preemption (ML: only when the
;;   preempted thread was marked), then the scheduler hook polls timeouts (and descriptors and child
;;   processes, once 2 ms, or ten times as long as their last poll took, have passed since it) and
;;   dispatches.  A thread that computes without ever calling a CML operation is never preempted;
;;   `cml/yield` is a tick too, and a yielding thread is preempted like a marked one.
;; - event.sml: base events are poll thunks returning (make-%enabled prio do-thunk) or
;;   (make-%blocked (λ (trans cleanup next) ...)); prio >= 0 is dynamic and -1 fixed (counted as the
;;   number of enabled events); the choice takes the max priority starting from 0 and breaks ties with
;;   ML's wrapping counter; one transaction box is shared by every base event of a blocked sync and
;;   queues drop cancelled entries lazily; guard / with-nack are forced at every sync, left to right,
;;   and each nack is signalled exactly once for the branches that were not chosen (chkCVars).
;; - options (ML `'a option`) are encoded as '() for NONE and (list v) for SOME v, everywhere.
;; - times are real seconds: (timeout-evt 0.5), (at-time-evt (+ (cml/now) 1)).
;; - CML's own errors are CHICKEN conditions of kinds (exn cml <kind>), e.g. (exn cml put) for a
;;   double put, (exn cml not-running) when a blocking operation (sync, send, recv, their polls, the
;;   SyncVar takes/gets/swaps, the Mailbox operations, ...) is attempted outside `run-cml`, (exn cml
;;   barrier) for a misused barrier; ivar-put! and mvar-put!, and the non-blocking polls
;;   ivar-get-poll, mvar-take-poll, mvar-get-poll and mailbox-recv-poll, work outside and never
;;   wake anybody there, while mailbox-send! (which may hand off to a receiver) raises.  Bad arguments (sync on a non-event, a guard returning
;;   a non-event, a non-port given to a port event, an ivar given to an mvar operation, a cleaner
;;   that is not a procedure, ...) raise ordinary errors, of kind (exn) only, in the caller.
;; - `guard` is CML's guard combinator: a client importing (chicken base) as well should write
;;   (import (except (chicken base) guard)) to drop the R7RS exception-handling syntax.
;;
;; Deviations from ML, all deliberate: (make-barrier init update) takes the initial state first
;; (ML: Barrier.barrier update init); Barrier's three bugs are fixed, waits are events
;; (barrier-wait-evt) and a completed round wakes its waiters oldest first (ML: newest first);
;; `recv` leaves the atomic region when resumed; tid->string prints negative ids as "[-000001]";
;; the clock is read on demand instead of being cached per quantum; the idle scheduler sleeps
;; in file-select until the next deadline instead of waiting for SIGALRM; blocked mailbox
;; receivers are filtered on enqueue; the default exception handler prints the condition;
;; a queue entry left by a previous run-cml session is stale (the transaction records its run), so
;; channels & co. that were not logged with the CleanUp registry cannot resume dead continuations;
;; the block functions of a blocked sync still run nested (event.sml's `log`) but the wrappers of an
;; event (wrap, wrap-handler) are kept apart from its block function and applied at the sync's own
;; continuation, so a server loop recursing from a wrap function does not grow, a wrap-handler
;; never catches what another branch raised (an ML bug) and a continuable condition stays
;; continuable; when a sync commits on an enabled event its nacks are set right after the commit,
;; before sync returns (event.sml sets them first, and so can cancel the very partner the commit
;; then dequeues); a sync abandoned while forcing (a guard or with-nack body that raises, escapes
;; with a continuation or exits its thread, or whose thread dies while switched out of it) or
;; polling (a poll that raises on a bad argument) sets the nacks made so far (ML never does, leaving
;; their servers waiting); the waiters left in SyncVar read queues and in
;; the cvars of join-evt and nacks by syncs that went another way are dropped as new ones are
;; added (ML keeps them until the next put, so a select loop leaks), and every queue of waiters,
;; channels' and mailboxes' included, drops them by a full clean only once the additions since the
;; previous one reach the length it left, so that blocking n threads costs O(n), not O(n^2) (ML
;; cleans a channel queue at every enqueue); the pending timeouts are a heap, so blocking n
;; sleepers is O(n log n) and a preemption tick does not scan them all (ML's sorted list is cleaned
;; whole at every poll, which is harmless at its 20 ms SIGALRM but not at every tick), and the
;; descriptors and child processes are polled at a tick only 2 ms (or ten times as long as that
;; poll took) after their last poll; a thread is promoted from rdyQ2 at every preemption (see
;; above), and at every multicast! (see Multicast); a rendezvous never resumes a thread that
;; died meanwhile (an after thunk that raised while it switched to its partner), a receiver whose
;; blocked sender dies as it is switched in (a before thunk that raised) receives again instead of
;; being left blocked on nothing, when a SyncVar reader dies so, the next reader is handed the
;; value the variable holds by then (the relay, or the take, was left to the dead one), when a
;; Mailbox receiver dies so, its message goes to the next receiver or back to the mailbox, and
;; when any thread dies so, the nacks of the blocked sync it was resumed from are set (see
;; %thread-died!); a dying thread's handler runs once everything others wait for is done, so it
;; may exit its thread or block; children
;; still running when their run ends are reaped by later runs without being waited for (ML's
;; ProcManager never forgets them either), and a reaped child's pid leaves process-evt's memo.
;; Not in ML at all: sync/timeout, whose 0 (or negative) seconds make it a poll.
;;
;; Internal hooks for later layers (ports, tcp, ...), all exported and marked with a `%`:
;;   (%base-evt poll)                 a base event from a poll thunk (see the protocol above)
;;   make-%enabled, make-%blocked     poll results; do-thunks and block-fns must leave the atomic
;;                                    region exactly once (%atomic-end, a switch, or a dispatch);
;;                                    wrap keeps its function apart from a block-fn (%blocked-post)
;;   %atomic-begin, %atomic-end, %atomic-dispatch, %dispatch   Scheduler.atomicBegin & co.
;;   (%enqueue-thread! tid k)         wake tid by calling (k (void)) later, marking it
;;   (%enqueue-tmp-thread! thunk)     run thunk soon in a throw-away thread (ProcManager style)
;;   (%trans-live? trans) (%trans-cancel! trans)  transaction boxes given to block-fns
;;   (%add-os-poller! name poll waiting?)  extra polling done at every preemption tick (every
;;                                    `quantum` CML operations or yields) and while idle; waiting?
;;                                    gives #f, a bound in ms on the idle sleep, or #t (5 ms)
;; and the public (poll-evt* specs) / (io-evt fd mode), readiness of file descriptors, on which the
;; port events are built.  Every other name without a `%` is public API (e.g. TraceCML's
;; trace-exn-handler, uncaught-default-handler and trace-close-files!).
;;
;; Dynamic-wind and thread switches: the after/before thunks of a thread's frames run at every
;; switch out of/into it (every blocking sync, yield or preemption) and %cur-tid is already the
;; target of the switch: an after thunk runs on behalf of the OTHER thread, the one being switched
;; to (or the dummy tid [-000001] of the idle loop and of the scheduler, when the thread is
;; preempted), so there (current-tid) is that thread and a CML operation acts, and may block, as
;; that thread.  Hence these thunks must not use CML operations (sync, send, mvar-put!, ...) nor
;; (current-tid), and a dynamic-wind cannot protect a critical section: its after thunk would
;; release the lock at the first switch while the body is still inside; take and put an mvar
;; explicitly instead.  (Switching with continuation-graft would skip the winders, but then
;; parameterize, which is built on them, would leak between threads.)  The thunks must not raise
;; either: if an after thunk raises while its thread is being switched out and the thread does not
;; catch the condition, the thread dies of it (its handler runs, as that thread) and the switch goes
;; on to its target; a dead thread is never resumed, also when its entry was left in a ready queue.
;; A before thunk that raises while its thread is switched in kills it likewise (a blocked sender
;; being switched to by its receiver: the receiver receives again; a SyncVar reader being handed
;; the value: the next reader gets it; a Mailbox receiver being handed a message: the next receiver
;; gets it, or the mailbox keeps it; a thread resumed from a blocked sync with nacks: they are all
;; set); but if the thread catches that condition, whatever switched to it expecting an answer
;; (such a receiver) waits for good.  A thread that dies while switched out of the forcing of a
;; sync (its guard blocked), or out of a blocked sync, sets the nacks of that sync.  Either way
;; the dying thread's handler runs after the switch it interrupted has been queued to go on, so a
;; handler may call cml/exit or block without stranding the thread switched to.  Escaping `run-cml`
;; with a continuation ends the session (no at-shutdown cleaners run) and leaves CML ready for the
;; next `run-cml`.

(module (aux cml) *

  (import scheme
          (only (scheme base) make-parameter exact-integer?)
          (except (chicken base) guard)             ; CML.guard below replaces the R7RS guard syntax
          (chicken condition)
          (chicken time)
          (chicken file posix)
          (chicken process)
          (only (chicken process signal) signal-default signal/pipe)
          (chicken port)
          (chicken tcp)
          (chicken errno)
          (chicken bitwise)
          (chicken bytevector)
          (only (chicken io) read-byte)
          (only (chicken string) string-split ->string)
          (only srfi-1 filter any every remove append-map cons* fold)
          (aux base)
          (aux continuation))

  ; conditions -------------------------------------------------------------------------------------

  (define (%cml-condition kind msg args)
    (make-composite-condition
      (make-property-condition 'exn 'message msg 'arguments args 'location kind)
      (make-property-condition 'cml)
      (make-property-condition kind)))

  (define (%cml-raise kind msg . args) (abort (%cml-condition kind msg args)))

  (define cml-condition? (condition-predicate 'cml))
  (define cml-put-condition? (condition-predicate 'put))
  (define cml-not-running-condition? (condition-predicate 'not-running))

  (define (%impossible who) (%cml-raise 'impossible "impossible" who))

  ; queues (queue.sml) ------------------------------------------------------------------------------

  ; imperative two-list FIFO; rear is kept reversed, exactly as RepTypes.Q.  budget counts the
  ; enqueues left before the next full clean, see %clean-and-enqueue!
  (define-record %queue front rear budget)

  (define (%q) (make-%queue '() '() 0))
  (define (%q-empty? q) (and (null? (%queue-front q)) (null? (%queue-rear q))))
  (define (%q-enqueue! q x) (%queue-rear-set! q (cons x (%queue-rear q))))
  (define (%q-enqueue-front! q x) (%queue-front-set! q (cons x (%queue-front q))))
  (define (%q-reset! q) (%queue-front-set! q '()) (%queue-rear-set! q '()) (%queue-budget-set! q 0))

  ; Q.dequeue, for a queue known to be non-empty
  (define (%q-dequeue! q)
    (let1 (front (%queue-front q))
      (if (pair? front)
        (begin (%queue-front-set! q (cdr front)) (car front))
        (let1 (l (reverse (%queue-rear q)))
          (%queue-rear-set! q '())
          (%queue-front-set! q (cdr l))
          (car l)))))

  ; Q.next: #f when empty (items are never #f)
  (define (%q-next! q) (if (%q-empty? q) #f (%q-dequeue! q)))

  ; transaction ids (rep-types.sml trans_id) -------------------------------------------------------

  ; a box holding the owner tid (TRANS tid) or #f (CANCEL); a transaction made outside run-cml, in a
  ; previous run-cml session or by a thread that has died counts as cancelled, so objects that
  ; outlive a run (or a failed call outside of one) never resume a dead continuation
  (define-record %trans tid)

  (define (%trans-live? t)
    (let1 (tid (%trans-tid t))
      (and tid %running (eq? %run-id (%tid-run tid)) (not (%tid-dead? tid)))))
  (define (%trans-cancel! t) (%trans-tid-set! t #f))
  ; the transaction of a blocking operation, remembered by its thread (see %abandon-blocked!)
  (define (%mk-id) (let1 (t (make-%trans %cur-tid)) (%tid-trans-set! %cur-tid t) t))

  ; getIdFromTrans: claim the transaction and return its thread
  (define (%get-id-from-trans! t) (let1 (tid (%trans-tid t)) (%trans-tid-set! t #f) tid))

  ; queue items are (trans . k); these are channel.sml's clean / cleanRev / cleanAll
  (define (%stale? item) (not (%trans-live? (car item))))
  (define (%clean l) (cond ((null? l) l) ((%stale? (car l)) (%clean (cdr l))) (else l)))
  (define (%clean-rev l acc)
    (cond ((null? l) acc) ((%stale? (car l)) (%clean-rev (cdr l) acc)) (else (%clean-rev (cdr l) (cons (car l) acc)))))
  (define (%clean-all l) (reverse (%clean-rev l '())))

  ; cleanAndRemove: the first live item (removed) or #f
  (define (%clean-and-remove! q)
    (let1 (f (%clean (%queue-front q)))
      (if (pair? f)
        (begin (%queue-front-set! q (cdr f)) (car f))
        (let1 (rr (%clean-rev (%queue-rear q) '()))
          (%queue-rear-set! q '())
          (if (pair? rr)
            (begin (%queue-front-set! q (cdr rr)) (car rr))
            (begin (%queue-front-set! q '()) #f))))))

  ; cleanAndEnqueue: drop every stale item, then append.  ML cleans the whole queue at every
  ; enqueue, which makes n waiters cost O(n^2); here a full clean runs only once the enqueues since
  ; the previous one reach the queue's length after it (at least %clean-budget-min), so an enqueue is
  ; O(1) amortized and stale items still never pile up (a select loop over a queue does not leak)
  (define %clean-budget-min 8)

  (define (%clean-and-enqueue! q item)
    (let1 (b (%queue-budget q))
      (if (positive? b)
        (begin (%queue-budget-set! q (sub1 b)) (%q-enqueue! q item))
        (let1 (f (%clean-all (%queue-front q)))
          (if (null? f)
            (begin (%queue-front-set! q (%clean-rev (%queue-rear q) (list item))) (%queue-rear-set! q '()))
            (begin (%queue-front-set! q f) (%queue-rear-set! q (cons item (%clean-all (%queue-rear q))))))
          (%queue-budget-set! q (max %clean-budget-min (+ (length (%queue-front q)) (length (%queue-rear q)))))))))

  ; thread ids (rep-types.sml thread_id) ------------------------------------------------------------

  ; forcing: the nack cells of the syncs this thread is forcing (see %force-group); watch: the
  ; (run . ivar) of a TraceCML watch (see watch); trans: the transaction of its latest blocking
  ; operation (see %mk-id); nacks: the flag sets of the blocked sync it waits in, when that sync has
  ; nacks (see %sync-on-grp), '() otherwise; holds: the internal locks (mvars) it holds, given back
  ; when it dies (see %release-holds!)
  (define-record %tid id done-comm exn-handler props dead run forcing watch trans nacks holds)

  ; RepTypes.tidToString, with negative ids printed as "[-000001]" instead of ML's "[0000~1]"
  (define (tid->string t)
    (let* ((id (%tid-id t))
           (s (number->string (abs id)))
           (pad (let loop ((s s)) (if (< (string-length s) 6) (loop (string-append "0" s)) s))))
      (string-append "[" (if (negative? id) "-" "") pad "]")))

  (set-record-printer! %tid (λ (t port) (display "#<tid " port) (display (tid->string t) port) (display ">" port)))

  ; CML.thread_id predicates and orders (thread.sml)
  (define (tid? x) (%tid? x))
  (define (tid=? a b) (= (%tid-id a) (%tid-id b)))                       ; CML.sameTid
  (define (tid<? a b) (< (%tid-id a) (%tid-id b)))
  ; CML.compareTid: -1, 0 or 1
  (define (tid-compare a b) (let ((a (%tid-id a)) (b (%tid-id b))) (cond ((< a b) -1) ((= a b) 0) (else 1))))
  (define (tid-hash t) (%tid-id t))                                      ; CML.hashTid

  ; scheduler state (scheduler.sml) -----------------------------------------------------------------

  (define %default-quantum 64)
  (define %idle-poll-interval-ms 5)

  (define %running #f)                   ; Running.isRunning
  (define %run-id 0)                     ; one per run-cml session
  (define %tid-count 0)
  (define %dummy-tid #f)                 ; ML dummyTid, id -1: idle loop, temp threads, root context
  (define %cur-tid #f)
  (define %atomic-state 'non-atomic)     ; non-atomic | atomic
  (define %rdy-q1 (%q))
  (define %rdy-q2 (%q))
  (define %quantum %default-quantum)
  (define %ticks %default-quantum)
  (define %base-k #f)                    ; isolation continuation: (%base-k thunk) runs thunk afresh
  (define %shutdown-k #f)                ; shutdownHook

  ; the switch in progress (see %switch!): only between the call of (k x) and the arrival of the
  ; target at its resume point, i.e. while the dynamic-wind thunks run, does it describe the switch
  ; that a dying thread was in (see %thread-died!)
  (define %switch-k #f)
  (define %switch-x #f)
  (define %switch-box #f)                ; the SyncVar or Mailbox whose value x is, handed to a reader
  (define (%switched-in!) (set! %switch-k #f) (set! %switch-x #f) (set! %switch-box #f))

  ; every continuation that a switch resumes is captured by this letcc/call, which ends the switch
  ; state once the continuation is back: the before thunks have all run by then, so a thread that
  ; dies later, of an ordinary uncaught condition, is not taken for one being switched in (whose
  ; message would go to another receiver, see %thread-died!)
  (define-syntax %letcc/call
    (syntax-rules ()
      ((_ hop body ...) (let1 (v (call-with-current-continuation (λ (hop) body ...))) (%switched-in!) v))))
  (define %random-count 0)               ; selectDoFn tie-break counter
  (define %sync-count 0)                 ; the number of the current sync, never reset

  ; the cvars are needed by the tids ---------------------------------------------------------------

  ; RepTypes.cvar: state is a list of waiting entries #(trans cleanup k) (CVAR_unset, newest first)
  ; or a fixnum (CVAR_set n, n being the polling priority); budget as for queues (%clean-and-enqueue!)
  (define-record %cvar state budget)

  (define (%new-cvar) (make-%cvar '() 0))

  ; a thread is dead once its dead cvar (the one of join-evt) is set
  (define (%tid-dead? t) (number? (%cvar-state (%tid-dead t))))

  (define (%new-dummy-tid) (make-%tid -1 #f (λ (e) (void)) '() (%new-cvar) %run-id '() #f #f '() '()))

  (set! %dummy-tid (%new-dummy-tid))
  (set! %cur-tid %dummy-tid)

  ; Thread.newTId
  (define (%new-tid)
    (let1 (n %tid-count)
      (set! %tid-count (add1 n))
      (make-%tid n #f (default-exn-handler) '() (%new-cvar) %run-id '() #f #f '() '())))

  ; Scheduler primitives ---------------------------------------------------------------------------

  (define (current-tid) %cur-tid)        ; CML.getTid

  (define (%check-running who)
    (unless %running
      (set! %atomic-state 'non-atomic)
      (%cml-raise 'not-running "CML is not running, use run-cml" who)))

  (define (cml-running?) %running)      ; RunCML.isRunning

  (define (%mark! t) (%tid-done-comm-set! t #t))

  ; plain enqueue on the rear of rdyQ1 (no mark)
  (define (%enqueue! item) (%q-enqueue! %rdy-q1 item))

  ; Scheduler.enqueueThread = markAndEnqueue
  (define (%enqueue-thread! tid k) (%mark! tid) (%enqueue! (cons tid k)))

  ; Scheduler.enqueueAndSwitchCurThread
  (define (%enqueue-and-switch-cur-thread! resume tid)
    (%enqueue-thread! %cur-tid resume)
    (set! %cur-tid tid))

  (define (%promote!) (let1 (x (%q-next! %rdy-q2)) (when x (%enqueue! x))))

  ; the SIGALRM handler's preempt, except that a thread is promoted from rdyQ2 at every preemption,
  ; also when the preempted thread is demoted: ticks are deterministic, so a loop could otherwise
  ; always be preempted at the same point, in an unmarked thread (a child it just spawned, say),
  ; and never promote anybody while it keeps rdyQ1 busy (ML's asynchronous SIGALRM does not
  ; phase-lock like that)
  (define (%preempt! k)
    (let1 (cur %cur-tid)
      (%promote!)
      (if (%tid-done-comm cur)
        (begin (%tid-done-comm-set! cur #f) (%enqueue! (cons cur k)))
        (%q-enqueue! %rdy-q2 (cons cur k)))))

  (define (%dequeue1!) (if (%q-empty? %rdy-q1) (%dequeue2!) (%q-dequeue! %rdy-q1)))
  (define (%dequeue2!) (if (%q-empty? %rdy-q2) (cons %dummy-tid %pause-item) (%q-dequeue! %rdy-q2)))

  ; hooks run in the isolated context, never on top of a thread's dynamic context.  The scheduler
  ; hook runs as the dummy thread, the current one having been queued: leaving the current thread
  ; for the scheduler is a thread switch like any other, so its dynamic-wind after thunks run with
  ; %cur-tid no longer that thread (a preemption is not mistaken for an escape, see %force-group)
  (define (%isolated thunk) (λ ignored (%base-k thunk)))
  (define (%dispatch-scheduler-hook) (set! %cur-tid %dummy-tid) (%switch! %base-k %poll-k))
  (define %pause-item (λ ignored (%base-k %pause-k)))

  ; Scheduler.atomicBegin: regions do not nest
  (define (%atomic-begin) (set! %atomic-state 'atomic))

  ; Scheduler.atomicEnd, plus the preemption tick
  (define (%atomic-end)
    (when %commit-hook (%run-commit-hook!))
    (set! %atomic-state 'non-atomic)
    (set! %ticks (sub1 %ticks))
    (when (<= %ticks 0)
      (set! %ticks %quantum)
      (when %running (%letcc/call k (%preempt! k) (%dispatch-scheduler-hook))))
    (void))

  ; every switch to another thread (or to the isolated context on behalf of another thread) calls
  ; (k x) through here, once %cur-tid is the new thread: when a dynamic-wind after thunk of the thread
  ; being left raises, that thread's handler dies of it and resumes the switch (see %thread-died!).
  ; The switch state (%switch-k, %switch-x, %switch-box, see %letcc/call) lasts until the target
  ; has been resumed.  %switch-count counts them: no thread ran while it stays the same (see
  ; %close-sweep!)
  (define %switch-count 0)
  (define (%switch! k x)
    (when %commit-hook (%run-commit-hook!))
    (set! %switch-count (add1 %switch-count))
    (set! %switch-k k) (set! %switch-x x) (set! %switch-box #f) (k x))
  (define (%box-switch! b k x)
    (when %commit-hook (%run-commit-hook!))
    (set! %switch-count (add1 %switch-count))
    (set! %switch-k k) (set! %switch-x x) (set! %switch-box b) (k x))

  ; set by a sync committing on an enabled event that has nacks to set: it runs as soon as the
  ; chosen do-thunk has done its commit, i.e. when that do-thunk leaves the atomic region (by
  ; %atomic-end or a switch), before control goes back to the syncing thread or to anybody else
  (define %commit-hook #f)
  (define (%run-commit-hook!) (let1 (h %commit-hook) (set! %commit-hook #f) (h)))

  ; Scheduler.atomicDispatch: run the next ready thread, never returns; a dead thread is skipped
  (define (%atomic-dispatch)
    (%check-running 'atomic-dispatch)
    (let loop ()
      (let1 (item (%dequeue1!))
        (if (%tid-dead? (car item))
          (loop)
          (begin
            (set! %cur-tid (car item))
            (set! %atomic-state 'non-atomic)
            (%switch! (cdr item) (void)))))))

  ; Scheduler.dispatch
  (define (%dispatch) (%atomic-begin) (%atomic-dispatch))

  ; Scheduler.atomicSwitchTo: the current thread goes to the rear of rdyQ1, tid resumes k with x.
  ; A dead tid is never resumed: the current thread just leaves the atomic region and goes on (a
  ; receiver whose after thunk killed it while it was switching to its sender, say: the message is
  ; lost with it, as if it had died right after the rendezvous).  The switch is a clock tick, as
  ; %atomic-end: two threads trading messages over channels hand off to each other through here
  ; without ever leaving the atomic region otherwise, and would keep timeouts, descriptors,
  ; processes and rdyQ2 from being served for good (ML's SIGALRM preempts them).  When the quantum
  ; expires the switch becomes a preemption: tid is queued to resume k with x, after the thread
  ; promoted from rdyQ2, the current thread behind it (unmarked, as a marked preempted thread), and
  ; the scheduler hook polls before dispatching
  (define (%atomic-switch-to tid k x)
    (if (%tid-dead? tid)
      (%atomic-end)
      (%letcc/call cur-k
        (cond
          ((begin (set! %ticks (sub1 %ticks)) (and (<= %ticks 0) %running))
            (set! %ticks %quantum)
            (%promote!)
            (%enqueue! (cons tid (λ ignored (%switch! k x))))
            (%tid-done-comm-set! %cur-tid #f)
            (%enqueue! (cons %cur-tid cur-k))
            (%dispatch-scheduler-hook))
          (else (%enqueue-and-switch-cur-thread! cur-k tid) (set! %atomic-state 'non-atomic)))
        (%switch! k x))))

  ; Scheduler.atomicYield; a yield is a clock tick too, so that a thread looping on cml/yield does not
  ; keep timeouts, descriptors and processes from being polled, nor threads in rdyQ2 from running:
  ; when the quantum expires the yielding (marked) thread is preempted as ML's SIGALRM would do it,
  ; i.e. unmarked, one thread is promoted and it goes to the rear of rdyQ1
  (define (%atomic-yield k)
    (set! %ticks (sub1 %ticks))
    (if (and (<= %ticks 0) %running)
      (begin
        (set! %ticks %quantum)
        (%tid-done-comm-set! %cur-tid #f)
        (%promote!)
        (%enqueue! (cons %cur-tid k))
        (%dispatch-scheduler-hook))
      (begin (%enqueue-thread! %cur-tid k) (%atomic-dispatch))))

  ; a yield that is a preemption as well, whatever the ticks left: the scheduler hook polls the
  ; timeouts, descriptors and processes before the next thread runs.  For a thread that has just
  ; done much work without a CML operation, and would otherwise delay them for many such rounds
  ; (see %port-reader)
  (define (%yield/poll!)
    (%check-running 'yield)
    (set! %ticks 1)
    (%letcc/call k (%atomic-begin) (%atomic-yield k))
    (void))

  ; Scheduler.enqueueTmpThread: thunk runs next, as the dummy thread, exceptions ignored
  (define (%enqueue-tmp-thread! thunk)
    (%q-enqueue-front! %rdy-q1 (cons %dummy-tid (%isolated (τ (condition-case (thunk) (e () (void))) (%dispatch))))))

  ; the scheduler hook (pollK) and the pause hook (pauseK) of export-fn-fn.sml.  The preemption
  ; ticks come every `quantum` CML operations, far more often than ML's 20 ms SIGALRM, and polling
  ; descriptors and child processes costs a select over every waiter's descriptor and a waitpid per
  ; child: at a tick they are polled only once %poll-interval-ms, or ten times as long as their
  ; previous poll took (a select over thousands of descriptors takes milliseconds), have passed
  ; since that poll, so that idle waiters (a server's thousands of idle connections) do not slow
  ; down every other thread.  Timeouts (a heap) and the extra os pollers are polled at every tick,
  ; and everything is polled at every round of the idle loop
  (define %poll-interval-ms 2)
  (define %poll-next-ms 0)                ; when descriptors and processes are due at a tick

  (define (%poll-io+procs!)
    (let1 (t0 (%now-ms))
      (%poll-io!)
      (%poll-procs!)
      (let1 (t1 (%now-ms)) (set! %poll-next-ms (+ t1 (max %poll-interval-ms (* 10 (- t1 t0))))))))

  (define (%poll-os/tick!)
    (%poll-time!)
    (let1 (now (%now-ms))
      (when (or (>= now %poll-next-ms) (< (+ now 60000) %poll-next-ms)) (%poll-io+procs!)))
    (%poll-os-pollers!))

  (define (%poll-k) (%atomic-begin) (%poll-os/tick!) (%atomic-dispatch))

  (define (%pause-k)
    (%atomic-begin)
    (let loop ()
      (%poll-os!)
      (cond
        ((not (%q-empty? %rdy-q1)) (%atomic-dispatch))
        ((%os-pause!) (loop))
        (else (set! %atomic-state 'non-atomic) (%shutdown-k (cons #t 'failure))))))

  ; clock ------------------------------------------------------------------------------------------

  (define %epoch-ms (- (* 1000 (current-seconds)) (current-process-milliseconds)))

  (define (%now-ms) (+ %epoch-ms (inexact->exact (round (current-process-milliseconds)))))

  ; the absolute clock, in seconds, used by at-time-evt
  (define (cml/now) (/ (%now-ms) 1000.0))

  (define (%secs->ms secs) (inexact->exact (ceiling (* secs 1000))))

  ; events: representation (rep-types.sml, event.sml) ----------------------------------------------

  (define-record %enabled prio do)       ; ENABLED{prio, doFn}

  ; BLOCKED blockFn, called as (block trans cleanup next).  Unlike ML the wrappers (wrap,
  ; wrap-handler) are not composed into the block-fn but kept apart as post, a procedure applied
  ; to a thunk that gives the block-fn's outcome: a blocked sync applies it at its own
  ; continuation, once out of the nested block-fns of its branches (see %sync-on-bevts)
  (define-record %blocked* block post)

  (define (%post-identity th) (th))
  (define (make-%blocked block) (make-%blocked* block %post-identity))
  (define (%blocked? x) (%blocked*? x))
  (define (%blocked-block b) (%blocked*-block b))
  (define (%blocked-post b) (%blocked*-post b))

  ; tag: base (payload: list of poll thunks), choose (list of events), guard (thunk), nack (procedure)
  (define-record %event tag payload)

  (set-record-printer! %event
    (λ (e port)
      (display "#<event " port)
      (display (%event-tag e) port)
      (when (memq (%event-tag e) '(base choose)) (display "/" port) (display (length (%event-payload e)) port))
      (display ">" port)))

  (define (event? x) (%event? x))

  (define (%base-evt poll) (make-%event 'base (list poll)))

  ; cvars (event.sml atomicCVarSet / cvarGetEvt) ---------------------------------------------------

  ; must be called while atomic; waiters wake FIFO and their cleanUp runs here, in the setter
  (define (%atomic-cvar-set! cv)
    (let1 (st (%cvar-state cv))
      (when (number? st) (error "cml: cvar already set"))
      (%cvar-state-set! cv 1)
      (let1 (added (let add ((ws st))
                     (cond
                       ((null? ws) '())
                       ((not (%trans-live? (vector-ref (car ws) 0))) (add (cdr ws)))
                       (else (let* ((w (car ws))
                                    (tid (%get-id-from-trans! (vector-ref w 0))))
                               ((vector-ref w 1))
                               (let1 (item (cons tid (vector-ref w 2))) (cons item (add (cdr ws)))))))))
        (%queue-rear-set! %rdy-q1 (append added (%queue-rear %rdy-q1))))))

  ; the block-fn is made where no old state of the cvar is in scope: the interpreter's closures keep
  ; their whole environment, so a closure over an old waiting list would keep every continuation
  ; of a select loop alive through its waiters (the same holds for the mailboxes)
  (define (%cvar-block cv)
    (make-%blocked
      (λ (trans cleanup next)
        (%letcc/call k       ; dropping the waiters of syncs that went another way (ML keeps them),
          (let1 (b (%cvar-budget cv))         ; amortized as in %clean-and-enqueue!
            (if (positive? b)
              (begin (%cvar-budget-set! cv (sub1 b)) (%cvar-state-set! cv (cons (vector trans cleanup k) (%cvar-state cv))))
              (let1 (live (filter (λ (w) (%trans-live? (vector-ref w 0))) (%cvar-state cv)))
                (%cvar-budget-set! cv (max %clean-budget-min (length live)))
                (%cvar-state-set! cv (cons (vector trans cleanup k) live)))))
          (next)
          (%impossible 'cvar-get-evt))
        (void))))

  (define (%cvar-get-evt cv)
    (%base-evt
      (τ (if (number? (%cvar-state cv))
           (let1 (st (%cvar-state cv))
             (%cvar-state-set! cv (add1 st))
             (make-%enabled st (τ (%cvar-state-set! cv 1) (%atomic-end))))
           (%cvar-block cv)))))

  ; event combinators (CML EVENT) ------------------------------------------------------------------

  ; CML.never
  (define never-evt (make-%event 'base '()))

  ; CML.alwaysEvt
  (define (always-evt v) (%base-evt (τ (make-%enabled -1 (τ (%atomic-end) v)))))

  ; CML.guard
  (define (guard thunk) (make-%event 'guard thunk))

  ; CML.withNack: f receives the nack event
  (define (with-nack f) (make-%event 'nack f))

  (define (%map-event evt fpoll)
    (let recur ((evt evt))
      (let1 (p (%event-payload evt))
        (case (%event-tag evt)
          ((base) (make-%event 'base (map fpoll p)))
          ((choose) (make-%event 'choose (map recur p)))
          ((guard) (make-%event 'guard (τ (recur (p)))))
          ((nack) (make-%event 'nack (λ (n) (recur (p n)))))
          (else (%impossible 'map-event))))))

  ; CML.wrap: f runs after the commit, outside the atomic region
  (define (wrap evt f)
    (%map-event evt (λ (poll)
                      (τ (let1 (s (poll))
                           (if (%enabled? s)
                             (let1 (d (%enabled-do s)) (make-%enabled (%enabled-prio s) (τ (f (d)))))
                             (let1 (p (%blocked-post s))
                               (make-%blocked* (%blocked-block s) (λ (th) (f (p th)))))))))))

  ; CML.wrapHandler: h takes the condition and returns the event's value (event-sig.sml, not mldoc);
  ; h also covers the wrap functions inside it, which therefore do not run in tail position: a loop
  ; that recurses from a wrap under a wrap-handler nests one handler per iteration (as in ML).  Each
  ; handler is a dynamic-wind frame (condition-case), and a thread's frames are unwound and rewound
  ; at every switch out of and into it, so such a loop also costs O(depth) per switch, i.e. O(n^2)
  ; time over n iterations (ML's handlers cost nothing at a switch): recurse outside the wrap-handler
  (define (wrap-handler evt h)
    (define (protect thunk) (condition-case (thunk) (e () (h e))))
    (%map-event evt (λ (poll)
                      (τ (let1 (s (poll))
                           (if (%enabled? s)
                             (let1 (d (%enabled-do s)) (make-%enabled (%enabled-prio s) (τ (protect d))))
                             (let1 (p (%blocked-post s))
                               (make-%blocked* (%blocked-block s) (λ (th) (protect (τ (p th))))))))))))

  ; CML.choose (gatherBEvts / gather)
  (define (choose* evts)
    (define (base? e) (eq? 'base (%event-tag e)))
    (define (gather el evts)
      (cond
        ((null? el) (if (and (pair? evts) (null? (cdr evts))) (car evts) (make-%event 'choose evts)))
        ((eq? 'choose (%event-tag (car el))) (gather (cdr el) (append (%event-payload (car el)) evts)))
        ((and (base? (car el)) (pair? evts) (base? (car evts)))
          (gather (cdr el) (cons (make-%event 'base (append (%event-payload (car el)) (%event-payload (car evts))))
                                 (cdr evts))))
        (else (gather (cdr el) (cons (car el) evts)))))
    (let gather-b ((el (reverse evts)) (l '()))
      (cond
        ((null? el) (make-%event 'base l))
        ((base? (car el)) (gather-b (cdr el) (append (%event-payload (car el)) l)))
        ((null? l) (gather el '()))
        (else (gather el (list (make-%event 'base l)))))))

  (define (choose . evts) (choose* evts))

  ; forcing (event.sml force / force' / forceBL / forceL) ------------------------------------------

  ; groups: (base . polls) | (grp . groups) | (nack cvar group); nacks is a (nacks . cvars) cell
  ; collecting the nack cvars made so far
  (define (%force* evt nacks)
    (let1 (p (%event-payload evt))
      (case (%event-tag evt)
        ((guard) (let1 (e (p)) (unless (event? e) (error "cml: guard did not return an event" e)) (%force* e nacks)))
        ((nack) (let1 (cv (%new-cvar))
                  (set-cdr! nacks (cons cv (cdr nacks)))
                  (let1 (e (p (%cvar-get-evt cv)))
                    (unless (event? e) (error "cml: with-nack did not return an event" e))
                    (list 'nack cv (%force* e nacks)))))
        ((base) (cons 'base p))
        ((choose) (%force-bl p '() nacks))
        (else (%impossible 'force)))))

  (define (%force-bl evts bevs nacks)
    (if (null? evts)
      (cons 'base bevs)
      (let1 (g (%force* (car evts) nacks))
        (case (car g)
          ((base) (%force-bl (cdr evts) (append (cdr g) bevs) nacks))
          ((grp) (%force-l (cdr evts) (append (cdr g) (list (cons 'base bevs))) nacks))
          (else (%force-l (cdr evts) (list g (cons 'base bevs)) nacks))))))

  (define (%force-l evts l nacks)
    (if (null? evts)
      (if (null? (cdr l)) (car l) (cons 'grp l))
      (let1 (g (%force* (car evts) nacks))
        (cond
          ((and (eq? 'base (car g)) (eq? 'base (caar l)))
            (%force-l (cdr evts) (cons (cons 'base (append (cdr g) (cdar l))) (cdr l)) nacks))
          ((eq? 'grp (car g)) (%force-l (cdr evts) (append (cdr g) l) nacks))
          (else (%force-l (cdr evts) (cons g l) nacks))))))

  ; (force nacks) forces the events of a sync; when a guard or with-nack body leaves it for good
  ; (it raises and the handler escapes, it escapes with a continuation, or its thread dies, e.g. by
  ; cml/exit) the sync is abandoned, as in ML, but, unlike ML, the nacks made so far are set, so
  ; that the servers behind them (a port input event's reader holding the port lock, say) are not
  ; left waiting forever.  A thread switch out of the forcing (a guard that blocks, a preemption,
  ; a handler of a continuable condition that syncs) is not an exit: then %cur-tid is already
  ; another thread when the after thunk runs, and nothing fires.  The cells of the forcings in
  ; progress are also kept on the thread (%tid-forcing) and its death sets them (%mark-dead!): a
  ; thread killed while switched out of a forcing (an after thunk that raised as a guard blocked)
  ; has that frame unwound by its handler's escape, as another thread, before it is marked dead
  (define (%fire-nacks! nacks)
    (let ((cvs (cdr nacks)) (state %atomic-state))
      (set-cdr! nacks '())
      (unless (null? cvs)
        (%atomic-begin)
        (for-each (λ (cv) (unless (number? (%cvar-state cv)) (%atomic-cvar-set! cv))) cvs)
        (set! %atomic-state state))))

  (define (%force-group force)
    (let ((nacks (list 'nacks)) (me %cur-tid) (done? #f))
      (define (leave!)
        (let1 (l (%tid-forcing me))
          (%tid-forcing-set! me (if (and (pair? l) (eq? nacks (car l))) (cdr l) (remove (λ (c) (eq? c nacks)) l)))))
      (dynamic-wind
        void
        (τ (%tid-forcing-set! me (cons nacks (%tid-forcing me)))
           (let1 (g (force nacks)) (set! done? #t) (leave!) g))
        (τ (unless done? (when (or (eq? me %cur-tid) (%tid-dead? me)) (leave!) (%fire-nacks! nacks)))))))

  ; selection (event.sml selectDoFn) ---------------------------------------------------------------

  (define (%random i)
    (let1 (j %random-count)
      (set! %random-count (if (= j 1000000) 0 (add1 j)))
      (remainder j i)))

  ; l is a list of (prio . x), n the number of enabled events; -1 counts as n, max starts at 0.
  ; Not in ML: -2 (%last-resort-prio, only used by sync/timeout) is below every other priority, so
  ; it is chosen only when nothing else is enabled
  (define %last-resort-prio -2)

  (define (%select-do-fn l n)
    (if (null? (cdr l))
      (cdar l)
      (let loop ((l l) (max-p -1) (k 0) (xs '()))       ; -1 then 0 as ML's start: p >= 0 wins over -1
        (if (null? l)
          (if (and (pair? xs) (null? (cdr xs))) (car xs) (list-ref xs (%random k)))
          (let1 (p (let1 (p (caar l)) (cond ((= p -1) n) ((= p %last-resort-prio) -1) (else p))))
            (cond
              ((> p max-p) (loop (cdr l) p 1 (list (cdar l))))
              ((= p max-p) (loop (cdr l) max-p (add1 k) (cons (cdar l) xs)))
              (else (loop (cdr l) max-p k xs))))))))

  ; synchronization (event.sml syncOnOneEvt / syncOnBEvts / collect / syncOnGrp) --------------------

  (define (%sync-on-one poll)
    (%atomic-begin)
    (let1 (s (poll))
      (if (%enabled? s)
        ((%enabled-do s))
        (let1 (t (%mk-id))
          ((%blocked-post s) (τ ((%blocked-block s) t (τ (%trans-cancel! t)) %atomic-dispatch)))))))

  ; a blocked sync calls each block-fn inside the dynamic extent of the previous one (event.sml's
  ; `log`); the one resumed by the commit only finishes its own (library) work there: its outcome,
  ; values or condition, is captured and handed as a thunk to the sync's continuation, where the
  ; branch's post (its wrap functions and wrap-handler handlers) is applied to it.  So a wrap
  ; function runs in the dynamic context of the sync itself, not nested in the other branches'
  ; block-fns: a server loop that recurses from a wrap does not grow, a branch's wrap-handler never
  ; sees what another branch raised (ML has that bug) and a continuable condition (`signal`,
  ; raise-continuable) raised by a wrap function keeps its meaning.  A condition raised before the
  ; commit, while blocking, cancels the transaction and leaves the atomic region before it goes on
  (define (%outcome t thunk)
    (condition-case (receive vals (thunk) (τ (apply values vals)))
      (e () (%trans-cancel! t) (set! %atomic-state 'non-atomic) (τ (abort e)))))

  (define (%sync-on-bevts polls)
    (define (ext polls blocked)
      (if (null? polls)
        ((%letcc/call k
           (let1 (t (%mk-id))
             (let1 (set-flg (τ (%trans-cancel! t)))
               (let log ((bs blocked))
                 (if (null? bs)
                   (%atomic-dispatch)
                   (let1 (b (car bs))
                     (k (let1 (th (%outcome t (τ ((%blocked-block b) t set-flg (τ (log (cdr bs)))))))
                          (τ ((%blocked-post b) th)))))))))))
        (let1 (s ((car polls)))
          (if (%enabled? s)
            (ext-rdy (cdr polls) (list (cons (%enabled-prio s) (%enabled-do s))) 1)
            (ext (cdr polls) (cons s blocked))))))
    (define (ext-rdy polls do-fns n)
      (if (null? polls)
        ((%select-do-fn do-fns n))
        (let1 (s ((car polls)))
          (if (%enabled? s)
            (ext-rdy (cdr polls) (cons (cons (%enabled-prio s) (%enabled-do s)) do-fns) (add1 n))
            (ext-rdy (cdr polls) do-fns n)))))
    (cond
      ((null? polls) (%dispatch))
      ((null? (cdr polls)) (%sync-on-one (car polls)))
      (else (%atomic-begin) (ext polls '()))))

  (define (%flag) (vector #f))
  (define (%flag-set? f) (vector-ref f 0))
  (define (%flag-set! f) (vector-set! f 0 #t))

  ; returns (values bl flg-sets): bl a list of (poll . flag), flg-sets a list of (cvar . flags)
  (define (%collect grp)
    (define (gather-wrapped grp bl flg-sets)
      (define (gather grp bl all-flgs flg-sets)
        (case (car grp)
          ((base) (let loop ((bevs (cdr grp)) (bl bl) (all all-flgs))
                    (if (null? bevs)
                      (values bl all flg-sets)
                      (let1 (f (%flag)) (loop (cdr bevs) (cons (cons (car bevs) f) bl) (cons f all))))))
          ((grp) (let loop ((gs (cdr grp)) (bl bl) (all all-flgs) (fs flg-sets))
                   (if (null? gs)
                     (values bl all fs)
                     (receive (bl all fs) (gather (car gs) bl all fs) (loop (cdr gs) bl all fs)))))
          ((nack) (receive (bl2 all2 fs2) (gather (caddr grp) bl '() flg-sets)
                    (values bl2 (append all2 all-flgs) (cons (cons (cadr grp) all2) fs2))))
          (else (%impossible 'collect))))
      (receive (bl all fs) (gather grp bl '() flg-sets) (values bl fs)))
    (if (eq? 'grp (car grp))
      (let1 (unwrapped (%flag))
        (let gather ((grp grp) (bl '()) (fs '()))
          (case (car grp)
            ((base) (values (let loop ((bevs (cdr grp)) (bl bl))
                              (if (null? bevs) bl (loop (cdr bevs) (cons (cons (car bevs) unwrapped) bl))))
                            fs))
            ((grp) (let loop ((gs (cdr grp)) (bl bl) (fs fs))
                     (if (null? gs) (values bl fs) (receive (bl fs) (gather (car gs) bl fs) (loop (cdr gs) bl fs)))))
            (else (gather-wrapped grp bl fs)))))
      (gather-wrapped grp '() '())))

  ; a poll that raises (a bad argument, e.g. recv-evt on a non-channel) abandons the sync: the nacks
  ; are set first, as when forcing is abandoned (see %force-group)
  (define (%sync-on-grp grp)
    (receive (bl flg-sets) (%collect grp)
      ; a nack may be set already: forcing that escaped (see %force-group) set it, and the escaped
      ; continuation was re-entered (a generator, backtracking), finishing the forcing with it.
      ; chk-cvars runs in whoever wakes a blocked sync, so it must never raise there
      (define (chk-cvars)
        (for-each (λ (fs) (unless (or (any %flag-set? (cdr fs)) (number? (%cvar-state (car fs)))) (%atomic-cvar-set! (car fs))))
                  flg-sets))
      (define (poll p)
        (handle-exceptions e
          (begin
            (for-each (λ (fs) (unless (number? (%cvar-state (car fs))) (%atomic-cvar-set! (car fs)))) flg-sets)
            (set! %atomic-state 'non-atomic)
            (abort e))
          (p)))
      ; while blocked, the flag sets are kept on the thread (%tid-nacks), until the chosen branch's
      ; cleanup or the sync's own continuation runs: a thread that dies meanwhile, as it is
      ; switched in by the partner that committed (a before thunk that raises), sets the nacks
      ; itself (see %abandon-blocked!)
      (define (ext bl blocked)
        (if (null? bl)
          (let* ((me %cur-tid)
                 (th (%letcc/call k
                       (let1 (t (%mk-id))
                         (%tid-nacks-set! me flg-sets)
                         (let log ((bs blocked))
                           (if (null? bs)
                             (%atomic-dispatch)
                             (let ((b (caar bs)) (flg (cdar bs)))
                               (k (let1 (th (%outcome t (τ ((%blocked-block b) t (τ (%tid-nacks-set! me '())
                                                                                       (%trans-cancel! t) (%flag-set! flg) (chk-cvars))
                                                                              (τ (log (cdr bs)))))))
                                    (τ ((%blocked-post b) th)))))))))))
            (%tid-nacks-set! me '())
            (th))
          (let1 (s (poll (caar bl)))
            (if (%enabled? s)
              (ext-rdy (cdr bl) (list (cons (%enabled-prio s) (cons (%enabled-do s) (cdar bl)))) 1)
              (ext (cdr bl) (cons (cons s (cdar bl)) blocked))))))
      ; the nacks are set once the chosen do-thunk has committed, as it leaves the atomic region
      ; (%commit-hook), still before sync returns: event.sml sets them before it runs, so a nack
      ; could cancel the very partner the do-thunk is about to dequeue
      (define (ext-rdy bl do-fns n)
        (if (null? bl)
          (let1 (x (%select-do-fn do-fns n))
            (%flag-set! (cdr x))
            (unless (null? flg-sets) (set! %commit-hook chk-cvars))
            ((car x)))
          (let1 (s (poll (caar bl)))
            (if (%enabled? s)
              (ext-rdy (cdr bl) (cons (cons (%enabled-prio s) (cons (%enabled-do s) (cdar bl))) do-fns) (add1 n))
              (ext-rdy (cdr bl) do-fns n)))))
      (%atomic-begin)
      (ext bl '())))

  ; every sync is numbered once its events are forced, before any poll (see barrier-wait-evt)
  (define (%sync-group g)
    (set! %sync-count (add1 %sync-count))
    (if (eq? 'base (car g)) (%sync-on-bevts (cdr g)) (%sync-on-grp g)))

  ; CML.sync
  (define (sync evt)
    (%check-running 'sync)
    (unless (event? evt) (error "sync: not an event" evt))
    (%sync-group (if (eq? 'base (%event-tag evt))
                   (cons 'base (%event-payload evt))
                   (%force-group (λ (nacks) (%force* evt nacks))))))

  ; CML.select, taking a list
  (define (select* evts)
    (%check-running 'select)
    (%sync-group (%force-group (λ (nacks) (%force-bl evts '() nacks)))))

  ; CML.select
  (define (select . evts) (select* evts))

  ; (select/case (evt formals body ...) (evt => proc) ...), an `()` formals ignores the value
  (define-syntax %select/case-clause
    (syntax-rules (=>)
      ((_ (evt => proc)) (wrap evt proc))
      ((_ (evt () body ...)) (wrap evt (λ ignored body ...)))
      ((_ (evt formals body ...)) (wrap evt (λ formals body ...)))))

  (define-syntax select/case
    (syntax-rules ()
      ((_ clause ...) (select (%select/case-clause clause) ...))))

  ; threads (thread.sml) ---------------------------------------------------------------------------

  ; Thread.defaultExnHandler, copied into each thread at spawn time
  (define default-exn-handler
    (make-parameter
      (λ (e)
        (print-error-message e (current-error-port)
                             (string-append "cml: thread " (tid->string %cur-tid) " died of an uncaught exception")))))

  (define (%do-handler tid e) (condition-case ((%tid-exn-handler tid) e) (ignored () (void))))

  ; the death of tid fires its join-evt, once: a thread never dies twice,
  ; and sets the nacks of the syncs it was still forcing (see %force-group)
  (define (%mark-dead! tid)
    (unless (%tid-dead? tid)
      (%release-holds! tid)
      (let1 (cells (%tid-forcing tid))
        (%tid-forcing-set! tid '())
        (for-each %fire-nacks! cells))
      (%atomic-cvar-set! (%tid-dead tid))))

  ; the internal locks that tid holds (a port's write lock, see %port-write-chunks!) are put back
  ; when it dies holding them: it may die as it is switched in or out in the middle of the work
  ; (a dynamic-wind thunk that raises), outside the frame that would release them.  Each put runs
  ; in a temporary thread, as it may switch to a waiter; called atomic
  (define (%release-holds! tid)
    (let1 (l (%tid-holds tid))
      (%tid-holds-set! tid '())
      (for-each (λ (mv) (%enqueue-tmp-thread! (τ (mvar-put! mv (void))))) l)))

  ; take mv, as mvar-take!, recording it among the current thread's holds before leaving the
  ; atomic region (a preemption there switches the thread out, holding it)
  (define (%hold-take! mv)
    (%check-running 'mvar-take!)
    (%atomic-begin)
    (when (eq? %empty (%cell-value mv)) (%cell-block! mv))
    (%cell-value-set! mv %empty)
    (%tid-holds-set! %cur-tid (cons mv (%tid-holds %cur-tid)))
    (%atomic-end))

  ; mvar-take-poll as %hold-take!: #t when mv was taken
  (define (%hold-take-poll! mv)
    (%atomic-begin)
    (if (eq? %empty (%cell-value mv))
      (begin (%atomic-end) #f)
      (begin
        (%cell-value-set! mv %empty)
        (%tid-holds-set! %cur-tid (cons mv (%tid-holds %cur-tid)))
        (%atomic-end)
        #t)))

  ; give back mv, taken by %hold-take! or %hold-take-poll!
  (define (%hold-put! mv)
    (%tid-holds-set! %cur-tid (remove (λ (x) (eq? x mv)) (%tid-holds %cur-tid)))
    (mvar-put! mv (void)))

  (define (%notify-and-dispatch tid)
    (%atomic-begin)
    (%mark-dead! tid)
    (%atomic-dispatch))

  ; what a thread that dies leaves behind it, made harmless at once, before its handler runs: the
  ; transaction of the operation it was blocked in is cancelled (nobody resumes that continuation
  ; again), its entries left in the ready queues go, and the nacks of the blocked sync it waited in
  ; (if any) are set: the sync is abandoned, as when forcing is (see %force-group).  All of them,
  ; the chosen branch's included when a partner had committed to it: the dead thread never learnt
  ; which branch that was.  A running thread has entries in the ready queues only when it dies in
  ; the middle of a switch (it was queued as it was switched out, and an after thunk raised):
  ; queued? says so, and only then are the queues scanned, so that n ready threads dying of
  ; ordinary conditions one after the other cost O(n), not O(n^2)
  (define (%abandon-blocked! id queued?)
    (let ((t (%tid-trans id)) (fss (%tid-nacks id)))
      (when t (%trans-cancel! t))
      (%release-holds! id)
      (%tid-nacks-set! id '())
      (when queued?
        (for-each (λ (q) (let loop ((n (+ (length (%queue-front q)) (length (%queue-rear q)))))
                           (when (positive? n)
                             (let1 (item (%q-dequeue! q)) (unless (eq? id (car item)) (%q-enqueue! q item)))
                             (loop (sub1 n)))))
                  (list %rdy-q1 %rdy-q2)))
      (for-each (λ (fs) (unless (or (any %flag-set? (cdr fs)) (number? (%cvar-state (car fs)))) (%atomic-cvar-set! (car fs))))
                fss)))

  ; thread id dies of the uncaught condition e, its handler running as id.  Everything that others
  ; wait for is done first, so that a handler that exits its thread (cml/exit) or blocks never
  ; strands them.  When e comes from a dynamic-wind after thunk that raised while id was being
  ; switched out, %cur-tid is already the target of the switch, which was taken off its queue: the
  ; switch is queued first in rdyQ1, to go on once the handler is done (a %partner that id handed
  ; over, id being a receiver switching to its blocked sender, is voided: that sender goes on
  ; without resuming the dead receiver).  When it comes from a before thunk that raised while id
  ; was being switched in by a partner, what that partner handed over goes on to others: a blocked
  ; sender switched in by a receiver (x is the receiver's %partner, not claimed yet) leaves the
  ; receiver held by nobody else, which is woken to receive again (see recv); a reader of a SyncVar
  ; being handed its value (by a put, or relayed by another reader) leaves the value in the variable
  ; and the relay to the other readers (or the take) was its job, so the next reader is handed the
  ; value the variable holds by then (%cell-wake-next!); a receiver of a Mailbox leaves the message,
  ; which goes to the next receiver or back to the front of the mailbox (%mailbox-redeliver!).  The
  ; switch state is only there while a switch is in progress (see %letcc/call): a thread that dies
  ; of an ordinary condition, after it was resumed, finds none, and what it received is its own
  (define (%thread-died! id e)
    (let ((target %cur-tid) (k %switch-k) (x %switch-x) (box %switch-box) (state %atomic-state))
      (set! %cur-tid id)
      (%switched-in!)
      (%atomic-begin)
      (cond
        ((not k))
        ((eq? target id)
          (when (and (%partner? x) (not (%partner-claimed x)))
            (%partner-claimed-set! x #t)
            (unless (%tid-dead? (%partner-tid x))
              (%enqueue-thread! (%partner-tid x) (λ ignored ((%partner-k x) %rendezvous-failed)))))
          (cond
            ((%cell? box) (%cell-wake-next! box))
            ((%mailbox? box) (%mailbox-redeliver! box x))))
        ((and k (%tid? target) (not (%tid-dead? target)))
          (when (and (%partner? x) (eq? id (%partner-tid x))) (%partner-k-set! x #f))
          (%q-enqueue-front! %rdy-q1 (cons target (λ ignored (set! %atomic-state state) (%box-switch! box k x))))))
      (%abandon-blocked! id (or k (not (eq? target id))))
      (set! %atomic-state 'non-atomic)
      (%do-handler id e)
      (%notify-and-dispatch id)))

  ; CML.spawnc: the child runs first, the parent goes to the rear of rdyQ1.  A non-procedure raises
  ; in the caller, as every bad argument does (the child would die of it, unseen by the caller)
  (define (spawn/call f x)
    (%check-running 'spawn)
    (unless (procedure? f) (error 'spawn/call "not a procedure" f))
    (%atomic-begin)
    (let1 (id (%new-tid))
      (%letcc/call parent-k
        (%enqueue-and-switch-cur-thread! parent-k id)
        (%switch! %base-k (τ (%atomic-end)
                             (handle-exceptions e (%thread-died! id e) (f x))
                             (%notify-and-dispatch id))))
      id))

  ; CML.spawn
  (define (spawn thunk)
    (%check-running 'spawn)
    (unless (procedure? thunk) (error 'spawn "not a procedure" thunk))
    (spawn/call (λ (ignored) (thunk)) (void)))

  ; CML.exit
  (define (cml/exit)
    (%check-running 'exit)
    (let1 (t %cur-tid)
      (%tid-props-set! t '())
      (%notify-and-dispatch t)))

  ; CML.joinEvt
  (define (join-evt tid) (%cvar-get-evt (%tid-dead tid)))

  ; CML.yield
  (define (cml/yield)
    (%check-running 'yield)
    (%letcc/call k (%atomic-begin) (%atomic-yield k))
    (void))

  ; CML.newThreadProp: (values clear! get peek set!), peek returns '() or (list v)
  (define (make-thread-property init)
    (let1 (key (list 'thread-property))
      (define (props) (%tid-props %cur-tid))
      (define (props! l) (%tid-props-set! %cur-tid l))
      (define (delete l) (cond ((null? l) l) ((eq? key (caar l)) (cdr l)) (else (cons (car l) (delete (cdr l))))))
      (define (peek) (let1 (p (assq key (props))) (if p (list (cdr p)) '())))
      (define (get) (let1 (p (assq key (props))) (if p (cdr p) (let1 (b (init)) (props! (cons (cons key b) (props))) b))))
      (define (clear!) (props! (delete (props))))
      (define (put! x) (props! (cons (cons key x) (delete (props)))))
      (values clear! get peek put!)))

  ; CML.newThreadFlag: (values get set!)
  (define (make-thread-flag)
    (receive (clear! get peek put!) (make-thread-property (τ #f))
      (values (τ (pair? (peek))) (λ (b) (if b (unless (pair? (peek)) (put! #t)) (clear!))))))

  ; channels (channel.sml) -------------------------------------------------------------------------

  (define-record %channel priority in-q out-q)

  (set-record-printer! %channel (λ (c port) (display "#<channel>" port)))

  (define (make-channel) (make-%channel 1 (%q) (%q)))             ; CML.channel
  (define (channel? x) (%channel? x))
  (define (channel=? a b) (eq? a b))                              ; CML.sameChannel

  (define (%channel-reset! ch)                                    ; Channel.resetChan
    (%channel-priority-set! ch 1)
    (%q-reset! (%channel-in-q ch))
    (%q-reset! (%channel-out-q ch)))

  ; bumpPriority: return the old value
  (define (%channel-bump! ch) (let1 (n (%channel-priority ch)) (%channel-priority-set! ch (add1 n)) n))

  ; cleanAndChk: 0 when no live partner, otherwise the (bumped) priority
  (define (%clean-and-chk! ch q)
    (let1 (f (%clean (%queue-front q)))
      (if (pair? f)
        (begin (%queue-front-set! q f) (%channel-bump! ch))
        (let1 (r (%queue-rear q))
          (%queue-front-set! q '())
          (if (null? r)
            0
            (let1 (rr (%clean-rev r '()))
              (%queue-rear-set! q '())
              (if (null? rr) 0 (begin (%queue-front-set! q rr) (%channel-bump! ch)))))))))

  (define (%set-cur-thread-from-trans! t) (set! %cur-tid (%get-id-from-trans! t)))

  ; a receiver that finds a blocked sender switches to it handing over a %partner (ML: the pair of
  ; its tid and continuation), and the sender switches back with the message.  The receiver sits in
  ; no queue meanwhile: if the sender dies before claiming it (a dynamic-wind before thunk of the
  ; sender that raised as it was switched in), %thread-died! resumes the receiver with
  ; %rendezvous-failed and the receiver receives again, as if that sender had never been there
  (define-record %partner tid k claimed)
  (define %rendezvous-failed (list 'rendezvous-failed))

  ; a partner voided by the death of its receiver (k #f, see %thread-died!) resumes nobody
  (define (%partner-switch-to! p msg)
    (%partner-claimed-set! p #t)
    (if (%partner-k p)
      (%atomic-switch-to (%partner-tid p) (%partner-k p) msg)
      (%atomic-end)))

  ; switch to the sender of item as the receiver, with k resuming the receiver
  (define (%switch-to-sender! ch item k)
    (let1 (me %cur-tid)
      (%set-cur-thread-from-trans! (car item))
      (%channel-priority-set! ch 1)
      (%switch! (cdr item) (make-%partner me k #f))))

  ; CML.send
  (define (send ch msg)
    (%check-running 'send)
    (%atomic-begin)
    (let1 (item (%clean-and-remove! (%channel-in-q ch)))
      (if item
        (%letcc/call send-k
          (%enqueue-and-switch-cur-thread! send-k (%get-id-from-trans! (car item)))
          (%channel-priority-set! ch 1)
          (%switch! (cdr item) msg))
        (let1 (r (%letcc/call send-k
                   (%clean-and-enqueue! (%channel-out-q ch) (cons (%mk-id) send-k))
                   (%atomic-dispatch)))
          (%partner-switch-to! r msg))))
    (void))

  ; CML.recv; unlike ML, it leaves the atomic region when resumed by a sender
  (define (recv ch)
    (%check-running 'recv)
    (%atomic-begin)
    (let1 (item (%clean-and-remove! (%channel-out-q ch)))
      (if item
        (let1 (v (%letcc/call recv-k (%switch-to-sender! ch item recv-k)))
          (if (eq? v %rendezvous-failed) (recv ch) v))
        (let1 (msg (%letcc/call recv-k
                     (%clean-and-enqueue! (%channel-in-q ch) (cons (%mk-id) recv-k))
                     (%atomic-dispatch)))
          (%atomic-end)
          msg))))

  ; CML.sendEvt, with the message computed by (make-msg) at the commit, while atomic
  (define (%send-evt/commit ch make-msg)
    (%base-evt
      (τ (let1 (p (%clean-and-chk! ch (%channel-in-q ch)))
           (if (zero? p)
             (make-%blocked
               (λ (trans cleanup next)
                 (let1 (r (%letcc/call send-k
                            (%clean-and-enqueue! (%channel-out-q ch) (cons trans send-k))
                            (next)
                            (%impossible 'send-evt)))
                   (cleanup)
                   (%partner-switch-to! r (make-msg)))))
             (make-%enabled p (τ (let ((msg (make-msg)) (item (%q-dequeue! (%channel-in-q ch))))
                                   (%letcc/call send-k
                                     (%enqueue-and-switch-cur-thread! send-k (%get-id-from-trans! (car item)))
                                     (%channel-priority-set! ch 1)
                                     (%switch! (cdr item) msg))
                                   (void)))))))))

  ; CML.sendEvt
  (define (send-evt ch msg) (%send-evt/commit ch (τ msg)))

  ; CML.recvEvt
  (define (recv-evt ch)
    (%base-evt
      (τ (let1 (p (%clean-and-chk! ch (%channel-out-q ch)))
           (if (zero? p)
             (make-%blocked
               (λ (trans cleanup next)
                 (let1 (msg (%letcc/call recv-k
                              (%clean-and-enqueue! (%channel-in-q ch) (cons trans recv-k))
                              (next)
                              (%impossible 'recv-evt)))
                   (cleanup)
                   (%atomic-end)
                   msg)))
             ; committed: when the sender dies in the rendezvous (see %partner), a plain recv follows
             (make-%enabled p (τ (let* ((item (%q-dequeue! (%channel-out-q ch)))
                                        (v (%letcc/call recv-k (%switch-to-sender! ch item recv-k))))
                                   (if (eq? v %rendezvous-failed) (recv ch) v)))))))))

  ; CML.sendPoll: #t iff a receiver was already blocked
  (define (send-poll ch msg)
    (%check-running 'send-poll)
    (%atomic-begin)
    (let1 (item (%clean-and-remove! (%channel-in-q ch)))
      (if item
        (begin
          (%letcc/call send-k
            (%enqueue-and-switch-cur-thread! send-k (%get-id-from-trans! (car item)))
            (%channel-priority-set! ch 1)
            (%switch! (cdr item) msg))
          #t)
        (begin (%atomic-end) #f))))

  ; CML.recvPoll: '() or (list msg)
  (define (recv-poll ch)
    (%check-running 'recv-poll)
    (%atomic-begin)
    (let1 (item (%clean-and-remove! (%channel-out-q ch)))
      (if item
        (let1 (v (%letcc/call recv-k (%switch-to-sender! ch item recv-k)))
          (if (eq? v %rendezvous-failed) (recv-poll ch) (list v)))
        (begin (%atomic-end) '()))))

  ; timeouts (timeout.sml) -------------------------------------------------------------------------

  ; a binary min-heap of the entries #(time-ms cleanup trans k seq) in %time-q[0, %time-n), ordered
  ; by time and, among equal times, newest first (ML's timeWait puts an entry before the ones due at
  ; the same time: ties wake LIFO).  The entries of syncs that went another way are dropped when
  ; they reach the top, and by a full purge once the insertions since the previous one reach the
  ; number of entries it kept (as %clean-and-enqueue! does), so they never pile up.  ML keeps a
  ; sorted list, inserts by a linear walk and cleans all of it at every poll: that is every 20 ms
  ; there, but every `quantum` CML operations here, so n sleepers would make every operation O(n)
  ; and blocking them O(n^2); here blocking is O(log n) and a poll stops at the first live entry
  ; that is not due
  (define %time-q (make-vector 16 #f))
  (define %time-n 0)
  (define %time-seq 0)
  (define %time-budget 0)

  (define (%time-reset!) (set! %time-q (make-vector 16 #f)) (set! %time-n 0) (set! %time-budget 0))

  (define (%time-item-live? it) (%trans-live? (vector-ref it 2)))

  (define (%time-before? a b)
    (let ((ta (vector-ref a 0)) (tb (vector-ref b 0)))
      (or (< ta tb) (and (= ta tb) (> (vector-ref a 4) (vector-ref b 4))))))

  (define (%time-sift-up! i)
    (when (positive? i)
      (let* ((p (quotient (sub1 i) 2)) (x (vector-ref %time-q i)) (y (vector-ref %time-q p)))
        (when (%time-before? x y)
          (vector-set! %time-q i y)
          (vector-set! %time-q p x)
          (%time-sift-up! p)))))

  (define (%time-sift-down! i)
    (let* ((l (add1 (* 2 i)))
           (r (add1 l))
           (m (if (and (< l %time-n) (%time-before? (vector-ref %time-q l) (vector-ref %time-q i))) l i))
           (m (if (and (< r %time-n) (%time-before? (vector-ref %time-q r) (vector-ref %time-q m))) r m)))
      (unless (= m i)
        (let1 (x (vector-ref %time-q i))
          (vector-set! %time-q i (vector-ref %time-q m))
          (vector-set! %time-q m x)
          (%time-sift-down! m)))))

  (define (%time-pop!)
    (let1 (n (sub1 %time-n))
      (set! %time-n n)
      (vector-set! %time-q 0 (vector-ref %time-q n))
      (vector-set! %time-q n #f)
      (when (positive? n) (%time-sift-down! 0))))

  ; keep the live entries only, in a heap made anew
  (define (%time-purge!)
    (let1 (live (let loop ((i 0) (acc '()))
                  (if (= i %time-n)
                    acc
                    (loop (add1 i) (let1 (it (vector-ref %time-q i)) (if (%time-item-live? it) (cons it acc) acc))))))
      (let1 (n (length live))
        (set! %time-q (make-vector (max 16 (* 2 n)) #f))
        (let loop ((l live) (i 0)) (unless (null? l) (vector-set! %time-q i (car l)) (loop (cdr l) (add1 i))))
        (set! %time-n n)
        (let loop ((i (sub1 (quotient n 2)))) (when (>= i 0) (%time-sift-down! i) (loop (sub1 i))))
        (set! %time-budget (max %clean-budget-min n)))))

  ; timeWait
  (define (%time-wait! t cleanup trans k)
    (if (positive? %time-budget) (set! %time-budget (sub1 %time-budget)) (%time-purge!))
    (when (= %time-n (vector-length %time-q))
      (let1 (v (make-vector (* 2 %time-n) #f))
        (do ((i 0 (add1 i))) ((= i %time-n)) (vector-set! v i (vector-ref %time-q i)))
        (set! %time-q v)))
    (set! %time-seq (add1 %time-seq))
    (vector-set! %time-q %time-n (vector t cleanup trans k %time-seq))
    (set! %time-n (add1 %time-n))
    (%time-sift-up! (sub1 %time-n)))

  ; the live entry due first, the stale ones above it dropped, or #f
  (define (%time-first)
    (cond
      ((zero? %time-n) #f)
      ((%time-item-live? (vector-ref %time-q 0)) (vector-ref %time-q 0))
      (else (%time-pop!) (%time-first))))

  ; TimeOut.pollTime: wake the live entries that are due, in order
  (define (%poll-time!)
    (unless (zero? %time-n)
      (let1 (now (%now-ms))
        (let loop ()
          (let1 (it (%time-first))
            (when (and it (<= (vector-ref it 0) now))
              (%time-pop!)
              (%enqueue-thread! (%trans-tid (vector-ref it 2)) (vector-ref it 3))
              ((vector-ref it 1))
              (loop)))))))

  ; TimeOut.anyWaiting: milliseconds to the next deadline, or #f
  (define (%timeout-any-waiting)
    (let1 (it (%time-first))
      (and it (max 0 (- (vector-ref it 0) (%now-ms))))))

  (define (%timeout-block deadline-thunk)
    (make-%blocked
      (λ (trans cleanup next)
        (let1 (t (deadline-thunk))
          (%letcc/call k (%time-wait! t cleanup trans k) (next) (%impossible 'timeout-evt)))
        (%atomic-end))))

  ; CML.timeOutEvt: the delay starts when the sync blocks; 0 is immediately enabled
  (define (timeout-evt secs)
    (let1 (ms (%secs->ms secs))
      (%base-evt (τ (if (zero? secs)
                      (make-%enabled -1 %atomic-end)
                      (%timeout-block (τ (+ (%now-ms) ms))))))))

  ; CML.atTimeEvt, abs-secs on the (cml/now) clock
  (define (at-time-evt abs-secs)
    (let1 (t (%secs->ms abs-secs))
      (%base-evt (τ (if (<= t (%now-ms))
                      (make-%enabled -1 %atomic-end)
                      (%timeout-block (τ t)))))))

  ; OS.Process.sleep
  (define (cml/sleep secs) (sync (timeout-evt secs)))

  ; sync on evt, giving up after secs with default.  With secs <= 0 it is a poll: evt is chosen
  ; whenever one of its branches is enabled (an expired timeout-evt is enabled too, and would compete
  ; with them under select's priorities), default only when none is
  (define (sync/timeout evt secs #!optional (default #f))
    (if (<= secs 0)
      (select evt (%base-evt (τ (make-%enabled %last-resort-prio (τ (%atomic-end) default)))))
      (select evt (wrap (timeout-evt secs) (λ ignored default)))))

  ; IO manager (io-manager.sml) --------------------------------------------------------------------

  ; entries #(specs trans cleanup k), newest first; specs are (fd mode) lists, mode 'input or 'output
  (define %io-waiting '())

  (define (%io-item-live? it) (%trans-live? (vector-ref it 1)))

  ; select on specs: #f when none is ready, else a predicate telling the ready specs, looked up by
  ; descriptor in a table (not by member in the lists of ready ones, which made waking r of n
  ; waiters O(n*r)); never raises (as ML's poll, errors count as not ready)
  (define (%fds-select* specs timeout)
    (let ((rl (map car (filter (λ (s) (eq? 'input (cadr s))) specs)))
          (wl (map car (filter (λ (s) (eq? 'output (cadr s))) specs))))
      (condition-case
        (receive (r w) (if timeout (file-select rl wl timeout) (file-select rl wl))
          (let ((r (if (list? r) r '())) (w (if (list? w) w '())))
            (and (or (pair? r) (pair? w))
                 (let1 (v (make-vector (add1 (fold max 0 (append r w))) 0))
                   (for-each (λ (fd) (vector-set! v fd (bitwise-ior 1 (vector-ref v fd)))) r)
                   (for-each (λ (fd) (vector-set! v fd (bitwise-ior 2 (vector-ref v fd)))) w)
                   (λ (s)
                     (let1 (fd (car s))
                       (and (< fd (vector-length v))
                            (not (zero? (bitwise-and (vector-ref v fd) (if (eq? 'input (cadr s)) 1 2)))))))))))
        (ignored () #f))))

  ; the ready subset of specs
  (define (%fds-select specs timeout)
    (let1 (ready? (%fds-select* specs timeout)) (if ready? (filter ready? specs) '())))

  (define (%check-io-spec s)
    (unless (and (list? s) (= 2 (length s)) (exact-integer? (car s)) (>= (car s) 0) (memq (cadr s) '(input output)))
      (error "cml: bad io spec, expected (fd input|output)" s))
    s)

  ; ready descriptors among specs, as a list of (fd mode); priority -1 when enabled
  (define (poll-evt* specs)
    (let1 (specs (map %check-io-spec specs))
      (if (null? specs)
        never-evt
        (%base-evt
          (τ (let1 (ready (%fds-select specs 0))
               (if (pair? ready)
                 (make-%enabled -1 (τ (%atomic-end) ready))
                 (make-%blocked
                   (λ (trans cleanup next)
                     (%letcc/call k
                       (set! %io-waiting (cons (vector specs trans cleanup k) %io-waiting))
                       (next)
                       (%impossible 'poll-evt)))))))))))

  (define (poll-evt . specs) (poll-evt* specs))

  ; IOManager.ioEvt on one descriptor, its value is fd
  (define (io-evt fd mode) (wrap (poll-evt* (list (list fd mode))) caar))

  ; IOManager.pollIO: wake, oldest first, every live waiter with a ready descriptor
  (define (%poll-io!)
    (unless (null? %io-waiting)
      (let1 (live (reverse (filter %io-item-live? %io-waiting)))
        (if (null? live)
          (set! %io-waiting '())
          (let1 (ready? (%fds-select* (append-map (λ (it) (vector-ref it 0)) live) 0))
            (set! %io-waiting
              (let loop ((items (if ready? live '())) (kept (if ready? '() (reverse live))))
                (if (null? items)
                  kept
                  (let* ((it (car items))
                         (mine (filter ready? (vector-ref it 0))))
                    (cond
                      ((not (%io-item-live? it)) (loop (cdr items) kept))
                      ((null? mine) (loop (cdr items) (cons it kept)))
                      (else (let1 (tid (%get-id-from-trans! (vector-ref it 1)))
                              ((vector-ref it 2))
                              (let1 (k (vector-ref it 3)) (%enqueue-thread! tid (λ ignored (k mine))))
                              (loop (cdr items) kept)))))))))))))

  (define (%io-live-specs) (append-map (λ (it) (vector-ref it 0)) (filter %io-item-live? %io-waiting)))

  ; result (util/result.sml) -----------------------------------------------------------------------

  ; forward: results sit on ivars, defined below
  (define-record %result ivar)

  (set-record-printer! %result (λ (r port) (display "#<result>" port)))

  (define (make-result) (make-%result (make-ivar)))                    ; Result.result
  (define (result? x) (%result? x))
  (define (result-put! r v) (ivar-put! (%result-ivar r) (cons 'ok v)))  ; Result.put
  (define (result-put-exn! r e) (ivar-put! (%result-ivar r) (cons 'exn e))) ; Result.putExn
  (define (%result-value p) (if (eq? 'ok (car p)) (cdr p) (abort (cdr p))))
  (define (result-get r) (%result-value (ivar-get (%result-ivar r))))  ; Result.get
  (define (result-get-evt r) (wrap (ivar-get-evt (%result-ivar r)) %result-value)) ; Result.getEvt

  ; process manager (Unix/proc-manager.sml) --------------------------------------------------------

  (define %proc-waiting '())             ; (pid . result)
  (define %proc-memo '())                ; (pid . event), for the pids in %proc-waiting only
  (define %proc-orphans '())             ; pids still running when their run ended, never reset

  ; reap the orphans that have exited, as ML's ProcManager keeps doing in later runs (its `waiting`
  ; list is never reset): they are not waited for, nobody gets their status
  (define (%reap-orphans!)
    (unless (null? %proc-orphans)
      (set! %proc-orphans
        (filter (λ (pid) (condition-case (receive (p normal? code) (process-wait pid #t) (and (number? p) (zero? p)))
                           (ignored () #f)))
                %proc-orphans))))

  ; ProcManager.addPid, memoized per pid while the child is not reaped: the event's value is (list
  ; exited-normally? code).  The memo entry goes when the child is reaped (the event keeps its
  ; value), so the memo does not grow with every child of a run and a pid the kernel reuses later
  ; is waited for afresh; a new process-evt on a numeric pid already reaped fails (ECHILD), as in
  ; ML, while a CHICKEN process object keeps its status and gives it again
  (define (process-evt pid)
    (unless (or (and (exact-integer? pid) (positive? pid)) (process? pid))
      (error 'process-evt "not a process id nor a process" pid))
    (%check-running 'process-evt)
    (let1 (m (assv pid %proc-memo))
      (if m
        (cdr m)
        (let* ((r (make-result))
               (e (result-get-evt r)))
          (%atomic-begin)
          (set! %proc-orphans (remove (λ (p) (eqv? p pid)) %proc-orphans))
          (set! %proc-waiting (cons (cons pid r) %proc-waiting))
          (set! %proc-memo (cons (cons pid e) %proc-memo))
          (%atomic-end)
          e))))

  ; ProcManager.pollProcs
  (define (%poll-procs!)
    (%reap-orphans!)
    (unless (null? %proc-waiting)
      (let1 (done '())
        (set! %proc-waiting
          (filter (λ (p)
                    (let1 (r (cdr p))
                      (or (condition-case
                            (receive (pid normal? code) (process-wait (car p) #t)
                              (if (and (number? pid) (zero? pid))
                                #t
                                (begin (%enqueue-tmp-thread! (τ (result-put! r (list normal? code)))) #f)))
                            (e () (%enqueue-tmp-thread! (τ (result-put-exn! r e))) #f))
                          (begin (set! done (cons (car p) done)) #f))))
                  %proc-waiting))
        (unless (null? done)
          (set! %proc-memo (remove (λ (m) (memv (car m) done)) %proc-memo))))))

  ; OS glue (Unix/unix-glue.sml) -------------------------------------------------------------------

  ; extra pollers for later layers: (name poll-thunk waiting?-thunk)
  (define %os-pollers '())

  (define (%add-os-poller! name poll waiting?)
    (set! %os-pollers (cons (list name poll waiting?) (remove (λ (p) (equal? name (car p))) %os-pollers))))

  (define (%remove-os-poller! name) (set! %os-pollers (remove (λ (p) (equal? name (car p))) %os-pollers)))

  (define (%poll-os-pollers!) (for-each (λ (p) ((cadr p))) %os-pollers))

  ; UnixGlue.pollOS
  (define (%poll-os!)
    (%poll-time!)
    (%poll-io+procs!)
    (%poll-os-pollers!))

  (define (%sleep-ms ms) (when (> ms 0) (condition-case (file-select '() '() (/ ms 1000.0)) (ignored () (void)))))

  ; UnixGlue.pause: wait for something to happen, #f when nothing can ever happen (deadlock)
  ; an os poller's waiting? gives #f (nothing awaited), a number of milliseconds (poll me within
  ; that time, a deadline as a timeout's) or any other true value (poll me every
  ; %idle-poll-interval-ms)
  (define (%os-pause!)
    (let* ((waits (map (λ (p) ((caddr p))) %os-pollers))
           (dt (fold (λ (w dt) (if (number? w) (if dt (min dt (max 0 w)) (max 0 w)) dt)) (%timeout-any-waiting) waits))
           (specs (%io-live-specs))
           (others? (or (pair? %proc-waiting) (any (λ (w) (and w (not (number? w)))) waits)))
           (bound (cond
                    ((and dt others?) (min dt %idle-poll-interval-ms))
                    (dt dt)
                    (others? %idle-poll-interval-ms)
                    (else #f))))
      (cond
        ((pair? specs)
          (when (null? (%fds-select specs (and bound (/ bound 1000.0))))
            (unless bound (%sleep-ms %idle-poll-interval-ms)))   ; only reached on select errors
          #t)
        (bound (%sleep-ms bound) #t)
        (else #f))))

  ; SyncVar (sync-var.sml) -------------------------------------------------------------------------

  (define %empty (list 'empty))

  ; kind is ivar or mvar; value is %empty or the value
  (define-record %cell kind priority read-q value)

  (set-record-printer! %cell (λ (c port) (display "#<" port) (display (%cell-kind c) port) (display ">" port)))

  (define (make-ivar) (make-%cell 'ivar 0 (%q) %empty))              ; SyncVar.iVar
  (define (make-mvar #!optional (v %empty)) (make-%cell 'mvar 0 (%q) v)) ; SyncVar.mVar / mVarInit
  (define (ivar? x) (and (%cell? x) (eq? 'ivar (%cell-kind x))))
  (define (mvar? x) (and (%cell? x) (eq? 'mvar (%cell-kind x))))
  (define (ivar=? a b) (eq? a b))                                      ; SyncVar.sameIVar
  (define (mvar=? a b) (eq? a b))                                      ; SyncVar.sameMVar

  ; ivars and mvars are one record, so its accessors cannot tell them apart: each operation checks
  ; the kind, else an mvar take or swap would empty or overwrite an ivar (ML's types rule it out)
  (define (%check-ivar who x) (unless (ivar? x) (error who "not an ivar" x)))
  (define (%check-mvar who x) (unless (mvar? x) (error who "not an mvar" x)))

  (define (%cell-bump! c) (let1 (n (%cell-priority c)) (%cell-priority-set! c (add1 n)) n))

  ; relayMsg: called atomic, leaves the atomic region at the end of the chain
  (define (%relay-msg! c msg)
    (let1 (item (%clean-and-remove! (%cell-read-q c)))
      (if item
        (%letcc/call my-k
          (%enqueue-and-switch-cur-thread! my-k (%get-id-from-trans! (car item)))
          (%box-switch! c (cdr item) msg))
        (%atomic-end))))

  ; a reader handed the value of c that dies as it is switched in (a dynamic-wind before thunk that
  ; raises, see %thread-died!) never relays it nor takes it: a temporary thread, run before anybody
  ; else, relays to the next reader the value that c holds by then, as a put would have (a full
  ; variable never has readers left waiting); the value is read when it runs, not now, since the
  ; dead reader's handler runs first and may take or swap it
  (define (%cell-wake-next! c)
    (%enqueue-tmp-thread!
      (τ (%atomic-begin)
         (let1 (v (%cell-value c))
           (if (eq? v %empty) (%atomic-end) (%relay-msg! c v))))))

  ; iPut / mPut
  (define (%cell-put! c x who)
    (%atomic-begin)
    (if (eq? %empty (%cell-value c))
      (begin
        (%cell-value-set! c x)
        (let1 (item (%clean-and-remove! (%cell-read-q c)))
          (if item
            (%letcc/call my-k
              (%enqueue-and-switch-cur-thread! my-k (%get-id-from-trans! (car item)))
              (%cell-priority-set! c 1)
              (%box-switch! c (cdr item) x))
            (%atomic-end))))
      (begin (%atomic-end) (%cml-raise 'put "put on a full variable" who c)))
    (void))

  ; the blocking path shared by the direct operations; unlike ML the waiters left by syncs that
  ; went another way are dropped as new ones are enqueued (%clean-and-enqueue!), so a select loop
  ; over a variable that stays empty does not leak (as for channels)
  (define (%cell-block! c)
    (%letcc/call k (%clean-and-enqueue! (%cell-read-q c) (cons (%mk-id) k)) (%atomic-dispatch)))

  (define (%cell-block-fn c after)
    (make-%blocked
      (λ (trans cleanup next)
        (let1 (v (%letcc/call k (%clean-and-enqueue! (%cell-read-q c) (cons trans k)) (next) (%impossible 'syncvar)))
          (cleanup)
          (after v)))))

  (define (%option v) (if (eq? v %empty) '() (list v)))

  ; SyncVar.iPut, raises (exn cml put) when full
  (define (ivar-put! iv x) (%check-ivar 'ivar-put! iv) (%cell-put! iv x 'ivar-put!))

  ; SyncVar.iGet
  (define (ivar-get iv)
    (%check-ivar 'ivar-get iv)
    (%check-running 'ivar-get)
    (%atomic-begin)
    (let1 (v (%cell-value iv))
      (if (eq? v %empty)
        (let1 (msg (%cell-block! iv)) (%relay-msg! iv msg) msg)
        (begin (%atomic-end) v))))

  ; SyncVar.iGetEvt
  (define (ivar-get-evt iv)
    (%check-ivar 'ivar-get-evt iv)
    (%base-evt
      (τ (let1 (v (%cell-value iv))
           (if (eq? v %empty)
             (%cell-block-fn iv (λ (msg) (%relay-msg! iv msg) msg))
             (make-%enabled (%cell-bump! iv) (τ (%cell-priority-set! iv 1) (%atomic-end) v)))))))

  ; a read-only poll is a clock tick too, as every CML operation: a thread busy-waiting on it gets
  ; preempted (ML's SIGALRM would preempt it)
  (define (%cell-poll c) (let1 (v (%cell-value c)) (%atomic-begin) (%atomic-end) (%option v)))

  ; SyncVar.iGetPoll: '() or (list v)
  (define (ivar-get-poll iv) (%check-ivar 'ivar-get-poll iv) (%cell-poll iv))

  ; SyncVar.mPut, raises (exn cml put) when full
  (define (mvar-put! mv x) (%check-mvar 'mvar-put! mv) (%cell-put! mv x 'mvar-put!))

  ; SyncVar.mTake
  (define (mvar-take! mv)
    (%check-mvar 'mvar-take! mv)
    (%check-running 'mvar-take!)
    (%atomic-begin)
    (let1 (v (%cell-value mv))
      (if (eq? v %empty)
        (let1 (v (%cell-block! mv)) (%cell-value-set! mv %empty) (%atomic-end) v)
        (begin (%cell-value-set! mv %empty) (%atomic-end) v))))

  ; SyncVar.mTakeEvt (its doFn does not reset the priority)
  (define (mvar-take-evt mv)
    (%check-mvar 'mvar-take-evt mv)
    (%base-evt
      (τ (let1 (v (%cell-value mv))
           (if (eq? v %empty)
             (%cell-block-fn mv (λ (v) (%cell-value-set! mv %empty) (%atomic-end) v))
             (make-%enabled (%cell-bump! mv) (τ (%cell-value-set! mv %empty) (%atomic-end) v)))))))

  ; SyncVar.mTakePoll: '() or (list v)
  (define (mvar-take-poll mv)
    (%check-mvar 'mvar-take-poll mv)
    (%atomic-begin)
    (let1 (v (%cell-value mv))
      (unless (eq? v %empty) (%cell-value-set! mv %empty))
      (%atomic-end)
      (%option v)))

  ; SyncVar.mGet
  (define (mvar-get mv)
    (%check-mvar 'mvar-get mv)
    (%check-running 'mvar-get)
    (%atomic-begin)
    (let1 (v (%cell-value mv))
      (if (eq? v %empty)
        (let1 (v (%cell-block! mv)) (%relay-msg! mv v) v)
        (begin (%atomic-end) v))))

  ; SyncVar.mGetEvt
  (define (mvar-get-evt mv)
    (%check-mvar 'mvar-get-evt mv)
    (%base-evt
      (τ (let1 (v (%cell-value mv))
           (if (eq? v %empty)
             (%cell-block-fn mv (λ (v) (%relay-msg! mv v) v))
             (make-%enabled (%cell-bump! mv) (τ (%atomic-end) v)))))))

  ; SyncVar.mGetPoll
  (define (mvar-get-poll mv) (%check-mvar 'mvar-get-poll mv) (%cell-poll mv))

  ; SyncVar.mSwap
  (define (mvar-swap! mv new)
    (%check-mvar 'mvar-swap! mv)
    (%check-running 'mvar-swap!)
    (%atomic-begin)
    (let1 (v (%cell-value mv))
      (if (eq? v %empty)
        (let1 (v (%cell-block! mv))
          (%cell-value-set! mv new)
          (%relay-msg! mv new)
          v)
        (begin (%cell-value-set! mv new) (%atomic-end) v))))

  ; SyncVar.mSwapEvt
  (define (mvar-swap-evt mv new)
    (%check-mvar 'mvar-swap-evt mv)
    (%base-evt
      (τ (let1 (v (%cell-value mv))
           (if (eq? v %empty)
             (%cell-block-fn mv (λ (v) (%cell-value-set! mv new) (%relay-msg! mv new) v))
             (make-%enabled (%cell-bump! mv) (τ (%cell-value-set! mv new) (%atomic-end) v)))))))

  ; Mailbox (mailbox.sml) ---------------------------------------------------------------------------

  ; state: (empty . fq) with fq a functional queue of waiting (trans . k), or (nonempty prio . fq)
  ; with fq a non-empty functional queue of messages; functional queues are (front . rear)
  (define-record %mailbox state budget)   ; budget as for queues (%clean-and-enqueue!)

  (set-record-printer! %mailbox (λ (m port) (display "#<mailbox>" port)))

  (define %fq-empty '(() . ()))
  (define (%fq-enqueue q x) (cons (car q) (cons x (cdr q))))
  (define (%fq-empty? q) (and (null? (car q)) (null? (cdr q))))
  ; (item . rest), for a non-empty queue
  (define (%fq-dequeue q)
    (if (pair? (car q))
      (cons (caar q) (cons (cdar q) (cdr q)))
      (%fq-dequeue (cons (reverse (cdr q)) '()))))

  ; cleanAndRemove on a queue of waiters: (item . rest-queue) or #f
  (define (%fq-clean-and-remove q)
    (let1 (f (%clean (car q)))
      (if (pair? f)
        (cons (car f) (cons (cdr f) (cdr q)))
        (let1 (rr (%clean-rev (cdr q) '()))
          (and (pair? rr) (cons (car rr) (cons (cdr rr) '())))))))

  (define (%fq-clean q) (cons (%clean-all (car q)) (%clean-all (cdr q))))

  (define (make-mailbox) (make-%mailbox (cons 'empty %fq-empty) 0)) ; Mailbox.mailbox
  (define (mailbox? x) (%mailbox? x))
  (define (mailbox=? a b) (eq? a b))                                 ; Mailbox.sameMailbox
  (define (%mailbox-reset! mb) (%mailbox-state-set! mb (cons 'empty %fq-empty)) (%mailbox-budget-set! mb 0)) ; Mailbox.resetMbox

  ; Mailbox.send: never blocks; hands off to a waiting receiver, or buffers (yielding when the
  ; mailbox already held messages)
  (define (mailbox-send! mb x)
    (%check-running 'mailbox-send!)
    (%atomic-begin)
    (let1 (st (%mailbox-state mb))
      (if (eq? 'empty (car st))
        (let1 (r (%fq-clean-and-remove (cdr st)))
          (if r
            (%letcc/call k
              (%mailbox-state-set! mb (cons 'empty (cdr r)))
              (%enqueue-and-switch-cur-thread! k (%get-id-from-trans! (caar r)))
              (%box-switch! mb (cdar r) x))
            (begin (%mailbox-state-set! mb (cons* 'nonempty 1 (cons (list x) '()))) (%atomic-end))))
        (%letcc/call k
          (%mailbox-state-set! mb (cons* 'nonempty (cadr st) (%fq-enqueue (cddr st) x)))
          (%atomic-yield k))))
    (void))

  ; the message x handed to a receiver that died as it was switched in (see %thread-died!), called
  ; atomic: it goes to the next live receiver, or back to the front of the mailbox, as if that
  ; receiver had never been there (mailbox-send! never blocks and the message must not be lost)
  (define (%mailbox-redeliver! mb x)
    (let1 (st (%mailbox-state mb))
      (if (eq? 'empty (car st))
        (let1 (r (%fq-clean-and-remove (cdr st)))
          (if r
            (let ((tid (%get-id-from-trans! (caar r))) (k (cdar r)))
              (%mailbox-state-set! mb (cons 'empty (cdr r)))
              (%enqueue-thread! tid (λ ignored (%atomic-begin) (%box-switch! mb k x))))
            (%mailbox-state-set! mb (cons* 'nonempty 1 (cons (list x) '())))))
        (let1 (q (cddr st))
          (%mailbox-state-set! mb (cons* 'nonempty (cadr st) (cons (cons x (car q)) (cdr q))))))))

  ; getMsg: called atomic, leaves the atomic region
  (define (%mailbox-get-msg! mb q)
    (let* ((p (%fq-dequeue q))
           (q (cdr p)))
      (%mailbox-state-set! mb (if (%fq-empty? q) (cons 'empty %fq-empty) (cons* 'nonempty 1 q)))
      (%atomic-end)
      (car p)))

  ; add the waiter item to the empty mailbox mb, the stale waiters being dropped as in
  ; %clean-and-enqueue! (by the blocking recv too: a waiter left by a previous session would
  ; otherwise stay there for good, with its continuation, when nobody sends on mb any more)
  (define (%mailbox-add-waiter! mb item)
    (let ((q (cdr (%mailbox-state mb))) (b (%mailbox-budget mb)))
      (if (positive? b)
        (%mailbox-budget-set! mb (sub1 b))
        (begin
          (set! q (%fq-clean q))
          (%mailbox-budget-set! mb (max %clean-budget-min (+ (length (car q)) (length (cdr q)))))))
      (%mailbox-state-set! mb (cons 'empty (%fq-enqueue q item)))))

  ; Mailbox.recv; the waiter is added where no old state of the mailbox is in scope (see
  ; %cvar-block)
  (define (mailbox-recv mb)
    (%check-running 'mailbox-recv)
    (%atomic-begin)
    (if (eq? 'empty (car (%mailbox-state mb)))
      (let1 (msg (%letcc/call k (%mailbox-add-waiter! mb (cons (%mk-id) k)) (%atomic-dispatch)))
        (%atomic-end)
        msg)
      (%mailbox-get-msg! mb (cddr (%mailbox-state mb)))))

  ; made where no old state of the mailbox is in scope, see %cvar-block
  (define (%mailbox-block mb)
    (make-%blocked
      (λ (trans cleanup next)
        (let1 (msg (%letcc/call k
                     (%mailbox-add-waiter! mb (cons trans k))
                     (next)
                     (%impossible 'mailbox-recv-evt)))
          (cleanup)
          (%atomic-end)
          msg))))

  ; Mailbox.recvEvt
  (define (mailbox-recv-evt mb)
    (%base-evt
      (τ (if (eq? 'empty (car (%mailbox-state mb)))
           (%mailbox-block mb)
           (let* ((st (%mailbox-state mb)) (p (cadr st)) (q (cddr st)))
             (%mailbox-state-set! mb (cons* 'nonempty (add1 p) q))
             (make-%enabled p (τ (%mailbox-get-msg! mb q))))))))

  ; Mailbox.recvPoll: '() or (list msg)
  (define (mailbox-recv-poll mb)
    (%atomic-begin)
    (let1 (st (%mailbox-state mb))
      (if (eq? 'empty (car st))
        (begin (%atomic-end) '())
        (list (%mailbox-get-msg! mb (cddr st))))))

  ; Barrier (barrier.sml, with its bugs fixed) ------------------------------------------------------
  ;
  ; Fixes: (1) an enrollment is back to `enrolled` after every round, so it can wait again;
  ; (2) resigning decrements the number of enrolled threads and completes a pending round if the
  ; remaining ones are all waiting; (3) every path leaves the atomic region.  Moreover waits are
  ; events: a waiter is registered with the transaction of its sync and counted only while that
  ; transaction is live, so a barrier-wait-evt that loses a choice does not count as arrived; the
  ; arrivals are the distinct live transactions, so a sync waiting on several enrollments of one
  ; barrier counts once, and two waits on the same enrollment in one sync raise "multiple barrier
  ; waits" as a second Barrier.wait does in ML.  Waiters are woken in arrival order, oldest first (a
  ; deliberate departure: barrier.sml's List.app over its newest-first list wakes them newest first,
  ; an order that BARRIER does not specify); if the update
  ; raises, every participant raises it.  The arrivals are counted exactly (an O(waiters) scan) only
  ; when an upper bound, the number of distinct transactions registered, says that the round may be
  ; complete, so a round of n threads costs O(n), as with ML's counter.

  ; waiting: #(enrollment trans cleanup k), newest first; distinct: the number of distinct
  ; transactions in waiting, stale ones included (the entries of one sync are adjacent there: its
  ; block-fns run one after the other, atomically)
  (define-record %barrier state update n-enrolled waiting distinct)
  (define-record %enrollment barrier status entry polled)   ; status: enrolled | resigned; polled: a sync number

  (set-record-printer! %barrier (λ (b port) (display "#<barrier>" port)))
  (set-record-printer! %enrollment
    (λ (b port) (display "#<enrollment " port) (display (%enrollment-status b) port) (display ">" port)))

  ; Barrier.barrier, but with the initial state first: (make-barrier init update)
  (define (make-barrier init update) (make-%barrier init update 0 '() 0))
  (define (barrier? x) (%barrier? x))
  (define (enrollment? x) (%enrollment? x))

  ; Barrier.enroll
  (define (barrier-enroll b)
    (%atomic-begin)
    (%barrier-n-enrolled-set! b (add1 (%barrier-n-enrolled b)))
    (%atomic-end)
    (make-%enrollment b 'enrolled #f #f))

  ; the number of distinct transactions of a waiting list, whose equal ones are adjacent
  (define (%distinct-transactions ws)
    (let loop ((ws ws) (prev #f) (n 0))
      (cond
        ((null? ws) n)
        ((eq? prev (vector-ref (car ws) 1)) (loop (cdr ws) prev n))
        (else (loop (cdr ws) (vector-ref (car ws) 1) (add1 n))))))

  (define (%barrier-live-waiters! b)
    (let1 (live (filter (λ (w) (%trans-live? (vector-ref w 1))) (%barrier-waiting b)))
      (for-each (λ (w) (unless (%trans-live? (vector-ref w 1))
                         (let1 (e (vector-ref w 0)) (when (eq? w (%enrollment-entry e)) (%enrollment-entry-set! e #f)))))
                (%barrier-waiting b))
      (%barrier-waiting-set! b live)
      (%barrier-distinct-set! b (%distinct-transactions live))
      live))

  ; the number of threads waiting, i.e. of distinct live transactions: O(waiters)
  (define (%barrier-arrived b) (%barrier-live-waiters! b) (%barrier-distinct b))

  ; whether the sync polling a wait completes the round, i.e. all the other enrolled threads wait.
  ; The live ones are at most the distinct ones, so the exact count is needed only when those are
  ; enough: one O(waiters) count per round instead of one per arrival (ML keeps a counter)
  (define (%barrier-last-arrival? b)
    (let1 (n (%barrier-n-enrolled b))
      (and (>= (add1 (%barrier-distinct b)) n)
           (= (add1 (%barrier-arrived b)) n))))

  (define (%enrollment-waiting? e)
    (let1 (w (%enrollment-entry e)) (and w (%trans-live? (vector-ref w 1)) #t)))

  ; run the update and wake every live waiter; called atomic, returns (ok . v) or (exn . e)
  (define (%barrier-complete! b)
    (let* ((r (condition-case (let1 (x ((%barrier-update b) (%barrier-state b)))
                                (%barrier-state-set! b x)
                                (cons 'ok x))
                (e () (cons 'exn e))))
           (ws (reverse (%barrier-live-waiters! b))))
      (%barrier-waiting-set! b '())
      (%barrier-distinct-set! b 0)
      (for-each (λ (w)
                  (%enrollment-entry-set! (vector-ref w 0) #f)
                  (when (%trans-live? (vector-ref w 1))            ; claimed by a previous entry of its sync
                    (let1 (tid (%get-id-from-trans! (vector-ref w 1)))
                      (let1 (k (vector-ref w 3)) (%enqueue-thread! tid (λ ignored (k r))))
                      ((vector-ref w 2)))))
                ws)
      r))

  (define (%barrier-error msg e) (τ (%atomic-end) (%cml-raise 'barrier msg e)))

  ; the event form of Barrier.wait: its value is the new state
  (define (barrier-wait-evt e)
    (%base-evt
      (τ (let1 (b (%enrollment-barrier e))
           (cond
             ((eq? 'resigned (%enrollment-status e)) (make-%enabled -1 (%barrier-error "barrier wait after resignation" e)))
             ((or (%enrollment-waiting? e) (eqv? %sync-count (%enrollment-polled e)))
               (make-%enabled -1 (%barrier-error "multiple barrier waits" e)))
             ((begin (%enrollment-polled-set! e %sync-count) (%barrier-last-arrival? b))
               (make-%enabled -1 (τ (let1 (r (%barrier-complete! b)) (%atomic-end) (%result-value r)))))
             (else (make-%blocked
                     (λ (trans cleanup next)
                       (let1 (r (%letcc/call k
                                  (let ((w (vector e trans cleanup k)) (ws (%barrier-waiting b)))
                                    (%enrollment-entry-set! e w)
                                    (unless (and (pair? ws) (eq? trans (vector-ref (car ws) 1)))
                                      (%barrier-distinct-set! b (add1 (%barrier-distinct b))))
                                    (%barrier-waiting-set! b (cons w ws)))
                                  (next)
                                  (%impossible 'barrier-wait-evt)))
                         (%result-value r))))))))))

  ; Barrier.wait
  (define (barrier-wait e) (sync (barrier-wait-evt e)))

  ; Barrier.resign
  (define (barrier-resign e)
    (%atomic-begin)
    (cond
      ((eq? 'resigned (%enrollment-status e)) (%atomic-end))
      ((%enrollment-waiting? e) (%atomic-end) (%cml-raise 'barrier "resign while waiting" e))
      (else
        (let1 (b (%enrollment-barrier e))
          (%enrollment-status-set! e 'resigned)
          (%barrier-n-enrolled-set! b (sub1 (%barrier-n-enrolled b)))
          (let1 (n (%barrier-arrived b))
            (when (and (positive? n) (= n (%barrier-n-enrolled b))) (%barrier-complete! b)))
          (%atomic-end))))
    (void))

  ; Barrier.value
  (define (barrier-value e) (%barrier-state (%enrollment-barrier e)))

  ; CleanUp (cleanup.sml, init-cleanup.sml) ---------------------------------------------------------

  ; when: at-init | at-init-fn | at-shutdown | at-exit; run-cml only triggers at-init and at-shutdown
  (define cml/at-all '(at-exit at-shutdown at-init at-init-fn))

  (define %hooks '())                    ; (name whens proc), newest first
  (define %cleanup-lock #f)
  (define %chan-list '())                ; (name init shut), newest first
  (define %mbox-list '())
  (define %server-list '())

  ; whens as a list of cml/at-all's symbols: 'all, one of them, or a list of them
  (define (%whens w)
    (let1 (l (cond ((eq? w 'all) cml/at-all) ((symbol? w) (list w)) (else w)))
      (unless (and (list? l) (every (λ (x) (memq x cml/at-all)) l))
        (error 'cml/add-cleaner! "bad whens, expected all, or one or a list of" cml/at-all w))
      l))

  ; CleanUp.protect: hold the lock while CML runs.  The lock is taken as an internal hold (see
  ; %hold-take!): a thread that dies holding it (a dynamic-wind thunk raising as it is switched out
  ; right after the take) gives it back, or every later registration and the next cleanup would
  ; wait for it for good
  (define (%protect thunk)
    (if %running
      (begin
        (%hold-take! %cleanup-lock)
        (let1 (v (handle-exceptions e (begin (%hold-put! %cleanup-lock) (abort e)) (thunk)))
          (%hold-put! %cleanup-lock)
          v))
      (thunk)))

  (define (%delete-named name l) (remove (λ (h) (equal? name (car h))) l))

  ; CleanUp.addCleaner: returns the previous (whens proc) as an option.  A bad whens or a
  ; non-procedure raises here, in the caller: the cleaners run in run-cml's own context, where a
  ; non-procedure would make every later run-cml raise before running its thunk
  (define (cml/add-cleaner! name whens proc)
    (unless (procedure? proc) (error 'cml/add-cleaner! "not a procedure" proc))
    (set! whens (%whens whens))
    (%protect (τ (let1 (old (assoc name %hooks))
                   (set! %hooks (cons (list name whens proc) (%delete-named name %hooks)))
                   (if old (list (cdr old)) '())))))

  ; CleanUp.removeCleaner: returns the removed (whens proc) as an option
  (define (cml/remove-cleaner! name)
    (%protect (τ (let1 (old (assoc name %hooks))
                   (set! %hooks (%delete-named name %hooks))
                   (if old (list (cdr old)) '())))))

  ; CleanUp.clean: each cleaner runs in its own thread, given at most 1 second
  (define (%cleanup-run w)
    (mvar-take! %cleanup-lock)
    (let1 (fns (let1 (l (filter (λ (h) (memq w (cadr h))) %hooks))
                 (if (memq w '(at-init at-init-fn)) (reverse l) l)))
      (when (eq? w 'at-init-fn) (set! %hooks (filter (λ (h) (memq 'at-exit (cadr h))) %hooks)))
      (mvar-put! %cleanup-lock (void))
      (for-each (λ (h) (select (join-evt (spawn/call (caddr h) w)) (timeout-evt 1))) fns)))

  (define (%unlog-item l name)
    (unless (assoc name l) (%cml-raise 'unlog "unlog: no such name" name))
    (%delete-named name l))

  ; CleanUp.logChannel and friends: logged items are reset at every run start and shutdown.  A
  ; wrong-typed item raises here, in the caller: its reset, run with the others' in one cleaner
  ; thread, would kill that thread at every start and shutdown and so skip the items after it
  (define (cml/log-channel! name ch)
    (unless (channel? ch) (error 'cml/log-channel! "not a channel" ch))
    (%protect (τ (let1 (f (τ (%channel-reset! ch)))
                   (set! %chan-list (cons (list name f f) (%delete-named name %chan-list)))))))
  (define (cml/unlog-channel! name) (%protect (τ (set! %chan-list (%unlog-item %chan-list name)))))

  (define (cml/log-mailbox! name mb)
    (unless (mailbox? mb) (error 'cml/log-mailbox! "not a mailbox" mb))
    (%protect (τ (let1 (f (τ (%mailbox-reset! mb)))
                   (set! %mbox-list (cons (list name f f) (%delete-named name %mbox-list)))))))
  (define (cml/unlog-mailbox! name) (%protect (τ (set! %mbox-list (%unlog-item %mbox-list name)))))

  ; CleanUp.logServer: init runs at every start, shut at every shutdown (at most 2 seconds)
  (define (cml/log-server! name init shut)
    (unless (procedure? init) (error 'cml/log-server! "not a procedure" init))
    (unless (procedure? shut) (error 'cml/log-server! "not a procedure" shut))
    (%protect (τ (set! %server-list (cons (list name init shut) (%delete-named name %server-list))))))
  (define (cml/unlog-server! name) (%protect (τ (set! %server-list (%unlog-item %server-list name)))))

  (define (cml/unlog-all!) (set! %chan-list '()) (set! %mbox-list '()) (set! %server-list '()))

  (define (%app-init l) (for-each (λ (it) ((cadr it))) (reverse l)))

  (define (%clean-channels w) (%app-init %chan-list) (%app-init %mbox-list))

  (define (%clean-servers w)
    (if (memq w '(at-init at-init-fn))
      (%app-init %server-list)
      (for-each (λ (it) (select (join-evt (spawn (caddr it))) (timeout-evt 2))) %server-list)))

  ; RunCML (glue/run-cml-fn.sml) --------------------------------------------------------------------

  ; Thread.reset / Scheduler.reset / TimeOut.reset and friends: every run starts afresh
  (define (%reset!)
    (set! %run-id (add1 %run-id))
    (set! %tid-count 0)
    (set! %dummy-tid (%new-dummy-tid))
    (set! %cur-tid %dummy-tid)
    (set! %atomic-state 'non-atomic)
    (%q-reset! %rdy-q1)
    (%q-reset! %rdy-q2)
    (%time-reset!)
    (set! %io-waiting '())
    (set! %proc-orphans (append (map car %proc-waiting) %proc-orphans))
    (%reap-orphans!)
    (set! %proc-waiting '())
    (set! %proc-memo '())
    (set! %random-count 0)
    (set! %cleanup-lock (make-mvar (void)))
    (set! %base-k #f)
    (set! %shutdown-k #f)
    (%switched-in!)
    (set! %commit-hook #f))

  ; RunCML.doit: runs thunk in a new thread and returns 'failure on deadlock (no runnable thread and
  ; nothing pending on timeouts, descriptors or processes), or the status given to cml/shutdown.
  ; quantum is the number of CML operations (%atomic-end ticks) between two preemptions.
  (define (run-cml thunk #!key (quantum %default-quantum))
    (when %running (%cml-raise 'running "run-cml: CML is already running"))
    (unless (procedure? thunk) (error "run-cml: not a procedure" thunk))
    (unless (and (exact-integer? quantum) (> quantum 0))
      (error "run-cml: quantum must be a positive exact integer" quantum))
    (%reset!)
    (set! %quantum quantum)
    (set! %ticks %quantum)
    (set! %running #t)
    ; every thread runs within this dynamic extent, so the after thunk only runs when the session
    ; ends: normally, by an exception, or by a continuation escaping from it (then it resets)
    (dynamic-wind
      void
      (τ (handle-exceptions e
           (begin (%reset!) (set! %running #f) (abort e))
           (let* ((result (%letcc/call done-k
                            (set! %shutdown-k done-k)
                            (let1 (th (%letcc/call k (set! %base-k k) #f))
                              (when th (th) (%impossible 'isolated-thread-returned)))
                            (%cleanup-run 'at-init)
                            (spawn thunk)
                            (%dispatch)))
                  (status (cdr result)))
             ; a further shutdown during the shutdown cleaners just stops them
             (%letcc/call finish-k
               (set! %shutdown-k (λ ignored (finish-k #f)))
               (%cleanup-run 'at-shutdown))
             (%reset!)
             (set! %running #f)
             status)))
      (τ (when %running (%reset!) (set! %running #f)))))

  ; RunCML.shutdown
  (define (cml/shutdown #!optional (status 'success))
    (%check-running 'shutdown)
    (set! %cur-tid %dummy-tid)
    (%switch! %shutdown-k (cons #t status)))

  ; version (version.sml) and debug (debug.sml) ----------------------------------------------------

  (define cml/version '((system . "Concurrent ML (aux cml)") (version-id 1 0 10) (date . "September 15, 1997")))

  (define cml/banner "Concurrent ML (aux cml), Version 1.0.10, September 15, 1997")

  (define cml/debug? (make-parameter #f))

  ; Debug.sayDebugId
  (define (cml/debug . strings)
    (when (cml/debug?)
      (let1 (p (current-error-port))
        (display (tid->string %cur-tid) p)
        (for-each (λ (s) (display " " p) (display s p)) strings)
        (newline p))))

  ; standard cleaners (init-cleanup.sml), registered at load time -----------------------------------

  (set! %cleanup-lock (make-mvar (void)))
  (cml/add-cleaner! "Channels&Mailboxes" '(at-init at-shutdown) %clean-channels)
  ; TraceCML's (see trace-close-files!) is registered before "Servers" so that it runs after it at
  ; shutdown (cleaners run newest first there): the shutdown functions of logged servers may trace
  ; to a file, and it must be closed after them (ML's tracerStop is itself a server logged first)
  (cml/add-cleaner! "TraceCML" '(at-shutdown) (λ (w) (trace-close-files!)))
  (cml/add-cleaner! "Servers" 'all %clean-servers)

  ; IO / OS layer ==================================================================================

  ;; IO / OS layer of (aux cml), on top of the core io-evt / process-evt (Unix/os-process.sml,
  ;; Unix/new-unix.sml, IO/new-text-io-fn.sml, IO/chan-io-fn.sml, Sockets/cml-socket.sml).
  ;;
  ;; - system-evt forks eagerly, as ML's OS.Process.systemEvt; its value is the shell's exit status as
  ;;   an integer (0 is success, 128 + n when killed by signal n), cml/system syncs on it.  cml/execute is
  ;;   Unix.execute: a child process whose stdin/stdout are fd-backed ports, reaped with process-evt,
  ;;   exiting with status 128 when the exec fails; unlike ML, a command without a "/" is searched
  ;;   in PATH and argv[0] is the command as given, not its basename.  Its optional third argument,
  ;;   a list of "NAME=value" strings, is Unix.executeInEnv's environment (no PATH search then, as
  ;;   in ML).  The children of both start
  ;;   with SIGPIPE at its default action, as in ML, although this process ignores it (see below).
  ;; - port input events follow the imperative TextIO idiom of new-text-io-fn.sml: each sync spawns a
  ;;   helper thread (with-nack + reply channel) that takes the port's lock, reads what is available
  ;;   WITHOUT blocking the process into a per-port side buffer, waits for readiness (io-evt) when that
  ;;   is not enough and offers the value on a rendezvous together with the nack.  Only the commit
  ;;   removes characters from the side buffer (atomically, as part of the rendezvous), so a branch that
  ;;   loses a select, or a timeout that fires in the middle of a line, loses nothing: the partial data
  ;;   is kept for the next input event on the port.  The event polls a fast path of its own too:
  ;;   when the lock is free and the side buffer, completed by a drain for stdio, tcp and string
  ;;   ports, holds the value, it commits in the syncing thread, so a poll (sync/timeout with 0
  ;;   seconds) finds the input that is there; the helper is queued, not run first, and leaves at
  ;;   once when its sync went another way.  Spurious readiness just makes the helper read
  ;;   nothing and wait again.  A sync abandoned while forcing or polling (a later guard that raises,
  ;;   say) sets the nacks made so far (see %force-group), so such a helper releases the port.  A helper
  ;;   reads at most 4096 characters at a time, then checks its nack and yields with a preemption, so a
  ;;   stream that never pauses (a line that never ends) neither freezes the other threads and the
  ;;   timeouts nor keeps it reading once its sync went another way.
  ;; - "without blocking": CHICKEN's `char-ready?` on a stdio (FILE*) port only looks at the descriptor,
  ;;   not at data already buffered by stdio (e.g. after a direct read-char), so those ports are read
  ;;   from their descriptor: it is polled with a 0 timeout and, when readable, read once (a readable
  ;;   pipe, tty or socket gives what it has at once); reading stops when it is not readable.  Its file
  ;;   status flags are never changed (O_NONBLOCK would hit every holder of the open file description:
  ;;   stdout on the same tty, children that inherited it).  The bytes are decoded here with the port's
  ;;   encoding (UTF-8, latin-1 or binary, as a direct read decodes them), so a UTF-8 sequence split
  ;;   across two writes waits for its end.  On a port read directly before its first input event, what is left
  ;;   in its stdio buffer is taken first, with read-char while the descriptor is swapped for an empty
  ;;   pipe (so stdio never reads the descriptor itself); only a sequence that the direct read left
  ;;   split in the stdio buffer itself is garbled.  tcp ports have a char-ready? that is exact per
  ;;   byte, not per char: they are read byte by byte while it holds (read-byte) and decoded here too,
  ;;   as a read-char on the lead byte of a sequence whose end has not arrived would block the
  ;;   process; ports without a descriptor (custom ports) are read while char-ready? holds and polled
  ;;   every 5 ms otherwise; channel ports wait on their channel.
  ;; - once input events are used on a port, direct reads on it may miss characters that sit in the
  ;;   side buffer, or that the drain has read from the descriptor: a line or string cut by a timeout
  ;;   or a lost select, the char of a successful peek-char-evt (it stays there for the next input
  ;;   event), the char after a lone "\r" ending a line (read to rule out "\r\n").  So after input
  ;;   events, keep reading the port with input events.  input-line-evt ends a line at "\n", "\r\n"
  ;;   or a lone "\r", as read-line does.  input-evt (TextIO.inputEvt) commits as soon as a char is
  ;;   there and gives every char available by then (at most 4096 more than the side buffer held).
  ;; - port output events commit when the descriptor is writable; the string is then written in chunks
  ;;   of at most 128 characters, each after a new readiness wait and followed by a flush, so a writer
  ;;   never blocks the process on a full pipe (only the syncing thread waits); on a non-blocking
  ;;   descriptor (a tcp socket) a chunk is up to 65536 characters, written as far as the kernel
  ;;   takes it, the rest after a readiness wait (small writes would be held by Nagle's algorithm).
  ;;   A per-port write lock is held from the first chunk to the last, so the strings of two
  ;;   write-string-evts on one port never interleave (TextIO.output holds the stream lock).  A write
  ;;   to a pipe whose reader has exited raises the i/o condition of file-write, errno EPIPE (as ML
  ;;   raises Io): loading (chicken tcp), as this module does, ignores SIGPIPE, and CHICKEN's stdio
  ;;   flush drops write errors, so stdio and tcp ports are written through their descriptor,
  ;;   whatever their encoding.  Input
  ;;   events on a closed port raise at sync, as a direct read does (the descriptor number may
  ;;   belong to another file by then), and so does a reader, or a writer between two chunks,
  ;;   waiting on a descriptor port that another thread closes: one sweep every 0.1 s looks for
  ;;   the closed ports among those awaited, whatever their descriptor numbers have become
  ;;   meanwhile, and only when some thread ran since the previous sweep, so an idle waiter costs
  ;;   nothing by itself.  Channel ports are not swept: a reader waits on the channel only, so a reader
  ;;   blocked on a channel port nobody writes to is a deadlock, as in ML.
  ;; - tcp: (chicken tcp) exists in CHICKEN 6.  tcp-accept-evt waits for the listener's readiness and
  ;;   accepts in the wrap (never in a guard, so a losing branch does not consume a connection, which
  ;;   fixes ML's acceptEvt); the wrap checks the readiness again and waits again if another thread
  ;;   accepted first.  Its value is (list in out).  tcp-connect-evt connects after the commit:
  ;;   (chicken tcp) has no non-blocking connect, so the handshake itself blocks the whole process.
  ;; - ChanIO: channel ports carry strings (chars are accepted too); empty strings are skipped as in
  ;;   ML, #!eof ends the input port for good (ML never signals end of stream), any other value is an
  ;;   error raised by the read (or delivered by the input event) that reads it (an input event keeps
  ;;   the chars that came before it in the side buffer, for the next input event); the output port
  ;;   buffers and sends one string per flush-output (or every 1024 characters), close flushes and
  ;;   sends #!eof; write-string-evt on it, and the send of a flush, take the buffered output at their
  ;;   commit, so a thread's output keeps its order whichever commits first; neither ever sends an
  ;;   empty string (a write-string-evt of "" with nothing buffered commits at once, as ML's writer
  ;;   never puts an empty vector on its channel).  The output events (output-evt, write-string-evt)
  ;;   raise, at every sync, once their port is closed.
  ;; - the per-port state is found in O(1) (see %port-table): the ports without a descriptor (channel,
  ;;   string and custom ports) carry theirs in their data slot, and the entries of the other ports
  ;;   are weak, so ports dropped without being closed are collected; only a custom port whose maker
  ;;   put data of its own in that slot has its state looked up linearly among such ports.

  ; ports: shared helpers --------------------------------------------------------------------------

  (define %port-poll-secs 0.005)           ; polling interval for ports without a descriptor
  (define %port-closed-check-ms 100)       ; how often the waiting readers and writers are checked for a close
  (define %port-write-chunk 128)           ; characters per write, at most 512 bytes in UTF-8
  (define %port-write-chunk/nonblock 65536) ; the same on a non-blocking descriptor (a tcp socket)
  (define %chan-port-chunk 1024)           ; ChanIO chunkSize

  ; the descriptor of port, or #f
  (define (%port-fileno port) (condition-case (port->fileno port) (ignored () #f)))

  ; stdio (FILE*) ports are 'stream, tcp ports 'socket, custom ports 'custom
  (define (%port-kind port) (##sys#slot port 7))

  (define (%string-index s c)
    (let1 (n (string-length s))
      (let loop ((i 0)) (cond ((= i n) #f) ((char=? c (string-ref s i)) i) (else (loop (add1 i)))))))

  ; read with step (a thunk returning a char, #!eof or 'none) until done? holds for a char, nothing is
  ; available, eof, or %port-drain-max chars were read: (values chars-in-reverse eof? more?), more?
  ; telling the last case.  The bound keeps a reader of a stream that never pauses (a long line,
  ; input-all-evt on a busy pipe) from keeping the processor: see %port-reader
  (define %port-drain-max 4096)

  (define (%port-drain step done?)
    (let loop ((acc '()) (n 0))
      (if (= n %port-drain-max)
        (values acc #f #t)
        (let1 (c (step))
          (cond
            ((eq? c 'none) (values acc #f #f))
            ((eof-object? c) (values acc #t #f))
            ((done? c) (values (cons c acc) #f #f))
            (else (loop (cons c acc) (add1 n))))))))

  (define (%eagain? e)
    (and ((condition-predicate 'exn) e)
         (memv ((condition-property-accessor 'exn 'errno #f) e) (list errno/again errno/wouldblock))
         #t))

  ; UTF-8, for %stream-drainer: the length of the sequence a lead byte starts, #f for a byte that
  ; cannot start one
  (define (%utf8-length b)
    (cond ((< b #x80) 1) ((< b #xC2) #f) ((< b #xE0) 2) ((< b #xF0) 3) ((< b #xF5) 4) (else #f)))

  (define (%utf8-continuation? b) (= #x80 (bitwise-and b #xC0)))

  ; the char of the complete sequence of len bytes at i in bv, #\xFFFD when it is not a scalar value
  (define (%utf8-decode bv i len)
    (let loop ((k 1) (cp (bitwise-and (bytevector-u8-ref bv i) (vector-ref '#(#x7F #x1F #x0F #x07) (sub1 len)))))
      (if (< k len)
        (loop (add1 k) (bitwise-ior (arithmetic-shift cp 6) (bitwise-and (bytevector-u8-ref bv (+ i k)) #x3F)))
        (if (or (< cp (vector-ref '#(0 #x80 #x800 #x10000) (sub1 len))) (<= #xD800 cp #xDFFF) (> cp #x10FFFF))
          #\xFFFD
          (integer->char cp)))))

  ; a stdio port that was never read (so its stdio buffer is empty)
  (define (%port-untouched? port)
    (and (eqv? 1 (##sys#slot port 4)) (eqv? 0 (##sys#slot port 5))      ; row and column
         (not (##sys#slot port 10)) (not (##sys#slot port 6))))           ; no peeked char, no pending eof

  ; a step thunk decoding UTF-8 from the bytes that (fill port) gives: a non-empty bytevector, #f at
  ; end of file or 'none when nothing is available now.  An incomplete sequence waits in bv for the
  ; next fill, so a sequence split across two writes (or two segments) is decoded whole
  (define (%utf8-stepper fill)
    (let ((bv (make-bytevector 0)) (i 0))
      ; #t when bytes were read, #f at end of file, 'none when nothing is available
      (define (read-bytes! port)
        (let1 (r (fill port))
          (if (bytevector? r)
            (let* ((m (- (bytevector-length bv) i)) (n (bytevector-length r)) (nbv (make-bytevector (+ m n))))
              (bytevector-copy! nbv 0 bv i)
              (bytevector-copy! nbv m r 0 n)
              (set! bv nbv)
              (set! i 0)
              #t)
            r)))
      (define (step port)
        (let1 (avail (- (bytevector-length bv) i))
          (if (zero? avail)
            (case (read-bytes! port) ((#t) (step port)) ((none) 'none) (else #!eof))
            (let1 (len (%utf8-length (bytevector-u8-ref bv i)))
              (if (not len)
                (begin (set! i (add1 i)) #\xFFFD)
                (let1 (bad (let loop ((k 1))
                             (cond ((or (= k len) (= k avail)) #f)
                                   ((%utf8-continuation? (bytevector-u8-ref bv (+ i k))) (loop (add1 k)))
                                   (else k))))
                  (cond
                    (bad (set! i (+ i bad)) #\xFFFD)
                    ((< avail len)
                      (case (read-bytes! port)
                        ((#t) (step port))
                        ((none) 'none)
                        (else (set! i (+ i avail)) #\xFFFD)))          ; end of file inside a sequence
                    (else (let1 (c (%utf8-decode bv i len)) (set! i (+ i len)) c)))))))))
      step))

  ; the chars left in the stdio buffer of a port that was read directly: they are read with
  ; read-char while the descriptor is replaced by an empty non-blocking pipe, so that stdio never
  ; reads the descriptor itself (an EAGAIN inside a UTF-8 sequence would garble it) and stops at the
  ; first EAGAIN; a pending end of file ends the list.  Only a sequence that the direct read left
  ; split in the stdio buffer itself comes out garbled
  (define (%stdio-buffered-chars port fd)
    (receive (r w) (create-pipe)
      (let1 (saved (duplicate-fileno fd))
        (define (restore) (duplicate-fileno saved fd) (for-each file-close (list saved r w)))
        (file-control r fcntl/setfl (bitwise-ior (file-control r fcntl/getfl) open/nonblock))
        (duplicate-fileno r fd)
        (let1 (l (handle-exceptions e
                   (begin (restore) (abort e))
                   (let loop ((acc '()))
                     (let1 (c (condition-case (read-char port) (e () (if (%eagain? e) 'none (abort e)))))
                       (cond
                         ((eq? c 'none) (reverse acc))
                         ((eof-object? c) (reverse (cons c acc)))
                         (else (loop (cons c acc))))))))
          (restore)
          l))))

  ; a step thunk giving, one by one, the chars of the bytes that (fill port) gives (see
  ; %utf8-stepper), each piece decoded whole with the port encoding enc, as a direct read does: for
  ; the encodings other than UTF-8, whose chars are single bytes (latin-1, binary)
  (define (%bytes-stepper fill enc)
    (let ((s "") (i 0))
      (define (step port)
        (if (< i (string-length s))
          (let1 (c (string-ref s i)) (set! i (add1 i)) c)
          (let1 (r (fill port))
            (cond
              ((bytevector? r) (set! s (##sys#buffer->string/encoding r 0 (bytevector-length r) enc)) (set! i 0) (step port))
              ((eq? r 'none) 'none)
              (else #!eof)))))
      step))

  ; whether fd can be read now without blocking (it has data, or is at end of file); an error
  ; (an interrupted poll, say) counts as not readable, the reader then waits and tries again
  (define (%fd-readable? fd) (pair? (%fds-select (list (list fd 'input)) 0)))

  ; drain a stdio port without blocking the process: the descriptor is polled (with a 0 timeout) and
  ; read once, by one file-read, only when it is readable, which a pipe, tty or socket answers with
  ; what it has at once; nothing readable means "nothing left".  The descriptor's file status flags
  ; are never touched: O_NONBLOCK belongs to the open file description, which stdout and stderr (on
  ; the same tty or socket) and every child process that inherited it share, and they would get
  ; EAGAIN.  read-char alone would not do: stdio may read more than is ready and block, and it
  ; cannot stop in the middle of a UTF-8 sequence (a writer that split it across two writes).  So
  ; bytes are read from the descriptor itself and decoded here with the port's encoding (UTF-8 by
  ; %utf8-stepper, the single-byte ones by %bytes-stepper, as a direct read decodes them); the stdio
  ; buffer of a port that was read directly before (untouched? #f) is taken first, once, see
  ; %stdio-buffered-chars
  (define (%stream-drainer fd untouched? enc)
    (let ((step ((if (eq? 'utf-8 enc) %utf8-stepper (λ (fill) (%bytes-stepper fill enc)))
                  (λ (port)
                    (if (%fd-readable? fd)
                      (condition-case
                        (let* ((r (file-read fd 4096)) (n (cadr r)))
                          (and (positive? n)
                               (let1 (bv (make-bytevector n)) (bytevector-copy! bv 0 (car r) 0 n) bv)))
                        (e () (if (%eagain? e) 'none (abort e))))     ; another reader took it first
                      'none))))
          (pending (and untouched? '())))                    ; #f until the stdio buffer is taken
      (define (next port)
        (cond
          ((null? pending) (step port))
          ((eof-object? (car pending)) (car pending))
          (else (let1 (c (car pending)) (set! pending (cdr pending)) c))))
      (λ (port done?)
        (unless pending (set! pending (%stdio-buffered-chars port fd)))
        (%port-drain (τ (next port)) done?))))

  ; drain a socket port at byte level: its char-ready? only says that a byte is there, and a
  ; read-char on the lead byte of a sequence whose end has not arrived yet would block the process
  (define (%socket-drainer)
    (let1 (step (%utf8-stepper
                  (λ (port)
                    (let loop ((acc '()) (n 0))
                      (let1 (b (if (and (< n 4096) (char-ready? port)) (read-byte port) 'none))
                        (cond
                          ((fixnum? b) (loop (cons b acc) (add1 n)))
                          ((pair? acc) (apply bytevector (reverse acc)))
                          ((eq? b 'none) 'none)
                          (else #f)))))))
      (λ (port done?) (%port-drain (τ (step port)) done?))))

  ; drain a port whose char-ready? is exact (channel and custom ports).  When char-ready? or
  ; read-char raise (a channel port receiving a value that is not a string, say) after some chars
  ; were read, the drain ends with those chars, which go to the side buffer as usual, and the
  ; condition is raised by the next drain: the chars that came before the bad value are not lost
  (define (%ready-drainer)
    (let1 (deferred #f)
      (λ (port done?)
        (when deferred (let1 (e deferred) (set! deferred #f) (abort e)))
        (let1 (read? #f)
          (%port-drain (τ (condition-case (let1 (c (if (char-ready? port) (read-char port) 'none))
                                            (when (char? c) (set! read? #t))
                                            c)
                            (e () (if read? (begin (set! deferred e) 'none) (abort e)))))
                       done?)))))

  ; ports: per-port state ---------------------------------------------------------------------------

  ; The state kept per port (an input driver, the buffer of a channel output port, a write lock) is
  ; found in O(1), so that n open ports do not make every event on them O(n).  The ports without a
  ; descriptor carry theirs in the port itself, in its data slot, where nothing reads it: CHICKEN's
  ; custom ports (make-input-port, and so the channel ports made here) hold a vector #(#f) there
  ; (only a socket port's data is read, by port->fileno) and string ports nothing (#f), which is
  ; replaced by #(#f state).  Ports with a descriptor have theirs in a %port-table: weak pairs
  ; (port . state) in a bucket per descriptor (the port's, one entry but for ports sharing a
  ; descriptor); so do the custom ports whose data slot holds anything else (set by their maker
  ; with ##sys#set-port-data!), in a list looked up linearly, the only case not in O(1).  A dropped
  ; port is collected, and its entry, as that of a closed port, is purged when its bucket is next
  ; updated, or, in the list, by a full purge once the registrations since the previous one reach
  ; the length it left (as %clean-and-enqueue!)

  (define (%own-port-state port)
    (and (memq (%port-kind port) '(custom string))
         (let1 (d (##sys#slot port 9)) (and (vector? d) (= 2 (vector-length d)) (not (vector-ref d 0)) (vector-ref d 1)))))

  ; whether port's data slot is free for its state: #(#f) or #f as CHICKEN leaves it, or a state
  (define (%port-carries-state? port)
    (let1 (d (##sys#slot port 9))
      (and (memq (%port-kind port) '(custom string))
           (or (not d)
               (and (vector? d)
                    (<= 1 (vector-length d) 2)
                    (not (vector-ref d 0))
                    (or (= 1 (vector-length d))
                        (and (= 2 (vector-length d)) (let1 (x (vector-ref d 1)) (or (%pdriver? x) (%chan-out? x))))))))))

  (define (%own-port-state-set! port x) (##sys#setslot port 9 (vector #f x)))

  (define-record %port-table fds others budget)

  (define (%make-port-table) (make-%port-table (make-vector 64 '()) '() 0))

  ; the car of a weak pair (port . x) whose port was collected or closed
  (define (%port-gone? p) (or (bwp-object? p) (port-closed? p)))

  (define (%weak-assq port l)
    (cond ((null? l) #f) ((eq? port (caar l)) (cdar l)) (else (%weak-assq port (cdr l)))))

  (define (%weak-purge l) (remove (λ (w) (%port-gone? (car w))) l))

  ; fd is the port's descriptor, or #f
  (define (%port-table-ref tab port fd)
    (if fd
      (let1 (v (%port-table-fds tab)) (and (< fd (vector-length v)) (%weak-assq port (vector-ref v fd))))
      (%weak-assq port (%port-table-others tab))))

  (define (%port-table-set! tab port fd x)
    (if fd
      (let1 (v (%port-table-fds tab))
        (when (>= fd (vector-length v))
          (let1 (nv (make-vector (max (add1 fd) (* 2 (vector-length v))) '()))
            (do ((i 0 (add1 i))) ((= i (vector-length v))) (vector-set! nv i (vector-ref v i)))
            (%port-table-fds-set! tab nv)
            (set! v nv)))
        (vector-set! v fd (cons (weak-cons port x) (%weak-purge (vector-ref v fd)))))
      (let ((l (%port-table-others tab)) (b (%port-table-budget tab)))
        (if (positive? b)
          (begin (%port-table-budget-set! tab (sub1 b)) (%port-table-others-set! tab (cons (weak-cons port x) l)))
          (let1 (l (%weak-purge l))
            (%port-table-budget-set! tab (max %clean-budget-min (length l)))
            (%port-table-others-set! tab (cons (weak-cons port x) l)))))))

  ; ports: waiting for a close -----------------------------------------------------------------------

  ; A reader or writer waiting on a port's descriptor registers the port here while it waits, and
  ; one sweep, every %port-closed-check-ms, wakes those whose port another thread closed
  ; meanwhile (its descriptor number may already belong to another file, which may never become
  ; ready).  An idle waiter costs nothing but its share of that sweep, which is skipped when no
  ; thread ran since the previous one (a timeout per waiter, re-armed at every check, made a few
  ; thousand idle readers keep the process busy).  Entries are #(port ivar tid active?); the
  ; ivar is put when the port is found closed; an entry leaves the list at the first sweep after
  ; its waiter is done (unwatched, or dead)
  (define %close-watch '())
  (define %close-watch-active 0)          ; the number of active entries
  (define %close-watch-run #f)            ; the run-cml session of the entries
  (define %close-watch-next 0)            ; when the next sweep is due, in %now-ms time

  (define (%close-watch! port)
    (unless (and (eq? %close-watch-run %run-id) (positive? %close-watch-active))
      (set! %close-watch '())
      (set! %close-watch-active 0)
      (set! %close-watch-run %run-id)
      (set! %close-watch-next (+ (%now-ms) %port-closed-check-ms)))
    (let1 (w (vector port (make-ivar) %cur-tid #t))
      (set! %close-watch (cons w %close-watch))
      (set! %close-watch-active (add1 %close-watch-active))
      w))

  (define (%close-unwatch! w)
    (when (vector-ref w 3)
      (vector-set! w 3 #f)
      (set! %close-watch-active (sub1 %close-watch-active))))

  ; commits once the port of w is found closed
  (define (%close-watch-evt w) (ivar-get-evt (vector-ref w 1)))

  (define (%close-watching?) (and (eq? %close-watch-run %run-id) (positive? %close-watch-active)))

  ; the sweep, an os poller (run at every preemption tick and while idle): the idle scheduler
  ; sleeps at most until it is due.  A sweep is needed only when some thread ran since the last
  ; one (only a thread can close a port): once idle, the scheduler sleeps after one more sweep
  ; until a descriptor or a timeout wakes a thread, whatever the number of waiters
  (define %close-watch-switches -1)       ; %switch-count at the last sweep

  (define (%close-sweep-needed?)
    (and (%close-watching?) (not (= %switch-count %close-watch-switches))))

  (define (%close-sweep!)
    (when (and (%close-sweep-needed?) (>= (%now-ms) %close-watch-next))
      (set! %close-watch-next (+ (%now-ms) %port-closed-check-ms))
      (set! %close-watch-switches %switch-count)
      (set! %close-watch
        (filter (λ (w)
                  (and (vector-ref w 3)
                       (cond
                         ((%tid-dead? (vector-ref w 2)) (%close-unwatch! w) #f)
                         ((port-closed? (vector-ref w 0))
                           (%close-unwatch! w)
                           (let1 (iv (vector-ref w 1)) (%enqueue-tmp-thread! (τ (ivar-put! iv #t))))
                           #f)
                         (else #t))))
                %close-watch))))

  (%add-os-poller! "port-close-watch" %close-sweep! (τ (and (%close-sweep-needed?) (- %close-watch-next (%now-ms)))))

  ; ports: per-port input state ---------------------------------------------------------------------

  ; drain: (port done? -> (values chars-in-reverse eof? more?), see %port-drain); wait: a thunk
  ; giving the event to wait on when the drain is not enough, whose value is a thunk the reader runs
  ; where its errors are caught (a channel port receives its data there); buffer, chunks and eof:
  ; characters read but not delivered yet, the string buffer followed by the strings of the list
  ; chunks (newest first, not appended yet so that input arriving in many small pieces is not copied
  ; again at every piece), size characters in all; lock: an mvar full when free, re-created for
  ; every run-cml session (a helper of a previous run may have died holding it).  A driver never
  ; refers to its port, and its closures are made where the port is not in scope (the interpreter's
  ; closures keep their whole environment)
  (define-record %pdriver drain wait buffer chunks size eof lock run)

  ; the drivers of the ports that are not channel ports (see %port-table)
  (define %pdrivers (%make-port-table))

  (define (%new-pdriver drain wait) (make-%pdriver drain wait "" '() 0 #f (make-mvar (void)) %run-id))

  (define (%pdriver-set-buffer! d s) (%pdriver-buffer-set! d s) (%pdriver-chunks-set! d '()) (%pdriver-size-set! d (string-length s)))

  (define (%pdriver-add! d s)
    (%pdriver-chunks-set! d (cons s (%pdriver-chunks d)))
    (%pdriver-size-set! d (+ (%pdriver-size d) (string-length s))))

  ; the whole side buffer as one string (not with apply: CHICKEN's apply hangs on some 32000
  ; arguments, and there is a chunk per piece of input)
  (define (%pdriver-text d)
    (unless (null? (%pdriver-chunks d))
      (%pdriver-set-buffer! d (call-with-output-string
                                (λ (o)
                                  (display (%pdriver-buffer d) o)
                                  (for-each (λ (s) (display s o)) (reverse (%pdriver-chunks d)))))))
    (%pdriver-buffer d))

  ; the last character of the side buffer, or #f
  (define (%pdriver-last d)
    (let1 (s (if (null? (%pdriver-chunks d)) (%pdriver-buffer d) (car (%pdriver-chunks d))))
      (let1 (n (string-length s)) (and (positive? n) (string-ref s (sub1 n))))))

  (define (%fd-wait fd) (τ (wrap (io-evt fd 'input) (λ ignored void))))
  (define (%poll-wait) (wrap (timeout-evt %port-poll-secs) (λ ignored void)))

  (define (%port-driver/default port)
    (let1 (fd (%port-fileno port))
      (cond
        ((not fd) (%new-pdriver (%ready-drainer) %poll-wait))
        ((eq? 'stream (%port-kind port))
          (%new-pdriver (%stream-drainer fd (%port-untouched? port) (##sys#slot port 15)) (%fd-wait fd)))
        (else (%new-pdriver (%socket-drainer) (%fd-wait fd))))))

  (define (%port-driver port)
    (let1 (d (let1 (x (%own-port-state port))
               (cond
                 ((%pdriver? x) x)
                 ((%port-carries-state? port) #f)
                 (else (%port-table-ref %pdrivers port (%port-fileno port))))))
      (cond
        ((not d) (let1 (d (%port-driver/default port))
                   (if (%port-carries-state? port)
                     (%own-port-state-set! port d)
                     (%port-table-set! %pdrivers port (%port-fileno port) d))
                   d))
        ((eq? %run-id (%pdriver-run d)) d)
        (else (%pdriver-lock-set! d (make-mvar (void))) (%pdriver-run-set! d %run-id) d))))

  ; one step of a reader: the value is available (list v buffer' eof'), #f (wait for more input) or
  ; 'more (a drain read as much as it may at once, more may be there already: step again, after a
  ; yield); take maps (buffer eof?) to that, need maps the size and the last char (or #f) of the
  ; buffer to the done? predicate of a drain; the drain is repeated as long as it reads something
  ; and take is not satisfied, but a drain that stopped at its bound ends the step ('more).  A take
  ; that fails on a buffer keeps failing as characters are added until one of them satisfies done?
  ; or the end of file comes (every take/need pair keeps to that), so take is tried only then, or
  ; when check? (the first step of a reader): the buffer is not made into one string, nor scanned,
  ; at every piece of input.  A need that is the symbol available (input-evt) asks for the whole
  ; drain instead, take being tried after every drain that read something, the bounded one
  ; included.  A closed port raises (its descriptor number may belong to another file by now)
  (define (%pdriver-step! d port take need check?)
    (let loop ((check? check?))
      (when (port-closed? port) (error 'input-evt "port is closed" port))
      (or (and (or check? (%pdriver-eof d)) (take (%pdriver-text d) (%pdriver-eof d)))
          (and (not (%pdriver-eof d))
               (let* ((hit #f)
                      (need (need (%pdriver-size d) (%pdriver-last d)))
                      (available? (eq? 'available need))
                      (done? (if available? (λ (c) #f) (λ (c) (and (need c) (begin (set! hit #t) #t))))))
                 (receive (acc eof? more?) ((%pdriver-drain d) port done?)
                   (unless (null? acc) (%pdriver-add! d (list->string (reverse acc))))
                   (when eof? (%pdriver-eof-set! d #t))
                   (if (and more? (not available?)) 'more (and (or eof? (pair? acc)) (loop (or hit available?))))))))))

  ; the helper thread (TextIO's inputThread): serializes readers with the lock, reads without blocking
  ; the process, offers the value or the condition on reply, and gives up on the nack.  Between two
  ; bounded drains ('more) it checks the nack and yields with a preemption (%yield/poll!), so a
  ; stream that keeps it busy neither freezes the other threads and the timeouts, nor keeps it
  ; reading once its sync went another way.  The side
  ; buffer is updated by the commit itself (the make-msg of the reply runs atomically before the
  ; receiver gets the value), so a reader that leaves the session right away loses nothing
  (define (%port-reader d port take need nack reply)
    (let1 (lock (%pdriver-lock d))
      (define (release) (mvar-put! lock (void)))
      (define (offer msg commit!)
        (select (wrap (%send-evt/commit reply (τ (commit!) msg)) (λ ignored (release)))
                (wrap nack (λ ignored (release)))))
      ; absorb: the value of the wait event
      (define (loop absorb check?)
        (let1 (r (condition-case (begin (absorb) (cons 'ok (%pdriver-step! d port take need check?))) (e () (cons 'exn e))))
          (cond
            ((eq? 'exn (car r)) (offer r void))
            ((eq? 'more (cdr r))
              (if (sync/timeout (wrap nack (λ ignored #t)) 0 #f)
                (release)
                (begin (%yield/poll!) (loop void #f))))
            ((cdr r) (let1 (t (cdr r))
                       (offer (cons 'ok (car t)) (τ (%pdriver-set-buffer! d (cadr t)) (%pdriver-eof-set! d (caddr t))))))
            (else (wait)))))
      ; the wait on a descriptor also ends when another thread closes the port (see %close-watch!):
      ; the descriptor may have been closed and its number handed to another file, whose readiness
      ; says nothing about the port.  Other ports need no watch: a channel port only waits on its
      ; channel (so a reader blocked on it is a deadlock, as in ML), a custom port polls anyway
      (define (wait)
        (if fd?
          (let1 (w (%close-watch! port))
            (select (wrap ((%pdriver-wait d)) (λ (absorb) (%close-unwatch! w) (loop absorb #f)))
                    (wrap (%close-watch-evt w) (λ ignored (loop void #f)))
                    (wrap nack (λ ignored (%close-unwatch! w) (release)))))
          (select (wrap ((%pdriver-wait d)) (λ (absorb) (loop absorb #f)))
                  (wrap nack (λ ignored (release))))))
      (define fd? (and (%port-fileno port) #t))
      ; a helper whose sync went another way before it ran (see %port-input-evt) leaves at once
      (unless (sync/timeout (wrap nack (λ ignored #t)) 0 #f)
        (select (wrap (mvar-take-evt lock) (λ ignored (loop void #t))) nack))))

  ; a thread that runs thunk once the current one blocks or yields (spawn runs its child first)
  (define (%spawn/queued! thunk)
    (let1 (id (%new-tid))
      (%enqueue! (cons id (%isolated (τ (handle-exceptions e (%thread-died! id e) (thunk))
                                        (%notify-and-dispatch id)))))
      id))

  ; the input event's own fast path, polled by the syncing thread: when the port's lock is free (no
  ; helper is reading) and the side buffer, completed by a drain for the ports whose drain never
  ; runs user code nor CML operations (stdio, tcp and string ports), satisfies take, the event is
  ; enabled and its commit updates the side buffer itself; a drain that raises gives the condition
  ; as the event's value, as the helper does.  Without it, a poll (sync/timeout with 0 seconds)
  ; found the value only once the helper, run first, had offered it: when the helper was preempted
  ; while it held the lock, it waited in rdyQ2 and every poll until the next preemption missed data
  ; that was there.  The commit takes the value from the buffer as it is by then, as a drain by
  ; another branch of the same sync may have added to it (never taking anything away).  Its
  ; priority is the one the helper's offer on a new reply channel has
  (define (%port-ready-evt d port take need)
    (define drain? (memq (%port-kind port) '(stream socket string)))
    (define (commit)
      (let1 (t (take (%pdriver-text d) (%pdriver-eof d)))
        (%pdriver-set-buffer! d (cadr t))
        (%pdriver-eof-set! d (caddr t))
        (%atomic-end)
        (cons 'ok (car t))))
    (wrap (%base-evt
            (τ (let1 (r (and (not (eq? %empty (%cell-value (%pdriver-lock d))))
                             (condition-case
                               (if drain?
                                 (%pdriver-step! d port take need #t)
                                 (take (%pdriver-text d) (%pdriver-eof d)))
                               (e () (vector e)))))
                 (cond
                   ((vector? r) (make-%enabled 1 (τ (%atomic-end) (cons 'exn (vector-ref r 0)))))
                   ((pair? r) (make-%enabled 1 commit))
                   (else (make-%blocked (λ (trans cleanup next) (next))))))))
          %result-value))

  ; a closed port raises at sync, as a direct read on it does (and the reader checks it again
  ; before every drain, another thread may close the port while it waits).  The helper is queued
  ; rather than run first, so that the fast path is polled before it may take the lock; when the
  ; fast path commits, the nack tells the helper to leave
  (define (%port-input-evt who port take need)
    (unless (input-port? port) (error who "not an input port" port))
    (guard
      (τ (when (port-closed? port) (error who "port is closed" port))
         (let1 (d (%port-driver port))
           (choose
             (%port-ready-evt d port take need)
             (with-nack
               (λ (nack)
                 (let1 (reply (make-channel))
                   (%spawn/queued! (τ (%port-reader d port take need nack reply)))
                   (wrap (recv-evt reply) %result-value)))))))))

  ; ports: input events (TextIO.input1Evt / inputNEvt / inputEvt / inputAllEvt, StreamIO.inputLineEvt)

  (define (%need-one size last) (λ (c) #t))

  ; TextIO.input1Evt: the next char, or #!eof
  (define (input-char-evt port)
    (%port-input-evt 'input-char-evt port
      (λ (buf eof?)
        (cond
          ((positive? (string-length buf)) (list (string-ref buf 0) (substring buf 1) eof?))
          (eof? (list #!eof "" #f))
          (else #f)))
      %need-one))

  ; the next char without consuming it, or #!eof (the end of file stays pending)
  (define (peek-char-evt port)
    (%port-input-evt 'peek-char-evt port
      (λ (buf eof?)
        (cond
          ((positive? (string-length buf)) (list (string-ref buf 0) buf eof?))
          (eof? (list #!eof "" #t))
          (else #f)))
      %need-one))

  ; the index of the first line terminator of s (#\newline or #\return) or #f
  (define (%line-end s)
    (let1 (n (string-length s))
      (let loop ((i 0))
        (cond ((= i n) #f) ((memv (string-ref s i) '(#\newline #\return)) i) (else (loop (add1 i)))))))

  ; TextIO.inputLine, with read-line's conventions: a line ends at "\n", "\r\n" or "\r" and comes
  ; without its terminator, the last unterminated line comes at end of file, then #!eof.  A "\r"
  ; that ends the available input waits for the next char (or end of file) to know if "\n" follows
  (define (input-line-evt port)
    (%port-input-evt 'input-line-evt port
      (λ (buf eof?)
        (let ((i (%line-end buf)) (n (string-length buf)))
          (cond
            ((and i (char=? #\newline (string-ref buf i))) (list (substring buf 0 i) (substring buf (add1 i)) eof?))
            ((and i (< (add1 i) n))
              (let1 (j (if (char=? #\newline (string-ref buf (add1 i))) (+ i 2) (add1 i)))   ; "\r\n" or "\r"
                (list (substring buf 0 i) (substring buf j) eof?)))
            (i (and eof? (list (substring buf 0 i) "" #t)))
            ((not eof?) #f)
            ((zero? n) (list #!eof "" #f))
            (else (list buf "" #t)))))
      ; a drain stops at a line terminator, and after one char when the buffer ends with "\r"
      (λ (size last)
        (if (eqv? last #\return)
          (λ (c) #t)
          (λ (c) (or (char=? c #\newline) (char=? c #\return)))))))

  ; TextIO.inputNEvt: n chars, fewer only at end of file, #!eof when nothing is left
  (define (input-string-evt port n)
    (unless (and (exact-integer? n) (>= n 0)) (error 'input-string-evt "bad count" n))
    (%port-input-evt 'input-string-evt port
      (λ (buf eof?)
        (let1 (len (string-length buf))
          (cond
            ((zero? n) (list "" buf eof?))
            ((>= len n) (list (substring buf 0 n) (substring buf n) eof?))
            ((not eof?) #f)
            ((zero? len) (list #!eof "" #f))
            (else (list buf "" #t)))))
      (λ (size last) (let1 (k (- n size)) (λ (c) (set! k (sub1 k)) (<= k 0))))))

  ; TextIO.inputEvt: as soon as a char is there, every char available by then (what the side
  ; buffer holds and what one drain reads, at most 4096 chars more); #!eof at end of file, which
  ; is only returned once the chars before it have been
  (define (input-evt port)
    (%port-input-evt 'input-evt port
      (λ (buf eof?)
        (cond
          ((positive? (string-length buf)) (list buf "" eof?))
          (eof? (list #!eof "" #f))
          (else #f)))
      (λ (size last) 'available)))

  ; TextIO.inputAllEvt: everything up to end of file, "" when nothing is left
  (define (input-all-evt port)
    (%port-input-evt 'input-all-evt port
      (λ (buf eof?) (and eof? (list buf "" #f)))
      (λ (size last) (λ (c) #f))))

  ; ports: output events -----------------------------------------------------------------------------

  ; the state of a channel output port (see open-channel-output-port), or #f; a state never refers
  ; to its port
  (define (%chan-out-state port) (let1 (x (%own-port-state port)) (and (%chan-out? x) x)))

  ; the output events check at every sync that their port is still open (a closed channel output
  ; port has sent its #!eof and must not send more): a closed port raises, as a write on it does
  (define (%open-output-guard who port thunk)
    (guard (τ (if (port-closed? port) (error who "port is closed" port) (thunk)))))

  ; commit when port can take output without blocking the process; the value is port
  (define (output-evt port)
    (unless (output-port? port) (error 'output-evt "not an output port" port))
    (%open-output-guard 'output-evt port
      (τ (let1 (fd (and (not (%chan-out-state port)) (%port-fileno port)))
           (if fd
             (wrap (io-evt fd 'output) (λ ignored port))
             (always-evt port))))))

  ; the write locks of descriptor-backed ports, #(mvar run) in a %port-table, the mvar full when
  ; free and re-created for every run-cml session (a writer of a previous run may have died holding it)
  (define %port-write-locks (%make-port-table))

  (define (%port-write-lock port fd)
    (let1 (l (%port-table-ref %port-write-locks port fd))
      (cond
        ((and l (eq? %run-id (vector-ref l 1))) (vector-ref l 0))
        (l (vector-set! l 0 (make-mvar (void))) (vector-set! l 1 %run-id) (vector-ref l 0))
        (else (let1 (v (vector (make-mvar (void)) %run-id))
                (%port-table-set! %port-write-locks port fd v)
                (vector-ref v 0))))))

  ; wait until fd, the descriptor of port, can take output, raising once port is closed: the wait
  ; also ends when another thread closes port (see %close-watch!), since the number of a
  ; descriptor that another thread closed may belong to another file by then, whose readiness says
  ; nothing about port (and which must not get port's output)
  (define (%port-output-wait port fd)
    (unless (port-closed? port)
      (let1 (w (%close-watch! port))
        (sync (choose (wrap (io-evt fd 'output) (λ ignored (%close-unwatch! w)))
                      (%close-watch-evt w)))))
    (when (port-closed? port) (error 'write-string-evt "port is closed" port)))

  ; write s on the descriptor-backed port in chunks, each one after a readiness wait, and flush.
  ; The chunks are small on a blocking descriptor (a pipe or tty), which a write of more than
  ; what it can take at once would block for the whole process, and large on a non-blocking one
  ; (a tcp socket), which %port-write! writes as far as it goes and waits for the rest: small
  ; writes on a socket would be small segments, and Nagle's algorithm holds each one after the
  ; first until the peer's delayed ACK, some 40 ms per message in a request/response protocol.
  ; The port's write lock is held from the first chunk to the last, so the chunks of two
  ; write-string-evts on one port never interleave (TextIO.output holds the stream lock as well);
  ; when the lock was not free the readiness seen at the commit is stale and is waited for again.
  ; A writer that dies holding the lock (a dynamic-wind thunk raising as it is switched in or out
  ; between two chunks, outside handle-exceptions) gives it back too (see %release-holds!)
  (define (%fd-nonblocking? fd)
    (condition-case (not (zero? (bitwise-and (file-control fd fcntl/getfl) open/nonblock))) (ignored () #f)))

  (define (%port-write-chunks! port fd s first-ready?)
    (let* ((lock (%port-write-lock port fd))
           (chunk (if (%fd-nonblocking? fd) %port-write-chunk/nonblock %port-write-chunk))
           (ready? (and first-ready? (%hold-take-poll! lock))))
      (unless ready? (%hold-take! lock))
      (handle-exceptions e
        (begin (%hold-put! lock) (abort e))
        (let1 (n (string-length s))
          (let loop ((i 0) (ready? ready?))
            (if ready?
              (when (port-closed? port) (error 'write-string-evt "port is closed" port))
              (%port-output-wait port fd))
            (let1 (j (min n (+ i chunk)))
              (%port-write! port fd (substring s i j))
              (when (< j n) (loop j #f))))))
      (%hold-put! lock)))

  ; the bytes of s in the encoding enc of a stdio port, as a write on the port encodes them (with
  ; the encoder CHICKEN registered for enc: latin-1 keeps the low byte of each char, binary and
  ; UTF-8 give UTF-8)
  (define (%encode-string s enc)
    (let1 (bv (string->utf8 s))
      (if (eq? 'utf-8 enc)
        bv
        (##sys#encoding-hook enc (λ (dec encode scan)
                                   (encode bv 0 (bytevector-length bv) (λ (buf start n) (bytevector-copy buf start (+ start n)))))))))

  ; write s on the descriptor-backed port and flush it.  A stdio port is written through its
  ; descriptor (its stdio buffer flushed first), s encoded with the port's encoding, because
  ; CHICKEN's stdio flush drops write errors: and since SIGPIPE is ignored once (chicken tcp) is
  ; loaded (by this module), a write to a pipe whose reader has exited would lose the data
  ; silently, so a writer loop would never stop.  Here it raises the i/o condition of file-write
  ; (errno EPIPE), as ML's output raises Io, whatever the encoding.  A partial write (a descriptor
  ; made non-blocking) waits for readiness again.  A tcp port is written through its descriptor
  ; too (its own buffer flushed first), which is non-blocking: its own write would wait for a full
  ; socket to drain by blocking the process, and a whole chunk goes out in one send
  (define (%port-write! port fd s)
    (if (memq (%port-kind port) '(stream socket))
      (begin
        (flush-output port)
        (let loop ((bv (%encode-string s (##sys#slot port 15))))
          (let1 (n (condition-case (file-write fd bv) (e () (if (%eagain? e) 0 (abort e)))))
            (when (< n (bytevector-length bv))
              (%port-output-wait port fd)
              (loop (bytevector-copy bv n))))))
      (begin (display s port) (flush-output port))))

  ; commit when port is writable, then write s (blocking only the syncing thread); on a channel output
  ; port it is the rendezvous that sends the buffered output followed by s, the buffer being taken
  ; at the commit itself (so a flush by another thread meanwhile is never sent twice), and when
  ; there is nothing to send it commits at once, sending nothing (see %chan-out-send-evt)
  (define (write-string-evt port s)
    (unless (string? s) (error 'write-string-evt "not a string" s))
    (unless (output-port? port) (error 'write-string-evt "not an output port" port))
    (%open-output-guard 'write-string-evt port
      (τ (let1 (st (%chan-out-state port))
           (if st
             (%chan-out-send-evt st s)
             (let1 (fd (%port-fileno port))
               (if fd
                 (wrap (io-evt fd 'output) (λ ignored (%port-write-chunks! port fd s #t)))
                 (wrap (always-evt port) (λ ignored (display s port) (flush-output port))))))))))

  ; ChanIO (chan-io-fn.sml): ports over channels -----------------------------------------------------

  ; input side: pending is the string being consumed from index i, eof is sticky
  (define-record %chan-in ch pending i eof)

  (define (%chan-in-push! st v)
    (cond
      ((eof-object? v) (%chan-in-eof-set! st #t))
      ((or (string? v) (char? v))
        (let1 (rest (substring (%chan-in-pending st) (%chan-in-i st)))
          (%chan-in-pending-set! st (string-append rest (if (char? v) (string v) v)))
          (%chan-in-i-set! st 0)))
      (else (error 'open-channel-input-port "expected a string, a char or #!eof" v))))

  (define (%chan-in-available? st) (< (%chan-in-i st) (string-length (%chan-in-pending st))))

  (define (%chan-in-read-char! st)
    (let loop ()
      (cond
        ((%chan-in-available? st)
          (let1 (i (%chan-in-i st)) (%chan-in-i-set! st (add1 i)) (string-ref (%chan-in-pending st) i)))
        ((%chan-in-eof st) #!eof)
        (else (%chan-in-push! st (recv (%chan-in-ch st))) (loop)))))

  ; the next char without consuming it: CHICKEN's own peek buffer would hide it from char-ready?
  (define (%chan-in-peek-char st)
    (let loop ()
      (cond
        ((%chan-in-available? st) (string-ref (%chan-in-pending st) (%chan-in-i st)))
        ((%chan-in-eof st) #!eof)
        (else (%chan-in-push! st (recv (%chan-in-ch st))) (loop)))))

  (define (%chan-in-ready? st)
    (or (%chan-in-available? st)
        (%chan-in-eof st)
        (let1 (o (recv-poll (%chan-in-ch st)))
          (and (pair? o) (begin (%chan-in-push! st (car o)) (%chan-in-ready? st))))))

  ; the value received is pushed by the reader, where an error (a bad value) is caught and offered
  (define (%chan-in-driver ch st)
    (%new-pdriver (%ready-drainer) (τ (wrap (recv-evt ch) (λ (v) (τ (%chan-in-push! st v)))))))

  ; ChanIO.mkReader as an input port: reads strings (or chars) from ch until #!eof; a read on an empty
  ; port blocks the calling CML thread only; input events on it wait on the channel
  (define (open-channel-input-port ch)
    (let* ((st (make-%chan-in ch "" 0 #f))
           (port (make-input-port (τ (%chan-in-read-char! st)) (τ (%chan-in-ready? st)) void
                                  peek-char: (τ (%chan-in-peek-char st)))))
      (%own-port-state-set! port (%chan-in-driver ch st))
      port))

  ; output side: chunks is the reversed list of the strings written since the last flush;
  ; flushers the blocked sends that carry the buffer only (#(trans cleanup k), see %chan-out-send-evt)
  (define-record %chan-out ch chunks size flushers)

  (define (%chan-out-contents st) (apply string-append (reverse (%chan-out-chunks st))))

  (define %chan-out-flushed (list 'flushed))

  ; the buffered output, emptying the buffer; called atomic, at the commit of a send.  The blocked
  ; sends that were to carry the buffer only have nothing left to send: they commit now, sending
  ; nothing (ML's writer never sends an empty vector on its channel)
  (define (%chan-out-take! st)
    (let1 (s (%chan-out-contents st))
      (%chan-out-chunks-set! st '())
      (%chan-out-size-set! st 0)
      (let1 (fs (%chan-out-flushers st))
        (%chan-out-flushers-set! st '())
        (for-each (λ (w)
                    (when (%trans-live? (vector-ref w 0))
                      (let ((tid (%get-id-from-trans! (vector-ref w 0))) (k (vector-ref w 2)))
                        (%enqueue-thread! tid (λ ignored (k %chan-out-flushed)))
                        ((vector-ref w 1)))))
                  fs))
      s))

  ; the send of the buffered output followed by s, on a channel output port: the buffer is taken at
  ; the commit, so whichever of two sends (a write-string-evt and a flush of another thread, say)
  ; commits first carries everything written so far, and a thread's output keeps its order.  An
  ; empty string never reaches the channel, as in ML's writer: with nothing buffered and s empty it
  ; commits at once, and a blocked send whose s is empty commits without sending once another
  ; send has taken the buffer (see %chan-out-take!)
  (define (%chan-out-send-evt st s)
    (let ((ch (%chan-out-ch st)) (only-buffer? (zero? (string-length s))))
      (define (make-msg) (string-append (%chan-out-take! st) s))
      (%base-evt
        (τ (if (and only-buffer? (null? (%chan-out-chunks st)))
             (make-%enabled -1 (τ (%atomic-end) (void)))
             (let1 (p (%clean-and-chk! ch (%channel-in-q ch)))
               (if (zero? p)
                 (make-%blocked
                   (λ (trans cleanup next)
                     (let1 (r (%letcc/call send-k
                                (%clean-and-enqueue! (%channel-out-q ch) (cons trans send-k))
                                (when only-buffer?
                                  (%chan-out-flushers-set! st (cons (vector trans cleanup send-k)
                                                                    (filter (λ (w) (%trans-live? (vector-ref w 0)))
                                                                            (%chan-out-flushers st)))))
                                (next)
                                (%impossible 'write-string-evt)))
                       (unless (eq? r %chan-out-flushed)
                         (cleanup)
                         (%partner-switch-to! r (make-msg))))))
                 (make-%enabled p (τ (let ((msg (make-msg)) (item (%q-dequeue! (%channel-in-q ch))))
                                       (%letcc/call send-k
                                         (%enqueue-and-switch-cur-thread! send-k (%get-id-from-trans! (car item)))
                                         (%channel-priority-set! ch 1)
                                         (%switch! (cdr item) msg))
                                       (void)))))))))))

  (define (%chan-out-flush! st)
    (unless (null? (%chan-out-chunks st))
      (sync (%chan-out-send-evt st ""))))

  (define (%chan-out-write! st s)
    (unless (zero? (string-length s))
      (%chan-out-chunks-set! st (cons s (%chan-out-chunks st)))
      (%chan-out-size-set! st (+ (%chan-out-size st) (string-length s)))
      (when (>= (%chan-out-size st) %chan-port-chunk) (%chan-out-flush! st))))

  ; ChanIO.mkWriter as an output port: flush-output sends what was written as one string, close
  ; flushes and sends #!eof; the sends block the calling CML thread until a receiver takes them
  (define (open-channel-output-port ch)
    (let* ((st (make-%chan-out ch '() 0 '()))
           (port (make-output-port (λ (s) (%chan-out-write! st s))
                                   (τ (%chan-out-flush! st) (send ch #!eof))
                                   force-output: (τ (%chan-out-flush! st)))))
      (%own-port-state-set! port st)
      port))

  ; OS.Process.systemEvt / Unix.execute (Unix/os-process.sml, Unix/new-unix.sml) ----------------------

  ; process-evt's (normal? code) as a shell exit status
  (define (%status->code st) (if (car st) (cadr st) (+ 128 (cadr st))))

  ; a child process running (process-execute cmd args env) once (setup) has run in it, exiting with
  ; status failed when the exec fails; env is #f (the parent's environment) or an alist.  SIGPIPE is ignored in this process ((chicken tcp) ignores
  ; it when loaded) and an ignored signal stays ignored across exec: it is set back to its default
  ; first, as the child of an ML program starts with it (a shell pipeline such as `producer | head
  ; -1` would otherwise never end, or report broken pipes)
  (define (%fork-exec cmd args failed setup #!optional env)
    (process-fork (τ (setup)
                     (signal-default signal/pipe)
                     (condition-case (process-execute cmd args env) (ignored () (emergency-exit failed))))))

  ; an environment given as ML does, a list of "NAME=value" strings, as the alist of process-execute
  (define (%env->alist who env)
    (map (λ (b)
           (let1 (i (and (string? b) (%string-index b #\=)))
             (unless i (error who "bad environment binding, not \"NAME=value\"" b))
             (cons (substring b 0 i) (substring b (add1 i)))))
         env))

  ; OS.Process.systemEvt: cmd runs through /bin/sh at once (not at sync time), the value is the exit
  ; status (0 is success, 127 when /bin/sh cannot be run, as in ML); syncing again gives the same
  ; status.  /bin/sh as in ML, not the user's $SHELL that (process-run cmd) would use
  (define (system-evt cmd)
    (unless (string? cmd) (error 'system-evt "not a string" cmd))
    (%check-running 'system-evt)
    (let1 (pid (%fork-exec "/bin/sh" (list "-c" cmd) 127 void))
      (wrap (process-evt pid) %status->code)))

  ; OS.Process.system
  (define (cml/system cmd) (sync (system-evt cmd)))

  ; FD_CLOEXEC, which (chicken file posix) does not export
  (define %fd-cloexec 1)

  (define (%set-cloexec! fd) (file-control fd fcntl/setfd (bitwise-ior (file-control fd fcntl/getfd) %fd-cloexec)))

  ; Unix.execute: (values in out pid), in reads the child's stdout and out writes its stdin, both
  ; descriptor-backed so the port events apply; (process-evt pid) reaps it (Unix.reapEvt).  As in
  ; new-unix.sml the parent's ends are close-on-exec: children started later do not inherit them
  ; (a child keeping the write end of another one's stdin would never let it see end of file), and
  ; a child whose exec fails exits with status 128.  Unlike new-unix.sml (execve with the basename
  ; of cmd as argv[0]), the exec is CHICKEN's process-execute: a cmd without a "/" is searched in
  ; PATH (ML's exec fails on it) and argv[0] is cmd as given (the interpreted module has no FFI to
  ; set it apart).  env is Unix.executeInEnv's: #f (the default) passes this process's environment
  ; (Unix.execute), a list of "NAME=value" strings is the child's whole environment, and then cmd is
  ; exec'd as given, without a PATH search, as ML does.  In the child, the pipe ends are first moved
  ; above 2 (the parent may have closed its stdin or stdout, so that create-pipe handed out 0 or 1),
  ; then copied onto 0 and 1 and closed
  (define (cml/execute cmd #!optional (args '()) env)
    (define alist
      (begin
        (unless (string? cmd) (error 'cml/execute "not a string" cmd))
        (unless (and (list? args) (every string? args)) (error 'cml/execute "not a list of strings" args))
        (and env (%env->alist 'cml/execute env))))
    (receive (child-in parent-out) (create-pipe)
      (receive (parent-in child-out) (create-pipe)
        (%set-cloexec! parent-in)
        (%set-cloexec! parent-out)
        (let1 (pid (%fork-exec cmd args 128
                     (τ (let ((in (file-control child-in fcntl/dupfd 3))
                              (out (file-control child-out fcntl/dupfd 3)))
                          (for-each (λ (fd) (when (> fd 1) (file-close fd)))
                                    (list child-in child-out parent-in parent-out))
                          (duplicate-fileno in 0)
                          (duplicate-fileno out 1)
                          (file-close in)
                          (file-close out)))
                     alist))
          (file-close child-in)
          (file-close child-out)
          (values (open-input-file* parent-in) (open-output-file* parent-out) pid)))))

  ; tcp (Sockets/cml-socket.sml, over (chicken tcp)) -------------------------------------------------

  ; Socket.acceptEvt: the value is (list in out); the connection is accepted after the commit only
  (define (tcp-accept-evt listener)
    (unless (tcp-listener? listener) (error 'tcp-accept-evt "not a tcp listener" listener))
    (let1 (fd (tcp-listener-fileno listener))
      (define (accept) (receive (in out) (tcp-accept listener) (list in out)))
      ; the readiness seen by the guard or io-evt may be gone by the time the wrap runs (another
      ; acceptor may have run in between, e.g. after a preemption at the commit): check again, as a
      ; tcp-accept on an empty backlog would block the whole process
      (define (accept-or-wait) (if (tcp-accept-ready? listener) (accept) (sync evt)))
      (define evt
        (guard (τ (if (tcp-accept-ready? listener)
                    (wrap (always-evt #t) (λ ignored (accept-or-wait)))
                    (wrap (io-evt fd 'input) (λ ignored (accept-or-wait)))))))
      evt))

  ; Socket.connectEvt: the value is (list in out); the connection is made after the commit and the
  ; handshake blocks the process, (chicken tcp) having no non-blocking connect
  (define (tcp-connect-evt host #!optional port)
    (wrap (always-evt #t)
          (λ ignored (receive (in out) (if port (tcp-connect host port) (tcp-connect host)) (list in out)))))

  ; cml-lib ========================================================================================

  ;; cml-lib (smlnj/libraries/cml/cml-lib): Multicast, SimpleRPC and TraceCML, built on the core above.
  ;;
  ;; Deviations from ML, all deliberate:
  ;; - Multicast has no server thread: `multicast!` appends to the stream of ivars itself, which is
  ;;   atomic because the scheduler only switches at CML operations, and then yields (promoting a
  ;;   thread from rdyQ2), which keeps a producer to the pace of its readers as ML's rendezvous with
  ;;   the server does; ports keep ML's tee thread, so several readers of one port share its
  ;;   messages exactly as in ML.  A port needs a running CML (it spawns its tee) and belongs to
  ;;   that run-cml session; the channel itself does not.
  ;; - SimpleRPC answers with a Result: when f raises, the caller of `call` gets the exception (ML
  ;;   leaves it blocked forever).  The server goes on with make-rpc, make-rpc/in, make-rpc/in-out
  ;;   (whose entry event then yields the old state) and make-rpc/state; make-rpc/out has no state
  ;;   to yield, so its entry event re-raises the exception in the server as well, as ML does.
  ;; - TraceCML has no servers: trace modules, destinations, watches and the handler registry are
  ;;   plain atomic updates, so ML's "carefully" protocol and its failure modes disappear (an error
  ;;   such as `trace-module-of` on a missing name is raised in the caller, never in a server).
  ;;   Module names get a "/" separator ("/ThreadWatcher/", as ML's documentation says); the trace
  ;;   thunk runs in the tracing thread; port destinations print even outside run-cml; a trace file
  ;;   is closed at every shutdown and reopened for appending (ML kept writing to the closed stream);
  ;;   a watch is registered before `watch` returns and a watched thread's death never blocks the
  ;;   watcher bookkeeping (ML's two watcher bugs); uncaught exceptions are dispatched in the dying
  ;;   thread itself, before its join event fires (ML forwards them to a server that spawns a thread).

  ; Multicast (multicast.sml) ------------------------------------------------------------------------

  ; the stream is a chain of ivars, each one eventually holding (v . next-ivar); tail is the empty one
  (define-record %mchan tail)
  ; out: the channel fed by the port's tee thread; state: an mvar with the ivar of the next unread message
  (define-record %mport out state)

  (set-record-printer! %mchan (λ (m port) (display "#<multicast-channel>" port)))
  (set-record-printer! %mport (λ (m port) (display "#<multicast-port>" port)))

  ; Multicast.mChannel
  (define (make-multicast-channel) (make-%mchan (make-ivar)))
  (define (multicast-channel? x) (%mchan? x))
  (define (multicast-port? x) (%mport? x))

  ; mkPort: the tee thread copies the stream from cv on into the port's channel
  (define (%make-mport cv)
    (let1 (out (make-channel))
      (spawn (τ (let tee ((cv cv))
                  (let1 (m (ivar-get cv))
                    (send out m)
                    (tee (cdr m))))))
      (make-%mport out (make-mvar cv))))

  ; Multicast.port: a new port sees every message multicast after its creation
  (define (multicast-port mc) (%make-mport (%mchan-tail mc)))

  ; Multicast.copy: a port whose future stream is the one of port (exact for a single reader)
  (define (multicast-copy-port port) (%make-mport (mvar-get (%mport-state port))))

  ; Multicast.multicast: never blocks, but within run-cml it yields, and promotes a thread from
  ; rdyQ2 (see %preempt!) as it does.  ML's multicast is a rendezvous with the server thread, which
  ; throttles a producer to the pace of the tees and receivers: without it a producer looping on
  ; multicast! would use its whole quantum while they, several switches per message and often
  ; demoted to rdyQ2 by preemptions, fall behind for good, and the chain of ivars that the slowest
  ; port has not read yet grows without bound
  (define (multicast! mc v)
    (let* ((cv (%mchan-tail mc))
           (next (make-ivar)))
      (%mchan-tail-set! mc next)
      (ivar-put! cv (cons v next))
      (when %running (%letcc/call k (%atomic-begin) (%promote!) (%atomic-yield k)))
      (void)))

  (define (%mport-advance! port m) (mvar-swap! (%mport-state port) (cdr m)) (car m))

  ; Multicast.recvEvt
  (define (multicast-recv-evt port) (wrap (recv-evt (%mport-out port)) (λ (m) (%mport-advance! port m))))

  ; Multicast.recv
  (define (multicast-recv port) (%mport-advance! port (recv (%mport-out port))))

  ; SimpleRPC (simple-rpc.sml) -----------------------------------------------------------------------

  ; call: enqueue (arg . result) and wait for the answer, re-raising what the server raised
  (define (%rpc-call mb) (λ (arg) (let1 (r (make-result)) (mailbox-send! mb (cons arg r)) (result-get r))))

  ; run thunk in the server and answer the caller; (ok v ...) with thunk's values, or (exn . e).
  ; With state? thunk must return the result and the new state, anything else is answered with
  ; (exn cml rpc); otherwise the result is its first value, (void) when it returns none.  commit!
  ; gets that outcome before the answer is put, since the put may switch to the caller, or be a
  ; preemption, and so let another thread serve the next request meanwhile
  (define (%rpc-answer! r thunk state? #!optional (commit! void))
    (let1 (x (handle-exceptions e (cons 'exn e)
               (receive vals (thunk)
                 (if (and state? (not (and (pair? vals) (pair? (cdr vals)))))
                   (cons 'exn (%cml-condition 'rpc "rpc: expected a result and a new state" vals))
                   (cons 'ok vals)))))
      (commit! x)
      (if (eq? 'ok (car x))
        (result-put! r (if (pair? (cdr x)) (cadr x) (void)))
        (result-put-exn! r (cdr x)))
      x))

  (define (%rpc-entry-evt mb f)
    (wrap (mailbox-recv-evt mb) (λ (req) (f (car req) (cdr req)))))

  ; SimpleRPC.mkRPC: (f arg) → result; entry-evt's value is unspecified
  (define (make-rpc f)
    (let1 (mb (make-mailbox))
      (values (%rpc-call mb)
              (%rpc-entry-evt mb (λ (arg r) (%rpc-answer! r (τ (f arg)) #f) (void))))))

  ; SimpleRPC.mkRPC_In: (f arg state) → result; (entry-evt state) is an event with an unspecified value
  (define (make-rpc/in f)
    (let1 (mb (make-mailbox))
      (values (%rpc-call mb)
              (λ (st) (%rpc-entry-evt mb (λ (arg r) (%rpc-answer! r (τ (f arg st)) #f) (void)))))))

  ; SimpleRPC.mkRPC_Out: (f arg) → (values result new-state); entry-evt's value is new-state.  When f
  ; raises there is no state to give, so the server re-raises the exception too (as ML does)
  (define (make-rpc/out f)
    (let1 (mb (make-mailbox))
      (values (%rpc-call mb)
              (%rpc-entry-evt mb (λ (arg r)
                                   (let1 (x (%rpc-answer! r (τ (f arg)) #t))
                                     (if (eq? 'ok (car x)) (caddr x) (abort (cdr x)))))))))

  ; SimpleRPC.mkRPC_InOut: (f arg state) → (values result new-state); (entry-evt state) is an event
  ; whose value is new-state, or state itself when f raised
  (define (make-rpc/in-out f)
    (let1 (mb (make-mailbox))
      (values (%rpc-call mb)
              (λ (st) (%rpc-entry-evt mb (λ (arg r)
                                           (let1 (x (%rpc-answer! r (τ (f arg st)) #t))
                                             (if (eq? 'ok (car x)) (caddr x) st))))))))

  ; make-rpc/in-out with the state kept inside, starting from init: entry-evt's value is the new state
  ; (the old one when f raised).  The state is read and written when a request is served, not when
  ; entry-evt is synced on, and the new state is stored before the caller is answered, so several
  ; server threads can share it (f runs without a CML operation in between, unless f itself
  ; performs one)
  (define (make-rpc/state init f)
    (let ((mb (make-mailbox)) (st init))
      (values (%rpc-call mb)
              (%rpc-entry-evt mb (λ (arg r)
                                   (let1 (new #f)
                                     (%rpc-answer! r (τ (f arg st)) #t
                                                   (λ (x) (set! new (if (eq? 'ok (car x)) (caddr x) st)) (set! st new)))
                                     new))))))

  ; TraceCML (trace-cml.sml): trace modules ---------------------------------------------------------

  (define-record %trace-module full-name label tracing children) ; children newest first, as in ML

  (set-record-printer! %trace-module
    (λ (m port) (display "#<trace-module " port) (display (%trace-module-full-name m) port) (display ">" port)))

  (define (trace-module? x) (%trace-module? x))

  ; TraceCML.traceRoot
  (define trace-module/root (make-%trace-module "/" "" #f '()))

  ; TraceCML.traceModule: the child of parent called name, created (inheriting parent's flag) when missing
  (define (trace-module parent name)
    (unless (and (string? name) (equal? (list name) (string-split name "/")))
      (%cml-raise 'trace "trace-module: a name is a non-empty string without /" name))
    (let1 (parent (%->trace-module parent))
      (or (let find ((l (%trace-module-children parent)))
            (cond ((null? l) #f) ((string=? name (%trace-module-label (car l))) (car l)) (else (find (cdr l)))))
          (let1 (m (make-%trace-module (string-append (%trace-module-full-name parent) name "/") name
                                       (%trace-module-tracing parent) '()))
            (%trace-module-children-set! parent (cons m (%trace-module-children parent)))
            m))))

  ; TraceCML.nameOf
  (define (trace-module-name m) (%trace-module-full-name m))

  ; TraceCML.moduleOf: empty arcs are ignored; raises (exn cml no-such-module)
  (define (trace-module-of name)
    (let find ((arcs (string-split name "/")) (m trace-module/root))
      (if (null? arcs)
        m
        (let1 (c (let loop ((l (%trace-module-children m)))
                   (cond ((null? l) #f) ((string=? (car arcs) (%trace-module-label (car l))) (car l)) (else (loop (cdr l))))))
          (if c (find (cdr arcs) c) (%cml-raise 'no-such-module "no such trace module" name))))))

  ; a module, or its name
  (define (%->trace-module m) (if (string? m) (trace-module-of m) m))

  (define (%trace-for-all! f m)
    (let recur ((m (%->trace-module m))) (f m) (for-each recur (%trace-module-children m))))

  ; TraceCML.traceOn / traceOff: the module and its descendants
  (define (trace-on! m) (%trace-for-all! (λ (m) (%trace-module-tracing-set! m #t)) m))
  (define (trace-off! m) (%trace-for-all! (λ (m) (%trace-module-tracing-set! m #f)) m))

  ; TraceCML.traceOnly: the module alone
  (define (trace-on-only! m) (%trace-module-tracing-set! (%->trace-module m) #t))

  ; TraceCML.amTracing
  (define (tracing? m) (%trace-module-tracing (%->trace-module m)))

  ; TraceCML.status: (module . tracing?) for m and its descendants, pre-order, children newest first
  (define (trace-status m)
    (let recur ((m (%->trace-module m)))
      (cons (cons m (%trace-module-tracing m)) (append-map recur (%trace-module-children m)))))

  ; TraceCML: trace output -------------------------------------------------------------------------

  ; TraceCML.setTraceFile as a parameter: 'out, 'err, 'null, an output port, a file name (opened on
  ; the first trace, closed at every shutdown and then reopened for appending), a channel or a mailbox
  ; (the output string is sent to it, only while CML is running)
  (define trace-to
    (make-parameter 'out
      (λ (d)
        (unless (or (memq d '(out err null)) (output-port? d) (string? d) (channel? d) (mailbox? d))
          (%cml-raise 'trace "trace-to: expected out, err, null, an output port, a file name, a channel or a mailbox" d))
        d)))

  (define %trace-files '())              ; (name . port-or-#f), #f when the file cannot be opened
  (define %trace-files-opened '())       ; names opened at least once: they are reopened to append

  (define (%trace-file-port name)
    (let1 (p (assoc name %trace-files))
      (if p
        (cdr p)
        (let1 (port (condition-case (if (member name %trace-files-opened)
                                      (open-output-file name #:append)
                                      (open-output-file name))
                      (ignored () #f)))
          (if port
            (push! name %trace-files-opened)
            (let1 (err (current-error-port))
              (display (string-append "TraceCML: unable to open \"" name "\", redirecting to stdout\n") err)
              (flush-output err)))
          (push! (cons name port) %trace-files)
          port))))

  ; closes every trace file (TraceCML's tracerStop), done at every shutdown after the logged
  ; servers have been shut down (see the "TraceCML" cleaner)
  (define (trace-close-files!)
    (let1 (l %trace-files)
      (set! %trace-files '())
      (for-each (λ (p) (when (cdr p) (close-output-port (cdr p)))) l)))

  (define (%trace-print s)
    (define (out port) (display s port) (flush-output port))
    (let1 (d (trace-to))
      (cond
        ((eq? d 'out) (out (current-output-port)))
        ((eq? d 'err) (out (current-error-port)))
        ((eq? d 'null) (void))
        ((output-port? d) (out d))
        ((string? d) (out (or (%trace-file-port d) (current-output-port))))
        ((not %running) (void))
        ((channel? d) (send d s))
        (else (mailbox-send! d s)))))

  ; TraceCML.trace: when m is traced, thunk runs (in the calling thread) and the strings (or any
  ; displayable objects) of the list it returns are printed, concatenated, to (trace-to)
  (define (trace m thunk)
    (when (tracing? m)
      (let1 (x (thunk))
        ; not with apply: CHICKEN's apply hangs on some 32000 arguments (see %pdriver-text)
        (%trace-print (if (string? x) x (call-with-output-string (λ (o) (for-each (λ (y) (display y o)) x))))))))

  ; TraceCML: thread watching -------------------------------------------------------------------------

  ; TraceCML.watcher, on at load time
  (define trace-watcher (trace-module trace-module/root "ThreadWatcher"))
  (trace-on! trace-watcher)

  ; a watch is kept on the tid itself (ML's watcher keeps a hash table), as (run . ivar), putting
  ; the ivar stops the watcher thread; one made in a previous run-cml session does not count
  (define (%watch-ivar tid)
    (let1 (w (%tid-watch tid)) (and w (eq? %run-id (car w)) (cdr w))))

  (define (%watched-remove! tid iv)
    (let1 (cur (%watch-ivar tid)) (when (and cur (or (not iv) (eq? iv cur))) (%tid-watch-set! tid #f))))

  ; TraceCML.unwatch: stop watching tid, a no-op when it is not watched
  (define (unwatch tid)
    (let1 (iv (%watch-ivar tid))
      (when iv
        (%tid-watch-set! tid #f)
        (ivar-put! iv #t))
      (void)))

  ; TraceCML.watch: report (on the trace-watcher module) the death of tid, named name; watching a
  ; watched thread again replaces the previous watch
  (define (watch name tid)
    (%check-running 'watch)
    (unwatch tid)
    (let1 (iv (make-ivar))
      (%tid-watch-set! tid (cons %run-id iv))
      (spawn (τ (select (ivar-get-evt iv)
                        (wrap (join-evt tid)
                              (λ ignored
                                (%watched-remove! tid iv)
                                (trace trace-watcher
                                       (τ (list "WARNING!  Watched thread " name " " (tid->string tid) " has died.\n"))))))))
      (void)))

  ; whether tid is watched
  (define (watched? tid) (and (%watch-ivar tid) #t))

  ; TraceCML: uncaught exceptions --------------------------------------------------------------------

  ; TraceCML's defaultHandlerFn: the message of the core's default-exn-handler, for thread tid
  (define (uncaught-default-handler tid e)
    (print-error-message e (current-error-port)
                         (string-append "cml: thread " (tid->string tid) " died of an uncaught exception")))

  (define %uncaught-default uncaught-default-handler)
  (define %uncaught-handlers '())

  ; TraceCML.setUncaughtFn: h takes the tid and the condition
  (define (set-uncaught-handler! h) (set! %uncaught-default h))

  ; TraceCML.setHandleFn: h takes the tid and the condition and returns #t when it handled it; the
  ; newest handler is tried first, the default one runs when none handles it or one raises
  (define (add-uncaught-handler! h) (push! h %uncaught-handlers))

  ; TraceCML.resetUncaughtFn
  (define (reset-uncaught-handlers!)
    (set! %uncaught-default uncaught-default-handler)
    (set! %uncaught-handlers '()))

  ; the value of default-exn-handler once cml-lib is loaded (TraceCML's threadHandler), running in
  ; the dying thread; parameterize default-exn-handler to bypass the registry
  (define (trace-exn-handler e)
    (let ((tid %cur-tid) (hs %uncaught-handlers) (default %uncaught-default))
      (unless (handle-exceptions ignored #f (any (λ (h) (h tid e)) hs))
        (default tid e))))

  (default-exn-handler trace-exn-handler)

  ; the "TraceCML" cleaner closing the trace files is registered with the standard ones, above

  )
