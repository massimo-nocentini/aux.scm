; The suites for the IO / OS layer of (aux cml): port events over pipes, sockets and channels,
; system-evt and tcp.  As in cml.scm every case runs complete `run-cml` sessions and asserts
; OUTSIDE of them, on what the threads recorded with `note!`.  Timing-dependent cases use generous
; margins; pipes are created per case and closed afterwards.

(import scheme
        (except (chicken base) guard)
        (chicken condition)
        (only (chicken time) current-process-milliseconds cpu-time)
        (chicken port)
        (chicken process)
        (chicken file posix)
        (chicken bytevector)
        (chicken io)
        (chicken tcp)
        (chicken gc)
        (chicken process-context)
        (chicken errno)
        (only (scheme base) open-input-string)
        (only (chicken file) create-temporary-file delete-file)
        (only (chicken bitwise) bitwise-and)
        (only srfi-1 iota count filter)
        (aux base)
        (aux unittest)
        (aux cml))

; helpers ---------------------------------------------------------------------------------------

; ⊦⧳ tolerates a condition, this one requires it
(define-syntax-rule (⊦raises (kind ...) body ...)
  (⊦= 'raised (condition-case (begin body ... 'not-raised) ((kind ...) 'raised))))

; run thunk under CML, returning (status . log) where log is what (note! x) collected, in order
(define cml-test/log '())
(define (note! x) (set! cml-test/log (cons x cml-test/log)))
(define (run/log thunk . args)
  (set! cml-test/log '())
  (let1 (status (apply run-cml thunk args))
    (cons status (reverse cml-test/log))))

; run thunk, stopping the run with its value
(define (run/value thunk) (run-cml (τ (cml/shutdown (thunk)))))

; the log of a run that ends with (cml/shutdown)
(define (log-of thunk) (cdr (run/log (τ (thunk) (cml/shutdown)))))

(define (sleep-ms ms) (file-select '() '() (/ ms 1000.0)))

; the CPU time used by this process, in ms
(define (cpu-ms) (receive (user system) (cpu-time) (+ user system)))

; raw write on a descriptor, bypassing any port
(define (fd-write! fd s) (file-write fd (string->utf8 s)))

; (f in-port out-fd) on a fresh pipe, both ends closed afterwards
(define (with-pipe f)
  (receive (in out) (create-pipe)
    (let1 (p (open-input-file* in))
      (begin1 (f p out)
        (close-input-port p)
        (condition-case (file-close out) (ignored () (void)))))))

(define-suite cml-io-ports-suite

  ((doc r) `((structure/section "Concurrent ML: events on ports, processes and sockets")
             (p "Input events read without blocking the process into a per-port side buffer that only a "
                "commit consumes, see the IO section of " (code/inline "aux.cml.scm") ".")))

  ((test/line-reader/slow-writer _)
   ; the writer sends a line in three pieces; a reader with a short timeout gives up in the middle
   ; of the line and nothing is lost: the next input-line-evt continues where the data stopped
   (⊦= '(timeout "hello" "world" "tail" #!eof)
       (with-pipe
         (λ (p out)
           (log-of (τ (spawn (τ (fd-write! out "hel")
                                (cml/sleep 0.05)
                                (fd-write! out "lo\nwor")
                                (cml/sleep 0.02)
                                (fd-write! out "ld\ntail")
                                (file-close out)))
                      (note! (sync/timeout (input-line-evt p) 0.02 'timeout))
                      (note! (sync (input-line-evt p)))
                      (note! (sync (input-line-evt p)))
                      (note! (sync (input-line-evt p)))
                      (note! (sync (input-line-evt p)))))))))

  ((test/stdio-buffered-data _)
   ; a direct read-char leaves the rest in the stdio buffer, where char-ready? cannot see it: the
   ; events must find it without waiting on the (empty) descriptor
   (⊦= '(#\a "bc" "de" #\f "ghi" #\j timeout)
       (with-pipe
         (λ (p out)
           (fd-write! out "abc\ndefghij")
           (let1 (c (read-char p))
             (log-of (τ (note! c)
                        (note! (sync/timeout (input-line-evt p) 1 'stuck))
                        (note! (sync/timeout (input-string-evt p 2) 1 'stuck))
                        (note! (sync/timeout (input-char-evt p) 1 'stuck))
                        (note! (sync/timeout (input-string-evt p 3) 1 'stuck))
                        (note! (sync/timeout (input-char-evt p) 1 'stuck))
                        (note! (sync/timeout (input-char-evt p) 0.02 'timeout)))))))))

  ((test/char-peek-string-all _)
   (⊦= '(#\x #\x "" "yz1" "23\nrest\n" "" #!eof #!eof #!eof)
       (with-pipe
         (λ (p out)
           (fd-write! out "xyz123\nrest\n")
           (file-close out)
           (log-of (τ (note! (sync (peek-char-evt p)))
                      (note! (sync (input-char-evt p)))
                      (note! (sync (input-string-evt p 0)))
                      (note! (sync (input-string-evt p 3)))
                      (note! (sync (input-all-evt p)))
                      (note! (sync (input-all-evt p)))            ; nothing left
                      (note! (sync (input-char-evt p)))
                      (note! (sync (peek-char-evt p)))
                      (note! (sync (input-line-evt p)))))))))

  ((test/eof-partial-string _)
   (⊦= '("ab" #!eof)
       (with-pipe
         (λ (p out)
           (fd-write! out "ab")
           (file-close out)
           (log-of (τ (note! (sync (input-string-evt p 5)))
                      (note! (sync (input-string-evt p 5)))))))))

  ((test/select-two-pipes _)
   ; the losing branch does not consume anything and releases the port for later readers
   (⊦= '((two "b") (one "a") (two "c"))
       (with-pipe
         (λ (p1 out1)
           (with-pipe
             (λ (p2 out2)
               (define (either)
                 (select (wrap (input-line-evt p1) (λ (l) (list 'one l)))
                         (wrap (input-line-evt p2) (λ (l) (list 'two l)))))
               (log-of (τ (spawn (τ (cml/sleep 0.02) (fd-write! out2 "b\n")))
                          (note! (either))
                          (fd-write! out1 "a\n")
                          (note! (either))
                          (fd-write! out2 "c\n")
                          (note! (either))))))))))

  ((test/losing-branch-keeps-input _)
   ; data already available on the port: the input event loses the select against a ready event
   ; and its helper, which read the data meanwhile, keeps it for the next event on the port
   (define (lose-then-read mk data)
     (with-pipe
       (λ (p out)
         (fd-write! out data)
         (log-of (τ (note! (select (mk p) (always-evt 'lost)))
                    (cml/sleep 0.01)
                    (note! (sync (mk p))))))))
   (⊦= '(lost #\a) (lose-then-read input-char-evt "ab"))
   (⊦= '(lost "ab") (lose-then-read input-line-evt "ab\nc\n"))
   (⊦= '(lost "abc") (lose-then-read (λ (p) (input-string-evt p 3)) "abcdef")))

  ((test/line-terminators _)
   ; "\r\n" and a lone "\r" end a line too, as for read-line; a "\r" at the end of the available
   ; input waits for the next char
   (⊦= '("GET / HTTP/1.0" "Host: x" "" "a" "b" "c" #!eof)
       (with-pipe
         (λ (p out)
           (fd-write! out "GET / HTTP/1.0\r\nHost: x\r\n\r\na\rb\nc\r")
           (file-close out)
           (log-of (τ (for-each (λ (i) (note! (sync (input-line-evt p)))) (iota 7)))))))
   (⊦= '("x" "y")
       (with-pipe
         (λ (p out)
           (log-of (τ (spawn (τ (fd-write! out "x\r") (cml/sleep 0.02) (fd-write! out "\ny\n")))
                      (note! (sync (input-line-evt p)))
                      (note! (sync (input-line-evt p)))))))))

  ((test/readers-share-a-port _)
   ; concurrent readers of one port are serialized by its lock: each line goes to exactly one of them
   (⊦= '((a "1") (b "2") (a "3"))
       (with-pipe
         (λ (p out)
           (log-of (τ (let ((ra (make-mailbox)) (rb (make-mailbox)))
                        (spawn (τ (mailbox-send! ra (sync (input-line-evt p))) (mailbox-send! ra (sync (input-line-evt p)))))
                        (spawn (τ (mailbox-send! rb (sync (input-line-evt p)))))
                        (fd-write! out "1
2
")
                        (note! (list 'a (mailbox-recv ra)))
                        (note! (list 'b (mailbox-recv rb)))
                        (fd-write! out "3
")
                        (note! (list 'a (mailbox-recv ra))))))))))

  ((test/port-survives-a-run-boundary _)
   ; a run that ends while a reader waits on the port leaves its partial line buffered and its lock
   ; taken: the next run re-creates the lock and continues the line
   (⊦= '((timeout) ("abcd"))
       (with-pipe
         (λ (p out)
           (fd-write! out "ab")
           (list (log-of (τ (note! (sync/timeout (input-line-evt p) 0.01 'timeout))
                            (spawn (τ (sync (input-line-evt p))))
                            (cml/sleep 0.01)))
                 (begin (fd-write! out "cd
")
                        (log-of (τ (note! (sync/timeout (input-line-evt p) 1 'stuck))))))))))

  ((test/select-input-vs-timeout-keeps-waiting _)
   ; a reader blocked on a pipe keeps run-cml alive: no deadlock is reported while it waits
   (⊦= '(timeout "late")
       (with-pipe
         (λ (p out)
           (log-of (τ (note! (sync/timeout (input-line-evt p) 0.01 'timeout))
                      (spawn (τ (cml/sleep 0.03) (fd-write! out "late\n")))
                      (note! (sync (input-line-evt p)))))))))

  ((test/read-errors-reach-the-syncing-thread _)
   (⊦= '(raised)
       (receive (in out) (create-pipe)
         (let1 (p (open-input-file* in))
           (close-input-port p)
           (file-close out)
           (log-of (τ (note! (condition-case (sync (input-char-evt p)) (e () 'raised)))))))))

  ((test/writer-and-reader-over-a-full-pipe _)
   ; 200000 characters do not fit in a pipe: the writer waits for readiness chunk by chunk while
   ; the reader drains, and the process never blocks
   (let1 (big (make-string 200000 #\z))
     (receive (in out) (create-pipe)
       (let* ((p (open-input-file* in))
              (o (open-output-file* out))
              (log (log-of (τ (let1 (w (spawn (τ (sync (write-string-evt o big))
                                                  (note! 'written)
                                                  (close-output-port o))))
                                (let1 (s (sync (input-all-evt p)))
                                  (note! (list (string-length s) (equal? s big)))))))))
         (close-input-port p)
         (⊦= '(written (200000 #t)) log)))))

  ((test/output-evt _)
   (receive (in out) (create-pipe)
     (let* ((p (open-input-file* in))
            (o (open-output-file* out))
            (log (log-of (τ (note! (eq? o (sync (output-evt o))))
                            (sync (write-string-evt o "λ-line\n"))
                            (note! (sync (input-line-evt p)))))))
       (close-input-port p)
       (close-output-port o)
       (⊦= '(#t "λ-line") log))))

  ((test/commit-survives-an-escape _)
   ; the value of an input event leaves run-cml through a continuation at once: the commit itself
   ; removed it from the side buffer, so the next run reads what follows
   (⊦= '(#\a #\b)
       (with-pipe
         (λ (p out)
           (fd-write! out "ab")
           (map (λ (i) (call-with-current-continuation (λ (k) (run-cml (τ (k (sync (input-char-evt p))))))))
                '(1 2))))))

  ((test/forcing-raises-releases-the-port _)
   ; a select abandoned because a later guard raised sets the nack of the input event forced before
   ; the guard: its helper releases the port, and the data is still there for the next events
   (⊦= '(raised #\a "bc")
       (with-pipe
         (λ (p out)
           (fd-write! out "abc\n")
           (log-of (τ (note! (condition-case (select (input-char-evt p) (guard (τ (error "guard failed"))))
                               (e () 'raised)))
                      (note! (sync/timeout (input-char-evt p) 1 'stuck))
                      (note! (sync/timeout (input-line-evt p) 1 'stuck))))))))

  ((test/input-evt _)
   ; TextIO.inputEvt: commits as soon as a char is there and gives every char available by then,
   ; what the side buffer holds included; #!eof once everything before it was given
   (⊦= '("abc" timeout "defgh" "rest" "xy" #!eof lost "12" "34")
       (with-pipe
         (λ (p out)
           (fd-write! out "abc")
           (log-of (τ (note! (sync/timeout (input-evt p) 1 'stuck))
                      (note! (sync/timeout (input-evt p) 0.02 'timeout))
                      (spawn (τ (cml/sleep 0.01) (fd-write! out "def") (fd-write! out "gh\nrest")))
                      (note! (sync/timeout (input-line-evt p) 1 'stuck))
                      (note! (sync/timeout (input-evt p) 1 'stuck))
                      (fd-write! out "xy")
                      (file-close out)
                      (note! (sync/timeout (input-evt p) 1 'stuck))
                      (note! (sync/timeout (input-evt p) 1 'stuck))
                      (let1 (p (open-input-string "12"))           ; a losing branch keeps what it read
                        (note! (select (input-evt p) (always-evt 'lost)))
                        (cml/sleep 0.01)
                        (note! (sync (input-evt p))))
                      (let* ((ch (make-channel)) (in (open-channel-input-port ch)))   ; channel ports
                        (spawn (τ (send ch "3") (send ch "4") (send ch #!eof)))
                        (note! (let loop ((acc ""))
                                 (let1 (s (sync/timeout (input-evt in) 1 'stuck))
                                   (if (string? s) (loop (string-append acc s)) acc))))))))))
   ; with more available than one drain reads, it still commits at once, with at least 4096 chars.
   ; The input is written by a CML thread, never by a blocking write before the run: a pipe may
   ; hold less than 10000 bytes (Linux gives 4 or 8 KiB once the user's pipes exceed
   ; pipe-user-pages-soft, other systems may start smaller), and the whole suite hung on it
   (let1 (r (with-pipe
              (λ (p out)
                (log-of (τ (let* ((o (open-output-file* out))
                                  (w (spawn (τ (sync (write-string-evt o (make-string 10000 #\z))) (close-output-port o)))))
                             (let1 (all-in? (sync/timeout (wrap (join-evt w) (λ ignored #t)) 0.5 #f))
                               (let loop ((n 0) (k 0) (first #f))
                                 (let1 (s (sync/timeout (input-evt p) 1 'stuck))
                                   (if (string? s)
                                     (loop (+ n (string-length s)) (add1 k) (or first (string-length s)))
                                     (note! (list n k s first all-in?))))))))))))
     (⊦= 10000 (list-ref (car r) 0))
     (⊦= #!eof (list-ref (car r) 2))
     (⊨ (>= (list-ref (car r) 3) 4096))
     (when (list-ref (car r) 4)                   ; everything was in the pipe before the first read
       (⊨ (<= 2 (list-ref (car r) 1) 3)))))

  ((test/stream-drain-leaves-the-file-status-flags-alone _)
   ; the descriptor of a stdio port is polled, never put in O_NONBLOCK mode: that flag belongs to
   ; the open file description, shared with stdout on the same tty or socket and with children
   ; that inherited it, which would get EAGAIN (a child's large write on the shared stdout failed)
   (define (nonblock? fd) (not (zero? (bitwise-and open/nonblock (file-control fd fcntl/getfl)))))
   (⊦= '(("abc\n" #f #f (#f)) (() #f #f))
       (with-pipe
         (λ (p out)
           (fd-write! out "abc\n")
           (let* ((fd (port->fileno p)) (drain (%stream-drainer fd #t 'utf-8)) (seen '()))
             (list (receive (acc eof? more?) (drain p (λ (c) (unless (memv (nonblock? fd) seen) (set! seen (cons (nonblock? fd) seen))) #f))
                     (list (list->string (reverse acc)) eof? more? seen))
                   (receive (acc eof? more?) (drain p (λ (c) #f))     ; nothing available: no wait
                     (list acc eof? more?))))))))

  ((test/port-encodings _)
   ; input events decode with the port's encoding, as a direct read does (latin-1, binary)
   (define (direct f enc) (with-input-from-file f (τ (let* ((a (read-line)) (b (read-line)) (c (read-line))) (list a b c))) enc))
   (define (events f enc)
     (let1 (p (open-input-file f enc))
       (begin1 (run/value (τ (let* ((a (sync (input-line-evt p))) (b (sync (input-line-evt p))) (c (sync (input-line-evt p))))
                               (list a b c))))
         (close-input-port p))))
   (let1 (f (create-temporary-file))
     (with-output-to-file f (τ (display "caf\xE9;\n\xFF;x\n")) #:latin-1)
     (⊦= '("café" "\xFF;x" #!eof) (direct f #:latin-1))
     (⊦= (direct f #:latin-1) (events f #:latin-1))
     (⊦= (direct f #:binary) (events f #:binary))
     (delete-file f)))

  ((test/utf-8-split-across-writes _)
   ; a writer splits 2, 3 and 4 byte sequences across writes: the reader waits for their ends
   (⊦= '("café€𝄞" #!eof)
       (with-pipe
         (λ (p out)
           (log-of (τ (spawn (τ (for-each (λ (bytes) (file-write out (apply bytevector bytes)) (cml/sleep 0.01))
                                          '((99 97 102 #xC3) (#xA9 #xE2) (#x82 #xAC #xF0 #x9D) (#x84 #x9E 10)))
                                (file-close out)))
                      (note! (sync (input-line-evt p)))
                      (note! (sync (input-line-evt p)))))))))

  ((test/utf-8-split-after-a-direct-read _)
   ; a greeting line read directly, then events: the bytes of a sequence split across writes are
   ; still decoded whole (stdio never reads the descriptor for the events)
   (⊦= '("café!" "x" #!eof)
       (with-pipe
         (λ (p out)
           (fd-write! out "greeting\n")
           (read-line p)
           (file-write out (bytevector 99 97 102 #xC3))
           (log-of (τ (spawn (τ (cml/sleep 0.05) (file-write out (bytevector #xA9 33 10 120 10)) (file-close out)))
                      (note! (sync/timeout (input-line-evt p) 1 'stuck))
                      (note! (sync/timeout (input-line-evt p) 1 'stuck))
                      (note! (sync/timeout (input-line-evt p) 1 'stuck)))))))
   ; what the direct read left in the stdio buffer is found without waiting on the descriptor
   (⊦= '("rest" "é" timeout)
       (with-pipe
         (λ (p out)
           (fd-write! out "greeting\nrest\né\n")
           (read-line p)
           (log-of (τ (note! (sync/timeout (input-line-evt p) 1 'stuck))
                      (note! (sync/timeout (input-line-evt p) 1 'stuck))
                      (note! (sync/timeout (input-line-evt p) 0.02 'timeout))))))))

  ((test/waiting-reader-does-not-leak _)
   ; a reader waiting long on a port without a descriptor polls it every 5 ms: its helper's wait
   ; loop (a select against the nack, looping from a wrap) must not retain anything per iteration
   (define p (make-input-port (λ () #!eof) (λ () #f) void))
   (define (used) (gc #t) (vector-ref (memory-statistics) 1))
   (let1 (r (run/log (τ (spawn (τ (sync (input-char-evt p))))
                        (cml/sleep 0.2)
                        (let1 (m0 (used))
                          (cml/sleep 0.6)
                          (cml/shutdown (- (used) m0))))))
     (⊨ (< (car r) 100000))))

  ((test/poll-raises-releases-the-port _)
   ; a select abandoned because the poll of another branch raised (a bad argument) sets the nack
   ; of the input event: its helper releases the port, and the data is still there
   (⊦= '(raised "line1" "line2")
       (with-pipe
         (λ (p out)
           (fd-write! out "line1\nline2\n")
           (log-of (τ (note! (condition-case (select (input-line-evt p) (mvar-take-evt 'not-an-mvar)) (e () 'raised)))
                      (note! (sync/timeout (input-line-evt p) 1 'stuck))
                      (note! (sync/timeout (input-line-evt p) 1 'stuck))))))))

  ((test/concurrent-write-string-evts-do-not-interleave _)
   ; a string too long for the pipe is written chunk by chunk, each after a readiness wait: another
   ; write-string-evt on the port meanwhile waits for the port's write lock, and its string comes
   ; whole, after the first one, never in the middle of it.  Each of the two lines gets a generous
   ; timeout (the transfer of the long one takes about 0.5 s interpreted, more under load); only
   ; the check that nothing follows them uses a short one
   (let1 (big (string-append (make-string 100000 #\A) "\n"))
     (receive (in out) (create-pipe)
       (let* ((p (open-input-file* in))
              (o (open-output-file* out))
              (log (log-of (τ (spawn (τ (sync (write-string-evt o big)) (note! 'a-written)))
                              (cml/sleep 0.05)                                     ; the pipe is full
                              (spawn (τ (sync (write-string-evt o "BBBB\n")) (note! 'b-written)))
                              (cml/sleep 0.05)
                              (do ((i 0 (add1 i))) ((= i 2))
                                (let1 (l (sync/timeout (input-line-evt p) 30 'timeout))
                                  (note! (if (member l '("BBBB" timeout)) l (list (string-length l) (and (memv #\B (string->list l)) #t))))))
                              (note! (sync/timeout (input-line-evt p) 0.1 'nothing-more))))))
         (close-input-port p)
         (close-output-port o)
         (⊦= '((100000 #f) "BBBB" nothing-more) (filter (λ (x) (not (memq x '(a-written b-written)))) log))))))

  ((test/input-events-on-a-closed-port _)
   ; an input event on a closed port raises, as a direct read does: it must not read the descriptor
   ; number, which may belong to another file by now (here a new pipe, whose owner keeps its data).
   ; A reader already waiting when its port is closed raises too, at its next wake-up
   (define (message thunk) (condition-case (begin (thunk) 'not-raised) (e (exn) (get-condition-property e 'exn 'message))))
   (receive (r w) (create-pipe)
     (let ((in (open-input-file* r)) (out (open-output-file* w)))
       (⊦= '("one" "port is closed" #t "secret" "port is closed" #t "more")
           (log-of (τ (display "one\n" out) (flush-output out)
                      (note! (sync (input-line-evt in)))
                      (close-input-port in) (close-output-port out)
                      (receive (r2 w2) (create-pipe)
                        (let1 (in2 (open-input-file* r2))
                          (fd-write! w2 "secret\n")
                          (note! (message (τ (sync/timeout (input-line-evt in) 0.2 'timeout))))
                          (note! (equal? (list r w) (list r2 w2)))
                          (note! (sync/timeout (input-line-evt in2) 0.2 'timeout))
                          ; a reader waiting on in2 when it is closed and its descriptor reused
                          (let* ((res (make-ivar))
                                 (t (spawn (τ (ivar-put! res (message (τ (sync (input-line-evt in2)))))))))
                            (cml/sleep 0.02)
                            (close-input-port in2)
                            (file-close w2)
                            (receive (r3 w3) (create-pipe)
                              (let1 (in3 (open-input-file* r3))
                                (fd-write! w3 "more\n")
                                (note! (sync/timeout (ivar-get-evt res) 0.5 'still-waiting))
                                (note! (equal? (list r2 w2) (list r3 w3)))
                                (note! (sync/timeout (input-line-evt in3) 0.2 'timeout))
                                (close-input-port in3)
                                (file-close w3))))))))))))

  ((test/closed-port-with-a-reused-descriptor _)
   ; a reader (or a writer between two chunks) waiting when another thread closes its port raises,
   ; also when the descriptor number goes at once to a file that never becomes ready (here the read
   ; end of a new pipe: never readable for the reader's wait... and never writable for the
   ; writer's): the wait checks for the close by itself, not through the descriptor's readiness
   (define (message thunk) (condition-case (begin (thunk) 'not-raised) (e (exn) (get-condition-property e 'exn 'message))))
   (receive (r w) (create-pipe)
     (let ((in (open-input-file* r)) (new '()))
       (⊦= '("port is closed" #t)
           (log-of (τ (let1 (res (make-ivar))
                        (spawn (τ (ivar-put! res (message (τ (sync (input-line-evt in)))))))
                        (cml/sleep 0.02)
                        (close-input-port in)
                        (receive (r2 w2) (create-pipe)
                          (set! new (list r2 w2))
                          (note! (sync/timeout (ivar-get-evt res) 1 'still-waiting))
                          (note! (= r r2)))))))
       (file-close w)
       (for-each file-close new)))
   (receive (r w) (create-pipe)
     (let ((out (open-output-file* w)) (new '()))
       (⊦= '("port is closed" #t)
           (log-of (τ (let1 (res (make-ivar))
                        ; more than the pipe holds: the writer waits between two chunks
                        (spawn (τ (ivar-put! res (message (τ (sync (write-string-evt out (make-string 200000 #\x))))))))
                        (cml/sleep 0.05)
                        (close-output-port out)
                        (receive (r2 w2) (create-pipe)
                          (set! new (list r2 w2))
                          (note! (sync/timeout (ivar-get-evt res) 1 'still-waiting))
                          (note! (= w r2)))))))
       (file-close r)
       (for-each file-close new))))

  ((test/input-in-small-pieces-in-linear-time _)
   ; input arriving in many small pieces is not copied again, nor scanned again, at every piece: a
   ; long line or string read from a channel port fed 10 characters at a time costs O(length)
   ; (a line of 40000 characters took over a minute interpreted).  8 times as much input must not
   ; take 20 times as long
   (define (reading-time n make-evt)
     (let1 (t0 (current-process-milliseconds))
       (⊦= (* 10 n)
           (run/value (τ (let1 (ch (make-channel))
                           (spawn (τ (for-each (λ (i) (send ch "abcdefghi.")) (iota n)) (send ch "\n") (send ch #!eof)))
                           (string-length (sync (make-evt (open-channel-input-port ch))))))))
       (- (current-process-milliseconds) t0)))
   (for-each (λ (make-evt)
               (let* ((small (max 20 (reading-time 500 make-evt)))
                      (large (reading-time 4000 make-evt)))
                 (⊨ (< large (* 20 small)))))
             ; input-line-evt, and input-all-evt less the final "\n"
             (list input-line-evt (λ (p) (wrap (input-all-evt p) (λ (s) (substring s 0 (sub1 (string-length s)))))))))

  ((test/a-flooding-stream-does-not-freeze-the-others _)
   ; a line that never ends, from a writer that never pauses: the reader drains a bounded amount at
   ; a time and yields in between, so the other threads and the timeouts go on, and the sync gives
   ; up when its timeout fires (a drain that read as long as input was there never returned: every
   ; other thread froze and memory grew without bound)
   (receive (in out pid) (cml/execute "/bin/sh" '("-c" "yes | tr -d '\\n'"))
     (let1 (log (log-of (τ (spawn (τ (let loop () (cml/sleep 0.01) (note! 'tick) (loop))))
                           (note! (sync/timeout (input-line-evt in) 0.5 'timeout))
                           (note! (sync/timeout (input-all-evt in) 0.2 'timeout)))))
       (process-signal pid)
       (close-input-port in)
       (close-output-port out)
       (process-wait pid)
       (⊦= '(timeout timeout) (filter symbol? (filter (λ (x) (not (eq? x 'tick))) log)))
       (⊨ (>= (count (λ (x) (eq? x 'tick)) log) 3)))))

  ((test/write-to-a-closed-pipe-raises _)
   ; a pipe whose reader has exited: the write raises EPIPE (SIGPIPE is ignored once (chicken tcp)
   ; is loaded), every time, instead of reporting success while the data is lost
   (receive (in out) (create-pipe)
     (file-close in)
     (let1 (o (open-output-file* out))
       (⊦= (list errno/pipe errno/pipe errno/pipe)
           (run/value (τ (map (λ (s) (condition-case (begin (sync (write-string-evt o s)) 'no-error)
                                       (e (exn i/o) (get-condition-property e 'exn 'errno #f))))
                              (list "line\n" "again\n" (make-string 1000 #\x))))))
       (condition-case (close-output-port o) (ignored () (void)))))
   ; whatever the port's encoding: latin-1 and binary ports were written through stdio, whose
   ; flush drops the error, so the data was lost and the write reported success
   (for-each (λ (enc)
               (receive (in out) (create-pipe)
                 (file-close in)
                 (let1 (o (open-output-file* out))
                   (set! (port-encoding o) enc)
                   (⊦= (list enc errno/pipe errno/pipe)
                       (cons enc (run/value (τ (map (λ (s) (condition-case (begin (sync (write-string-evt o s)) 'no-error)
                                                             (e (exn i/o) (get-condition-property e 'exn 'errno #f))))
                                                    (list "line\n" "caf\xe9;\n"))))))
                   (condition-case (close-output-port o) (ignored () (void))))))
             '(latin-1 binary))
   ; and the bytes written are those of the encoding, as a direct write gives them
   (for-each (λ (enc)
               (receive (in out) (create-pipe)
                 (let ((i (open-input-file* in)) (o (open-output-file* out)))
                   (set! (port-encoding o) enc)
                   (run/value (τ (sync (write-string-evt o "a\xe9;\x20ac;"))))
                   (close-output-port o)
                   (set! (port-encoding i) 'binary)
                   (⊦= (if (eq? enc 'latin-1) '(97 233 172) '(97 195 169 226 130 172))
                       (let loop ((acc '())) (let1 (b (read-byte i)) (if (eof-object? b) (reverse acc) (loop (cons b acc))))))
                   (close-input-port i))))
             '(latin-1 binary utf-8)))

  ((test/writer-dying-with-the-write-lock-releases-it _)
   ; a writer waiting between two chunks holds the port's write lock; when it dies as it is switched
   ; back in (a dynamic-wind before thunk that raises, outside the frame that releases the lock) the
   ; lock goes back to the port: the next write-string-evt on it used to hang forever
   (receive (in out) (create-pipe)
     (let ((p (open-input-file* in)) (o (open-output-file* out)) (armed #f))
       (let1 (log (log-of (τ (parameterize ((default-exn-handler (λ (e) (note! 'a-died))))
                               (spawn (τ (dynamic-wind (τ (when armed (set! armed #f) (error "before thunk raised")))
                                                       (τ (sync (write-string-evt o (make-string 100000 #\A))) (note! 'a-written))
                                                       void))))
                             (cml/sleep 0.05)                 ; the pipe is full, the writer holds the lock
                             (set! armed #t)
                             (spawn (τ (sync (write-string-evt o "BBBB\n")) (note! 'b-written)))
                             (note! (let1 (l (sync/timeout (input-line-evt p) 2 'timeout))
                                      (if (string? l) (substring l (- (string-length l) 4)) l))))))
         (close-input-port p)
         (close-output-port o)
         (⊨ (and (memq 'a-died log) #t))
         (⊨ (and (memq 'b-written log) #t))
         (⊨ (and (member "BBBB" log) #t))
         (⊨ (not (memq 'a-written log)))))))

  ((test/idle-waiters-cost-nothing _)
   ; readers waiting on quiet pipes cost nothing while idle: each one used to re-arm a 0.1 s
   ; timeout to check for a close, so 2000 of them kept a core 80% busy.  Now one sweep checks
   ; them all, and only after some thread ran: 400 idle readers for 1 s cost a few ms of CPU
   ; (about 250 before)
   (let* ((pipes (map (λ (i) (receive (r w) (create-pipe) (cons (open-input-file* r) w))) (iota 400)))
          (cpu (run/value (τ (for-each (λ (pw) (spawn (τ (sync (input-line-evt (car pw)))))) pipes)
                             (cml/sleep 0.1)
                             (let1 (t0 (cpu-ms))
                               (cml/sleep 1)
                               (- (cpu-ms) t0))))))
     (for-each (λ (pw) (close-input-port (car pw)) (file-close (cdr pw))) pipes)
     (⊨ (< cpu 60))))

  ((test/bad-arguments _)
   ; counts and descriptors must be exact: an inexact count is rejected when the event is made
   (define (message thunk) (condition-case (begin (thunk) 'not-raised) (e (exn) (get-condition-property e 'exn 'message))))
   (⊦= "bad count" (message (τ (input-string-evt (open-input-string "abcdef") 2.0))))
   (⊦= "bad count" (message (τ (input-string-evt (open-input-string "abcdef") -1))))
   (⊦= "cml: bad io spec, expected (fd input|output)" (message (τ (poll-evt (list 0.0 'input)))))
   (⊦= "ab" (run/value (τ (sync (input-string-evt (open-input-string "abcdef") 2))))))

  ((test/not-running _)
   (with-pipe (λ (p out) (⊦raises (exn cml not-running) (sync (input-char-evt p)))))
   (⊦raises (exn cml not-running) (system-evt "true"))
   (⊦raises (exn) (input-char-evt 42)))

  )

(define-suite cml-io-os-suite

  ((test/system-evt/exit-codes _)
   (⊦= '(0 3 127 137 0)
       (log-of (τ (note! (cml/system "true"))
                  (note! (sync (system-evt "exit 3")))
                  (note! (cml/system "a-command-that-does-not-exist 2>/dev/null"))
                  (note! (cml/system "kill -9 $$"))
                  (let1 (e (system-evt "true"))
                    (sync e)
                    (note! (sync e)))))))                        ; the status is memoized

  ((test/system-evt/eager-and-selectable _)
   ; the command starts when the event is made; a timeout can win against it and it is reaped anyway
   (let1 (log (log-of (τ (let1 (e (system-evt "sleep 0.05"))
                           (note! (select (wrap e (λ (c) (list 'done c)))
                                          (wrap (timeout-evt 0.01) (λ ignored 'timeout))))
                           (note! (sync e))))))
     (⊦= '(timeout 0) log)))

  ((test/system-evt/concurrent _)
   ; three children run in parallel: all finish in about the time of the slowest one
   (let1 (log (log-of (τ (let* ((t0 (cml/now))
                                (es (map (λ (i) (system-evt (string-append "sleep 0.1; exit " (number->string i))))
                                         (iota 3))))
                           (note! (map sync es))
                           (note! (< (- (cml/now) t0) 0.25))))))
     (⊦= '((0 1 2) #t) log)))

  ((test/execute/cat _)
   (let1 (log (log-of (τ (receive (in out pid) (cml/execute "cat")
                           (sync (write-string-evt out "ping\npong\n"))
                           (note! (sync (input-line-evt in)))
                           (note! (sync/timeout (input-line-evt in) 1 'stuck))
                           (close-output-port out)                   ; cat sees end of file and exits
                           (note! (sync (input-line-evt in)))
                           (note! (sync (process-evt pid)))
                           (close-input-port in)))))
     (⊦= '("ping" "pong" #!eof (#t 0)) log)))

  ((test/execute/close-on-exec _)
   ; a second child does not inherit the first one's pipes: closing out1 ends the first cat
   (⊦= '((#t 0) #!eof (#t 0))
       (log-of (τ (receive (in1 out1 pid1) (cml/execute "cat")
                    (receive (in2 out2 pid2) (cml/execute "cat")
                      (close-output-port out1)
                      (note! (sync/timeout (process-evt pid1) 2 'still-running))
                      (note! (sync/timeout (input-line-evt in1) 1 'no-eof))
                      (close-output-port out2)
                      (note! (sync (process-evt pid2)))
                      (close-input-port in1)
                      (close-input-port in2)))))))

  ((test/process-evt/reaped-after-the-run _)
   ; a child still running when its run ends is reaped later, as ML's ProcManager does, instead of
   ; staying a zombie: waiting for its pid afterwards finds no such child
   (let1 (pid #f)
     (⊦= 'gave-up (run-cml (τ (set! pid (process-run "sleep" '("0.05")))
                              (cml/shutdown (sync/timeout (process-evt pid) 0.005 'gave-up)))))
     (run-cml (τ (cml/sleep 0.2) (cml/shutdown)))
     (⊦= 'reaped (condition-case (receive (p normal? code) (process-wait (process-id pid) #t) (list p normal? code))
                   (ignored () 'reaped)))))

  ((test/execute/closed-stdio _)
   ; the parent's stdin and stdout are closed, so create-pipe hands out 0 and 1 for the pipe ends:
   ; they must still become the child's stdin and stdout
   (flush-output (current-output-port))
   (let ((in0 (duplicate-fileno 0)) (out1 (duplicate-fileno 1)))
     (file-close 0)
     (file-close 1)
     (let1 (log (dynamic-wind
                  void
                  (τ (log-of (τ (receive (in out pid) (cml/execute "cat")
                                  (sync (write-string-evt out "ping\n"))
                                  (note! (sync/timeout (input-line-evt in) 2 'stuck))
                                  (close-output-port out)
                                  (note! (sync (process-evt pid)))
                                  (close-input-port in)))))
                  (τ (duplicate-fileno in0 0) (duplicate-fileno out1 1) (file-close in0) (file-close out1))))
       (⊦= '("ping" (#t 0)) log))))

  ((test/system-evt/bin-sh _)
   ; the command runs through /bin/sh, whatever the user's $SHELL
   (let1 (shell (get-environment-variable "SHELL"))
     (set-environment-variable! "SHELL" "/bin/false")
     (let1 (log (dynamic-wind
                  void
                  (τ (log-of (τ (note! (cml/system "exit 3")))))
                  (τ (if shell (set-environment-variable! "SHELL" shell) (unset-environment-variable! "SHELL")))))
       (⊦= '(3) log))))

  ((test/execute/missing-program _)
   ; a child whose exec fails exits with status 128, as in new-unix.sml
   (⊦= '((#t 128))
       (log-of (τ (receive (in out pid) (cml/execute "/nonexistent/program")
                    (note! (sync (process-evt pid)))
                    (close-input-port in)
                    (close-output-port out))))))

  ((test/execute/path-and-argv0 _)
   ; deviations from new-unix.sml, documented: a command without a "/" is searched in PATH, and
   ; argv[0] is the command as given
   (⊦= '("/bin/sh" "sh" (#t 0) (#t 0))
       (log-of (τ (receive (in1 out1 pid1) (cml/execute "/bin/sh" '("-c" "echo $0"))
                    (receive (in2 out2 pid2) (cml/execute "sh" '("-c" "echo $0"))
                      (note! (sync (input-line-evt in1)))
                      (note! (sync (input-line-evt in2)))
                      (note! (sync (process-evt pid1)))
                      (note! (sync (process-evt pid2)))
                      (for-each close-input-port (list in1 in2))
                      (for-each close-output-port (list out1 out2))))))))

  ((test/execute/in-env _)
   ; Unix.executeInEnv: a list of "NAME=value" strings is the child's whole environment (and cmd
   ; is not searched in PATH then, as in ML); without it the child inherits this process's
   (⊦= '("LANG=C\nFOO=a=b\n" (#t 0) "" (#t 0) "set\n" (#t 0) (#t 128) raised)
       (log-of (τ (define (run cmd args . env)
                    (receive (in out pid) (apply cml/execute cmd args env)
                      (close-output-port out)
                      (note! (sync (input-all-evt in)))
                      (note! (sync (process-evt pid)))
                      (close-input-port in)))
                  (run "/usr/bin/env" '() '("LANG=C" "FOO=a=b"))
                  (run "/usr/bin/env" '() '())
                  (run "/bin/sh" '("-c" "echo ${PATH:+set}"))
                  (receive (in out pid) (cml/execute "env" '() '("A=b"))
                    (note! (sync (process-evt pid)))
                    (close-input-port in)
                    (close-output-port out))
                  (note! (condition-case (cml/execute "/usr/bin/env" '() '("NOEQUALS")) (e (exn) 'raised)))))))

  ((test/children-start-with-sigpipe-default _)
   ; this process ignores SIGPIPE ((chicken tcp) does), and an ignored signal stays ignored across
   ; exec: the children are given its default action back, as ML's are, so a shell pipeline whose
   ; reader exits ends (a writer loop kept running for good).  A shell that sends itself SIGPIPE
   ; dies of it (status 128 + 13) instead of going on
   (⊦= '(141 (#f 13) 0)
       (log-of (τ (note! (cml/system "kill -PIPE $$; exit 0"))
                  (receive (in out pid) (cml/execute "/bin/sh" '("-c" "kill -PIPE $$; exit 0"))
                    (note! (sync (process-evt pid)))
                    (close-input-port in)
                    (close-output-port out))
                  (note! (sync/timeout (system-evt "(while :; do echo x; done 2>/dev/null) | head -1 >/dev/null") 5 'hung))))))

  )

(define-suite cml-io-tcp-suite

  ((test/echo-server _)
   ; an echo server accepting with tcp-accept-evt on an ephemeral port, one thread per client;
   ; two clients talk to it concurrently
   (let* ((l (tcp-listen 0 4 "127.0.0.1"))
          (port (tcp-listener-port l))
          (log (log-of
                 (τ (define (serve in out)
                      (let loop ()
                        (let1 (line (sync (input-line-evt in)))
                          (if (eof-object? line)
                            (begin (close-input-port in) (close-output-port out))
                            (begin (sync (write-string-evt out (string-append "echo " line "\n"))) (loop))))))
                    (define (client name)
                      (let* ((io (sync (tcp-connect-evt "127.0.0.1" port)))
                             (in (car io))
                             (out (cadr io)))
                        (let1 (replies (map (λ (i)
                                              (sync (write-string-evt out (string-append name (number->string i) "\n")))
                                              (sync (input-line-evt in)))
                                            (iota 3)))
                          (close-output-port out)
                          (let1 (end (sync (input-line-evt in)))
                            (close-input-port in)
                            (list name replies end)))))
                    (spawn (τ (let loop ()
                                (let1 (io (sync (tcp-accept-evt l)))
                                  (spawn (τ (serve (car io) (cadr io))))
                                  (loop)))))
                    (let* ((r1 (make-result))
                           (r2 (make-result)))
                      (spawn (τ (result-put! r1 (client "a"))))
                      (spawn (τ (result-put! r2 (client "b"))))
                      (note! (result-get r1))
                      (note! (result-get r2)))))))
     (tcp-close l)
     (⊦= '(("a" ("echo a0" "echo a1" "echo a2") #!eof) ("b" ("echo b0" "echo b1" "echo b2") #!eof)) log)))

  ((test/accept-evt/timeout-then-accept _)
   ; with no client the timeout wins and no connection is consumed; then a client comes in
   (let* ((l (tcp-listen 0 4 "127.0.0.1"))
          (port (tcp-listener-port l))
          (log (log-of (τ (note! (sync/timeout (tcp-accept-evt l) 0.02 'nobody))
                          (spawn (τ (cml/sleep 0.02)
                                    (let1 (io (sync (tcp-connect-evt "127.0.0.1" port)))
                                      (sync (write-string-evt (cadr io) "hi\n"))
                                      (close-output-port (cadr io))
                                      (close-input-port (car io)))))
                          (let1 (io (sync (tcp-accept-evt l)))
                            (note! (sync (input-line-evt (car io))))
                            (note! (sync (input-line-evt (car io))))
                            (close-input-port (car io))
                            (close-output-port (cadr io)))))))
     (tcp-close l)
     (⊦= '(nobody "hi" #!eof) log)))

  ((test/accept-evt/losing-branch _)
   ; a pending connection is accepted by the branch that commits only: a select lost against a
   ; ready event leaves it pending for the next accept
   (let* ((l (tcp-listen 0 4 "127.0.0.1"))
          (port (tcp-listener-port l))
          (log (log-of (τ (receive (cin cout) (tcp-connect "127.0.0.1" port)
                            (let1 (got (map (λ (i)
                                              (select (wrap (tcp-accept-evt l)
                                                            (λ (io)
                                                              (close-input-port (car io))
                                                              (close-output-port (cadr io))
                                                              'accepted))
                                                      (always-evt 'other)))
                                            (iota 4)))
                              (note! (count (λ (x) (eq? x 'accepted)) got))
                              (note! (tcp-accept-ready? l)))
                            (close-input-port cin)
                            (close-output-port cout))))))
     (tcp-close l)
     (⊦= '(1 #f) log)))

  ((test/accept-evt/two-acceptors-one-connection _)
   ; two threads accept on one listener with a single pending connection and a preemption at every
   ; operation: the one that loses the race waits again instead of blocking the process in tcp-accept
   (let* ((l (tcp-listen 0 4 "127.0.0.1"))
          (port (tcp-listener-port l)))
     (receive (cin cout) (tcp-connect "127.0.0.1" port)
       (let1 (r (run/log (τ (for-each (λ (tag)
                                        (spawn (τ (let1 (io (sync (tcp-accept-evt l)))
                                                    (note! 'accepted)
                                                    (close-input-port (car io))
                                                    (close-output-port (cadr io))))))
                                      '(a b))
                            (spawn (τ (cml/sleep 0.2) (cml/shutdown 'watchdog)))
                            (cml/exit))
                         quantum: 1))
         (close-input-port cin)
         (close-output-port cout)
         (tcp-close l)
         (⊦= '(watchdog accepted) r)))))

  ((test/utf-8-split-across-segments _)
   ; a peer splits a UTF-8 sequence across two segments: the reader waits for its end without
   ; blocking the process (a read-char on the lead byte would block in the tcp layer), so the
   ; ticker and the writer, a CML thread of the same process, keep running
   (let* ((l (tcp-listen 0 4 "127.0.0.1"))
          (port (tcp-listener-port l))
          (log (parameterize ((tcp-read-timeout 2000))
                 (log-of (τ (let* ((c (sync (tcp-connect-evt "127.0.0.1" port)))
                                   (s (sync (tcp-accept-evt l)))
                                   (fd (port->fileno (cadr c)))
                                   (ticks 0))
                              (spawn (τ (let loop () (cml/sleep 0.01) (set! ticks (add1 ticks)) (loop))))
                              (spawn (τ (file-write fd (bytevector 99 97 102 #xC3))
                                        (cml/sleep 0.1)
                                        (file-write fd (bytevector #xA9 33 10))))
                              (note! (condition-case (sync (input-line-evt (car s))) (e () 'blocked)))
                              (note! (> ticks 3))
                              (for-each close-input-port (list (car c) (car s)))
                              (for-each close-output-port (list (cadr c) (cadr s)))))))))
     (tcp-close l)
     (⊦= '("café!" #t) log)))

  )

(define-suite cml-io-chanio-suite

  ((test/round-trip _)
   ; an output port and an input port over the same channel: each flush sends one string and
   ; closing sends #!eof, which ends the input port for good
   (⊦= '(("hello" "world" "!" #!eof #!eof) ("hello\nworld" "\n!" #!eof))
       (begin
         (list
           (log-of (τ (let* ((ch (make-channel))
                             (out (open-channel-output-port ch))
                             (in (open-channel-input-port ch)))
                        (spawn (τ (display "hello\nworld" out)
                                  (flush-output out)
                                  (display "\n!" out)
                                  (close-output-port out)))
                        (note! (read-line in))
                        (note! (read-line in))
                        (note! (read-line in))
                        (note! (read-line in))
                        (note! (read-char in)))))
           (log-of (τ (let* ((ch (make-channel))
                             (out (open-channel-output-port ch)))
                        (spawn (τ (display "hello\nworld" out)
                                  (flush-output out)
                                  (display "\n!" out)
                                  (close-output-port out)))
                        (note! (recv ch))
                        (note! (recv ch))
                        (note! (recv ch)))))))))

  ((test/blocked-channel-port-reader-is-a-deadlock _)
   ; a reader blocked on a channel port that nobody writes to is an ordinary deadlock, as in ML
   ; (ChanIO is built from channels only), and run-cml returns 'failure: the reader used to arm a
   ; 0.1 s close check, so the run never ended.  Each run is made in a child process, killed
   ; after 10 s, so that a regression fails instead of hanging the suite
   (define (status-in-a-child thunk)
     (let1 (pid (process-fork (τ (emergency-exit (if (eq? 'failure (run-cml thunk)) 0 1)))))
       (let loop ((i 0))
         (receive (p normal? code) (process-wait pid #t)
           (cond
             ((not (zero? p)) (if (and normal? (zero? code)) 'failure (list 'exited code)))
             ((< i 1000) (sleep-ms 10) (loop (add1 i)))
             (else (process-signal pid) (process-wait pid) 'hung))))))
   (⊦= 'failure (status-in-a-child (τ (read-line (open-channel-input-port (make-channel))))))
   (⊦= 'failure (status-in-a-child (τ (sync (input-line-evt (open-channel-input-port (make-channel)))))))
   (⊦= 'failure (status-in-a-child (τ (let1 (in (open-channel-input-port (make-channel)))
                                        (spawn (τ (sync (input-char-evt in))))
                                        (sync (input-all-evt in)))))))

  ((test/events-on-channel-ports _)
   ; input events on a channel port wait on the channel; a timeout in the middle of a line keeps
   ; the partial line, chars and empty strings are accepted
   (⊦= '(timeout "abcd" #\e "fg" #!eof)
       (log-of (τ (let* ((ch (make-channel))
                         (in (open-channel-input-port ch)))
                    (spawn (τ (send ch "ab") (cml/sleep 0.03) (send ch "") (send ch #\c) (send ch "d\ne") (send ch "fg")
                              (send ch #!eof)))
                    (note! (sync/timeout (input-line-evt in) 0.01 'timeout))
                    (note! (sync (input-line-evt in)))
                    (note! (sync (input-char-evt in)))
                    (note! (sync (input-all-evt in)))
                    (note! (sync (input-line-evt in))))))))

  ((test/write-string-evt-is-a-rendezvous _)
   ; on a channel output port write-string-evt commits exactly when a receiver takes the string
   (⊦= '(timeout "buffered+now" sent)
       (log-of (τ (let* ((ch (make-channel))
                         (out (open-channel-output-port ch)))
                    (display "buffered+" out)
                    (note! (sync/timeout (write-string-evt out "now") 0.01 'timeout))
                    (spawn (τ (note! (recv ch))))
                    (sync (write-string-evt out "now"))            ; the receiver runs first
                    (note! 'sent))))))

  ((test/write-string-evt-and-a-flush _)
   ; the buffered output is taken when write-string-evt, or the send of a flush, commits: here
   ; another thread flushes while the event waits for a receiver, the event commits first and
   ; carries everything, in the order it was written, and the flush then has nothing left to send:
   ; it returns without sending "" (ML's writer never sends an empty vector on its channel)
   (⊦= '("abcqX" ok flushed nothing)
       (log-of (τ (let* ((ch (make-channel))
                         (out (open-channel-output-port ch)))
                    (display "abc" out)
                    (let ((f (spawn (τ (cml/yield) (display "q" out) (flush-output out))))
                          (a (spawn (τ (note! (condition-case (begin (sync (write-string-evt out "X")) 'ok)
                                                (e () 'raised)))))))
                      (note! (recv ch))
                      (note! (sync/timeout (wrap (join-evt f) (λ ignored 'flushed)) 0.5 'flush-stuck))
                      (sync (join-evt a))
                      (note! (sync/timeout (recv-evt ch) 0.05 'nothing))))))))

  ((test/channel-output-port-never-sends-empty-strings _)
   ; ML's writer only sends non-empty vectors: a write-string-evt of "" with nothing buffered
   ; commits at once, without a receiver, and none of the writes below puts "" on the channel
   (⊦= '(done done "ab" "c" #!eof nothing)
       (log-of (τ (let* ((ch (make-channel))
                         (out (open-channel-output-port ch)))
                    (note! (sync/timeout (wrap (write-string-evt out "") (λ ignored 'done)) 0.2 'blocked))
                    (spawn (τ (sync (write-string-evt out ""))
                              (display "" out) (flush-output out)
                              (sync (write-string-evt out "ab"))
                              (sync (write-string-evt out ""))
                              (display "c" out)
                              (sync (write-string-evt out ""))
                              (flush-output out)
                              (close-output-port out)))
                    (note! (sync/timeout (wrap (write-string-evt out "") (λ ignored 'done)) 0.2 'blocked))
                    (let loop ()
                      (let1 (v (sync/timeout (recv-evt ch) 0.2 'nothing))
                        (note! v)
                        (unless (symbol? v) (loop)))))))))

  ((test/flush-keeps-a-threads-output-in-order _)
   ; a thread displays "X" then waits in write-string-evt "s"; another thread's flush meanwhile must
   ; not deliver "s" before "X" (the flush used to take the buffer at once and queue its send
   ; behind the pending event); a channel input port reads it all, in order
   (⊦= '(("Xs" nothing) "Xsyz")
       (log-of (τ (let* ((ch (make-channel)) (out (open-channel-output-port ch)))
                    (spawn (τ (display "X" out) (sync (write-string-evt out "s"))))
                    (cml/yield)
                    (spawn (τ (flush-output out)))
                    (cml/yield)
                    (note! (let* ((a (recv ch)) (b (sync/timeout (recv-evt ch) 0.05 'nothing))) (list a b)))
                    (let* ((ch (make-channel)) (out (open-channel-output-port ch)) (in (open-channel-input-port ch)))
                      (spawn (τ (display "X" out) (sync (write-string-evt out "s")) (display "yz" out) (close-output-port out)))
                      (cml/yield)
                      (spawn (τ (flush-output out)))
                      (cml/yield)
                      (note! (sync (input-all-evt in)))))))))

  ((test/output-events-on-a-closed-port _)
   ; once a channel output port is closed (it sent #!eof) its output events raise, as a write on
   ; it does, instead of sending more data after the end of the stream
   (⊦= '("a" #!eof raised raised raised nothing-sent)
       (log-of (τ (let* ((ch (make-channel))
                         (o (open-channel-output-port ch)))
                    (spawn (τ (display "a" o) (close-output-port o)))
                    (note! (recv ch))
                    (note! (recv ch))
                    (note! (condition-case (begin (display "b" o) 'ok) (e () 'raised)))
                    (note! (condition-case (begin (sync (write-string-evt o "late")) 'ok) (e () 'raised)))
                    (note! (condition-case (begin (sync (output-evt o)) 'ok) (e () 'raised)))
                    (note! (sync/timeout (recv-evt ch) 0.02 'nothing-sent)))))))

  ((test/peek-char-then-events _)
   ; a direct peek-char keeps the char visible to char-ready? and to the input events
   (⊦= '(#\a #t #\a #\b)
       (log-of (τ (let* ((ch (make-channel))
                         (in (open-channel-input-port ch)))
                    (spawn (τ (send ch "a") (cml/sleep 0.1) (send ch "b")))
                    (note! (peek-char in))
                    (note! (char-ready? in))
                    (note! (sync/timeout (input-char-evt in) 0.02 'stuck))
                    (note! (read-char in)))))))

  ((test/bad-value-reaches-the-syncing-thread _)
   ; a value that is not a string, a char or #!eof arrives while the input event waits on the
   ; channel: the event raises it, as a direct read would, and the port goes on with the next values
   (⊦= '(raised #\g)
       (log-of (τ (let* ((ch (make-channel))
                         (in (open-channel-input-port ch)))
                    (spawn (τ (cml/sleep 0.01) (send ch 'not-a-string) (send ch "good")))
                    (note! (condition-case (sync/timeout (input-char-evt in) 1 'stuck) (e (exn) 'raised)))
                    (note! (sync/timeout (input-char-evt in) 1 'stuck)))))))

  ((test/bad-value-keeps-the-chars-before-it _)
   ; the chars sent before a bad value are not lost with it: the input event that reaches the bad
   ; value raises, and the next one starts with them, whichever input event reads the port
   (define (run mk n)
     (log-of (τ (let* ((ch (make-channel)) (in (open-channel-input-port ch)))
                  (spawn (τ (send ch "hello ")))
                  (spawn (τ (send ch 42)))
                  (spawn (τ (send ch "world\n")))
                  (cml/sleep 0.05)
                  (for-each (λ (i) (note! (condition-case (sync/timeout (mk in) 1 'stuck) (e (exn) 'raised)))) (iota n))))))
   (⊦= '(raised "hello world") (run input-line-evt 2))
   (⊦= '(#\h #\e #\l #\l #\o #\space raised #\w) (run input-char-evt 8)))

  ((test/dropped-ports-are-collected _)
   ; ports dropped without being closed are collected: a port without a descriptor carries its
   ; state and is in no registry, the other ports have weak entries, purged amortized (custom
   ; ports with data of their own) or when their descriptor's bucket is updated
   (define (collected? make)                            ; most of them: a few may still be referenced
     (let1 (ws (map (λ (i) (weak-cons (make) #f)) (iota 100)))
       (gc #t)
       (>= (count (λ (w) (bwp-object? (car w))) ws) 90)))
   (define (custom-port) (make-input-port (τ #\a) (τ #t) void))
   (⊨ (collected? (τ (open-channel-input-port (make-channel)))))
   (⊨ (collected? (τ (open-channel-output-port (make-channel)))))
   (⊨ (collected? (τ (let1 (p (custom-port)) (%port-driver p) p))))
   (⊨ (collected? (τ (let1 (p (open-channel-input-port (make-channel))) (run-cml (τ (sync/timeout (input-char-evt p) 0 #f))) p))))
   (let1 (most (let loop ((i 0) (most 0))
                 (if (= i 2000)
                   most
                   (begin (%port-driver (let1 (p (custom-port)) (##sys#set-port-data! p (list 'its-own)) p))
                          (when (zero? (remainder i 50)) (gc #t))
                          (loop (add1 i) (max most (length (%port-table-others %pdrivers))))))))
     (⊨ (< most 200)))
   (receive (in out) (create-pipe)
     (let1 (p (open-input-file* in))
       (%port-driver p)
       (close-input-port p)
       (file-close out)))
   (receive (in out) (create-pipe)
     (let1 (p (open-input-file* in))
       (%port-driver p)
       (⊦= 1 (length (vector-ref (%port-table-fds %pdrivers) in)))  ; the closed port's entry is gone
       (close-input-port p)
       (file-close out))))

  ((test/ports-without-a-descriptor-carry-their-state _)
   ; the input driver of a string port or a custom port is kept in its data slot (which CHICKEN
   ; leaves unused), found in O(1): it was in a list scanned at every event, so n such ports made
   ; n events O(n^2).  A custom port whose data slot holds its maker's data keeps it
   (define (custom-port) (make-input-port (let1 (s (open-input-string "b\n")) (τ (read-char s))) (τ #t) void))
   (let ((s (open-input-string "a\n")) (c (custom-port)) (o (custom-port)))
     (##sys#set-port-data! o (list 'its-own))
     (⊦= '("a" "b" "b") (run/value (τ (map (λ (p) (sync (input-line-evt p))) (list s c o)))))
     (⊨ (%pdriver? (%own-port-state s)))
     (⊨ (%pdriver? (%own-port-state c)))
     (⊦= '(its-own) (##sys#slot o 9))
     (⊭ (%own-port-state o))
     (⊭ (memq s (map car (%port-table-others %pdrivers))))
     (⊭ (memq c (map car (%port-table-others %pdrivers))))
     (⊨ (and (memq o (map car (%port-table-others %pdrivers))) #t))))

  ((test/many-open-ports-in-linear-time _)
   ; the state of a port is found in O(1): opening n channel ports that stay open, and an event on
   ; each, costs O(n) (every registration copied a list of all the open ports, every lookup scanned
   ; it: 8000 ports took seconds, compiled).  Ten times as many ports must not take thirty times as
   ; long
   (define (cost n)
     (let1 (t0 (current-process-milliseconds))
       (⊦= n (run/value (τ (let1 (ports (map (λ (i) (let1 (ch (make-channel))
                                                      (cons (open-channel-input-port ch) (open-channel-output-port ch))))
                                              (iota n)))
                             (for-each (λ (p) (sync (output-evt (cdr p))) (sync/timeout (input-char-evt (car p)) 0 #f)) ports)
                             (length ports)))))
       (- (current-process-milliseconds) t0)))
   (let* ((small (max 20 (cost 400)))
          (large (cost 4000)))
     (⊨ (< large (* 30 small)))))

  ((test/pipe-to-channel-bridge _)
   ; a thread copies lines from a pipe into a channel port: events on two kinds of ports together
   (⊦= '("one" "two" #!eof)
       (with-pipe
         (λ (p out)
           (log-of (τ (let* ((ch (make-channel))
                             (cin (open-channel-input-port ch))
                             (cout (open-channel-output-port ch)))
                        (spawn (τ (let loop ()
                                    (let1 (l (sync (input-line-evt p)))
                                      (if (eof-object? l)
                                        (close-output-port cout)
                                        (begin (display l cout) (newline cout) (flush-output cout) (loop)))))))
                        (spawn (τ (fd-write! out "one\ntw") (cml/sleep 0.01) (fd-write! out "o\n") (file-close out)))
                        (note! (sync (input-line-evt cin)))
                        (note! (sync (input-line-evt cin)))
                        (note! (sync (input-line-evt cin))))))))))

  )

(unittest/✓ cml-io-ports-suite)
(unittest/✓ cml-io-os-suite)
(unittest/✓ cml-io-tcp-suite)
(unittest/✓ cml-io-chanio-suite)
