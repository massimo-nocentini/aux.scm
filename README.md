# aux.scm

In order to install the `aux` modules just type in terminal:

```bash
make install -sudo
```

provided that a Chicken scheme is installed on your box.

## Included modules

The repository contains a collection of small CHICKEN Scheme modules, including
functional data structures, stream utilities, nondeterministic programming
helpers, and foreign-function wrappers.

### `(aux fds sbral)`

`(aux fds sbral)` implements a skew binary random-access list: a persistent
sequence that keeps constant-time access to the front and logarithmic indexed
lookup and update.

Exported entry points include:

- `empty/sbral`
- `cons/sbral`, `car/sbral`, `cdr/sbral`
- `sbral-ref`, `update/sbral`
- `length/sbral`
- `list->sbral`, `sbral->list`
- `map/sbral`, `filter/sbral`, `exists?/sbral`

Example:

```scheme
(import (aux fds sbral))

(define values (list->sbral '(a b c d)))

(sbral-ref values 2)
;; => c

(sbral->list (update/sbral 1 'B values))
;; => (a B c d)

(sbral->list (cons/sbral 'z values))
;; => (z a b c d)
```

### `(aux anthropic)`

`(aux anthropic)` talks to the Anthropic Messages API — `POST
https://api.anthropic.com/v1/messages` — as ordinary Scheme data: a JSON object
is a list of two-element `(symbol value)` lists, a JSON array is a vector, and
`null` is `(void)`, exactly as `(aux simdjson)` decodes them.

HTTPS is done by shelling out to the `curl` binary through `(chicken process)`,
because the container this repository is tested in has neither the
`http-client` egg nor `openssl`. That is hidden behind the `anthropic/transport`
parameter, whose contract is `(transport url headers body stream?) ->
anthropic-wire`, so a native backend can replace it without any call site
changing — and so the offline test suite runs against a stub with no network and
no key.

Exported entry points include:

- `anthropic/ask`, `anthropic/messages` — the blocking call
- `anthropic/stream` — server-sent events, reassembled into the same shape
- `anthropic/converse` — the agentic tool-use loop
- `define-tool`, `define-tool/strict` — a JSON `input_schema` derived from a
  Scheme lambda list
- `anthropic-message/user`, `anthropic-message/assistant`,
  `anthropic-block/text`, `anthropic-block/tool-result`
- `anthropic-response/text`, `anthropic-response/tool-uses`,
  `anthropic-response-stop-reason`, `anthropic-usage/prompt-tokens`
- `anthropic-json/parse`, `anthropic-json/write` — a byte-length-correct parser
  and a JSON-conformant encoder, both of which should eventually move into
  `(aux simdjson)`
- the parameters `anthropic/api-key`, `anthropic/model`, `anthropic/transport`,
  `anthropic/retries`, `anthropic/backoff`, `anthropic/sleep`

```scheme
(import (aux base) (aux anthropic) (chicken string))

(anthropic/ask "What is the capital of France?")
;; => "Paris."

(define-tool (get_weather
               (location string "The city and state, e.g. San Francisco, CA")
               (unit (enum "celsius" "fahrenheit") "Unit of temperature" (default "celsius")))
  "Get the current weather in a given location."
  (conc "22 " unit " in " location))

(receive (response transcript)
    (anthropic/converse (list (anthropic-message/user "Weather in Paris and Rome?"))
                        tools: (list get_weather/tool))
  (anthropic-response/text response))
;; => "It is 22 celsius in Paris and 22 celsius in Rome."

(anthropic/stream (list (anthropic-message/user "Write a haiku about Scheme."))
                  on-text: (lambda (fragment) (display fragment) (flush-output)))
```

The API key is read from `ANTHROPIC_API_KEY` at call time, or from the
`anthropic/api-key` parameter. It is never an argv element: it goes to `curl` in
a `0600` temporary file via `--header @FILE`, and the request body goes on
`curl`'s stdin via `--data-binary @-`, so neither appears in `ps`.

The test suite is offline by default: `make test` runs `test/anthropic.scm`
against a stub transport with no network and no key. The live suite is opt-in:

```bash
export ANTHROPIC_API_KEY=sk-ant-...
cd src && make test-anthropic-live
```

### `(aux cml)`

`(aux cml)` is a port of Concurrent ML, as found in SML/NJ
(`smlnj/libraries/cml`), to CHICKEN Scheme: first-class synchronous events,
lightweight threads, channels and the rest of the CML library, together with the
`cml-lib` utilities (Multicast, SimpleRPC, TraceCML) and an event-valued IO / OS
layer.

The design follows the ML sources closely:

- threads are first-class continuations driven by its own scheduler, exactly as
  CML does with `callcc`; there is no srfi-18 underneath. The scheduler keeps
  ML's two ready queues, its non-nesting atomic flag and its
  `atomicBegin`/`atomicEnd`/`dispatch` discipline. Because threads switch with
  `call/cc`, the before/after thunks of a thread's `dynamic-wind` frames run at
  every switch, an after thunk on behalf of the thread being switched to: they
  must not use CML operations, and `dynamic-wind` cannot protect a critical
  section (take and put an mvar instead);
- scheduling is **cooperative**: switches happen at CML operations (sync, spawn,
  yield, blocking). Every operation, polls and each channel rendezvous
  included, is also a clock tick, and ML's quantum and fairness heuristic are
  applied on those ticks (with one thread promoted from the compute-bound queue
  at every preemption, so that the deterministic ticks cannot starve it), but a
  thread that computes without calling CML is never preempted. A preemption
  polls the timeouts, and the descriptors and child processes once 2 ms (or ten
  times as long as their last poll took) have passed since their last poll, so
  that thousands of idle I/O waiters do not slow down the other threads;
- base events implement `event.sml`'s poll / enabled / blocked protocol, with
  priorities, shared transaction ids, and `guard`/`with-nack` forced at every
  sync;
- everything runs inside `(run-cml thunk #!key quantum)`. It returns the status
  given to `cml/shutdown` (`'success` when called with no argument), or
  `'failure` when no thread can run any more: as in ML, a run whose threads all
  finish or block without calling `cml/shutdown` counts as a deadlock, so
  `(run-cml (lambda () (do-work)))` returns `'failure`; end the thunk with
  `(cml/shutdown)` to get `'success`. Every session starts with fresh scheduler
  state (ready queues, timeouts, pending I/O, tids); registered cleaners,
  logged channels, mailboxes and servers, uncaught-exception handlers, trace
  settings and `cml/debug?` are global and persist across sessions, as in ML,
  and children still running when a session ends are reaped by later ones.
  When no thread is runnable, the idle scheduler sleeps in `file-select` until
  the next timeout or ready descriptor, polling every 5 ms instead while child
  processes or input on ports without a descriptor are awaited (there is no
  SIGCHLD handler); while port events wait on descriptors it also wakes once,
  0.1 s after a thread last ran, to look for ports that another thread closed;
- times are in real seconds, an ML `'a option` is `'()` or `(list v)`, and
  CML's own errors are conditions of kind `(exn cml <kind>)`, e.g.
  `(exn cml put)` for a double put, `(exn cml not-running)` or
  `(exn cml barrier)`; bad arguments (sync on a non-event, a non-port given to
  a port event, a bad count, a non-procedure given to `spawn` or
  `cml/add-cleaner!`, an ivar given to an mvar operation, a command that is not
  a string, ...) raise ordinary errors of kind `(exn)` in the caller, which an
  `(exn cml)` handler does not catch.

Exported entry points include:

- events: `never-evt`, `always-evt`, `wrap`, `wrap-handler`, `guard`,
  `with-nack`, `choose`, `choose*`, `sync`, `select`, `select*`, `select/case`,
  `sync/timeout` (with 0 seconds it is a poll: the event's value whenever it is
  ready, the default otherwise)
- threads: `spawn`, `spawn/call`, `current-tid`, `join-evt`, `cml/yield`,
  `cml/exit`, `tid=?`, `tid-compare`, `tid->string`, `make-thread-property`,
  `make-thread-flag`, `default-exn-handler`
- channels and timeouts: `make-channel`, `send`, `recv`, `send-evt`, `recv-evt`,
  `send-poll`, `recv-poll`, `timeout-evt`, `at-time-evt`, `cml/now`, `cml/sleep`
- synchronization variables: `make-ivar`, `ivar-put!`, `ivar-get`,
  `ivar-get-evt`, `make-mvar`, `mvar-put!`, `mvar-take!`, `mvar-get`,
  `mvar-swap!` and their `-evt`/`-poll` variants; `make-mailbox`,
  `mailbox-send!`, `mailbox-recv`, `mailbox-recv-evt`; `make-barrier`,
  `barrier-enroll`, `barrier-wait`, `barrier-wait-evt`, `barrier-resign`;
  `make-result`, `result-put!`, `result-put-exn!`, `result-get`, `result-get-evt`
- running: `run-cml`, `cml-running?`, `cml/shutdown`, `cml/add-cleaner!`,
  `cml/log-channel!`, `cml/log-mailbox!`, `cml/log-server!`, `cml/version`,
  `cml/debug`
- IO / OS: `io-evt`, `poll-evt`, `process-evt`, `system-evt`, `cml/system`,
  `cml/execute` (with an optional environment, as `Unix.executeInEnv`),
  `input-char-evt`, `peek-char-evt`, `input-line-evt`, `input-string-evt`,
  `input-evt` (`TextIO.inputEvt`: whatever is available), `input-all-evt`,
  `output-evt`, `write-string-evt`,
  `open-channel-input-port`, `open-channel-output-port`, `tcp-accept-evt`,
  `tcp-connect-evt`
- cml-lib: `make-multicast-channel`, `multicast-port`, `multicast-copy-port`,
  `multicast!`, `multicast-recv`, `multicast-recv-evt`; `make-rpc`,
  `make-rpc/in`, `make-rpc/out`, `make-rpc/in-out`, `make-rpc/state`;
  `trace-module`, `trace-on!`, `trace-off!`, `trace-on-only!`, `trace-status`,
  `trace`, `trace-to`, `watch`, `unwatch`, `set-uncaught-handler!`,
  `add-uncaught-handler!`

`guard` is CML's combinator, so clients should import
`(except (chicken base) guard)` to hide the R7RS exception syntax.

```scheme
(import scheme (except (chicken base) guard) (aux cml))

(define (square-server)
  (receive (call entry-evt) (make-rpc (lambda (n) (* n n)))
    (spawn (lambda () (let loop () (sync entry-evt) (loop))))
    call))

(run-cml
  (lambda ()
    (let ((ch (make-channel))
          (square (square-server)))
      (spawn (lambda () (for-each (lambda (i) (send ch i)) '(1 2 3))))
      (let loop ((acc '()))
        (select/case
          ((recv-evt ch) (v) (loop (cons (square v) acc)))
          ((timeout-evt 0.1) _ (cml/shutdown (reverse acc))))))))
;; => (1 4 9)
```

Where the port deliberately departs from ML (the Barrier bugs, barrier
waiters woken oldest first (ML wakes them newest first), `make-barrier` taking
the initial state first (ML: `Barrier.barrier update init`), SimpleRPC
delivering exceptions to the caller, TraceCML without servers, port events that
never lose input to a losing `select`, nacks set right after the commit and also
when a sync is abandoned while forcing or polling or by the death of its thread,
wrap functions run at the sync's own continuation, waiter queues cleaned in
amortized O(1) per enqueue, timeouts kept in a heap, `multicast!` without a
server thread but yielding at every message, a write to a pipe whose
reader has exited raising EPIPE, `cml/execute` searching PATH (but not when
an environment is given, as in ML) and passing the command as given as
argv[0], ...), the header of each part of
`src/aux.cml.scm` says so.

Not ported:

- signal-driven preemption: CHICKEN cannot safely capture continuations inside
  a signal handler, so preemption happens only at CML operations;
- the `OldCML` compatibility shim, which upstream no longer builds;
- the SML Basis plumbing (the `PRIM_IO`/`STREAM_IO` functors, stream
  positions, buffer modes), because CML events work directly on CHICKEN ports;
- `exportFn` and heap export, which have no CHICKEN equivalent;
- the Win32 glue;
- SMLNJ-Util atoms, because Scheme symbols already are atoms;
- the socket library's phantom types, UDP and Unix-domain sockets, because only
  TCP is available through `(chicken tcp)`.

The suites run with `cd src && make test-cml` (`test/cml.scm`,
`test/cml-lib.scm`, `test/cml-io.scm`).
