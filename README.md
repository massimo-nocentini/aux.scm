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

### `(aux commons)`

`(aux commons)` is a prelude of small combinators: curried list helpers, `values`
plumbing, folds, grouping, closures of a step function, and a few numeric and
symbol odds. The code comes from the shared `commons.scm` of the author's old
`on-scheme` repository, ported to CHICKEN 6.

The module holds only the helpers that `(aux base)` does not already provide.
Helpers with an `(aux base)` equivalent are not redefined here, because in
CHICKEN 6 defining an imported name overwrites that binding for every other
importer. The module header maps each old name to its replacement: `let₁` is
`let1`, `undefined?` is `void?`, `display-on-port` is `display/port`, `match₁`
is `match1/first`, `memoize` is `memoize!` in `(aux tabling)`, and `push!` takes
its arguments swapped, `(push! val var)`. Its `remove-duplicates/last` keeps the
*last* occurrence of each element and returns them in reverse order.
`remove-duplicates` from `(aux base)` keeps the first occurrences in order.

Exported entry points include:

- `○` (`compose`), `identity*`, `collect-values`, `eternity`, `cond/λ`
- `fmap`, `fapply`, `ffilter`, `fsort`, `flist-ref`, `fvector-ref`,
  `equals-to?`, `=to?`
- `map/with-index`, `map/call-with-values`, `map/values`, `map/tree`
- `accumulator`, `foldl1`, `foldl1/lshift`, `group`, `tuple/pred?`, `within?`
- `rtc`, the reflexive and transitive closure of a step function, up to an
  `eq?` fixed point
- `remove-duplicates/last`
- `sub2`, `≠`, `⁻¹`, `²`, `number->symbol`, `to-string`, `call+stdout`
- `subscripts`, `symbol∼`, `symbol∼subscripts`

Example:

```scheme
(import (aux base) (aux commons))

((fsort cdr <) '((a . 2) (b . 3) (c . 1)))
;; => ((c . 1) (a . 2) (b . 3))

((map/with-index (λ (i) (λ (x) (list i x))) 10) '(a b c))
;; => ((10 a) (11 b) (12 c))

((map/tree add1) '(1 (2 3) ((4)) 5))
;; => (2 (3 4) ((5)) 6)

(collect-values (τ (values 1 2)))
;; => (1 2)

((rtc (λ (n) (if (< n 5) (add1 n) n))) 0)
;; => (0 1 2 3 4 5)

((tuple/pred? <) '(1 2) '(2 3) '(3 4))
;; => #t

(remove-duplicates/last '(a b a a c d c e e))
;; => (e c d a b)

(symbol∼subscripts 'g123)
;; => g₁₂₃

(call+stdout (τ (display "hello") 'result) list)
;; => (result "hello")
```

### `(aux tabling)`

`(aux tabling)` provides memoization and tabled definitions. The code comes from
the tabling part of the old `commons.scm` in the author's `on-scheme`
repository.

`memoize!` is not the same as `memoize` from `(aux base)`. It uses
`mutate-procedure!` to patch the procedure object in place, so recursive calls
through the original binding, such as a `letrec`-bound `F`, are memoized too.
`memoize` returns a new wrapper instead, so that recursion stays exponential. A
procedure made by `define-tabled`, `letrec-tabled` or `lambda-tabled` accepts
two extra keyword arguments. With `fresh: #t` it recomputes the value even when
the arguments are already in the table. With `store: #f` it does not save the
computed value. The optional `λH` binding is a thunk, visible only in the body,
that returns the hidden hash table. A procedure that returns zero or several
values has all of them tabled, kept in the table in a `tabled/values` box; a
single value is kept as it is.

Exported entry points include:

- `memoize!`
- `define-tabled`, `letrec-tabled`, `lambda-tabled` (also as
  `(lambda-tabled λH → (arg ...) body ...)`)
- `tabled/get-hidden-hash-table`, a sentinel that a tabled procedure may answer
  with its table
- `hash-table-ref/store`, `hash-table-ref/maybe`

Example:

```scheme
(import (aux base) (aux tabling))

(define-tabled pascal
  (lambda (n k)
    (cond
      ((and (zero? n) (zero? k)) 1)
      ((zero? n) 0)
      ((zero? k) (pascal (sub1 n) 0))
      (else (+ (pascal (sub1 n) (sub1 k)) (pascal (sub1 n) k))))))

(pascal 100 50)
;; => 100891344545564193334812497256

(define fibonacci
  (letrec ((F (λ (i)
                (if (< i 2) i (+ (F (- i 1)) (F (- i 2)))))))
    (memoize! F)))

(fibonacci 100)
;; => 354224848179261915075

(letrec-tabled ((ev? (lambda (n) (if (zero? n) #t (od? (sub1 n)))))
                (od? (lambda (n) (if (zero? n) #f (ev? (sub1 n))))))
  (list (ev? 10) (od? 10)))
;; => (#t #f)

(define calls 0)
(define sq (lambda-tabled (x) (set! calls (add1 calls)) (* x x)))

(list (sq 3) (sq 3) calls)
;; => (9 9 1)

(list (sq 3 fresh: #t) calls)
;; => (9 2)
```

### `(aux variable)`

`(aux variable)` introduces distinct logic variables with `fresh`. The code
comes from `promise.scm` in the author's old `on-scheme` repository. The module
has a new name because that file has nothing to do with promises.

A `variable` record prints as its symbol with subscript digits, so `(V 'g123)`
shows `g₁₂₃`. `V` rejects any argument that is not a symbol. Records compare
with `equal?` slot by slot, so `(equal? (V 'a) (V 'a))` holds. The names given
by `fresh` come from `gensym`, so they depend on the global gensym counter. This
module's `fresh` is not the same as `fresh°` or `freshª` from
`(aux kanren micro)`. `(aux kanren unionfind)` also exports `V`, `fresh₁`,
`fresh` and `variable?` with different values, so import the two modules
together only with a `prefix` or `rename` (see `(aux kanren unionfind)` below).

Exported entry points include:

- the record `variable`: `make-variable`, `variable?`, `variable-s`
- `V`, a constructor that checks its argument
- `fresh₁`, `fresh`

Example:

```scheme
(import (aux base) (aux variable))

(V 'g123)
;; => g₁₂₃

(equal? (V 'hello) (V 'hello))
;; => #t

(fresh (v w) (variable? v))
;; => #t

(fresh (v w) (equal? v w))
;; => #f
```

### `(aux continuation classic)`

`(aux continuation classic)` collects classic continuation operators in Feeley's
`continuation-capture` style, inspired by "The Seasoned Schemer" and by chapter
16 of Springer and Friedman, "Scheme and the Art of Programming". It complements
`(aux continuation)`, whose `letcc`, `letcc*`, `callcc` and `trycc` are reused
here, never redefined, so both modules can be imported together. The code comes
from the author's old on-scheme repository (`src/continuations.scm` and
`introduction-to-continuations.scm`).

Exported entry points include:

- `continuation->λ` — a raw continuation object as a variadic procedure; like
  a `call/cc` procedure, it leaves and enters the `dynamic-wind` frames (and so
  the `parameterize` bindings) between the call and the capture, which a bare
  `continuation-return` does not, so resume a raw continuation through it
- `letcc/raw` — binds both the raw continuation and its procedure wrapper
- `apply/cc` — applies a procedure passing the current continuation last
- `escapecc` — escape with an `(else β)` fallback or an `(=> f)` receiver
- `trycc/named` — clauses that each name their own skip continuation
- `set/cc!`, `cond/cc`
- `current-continuation/cont` (the raw object), `current-continuation/λ`
- `letcc/escaper` — a scoped `make-escaper` (Springer and Friedman, ch. 16)

`cond/cc` keeps a known quirk of the original on purpose: its `(pred? handling)`
clauses are dead code, so `(cond/cc (number? add1) (else (λ (k) (+ 2 (k 3)))))`
gives 3.

Example:

```scheme
(import (aux base) (aux continuation) (aux continuation classic))

(define (leftmost l)
  (escapecc
    (hop (let L ((ll l))
           (cond
             ((null? ll) '(no symbol here))
             ((symbol? (car ll)) (hop (car ll)))
             (else (L (car ll)) (L (cdr ll))))))
    (else l)))

(leftmost '(((a) b) (c d)))
;; => a

(leftmost '((() ())))
;; => ((() ()))

(trycc/named
  (skip1 (list 1 (skip1 'discard)))
  (skip2 (list 2))
  (else 'useless))
;; => (2)

(trycc/named
  (skip1 (list 1 (skip1 'discard)))
  (skip2 (list 2 (skip2 'discard)))
  (else => identity))
;; => (discard discard)

(apply/cc (λ (a b k) (cons a (k 'escaped))) '(1 2))
;; => escaped

(letcc/escaper escaper (+ ((escaper *) 5 2) 3))
;; => 10

(let ((n 3) (acc '()))
  (let1 (k (current-continuation/λ))
    (push! n acc)
    (unless (zero? n)
      (set! n (sub1 n))
      (k k)))
  acc)
;; => (0 1 2 3)
```

### `(aux continuation amb)`

`(aux continuation amb)` implements McCarthy's `amb` with a stack of failure
continuations, and a small SAT solver built on it. `amb` takes a procedure that
receives four operators, `(ε ? ✗ ✓)`: choose among a list of values, assert a
condition, fail back to the latest choice point, and collect a result; when the
choices are exhausted `amb` returns the collected results, in order. The code
comes from the author's old on-scheme repository (`src/continuations.scm`).

Exported entry points include:

- `amb`
- `sat-solve` — `(sat-solve (var ...) formula)` over `and`, `or`, `not`,
  `implies`, `#t` and `#f`
- `implies` — exported because `sat-solve` matches it as a literal

This module lives in the extension `aux.continuation.classic`, so
`(aux continuation classic)` must be imported before `(aux continuation amb)`;
importing `(aux continuation amb)` alone fails with `cannot load extension:
aux.continuation.amb`. `sat-solve` keeps a known bug of the original on purpose:
each assertion is checked while the other variables are still `(void)`, which
counts as true, so models can be missed or duplicated (`(sat-solve (x y) (or x
y))` misses `(#f #t)`).

Example:

```scheme
(import (aux base) (aux continuation classic) (aux continuation amb))

(amb (λ (ε ? ✗ ✓)
       (let* ((a (ε '(1 2 3 4 5 6 7)))
              (b (ε '(1 2 3 4 5 6 7)))
              (c (ε '(1 2 3 4 5 6 7))))
         (? (= (* c c) (+ (* a a) (* b b))))
         (✓ (list a b c))
         (✗))))
;; => ((3 4 5) (4 3 5))

(amb (λ (ε ? ✗ ✓) (✗)))
;; => ()

(sat-solve (a b c)
  (and (implies a (not b)) (not a) c))
;; => ((#f #f #t))

(sat-solve (x y) (or x y))
;; => ((#t #t) (#t #f))
```

### `(aux schemer seasoned)`

`(aux schemer seasoned)` holds exercises from chapters 11-14 of "The Seasoned
Schemer", by Friedman and Felleisen: collectors (CPS) and escaping
continuations. The code comes from the author's old on-scheme repository
(`src/seasoned-schemer.scm`); its matchable patterns are rewritten with
`match/first` from `(aux base)`, and its `escape`, `try` and `apply/cc` are the
`escapecc`, `trycc/named` and `apply/cc` of `(aux continuation classic)`.

Exported entry points include:

- `multi-insert*&co` — a collector returning the new sexp and two counts
- `two-in-a-row?`, `two-in-a-row?&hop`
- `intersect`, `intersect/letrec` (the original `intersect-old`),
  `intersect+all`
- `comb-upto-last` — a generalised `rember-upto-last`
- `leftmost/awkward`, `leftmost/awkward+letcc`, `leftmost/escape+explicit`,
  `leftmost/escape`
- `rember1*/letcc`, `rember1*/try` — the latter prints its skip reasons on the
  current output port

Example:

```scheme
(import (aux schemer seasoned))

(multi-insert*&co 'tuna 'anchovy 'orange
                  '((((orange fish)) apple anchovy) apple (orange))
                  list)
;; => (((((orange tuna fish)) apple tuna anchovy) apple (orange tuna)) 1 2)

(two-in-a-row?&hop '(j f r e k s s))
;; => #t

(intersect+all '((3 mango and) (3 kiwis and) (3 hamburgers)))
;; => (3)

(intersect+all '((3 steaks and) (no food and) () (3 diet hamburgers)))
;; => ()

(comb-upto-last 'a -1 '(a b a d a e f))
;; => (e f)

(comb-upto-last 'a 2 '(b c d a e f a g h))
;; => (e f)

(leftmost/escape '((() ((() (())))) b (c)))
;; => b

(rember1*/letcc 'more '((delicious) more (food)))
;; => ((delicious) (food))
```

### `(aux stream sicp)`

`(aux stream sicp)` implements SICP-style lazy streams: a stream is either `'()`
or a promise that, once forced (possibly several times), yields `'()` or a pair
`(car . stream)`. `stream:cons` expands to `cons§` from `(aux stream)`, and
`stream:null?`, `stream:car` and `stream:cdr` force repeatedly, so streams built
by `(aux stream)` can be consumed too. The code comes from the author's old
`on-scheme` repository (`streams.scm`).

Every procedure carries the `stream:` prefix, so the module can be imported
together with `(aux stream)` and its `§`-suffixed names without clashes. The
original `Λ` ("big lambda", a lambda whose body is a delayed stream) is renamed
`λ§`, because `(aux base)` already exports a different `Λ`.

Exported entry points include:

- `stream:empty`, `stream:cons`, `:⁺`, `stream:singleton`, `λ§`
- `stream:null?`, `stream:car`, `stream:cdr`, `stream:cadr`, `stream:cddr`,
  `stream:ref`
- `letdelay`, `define-delay` — recursive (and mutually recursive) stream
  definitions
- `list->stream`, `list->`, `stream:->list`, `list○take`, `stream:iterator`
- `stream:map` (tree recursion with `*: #t`), `stream:filter`,
  `stream:append-map`, `stream:foldr` (lazy in its second argument),
  `stream:take`, `stream:take-while`
- `stream:repeat`, `stream:0s`, `stream:zip`, `stream:zip-with`,
  `stream:append`, `stream:merge`, `stream:convolution`; `stream:zip` and
  `stream:zip-with` stop at the end of the shortest stream, unless
  `stream:zip-with` is given a `pad:` value: then an exhausted stream reads as
  that value and zipping stops only when every stream has ended
- `stream:tails`, `stream:prefixes`, `stream:scan`, `stream:tableau`;
  `stream:scan` is a left scan in stream order, `a₀`, `(op a₀ a₁)`,
  `(op (op a₀ a₁) a₂)`, ..., so it also suits non-commutative operators
- `stream:enumerate-upper`, `stream:enumerate-lower`, `stream:enumerate-all`,
  `stream:enumerate-weighted`
- `stream:§₂`, `stream:§` (fair interleaving, monadic `mplus`), `stream:>>=`

Example:

```scheme
(import (aux base) (aux stream sicp))

(define nats (letdelay ((N (stream:cons 0 ((stream:map add1) N)))) N))

((list○take 5) nats)
;; => (0 1 2 3 4)

((list○take 5) ((stream:filter odd?) nats))
;; => (1 3 5 7 9)

((list○take 6) ((stream:scan +) nats))
;; => (0 1 3 6 10 15)

((list○take 4) ((stream:scan -) (list->stream '(100 1 2 3))))
;; => (100 99 97 94)

((list○take 6) (stream:§ (stream:repeat 'a) (list->stream '(1 2 3))))
;; => (a 1 a 2 a 3)

(stream:->list ((stream:zip-with +) (list->stream '(1 2 3)) (list->stream '(10 20))))
;; => (11 22)

(stream:->list ((stream:zip-with + pad: 0) (list->stream '(1 2 3)) (list->stream '(10 20))))
;; => (11 22 3)

((stream:ref 3) (:⁺ 'x 'y 'z 'w stream:empty))
;; => w

((list○take 4) (stream:enumerate-upper nats nats))
;; => ((0 0) (0 1) (1 1) (0 2))
```

### `(aux stream series)`

`(aux stream series)` builds on `(aux stream sicp)` to provide formal power
series in McIlroy's style (sum, product, reciprocal, division, composition,
reversion, derivative and integral), classic number sequences, and Taylor
expansions defined by their differential equations. The code comes from the
author's old `on-scheme` repository (`series.scm`).

Many definitions (the `numbers/*`, `primes/*` and `taylor/*` streams) are
module-level streams: they memoize forever, so they are state shared among all
importers. Some exported names are generic and unprefixed (`evens`, `odds`,
`prime?`, `factorization`, `divisable-by?` (sic, kept for compatibility),
`multiples-of`, `not-multiples-of`, `random-numbers`, `riordan-array`,
`formalvar-series`, `Pythagorean-triples`), so beware of clashes when
importing this module together with other number-theoretic modules.

A finite (`'()`-terminated) stream, such as one built by `list->stream`, is a
polynomial: `series:+` and `series:-` pad it with zeros, so the operations built
on them (`series:×`, `series:/`, `series:⁻¹`, `series:exp`, `series:expt`,
`series:√/∞`, ...) see its trailing zeros, whereas a bare `stream:zip-with`
stops at the shortest stream. `list->poly` builds the infinite zero-padded
stream explicitly.

Exported entry points include:

- `series:0`, `series:const`, `stream:1`, `stream:1s`, `list->poly`,
  `series:range`, `series:from`
- `series:+`, `series:-`, `series:*` (scaling), `series:×`, `series:×*`
  (bivariate), `series:expt`, `series:⁻¹`, `series:/`, `series:/&inversion`
- `series:∫`, `series:∂`, `series:○` (composition), `series:◇` (reversion),
  `series:exp`, `series:√/∞`, `series:√`
- `series:log₂`, `series:Euler-transform`, `series:integrator`,
  `series:integrator/∞`, `series:ode-solver-1st`, `series:ode-solver-2nd`,
  `series:Montecarlo`
- `numbers/nats`, `numbers/fibs/∞`, `numbers/lucas/∞`,
  `numbers/triangular`, `numbers/factorials/∞`, `numbers/powers-of-2`, ...
- `primes/∞`, `primes/eratosthenes`, `prime?`, `factorization`,
  `radix-expansion`
- `taylor/exponential`, `taylor/sine`, `taylor/cosine`, `taylor/catalan`,
  `taylor/fibonacci`, `taylor/π`
- `riordan-array`, `formalvar-series`, `Pythagorean-triples`

Example:

```scheme
(import (aux base) (aux stream sicp) (aux stream series))

((list○take 10) numbers/fibs/∞)
;; => (0 1 1 2 3 5 8 13 21 34)

((list○take 10) primes/∞)
;; => (2 3 5 7 11 13 17 19 23 29)

((list○take 6) taylor/exponential)
;; => (1 1 1/2 1/6 1/24 1/120)

((list○take 6) taylor/sine)
;; => (0 1 0 -1/6 0 1/120)

((list○take 7) taylor/catalan)
;; => (1 1 2 5 14 42 132)

((list○take 6) (series:⁻¹ (list->poly '(1 -1))))
;; => (1 1 1 1 1 1)

((list○take 5) (series:× (list->poly '(1 1)) (list->poly '(1 1))))
;; => (1 2 1 0 0)

((list○take 5) (series:+ (list->stream '(1 2 3)) (list->stream '(1))))
;; => (2 2 3)

((list○take 4) (factorization 360))
;; => (3 2 1 0)

(prime? 97)
;; => #t
```

### `(aux games dice-of-doom)`

`(aux games dice-of-doom)` is the Dice of Doom game of "Land of Lisp" (Conrad
Barski, chapters 15 and 18): hexagonal boards, a lazy game tree whose moves are
`(aux stream sicp)` streams, and a computer player that searches the tree with
minimax and α-β pruning up to a given depth. The code comes from the author's
old `on-scheme` repository (`dice-of-doom.scm`), keeping only its live Scheme
code.

A game tree is `(player board moves)`, where `moves` is a stream of `(action
tree)`. The players are given as a *circular* list (e.g. srfi-1's
`circular-list`), whose `car` plays now. `game-tree` is memoized on the board,
the turn order, the spare dice and the first-move flag, so two distinct
circular lists with the same turn order share one tree. As in the original
(and unlike Land of Lisp), `add-new-dice` does not cap the dice of a cell,
passing reinforces with `(sub1 spare-dice)` dice, and `computer-vs-computer`
stops when the search horizon of its first move is exhausted.

Exported entry points include:

- `make-board`, `board-cells`, `board-size`, `board-hexnum`, `gen-board`
  (random, via `pseudo-random-integer`)
- `game-tree`, `game-tree/key-hash` (the hash of its memo table, which reads
  the cells of the board: srfi-69's `equal?-hash` stops before them),
  `game-tree->sexp`, `players-rotation`, `neighbors` (and
  `neighbors/size`, the same on a board size, tabled), `board-attack`,
  `add-new-dice`
- `winners`, `announce-winner`
- `rate-position`, `get-ratings`, `score-board`, `threatened`,
  `limit-tree-depth`, `rate-position/αβ`
- `handle-computer`, `computer-vs-computer`

Example:

```scheme
(import (aux base) (aux stream sicp) (aux games dice-of-doom)
        (only srfi-1 circular-list))

(define board (make-board #((A 1) (B 1) (A 2) (B 1)) 2))

(display board)
;; prints, after a leading newline:
;;     A-1 B-1
;;   A-2 B-1

(stream:->list (neighbors 0 board))
;; => (2 1 3)

(define tree (game-tree board (circular-list 'A 'B) 0 #t))

(game-tree->sexp tree)
;; => (A "\n    A-1 B-1 \n  A-2 B-1 "
;;       (((cell 2 attacks 3) (A "\n    A-1 B-1 \n  A-1 A-1 "
;;                               ((pass (B "\n    A-1 B-1 \n  A-1 A-1 " ())))))))

((rate-position 'A) tree)
;; => 5

(winners (make-board #((A 1) (B 1) (B 1) (A 1)) 2))
;; => (A B)

((computer-vs-computer 2) tree)
;; prints each position ("Current player: A", "Board:", ...), then
;; "The winner is A"
;; => (A)
```

### `(aux fds unionfind)`

`(aux fds unionfind)` implements union-find (disjoint sets) with union by rank
and optional path compression; it is ported from `unionfind.scm` in the
author's old `on-scheme` repository. Unlike its persistent siblings
`(aux fds queue)` and `(aux fds sbral)`, this structure is **mutable**: a
`unionfind` record holds two srfi-69 `equal?` hash tables, one mapping each node
to its parent and one mapping each node to its rank, and unions update them in
place. Use `unionfind-copy` (O(n)) to get an independent copy.

Exported entry points include:

- `unionfind-empty`, `unionfind-new` (calls `(recv U ↑ ↑! ≡ →)` on a fresh
  structure), `unionfind-accessors` (calls `(recv ↑ ↑! ≡ →)` on an existing one)
- `unionfind-≡` — returns a binary union procedure (union by rank; nodes not
  seen before are inserted with rank 0)
- `unionfind-↑` (find without compression), `unionfind-↑!` (find with full
  path compression); both return a node that is not in the structure unchanged
- `unionfind-★` — returns a rank lookup
- `unionfind-keys`, `unionfind-size`, `unionfind-edges`, `unionfind-copy`
- `unionfind-walk`, `unionfind-walk/without-loops`, `unionfind->alist`,
  `unionfind->→+★`

Example:

```scheme
(import (aux fds unionfind))

(define U (unionfind-empty))
(define union! (unionfind-≡ U))
(define find (unionfind-↑ U))

(union! 'a 'b)
(union! 'c 'd)
(union! 'b 'd)

(find 'a)
;; => d

(equal? (find 'a) (find 'c))
;; => #t

(find 'z)
;; => z

(unionfind-size U)
;; => 4

(unionfind-edges U)
;; => 3

((unionfind-★ U) 'd)
;; => 2
```

### `(aux machine env)`

The three `(aux machine ...)` modules port the SC and SECD abstract machines,
with the environments they share, from the author's old `on-scheme`
repository. `(aux machine env)` provides those environments. An environment is a
procedure from an identifier to its value, where `(void)` means unbound.
`((extend E) '(x . 1) '(y . 2) ...)` returns a new environment that answers the
given associations and falls back to `E` for everything else. Every
environment made by `extend` is tabled (see `(aux tabling)`): lookups are cached
in a hidden hash table, and `E->alist` gathers the cached entries of the whole
chain. The hidden tables compare identifiers with `equal?`, so an environment
extended with a `same?:` other than `equal?`, and every extension of it, caches
nothing.

Exported entry points include:

- `E₀` — the empty environment
- `E⁺` — an environment that `eval`s any atom, answering `(void)` on error
- `extend` — takes an optional `same?:` keyword to compare identifiers
  (default `equal?`)
- `E->alist`, `E-null?`

Example:

```scheme
(import (aux base) (aux machine env))

(define E₁ ((extend E₀) '(a . 1) '(b . 2)))
(define E₂ ((extend E₁) '(b . 20)))

(E₂ 'a)
;; => 1

(E₂ 'b)
;; => 20

(E₁ 'b)
;; => 2

(void? (E₂ 'c))
;; => #t

(E-null? E₀)
;; => #t

(E-null? E₂)
;; => #f

((E⁺ 'car) '(x y))
;; => x
```

### `(aux machine sc)`

`(aux machine sc)` is the SC machine, a stack-and-control machine for curried
combinations (Landin; see also Danvy, "A rational deconstruction of Landin's
SECD machine"). A combination is either `(Id identifier)` or `(Comb rator
rand)`, and `curryfy` turns an s-expression such as `'(p a b)` into the
left-nested `((p a) b)`. The machine comes in two flavours over the same
`status` record (a stack `S` and a control list `C`): `→/interpreted` runs
combinations directly, and `→/compiled` runs the instructions that `compile`
produces. Both are one-step transitions. When `C` is empty they return the
*same* status object, which is the fixed point that `rtc` in `(aux commons)`
detects with `eq?`, so `(rtc (→/interpreted E))` runs the machine to the end
and returns the whole trace.

Exported entry points include:

- `Id`, `Comb`, `combination?`, `curryfy`, `value`
- `make-status`, `status-S`, `status-C`, `→/interpreted`
- `Load`, `Apply`, `instruction?`, `compile`, `→/compiled`

The names `Id`, `Comb`, `Load`, `Apply`, `curryfy`, `value`, `compile`,
`make-status`, ... are also exported by `(aux machine secd)`, and
`make-status` and `status?` are also exported by `(aux kanren unionfind)`: never
import any two of these modules without a `prefix` or `rename`. Because the
module exports `*`, the raw `%`-prefixed record API (`make-%Id`, ...) and
`%check` are exported too. They are private by convention only: use the checking
constructors `Id`, `Comb` and `Load` instead.

Example:

```scheme
(import (aux commons) (aux machine env) (aux machine sc))

(define c (curryfy '(p (m a b) c)))
c
;; => ((p ((m a) b)) c)

(define E ((extend E₀)
           '(a . 5) '(b . 3) '(c . 10)
           `(p . ,(lambda (x) (lambda (y) (+ x y))))
           `(m . ,(lambda (x) (lambda (y) (- x y))))))

((value E) c)
;; => 12

(define t ((rtc (→/interpreted E)) (make-status '() (list c))))
(length t)
;; => 14
(status-S (car (reverse t)))
;; => (12)

(define t/c ((rtc (→/compiled E)) (make-status '() ((compile E) c))))
(length t/c)
;; => 10
(status-S (car (reverse t/c)))
;; => (12)
```

### `(aux machine secd)`

`(aux machine secd)` is Landin's SECD machine, with interpreted, compiled and
optimised flavours and the `J` operator (see Danvy, "A rational deconstruction
of Landin's SECD machine", and Danvy and Millikin, "A rational deconstruction of
Landin's J operator"). Expressions are `Id`, `Lambda`, `Comb`, `If` and `J`.
`curryfy` desugars an s-expression into them: it expands `Y` into a
Y-combinator term, rewrites `cond` into nested `If`s, curries multi-argument
`λ`s and turns n-ary applications into left-nested `Comb`s. `value` is a direct
evaluator and raises an error on `J`. `→/interpreted` is the one-step SECD
transition over expressions. `expression->de-bruijn` turns expressions into de
Bruijn terms, which `compile` and the optimising `compile⁺` translate into
instructions for `→/compiled` and `→/compiled⁺`; only `→/compiled⁺` implements
`J`. As in the SC machine, every transition returns the same status object at
the end, so `rtc` from `(aux commons)` runs it to completion.

Exported entry points include:

- `Id`, `Lambda`, `Comb`, `If`, `J`, `expression?`, `curryfy`, `value`
- `status-init`, `make-status`, `status-S`, `status-E`, `status-C`,
  `status-D`, `→/interpreted`
- `expression->de-bruijn`, `Id₋`, `Id₊`, `Lambda₊`, `Comb₊`, `If₊`, `J₊`,
  `value₊`
- `Load`, `Apply`, `Position`, `Position&Apply`, `Closure`, `Enter`, `Exit`,
  `Test`, `Jump`, `compile`, `compile⁺`, `→/compiled`, `→/compiled⁺`

The names shared with `(aux machine sc)` (`Id`, `Comb`, `Load`, `Apply`,
`curryfy`, `value`, `compile`, `make-status`, ...), and the `make-status` and
`status?` also exported by `(aux kanren unionfind)`, mean that these modules
must be imported with a `prefix` or `rename` when used together, for example
`(import (prefix (aux machine sc) sc:) (prefix (aux machine secd) secd:))`. The
`%`-prefixed record API is exported but private by convention, as in
`(aux machine sc)`.

Example:

```scheme
(import (aux base) (aux commons) (aux machine env) (aux machine secd))

(define E ((extend E₀)
           '(one . 1) '(two . 2) '(three . 3)
           `(zero? . ,zero?) `(sub1 . ,sub1)
           `(* . ,(lambda (x) (lambda (y) (* x y))))
           `(² . ,(lambda (x) (* x x)))
           `(+ . ,(lambda (x) (lambda (y) (+ x y))))))

(define twice (curryfy '((λ (f x) (f (f x))) ² three)))
twice
;; => (((λ (f) (λ (x) (f (f x)))) ²) three)

((value E) twice)
;; => 81

(expression->de-bruijn twice)
;; => (((λ (λ (1 (1 0)))) ²) three)

(compile⁺ (expression->de-bruijn twice))
;; => ((Load three) (Load ²) Enter (Closure ((Position 0) (Position&Apply 1) (Position&Apply 1))) Exit Apply)

(define run (λ (sexp)
              (let* ((code (compile⁺ (expression->de-bruijn (curryfy sexp))))
                     (trace ((rtc (→/compiled⁺ E)) (status-init '() code))))
                (car (status-S (car (reverse trace)))))))

(run '((λ (f x) (f (f x))) ² three))
;; => 81

(run '((λ (L) (² two)) (J (λ (z) z))))
;; => 4

(run '((λ (L) (² (L two))) (J (λ (z) z))))
;; => 2

(run '((Y (λ (fact n) (cond ((zero? n) one) (else (* n (fact (sub1 n))))))) three))
;; => 6
```

### `(aux kanren unionfind)`

`(aux kanren unionfind)` and `(aux kanren unionfind reasoned)` port
`microkanren.scm` and `reasoned-schemer.scm` from the author's old `on-scheme`
repository. `(aux kanren unionfind)` is a small miniKanren whose substitution is
a union-find from `(aux fds unionfind)`. It is a separate teaching
implementation, not a variant of `(aux kanren micro)`. A state is a `status`
record holding the union-find and a `depth`, the number of disjunctions taken so
far. Goals return `(aux stream sicp)` streams. `run` returns a stream of
`(term depth)` lists. `run/with-symbols` returns a list in which reified
variables read as `▢₀`, `▢₁`, ...; its count is an expression (`∞`, that is
`+inf.0`, for all answers; `#t` for the first answer or `#f`; `↓` to keep
depths). There are four if-then-else flavours, each with its `cond°/X` form:
`if°/¦` (plain append, unfair), `if°/§` (interleaving, the usual `conde`),
`if°/!` (soft cut, `conda`) and `if°/!!` (first answer only, `condu`). As in the
original, there is no occurs check, vectors and records are not decomposed, and
every `≡` copies the whole union-find.

Exported entry points include:

- `✓`, `✗`, `≡`, `∧`, `∨`, `fresh`, `unify`
- `run`, `run/with-symbols`, `deepening`
- `if°/¦`, `if°/§`, `if°/!`, `if°/!!`, `cond°`, `cond°/¦`, `cond°/§`,
  `cond°/!`, `cond°/!!`
- `V`, `R`, `variable?`, `variable->symbol`, `walk*`, `reify/var`
- `make-status`, `status?`, `status-≡`, `status-depth`, `ε`

Do not import this module, or `(aux kanren unionfind reasoned)`, together with
any of the following without a `prefix` or `rename`. They export the same names
with incompatible values, and CHICKEN 6 silently keeps the binding from the
module imported last, without a warning:

- `(aux kanren micro)`: `null°`, `cons°` and `cond°`
- `(aux kanren arith)`: `car°`, `cdr°` and `append°`
- `(aux variable)`: `V`, `fresh₁`, `fresh` and `variable?`
- `(aux machine sc)` and `(aux machine secd)`: `make-status` and `status?`

For example, `(import (prefix (aux kanren unionfind) uf:))` or
`(import (rename (aux variable) (V var:V)))`.

Example:

```scheme
(import (aux base) (aux kanren unionfind))

(run/with-symbols ∞ (q) (≡ `(,q 4) '(3 4)))
;; => (3)

(run/with-symbols ∞ (v w) (≡ v w))
;; => ((▢₀ ▢₀))

(run/with-symbols ∞ (q) (cond°/§ ((≡ q 'tea)) ((≡ q 'cup))))
;; => (tea cup)

(run/with-symbols ∞ (q) (cond°/! ((≡ q 'tea)) ((≡ q 'cup))))
;; => (tea)

(run/with-symbols ∞ (v) (∧ (≡ v 3) (≡ v 4)))
;; => ()

(run/with-symbols #t (fresh (x) (≡ x 1)))
;; => (#t 0)
```

### `(aux kanren unionfind reasoned)`

`(aux kanren unionfind reasoned)` contains the relations of "The Reasoned
Schemer", written on top of `(aux kanren unionfind)` and ported from the same
`on-scheme` repository, plus Dyck words, Fibonacci and Tartaglia trees, and
sorting with one or two stacks. The co-import warning of `(aux kanren
unionfind)` applies to this module too.

This module lives in the extension `aux.kanren.unionfind`, so `(aux kanren
unionfind)` must be imported before `(aux kanren unionfind reasoned)`;
importing `(aux kanren unionfind reasoned)` alone fails with `cannot load
extension: aux.kanren.unionfind.reasoned`.

Exported entry points include:

- `null°`, `cons°`, `car°`, `cdr°`, `pair°`, `list°`, `append°`
- `tea-cup°`, `split-pea°`, `split-pea₁°`
- `any°`, `always°`, `never°`
- `dyck°`, `fibonacci°`, `tartaglia°`, `stacksort°`, `2stacksort°`

Example:

```scheme
(import (aux base) (aux kanren unionfind) (aux kanren unionfind reasoned))

(run/with-symbols ∞ (v w) (append° v w '(1 2 3)))
;; => ((() (1 2 3)) ((1) (2 3)) ((1 2) (3)) ((1 2 3) ()))

(run/with-symbols 4 (l) (list° l))
;; => (() (▢₀) (▢₀ ▢₁) (▢₀ ▢₁ ▢₂))

(run/with-symbols 5 (α) (dyck° α))
;; => (() (○ ●) (○ ○ ● ●) (○ ● ○ ●) (○ ○ ○ ● ● ●))

(run/with-symbols 3 (v) (∧ (≡ v 'onion) always°))
;; => (onion onion onion)

(run/with-symbols ↓ 3 (α) ((deepening 100) (dyck° α)))
;; => ((() 1) ((○ ●) 7) ((○ ○ ● ●) 17))
```

### `(aux kanren lambda)`

`(aux kanren lambda)` brings λProlog's hereditary Harrop formulas to
`(aux kanren micro)`, after Miller and Nadathur, "Programming with Higher-Order
Logic" (CUP 2012). Terms are λ-tree syntax: `(ƛ (x) body)` is an abstraction
whose Scheme binder `x` gives the object binder, and `(@ f a ...)` an
application, β-reduced at once when `f` is an abstraction. `(∀° (x) g ...)` runs
its goals with `x` bound to a fresh eigenvariable, a constant that no variable
created outside the `∀°` may be bound to. `(⊃° (D ...) g ...)` runs them with
the clauses `D` added to the relations of `define-dynamic-relation`; a clause
is a fact `(rel t ...)`, a rule `((rel t ...) ← g ...)`, or `(∀ (v ...) D)` for
variables of its own. Unification is higher-order pattern unification (L_λ),
with universe levels in place of raising. An equation outside L_λ waits until
it becomes a pattern, and is reported as a residual constraint otherwise, so
the printed answer can then depend on the order of the goals. Micro's own
`=°`, `≠°`, `absent°`, `symbol°` and the relations built on them work on
λ-terms and are scope-safe under `∀°`. The parameter `μkanren-λ-reading` says
how `≠°` reads an eigenvariable: `'generic`, the default and λProlog's reading,
takes it for a name distinct from every other term, and `'extensional` for any
term of its universe.

`°->list/datum` gives the answers as data, `(ƛ (x) (f x))` rather than records,
and drops their constraints as `°->list/ground` does. `°->list` keeps them: a
suspended equation is `(assert (≐ u v))`, and a disequality reads as micro's,
the mgu of its trial `(begin (deny (equal? α t)))`, under `∃` over the variables
the trial creates, or `(begin (deny (≐ u v)))` when the trial only suspends. `≐`
is equality of closed terms up to αβη, and `μkanren-datum` prints a term without
a state. The module registers how `(aux kanren micro show)` reads its answers:
`°->pretty` prints these forms as `(= u v)` and `(≠ u v)`, and the cells view
draws an abstraction as a node with a node per binder. The datum of an answer
cannot tell a λ-term from a list that reads the same, so with this module loaded
any list headed by `ƛ` or `@`, such as `'(@ a b)`, is drawn as a λ-term.

Import `(aux kanren micro)` too: the module does not re-export its goals.
Importing the module registers its extension (see below) for the whole process
and for good, so every query run afterwards, a first-order one too, goes through
its hooks. First-order answers do not change, and micro's six suites pass with
the module loaded (`make test-microkanren-with-lambda`), but they cost time. On
first-order benchmarks of 130 ms or more, searches with `≠°` run 4–17% slower,
around 10% for most, and the others 1–2% slower. Shorter searches with many
constraints can lose more: up to 17% with `symbol°` or `absent°`, up to 39% with
`≠°` (86 ms against 62), and up to 2.2 times when they mix `≠°` with `symbol°`
or `absent°` (22 ms against 10). The module keeps all its data in the state's
one slot for extensions, so no other extension that keeps data there can be
loaded with it.

**Extension protocol.** `(aux kanren lambda)` is a client of a protocol of
`(aux kanren micro)` that teaches the engine new kinds of terms and
constraints. `make-μkanren-extension` takes a name and hooks given by keyword,
each a procedure or `#f`: `term?` recognizes the extension's terms, which are
records; `atom?`, `unify` and `occurs` serve unification and its occurs check;
`post` runs inside `=°` on the bindings it has just made, before their
constraints are handed on; `diseq`, `tag`, `watch` and `parts` serve `≠°`, the
types, the store of disequalities and `absent°`; `value`, `repr`, `terms`,
`forms` and `environment` build the answers. `μkanren-extension-register!`
adds an extension to a global registry, which takes nothing back; registering
again under the same name replaces the extension. A state has a slot,
`μkanren-state-X`, that the engine never reads and every state built from it
carries along, for the extension's data of the branch. `μkanren-unify`,
`μkanren-post-unify`, `μkanren-state-forget`, `μkanren-state-remove-D`,
`μkanren-state-no-D?`, `μkanren-state-indexed?`, `μkanren-state-prefix`,
`μkanren-state-prefix-ids` and `μkanren-state-drop-prefix` are exported for
the hooks. The comment of `μkanren-extension` in `src/aux.kanren.micro.scm`
gives every hook's arguments and answer, and the invariants E1–E6 that the
engine and an extension keep. `(aux kanren micro show)` takes extensions of its
own, made by `make-μkanren-show-extension` with the hooks `datum`,
`constraint` and `cell` and registered by `μkanren-show-extension-register!`.
With no extension registered, the answers are the same as before the protocol,
and a hook site costs a test of the empty registry: searches of more than ten or
so milliseconds run 1–2% slower.

The tests are `make test-microkanren-lambda` (`test/microkanren-lambda.scm`,
which runs many of the book's programs, and `test/microkanren-lambda-limits.scm`)
and `test/microkanren-extension.scm` for the protocol. The design and its
prototype are in `doc/hol-kanren`.

Exported entry points include:

- terms: `ƛ`, `@`, `≐`, `μkanren-abstract`, `μkanren-apply`, `μkanren-hnorm`,
  `μkanren-α=?`, `μkanren-λ-term?`, `μkanren-lam?`, `μkanren-app?`,
  `μkanren-eigen?`, `μkanren-bvar?`, `μkanren-lam-hint`, `μkanren-eigen-name`,
  `μkanren-eigen-level`
- goals: `∀°`, `∀ª`, `⊃°`, `⊃ª`, `define-dynamic-relation`, plus `μkanren-hyps°`,
  `μkanren-clause` and the others that `define-dynamic-relation` and `⊃°`
  expand to
- answers: `°->list/datum`, `μkanren-datum`, `μkanren-λ-show-extension`
- the engine: `μkanren-λ-extension`, `μkanren-λ-reading`, and the data of a
  state, `μkanren-λ-depth`, `μkanren-λ-level`, `μkanren-λ-delayed` (the
  suspended constraints) and `μkanren-λ-disequalities`

Example:

```scheme
(import (aux base) (aux kanren micro) (aux kanren lambda)
        (only (aux kanren micro show) °->pretty))

; typeof (abs M) (arr A B) :- pi x\ typeof x A => typeof (M x) B.
(define-dynamic-relation (typeof° m t)
  (cond°
    ((fresh° (M N B) (=° m `(app ,M ,N)) (typeof° M `(arr ,B ,t)) (typeof° N B)))
    ((fresh° (R A B) (=° m `(abs ,R)) (=° t `(arr ,A ,B))
       (∀° (x) (⊃° ((typeof° x A)) (typeof° (@ R x) B)))))))

(°->list/datum (fresh° (q) (typeof° `(abs ,(ƛ (x) `(abs ,(ƛ (y) x)))) q)))
;; => ((arr α (arr β α)))

(°->list/datum (fresh° (q) (typeof° `(abs ,(ƛ (x) `(app ,x ,x))) q)))
;; => ()

(°->list/datum (fresh° (F) (∀° (x) (=° (@ F x) `(f ,x a)))))
;; => ((ƛ (x) (f x a)))

(°->list/datum (fresh° (q) (∀° (x) (=° q x))))
;; => ()

(°->list #f (fresh° (q) (∀° (x) (=° (@ q x x) x))))
;; => ((λ (α) (assert (≐ (ƛ (x) (@ α x x)) (ƛ (x) x))) α))

(°->pretty (fresh° (q F) (=° q F) (≠° (@ F 'a) 'a)))
;; => ((α : (≠ (@ α a) a)))

(define-dynamic-relation (rev° l k))
(define-relation (reverse° l k)
  (⊃° ((rev° '() k) (∀ (X L K) ((rev° `(,X . ,L) K) ← (rev° L `(,X . ,K)))))
    (rev° l '())))

(°->list/datum (fresh° (q) (reverse° '(1 2 3) q)))
;; => ((3 2 1))

(μkanren-datum (@ (ƛ (x) (ƛ (y) (@ x y))) (ƛ (z) (@ 'c z))))
;; => c

(≐ (ƛ (a) a) (ƛ (b) b))
;; => #t
```

### `(aux pie)`

`(aux pie)` is a port of Pie, the small dependently typed language of "The
Little Typer" by Daniel P. Friedman and David Thrane Christiansen (MIT Press
2018), from its Racket implementation
(https://github.com/the-little-typer/pie, commit 2c89553). It reads a
`#lang pie` file and elaborates its claims, definitions, sameness checks and
expressions in order. Each expression prints its normal form as it is reached,
the TODO reports come after the last form, and the first error ends the run.
The aim is to print what `racket FILE.pie` prints, the same bytes on stdout and
on stderr and the same exit status, 0 or 1 after an error. The one part left out
on purpose is the "context...:" block of Racket's error display, a stack trace
of Racket's own installation.

A file runs with the script `test/pie-run.scm`, the counterpart of
`racket FILE.pie`:

```
$ cat double.pie
#lang pie
(claim double (→ Nat Nat))
(define double
  (λ (n)
    (iter-Nat n 0 (λ (k) (add1 (add1 k))))))
(double 3)
(claim half (→ Nat Nat))
(define half (λ (n) TODO))
$ csi -s src/test/pie-run.scm double.pie
(the Nat 6)
/home/me/double.pie:8:20: TODO:
 n : Nat
---------
 Nat

```

From Scheme, `(pie-run-file "double.pie")` does the same and answers the exit
status instead of exiting, and `pie-main` takes a command line. The pieces of
upstream's `main.rkt` are exported too. `pie-read-module` reads a file,
`pie-module-begin` elaborates its forms, `pie-top-interaction` is one step of
the REPL that Racket opens after a file, `the-pie-ctx` holds the context of the
last file, and `norm` normalizes an expression in a context.

The port follows upstream file by file. Each Racket module becomes a module
`(aux pie NAME)` with the same names and the same order, and its comments cite
the upstream lines as "cf. FILE.rkt:LINE". Three modules have no upstream file
and stand in for Racket itself. `(aux pie racket)` has transparent structs,
boxes, Racket's printer and number syntax, its exceptions and error display,
`(aux pie syntax)` has syntax objects and Racket's reader with its error
messages, and `(aux pie syntax-parse)` has the part of `syntax/parse` that the
parser uses, failure reports included.

`make test-pie` runs `test/pie-units.scm` (the `module+ test` blocks of
upstream and the Racket stand-ins), `test/pie.scm` (all 121 tests of upstream's
`tests.rkt`) and `test/pie-programs.scm`, which runs the programs of `test/pie/`
and compares their output with what Racket 8.15 printed for them. The port was
also tested against Racket Pie on a corpus of about 930 programs, worked
examples, malformed syntax and generated programs, comparing stdout, stderr and
exit status. Every program prints the same, except the five that give `cong`
a function whose type depends on its argument.

There the port departs from upstream on purpose. The book types the function
of `cong` as `(→ X Y)`, but upstream accepts any `Π` and types `(cong p f)`
with the codomain at the left end of `p` only, so `(f to)` is read back at the
wrong type. Upstream then either stops inside `read-back` with a `match*`
error of `normalize.rkt`, or prints a wrong type, such as `(= (Vec Atom 1) …)`
for a vector of length 4. The port rejects such a function with "Expected a
function whose type does not depend on its argument", and the programs
`test/pie/fix-*.pie` record it.

Pie is distributed under the GNU AGPL-3.0, and so are the files of the port,
`src/aux.pie*.scm` and `src/test/pie*`, which are derived from it; the license
text is in `COPYING.AGPL`. The rest of aux is under the MIT license.

### `(aux lua)`

`(aux lua)` binds the whole C API of Lua 5.5 (`lua.h`, `lauxlib.h`, `lualib.h`)
as curried procedures, with the compatibility macros that `luaconf.h` always
defines: `lua-strlen` and `lua-objlen` (both `lua-rawlen`), `lua-equal` and
`lua-lessthan` (`lua-compare` with `LUA_OPEQ` and `LUA_OPLT`). `lua_foo`,
`luaL_foo` and `luaopen_foo` become `lua-foo`, `luaL-foo` and `luaopen-foo`,
and constants keep their C spelling (`LUA_TNIL`, `LUA_MULTRET`,
`LUA_REGISTRYINDEX`, ...).

**Currying.** A binding takes every argument except the `lua_State` first, and
returns a procedure of one argument, the state:

```scheme
((lua-pushinteger 42) L)   ((lua-getglobal "x") L)   ((lua-gettop) L)
((lua-xmove to n) from)    ((lua-resume from nargs) co)   ; C's first lua_State goes last
```

The state is a `luaR` record. It can be a main state from `(lua-make-empty)`,
`(luaL-newstate)` (the same state) or `(lua-newstate seed)`, a thread from
`lua-newthread` or `lua-tothread`, or the
view a Scheme callback receives. Because every binding is a procedure of the
state, bindings compose as reader-monad actions (`(aux category reader)`, then
`(aux category monad reader)`). Three bindings answer several values, like C's
out-parameters: `lua-tonumberx` and `lua-tointegerx` (the value and `isnum`)
and `lua-resume` (the status and `nresults`). A reader bind keeps only the
first value, so inside `do/monad` wrap them, e.g.
`(λ (L) (call-with-values (λ () ((lua-tointegerx -1) L)) list))`. A few
entries are not curried:

- the constants, and `luaopen-*`, which are C function pointers to hand to
  `lua-pushcfunction` or `luaL-requiref` (the only C function pointers these
  accept, with those `lua-tocfunction` reads from a light C function: see
  **Errors**);
- `lua-upvalueindex`, the `lua-debug-*` accessors and `lua-type->symbol`.
  `(lua-upvalueindex i)` names user upvalue `i` of the running Scheme
  closure, from 1 to 255. It is C's `lua_upvalueindex(i + 1)`, because the
  closure keeps its Scheme handle in its first upvalue. Up to 255 it is an
  acceptable index even beyond the closure's upvalues, as in C:
  `lua-type` answers `LUA_TNONE` and `lua-isnone` `#t`. For the same reason a
  Scheme procedure takes at most 254 upvalues (`lua-pushcclosure`,
  `luaL-setfuncs`), one fewer than a C function's 255; 255 is refused with
  `(exn lua api)`, code `AUX_ERANGE`;
- `lua-format`, which takes the directives of `lua_pushfstring` and returns the
  text itself, a string (a bytevector when it is not valid UTF-8), without a
  state: `(lua-format "%d items" 3)`. `((lua-pushfstring "%d items" 3) L)`
  pushes the same text;
- the predicates and accessors on Scheme values: `lua-ref?` (a registry
  reference made by `lua-toref`), `lua-live-handles`, the condition accessors
  (`lua-condition?`, `lua-condition-status`, `lua-condition-value`), and
  `luaR?`, `luaR-kind` and `luaDebug?`;
- `lua-make-empty`, `lua-newstate`, `luaL-newstate` and `lua-close`.

`(lua-make-empty)` and `(luaL-newstate)` start with warnings on and written to
stderr, as C's `luaL_newstate` does. `(lua-newstate seed)` starts with them
discarded, because C's `lua_newstate` installs no warning function;
`((lua-setwarnf 'stderr) L)` or `((lua-setwarnf 'collect) L)` turns them on.
`lua-setwarnf` installs a warning function, as `lua_setwarnf` does. `'off` (or
`#f`) is C's NULL: nothing reaches it, so a `"@off"` or a piece sent then is
dropped. `'stderr` and `'collect` start with warnings on and at a new message,
whatever the function they replace was told. When `'collect` is replaced in the
middle of a message, the pieces it has are kept as one warning.
In `'collect` mode, `((lua-warnings) L)` returns the completed warnings
collected so far, oldest first, as a list of strings, and clears them.

Before the port, `luaL-newstate` returned the bare `lua_State` pointer. It now
returns a main `luaR`, because no binding accepts a bare pointer.

**Exports.** `(aux lua)` exports a fixed list: the bindings, the constants, the
condition accessors (`lua-condition?`, `lua-condition-status`,
`lua-condition-value`), `lua-live-handles`, and three read-only record
procedures: `luaR?`, `luaR-kind` and `luaDebug?`. It exports no record
constructor or mutator and no raw C stub. The implementation module,
`(aux lua internal)`, exports everything, but it exists only for the test
suite. Its raw stubs skip the checks, and the module can be imported only after
`(aux lua)`.

**Values.**

- Strings go in as Scheme strings or bytevectors and may contain NUL bytes.
- Strings come back as strings, or as bytevectors when the bytes are not valid
  UTF-8. `lua-tobytevector` always returns bytes. The message of a condition
  is always a string: there, each byte of invalid UTF-8 that is 128 or above
  becomes U+FFFD, and the condition's `value` keeps the bytes.
- `lua-tolstring` and `lua-tostring`, like `luaL-checklstring`,
  `luaL-checkstring` and `luaL-optlstring`, read a number as the text Lua writes
  for it but convert only a copy: the slot keeps its number, and `lua-type` of
  that slot still answers `LUA_TNUMBER`. In C, `lua_tolstring` changes the
  value in the stack to a string, which is why the manual warns against it on
  the key of a `lua_next` traversal. Here that use is safe. C code ported to
  the module that relies on the slot becoming a string must push the string
  itself (`((luaL-tolstring idx) L)` pushes the text, as in C).
- `lua_Integer` is an exact integer in the int64 range, and `lua_Number` a flonum.
- Tables, functions, userdata and threads can be kept from Scheme as registry
  references (`lua-toref`, `lua-pushref`).
- Any Scheme object can be given to Lua as a userdata (`lua-pushobject`,
  `lua-toobject`). Lua holds only an integer handle, never a Scheme pointer.

**Errors.** The module follows C:

- Where C raises, the binding raises an `(exn lua)` condition. Its `status` is
  `'errrun`, `'errsyntax`, `'errmem`, `'errerr` or `'errfile`, and its `value` is
  the Lua error value. For `luaL-error`, `luaL-argerror`, `luaL-typeerror` and
  the checks built on them (`luaL-checkinteger`, `luaL-checktype`,
  `luaL-checkstack`, `luaL-argcheck`, ...) that value is the string C raises,
  computed as lauxlib does: prefixed by `luaL_where`, which is `""` at top level
  and the calling Lua function's position inside a callback, and for an
  argument error naming the function (`bad argument #1 to 'f' (...)`,
  `calling 'm' on bad self`, `bad extra argument`). Uncaught inside a
  callback, Lua receives the same string. The condition's message is the text
  without position or name (`bad argument #1 (...)`). On a thread where no
  operation can run (a suspended coroutine) the `value` is that text too.
- Where C returns a status (`lua-pcall`, `luaL-loadstring`, `lua-resume`, ...),
  the binding returns the status and leaves the error value on the stack.
  `(lua-check m)` turns such an action into a raising one. One deliberate
  deviation: `luaL-dostring` and `luaL-dofile` return the failing status
  (`LUA_ERRSYNTAX`, `LUA_ERRRUN`, `LUA_ERRMEM`, `LUA_ERRFILE`) where C's macros
  return 1, so that `lua-check` can raise it (1 would read as `LUA_YIELD`). On a coroutine
  suspended by a hook, or dead by an error raised in Lua code, the top of the
  stack belongs to the suspended Lua function, so `((lua-error) co)` and
  `lua-check` refuse to pop it, as `lua-pop` does. The exception is the error
  value that `lua-resume` has just pushed: `((lua-check (lua-resume from n)) co)`
  raises it.
- A call C would not survive is refused with `(exn lua api)` before Lua runs:
  a bad stack index, a closed state, an operation on a suspended or dead thread,
  resuming the main thread, or a NUL in a C string. liblua is built without
  API checks, so such a call would otherwise corrupt memory. A thread suspended
  by a hook's `lua-yield`, or dead by an error raised in a Lua function, keeps
  that function's registers on its stack (a pending call's arguments, a
  to-be-closed variable). `((lua-resume from n) co)` and `((lua-xmove to n) co)`
  take only what lies above the height it had then: the values pushed since,
  and the error value `lua-resume` left on a thread it saw die. A thread that
  Lua's `coroutine.resume` left dead has already lost its error value to Lua,
  so only the values pushed since are taken there.
  Push-only calls (`lua-pushfstring`, `luaL-where`, `luaL-traceback`,
  `lua-getglobal`, ...) work on a suspended or dead thread, as in C: they run
  on the main thread and move their result over.
- A few calls that C allows but that would corrupt memory are refused too:
  - the load mode `"B"` (Lua 5.5's fixed buffer), because the loaded function
    would keep pointers into a buffer that does not outlive the call;
  - `lua-setlocal` and `lua-setupvalue` on a variable the VM trusts (an
    upvalue that does not exist is not refused: `lua-setupvalue` answers `#f`
    and pops nothing, as C returns NULL, whatever the function). These are
    a for loop's control variable and a named vararg parameter
    (`function f(...t)`), which are read-only in Lua 5.5. A main chunk is a
    vararg function too but has no such parameter, so its locals are assigned
    as in C. For an upvalue, the
    `<const>` and `<close>` variables are refused as well, and so is
    `lua-upvaluejoin` onto such an upvalue, which would make it writable
    through another closure;
  - `lua-getlocal` on the C frame of a protected binding, which holds C state;
  - a non-table in `registry[LUA_RIDX_GLOBALS]`, the table of globals that
    `lua_load` and `lua_setglobal` read, with an API check, and that becomes
    a loaded chunk's `_ENV` without one. `lua-rawseti`, `lua-seti`, `lua-rawset`
    and `lua-settable` on `LUA_REGISTRYINDEX` refuse to store any other value
    there with `(exn lua api)`, code `AUX_EGLOBALS`
    (`registry[LUA_RIDX_GLOBALS] must be a table`). Lua code
    (`debug.getregistry`) can still replace it. While it has, the loaders
    (`lua-load`, `luaL-loadstring`, `luaL-loadbuffer`, `luaL-loadbufferx`,
    `luaL-loadfilex`, `luaL-dostring`, `luaL-dofile`, ...), `luaL-requiref`
    with a true `glb`, `luaL-openlibs` and `luaL-openselectedlibs` (when they
    would set a global) are refused the same way, before Lua runs. When the
    slot is replaced while such a call runs, by a `luaL-requiref` open
    function or a finalizer, the call raises the Lua error
    `aux.lua: registry[LUA_RIDX_GLOBALS] is not a table` before it reaches the
    slot. A `lua-load` reader that replaces it stops the load with the
    `AUX_EGLOBALS` refusal. `lua-getglobal` and `lua-setglobal` raise the Lua error
    of indexing the non-table, as a release liblua does. Everything works again
    once the slot holds a table;
  - a reference that `luaL-ref` or `lua-toref` would store in the wrong slot.
    lauxlib keeps a free list in `t[1]` and reads its head, and the next new
    reference `#t + 1`, as a C `int`. A head (or a border) past `INT_MAX`
    would be truncated into another key, possibly a negative one. A head
    whose link is not an integer, or one of the registry's reserved slots
    (`LUA_RIDX_GLOBALS`, `LUA_RIDX_MAINTHREAD`), would give out a slot that is
    in use. Lua code or the raw setters can plant such a list. The binding
    raises an `(exn lua)` error before storing anything (`aux.lua: the
    reference free list of this table is corrupted`, `aux.lua: no reference is
    left in this table`, or `... would be a reserved slot`), and references
    work again once `t[1]` is repaired;
  - a new Scheme handle (`lua-pushobject`, a Scheme procedure given to Lua, the
    error value of a Scheme callback, `lua-yieldk`, `lua-sethook`) once the
    registry entry of the handle metatable is no longer a table. Lua code
    (`debug.getregistry`) or `lua-rawset` on `LUA_REGISTRYINDEX` can replace it,
    and C's `lua_setmetatable` with a non-table is undefined behaviour. The
    binding raises an `(exn lua)` condition instead, and handles work again
    once the entry is restored. Meanwhile the existing handles are not
    recognized: `lua-toobject` answers `#f`, and a Scheme procedure refuses to
    run;
  - a C function pointer that the module cannot vouch for, given to Lua as a
    `lua_CFunction` by `lua-pushcfunction`, `lua-pushcclosure`,
    `lua-register`, `luaL-setfuncs`, `luaL-newlib`, `luaL-newlibtable` or
    `luaL-requiref`. Accepted are the `luaopen-*` openers and every function
    that `lua-tocfunction` has read from a light C function (one with no
    upvalue: Lua itself lets any code call it with any arguments), in any
    state; given upvalues, such a function ignores them. Lua would call any other
    pointer as code: the address of a table or userdata (`lua-topointer`,
    `lua-touserdata`, `lua-newuserdatauv`), or the function of a C closure that
    reads its upvalues (the iterators of `io.lines` or `string.gmatch`, which
    `lua-tocfunction` also returns) pushed without them. It is refused with
    `(exn lua api)`. A light C function of a C module that a state loaded
    (`require`, `package.loadlib`) stays accepted after that state is closed
    and the module unloaded: do not push it then, as in C;
  - the metatable of a Scheme handle (a `lua-pushobject` object, the error
    value of a Scheme callback, upvalue 1 of a Scheme procedure's closure,
    which `lua-getupvalue` returns) or of the registry, set by
    `lua-setmetatable` or `luaL-setmetatable`, and the handle metatable
    (which `lua-getmetatable` returns, ignoring `__metatable`) given to any
    other value or stored in the registry under a string key, where
    `luaL-setmetatable` and `luaL-checkudata` would find it. A handle's
    `__gc` releases its Scheme object, so another metatable would keep the
    object until `lua-close`, and `FILE*`'s would make `io.close` take the
    handle's bytes for a C stream. A `__mode` on the registry's metatable
    would let the collector clear the table of globals, which Lua's own
    `load` and `lua_getglobal` then read as garbage. Refused with
    `(exn lua api)`, code `AUX_EMETATABLE`. `luaL-setmetatable` refuses the
    same way when it only finds the handle metatable in `registry[tname]`
    (put there by Lua code with the debug library). Removing the registry's
    metatable with `nil` is allowed, and so is `luaL-setmetatable` on the
    registry with a `tname` that has no entry, which sets `nil` as in C.
    Lua code cannot do any of this without the debug library;
  - forging some of the entries that Lua's own libraries trust (a
    best-effort guard: see **Trust model** below). liolib takes a full
    userdata whose metatable is `registry["FILE*"]` for its C stream and calls
    the stream's close function, lauxlib takes one whose metatable is
    `registry["_UBOX*"]` for a buffer box and reallocates its block, and
    `package.loadlib` and `require` take a userdata at
    `registry._CLIBS[path]` for a `dlopen` handle. None of them checks, so a
    `lua-newuserdatauv` block with such a metatable, or a userdata in
    `_CLIBS`, would make plain Lua code (`io.close`, `f:write`,
    `package.loadlib`) crash the process. `lua-setmetatable` and
    `luaL-setmetatable` do not give those two metatables to a full or light
    userdata that does not have them already, and give no metatable to
    `_CLIBS` (code `AUX_EMETATABLE`). A file keeps its own metatable, and a
    table may take either one. `lua-setfield`, `lua-setglobal`,
    `lua-settable` and `lua-rawset` do not change `registry["FILE*"]`,
    `registry["_UBOX*"]` or `registry._CLIBS`, and do not store a userdata in
    `_CLIBS` under a string key (code `AUX_ELIBRARY`). Removing one of the
    three entries is allowed, and `luaL-newmetatable` then makes a fresh
    `"FILE*"` or `"_UBOX*"` table that no userdata has. The standard files
    keep the old table, which liolib no longer recognizes;
  - the `__tostring` of the handle metatable, which `lua-getmetatable` and
    `debug.getmetatable` reach, called on anything but a handle. It raises
    `bad argument #1 to '?' (aux.scheme expected, got number)`, as
    `luaL_checkudata` would;
  - Lua's `coroutine.close()` of the running coroutine while a binding runs
    Lua on that thread (`lua-pcall`, `luaL-dostring`, a metamethod run by a
    binding, `lua-gc`'s finalizers, typically from a Scheme callback). In Lua 5.5
    it jumps to the thread's outermost protected call, across the binding and
    the callback. It raises a Lua error instead. The `coroutine` library of
    `luaL-openlibs`, `luaL-openselectedlibs` and `luaopen-coroutine` is lcorolib's
    with that one check added. A coroutine that no binding is running (resumed
    by `lua-resume` or by Lua) still closes itself as in C. The main thread is
    recognized by its address, not by the registry slot lcorolib reads, which
    Lua code can overwrite: closing it always raises `cannot close main thread`.
- Arguments are checked before Lua runs: for example `lua-pushinteger`
  refuses anything but an exact integer in the int64 range, and
  `lua-pushnumber` anything but a real, with `(exn lua api)` as above.
  A C `int` used as a boolean takes `#t` or `#f` only: `b` of
  `lua-pushboolean`, `strip` of `lua-dump`, `tocont` of `lua-warning`, `glb`
  of `luaL-requiref`, and `cond` of `luaL-argcheck` and `luaL-argexpected`.
  With Scheme truthiness `0` would count as true, the opposite of C, so a
  ported `((lua-dump 0) L)` or `((luaL-requiref name f 0) L)` would silently
  do the reverse. Any other value, `0` and `1` included, is refused with
  `(exn lua api)` before Lua runs.
  `lua-getinfo` takes its `what` as C does, of any length, with letters
  repeated or none at all (`""` fills nothing). It returns the `luaDebug`
  rather than C's `int`, so a letter C does not know (for which C returns 0)
  is refused with `(exn lua api)` instead.
  `lua-pcall` and `lua-call` take at most 250 results, Lua 5.5's limit, which
  C only checks in a liblua built with API checks. The
  `%d` and `%c` directives of `lua-pushfstring`, `lua-format` and `luaL-error`
  take a C `int`, and `%I` a `lua_Integer`, in the same way. Their format and
  their `%s` arguments are C strings: a NUL in either is refused (C would stop
  reading there), and `%s` takes a string, a bytevector or `#f` (NULL, printed
  `(null)`), nothing else.
- Binary chunks are the one input the module cannot check. Lua does not verify
  bytecode, so a corrupted or malicious binary chunk crashes the process (Lua
  manual, section 3.3.2). `lua-load`, `luaL-loadbufferx` and `luaL-loadfilex`
  accept binary chunks with the mode `#f`, `"b"` or `"bt"`, and
  `luaL-loadbuffer`, `luaL-loadstring`, `luaL-dostring`, `luaL-loadfile` and
  `luaL-dofile` always do, as in C. Load binary chunks only when you made them
  with `lua-dump` or trust their source, and use the mode `"t"` for any other
  input. Lua's own `load`, `loadfile` and `dofile` have the same limit.
- The debug library is another limit: do not give it to untrusted Lua code.
  A Lua `__gc` finalizer runs inside any allocating API call, and from there
  (or from a hook) `debug.setlocal` can rewrite the slots of the C function
  below it, its `(C temporary)` values, which that C function trusts. The
  module's own C code checks its slots again after each allocation and raises
  `aux.lua: a slot of a protected operation was rewritten` instead of
  misusing one, and `lua-getlocal` and `lua-setlocal` refuse those slots. Lua's
  own C code does not check: the parser (`load`, `lua-load`,
  `luaL-loadstring`, `luaL-dostring`, ...) and `string.format` then crash the
  process, and other C functions of Lua's libraries may too. They do the same
  in a stock Lua 5.5 interpreter.
- **Trust model.** For the registry entries that Lua's standard libraries
  own, the bound API is exactly as trusting as the C API. The libraries keep
  private state there and never check it: `registry._IO_input` and
  `_IO_output`, the `"FILE*"` metatable and those of other library userdata,
  `"_UBOX*"`, `_HOOKKEY`, `_CLIBS`, `_LOADED`, `_PRELOAD`,
  `registry[LUA_RIDX_GLOBALS]` and `registry[LUA_RIDX_MAINTHREAD]`, among
  others. As in C, the caller must not overwrite them with values of another
  shape: a program that does may crash the process, as the same program in C
  would, and that is misuse of the API, not a defect of the bindings. The
  guards described above (`registry[LUA_RIDX_GLOBALS]`, `"FILE*"`,
  `"_UBOX*"`, `_CLIBS`) are best-effort. They stop some common forgeries made
  through the bound API, cover no other entry, and are not a guarantee.
- The registry has the same limit once Lua code can reach it. The bindings
  check some of their own writes (see the refusals above), but they cannot check what
  Lua code does with the registry or `registry._CLIBS` when it gets them.
  That happens through `debug.getregistry`, a global, field or upvalue that
  the bound API stored, or the registry made the table of globals or
  `registry._LOADED`, which `require` and `luaL-requiref` write to. The
  bindings also cannot check a write that goes through a metamethod, such as
  a table whose `__newindex` is the registry. From there plain Lua code can
  store a userdata's metatable as `registry["FILE*"]` or put `io.stdout` into
  `_CLIBS`, and `io.close` or `package.loadlib` then crashes the process.

Internally, every Lua call that may raise runs under a `lua_pcall` made by the
same C stub, so a Lua `longjmp` never crosses a CHICKEN frame. Operands are
copied before any Lua code can call back into Scheme.

That protected call goes through a C function of the module. It is not a call
the program made, so call and return hooks never see it enter or leave. In C,
`lua_pushcclosure` or `lua_sethook` fires no hook either, and Lua code that a
binding runs, such as a metamethod, is hooked as usual. Hiding the call takes a
`lua_sethook`, so a count hook installed together with a call or return hook
restarts its count at each such binding. A line or count hook on its own is
left alone.

**Callbacks.** A Scheme procedure `(λ (C) n)` becomes a Lua function through
`lua-pushcfunction`, `lua-pushcclosure`, `lua-register` or `luaL-setfuncs`.
`((lua-toprocedure idx) L)` gives the Scheme procedure of such a function
back, and answers `#f` for any other value.
It receives a view `C` of the calling thread, works on it with the same
bindings, and answers how many results it pushed. It can also end in other
ways:

- `((lua-error) C)`, `((luaL-error fmt ...) C)`, `((luaL-argerror ...) C)` and
  the `luaL-check*` family raise in Lua with C's exact messages.
- An uncaught Scheme condition reaches Lua as an error value. It comes back to
  Scheme as the same object (`eq?`). In Lua its `tostring` is the condition's
  message and written arguments. The arguments are written with CHICKEN's
  print-length limit, as `print-error-message` writes them, so a circular or
  huge argument ends in `...` after about 4096 characters. A string message
  is kept whole.
- `((lua-yield n) C)` and `((lua-yieldk n k) C)` yield the running coroutine.
  The suspended coroutine looks as it does in C. Its stack holds only what the
  function left there, because the continuation `k` is kept in a private table
  keyed by the thread. The resumer may pop, clear or move those values before
  it resumes. `k` is released once it has run, or when the thread is collected.
- A continuation that escapes the callback is stopped at the callback
  boundary, and resumed once Lua has unwound.

A callback returns to Lua once, in the srfi-18 thread that entered it. A
continuation captured inside it can be resumed while it runs, but only by that
thread. Resuming it after the callback has returned, or from another thread
while it runs, raises an error in the resuming thread. A thread created inside
the callback shares its dynamic extent, so such a resume there runs the rest of
the callback in that thread. When that run ends, with a value or a condition,
it raises the same error in that thread instead of returning to Lua. An escape
from it leaves as an ordinary Scheme escape of that thread. The callback's own
thread still gets its result.

A Lua error cannot unwind through a callback in one jump, as it does through a
C function: that `longjmp` would cross CHICKEN frames. A binding that raises
inside a callback (`lua-call`, `lua-gettable`, `luaL-dostring` under
`lua-check`, ...) catches the error at its own protected call, the error crosses
the callback as an `(exn lua)` condition, and the trampoline raises it again in
Lua once the callback has returned. The error value (the same object, `eq?`)
and the status (`LUA_ERRMEM`, `LUA_ERRERR`) are kept, but an enclosing message
handler, such as the one of `xpcall(f, debug.traceback)` or of a `lua-pcall`
with `msgh`, runs at the callback's frame, not where the error was raised. The
frames between the two are gone by then, so `debug.traceback` and
`debug.getinfo` in that handler do not see them. A callback that needs them
calls with a handler of its own, `((lua-pcall nargs nres msgh) C)`, which runs
at the error point as in C, and passes its result on.

Hooks (`lua-sethook`) and chunk readers (`lua-load`) are Scheme procedures too.
While a hook runs, the slots its thread held when it was called are the
registers of the hooked function: no binding can lower, move or overwrite them,
whichever `luaR` of that thread it is given. The same holds while any callback
runs, for every *normal* thread: one that resumed the running coroutine
(`coroutine.resume`, `coroutine.wrap`, `coroutine.close`, or a binding) and
waits for it in a C function. That frame is C state. For example, slot 1 of
`coroutine.resume` holds the running coroutine, and that may be its only
reference, so dropping it and then collecting would free the coroutine while it
runs. The first binding that uses such a thread in a callback, through the main
`luaR`, a thread `luaR` or the still-live view of an outer callback, records the
height of that frame. Below that height, lowering, moving or overwriting is
refused with `(exn lua api)`, code `AUX_EINDEX`. Values pushed onto the thread
can still be popped down to that height. The frame of a Scheme procedure has no
such floor, because its slots belong to that callback: for example the frame of
an outer callback, running in a coroutine, that called `lua-resume` itself.
`lua-resume` runs `lua_resume` in a frame of its own on the main thread, which
holds the resumed thread, so that frame does have a floor. A reader sees its thread in a frame
of its own, as a C function calling `lua_load` would. The parser works on a
private thread that no binding can reach. Lua code cannot find or release that
thread either, even through the `debug` library, and the loading thread cannot
be resumed or reset until the load returns. Each call from Lua into Scheme costs one CHICKEN callback, which is a minor
garbage collection. While a state holds no Scheme procedure, its bindings use
faster stubs that cannot call back. A callback is refused with Lua's own
`C stack overflow` error when too little C stack is left for it, so a runaway
recursion between Lua and Scheme ends with an error instead of hanging.

**Threads.** One srfi-18 thread at a time runs Lua that can call Scheme back.
A callback leaves Lua's C frames on the C stack, and a CHICKEN callback always
returns to the innermost C frame. If a callback switched threads (with
`thread-yield!`, a blocking call or the end of its timeslice) and a second
thread then entered a callback of its own, each callback's result would reach
the other thread's Lua call. To prevent that, a binding that can call back
takes a lock that belongs to one thread. Another thread that needs Lua with
callbacks waits, yielding, until the owner's outermost binding returns. Threads
that use separate states therefore take turns one binding at a time, and no
setting is needed. Bindings on a state that holds no Scheme procedure never
wait, except those that give it one (`lua-pushcfunction`, `lua-sethook`,
`lua-load` with a reader, ...). A binding waits before it looks at its state.
The owner may close that state while the other thread waits, return from the
callback whose view `C` it was handed, or let the coroutine of a thread `luaR`
be collected. The waiting binding then raises `(exn lua api)` for a closed or
invalidated state, and never touches the freed one. If the owner's callback waits for the waiting thread, the two threads
would wait forever. When the callback joins that thread (`thread-join!`) or
waits for a mutex that thread holds, the waiting thread raises `(exn lua api)`
instead. The module cannot detect a wait on anything else, such as a
condition variable or a mailbox, and that case deadlocks.

**Lifetimes.**

- `(lua-close L)` closes a state at once. Otherwise its finalizer closes it once
  the main `luaR` and every thread `luaR` of the state are unreachable. When the
  state is busy at that moment, or holds Scheme procedures (its `__gc`
  callbacks must not run inside CHICKEN's finalizer loop), the close waits for
  the next binding call.
- A callback may capture the `luaR` of its own state. The state is still
  collected, because Scheme objects given to Lua live in a table of the state,
  not in a global one.
- A thread `luaR` keeps its thread alive, through an anchor of its own. Lua code
  can remove that anchor, for example through `debug.getregistry()`. The next
  binding given that `luaR` then raises `(exn lua api)` instead of using a
  collected thread.

**Omitted entries.** The module header lists them, each with its reason:

- `lua_atpanic`: a panic handler cannot return safely;
- `lua_callk` and `lua_pcallk`: a continuation would unwind through a CHICKEN
  callback;
- `lua_getallocf` and `lua_setallocf`: the module owns the allocator (use
  `lua-setmemorylimit`);
- `lua_toclose` and `lua_closeslot`;
- `lua_getextraspace`, `lua_ident`, `lua_pushexternalstring` and
  `lua_pushvfstring`;
- `luaL_fileresult`, `luaL_execresult`, `luaL_alloc` and `luaL_intop`;
- the `luaL_Buffer` family: build the string in Scheme and push it once;
- the `LUA_COMPAT_APIINTCASTS` macros, which Lua 5.5 does not compile;
- `luaconf.h`'s configuration hooks (`lua_str2number`, `lua_strx2number`,
  `lua_integer2str`, `lua_number2strx`, `lua_pointer2str`,
  `lua_getlocaledecpoint`): they configure how Lua's own C code converts
  numbers, and are not API.

Example:

```scheme
(import (aux base) (aux lua) (aux category reader) (aux category monad reader))

(define L (lua-make-empty))
((luaL-openlibs) L)

((lua-check (luaL-dostring "function add(a, b) return a + b end")) L)
((lua-getglobal "add") L)
((lua-pushinteger 40) L)
((lua-pushinteger 2) L)
((lua-call 2 1) L)
((lua-tointeger -1) L)
;; => 42
((lua-pop 1) L)

(define (global-int name)
  (do/monad (lua-getglobal name) (← v (lua-tointeger -1)) (lua-pop 1) ,v))
((lua-check (luaL-dostring "x, y = 40, 2")) L)
((do/monad (← x (global-int "x")) (← y (global-int "y")) ,(+ x y)) L)
;; => 42

((lua-register "greet"
   (λ (C) ((lua-pushstring (string-append "hello, " ((luaL-checkstring 1) C))) C) 1))
 L)
((lua-check (luaL-dostring "return greet('lua')")) L)
((lua-tostring -1) L)
;; => "hello, lua"
((lua-settop 0) L)

(condition-case ((lua-check (luaL-dostring "error('boom', 0)")) L)
  (e (exn lua) (list (lua-condition-status e) (lua-condition-value e))))
;; => (errrun "boom")

(lua-close L)
```

The module needs the Lua 5.5 headers and `liblua`, so its tests are not part of
`make test`. Run them with `make test-lua`. For a Lua installed under
`~/.local`, set `CSC_OPTIONS="-I$HOME/.local/include -L -L$HOME/.local/lib"`
when building.

`aux.lua` is a shared object, so it needs a position-independent Lua: either a
shared `liblua`, or a static `liblua.a` compiled with `-fPIC`. The stock
`make linux` of Lua 5.5 builds without `-fPIC`, and linking the extension then
fails with `relocation R_X86_64_PC32 ... recompile with -fPIC`. Build Lua like
this:

```sh
make CC=clang MYCFLAGS=-fPIC linux
make install INSTALL_TOP=$HOME/.local
```
