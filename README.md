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
that returns the hidden hash table.

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

- `continuation->λ` — a raw continuation object as a variadic procedure
- `letcc/raw` — binds both the raw continuation and its procedure wrapper
- `apply/cc` — applies a procedure passing the current continuation last
- `escapecc` — escape with an `(else β)` fallback or an `(=> f)` receiver
- `trycc/named` — clauses that each name their own skip continuation
- `set/cc!`, `cond/cc`
- `current-continuation/cont`, `current-continuation/λ`
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
  `stream:append`, `stream:merge`, `stream:convolution`
- `stream:tails`, `stream:prefixes`, `stream:scan`, `stream:tableau`
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

((list○take 6) (stream:§ (stream:repeat 'a) (list->stream '(1 2 3))))
;; => (a 1 a 2 a 3)

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
- `game-tree`, `game-tree->sexp`, `players-rotation`, `neighbors`,
  `board-attack`, `add-new-dice`
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
chain.

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
