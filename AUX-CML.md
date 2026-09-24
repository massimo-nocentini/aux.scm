# `(aux cml)`: Concurrent ML for CHICKEN Scheme

This document describes `(aux cml)`, a port of the Concurrent ML library of SML/NJ
(`smlnj/libraries/cml`) to CHICKEN Scheme. It covers what was studied in the ML sources, the
complete public API, the algorithms as they are implemented today, every deliberate departure
from ML, the nine review rounds the port went through, how to run the test suites, and what is
still open.

The code in `src/aux.cml.scm` is the reference. Every behavioural claim below was checked
against it, some of them with small experiments run under `csi`. It describes
`src/aux.cml.scm` as of the commit on branch `aux-cml` that adds this document.

**ML source baseline.** Everything said here about SML/NJ CML (its sources, representations,
algorithms, bugs and doc/code mismatches, and every "ML does ..." comparison) refers to
`libraries/cml` of [`git@github.com:smlnj/smlnj.git`](https://github.com/smlnj/smlnj) up to
commit `92a912a5aea716ecb7454d07a803f3b172f6dc64` ("switch to V3 literals implementation",
2026-09-22). Later upstream changes are not reflected.

Contents

1. [Overview](#1-overview)
2. [Findings from studying SML/NJ CML](#2-findings-from-studying-smlnj-cml)
3. [Definitions: the public API](#3-definitions-the-public-api)
4. [Algorithms](#4-algorithms)
5. [Deviations from ML](#5-deviations-from-ml)
6. [Review log](#6-review-log)
7. [Testing](#7-testing)
8. [Known limitations and open items](#8-known-limitations-and-open-items)

---

## 1. Overview

### 1.1 What CML is

Concurrent ML (John Reppy) is a message-passing concurrency library built on **first-class
synchronous events**. A *thread* is a lightweight process. An *event* is a value that describes a
synchronous operation, such as receiving on a channel or waiting for a timeout, without
performing it. Events are composed with combinators:

- `choose` builds a choice among events;
- `wrap` post-processes the value of an event once it is chosen;
- `guard` delays the construction of an event until synchronization time;
- `withNack` does the same, but also hands the constructor a *negative acknowledgement* event
  that becomes enabled if this branch is **not** chosen.

`sync` performs an event: it commits to exactly one of the base events in the choice, and the
effects of the others do not happen. This makes it possible to write protocols (servers with
abortable requests, selective communication, timeouts) as ordinary first-class values.

### 1.2 What was ported

`(aux cml)` is one module in one file, `src/aux.cml.scm` (3578 lines). The module exports
everything it defines (`(module (aux cml) * ...)`). The file has three parts:

| Part | ML origin | Lines (approx.) |
|---|---|---|
| Core: scheduler, events, threads, channels, timeouts, IO manager, process manager, SyncVar, Mailbox, Barrier, Result, CleanUp, RunCML, version/debug | `src/core-cml/*`, `src/glue/*`, `src/util/result.sml`, `src/Unix/proc-manager.sml`, `src/Unix/unix-glue.sml` | 1–2205 |
| IO / OS layer: port events, channel ports, `system-evt`, `cml/execute`, TCP events | `src/Unix/os-process.sml`, `src/Unix/new-unix.sml`, `src/IO/new-text-io-fn.sml`, `src/IO/chan-io-fn.sml`, `src/Sockets/cml-socket.sml` | 2207–3243 |
| cml-lib: Multicast, SimpleRPC, TraceCML | `cml-lib/multicast.sml`, `cml-lib/simple-rpc.sml`, `cml-lib/trace-cml.sml` | 3245–3578 |

Each part starts with a `;;` header comment that holds its design notes and its deviations from
ML. The test suites are `src/test/cml.scm`, `src/test/cml-lib.scm` and `src/test/cml-io.scm`
(211 test cases in 16 suites, see [section 7](#7-testing)). The module is registered in
`src/aux.egg` as `(extension aux.cml)` and the suites run with `make test-cml` in `src/`.

The port targets CHICKEN **6** (it was developed against 6.0.1pre1, binary version 12). It
imports `(chicken tcp)`, `(chicken file posix)`, `(chicken process)`, `(chicken bytevector)`,
`(chicken errno)`, srfi-1 and the repository's own `(aux base)` and `(aux continuation)`. It
does **not** use srfi-18: threads are first-class continuations driven by the module's own
scheduler, as CML does with `callcc`.

The port was developed on branch `aux-cml` in two commits:

- `3227791` "Add (aux cml): a port of SML/NJ Concurrent ML": the port as it stood after the first
  eight review rounds (6895 lines added).
- `83434a8` "Fix twelve defects in (aux cml) found by the ninth review round": the fixes of
  the last round.
- The commit that adds this document also fixes the mismatches its writing uncovered between
  the code and its comments: the SimpleRPC and outside-`run-cml` wording of the header, the
  undocumented `make-barrier` argument order, the dead `signal-pending` state (removed), and
  `run-cml` silently accepting a bad `quantum` (now an error).

### 1.3 Scope and what was not ported

Ported: the whole public surface of core CML (`CML`, `SyncVar`, `Mailbox`, `Barrier`, `RunCML`,
`CleanUp`, the `Result` utility), the event-valued OS layer that has a CHICKEN counterpart
(descriptor readiness, child processes, text port events, channel-backed ports, TCP accept and
connect), and the three cml-lib libraries that are built on the core (Multicast, SimpleRPC,
TraceCML).

Not ported, and why:

| Not ported | Reason |
|---|---|
| Signal-driven preemption (SIGALRM every 20 ms) | CHICKEN cannot safely capture continuations inside a signal handler. Preemption happens at CML operations instead (a tick per operation, see [4.2](#42-the-scheduler)). |
| `OldCML` (the CML 0.9.8 compatibility shim) | It is not built upstream any more. |
| SML Basis I/O plumbing: `PRIM_IO`/`STREAM_IO` functors, functional input streams, stream positions, buffer modes, `BinIO` | CML events work directly on CHICKEN ports; there is no functional stream layer to reproduce. |
| `exportFn` / heap export (`glue/export-fn-fn.sml`) | No CHICKEN equivalent. As a consequence the cleaner times `at-exit` and `at-init-fn` exist but are never triggered. |
| Win32 glue | Not a target. |
| SMLNJ-Util (thread-safe atoms, maps, tables) | Scheme symbols already are atoms, and there is no preemption inside Scheme code. |
| SMLNJ-INet, the socket library's phantom types, UDP and Unix-domain sockets | Only TCP is available, through `(chicken tcp)`. |

The only test upstream (`src/tests/test.sml`) is a 24-line `Unix.execute "/bin/ls"` smoke
test, so every suite of the port was written from scratch.

---

## 2. Findings from studying SML/NJ CML

Before the port was written, the ML sources were analysed in eight reports (events,
scheduler and threads, channels and mailboxes, SyncVar and Barrier, cml-lib, IO/OS/Sockets, the
repository's idioms, and a critic's addendum that corrects the others). This section records
what came out of that study, with the critic's corrections applied.

### 2.1 Structure of the library

Under `smlnj/libraries/cml/`:

- `src/core-cml/`: the kernel. `rep-types.sml` (representation types), `queue.sml`,
  `scheduler.sml`, `event.sml`, `thread.sml`, `channel.sml`, `timeout.sml`, `io-manager.sml`,
  `sync-var.sml`, `mailbox.sml`, `barrier.sml`, `cleanup.sml`, `running.sml`, `version.sml`,
  `debug.sml`, and the signatures. `core-cml.cm` exports the signatures `CML`, `SYNC_VAR`,
  `MAILBOX`, `BARRIER`, `CML_CLEANUP` and the structures `CML`, `Event`, `Q`, `Thread`,
  `Scheduler`, `SyncVar`, `Mailbox`, `Barrier`, `TimeOut`, `IOManager`, `Running`, `CleanUp`,
  `Debug`. The public `cml.cm` re-exports only `CML`, `SyncVar`, `Mailbox`, `Barrier`, `RunCML`
  and `Debug` (plus the `CML_*_IO` signatures): that is the public surface.
- Internal extensions of the public signatures, used by the other layers:
  `Event.{atomicCVarSet, cvarGetEvt}`, `Thread.{defaultExnHandler, reset}`,
  `TimeOut.{reset, pollTime, anyWaiting}`, `Channel.resetChan`, `Mailbox.resetMbox`.
- `src/glue/`: `run-cml-fn.sml` / `new-run-cml-fn.sml` (`RunCML.doit`, `shutdown`),
  `export-fn-fn.sml` (the scheduler, pause and shutdown hooks, `exportFn`), `init-cleanup.sml`
  (the standard cleaners), `os-glue-sig.sml`.
- `src/Unix/`: `unix-glue.sml` (`pollOS`, `pause`), `proc-manager.sml` (child reaping),
  `os-process.sml` (`OS.Process.system`, `systemEvt`, `sleep`), `new-unix.sml`
  (`Unix.execute`, `executeInEnv`, `reapEvt`), `posix-*-prim-io.sml` (readers and writers over
  descriptors), `syscall.sml`. `src/Win32/` mirrors it.
- `src/IO/`: the imperative and stream I/O functors, `new-text-io-fn.sml` (the event-valued
  `TextIO`), `chan-io-fn.sml` (streams connected to channels), `clean-io.sml`.
- `src/OS/`: the `OS`, `OS.IO` and `OS.Process` signatures.
- `src/Sockets/`: `cml-socket.sml` and the generic, INet and Unix socket layers.
- `src/util/result.sml`: `Result`, an ivar that carries a value or an exception.
- `cml-lib/`: `multicast.sml`, `simple-rpc.sml`, `trace-cml.sml`, `old-cml.sml` (not built),
  `SMLNJ-Util`, `SMLNJ-INet`.

### 2.2 Key representations (`rep-types.sml`)

```sml
datatype 'a queue = Q of {front : 'a list ref, rear : 'a list ref}   (* rear is reversed *)
datatype thread_id = TID of { id:int, alert:bool ref, done_comm:bool ref,
     exnHandler:(exn->unit) ref, props:exn list ref, dead:cvar }
and trans_id = CANCEL | TRANS of thread_id
and cvar = CVAR of cvar_state ref
and cvar_state = CVAR_unset of {transId: trans_id ref, cleanUp: unit->unit,
                                kont: unit cont} list
               | CVAR_set of int
datatype 'a event_status
  = ENABLED of {prio : int, doFn : unit -> 'a}
  | BLOCKED of {transId : trans_id ref, cleanUp : unit -> unit, next : unit -> unit} -> 'a
type 'a base_evt = unit -> 'a event_status          (* the "pollFn" *)
datatype 'a event = BEVT of 'a base_evt list | CHOOSE of 'a event list
                  | GUARD of unit -> 'a event | W_NACK of unit event -> 'a event
```

- An **event** is `BEVT l` (a flat choice among base events; `BEVT []` is `never`), `CHOOSE`
  (a choice that contains at least one non-base event), `GUARD g` or `W_NACK f`. Guards and
  nacks are delayed: they are *forced* afresh at every `sync`.
- A **base event** is a *poll function*. Called inside the atomic region, it returns
  `ENABLED {prio, doFn}` (the event can commit now) or `BLOCKED blockFn` (the sync must wait).
  - `doFn` performs the commit and leaves the atomic region exactly once.
  - `blockFn {transId, cleanUp, next}` registers the thread in the object's wait queue with
    `callcc`, then calls `next` (which registers the next base event, or dispatches another
    thread). When resumed it runs `cleanUp`, leaves the atomic region and returns the value.
  - For cvar and timeout events the waker runs `cleanUp`, not the resumed thread.
- A **transaction id** (`trans_id ref`) is one box shared by all the base events of one blocked
  sync. The waker checks that it is `TRANS tid`, sets it to `CANCEL` and only then wakes the
  thread; every other queue entry holding the same box is dead from then on and is dropped
  lazily when its queue is next consulted.
- A **cvar** is a unit-valued ivar used for thread death (`joinEvt`) and for nacks.
  `CVAR_set n` stores a polling priority. Setting a cvar twice raises
  `Fail "cvar already set"`.
- **Priorities**: `~1` is a fixed priority (`alwaysEvt`, timeouts), counted as *n*, the number
  of enabled events in the sync. A priority `>= 0` is dynamic: a counter on the object,
  incremented by each poll that finds it enabled and reset to 1 by a communication.
- **Ready queues**: `rdyQ1` (primary) and `rdyQ2` (compute-bound threads).
- **Atomic regions**: one global flag with the states `NonAtomic`, `Atomic`, `SignalPending`.
  Regions do not nest.

### 2.3 How ML schedules and synchronizes

- The **scheduler** keeps the current thread in a register. SIGALRM fires every 20 ms (the
  default time quantum). In a non-atomic state the handler preempts the thread: a thread marked
  `done_comm` (it communicated since the last preemption) is unmarked, one thread is promoted
  from `rdyQ2` and the preempted thread goes to the rear of `rdyQ1`; an unmarked thread is
  demoted to `rdyQ2`. Inside an atomic region the signal only sets `SignalPending`, honoured at
  the next `atomicEnd`, `atomicDispatch` or `atomicSwitchTo`. `rdyQ2` is served only when
  `rdyQ1` is empty, or by promotion. `atomicSwitchTo (tid, k, x)` is the direct hand-off used
  by channels. `enqueueTmpThread f` pushes a throw-away thread on the *front* of `rdyQ1`. The
  clock (`getTime`) is cached until the next SIGALRM.
- **`sync`** forces the event into an event-group tree (`BASE_GRP`, `GRP`, `NACK_GRP`):
  guards and `withNack` bodies run left to right, depth first, outside the atomic region, each
  `withNack` with a fresh cvar. Without nacks the result is a flat list of poll functions
  (`syncOnBEvts`); with nacks, `collect` assigns boolean flags to the base events and pairs
  every nack cvar with the flags of the events it covers (`syncOnGrp`).
- Polling runs every poll function once. If any is enabled, *all* are still polled (to bump
  their counters) and `selectDoFn` picks the highest priority, breaking ties with a global
  counter modulo the number of candidates.
- If none is enabled, the thread captures its continuation and calls each `blockFn`, each one
  inside the `next` of the previous one (`log`), with one shared transaction id; the last
  `next` dispatches another thread. Whichever blockFn is resumed returns its value out of
  `sync` through `escape k`.
- **Nacks**: at the commit the chosen event's flag is set, then `chkCVars` sets every nack cvar
  none of whose flags is set. In the enabled case this happens *before* the chosen `doFn`
  runs; in the blocked case it runs from the winner's `cleanUp`.

### 2.4 Notable ML facts

- **Priority rule.** The event report claimed that an enabled priority must be `>= 1` or `~1`
  and that 0 crashes `selectDoFn`. The critic corrected it: `max` starts at `maxP = 0`, so a
  priority-0 event takes the `p = maxP` branch and is kept. Priority 0 does occur: a fresh
  ivar or `mVarInit` starts its counter at 0. `Div` happens only if every enabled priority is
  negative and not `~1`. The correct invariant is *enabled priority `>= 0` or `~1`*.
- **cvar waiters wake FIFO.** The waiting list is newest first and `atomicCVarSet` prepends it
  onto the reversed rear of `rdyQ1`, so the oldest blocker is dequeued first. (The scheduler
  report had said "reverse order"; the critic corrected it.) `atomicCVarSet` does not mark the
  woken threads.
- **Timeout ties wake LIFO.** `timeWait` inserts a new entry before an existing one with the
  same deadline.
- **`recv` returns while still atomic.** Its blocking path is resumed by a sender's
  `throw rkont msg` after `enqueueAndSwitchCurThread`, with no `atomicEnd`. The port leaves the
  region explicitly.
- **The `doFn` header comment of `event.sml` is stale.** It says doFns must not leave the atomic
  region; every doFn in the code does leave it, exactly once.
- **`tidToString`** pads negative ids around the `~`: the dummy thread prints as `[0000~1]`.
- **`Mailbox.send` yields** (`atomicYield`) when the mailbox already holds messages, to keep a
  producer from outrunning its consumers.
- **SyncVar priorities**: only `iGetEvt`'s doFn and the hand-off in `iPut`/`mPut` reset the
  priority to 1. `mTakeEvt`, `mGetEvt` and `mSwapEvt` do not.
- **The core's default exception handler** (`Thread.exnHandler`) ignores the exception: a thread
  dies silently unless TraceCML's handler, installed by its server at startup, is in place.
- **`selectDoFn` tie-break** uses a global counter that wraps at 1000000: deterministic, not
  random.

### 2.5 ML bugs and doc/code mismatches

| Where | Problem | What the port does |
|---|---|---|
| `wrapHandler` | `cml.mldoc` gives `('a event * (exn -> 'a event)) -> 'a event`; `event-sig.sml` and the code give `(exn -> 'a)`. | The handler returns a value (the code's signature). |
| `event.sml` `syncOnBEvts`/`syncOnGrp` | blockFns are nested (`log`), so a `wrapHandler` on one branch catches what another branch's wrap raises after the commit. | Wrappers are kept apart from block functions and applied at the sync's own continuation ([4.4](#44-the-sync-algorithm)). |
| `event.sml` `syncOnGrp` | In the enabled case nacks are set before the chosen `doFn` runs. A nack can then cancel the very partner the doFn dequeues. | Nacks are set right after the commit, before `sync` returns. |
| `event.sml` forcing | If a guard raises, the cvars made so far are never set: the servers behind them wait forever. | The nacks made so far are set ([4.4](#44-the-sync-algorithm)). |
| `barrier.sml` (1) | An enrollment's status is never reset to `ENROLLED` after a round, so a second `wait` raises "multiple barrier waits". The documentation's own example waits five times. | Fixed. |
| `barrier.sml` (2) | `resign` never decrements `nEnrolled` and never completes a round the remaining threads are all waiting for: a resignation can deadlock the others. | Fixed. |
| `barrier.sml` (3) | `resign` on an already resigned enrollment returns without `atomicEnd`, leaving the atomic region open. | Fixed. |
| `barrier.sml` (other) | Waiters are woken newest first (an artefact of the list); the doc says `value` raises `Fail`, the code never does; the doc example applies `resign` to the barrier, not to an enrollment (ill-typed). | Waiters are woken oldest first (documented deviation); `barrier-value` never raises. |
| `trace-cml.sml` watcher | After a watched thread dies, the watcher server blocks forever on a `send` nobody receives, so every later `watch`/`unwatch` hangs; `watch` returns before the watch is registered, so an immediate `unwatch` may miss it; the death message lacks a space before the tid. | TraceCML has no servers; all three are fixed. |
| `trace-cml.sml` module names | A full name is the parent's full name followed by the label, without a separator (`"/ThreadWatcher"`), while the documentation writes `/`-separated paths. | Each label is followed by `/` (`"/ThreadWatcher/"`). |
| `simple-rpc.sml` | If the server function raises, the caller stays blocked forever. | The caller gets the exception through a `Result`. |
| IO: imperative `input1` / `inputLine` at end of file | They take the stream mvar and never put it back: the next operation on the stream deadlocks. | Not applicable (no functional streams); port events never lose their lock. |
| IO: `PrimIO` `eventWrap` | Loses the lock when the nack fires, and runs `f` without the lock in the busy case. | The helper thread holds a per-port lock, released on the nack and after the offer. |
| Sockets: guard-time operations (`recv*Evt`, `acceptEvt`) | They can consume data or a connection even when the event is not chosen. | `tcp-accept-evt` accepts only after the commit; input events commit atomically from a side buffer. |
| `Unix.reapEvt` | Does not close the process's streams, and reaping twice raises `ECHILD`. | `process-evt` is memoized per pid until reaped; a reaped numeric pid raises `ECHILD` as in ML. |
| `connectEvt` | Ignores `SO_ERROR`. | `tcp-connect-evt` uses the blocking `tcp-connect`, which reports errors (and blocks the process, see [8](#8-known-limitations-and-open-items)). |
| `pollEvt` | Only supports one descriptor and swallows every poll error, contrary to its documentation. | `poll-evt` takes any number of `(fd mode)` specs; select errors still count as "not ready". |
| `OS.Process.system'` | Leaves the timer stopped if `fork` raises; a child that fails to exec can keep running CML. | The child execs at once and exits with 127 (`system-evt`) or 128 (`cml/execute`) when the exec fails. |
| `strReader.setPos`, Win32 `readA` | Impossible `andalso` check; wrong copy length. | Not applicable. |

---

## 3. Definitions: the public API

### 3.0 Conventions

- **Import.** `guard` is CML's combinator. A client that also imports `(chicken base)` must hide
  CHICKEN's R7RS `guard` syntax:

  ```scheme
  (import scheme (except (chicken base) guard) (aux cml))
  ```

- **Options.** Every ML `'a option` is `'()` for `NONE` and `(list v)` for `SOME v`
  (`recv-poll`, `ivar-get-poll`, the `peek` of a thread property, the return value of
  `cml/add-cleaner!`, ...).
- **Time.** Durations and absolute times are real numbers of **seconds**:
  `(timeout-evt 0.5)`, `(at-time-evt (+ (cml/now) 1))`. They are converted to whole
  milliseconds, rounded up.
- **Conditions.** CML's own errors are composite conditions of kinds `(exn cml <kind>)`; the
  `exn` part's `location` property is the kind. The kinds are:

  | Kind | Raised by |
  |---|---|
  | `put` | a put on a full ivar or mvar (ML `Put`) |
  | `not-running` | a blocking or scheduling operation outside `run-cml` |
  | `running` | `run-cml` called inside `run-cml` |
  | `barrier` | "multiple barrier waits", "barrier wait after resignation", "resign while waiting" |
  | `unlog` | `cml/unlog-channel!` & co. on a name that is not logged |
  | `rpc` | a state RPC function that does not return a result and a new state (delivered to the caller) |
  | `trace` | a bad module name, a bad `trace-to` destination |
  | `no-such-module` | `trace-module-of` on a missing name |
  | `impossible` | an internal invariant violation (should never be seen) |

  Predicates: `cml-condition?`, `cml-put-condition?`, `cml-not-running-condition?`.
  **Bad arguments** (sync on a non-event, a guard returning a non-event, a non-port given to a
  port event, a bad count, a non-procedure given to `spawn` or `cml/add-cleaner!`, an ivar given
  to an mvar operation, a command that is not a string, ...) raise ordinary errors of kind
  `(exn)` in the caller; an `(exn cml)` handler does not catch them.
- **Outside `run-cml`.** Operations that can block or schedule raise `(exn cml not-running)`:
  `sync`, `select`, `send`, `recv`, `send-poll`, `recv-poll`, `ivar-get`, `mvar-take!`,
  `mvar-get`, `mvar-swap!`, `mailbox-send!`, `mailbox-recv`, `spawn`, `cml/yield`, `cml/exit`,
  `cml/shutdown`, `process-evt`, `system-evt`, `watch`. The puts of SyncVar (`ivar-put!`,
  `mvar-put!`), the non-blocking polls `ivar-get-poll`, `mvar-take-poll`, `mvar-get-poll`,
  `mailbox-recv-poll`, event construction, `barrier-enroll`/`barrier-resign`, `multicast!` and
  the TraceCML registry work outside and never wake anybody there. Note that `mailbox-send!`,
  unlike the SyncVar puts, requires a running CML, because it may yield.
- **Thread ids** print as `#<tid [000042]>`. Inside a run, the at-init cleaners are spawned
  before the thunk given to `run-cml`, so with the two standard cleaners that thunk runs as
  thread `[000002]`. Code outside any thread (the idle loop, the scheduler hook, temporary
  threads, the root context) runs as the dummy thread `[-000001]`.

### 3.1 Running CML

**`(run-cml thunk #!key (quantum 64))`** (ML `RunCML.doit`). Starts a CML session, runs the
at-init cleaners, spawns `thunk` as a thread and schedules until one of:

- a thread calls `(cml/shutdown status)`: `run-cml` runs the at-shutdown cleaners and returns
  `status`;
- no thread can run and nothing is pending on timeouts, descriptors, child processes or the
  extra OS pollers (deadlock): `run-cml` returns `'failure`. As in ML, a session whose threads
  all finish without calling `cml/shutdown` counts as a deadlock, so
  `(run-cml (lambda () 42))` returns `'failure`.

`quantum` is the number of clock ticks (CML operations, see [4.2](#42-the-scheduler)) between
two preemptions (default 64); anything but a positive exact integer, like a `thunk` that is not a
procedure, is an error of kind `(exn)` raised before the session starts (ML falls back to 20 ms
for a non-positive quantum). A nested `run-cml` raises
`(exn cml running)`. Every session starts with fresh scheduler state: ready queues, timeouts,
pending I/O, tids (restarting from 0), the tie-break counter. Registered cleaners, logged
channels, mailboxes and servers, uncaught-exception handlers, trace settings and `cml/debug?`
are global and persist across sessions, as in ML; children still running when a session ends
are reaped by later ones. Leaving `run-cml` by an exception or by escaping with a continuation
resets the state (no at-shutdown cleaners run then) and leaves CML ready for the next session.
New threads inherit the dynamic context of the `run-cml` call (for example its
`current-output-port`), not that of their parent.

**`(cml/shutdown #!optional (status 'success))`** (ML `RunCML.shutdown`). Ends the session from
any thread. A shutdown called during the shutdown cleaners just stops them.

**`(cml-running?)`** (ML `RunCML.isRunning`).

### 3.2 Events

**`never-evt`** (ML `never`): an event that is never enabled. `(sync never-evt)` blocks the
thread for good.

**`(always-evt v)`** (ML `alwaysEvt`): always enabled with fixed priority, value `v`.

**`(wrap evt f)`** (ML `wrap`): the value of `evt` passed to `f`. `f` runs after the commit,
outside the atomic region, at the continuation of the `sync` (deviation: see
[4.4](#44-the-sync-algorithm)). `f` may return several values; they are returned by `sync`.

**`(wrap-handler evt h)`** (ML `wrapHandler`, with the signature of `event-sig.sml`): if the
commit or the wrap functions inside it raise a condition `e`, the value is `(h e)`. The handler
covers only its own branch (deviation: in ML it also caught what other branches' wraps raised
after a blocked sync). Nested handlers apply innermost first. A handler does not cover guard or
`with-nack` bodies. Each handler is a `condition-case` frame around the wrap functions inside
it, so those do not run in tail position: a loop that recurses from a wrap under a
`wrap-handler` nests one frame per iteration and costs O(depth) per thread switch (recurse
outside the handler).

**`(guard thunk)`** (ML `guard`): `thunk` runs at every sync on the event, before any polling,
and must return an event (else an `(exn)` error is raised).

**`(with-nack f)`** (ML `withNack`): at every sync, `f` receives a fresh nack event and must
return an event. The nack event becomes enabled if the sync commits on a branch outside the
event `f` returned, and also, unlike ML, when the sync is abandoned while forcing or polling or
by the death of its thread ([4.4](#44-the-sync-algorithm)). It is set at most once per sync.

**`(choose evt ...)`**, **`(choose* evts)`** (ML `choose`): the choice. Adjacent base events
are flattened into one base event; `(choose)` is an empty base event, which behaves as `never-evt`.

**`(sync evt)`** (ML `sync`). **`(select evt ...)`**, **`(select* evts)`** (ML `select`):
`(sync (choose evt ...))`.

**`(select/case clause ...)`**: syntax for a select whose clauses are
`(evt formals body ...)` (the value is bound as by `lambda formals`), `(evt () body ...)` (the
value is ignored) or `(evt => proc)`. Example:

```scheme
(select/case
  ((recv-evt ch) (v) (loop (cons v acc)))
  ((timeout-evt 0.1) _ (cml/shutdown (reverse acc))))
```

**`(sync/timeout evt secs #!optional (default #f))`**: not in ML. Syncs on `evt`, returning
`default` after `secs` seconds. With `secs <= 0` it is a poll: `evt` is chosen whenever one of
its branches is enabled, `default` only when none is (the fallback has a private priority below
every other one).

**`(event? x)`**.

### 3.3 Threads

**`(spawn thunk)`** (ML `spawn`), **`(spawn/call f x)`** (ML `spawnc`): create a thread and
return its tid. The child runs first; the parent goes to the rear of the primary ready queue. A
non-procedure raises an `(exn)` error in the caller. An uncaught condition in the thread is
passed to the thread's exception handler (copied from `default-exn-handler` at spawn time); a
condition raised by that handler is ignored; then the thread dies.

**`(current-tid)`** (ML `getTid`). **`(tid? x)`**, **`(tid=? a b)`** (ML `sameTid`),
**`(tid<? a b)`**, **`(tid-compare a b)`** → -1, 0 or 1 (ML `compareTid`), **`(tid-hash t)`**
(ML `hashTid`), **`(tid->string t)`** → `"[000042]"`, negative ids as `"[-000001]"` (ML
prints `"[0000~1]"`).

**`(join-evt tid)`** (ML `joinEvt`): enabled once `tid` has died (normal return, `cml/exit` or
uncaught condition). Its value is unspecified. Waiters wake FIFO.

**`(cml/yield)`** (ML `yield`): the current thread goes to the rear of the ready queue. It is a
clock tick, and a yielding thread is preempted like a marked one when the quantum expires.

**`(cml/exit)`** (ML `exit`): the current thread dies. Its properties are cleared.

**`(make-thread-property init)`** (ML `newThreadProp`) → `(values clear! get peek set!)`:
per-thread values keyed by a fresh key. `get` returns the thread's value, initialising it with
`(init)` on first use; `peek` returns `'()` or `(list v)`.

**`(make-thread-flag)`** (ML `newThreadFlag`) → `(values get set!)`: a per-thread boolean,
initially `#f`.

**`default-exn-handler`**: a parameter holding the procedure `(λ (condition) ...)` copied into
each new thread. The core default prints the condition to `(current-error-port)` with the
message "cml: thread [id] died of an uncaught exception". Once the module is loaded (cml-lib
is in the same file), its value is `trace-exn-handler`, which dispatches through the TraceCML
registry ([3.17](#317-tracecml)); parameterize `default-exn-handler` around a `spawn` to bypass
it for that thread.

### 3.4 Channels

**`(make-channel)`** (ML `channel`), **`(channel? x)`**, **`(channel=? a b)`** (ML
`sameChannel`).

**`(send ch v)`**, **`(recv ch)`** (ML `send`, `recv`): synchronous rendezvous; each blocks
until a partner arrives. `recv` leaves the atomic region when resumed (ML returns inside it).

**`(send-evt ch v)`**, **`(recv-evt ch)`** (ML `sendEvt`, `recvEvt`): the event forms.
`send-evt`'s value is unspecified.

**`(send-poll ch v)`** → `#t` iff a receiver was already blocked and got `v` (ML `sendPoll`).
**`(recv-poll ch)`** → `'()` or `(list v)` (ML `recvPoll`).

### 3.5 Timeouts and time

**`(timeout-evt secs)`** (ML `timeOutEvt`): enabled `secs` seconds after the sync blocks on it;
0 is enabled at once; a negative duration fires at the next time check. Fixed priority. The
value is unspecified.

**`(at-time-evt abs-secs)`** (ML `atTimeEvt`): enabled at the absolute time `abs-secs` of the
`(cml/now)` clock.

**`(cml/now)`**: the absolute clock in seconds (a real), read on demand.

**`(cml/sleep secs)`** (ML `OS.Process.sleep`): `(sync (timeout-evt secs))`.

### 3.6 Synchronization variables (SyncVar)

ivars are write-once, mvars are one-place buffers. Both are one record type, and every
operation checks the kind: an ivar given to an mvar operation (or the reverse) raises an
`(exn)` error. A put on a full variable raises `(exn cml put)`.

| Procedure | ML | Semantics |
|---|---|---|
| `(make-ivar)` | `iVar` | empty ivar |
| `(ivar-put! iv v)` | `iPut` | fill it and wake every reader |
| `(ivar-get iv)`, `(ivar-get-evt iv)` | `iGet`, `iGetEvt` | its value, blocking while empty |
| `(ivar-get-poll iv)` | `iGetPoll` | `'()` or `(list v)` |
| `(make-mvar)`, `(make-mvar v)` | `mVar`, `mVarInit` | empty or full mvar |
| `(mvar-put! mv v)` | `mPut` | fill it, waking readers in order until one takes it |
| `(mvar-take! mv)`, `(mvar-take-evt mv)` | `mTake`, `mTakeEvt` | take the value, leaving it empty |
| `(mvar-take-poll mv)` | `mTakePoll` | `'()` or `(list v)`, taking it |
| `(mvar-get mv)`, `(mvar-get-evt mv)` | `mGet`, `mGetEvt` | read without taking |
| `(mvar-get-poll mv)` | `mGetPoll` | `'()` or `(list v)` |
| `(mvar-swap! mv new)`, `(mvar-swap-evt mv new)` | `mSwap`, `mSwapEvt` | the old value, `new` stored in its place |
| `(ivar? x)`, `(mvar? x)`, `(ivar=? a b)`, `(mvar=? a b)` | `sameIVar`, `sameMVar` | predicates, `eq?` |

The read-only polls `ivar-get-poll` and `mvar-get-poll` are clock ticks, so a thread
busy-waiting on them is preempted.

### 3.7 Mailboxes

**`(make-mailbox)`**, **`(mailbox? x)`**, **`(mailbox=? a b)`** (ML `mailbox`, `sameMailbox`).

**`(mailbox-send! mb v)`** (ML `Mailbox.send`): asynchronous, never blocks: hands `v` to a
waiting receiver or buffers it; when the mailbox already held messages the sender yields (as in
ML). Requires a running CML.

**`(mailbox-recv mb)`**, **`(mailbox-recv-evt mb)`** (ML `recv`, `recvEvt`): the oldest
message, blocking while empty. **`(mailbox-recv-poll mb)`** → `'()` or `(list v)`.

### 3.8 Barriers

**`(make-barrier init update)`** (ML `Barrier.barrier update init`, **arguments in the
opposite order**, a documented deviation): a barrier whose state starts at `init`; `update` maps the state to the next
one each time a round completes.

**`(barrier-enroll b)`** → an enrollment (ML `enroll`).

**`(barrier-wait-evt e)`** (not in ML: barriers have no events there), **`(barrier-wait e)`**
(ML `wait`): wait until every enrolled enrollment waits; the last arrival runs `update` and
every waiter gets the new state. If `update` raises, every participant raises that condition
and the state is unchanged. A wait after `barrier-resign` raises `(exn cml barrier)`
"barrier wait after resignation"; two waits on one enrollment in one sync, or a wait while the
enrollment already waits, raise "multiple barrier waits". A `barrier-wait-evt` that loses a
choice is not counted as arrived; one sync waiting on several enrollments of the same barrier
counts once. Waiters are woken oldest first.

**`(barrier-resign e)`** (ML `resign`): leave the barrier. Resigning twice is ignored; resigning
while waiting raises "resign while waiting". A resignation that leaves only waiting threads
completes the round.

**`(barrier-value e)`** (ML `value`): the current state; never raises.

**`(barrier? x)`**, **`(enrollment? x)`**.

### 3.9 Results

`Result` (`util/result.sml`) is an ivar that carries a value or a condition.

**`(make-result)`**, **`(result? x)`**, **`(result-put! r v)`**, **`(result-put-exn! r e)`**,
**`(result-get r)`** (returns the value or raises the stored condition), **`(result-get-evt r)`**.
A second put raises `(exn cml put)`.

### 3.10 Cleaners and logged objects (CleanUp)

**`cml/at-all`** = `'(at-exit at-shutdown at-init at-init-fn)`. `run-cml` triggers only
`at-init` (at session start) and `at-shutdown` (after `cml/shutdown`); the other two belong to
`exportFn` and are never triggered.

**`(cml/add-cleaner! name whens proc)`** (ML `addCleaner`): register `(proc when)` under `name`
(compared with `equal?`) for `whens`, which is `'all`, one of the symbols of `cml/at-all` or a
list of them. Returns the previous `(whens proc)` for that name as an option. A non-procedure or
bad `whens` raises an `(exn)` error at registration.

**`(cml/remove-cleaner! name)`** (ML `removeCleaner`): returns the removed `(whens proc)` as an
option.

Cleaners run each in its own thread and are given at most 1 second each; at-init cleaners run
oldest first, at-shutdown cleaners newest first. Standard cleaners registered at load time, in
this order: `"Channels&Mailboxes"` (at-init, at-shutdown: resets the logged channels and
mailboxes), `"TraceCML"` (at-shutdown: closes trace files) and `"Servers"` (all: runs the init
thunks of logged servers at init, their shutdown thunks at shutdown, each shutdown thunk in its
own thread given at most 2 seconds). Because shutdown runs newest first, the trace files are
closed after the servers are shut down.

**`(cml/log-channel! name ch)`**, **`(cml/log-mailbox! name mb)`** (ML `logChannel`,
`logMailbox`): the object is reset (queues emptied, priority back to 1) at every session start
and shutdown. **`(cml/log-server! name init shut)`** (ML `logServer`). Each checks its argument
types at registration. **`(cml/unlog-channel! name)`**, **`(cml/unlog-mailbox! name)`**,
**`(cml/unlog-server! name)`** raise `(exn cml unlog)` for a name that is not logged.
**`(cml/unlog-all!)`**.

Registrations are serialized by an internal lock while CML runs (ML `CleanUp.protect`).

### 3.11 Descriptor and process events

**`(poll-evt spec ...)`**, **`(poll-evt* specs)`** (ML `IOManager` / `OS.IO.pollEvt`, extended
to several descriptors): each spec is `(fd mode)` with `mode` `'input` or `'output`; the event
is enabled when at least one is ready and its value is the list of ready specs. No spec gives
`never-evt`. Fixed priority. A select error counts as "not ready".

**`(io-evt fd mode)`** (ML `IOManager.ioEvt`): `poll-evt` on one descriptor; the value is `fd`.

**`(process-evt pid)`** (ML `ProcManager.addPid`, `Unix.reapEvt`): `pid` is a positive exact
integer or a CHICKEN process object. The value is `(list exited-normally? code)`. Memoized per
pid while the child is not reaped. A numeric pid that was already reaped raises `ECHILD` (as
in ML) when synced on; a process object keeps its status and gives it again.

**`(system-evt cmd)`** (ML `OS.Process.systemEvt`): runs `cmd` through `/bin/sh -c` **at once**
(not at sync time); the value is the exit status as an integer (0 success, 128 + n when killed
by signal n, 127 when `/bin/sh` cannot be run). Syncing again gives the same status.
**`(cml/system cmd)`** (ML `OS.Process.system`) syncs on it.

**`(cml/execute cmd #!optional (args '()) env)`** (ML `Unix.execute`, `Unix.executeInEnv`) →
`(values in out pid)`: `in` reads the child's stdout and `out` writes its stdin, both
descriptor-backed ports; reap it with `(process-evt pid)`. The parent's pipe ends are
close-on-exec. A failed exec exits with status 128, as in ML. Deviations: a `cmd` without `/`
is searched in `PATH` and `argv[0]` is `cmd` as given (ML execs the path as given with its
basename as `argv[0]`). `env`, a list of `"NAME=value"` strings, is the child's whole
environment; with it, `cmd` is not searched in `PATH` (as ML's `executeInEnv`). Children of
`system-evt` and `cml/execute` start with SIGPIPE at its default action.

### 3.12 Port events

Built on text ports (ML `TextIO` events of `new-text-io-fn.sml`). An input event on a closed
port raises at sync. Once input events are used on a port, keep using input events on it:
direct reads may miss characters held in the port's side buffer ([4.12](#412-port-input-events)).

| Event | ML | Value |
|---|---|---|
| `(input-char-evt port)` | `TextIO.input1Evt` | the next char, or `#!eof` |
| `(peek-char-evt port)` | (none) | the next char without consuming it, or `#!eof` (the end of file stays pending) |
| `(input-line-evt port)` | `inputLineEvt` | the next line without its terminator (`"\n"`, `"\r\n"` or a lone `"\r"`, as `read-line`); the last unterminated line at end of file, then `#!eof` |
| `(input-string-evt port n)` | `inputNEvt` | `n` chars, fewer only at end of file, `#!eof` when nothing is left; `n` must be an exact non-negative integer |
| `(input-evt port)` | `inputEvt` | as soon as one char is there, every char available by then (at most 4096 beyond what was buffered); `#!eof` at end of file |
| `(input-all-evt port)` | `inputAllEvt` | everything up to end of file, `""` when nothing is left |
| `(output-evt port)` | (none) | `port`, once it can take output without blocking the process |
| `(write-string-evt port s)` | `TextIO.output` as an event | commits when the port is writable, then writes `s` (blocking only the syncing thread) |

A branch that loses a `select`, or a timeout that fires in the middle of a line, loses no
input. Two `write-string-evt`s on one port never interleave their strings. A write to a pipe
whose reader has exited raises the i/o condition of `file-write` with errno `EPIPE`. Output
events on a closed port raise at every sync.

### 3.13 TCP

**`(tcp-accept-evt listener)`** (ML `Socket.acceptEvt`): enabled when a connection is pending;
the connection is accepted after the commit, so a losing branch never consumes one. The value is
`(list in out)`.

**`(tcp-connect-evt host #!optional port)`** (ML `Socket.connectEvt`): always enabled; connects
after the commit with `tcp-connect`, which **blocks the whole process** during the handshake.
The value is `(list in out)`.

Port events apply to the resulting ports.

### 3.14 Channel ports (ChanIO)

**`(open-channel-input-port ch)`** (ML `ChanIO.mkReader`): an input port reading the strings
(or chars) received on `ch`. Empty strings are skipped, `#!eof` ends the port for good; any
other value is an error raised by the read that reaches it (an input event delivers it as its
value's condition, keeping the chars before it for the next input event). A read on an empty
port blocks the calling CML thread only.

**`(open-channel-output-port ch)`** (ML `ChanIO.mkWriter`): buffers what is written and sends it
as one string at every `flush-output` (or every 1024 characters); `close-output-port` flushes
and sends `#!eof`. The sends block the calling CML thread until a receiver takes them. No empty
string is ever sent. `write-string-evt` on such a port is the rendezvous that sends the
buffered output followed by its string, the buffer being taken at the commit.

### 3.15 Multicast

**`(make-multicast-channel)`** (ML `mChannel`), **`(multicast-channel? x)`**,
**`(multicast-port mc)`** (ML `port`): a port that sees every message multicast after its
creation; it spawns a "tee" thread, so it needs a running CML and belongs to that session.
**`(multicast-copy-port port)`** (ML `copy`): a port whose future stream is that of `port`
(exact when `port` has a single reader). **`(multicast! mc v)`** (ML `multicast`): never blocks;
inside `run-cml` it yields. **`(multicast-recv port)`**, **`(multicast-recv-evt port)`** (ML
`recv`, `recvEvt`), **`(multicast-port? x)`**. Several readers of one port share its messages,
each message going to one of them.

### 3.16 SimpleRPC

Each maker returns `(values call entry)`. `(call arg)` sends the request to a mailbox and
waits for the answer. The server loop syncs on the entry event (usually in a `select` with its
other duties). If the server function raises, `call` raises the same condition in the caller
(ML leaves the caller blocked forever).

| Maker | ML | Server function | Entry |
|---|---|---|---|
| `(make-rpc f)` | `mkRPC` | `(f arg)` → result (`(void)` if it returns no value) | event, unspecified value |
| `(make-rpc/in f)` | `mkRPC_In` | `(f arg state)` → result | `(entry state)` → event |
| `(make-rpc/out f)` | `mkRPC_Out` | `(f arg)` → `(values result new-state)` | event whose value is `new-state`; when `f` raises, the event re-raises it in the server too |
| `(make-rpc/in-out f)` | `mkRPC_InOut` | `(f arg state)` → `(values result new-state)` | `(entry state)` → event whose value is `new-state`, or `state` when `f` raised |
| `(make-rpc/state init f)` | (none) | `(f arg state)` → `(values result new-state)` | event whose value is the new state; the state is kept inside, read and written when a request is served, so several server threads can share it |

A state maker whose `f` does not return two values answers the caller with `(exn cml rpc)`.

### 3.17 TraceCML

**Modules.** `trace-module/root` (ML `traceRoot`, name `"/"`), **`(trace-module parent name)`**
(ML `traceModule`: the child `name` of `parent`, created with `parent`'s flag when missing;
`name` is a non-empty string without `/`), **`(trace-module-name m)`** (ML `nameOf`, e.g.
`"/ThreadWatcher/"`), **`(trace-module-of name)`** (ML `moduleOf`: empty arcs ignored, raises
`(exn cml no-such-module)`), **`(trace-module? x)`**. Wherever a module is expected, its name
is accepted too.

**Flags.** **`(trace-on! m)`**, **`(trace-off! m)`** (the module and its descendants),
**`(trace-on-only! m)`** (the module alone), **`(tracing? m)`** (ML `amTracing`),
**`(trace-status m)`** → list of `(module . tracing?)`, pre-order, children newest first.

**Output.** **`(trace m thunk)`**: when `m` is traced, `thunk` runs in the calling thread and
the strings (or displayable objects) of the list it returns are printed, concatenated, to the
destination. **`trace-to`**: a parameter holding the destination: `'out` (default), `'err`,
`'null`, an output port, a file name (opened on first use, closed at every shutdown and then
reopened for appending; if it cannot be opened, output goes to stdout with a warning), a
channel or a mailbox (the string is sent, only while CML runs). A bad destination raises
`(exn cml trace)`. **`(trace-close-files!)`** closes the trace files.

**Watching.** `trace-watcher` (ML `watcher`, the module `"/ThreadWatcher/"`, on at load time).
**`(watch name tid)`**: report the death of `tid` on `trace-watcher` as
`"WARNING!  Watched thread <name> [id] has died."`; requires a running CML; watching again
replaces the previous watch. **`(unwatch tid)`** (a no-op when not watched),
**`(watched? tid)`**.

**Uncaught exceptions.** **`(set-uncaught-handler! h)`** (ML `setUncaughtFn`): the default
handler, `(h tid condition)`. **`(add-uncaught-handler! h)`** (ML `setHandleFn`): `h` returns
`#t` when it handled the condition; the newest is tried first; the default runs when none
handles it or one raises. **`(reset-uncaught-handlers!)`** (ML `resetUncaughtFn`).
**`uncaught-default-handler`**: the built-in default. **`trace-exn-handler`**: the value
installed in `default-exn-handler`; it runs in the dying thread, before its join event fires.

### 3.18 Version and debugging

**`cml/version`** =
`((system . "Concurrent ML (aux cml)") (version-id 1 0 10) (date . "September 15, 1997"))`
(ML `version.sml`'s id and date). **`cml/banner`** =
`"Concurrent ML (aux cml), Version 1.0.10, September 15, 1997"`.

**`cml/debug?`**: a parameter, `#f` by default. **`(cml/debug string ...)`** (ML
`Debug.sayDebugId`): when `(cml/debug?)`, prints the current tid and the strings to
`(current-error-port)`.

### 3.19 Internal hooks for extensions

Everything is exported; names with a `%` are internal. The header lists these as the hooks
intended for later layers:

| Hook | Purpose |
|---|---|
| `(%base-evt poll)` | a base event from a poll thunk; the thunk runs atomic and returns a poll result |
| `(make-%enabled prio do-thunk)` | an enabled poll result. `prio` >= 0 is dynamic, -1 fixed. The do-thunk commits and must leave the atomic region exactly once (`%atomic-end`, a switch, or a dispatch), then return the value |
| `(make-%blocked (λ (trans cleanup next) ...))` | a blocked poll result. The block function registers `trans` and a continuation, calls `(next)` (which never returns), and when resumed runs `(cleanup)`, leaves the atomic region and returns the value. `%blocked-post` holds the wrappers, kept apart from the block function |
| `%atomic-begin`, `%atomic-end`, `%atomic-dispatch`, `%dispatch` | `Scheduler.atomicBegin` & co. (renamed from `atomic-*` in review round 1) |
| `(%enqueue-thread! tid k)` | wake `tid` later by calling `(k (void))`, marking it |
| `(%enqueue-tmp-thread! thunk)` | run `thunk` soon, as the dummy thread, exceptions ignored (ML `enqueueTmpThread`) |
| `(%trans-live? trans)`, `(%trans-cancel! trans)` | transaction boxes handed to block functions; a waker must check `%trans-live?` and claim the transaction before waking |
| `(%add-os-poller! name poll waiting?)` | extra polling at every preemption tick and while idle; `waiting?` returns `#f` (nothing awaited), a bound in ms on the idle sleep, or another true value (poll every 5 ms). `%remove-os-poller!` removes one |

Other internals that a layer may find useful: `%send-evt/commit` (a send event whose message is
computed at the commit, while atomic), `%spawn/queued!` (a thread that runs once the current one
blocks or yields), `%yield/poll!` (a yield that is also a preemption), `%hold-take!` /
`%hold-put!` (take and give back an internal lock mvar that is released if its holder dies).
The public `poll-evt*` and `io-evt` are the base on which the port events are built.

---

## 4. Algorithms

### 4.1 Threads are continuations

A thread is a `%tid` record plus, whenever it is not running, a continuation stored in a ready
queue or in some object's wait queue:

```scheme
(define-record %tid id done-comm exn-handler props dead run forcing watch trans nacks holds)
```

`done-comm` is ML's mark; `dead` is the cvar that `join-evt` waits on (a thread is dead once it
is set); `run` is the session number; `forcing`, `trans`, `nacks` and `holds` record what must
be undone if the thread dies at an awkward moment ([4.3](#43-thread-death)); `watch` is its
TraceCML watch.

Continuations are captured with plain `call/cc`, through a macro that also ends the "switch in
progress" state once the continuation is resumed:

```scheme
(define-syntax %letcc/call
  (syntax-rules ()
    ((_ hop body ...) (let1 (v (call-with-current-continuation (λ (hop) body ...))) (%switched-in!) v))))
```

Plain `call/cc` was chosen over `continuation-capture`/`continuation-graft` because only
`call/cc` saves and restores the dynamic context per thread: `dynamic-wind` frames,
`parameterize` and exception handlers. The consequence, documented in the header and the
README, is that the before/after thunks of a thread's `dynamic-wind` frames run at **every**
switch out of and into it, and they run with `%cur-tid` already set to the *target* of the
switch. So these thunks must not use CML operations or `(current-tid)`, and `dynamic-wind`
cannot protect a critical section (its after thunk would release a lock at the first switch):
take and put an mvar instead. Switching with `continuation-graft` would skip the winders, but
then `parameterize` would leak between threads.

A new thread does not start inside its parent's dynamic context. `run-cml` captures an
*isolation continuation* `%base-k` once (ML's `isolate`); `(%base-k thunk)` runs `thunk` there.
`spawn/call` switches to `%base-k` with the child's body:

```scheme
(define (spawn/call f x)
  ...
  (%atomic-begin)
  (let1 (id (%new-tid))
    (%letcc/call parent-k
      (%enqueue-and-switch-cur-thread! parent-k id)       ; parent to the rear of rdyQ1, marked
      (%switch! %base-k (τ (%atomic-end)
                           (handle-exceptions e (%thread-died! id e) (f x))
                           (%notify-and-dispatch id))))
    id))
```

The scheduler hook and the idle loop also run in the isolated context, as the dummy thread.

Every switch goes through `%switch!` (or `%box-switch!`, which also records the SyncVar or
Mailbox whose value is being handed over). Between the call of `(k x)` and the arrival of the
target at its resume point, `%switch-k`, `%switch-x` and `%switch-box` describe the switch;
`%letcc/call` clears them on arrival. If a dynamic-wind thunk raises during that window, the
thread-death code knows which switch it interrupted ([4.3](#43-thread-death)).

### 4.2 The scheduler

**State.** `%rdy-q1`, `%rdy-q2` (two-list FIFO queues, `rear` reversed as in `RepTypes.Q`),
`%cur-tid`, `%atomic-state` (`non-atomic` | `atomic`), `%ticks` and
`%quantum` (default 64), `%base-k`, `%shutdown-k`, `%running`, `%run-id`. ML's third state,
`signal-pending`, is set only by its SIGALRM handler; there are no signals here, so the state
and the branches of `atomicEnd`, `atomicDispatch` and `atomicSwitchTo` that test for it are
left out.

**Atomic regions** do not nest. `%atomic-begin` sets `atomic`. `%atomic-end` first runs a
pending commit hook ([4.4](#44-the-sync-algorithm)), then leaves the region and counts a
**clock tick**:

```scheme
(define (%atomic-end)
  (when %commit-hook (%run-commit-hook!))
  (set! %atomic-state 'non-atomic)
  (set! %ticks (sub1 %ticks))
  (when (<= %ticks 0)
    (set! %ticks %quantum)
    (when %running (%letcc/call k (%preempt! k) (%dispatch-scheduler-hook))))
  (void))
```

Since every CML operation leaves the atomic region through `%atomic-end` (the read-only polls
included, which do an empty `%atomic-begin`/`%atomic-end`), every operation is a tick. The
other ticks are the direct hand-off `%atomic-switch-to` (a channel rendezvous), `%atomic-yield`
(`cml/yield`, `mailbox-send!` on a non-empty mailbox, `multicast!`). A thread that computes
without calling CML is never preempted.

**Preemption** reproduces ML's SIGALRM handler with one change: a thread is promoted from
`rdyQ2` at **every** preemption, also when the preempted thread is demoted.

```scheme
(define (%preempt! k)
  (let1 (cur %cur-tid)
    (%promote!)                                        ; ML: only when cur was marked
    (if (%tid-done-comm cur)
      (begin (%tid-done-comm-set! cur #f) (%enqueue! (cons cur k)))
      (%q-enqueue! %rdy-q2 (cons cur k)))))
```

The reason is determinism: ticks are counted, not timed, so a loop can be preempted at the same
point of every iteration, always in an unmarked thread (a child it just spawned, say), and
never promote anybody from `rdyQ2` while it keeps `rdyQ1` busy (review round 3). A yield whose
tick expires unmarks the thread, promotes one thread and requeues it on `rdyQ1`.

**Direct hand-off.** `(%atomic-switch-to tid k x)` resumes `tid` at `k` with `x` and puts the
current thread, marked, at the rear of `rdyQ1`. It never resumes a dead `tid` (the current
thread just leaves the region). It counts a tick: two threads trading messages over channels
hand off to each other through here without ever calling `%atomic-end`, and before round 6 they
starved timeouts, descriptors, processes and `rdyQ2` forever. When the quantum expires the
switch becomes a preemption: the target is queued to resume `k` with `x` after the promoted
thread, the current thread (unmarked) behind it, and the scheduler hook polls.

**Dispatch.** `%atomic-dispatch` dequeues from `rdyQ1`, else `rdyQ2`, else returns the pause
item (the idle loop). A dead thread's entry is skipped. `%dispatch` = `%atomic-begin` +
`%atomic-dispatch`. Syncing on `never-evt` is `%dispatch`: the thread is left on no queue.

**The scheduler hook** (ML `pollK`) runs as the dummy thread in the isolated context: it polls
the timeout heap and the extra OS pollers at every tick, and the descriptors and child
processes only once `%poll-interval-ms` (2 ms), or ten times as long as their previous poll
took, have passed since that poll. A `select` over thousands of descriptors takes milliseconds;
at a tick every 64 operations it would slow down every thread in proportion to the number of
idle waiters (review round 9).

```scheme
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
```

**The idle loop** (ML `pauseK` + `UnixGlue.pause`) runs when both ready queues are empty. It
polls everything, dispatches if anything was woken, and otherwise sleeps:

```scheme
(define (%pause-k)
  (%atomic-begin)
  (let loop ()
    (%poll-os!)
    (cond
      ((not (%q-empty? %rdy-q1)) (%atomic-dispatch))
      ((%os-pause!) (loop))
      (else (set! %atomic-state 'non-atomic) (%shutdown-k (cons #t 'failure))))))
```

`%os-pause!` computes a bound on the sleep: the time to the next timeout, the numeric answers of
the OS pollers' `waiting?` thunks, and 5 ms (`%idle-poll-interval-ms`) when child processes are
awaited or a poller answered a non-numeric true value (there is no SIGCHLD handler). If
descriptors are awaited it sleeps in `file-select` on them with that bound (forever if there is
none); otherwise it sleeps for the bound with an empty `file-select`. When there is nothing to
wait for at all it returns `#f`: **deadlock**, and `run-cml` returns `'failure`. Threads
blocked on channels, SyncVars, mailboxes, barriers or `never-evt` do not count as pending.

**Clock.** `(%now-ms)` is `current-process-milliseconds` plus an epoch computed once from
`current-seconds`, read on demand (ML caches it per quantum). `cml/now` is that in seconds.

**Sessions.** `run-cml`:

```scheme
(define (run-cml thunk #!key (quantum %default-quantum))
  (when %running (%cml-raise 'running "run-cml: CML is already running"))
  (unless (procedure? thunk) (error "run-cml: not a procedure" thunk))
  (unless (and (exact-integer? quantum) (> quantum 0))
    (error "run-cml: quantum must be a positive exact integer" quantum))
  (%reset!)
  (set! %quantum quantum) (set! %ticks %quantum) (set! %running #t)
  (dynamic-wind
    void
    (τ (handle-exceptions e
         (begin (%reset!) (set! %running #f) (abort e))
         (let* ((result (%letcc/call done-k
                          (set! %shutdown-k done-k)
                          (let1 (th (%letcc/call k (set! %base-k k) #f))      ; the isolation point
                            (when th (th) (%impossible 'isolated-thread-returned)))
                          (%cleanup-run 'at-init)
                          (spawn thunk)
                          (%dispatch)))
                (status (cdr result)))
           (%letcc/call finish-k
             (set! %shutdown-k (λ ignored (finish-k #f)))    ; a shutdown during shutdown stops the cleaners
             (%cleanup-run 'at-shutdown))
           (%reset!) (set! %running #f)
           status)))
    (τ (when %running (%reset!) (set! %running #f)))))       ; left by a continuation
```

`%reset!` increments `%run-id`, restarts tid numbering, makes a new dummy thread, empties the
ready queues, the timeout heap and the I/O waiters, moves the pending child pids to the orphan
list, resets the tie-break counter, the cleanup lock, the continuations, the switch state and
the commit hook. Because a transaction records its session (`%trans-live?` checks
`%running`, the tid's run and that the tid is not dead), a waiter left in a channel, SyncVar
or mailbox by an earlier session, or by a failed call outside `run-cml`, is stale and can never
resume a dead continuation.

### 4.3 Thread death

A thread dies by returning, by `cml/exit`, or by an uncaught condition. The ordinary path is
`%notify-and-dispatch`, which marks it dead (`%mark-dead!`: gives back the internal locks it
holds, sets the nacks of syncs it was still forcing, and sets its `dead` cvar, waking the
`join-evt` waiters FIFO) and dispatches.

The hard case is a thread that dies **during a switch**, because a `dynamic-wind` thunk of its
own raised:

- an *after* thunk raised as it was being switched out: `%cur-tid` is already the target, which
  has been taken off its queue;
- a *before* thunk raised as it was being switched in by a partner that handed it something (a
  channel partner record, a SyncVar value, a Mailbox message).

`%thread-died!` handles both, then runs the handler:

1. If the thread died while being switched in (`target = id`): a channel partner it received
   and has not claimed is woken with `%rendezvous-failed` so that the receiver receives again;
   a SyncVar value is relayed to the next reader by a temporary thread that reads the variable
   when it runs (`%cell-wake-next!`); a Mailbox message goes to the next receiver or back to the
   front of the mailbox (`%mailbox-redeliver!`).
2. If it died while being switched out: the interrupted switch is pushed on the **front** of
   `rdyQ1`, to go on once the handler is done; a partner record it handed over is voided so the
   partner never resumes the dead receiver.
3. `%abandon-blocked!`: its pending transaction is cancelled, its internal locks are given back,
   its leftover ready-queue entries are removed (the queues are scanned only when it died during
   a switch, so n threads dying of ordinary conditions cost O(n)), and every unset nack of the
   blocked sync it was in is set, the chosen branch's included (it never learnt which one was
   chosen).
4. Its exception handler runs, as that thread, with conditions it raises ignored. Because
   everything others wait for was done first, the handler may call `cml/exit` or block.
5. `%notify-and-dispatch`.

Internal locks (the per-port write lock, the cleanup lock) are taken with `%hold-take!`, which
records them on the tid inside the atomic region; `%release-holds!` puts them back, each in a
temporary thread, when the holder dies.

### 4.4 The sync algorithm

**Representation.** An event is `(%event tag payload)`: `base` with a list of poll thunks,
`choose` with a list of events, `guard` with a thunk, `nack` with a procedure. A poll returns
`(make-%enabled prio do-thunk)` or a `%blocked*` record with a block function and a *post*
procedure. `wrap` and `wrap-handler` map over the tree (`%map-event`), lazily through guards and
nacks; on an enabled result they compose the function into the do-thunk, on a blocked result
into the post, never into the block function:

```scheme
(define (wrap evt f)
  (%map-event evt (λ (poll)
                    (τ (let1 (s (poll))
                         (if (%enabled? s)
                           (let1 (d (%enabled-do s)) (make-%enabled (%enabled-prio s) (τ (f (d)))))
                           (let1 (p (%blocked-post s))
                             (make-%blocked* (%blocked-block s) (λ (th) (f (p th)))))))))))
```

`choose*` flattens its arguments as ML's `gatherBEvts`/`gather`: adjacent base events are
merged into one base event, nested chooses are spliced, and a choice of one event is that
event.

**Step 1: forcing.** `sync` on a base event skips forcing. Otherwise the event is forced into a
group tree, `(base . polls)`, `(grp . groups)` or `(nack cvar group)`, by `%force*`,
`%force-bl` and `%force-l`, transcriptions of ML's `force'`, `forceBL` and `forceL`. Guards and
`with-nack` bodies run left to right, depth first, outside the atomic region; each `with-nack`
gets a fresh cvar, collected in a cell. The forcing runs inside `%force-group`:

```scheme
(define (%force-group force)
  (let ((nacks (list 'nacks)) (me %cur-tid) (done? #f))
    (define (leave!) ...)                                      ; unregister the cell from the tid
    (dynamic-wind
      void
      (τ (%tid-forcing-set! me (cons nacks (%tid-forcing me)))
         (let1 (g (force nacks)) (set! done? #t) (leave!) g))
      (τ (unless done? (when (or (eq? me %cur-tid) (%tid-dead? me)) (leave!) (%fire-nacks! nacks)))))))
```

If the forcing is left for good (a guard raises and the handler escapes, a continuation escape,
`cml/exit`, the death of the thread), the nacks made so far are set, so that the servers behind
them are not left waiting (ML never sets them). A switch out of the forcing (a guard that
blocks, a preemption) is not an exit: `%cur-tid` is then another thread when the after thunk
runs, and nothing fires. The cells are also kept on the tid, so a thread killed while switched
out of a forcing sets them when it is marked dead.

**Step 2: numbering.** `%sync-group` increments `%sync-count` (used by `barrier-wait-evt` to
detect two waits on one enrollment in one sync) and dispatches: a `base` group goes to
`%sync-on-bevts`, anything else to `%sync-on-grp`.

**Step 3: polling.** Inside the atomic region, every poll thunk is called in order. As soon as
one is enabled, the rest are still polled (ML: to bump their dynamic priorities) but block
results are discarded. In `%sync-on-grp` a poll that raises (a bad argument) sets every nack
of the sync, leaves the region and re-raises.

**Step 4: choosing.** `%select-do-fn` takes the maximum priority, with `-1` counting as *n*
(the number of enabled events) and a private `-2` below everything (used only by
`sync/timeout` with 0 seconds), and breaks ties with ML's wrapping counter:

```scheme
(define (%select-do-fn l n)
  (if (null? (cdr l))
    (cdar l)
    (let loop ((l l) (max-p -1) (k 0) (xs '()))
      (if (null? l)
        (if (and (pair? xs) (null? (cdr xs))) (car xs) (list-ref xs (%random k)))
        (let1 (p (let1 (p (caar l)) (cond ((= p -1) n) ((= p %last-resort-prio) -1) (else p))))
          (cond
            ((> p max-p) (loop (cdr l) p 1 (list (cdar l))))
            ((= p max-p) (loop (cdr l) max-p (add1 k) (cons (cdar l) xs)))
            (else (loop (cdr l) max-p k xs))))))))
```

The fixed priority of `always-evt` and timeouts counts as *n*, so a dynamic event that has lost
more polls than there are enabled competitors wins over them: ML's anti-starvation rule. The
counter is reset at every session, so choices are reproducible.

**Step 5a: committing on an enabled event.** Without nacks the chosen do-thunk is called. With
nacks, the chosen event's flag is set and `chk-cvars` is installed as the **commit hook**; the
do-thunk then commits and, when it leaves the atomic region (`%atomic-end` or a switch), the
hook sets every nack cvar none of whose flags is set. So the nacks are set after the commit and
before `sync` returns. (ML sets them before the do-thunk, so a nack server woken by it could
cancel the very partner the do-thunk dequeues; an earlier version of the port deferred them to
a temporary thread, which left them unset when `sync` returned.) `chk-cvars` skips cvars that
are already set, which can happen when a forcing that escaped is re-entered.

**Step 5b: blocking.** If nothing was enabled, the thread captures its continuation `k`, makes
one transaction `t` (remembered on the tid), and calls the block functions one inside the
`next` of the previous one, as ML's `log`; the last `next` dispatches. Each block function is
called through `%outcome`, which captures its outcome (values or condition) as a thunk, and
that thunk, composed with the branch's post, is thrown to `k`:

```scheme
(define (%outcome t thunk)
  (condition-case (receive vals (thunk) (τ (apply values vals)))
    (e () (%trans-cancel! t) (set! %atomic-state 'non-atomic) (τ (abort e)))))

;; in %sync-on-bevts, once every poll said blocked:
((%letcc/call k
   (let1 (t (%mk-id))
     (let1 (set-flg (τ (%trans-cancel! t)))
       (let log ((bs blocked))
         (if (null? bs)
           (%atomic-dispatch)
           (let1 (b (car bs))
             (k (let1 (th (%outcome t (τ ((%blocked-block b) t set-flg (τ (log (cdr bs)))))))
                  (τ ((%blocked-post b) th))))))))))))
```

When a partner commits to one of the registrations, that block function is resumed *inside*
the nesting of the previous ones, but it only finishes its own library work there (cleanup,
leaving the atomic region); its outcome escapes to `k`, and the wrappers run at the sync's own
continuation. Consequences: a server loop that recurses from a wrap function does not grow the
stack or the dynamic-wind chain; a `wrap-handler` never catches what another branch raised; a
continuable condition raised by a wrap function stays continuable. With nacks
(`%sync-on-grp`), each registration's cleanup cancels `t`, sets the branch's flag and runs
`chk-cvars`; while blocked, the flag sets are kept on the tid so that a thread that dies before
it resumes sets them ([4.3](#43-thread-death)).

**Waking.** Whoever wakes a blocked sync checks `(%trans-live? t)`, claims it
(`%get-id-from-trans!`, which reads the tid and cancels `t`) and only then enqueues or switches
to the thread. Every other registration of the same sync becomes stale and is dropped when its
queue is next cleaned.

**Queue cleaning.** ML cleans the whole queue at every enqueue (O(n²) for n waiters). The port
keeps a budget per queue (and per cvar, mailbox, timeout heap and port table): a full clean runs
only once the enqueues since the previous one reach the length that clean left (at least 8), so
an enqueue is O(1) amortized and stale entries never pile up. Dequeues skip stale entries at the
front (`%clean-and-remove!`), as in ML.

### 4.5 Channel rendezvous

A channel is `(%channel priority in-q out-q)`: `in-q` holds blocked receivers, `out-q` blocked
senders, as `(trans . k)` items.

- **`send`** with a live receiver in `in-q`: claim it, enqueue the sender (marked) and switch to
  the receiver's continuation with the message. The receiver's `recv` then leaves the atomic
  region (a tick).
- **`send`** with no receiver: enqueue `(trans . send-k)` on `out-q` and dispatch. A receiver
  that arrives switches to the sender with a **partner record** `(tid k claimed)`; the sender
  claims it and hands the message back with `%atomic-switch-to` (a tick).
- **`recv`** with a live sender: `%switch-to-sender!` claims it and switches to it with a
  partner record. If that sender dies as it is switched in, `%thread-died!` resumes the receiver
  with `%rendezvous-failed` and `recv` simply receives again.
- **`recv`** with no sender: enqueue on `in-q` and dispatch; resumed with the message, it
  leaves the atomic region (ML does not).

The events follow `channel.sml`: the poll calls `%clean-and-chk!`, which drops stale items and
returns 0 (blocked) or the channel's priority, bumped; a communication resets it to 1. The
enabled do-thunk dequeues the partner (safe: the poll just cleaned the front and nothing runs in
between). The blocked function registers `(trans . k)`, calls `next`, and when resumed runs
`cleanup` and completes the hand-off. `%send-evt/commit ch make-msg` computes the message at the
commit, while atomic; `send-evt` is `(%send-evt/commit ch (τ msg))`, and the port reader and the
channel output port use it to update their buffers as part of the commit itself.

`send-poll` and `recv-poll` commit only with a partner already waiting.

### 4.6 SyncVars

An ivar or mvar is `(%cell kind priority read-q value)`, `value` being a unique `%empty` marker
when empty.

- **Put** (`%cell-put!`): if the cell is full, raise `(exn cml put)`. Otherwise store the value;
  if a live reader waits, set the priority to 1 and switch to it with the value (recording the
  cell, for [4.3](#43-thread-death)); else leave the region.
- **Blocked readers** are enqueued on `read-q` with `%clean-and-enqueue!`.
- **Relay** (`iGet`, `mGet`, `mSwap`): a reader resumed with the value passes it on to the next
  live reader (`%relay-msg!`), each switch handing over to the next, until the queue is empty;
  the last one leaves the region. `mvar-take` instead empties the cell, which stops the chain.
  `mvar-swap` stores the new value and relays that.
- **Priorities**: a cell starts at 0; each enabled poll returns the counter and bumps it;
  `ivar-get-evt`'s do-thunk and a put's hand-off reset it to 1; `mvar-take-evt`, `mvar-get-evt`
  and `mvar-swap-evt` do not, as in `sync-var.sml`.

### 4.7 Mailboxes

A mailbox's state is `(empty . q)` with `q` a functional queue of waiting receivers, or
`(nonempty prio . q)` with `q` a non-empty functional queue of messages. There are never
messages and waiting receivers at the same time.

- `mailbox-send!` on an empty mailbox: hand the message to the first live receiver (switching to
  it) or become `(nonempty 1 . (x))`. On a non-empty mailbox: append and **yield**, as ML does,
  so a producer cannot outrun its consumers.
- `mailbox-recv` / the event: take the front message, or register as a waiter (the stale waiters
  are dropped with a budget, also by the blocking `mailbox-recv`, since round 9). The priority is
  bumped by each enabled poll and reset to 1 when a message is taken and others remain.
- `%mailbox-redeliver!` gives a message handed to a receiver that died while switched in to the
  next receiver, or puts it back at the front, so it is never lost nor delivered twice.

### 4.8 Barriers (the fixed algorithm)

```scheme
(define-record %barrier state update n-enrolled waiting distinct)
(define-record %enrollment barrier status entry polled)   ; status: enrolled | resigned
```

`waiting` holds entries `#(enrollment trans cleanup k)`, newest first; the entries of one sync
are adjacent (its block functions run one after the other, atomically). `distinct` counts the
distinct transactions in `waiting`, stale ones included: an upper bound on the live arrivals.

The poll of `barrier-wait-evt e`:

1. `e` resigned: enabled with a do-thunk that raises "barrier wait after resignation".
2. `e` already has a live entry, or was already polled by this sync (`polled` = `%sync-count`):
   enabled, raising "multiple barrier waits".
3. Otherwise record the sync number and ask `%barrier-last-arrival?`: the round is complete if
   the other enrolled threads all wait. The exact count (the distinct live transactions, an
   O(waiters) scan that also drops stale entries) is done only when the bound `distinct + 1`
   reaches `n-enrolled`, so a round of n threads costs O(n).
4. If complete: enabled (fixed priority) with a do-thunk that runs `%barrier-complete!` and
   returns the new state (or raises the update's condition).
5. Else blocked: the block function adds the entry, bumps `distinct` unless the previous entry
   has the same transaction, and dispatches; when resumed it returns the delivered result.

`%barrier-complete!` runs `update` on the state inside the atomic region (catching a condition),
empties `waiting`, and wakes every live waiter **oldest first**, each with `(ok . state)` or
`(exn . condition)`, claiming its transaction and running its cleanup; a waiter whose
transaction was already claimed by an earlier entry of its own sync is skipped. After a round
the enrollments are simply `enrolled` again (fix 1). `barrier-resign` marks the enrollment
`resigned`, decrements `n-enrolled` (fix 2) and completes the round if every remaining enrolled
thread waits; every path leaves the atomic region (fix 3). A barrier wait that loses a choice
leaves only a stale entry, which is not counted.

### 4.9 Timeouts

The pending timeouts are a binary min-heap in a vector, of entries
`#(time-ms cleanup trans k seq)`, ordered by deadline and, among equal deadlines, by
**decreasing** sequence number, which reproduces ML's LIFO ties. Stale entries (syncs that went
another way) are dropped when they reach the top, and by a full purge (rebuilding the heap)
once the insertions since the previous purge reach the number of entries it kept.

- `timeout-evt secs`: enabled at once for 0; otherwise the block function computes the deadline
  `now + ms` when the sync blocks, inserts the entry in O(log n) and dispatches.
- `at-time-evt t`: enabled if `t <= now`, otherwise blocks until `t`.
- `%poll-time!` pops the due live entries in order, enqueues each thread (marked) and runs its
  cleanup (which cancels the transaction and may set nacks); it stops at the first live entry
  that is not due, so a tick does not scan all sleepers.
- `%timeout-any-waiting` gives the milliseconds to the next deadline for the idle loop.

ML keeps a sorted list, inserts by a linear walk and cleans all of it at every poll, which is
harmless at a 20 ms SIGALRM but made every CML operation O(sleepers) at a per-operation tick
(review round 5: 40000 sleepers went from 233 s to 0.28 s of setup).

### 4.10 IOManager and ProcManager

**Descriptors.** `%io-waiting` is a list of `#(specs trans cleanup k)`, newest first. The poll of
`poll-evt*` runs a `file-select` with timeout 0 on its specs and is enabled with the ready ones;
otherwise its block function adds an entry. `%poll-io!` takes the live entries oldest first,
runs one `file-select` over all their specs, looks the ready descriptors up in a vector indexed
by descriptor (O(1) per spec), and wakes each entry with a ready subset, passing that subset to
its continuation. Errors of the select count as "nothing ready", as in ML.

**Processes.** `process-evt` creates a `Result`, adds `(pid . result)` to `%proc-waiting` and
memoizes the event per pid. `%poll-procs!` calls `process-wait pid #t` (non-blocking) on each;
a finished child's status is put in its result by a temporary thread (ML's
`enqueueTmpThread`), and its memo entry is dropped (the event keeps its value; a kernel-reused
pid is waited for afresh). An error, such as `ECHILD`, is put as the result's exception. Pids
still running when a session ends move to `%proc-orphans`, which is never reset: they are reaped
without being waited for by later sessions, as ML's ProcManager keeps doing.

**Extra pollers.** `%add-os-poller!` registers `(name poll waiting?)`. The one registered by the
module is the close-watch sweep of port waiters ([4.12](#412-port-input-events)).

### 4.11 Cleaners

`%cleanup-run when` takes the cleanup lock, selects the hooks registered for `when` (reversed,
i.e. oldest first, for the init times), drops the non-`at-exit` hooks for `at-init-fn`, gives
the lock back, and runs each cleaner as `(select (join-evt (spawn/call proc when)) (timeout-evt 1))`:
a cleaner that takes longer is not waited for. At init the standard `"Channels&Mailboxes"`
cleaner resets the logged channels and mailboxes and `"Servers"` runs the logged servers' init
thunks in registration order; at shutdown `"Servers"` spawns each shutdown thunk and waits at
most 2 seconds for it. The cleaners registered at load time make the user's thunk thread
`[000002]` of each session.

### 4.12 Port input events

Each input event follows the imperative `TextIO` idiom of `new-text-io-fn.sml` (a helper
thread per sync, a nack, a reply channel), with a per-port **driver**:

```scheme
(define-record %pdriver drain wait buffer chunks size eof lock run)
```

- `drain`: reads what is available **without blocking the process**, for a given predicate
  `done?`, returning `(values chars-in-reverse eof? more?)`; at most 4096 characters per call.
- `wait`: a thunk giving the event to wait on when the drain is not enough (readiness of the
  descriptor, a 5 ms timeout for a custom port, the channel of a channel port). Its value is a
  thunk that the reader runs where its errors are caught.
- the **side buffer**: characters read but not yet delivered, as a string plus a list of newer
  chunks (joined lazily, and not with `apply`, which hangs in CHICKEN 6 on some 32000
  arguments), `size` characters in all, and a sticky `eof` flag.
- `lock`: an mvar, full when free, re-created in each session.

**Finding the driver** is O(1): ports without a descriptor (channel, string and custom ports)
carry it in their data slot, which CHICKEN leaves as `#(#f)` or `#f` (it becomes
`#(#f driver)`); ports with a descriptor use a `%port-table`, a vector of buckets indexed by
descriptor holding weak pairs `(port . driver)`, so dropped ports are collected; custom ports
whose maker uses the data slot go in a linearly searched list purged with a budget.

**The event** is

```scheme
(guard (τ (when (port-closed? port) (error who "port is closed" port))
          (let1 (d (%port-driver port))
            (choose (%port-ready-evt d port take need)            ; the fast path
                    (with-nack (λ (nack)
                                 (let1 (reply (make-channel))
                                   (%spawn/queued! (τ (%port-reader d port take need nack reply)))
                                   (wrap (recv-evt reply) %result-value))))))))
```

Each event kind is a pair of procedures: `take` maps the side buffer and the eof flag to
`(list value new-buffer new-eof)` or `#f`, and `need` gives the `done?` predicate of a drain
given the buffer's size and last character (for example `input-line-evt` stops at a line
terminator, or after one more char when the buffer ends in `"\r"`, to tell `"\r\n"` from a lone
`"\r"`). `%pdriver-step!` drains while it reads something and `take` fails, trying `take` only
when `done?` was hit, at end of file, or on the first step, so input arriving in many small
pieces is neither copied nor scanned again at each piece. It raises if the port was closed.

**The fast path** is polled by the syncing thread. When the lock is free and the side buffer,
completed by a drain for the ports whose drain runs no user code nor CML operation (stdio, TCP
and string ports), satisfies `take`, it is enabled with priority 1 and its commit takes the
value from the buffer as it is then (a drain by another branch of the same sync may have added
to it). A drain that raises gives the condition as the event's value. Without it a poll
(`sync/timeout` with 0 seconds) missed input that was there whenever the helper had been
demoted to `rdyQ2` holding the lock (review round 9).

**The helper** (`%port-reader`, ML's `inputThread`) is queued rather than run first, and leaves
at once if its nack is already set. Otherwise it selects between taking the lock and the nack,
then loops: absorb what the wait delivered, step; on success **offer** the value on the reply
channel with `%send-evt/commit`, whose message thunk updates the side buffer **at the commit**,
atomically, together with the nack (either releases the lock); on a condition offer it the same
way; when the drain stopped at its bound (`'more`), check the nack and yield with a preemption
(`%yield/poll!`), so a stream that never pauses neither freezes the other threads and timeouts
nor keeps the helper reading after its sync went elsewhere; when more input is needed, select
between the wait event, a **close watch** (descriptor ports only) and the nack. Consequences: a branch that loses a
select, or a timeout in the middle of a line, loses nothing (the partial data stays in the side
buffer for the next event); escaping `run-cml` right after the commit leaves nothing half-done.

**Draining without blocking the process.**

- *stdio (FILE\*) ports*: CHICKEN's `char-ready?` looks only at the descriptor, not at stdio's
  buffer, and `read-char` may block or stop inside a UTF-8 sequence. So the descriptor is polled
  with a 0 timeout and, when readable, read once with `file-read` (4096 bytes; a readable pipe,
  tty or socket returns what it has). The file status flags are **never** changed: `O_NONBLOCK`
  belongs to the open file description, shared with stdout on the same tty and with children
  (review round 7). The bytes are decoded with the port's encoding: UTF-8 by `%utf8-stepper`,
  which keeps an incomplete sequence for the next read, so a sequence split across two writes is
  decoded whole; latin-1 and binary by `%bytes-stepper`, with CHICKEN's own decoder, as a direct
  read would. A port that was read directly before its first input event may have data in its
  stdio buffer: it is taken first, once, with `read-char` while the descriptor is temporarily
  replaced by an empty non-blocking pipe (`duplicate-fileno`), so stdio never reads the real
  descriptor; only a sequence that the direct read left split inside the stdio buffer comes out
  garbled.
- *TCP ports*: their `char-ready?` is exact per byte, so they are read byte by byte with
  `read-byte` while it holds and decoded by `%utf8-stepper` (a `read-char` on a lead byte whose
  continuation has not arrived would block the process).
- *channel and custom ports*: read with `read-char` while `char-ready?` holds. If a read raises
  after some chars were read (a bad value on a channel port), the chars go to the side buffer and
  the condition is raised by the next drain.

**Closed ports.** A reader waiting on a descriptor registers its port in a close-watch list. One
OS poller sweeps that list every 0.1 s, but only when some thread ran since the previous sweep
(only a thread can close a port), and puts an ivar for each port found closed, which wakes its
waiter; the idle loop sleeps at most until the next due sweep, and not at all for it once idle.
This replaced a 0.1 s timeout per waiter, which kept the process busy with a few thousand idle
readers (round 8: 2000 waiters went from 1653 ms to 14 ms of CPU over 2 s). A channel port is not
watched, so a reader blocked on a channel nobody writes to is a deadlock, as in ML.

### 4.13 Port output events

- `output-evt`: readiness of the descriptor for output (always enabled for ports without one).
- `write-string-evt` on a descriptor port commits on output readiness, then writes the string in
  chunks under the port's **write lock** (an mvar per port in a weak `%port-table`, taken with
  `%hold-take!`), each chunk after a new readiness wait except the first when the lock was free.
  Chunks are 128 characters on a blocking descriptor (a pipe or tty: a larger write could block
  the process) and 65536 on a non-blocking one (a TCP socket: small writes are held by Nagle's
  algorithm until the peer's delayed ACK, about 40 ms per message, review round 9).
- stdio and TCP ports are written **through their descriptor** (`file-write`, after flushing the
  port's own buffer), the string encoded with the port's encoding; a partial write waits for
  readiness again. CHICKEN's stdio flush drops write errors, and loading `(chicken tcp)` makes
  the process ignore SIGPIPE, so without this a write to a pipe whose reader exited was lost
  silently; now it raises `EPIPE`.
- A writer waiting between chunks is registered in the close watch too, and raises if the port
  is closed.
- Output events check at every sync (in a guard) that the port is open.

### 4.14 Channel ports

The input port is a CHICKEN custom port over a `%chan-in` state (the pending string, an index,
a sticky eof); `read-char` receives from the channel (blocking the CML thread) when the pending
string is used up, `char-ready?` uses `recv-poll`, and `peek-char` is provided so that CHICKEN's
own peek slot (invisible to `char-ready?`) is never used. Its driver's wait event is
`(recv-evt ch)`, whose value is pushed into the state inside the reader's error handler.

The output port buffers written strings in `%chan-out`. Every send (a flush, a
`write-string-evt`) is a `%chan-out-send-evt`: its message is the whole buffer, taken at the
commit, followed by its own string, so whichever of two sends commits first carries everything
written so far and a thread's output keeps its order. A send with nothing to carry commits at
once without sending; a blocked flush whose buffer another send took commits without sending
(`%chan-out-take!` wakes it). The port sends automatically once 1024 characters are buffered;
`close-output-port` flushes and sends `#!eof`.

### 4.15 OS processes

`%fork-exec` forks with `process-fork`; the child runs a setup thunk, resets SIGPIPE to its
default action (an ignored signal stays ignored across exec, and `producer | head -1` would
never end) and calls `process-execute`, exiting with the given status if the exec fails.

- `system-evt`: `("/bin/sh" "-c" cmd)`, status 127 on failure, forked when the event is made;
  the event is `(wrap (process-evt pid) status->code)`.
- `cml/execute`: two pipes; the parent's ends are made close-on-exec (`FD_CLOEXEC`, the literal
  1, since `(chicken file posix)` does not export it); in the child both ends are first moved
  above 2 with `fcntl F_DUPFD` (the parent may have closed its stdin or stdout, so `create-pipe`
  could have returned 0 or 1), the originals closed, then copied onto 0 and 1. The parent returns
  `open-input-file*` / `open-output-file*` ports on its ends and the pid.

### 4.16 Multicast

The stream of a multicast channel is a chain of ivars, each eventually holding
`(v . next-ivar)`; the channel records the empty tail. `multicast!` puts the tail and advances
it, which is atomic because the scheduler switches only at CML operations; inside `run-cml` it
then promotes one thread from `rdyQ2` and yields. ML's `multicast` is a rendezvous with a server
thread, which throttles a producer to the pace of the tees and receivers; without that
throttling a producer looping on `multicast!` fell behind for good and the unread chain grew
without bound (review round 9: a lag of 66158 messages became 0).

A port is a channel fed by a **tee** thread that follows the chain from the port's starting ivar,
plus an mvar holding the ivar of its next unread message; `multicast-recv` receives from that
channel and advances the mvar. `multicast-copy-port` starts a new tee at that mvar's ivar.

### 4.17 SimpleRPC

A request is `(arg . result)` sent on a mailbox; `call` waits on the result and re-raises a stored
condition. The entry event is `(wrap (mailbox-recv-evt mb) serve)`. `serve` runs `f` in the
server thread under a handler; for the state makers it checks that `f` returned two values
(else `(exn cml rpc)`), then calls an optional `commit!` with the outcome **before** putting the
answer, because the put may switch to the caller or be a preemption and let another server thread
serve the next request (review round 6: `make-rpc/state` lost updates with several servers).

### 4.18 TraceCML

TraceCML has no server threads: modules, flags, destinations, watches and the handler registry
are plain updates, atomic because nothing switches in between. A module is
`(%trace-module full-name label tracing children)`. A watch is stored on the tid itself as
`(run . ivar)`; `watch` spawns a thread selecting between that ivar (put by `unwatch`) and
`(join-evt tid)`, whose wrap removes the watch and traces the warning. `trace-exn-handler` runs
in the dying thread: it tries the handlers newest first under a handler that turns any raise
into "not handled", and calls the default when none handled it.

### 4.19 Complexity notes

These bounds came out of the performance findings of the reviews; each is pinned by a ratio test
in the suites.

| Operation | ML | `(aux cml)` | Round |
|---|---|---|---|
| Enqueue a waiter on a channel, SyncVar, cvar (`join-evt`, nacks), mailbox | O(queue) (clean at every enqueue) | O(1) amortized (budgeted clean) | 4, 9 |
| n blocked select loops over one variable | leaks (losing waiters kept until the next put) | constant space | 3 |
| Block n sleepers; one tick with n pending timeouts | O(n²); O(n) per poll | O(n log n); O(1) + due entries | 5 |
| One barrier round of n threads | O(n) | O(n) (was O(n³) in an early version) | 4 |
| n threads dying of uncaught conditions | n/a | O(n) | 7 |
| Port driver lookup, n open ports | n/a | O(1) (was O(n)) | 5, 6 |
| Input arriving in k small pieces | n/a | O(k) (side buffer as chunks, `take` tried on a hit) | 4 |
| Tick with d idle descriptor waiters | n/a | O(1) unless 2 ms (or 10× the last poll) have passed | 9 |
| d idle port readers while idle | n/a | one sweep, only after some thread ran | 8 |
| Watching n threads | O(n) per operation | O(1) | 5 |
| A server loop recursing from a wrap in a blocked select | grows (nested `log` frames) | constant | 3 |
| A loop recursing from a wrap under `wrap-handler` | nests one handler per iteration | same, plus O(depth) per switch (documented) | 4 |

---

## 5. Deviations from ML

All are deliberate; most are also listed in the header comments of `src/aux.cml.scm`.

### 5.1 Core

| ML behaviour | `(aux cml)` behaviour | Reason |
|---|---|---|
| Preemption by SIGALRM every 20 ms; a computing thread is always preempted | A clock tick per CML operation (every `%atomic-end`, rendezvous hand-off, yield, read-only poll); preemption every `quantum` ticks (64); a thread that never calls CML is never preempted | Continuations cannot be captured safely in a CHICKEN signal handler |
| `rdyQ2` promotes a thread only when the preempted thread was marked | One thread promoted from `rdyQ2` at every preemption (and at every `multicast!`) | Deterministic ticks can phase-lock with a loop and starve `rdyQ2` |
| `yield` and channel hand-offs are not ticks (the timer is) | Both count as ticks, and a quantum expiring there is a preemption | Otherwise a yield loop or a channel ping-pong starved timeouts, I/O, processes and `rdyQ2` forever |
| Clock cached per quantum | Clock read on demand | No timer to invalidate a cache |
| Idle: wait for SIGALRM / `pause` | Sleep in `file-select` until the next deadline or ready descriptor; poll every 5 ms while children (or custom ports) are awaited | No signals; no SIGCHLD handler |
| Descriptors and children polled at every SIGALRM | At a tick, only once 2 ms (or 10× the previous poll's duration) have passed; always when idle | A tick is far more frequent than 20 ms; idle waiters must not slow other threads |
| `recv` returns inside the atomic region | `recv` leaves the region when resumed | ML bug |
| `tidToString` of the dummy tid gives `[0000~1]` | `[-000001]` | Readability |
| `sync` wrappers composed into block functions and run nested in the other branches' block functions (`log`) | Wrappers kept apart (`post`) and applied at the sync's own continuation | Server loops recursing from a wrap grew without bound; a `wrapHandler` caught other branches' conditions (ML bug); continuable conditions stayed continuable |
| Nacks of an enabled commit set before the chosen doFn runs | Set right after the commit (commit hook), before `sync` returns | A nack could cancel the very partner the doFn dequeues |
| A sync abandoned while forcing sets no nack | Nacks made so far are set when a guard/with-nack body raises and the handler escapes, escapes with a continuation, exits its thread, or its thread dies while switched out; also when a poll raises | Servers behind them (e.g. a port reader holding the port lock) waited forever |
| A thread dying while blocked in a sync with nacks sets nothing | All unset nacks of that sync are set (the chosen branch's included) | Same |
| Waiters left in SyncVar read queues and in the cvars of `joinEvt` and nacks by syncs that went another way stay until the next put | Dropped as new waiters are added | A select loop over an empty variable leaked |
| Queues cleaned in full at every enqueue | Full clean only after as many enqueues as the queue held after the previous clean | O(n²) blocking of n threads |
| Sorted list of timeouts, cleaned at every poll | Binary heap with lazy deletion and budgeted purge; ties still LIFO | O(n) per tick with n sleepers |
| A queue entry of a previous `RunCML.doit` can be resumed | Transactions record their session; entries of dead sessions, of failed calls outside `run-cml` and of dead threads are stale | No dead continuation is ever resumed |
| Blocked mailbox receivers cleaned lazily at send | Filtered with a budget when a receiver is added (by `mailbox-recv` too) | Stale waiters of earlier sessions leaked |
| The core's default exception handler ignores the exception (TraceCML's prints it) | The core default prints the condition with `print-error-message` and the tid; once loaded, TraceCML's handler is installed | Silent thread deaths are hard to debug |
| A thread whose dynamic-wind thunk raises during a switch: n/a (no dynamic-wind) | Dies once; its handler runs after the interrupted switch is queued to go on; a partner, SyncVar value or Mailbox message it was handed goes to the next taker; a dead thread is never resumed | `call/cc` runs winders at every switch |
| Children still running at the end of a run are polled by later runs | Same (orphans reaped without being waited for); a reaped child's pid leaves the `process-evt` memo | Pid reuse, memory |
| n/a | `sync/timeout` (0 seconds is a poll) | Convenience |
| `withNack`'s cvar set twice raises `Fail` | Never raised: `chk-cvars` skips cvars already set | Re-entered forcing continuations |

### 5.2 SyncVar, Mailbox, Barrier, CleanUp

| ML behaviour | `(aux cml)` behaviour | Reason |
|---|---|---|
| Barrier status never reset after a round | Back to `enrolled` after every round | ML bug |
| `resign` never decrements `nEnrolled` nor completes a round | Decrements and completes the round if the rest wait | ML bug (deadlock) |
| `resign` of a resigned enrollment leaves the atomic region open | Every path leaves it | ML bug |
| Barriers have no event | `barrier-wait-evt`; a losing wait is not counted; several enrollments in one sync count once; two waits on one enrollment in one sync raise | Composability |
| Waiters woken newest first | Oldest first | Arrival order; not specified by `BARRIER` |
| `Barrier.barrier update init` | `(make-barrier init update)` | Initial state first, as `make-mvar`'s initial value; listed among the header's and README's deviations |
| ivar and mvar are different types | One record, every operation checks the kind (an `(exn)` error) | Scheme is untyped |
| Puts outside `RunCML` | SyncVar puts and polls work outside and wake nobody; blocking operations raise `(exn cml not-running)` | No scheduler outside |
| Cleaner registration with bad arguments | Checked at registration (non-procedure, bad `whens`, wrong-typed logged object) | A bad entry broke every later `run-cml` |
| Trace files closed by TraceCML's server | `"TraceCML"` cleaner registered before `"Servers"`, so it runs after them at shutdown | Servers may trace while shutting down |

### 5.3 IO / OS layer

| ML behaviour | `(aux cml)` behaviour | Reason |
|---|---|---|
| Functional streams, `PRIM_IO` readers, a lock per stream | Port events over CHICKEN ports with a per-port driver, side buffer and lock | No functional stream layer |
| `inputNEvt`, `input1Evt`, `inputLineEvt`, `inputAllEvt`, `inputEvt` | `input-string-evt`, `input-char-evt`, `input-line-evt`, `input-all-evt`, `input-evt`, plus `peek-char-evt` | |
| Lines end at `"\n"` | `"\n"`, `"\r\n"` or a lone `"\r"`, without the terminator (as `read-line`) | CHICKEN convention |
| A losing branch may lose input taken at guard time | Nothing is lost: the side buffer is updated at the commit | Documented guarantee |
| Output streams have no events | `output-evt`, `write-string-evt` (chunked, one write lock per port, `EPIPE` raised) | Useful; `TextIO.output` holds the stream lock too |
| `OS.Process.systemEvt` | `system-evt` forks at once through `/bin/sh`; value is an integer status (128 + n for signal n) | Scheme convention |
| `Unix.execute`: no `PATH` search, `argv[0]` = basename | `PATH` searched (unless an environment is given), `argv[0]` = the command as given; failure status 128 as in ML | `process-execute` cannot set `argv[0]` separately and the module has no FFI |
| Children inherit the parent's signal state | SIGPIPE reset to default in children | This process ignores SIGPIPE (loading `(chicken tcp)` does) |
| `acceptEvt` may accept in the guard | `tcp-accept-evt` accepts after the commit and re-checks readiness | ML bug; a second acceptor would block the process |
| `connectEvt` waits for writability, ignores `SO_ERROR` | `tcp-connect-evt` connects after the commit, blocking the process | `(chicken tcp)` has no non-blocking connect |
| ChanIO reader never signals end of stream | `#!eof` on the channel ends the input port for good; a non-string value is an error | Scheme ports need an end |
| ChanIO writer | Buffer taken at the commit of each send; never sends an empty string; `close` sends `#!eof` | Ordering; ML never sends empty vectors |
| `pollEvt` on one descriptor | `poll-evt` on any number of `(fd mode)` specs | Generality |

### 5.4 cml-lib

| ML behaviour | `(aux cml)` behaviour | Reason |
|---|---|---|
| Multicast: a server thread per channel; `multicast` is a rendezvous with it | No server; `multicast!` appends to the ivar chain and yields (promoting from `rdyQ2`); ports keep the tee thread | Simpler; the yield keeps the producer at the readers' pace as ML's rendezvous does |
| SimpleRPC: if the function raises, the caller blocks forever | The caller gets the condition; the server goes on (except `make-rpc/out`, whose entry event re-raises it, as ML does) | ML bug |
| n/a | `make-rpc/state` (state kept inside, safe with several servers) | Convenience |
| TraceCML: trace, watcher and exception servers; the "carefully" protocol | Plain atomic updates, no servers | ML's watcher deadlocks after the first watched death and races on watch/unwatch |
| A module's full name is its parent's followed by its label, with no separator (`"/ThreadWatcher"`, and a child `c` of it would be `"/ThreadWatcherc"`) | Each label is followed by `/`: `"/ThreadWatcher/"`, `"/ThreadWatcher/c/"` | ML's documentation uses `/` separators; doc/code mismatch |
| Trace thunk runs in the trace server | Runs in the tracing thread | No server |
| A trace file stays in use after `tracerStop` closes it | Closed at every shutdown, reopened for appending | ML kept writing to the closed stream |
| Uncaught exceptions forwarded to a server that spawns a handler thread | Dispatched in the dying thread, before its join event fires | No server |
| Death message without a space before the tid | With it | ML bug |

---

## 6. Review log

### 6.1 How the port was built and reviewed

The port was written in four phases (core; cml-lib; IO/OS layer; integration into the egg, the
Makefile and the README), each delivering its own test suite. It then went through **nine review
rounds**. Each round had the same structure:

1. **Review**, by independent reviewers, one per *lens*:
   - *semantics*: fidelity to the ML code and documentation, API semantics, the option,
     time and condition conventions;
   - *concurrency*: the scheduler, atomic regions, races, thread death, nacks, fairness;
   - *io-lib*: the IO/OS layer and cml-lib;
   - *style-tests*: the repository's idioms, the accuracy of comments and README, and the
     strength of the tests (mutants that survive);
   - *programs*: black-box programs and benchmarks written against the public API only (servers,
     pipelines, select loops, thousands of threads, waiters or ports). This lens was added from
     round 3; rounds 1 and 2 had the first four.
2. **Verification**: every finding was handed to **two skeptic verifiers**, each told to try to
   refute it by reading the code and reproducing the scenario. A finding confirmed by at least one
   verifier was kept; the six findings rejected by both were dropped (a test cleanup already
   handled, a nack-timing claim then re-reported and confirmed in round 5, the `cml/execute`
   differences then re-reported and confirmed in round 6, an interpreter-only continuation leak,
   dead code, and a wording ambiguity about Mailbox puts).
3. **Fix**: every confirmed finding was fixed (or, in a few cases, documented), each with a new
   or strengthened regression test. The fixer then checked that each new test **fails** against
   the pre-fix source (or that a mutant the old tests missed now fails), and ran all three suites
   three (in round 9 four) consecutive times, both interpreted and compiled with `csc`, with zero
   failures.

137 findings were raised; 131 were confirmed; after merging the duplicates (the same defect
reported by two lenses) there were **123 distinct defects**, all fixed or documented.

| Round | Distinct confirmed | High | Medium | Low |
|---|---|---|---|---|
| 1 | 21 | 3 | 7 | 11 |
| 2 | 15 | 2 | 6 | 7 |
| 3 | 19 | 4 | 8 | 7 |
| 4 | 14 | 0 | 5 | 9 |
| 5 | 10 | 0 | 4 | 6 |
| 6 | 13 | 1 | 2 | 10 |
| 7 | 9 | 0 | 2 | 7 |
| 8 | 10 | 0 | 2 | 8 |
| 9 | 12 | 0 | 4 | 8 |
| **Total** | **123** | **10** | **40** | **73** |

The trend: high-severity findings stopped after round 6 (the last one was the channel ping-pong
that never ticked), and later rounds found mostly low-severity edge cases (threads dying inside
`dynamic-wind` thunks during a switch, argument checking, documentation accuracy) and
performance cliffs at scale. No round came back empty: even round 9 confirmed 12 defects, four
of them medium.

In the tables below the lens is abbreviated: **sem** semantics, **conc** concurrency, **io**
io-lib, **st** style-tests, **prog** programs.

### 6.2 Round 1

| Sev. | Lens | Finding | Fix |
|---|---|---|---|
| high | sem | `barrier-wait-evt` counts one enrollment twice in a single select, completing the round early and crashing | Count distinct live transactions; number every sync and reject a second wait on one enrollment in the same sync ("multiple barrier waits") |
| high | conc, io | `cml/yield` never ticks, so a yield loop keeps timers, I/O and process waits from ever being polled | `%atomic-yield` counts a tick and goes through the scheduler hook when the quantum expires |
| high | io | `cml/execute` leaves the parent's pipe ends inheritable, so later children keep earlier children's stdin open (no EOF, hang) | Set `FD_CLOEXEC` on the parent's ends |
| medium | sem | Outside `run-cml`, `send`/`recv` can resume the continuation of an earlier failed call instead of raising not-running | `%trans-live?` also requires `%running` and a live thread |
| medium | conc | A `wrap-handler` on one select branch catches exceptions raised by another branch's wrap (nested block-fn extent; an ML bug) | Each resumed block-fn hands its outcome to the sync's continuation as a thunk (`%outcome`) |
| medium | conc | Barrier completion wakes the same transaction twice when one select waits on two enrollments | Re-check `%trans-live?` before claiming each waiter |
| medium | io | `write-string-evt` on a channel output port drops the sent prefix after the commit, racing with flushes | New `%send-evt/commit`: the buffer is taken at the commit |
| medium | io | `make-rpc/state` reads the state when the entry event is forced, so two server threads lose updates | Rebuilt on the mailbox; state read and written when a request is served |
| medium | st | No test covers the guarantee that a losing select branch keeps its port input | Test `losing-branch-keeps-input` (kills the "commit before offer" mutant) |
| medium | st | `tcp-accept-evt`'s "a losing branch never consumes a connection" is untested | Test `accept-evt/losing-branch` |
| low | conc | A dynamic-wind after thunk that raises during a switch kills the thread, which is later resumed; `run-cml` aborts with "cvar already set" | A thread dies once; dispatch skips dead threads; every switch goes through `%switch!` and `%thread-died!` resumes the interrupted switch |
| low | conc | Escaping `run-cml` with a continuation leaves `%running` set, so every later `run-cml` raises | The session runs inside a `dynamic-wind` whose after thunk resets |
| low | conc | Blocking primitives outside `run-cml` register a live waiter before raising not-running | `%check-running` at the entry of every blocking primitive |
| low | io | Children still running when `run-cml` returns are never reaped | `%proc-orphans`, reaped non-blockingly by later sessions |
| low | io | Channel input port: a char taken by a direct `peek-char` is invisible to `char-ready?` and to input events | Pass a `peek-char:` procedure to `make-input-port` |
| low | io | `input-line-evt` keeps the `#\return` of CRLF lines | Lines end at `"\n"`, `"\r\n"` or a lone `"\r"` |
| low | io | `make-rpc` with an `f` returning zero values kills the server and blocks the caller | Answer `(void)`; state makers check for two values (`(exn cml rpc)`) |
| low | st | `test/logged-channel` passes even when `cml/log-channel!` does nothing | Assert the queues and priority are reset, against an unlogged control channel |
| low | st | Public names with no test (`choose*`, `cml/unlog-all!`, `poll-evt*`, predicates) | Assertions added |
| low | st | Global registrations by tests are undone only on success, so one failure cascades | Undo them in `dynamic-wind` after thunks |
| low | st | Header says internal hooks are `%`-marked, but `atomic-begin` & co. are exported unprefixed | Renamed `%atomic-begin`, `%atomic-end`, `%atomic-dispatch`, `%dispatch`; header corrected |

### 6.3 Round 2

| Sev. | Lens | Finding | Fix |
|---|---|---|---|
| high | conc | A nack set in `chkCVars` before the chosen channel do-thunk runs makes it dequeue a cancelled partner: current tid `#f`, a continuation resumed twice, the scheduler loops | Nacks set after the commit (first deferred to a temporary thread; since round 5 the commit hook) |
| high | io | `tcp-accept-evt`'s ready branch accepts without re-checking; a preempted second acceptor calls `tcp-accept` on an empty backlog and freezes the process | The ready branch re-checks `tcp-accept-ready?` like the I/O branch |
| medium | conc | `%outcome` turns continuable conditions raised in a wrap into non-continuable ones (only for blocked multi-branch syncs) | Run the branch under the sync's handler (superseded in round 3 by applying wrappers at the sync's continuation) |
| medium | conc | User `dynamic-wind` thunks run at every switch as a different thread, so CML operations in them break mutual exclusion | Documented in header and README (switching with `continuation-graft` would leak `parameterize`); test pins the behaviour |
| medium | io | A sync aborted while forcing leaves an input event's helper holding the port lock forever | `%force-group`: nacks made so far are set when forcing is abandoned |
| medium | io | A bad value on a channel input port kills the helper while it holds the lock | The wait event returns a thunk run inside the reader's handler; the error is offered to the syncing thread |
| medium | io | `cml/execute` gives the child no stdin/stdout when the parent closed fd 0 or 1 | Child moves the ends above 2 with `F_DUPFD` before `dup2` |
| medium | st | `test/thread-flag` checks a literal `#t`, not the child's own flag | Child records its own flag before and after its own set |
| low | conc | Escaping `run-cml` right after an input event commits leaves the side buffer uncommitted | The reader offers with `%send-evt/commit`, updating the buffer at the commit |
| low | io | `input-line-evt` on a stdio pipe corrupts a UTF-8 char split across two writes | Read bytes from the descriptor and decode UTF-8 in the driver, keeping an incomplete tail |
| low | io | `peek-char-evt` (and a lone `"\r"`) leave data in the hidden side buffer, so a direct read loses chars | Header corrected: keep using input events on a port; line drains stop at `"\r"` |
| low | io | Every port used with input events stays in `%pdrivers` until closed; registration is linear | Weak registries (later O(1) tables, rounds 5–6) |
| low | io | `system-evt` runs the command through `$SHELL`, not `/bin/sh` | `/bin/sh -c` |
| low | st | Two header lines at column 0 inside the module body | Re-indented (plus typo and divider fixes) |
| low | st | No test for `sync/timeout`'s default or `'all` cleaner registration | Tests added |

### 6.4 Round 3

| Sev. | Lens | Finding | Fix |
|---|---|---|---|
| high | conc | Every blocking multi-branch select adds nested frames, so server loops slow down quadratically and leak | Wrappers kept apart from block functions (`%blocked-post`) and applied at the sync's continuation |
| high | io | TCP input events block the whole process when a UTF-8 sequence is split across segments | `%socket-drainer`: byte-level reads while `char-ready?`, UTF-8 decoded in the driver |
| high | prog | A wrap function on a blocked sync runs inside a dynamic-wind frame; recursive server loops grow without bound | Same fix as the first item (5.3 s → 0.86 s on the reviewer's loop) |
| high | prog | The yield tick never promotes from `rdyQ2`: a yield busy-wait or a fast mailbox producer starves demoted threads | An expiring yield tick unmarks the thread and promotes one from `rdyQ2` |
| medium | sem, conc | `ivar-get-poll` and `mvar-get-poll` are not ticks, so a busy-wait on them hangs the run | `%cell-poll` counts a tick (and still works outside `run-cml`) |
| medium | conc | A thread that died during `recv`'s hand-off switch is resumed by the sender | `%atomic-switch-to` never resumes a dead thread |
| medium | conc | A preemption inside the handler of a forcing sync fires its nacks early | Preemption switches to the dummy thread through `%switch!`, so it is not taken for an escape |
| medium | conc | A sync abandoned because a poll raised never sets its nacks | `%sync-on-grp` sets every nack before re-raising |
| medium | io | Input events on descriptor-less ports leak memory while they wait (nack waiter re-registered every 5 ms) | Stale cvar waiters dropped as new ones are added |
| medium | st | `test/priorities/prio-0` passes even with a wrong priority | Assert the exact list (kills the fixed-priority mutant) |
| medium | prog | Deterministic ticks phase-lock with a loop, so spawned threads pile up in `rdyQ2` | Promote one thread from `rdyQ2` at every preemption |
| medium | prog | Losing ivar/mvar events and `join-evt`/nack waits are never cleaned: select loops leak | `%clean-and-enqueue!` for read queues; cvar waiters filtered; closures built where old state is out of scope (interpreter) |
| low | conc | Nacks are never set when a guard or with-nack body leaves the sync by a continuation or `cml/exit` | `%force-group` is a `dynamic-wind` whose after thunk sets pending nacks on a real exit |
| low | io | `write-string-evt` on a closed channel output port still sends after `#!eof` | Output events check at every sync that the port is open |
| low | io | UTF-8 split garbled on a pipe port read directly before its first input event | Take the stdio buffer once with the descriptor swapped for an empty pipe |
| low | io | `input-string-evt` accepts non-exact counts | Exact non-negative integer check (also `poll-evt` specs) |
| low | st | README says `run-cml` returns `'success`; a returning thunk gives `'failure` | README corrected |
| low | st | README says errors are `(exn cml ...)`, but argument errors are plain `(exn)` | README and header distinguish the two |
| low | st | Header says OS pollers run at every scheduling point | Corrected: at preemption ticks and while idle |

### 6.5 Round 4

| Sev. | Lens | Finding | Fix |
|---|---|---|---|
| medium | io | Concurrent `write-string-evt`s on one descriptor port interleave their strings | A write lock per port, held from the first chunk to the last |
| medium | io | Input events on a closed port read whatever descriptor now has the old number | Raise at sync; the helper re-checks before every drain |
| medium | st | No test covers skipping dead threads in `%atomic-dispatch` | Tests with a dead thread's entry left in a ready queue (yield and preemption variants) |
| medium | prog | Blocking on an ivar, mvar, `join-evt` or nack costs O(waiters): N waiters O(N²) (introduced by the round-3 fix) | Budgeted full clean: O(1) amortized (20000 waiters: 53.7 s → 91 ms) |
| medium | prog | Barrier arrival counting O(n²) per poll, O(n³) per round | `distinct` upper bound; exact scan only when a round may be complete (2000 threads: 9.5 s → 44 ms) |
| low | sem | `sync/timeout` with 0 seconds usually returns the default even when the event is ready | A private lowest priority (-2) for the fallback: a real poll |
| low | sem | Trace file closed before the logged servers shut down | `"TraceCML"` cleaner registered before `"Servers"` |
| low | conc | A sender that dies while switched in by `recv` strands the receiver | Partner records: the receiver is woken with `%rendezvous-failed` and receives again |
| low | io | Channel output port reorders a thread's own output when another thread flushes | Flushes also take the buffer at the commit |
| low | io, prog | `process-evt`'s pid memo never pruned; a reused pid gets a stale status | Memo entry dropped when the child is reaped |
| low | st | `channel=?` and `mvar=?` tested one way only | Both ways |
| low | st | `test/trace-file` writes a fixed-name file in the current directory | `create-temporary-file` and cleanup in `dynamic-wind` |
| low | prog | The port side buffer is re-copied at every drain: O(n²) `input-all-evt` | Side buffer as a list of chunks; `take` tried only on a hit (40000-char line in small pieces: 148 s → 1.3 s) |
| low | prog | Recursing from a wrap under `wrap-handler` is quadratic (a frame replayed per switch) | Documented, with the advice to recurse outside the handler |

### 6.6 Round 5

| Sev. | Lens | Finding | Fix |
|---|---|---|---|
| medium | io | A port input event reading a fast stream never yields: everything else freezes, memory grows | Drains bounded to 4096 chars; the helper checks its nack and yields with a preemption (`%yield/poll!`) |
| medium | st | `test/concurrent-write-string-evts-do-not-interleave` is timing-marginal | Read exactly two lines with 30 s timeouts |
| medium | prog | Pending timeouts slow every operation in proportion to their number; blocking N sleepers O(N²) | Binary heap with lazy deletion (40000 sleepers: 233 s → 0.28 s) |
| medium | prog | Every channel or descriptor port registration is O(live ports): an 8000-stage channel-port pipeline takes 19 s | O(1) state lookup (data slot, per-descriptor weak buckets); pipeline 1.25 s |
| low | conc | A thread killed by an after thunk while blocked inside a guard never sets that sync's nacks | Forcing cells kept on the tid, set by `%mark-dead!` |
| low | conc | An ivar/mvar reader that dies as it is switched in strands the other readers | Hand-offs record the cell; `%cell-wake-next!` relays to the next reader |
| low | conc | Nacks of a sync committing on an enabled event are still unset when `sync` returns | The commit hook sets them when the do-thunk leaves the atomic region |
| low | io | `write-string-evt` on a pipe whose reader exited reports success and drops the data | Write through the descriptor; `EPIPE` raised (UTF-8 ports; all encodings in round 8) |
| low | st | README and header lines joined by fix edits, past the wrap width | Re-wrapped |
| low | prog | TraceCML `watch`/`unwatch` and watched deaths are O(watched threads) | Watch stored on the tid (16000 watches: 30 s → 0.34 s) |

### 6.7 Round 6

| Sev. | Lens | Finding | Fix |
|---|---|---|---|
| high | prog | Two threads passing messages over channels never reach a clock tick: sleepers, timeouts, I/O, processes and `rdyQ2` wait forever | `%atomic-switch-to` counts a tick and becomes a preemption when the quantum expires |
| medium | io | `make-rpc/state` loses updates when several server threads share the entry event | `commit!` stores the new state before the answer is put |
| medium | io | Children of `system-evt`, `cml/system`, `cml/execute` inherit SIGPIPE ignored: pipelines hang | `%fork-exec` resets SIGPIPE to default before exec |
| low | sem | `cml/execute` differs from `Unix.execute` (status 127, `argv[0]`, `PATH`) undocumented | Status 128 as ML; `PATH`/`argv[0]` kept and documented |
| low | conc | `%cell-wake-next!` hands the next mvar reader a stale value, which can be delivered twice | The relay thread reads the cell when it runs |
| low | conc | A thread that dies as it is switched in by a commit never sets its sync's nacks | Tid keeps its transaction and flag sets; `%abandon-blocked!` sets them |
| low | conc | A `default-exn-handler` that calls `cml/exit` (or blocks) in `%thread-died!` strands the switch target | Wake-ups done and the interrupted switch queued before the handler runs |
| low | conc | A mailbox message handed to a receiver that dies as it is switched in is lost | `%box-switch!` records the mailbox; `%mailbox-redeliver!` |
| low | io | A reader blocked on a port closed by another thread hangs when the fd number is reused | Waiting readers and writers check for a close every 0.1 s (reworked in round 8) |
| low | io | Input events on string and custom ports cost O(live ports) | State kept in the port's data slot |
| low | io | `trace` hangs when its thunk returns ~35000 items (CHICKEN `apply` limit) | Concatenate through a string port |
| low | st | `tid<?`, `tid-compare`'s -1 and `trace-module?` tested one way | Negative cases added |
| low | st | README claims each session starts from a fresh state, but cleaners, handlers, logged objects and trace flags persist | README lists what is global |

### 6.8 Round 7

| Sev. | Lens | Finding | Fix |
|---|---|---|---|
| medium | conc, prog | A thread that crashes *after* receiving a mailbox message hands it to another receiver too (duplicate delivery; one bad job kills every worker) | The switch state is cleared by `%letcc/call` once the target is resumed, so only a real switch-in death redelivers |
| medium | io | The stdio drain sets `O_NONBLOCK` on the shared open file description: children and other holders get `EAGAIN` | Poll the descriptor with a 0 timeout and read once when readable; flags untouched |
| low | sem | ML's `TextIO.inputEvt` has no counterpart, undocumented | `input-evt` added |
| low | sem | `Unix.executeInEnv` not ported, undocumented | Optional `env` argument of `cml/execute` |
| low | io | Input events on stdio ports ignore the port's encoding | UTF-8 decoder only for UTF-8 ports; others decoded with CHICKEN's decoder |
| low | io | A bad value on a channel input port discards the chars received before it | Chars kept in the side buffer; condition raised by the next drain |
| low | st | `test/cml.scm` defines an unused helper `every?` | Removed |
| low | st | README says the idle scheduler sleeps until a child exits; it polls every 5 ms | README corrected |
| low | prog | Each uncaught-exception death scans both ready queues: N deaths O(N²) | Scan only for a thread that died during a switch |

### 6.9 Round 8

| Sev. | Lens | Finding | Fix |
|---|---|---|---|
| medium | io | Idle port readers poll every 0.1 s: ~2000 blocked input events keep the process near 80% CPU | One close-watch sweep as an OS poller, only after some thread ran (2000 waiters: 1653 → 14 ms CPU per 2 s) |
| medium | st | `test/input-evt` writes 10000 bytes into a pipe only its own process reads: hangs on small pipe buffers | The pre-fill is written by a CML thread with `write-string-evt` |
| low | sem | Channel output port sends empty strings, which ML's writer never does | `%chan-out-send-evt` never sends an empty string |
| low | sem | Barrier wakes waiters oldest first, ML newest first; not listed as a deviation | Kept and documented |
| low | conc | A re-entered escaped forcing sets an already-set nack cvar and aborts `run-cml` | `chk-cvars` skips cvars already set |
| low | conc | A `write-string-evt` writer that dies while switched in keeps the port's write lock | Internal locks recorded on the tid (`holds`) and given back at death |
| low | io | `EPIPE` fix covers UTF-8 ports only: latin-1/binary writes to a dead pipe still vanish | Every stdio port written through its descriptor, encoded with its encoding |
| low | st | README says `cml/execute` searches `PATH`, but not with the optional environment | README qualified |
| low | st | README says bad arguments raise in the caller, but `spawn` of a non-procedure does not | `spawn`/`spawn/call` check `procedure?` |
| low | prog | A thread waiting on an input event on a channel input port prevents deadlock detection | Channel and custom ports are not close-watched |

### 6.10 Round 9

| Sev. | Lens | Finding | Fix |
|---|---|---|---|
| medium | sem | A non-procedure cleaner makes every later `run-cml` raise before its thunk runs | `cml/add-cleaner!` checks `proc` and `whens` at registration |
| medium | io | Every preemption tick polls every pending descriptor and child: idle waiters slow all CML work | Tick polling rate-limited (2 ms or 10× the last poll); ready fds looked up in a vector (4000 waiters: 6553 → 292 ms) |
| medium | prog | `write-string-evt` on a TCP port sends 128-char chunks: ~40 ms stall per message (Nagle + delayed ACK) | Sockets written through the descriptor in 65536-char chunks (20 round trips: 1684 → 55 ms) |
| medium | prog | Blocking `recv`/`send`/`mailbox-recv` never drop stale waiters of earlier sessions | `%clean-and-enqueue!` and `%mailbox-add-waiter!` |
| low | sem | Logging a non-channel stops every later logged channel and mailbox from being reset | Type checks in `cml/log-*!` |
| low | sem | ivar and mvar operations accept each other's cells | `%check-ivar` / `%check-mvar` in every operation |
| low | conc | A thread that dies preempted inside `%protect` keeps the cleanup lock | `%protect` uses `%hold-take!`/`%hold-put!` |
| low | conc | Polling a port input event misses buffered data for up to a quantum when the helper holding the lock is demoted | Fast path polled by the syncing thread; helper queued, not run first (misses 1203 → 0) |
| low | io | Bad arguments to `system-evt`, `cml/system`, `cml/execute`, `process-evt` come back as exit statuses or a false `ECHILD` | Argument checks in the parent, before forking |
| low | st | A README line breaks the 80-column wrap | Reflowed |
| low | st | IO-layer comment lines outdented to column 0 with a broken reflow | Re-indented and reflowed |
| low | prog | `multicast!` never yields: a producer outruns its subscribers without bound | `multicast!` promotes from `rdyQ2` and yields (lag 66158 → 0) |

---

## 7. Testing

### 7.1 Running the suites

**With the egg installed** (`chicken-install` in `src/`, or `make install`):

```sh
cd src && make test-cml
```

This runs `csi -s cml.scm`, `csi -s cml-lib.scm` and `csi -s cml-io.scm` in `src/test` (the
plain `make test` target runs them too, after the other suites).

**Without installing the egg.** The suites import `(aux base)`, `(aux unittest)` and
`(aux cml)`. Compiled `aux.base` and `aux.unittest` (with their import libraries) must be
available in `src/` (for example built with `csc -s -J aux.base.scm` and likewise for
`aux.unittest` and `aux.sxml`, which `aux.unittest` uses); `aux.continuation` and `aux.cml` can
simply be loaded from source. Write a small driver per suite in `src/test`, e.g. `run-cml.scm`:

```scheme
(import (chicken base) (chicken file))
(load "../aux.continuation.scm")
(load "../aux.cml.scm")
(load "cml.scm")          ; or cml-lib.scm, cml-io.scm
```

and run it with `src/` prepended to the repository path:

```sh
cd src/test
CHICKEN_REPOSITORY_PATH="$PWD/..:$(chicken-install -repository)" csi -s run-cml.scm
```

Alternatively compile the module itself (`csc -s -J aux.cml.scm`, and likewise
`aux.continuation.scm`) into a scratch directory, put that directory first in
`CHICKEN_REPOSITORY_PATH` and run `csi -s cml.scm` directly. Every review round ran the suites
both ways; keep build products out of the repository.

**Reading the results.** Each suite prints `((ran N) (failed M ...))` and writes a
`testsuite-<name>.html` report in the current directory (ignored by `.gitignore` in `src/test`).
The exit status is always 0, so read the summaries. On the development machine the three files
take about 19 s, 8 s and 26 s interpreted (run in parallel); `cml-io.scm` needs `/bin/sh`,
`cat`, `tr`, `yes` and a loopback TCP port.

### 7.2 Conventions of the suites

Every case runs one or more complete `run-cml` sessions and asserts **outside** them: an assertion
failing inside a CML thread would only reach that thread's exception handler, so threads record
what they observe (`note!`) and the assertions look at the log once `run-cml` returns. The
scheduler is deterministic (ticks are counted, the tie-break counter is reset per session), so
most cases are exact. A few scaling and timing cases use ratio tests or generous margins (a
10× larger input must stay under a bound on the time ratio). Cases that could hang on a
regression run under a watchdog (a timeout in the session, or a forked child killed after 10 s).
Global registrations made by a case are undone in a `dynamic-wind` after thunk.

### 7.3 Inventory

211 cases in 16 suites; all pass (`failed 0`), interpreted and compiled.

| File | Suite | Cases | What it covers |
|---|---|---|---|
| `cml.scm` | `cml-events-suite` | 36 | version, run status, not-running, bad `run-cml` arguments, escaping `run-cml`, internal names, rendezvous both ways and as events, polls and their ticks, `choose` flattening, `select/case`, guards per sync, `wrap-handler` nesting and scoping, `with-nack` (losers only, blocked, nested, forcing raising/escaping/re-entered, killed forcing thread, set when `sync` returns, preempted handler, raising poll), continuable conditions, wrap loops that do not nest, priorities (prio 0, reset, tie-break) |
| | `cml-threads-suite` | 27 | tids, independent runs, child first, non-procedure spawn, `join-evt` (all deaths, FIFO), exception handlers, every `dynamic-wind` death case of [4.3](#43-thread-death), mailbox redelivery only for switch-in deaths, yield, thread properties and flags, fairness (producers, preemption, yield promotion, no phase-lock, channel ping-pong ticks) |
| | `cml-time-suite` | 10 | timeout order, duration, zero and past deadlines, LIFO ties, `sync/timeout` (and 0 as a poll), a yield loop cannot starve timeouts, many sleepers (ratio), losing timeouts dropped, cancelled waits not woken |
| | `cml-syncvar-suite` | 10 | ivar and mvar semantics, predicates, kind checks, condition predicates, relay chains, losing waiters dropped, many waiters in linear time, many deaths in linear time, `Result` |
| | `cml-mailbox-suite` | 3 | ordering, blocking receive, predicates |
| | `cml-barrier-suite` | 9 | rounds (and in linear time), wake order, resignation completing a round, raising update, the ML documentation example, predicates, one sync counted once, a wait event losing a choice |
| | `cml-cleanup-suite` | 9 | cleaners and their times, logged channels (against a control), `cml/unlog-all!`, stale entries across runs, stale waiters of blocking operations, bad registrations, the registration lock given back by a dying thread, logged servers, `cml/debug` |
| | `cml-os-suite` | 7 | `io-evt` on a pipe, `poll-evt`, a yield loop cannot starve I/O, idle waiters do not slow others (timing), `process-evt` (and bad pids), a pending child keeps the run alive |
| | `cml-examples-suite` | 2 | a prime sieve of threads and channels, a server with `with-nack` |
| `cml-lib.scm` | `cml-multicast-suite` | 8 | predicates, readers at independent speeds, new ports see later messages, copy ports, `multicast-recv-evt`, a port shared by readers, many messages, a producer kept to its readers' pace |
| | `cml-rpc-suite` | 11 | the five makers, FIFO answers, several values, select loops, exceptions delivered to the caller (and with state), shared state across servers at several quanta |
| | `cml-trace-suite` | 12 | the module tree and flag hierarchy, output destinations (and long lists), trace files (closed after the servers), `watch`/`unwatch` (death, exception, after death, many threads by ratio), the uncaught-exception registry |
| `cml-io.scm` | `cml-io-ports-suite` | 33 | line reading with a slow writer, data already in stdio buffers, char/peek/string/all events, end of file, selecting over two pipes, losing branches keep input, line terminators, readers sharing a port, ports across sessions, timeouts vs input, read errors, writer and reader over a full pipe, `output-evt`, commit surviving an escape, forcing that raises releases the port, `input-evt`, file status flags untouched, encodings, split UTF-8 (writes, after a direct read), no leak while waiting, raising polls, non-interleaving writers, closed ports (and reused descriptors), linear time with small pieces, a flooding stream, `EPIPE`, a dying writer releases the lock, idle waiters cost nothing, bad arguments, polls find available input, not-running |
| | `cml-io-os-suite` | 13 | `system-evt` exit codes, eagerness, concurrency and `/bin/sh`; bad arguments; `cml/execute` (cat, close-on-exec, closed stdio, missing program, `PATH` and `argv[0]`, environment); reaping after a run; children start with SIGPIPE default |
| | `cml-io-tcp-suite` | 6 | an echo server, messages sent at once (no Nagle stall, timing), accept with timeout, a losing accept branch, two acceptors and one connection, UTF-8 split across segments |
| | `cml-io-chanio-suite` | 15 | round trip, a blocked channel-port reader is a deadlock, events on channel ports, `write-string-evt` as a rendezvous and with flushes, no empty strings, order of a thread's output, closed output ports, peek then events, bad values (reaching the syncing thread, keeping earlier chars), dropped ports collected, ports without a descriptor carry their state, many ports in linear time, a pipe-to-channel bridge |

---

## 8. Known limitations and open items

**Scheduling**

- Preemption is cooperative: a thread that computes without calling any CML operation is never
  preempted, and it delays timeouts, I/O and every other thread for as long as it computes.
  Long computations should call `cml/yield` now and then.
- Blocking system calls block the whole process: `tcp-connect-evt`'s handshake (no
  non-blocking connect in `(chicken tcp)`), direct reads and writes on ports outside the port
  events, `process-wait` without `#t`, a DNS lookup, and so on.
- Child processes and ports without a descriptor are polled every 5 ms while awaited (there is
  no SIGCHLD handler), so an idle run waiting only on a child wakes about 200 times a second.
- `quantum` is counted in CML operations, not time.

**Dynamic context**

- `dynamic-wind` before/after thunks run at every switch into and out of their thread, on behalf
  of the other thread; they must not use CML operations or `(current-tid)`, must not raise, and
  cannot protect a critical section. If a before thunk raises and the thread itself catches the
  condition, whatever switched to it expecting an answer (a receiver, say) waits for good.
- A loop that recurses from a wrap function under a `wrap-handler` nests one handler frame per
  iteration and pays O(depth) at every thread switch.

**Ports**

- Once input events are used on a port, direct reads on it may miss characters that sit in the
  side buffer or were drained from the descriptor (a line cut by a timeout, the char of a
  `peek-char-evt`, the char after a lone `"\r"`). Keep reading such a port with input events.
- On a stdio port read directly before its first input event, a UTF-8 sequence that the direct
  read left split inside the stdio buffer comes out garbled.
- A reader blocked on a channel input port that nobody writes to is a deadlock (`run-cml`
  returns `'failure`), as in ML; channel ports are not swept for closes.
- Errors of the descriptor `select` count as "nothing ready". A descriptor closed under a
  waiter (`EBADF`) may therefore keep the other I/O waiters from being woken until that waiter
  is released; closes by other threads are caught by the 0.1 s sweep, but the scenario was
  flagged by a fixer in round 4 and never reproduced or tested.
- Only TCP (no UDP or Unix-domain sockets); no binary I/O events; no stream positions.

**Processes**

- `cml/execute` searches `PATH` and passes the command as given as `argv[0]` (ML does neither),
  because `process-execute` offers no separate `argv[0]` and the module uses no FFI.
- Children still running when a session ends are reaped by later sessions but their status is
  never reported.
- A `process-evt` on a numeric pid that was already reaped raises `ECHILD` when synced on (as in
  ML).

**Library**

- The cleaner times `at-exit` and `at-init-fn` are accepted but never triggered (there is no
  `exportFn`).
- A cleaner that runs longer than 1 second (a server shutdown longer than 2 seconds) is not
  waited for, but its thread keeps running until the session ends.
- `make-rpc/out`'s entry event re-raises the server function's condition in the server thread
  (as ML does), so a server loop that does not handle it dies, although the caller gets the
  condition too.
- `multicast-copy-port` is exact only when the copied port has a single reader.
- Trace output to a channel or a mailbox is dropped outside `run-cml`.

**Process**

- The review rounds never came back empty: round 9 still confirmed twelve defects. The rate and
  severity went down (no high-severity finding after round 6), but the scheduler's handling of
  thread deaths inside `dynamic-wind` thunks, the port layer and the performance at scale are
  where the last rounds kept finding problems, and where further review is most likely to pay.
- The timing and ratio tests (idle waiters, TCP round trips, many sleepers, many ports) use
  generous margins but can still be sensitive on a heavily loaded machine.
- Upstream has no CML test suite to compare against; all tests were written for this port.
