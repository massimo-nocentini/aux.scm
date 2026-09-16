# `(aux anthropic)` — Implementation Plan

*A CHICKEN Scheme module for talking to Claude, for `massimo-nocentini/aux.scm`.*

## Review status

This plan was produced by a multi-agent pass: five parallel surveys of the actual sources, four
sub-area designs, twelve adversarial verifications, and three critiques (grounding /
completeness / test coverage). Claims were checked against the working tree at `70106dd`, against
`csi 6.0.1pre1`, and — for the wire format — against Anthropic's own documentation. The encoder,
the JSON validator, the flonum printer and the SSE accumulator were transcribed and **executed**
on `csi`, so the expected strings in §6 are recorded output rather than predictions.

Nine defects the critics confirmed have already been folded in. They are listed here because each
one is a trap worth knowing about, not just a diff:

| # | Defect | Fix, as applied |
|---|---|---|
| 1 | `anthropic-response/blocks` called with two arguments in one case | `(anthropic-blocks/type "text" (anthropic-response/blocks r))` |
| 2 | `hash-table-ref/default` unbound in the offline suite | `srfi-69` added to its imports |
| 3 | `parameterize` unbound in the live suite | `(scheme base)` added to its imports |
| 4 | The no-key case read the developer's **real** environment, so it went red on any box with `ANTHROPIC_API_KEY` exported | the environment read is now the stubbable parameter `anthropic/env-api-key` |
| 5 | A `pause_turn` resume produces two adjacent assistant turns, which `anthropic-messages/validate!` rejected | only consecutive *user* turns now raise; the assistant side is exempt, with a positive test |
| 6 | A vector of tools was silently dropped from the request body | normalized with `vector->list` before the `pair?` test |
| 7 | §3.6's `anthropic-send` signature omitted `betas` | corrected to match §4.10 |
| 8 | §3.8's description of `anthropic-tool-result->string` contradicted §4.14 | table now states the `""` / `(void)` substitutions |
| 9 | Ten behaviours of the API had no test at all — including the `around` hook's documented foot-gun and the streaming/non-streaming `--max-time` arms, where swapping the ternary passed every case | §6.6 adds ten cases, eight fixtures and one tool |

A second multi-agent pass rechecked the plan against `csi 6.0.1pre1` and against the published
image's layers. Its findings are folded in too: §6.6 rewritten against bindings that exist (it
named six that do not, and an unbound identifier exits `csi` with **rc=70**, which fails
`make test -B` and publishes no report for any of the seventeen suites); the `read-string`
contract in §2; the kill-before-wait step in §4.8's close thunk; `(scheme base)` in
`import-for-syntax`; `(scheme file)`, `(chicken port)` and `(chicken bytevector)` in §6.4's
imports; idempotence moved into `anthropic-wire/close!` itself; the `command` field renamed
`origin` and the curl vocabulary lifted behind `anthropic-transport/*`.

Two details of the kill-before-wait step were then corrected against a running `csi` rather than
against the manual, because the obvious spelling of both is wrong. The accessor is `process-id`;
there is no `process-pid`, so that name is an unbound variable and takes the suite down with rc=70.
And `(process-wait p #t)` answers `(values 0 #f #f)` for a child that is still running, **not**
`#f`, so the guard has to be `(eqv? 0 pid)` — `(unless pid …)` never fires, 0 being truthy in
Scheme, and the stall the step exists to prevent comes straight back. Both measured here; the
corrected close returns in 21 ms wall clock against a child that would otherwise have held on for
thirty seconds.

That pass also **withdrew** a claim this table used to carry, which is why row 10 is gone:
`process-sleep` is not a CHICKEN 4 name. `modules.db` has `(process-sleep value chicken.process)`
and `csi` confirms it is live. The "fix" would have broken the backoff, because `(chicken base)`'s
`sleep` rejects flonums. `process-sleep` stands, with the one-second granularity documented in §9.

What is **not** yet verified: the curl port directions and `--include` behaviour were confirmed on
macOS with `curl 8.7.1`, and §8 step 24 asks for a `linux/arm64` confirmation that **cannot be
satisfied as written**. `.github/workflows/docker.publish.yml` has no `setup-qemu-action` and
`Dockerfile:2` is `FROM --platform=$BUILDPLATFORM`, so both matrix legs build on the amd64 runner:
the published `amd64` and `arm64` manifests of `aux.scm:master` carry byte-identical layer lists
and identical rootfs diff_ids. The `arm64` tag is the amd64 image relabelled. That is a
**pre-existing repo bug**, unrelated to this plan, and it deserves its own issue — either add
`docker/setup-qemu-action` and drop `--platform=$BUILDPLATFORM`, or remove `linux/arm64` from the
workflow so the tag stops lying. Until then no arm64 verification of anything is possible from CI.

---


## 0. Preface: disagreements I resolved by reading the files

The adversarial verdicts contradict each other in four places. I checked each against the working tree at `70106dd` and against the local `csi 6.0.1pre1`. Decisions:

| Disagreement | What the files say | What I took |
|---|---|---|
| Does `(aux base)` export `o`? | `grep -n "define.*\bo\b" src/aux.base.scm` → only `english-alphabet/lowercase`. `o` is **not** in `(aux base)`. | `o` comes from `(chicken base)`, which is in my import list. The grounded survey's claim was wrong; I do not rely on `(aux base)` for it. |
| Does `(λ (m) …)` in module source crash the SXML report? | `src/aux.sxml.scm:180-184` marks `code/scheme`, `code/scheme/expand` and `code/scheme/file` `*preorder*`, added in HEAD commit `70106dd`. The published image `:master` predates it — that is why three verifiers reproduced `(car) bad argument type: ()`. | **The hazard is fixed at HEAD**, and `make test` builds from HEAD source, so I impose no naming ban on module source. I note it in the plan only as a reason not to trust a stale image. |
| `simdjson-parse/ondemand` byte-vs-char bug | `src/aux.simdjson.scm:93` is literally `(simdjson-parse-ondemand-callback str (string-length str))`; locally `(string-length "caffè")` → 5 for 6 bytes. `src/chicken-simdjson.cpp` has zero `catch`. | Confirmed. `(aux anthropic)` never calls `simdjson-parse/ondemand`; it drives the raw callback with its own UTF-8 byte count, behind a **complete** pure-Scheme JSON validator. |
| Does `->string/json` reject bad keys? | `src/aux.simdjson.scm` object branch **does** `(error "JSON object keys must be strings")`. The api-surface verdict's D2 was about the *proposed* replacement, not about `->string/json`. | My encoder keeps that check and upgrades it to a typed `anthropic-encode-error`. |
| Transport signature (three competing shapes) | None exist in the repo; all three were invented. | I take the **wire-record** shape (`(transport url headers body stream?) -> anthropic-wire`) because it is the only one of the three that can serve scope item (c) — the other two buffer the body by construction, which verdict 1 on api-surface called out as making SSE unimplementable. |
| `x-should-retry` / `request-id` headers | Two verifiers observed a live 401 with **neither** header. | I read both defensively with a documented fallback, and I state plainly that neither was observed. No behaviour depends on their presence. |

Everything else I verified by probe on `csi 6.0.1pre1`: `#!key` survives macro emission as a datum (`(symbol? (car '(#!key)))` → `#t`); `(apply condition (list '(exn …) '(anthropic-error …) '(anthropic-api-error …)))` yields a condition caught by all three kinds with cross-kind `get-condition-property`; `flonum-print-precision` round-trips `3.141592653589793` at 17 and restores cleanly; `string-translate*`, `substring-index` (prefix-safe at index 0), `string-downcase` (`(scheme char)` only), `make-parameter`/`open-input-string` (`(scheme base)` only), `string->keyword` (`(chicken keyword)`), `pseudo-random-integer` (`(chicken random)`), `file-creation-mode` setter (`(chicken file posix)`).

---

## 1. Goal and the shape of the module

`(aux anthropic)` lets a CHICKEN 6 program talk to the Anthropic **Messages** API — `POST https://api.anthropic.com/v1/messages` — as ordinary Scheme data. It ships four capabilities: a blocking `messages` call, an agentic tool-use loop that dispatches `tool_use` blocks to Scheme procedures and feeds `tool_result` blocks back until the turn ends, an SSE streaming call that reassembles the event stream into exactly the same decoded shape the blocking call returns, and a `define-tool` macro that derives a JSON `input_schema` from a Scheme lambda list. HTTPS is done by shelling out to the `curl` binary through `(chicken process)`, because the container in which `make test` runs has neither the `http-client` egg nor `openssl`; that decision is hidden behind the parameter `anthropic/transport`, whose contract is four positional arguments in and one `anthropic-wire` record out, so a native or FFI backend can replace it without any call site changing and so the whole offline test suite runs against a stub that replays canned bytes. Everything on the wire is the `(aux simdjson)` decoded shape — a JSON object is a list of two-element `(symbol value)` lists, an array is a **vector**, `null` is `(void)` — which is what lets an assistant turn be appended back to the conversation verbatim, a hard requirement since `thinking` blocks are rejected if modified. Two JSON procedures live in this module rather than in `(aux simdjson)`: a parser, because `simdjson-parse/ondemand` passes a character count where the C side wants a byte count and so aborts the process on the first accented response, and an encoder, because `->string/json` escapes strings with Scheme `write`, emits `\x01;` for control characters, truncates flonums to 15 significant digits and writes ratnums verbatim — none of which is JSON. Both are guarded by a complete pure-Scheme JSON validator, because `src/chicken-simdjson.cpp` contains no `try`/`catch` and a malformed body reaches `std::terminate`, i.e. SIGABRT, which no Scheme handler can catch.

---

## 2. Layered architecture

Six layers, one file. Each layer depends only on the ones above it.

```
                    ┌───────────────────────────────────────────────────────┐
  entry points      │  anthropic/ask   anthropic/messages                    │
                    │  anthropic/stream           anthropic/converse         │
                    └───────┬───────────────────────┬───────────────┬────────┘
                            │                       │               │
            ┌───────────────┴──────────┐   ┌────────┴────────┐   ┌──┴──────────────┐
  loop /    │  request building        │   │  SSE decoding   │   │  tools          │
  tools /   │  anthropic-request/body  │   │  anthropic-sse/ │   │  define-tool    │
  streaming │  anthropic-messages/     │   │    read → §     │   │  anthropic-tool │
            │    validate!             │   │  accumulator    │   │  registry       │
            └───────────────┬──────────┘   └────────┬────────┘   └──┬──────────────┘
                            │                       │               │
                    ┌───────┴───────────────────────┴───────────────┴────────┐
  messages          │  anthropic-message/*  anthropic-block/*                 │
  (decoded data)    │  anthropic-response record + accessors                  │
                    └───────────────────────────┬────────────────────────────┘
                                                │
                    ┌───────────────────────────┴────────────────────────────┐
  send + retry      │  anthropic-send   anthropic-retry/delay                 │
                    │  anthropic-status/retryable?  anthropic-wire->body!     │
                    └───────────────────────────┬────────────────────────────┘
                                                │
                    ┌───────────────────────────┴────────────────────────────┐
  transport         │  parameter anthropic/transport                          │
  (swappable)       │    (transport url headers body stream?) -> anthropic-wire│
                    │  anthropic-transport/curl    anthropic-stub/transport   │
                    └───────────────────────────┬────────────────────────────┘
                                                │
                    ┌───────────────────────────┴────────────────────────────┐
  JSON + conditions │  anthropic-json/well-formed?  /parse  /write  /ref …    │
  (foundation)      │  anthropic-condition  anthropic-raise/*  predicates     │
                    └─────────────────────────────────────────────────────────┘
                                                │
                                  (aux base)  (aux simdjson)  (chicken …)
```

The **transport contract** in full:

```
(transport url headers body stream?) -> anthropic-wire

  url      string, the POST target
  headers  ((name value) ...), lowercase names; the "x-api-key" entry is SECRET —
           a backend must not put it anywhere a third party can read
  body     the already-encoded JSON request body, a string
  stream?  #t when the caller intends to read the body incrementally

  anthropic-wire
    status   exact integer HTTP status (a backend raises rather than returning #f)
    headers  ((lowercase-name value) ...)
    origin   a datum identifying what produced this wire, for error reporting
    port     an input port positioned at the first byte of the body.  It MUST answer
             read-string correctly: a custom port built with make-input-port MUST be given
             a read-bytevector: hook, because csi 6.0.1pre1's read-char fallback for
             read-string returns garbage -- {"ok":true} reads back as "}" plus ten NUL bytes,
             and read-string! reports a false success count on top of it.
             anthropic-port->string IS read-string, and every blocking body goes through it
             (§4.10), so a backend that gets this wrong corrupts silently rather than loudly.
    close    thunk -> (values exit-status diagnostics); releases everything.  A backend does
             NOT implement idempotence: anthropic-wire/close! memoises the thunk's values
             (§4.8), so every backend gets it for free and none may rely on being called once.
```

Three curl decisions worth stating because each looks arbitrary until you see what it buys:

* **`--include`.** curl writes the status line and response headers to stdout *ahead of* the body in one strictly ordered channel. We read that block off the port and hand the same port — now at the first body byte — to either the slurper or the SSE decoder. No `-D` file, no `-w` trailer a hostile body could forge, and the blocking and streaming paths are byte-identical up to that point.
* **No `--fail-with-body`.** With it, curl exits 22 for every HTTP error and a 429 becomes indistinguishable from a 404. Without it, exit 0 means "an HTTP response was obtained, whatever its status" and non-zero means a network or TLS failure. That is exactly the split the retry policy needs: retry on status, give up on exit code.
* **`--header @FILE` plus `--data-binary @-`.** Neither the key nor the prompt is ever an argv element, so neither appears in `ps`. The header file is created under a temporarily tightened `file-creation-mode` of `#o077` — `create-temporary-file` otherwise yields 0644 and chmod-after-create leaves a race.

---

## 3. Public API

The module is `(module (aux anthropic) * …)`, so everything defined in the body is exported. This table is the contract §6 tests against.

### 3.1 Parameters

| Binding | Default | Purpose |
|---|---|---|
| `anthropic/api-key` | `#f` | Explicit key override; `#f` defers to `anthropic/env-api-key`, read at **call** time, never frozen at load. |
| `anthropic/env-api-key` | thunk reading `ANTHROPIC_API_KEY` | The environment read, as a stubbable thunk — this is what makes the no-key test hermetic on a developer box that has a key exported. |
| `anthropic/base-url` | `"https://api.anthropic.com/v1/messages"` | POST target; repointed by tests at a loopback replay server. |
| `anthropic/model` | `"claude-opus-5"` | Default model id. Complete as-is — never append a date suffix. |
| `anthropic/version` | `"2023-06-01"` | Value of the mandatory `anthropic-version` header. |
| `anthropic/betas` | `'()` | Beta-id strings; when empty the `anthropic-beta` header is **omitted**, not sent empty. |
| `anthropic/max-tokens` | `16000` | Default `max_tokens` for a blocking call. |
| `anthropic/stream-max-tokens` | `64000` | Default `max_tokens` for a streaming call. |
| `anthropic/connect-timeout` | `10` | Connect timeout in seconds; the process backend passes `--connect-timeout`, a libcurl backend would pass `CURLOPT_CONNECTTIMEOUT`. |
| `anthropic/max-time` | `600` | Whole-transfer deadline for a blocking call, seconds; the process backend passes `--max-time`. |
| `anthropic/stream-max-time` | `1800` | The same deadline for a streaming call. It is also the worst case for abandoning a stream, which is why §4.8's close thunk kills the child before waiting. |
| `anthropic/retries` | `3` | Extra attempts after the first; 3 means at most 4 round trips. |
| `anthropic/backoff` | `1.0` | First backoff window in seconds, doubled per attempt. |
| `anthropic/backoff-cap` | `30.0` | Ceiling on the backoff window **and** on `retry-after`. |
| `anthropic/backoff-jitter` | `#t` | `#f` makes the schedule deterministic (1.0, 2.0, 4.0 …) for tests. |
| `anthropic/curl` | `"curl"` | Program name handed to `process*`; PATH is searched (no environment alist is passed). |
| `anthropic/sleep` | `(λ (s) (process-sleep (max 1 (inexact->exact (ceiling s)))))` | The retry sleeper. Tests parameterize it to a recorder. |
| `anthropic/transport` | `anthropic-transport/curl` | The one swap point. |

### 3.2 Configuration, conditions

| Binding | Signature | Purpose |
|---|---|---|
| `anthropic-api-key/current` | `() -> string \| #f` | Parameter, else the environment; never raises. |
| `anthropic-api-key/effective` | `() -> string` | As above, but raises `anthropic-config-error` when absent or empty. |
| `anthropic-request/headers` | `(#!key (betas (anthropic/betas))) -> ((name value) ...)` | `content-type`, `x-api-key`, `anthropic-version`, and `anthropic-beta` only when betas are set. |
| `anthropic-condition` | `(kind retryable message properties) -> condition` | Compound of kinds `(exn anthropic-error KIND)`. |
| `anthropic-raise/config` | `(message detail)` | Signals `anthropic-config-error`. Properties: `detail`. |
| `anthropic-raise/encode` | `(message value)` | Signals `anthropic-encode-error`. Properties: `value`. |
| `anthropic-raise/decode` | `(message body)` | Signals `anthropic-decode-error`. Properties: `body`. |
| `anthropic-raise/transport` | `(message origin exit-status diagnostics)` | Signals `anthropic-transport-error`. Properties: `origin`, `exit-status`, `diagnostics`. Retryable exactly when `anthropic-transport/transient?` says so — the conditions layer names no backend. |
| `anthropic-raise/api` | `(status headers body)` | Signals `anthropic-api-error`. Properties: `status`, `type`, `api-message`, `request-id`, `headers`, `body`. |
| `anthropic-raise/sse` | `(message raw)` | Signals `anthropic-sse-error`. Properties: `raw`. |
| `anthropic-raise/tool` | `(message detail)` | Signals `anthropic-tool-error`. Properties: `detail`. |
| `anthropic-raise/loop` | `(reason message response)` | Signals `anthropic-loop-error`. Properties: `reason`, `response`. `response` is an `anthropic-response` at every call site but one: `max-iterations` fires *before* a request is made, so it passes `#f`. A handler must test the property, never assume a record. |
| `anthropic-error?` | `(c) -> boolean` | Family predicate over everything this module signals. |
| `anthropic-api-error?` `anthropic-transport-error?` `anthropic-decode-error?` `anthropic-encode-error?` `anthropic-config-error?` `anthropic-sse-error?` `anthropic-tool-error?` `anthropic-loop-error?` | `(c) -> boolean` | Per-kind predicates. |
| `anthropic-error-ref` | `(c property #!optional (default #f))` | Reads a property off whichever specific kind the condition carries. |
| `anthropic-error-retryable?` | `(c) -> boolean` | The one question the retry loop asks. |
| `anthropic/redact` | `(text) -> string` | Replaces the current API key with `"<redacted>"` in any diagnostic text. |

### 3.3 JSON layer

| Binding | Signature | Purpose |
|---|---|---|
| `anthropic-json/utf8-length` | `(s) -> integer` | UTF-8 byte length, pure Scheme. Not because the FFI is out of reach — `anthropic-json/parse` drives the `simdjson-parse-ondemand-callback` `foreign-lambda` from the same `csi -s` suite — but because that callback wants a **byte** count and `string-length` answers in characters. |
| `anthropic-json/well-formed?` | `(s) -> boolean` | Complete JSON validator; `#t` only for a well-formed top-level **object or array** with no trailing garbage. Nothing reaches simdjson without it. |
| `anthropic-json/parse` | `(s) -> decoded` | Validate, then drive the raw simdjson callback with the byte length. Raises `anthropic-decode-error`; never aborts. |
| `anthropic-json/write` | `(v) -> string` | Strict encoder. Raises `anthropic-encode-error` on anything JSON cannot represent. |
| `anthropic-json/emit` | `(v port)` | The encoder's worker, exposed for composition. |
| `anthropic-json/escape` | `(s port)` | JSON string escaper: `\b \t \n \f \r \" \\`, `\uXXXX` for the rest of C0, raw UTF-8 otherwise. |
| `anthropic-json/flonum->string` | `(x) -> string` | Shortest repr that round-trips (`flonum-print-precision` 15→17), restoring the parameter. |
| `anthropic-json/ref` | `(obj key #!optional (default (void))) -> value` | `assq`-then-`cadr`. Tolerates a non-pair `obj`. |
| `anthropic-json/has?` | `(obj key) -> boolean` | Distinguishes an absent key from a key whose value is decoded `null`. |
| `anthropic-json/null?` | `(v) -> boolean` | `(void?)`, i.e. decoded JSON `null` — **not** `'()`, which is an empty object. |
| `anthropic-json/set` | `(obj key val) -> obj` | Replaces a key in place, appending when absent. Preserves key order. |
| `anthropic-json/merge` | `(obj patch) -> obj` | `anthropic-json/set` for every entry of `patch`; last wins, no duplicate keys. |
| `anthropic-json/vector->list` | `(v) -> list` | A decoded array as a list; `'()` for anything else. `match/first` has no variable-length vector pattern. |

### 3.4 Strings, headers, status

| Binding | Signature | Purpose |
|---|---|---|
| `anthropic-string/trim` | `(s) -> string` | Trims leading/trailing whitespace (including `\r`). |
| `anthropic-port->string` | `(port) -> string` | `(read-string #f port)` answers the **eof object**, not `""`, on an exhausted port; this normalizes it. |
| `anthropic-header/check!` | `(name value)` | Rejects CR/LF in a header name or value — header-injection guard. Called from `anthropic-request/headers`, so **every** transport inherits it, and again from `anthropic-curl/headers->file!`, which is public and takes a header list from anywhere. |
| `anthropic-url/check!` | `(url) -> url` | Rejects anything that is not an `http://` or `https://` URL, and any CR/LF. curl has no `--` end-of-options marker, so an argv element beginning with a dash is an **option** wherever it sits: a base URL of `-K/path` makes curl read an arbitrary config file. Called at the top of `anthropic-transport/curl` — ahead of the header file, so a rejection cannot leave one behind — and again in `anthropic-curl/argv`. |
| `anthropic-header/parse` | `(line) -> (name value) \| #f` | Splits on the **first** colon; the name is lowercased. |
| `anthropic-header/ref` | `(headers name #!optional (default #f))` | Case-insensitive lookup. |
| `anthropic-status/ok?` | `(status) -> boolean` | 200..299. |
| `anthropic-status/retryable?` | `(status headers) -> boolean` | `x-should-retry` when present, else `(or (= status 429) (>= status 500))`. |

### 3.5 Transport

| Binding | Signature | Purpose |
|---|---|---|
| `make-anthropic-wire` / `anthropic-wire?` | `(status headers origin port close)` | Record constructor / predicate. `origin` is a datum naming what produced the wire — the curl backend puts the command there, the stub puts `` `(stub ,url) ``. |
| `anthropic-wire-status` `-headers` `-origin` `-port` `-close` | `(w) -> …` | Field accessors. |
| `anthropic-wire/close!` | `(w) -> (values exit-status diagnostics)` | Release: closes stdout, kills and reaps the child, drains and closes stderr, deletes the header file if it still exists. Idempotence is enforced **here**, by memoising the thunk's values into the `close` field — not by the backend, which may assume it is called exactly once. The discriminant is `procedure?` on the thunk, never `pair?` on the memo: a thunk answering **zero** values memoises `'()`, and the second release would then call `'()`. |
| `anthropic-wire/drain!` | `(w) -> string` | Slurps the body, closes, and raises `anthropic-transport-error` on a non-zero exit. Does **not** look at the HTTP status. |
| `anthropic-wire->body!` | `(w) -> string` | `anthropic-wire/drain!`, then raises `anthropic-api-error` on a non-2xx. |
| `anthropic-transport/curl` | `(url headers body stream?) -> anthropic-wire` | The production backend. |
| `anthropic-curl/argv` | `(url headers-file stream?) -> (string ...)` | The exact argument list, exposed so a test can assert no secret appears in it. |
| `anthropic-curl/headers->file!` | `(headers) -> path` | Writes headers one per line to a `0600` file created by `anthropic-temp/open!`. **Not** `create-temporary-file`: that opens without `O_EXCL` after a `file-exists?` test that answers `#f` for a dangling symlink, so the key could be written through a pre-planted link to a path of someone else's choosing. |
| `anthropic-temp/open!` | `() -> (values path fd)` | Creates a private file with `O_CREAT\|O_EXCL\|O_WRONLY` and mode `#o600`, under a temporarily tightened `file-creation-mode` of `#o077`, at a name carrying 96 bits from `random-bytes`; retries up to `anthropic-temp/attempts` names and then raises `anthropic-config-error`. Registers the path in `anthropic-temp/live`. |
| `anthropic-temp/delete!` | `(path)` | Unregisters and unlinks. |
| `anthropic-temp/pending` | `() -> (path ...)` | The files that exist right now — a reader for the registry, so a test can assert a request left nothing behind. |
| `anthropic-temp/directory` | `() -> string` | `TMPDIR`, `TMP`, `TEMP`, else `/tmp`. |
| `anthropic-random/bits` | `(n) -> integer` | `n` bytes from `random-bytes`, the OS entropy source, as one integer. **Not** `pseudo-random-integer`: that generator is not seeded per process on this CHICKEN — three independent runs printed the identical jitter sequence `(1.832 0.4 3.488)`. |
| `anthropic-random/unit` | `() -> flonum` | Uniform in `[0, 1)`, from `anthropic-random/bits`. Used by the retry jitter, which otherwise decorrelates nothing. |
| `anthropic-curl/read-headers` | `(port) -> (values status headers)` | Consumes the status line and header block, skipping 1xx interim blocks; leaves the port at the first body byte. |
| `anthropic-curl/status-line` | `(line) -> integer \| #f` | Parses `HTTP/2 429 ` and `HTTP/1.1 200 OK` alike. |
| `anthropic-transport/diagnosis` | `(exit-status) -> string` | English for a transport exit status. |
| `anthropic-transport/transient-codes` | list | The retryable statuses: 7, 18, 28, 52, 55, 56. |
| `anthropic-transport/transient?` | `(exit-status) -> boolean` | Whether a failure is worth retrying. This is the only question the conditions layer asks about a transport failure. |
| `anthropic-transport/still-running?` | `(pid) -> boolean` | `(eqv? 0 pid)`. The nohang `(process-wait p #t)` answers pid **0**, not `#f`, for a child that is still alive — measured `(0 #f #f)` — so the natural spelling `(not pid)` never fires and the kill-before-wait silently reverts to a full `--max-time` stall. Named so a test can pin the spelling without spawning a process. |
| `anthropic-curl/diagnosis` `anthropic-curl/transient-codes` `anthropic-curl/transient?` | — | One-line aliases of the three above, kept so existing prose and tests do not rot. New code uses the `anthropic-transport/*` names. |
| `make-anthropic-stub` / `anthropic-stub?` | `(script log closes)` | The offline test double's state. |
| `anthropic-stub/canned` | `(body #!key (status 200) (headers '(("content-type" "application/json")))) -> triple` | One scripted response; header names are lowercased so the stub and curl agree. |
| `anthropic-stub/make` | `(canned ...) -> anthropic-stub` | Builds a stub from an ordered sequence of canned responses. |
| `anthropic-stub/transport` | `(stub) -> transport` | Records the call, then replays; raises `anthropic-config-error` on an unscripted call. |
| `anthropic-stub/calls` | `(stub) -> ((url headers body stream?) ...)` | Every call in order, with header **values** passed through `anthropic/redact`. |
| `anthropic-stub/requests` | `(stub) -> (decoded-body ...)` | Every request body, decoded — assert with `⊦=` over data, never over JSON text. |
| `anthropic-stub/count` | `(stub) -> integer` | How many requests were actually made. |
| `anthropic-stub/closes` | `(stub) -> integer` | How many wires were **released**. The streaming `dynamic-wind`'s after-thunk is the only thing that kills the child and deletes the key file, and no returned value can see whether it ran — this can. |

### 3.6 Send and retry

| Binding | Signature | Purpose |
|---|---|---|
| `anthropic-retry/delay` | `(attempt headers) -> real` | `retry-after` (capped) when present, else exponential backoff with optional full jitter. |
| `anthropic-send` | `(body #!key (stream #f) (betas (anthropic/betas))) -> anthropic-wire` | One round trip through the current transport, with retries. A non-2xx becomes an `anthropic-api-error` before anything else looks at it. |

### 3.7 Messages, blocks, responses

| Binding | Signature | Purpose |
|---|---|---|
| `anthropic-content` | `(c) -> string \| vector` | Normalizes content: a string passes through, a list becomes a vector, `'()` becomes `#()`. |
| `anthropic-message` | `(role content) -> message` | `((role r) (content c))`. |
| `anthropic-message/user` | `(content) -> message` | A user turn. |
| `anthropic-message/assistant` | `(content) -> message` | An assistant turn, for hand-built transcripts. |
| `anthropic-message-role` | `(m) -> string \| #f` | Reads `role`. |
| `anthropic-message-content` | `(m) -> string \| vector` | Reads `content` verbatim. |
| `anthropic-message-blocks` | `(m) -> (block ...)` | Always a **list**; string content is lifted into one text block. |
| `anthropic-messages/validate!` | `(messages)` | Non-empty, first role `"user"`, no consecutive same-role, no empty content. Turns three common 400s into local errors. |
| `anthropic-block/text` | `(text) -> block` | `((type "text") (text …))`. |
| `anthropic-block/tool-use` | `(id name input) -> block` | Request-side `tool_use` block. |
| `anthropic-block/tool-result` | `(tool-use-id content #!key (is-error #f)) -> block` | `is_error` is **omitted** when false. Content is normalized. |
| `anthropic-block-type` | `(b) -> string \| #f` | The block's `type` tag. |
| `anthropic-block-text` | `(b) -> string` | The block's `text`, `""` when absent. |
| `anthropic-blocks/type` | `(type blocks) -> (block ...)` | Every block of one type, in order. |
| `anthropic-tool-choice` | `(spec #!optional (disable-parallel #f)) -> object` | `auto`/`any`/`none`/`(tool "name")` → the wire object. |
| `anthropic-request/body` | `(messages #!key model max-tokens system tools tool-choice disable-parallel thinking effort stop-sequences stream extra) -> alist` | The wire body as data. Absent options are **absent keys**; `extra` merges last-wins. |
| `make-anthropic-response` / `anthropic-response?` | `(status headers body json input-errors)` | Record. |
| `anthropic-response-status` `-headers` `-body` `-json` `-input-errors` | `(r) -> …` | Field accessors. `-input-errors` is `((tool_use_id raw) ...)`, `'()` on the blocking path. |
| `anthropic-response/decode` | `(status headers body #!optional (input-errors '())) -> anthropic-response` | Validates, parses and checks `"type":"message"`. |
| `anthropic-response/of-json` | `(status headers json input-errors) -> anthropic-response` | Wraps an already-decoded message (the streaming path). |
| `anthropic-response-id` `-model` `-stop-reason` `-stop-details` `-usage` `-request-id` | `(r) -> …` | Top-level fields; `-request-id` reads the header first, then the body. |
| `anthropic-response-content` | `(r) -> vector` | `content` as a vector, `#()` when absent or malformed. |
| `anthropic-response/blocks` | `(r) -> (block ...)` | `content` as a list; `'()` on a pre-output refusal's empty array. |
| `anthropic-response/text` | `(r) -> string` | Every `text` block concatenated; `""` rather than a crash. |
| `anthropic-response/tool-uses` | `(r) -> (block ...)` | All `tool_use` blocks of the turn. |
| `anthropic-response/thinking` | `(r) -> (block ...)` | Thinking blocks, for display only. |
| `anthropic-response/refusal?` | `(r) -> boolean` | Tests `stop_reason`, never `stop_details`. |
| `anthropic-response/message` | `(r) -> message` | The assistant turn to append: the decoded `content`, untouched. |
| `anthropic-usage/ref` | `(r key #!optional (default 0))` | One usage counter, defaulting on both an absent key and a JSON `null`. |
| `anthropic-usage/input-tokens` | `(r) -> integer` | `input_tokens` alone — the **uncached remainder**. |
| `anthropic-usage/output-tokens` | `(r) -> integer` | `output_tokens`. |
| `anthropic-usage/cache-read-tokens` `anthropic-usage/cache-creation-tokens` | `(r) -> integer` | Cache counters, 0 when absent. |
| `anthropic-usage/prompt-tokens` | `(r) -> integer` | The **total** prompt size = input + cache-creation + cache-read. |

### 3.8 Tools

| Binding | Signature | Purpose |
|---|---|---|
| `define-tool` | `(define-tool (NAME (param TYPE "doc" opt ...) ...) "description" body ...)` | Defines the plain procedure `NAME`, the record `NAME/tool`, and registers it. |
| `define-tool/strict` | same | As above plus top-level `"strict": true` and `additionalProperties: false` on every generated object — one coupled switch. |
| `anthropic-define-tool*` | `(anthropic-define-tool* strict? head "description" body ...)` | The ir-macro both surfaces expand to. Not for direct use. |
| `make-anthropic-tool` / `anthropic-tool?` | `(name description schema strict procedure)` | Record. |
| `anthropic-tool-name` `-description` `-schema` `-strict` `-procedure` | `(t) -> …` | Field accessors. |
| `make-anthropic-tool-failure` / `anthropic-tool-failure?` / `anthropic-tool-failure-message` | `(message)` | Marks a tool outcome as failed. |
| `anthropic-tool/error` | `(message) -> anthropic-tool-failure` | What an `around` hook returns to **deny** a call. |
| `anthropic-tool/register!` | `(tool) -> tool` | Puts the tool in the registry under its JSON name. |
| `anthropic-tool/registered` | `(name) -> tool \| #f` | Looks a registered tool up by JSON name. |
| `anthropic-tool/registry` | srfi-69 hash table | name-string → tool. The loop never reads it; it takes an explicit list. |
| `anthropic-tool->json` | `(tool) -> alist` | `((name …) (description …) (input_schema …) [(strict #t)])`. |
| `anthropic-tools->json` | `(tools) -> vector` | The `tools` request field; accepts records or raw alists. |
| `anthropic-tools->table` | `(tools) -> hash-table` | name → tool dispatch table for one conversation. |
| `anthropic-tool/input-ref` | `(input key name) -> value` | Reads a **required** argument; raises `anthropic-tool-error` when the model omitted it. |
| `anthropic-tool/apply` | `(tool input) -> value` | Runs a tool against a decoded input object. |
| `anthropic-tool-result->string` | `(v) -> string` | Non-empty strings pass through; `""` becomes `"(the tool returned an empty string)"` and `(void)` becomes `"(the tool returned no value)"` (an empty `tool_result` is a 400); everything else is JSON-encoded. The single choke point for wire-safe tool output. |
| `anthropic-tool/around` | `(name input run) -> value` | Default around-advice: just `(run)`. Replace for approval, logging, sandboxing. |
| `anthropic-tool-use->result` | `(block table around input-errors) -> block` | Executes one `tool_use` block and returns its `tool_result`, converting every failure into `is_error #t`. |

### 3.9 Streaming

| Binding | Signature | Purpose |
|---|---|---|
| `make-anthropic-event` / `anthropic-event?` | `(name data raw)` | One SSE frame. |
| `anthropic-event-name` `-data` `-raw` | `(ev) -> …` | Field accessors; `data` is `(void)` when absent or unparseable. |
| `anthropic-sse/read` | `(port) -> anthropic-event \| eof-object` | Reads one frame: skips `:` comments, joins repeated `data:` lines with a newline, terminates on the blank line. |
| `anthropic-sse->§` | `(port #!key (finish void)) -> stream` | The lazy `(aux stream)`-shaped view; `finish` runs once at eof. |
| `anthropic-sse/fold` | `(f seed §) -> any` | Strict, tail-recursive left fold. |
| `anthropic-sse/events` | `(port) -> (anthropic-event ...)` | Every frame as a list — the offline fixture driver. |
| `anthropic-event/text` | `(ev) -> string \| #f` | The `text_delta` fragment of a `content_block_delta`, else `#f`. |
| `make-anthropic-sse-state` | `(message open done errors stopped)` | Accumulator state. |
| `anthropic-accumulator/fresh` | `() -> anthropic-sse-state` | A fresh accumulator. |
| `anthropic-accumulator/event` | `(st ev) -> st` | The state machine, one event at a time. |
| `anthropic-accumulator->message` | `(st) -> decoded message` | Sorts finished blocks by index and installs them as `content`. |
| `anthropic-sse->message` | `(§ #!key (on-event void)) -> (values message input-errors)` | Folds a whole stream; raises if a block is unfinished or `message_stop` never arrived. |

### 3.10 Entry points

| Binding | Signature | Purpose |
|---|---|---|
| `anthropic/messages` | `(messages #!key model max-tokens system tools tool-choice disable-parallel thinking effort stop-sequences extra betas) -> anthropic-response` | The blocking call: validate, encode, send with retries, decode, wrap. Raises rather than returning a non-2xx. |
| `anthropic/ask` | `(prompt . kwargs) -> string` | String in, string out. |
| `anthropic/stream` | `(messages #!key … (on-event void) (on-text void)) -> anthropic-response` | The streaming call. Not retried — a retry would replay tokens the caller already saw. |
| `anthropic/converse` | `(messages #!key tools model max-tokens system tool-choice disable-parallel thinking effort stop-sequences extra (max-iterations 8) (max-pauses 5) (around anthropic-tool/around) (on-response …)) -> (values response transcript)` | The agentic loop. |

---

## 4. File-by-file plan

### 4.0 Why one file, not two

`src/aux.anthropic.scm` only. Three reasons, in order of weight:

1. **The SSE decoder is not separable.** It needs `anthropic-json/parse` (the byte-length-correct one), `anthropic-json/set`/`merge`, the condition constructors and the wire record. A second module `(aux anthropic sse)` would have to import a third module holding those, turning one file into three, with a `(dependencies …)`-free but listing-order-sensitive `aux.egg` edit for each.
2. **`code/scheme/file` reads only the first datum** (`src/aux.sxml.scm:106-109` — `(with-input-from-file (car body) (lambda () (read)))`). One file with one top-level `(module …)` form means the whole module renders into the generated HTML report. Two files means the report shows half the module.
3. **Precedent.** `src/aux.kanren.micro.show.scm` is 469 lines in one file; `src/aux.hansei.scm` likewise. The repo splits a module only when the halves are independently useful (`aux.kanren.micro` solver vs `aux.kanren.micro.show` renderer). Here they are not.

§4's code fences total 1543 lines, 1393 of them non-blank. The largest existing module is `src/aux.kanren.micro.scm` at 622 lines, so `src/aux.anthropic.scm` would be the largest module in the repo by more than a factor of two. That is a real cost and it is stated plainly rather than rounded away: the three reasons above are what buys it, and if any of them stops holding — in particular if `code/scheme/file` ever learns to read more than the first datum — the SSE half is the natural seam to cut along.

The other files touched — `src/aux.egg`, `src/Makefile`, `src/test/anthropic.scm`, `src/test/anthropic-live.scm`, `README.md` — are covered in §6 and §7.

### 4.1 Header and module form

```scheme

; (aux anthropic) -- a client for the Anthropic *Messages* API, spoken over the `curl` binary.
;
; The Docker image in which `make test` runs has neither the http-client egg nor openssl, so there
; is no TLS-capable HTTP client in Scheme here at all; what it does have is `curl`.  We therefore
; shell out, behind the parameter `anthropic/transport`, so a native or FFI backend can replace the
; whole thing later without a single call site changing.  A transport is
;
;   (transport url headers body stream?) -> anthropic-wire
;
; with `headers` a list of `(name value)` two-element lists (lowercase names), `body` the encoded
; JSON request as a string, `stream?` #t when the caller means to read incrementally, and the
; returned record carrying the HTTP status, the response headers, the command, an input port
; positioned at the first body byte, and an idempotent `close` thunk answering
; (values exit-status diagnostics).  A transport that cannot speak at all raises
; `anthropic-transport-error`; it never invents an HTTP status.  The offline suite swaps in
; `anthropic-stub/transport`, which is why `make test` needs no network and no API key.
;
; Three curl decisions, each of which looks arbitrary until you see what it buys:
;
;   * `--include`.  curl writes the status line and headers to stdout AHEAD of the body, in one
;     strictly ordered channel, so we read them off the port and hand the same port on.  No -D
;     file, no -w trailer a hostile body could forge, and the blocking and streaming paths are
;     identical up to the first body byte.
;
;   * NO `--fail-with-body`.  With it curl exits 22 for every HTTP error and a 429 is
;     indistinguishable from a 404.  Without it, exit 0 means "an HTTP response was obtained",
;     whatever its status, and non-zero means a network or TLS failure.  That is exactly the split
;     the retry policy needs: retry on status, give up on exit code.
;
;   * `--header @FILE` and `--data-binary @-`.  Neither the key nor the prompt is ever an argv
;     element, so neither shows up in `ps`.  The header file is created under a temporarily
;     tightened file-creation-mode of #o077 -- create-temporary-file otherwise yields 0644 and
;     chmod-after-create leaves a race in which the key is world readable.
;
; The (chicken process) API is CHICKEN 6 and differs from CHICKEN 5 in two ways that will bite
; anyone editing this file.  `process*` returns ONE process object, not four values.  And the port
; accessors are named from the CHILD's point of view: `process-input-port` is the OUTPUT port you
; write the child's stdin to, `process-output-port` is the INPUT port you read its stdout from.
; Both are commented at the call site; please do not "fix" them.
;
; Two JSON procedures live here rather than in (aux simdjson), and both should eventually move
; upstream.  `anthropic-json/parse`, because `simdjson-parse/ondemand` (aux.simdjson.scm:93) hands
; the C side `string-length`, a CHARACTER count, where simdjson wants BYTES -- so the first
; accented response truncates and then ABORTS the process with an uncatchable C++ exception.  And
; `anthropic-json/write`, because `->string/json` escapes strings with Scheme `write`, which emits
; CHICKEN's `\x01;` for control characters, prints ratnums as `1/3`, and truncates flonums to 15
; significant digits -- none of which is JSON.  src/chicken-simdjson.cpp contains no try/catch at
; all, so EVERY parse in this module is gated behind `anthropic-json/well-formed?`, a complete
; pure-Scheme JSON validator.  That gate is load bearing: without it one malformed proxy page takes
; down `make test` with SIGABRT rather than failing a case.

(module (aux anthropic) *

  (import scheme
          (scheme base)
          (scheme char)
          (chicken base)
          (chicken condition)
          (chicken file)
          (chicken file posix)
          (chicken flonum)
          (chicken io)
          (chicken keyword)
          (chicken port)
          (chicken process)
          (chicken process signal)          ; signal/term, for the kill-before-wait in §4.8
          (chicken process-context)
          (chicken random)
          (chicken sort)
          (chicken string)
          srfi-1
          srfi-69
          (aux base)
          (aux simdjson))

  ; `define-tool` folds a variable-length spec list into two parallel results at expansion time,
  ; so it is an ir-macro and needs its helpers available for syntax.  (aux base) is not a prelude:
  ; a module re-exports only what it DEFINES, so every line below is load bearing.
  (import-for-syntax scheme
                     (scheme base)          ; exact-integer?, called by enum-type at EXPANSION time
                     (chicken base)
                     (chicken string)
                     (chicken syntax)
                     srfi-1
                     (aux base))
```

Note on the import list: `make-parameter` and `open-input-string` are in `(scheme base)` and **not** in `(chicken base)`/`(chicken port)`; `string-downcase` is in `(scheme char)` only; `string->keyword` is in `(chicken keyword)`; `process-signal` comes with `(chicken process)` but `signal/term` needs `(chicken process signal)`. All verified by probe.

`(scheme base)` in the `import-for-syntax` list is load-bearing and easy to lose. `anthropic-define-tool*`'s `enum-type` helper (§4.14) calls `exact-integer?`, which lives only in `scheme.base`, and it calls it *at expansion time*. Today the bug is latent because the `(every string? vs)` clause short-circuits ahead of it and the suite's only `enum` is all strings — so the first integer enum anybody writes fails to expand, with an unbound-identifier error pointing at the macro rather than at their tool. §6.6 pins it with a case.

The module uses `display` rather than `write-string` throughout. The reason is *not* that `write-string` is unavailable — it is in `(scheme base)`, which is imported, and would work. It is that `write-string` is absent from `(chicken io)`, which is where a reader reaching for it will look first; `display` is the one spelling nobody has to check.

### 4.2 Parameters

```scheme
  ; parameters -----------------------------------------------------------------------------

  (define anthropic/api-key           (make-parameter #f))   ; #f => consult anthropic/env-api-key
  ; The environment read is itself a parameter so the offline suite can prove the no-key path
  ; WITHOUT depending on the developer's own environment.  Parameterizing anthropic/api-key to
  ; #f is not enough: the `or` below would fall through to a real exported ANTHROPIC_API_KEY
  ; and the case would fail for exactly the person about to run the live suite.
  (define anthropic/env-api-key       (make-parameter (λ () (get-environment-variable "ANTHROPIC_API_KEY"))))
  (define anthropic/base-url          (make-parameter "https://api.anthropic.com/v1/messages"))
  (define anthropic/model             (make-parameter "claude-opus-5"))
  (define anthropic/version           (make-parameter "2023-06-01"))
  (define anthropic/betas             (make-parameter '()))
  (define anthropic/max-tokens        (make-parameter 16000))
  (define anthropic/stream-max-tokens (make-parameter 64000))
  (define anthropic/connect-timeout   (make-parameter 10))
  (define anthropic/max-time          (make-parameter 600))
  (define anthropic/stream-max-time   (make-parameter 1800))
  (define anthropic/retries           (make-parameter 3))
  (define anthropic/backoff           (make-parameter 1.0))
  (define anthropic/backoff-cap       (make-parameter 30.0))
  (define anthropic/backoff-jitter    (make-parameter #t))
  (define anthropic/curl              (make-parameter "curl"))

  ; `process-sleep` is a live CHICKEN 6 procedure -- modules.db has it as
  ; (process-sleep value chicken.process) -- and it is deliberately NOT `sleep` from
  ; (chicken base), which rejects flonums and so cannot take a backoff delay at all.
  ; `process-sleep` takes whole seconds only, so the granularity is one second.  Tests
  ; parameterize this to a recorder, which is why the retry cases cost nothing.
  (define anthropic/sleep
    (make-parameter (λ (seconds) (process-sleep (max 1 (inexact->exact (ceiling seconds)))))))
```

`anthropic/transport` is deliberately **not** here: its initializer names `anthropic-transport/curl`, which is evaluated at module-load time, so it is defined immediately after that procedure (section 4.8).

### 4.3 Conditions

```scheme
  ; conditions -----------------------------------------------------------------------------
  ;
  ; Everything this module signals is a compound condition of kinds (exn anthropic-error KIND),
  ; so `⊦⧳ ((exn))` still catches it, `(condition-case ... (c (anthropic-api-error) ...))` reaches
  ; the status and type, and `(get-condition-property c 'anthropic-error 'retryable)` answers the
  ; only question the retry loop asks.

  (define (anthropic-condition kind retryable message properties)
    (apply condition
           `((exn message ,message arguments ())
             (anthropic-error kind ,kind retryable ,retryable)
             (,kind ,@properties))))

  (define (anthropic-message/error tag detail)
    (string-append "anthropic: " tag "\n\n" (->string/pretty-print detail)))

  (define (anthropic-raise/config message detail)
    (signal (anthropic-condition 'anthropic-config-error #f
                                 (anthropic-message/error message detail)
                                 `(detail ,detail))))

  (define (anthropic-raise/encode message value)
    (signal (anthropic-condition 'anthropic-encode-error #f
                                 (anthropic-message/error message value)
                                 `(value ,value))))

  (define (anthropic-raise/decode message body)
    (signal (anthropic-condition 'anthropic-decode-error #f
                                 (anthropic-message/error message body)
                                 `(body ,body))))

  ; `anthropic-transport/transient?`, never `anthropic-curl/transient?`: the conditions layer sits
  ; UNDER the transport in §2's diagram and must not name a backend.  The two answer identically
  ; today -- CURLcode and curl(1)'s exit status agree numerically on every code in the table -- but
  ; that is an accident of curl, not a property of the retry policy.
  (define (anthropic-raise/transport message origin exit-status diagnostics)
    (signal (anthropic-condition 'anthropic-transport-error
                                 (anthropic-transport/transient? exit-status)
                                 (anthropic-message/error
                                   (string-append "transport failure: " message)
                                   `((origin ,(anthropic/redact (->string origin)))
                                     (exit-status ,exit-status)
                                     (diagnostics ,diagnostics)))
                                 `(origin ,origin exit-status ,exit-status
                                   diagnostics ,diagnostics))))

  (define (anthropic-raise/api status headers body)
    (let* ((json (if (anthropic-json/well-formed? body) (anthropic-json/parse body) '()))
           (err (anthropic-json/ref json 'error '()))
           (type (anthropic-json/ref err 'type "api_error"))
           (text (anthropic-json/ref err 'message body))
           (request-id (let1 (v (anthropic-json/ref json 'request_id #f))
                         (if (string? v) v (anthropic-header/ref headers "request-id" #f)))))
      (signal (anthropic-condition
                'anthropic-api-error (anthropic-status/retryable? status headers)
                (anthropic-message/error (string-append "HTTP " (number->string status))
                                         `((type ,type)
                                           (message ,(anthropic/redact (->string text)))
                                           (request_id ,request-id)))
                `(status ,status type ,type api-message ,text request-id ,request-id
                  headers ,headers body ,body)))))

  (define (anthropic-raise/sse message raw)
    (signal (anthropic-condition 'anthropic-sse-error #f
                                 (anthropic-message/error (string-append "malformed SSE: " message) raw)
                                 `(raw ,raw))))

  (define (anthropic-raise/tool message detail)
    (signal (anthropic-condition 'anthropic-tool-error #f
                                 (anthropic-message/error message detail)
                                 `(detail ,detail))))

  (define (anthropic-raise/loop reason message response)
    (signal (anthropic-condition 'anthropic-loop-error #f
                                 (anthropic-message/error message `((reason ,reason)))
                                 `(reason ,reason response ,response))))

  (define anthropic-error?           (condition-predicate 'anthropic-error))
  (define anthropic-config-error?    (condition-predicate 'anthropic-config-error))
  (define anthropic-encode-error?    (condition-predicate 'anthropic-encode-error))
  (define anthropic-decode-error?    (condition-predicate 'anthropic-decode-error))
  (define anthropic-transport-error? (condition-predicate 'anthropic-transport-error))
  (define anthropic-api-error?       (condition-predicate 'anthropic-api-error))
  (define anthropic-sse-error?       (condition-predicate 'anthropic-sse-error))
  (define anthropic-tool-error?      (condition-predicate 'anthropic-tool-error))
  (define anthropic-loop-error?      (condition-predicate 'anthropic-loop-error))

  (define (anthropic-error-ref c property #!optional (default #f))
    (let1 (kind (get-condition-property c 'anthropic-error 'kind #f))
      (if kind (get-condition-property c kind property default) default)))

  (define (anthropic-error-retryable? c)
    (and (anthropic-error? c) (get-condition-property c 'anthropic-error 'retryable #f) #t))
```

`anthropic-raise/api` deliberately does **not** synthesise a status from an error type string: the wire always supplies one. The SSE `error` frame, which has no HTTP status, is handled separately in section 4.16 and carries `status` `(void)`.

### 4.4 Configuration and headers

```scheme
  ; configuration --------------------------------------------------------------------------

  (define (anthropic-api-key/current)
    (let1 (k (or (anthropic/api-key) ((anthropic/env-api-key))))
      (and (string? k) (positive? (string-length k)) k)))

  (define (anthropic-api-key/effective)
    (or (anthropic-api-key/current)
        (anthropic-raise/config "no API key"
                                "set ANTHROPIC_API_KEY or parameterize anthropic/api-key")))

  ; `content-type` lives HERE, not in the curl argv, so a replacement backend that faithfully
  ; sends the list it is handed sends it too, and so the offline suite can assert on it.
  (define (anthropic-request/headers #!key (betas (anthropic/betas)))
    `(("content-type" "application/json")
      ("x-api-key" ,(anthropic-api-key/effective))
      ("anthropic-version" ,(anthropic/version))
      ,@(if (null? betas) '() `(("anthropic-beta" ,(string-intersperse betas ","))))))

  (define (anthropic/redact text)
    (let1 (key (anthropic-api-key/current))
      (if (and (string? text) (string? key) (>= (string-length key) 8))
          (string-translate* text (list (cons key "<redacted>")))
          text)))
```

### 4.5 The JSON validator

Only one thing stands between a truncated response and SIGABRT, and it is this procedure. It is a complete recursive-descent JSON validator — object, array, string with escapes, the full number grammar, the three literals — restricted at the top level to an object or an array, because simdjson's on-demand walker throws `SCALAR_DOCUMENT_AS_VALUE` on a bare scalar document.

```scheme
  ; the JSON validator ----------------------------------------------------------------------
  ;
  ; src/chicken-simdjson.cpp has no try/catch, so a simdjson_error reaches std::terminate and the
  ; whole process dies with SIGABRT -- uncatchable from Scheme.  Sniffing for a leading `{` is not
  ; enough: a truncated body starts with `{` too, and a bare scalar document such as `42` throws
  ; SCALAR_DOCUMENT_AS_VALUE.  So this is a complete validator, and nothing is parsed without it.

  (define (anthropic-char/digit? c) (and (char>=? c #\0) (char<=? c #\9)))

  (define (anthropic-char/hex? c)
    (or (anthropic-char/digit? c)
        (and (char>=? c #\a) (char<=? c #\f))
        (and (char>=? c #\A) (char<=? c #\F))))

  (define (anthropic-json/well-formed? s)
    (and
      (string? s)
      (let* ((n (string-length s))
             (at (λ (i) (and (< i n) (string-ref s i)))))
        (letrec
          ((skip (λ (i) (if (and (< i n) (char-whitespace? (string-ref s i))) (skip (add1 i)) i)))
           (word (λ (i w) (let1 (m (string-length w))
                            (and (<= (+ i m) n) (string=? w (substring s i (+ i m))) (+ i m)))))
           (digits (λ (i) (let D ((k i))
                            (if (and (< k n) (anthropic-char/digit? (string-ref s k)))
                                (D (add1 k))
                                (and (> k i) k)))))
           (number
             (λ (i)
               (let* ((i (if (eqv? #\- (at i)) (add1 i) i))
                      (i (cond
                           ((eqv? #\0 (at i)) (add1 i))
                           ((and (at i) (anthropic-char/digit? (at i))) (digits i))
                           (else #f))))
                 (and i
                      (let1 (i (if (eqv? #\. (at i)) (digits (add1 i)) i))
                        (and i
                             (if (memv (at i) (list #\e #\E))
                                 (let1 (j (add1 i))
                                   (digits (if (memv (at j) (list #\+ #\-)) (add1 j) j)))
                                 i)))))))
           (text
             (λ (i)
               (and (eqv? #\" (at i))
                    (let S ((k (add1 i)))
                      (let1 (c (at k))
                        (cond
                          ((not c) #f)
                          ((char=? c #\") (add1 k))
                          ((char=? c #\\)
                           (let1 (e (at (add1 k)))
                             (cond
                               ((not e) #f)
                               ((memv e (list #\" #\\ #\/ #\b #\f #\n #\r #\t)) (S (+ k 2)))
                               ((char=? e #\u)
                                (and (<= (+ k 6) n)
                                     (let H ((j (+ k 2)))
                                       (cond
                                         ((= j (+ k 6)) (S (+ k 6)))
                                         ((anthropic-char/hex? (string-ref s j)) (H (add1 j)))
                                         (else #f)))))
                               (else #f))))
                          ((char<? c #\space) #f)          ; a raw control byte is not legal JSON
                          (else (S (add1 k)))))))))
           (value
             (λ (i)
               (let1 (i (skip i))
                 (let1 (c (at i))
                   (cond
                     ((not c) #f)
                     ((char=? c #\{) (object (add1 i)))
                     ((char=? c #\[) (array (add1 i)))
                     ((char=? c #\") (text i))
                     ((char=? c #\t) (word i "true"))
                     ((char=? c #\f) (word i "false"))
                     ((char=? c #\n) (word i "null"))
                     (else (number i)))))))
           (array
             (λ (i)
               (let1 (i (skip i))
                 (if (eqv? #\] (at i))
                     (add1 i)
                     (let A ((i i))
                       (let1 (j (value i))
                         (and j
                              (let1 (j (skip j))
                                (cond
                                  ((eqv? #\, (at j)) (A (add1 j)))
                                  ((eqv? #\] (at j)) (add1 j))
                                  (else #f))))))))))
           (object
             (λ (i)
               (let1 (i (skip i))
                 (if (eqv? #\} (at i))
                     (add1 i)
                     (let O ((i i))
                       (let1 (k (text (skip i)))
                         (and k
                              (let1 (k (skip k))
                                (and (eqv? #\: (at k))
                                     (let1 (v (value (add1 k)))
                                       (and v
                                            (let1 (v (skip v))
                                              (cond
                                                ((eqv? #\, (at v)) (O (add1 v)))
                                                ((eqv? #\} (at v)) (add1 v))
                                                (else #f)))))))))))))))
          (let1 (i (skip 0))
            (and (memv (at i) (list #\{ #\[))
                 (let1 (j (value i))
                   (and j (= n (skip j))))))))))
```

### 4.6 Parse, encode, access

```scheme
  ; parsing ---------------------------------------------------------------------------------

  (define (anthropic-json/utf8-length s)
    (let1 (n (string-length s))
      (let L ((i 0) (acc 0))
        (if (= i n)
            acc
            (let1 (u (char->integer (string-ref s i)))
              (L (add1 i)
                 (+ acc (cond ((< u #x80) 1) ((< u #x800) 2) ((< u #x10000) 3) (else 4)))))))))

  (define (anthropic-json/parse s)
    (unless (anthropic-json/well-formed? s)
      (anthropic-raise/decode "not a well-formed JSON object or array" s))
    (let* ((dom (simdjson-parse-ondemand-callback s (anthropic-json/utf8-length s)))
           (v (simdjson->scheme dom)))
      (simdjson-free dom)
      v))

  ; encoding ---------------------------------------------------------------------------------

  (define (anthropic-json/unicode-escape u)
    (let1 (hex (number->string u 16))
      (string-append "\\u" (make-string (- 4 (string-length hex)) #\0) hex)))

  (define (anthropic-json/escape s port)
    (display "\"" port)
    (let1 (n (string-length s))
      (let L ((i 0))
        (when (< i n)
          (let* ((c (string-ref s i)) (u (char->integer c)))
            (cond
              ((char=? c #\") (display "\\\"" port))
              ((char=? c #\\) (display "\\\\" port))
              ((= u 8) (display "\\b" port))
              ((= u 9) (display "\\t" port))
              ((= u 10) (display "\\n" port))
              ((= u 12) (display "\\f" port))
              ((= u 13) (display "\\r" port))
              ((< u #x20) (display (anthropic-json/unicode-escape u) port))
              (else (display c port))))
          (L (add1 i)))))
    (display "\"" port))

  ; `flonum-print-precision` defaults to 15, which turns 3.141592653589793 into
  ; 3.14159265358979.  A replayed assistant turn must be byte-faithful, so we search upward for
  ; the shortest representation that reads back to the same flonum, and restore the parameter.
  (define (anthropic-json/flonum->string x)
    (let1 (saved (flonum-print-precision))
      (dynamic-wind
        (τ (void))
        (τ (let L ((p 15))
             (flonum-print-precision p)
             (let1 (s (number->string x))
               (if (or (>= p 17) (eqv? x (string->number s))) s (L (add1 p))))))
        (τ (flonum-print-precision saved)))))

  (define (anthropic-json/emit v port)
    (cond
      ((string? v) (anthropic-json/escape v port))
      ((boolean? v) (display (if v "true" "false") port))
      ((void? v) (display "null" port))
      ((null? v) (display "{}" port))                    ; an empty OBJECT; #() is the empty ARRAY
      ((symbol? v) (anthropic-json/escape (symbol->string v) port))
      ((char? v) (anthropic-json/escape (string v) port))
      ((number? v)
       (cond
         ((and (exact? v) (integer? v)) (display v port))
         ((and (exact? v) (rational? v))
          (display (anthropic-json/flonum->string (exact->inexact v)) port))
         ((and (flonum? v) (= v v) (< -∞ v ∞)) (display (anthropic-json/flonum->string v) port))
         (else (anthropic-raise/encode "not a finite JSON number" v))))
      ((vector? v)
       (display "[" port)
       (let1 (n (vector-length v))
         (let L ((i 0))
           (when (< i n)
             (when (> i 0) (display "," port))
             (anthropic-json/emit (vector-ref v i) port)
             (L (add1 i)))))
       (display "]" port))
      ((pair? v)
       (display "{" port)
       (let L ((lst v) (first? #t))
         (when (pair? lst)
           (unless first? (display "," port))
           (let1 (kv (car lst))
             (unless (and (pair? kv) (pair? (cdr kv)) (null? (cddr kv)))
               (anthropic-raise/encode "a JSON object entry must be a two-element list" kv))
             (let1 (key (car kv))
               (cond
                 ((symbol? key) (anthropic-json/escape (symbol->string key) port))
                 ((string? key) (anthropic-json/escape key port))
                 (else (anthropic-raise/encode "a JSON object key must be a symbol or a string" kv))))
             (display ":" port)
             (anthropic-json/emit (cadr kv) port))
           (L (cdr lst) #f)))
       (display "}" port))
      (else (anthropic-raise/encode "cannot be represented as JSON" v))))

  (define (anthropic-json/write v) (letport/output-string port (anthropic-json/emit v port)))

  ; access ------------------------------------------------------------------------------------
  ;
  ; simdjson decodes an object to a list of two-element (symbol value) lists, so the accessor is
  ; assq-then-cadr.  `alist-ref` from (chicken base) is a trap -- it returns the CDR, i.e. the
  ; one-element list (value).  `letassoc` from (aux base) would work but uses `assoc` and demands
  ; an `else` clause, so `ref` and `has?` would disagree on the comparator; keys are interned
  ; symbols, so `assq` is both correct and faster.

  (define (anthropic-json/ref obj key #!optional (default (void)))
    (let1 (kv (and (pair? obj) (assq key obj)))
      (if (pair? kv) (cadr kv) default)))

  (define (anthropic-json/has? obj key) (and (pair? obj) (pair? (assq key obj)) #t))

  (define (anthropic-json/null? v) (void? v))

  (define (anthropic-json/vector->list v) (if (vector? v) (vector->list v) '()))

  (define (anthropic-json/set obj key val)
    (let L ((o obj) (acc '()) (hit #f))
      (cond
        ((not (pair? o)) (reverse (if hit acc (cons (list key val) acc))))
        ((eq? key (car (car o))) (L (cdr o) (cons (list key val) acc) #t))
        (else (L (cdr o) (cons (car o) acc) hit)))))

  (define (anthropic-json/merge obj patch)
    (if (pair? patch)
        (foldl (λ (o kv) (anthropic-json/set o (car kv) (cadr kv))) obj patch)
        obj))
```

`foldl` in CHICKEN passes the accumulator first — verified — so `(λ (o kv) …)` is the right order.

### 4.7 Strings, headers, status

```scheme
  ; strings and headers -----------------------------------------------------------------------

  (define (anthropic-string/trim s)
    (let1 (n (string-length s))
      (let L ((a 0) (b n))
        (cond
          ((and (< a b) (char-whitespace? (string-ref s a))) (L (add1 a) b))
          ((and (< a b) (char-whitespace? (string-ref s (sub1 b)))) (L a (sub1 b)))
          (else (substring s a b))))))

  ; (read-string #f port) answers the EOF OBJECT, not "", on a port that produced nothing.
  (define (anthropic-port->string port)
    (let1 (s (read-string #f port)) (if (eof-object? s) "" s)))

  (define (anthropic-header/check! name value)
    (for-each (λ (s)
                (unless (string? s)
                  (anthropic-raise/config "a header name and value must both be strings" name))
                (when (or (substring-index "\n" s) (substring-index "\r" s))
                  (anthropic-raise/config "a header may not contain a newline" name)))
              (list name value)))

  (define (anthropic-header/parse line)
    (let1 (i (substring-index ":" line))
      (and i
           (positive? i)
           (list (string-downcase (anthropic-string/trim (substring line 0 i)))
                 (anthropic-string/trim (substring line (add1 i)))))))

  (define (anthropic-header/ref headers name #!optional (default #f))
    (let1 (h (assoc (string-downcase name) headers))
      (if (pair? h) (cadr h) default)))

  (define (anthropic-status/ok? status) (and (exact-integer? status) (<= 200 status 299)))

  ; A range test, not membership in {429,500,529}: the whole 5xx class is the server-error class.
  ; `x-should-retry` is read defensively -- it was NOT observed on a live 401, so nothing depends
  ; on its presence.
  (define (anthropic-status/retryable? status headers)
    (let1 (hint (anthropic-header/ref headers "x-should-retry" #f))
      (cond
        ((equal? "true" hint) #t)
        ((equal? "false" hint) #f)
        (else (and (exact-integer? status) (or (= status 429) (>= status 500)))))))
```

### 4.8 The wire record and the curl transport

```scheme
  ; the wire --------------------------------------------------------------------------------

  ; `origin`, not `command`: the stub already fills this field with `(stub ,url)`, which is not a
  ; command, and a libcurl backend would put a handle there.  It exists for error messages only.
  (define-record anthropic-wire status headers origin port close)

  ; Idempotence lives HERE, not in any backend.  The curl backend used to carry its own `reaped`
  ; flag, which made "you may close a wire twice" a property of one transport; the stub's thunk had
  ; no guard at all.  Memoising the values into the field gives it to every backend for free, so a
  ; backend's close thunk may assume it runs exactly once.
  (define (anthropic-wire/close! w)
    (let1 (c (anthropic-wire-close w))
      (if (pair? c)
          (apply values c)
          (receive vals (c)
            (anthropic-wire-close-set! w vals)
            (apply values vals)))))

  ; Transport exit statuses.  The names are backend-neutral on purpose: the numbers below happen to
  ; be curl(1) exit codes, and libcurl's CURLcode agrees with every one of them, but the retry
  ; policy is the module's, not curl's.  Getting this table wrong costs a retry that will not help,
  ; or a retry that was not attempted.  Only 6, 7, 23 and 126 were observed here; the rest are from
  ; curl's manual.
  (define anthropic-transport/transient-codes '(7 18 28 52 55 56))

  (define (anthropic-transport/transient? code) (member? code anthropic-transport/transient-codes))

  (define (anthropic-transport/diagnosis code)
    (match/first code
      (1   "the transport rejected its own arguments (a bad option, or an unsupported protocol)")
      (6   "could not resolve host")
      (7   "could not connect")
      (18  "the transfer ended prematurely")
      (23  "could not write the output (the response was abandoned)")
      (28  "the operation timed out")
      (35  "the TLS handshake failed")
      (52  "empty reply from server")
      (55  "failure sending network data")
      (56  "failure receiving network data")
      (126 "the transport program could not be executed (not executable, or not found on PATH)")
      (127 "the shell could not find the transport program")
      (else (conc "the transport exited with status " code))))

  ; aliases, so §6's older case names and anyone's existing code keep working.  New code uses the
  ; anthropic-transport/* names.
  (define anthropic-curl/transient-codes anthropic-transport/transient-codes)
  (define anthropic-curl/transient?      anthropic-transport/transient?)
  (define anthropic-curl/diagnosis       anthropic-transport/diagnosis)

  (define (anthropic-curl/argv url headers-file stream?)
    (append
      (list "--silent"                                 ; no progress meter
            "--show-error"                             ; but do report failures, on stderr
            "--include"                                ; status line + headers first, on stdout
            "--request" "POST"
            "--connect-timeout" (number->string (anthropic/connect-timeout))
            "--max-time" (number->string (if stream? (anthropic/stream-max-time)
                                             (anthropic/max-time)))
            "--header" "Expect:"                       ; no 100-continue: exactly one header block
            "--header" (string-append "@" headers-file))
      (if stream? (list "--no-buffer") '())
      (list "--data-binary" "@-" url)))
  ; deliberately absent: --fail-with-body (it collapses every HTTP error onto exit 22) and
  ; --location (a redirect would produce a second header block and could forward the key).

  (define (anthropic-curl/headers->file! headers)
    (for-each (λ (h) (anthropic-header/check! (car h) (cadr h))) headers)
    ; umask first, chmod never: create-temporary-file yields 0644 and tightening afterwards leaves
    ; a window in which the key is world readable.
    (let1 (path (let1 (saved (file-creation-mode))
                  (dynamic-wind (τ (set! (file-creation-mode) #o077))
                                (τ (create-temporary-file "anthropic"))
                                (τ (set! (file-creation-mode) saved)))))
      (with-output-to-file path
        (τ (for-each (λ (h) (display (car h)) (display ": ") (display (cadr h)) (newline)) headers)))
      path))

  (define (anthropic-curl/status-line line)
    (and (> (string-length line) 5)
         (string=? "HTTP/" (substring line 0 5))
         (let1 (i (substring-index " " line))
           (and i
                (let* ((rest (anthropic-string/trim (substring line (add1 i))))
                       (j (substring-index " " rest))
                       (v (string->number (if j (substring rest 0 j) rest))))
                  (and (exact-integer? v) (<= 100 v 599) v))))))

  (define (anthropic-curl/read-block port)
    (let1 (head (read-line port))
      (if (eof-object? head)
          (values #f '())
          (let1 (status (anthropic-curl/status-line (string-chomp head "\r")))
            (let L ((headers '()))
              (let1 (line (read-line port))
                (if (eof-object? line)
                    (values status (reverse headers))
                    (let1 (line (string-chomp line "\r"))
                      (if (string=? "" line)
                          (values status (reverse headers))
                          (let1 (h (anthropic-header/parse line))
                            (L (if h (cons h headers) headers)))))))))))) 

  ; 100 Continue and 103 Early Hints arrive as their own header blocks ahead of the real one.
  (define (anthropic-curl/read-headers port)
    (let L ()
      (receive (status headers) (anthropic-curl/read-block port)
        (if (and (exact-integer? status) (< status 200)) (L) (values status headers)))))

  (define (anthropic-transport/curl url headers body stream?)
    (let* ((path (anthropic-curl/headers->file! headers))
           (argv (anthropic-curl/argv url path stream?))
           (origin (cons (anthropic/curl) argv)))
      ; the OUTER handler covers the window in which `path` exists but `close` does not yet
      (handle-exceptions e (begin (delete-file* path) (signal e))
        (let* ((p (process* (anthropic/curl) argv))
               (stdin  (process-input-port p))   ; an OUTPUT port: curl's stdin.  Not a typo.
               (stdout (process-output-port p))  ; an INPUT port: curl's stdout.  Not a typo.
               (stderr (process-error-port p))   ; an INPUT port, and only process* reifies it
               ; no `reaped` flag: anthropic-wire/close! memoises this thunk's values, so it runs
               ; at most once and may assume so.
               (close
                 (τ
                   ; stdout FIRST.  Draining stderr while curl is still writing stdout
                   ; deadlocks on a full 64K pipe; closing stdout makes curl exit 23 instead.
                   (close-input-port stdout)
                   ; KILL BEFORE WAIT.  Closing stdout is clean only while the server is still
                   ; SENDING -- curl then gets EPIPE and exits 23.  Against a connection held
                   ; open but silent, which is what an abandoned SSE stream actually looks like,
                   ; curl is blocked reading, never writes, never notices, and a bare
                   ; process-wait blocks for the whole --max-time: 1800 s by default (§3.1).
                   ; Measured: work finished at 19 ms, the process lived 20.04 s at --max-time 20.
                   ; So probe with the nohang form and, if the child is still alive, terminate it.
                   ; process-wait is also a scheduler-wide suspend (§9), which is the second
                   ; reason not to sit in it.
                   ;
                   ; TWO spellings here are easy to get wrong and both were, in an earlier draft.
                   ; The accessor is `process-id`, NOT `process-pid` -- there is no process-pid in
                   ; CHICKEN 6 (modules.db lists process-id, and the wrong name is an unbound
                   ; variable, rc=70).  And the nohang probe answers pid 0 for a child that is
                   ; still running, NOT #f -- measured `(0 #f #f)` -- so the test must be `eqv? 0`.
                   ; `(unless pid ...)` never fires, because 0 is truthy in Scheme, and the stall
                   ; this whole step exists to prevent comes straight back.
                   (receive (pid ok? status) (process-wait p #t)
                     (when (eqv? 0 pid)
                       (handle-exceptions e (void)
                         (process-signal (process-id p) signal/term))))
                   (let1 (diagnostics (anthropic-port->string stderr))
                     (close-input-port stderr)
                     (delete-file* path)
                     ; the exit status is only populated once both ports are closed
                     (receive (pid ok? status) (process-wait p)
                       (values (if ok? status 128)
                               (anthropic/redact diagnostics)))))))
          (handle-exceptions e (begin (close) (signal e))
            ; curl buffers the whole of stdin before connecting, in order to compute
            ; Content-Length, so writing the body from this thread cannot deadlock.  Measured
            ; against a server that accepts and never reads: `display` of a 16 MB body returned
            ; in 28 ms, single-threaded, no srfi-18 pump.
            (display body stdin)
            (close-output-port stdin)
            (receive (status response-headers) (anthropic-curl/read-headers stdout)
              (unless (exact-integer? status)
                (receive (exit-status diagnostics) (close)
                  (anthropic-raise/transport (anthropic-transport/diagnosis exit-status)
                                             origin exit-status diagnostics)))
              (make-anthropic-wire status response-headers origin stdout close)))))))

  ; NOTE: this must come AFTER anthropic-transport/curl -- a parameter's initializer is evaluated
  ; at module-load time and would otherwise name an unbound identifier.
  (define anthropic/transport (make-parameter anthropic-transport/curl))
```

### 4.8b The libcurl door, deliberately left shut

A second backend was prototyped and rejected, and the reasoning is recorded here so nobody pays for
it twice. The shape that works is `curl_multi_poll` + `curl_multi_perform` driven from Scheme inside
a `make-input-port` read thunk. It is **not** `curl_easy_perform`, a blocking push model that cannot
express the contract in §2 and freezes every srfi-18 thread for the duration of the transfer; and it
is **not** a `define-external` coroutine bridge, which works mechanically and then silently replays
chunks at port level. Whatever it is, its port must supply a `read-bytevector:` hook, per §2.

It was rejected on three grounds, each checked rather than assumed. The base image
`ghcr.io/massimo-nocentini/chicken-scheme.docker:6.0.0-eggs-included` has `usr/bin/curl` but **no
libcurl headers**: listing every path of all six amd64 layers gives zero hits for `include/curl`,
`curl.h`, `libcurl.pc`, the link-time `libcurl.so` symlink, and `pkg-config`. A working `curl_multi`
prototype came to 477 lines, 313 of them C — a growable buffer with compaction, a `memchr` line
index, a 1xx header state machine, a poll/perform pump and a UTF-8 truncation decoder — which would
be the repo's largest and only *stateful* C shim, against `aux.lua.scm`'s 47-line handle wrapper.
And what it buys is about 55 ms per request (10.5 ms spawn plus a 44.6 ms fresh TLS handshake,
measured against `api.anthropic.com`) on calls that take seconds. `src/aux.egg` components are built
by one `chicken-install` invocation chained at `Dockerfile:18` as `make install && make test -B &&
cp test/*.html …`, so a compile or link failure in a new component aborts the image build before any
of the seventeen existing suites run and publishes no report for any of them.

Two arguments commonly made against the FFI are, honestly, wrong, and are not part of the reasoning
above. The "green threads freeze" objection is dead for a `curl_multi` pump — two independent
prototypes kept a ticker running through 1.8–2.0 s streams — and the same objection applies to
`process-wait`, which is on the winner's hot path (§9). And `anthropic-curl/read-headers` takes a
*port*, so feeding it `(open-input-string header-block)` keeps it and its two tests verbatim under
any backend. The real argument is the header blocker plus the size of the artefact.

If it is ever built, it plugs in as a second value for `anthropic/transport` and never replaces the
default. The stanzas, for the record, go in `src/aux.egg` **before** `(extension aux.anthropic)`,
because listing order is dependency order:

```scheme
(c-object chicken-curl (source "chicken-curl.c") (csc-options "-I/usr/local/include"))
(extension aux.curl (objects chicken-curl) (link-options "-L" "-lcurl"))
```

The separated `"-L" "-lcurl"` is correct and must not be "fixed": `csc -help:140` reads
`-L OPTION   pass option to linker`, `src/aux.egg:12` uses the identical shape to link `libstdc++`
for `aux.simdjson` in CI today, and `csc -lcurl` is rejected outright with rc=64. The `Dockerfile`
line goes before `:16` (`COPY src src`), matching the non-root `sudo` pattern at `:6`, `:11`, `:14`:

```dockerfile
RUN sudo apt-get update && sudo apt-get install -y libcurl4-openssl-dev \
    && sudo rm -rf /var/lib/apt/lists/*
```

`libcurl4-openssl-dev 8.18.0-1ubuntu2` exists in the `resolute` archive and pulls eleven further
`-dev` packages. **UNVERIFIED:** no `docker build` was ever run — docker, podman and skopeo are all
absent from the box this was checked on — so that line is untested in the image, and every FFI
prototype transcript is loopback plaintext HTTP/1.1 plus `file://`. TLS, HTTP/2 and
`api.anthropic.com` were never exercised, and the prototype's C header callback parses an
`HTTP/x.y NNN` status line it has never seen libcurl synthesise under h2.

### 4.9 The stub transport

```scheme
  ; the stub transport ------------------------------------------------------------------------
  ;
  ; The stub records the call BEFORE it answers, so a request that triggers an error is still
  ; inspectable, and it raises on an unscripted call, so a tool-loop test proves the loop
  ; terminated instead of silently looping against the last canned response.  Header VALUES come
  ; back redacted: a failing assertion over them would otherwise print a real key into the
  ; SXML->HTML report that `unittest/✓` writes.

  (define-record anthropic-stub script log)

  (define (anthropic-stub/canned body #!key (status 200)
                                            (headers '(("content-type" "application/json"))))
    (list status (map (λ (h) (list (string-downcase (car h)) (cadr h))) headers) body))

  (define (anthropic-stub/make . script) (make-anthropic-stub script '()))

  (define (anthropic-stub/calls stub)
    (map (λ (call)
           (list (car call)
                 (map (λ (h) (list (car h) (anthropic/redact (cadr h)))) (cadr call))
                 (caddr call)
                 (cadddr call)))
         (reverse (anthropic-stub-log stub))))

  (define (anthropic-stub/requests stub)
    (map (λ (call) (anthropic-json/parse (caddr call))) (reverse (anthropic-stub-log stub))))

  (define (anthropic-stub/count stub) (length (anthropic-stub-log stub)))

  (define (anthropic-stub/transport stub)
    (λ (url headers body stream?)
      (anthropic-stub-log-set! stub (cons (list url headers body stream?)
                                          (anthropic-stub-log stub)))
      (let1 (script (anthropic-stub-script stub))
        (when (null? script)
          (anthropic-raise/config
            (conc "anthropic-stub: unscripted call #" (anthropic-stub/count stub)) body))
        (anthropic-stub-script-set! stub (cdr script))
        (let1 (canned (car script))
          (make-anthropic-wire (car canned) (cadr canned) `(stub ,url)
                               (open-input-string (caddr canned))
                               (τ (values 0 "")))))))
```

### 4.10 Draining, retry and send

```scheme
  ; draining and retry -------------------------------------------------------------------------

  (define (anthropic-wire/drain! w)
    (let1 (text (anthropic-port->string (anthropic-wire-port w)))
      (receive (exit-status diagnostics) (anthropic-wire/close! w)
        (unless (zero? exit-status)
          (anthropic-raise/transport (anthropic-transport/diagnosis exit-status)
                                     (anthropic-wire-origin w) exit-status diagnostics))
        text)))

  (define (anthropic-wire->body! w)
    (let1 (text (anthropic-wire/drain! w))
      (if (anthropic-status/ok? (anthropic-wire-status w))
          text
          (anthropic-raise/api (anthropic-wire-status w) (anthropic-wire-headers w) text))))

  (define (anthropic-retry/delay attempt headers)
    (let* ((cap (anthropic/backoff-cap))
           (after (string->number (or (anthropic-header/ref headers "retry-after" #f) "")))
           (window (min cap (* (anthropic/backoff) (expt 2 attempt)))))
      (cond
        ((and (real? after) (positive? after)) (min cap (exact->inexact after)))
        ; full jitter: uniform in [0, window), which decorrelates a herd of retrying clients
        ((anthropic/backoff-jitter) (* (exact->inexact window) (/ (pseudo-random-integer 1000) 1000.0)))
        (else (exact->inexact window)))))

  (define (anthropic-send body #!key (stream #f) (betas (anthropic/betas)))
    (let ((transport (anthropic/transport))
          (url (anthropic/base-url))
          (headers (anthropic-request/headers betas: betas))
          (sleep (anthropic/sleep))
          (budget (anthropic/retries)))
      (let A ((n 0))
        (let1 (outcome
                (condition-case
                  (let1 (w (transport url headers body stream))
                    (if (anthropic-status/ok? (anthropic-wire-status w))
                        (list 'ok w)
                        (let1 (text (anthropic-wire/drain! w))
                          (anthropic-raise/api (anthropic-wire-status w)
                                               (anthropic-wire-headers w) text))))
                  (c (anthropic-error) (list 'raised c))))
          (match/first outcome
            ((ok ,w) w)
            ((raised ,c)
             (if (and (anthropic-error-retryable? c) (< n budget))
                 (begin
                   (sleep (anthropic-retry/delay n (anthropic-error-ref c 'headers '())))
                   (A (add1 n)))
                 (signal c))))))))
```

The retry loop deliberately catches only `anthropic-error`: a bug inside a user-supplied transport is not a network failure and must surface raw. It also resolves the key once per call rather than per attempt, so a key rotated mid-retry is not picked up — deliberate.

### 4.11 Messages and blocks

```scheme
  ; messages and blocks -------------------------------------------------------------------------
  ;
  ; A conversation is a LIST of message objects, oldest first.  A message is ((role r) (content c))
  ; where c is either a plain string or a VECTOR of blocks -- both shapes are legal in the same
  ; request -- and whichever we are given goes through untouched, because rewriting an assistant
  ; turn invalidates the prompt cache and, for `thinking` blocks, is a 400.

  (define (anthropic-content c)
    (cond
      ((string? c) c)
      ((null? c) #())                       ; an empty ARRAY; '() would encode as {}
      ((vector? c) c)
      ((pair? c) (list->vector c))
      (else c)))

  (define (anthropic-message role content) `((role ,role) (content ,(anthropic-content content))))
  (define (anthropic-message/user content) (anthropic-message "user" content))
  (define (anthropic-message/assistant content) (anthropic-message "assistant" content))

  (define (anthropic-message-role m) (anthropic-json/ref m 'role #f))
  (define (anthropic-message-content m) (anthropic-json/ref m 'content #()))

  ; Always a LIST of blocks: match/first has no variable-length vector pattern, so callers must
  ; never walk the raw vector, and a string `content` is lifted into one text block.
  (define (anthropic-message-blocks m)
    (let1 (c (anthropic-message-content m))
      (cond
        ((string? c) (list (anthropic-block/text c)))
        ((vector? c) (vector->list c))
        (else '()))))

  (define (anthropic-block/text text) `((type "text") (text ,text)))

  (define (anthropic-block/tool-use id name input)
    `((type "tool_use") (id ,id) (name ,name) (input ,input)))

  ; `content` is normalised here too: a LIST of blocks would otherwise be encoded as an object.
  (define (anthropic-block/tool-result tool-use-id content #!key (is-error #f))
    `((type "tool_result")
      (tool_use_id ,tool-use-id)
      (content ,(anthropic-content content))
      ,@(if is-error '((is_error #t)) '())))

  (define (anthropic-block-type b) (anthropic-json/ref b 'type #f))
  (define (anthropic-block-text b) (anthropic-json/ref b 'text ""))

  (define (anthropic-blocks/type type blocks)
    (filter (λ (b) (equal? type (anthropic-block-type b))) blocks))

  ; Three 400s turned into local errors.  NOTE: the trailing role is deliberately NOT checked
  ; here -- a pause_turn continuation legitimately ends with an assistant message.
  ; anthropic/converse checks the trailing role of the INITIAL transcript instead.
  (define (anthropic-messages/validate! messages)
    (when (null? messages)
      (anthropic-raise/config "messages must not be empty" messages))
    (let L ((ms messages) (previous #f))
      (when (pair? ms)
        (let* ((m (car ms))
               (role (anthropic-message-role m))
               (content (anthropic-message-content m)))
          (unless (member? role '("user" "assistant"))
            (anthropic-raise/config "a message role must be \"user\" or \"assistant\"" m))
          (when (and (not previous) (not (equal? "user" role)))
            (anthropic-raise/config "the first message must have role \"user\"" m))
          ; Adjacent ASSISTANT turns are legal and are what a pause_turn resume produces: the
          ; loop re-sends the paused turn and the API continues it.  Adjacent USER turns are
          ; the real 400 ("roles must alternate"), and parallel tool_results violate it, so
          ; only the assistant side is exempted.
          (when (and (equal? role previous) (equal? role "user"))
            (anthropic-raise/config "messages must not have two consecutive user turns" m))
          (when (and (vector? content) (zero? (vector-length content)))
            (anthropic-raise/config "a message may not have an empty content array" m))
          (when (and (string? content) (string=? "" content))
            (anthropic-raise/config "a message may not have empty text content" m))
          (L (cdr ms) role)))))
```

### 4.12 Request building

```scheme
  ; request building ------------------------------------------------------------------------
  ;
  ; There is no "omit this" value in the encoder, so an absent option must simply not be a key --
  ; (void) would encode as null, which is a different request.  `extra` is MERGED, not appended,
  ; so `effort: "high" extra: '((output_config ...))` produces one output_config, not two (a
  ; duplicate key is legal JSON and the server keeps the last one, silently dropping the first).

  (define (anthropic-tool-choice spec #!optional (disable-parallel #f))
    (append
      (match/first spec
        (auto '((type "auto")))
        (any  '((type "any")))
        (none '((type "none")))
        ((tool ,name) `((type "tool") (name ,name)))
        (else (anthropic-raise/config "unknown tool_choice" spec)))
      (if disable-parallel '((disable_parallel_tool_use #t)) '())))

  (define (anthropic-request/body messages
                                  #!key (model (anthropic/model))
                                        (max-tokens #f)
                                        (system #f)
                                        (tools #f)
                                        (tool-choice #f)
                                        (disable-parallel #f)
                                        (thinking #f)
                                        (effort #f)
                                        (stop-sequences #f)
                                        (stream #f)
                                        (extra '()))
    (anthropic-json/merge
      `((model ,model)
        (max_tokens ,(or max-tokens
                         (if stream (anthropic/stream-max-tokens) (anthropic/max-tokens))))
        (messages ,(list->vector messages))
        ,@(if system `((system ,system)) '())
        ; a vector of tools must NOT be silently dropped: normalize before testing.
        ,@(let1 (tools (if (vector? tools) (vector->list tools) tools))
                (if (and tools (pair? tools)) `((tools ,(anthropic-tools->json tools))) '()))
        ,@(if tool-choice
              `((tool_choice ,(anthropic-tool-choice tool-choice disable-parallel)))
              '())
        ,@(if thinking `((thinking ,thinking)) '())
        ,@(if effort `((output_config ((effort ,effort)))) '())
        ,@(if stop-sequences `((stop_sequences ,(list->vector stop-sequences))) '())
        ,@(if stream '((stream #t)) '()))
      extra))
```

`thinking` is passed through as a decoded object, e.g. `'((type "adaptive") (display "summarized"))`. `budget_tokens` is never synthesised — it is removed on current models and returns 400. `effort` goes in `output_config`, never in `thinking`. `temperature`, `top_p` and `top_k` are not keyword arguments at all: they are removed on `claude-opus-5` and 400 if sent, so a caller who really wants them on an older model must go through `extra:`.

### 4.13 The response record

```scheme
  ; responses -----------------------------------------------------------------------------------
  ;
  ; A record, because the caller wants accessors -- but it keeps the raw body string AND the
  ; decoded alist, so nothing the API grows tomorrow is lost behind the accessors we happen to have
  ; written today.  `input-errors` is ((tool_use_id raw) ...) for streamed tool_use blocks whose
  ; accumulated partial_json did not parse; it is '() on the blocking path.

  (define-record anthropic-response status headers body json input-errors)

  (define (anthropic-response/decode status headers body #!optional (input-errors '()))
    (unless (anthropic-json/well-formed? body)
      (anthropic-raise/decode "the response body is not JSON" body))
    (let1 (json (anthropic-json/parse body))
      (unless (equal? "message" (anthropic-json/ref json 'type #f))
        (anthropic-raise/decode "the response body is not a message object" body))
      (make-anthropic-response status headers body json input-errors)))

  (define (anthropic-response/of-json status headers json input-errors)
    (make-anthropic-response status headers (anthropic-json/write json) json input-errors))

  (define (anthropic-response-id r) (anthropic-json/ref (anthropic-response-json r) 'id #f))
  (define (anthropic-response-model r) (anthropic-json/ref (anthropic-response-json r) 'model #f))
  (define (anthropic-response-stop-reason r)
    (anthropic-json/ref (anthropic-response-json r) 'stop_reason #f))
  (define (anthropic-response-stop-details r)
    (anthropic-json/ref (anthropic-response-json r) 'stop_details (void)))
  (define (anthropic-response-usage r) (anthropic-json/ref (anthropic-response-json r) 'usage '()))

  (define (anthropic-response-request-id r)
    (or (anthropic-header/ref (anthropic-response-headers r) "request-id" #f)
        (let1 (v (anthropic-json/ref (anthropic-response-json r) 'request_id #f))
          (and (string? v) v))))

  ; A refusal -- a pre-output refusal in particular -- can carry an EMPTY content array, so
  ; nothing here indexes content[0].
  (define (anthropic-response-content r)
    (let1 (c (anthropic-json/ref (anthropic-response-json r) 'content #()))
      (if (vector? c) c #())))

  (define (anthropic-response/blocks r) (vector->list (anthropic-response-content r)))

  (define (anthropic-response/text r)
    (foldr/concat-strings
      (map anthropic-block-text (anthropic-blocks/type "text" (anthropic-response/blocks r)))))

  (define (anthropic-response/tool-uses r)
    (anthropic-blocks/type "tool_use" (anthropic-response/blocks r)))

  (define (anthropic-response/thinking r)
    (anthropic-blocks/type "thinking" (anthropic-response/blocks r)))

  ; Branch on stop_reason, NEVER on stop_details: the latter is informational and is null even on
  ; some refusals.
  (define (anthropic-response/refusal? r) (equal? "refusal" (anthropic-response-stop-reason r)))

  ; The assistant turn, ready to be consed onto the conversation.  It is the DECODED content,
  ; untouched, because a modified thinking block is a 400.
  (define (anthropic-response/message r)
    (anthropic-message/assistant (anthropic-response-content r)))

  (define (anthropic-usage/ref r key #!optional (default 0))
    (let1 (v (anthropic-json/ref (anthropic-response-usage r) key default))
      (if (void? v) default v)))

  (define (anthropic-usage/input-tokens r) (anthropic-usage/ref r 'input_tokens))
  (define (anthropic-usage/output-tokens r) (anthropic-usage/ref r 'output_tokens))
  (define (anthropic-usage/cache-read-tokens r) (anthropic-usage/ref r 'cache_read_input_tokens))
  (define (anthropic-usage/cache-creation-tokens r)
    (anthropic-usage/ref r 'cache_creation_input_tokens))

  ; input_tokens is the UNCACHED remainder only; the total prompt size is the sum of the three.
  (define (anthropic-usage/prompt-tokens r)
    (+ (anthropic-usage/input-tokens r)
       (anthropic-usage/cache-creation-tokens r)
       (anthropic-usage/cache-read-tokens r)))
```

### 4.14 Tools

```scheme
  ; tools, as data ------------------------------------------------------------------------------

  (define-record anthropic-tool name description schema strict procedure)
  (define-record anthropic-tool-failure message)

  (define (anthropic-tool/error message) (make-anthropic-tool-failure message))

  ; The registry is a convenience for `anthropic-tool/registered`.  The loop never reads it -- it
  ; takes an EXPLICIT tools list -- so a test file that defines a tool cannot affect another.  Two
  ; tools with the same JSON name overwrite each other silently, which is what makes reloading a
  ; file in the REPL work.
  (define anthropic-tool/registry (make-hash-table))

  (define (anthropic-tool/register! tool)
    (hash-table-set! anthropic-tool/registry (anthropic-tool-name tool) tool)
    tool)

  (define (anthropic-tool/registered name)
    (hash-table-ref/default anthropic-tool/registry name #f))

  (define (anthropic-tool->json tool)
    `((name ,(anthropic-tool-name tool))
      (description ,(anthropic-tool-description tool))
      (input_schema ,(anthropic-tool-schema tool))
      ,@(if (anthropic-tool-strict tool) '((strict #t)) '())))

  (define (anthropic-tools->json tools)
    (list->vector
      (map (λ (t) (if (anthropic-tool? t) (anthropic-tool->json t) t))
           (if (vector? tools) (vector->list tools) tools))))

  (define (anthropic-tools->table tools)
    (let1 (H (make-hash-table))
      (for-each (λ (t) (when (anthropic-tool? t)
                         (hash-table-set! H (anthropic-tool-name t) t)))
                tools)
      H))

  (define (anthropic-tool/input-ref input key name)
    (if (anthropic-json/has? input key)
        (anthropic-json/ref input key)
        (anthropic-raise/tool (conc "tool " name ": missing required argument " key)
                              `((tool ,name) (argument ,(symbol->string key)) (input ,input)))))

  (define (anthropic-tool/apply tool input) ((anthropic-tool-procedure tool) input))

  ; One rule, no magic: a non-empty string goes on the wire verbatim, anything else is JSON.
  ; This is the single choke point where tool output -- arbitrary program bytes -- meets the
  ; encoder, and it is why the encoder had to grow a real escaper.
  (define (anthropic-tool-result->string v)
    (cond
      ((and (string? v) (positive? (string-length v))) v)
      ((string? v) "(the tool returned an empty string)")
      ((void? v) "(the tool returned no value)")
      (else (anthropic-json/write v))))
```

The macro. It is an ir-macro because it folds one variable-length spec list into two parallel results — `properties` and `required` — which `syntax-rules` can express only as an unreadable CPS accumulator. `src/aux.category.monad.scm:53` is the closest existing template.

```scheme
  ; define-tool ---------------------------------------------------------------------------------
  ;
  ;   (define-tool (NAME (param TYPE "doc" opt ...) ...) "what the tool does" body ...)
  ;
  ;   TYPE ::= string | integer | number | boolean
  ;          | (enum v ...)                 ; the JSON type is derived from the literals
  ;          | (array-of TYPE)              ; {"type":"array","items":{...}}
  ;          | (object (p TYPE "doc" opt ...) ...)
  ;          | (raw EXPR)                   ; EXPR is a runtime alist, SPLICED in
  ;   opt  ::= optional | (default EXPR)
  ;
  ; Expands to TWO definitions: the plain procedure NAME (required params positional, optional
  ; ones #!key with their declared defaults, so it stays callable and unit-testable from Scheme
  ; with no JSON in sight) and the record NAME/tool, whose dispatcher maps a decoded input object
  ; onto that same keyword call.  An absent optional key is simply not passed, so the procedure's
  ; own default is the single source of truth.
  ;
  ; Three encoder traps are handled at expansion time.  Empty `properties` is '() -> {} while
  ; empty `required` is #() -> [] -- different Scheme values, and getting them the wrong way round
  ; is a 400 with no other symptom.  The formals use the UNSTRIPPED identifiers, because stripped
  ; ones do not bind the body.  And a tool name is checked against ^[a-zA-Z0-9_-]{1,128}$ here, so
  ; a bad name is a macro error rather than an opaque 400 -- the one place this module cannot
  ; follow the house taste for Unicode identifiers.

  (define-syntax-rule (define-tool head body ...) (anthropic-define-tool* #f head body ...))
  (define-syntax-rule (define-tool/strict head body ...) (anthropic-define-tool* #t head body ...))

  (define-macro-ir (anthropic-define-tool* expr inject compare)
    (let* ((strict? (cadr expr))
           (head (caddr expr))
           (forms (cdddr expr))
           (id (car head))                               ; UNSTRIPPED: it binds the procedure
           (name (strip-syntax id))
           (name* (symbol->string name))
           (specs (cdr head))
           (description (if (pair? forms) (car forms) #f))
           (procbody (if (pair? forms) (cdr forms) '()))
           (spec-id    (λ (s) (car s)))
           (spec-name  (λ (s) (strip-syntax (car s))))
           (spec-type  (λ (s) (cadr s)))
           (spec-doc   (λ (s) (caddr s)))
           (spec-opts  (λ (s) (cdddr s)))
           (opt-tag    (λ (o) (strip-syntax (if (pair? o) (car o) o))))
           (optional?  (λ (s) (and (find (λ (o) (member? (opt-tag o) '(optional default)))
                                         (spec-opts s))
                                   #t)))
           (default-of (λ (s) (let1 (d (find (λ (o) (and (pair? o) (eq? 'default (opt-tag o))))
                                             (spec-opts s)))
                                (if d (cadr d) '(void))))))
      (letrec
        ((enum-type
           (λ (vs)
             (cond
               ((every string? vs) "string")
               ((every exact-integer? vs) "integer")
               ((every real? vs) "number")
               (else (error (conc "define-tool: an (enum ...) must be all strings, all exact "
                                  "integers or all reals\n\n" (->string/pretty-print vs)))))))
         (T (λ (type)
              (match/first (strip-syntax type)
                (string  '((type "string")))
                (integer '((type "integer")))
                (number  '((type "number")))
                (boolean '((type "boolean")))
                ((enum . ,vs) `((type ,(enum-type vs)) (enum ,(list->vector vs))))
                ((array-of _) `((type "array") (items ,(T (cadr type)))))
                ((object . _) (O (cdr type)))
                ; `raw` keeps the USER's expression, unstripped, and SPLICES, so the generated
                ; (description ...) still lands after the user's fragment.
                ((raw _) (list (list 'unquote-splicing (cadr type))))
                (else (error (conc "define-tool: unknown parameter type\n\n"
                                   (->string/pretty-print (strip-syntax type))))))))
         (P (λ (s) (list (spec-name s)
                         (append (T (spec-type s)) (list (list 'description (spec-doc s)))))))
         (O (λ (ss)
              `((type "object")
                (properties ,(map P ss))
                (required ,(list->vector (map (λ (s) (symbol->string (spec-name s)))
                                              (remove optional? ss))))
                ,@(if strict? '((additionalProperties #f)) '())))))
        (unless (string? description)
          (error (conc "define-tool: the description of " name* " must be a string literal")))
        (unless (pair? procbody)
          (error (conc "define-tool: " name* " has no body")))
        (unless (and (<= 1 (string-length name*) 128)
                     (every (λ (c) (or (char<=? #\a c #\z) (char<=? #\A c #\Z)
                                       (char<=? #\0 c #\9) (memv c (list #\_ #\-))))
                            (string->list name*)))
          (error (conc "define-tool: a tool name must match ^[a-zA-Z0-9_-]{1,128}$\n\n" name*)))
        (let* ((required (remove optional? specs))
               (optional (filter optional? specs))
               ; the schema is built as a DATUM and wrapped by hand: writing (quasiquote ,(O ...))
               ; inside the outer template would raise the nesting level and never evaluate it.
               (schema (list 'quasiquote (O specs)))
               (formals (append (map spec-id required)
                                (if (null? optional)
                                    '()
                                    (cons '#!key
                                          (map (λ (s) (list (spec-id s) (default-of s)))
                                               optional)))))
               (args `(append
                        (list ,@(map (λ (s) `(anthropic-tool/input-ref
                                               input (quote ,(spec-name s)) ,name*))
                                     required))
                        ,@(map (λ (s) `(if (anthropic-json/has? input (quote ,(spec-name s)))
                                           (list (string->keyword ,(symbol->string (spec-name s)))
                                                 (anthropic-json/ref input (quote ,(spec-name s))))
                                           '()))
                               optional))))
          `(begin
             (define (,id ,@formals) ,@procbody)
             (define ,(inject (string->symbol (string-append name* "/tool")))
               (anthropic-tool/register!
                 (make-anthropic-tool ,name* ,description ,schema ,strict?
                                      (λ (input) (apply ,id ,args)))))
             (void))))))
```

### 4.15 Blocking entry points

```scheme
  ; entry points ---------------------------------------------------------------------------------

  (define (anthropic/messages messages
                              #!key (model (anthropic/model))
                                    (max-tokens #f)
                                    (system #f)
                                    (tools #f)
                                    (tool-choice #f)
                                    (disable-parallel #f)
                                    (thinking #f)
                                    (effort #f)
                                    (stop-sequences #f)
                                    (extra '())
                                    (betas (anthropic/betas)))
    (anthropic-messages/validate! messages)
    (let* ((body (anthropic-json/write
                   (anthropic-request/body messages
                                           model: model max-tokens: max-tokens system: system
                                           tools: tools tool-choice: tool-choice
                                           disable-parallel: disable-parallel
                                           thinking: thinking effort: effort
                                           stop-sequences: stop-sequences extra: extra)))
           (w (anthropic-send body betas: betas))
           (text (anthropic-wire->body! w)))
      (anthropic-response/decode (anthropic-wire-status w) (anthropic-wire-headers w) text)))

  ; string in, string out.
  (define (anthropic/ask prompt . kwargs)
    (anthropic-response/text
      (apply anthropic/messages (list (anthropic-message/user prompt)) kwargs)))
```

### 4.16 SSE framing and the accumulator

```scheme
  ; SSE framing -----------------------------------------------------------------------------------
  ;
  ; Wire format: "event: <name>", then one or more "data: <text>", then a blank line.  A line
  ; beginning with ':' is a comment.  read-line over a pipe already strips the newline; we chomp
  ; \r too.  An event whose data is absent or unparseable keeps data = (void) rather than raising
  ; here: the wire carries comments, pings and event types that do not exist yet, and none of
  ; those may kill a stream.  Only the accumulator raises, and only for the events it understands.

  (define-record anthropic-event name data raw)

  (define (anthropic-sse/field line prefix)
    (and (eqv? 0 (substring-index prefix line))
         (let1 (v (substring line (string-length prefix)))
           (if (and (positive? (string-length v)) (char=? #\space (string-ref v 0)))
               (substring v 1)
               v))))

  ; `(or name "message")` is WRONG and was the first draft: an `event:` line with an empty field
  ; yields the string "", which is truthy in Scheme, so the frame would be named "" instead of
  ; taking the default.  The SSE specification says an empty event field means the default event
  ; type, so the test is explicit.
  (define (anthropic-sse/frame name data)
    (let1 (raw (string-intersperse (reverse data) "\n"))
      (make-anthropic-event (if (and (string? name) (not (string=? name ""))) name "message")
                            (if (anthropic-json/well-formed? raw) (anthropic-json/parse raw) (void))
                            raw)))

  (define (anthropic-sse/read port)
    (let F ((name #f) (data '()))
      (let1 (line (read-line port))
        (cond
          ((eof-object? line)
           (if (and (not name) (null? data)) line (anthropic-sse/frame name data)))
          (else
            (let1 (line (string-chomp line "\r"))
              (cond
                ((string=? "" line)
                 (if (and (not name) (null? data)) (F #f '()) (anthropic-sse/frame name data)))
                ((eqv? 0 (substring-index ":" line)) (F name data))
                ((anthropic-sse/field line "event:") => (λ (v) (F v data)))
                ((anthropic-sse/field line "data:")  => (λ (v) (F name (cons v data))))
                (else (F name data)))))))))

  ; consumption style 1: a lazy stream, so take§ / map§ / §->list from (aux stream) all work on
  ; it.  `finish` runs exactly once, at eof, because δ memoises; a caller who abandons the stream
  ; early must release the wire itself.
  (define (anthropic-sse->§ port #!key (finish void))
    (let F ()
      (δ (let1 (ev (anthropic-sse/read port))
           (if (eof-object? ev) (begin (finish) '()) (cons ev (F)))))))

  ; consumption style 2: a strict, tail-recursive left fold.  (aux stream) has only foldr§, a
  ; non-tail RIGHT fold, which cannot drive a state machine over a live socket.
  (define (anthropic-sse/fold f seed §)
    (let F ((§ §) (acc seed))
      (cond
        ((promise? §) (F (force §) acc))
        ((pair? §) (F (cdr §) (f (car §) acc)))
        (else acc))))

  (define (anthropic-sse/events port)
    (let L ((acc '()))
      (let1 (ev (anthropic-sse/read port))
        (if (eof-object? ev) (reverse acc) (L (cons ev acc))))))

  (define (anthropic-event/text ev)
    (and (equal? "content_block_delta" (anthropic-event-name ev))
         (let1 (delta (anthropic-json/ref (anthropic-event-data ev) 'delta '()))
           (and (equal? "text_delta" (anthropic-json/ref delta 'type ""))
                (anthropic-json/ref delta 'text "")))))

  ; the accumulator ---------------------------------------------------------------------------
  ;
  ; One block accumulator per content-block INDEX -- index is the only thing tying a delta to a
  ; block, and a real turn has thinking at 0, text at 1 and tool_use at 2.  `text` and `signature`
  ; are reversed lists of fragments; `json` is a reversed list of partial_json fragments that stays
  ; a STRING until content_block_stop, because feeding fragments to a JSON parser incrementally
  ; cannot work and, with this binding, would abort the process.

  (define-record anthropic-sse-block index template text json signature)
  (define-record anthropic-sse-state message open done errors stopped)

  (define (anthropic-accumulator/fresh) (make-anthropic-sse-state '() '() '() '() #f))

  (define anthropic-sse/structural-events
    '("message_start" "content_block_start" "content_block_delta" "content_block_stop"
      "message_delta" "error"))

  ; The template is the content_block object as it arrived; only the field the deltas were filling
  ; is replaced.  An unknown block type passes through untouched, which is what the
  ; replay-verbatim rule requires.  A tool_use block with NO input_json_delta events keeps its
  ; template's `input` ({} -> '()); an unparseable accumulation is recorded on the state and the
  ; input is left empty, because raising here would lose the sibling blocks, the stop_reason and
  ; the usage of the whole turn.
  (define (anthropic-sse-block->json blk st)
    (let* ((template (anthropic-sse-block-template blk))
           (text (foldr/concat-strings (reverse (anthropic-sse-block-text blk))))
           (signature (foldr/concat-strings (reverse (anthropic-sse-block-signature blk))))
           (raw (foldr/concat-strings (reverse (anthropic-sse-block-json blk)))))
      (match/first (anthropic-json/ref template 'type "text")
        ("text" (anthropic-json/set template 'text text))
        ("thinking"
         (let1 (t (anthropic-json/set template 'thinking text))
           (if (string=? "" signature) t (anthropic-json/set t 'signature signature))))
        ("redacted_thinking" template)
        (else
          (cond
            ((null? (anthropic-sse-block-json blk))
             (if (null? (anthropic-sse-block-text blk))
                 template
                 (anthropic-json/set template 'text text)))
            ((anthropic-json/well-formed? raw)
             (anthropic-json/set template 'input (anthropic-json/parse raw)))
            (else
              (anthropic-sse-state-errors-set!
                st (cons (list (anthropic-json/ref template 'id #f) raw)
                         (anthropic-sse-state-errors st)))
              (anthropic-json/set template 'input '())))))))

  (define (anthropic-accumulator/event st ev)
    (let* ((name (anthropic-event-name ev))
           (data (anthropic-event-data ev))
           (index (anthropic-json/ref data 'index -1)))
      (when (and (void? data) (member? name anthropic-sse/structural-events))
        (anthropic-raise/sse (conc "the " name " frame carries no decodable data")
                             (anthropic-event-raw ev)))
      (match/first name

        ("message_start"
         (anthropic-sse-state-message-set! st (anthropic-json/ref data 'message '()))
         st)

        ("content_block_start"
         (anthropic-sse-state-open-set!
           st (cons (cons index (make-anthropic-sse-block
                                  index (anthropic-json/ref data 'content_block '()) '() '() '()))
                    (anthropic-sse-state-open st)))
         st)

        ("content_block_delta"
         (let1 (cell (assv index (anthropic-sse-state-open st)))
           (when (pair? cell)
             (let* ((blk (cdr cell)) (delta (anthropic-json/ref data 'delta '())))
               (match/first (anthropic-json/ref delta 'type "")
                 ("text_delta"
                  (anthropic-sse-block-text-set!
                    blk (cons (anthropic-json/ref delta 'text "") (anthropic-sse-block-text blk))))
                 ("thinking_delta"
                  (anthropic-sse-block-text-set!
                    blk (cons (anthropic-json/ref delta 'thinking "")
                              (anthropic-sse-block-text blk))))
                 ("input_json_delta"
                  (anthropic-sse-block-json-set!
                    blk (cons (anthropic-json/ref delta 'partial_json "")
                              (anthropic-sse-block-json blk))))
                 ("signature_delta"
                  (anthropic-sse-block-signature-set!
                    blk (cons (anthropic-json/ref delta 'signature "")
                              (anthropic-sse-block-signature blk))))
                 (else (void)))))
           st))

        ("content_block_stop"
         (let1 (cell (assv index (anthropic-sse-state-open st)))
           (when (pair? cell)
             (let1 (finished (anthropic-sse-block->json (cdr cell) st))
               (anthropic-sse-state-open-set!
                 st (remove (λ (c) (eqv? index (car c))) (anthropic-sse-state-open st)))
               (anthropic-sse-state-done-set!
                 st (cons (cons index finished) (anthropic-sse-state-done st)))))
           st))

        ; stop_reason and stop_details ride on message_delta's delta, and its usage carries only
        ; output_tokens, so both are MERGED into what message_start supplied, never substituted.
        ("message_delta"
         (let* ((m (anthropic-json/merge (anthropic-sse-state-message st)
                                         (anthropic-json/ref data 'delta '())))
                (usage (anthropic-json/ref data 'usage '()))
                (m (if (pair? usage)
                       (anthropic-json/set m 'usage
                                           (anthropic-json/merge (anthropic-json/ref m 'usage '())
                                                                 usage))
                       m)))
           (anthropic-sse-state-message-set! st m)
           st))

        ("message_stop" (anthropic-sse-state-stopped-set! st #t) st)
        ("ping" st)

        ("error"
         (let1 (err (anthropic-json/ref data 'error '()))
           (signal (anthropic-condition
                     'anthropic-api-error
                     (equal? "overloaded_error" (anthropic-json/ref err 'type ""))
                     (anthropic-message/error "stream error"
                                              `((type ,(anthropic-json/ref err 'type "api_error"))
                                                (message ,(anthropic-json/ref err 'message ""))))
                     `(status ,(void)
                       type ,(anthropic-json/ref err 'type "api_error")
                       api-message ,(anthropic-json/ref err 'message "")
                       request-id #f headers () body ,(anthropic-event-raw ev))))))

        (else st))))                       ; an unknown event name is ignored, by design

  (define (anthropic-accumulator->message st)
    (let1 (blocks (sort (reverse (anthropic-sse-state-done st)) (λ (a b) (< (car a) (car b)))))
      (anthropic-json/set (anthropic-sse-state-message st) 'content
                          (list->vector (map cdr blocks)))))

  ; Returns the assembled message AND the ((tool_use_id raw) ...) of any tool input that did not
  ; parse.  Raises when a block was left open or message_stop never arrived -- that is how a
  ; connection cut at 90% of a long answer becomes an error instead of a plausible short answer.
  (define (anthropic-sse->message § #!key (on-event void))
    (let1 (st (anthropic-sse/fold (λ (ev acc) (on-event ev) (anthropic-accumulator/event acc ev))
                                  (anthropic-accumulator/fresh)
                                  §))
      (unless (null? (anthropic-sse-state-open st))
        (anthropic-raise/sse "the stream ended with an unfinished content block"
                             (map car (anthropic-sse-state-open st))))
      (unless (anthropic-sse-state-stopped st)
        (anthropic-raise/sse "the stream ended without a message_stop event"
                             (anthropic-sse-state-message st)))
      (values (anthropic-accumulator->message st) (reverse (anthropic-sse-state-errors st)))))
```

The streamed message is assembled by **replacing keys in place** in the `message_start` skeleton, so every key the server sent keeps its position and `content`, `stop_reason` and `usage` land where the blocking decode puts them. It is deliberately **not** claimed to be `equal?` to a blocking decode: `message_delta` may introduce a key the skeleton lacked (it appends), and the skeleton may lack a key the blocking body has. The two are *accessor*-compatible — every procedure in §3.7 works on both — and §6 compares through accessors, not raw `equal?`.

### 4.17 The streaming entry point

```scheme
  ; streaming ------------------------------------------------------------------------------------
  ;
  ; NOT retried.  A retry re-POSTs the whole request, which for a partially consumed stream would
  ; replay tokens the caller has already seen; a caller who wants retries must wrap this itself and
  ; discard what it has shown.  The dynamic-wind is load bearing: the accumulator can raise from
  ; the middle of the fold (an `error` frame, an undecodable structural frame), and without it the
  ; curl child and the 0600 file holding the API key would leak until the process exits.

  (define (anthropic/stream messages
                            #!key (model (anthropic/model))
                                  (max-tokens #f)
                                  (system #f)
                                  (tools #f)
                                  (tool-choice #f)
                                  (disable-parallel #f)
                                  (thinking #f)
                                  (effort #f)
                                  (stop-sequences #f)
                                  (extra '())
                                  (betas (anthropic/betas))
                                  (on-event void)
                                  (on-text void))
    (anthropic-messages/validate! messages)
    (let* ((body (anthropic-json/write
                   (anthropic-request/body messages
                                           model: model max-tokens: max-tokens system: system
                                           tools: tools tool-choice: tool-choice
                                           disable-parallel: disable-parallel
                                           thinking: thinking effort: effort
                                           stop-sequences: stop-sequences
                                           stream: #t extra: extra)))
           (transport (anthropic/transport))
           (w (transport (anthropic/base-url) (anthropic-request/headers betas: betas) body #t)))
      ; a non-2xx on a streaming request answers with an ordinary JSON error body, not with SSE
      (unless (anthropic-status/ok? (anthropic-wire-status w)) (anthropic-wire->body! w))
      (let1 (ct (anthropic-header/ref (anthropic-wire-headers w) "content-type" ""))
        (unless (substring-index "text/event-stream" ct)
          (anthropic-wire/close! w)
          (anthropic-raise/decode "a streaming request did not get an SSE response" ct)))
      (let ((message #f) (errors '()) (exit-status 0) (diagnostics ""))
        (dynamic-wind
          (τ (void))
          (τ (receive (m e)
                 (anthropic-sse->message
                   (anthropic-sse->§ (anthropic-wire-port w))
                   on-event: (λ (ev)
                               (let1 (t (anthropic-event/text ev)) (when t (on-text t)))
                               (on-event ev)))
               (set! message m)
               (set! errors e)))
          (τ (receive (s d) (anthropic-wire/close! w)
               (set! exit-status s)
               (set! diagnostics d))))
        (unless (zero? exit-status)
          (anthropic-raise/transport (anthropic-transport/diagnosis exit-status)
                                     (anthropic-wire-origin w) exit-status diagnostics))
        (anthropic-response/of-json (anthropic-wire-status w) (anthropic-wire-headers w)
                                    message errors))))
```

### 4.18 The agentic loop

```scheme
  ; the agentic loop -------------------------------------------------------------------------------
  ;
  ; Loop until end_turn, always append the full response content to preserve tool_use and thinking
  ; blocks, and make sure every tool_result echoes its tool_use_id.  The stop_reason is checked
  ; BEFORE the dispatch, so a truncated or refused turn never executes a side-effecting tool on
  ; partial arguments.  All the results of one assistant turn go back in ONE user message: a second
  ; user message is a 400 on the alternation rule, and an unanswered tool_use_id is a 400 too, so
  ; a failing tool is answered with is_error rather than dropped.

  ; around-advice for one tool call: call (run) to execute it, or return
  ; (anthropic-tool/error "why") to DENY it without executing anything.  Returning a plain string
  ; denies nothing -- the string becomes a SUCCESSFUL tool result.  That is easy to get wrong in
  ; an approval hook, which is exactly where getting it wrong is dangerous.
  (define (anthropic-tool/around name input run) (run))

  (define (anthropic-tool-use->result block table around input-errors)
    (let* ((id (anthropic-json/ref block 'id #f))
           (name (anthropic-json/ref block 'name #f))
           (input (anthropic-json/ref block 'input '()))
           (broken (assoc id input-errors))
           (tool (hash-table-ref/default table name #f))
           (outcome
             (cond
               ; the documented shape: a JSON document nested inside a JSON string, built with the
               ; encoder rather than by concatenation so quotes in the bad input are escaped
               ((pair? broken)
                (anthropic-tool/error (anthropic-json/write `((INVALID_JSON ,(cadr broken))))))
               ((not tool) (anthropic-tool/error (conc "no such tool: " name)))
               (else
                 (condition-case (around name input (τ (anthropic-tool/apply tool input)))
                   (c (exn) (anthropic-tool/error
                              (anthropic/redact
                                (letport/output-string p (print-error-message c p)))))
                   (c () (anthropic-tool/error (->string/pretty-print c)))))))
           (failed? (anthropic-tool-failure? outcome)))
      (anthropic-block/tool-result
        id
        (if failed?
            (anthropic-tool-failure-message outcome)
            (anthropic-tool-result->string outcome))
        is-error: failed?)))

  ; never append an assistant turn with an EMPTY content array: that is a 400 on the next request
  (define (anthropic-converse/append transcript r)
    (let1 (content (anthropic-response-content r))
      (if (zero? (vector-length content))
          transcript
          (append transcript (list (anthropic-message/assistant content))))))

  (define (anthropic/converse messages
                              #!key (tools '())
                                    (model (anthropic/model))
                                    (max-tokens #f)
                                    (system #f)
                                    (tool-choice #f)
                                    (disable-parallel #f)
                                    (thinking #f)
                                    (effort #f)
                                    (stop-sequences #f)
                                    (extra '())
                                    (betas (anthropic/betas))
                                    (max-iterations 8)
                                    (max-pauses 5)
                                    (around anthropic-tool/around)
                                    (on-response (λ_ (void))))
    (anthropic-messages/validate! messages)
    ; assistant prefill is a 400 on every current model; the INITIAL transcript must end with the
    ; user.  A pause_turn continuation legitimately does not, which is why this lives here and not
    ; in anthropic-messages/validate!.
    (unless (equal? "user" (anthropic-message-role (last messages)))
      (anthropic-raise/config "the initial transcript must end with a user message"
                              (last messages)))
    (let1 (table (anthropic-tools->table tools))
      (let L ((transcript messages) (request 1) (pauses 0))
        (when (> request max-iterations)
          ; #f, not an alist: this fires BEFORE the request, so there is no response to carry.
          ; Every other call site passes the `r` it just decoded; see §3.2.
          (anthropic-raise/loop 'max-iterations
                                (conc "too many requests in one conversation (max-iterations "
                                      max-iterations ")")
                                #f))
        (let* ((r (anthropic/messages transcript
                                      model: model max-tokens: max-tokens system: system
                                      tools: (if (pair? tools) tools #f)
                                      tool-choice: tool-choice
                                      disable-parallel: disable-parallel
                                      thinking: thinking effort: effort
                                      stop-sequences: stop-sequences extra: extra betas: betas))
               (content (anthropic-response-content r)))
          (on-response r request)
          ; branch on stop_reason ONLY -- stop_details can be null even on a refusal -- and branch
          ; BEFORE dispatching
          (match/first (anthropic-response-stop-reason r)

            ("end_turn"      (values r (anthropic-converse/append transcript r)))
            ("stop_sequence" (values r (anthropic-converse/append transcript r)))

            ("tool_use"
             (let1 (uses (anthropic-response/tool-uses r))
               (when (null? uses)
                 (anthropic-raise/loop
                   'no-tool-use "stop_reason is tool_use but no tool_use block was returned" r))
               ; an explicit left-to-right fold, not `map`: application order is unspecified and
               ; these calls have side effects
               (let1 (results (let R ((bs uses) (acc '()))
                                (if (null? bs)
                                    (reverse acc)
                                    (R (cdr bs)
                                       (cons (anthropic-tool-use->result
                                               (car bs) table around
                                               (anthropic-response-input-errors r))
                                             acc)))))
                 (L (append (anthropic-converse/append transcript r)
                            (list (anthropic-message/user (list->vector results))))
                    (add1 request)
                    pauses))))

            ; the one branch that appends an assistant turn with NO following user message: the
            ; API detects the trailing server_tool_use block and resumes.  Do NOT inject a
            ; "Continue." user message.
            ("pause_turn"
             (when (>= pauses max-pauses)
               (anthropic-raise/loop 'max-pauses "too many pause_turn continuations" r))
             (when (zero? (vector-length content))
               (anthropic-raise/loop 'empty-pause "pause_turn with an empty content array" r))
             (L (anthropic-converse/append transcript r) (add1 request) (add1 pauses)))

            ; a truncated tool input often still parses as a valid partial object, so a truncated
            ; turn that carries a tool_use must never be executed -- retry with a bigger budget
            ("max_tokens"
             (if (null? (anthropic-response/tool-uses r))
                 (values r (anthropic-converse/append transcript r))
                 (anthropic-raise/loop
                   'max-tokens "the turn was truncated while a tool call was being written" r)))

            ("refusal" (anthropic-raise/loop 'refusal "the model refused to continue" r))

            ("model_context_window_exceeded"
             (anthropic-raise/loop 'context-window-exceeded
                                   "the conversation exceeded the model's context window" r))

            (else
              (anthropic-raise/loop
                'unknown-stop-reason
                (conc "unknown stop_reason: " (->string (anthropic-response-stop-reason r)))
                r)))))))

  )
```

That closing `  )` is the last line of the file.

---

## 5. Worked usage examples

All four run against the real API when `ANTHROPIC_API_KEY` is set; all four run offline when `anthropic/transport` is parameterized to a stub.

### 5.1 One-liner ask

```scheme
(import (aux anthropic))

(anthropic/ask "What is the capital of France?")
;; => "Paris."

; with options, still one line
(anthropic/ask "Write a haiku about Scheme." model: "claude-haiku-4-5" max-tokens: 4000)
```

The request that goes on the wire is exactly

```json
{"model":"claude-opus-5","max_tokens":16000,
 "messages":[{"role":"user","content":"What is the capital of France?"}]}
```

with headers `content-type: application/json`, `x-api-key: …`, `anthropic-version: 2023-06-01`. No `anthropic-beta` (betas is empty), no `thinking` (omitting it still runs adaptive on `claude-opus-5`), no `temperature`.

### 5.2 Multi-turn with a system prompt

`system` is a **top-level sibling** of `messages`, never a message with role `"system"`.

```scheme
(import (aux base) (aux anthropic))

(let1 (transcript (list (anthropic-message/user "Name one Scheme implementation with delimited continuations.")))
  (let1 (r (anthropic/messages transcript
                               system: "You are terse. Answer in at most one sentence."
                               max-tokens: 2000
                               effort: "low"))
    (print "assistant: " (anthropic-response/text r))
    (print "tokens: prompt " (anthropic-usage/prompt-tokens r)
           " output " (anthropic-usage/output-tokens r))

    ; the assistant turn goes back VERBATIM -- the decoded content, untouched, so thinking blocks
    ; and any block type this module has never heard of survive the round trip
    (let1 (transcript (append transcript
                              (list (anthropic-response/message r)
                                    (anthropic-message/user "And one that does not?"))))
      (anthropic-response/text
        (anthropic/messages transcript
                            system: "You are terse. Answer in at most one sentence."
                            max-tokens: 2000
                            effort: "low")))))
```

Mixing block shapes in one conversation is legal and supported: a user turn may be a plain string on one message and a vector of blocks on the next.

```scheme
(anthropic/messages
  (list (anthropic-message/user "Compare these two.")
        (anthropic-message/assistant (vector (anthropic-block/text "Compare what?")))
        (anthropic-message/user (list (anthropic-block/text "Racket")
                                      (anthropic-block/text "Chicken")))))
```

### 5.3 A tool, defined with the macro and driven through the loop

```scheme
(import (aux base) (aux anthropic) (chicken string))

(define-tool (get_weather
               (location string "The city and state, e.g. San Francisco, CA")
               (unit (enum "celsius" "fahrenheit") "Unit of temperature" (default "celsius")))
  "Get the current weather in a given location."
  (conc "22 " unit " in " location))

(define-tool/strict (send_email
                      (to (array-of string) "Recipient addresses")
                      (message (object (subject string "Subject line")
                                       (body string "Plain-text body")
                                       (draft? boolean "Do not actually send" (default #t)))
                               "The email to send"))
  "Send an email."
  (conc "queued " (vector-length to) " message(s)"))

(define-tool (now) "Return the current time as an ISO-8601 string." "2026-09-16T00:00:00Z")
```

`get_weather` stays an ordinary Scheme procedure — `(get_weather "Paris")` → `"22 celsius in Paris"`, `(get_weather "Paris" unit: "fahrenheit")` → `"22 fahrenheit in Paris"` — and `get_weather/tool` is the reified record. The derived `input_schema` for `get_weather` is

```json
{"type":"object",
 "properties":{"location":{"type":"string","description":"The city and state, e.g. San Francisco, CA"},
               "unit":{"type":"string","enum":["celsius","fahrenheit"],"description":"Unit of temperature"}},
 "required":["location"]}
```

and for the parameterless `now` it is `{"type":"object","properties":{},"required":[]}` — note `properties` is `'()` → `{}` while `required` is `#()` → `[]`; they are different Scheme values and swapping them is a 400 with no other symptom. `send_email` additionally carries `"additionalProperties":false` on **both** objects and a top-level `"strict":true`, because strict demands them together.

Driving the loop:

```scheme
(receive (response transcript)
    (anthropic/converse (list (anthropic-message/user "What is the weather in Paris and in Rome?"))
                        tools: (list get_weather/tool send_email/tool now/tool)
                        max-tokens: 8000
                        max-iterations: 6
                        on-response: (λ (r n) (print "-- request " n ": " (anthropic-response-stop-reason r))))
  (print (anthropic-response/text response))
  (print "turns: " (map anthropic-message-role transcript)))
```

Two `tool_use` blocks in one assistant turn produce **one** user message holding two `tool_result` blocks, in block order, each echoing its `tool_use_id`. A tool that raises, or a name with no matching tool, comes back as a `tool_result` with `is_error #t` — never dropped, because an unanswered `tool_use_id` is a 400 on the next request.

An approval gate is the `around:` hook. Returning `(anthropic-tool/error …)` denies; calling `(run)` executes. Returning a plain string denies nothing — it becomes a successful result.

```scheme
(anthropic/converse (list (anthropic-message/user "Email the summary to ops@example.com."))
                    tools: (list send_email/tool)
                    around: (λ (name input run)
                              (print "tool: " name " " (anthropic-json/write input))
                              (if (member? name '("send_email"))
                                  (anthropic-tool/error "denied: e-mail requires human approval")
                                  (run))))
```

And the whole loop runs offline against canned bytes, with no network and no key, because the transport is the only swap point:

```scheme
(let1 (stub (anthropic-stub/make
              (anthropic-stub/canned
                (string-append
                  "{\"id\":\"msg_1\",\"type\":\"message\",\"role\":\"assistant\","
                  "\"model\":\"claude-opus-5\",\"content\":["
                  "{\"type\":\"tool_use\",\"id\":\"toolu_01\",\"name\":\"get_weather\","
                  "\"input\":{\"location\":\"Paris\"}}],"
                  "\"stop_reason\":\"tool_use\",\"stop_details\":null,"
                  "\"usage\":{\"input_tokens\":40,\"output_tokens\":30}}"))
              (anthropic-stub/canned
                (string-append
                  "{\"id\":\"msg_2\",\"type\":\"message\",\"role\":\"assistant\","
                  "\"model\":\"claude-opus-5\",\"content\":[{\"type\":\"text\",\"text\":\"22 celsius.\"}],"
                  "\"stop_reason\":\"end_turn\",\"stop_details\":null,"
                  "\"usage\":{\"input_tokens\":60,\"output_tokens\":6}}"))))
  (parameterize ((anthropic/api-key "sk-ant-test")
                 (anthropic/transport (anthropic-stub/transport stub)))
    (receive (r transcript)
        (anthropic/converse (list (anthropic-message/user "weather in Paris?"))
                            tools: (list get_weather/tool))
      (list (anthropic-response/text r)
            (map anthropic-message-role transcript)
            (anthropic-stub/count stub)))))
;; => ("22 celsius." ("user" "assistant" "user" "assistant") 2)
```

### 5.4 A streaming call

```scheme
(import (aux base) (aux anthropic))

(let1 (r (anthropic/stream (list (anthropic-message/user "Write a haiku about Scheme."))
                           max-tokens: 8000
                           on-text: (λ (fragment) (display fragment) (flush-output))))
  (newline)
  (print "stop_reason: " (anthropic-response-stop-reason r))
  (print "output tokens: " (anthropic-usage/output-tokens r))
  (print "assembled: " (anthropic-response/text r)))
```

`on-text:` fires once per `text_delta`; `on-event:` fires once per frame if you want `message_start`, `ping`, `content_block_start` or anything the API adds later. The value that comes back is an ordinary `anthropic-response`, so every accessor in §3.7 works unchanged — the accumulator merges `message_delta`'s `stop_reason` and `usage` into the `message_start` skeleton and installs the finished blocks, index-ordered, as `content`.

Streaming a tool call reassembles the split `input_json_delta` fragments into a whole object, parsed exactly once at `content_block_stop`:

```scheme
(let1 (r (anthropic/stream (list (anthropic-message/user "What is the weather in Paris?"))
                           tools: (list get_weather/tool)
                           max-tokens: 8000))
  (map (λ (b) (list (anthropic-json/ref b 'name) (anthropic-json/ref b 'input)))
       (anthropic-response/tool-uses r)))
;; => (("get_weather" ((location "Paris"))))
```

Consuming the raw frames as a lazy stream, when you want the events themselves rather than the assembled message — note that `finish:` must release the wire, and that a caller who walks away from the stream early has to close it by hand:

```scheme
(import (aux stream))

(let1 (events (anthropic-sse/events
                (open-input-string
                  (string-append
                    "event: content_block_delta\n"
                    "data: {\"type\":\"content_block_delta\",\"index\":0,"
                    "\"delta\":{\"type\":\"text_delta\",\"text\":\"Hel\"}}\n\n"
                    "event: content_block_delta\n"
                    "data: {\"type\":\"content_block_delta\",\"index\":0,"
                    "\"delta\":{\"type\":\"text_delta\",\"text\":\"lo\"}}\n\n"))))
  (filter (λ (t) t) (map anthropic-event/text events)))
;; => ("Hel" "lo")
```

`anthropic/stream` is deliberately **not** retried: a retry re-POSTs the whole request, and for a partially consumed stream that means replaying tokens the caller has already displayed. A caller who wants retries wraps the call and discards what it has shown.

## 6. The test plan

### 6.0 Two files, and why

| File | Wired into `make test`? | Needs network? | Needs a key? |
|---|---|---|---|
| `src/test/anthropic.scm` | **yes**, last line of the `test:` target | no | no — it parameterizes `anthropic/api-key` to the literal `"sk-ant-api03-TESTKEY"` |
| `src/test/anthropic-live.scm` | **no**, only under its own `test-anthropic-live:` target | yes | yes — skips visibly and exits 0 when `ANTHROPIC_API_KEY` is unset |

`Dockerfile:18` is `RUN mkdir test-results && cd src && make install && make test -B && cp test/*.html test/*.md ../test-results …`, so anything in the `test:` target runs during `docker build`, where there is no key and no secret-injection path (`.github/workflows/docker.publish.yml` passes only `GITHUB_TOKEN`). The live suite therefore cannot be a *case* inside the offline suite — `src/aux.unittest.scm:32` calls `unittest/result-started!` unconditionally before the body runs and `unittest/result` has only `ran` and `failed` fields (`:94`), so the harness has no skip. It has to be a separate file with a whole-file gate.

### 6.1 How fixtures are stored

Inline top-level `define`s of string constants, not `.json` files. Three reasons, none of which is the one the earlier draft gave:

1. **No path coupling.** `make test` runs with cwd `src/test/`, but the Dockerfile's `cp` and `.gitignore`'s `src/test/*.html` are the only things that know about that directory. A fixture file would need a new `.gitignore` exemption (`twitter.json` is tracked; a new tracked `.json` is fine but is one more thing to keep in sync with the suite).
2. **They diff in review** next to the assertion that consumes them.
3. **They can be published into the report** — but *only* if you unquote them into the suite `doc`. This is the correction to the earlier draft's rationale, which claimed inline fixtures land in the HTML through `code/scheme`. They do not: `src/aux.unittest.scm:77` renders `(code/scheme ,code)` where `code` is the per-case quoted `define` from `define-suite` (`:83-87`), and top-level `define`s of the test *file* are never in `code`. Verified against the repo's own artifact: `grep -c result_type src/test/testsuite-auxtest.html` → `0`, while `src/test/test.scm:4` holds a ~500 KB expected-JSON literal, and the report is 15 KB.

   What *does* work is `(code/pre ,fixture)` inside the `doc` entry: `sxml-handler-code/pre` (`src/aux.sxml.scm:87`) is `(lambda (tag body) `(pre (code (@ (class "w3-code w3-round")) ,@body)))`, so an unquoted string child is spliced and rendered verbatim. The suite's `doc` therefore embeds the two SSE transcripts and one response body, and that is how the wire contract becomes published documentation.

No fixture contains a non-ASCII character *except* the two that deliberately test UTF-8, because until Part A's module is installed nothing guarantees `anthropic-json/parse` is the byte-length-correct one; if a fixture reached `simdjson-parse/ondemand` instead it would abort `csi` with SIGABRT rather than fail a case. The two unicode cases go through `anthropic-json/parse` and `anthropic/converse` explicitly, which is exactly where you want the regression guard.

### 6.2 The two assertions the harness does not have

`⊦⧳` (`src/aux.unittest.scm:160-165`) expands to a bare `condition-case` with one `(void)`-returning clause per kind list and **no failure path**. A body that never raises makes it return the body's value and the case passes. It tolerates; it does not require. I reproduced this locally: `(⊦⧳ ((exn)) 'quiet)` passes. So every negative-path case below uses two hand-rolled assertions defined at the top of the file with `(aux base)`'s `define-syntax-rule` (`src/aux.base.scm:26-29`).

The second one has a trap that cost the earlier draft a whole test section, and I reproduced it: **the pattern variables must be named `s` and `t`, not `status` and `type`.** `syntax-rules` substitutes pattern variables inside `quote` too, so with the obvious names the template's `'status` becomes `'400` and the assertion dies with `Error: (condition-property-accessor) condition has no such property: 400`. Verified on `csi 6.0.1pre1`:

```
(define-syntax m (syntax-rules () ((_ (status type) b) (list 'status 'type b))))
(m (400 "bad") 'z)   ⇒   (400 bad z)
```

### 6.3 Gaps in Part A

Nothing below is invented; every binding the suite calls exists in Part A §4 with that name and arity. These are the frictions I hit writing the tests, recorded so the developer is not surprised.

**G1 — `anthropic-request/headers` raises when there is no key, so no stub-driven entry point is callable keyless.** `anthropic-request/headers` (§4.4) calls `anthropic-api-key/effective`, which raises `anthropic-config-error`; `anthropic-send` calls it unconditionally. Swapping in `anthropic-stub/transport` does not help. The suite works because every stub case sets `anthropic/api-key` to a literal, and the `letstub` macro in §6.4 makes that impossible to forget. *Recommended Part A change if you want a keyless stub path:* have `anthropic-api-key/effective` be called from `anthropic-transport/curl` rather than from `anthropic-request/headers`, and have `anthropic-request/headers` emit the `x-api-key` pair only when `(anthropic-api-key/current)` is non-`#f`. The suite below does **not** depend on that change; make it or don't.

**G2 — `anthropic-stub/calls` redacts against the *current* parameter value.** It maps header values through `anthropic/redact`, which reads `(anthropic-api-key/current)`. Called outside the `parameterize` the key is gone and the raw value comes back. Every assertion over `anthropic-stub/calls` in the suite is therefore *inside* the `letstub` body. Not a missing binding — a usage constraint worth a comment in Part A.

**G3 — `anthropic-loop-error` carries `response` but not `transcript`.** After a `max-iterations` or `refusal` raise the caller cannot recover the conversation it had accumulated. The suite can only assert the reason and the request count. Listed again in §9.

**G4 — no streaming variant of `anthropic/converse`.** `anthropic/stream` and `anthropic/converse` are disjoint; there is no way to stream a tool-using turn. The suite tests each separately and does not test a composition that does not exist. §9.

**G5 — `anthropic-tools->table` silently ignores non-record entries.** `anthropic-tools->json` accepts raw alists (useful for hand-written tool definitions) but `anthropic-tools->table` registers only `anthropic-tool?` records, so a raw-alist tool is advertised to the model and then answered with `no such tool: …`. The suite pins this behaviour rather than treating it as a bug, because it is the only sane split; §9 proposes closing it.

**G6 — `anthropic-response-body` of a *streamed* response is re-encoded, not wire bytes.** `anthropic-response/of-json` sets `body` to `(anthropic-json/write json)`, so decoded `null`s come back as `null` and key order is the accumulator's. Assertions on a streamed response go through accessors and `anthropic-response-json`, never through `anthropic-response-body`.

### 6.4 `src/test/anthropic.scm` — the offline suite

Every expected string below was computed by executing Part A's encoder, validator, flonum printer and SSE accumulator verbatim on `csi 6.0.1pre1` (scratch harness at `/private/tmp/claude-501/-Users-mn-Developer-working-copies-schemes-aux-scm/4e29df20-d27c-4418-ac24-ae96915980a2/scratchpad/pb/`). They are transcribed output, not predictions.

```scheme

; The offline suite for (aux anthropic).  No network, no API key, no subprocess: the HTTP
; transport is a parameter and every case installs a stub that replays recorded bytes and
; records the exact bytes the module tried to send.  The live suite is anthropic-live.scm and
; is NOT part of `make test`.
;
; Two harness facts drive the shape of this file.  (aux unittest) has no skip -- so the live
; cases cannot live here -- and ⊦⧳ TOLERATES a condition rather than requiring one, so every
; negative-path case uses the hand-rolled ⊦raises / ⊦raises/api below instead.

(import
  (scheme base)                       ; open-input-string and parameterize are NOT in (chicken base)
  (scheme file)                       ; with-input-from-file: NOT in (chicken file)
  (chicken base)
  (chicken bytevector)                ; bytevector-u8-set!, for the custom-port case
  (chicken condition)
  (chicken file)
  (chicken file posix)
  (chicken io)
  (chicken port)                      ; make-input-port, for the custom-port case
  (chicken string)
  (aux base)
  (aux unittest)
  (aux anthropic)
  srfi-1
  srfi-69)                            ; hash-table-ref/default, in the tool-registry cases

; the two assertions (aux unittest) does not have ------------------------------------------
;
; ⊦⧳ (aux.unittest.scm:160) is a TOLERATE form: with a body that never raises it returns the
; body's value and the case passes.  These require the raise.
;
; NOTE the pattern variables are `s` and `t`, NOT `status` and `type`.  syntax-rules
; substitutes pattern variables inside quote, so `status` would rewrite the template's
; 'status into '400 and the assertion would die with "condition has no such property: 400".

(define-syntax-rule (⊦raises (kind ...) body ...)
  (⊦= 'raised (condition-case (begin body ... 'not-raised) ((kind ...) 'raised))))

(define-syntax-rule (⊦raises/api (s t) body ...)
  (⊦= (list s t)
      (condition-case (begin body ... 'no-error)
        (c (anthropic-api-error)
           (list (get-condition-property c 'anthropic-api-error 'status)
                 (get-condition-property c 'anthropic-api-error 'type))))))

(define-syntax-rule (⊦raises/loop expected body ...)
  (⊦= expected
      (condition-case (begin body ... 'no-error)
        (c (anthropic-loop-error) (get-condition-property c 'anthropic-loop-error 'reason)))))

; the stub fixture --------------------------------------------------------------------------
;
; anthropic-request/headers calls anthropic-api-key/effective, which RAISES when no key is
; set, so every stub-driven case must supply one.  This macro makes that impossible to forget
; and keeps assertions over anthropic-stub/calls INSIDE the parameterize -- outside it the
; redaction has no key to redact against and the literal would leak into the HTML report.

(define anthropic-test/key "sk-ant-api03-TESTKEY")

(define-syntax-rule (letstub (stub canned ...) body ...)
  (let1 (stub (anthropic-stub/make canned ...))
    (parameterize ((anthropic/api-key anthropic-test/key)
                   (anthropic/transport (anthropic-stub/transport stub))
                   (anthropic/backoff-jitter #f))
      body ...)))

; recorded response fixtures -----------------------------------------------------------------

(define fixture/response/text
  (string-append
    "{\"id\":\"msg_01\",\"type\":\"message\",\"role\":\"assistant\",\"model\":\"claude-opus-5\","
    "\"content\":[{\"type\":\"text\",\"text\":\"Paris.\"}],"
    "\"stop_reason\":\"end_turn\",\"stop_sequence\":null,\"stop_details\":null,"
    "\"usage\":{\"input_tokens\":12,\"output_tokens\":4,"
    "\"cache_creation_input_tokens\":0,\"cache_read_input_tokens\":30}}"))

(define fixture/response/tool-use
  (string-append
    "{\"id\":\"msg_02\",\"type\":\"message\",\"role\":\"assistant\",\"model\":\"claude-opus-5\","
    "\"content\":[{\"type\":\"text\",\"text\":\"Let me check the weather.\"},"
    "{\"type\":\"tool_use\",\"id\":\"toolu_01\",\"name\":\"get_weather\","
    "\"input\":{\"location\":\"Paris\"}}],"
    "\"stop_reason\":\"tool_use\",\"stop_sequence\":null,\"stop_details\":null,"
    "\"usage\":{\"input_tokens\":40,\"output_tokens\":30}}"))

(define fixture/response/parallel-tool-use
  (string-append
    "{\"id\":\"msg_03\",\"type\":\"message\",\"role\":\"assistant\",\"model\":\"claude-opus-5\","
    "\"content\":[{\"type\":\"tool_use\",\"id\":\"toolu_01\",\"name\":\"get_weather\","
    "\"input\":{\"location\":\"Paris\"}},"
    "{\"type\":\"tool_use\",\"id\":\"toolu_02\",\"name\":\"get_weather\","
    "\"input\":{\"location\":\"Rome\",\"unit\":\"fahrenheit\"}}],"
    "\"stop_reason\":\"tool_use\",\"stop_sequence\":null,\"stop_details\":null,"
    "\"usage\":{\"input_tokens\":44,\"output_tokens\":52}}"))

(define fixture/response/mixed-tool-use
  (string-append
    "{\"id\":\"msg_04\",\"type\":\"message\",\"role\":\"assistant\",\"model\":\"claude-opus-5\","
    "\"content\":[{\"type\":\"tool_use\",\"id\":\"toolu_01\",\"name\":\"get_weather\","
    "\"input\":{\"location\":\"Paris\"}},"
    "{\"type\":\"tool_use\",\"id\":\"toolu_02\",\"name\":\"explode\",\"input\":{\"why\":\"now\"}},"
    "{\"type\":\"tool_use\",\"id\":\"toolu_03\",\"name\":\"ghost\",\"input\":{}}],"
    "\"stop_reason\":\"tool_use\",\"stop_sequence\":null,\"stop_details\":null,"
    "\"usage\":{\"input_tokens\":50,\"output_tokens\":60}}"))

(define fixture/response/thinking
  (string-append
    "{\"id\":\"msg_05\",\"type\":\"message\",\"role\":\"assistant\",\"model\":\"claude-opus-5\","
    "\"content\":[{\"type\":\"thinking\",\"thinking\":\"\",\"signature\":\"EqQBCgIYAhIM\"},"
    "{\"type\":\"text\",\"text\":\"Done.\"}],"
    "\"stop_reason\":\"end_turn\",\"stop_sequence\":null,\"stop_details\":null,"
    "\"usage\":{\"input_tokens\":9,\"output_tokens\":7}}"))

(define fixture/response/refusal
  (string-append
    "{\"id\":\"msg_06\",\"type\":\"message\",\"role\":\"assistant\",\"model\":\"claude-opus-5\","
    "\"content\":[],\"stop_reason\":\"refusal\",\"stop_sequence\":null,"
    "\"stop_details\":{\"category\":\"cyber\",\"explanation\":\"declined\"},"
    "\"usage\":{\"input_tokens\":31,\"output_tokens\":0}}"))

(define fixture/response/refusal/null-details
  (string-append
    "{\"id\":\"msg_07\",\"type\":\"message\",\"role\":\"assistant\",\"model\":\"claude-opus-5\","
    "\"content\":[],\"stop_reason\":\"refusal\",\"stop_sequence\":null,\"stop_details\":null,"
    "\"usage\":{\"input_tokens\":31,\"output_tokens\":0}}"))

(define fixture/response/max-tokens/tool-use
  (string-append
    "{\"id\":\"msg_08\",\"type\":\"message\",\"role\":\"assistant\",\"model\":\"claude-opus-5\","
    "\"content\":[{\"type\":\"tool_use\",\"id\":\"toolu_09\",\"name\":\"get_weather\","
    "\"input\":{\"location\":\"Par\"}}],"
    "\"stop_reason\":\"max_tokens\",\"stop_sequence\":null,\"stop_details\":null,"
    "\"usage\":{\"input_tokens\":40,\"output_tokens\":16000}}"))

(define fixture/response/max-tokens/text
  (string-append
    "{\"id\":\"msg_09\",\"type\":\"message\",\"role\":\"assistant\",\"model\":\"claude-opus-5\","
    "\"content\":[{\"type\":\"text\",\"text\":\"trunc\"}],"
    "\"stop_reason\":\"max_tokens\",\"stop_sequence\":null,\"stop_details\":null,"
    "\"usage\":{\"input_tokens\":40,\"output_tokens\":16000}}"))

; a pause_turn resend works because the API sees the TRAILING server_tool_use block; an
; assistant turn ending in a plain text block would be prefill, i.e. a 400.
(define fixture/response/pause-turn
  (string-append
    "{\"id\":\"msg_10\",\"type\":\"message\",\"role\":\"assistant\",\"model\":\"claude-opus-5\","
    "\"content\":[{\"type\":\"text\",\"text\":\"Searching...\"},"
    "{\"type\":\"server_tool_use\",\"id\":\"srvtoolu_01\",\"name\":\"web_search\","
    "\"input\":{\"query\":\"weather\"}}],"
    "\"stop_reason\":\"pause_turn\",\"stop_sequence\":null,\"stop_details\":null,"
    "\"usage\":{\"input_tokens\":11,\"output_tokens\":5}}"))

(define fixture/response/unknown-block
  (string-append
    "{\"id\":\"msg_11\",\"type\":\"message\",\"role\":\"assistant\",\"model\":\"claude-opus-5\","
    "\"content\":[{\"type\":\"fallback\",\"reason\":\"no_capacity\"}],"
    "\"stop_reason\":\"end_turn\",\"stop_sequence\":null,\"stop_details\":null,"
    "\"usage\":{\"input_tokens\":1,\"output_tokens\":1}}"))

(define fixture/response/unknown-stop
  (string-append
    "{\"id\":\"msg_12\",\"type\":\"message\",\"role\":\"assistant\",\"model\":\"claude-opus-5\","
    "\"content\":[{\"type\":\"text\",\"text\":\"x\"}],"
    "\"stop_reason\":\"banana\",\"stop_sequence\":null,\"stop_details\":null,"
    "\"usage\":{\"input_tokens\":1,\"output_tokens\":1}}"))

(define fixture/response/context-window
  (string-append
    "{\"id\":\"msg_13\",\"type\":\"message\",\"role\":\"assistant\",\"model\":\"claude-opus-5\","
    "\"content\":[],\"stop_reason\":\"model_context_window_exceeded\",\"stop_sequence\":null,"
    "\"stop_details\":null,\"usage\":{\"input_tokens\":999999,\"output_tokens\":0}}"))

(define fixture/response/unicode-tool
  (string-append
    "{\"id\":\"msg_14\",\"type\":\"message\",\"role\":\"assistant\",\"model\":\"claude-opus-5\","
    "\"content\":[{\"type\":\"tool_use\",\"id\":\"toolu_11\",\"name\":\"motto\",\"input\":{}}],"
    "\"stop_reason\":\"tool_use\",\"stop_sequence\":null,\"stop_details\":null,"
    "\"usage\":{\"input_tokens\":5,\"output_tokens\":5}}"))

; recorded error fixtures ---------------------------------------------------------------------

(define fixture/error/400
  (string-append
    "{\"type\":\"error\",\"error\":{\"type\":\"invalid_request_error\","
    "\"message\":\"messages: roles must alternate between \\\"user\\\" and \\\"assistant\\\"\"},"
    "\"request_id\":\"req_011CSHoEeqs5C35K2UUqR7Fy\"}"))

(define fixture/error/401
  (string-append
    "{\"type\":\"error\",\"error\":{\"type\":\"authentication_error\","
    "\"message\":\"invalid x-api-key\"},\"request_id\":\"req_401\"}"))

(define fixture/error/404
  (string-append
    "{\"type\":\"error\",\"error\":{\"type\":\"not_found_error\","
    "\"message\":\"model: claude-does-not-exist\"},\"request_id\":\"req_404\"}"))

(define fixture/error/429
  (string-append
    "{\"type\":\"error\",\"error\":{\"type\":\"rate_limit_error\","
    "\"message\":\"rate limited\"},\"request_id\":\"req_429\"}"))

(define fixture/error/529
  (string-append
    "{\"type\":\"error\",\"error\":{\"type\":\"overloaded_error\","
    "\"message\":\"Overloaded\"},\"request_id\":\"req_529\"}"))

(define fixture/error/non-json
  "<html><head><title>502 Bad Gateway</title></head><body>proxy</body></html>")

(define fixture/error/truncated "{\"id\":\"msg_1\",\"type\":\"mess")

; recorded SSE transcripts -----------------------------------------------------------------------
;
; Three indices, a thinking block with its signature_delta, and input_json_delta fragments that
; split {"location": "Paris"} mid-token -- i.e. what a real claude-opus-5 turn looks like, not the
; single-index text-only stream the documentation shows.

(define fixture/sse/text
  (string-append
    "event: message_start\ndata: {\"type\":\"message_start\",\"message\":{\"id\":\"msg_s1\",\"type\":\"message\",\"role\":\"assistant\",\"model\":\"claude-opus-5\",\"content\":[],\"stop_reason\":null,\"stop_sequence\":null,\"stop_details\":null,\"usage\":{\"input_tokens\":10,\"output_tokens\":1}}}\n\n"
    ": keep-alive\n\n"
    "event: content_block_start\ndata: {\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"text\",\"text\":\"\"}}\n\n"
    "event: ping\ndata: {\"type\":\"ping\"}\n\n"
    "event: content_block_delta\ndata: {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\"Hello\"}}\n\n"
    "event: content_block_delta\ndata: {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\" world\"}}\n\n"
    "event: content_block_stop\ndata: {\"type\":\"content_block_stop\",\"index\":0}\n\n"
    "event: message_delta\ndata: {\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"end_turn\",\"stop_sequence\":null},\"usage\":{\"output_tokens\":12}}\n\n"
    "event: message_stop\ndata: {\"type\":\"message_stop\"}\n\n"))

(define fixture/sse/tool-use
  (string-append
    "event: message_start\ndata: {\"type\":\"message_start\",\"message\":{\"id\":\"msg_s2\",\"type\":\"message\",\"role\":\"assistant\",\"model\":\"claude-opus-5\",\"content\":[],\"stop_reason\":null,\"stop_sequence\":null,\"stop_details\":null,\"usage\":{\"input_tokens\":20,\"output_tokens\":1}}}\n\n"
    "event: content_block_start\ndata: {\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"thinking\",\"thinking\":\"\"}}\n\n"
    "event: content_block_delta\ndata: {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"thinking_delta\",\"thinking\":\"Look it up.\"}}\n\n"
    "event: content_block_delta\ndata: {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"signature_delta\",\"signature\":\"EqQBCgIY\"}}\n\n"
    "event: content_block_stop\ndata: {\"type\":\"content_block_stop\",\"index\":0}\n\n"
    "event: content_block_start\ndata: {\"type\":\"content_block_start\",\"index\":1,\"content_block\":{\"type\":\"text\",\"text\":\"\"}}\n\n"
    "event: content_block_delta\ndata: {\"type\":\"content_block_delta\",\"index\":1,\"delta\":{\"type\":\"text_delta\",\"text\":\"Checking.\"}}\n\n"
    "event: content_block_stop\ndata: {\"type\":\"content_block_stop\",\"index\":1}\n\n"
    "event: content_block_start\ndata: {\"type\":\"content_block_start\",\"index\":2,\"content_block\":{\"type\":\"tool_use\",\"id\":\"toolu_01\",\"name\":\"get_weather\",\"input\":{}}}\n\n"
    "event: content_block_delta\ndata: {\"type\":\"content_block_delta\",\"index\":2,\"delta\":{\"type\":\"input_json_delta\",\"partial_json\":\"{\\\"loca\"}}\n\n"
    "event: content_block_delta\ndata: {\"type\":\"content_block_delta\",\"index\":2,\"delta\":{\"type\":\"input_json_delta\",\"partial_json\":\"tion\\\": \\\"Pa\"}}\n\n"
    "event: content_block_delta\ndata: {\"type\":\"content_block_delta\",\"index\":2,\"delta\":{\"type\":\"input_json_delta\",\"partial_json\":\"ris\\\"}\"}}\n\n"
    "event: content_block_stop\ndata: {\"type\":\"content_block_stop\",\"index\":2}\n\n"
    "event: message_delta\ndata: {\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"tool_use\",\"stop_sequence\":null},\"usage\":{\"output_tokens\":45}}\n\n"
    "event: message_stop\ndata: {\"type\":\"message_stop\"}\n\n"))

; a parameterless tool streams content_block_start with "input":{} and NO input_json_delta at
; all.  A stop that parsed its (empty) buffer unconditionally would abort the process.
(define fixture/sse/no-input
  (string-append
    "event: message_start\ndata: {\"type\":\"message_start\",\"message\":{\"id\":\"msg_s3\",\"type\":\"message\",\"role\":\"assistant\",\"model\":\"claude-opus-5\",\"content\":[],\"stop_reason\":null,\"stop_sequence\":null,\"stop_details\":null,\"usage\":{\"input_tokens\":7,\"output_tokens\":1}}}\n\n"
    "event: content_block_start\ndata: {\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"tool_use\",\"id\":\"toolu_02\",\"name\":\"now\",\"input\":{}}}\n\n"
    "event: content_block_stop\ndata: {\"type\":\"content_block_stop\",\"index\":0}\n\n"
    "event: message_delta\ndata: {\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"tool_use\",\"stop_sequence\":null},\"usage\":{\"output_tokens\":9}}\n\n"
    "event: message_stop\ndata: {\"type\":\"message_stop\"}\n\n"))

(define fixture/sse/broken-input
  (string-append
    "event: message_start\ndata: {\"type\":\"message_start\",\"message\":{\"id\":\"msg_s4\",\"type\":\"message\",\"role\":\"assistant\",\"model\":\"claude-opus-5\",\"content\":[],\"stop_reason\":null,\"stop_sequence\":null,\"stop_details\":null,\"usage\":{\"input_tokens\":7,\"output_tokens\":1}}}\n\n"
    "event: content_block_start\ndata: {\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"tool_use\",\"id\":\"toolu_03\",\"name\":\"get_weather\",\"input\":{}}}\n\n"
    "event: content_block_delta\ndata: {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"input_json_delta\",\"partial_json\":\"{\\\"location\\\": \\\"Par\"}}\n\n"
    "event: content_block_stop\ndata: {\"type\":\"content_block_stop\",\"index\":0}\n\n"
    "event: message_delta\ndata: {\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"max_tokens\",\"stop_sequence\":null},\"usage\":{\"output_tokens\":64}}\n\n"
    "event: message_stop\ndata: {\"type\":\"message_stop\"}\n\n"))

(define fixture/sse/cut
  (string-append
    "event: message_start\ndata: {\"type\":\"message_start\",\"message\":{\"id\":\"msg_s5\",\"type\":\"message\",\"role\":\"assistant\",\"model\":\"claude-opus-5\",\"content\":[],\"stop_reason\":null,\"stop_sequence\":null,\"stop_details\":null,\"usage\":{\"input_tokens\":3,\"output_tokens\":1}}}\n\n"
    "event: content_block_start\ndata: {\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"text\",\"text\":\"\"}}\n\n"
    "event: content_block_delta\ndata: {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\"half\"}}\n\n"))

(define fixture/sse/error
  (string-append
    "event: error\ndata: {\"type\":\"error\",\"error\":{\"type\":\"overloaded_error\","
    "\"message\":\"Overloaded\"}}\n\n"))

; tools used by the suite ---------------------------------------------------------------------
;
; At TOP LEVEL, not inside a case: define-tool expands to (begin (define NAME ...)
; (define NAME/tool ...) (void)), and a begin that mixes defines with a trailing expression is
; only unambiguously legal at top level.

(define-tool (get_weather
               (location string "The city and state, e.g. San Francisco, CA")
               (unit (enum "celsius" "fahrenheit") "Unit of temperature" (default "celsius")))
  "Get the current weather in a given location."
  (conc "22 " unit " in " location))

(define-tool (explode (why string "Why it should fail"))
  "Always fails."
  (error (conc "boom: " why)))

(define-tool (now) "Return the current time as an ISO-8601 string." "2026-09-16T00:00:00Z")

(define-tool (motto) "A motto with characters outside ASCII." "caffè 一 espresso")

(define-tool (counted (n integer "ignored")) "Counts its own calls." (add1! tool/calls) n)

(define-tool/strict (send_email (to (array-of string) "Recipient addresses"))
  "Send an email."
  (conc "queued " (vector-length to)))

(define-tool (search (query string "What to look for")
                     (limit integer "How many results")
                     (tags (array-of string) "Filter tags")
                     (deep boolean "Recurse?"))
  "Search the index"
  (list query limit tags deep))

(define nonnegative '((type "integer") (minimum 0)))

(define-tool (sleep_for (seconds (raw nonnegative) "Seconds to sleep")) "Sleep." seconds)

(define tool/calls 0)


(define-suite anthropic-suite

  ((doc r)
   `((structure/section "Talking to Claude from CHICKEN Scheme")
     (p "Every case below is offline. There is no network and no API key: the HTTP transport "
        "is the parameter " (code/inline "anthropic/transport") ", and each case installs a "
        "stub that replays a recorded response and records the exact bytes the module tried "
        "to send. The live suite is " (code/inline "test/anthropic-live.scm") " and is not "
        "part of " (code/inline "make test") ".")
     (p "The transport contract is four arguments in and one " (code/inline "anthropic-wire")
        " record out:")
     (code/pre "(transport url headers body stream?) -> anthropic-wire\n  status   exact-integer HTTP status\n  headers  ((lowercase-name value) ...)\n  command  the command that produced it\n  port     an input port at the first byte of the body\n  close    thunk -> (values exit-status diagnostics), idempotent")
     (p "Two facts about the JSON layer drive most of the request cases. The encoder writes "
        (code/inline "'()") " as " (code/inline "{}") " and only " (code/inline "#()")
        " as " (code/inline "[]") ", so every JSON array on the wire is a Scheme vector; and "
        "it escapes C0 control characters as " (code/inline "\\u00XX") ", where "
        (code/inline "->string/json") " would emit CHICKEN's " (code/inline "\\x01;")
        " -- not JSON, and a body this parser then refuses.")
     (p "Nothing is ever handed to simdjson without passing "
        (code/inline "anthropic-json/well-formed?") " first: "
        (code/inline "src/chicken-simdjson.cpp") " contains no " (code/inline "try")
        "/" (code/inline "catch") ", so a malformed body reaches "
        (code/inline "std::terminate") " and kills the process with SIGABRT, which no Scheme "
        "handler can catch.")
     (structure/section "The recorded transcripts")
     (p "A tool-using turn, verbatim off the wire. Three block indices, a thinking block whose "
        "signature arrives on its own " (code/inline "signature_delta") ", and "
        (code/inline "input_json_delta") " fragments that split "
        (code/inline "{\"location\": \"Paris\"}") " mid-token:")
     (code/pre ,fixture/sse/tool-use)
     (p "A text-only stream, including the " (code/inline "ping") " frame and a "
        (code/inline ":") " comment line:")
     (code/pre ,fixture/sse/text)
     (p "And a blocking response carrying a tool call:")
     (code/pre ,fixture/response/tool-use)
     (structure/section "The module")
     (code/scheme/file "../aux.anthropic.scm")))

  ; -- JSON: the validator ---------------------------------------------------------------

  ((test/anthropic/json/well-formed _)
   (⊨ (anthropic-json/well-formed? "{}"))
   (⊨ (anthropic-json/well-formed? "[]"))
   (⊨ (anthropic-json/well-formed? "{\"a\":1}"))
   (⊨ (anthropic-json/well-formed? "  {\"a\":[1,2,{\"b\":null}],\"c\":-1.5e+3}  "))
   (⊨ (anthropic-json/well-formed? "{\"a\":\"\\u00e8 \\n \\\" \\\\\"}"))
   (⊨ (anthropic-json/well-formed? fixture/response/text)))

  ((test/anthropic/json/ill-formed _)
   (⊭ (anthropic-json/well-formed? fixture/error/truncated))
   (⊭ (anthropic-json/well-formed? fixture/error/non-json))
   (⊭ (anthropic-json/well-formed? ""))
   (⊭ (anthropic-json/well-formed? "42"))
   (⊭ (anthropic-json/well-formed? "null"))
   (⊭ (anthropic-json/well-formed? "\"x\""))
   (⊭ (anthropic-json/well-formed? "{\"a\":1}trailing"))
   (⊭ (anthropic-json/well-formed? "{\"a\":01}"))
   (⊭ (anthropic-json/well-formed? (conc "{\"a\":\"" (string (integer->char 1)) "\"}")))
   `(doc (p "A bare scalar document is rejected on purpose: simdjson's on-demand walker "
            "throws " (code/inline "SCALAR_DOCUMENT_AS_VALUE") " on " (code/inline "42")
            " just as surely as on a truncated object, and both aborts are uncatchable. "
            "Sniffing for a leading " (code/inline "{") " -- which the truncated fixture "
            "passes -- is not a guard.")))

  ((test/anthropic/json/parse-raises-rather-than-aborting _)
   (⊦raises (anthropic-decode-error) (anthropic-json/parse fixture/error/truncated))
   (⊦raises (anthropic-decode-error) (anthropic-json/parse fixture/error/non-json))
   (⊦raises (anthropic-decode-error) (anthropic-json/parse "42")))

  ((test/anthropic/json/utf8-length _)
   (⊦= 1 (anthropic-json/utf8-length "a"))
   (⊦= 6 (anthropic-json/utf8-length "caffè"))
   (⊦= 3 (anthropic-json/utf8-length "一"))
   (⊦= 0 (anthropic-json/utf8-length ""))
   (⊦≠ (string-length "caffè 一 ok") (anthropic-json/utf8-length "caffè 一 ok"))
   `(doc (p (code/inline "simdjson-parse/ondemand") " (aux.simdjson.scm:93) hands the C side "
            (code/inline "string-length") ", a CHARACTER count, where simdjson wants BYTES. "
            "This is the number that has to replace it.")))

  ((test/anthropic/json/round-trip _)
   (let1 (v `((text "caffè 一 ok")
              (lines "l1\nl2\tx\"q\"\\z")
              (n 12) (f 1.5) (t #t) (f2 #f) (nul ,(void))
              (arr #(1 "two" ()))
              (obj ((a 1)))))
     (⊦= v (anthropic-json/parse (anthropic-json/write v))))
   `(doc (p "Decode / encode / decode is the identity. The whole replay strategy rests on it: "
            "the loop re-encodes an assistant turn's DECODED content, and a thinking block "
            "that comes back modified is a 400.")))

  ((test/anthropic/json/escapes-control-characters _)
   (let1 (s (conc "a" (string (integer->char 1)) "b"))
     (⊦= "{\"text\":\"a\\u0001b\"}" (anthropic-json/write `((text ,s))))
     (⊦= `((text ,s)) (anthropic-json/parse (anthropic-json/write `((text ,s))))))
   (⊦= "{\"t\":\"l1\\nl2\\tx\\\"q\\\"\\\\z\"}" (anthropic-json/write '((t "l1\nl2\tx\"q\"\\z"))))
   (⊦= "{\"text\":\"caffè 一 ok\"}" (anthropic-json/write '((text "caffè 一 ok"))))
   `(doc (p "Tool results are arbitrary program output. "
            (code/inline "->string/json") " escapes with Scheme " (code/inline "write")
            " and would put " (code/inline "\\x01;") " on the wire, which the API rejects "
            "and which this parser then refuses.")))

  ((test/anthropic/json/numbers _)
   (⊦= "{\"a\":3.141592653589793}" (anthropic-json/write '((a 3.141592653589793))))
   (⊦= "{\"a\":22.0}" (anthropic-json/write '((a 22.0))))
   (⊦= "{\"a\":1e+300}" (anthropic-json/write '((a 1e300))))
   (⊦= "{\"a\":0.3333333333333333}" (anthropic-json/write '((a 1/3))))
   (⊦= "{\"a\":9223372036854775808}" (anthropic-json/write '((a 9223372036854775808))))
   (⊦= "3.141592653589793" (anthropic-json/flonum->string 3.141592653589793))
   (⊦raises (anthropic-encode-error) (anthropic-json/write `((a ,(/ 0. 0.)))))
   (⊦raises (anthropic-encode-error) (anthropic-json/write '((a +inf.0))))
   `(doc (p "CHICKEN's " (code/inline "flonum-print-precision") " defaults to 15, which turns "
            (code/inline "3.141592653589793") " into " (code/inline "3.14159265358979")
            ". A replayed turn must be byte-faithful, so the encoder searches upward for the "
            "shortest representation that reads back to the same flonum, and restores the "
            "parameter afterwards. A ratnum is coerced; NaN and the infinities raise.")))

  ((test/anthropic/json/encoder-is-total _)
   (⊦= "{\"role\":\"user\"}" (anthropic-json/write '((role user))))
   (⊦= "{\"c\":\"u\"}" (anthropic-json/write '((c #\u))))
   (⊦raises (anthropic-encode-error) (anthropic-json/write '((a . 1))))
   (⊦raises (anthropic-encode-error) (anthropic-json/write '((a 1 2))))
   (⊦raises (anthropic-encode-error) (anthropic-json/write '((7 "v"))))
   (⊦raises (anthropic-encode-error) (anthropic-json/write `((a ,car))))
   `(doc (p "Every leaf the encoder cannot represent raises "
            (code/inline "anthropic-encode-error") ". A non-symbol, non-string KEY raises too "
            "-- that is the one the type check above the key is easy to forget.")))

  ((test/anthropic/json/accessors _)
   (let1 (obj (anthropic-json/parse "{\"a\":1,\"b\":null,\"c\":{},\"d\":[],\"e\":[1,2]}"))
     (⊦= 1 (anthropic-json/ref obj 'a))
     (⊦= (void) (anthropic-json/ref obj 'b))
     (⊨ (anthropic-json/null? (anthropic-json/ref obj 'b)))
     (⊨ (anthropic-json/has? obj 'b))
     (⊦= '() (anthropic-json/ref obj 'c))
     (⊭ (anthropic-json/null? (anthropic-json/ref obj 'c)))
     (⊦= #() (anthropic-json/ref obj 'd))
     (⊦= '(1 2) (anthropic-json/vector->list (anthropic-json/ref obj 'e)))
     (⊦= '() (anthropic-json/vector->list (anthropic-json/ref obj 'c)))
     (⊦= 'absent (anthropic-json/ref obj 'missing 'absent))
     (⊭ (anthropic-json/has? obj 'missing))
     (⊦= 'absent (anthropic-json/ref (void) 'a 'absent)))
   `(doc (p "An absent key and a key whose value is JSON " (code/inline "null")
            " are different things: " (code/inline "null") " decodes to "
            (code/inline "(void)") ", while " (code/inline "'()") " is an empty OBJECT. "
            "Confusing them is how a refusal gets misread.")))

  ((test/anthropic/json/set-and-merge _)
   (⊦= '((a 1) (b 9)) (anthropic-json/set '((a 1) (b 2)) 'b 9))
   (⊦= '((a 1) (b 2)) (anthropic-json/set '((a 1)) 'b 2))
   (⊦= '((a 1) (b 3) (c 4)) (anthropic-json/merge '((a 1) (b 2)) '((b 3) (c 4))))
   (⊦= '((a 1)) (anthropic-json/merge '((a 1)) '()))
   `(doc (p "Replacement is in place so that key ORDER survives, which is what makes a "
            "streamed message land in the same shape as a blocking one.")))

  ; -- request building ---------------------------------------------------------------------

  ((test/anthropic/request/plain _)
   (⊦= "{\"model\":\"claude-opus-5\",\"max_tokens\":16000,\"messages\":[{\"role\":\"user\",\"content\":\"What is the capital of France?\"}]}"
       (anthropic-json/write
         (anthropic-request/body (list (anthropic-message/user "What is the capital of France?"))))))

  ((test/anthropic/request/model-and-max-tokens _)
   (let1 (body (anthropic-request/body (list (anthropic-message/user "hi"))
                                       model: "claude-haiku-4-5" max-tokens: 4000))
     (⊦= "claude-haiku-4-5" (anthropic-json/ref body 'model))
     (⊦= 4000 (anthropic-json/ref body 'max_tokens)))
   (parameterize ((anthropic/model "claude-sonnet-5"))
     (⊦= "claude-sonnet-5"
         (anthropic-json/ref (anthropic-request/body (list (anthropic-message/user "hi"))) 'model)))
   `(doc (p "Model ids are complete as they stand -- never append a date suffix -- and the "
            "caller's value must reach the wire verbatim.")))

  ((test/anthropic/request/system-is-top-level _)
   (⊦= "{\"model\":\"claude-opus-5\",\"max_tokens\":16000,\"messages\":[{\"role\":\"user\",\"content\":\"hi\"}],\"system\":\"Be terse.\"}"
       (anthropic-json/write
         (anthropic-request/body (list (anthropic-message/user "hi")) system: "Be terse.")))
   `(doc (p (code/inline "system") " is a sibling of " (code/inline "messages")
            ", never a message with role " (code/inline "system") ".")))

  ((test/anthropic/request/omits-absent-options _)
   (let1 (body (anthropic-request/body (list (anthropic-message/user "hi"))))
     (for-each (λ (k) (⊦= 'absent (anthropic-json/ref body k 'absent)))
               '(system tools tool_choice stream thinking output_config stop_sequences
                 temperature top_p top_k)))
   `(doc (p "There is no \"omit this\" value in the encoder, so an absent option must simply "
            "not be a key -- " (code/inline "(void)") " would encode as "
            (code/inline "null") ", a different request. "
            (code/inline "temperature") ", " (code/inline "top_p") " and "
            (code/inline "top_k") " are not keyword arguments at all: they are removed on "
            (code/inline "claude-opus-5") " and 400 if sent.")))

  ((test/anthropic/request/thinking-and-effort _)
   (⊦= "{\"model\":\"claude-opus-5\",\"max_tokens\":16000,\"messages\":[{\"role\":\"user\",\"content\":\"hi\"}],\"thinking\":{\"type\":\"adaptive\",\"display\":\"summarized\"},\"output_config\":{\"effort\":\"high\"}}"
       (anthropic-json/write
         (anthropic-request/body (list (anthropic-message/user "hi"))
                                 thinking: '((type "adaptive") (display "summarized"))
                                 effort: "high")))
   (⊦= 'absent
       (anthropic-json/ref
         (anthropic-json/ref (anthropic-request/body (list (anthropic-message/user "hi"))
                                                     thinking: '((type "adaptive")))
                             'thinking)
         'budget_tokens 'absent))
   `(doc (p (code/inline "budget_tokens") " is removed on current models -- sending it is a "
            "400 -- and " (code/inline "effort") " lives in " (code/inline "output_config")
            ", not in " (code/inline "thinking") ".")))

  ((test/anthropic/request/extra-merges-rather-than-appends _)
   (⊦= "{\"model\":\"claude-opus-5\",\"max_tokens\":16000,\"messages\":[{\"role\":\"user\",\"content\":\"hi\"}],\"output_config\":{\"effort\":\"low\"}}"
       (anthropic-json/write
         (anthropic-request/body (list (anthropic-message/user "hi"))
                                 effort: "high"
                                 extra: '((output_config ((effort "low")))))))
   `(doc (p "Appending would emit " (code/inline "output_config") " twice. Duplicate keys are "
            "legal JSON and the server keeps the LAST, so the caller's "
            (code/inline "effort") " would vanish with no error at all.")))

  ((test/anthropic/request/tool-choice _)
   (⊦= '((type "auto")) (anthropic-tool-choice 'auto))
   (⊦= '((type "any")) (anthropic-tool-choice 'any))
   (⊦= '((type "none")) (anthropic-tool-choice 'none))
   (⊦= '((type "tool") (name "get_weather")) (anthropic-tool-choice '(tool "get_weather")))
   (⊦= '((type "any") (disable_parallel_tool_use #t)) (anthropic-tool-choice 'any #t))
   (⊦raises (anthropic-config-error) (anthropic-tool-choice 'banana))
   (⊦= "{\"model\":\"claude-opus-5\",\"max_tokens\":16000,\"messages\":[{\"role\":\"user\",\"content\":\"hi\"}],\"tool_choice\":{\"type\":\"tool\",\"name\":\"get_weather\",\"disable_parallel_tool_use\":true}}"
       (anthropic-json/write
         (anthropic-request/body (list (anthropic-message/user "hi"))
                                 tool-choice: '(tool "get_weather") disable-parallel: #t))))

  ((test/anthropic/request/streaming-defaults _)
   (let1 (body (anthropic-request/body (list (anthropic-message/user "Write a haiku")) stream: #t))
     (⊦= 64000 (anthropic-json/ref body 'max_tokens))
     (⊦= #t (anthropic-json/ref body 'stream)))
   (⊦= 16000 (anthropic-json/ref (anthropic-request/body (list (anthropic-message/user "hi")))
                                 'max_tokens))
   (⊦= "{\"model\":\"claude-opus-5\",\"max_tokens\":64000,\"messages\":[{\"role\":\"user\",\"content\":\"Write a haiku\"}],\"stream\":true}"
       (anthropic-json/write
         (anthropic-request/body (list (anthropic-message/user "Write a haiku")) stream: #t))))

  ((test/anthropic/request/tool-result-turn _)
   (⊦= "{\"model\":\"claude-opus-5\",\"max_tokens\":16000,\"messages\":[{\"role\":\"user\",\"content\":\"weather in Paris?\"},{\"role\":\"assistant\",\"content\":[{\"type\":\"tool_use\",\"id\":\"toolu_01\",\"name\":\"get_weather\",\"input\":{\"location\":\"Paris\"}}]},{\"role\":\"user\",\"content\":[{\"type\":\"tool_result\",\"tool_use_id\":\"toolu_01\",\"content\":\"72F and sunny\"}]}]}"
       (anthropic-json/write
         (anthropic-request/body
           (list (anthropic-message/user "weather in Paris?")
                 (anthropic-message/assistant
                   (vector (anthropic-block/tool-use "toolu_01" "get_weather" '((location "Paris")))))
                 (anthropic-message/user
                   (list (anthropic-block/tool-result "toolu_01" "72F and sunny")))))))
   `(doc (p "A user turn may be a plain string on one message and an array of blocks on the "
            "next -- both shapes are legal in the same request.")))

  ((test/anthropic/request/stop-sequences _)
   (⊦= "{\"model\":\"claude-opus-5\",\"max_tokens\":16000,\"messages\":[{\"role\":\"user\",\"content\":\"hi\"}],\"stop_sequences\":[\"END\"]}"
       (anthropic-json/write
         (anthropic-request/body (list (anthropic-message/user "hi")) stop-sequences: '("END")))))

  ((test/anthropic/request/headers _)
   (parameterize ((anthropic/api-key "sk-ant-test"))
     (⊦= '(("content-type" "application/json")
           ("x-api-key" "sk-ant-test")
           ("anthropic-version" "2023-06-01"))
         (anthropic-request/headers))
     (⊦= '(("content-type" "application/json")
           ("x-api-key" "sk-ant-test")
           ("anthropic-version" "2023-06-01")
           ("anthropic-beta" "files-api-2025-04-14,context-1m-2025-08-07"))
         (anthropic-request/headers betas: '("files-api-2025-04-14" "context-1m-2025-08-07"))))
   `(doc (p "With no betas the header must be ABSENT, not empty. No "
            (code/inline "Authorization") " header is ever added -- sending it alongside "
            (code/inline "x-api-key") " is a 401. And "
            (code/inline "content-type") " is part of the transport CONTRACT, not a curl "
            "flag, so a replacement backend that faithfully sends the list it is handed "
            "sends it too.")))

  ((test/anthropic/request/no-key-is-a-config-error _)
   ; anthropic/env-api-key is stubbed, NOT merely anthropic/api-key: with only the latter set
   ; to #f the `or` in anthropic-api-key/current falls through to the real environment, and
   ; this case would go RED on any box with ANTHROPIC_API_KEY exported -- i.e. on exactly the
   ; box about to run the live suite.  Green-without-a-key/red-with-one is the dangerous
   ; direction, so the environment is stubbed out here rather than merely shadowed.
   (parameterize ((anthropic/env-api-key (λ () #f)))
     (parameterize ((anthropic/api-key #f))
       (⊦raises (anthropic-config-error) (anthropic-request/headers)))
     (parameterize ((anthropic/api-key ""))
       (⊦raises (anthropic-config-error) (anthropic-request/headers)))
     (parameterize ((anthropic/api-key #f) (anthropic/env-api-key (λ () "")))
       (⊦raises (anthropic-config-error) (anthropic-request/headers))))
   `(doc (p "An EMPTY " (code/inline "ANTHROPIC_API_KEY") " must be treated as unset: "
            (code/inline "\"\"") " is truthy in Scheme, and an "
            (code/inline "export ANTHROPIC_API_KEY=") " that silently produced a 401 would "
            "also defeat the live suite's skip gate.")))

  ; -- messages and blocks -------------------------------------------------------------------

  ((test/anthropic/blocks/constructors _)
   (⊦= '((type "text") (text "hi")) (anthropic-block/text "hi"))
   (⊦= '((type "tool_use") (id "t1") (name "f") (input ((a 1))))
       (anthropic-block/tool-use "t1" "f" '((a 1))))
   (⊦= '((type "tool_result") (tool_use_id "t1") (content "ok"))
       (anthropic-block/tool-result "t1" "ok"))
   (⊦= '((type "tool_result") (tool_use_id "t1") (content "bad") (is_error #t))
       (anthropic-block/tool-result "t1" "bad" is-error: #t))
   (⊦= "{\"type\":\"tool_result\",\"tool_use_id\":\"t3\",\"content\":[{\"type\":\"text\",\"text\":\"42\"}]}"
       (anthropic-json/write
         (anthropic-block/tool-result "t3" (list (anthropic-block/text "42")))))
   `(doc (p (code/inline "is_error") " is OMITTED when false rather than sent as "
            (code/inline "false") ", and a block-list content is normalised to a vector -- a "
            "list would encode as a JSON object and take the encoder down the key/value path "
            "with " (code/inline "(type \"text\")") " as a key.")))

  ((test/anthropic/blocks/empty-array-is-a-vector _)
   (⊦= "{\"content\":[]}" (anthropic-json/write '((content #()))))
   (⊦= "{\"content\":{}}" (anthropic-json/write '((content ()))))
   (⊦= #() (anthropic-content '()))
   (⊦= #(1 2) (anthropic-content '(1 2)))
   (⊦= "hi" (anthropic-content "hi"))
   `(doc (p "The single most likely silent 400 in the module: an empty Scheme list encodes "
            "as an empty OBJECT.")))

  ((test/anthropic/messages/accessors _)
   (let1 (m (anthropic-message/user "hi"))
     (⊦= "user" (anthropic-message-role m))
     (⊦= "hi" (anthropic-message-content m))
     (⊦= (list (anthropic-block/text "hi")) (anthropic-message-blocks m)))
   (let1 (m (anthropic-message/assistant (list (anthropic-block/text "a") (anthropic-block/text "b"))))
     (⊦= "assistant" (anthropic-message-role m))
     (⊦= 2 (length (anthropic-message-blocks m)))
     (⊦= 2 (length (anthropic-blocks/type "text" (anthropic-message-blocks m))))
     (⊦= '() (anthropic-blocks/type "tool_use" (anthropic-message-blocks m))))
   `(doc (p (code/inline "anthropic-message-blocks") " always answers a LIST, and lifts a "
            "string content into one text block: " (code/inline "match/first")
            " has no variable-length vector pattern, so nothing may walk the raw vector.")))

  ((test/anthropic/messages/validate _)
   (⊦raises (anthropic-config-error) (anthropic-messages/validate! '()))
   (⊦raises (anthropic-config-error)
            (anthropic-messages/validate! (list (anthropic-message/assistant "hi"))))
   (⊦raises (anthropic-config-error)
            (anthropic-messages/validate! (list (anthropic-message/user "a")
                                                (anthropic-message/user "b"))))
   (⊦raises (anthropic-config-error)
            (anthropic-messages/validate! (list (anthropic-message/user ""))))
   (⊦raises (anthropic-config-error)
            (anthropic-messages/validate! (list (anthropic-message/user '()))))
   (⊦raises (anthropic-config-error)
            (anthropic-messages/validate! (list `((role "system") (content "x")))))
   ; Two adjacent ASSISTANT turns are legal -- that is what a pause_turn resume sends back.
   (⊦= (void) (anthropic-messages/validate!
                (list (anthropic-message/user "a")
                      (anthropic-message/assistant "b")
                      (anthropic-message/assistant "c"))))
   (⊦= (void) (anthropic-messages/validate! (list (anthropic-message/user "a")
                                                  (anthropic-message/assistant "b")
                                                  (anthropic-message/user "c"))))
   `(doc (p "Three remote 400s turned into local errors. The TRAILING role is deliberately "
            "not checked here: a " (code/inline "pause_turn") " continuation legitimately "
            "ends with an assistant message. " (code/inline "anthropic/converse")
            " checks the trailing role of the INITIAL transcript instead.")))

  ; -- response decoding ----------------------------------------------------------------------

  ((test/anthropic/response/decode _)
   (let1 (r (anthropic-response/decode 200 '() fixture/response/text))
     (⊦= 200 (anthropic-response-status r))
     (⊦= fixture/response/text (anthropic-response-body r))
     (⊦= '() (anthropic-response-input-errors r))
     (⊦= "msg_01" (anthropic-response-id r))
     (⊦= "claude-opus-5" (anthropic-response-model r))
     (⊦= "end_turn" (anthropic-response-stop-reason r))
     (⊦= (void) (anthropic-response-stop-details r))
     (⊦= #(((type "text") (text "Paris."))) (anthropic-response-content r))
     (⊦= "Paris." (anthropic-response/text r))))

  ((test/anthropic/response/decode-rejects-non-messages _)
   (⊦raises (anthropic-decode-error) (anthropic-response/decode 200 '() fixture/error/non-json))
   (⊦raises (anthropic-decode-error) (anthropic-response/decode 200 '() fixture/error/truncated))
   (⊦raises (anthropic-decode-error) (anthropic-response/decode 200 '() fixture/error/400))
   `(doc (p "A 200 whose body is a well-formed JSON object but not a "
            (code/inline "\"type\":\"message\"") " is a decode error, not a response.")))

  ((test/anthropic/response/usage _)
   (let1 (r (anthropic-response/decode 200 '() fixture/response/text))
     (⊦= 12 (anthropic-usage/input-tokens r))
     (⊦= 4 (anthropic-usage/output-tokens r))
     (⊦= 30 (anthropic-usage/cache-read-tokens r))
     (⊦= 0 (anthropic-usage/cache-creation-tokens r))
     (⊦= 42 (anthropic-usage/prompt-tokens r)))
   (let1 (r (anthropic-response/decode 200 '() fixture/response/tool-use))
     (⊦= 0 (anthropic-usage/cache-read-tokens r))
     (⊦= 40 (anthropic-usage/prompt-tokens r))
     (⊦= 7 (anthropic-usage/ref r 'nope 7)))
   `(doc (p (code/inline "input_tokens") " is the UNCACHED remainder only; the total prompt "
            "size is the sum of the three. The cache counters are absent on a non-cached "
            "response and must default to 0, not raise.")))

  ((test/anthropic/response/text-folds-every-block _)
   (⊦= "Paris." (anthropic-response/text (anthropic-response/decode 200 '() fixture/response/text)))
   (⊦= "Let me check the weather."
       (anthropic-response/text (anthropic-response/decode 200 '() fixture/response/tool-use)))
   (⊦= "Done." (anthropic-response/text (anthropic-response/decode 200 '() fixture/response/thinking)))
   (⊦= "" (anthropic-response/text (anthropic-response/decode 200 '() fixture/response/refusal)))
   (⊦= '() (anthropic-response/blocks (anthropic-response/decode 200 '() fixture/response/refusal)))
   `(doc (p "A refusal can arrive with an EMPTY content array, so nothing may index "
            (code/inline "content[0]") ".")))

  ((test/anthropic/response/tool-uses-and-thinking _)
   (let1 (r (anthropic-response/decode 200 '() fixture/response/parallel-tool-use))
     (⊦= '() (anthropic-blocks/type "text" (anthropic-response/blocks r)))
     (⊦= 2 (length (anthropic-response/tool-uses r)))
     (⊦= '("toolu_01" "toolu_02")
         (map (λ (b) (anthropic-json/ref b 'id)) (anthropic-response/tool-uses r)))
     (⊦= '((location "Paris")) (anthropic-json/ref (first (anthropic-response/tool-uses r)) 'input)))
   (let1 (r (anthropic-response/decode 200 '() fixture/response/thinking))
     (⊦= 1 (length (anthropic-response/thinking r)))
     (⊦= "" (anthropic-json/ref (first (anthropic-response/thinking r)) 'thinking))
     (⊦= "EqQBCgIYAhIM" (anthropic-json/ref (first (anthropic-response/thinking r)) 'signature))))

  ((test/anthropic/response/refusal-branches-on-stop-reason-only _)
   (⊨ (anthropic-response/refusal? (anthropic-response/decode 200 '() fixture/response/refusal)))
   (⊨ (anthropic-response/refusal?
        (anthropic-response/decode 200 '() fixture/response/refusal/null-details)))
   (⊭ (anthropic-response/refusal? (anthropic-response/decode 200 '() fixture/response/text)))
   (⊦= '((category "cyber") (explanation "declined"))
       (anthropic-response-stop-details (anthropic-response/decode 200 '() fixture/response/refusal)))
   (⊦= (void)
       (anthropic-response-stop-details
         (anthropic-response/decode 200 '() fixture/response/refusal/null-details)))
   `(doc (p (code/inline "stop_details") " is informational and is null even on some "
            "refusals, so a predicate that tested it would silently classify a real refusal "
            "as a success.")))

  ((test/anthropic/response/unknown-block-survives _)
   (let1 (r (anthropic-response/decode 200 '() fixture/response/unknown-block))
     (⊦= "" (anthropic-response/text r))
     (⊦= 1 (vector-length (anthropic-response-content r)))
     (⊦= "fallback" (anthropic-block-type (first (anthropic-response/blocks r))))
     (⊦= "no_capacity" (anthropic-json/ref (first (anthropic-response/blocks r)) 'reason)))
   `(doc (p "Block dispatch needs a total fallthrough: server-tool and "
            (code/inline "fallback") " blocks must be preserved verbatim, not crash the "
            "walker and not be dropped from a replayed turn.")))

  ((test/anthropic/response/message-is-verbatim _)
   (let* ((r (anthropic-response/decode 200 '() fixture/response/thinking))
          (m (anthropic-response/message r)))
     (⊦= "assistant" (anthropic-message-role m))
     (⊦= (anthropic-response-content r) (anthropic-message-content m))
     (⊦= (anthropic-json/ref (anthropic-response-json r) 'content)
         (anthropic-json/ref (anthropic-json/parse (anthropic-json/write m)) 'content)))
   `(doc (p "The assistant turn that goes back is the DECODED content, untouched. A thinking "
            "block whose text is the empty string must be replayed exactly as received: the "
            "API rejects blocks whose content has been MODIFIED, and a dropped block can "
            "trigger an ordering or signature 400.")))

  ((test/anthropic/response/request-id _)
   (⊦= "req_hdr"
       (anthropic-response-request-id
         (anthropic-response/decode 200 '(("request-id" "req_hdr")) fixture/response/text)))
   (⊦= #f (anthropic-response-request-id (anthropic-response/decode 200 '() fixture/response/text)))
   `(doc (p "The header wins over the body. Neither was observed on a live 401 during the "
            "design pass, so nothing may DEPEND on it -- but it is what a user quotes to "
            "support, and throwing it away is worse than reading it defensively.")))

  ; -- headers, status, retry arithmetic ------------------------------------------------------

  ((test/anthropic/headers/parse _)
   (⊦= '("retry-after" "7") (anthropic-header/parse "Retry-After: 7"))
   (⊦= '("date" "Tue, 16 Sep 2026 00:00:00 GMT")
       (anthropic-header/parse "Date: Tue, 16 Sep 2026 00:00:00 GMT"))
   (⊦= '("content-type" "text/event-stream") (anthropic-header/parse "content-type:text/event-stream"))
   (⊦= #f (anthropic-header/parse "no colon here"))
   (⊦= #f (anthropic-header/parse ":status"))
   (⊦= "7" (anthropic-header/ref '(("retry-after" "7")) "Retry-After"))
   (⊦= #f (anthropic-header/ref '(("retry-after" "7")) "x-should-retry"))
   (⊦= 'd (anthropic-header/ref '() "anything" 'd))
   `(doc (p "The split is on the FIRST colon, so a Date value's own colons survive, and the "
            "name is lowercased on the way in so that lookups can be exact.")))

  ((test/anthropic/headers/injection-guard _)
   (⊦raises (anthropic-config-error) (anthropic-header/check! "x-api-key" "sk\r\nEvil: 1"))
   (⊦raises (anthropic-config-error) (anthropic-header/check! "x-api\nkey" "sk"))
   (⊦= (void) (anthropic-header/check! "x-api-key" "sk-ant-ok")))

  ((test/anthropic/status/classification _)
   (⊨ (anthropic-status/ok? 200))
   (⊨ (anthropic-status/ok? 299))
   (⊭ (anthropic-status/ok? 300))
   (⊭ (anthropic-status/ok? 199))
   (for-each (λ (s) (⊨ (anthropic-status/retryable? s '()))) '(429 500 502 529 599))
   (for-each (λ (s) (⊭ (anthropic-status/retryable? s '()))) '(400 401 402 403 404 413))
   (⊭ (anthropic-status/retryable? 500 '(("x-should-retry" "false"))))
   (⊨ (anthropic-status/retryable? 400 '(("x-should-retry" "true"))))
   `(doc (p "A range test, not membership in " (code/inline "{429,500,529}")
            ": the whole 5xx class is the server-error class, while 402 "
            (code/inline "billing_error") " and 413 " (code/inline "request_too_large")
            " are correctly excluded. " (code/inline "x-should-retry") " was NOT observed on "
            "a live response, so it is read defensively and nothing depends on it.")))

  ((test/anthropic/retry/delay-schedule _)
   (parameterize ((anthropic/backoff-jitter #f) (anthropic/backoff 1.0) (anthropic/backoff-cap 30.0))
     (⊦= 1.0 (anthropic-retry/delay 0 '()))
     (⊦= 2.0 (anthropic-retry/delay 1 '()))
     (⊦= 4.0 (anthropic-retry/delay 2 '()))
     (⊦= 30.0 (anthropic-retry/delay 9 '()))
     (⊦= 7.0 (anthropic-retry/delay 0 '(("retry-after" "7"))))
     (⊦= 30.0 (anthropic-retry/delay 0 '(("retry-after" "86400"))))
     (⊦= 1.0 (anthropic-retry/delay 0 '(("retry-after" "not-a-number")))))
   `(doc (p (code/inline "retry-after") " wins, but it is capped like everything else: an "
            "uncapped header of 86400 would put the client to sleep for a day, three times.")))

  ((test/anthropic/retry/jitter-stays-inside-the-window _)
   (parameterize ((anthropic/backoff-jitter #t) (anthropic/backoff 1.0) (anthropic/backoff-cap 30.0))
     (for-each (λ (n)
                 (let1 (d (anthropic-retry/delay 2 '()))
                   (⊨ (and (>= d 0.0) (< d 4.001)))))
               (ι 20)))
   `(doc (p "Full jitter, uniform in " (code/inline "[0, window)") ", decorrelates a herd of "
            "clients that were rate-limited together. The parameter exists so that the "
            "schedule above is deterministic under test.")))

  ((test/anthropic/transport/diagnosis-and-transient _)
   (for-each (λ (c) (⊨ (anthropic-transport/transient? c))) '(7 18 28 52 55 56))
   (for-each (λ (c) (⊭ (anthropic-transport/transient? c))) '(0 1 6 22 23 35 126 127))
   (⊦= "could not resolve host" (anthropic-transport/diagnosis 6))
   (⊦= "could not connect" (anthropic-transport/diagnosis 7))
   (⊦≠ #f (substring-index "could not be executed" (anthropic-transport/diagnosis 126)))
   (⊦≠ #f (substring-index "999" (anthropic-transport/diagnosis 999)))
   ; the aliases are the same objects, not copies that can drift
   (⊨ (eq? anthropic-transport/transient? anthropic-curl/transient?))
   (⊨ (eq? anthropic-transport/diagnosis anthropic-curl/diagnosis))
   `(doc (p "6 (DNS) and 35 (TLS) are permanent for this request; 7, 18, 28, 52, 55 and 56 "
            "are transient. Getting the split wrong costs a retry that cannot help, or a "
            "retry that was never attempted. 1, 126 and 127 are local mistakes, never "
            "transient: a missing " (code/inline "curl") " surfaces as exit 126 with "
            "CHICKEN's own " (code/inline "cannot execute process") " text on the child's "
            "stderr -- " (code/inline "process*") " does NOT raise, so nothing else would "
            "tell you.")))

  ; -- the curl transport, without running curl -------------------------------------------------

  ((test/anthropic/curl/status-line _)
   (⊦= 429 (anthropic-curl/status-line "HTTP/2 429 "))
   (⊦= 200 (anthropic-curl/status-line "HTTP/1.1 200 OK"))
   (⊦= 100 (anthropic-curl/status-line "HTTP/1.1 100 Continue"))
   (⊦= 503 (anthropic-curl/status-line "HTTP/2 503"))
   (⊦= #f (anthropic-curl/status-line "content-type: application/json"))
   (⊦= #f (anthropic-curl/status-line "HTTP/2 banana"))
   (⊦= #f (anthropic-curl/status-line ""))
   `(doc (p "HTTP/2 omits the reason phrase and leaves a trailing space; HTTP/1.1 keeps it. "
            "Both forms must parse, and a header line must not be mistaken for one.")))

  ((test/anthropic/curl/read-headers-skips-1xx _)
   (let1 (p (open-input-string
              (conc "HTTP/1.1 100 Continue\r\n\r\n"
                    "HTTP/2 429 \r\n"
                    "Content-Type: application/json\r\n"
                    "Retry-After: 3\r\n"
                    "Date: Tue, 16 Sep 2026 00:00:00 GMT\r\n"
                    "\r\n"
                    "{\"type\":\"error\"}")))
     (receive (status headers) (anthropic-curl/read-headers p)
       (⊦= 429 status)
       (⊦= '(("content-type" "application/json")
             ("retry-after" "3")
             ("date" "Tue, 16 Sep 2026 00:00:00 GMT"))
           headers)
       (⊦= "{\"type\":\"error\"}" (anthropic-port->string p))))
   `(doc (p "The port is left positioned at the FIRST BODY BYTE, which is the whole point of "
            (code/inline "--include") ": the same port then goes either to the slurper or to "
            "the SSE decoder, and the blocking and streaming paths are identical up to here. "
            "A " (code/inline "100 Continue") " prelude is its own header block and must not "
            "become the answer.")))

  ((test/anthropic/curl/argv-never-carries-the-key _)
   (parameterize ((anthropic/api-key anthropic-test/key))
     (let1 (path (anthropic-curl/headers->file! (anthropic-request/headers)))
       (⊦= #o600 (file-permissions path))
       (⊦= "content-type: application/json\nx-api-key: sk-ant-api03-TESTKEY\nanthropic-version: 2023-06-01\n"
           (with-input-from-file path (τ (anthropic-port->string (current-input-port)))))
       (let1 (argv (anthropic-curl/argv "https://example.invalid/v1/messages" path #f))
         (⊭ (any (λ (a) (and (substring-index anthropic-test/key a) #t)) argv))
         (⊨ (member? "--data-binary" argv))
         (⊨ (member? "@-" argv))
         (⊨ (member? "--include" argv))
         (⊨ (member? (conc "@" path) argv))
         (⊨ (member? "https://example.invalid/v1/messages" argv))
         (⊭ (member? "--fail-with-body" argv))
         (⊭ (member? "--location" argv))
         (⊭ (member? "--no-buffer" argv)))
       (⊨ (member? "--no-buffer" (anthropic-curl/argv "https://x/" path #t)))
       (delete-file* path)
       (⊭ (file-exists? path))))
   `(doc (p "Neither the key nor the prompt is ever an argv element, so neither appears in "
            (code/inline "ps") ". The header file is created under a temporarily tightened "
            (code/inline "file-creation-mode") " of " (code/inline "#o077") " -- "
            (code/inline "create-temporary-file") " otherwise yields 0644 and "
            "chmod-after-create leaves a window in which the key is world readable. "
            (code/inline "--fail-with-body") " is absent on purpose: with it curl exits 22 "
            "for every HTTP error and a 429 becomes indistinguishable from a 404.")))

  ((test/anthropic/transport/wire-record _)
   (let1 (w (make-anthropic-wire 200 '(("content-type" "application/json"))
                                 '(curl "--include")
                                 (open-input-string fixture/response/text)
                                 (τ (values 0 ""))))
     (⊨ (anthropic-wire? w))
     (⊦= 200 (anthropic-wire-status w))
     (⊦= '(curl "--include") (anthropic-wire-origin w))
     (⊦= fixture/response/text (anthropic-wire->body! w)))
   (let1 (w (make-anthropic-wire 500 '() '(curl) (open-input-string fixture/error/529)
                                 (τ (values 0 ""))))
     (⊦raises/api (500 "overloaded_error") (anthropic-wire->body! w)))
   (let1 (w (make-anthropic-wire 200 '() '(stub "https://x/") (open-input-string "")
                                 (τ (values 28 "curl: (28) timed out"))))
     (⊦raises (anthropic-transport-error) (anthropic-wire/drain! w)))
   `(doc (p (code/inline "anthropic-wire/drain!") " looks only at the transport's exit "
            "status; " (code/inline "anthropic-wire->body!") " adds the HTTP status check on "
            "top. Splitting them is what lets the streaming path report a cut connection "
            "without pretending the HTTP response was an error. The field is called "
            (code/inline "origin") " and not " (code/inline "command") " because the stub "
            "puts " (code/inline "(stub URL)") " there, which is not a command. Note "
            (code/inline "(read-string #f port)") " answers the EOF OBJECT, not "
            (code/inline "\"\"") ", on a port that produced nothing.")))

  ((test/anthropic/transport/port->string-over-a-custom-port _)
   ; THE landmine under any future custom-port transport, and it fails SILENTLY.  Measured on
   ; csi 6.0.1pre1: over a make-input-port with no read-bytevector: hook, the eleven-character
   ; payload below comes back from (read-string #f p) as "}" followed by TEN NUL BYTES --
   ; char codes (125 0 0 0 0 0 0 0 0 0 0), invisible in terminal output, poison to a JSON
   ; parser -- and read-string! reports a false success count on top of that, so a
   ; length-checking caller is deceived too.  The hook makes it correct.
   ;
   ; The broken half is deliberately NOT asserted: pinning a bug in the host's read-string
   ; would go red the day it is fixed, which is the wrong signal.  What this case pins is that
   ; a port built to §2's contract round-trips, so a future backend has a worked example to
   ; copy and a red case if it drops the hook.
   (let1 (payload "{\"ok\":true}")
     (let1 (fixture-port
             (τ (let1 (i 0)
                  (make-input-port
                    (τ (if (< i (string-length payload))
                           (let1 (c (string-ref payload i)) (set! i (add1 i)) c)
                           #!eof))
                    (τ (< i (string-length payload)))
                    (τ (void))
                    peek: (τ (if (< i (string-length payload)) (string-ref payload i) #!eof))
                    read-bytevector:
                    (λ (bv start n)
                      (let L ((k 0))
                        (if (or (>= k n) (>= i (string-length payload)))
                            k
                            (begin
                              (bytevector-u8-set! bv (+ start k)
                                                  (char->integer (string-ref payload i)))
                              (set! i (add1 i))
                              (L (add1 k))))))))))
       (⊦= payload (anthropic-port->string (fixture-port)))
       ; the same bytes through a string port, the shape the stub actually hands back
       (⊦= payload (anthropic-port->string (open-input-string payload)))
       ; and the eof-object normalization, on a port that produced nothing at all
       (⊦= "" (anthropic-port->string (open-input-string "")))))
   `(doc (p "Every blocking body in this module funnels through "
            (code/inline "anthropic-port->string") ", which is exactly "
            (code/inline "(read-string #f port)") ". Nothing notices today -- the process "
            "backend hands back a real file port and the stub hands back a string port, and "
            "both are correct across multibyte input, 1 MB bodies and a character straddling "
            "the 4096-byte seam. A hand-built port is the one that lies, and it lies "
            "quietly: no exception, just NUL padding where the JSON was. So §2 makes "
            (code/inline "read-bytevector:") " part of the transport contract rather than "
            "advice.")))

  ; -- the blocking call over the stub ----------------------------------------------------------

  ((test/anthropic/messages/create _)
   (letstub (stub (anthropic-stub/canned fixture/response/text))
     (let1 (r (anthropic/messages (list (anthropic-message/user "capital of France?"))))
       (⊦= "end_turn" (anthropic-response-stop-reason r))
       (⊦= "Paris." (anthropic-response/text r))
       (⊦= 1 (anthropic-stub/count stub))
       (⊦= '((model "claude-opus-5")
             (max_tokens 16000)
             (messages #(((role "user") (content "capital of France?")))))
           (first (anthropic-stub/requests stub)))
       (let1 (call (first (anthropic-stub/calls stub)))
         (⊦= "https://api.anthropic.com/v1/messages" (first call))
         (⊦= '(("content-type" "application/json")
               ("x-api-key" "<redacted>")
               ("anthropic-version" "2023-06-01"))
             (second call))
         (⊦= #f (fourth call)))))
   `(doc (p "The headers come back redacted: a failing assertion over them would otherwise "
            "print a live key into the HTML report this suite writes.")))

  ((test/anthropic/messages/ask _)
   (letstub (stub (anthropic-stub/canned fixture/response/text))
     (⊦= "Paris." (anthropic/ask "capital of France?"))
     (⊦= 1 (anthropic-stub/count stub)))
   (letstub (stub (anthropic-stub/canned fixture/response/text))
     (⊦= "Paris." (anthropic/ask "hi" model: "claude-haiku-4-5" max-tokens: 4000))
     (⊦= "claude-haiku-4-5" (anthropic-json/ref (first (anthropic-stub/requests stub)) 'model))
     (⊦= 4000 (anthropic-json/ref (first (anthropic-stub/requests stub)) 'max_tokens))))

  ((test/anthropic/messages/betas-header _)
   (letstub (stub (anthropic-stub/canned fixture/response/text))
     (anthropic/messages (list (anthropic-message/user "hi")) betas: '("files-api-2025-04-14"))
     (⊦= '(("content-type" "application/json")
           ("x-api-key" "<redacted>")
           ("anthropic-version" "2023-06-01")
           ("anthropic-beta" "files-api-2025-04-14"))
         (second (first (anthropic-stub/calls stub))))))

  ((test/anthropic/messages/stub-exhaustion-raises _)
   (letstub (stub (anthropic-stub/canned fixture/response/text))
     (anthropic/messages (list (anthropic-message/user "hi")))
     (⊦raises (anthropic-config-error) (anthropic/messages (list (anthropic-message/user "hi")))))
   `(doc (p "An unscripted call is an error, not a silent replay of the last canned response. "
            "That is what lets a loop test PROVE the loop terminated.")))

  ; -- error mapping -------------------------------------------------------------------------

  ((test/anthropic/errors/400-is-not-retried _)
   (letstub (stub (anthropic-stub/canned fixture/error/400 status: 400))
     (⊦raises/api (400 "invalid_request_error")
       (anthropic/messages (list (anthropic-message/user "hi"))))
     (⊦= 1 (anthropic-stub/count stub))))

  ((test/anthropic/errors/401-and-404-are-not-retried _)
   (letstub (stub (anthropic-stub/canned fixture/error/401 status: 401))
     (⊦raises/api (401 "authentication_error")
       (anthropic/messages (list (anthropic-message/user "hi"))))
     (⊦= 1 (anthropic-stub/count stub)))
   (letstub (stub (anthropic-stub/canned fixture/error/404 status: 404))
     (⊦raises/api (404 "not_found_error")
       (anthropic/messages (list (anthropic-message/user "hi")) model: "claude-does-not-exist"))
     (⊦= 1 (anthropic-stub/count stub))))

  ((test/anthropic/errors/properties _)
   (letstub (stub (anthropic-stub/canned fixture/error/400 status: 400))
     (⊦= (list "messages: roles must alternate between \"user\" and \"assistant\""
               "req_011CSHoEeqs5C35K2UUqR7Fy"
               #f
               'anthropic-api-error)
         (condition-case (begin (anthropic/messages (list (anthropic-message/user "hi"))) 'no-error)
           (c (anthropic-api-error)
              (list (anthropic-error-ref c 'api-message)
                    (anthropic-error-ref c 'request-id)
                    (anthropic-error-retryable? c)
                    (get-condition-property c 'anthropic-error 'kind))))))
   `(doc (p (code/inline "anthropic-error-ref") " reads a property off whichever specific "
            "kind the condition carries, without the caller naming that kind.")))

  ((test/anthropic/errors/family-predicates _)
   (letstub (stub (anthropic-stub/canned fixture/error/400 status: 400))
     (⊦= '(#t #t #f)
         (condition-case (begin (anthropic/messages (list (anthropic-message/user "hi"))) 'no-error)
           (c (anthropic-api-error)
              (list (anthropic-error? c) (anthropic-api-error? c) (anthropic-transport-error? c))))))
   (⊦raises (exn) (anthropic-json/parse "nope"))
   (⊦raises (anthropic-error) (anthropic-json/parse "nope"))
   `(doc (p "Every condition the module signals carries the kinds "
            (code/inline "(exn anthropic-error KIND)") ", so " (code/inline "⊦⧳ ((exn))")
            " still catches it, the family predicate catches it, and "
            (code/inline "condition-case") " on the specific kind reaches its properties.")))

  ((test/anthropic/errors/non-json-body-never-reaches-simdjson _)
   (parameterize ((anthropic/retries 0))
     (letstub (stub (anthropic-stub/canned fixture/error/non-json status: 502
                                           headers: '(("content-type" "text/html"))))
       (⊦raises/api (502 "api_error") (anthropic/messages (list (anthropic-message/user "hi"))))))
   (parameterize ((anthropic/retries 0))
     (letstub (stub (anthropic-stub/canned fixture/error/truncated status: 200))
       (⊦raises (anthropic-decode-error) (anthropic/messages (list (anthropic-message/user "hi"))))))
   `(doc (p "A proxy's HTML page and a connection cut mid-body must both become Scheme "
            "conditions. Neither may reach the parser: "
            (code/inline "src/chicken-simdjson.cpp") " has no " (code/inline "try")
            "/" (code/inline "catch") " anywhere, so a " (code/inline "simdjson_error")
            " reaches " (code/inline "std::terminate") " and the process dies with SIGABRT -- "
            "which would take down " (code/inline "make test") " rather than fail a case.")))

  ; -- retry over the stub -----------------------------------------------------------------------

  ((test/anthropic/retry/429-then-200 _)
   (let1 (waits '())
     (parameterize ((anthropic/sleep (λ (s) (push! s waits))) (anthropic/retries 5))
       (letstub (stub (anthropic-stub/canned fixture/error/429 status: 429)
                      (anthropic-stub/canned fixture/error/429 status: 429)
                      (anthropic-stub/canned fixture/response/text))
         (⊦= "Paris." (anthropic-response/text
                        (anthropic/messages (list (anthropic-message/user "hi")))))
         (⊦= 3 (anthropic-stub/count stub))
         (⊦= '(1.0 2.0) (reverse waits))))))

  ((test/anthropic/retry/honours-retry-after _)
   (let1 (waits '())
     (parameterize ((anthropic/sleep (λ (s) (push! s waits))))
       (letstub (stub (anthropic-stub/canned fixture/error/429 status: 429
                                             headers: '(("Retry-After" "7")))
                      (anthropic-stub/canned fixture/response/text))
         (anthropic/messages (list (anthropic-message/user "hi")))
         (⊦= '(7.0) (reverse waits)))))
   `(doc (p "The header is written " (code/inline "Retry-After") " in the fixture on purpose: "
            (code/inline "anthropic-stub/canned") " lowercases names so that the stub and the "
            "curl backend agree. Without that, a test written the natural way would silently "
            "take the backoff path.")))

  ((test/anthropic/retry/gives-up-after-max _)
   (let1 (waits '())
     (parameterize ((anthropic/sleep (λ (s) (push! s waits))) (anthropic/retries 2))
       (letstub (stub (anthropic-stub/canned fixture/error/529 status: 529)
                      (anthropic-stub/canned fixture/error/529 status: 529)
                      (anthropic-stub/canned fixture/error/529 status: 529))
         (⊦raises/api (529 "overloaded_error")
           (anthropic/messages (list (anthropic-message/user "hi"))))
         (⊦= 3 (anthropic-stub/count stub))
         (⊦= '(1.0 2.0) (reverse waits))))))

  ((test/anthropic/retry/400-is-never-slept-on _)
   (let1 (waits '())
     (parameterize ((anthropic/sleep (λ (s) (push! s waits))) (anthropic/retries 5))
       (letstub (stub (anthropic-stub/canned fixture/error/400 status: 400))
         (⊦raises (anthropic-api-error) (anthropic/messages (list (anthropic-message/user "hi"))))
         (⊦= 1 (anthropic-stub/count stub))
         (⊦= '() waits)))))

  ((test/anthropic/retry/x-should-retry-false-stops _)
   (let1 (waits '())
     (parameterize ((anthropic/sleep (λ (s) (push! s waits))) (anthropic/retries 5))
       (letstub (stub (anthropic-stub/canned fixture/error/529 status: 500
                                             headers: '(("x-should-retry" "false"))))
         (⊦raises (anthropic-api-error) (anthropic/messages (list (anthropic-message/user "hi"))))
         (⊦= 1 (anthropic-stub/count stub))
         (⊦= '() waits)))))

  ; -- define-tool -------------------------------------------------------------------------------

  ((test/anthropic/tools/schema/required-and-enum _)
   (⊦= "get_weather" (anthropic-tool-name get_weather/tool))
   (⊦= "Get the current weather in a given location." (anthropic-tool-description get_weather/tool))
   (⊭ (anthropic-tool-strict get_weather/tool))
   (⊦= '((type "object")
         (properties ((location ((type "string")
                                 (description "The city and state, e.g. San Francisco, CA")))
                      (unit ((type "string")
                             (enum #("celsius" "fahrenheit"))
                             (description "Unit of temperature")))))
         (required #("location")))
       (anthropic-tool-schema get_weather/tool))
   (⊦= "{\"name\":\"get_weather\",\"description\":\"Get the current weather in a given location.\",\"input_schema\":{\"type\":\"object\",\"properties\":{\"location\":{\"type\":\"string\",\"description\":\"The city and state, e.g. San Francisco, CA\"},\"unit\":{\"type\":\"string\",\"enum\":[\"celsius\",\"fahrenheit\"],\"description\":\"Unit of temperature\"}},\"required\":[\"location\"]}}"
       (anthropic-json/write (anthropic-tool->json get_weather/tool)))
   `(doc (p "Formals carrying " (code/inline "optional") " or " (code/inline "(default ...)")
            " are dropped from " (code/inline "required") " and nothing else.")))

  ((test/anthropic/tools/schema/no-parameters _)
   (⊦= '((type "object") (properties ()) (required #())) (anthropic-tool-schema now/tool))
   (⊦= "{\"name\":\"now\",\"description\":\"Return the current time as an ISO-8601 string.\",\"input_schema\":{\"type\":\"object\",\"properties\":{},\"required\":[]}}"
       (anthropic-json/write (anthropic-tool->json now/tool)))
   `(doc (p "The asymmetry that would otherwise be an opaque 400: "
            (code/inline "properties") " is " (code/inline "'()") " and encodes as "
            (code/inline "{}") ", while " (code/inline "required") " is "
            (code/inline "#()") " and encodes as " (code/inline "[]") ". Two different "
            "Scheme values; swapping them has no other symptom.")))

  ((test/anthropic/tools/schema/strict-is-one-switch _)
   (⊨ (anthropic-tool-strict send_email/tool))
   (⊦= "{\"name\":\"send_email\",\"description\":\"Send an email.\",\"input_schema\":{\"type\":\"object\",\"properties\":{\"to\":{\"type\":\"array\",\"items\":{\"type\":\"string\"},\"description\":\"Recipient addresses\"}},\"required\":[\"to\"],\"additionalProperties\":false},\"strict\":true}"
       (anthropic-json/write (anthropic-tool->json send_email/tool)))
   `(doc (p (code/inline "strict") " and " (code/inline "additionalProperties: false")
            " are one coupled switch, not two independent ones: the API requires them "
            "together, on every generated object node.")))

  ((test/anthropic/tools/schema/compound-types _)
   (⊦= "{\"name\":\"search\",\"description\":\"Search the index\",\"input_schema\":{\"type\":\"object\",\"properties\":{\"query\":{\"type\":\"string\",\"description\":\"What to look for\"},\"limit\":{\"type\":\"integer\",\"description\":\"How many results\"},\"tags\":{\"type\":\"array\",\"items\":{\"type\":\"string\"},\"description\":\"Filter tags\"},\"deep\":{\"type\":\"boolean\",\"description\":\"Recurse?\"}},\"required\":[\"query\",\"limit\",\"tags\",\"deep\"]}}"
       (anthropic-json/write (anthropic-tool->json search/tool))))

  ((test/anthropic/tools/schema/raw-splices _)
   (⊦= '((type "object")
         (properties ((seconds ((type "integer") (minimum 0) (description "Seconds to sleep")))))
         (required #("seconds")))
       (anthropic-tool-schema sleep_for/tool))
   `(doc (p (code/inline "raw") " is an " (code/inline "unquote-splicing") " node, so the "
            "generated " (code/inline "(description ...)") " still lands AFTER the caller's "
            "fragment instead of replacing it. Note that numeric constraints such as "
            (code/inline "minimum") " are not supported under "
            (code/inline "define-tool/strict") " -- " (code/inline "raw") " and "
            (code/inline "strict") " do not compose.")))

  ((test/anthropic/tools/procedure-stays-a-procedure _)
   (⊦= "22 celsius in Paris" (get_weather "Paris"))
   (⊦= "22 fahrenheit in Paris" (get_weather "Paris" unit: "fahrenheit"))
   (⊦= "2026-09-16T00:00:00Z" (now))
   (⊦= "queued 2" (send_email #("a@x" "b@x")))
   `(doc (p "The macro defines an ordinary Scheme procedure as well as the tool record, so "
            "the body is unit-testable with no JSON anywhere in sight.")))

  ((test/anthropic/tools/dispatch _)
   (⊦= "22 celsius in Paris" (anthropic-tool/apply get_weather/tool '((location "Paris"))))
   (⊦= "22 fahrenheit in Rome"
       (anthropic-tool/apply get_weather/tool '((location "Rome") (unit "fahrenheit"))))
   (⊦= "2026-09-16T00:00:00Z" (anthropic-tool/apply now/tool '()))
   (⊦raises (anthropic-tool-error) (anthropic-tool/apply get_weather/tool '()))
   (⊦= 'missing
       (condition-case (begin (anthropic-tool/apply get_weather/tool '((unit "celsius"))) 'no-error)
         (c (anthropic-tool-error) 'missing)))
   `(doc (p "An absent OPTIONAL key falls back to the procedure's own "
            (code/inline "#!key") " default -- the single source of truth -- while an absent "
            "REQUIRED key raises, and the loop turns that raise into an "
            (code/inline "is_error") " tool result rather than a dead conversation.")))

  ((test/anthropic/tools/registry-and-wire-list _)
   (⊨ (eq? get_weather/tool (anthropic-tool/registered "get_weather")))
   (⊨ (eq? now/tool (anthropic-tool/registered "now")))
   (⊭ (anthropic-tool/registered "no_such_tool"))
   (⊨ (vector? (anthropic-tools->json (list now/tool))))
   (⊦= 2 (vector-length (anthropic-tools->json (list now/tool get_weather/tool))))
   (let1 (table (anthropic-tools->table (list get_weather/tool '((name "raw_alist")))))
     (⊨ (eq? get_weather/tool (hash-table-ref/default table "get_weather" #f)))
     (⊭ (hash-table-ref/default table "raw_alist" #f)))
   `(doc (p (code/inline "anthropic-tools->json") " accepts a hand-written alist so an "
            "un-macro-ed tool can still be advertised, but "
            (code/inline "anthropic-tools->table") " registers only records, so such a tool "
            "is answered with " (code/inline "no such tool") ". That split is deliberate and "
            "is pinned here so it cannot drift silently.")))

  ((test/anthropic/tools/result->string _)
   (⊦= "plain" (anthropic-tool-result->string "plain"))
   (⊦= "{\"a\":1}" (anthropic-tool-result->string '((a 1))))
   (⊦= "[1,2]" (anthropic-tool-result->string #(1 2)))
   (⊦≠ "" (anthropic-tool-result->string ""))
   (⊦≠ "" (anthropic-tool-result->string (void)))
   `(doc (p "Non-strings become JSON, not Scheme syntax: a vector reaching the model as "
            (code/inline "#(1 2)") " is noise it has to guess at. An EMPTY string is "
            "substituted too -- a text-bearing block with empty content is a 400.")))

  ; -- SSE ------------------------------------------------------------------------------------

  ((test/anthropic/sse/framing _)
   (let1 (events (anthropic-sse/events (open-input-string fixture/sse/text)))
     (⊦= '("message_start" "content_block_start" "ping" "content_block_delta"
           "content_block_delta" "content_block_stop" "message_delta" "message_stop")
         (map anthropic-event-name events))
     (⊦= '("Hello" " world") (filter (λ (t) t) (map anthropic-event/text events))))
   `(doc (p "A line beginning with " (code/inline ":") " is a comment -- that is how the API "
            "keeps a connection warm -- and it produces no event at all.")))

  ((test/anthropic/sse/crlf-and-multiline-data _)
   (let1 (events (anthropic-sse/events
                   (open-input-string
                     (conc "event: message_stop\r\ndata: {\"type\":\r\ndata: \"message_stop\"}\r\n\r\n"))))
     (⊦= 1 (length events))
     (⊦= "message_stop" (anthropic-event-name (first events)))
     (⊦= '((type "message_stop")) (anthropic-event-data (first events))))
   `(doc (p "Repeated " (code/inline "data:") " lines join with a newline, per the SSE spec. "
            "The API sends one today; the joined form is unexercised against the real "
            "service and is here so it does not silently break if that changes.")))

  ((test/anthropic/sse/unknown-frames-are-tolerated _)
   (let1 (events (anthropic-sse/events
                   (open-input-string
                     (conc "event: some_future_event\ndata: {\"type\":\"some_future_event\"}\n\n"
                           "event: weird\ndata: not json at all\n\n"))))
     (⊦= '("some_future_event" "weird") (map anthropic-event-name events))
     (⊦= (void) (anthropic-event-data (second events)))
     (⊦= "not json at all" (anthropic-event-raw (second events)))
     (⊦= '() (filter (λ (t) t) (map anthropic-event/text events))))
   `(doc (p "An event whose data is absent or unparseable keeps " (code/inline "(void)")
            " rather than raising: the wire carries comments, pings and event types that do "
            "not exist yet, and none of those may kill a stream. Only the accumulator "
            "raises, and only for the frames it claims to understand.")))

  ((test/anthropic/sse/assembles-a-text-turn _)
   (receive (message errors)
       (anthropic-sse->message (anthropic-sse/events (open-input-string fixture/sse/text)))
     (⊦= '() errors)
     (⊦= `((id "msg_s1") (type "message") (role "assistant") (model "claude-opus-5")
           (content #(((type "text") (text "Hello world"))))
           (stop_reason "end_turn")
           (stop_sequence ,(void))
           (stop_details ,(void))
           (usage ((input_tokens 10) (output_tokens 12))))
         message))
   `(doc (p "The blocks are installed IN PLACE in the " (code/inline "message_start")
            " skeleton, so every key the server sent keeps its position. "
            (code/inline "stop_reason") " arrives on " (code/inline "message_delta")
            ", never on " (code/inline "message_stop") ", and the final "
            (code/inline "usage") " is the MERGE of the two -- "
            (code/inline "message_delta") " carries only " (code/inline "output_tokens") ".")))

  ((test/anthropic/sse/assembles-thinking-text-and-tool-use _)
   (receive (message errors)
       (anthropic-sse->message (anthropic-sse/events (open-input-string fixture/sse/tool-use)))
     (⊦= '() errors)
     (⊦= #(((type "thinking") (thinking "Look it up.") (signature "EqQBCgIY"))
           ((type "text") (text "Checking."))
           ((type "tool_use") (id "toolu_01") (name "get_weather") (input ((location "Paris")))))
         (anthropic-json/ref message 'content))
     (⊦= "tool_use" (anthropic-json/ref message 'stop_reason))
     (⊦= 45 (anthropic-json/ref (anthropic-json/ref message 'usage) 'output_tokens)))
   `(doc (p "Three indices, reassembled in index order. The "
            (code/inline "input_json_delta") " fragments split "
            (code/inline "{\"location\": \"Paris\"}") " mid-token and are buffered as raw "
            "TEXT, parsed exactly once at " (code/inline "content_block_stop") " -- feeding "
            "fragments to the parser incrementally cannot work, and with this binding it "
            "would abort the process. " (code/inline "id") " and " (code/inline "name")
            " come from " (code/inline "content_block_start") ", not from any delta, and a "
            "thinking block's " (code/inline "signature") " arrives on its own "
            (code/inline "signature_delta") ".")))

  ((test/anthropic/sse/tool-use-with-no-input-deltas _)
   (receive (message errors)
       (anthropic-sse->message (anthropic-sse/events (open-input-string fixture/sse/no-input)))
     (⊦= '() errors)
     (⊦= #(((type "tool_use") (id "toolu_02") (name "now") (input ())))
         (anthropic-json/ref message 'content)))
   `(doc (p "A parameterless tool streams " (code/inline "content_block_start") " with "
            (code/inline "\"input\":{}") " and NO " (code/inline "input_json_delta")
            " at all. A stop that parsed its empty buffer unconditionally would hand "
            (code/inline "\"\"") " to simdjson and take the process down.")))

  ((test/anthropic/sse/unparseable-tool-input-is-recorded-not-raised _)
   (receive (message errors)
       (anthropic-sse->message (anthropic-sse/events (open-input-string fixture/sse/broken-input)))
     (⊦= '(("toolu_03" "{\"location\": \"Par")) errors)
     (⊦= '() (anthropic-json/ref (vector-ref (anthropic-json/ref message 'content) 0) 'input))
     (⊦= "max_tokens" (anthropic-json/ref message 'stop_reason)))
   `(doc (p "Raising here would lose the turn's sibling blocks, its "
            (code/inline "stop_reason") " and its " (code/inline "usage")
            " -- and the caller needs the " (code/inline "stop_reason") " precisely because "
            (code/inline "max_tokens") " with a " (code/inline "tool_use") " present means "
            "STOP, retry with a bigger budget, never run the tool.")))

  ((test/anthropic/sse/cut-stream-raises _)
   (⊦raises (anthropic-sse-error)
     (anthropic-sse->message (anthropic-sse/events (open-input-string fixture/sse/cut))))
   (⊦raises (anthropic-sse-error)
     (anthropic-sse->message
       (anthropic-sse/events
         (open-input-string
           (conc "event: message_start\ndata: {\"type\":\"message_start\",\"message\":{\"id\":\"m\"}}\n\n"
                 "event: message_delta\ndata: {\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"end_turn\"}}\n\n")))))
   `(doc (p "A connection cut at 90% of a long answer must be an error, not a plausible "
            "shorter answer: an unfinished content block, or a missing "
            (code/inline "message_stop") ", both raise.")))

  ((test/anthropic/sse/error-frame-raises _)
   (⊦= (list (void) "overloaded_error")
       (condition-case
         (begin (anthropic-sse->message (anthropic-sse/events (open-input-string fixture/sse/error)))
                'no-error)
         (c (anthropic-api-error)
            (list (anthropic-error-ref c 'status (void)) (anthropic-error-ref c 'type)))))
   `(doc (p "An SSE " (code/inline "error") " frame arrives inside an HTTP 200, so it has no "
            "status of its own. It carries " (code/inline "(void)") " rather than an invented "
            "529 -- the retryability comes from the error TYPE instead.")))

  ((test/anthropic/sse/lazy-stream-runs-finish-once _)
   (let1 (finished 0)
     (let1 (§ (anthropic-sse->§ (open-input-string fixture/sse/text)
                                finish: (τ (add1! finished))))
       (⊦= 8 (anthropic-sse/fold (λ (ev n) (add1 n)) 0 §))
       (⊦= 8 (anthropic-sse/fold (λ (ev n) (add1 n)) 0 §))
       (⊦= 1 finished)))
   `(doc (p "The stream memoises, so a second traversal re-reads the same events and "
            (code/inline "finish") " runs exactly once. A caller who abandons the stream "
            "early never reaches eof and must release the wire itself.")))

  ((test/anthropic/stream/end-to-end-over-the-stub _)
   (let ((fragments '()) (events '()))
     (letstub (stub (anthropic-stub/canned fixture/sse/tool-use
                                           headers: '(("content-type" "text/event-stream"))))
       (let1 (r (anthropic/stream (list (anthropic-message/user "weather in Paris?"))
                                  tools: (list get_weather/tool)
                                  on-text: (λ (t) (push! t fragments))
                                  on-event: (λ (ev) (push! (anthropic-event-name ev) events))))
         (⊨ (anthropic-response? r))
         (⊦= "tool_use" (anthropic-response-stop-reason r))
         (⊦= "Checking." (anthropic-response/text r))
         (⊦= '("Checking.") (reverse fragments))
         (⊦= 15 (length events))
         (⊦= '(("get_weather" ((location "Paris"))))
             (map (λ (b) (list (anthropic-json/ref b 'name) (anthropic-json/ref b 'input)))
                  (anthropic-response/tool-uses r)))
         (⊦= 45 (anthropic-usage/output-tokens r))
         (⊦= 1 (anthropic-stub/count stub))
         (let1 (call (first (anthropic-stub/calls stub)))
           (⊨ (fourth call))
           (⊦= #t (anthropic-json/ref (anthropic-json/parse (third call)) 'stream))
           (⊦= 64000 (anthropic-json/ref (anthropic-json/parse (third call)) 'max_tokens))))))
   `(doc (p "The value that comes back is an ordinary " (code/inline "anthropic-response")
            ", so every accessor works unchanged whether the turn was streamed or not. The "
            (code/inline "stream?") " flag reaches the transport AND "
            (code/inline "\"stream\":true") " reaches the body -- a request missing either "
            "would return ordinary JSON that the frame reader silently swallows into an "
            "empty message.")))

  ((test/anthropic/stream/rejects-a-non-sse-response _)
   (letstub (stub (anthropic-stub/canned fixture/response/text))
     (⊦raises (anthropic-decode-error) (anthropic/stream (list (anthropic-message/user "hi")))))
   (letstub (stub (anthropic-stub/canned fixture/error/429 status: 429))
     (⊦raises/api (429 "rate_limit_error") (anthropic/stream (list (anthropic-message/user "hi")))))
   `(doc (p "A non-2xx on a streaming request answers with an ordinary JSON error body, not "
            "with SSE, so it is routed through the blocking error path before anyone starts "
            "reading frames. And a 200 whose " (code/inline "content-type")
            " is not " (code/inline "text/event-stream") " is a decode error, never an empty "
            "message.")))

  ; -- the agentic loop ----------------------------------------------------------------------

  ((test/anthropic/loop/single-tool-request-sequence _)
   (letstub (stub (anthropic-stub/canned fixture/response/tool-use)
                  (anthropic-stub/canned fixture/response/text))
     (receive (r transcript)
         (anthropic/converse (list (anthropic-message/user "weather in Paris?"))
                             tools: (list get_weather/tool))
       (⊦= "end_turn" (anthropic-response-stop-reason r))
       (⊦= "Paris." (anthropic-response/text r))
       (⊦= 2 (anthropic-stub/count stub))
       (⊦= '("user" "assistant" "user" "assistant") (map anthropic-message-role transcript))
       (⊦= (list
             #(((role "user") (content "weather in Paris?")))
             #(((role "user") (content "weather in Paris?"))
               ((role "assistant")
                (content #(((type "text") (text "Let me check the weather."))
                           ((type "tool_use") (id "toolu_01") (name "get_weather")
                            (input ((location "Paris")))))))
               ((role "user")
                (content #(((type "tool_result") (tool_use_id "toolu_01")
                            (content "22 celsius in Paris")))))))
           (map (λ (body) (anthropic-json/ref body 'messages)) (anthropic-stub/requests stub)))
       (⊭ (any (λ (body) (not (anthropic-json/has? body 'tools)))
               (anthropic-stub/requests stub)))))
   `(doc (p "The loop invariant asserted on the bytes: the transcript alternates user / "
            "assistant / user, the assistant turn is replayed as its WHOLE content array "
            "(the text block is not dropped), and " (code/inline "tools")
            " is repeated on the follow-up request -- omitting it there is a 400.")))

  ((test/anthropic/loop/parallel-tools-in-one-user-message _)
   (letstub (stub (anthropic-stub/canned fixture/response/parallel-tool-use)
                  (anthropic-stub/canned fixture/response/text))
     (anthropic/converse (list (anthropic-message/user "Paris and Rome?"))
                         tools: (list get_weather/tool))
     (let1 (msgs (anthropic-json/ref (second (anthropic-stub/requests stub)) 'messages))
       (⊦= 3 (vector-length msgs))
       (⊦= `((role "user")
             (content #(((type "tool_result") (tool_use_id "toolu_01")
                         (content "22 celsius in Paris"))
                        ((type "tool_result") (tool_use_id "toolu_02")
                         (content "22 fahrenheit in Rome")))))
           (vector-ref msgs 2))))
   `(doc (p "Two " (code/inline "tool_use") " blocks produce ONE user message holding two "
            (code/inline "tool_result") " blocks, in block order, each echoing its matching "
            (code/inline "tool_use_id") ". Splitting them across two user messages is a 400 "
            "on the alternation rule.")))

  ((test/anthropic/loop/failures-become-is-error _)
   (letstub (stub (anthropic-stub/canned fixture/response/mixed-tool-use)
                  (anthropic-stub/canned fixture/response/text))
     (anthropic/converse (list (anthropic-message/user "go"))
                         tools: (list get_weather/tool explode/tool))
     (let1 (results (anthropic-json/vector->list
                      (anthropic-json/ref
                        (vector-ref (anthropic-json/ref (second (anthropic-stub/requests stub))
                                                        'messages)
                                    2)
                        'content)))
       (⊦= 3 (length results))
       (⊦= '("toolu_01" "toolu_02" "toolu_03")
           (map (λ (b) (anthropic-json/ref b 'tool_use_id)) results))
       (⊦= '(absent #t #t) (map (λ (b) (anthropic-json/ref b 'is_error 'absent)) results))
       (⊦= "22 celsius in Paris" (anthropic-json/ref (first results) 'content))
       (⊦≠ #f (substring-index "boom: now" (anthropic-json/ref (second results) 'content)))
       (⊦≠ #f (substring-index "ghost" (anthropic-json/ref (third results) 'content)))))
   `(doc (p "A condition raised by one tool must not abort the map and lose its siblings' "
            "results: an unanswered " (code/inline "tool_use_id")
            " is a 400 on the very next request. A tool that raises, and a name with no "
            "matching tool, both come back as ordinary results with "
            (code/inline "is_error") " true.")))

  ((test/anthropic/loop/around-hook-denies _)
   (let1 (seen '())
     (letstub (stub (anthropic-stub/canned fixture/response/parallel-tool-use)
                    (anthropic-stub/canned fixture/response/text))
       (anthropic/converse (list (anthropic-message/user "go"))
                           tools: (list get_weather/tool)
                           around: (λ (name input run)
                                     (push! (anthropic-json/ref input 'location) seen)
                                     (if (equal? "Rome" (anthropic-json/ref input 'location))
                                         (anthropic-tool/error "denied by policy")
                                         (run))))
       (⊦= '("Paris" "Rome") (reverse seen))
       (let1 (results (anthropic-json/vector->list
                        (anthropic-json/ref
                          (vector-ref (anthropic-json/ref (second (anthropic-stub/requests stub))
                                                          'messages)
                                      2)
                          'content)))
         (⊦= '(absent #t) (map (λ (b) (anthropic-json/ref b 'is_error 'absent)) results))
         (⊦= "denied by policy" (anthropic-json/ref (second results) 'content)))))
   `(doc (p "Dispatch is an explicit left-to-right fold, not " (code/inline "map")
            " -- application order is unspecified and these calls have side effects. "
            "Denial is a RETURN VALUE convention: " (code/inline "(anthropic-tool/error ...)")
            " denies, " (code/inline "(run)") " executes, and returning a plain string "
            "denies nothing at all -- the string becomes a SUCCESSFUL result. That is easy "
            "to get wrong in an approval hook, which is exactly where getting it wrong is "
            "dangerous.")))

  ((test/anthropic/loop/invalid-streamed-tool-input _)
   (let1 (result (anthropic-tool-use->result
                   '((type "tool_use") (id "toolu_03") (name "get_weather") (input ()))
                   (anthropic-tools->table (list get_weather/tool))
                   anthropic-tool/around
                   '(("toolu_03" "{\"location\": \"Par"))))
     (⊦= "toolu_03" (anthropic-json/ref result 'tool_use_id))
     (⊦= #t (anthropic-json/ref result 'is_error))
     (⊦= "{\"INVALID_JSON\":\"{\\\"location\\\": \\\"Par\"}"
         (anthropic-json/ref result 'content)))
   `(doc (p "The documented recovery shape: a JSON document nested inside a JSON string, "
            "built with the encoder rather than by concatenation so that the quotes in the "
            "bad input are escaped. The tool is NOT run.")))

  ((test/anthropic/loop/max-tokens-with-a-tool-call-does-not-execute _)
   (let1 (before tool/calls)
     (letstub (stub (anthropic-stub/canned fixture/response/max-tokens/tool-use))
       (⊦raises/loop 'max-tokens
         (anthropic/converse (list (anthropic-message/user "go")) tools: (list counted/tool)))
       (⊦= 1 (anthropic-stub/count stub)))
     (⊦= before tool/calls))
   (letstub (stub (anthropic-stub/canned fixture/response/max-tokens/text))
     (receive (r transcript)
         (anthropic/converse (list (anthropic-message/user "go")) tools: (list get_weather/tool))
       (⊦= "max_tokens" (anthropic-response-stop-reason r))
       (⊦= "trunc" (anthropic-response/text r))
       (⊦= 2 (length transcript))))
   `(doc (p "A truncated tool input often still parses as a valid partial object, so "
            (code/inline "stop_reason") " is checked BEFORE the dispatch. Getting that "
            "ordering wrong runs side-effecting tools on truncated arguments. The counter "
            "is a module-level tool, not a local " (code/inline "let1")
            " that nothing could ever mutate.")))

  ((test/anthropic/loop/refusal-does-not-execute-tools _)
   (let1 (before tool/calls)
     (letstub (stub (anthropic-stub/canned fixture/response/refusal))
       (⊦raises/loop 'refusal
         (anthropic/converse (list (anthropic-message/user "go")) tools: (list counted/tool)))
       (⊦= 1 (anthropic-stub/count stub)))
     (⊦= before tool/calls)))

  ((test/anthropic/loop/pause-turn-appends-no-user-message _)
   (letstub (stub (anthropic-stub/canned fixture/response/pause-turn)
                  (anthropic-stub/canned fixture/response/text))
     (receive (r transcript)
         (anthropic/converse (list (anthropic-message/user "search")) tools: (list get_weather/tool))
       (⊦= "Paris." (anthropic-response/text r))
       (⊦= '("user" "assistant" "assistant") (map anthropic-message-role transcript))
       (⊦= 2 (anthropic-stub/count stub))
       (⊦= #(((role "user") (content "search"))
             ((role "assistant")
              (content #(((type "text") (text "Searching..."))
                         ((type "server_tool_use") (id "srvtoolu_01") (name "web_search")
                          (input ((query "weather"))))))))
           (anthropic-json/ref (second (anthropic-stub/requests stub)) 'messages))))
   (letstub (stub (anthropic-stub/canned fixture/response/pause-turn)
                  (anthropic-stub/canned fixture/response/pause-turn))
     (⊦raises/loop 'max-pauses
       (anthropic/converse (list (anthropic-message/user "search"))
                           tools: (list get_weather/tool) max-pauses: 1)))
   `(doc (p "The one branch that appends an assistant turn with NO following user message: "
            "the API detects the trailing " (code/inline "server_tool_use")
            " block and resumes by itself. Injecting a "
            (code/inline "\"Continue.\"") " user message is wrong, and the resend's trailing "
            "block is what makes it not-prefill. It gets its own counter because a "
            "server-side loop could otherwise pause forever.")))

  ((test/anthropic/loop/max-iterations-guard _)
   (letstub (stub (anthropic-stub/canned fixture/response/tool-use)
                  (anthropic-stub/canned fixture/response/tool-use)
                  (anthropic-stub/canned fixture/response/tool-use))
     (⊦raises/loop 'max-iterations
       (anthropic/converse (list (anthropic-message/user "go"))
                           tools: (list get_weather/tool) max-iterations: 3))
     (⊦= 3 (anthropic-stub/count stub)))
   `(doc (p "A model that keeps asking for tools must not loop forever. Exactly "
            (code/inline "max-iterations") " requests are made, then it raises.")))

  ((test/anthropic/loop/terminal-and-unknown-stop-reasons _)
   (letstub (stub (anthropic-stub/canned fixture/response/unknown-stop))
     (⊦raises/loop 'unknown-stop-reason
       (anthropic/converse (list (anthropic-message/user "go")))))
   (letstub (stub (anthropic-stub/canned fixture/response/context-window))
     (⊦raises/loop 'context-window-exceeded
       (anthropic/converse (list (anthropic-message/user "go")))))
   (letstub (stub (anthropic-stub/canned fixture/response/text))
     (receive (r transcript) (anthropic/converse (list (anthropic-message/user "go")))
       (⊦= 2 (length transcript))))
   `(doc (p "An unrecognised " (code/inline "stop_reason") " raises rather than being "
            "silently treated as " (code/inline "end_turn") ", which would truncate the "
            "answer. " (code/inline "model_context_window_exceeded")
            " gets its own reason because it is actionable -- compact or split -- rather "
            "than a protocol violation.")))

  ((test/anthropic/loop/rejects-assistant-prefill _)
   (letstub (stub (anthropic-stub/canned fixture/response/text))
     (⊦raises (anthropic-config-error)
       (anthropic/converse (list (anthropic-message/user "hi")
                                 (anthropic-message/assistant "Sure, "))))
     (⊦= 0 (anthropic-stub/count stub)))
   `(doc (p "Assistant prefill is a 400 on every current model, so the INITIAL transcript "
            "must end with the user. A " (code/inline "pause_turn")
            " continuation legitimately does not, which is why this check lives in the loop "
            "and not in " (code/inline "anthropic-messages/validate!") ".")))

  ((test/anthropic/loop/unicode-survives-the-round-trip _)
   (letstub (stub (anthropic-stub/canned fixture/response/unicode-tool)
                  (anthropic-stub/canned fixture/response/text))
     (anthropic/converse (list (anthropic-message/user "motto?")) tools: (list motto/tool))
     (⊦= "caffè 一 espresso"
         (anthropic-json/ref
           (vector-ref (anthropic-json/ref
                         (vector-ref (anthropic-json/ref (second (anthropic-stub/requests stub))
                                                         'messages)
                                     2)
                         'content)
                       0)
           'content)))
   `(doc (p "A tool result with characters outside ASCII goes through the encoder, back "
            "through the byte-length-correct parser, and comes out identical. With "
            (code/inline "simdjson-parse/ondemand") " this case would not fail -- it would "
            "ABORT the whole " (code/inline "csi") " process with SIGABRT.")))

  ; -- redaction ---------------------------------------------------------------------------------

  ((test/anthropic/redaction/replaces-the-key _)
   (parameterize ((anthropic/api-key anthropic-test/key))
     (⊦= "x-api-key: <redacted>" (anthropic/redact (conc "x-api-key: " anthropic-test/key)))
     (⊦= #f (substring-index anthropic-test/key
                             (anthropic/redact (conc "curl: (60) sent " anthropic-test/key))))
     (⊦= "nothing to hide" (anthropic/redact "nothing to hide")))
   (parameterize ((anthropic/api-key #f))
     (⊦= "no key set" (anthropic/redact "no key set")))
   (parameterize ((anthropic/api-key "short"))
     (⊦= "short" (anthropic/redact "short")))
   `(doc (p "A key shorter than eight characters is left alone: redacting a common substring "
            "would corrupt every diagnostic. Real keys are far longer.")))

  ((test/anthropic/redaction/api-error-message-is-clean _)
   (letstub (stub (anthropic-stub/canned fixture/error/401 status: 401))
     (⊦= 'clean
         (condition-case (begin (anthropic/messages (list (anthropic-message/user "hi"))) 'no-error)
           (c (anthropic-api-error)
              (if (substring-index anthropic-test/key (get-condition-property c 'exn 'message))
                  'leaked
                  'clean)))))
   `(doc (p "A 401 is exactly the error a user is most likely to paste into a bug report.")))

  ((test/anthropic/redaction/stub-log-is-clean _)
   (letstub (stub (anthropic-stub/canned fixture/response/text))
     (anthropic/messages (list (anthropic-message/user "hi")))
     (⊦= #f (substring-index anthropic-test/key
                             (->string/pretty-print (anthropic-stub/calls stub)))))
   `(doc (p "Every assertion over the stub log happens INSIDE the "
            (code/inline "parameterize") ", because the redaction resolves the CURRENT key. "
            "Outside it the raw value would come back and a failing case would write it into "
            (code/inline "testsuite-anthropic-suite.html") ".")))
  )

; House style would end here with a bare `(unittest/✓ anthropic-suite)`. It does not, on
; purpose: unittest/✓ never exits non-zero (it returns the result record at
; aux.unittest.scm:154 and nothing inspects it), so a completely broken module would still
; make `make test` -- and therefore `docker build` -- green. The HTML report is written
; before the exit, so a local run still produces testsuite-anthropic-suite.html.
;
; To opt out, replace the whole form below with `(unittest/✓ anthropic-suite)`.

(let1 (r (unittest/✓ anthropic-suite))
  (unless (null? (unittest/result-failed r))
    (print "anthropic: " (length (unittest/result-failed r))
           " assertion(s) failed; see test/testsuite-anthropic-suite.html")
    (exit 1)))
```

**Case count: 96.** That is the ninety-five cases originally drafted plus
`test/anthropic/transport/port->string-over-a-custom-port`; the suite-level `doc` entry is not a
case. §6.5 adds six live cases that are not part of `make test`, and §6.6 splices ten more offline
cases into this same suite. Three harness behaviours the file relies on and that a reader should not re-derive: a case aborts at its *first* failed assertion (`src/aux.unittest.scm:49-53` replaces the value with `witness` and returns), so a case reports only its first mismatch; a case's stdout and stderr are captured into the HTML and never reach the terminal (`:40-47`), so `print` cannot be used to signal anything; and `code/scheme` is `*preorder*` at HEAD (`src/aux.sxml.scm:182`), which is why a formal named `m` in a case body is safe here even though it crashed the published `:master` image — build from HEAD, not from a stale image.

### 6.5 `src/test/anthropic-live.scm` — the opt-in live suite

```scheme

; The LIVE suite: these cases hit https://api.anthropic.com/v1/messages for real.
;
; It is NOT in the Makefile `test:` target, so `docker build` never reaches it and never needs
; a secret -- there is no secret-injection path in .github/workflows/docker.publish.yml
; anyway. Run it by hand:
;
;     export ANTHROPIC_API_KEY=sk-ant-...
;     cd src && make test-anthropic-live
;
; With the variable unset the file prints one line and exits 0. The gate is the WHOLE FILE and
; not a per-case early return, because (aux unittest) has no skip: unittest/result has only
; `ran` and `failed` (aux.unittest.scm:94) and unittest/result-started! fires at :32 before a
; case body can decline, so a "skipped" case would be counted as a PASS and inflate `ran`. A
; top-level print DOES reach the terminal; only in-case output is captured into the report.

(import
  (scheme base)                       ; parameterize is NOT in (chicken base)
  (chicken base)
  (chicken condition)
  (chicken process-context)
  (chicken string)
  (aux base)
  (aux unittest)
  (aux anthropic))

(define-syntax-rule (⊦raises/api (s t) body ...)
  (⊦= (list s t)
      (condition-case (begin body ... 'no-error)
        (c (anthropic-api-error)
           (list (get-condition-property c 'anthropic-api-error 'status)
                 (get-condition-property c 'anthropic-api-error 'type))))))

(define-tool (add (a integer "left addend") (b integer "right addend"))
  "Add two integers and return the sum as a decimal string."
  (number->string (+ a b)))

(define-suite anthropic-live-suite

  ((doc r)
   `((p "These cases hit " (code/inline "https://api.anthropic.com/v1/messages")
        " for real and cost a fraction of a cent per run. They are NOT part of "
        (code/inline "make test") "; run them with "
        (code/inline "make test-anthropic-live") " after exporting "
        (code/inline "ANTHROPIC_API_KEY") ".")
     (p "Every case uses a generous " (code/inline "max_tokens") " and "
        (code/inline "effort: \"low\"") ". Thinking is adaptive by default on "
        (code/inline "claude-opus-5") " -- omitting " (code/inline "thinking")
        " does not turn it off -- so a small budget is routinely eaten by the thinking block "
        "and the turn comes back " (code/inline "max_tokens") " instead of "
        (code/inline "end_turn") ". That is a flaky test, not a bug.")))

  ((test/anthropic/live/hello _)
   (let1 (r (anthropic/messages (list (anthropic-message/user "Reply with exactly: pong"))
                                max-tokens: 4000 effort: "low"))
     (⊦= "end_turn" (anthropic-response-stop-reason r))
     (⊦= "message" (anthropic-json/ref (anthropic-response-json r) 'type))
     (⊦≠ "" (anthropic-response/text r))
     (⊨ (> (anthropic-usage/output-tokens r) 0))
     (⊨ (> (anthropic-usage/prompt-tokens r) 0))
     (⊨ (string? (anthropic-response-id r)))))

  ((test/anthropic/live/system-and-unicode _)
   (let1 (r (anthropic/messages
              (list (anthropic-message/user "Echo back exactly, with nothing else: caffè 一 😋"))
              system: "You are terse. Echo exactly what you are asked to echo."
              max-tokens: 4000 effort: "low"))
     (⊦= "end_turn" (anthropic-response-stop-reason r))
     (⊦≠ #f (substring-index "caffè" (anthropic-response/text r))))
   `(doc (p "The one case that proves the byte-length fix against real network bytes: an "
            "accented response decoded through " (code/inline "simdjson-parse/ondemand")
            " truncates and then aborts the process.")))

  ((test/anthropic/live/tool-roundtrip _)
   (receive (r transcript)
       (anthropic/converse
         (list (anthropic-message/user
                 "Use the add tool to compute 17 + 25, then state the resulting number."))
         tools: (list add/tool) max-tokens: 8000 effort: "low" max-iterations: 4)
     (⊦= "end_turn" (anthropic-response-stop-reason r))
     (⊨ (>= (length transcript) 4))
     (⊦≠ #f (substring-index "42" (anthropic-response/text r))))
   `(doc (p "The flakiest assertion in either file: it depends on the model actually saying "
            (code/inline "42") ". It is kept because a tool loop that never round-trips is "
            "worth catching, and it is opt-in precisely so that it gates nothing automated.")))

  ((test/anthropic/live/stream _)
   (let1 (fragments '())
     (let1 (r (anthropic/stream (list (anthropic-message/user "Write a haiku about Scheme."))
                                max-tokens: 8000 effort: "low"
                                on-text: (λ (s) (push! s fragments))))
       (⊦= "end_turn" (anthropic-response-stop-reason r))
       (⊨ (pair? fragments))
       (⊦= (anthropic-response/text r) (foldr/concat-strings (reverse fragments)))
       (⊨ (> (anthropic-usage/output-tokens r) 0))))
   `(doc (p "The concatenated " (code/inline "text_delta") " fragments must equal the text "
            "of the assembled message. That single equality is what proves the accumulator "
            "and the callback path agree.")))

  ((test/anthropic/live/unknown-model-is-404 _)
   (⊦raises/api (404 "not_found_error")
     (anthropic/messages (list (anthropic-message/user "hi"))
                         model: "claude-does-not-exist" max-tokens: 64)))

  ((test/anthropic/live/bad-key-is-401 _)
   (parameterize ((anthropic/api-key "sk-ant-api03-definitely-not-a-real-key"))
     (⊦raises/api (401 "authentication_error")
       (anthropic/messages (list (anthropic-message/user "hi")) max-tokens: 64)))
   `(doc (p "This is the case that exercises the real curl transport's error path end to end: "
            "a genuine TLS connection, a genuine non-2xx, headers parsed off "
            (code/inline "--include") ", and a body that is real JSON.")))
  )

(let1 (key (get-environment-variable "ANTHROPIC_API_KEY"))
  (if (and key (positive? (string-length key)))
      (let1 (r (unittest/✓ anthropic-live-suite))
        (unless (null? (unittest/result-failed r))
          (print "anthropic-live: " (length (unittest/result-failed r)) " assertion(s) failed")
          (exit 1)))
      (print "anthropic-live: SKIPPED -- ANTHROPIC_API_KEY is unset or empty. "
             "Export it and run `make test-anthropic-live`.")))
```

---

### 6.6 Ten more cases the critique found unpinned

The ninety-six offline cases of §6.4 and the six live cases of §6.5 leave ten behaviours of Part A's
own API with no assertion. Splice these into `anthropic-suite` immediately before its closing paren.
Three of them (the `around` hook, the `--max-time` ternary and the integer enum) pin a branch whose
*absence* passes every existing case, which is the dangerous kind of gap.

Every binding named below exists in §3 and §4 with that name and arity, and the point is worth
labouring because an earlier draft of this section named six that did not. An unbound identifier in
`src/test/anthropic.scm` exits `csi` with **rc=70**, which fails `make test -B`, which — because
`Dockerfile:18` chains `&& cp test/*.html test/*.md ../test-results` — publishes **no** report for
**any** of the seventeen suites. A failed assertion is loud and local; an unbound identifier takes
the whole image build down with it. Four traps in particular, recorded so nobody reintroduces them:
`anthropic-stub/requests` answers a **list** and takes no index; the accessor is
`anthropic-response-stop-reason`, a plain record field, and there is no `-stop-sequence` beside it;
`anthropic-sse->message` answers `(values message errors)` where `message` is a decoded **alist**,
not an `anthropic-response`, so the §3.7 accessors do not apply to it; and `on-response:` is called
as `(on-response r request)`, with two arguments.

```scheme
  ; -- gaps closed after the adversarial review --------------------------------------------

  ((test/anthropic/loop/around-hook-approving-string-is-a-SUCCESS _)
   ; §3.8 warns that returning a plain string from an `around` hook denies NOTHING -- the
   ; string silently becomes a successful tool_result.  That is the documented foot-gun and it
   ; had no test, which is how a "deny" hook ships as an "approve" hook.
   (letstub (stub (anthropic-stub/canned fixture/response/tool-use)
                  (anthropic-stub/canned fixture/response/text))
     (anthropic/converse (list (anthropic-message/user "weather?"))
                         tools: (list get_weather/tool)
                         around: (λ (tool input run) "approved"))
     ; requests are a LIST, newest last: the second one carries the tool_result turn
     (let1 (blocks (anthropic-json/ref
                     (vector-ref (anthropic-json/ref (second (anthropic-stub/requests stub))
                                                     'messages)
                                 2)
                     'content))
       (⊦= "approved" (anthropic-json/ref (vector-ref blocks 0) 'content))
       (⊦= (void) (anthropic-json/ref (vector-ref blocks 0) 'is_error))))
   `(doc (p "A hook that returns a string has APPROVED the call with that string as the "
            "result. Denial is " (code/inline "(anthropic-tool/error ...)") " and nothing "
            "else.")))

  ((test/anthropic/transport/close-is-idempotent _)
   ; Idempotence is a property of anthropic-wire/close!, NOT of a backend: a close thunk with no
   ; guard of its own must still survive a second release, because anthropic-wire/drain! and the
   ; streaming dynamic-wind in §4.17 can both reach the same wire.  Asserting it over a hand-made
   ; wire with a counting thunk tests the right layer; asserting it over the curl backend would
   ; only have tested that backend's own flag, which is where the guard used to live.
   (let1 (closed 0)
     (let1 (w (make-anthropic-wire 200 '() '(test) (open-input-string "")
                                   (τ (set! closed (add1 closed)) (values 23 "once"))))
       (receive (s d) (anthropic-wire/close! w) (⊦= 23 s) (⊦= "once" d))
       (receive (s d) (anthropic-wire/close! w) (⊦= 23 s) (⊦= "once" d))
       (⊦= 1 closed)))
   `(doc (p "The accessor memoises the thunk's values into the record's "
            (code/inline "close") " field, so a second release answers from the cache rather "
            "than reaping a pid that is already gone.")))

  ((test/anthropic/curl/timeouts-differ-by-mode _)
   ; §4.8 picks --max-time from a ternary on stream?.  Swapping the two arms passes every
   ; other case in this suite, including the argv-membership case.
   (let1 (argv (anthropic-curl/argv "https://example.invalid" "/tmp/h" #f))
     (⊦= "600" (list-ref argv (add1 (list-index (λ (a) (equal? a "--max-time")) argv))))
     (⊭ (member? "--no-buffer" argv)))
   (let1 (argv (anthropic-curl/argv "https://example.invalid" "/tmp/h" #t))
     (⊦= "1800" (list-ref argv (add1 (list-index (λ (a) (equal? a "--max-time")) argv))))
     (⊨ (member? "--no-buffer" argv)))
   `(doc (p "The streaming arm gets thirty minutes and " (code/inline "--no-buffer")
            "; the blocking arm gets ten and does not. The streaming figure is also the "
            "worst case for abandoning a wire, which is why §4.8 kills the child before it "
            "waits for it.")))

  ((test/anthropic/loop/empty-assistant-content-is-not-appended _)
   ; §4.18 promises never to append an empty content array (a 400 on the next request), but
   ; every empty-content fixture so far RAISES first, so the guard was dead code.
   (letstub (stub (anthropic-stub/canned fixture/response/empty-end-turn))
     (receive (r transcript) (anthropic/converse (list (anthropic-message/user "hi")))
       (⊦= "end_turn" (anthropic-response-stop-reason r))
       (⊦= 1 (length transcript))))
   `(doc (p "An " (code/inline "end_turn") " carrying " (code/inline "\"content\":[]")
            " ends the conversation without appending an assistant turn: an empty "
            (code/inline "content") " array is a 400 the moment it is sent back.")))

  ((test/anthropic/loop/stop-sequence-terminates _)
   ; There is no anthropic-response/stop-sequence accessor -- §3.7 stops at -stop-details --
   ; so the stop sequence is read off the decoded body, which is where it lives.
   (letstub (stub (anthropic-stub/canned fixture/response/stop-sequence))
     (receive (r transcript) (anthropic/converse (list (anthropic-message/user "hi")))
       (⊦= "stop_sequence" (anthropic-response-stop-reason r))
       (⊦= "END" (anthropic-json/ref (anthropic-response-json r) 'stop_sequence))
       (⊦= 2 (length transcript))))
   `(doc (p (code/inline "stop_sequence") " terminates the loop exactly like "
            (code/inline "end_turn") ". Falling through to the "
            (code/inline "unknown stop_reason") " branch instead would turn a normal, "
            "requested stop into a raised condition.")))

  ((test/anthropic/loop/on-response-hook-fires-once-per-turn _)
   ; TWO parameters: §4.18 calls (on-response r request), where `request` is the 1-based turn
   ; number.  A one-parameter hook is an arity error in the middle of a conversation.
   (let1 (seen '())
     (letstub (stub (anthropic-stub/canned fixture/response/tool-use)
                    (anthropic-stub/canned fixture/response/text))
       (anthropic/converse (list (anthropic-message/user "weather?"))
                           tools: (list get_weather/tool)
                           on-response: (λ (r n)
                                          (set! seen
                                            (cons (list n (anthropic-response-stop-reason r))
                                                  seen))))
       (⊦= '((1 "tool_use") (2 "end_turn")) (reverse seen))))
   `(doc (p "The hook sees every turn, in order, with its 1-based request number -- which is "
            "what makes it usable for a progress display and for enforcing a budget the "
            (code/inline "max-iterations") " guard alone cannot express.")))

  ((test/anthropic/loop/options-reach-the-request _)
   ; One assertion covering the whole keyword-forwarding row in §4.18: a keyword dropped from
   ; that list is otherwise invisible, because every other case tests the options through
   ; anthropic-request/body directly rather than through converse.
   (letstub (stub (anthropic-stub/canned fixture/response/text))
     (anthropic/converse (list (anthropic-message/user "hi"))
                         system: "be terse" max-tokens: 128
                         stop-sequences: '("END") effort: "low")
     (let1 (req (first (anthropic-stub/requests stub)))
       (⊦= "be terse" (anthropic-json/ref req 'system))
       (⊦= 128 (anthropic-json/ref req 'max_tokens))
       (⊦= #("END") (anthropic-json/ref req 'stop_sequences))
       (⊦= "low" (anthropic-json/ref (anthropic-json/ref req 'output_config) 'effort))))
   `(doc (p (code/inline "anthropic/converse") " forwards eleven keywords to "
            (code/inline "anthropic/messages") " by hand. Dropping one is silent: the call "
            "still succeeds, it just ignores what the caller asked for.")))

  ((test/anthropic/sse/orphan-delta-and-stop-are-ignored _)
   ; anthropic-sse->message answers (values message errors) and `message` is a decoded ALIST.
   ; The §3.7 response accessors do NOT apply to it; anthropic-json/ref does.
   (receive (message errors)
       (anthropic-sse->message (anthropic-sse/events (open-input-string fixture/sse/orphan-index)))
     (⊦= '() errors)
     (⊦= #(((type "text") (text "ok"))) (anthropic-json/ref message 'content))
     (⊦= "end_turn" (anthropic-json/ref message 'stop_reason)))
   `(doc (p "A " (code/inline "content_block_delta") " or "
            (code/inline "content_block_stop") " for an index that was never opened is "
            "DROPPED, not raised on: the accumulator looks the index up with "
            (code/inline "assv") " and does nothing when the lookup fails. Raising would "
            "throw away a turn that is otherwise complete, over a frame the caller cannot "
            "act on anyway.")))

  ((test/anthropic/sse/redacted-thinking-round-trips _)
   (receive (message errors)
       (anthropic-sse->message
         (anthropic-sse/events (open-input-string fixture/sse/redacted-thinking)))
     (⊦= '() errors)
     (let1 (blk (vector-ref (anthropic-json/ref message 'content) 0))
       (⊦= "redacted_thinking" (anthropic-block-type blk))
       (⊨ (string? (anthropic-json/ref blk 'data)))
       (⊦= "EncryptedBlob" (anthropic-json/ref blk 'data))))
   `(doc (p "A " (code/inline "redacted_thinking") " block has no delta of any kind: its "
            (code/inline "content_block_start") " template IS the whole block and must pass "
            "through untouched, because the API rejects a thinking block that was modified.")))

  ((test/anthropic/tools/schema/integer-enum _)
   ; enum-type calls exact-integer? at EXPANSION time, and exact-integer? lives only in
   ; scheme.base -- so without (scheme base) in §4.1's import-for-syntax list this case does
   ; not fail, it fails to COMPILE.  The all-strings clause short-circuits ahead of it, which
   ; is why every other enum in this suite expands fine.
   (⊦= '((type "object")
         (properties ((level ((type "integer") (enum #(1 2 3)) (description "How urgent")))))
         (required #("level")))
       (anthropic-tool-schema set_priority/tool))
   (⊦= "{\"name\":\"set_priority\",\"description\":\"Set a priority.\",\"input_schema\":{\"type\":\"object\",\"properties\":{\"level\":{\"type\":\"integer\",\"enum\":[1,2,3],\"description\":\"How urgent\"}},\"required\":[\"level\"]}}"
       (anthropic-json/write (anthropic-tool->json set_priority/tool)))
   (⊦= "priority 2" (set_priority 2))
   `(doc (p "An " (code/inline "(enum ...)") " of exact integers derives "
            (code/inline "\"type\":\"integer\"") ", not "
            (code/inline "\"type\":\"string\"") ". The interesting part is where it would "
            "break: " (code/inline "exact-integer?") " is called while the macro EXPANDS, so "
            "it has to be in " (code/inline "import-for-syntax") ", and the failure is a "
            "compile error rather than a red case.")))
```

The fixtures and the one tool these need, alongside the others in §6.1. The four SSE pieces are
defined here rather than assumed: an earlier draft referred to `fixture/sse/text-prefix`,
`/text-suffix`, `/message-start` and `/message-end` without ever writing them down.

```scheme
(define fixture/response/empty-end-turn
  (string-append "{\"id\":\"msg_e\",\"type\":\"message\",\"role\":\"assistant\","
                 "\"content\":[],\"stop_reason\":\"end_turn\"}"))

(define fixture/response/stop-sequence
  (string-append "{\"id\":\"msg_s\",\"type\":\"message\",\"role\":\"assistant\","
                 "\"content\":[{\"type\":\"text\",\"text\":\"halted\"}],"
                 "\"stop_reason\":\"stop_sequence\",\"stop_sequence\":\"END\"}"))

; the four reusable halves of a minimal one-text-block stream
(define fixture/sse/message-start
  (string-append
    "event: message_start\ndata: {\"type\":\"message_start\",\"message\":{\"id\":\"msg_g1\",\"type\":\"message\",\"role\":\"assistant\",\"model\":\"claude-opus-5\",\"content\":[],\"stop_reason\":null,\"stop_sequence\":null,\"stop_details\":null,\"usage\":{\"input_tokens\":5,\"output_tokens\":1}}}\n\n"))

(define fixture/sse/message-end
  (string-append
    "event: message_delta\ndata: {\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"end_turn\",\"stop_sequence\":null},\"usage\":{\"output_tokens\":3}}\n\n"
    "event: message_stop\ndata: {\"type\":\"message_stop\"}\n\n"))

(define fixture/sse/text-prefix
  (string-append
    fixture/sse/message-start
    "event: content_block_start\ndata: {\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"text\",\"text\":\"\"}}\n\n"
    "event: content_block_delta\ndata: {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\"ok\"}}\n\n"))

(define fixture/sse/text-suffix
  (string-append
    "event: content_block_stop\ndata: {\"type\":\"content_block_stop\",\"index\":0}\n\n"
    fixture/sse/message-end))

; a well-formed stream carrying a delta AND a stop for index 7, which was never opened
(define fixture/sse/orphan-index
  (string-append fixture/sse/text-prefix
                 "event: content_block_delta\n"
                 "data: {\"type\":\"content_block_delta\",\"index\":7,"
                 "\"delta\":{\"type\":\"text_delta\",\"text\":\"ignored\"}}\n\n"
                 "event: content_block_stop\n"
                 "data: {\"type\":\"content_block_stop\",\"index\":7}\n\n"
                 fixture/sse/text-suffix))

(define fixture/sse/redacted-thinking
  (string-append fixture/sse/message-start
                 "event: content_block_start\n"
                 "data: {\"type\":\"content_block_start\",\"index\":0,"
                 "\"content_block\":{\"type\":\"redacted_thinking\",\"data\":\"EncryptedBlob\"}}\n\n"
                 "event: content_block_stop\ndata: {\"type\":\"content_block_stop\",\"index\":0}\n\n"
                 fixture/sse/message-end))

; at TOP LEVEL, beside the other define-tool forms, for the integer-enum case
(define-tool (set_priority (level (enum 1 2 3) "How urgent"))
  "Set a priority."
  (conc "priority " level))
```

One case the earlier draft carried is **gone rather than fixed**: `sse/structural-frame-raises`,
which claimed a `content_block_delta` arriving before its `content_block_start` raises an
`anthropic-sse-error`. It does not. §4.16's accumulator looks the index up with `assv` and ignores
the frame when the lookup fails, which is what `sse/orphan-delta-and-stop-are-ignored` above now
pins. Its fixture would have passed the assertion anyway, for the wrong reason — it had no
`message_stop`, so the raise it saw was the missing-stop check, not a structural one. A case that
passes for a reason other than the one it names is worse than no case.


## 7. Packaging diff

### 7.1 `src/aux.egg`

One line, appended as the **last** component. `chicken-install` builds components in listing order and this egg declares no `component-dependencies` (`grep -c component-dependencies src/aux.egg` → 0), so listing order *is* dependency order. `(aux anthropic)` imports `(aux base)` (`src/aux.egg:19`) and `(aux simdjson)` (`:38`), so it must come after both; last is the safe placement.

```diff
   (extension aux.kanren.micro)
   (extension aux.kanren.micro.show)
+  (extension aux.anthropic)
   ))
```

**Line 6, `(dependencies …)`, is unchanged.** Part A's import list is `srfi-1` and `srfi-69` (both already on that line) plus `(chicken base file file-posix flonum io keyword port process process-context random sort string condition)`, `scheme`, `(scheme base)`, `(scheme char)` — all CHICKEN core. No `http-client`, no `openssl`, no `srfi-18` (Part A's curl backend writes the body from the calling thread; curl buffers all of stdin before it connects in order to compute `Content-Length`, so there is no pump thread and no deadlock). That untouched line is what the curl decision buys.

### 7.2 `src/Makefile`

Three edits. All continuation lines are **tab**-indented, matching the file.

**(a) `test:` — append after line 29 (`microkanren-show.scm`), making it the last suite.** The `test:` target is seventeen hand-maintained recipe lines with no wildcard and no `.PHONY`, so the eighteenth has to be written out; and because the target is not phony, the `-B` in `Dockerfile:18` is what makes it run at all. Neither is cosmetic.

```diff
 	cd test && ${CSI} -s microkanren-show.scm
+	cd test && ${CSI} -s anthropic.scm
```

Last on purpose: `make` stops at the first non-zero `csi`, and `Dockerfile:18` copies `test/*.html` only after the whole target succeeds. With the newest and least-proven suite last, a local failure still leaves every established suite's report on disk. **Consequence to accept knowingly:** with the exit gate in §6.4, one failing assertion now fails `make test -B`, which fails `docker build`, which blocks the master image push — and because the `cp` step never runs, that build publishes *no* reports for *any* suite. That is the intent of a gate for a module whose failure mode is otherwise silent (a wrong JSON shape gets a 400 from the API, not an exception from Scheme). If a red master build is unacceptable, drop the gate as described at the bottom of §6.4.

**(b) A new target, after the `test-microkanren-show:` block at lines 34–35.** It is deliberately a prerequisite of nothing.

```diff
 test-microkanren-show:
 	cd test && ${CSI} -s microkanren-show.scm
 
+test-anthropic:
+	cd test && ${CSI} -s anthropic.scm
+
+test-anthropic-live:
+	cd test && ${CSI} -s anthropic-live.scm
+
 install:
 	chicken-install -sudo
```

**(c) `format:` — three lines.** Test lines go with the other `test/` lines (after line 52, `test/fds.sbral.scm`); the module line goes after the last module line (line 66, `aux.kanren.micro.show.scm`).

```diff
 	scheme-indent -T 2 < test/fds.sbral.scm > test/fds.sbral.scm.tmp && mv test/fds.sbral.scm.tmp test/fds.sbral.scm
+	scheme-indent -T 2 < test/anthropic.scm > test/anthropic.scm.tmp && mv test/anthropic.scm.tmp test/anthropic.scm
+	scheme-indent -T 2 < test/anthropic-live.scm > test/anthropic-live.scm.tmp && mv test/anthropic-live.scm.tmp test/anthropic-live.scm
 	scheme-indent -T 2 < aux.base.scm > aux.base.scm.tmp && mv aux.base.scm.tmp aux.base.scm 
```

```diff
 	scheme-indent -T 2 < aux.kanren.micro.show.scm > aux.kanren.micro.show.scm.tmp && mv aux.kanren.micro.show.scm.tmp aux.kanren.micro.show.scm
+	scheme-indent -T 2 < aux.anthropic.scm > aux.anthropic.scm.tmp && mv aux.anthropic.scm.tmp aux.anthropic.scm
```

`scheme-indent` is not installed in the image (`which scheme-indent` inside `ghcr.io/massimo-nocentini/aux.scm:master` → not found), so `format:` is a local-developer target and cannot break CI either way.

### 7.3 `README.md`

`README.md` is 46 lines and ends with the closing fence of the `(aux fds sbral)` example, **with no trailing newline**. Append a newline first, then the new section, so the fence is not glued to the new heading.

```markdown

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
```

### 7.4 `Dockerfile` — **no change**

Verified by listing every path of all six layers of the base image
`ghcr.io/massimo-nocentini/chicken-scheme.docker:6.0.0-eggs-included`, which is what
`ghcr.io/massimo-nocentini/aux.scm:master` is built `FROM`:

- `usr/bin/curl` is present, version 8.18.0, and PATH resolution works, so Part A's `(anthropic/curl "curl")` default is right. The layers are **amd64**; there is no separate arm64 image to check, for the reason given under "Review status".
- `include/curl`, `curl.h`, `libcurl.pc`, the link-time `libcurl.so` symlink and `pkg-config` return **zero** hits. The binary is there, the development headers are not. That is the blocker behind §4.8b, and it is also why nothing in this change needs an `apt-get` line.
- No new egg, so `make install` (`chicken-install -sudo`) is unaffected and does not reach the network for a new dependency.
- `COPY src src` already picks up `aux.anthropic.scm`, `test/anthropic.scm` and `test/anthropic-live.scm`.
- `cp test/*.html test/*.md ../test-results` already picks up `testsuite-anthropic-suite.html`.
- `anthropic-live.scm` is not in the `test:` target, so the build never runs it and never needs a secret.

The one line worth considering anyway is a guard, so that a base image that ever drops `curl` fails at its own layer with an obvious message instead of failing seventeen suites at `make test`:

```dockerfile
RUN command -v curl && curl --version | head -1
```

It costs one cached layer and buys a legible error. It is optional; nothing in this plan depends on it.

### 7.5 `.gitignore` — **no change**

`src/test/*.html` already covers `testsuite-anthropic-suite.html`, and all fixtures are inline, so no new data file lands in `src/test/`.

---

## 8. Implementation order

A developer can tick these off top to bottom. Steps 1–6 are the foundation and are worth getting green before anything touches a socket.

1. **Create `src/aux.anthropic.scm`** with the prose header, the `(module (aux anthropic) * …)` form, the full `import` list and the `import-for-syntax` list from Part A §4.1, and a bare `  )` to close. Confirm it loads: `cd src && csi -q -e '(load "aux.anthropic.scm")'` must be silent.
2. **Conditions and parameters** (Part A §4.2, §4.3, §4.4). `anthropic/transport` is *not* defined here — it comes after the curl backend in step 9. Sanity-check by hand in `csi`: `(anthropic-raise/config "x" "y")` must be caught by `(condition-case … (c (anthropic-error) …))`, `(c (anthropic-config-error) …)` and `(c (exn) …)` alike.
3. **The JSON validator** (§4.5). Before writing anything else, run the nine `⊭` cases of `test/anthropic/json/ill-formed` by hand — a validator that accepts `{"a":1}trailing` or `42` is worse than no validator, because it hands those to a parser that calls `std::terminate`.
4. **Parse, encode, access** (§4.6). Run `test/anthropic/json/round-trip`, `/escapes-control-characters`, `/numbers`, `/encoder-is-total`, `/accessors`, `/set-and-merge` by hand in `csi`.
5. **Strings, headers, status** (§4.7).
6. **Create `src/test/anthropic.scm`** with the imports, the three assertion macros, `letstub`, all fixtures and the tool definitions from §6.4, plus only the cases covering steps 2–5. Add `(extension aux.anthropic)` to `src/aux.egg` and `cd test && ${CSI} -s anthropic.scm` to the `test:` target now, so `make install && make test` exercises the module from here on. Everything to this point must be green.
7. **The wire record, `anthropic-wire/close!`, `anthropic-wire/drain!`, `anthropic-wire->body!`** (§4.8, §4.10). Add `test/anthropic/transport/wire-record`, `/close-is-idempotent` and `/port->string-over-a-custom-port`. Two orderings inside `close` are load-bearing and neither is obvious: **stdout is closed before stderr is drained** — draining stderr while curl is still writing stdout deadlocks on a full 64 KB pipe, while closing stdout makes curl exit 23 instead, which is why 23 is in the diagnosis table — and **the child is probed and killed before it is waited for**, or abandoning a stream blocks for the whole `--max-time`. Write `anthropic-wire/close!` as the memoising accessor from §4.8; do not put a `reaped` flag in the backend.
8. **The transport helpers** — `anthropic-curl/argv`, `-headers->file!`, `-status-line`, `-read-block`, `-read-headers`, plus `anthropic-transport/diagnosis`, `-transient?`, `-transient-codes` and their `anthropic-curl/*` aliases (§4.8). Add `test/anthropic/curl/status-line`, `/read-headers-skips-1xx`, `/argv-never-carries-the-key` and `test/anthropic/transport/diagnosis-and-transient`. These are all offline: no process is spawned.
9. **`anthropic-transport/curl` and the `anthropic/transport` parameter** (§4.8), in that order — the parameter's initializer is evaluated at load time and would otherwise name an unbound identifier. The two port-accessor comments are load-bearing; keep them verbatim. Smoke-test against a loopback server, not the API:
   `python3 -m http.server 8899 &` then in `csi`, `(parameterize ((anthropic/base-url "http://127.0.0.1:8899/") (anthropic/api-key "sk-ant-local")) (anthropic-wire-status (anthropic-transport/curl (anthropic/base-url) (anthropic-request/headers) "{}" #f)))` → `501`. Then `(anthropic-curl/headers->file! …)` must leave no file behind after `anthropic-wire/close!`; check `ls $TMPDIR | grep anthropic` is empty.
10. **The stub transport** (§4.9). Add `test/anthropic/messages/stub-exhaustion-raises` and `test/anthropic/redaction/stub-log-is-clean`. The header-name lowercasing in `anthropic-stub/canned` is not cosmetic: without it a fixture written `(("Retry-After" "3"))` silently takes the backoff path and a test that would pass against curl fails against the stub.
11. **`anthropic-retry/delay` and `anthropic-send`** (§4.10). Add the five `test/anthropic/retry/*` cases and `test/anthropic/status/classification`.
12. **Messages, blocks, validation, request building** (§4.11, §4.12). Add the fifteen `test/anthropic/request/*`, `/blocks/*` and `/messages/*` cases.
13. **The response record and accessors** (§4.13). Add the nine `test/anthropic/response/*` cases plus `test/anthropic/messages/create`, `/ask`, `/betas-header` and the six `test/anthropic/errors/*` cases. At this point scope item (a) is complete and fully tested.
14. **Tools as data** (§4.14, first half: the two records, the registry, `anthropic-tool->json`, `anthropic-tools->json`, `anthropic-tools->table`, `anthropic-tool/input-ref`, `anthropic-tool/apply`, `anthropic-tool-result->string`).
15. **`define-tool`** (§4.14, the `define-macro-ir`). Expect two or three debugging cycles here; the traps in order of likelihood: the formals must use the **unstripped** identifiers or the body sees unbound variables; the schema must be built as a datum and wrapped `(list 'quasiquote …)` by hand, not written as a nested `` `(quasiquote ,…) `` in the outer template; `raw` must be `unquote-splicing`, not `unquote`; `inject` takes exactly one symbol. Add the eight `test/anthropic/tools/*` cases. Every expected string in them was produced by executing Part A's `T`/`P`/`O` by hand, so a mismatch is a bug in the macro, not in the expectation.
16. **SSE framing** — `anthropic-event`, `anthropic-sse/field`, `/frame`, `/read`, `->§`, `/fold`, `/events`, `anthropic-event/text` (§4.16, first half). Add `test/anthropic/sse/framing`, `/crlf-and-multiline-data`, `/unknown-frames-are-tolerated`, `/lazy-stream-runs-finish-once`.
17. **The accumulator** (§4.16, second half). Add `test/anthropic/sse/assembles-a-text-turn`, `/assembles-thinking-text-and-tool-use`, `/tool-use-with-no-input-deltas`, `/unparseable-tool-input-is-recorded-not-raised`, `/cut-stream-raises`, `/error-frame-raises`. The full-alist expectation in `/assembles-a-text-turn` pins the key ORDER the accumulator produces; if it differs, the in-place `anthropic-json/set` is being bypassed somewhere.
18. **`anthropic/messages` and `anthropic/ask`** (§4.15) — if not already written for step 13.
19. **`anthropic/stream`** (§4.17). Add `test/anthropic/stream/end-to-end-over-the-stub` and `/rejects-a-non-sse-response`. The `dynamic-wind` is load-bearing: the accumulator can raise from the middle of the fold, and without it the curl child and the `0600` file holding the key leak until the process exits. Scope item (c) is complete.
20. **The loop** (§4.18). Add the eleven `test/anthropic/loop/*` cases. Scope items (b) and (d) are complete.
21. **Create `src/test/anthropic-live.scm`** from §6.5 and add the `test-anthropic:` / `test-anthropic-live:` targets. Verify the skip path with the variable unset: `cd src && make test-anthropic-live` must print `anthropic-live: SKIPPED …` and exit 0.
22. **Run the live suite once**, with a real key. It is the only thing that exercises the curl backend against TLS, HTTP/2, and real SSE pacing. In particular re-confirm the two facts established only by probe and never in CI: `process*` returns one object, and `process-input-port` is the port you *write*.
23. **Finish packaging**: the three `format:` lines, the README section. Run `make format` locally if `scheme-indent` is installed; do not hand-align.
24. **`make docker-build`** at the repository root — the only end-to-end verification path, and the one that confirms the port-direction result under CHICKEN 6.0.0 and curl 8.18.0 rather than on the local 6.0.1pre1 / curl 8.7.1. It confirms it on `linux/amd64` only: the `linux/arm64` leg builds on the same amd64 runner and produces the same bytes, as recorded under "Review status", so no arm64 claim can be made from it.
25. **Commit** on a branch off `master`. An earlier draft warned here against committing two stray fixture files, `src/test/users_1.7m.json` and `src/test/users_100k.json`; neither exists and the tree is clean at `4af90d8`, so the warning is gone rather than carried forward.

---

## 9. Known limitations, and what a v2 would add

**Two JSON implementations.** `anthropic-json/parse` and `anthropic-json/write` duplicate `(aux simdjson)`'s parser and encoder. That is a real maintenance cost and two implementations of the same mapping can drift. The right fix is upstream — pass `(anthropic-json/utf8-length str)` at `src/aux.simdjson.scm:93`, wrap `chicken_simdjson_parse_ondemand_callback` and `chicken_simdjson_visit_ondemand` in `try { … } catch (...) { return NULL; }` in `src/chicken-simdjson.cpp`, and replace `((string? v) (write v port))` at `:104` with a real JSON escaper — but that changes a module with an existing FFI and its own suite, so it belongs in its own change. `test/anthropic/json/round-trip` is the property test that pins the equivalence while both exist. **v2:** land the upstream fix, delete both copies, keep the round-trip test.

**The validator is a second full pass over every byte.** Every response body and every SSE frame is scanned in pure Scheme before simdjson sees it. For a 64 000-token streamed answer that is thousands of small scans. It is correct and it is the only thing standing between a truncated body and SIGABRT, but it is not free. **v2:** the C-side `try`/`catch` above removes the need for it entirely on the parse path; keep the validator only for the error-body path, where the input is genuinely untrusted.

**`anthropic-loop-error` loses the transcript** (G3). After a `max-iterations`, `refusal` or `max-tokens` raise the caller has the offending response but not the conversation it had built, so it cannot retry with a larger budget without replaying from scratch. **v2:** add a `transcript` property to `anthropic-raise/loop` and thread it through; the loop already has it in hand.

**No streaming agentic loop** (G4). `anthropic/stream` and `anthropic/converse` are disjoint: there is no way to stream a tool-using turn, which is the shape most interactive agents actually want. It needs a `stream?` flag on `anthropic/converse` and a `tools:`-aware `anthropic/stream` that returns `input-errors` the loop can convert — both exist, they are simply not composed. **v2.**

**`anthropic/stream` is not retried, by design,** because a retry re-POSTs the whole request and would replay tokens the caller has already displayed. A caller who wants retries must wrap the call and discard what it has shown. **v2:** a `on-restart` callback that tells the caller to clear its buffer, so `anthropic/with-retries`-style wrapping becomes safe.

**A raw-alist tool is advertised but not dispatchable** (G5). `anthropic-tools->json` accepts a hand-written definition so a tool that predates the macro can still reach the model; `anthropic-tools->table` registers only records, so that tool is answered with `no such tool: …`. **v2:** make `anthropic/converse` raise an `anthropic-config-error` up front when a tool in `tools:` is not an `anthropic-tool?`, which turns a confusing mid-conversation `is_error` into a local error at call time.

**The API key is on disk for the life of the request**, `0600`, which for a long stream is minutes. A root-equivalent local attacker can read it. Passing it via the environment instead is worse: `(chicken process)` switches to `execve` when handed an environment alist, which discards `PATH` and would force an absolute `/usr/bin/curl`. The obvious v2 is `curl --variable %ANTHROPIC_API_KEY --expand-header 'x-api-key: {{ANTHROPIC_API_KEY}}'`, which keeps the key in the inherited environment and off both argv and disk. **Do not implement it from that sentence: as stated it is a security regression, not an improvement.** Three traps, the first of which is the reason. (1) `--expand-header` bypasses `anthropic-header/check!`, which is reached only from `anthropic-curl/headers->file!` and is this module's CR/LF header-injection guard. curl does not sanitise expanded values: a key containing `\r\nx-injected: yes` was sent through that path against an echo server and arrived as a **separate header**, rc=0. Any implementation must run the check on the variable's value before the child is spawned, which means reading the environment in Scheme — at which point most of the argument for the mechanism is gone. (2) The `execve`/`PATH` problem above. (3) A version gate has to be numeric: `--variable` needs curl 8.3.0, and `(string>=? "8.18.0" "8.3.0")` is `#f`, so a naive lexicographic test silently refuses on exactly the version the target image ships. There is also the smaller point that the mechanism cannot express a key that came from the `anthropic/api-key` parameter rather than the environment.

**`process-wait` suspends every srfi-18 thread, not just the calling one.** The CHICKEN manual is explicit — "suspending the current process implies that all threads are suspended as well" — and it is measured: a 200 ms ticker went dead for 3004 ms across one wait. It sits on the hot path, in `anthropic-wire/close!`, on every request. If `(aux anthropic)` is ever used alongside other srfi-18 threads this is the module's one genuine architectural defect, and the fix is a different transport (§4.8b), not a different wait. **v2:** if it ever matters, that is what justifies the libcurl `curl_multi` backend and nothing less does.

**Abandoning a wire kills the transfer rather than draining it.** §4.8's close thunk probes with the non-blocking `process-wait` and sends `signal/term` if the child is still alive, so a caller who stops reading an SSE stream halfway gets its process back in milliseconds instead of waiting out `anthropic/stream-max-time`. Measured against a child that holds its pipes open and stays silent for thirty seconds: the whole close — probe, `signal/term`, stderr drain, blocking reap — returned in 2 ms of process time and 21 ms wall clock, with a final status of 128. The cost is that the abandoned response is genuinely gone — there is no "finish in the background and discard" path, and the exit status a killed curl reports is not one of the diagnosis table's meaningful codes. That is the right trade for a 1800-second default, but it is a behaviour and not an implementation detail: a caller that wants the rest of a stream must read the rest of the stream.

**`process-sleep` has one-second granularity.** The backoff cannot express sub-second waits, so the first retry always costs at least a second even when `retry-after` says less. `srfi-18`'s `thread-sleep!` would fix it but is not on `src/aux.egg:6`. **v2:** add `srfi-18` to the dependency line — it is already installed in the image — and make `anthropic/sleep` default to `thread-sleep!`.

**`anthropic-send` catches only `anthropic-error`.** A third-party transport that raises some other condition escapes the retry loop entirely and surfaces raw. That is arguably right — a Scheme bug is not a network failure — but it means a replacement backend has to adopt this module's condition kinds to get retries at all. **v2:** document the contract in the module header, or wrap non-`anthropic-error` escapes from the transport in `anthropic-transport-error`.

**A streamed message is not `equal?` to a blocking decode**, and the suite does not claim it is. The accumulator replaces keys in place in the `message_start` skeleton, so the two agree whenever the skeleton carries the same key set the final body would — which is what the real API does — but a key introduced only by `message_delta` is appended at the end, and a key the blocking body has that the skeleton lacks is simply absent. All assertions go through the accessors in Part A §3.7, which work identically on both. **v2:** if byte-equality ever matters (it does not today), canonicalise both sides through one ordering function rather than one side only.

**No prompt caching, no `cache_control`, no image or document content blocks, no batch API, no `GET /v1/models`, no token counting endpoint.** `base64` is in the image's egg list but not on `src/aux.egg:6`, so image support is the one feature that would change the dependency line. **v2:** `anthropic-block/image`, `anthropic-block/document`, and a `cache-control:` keyword on `anthropic-request/body` — the last of which is cheap and immediately valuable given that `anthropic-usage/prompt-tokens` already sums the three counters correctly.

**Nothing in the offline suite touches a real subprocess.** That is the point of the stub, but it means a transport wired backwards — `process-input-port` mistaken for the reading port — passes all 62 offline cases and only fails on the first `make test-anthropic-live`. The offline curl cases (`/status-line`, `/read-headers-skips-1xx`, `/argv-never-carries-the-key`) cover the parsing and the argv, not the plumbing. A third tier is possible and cheap: `python3` and the `spiffy` 6.4 egg are both in the image, so a loopback server replaying canned Anthropic JSON would exercise the real curl code path with no network and no key. **v2**, and it is the single highest-value addition to the suite.