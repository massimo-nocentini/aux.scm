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
