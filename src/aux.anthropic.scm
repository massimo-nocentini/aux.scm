
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
; returned record carrying the HTTP status, the response headers, the `origin` datum naming
; whatever produced the wire, an input port positioned at the first body byte, and a `close` thunk
; answering (values exit-status diagnostics).  Both `origin` and `diagnostics` end up inside an
; `anthropic-transport-error` condition and from there in the SXML report `make test` publishes, so
; a transport MUST keep credentials out of its `origin` -- the curl backend puts the header FILE's
; path there, never the key -- and SHOULD hand back diagnostics it has already passed through
; `anthropic/redact`.  A wire MUST be closed: the curl backend's close thunk is what reaps the
; child -- and removes the 0600 header file, on the paths that did not already remove it when the
; response headers came back -- so a wire taken straight from a transport and then dropped keeps a
; zombie, three file descriptors and possibly that file until the process exits.  That port MUST answer `read-string` correctly: a
; custom port built with `make-input-port` MUST be given a `read-bytevector:` hook, because this
; csi's read-char fallback for read-string returns garbage -- `{"ok":true}` comes back as `}` plus
; ten NUL bytes, and `read-string!` reports a false success count on top of it.  A transport that
; cannot speak at all raises `anthropic-transport-error`; it never invents an HTTP status.  The
; offline suite swaps in `anthropic-stub/transport`, which is why `make test` needs no network and
; no API key.
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
;     element, so neither shows up in `ps`.  The header file is NOT made with
;     create-temporary-file -- that opens without O_EXCL after a `file-exists?` test that answers
;     #f for a dangling symlink, which is a demonstrated way to get the key written to a path
;     someone else chose.  See `anthropic-temp/open!`.  The URL, which unavoidably IS an argv
;     element, goes through `anthropic-url/check!` because curl has no `--` end-of-options marker
;     and would read a leading-dash "URL" as an option.
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
;
; The module uses `display` rather than `write-string` throughout.  Not because `write-string` is
; unavailable -- it is in (scheme base), which is imported, and would work -- but because it is
; absent from (chicken io), which is where a reader reaching for it looks first.  `display` is the
; one spelling nobody has to check.

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
                                 ; the stored properties are redacted too, not only the printed
                                 ; message: a handler that reads `diagnostics` and logs it would
                                 ; otherwise put a key into whatever it logs.  `origin` is left as
                                 ; a datum so it stays readable, which is why §2's transport
                                 ; contract requires a CREDENTIAL-FREE origin -- the curl backend
                                 ; puts only the header FILE's path there, never the key.
                                 `(origin ,origin exit-status ,exit-status
                                   diagnostics ,(anthropic/redact diagnostics)))))

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

  ; `response` is an anthropic-response record at every call site but one: `max-iterations` fires
  ; BEFORE a request is made and so passes #f.  It is never a quasiquoted alist -- a handler that
  ; reads the property has one shape to test, plus #f.
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
    (let1 (headers
            `(("content-type" "application/json")
              ("x-api-key" ,(anthropic-api-key/effective))
              ("anthropic-version" ,(anthropic/version))
              ,@(if (null? betas) '() `(("anthropic-beta" ,(string-intersperse betas ","))))))
      ; The CR/LF injection guard runs HERE, in the request layer, and not only inside the curl
      ; backend's headers->file!.  It used to live only there, which made "headers cannot be
      ; injected into" a property of one transport: a replacement backend never goes near
      ; headers->file! and would have inherited nothing.  Both call sites are asserted in §6.
      (for-each (λ (h) (anthropic-header/check! (car h) (cadr h))) headers)
      headers))

  (define (anthropic/redact text)
    (let1 (key (anthropic-api-key/current))
      (if (and (string? text) (string? key) (>= (string-length key) 8))
          (string-translate* text (list (cons key "<redacted>")))
          text)))

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

  ; parsing ---------------------------------------------------------------------------------
  ;
  ; The byte count is hand-computed rather than taken from the FFI because the callback wants
  ; BYTES and `string-length` answers in CHARACTERS -- not because the FFI is out of reach.  This
  ; very procedure drives `simdjson-parse-ondemand-callback`, a foreign-lambda, from the same
  ; interpreted `csi -s` suite: a compiled extension loads into csi perfectly well.

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

  ; `foldl` in CHICKEN passes the ACCUMULATOR first -- verified -- so (λ (o kv) ...) is the order.
  (define (anthropic-json/merge obj patch)
    (if (pair? patch)
        (foldl (λ (o kv) (anthropic-json/set o (car kv) (cadr kv))) obj patch)
        obj))

  ; strings and headers -----------------------------------------------------------------------

  (define (anthropic-string/trim s)
    (let1 (n (string-length s))
      (let L ((a 0) (b n))
        (cond
          ((and (< a b) (char-whitespace? (string-ref s a))) (L (add1 a) b))
          ((and (< a b) (char-whitespace? (string-ref s (sub1 b)))) (L a (sub1 b)))
          (else (substring s a b))))))

  ; (read-string #f port) answers the EOF OBJECT, not "", on a port that produced nothing.
  ; This IS read-string, and every blocking response body in the module funnels through it, which
  ; is why §2 requires a custom transport port to carry a `read-bytevector:` hook: without one this
  ; csi's read-char fallback hands back `}` plus ten NUL bytes for `{"ok":true}`, silently.
  (define (anthropic-port->string port)
    (let1 (s (read-string #f port)) (if (eof-object? s) "" s)))

  (define (anthropic-header/check! name value)
    (for-each (λ (s)
                (unless (string? s)
                  (anthropic-raise/config "a header name and value must both be strings" name))
                (when (or (substring-index "\n" s) (substring-index "\r" s))
                  (anthropic-raise/config "a header may not contain a newline" name)))
              (list name value)))

  ; curl has NO `--` end-of-options marker, so an argv element beginning with a dash is an option
  ; wherever it sits: a base URL of "-K/path" makes curl read an arbitrary config file, which can
  ; then set --output or --upload-file.  Measured: `curl --silent --request POST --data-binary @-
  ; -K/etc/hostname` answers "(2) no URL specified", i.e. the string was consumed as an option and
  ; never as a URL.  Nothing analogous to anthropic-header/check! guarded this, so an application
  ; that let untrusted configuration reach anthropic/base-url handed curl an option.  Requiring a
  ; scheme prefix closes it completely, and the CR/LF test is there for a backend that puts the URL
  ; into a request line rather than into an argv.
  (define (anthropic-url/check! url)
    (unless (and (string? url)
                 (or (eqv? 0 (substring-index "https://" url))
                     (eqv? 0 (substring-index "http://" url)))
                 (not (substring-index "\n" url))
                 (not (substring-index "\r" url)))
      (anthropic-raise/config
        "the request URL must begin with http:// or https:// and contain no newline" url))
    url)

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

  ; the wire --------------------------------------------------------------------------------

  ; `origin`, not `command`: the stub already fills this field with `(stub ,url)`, which is not a
  ; command, and a libcurl backend would put a handle there.  It exists for error messages only.
  (define-record anthropic-wire status headers origin port close)

  ; Idempotence lives HERE, not in any backend.  The curl backend used to carry its own `reaped`
  ; flag, which made "you may close a wire twice" a property of one transport; the stub's thunk had
  ; no guard at all.  Memoising the values into the field gives it to every backend for free, so a
  ; backend's close thunk may assume it runs exactly once.
  ; The test is `procedure?` on the THUNK, not `pair?` on the memo.  A close thunk that returns
  ; ZERO values memoises '(), which is not a pair, and the second release would then call '() as a
  ; procedure and die with "call of non-procedure: ()" -- idempotence for every backend EXCEPT the
  ; ones that return nothing.  Both shipped backends return two values, so this was latent; a
  ; third-party transport is exactly where it would not be.
  (define (anthropic-wire/close! w)
    (let1 (c (anthropic-wire-close w))
      (if (procedure? c)
          (receive vals (c)
            (anthropic-wire-close-set! w vals)
            (apply values vals))
          (apply values c))))

  ; Transport exit statuses.  The names are backend-neutral on purpose: the numbers below happen to
  ; be curl(1) exit codes, and libcurl's CURLcode agrees with every one of them, but the retry
  ; policy is the module's, not curl's.  Getting this table wrong costs a retry that will not help,
  ; or a retry that was not attempted.  Only 6, 7, 23 and 126 were observed here; the rest are from
  ; curl's manual.
  (define anthropic-transport/transient-codes '(7 18 28 52 55 56))

  (define (anthropic-transport/transient? code) (member? code anthropic-transport/transient-codes))

  ; `(process-wait p #t)` -- the nohang probe -- answers pid 0 for a child that is STILL RUNNING,
  ; not #f.  Measured on this csi: `(0 #f #f)`.  Spelling this `(not pid)` is the natural reading
  ; and is wrong in the silent direction: 0 is truthy in Scheme, so the kill in §4.8's close thunk
  ; never fires and abandoning a stream goes back to blocking for the whole --max-time (1800 s at
  ; the streaming default).  It is a named predicate so that the spelling is a thing a test can
  ; hold, rather than a literal buried in a closure.
  (define (anthropic-transport/still-running? pid) (eqv? 0 pid))

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
      ; checked HERE and not at the parameter, because this is where it becomes an argv element
      (list "--data-binary" "@-" (anthropic-url/check! url))))
  ; deliberately absent: --fail-with-body (it collapses every HTTP error onto exit 22) and
  ; --location (a redirect would produce a second header block and could forward the key).

  ; a temporary file that is safe to put an API key in -----------------------------------------
  ;
  ; NOT create-temporary-file.  That picks a name, tests `file-exists?`, and then opens for writing
  ; with no O_EXCL -- and `file-exists?` answers #f for a DANGLING symlink.  So a local attacker who
  ; can write $TMPDIR pre-plants `temp<4 hex>.<pid>.anthropic` (65536 names for a known pid) and the
  ; key is written THROUGH the link to a path of their choosing; worse, the `delete-file*` at close
  ; unlinks only the link, so the key then survives the request permanently.  Demonstrated on this
  ; box against the previous version of this procedure, with the key landing in a file the attacker
  ; named.  The tightened creation mode does not help: it only sets the mode of the target.
  ;
  ; O_CREAT|O_EXCL is the whole fix.  It fails with EEXIST on anything already at the name, symlink
  ; included, and never follows one -- verified here: the open is refused and the link's target is
  ; not created.  The name carries 96 bits from the OS entropy source (see anthropic-random/bits;
  ; `pseudo-random-integer` would make it predictable and pre-planting a cheap denial of service).
  ; The tightened file-creation-mode stays as belt and braces: it pins the result at exactly #o600
  ; whatever the caller's ambient umask was.

  (define anthropic-temp/attempts 64)

  (define (anthropic-temp/directory)
    (or (get-environment-variable "TMPDIR")
        (get-environment-variable "TMP")
        (get-environment-variable "TEMP")
        "/tmp"))

  ; `random-bytes` fills a bytevector from the OS source.  It is NOT `pseudo-random-integer`:
  ; that generator is not seeded per process on this CHICKEN -- three independent runs of the same
  ; program printed the identical sequence (1.832 0.4 3.488) -- which would make a temp-file name
  ; predictable and, worse, would make the retry jitter below identical in every client.
  (define (anthropic-random/bits n)
    (let1 (b (random-bytes (make-bytevector n)))
      (let L ((i 0) (acc 0))
        (if (>= i n) acc (L (add1 i) (+ (* acc 256) (bytevector-u8-ref b i)))))))

  (define (anthropic-random/unit) (/ (anthropic-random/bits 4) 4294967296.0))

  (define (anthropic-temp/token) (number->string (anthropic-random/bits 12) 16))

  ; Every header file that exists right now.  The transport deletes its own entry as soon as the
  ; response headers come back, and this sweep catches the rest -- a caller who takes a wire
  ; straight from a transport and never closes it, which is the leak that actually happens.
  ;
  ; Measure before relying on it: `on-exit` fires on NORMAL program termination ONLY.  Verified on
  ; this csi -- `(exit 1)` rc=1, an uncaught error rc=70, SIGTERM rc=143 and SIGINT rc=70 all skip
  ; the handler; only running off the end of the program ran it.  So this is not a Ctrl-C guard and
  ; must not be sold as one.  The real defence against a long-lived key file is deleting it at the
  ; first response header, above, which bounds its life by the time to first byte instead of by
  ; --max-time.  One handler for the whole module, not one per request: on-exit STACKS, so
  ; registering per request would grow without bound in a long-lived process.
  (define anthropic-temp/live '())

  (define (anthropic-temp/open!)
    (let1 (saved (file-creation-mode))
      (dynamic-wind
        (τ (set! (file-creation-mode) #o077))
        (τ (let L ((n 0))
             (let* ((path (conc (anthropic-temp/directory) "/anthropic-"
                                (anthropic-temp/token) ".headers"))
                    (fd (handle-exceptions e #f
                          (file-open path (+ open/wronly open/creat open/excl) #o600))))
               (cond
                 (fd (set! anthropic-temp/live (cons path anthropic-temp/live))
                     (values path fd))
                 ((< n anthropic-temp/attempts) (L (add1 n)))
                 (else (anthropic-raise/config
                         "could not create a private file for the request headers"
                         (anthropic-temp/directory)))))))
        (τ (set! (file-creation-mode) saved)))))

  ; a reader for the registry, so a test can assert that a request left nothing behind without
  ; reaching into a mutable module variable
  (define (anthropic-temp/pending) anthropic-temp/live)

  (define (anthropic-temp/delete! path)
    (set! anthropic-temp/live (delete! path anthropic-temp/live equal?))
    (delete-file* path))

  (on-exit (τ (for-each delete-file* anthropic-temp/live)))

  (define (anthropic-curl/headers->file! headers)
    ; the guard is ALSO in anthropic-request/headers, so that a replacement transport inherits it.
    ; Here as well, because this procedure is public and takes a header list from anywhere.
    (for-each (λ (h) (anthropic-header/check! (car h) (cadr h))) headers)
    (receive (path fd) (anthropic-temp/open!)
      ; the port is taken OUTSIDE the handler so the handler can always close it: an escape from
      ; the writes below -- ENOSPC is the realistic one -- would otherwise leak a descriptor per
      ; failed request as well as the file.
      (let1 (port (open-output-file* fd))
        (handle-exceptions e (begin (handle-exceptions e2 (void) (close-output-port port))
                                    (anthropic-temp/delete! path)
                                    (signal e))
          (for-each (λ (h) (display (car h) port) (display ": " port)
                          (display (cadr h) port) (newline port))
                    headers)
          (close-output-port port)))
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
    ; The URL is checked HERE, ahead of everything, and again inside anthropic-curl/argv (which is
    ; public).  Ahead, because a raise from the let* below would escape the handler that deletes
    ; the header file -- measured: with the check only in argv, a rejected URL left the file, with
    ; the key in it, for the module's exit sweep to find.  Everything that can raise now happens
    ; either before the file exists or inside the handler.
    (anthropic-url/check! url)
    (let1 (path (anthropic-curl/headers->file! headers))
      ; the OUTER handler covers the window in which `path` exists but `close` does not yet
      (handle-exceptions e (begin (anthropic-temp/delete! path) (signal e))
        (let* ((argv (anthropic-curl/argv url path stream?))
               (origin (cons (anthropic/curl) argv))
               (p (process* (anthropic/curl) argv))
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
                   ; The accessor is `process-id`, NOT `process-pid` -- there is no process-pid in
                   ; CHICKEN 6 (modules.db lists process-id, and the wrong name is an unbound
                   ; variable, rc=70).  The other easy mistake -- reading the nohang probe's
                   ; answer as #f rather than 0 -- lives in `anthropic-transport/still-running?`,
                   ; which exists so that a test can hold that spelling without spawning a child.
                   (receive (pid ok? status) (process-wait p #t)
                     (when (anthropic-transport/still-running? pid)
                       (handle-exceptions e (void)
                         (process-signal (process-id p) signal/term))))
                   (let1 (diagnostics (anthropic-port->string stderr))
                     (close-input-port stderr)
                     (anthropic-temp/delete! path)
                     ; the exit status is only populated once both ports are closed
                     (receive (pid ok? status) (process-wait p)
                       (values (if ok? status 128)
                               (anthropic/redact diagnostics)))))))
          ; `released?` guards the SETUP path only -- NOT the thunk, which stays unguarded so
          ; that anthropic-wire/close! remains the single place idempotence lives.  The no-status
          ; branch below calls `close` and then raises, and that raise runs this very handler,
          ; which would call `close` a SECOND time: the stderr drain then dies with
          ; "(read-string) port already closed", masking the real anthropic-transport-error.
          ; Measured against a refused connection, where curl exits 7 and writes no status line.
          (let ((released? #f))
            (handle-exceptions e (begin (unless released? (close)) (signal e))
              ; curl buffers the whole of stdin before connecting, in order to compute
              ; Content-Length, so writing the body from this thread cannot deadlock.  Measured
              ; against a server that accepts and never reads: `display` of a 16 MB body returned
              ; in 28 ms, single-threaded, no srfi-18 pump.
              (display body stdin)
              (close-output-port stdin)
              (receive (status response-headers) (anthropic-curl/read-headers stdout)
                ; The header file dies HERE, not at close.  curl parses every option before it
                ; connects, so by the time a response header block has come back it is long done
                ; with the file -- and deleting it at close instead would keep the API key on
                ; disk for the whole life of the wire, which for a stream is up to --max-time,
                ; 1800 s by default.  `close` deletes as well (delete-file* on a file that is
                ; already gone is a no-op), so the paths that never reach this line -- a refused
                ; connection, a raise inside read-headers -- are still covered.
                (anthropic-temp/delete! path)
                (unless (exact-integer? status)
                  (set! released? #t)
                  (receive (exit-status diagnostics) (close)
                    (anthropic-raise/transport (anthropic-transport/diagnosis exit-status)
                                               origin exit-status diagnostics)))
                (make-anthropic-wire status response-headers origin stdout close))))))))

  ; NOTE: this must come AFTER anthropic-transport/curl -- a parameter's initializer is evaluated
  ; at module-load time and would otherwise name an unbound identifier.
  (define anthropic/transport (make-parameter anthropic-transport/curl))

  ; the stub transport ------------------------------------------------------------------------
  ;
  ; The stub records the call BEFORE it answers, so a request that triggers an error is still
  ; inspectable, and it raises on an unscripted call, so a tool-loop test proves the loop
  ; terminated instead of silently looping against the last canned response.  Header VALUES come
  ; back redacted: a failing assertion over them would otherwise print a real key into the
  ; SXML->HTML report that `unittest/✓` writes.

  ; `closes` exists so a test can assert that a wire was RELEASED.  Without it the streaming
  ; dynamic-wind's after-thunk -- the only thing that kills the curl child and deletes the 0600
  ; file holding the key when the accumulator raises from the middle of the fold -- is invisible:
  ; deleting it changes no value any assertion can see, because the locals it sets are already
  ; initialised and the stub's close answers (values 0 "") either way.
  (define-record anthropic-stub script log closes)

  (define (anthropic-stub/canned body #!key (status 200)
                                            (headers '(("content-type" "application/json"))))
    (list status (map (λ (h) (list (string-downcase (car h)) (cadr h))) headers) body))

  (define (anthropic-stub/make . script) (make-anthropic-stub script '() 0))

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

  (define (anthropic-stub/closes stub) (anthropic-stub-closes stub))

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
          (let1 (close (τ (anthropic-stub-closes-set! stub (add1 (anthropic-stub-closes stub)))
                          (values 0 "")))
            (make-anthropic-wire (car canned) (cadr canned) `(stub ,url)
                                 (open-input-string (caddr canned))
                                 close))))))

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
        ; Full jitter: uniform in [0, window), which decorrelates a herd of clients that were
        ; rate-limited together.  It is drawn from anthropic-random/unit and NOT from
        ; pseudo-random-integer, because that generator is not seeded per process here: measured,
        ; three separate csi runs each printed (1.832 0.4 3.488), so every client in the herd would
        ; have backed off by exactly the same amount and the jitter would decorrelate nothing.
        ((anthropic/backoff-jitter) (* (exact->inexact window) (anthropic-random/unit)))
        (else (exact->inexact window)))))

  ; The retry loop catches only `anthropic-error` on purpose: a bug inside a user-supplied
  ; transport is not a network failure and must surface raw.  It also resolves the key once per
  ; call rather than per attempt, so a key rotated mid-retry is not picked up -- deliberate.
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
