
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

; fixtures and the one tool the §6.6 cases need ---------------------------------------------

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
     (code/pre "(transport url headers body stream?) -> anthropic-wire\n  status   exact-integer HTTP status\n  headers  ((lowercase-name value) ...)\n  origin   a datum identifying what produced this wire\n  port     an input port at the first byte of the body; it MUST answer read-string\n           correctly, so a custom make-input-port needs a read-bytevector: hook\n  close    thunk -> (values exit-status diagnostics); idempotence is enforced by\n           anthropic-wire/close!, which memoises the thunk's values, not by a backend")
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
   (⊦= (void) (anthropic-header/check! "x-api-key" "sk-ant-ok"))
   ; and the guard runs in the REQUEST layer, not only inside the curl backend's
   ; headers->file!.  It used to live only there, which made "headers cannot be injected into"
   ; a property of one transport: a replacement backend never goes near headers->file! and
   ; would have inherited nothing at all.
   (parameterize ((anthropic/api-key "sk-ant-ok\r\nx-evil: 1"))
     (⊦raises (anthropic-config-error) (anthropic-request/headers)))
   (parameterize ((anthropic/api-key "sk-ant-ok") (anthropic/betas '("beta\nx-evil: 1")))
     (⊦raises (anthropic-config-error) (anthropic-request/headers)))
   (parameterize ((anthropic/api-key "sk-ant-ok") (anthropic/version "2023-06-01\rx-evil: 1"))
     (⊦raises (anthropic-config-error) (anthropic-request/headers)))
   (parameterize ((anthropic/api-key "sk-ant-ok"))
     (⊦= '(("content-type" "application/json")
           ("x-api-key" "sk-ant-ok")
           ("anthropic-version" "2023-06-01"))
         (anthropic-request/headers)))
   `(doc (p "A header value carrying CR/LF appends headers of the attacker's choosing to the "
            "request. The check is at BOTH ends -- in "
            (code/inline "anthropic-request/headers") ", so every transport inherits it, and "
            "again in " (code/inline "anthropic-curl/headers->file!") ", which is public and "
            "takes a header list from anywhere -- and each call site is asserted "
            "independently, because deleting either one on its own left the suite green.")))

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
       ; the file is a REGULAR file at a name nobody could have guessed, not something we
       ; followed a symlink to: create-temporary-file opens after a `file-exists?` test that
       ; answers #f for a dangling link, so the key used to be writable to a path of an
       ; attacker's choosing.  O_CREAT|O_EXCL refuses anything already at the name.
       (⊭ (symbolic-link? path))
       (⊨ (regular-file? path))
       (⊨ (member? path (anthropic-temp/pending)))
       (anthropic-temp/delete! path)
       (⊭ (file-exists? path))
       (⊭ (member? path (anthropic-temp/pending)))))
   ; The CR/LF guard is reached THROUGH this procedure, and testing anthropic-header/check! on
   ; its own does not pin the call: deleting the `for-each` line left the whole suite green and
   ; silently reopened header injection into the file curl reads with `--header @FILE`.
   (⊦raises (anthropic-config-error)
     (anthropic-curl/headers->file! '(("x-api-key" "sk\r\nEvil: 1"))))
   (⊦raises (anthropic-config-error)
     (anthropic-curl/headers->file! '(("x-api\nkey" "sk-ant-ok"))))
   (⊦= '() (anthropic-temp/pending))
   `(doc (p "Neither the key nor the prompt is ever an argv element, so neither appears in "
            (code/inline "ps") ". The header file is created under a temporarily tightened "
            (code/inline "file-creation-mode") " of " (code/inline "#o077") " -- "
            (code/inline "create-temporary-file") " otherwise yields 0644 and "
            "chmod-after-create leaves a window in which the key is world readable. "
            (code/inline "--fail-with-body") " is absent on purpose: with it curl exits 22 "
            "for every HTTP error and a 429 becomes indistinguishable from a 404. A refused "
            "header leaves no file behind, which the registry reader "
            (code/inline "anthropic-temp/pending") " is what checks.")))

  ((test/anthropic/curl/url-is-an-argv-element _)
   ; curl has NO `--` end-of-options marker, so an argv element beginning with a dash is an
   ; option wherever it sits.  Measured: `curl --silent --request POST --data-binary @-
   ; -K/etc/hostname` answers "(2) no URL specified" -- the string was consumed as -K, which
   ; makes curl read an arbitrary config file, which can in turn set --output or --upload-file.
   (⊦raises (anthropic-config-error) (anthropic-curl/argv "-K/etc/hostname" "/tmp/h" #f))
   (⊦raises (anthropic-config-error) (anthropic-url/check! "--output=/etc/passwd"))
   (⊦raises (anthropic-config-error) (anthropic-url/check! "file:///etc/passwd"))
   (⊦raises (anthropic-config-error) (anthropic-url/check! "https://x/\r\nHost: evil"))
   (⊦raises (anthropic-config-error) (anthropic-url/check! 'not-a-string))
   (⊦= "https://api.anthropic.com/v1/messages"
       (anthropic-url/check! "https://api.anthropic.com/v1/messages"))
   (⊦= "http://127.0.0.1:8080/v1/messages"
       (anthropic-url/check! "http://127.0.0.1:8080/v1/messages"))
   ; and the check runs BEFORE the header file is created, not merely inside argv.  This spawns
   ; no process and touches no network: the URL is rejected in the transport's first line, ahead
   ; of headers->file!.  With the check only in anthropic-curl/argv the raise came from inside
   ; the `let*` that binds argv -- outside the handler that deletes the file -- and a rejected
   ; URL left the API key sitting in /tmp.
   (parameterize ((anthropic/api-key anthropic-test/key))
     (⊦raises (anthropic-config-error)
       (anthropic-transport/curl "-K/etc/hostname" (anthropic-request/headers) "{}" #f))
     (⊦= '() (anthropic-temp/pending)))
   `(doc (p "The key and the prompt are kept out of " (code/inline "argv") ", but the URL "
            "cannot be -- so it is the one argv element that needs a guard of its own. "
            "Plain " (code/inline "http://") " stays allowed: the offline transport probes "
            "and any local proxy need it, and the "
            (code/inline "https://") "-only decision belongs to the caller who sets "
            (code/inline "anthropic/base-url") ".")))

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
   ; Redaction is a property of the CONDITION, not only of the string it prints.  A handler
   ; that reads `diagnostics` off the condition and logs it -- or a failing assertion over it
   ; in a future case -- would otherwise put the key into whatever it writes, and for this
   ; suite that is the SXML report the Docker build copies into /test-results.  Asserted as
   ; booleans on purpose: a red run here must not print the thing it is protecting.
   (parameterize ((anthropic/api-key anthropic-test/key))
     (let1 (w (make-anthropic-wire 200 '() '(stub "https://x/") (open-input-string "")
                                   (τ (values 7 (conc "curl: (7) sent x-api-key: "
                                                      anthropic-test/key)))))
       (let1 (stored (condition-case (begin (anthropic-wire/drain! w) "no-error")
                       (c (anthropic-transport-error)
                          (get-condition-property c 'anthropic-transport-error 'diagnostics))))
         (⊭ (substring-index anthropic-test/key stored))
         (⊦≠ #f (substring-index "<redacted>" stored)))))
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
   ; An `event:` line with an EMPTY field must take the DEFAULT name.  The SSE specification
   ; says so, and the natural spelling `(or name "message")` gets it wrong, because "" is
   ; truthy in Scheme and the frame comes out named "".  Nothing else in this suite would
   ; notice: every other fixture names every one of its events.
   (let1 (names (λ (s) (map anthropic-event-name (anthropic-sse/events (open-input-string s)))))
     (⊦= '("message") (names "event:\ndata: {\"type\":\"ping\"}\n\n"))
     (⊦= '("message") (names "event: \ndata: {\"type\":\"ping\"}\n\n"))
     (⊦= '("message") (names "event:\r\ndata: {\"type\":\"ping\"}\r\n\r\n"))
     (⊦= '("message") (names "data: {\"type\":\"ping\"}\n\n"))
     (⊦= '("ping")    (names "event: ping\ndata: {\"type\":\"ping\"}\n\n")))
   `(doc (p "A line beginning with " (code/inline ":") " is a comment -- that is how the API "
            "keeps a connection warm -- and it produces no event at all.  An "
            (code/inline "event:") " line whose field is EMPTY is not the same as an absent "
            "name: the SSE specification makes both mean the default event type, "
            (code/inline "\"message\"") ".")))

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
         ; the after-thunk of §4.17's dynamic-wind ran, exactly once.  It is the ONLY thing
         ; that kills the curl child and deletes the 0600 file holding the key, and deleting
         ; it changes no other value in this case: the locals it sets are pre-initialised and
         ; the stub's close answers (values 0 "") either way.
         (⊦= 1 (anthropic-stub/closes stub))
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
     (⊦raises (anthropic-decode-error) (anthropic/stream (list (anthropic-message/user "hi"))))
     (⊦= 1 (anthropic-stub/closes stub)))
   (letstub (stub (anthropic-stub/canned fixture/error/429 status: 429))
     (⊦raises/api (429 "rate_limit_error") (anthropic/stream (list (anthropic-message/user "hi"))))
     (⊦= 1 (anthropic-stub/closes stub)))
   `(doc (p "A non-2xx on a streaming request answers with an ordinary JSON error body, not "
            "with SSE, so it is routed through the blocking error path before anyone starts "
            "reading frames. And a 200 whose " (code/inline "content-type")
            " is not " (code/inline "text/event-stream") " is a decode error, never an empty "
            "message. Both of those early exits release the wire before they raise.")))

  ((test/anthropic/stream/releases-the-wire-when-the-fold-raises _)
   ; §4.17 wraps the fold in a dynamic-wind whose after-thunk is the ONLY release of the wire.
   ; Without it the curl child and the 0600 file holding the API key survive until the process
   ; exits -- and no assertion over a RETURNED value can see the difference, because on this
   ; path nothing is returned at all.  So the stub counts its releases instead.
   ;
   ; Both raising shapes, because they leave the fold at different points: an `error` frame
   ; raises from the MIDDLE of the fold, and a cut stream raises AFTER it, from
   ; anthropic-sse->message's unfinished-block check.
   (letstub (stub (anthropic-stub/canned fixture/sse/error
                                         headers: '(("content-type" "text/event-stream"))))
     (⊦raises (anthropic-api-error) (anthropic/stream (list (anthropic-message/user "hi"))))
     (⊦= 1 (anthropic-stub/closes stub)))
   (letstub (stub (anthropic-stub/canned fixture/sse/cut
                                         headers: '(("content-type" "text/event-stream"))))
     (⊦raises (anthropic-sse-error) (anthropic/stream (list (anthropic-message/user "hi"))))
     (⊦= 1 (anthropic-stub/closes stub)))
   `(doc (p "An abandoned stream is the expensive leak, not the tidy one: the transport's "
            "close thunk is what deletes the " (code/inline "#o600")
            " file holding the key and what terminates the child, and until it runs a "
            "streaming request holds both for up to " (code/inline "--max-time")
            " -- thirty minutes at the default.")))

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

  ; NAMED for the tool layer, not the loop: the body calls the leaf directly with a
  ; hand-written input-errors list.  The producer end -- anthropic/stream putting the list into
  ; the record -- is the case immediately below.
  ((test/anthropic/tools/invalid-streamed-tool-input _)
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

  ((test/anthropic/stream/carries-input-errors-into-the-response _)
   ; Producer and consumer were each covered and the wire between them was not: replacing
   ; `(set! errors e)` in §4.17 with `(set! errors '())` left the whole suite green, and so did
   ; making §4.18's converse forward '() instead of the record's list.  Half a streamed
   ; tool_use argument would then have been reported as a VALID empty input, and the tool would
   ; have run on it.
   (letstub (stub (anthropic-stub/canned fixture/sse/broken-input
                                         headers: '(("content-type" "text/event-stream"))))
     (let1 (r (anthropic/stream (list (anthropic-message/user "weather?"))
                                tools: (list get_weather/tool)))
       (⊦= '(("toolu_03" "{\"location\": \"Par")) (anthropic-response-input-errors r))
       (⊦= "max_tokens" (anthropic-response-stop-reason r))
       (⊦= 1 (anthropic-stub/closes stub))))
   ; and the blocking path, where there is no partial JSON to fail on, reports none
   (letstub (stub (anthropic-stub/canned fixture/response/tool-use))
     (⊦= '() (anthropic-response-input-errors
              (anthropic/messages (list (anthropic-message/user "weather?"))
                                  tools: (list get_weather/tool)))))
   `(doc (p "A tool_use block whose " (code/inline "input_json_delta") " stream was cut "
            "leaves the block with an EMPTY input, which is indistinguishable from a tool "
            "that legitimately takes no arguments. The id and the partial text ride back on "
            "the response record so that " (code/inline "anthropic/converse")
            " can answer that particular " (code/inline "tool_use_id")
            " with an " (code/inline "is_error") " result instead of running the tool on "
            "nothing.")))

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
   ; A thunk that returns ZERO values memoises '(), which is not a pair.  Discriminating on the
   ; memo rather than on the thunk therefore called '() as a procedure on the second release and
   ; died with "call of non-procedure: ()" -- idempotence for every backend EXCEPT the ones that
   ; answer nothing.  Both shipped backends return two values, so nothing here would have noticed.
   (let1 (closed 0)
     (let1 (w (make-anthropic-wire 200 '() '(test) (open-input-string "")
                                   (τ (set! closed (add1 closed)) (values))))
       (⊦= '() (receive vals (anthropic-wire/close! w) vals))
       (⊦= '() (receive vals (anthropic-wire/close! w) vals))
       (⊦= 1 closed)))
   `(doc (p "The accessor memoises the thunk's values into the record's "
            (code/inline "close") " field, so a second release answers from the cache rather "
            "than reaping a pid that is already gone. The discriminant is "
            (code/inline "procedure?") " on the thunk and not " (code/inline "pair?")
            " on the memo, because a thunk that answers zero values memoises "
            (code/inline "'()") " and the second release would then call it.")))

  ((test/anthropic/transport/still-running-is-pid-zero _)
   ; The nohang probe `(process-wait p #t)` answers pid 0 -- NOT #f -- for a child that is still
   ; running; measured on this csi as (0 #f #f).  The natural spelling `(not pid)` never fires,
   ; because 0 is truthy in Scheme, and §4.8's close thunk then goes back to sitting in a blocking
   ; process-wait for the whole --max-time: 1800 s at the streaming default.  Nothing about that
   ; is visible in a value, which is why the predicate exists at all.
   (⊨ (anthropic-transport/still-running? 0))
   (⊭ (anthropic-transport/still-running? #f))
   (⊭ (anthropic-transport/still-running? 49345))
   `(doc (p "Reverting this to " (code/inline "(not pid)") " -- which is what the shape of "
            (code/inline "process-wait") " in most other languages suggests -- silently "
            "restores the full " (code/inline "--max-time") " stall when a stream is "
            "abandoned. The offline suite spawns no processes, so this pins the SPELLING; "
            "the stall itself was measured by hand against a server that sends a header block "
            "and then holds the socket silent (0.0 s and exit 128 with the fix, 8.0 s and "
            "exit 28 without it, at " (code/inline "--max-time 8") ").")))

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
