
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

; ⊦⧳ (aux.unittest.scm:160) is a TOLERATE form: with a body that never raises it returns the
; body's value and the case passes.  This one requires the raise.
;
; NOTE the pattern variables are `s` and `t`, NOT `status` and `type`.  syntax-rules
; substitutes pattern variables inside quote, so `status` would rewrite the template's
; 'status into '400 and the assertion would die with "condition has no such property: 400".

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
