;;; (aux pie serialization) -- contexts written to a string and read back.
;;;
;;; A port of serialization.rkt of Pie, the language of "The Little Typer" by Daniel P. Friedman and David
;;; Thrane Christiansen (https://github.com/the-little-typer/pie, commit 2c89553). Pie is distributed under
;;; the GNU AGPL-3.0, and so is this file, which is derived from it.
;;;
;;; The file is ported whole, with the upstream names and exports (cf. serialization.rkt:6). At the end of
;;; a run, main reads back the context of the program into a Serializable-Ctx, dumps it to a string, and
;;; restores it from that string before it evaluates it again into the-pie-ctx. The port keeps this round
;;; trip so that main has the data flow of upstream.
;;;
;;; Upstream writes with print-graph #t, which only labels the structure that the context shares. The port
;;; has no such labels and prints shared structure twice, and CHICKEN's read gives back a context that is
;;; equal? to the dumped one, which is all that val-of-ctx needs.
;;;
;;; Racket's write and read agree on every symbol, while CHICKEN's do not. CHICKEN's write puts no bars
;;; around the names +i, -i, +inf.0, +nan.0, +.5 or -inf.0i, and its read then gives numbers back. A Pie
;;; program can use such a name, written |+i| for instance, as a variable, and Racket Pie accepts it. So
;;; dump writes the context with write-datum, which bars a symbol when CHICKEN's own write of it does not
;;; read back as the same symbol. Every other symbol is written as CHICKEN writes it, among them |::|,
;;; |vec::|, |a:|, |a b|, |1|, x₁ and λ, as are the source strings and numbers of the TODO srclocs.
;;;
;;; Like upstream, the module imports read-back-ctx and val-of-ctx of (aux pie normalize) without using
;;; them (cf. serialization.rkt:3).

(module (aux pie serialization) (dump restore)

  (import scheme
          (chicken base)
          (chicken port)
          (only (chicken format) format)
          (aux base)
          (aux pie basics)
          (only (aux pie normalize) read-back-ctx val-of-ctx)) ; unused, as upstream

  ;;; ==================== DUMPING AND RESTORING ====================

  ;; dump : Serializable-Ctx -> String, the context written as a datum, cf. serialization.rkt:8
  (define (dump v)
    (with-output-to-string
      (τ (write-datum v))))

  ;; write-datum : Datum -> Void, write of the datum to the current output port, except that a symbol
  ;; whose CHICKEN write does not read back as itself is written with bars. It stands for the
  ;; (parameterize ((print-graph #t)) (write v)) of serialization.rkt:12-13.
  (define (write-datum v)
    (cond
      ((symbol? v) (display (symbol->readable-string v)))
      ((pair? v)
       (display "(")
       (write-datum (car v))
       (let loop ((d (cdr v)))
         (cond
           ((null? d) (void))
           ((pair? d) (display " ") (write-datum (car d)) (loop (cdr d)))
           (else (display " . ") (write-datum d))))
       (display ")"))
      (else (write v))))

  ;; symbol->readable-string : Symbol -> String, the written form of the symbol that CHICKEN's read gives
  ;; back as the same symbol.
  (define (symbol->readable-string sym)
    (let1 (w (with-output-to-string (τ (write sym))))
      (if (eq? sym (with-input-from-string w read))
          w
          (string-append
           "|"
           (with-output-to-string
             (τ (for-each (λ (c)
                            (when (memv c '(#\| #\\)) (display #\\))
                            (display c))
                          (string->list (symbol->string sym)))))
           "|"))))

  ;; restore : String -> Serializable-Ctx, the datum read from the string, which must be a serializable
  ;; context, cf. serialization.rkt:15
  (define (restore str)
    (with-input-from-string str
      (τ (let1 (v (read))
           (if (serializable-ctx? v)
               v
               (error 'restore (format "Invalid deserialized context: ~a" v))))))))
