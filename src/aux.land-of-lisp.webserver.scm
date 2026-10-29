;;; (aux land-of-lisp webserver) -- the web server of "Land of Lisp" (Conrad Barski, No Starch Press
;;; 2010, chapter 13), a port of http://landoflisp.com/webserver.lisp.
;;;
;;; webserver.lisp is distributed under the GNU General Public License, version 2 only, and so is this
;;; file, which is derived from it. Its notice reads:
;;;
;;;   This program is free software; you can redistribute it and/or modify
;;;   it under the terms of the GNU General Public License as published by
;;;   the Free Software Foundation; version 2 of the License.
;;;
;;;   This program is distributed in the hope that it will be useful,
;;;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;   GNU General Public License for more details.
;;;
;;;   Partial Author: Conrad Barski, M.D.
;;;   Parts Adapted with permission from http.lisp by Ron Garret
;;;
;;; Scheme port: Copyright (C) 2026 Massimo Nocentini. The license text is in COPYING.GPL-2.0.
;;; SPDX-License-Identifier: GPL-2.0-only
;;;
;;; The seven functions of webserver.lisp, with upstream's names, arguments and data flow, over
;;; (chicken tcp). A request handler is called as (request-handler path header params), where `path` is
;;; the URL between the leading / and the ?, undecoded ("greeting", "" for /), `header` an alist of
;;; (name . "value") in request order and `params` an alist of (name . "value"), the query parameters
;;; first and then those of the body; the handler prints the page to the current output port.
;;; (aux land-of-lisp webserver spiffy) serves the same handlers over spiffy.
;;;
;;; Departures from upstream:
;;;   - parameter and header names are interned lower-case (CL interns upper-case and its reader upcases
;;;     'name), so (assq 'name params) finds them either way;
;;;   - the reply starts with a status line and headers (HTTP/1.1 200 OK, Content-Type text/html;
;;;     charset=utf-8, Content-Length in bytes, Connection: close); upstream writes the bare page,
;;;     HTTP/0.9 style, which Chrome rejects on ports other than 80;
;;;   - the page is buffered: the handler writes to a string port, not to the socket, so that
;;;     Content-Length can be computed and a failed page discarded;
;;;   - a request that cannot be read or parsed is answered 400 Bad Request: a request line with fewer
;;;     than two spaces, a malformed percent escape (decode-param raises, as upstream), a Content-Length
;;;     that is not a non-negative integer, a query that parse-params turns into an improper alist
;;;     (append rejects it, in CL as here), an EOF inside the header;
;;;   - the request is bounded: the request line and the header together are read through a port that
;;;     fails after `head-limit` characters (answered 400), a Content-Length above `content-limit` is
;;;     answered 413 Payload Too Large without reading the body, and a request whose line, header and
;;;     body are not read within time-limit: seconds (10 by default) is answered 408 Request Timeout,
;;;     as is one whose read waits longer than tcp-read-timeout. Upstream waits for ever on a client
;;;     that stalls or trickles its request;
;;;   - after a reply, the server reads what the client has already sent and the server did not read
;;;     (the rest of a refused request), so that the close does not reset the connection before the
;;;     client has read the reply;
;;;   - a handler error answers 500 Internal Server Error and the server goes on; in CL the error reaches
;;;     the debugger and the loop stops. The body of a refused request is the reason phrase and a
;;;     newline ("Internal Server Error\n", "Bad Request\n", ...), as text/plain. Each 400, 408 and 500
;;;     writes the line "serve: <message>" to the current error port. An EOF in place of the request
;;;     line (a browser preconnect that was closed) is dropped silently, and errors while writing a
;;;     reply (the peer is gone) are ignored;
;;;   - `serve` takes the keyword arguments port: (upstream hard-codes 8080), listener: (a listener of
;;;     (chicken tcp) to accept on), requests: (stop after that many connections, for tests) and
;;;     time-limit:; the listener is closed on exit in a dynamic-wind, upstream's unwind-protect;
;;;   - get-content-params follows the book's printed text: the downloadable file calls (read content)
;;;     on a cons and passes the integer that read-sequence answers to parse-params, so every request
;;;     with a body fails there. A body shorter than its Content-Length answers what was read;
;;;   - CL keeps the \r at the end of each header value (read-line on a CRLF stream); CHICKEN's
;;;     read-line strips it. So get-header starts the value at (+ i 2) or at the end of the line,
;;;     whichever comes first: on a CRLF line that is CL's value without its \r ("X-Empty:" gives ""),
;;;     on a bare-LF line ending right after its colon CL fails;
;;;   - CL's (parse-integer ... :radix 16 :junk-allowed t) of decode-param and the parse-integer of the
;;;     book's get-content-params are the local `parse-integer`, which reads the same prefixes.
;;;
;;; Kept as upstream: per-character % decoding (no UTF-8, so "%C3%A8" gives two characters), keys not
;;; decoded, parse-params answering a bare string for a key without = (hence an improper alist), the
;;; substring failure when & precedes =, get-header assuming ": ", any method treated alike, 200 for
;;; unknown paths, a single connection served at a time (so a client that stalls or trickles holds the
;;; server for up to time-limit: plus one tcp-read-timeout, 60 s by default). Content-Length counts
;;; bytes, the body is read as characters, as in CL: a non-ASCII body waits for characters that never
;;; come and is answered 408; browsers percent-encode forms, so their bodies are ASCII.

(module (aux land-of-lisp webserver)
  (decode-param parse-params parse-url get-header get-content-params serve hello-request-handler)

  (import scheme
          (chicken base)
          (chicken format)
          (chicken io)
          (chicken port)
          (chicken tcp)
          (chicken condition)
          (chicken bytevector)
          (only (chicken time) current-process-milliseconds)
          (only (scheme char) string-downcase char-downcase char-whitespace?)
          (only srfi-13 string-index string-index-right)
          (aux base))

  ; the bounds of a request: the request line plus the header, in characters, and the body, in the
  ; units of Content-Length.
  (define head-limit 16384)
  (define content-limit 65536)

  ; helpers ----------------------------------------------------------------------------------------

  ; digit-weight : char radix -> integer or #f, the value of `c` as a digit of `radix` (up to 36).
  (define (digit-weight c radix)
    (let1 (w (cond
               ((char<=? #\0 c #\9) (- (char->integer c) (char->integer #\0)))
               ((char<=? #\a (char-downcase c) #\z) (+ 10 (- (char->integer (char-downcase c)) (char->integer #\a))))
               (else radix)))
      (and (< w radix) w)))

  ; parse-integer : string -> integer or #f, CL's PARSE-INTEGER: optional whitespace, an optional sign,
  ; then digits of `radix`. With `junk-allowed` it stops at the first character that is not a digit and
  ; answers #f when it read no digit; without it, only whitespace may follow the digits, and anything
  ; else (no digit included) is an error.
  (define (parse-integer s #!key (radix 10) (junk-allowed #f))
    (let* ((n (string-length s))
           (skip-whitespace (λ (i) (let loop ((i i))
                                     (if (and (< i n) (char-whitespace? (string-ref s i))) (loop (add1 i)) i))))
           (i (skip-whitespace 0))
           (sign (and (< i n) (memv (string-ref s i) '(#\+ #\-)) (string-ref s i)))
           (start (if sign (add1 i) i)))
      (let loop ((j start) (acc 0))
        (let1 (d (and (< j n) (digit-weight (string-ref s j) radix)))
          (if d
            (loop (add1 j) (+ (* acc radix) d))
            (let1 (v (and (> j start) (if (eqv? sign #\-) (- acc) acc)))
              (cond
                (junk-allowed v)
                ((and v (= (skip-whitespace j) n)) v)
                (else (error 'parse-integer "not an integer" s)))))))))

  ; the condition of a request not read in time, of kind timeout like a read that waits longer than
  ; tcp-read-timeout.
  (define (request-timeout)
    (make-composite-condition (make-property-condition 'exn 'location 'serve 'message "request timeout"
                                                       'arguments '())
                              (make-property-condition 'timeout)))

  (define timeout? (condition-predicate 'timeout))

  ; limited-input-port : port integer integer -> port, reads from `in`, fails after `n` characters and
  ; signals (request-timeout) once (current-process-milliseconds) passes `deadline`. It buffers nothing,
  ; so `in` can be read directly again afterwards.
  (define (limited-input-port in n deadline)
    (let1 (check! (λ ()
                    (when (> (current-process-milliseconds) deadline) (signal (request-timeout)))
                    (when (zero? n) (error 'serve "request header too large"))))
      (make-input-port
        (λ () (check!) (sub1! n) (read-char in))
        (λ () (char-ready? in))
        void
        (λ () (check!) (peek-char in)))))

  ; upstream ---------------------------------------------------------------------------------------

  (define (decode-param s)
    (letrec ((f (λ (lst)
                  (if (null? lst)
                    '()
                    (case (car lst)
                      ((#\%) (cons (integer->char (parse-integer (list->string (list (cadr lst) (caddr lst)))
                                                                 radix: 16 junk-allowed: #t))
                                   (f (cdddr lst))))
                      ((#\+) (cons #\space (f (cdr lst))))
                      (else (cons (car lst) (f (cdr lst)))))))))
      (list->string (f (string->list s)))))

  (define (parse-params s)
    (let* ((i1 (string-index s #\=))
           (i2 (string-index s #\&)))
      (cond
        (i1 (cons (cons (string->symbol (string-downcase (substring s 0 i1)))
                        (decode-param (substring s (add1 i1) (or i2 (string-length s)))))
                  (if i2 (parse-params (substring s (add1 i2))) '())))
        ((equal? s "") '())
        (else s))))

  (define (parse-url s)
    (let* ((url (substring s
                           (+ 2 (string-index s #\space))
                           (string-index-right s #\space)))
           (x (string-index url #\?)))
      (if x
        (cons (substring url 0 x) (parse-params (substring url (add1 x))))
        (cons url '()))))

  (define (get-header stream)
    (let* ((s (read-line stream))
           (h (let1 (i (string-index s #\:))
                (and i (cons (string->symbol (string-downcase (substring s 0 i)))
                             (substring s (min (+ i 2) (string-length s))))))))
      (if h (cons h (get-header stream)) '())))

  ; the book's text, as printed in chapter 13.
  (define (get-content-params stream header)
    (let1 (length (alist-ref 'content-length header))
      (if length
        (let1 (content (read-string (parse-integer length) stream))
          (parse-params (if (eof-object? content) "" content)))
        '())))

  (define (hello-request-handler path header params)
    (if (equal? path "greeting")
      (let1 (name (assq 'name params))
        (if (not name)
          (display "<form>What is your name?<input name='name' /></form>")
          (format #t "Nice to meet you, ~a!" (cdr name))))
      (display "Sorry... I don't know that page.")))

  ; serving ----------------------------------------------------------------------------------------

  (define (reply status content-type body)
    (format #f "HTTP/1.1 ~a\r\nContent-Type: ~a\r\nContent-Length: ~a\r\nConnection: close\r\n\r\n~a"
            status content-type (bytevector-length (string->utf8 body)) body))

  ; the reply that refuses a request, its reason phrase as body.
  (define (text-reply code reason)
    (reply (string-append code " " reason) "text/plain; charset=utf-8" (string-append reason "\n")))

  (define (log-condition exn)
    (handle-exceptions _ (void)
      (format (current-error-port) "serve: ~a\n"
              (if (condition? exn)
                ((condition-property-accessor 'exn 'message "unknown condition") exn)
                (format #f "~s" exn)))))

  ; the Content-Length of `header` as an integer, #f when there is none; an error when it is not a
  ; non-negative integer.
  (define (content-length header)
    (let1 (value (alist-ref 'content-length header))
      (and value
           (let1 (n (parse-integer value))
             (if (negative? n) (error 'serve "negative Content-Length" value) n)))))

  ; respond : procedure input-port number -> string or #f, the reply to the request read from `in`
  ; within `time-limit` seconds, #f when the connection was closed before its request line.
  (define (respond request-handler in time-limit)
    (let* ((deadline (+ (current-process-milliseconds) (* 1000 time-limit)))
           (head (limited-input-port in head-limit deadline)))
      (handle-exceptions exn (begin
                               (log-condition exn)
                               (if (timeout? exn)
                                 (text-reply "408" "Request Timeout")
                                 (text-reply "400" "Bad Request")))
        (let1 (line (read-line head))
          (and (not (eof-object? line))
               (let* ((url (parse-url line))
                      (path (car url))
                      (header (get-header head))
                      (size (content-length header)))
                 (if (and size (> size content-limit))
                   (text-reply "413" "Payload Too Large")
                   (let* ((body (limited-input-port in content-limit deadline))
                          (params (append (cdr url) (get-content-params body header))))
                     (handle-exceptions exn (begin
                                              (log-condition exn)
                                              (text-reply "500" "Internal Server Error"))
                       (reply "200 OK" "text/html; charset=utf-8"
                              (with-output-to-string (τ (request-handler path header params)))))))))))))

  ; after every reply, reads what the peer has already sent and the server did not read (the rest of a
  ; refused request), so that closing the socket does not reset the connection before the peer has
  ; read the reply.
  (define (drain in)
    (handle-exceptions _ (void)
      (let loop ((k content-limit))
        (when (and (positive? k) (char-ready? in) (not (eof-object? (read-char in))))
          (loop (sub1 k))))))

  (define (serve-connection request-handler in out time-limit)
    (let1 (text (respond request-handler in time-limit))
      (when text
        (handle-exceptions _ (void)
          (display text out)
          (flush-output out))
        (drain in))))

  (define (serve request-handler #!key (port 8080) (listener #f) (requests #f) (time-limit 10))
    (let1 (socket (or listener (tcp-listen port)))
      (dynamic-wind
        void
        (τ (let loop ((accepted 0))
             (unless (and requests (>= accepted requests))
               (receive (in out) (tcp-accept socket)
                 (serve-connection request-handler in out time-limit)
                 (handle-exceptions _ (void) (close-input-port in))
                 (handle-exceptions _ (void) (close-output-port out)))
               (loop (add1 accepted)))))
        (τ (tcp-close socket)))))

  )
