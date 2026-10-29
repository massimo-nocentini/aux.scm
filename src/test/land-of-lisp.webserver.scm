;;; Tests of (aux land-of-lisp webserver), the port of the web server of "Land of Lisp" (Conrad Barski,
;;; No Starch Press 2010, chapter 13), http://landoflisp.com/webserver.lisp.
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
;;; The pure functions are checked on the examples of the book first, then on the quirks that the port
;;; keeps. The server runs in a srfi-18 thread on a loopback port: (tcp-listen 0) lets the kernel pick
;;; a free one, and the case that passes port: itself probes ports from 47613 on, skipping those in
;;; use. Each server is started with requests: n, so that it stops by itself after n connections and
;;; closes its listener; the client reads each reply up to the EOF, since the server closes every
;;; connection. The drain before each close (see the module) is not observable on the loopback, where
;;; Linux hands the client the reply even after a reset, so no case checks it.

(import scheme
        (chicken base)
        (chicken io)
        (chicken tcp)
        (chicken port)
        (chicken condition)
        (only (scheme base) open-input-string open-output-string get-output-string)
        (only srfi-1 every)
        (only (chicken time) current-process-milliseconds)
        (only (chicken string) string-split)
        (only srfi-13 string-prefix?)
        srfi-18
        (aux base)
        (aux unittest)
        (aux land-of-lisp webserver))

;;; ==================== HELPERS ====================

; http : integer string -> string, the whole reply to `text` sent on a fresh connection to `port`;
; with `shut`, the client closes its side of the connection after `text`, so the server reads an EOF.
(define (http port text #!optional shut)
  (parameterize ((tcp-read-timeout 20000))
    (receive (in out) (tcp-connect "127.0.0.1" port)
      (display text out)
      (flush-output out)
      (when shut (close-output-port out))
      (begin1 (read-string #f in) (close-input-port in) (close-output-port out)))))

; start-server : procedure integer -> (values port thread log), serves `handler` for `requests`
; connections on a fresh loopback listener, `serve-args` passed on to serve; `log` answers the lines
; that the server wrote to its error port.
(define (start-server handler requests #!rest serve-args)
  (let* ((L (tcp-listen 0 10 "127.0.0.1"))
         (err (open-output-string))
         (th (parameterize ((current-error-port err))
               (make-thread (τ (apply serve handler listener: L requests: requests serve-args))))))
    (thread-start! th)
    (values (tcp-listener-port L) th (τ (string-split (get-output-string err) "\n")))))

(define (ok-reply length body)
  (string-append "HTTP/1.1 200 OK\r\nContent-Type: text/html; charset=utf-8\r\nContent-Length: " length
                 "\r\nConnection: close\r\n\r\n" body))

(define (error-reply code reason length)
  (string-append "HTTP/1.1 " code " " reason "\r\nContent-Type: text/plain; charset=utf-8\r\nContent-Length: "
                 length "\r\nConnection: close\r\n\r\n" reason "\n"))

(define internal-server-error (error-reply "500" "Internal Server Error" "22"))
(define bad-request (error-reply "400" "Bad Request" "12"))
(define request-timeout (error-reply "408" "Request Timeout" "16"))
(define payload-too-large (error-reply "413" "Payload Too Large" "18"))

; logged? : (list string ...) integer -> boolean, whether `lines` are `n` lines of serve.
(define (logged? lines n) (and (= n (length lines)) (every (λ (l) (string-prefix? "serve: " l)) lines)))

; join : thread -> value, the result of `server`, or 'timeout when it has not ended within 30 s.
; It polls: thread-join! with a timeout waits for the whole timeout when the thread ended after an I/O
; wait (CHICKEN 6.0.1pre1).
(define (join server)
  (let loop ((k 600))
    (cond
      ((memq (thread-state server) '(terminated dead)) (thread-join! server))
      ((zero? k) 'timeout)
      (else (thread-sleep! 0.05) (loop (sub1 k))))))

; connection-refused? : integer -> boolean, whether nothing listens on `port` any more.
(define (connection-refused? port)
  (condition-case (receive (in out) (tcp-connect "127.0.0.1" port) (close-input-port in) (close-output-port out) #f)
    ((exn i/o net) #t)))

;;; ==================== SUITE ====================

(define-suite land-of-lisp-webserver-suite

  ; the examples of chapter 13.
  ((test/webserver/book _)
   (⊦= "foo" (decode-param "foo"))
   (⊦= "foo?" (decode-param "foo%3F"))
   (⊦= "foo bar" (decode-param "foo+bar"))
   (⊦= '((name . "bob") (age . "25") (gender . "male")) (parse-params "name=bob&age=25&gender=male"))
   (⊦= '("lolcats.html") (parse-url "GET /lolcats.html HTTP/1.1"))
   (⊦= '("lolcats.html" (extra-funny . "yes")) (parse-url "GET /lolcats.html?extra-funny=yes HTTP/1.1"))
   (⊦= '((foo . "1") (bar . "abc, 123")) (get-header (open-input-string "foo: 1\nbar: abc, 123\n\n")))
   (⊦= '((userid . "foo") (password . "supersecret"))
       (get-content-params (open-input-string "userid=foo&password=supersecret") '((content-length . "31"))))
   (⊦= '() (get-content-params (open-input-string "") '())))

  ((test/webserver/quirks _)
   (⊦= '() (parse-params ""))
   (⊦= "foo" (parse-params "foo"))
   (⊦= '((a . "1") . "b") (parse-params "a=1&b"))
   (⊦= '((x . "1")) (parse-params "x=1&"))
   (⊦= '((name . "Bob")) (parse-params "Name=Bob"))
   (⊦= '((a%41 . "1")) (parse-params "a%41=1"))             ; keys are not decoded
   (⊦= "AB" (decode-param "%41%42"))
   (⊦= (string (integer->char 4)) (decode-param "%4x"))     ; :junk-allowed t reads the 4 only
   (⊦= (string (integer->char #xC3) (integer->char #xA8)) (decode-param "%C3%A8"))
   (⊦= "\n" (decode-param "% a"))                           ; parse-integer skips the space, reads a
   (⊦= '("") (parse-url "GET / HTTP/1.1"))
   (⊦= '("a/b.html" (q . "x y")) (parse-url "GET /a/b.html?q=x+y HTTP/1.0"))
   ; CRLF lines: CHICKEN's read-line strips the \r that CL keeps; a line without : ends the header.
   (⊦= '((host . "x") (content-length . "8"))
       (get-header (open-input-string "Host: x\r\nContent-Length: 8\r\n\r\nname=Ann")))
   (⊦= '((a . "1")) (get-header (open-input-string "a: 1\nno colon here\nb: 2\n\n")))
   ; a value that ends right after the colon: CL takes (subseq "X-Empty:\r" 9), "".
   (⊦= '((x-empty . "") (host . "x")) (get-header (open-input-string "X-Empty:\r\nHost: x\r\n\r\n")))
   (⊦= '((a . "")) (get-header (open-input-string "a:\n\n")))
   ; a body shorter than its Content-Length answers what was read.
   (⊦= '((name . "An")) (get-content-params (open-input-string "name=An") '((content-length . "20"))))
   (⊦= '() (get-content-params (open-input-string "") '((content-length . "3"))))
   (⊦= '((a . "1")) (get-content-params (open-input-string "a=1") '((content-length . " 3 ")))))

  ((test/webserver/errors _)
   (⊦raises (exn) (decode-param "%zz"))                      ; no digit: (code-char nil) in CL
   (⊦raises (exn) (decode-param "%4"))                       ; one character after %: coerce in CL
   (⊦raises (exn) (decode-param "%"))
   (⊦raises (exn) (parse-url "GET"))
   (⊦raises (exn) (parse-url "GET /"))
   (⊦raises (exn) (parse-params "a&b=1"))                    ; & before =: subseq in CL
   (⊦raises (exn) (get-header (open-input-string "a: 1\n")))   ; EOF: read-line in CL
   (⊦raises (exn) (get-content-params (open-input-string "a=1") '((content-length . "3x")))))

  ((test/webserver/hello-request-handler _)
   (⊦= "<form>What is your name?<input name='name' /></form>"
       (with-output-to-string (τ (hello-request-handler "greeting" '() '()))))
   (⊦= "Nice to meet you, Bob!"
       (with-output-to-string (τ (hello-request-handler "greeting" '() '((name . "Bob"))))))
   (⊦= "Sorry... I don't know that page."
       (with-output-to-string (τ (hello-request-handler "lolcats.html" '() '((name . "Bob")))))))

  ; the example of the book, served over TCP.
  ((test/webserver/serve _)
   (receive (port server log) (start-server hello-request-handler 9)
     (⊦= (ok-reply "52" "<form>What is your name?<input name='name' /></form>")
         (http port "GET /greeting HTTP/1.1\r\nHost: localhost\r\n\r\n"))
     (⊦= (ok-reply "22" "Nice to meet you, Bob!") (http port "GET /greeting?name=Bob HTTP/1.1\r\n\r\n"))
     (⊦= (ok-reply "22" "Nice to meet you, Ann!")
         (http port "POST /greeting HTTP/1.1\r\nContent-Length: 8\r\n\r\nname=Ann"))
     (⊦= (ok-reply "32" "Sorry... I don't know that page.") (http port "GET /other HTTP/1.1\r\n\r\n"))
     ; query parameters come before the body's.
     (⊦= (ok-reply "22" "Nice to meet you, Bob!")
         (http port "POST /greeting?name=Bob HTTP/1.1\r\nContent-Length: 8\r\n\r\nname=Ann"))
     ; a browser preconnect: opened and closed without a request line, it gets no reply.
     (receive (in out) (tcp-connect "127.0.0.1" port) (close-output-port out) (close-input-port in))
     ; a body that parse-params turns into an improper alist reaches the handler, as upstream, and
     ; the assq of hello-request-handler fails on it.
     (⊦= internal-server-error (http port "POST /greeting HTTP/1.1\r\nContent-Length: 5\r\n\r\na=1&b"))
     (⊦= (ok-reply "22" "Nice to meet you, Eve!") (http port "GET /greeting?name=Eve HTTP/1.1\r\n\r\n"))
     ; a header line with an empty value, as upstream reads it from a CRLF stream.
     (⊦= (ok-reply "22" "Nice to meet you, Kim!")
         (http port "GET /greeting?name=Kim HTTP/1.1\r\nX-Empty:\r\nHost: x\r\n\r\n"))
     (⊦= (void) (join server))
     (⊦= 1 (length (log)))
     (⊨ (string-prefix? "serve: bad argument type" (car (log))))
     (⊨ (connection-refused? port))))

  ; a malformed request is answered 400 and an oversized one 413; the server goes on.
  ((test/webserver/serve/malformed _)
   (receive (port server log) (start-server hello-request-handler 10)
     (⊦= bad-request (http port "GET /greeting?name=%zz HTTP/1.1\r\n\r\n"))
     (⊦= bad-request (http port "GET /greeting?name=% HTTP/1.1\r\n\r\n"))
     (⊦= bad-request (http port "GARBAGE\r\n\r\n"))
     ; the query ((a . "1") . "b") is an improper alist, which append rejects before the handler runs.
     (⊦= bad-request (http port "GET /greeting?a=1&b HTTP/1.1\r\n\r\n"))
     (⊦= bad-request (http port "GET /greeting HTTP/1.1\r\nContent-Length: ten\r\n\r\n"))
     (⊦= bad-request (http port "GET /greeting HTTP/1.1\r\nContent-Length: -1\r\n\r\n"))
     (⊦= bad-request (http port "GET /greeting HTTP/1.1\r\nHost: x\r\n" #t))   ; EOF inside the header
     (⊦= bad-request (http port (string-append "GET /" (make-string 20000 #\a) " HTTP/1.1\r\n\r\n")))
     (⊦= payload-too-large
         (http port "POST /greeting HTTP/1.1\r\nContent-Length: 1000000000\r\n\r\nname=Ann"))
     (⊦= (ok-reply "22" "Nice to meet you, Bob!") (http port "GET /greeting?name=Bob HTTP/1.1\r\n\r\n"))
     (⊦= (void) (join server))
     (⊨ (logged? (log) 8))))       ; one line per 400, none for the 413

  ; a handler error answers 500, without the partial page, and the server goes on.
  ((test/webserver/serve/handler-error _)
   (let* ((broken #t)
          (handler (λ (path header params)
                     (display "partial")
                     (when broken (set! broken #f) (error "boom"))
                     (display "è"))))
     (receive (port server log) (start-server handler 2)
       (⊦= internal-server-error (http port "GET / HTTP/1.1\r\n\r\n"))
       ; Content-Length counts the bytes of the UTF-8 page: 7 for "partial", 2 for è.
       (⊦= (ok-reply "9" "partialè") (http port "GET / HTTP/1.1\r\n\r\n"))
       (⊦= (void) (join server))
       (⊦= '("serve: boom") (log)))))

  ; a client that trickles its request is answered 408 once time-limit: has passed, and the server
  ; goes on; upstream, and a per-read timeout alone, would wait for the whole request.
  ((test/webserver/serve/trickle _)
   (receive (port server log) (start-server hello-request-handler 2 time-limit: 0.5)
     (let* ((slow (thread-start!
                    (make-thread
                      (τ (parameterize ((tcp-read-timeout 20000))
                           (receive (in out) (tcp-connect "127.0.0.1" port)
                             ; one character every 0.1 s, about 4 s in all, until the reply comes.
                             (let loop ((cs (string->list "GET /greeting?name=Slow HTTP/1.1\r\nHost: x\r\n\r\n")))
                               (unless (or (null? cs) (char-ready? in))
                                 (write-char (car cs) out)
                                 (flush-output out)
                                 (thread-sleep! 0.1)
                                 (loop (cdr cs))))
                             (begin1 (read-string #f in) (close-input-port in) (close-output-port out))))))))
            (start (begin (thread-sleep! 0.05) (current-process-milliseconds)))
            (fast (http port "GET /greeting?name=Fast HTTP/1.1\r\n\r\n"))
            (elapsed (- (current-process-milliseconds) start)))
       (⊦= request-timeout (thread-join! slow))
       (⊦= (ok-reply "23" "Nice to meet you, Fast!") fast)
       (⊨ (< elapsed 2500))
       (⊦= (void) (join server))
       (⊦= '("serve: request timeout") (log)))))

  ; a read that waits longer than tcp-read-timeout is answered 408 too.
  ((test/webserver/serve/stall _)
   (receive (port server log) (parameterize ((tcp-read-timeout 300)) (start-server hello-request-handler 1))
     (receive (in out) (tcp-connect "127.0.0.1" port)
       (display "GET /greeting HTTP/1.1\r\nHost" out)
       (flush-output out)
       (⊦= request-timeout (parameterize ((tcp-read-timeout 20000)) (read-string #f in)))
       (close-input-port in)
       (close-output-port out))
     (⊦= (void) (join server))
     (⊨ (logged? (log) 1))))

  ; port: listens itself, as upstream's (socket-server 8080) does.
  ((test/webserver/serve/port _)
   (let* ((port (let loop ((p 47613))
                  (condition-case (begin (tcp-close (tcp-listen p)) p)
                    ((exn i/o net) (loop (add1 p))))))
          (server (thread-start! (make-thread (τ (serve hello-request-handler port: port requests: 1))))))
     ; the thread may not listen yet: retry the connection for a while.
     (⊦= (ok-reply "22" "Nice to meet you, Bob!")
         (let retry ((k 50))
           (condition-case (http port "GET /greeting?name=Bob HTTP/1.1\r\n\r\n")
             ((exn i/o net) (if (zero? k) 'refused (begin (thread-sleep! 0.05) (retry (sub1 k))))))))
     (⊦= (void) (join server))))

  )

(unittest/✓ land-of-lisp-webserver-suite)
