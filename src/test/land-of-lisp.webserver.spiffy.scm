; Tests of (aux land-of-lisp webserver spiffy), the handlers of the web server of "Land of Lisp" (Conrad
; Barski, chapter 13) served by spiffy.
;
; Each case starts spiffy in a srfi-18 thread on a listener of its own, (tcp-listen 0) on the loopback,
; so the kernel picks a free port; the case that passes port: itself probes ports from 47713 on,
; skipping those in use. spiffy has no stop procedure: a case ends its server with thread-terminate!
; and closes the listener. spiffy keeps connections alive, so the client does not wait for an EOF: it
; reads the status line and the header, then as many bytes as Content-Length says. The example of the
; book is hello-request-handler of (aux land-of-lisp webserver), imported for it alone.

(import scheme
        (chicken base)
        (chicken io)
        (chicken tcp)
        (chicken port)
        (chicken string)
        (chicken condition)
        (chicken bytevector)
        (only (scheme base) read-bytevector open-output-string get-output-string make-list)
        (only (scheme char) string-downcase)
        (only srfi-13 string-index string-trim-both string-prefix?)
        srfi-18
        (only spiffy error-log vhost-map start-server)
        (only intarweb request-parsers)
        (aux base)
        (aux unittest)
        (only (aux land-of-lisp webserver) hello-request-handler)
        (aux land-of-lisp webserver spiffy))

;;; ==================== HELPERS ====================

; with-server : procedure (integer -> value) -> value, serves `handler` with spiffy on a fresh loopback
; listener while (body port) runs, `serve-args` passed on to serve; spiffy's error log goes to a string
; port, which `body` reads with (log).
(define (with-server handler body #!rest serve-args)
  (let* ((L (tcp-listen 0 10 "127.0.0.1"))
         (err (open-output-string))
         (th (parameterize ((error-log err))
               (make-thread (τ (apply serve handler listener: L serve-args))))))
    (thread-start! th)
    (dynamic-wind
      void
      (τ (body (tcp-listener-port L) (τ (get-output-string err))))
      (τ (thread-terminate! th) (tcp-close L)))))

; read-reply : input-port -> (list status header body) or 'closed, one reply of a kept-alive
; connection; the header names are lower-case strings. With `head`, the reply has no body.
(define (read-reply in #!optional head)
  (let1 (status (read-line in))
    (if (eof-object? status)
      'closed
      (let* ((header (let loop ((acc '()))
                       (let1 (line (read-line in))
                         (if (or (eof-object? line) (equal? line ""))
                           (reverse acc)
                           (let1 (i (string-index line #\:))
                             (loop (cons (cons (string-downcase (substring line 0 i))
                                               (string-trim-both (substring line (add1 i))))
                                         acc)))))))
             (n (string->number (alist-ref "content-length" header equal? "0"))))
        (list status header (if (or head (zero? n)) "" (utf8->string (read-bytevector n in))))))))

; http : integer string ... -> (list reply ...), sends each request on one connection and reads its reply.
(define (http port . requests)
  (parameterize ((tcp-read-timeout 20000))
    (receive (in out) (tcp-connect "127.0.0.1" port)
      (begin1
        (map (λ (text)
               (display text out)
               (flush-output out)
               (read-reply in (string-prefix? "HEAD " text)))
             requests)
        (close-input-port in)
        (close-output-port out)))))

; http1 : integer string -> reply, a single request on its own connection.
(define (http1 port text) (car (http port text)))

(define reply-status car)
(define (reply-header name reply) (alist-ref name (cadr reply) equal?))
(define reply-body caddr)

(define (get path) (string-append "GET " path " HTTP/1.1\r\nHost: localhost\r\n\r\n"))

(define (post path body)
  (string-append "POST " path " HTTP/1.1\r\nHost: localhost\r\n"
                 "Content-Type: application/x-www-form-urlencoded\r\n"
                 "Content-Length: " (number->string (string-length body)) "\r\n\r\n" body))

; post/header : string string string -> string, a POST of `type` with the raw framing header lines
; `framing` (each ending in CRLF) and the raw `body`.
(define (post/header type framing body)
  (string-append "POST /greeting HTTP/1.1\r\nHost: localhost\r\nContent-Type: " type "\r\n" framing "\r\n" body))

; statuses : (list reply ...) -> (list string-or-'closed ...)
(define (statuses rs) (map (λ (r) (if (pair? r) (reply-status r) r)) rs))

;;; ==================== SUITE ====================

(define-suite land-of-lisp-webserver-spiffy-suite

  ; the example of the book.
  ((test/webserver-spiffy/hello _)
   (with-server hello-request-handler
     (λ (port log)
       (let1 (r (http1 port (get "/greeting")))
         (⊦= "HTTP/1.1 200 OK" (reply-status r))
         (⊦= "text/html; charset=utf-8" (reply-header "content-type" r))
         (⊦= "no-store" (reply-header "cache-control" r))
         (⊦= "52" (reply-header "content-length" r))
         (⊦= "<form>What is your name?<input name='name' /></form>" (reply-body r)))
       (⊦= "Nice to meet you, Bob!" (reply-body (http1 port (get "/greeting?name=Bob"))))
       (⊦= "Nice to meet you, Ann!" (reply-body (http1 port (post "/greeting" "name=Ann"))))
       (⊦= "Sorry... I don't know that page." (reply-body (http1 port (get "/other"))))
       ; query parameters come before the body's; keys are lower-case.
       (⊦= "Nice to meet you, Bob!" (reply-body (http1 port (post "/greeting?Name=Bob" "name=Ann"))))
       ; several requests on one kept-alive connection.
       (⊦= '("Nice to meet you, A!" "Nice to meet you, B!" "Sorry... I don't know that page.")
           (map reply-body (http port (get "/greeting?name=A") (get "/greeting?name=B") (get "/")))))))

  ((test/webserver-spiffy/contract _)
   (with-server (λ (path header params)
                  (write (if (equal? path "header") header (list path (alist-ref 'host header) params))))
     (λ (port log)
       (⊦= '("" "localhost" ()) (with-input-from-string (reply-body (http1 port (get "/"))) read))
       (⊦= '("a/b.html" "localhost" ()) (with-input-from-string (reply-body (http1 port (get "/a/b.html"))) read))
       ; the header in request order, a repeated header joined.
       (⊦= '((host . "localhost") (x-b . "1, 3") (x-a . "2"))
           (with-input-from-string
             (reply-body (http1 port "GET /header HTTP/1.1\r\nHost: localhost\r\nX-B: 1\r\nX-A: 2\r\nX-B: 3\r\n\r\n"))
             read))
       ; values are UTF-8-decoded, a key without = has #t, and the body follows the query.
       (⊦= '("game.html" "localhost" ((chosen . "3") (pass . #t) (name . "è") (x . "1 2")))
           (with-input-from-string
             (reply-body (http1 port (post "/game.html?chosen=3&PASS" "name=%C3%A8&X=1+2")))
             read))
       ; an empty query or form body gives no parameter, as upstream's (parse-params "").
       (⊦= '("x" "localhost" ()) (with-input-from-string (reply-body (http1 port (get "/x?"))) read))
       (⊦= '("x" "localhost" ()) (with-input-from-string (reply-body (http1 port (post "/x" ""))) read))
       ; a chunked form body is read.
       (⊦= '("greeting" "localhost" ((name . "Ann")))
           (with-input-from-string
             (reply-body (http1 port (post/header "application/x-www-form-urlencoded"
                                                  "Transfer-Encoding: chunked\r\n" "8\r\nname=Ann\r\n0\r\n\r\n")))
             read)))))

  ; Content-Length counts the bytes of the UTF-8 page (spiffy's send-response would say 1).
  ((test/webserver-spiffy/utf-8 _)
   (with-server (λ (path header params) (display "è"))
     (λ (port log)
       (let1 (r (http1 port (get "/")))
         (⊦= "2" (reply-header "content-length" r))
         (⊦= "è" (reply-body r)))
       ; a HEAD gets the header of the page, and no body: the next reply of the connection follows it.
       (let1 (rs (http port "HEAD / HTTP/1.1\r\nHost: localhost\r\n\r\n" (get "/")))
         (⊦= '("HTTP/1.1 200 OK" "HTTP/1.1 200 OK") (statuses rs))
         (⊦= "2" (reply-header "content-length" (car rs)))
         (⊦= "è" (reply-body (cadr rs)))))))

  ; a handler error answers 500, without the partial page, and the server goes on.
  ((test/webserver-spiffy/handler-error _)
   (let* ((broken #t)
          (handler (λ (path header params)
                     (display "partial")
                     (when broken (set! broken #f) (error "boom"))
                     (display "ok"))))
     (with-server handler
       (λ (port log)
         (let1 (r (http1 port (get "/")))
           (⊦= "HTTP/1.1 500 Internal Server Error" (reply-status r))
           (⊨ (not (substring-index "partial" (reply-body r)))))
         (⊦= "partialok" (reply-body (http1 port (get "/"))))
         (⊨ (number? (substring-index "boom" (log))))))))

  ; malformed and oversized requests; the server goes on.
  ((test/webserver-spiffy/malformed _)
   (with-server hello-request-handler
     (λ (port log)
       (⊦= "HTTP/1.1 400 Bad Request" (reply-status (http1 port (get "/greeting?name=%zz"))))
       (⊦= "HTTP/1.1 400 Bad Request" (reply-status (http1 port (get "/%4"))))
       (⊦= "HTTP/1.1 400 Bad Request" (reply-status (http1 port "GARBAGE\r\n\r\n")))
       (⊦= "HTTP/1.1 400 Bad Request" (reply-status (http1 port "GET /greeting\r\n\r\n")))
       ; a chunked form longer than the limit, which has no Content-Length to refuse it by.
       (let1 (r (http1 port (post/header "application/x-www-form-urlencoded" "Transfer-Encoding: chunked\r\n"
                                         (string-append "11171\r\n" (make-string 70001 #\a) "\r\n0\r\n\r\n"))))
         (⊦= "HTTP/1.1 400 Bad Request" (reply-status r))
         (⊦= "close" (reply-header "connection" r)))
       ; 65 header lines, one more than intarweb accepts.
       (⊦= "HTTP/1.1 400 Bad Request"
           (reply-status (http1 port (string-append "GET / HTTP/1.1\r\n"
                                                    (apply string-append (make-list 65 "X-A: 1\r\n"))
                                                    "\r\n"))))
       ; a 400 closes the connection: the second request is not read.
       (⊦= '("HTTP/1.1 400 Bad Request" closed)
           (map (λ (r) (if (pair? r) (reply-status r) r)) (http port "GARBAGE\r\n\r\n" (get "/greeting"))))
       (let1 (r (http1 port "POST /greeting HTTP/1.1\r\nHost: localhost\r\nContent-Length: 1000000000\r\n\r\nname=Ann"))
         (⊦= "HTTP/1.1 413 Payload Too Large" (reply-status r))
         (⊦= "close" (reply-header "connection" r)))
       ; an over-long request line: intarweb raises before any parser runs, spiffy closes.
       (⊦= 'closed (http1 port (get (string-append "/" (make-string 5000 #\a)))))
       ; the body of a request that is not a form is not read: the connection is closed after it.
       (let1 (rs (http port (string-append "POST /greeting HTTP/1.1\r\nHost: localhost\r\n"
                                           "Content-Type: text/plain\r\nContent-Length: 8\r\n\r\nname=Ann")
                       (get "/greeting?name=Bob")))
         (⊦= "<form>What is your name?<input name='name' /></form>" (reply-body (car rs)))
         (⊦= "close" (reply-header "connection" (car rs)))
         (⊦= 'closed (cadr rs)))
       (⊦= "Nice to meet you, Bob!" (reply-body (http1 port (get "/greeting?name=Bob")))))))

  ; a body whose length is not clear is answered 400 and the connection closed, so that it is never
  ; read as the next request; intarweb alone reads "-1" and "abc" as 0 and keeps the last of repeated
  ; Content-Lengths.
  ((test/webserver-spiffy/framing _)
   (with-server (λ (path header params) (write (list path params)))
     (λ (port log)
       (let1 (smuggled (get "/smuggled?z=9"))
         (for-each
           (λ (type framing)
             (⊦= '("HTTP/1.1 400 Bad Request" closed)
                 (statuses (http port (post/header type framing smuggled) (get "/after")))))
           '("application/x-www-form-urlencoded" "text/plain" "application/x-www-form-urlencoded"
             "application/x-www-form-urlencoded" "text/plain" "application/x-www-form-urlencoded"
             "application/x-www-form-urlencoded" "text/plain")
           '("Content-Length: -1\r\n" "Content-Length: abc\r\n" "Content-Length: 3x\r\n" "Content-Length:\r\n"
             "Content-Length: 0\r\nContent-Length: 30\r\n" "Transfer-Encoding: gzip\r\n"
             "Transfer-Encoding: chunked\r\nContent-Length: 0\r\n"
             "Transfer-Encoding: chunked\r\nTransfer-Encoding: chunked\r\n")))
       ; HTTP/1.0 has no chunked encoding.
       (⊦= "HTTP/1.1 400 Bad Request"
           (reply-status (http1 port "POST /x HTTP/1.0\r\nTransfer-Encoding: chunked\r\n\r\n0\r\n\r\n")))
       ; a repeated Content-Length that agrees is accepted.
       (⊦= '("HTTP/1.1 200 OK" "HTTP/1.1 200 OK")
           (statuses (http port (post/header "application/x-www-form-urlencoded"
                                             "Content-Length: 5\r\nContent-Length: 5\r\n" "a=1&b")
                           (get "/after")))))))

  ; with serialize: #t (the default) requests reach the handler one at a time.
  ((test/webserver-spiffy/serialize _)
   (define (max-overlap serialize)
     (let* ((inside 0)
            (most 0)
            (handler (λ (path header params)
                       (add1! inside)
                       (set! most (max most inside))
                       (thread-sleep! 0.2)
                       (sub1! inside)
                       (display most))))
       (with-server handler
         (λ (port log)
           (let1 (clients (map (λ (_) (thread-start! (make-thread (τ (http1 port (get "/")))))) '(1 2 3)))
             (for-each thread-join! clients)
             most))
         serialize: serialize)))
   (⊦= 1 (max-overlap #t))
   (⊦= 3 (max-overlap #f)))

  ; vhost-handler mounted in an application's own vhost-map, next to a handler of its own.
  ((test/webserver-spiffy/vhost-handler _)
   (let* ((L (tcp-listen 0 10 "127.0.0.1"))
          (err (open-output-string))
          (th (parameterize ((error-log err)
                             (request-parsers strict-request-parsers)
                             (vhost-map `(("localhost" . ,(vhost-handler hello-request-handler serialize: #f))
                                          (".*" . ,(λ (continue) (continue))))))
                (make-thread (τ (start-server port: (tcp-listener-port L) listen: (λ _ L)))))))
     (thread-start! th)
     (dynamic-wind
       void
       (τ (⊦= "Nice to meet you, Bob!" (reply-body (http1 (tcp-listener-port L) (get "/greeting?name=Bob"))))
          ; another vhost falls through to spiffy's static files, here none.
          (⊦= "HTTP/1.1 404 Not Found"
              (reply-status (http1 (tcp-listener-port L) "GET /greeting HTTP/1.1\r\nHost: other\r\n\r\n")))
          (⊦= '("HTTP/1.1 400 Bad Request" closed)
              (statuses (http (tcp-listener-port L)
                              (post/header "text/plain" "Content-Length: abc\r\n" (get "/greeting?name=Eve"))
                              (get "/")))))
       (τ (thread-terminate! th) (tcp-close L)))))

  ; port: listens itself; the round trip of the book's example.
  ((test/webserver-spiffy/port _)
   (let* ((port (let loop ((p 47713))
                  (condition-case (begin (tcp-close (tcp-listen p 10 "127.0.0.1")) p)
                    ((exn i/o net) (loop (add1 p))))))
          (err (open-output-string))
          (server (parameterize ((error-log err))
                    (thread-start! (make-thread (τ (serve hello-request-handler port: port bind-address: "127.0.0.1")))))))
     ; the thread may not listen yet: retry the connection for a while.
     (⊦= "Nice to meet you, Bob!"
         (let retry ((k 50))
           (condition-case (reply-body (http1 port (get "/greeting?name=Bob")))
             ((exn i/o net) (if (zero? k) 'refused (begin (thread-sleep! 0.05) (retry (sub1 k))))))))
     (thread-terminate! server)))

  )

(unittest/✓ land-of-lisp-webserver-spiffy-suite)
