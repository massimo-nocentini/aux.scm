; (aux land-of-lisp webserver spiffy): the request handlers of the web server of "Land of Lisp" (Conrad
; Barski, No Starch Press 2010, chapter 13, http://landoflisp.com/webserver.lisp), served by spiffy.
;
; A handler has the contract of webserver.lisp, the one of (aux land-of-lisp webserver): it is called as
; (request-handler path header params) and prints the page to the current output port. Here
;   - `path` is the URL path without its leading /, the segments joined by /: "game.html", "" for /,
;     "a/b.html" for /a/b.html; uri-common has percent-decoded the segments, upstream does not;
;   - `params` is the query alist of uri-common followed, for a POST of a form
;     (application/x-www-form-urlencoded), by the alist of its body; the keys are lower-case symbols,
;     as in (aux land-of-lisp webserver), and the values are uri-common's: UTF-8-decoded strings, #t
;     for a key without = (upstream answers a bare string there) and #f for a value whose percent
;     escapes are malformed;
;   - `header` is an alist of (name . "value") in request order, the names lower-case symbols, the
;     values the header text as intarweb writes it back, the values of a repeated header joined by ", "
;     where the header first appears.
; This file does not derive from the code of webserver.lisp, only from its contract, and is MIT like
; the rest of aux.
;
; The reply is written here, not with spiffy's send-response, which sets Content-Length to the
; string-length of the body, wrong for a non-ASCII page in CHICKEN 6: the page is buffered and sent
; with Content-Type: text/html; charset=utf-8, Content-Length in bytes and Cache-Control: no-store
; (the pages of a game are state, so Back must ask again). No body is sent to a HEAD. A handler error
; is left to spiffy's handle-exception, which logs it to (error-log) and answers 500; thanks to the
; buffering no partial page is sent, and the server goes on.
;
; Robustness, on top of intarweb's limits (4096 characters per line, 64 header lines):
;   - a request line that intarweb does not accept (uri-common rejects a malformed percent escape, so
;     /game.html?chosen=%zz is one) and a header that intarweb's read-headers rejects are answered 400
;     Bad Request by spiffy itself and the connection is closed: the parsers of `strict-request-parsers`,
;     which `serve` installs, turn them into a request for a relative URI, which spiffy refuses with
;     400. spiffy instead closes, without a reply, a connection whose request line is longer than the
;     limit (intarweb raises before any parser runs);
;   - the framing of the body is checked the same way, so that a body is never taken for the next
;     request of the connection: intarweb reads a Content-Length that is not a number of digits
;     ("abc", "-1", "3x") as 0 and keeps the last of repeated ones, so these parsers accept only
;     digits, keep every Content-Length and Transfer-Encoding line, and answer 400 to differing
;     Content-Lengths, to a Transfer-Encoding other than one chunked, to a Transfer-Encoding next to a
;     Content-Length and to one in HTTP/1.0;
;   - a Content-Length above `content-limit` is answered 413 Payload Too Large without reading the
;     body, and a form body that intarweb fails to read (a chunked one longer than `content-limit`) 400;
;     both close the connection, as does any reply to a request whose body is not read (not a form).
;     An empty form body or query (POST with Content-Length: 0, /x?) gives no parameter, as upstream's
;     (parse-params ""), where uri-common answers ((|| . #t));
;   - by default the handler runs holding a mutex of its vhost-handler, so that requests reach it one
;     at a time, as in upstream's server: Dice of Doom v3 and v4 keep the game in global cells.
;     serialize: #f lets a handler that does its own locking run concurrently.
;
; `serve` blocks for ever (spiffy has no stop procedure). A test runs it in a srfi-18 thread on a
; listener of its own (listener:), and stops it with thread-terminate! followed by tcp-close of the
; listener; the threads of connections still open end when their clients close them.
;
; An application that mounts vhost-handler in its own vhost-map, next to static files, parameterizes
; intarweb's request-parsers with `strict-request-parsers` around its start-server, as `serve` does.
;
; Both server modules export `serve`: a program imports one of them, or uses `prefix`.

(module (aux land-of-lisp webserver spiffy) (serve vhost-handler strict-request-parsers)

  (import scheme
          (chicken base)
          (chicken port)
          (chicken string)
          (chicken tcp)
          (chicken bytevector)
          (chicken condition)
          (only (scheme char) string-downcase)
          (only srfi-1 remove every)
          (only srfi-13 string-every)
          srfi-18
          spiffy
          intarweb
          uri-common
          (aux base))

  ; the bound of a request body, in the units of Content-Length.
  (define content-limit 65536)

  ; replies ----------------------------------------------------------------------------------------

  (define text/html '#(text/html ((charset . "utf-8"))))
  (define text/plain '#(text/plain ((charset . "utf-8"))))

  ; respond : integer string vector string -> void, writes the reply to the current request, in place
  ; of spiffy's send-response (see the header); `close` asks the client to close the connection.
  (define (respond code reason content-type body #!key (close #f))
    (let* ((bytes (string->utf8 body))
           (resp (update-response (current-response) code: code reason: reason
                   headers: (headers `((content-type ,content-type)
                                       (content-length ,(bytevector-length bytes))
                                       (cache-control no-store)
                                       ,@(if close '((connection close)) '()))
                                     (response-headers (current-response))))))
      (parameterize ((current-response resp))
        (write-logged-response)
        (unless (eq? (request-method (current-request)) 'HEAD)
          (display body (response-port resp)))
        (finish-response-body resp))))

  ; requests ---------------------------------------------------------------------------------------

  (define (form-body? req)
    (and (eq? (request-method req) 'POST)
         (eq? (header-value 'content-type (request-headers req)) 'application/x-www-form-urlencoded)))

  ; whether `req` carries a body that `vhost-handler` leaves unread, which would be taken for the next
  ; request of the connection.
  (define (unread-body? req)
    (let1 (hs (request-headers req))
      (and (not (form-body? req))
           (or (positive? (header-value 'content-length hs 0))
               (memq 'chunked (header-values 'transfer-encoding hs))))))

  (define (lower-case-key p) (cons (string->symbol (string-downcase (symbol->string (car p)))) (cdr p)))

  ; uri-common's alist of an empty query or form body.
  (define (empty-urlencoded? p) (and (eq? (car p) '||) (eq? (cdr p) #t)))

  (define (request-params req)
    (map lower-case-key
         (remove empty-urlencoded?
                 (append (or (uri-query (request-uri req)) '())
                         (if (form-body? req) (read-urlencoded-request-data req content-limit) '())))))

  ; intarweb keeps the headers last first.
  (define (request-header req)
    (reverse (map (λ (h) (cons (car h) (string-intersperse (unparse-header (car h) (cdr h)) ", ")))
                  (headers->list (request-headers req)))))

  (define (request-path req) (string-intersperse (cdr (uri-path (request-uri req))) "/"))

  ; a request line that no parser accepts, or a header that the parser of HTTP/1.x rejects, becomes a
  ; request for a relative URI, which spiffy answers 400; HTTP/1.0 without keep-alive, so that spiffy
  ; closes the connection after.
  (define (bad-request-parser line in)
    (make-request method: 'GET uri: (uri-reference "bad-request") major: 1 minor: 0
                  headers: (headers '()) port: in))

  ; the headers that frame the body, parsed strictly and kept when repeated.
  (define framing-headers '(content-length transfer-encoding))

  (define (strict-parser name accept)
    (single (λ (s) (or (accept s) (error name "malformed header" s)))))

  (define content-length-parser
    (strict-parser 'content-length
                   (λ (s) (and (positive? (string-length s)) (string-every (λ (c) (char<=? #\0 c #\9)) s)
                               (string->number s)))))

  (define transfer-encoding-parser
    (strict-parser 'transfer-encoding (λ (s) (and (string-ci=? s "chunked") 'chunked))))

  ; whether the body of `req` has one length that intarweb and any proxy in front agree on.
  (define (well-framed? req)
    (let* ((hs (request-headers req))
           (lengths (header-values 'content-length hs))
           (encodings (header-values 'transfer-encoding hs)))
      (and (or (null? lengths) (every (λ (n) (= n (car lengths))) lengths))
           (or (null? encodings)
               (and (null? (cdr encodings))
                    (null? lengths)
                    (or (> (request-major req) 1) (>= (request-minor req) 1)))))))

  (define (guarded-parser parser)
    (λ (line in)
      (handle-exceptions _ (bad-request-parser line in)
        (let1 (req (let1 (default (header-parse-error-handler))
                     (parameterize ((header-parsers `((content-length . ,content-length-parser)
                                                      (transfer-encoding . ,transfer-encoding-parser)
                                                      ,@(header-parsers)))
                                    (header-parse-error-handler
                                      (λ (name contents headers exn)
                                        (if (memq name framing-headers)
                                          (signal exn)
                                          (default name contents headers exn))))
                                    (single-headers (remove (λ (name) (memq name framing-headers))
                                                            (single-headers))))
                       (parser line in))))
          (if (and req (not (well-framed? req))) (bad-request-parser line in) req)))))

  ; the request parsers that `serve` installs in intarweb's request-parsers: HTTP/1.x with the checks
  ; of the header, then the fallback to 400.
  (define strict-request-parsers (list (guarded-parser http-1.x-request-parser) bad-request-parser))

  ; serving ----------------------------------------------------------------------------------------

  (define (with-lock mutex thunk)
    (dynamic-wind (τ (mutex-lock! mutex)) thunk (τ (mutex-unlock! mutex))))

  ; vhost-handler : procedure -> procedure, a spiffy vhost handler (λ (continue) ...) that answers every
  ; request of its vhost with `request-handler`, for an application that mounts it in its own vhost-map.
  (define (vhost-handler request-handler #!key (serialize #t))
    (let1 (mutex (make-mutex 'land-of-lisp))
      (λ (continue)
        (let1 (req (current-request))
          (cond
            ((> (header-value 'content-length (request-headers req) 0) content-limit)
             (respond 413 "Payload Too Large" text/plain "Payload Too Large\n" close: #t))
            ((handle-exceptions _ #f (request-params req))
             => (λ (params)
                  (let* ((path (request-path req))
                         (header (request-header req))
                         (page (τ (with-output-to-string (τ (request-handler path header params))))))
                    (respond 200 "OK" text/html (if serialize (with-lock mutex page) (page))
                             close: (unread-body? req)))))
            (else (respond 400 "Bad Request" text/plain "Bad Request\n" close: #t)))))))

  (define (serve request-handler #!key (port 8080) (bind-address #f) (listener #f) (serialize #t))
    (parameterize ((vhost-map `((".*" . ,(vhost-handler request-handler serialize: serialize))))
                   (request-parsers strict-request-parsers))
      (start-server port: (if listener (tcp-listener-port listener) port)
                    bind-address: bind-address
                    listen: (if listener (λ _ listener) tcp-listen))))

  )
