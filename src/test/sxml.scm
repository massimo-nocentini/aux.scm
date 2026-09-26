(import (only (chicken file) create-temporary-file delete-file)
        (only (chicken io) read-string)
        (only (chicken irregex) irregex-replace/all)
        (only (scheme base) open-input-string)
        (only spiffy current-request)
        (only intarweb make-request request-method headers)
        (only uri-common uri-reference uri-path)
        ; no (aux base) here: `define-vhost` must work for a client that imports (aux sxml) only.
        (aux unittest) (aux sxml))

; renders `tree` with `SXML->HTML->file!` and answers the markup it wrote, without the
; doctype and the newlines the serializer puts between elements.
(define (SXML->HTML->string tree)
  (let* ((filename (create-temporary-file))
         (html (string-append filename ".html")))
    (SXML->HTML->file! tree filename)
    (let ((s (with-input-from-file html read-string)))
      (delete-file filename)
      (delete-file html)
      (irregex-replace/all "\n" (substring s (string-length "<!doctype html>")) ""))))

; answers what the `define-vhost` handler `h` does with a request `method path`, where the
; continuation it gets for an unrouted request answers `'unrouted`.
(define (vhost-dispatch h method path)
  (parameterize ((current-request (make-request uri: (uri-reference (string-append "http://example.org" path))
                                                method: method
                                                port: (open-input-string "")
                                                headers: (headers '((content-length 0))))))
    (h (lambda () 'unrouted))))

(define-vhost (vhost request $)
  (GET (("") 'root)
       (("foo") (list 'foo (request-method request)))
       (("bar" ,x) (list 'bar x ($ 'q))))
  (POST (("foo") 'posted)))

(define-suite sxml-suite

  ((test/math-numbers _)
   ; every real is `rational?` in CHICKEN: integers and flonums are plain `mn`s,
   (⊦= "<math display=\"block\"><mn>3</mn></math>" (SXML->HTML->string '((displaymath (m 3)))))
   (⊦= "<math display=\"block\"><mn>3.14</mn></math>" (SXML->HTML->string '((displaymath (m 3.14)))))
   ; only an exact ratio is a fraction, whose two children are elements.
   (⊦= "<math display=\"block\"><mfrac><mn>1</mn><mn>2</mn></mfrac></math>"
       (SXML->HTML->string '((displaymath (m 1/2)))))
   (⊦= "<math display=\"block\"><mfrac><mn>1</mn><mn>2</mn></mfrac></math>"
       (SXML->HTML->string '((displaymath (frac 1 2)))))
   (⊦= "<math display=\"block\"><mfrac><mi>x</mi><mn>2</mn></mfrac></math>"
       (SXML->HTML->string '((displaymath (frac (m x) 2)))))
   (⊦= "<math display=\"block\"><mrow><mi>x</mi><mi>+</mi><mn>1</mn></mrow></math>"
       (SXML->HTML->string '((displaymath (m (x + 1)))))))

  ; the routes match `uri-path`'s `(/ "seg" ...)`, per method, falling back to the continuation.
  ((test/define-vhost _)
   (⊦= '(/ "foo") (uri-path (uri-reference "http://example.org/foo")))
   (⊦= 'root (vhost-dispatch vhost 'GET "/"))
   (⊦= '(foo GET) (vhost-dispatch vhost 'GET "/foo"))
   (⊦= '(bar "baz" "1") (vhost-dispatch vhost 'GET "/bar/baz?q=1"))
   (⊦= 'posted (vhost-dispatch vhost 'POST "/foo"))
   (⊦= 'unrouted (vhost-dispatch vhost 'PUT "/foo"))
   ; a path no route of its method matches falls back too, instead of raising.
   (⊦= 'unrouted (vhost-dispatch vhost 'GET "/nope"))
   (⊦= 'unrouted (vhost-dispatch vhost 'GET "/bar/baz/qux"))
   (⊦= 'unrouted (vhost-dispatch vhost 'POST "/")))

  )

(unittest/✓ sxml-suite)
