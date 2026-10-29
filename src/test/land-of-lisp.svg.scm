;
; Tests of (aux land-of-lisp svg), the SVG macros of "Land of Lisp" (Conrad Barski, chapter 17),
; a port of http://landoflisp.com/svg.lisp. The first cases are the examples printed in the book,
; with names written in lower case (CL prints `BLUE` for `'blue`); then the departures, the
; attribute escaping and the string attribute names, and the helper functions.

(import scheme
        (chicken base)
        (chicken port)
        (aux unittest)
        (aux base)
        (aux land-of-lisp svg))

(define-syntax printed ; what `body ...` writes to the current output port
  (syntax-rules ()
    ((_ body ...) (with-output-to-string (τ body ...)))))

(define-suite land-of-lisp-svg-suite

  ((test/land-of-lisp-svg/book _)
   (⊦= "<mytag color=\"blue\" height=\"9\">" (printed (print-tag 'mytag '((color . blue) (height . 9)) #f)))
   (⊦= "</mytag>" (printed (print-tag 'mytag '() #t)))
   (⊦= "<mytag color=\"blue\" height=\"9\"></mytag>" (printed (tag mytag (color 'blue height (+ 4 5)))))
   (⊦= "<mytag color=\"blue\" size=\"big\"><first_inner_tag></first_inner_tag><second_inner_tag></second_inner_tag></mytag>"
       (printed (tag mytag (color 'blue size 'big) (tag first_inner_tag ()) (tag second_inner_tag ()))))
   (⊦= "<html><body>Hello World!</body></html>" (printed (tag html () (tag body () (display "Hello World!")))))
   (⊦= (string-append "<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\">"
                      "<circle cx=\"50\" cy=\"50\" r=\"50\" style=\"fill:rgb(255,0,0);stroke:rgb(155,0,0)\"></circle>"
                      "<circle cx=\"100\" cy=\"100\" r=\"50\" style=\"fill:rgb(0,0,255);stroke:rgb(0,0,155)\"></circle></svg>")
       (printed (svg (tag circle (cx 50 cy 50 r 50 style (svg-style '(255 0 0))))
                     (tag circle (cx 100 cy 100 r 50 style (svg-style '(0 0 255)))))))
   (⊦= "<polygon points=\"0,0 0,100 100,0 \" style=\"fill:rgb(255,0,0);stroke:rgb(155,0,0)\"></polygon>"
       (printed (polygon '((0 . 0) (0 . 100) (100 . 0)) '(255 0 0)))))

  ((test/land-of-lisp-svg/tag _)
   ; attribute values are escaped (a departure), names may be strings, as "xlink:href" in v3
   (⊦= "<a href=\"x?a=1&amp;b=&quot;2&quot;\"></a>" (printed (tag a (href "x?a=1&b=\"2\""))))
   (⊦= "<p>&lt;</p>" (printed (tag p () (display "&lt;"))))  ; the body is not escaped
   (⊦= "<a xlink:href=\"/g\"></a>" (printed (tag a ("xlink:href" "/g"))))
   (⊦= "<br></br>" (printed (tag br ())))
   ; names are downcased, as upstream's `string-downcase`
   (⊦= "<rect width=\"1\"></rect>" (printed (tag RECT (Width 1))))
   ; an odd trailing name is dropped, as upstream's `pairs`
   (⊦= "<g x=\"1\"></g>" (printed (tag g (x 1 y))))
   ; values are evaluated once, in order, before the body
   (⊦= "<g a=\"1\" b=\"2\">3</g>"
       (let1 (n 0)
         (printed (tag g (a (begin (set! n (add1 n)) n) b (begin (set! n (add1 n)) n)) (display (add1 n)))))))

  ((test/land-of-lisp-svg/helpers _)
   (⊦= '((a . b) (c . d) (e . f)) (pairs '(a b c d e f)))
   (⊦= '((a . b)) (pairs '(a b c)))
   (⊦= '() (pairs '()))
   (⊦= '(1 (2 3)) (split '(1 2 3) (list head tail) 'none))
   (⊦= 'none (split '() head 'none))
   (⊦= 1 (let1 (n 0) (split (begin (set! n (add1 n)) '(a)) n 'none)))  ; `val` is evaluated once
   (⊦raises (exn) (split 'atom head 'none))                              ; as CL's `car` of an atom
   (⊦= '(155 0 0) (brightness '(255 0 0) -100))
   (⊦= '(30 255 148) (brightness '(10 250 128) 20))
   (⊦= "fill:rgb(255,63,63);stroke:rgb(155,0,0)" (svg-style '(255 63 63)))
   (⊦= "<polygon points=\"26.0,20.5 \" style=\"fill:rgb(1,2,3);stroke:rgb(0,0,0)\"></polygon>"
       (printed (polygon (list (cons (+ 50 (* 40 -0.6)) 20.5)) '(1 2 3)))))

  )

(unittest/✓ land-of-lisp-svg-suite)
