; (aux land-of-lisp svg): the SVG macros of "Land of Lisp" (Conrad Barski, No Starch Press 2010,
; chapter 17), a port of http://landoflisp.com/svg.lisp.
;
; svg.lisp carries no license notice; this file is derived from Land of Lisp and is distributed,
; like the rest of aux, under the MIT license.
;
; `tag` prints an XML element to the current output port: `(tag name (att₁ val₁ att₂ val₂ ...)
; body ...)` prints the opening tag with its attributes, evaluates the body, which prints the
; content, and then prints the closing tag. Attribute names are written literally (a symbol, or a
; string such as "xlink:href"), values are evaluated. `svg` wraps its body in an `svg` element
; with the SVG and XLink namespaces. The functions are upstream's: `pairs`, `print-tag`,
; `brightness`, `svg-style` and `polygon`.
;
; Differences from the original:
;   - `let1` is not ported: (aux base) already has it, as `(let1 (var val) body ...)`; upstream
;     uses it only inside `split`;
;   - `split` is anaphoric, so it is an `ir-macro-transformer` that injects `head` and `tail`; it
;     tests `null?` where upstream tests nil, so a non-list atom still fails in `car`, as in CL;
;   - `tag` is a `syntax-rules` macro that pairs up the attributes at expansion time, as upstream's
;     `(pairs atts)` does, so an odd trailing name is dropped as well;
;   - attribute values are escaped (`&`, `<`, `>` and `"` become entities); upstream prints them
;     as they are;
;   - names print as written: `'blue` prints `blue`, where CL prints `BLUE`; element and attribute
;     names are downcased, as upstream.

(module (aux land-of-lisp svg) (split pairs print-tag tag svg brightness svg-style polygon)

  (import scheme
          (chicken base)
          (chicken format)
          (chicken string)
          (chicken syntax)
          (only (scheme char) string-downcase)
          (aux base))

  (import-for-syntax (aux base))

  ; `(split val yes no)` evaluates `val` once: when it is '() it answers `no`, otherwise `yes`,
  ; where `head` and `tail` are bound to the car and the cdr of the value.
  (define-macro-ir (split expr inject compare)
    (let ((val (cadr expr))
          (yes (caddr expr))
          (no (cadddr expr)))
      `(let ((g ,val))
         (if (null? g)
           ,no
           (let ((,(inject 'head) (car g))
                 (,(inject 'tail) (cdr g)))
             ,yes)))))

  (define pairs
    (λ (lst)
      (letrec ((f (λ (lst acc)
                    (split lst
                           (if (null? tail)
                             (reverse acc)
                             (f (cdr tail) (cons (cons head (car tail)) acc)))
                           (reverse acc)))))
        (f lst '()))))

  (define print-tag/escape
    (λ (v)
      (string-translate* (->string v) '(("&" . "&amp;") ("<" . "&lt;") (">" . "&gt;") ("\"" . "&quot;")))))

  (define print-tag
    (λ (name alst closingp)
      (display #\<)
      (when closingp
        (display #\/))
      (display (string-downcase (->string name)))
      (for-each (λ (att)
                  (format #t " ~a=\"~a\"" (string-downcase (->string (car att))) (print-tag/escape (cdr att))))
                alst)
      (display #\>)))

  ; the "atts" clauses come first, so that the public clause cannot capture them.
  (define-syntax tag
    (syntax-rules ()
      ((_ "atts" name (a v rest ...) (acc ...) body ...) (tag "atts" name (rest ...) (acc ... (cons 'a v)) body ...))
      ((_ "atts" name (a) (acc ...) body ...) (tag "atts" name () (acc ...) body ...))
      ((_ "atts" name () (acc ...) body ...)
       (begin (print-tag 'name (list acc ...) #f) body ... (print-tag 'name '() #t)))
      ((_ name (atts ...) body ...) (tag "atts" name (atts ...) () body ...))))

  (define-syntax svg
    (syntax-rules ()
      ((_ body ...) (tag svg (xmlns "http://www.w3.org/2000/svg" "xmlns:xlink" "http://www.w3.org/1999/xlink")
                         body ...))))

  (define brightness
    (λ (col amt)
      (map (λ (x) (min 255 (max 0 (+ x amt)))) col)))

  (define svg-style
    (λ (color)
      (apply format #f "fill:rgb(~a,~a,~a);stroke:rgb(~a,~a,~a)" (append color (brightness color -100)))))

  ; `points` is a list of pairs `(x . y)`.
  (define polygon
    (λ (points color)
      (tag polygon (points (apply string-append (map (λ (tp) (format #f "~a,~a " (car tp) (cdr tp))) points))
                    style (svg-style color)))))

  )
