
(import scheme (chicken base) (chicken bitwise) (chicken io) (chicken port) 
    (only srfi-1 last-pair) srfi-13 srfi-69
    (aux base))


  (define superscript-table
    (alist->hash-table
     '((#\0 . "⁰") (#\1 . "¹") (#\2 . "²") (#\3 . "³") (#\4 . "⁴")
       (#\5 . "⁵") (#\6 . "⁶") (#\7 . "⁷") (#\8 . "⁸") (#\9 . "⁹")
       (#\a . "ᵃ") (#\b . "ᵇ") (#\c . "ᶜ") (#\d . "ᵈ") (#\e . "ᵉ")
       (#\f . "ᶠ") (#\g . "ᵍ") (#\h . "ʰ") (#\i . "ⁱ") (#\j . "ʲ")
       (#\k . "ᵏ") (#\l . "ˡ") (#\m . "ᵐ") (#\n . "ⁿ") (#\o . "ᵒ")
       (#\p . "ᵖ") (#\r . "ʳ") (#\s . "ˢ") (#\t . "ᵗ") (#\u . "ᵘ")
       (#\v . "ᵛ") (#\w . "ʷ") (#\x . "ˣ") (#\y . "ʸ") (#\z . "ᶻ")
       (#\- . "⁻") (#\+ . "⁺") (#\. . "·")  (#\/ . "ᐟ")
       (#\( . "⁽") (#\) . "⁾"))
     eqv?))

  (define (char->superscript c #!optional fallback)
    (hash-table-ref superscript-table c
                    (lambda ()
                      (if fallback
                          fallback
                          (error 'char->superscript
                                 "no superscript glyph for character" c)))))

  (define (number->superscript n #!optional (radix 10) fallback)
    (call-with-output-string
      (lambda (port)
        (string-for-each
         (lambda (c) (write-string (char->superscript c fallback) #f port))
         (number->string n radix)))))

#|

(number->superscript 1234)        ; => "¹²³⁴"
(number->superscript -42)         ; => "⁻⁴²"
(number->superscript 3/4)         ; => "³ᐟ⁴"
(number->superscript 1.5)         ; => "¹·⁵"
(number->superscript 255 16)      ; => "ᶠᶠ"
(number->superscript 3+4i)        ; => "³⁺⁴ⁱ"

|#

(define witness/sentinel '(-1 . 0))

(define make-polynomial
    (λ pairs 
        (let1 (sentinel `(,witness/sentinel . ,pairs))
            (cond 
                ((null? pairs) (set-cdr! sentinel sentinel))
                (else (set-cdr! (last-pair pairs) sentinel)))
            sentinel)))

(define print/polynomial
    (λ (p)
        (let L ((p* (cdr p)))
            (unless (eq? witness/sentinel (car p*))
                (unless (= 1 (cdar p*)) (display (cdar p*)))
                (display "x")
                (display (number->superscript (caar p*)))
                (display " ")
                (L (cdr p*))))))

(define index/var caar)
(define coeff/var cdar)

(define (op/polynomial op)
    (λ (p q)
        (let1 (sentinel `(,witness/sentinel))
            (set-cdr! sentinel
                (let L ((p* (cdr p)) (q* (cdr q)))
                    (let ((i (index/var p*)) (j (index/var q*)))
                        (cond
                            ((< i j) (cons (car q*) (L p* (cdr q*))))
                            ((= i j) (cond
                                        ((< i 0) sentinel)
                                        (else (cons `(,i . ,(op (coeff/var p*) (coeff/var q*))) 
                                                (L (cdr p*) (cdr q*))))))
                            (else (cons (car p*) (L (cdr p*) q*)))))))
            sentinel)))

(define 0/polynomial (make-polynomial))
(define +/polynomial (op/polynomial +))
(define ior/polynomial (op/polynomial bitwise-ior))

#|

(define p1 (make-polynomial '(5 . 1) '(2 . -2) '(0 . -1)))
(define p2 (make-polynomial '(6 . 1) '(2 . 1)))

(+/polynomial p1 p2)
(+/polynomial p2 p1)

|#

(define colors (make-hash-table))

(let L ((i 0) (spec (read)))
    (unless (or (eof-object? spec) (> i 1000000))
        (match/first spec
            (((_ ,block-id ,tx-id _ _ _ _) ,inputs ,outputs)
                (let1 (tx-color (cond
                                    ((null? inputs) (make-polynomial `(,block-id . 1)))
                                    (else (foldr (λ-match/first
                                                    (((_ ,amount ,prev-tx-id _) ,color)
                                                            (let* ( (color-entry (hash-table-ref colors prev-tx-id))
                                                                    (color* (car color-entry))
                                                                    (outputs-count (cdr color-entry)))
                                                                (if (= 1 outputs-count)
                                                                    (hash-table-delete! colors prev-tx-id)
                                                                    (set-cdr! color-entry (sub1 outputs-count)))
                                                                (ior/polynomial color* color))))
                                                0/polynomial inputs))))
                    (let1 (l (length outputs)) (when (> l 0) (hash-table-set! colors tx-id (cons tx-color l))))
                    (print/polynomial tx-color)
                    (newline))))
        (L (add1 i) (read))))