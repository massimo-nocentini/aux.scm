
(import scheme (only srfi-1 last-pair) (chicken base) (aux base))

(define witness/sentinel '(-1 . 0))

(define make-polynomial
    (λ pairs 
        (let1 (sentinel `(,witness/sentinel . ,pairs))
            (cond 
                ((null? pairs) (set-cdr! sentinel sentinel))
                (else (set-cdr! (last-pair pairs) sentinel)))
            sentinel)))

(define index/var caar)
(define coeff/var cdar)

(make-polynomial '(5 . 1) '(1 . -2) '(0 . -1))
(make-polynomial )

(define +/polynomial
    (λ (p q)
        (let1 (sentinel `(,witness/sentinel))
            (set-cdr! sentinel
                (let L ((p* (cdr p)) (qprev q) (q* (cdr q)))
                    (cond
                        ((< (index/var p*) (index/var q*)) (cons (car q*) (L p* q* (cdr q*))))
                        ((= (index/var p*) (index/var q*)) (cond 
                                                            ((= -1 (index/var p*)) sentinel)
                                                            (else (cons `(,(index/var p*) . ,(+ (coeff/var p*) (coeff/var q*))) (L (cdr p*) q* (cdr q*))))))
                        (else (cons (car p*) (L (cdr p*) qprev q*))))))
            sentinel)))

(define p1 (make-polynomial '(5 . 1) '(2 . -2) '(0 . -1)))
(define p2 (make-polynomial '(6 . 1) '(2 . 1)))

(+/polynomial p1 p2)