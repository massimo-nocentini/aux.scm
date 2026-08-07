
;;; Polynomials as circular linked lists, after Knuth's TAOCP §2.2.4.
;;;
;;; A polynomial is a circular list of nodes.  The head node is a *sentinel*
;;; carrying the term (-1 . 0); the nodes after it carry the real terms, one
;;; `(exponent . coefficient)` pair each, sorted by strictly decreasing
;;; exponent.  Since -1 is smaller than every legal exponent, the sentinel acts
;;; as a term that compares below all others, and the addition loop needs no
;;; end-of-list tests at all: meeting the sentinel in *both* operands at the
;;; same time is what terminates it.

(import scheme (only srfi-1 last-pair) (chicken base) (aux base) (aux unittest))

;;; terms

(define (term/make exponent coeff) (cons exponent coeff))
(define term/exponent car)
(define term/coeff cdr)

(define sentinel/exponent -1)           ; below every legal exponent
(define (sentinel/term) (term/make sentinel/exponent 0))

;;; nodes

(define node/term car)
(define node/next cdr)
(define node/exponent (o term/exponent node/term))
(define node/coeff (o term/coeff node/term))
(define (node/sentinel? node) (= sentinel/exponent (node/exponent node)))

;;; polynomials

;; Each polynomial owns a fresh sentinel, so no two rings share structure.
;; `last-pair` lands on the sentinel itself when there are no terms, which is
;; exactly the self-loop an empty polynomial needs.
(define make-polynomial
  (λ terms
    (let1 (p (cons (sentinel/term) terms))
      (set-cdr! (last-pair p) p)
      p)))

(define (polynomial/empty? p) (node/sentinel? (node/next p)))

(define (polynomial->list p)
  (let L ((node (node/next p)))
    (if (node/sentinel? node)
      '()
      (cons (node/term node) (L (node/next node))))))

;; Walk the ring from the sentinel back to it, adding p and q term by term.
;; The sum is built as a fresh ring whose terms are freshly consed too, so
;; mutating it can never reach back into either operand.
(define (+/polynomial p q)
  (let1 (sum (list (sentinel/term)))
    (set-cdr! sum
      (let L ((p* (node/next p)) (q* (node/next q)))
        (let ((i (node/exponent p*)) (j (node/exponent q*)))
          (cond
            ((< i j) (cons (term/make j (node/coeff q*)) (L p* (node/next q*))))
            ((> i j) (cons (term/make i (node/coeff p*)) (L (node/next p*) q*)))
            ((node/sentinel? p*) sum)   ; both exhausted: close the ring
            (else (cons (term/make i (+ (node/coeff p*) (node/coeff q*)))
                        (L (node/next p*) (node/next q*))))))))
    sum))

;;; tests

;; Steps needed to walk from the sentinel back to it.  This diverges rather
;; than reporting failure on a non-circular list, which cannot happen for a
;; polynomial built by the constructors above.
(define (ring-length p)
  (let L ((node (node/next p)) (n 1))
    (if (eq? node p) n (L (node/next node) (add1 n)))))

(define-suite circular-polynomial-suite

  ((test-empty _)
   (let1 (p (make-polynomial))
     (⊨ (polynomial/empty? p))
     (⊦= '() (polynomial->list p))
     (⊦= 1 (ring-length p))
     (⊨ (eq? p (node/next p)))))

  ((test-make _)
   (let1 (p (make-polynomial '(5 . 1) '(1 . -2) '(0 . -1)))
     (⊭ (polynomial/empty? p))
     (⊦= '((5 . 1) (1 . -2) (0 . -1)) (polynomial->list p))
     (⊦= 4 (ring-length p))
     (⊦= sentinel/exponent (node/exponent p))))

  ((test-sentinels-are-not-shared _)
   (let ((p (make-polynomial '(1 . 1)))
         (q (make-polynomial '(1 . 1))))
     (⊭ (eq? (node/term p) (node/term q)))))

  ((test-add _)
   (let ((p (make-polynomial '(5 . 1) '(2 . -2) '(0 . -1)))
         (q (make-polynomial '(6 . 1) '(2 . 1))))
     (⊦= '((6 . 1) (5 . 1) (2 . -1) (0 . -1))
         (polynomial->list (+/polynomial p q)))
     ;; addition is commutative and leaves both operands alone
     (⊦= '((6 . 1) (5 . 1) (2 . -1) (0 . -1))
         (polynomial->list (+/polynomial q p)))
     (⊦= '((5 . 1) (2 . -2) (0 . -1)) (polynomial->list p))
     (⊦= '((6 . 1) (2 . 1)) (polynomial->list q))))

  ((test-add-is-closed _)
   (let1 (sum (+/polynomial (make-polynomial '(3 . 1)) (make-polynomial '(1 . 1))))
     (⊦= 3 (ring-length sum))
     (⊦= sentinel/exponent (node/exponent sum))
     ;; being a polynomial again, it can be fed straight back in
     (⊦= '((3 . 2) (1 . 2)) (polynomial->list (+/polynomial sum sum)))))

  ((test-add-empty-is-identity _)
   (let ((p (make-polynomial '(2 . 3) '(0 . 4)))
         (zero (make-polynomial)))
     (⊦= '((2 . 3) (0 . 4)) (polynomial->list (+/polynomial p zero)))
     (⊦= '((2 . 3) (0 . 4)) (polynomial->list (+/polynomial zero p)))
     (⊦= '() (polynomial->list (+/polynomial zero zero)))))

  ((test-add-disjoint-exponents _)
   (let ((p (make-polynomial '(4 . 1) '(2 . 1)))
         (q (make-polynomial '(3 . 1) '(1 . 1))))
     (⊦= '((4 . 1) (3 . 1) (2 . 1) (1 . 1))
         (polynomial->list (+/polynomial p q)))))

  ((test-add-cancelling-terms _)
   ;; NOTE: cancellation is *not* pruned -- terms with a zero coefficient stay
   ;; in the ring, so the representation is not canonical.
   (let ((p (make-polynomial '(5 . 1) '(2 . -2)))
         (q (make-polynomial '(5 . -1) '(2 . 2))))
     (⊦= '((5 . 0) (2 . 0)) (polynomial->list (+/polynomial p q)))))
  )

(unittest/✓ circular-polynomial-suite)
