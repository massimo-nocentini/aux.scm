;;; Tests of (aux j-bob lang), (aux j-bob) and (aux j-bob little-prover), the port of J-Bob, the proof
;;; assistant of "The Little Prover" by Daniel P. Friedman and Carl Eastlund
;;; (https://github.com/the-little-prover/j-bob, commit 2a4567b). J-Bob is distributed under the 2-clause
;;; BSD license, and so is this file, which is derived from it; the license text is in COPYING.J-BOB.
;;;
;;; Upstream has no tests: its transcript little-prover.scm is the test. The case test/transcript runs
;;; every function of (aux j-bob little-prover) and compares what it answers with the file
;;; test/j-bob/little-prover.scm, what the same function answered when upstream's three files were loaded
;;; into csi. The other cases
;;; check the language, the three entry points on well-formed and ill-formed input, and the two holes of
;;; upstream that the port keeps.
;;;
;;; J-Bob's language is imported with the prefix jb:, so that if, car, cdr, + and < stay Scheme's here.

(import scheme
        (chicken base)
        (only (chicken io) read-list)
        (aux base)
        (aux unittest)
        (prefix (aux j-bob lang) jb:)
        (aux j-bob)
        (aux j-bob little-prover))

;;; ==================== HELPERS ====================

; golden : -> (listof (list symbol value)), what each function of little-prover.scm answered upstream.
(define (golden) (with-input-from-file "j-bob/little-prover.scm" read-list))

; transcript : (list symbol procedure) ..., the 49 functions of (aux j-bob little-prover) in upstream order.
(define transcript
  (list (list 'chapter1.example1 chapter1.example1) (list 'chapter1.example2 chapter1.example2)
        (list 'chapter1.example3 chapter1.example3) (list 'chapter1.example4 chapter1.example4)
        (list 'chapter1.example5 chapter1.example5) (list 'chapter1.example6 chapter1.example6)
        (list 'chapter1.example7 chapter1.example7) (list 'chapter1.example8 chapter1.example8)
        (list 'chapter1.example9 chapter1.example9) (list 'chapter1.example10 chapter1.example10)
        (list 'chapter1.example11 chapter1.example11) (list 'chapter1.example12 chapter1.example12)
        (list 'chapter2.example1 chapter2.example1) (list 'chapter2.example2 chapter2.example2)
        (list 'chapter2.example3 chapter2.example3)
        (list 'defun.pair defun.pair) (list 'defun.first-of defun.first-of)
        (list 'defun.second-of defun.second-of) (list 'dethm.first-of-pair dethm.first-of-pair)
        (list 'dethm.second-of-pair dethm.second-of-pair) (list 'defun.in-pair? defun.in-pair?)
        (list 'dethm.in-first-of-pair dethm.in-first-of-pair)
        (list 'dethm.in-second-of-pair dethm.in-second-of-pair)
        (list 'defun.list0? defun.list0?) (list 'defun.list1? defun.list1?) (list 'defun.list2? defun.list2?)
        (list 'dethm.contradiction dethm.contradiction) (list 'defun.list? defun.list?)
        (list 'defun.sub defun.sub) (list 'defun.memb? defun.memb?) (list 'defun.remb defun.remb)
        (list 'dethm.memb?/remb0 dethm.memb?/remb0) (list 'dethm.memb?/remb1 dethm.memb?/remb1)
        (list 'dethm.memb?/remb2 dethm.memb?/remb2) (list 'dethm.memb?/remb dethm.memb?/remb)
        (list 'defun.ctx? defun.ctx?) (list 'dethm.ctx?/sub dethm.ctx?/sub)
        (list 'defun.member? defun.member?) (list 'defun.set? defun.set?)
        (list 'defun.add-atoms defun.add-atoms) (list 'defun.atoms defun.atoms)
        (list 'dethm.set?/atoms.attempt dethm.set?/atoms.attempt) (list 'dethm.set?/atoms dethm.set?/atoms)
        (list 'defun.rotate defun.rotate) (list 'dethm.rotate/cons dethm.rotate/cons)
        (list 'defun.align.attempt defun.align.attempt) (list 'defun.wt defun.wt)
        (list 'defun.align defun.align) (list 'dethm.align/align dethm.align/align)))

; names : defs -> (listof symbol), the names of a list of definitions.
(define (names defs) (map def.name defs))

; the proof of (equal 't 'nil) through a defun named after the primitive atom: the claim 't becomes
; (atom (cons 'a 'b)) by the definition, then 'nil by the axiom atom/cons.
(define shadow-atom
  '(((defun atom (x) 't) nil)
    ((dethm boom () (equal 't 'nil)) nil
     ((1) (atom (cons 'a 'b)))
     ((1) (atom/cons 'a 'b))
     (() (equal-same 'nil)))))

;; the proof of (equal 't 'nil) through floating-point constants: evaluation, used backwards, turns 'nil
; into (equal (+ (+ '1e16 '1.0) '1.0) (+ '1e16 (+ '1.0 '1.0))), where the host's + rounds the left side
; to 1e16, and associate-+ makes the two sides the same.
(define float-boom
  '(((dethm boom () (equal 't 'nil)) nil
     ((2) (equal '1e16 '10000000000000002.0))
     ((2 1) (+ '1e16 '1.0))
     ((2 1 1) (+ '1e16 '1.0))
     ((2 2) (+ '1e16 '2.0))
     ((2 2 2) (+ '1.0 '1.0))
     ((2 1) (associate-+ '1e16 '1.0 '1.0))
     ((2) (equal-same (+ '1e16 (+ '1.0 '1.0))))
     (() (equal-same 't)))))

;;; ==================== TESTS ====================

(define-suite j-bob-suite

  ((doc r) `((p "The port (aux j-bob) of J-Bob, the proof assistant of " (i "The Little Prover") ".")))

  ((test/lang-total _)
   ; cf. j-bob-lang.scm:6-18, car, cdr, + and < are total and answer the symbols t and nil.
   (⊦= '() (jb:car 'a))
   (⊦= '() (jb:cdr '()))
   (⊦= 1 (jb:+ 'a 1))
   (⊦= 't (jb:< 'a 1))
   (⊦= 'nil (jb:natp -1))
   (⊦= 't (jb:natp 0))
   (⊦= 'nil (jb:atom '(a)))
   (⊦= 't (jb:equal '(1 2) '(1 2)))
   (⊦= 3 (jb:size '((a) b))))

  ((test/lang-if _)
   ; cf. j-bob-lang.scm:20-23, only the symbol nil is false, and only one arm is evaluated.
   (⊦= 2 (jb:if 'nil 1 2))
   (⊦= 1 (jb:if '() 1 2))
   (⊦= 1 (jb:if #f 1 2))
   (⊦= 'ok (jb:if 't 'ok (error "the E arm was evaluated"))))

  ((test/lang-dethm _)
   ; cf. j-bob-lang.scm:30-33, a dethm is a procedure, so a theorem can be tried on values.
   (jb:dethm car-of-cons (x) (jb:equal (jb:car (cons x '())) x))
   (⊦= 't (car-of-cons 5))
   (⊦= 't (car-of-cons '(a))))

  ((test/transcript _)
   ; every function of little-prover.scm answers what it answers upstream
   (let1 (ported (map (λ (entry) (list (car entry) ((cadr entry)))) transcript))
     (⊦= (golden) ported)))

  ((test/book _)
   ; the whole book: 21 axioms, the two induction functions of the prelude and 37 definitions and theorems
   (⊦= 21 (length (axioms)))
   (⊦= '(list-induction star-induction) (list-tail (names (prelude)) 21))
   (⊦= 60 (length (dethm.align/align)))
   (⊦= 'align/align (def.name (car (reverse (dethm.align/align))))))

  ((test/step _)
   ; cf. j-bob.scm:775-779 and chapter1.example1 of little-prover.scm
   (⊦= ''ham (J-Bob/step (prelude) '(car (cons 'ham '(eggs)))
                         '(((1) (cons 'ham '(eggs))) (() (car '(ham eggs))))))
   ; a step that does not apply ends the rewriting, and the steps after it are not tried
   (⊦= '(equal a a) (J-Bob/step (prelude) '(equal (car (cons a b)) a)
                                '(((1) (car/cons a b)) ((1) (cdr/cons a b)) (() (equal-same a)))))
   ; an equation rewrites either way: its right side becomes its left side
   (⊦= '(equal (car (cons a b)) a) (J-Bob/step (prelude) '(equal a a) '(((1) (car/cons a b)))))
   ; an ill-formed step answers the claim unchanged
   (⊦= '(equal (car (cons a b)) a) (J-Bob/step (prelude) '(equal (car (cons a b)) a)
                                               '(((1) (car/cons a b)) (() (equal-sam a))))))

  ((test/prove _)
   ; cf. j-bob.scm:781-789
   (let1 (thm '((dethm car-pair (a b) (equal (car (cons a b)) a)) nil
                ((1) (car/cons a b))
                (() (equal-same a))))
     ; a proof that ends in 't
     (⊦= ''t (J-Bob/prove (prelude) (list thm)))
     ; one that stops early answers the claim left
     (⊦= '(equal a a) (J-Bob/prove (prelude) (list (list (car thm) 'nil (caddr thm)))))
     ; an ill-formed one answers 'nil
     (⊦= ''nil (J-Bob/prove (prelude) '(((dethm car-pair (a b) (equal (car (cons a b)) c)) nil))))))

  ((test/define _)
   ; cf. j-bob.scm:791-796, definitions are added up to the first proof that fails
   (let1 (pfs '(((defun len (x) (if (atom x) '0 (+ '1 (len (cdr x)))))
                 (size x)
                 ((A E) (size/cdr x))
                 ((A) (if-same (atom x) 't))
                 ((Q) (natp/size x))
                 (() (if-true 't 'nil)))
                ((defun loop (x) (loop x)) (size x))))
     (⊦= '(len) (list-tail (names (J-Bob/define (prelude) pfs)) 23))))

  ((test/totality _)
   ; a function that does not terminate has no measure that proves it total, so it is not defined
   (⊦= 23 (length (J-Bob/define (prelude) '(((defun f (x) (+ '1 (f x))) (size x))))))
   (⊦= 23 (length (J-Bob/define (prelude) '(((defun f (x) (+ '1 (f x))) nil))))))

  ((test/hole-shadow _)
   ; upstream's hole, kept: undefined? (j-bob.scm:138) does not reserve the names of the primitives, so a
   ; defun named atom is admitted and proves (equal 't 'nil)
   (⊦= ''t (J-Bob/prove (prelude) shadow-atom))
   (⊦= '(atom boom) (list-tail (names (J-Bob/define (prelude) shadow-atom)) 23)))

  ((test/hole-float _)
   ; upstream's hole, kept: eval-op (j-bob.scm:652) gives quoted floats to the host's +, which is not
   ; associative on them, while the axiom associate-+ says it is
   (⊦= ''t (J-Bob/prove (prelude) float-boom))
   (⊦= '(boom) (list-tail (names (J-Bob/define (prelude) float-boom)) 23))))

(unittest/✓ j-bob-suite)
