
(module (aux unittest) *

  (import
    scheme 
    (chicken base) 
    (chicken condition) 
    (chicken pretty-print) 
    (chicken port) 
    (chicken string) 
    (chicken syntax) 
    (chicken flonum) 
    (chicken time) 
    (chicken gc) 
    srfi-1 
    srfi-19
    (aux sxml))


  (define-record unittest/testcase name log)

  (define (unittest/wasrun name) (make-unittest/testcase name '()))

  (define (unittest/testcase-logcons! testcase msg)
    (unittest/testcase-log-set! testcase (cons msg (unittest/testcase-log testcase))))

  (define (unittest/testcase-run testcase result sut)
    (let* ((methods (cdr sut))
           (setup (alist-ref 'setup methods))
           (teardown (alist-ref 'teardown methods))
           (testcase-name (unittest/testcase-name testcase)))
      (unittest/result-started! result)
      (let-values ((args (if setup ((car setup) testcase) (values)))
                   ((f code) (apply values (alist-ref testcase-name methods))))
        (let* ((witness (gensym))
               (no-outsrt "")
               (eta-time 0)
               (pair (condition-case (let* ((res (void))
                                            (outstr no-outsrt)
                                            (errstr (with-error-output-to-string
                                                      (lambda ()
                                                        (set! outstr (with-output-to-string
                                                                       (lambda () 
                                                                         (let-values (((u s) (cpu-time)))
                                                                           (set! res (apply f testcase args))
                                                                           (let-values (((uu ss) (cpu-time)))
                                                                             (set! eta-time (- (+ uu ss) (+ u s))))))))))))
                                       (list res outstr errstr))
                       (c (exn unittest-assert-equal) 
                          (begin
                            (unittest/result-failed!
                              result (cons testcase-name (get-condition-property c 'unittest-assert-equal 'comparison)))
                            (list witness no-outsrt no-outsrt)))
                       (c (exn)
                          (begin
                            (unittest/result-failed!
                              result (list testcase-name (call-with-output-string
                                                           (lambda (port) (print-error-message c port)))))
                            (list witness no-outsrt no-outsrt)))
                       (c () (begin
                               (unittest/result-failed! result (list testcase-name c))
                               (list witness no-outsrt no-outsrt)))))
               (v (first pair))
               (outstr (second pair))
               (errstr (third pair))
               (hasdoc (and (pair? v) (eq? (car v) 'doc))))
          (when teardown (apply (car teardown) testcase args))
          `((structure/section (code ,testcase-name)
                               ": " 
                               ,(if (eq? v witness) 
                                    '(span (@ (class "w3-text-red")) fail) 
                                    '(span (@ (class "w3-text-green")) pass)))
            ,@(if hasdoc (cdr v) '())
            (code/scheme ,(if hasdoc (butlast code) code))
            (code/scheme ((eta ,(exact->inexact (/ eta-time 1000)))
                          (memory ,(memory-statistics))
                          (stdout ,outstr)
                          (stderr ,errstr))))))))

  (define-syntax define-suite
    (syntax-rules ()
      ((_ sutname ((casename formal ...) body ...) ...)
       (define sutname `(sutname (casename ,(lambda (formal ...) body ...) 
                                           ,(quote (define (casename formal ...) body ...))) ...)))))

  (define-syntax lettest
    (syntax-rules ()
      ((_ ((test nameexp) ...) body ...)
       (let ((test (unittest/wasrun nameexp)) ...) body ...))))

  (define-record unittest/result ran failed)

  (define (unittest/result-summary result)
    `((ran ,(unittest/result-ran result))
      (failed ,(length (unittest/result-failed result))
              ,@(unittest/result-failed result))))

  (define (unittest/result-started! result)
    (unittest/result-ran-set! result (add1 (unittest/result-ran result))))

  (define (unittest/result-failed! result exn)
    (unittest/result-failed-set! result (cons exn (unittest/result-failed result))))

  (define (equal-approx? p) 
    (letrec ((? (lambda (a b)
                  (cond
                    ((and (real? a) (real? b)) (< (abs (- a b)) p))
                    ((and (pair? a) (pair? b)) (and (? (car a) (car b)) (? (cdr a) (cdr b))))
                    (else (equal=? a b))))))
      ?))

  (define (⊦ pred? a b) (unless (pred? a b) (signal (unittest/condition-expected-actual a b))))
  (define (⊦= a b) (⊦ (equal-approx? 0.000001) a b))
  (define (⊦≠ a b) (⊦ (complement equal?) a b))
  (define (⊨ a) (⊦= #t a))
  (define (⊭ a) (⊦= #f a))

  (define-syntax letsuite
    (syntax-rules ()
      ((_ ((name '(method ...)) ...) body ...)
       (letrec ((name (lettest ((method 'method) ...) (list method ...))) ...)
         body ...))))

  (define (unittest/testsuite-run suite r sut)
    (map (lambda (testcase) 
           (unittest/testcase-run testcase r sut)) 
      suite))

  (define (unittest/✓ sut)
    (let* ((r (make-unittest/result 0 '()))
           (sut-name (car sut))
           (sut-methods (cdr sut))
           (F (lambda (x)
                (let ((name (car x)))
                  (and (not (eq? name 'setup)) 
                       (not (eq? name 'teardown))
                       (not (eq? name 'doc))))))
           (methods (filter F sut-methods))
           (s (map (lambda (pair) (lettest ((t (car pair))) t)) methods)))
      (let ((docs (unittest/testsuite-run s r sut))
            (res (unittest/result-summary r))
            (sxml (alist-ref 'doc sut-methods)))
        (SXML->HTML->file! (sxml-tree sut-name 
                                      `((p (b "Tests summary")
                                           (code/scheme ,res))
                                        ,@(if sxml ((car sxml) r) '())
                                        ,@docs))
                           (symbol-append 'testsuite- sut-name))
        (pretty-print res)
        r)))

  (define (unittest/condition-expected-actual a b)
    (condition `(exn message "assert-equal failed") 
               `(unittest-assert-equal comparison ((expected ,a) (got ,b)))))

  (define-syntax ⊦⧳
    (syntax-rules ()
      ((_ ((exn ...) ...) body ...)
       (condition-case (begin body ...)
         ((exn ...) (void)) ...
         ))))

  )


































