
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

  ; runs thunk, recording a condition it raises as a failure of testcase-name, then answers witness
  (define (unittest/testcase-guard result testcase-name witness thunk)
    (condition-case (thunk)
      (c (exn unittest-assert-equal) 
         (unittest/result-failed!
           result (cons testcase-name (get-condition-property c 'unittest-assert-equal 'comparison)))
         witness)
      (c (exn)
         (unittest/result-failed!
           result (list testcase-name (call-with-output-string
                                        (lambda (port) (print-error-message c port)))))
         witness)
      (c () 
         (unittest/result-failed! result (list testcase-name c))
         witness)))

  (define (unittest/testcase-run testcase result sut)
    (let* ((methods (cdr sut))
           (setup (alist-ref 'setup methods))
           (teardown (alist-ref 'teardown methods))
           (testcase-name (unittest/testcase-name testcase))
           (witness (gensym))
           (guarded (lambda (thunk) (unittest/testcase-guard result testcase-name witness thunk))))
      (unittest/result-started! result)
      ; setup and teardown raise as a failure of the case too: a raising setup skips the body and
      ; the teardown, a raising teardown fails a case whose body passed.  A case fails once: the
      ; condition of a teardown raising after a failed body goes to the case's stderr instead.
      (let ((args (if setup 
                    (guarded (lambda () (call-with-values (lambda () ((car setup) testcase)) list)))
                    '())))
        (let-values (((f code) (apply values (alist-ref testcase-name methods))))
          (let* ((no-outsrt "")
                 (eta-time 0)
                 (pair (if (eq? args witness)
                         witness
                         (guarded (lambda ()
                                    (let* ((res (void))
                                           (outstr no-outsrt)
                                           (errstr (with-error-output-to-string
                                                     (lambda ()
                                                       (set! outstr (with-output-to-string
                                                                      (lambda () 
                                                                        (let-values (((u s) (cpu-time)))
                                                                          (set! res (apply f testcase args))
                                                                          (let-values (((uu ss) (cpu-time)))
                                                                            (set! eta-time (- (+ uu ss) (+ u s))))))))))))
                                      (list res outstr errstr))))))
                 (pair (if (eq? pair witness) (list witness no-outsrt no-outsrt) pair))
                 (v (first pair))
                 (outstr (second pair))
                 (errstr (third pair))
                 (hasdoc (and (pair? v) (eq? (car v) 'doc)))
                 (torn (cond
                         ((or (not teardown) (eq? args witness)) (void))
                         ((eq? v witness)
                          (condition-case (apply (car teardown) testcase args)
                            (c ()
                               (set! errstr (string-append 
                                              errstr
                                              (call-with-output-string
                                                (lambda (port) (print-error-message c port)))))
                               witness)))
                         (else (guarded (lambda () (apply (car teardown) testcase args)))))))
            (let ((header-id (symbol->string testcase-name)))
              `((h2 (@ (id ,header-id))
                  (a (@ (href ,(string-append "#" header-id)))
                    (code ,header-id)
                    ": " 
                    ,(if (or (eq? v witness) (eq? torn witness))
                        '(span (@ (class "w3-text-red")) fail) 
                        '(span (@ (class "w3-text-green")) pass))))
                ,@(if hasdoc (cdr v) '())
                (code/scheme ,(if hasdoc (butlast code) code))
                (code/scheme ((eta ,(exact->inexact (/ eta-time 1000)))
                              (memory ,(memory-statistics))
                              (stdout ,outstr)
                              (stderr ,errstr))))))))))

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

  (define (⊦ pred? . args) (unless (apply pred? args) (signal (apply unittest/condition-expected-actual args))))
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
                                      `(#;(p (b "Tests summary")
                                           (code/scheme ,res))
                                        ,@(if sxml ((car sxml) r) '())
                                        #;(structure/section "Tests")
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


































