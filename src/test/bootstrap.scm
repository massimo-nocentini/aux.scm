
(import (aux unittest) (chicken base) (chicken pretty-print) (chicken condition)
        (chicken file) (chicken process) (chicken process-context) (only srfi-1 last))

; the exit status of a csi process running a suite whose one case asserts `assertion`
(define (suite-exit-status assertion)
  (let* ((dir (create-temporary-directory))
         (script (string-append dir "/suite.scm")))
    (with-output-to-file script
      (lambda ()
        (display (string-append "(import (aux unittest)) (define-suite exit-sut ((t _) "
                                assertion ")) (unittest/✓ exit-sut)"))))
    (let-values (((pid normal? status)
                  (process-wait (process-run (string-append "cd " (qs dir) " && " (qs (executable-pathname))
                                                            " -s suite.scm >/dev/null 2>&1")))))
      (delete-directory dir #t)
      status)))

(define-suite wasrun-sut
  ((setup tc)         (unittest/testcase-logcons! tc 'setup))
  ((teardown tc _)    (unittest/testcase-logcons! tc 'teardown))
  ((test-method tc _) (unittest/testcase-logcons! tc 'test-method))
  ((test-unbound-variable tc _) unbound-v)
  ((test-broken tc _) (signal (unittest/condition-expected-actual 'useless '_)))
  ((test-printing-broken tc _)
   (print "printed before failing")
   (display "warned before failing" (current-error-port))
   (⊦= 1 2)))

(define-suite raising-setup-sut
  ((setup tc)         (error "setup exploded"))
  ((teardown tc)      (unittest/testcase-logcons! tc 'teardown))
  ((test-method tc)   (unittest/testcase-logcons! tc 'test-method))
  ((test-other tc)    (unittest/testcase-logcons! tc 'test-other)))

(define-suite raising-teardown-sut
  ((teardown tc)      (error "teardown exploded"))
  ((test-method tc)   (unittest/testcase-logcons! tc 'test-method))
  ((test-broken tc)   (⊦= 1 2)))

(define-suite bootstrap-sut 

  ((doc r) `((p "This test suite drove the implementation of the unittest framework itself.") 
             (code/scheme/file "../aux.unittest.scm")))

  ((setup tc) (lettest ((t 'test-method)) (values t (make-unittest/result 0 '()))))

  ((test-running tc t r)
   (⊦= '() (unittest/testcase-log t))
   (unittest/testcase-run t r wasrun-sut)
   (⊦= '(teardown test-method setup) (unittest/testcase-log t))
   `(doc (p "hello")))

  ((test-result tc t r)
   (unittest/testcase-run t r wasrun-sut)
   (⊦= '((ran 1) (failed 0)) (unittest/result-summary r)))

  ((test-failed tc _ r)
   (lettest ((t 'test-broken))
            (unittest/testcase-run t r wasrun-sut)
            (⊦= '((ran 1) (failed 1 (test-broken (expected useless) (got _)))) 
                  (unittest/result-summary r))))

  ((test-unbound-variable tc _ r)
   (lettest ((t 'test-unbound-variable))
            (unittest/testcase-run t r wasrun-sut)
            (⊦= '((ran 1) (failed 1 (test-unbound-variable "\nError: unbound variable: unbound-v\n")))
                  (unittest/result-summary r))))

  ((test-raising-setup tc _ r)
   (lettest ((t 'test-method))
            (unittest/testcase-run t r raising-setup-sut)
            (⊦= '() (unittest/testcase-log t))
            (⊦= '((ran 1) (failed 1 (test-method "\nError: setup exploded\n")))
                  (unittest/result-summary r))))

  ((test-raising-setup-suite tc _ r)
   (letsuite ((suite '(test-method test-other)))
             (unittest/testsuite-run suite r raising-setup-sut)
             (⊦= '((ran 2) (failed 2 (test-other "\nError: setup exploded\n")
                                      (test-method "\nError: setup exploded\n")))
                   (unittest/result-summary r))))

  ((test-raising-teardown-after-failure tc _ r)
   (lettest ((t 'test-broken))
            (let ((report (cadr (car (reverse (unittest/testcase-run t r raising-teardown-sut))))))
              (⊦= '("\nError: teardown exploded\n") (alist-ref 'stderr report))
              (⊦= '((ran 1) (failed 1 (test-broken (expected 1) (got 2))))
                  (unittest/result-summary r)))))

  ((test-raising-teardown tc _ r)
   (lettest ((t 'test-method))
            (unittest/testcase-run t r raising-teardown-sut)
            (⊦= '(test-method) (unittest/testcase-log t))
            (⊦= '((ran 1) (failed 1 (test-method "\nError: teardown exploded\n")))
                  (unittest/result-summary r))))

  ((test-duplicate-case-name tc _ _)
   (⊦= '("duplicate test case name" (dup-sut t1))
       (condition-case (let ()
                         (define-suite dup-sut ((t1 _) (⊦= 1 1)) ((t2 _) (⊦= 1 1)) ((t1 _) (⊦= 1 2)))
                         dup-sut)
         (c (exn) (list (get-condition-property c 'exn 'message)
                        (get-condition-property c 'exn 'arguments))))))

  ((test-exit-status tc _ _)
   (⊦= 0 (suite-exit-status "(⊦= 1 1)"))
   (⊦= 1 (suite-exit-status "(⊦= 1 2)")))

  ((test-failed-output tc _ r)
   (lettest ((t 'test-printing-broken))
            (let ((report (cadr (last (unittest/testcase-run t r wasrun-sut)))))
              (⊦= '("printed before failing\n") (alist-ref 'stdout report))
              (⊦= '("warned before failing") (alist-ref 'stderr report))
              (⊦= '((ran 1) (failed 1 (test-printing-broken (expected 1) (got 2))))
                  (unittest/result-summary r)))))

  ((test-failed-result tc _ r)
   (unittest/result-started! r)
   (unittest/result-failed! r 'no-reason)
   (⊦= '((ran 1) (failed 1 no-reason)) (unittest/result-summary r)))

  ((test-suite tc _ r)
   (letsuite ((suite '(test-running test-failed)))
             (unittest/testsuite-run suite r bootstrap-sut)
             (⊦= '((ran 2) (failed 0)) (unittest/result-summary r))))

  ((test-⊦=-infinities tc _ _)
   (⊦= +inf.0 +inf.0)
   (⊦= -inf.0 -inf.0)
   (⊦= '(1 +inf.0) (list 1.0000001 +inf.0))
   (⊭ ((equal-approx? 0.000001) +inf.0 -inf.0))
   (⊭ ((equal-approx? 0.000001) +inf.0 1e300))
   (⊭ ((equal-approx? 0.000001) +nan.0 +nan.0)))

  ((test-⊦=-vectors tc _ _)
   (⊦= (vector 1.0000000001 '(2.0000000001 #(3.0000000001))) (vector 1 '(2 #(3))))
   (⊦= #() #())
   (⊭ ((equal-approx? 0.000001) #(1 2) #(1)))
   (⊭ ((equal-approx? 0.000001) #(1.1) #(1)))
   (⊭ ((equal-approx? 0.000001) #(1) '(1))))

  ((test-⊦-arity tc _ _)
   (⊦= '(((got 4)) ((got 1 2 3 1)) ((got)))
       (map (lambda (thunk)
              (condition-case (thunk)
                (c (exn unittest-assert-equal) 
                   (get-condition-property c 'unittest-assert-equal 'comparison))))
            (list (lambda () (⊦ odd? 4)) 
                  (lambda () (⊦ < 1 2 3 1)) 
                  (lambda () (⊦ (lambda () #f)))))))

  ((test-⊨ tc _ _) (⊨ #t))

  ((test-⊭ tc _ _) (⊭ #f))

  ((test-should-signal tc _ _)
   (⊦⧳ ((exn)) (signal (condition '(exn message "useless"))))
   (⊦⧳ ((exn-a)) (signal (condition '(exn-a message "useless"))))
   (⊦⧳ ((exn) (exn-a)) (signal (condition '(exn-a message "useless"))))
   (condition-case (⊦⧳ ((exn)) (signal (condition '(exn-a message "useless"))))
     ((exn-a) (void)))
   (condition-case (⊦⧳ ((exn)) (signal 99))
     (() (void)))
   (⊦⧳ () 99))

  ((test-should-raise tc _ _)
   (⊦raises (exn) (car '()))
   (⊦raises (exn-a) (signal (condition '(exn-a message "useless"))))
   (⊦= '((expected raised) (got not-raised))
       (condition-case (⊦raises (exn) 99)
         (c (unittest-assert-equal) (get-condition-property c 'unittest-assert-equal 'comparison))))
   (⊦= 'escaped
       (condition-case (⊦raises (exn) (signal (condition '(exn-a message "useless"))))
         ((exn-a) 'escaped))))
  )

(unittest/✓ bootstrap-sut)
#;(⊦= '((ran 8) (failed 0)) 
        (unittest/result-summary (unittest/✓ bootstrap-sut)))
































