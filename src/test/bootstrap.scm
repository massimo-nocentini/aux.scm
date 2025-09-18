
(import (aux unittest) (chicken base) (chicken pretty-print) (chicken condition))

(define-suite wasrun-sut
  ((setup tc)         (unittest/testcase-logcons! tc 'setup))
  ((teardown tc _)    (unittest/testcase-logcons! tc 'teardown))
  ((test-method tc _) (unittest/testcase-logcons! tc 'test-method))
  ((test-unbound-variable tc _) unbound-v)
  ((test-broken tc _) (signal (unittest/condition-expected-actual 'useless '_))))

(define-suite bootstrap-sut 

  ((doc r) `((p "This test suite drove the implementation of the unittest framework itself.") 
             (code/scheme-file "../unittest.scm")))

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

  ((test-failed-result tc _ r)
   (unittest/result-started! r)
   (unittest/result-failed! r 'no-reason)
   (⊦= '((ran 1) (failed 1 no-reason)) (unittest/result-summary r)))

  ((test-suite tc _ r)
   (letsuite ((suite '(test-running test-failed)))
             (unittest/testsuite-run suite r bootstrap-sut)
             (⊦= '((ran 2) (failed 0)) (unittest/result-summary r))))

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
  )

(unittest/✓ bootstrap-sut)
#;(⊦= '((ran 8) (failed 0)) 
        (unittest/result-summary (unittest/✓ bootstrap-sut)))




















