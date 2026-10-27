;;; (aux pie pie-err) -- the errors that Pie reports about a program.
;;;
;;; A port of pie-err.rkt of Pie, the language of "The Little Typer" by Daniel P. Friedman and David Thrane
;;; Christiansen (https://github.com/the-little-typer/pie, commit 2c89553). Pie is distributed under the
;;; GNU AGPL-3.0, and so is this file, which is derived from it.
;;;
;;; The file is ported whole. Upstream defines the struct exn:fail:pie, a subtype of exn:fail with one
;;; more field, where, and raise-pie-error, which renders a Message with pprint-message and raises the
;;; struct (cf. pie-err.rkt:9-30). Here the exception is the condition of kind exn:fail:pie that
;;; raise-racket-exn of (aux pie racket) makes. Its message is the rendered Message and its srclocs are
;;; the one Srcloc of where, which display-racket-exn prints under "location...:" as Racket's error display
;;; does with the prop:exn:srclocs of pie-err.rkt:10-22.
;;;
;;; That property turns a source string that names an existing file into a path, and Racket prints a path
;;; relative to the current directory. The rule lives in srcloc->string/relative of (aux pie racket), so
;;; the condition keeps the Srcloc list as it is.
;;;
;;; Upstream provides all its definitions. The port exports raise-pie-error, the predicate exn:fail:pie?
;;; and the accessor exn:fail:pie-where, the names that the rest of Pie and the tests need. The struct
;;; constructor exn:fail:pie and struct:exn:fail:pie are not exported, since nothing but raise-pie-error
;;; builds the exception. Upstream also requires resugar.rkt without using it (cf. pie-err.rkt:4); the
;;; port drops that import.

(module (aux pie pie-err) (raise-pie-error exn:fail:pie? exn:fail:pie-where)

  (import scheme
          (chicken base)
          (chicken port)
          (aux base)
          (aux pie racket)
          (aux pie locations)
          (aux pie pretty))

  ;;; ==================== THE EXCEPTION ====================

  ;; exn:fail:pie? : Any -> Boolean, whether obj is an exception raised by raise-pie-error,
  ;; cf. pie-err.rkt:9
  (define (exn:fail:pie? obj)
    (racket-exn? obj 'exn:fail:pie))

  ;; exn:fail:pie-where : exn:fail:pie -> Srcloc, the place of the error as the list
  ;; (source line column position span), cf. pie-err.rkt:9 and :12-13
  (define (exn:fail:pie-where e)
    (car (racket-exn-srclocs e)))

  ;;; ==================== RAISING ====================

  ;; raise-pie-error : Loc Message -> Nothing, raises the exception whose message is the Message as
  ;; pprint-message prints it, located at where, cf. pie-err.rkt:25
  (define (raise-pie-error where msg)
    (raise-racket-exn 'exn:fail:pie
                      (with-output-to-string
                        (τ (pprint-message msg)))
                      (list (location->srcloc where)))))
