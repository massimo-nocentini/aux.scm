;;; (aux pie rep) -- normal forms of Pie expressions, for the REPL, for main and for the tests.
;;;
;;; A port of rep.rkt of Pie, the language of "The Little Typer" by Daniel P. Friedman and David Thrane
;;; Christiansen (https://github.com/the-little-typer/pie, commit 2c89553). Pie is distributed under the
;;; GNU AGPL-3.0, and so is this file, which is derived from it.
;;;
;;; The file is ported whole and in upstream order. Upstream provides everything it defines (cf.
;;; rep.rkt:10), and so does this module. Each procedure elaborates a Src in a context with the judgments
;;; of (aux pie typechecker), evaluates the Core it gets with val-in-ctx and reads the value back into its
;;; normal form with read-back or read-back-type of (aux pie normalize).
;;;
;;; go-on of basics chains Perhaps values as upstream does, and its binding patterns are match/first
;;; patterns, so the e-out of upstream is ,e-out and `(the ,t-out ,e-out) is (the ,t-out ,e-out). The
;;; Racket matches on a Perhaps become match/first on the structs, #(go ,e) and #(stop ,where ,msg), and
;;; their catch-all _ stays _. Where upstream binds two values with let, the port uses let* so that the
;;; left to right order of Racket's let is explicit; val-in-ctx may force DELAY boxes, and the tests compare
;;; contexts down to the state of those boxes.
;;;
;;; norm runs is-type again after a failed synth, as upstream, so a TODO inside such an expression reaches
;;; the info hook twice, and main reports it twice. Typed Racket signatures are kept as comments; Typed
;;; Racket only checks them, so nothing at run time depends on them.

(module (aux pie rep) (norm-type rep norm type-or-expr check-same)

  (import scheme
          (chicken base)
          (aux base)
          (aux pie basics)
          (aux pie normalize)
          (aux pie typechecker))

  ;;; ==================== NORMAL FORMS ====================

  ;; norm-type : Ctx Src -> (Perhaps Core), the normal form of the type e, cf. rep.rkt:12
  (define (norm-type Γ e)
    (go-on ((,e-out (is-type Γ '() e)))
      (go (read-back-type Γ (val-in-ctx Γ e-out)))))

  ;; rep : Ctx Src -> (Perhaps (List 'the Core Core)), the normal forms of the type that e synthesizes and
  ;; of e itself, cf. rep.rkt:17
  (define (rep Γ e)
    (go-on (((the ,t-out ,e-out) (synth Γ '() e)))
      (let* ((tv (val-in-ctx Γ t-out))
             (v (val-in-ctx Γ e-out)))
        (go `(the ,(read-back-type Γ tv)
                  ,(read-back Γ tv v))))))

  ;; norm : Ctx Src -> (Perhaps Core), what rep answers for e and, when e synthesizes no type, the normal
  ;; form of e as a type; when both fail, the stop of the synthesis, cf. rep.rkt:25
  (define (norm Γ e)
    (match/first (go-on (((the ,t-out ,e-out) (synth Γ '() e)))
                   (let* ((tv (val-in-ctx Γ t-out))
                          (v (val-in-ctx Γ e-out)))
                     (go `(the ,(read-back-type Γ tv)
                               ,(read-back Γ tv v)))))
      (#(go ,e) (go e))
      (#(stop ,where ,msg)
       (match/first (norm-type Γ e)
         (#(go ,out) (go out))
         (_ (stop where msg))))))

  ;;; ==================== PRINTING ====================

  ;; type-or-expr : Ctx Src -> (Perhaps Ctx), displays what rep answers for e or else the normal form of e
  ;; as a type, followed by a newline, and answers the context unchanged; when both fail, the stop of
  ;; rep, cf. rep.rkt:39
  (define (type-or-expr Γ e)
    (match/first (rep Γ e)
      (#(go ,out)
       (begin (display out) (newline)
              (go Γ)))
      (#(stop ,where ,msg)
       (match/first (norm-type Γ e)
         (#(go ,out) (begin (display out) (newline) (go Γ)))
         (_ (stop where msg))))))

  ;;; ==================== SAMENESS ====================

  ;; check-same : Ctx Loc Src Src Src -> (Perhaps Void), whether a and b are the same t, as the
  ;; declaration (check-same t a b) asks, cf. rep.rkt:50
  (define (check-same Γ loc t a b)
    (go-on ((,t-out (is-type Γ '() t))
            (,tv (go (val-in-ctx Γ t-out)))
            (,a-out (check Γ '() a tv))
            (,b-out (check Γ '() b tv))
            (,av (go (val-in-ctx Γ a-out)))
            (,bv (go (val-in-ctx Γ b-out))))
      (convert Γ loc tv av bv))))
