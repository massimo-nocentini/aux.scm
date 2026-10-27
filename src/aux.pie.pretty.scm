;;; (aux pie pretty) -- Pie's own printer for expressions and error messages.
;;;
;;; A port of pretty.rkt of Pie, the language of "The Little Typer" by Daniel P. Friedman and David Thrane
;;; Christiansen (https://github.com/the-little-typer/pie, commit 2c89553). Pie is distributed under the
;;; GNU AGPL-3.0, and so is this file, which is derived from it.
;;;
;;; The file is ported whole and line for line, with the upstream names. Upstream provides pprint-pie,
;;; pprint-message and the indented macro (cf. pretty.rkt:8), and so does this module. pprint-claim is
;;; defined and unused, as upstream.
;;;
;;; This is not racket/pretty. There is no line width, and every line break comes from the shape of the
;;; expression. Every rule is relative to the value of the indentation parameter when a form starts to
;;; print, which is not always the column where the form starts. The misalignments that follow from it, the
;;; first argument of a non-atomic → and the type of a Π binding, are upstream's and are kept.
;;;
;;; print-pie never fails. On any error it displays the whole expression after whatever it has already
;;; printed (cf. pretty.rkt:101). Racket's with-handlers runs its handler outside the parameterizations of
;;; the body, and so does CHICKEN's handle-exceptions, which is used here. Racket catches only exn:fail
;;; values, and the handler here only conditions of kind exn, which every CHICKEN error is (match failures,
;;; split-at, length and reverse of improper lists, racket-printf), while an interrupt is not.
;;;
;;; Racket's match becomes match/first of (aux base) clause by clause, in the same order. (? pred) and
;;; #:when become ⊣ guards, (app f p) a let in the body, (or 'a 'b) a guard on the head. The two
;;; (printf string) calls of pretty.rkt:85 and :107 print a string as a format string, so they become
;;; racket-printf, which reproduces Racket's treatment of ~ in a name (oracle printf-tilde). Racket's
;;; display and CHICKEN's display print Pie data alike.

(module (aux pie pretty)
  ( ; printing expressions
    pprint-pie
    ; printing messages
    pprint-message
    ; the indentation; indentation is exported with the macro, hidden from importers
    (indented indentation))

  (import scheme
          (chicken base)
          (chicken format)
          (only (chicken condition) handle-exceptions condition-predicate abort)
          (only (scheme base) make-parameter)
          (only srfi-1 split-at every)
          (aux base)
          (aux pie racket)
          (aux pie resugar))

  ;;; ==================== INDENTATION ====================

  ;; indentation : (Parameterof Natural), the column where a new line starts, cf. pretty.rkt:10
  (define indentation (make-parameter 0))

  ;; (indented i e ...) runs e ... with the indentation increased by i, cf. pretty.rkt:12
  (define-syntax indented
    (syntax-rules ()
      ((_ i e ...)
       (parameterize ((indentation (+ i (indentation))))
         (begin e ...)))))

  ;; spaces : Natural -> Void, cf. pretty.rkt:18
  (define (spaces n)
    (do ((i 0 (add1 i)))
        ((>= i n))
      (display " ")))

  ;; space : -> Void, cf. pretty.rkt:22
  (define (space) (spaces 1))

  ;; indent : -> Void, cf. pretty.rkt:24
  (define (indent)
    (spaces (indentation)))

  ;; start-line : -> Void, cf. pretty.rkt:27
  (define (start-line)
    (indent))

  ;; terpri : -> Void, a new line under the current indentation, cf. pretty.rkt:30
  (define (terpri)
    (display "\n")
    (start-line))

  ;; l : -> Void, cf. pretty.rkt:34
  (define (l)
    (display "("))

  ;; r : -> Void, cf. pretty.rkt:37
  (define (r)
    (display ")"))

  ;; (parens e ...) prints e ... between parentheses, one column further in, cf. pretty.rkt:40
  (define-syntax parens
    (syntax-rules ()
      ((_ e ...)
       (begin (l)
              (indented 1 e ...)
              (r)))))

  ;;; ==================== CLASSIFYING HEADS ====================

  ;; top-binder? : Any -> Boolean, cf. pretty.rkt:47
  (define (top-binder? sym)
    (and (symbol? sym)
         (or (eqv? sym '?)
             (eqv? sym 'Pi)
             (eqv? sym 'Π)
             (eqv? sym 'Sigma)
             (eqv? sym 'Σ))))

  ;; ind? : Any -> Boolean, a symbol whose name starts with ind-, cf. pretty.rkt:55
  (define (ind? sym)
    (and (symbol? sym)
         (let1 (str (symbol->string sym))
           (and (>= (string-length str) 4)
                (string=? (substring str 0 4) "ind-")))))

  ;; simple-elim? : Any -> Boolean, which-Nat, iter-Nat or a symbol whose name starts with rec-,
  ;; cf. pretty.rkt:61
  (define (simple-elim? sym)
    (and (symbol? sym)
         (or (eqv? sym 'which-Nat)
             (eqv? sym 'iter-Nat)
             (let1 (str (symbol->string sym))
               (and (>= (string-length str) 4)
                    (string=? (substring str 0 4) "rec-"))))))

  ;;; ==================== SEPARATORS ====================

  ;; sep : (-> Void) (Any -> Void) (Listof Any) -> Void, op on each element with s between them; an
  ;; improper list raises after the proper part is printed, cf. pretty.rkt:69
  (define (sep s op args)
    (match/first args
      (() (void))
      ((,b) (op b))
      ((,b . ,bs) (op b) (s) (sep s op bs))))

  ;; vsep : (Any -> Void) (Listof Any) -> Void, one element per line, cf. pretty.rkt:75
  (define (vsep op args)
    (sep terpri op args))

  ;; hsep : (Any -> Void) (Listof Any) -> Void, elements separated by a space, cf. pretty.rkt:77
  (define (hsep op args)
    (sep space op args))

  ;; annots : (Listof (List Symbol Any)) -> Void, the bindings of a Π, Σ or ?, one per line, the type of
  ;; each one column right of where it starts; a binding of another shape raises, cf. pretty.rkt:80
  (define (annots bindings)
    (define (print-binding b)
      (match/first b
        ((,name ,ty)
         (let1 (x (symbol->string name))
           (parens
            (racket-printf x)
            (space)
            (indented (+ (string-length x) 2)
                      (print-pie ty)))))))
    (vsep print-binding bindings))

  ;; atomic? : Any -> Boolean, a symbol, a number or a quotation, cf. pretty.rkt:91
  (define (atomic? x)
    (match/first x
      ((,s ⊣ (symbol? s)) #t)
      ((,n ⊣ (number? n)) #t)
      ((quote _) #t)
      (_ #f)))

  ;;; ==================== PRINTING EXPRESSIONS ====================

  ;; exn:fail? : Any -> Boolean, the conditions that the handler of print-pie catches, cf. pretty.rkt:101
  (define exn:fail? (condition-predicate 'exn))

  ;; print-pie : Any -> Void, prints a high-level Pie expression; the clauses are those of upstream in
  ;; upstream order, cf. pretty.rkt:98
  (define (print-pie expr)
    ;; Always display something, even if the print code is borked, cf. pretty.rkt:100
    (handle-exceptions e (if (exn:fail? e) (display expr) (abort e))
      (match/first expr
        ; Π, Σ and ? with their bindings, cf. pretty.rkt:104
        (((,binder ,bindings ,body) ⊣ (top-binder? binder))
         (let1 (binder-string (symbol->string binder))
           (parens (racket-printf binder-string)
                   (spaces 1)
                   (indented (add1 (string-length binder-string))
                             (parens (annots bindings)))
                   (terpri)
                   (print-pie body))))
        ; λ with its arguments, any value in place of the arguments, cf. pretty.rkt:113
        (((,lam ,args ,body) ⊣ (or (eqv? lam 'lambda) (eqv? lam 'λ)))
         (parens
          (display 'λ) (space) (parens (hsep display args))
          (indented 1 (terpri) (print-pie body))))
        ; → with at least one type; the reverse of an improper list raises, as Racket's does in
        ; (app reverse ...), cf. pretty.rkt:117
        (((,arrow . ,types) ⊣ (and (or (eqv? arrow '->) (eqv? arrow '→)) (pair? (reverse types))))
         (let* ((reversed (reverse types))
                (ret (car reversed))
                (args (reverse (cdr reversed))))
           (parens (display "→")
                   (if (every atomic? args)
                       (begin (space)
                              (hsep display args))
                       (indented 3
                                 (space)
                                 (vsep print-pie
                                       args)))
                   (indented 1 (terpri) (print-pie ret)))))
        ; cf. pretty.rkt:127
        ((quote ,x) (display "'") (display x))
        ; the eliminators ind-X, the targets on the first line, cf. pretty.rkt:128
        (((,op . ,args) ⊣ (ind? op))
         (let* ((elim (symbol->string op))
                (target-count
                  (case op
                    ((ind-Vec trans) 2)
                    (else 1))))
           (receive (targets normal) (split-at args target-count)
             (parens
              (display elim)
              (space)
              (hsep print-pie targets)
              (match/first normal
                (() (void))
                ((,motive . ,others)
                 (indented 2 (terpri) (vsep print-pie (cons motive others)))))))))
        ; which-Nat, iter-Nat and rec-X, the target on the first line, cf. pretty.rkt:143
        (((,op . ,args) ⊣ (simple-elim? op))
         (let1 (elim (symbol->string op))
           (match1/first ((,target . ,normal) args)
             (parens
              (display elim)
              (space)
              (print-pie target)
              (match/first normal
                (() (void))
                (,others
                 (indented 2
                           (terpri)
                           (vsep print-pie others))))))))
        ; an application of a name to fewer than 21 atomic arguments, on one line; length raises on an
        ; improper list, as Racket's does in the #:when, cf. pretty.rkt:155
        (((,op ,arg . ,args) ⊣ (and (symbol? op)
                                    (atomic? arg)
                                    (< (length args) 20)
                                    (every atomic? args)))
         (parens
          (hsep print-pie
                (cons op (cons arg args)))))
        ; an application of a name, the first argument on the first line, cf. pretty.rkt:161
        (((,op ,arg . ,args) ⊣ (symbol? op))
         (parens (print-pie op) (space)
                 (indented (add1 (string-length (symbol->string op)))
                           (print-pie arg))
                 (indented 1
                           (when (pair? args)
                             (terpri)
                             (vsep print-pie
                                   args)))))
        ; any other list, the line break printed even with no arguments, cf. pretty.rkt:170
        ((,op . ,args)
         (parens (print-pie op)
                 (indented 1
                           (terpri)
                           (vsep print-pie args))))
        ; cf. pretty.rkt:175
        (,other (display other)))))

  ;; pprint-pie : Any [String] -> Void, an expression after a description, both under the current
  ;; indentation, cf. pretty.rkt:177
  (define (pprint-pie expr #!optional (description ""))
    (start-line)
    (indented (string-length description)
              (when (not (string=? description ""))
                (display description))
              (print-pie expr)))

  ;; pprint-claim : Symbol Any -> Void, unused upstream too, cf. pretty.rkt:184
  (define (pprint-claim name ty)
    (start-line)
    (parens
     (indented 1
               (display 'claim) (space) (display name) (terpri)
               (print-pie ty))))

  ;;; ==================== PRINTING MESSAGES ====================

  ;; pprint-message : Message -> Void, the strings of a message displayed, its symbols between spaces and
  ;; its Core terms resugared, each on lines of its own two columns further in, cf. pretty.rkt:191
  (define (pprint-message msg)
    (start-line)
    (for-each
      (λ (part)
        (cond ((string? part)
               (display part))
              ((symbol? part)
               (space)
               (display part)
               (space))
              (else (indented 2 (terpri) (print-pie (resugar part))) (terpri))))
      msg)))
