;;; (aux pie) -- Pie programs, read from a file, elaborated and reported as `racket FILE.pie` does.
;;;
;;; A port of main.rkt of Pie, the language of "The Little Typer" by Daniel P. Friedman and David Thrane
;;; Christiansen (https://github.com/the-little-typer/pie, commit 2c89553). Pie is distributed under the
;;; GNU AGPL-3.0, and so is this file, which is derived from it.
;;;
;;; Upstream, main.rkt is the module language `pie`. Its reader (the submodule `reader` of main.rkt:250) reads
;;; the body of a `#lang pie` file as syntax objects, and its #%module-begin (main.rkt:119) elaborates every
;;; form while Racket expands the module. Expressions are printed during that expansion, the first error ends
;;; it, and the module that results only stores the final context in the-pie-ctx and prints the TODO reports.
;;; The REPL that Racket opens after a file evaluates expressions with #%top-interaction (main.rkt:229).
;;;
;;; The port has no phases. Each upstream piece becomes a procedure that does at run time what the macro
;;; does at expansion time, in the same order.
;;;   - pie-read-module reads a file as the module reader does, the whole body before anything else.
;;;   - pie-module-begin is the elaboration loop of main.rkt:119-205 together with the module body that it
;;;     expands into, so it prints the expressions as it reaches them. Then it expands that body, the
;;;     (pie-decl->binders form) of each form, as far as Racket's expander can fail on it, and only then
;;;     prints the TODO reports and stores the context in the-pie-ctx.
;;;   - pie-top-interaction is main.rkt:229-248 on one syntax object.
;;;   - pie-run-file and pie-main are what `racket FILE.pie` adds around them, the load handler and the
;;;     error display; they have no upstream definition.
;;;
;;; The info hook of main.rkt:71 is installed once and globally upstream, at phase 1, so it sees only the
;;; elaboration of the module and never the REPL, which runs at phase 0 with the default hook. Here
;;; pie-module-begin parameterizes pie-info-hook for its run, which keeps both facts. The hook collects the
;;; TODO holes in a list local to the run, in place of the box `holes` of main.rkt:61. Everything else it
;;; does upstream builds DrRacket tooltips, which never print and never fail on a located term, so it is
;;; dropped.
;;;
;;; Not ported, being DrRacket or Racket expansion machinery that prints nothing on `racket FILE.pie`, are
;;; the tooltips of the hook, a-or-an and log-all-tooltips! (main.rkt:62-112, 126, 160), the prefab structs
;;; todo-item and command and the syntax properties 'todo and 'editing-command that carry them
;;; (main.rkt:115-117, 190-199), pie-top and pie-datum (main.rkt:208-225, the #%top and #%datum of the
;;; expansion, which only quote, so the port only knows when a user's #%datum hides pie-datum), the go-on of
;;; phase 1 (main.rkt:49-57, unused), the reader's #:info (main.rkt:252-344, file filters, indentation and
;;; coloring), and the provides of main.rkt:24-47 beyond the-pie-ctx and norm (the Pie keywords of
;;; pie-info.rkt and Racket's list and match, which the module language passes on).
;;;
;;; ==================== WHAT `racket FILE.pie` PRINTS ====================
;;; pie-run-file reproduces stdout byte for byte, stderr without the "  context...:" block of Racket's error
;;; display (a stack trace with installation paths), and the exit status, 0 or 1 after an error.
;;;   - Each expression prints its normal form and a newline as it is reached; claims, definitions and
;;;     sameness checks print nothing.
;;;   - The first error ends the run. A Pie error, a syntax error of the parser, of the module expansion or
;;;     a read error, and a contract error of Racket's are printed by display-racket-exn of (aux pie racket),
;;;     which follows Racket's error display handler.
;;;   - The module expansion comes after the elaboration, so its errors come after every printed expression.
;;;   - The TODO reports come after the last form, in the order the elaboration met the holes, duplicates
;;;     kept, and only when no form failed and the module expanded.
;;;   - A read error anywhere comes before everything, since the whole body is read first.
;;; The default load handler of Racket accepts a file without `#lang` only when it holds one `module`
;;; form. Its errors for the other cases are reproduced, for a missing file, a directory, a file whose first
;;; datum is no module form, and a file with no datum at all. A `#lang` other than pie and a `module` form are
;;; not supported and raise a CHICKEN error.

(module (aux pie)
  ( ; the context of the last file
    the-pie-ctx
    ; reading and elaborating a module
    pie-read-module pie-module-begin
    ; the REPL
    pie-top-interaction
    ; running a file as `racket FILE.pie`
    pie-run-file pie-main
    ; passed on from (aux pie rep), cf. main.rkt:32
    norm)

  (import scheme
          (chicken base)
          (chicken port)
          (chicken file)
          (chicken pathname)
          (chicken process-context)
          (only (chicken string) substring-index)
          (chicken condition)
          (only (chicken io) read-bytevector)
          (only (scheme base) bytevector)
          (only srfi-1 any)
          (aux base)
          (aux pie racket)
          (aux pie syntax)
          (aux pie syntax-parse)
          (aux pie locations)
          (aux pie basics)
          (aux pie normalize)
          (aux pie typechecker)
          (aux pie rep)
          (aux pie parser)
          (aux pie serialization)
          (aux pie resugar)
          (aux pie pretty)
          (aux pie pie-err)
          (aux pie show-goal))

  ;;; ==================== THE CONTEXT ====================

  ;; the-pie-ctx : (Boxof Ctx), the context of the last file that elaborated without error, which the REPL
  ;; uses; the initial context until then, cf. main.rkt:227
  (define the-pie-ctx (box init-ctx))

  ;;; ==================== READING ====================

  ;; pie-read-module : Path-String -> (Listof Syntax), the forms of the body of the `#lang pie` file at path,
  ;; read as the module reader of main.rkt:250 reads them, every one before any is elaborated. The source of
  ;; the syntax objects is the complete path of the file, as Racket's module name resolver gives it. A file
  ;; that cannot be opened, or that has no `#lang` line, raises what Racket's default load handler raises.
  ;; The bytes of the file are decoded as Racket's file port decodes them for read-syntax, so each byte that
  ;; is not part of valid UTF-8 becomes #\uFFFD, in a name too. An absolute path is taken as it is, and a
  ;; relative one is completed with racket-current-directory.
  (define (pie-read-module path)
    (let1 (source (normalize-pathname (if (absolute-pathname? path)
                                        path
                                        (make-absolute-pathname (racket-current-directory) path))))
      (cond
        ((not (file-exists? source)) (cannot-open-module-file source "no such file or directory; rkt_err=3"))
        ((directory-exists? source) (cannot-open-module-file source "path refers to a directory; rkt_err=9"))
        (else
          (let* ((text (let1 (bytes (call-with-input-file source (λ (port) (read-bytevector #f port)) #:binary))
                         (racket-bytes->string/utf-8 (if (eof-object? bytes) (bytevector) bytes) #\xFFFD)))
                 (sp (open-syntax-input source text)))
            (match/first (read-language sp)
              (pie (let loop ((forms '()))
                     (let1 (form (read-syntax sp))
                       (if (eof-object? form) (reverse forms) (loop (cons form forms))))))
              (#f (no-module-declaration sp source))
              (,lang (error 'pie-read-module "only `#lang pie` is supported, not" lang))))))))

  ;; The error of open-input-file in Racket's load handler, for a file that does not exist or is a directory.
  (define (cannot-open-module-file source system-error)
    (raise-racket-exn 'exn:fail:filesystem
                      (string-append "open-input-file: cannot open module file\n"
                                     "  module path: #<path:" source ">\n"
                                     "  path: " source "\n"
                                     "  system error: " system-error)
                      '()))

  ;; Racket's default load handler on a file without `#lang`, which must hold one `module` form. It reads the
  ;; first datum, so a read error comes first; then end of file and any datum other than a module form are
  ;; errors with no srclocs. A module form is not supported by the port.
  (define (no-module-declaration sp source)
    (let1 (form (read-syntax sp))
      (cond
        ((eof-object? form)
          (raise-racket-exn 'exn:fail
                            (string-append "default-load-handler: expected a `module' declaration;\n"
                                           " found end-of-file\n"
                                           "  in: #<path:" source ">")
                            '()))
        ((match/first (syntax->datum form) ((module . _) #t) (_ #f))
          (error 'pie-read-module "a `module` form without `#lang` is not supported" source))
        (else
          (raise-racket-exn 'exn:fail
                            (string-append "default-load-handler: expected a `module' declaration, but found something else\n"
                                           "  file: " source)
                            '())))))

  ;;; ==================== ELABORATING A MODULE ====================

  ;; pie-module-begin : (Listof Syntax) -> Ctx, elaborates the forms of a module one after the other as the
  ;; #%module-begin of main.rkt:119 does while Racket expands the module, then expands the module it
  ;; makes and does what that module does when it runs (main.rkt:201-205). It answers the context of the
  ;; last form.
  ;;   - A claim, a definition and a sameness check extend or test the context and print nothing; an
  ;;     expression prints its normal form, resugared, and a newline, cf. main.rkt:133-159.
  ;;   - The first stop raises its Pie error, so nothing more is printed.
  ;;   - At the end the context is read back and dumped and the TODO reports are made, cf.
  ;;     main.rkt:161-199. Then the module body is expanded, which can raise a syntax error, cf.
  ;;     main.rkt:201-203. Last the dumped context is restored into the-pie-ctx and the reports are printed
  ;;     in the order the holes were met, cf. main.rkt:204-205.
  (define (pie-module-begin forms)
    (let* ((holes '())
           (Γ (parameterize ((pie-info-hook (hook (λ (hole) (push! hole holes)))))
                (let loop ((forms forms)
                           (Γ init-ctx))
                  (cond
                    ((null? forms)
                      Γ)
                    (else
                      (match/first (parse-pie-decl (car forms))
                        ((claim ,f ,f-loc ,ty)
                          (match/first (add-claim Γ f f-loc ty)
                            (#(go ,new-Γ)
                              (loop (cdr forms) new-Γ))
                            (#(stop ,where ,why)
                              (raise-pie-error where why))))
                        ((definition ,f ,f-loc ,e)
                          (match/first (add-def Γ f f-loc e)
                            (#(go ,new-Γ)
                              (loop (cdr forms) new-Γ))
                            (#(stop ,where ,why)
                              (raise-pie-error where why))))
                        ((check-same ,loc ,ty ,e1 ,e2)
                          (match/first (check-same Γ loc ty e1 e2)
                            (#(go _)
                              (loop (cdr forms) Γ))
                            (#(stop ,where ,why)
                              (raise-pie-error where why))))
                        ((expression ,e)
                          (match/first (norm Γ e)
                            (#(go ,out)
                              (begin (pprint-pie (resugar out))
                                     (display "\n")
                                     (loop (cdr forms) Γ)))
                            (#(stop ,where ,why)
                              (raise-pie-error where why))))))))))
           ; the context is read back and dumped before any report is made, cf. main.rkt:161
           (ctx-string (dump (read-back-ctx Γ)))
           ; the reports of the holes in the order they were met, cf. main.rkt:163-199
           (hole-output-strs (map (λ (info) (match1/first ((,loc ,Γ ,t) info) (hole-output-str loc Γ t)))
                                  (reverse holes))))
      ; Racket expands the module that the macro answers, with a (pie-decl->binders form) for each form,
      ; cf. main.rkt:201-203
      (expand-module-body (map (λ (form) `(pie-decl->binders ,form)) forms))
      ; what the expanded module does when it runs, cf. main.rkt:204-205
      (set-box! the-pie-ctx (val-of-ctx (restore ctx-string)))
      (for-each (λ (s) (display s) (newline)) hole-output-strs)
      Γ))

  ;;; ==================== EXPANDING THE MODULE ====================
  ;;
  ;; The module of main.rkt:201-205 holds the forms (pie-decl->binders form) ..., which Racket's expander
  ;; works through in the two passes of a module body. The port expands their Forms, of (aux pie parser),
  ;; only as far as the two errors that a Pie program can reach. The rest of the expansion succeeds on
  ;; every program that elaborated, so it is not done.
  ;;   - Pass 1 expands each form to its head, which finds the module-level definitions, and a second
  ;;     definition of a name is "identifier already defined", located at the name.
  ;;   - Pass 2 expands the expressions, the right-hand sides of the definitions too, in order. A literal
  ;;     number is wrapped in the #%datum of its context, the module language's pie-datum, unless the
  ;;     user's name #%datum is bound there, by a module-level definition or by a let*. Then it is a
  ;;     variable, and Racket says "literal data is not allowed".
  ;; The run part of the module never starts after such an error, so there are no TODO reports either.
  ;; Every name that a let* or a module-level definition binds is a user's identifier, and the
  ;; templates' symbols are never bound, so the bound names are kept as a list of symbols.
  ;;
  ;; Not done, because a Pie program that elaborates never meets them, are the #%top of an identifier
  ;; without a binding (every Pie variable has its let* or its definition) and the #%app of a user's
  ;; parentheses (the templates rebuild every list). The run part itself only evaluates (void ...) and
  ;; variables defined before their uses, which never fails.

  ;; expand-module-body : (Listof Form) -> Void, the expansion of the body of the module of main.rkt:201
  ;; by racket/base's #%module-begin, which raises the first error of pass 1, else the first of pass 2
  (define (expand-module-body forms)
    (let loop ((forms forms)
               (expanded '())
               (defined '()))
      (cond
        ((null? forms)
          ; pass 2, with every module-level name bound
          (for-each (λ (form)
                      (match/first form
                        ((define-values (,x) ,rhs) (expand-expression rhs defined))
                        (,e (expand-expression e defined))))
                    (reverse expanded)))
        (else
          ; pass 1
          (match/first (expand-to-head (car forms))
            ((define ,x ,rhs)
              (let1 (form `(define-values (,x) ,rhs))
                (when (memq (syntax-e x) defined)
                  (raise-syntax-error 'module "identifier already defined" (datum->syntax #f form) x))
                (loop (cdr forms) (cons form expanded) (cons (syntax-e x) defined))))
            (,e
              (loop (cdr forms) (cons e expanded) defined)))))))

  ;; expand-to-head : Form -> Form, a form after the uses of pie-decl->binders and pie->binders at its head
  ;; have been expanded
  (define (expand-to-head form)
    (match/first form
      ((pie-decl->binders ,stx) (expand-to-head (pie-decl->binders stx)))
      ((pie->binders ,stx) (expand-to-head (pie->binders stx)))
      (_ form)))

  ;; expand-expression : Form (Listof Symbol) -> Void, the expansion of an expression where the user's
  ;; names bound are those of the list, from left to right as Racket expands the subexpressions
  (define (expand-expression form bound)
    (match/first (expand-to-head form)
      ((quote _)
        (void))
      ((let* ,bindings ,body)
        (expand-expression body
                           (foldl (λ (bound binding)
                                    (match1/first ((,x ,rhs) binding)
                                      (expand-expression rhs bound)
                                      (cons (syntax-e x) bound)))
                                  bound
                                  bindings)))
      ((,e . ,es)
        (for-each (λ (e) (expand-expression e bound)) (cons e es)))
      (,e
        (when (and (syntax? e) (number? (syntax-e e)) (memq '|#%datum| bound))
          ; the #%datum of the literal is the user's variable, cf. parser.rkt:641-642
          (raise-syntax-error #f "literal data is not allowed;\n no #%datum syntax transformer is bound" #f e)))))

  ;; hook : ((List Syntax Serializable-Ctx Core) -> Void) -> (Loc Info -> Void), the info hook of a run, which
  ;; gives push-hole the term, the context and the type of each TODO, cf. main.rkt:71. Upstream it also
  ;; computes the start and end of the term and builds a tooltip for every kind of information; the tooltips
  ;; are dropped, but start and end are kept, since they fail as upstream for a term without a position.
  (define (hook push-hole)
    (λ (loc what)
      (when (and (not (eqv? loc #f)))
        (let* ((loc-stx (location->syntax loc))
               (start (sub1 (syntax-position loc-stx)))
               (end (sub1 (+ (syntax-position loc-stx)
                             (syntax-span loc-stx)))))
          (match/first what
            ((TODO ,Γ ,t)
              (push-hole (list loc-stx Γ t)))
            (_ (void)))
          (void)))))

  ;; hole-output-str : Syntax Serializable-Ctx Core -> String, the report of a TODO at loc, of type t in
  ;; the context Γ, cf. main.rkt:170. With free local variables in Γ it is the inference rule of
  ;; goal->strings, without them only the type, on its own lines when it takes more than one.
  (define (hole-output-str loc Γ t)
    (match1/first ((,hole-summary ,hole-details) (goal->strings loc Γ t))
      (string-append
        (source-location->prefix* loc)
        ; whether there are free local variables
        (if (any (λ (H)
                   (match/first H
                     ((_ (free _)) #t)
                     (_ #f)))
                 Γ)
          ; if there are free variables, the version with the horizontal line
          (string-append "TODO:\n"
                         hole-details)
          ; if there are none, only the goal type
          (string-append "TODO: "
                         (if (substring-index "\n" hole-summary)
                           (string-append "\n"
                                          (indent-string 1 hole-summary))
                           hole-summary)))
        "\n")))

  ;; syntax/srcloc's source-location->prefix on a syntax object, which gives "" for a syntax without a
  ;; location, cf. main.rkt:172
  (define (source-location->prefix* stx)
    (let1 (srcloc (syntax-srcloc stx))
      (if srcloc (source-location->prefix srcloc) "")))

  ;;; ==================== THE REPL ====================

  ;; pie-top-interaction : Syntax -> Void, one interaction of the REPL that Racket opens after a file, against
  ;; the context of that file, cf. main.rkt:229. An expression prints its normal form, with no newline, and a
  ;; sameness check prints nothing; a claim or a definition is a syntax error.
  (define (pie-top-interaction e)
    (match/first (parse-pie-decl e)
      ((expression ,expr)
        (let* ((Γ (unbox the-pie-ctx))
               (n (norm Γ expr)))
          (match/first n
            (#(go ,out)
              (pprint-pie (resugar out)))
            (#(stop ,where ,what)
              (raise-pie-error where what)))))
      ((check-same ,loc ,ty ,e1 ,e2)
        (match/first (check-same (unbox the-pie-ctx) loc ty e1 e2)
          (#(go _)
            (void))
          (#(stop ,where ,why)
            (raise-pie-error where why))))
      (_ (raise-syntax-error #f "The Pie REPL does not support adding new definitions. Please load a file containing Pie code." e))))

  ;;; ==================== RUNNING A FILE ====================

  ;; pie-run-file : Path-String -> (U 0 1), what `racket FILE.pie` does with the file at path. It reads and
  ;; elaborates the file, printing to the current output port, and answers 0; after an error of Racket's,
  ;; which it prints to the current error port as Racket's error display does, it answers 1. Any other
  ;; condition, a bug, is raised again. It never exits, so tests can call it.
  (define (pie-run-file path)
    (handle-exceptions e
        (if (racket-exn? e)
          (begin (display-racket-exn e) 1)
          (abort e))
      (pie-module-begin (pie-read-module path))
      0))

  ;; pie-main : (Listof String) -> Nothing, the command line `pie FILE.pie`, which exits with the status of
  ;; pie-run-file. Without a file Racket would open its REPL; the port prints how to use it and exits with 2.
  (define (pie-main args)
    (if (null? args)
      (begin
        (display "usage: pie FILE.pie\n" (current-error-port))
        (exit 2))
      (exit (pie-run-file (car args))))))
