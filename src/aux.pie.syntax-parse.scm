;;; (aux pie syntax-parse) -- the part of Racket's syntax/parse that the parser of Pie uses.
;;;
;;; Pie, the language of "The Little Typer" by Daniel P. Friedman and David Thrane Christiansen
;;; (https://github.com/the-little-typer/pie, commit 2c89553), parses its programs with syntax-parse
;;; (parser.rkt:13-21 and 235-423), and when a program is malformed it is syntax-parse that writes the
;;; error message, with "expected more terms starting with any term", "unexpected term", "expected valid Pie
;;; name", the at:, within: and in: lines and the parsing context. This module has no upstream Pie file.
;;; It gives the port a syntax-parse for the patterns that parser.rkt writes, with Racket's failure reports
;;; byte for byte, because CHICKEN has no syntax-parse. It holds no Pie code, but it belongs to the port and
;;; is distributed under the GNU AGPL-3.0 like the rest of it.
;;;
;;; It imitates Racket 8.15, collects/syntax/parse/private, whose files are cited below without that prefix
;;;   - residual.rkt          the progress frames and the expectation stacks (:155-358), and the ellipsis
;;;                           parser for plain variables (:814);
;;;   - residual-interp.rkt   the backtracking monad (:24-68) and the run-time matchers of the interpreting
;;;                           backend (:106-320, :441, :524, :600-700), which is the one syntax-parse uses
;;;                           (parse.rkt:27-41);
;;;   - parse-interp.rkt      how patterns are simplified (simplify :60) and become matchers (compile-s :111,
;;;                           first-desc-s :244, compile-dots :273), and how the clauses are joined
;;;                           (codegen-clauses :636);
;;;   - parse-classic.rkt     how a syntax class parses (codegen-rhs :27), since define-syntax-class uses
;;;                           the classic backend (parse.rkt:34);
;;;   - rep.rkt               how a #:when becomes a post action (:1537);
;;;   - runtime-report.rkt    the whole report, from the failures to the message (:53-784);
;;;   - kws.rkt               join-sep (:114);
;;;   - report-config.rkt     the default way literals and data are named in a report.
;;;
;;; ==================== PATTERNS ====================
;;; A clause is (PATTERN body ...), and PATTERN is made of
;;;   - a symbol of #:datum-literals, which matches a term whose datum is that symbol;
;;;   - `_`, which matches anything;
;;;   - a pattern variable x;
;;;   - x:id, x:nat, or x:CLASS for a class made by make-syntax-class and bound to the variable CLASS (the
;;;     name is split at its first colon, as rep.rkt:711 splits it, after the datum literals are looked up);
;;;   - (~describe DESC PATTERN), where DESC is a string literal (a static description) or any other
;;;     expression (a dynamic description), evaluated over the pattern variables bound before it when the
;;;     matcher reaches it, as Racket unwraps it (residual-interp.rkt:261);
;;;   - a list of patterns, possibly dotted, where ~! is a cut and a pattern followed by `...` matches the
;;;     rest of the list element by element;
;;;   - any other datum, such as (), a number or a string, which matches a term whose datum is equal to it.
;;; In a body, a variable is bound to the syntax object it matched, and a variable under `...` to the list
;;; of the syntax objects it matched, so upstream's #'x is x, (syntax->list #'(x0 x ...)) is (cons x0 x),
;;; (syntax->list #'((x A) ...)) is (map list x A), and #'x.name is x.
;;;
;;; The macro turns PATTERN into data at expansion time, the body into a λ over the pattern variables in
;;; order, the dynamic descriptions into λs over the variables bound before them, and the class names into
;;; references to the classes. The matcher interprets the data with the matchers of residual-interp.rkt.
;;; Like Racket (parse-interp.rkt:60), the macro first turns the parts of a pattern that are made of
;;; variables, identifiers, data, pairs, plain ellipses and static descriptions into "simple" patterns,
;;; matched without backtracking by residual-interp.rkt:603. The difference shows in the reports, because
;;; a simple pattern at the head of a pair that is not simple has no first description, so (claim) reports
;;; "expected more terms starting with name" while (check-same) reports only "expected more terms",
;;; though both patterns start with a static ~describe. Racket also tries to factor the clauses into a
;;; matrix (opt-interp.rkt); it keeps the matrix only when it scores 2 or more, and Pie's clauses, whose
;;; first patterns all differ, never do, so the clauses are tried one after the other as in p-or
;;; (parse-interp.rkt:651).
;;;
;;; ==================== FAILURES ====================
;;; A failed match is a progress, the path to the term where matching stopped, and an expectation stack,
;;; what was expected there and inside which descriptions. When every clause has failed, the report keeps
;;; the failures that went furthest, merges their expectations and writes the message of
;;; runtime-report.rkt:739, raised as (raise-racket-exn 'exn:fail:syntax MESSAGE SRCLOCS) of
;;; (aux pie racket). Terms are written with racket-write and cut at error-print-width, 256 characters for
;;; at:, within: and in: and 50 for the terms of the parsing context.
;;;
;;; Not imitated, because parser.rkt does not use them, are #:literals and ~literal, ~or, ~and, ~not,
;;; ~seq and the other head patterns, ~optional, ~once and ~between, ellipses that are not plain `...`,
;;; side clauses in syntax-parse itself, ~do, ~parse, ~fail, #:context, #:opaque and #:role descriptions,
;;; nested attributes such as x.name, splicing classes, and vector, box and prefab patterns. Without
;;; ~and there are no ord progress frames, and without #:opaque no opaque ones are ever made, though the
;;; report still handles the latter.

(module (aux pie syntax-parse)
  ((syntax-parse syntax-parse-run) make-syntax-class raise-syntax-error)

  (import scheme
          (chicken base)
          (only (scheme base) exact-integer?)
          (chicken string)
          (chicken format)
          (only srfi-1 filter any every partition last concatenate)
          (aux base)
          (aux pie racket)
          (aux pie syntax))

  ;; The transformer of syntax-parse runs in every importer, so what it calls is imported for syntax too.
  (import-for-syntax scheme
                     (chicken base)
                     (chicken syntax)
                     (only srfi-13 string-index)
                     (aux base))

  ;;; ==================== THE MACRO ====================

  ;; (syntax-parse STX-EXPR [#:datum-literals (symbol ...)] (PATTERN body ...) ...), cf. parse.rkt:133
  ;; and parse-interp.rkt:636. STX-EXPR is evaluated once, then the clauses are tried in order.
  ;;
  ;; The expansion is (syntax-parse-run STX (list CLAUSE ...)), where a CLAUSE is the vector
  ;; #(DATA CLASSES DYNAMICS BODY). DATA is the pattern as a quoted datum, made of
  ;;   (any) (var) (datum D) (integrated BIND? KIND) (class BIND? K) (describe DESC P) (pair P1 P2)
  ;;   (cut P) (dots P TAIL N) (simple S)
  ;; where KIND is id or nat, K indexes CLASSES (the vector of the classes the pattern names), DESC is a
  ;; string or (dyn K) with K indexing DYNAMICS (the vector of λs that compute the dynamic descriptions), N
  ;; is the number of variables of P, BIND? says whether the class use binds a variable, and S is a simple
  ;; pattern (see SIMPLE PATTERNS). BODY is a λ over every pattern variable, in the order the pattern binds
  ;; them.
  ;;
  ;; clause->expr makes the expression of the vector #(DATA CLASSES DYNAMICS BODY) of one clause, given the
  ;; datum literals. The variables are injected, so the λs bind the names that the user's bodies and
  ;; descriptions refer to.
  (define-macro (syntax-parse (inject) (compare))
    ((_ ,stx-expr . ,rest)
      (let1 (clause->expr
              (λ (clause literals)
                (let ((vars '()) (classes '()) (dynamics '()))
                  (define (bind! name) (set! vars (cons name vars)))
                  (define (params) (map inject (reverse vars)))
                  (define (symbol-pattern s)
                    (let* ((name (symbol->string s))
                           (i (string-index name #\:)))
                      (cond
                        ((memq s literals) (list 'datum s))
                        ((eq? s '_) '(any))
                        ;; x:CLASS, as the regexp ^([^:]*):(.+)$ of rep.rkt:711 splits it
                        ((and i (< (add1 i) (string-length name)))
                          (let* ((var (substring name 0 i))
                                 (class (substring name (add1 i)))
                                 (bind? (not (string=? var "_")))
                                 (pattern (cond
                                            ((string=? class "id") (list 'integrated bind? 'id))
                                            ((string=? class "nat") (list 'integrated bind? 'nat))
                                            (else
                                              (push! (inject (string->symbol class)) classes)
                                              (list 'class bind? (sub1 (length classes)))))))
                            (when bind? (bind! (string->symbol var)))
                            pattern))
                        (else (bind! s) '(var)))))
                  (define (single p)
                    (let1 (s (strip-syntax p))
                      (cond
                        ((symbol? s) (symbol-pattern s))
                        ((and (pair? s) (eq? (car s) '~describe))
                          (let* ((desc (cadr p))
                                 (desc* (if (string? (strip-syntax desc))
                                          (strip-syntax desc)
                                          (begin
                                            (push! `(λ ,(params) ,desc) dynamics)
                                            (list 'dyn (sub1 (length dynamics)))))))
                            (list 'describe desc* (single (caddr p)))))
                        ((pair? s) (sequence p))
                        ((null? s) (list 'datum '()))
                        (else (list 'datum s)))))
                  (define (sequence p)
                    (cond
                      ((null? p) (list 'datum '()))
                      ((not (pair? p)) (single p))
                      ((eq? (strip-syntax (car p)) '~!) (list 'cut (sequence (cdr p))))
                      ((and (pair? (cdr p)) (eq? (strip-syntax (cadr p)) '...))
                        (let* ((n0 (length vars))
                               (head (single (car p)))
                               (n (- (length vars) n0))
                               (tail (sequence (cddr p))))
                          (list 'dots head tail n)))
                      (else
                        (let* ((head (single (car p)))
                               (tail (sequence (cdr p))))
                          (list 'pair head tail)))))
                  ;; (simplify d) : (list KIND DATA S), the simplification of parse-interp.rkt:60, bottom up.
                  ;; KIND is trivial, simple or plain, S the simple pattern of the first two kinds, and DATA
                  ;; the pattern to match when this part is not absorbed by a larger simple one.
                  (define (simplify d)
                    (let1 (tag (car d))
                      (cond
                        ((eq? tag 'any) (list 'trivial d '_))
                        ((eq? tag 'var) (list 'trivial d 'var))
                        ((and (eq? tag 'integrated) (eq? (caddr d) 'id)) (list 'trivial d (if (cadr d) 'id '_id)))
                        ((and (eq? tag 'datum) (null? (cadr d))) (list 'trivial d 'null))
                        ((eq? tag 'datum) (list 'trivial d (list 'sdatum (cadr d))))
                        ((eq? tag 'pair)
                          (let* ((h (simplify (cadr d))) (t (simplify (caddr d))))
                            (if (and (simple? h) (simple? t))
                              (let1 (s (list 'scons (caddr h) (caddr t))) (list 'simple (list 'simple s) s))
                              (list 'plain (list 'pair (final h) (final t)) #f))))
                        ((eq? tag 'dots)
                          (let* ((h (simplify (cadr d))) (t (simplify (caddr d))))
                            (if (and (simple? h) (simple? t) (eq? (caddr t) 'null))
                              (let1 (s (list 'sdots (caddr h) (cadddr d)))
                                (if (memq (caddr h) '(var id expr))
                                  (list 'trivial d s)
                                  (list 'simple (list 'simple s) s)))
                              (list 'plain (list 'dots (final h) (final t) (cadddr d)) #f))))
                        ((eq? tag 'describe)
                          (let1 (p (simplify (caddr d)))
                            (if (and (string? (cadr d)) (simple? p))
                              (let1 (s (list 'sdescribe (caddr p) (cadr d))) (list 'simple (list 'simple s) s))
                              (list 'plain (list 'describe (cadr d) (final p)) #f))))
                        ((eq? tag 'cut) (list 'plain (list 'cut (final (simplify (cadr d)))) #f))
                        (else (list 'plain d #f)))))
                  (define (simple? r) (memq (car r) '(trivial simple)))
                  ;; a trivial part that stays alone goes back to its original, cf. detrivialize-transform
                  (define (final r) (cadr r))
                  (let1 (data (final (simplify (single (car clause)))))
                    `(vector (quote ,data)
                             (vector ,@(reverse classes))
                             (vector ,@(reverse dynamics))
                             (λ ,(params) ,@(cdr clause)))))))
        (match/first rest
          ((#:datum-literals ,literals . ,clauses)
            (let1 (literals (strip-syntax literals))
              `(syntax-parse-run ,stx-expr (list ,@(map (λ (clause) (clause->expr clause literals)) clauses)))))
          (,clauses
            `(syntax-parse-run ,stx-expr (list ,@(map (λ (clause) (clause->expr clause '())) clauses))))))))

  ;;; ==================== PROGRESS ====================
  ;;
  ;; A Progress is a non-empty list of frames, the latest first, cf. residual.rkt:160-190
  ;;   - a syntax object, the base frame that syntax-parse starts from;
  ;;   - car, the car of a pair;
  ;;   - a positive integer, that many cdrs;
  ;;   - post, a check made after the term was matched (a #:when);
  ;;   - opaque, the inside of an opaque description.
  ;; The frames apply from the last to the first, so (car 1 stx) is the car of the cdr of stx.

  ;; ps-add-car : Progress -> Progress, cf. residual.rkt:196
  (define (ps-add-car parent) (cons 'car parent))

  ;; ps-add-cdr : Progress [Natural] -> Progress, merging consecutive cdrs, cf. residual.rkt:198
  (define (ps-add-cdr parent #!optional (times 1))
    (cond
      ((zero? times) parent)
      ((exact-positive-integer? (car parent)) (cons (+ times (car parent)) (cdr parent)))
      (else (cons times parent))))

  ;; ps-add-post : Progress -> Progress, cf. residual.rkt:218
  (define (ps-add-post parent) (cons 'post parent))

  (define (exact-positive-integer? v) (and (exact-integer? v) (positive? v)))

  ;;; ==================== EXPECTATIONS ====================
  ;;
  ;; While matching, an ExpectStack is #t (nothing expected yet) or an expectation whose next field is
  ;; the rest of the stack, the most specific on top, cf. residual.rkt:279-290
  ;;   - (expect:thing Progress String Boolean Role ExpectStack), a description entered at that progress;
  ;;   - (expect:message String ExpectStack);
  ;;   - (expect:atom Datum ExpectStack), a datum that was expected;
  ;;   - (expect:proper-pair FirstDesc ExpectStack), more terms were expected.
  ;; Only things occur below the top. For the report the stack becomes an ExpectList, where next is the
  ;; StxIdx of the term the expectation is about and a thing's progress is #f, and expect:disj joins the
  ;; expectations of several failures, cf. residual.rkt:299-316.
  ;;
  ;; A FirstDesc says what the first of the missing terms should have been, cf. residual.rkt:352-357
  ;;   - #f, unknown;
  ;;   - a string, a static description;
  ;;   - (any), any term;
  ;;   - (datum D), the datum D.
  ;;
  ;; A StxIdx is (cons Syntax Natural), a term and how many cdrs into it the expectation points.

  ;; cf. residual.rkt:318-323
  (define-struct expect:thing term description transparent? role next)
  (define-struct expect:message message next)
  (define-struct expect:atom atom next)
  (define-struct expect:disj expects next)
  (define-struct expect:proper-pair first-desc next)

  ;; expect? : Any -> Boolean, cf. residual.rkt:325
  (define (expect? x)
    (or (expect:thing? x) (expect:message? x) (expect:atom? x) (expect:disj? x) (expect:proper-pair? x)))

  ;; es-add-thing : Progress (U String #f) Boolean Role ExpectStack -> ExpectStack, cf. residual.rkt:333
  (define (es-add-thing ps description transparent? role next)
    (if (and next (or description (not transparent?)))
        (expect:thing ps description transparent? role next)
        next))

  ;; es-add-message : (U String #f) ExpectStack -> ExpectStack, cf. residual.rkt:338
  (define (es-add-message message next)
    (if (and next message) (expect:message message next) next))

  ;; es-add-atom : Datum ExpectStack -> ExpectStack, cf. residual.rkt:343
  (define (es-add-atom atom next)
    (and next (expect:atom atom next)))

  ;; es-add-proper-pair : FirstDesc ExpectStack -> ExpectStack, cf. residual.rkt:349
  (define (es-add-proper-pair first-desc next)
    (and next (expect:proper-pair first-desc next)))

  ;; A Failure is (failure Progress ExpectStack), and a FailureSet is a Failure or a pair of FailureSets,
  ;; cf. residual.rkt:148-155
  (define-struct failure progress expectstack)

  ;; Racket's equal? on the data of a report. Syntax objects are compared with eq?, as Racket's equal? does,
  ;; because they are opaque there, while the stx struct of (aux pie syntax) would be compared field by
  ;; field by CHICKEN's equal?.
  (define (equal/stx? a b)
    (cond
      ((syntax? a) (eq? a b))
      ((pair? a) (and (pair? b) (equal/stx? (car a) (car b)) (equal/stx? (cdr a) (cdr b))))
      ((expect? a) (and (expect? b) (equal/stx? (expect->list a) (expect->list b))))
      (else (equal? a b))))

  (define (expect->list e)
    (match/first e
      (#(expect:thing ,ps ,d ,t ,r ,n) (list 'thing ps d t r n))
      (#(expect:message ,m ,n) (list 'message m n))
      (#(expect:atom ,a ,n) (list 'atom a n))
      (#(expect:disj ,es ,n) (list 'disj es n))
      (#(expect:proper-pair ,d ,n) (list 'proper-pair d n))))

  ;;; ==================== BACKTRACKING ====================
  ;;
  ;; A matcher gives a computation BT, a procedure of a success continuation sk, a failure handler fh and
  ;; a cut prompt cp, cf. residual-interp.rkt:11-20. The success continuation takes the failure handler and
  ;; the cut prompt to go on with and the result, the failure handler takes a FailureSet. The undo stack of
  ;; upstream is left out, because nothing in these patterns has an effect to undo.

  ;; succeed : Any -> BT, cf. residual-interp.rkt:39
  (define (succeed v) (λ (sk fh cp) (sk fh cp v)))

  ;; fail : FailureSet -> BT, cf. residual-interp.rkt:46
  (define (fail f) (λ (sk fh cp) (fh f)))

  ;; bind : BT (Any -> BT) -> BT, cf. residual-interp.rkt:50
  (define (bind c f) (λ (sk fh cp) (c (λ (fh cp v) ((f v) sk fh cp)) fh cp)))

  ;; fh-add-failure : FH FailureSet -> FH, a handler that adds f1 to the failures it gets, cf.
  ;; residual-interp.rkt:24
  (define (fh-add-failure fh f1) (λ (f2) (fh (cons f1 f2))))

  ;; disj* : BT (-> BT) -> BT, c1, and when it fails, the computation of cp2 with the failure kept,
  ;; cf. residual-interp.rkt:62
  (define (disj* c1 cp2)
    (λ (sk fh cp)
      (c1 sk (λ (f) ((cp2) sk (fh-add-failure fh f) cp)) cp)))

  ;;; ==================== SYNTAX CLASSES ====================

  ;; make-syntax-class : String Symbol (Syntax -> Boolean) -> Class, the class of
  ;;   (define-syntax-class NAME #:description DESCRIPTION (pattern name:BASE #:when (WHEN #'name)))
  ;; where BASE is id or nat, as parser.rkt:13-16 defines pie-id.
  (define-struct syntax-class description base when)
  (define (make-syntax-class description base when) (syntax-class description base when))

  ;; The built-in classes that are integrated into the patterns, with their descriptions, cf. lib.rkt:46-55
  (define (integrated-predicate kind)
    (case kind
      ((id) identifier?)
      ((nat) (λ (x) (and (syntax? x) (let1 (e (syntax-e x)) (and (exact-integer? e) (>= e 0))))))
      (else (error 'syntax-parse "unknown built-in class" kind))))

  (define (integrated-description kind)
    (case kind ((id) "identifier") ((nat) "exact-nonnegative-integer")))

  ;;; ==================== MATCHERS ====================
  ;;
  ;; (s-match p x cx pr es renv ext) : BT, the matcher of the pattern data p, cf. compile-s of
  ;; parse-interp.rkt:111. x is the term, a syntax object or the rest of a list of syntax objects; cx is the
  ;; nearest syntax object around x; pr and es are the progress and the expectation stack at x; renv holds
  ;; the values of the pattern variables bound so far, the latest first; ext is the clause, whose classes
  ;; and dynamic descriptions the pattern refers to by index. On success the result is renv extended with
  ;; the variables of p.

  (define (stx-e v) (if (syntax? v) (syntax-e v) v))

  (define (clause-classes ext) (vector-ref ext 1))
  (define (clause-dynamics ext) (vector-ref ext 2))

  (define (s-match p x cx pr es renv ext)
    (match/first p
      ;; cf. s-any, residual-interp.rkt:146
      ((any) (succeed renv))
      ;; cf. s-var, residual-interp.rkt:149
      ((var) (succeed (cons (datum->syntax cx x cx) renv)))
      ;; cf. s-null and s-datum, residual-interp.rkt:203-211
      ((datum ()) (if (null? (stx-e x)) (succeed renv) (fail (failure pr (es-add-atom '() es)))))
      ((datum ,d) (if (equal? (syntax->datum x) d) (succeed renv) (fail (failure pr (es-add-atom d es)))))
      ;; cf. s-integrated, residual-interp.rkt:186, and s-match-id, :181
      ((integrated ,bind? ,kind)
        (if ((integrated-predicate kind) x)
            (succeed (if bind? (cons x renv) renv))
            (fail (failure pr (es-add-thing pr (integrated-description kind) #t #f es)))))
      ((class ,bind? ,k) (s-parser (vector-ref (clause-classes ext) k) bind? x cx pr es renv))
      ;; cf. s-describe, residual-interp.rkt:261, transparent and without a role
      ((describe ,desc ,p)
        (let1 (description (if (string? desc) desc (apply (vector-ref (clause-dynamics ext) (cadr desc)) (reverse renv))))
          (s-match p x cx pr (es-add-thing pr description #t #f es) renv ext)))
      ;; cf. s-pair, residual-interp.rkt:228
      ((pair ,headp ,tailp)
        (let1 (d (stx-e x))
          (cond
            ((pair? d)
              (let1 (cx* (if (syntax? x) x cx))
                (bind (s-match headp (car d) (car d) (ps-add-car pr) es renv ext)
                      (λ (renv) (s-match tailp (cdr d) cx* (ps-add-cdr pr) es renv ext)))))
            ((null? d) (fail (failure pr (es-add-proper-pair (first-desc-s headp ext) es))))
            (else (fail (failure pr es))))))
      ;; ~! as p-action of a-cut, cf. residual-interp.rkt:112 and 441. What follows fails to the cut prompt,
      ;; which drops the failures of the clauses tried before.
      ((cut ,p) (λ (sk fh cp) ((s-match p x cx pr es renv ext) sk cp cp)))
      ;; cf. compile-dots, parse-interp.rkt:273
      ((dots ,headp ,tailp ,n)
        (match/first (list headp tailp)
          (((var) (datum ())) (s-dots1-i-null #t (λ (x) #t) #f x cx pr es renv))
          (((integrated ,bind? ,kind) (datum ()))
            (s-dots1-i-null bind? (integrated-predicate kind) (integrated-description kind) x cx pr es renv))
          (else (s-dots1 headp tailp n x cx pr es renv ext))))
      ;; cf. s-simple, residual-interp.rkt:603
      ((simple ,s)
        (let1 (r (parse-simple s x cx pr es renv))
          (if (or (pair? r) (null? r)) (succeed r) (fail r))))))

  ;; first-desc-s : Pattern Clause -> FirstDesc, what a report says the term matched by p should have been
  ;; when the list ends before it, cf. parse-interp.rkt:244. A dynamic description gives #f.
  (define (first-desc-s p ext)
    (match/first p
      ((any) '(any))
      ((var) '(any))
      ((integrated _ ,kind) (integrated-description kind))
      ((class _ ,k) (syntax-class-description (vector-ref (clause-classes ext) k)))
      ((datum ,d) (list 'datum d))
      ((describe ,desc _) (and (string? desc) desc))
      (else #f)))

  ;; s-parser : Class Boolean ... -> BT, the parser of a syntax class, cf. s-parser of
  ;; residual-interp.rkt:152 and the parser that codegen-rhs of parse-classic.rkt:27 makes. It enters the
  ;; class description, matches the base class, then checks the #:when as a post action whose failure adds
  ;; no message, cf. rep.rkt:1537 and a-fail of residual-interp.rkt:446.
  (define (s-parser class bind? x cx pr es renv)
    (let* ((kind (syntax-class-base class))
           (es (es-add-thing pr (syntax-class-description class) #t #f es)))
      (cond
        ((not ((integrated-predicate kind) x))
          (fail (failure pr (es-add-thing pr (integrated-description kind) #t #f es))))
        ((not ((syntax-class-when class) x)) (fail (failure (ps-add-post pr) es)))
        (else (succeed (if bind? (cons (datum->syntax cx x cx) renv) renv))))))

  ;; s-dots1-i-null : Boolean (Any -> Any) (U String #f) ... -> BT, (x ... . ()) for a plain variable or
  ;; an integrated class, which binds the list of the elements, cf. residual-interp.rkt:277-287
  (define (s-dots1-i-null bind? pred? desc x cx pr es renv)
    (receive (status result) (predicate-ellipsis-parser x cx pr es pred? desc #f)
      (if (eq? status 'ok)
          (succeed (if bind? (cons result renv) renv))
          (fail result))))

  ;; predicate-ellipsis-parser : ... -> (values 'ok (Listof Syntax)) or (values 'fail Failure), cf.
  ;; residual.rkt:814. A term that does not end with () fails at its tail without any expectation, so that
  ;; no spurious "expected ()" shows.
  (define (predicate-ellipsis-parser x cx pr es pred? desc rl)
    (let1 (elems (stx->list x))
      (if (and elems (every pred? elems))
          (values 'ok elems)
          (let loop ((x x) (cx cx) (i 0))
            (cond
              ((syntax? x) (loop (syntax-e x) x i))
              ((pair? x)
                (if (pred? (car x))
                    (loop (cdr x) cx (+ i 1))
                    (let* ((pr (ps-add-car (ps-add-cdr pr i)))
                           (es (es-add-thing pr desc #t rl es)))
                      (values 'fail (failure pr es)))))
              (else (values 'fail (failure (ps-add-cdr pr i) es))))))))

  ;; stx->list : Any -> (U (Listof Syntax) #f), the elements of a syntax list or of a list of syntax objects
  ;; that may end with a syntax list, cf. collects/syntax/stx.rkt
  (define (stx->list e)
    (cond
      ((syntax? e) (syntax->list e))
      ((null? e) '())
      ((pair? e) (let1 (r (stx->list (cdr e))) (and r (cons (car e) r))))
      (else #f)))

  ;; s-dots1 : Pattern Pattern Natural ... -> BT, (headp ... . tailp) in general, cf. residual-interp.rkt:304.
  ;; It matches headp on as many elements as it can, keeping at each step the choice to stop there and
  ;; match tailp on the rest. Each variable of headp gets the list of its values, in order.
  (define (s-dots1 headp tailp attrs-n x cx pr es renv ext)
    (let dots1-loop ((x x) (cx cx) (pr pr) (acc '()))
      (disj* (h-single headp x cx pr es renv ext (λ (renv* x cx pr) (dots1-loop x cx pr (cons renv* acc))))
             (τ (s-match tailp x cx pr es (append (transpose attrs-n (reverse acc)) renv) ext)))))

  ;; h-single : Pattern ... (REnv Any Syntax Progress -> BT) -> BT, sp on the first element of x, then k
  ;; on the rest, cf. residual-interp.rkt:524
  (define (h-single sp x cx pr es renv ext k)
    (let1 (d (stx-e x))
      (cond
        ((pair? d)
          (bind (s-match sp (car d) (car d) (ps-add-car pr) es renv ext)
                (λ (renv) (k renv (cdr d) (if (syntax? x) x cx) (ps-add-cdr pr)))))
        ((null? d) (fail (failure pr (es-add-proper-pair (first-desc-s sp ext) es))))
        (else (fail (failure pr es))))))

  ;; transpose : Natural (Listof REnv) -> (Listof (Listof Any)), the values of the ncols latest variables of
  ;; each row, one list per variable, cf. residual-interp.rkt:320
  (define (transpose ncols rows)
    (if (zero? ncols)
        '()
        (cons (map car rows) (transpose (- ncols 1) (map cdr rows)))))

  ;;; ==================== SIMPLE PATTERNS ====================
  ;;
  ;; A simple pattern S is one of, cf. the simple submodule of rep-patterns.rkt:218
  ;;   _ var id _id    anything, anything bound, an identifier bound, an identifier not bound;
  ;;   null            ();
  ;;   (scons S S)     a pair;
  ;;   (sdots S N)     (S ... . ()) where S binds N variables;
  ;;   (sdatum D)      the datum D;
  ;;   (sdescribe S D) S under the static description D.
  ;; It is matched without backtracking. A Result is the extended renv on success and a Failure otherwise.

  ;; parse-simple : S Any Syntax Progress ExpectStack REnv -> Result, cf. parse of residual-interp.rkt:608
  (define (parse-simple simple x cx pr es renv)
    (define (rfail-thing desc) (failure pr (es-add-thing pr desc #t #f es)))
    (match/first simple
      (var (cons (datum->syntax cx x cx) renv))
      (id (if (identifier? x) (cons x renv) (rfail-thing "identifier")))
      ((,s ⊣ (eq? s '_)) renv)
      (_id (if (identifier? x) renv (rfail-thing "identifier")))
      (null (if (null? (stx-e x)) renv (failure pr (es-add-atom '() es))))
      ((scons ,simple1 ,simple2)
        (let1 (d (stx-e x))
          (cond
            ((pair? d)
              (let1 (cx* (if (syntax? x) x cx))
                (rbind (parse-simple simple1 (car d) (car d) (ps-add-car pr) es renv)
                       (λ (renv) (parse-simple simple2 (cdr d) cx* (ps-add-cdr pr) es renv)))))
            ((null? d) (failure pr (es-add-proper-pair (first-desc-simple simple1) es)))
            (else (failure pr es)))))
      ((sdots ,simple ,nattrs)
        (match/first simple
          (var (receive (status result) (predicate-ellipsis-parser x cx pr es (λ (x) #t) #f #f)
                 (if (eq? status 'ok) (cons result renv) result)))
          (id (receive (status result) (predicate-ellipsis-parser x cx pr es identifier? "identifier" #f)
                (if (eq? status 'ok) (cons result renv) result)))
          (else (parse-simple-dots simple nattrs x cx pr es renv))))
      ((sdatum ,d) (if (equal? (syntax->datum x) d) renv (failure pr (es-add-atom d es))))
      ((sdescribe ,simple ,desc) (parse-simple simple x cx pr (es-add-thing pr desc #t #f es) renv))))

  ;; rbind : Result (REnv -> Result) -> Result, cf. residual-interp.rkt:600
  (define (rbind c f) (if (or (null? c) (pair? c)) (f c) c))

  ;; parse-simple-dots : S Natural ... -> Result, every element matched with S, which cannot backtrack,
  ;; cf. parse-dots of residual-interp.rkt:661
  (define (parse-simple-dots simple nattrs x cx pr es renv)
    (let dots-loop ((x x) (cx cx) (pr pr) (acc '()))
      (let1 (d (stx-e x))
        (cond
          ((pair? d)
            (rbind (parse-simple simple (car d) (car d) (ps-add-car pr) es '())
                   (λ (local-renv) (dots-loop (cdr d) (if (syntax? x) x cx) (ps-add-cdr pr) (cons local-renv acc)))))
          ((null? d) (append (transpose nattrs (reverse acc)) renv))
          (else (failure pr es))))))

  ;; first-desc-simple : S -> FirstDesc, cf. residual-interp.rkt:683. A simple () gives #f, unlike the
  ;; FirstDesc of the datum () in first-desc-s.
  (define (first-desc-simple simple)
    (match/first simple
      (var '(any))
      (id "identifier")
      ((sdescribe _ ,desc) desc)
      ((sdatum ,d) (list 'datum d))
      ((,s ⊣ (eq? s '_)) '(any))
      (else #f)))

  ;;; ==================== CLAUSES ====================

  ;; syntax-parse-run : Syntax (Listof Clause) -> Any, the clauses tried in order on x, the body of the first
  ;; one that matches applied to its variables, cf. run-clauses of residual-interp.rkt:753 and
  ;; codegen-clauses of parse-interp.rkt:636. The expectations start from #t, the progress from x itself,
  ;; and when every clause fails the failures go to the report. The cut prompt is the final handler, so a
  ;; cut drops every failure collected before it.
  (define (syntax-parse-run x clauses)
    (let1 (fh (λ (fs) (handle-failureset (list #f x) fs)))
      ((p-or (map (λ (clause) (clause-matcher clause x)) clauses)) (λ (fh cp thunk) (thunk)) fh fh)))

  ;; The matcher of a clause, which answers the thunk of its body, as the action that codegen-clauses adds
  ;; after each pattern (parse-interp.rkt:641-644) and base-sk calls (residual-interp.rkt:766).
  (define (clause-matcher clause x)
    (let ((pattern (vector-ref clause 0)) (body (vector-ref clause 3)))
      (bind (s-match pattern x x (list x) #t '() clause)
            (λ (renv) (succeed (τ (apply body (reverse renv))))))))

  ;; p-or : (Listof BT) -> BT, cf. residual-interp.rkt:106
  (define (p-or cs)
    (if (null? (cdr cs))
        (car cs)
        (disj* (car cs) (τ (p-or (cdr cs))))))

  ;;; ==================== FAILURE SETS ====================

  ;; handle-failureset : (list Symbol/#f Syntax) FailureSet -> Nothing, cf. runtime-report.rkt:53. The
  ;; failures are inverted, the ones that went furthest kept, and their expectations made into one report.
  (define (handle-failureset ctx fs)
    (let* ((inverted-fs (map invert-failure (reverse (flatten* fs))))
           (maximal-classes (maximal-failures inverted-fs))
           (ess (map failure-expectstack (concatenate maximal-classes)))
           (report (report/sync-shared ess)))
      (error/report ctx report)))

  ;; flatten* : FailureSet -> (Listof Failure), racket/list's flatten, which also takes the improper pairs
  ;; (cons f1 f2) that fh-add-failure builds. CHICKEN's flatten wants proper lists.
  (define (flatten* fs)
    (cond ((null? fs) '()) ((pair? fs) (append (flatten* (car fs)) (flatten* (cdr fs)))) (else (list fs))))

  ;; invert-failure : Failure -> RFailure, the progress inverted and the expectations made into a list
  ;; from the root to the leaf, cf. runtime-report.rkt:68
  (define (invert-failure f)
    (match1/first (#(failure ,ps ,es) f)
      (failure (invert-ps ps) (invert-expectstack es (ps->stx+index ps)))))

  ;; A Report is (report String (Listof String) Syntax/#f Syntax/#f), cf. runtime-report.rkt:74
  (define-struct report message context stx within-stx)

  ;;; ==================== PROGRESS ORDERING ====================
  ;;
  ;; Progress compares lexicographically from the root, with car < cdr and post above both, while two
  ;; different syntax frames do not compare, cf. runtime-report.rkt:103-122.

  ;; maximal-failures : (Listof RFailure) -> (Listof (Listof RFailure)), cf. runtime-report.rkt:98
  (define (maximal-failures fs)
    (maximal/progress (map (λ (f) (cons (failure-progress f) f)) fs)))

  ;; invert-ps : Progress -> IPS, reversed and cut at the earliest opaque frame, cf. runtime-report.rkt:129
  (define (invert-ps ps) (reverse (ps-truncate-opaque ps)))

  ;; ps-truncate-opaque : Progress -> Progress, the longest tail without an opaque frame,
  ;; cf. runtime-report.rkt:134
  (define (ps-truncate-opaque ps)
    (let loop ((ps ps) (acc ps))
      (cond
        ((null? ps) acc)
        ((eq? (car ps) 'opaque) (loop (cdr ps) (cdr ps)))
        (else (loop (cdr ps) acc)))))

  ;; maximal/progress : (Listof (cons IPS A)) -> (Listof (Listof A)), the items of maximal progress grouped
  ;; by progress, cf. runtime-report.rkt:145. Without ~and there are no ord frames to sort out first.
  (define (maximal/progress items)
    (cond
      ((null? items) '())
      ((null? (cdr items)) (list (list (cdar items))))
      (else (maximal/prf1 items))))

  ;; maximal/prf1 : (Listof (cons IPS A)) -> (Listof (Listof A)), cf. runtime-report.rkt:160
  (define (maximal/prf1 items)
    (receive (POST rest1) (partition (λ (item) (eq? 'post (item-first-prf item))) items)
      (if (pair? POST)
          (maximal/progress (map item-pop-prf POST))
          (receive (STX rest2) (partition (λ (item) (syntax? (item-first-prf item))) rest1)
            (receive (CDR rest3) (partition (λ (item) (exact-integer? (item-first-prf item))) rest2)
              (receive (CAR rest4) (partition (λ (item) (eq? 'car (item-first-prf item))) rest3)
                (receive (NULL rest5) (partition (λ (item) (eq? #f (item-first-prf item))) rest4)
                  (unless (null? rest5) (error 'syntax-parse (format "INTERNAL ERROR: bad progress: ~s\n" rest5)))
                  (cond
                    ((pair? CDR)
                      (let1 (leastCDR (apply min (map item-first-prf CDR)))
                        (append (maximal/stx STX)
                                (maximal/progress (map (λ (item) (item-pop-prf-ncdrs item leastCDR)) CDR)))))
                    ((pair? CAR) (append (maximal/stx STX) (maximal/progress (map item-pop-prf CAR))))
                    ((pair? STX) (maximal/stx STX))
                    ((pair? NULL) (list (map cdr NULL)))
                    (else '())))))))))

  ;; maximal/stx : (Listof (cons IPS A)) -> (Listof (Listof A)), items that start with a syntax frame,
  ;; grouped by that frame, cf. runtime-report.rkt:204
  (define (maximal/stx items)
    (concatenate (map (λ (group) (maximal/progress (map item-pop-prf group))) (group-by item-first-prf items))))

  ;; item-first-prf : (cons IPS A) -> (U Frame #f), cf. runtime-report.rkt:226
  (define (item-first-prf item)
    (let1 (ips (car item)) (and (pair? ips) (car ips))))

  ;; item-pop-prf : (cons IPS A) -> (cons IPS A), cf. runtime-report.rkt:242
  (define (item-pop-prf item) (cons (cdar item) (cdr item)))

  ;; item-pop-prf-ncdrs : (cons IPS A) Natural -> (cons IPS A), cf. runtime-report.rkt:249
  (define (item-pop-prf-ncdrs item ncdrs)
    (let ((ips (car item)) (a (cdr item)))
      (if (= (car ips) ncdrs)
          (cons (cdr ips) a)
          (cons (cons (- (car ips) ncdrs) (cdr ips)) a))))

  ;; ps->stx+index : Progress -> StxIdx, the innermost syntax object on the path and how many cdrs into it
  ;; the progress ends, cf. runtime-report.rkt:260
  (define (ps->stx+index ps)
    (define (interp ps top?)
      (let1 (frame (car ps))
        (cond
          ((syntax? frame) frame)
          ((eq? frame 'car)
            (let* ((x (interp (cdr ps) #f))
                   (d (if (syntax? x) (syntax-e x) x)))
              (cond
                ((pair? d) (car d))
                ((vector? d) (if top? x (vector->list d)))
                ((box? d) (unbox d))
                (else (error 'ps->stx+index (format "INTERNAL ERROR: unexpected: ~s" d))))))
          ((exact-positive-integer? frame)
            (let loop ((stx (interp (cdr ps) #f)) (i 0))
              (if (= i frame) stx (loop (stx-cdr stx) (+ i 1)))))
          ((eq? frame 'post) (interp (cdr ps) top?)))))
    (let loop ((ps (ps-truncate-opaque ps)))
      (let1 (frame (car ps))
        (cond
          ((syntax? frame) (cons frame 0))
          ((eq? frame 'car) (cons (interp ps #t) 0))
          ((exact-positive-integer? frame)
            (let1 (stx+index (loop (cdr ps))) (cons (car stx+index) (+ (cdr stx+index) frame))))
          ((eq? frame 'post) (loop (cdr ps)))))))

  (define (stx-car p) (car (stx-e p)))
  (define (stx-cdr p) (cdr (stx-e p)))
  (define (stx-pair? x) (pair? (stx-e x)))

  ;; stx+index->at+within : StxIdx -> (values Syntax (U Syntax #f)), the "at" term, made a syntax object
  ;; with the location of "within" when it is a tail, and the enclosing term when it differs, cf. the table
  ;; of runtime-report.rkt:84-92 and stx+index->at+within, :297
  (define (stx+index->at+within stx+index)
    (let ((within-stx (car stx+index)) (index (cdr stx+index)))
      (if (zero? index)
          (values within-stx #f)
          (let* ((d (syntax-e within-stx))
                 (stx* (if (vector? d) (vector->list d) within-stx))
                 (at-stx* (let loop ((x stx*) (i 0)) (if (= i index) x (loop (stx-cdr x) (+ i 1))))))
            (values (datum->syntax within-stx at-stx* within-stx) within-stx)))))

  ;;; ==================== EXPECTATION LISTS ====================

  ;; invert-expectstack : ExpectStack StxIdx -> RExpectList, the stack as a list from the root to the leaf,
  ;; each expectation pointing at its term, the frames inside an opaque description and the things
  ;; without a description dropped, cf. runtime-report.rkt:325
  (define (invert-expectstack es stx+index)
    (let loop ((es es) (acc '()))
      (match/first es
        (#f acc)
        (#t acc)
        (#(expect:thing ,ps ,desc ,tr? ,role ,rest-es)
          (let* ((acc (if (not tr?) '() acc))
                 (acc (if desc (cons (expect:thing #f desc tr? role (ps->stx+index ps)) acc) acc)))
            (loop rest-es acc)))
        (#(expect:message ,message ,rest-es) (loop rest-es (cons (expect:message message stx+index) acc)))
        (#(expect:atom ,atom ,rest-es) (loop rest-es (cons (expect:atom atom stx+index) acc)))
        (#(expect:proper-pair ,first-desc ,rest-es)
          (loop rest-es (cons (expect:proper-pair first-desc stx+index) acc))))))

  ;; expect->stxidx : Expect -> StxIdx, cf. runtime-report.rkt:346
  (define (expect->stxidx e)
    (cond
      ((expect:thing? e) (expect:thing-next e))
      ((expect:message? e) (expect:message-next e))
      ((expect:atom? e) (expect:atom-next e))
      ((expect:proper-pair? e) (expect:proper-pair-next e))
      ((expect:disj? e) (expect:disj-next e))))

  ;;; ==================== SIMPLIFICATION ====================
  ;;
  ;; The expectation lists of the maximal failures form a tree. The report keeps the frames that all of them
  ;; share, from the root, and where they diverge it syncs on the frames and then on the terms they still
  ;; share, joining the most specific expectations about the last shared term in a disjunction; '... marks
  ;; the frames it skipped. The cases of runtime-report.rkt:354-440 explain the choice.

  ;; report* : (Listof RExpectList) ((Listof (Listof RExpectList)) -> ExpectList) -> Report,
  ;; cf. runtime-report.rkt:444
  (define (report* ess handle-divergence)
    (let* ((es (let loop ((ess ess) (acc '()))
                 (if (any null? ess)
                     acc
                     (let1 (groups (group-by car ess))
                       (if (singleton? groups)
                           (let* ((group (car groups)) (frame (car (car group))))
                             (loop (map cdr group) (cons frame acc)))
                           (append (handle-divergence groups) acc))))))
           (stx+index (if (pair? es) (expect->stxidx (car es)) (cons #f 0))))
      (report/expectstack (clean-up es) stx+index)))

  ;; clean-up : ExpectList -> ExpectList, without leading '... and with adjacent ones collapsed,
  ;; cf. runtime-report.rkt:461
  (define (clean-up es)
    (if (and (pair? es) (eq? (car es) '...))
        (clean-up (cdr es))
        (let loop ((es es))
          (cond
            ((null? es) '())
            ((eq? (car es) '...) (cons '... (clean-up es)))
            (else (cons (car es) (loop (cdr es))))))))

  ;; report/sync-shared : (Listof RExpectList) -> Report, cf. runtime-report.rkt:490
  (define (report/sync-shared ess) (report* ess handle-divergence/sync-shared))

  ;; handle-divergence/sync-shared : (Listof (Listof RExpectList)) -> ExpectList, cf. runtime-report.rkt:494
  (define (handle-divergence/sync-shared ess-groups)
    (let* ((ess (concatenate ess-groups))
           (shared-frames (get-shared ess (λ (x) x)))
           (rsegs (map (λ (es) (rsplit es (λ (x) x) shared-frames)) ess))
           (final-seg (map car rsegs))
           (ctx-rsegs (transpose* (map cdr rsegs))))
      (append (hd/sync-shared/final final-seg) (hd/sync-shared/ctx ctx-rsegs))))

  ;; hd/sync-shared/final : (Listof RExpectList) -> ExpectList, the disjunction of the most specific
  ;; expectations about the last term the lists share, cf. runtime-report.rkt:506
  (define (hd/sync-shared/final ess0)
    (let* ((ess (remove-extensions ess0))
           (shared-terms (get-shared ess expect->stxidx)))
      (if (null? shared-terms)
          '()
          (let* ((last-term (last shared-terms))
                 (rsegs (map (λ (es) (rsplit es expect->stxidx (list last-term))) ess))
                 (last-term-ess (map cadr rsegs))
                 (pre-term-ess (map caddr rsegs)))
            (append (list (expect:disj (remove-duplicates* (reverse (map last last-term-ess))) last-term))
                    (if (any pair? pre-term-ess) '(...) '()))))))

  ;; hd/sync-shared/ctx : (Listof (Listof RExpectList)) -> ExpectList, the shared frames from the leaf to the
  ;; root, with '... where some list had other frames in between, cf. runtime-report.rkt:529
  (define (hd/sync-shared/ctx rsegs)
    (let loop ((rsegs rsegs))
      (cond
        ((null? rsegs) '())
        ((null? (cdr rsegs)) (error 'syntax-parse "INTERNAL ERROR: bad segments"))
        (else (append (list (car (car (car rsegs))))
                      (if (any pair? (cadr rsegs)) '(...) '())
                      (loop (cddr rsegs)))))))

  ;; transpose* : (Listof (Listof X)) -> (Listof (Listof X)), as long as the shortest list,
  ;; cf. runtime-report.rkt:542
  (define (transpose* xss)
    (if (any null? xss)
        '()
        (cons (map car xss) (transpose* (map cdr xss)))))

  ;; get-shared : (Listof (Listof X)) (X -> Y) -> (Listof Y), the Ys that occur in the same order in every
  ;; list, cf. runtime-report.rkt:548. The hash of each list maps a Y to the index of its last occurrence.
  (define (get-shared xss get-y)
    (if (null? xss)
        '()
        (let* ((yhs (map (λ (xs)
                           (let loop ((xs xs) (i 1) (h '()))
                             (if (null? xs) h (loop (cdr xs) (+ i 1) (cons (cons (get-y (car xs)) i) h)))))
                         xss))
               (hash-ref (λ (yh y) (let1 (entry (assoc* y yh)) (if entry (cdr entry) -1)))))
          (remove-duplicates*
            (let loop ((xs (car xss)) (last (map (λ (xs) 0) xss)))
              (if (null? xs)
                  '()
                  (let* ((y (get-y (car xs)))
                         (curr (map (λ (yh) (hash-ref yh y)) yhs)))
                    (if (every > curr last)
                        (cons y (loop (cdr xs) curr))
                        (loop (cdr xs) last)))))))))

  (define (assoc* key alist)
    (cond ((null? alist) #f) ((equal/stx? key (caar alist)) (car alist)) (else (assoc* key (cdr alist)))))

  ;; rsplit : (Listof X) (X -> Y) (Listof Y) -> (Listof (Listof X)), xs cut at y1 ... yN into
  ;; [rest gotN preN ... got1 pre1], cf. runtime-report.rkt:570
  (define (rsplit xs get-y ys)
    (define (loop xs ys segsacc)
      (if (null? ys) (cons xs segsacc) (pre-loop xs ys segsacc '())))
    (define (pre-loop xs ys segsacc preacc)
      (if (and (pair? xs) (equal/stx? (get-y (car xs)) (car ys)))
          (got-loop (cdr xs) ys segsacc preacc (list (car xs)))
          (pre-loop (cdr xs) ys segsacc (cons (car xs) preacc))))
    (define (got-loop xs ys segsacc preacc gotacc)
      (if (and (pair? xs) (equal/stx? (get-y (car xs)) (car ys)))
          (got-loop (cdr xs) ys segsacc preacc (cons (car xs) gotacc))
          (loop xs (cdr ys) (cons (reverse gotacc) (cons (reverse preacc) segsacc)))))
    (loop xs ys '()))

  ;; singleton? : Any -> Boolean, cf. runtime-report.rkt:587
  (define (singleton? x) (and (pair? x) (null? (cdr x))))

  ;; remove-extensions : (Listof (Listof X)) -> (Listof (Listof X)), without the lists that extend another
  ;; one, cf. runtime-report.rkt:591
  (define (remove-extensions xss)
    (if (null? xss)
        '()
        (let loop ((xss xss))
          (cond
            ((singleton? xss) xss)
            ((any null? xss) (list '()))
            (else (concatenate (map (λ (group) (map (λ (x) (cons (caar group) x)) (loop (map cdr group))))
                                (group-by car xss))))))))

  ;; group-by : (X -> Y) (Listof X) -> (Listof (Listof X)), racket/list's group-by under Racket's equal?,
  ;; that is the classes in the order of their first element, each in the order of the list.
  (define (group-by key lst)
    (let loop ((lst lst) (classes '()))
      (if (null? lst)
          (reverse (map (λ (class) (reverse (cdr class))) classes))
          (let* ((k (key (car lst))) (class (assoc* k classes)))
            (if class
                (begin (set-cdr! class (cons (car lst) (cdr class))) (loop (cdr lst) classes))
                (loop (cdr lst) (cons (list k (car lst)) classes)))))))

  ;; remove-duplicates* : (Listof X) -> (Listof X), racket/list's remove-duplicates under Racket's equal?,
  ;; keeping the first occurrence of each.
  (define (remove-duplicates* lst)
    (let loop ((lst lst) (seen '()))
      (cond
        ((null? lst) '())
        ((any (λ (y) (equal/stx? y (car lst))) seen) (loop (cdr lst) seen))
        (else (cons (car lst) (loop (cdr lst) (cons (car lst) seen)))))))

  ;;; ==================== REPORTING ====================

  ;; report/expectstack : ExpectList StxIdx -> Report, the leaf expectation as the message and the frames
  ;; above it as the parsing context, cf. runtime-report.rkt:612
  (define (report/expectstack es stx+index)
    (let* ((frame-expect (and (pair? es) (car es)))
           (context-frames (if (pair? es) (cdr es) '()))
           (context (concatenate (map context-prose-for-expect context-frames))))
      (if (not frame-expect)
          (report "bad syntax" context #f #f)
          (receive (frame-stx within-stx) (stx+index->at+within stx+index)
            (cond
              ((and (expect:atom? frame-expect) (null? (expect:atom-atom frame-expect)) (stx-pair? frame-stx))
                (report "unexpected term" context (stx-car frame-stx) #f))
              ((expect:disj? frame-expect)
                (report (prose-for-expects (expect:disj-expects frame-expect)) context frame-stx within-stx))
              (else (report (prose-for-expects (list frame-expect)) context frame-stx within-stx)))))))

  ;; How a report names a datum, the default configuration of report-config.rkt. Racket has no ~literal
  ;; here, so the literal-to-* entries of the configuration are not needed.
  (define (datum-to-what v) (if (symbol? v) '("literal symbol" "literal symbols") '("literal" "literals")))
  (define (datum-to-string v)
    (if (symbol? v) (string-append "`" (racket-write->string v) "'") (racket-write->string v)))

  ;; prose-for-expects : (Listof Expect) -> String, cf. runtime-report.rkt:632
  (define (prose-for-expects expects)
    (let* ((msgs (filter expect:message? expects))
           (things (filter expect:thing? expects))
           (atom (filter expect:atom? expects))
           (atom-whats (map datum-to-what (map expect:atom-atom atom)))
           (what-vss (group-by-what atom atom-whats))
           (proper-pairs (filter expect:proper-pair? expects)))
      (join-sep (append (map (prose-for-expect "thing") (append msgs things))
                        (concatenate (map (λ (what-vs) (prose-for-expects/literals (cdr what-vs) (car what-vs))) what-vss))
                        (prose-for-expects/pairs proper-pairs))
                ";" "or")))

  ;; prose-for-expects/literals : (Listof Expect) What -> (Listof String), cf. runtime-report.rkt:651
  (define (prose-for-expects/literals expects what)
    (cond
      ((null? expects) '())
      ((singleton? expects) (map (prose-for-expect what) expects))
      (else (list (string-append "expected one of these " (plural what) ": "
                                 (join-sep (map (λ (e) (datum-to-string (expect:atom-atom e))) expects) "," "or"))))))

  ;; prose-for-expects/pairs : (Listof expect:proper-pair) -> (Listof String), cf. runtime-report.rkt:665
  (define (prose-for-expects/pairs expects)
    (if (pair? expects) (list (prose-for-proper-pair-expects expects)) '()))

  ;; prose-for-expect : What -> Expect -> String, cf. runtime-report.rkt:669
  (define ((prose-for-expect what) e)
    (match/first e
      (#(expect:thing _ ,description _ ,role _)
        (if role (format "expected ~a for ~a" description role) (format "expected ~a" description)))
      (#(expect:atom ,atom _) (format "expected the ~a ~a" (singular what) (datum-to-string atom)))
      (#(expect:message ,message _) message)
      (#(expect:proper-pair #f _) "expected more terms")))

  ;; prose-for-proper-pair-expects : (Listof expect:proper-pair) -> String, cf. runtime-report.rkt:689
  (define (prose-for-proper-pair-expects es)
    (let1 (descs (remove-duplicates* (map expect:proper-pair-first-desc es)))
      (if (any not descs)
          "expected more terms"
          (format "expected more terms starting with ~a" (join-sep (map prose-for-first-desc descs) "," "or")))))

  ;; prose-for-first-desc : FirstDesc -> String, cf. runtime-report.rkt:701
  (define (prose-for-first-desc desc)
    (match/first desc
      ((,s ⊣ (string? s)) s)
      ((any) "any term")
      ((datum ,d) (format "the ~a ~a" (singular (datum-to-what d)) (datum-to-string d)))))

  ;; context-prose-for-expect : (U '... expect:thing) -> (Listof String), a frame of the parsing context
  ;; with its term and location, cf. runtime-report.rkt:714
  (define (context-prose-for-expect e)
    (match/first e
      ((,marker ⊣ (eq? marker '...)) (list "while parsing different things..."))
      (#(expect:thing #f ,description _ ,role ,stx+index)
        (receive (stx _within-stx) (stx+index->at+within stx+index)
          (list (string-append "while parsing " description (if role (string-append " for " role) ""))
                (string-append " term: " (error-syntax->string stx 50))
                (string-append " location: " (source-location->string (syntax->srcloc* stx))))))))

  (define (singular what/s) (if (pair? what/s) (car what/s) what/s))
  (define (plural what/s) (if (pair? what/s) (cadr what/s) what/s))

  ;; group-by-what : (Listof Expect) (Listof What) -> (Listof (cons What (Listof Expect))),
  ;; cf. runtime-report.rkt:732
  (define (group-by-what vs whats)
    (map (λ (l) (cons (caar l) (map cdr l))) (group-by car (map cons whats vs))))

  ;; join-sep : (Listof String) String String -> (U String #f), "a, b, or c", cf. kws.rkt:114
  (define (join-sep items sep0 ult0)
    (let ((sep (string-append sep0 " ")) (ult (string-append ult0 " ")))
      (define (loop items)
        (cond
          ((null? items) '())
          ((null? (cdr items)) (list sep ult (car items)))
          (else (cons sep (cons (car items) (loop (cdr items)))))))
      (case (length items)
        ((0) #f)
        ((1) (car items))
        ((2) (format "~a~a ~a" (car items) ult (cadr items)))
        (else (apply string-append (car items) (loop (cdr items)))))))

  ;;; ==================== RAISING ====================

  ;; The Srcloc of a syntax object, with its source as it is, so that a syntax object without a location
  ;; prints as the empty string, as syntax/srcloc prints it.
  (define (syntax->srcloc* stx)
    (list (syntax-source stx) (syntax-line stx) (syntax-column stx) (syntax-position stx) (syntax-span stx)))

  ;; The srclocs of an exn:fail:syntax, as Racket's error display gets them from its exprs, without the
  ;; syntax objects that have no source, which the display leaves out (checked with Racket).
  (define (exn-srclocs exprs)
    (map syntax->srcloc* (filter syntax-source exprs)))

  ;; error-syntax->string : Syntax Natural -> String, the default error-syntax->string-handler, which writes
  ;; the datum and cuts it at the width.
  (define (error-syntax->string stx width)
    (racket-truncate (racket-write->string (syntax->datum stx)) width))

  ;; error/report : (list Symbol/#f Syntax) Report -> Nothing, cf. runtime-report.rkt:739. The message names
  ;; who, says what was expected, quotes the terms at, within and in, and lists the parsing context; it
  ;; starts with the location of the whole term. The srcloc is within's, else at's, else the term's.
  (define (error/report ctx report)
    (let* ((message (report-message report))
           (context (report-context report))
           (stx (cadr ctx))
           (who (or (car ctx) (infer-who stx)))
           (sub-stx (report-stx report))
           (within-stx (report-within-stx report))
           (message (format "~a: ~a~a~a~a~a"
                            who message
                            (format-if "at" (stx-if-loc sub-stx))
                            (format-if "within" (stx-if-loc within-stx))
                            (format-if "in" (stx-if-loc stx))
                            (if (null? context)
                                ""
                                (apply string-append "\n  parsing context: "
                                       (map (λ (c) (string-append "\n   " c)) context)))))
           (source-stx (or stx sub-stx within-stx))
           (message (string-append (source-location->prefix (syntax->srcloc* source-stx)) message)))
      (raise-racket-exn 'exn:fail:syntax
                        message
                        (exn-srclocs (cond (within-stx (list within-stx))
                                           (sub-stx (list sub-stx))
                                           (stx (list stx))
                                           (else '()))))))

  ;; format-if : String (U String #f) -> String, "\n  PREFIX: VAL" with the later lines of VAL indented under
  ;; it, cf. runtime-report.rkt:771
  (define (format-if prefix val)
    (if val
        (let1 (indent (make-string (+ 4 (string-length prefix)) #\space))
          (format "\n  ~a: ~a" prefix (string-translate* val (list (cons "\n" (string-append "\n" indent))))))
        ""))

  ;; stx-if-loc : (U Syntax #f) -> (U String #f), cf. runtime-report.rkt:777
  (define (stx-if-loc stx)
    (and (syntax? stx) (error-syntax->string stx 256)))

  ;; infer-who : Syntax -> Symbol, the head identifier of the term, or the term itself when it is one,
  ;; or ?, cf. runtime-report.rkt:782
  (define (infer-who stx)
    (let1 (maybe-id (if (stx-pair? stx) (stx-car stx) stx))
      (if (identifier? maybe-id) (syntax-e maybe-id) '?)))

  ;; raise-syntax-error : (U Symbol #f) String Syntax [Syntax] -> Nothing, racket/base's raise-syntax-error
  ;; as the REPL of main.rkt:248 calls it. The message is "LOC: WHO: MESSAGE", then at: and in:, where LOC
  ;; is the location of sub-stx, else of stx, and WHO is who, else the head identifier of stx, else ?. Its
  ;; srcloc is sub-stx's, else stx's (checked with Racket).
  (define (raise-syntax-error who message stx #!optional (sub-stx #f))
    (define (form-name s)
      (cond
        ((identifier? s) (syntax-e s))
        ((and (syntax? s) (pair? (syntax-e s)) (identifier? (car (syntax-e s)))) (syntax-e (car (syntax-e s))))
        (else #f)))
    (define (source-location s)
      (and (syntax? s) (string? (syntax-source s))
           (string-append (srcloc->string/relative (syntax->srcloc* s)) ": ")))
    (raise-racket-exn 'exn:fail:syntax
                      (string-append (or (source-location sub-stx) (source-location stx) "")
                                     (format "~a" (or who (form-name stx) '?)) ": " message
                                     (if sub-stx (string-append "\n  at: " (error-syntax->string sub-stx 256)) "")
                                     (if stx (string-append "\n  in: " (error-syntax->string stx 256)) ""))
                      (exn-srclocs (cond ((syntax? sub-stx) (list sub-stx)) ((syntax? stx) (list stx)) (else '()))))))
