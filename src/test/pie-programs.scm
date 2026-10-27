;;; Whole Pie programs run by (aux pie), the port of Pie, the language of "The Little Typer" by Daniel P.
;;; Friedman and David Thrane Christiansen (https://github.com/the-little-typer/pie, commit 2c89553). Pie
;;; is distributed under the GNU AGPL-3.0, and so is this file, which is derived from it.
;;;
;;; The programs live in test/pie/ and run in-process with pie-run-file, from inside that directory as
;;; `racket FILE.pie` runs them, because the paths in the reports are relative to the current directory.
;;; Every NAME.pie of test/pie comes with NAME.out, NAME.err and NAME.status, what `racket NAME.pie`
;;; (Racket 8.15, pie 2c89553) printed on stdout and stderr and its exit status, recorded by running it
;;; from inside the directory. In them the absolute directory of the programs is written <dir> and the
;;; upstream package directory <pie>, and the "  context...:" block of Racket's stderr (its backtrace,
;;; with installation paths) is left out; the port never prints that block. There is one case per prefix
;;; of the file names (err, parse, read, todo, ...), which compares every program of the group.
;;; The files absolute-NAME.out, .err and .status are what `racket <dir>/NAME.pie` printed when run from
;;; test/, the parent of the programs' directory, with the absolute path of the file.
;;; The files fix-NAME.out, .err and .status are not Racket's but the port's: they record where the port
;;; departs from upstream on purpose, the check that the function of a cong has a non-dependent type.
;;; The case test/todo-output ports test-todo-output.rkt. It runs todo-test.pie, a copy of upstream's,
;;; and compares what it prints with the literal string of test-todo-output.rkt:12, after the rewriting
;;; of locations that upstream does.

(import scheme
        (chicken base)
        (only (chicken format) format)
        (only (chicken port) with-output-to-string with-error-output-to-string)
        (only (chicken io) read-string)
        (only (chicken file) glob)
        (only (chicken pathname) pathname-strip-directory pathname-strip-extension)
        (only (chicken process) process* process-wait process-input-port process-output-port process-error-port)
        (only (chicken sort) sort)
        (only srfi-1 filter)
        (only srfi-13 string-prefix?)
        (only (chicken string) string-translate* string-split)
        (only (chicken irregex) irregex-replace/all irregex-match-substring)
        (only (chicken process-context) current-directory change-directory get-environment-variables executable-pathname)
        (only (chicken file) delete-file)
        (only (chicken file posix) create-symbolic-link)
        (aux base)
        (aux unittest)
        (only (aux pie) pie-run-file))

;;; ==================== HELPERS ====================

; in-corpus : (-> A) -> A, calls thunk with test/pie as the current directory, and restores it.
(define (in-corpus thunk)
  (let1 (here (current-directory))
    (dynamic-wind
      (τ (change-directory "pie"))
      thunk
      (τ (change-directory here)))))

; file->string : path -> string, the whole content of a file.
(define (file->string path) (with-input-from-file path (τ (let1 (s (read-string)) (if (eof-object? s) "" s)))))

; golden : string string [string] -> (list string string string number), what Racket printed for NAME.pie,
; read from the files PREFIX NAME.out, .err and .status of the current directory, with <dir> replaced
; by dir, the physical path of test/pie.
(define (golden dir name #!optional (prefix ""))
  (let ((at-dir (λ (s) (string-translate* s (list (cons "<dir>" dir)))))
        (file (λ (extension) (file->string (string-append prefix name extension)))))
    (list name
          (at-dir (file ".out"))
          (at-dir (file ".err"))
          (string->number (car (string-split (file ".status")))))))

; ported : string [string] -> (list string string string number), what the port prints for NAME.pie,
; run in-process with pie-run-file, or for the file at path when it is given.
(define (ported name #!optional (path (string-append name ".pie")))
  (let* ((status #f)
         (err #f)
         (out (with-output-to-string
                (τ (set! err (with-error-output-to-string
                               (τ (set! status (pie-run-file path)))))))))
    (list name out err status)))

; corpus-group : string -> (list (listof result) (listof result)), the golden and the ported results of
; the programs whose names start with PREFIX-, in alphabetical order, kept only where they differ, so
; that a failure shows just the programs that differ.
(define (corpus-group prefix)
  (in-corpus
    (τ (let* ((dir (current-directory))
              (names (sort (filter (λ (n) (string-prefix? (string-append prefix "-") n))
                                   (map (o pathname-strip-extension pathname-strip-directory) (glob "*.pie")))
                           string<?))
              (pairs (map (λ (n) (cons (golden dir n) (ported n))) names)))
         (⊨ (pair? names))
         (let1 (differ (filter (λ (p) (not (equal? (car p) (cdr p)))) pairs))
           (⊦= (map car differ) (map cdr differ)))))))

; run-runner : string [alist] -> (list string number), the stdout and the exit status of `csi -s
; ../pie-run.scm FILE` run from test/pie by the csi that runs this file, with the environment of this process (CHICKEN_REPOSITORY_PATH
; included), where the given variables replace those of the same names.
(define (run-runner file #!optional (extra '()))
  (in-corpus
    (τ (let* ((inherited (filter (λ (binding) (not (assoc (car binding) extra))) (get-environment-variables)))
              (p (process* (executable-pathname)
                           (list "-s" "../pie-run.scm" file)
                           (append extra inherited))))
         ; the input port of the process is its stdin, its output port its stdout
         (close-output-port (process-input-port p))
         (let* ((stdout (read-string #f (process-output-port p)))
                (stderr (read-string #f (process-error-port p))))
           (let-values (((pid ok status) (process-wait p)))
             (list (if (eof-object? stdout) "" stdout) status)))))))

;;; ==================== TESTS ====================

(define-suite pie-programs-suite

  ((doc r) `((p "Whole Pie programs run by the port (aux pie), compared with what Racket Pie prints.")))

  ((test/todo-output _)
   ; cf. test-todo-output.rkt:5. Racket prints the location of a file inside the pie package as
   ; <pkgs>/pie/FILE, while the port prints the absolute path of test/pie/FILE, so that directory
   ; becomes <pkgs>/pie before upstream's rewriting of LINE:COL into LINE.COL.
   (let* ((dir (in-corpus current-directory))
          (out (in-corpus (τ (with-output-to-string (τ (pie-run-file "todo-test.pie")))))))
     (⊦= "<pkgs>/pie/todo-test.pie:10.15: TODO:\n           n : Nat\n         n-1 : Nat\n peas-of-n-1 : (Vec Atom n-1)\n------------------------------\n Atom\n\n\n<pkgs>/pie/todo-test.pie:10.20: TODO:\n           n : Nat\n         n-1 : Nat\n peas-of-n-1 : (Vec Atom n-1)\n------------------------------\n (Vec Atom n-1)\n\n\n<pkgs>/pie/todo-test.pie:13.17: TODO: Nat\n\n<pkgs>/pie/todo-test.pie:15.19: TODO: \n (Π ((n Nat))\n  (Vec Atom n))\n\n\n"
         (irregex-replace/all "todo-test\\.pie:(\\d+)(:|\\.)(\\d+)"
                              (string-translate* out (list (cons (string-append dir "/") "<pkgs>/pie/")))
                              (λ (m) (format "todo-test.pie:~a.~a"
                                             (irregex-match-substring m 1)
                                             (irregex-match-substring m 3)))))))

  ((test/err _)
   ; type errors, declaration errors and Pie errors after TODOs.
   (corpus-group "err"))

  ((test/expand _)
   ; the errors of Racket's expansion of the module, after the elaboration, cf. main.rkt:203, for a
   ; name internal-claim-binding-y defined twice and for a name #%datum around a literal.
   (corpus-group "expand"))

  ((test/layout _)
   ; CRLF and tab layout, and the columns of TODO reports.
   (corpus-group "layout"))

  ((test/order _)
   ; left-to-right reporting of the first malformed sub-term.
   (corpus-group "order"))

  ((test/parse _)
   ; the failure reports of syntax-parse.
   (corpus-group "parse"))

  ((test/pretty _)
   ; every clause of Pie's printer.
   (corpus-group "pretty"))

  ((test/printf _)
   ; the printf quirk of names with a tilde.
   (corpus-group "printf"))

  ((test/fix _)
   ; the deliberate departures from upstream, see non-dependent-codomain in (aux pie typechecker).
   (corpus-group "fix"))

  ((test/prog _)
   ; whole programs about lists, vectors, Either, Absurd, equality and TODOs.
   (corpus-group "prog"))

  ((test/programs _)
   ; the basic programs.
   (corpus-group "programs"))

  ((test/quirk _)
   ; the Pi and ∏ rewrites located in upstream parser.rkt.
   (corpus-group "quirk"))

  ((test/read _)
   ; the reader, the #lang line and files that are not modules.
   (corpus-group "read"))

  ((test/serial _)
   ; the dump and restore of contexts with odd variable names.
   (corpus-group "serial"))

  ((test/todo _)
   ; TODO reports, with and without free variables.
   (corpus-group "todo"))

  ((test/absolute _)
   ; a file given by its absolute path, from test/, so the locations under location...: are relative to
   ; test/ and the TODO reports and the error of a missing file name the file as given.
   (let* ((dir (in-corpus current-directory))
          (names '("err-e1" "todo-holes1" "read-r1" "nope"))
          (pairs (map (λ (n) (cons (in-corpus (τ (golden dir n "absolute-")))
                                   (ported n (string-append dir "/" n ".pie"))))
                      names))
          (differ (filter (λ (p) (not (equal? (car p) (cdr p)))) pairs)))
     (⊦= (map car differ) (map cdr differ))))

  ((test/runner _)
   ; the command-line runner test/pie-run.scm, the counterpart of `racket FILE.pie`. A program that works
   ; exits 0, and one with an error exits 1.
   (⊦= (list (file->string "pie/prog-lists.out") 0) (run-runner "prog-lists.pie"))
   (⊦= 1 (cadr (run-runner "err-e1.pie"))))

  ((test/runner-symlink _)
   ; from a directory reached through a symbolic link, Racket names the files by $PWD, the logical path,
   ; when it names the current directory. After a shell's `cd lnk` the physical directory is test/pie
   ; and $PWD is test/pie/lnk, where the link test/pie/lnk points to test/pie itself and lives only
   ; during the test.
   (let1 (dir (in-corpus current-directory))
     (in-corpus (τ (create-symbolic-link "." "lnk")))
     (dynamic-wind
       void
       (τ (⊦= (list (string-translate* (file->string "pie/absolute-todo-holes1.out")
                                       (list (cons "<dir>" (string-append dir "/lnk"))))
                    0)
              (run-runner "todo-holes1.pie" (list (cons "PWD" (string-append dir "/lnk"))))))
       (τ (in-corpus (τ (delete-file "lnk"))))))))

(unittest/✓ pie-programs-suite)
