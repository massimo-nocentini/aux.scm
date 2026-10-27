;;; (aux pie show-goal) -- TODO goals shown as inference rules.
;;;
;;; A port of show-goal.rkt of Pie, the language of "The Little Typer" by Daniel P. Friedman and David
;;; Thrane Christiansen (https://github.com/the-little-typer/pie, commit 2c89553). Pie is distributed under
;;; the GNU AGPL-3.0, and so is this file, which is derived from it.
;;;
;;; The file is ported whole, with the upstream names and exports (cf. show-goal.rkt:7). It shows a TODO in
;;; the format of the second recess of The Little Typer, that is the free variables of the context with
;;; their types, oldest first, then a bar of dashes, then the type of the hole.
;;;
;;; Racket's ~a displays its arguments, as CHICKEN's display does for Pie data, so a quoted atom in a
;;; hypothesis appears in full as (quote a) (oracle todo-atoms). The padding of ~a is racket-pad-left and
;;; racket/string's string-split is racket-string-split, both from (aux pie racket). string-join becomes
;;; string-intersperse of (chicken string).

(module (aux pie show-goal) (goal->strings indent-string)

  (import scheme
          (chicken base)
          (chicken port)
          (only (chicken string) string-intersperse)
          (only srfi-1 filter-map)
          (aux base)
          (aux pie racket)
          (aux pie resugar)
          (aux pie pretty))

  ;;; ==================== GOALS ====================

  ;; goal->strings : Loc Serializable-Ctx Core -> (List String String), the summary of the hole, which is
  ;; its type printed by pprint-pie, and its details, the hypotheses, the bar and the indented summary
  ;; joined by newlines. loc is unused, as upstream. cf. show-goal.rkt:12
  (define (goal->strings loc Γ t)
    (let* ((hole-summary
             (with-output-to-string
               (τ (pprint-pie (resugar t)))))
           (free-vars
             ; the entries (x (free ty)) of Γ, newest first, cf. show-goal.rkt:17
             (filter-map (λ (H)
                           (and (pair? H)
                                (pair? (cdr H))
                                (pair? (cadr H))
                                (eqv? (caadr H) 'free)
                                (match1/first ((,x (free ,ty)) H)
                                  (list x ty))))
                         Γ))
           (var-width
             (apply max 0
                    (map (λ (b) (string-length (symbol->string (car b))))
                         free-vars)))
           (hyps
             ; each hypothesis is the name padded on the left to one more than the widest, then " : "
             ; and the resugared type, displayed, cf. show-goal.rkt:29
             (map (λ (b)
                    (match1/first ((,x ,ty) b)
                      (let1 (padded-x (racket-pad-left (symbol->string x) (add1 var-width)))
                        (string-append
                          padded-x
                          " : "
                          (with-output-to-string (τ (display (resugar ty))))))))
                  free-vars))
           (conclusion
             (indent-string 1 hole-summary))
           (inference-bar
             (make-string
               (apply max 7
                      (+ 2 (max-line-length hole-summary))
                      ; the add1 makes the line extend at least one space past the width of the
                      ; premise, cf. show-goal.rkt:48
                      (map (λ (h) (add1 (max-line-length h))) hyps))
               #\-)))
      (list hole-summary
            (string-intersperse (append (reverse hyps)
                                        (list inference-bar
                                              conclusion))
                                "\n"))))

  ;;; ==================== STRINGS ====================

  ;; indent-string : Natural String -> String, every line of str prefixed by how-much spaces and
  ;; followed by a newline, cf. show-goal.rkt:59
  (define (indent-string how-much str)
    (let1 (pad (make-string how-much #\space))
      (apply string-append
             (map (λ (line) (string-append pad line "\n"))
                  (racket-string-split str "\n")))))

  ;; max-line-length : String -> Natural, the length of the longest line of str, 0 when it has none,
  ;; cf. show-goal.rkt:67
  (define (max-line-length str)
    (apply max 0
           (map string-length (racket-string-split str "\n")))))
