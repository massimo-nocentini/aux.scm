
(import
  scheme (chicken base) (chicken memory representation) (chicken sort) (chicken string)
  srfi-1
  (aux unittest) (aux base) (aux stream) (aux kanren micro) (aux kanren micro show))

(define-relation (append° r s rs)
  (cond°
    ((null° r) (=° s rs))
    ((fresh° (a d c) (cons° a d r) (append° d s c) (cons° a c rs)))))

(define-relation (peano° n) (or° (=° n 'z) (fresh° (r) (=° n `(s ,r)) (peano° r))))

; The sample database of employees taken from
; https://www.postgresql.org/docs/current/tutorial-window.html.
(define-relation (empsalary° depname empno salary)
  (or°  (and° (=° depname 'develop)   (=° empno 7)  (=° salary 4200))
        (and° (=° depname 'develop)   (=° empno 8)  (=° salary 6000))
        (and° (=° depname 'develop)   (=° empno 9)  (=° salary 4500))
        (and° (=° depname 'develop)   (=° empno 10) (=° salary 5200))
        (and° (=° depname 'develop)   (=° empno 11) (=° salary 5200))
        (and° (=° depname 'personnel) (=° empno 2)  (=° salary 3900))
        (and° (=° depname 'personnel) (=° empno 5)  (=° salary 3500))
        (and° (=° depname 'sales)     (=° empno 1)  (=° salary 5000))
        (and° (=° depname 'sales)     (=° empno 3)  (=° salary 4800))
        (and° (=° depname 'sales)     (=° empno 4)  (=° salary 4800))))

; The quine relation of `microkanren-untagged.scm`, the smallest query we have whose answer
; carries a non-trivial constraint store.
(define (not-in-env° x env)
  (cond°
    ((=° '() env))
    ((fresh° (y v rest) (=° `((,y . ,v) . ,rest) env) (≠° y x) (not-in-env° x rest)))))

(define (proper-list° exp env val)
  (cond°
    ((=° '() exp) (=° '() val))
    ((fresh° (a d v-a v-d)
      (=° `(,a . ,d) exp) (=° `(,v-a . ,v-d) val)
      (eval-exp° a env v-a) (proper-list° d env v-d)))))

(define (lookup° x env t)
  (fresh° (y v rest)
    (=° `((,y . ,v) . ,rest) env)
    (cond° ((=° y x) (=° v t)) ((≠° y x) (lookup° x rest t)))))

(define (eval-exp° exp env val)
  (cond°
    ((fresh° (v) (=° `(quote ,v) exp) (not-in-env° 'quote env) (absent° 'closure v) (=° v val)))
    ((fresh° (a*) (=° `(list . ,a*) exp) (not-in-env° 'list env) (absent° 'closure a*) (proper-list° a* env val)))
    ((symbol° exp) (lookup° exp env val))
    ((fresh° (rator rand x body envˆ a)
      (=° `(,rator ,rand) exp)
      (eval-exp° rator env `(closure ,x ,body ,envˆ))
      (eval-exp° rand env a)
      (eval-exp° body `((,x . ,a) . ,envˆ) val)))
    ((fresh° (x body)
      (=° `(λ (,x) ,body) exp) (symbol° x) (not-in-env° 'λ env)
      (=° `(closure ,x ,body ,env) val)))))

(define-suite microkanren-show-suite

  ((doc r)
   `((p "The engine answers a query with a " (i "reified") " λ-expression: faithful, but the cons "
        "spine hides the shape of the term and the constraint store shows up as executable assertions. "
        (code/inline "(aux kanren micro show)") " splits that answer in a " (i "datum") " and a list of "
        (i "constraints") ", then renders both as a table, as a Mermaid graph of the term (where "
        "hash-consing makes sharing visible) and, optionally, as a profile of how many suspensions "
        "the engine had to force before each answer showed up.")))

  ; the answer, taken apart --------------------------------------------------------

  ((test/repr->datum _)
   (⊦= '(α z) (μkanren-repr->datum '(cons α (cons (quote z) (quote ())))))
   (⊦= 42 (μkanren-repr->datum 42))
   (⊦= '#(α 3) (μkanren-repr->datum '(vector α 3))))

  ((test/form->constraint _)
   (⊦= '(≠ α list) (μkanren-form->constraint '(begin (deny (equal? α (quote list))))))
   (⊦= '(≠ (α β) (list quote)) (μkanren-form->constraint '(begin (deny (equal? α (quote list)))
                                                                 (deny (equal? β (quote quote))))))
   (⊦= '(symbol? α) (μkanren-form->constraint '(assert (every (μ v (symbol? v)) (list α)))))
   (⊦= '(absento closure α) (μkanren-form->constraint '(assert (absent? (quote closure) α)))))

  ((test/occurrences _)
   (⊦= 2 (μkanren-datum-occurrences '((α z) z (α β)) 'α))
   (⊦= 1 (μkanren-datum-occurrences '((α z) z (α β)) 'β))
   (⊦= 0 (μkanren-datum-occurrences '((α z) z (α β)) 'γ)))

  ((test/answer _)
   (let1 (a (car (°->answers (fresh° (q) (=° q (list 'z 'z))))))
     (⊦= '(z z) (μkanren-answer->datum a))
     (⊦= '() (μkanren-answer->constraints a))
     (⊨ (μkanren-answer-ground? a))))

  ; the answer, shown --------------------------------------------------------------

  ((test/append° _)
   (let1 (answers (°->answers (take° 5 (fresh° (l) (fresh° (a d) (append° a d l))))))
     (⊦= '(α (α . β) (α β . γ) (α β γ . δ) (α β γ δ . ε)) (map μkanren-answer->datum answers)))
   `(doc (p "Two altitudes, from the same answers. The " (code/inline "variables") " view keeps a value "
            "whole and hangs the unknowns it is still free in off it, labelling each edge with how many "
            "times that unknown occurs; the " (code/inline "cells") " view opens the term up into the DAG "
            "it really is, where equal subterms are hash-consed so that " (i "sharing is visible") ".")
         ,@(μkanren-run/sxml (l 5) (fresh° (a d) (append° a d l)))))

  ((test/peano°/shared _)
   (let1 (answers (°->answers (take° 6 (fresh° (n) (peano° n)))))
     (⊦= '(z (s z) (s (s z)) (s (s (s z))) (s (s (s (s z)))) (s (s (s (s (s z))))))
         (map μkanren-answer->datum answers))
     `(doc (p "With " (code/inline "share: 'all") " every " (i "ground") " subterm is shared by the whole "
              "answer set, so a family such as the Peano numerals collapses into a single spine that "
              "each answer points into: the diagram grows linearly, not quadratically.")
           ,@(μkanren-answers->sxml answers share: 'all))))

  ((test/variables-view _)
   (let* ((answers (°->answers (take° 2 (fresh° (q) (eval-exp° q '() q)))))
          (g (μkanren-answers->mermaid answers view: 'variables var: 'q)))
     (⊨ (and (substring-index "((\"q\"))" g) #t))         ; the query variable is the root
     (⊨ (and (substring-index "|×6|" g) #t))               ; α occurs six times in the first quine
     (⊨ (and (substring-index "{{\"(symbol? α β)\"}}" g) #t))))  ; a constraint over two variables

  ((test/sharing _)
   (let1 (a (car (°->answers (fresh° (r) (fresh° (n q x)
                                           (=° q 'z)
                                           (fresh° (w r*) (=° n (list w q)) (=° x (list w r*)))
                                           (=° r (list n q x)))))))
     (⊦= '((α z) z (α β)) (μkanren-answer->datum a))
     `(doc (p "The variable " (code/inline "α") " occurs in two different components of the answer: "
              "in the graph it is " (i "one") " node with two parents.")
           ,@(μkanren-answers->sxml (list a)))))

  ; constraints ---------------------------------------------------------------------

  ((test/quine _)
   (match1/first ((,answers ,profile) (°->answers/profiled (take° 1 (fresh° (q) (eval-exp° q '() q)))))
     (let ((a (car answers)))
       (⊦= '((≠ α list) (≠ α quote) (symbol? α)) (μkanren-answer->constraints a))
       (⊦= '((λ (α) (list α (list (quote quote) α))) (quote (λ (α) (list α (list (quote quote) α)))))
           (μkanren-answer->datum a))
       `(doc (p "The smallest quine of the relational interpreter. An answer too wide for a table cell "
                "gets a block of its own, pretty-printed with the reader's abbreviations — "
                (code/inline "'quote") ", not " (code/inline "(quote quote)") ". Grounding it with "
                (code/inline "°->list/ground") " would silently drop its three constraints; here they "
                "stay, under the term and as chips on the variable they restrict. "
                "The bar chart counts the suspensions the engine forced before handing out the answer.")
             ,@(μkanren-answers->sxml answers profile: profile)))))

  ((test/blocks _)
   (let1 (md (°->markdown (take° 1 (fresh° (q) (eval-exp° q '() q))) title: "quine" var: 'q diagram: #f))
     (⊨ (and (substring-index "### answer 1" md) #t))
     (⊨ (and (substring-index "((λ (α) (list α (list 'quote α)))" md) #t))   ; pretty-printed, abbreviated
     (⊨ (and (substring-index "constraints: `(≠ α list)`" md) #t))))

  ; tuples ---------------------------------------------------------------------------

  ((test/window° _)
   (let1 (answers (°->answers (fresh° (r) (fresh° (d e s)
                                            (window° (((s* foldr/avg) s)) over (d) from (empsalary° d e s)
                                              => (=° r `(,d ,e ,s ,s*)))))))
     (⊦= 10 (length answers))
     (⊨ (every μkanren-answer-ground? answers))
     `(doc (p "When every answer is a ground tuple of the same length the report spreads it over one "
              "column per field, which is what a relational query deserves: this is the "
              (cite/a "https://www.postgresql.org/docs/current/tutorial-window.html" "PostgreSQL window function")
              " example, computed by " (code/inline "window°") ".")
           ,@(μkanren-answers->sxml answers headers: '(depname empno salary avg) diagram: #f))))

  ; the Markdown report ----------------------------------------------------------------

  ((test/markdown _)
   (let1 (md (μkanren-run/markdown (l 4) (fresh° (a d) (append° a d l))))
     (⊨ (string? md))
     (⊨ (and (substring-index "| 2 | `(α . β)` |" md) #t))
     (⊨ (and (substring-index "```mermaid" md) #t))
     (⊨ (and (substring-index "classDef query" md) #t))   ; the variables view
     (⊨ (and (substring-index "|car|" md) #t))            ; and the cells view
     (μkanren-markdown->file! md "microkanren-show")))
)

(unittest/✓ microkanren-show-suite)
