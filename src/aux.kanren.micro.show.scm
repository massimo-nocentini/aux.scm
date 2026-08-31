(module (aux kanren micro show) *

  ; A *presentation* layer for the μkanren engine defined in `(aux kanren micro)`.
  ;
  ; The engine answers a query with a *reified* λ-expression, eg.
  ;
  ;   (λ (α β) (begin (deny (equal? α 'list)))
  ;            (assert (every (μ v (symbol? v)) (list α)))
  ;            (cons α (cons 'z (cons β '()))))
  ;
  ; which is faithful but hard to read: the cons spine hides the shape of the term and the
  ; constraint store is spelled out as executable assertions.  Grounding it with `°->list/ground`
  ; reads better but *silently drops every constraint*.  Here we keep both: an answer is split in
  ; a `datum` (the term, in ordinary list notation) and a list of `constraints` (in relational
  ; notation), then rendered as Markdown, as a Mermaid diagram, or as SXML for the HTML test
  ; reports produced by `(aux unittest)`.

  (import scheme
          (chicken base)
          (chicken string)
          (chicken time)
          (chicken port)
          (chicken pretty-print)
          (chicken memory representation)
          srfi-1 srfi-69
          (aux base)
          (aux stream)
          (aux kanren micro))

  ; answers, as data ---------------------------------------------------------------

  ; An answer is the s-expression `(λ (var ...) form ... repr)` produced by `μkanren-project`.

  (define (μkanren-answer-vars a) (cadr a))            ; the fresh (unbound) variables it mentions.
  (define (μkanren-answer-repr a) (last a))            ; the reified term, as a constructor expression.
  (define (μkanren-answer-forms a) (drop-right (cddr a) 1))  ; the constraint store, as assertions.

  (define (μkanren-tagged? r tag) (and (pair? r) (eq? (car r) tag)))

  ; `(cons α (cons 'z '()))` ↦ `(α z)`, without `eval`uating anything.
  (define (μkanren-repr->datum r)
    (let D ((r r))
      (cond
        ((not (pair? r)) r)
        ((μkanren-tagged? r 'quote) (cadr r))
        ((μkanren-tagged? r 'cons) (cons (D (cadr r)) (D (caddr r))))
        ((μkanren-tagged? r 'vector) (list->vector (map D (cdr r))))
        ((μkanren-tagged? r 'make-record-instance) (apply make-record-instance (map D (cdr r))))
        (else r))))

  ; `(μ v (symbol? v))` ↦ `symbol?`, ie. the name of the predicate carried by a tag.
  (define (μkanren-tag-def->name def)
    (cond
      ((and (pair? def) (pair? (cddr def)) (pair? (caddr def))) (car (caddr def)))
      (else def)))

  ; `(begin (deny (equal? α 'list)))` ↦ `(≠ α list)`, `(assert (every ...))` ↦ `(symbol? α)`, ...
  (define (μkanren-form->constraint c)
    (cond
      ((μkanren-tagged? c 'begin)
        (let* ((equalities (map cadr (cdr c)))                             ; (equal? α repr)
               (ls (map cadr equalities))
               (rs (map (o μkanren-repr->datum caddr) equalities)))
          (if (one? (length ls)) `(≠ ,(car ls) ,(car rs)) `(≠ ,ls ,rs))))  ; a tuple disequality.
      ((μkanren-tagged? c 'assert)
        (let1 (b (cadr c))
          (cond
            ((μkanren-tagged? b 'every) (cons (μkanren-tag-def->name (cadr b)) (cdr (caddr b))))
            ((μkanren-tagged? b 'absent?) `(absento ,(cadr (cadr b)) ,(caddr b)))
            (else c))))
      (else c)))

  (define (μkanren-answer->datum a) (μkanren-repr->datum (μkanren-answer-repr a)))
  (define (μkanren-answer->constraints a) (map μkanren-form->constraint (μkanren-answer-forms a)))
  (define (μkanren-answer-ground? a) (and (null? (μkanren-answer-vars a)) (null? (μkanren-answer-forms a))))

  ; running -------------------------------------------------------------------------

  (define (°->answers g) (°->list #f g))

  ; Consume the answer stream by hand, timing each answer and counting how many suspensions the
  ; engine had to force to produce it: a cheap profile of *where* the search spends its time.
  (define (°->answers/profiled g)
    (let L ((§ (°->§ g)) (steps 0) (t (current-process-milliseconds)) (i 1) (answers '()) (profile '()))
      (cond
        ((promise? §) (L (force §) (add1 steps) t i answers profile))
        ((pair? §) (let1 (t* (current-process-milliseconds))
                     (L (cdr §) 0 t* (add1 i)
                        (cons (car §) answers)
                        (cons (list i steps (- t* t)) profile))))
        (else (list (reverse answers) (reverse profile))))))

  ; text utilities ------------------------------------------------------------------

  (define (μkanren-show-flatten ls)
    (cond
      ((null? ls) '())
      ((pair? ls) (append (μkanren-show-flatten (car ls)) (μkanren-show-flatten (cdr ls))))
      (else (list ls))))

  (define (μkanren-show-lines . ls) (string-intersperse (μkanren-show-flatten ls) "\n"))

  (define (->string/write v) (call-with-output-string (λ (p) (write v p))))

  ; The datum on one line, but with the reader's abbreviations: `(quote quote)` shows up as `'quote`,
  ; which is how a Schemer reads a term.
  (define (->string/short v)
    (let C ((cs (string->list (->string/pretty-print v))) (space #f) (acc '()))
      (cond
        ((null? cs) (list->string (reverse acc)))
        ((char-whitespace? (car cs)) (C (cdr cs) #t acc))
        (else (C (cdr cs) #f (cons (car cs) (if (and space (pair? acc)) (cons #\space acc) acc)))))))

  (define (μkanren-markdown-escape s) (string-translate* s '(("|" . "\\|") ("\n" . " "))))
  (define (μkanren-mermaid-escape s) (string-translate* s '(("&" . "#amp;") ("\"" . "#quot;") ("<" . "#lt;") (">" . "#gt;") ("\n" . " "))))

  (define (μkanren-markdown-code v) (conc "`" (μkanren-markdown-escape (->string/short v)) "`"))

  (define (μkanren-markdown-row cells) (conc "| " (string-intersperse cells " | ") " |"))

  (define (μkanren-markdown-table headers rows)
    (μkanren-show-lines
      (μkanren-markdown-row (map ->string headers))
      (μkanren-markdown-row (map (K "---") headers))
      (map μkanren-markdown-row rows)))

  ; Markdown -------------------------------------------------------------------------

  ; Do the answers form a relation, ie. is every one of them a ground tuple of the same length?
  ; That is the shape a query over a database of facts produces, and it deserves a real table.
  (define (μkanren-answers-tabular? answers data)
    (and (pair? data)
         (every μkanren-answer-ground? answers)
         (every list? data)
         (let1 (l (length (car data))) (and (< 1 l) (every (μ d (= l (length d))) data)))))

  ; Is any answer too wide to sit in a table cell?  Those want a block of their own instead.
  (define (μkanren-answers-wide? answers width)
    (any (μ a (< width (string-length (->string/short (μkanren-answer->datum a))))) answers))

  ; `layout` picks how the answers are laid out: `table` for one row each, `blocks` for one
  ; pretty-printed code block each, `auto` (the default) for a table until an answer outgrows
  ; `width` characters — a table cell is no place for a term that has to be read.  Tabular
  ; answers always get their columns.
  (define (μkanren-answers-layout answers data layout width)
    (cond
      ((μkanren-answers-tabular? answers data) 'tuples)
      ((eq? 'auto layout) (if (μkanren-answers-wide? answers width) 'blocks 'table))
      (else layout)))

  ; One row per answer: the term, then its constraints; or one column per field, `headers` naming
  ; them, when the answers are tabular.
  (define (μkanren-answers->markdown/table answers #!key (headers #f))
    (let* ((data (map μkanren-answer->datum answers))
           (cs (map μkanren-answer->constraints answers)))
      (cond
        ((null? answers) "_no answers_")
        ((μkanren-answers-tabular? answers data)
          (let1 (hs (or headers (map (μ i (conc "c" i)) (ι (length (car data))))))
            (μkanren-markdown-table (cons "#" hs)
              (map (λ (i d) (cons (->string i) (map μkanren-markdown-code d))) (ι (length data) 1) data))))
        (else
          (μkanren-markdown-table
            (if (every null? cs) '("#" "answer") '("#" "answer" "constraints"))
            (map (λ (i d c)
                   (let1 (row (list (->string i) (μkanren-markdown-code d)))
                     (if (every null? cs) row (append row (list (string-intersperse (map μkanren-markdown-code c) ", "))))))
              (ι (length data) 1) data cs))))))

  ; One section per answer: the term pretty-printed in a `scheme` block, its constraints under it.
  (define (μkanren-answers->markdown/blocks answers)
    (map (λ (i a)
           (let1 (cs (μkanren-answer->constraints a))
             (list (conc "### answer " i)
                   ""
                   (μkanren-fence "scheme" (string-chomp (->string/pretty-print (μkanren-answer->datum a)) "\n"))
                   ""
                   (if (null? cs)
                       '()
                       (list (conc "constraints: " (string-intersperse (map μkanren-markdown-code cs) ", ")) "")))))
      (ι (length answers) 1) answers))

  ; Mermaid ---------------------------------------------------------------------------

  (define (μkanren-mermaid-wrap s n)   ; break a long label every ~n characters, at a space.
    (let W ((cs (string->list s)) (col 0) (acc '()))
      (cond
        ((null? cs) (list->string (reverse acc)))
        ((and (< n col) (char=? #\space (car cs))) (W (cdr cs) 0 (append (reverse (string->list "<br/>")) acc)))
        (else (W (cdr cs) (add1 col) (cons (car cs) acc))))))

  (define (μkanren-mermaid-label s #!key (wrap #f))
    (let1 (escaped (μkanren-mermaid-escape s)) (if wrap (μkanren-mermaid-wrap escaped wrap) escaped)))

  (define (μkanren-mermaid-node id open label close) (conc "    " id open "\"" label "\"" close))
  (define (μkanren-mermaid-arrow from label to)
    (conc "    " from (if (string=? "" label) " --> " (conc " -->|" label "| ")) to))
  (define (μkanren-mermaid-dash from label to)
    (conc "    " from (if (string=? "" label) " -.-> " (conc " -. " label " .-> ")) to))

  (define (μkanren-mermaid-classes classified)
    (map (λ (cls) (let1 (members (map cdr (filter (λ (p) (equal? cls (car p))) classified)))
                    (if (null? members) '() (conc "  class " (string-intersperse members ",") " " cls))))
      '("query" "value" "var" "atom" "cell" "constraint" "answer")))

  (define μkanren-mermaid-classdefs
    '("  classDef query fill:#ebfbee,stroke:#2f9e44,stroke-width:2px;"
      "  classDef value fill:#ffffff,stroke:#495057;"
      "  classDef var fill:#fff3bf,stroke:#f08c00,stroke-width:2px;"
      "  classDef atom fill:#e7f5ff,stroke:#1971c2;"
      "  classDef cell fill:#f8f9fa,stroke:#868e96;"
      "  classDef constraint fill:#ffe3e3,stroke:#e03131,stroke-dasharray:4 3;"
      "  classDef answer fill:#ebfbee,stroke:#2f9e44,stroke-width:2px;"))

  (define (μkanren-datum-occurrences v α)   ; how many times does the variable α occur in v?
    (let O ((v v))
      (cond
        ((and (symbol? v) (eq? v α)) 1)
        ((pair? v) (+ (O (car v)) (O (cdr v))))
        ((vector? v) (O (vector->list v)))
        ((and (record-instance? v) (not (μkanren-var? v))) (O (record->vector v)))
        (else 0))))

  ; The high-level picture: what the query variable is *worth*, one value per answer, and which
  ; variables each value is still free in.  A value is written as a term, not exploded into cells;
  ; an edge to a variable is labelled with how many times that variable occurs in it, so aliasing
  ; ("the same unknown appears here and there") is the thing the diagram shows.  Every constraint
  ; becomes a chip attached to the variables it mentions.
  (define (μkanren-answers->mermaid/variables answers var max-diagrams)
    (let* ((shown (if (> (length answers) max-diagrams) (take answers max-diagrams) answers))
           (k 0)
           (decls '()) (edges '()) (classified '()))
      (define (id!) (begin1 (conc "n" k) (add1! k)))
      (define (decl! open label close cls)
        (let1 (id (id!))
          (push! (μkanren-mermaid-node id open label close) decls)
          (push! (cons cls id) classified)
          id))
      (define (edge! from label to) (push! (μkanren-mermaid-arrow from label to) edges))
      (define (dashed! from label to) (push! (μkanren-mermaid-dash from label to) edges))
      (define (answer! root i a)
        (let* ((vars (μkanren-answer-vars a))
               (datum (μkanren-answer->datum a))
               (value (decl! "[" (μkanren-mermaid-label (->string/short datum) wrap: 44) "]" "value"))
               (ids (map (λ (α) (cons α (decl! "((" (μkanren-mermaid-label (->string α)) "))" "var"))) vars)))
          (edge! root (->string i) value)
          (for-each (λ (α) (edge! value (conc "×" (μkanren-datum-occurrences datum α)) (cdr (assq α ids)))) vars)
          (for-each (λ (c)
                      (let ((chip (decl! "{{" (μkanren-mermaid-label (->string/write c) wrap: 30) "}}" "constraint"))
                            (mentioned (filter (λ (α) (member? α (μkanren-show-flatten c))) vars)))
                        (for-each (λ (α) (dashed! (cdr (assq α ids)) "" chip)) mentioned)))
            (μkanren-answer->constraints a))))
      (let1 (root (decl! "((" (μkanren-mermaid-label (->string var)) "))" "query"))
        (for-each (λ (i a) (answer! root i a)) (ι (length shown) 1) shown)
        (μkanren-show-lines
          "flowchart LR"
          (reverse decls)
          (reverse edges)
          (μkanren-mermaid-classes classified)
          μkanren-mermaid-classdefs))))

  ; The low-level picture: the answer term drawn as the DAG it really is, where cons cells are
  ; ⟦cons⟧ nodes, logic variables are circles, atoms are boxes, and *sharing is visible* because
  ; equal subterms are hash-consed to a single node.  Constraints hang off the variables they
  ; restrict as dashed edges.
  ;
  ; `share` says how far hash-consing goes: with `answer` (the default) each answer is a subgraph of
  ; its own; with `all` every *ground* subterm is shared by the whole answer set, so a family of
  ; answers such as the Peano numbers collapses into one spine that each answer points into.  Terms
  ; mentioning variables are never shared across answers, since `α` of one answer is not `α` of another.
  (define (μkanren-answers->mermaid/cells answers share max-diagrams)
    (let* ((shown (if (> (length answers) max-diagrams) (take answers max-diagrams) answers))
           (k 0)
           (ids (make-hash-table))
           (decls '()) (edges '()) (classified '()))
      (define (id!) (begin1 (conc "n" k) (add1! k)))
      (define (decl! open label close cls)
        (let1 (id (id!))
          (push! (μkanren-mermaid-node id open (μkanren-mermaid-label label) close) decls)
          (push! (cons cls id) classified)
          id))
      (define (edge! from label to) (push! (μkanren-mermaid-arrow from label to) edges))
      (define (dashed! from label to) (push! (μkanren-mermaid-dash from label to) edges))
      (define (flush!) (begin1 (list (reverse decls) (reverse edges)) (set! decls '()) (set! edges '())))
      (define (ground? vars v)
        (cond
          ((symbol? v) (not (member? v vars)))
          ((pair? v) (and (ground? vars (car v)) (ground? vars (cdr v))))
          ((vector? v) (ground? vars (vector->list v)))
          ((and (record-instance? v) (not (μkanren-var? v))) (ground? vars (record->vector v)))
          (else #t)))
      (define ((node i vars) v)
        (let* ((N (node i vars))
               (key (if (and (eq? share 'all) (ground? vars v)) (cons 'ground v) (cons i v))))
          (cond
            ((hash-table-exists? ids key) (hash-table-ref ids key))
            (else
              (let1 (id (cond
                          ((and (symbol? v) (member? v vars)) (decl! "((" (->string v) "))" "var"))
                          ((null? v) (decl! "([" "()" "])" "atom"))
                          ((pair? v) (let1 (id (decl! "[[" "cons" "]]" "cell"))
                                       (edge! id "car" (N (car v)))
                                       (edge! id "cdr" (N (cdr v)))
                                       id))
                          ((vector? v) (let1 (id (decl! "[[" "vector" "]]" "cell"))
                                         (for-each (λ (j w) (edge! id (conc j) (N w))) (ι (vector-length v)) (vector->list v))
                                         id))
                          ((and (record-instance? v) (not (μkanren-var? v)))
                            (let* ((rv (record->vector v))
                                   (id (decl! "[[" (conc "record " (vector-ref rv 0)) "]]" "cell")))
                              (for-each (λ (w) (edge! id "" (N w))) (cdr (vector->list rv)))
                              id))
                          ((symbol? v) (decl! "[" (conc "'" v) "]" "atom"))
                          (else (decl! "[" (->string/write v) "]" "atom"))))
                (hash-table-set! ids key id)   ; hash-consing: an equal subterm is drawn once.
                id)))))
      (define ((constraint i vars) c)
        (let1 (N (node i vars))
          (match/first c
            ((≠ ,ls ,rs) (cond   ; a tuple disequality denies the *conjunction* of its equalities.
                           ((list? ls) (for-each (λ (l r) (dashed! (N l) "≠ (tuple)" (N r))) ls rs))
                           (else (dashed! (N ls) "≠" (N rs)))))
            ((absento ,tag ,α) (dashed! (N α) "" (decl! "{{" (conc "absento " tag) "}}" "constraint")))
            ((,pred . ,αs) (let1 (id (decl! "{{" (->string pred) "}}" "constraint"))
                             (for-each (λ (α) (dashed! (N α) "" id)) αs)))
            (else (void)))))
      (define (answer! i a)
        (let1 (vars (μkanren-answer-vars a))
          (let1 (root ((node i vars) (μkanren-answer->datum a)))
            (for-each (constraint i vars) (μkanren-answer->constraints a))
            (when (eq? share 'all) (edge! (decl! "(" (conc "answer " i) ")" "answer") "" root))
            root)))
      (define (subgraph! i a)
        (answer! i a)
        (match1/first ((,ds ,es) (flush!))
          (list (conc "  subgraph a" i "[\"answer " i "\"]") "    direction LR" ds es "  end")))
      (let1 (is (ι (length shown) 1))
        (μkanren-show-lines
          "flowchart LR"
          (cond
            ((eq? share 'all) (begin (for-each answer! is shown) (flush!)))
            (else (map subgraph! is shown)))
          (μkanren-mermaid-classes classified)
          μkanren-mermaid-classdefs))))

  ; `view` picks the altitude: `variables` for the values the query takes and the unknowns they are
  ; still free in, `cells` for the term as a graph of cons cells.
  (define (μkanren-answers->mermaid answers #!key (max-diagrams 8) (share 'answer) (view 'variables) (var 'q))
    (match/first view
      (cells (μkanren-answers->mermaid/cells answers share max-diagrams))
      (else (μkanren-answers->mermaid/variables answers var max-diagrams))))

  ; `diagram` is `#t` for both views, `#f` for none, or one view (or a list of them).
  (define (μkanren-views diagram)
    (cond
      ((eq? #t diagram) '(variables cells))
      ((not diagram) '())
      ((pair? diagram) diagram)
      (else (list diagram))))

  ; How much work did the engine do to hand us each answer?
  (define (μkanren-profile->mermaid profile)
    (μkanren-show-lines
      "xychart-beta"
      "  title \"suspensions forced per answer\""
      (conc "  x-axis \"answer\" [" (string-intersperse (map (λ (p) (conc "\"" (car p) "\"")) profile) ", ") "]")
      "  y-axis \"forced\""
      (conc "  bar [" (string-intersperse (map (o ->string cadr) profile) ", ") "]")))

  (define (μkanren-fence lang body) (μkanren-show-lines (conc "```" lang) body "```"))

  ; the whole report -----------------------------------------------------------------

  (define (μkanren-answers->markdown answers
                                     #!key (query #f) (title "μKanren query") (headers #f) (var 'q)
                                           (layout 'auto) (width 48)
                                           (diagram #t) (max-diagrams 8) (share 'answer) (profile #f))
    (μkanren-show-lines
      (conc "## " title)
      ""
      (if query (list (μkanren-fence "scheme" (string-chomp (->string/pretty-print query) "\n")) "") '())
      (conc "**" (length answers) "** answer" (if (one? (length answers)) "" "s") ".")
      ""
      (if (eq? 'blocks (μkanren-answers-layout answers (map μkanren-answer->datum answers) layout width))
          (μkanren-answers->markdown/blocks answers)
          (list (μkanren-answers->markdown/table answers headers: headers) ""))
      (if (pair? answers)
          (map (λ (view)
                 (list (μkanren-fence "mermaid" (μkanren-answers->mermaid answers view: view var: var
                                                                          max-diagrams: max-diagrams share: share))
                       ""))
            (μkanren-views diagram))
          '())
      (if (pair? profile)
          (list (μkanren-fence "mermaid" (μkanren-profile->mermaid profile))
                ""
                (μkanren-markdown-table '("#" "forced" "ms")
                                        (map (μ p (map ->string p)) profile))
                "")
          '())))

  (define (°->markdown g #!key (query #f) (title "μKanren query") (headers #f) (var 'q)
                                (layout 'auto) (width 48)
                                (diagram #t) (max-diagrams 8) (share 'answer) (profile #f))
    (match1/first ((,answers ,prof) (if profile (°->answers/profiled g) (list (°->answers g) '())))
      (μkanren-answers->markdown answers query: query title: title headers: headers var: var
                                 layout: layout width: width
                                 diagram: diagram max-diagrams: max-diagrams share: share profile: prof)))

  (define (μkanren-markdown->file! markdown filename)
    (with-output-to-file (conc filename ".md") (τ (display markdown) (newline))))

  ; `(μkanren-run/markdown (q 10) g ...)` mirrors `μkanren-run`, answering with a Markdown report
  ; that quotes the query itself.
  (define-syntax-rule (μkanren-run/markdown (v n) g ...)
    (°->markdown (take° n (fresh° (v) g ...)) query: '(μkanren-run (v n) g ...) var: 'v))

  ; `headers` names the columns when the answers are ground tuples, as in a relational query.
  (define-syntax-rule (μkanren-run/markdown/table (v n) headers g ...)
    (°->markdown (take° n (fresh° (v) g ...)) query: '(μkanren-run (v n) g ...) var: 'v headers: headers diagram: #f))

  (define-syntax-rule (μkanren-run/markdown/profiled (v n) g ...)
    (°->markdown (take° n (fresh° (v) g ...)) query: '(μkanren-run (v n) g ...) var: 'v profile: #t))

  (define-syntax-rule (μkanren-run/display (v n) g ...) (display (μkanren-run/markdown (v n) g ...)))

  ; SXML, for the HTML reports of `(aux unittest)` -------------------------------------

  (define (μkanren-answers->sxml answers #!key (headers #f) (var 'q) (layout 'auto) (width 48)
                                         (diagram #t) (max-diagrams 8) (share 'answer) (profile '()))
    (let* ((data (map μkanren-answer->datum answers))
           (cs (map μkanren-answer->constraints answers))
           (is (ι (length data) 1))
           (how (μkanren-answers-layout answers data layout width))
           (tabular? (and headers (eq? 'tuples how)))
           (cell (λ (v) `(td (code/inline ,(->string/short v)))))
           (constraints (λ (c) (if (null? c) '() `((p "constraints: " (code/inline ,(string-intersperse (map ->string/write c) ", ")))))))
           (block (λ (i d c) `((h3 "answer " ,i) (code/scheme ,d) ,@(constraints c)))))
      `((p (b ,(length answers)) " answer" ,(if (one? (length answers)) "" "s") ".")
        ,@(if (eq? 'blocks how)
              (foldr/concat (map block is data cs))
              `((table (@ (class "w3-table w3-bordered w3-small"))
                  (tr (th "#") ,@(cond
                                   (tabular? (map (μ h `(th ,h)) headers))
                                   ((every null? cs) '((th "answer")))
                                   (else '((th "answer") (th "constraints")))))
                  ,@(map (λ (i d c)
                           `(tr (td ,i)
                                ,@(cond
                                    (tabular? (map cell d))
                                    ((every null? cs) (list (cell d)))
                                    (else (list (cell d) `(td (code/inline ,(string-intersperse (map ->string/write c) ", "))))))))
                      is data cs))))
        ,@(if (pair? answers)
              (map (λ (view) `(mermaid ,(μkanren-answers->mermaid answers view: view var: var
                                                                  max-diagrams: max-diagrams share: share)))
                (μkanren-views diagram))
              '())
        ,@(if (pair? profile) `((mermaid ,(μkanren-profile->mermaid profile))) '()))))

  (define-syntax-rule (μkanren-run/sxml (v n) g ...)
    `((code/scheme (μkanren-run (v n) g ...))
      ,@(μkanren-answers->sxml (°->answers (take° n (fresh° (v) g ...))) var: 'v)))

  (define-syntax-rule (μkanren-run/sxml/profiled (v n) g ...)
    (match1/first ((,answers ,prof) (°->answers/profiled (take° n (fresh° (v) g ...))))
      `((code/scheme (μkanren-run (v n) g ...))
        ,@(μkanren-answers->sxml answers var: 'v profile: prof))))
)
