; (aux search): state-space search, uninformed and heuristic, whose answers are lazy streams of the
; solutions, in the order each algorithm finds them.
;
; A *problem* (`make-problem`) is a start state, a `successors` procedure and a goal test, plus the
; optional key, equality and hash of states (for duplicate detection), a heuristic and, for
; bidirectional search, a goal state with a `predecessors` procedure. `successors` maps a state to a
; list, or an (aux stream sicp) stream, of *steps* `(action state)`, of cost 1, or
; `(action state cost)`, in the spirit of the moves `(action tree)` of Land of Lisp.
; A search grows *nodes* (state, parent node, action, path cost g, depth): solutions share their
; prefixes, `node-path` and `node-actions` read a path back. Every search answers a stream of goal
; nodes and updates a `search-stats` record (expanded, generated, max-frontier, cutoff?,
; iterations) *as the stream is forced*: a goal node is yielded before it is expanded, so the stats
; read after `first-solution` count exactly the work that finding it needed.
;
; Duplicates (`duplicates:`): 'tree detects none, 'path drops a node whose state repeats one of its
; ancestors, 'graph keeps one table for the whole search: a *reached* set for breadth-first,
; depth-first, beam and bidirectional search, and a *best g* table for the best-first family, which
; reopens a state whenever a strictly cheaper path to it appears (AIMA 4th ed., BEST-FIRST-SEARCH),
; so A* stays optimal with an admissible but inconsistent heuristic.
;
; Choices:
;   - each algorithm is a small positional recursion of `λ§` procedures (whose bodies are delayed),
;     behind a keyword wrapper; frontiers are persistent: an (aux fds queue) for breadth-first
;     search, an (aux fds heap) of #(f serial node) entries for the best-first family, where the
;     serial breaks ties first in, first out; depth-first search is the stream monad itself, with
;     the sequential `stream:append-map` of (aux stream sicp);
;   - a successors list is consumed directly as a stream (a list is a stream whose cells are all
;     forced already), and a successors stream is forced into a list only where every child is
;     needed at once (breadth-first, best-first, beam and bidirectional search);
;   - iterative deepening (Korf 1985) keeps from iteration d only the solutions of depth exactly d,
;     with `stream:iterative-deepening` of (aux stream sicp) as the filter; whether to run
;     iteration d+1 is decided only after iteration d is exhausted, so a finite space without
;     solutions ends as soon as an iteration cuts nothing;
;   - IDA* (Korf 1985) yields from each iteration the goal nodes that the previous iteration did not
;     visit: those whose path has some f above the previous bound. With a consistent heuristic
;     (and h = 0 at goals) these are the goals with previous bound < g ≤ bound; the path maximum is
;     what makes the rule exact for an inconsistent one too;
;   - bidirectional search (Pohl 1971) assumes unit costs and stops at the first meeting state;
;   - numbers are compared with `<` and `>` only, so exact costs stay exact.
;
; References: Hart, Nilsson & Raphael, "A formal basis for the heuristic determination of minimum
; cost paths", IEEE Trans. SSC 4(2), 1968 (A*) · Dijkstra, "A note on two problems in connexion
; with graphs", 1959 (uniform cost) · Korf, "Depth-first iterative-deepening: an optimal admissible
; tree search", AI 27(1), 1985 (IDS, IDA*) · Pohl, "Bi-directional search", Machine Intelligence 6,
; 1971 · Russell & Norvig, "Artificial Intelligence: A Modern Approach", 4th ed., ch. 3.

(module (aux search) (make-problem problem? problem-start problem-successors problem-goal? problem-goal
                      problem-predecessors problem-key problem-test problem-hash problem-heuristic
                      step-action step-state step-cost
                      node? node-state node-parent node-action node-cost node-depth node-path node-actions
                      make-search-stats search-stats? search-stats-expanded search-stats-generated
                      search-stats-max-frontier search-stats-cutoff? search-stats-iterations
                      search-stats->alist
                      breadth-first-search depth-first-search depth-limited-search iterative-deepening-search
                      best-first-search uniform-cost-search greedy-best-first-search a*-search ida*-search
                      beam-search bidirectional-search first-solution)

  (import scheme
          (chicken base)
          (chicken fixnum)
          (chicken sort)
          (only srfi-1 filter)
          srfi-69
          (aux base)
          (aux stream sicp)
          (aux fds queue)
          (aux fds heap))

  ; problems ---------------------------------------------------------------------------------------

  (define-record-type problem
    (%make-problem start successors goal? goal predecessors key test hash heuristic)
    problem?
    (start problem-start)
    (successors problem-successors)
    (goal? problem-goal?)
    (goal problem-goal)
    (predecessors problem-predecessors)
    (key problem-key)
    (test problem-test)
    (hash problem-hash)
    (heuristic problem-heuristic))

  ; without `goal?:`, the goal test compares keys with the key of `goal:`, if any.
  (define (make-problem #!key
                        (start (error "make-problem: start: is required"))
                        (successors (error "make-problem: successors: is required"))
                        (goal? #f) (goal #f) (predecessors #f)
                        (key identity) (test equal?) (hash #f) (heuristic (K 0)))
    (let ((goal? (cond
                   (goal? goal?)
                   (goal (let ((k (key goal))) (λ (s) (test (key s) k))))
                   (else (K #f)))))
      (%make-problem start successors goal? goal predecessors key test hash heuristic)))

  (define step-action car)
  (define step-state cadr)
  (define (step-cost s) (if (pair? (cddr s)) (caddr s) 1))

  ; the steps that (successors s) answers, as a list, for the searches that need every child at once.
  (define (steps-list successors s)
    (let ((steps (successors s)))
      (if (promise? steps) (stream:->list steps) steps)))

  (define (successor-steps p s) (steps-list (problem-successors p) s))

  (define (key-table p)
    (if (problem-hash p)
      (make-hash-table (problem-test p) (problem-hash p))
      (make-hash-table (problem-test p))))

  ; nodes ------------------------------------------------------------------------------------------

  (define-record-type search-node
    (%make-node state parent action cost depth)
    node?
    (state node-state)
    (parent node-parent)
    (action node-action)
    (cost node-cost)
    (depth node-depth))

  (define (root-node p) (%make-node (problem-start p) #f #f 0 0))

  (define (child-node n step)
    (%make-node (step-state step) n (step-action step) (+ (node-cost n) (step-cost step)) (fx+ (node-depth n) 1)))

  ; the children of n as a stream, as lazy as the successors of its state.
  (define (child-stream p n)
    ((stream:map (λ (s) (child-node n s))) ((problem-successors p) (node-state n))))

  (define (node-path n)
    (let walk ((n n) (states '()))
      (if n (walk (node-parent n) (cons (node-state n) states)) states)))

  (define (node-actions n)
    (let walk ((n n) (actions '()))
      (if (node-parent n) (walk (node-parent n) (cons (node-action n) actions)) actions)))

  (define (first-solution s)
    (stream:dest/car+cdr s
                         ((n _) n)
                         (else #f)))

  ; statistics -------------------------------------------------------------------------------------

  (define-record-type search-stats
    (%make-search-stats expanded generated max-frontier cutoff? iterations)
    search-stats?
    (expanded search-stats-expanded search-stats-expanded-set!)
    (generated search-stats-generated search-stats-generated-set!)
    (max-frontier search-stats-max-frontier search-stats-max-frontier-set!)
    (cutoff? search-stats-cutoff? search-stats-cutoff?-set!)
    (iterations search-stats-iterations search-stats-iterations-set!))

  (define (make-search-stats) (%make-search-stats 0 0 0 #f 0))

  (define (search-stats->alist st)
    `((expanded . ,(search-stats-expanded st))
      (generated . ,(search-stats-generated st))
      (max-frontier . ,(search-stats-max-frontier st))
      (cutoff? . ,(search-stats-cutoff? st))
      (iterations . ,(search-stats-iterations st))))

  (define (expanded! st) (search-stats-expanded-set! st (fx+ (search-stats-expanded st) 1)))
  (define (generated! st) (search-stats-generated-set! st (fx+ (search-stats-generated st) 1)))
  (define (iteration! st) (search-stats-iterations-set! st (fx+ (search-stats-iterations st) 1)))
  (define (frontier! st size)
    (when (fx> size (search-stats-max-frontier st)) (search-stats-max-frontier-set! st size)))

  ; duplicate policies -----------------------------------------------------------------------------

  ; the policy as a predicate on a new node: does it enter the search? Under 'graph the predicate
  ; marks the state it admits, so every search makes its own.
  (define (admission p duplicates)
    (let ((key (problem-key p)) (same? (problem-test p)))
      (case duplicates
        ((tree) (K #t))
        ((path) (λ (n)
                  (let ((k (key (node-state n))))
                    (let ancestors ((a (node-parent n)))
                      (cond
                        ((not a) #t)
                        ((same? k (key (node-state a))) #f)
                        (else (ancestors (node-parent a))))))))
        ((graph) (let ((reached (key-table p)))
                   (λ (n)
                     (let ((k (key (node-state n))))
                       (and (not (hash-table-exists? reached k))
                            (begin (hash-table-set! reached k #t) #t))))))
        (else (error "search: duplicates: must be 'tree, 'path or 'graph" duplicates)))))

  ; breadth-first search ---------------------------------------------------------------------------

  ; Invariant: the queue holds the generated, unexpanded nodes in nondecreasing depth, so the goal
  ; test at dequeue yields the solutions in nondecreasing depth.
  (define (bfs p duplicates st)
    (let ((goal? (problem-goal? p))
          (admit? (admission p duplicates))
          (root (root-node p)))
      (define (enqueue-children n q)
        (expanded! st)
        (let loop ((steps (successor-steps p (node-state n))) (q q))
          (cond
            ((null? steps) (frontier! st (fds-queue-size q)) q)
            (else (let ((c (child-node n (car steps))))
                    (cond
                      ((admit? c) (generated! st) (loop (cdr steps) (fds-queue-cons c q)))
                      (else (loop (cdr steps) q))))))))
      (letrec ((B (λ§ (q) ; the solutions in the subtrees of the nodes in q
                    (cond
                      ((fds-queue-empty? q) stream:empty)
                      (else (let ((n (fds-queue-car q)) (q (fds-queue-cdr q)))
                              (if (goal? (node-state n))
                                (stream:cons n (E n q))
                                (E n q)))))))
               (E (λ§ (n q) (B (enqueue-children n q)))))
        (admit? root)
        (generated! st)
        (frontier! st 1)
        (B (fds-queue-cons root fds-queue-empty)))))

  ; depth-first search -----------------------------------------------------------------------------

  ; The solutions in the subtree of the root, depth first, as the stream monad: a node contributes
  ; itself if it is a goal, then the solutions of its children in order. With a `limit`, a node at
  ; depth `limit` is not expanded, and `cut!` is called when it is not a goal (as in AIMA, whether
  ; it has successors is not checked).
  (define (dfs p admit? limit cut! st)
    (let ((goal? (problem-goal? p)))
      (letrec ((D (λ§ (n)
                    (cond
                      ((not (admit? n)) stream:empty)
                      (else (generated! st)
                            (let* ((goal (goal? (node-state n)))
                                   (here (if goal (stream:singleton n) stream:empty)))
                              (cond
                                ((and limit (fx= (node-depth n) limit)) (unless goal (cut!)) here)
                                (else (stream:append here (X n)))))))))
               (X (λ§ (n)
                    (expanded! st)
                    ((stream:append-map D) (child-stream p n)))))
        (D (root-node p)))))

  ; Iterative deepening (Korf 1985). Invariant: iteration d yields exactly the solutions of depth d,
  ; since the shallower ones were yielded by the iterations before it.
  (define (ids p max-depth duplicates st)
    (letrec ((I (λ§ (d)
                  (iteration! st)
                  (search-stats-cutoff?-set! st #f)
                  (let* ((cut? #f)
                         (cut! (λ () (set! cut? #t) (search-stats-cutoff?-set! st #t)))
                         (solutions (dfs p (admission p duplicates) d cut! st)))
                    (stream:append ((stream:iterative-deepening d d node-depth) solutions)
                                   (δ! (if (and cut? (not (and max-depth (fx>= d max-depth))))
                                         (I (fx+ d 1))
                                         stream:empty)))))))
      (I 0)))

  ; best-first search ------------------------------------------------------------------------------

  ; Invariant: the heap holds the generated, unexpanded nodes, so popping by f and testing the goal
  ; at pop yields the solutions in nondecreasing f. Under 'graph a child is pushed only if its g is
  ; strictly below the best g recorded for its state, and an entry whose g was beaten since it was
  ; pushed is stale: it is dropped at pop, unexpanded.
  (define (best-first p f duplicates st)
    (let* ((goal? (problem-goal? p))
           (key (problem-key p))
           (best-g (and (eq? duplicates 'graph) (key-table p)))
           (serial 0))
      (define (cheapest! n) ; is n the cheapest path found to its state? If so, record its g
        (let* ((k (key (node-state n)))
               (g (hash-table-ref/default best-g k #f)))
          (and (or (not g) (< (node-cost n) g))
               (begin (hash-table-set! best-g k (node-cost n)) #t))))
      (define admit? (if best-g cheapest! (admission p duplicates)))
      (define (stale? n) (and best-g (> (node-cost n) (hash-table-ref best-g (key (node-state n))))))
      (define (entry n) (begin1 (vector (f n) serial n) (set! serial (fx+ serial 1))))
      (define (entry<? a b)
        (or (< (vector-ref a 0) (vector-ref b 0))
            (and (= (vector-ref a 0) (vector-ref b 0)) (fx< (vector-ref a 1) (vector-ref b 1)))))
      (define (push-children n h)
        (expanded! st)
        (let loop ((steps (successor-steps p (node-state n))) (h h))
          (cond
            ((null? steps) (frontier! st (fds-heap-size h)) h)
            (else (let ((c (child-node n (car steps))))
                    (cond
                      ((admit? c) (generated! st) (loop (cdr steps) (fds-heap-insert (entry c) h)))
                      (else (loop (cdr steps) h))))))))
      (letrec ((B (λ§ (h) ; the solutions in the subtrees of the nodes in h
                    (cond
                      ((fds-heap-empty? h) stream:empty)
                      (else (let-values (((e h) (fds-heap-pop h)))
                              (let ((n (vector-ref e 2)))
                                (cond
                                  ((stale? n) (B h))
                                  ((goal? (node-state n)) (stream:cons n (E n h)))
                                  (else (E n h)))))))))
               (E (λ§ (n h) (B (push-children n h)))))
        (let ((root (root-node p)))
          (admit? root)
          (generated! st)
          (frontier! st 1)
          (B (fds-heap-insert (entry root) (make-fds-heap entry<?)))))))

  ; IDA* (Korf 1985): iterative deepening on f = g + h. Iteration k visits, depth first, the nodes
  ; whose path keeps every f within `bound`, and records the least f beyond it as the next bound.
  ; Invariant: a node is visited by iteration k, and by every later one, iff the largest f on its
  ; path m is ≤ bound; so it is new in iteration k iff m > previous, and each solution comes out
  ; once. With an admissible heuristic the first one is optimal: the bounds run through the path
  ; maxima, the optimal cost C* is the path maximum of an optimal goal, and no goal has g ≤ bound
  ; while bound < C*.
  (define (ida* p duplicates st)
    (let* ((goal? (problem-goal? p))
           (h (problem-heuristic p))
           (f (λ (n) (+ (node-cost n) (h (node-state n)))))
           (root (root-node p)))
      (letrec ((I (λ§ (previous bound)
                    (iteration! st)
                    (search-stats-cutoff?-set! st #f)
                    (let ((next +inf.0) (admit? (admission p duplicates)))
                      (letrec ((D (λ§ (n m) ; m: the largest f on the path above n
                                    (cond
                                      ((not (admit? n)) stream:empty)
                                      (else (generated! st)
                                            (let ((fn (f n)))
                                              (cond
                                                ((> fn bound)
                                                 (when (< fn next) (set! next fn))
                                                 (search-stats-cutoff?-set! st #t)
                                                 stream:empty)
                                                (else (let ((m (if (> fn m) fn m)))
                                                        (stream:append
                                                          (if (and (> m previous) (goal? (node-state n)))
                                                            (stream:singleton n)
                                                            stream:empty)
                                                          (X n m))))))))))
                               (X (λ§ (n m)
                                    (expanded! st)
                                    ((stream:append-map (λ (c) (D c m))) (child-stream p n)))))
                        (stream:append (D root -inf.0)
                                       (δ! (if (< next +inf.0) (I bound next) stream:empty))))))))
        (I -inf.0 (f root)))))

  ; beam search ------------------------------------------------------------------------------------

  ; Layer by layer, as breadth-first search, but each layer keeps only its `width` best children by
  ; `score` (ties in generation order, `sort` is stable). Incomplete; with an infinite width it is
  ; breadth-first search ordered within each layer.
  (define (beam p width score duplicates st)
    (let ((goal? (problem-goal? p))
          (admit? (admission p duplicates))
          (root (root-node p)))
      (define (children layer) ; the admitted children of the nodes of layer, in order
        (let loop ((layer layer) (acc '()))
          (cond
            ((null? layer) (reverse acc))
            (else (let ((n (car layer)))
                    (expanded! st)
                    (let steps ((ss (successor-steps p (node-state n))) (acc acc))
                      (cond
                        ((null? ss) (loop (cdr layer) acc))
                        (else (let ((c (child-node n (car ss))))
                                (cond
                                  ((admit? c) (generated! st) (steps (cdr ss) (cons c acc)))
                                  (else (steps (cdr ss) acc))))))))))))
      (define (best nodes) ; the first `width` nodes by score
        (let loop ((ranked (sort (map (λ (n) (cons (score n) n)) nodes) (λ (a b) (< (car a) (car b)))))
                   (i 0)
                   (acc '()))
          (if (or (null? ranked) (not (< i width)))
            (reverse acc)
            (loop (cdr ranked) (add1 i) (cons (cdar ranked) acc)))))
      (define (goals layer) (filter (λ (n) (goal? (node-state n))) layer))
      (letrec ((L (λ§ (layer)
                    (cond
                      ((null? layer) stream:empty)
                      (else (iteration! st)
                            (frontier! st (length layer))
                            (stream:append (list->stream (goals layer))
                                           (δ! (L (best (children layer))))))))))
        (admit? root)
        (generated! st)
        (L (list root)))))

  ; bidirectional search ---------------------------------------------------------------------------

  ; Breadth first from both ends (Pohl 1971), expanding at each round the whole layer of the side
  ; with the smaller frontier. The forward table maps keys to nodes, the backward one to links
  ; #(state action next), where state --action--> (the state of next), and next is #f at the goal.
  ; Optimality of the first meeting: before the round the balls of radii k_f and k_b were
  ; disjoint, so the distance D > k_f + k_b; a meeting now gives D ≤ k_f + k_b + 1, so equality,
  ; and every meeting state has backward distance exactly k_b.
  (define (bidirectional p st)
    (let ((key (problem-key p))
          (forward (key-table p))
          (backward (key-table p))
          (root (root-node p))
          (goal (problem-goal p)))
      (define (join n link) ; n, extended along the backward links to the goal
        (let ((next (vector-ref link 2)))
          (if next
            (join (child-node n (list (vector-ref link 1) (vector-ref next 0))) next)
            n)))
      ; expands a layer: (values next-layer #f), or (values '() solution) at the first meeting
      (define (expand layer steps-of new-entry other)
        (let loop ((layer layer) (next '()))
          (cond
            ((null? layer) (values (reverse next) #f))
            (else (expanded! st)
                  (let steps ((ss (steps-of (car layer))) (next next))
                    (cond
                      ((null? ss) (loop (cdr layer) next))
                      (else (let-values (((k entry) (new-entry (car layer) (car ss))))
                              (cond
                                ((not entry) (steps (cdr ss) next))
                                ((hash-table-ref/default other k #f)
                                 => (λ (there) (values '() (if (node? entry) (join entry there) (join there entry)))))
                                (else (steps (cdr ss) (cons entry next))))))))))))
      (define (new-node n step) ; a forward child, if its state is new
        (let* ((c (child-node n step)) (k (key (node-state c))))
          (cond
            ((hash-table-exists? forward k) (values k #f))
            (else (hash-table-set! forward k c) (generated! st) (values k c)))))
      (define (new-link link step) ; a backward link, if its state is new
        (let ((k (key (step-state step))))
          (cond
            ((hash-table-exists? backward k) (values k #f))
            (else (let ((l (vector (step-state step) (step-action step) link)))
                    (hash-table-set! backward k l)
                    (generated! st)
                    (values k l))))))
      (unless goal (error "bidirectional-search: the problem needs goal:"))
      (unless (problem-predecessors p) (error "bidirectional-search: the problem needs predecessors:"))
      (δ!
        (let ((start-key (key (node-state root)))
              (goal-link (vector goal #f #f)))
          (hash-table-set! forward start-key root)
          (hash-table-set! backward (key goal) goal-link)
          (generated! st)
          (generated! st)
          (if ((problem-test p) start-key (key goal))
            (stream:singleton root)
            (let round ((fs (list root)) (bs (list goal-link)))
              (let ((nf (length fs)) (nb (length bs)))
                (frontier! st (fx+ nf nb))
                (cond
                  ((or (fx= nf 0) (fx= nb 0)) stream:empty)
                  ((fx<= nf nb)
                   (let-values (((fs solution) (expand fs (λ (n) (successor-steps p (node-state n)))
                                                       new-node backward)))
                     (if solution (stream:singleton solution) (round fs bs))))
                  (else
                    (let-values (((bs solution) (expand bs (λ (l) (steps-list (problem-predecessors p) (vector-ref l 0)))
                                                        new-link forward)))
                      (if solution (stream:singleton solution) (round fs bs))))))))))))

  ; the interface ----------------------------------------------------------------------------------

  (define (breadth-first-search p #!key (duplicates 'graph) (stats (make-search-stats)))
    (bfs p duplicates stats))

  (define (depth-first-search p #!key (duplicates 'path) (stats (make-search-stats)))
    (dfs p (admission p duplicates) #f void stats))

  (define (depth-limited-search p limit #!key (duplicates 'path) (stats (make-search-stats)))
    (dfs p (admission p duplicates) limit (λ () (search-stats-cutoff?-set! stats #t)) stats))

  (define (iterative-deepening-search p #!key (max-depth #f) (duplicates 'path) (stats (make-search-stats)))
    (ids p max-depth duplicates stats))

  (define (best-first-search p f #!key (duplicates 'graph) (stats (make-search-stats)))
    (best-first p f duplicates stats))

  (define (uniform-cost-search p #!key (duplicates 'graph) (stats (make-search-stats)))
    (best-first p node-cost duplicates stats))

  (define (greedy-best-first-search p #!key (duplicates 'graph) (stats (make-search-stats)))
    (let ((h (problem-heuristic p)))
      (best-first p (λ (n) (h (node-state n))) duplicates stats)))

  (define (a*-search p #!key (duplicates 'graph) (stats (make-search-stats)))
    (let ((h (problem-heuristic p)))
      (best-first p (λ (n) (+ (node-cost n) (h (node-state n)))) duplicates stats)))

  (define (ida*-search p #!key (duplicates 'path) (stats (make-search-stats)))
    (ida* p duplicates stats))

  (define (beam-search p width #!key (score #f) (duplicates 'graph) (stats (make-search-stats)))
    (let ((h (problem-heuristic p)))
      (beam p width (or score (λ (n) (h (node-state n)))) duplicates stats)))

  (define (bidirectional-search p #!key (stats (make-search-stats)))
    (bidirectional p stats))

  )
