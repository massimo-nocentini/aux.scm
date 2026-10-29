; (aux search adversarial): game-tree search on lazy trees, from minimax to alpha-beta pruning and
; its *-minimax extensions to trees with chance nodes, with transposition tables, iterative
; deepening under a node budget, and the move choice of Land of Lisp's `handle-computer`.
;
; A *game* (`make-game`) describes a tree by procedures on its nodes: `children` (a stream, built
; with `λ§` or `stream:map` so that it is memoized), `evaluate` (the static value of a leaf),
; `maximizing?`, and, for chance nodes, `chance?` and `outcomes` (a list of (probability . node));
; `make-chance-node` is a ready chance-node record. Values are always from the point of view of one
; rated player: the tree is *paranoid* (Sturtevant & Korf 2000), every other player minimizes,
; and turns need not alternate (in Dice of Doom a player attacks several times, then passes).
; `depth:` is #f, for a finite tree searched to its leaves, or the number of plies left, a
; nonnegative fixnum (anything else raises): a decision node reached with depth 0 is a horizon leaf, a chance node passes its depth unchanged to
; its outcomes, so a ply is a decision together with the chance it brings, as in LoL's
; `limit-tree-depth`.
;
; The eight value procedures share one signature and one skeleton: the brute-force `minimax`,
; `negamax` and `expectiminimax`, and the pruned `αβ` (fail-soft), `αβ/fail-hard`, `αβ/negamax`,
; `star1` and `star2`. With the window (α, β), α < β, on a node of value v, a fail-soft result r
; satisfies  r ≤ α ⇒ v ≤ r,  α < r < β ⇒ v = r,  r ≥ β ⇒ v ≥ r  (property P), and a fail-hard
; one also α ≤ r ≤ β (property H): with the window (−∞, +∞) both give r = v. `best-move` rates the
; children of the root as LoL's `ab-get-ratings-max` does and picks the first best;
; `best-move/iterative-deepening` and `principal-variation` build on it. A `game-stats` record
; counts the nodes entered, the chance nodes, the leaves evaluated, the cutoffs, the Star2 probes
; and the transposition hits.
;
; Correspondence with Land of Lisp (ch. 15, 18 and 20): `rate-position` with `get-ratings` is
; `minimax` at a node plus the `decision-ratings` of `best-move`; `limit-tree-depth` is `depth:`;
; `ab-rate-position` with `ab-get-ratings-max/min` is `αβ` (it is fail-soft: it returns the
; unclamped extremum of ratings cut at the first one beyond the limit) plus the `best-move` root;
; the chance-weighted `get-ratings` of version 4 is `expectiminimax`; `handle-computer`'s
; `(position (apply #'max ratings) ratings)` is `decision-index`, the first maximal child. LoL's
; limits are (upper-limit lower-limit), here they are (α β).
;
; Choices:
;   - each procedure is a positional recursion (R node depth α β) closed over the game, the
;     stats, the move order and the table, behind a keyword wrapper: keywords are parsed once;
;   - children are consumed with `stream:dest/car+cdr`, one force per cell, and a pruned search
;     never forces the children it skips;
;   - values are never combined with `max`/`min` nor folded from ±inf.0, since (max -inf.0 3) is
;     3.0: folds start from the first child and keep the better value with `>`/`<`, so exact
;     evaluations stay exact (tic-tac-toe values are 0, not 0.); windows may be infinite, since
;     they are only compared;
;   - negamax handles consecutive moves of one side: the sign flips only where the side changes;
;   - a Star1/Star2 chance node first answers from the bounds alone when the window lies beyond
;     them (β ≤ L or α ≥ U), so the windows it passes down are never empty;
;   - a transposition entry records the remaining depth only when the search below it reached the
;     horizon; an entry of a subtree searched to its leaves is complete (depth #f) and serves any
;     depth, and a hit on a depth-limited entry counts as reaching the horizon, so iterative
;     deepening never stops early on the strength of a table. An entry answers any search no
;     deeper than its own, as usual: where a node recurs with different depths left (below a
;     different number of chance nodes, say), a table may answer with a better informed value
;     than the depth-limited search would compute, so a table per rated player and decision is
;     the safe use;
;   - the node budget of iterative deepening escapes with a continuation from the node counter,
;     which the stats hold only within the dynamic extent of an iteration.
;
; References: Knuth & Moore, "An analysis of alpha-beta pruning", AI 6(4), 1975 (αβ, negamax) ·
; Fishburn, "Analysis of speedup in distributed algorithms", 1981 (fail-soft) · Michie,
; "Game-playing and game-learning automata", 1966 (expectiminimax) · Ballard, "The *-minimax search
; procedure for trees containing chance nodes", AI 21(3), 1983 (Star1, Star2) · Hauk, Buro &
; Schaeffer, "Rediscovering *-minimax search", Computers and Games 2004, LNCS 3846 (the general
; probabilities form used here) · Sturtevant & Korf, "On pruning techniques for multi-player
; games", AAAI 2000 (paranoid) · Slate & Atkin, "Chess 4.5", 1977 (iterative deepening,
; transposition tables) · Barski, "Land of Lisp", 2010, ch. 15-20.

(module (aux search adversarial) (make-game game? game-children game-evaluate game-maximizing? game-chance?
                                  game-outcomes game-bounds game-key game-test game-hash
                                  make-chance-node chance-node? chance-node-outcomes
                                  make-game-stats game-stats? game-stats-nodes game-stats-chance-nodes
                                  game-stats-leaves game-stats-cutoffs game-stats-probes game-stats-tt-hits
                                  game-stats-horizon? game-stats->alist
                                  minimax negamax αβ αβ/fail-hard αβ/negamax expectiminimax star1 star2
                                  best-move decision? decision-value decision-index decision-child
                                  decision-ratings decision-depth decision-stats
                                  best-move/iterative-deepening principal-variation order-by
                                  make-transposition-table)

  (import scheme
          (chicken base)
          (chicken fixnum)
          (chicken sort)
          (only srfi-1 filter)
          srfi-69
          (aux base)
          (aux stream sicp))

  ; games ------------------------------------------------------------------------------------------

  (define-record chance-node outcomes)

  (define-record-type game
    (%make-game children evaluate maximizing? chance? outcomes bounds key test hash)
    game?
    (children game-children)
    (evaluate game-evaluate)
    (maximizing? game-maximizing?)
    (chance? game-chance?)
    (outcomes game-outcomes)
    (bounds game-bounds)
    (key game-key)
    (test game-test)
    (hash game-hash))

  ; `bounds:` is #f or (L . U), finite reals with L ≤ value ≤ U at every node, as Star1 needs.
  (define (make-game #!key
                     (children (error "make-game: children: is required"))
                     (evaluate (error "make-game: evaluate: is required"))
                     (maximizing? (K #t)) (chance? chance-node?) (outcomes chance-node-outcomes)
                     (bounds #f) (key identity) (test equal?) (hash #f))
    (%make-game children evaluate maximizing? chance? outcomes bounds key test hash))

  ; statistics -------------------------------------------------------------------------------------

  (define-record-type game-stats
    (%make-game-stats nodes chance-nodes leaves cutoffs probes tt-hits horizon? limit escape)
    game-stats?
    (nodes game-stats-nodes game-stats-nodes-set!)
    (chance-nodes game-stats-chance-nodes game-stats-chance-nodes-set!)
    (leaves game-stats-leaves game-stats-leaves-set!)
    (cutoffs game-stats-cutoffs game-stats-cutoffs-set!)
    (probes game-stats-probes game-stats-probes-set!)
    (tt-hits game-stats-tt-hits game-stats-tt-hits-set!)
    (horizon? game-stats-horizon? game-stats-horizon?-set!)
    (limit game-stats-limit game-stats-limit-set!)     ; #f, or the number of nodes after which…
    (escape game-stats-escape game-stats-escape-set!)) ; …this continuation is called with #f

  (define (make-game-stats) (%make-game-stats 0 0 0 0 0 0 #f #f #f))

  (define (game-stats->alist st)
    `((nodes . ,(game-stats-nodes st))
      (chance-nodes . ,(game-stats-chance-nodes st))
      (leaves . ,(game-stats-leaves st))
      (cutoffs . ,(game-stats-cutoffs st))
      (probes . ,(game-stats-probes st))
      (tt-hits . ,(game-stats-tt-hits st))
      (horizon? . ,(game-stats-horizon? st))))

  ; every node entered goes through here, which makes the node budget exact
  (define (enter! st)
    (let ((n (fx+ (game-stats-nodes st) 1))
          (limit (game-stats-limit st)))
      (game-stats-nodes-set! st n)
      (when (and limit (fx> n limit)) ((game-stats-escape st) #f))))

  (define (chance! st) (game-stats-chance-nodes-set! st (fx+ (game-stats-chance-nodes st) 1)))
  (define (leaf! st) (game-stats-leaves-set! st (fx+ (game-stats-leaves st) 1)))
  (define (cutoff! st) (game-stats-cutoffs-set! st (fx+ (game-stats-cutoffs st) 1)))
  (define (probe! st) (game-stats-probes-set! st (fx+ (game-stats-probes st) 1)))
  (define (hit! st) (game-stats-tt-hits-set! st (fx+ (game-stats-tt-hits st) 1)))
  (define (horizon! st) (game-stats-horizon?-set! st #t))

  ; values and windows -----------------------------------------------------------------------------

  ; max and min that return one of their arguments as it is, exact or not
  (define (larger a b) (if (> a b) a b))
  (define (smaller a b) (if (< a b) a b))

  (define (clamp v α β)
    (cond
      ((< v α) α)
      ((> v β) β)
      (else v)))

  ; transposition tables ---------------------------------------------------------------------------

  ; Entries are #(depth value flag): depth is #f for a complete search, flag tells whether value
  ; is the exact value, a lower bound or an upper bound. Only decision nodes are stored.
  (define (make-transposition-table game)
    (if (game-hash game)
      (make-hash-table (game-test game) (game-hash game))
      (make-hash-table (game-test game))))

  ; the value of the entry for key k, if it answers a search to depth d in the window (α, β)
  (define (tt-probe table st k d α β)
    (let ((e (hash-table-ref/default table k #f)))
      (and e
           (let ((depth (vector-ref e 0)) (v (vector-ref e 1)))
             (and (or (not depth) (and d (fx>= depth d)))
                  (case (vector-ref e 2)
                    ((exact) #t)
                    ((lower) (>= v β))
                    ((upper) (<= v α)))
                  (begin
                    (hit! st)
                    (when depth (horizon! st))
                    v))))))

  (define (tt-store! table k depth v α β)
    (hash-table-set! table k (vector depth v (cond
                                               ((>= v β) 'lower)
                                               ((<= v α) 'upper)
                                               (else 'exact)))))

  ; the skeleton -----------------------------------------------------------------------------------

  ; the children of n, as a stream in the game's order or as the list that `order` sorts
  (define ((moves-of game order) n)
    (let ((children ((game-children game) n)))
      (if order (order n (stream:->list children)) children)))

  ; The recursion (R n d α β) of a value procedure. At a decision node it handles the table, the
  ; horizon and the terminal leaves, and leaves the children to `fold`, called as
  ; (fold R st max? first-child other-children d−1 α β); a chance node goes to the procedure that
  ; (chance-rule game st R moves) makes. `clamp?` makes the result fail-hard.
  (define (searcher chance-rule fold clamp?)
    (λ (game st order table)
      (let ((evaluate (game-evaluate game))
            (maximizing? (game-maximizing? game))
            (chance? (game-chance? game))
            (key (game-key game))
            (moves (moves-of game order)))
        (define (leaf n) (leaf! st) (evaluate n))
        (define (decide n d α β)
          (cond
            ((and d (fx= d 0)) (horizon! st) (leaf n))
            (else (stream:dest/car+cdr (moves n)
                                       ((c cs) (fold R st (maximizing? n) c cs (and d (fx- d 1)) α β))
                                       (else (leaf n))))))
        (define (decide/table n d α β)
          (let ((k (key n)))
            (or (tt-probe table st k d α β)
                (let ((outer (game-stats-horizon? st)))
                  (game-stats-horizon?-set! st #f)
                  (let* ((v (decide n d α β))
                         (cut? (game-stats-horizon? st)))
                    (tt-store! table k (and cut? d) v α β)
                    (game-stats-horizon?-set! st (or outer cut?))
                    v)))))
        (define (R n d α β)
          (enter! st)
          (cond
            ((chance? n) (chance! st) (chance n d α β))
            (else (let ((v (if table (decide/table n d α β) (decide n d α β))))
                    (if clamp? (clamp v α β) v)))))
        (define chance (chance-rule game st R moves))
        R)))

  ; decision nodes ---------------------------------------------------------------------------------

  ; minimax: the best of every child, the first one of the best on ties.
  (define (minimax-fold R st max? c cs d α β)
    (let loop ((cs cs) (best (R c d α β)))
      (stream:dest/car+cdr cs
                           ((c cs) (let ((v (R c d α β)))
                                     (loop cs (if max? (larger v best) (smaller v best)))))
                           (else best))))

  ; Alpha-beta, fail-soft (Knuth & Moore 1975, procedure F2; Fishburn 1981). Invariant at a max
  ; node: best is the largest value of the children searched so far, and α the larger of the
  ; incoming α and best; each child is searched in (α, β), and once best ≥ β the remaining children
  ; cannot matter to the caller (property P: the value is ≥ best ≥ β). The min side is symmetric.
  (define (αβ-fold R st max? c cs d α β)
    (if max?
      (let loop ((cs cs) (α α) (best (R c d α β)))
        (if (>= best β)
          (begin (cutoff! st) best)
          (stream:dest/car+cdr cs
                               ((c cs) (let* ((α (larger best α)) (v (R c d α β)))
                                         (loop cs α (larger v best))))
                               (else best))))
      (let loop ((cs cs) (β β) (best (R c d α β)))
        (if (<= best α)
          (begin (cutoff! st) best)
          (stream:dest/car+cdr cs
                               ((c cs) (let* ((β (smaller best β)) (v (R c d α β)))
                                         (loop cs β (smaller v best))))
                               (else best))))))

  ; chance nodes -----------------------------------------------------------------------------------

  (define ((refuse message) game st R moves)
    (λ (n d α β) (error message)))

  ; expectiminimax (Michie 1966): Σ p·v over every outcome, p = 0 included, folded left from the
  ; first product, as LoL's (+ (* odds s) (* (- 1 odds) f)).
  (define (expectation game st R moves)
    (let ((outcomes (game-outcomes game)))
      (λ (n d α β)
        (let ((os (outcomes n)))
          (let loop ((os (cdr os)) (sum (* (caar os) (R (cdar os) d α β))))
            (if (null? os)
              sum
              (loop (cdr os) (+ sum (* (caar os) (R (cdar os) d α β))))))))))

  ; Star1 and, with `probing?`, Star2 (Ballard 1983), with general probabilities as Hauk, Buro and
  ; Schaeffer (2004) state them, fail-soft. Each outcome i of probability p_i > 0 has known bounds
  ; lo_i ≤ V_i ≤ hi_i, at first the game's (L . U). With S_lo(i) = Σ_{j≠i} p_j·lo_j and S_hi(i)
  ; likewise, the expectation E lies in [S_lo(i) + p_i·lo_i, S_hi(i) + p_i·hi_i], so a value of
  ; outcome i at or below α_i = (α − S_hi(i))/p_i makes E ≤ α (fail low), and one at or above
  ; β_i = (β − S_lo(i))/p_i makes E ≥ β (fail high). Outcome i is searched in its window clamped
  ; into [lo_i, hi_i]; a result strictly inside (α_i, β_i) is then exact, even when it touches lo_i
  ; or hi_i, by property P and the bounds. Invariant before each outcome: E_lo < β and E_hi > α, so
  ; the window is never empty. Star2 first probes each decision outcome on its first child only:
  ; a max outcome is at least that child's value, a min outcome at most, which may already fail.
  (define ((star name probing?) game st R moves)
    (let ((bounds (or (game-bounds game) (error (string-append name ": game-bounds required"))))
          (outcomes (game-outcomes game))
          (chance? (game-chance? game))
          (maximizing? (game-maximizing? game)))
      (let ((L (car bounds)) (U (cdr bounds)))
        (λ (n d α β)
          (cond
            ((<= β L) (cutoff! st) L)   ; every value is ≥ L ≥ β
            ((>= α U) (cutoff! st) U)   ; every value is ≤ U ≤ α
            (else
              (let* ((os (filter (λ (o) (> (car o) 0)) (outcomes n)))
                     (k (length os))
                     (p (list->vector (map car os)))
                     (o (list->vector (map cdr os)))
                     (lo (make-vector k L))
                     (hi (make-vector k U)))
                (define (others v i) ; Σ_{j≠i} p_j·v_j
                  (let loop ((j 0) (sum 0))
                    (cond
                      ((fx= j k) sum)
                      ((fx= j i) (loop (fx+ j 1) sum))
                      (else (loop (fx+ j 1) (+ sum (* (vector-ref p j) (vector-ref v j))))))))
                (define (α-of i) (/ (- α (others hi i)) (vector-ref p i)))
                (define (β-of i) (/ (- β (others lo i)) (vector-ref p i)))
                (define (fail-low i v) (cutoff! st) (+ (others hi i) (* (vector-ref p i) v)))
                (define (fail-high i v) (cutoff! st) (+ (others lo i) (* (vector-ref p i) v)))
                (define (probe i) ; Star2: a fail value, or #f after tightening the bounds of outcome i
                  (let ((oi (vector-ref o i)))
                    (and (not (chance? oi))
                         (or (not d) (fx> d 0))
                         (stream:dest/car+cdr (moves oi)
                                              ((g _)
                                               (let ((d (and d (fx- d 1))))
                                                 (probe! st)
                                                 (if (maximizing? oi)
                                                   (let* ((βi (β-of i))
                                                          (w (R g d (vector-ref lo i) (smaller βi (vector-ref hi i)))))
                                                     (when (> w (vector-ref lo i)) (vector-set! lo i w))
                                                     (and (>= w βi) (fail-high i w)))
                                                   (let* ((αi (α-of i))
                                                          (w (R g d (larger αi (vector-ref lo i)) (vector-ref hi i))))
                                                     (when (< w (vector-ref hi i)) (vector-set! hi i w))
                                                     (and (<= w αi) (fail-low i w))))))
                                              (else #f)))))
                (define (search i) ; Star1: a fail value, or #f after making outcome i exact
                  (and (not (= (vector-ref lo i) (vector-ref hi i)))
                       (let* ((αi (α-of i))
                              (βi (β-of i))
                              (v (R (vector-ref o i) d (larger αi (vector-ref lo i)) (smaller βi (vector-ref hi i)))))
                         (cond
                           ((<= v αi) (fail-low i v))
                           ((>= v βi) (fail-high i v))
                           (else (vector-set! lo i v) (vector-set! hi i v) #f)))))
                (define (each step i then) ; the first fail value of (step i), (step i+1), ..., else (then)
                  (cond
                    ((fx= i k) (then))
                    ((step i) => identity)
                    (else (each step (fx+ i 1) then))))
                (define (expected) ; every lo_i is exact now: Σ p_i·lo_i, from the first product
                  (let loop ((i 1) (sum (* (vector-ref p 0) (vector-ref lo 0))))
                    (if (fx= i k)
                      sum
                      (loop (fx+ i 1) (+ sum (* (vector-ref p i) (vector-ref lo i)))))))
                (if probing?
                  (each probe 0 (λ () (each search 0 expected)))
                  (each search 0 expected)))))))))

  ; negamax ----------------------------------------------------------------------------------------

  ; Negamax (Knuth & Moore 1975) for turns that need not alternate: with color(n) = +1 at max
  ; nodes and −1 at min nodes, N(n) = color(n)·value(n) is the largest s·N(c) over the children
  ; c, where s = color(n)·color(c); the window of a child is (α, β) when s = +1 and (−β, −α),
  ; negating the result, when s = −1. The procedures answer color(n)·N(n), the ordinary value. N
  ; maximizes, so its children go to the max side of `fold`, through S, which gives s·N(c).
  (define (negamax-searcher name instead fold)
    (λ (game st order table)
      (when table (error (string-append name ": no table")))
      (let ((evaluate (game-evaluate game))
            (maximizing? (game-maximizing? game))
            (chance? (game-chance? game))
            (moves (moves-of game order))
            (refusal (string-append name ": chance node, use " instead)))
        (define (min-side? n) ; is color(n) = −1? Asked before anything else of n, it refuses chance nodes
          (when (chance? n) (error refusal))
          (not (maximizing? n)))
        (define (leaf n) (leaf! st) (if (maximizing? n) (evaluate n) (- (evaluate n))))
        (define (N n d α β)
          (enter! st)
          (cond
            ((and d (fx= d 0)) (horizon! st) (leaf n))
            (else (stream:dest/car+cdr (moves n)
                                       ((c cs) (let ((side (not (maximizing? n))))
                                                 (define (S c d α β) ; s·N(c)
                                                   (if (eq? side (min-side? c))
                                                     (N c d α β)
                                                     (- (N c d (- β) (- α)))))
                                                 (fold S st #t c cs (and d (fx- d 1)) α β)))
                                       (else (leaf n))))))
        (λ (n d α β)
          (if (min-side? n) (- (N n d (- β) (- α))) (N n d α β))))))

  ; the value procedures ---------------------------------------------------------------------------

  ; (proc game node #!key depth α β stats order table) → the value of node; the brute-force
  ; procedures ignore the window.
  (define (rated name make-R window?)
    (let ((bad-depth (string-append name ": depth must be #f or a nonnegative fixnum")))
      (λ (game node #!key (depth #f) (α -inf.0) (β +inf.0) (stats (make-game-stats)) (order #f) (table #f))
        (unless (or (not depth) (and (fixnum? depth) (fx>= depth 0))) (error bad-depth depth))
        ((make-R game stats order table) node depth (if window? α -inf.0) (if window? β +inf.0)))))

  (define minimax (rated "minimax" (searcher (refuse "minimax: chance node, use expectiminimax") minimax-fold #f) #f))
  (define negamax (rated "negamax" (negamax-searcher "negamax" "expectiminimax" minimax-fold) #f))
  (define αβ (rated "αβ" (searcher (refuse "αβ: chance node, use star1") αβ-fold #f) #t))
  (define αβ/fail-hard (rated "αβ/fail-hard" (searcher (refuse "αβ/fail-hard: chance node, use star1") αβ-fold #t) #t))
  (define αβ/negamax (rated "αβ/negamax" (negamax-searcher "αβ/negamax" "star1" αβ-fold) #t))
  (define expectiminimax (rated "expectiminimax" (searcher expectation minimax-fold #f) #f))
  (define star1 (rated "star1" (searcher (star "star1" #f) αβ-fold #f) #t))
  (define star2 (rated "star2" (searcher (star "star2" #t) αβ-fold #f) #t))

  ; move ordering ----------------------------------------------------------------------------------

  ; an `order:` that sorts the children by (score child), computed once per child: descending at
  ; max nodes, ascending at min nodes, stable on ties.
  (define (order-by game score)
    (let ((maximizing? (game-maximizing? game)))
      (λ (node children)
        (let ((better? (if (maximizing? node)
                         (λ (a b) (> (car a) (car b)))
                         (λ (a b) (< (car a) (car b))))))
          (map cdr (sort (map (λ (c) (cons (score c) c)) children) better?))))))

  ; the decision -----------------------------------------------------------------------------------

  (define-record decision value index child ratings depth stats)

  ; LoL's `handle-computer`: rate the children of the root in stream order, narrowing the window
  ; with each rating, as `ab-get-ratings-max` does, and choose the first best. Its value and index
  ; are those of minimax (or expectiminimax) for every `rate` with property P: the ratings before
  ; the first best child are below the best value, its rating is exact, the later ones are bounds
  ; that never exceed it. As in LoL, the ratings stop at the first one at or beyond the far limit
  ; (≥ upper at a max root, ≤ lower at a min root), which the default infinite limits never cut
  ; for finite values: the value is then a fail-high (fail-low) bound, as LoL's, and no child is
  ; ever searched in an empty window.
  (define (best-move rate game root #!key (depth #f) (lower -inf.0) (upper +inf.0) (stats (make-game-stats))
                     (order #f) (table #f))
    (enter! stats)
    (cond
      (((game-chance? game) root) (error "best-move: root is a chance node"))
      ((and depth (not (and (fixnum? depth) (fx> depth 0)))) (error "best-move: depth must be positive" depth)))
    (let ((max? ((game-maximizing? game) root))
          (d (and depth (fx- depth 1))))
      (stream:dest/car+cdr ((game-children game) root)
                           ((c cs)
                            (let loop ((i 0) (c c) (cs cs) (α lower) (β upper) (best-i #f) (best #f) (best-c #f) (ratings '()))
                              (let* ((x (rate game c depth: d α: α β: β stats: stats order: order table: table))
                                     (better? (or (not best-i) (if max? (> x best) (< x best))))
                                     (best-i (if better? i best-i))
                                     (best (if better? x best))
                                     (best-c (if better? c best-c))
                                     (ratings (cons x ratings)))
                                (define (decision) (make-decision best best-i best-c (reverse ratings) depth stats))
                                (if (if max? (>= x upper) (<= x lower))
                                  (decision)
                                  (stream:dest/car+cdr cs
                                                       ((c cs) (loop (fx+ i 1) c cs
                                                                     (if max? (larger x α) α)
                                                                     (if max? β (smaller x β))
                                                                     best-i best best-c ratings))
                                                       (else (decision)))))))
                           (else (error "best-move: no moves")))))

  ; the value of (thunk), or #f as soon as the count of nodes entered exceeds `limit`; the limit
  ; and the escape live in the stats only while (thunk) runs, however it exits, an error included,
  ; so that a later search on the same stats never calls a dead continuation.
  (define (within-limit st limit thunk)
    (if (not limit)
      (thunk)
      (call-with-current-continuation
        (λ (escape)
          (dynamic-wind
            (λ () (game-stats-escape-set! st escape) (game-stats-limit-set! st limit))
            thunk
            (λ () (game-stats-limit-set! st #f) (game-stats-escape-set! st #f)))))))

  ; Iterative deepening (Slate & Atkin 1977): best-move to depth 1, 2, …, on shared stats and
  ; table, until `max-depth`, or until an iteration never reached the horizon (its search was then
  ; the search of the whole tree), or until the budget runs out in the middle of an iteration
  ; (the first one always completes): the decision of the deepest completed iteration. The budget
  ; counts the nodes entered by this call, all iterations together.
  (define (best-move/iterative-deepening rate game root #!key (max-depth #f) (budget #f) (lower -inf.0)
                                         (upper +inf.0) (stats (make-game-stats)) (order #f) (table #f))
    (when (and max-depth (not (and (fixnum? max-depth) (fx> max-depth 0))))
      (error "best-move/iterative-deepening: max-depth must be positive" max-depth))
    (let ((limit (and budget (fx+ (game-stats-nodes stats) budget))))
      (let deepen ((k 1) (previous #f))
        (game-stats-horizon?-set! stats #f)
        (let* ((search (λ () (best-move rate game root depth: k lower: lower upper: upper stats: stats
                                        order: order table: table)))
               (d (if previous (within-limit stats limit search) (search))))
          (cond
            ((not d) previous)
            ((or (and max-depth (fx>= k max-depth)) (not (game-stats-horizon? stats))) d)
            (else (deepen (fx+ k 1) d)))))))

  ; the indices of the best moves from root, one best-move per ply, until a leaf, the depth, or a
  ; chance child (whose index is the last one).
  (define (principal-variation rate game root #!key (depth #f) (stats (make-game-stats)) (order #f) (table #f))
    (let ((chance? (game-chance? game)))
      (let pv ((n root) (depth depth))
        (if (or (and depth (fx= depth 0)) (chance? n) (stream:null? ((game-children game) n)))
          '()
          (let ((d (best-move rate game n depth: depth stats: stats order: order table: table)))
            (cons (decision-index d)
                  (let ((c (decision-child d)))
                    (if (chance? c) '() (pv c (and depth (fx- depth 1)))))))))))

  )
