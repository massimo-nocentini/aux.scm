; Tests of (aux search adversarial): tic-tac-toe, whose values and node counts are known exactly,
; a poisoned tree for laziness, and random lazy trees, deterministic and with chance nodes, for 1
; to 4 paranoid players, on which every pruned procedure is compared with its brute-force
; counterpart. The expected tic-tac-toe numbers come from an independent reference implementation
; (same child order, same counting: every node entered, the root included).

(import scheme
        (chicken base)
        (chicken fixnum)
        (chicken random)
        (chicken bytevector)
        (chicken condition)
        (only (scheme base) vector-copy)
        (only srfi-1 iota every any filter take list-index)
        srfi-69
        (aux unittest)
        (aux base)
        (aux stream sicp)
        (aux search adversarial))

;;; ==================== TIC-TAC-TOE ====================

; A board is a vector of the 9 cells, 'X, 'O or '_, followed by the player to move and the winner
; (#f if none), which depend only on the cells and are kept to spare the interpreter the work of
; recomputing them at every node. X moves when the counts are equal, and X is rated.
(define ttt/lines '((0 1 2) (3 4 5) (6 7 8) (0 3 6) (1 4 7) (2 5 8) (0 4 8) (2 4 6)))

(define ttt/lines-through ; for each cell, the lines through it
  (list->vector (map (λ (i) (filter (λ (l) (memv i l)) ttt/lines)) (iota 9))))

(define (ttt/line? b l m) (every (λ (i) (eq? (vector-ref b i) m)) l))

(define (ttt/mover b) (vector-ref b 9))
(define (ttt/winner b) (vector-ref b 10))

; the boards after each move, in cell order, as a stream; none once the game is over
(define (ttt/children b)
  (if (ttt/winner b)
    '()
    (let* ((m (ttt/mover b)) (next (if (eq? m 'X) 'O 'X)))
      (let loop ((i 8) (acc '()))
        (if (fx< i 0)
          (list->stream acc)
          (loop (fx- i 1)
                (if (eq? (vector-ref b i) '_)
                  (let ((c (vector-copy b)))
                    (vector-set! c i m)
                    (vector-set! c 9 next)
                    (when (any (λ (l) (ttt/line? c l m)) (vector-ref ttt/lines-through i)) (vector-set! c 10 m))
                    (cons c acc))
                  acc)))))))

(define (ttt/evaluate b) (case (ttt/winner b) ((X) 1) ((O) -1) (else 0)))

(define ttt
  (make-game children: ttt/children evaluate: ttt/evaluate maximizing?: (λ (b) (eq? (ttt/mover b) 'X))
             bounds: '(-1 . 1) test: equal?))

(define (board s) ; "XO_______" → #(X O _ _ _ _ _ _ _ X #f)
  (let* ((cells (map (λ (c) (case c ((#\X) 'X) ((#\O) 'O) (else '_))) (string->list s)))
         (b (list->vector (append cells '(X #f)))))
    (vector-set! b 9 (if (= (length (filter (λ (c) (eq? c 'X)) cells)) (length (filter (λ (c) (eq? c 'O)) cells))) 'X 'O))
    (vector-set! b 10 (cond
                        ((any (λ (l) (ttt/line? b l 'X)) ttt/lines) 'X)
                        ((any (λ (l) (ttt/line? b l 'O)) ttt/lines) 'O)
                        (else #f)))
    b))

(define empty (board "_________"))

;;; ==================== RANDOM TREES ====================

; A pure generator: every attribute of a node derives from its seed, through the Park–Miller
; generator, so the order in which a search forces the tree cannot change it.
(define M 2147483647)
(define (next s) (modulo (* s 48271) M))

(define-record rnode seed depth kind mover value kids dist)

(define dyadic '((0.5 0.5) (0.5 0.25 0.25) (0.25 0.25 0.25 0.25) (0.5 0.25 0.125 0.125) (1.0 0.0)))
(define general '((0.84 0.16) (0.3 0.7) (0.2 0.3 0.5) (0.46 0.54)))

; a tree of depth 7 whose root is a decision node of player 0; with a `pool`, the seeds of the
; other nodes are drawn from 1..pool, so that equal subtrees (equal seed and depth) recur often.
(define (random-tree seed players branching chance-rate dyadic? #!optional (pool #f))
  (define dists (if dyadic? dyadic general))
  (define (child-seed s i)
    (let ((c (next (modulo (+ (* 16807 s) i 1) M))))
      (cond
        (pool (+ 1 (modulo c pool)))
        ((= c 0) 1)
        (else c))))
  (define (node s depth root?)
    (let* ((x₁ (next s)) (x₂ (next x₁)) (x₃ (next x₂))
           (kind (cond
                   (root? 'decision)
                   ((or (= depth 0) (< (modulo x₃ 100) 10)) 'leaf)
                   ((< (modulo (next x₃) 100) chance-rate) 'chance)
                   (else 'decision)))
           (dist (list-ref dists (modulo (next (+ s 3)) (length dists))))
           (k (if (eq? kind 'chance) (length dist) (+ 1 (modulo (next (next (+ s 7))) branching)))))
      (letrec ((C (λ§ (i)
                    (if (= i k)
                      stream:empty
                      (stream:cons (node (child-seed s i) (- depth 1) #f) (C (+ i 1)))))))
        (make-rnode s depth kind (if root? 0 (modulo x₁ players)) (+ -10 (modulo x₂ 21))
                    (if (eq? kind 'leaf) stream:empty (C 0))
                    dist))))
  (node seed 7 #t))

(define random-game
  (make-game children: rnode-kids
             evaluate: rnode-value
             maximizing?: (λ (n) (= (rnode-mover n) 0))
             chance?: (λ (n) (eq? (rnode-kind n) 'chance))
             outcomes: (λ (n) (map cons (rnode-dist n) (stream:->list (rnode-kids n))))
             bounds: '(-10 . 10)
             key: (λ (n) (+ (* 8 (rnode-seed n)) (rnode-depth n))) ; the seed and the depth make the subtree
             test: eqv?))

; LoL's `limit-tree-depth`, for any game: a node is (node . plies left), a decision node with no
; plies left has no children, a chance node passes its plies unchanged to its outcomes.
(define (limit-depth game)
  (make-game children: (λ (w)
                         (if (= (cdr w) 0)
                           '()
                           ((stream:map (λ (c) (cons c (- (cdr w) 1)))) ((game-children game) (car w)))))
             evaluate: (λ (w) ((game-evaluate game) (car w)))
             maximizing?: (λ (w) ((game-maximizing? game) (car w)))
             chance?: (λ (w) ((game-chance? game) (car w)))
             outcomes: (λ (w) (map (λ (o) (cons (car o) (cons (cdr o) (cdr w)))) ((game-outcomes game) (car w))))
             bounds: (game-bounds game)))

;;; ==================== LAND OF LISP ====================

; LoL's `ab-rate-position` and `ab-get-ratings-max/min` (dice_of_doom_v2.lisp), literally, on a
; game without chance nodes, with the limits in LoL's order (upper lower); finite limits stand
; for most-positive-fixnum and most-negative-fixnum, so that `max` and `min` stay exact.
(define (lol/ab-rate-position game n upper lower)
  (let ((moves ((game-children game) n)))
    (if (not (stream:null? moves))
      (if ((game-maximizing? game) n)
        (apply max (lol/ab-get-ratings-max game moves upper lower))
        (apply min (lol/ab-get-ratings-min game moves upper lower)))
      ((game-evaluate game) n))))

(define (lol/ab-get-ratings-max game moves upper lower)
  (let f ((moves moves) (lower lower))
    (if (stream:null? moves)
      '()
      (let ((x (lol/ab-rate-position game (stream:car moves) upper lower)))
        (if (>= x upper)
          (list x)
          (cons x (f (stream:cdr moves) (max x lower))))))))

(define (lol/ab-get-ratings-min game moves upper lower)
  (let f ((moves moves) (upper upper))
    (if (stream:null? moves)
      '()
      (let ((x (lol/ab-rate-position game (stream:car moves) upper lower)))
        (if (<= x lower)
          (list x)
          (cons x (f (stream:cdr moves) (min x upper))))))))

;;; ==================== HELPERS ====================

; the message of the error raised by thunk, or 'not-raised
(define (error-message thunk)
  (condition-case (begin (thunk) 'not-raised)
    (e (exn) ((condition-property-accessor 'exn 'message) e))))

; a position after k random moves from the empty board
(define (random-position k)
  (let play ((b empty) (k k))
    (let ((cs (stream:->list (ttt/children b))))
      (if (or (= k 0) (null? cs)) b (play (list-ref cs (pseudo-random-integer (length cs))) (sub1 k))))))

(define (nodes-of d) (game-stats-nodes (decision-stats d)))

; value, index and nodes of (best-move rate game root keys ...), on fresh stats
(define (decide rate game root . keys)
  (apply best-move rate game root stats: (make-game-stats) keys))

; properties P and H of a result r in the window (α, β), for a node of value v
(define (fail-soft? r v α β)
  (cond
    ((<= r α) (<= v r))
    ((>= r β) (>= v r))
    (else (= v r))))

(define (fail-hard? r v α β) (and (<= α r β) (fail-soft? r v α β)))

; a window drawn from the values -11..11, α < β, now and then infinite
(define (random-window)
  (let* ((a (- (pseudo-random-integer 23) 11)) (b (+ a 1 (pseudo-random-integer 8))))
    (case (pseudo-random-integer 6)
      ((0) (cons -inf.0 b))
      ((1) (cons a +inf.0))
      (else (cons a b)))))

(define (same-choice? a b)
  (and (= (decision-value a) (decision-value b)) (= (decision-index a) (decision-index b))))

;;; ==================== SUITE ====================

(define-suite search-adversarial-suite

  ((test-minimax _)
   (let* ((st (make-game-stats)) (v (minimax ttt empty stats: st)))
     (⊨ (eqv? 0 v))
     (⊦= 549946 (game-stats-nodes st))
     (⊦= 255168 (game-stats-leaves st))
     (⊦= 0 (game-stats-cutoffs st))
     (⊭ (game-stats-horizon? st))))

  ((test-αβ _)
   (define (value+nodes proc . keys)
     (let* ((st (make-game-stats)) (v (apply proc ttt empty stats: st keys)))
       (list v (game-stats-nodes st))))
   (⊦= '(0 18297) (value+nodes αβ))
   (⊦= '(0 18297) (value+nodes αβ/fail-hard))
   (⊦= '(0 18297) (value+nodes αβ/negamax))
   (⊦= '(0 16811) (value+nodes αβ α: -1 β: 1))
   (⊦= '(0 549946) (value+nodes negamax))
   ; without chance nodes, expectiminimax is minimax, and Star1 and Star2 are αβ
   (⊦= '(0 549946) (value+nodes expectiminimax))
   (⊦= '(0 18297) (value+nodes star1))
   (⊦= '(0 18297) (value+nodes star2))
   (⊨ (eqv? 0 (αβ ttt empty)))
   ; fail-hard stays in its window, fail-soft need not
   (⊦= 1/2 (αβ/fail-hard ttt (board "XX_OO____") α: -1/2 β: 1/2))
   (⊦= 1 (αβ ttt (board "XX_OO____") α: -1/2 β: 1/2))
   (⊦= -1/2 (αβ/fail-hard ttt (board "XX_OOO_X_") α: -1/2 β: 1/2))
   (⊦= -1 (αβ ttt (board "XX_OOO_X_") α: -1/2 β: 1/2)))

  ((test-depth _)
   (⊦= '(1 10 82 586 3610 18730 73450 221626 422074 549946)
       (map (λ (d)
              (let* ((st (make-game-stats)) (v (minimax ttt empty depth: d stats: st)))
                (⊨ (eqv? 0 v))
                (⊨ (game-stats-horizon? st))
                (game-stats-nodes st)))
            (iota 10)))
   (let ((st (make-game-stats)))
     (minimax ttt empty depth: 10 stats: st)
     (⊭ (game-stats-horizon? st))
     (⊦= 549946 (game-stats-nodes st))))

  ((test-best-move _)
   (let ((d (decide minimax ttt empty)))
     (⊨ (decision? d))
     (⊨ (eqv? 0 (decision-value d)))
     (⊦= 0 (decision-index d))
     (⊨ (equal? '(0 0 0 0 0 0 0 0 0) (decision-ratings d)))
     (⊨ (equal? (board "X________") (decision-child d)))
     (⊭ (decision-depth d))
     (⊦= 549946 (nodes-of d)))
   (let ((d (decide αβ ttt empty)))
     (⊨ (eqv? 0 (decision-value d)))
     (⊦= 0 (decision-index d))
     (⊨ (equal? '(0 0 0 0 0 0 0 0 0) (decision-ratings d)))
     (⊦= 18297 (nodes-of d))))

  ((test-positions _)
   ; LoL's ratings: exact for minimax; for αβ, bounds after the first best child
   (let ((mm (decide minimax ttt (board "XO_______")))
         (ab (decide αβ ttt (board "XO_______"))))
     (⊦= '(0 1 1 0 1 0 0) (decision-ratings mm))
     (⊦= 8232 (nodes-of mm))
     (⊦= '(0 1 1 1 1 1 1) (decision-ratings ab))
     (⊦= 749 (nodes-of ab))
     (⊦= '(1 1) (list (decision-index mm) (decision-value mm)))
     (⊦= '(1 1) (list (decision-index ab) (decision-value ab))))
   (let ((mm (decide minimax ttt (board "XX_OO____")))
         (ab (decide αβ ttt (board "XX_OO____"))))
     (⊦= '(1 0 -1 -1 -1) (decision-ratings mm))
     (⊦= 157 (nodes-of mm))
     (⊦= '(1 0 0 -1 -1) (decision-ratings ab))
     (⊦= 36 (nodes-of ab))
     (⊦= '(0 1) (list (decision-index mm) (decision-value mm)))
     (⊦= '(0 1) (list (decision-index ab) (decision-value ab))))
   ; a min root: O to move, rated for X
   (let ((mm (decide minimax ttt (board "XX_O_____")))
         (ab (decide αβ ttt (board "XX_O_____"))))
     (⊨ (same-choice? mm ab))
     (⊦= 0 (decision-index mm))
     (⊨ (< (nodes-of ab) (nodes-of mm)))))

  ((test-transposition-table _)
   (let* ((st (make-game-stats)) (table (make-transposition-table ttt)) (v (minimax ttt empty stats: st table: table)))
     (⊨ (eqv? 0 v))
     (⊦= 16168 (game-stats-nodes st))
     (⊦= 10690 (game-stats-tt-hits st))
     (⊦= 5478 (hash-table-size table)))
   (let* ((st (make-game-stats)) (table (make-transposition-table ttt)) (v (αβ ttt empty stats: st table: table)))
     (⊨ (eqv? 0 v))
     (⊦= 4866 (game-stats-nodes st))
     (⊦= 2073 (game-stats-tt-hits st))
     (⊦= 2511 (hash-table-size table)))
   ; a key, a test and a hash of the game's own: the cells, as a string, hashed by its procedure
   (let* ((hashed 0)
          (cells (λ (b) (list->string (map (λ (i) (string-ref (symbol->string (vector-ref b i)) 0)) (iota 9)))))
          (g (make-game children: ttt/children evaluate: ttt/evaluate maximizing?: (λ (b) (eq? (ttt/mover b) 'X))
                        key: cells test: string=? hash: (λ args (set! hashed (add1 hashed)) (apply string-hash args))))
          (st (make-game-stats))
          (table (make-transposition-table g)))
     (⊨ (eq? cells (game-key g)))
     (⊨ (eq? string=? (game-test g)))
     (⊨ (procedure? (game-hash g)))
     (⊭ (game-hash ttt))
     (⊦= 0 (αβ g empty stats: st table: table))
     (⊦= '(4866 2073 2511) (list (game-stats-nodes st) (game-stats-tt-hits st) (hash-table-size table)))
     (⊨ (hash-table-exists? table "XOXOX____"))
     (⊨ (<= 4866 hashed)))
   (let* ((table (make-transposition-table ttt)) (d (decide αβ ttt empty table: table)))
     (⊦= 0 (decision-value d))
     (⊦= 0 (decision-index d))
     (⊦= 4866 (nodes-of d))
     (⊦= 2510 (hash-table-size table)))
   ; a table shared by depth-limited searches
   (let ((table (make-transposition-table ttt)))
     (⊦= 1 (decision-value (decide αβ ttt (board "XX_OO____") depth: 2 table: table)))
     (⊦= 1 (decision-value (decide αβ ttt (board "XX_OO____") depth: 4 table: table)))
     (⊦= 0 (decision-value (decide αβ/fail-hard ttt empty depth: 9 table: (make-transposition-table ttt))))))

  ((test-principal-variation _)
   (⊦= '(0 3 0 0 2 0 0 0 0) (principal-variation αβ ttt empty))
   (⊦= '(0 3 0 0 2 0 0 0 0) (principal-variation αβ ttt empty table: (make-transposition-table ttt)))
   (⊦= '(0 0) (principal-variation αβ ttt empty depth: 2)) ; the horizon sees no difference
   (⊦= '() (principal-variation αβ ttt (board "XXXOO____"))))

  ((test-iterative-deepening _)
   (let ((d (best-move/iterative-deepening αβ ttt (board "XX_OO____"))))
     (⊦= 0 (decision-index d))
     (⊦= 1 (decision-value d))
     (⊨ (<= (decision-depth d) 6)))
   (let ((d (best-move/iterative-deepening αβ ttt (board "XX_OO____") budget: 1)))
     (⊦= 1 (decision-depth d))
     (⊦= 0 (decision-index d)))
   ; a position whose choice changes with the depth: the first child up to depth 4, the second
   ; from depth 5, which takes 8 + 20 + 119 + 232 + 380 nodes to reach, and 593 more to pass
   (let ((b (board "XO_______")))
     (⊦= '(0 0 0 0 1 1 1) (map (λ (k) (decision-index (decide αβ ttt b depth: k))) '(1 2 3 4 5 6 7)))
     (let ((small (best-move/iterative-deepening αβ ttt b budget: 200))
           (large (best-move/iterative-deepening αβ ttt b budget: 1000)))
       (⊦= '(3 0 0) (list (decision-depth small) (decision-index small) (decision-value small)))
       (⊦= '(5 1 1) (list (decision-depth large) (decision-index large) (decision-value large)))))
   (for-each (λ (k)
               (let ((id (best-move/iterative-deepening αβ ttt empty max-depth: k))
                     (d (decide αβ ttt empty depth: k)))
                 (⊦= k (decision-depth id))
                 (⊨ (same-choice? id d))
                 (⊦= (decision-ratings d) (decision-ratings id))))
             '(1 2 3 4 5))
   ; with a table, and with a budget that stops the deepening in the middle of an iteration
   (let* ((st (make-game-stats))
          (d (best-move/iterative-deepening αβ ttt empty budget: 3000 stats: st table: (make-transposition-table ttt))))
     (⊨ (<= (game-stats-nodes st) 3001))
     (⊨ (same-choice? d (decide αβ ttt empty depth: (decision-depth d)))))
   (let ((d (best-move/iterative-deepening αβ ttt empty table: (make-transposition-table ttt))))
     (⊦= 0 (decision-value d))
     (⊦= 0 (decision-index d))
     (⊨ (<= (decision-depth d) 10))))

  ((test-windows _)
   ; P and H on positions of random play, in every window from {-1, -1/2, 0, 1/2, 1, ±∞}, one
   ; table per position and procedure, shared by all the windows (so that its bounds are reused)
   (set-pseudo-random-seed! (string->utf8 "aux-search-adversarial-windows"))
   (let ((bounds '(-inf.0 -1 -1/2 0 1/2 1 +inf.0)))
     (do ((t 0 (add1 t))) ((= t 60))
       (let* ((b (random-position (+ 2 (pseudo-random-integer 4))))
              (depth (if (even? t) #f (+ 1 (pseudo-random-integer 4))))
              (v (minimax ttt b depth: depth)))
         (for-each (λ (rate property)
                     (let ((table (make-transposition-table ttt)))
                       (for-each (λ (α)
                                   (for-each (λ (β)
                                               (when (< α β)
                                                 (⊨ (property (rate ttt b depth: depth α: α β: β) v α β))
                                                 (unless (eq? rate αβ/negamax)
                                                   (⊨ (property (rate ttt b depth: depth α: α β: β table: table) v α β)))))
                                             bounds))
                                 bounds)))
                   (list αβ αβ/fail-hard αβ/negamax star1 star2)
                   (list fail-soft? fail-hard? fail-soft? fail-soft? fail-soft?))))))

  ((test-order _)
   (let* ((score (λ (b) (- (ttt/evaluate b))))
          (order (order-by ttt score))
          (plain (decide αβ ttt empty))
          (ordered (decide αβ ttt empty order: order)))
     (⊨ (same-choice? plain ordered))
     (⊦= 0 (decision-value (decide star2 ttt empty order: order))))
   ; order-by is stable, descending at max nodes and ascending at min nodes
   (let ((x-center (order-by ttt (λ (b) (if (eq? (vector-ref b 4) 'X) 1 0))))
         (o-corner (order-by ttt (λ (b) (if (eq? (vector-ref b 8) 'O) -1 0)))))
     (⊦= (map board '("____X____" "X________" "_X_______"))
         (take (x-center empty (stream:->list (ttt/children empty))) 3))
     (⊦= (map board '("XX_O____O" "XXOO_____" "XX_OO____"))
         (take (o-corner (board "XX_O_____") (stream:->list (ttt/children (board "XX_O_____")))) 3))))

  ((test-poisoned-tree _)
   ; a max root over (leaf 5, M), where M is a min node whose second child must never be forced
   (let ((poisoned (make-game children: (λ (n)
                                          (case n
                                            ((root) (list->stream '(five M)))
                                            ((M) (stream:cons 'four (δ (error "forced"))))
                                            (else '())))
                              evaluate: (λ (n) (case n ((five) 5) ((four) 4)))
                              maximizing?: (λ (n) (not (eq? n 'M)))
                              bounds: '(0 . 9))))
     (let ((d (decide αβ poisoned 'root)))
       (⊦= 0 (decision-index d))
       (⊦= 5 (decision-value d)))
     (⊦= 5 (decision-value (decide αβ/negamax poisoned 'root)))
     (⊦= 5 (decision-value (decide star2 poisoned 'root)))
     (⊦raises (exn) (decide minimax poisoned 'root))))

  ((test-errors _)
   ; `maximizing?` reads the name of a node, so it cannot be asked of a chance node: the
   ; procedures must refuse a chance node before they ask
   (let ((chancy (make-game children: (λ (n) (if (eq? n 'root) (list->stream (list (make-chance-node '((1/2 . a) (1/2 . b))))) '()))
                            evaluate: (λ (n) (if (eq? n 'a) 1 0))
                            maximizing?: (λ (n) (memv (string-ref (symbol->string n) 0) '(#\r #\a #\b))))))
     (⊦= "minimax: chance node, use expectiminimax" (error-message (λ () (minimax chancy 'root))))
     (⊦= "αβ: chance node, use star1" (error-message (λ () (αβ chancy 'root))))
     (⊦= "αβ/fail-hard: chance node, use star1" (error-message (λ () (αβ/fail-hard chancy 'root))))
     (⊦= "negamax: chance node, use expectiminimax" (error-message (λ () (negamax chancy 'root))))
     (⊦= "αβ/negamax: chance node, use star1" (error-message (λ () (αβ/negamax chancy 'root))))
     (⊦= "negamax: chance node, use expectiminimax" (error-message (λ () (negamax chancy (make-chance-node '((1 . a)))))))
     (⊦= "star1: game-bounds required" (error-message (λ () (star1 chancy 'root))))
     (⊦= "star2: game-bounds required" (error-message (λ () (star2 chancy 'root))))
     (⊦= 1/2 (expectiminimax chancy 'root))
     (⊦= "best-move: root is a chance node" (error-message (λ () (best-move expectiminimax chancy (make-chance-node '((1 . a)))))))
     (⊦= 1/2 (decision-value (best-move expectiminimax chancy 'root))))
   (⊦= "negamax: no table" (error-message (λ () (negamax ttt empty table: (make-transposition-table ttt)))))
   (⊦= "αβ/negamax: no table" (error-message (λ () (αβ/negamax ttt empty table: (make-transposition-table ttt)))))
   (⊦= "best-move: depth must be positive" (error-message (λ () (best-move αβ ttt empty depth: 0))))
   (⊦= "best-move: no moves" (error-message (λ () (best-move αβ ttt (board "XXXOO____")))))
   (⊦= "make-game: children: is required" (error-message (λ () (make-game evaluate: (K 0)))))
   ; a negative depth is refused, not taken for an unbounded one
   (⊦= "minimax: depth must be #f or a nonnegative fixnum" (error-message (λ () (minimax ttt empty depth: -1))))
   (⊦= "αβ: depth must be #f or a nonnegative fixnum" (error-message (λ () (αβ ttt empty depth: 1.5))))
   (⊦= "best-move: depth must be positive" (error-message (λ () (best-move αβ ttt empty depth: -3))))
   (⊦= "best-move/iterative-deepening: max-depth must be positive"
       (error-message (λ () (best-move/iterative-deepening αβ ttt empty max-depth: 0))))
   (⊦= 0 (minimax ttt empty depth: 0)))

  ((test-error-in-budget _)
   ; an error inside a budgeted iteration leaves no limit behind it in the stats: a later search on
   ; the same stats neither stops early nor jumps back into the deepening that the error ended
   (let* ((st (make-game-stats))
          (calls 0)
          (returns 0)
          (flaky (make-game children: ttt/children maximizing?: (λ (b) (eq? (ttt/mover b) 'X))
                            evaluate: (λ (b)
                                        (set! calls (add1 calls))
                                        (when (= calls 10) (error "flaky")) ; in the second iteration
                                        (ttt/evaluate b)))))
     (⊦= "flaky" (error-message (λ () (best-move/iterative-deepening αβ flaky (board "XO_______") budget: 100 stats: st))))
     (set! returns (add1 returns))
     (let ((d (best-move αβ ttt (board "XO_______") stats: st)))
       (⊦= 1 returns)
       (⊦= '(1 1) (list (decision-index d) (decision-value d)))
       (⊨ (< 749 (game-stats-nodes st))))))

  ((test-limits _)
   ; finite limits at the root: the ratings are LoL's, cut at the first one at or beyond the far
   ; limit, and its choice, `(position (apply #'max ratings) ratings)`
   (let* ((t (make-game children: (λ (n) (case n
                                           ((root) (list->stream '(c1 c2)))
                                           ((c1) (list->stream '(l5 l100)))
                                           ((c2) (list->stream '(g l6)))
                                           ((g) (list->stream '(l7 l100)))
                                           (else '())))
                        evaluate: (λ (n) (case n ((l5) 5) ((l6) 6) ((l7) 7) ((l100) 100)))
                        maximizing?: (λ (n) (memq n '(root c1 g)))))
          (d (best-move αβ t 'root upper: 3)))
     (⊦= '(100 6) (decision-ratings (best-move minimax t 'root)))
     (⊦= '(5) (decision-ratings d))
     (⊦= '(0 5 c1) (list (decision-index d) (decision-value d) (decision-child d)))
     (⊦= '(5) (lol/ab-get-ratings-max t ((game-children t) 'root) 3 -1000)))
   (set-pseudo-random-seed! (string->utf8 "aux-search-adversarial-limits"))
   (let ((limits '(-1000 -1 -1/2 0 1/2 1 1000)))
     (do ((t 0 (add1 t))) ((= t 30))
       (let* ((b (random-position (+ 3 (pseudo-random-integer 4))))
              (max? (eq? (ttt/mover b) 'X))
              (moves (ttt/children b)))
         (unless (ttt/winner b)
           (for-each (λ (lower)
                       (for-each (λ (upper)
                                   (when (< lower upper)
                                     (let ((d (best-move αβ ttt b lower: lower upper: upper))
                                           (ratings ((if max? lol/ab-get-ratings-max lol/ab-get-ratings-min) ttt moves upper lower)))
                                       (⊦= ratings (decision-ratings d))
                                       (⊦= (apply (if max? max min) ratings) (decision-value d))
                                       (⊦= (list-index (λ (x) (= x (decision-value d))) ratings) (decision-index d)))))
                                 limits))
                     limits))))))

  ((test-chance-nodes _)
   ; a max root choosing between a sure 3 and a gamble of 10 or 0, with exact odds
   (let* ((gamble (λ (p) (make-chance-node `((,p . ten) (,(- 1 p) . zero)))))
          (g (make-game children: (λ (n) (if (eq? n 'root) (list->stream (list 'three (gamble 1/4) (gamble 1/2))) '()))
                        evaluate: (λ (n) (case n ((three) 3) ((ten) 10) (else 0)))
                        bounds: '(0 . 10))))
     (for-each (λ (rate)
                 (let ((d (decide rate g 'root)))
                   (⊦= 2 (decision-index d))
                   (⊨ (eqv? 5 (decision-value d)))))
               (list expectiminimax star1 star2))
     (⊦= '(3 5/2 5) (decision-ratings (decide expectiminimax g 'root)))
     ; Star1 fails low on the second gamble: 1/4·10 + 3/4·0 ≤ 3 is settled without its exact value
     (let ((d (decide star1 g 'root)))
       (⊨ (<= (cadr (decision-ratings d)) 3))
       (⊨ (< 0 (game-stats-cutoffs (decision-stats d)))))
     ; the root, three, the two gambles and their four outcomes
     (let ((st (decision-stats (decide expectiminimax g 'root))))
       (⊦= '(8 2 5) (list (game-stats-nodes st) (game-stats-chance-nodes st) (game-stats-leaves st))))
     (⊨ (chance-node? (gamble 1/2)))
     (⊭ (chance-node? 'three))
     (⊦= '((1/2 . ten) (1/2 . zero)) (chance-node-outcomes (gamble 1/2)))
     (⊦= '(0 . 10) (game-bounds g)))
   (let ((st (make-game-stats)))
     (⊦= '((nodes . 0) (chance-nodes . 0) (leaves . 0) (cutoffs . 0) (probes . 0) (tt-hits . 0) (horizon? . #f))
         (game-stats->alist st))))

  ((test-random-trees _)
   (set-pseudo-random-seed! (string->utf8 "aux-search-adversarial"))
   (let ((totals (make-vector 7 0))) ; Σ nodes: minimax, αβ (deterministic); expectiminimax, star1, star2 (chance); Star2 probes, chance nodes
     (define (add! i n) (vector-set! totals i (+ (vector-ref totals i) n)))
     (do ((t 0 (add1 t))) ((= t 600))
       (let* ((players (+ 1 (modulo t 4)))
              (branching (+ 2 (modulo (quotient t 4) 4)))
              (deterministic? (= 0 (modulo t 3)))
              (dyadic? (not (= 4 (modulo t 5))))
              (pool (and (odd? (quotient t 48)) 97))
              (root (random-tree (+ 1 (pseudo-random-integer (- M 1))) players branching
                                 (if deterministic? 0 35) dyadic? pool))
              (g random-game))
         (if deterministic?
           (let ((mm (decide minimax g root))
                 (ab (decide αβ g root))
                 (hard (decide αβ/fail-hard g root))
                 (nab (decide αβ/negamax g root))
                 (nm (decide negamax g root))
                 (s1 (decide star1 g root))
                 (s2 (decide star2 g root))
                 (em (decide expectiminimax g root)))
             (for-each (λ (d) (⊨ (same-choice? mm d))) (list ab hard nab nm s1 s2 em))
             (⊦= (nodes-of ab) (nodes-of hard))
             (⊦= (nodes-of ab) (nodes-of nab))
             (⊦= (nodes-of ab) (nodes-of s1))
             (⊦= (nodes-of ab) (nodes-of s2))
             (⊦= (nodes-of mm) (nodes-of nm))
             (⊦= (nodes-of mm) (nodes-of em))
             (⊨ (<= (nodes-of ab) (nodes-of mm)))
             (⊨ (equal? (decision-ratings ab) (decision-ratings nab)))
             (let* ((w (random-window)) (α (car w)) (β (cdr w)) (v (decision-value mm)) (table (make-transposition-table g)))
               (⊨ (fail-soft? (αβ g root α: α β: β) v α β))
               (⊨ (fail-soft? (αβ g root α: α β: β table: table) v α β))
               (⊨ (fail-soft? (αβ g root α: α β: β table: table) v α β)) ; again, on the warm table
               (⊨ (fail-hard? (αβ/fail-hard g root α: α β: β) v α β))
               (⊨ (fail-soft? (αβ/negamax g root α: α β: β) v α β)))
             (⊨ (same-choice? mm (decide αβ g root table: (make-transposition-table g))))
             ; iterative deepening ends on an iteration that never reached the horizon: the whole tree
             (⊨ (same-choice? mm (best-move/iterative-deepening αβ g root)))
             (⊨ (same-choice? mm (best-move/iterative-deepening αβ g root table: (make-transposition-table g))))
             (⊨ (same-choice? (decide minimax g root depth: 3) (decide minimax (limit-depth g) (cons root 3))))
             (let ((limited (decide αβ g root depth: 3)) (wrapped (decide αβ (limit-depth g) (cons root 3))))
               (⊨ (same-choice? limited wrapped))
               (⊦= (nodes-of limited) (nodes-of wrapped)))
             (add! 0 (nodes-of mm))
             (add! 1 (nodes-of ab)))
           (let ((em (decide expectiminimax g root))
                 (s1 (decide star1 g root))
                 (s2 (decide star2 g root))
                 (s1/table (decide star1 g root table: (make-transposition-table g)))
                 (s2/table (decide star2 g root table: (make-transposition-table g))))
             (cond
               (dyadic? ; every operation is exact
                (for-each (λ (d) (⊨ (same-choice? em d))) (list s1 s2 s1/table s2/table)))
               (else
                 (for-each (λ (d) (⊨ (< (abs (- (decision-value em) (decision-value d))) 1e-9)))
                           (list s1 s2 s1/table s2/table))))
             (⊨ (<= (nodes-of s1) (nodes-of em)))
             (when dyadic?
               (⊨ (same-choice? em (best-move/iterative-deepening star2 g root table: (make-transposition-table g)))))
             (when dyadic?
               (let* ((w (random-window)) (α (car w)) (β (cdr w)) (v (expectiminimax g root)) (table (make-transposition-table g)))
                 (⊨ (fail-soft? (star1 g root α: α β: β) v α β))
                 (⊨ (fail-soft? (star2 g root α: α β: β) v α β))
                 (⊨ (fail-soft? (star2 g root α: α β: β table: table) v α β))
                 ; with a fresh table, and only where every node recurs at one remaining depth: an
                 ; entry answers any search no deeper than its own, so a node met again below fewer
                 ; chance nodes, hence with less depth left, would get the deeper value
                 (unless pool
                   (⊨ (fail-soft? (star1 g root α: α β: β table: (make-transposition-table g) depth: 4)
                                  (expectiminimax g root depth: 4) α β)))))
             (let ((limited (decide star1 g root depth: 3)) (wrapped (decide star1 (limit-depth g) (cons root 3))))
               (⊦= (decision-value limited) (decision-value wrapped))
               (⊦= (decision-index limited) (decision-index wrapped))
               (⊦= (nodes-of limited) (nodes-of wrapped)))
             (⊦= (decision-value (decide expectiminimax g root depth: 3))
                 (decision-value (decide expectiminimax (limit-depth g) (cons root 3))))
             (add! 2 (nodes-of em))
             (add! 3 (nodes-of s1))
             (add! 4 (nodes-of s2))
             (add! 5 (game-stats-probes (decision-stats s2)))
             (⊦= 0 (game-stats-probes (decision-stats s1)))
             (add! 6 (game-stats-chance-nodes (decision-stats em)))))))
     (display `(nodes (minimax ,(vector-ref totals 0)) (αβ ,(vector-ref totals 1))
                      (expectiminimax ,(vector-ref totals 2)) (star1 ,(vector-ref totals 3)) (star2 ,(vector-ref totals 4))))
     (newline)
     (⊨ (< (vector-ref totals 1) (vector-ref totals 0)))
     (⊨ (< (vector-ref totals 3) (vector-ref totals 2)))
     (⊨ (< (vector-ref totals 4) (vector-ref totals 2)))
     (⊨ (< 0 (vector-ref totals 5)))
     (⊨ (< 0 (vector-ref totals 6)))))

  )

(unittest/✓ search-adversarial-suite)
