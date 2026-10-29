; Tests of (aux search adversarial) on real Dice of Doom trees of "Land of Lisp" (Conrad Barski,
; chapters 18 and 20), through the game `dice-of-doom/game` of (aux games dice-of-doom app): the
; generic procedures against the ports of the book's own AI, v4's chance-weighted `get-ratings` and
; v2's `rate-position` and `ab-rate-position`, on boards drawn from seeds; then the AI of the app,
; Star1 with a transposition table by iterative deepening under a node budget, against v4's
; `handle-computer` on mid-game positions, with the timings of every configuration printed.
;
; v4's parameters are in force (4 players, 5 dice, 5x5); the 2-player and the v2 cases parameterize
; them, and clear the memo of `neighbors`, keyed on the position alone as upstream, around a
; change of size. The seeds give their variety in their first 8 bytes, which are all CHICKEN 6
; reads. The positions depend on CHICKEN's generator, the assertions on them do not.

(import scheme
        (chicken base)
        (chicken fixnum)
        (chicken random)
        (chicken bytevector)
        (chicken time)
        (chicken gc)
        (chicken format)
        (only (chicken string) conc)
        (only srfi-1 iota every filter list-index take)
        srfi-69
        (aux unittest)
        (aux base)
        (aux stream sicp)
        (aux search adversarial)
        (aux games dice-of-doom v4)
        (prefix (only (aux games dice-of-doom v2) game-tree game-tree/previous limit-tree-depth get-ratings
                      ab-get-ratings-max handle-computer)
                v2:)
        (only (aux games dice-of-doom app) dice-of-doom/game dice-of-doom/move->child dice-of-doom/tree-hash
              choose-computer-move))

(define (with-board-size n thunk)
  (parameterize ((*board-size* n) (*board-hexnum* (* n n)))
    (dynamic-wind (τ (hash-table-clear! neighbors/previous)) thunk (τ (hash-table-clear! neighbors/previous)))))

(define (seeded seed thunk) (set-pseudo-random-seed! (string->utf8 seed)) (thunk))

; v4's `handle-computer` choice: the first index of the maximum, compared with `=`.
(define (lol-index ratings)
  (let1 (best (apply max ratings))
    (list-index (λ (r) (= r best)) ratings)))

(define (seeds tag n) (map (λ (i) (conc i "-" tag)) (iota n)))

; the odds of the book as exact rationals: with them every product and sum is exact.
(define (vector-map* f v) (list->vector (map f (vector->list v))))

(define exact-odds (vector-map* (λ (row) (vector-map* (λ (p) (/ (inexact->exact (round (* 100 p))) 100)) row))
                                (*dice-odds*)))

(define (decide rate tree depth #!key (odds (*dice-odds*)) (table #f))
  (let* ((g (dice-of-doom/game (car tree) odds: odds))
         (st (make-game-stats)))
    (best-move rate g tree depth: depth stats: st table: (and table (make-transposition-table g)))))

; the same game whose `evaluate` checks the bounds on every leaf it is called on.
(define (checked game n)
  (make-game children: (game-children game)
             evaluate: (λ (tree)
                         (let1 (v ((game-evaluate game) tree))
                           (unless (and (<= (- n) v) (<= v (* 2 n))) (error "score out of bounds" v))
                           v))
             maximizing?: (game-maximizing? game)
             bounds: (game-bounds game)
             key: (game-key game) test: (game-test game) hash: (game-hash game)))

;;; mid-game positions -------------------------------------------------------------------------------

; (board player spare-dice first-move) after `plies` random moves from a seeded board, the dice
; rolled at every attack, the pass taken with probability 1/3 when allowed; #f if the game ended or
; the position has a human (player 0) or fewer than 4 moves to choose from.
(define (random-position seed plies)
  (set-pseudo-random-seed! (string->utf8 seed))
  (let walk ((board (gen-board)) (player 0) (spare 0) (first #t) (k 0))
    (let1 (moves (stream:->list (caddr (game-tree board player spare first))))
      (cond
        ((null? moves) #f)
        ((>= k plies) (and (not (zero? player)) (> (length moves) 3) (list board player spare first)))
        (else
          (let* ((attacks (filter car moves))
                 (pass (and (not (car (car moves))) (car moves)))
                 (move (if (or (null? attacks) (and pass (zero? (pseudo-random-integer 3))))
                         pass
                         (list-ref attacks (pseudo-random-integer (length attacks))))))
            (if (not (car move))
              (let1 (next (cadr move)) (walk (cadr next) (car next) 0 #t (add1 k)))
              (let* ((dice (λ (pos) (cadr (vector-ref board pos))))
                     (src (caar move))
                     (dst (cadar move))
                     (next (if (> (roll src) (roll dst)) (cadr move) (caddr move))))
                (walk (cadr next) player (+ spare (dice dst)) #f (add1 k))))))))))

(define (roll n) (let R ((n n) (s 0)) (if (zero? n) s (R (sub1 n) (+ s 1 (pseudo-random-integer 6))))))

(define positions
  (let loop ((i 0) (acc '()))
    (if (= (length acc) 24)
      (reverse acc)
      (let1 (p (random-position (conc i "-mid-game") (+ 4 (modulo (* i 7) 45))))
        (loop (add1 i) (if (and p (not (member p acc))) (cons p acc) acc))))))

; the root of a position, built afresh, so that every search pays for the tree it forces.
(define (fresh-root position)
  (hash-table-clear! game-tree/previous)
  (apply game-tree position))

(define (timed thunk)
  (gc #t)
  (let* ((t0 (current-process-milliseconds))
         (v (thunk)))
    (values v (- (current-process-milliseconds) t0))))

; the configurations of the app's benchmark: name -> (λ (tree level stats) decision)
(define configurations
  `((expectiminimax . ,(λ (t l st) (let1 (g (dice-of-doom/game (car t))) (best-move expectiminimax g t depth: l stats: st))))
    (star1 . ,(λ (t l st) (let1 (g (dice-of-doom/game (car t))) (best-move star1 g t depth: l stats: st))))
    (star2 . ,(λ (t l st) (let1 (g (dice-of-doom/game (car t))) (best-move star2 g t depth: l stats: st))))
    (star1+table . ,(λ (t l st) (let1 (g (dice-of-doom/game (car t)))
                                  (best-move star1 g t depth: l stats: st table: (make-transposition-table g)))))
    (star2+table . ,(λ (t l st) (let1 (g (dice-of-doom/game (car t)))
                                  (best-move star2 g t depth: l stats: st table: (make-transposition-table g)))))
    (app . ,(λ (t l st) (let1 (g (dice-of-doom/game (car t)))
                          (best-move/iterative-deepening star1 g t max-depth: l budget: 100000 stats: st
                                                         table: (make-transposition-table g)))))))

; for each configuration: (name total-ms total-nodes slowest-ms indices)
(define (benchmark level)
  (map (λ (c)
         (let loop ((ps positions) (ms 0) (nodes 0) (slowest 0) (indices '()))
           (if (null? ps)
             (list (car c) ms nodes slowest (reverse indices))
             (let* ((root (fresh-root (car ps)))
                    (st (make-game-stats)))
               (receive (d t) (timed (τ ((cdr c) root level st)))
                 (loop (cdr ps) (+ ms t) (+ nodes (game-stats-nodes st)) (max slowest t)
                       (cons (decision-index d) indices)))))))
       configurations))

(define (show-benchmark level rows)
  (for-each (λ (r) (format #t "level ~a ~a: ~a ms, ~a nodes, slowest ~a ms~%" level (car r) (cadr r) (caddr r) (cadddr r)))
            rows))

(define b5/4p #((0 3) (1 2) (2 1) (3 1) (0 2) (1 3) (0 1) (1 1) (0 2) (1 2) (0 3) (0 1) (2 2) (3 3) (0 2)
                (1 1) (0 3) (1 2) (2 1) (3 5) (0 2) (1 1) (1 3) (0 3) (1 2)))

(define-suite search-adversarial-dice-of-doom-suite

  ; v4's chance-weighted get-ratings is expectiminimax, the AI of the book its best-move
  ((test/dod-search/expectiminimax _)
   (let1 (t (game-tree b5/4p 0 0 #t))
     (⊦= '(4.086 4.86 4.28 4.6032 5.2 3.82 5.2 5.2 4.84 4.32 4.6152 4.84 3.54 3.97 4.46 4.6152 4.08 3.87 5.2368 4.59 4.46 4.08 3.87 4.08)
         (decision-ratings (decide expectiminimax t 2))))
   (for-each (λ (seed)
               (let* ((t (seeded seed (τ (game-tree (gen-board) 0 0 #t))))
                      (d (decide expectiminimax t 2))
                      (ratings (get-ratings (limit-tree-depth t 2) 0)))
                 (⊦= ratings (decision-ratings d))
                 (⊦= (lol-index ratings) (decision-index d))
                 ; the child is the chance node of the move: success first, at the move's odds
                 (let* ((move ((stream:ref (decision-index d)) (caddr t)))
                        (outcomes (chance-node-outcomes (decision-child d))))
                   (⊨ (eq? (cadr move) (cdar outcomes)))
                   (⊨ (eq? (caddr move) (cdadr outcomes)))
                   (⊦= (vector-ref (vector-ref (*dice-odds*) (sub1 (cadr (vector-ref (cadr t) (cadar move)))))
                                   (- (cadr (vector-ref (cadr t) (caar move))) 2))
                       (caar outcomes)))))
             (seeds "v4-board" 4))
   ; two players on 3x3
   (parameterize ((*num-players* 2))
     (with-board-size 3
       (τ (let1 (trees (filter (λ (t) (> (length (stream:->list (caddr t))) 1))
                               (map (λ (seed) (seeded seed (τ (game-tree (gen-board) 0 0 #t)))) (seeds "3x3-board" 12))))
            (⊨ (>= (length trees) 4))
            (for-each (λ (t)
                        (let* ((d (decide expectiminimax t 2))
                               (ratings (get-ratings (limit-tree-depth t 2) 0)))
                          (⊦= ratings (decision-ratings d))
                          (⊦= (lol-index ratings) (decision-index d))))
                      trees))))))

  ; with exact odds every procedure is exact: star1 and star2 give expectiminimax's value and move
  ((test/dod-search/exact-odds _)
   (parameterize ((*dice-odds* exact-odds))
     (let1 (totals (map (λ (seed)
                          (let* ((t (seeded seed (τ (game-tree (gen-board) 0 0 #t))))
                                 (e (decide expectiminimax t 2 odds: exact-odds))
                                 (ratings (get-ratings (limit-tree-depth t 2) 0)))
                            (⊨ (equal? ratings (decision-ratings e)))     ; exactly v4's numbers
                            (⊨ (every exact? ratings))
                            (for-each (λ (rate)
                                        (for-each (λ (table)
                                                    (let1 (d (decide rate t 2 odds: exact-odds table: table))
                                                      (⊨ (= (decision-value e) (decision-value d)))
                                                      (⊦= (decision-index e) (decision-index d))))
                                                  '(#f #t)))
                                      (list star1 star2))
                            (list (game-stats-nodes (decision-stats e))
                                  (game-stats-nodes (decision-stats (decide star1 t 2 odds: exact-odds))))))
                        (seeds "exact-board" 5)))
       (⊨ (< (apply + (map cadr totals)) (apply + (map car totals)))))))

  ; with the book's flonum odds the values agree within 1e-9, and a different move is a near-tie
  ((test/dod-search/flonum-odds _)
   (for-each (λ (seed)
               (let* ((t (seeded seed (τ (game-tree (gen-board) 0 0 #t))))
                      (e (decide expectiminimax t 2)))
                 (for-each (λ (rate)
                             (let1 (d (decide rate t 2))
                               (⊨ (< (abs (- (decision-value e) (decision-value d))) 1e-9))
                               (⊨ (< (abs (- (decision-value e) (list-ref (decision-ratings e) (decision-index d)))) 1e-9))))
                           (list star1 star2))))
             (seeds "float-board" 5)))

  ; v2, two players and no chance: minimax is v2's rate-position, αβ its ab-rate-position, ratings
  ; and move, which is v2's handle-computer's
  ((test/dod-search/v2 _)
   (parameterize ((*num-players* 2) (*max-dice* 3))
     (for-each (λ (n seed depth)
                 (with-board-size n
                   (τ (let* ((t (seeded seed (τ (v2:game-tree (gen-board) 0 0 #t))))
                             (g (dice-of-doom/game 0))
                             (a (best-move αβ g t depth: depth))
                             (m (best-move minimax g t depth: depth)))
                        (⊦= (v2:ab-get-ratings-max (v2:limit-tree-depth t depth) 0 most-positive-fixnum most-negative-fixnum)
                            (decision-ratings a))
                        (⊦= (v2:get-ratings (v2:limit-tree-depth t depth) 0) (decision-ratings m))
                        (⊦= (decision-index m) (decision-index a))
                        (⊨ (fixnum? (decision-value a)))
                        (⊨ (<= (game-stats-nodes (decision-stats a)) (game-stats-nodes (decision-stats m))))
                        (⊨ (eq? (parameterize ((*ai-level* depth)) (v2:handle-computer t)) (decision-child a)))
                        (hash-table-clear! v2:game-tree/previous)))))
               '(4 4 5 5)
               '("1-v2-board" "2-v2-board" "3-v2-board" "4-v2-board")
               '(4 4 4 3))))

  ; score-board lies in [−25, 50] on every leaf of every search
  ((test/dod-search/bounds _)
   (for-each (λ (seed)
               (let* ((t (seeded seed (τ (game-tree (gen-board) 0 0 #t))))
                      (g (checked (dice-of-doom/game 0) 25)))
                 (for-each (λ (rate) (best-move rate g t depth: 2)) (list expectiminimax star1 star2))))
             (seeds "bounds-board" 3))
   (⊦= '(-25 . 50) (game-bounds (dice-of-doom/game 0))))

  ; the content hash: equal positions hash alike, the hash stays within its bound
  ((test/dod-search/hash _)
   (let* ((t (game-tree b5/4p 0 0 #t))
          (u (list 0 (vector-copy* b5/4p) 'other)))
     (⊦= (dice-of-doom/tree-hash t) (dice-of-doom/tree-hash u))
     (⊭ (= (dice-of-doom/tree-hash t) (dice-of-doom/tree-hash (list 1 b5/4p '()))))
     (⊨ (< -1 (dice-of-doom/tree-hash t 97) 97))))

  ; the AI of the app, on mid-game positions: the move of v4's handle-computer at levels 2 and 3
  ((test/dod-search/app-ai _)
   (⊦= 24 (length positions))
   (for-each (λ (position)
               (let* ((t (fresh-root position))
                      (ratings (get-ratings (limit-tree-depth t 2) (car t))))
                 (⊦= (lol-index ratings) (decision-index (decide expectiminimax t 2)))
                 (⊨ (eq? ((stream:ref (lol-index ratings)) (caddr t)) (choose-computer-move t level: 2)))))
             positions)
   (for-each (λ (level)
               (let* ((rows (benchmark level))
                      (expected (list-ref (assq 'expectiminimax rows) 4))
                      (nodes (λ (name) (caddr (assq name rows)))))
                 (show-benchmark level rows)
                 (for-each (λ (r) (⊦= expected (list-ref r 4))) rows)
                 (⊨ (< (nodes 'star1) (nodes 'expectiminimax)))
                 (⊨ (< (nodes 'star1+table) (nodes 'star1)))
                 (⊨ (< (list-ref (assq 'app rows) 3) 2000))))       ; the slowest move, ms
             '(2 3))
   (for-each (λ (position)
               (let1 (t (fresh-root position))
                 (⊨ (eq? ((stream:ref (decision-index (decide expectiminimax t 3))) (caddr t))
                         (choose-computer-move t)))))
             (take positions 6)))

  )

(define (vector-copy* v) (list->vector (map (λ (hex) (list (car hex) (cadr hex))) (vector->list v))))

(unittest/✓ search-adversarial-dice-of-doom-suite)
