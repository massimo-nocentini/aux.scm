
; (aux games dice-of-doom): the Dice of Doom game of "Land of Lisp" (Conrad Barski, ch. 15 and 18),
; ported from the old `on-scheme` repository (dice-of-doom.scm); only the live Scheme code is kept,
; the Common Lisp reference blocks are not.
;
; Differences from the original:
;   - the `loop` egg is gone: its seven sites are rewritten with `do`, named `let` and srfi-1;
;   - `define-record-printer` is gone in CHICKEN 6, so printers are set with `set-record-printer!`;
;     the board printer writes *everything* to its port (the original leaked the newlines and the
;     indentation to the current output port);
;   - streams come from (aux stream sicp): `Λ` is `λ§`, `stream:append-map` is sequential (so the
;     moves keep their source-by-source, neighbor-by-neighbor order, which the AI depends on,
;     because it picks the *first* best move) and `stream:foldr` is lazy;
;     `series:range` is `(list->stream (iota n))`;
;   - `game-tree` is memoized on `(board rotation spare-dice first-move)`, where `rotation` is the
;     finite list of the players met walking the circular `players` once from `(car players)`
;     (see `players-rotation`), not on the players list itself: `equal?` on two distinct circular
;     lists never terminates. The key keeps the whole turn order, as the original did, so two
;     games with the same first player but different turn orders get different trees.
;     Hence it uses `hash-table-ref/store` from (aux tabling) instead of `define-tabled` (whose key
;     is the whole argument list); `neighbors` still uses `define-tabled`;
;   - `winners` uses `remove-duplicates/last` from (aux commons), that keeps the original tie order;
;   - `game-tree->sexp` is new: it forces the moves recursively, so a tree can be compared;
;   - dropped as dead code: `game`, `num-players`, `add-new-dice₀` and `rate-position/tabled`.
;
; Kept as in the original (they differ from Land of Lisp): `add-new-dice` has no cap on the dice
; of a cell, `add-passing-move` reinforces with `(sub1 spare-dice)` dice, and `handle-computer`
; picks the next tree among the moves of the *pruned* tree, so in `computer-vs-computer` the game
; ends when the search horizon of the first move is exhausted.

(module (aux games dice-of-doom) *

  (import scheme
          (chicken base)
          (chicken format)
          (chicken fixnum)
          (chicken random)
          (only srfi-1 first second iota list-tabulate list-index count remove)
          srfi-69
          (aux base)
          (only (aux commons) ○ ² ≠ equals-to? fvector-ref flist-ref remove-duplicates/last to-string)
          (only (aux tabling) hash-table-ref/store define-tabled)
          (aux stream sicp))

  ; boards -----------------------------------------------------------------------------------------

  (define-record board cells size)

  (define board-cell@ (λ (b) (fvector-ref (board-cells b))))
  (define board-player@cell (λ (b) (○ first (board-cell@ b))))
  (define board-dices@cell (λ (b) (○ second (board-cell@ b))))
  (define board-hexnum (○ ² board-size))

  ; `players` is a list of player names, `dices` the max number of dice on a cell.
  (define gen-board
    (λ (size players dices)
      (let* ((select-player (flist-ref players))
             (num-players (length players))
             (choose-player (○ select-player pseudo-random-integer (K num-players)))
             (assign-dices (○ add1 pseudo-random-integer (K dices)))
             (ctors (list choose-player assign-dices))
             (R (λ (i) (map ($ i) ctors))))
        (make-board (list->vector (list-tabulate (² size) R)) size))))

  (set-record-printer! board
    (λ (b out)
      (let ((size (board-size b)))
        (do ((y 0 (add1 y)))
          ((= y size))
          (newline out)
          (do ((i 0 (add1 i)))
            ((= i (- size y)))
            (display "  " out))
          (do ((x 0 (add1 x)))
            ((= x size))
            (let1 (hex ((board-cell@ b) (+ x (* size y))))
              (format out "~a-~a " (first hex) (second hex))))))))

  ; the game tree ----------------------------------------------------------------------------------

  ; `players` is a circular list: `(car players)` plays now, `(cdr players)` are the next ones.
  ; `players-rotation` answers the players of one turn, in order, as a finite list: it walks the
  ; cycle until it gets back to the pair it started from (a finite list is accepted as well).
  (define players-rotation
    (λ (players)
      (let R ((p (cdr players)) (acc (list (car players))))
        (cond
          ((or (null? p) (eq? p players)) (reverse acc))
          (else (R (cdr p) (cons (car p) acc)))))))

  ; A tree is `(player board moves)`, where `moves` is a stream of `(action tree)`.
  (define game-tree
    (let1 (↑ (hash-table-ref/store (make-hash-table test: equal?)))
      (λ (board players spare-dice first-move)
        (↑ (list board (players-rotation players) spare-dice first-move)
           (λ (board rotation spare-dice first-move)
             (let* ((attack-moves (attacking-moves board players spare-dice))
                    (moves (add-passing-move board players spare-dice first-move attack-moves)))
               (list (car rotation) board moves)))))))

  (define add-passing-move
    (λ§ (board players spare-dice first-move moves)
      (cond
        (first-move moves)
        (else (let* ((new-board (add-new-dice board (car players) (sub1 spare-dice)))
                     (tree (game-tree new-board (cdr players) 0 #t)))
                (stream:cons (list 'pass tree) moves))))))

  (define attacking-moves
    (λ§ (board players spare-dice)
      (let ((S (λ (src)
                 (let ((player (car players)))
                   (cond
                     ((equal? ((board-player@cell board) src) player)
                      (let ((D (λ (dst)
                                 (let ((dices@src-cell ((board-dices@cell board) src))
                                       (dices@dst-cell ((board-dices@cell board) dst)))
                                   (cond
                                     ((and
                                        (≠ ((board-player@cell board) dst) player)
                                        (> dices@src-cell dices@dst-cell))
                                      (stream:singleton
                                        (list
                                          `(cell ,src attacks ,dst) ; description
                                          (game-tree
                                            (board-attack board player src dst dices@src-cell)
                                            players (+ spare-dice dices@dst-cell) #f))))
                                     (else stream:empty))))))
                        ((stream:append-map D) (neighbors src board))))
                     (else stream:empty))))))
        ((stream:append-map S) (list->stream (iota (board-hexnum board)))))))

  (define-tabled neighbors
    (lambda (pos board)
      (let* ((size (board-size board))
             (up (- pos size))
             (down (+ pos size))
             (divisible-by-size? (λ (v) (zero? (modulo v size))))
             (l₁ (cond
                   ((not (divisible-by-size? pos)) (list (sub1 up) (sub1 pos)))
                   (else '())))
             (l₂ (cond
                   ((not (divisible-by-size? (add1 pos))) (list (add1 pos) (add1 down)))
                   (else '())))
             (l (append (list up down) l₁ l₂))
             (hexnum (board-hexnum board)))
        (list->stream
          (let F ((l l))
            (cond
              ((null? l) '())
              ((and (>= (car l) 0) (< (car l) hexnum)) (cons (car l) (F (cdr l))))
              (else (F (cdr l)))))))))

  (define board-attack
    (λ (board player src dst dice)
      (let* ((cells (board-cells board))
             (new-cells (let F ((pos (sub1 (vector-length cells))) (acc '()))
                          (cond
                            ((negative? pos) acc)
                            (else (F (sub1 pos)
                                     (cons (cond
                                             ((equal? pos src) (list player 1))
                                             ((equal? pos dst) (list player (sub1 dice)))
                                             (else (vector-ref cells pos)))
                                           acc)))))))
        (make-board (list->vector new-cells) (board-size board)))))

  (define add-new-dice
    (λ (board player spare-dice)
      (letrec ((F (λ (lst n acc)
                    (cond
                      ((null? lst) (reverse acc))
                      ((zero? n) (append (reverse acc) lst))
                      (else (let ((cur-player (caar lst))
                                  (cur-dice (cadar lst)))
                              (cond
                                ((equal? cur-player player) (F (cdr lst) (sub1 n) (cons (list cur-player (add1 cur-dice)) acc)))
                                (else (F (cdr lst) n (cons (car lst) acc))))))))))
        (let ((cells (F ((○ vector->list board-cells) board) spare-dice '())))
          (make-board (list->vector cells) (board-size board))))))

  ; forces the whole (finite) tree into nested lists `(player board ((action tree) ...))`,
  ; where each board is mapped by `show:` (by default, to its printed representation).
  (define game-tree->sexp
    (λ (tree #!key (show to-string))
      (let T ((tree tree))
        (list (car tree)
              (show (cadr tree))
              (map (λ (move) (list (car move) (T (cadr move))))
                   (stream:->list (caddr tree)))))))

  (define-record gametree player board moves)

  (set-record-printer! gametree
    (λ (tree out)
      (format out "\nCurrent player: ~a\nBoard:~a"
              (gametree-player tree) (gametree-board tree))))

  ; the end of the game ----------------------------------------------------------------------------

  (define winners
    (λ (board)
      (let* ((tally (map car (vector->list (board-cells board))))
             (C (λ (player) (cons player (count (equals-to? player) tally))))
             (totals (map C (remove-duplicates/last tally)))
             (best (apply max (map cdr totals))))
        (map car (remove (λ (x) (≠ (cdr x) best)) totals)))))

  (define announce-winner
    (λ (board)
      (newline)
      (let ((w (winners board)))
        ((K w)
         (cond
           ((> (length w) 1) (format #t "The game is a tie between ~a" w))
           (else (format #t "The winner is ~a" (car w))))))))

  ; the AI -----------------------------------------------------------------------------------------

  (define rate-position
    (λ (player)
      (λ (tree)
        (let ((moves (caddr tree)))
          (cond
            ((not (stream:null? moves)) (let ((opt (if (equal? (car tree) player) max min)))
                                          (apply opt (get-ratings tree player))))
            (else (score-board (cadr tree) player)))))))

  (define get-ratings
    (λ (tree player)
      (let ((R (○ (rate-position player) cadr))
            (moves (caddr tree)))
        ((○ stream:->list (stream:map R)) moves))))

  (define handle-computer
    (λ (tree #!key (handler identity))
      (let* ((tree (handler tree))
             (ratings (get-ratings/αβ-max tree (car tree) most-positive-fixnum most-negative-fixnum))
             (maximum (apply max ratings))
             (n (list-index (equals-to? maximum) ratings)) ; here we can add non-determinism when there is more than one maximum.
             (moves (caddr tree)))
        (cadr ((stream:ref n) moves)))))

  (define computer-vs-computer
    (λ (AI-level)
      (let ((pruning (limit-tree-depth AI-level)))
        (letrec ((P (λ (tree)
                      (display (make-gametree (car tree) (cadr tree) (caddr tree)))
                      (cond
                        ((stream:null? (caddr tree)) (announce-winner (cadr tree)))
                        (else (P (handle-computer tree handler: pruning)))))))
          P))))

  ; improvements for the AI ------------------------------------------------------------------------

  (define limit-tree-depth
    (λ (depth)
      (λ (tree)
        (letrec ((L (λ (tree depth)
                      (list
                        (car tree)
                        (cadr tree)
                        (cond
                          ((zero? depth) stream:empty)
                          (else (let ((L₀ (λ (move)
                                            (list
                                              (car move)
                                              (L (cadr move) (sub1 depth))))))
                                  ((stream:map L₀) (caddr tree)))))))))
          (L tree depth)))))

  (define score-board
    (λ (board player)
      (let* ((cells (board-cells board))
             (n (vector-length cells)))
        (let S ((pos 0) (sum 0))
          (cond
            ((= pos n) sum)
            (else (let1 (hex (vector-ref cells pos))
                    (S (add1 pos)
                       (+ sum (cond
                                ((equal? (car hex) player) (if (threatened pos board) 1 2))
                                (else -1)))))))))))

  (define threatened
    (λ (pos board)
      (let* ((cell@ (board-cell@ board))
             (hex (cell@ pos))
             (player (car hex))
             (dice (cadr hex))
             (T (stream:foldr
                  (λ (n α)
                    (let* ((nhex (cell@ n))
                           (nplayer (car nhex))
                           (ndice (cadr nhex)))
                      (or (and (not (equal? player nplayer)) (> ndice dice)) (force α))))
                  (λ () #f))))
        (T (neighbors pos board)))))

  ; α-β pruning ------------------------------------------------------------------------------------

  (define get-ratings/αβ-max
    (λ (tree player upper-limit lower-limit)
      (letrec ((F (λ (moves lower-limit)
                    (cond
                      ((stream:null? moves) '())
                      (else (let ((x (rate-position/αβ (cadr (stream:car moves)) player upper-limit lower-limit)))
                              (cond
                                ((>= x upper-limit) (list x))
                                (else (cons x (F (stream:cdr moves) (max x lower-limit)))))))))))
        (F (caddr tree) lower-limit))))

  (define get-ratings/αβ-min
    (λ (tree player upper-limit lower-limit)
      (letrec ((F (λ (moves upper-limit)
                    (cond
                      ((stream:null? moves) '())
                      (else (let ((x (rate-position/αβ (cadr (stream:car moves)) player upper-limit lower-limit)))
                              (cond
                                ((<= x lower-limit) (list x))
                                (else (cons x (F (stream:cdr moves) (min x upper-limit)))))))))))
        (F (caddr tree) upper-limit))))

  (define rate-position/αβ
    (λ (tree player upper-limit lower-limit)
      (let ((moves (caddr tree)))
        (if (not (stream:null? moves))
          (if (equal? (car tree) player)
            (apply max (get-ratings/αβ-max tree player upper-limit lower-limit))
            (apply min (get-ratings/αβ-min tree player upper-limit lower-limit)))
          (score-board (cadr tree) player)))))

  )
