; (aux games dice-of-doom v1): the first version of Dice of Doom in "Land of Lisp" (Conrad Barski,
; No Starch Press 2010, chapter 15), a port of http://landoflisp.com/dice_of_doom_v1.lisp.
;
; dice_of_doom_v1.lisp carries no license notice; this file is derived from Land of Lisp and is
; distributed, like the rest of aux, under the MIT license.
;
; The module behaves as the Common Lisp image after `(load "dice_of_doom_v1.lisp")`: two players,
; at most 3 dice per hex, a 3x3 board (the file starts with 2x2 and ends with 3x3), a game tree
; computed eagerly and memoized, and a minimax AI that rates the whole tree. Upstream's names,
; argument orders and data flow are kept: a player is a fixnum `0 ... n-1`, a hex is
; `(player dice)`, a board is a vector of hexes in row-major order, a tree is
; `(player board moves)`, a move is `(action tree)` and an action is `(src dst)`, or #f (CL's
; nil) for the passing move. The three memoizations at the end of the file are, as upstream,
; redefinitions that capture the old function: here they are `set!`s of the module's own
; variables, so every caller, recursive calls included, sees the memoized version, as late
; binding does in Common Lisp.
;
; Differences from the original:
;   - the special variables `*num-players*`, `*max-dice*`, `*board-size*` and `*board-hexnum*`
;     are parameters, read as `(*board-size*)`; the second `defparameter` of the board size is a
;     call that sets them when the module is loaded, `(*board-size* 3)`. `*board-hexnum*` stays
;     a parameter of its own, not derived from `*board-size*`, so set both;
;   - `fresh-line` is new and writes a newline unconditionally: CHICKEN ports do not track their
;     column, so the transcripts have a leading empty line, and an empty line after each list of
;     moves, where CL's would not;
;   - the first, non-tail-recursive `add-new-dice` of the file is dead code in the image (the
;     second one replaces it) and is dropped;
;   - the memo tables of `neighbors`, `game-tree` and `rate-position` are exported as
;     `neighbors/previous`, `game-tree/previous` and `rate-position/previous`, so that tests and
;     applications can clear them; upstream keeps them in closures;
;   - upstream's `equalp` table of `game-tree` becomes an `equal?` table hashed by
;     `game-tree/key-hash`, which reads every cell of the board: `equal?-hash` stops a few levels
;     deep, so it would send almost every key to one bucket;
;   - CL's `(position (apply #'max ratings) ratings)` compares with `=` (`list-index`), because
;     `max` is inexact-contagious in Scheme and `eqv?` would miss `2.` against `2`;
;   - `winners` keeps the order of CL's `remove-duplicates`, the last occurrence of each player,
;     in board order;
;   - `board-array` asserts the length of its list, as CL's `make-array` checks the
;     `:initial-contents`;
;   - CL's random generator and CHICKEN's draw different boards from the same seed.
;
; Kept as upstream: `neighbors` is memoized on `pos` alone, so changing the board size after a
; call answers stale neighbors (clear `neighbors/previous`); the game ends as soon as the player
; to move has no attack at the start of a turn; the memo tables grow for the whole process.

(module (aux games dice-of-doom v1)

  (*num-players* *max-dice* *board-size* *board-hexnum* board-array gen-board player-letter draw-board
    game-tree add-passing-move attacking-moves neighbors board-attack add-new-dice play-vs-human
    print-info handle-human winners announce-winner rate-position get-ratings handle-computer
    play-vs-computer fresh-line game-tree/key-hash neighbors/previous game-tree/previous
    rate-position/previous)

  (import scheme
          (chicken base)
          (chicken format)
          (chicken fixnum)
          (chicken random)
          (only (scheme base) make-parameter)
          (only srfi-1 iota append-map count filter remove list-index)
          srfi-69
          (aux base)
          (only (aux commons) remove-duplicates/last))

  (define *num-players* (make-parameter 2))
  (define *max-dice* (make-parameter 3))
  (define *board-size* (make-parameter 2))
  (define *board-hexnum* (make-parameter (* (*board-size*) (*board-size*))))

  ; CL's `fresh-line`, without the column test (see the header).
  (define fresh-line (λ () (newline)))

  ; the board --------------------------------------------------------------------------------------

  (define board-array
    (λ (lst)
      (assert (= (length lst) (*board-hexnum*)) 'board-array "wrong number of hexes" lst)
      (list->vector lst)))

  ; for each hex in order, the player is drawn before the dice, as in upstream's `loop`.
  (define gen-board
    (λ ()
      (board-array (let G ((n 0))
                     (cond
                       ((< n (*board-hexnum*)) (let* ((player (pseudo-random-integer (*num-players*)))
                                                      (dice (add1 (pseudo-random-integer (*max-dice*)))))
                                                 (cons (list player dice) (G (add1 n)))))
                       (else '()))))))

  (define player-letter
    (λ (n)
      (integer->char (+ 97 n))))

  (define draw-board
    (λ (board)
      (do ((y 0 (add1 y)))
        ((= y (*board-size*)))
        (fresh-line)
        (do ((i 0 (add1 i)))
          ((= i (- (*board-size*) y)))
          (display "  "))
        (do ((x 0 (add1 x)))
          ((= x (*board-size*)))
          (let1 (hex (vector-ref board (+ x (* (*board-size*) y))))
            (format #t "~a-~a " (player-letter (car hex)) (cadr hex)))))))

  ; the game tree ----------------------------------------------------------------------------------

  (define game-tree
    (λ (board player spare-dice first-move)
      (list player
            board
            (add-passing-move board
                              player
                              spare-dice
                              first-move
                              (attacking-moves board player spare-dice)))))

  (define add-passing-move
    (λ (board player spare-dice first-move moves)
      (if first-move
        moves
        (cons (list #f
                    (game-tree (add-new-dice board player (sub1 spare-dice))
                               (modulo (add1 player) (*num-players*))
                               0
                               #t))
              moves))))

  (define attacking-moves
    (λ (board cur-player spare-dice)
      (let ((player (λ (pos) (car (vector-ref board pos))))
            (dice (λ (pos) (cadr (vector-ref board pos)))))
        (append-map (λ (src)
                      (if (eqv? (player src) cur-player)
                        (append-map (λ (dst)
                                      (if (and (not (eqv? (player dst) cur-player))
                                               (> (dice src) (dice dst)))
                                        (list
                                          (list (list src dst)
                                                (game-tree (board-attack board cur-player src dst (dice src))
                                                           cur-player
                                                           (+ spare-dice (dice dst))
                                                           #f)))
                                        '()))
                                    (neighbors src))
                        '()))
                    (iota (*board-hexnum*))))))

  (define neighbors
    (λ (pos)
      (let ((up (- pos (*board-size*)))
            (down (+ pos (*board-size*))))
        (filter (λ (p) (and (>= p 0) (< p (*board-hexnum*))))
                (append (list up down)
                        (if (zero? (modulo pos (*board-size*))) '() (list (sub1 up) (sub1 pos)))
                        (if (zero? (modulo (add1 pos) (*board-size*))) '() (list (add1 pos) (add1 down))))))))

  (define board-attack
    (λ (board player src dst dice)
      (board-array (map (λ (pos hex)
                          (cond
                            ((eqv? pos src) (list player 1))
                            ((eqv? pos dst) (list player (sub1 dice)))
                            (else hex)))
                        (iota (vector-length board))
                        (vector->list board)))))

  ; playing against another human ------------------------------------------------------------------

  (define play-vs-human
    (λ (tree)
      (print-info tree)
      (if (pair? (caddr tree))
        (play-vs-human (handle-human tree))
        (announce-winner (cadr tree)))))

  (define print-info
    (λ (tree)
      (fresh-line)
      (format #t "current player = ~a" (player-letter (car tree)))
      (draw-board (cadr tree))))

  (define handle-human
    (λ (tree)
      (fresh-line)
      (display "choose your move:")
      (let1 (moves (caddr tree))
        (let loop ((ms moves) (n 1))
          (when (pair? ms)
            (let1 (action (caar ms))
              (fresh-line)
              (format #t "~a. " n)
              (if action
                (format #t "~a -> ~a" (car action) (cadr action))
                (display "end turn")))
            (loop (cdr ms) (add1 n))))
        (fresh-line)
        (cadr (list-ref moves (sub1 (read)))))))

  (define winners
    (λ (board)
      (let* ((tally (map car (vector->list board)))
             (totals (map (λ (player) (cons player (count (λ (p) (eqv? p player)) tally)))
                          (reverse (remove-duplicates/last tally)))) ; CL's `remove-duplicates`
             (best (apply max (map cdr totals))))
        (map car (remove (λ (x) (not (eqv? (cdr x) best))) totals)))))

  (define announce-winner
    (λ (board)
      (fresh-line)
      (let1 (w (winners board))
        (if (> (length w) 1)
          (format #t "The game is a tie between ~a" (map player-letter w))
          (format #t "The winner is ~a" (player-letter (car w)))))))

  ; the AI -----------------------------------------------------------------------------------------

  (define rate-position
    (λ (tree player)
      (let1 (moves (caddr tree))
        (if (pair? moves)
          (apply (if (eqv? (car tree) player) max min)
                 (get-ratings tree player))
          (let1 (w (winners (cadr tree)))
            (if (memv player w)
              (/ 1 (length w))
              0))))))

  (define get-ratings
    (λ (tree player)
      (map (λ (move) (rate-position (cadr move) player))
           (caddr tree))))

  (define handle-computer
    (λ (tree)
      (let* ((ratings (get-ratings tree (car tree)))
             (best (apply max ratings)))
        (cadr (list-ref (caddr tree) (list-index (λ (r) (= r best)) ratings))))))

  (define play-vs-computer
    (λ (tree)
      (print-info tree)
      (cond
        ((null? (caddr tree)) (announce-winner (cadr tree)))
        ((zero? (car tree)) (play-vs-computer (handle-human tree)))
        (else (play-vs-computer (handle-computer tree))))))

  ; a 3x3 board and the memoizations ---------------------------------------------------------------

  (*board-size* 3)
  (*board-hexnum* (* (*board-size*) (*board-size*)))

  (define neighbors/previous (make-hash-table test: eqv?))

  (set! neighbors
    (let ((old-neighbors neighbors)
          (previous neighbors/previous))
      (λ (pos)
        (hash-table-ref previous pos (τ (let1 (v (old-neighbors pos))
                                           (hash-table-set! previous pos v)
                                           v))))))

  ; mixes the hash of every atom of a key `(board player spare-dice first-move)`, the player and
  ; the dice of each cell included, modulo 2³² to stay a fixnum: boards that are `equal?` hash
  ; alike, and boards that differ deep inside do not share a bucket.
  (define game-tree/key-hash
    (λ (key #!optional (bound most-positive-fixnum))
      (let1 (mix (λ (h x) (fxand (fx+ (fx* h 31) (equal?-hash x)) #xFFFFFFFF)))
        (match1/first ((,board ,player ,spare-dice ,first-move) key)
          (let* ((h (foldl (λ (h hex) (foldl mix h hex)) 0 (vector->list board)))
                 (h (mix (mix (mix h player) spare-dice) first-move)))
            (fxmod h bound))))))

  (define game-tree/previous (make-hash-table test: equal? hash: game-tree/key-hash))

  (set! game-tree
    (let ((old-game-tree game-tree)
          (previous game-tree/previous))
      (λ rest
        (hash-table-ref previous rest (τ (let1 (v (apply old-game-tree rest))
                                            (hash-table-set! previous rest v)
                                            v))))))

  ; player -> (tree -> rating), the trees by identity, which works because `game-tree` is memoized.
  (define rate-position/previous (make-hash-table test: eqv?))

  (set! rate-position
    (let ((old-rate-position rate-position)
          (previous rate-position/previous))
      (λ (tree player)
        (let1 (tab (hash-table-ref previous player (τ (let1 (t (make-hash-table test: eq?))
                                                        (hash-table-set! previous player t)
                                                        t))))
          (hash-table-ref tab tree (τ (let1 (v (old-rate-position tree player))
                                         (hash-table-set! tab tree v)
                                         v)))))))

  ; the tail-recursive version, which replaces upstream's first one.
  (define add-new-dice
    (λ (board player spare-dice)
      (letrec ((f (λ (lst n acc)
                    (cond
                      ((zero? n) (append (reverse acc) lst))
                      ((null? lst) (reverse acc))
                      (else (let ((cur-player (caar lst))
                                  (cur-dice (cadar lst)))
                              (if (and (eqv? cur-player player) (< cur-dice (*max-dice*)))
                                (f (cdr lst) (sub1 n) (cons (list cur-player (add1 cur-dice)) acc))
                                (f (cdr lst) n (cons (car lst) acc)))))))))
        (board-array (f (vector->list board) spare-dice '())))))

  )
