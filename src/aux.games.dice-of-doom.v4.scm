; (aux games dice-of-doom v4): the fourth version of Dice of Doom in "Land of Lisp" (Conrad Barski,
; No Starch Press 2010, chapter 20), a port of http://landoflisp.com/dice_of_doom_v4.lisp.
;
; dice_of_doom_v4.lisp carries no license notice; this file is derived from Land of Lisp and is
; distributed, like the rest of aux, under the MIT license.
;
; The module behaves as the Common Lisp image after `(load "dice_of_doom_v4.lisp")`: four players,
; up to five dice per hex, attacks that succeed or fail by a roll of the dice (each attack is a
; chance node `((src dst) tree-success tree-fail)`, the passing move stays `(#f tree)`), an AI that
; weighs both outcomes by `*dice-odds*`, and reinforcements as many as the hexes of the player's
; largest connected cluster. Loading it sets the parameters of the earlier versions as v4.lisp's
; `defparameter`s do: `*num-players*` 4, `*max-dice*` 5, `*ai-level*` 2 and four `*die-colors*`;
; the board stays 5x5.
;
; The functions of v3 (and so of v2 and v1) that v4.lisp does not redefine are re-exported from
; (aux games dice-of-doom v3), the macros `tag` and `svg` included. In the CL image the functions
; that call a redefined one call the new definition (late binding), so this module also carries
; copies of their bodies: `game-tree` (it calls `attacking-moves` and `add-passing-move`; memoized
; with a table of its own, `game-tree/previous`), `add-passing-move` (`add-new-dice`),
; `play-vs-human` (`handle-human`), `play-vs-computer` (`handle-human`, `handle-computer`),
; `rate-position` (`get-ratings`), `web-initialize` (`game-tree`), `web-handle-computer`
; (`handle-computer`) and `dod-request-handler` (`web-initialize`, `web-handle-computer`).
;
; Differences from the original:
;   - `*dice-odds*` is a parameter; its entries are flonums, CL's are single-floats, so ratings
;     differ in the last digits, and a tie in CL may not be one here, or the reverse;
;   - CL's `(position (apply #'max ratings) ratings)` compares with `=` (`list-index`);
;   - CL's random generator and CHICKEN's roll different dice from the same seed.
;
; Kept as upstream, quirks included: on the web every human attack succeeds, because v3's
; `web-handle-human` takes the `cadr` of the move and v4.lisp does not redefine it; the computer's
; rolls are written into the page by `roll-dice`; a failed attack still adds the defender's dice
; to `spare-dice`, which `add-new-dice` ignores; the `ab-` functions of v2 survive, read only the
; success tree and are called by nothing; the game ends as soon as the player to move has no
; attack at the start of a turn, an eliminated player included.

(module (aux games dice-of-doom v4)

  (attacking-moves handle-human handle-computer get-ratings limit-tree-depth add-new-dice game-tree
    add-passing-move play-vs-human play-vs-computer rate-position web-initialize web-handle-computer
    dod-request-handler game-tree/previous board-attack-fail roll-dice roll-against pick-chance-branch
    *dice-odds* get-connected largest-cluster-size)

  (import scheme
          (chicken base)
          (chicken format)
          (chicken random)
          (only (chicken module) reexport)
          (only (scheme base) make-parameter)
          (only srfi-1 iota list-index)
          srfi-69
          (aux base)
          (aux stream sicp)
          (except (aux games dice-of-doom v3)
                  attacking-moves handle-human handle-computer get-ratings limit-tree-depth add-new-dice
                  game-tree game-tree/previous add-passing-move play-vs-human play-vs-computer rate-position
                  web-initialize web-handle-computer dod-request-handler))

  (reexport (except (aux games dice-of-doom v3)
                    attacking-moves handle-human handle-computer get-ratings limit-tree-depth add-new-dice
                    game-tree game-tree/previous add-passing-move play-vs-human play-vs-computer rate-position
                    web-initialize web-handle-computer dod-request-handler))

  (*num-players* 4)
  (*die-colors* '((255 63 63) (63 63 255) (63 255 63) (255 63 255)))

  (*max-dice* 5)
  (*ai-level* 2)

  ; chance nodes -----------------------------------------------------------------------------------

  ; an attack needs two dice at least, whatever the defender has; it leads to two trees.
  (define attacking-moves
    (λ (board cur-player spare-dice)
      (let ((player (λ (pos) (car (vector-ref board pos))))
            (dice (λ (pos) (cadr (vector-ref board pos)))))
        ((stream:append-map
           (λ (src)
             (if (eqv? (player src) cur-player)
               ((stream:append-map
                  (λ (dst)
                    (if (and (not (eqv? (player dst) cur-player))
                             (> (dice src) 1))
                      (list->stream
                        (list (list (list src dst)
                                    (game-tree (board-attack board cur-player src dst (dice src))
                                               cur-player
                                               (+ spare-dice (dice dst))
                                               #f)
                                    (game-tree (board-attack-fail board cur-player src dst (dice src))
                                               cur-player
                                               (+ spare-dice (dice dst))
                                               #f))))
                      stream:empty)))
                (list->stream (neighbors src)))
               stream:empty)))
         (list->stream (iota (*board-hexnum*)))))))

  (define board-attack-fail
    (λ (board player src dst dice)
      (board-array (map (λ (pos hex)
                          (if (eqv? pos src)
                            (list player 1)
                            hex))
                        (iota (vector-length board))
                        (vector->list board)))))

  ; the `dice-num` draws are made in order, before anything is printed.
  (define roll-dice
    (λ (dice-num)
      (let1 (total (let R ((n dice-num) (sum 0))
                     (if (zero? n)
                       sum
                       (R (sub1 n) (+ sum (add1 (pseudo-random-integer 6)))))))
        (fresh-line)
        (format #t "On ~a dice rolled ~a. " dice-num total)
        total)))

  (define roll-against
    (λ (src-dice dst-dice)
      (let* ((src-total (roll-dice src-dice))     ; the attacker rolls first
             (dst-total (roll-dice dst-dice)))
        (> src-total dst-total))))

  (define pick-chance-branch
    (λ (board move)
      (let ((dice (λ (pos) (cadr (vector-ref board pos))))
            (path (car move)))
        (if (or (not path) (roll-against (dice (car path)) (dice (cadr path))))
          (cadr move)
          (caddr move)))))

  (define handle-human
    (λ (tree)
      (fresh-line)
      (display "choose your move:")
      (let1 (moves (caddr tree))
        (let print-moves ((moves moves) (n 1))
          (unless (stream:null? moves)
            (let* ((move (stream:car moves))
                   (action (car move)))
              (fresh-line)
              (format #t "~a. " n)
              (if action
                (format #t "~a -> ~a" (car action) (cadr action))
                (display "end turn")))
            (print-moves (stream:cdr moves) (add1 n))))
        (fresh-line)
        (pick-chance-branch (cadr tree) ((stream:ref (sub1 (read))) moves)))))

  (define handle-computer
    (λ (tree)
      (let* ((ratings (get-ratings (limit-tree-depth tree (*ai-level*)) (car tree)))
             (best (apply max ratings)))
        (pick-chance-branch
          (cadr tree)
          ((stream:ref (list-index (λ (r) (= r best)) ratings)) (caddr tree))))))

  ; rows: the dice of the defender, 1 to 5; columns: the dice of the attacker, 2 to 5.
  (define *dice-odds* (make-parameter #(#(0.84 0.97 1.0 1.0)
                                        #(0.44 0.78 0.94 0.99)
                                        #(0.15 0.45 0.74 0.91)
                                        #(0.04 0.19 0.46 0.72)
                                        #(0.01 0.06 0.22 0.46))))

  (define get-ratings
    (λ (tree player)
      (let* ((board (cadr tree))
             (dice (λ (pos) (cadr (vector-ref board pos)))))
        (stream:->list
          ((stream:map
             (λ (move)
               (let1 (path (car move))
                 (if path
                   (let* ((src (car path))
                          (dst (cadr path))
                          (odds (vector-ref (vector-ref (*dice-odds*) (sub1 (dice dst)))
                                            (- (dice src) 2))))
                     (+ (* odds (rate-position (cadr move) player))
                        (* (- 1 odds) (rate-position (caddr move) player))))
                   (rate-position (cadr move) player)))))
           (caddr tree))))))

  ; every tree of a move is cut, the two of an attack as the one of the passing move.
  (define limit-tree-depth
    (λ (tree depth)
      (list (car tree)
            (cadr tree)
            (if (zero? depth)
              stream:empty
              ((stream:map (λ (move)
                             (cons (car move)
                                   (map (λ (x) (limit-tree-depth x (sub1 depth)))
                                        (cdr move)))))
               (caddr tree))))))

  ; reinforcements by the largest cluster ----------------------------------------------------------

  ; the hexes of `player` connected to `pos`, the last one found first.
  (define get-connected
    (λ (board player pos)
      (letrec ((check-pos (λ (pos visited)
                            (if (and (eqv? (car (vector-ref board pos)) player)
                                     (not (memv pos visited)))
                              (check-neighbors (neighbors pos) (cons pos visited))
                              visited)))
               (check-neighbors (λ (lst visited)
                                  (if (pair? lst)
                                    (check-neighbors (cdr lst) (check-pos (car lst) visited))
                                    visited))))
        (check-pos pos '()))))

  (define largest-cluster-size
    (λ (board player)
      (let f ((pos 0) (visited '()) (best 0))
        (if (< pos (*board-hexnum*))
          (if (and (eqv? (car (vector-ref board pos)) player)
                   (not (memv pos visited)))
            (let* ((cluster (get-connected board player pos))
                   (size (length cluster)))
              (if (> size best)
                (f (add1 pos) (append cluster visited) size)
                (f (add1 pos) (append cluster visited) best)))
            (f (add1 pos) visited best))
          best))))

  ; `spare-dice` is ignored: the dice are as many as the hexes of the largest cluster.
  (define add-new-dice
    (λ (board player spare-dice)
      (letrec ((f (λ (lst n)
                    (cond
                      ((zero? n) lst)
                      ((null? lst) '())
                      (else (let ((cur-player (caar lst))
                                  (cur-dice (cadar lst)))
                              (if (and (eqv? cur-player player) (< cur-dice (*max-dice*)))
                                (cons (list cur-player (add1 cur-dice))
                                      (f (cdr lst) (sub1 n)))
                                (cons (car lst) (f (cdr lst) n)))))))))
        (board-array (f (vector->list board) (largest-cluster-size board player))))))

  ; the copies that late binding makes call v4's functions -----------------------------------------

  ; v1's body and memoization, with a table of its own.
  (define game-tree
    (λ (board player spare-dice first-move)
      (list player
            board
            (add-passing-move board
                              player
                              spare-dice
                              first-move
                              (attacking-moves board player spare-dice)))))

  (define game-tree/previous (make-hash-table test: equal? hash: game-tree/key-hash))

  (set! game-tree
    (let ((old-game-tree game-tree)
          (previous game-tree/previous))
      (λ rest
        (hash-table-ref previous rest (τ (let1 (v (apply old-game-tree rest))
                                            (hash-table-set! previous rest v)
                                            v))))))

  ; v2's bodies.
  (define add-passing-move
    (λ (board player spare-dice first-move moves)
      (if first-move
        moves
        (stream:cons (list #f
                           (game-tree (add-new-dice board player (sub1 spare-dice))
                                      (modulo (add1 player) (*num-players*))
                                      0
                                      #t))
                     moves))))

  (define play-vs-human
    (λ (tree)
      (print-info tree)
      (if (not (stream:null? (caddr tree)))
        (play-vs-human (handle-human tree))
        (announce-winner (cadr tree)))))

  (define play-vs-computer
    (λ (tree)
      (print-info tree)
      (cond
        ((stream:null? (caddr tree)) (announce-winner (cadr tree)))
        ((zero? (car tree)) (play-vs-computer (handle-human tree)))
        (else (play-vs-computer (handle-computer tree))))))

  (define rate-position
    (λ (tree player)
      (let1 (moves (caddr tree))
        (if (not (stream:null? moves))
          (apply (if (eqv? (car tree) player) max min)
                 (get-ratings tree player))
          (score-board (cadr tree) player)))))

  ; v3's bodies.
  (define dod-request-handler
    (λ (path header params)
      (if (equal? path "game.html")
        (begin (display "<!doctype html>")
               (tag center ()
                    (display "Welcome to DICE OF DOOM!")
                    (tag br ())
                    (let1 (chosen (assq 'chosen params))
                      (when (or (not (*cur-game-tree*)) (not chosen))
                        (set! chosen #f)
                        (web-initialize))
                      (cond
                        ((stream:null? (caddr (*cur-game-tree*)))
                         (web-announce-winner (cadr (*cur-game-tree*))))
                        ((zero? (car (*cur-game-tree*)))
                         (web-handle-human (and chosen (parse-chosen (cdr chosen)))))
                        (else (web-handle-computer))))
                    (tag br ())
                    (draw-dod-page (*cur-game-tree*) (*from-tile*))))
        (display "Sorry... I don't know that page."))))

  (define web-initialize
    (λ ()
      (*from-tile* #f)
      (*cur-game-tree* (game-tree (gen-board) 0 0 #t))))

  (define web-handle-computer
    (λ ()
      (*cur-game-tree* (handle-computer (*cur-game-tree*)))
      (display "The computer has moved. ")
      (tag script ()
           (display "window.setTimeout('window.location=\"game.html?chosen=NIL\"',5000)"))))

  )
