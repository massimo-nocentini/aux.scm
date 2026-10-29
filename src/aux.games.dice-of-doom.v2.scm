; (aux games dice-of-doom v2): the second version of Dice of Doom in "Land of Lisp" (Conrad Barski,
; No Starch Press 2010, chapter 18), a port of http://landoflisp.com/dice_of_doom_v2.lisp.
;
; dice_of_doom_v2.lisp carries no license notice; this file is derived from Land of Lisp and is
; distributed, like the rest of aux, under the MIT license.
;
; The module behaves as the Common Lisp image after `(load "dice_of_doom_v2.lisp")`, which loads
; dice_of_doom_v1.lisp and lazy.lisp first: the moves of a tree are a lazy list, so the tree is
; computed only as far as it is explored; the AI looks `*ai-level*` moves ahead
; (`limit-tree-depth`), rates the leaves with the heuristic `score-board` and prunes with α-β; the
; board is 5x5. The lazy lists of lazy.lisp are the streams of (aux stream sicp): `lazy-cons` is
; `stream:cons`, `lazy-nil` is `stream:empty`, `lazy-null` is `stream:null?`, `make-lazy` is
; `list->stream`, `take-all` is `stream:->list`, `lazy-mapcar` and `lazy-mapcan` are `stream:map`
; and `stream:append-map`, `lazy-nth` is `stream:ref`; they force the same cells, in the same
; order, and memoize as LoL's closures do.
;
; v1's functions that v2.lisp does not redefine are re-exported from (aux games dice-of-doom v1).
; In the CL image a v1 function that calls a redefined one calls the new definition (late
; binding): of those, only `game-tree` (it calls `add-passing-move` and `attacking-moves`) is
; still live, so this module carries a copy of its v1 body, memoized as in v1 with a table of its
; own, `game-tree/previous`.
;
; Differences from the original:
;   - `*ai-level*` is a parameter, and the two `defparameter`s of the board size set v1's
;     parameters when the module is loaded (to 4, then to 5, as upstream);
;   - the first, minimax `handle-computer` of the file is dead code in the image (the α-β one
;     replaces it) and is dropped; src/test/games.dice-of-doom.v2.scm rebuilds it to check that
;     both choose the same move;
;   - v1's memoization of `rate-position` is gone from the image, since v2's plain `defun`
;     replaces it, so `rate-position` is not memoized and `rate-position/previous` is not
;     re-exported;
;   - CL's `(position (apply #'max ratings) ratings)` compares with `=` (`list-index`), and
;     `(loop ... (return t))` in `threatened` is srfi-1's `any`.
;
; Kept as upstream: `limit-tree-depth` builds fresh trees on every turn of the computer, and the
; game ends as soon as the player to move has no attack at the start of a turn.

(module (aux games dice-of-doom v2)

  (game-tree game-tree/previous add-passing-move attacking-moves handle-human play-vs-human
    limit-tree-depth *ai-level* handle-computer play-vs-computer score-board threatened get-ratings
    rate-position ab-get-ratings-max ab-get-ratings-min ab-rate-position)

  (import scheme
          (chicken base)
          (chicken format)
          (chicken fixnum)
          (only (chicken module) reexport)
          (only (scheme base) make-parameter)
          (only srfi-1 iota any list-index)
          srfi-69
          (aux base)
          (aux stream sicp))

  ; v1, without the names that v2 redefines (defining an imported name would overwrite it for
  ; every importer of v1), and without the memo table of v1's `rate-position`.
  (import (except (aux games dice-of-doom v1)
                  game-tree game-tree/previous add-passing-move attacking-moves handle-human
                  play-vs-human handle-computer play-vs-computer get-ratings rate-position
                  rate-position/previous))

  (reexport (except (aux games dice-of-doom v1)
                    game-tree game-tree/previous add-passing-move attacking-moves handle-human
                    play-vs-human handle-computer play-vs-computer get-ratings rate-position
                    rate-position/previous))

  (*board-size* 4)
  (*board-hexnum* (* (*board-size*) (*board-size*)))

  ; v1's body, which now calls v2's `add-passing-move` and `attacking-moves`, and v1's memoization.
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

  ; lazy game trees --------------------------------------------------------------------------------

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
                             (> (dice src) (dice dst)))
                      (list->stream
                        (list (list (list src dst)
                                    (game-tree (board-attack board cur-player src dst (dice src))
                                               cur-player
                                               (+ spare-dice (dice dst))
                                               #f))))
                      stream:empty)))
                (list->stream (neighbors src)))
               stream:empty)))
         (list->stream (iota (*board-hexnum*)))))))

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
        (cadr ((stream:ref (sub1 (read))) moves)))))

  (define play-vs-human
    (λ (tree)
      (print-info tree)
      (if (not (stream:null? (caddr tree)))
        (play-vs-human (handle-human tree))
        (announce-winner (cadr tree)))))

  ; improvements for the AI ------------------------------------------------------------------------

  (define limit-tree-depth
    (λ (tree depth)
      (list (car tree)
            (cadr tree)
            (if (zero? depth)
              stream:empty
              ((stream:map (λ (move)
                             (list (car move)
                                   (limit-tree-depth (cadr move) (sub1 depth)))))
               (caddr tree))))))

  (define *ai-level* (make-parameter 4))

  (define play-vs-computer
    (λ (tree)
      (print-info tree)
      (cond
        ((stream:null? (caddr tree)) (announce-winner (cadr tree)))
        ((zero? (car tree)) (play-vs-computer (handle-human tree)))
        (else (play-vs-computer (handle-computer tree))))))

  (define score-board
    (λ (board player)
      (let S ((pos 0) (sum 0))
        (if (< pos (vector-length board))
          (S (add1 pos)
             (+ sum (if (eqv? (car (vector-ref board pos)) player)
                      (if (threatened pos board) 1 2)
                      -1)))
          sum))))

  (define threatened
    (λ (pos board)
      (let* ((hex (vector-ref board pos))
             (player (car hex))
             (dice (cadr hex)))
        (any (λ (n)
               (let* ((nhex (vector-ref board n))
                      (nplayer (car nhex))
                      (ndice (cadr nhex)))
                 (and (not (eqv? player nplayer)) (> ndice dice))))
             (neighbors pos)))))

  (define get-ratings
    (λ (tree player)
      (stream:->list ((stream:map (λ (move) (rate-position (cadr move) player)))
                      (caddr tree)))))

  (define rate-position
    (λ (tree player)
      (let1 (moves (caddr tree))
        (if (not (stream:null? moves))
          (apply (if (eqv? (car tree) player) max min)
                 (get-ratings tree player))
          (score-board (cadr tree) player)))))

  ; α-β pruning ------------------------------------------------------------------------------------

  (define ab-get-ratings-max
    (λ (tree player upper-limit lower-limit)
      (letrec ((f (λ (moves lower-limit)
                    (if (stream:null? moves)
                      '()
                      (let1 (x (ab-rate-position (cadr (stream:car moves)) player upper-limit lower-limit))
                        (if (>= x upper-limit)
                          (list x)
                          (cons x (f (stream:cdr moves) (max x lower-limit)))))))))
        (f (caddr tree) lower-limit))))

  (define ab-get-ratings-min
    (λ (tree player upper-limit lower-limit)
      (letrec ((f (λ (moves upper-limit)
                    (if (stream:null? moves)
                      '()
                      (let1 (x (ab-rate-position (cadr (stream:car moves)) player upper-limit lower-limit))
                        (if (<= x lower-limit)
                          (list x)
                          (cons x (f (stream:cdr moves) (min x upper-limit)))))))))
        (f (caddr tree) upper-limit))))

  (define ab-rate-position
    (λ (tree player upper-limit lower-limit)
      (let1 (moves (caddr tree))
        (if (not (stream:null? moves))
          (if (eqv? (car tree) player)
            (apply max (ab-get-ratings-max tree player upper-limit lower-limit))
            (apply min (ab-get-ratings-min tree player upper-limit lower-limit)))
          (score-board (cadr tree) player)))))

  (define handle-computer
    (λ (tree)
      (let* ((ratings (ab-get-ratings-max (limit-tree-depth tree (*ai-level*))
                                          (car tree)
                                          most-positive-fixnum
                                          most-negative-fixnum))
             (best (apply max ratings)))
        (cadr ((stream:ref (list-index (λ (r) (= r best)) ratings)) (caddr tree))))))

  (*board-size* 5)
  (*board-hexnum* (* (*board-size*) (*board-size*)))

  )
