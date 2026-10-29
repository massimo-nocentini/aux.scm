; (aux games dice-of-doom v3): the third version of Dice of Doom in "Land of Lisp" (Conrad Barski,
; No Starch Press 2010, chapter 19), a port of http://landoflisp.com/dice_of_doom_v3.lisp.
;
; dice_of_doom_v3.lisp carries no license notice; this file is derived from Land of Lisp and is
; distributed, like the rest of aux, under the MIT license.
;
; The module behaves as the Common Lisp image after `(load "dice_of_doom_v3.lisp")`: the game of
; v2, drawn as an SVG board, and played in the browser through the request handler
; `dod-request-handler`, which follows the contract of the web server of chapter 13:
; `(dod-request-handler path header params)` writes the page to the current output port, `path`
; is the URL without its leading slash and `params` an alist whose keys are lower-case symbols.
; Serve it with either web server of aux, for example
;
;   (import (aux land-of-lisp webserver)) (serve dod-request-handler)
;
; and open http://localhost:8080/game.html. v3.lisp redefines no function of v2, so all of v2 is
; re-exported; it copies `let1`, `split`, `pairs`, `print-tag`, `tag`, `brightness`, `svg-style`
; and `polygon` from svg.lisp, so they are re-exported from (aux land-of-lisp svg), except `svg`,
; which v3 redefines with a width and a height.
;
; Differences from the original:
;   - v3.lisp loads webserver.lisp, but this module neither imports nor re-exports
;     (aux land-of-lisp webserver): that port is GPL-2.0-only, and every program built on v3 or
;     v4 would then be a GPL-2.0-only work. Import a server next to v3 (see above);
;   - the drawing parameters (`*board-width*` ... `*dot-size*`, `*die-colors*`) are parameters;
;   - `*cur-game-tree*` and `*from-tile*` are cells, not parameters: they read and set as
;     parameters do, `(*from-tile*)` and `(*from-tile* 3)`, but CHICKEN parameters are local to a
;     thread and a server may serve each request in a thread of its own, so the game state lives
;     in a variable that every thread shares; they cannot be `parameterize`d;
;   - `parse-chosen` is new: it reads the `chosen` parameter as a number, the symbol `pass`
;     (in any case) or #f (anything else, `NIL` included); as with upstream's reader, the number
;     is any one that `string->number` reads, `2.0` and `1+2i` included, and one that is not a
;     tile index matches no move. It replaces upstream's
;     `(read-from-string (cdr chosen))`, which would run the CL reader, macros included, on input
;     from the network, and whose Scheme counterpart would answer the symbols `PASS` and `NIL`;
;   - `make-game-link` writes #f as `NIL` and a symbol upper-cased, the URLs that CL prints;
;   - where upstream takes `car` of nil, which is nil in CL, the port tests first: `draw-dod-page`
;     reads the source and destination of the passing move `(#f tree)` as #f, and
;     `web-handle-human` answers #f for a destination that matches no move;
;   - `dod-request-handler` finds `chosen` with `assq`, since keys are symbols.
;
; Kept as upstream: on a turn of the computer the page has no link, only a script that reloads
; it after 5 seconds, so it does not work without JavaScript; the source tile is not a link, so
; "Move cancelled." cannot be reached from the board; `chosen=PASS` at a node without a passing
; move takes the first attack; an illegal destination sets the tree to #f, the request fails in
; `draw-dod-page`, and the next request starts a new game; `(tag br ())` prints `<br></br>`;
; one global game is shared by every visitor.

(module (aux games dice-of-doom v3)

  (svg *board-width* *board-height* *board-scale* *top-offset* *dice-scale* *dot-size* draw-die-svg
    draw-tile-svg *die-colors* draw-board-svg make-game-link *cur-game-tree* *from-tile* parse-chosen
    dod-request-handler web-initialize web-announce-winner web-handle-human web-handle-computer
    draw-dod-page)

  (import scheme
          (chicken base)
          (chicken format)
          (only (chicken module) reexport)
          (only (scheme base) make-parameter)
          (only (scheme case-lambda) case-lambda)
          (only (scheme char) string-upcase string-ci=?)
          (aux base)
          (aux stream sicp)
          (aux games dice-of-doom v2)
          (except (aux land-of-lisp svg) svg))

  (reexport (aux games dice-of-doom v2)
            (except (aux land-of-lisp svg) svg))

  ; the `svg` of v3 takes a size; the pattern variables are not named `width` and `height`,
  ; since in `syntax-rules` they would replace the attribute names of the template too.
  (define-syntax svg
    (syntax-rules ()
      ((_ w h body ...) (tag svg (xmlns "http://www.w3.org/2000/svg"
                                  "xmlns:xlink" "http://www.w3.org/1999/xlink"
                                  height h
                                  width w)
                             body ...))))

  ; drawing the board ------------------------------------------------------------------------------

  (define *board-width* (make-parameter 900))
  (define *board-height* (make-parameter 500))
  (define *board-scale* (make-parameter 64))
  (define *top-offset* (make-parameter 3))
  (define *dice-scale* (make-parameter 40))
  (define *dot-size* (make-parameter 0.05))

  (define draw-die-svg
    (λ (x y col)
      (letrec ((calc-pt (λ (pt)
                          (cons (+ x (* (*dice-scale*) (car pt)))
                                (+ y (* (*dice-scale*) (cdr pt))))))
               (f (λ (pol col)
                    (polygon (map calc-pt pol) col))))
        (f '((0 . -1) (-0.6 . -0.75) (0 . -0.5) (0.6 . -0.75))
           (brightness col 40))
        (f '((0 . -0.5) (-0.6 . -0.75) (-0.6 . 0) (0 . 0.25))
           col)
        (f '((0 . -0.5) (0.6 . -0.75) (0.6 . 0) (0 . 0.25))
           (brightness col -40))
        (for-each (λ (x y)
                    (polygon (map (λ (xx yy)
                                    (calc-pt (cons (+ x (* xx (*dot-size*)))
                                                   (+ y (* yy (*dot-size*))))))
                                  '(-1 -1 1 1)
                                  '(-1 1 1 -1))
                             '(255 255 255)))
                  '(-0.05 0.125 0.3 -0.3 -0.125 0.05 0.2 0.2 0.45 0.45 -0.45 -0.2)
                  '(-0.875 -0.80 -0.725 -0.775 -0.70 -0.625
                    -0.35 -0.05 -0.45 -0.15 -0.45 -0.05)))))

  (define draw-tile-svg
    (λ (x y pos hex xx yy col chosen-tile)
      (do ((z 0 (add1 z)))
        ((= z 2))
        (polygon (map (λ (pt)
                        (cons (+ xx (* (*board-scale*) (car pt)))
                              (+ yy (* (*board-scale*)
                                       (+ (cdr pt) (* (- 1 z) 0.1))))))
                      '((-1 . -0.2) (0 . -0.5) (1 . -0.2)
                        (1 . 0.2) (0 . 0.5) (-1 . 0.2)))
                 (if (eqv? pos chosen-tile)
                   (brightness col 100)
                   col)))
      (do ((z 0 (add1 z)))
        ((= z (cadr hex)))
        (draw-die-svg (+ xx
                         (* (*dice-scale*)
                            0.3
                            (if (odd? (+ x y z))
                              -0.3
                              0.3)))
                      (- yy (* (*dice-scale*) z 0.8))
                      col))))

  (define *die-colors* (make-parameter '((255 63 63) (63 63 255))))

  (define draw-board-svg
    (λ (board chosen-tile legal-tiles)
      (do ((y 0 (add1 y)))
        ((= y (*board-size*)))
        (do ((x 0 (add1 x)))
          ((= x (*board-size*)))
          (let* ((pos (+ x (* (*board-size*) y)))
                 (hex (vector-ref board pos))
                 (xx (* (*board-scale*) (+ (* 2 x) (- (*board-size*) y))))
                 (yy (* (*board-scale*) (+ (* y 0.7) (*top-offset*))))
                 (col (brightness (list-ref (*die-colors*) (car hex))
                                  (* -15 (- (*board-size*) y)))))
            (if (memv pos legal-tiles)
              (tag g ()
                   (tag a ("xlink:href" (make-game-link pos))
                        (draw-tile-svg x y pos hex xx yy col chosen-tile)))
              (draw-tile-svg x y pos hex xx yy col chosen-tile)))))))

  ; the URL of a choice, as CL prints it: #f is NIL, a symbol is upper case.
  (define make-game-link
    (λ (pos)
      (format #f "/game.html?chosen=~a" (cond
                                          ((not pos) "NIL")
                                          ((symbol? pos) (string-upcase (symbol->string pos)))
                                          (else pos)))))

  ; the web game -----------------------------------------------------------------------------------

  ; a variable shared by every thread, read with `(c)` and set with `(c v)`, as a parameter.
  (define make-cell
    (λ (v)
      (case-lambda
        (() v)
        ((x) (set! v x)))))

  (define *cur-game-tree* (make-cell #f))
  (define *from-tile* (make-cell #f))

  (define parse-chosen
    (λ (s)
      (cond
        ((not (string? s)) #f)
        ((string->number s))
        ((string-ci=? s "pass") 'pass)
        (else #f))))

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

  (define web-announce-winner
    (λ (board)
      (fresh-line)
      (let1 (w (winners board))
        (if (> (length w) 1)
          (format #t "The game is a tie between ~a" (map player-letter w))
          (format #t "The winner is ~a" (player-letter (car w)))))
      (tag a (href "game.html")
           (display " play again"))))

  (define web-handle-human
    (λ (pos)
      (cond
        ((not pos) (display "Please choose a hex to move from:"))
        ((eq? pos 'pass) (*cur-game-tree* (cadr (stream:car (caddr (*cur-game-tree*)))))
                         (display "Your reinforcements have been placed.")
                         (tag a (href (make-game-link #f))
                              (display "continue")))
        ((not (*from-tile*)) (*from-tile* pos)
                             (display "Now choose a destination:"))
        ((eqv? pos (*from-tile*)) (*from-tile* #f)
                                  (display "Move cancelled."))
        (else (*cur-game-tree*
                (let1 (move ((stream:find (λ (move)
                                            (equal? (car move) (list (*from-tile*) pos))))
                             (caddr (*cur-game-tree*))))
                  (and move (cadr move))))
              (*from-tile* #f)
              (display "You may now ")
              (tag a (href (make-game-link 'pass))
                   (display "pass"))
              (display " or make another move:")))))

  (define web-handle-computer
    (λ ()
      (*cur-game-tree* (handle-computer (*cur-game-tree*)))
      (display "The computer has moved. ")
      (tag script ()
           (display "window.setTimeout('window.location=\"game.html?chosen=NIL\"',5000)"))))

  (define draw-dod-page
    (λ (tree selected-tile)
      (svg (*board-width*)
           (*board-height*)
           (draw-board-svg (cadr tree)
                           selected-tile
                           (stream:->list (if selected-tile
                                            ((stream:map (λ (move)
                                                           (and (car move)
                                                                (eqv? (caar move) selected-tile)
                                                                (cadar move))))
                                             (caddr tree))
                                            ((stream:map (λ (move) (and (car move) (caar move))))
                                             (caddr tree))))))))

  )
