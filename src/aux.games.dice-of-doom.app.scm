; (aux games dice-of-doom app): the web game of Dice of Doom version 4 in "Land of Lisp" (Conrad
; Barski, No Starch Press 2010, chapters 19 and 20), restyled and made deployable: an SVG board drawn
; on the server, a side panel with the players and the last moves, a tiny script for the turns of the
; computer, and one game per browser. It builds on (aux games dice-of-doom v4), whose engine it uses
; unchanged, on (aux search adversarial) for the AI and on (aux land-of-lisp webserver spiffy).
;
; The drawing and the web code derive from dice_of_doom_v3.lisp and dice_of_doom_v4.lisp, which carry
; no license notice; this file is distributed, like the rest of aux, under the MIT license.
;
; Serve it with `(dod-serve port: 8080)`, or `csi -s dice-of-doom-server.scm` in src, and open
; http://localhost:8080/game.html. The handler `dod-request-handler` has the contract of the server
; of chapter 13, so (aux land-of-lisp webserver) serves it too, one connection at a time.
;
; The module stands to v4 as v4 stands to v3: it imports the engine without the names it redefines
; (the drawing, `make-game-link`, the `web-` procedures, `dod-request-handler` and `roll-dice`), and
; sets two of v4's parameters when it is loaded, as a later `defparameter` would: `*die-colors*` to
; the Okabe-Ito colours of the four players and `*ai-level*` to 3. The drawing reads v3's
; `*board-scale*`, `*dice-scale*` and `*top-offset*`; the new drawing and serving parameters are
; parameters too.
;
; A game is a `web-game` record kept in a table under a random id, which travels in every link with
; a step token: `/game.html?g=ID&t=STEP&chosen=CHOICE`, where CHOICE is `nil`, `pass` or a hex. A
; request whose step is not the current one changes nothing and shows the game as it is, so reloads,
; Back, double clicks and a second tab are harmless. A new game starts at `/game.html` (typed, or a
; crawler) or at `/game.html?new=ID`, the link of New game and Play again, whose id is drawn when
; the page is made: that link always answers the same game, so Back onto it is harmless too, with
; or without JavaScript; with JavaScript a new game's page also rewrites its own address. Only Back
; onto a typed `/game.html` without JavaScript deals again, as the book did. `web-game-advance!`
; is the book's dispatch: the human's choice (`web-handle-human`), a move of the computer
; (`web-handle-computer`), or the end of the game. Every attack, the human's and the computer's,
; rolls the dice (`roll-dice` draws as v4's does, attacker first, without printing) and goes to the
; log with its rolls.
;
; At most `*max-games*` games are kept: when a game starts, the games idle for more than
; `*game-idle-seconds*` are dropped, then, while the table is full, the least recently used game
; nobody has moved in (step 0, as a bare request leaves it), and only when there is none the least
; recently used of all; a request on an idle game finds it gone. So a burst of anonymous requests
; does not push out the games being played.
;
; The AI (`choose-computer-move`) is the search of v4's `handle-computer`, made with Star1, the
; *-minimax extension of alpha-beta pruning to chance nodes (Ballard 1983), on the game
; `dice-of-doom/game` (search.md 3.10), with a fresh transposition table per move, by iterative
; deepening to `*ai-level*` under a budget of `*ai-budget*` nodes. Measured on 60 mid-game v4
; positions (random play from 60 seeded boards, a computer to move, 4 to 25 moves; compiled; one
; core of a Xeon Gold 6238R; each search on a fresh tree, so the times include building it), every
; configuration chose the move of expectiminimax, which is v4's `handle-computer`:
;
;   level 2, total ms / nodes:  v4 get-ratings 633 · expectiminimax 620 / 61374 · star1 638 / 56456
;            star2 642 / 57932 · star1+table 619 / 51568 · star2+table 620 / 53044
;            iterative star1+table 624 / 53838 · iterative star2+table 635 / 55314; slowest move 40 ms
;   level 3, total ms / nodes:  v4 get-ratings 10690 · expectiminimax 10111 / 1560990
;            star1 9829 / 1305835 · star2 10264 / 1377443 · star1+table 8013 / 746798
;            star2+table 8264 / 786421 · iterative star1+table 8062 / 800636
;            iterative star2+table 8358 / 841735 · the same with a budget of 100000 nodes 8133;
;            slowest move: v4 1022 ms, expectiminimax 865 ms, iterative star1+table 610 ms
;
; Building the lazy tree costs most of the time (each attack makes two trees, each with its passing
; move and its reinforcements), so pruning half of the nodes saves a fifth of the time. Star2 probes
; more than it prunes here, and the bounds of `score-board`, [−25, 50], are loose. Star1 with a
; table is the fastest at both levels, so it is the default, and level 3 is affordable: 135 ms per
; move on average, about 0.6 s at worst; the budget, about 1 s of search, bounds the rare larger
; position, and then the move is the one of the deepest search completed.
; src/test/search.adversarial.dice-of-doom.scm checks the choice against expectiminimax on 24 such
; positions at levels 2 and 3, and prints its own timings.
;
; One lock serialises the requests on games (see below), so the players wait for each other: with
; 4 clients at once a page waited up to 1.3 s, with 20 (16 playing, 4 sending garbage) the median
; was 0.8 s and the slowest 3 s. It is a toy server for a few players at a time.
;
; Differences from the web code of v3 and v4 (ui.md 12):
;   - the drawing answers SXML, serialised by sxml-transforms, instead of printing tags with the
;     `tag` and `svg` macros; the names `draw-die-svg`, `draw-tile-svg`, `draw-board-svg`,
;     `draw-dod-page`, `make-game-link`, `web-handle-human`, `web-handle-computer` and
;     `web-announce-winner` are kept, their arguments change;
;   - one global game (`*cur-game-tree*`, `*from-tile*`) becomes a table of games under ids that
;     travel in the URL, with a step token (see above);
;   - `chosen` is read by v3's `parse-chosen` and then checked: `pass`, or an exact hex index,
;     else `nil`; a source and a destination must be legal, otherwise nothing happens (upstream
;     took any number as a source and lost the game on an impossible destination);
;   - the human's attacks roll the dice (v4's web code takes the winning branch: in the browser
;     the human never lost); the rolls are kept as data and shown in the status and in the log,
;     not printed into the page by `roll-dice`;
;   - the source tile is a link, so a choice can be cancelled; End turn is shown disabled at the
;     first move of a turn, and a crafted `chosen=pass` there changes nothing (upstream took the
;     first attack);
;   - a new game is dealt again while the human has no attack at all (`deal-game-tree`), about one
;     board in 275, which upstream ended before its first move;
;   - the turns of the computer have a Continue link, which a script follows after
;     `*auto-advance-ms*` (1.6 s, upstream 5 s) with `location.replace`, unless Pause or Escape
;     stops it; the game works without JavaScript;
;   - the page is restyled: palette, glyphs and hex numbers on the tiles, tiles drawn at
;     `*tile-gap*` with thickness `*tile-thickness*`, round pips on the faces of the dice, shading,
;     odds of each target in a pill that is a link to it, the last attack as an arc between the
;     numbers of its hexes, the dice gained above their stacks, the other tiles muted while a
;     target is chosen, with their dice faded and click-through where they hide the chosen tile or
;     a target, a list of the moves under the board on phones, the players, the last moves, the
;     winner in a banner that says why the game ended; the root path is the game, and the pages
;     carry `noindex`;
;   - the AI searches with Star1 to depth 3 instead of expectiminimax to depth 2 (see above); the
;     move is the same as v4's at the same depth, unless the budget runs out;
;   - the engine memoizes `game-tree` and `neighbors` in tables shared by all games, which srfi-18
;     threads must not change at the same time, so every request on a game holds one lock, instead
;     of the lock per game of ui.md 9.2; the table of `game-tree` is emptied when a game starts and
;     after every move of the computer (APP quirk 13 of ports.md), so that the trees of finished
;     moves can be collected;
;   - colours: CHICKEN's `round` rounds half to even, as Python's, which made the prototype; the
;     grey of `mute` is rounded to a colour before it is mixed, as there.
; Differences from the specs (ui.md, search.md), besides the ones above:
;   - names that would clash with the engine and with (aux search adversarial) are prefixed: the
;     record is `web-game`, not `game`, and its procedures `web-game-new!`, `web-game-lookup` and
;     `web-game-advance!`; `choose-computer-move`, `board-changes`, `deal-game-tree`,
;     `draw-gain-pill` and `parse-choice` are new;
;   - `dice-of-doom/game` takes `hexnum:` and `odds:` as keywords (search.md 3.10 has them
;     positional), since the tests give the exact odds alone;
;   - `draw-board-svg` always draws the odds of the targets (ui.md's `odds-for` key is dropped) and
;     `draw-last-move` takes the board, to stand the arc on the stacks;
;   - `dod-serve` takes keywords only, `port:` (8080), `bind-address:` and `listener:`, as the
;     spiffy `serve` it calls with `serialize: #f`, since the handler does its own locking;
;   - the hex number is drawn in a darker `label` colour, 11 units high, and the rings have a dark
;     underlay, so that both reach 3:1 or more on every tile; focus thickens the ring and its
;     underlay instead of painting it in `--focus`;
;   - `svg-num` rounds half to even, so 0.105, a little less in binary, prints as 0.1, as the
;     prototype; the links of ui.md 13 test 12 come in painter's order, 6, 11, 12, 17;
;   - params are looked up with `assq`: both servers give lower-case symbols.
; Kept on purpose: the first move of a turn cannot be a pass; the game ends as soon as the player to
; move has no move at all, which may be a player with no hex left (the banner says so); the human
; is always player a.

(module (aux games dice-of-doom app)

  (*tile-gap* *tile-thickness* *board-viewbox* *player-glyphs* *auto-advance-ms* *max-games*
    *game-idle-seconds* *ai-budget* mix colour->hex row-shade mute tile-colours svg-num hex-centre
    hex-points points->string die-position stack-top attack-odds die-defs draw-die-svg draw-tile-svg
    draw-odds-pill draw-gain-pill draw-last-move draw-board-svg dice-of-doom/move->child
    dice-of-doom/tree-hash dice-of-doom/game choose-computer-move roll-dice board-stats legal-tiles
    board-changes deal-game-tree web-game? web-game-id web-game-tree web-game-from-tile web-game-step
    web-game-message web-game-log web-game-prev-board web-game-last-move web-games web-game-new!
    web-game-lookup web-game-advance! make-game-link make-new-game-link parse-choice
    web-handle-human web-handle-computer
    web-announce-winner draw-dod-page dod-css dod-js dod-rules dod-request-handler dod-serve)

  (import scheme
          (chicken base)
          (chicken string)
          (chicken fixnum)
          (chicken random)
          (chicken bytevector)
          (chicken time)
          (only (scheme base) make-parameter exact-integer?)
          (only (scheme char) string-upcase)
          (only srfi-1 filter count iota delete-duplicates take last every any)
          srfi-18
          srfi-69
          sxml-transforms
          (aux base)
          (aux stream sicp)
          (only (aux search adversarial) make-game make-chance-node star1 best-move/iterative-deepening
                decision-index make-transposition-table)
          (only (aux land-of-lisp webserver spiffy) serve)
          (except (aux games dice-of-doom v4)
                  draw-die-svg draw-tile-svg draw-board-svg draw-dod-page make-game-link
                  web-handle-human web-handle-computer web-announce-winner dod-request-handler
                  roll-dice))

  ; the colours of the players, Okabe-Ito (the last one darkened to reach 3:1 on the light page),
  ; in the book's order: red, blue, green, magenta; and a deeper search than v4's (see the header).
  (*die-colors* '((213 94 0) (0 114 178) (0 158 115) (184 87 143)))
  (*ai-level* 3)

  (define *tile-gap* (make-parameter 0.94))            ; tiles drawn at 94%, so a gutter shows
  (define *tile-thickness* (make-parameter 0.12))      ; the book's is 0.1
  (define *board-viewbox* (make-parameter '(-16 -4 928 444)))
  (define *player-glyphs* (make-parameter '(circle triangle square diamond)))
  (define *auto-advance-ms* (make-parameter 1600))
  (define *max-games* (make-parameter 256))
  (define *game-idle-seconds* (make-parameter 7200))
  (define *ai-budget* (make-parameter 100000))         ; nodes, about 1 s of search

  ; colours ----------------------------------------------------------------------------------------

  ; colours are lists of three exact integers 0-255, as the book's `*die-colors*`.

  (define white '(255 255 255))
  (define black '(0 0 0))

  ; the colour a fraction t of the way from c to k.
  (define (mix c k t)
    (map (λ (x y)
           (let1 (v (inexact->exact (round (+ x (* (- y x) t)))))
             (cond
               ((< v 0) 0)
               ((> v 255) 255)
               (else v))))
         c
         k))

  (define (colour->hex c)
    (apply string-append "#" (map (λ (v) (string-append (if (< v 16) "0" "") (string-upcase (number->string v 16))))
                                  c)))

  ; the back rows darker, 3.5% a row; the book's `brightness` went from -75 to -15.
  (define (row-shade c y) (mix c black (* 0.035 (- (*board-size*) 1 y))))

  ; keeps the lightness and drops most of the hue.
  (define (mute c)
    (let* ((l (+ (* 0.299 (car c)) (* 0.587 (cadr c)) (* 0.114 (caddr c))))
           (grey (let1 (v (inexact->exact (round l))) (list v v v))))
      (mix (mix c grey 0.72) white 0.08)))

  ; the colours of a tile of `player` on row `y`, keyed by their role (ui.md 3.2).
  (define (tile-colours player y muted?)
    (let* ((b (row-shade (list-ref (*die-colors*) player) y))
           (b (if muted? (mute b) b)))
      `((die . ,b)
        (edge . ,(mix b black 0.45))
        (top . ,(mix b white 0.34))
        (side . ,(mix b black 0.30))
        (stroke . ,(mix b black 0.50))
        (mark . ,(mix b black 0.38))
        (label . ,(mix b black 0.62)))))       ; the hex number: 4.9:1 or more on every top

  ; geometry ---------------------------------------------------------------------------------------

  ; x rounded to two decimals, without trailing zeros: CHICKEN prints the flonum 192 as `192.`,
  ; which must not reach the SVG. Halves round to even, so 0.105, a little less in binary, is 0.1.
  (define (svg-num x)
    (let* ((n (inexact->exact (round (* 100 x))))
           (a (abs n))
           (i (quotient a 100))
           (f (remainder a 100)))
      (string-append (if (negative? n) "-" "")
                     (number->string i)
                     (cond
                       ((zero? f) "")
                       ((zero? (remainder f 10)) (string-append "." (number->string (quotient f 10))))
                       (else (string-append (if (< f 10) ".0" ".") (number->string f)))))))

  ; the book's coordinates of the centre of a hex.
  (define (hex-centre pos)
    (let* ((n (*board-size*))
           (x (modulo pos n))
           (y (quotient pos n)))
      (values (* (*board-scale*) (+ (* 2 x) (- n y)))
              (* (*board-scale*) (+ (* y 0.7) (*top-offset*))))))

  ; the book's unit hexagon, scaled by k and lowered by dy.
  (define (hex-points xx yy dy k)
    (map (λ (pt)
           (cons (+ xx (* (*board-scale*) k (car pt)))
                 (+ yy (* (*board-scale*) (+ (* k (cdr pt)) dy)))))
         '((-1 . -0.2) (0 . -0.5) (1 . -0.2) (1 . 0.2) (0 . 0.5) (-1 . 0.2))))

  (define (points->string pts)
    (string-intersperse (map (λ (pt) (string-append (svg-num (car pt)) "," (svg-num (cdr pt)))) pts) " "))

  ; the book's place of die z of the stack on tile (x, y), jitter included.
  (define (die-position x y z xx yy)
    (values (+ xx (* (*dice-scale*) 0.3 (if (odd? (+ x y z)) -0.3 0.3)))
            (- yy (* (*dice-scale*) z 0.8))))

  ; v4's odds that an attack from `src` takes `dst` on `board`.
  (define (attack-odds board src dst #!optional (odds (*dice-odds*)))
    (let1 (dice (λ (pos) (cadr (vector-ref board pos))))
      (vector-ref (vector-ref odds (sub1 (dice dst))) (- (dice src) 2))))

  ; drawing ----------------------------------------------------------------------------------------

  (define font "system-ui, -apple-system, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, 'Noto Sans', sans-serif")

  (define (scaled pts) (map (λ (pt) (cons (* (*dice-scale*) (car pt)) (* (*dice-scale*) (cdr pt)))) pts))

  ; the faces of the book's die, and the frames that map the unit square onto them, in die units.
  (define die-top '((0 . -1) (-0.6 . -0.75) (0 . -0.5) (0.6 . -0.75)))
  (define die-left '((0 . -0.5) (-0.6 . -0.75) (-0.6 . 0) (0 . 0.25)))
  (define die-right '((0 . -0.5) (0.6 . -0.75) (0.6 . 0) (0 . 0.25)))

  ; (face origin s-axis t-axis opacity pips): six on top, two on the left, four on the right.
  (define die-faces
    '((top (-0.6 . -0.75) (0.6 . -0.25) (0.6 . 0.25) ".95" ((.25 . .22) (.25 . .5) (.25 . .78) (.75 . .22) (.75 . .5) (.75 . .78)))
      (left (-0.6 . -0.75) (0.6 . 0.25) (0 . 0.75) ".90" ((.27 . .27) (.73 . .73)))
      (right (0 . -0.5) (0.6 . -0.25) (0 . 0.75) ".80" ((.27 . .27) (.73 . .27) (.27 . .73) (.73 . .73)))))

  (define (die-defs)
    (let ((face (λ (pts . paint) `(polygon (@ (points ,(points->string (scaled pts))) ,@paint))))
          (d (*dice-scale*)))
      `(defs
         (filter (@ (id "soft") (x "-10%") (y "-20%") (width "120%") (height "150%"))
                 (feGaussianBlur (@ (stdDeviation "7"))))
         (filter (@ (id "glow") (x "-30%") (y "-60%") (width "160%") (height "220%"))
                 (feGaussianBlur (@ (stdDeviation "5"))))
         (marker (@ (id "arrow") (viewBox "0 0 10 10") (refX "7") (refY "5") (markerWidth "5") (markerHeight "5")
                    (orient "auto-start-reverse"))
                 (path (@ (d "M1,1 L9,5 L1,9") (fill "none") (stroke "#FFFFFF") (stroke-width "2")
                          (stroke-linecap "round") (stroke-linejoin "round"))))
         ; faces with no paint of their own: each `use` gives the fill and the stroke
         (g (@ (id "die") (stroke-linejoin "round") (stroke-width "1.1"))
            ,(face die-left)
            ,(face die-right)
            ,(face die-top)
            ,(face die-top '(fill "#FFFFFF") '(fill-opacity ".30") '(stroke "none"))   ; light from the upper left
            ,(face die-right '(fill "#000000") '(fill-opacity ".22") '(stroke "none"))
            ,(face die-left '(fill "#FFFFFF") '(fill-opacity ".06") '(stroke "none"))
            ,@(map (λ (f)
                     (match1/first ((_ ,o ,s ,t ,opacity ,pips) f)
                       `(g (@ (transform ,(string-append "matrix(" (string-intersperse
                                                                    (map (λ (v) (svg-num (* d v)))
                                                                         (list (car s) (cdr s) (car t) (cdr t) (car o) (cdr o)))
                                                                    " ")
                                                         ")"))
                              (fill "#FFFFFF") (fill-opacity ,opacity) (stroke "none"))
                           ,@(map (λ (pip) `(circle (@ (cx ,(svg-num (car pip))) (cy ,(svg-num (cdr pip))) (r "0.1"))))
                                  pips))))
                   die-faces)))))

  (define (colour-of key colours) (colour->hex (cdr (assq key colours))))

  (define (draw-die-svg x y colours #!key new)
    `(use (@ (href "#die") (x ,(svg-num x)) (y ,(svg-num y))
             (fill ,(colour-of 'die colours)) (stroke ,(colour-of 'edge colours))
             ,@(if new '((class "new")) '()))))

  ; the glyph of `player` centred at (x, y), on a tile front.
  (define (tile-glyph player x y fill)
    (let ((r 4.2)
          (polygon (λ (pts) `(polygon (@ (points ,(points->string pts)) (fill ,fill))))))
      (case (list-ref (*player-glyphs*) player)
        ((circle) `(circle (@ (cx ,(svg-num x)) (cy ,(svg-num y)) (r ,(svg-num r)) (fill ,fill))))
        ((triangle) (polygon (list (cons x (- y (* r 1.15))) (cons (+ x (* r 1.2)) (+ y (* r 0.85)))
                                   (cons (- x (* r 1.2)) (+ y (* r 0.85))))))
        ((square) `(rect (@ (x ,(svg-num (- x (* r 0.9)))) (y ,(svg-num (- y (* r 0.9))))
                            (width ,(svg-num (* r 1.8))) (height ,(svg-num (* r 1.8))) (fill ,fill))))
        (else (polygon (list (cons x (- y (* r 1.3))) (cons (+ x (* r 1.3)) y)
                             (cons x (+ y (* r 1.3))) (cons (- x (* r 1.3)) y)))))))

  (define (dice-word n) (if (= n 1) "1 die" (conc n " dice")))

  (define (letter player) (string (player-letter player)))

  (define (hex-title pos hex) (conc "Hex " pos ": player " (letter (car hex)) ", " (dice-word (cadr hex))))

  ; one tile, a link when `href` is given; `state` is idle, source, target or chosen, `changed` #f,
  ; captured, spent or reinforced, and the last `new-dice` dice of the stack have just arrived;
  ; `faded` dice are seen through and let the pointer through, to the tile they hide.
  (define (draw-tile-svg x y pos hex xx yy state #!key href label changed muted faded (new-dice 0))
    (let* ((player (car hex))
           (dice (cadr hex))
           (colours (tile-colours player y muted))
           (colour (λ (key) (colour-of key colours)))
           (chosen? (eq? state 'chosen))
           (lift (if chosen? -6 0))
           (title (or label (hex-title pos hex)))
           (outline (λ (fill) `((fill ,fill) (stroke ,(colour 'stroke)) (stroke-width "1") (stroke-linejoin "round"))))
           ; a white ring on a dark underlay, which keeps it at 3:1 or more on every top
           (ring (λ (k width . more)
                   (let1 (pts (points->string (hex-points xx yy 0 k)))
                     `(g (@ (class "ring") (fill "none") (stroke-linejoin "round"))
                         (polygon (@ (class "ring-under") (points ,pts) (stroke "#1F2228") (stroke-opacity ".75")
                                     (stroke-width ,(svg-num (+ (string->number width) 1.6)))))
                         (polygon (@ (class "ring-line") (points ,pts) (stroke "#FFFFFF") ,@more
                                     (stroke-width ,width))))))))
      `(,(if href 'a 'g)
        (@ (class ,(conc "hex p" player " " state (if changed (conc " " changed) "") (if muted " muted" "")))
           ,@(if href `((href ,href) (aria-label ,title)) '()))
        (title ,title)
        ,@(if chosen?
            `((polygon (@ (class "glow") (points ,(points->string (hex-points xx (+ yy lift) 0.06 1.16)))
                          (fill "#FFF1B8") (fill-opacity "1") (filter "url(#glow)"))))
            '())
        (g (@ (class "lift") (transform ,(conc "translate(0 " lift ")")))
           (polygon (@ (class "side") (points ,(points->string (hex-points xx yy (*tile-thickness*) (*tile-gap*))))
                       ,@(outline (colour 'side))))
           (polygon (@ (class "top") (points ,(points->string (hex-points xx yy 0 (*tile-gap*))))
                       ,@(outline (if chosen? (colour->hex (mix (cdr (assq 'top colours)) white 0.35)) (colour 'top)))))
           ,@(case state
               ((chosen) (list (ring 0.80 "2.2")))
               ((source) (list (ring 0.76 "1.6" '(stroke-opacity ".9"))))
               ((target) (list (ring 0.76 "1.6" '(stroke-opacity ".9") '(stroke-dasharray "5 4"))))
               (else '()))
           ,(tile-glyph player (- xx 7) (+ yy 20) (colour 'mark))
           (text (@ (x ,(svg-num (+ xx 1))) (y ,(svg-num (+ yy 24))) (font-family ,font) (font-size "11")
                    (font-weight "650") (fill ,(colour 'label)))
                 ,(number->string pos))
           (g (@ (class "dice") ,@(if faded '((opacity ".3") (pointer-events "none")) '()))
              (ellipse (@ (cx ,(svg-num xx)) (cy ,(svg-num (+ yy 0.5))) (rx ,(svg-num (* (*dice-scale*) 0.62)))
                          (ry ,(svg-num (* (*dice-scale*) 0.26))) (fill "#000000") (fill-opacity ".22")))
              ,@(map (λ (z)
                       (receive (dx dy) (die-position x y z xx yy)
                         (draw-die-svg dx dy colours new: (>= z (- dice new-dice)))))
                     (iota dice)))))))

  ; the top vertex of the highest die of a stack of `dice` on `pos`, jitter aside: (values x y).
  (define (stack-top pos dice)
    (receive (xx yy) (hex-centre pos)
      (values xx (- yy (* (*dice-scale*) 0.8 (- dice 1)) (*dice-scale*)))))

  ; a pill of `width` with `text`, pointing down at the top of a stack of `dice` on `pos`.
  (define (pill pos dice text cls width paint ink)
    (receive (xx top) (stack-top pos dice)
      (let* ((cy (- top 17))
             (w (/ width 2))
             (rect (λ more `(rect (@ (x ,(svg-num (- xx w))) (y ,(svg-num (- cy 10.5))) (width ,(svg-num width))
                                     (height "21") (rx "10.5") ,@more)))))
        `(g (@ (class ,cls))
            ,(rect '(fill "#000000") '(fill-opacity ".18") '(transform "translate(0 1.5)"))
            ,(rect `(fill ,paint))
            (polygon (@ (points ,(points->string (list (cons (- xx 4) (+ cy 10)) (cons (+ xx 4) (+ cy 10))
                                                       (cons xx (+ cy 14.5)))))
                        (fill ,paint)))
            (text (@ (x ,(svg-num xx)) (y ,(svg-num (+ cy 4.6))) (text-anchor "middle") (font-family ,font)
                     (font-size "13") (font-weight "650") (fill ,ink))
                  ,text)))))

  ; the odds of a target with `dice` dice, in a pill above its stack.
  (define (draw-odds-pill pos dice odds)
    (pill pos dice (conc (inexact->exact (round (* 100 odds))) "%") "odds" 44 "#FFFFFF" "#1F2228"))

  ; the `n` dice just gained by the stack of `dice` on `pos`, in a dark pill above it, which stands
  ; out on the dice of every player.
  (define (draw-gain-pill pos dice n)
    (pill pos dice (conc "+" n) "gain" 34 "#1F2228" "#FFFFFF"))

  ; the last attack on `board`, the board after it: an arc between two badges with the numbers of
  ; `src` and `dst`, each above its stack, so that no other stack hides or claims them; the arc is
  ; dashed when the attack failed, and bends up, no higher than the top of the board.
  (define (draw-last-move board src dst captured?)
    (let* ((r 9)
           (badge (λ (pos) (receive (x top) (stack-top pos (cadr (vector-ref board pos))) (cons x (- top 14)))))
           (p0 (badge src))
           (p2 (badge dst))
           (low (min (cdr p0) (cdr p2)))
           (p1 (cons (/ (+ (car p0) (car p2)) 2) (max (- low 40) (- low))))
           ; `p` moved by `d` toward `q`, to start and end the arc at the rims of the badges
           (toward (λ (p q d)
                     (let* ((dx (- (car q) (car p)))
                            (dy (- (cdr q) (cdr p)))
                            (l (sqrt (+ (* dx dx) (* dy dy)))))
                       (cons (+ (car p) (* d (/ dx l))) (+ (cdr p) (* d (/ dy l)))))))
           (a (toward p0 p1 (+ r 2)))
           (b (toward p2 p1 (+ r 4)))       ; the arrowhead overshoots its end by 2
           (point (λ (p) (conc (svg-num (car p)) "," (svg-num (cdr p)))))
           (d (conc "M" (point a) " Q" (point p1) " " (point b)))
           (draw-badge (λ (pos p)
                         `(g (@ (class "badge"))
                             (circle (@ (cx ,(svg-num (car p))) (cy ,(svg-num (cdr p))) (r ,(svg-num r)) (fill "#FFFFFF")
                                        (stroke "#000000") (stroke-opacity ".28") (stroke-width "1.5")))
                             (text (@ (x ,(svg-num (car p))) (y ,(svg-num (+ (cdr p) 3.9))) (text-anchor "middle")
                                      (font-family ,font) (font-size "11") (font-weight "650") (fill "#1F2228"))
                                   ,(number->string pos))))))
      `(g (@ (class "lastmove") (fill "none") (stroke-linecap "round"))
          (path (@ (d ,d) (stroke "#000000") (stroke-opacity ".28") (stroke-width "5")))
          (path (@ (d ,d) (stroke "#FFFFFF") (stroke-width "2.2")
                   ,@(if captured? '() '((stroke-dasharray "6 5")))
                   (marker-end "url(#arrow)")))
          ,(draw-badge src p0)
          ,(draw-badge dst p2))))

  ; whether a stack of `dice` on `pos` hides part of the top of a tile in `tiles`: one of the rows
  ; behind, at most one column off, whose top face reaches below the top of the stack.
  (define (hides? pos dice tiles)
    (receive (xx top) (stack-top pos dice)
      (receive (_ yy) (hex-centre pos)
        (any (λ (t)
               (receive (tx ty) (hex-centre t)
                 (and (< ty yy) (<= (abs (- tx xx)) (*board-scale*)) (< top (+ ty (* (*board-scale*) 0.5))))))
             tiles))))

  ; the board, as the book's `draw-board-svg`: `legal-tiles` are the sources when no tile is
  ; chosen, the targets otherwise, and they are links made by `make-link` (none if it is #f);
  ; `changes` maps hexes to captured, spent or reinforced, `new-dice` to the dice just added.
  (define (draw-board-svg board chosen-tile legal-tiles #!key make-link last-move (changes '()) (new-dice '())
                          standalone)
    (let* ((n (*board-size*))
           (vb (*board-viewbox*))
           (viewbox (string-intersperse (map number->string vb) " "))
           (dice (λ (pos) (cadr (vector-ref board pos)))))
      `(svg (@ ,@(if standalone
                   `((xmlns "http://www.w3.org/2000/svg") (xmlns:xlink "http://www.w3.org/1999/xlink")
                     (viewBox ,viewbox) (width ,(number->string (caddr vb))) (height ,(number->string (cadddr vb))))
                   `((class "board") (viewBox ,viewbox) (role "group") (aria-labelledby "board-title"))))
            ,@(if standalone '() `((title (@ (id "board-title")) ,(conc "Board, " n " by " n " hexes"))))
            ,(die-defs)
            ; the footprints of every tile, lowered and blurred once
            (g (@ (class "ground") (fill "#000000") (fill-opacity ".16") (filter "url(#soft)"))
               ,@(map (λ (pos)
                        (receive (xx yy) (hex-centre pos)
                          `(polygon (@ (points ,(points->string (hex-points xx (+ yy 12) (*tile-thickness*) 1.0)))))))
                      (iota (vector-length board))))
            ; painter's order: the back row first, left to right
            ,@(map (λ (pos)
                     (receive (xx yy) (hex-centre pos)
                       (let* ((hex (vector-ref board pos))
                              (legal? (memv pos legal-tiles))
                              (state (cond
                                       ((eqv? pos chosen-tile) 'chosen)
                                       ((not legal?) 'idle)
                                       (chosen-tile 'target)
                                       (else 'source)))
                              (label (case state
                                       ((chosen) (conc "Hex " pos " selected, choose it again to cancel"))
                                       ((source) (conc "Attack from hex " pos " (" (dice-word (dice pos)) ")"))
                                       ((target) (conc "Attack hex " pos " (player " (letter (car hex)) ", "
                                                       (dice-word (dice pos)) ") from hex " chosen-tile ": "
                                                       (inexact->exact (round (* 100 (attack-odds board chosen-tile pos))))
                                                       "% to capture"))
                                       (else #f))))
                         (draw-tile-svg (modulo pos n) (quotient pos n) pos hex xx yy state
                                        href: (and make-link (not (eq? state 'idle)) (make-link pos))
                                        label: label
                                        changed: (alist-ref pos changes)
                                        muted: (and chosen-tile (eq? state 'idle))
                                        faded: (and chosen-tile (eq? state 'idle)
                                                    (hides? pos (dice pos) (cons chosen-tile legal-tiles)))
                                        new-dice: (or (alist-ref pos new-dice) 0)))))
                   (iota (vector-length board)))
            ; above every stack: the last attack and the dice gained, or the odds of the targets, each
            ; pill a link to its target, since it covers other tiles
            (g (@ (class "overlay") (aria-hidden "true"))
               ,@(if chosen-tile
                   (map (λ (pos)
                          (let1 (pill (draw-odds-pill pos (dice pos) (attack-odds board chosen-tile pos)))
                            (if make-link `(a (@ (href ,(make-link pos)) (tabindex "-1")) ,pill) pill)))
                        legal-tiles)
                   `(,@(map (λ (p) (draw-gain-pill (car p) (dice (car p)) (cdr p))) new-dice)
                     ,@(if last-move (list (apply draw-last-move board last-move)) '())))))))

  ; the AI ------------------------------------------------------------------------------------------

  ; A tree as a game of (aux search adversarial) (search.md 3.10): an attack is a chance node whose
  ; outcomes are the success and the failure, weighed by the odds of the *parent's* board; the
  ; passing move is its tree. A move with one tree, an attack of v2 for example, is no chance.
  (define (dice-of-doom/move->child board move #!optional (odds (*dice-odds*)))
    (if (and (car move) (pair? (cddr move)))
      (let1 (p (attack-odds board (caar move) (cadar move) odds))
        (make-chance-node (list (cons p (cadr move)) (cons (- 1 p) (caddr move)))))
      (cadr move)))

  ; a content hash of the visible position: srfi-69's hash of `eq?` looks into the pairs of a tree.
  ; It stays below 2^24, so that 31 times it is a fixnum even on a 32-bit CHICKEN.
  (define (dice-of-doom/tree-hash tree #!optional (bound most-positive-fixnum))
    (let ((board (cadr tree))
          (mix (λ (h x) (fxand (fx+ (fx* h 31) x) #xFFFFFF))))
      (let loop ((i 0) (h (car tree)))
        (if (fx= i (vector-length board))
          (fxmod h bound)
          (let1 (hex (vector-ref board i))
            (loop (fx+ i 1) (mix (mix h (car hex)) (cadr hex))))))))

  ; Trees are rated for `player` by v2's `score-board`, which lies in [−hexnum, 2·hexnum]; equal
  ; positions are the same tree, because `game-tree` is memoized, so the table compares with `eq?`.
  (define (dice-of-doom/game player #!key (hexnum (*board-hexnum*)) (odds (*dice-odds*)))
    (make-game children: (λ (tree) ((stream:map (λ (move) (dice-of-doom/move->child (cadr tree) move odds)))
                                    (caddr tree)))
               evaluate: (λ (tree) (score-board (cadr tree) player))
               maximizing?: (λ (tree) (eqv? (car tree) player))
               bounds: (cons (- hexnum) (* 2 hexnum))
               key: identity test: eq? hash: dice-of-doom/tree-hash))

  ; v4's `handle-computer` up to the choice: the move, before its dice are rolled.
  (define (choose-computer-move tree #!key (level (*ai-level*)) (budget (*ai-budget*)))
    (let* ((game (dice-of-doom/game (car tree)))
           (d (best-move/iterative-deepening star1 game tree max-depth: level budget: budget
                                             table: (make-transposition-table game))))
      ((stream:ref (decision-index d)) (caddr tree))))

  ; the rules on the page ---------------------------------------------------------------------------

  ; the sum of `n` dice, drawn in order as v4's `roll-dice`, which also prints it.
  (define (roll-dice n)
    (let R ((n n) (sum 0))
      (if (zero? n)
        sum
        (R (sub1 n) (+ sum (add1 (pseudo-random-integer 6)))))))

  ; for every player, (player hexes dice gain), gain being the size of the largest cluster.
  (define (board-stats board)
    (map (λ (player)
           (let1 (mine (filter (λ (hex) (eqv? (car hex) player)) (vector->list board)))
             (list player (length mine) (apply + (map cadr mine)) (largest-cluster-size board player))))
         (iota (*num-players*))))

  ; the book's `draw-dod-page` computation: the sources of the attacks, or the targets from `from`,
  ; without the passing move and without repetitions.
  (define (legal-tiles tree from)
    (delete-duplicates
      (filter identity
              (stream:->list ((stream:map (λ (move)
                                            (let1 (path (car move))
                                              (cond
                                                ((not path) #f)
                                                ((not from) (car path))
                                                ((eqv? (car path) from) (cadr path))
                                                (else #f)))))
                              (caddr tree))))
      eqv?))

  ; what the last action did to each hex: (values changes new-dice), the alists of `draw-board-svg`.
  (define (board-changes before after)
    (if (not before)
      (values '() '())
      (let loop ((pos (sub1 (vector-length after))) (changes '()) (new '()))
        (if (negative? pos)
          (values changes new)
          (let ((b (vector-ref before pos))
                (a (vector-ref after pos)))
            (cond
              ((not (eqv? (car a) (car b))) (loop (sub1 pos) (cons (cons pos 'captured) changes) new))
              ((and (= (cadr a) 1) (> (cadr b) 1)) (loop (sub1 pos) (cons (cons pos 'spent) changes) new))
              ((> (cadr a) (cadr b)) (loop (sub1 pos) (cons (cons pos 'reinforced) changes)
                                           (cons (cons pos (- (cadr a) (cadr b))) new)))
              (else (loop (sub1 pos) changes new))))))))

  ; games -------------------------------------------------------------------------------------------

  ; `message` is the SXML of the last event, `log` the newest entries first, each
  ; (attack player src dst src-dice dst-dice src-roll dst-roll captured?) or (pass player gained);
  ; `prev-board` is the board before the last action, `last-move` (src dst captured?) or #f;
  ; `touched` the seconds and `order` the rank of the last request, for the eviction.
  (define-record web-game id tree from-tile step message log prev-board last-move touched order)

  (define web-games (make-hash-table string=? string-hash))
  (define web-games-lock (make-mutex 'dice-of-doom))
  (define requests 0)

  (define (touch! g)
    (set! requests (add1 requests))
    (web-game-touched-set! g (current-seconds))
    (web-game-order-set! g requests)
    g)

  (define (fresh-id)
    (let* ((bytes (random-bytes (make-bytevector 8)))
           (id (apply string-append (map (λ (i) (let1 (b (bytevector-u8-ref bytes i))
                                                  (string-append (if (< b 16) "0" "") (number->string b 16))))
                                         (iota 8)))))
      (if (hash-table-exists? web-games id) (fresh-id) id)))

  ; an id as `fresh-id` makes them
  (define (game-id? s)
    (and (string? s)
         (= (string-length s) 16)
         (every (λ (c) (or (char-numeric? c) (and (char<=? #\a c) (char<=? c #\f)))) (string->list s))))

  (define (idle? g now) (> (- now (web-game-touched g)) (*game-idle-seconds*)))

  (define (least-recent games)
    (foldl (λ (a g) (if (or (not a) (< (web-game-order g) (web-game-order a))) g a)) #f games))

  ; the idle games, then, while the table is full, the least recently used game that nobody moved
  ; in, or else the least recently used one: the games being played go last.
  (define (evict!)
    (let1 (now (current-seconds))
      (for-each (λ (g) (when (idle? g now) (hash-table-delete! web-games (web-game-id g))))
                (hash-table-values web-games)))
    (let loop ()
      (when (and (positive? (hash-table-size web-games)) (>= (hash-table-size web-games) (*max-games*)))
        (let* ((games (hash-table-values web-games))
               (unplayed (filter (λ (g) (zero? (web-game-step g))) games)))
          (hash-table-delete! web-games (web-game-id (least-recent (if (pair? unplayed) unplayed games))))
          (loop)))))

  ; the first tree of a new game, dealt again while the human has no attack at all, which would end
  ; the game before its first move.
  (define (deal-game-tree #!optional (gen gen-board))
    (let1 (tree (game-tree (gen) 0 0 #t))
      (if (stream:null? (caddr tree)) (deal-game-tree gen) tree)))

  ; the book's `web-initialize`, for a game of its own, under `id` if it is given.
  (define (web-game-new! #!optional tree id)
    (evict!)
    (unless tree (hash-table-clear! game-tree/previous))
    (let1 (g (make-web-game (or id (fresh-id)) (or tree (deal-game-tree)) #f 0 '((b "Your turn.")) '() #f #f 0 0))
      (hash-table-set! web-games (web-game-id g) (touch! g))
      g))

  ; the game under `id`, unless it is idle, and then it goes.
  (define (web-game-lookup id)
    (and (string? id)
         (let1 (g (hash-table-ref/default web-games id #f))
           (cond
             ((not g) #f)
             ((idle? g (current-seconds)) (hash-table-delete! web-games id) #f)
             (else (touch! g))))))

  (define (log! g entry)
    (let1 (l (cons entry (web-game-log g)))
      (web-game-log-set! g (take-at-most l 12))))

  (define (take-at-most l n) (take l (min n (length l))))

  (define (board-dice board player)
    (apply + (map cadr (filter (λ (hex) (eqv? (car hex) player)) (vector->list board)))))

  (define (passing-allowed? tree)
    (let1 (moves (caddr tree))
      (and (not (stream:null? moves)) (not (car (stream:car moves))))))

  ; the next tree after `move` of the player of `tree`, with its log entry, the dice rolled for an
  ; attack as v4's `pick-chance-branch` rolls them.
  (define (play! g tree move)
    (let ((board (cadr tree))
          (player (car tree)))
      (web-game-prev-board-set! g board)
      (web-game-from-tile-set! g #f)
      (if (car move)
        (let* ((src (caar move))
               (dst (cadar move))
               (src-dice (cadr (vector-ref board src)))
               (dst-dice (cadr (vector-ref board dst)))
               (src-roll (roll-dice src-dice))       ; the attacker first
               (dst-roll (roll-dice dst-dice))
               (captured? (> src-roll dst-roll))
               (entry (list 'attack player src dst src-dice dst-dice src-roll dst-roll captured?)))
          (web-game-tree-set! g (if captured? (cadr move) (caddr move)))
          (web-game-last-move-set! g (list src dst captured?))
          (log! g entry)
          entry)
        (let* ((next (cadr move))
               (entry (list 'pass player (- (board-dice (cadr next) player) (board-dice board player)))))
          (web-game-tree-set! g next)
          (web-game-last-move-set! g #f)
          (log! g entry)
          entry))))

  ; "4 dice rolled 15 vs 3 dice rolled 9"
  (define (rolls entry)
    (match1/first ((attack _ _ _ ,src-dice ,dst-dice ,src-roll ,dst-roll _) entry)
      (conc (dice-word src-dice) " rolled " src-roll " vs " (dice-word dst-dice) " rolled " dst-roll)))

  (define (outcome entry) (if (list-ref entry 8) "captured" "repelled"))

  (define (say! g . message) (web-game-message-set! g message))

  ; nothing else changes the board: no tile shows a change.
  (define (keep-board! g) (web-game-prev-board-set! g (cadr (web-game-tree g))))

  ; the human's choice: #f, pass or a hex (the book's `web-handle-human`, checked).
  (define (web-handle-human g pos)
    (let* ((tree (web-game-tree g))
           (from (web-game-from-tile g)))
      (cond
        ((not pos))
        ((eq? pos 'pass)
         (if (passing-allowed? tree)
           (begin (play! g tree (stream:car (caddr tree)))
                  (say! g "Your reinforcements have been placed."))
           (say! g "You must attack at least once before ending your turn.")))
        ((not from)
         (keep-board! g)
         (if (memv pos (legal-tiles tree #f))
           (begin (web-game-from-tile-set! g pos)
                  (say! g '(b "Your turn.")))
           (say! g (conc "Hex " pos " cannot attack."))))
        ((eqv? pos from)
         (keep-board! g)
         (web-game-from-tile-set! g #f)
         (say! g "Move cancelled."))
        ((memv pos (legal-tiles tree from))
         (let1 (entry (play! g tree ((stream:find (λ (move) (equal? (car move) (list from pos)))) (caddr tree))))
           (say! g (conc "You attacked " from " → " pos ": " (rolls entry) " — " (outcome entry) "."))))
        (else (say! g (conc "Hex " pos " cannot be attacked from hex " from "."))))))

  ; one move of the computer (the book's `web-handle-computer`); the engine's memo of `game-tree`
  ; is emptied afterwards, see the header.
  (define (web-handle-computer g)
    (let* ((tree (web-game-tree g))
           (entry (play! g tree (choose-computer-move tree)))
           (who `(b ,(letter (cadr entry)))))
      (hash-table-clear! game-tree/previous)
      (if (eq? (car entry) 'attack)
        (match1/first ((attack _ ,src ,dst _ _ _ _ _) entry)
          (say! g who (conc " attacked " src " → " dst ": " (rolls entry) " — " (outcome entry) ".")))
        (apply say! g who (conc " ended the turn and gained " (dice-word (caddr entry)) ".")
               (if (zero? (car (web-game-tree g))) '(" " (b "Your turn.")) '())))))

  ; the book's dispatch of `dod-request-handler`; the step grows when the tree or the chosen hex
  ; changes.
  (define (web-game-advance! g chosen)
    (let ((tree (web-game-tree g))
          (from (web-game-from-tile g)))
      (cond
        ((stream:null? (caddr tree)))
        ((zero? (car tree)) (web-handle-human g chosen))
        (else (web-handle-computer g)))
      (unless (and (eq? tree (web-game-tree g)) (eqv? from (web-game-from-tile g)))
        (web-game-step-set! g (add1 (web-game-step g))))))

  ; the link of a choice in game `g` at its current step: #f is nil.
  (define (make-game-link g chosen)
    (conc "/game.html?g=" (web-game-id g) "&t=" (web-game-step g) "&chosen="
          (cond
            ((not chosen) "nil")
            ((symbol? chosen) (symbol->string chosen))
            (else chosen))))

  ; the link of New game and Play again: the id of the game it starts is drawn now, so the link
  ; answers that same game when it is followed again.
  (define (make-new-game-link) (conc "/game.html?new=" (fresh-id)))

  ; v3's `parse-chosen`, then checked: pass, a hex, or #f for anything else.
  (define (parse-choice s)
    (let1 (c (parse-chosen s))
      (cond
        ((eq? c 'pass) 'pass)
        ((and (exact-integer? c) (<= 0 c) (< c (*board-hexnum*))) c)
        (else #f))))

  ; the page ------------------------------------------------------------------------------------------

  (define (glyph player)
    `(svg (@ (class ,(conc "glyph p" player)) (viewBox "0 0 14 14") (aria-hidden "true"))
          ,(case (list-ref (*player-glyphs*) player)
             ((circle) '(circle (@ (cx "7") (cy "7") (r "5.2"))))
             ((triangle) '(polygon (@ (points "7,1.4 12.8,11.6 1.2,11.6"))))
             ((square) '(rect (@ (x "2") (y "2") (width "10") (height "10"))))
             (else '(polygon (@ (points "7,0.8 13.2,7 7,13.2 0.8,7")))))))

  (define logo
    '(svg (@ (viewBox "0 0 26 26") (aria-hidden "true"))
          (path (@ (d "M13 2 23 7 13 12 3 7Z") (fill "var(--p0)")))
          (path (@ (d "M3 7 13 12v12L3 19Z") (fill "var(--p1)")))
          (path (@ (d "M23 7 13 12v12l10-5Z") (fill "var(--p2)")))))

  (define favicon
    (string-append "data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 16 16'%3E"
                   "%3Cpath d='M8 1 15 4.5 8 8 1 4.5Z' fill='%23D55E00'/%3E"
                   "%3Cpath d='M1 4.5 8 8v7L1 11.5Z' fill='%230072B2'/%3E"
                   "%3Cpath d='M15 4.5 8 8v7l7-3.5Z' fill='%23009E73'/%3E%3C/svg%3E"))

  ; "a", "a and c", "a, b and c"
  (define (letters players)
    (let1 (ls (map letter players))
      (if (null? (cdr ls))
        (car ls)
        (conc (string-intersperse (reverse (cdr (reverse ls))) ", ") " and " (last ls)))))

  (define (hexes-of board player) (count (λ (hex) (eqv? (car hex) player)) (vector->list board)))

  ; the book's `web-announce-winner`, as a banner; given the player to move, who has no move, it
  ; first says why the game ended.
  (define (web-announce-winner board #!optional to-move)
    (let ((w (winners board))
          (mine (hexes-of board 0)))
      `(div (@ (class "banner") (role "alert"))
            (span ,@(cond
                      ((not to-move) '())
                      (else `((span (@ (class "why"))
                                    ,(conc (if (zero? to-move) "You have" (conc (letter to-move) " has"))
                                           (if (zero? (hexes-of board to-move)) " no hex left" " no attack left")
                                           ", so the game ends."))
                              " ")))
                  ,@(if (pair? (cdr w))
                      `("The game is a tie between " ,(letters w) ".")
                      `("The winner is " (b ,(letter (car w)))
                        ,(if (zero? mine)
                           " — you hold no hex."
                           (conc " — you hold " mine " of " (vector-length board) " hexes.")))))
            (a (@ (class "btn primary") (href ,(make-new-game-link))) "Play again"))))

  (define (log-item entry i)
    (let ((player (cadr entry))
          (you? (zero? (cadr entry))))
      `(li (@ ,@(if (>= i 3) '((class "older")) '()))
           ,(glyph player)
           (span ,@(if (eq? (car entry) 'attack)
                     (match1/first ((attack _ ,src ,dst _ _ _ _ ,captured?) entry)
                       `((span (@ (class "move")) ,(conc (if you? "You attacked " (conc (letter player) " ")) src " → " dst))
                         " " (span (@ (class "roll")) ,(rolls entry))
                         " · " (span (@ (class ,(if captured? "won" "lost"))) ,(outcome entry))))
                     `(,(conc (if you? "You" (letter player)) " ended the turn · +" (dice-word (caddr entry)))))))))

  ; the row of a player, the one to move or one of the `winners` marked; a player out of the game
  ; has its name struck and dashes for numbers.
  (define (player-row stat current #!optional (winners '()))
    (match1/first ((,player ,hexes ,dice ,gain) stat)
      (let ((current? (eqv? player current))
            (out? (zero? hexes)))
        `(tr (@ ,@(cond
                    (current? `((class "current") (aria-current "step") (style ,(conc "--pc:var(--p" player ")"))))
                    ((memv player winners) `((class "winner") (style ,(conc "--pc:var(--p" player ")"))))
                    (out? '((class "out")))
                    (else '())))
             (td (span (@ (class "who")) ,(glyph player)
                       (span (@ (class "name")) (b ,(letter player)) " " ,(if (zero? player) "You" "Computer"))
                       ,@(cond
                           (current? '((span (@ (class "sr")) " (to move)")))
                           ((memv player winners) '((span (@ (class "sr")) " (winner)")))
                           (out? '((span (@ (class "sr")) " (out)")))
                           (else '()))))
             ,@(if out?
                 '((td "–") (td "–") (td "–"))
                 `((td ,hexes) (td ,dice) (td ,(conc "+" gain))))))))

  ; the status line: what happened, then what to do.
  (define (status g)
    (let* ((tree (web-game-tree g))
           (from (web-game-from-tile g))
           (hint (λ (s) `(" " (span (@ (class "hint")) ,s)))))
      `(,@(web-game-message g)
        ,@(cond
            ((not (zero? (car tree))) '())
            (from (hint (conc "Choose a hex to attack from hex " from ", or select it again to cancel.")))
            ((not (passing-allowed? tree)) (hint "Choose a hex to attack from."))
            ((pair? (legal-tiles tree #f)) (hint "You may now end your turn or attack again."))
            (else (hint "No attack is left: end your turn."))))))

  (define (actions g)
    (let ((tree (web-game-tree g))
          (from (web-game-from-tile g)))
      (if (zero? (car tree))
        `(,(if (passing-allowed? tree)
             `(a (@ (class "btn primary") (href ,(make-game-link g 'pass))) "End turn")
             '(span (@ (class "btn primary") (aria-disabled "true")) "End turn"))
          ,@(if from `((a (@ (class "btn") (href ,(make-game-link g from))) "Cancel selection")) '()))
        `((a (@ (class "btn primary") (href ,(make-game-link g #f)) (data-auto ,(number->string (*auto-advance-ms*)))
                (style ,(conc "--auto:" (*auto-advance-ms*) "ms")))
             "Continue")
          ; shown by the script, which follows Continue by itself
          (button (@ (class "btn") (type "button") (data-pause "") (hidden "")) "Pause " (kbd "Esc"))))))

  ; the moves as a list of links, for small screens, where the board is small: the sources, or the
  ; targets of the chosen hex with their odds.
  (define (move-list g)
    (let* ((tree (web-game-tree g))
           (board (cadr tree))
           (from (web-game-from-tile g))
           (dice (λ (pos) (cadr (vector-ref board pos))))
           (tiles (legal-tiles tree from)))
      (if (null? tiles)
        '()
        `((nav (@ (class "moves") (aria-label "Moves"))
               (h2 (@ (class "eyebrow")) ,(if from (conc "Attack from hex " from) "Attack from"))
               (ul ,@(map (λ (pos)
                            `(li (a (@ (href ,(make-game-link g pos)))
                                    ,@(if from
                                        `(,(glyph (car (vector-ref board pos))) (b ,(number->string pos))
                                          (span ,(dice-word (dice pos)))
                                          (span (@ (class "odds")) ,(conc (inexact->exact (round (* 100 (attack-odds board from pos)))) "%")))
                                        `((b ,(number->string pos)) (span ,(dice-word (dice pos))))))))
                          tiles)))))))

  ; the book's `draw-dod-page`, the whole page; `fresh` when the request made the game, so that the
  ; address becomes the game's own.
  (define (draw-dod-page g #!key fresh)
    (let* ((tree (web-game-tree g))
           (board (cadr tree))
           (player (car tree))
           (over? (stream:null? (caddr tree)))
           (human? (and (not over?) (zero? player)))
           (from (and human? (web-game-from-tile g)))
           (stats (board-stats board))
           (hexnum (vector-length board)))
      (receive (changes new-dice) (board-changes (web-game-prev-board g) board)
        `(*TOP*
           (html (@ (lang "en"))
                 (head
                   (meta (@ (charset "utf-8")))
                   (meta (@ (name "viewport") (content "width=device-width, initial-scale=1")))
                   (meta (@ (name "color-scheme") (content "light dark")))
                   (meta (@ (name "robots") (content "noindex, nofollow")))
                   (title ,(cond
                             (over? (let1 (w (winners board))
                                      (if (pair? (cdr w)) (conc (letters w) " tie") (conc (letter (car w)) " wins"))))
                             (human? "Your turn")
                             (else (conc (letter player) " is playing")))
                          " · Dice of Doom")
                   (link (@ (rel "icon") (href ,favicon)))
                   (script (raw "document.documentElement.classList.add('js');"
                                ,@(if fresh
                                    (list (conc "history.replaceState(null,'','" (make-game-link g #f) "');"))
                                    '())))
                   (style (raw ,dod-css)))
                 (body
                   (div (@ (class "shell"))
                        (header (@ (class "top"))
                                (div (@ (class "brand"))
                                     ,logo
                                     (h1 "Dice of Doom " (small "· after " (cite "Land of Lisp"))))
                                (nav (a (@ (class "btn") (href ,(make-new-game-link))) "New game")))
                        (main (@ (class "game"))
                              (section (@ (class "stage") (aria-label "Board"))
                                       ,(if over?
                                          (web-announce-winner board player)
                                          `(p (@ (class "status") (id "status") (role "status")) ,@(status g)))
                                       ,(draw-board-svg board from (if human? (legal-tiles tree from) '())
                                                        make-link: (and human? (λ (pos) (make-game-link g pos)))
                                                        last-move: (web-game-last-move g)
                                                        changes: changes
                                                        new-dice: new-dice)
                                       ,@(if human? (move-list g) '()))
                              (aside (@ (class "side"))
                                     (section (@ (class "card") (aria-labelledby "h-players"))
                                              (h2 (@ (class "eyebrow") (id "h-players")) "Players")
                                              (div (@ (class "share") (role "img")
                                                      (aria-label ,(conc "Hexes held: "
                                                                         (string-intersperse
                                                                           (map (λ (s) (conc (letter (car s)) " " (cadr s) " of " hexnum))
                                                                                stats)
                                                                           ", "))))
                                                   ,@(map (λ (s) `(span (@ (style ,(conc "flex:" (cadr s) ";background:var(--p" (car s) ")")))))
                                                          (filter (λ (s) (positive? (cadr s))) stats)))
                                              (table (@ (class "players"))
                                                     (thead (tr (th (@ (scope "col")) "Player")
                                                                (th (@ (scope "col") (title "Hexes held")) "Hexes")
                                                                (th (@ (scope "col") (title "Dice on the board")) "Dice")
                                                                (th (@ (scope "col") (title "Dice gained when the turn ends: the size of the largest connected group of hexes")) "Gain")))
                                                     (tbody ,@(map (λ (s) (player-row s (and (not over?) player) (if over? (winners board) '()))) stats))))
                                     ,@(if over? '() `((nav (@ (class "actions") (aria-label "Turn")) ,@(actions g))))
                                     (section (@ (class "card log") (aria-labelledby "h-log"))
                                              (h2 (@ (class "eyebrow") (id "h-log")) "Last moves")
                                              ,(let1 (entries (take-at-most (web-game-log g) 8))
                                                 (if (null? entries)
                                                   '(p (@ (class "empty")) "No move yet: you attack first.")
                                                   `(ol ,@(map log-item entries (iota (length entries)))))))))
                        (footer (@ (class "foot"))
                                "Computer players search the game tree with alpha–beta pruning, weighting each attack "
                                "by its chance of winning the roll. Hexes are numbered 0–" ,(sub1 hexnum)
                                ", row by row from the back."))
                   ,@(if (or over? human?) '() `((script (raw ,dod-js))))))))))

  (define dod-css #<<CSS
:root {
  color-scheme: light dark;
  --bg: #F6F4EF; --surface: #FFFFFF; --ink: #1F2228; --muted: #686C73; --line: #E3E0D8;
  --accent: #1F2228; --accent-ink: #FFFFFF; --focus: #0072B2; --tint: rgba(31, 34, 40, .05);
  --p0: #D55E00; --p1: #0072B2; --p2: #009E73; --p3: #B8578F;
  --win: #1F7A55; --lose: #9A3B12;
  --radius: 10px; --gap: clamp(16px, 3vw, 32px);
  --font: system-ui, -apple-system, "Segoe UI", Roboto, "Helvetica Neue", Arial, "Noto Sans", sans-serif;
  --mono: ui-monospace, "SF Mono", Menlo, Consolas, "DejaVu Sans Mono", monospace;
}
@media (prefers-color-scheme: dark) {
  :root:not([data-theme="light"]) {
    --bg: #15171C; --surface: #1C1F25; --ink: #E9E7E2; --muted: #9EA1A7; --line: #2B2F37;
    --accent: #E9E7E2; --accent-ink: #15171C; --focus: #56B4E9; --tint: rgba(233, 231, 226, .06); --win: #6CCB9F; --lose: #F0A070;
  }
}
:root[data-theme="dark"] {
  --bg: #15171C; --surface: #1C1F25; --ink: #E9E7E2; --muted: #9EA1A7; --line: #2B2F37;
  --accent: #E9E7E2; --accent-ink: #15171C; --focus: #56B4E9; --tint: rgba(233, 231, 226, .06); --win: #6CCB9F; --lose: #F0A070;
}

* { box-sizing: border-box; }
html { -webkit-text-size-adjust: 100%; }
body {
  margin: 0; background: var(--bg); color: var(--ink);
  font: 15px/1.5 var(--font); font-variant-numeric: tabular-nums;
}
a { color: inherit; }
.shell { max-width: 1240px; margin: 0 auto; padding: 20px var(--gap) 28px; }

/* header ------------------------------------------------------------------------------- */
.top { display: flex; align-items: center; justify-content: space-between; gap: 16px; }
.brand { display: flex; align-items: center; gap: 10px; }
.brand svg { width: 26px; height: 26px; }
.brand h1 { margin: 0; font-size: 17px; font-weight: 650; letter-spacing: .02em; }
.brand small { color: var(--muted); font-size: 13px; font-weight: 400; letter-spacing: 0; }

/* layout ------------------------------------------------------------------------------- */
.game {
  display: grid; grid-template-columns: minmax(0, 1fr) 300px; gap: var(--gap);
  align-items: start; margin-top: 18px;
}
@media (max-width: 1020px) { .game { grid-template-columns: minmax(0, 1fr); } }

.status {
  margin: 0 0 6px; min-height: 3em; font-size: 16px;
}
.status .hint { color: var(--muted); }
.banner {
  display: flex; align-items: center; justify-content: space-between; gap: 12px;
  margin: 0 0 8px; padding: 12px 16px; border-radius: var(--radius);
  background: var(--surface); border: 1px solid var(--line); font-size: 17px; font-weight: 600;
}
.banner .btn { flex: none; white-space: nowrap; }
.banner .why { font-weight: 400; color: var(--muted); }

/* the board ---------------------------------------------------------------------------- */
.board { display: block; width: 100%; height: auto; overflow: visible; }
.board .hex { outline: none; }
.board .lift { transition: transform .18s ease, filter .18s ease; }
.board a.hex { cursor: pointer; }
.board a.hex:hover .lift,
.board a.hex:focus-visible .lift { transform: translateY(-5px); filter: brightness(1.06); }
.board a.chosen .lift { transform: translateY(-6px); }
.board a.chosen:hover .lift { transform: translateY(-3px); }
.board a.hex:focus-visible .ring-line { stroke-opacity: 1; stroke-width: 3.4; stroke-dasharray: none; }
.board a.hex:focus-visible .ring-under { stroke-opacity: .9; stroke-width: 7.4; }
.board .ring-line { transition: stroke-opacity .18s ease; }
.board a.hex:hover .ring-line { stroke-opacity: 1; }
.board .captured .dice, .board use.new { animation: dod-arrive .55s cubic-bezier(.2, .8, .2, 1) both; }
.board .captured .top, .board .spent .top { animation: dod-flash 1.1s ease-out both; }
.board .lastmove { animation: dod-fade .4s ease-out both; pointer-events: none; }
.board .overlay { pointer-events: none; }
.board .overlay a { pointer-events: auto; cursor: pointer; }
.board .overlay a:hover .odds { filter: brightness(.94); }
.board .gain { animation: dod-fade .25s ease-out both; }
@keyframes dod-arrive { from { transform: translateY(-16px); opacity: 0; } }
@keyframes dod-flash { from { filter: brightness(1.35); } }
@keyframes dod-fade { from { opacity: 0; } }
@media (prefers-color-scheme: dark) { :root:not([data-theme="light"]) .board .ground { fill-opacity: .55; } }
:root[data-theme="dark"] .board .ground { fill-opacity: .55; }

/* side panel --------------------------------------------------------------------------- */
.side { display: grid; gap: 18px; }
.card { background: var(--surface); border: 1px solid var(--line); border-radius: var(--radius); padding: 14px 16px; }
.eyebrow {
  margin: 0 0 8px; font-size: 11.5px; font-weight: 600; letter-spacing: .08em;
  text-transform: uppercase; color: var(--muted);
}
.share { display: flex; height: 6px; border-radius: 3px; overflow: hidden; margin: 2px 0 12px; gap: 2px; }
.share span { display: block; }
.players { width: 100%; border-collapse: collapse; font-size: 14px; }
.players th { font-size: 11.5px; font-weight: 500; color: var(--muted); text-align: right; padding: 0 0 6px 10px; }
.players th:first-child { text-align: left; }
.players td { padding: 7px 0 7px 10px; text-align: right; border-top: 1px solid var(--line); white-space: nowrap; }
.players td:first-child { text-align: left; padding-left: 8px; }
.players th:first-child { padding-left: 8px; }
.players tr.current td { font-weight: 650; background: var(--tint); }
.players tr.current td:first-child { border-radius: 6px 0 0 6px; box-shadow: inset 3px 0 0 var(--pc, var(--ink)); }
.players tr.current td:last-child { border-radius: 0 6px 6px 0; padding-right: 8px; }
.players td:last-child, .players th:last-child { padding-right: 8px; }
.players tr.winner td { font-weight: 650; }
.players tr.winner td:first-child { box-shadow: inset 3px 0 0 var(--pc, var(--ink)); }
.players .who { display: inline-flex; align-items: center; gap: 8px; }
.sr { position: absolute; width: 1px; height: 1px; overflow: hidden; clip: rect(0 0 0 0); white-space: nowrap; }
.players tr.out td { color: var(--muted); }
.players tr.out .name { text-decoration: line-through; text-decoration-thickness: 1px; }
.players tr.out .glyph { opacity: .5; }
.glyph { width: 14px; height: 14px; flex: none; }
.glyph.p0 { fill: var(--p0); } .glyph.p1 { fill: var(--p1); }
.glyph.p2 { fill: var(--p2); } .glyph.p3 { fill: var(--p3); }

.actions { display: flex; flex-wrap: wrap; gap: 8px; }
.btn {
  display: inline-flex; align-items: center; justify-content: center; gap: 6px; position: relative;
  min-height: 38px; padding: 0 16px; border-radius: 999px; border: 1px solid var(--line);
  background: var(--surface); color: var(--ink); font: 600 14px/1 var(--font);
  text-decoration: none; transition: background .15s ease, border-color .15s ease, transform .15s ease;
}
.btn:hover { border-color: var(--muted); }
.btn:active { transform: translateY(1px); }
.btn:focus-visible { outline: 2px solid var(--focus); outline-offset: 2px; }
.btn.primary { background: var(--accent); border-color: var(--accent); color: var(--accent-ink); }
.btn[aria-disabled="true"] { opacity: .4; pointer-events: none; }
.btn[hidden] { display: none; }
button.btn { cursor: pointer; }
.btn kbd { font: 500 11px var(--mono); opacity: .6; }
.js .btn[data-auto] { overflow: hidden; }
.js .btn[data-auto]::after {
  content: ""; position: absolute; left: 0; bottom: 0; height: 2px; width: 100%;
  background: currentColor; opacity: .5; transform-origin: left;
  animation: dod-countdown var(--auto, 1s) linear both;
}
@keyframes dod-countdown { from { transform: scaleX(0); } }

.log ol { list-style: none; margin: 0; padding: 0; display: grid; gap: 8px; font-size: 13.5px; }
.log li { display: grid; grid-template-columns: 14px 1fr; gap: 8px; align-items: start; }
.log li .glyph { margin-top: 3px; }
.log li:first-child { animation: dod-fade .4s ease-out both; }
.log .move { font-weight: 600; }
.log .roll { color: var(--muted); }
.log .won { color: var(--win); font-weight: 600; }
.log .lost { color: var(--lose); font-weight: 600; }
.log li.older { color: var(--muted); }
.log li.older .move { font-weight: 500; }
.log .empty { margin: 0; color: var(--muted); font-size: 13.5px; }

.moves { display: none; }
.moves .eyebrow { margin: 10px 0 6px; }
.moves ul { list-style: none; margin: 0; padding: 0; display: flex; flex-wrap: wrap; gap: 8px; }
.moves a {
  display: inline-flex; align-items: center; gap: 6px; min-height: 44px; padding: 0 14px;
  border-radius: 999px; border: 1px solid var(--line); background: var(--surface);
  text-decoration: none; font-size: 14px;
}
.moves a span { color: var(--muted); }
.moves a .odds { color: var(--ink); font-weight: 650; }
.moves a:focus-visible { outline: 2px solid var(--focus); outline-offset: 2px; }

.foot { margin-top: 22px; color: var(--muted); font-size: 12.5px; }
.foot a { color: inherit; }

/* small screens: the board first, full bleed, panel below ------------------------------ */
@media (max-width: 560px) {
  body { font-size: 14px; }
  .shell { padding: 12px 16px 20px; }
  .brand small { display: none; }
  .game { margin-top: 10px; gap: 14px; }
  .stage { margin: 0 -10px; }
  .status { padding: 0 10px; font-size: 15px; min-height: 4.5em; }
  .banner { font-size: 15px; padding: 10px 12px; margin: 0 10px 8px; }
  .moves { display: block; padding: 0 10px; }
  .btn { min-height: 44px; }
}
@media (prefers-reduced-motion: reduce) {
  *, *::before, *::after { animation: none !important; transition: none !important; }
}
CSS
)

  ; follows the Continue link after its delay, unless Pause or Escape stops it.
  (define dod-js #<<JS
(function () {
  var a = document.querySelector('a[data-auto]'); if (!a) return;
  var p = document.querySelector('[data-pause]');
  var t = setTimeout(function () { location.replace(a.href); }, +a.dataset.auto);
  function pause() { clearTimeout(t); a.removeAttribute('data-auto'); if (p) p.hidden = true; }
  if (p) { p.hidden = false; p.addEventListener('click', pause); }
  addEventListener('keydown', function (e) { if (e.key === 'Escape') pause(); });
})();
JS
)

  ; serving -----------------------------------------------------------------------------------------

  ; `raw` writes the CSS and the script as they are.
  (define dod-rules
    `((*TOP* . ,(λ (tag . body) body))
      (raw *preorder* . ,(λ (tag . body) body))
      ,@universal-conversion-rules*))

  ; the value of `name` in `params`, if it is a string; both servers give lower-case symbols.
  (define (param name params)
    (let1 (p (assq name params))
      (and p (string? (cdr p)) (cdr p))))

  ; the game of a request, after its choice: (values game fresh?), fresh when the address does not
  ; name the game by `g`.
  (define (web-game-request params)
    (let ((id (param 'g params))
          (new (param 'new params))
          (step (param 't params))
          (chosen (param 'chosen params)))
      (cond
        (new (values (or (web-game-lookup new) (web-game-new! #f (and (game-id? new) new))) #t))
        ((not chosen) (values (web-game-new!) #t))
        ((web-game-lookup id)
         => (λ (g)
              (when (equal? step (number->string (web-game-step g)))
                (web-game-advance! g (parse-choice chosen)))
              (values g #f)))
        (else (let1 (g (web-game-new!))
                (when (and id (positive? (string-length id)))
                  (say! g "That game has expired, so a new one has started. " '(b "Your turn.")))
                (values g #t))))))

  (define (with-lock mutex thunk)
    (dynamic-wind (τ (mutex-lock! mutex)) thunk (τ (mutex-unlock! mutex))))

  ; the book's handler: game.html (or the root) is the game, any other page is unknown.
  (define (dod-request-handler path header params)
    (if (member path '("game.html" ""))
      (let1 (page (with-lock web-games-lock (τ (receive (g fresh?) (web-game-request params)
                                                  (draw-dod-page g fresh: fresh?)))))
        (display "<!doctype html>")
        (SRV:send-reply (pre-post-order* page dod-rules)))
      (display "Sorry... I don't know that page.")))

  ; the game on spiffy, for ever; the handler does its own locking. The keywords are those of
  ; (aux land-of-lisp webserver spiffy)'s `serve`: `listener:` serves on a listener already open.
  (define (dod-serve #!key (port 8080) (bind-address #f) (listener #f))
    (serve dod-request-handler port: port bind-address: bind-address listener: listener serialize: #f))

  )
