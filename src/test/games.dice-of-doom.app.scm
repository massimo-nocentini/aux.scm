
; Tests of (aux games dice-of-doom app), the deployable Dice of Doom v4 of "Land of Lisp" (Conrad
; Barski, chapters 19 and 20): colours, geometry, the SVG board, the page, the protocol of games and
; step tokens, and whole games, in the process and over HTTP.
;
; The expected numbers come from the UI design (ui.md, sections 3, 4, 9 and 13), whose prototype
; computed them in Python with the same formulas; colours round half to even in both. The fixture is
; the board of the prototype, ui-proto/page.png. The seeded cases depend on CHICKEN's generator.
;
; Visual check, by hand: write (draw-board-svg fixture 11 '(6 12 17) standalone: #t ...) serialised
; with `dod-rules` to a file, render it with resvg or a browser and compare it with
; ui-proto/board-chosen.png; or run `csi -s dice-of-doom-server.scm` in src and play.

(import scheme
        (chicken base)
        (chicken io)
        (chicken tcp)
        (chicken port)
        (chicken string)
        (chicken random)
        (chicken bytevector)
        (only (scheme base) read-bytevector)
        (only srfi-1 iota every count filter remove find take first last)
        (only srfi-13 string-contains string-index string-prefix?)
        srfi-18
        srfi-69
        sxml-transforms
        (aux base)
        (aux unittest)
        (aux stream sicp)
        (only (aux games dice-of-doom v4) game-tree neighbors *max-dice* *ai-level* *board-hexnum*
              pick-chance-branch game-tree/previous)
        (aux games dice-of-doom app))

(define fixture #((1 3) (1 2) (2 4) (2 1) (3 2)
                  (0 2) (1 5) (1 1) (2 3) (3 4)
                  (0 4) (0 3) (1 2) (3 1) (3 3)
                  (0 1) (0 5) (2 2) (2 5) (3 2)
                  (0 3) (2 1) (0 2) (1 3) (3 5)))

(define (fixture-game) (web-game-new! (game-tree fixture 0 0 #t)))

(define (serialise sxml) (with-output-to-string (τ (SRV:send-reply (pre-post-order* sxml dod-rules)))))

(define (contains? s sub) (and (string-contains s sub) #t))

(define (occurrences s sub)
  (let loop ((i 0) (n 0))
    (let1 (j (string-contains s sub i))
      (if j (loop (add1 j) (add1 n)) n))))

; the elements of an SXML tree that satisfy `pred?`, in document order.
(define (elements pred? tree)
  (cond
    ((not (pair? tree)) '())
    ((and (symbol? (car tree)) (pred? tree)) (cons tree (append-map* (λ (t) (elements pred? t)) (cdr tree))))
    (else (append-map* (λ (t) (elements pred? t)) (if (symbol? (car tree)) (cdr tree) tree)))))

(define (append-map* f l) (apply append (map f l)))

(define (attr name element)
  (let1 (@ (and (pair? (cdr element)) (pair? (cadr element)) (eq? (caadr element) '@) (cdadr element)))
    (and @ (let1 (a (assq name @)) (and a (cadr a))))))

(define (class-of? cls) (λ (e) (let1 (c (attr 'class e)) (and c (member cls (string-split c " ")) #t))))

(define (tag? name) (λ (e) (eq? (car e) name)))

; a request to the handler for game `g` at its current step: the page.
(define (request g chosen #!key (step (web-game-step g)))
  (with-output-to-string
    (τ (dod-request-handler "game.html" '() `((g . ,(web-game-id g)) (t . ,(number->string step)) (chosen . ,chosen))))))

(define (decode-href s) (string-translate* s '(("&amp;" . "&"))))

; the params of a link of the page, /game.html?g=..&t=..&chosen=..
(define (link-params href)
  (map (λ (kv) (let1 (p (string-split kv "=")) (cons (string->symbol (car p)) (if (pair? (cdr p)) (cadr p) ""))))
       (string-split (cadr (string-split href "?")) "&")))

; the hrefs of the page that hold `chosen=`, in order.
(define (game-links page) (filter (λ (l) (string-contains l "chosen=")) (page-links page)))

; the hrefs of the page to /game.html?...
(define (page-links page)
  (let loop ((i 0) (acc '()))
    (let1 (j (string-contains page "href=\"/game.html?" i))
      (if (not j)
        (reverse acc)
        (let* ((start (+ j 6))
               (end (string-index page #\" start)))
          (loop end (cons (decode-href (substring page start end)) acc)))))))

; the link of New game on the page
(define (new-game-link page) (find (λ (l) (string-contains l "?new=")) (page-links page)))

(define (handle params) (with-output-to-string (τ (dod-request-handler "game.html" '() params))))

(define-suite dice-of-doom-app-suite

  ((test/dice-of-doom-app/svg-num _)
   (⊦= "192" (svg-num 192.))
   (⊦= "371.2" (svg-num 371.2))
   (⊦= "-3.6" (svg-num -3.6))
   (⊦= "0.1" (svg-num 0.105))           ; 0.105 is a little less in binary: 0.1, as the prototype
   (⊦= "0" (svg-num -0.))
   (⊦= "0" (svg-num -0.001))
   (⊦= "269.57" (svg-num 269.568))
   (⊦= "0.05" (svg-num 0.05))
   (⊦= "7" (svg-num 7)))

  ((test/dice-of-doom-app/geometry _)
   (⊦= '((320 "192") (832 "192") (448 "281.6") (64 "371.2") (576 "371.2"))
       (map (λ (pos) (receive (xx yy) (hex-centre pos) (list xx (svg-num yy)))) '(0 4 12 20 24)))
   (receive (xx yy) (hex-centre 12)
     (⊦= "387.84,269.57 448,251.52 508.16,269.57 508.16,293.63 448,311.68 387.84,293.63"
         (points->string (hex-points xx yy 0 0.94)))
     (⊦= "387.84,277.25 448,259.2 508.16,277.25 508.16,301.31 448,319.36 387.84,301.31"
         (points->string (hex-points xx yy 0.12 0.94)))
     (⊦= '(("451.6" "281.6") ("444.4" "249.6") ("451.6" "217.6"))
         (map (λ (z) (receive (dx dy) (die-position 2 2 z xx yy) (list (svg-num dx) (svg-num dy)))) '(0 1 2)))))

  ((test/dice-of-doom-app/colours _)
   (⊦= "#D55E00" (colour->hex '(213 94 0)))
   (⊦= '(128 128 128) (mix '(0 0 0) '(255 255 255) 0.5))       ; 127.5 rounds to even
   (⊦= '(255 0 0) (mix '(200 10 0) '(300 -20 0) 1))            ; clamped
   ; ui.md 3.2: (p y die edge top side stroke mark)
   (for-each (λ (row)
               (let1 (c (tile-colours (car row) (cadr row) #f))
                 (⊦= (cddr row) (map (λ (key) (colour->hex (alist-ref key c))) '(die edge top side stroke mark)))))
             '((0 4 "#D55E00" "#753400" "#E39557" "#954200" "#6A2F00" "#843A00")
               (0 0 "#B75100" "#652D00" "#CF8C57" "#803900" "#5C2800" "#713200")
               (1 4 "#0072B2" "#003F62" "#57A2CC" "#00507D" "#003959" "#00476E")
               (1 0 "#006299" "#003654" "#5797BC" "#00456B" "#00314C" "#003D5F")
               (2 4 "#009E73" "#00573F" "#57BFA3" "#006F50" "#004F3A" "#006247")
               (2 0 "#008863" "#004B36" "#57B098" "#005F45" "#004432" "#00543D")
               (3 4 "#B8578F" "#65304F" "#D090B5" "#813D64" "#5C2C48" "#723659")
               (3 0 "#9E4B7B" "#572944" "#BF88A8" "#6F3456" "#4F263E" "#622E4C")))
   (⊦= '("#91745D" "#B6A394") (let1 (c (tile-colours 0 2 #t)) (map (λ (k) (colour->hex (alist-ref k c))) '(die top))))
   (⊦= '("#4A6575" "#8899A4") (let1 (c (tile-colours 1 2 #t)) (map (λ (k) (colour->hex (alist-ref k c))) '(die top))))
   (⊦= "#E6B792" (colour->hex (mix (alist-ref 'top (tile-colours 0 2 #f)) '(255 255 255) 0.35)))
   (⊦= (row-shade '(213 94 0) 4) '(213 94 0))
   (⊦= '(145 116 93) (mute '(198 87 0))))

  ((test/dice-of-doom-app/rules _)
   (⊦= '((0 7 20 7) (1 6 16 5) (2 6 16 3) (3 6 17 6)) (board-stats fixture))
   (⊦= '((5 1 6) (9 3) (6 16 5 10 12 17) (7 17 6 11 13 18) (15 21) (19 18 23))
       (map neighbors '(0 4 11 12 20 24)))
   (let1 (t (game-tree fixture 0 0 #t))
     (⊦= '(5 11 16 20 22) (legal-tiles t #f))
     (⊦= '(6 12 17) (legal-tiles t 11))
     (⊦= '() (legal-tiles t 13))
     (⊦= '(6 78 78) (map (λ (dst) (inexact->exact (round (* 100 (attack-odds fixture 11 dst))))) '(6 12 17))))
   (receive (changes new) (board-changes fixture (vector-map* (λ (pos hex) (case pos
                                                                             ((14) '(1 2))
                                                                             ((9) '(3 1))
                                                                             ((20) '(0 5))
                                                                             ((3) '(2 1))
                                                                             (else hex)))
                                                              fixture))
     (⊦= '((9 . spent) (14 . captured) (20 . reinforced)) changes)
     (⊦= '((20 . 2)) new))
   (⊦= '(pass 24 #f #f #f #f #f #f) (map parse-choice '("PASS" "24" "25" "-1" "2.0" "1+2i" "nil" "x")))
   (⊭ (parse-choice #t)))

  ((test/dice-of-doom-app/overlay _)
   (let1 (pill (serialise (draw-odds-pill 6 5 0.06)))
     (⊨ (contains? pill "x=\"362\""))
     (⊨ (contains? pill "y=\"41.3\""))
     (⊨ (contains? pill "x=\"384\" y=\"56.4\""))
     (⊨ (contains? pill ">6%</text>")))
   ; the gain of a pass: +2 above the 3 dice of hex 20
   (let1 (pill (serialise (draw-gain-pill 20 3 2)))
     (⊨ (contains? pill "<rect x=\"47\" y=\"239.7\" width=\"34\""))
     (⊨ (contains? pill "rx=\"10.5\" fill=\"#1F2228\""))
     (⊨ (contains? pill ">+2</text>")))
   ; the last attack stands on the stacks: 13 (1 die, top 259.6) to 14 (3 dice, top 195.6), the
   ; numbers in badges 14 above the tops, the arc from rim to rim
   (let ((won (serialise (draw-last-move fixture 13 14 #t)))
         (lost (serialise (draw-last-move fixture 13 14 #f))))
     (⊨ (contains? won "d=\"M581.77,218.23 Q640,123.6 692.98,156.71\""))
     (⊨ (contains? won "<circle cx=\"576\" cy=\"227.6\" r=\"9\""))
     (⊨ (contains? won "<circle cx=\"704\" cy=\"163.6\" r=\"9\""))
     (⊨ (contains? won ">13</text>"))
     (⊨ (contains? won ">14</text>"))
     (⊭ (contains? won "stroke-dasharray"))
     (⊨ (contains? lost "stroke-dasharray=\"6 5\"")))
   ; a 5-die stack on the back row: the arc bends no higher than the top of the board
   (let1 (arc (serialise (draw-last-move (vector-map* (λ (pos hex) (if (memv pos '(1 2)) '(1 5) hex)) fixture) 1 2 #t)))
     (⊨ (contains? arc " Q512,-10 "))))

  ((test/dice-of-doom-app/board _)
   (let* ((g (fixture-game))
          (svg (draw-board-svg fixture 11 '(6 12 17) make-link: (λ (pos) (make-game-link g pos))))
          (text (serialise svg))
          (links (filter (class-of? "hex") (elements (tag? 'a) svg)))
          (pills (remove (class-of? "hex") (elements (tag? 'a) svg))))
     (⊦= '("6" "11" "12" "17") (map (λ (a) (alist-ref 'chosen (link-params (attr 'href a)))) links))
     (⊦= 4 (length links))
     (⊦= 21 (length (filter (class-of? "hex") (elements (tag? 'g) svg))))
     (⊨ ((class-of? "chosen") (cadr links)))
     (⊦= '("target" "chosen" "target" "target") (map (λ (a) (caddr (string-split (attr 'class a) " "))) links))
     (⊨ (contains? text (conc "href=\"/game.html?g=" (web-game-id g) "&amp;t=0&amp;chosen=17\"")))
     (⊨ (contains? text "aria-label=\"Attack hex 17 (player c, 2 dice) from hex 11: 78% to capture\""))
     (⊭ (contains? text "<title>Hex 12: player b, 2 dice</title>"))          ; 12 is a target: a label instead
     (⊨ (contains? text "<title>Hex 15: player a, 1 die</title>"))
     (⊦= 69 (occurrences text "<use href=\"#die\""))
     ; muted while targeting: every idle tile, here 10 (a, row 2)
     (⊨ (contains? text "fill=\"#B6A394\""))
     (⊦= 3 (length (filter (class-of? "odds") (elements (tag? 'g) svg))))
     ; each pill is a link to its target, out of the tab order
     (⊦= '("6" "12" "17") (map (λ (a) (alist-ref 'chosen (link-params (attr 'href a)))) pills))
     (⊨ (every (λ (a) (equal? "-1" (attr 'tabindex a))) pills))
     (⊨ (every (λ (a) (pair? (filter (class-of? "odds") (elements (tag? 'g) a)))) pills))
     ; every idle tile is muted; the dice of those that hide the chosen tile or a target are faded
     ; and click-through: 16 (5 dice) hides 11, 18 (5) 12, 22 (2) 11 and 17, 23 (3) 12 and 17
     (⊦= 21 (length (filter (class-of? "muted") (elements (tag? 'g) svg))))
     (⊦= '(16 18 22 23)
         (filter (λ (pos)
                   (contains? (serialise (car (filter (λ (e) (contains? (serialise e) (conc "<title>Hex " pos ":")))
                                                      (filter (class-of? "hex") (elements (tag? 'g) svg)))))
                              "<g class=\"dice\" opacity=\".3\" pointer-events=\"none\">"))
                 '(0 1 2 3 4 5 7 8 9 10 13 14 15 16 18 19 20 21 22 23 24)))
     (⊦= 4 (occurrences text "class=\"ring-under\""))
     (⊦= 4 (occurrences text "class=\"ring-line\"")))
   ; nothing chosen: no muting; the painter's order is pos 0 ... 24; ground first, overlay last
   (let* ((svg (draw-board-svg fixture #f '() last-move: '(13 14 #t)))
          (text (serialise svg))
          (positions (map (λ (pos) (string-contains text (conc "<title>Hex " pos ":"))) (iota 25)))
          (children (filter pair? (cddr svg))))
     (⊭ (contains? text "fill=\"#B6A394\""))
     (⊨ (every number? positions))
     (⊨ (apply < positions))
     (⊦= 'defs (car (cadr children)))
     (⊨ ((class-of? "ground") (caddr children)))
     (⊨ ((class-of? "overlay") (last children)))
     (⊨ (contains? text "class=\"lastmove\"")))
   ; reinforced dice drop in: the top two of hex 20
   (let1 (text (serialise (draw-board-svg fixture #f '() changes: '((20 . reinforced)) new-dice: '((20 . 2)))))
     (⊦= 2 (occurrences text "class=\"new\""))
     (⊨ (contains? text "class=\"hex p0 idle reinforced\"")))
   ; standalone, for a file
   (let1 (text (serialise (draw-board-svg fixture #f '() standalone: #t)))
     (⊨ (contains? text "<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\""))
     (⊨ (contains? text "width=\"928\" height=\"444\""))))

  ((test/dice-of-doom-app/page _)
   (let* ((g (fixture-game))
          (page (request g "nil")))
     (⊨ (string-prefix? "<!doctype html>" page))
     (⊦= 1 (occurrences page "<style>"))
     (⊨ (contains? page (string-append "<style>" dod-css "</style>")))
     (⊨ (contains? page "<title>Your turn · Dice of Doom</title>"))
     (⊨ (contains? page "<tr class=\"current\" aria-current=\"step\" style=\"--pc:var(--p0)\">"))
     (⊭ (contains? page "data-auto=\""))
     (⊭ (contains? page "location.replace"))
     (⊨ (contains? page "<span class=\"btn primary\" aria-disabled=\"true\">End turn</span>"))
     (⊨ (contains? page "<b>Your turn.</b>"))
     (⊨ (contains? page "No move yet"))
     (⊦= 10 (length (game-links page)))                ; the sources, on the board and in the list
     (⊦= '("5" "11" "16" "20" "22") (map (λ (l) (alist-ref 'chosen (link-params l))) (take (game-links page) 5)))
     (⊨ (contains? page "<nav class=\"moves\" aria-label=\"Moves\">"))
     (⊨ (contains? page "<b>16</b><span>5 dice</span>"))
     (⊨ (string-prefix? "/game.html?new=" (new-game-link page)))
     ; hex 11 chosen: the list holds its targets with their odds
     (let1 (page (begin (request g "11") (request g "nil" step: 99)))
       (⊨ (contains? page "Attack from hex 11</h2>"))
       (⊨ (contains? page "<b>17</b><span>2 dice</span><span class=\"odds\">78%</span>")))
     ; the computer to move: Continue, with the script
     (let* ((g (web-game-new! (game-tree fixture 2 0 #t)))
            (page (request g "nil" step: 99)))           ; a stale step: the page, no move
       (⊨ (contains? page "<title>c is playing · Dice of Doom</title>"))
       (⊨ (contains? page (conc "data-auto=\"" (*auto-advance-ms*) "\"")))
       (⊨ (contains? page dod-js))
       (⊨ (contains? page "<button class=\"btn\" type=\"button\" data-pause=\"\" hidden=\"\">Pause <kbd>Esc</kbd></button>"))
       (⊭ (contains? page "class=\"moves\""))
       (⊨ (contains? page (conc "href=\"/game.html?g=" (web-game-id g) "&amp;t=0&amp;chosen=nil\"")))
       (⊨ (contains? page "aria-current=\"step\" style=\"--pc:var(--p2)\""))
       (⊦= 0 (web-game-step g)))))

  ((test/dice-of-doom-app/new-game _)
   (let* ((before (hash-table-size web-games))
          (page (with-output-to-string (τ (dod-request-handler "game.html" '() '()))))
          (links (game-links page))
          (id (alist-ref 'g (link-params (car links)))))
     (⊦= (min (*max-games*) (add1 before)) (hash-table-size web-games))
     (⊨ (pair? links))
     (⊨ (every (λ (l) (equal? "0" (alist-ref 't (link-params l)))) links))
     (⊦= 16 (string-length id))
     (⊨ (every (λ (c) (true? (or (char-numeric? c) (memv c '(#\a #\b #\c #\d #\e #\f))))) (string->list id)))
     (⊨ (contains? page (conc "history.replaceState(null,'','/game.html?g=" id "&t=0&chosen=nil');")))
     (⊨ (every (λ (hex) (and (< (car hex) 4) (<= 1 (cadr hex) (*max-dice*)))) (vector->list (cadr (web-game-tree (web-game-lookup id))))))
     ; the root is the game too
     (⊨ (contains? (with-output-to-string (τ (dod-request-handler "" '() '()))) "<title>Your turn"))))

  ((test/dice-of-doom-app/protocol _)
   ; a stale step changes nothing
   (let1 (g (fixture-game))
     (request g "16" step: 0)
     (request g "16" step: 0)
     (⊦= 1 (web-game-step g))
     (⊦= 16 (web-game-from-tile g)))
   ; the chosen tile again cancels
   (let1 (g (fixture-game))
     (request g "16")
     (let1 (page (request g "16"))
       (⊭ (web-game-from-tile g))
       (⊦= 2 (web-game-step g))
       (⊨ (contains? page "Move cancelled."))))
   ; a hex of another player, a hex that is not a target: nothing changes
   (let1 (g (fixture-game))
     (⊨ (contains? (request g "13") "Hex 13 cannot attack."))
     (⊦= 0 (web-game-step g))
     (⊭ (web-game-from-tile g))
     (request g "16")
     (⊨ (contains? (request g "6") "Hex 6 cannot be attacked from hex 16."))
     (⊦= 1 (web-game-step g))
     (⊦= 16 (web-game-from-tile g))
     (request g "99")                                   ; not a hex: nil
     (⊦= 1 (web-game-step g)))
   ; no pass at the first move of a turn
   (let* ((g (fixture-game))
          (tree (web-game-tree g))
          (page (request g "pass")))
     (⊨ (eq? tree (web-game-tree g)))
     (⊦= 0 (web-game-step g))
     (⊨ (contains? page "You must attack at least once before ending your turn.")))
   ; the keys are lower-case symbols, as both servers give them: others are no params
   (let1 (g (fixture-game))
     (handle `((G . ,(web-game-id g)) (T . "0") (CHOSEN . "16")))
     (⊭ (web-game-from-tile g))
     (⊦= 0 (web-game-step g))))

  ((test/dice-of-doom-app/attack _)
   ; 16 (5 dice) attacks 21 (1 die): the rolls decide, as v4's pick-chance-branch with the same seed
   (for-each
     (λ (seed)
       (let* ((g (fixture-game))
              (tree (web-game-tree g))
              (move ((stream:find (λ (m) (equal? (car m) '(16 21)))) (caddr tree))))
         (request g "16")
         (set-pseudo-random-seed! (string->utf8 seed))
         (let* ((page (request g "21"))
                (board (cadr (web-game-tree g)))
                (entry (car (web-game-log g))))
           (set-pseudo-random-seed! (string->utf8 seed))
           (let1 (v4 (let1 (r #f) (with-output-to-string (τ (set! r (pick-chance-branch fixture move)))) r))
             (⊨ (eq? v4 (web-game-tree g))))
           (⊦= 'attack (car entry))
           (⊦= '(0 16 21 5 1) (take* (cdr entry) 5))
           (⊨ (<= 5 (list-ref entry 6) 30))
           (⊨ (<= 1 (list-ref entry 7) 6))
           (⊦= (> (list-ref entry 6) (list-ref entry 7)) (list-ref entry 8))
           (if (list-ref entry 8)
             (begin (⊦= '(0 4) (vector-ref board 21))
                    (⊦= '(0 1) (vector-ref board 16))
                    (⊨ (contains? page "— captured.")))
             (begin (⊦= '(2 1) (vector-ref board 21))
                    (⊦= '(0 1) (vector-ref board 16))
                    (⊨ (contains? page "— repelled."))))
           (⊭ (web-game-from-tile g))
           (⊦= 2 (web-game-step g))
           (⊨ (contains? page "You may now end your turn or attack again."))
           (⊨ (contains? page "class=\"lastmove\""))
           ; now the human may pass, and the dice of the largest cluster come
           (let1 (page (request g "pass"))
             (⊦= 1 (car (web-game-tree g)))
             (⊦= 'pass (car (car (web-game-log g))))
             (⊨ (contains? page "Your reinforcements have been placed."))
             (⊨ (contains? page "data-auto"))))))
     '("1-attack-seed" "2-attack-seed" "3-attack-seed" "4-attack-seed"))
   ; a 78% attack, 11 → 17 (3 dice against 2): over 40 seeds some are repelled, some capture, and
   ; a tie defends, as in the book
   (let1 (entries (map (λ (i)
                         (let1 (g (fixture-game))
                           (request g "11")
                           (set-pseudo-random-seed! (string->utf8 (conc i "-outcome-seed")))
                           (request g "17")
                           (car (web-game-log g))))
                       (iota 40)))
     (⊨ (true? (memq #t (map (λ (e) (list-ref e 8)) entries))))
     (⊨ (true? (memq #f (map (λ (e) (list-ref e 8)) entries))))
     (⊨ (every (λ (e) (eq? (list-ref e 8) (> (list-ref e 6) (list-ref e 7)))) entries))
     (⊨ (positive? (count (λ (e) (= (list-ref e 6) (list-ref e 7))) entries)))))

  ((test/dice-of-doom-app/computer _)
   (parameterize ((*ai-level* 2))
     (let* ((g (web-game-new! (game-tree fixture 3 0 #t)))
            (tree (web-game-tree g))
            (move (choose-computer-move tree)))
       (set-pseudo-random-seed! (string->utf8 "computer-seed"))
       (let* ((page (request g "nil"))
              (entry (car (web-game-log g))))
         (⊦= 1 (web-game-step g))
         (⊦= (list 'attack 3 (caar move) (cadar move)) (take* entry 4))
         (⊨ (contains? page "class=\"lastmove\""))
         (⊨ (contains? page (conc "<b>d</b> attacked " (caar move) " → " (cadar move) ": ")))
         (⊨ (eq? (web-game-tree g) (if (list-ref entry 8) (cadr move) (caddr move))))
         (⊦= 0 (hash-table-size game-tree/previous))))))     ; emptied after the move

  ((test/dice-of-doom-app/expired _)
   (let* ((before (hash-table-size web-games))
          (page (handle '((g . "0123456789abcdef") (t . "3") (chosen . "nil")))))
     (⊨ (contains? page "That game has expired, so a new one has started."))
     (⊨ (contains? page "history.replaceState"))
     (⊦= (min (*max-games*) (add1 before)) (hash-table-size web-games)))
   ; no game named: a new one, and no word of expiry
   (for-each (λ (params)
               (let1 (page (handle params))
                 (⊭ (contains? page "expired"))
                 (⊨ (contains? page "<b>Your turn.</b>"))
                 (⊨ (contains? page "history.replaceState"))))
             '(((chosen . "nil")) ((t . "0") (chosen . "0")) ((g . "") (t . "0") (chosen . "nil")))))

  ; New game is a link with the id of the game it starts: followed again, Back for example, it
  ; answers that same game as it is now
  ((test/dice-of-doom-app/new-game-link _)
   (let* ((link (new-game-link (handle '())))
          (params (link-params link))
          (id (alist-ref 'new params))
          (first (handle params))
          (g (web-game-lookup id)))
     (⊨ (web-game? g))
     (⊨ (contains? first (conc "history.replaceState(null,'','/game.html?g=" id "&t=0&chosen=nil');")))
     (let1 (src (car (legal-tiles (web-game-tree g) #f)))
       (handle (link-params (make-game-link g src)))
       (⊦= 1 (web-game-step g))
       (let1 (again (handle params))
         (⊨ (eq? g (web-game-lookup id)))
         (⊦= src (web-game-from-tile g))
         (⊨ (contains? again (conc "history.replaceState(null,'','/game.html?g=" id "&t=1&chosen=nil');"))))))
   ; an id that is not one of ours starts a game under an id of ours
   (let* ((before (hash-table-size web-games))
          (page (handle '((new . "<script>")))))
     (⊭ (contains? page "<script>\""))
     (⊭ (web-game-lookup "<script>"))
     (⊦= (min (*max-games*) (add1 before)) (hash-table-size web-games)))
   ; New game and Play again draw a new id on every page
   (let1 (g (fixture-game))
     (⊭ (equal? (new-game-link (request g "nil")) (new-game-link (request g "nil"))))))

  ((test/dice-of-doom-app/eviction _)
   (hash-table-clear! web-games)                          ; the games of the cases above go
   (parameterize ((*max-games* 16))
     (let1 (games (map (λ (_) (fixture-game)) (iota (add1 (*max-games*)))))
       (⊨ (<= (hash-table-size web-games) (*max-games*)))
       (⊭ (web-game-lookup (web-game-id (car games))))
       (⊨ (every (λ (g) (true? (web-game-lookup (web-game-id g)))) (cdr games))))
     ; the least recently used goes, not the oldest
     (let* ((games (map (λ (_) (fixture-game)) (iota (*max-games*)))))
       (web-game-lookup (web-game-id (car games)))
       (fixture-game)
       (⊨ (true? (web-game-lookup (web-game-id (car games)))))
       (⊭ (web-game-lookup (web-game-id (cadr games)))))
     ; a flood of bare requests, crawlers say, drops the games nobody moved in before a game being
     ; played, however long ago it was played
     (let1 (played (fixture-game))
       (request played "16")
       (⊦= 1 (web-game-step played))
       (for-each (λ (_) (handle '())) (iota (* 3 (*max-games*))))
       (⊨ (eq? played (web-game-lookup (web-game-id played))))
       (⊨ (contains? (request played "21") "You attacked 16 → 21")))
     ; when every game is played, the least recently used goes
     (let1 (games (map (λ (_) (let1 (g (fixture-game)) (request g "16") g)) (iota (*max-games*))))
       (fixture-game)
       (⊭ (web-game-lookup (web-game-id (car games))))
       (⊨ (true? (web-game-lookup (web-game-id (cadr games)))))))
   ; an idle game is gone, at the next request on it or when a game starts
   (let1 (g (fixture-game))
     (parameterize ((*game-idle-seconds* -1))
       (⊭ (web-game-lookup (web-game-id g)))
       (let1 (h (fixture-game))
         (fixture-game)
         (⊭ (hash-table-exists? web-games (web-game-id h)))
         (⊦= 1 (hash-table-size web-games))))))

  ; a deal on which the human has no attack is dealt again
  ((test/dice-of-doom-app/deal _)
   (let* ((dud (make-vector 25 '(1 3)))
          (_ (vector-set! dud 12 '(0 1)))
          (boards (list dud fixture))
          (tree (deal-game-tree (τ (let1 (b (car boards)) (set! boards (cdr boards)) b)))))
     (⊨ (stream:null? (caddr (game-tree dud 0 0 #t))))
     (⊨ (eq? fixture (cadr tree)))
     (⊦= 0 (car tree)))
   (⊨ (every (λ (seed)
               (set-pseudo-random-seed! (string->utf8 seed))
               (not (stream:null? (caddr (web-game-tree (web-game-new!))))))
             (map (λ (i) (conc i "-deal-seed")) (iota 20)))))

  ; the banner of the end: why it ended, then the winner, or the tie
  ((test/dice-of-doom-app/banner _)
   (let* ((board (vector-map* (λ (pos hex) (list (cond ((< pos 12) 1) ((< pos 24) 2) (else 0)) 2)) fixture))
          (text (serialise (web-announce-winner board 3))))
     (⊨ (contains? text "d has no hex left, so the game ends."))
     (⊨ (contains? text "The game is a tie between b and c."))
     (⊨ (contains? text "href=\"/game.html?new="))
     (⊨ (contains? text ">Play again</a>")))
   (let1 (text (serialise (web-announce-winner (vector-map* (λ (pos hex) (list (if (< pos 13) 1 2) 2)) fixture) 1)))
     (⊨ (contains? text "b has no attack left, so the game ends."))
     (⊨ (contains? text "The winner is <b>b</b> — you hold no hex.")))
   (let1 (text (serialise (web-announce-winner fixture 0)))
     (⊨ (contains? text "You have no attack left, so the game ends."))
     (⊨ (contains? text "The winner is <b>a</b> — you hold 7 of 25 hexes.")))
   (⊭ (contains? (serialise (web-announce-winner fixture)) "so the game ends"))
   (⊨ (contains? (serialise (web-announce-winner (vector-map* (λ (pos hex) (list 1 2)) fixture) 0))
                 "You have no hex left, so the game ends."))
   ; the page: the winner's row marked, the players out struck, with dashes
   (let* ((board (vector-map* (λ (pos hex) (if (eqv? (car hex) 3) '(1 1) hex)) fixture))
          (g (web-game-new! (list 3 board stream:empty)))
          (page (request g "nil")))
     (⊨ (contains? page "d has no hex left, so the game ends."))
     (⊨ (contains? page "<title>b wins · Dice of Doom</title>"))
     (⊨ (contains? page "<tr class=\"winner\" style=\"--pc:var(--p1)\">"))
     (⊨ (contains? page "<tr class=\"out\">"))
     (⊦= 3 (occurrences page "<td>–</td>"))))

  ; the AI under a budget too small for a single ply still moves, legally; the dice
  ((test/dice-of-doom-app/budget-and-dice _)
   (let1 (tree (game-tree fixture 3 0 #t))
     (for-each (λ (budget)
                 (let1 (move (choose-computer-move tree level: 3 budget: budget))
                   (⊨ (true? (memq move (stream:->list (caddr tree)))))))
               '(0 1 5 50)))
   (set-pseudo-random-seed! (string->utf8 "roll-dice-seed"))
   (⊨ (every (λ (n) (every (λ (_) (<= n (roll-dice n) (* 6 n))) (iota 50))) (iota 8 1))))

  ((test/dice-of-doom-app/unknown-path _)
   (⊦= "Sorry... I don't know that page." (with-output-to-string (τ (dod-request-handler "favicon.ico" '() '())))))

  ; whole games, the human playing random legal moves, the computer at level 1 to keep it short
  ((test/dice-of-doom-app/games _)
   (parameterize ((*ai-level* 1))
     (for-each
       (λ (seed)
         (set-pseudo-random-seed! (string->utf8 seed))
         (let1 (page (with-output-to-string (τ (dod-request-handler "game.html" '() '()))))
           (let loop ((page page) (n 0))
             (let1 (links (game-links page))
               (cond
                 ((contains? page "Play again")
                  (⊨ (or (contains? page "The winner is") (contains? page "The game is a tie between")))
                  (⊨ (or (contains? page " wins · Dice of Doom</title>") (contains? page " tie · Dice of Doom</title>"))))
                 ((> n 3000) (⊨ #f))
                 (else
                   (let* ((chosen (filter (λ (l) (not (string-contains l "chosen=nil"))) links))
                          (pass (filter (λ (l) (string-contains l "chosen=pass")) chosen))
                          (href (cond
                                  ((null? chosen) (car links))                                ; Continue
                                  ((and (pair? pass) (zero? (pseudo-random-integer 3))) (car pass))
                                  (else (list-ref chosen (pseudo-random-integer (length chosen)))))))
                     (loop (with-output-to-string (τ (dod-request-handler "game.html" '() (link-params href))))
                           (add1 n)))))))))
       '("1-whole-game" "2-whole-game"))))

  ; the game over HTTP, served by spiffy on a loopback port
  ((test/dice-of-doom-app/http _)
   (let* ((L (tcp-listen 0 10 "127.0.0.1"))
          (th (make-thread (τ (dod-serve listener: L)))))      ; keywords alone
     (thread-start! th)
     (dynamic-wind
       void
       (τ (parameterize ((tcp-read-timeout 20000))
            (receive (in out) (tcp-connect "127.0.0.1" (tcp-listener-port L))
              (let* ((get (λ (path)
                            (display (conc "GET " path " HTTP/1.1\r\nHost: localhost\r\n\r\n") out)
                            (flush-output out)
                            (let* ((status (read-line in))
                                   (header (let loop ((acc '()))
                                             (let1 (line (read-line in))
                                               (if (equal? line "") acc (loop (cons line acc))))))
                                   (n (string->number (cadr (string-split (find-header header "Content-Length") " ")))))
                              (list status header (utf8->string (read-bytevector n in))))))
                     (first (get "/game.html"))
                     (link (car (game-links (caddr first))))
                     (second (get link)))
                (⊦= "HTTP/1.1 200 OK" (car first))
                (⊨ (true? (find-header (cadr first) "Content-Type: text/html; charset=utf-8")))
                (⊨ (true? (find-header (cadr first) "Cache-Control: no-store")))
                (⊨ (contains? (caddr second) "select it again to cancel"))
                ; New game, followed twice, is one game
                (let* ((new (new-game-link (caddr second)))
                       (a (get new))
                       (b (get new)))
                  (⊦= (game-links (caddr a)) (game-links (caddr b))))
                (⊦= "Sorry... I don't know that page." (caddr (get "/nothing.html")))
                (close-input-port in)
                (close-output-port out)))))
       (τ (thread-terminate! th) (tcp-close L)))))

  )

(define (true? x) (and x #t))

(define (find-header lines prefix) (let1 (l (filter (λ (l) (string-prefix? prefix l)) lines)) (and (pair? l) (car l))))

(define (take* l n) (if (or (zero? n) (null? l)) '() (cons (car l) (take* (cdr l) (sub1 n)))))

(define (vector-map* f v) (list->vector (map f (iota (vector-length v)) (vector->list v))))

(unittest/✓ dice-of-doom-app-suite)
