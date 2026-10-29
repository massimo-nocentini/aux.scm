;;; Tests of (aux games dice-of-doom v3), the third Dice of Doom of "Land of Lisp" (Conrad Barski,
;;; chapter 19), a port of http://landoflisp.com/dice_of_doom_v3.lisp: the SVG board and the web
;;; handler `dod-request-handler`.
;;;
;;; The last case serves the handler with (aux land-of-lisp webserver), the port of webserver.lisp,
;;; which is distributed under the GNU General Public License, version 2 only; so is this file, as a
;;; program that links it. The license text is in COPYING.GPL-2.0. The module under test is MIT.
;;; SPDX-License-Identifier: GPL-2.0-only
;;;
;;; The pages in games.dice-of-doom/v3-*.html come from an oracle, a literal transliteration of the
;;; Common Lisp files (v1, lazy.lisp, v2, v3) to csi top-level code, where `define` is late bound as
;;; `defun` is: a scripted session on the 2x2 board of the book, a request per step. The 2x2 cases
;;; run inside `with-board-size`, which clears the memo of `neighbors`, keyed on `pos` alone as
;;; upstream; the game state is in the cells `*cur-game-tree*` and `*from-tile*`.

(import scheme
        (chicken base)
        (chicken io)
        (chicken port)
        (chicken tcp)
        (chicken random)
        (chicken bytevector)
        (chicken condition)
        (only srfi-1 iota count)
        (only srfi-13 string-prefix? string-suffix? string-contains)
        srfi-18
        srfi-69
        (aux unittest)
        (aux base)
        (aux stream sicp)
        (aux games dice-of-doom v3)
        (only (aux land-of-lisp webserver) serve))

(define (with-board-size n thunk)        ; neighbors is memoized on pos alone (upstream), so clear it
  (parameterize ((*board-size* n) (*board-hexnum* (* n n)))
    (dynamic-wind (τ (hash-table-clear! neighbors/previous)) thunk (τ (hash-table-clear! neighbors/previous)))))

(define (read-golden name) (with-input-from-file (string-append "games.dice-of-doom/" name) (τ (read-string #f))))

(define book-2x2 #((0 1) (1 1) (0 2) (1 1)))

; the page of one request of the browser, `chosen` = #f for a request without it.
(define (request chosen)
  (with-output-to-string
    (τ (dod-request-handler "game.html" '() (if chosen (list (cons 'chosen chosen)) '())))))

(define (occurrences sub s)
  (let loop ((i 0) (n 0))
    (let1 (j (string-contains s sub i))
      (if j (loop (add1 j) (add1 n)) n))))

(define (contains? s sub) (and (string-contains s sub) #t))

(define page-prefix "<!doctype html><center>Welcome to DICE OF DOOM!<br></br>")

(define svg-tag "<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" height=\"500\" width=\"900\">")

(define-suite dice-of-doom-v3-suite

  ((test/dice-of-doom-v3/parameters _)
   (⊦= 2 (*num-players*))
   (⊦= 5 (*board-size*))
   (⊦= 4 (*ai-level*))
   (⊦= '((255 63 63) (63 63 255)) (*die-colors*))
   (⊦= '(900 500 64 3 40 0.05) (list (*board-width*) (*board-height*) (*board-scale*) (*top-offset*) (*dice-scale*) (*dot-size*))))

  ; v3's `svg` takes a size, `tag` and the other macros come from (aux land-of-lisp svg)
  ((test/dice-of-doom-v3/svg _)
   (⊦= (string-append svg-tag "<g></g></svg>") (with-output-to-string (τ (svg 900 500 (tag g ())))))
   (⊦= '(1 (2)) (split '(1 2) (list head tail) 'none))
   (⊦= '((a . b)) (pairs '(a b)))
   (let1 (die (with-output-to-string (τ (draw-die-svg 50 50 '(255 0 0)))))
     (⊨ (string-prefix? "<polygon points=\"50,10 26.0,20.0 50,30.0 74.0,20.0 \" style=\"fill:rgb(255,40,40);stroke:rgb(155,0,0)\"></polygon>" die))
     (⊦= 15 (occurrences "<polygon" die)))                        ; 3 faces and 12 pips
   (⊦= (+ 2 (* 2 15)) (occurrences "<polygon" (with-output-to-string (τ (draw-tile-svg 0 0 0 '(0 2) 128 192 '(255 63 63) #f))))))

  ((test/dice-of-doom-v3/links _)
   (⊦= "/game.html?chosen=3" (make-game-link 3))
   (⊦= "/game.html?chosen=NIL" (make-game-link #f))
   (⊦= "/game.html?chosen=PASS" (make-game-link 'pass))
   (⊦= 3 (parse-chosen "3"))
   (⊦= 'pass (parse-chosen "PASS"))
   (⊦= 'pass (parse-chosen "pass"))
   (⊦= #f (parse-chosen "NIL"))
   (⊦= #f (parse-chosen "zz"))
   (⊦= #f (parse-chosen "#.(exit)"))
   (⊦= #f (parse-chosen #t))                         ; spiffy's value of a key without =
   (⊦= "Sorry... I don't know that page." (with-output-to-string (τ (dod-request-handler "favicon.ico" '() '())))))

  ((test/dice-of-doom-v3/cells _)
   (*from-tile* 7)
   (⊦= 7 (thread-join! (thread-start! (make-thread *from-tile*))))      ; shared by every thread
   (thread-join! (thread-start! (make-thread (τ (*from-tile* 8)))))
   (⊦= 8 (*from-tile*))
   (*from-tile* #f))

  ; the scripted session of the oracle: from 2, attack 3, pass, and the computer has no move.
  ((test/dice-of-doom-v3/session _)
   (with-board-size 2
     (τ
       (*cur-game-tree* (game-tree book-2x2 0 0 #t))
       (*from-tile* #f)
       (for-each (λ (chosen golden polygons links from-tile)
                   (let1 (page (request chosen))
                     (⊦= (read-golden golden) page)
                     (⊨ (string-prefix? page-prefix page))
                     (⊨ (string-suffix? "</svg></center>" page))
                     (⊨ (contains? page svg-tag))
                     (⊦= polygons (occurrences "<polygon" page))           ; 15 per die and 2 per hex
                     (⊦= links (occurrences "<a xlink:href" page))
                     (⊦= from-tile (*from-tile*))))
                 '("NIL" "2" "3" "PASS" "NIL")
                 '("v3-1-nil.html" "v3-2-from2.html" "v3-3-attack3.html" "v3-4-pass.html" "v3-5-over.html")
                 '(83 83 68 68 68)
                 '(1 1 0 0 0)
                 '(#f 2 #f #f #f))
       (⊨ (contains? (read-golden "v3-1-nil.html") "<a xlink:href=\"/game.html?chosen=2\">"))
       (⊨ (contains? (read-golden "v3-2-from2.html") "<a xlink:href=\"/game.html?chosen=3\">"))
       (⊨ (contains? (read-golden "v3-5-over.html") "\nThe winner is a<a href=\"game.html\"> play again</a>")))))

  ((test/dice-of-doom-v3/human-quirks _)
   (with-board-size 2
     (τ
       (let1 (t (game-tree book-2x2 0 0 #t))
         ; choosing the source again cancels the move
         (*cur-game-tree* t) (*from-tile* #f)
         (request "2")
         (⊨ (contains? (request "2") "Move cancelled."))
         (⊦= #f (*from-tile*))
         (⊨ (eq? t (*cur-game-tree*)))
         ; PASS where there is no passing move takes the first attack
         (request "PASS")
         (⊨ (eq? (*cur-game-tree*) (cadr (stream:car (caddr t)))))
         ; an illegal destination: the tree becomes #f and the page fails, as upstream
         (*cur-game-tree* t) (*from-tile* 2)
         (⊦raises (exn) (request "0"))
         (⊦= #f (*cur-game-tree*))
         (⊦= #f (*from-tile*))
         ; ... and the next request starts a new game
         (⊨ (string-prefix? page-prefix (request "2")))
         (⊦= 4 (vector-length (cadr (*cur-game-tree*))))
         (⊭ (eq? t (*cur-game-tree*)))))))

  ((test/dice-of-doom-v3/computer _)
   (with-board-size 2
     (τ
       (let1 (t (game-tree #((1 2) (0 1) (1 1) (0 1)) 1 0 #t))
         (*cur-game-tree* t) (*from-tile* #f)
         (let1 (page (request "NIL"))
           (⊨ (contains? page "The computer has moved. <script>window.setTimeout('window.location=\"game.html?chosen=NIL\"',5000)</script>"))
           (⊦= 0 (occurrences "<a xlink:href" page)))                  ; no link: it needs JavaScript
         (⊨ (eq? (*cur-game-tree*) (handle-computer t)))))))

  ; a request without `chosen` starts a new game, at the module's size
  ((test/dice-of-doom-v3/initialize _)
   (set-pseudo-random-seed! (string->utf8 "web-initialize"))
   (*cur-game-tree* (game-tree book-2x2 0 0 #t))
   (*from-tile* 3)
   (let1 (page (request #f))
     (⊨ (string-prefix? page-prefix page))
     (⊦= #f (*from-tile*))
     (⊦= 0 (car (*cur-game-tree*)))
     (⊦= 25 (vector-length (cadr (*cur-game-tree*))))
     (⊦= (+ (* 2 25) (* 15 (apply + (map cadr (vector->list (cadr (*cur-game-tree*)))))))
         (occurrences "<polygon" page))))

  ; the page of the first step, served by the web server of the book
  ((test/dice-of-doom-v3/served _)
   (with-board-size 2
     (τ
       (*cur-game-tree* (game-tree book-2x2 0 0 #t))
       (*from-tile* #f)
       (let* ((L (tcp-listen 0))
              (server (thread-start! (make-thread (τ (serve dod-request-handler listener: L requests: 1)))))
              (reply (receive (in out) (tcp-connect "127.0.0.1" (tcp-listener-port L))
                       (display "GET /game.html?chosen=NIL HTTP/1.1\r\nHost: localhost\r\n\r\n" out)
                       (flush-output out)
                       (begin1 (read-string #f in) (close-input-port in) (close-output-port out))))
              (body (read-golden "v3-1-nil.html")))
         (thread-join! server)          ; with a timeout, CHICKEN 6's thread-join! waits it all out on a dead thread
         (⊨ (string-prefix? "HTTP/1.1 200 OK\r\n" reply))
         (⊨ (contains? reply "\r\nContent-Length: 10939\r\n"))
         (⊨ (string-suffix? (string-append "\r\n\r\n" body) reply))))))

  )

(unittest/✓ dice-of-doom-v3-suite)
