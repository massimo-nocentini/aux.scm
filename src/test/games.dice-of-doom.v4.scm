;
; Tests of (aux games dice-of-doom v4), the fourth Dice of Doom of "Land of Lisp" (Conrad Barski,
; chapter 20), a port of http://landoflisp.com/dice_of_doom_v4.lisp: four players, chance nodes and
; reinforcements by the largest cluster, on a 5x5 board.
;
; The expected values come from an oracle, a literal transliteration of the Common Lisp files (v1,
; lazy.lisp, v2, v3, v4) to csi top-level code, where `define` is late bound as `defun` is; the
; transcripts in games.dice-of-doom/v4-transcript-3x3.txt and v4-human-3x3.txt are its output.
; The ratings are flonums, compared within 1e-6 by ⊦=. The 3x3 cases run inside `with-board-size`,
; which clears the memo of `neighbors`, keyed on `pos` alone as upstream. The seeded cases depend on
; CHICKEN's generator.

(import scheme
        (chicken base)
        (chicken io)
        (chicken port)
        (chicken fixnum)
        (chicken random)
        (chicken bytevector)
        (only (scheme base) vector-copy)
        (only srfi-1 iota every count)
        (only srfi-13 string-contains)
        srfi-69
        (aux unittest)
        (aux base)
        (aux stream sicp)
        (aux games dice-of-doom v4))

(define (with-board-size n thunk)        ; neighbors is memoized on pos alone (upstream), so clear it
  (parameterize ((*board-size* n) (*board-hexnum* (* n n)))
    (dynamic-wind (τ (hash-table-clear! neighbors/previous)) thunk (τ (hash-table-clear! neighbors/previous)))))

(define (read-golden name) (with-input-from-file (string-append "games.dice-of-doom/" name) (τ (read-string #f))))

(define (quietly thunk) (let1 (r #f) (with-output-to-string (τ (set! r (thunk)))) r))   ; its value, no output

(define (contains? s sub) (and (string-contains s sub) #t))

; the value of `thunk` and what it prints.
(define (value+output thunk)
  (let* ((v #f)
         (out (with-output-to-string (τ (set! v (thunk))))))
    (values v out)))

(define ones (apply string-append (map (K "1 ") (iota 200))))   ; the human always answers 1

(define b5/4p #((0 3) (1 2) (2 1) (3 1) (0 2) (1 3) (0 1) (1 1) (0 2) (1 2) (0 3) (0 1) (2 2) (3 3) (0 2)
                (1 1) (0 3) (1 2) (2 1) (3 5) (0 2) (1 1) (1 3) (0 3) (1 2)))
(define b3/4p #((0 3) (1 2) (2 1) (3 2) (0 2) (1 3) (2 2) (3 1) (0 1)))

(define actions (λ (tree) (map car (stream:->list (caddr tree)))))

(define-suite dice-of-doom-v4-suite

  ((test/dice-of-doom-v4/parameters _)
   (⊦= 4 (*num-players*))
   (⊦= 5 (*max-dice*))
   (⊦= 2 (*ai-level*))
   (⊦= 4 (length (*die-colors*)))
   (⊦= 5 (*board-size*))
   (⊦= 25 (*board-hexnum*))
   (⊦= 0.84 (vector-ref (vector-ref (*dice-odds*) 0) 0)))

  ; `tag` and v3's `svg` come through the re-exports, the v2 functions too
  ((test/dice-of-doom-v4/layering _)
   (⊦= "<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" height=\"2\" width=\"1\"><g></g></svg>"
       (with-output-to-string (τ (svg 1 2 (tag g ())))))
   (⊦= 2 (with-board-size 2 (τ (score-board #((0 1) (1 1) (0 2) (1 1)) 0))))
   (⊦= "/game.html?chosen=PASS" (make-game-link 'pass))
   (hash-table-clear! game-tree/previous)
   (game-tree b5/4p 0 0 #t)
   (⊦= 1 (hash-table-size game-tree/previous)))

  ((test/dice-of-doom-v4/clusters _)
   (⊦= '(10 16 11 6 0) (get-connected b5/4p 0 0))
   (⊦= '() (get-connected b5/4p 0 1))                       ; hex 1 is not player 0's
   (⊦= '(5 4 2 2) (map (λ (p) (largest-cluster-size b5/4p p)) '(0 1 2 3)))
   ; 5 dice, to hexes 0, 4, 6, 8, 10; the `spare-dice` argument is ignored
   (⊦= #((0 4) (1 2) (2 1) (3 1) (0 3) (1 3) (0 2) (1 1) (0 3) (1 2) (0 4) (0 1) (2 2) (3 3) (0 2) (1 1) (0 3) (1 2) (2 1) (3 5) (0 2) (1 1) (1 3) (0 3) (1 2))
       (add-new-dice b5/4p 0 0))
   (⊦= (add-new-dice b5/4p 0 0) (add-new-dice b5/4p 0 17))
   (⊦= #((3 1) (1 2) (2 1) (3 1) (0 2) (1 3) (0 1) (1 1) (0 2) (1 2) (0 3) (0 1) (2 2) (3 3) (0 2) (1 1) (0 3) (1 2) (2 1) (3 5) (0 2) (1 1) (1 3) (0 3) (1 2))
       (board-attack-fail b5/4p 3 0 1 3))
   (⊦= (let1 (b (vector-copy b5/4p)) (vector-set! b 0 '(0 1)) b) (board-attack-fail b5/4p 0 0 1 3)))

  ((test/dice-of-doom-v4/tree _)
   (let1 (t (game-tree b5/4p 0 0 #t))
     (⊦= '((0 5) (0 1) (4 9) (4 3) (8 3) (8 13) (8 2) (8 7) (8 9) (10 5) (10 15) (14 9) (14 19) (14 13) (16 21) (16 15) (16 17) (16 22) (20 15) (20 21) (23 18) (23 17) (23 22) (23 24))
         (actions t))
     (⊨ (every (λ (move) (= 3 (length move))) (stream:->list (caddr t))))
     (let1 (move (stream:car (caddr t)))                        ; (0 5): 3 dice against 3
       (⊦= #((0 1) (1 2) (2 1) (3 1) (0 2) (0 2) (0 1) (1 1) (0 2) (1 2) (0 3) (0 1) (2 2) (3 3) (0 2) (1 1) (0 3) (1 2) (2 1) (3 5) (0 2) (1 1) (1 3) (0 3) (1 2))
           (cadr (cadr move)))
       (⊦= (board-attack-fail b5/4p 0 0 5 3) (cadr (caddr move))))
     ; after an attack the passing move comes first
     (⊦= #f (car (stream:car (caddr (cadr (stream:car (caddr t)))))))
     ; limit-tree-depth cuts both trees of an attack
     (⊨ (every (λ (move) (and (= 3 (length move)) (stream:null? (caddr (cadr move))) (stream:null? (caddr (caddr move)))))
               (stream:->list (caddr (limit-tree-depth t 1)))))))

  ((test/dice-of-doom-v4/ratings _)
   (let1 (t (game-tree b5/4p 0 0 #t))
     (⊦= '(1.35 2.34 1.76 1.68 2.68 1.3 2.68 2.68 2.32 1.8 2.91 2.32 1.02 1.45 1.94 2.91 1.56 1.35 3.52 2.68 1.94 1.56 1.35 1.56)
         (get-ratings (limit-tree-depth t 1) 0))
     (⊦= '(4.086 4.86 4.28 4.6032 5.2 3.82 5.2 5.2 4.84 4.32 4.6152 4.84 3.54 3.97 4.46 4.6152 4.08 3.87 5.2368 4.59 4.46 4.08 3.87 4.08)
         (get-ratings (limit-tree-depth t 2) 0))
     (⊦= 5.2368 (rate-position (limit-tree-depth t 2) 0)))
   (with-board-size 3
     (τ
       (let1 (t (game-tree b3/4p 0 0 #t))
         (⊦= '((0 3) (0 1) (4 1) (4 7) (4 3) (4 5)) (actions t))
         (⊦= '(1.34 1.34 0.5632 1.02 0.22 -0.093) (get-ratings (limit-tree-depth t 2) 0))
         (⊦= '(3 2 1 2) (map (λ (p) (largest-cluster-size b3/4p p)) '(0 1 2 3)))
         (⊦= '(8 4 0) (get-connected b3/4p 0 0))))))

  ((test/dice-of-doom-v4/chance _)
   (set-pseudo-random-seed! (string->utf8 "dice-of-doom"))
   (receive (v out) (value+output (τ (roll-dice 3)))
     (⊦= 14 v)
     (⊦= "\nOn 3 dice rolled 14. " out))
   (with-board-size 3
     (τ
       (let* ((t (game-tree b3/4p 0 0 #t))
              (move (stream:car (caddr t))))                       ; (0 3)
         (set-pseudo-random-seed! (string->utf8 "pick-chance"))
         (receive (v out) (value+output (τ (pick-chance-branch b3/4p move)))
           (⊨ (eq? v (cadr move)))                                  ; 12 against 8: the attack succeeds
           (⊦= "\nOn 3 dice rolled 12. \nOn 2 dice rolled 8. " out))
         (set-pseudo-random-seed! (string->utf8 "attack-1"))
         (receive (v out) (value+output (τ (pick-chance-branch b3/4p move)))
           (⊨ (eq? v (caddr move)))                                 ; 5 against 8: it fails
           (⊦= "\nOn 3 dice rolled 5. \nOn 2 dice rolled 8. " out))
         (receive (v out) (value+output (τ (pick-chance-branch b3/4p (list #f t))))
           (⊨ (eq? v t))
           (⊦= "" out))
         ; the attacker rolls first and must roll more: a tie is a failure
         (let1 (seeds (map (λ (i) (string->utf8 (string-append (number->string i) "-tie-seed"))) (iota 60)))
           (⊨ (every (λ (seed)
                       (set-pseudo-random-seed! seed)
                       (let* ((a (quietly (τ (roll-dice 2))))
                              (d (quietly (τ (roll-dice 1)))))
                         (set-pseudo-random-seed! seed)
                         (eq? (> a d) (quietly (τ (roll-against 2 1))))))
                     seeds))
           (⊨ (positive? (count (λ (seed)
                                  (set-pseudo-random-seed! seed)
                                  (let* ((a (quietly (τ (roll-dice 1))))
                                         (d (quietly (τ (roll-dice 1)))))
                                    (set-pseudo-random-seed! seed)
                                    (and (= a d) (not (quietly (τ (roll-against 1 1)))))))
                                seeds))))))))

  ((test/dice-of-doom-v4/players _)
   (with-board-size 3
     (τ
       (let* ((t (game-tree b3/4p 0 0 #t))
              (move ((stream:ref 1) (caddr t))))                    ; (0 1)
         (set-pseudo-random-seed! (string->utf8 "pick-chance"))
         (receive (v out) (value+output (τ (with-input-from-string "2" (τ (handle-human t)))))
           (⊨ (eq? v (cadr move)))                                   ; 12 beats 8: the attack succeeds
           (⊨ (contains? out "choose your move:\n1. 0 -> 3\n2. 0 -> 1\n3. 4 -> 1\n"))
           (⊨ (contains? out "\nOn 3 dice rolled 12. \nOn 2 dice rolled 8. ")))
         (receive (v out) (value+output (τ (handle-computer t)))
           (⊨ (let1 (m (stream:car (caddr t)))                       ; the first of the best, (0 3)
                (or (eq? v (cadr m)) (eq? v (caddr m)))))
           (⊨ (contains? out "On 3 dice rolled")))))))

  ; v4's own `play-vs-human`, whose `handle-human` rolls the dice, against the oracle's output
  ((test/dice-of-doom-v4/play-vs-human _)
   (with-board-size 3
     (τ
       (set-pseudo-random-seed! (string->utf8 "play-vs-human!"))
       (let1 (out (with-output-to-string (τ (with-input-from-string ones (τ (play-vs-human (game-tree b3/4p 0 0 #t)))))))
         (⊨ (contains? out "rolled"))
         (⊦= (read-golden "v4-human-3x3.txt") out)))))

  ((test/dice-of-doom-v4/transcript _)
   (with-board-size 3
     (τ
       (set-pseudo-random-seed! (string->utf8 "transcript"))
       (⊦= (read-golden "v4-transcript-3x3.txt")
           (with-output-to-string (τ (with-input-from-string ones (τ (play-vs-computer (game-tree b3/4p 0 0 #t))))))))))

  ((test/dice-of-doom-v4/web _)
   (let1 (t (game-tree b5/4p 0 0 #t))
     ; kept quirk: on the web every human attack succeeds, nothing is rolled
     (*cur-game-tree* t)
     (*from-tile* 0)
     (let1 (page (with-output-to-string (τ (dod-request-handler "game.html" '() '((chosen . "1"))))))
       (⊨ (eq? (*cur-game-tree*) (cadr ((stream:ref 1) (caddr t)))))
       (⊭ (contains? page "rolled"))
       (⊨ (contains? page "You may now <a href=\"/game.html?chosen=PASS\">pass</a> or make another move:")))
     ; the computer rolls its dice through v4's handle-computer, into the page
     (*cur-game-tree* (game-tree b5/4p 1 0 #t))
     (*from-tile* #f)
     (let1 (page (with-output-to-string (τ (dod-request-handler "game.html" '() '((chosen . "NIL"))))))
       (⊨ (contains? page "rolled"))
       (⊨ (contains? page "The computer has moved. ")))
     ; v4's colors: player 2 is green, darker on the far rows, as in v3
     (⊨ (contains? (with-output-to-string (τ (draw-board-svg b5/4p #f '()))) "style=\"fill:rgb(0,180,0);stroke:rgb(0,80,0)\""))
     ; a new game is a v4 game
     (set-pseudo-random-seed! (string->utf8 "web-initialize"))
     (with-output-to-string (τ (dod-request-handler "game.html" '() '())))
     (⊨ (every (λ (hex) (and (< (car hex) 4) (<= 1 (cadr hex) 5))) (vector->list (cadr (*cur-game-tree*)))))
     (⊨ (hash-table-exists? game-tree/previous (list (cadr (*cur-game-tree*)) 0 0 #t)))))

  )

(unittest/✓ dice-of-doom-v4-suite)
