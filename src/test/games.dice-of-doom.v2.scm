;
; Tests of (aux games dice-of-doom v2), the second Dice of Doom of "Land of Lisp" (Conrad Barski,
; chapter 18), a port of http://landoflisp.com/dice_of_doom_v2.lisp: lazy game trees, the
; heuristic `score-board` and α-β pruning, on a 5x5 board.
;
; The expected values come from an oracle, a literal transliteration of the Common Lisp files
; (v1, lazy.lisp, v2) to csi top-level code, where `define` is late bound as `defun` is; the
; transcript in games.dice-of-doom/v2-transcript-3x3.txt is its output. The 2x2 and 3x3 cases run
; inside `with-board-size`, which clears the memo of `neighbors`, keyed on `pos` alone as upstream.

(import scheme
        (chicken base)
        (chicken io)
        (chicken port)
        (chicken fixnum)
        (chicken random)
        (chicken bytevector)
        (only (scheme base) vector-copy)
        (only srfi-1 iota every list-index)
        srfi-69
        (aux unittest)
        (aux base)
        (aux stream sicp)
        (aux games dice-of-doom v2))

(define (with-board-size n thunk)        ; neighbors is memoized on pos alone (upstream), so clear it
  (parameterize ((*board-size* n) (*board-hexnum* (* n n)))
    (dynamic-wind (τ (hash-table-clear! neighbors/previous)) thunk (τ (hash-table-clear! neighbors/previous)))))

(define (read-golden name) (with-input-from-file (string-append "games.dice-of-doom/" name) (τ (read-string #f))))

(define (quietly thunk) (let1 (r #f) (with-output-to-string (τ (set! r (thunk)))) r))   ; its value, no output

(define ones (apply string-append (map (K "1 ") (iota 200))))   ; the human always answers 1

(define book-2x2 #((0 1) (1 1) (0 2) (1 1)))
(define b3 #((0 3) (1 2) (0 1) (1 1) (0 2) (1 3) (0 1) (1 1) (0 2)))
(define b5 #((0 3) (1 2) (0 1) (1 1) (0 2) (1 3) (0 1) (1 1) (0 2) (1 2) (0 3) (0 1) (1 2) (1 3) (0 2)
             (1 1) (0 3) (1 2) (0 1) (1 3) (0 2) (1 1) (1 3) (0 3) (1 2)))

(define actions (λ (tree) (map car (stream:->list (caddr tree)))))

; upstream's first, minimax `handle-computer`, dead code in the image after the α-β one.
(define (handle-computer/minimax tree)
  (let1 (ratings (get-ratings (limit-tree-depth tree (*ai-level*)) (car tree)))
    (cadr ((stream:ref (list-index (λ (r) (= r (apply max ratings))) ratings)) (caddr tree)))))

(define-suite dice-of-doom-v2-suite

  ((test/dice-of-doom-v2/parameters _)
   (⊦= 2 (*num-players*))
   (⊦= 3 (*max-dice*))
   (⊦= 5 (*board-size*))                   ; the last defparameter of v2.lisp
   (⊦= 25 (*board-hexnum*))
   (⊦= 4 (*ai-level*)))

  ((test/dice-of-doom-v2/score-board _)
   (⊦= 5 (score-board b5 0))
   (⊦= 7 (score-board b5 1))
   (⊦= '(0 1 1 1 0 0 1 1 1 0 0 1 0 0 1 1 0 1 1 0 0 1 0 0 1) (map (λ (p) (if (threatened p b5) 1 0)) (iota 25)))
   (⊦= '((5 1 6) (9 3) (7 17 6 11 13 18) (19 18 23)) (map neighbors '(0 4 12 24))))

  ((test/dice-of-doom-v2/lazy-tree _)
   (hash-table-clear! game-tree/previous)
   (let1 (t5 (game-tree b5 0 0 #t))
     (⊦= 1 (hash-table-size game-tree/previous))           ; no move is computed yet
     (stream:car (caddr t5))
     (⊦= 2 (hash-table-size game-tree/previous))           ; only the first one
     (⊦= '((0 1) (4 3) (8 3) (8 7) (10 15) (16 21) (16 15) (16 17) (20 15) (20 21) (23 17) (23 24)) (actions t5))
     (⊨ (every (λ (move) (stream:null? (caddr (cadr move))))
               (stream:->list (caddr (limit-tree-depth t5 1)))))
     (⊨ (stream:null? (caddr (limit-tree-depth t5 0))))
     (⊨ (eq? t5 (game-tree (vector-copy b5) 0 0 #t)))))

  ; the 2x2 tree of the book is the same as v1's, once forced
  ((test/dice-of-doom-v2/book-2x2 _)
   (with-board-size 2
     (τ
       (let1 (t (game-tree book-2x2 0 0 #t))
         (⊦= '((2 3)) (actions t))
         (⊦= '(#f) (actions (cadr (stream:car (caddr t)))))
         (⊦= #((0 1) (1 1) (0 1) (0 1)) (cadr (cadr (stream:car (caddr (cadr (stream:car (caddr t)))))))))
       (⊦= "\ncurrent player = a\n    a-1 b-1 \n  a-2 b-1 \nchoose your move:\n1. 2 -> 3\n\ncurrent player = a\n    a-1 b-1 \n  a-1 a-1 \nchoose your move:\n1. end turn\n\ncurrent player = b\n    a-1 b-1 \n  a-1 a-1 \nThe winner is a"
           (with-output-to-string (τ (with-input-from-string "1 1" (τ (play-vs-human (game-tree book-2x2 0 0 #t))))))))))

  ((test/dice-of-doom-v2/ratings _)
   (let1 (t5 (game-tree b5 0 0 #t))
     (⊦= '(15 14 15 14 14 14 14 14 15 14 14 14) (get-ratings (limit-tree-depth t5 4) 0))
     (⊦= '(15 14 15 14 14 14 14 14 15 14 14 14)
         (ab-get-ratings-max (limit-tree-depth t5 4) 0 most-positive-fixnum most-negative-fixnum))
     (⊦= 15 (rate-position (limit-tree-depth t5 4) 0))
     (⊦= 15 (ab-rate-position (limit-tree-depth t5 4) 0 most-positive-fixnum most-negative-fixnum))
     ; with the whole window α-β answers the minimax value, at every depth and for both players
     (⊨ (every (λ (d)
                 (every (λ (p)
                          (let1 (t (limit-tree-depth t5 d))
                            (= (rate-position t p) (ab-rate-position t p most-positive-fixnum most-negative-fixnum))))
                        '(0 1)))
               '(0 1 2 3)))
     ; the cuts: a rating at or above the upper limit ends a max node, one at or below the lower
     ; limit ends a min node
     (⊦= '(15) (ab-get-ratings-max (limit-tree-depth t5 4) 0 15 most-negative-fixnum))
     (let1 (c (cadr (stream:car (caddr (limit-tree-depth t5 2)))))     ; player 1 to move
       (⊦= '(8 10 9 10 10 10 9 10 9 11 10 9 9) (get-ratings c 0))
       (⊦= '(8 10 9 10 10 10 9 10 9 11 10 9 9) (ab-get-ratings-min c 0 most-positive-fixnum most-negative-fixnum))
       (⊦= '(8) (ab-get-ratings-min c 0 most-positive-fixnum 14)))
     (⊨ (eq? (handle-computer t5) (cadr (stream:car (caddr t5)))))   ; the first move (0 1)
     (⊦= #((0 1) (0 2) (0 1) (1 1) (0 2) (1 3) (0 1) (1 1) (0 2) (1 2) (0 3) (0 1) (1 2) (1 3) (0 2) (1 1) (0 3) (1 2) (0 1) (1 3) (0 2) (1 1) (1 3) (0 3) (1 2))
         (cadr (handle-computer t5))))
   ; `handle-computer` prunes: from an empty memo, the trees it builds to choose the move at
   ; depth 4, against those the minimax one builds (the oracle's counts of distinct keys)
   (let1 (built (λ (choose)
                  (hash-table-clear! game-tree/previous)
                  (choose (game-tree (vector-copy b5) 0 0 #t))
                  (hash-table-size game-tree/previous)))
     (⊦= 801 (built handle-computer))
     (⊦= 3137 (built handle-computer/minimax))))

  ; why it must hold: with exact scores and the whole window at the root, the lower limit passed
  ; to child k is the maximum of the exact values of the children before it, so the first child
  ; reaching the maximum is rated exactly and every child before it rates below; the trees are
  ; compared, not the ratings, since a child that fails low may answer a bound.
  ((test/dice-of-doom-v2/alpha-beta-agrees-with-minimax _)
   (set-pseudo-random-seed! (string->utf8 "alpha-beta"))
   (⊦= 30 (let loop ((i 0) (n 0))
            (if (= i 30)
              n
              (let* ((b (gen-board))
                     (t (game-tree b (modulo i 2) 0 #t)))
                (loop (add1 i) (if (parameterize ((*ai-level* 3)) (eq? (handle-computer/minimax t) (handle-computer t)))
                                 (add1 n)
                                 n)))))))

  ((test/dice-of-doom-v2/human _)
   (with-board-size 3
     (τ
       (let1 (t (game-tree b3 0 0 #t))
         (⊦= "\nchoose your move:\n1. 0 -> 3\n2. 0 -> 1\n3. 4 -> 7\n4. 4 -> 3\n5. 8 -> 7\n"
             (with-output-to-string (τ (with-input-from-string "2" (τ (handle-human t))))))
         (⊨ (eq? (cadr ((stream:ref 1) (caddr t))) (quietly (τ (with-input-from-string "2" (τ (handle-human t)))))))
         ; after an attack the passing move comes first
         (⊦= "\nchoose your move:\n1. end turn\n2. 4 -> 7\n3. 4 -> 3\n4. 8 -> 7\n"
             (with-output-to-string (τ (with-input-from-string "1" (τ (handle-human (cadr ((stream:ref 1) (caddr t)))))))))))))

  ((test/dice-of-doom-v2/transcript _)
   (with-board-size 3
     (τ (⊦= (read-golden "v2-transcript-3x3.txt")
            (with-output-to-string (τ (with-input-from-string ones (τ (play-vs-computer (game-tree b3 0 0 #t))))))))))

  )

(unittest/✓ dice-of-doom-v2-suite)
