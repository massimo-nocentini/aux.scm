;
; Tests of (aux games dice-of-doom v1), the first Dice of Doom of "Land of Lisp" (Conrad Barski,
; chapter 15), a port of http://landoflisp.com/dice_of_doom_v1.lisp.
;
; The expected values come from the examples printed in the book (the 2x2 game tree, `neighbors`,
; `board-attack`, `add-new-dice`) and from an oracle, a literal transliteration of the Common Lisp
; file to csi top-level code, where `define` is late bound as `defun` is; the transcript in
; games.dice-of-doom/v1-transcript-3x3.txt is its output. The module's board size is 3, as at
; the end of the file; the 2x2 cases run inside `with-board-size`, which clears the memo of
; `neighbors`, keyed on `pos` alone as upstream. Each version of Dice of Doom has its own test
; file, run by its own csi, because loading a version sets the parameters of the earlier ones.

(import scheme
        (chicken base)
        (chicken io)
        (chicken port)
        (chicken random)
        (chicken bytevector)
        (only (scheme base) vector-copy)
        (only srfi-1 iota)
        srfi-69
        (aux unittest)
        (aux base)
        (aux games dice-of-doom v1))

(define (with-board-size n thunk)        ; neighbors is memoized on pos alone (upstream), so clear it
  (parameterize ((*board-size* n) (*board-hexnum* (* n n)))
    (dynamic-wind (τ (hash-table-clear! neighbors/previous)) thunk (τ (hash-table-clear! neighbors/previous)))))

(define (read-golden name) (with-input-from-file (string-append "games.dice-of-doom/" name) (τ (read-string #f))))

(define (quietly thunk) (let1 (r #f) (with-output-to-string (τ (set! r (thunk)))) r))   ; its value, no output

(define ones (apply string-append (map (K "1 ") (iota 200))))   ; the human always answers 1

(define book-2x2 #((0 1) (1 1) (0 2) (1 1)))
(define b3 #((0 3) (1 2) (0 1) (1 1) (0 2) (1 3) (0 1) (1 1) (0 2)))

(define-suite dice-of-doom-v1-suite

  ((test/dice-of-doom-v1/parameters _)
   (⊦= 2 (*num-players*))
   (⊦= 3 (*max-dice*))
   (⊦= 3 (*board-size*))                   ; the second defparameter of the file
   (⊦= 9 (*board-hexnum*)))

  ; chapter 15 of the book, on its 2x2 board
  ((test/dice-of-doom-v1/book-2x2 _)
   (with-board-size 2
     (τ
       (⊦= '(0 #((0 1) (1 1) (0 2) (1 1)) (((2 3) (0 #((0 1) (1 1) (0 1) (0 1)) ((#f (1 #((0 1) (1 1) (0 1) (0 1)) ())))))))
           (game-tree book-2x2 0 0 #t))
       (⊦= '((2 1 3) (3 0) (0 3) (1 0 2)) (map neighbors '(0 1 2 3)))
       (⊦= #((0 3) (0 1) (1 3) (0 2)) (board-attack #((0 3) (0 3) (1 3) (1 1)) 0 1 3 3))
       (⊦= #((0 2) (1 3) (0 3) (1 1)) (add-new-dice #((0 1) (1 3) (0 2) (1 1)) 0 2))
       (⊦= "\n    a-1 b-1 \n  a-2 b-1 " (with-output-to-string (τ (draw-board book-2x2))))
       (⊦= "\ncurrent player = a\n    a-1 b-1 \n  a-2 b-1 \nchoose your move:\n1. 2 -> 3\n\ncurrent player = a\n    a-1 b-1 \n  a-1 a-1 \nchoose your move:\n1. end turn\n\ncurrent player = b\n    a-1 b-1 \n  a-1 a-1 \nThe winner is a"
           (with-output-to-string (τ (with-input-from-string "1 1" (τ (play-vs-human (game-tree book-2x2 0 0 #t))))))))))

  ((test/dice-of-doom-v1/winners _)
   (with-board-size 2
     (τ
       (⊦= '(0 1) (winners book-2x2))
       (⊦= '(0 1) (winners #((1 1) (0 1) (0 2) (1 1))))   ; CL's remove-duplicates order, not (1 0)
       (⊦= '(0 1) (winners #((0 2) (0 1) (1 2) (1 1))))
       (⊦= '(1 0) (winners #((0 1) (1 1) (1 1) (0 1))))
       (⊦= "\nThe game is a tie between (a b)" (with-output-to-string (τ (announce-winner #((1 1) (0 1) (0 2) (1 1))))))
       (⊦= 1 (rate-position (game-tree book-2x2 0 0 #t) 0))
       (⊦= 0 (rate-position (game-tree book-2x2 0 0 #t) 1))
       ; a 2-2 tie with no moves rates 1/2, exact: eqv?, since ⊦= would accept 0.5 too
       (⊨ (eqv? 1/2 (rate-position (game-tree #((0 1) (1 1) (1 1) (0 1)) 0 0 #t) 0)))))
   (⊦= '(0) (winners #((0 1) (0 1) (1 1) (1 1) (0 1) (1 1) (0 1) (1 1) (0 1))))
   (⊦= "\nThe winner is a" (with-output-to-string (τ (announce-winner #((0 1) (0 1) (1 1) (1 1) (0 1) (1 1) (0 1) (1 1) (0 1)))))))

  ((test/dice-of-doom-v1/board-3x3 _)
   (⊦= '((3 1 4) (4 0 2 5) (5 1) (0 6 4 7) (1 7 0 3 5 8) (2 8 1 4) (3 7) (4 3 6 8) (5 4 7)) (map neighbors (iota 9)))
   (let1 (t3 (game-tree b3 0 0 #t))
     (⊦= '((0 3) (0 1) (4 7) (4 3) (8 7)) (map car (caddr t3)))
     (⊦= '(0 1 1 1 1) (get-ratings t3 0))
     (⊦= '(1 0 0 0 0) (get-ratings t3 1))
     (⊦= 1 (rate-position t3 0))
     (⊨ (eq? (handle-computer t3) (cadr (list-ref (caddr t3) 1))))    ; the first best move, (0 1)
     (⊦= #((0 3) (1 2) (0 1) (1 1) (0 2) (1 1) (0 1) (1 1) (1 2)) (cadr (handle-computer (game-tree b3 1 0 #t))))
     (⊦= "\ncurrent player = a\n      a-3 b-2 a-1 \n    b-1 a-2 b-3 \n  a-1 b-1 a-2 " (with-output-to-string (τ (print-info t3))))
     (⊦= "\nchoose your move:\n1. 0 -> 3\n2. 0 -> 1\n3. 4 -> 7\n4. 4 -> 3\n5. 8 -> 7\n"
         (with-output-to-string (τ (with-input-from-string "3" (τ (handle-human t3))))))
     (⊨ (eq? (cadr (list-ref (caddr t3) 2)) (quietly (τ (with-input-from-string "3" (τ (handle-human t3)))))))))

  ((test/dice-of-doom-v1/memo _)
   (let1 (t3 (game-tree b3 0 0 #t))
     (⊨ (eq? t3 (game-tree (vector-copy b3) 0 0 #t)))        ; an `equal?` key gives the same tree
     (⊭ (eq? t3 (game-tree b3 1 0 #t)))
     (rate-position t3 0)
     (⊨ (hash-table? (hash-table-ref/default rate-position/previous 0 #f)))
     (⊨ (hash-table-exists? (hash-table-ref rate-position/previous 0) t3)))
   ; boards that differ only in their last cells do not share a bucket
   (⊭ (= (game-tree/key-hash (list #((0 1) (0 1) (0 1) (0 1) (0 1) (0 1) (0 1) (0 1) (0 1)) 0 0 #t))
         (game-tree/key-hash (list #((0 1) (0 1) (0 1) (0 1) (0 1) (0 1) (0 1) (0 1) (1 1)) 0 0 #t))))
   (⊦= (game-tree/key-hash (list (vector-copy b3) 0 0 #t)) (game-tree/key-hash (list b3 0 0 #t)))
   (⊨ (< (game-tree/key-hash (list b3 0 0 #t) 7) 7))
   ; the memo of `neighbors` is keyed on `pos` alone, as upstream: stale after a size change
   (neighbors 0)
   (⊦= '(3 1 4) (parameterize ((*board-size* 2) (*board-hexnum* 4)) (neighbors 0)))
   (⊦= '(2 1 3) (with-board-size 2 (τ (neighbors 0)))))

  ((test/dice-of-doom-v1/transcript _)
   (⊦= (read-golden "v1-transcript-3x3.txt")
       (with-output-to-string (τ (with-input-from-string ones (τ (play-vs-computer (game-tree b3 0 0 #t))))))))

  ((test/dice-of-doom-v1/gen-board _)
   (set-pseudo-random-seed! (string->utf8 "land-of-lisp"))
   (⊦= #((0 1) (0 2) (0 1) (0 1) (0 2) (0 2) (0 2) (0 2) (0 3)) (gen-board))
   (⊦= #((1 3) (1 3) (1 2) (0 3) (0 3) (1 2) (1 1) (0 2) (0 3)) (gen-board))
   (⊦= 4 (vector-length (with-board-size 2 gen-board)))
   (⊦raises (exn) (board-array '((0 1) (1 1)))))     ; as CL's make-array, of the wrong length

  )

(unittest/✓ dice-of-doom-v1-suite)
