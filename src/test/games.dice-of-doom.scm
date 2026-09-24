
;
; The dice-of-doom-test.scm of the old `on-scheme` repository, ported to (aux unittest).
;
; The original seeded the generator once, at top level, with a string, and then every assertion
; consumed the random sequence in turn. In CHICKEN 6 the seed must be a bytevector and the random
; sequence differs from CHICKEN 5's, so:
;   - `seeded-boards` re-seeds with (string->utf8 "gkfifgousrwbtrm") and replays the original
;     sequence of `gen-board` calls, so every case sees the same boards whatever the case order;
;   - the expected values that depend on the generator are regenerated under CHICKEN 6 and checked
;     by hand (the replies of the original were never run: the `loop` egg was missing);
;   - each seed-dependent assertion is doubled by an assertion on a literal board.
; The board printer no longer leaks newlines to the current output port, hence the transcript of
; the AI-vs-AI game shows each board under its "Board:" line.
;
; The original 5x5 AI-vs-AI demo at top level is not a test and is dropped; `test-exit` becomes
; `unittest/✓`.

(import scheme
        (chicken base)
        (chicken port)
        (chicken random)
        (chicken fixnum)
        (chicken bytevector)
        (only (scheme base) open-output-string get-output-string)
        (only srfi-1 circular-list every)
        (aux unittest)
        (aux base)
        (aux commons)
        (aux stream sicp)
        (aux games dice-of-doom))

; the boards made by the `gen-board` calls of the original test, in the original order.
(define seeded-boards
  (λ ()
    (set-pseudo-random-seed! (string->utf8 "gkfifgousrwbtrm"))
    (let* ((b₁ (gen-board 2 '(A B) 4))
           (b₂ (gen-board 2 '(A B) 4))
           (b₃ (gen-board 2 '(A B) 4))
           (b₄ (gen-board 2 '(A B) 4))
           (b₅ (gen-board 2 '(A B) 4))
           (b₆ (gen-board 2 '(A B) 4))
           (b₇ (gen-board 5 '(A B) 4))
           (b₈ (gen-board 4 '(A B) 3)))
      (list b₁ b₂ b₃ b₄ b₅ b₆ b₇ b₈))))

(define board/5x5
  (make-board #((A 4) (B 1) (B 4) (A 3) (A 4)
                (B 1) (A 2) (A 1) (A 2) (A 1)
                (A 1) (B 3) (B 4) (A 1) (A 3)
                (A 2) (B 3) (B 2) (A 2) (B 4)
                (A 1) (A 4) (A 1) (B 3) (B 2))
              5))

(define board/4x4
  (make-board #((B 2) (A 1) (A 3) (B 3)
                (A 2) (A 1) (A 2) (B 3)
                (B 1) (B 1) (B 3) (B 1)
                (B 3) (A 3) (A 3) (A 1))
              4))

(define board/2x2 (make-board #((A 1) (B 1) (A 2) (B 1)) 2))

(define transcript/4x4
  "\nCurrent player: A\nBoard:\n        B-2 A-1 A-3 B-3 \n      A-2 A-1 A-2 B-3 \n    B-1 B-1 B-3 B-1 \n  B-3 A-3 A-3 A-1 \nCurrent player: A\nBoard:\n        B-2 A-1 A-3 B-3 \n      A-2 A-1 A-1 B-3 \n    B-1 B-1 B-3 A-1 \n  B-3 A-3 A-3 A-1 \nCurrent player: A\nBoard:\n        B-2 A-1 A-3 B-3 \n      A-2 A-1 A-1 B-3 \n    B-1 A-2 B-3 A-1 \n  B-3 A-1 A-3 A-1 \nCurrent player: A\nBoard:\n        B-2 A-1 A-3 B-3 \n      A-2 A-1 A-1 B-3 \n    A-1 A-1 B-3 A-1 \n  B-3 A-1 A-3 A-1 \nCurrent player: B\nBoard:\n        B-2 A-2 A-4 B-3 \n      A-2 A-1 A-1 B-3 \n    A-1 A-1 B-3 A-1 \n  B-3 A-1 A-3 A-1 \nThe winner is A")

(define-suite dice-of-doom-suite

  ; the original `test-group "initial"`, 12 `test` forms.
  ((test/dice-of-doom/initial _)
   (let1 (boards (seeded-boards))

     ; 'A and 'B play on 2x2 board starting with at most 4 dices on each cell.
     ; was "\n    A-1 B-1 \n  B-3 A-3 " under CHICKEN 5.
     (⊦= "\n    B-2 B-2 \n  B-2 B-2 " (to-string (list-ref boards 0)))
     ; was #((B 2) (B 2) (B 4) (B 1)) under CHICKEN 5.
     (⊦= #((A 4) (A 3) (A 3) (B 2)) (board-cells (list-ref boards 1)))

     (⊦= '(2 1 3) (stream:->list (neighbors 0 (list-ref boards 2))))
     (⊦= '(3 0)   (stream:->list (neighbors 1 (list-ref boards 3))))
     (⊦= '(0 3)   (stream:->list (neighbors 2 (list-ref boards 4))))
     (⊦= '(1 0 2) (stream:->list (neighbors 3 (list-ref boards 5))))

     (⊦= #((A 3) (A 1) (B 3) (A 2))
         (board-cells
           (board-attack
             (make-board #((A 3) (A 3) (B 3) (B 1)) 2)
             'A 1 3 3)))

     (⊦= #((A 2) (B 3) (A 3) (B 1))
         (board-cells (add-new-dice (make-board #((A 1) (B 3) (A 2) (B 1)) 2) 'A 2)))

     ; the original compared through `map/tree`, which cannot force the moves streams;
     ; `game-tree->sexp` does. The expected value is the original one, unchanged.
     (⊦= '(A "\n    A-1 B-1 \n  A-2 B-1 "
             (((cell 2 attacks 3) (A "\n    A-1 B-1 \n  A-1 A-1 "
                                     ((pass (B "\n    A-1 B-1 \n  A-1 A-1 "
                                               ())))))))
         (game-tree->sexp
           (game-tree
             (make-board #((A 1) (B 1) (A 2) (B 1)) 2)
             (apply circular-list '(A B)) 0 #t)))

     (⊦= '(A) (winners (make-board #((A 1) (B 1) (A 1) (A 1)) 2)))

     ; regenerated under CHICKEN 6.
     (⊦= "\n          A-4 B-1 B-4 A-3 A-4 \n        B-1 A-2 A-1 A-2 A-1 \n      A-1 B-3 B-4 A-1 A-3 \n    A-2 B-3 B-2 A-2 B-4 \n  A-1 A-4 A-1 B-3 B-2 "
         (with-output-to-string (τ (display (list-ref boards 6)))))

     ; slow in principle (a lazy 4x4 tree searched with α-β at depth 4), fast in practice.
     ; Regenerated under CHICKEN 6, with the printer fix; checked by hand: A attacks 6→11,
     ; 13→9 and 9→8, then passes and reinforces cells 1 and 2 with (sub1 3) = 2 dice; the game
     ; stops there because the next tree is the depth-0 horizon of the first search (as in the
     ; original, see the module header).
     (⊦= transcript/4x4
         (with-output-to-string
           (τ (let ((board (list-ref boards 7))
                    (players (apply circular-list '(A B))))
                ((computer-vs-computer 4) (game-tree board players 0 #t))))))))

  ; the random boards above, as literals, so the printer and the game do not depend on the RNG.
  ((test/dice-of-doom/literal-boards _)
   (let1 (boards (seeded-boards))
     (⊦= (board-cells board/5x5) (board-cells (list-ref boards 6)))
     (⊦= (board-cells board/4x4) (board-cells (list-ref boards 7))))
   (⊦= "\n          A-4 B-1 B-4 A-3 A-4 \n        B-1 A-2 A-1 A-2 A-1 \n      A-1 B-3 B-4 A-1 A-3 \n    A-2 B-3 B-2 A-2 B-4 \n  A-1 A-4 A-1 B-3 B-2 "
       (with-output-to-string (τ (display board/5x5))))
   (⊦= '(2 1 3) (stream:->list (neighbors 0 board/2x2)))
   (⊦= '(3 0)   (stream:->list (neighbors 1 board/2x2)))
   (⊦= '(0 3)   (stream:->list (neighbors 2 board/2x2)))
   (⊦= '(1 0 2) (stream:->list (neighbors 3 board/2x2)))
   (call+stdout
     (τ ((computer-vs-computer 4) (game-tree board/4x4 (apply circular-list '(A B)) 0 #t)))
     (λ (result transcript)
       (⊦= '(A) result)
       (⊦= transcript/4x4 transcript))))

  ((test/dice-of-doom/gen-board/shape _)
   (let1 (b (gen-board 5 '(A B) 4))
     (⊦= 5 (board-size b))
     (⊦= 25 (board-hexnum b))
     (⊦= 25 (vector-length (board-cells b)))
     (⊨ (every (λ (hex) (and (memq (car hex) '(A B)) (<= 1 (cadr hex) 4) #t))
               (vector->list (board-cells b))))))

  ; nothing is printed on the current output port by the board printer.
  ((test/dice-of-doom/printer _)
   (⊦= "\nCurrent player: A\nBoard:\n    A-1 B-1 \n  A-2 B-1 "
       (with-output-to-string
         (τ (display (make-gametree 'A board/2x2 '())))))
   (⊦= "[\n    A-1 B-1 \n  A-2 B-1 ]"
       (with-output-to-string
         (τ (let1 (s (open-output-string))
              (display board/2x2 s)
              (display "[")
              (display (get-output-string s))
              (display "]"))))))

  ; the game tree is memoized on (board rotation spare-dice first-move): two fresh circular
  ; lists of players give the same tree, and the lookup terminates.
  ((test/dice-of-doom/game-tree/tabled _)
   (let ((t₁ (game-tree board/2x2 (apply circular-list '(A B)) 0 #t))
         (t₂ (game-tree board/2x2 (list/circular 'A 'B) 0 #t)))
     (⊨ (eq? t₁ t₂)))
   (⊦= '(A B C) (players-rotation (circular-list 'A 'B 'C)))
   (⊦= '(B C A) (players-rotation (cdr (circular-list 'A 'B 'C)))))

  ; regression: the memo key keeps the whole turn order, so the same board and first player with
  ; a different roster, or a different order of 3 players, give different trees whose pass goes to
  ; the right player (the original keyed on the whole players list).
  ((test/dice-of-doom/game-tree/turn-order _)
   (let* ((b (make-board #((A 1) (B 1) (A 2) (C 1)) 2))
          ; with first-move #f, the first move of the root is the pass.
          (next-after-pass (λ (t) (car (cadr (car (stream:->list (caddr t)))))))
          (t/ABC (game-tree b (circular-list 'A 'B 'C) 0 #f))
          (t/ACB (game-tree b (circular-list 'A 'C 'B) 0 #f)))
     (⊨ (not (eq? t/ABC t/ACB)))
     (⊦= 'B (next-after-pass t/ABC))
     (⊦= 'C (next-after-pass t/ACB)))
   (let ((t/AB (game-tree board/2x2 (circular-list 'A 'B) 0 #t))
         (t/AC (game-tree board/2x2 (circular-list 'A 'C) 0 #t)))
     (⊨ (not (eq? t/AB t/AC)))
     ; the root has only the attack (first move), whose tree starts with the pass.
     (⊦= 'B (car (cadr (car (stream:->list (caddr (cadr (car (stream:->list (caddr t/AB))))))))))
     (⊦= 'C (car (cadr (car (stream:->list (caddr (cadr (car (stream:->list (caddr t/AC))))))))))))

  ((test/dice-of-doom/winners/tie _)
   (⊦= '(A B) (winners (make-board #((A 1) (B 1) (B 1) (A 1)) 2)))
   (⊦= "\nThe game is a tie between (A B)"
       (with-output-to-string (τ (announce-winner (make-board #((A 1) (B 1) (B 1) (A 1)) 2))))))

  ((test/dice-of-doom/heuristics _)
   (⊦= '(#f #f #f #t) (map (λ (pos) (threatened pos board/2x2)) '(0 1 2 3)))
   (⊦= 2 (score-board board/2x2 'A))
   (⊦= 1 (score-board board/2x2 'B)))

  ((test/dice-of-doom/ratings _)
   (let1 (t (game-tree board/2x2 (apply circular-list '(A B)) 0 #t))
     (⊦= '(5) (get-ratings t 'A))
     (⊦= 5 ((rate-position 'A) t))
     (⊦= 5 (rate-position/αβ t 'A most-positive-fixnum most-negative-fixnum))
     (⊦= '(5) (get-ratings/αβ-max t 'A most-positive-fixnum most-negative-fixnum))
     (⊦= '(A "\n    A-1 B-1 \n  A-1 A-1 " ((pass (B "\n    A-1 B-1 \n  A-1 A-1 " ()))))
         (game-tree->sexp (handle-computer t)))))

  ((test/dice-of-doom/limit-tree-depth _)
   (let1 (t (game-tree board/2x2 (apply circular-list '(A B)) 0 #t))
     (⊨ (stream:null? (caddr ((limit-tree-depth 0) t))))
     (⊦= '(A "\n    A-1 B-1 \n  A-2 B-1 " (((cell 2 attacks 3) (A "\n    A-1 B-1 \n  A-1 A-1 " ()))))
         (game-tree->sexp ((limit-tree-depth 1) t)))
     (⊦= (game-tree->sexp t) (game-tree->sexp ((limit-tree-depth 5) t)))))

  )

(unittest/✓ dice-of-doom-suite)
