; Tests of (aux search) on the classic problems: route finding in Romania and the 8-puzzle (AIMA,
; 4th ed., ch. 3), the water jugs, missionaries and cannibals, n-queens, and a small graph whose
; heuristic is admissible but inconsistent. The expected numbers come from independent reference
; implementations of the same algorithms (same successor order, same tie breaking). The largest
; cases, the hardest 8-puzzles and the unsolvable one, take a few seconds.

(import scheme
        (chicken base)
        (chicken fixnum)
        (chicken random)
        (chicken bytevector)
        (only srfi-1 iota last any find)
        srfi-69
        (aux unittest)
        (aux base)
        (aux stream sicp)
        (aux search))

;;; ==================== PROBLEMS ====================

; Romania: the adjacency of each city, in the order the successors follow.
(define romania
  '((Arad (Zerind 75) (Sibiu 140) (Timisoara 118))
    (Zerind (Arad 75) (Oradea 71))
    (Sibiu (Arad 140) (Oradea 151) (Fagaras 99) (RimnicuVilcea 80))
    (Timisoara (Arad 118) (Lugoj 111))
    (Oradea (Zerind 71) (Sibiu 151))
    (Lugoj (Timisoara 111) (Mehadia 70))
    (Mehadia (Lugoj 70) (Drobeta 75))
    (Drobeta (Mehadia 75) (Craiova 120))
    (Craiova (Drobeta 120) (RimnicuVilcea 146) (Pitesti 138))
    (RimnicuVilcea (Craiova 146) (Sibiu 80) (Pitesti 97))
    (Pitesti (Craiova 138) (RimnicuVilcea 97) (Bucharest 101))
    (Fagaras (Sibiu 99) (Bucharest 211))
    (Bucharest (Fagaras 211) (Pitesti 101) (Giurgiu 90) (Urziceni 85))
    (Giurgiu (Bucharest 90))
    (Urziceni (Bucharest 85) (Hirsova 98) (Vaslui 142))
    (Hirsova (Urziceni 98) (Eforie 86))
    (Eforie (Hirsova 86))
    (Vaslui (Urziceni 142) (Iasi 92))
    (Iasi (Vaslui 92) (Neamt 87))
    (Neamt (Iasi 87))))

; straight-line distances to Bucharest
(define romania/h
  '((Arad . 366) (Bucharest . 0) (Craiova . 160) (Drobeta . 242) (Eforie . 161) (Fagaras . 176)
    (Giurgiu . 77) (Hirsova . 151) (Iasi . 226) (Lugoj . 244) (Mehadia . 241) (Neamt . 234)
    (Oradea . 380) (Pitesti . 100) (RimnicuVilcea . 193) (Sibiu . 253) (Timisoara . 329)
    (Urziceni . 80) (Vaslui . 199) (Zerind . 374)))

(define (romania/successors city)
  (map (λ (e) (list (cons 'go (car e)) (car e) (cadr e))) (cdr (assq city romania))))

(define (romania-problem #!key (goal? (λ (c) (eq? c 'Bucharest))))
  (make-problem start: 'Arad successors: romania/successors goal?: goal? test: eq?
                heuristic: (λ (c) (cdr (assq c romania/h)))))

; the 8-puzzle: a state is a 9-character string, #\0 is the blank; a step moves the blank.
(define puzzle/goal "123456780")

(define (puzzle/blank s) (let loop ((i 0)) (if (char=? (string-ref s i) #\0) i (loop (fx+ i 1)))))

(define (puzzle/swap s i j)
  (let ((t (string-copy s)))
    (string-set! t i (string-ref s j))
    (string-set! t j (string-ref s i))
    t))

(define (puzzle/successors s)
  (let* ((z (puzzle/blank s)) (row (fx/ z 3)) (col (fxmod z 3)))
    (let* ((steps '())
           (steps (if (fx< col 2) (cons (list 'right (puzzle/swap s z (fx+ z 1))) steps) steps))
           (steps (if (fx> col 0) (cons (list 'left (puzzle/swap s z (fx- z 1))) steps) steps))
           (steps (if (fx< row 2) (cons (list 'down (puzzle/swap s z (fx+ z 3))) steps) steps))
           (steps (if (fx> row 0) (cons (list 'up (puzzle/swap s z (fx- z 3))) steps) steps)))
      steps)))

(define (puzzle/inverse a) (case a ((up) 'down) ((down) 'up) ((left) 'right) ((right) 'left)))

; the steps (action t) such that t --action--> s
(define (puzzle/predecessors s)
  (map (λ (step) (list (puzzle/inverse (car step)) (cadr step))) (puzzle/successors s)))

(define (puzzle/manhattan s)
  (let loop ((i 0) (sum 0))
    (if (fx= i 9)
      sum
      (let ((v (fx- (char->integer (string-ref s i)) (char->integer #\0))))
        (loop (fx+ i 1)
              (if (fx= v 0)
                sum
                (let ((g (fx- v 1)))
                  (fx+ sum (fx+ (abs (fx- (fx/ i 3) (fx/ g 3))) (abs (fx- (fxmod i 3) (fxmod g 3))))))))))))

(define (puzzle start)
  (make-problem start: start successors: puzzle/successors goal: puzzle/goal predecessors: puzzle/predecessors
                test: string=? heuristic: puzzle/manhattan))

; water jugs of 3 and 5 liters, until one holds 4.
(define (jugs/successors s)
  (let* ((a (car s)) (b (cdr s))
         (ab (min a (- 5 b))) (ba (min b (- 3 a)))
         (steps `(((fill 0) (3 . ,b)) ((fill 1) (,a . 5)) ((empty 0) (0 . ,b)) ((empty 1) (,a . 0))
                  ((pour 0 1) (,(- a ab) . ,(+ b ab))) ((pour 1 0) (,(+ a ba) . ,(- b ba))))))
    (let loop ((steps steps))
      (cond
        ((null? steps) '())
        ((equal? (cadar steps) s) (loop (cdr steps)))
        (else (cons (car steps) (loop (cdr steps))))))))

(define jugs (make-problem start: '(0 . 0) successors: jugs/successors goal?: (λ (s) (or (= (car s) 4) (= (cdr s) 4)))))

; missionaries and cannibals: (m c boat) on the start bank, boat 1 when it is there.
(define (mc/legal? m c)
  (and (<= 0 m 3) (<= 0 c 3)
       (or (= m 0) (>= m c))
       (or (= (- 3 m) 0) (>= (- 3 m) (- 3 c)))))

(define (mc/successors s)
  (let ((m (car s)) (c (cadr s)) (boat (caddr s)))
    (let loop ((loads '((1 0) (2 0) (0 1) (0 2) (1 1))))
      (cond
        ((null? loads) '())
        (else (let* ((dm (caar loads)) (dc (cadar loads))
                     (m′ (if (= boat 1) (- m dm) (+ m dm)))
                     (c′ (if (= boat 1) (- c dc) (+ c dc))))
                (if (mc/legal? m′ c′)
                  (cons (list (car loads) (list m′ c′ (- 1 boat))) (loop (cdr loads)))
                  (loop (cdr loads)))))))))

(define (mc #!key (goal? (λ (s) (equal? s '(0 0 0)))))
  (make-problem start: '(3 3 1) successors: mc/successors goal?: goal?))

; n-queens: the columns placed so far, the last row first.
(define (queens n)
  (define (safe? col placed)
    (let loop ((placed placed) (k 1))
      (or (null? placed)
          (and (not (= col (car placed)))
               (not (= (abs (- col (car placed))) k))
               (loop (cdr placed) (add1 k))))))
  (make-problem start: '()
                successors: (λ (placed)
                              (if (= (length placed) n)
                                '()
                                (let loop ((col (sub1 n)) (steps '()))
                                  (cond
                                    ((< col 0) steps)
                                    ((safe? col placed) (loop (sub1 col) (cons (list col (cons col placed)) steps)))
                                    (else (loop (sub1 col) steps))))))
                goal?: (λ (placed) (= (length placed) n))))

; a directed graph with an admissible, inconsistent heuristic: h(A) = 4 > c(A,C) + h(C) = 1.
(define inconsistent
  (make-problem start: 'S
                successors: (λ (s) (case s
                                     ((S) '((→A A 1) (→B B 1)))
                                     ((A) '((→C C 1)))
                                     ((B) '((→C C 3)))
                                     ((C) '((→G G 3)))
                                     (else '())))
                goal: 'G test: eq?
                heuristic: (λ (s) (case s ((A) 4) (else 0)))))

;;; ==================== RANDOM GRAPHS ====================

; a random directed graph on the vertices 0..n−1, as a vector of adjacency lists ((j cost) ...):
; each edge is present with probability `percent`/100, with a cost in 1..9 (or 1, if `unit?`).
(define (random-graph n percent unit?)
  (list->vector
    (map (λ (i)
           (let loop ((j (sub1 n)) (edges '()))
             (cond
               ((< j 0) edges)
               ((and (not (= i j)) (< (pseudo-random-integer 100) percent))
                (loop (sub1 j) (cons (list j (if unit? 1 (add1 (pseudo-random-integer 9)))) edges)))
               (else (loop (sub1 j) edges)))))
         (iota n))))

(define (reversed graph)
  (let ((r (make-vector (vector-length graph) '())))
    (do ((i (sub1 (vector-length graph)) (sub1 i))) ((< i 0) r)
      (for-each (λ (e) (vector-set! r (car e) (cons (list i (cadr e)) (vector-ref r (car e)))))
                (vector-ref graph i)))))

(define ((graph/steps graph) i) (map (λ (e) (list (cons '→ (car e)) (car e) (cadr e))) (vector-ref graph i)))

; the cost from every vertex to `goal`, by uniform-cost search on the reversed graph: each state is
; popped once, at its optimal cost, in nondecreasing order
(define (distances-to graph goal)
  (let ((table (make-hash-table =)))
    (stream:->list ((stream:map (λ (n) (hash-table-set! table (node-state n) (node-cost n))))
                    (uniform-cost-search (make-problem start: goal successors: (graph/steps (reversed graph)) goal?: (K #t)))))
    table))

(define (graph-problem graph h)
  (let ((goal (sub1 (vector-length graph))))
    (make-problem start: 0 successors: (graph/steps graph) goal: goal test: =
                  predecessors: (λ (i) (map (λ (e) (list (cons '→ i) (car e))) (vector-ref (reversed graph) i)))
                  heuristic: h)))

;;; ==================== HELPERS ====================

; the state that the actions of n lead to from the start, replaying the successors of p
(define (replay p n)
  (let loop ((s (problem-start p)) (actions (node-actions n)))
    (if (null? actions)
      s
      (let ((step (find (λ (step) (equal? (step-action step) (car actions))) ((problem-successors p) s))))
        (loop (step-state step) (cdr actions))))))

(define (⊦solution p n)
  (⊨ (node? n))
  (⊨ ((problem-goal? p) (replay p n)))
  (⊨ ((problem-test p) (problem-start p) (car (node-path n))))
  (⊨ ((problem-goal? p) (last (node-path n))))
  (⊦= (node-depth n) (length (node-actions n)))
  (⊦= (add1 (node-depth n)) (length (node-path n))))

; the first solution of (search p stats: st) and the stats right after it
(define (first/stats search p . args)
  (let* ((st (make-search-stats))
         (n (first-solution (apply search p (append args (list stats: st))))))
    (values n st)))

(define (count s) (length (stream:->list s)))

;;; ==================== SUITE ====================

(define-suite search-suite

  ((test-problem _)
   (let ((p (puzzle "123456078")))
     (⊨ (problem? p))
     (⊦= "123456078" (problem-start p))
     (⊦= puzzle/goal (problem-goal p))
     (⊨ ((problem-goal? p) (string-copy puzzle/goal)))
     (⊭ ((problem-goal? p) "123456078"))
     (⊦= 2 ((problem-heuristic p) "123456078"))
     (⊨ (eq? string=? (problem-test p)))
     (⊭ (problem-hash p))
     (⊦= '((up "123450786") (left "123456708")) (puzzle/successors "123456780")))
   (⊭ ((problem-goal? jugs) '(0 . 0)))
   (⊭ ((problem-goal? (make-problem start: 0 successors: (K '()))) 0))
   (⊦= '(a b) (map step-action '((a 1) (b 2 7))))
   (⊦= '(1 2) (map step-state '((a 1) (b 2 7))))
   (⊦= '(1 7) (map step-cost '((a 1) (b 2 7)))))

  ((test-romania-ucs _)
   (let-values (((n st) (first/stats uniform-cost-search (romania-problem))))
     (⊦= 418 (node-cost n))
     (⊦= '(Arad Sibiu RimnicuVilcea Pitesti Bucharest) (node-path n))
     (⊦= '((go . Sibiu) (go . RimnicuVilcea) (go . Pitesti) (go . Bucharest)) (node-actions n))
     (⊦= 12 (search-stats-expanded st))
     (⊦= 14 (search-stats-generated st))
     (⊦solution (romania-problem) n)))

  ((test-romania-a* _)
   (let-values (((n st) (first/stats a*-search (romania-problem))))
     (⊦= 418 (node-cost n))
     (⊦= '(Arad Sibiu RimnicuVilcea Pitesti Bucharest) (node-path n))
     (⊦= 5 (search-stats-expanded st))
     (⊦= 11 (search-stats-generated st))))

  ((test-romania-greedy _)
   (let-values (((n st) (first/stats greedy-best-first-search (romania-problem))))
     (⊦= 450 (node-cost n))
     (⊦= '(Arad Sibiu Fagaras Bucharest) (node-path n))
     (⊦= 3 (search-stats-expanded st))
     (⊦= 8 (search-stats-generated st))))

  ((test-romania-bfs _)
   (let-values (((n st) (first/stats breadth-first-search (romania-problem))))
     (⊦= 3 (node-depth n))
     (⊦= 450 (node-cost n))
     (⊦= '(Arad Sibiu Fagaras Bucharest) (node-path n))
     (⊦= 8 (search-stats-expanded st))))

  ((test-romania-beam _)
   (⊦= '(3 5 7 8)
       (map (λ (width)
              (let-values (((n st) (first/stats beam-search (romania-problem) width)))
                (⊦= '(Arad Sibiu Fagaras Bucharest) (node-path n))
                (⊦= 450 (node-cost n))
                (search-stats-expanded st)))
            '(1 2 3 10)))
   ; an infinite width is breadth-first search
   (let-values (((n st) (first/stats beam-search (romania-problem) +inf.0)))
     (⊦= 3 (node-depth n))
     (⊦= 8 (search-stats-expanded st))
     (⊦= 4 (search-stats-iterations st))))

  ((test-reopening _)
   ; A closed-set A* that never reopened a state would expand C first through B (f = 4 < 5), close it,
   ; and answer 7 through (S B C G); reopening C when the cheaper path through A appears gives 5.
   (let-values (((n st) (first/stats a*-search inconsistent)))
     (⊦= 5 (node-cost n))
     (⊦= '(S A C G) (node-path n))
     (⊦= 5 (search-stats-expanded st)))
   (⊦= 5 (node-cost (first-solution (uniform-cost-search inconsistent))))
   (⊦= 5 (node-cost (first-solution (ida*-search inconsistent)))))

  ((test-ucs-stream-order _)
   (let ((s (uniform-cost-search (romania-problem goal?: (λ (c) (memq c '(Bucharest Pitesti Craiova)))))))
     (⊦= '((Pitesti . 317) (Craiova . 366) (Bucharest . 418))
         (map (λ (n) (cons (node-state n) (node-cost n))) (stream:->list ((stream:take 3) s))))))

  ((test-8-puzzle-optimal-depths _)
   (let ((instances '("123456078" "123406758" "123046758" "013425786" "413726058" "813402765"))
         (depths '(2 2 3 4 6 14)))
     (define (depths/expanded search)
       (map (λ (start)
              (let-values (((n st) (first/stats search (puzzle start))))
                (⊦solution (puzzle start) n)
                (list (node-depth n) (search-stats-expanded st))))
            instances))
     (let ((a* (depths/expanded a*-search)))
       (⊦= depths (map car a*))
       (⊦= '(2 2 3 4 6 76) (map cadr a*)))
     (let ((bfs (depths/expanded breadth-first-search)))
       (⊦= depths (map car bfs))
       (⊦= '(6 8 16 28 52 5960) (map cadr bfs)))
     (⊦= depths (map car (depths/expanded iterative-deepening-search)))
     (⊦= depths (map car (depths/expanded ida*-search)))
     (⊦= depths (map car (depths/expanded bidirectional-search)))))

  ((test-8-puzzle-medium-a* _)
   (⊦= '((8 8 16) (12 47 84) (16 194 316) (20 540 857) (24 2310 3593))
       (map (λ (start)
              (let-values (((n st) (first/stats a*-search (puzzle start))))
                (⊦solution (puzzle start) n)
                (list (node-depth n) (search-stats-expanded st) (search-stats-generated st))))
            '("012483765" "012356478" "012345786" "012347856" "012347685"))))

  ((test-8-puzzle-hardest _)
   ; the two instances at the largest distance from the goal, 31 moves
   (for-each (λ (start bfs-expanded)
               (let-values (((n st) (first/stats a*-search (puzzle start))))
                 (⊦= 31 (node-depth n))
                 (⊦= 20290 (search-stats-expanded st))
                 (⊦solution (puzzle start) n))
               (let-values (((n st) (first/stats breadth-first-search (puzzle start))))
                 (⊦= 31 (node-depth n))
                 (⊦= bfs-expanded (search-stats-expanded st)))
               (let-values (((n st) (first/stats bidirectional-search (puzzle start))))
                 (⊦= 31 (node-depth n))
                 (⊦solution (puzzle start) n)
                 (⊨ (< (search-stats-expanded st) 20290)))
               (⊦= 31 (node-depth (first-solution (ida*-search (puzzle start))))))
             '("867254301" "647850321")
             '(181438 181439)))

  ((test-8-puzzle-unsolvable _)
   ; an odd permutation: breadth-first search exhausts its component, half of the 9! states
   (let* ((st (make-search-stats))
          (s (breadth-first-search (puzzle "283164705") stats: st)))
     (⊨ (stream:null? s))
     (⊦= 181440 (search-stats-expanded st))
     (⊦= 181440 (search-stats-generated st))))

  ((test-water-jugs _)
   (let-values (((n st) (first/stats breadth-first-search jugs)))
     (⊦= 6 (node-depth n))
     (⊦= '((0 . 0) (0 . 5) (3 . 2) (0 . 2) (2 . 0) (2 . 5) (3 . 4)) (node-path n))
     (⊦= 13 (search-stats-expanded st))
     (⊦solution jugs n))
   (let ((n (first-solution (iterative-deepening-search jugs))))
     (⊦= 6 (node-depth n))
     (⊦solution jugs n)))

  ((test-missionaries-and-cannibals _)
   (let-values (((n st) (first/stats breadth-first-search (mc))))
     (⊦= 11 (node-depth n))
     (⊦= 14 (search-stats-expanded st))
     (⊦solution (mc) n))
   (let* ((st (make-search-stats))
          (s (breadth-first-search (mc goal?: (K #f)) stats: st)))
     (⊨ (stream:null? s))
     (⊦= 16 (search-stats-expanded st))
     (⊦= 16 (search-stats-generated st))))

  ((test-n-queens _)
   (⊦= '((1 2 2) (0 3 3) (0 6 6) (2 17 17) (10 54 54) (4 153 153) (40 552 552) (92 2057 2057) (352 8394 8394))
       (map (λ (n)
              (let* ((st (make-search-stats))
                     (k (count (depth-first-search (queens n) duplicates: 'tree stats: st))))
                (list k (search-stats-generated st) (search-stats-expanded st))))
            (iota 9 1)))
   (⊦= '(0 4 7 5 2 6 1 3) (reverse (node-state (first-solution (depth-first-search (queens 8) duplicates: 'tree)))))
   (⊦= 92 (count (breadth-first-search (queens 8) duplicates: 'tree)))
   (⊦= 92 (count (depth-first-search (queens 8)))))

  ((test-laziness _)
   (let* ((naturals (make-problem start: 0 successors: (λ (n) `((inc ,(+ n 1)))) goal?: even?))
          (st (make-search-stats))
          (s (depth-first-search naturals stats: st)))
     (⊦= '(0 2 4) (map node-state (stream:->list ((stream:take 3) s))))
     (⊦= 4 (search-stats-expanded st)))
   ; a successors stream is explored lazily too
   (let* ((naturals (make-problem start: 0 successors: (λ (n) (stream:singleton `(inc ,(+ n 1)))) goal?: even?))
          (s (depth-first-search naturals)))
     (⊦= '(0 2 4) (map node-state (stream:->list ((stream:take 3) s)))))
   (let* ((cycle (make-problem start: 'a successors: (λ (s) (if (eq? s 'a) '((→b b)) '((→a a)))) goal?: (K #t)))
          (s (breadth-first-search cycle duplicates: 'tree)))
     (⊦= '(a b a b a) (map node-state (stream:->list ((stream:take 5) s))))
     (⊦= '(0 1 2 3 4) (map node-depth (stream:->list ((stream:take 5) s)))))
   (⊦= '(a b) (map node-state (stream:->list (breadth-first-search
                                                (make-problem start: 'a successors: (λ (s) (if (eq? s 'a) '((→b b)) '((→a a)))) goal?: (K #t)))))))

  ((test-ids-semantics _)
   (let* ((st (make-search-stats))
          (s (iterative-deepening-search (queens 6) stats: st)))
     (⊦= 4 (count s))
     (⊦= 7 (search-stats-iterations st))
     (⊭ (search-stats-cutoff? st)))
   ; depth-limited search marks the cut, and finds nothing below its limit
   (let ((st (make-search-stats)))
     (⊦= 0 (count (depth-limited-search (queens 6) 5 stats: st)))
     (⊨ (search-stats-cutoff? st)))
   (let ((st (make-search-stats)))
     (⊦= 4 (count (depth-limited-search (queens 6) 6 stats: st)))
     (⊭ (search-stats-cutoff? st)))
   ; max-depth: bounds the deepening of an infinite space without solutions
   (let ((st (make-search-stats)))
     (⊦= 0 (count (iterative-deepening-search (make-problem start: 0 successors: (λ (n) `((inc ,(+ n 1)))))
                                              max-depth: 5 stats: st)))
     (⊦= 6 (search-stats-iterations st))
     (⊨ (search-stats-cutoff? st))))

  ((test-ida*-semantics _)
   ; With three goals, the straight-line heuristic is not 0 at Pitesti: f = g + h = 417 there.
   ; IDA* yields a goal in the first iteration that visits it (the one where the largest f on its
   ; path first fits the bound), so Pitesti is not lost, although its g = 317 is below the
   ; previous bound of that iteration.
   (let* ((st (make-search-stats))
          (s (ida*-search (romania-problem goal?: (λ (c) (memq c '(Bucharest Pitesti Craiova)))) stats: st)))
     (let ((first (first-solution s)))
       (⊦= '(Arad Sibiu RimnicuVilcea Pitesti) (node-path first))
       (⊦= 317 (node-cost first))))
   (let ((n (first-solution (ida*-search (romania-problem)))))
     (⊦= 418 (node-cost n))
     (⊦= '(Arad Sibiu RimnicuVilcea Pitesti Bucharest) (node-path n)))
   (⊦= 92 (count (ida*-search (queens 8)))))

  ((test-bidirectional _)
   (let ((st (make-search-stats)))
     (let ((n (first-solution (bidirectional-search (puzzle "813402765") stats: st))))
       (⊦= 14 (node-depth n))
       (⊦= 14 (node-cost n))
       (⊦solution (puzzle "813402765") n)
       (⊨ (< (search-stats-expanded st) 5960))))
   (let ((s (bidirectional-search (puzzle puzzle/goal))))
     (⊦= '(0) (map node-depth (stream:->list s))))
   ; a single solution
   (⊦= 1 (count (bidirectional-search (puzzle "123456078"))))
   ; an unsolvable instance in a small space: the forward side runs out first
   (⊨ (stream:null? (bidirectional-search (make-problem start: 'a successors: (K '()) goal: 'b
                                                        predecessors: (λ (s) '((x c) (y d))))))))

  ((test-live-stats _)
   (define (live search duplicates)
     (let* ((st (make-search-stats))
            (s (search (queens 6) stats: st duplicates: duplicates))
            (before (list (search-stats-expanded st) (search-stats-generated st))))
       (let* ((one (stream:->list ((stream:take 1) s)))
              (after-one (search-stats-expanded st))
              (two (stream:->list ((stream:take 2) s)))
              (after-two (search-stats-expanded st)))
         (⊨ (<= after-one after-two))
         (⊨ (< 0 after-one))
         before)))
   (⊦= '(0 1) (live breadth-first-search 'tree))
   (⊦= '(0 1) (live a*-search 'tree))
   (⊦= '(0 0) (live depth-first-search 'path))
   (⊦= '(0 1) (live (λ (p #!key stats duplicates) (beam-search p 3 stats: stats duplicates: duplicates)) 'graph)))

  ((test-random-graphs _)
   ; on random graphs, every complete algorithm finds a solution iff uniform-cost search does, and
   ; the optimal ones find its cost: A* with h = 0, with the exact distance (consistent), and with
   ; a random fraction of it (admissible, often inconsistent, although these small graphs seldom
   ; make A* reopen a state: test-random-reopening does), IDA* likewise; with unit costs,
   ; breadth-first, iterative deepening and bidirectional search find the same depth.
   (set-pseudo-random-seed! (string->utf8 "aux-search"))
   (do ((t 0 (add1 t))) ((= t 150))
     (let* ((unit? (even? t))
            (graph (random-graph (+ 4 (modulo t 7)) (+ 15 (modulo t 4) (* 5 (modulo t 3))) unit?))
            (exact (distances-to graph (sub1 (vector-length graph))))
            (h* (λ (i) (hash-table-ref/default exact i 0)))
            (fractions (list->vector (map (λ (_) (pseudo-random-integer 101)) (iota (vector-length graph)))))
            (h (λ (i) (* (h* i) (/ (vector-ref fractions i) 100))))
            (cost (λ (s) (let ((n (first-solution s))) (and n (node-cost n)))))
            (depth (λ (s) (let ((n (first-solution s))) (and n (node-depth n)))))
            (c (cost (uniform-cost-search (graph-problem graph (K 0))))))
       (⊦= (hash-table-ref/default exact 0 #f) c)
       (⊦= c (cost (a*-search (graph-problem graph (K 0)))))
       (⊦= c (cost (a*-search (graph-problem graph h*))))
       (⊦= c (cost (a*-search (graph-problem graph h))))
       (⊦= c (cost (ida*-search (graph-problem graph h))))
       (⊦= c (cost (ida*-search (graph-problem graph h*))))
       (⊦= (and c #t) (and (first-solution (depth-first-search (graph-problem graph h) duplicates: 'graph)) #t))
       (⊦= (and c #t) (and (first-solution (breadth-first-search (graph-problem graph h))) #t))
       (when unit?
         (⊦= c (depth (breadth-first-search (graph-problem graph h))))
         (⊦= c (depth (iterative-deepening-search (graph-problem graph h))))
         (⊦= c (depth (bidirectional-search (graph-problem graph h))))
         (⊦= c (depth (beam-search (graph-problem graph h) +inf.0))))
       (let ((n (first-solution (a*-search (graph-problem graph h)))))
         (when n (⊦solution (graph-problem graph h) n))))))

  ((test-random-reopening _)
   ; graphs whose heuristic is, state by state, either the exact distance or 0: admissible and
   ; far from consistent, so that A* often expands a state through a costlier path first, and must
   ; reopen it to stay optimal; a reopening shows as a second call of the successors of a state.
   (set-pseudo-random-seed! (string->utf8 "aux-search-reopening"))
   (let ((reopened 0))
     (do ((t 0 (add1 t))) ((= t 300))
       (let* ((graph (random-graph (+ 12 (modulo t 7)) (+ 20 (* 5 (modulo t 4))) #f))
              (goal (sub1 (vector-length graph)))
              (exact (distances-to graph goal))
              (exact? (list->vector (map (λ (_) (< (pseudo-random-integer 100) 30)) (iota (vector-length graph)))))
              (h (λ (i) (if (vector-ref exact? i) (hash-table-ref/default exact i 0) 0)))
              (calls (make-vector (vector-length graph) 0))
              (p (make-problem start: 0 goal: goal test: = heuristic: h
                               successors: (λ (i)
                                             (vector-set! calls i (add1 (vector-ref calls i)))
                                             ((graph/steps graph) i))))
              (cost (λ (s) (let ((n (first-solution s))) (and n (node-cost n)))))
              (c (hash-table-ref/default exact 0 #f)))
         (⊦= c (cost (a*-search p)))
         (when (any (λ (k) (> k 1)) (vector->list calls)) (set! reopened (add1 reopened)))
         (⊦= c (cost (ida*-search p)))))
     (⊨ (<= 20 reopened)))) ; 22 of these 300 graphs

  ((test-search-stats _)
   (let ((st (make-search-stats)))
     (⊨ (search-stats? st))
     (⊦= '((expanded . 0) (generated . 0) (max-frontier . 0) (cutoff? . #f) (iterations . 0))
         (search-stats->alist st))
     (first-solution (breadth-first-search (romania-problem) stats: st))
     (⊦= '((expanded . 8) (generated . 12) (max-frontier . 4) (cutoff? . #f) (iterations . 0))
         (search-stats->alist st)))
   ; depth-first search has no frontier to measure
   (let-values (((n st) (first/stats depth-first-search (romania-problem))))
     (⊦= 'Bucharest (node-state n))
     (⊦= 0 (search-stats-max-frontier st))))

  ((test-best-first _)
   ; the generic best-first search, with uniform cost and with a depth-first f: Oradea, reached
   ; through Zerind, is a dead end, since its neighbors are known by cheaper paths
   (let-values (((n st) (first/stats best-first-search (romania-problem) node-cost)))
     (⊦= 418 (node-cost n))
     (⊦= 12 (search-stats-expanded st))
     (⊦= 14 (search-stats-generated st)))
   (let-values (((n st) (first/stats best-first-search (romania-problem) (λ (n) (- (node-depth n))))))
     (⊦= '(Arad Sibiu Fagaras Bucharest) (node-path n))
     (⊦= 5 (search-stats-expanded st))))

  ((test-nodes _)
   (let* ((n (first-solution (breadth-first-search (romania-problem))))
          (parent (node-parent n)))
     (⊦= 'Fagaras (node-state parent))
     (⊦= '(go . Bucharest) (node-action n))
     (⊦= 239 (node-cost parent))
     (⊦= 2 (node-depth parent))
     (⊭ (node-parent (node-parent (node-parent parent)))))
   ; solutions share their prefixes
   (let* ((s (uniform-cost-search (romania-problem goal?: (λ (c) (memq c '(Pitesti Bucharest))))))
          (pitesti (first-solution s))
          (bucharest (first-solution (stream:cdr s))))
     (⊦= 'Bucharest (node-state bucharest))
     (⊨ (eq? pitesti (node-parent bucharest)))))

  ((test-keys _)
   ; states that carry the action that made them, which their key ignores: every duplicate table
   ; must compare keys, through the problem's test and hash, for the searches to stay as small as
   ; on the plain 8-puzzle
   (let* ((hashed 0)
          (tagged (λ (steps) (map (λ (step) (list (car step) (cons (cadr step) (car step)))) steps)))
          (p (make-problem start: (cons "813402765" 'start)
                           successors: (λ (s) (tagged (puzzle/successors (car s))))
                           predecessors: (λ (s) (tagged (puzzle/predecessors (car s))))
                           goal: (cons puzzle/goal 'goal)
                           key: car test: string=? hash: (λ args (set! hashed (add1 hashed)) (apply string-hash args))
                           heuristic: (λ (s) (puzzle/manhattan (car s))))))
     (⊨ (eq? car (problem-key p)))
     (⊨ (procedure? (problem-predecessors p)))
     (⊨ ((problem-goal? p) (cons (string-copy puzzle/goal) 'other)))
     (let-values (((n st) (first/stats a*-search p)))
       (⊦= 14 (node-depth n))
       (⊦= 76 (search-stats-expanded st)))
     (let-values (((n st) (first/stats breadth-first-search p)))
       (⊦= 14 (node-depth n))
       (⊦= 5960 (search-stats-expanded st)))
     (let ((n (first-solution (bidirectional-search p))))
       (⊦= 14 (node-depth n))
       (⊦= (node-path (first-solution (bidirectional-search (puzzle "813402765"))))
           (map car (node-path n))))
     (⊦= 14 (node-depth (first-solution (iterative-deepening-search p))))
     (⊦= 14 (node-depth (first-solution (ida*-search p))))
     (⊨ (< 0 hashed))))

  )

(unittest/✓ search-suite)
