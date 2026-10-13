
(import (chicken condition) (chicken string) (only srfi-1 iota fold every) (aux unittest) (aux base) (aux category state) (aux category monad state))

; a counter: answers the current state, and increments it
(define tick (do/monad (← n get) (put (add1 n)) ,n))

; the usage example of the module: numbers the leaves of a tree, left to right
(define (label tree)
  (match/first tree
    ((,l . ,r) (do/monad (← l* (label l)) (← r* (label r)) ,(cons l* r*)))
    (else (do/monad (← n get) (put (add1 n)) ,(cons n tree)))))

(define-suite category-state-suite

  ((doc r) `((structure/section "Introduction")
             (p "This section tests the state monad, Haskell's " (code "State s a") ", where a computation is a transition from a state to the pair of "
                "its result and the next state, Haskell's " (code "(a, s)") " tuple; binding threads the state from one computation to the next, strictly, "
                "as Haskell's " (code "Control.Monad.State.Strict") " does.")

             (structure/section "Implementation")

             (p "A computation is a " (code category-state) " record around its transition, the image of Haskell's " (code newtype)
                ", so that a computation is a type of its own and not any procedure; " (code run-state) " applies the transition to a state.")

             (code/scheme/file "../aux.category.state.scm")))

  ((test/return _)
      (⊦= '(4 . s) (run-state (return 4) 's))
      (⊦= '(4 . s) (run-state (do/monad ,4) 's))
      `(doc (code return) " answers its value and leaves the state as it is."))

  ((test/get-put _)
      (⊦= '(3 . 3) (run-state get 3))
      (⊦= (cons (void) 7) (run-state (put 7) 3))
      (⊦= '((3 . 7) . 7) (run-state (do/monad (← a get) (put 7) (← b get) ,(cons a b)) 3))
      (⊦= '(10 . 10) (run-state (with-state (λ (s) (* 10 s)) get) 1))
      `(doc (code get) " answers the state, " (code put) " replaces it, and a later " (code get) " sees the new one; "
            (code with-state) " transforms the state before running a computation."))

  ((test/modify-gets _)
      (⊦= (cons (void) 6) (run-state (modify (λ (s) (* 2 s))) 3))
      (⊦= '(4 . 3) (run-state (gets add1) 3))
      (⊦= '((a b) . (b)) (run-state (do/monad (modify cdr) (← x (gets car)) (put (list x)) (gets (λ (s) (cons 'a s)))) '(z b c)))
      `(doc (code modify) " applies a function to the state, " (code gets) " answers a function of it."))

  ((test/state _)
      (⊦= '(6 . 4) (run-state (state (λ (s) (cons (* 2 s) (add1 s)))) 3))
      `(doc (code state) " embeds a transition that answers the pair of the result and the next state."))

  ((test/run-eval-exec _)
      (⊦= '(0 . 1) (run-state tick 0))
      (⊦= 0 (eval-state tick 0))
      (⊦= 1 (exec-state tick 0))
      (⊦= 10 (eval-state (with-state (λ (s) (* 10 s)) tick) 1))
      (⊦= 11 (exec-state (with-state (λ (s) (* 10 s)) tick) 1))
      `(doc (code run-state) " answers the pair of the result and the final state, " (code eval-state) " the result only and " (code exec-state)
            " the final state only, as Haskell's " (code runState) ", " (code evalState) " and " (code execState) "."))

  ((test/laws _)
      (let ((f (λ (x) (do/monad (modify (λ (s) (cons x s))) ,(* 2 x))))
            (g (λ (y) (do/monad (← s get) (put (list y s)) ,(+ y 1))))
            (m (do/monad (← s get) (put (cons 'm s)) ,(length s))))
        (for-each (λ (s0)
                    (⊦= (run-state (f 5) s0) (run-state (>>= (return 5) f) s0))
                    (⊦= (run-state m s0) (run-state (>>= m return) s0))
                    (⊦= (run-state (>>= (>>= m f) g) s0) (run-state (>>= m (λ (x) (>>= (f x) g))) s0)))
                  '(() (a) (a b c))))
      `(doc "The left identity, right identity and associativity laws hold on every state they are run on."))

  ((test/order _)
      (⊦= '((0 1 2) . 3) (run-state (do/monad (← a tick) (← b tick) (← c tick) ,(list a b c)) 0))
      (⊦= '(c b a) (exec-state (do/monad (modify (cut cons 'a <>)) (modify (cut cons 'b <>)) (modify (cut cons 'c <>))) '()))
      `(doc "The clauses of a " (code do/monad) " block run top to bottom, each on the state the previous one left."))

  ((test/when-unless _)
      (⊦= 1 (exec-state (when/monad #t tick) 0))
      (⊦= 0 (exec-state (when/monad #f tick) 0))
      (⊦= 0 (exec-state (unless/monad #t tick) 0))
      (⊦= 1 (exec-state (unless/monad #f tick) 0))
      `(doc (code when/monad) " and " (code unless/monad) " run a computation only when a condition holds, or fails, as Haskell's "
            (code when) " and " (code unless) "."))

  ((test/applicative _)
      (⊦= '(10 . 2) (run-state (<$> (λ (n) (* 10 n)) tick) 1))
      (⊦= '(1 . 2) (run-state (<*> (do/monad (← n tick) ,(λ (m) (- m n))) tick) 0))
      (⊦= '((0 . 1) . 2) (run-state (lift/monad cons tick tick) 0))
      (⊦= '((0 . 1) . 2) (run-state (zip/monad tick tick) 0))
      `(doc (code <$>) ", " (code <*>) " and " (code lift/monad) " run their computations left to right too, "
            "so in " (code "(<*> mf mx)") " the effects of " (code mf) " come first."))

  ((test/strict _)
      (let* ((log '())
             (m (do/monad
                  (← a (state (λ (s) (set! log (cons 'm log)) (cons 1 s))))
                  (let _ (set! log (cons 'f log)))
                  ,a)))
        (⊦= '() log)
        (⊦= '(1 . s) (run-state m 's))
        (⊦= '(f m) log))
      `(doc "Building a computation runs nothing; running " (code "(>>= m f)") " runs " (code m) " before " (code f) " is applied to its result, "
            "as the strict state monad does."))

  ((test/rerun _)
      (let ((m (traverse/monad (λ (x) (do/monad (← n tick) ,(* n x))) '(1 2 3))))
        (⊦= '((0 2 6) . 3) (run-state m 0))
        (⊦= '((10 22 36) . 13) (run-state m 10))
        (⊦= '((0 2 6) . 3) (run-state m 0)))
      `(doc "A computation is a value: it may be run again, on any state, and it answers the same on the same state."))

  ((test/fail _)
      (let* ((m (do/monad (← (,a . ,b) ,'()) ,a))
             (message (condition-case (begin (run-state m 's) #f)
                        (e (exn) ((condition-property-accessor 'exn 'message) e)))))
        (⊨ (category-state? m))
        (⊨ (and (string? message) (substring-index "State monad does not support failure" message) #t))
        (⊨ (category-state? (fail 'x)))
        (⊦raises (exn) (run-state (fail 'x) 's)))
      `(doc "A value that does not match the pattern of a " (code ←) " clause goes to " (code fail)
            ", which raises an error when the computation runs, as Haskell's " (code "fail msg = error msg")
            " does, and not when it is built."))

  ((test/long-chain _)
      (let* ((n 100000)
             (sum (quotient (* n (- n 1)) 2))
             (left (fold (λ (i m) (>>= m (λ (_) (modify (λ (s) (+ s i)))))) (return (void)) (iota n))))
        (⊦= sum (exec-state (for-each/monad (λ (i) (modify (λ (s) (+ s i)))) (iota n)) 0))
        (⊦= sum (exec-state left 0))
        (⊦= n (length (eval-state (replicate/monad n tick) 0))))
      `(doc "Chains of a hundred thousand binds run, nested to the right as the traversals build them "
            "and to the left as a fold over " (code >>=) " does."))

  ((test/representation _)
      (⊨ (category-state? (do/monad (← x get) (return x))))
      (⊨ (not (category-state? (λ (s) (cons 1 s)))))
      (⊦raises (exn type) (run-state 42 0))
      `(doc "A computation is a " (code category-state) " record, as Haskell's " (code newtype) " makes " (code "State s a")
            " a type of its own: a procedure, even one shaped as a transition, is not a computation.  "
            (code run-state) " does not check its argument, so running a value that is not a computation raises CHICKEN's own error "
            "for a record of the wrong type, the cheapest outcome."))

  ((test/traversals _)
      (⊦= '(((a . 0) (b . 1) (c . 2)) . 3) (run-state (traverse/monad (λ (x) (<$> (cut cons x <>) tick)) '(a b c)) 0))
      (⊦= '(() . s) (run-state (traverse/monad (λ (x) tick) '()) 's))
      (⊦= '((0 1 2) . 3) (run-state (replicate/monad 3 tick) 0))
      (⊦= '(() . 0) (run-state (replicate/monad 0 tick) 0))
      (⊦= (cons (list 0 (void) 10) 11) (run-state (sequence/monad (list tick (modify (λ (s) (* 10 s))) tick)) 0))
      (⊦= '(c b a) (exec-state (for-each/monad (λ (x) (modify (cut cons x <>))) '(a b c)) '()))
      (⊦= '(((1 b) (0 a)) . 2) (run-state (fold/monad (λ (acc x) (do/monad (← n tick) ,(cons (list n x) acc))) '() '(a b)) 0))
      `(doc "The traversals run their computations left to right, the head first, so effects on the state happen in list order, "
            "as Haskell's " (code mapM) ", " (code sequence) ", " (code replicateM) ", " (code mapM_) " and " (code foldM) " do."))

  ((test/list->monad-is-not-sequence _)
      (⊦= '((1 2) . 0) (run-state (list->monad '(1 2)) 0))
      (⊦= '(1 2) (eval-state (list->monad '(1 2)) 0))
      (let1 (r (run-state (list->monad (list tick tick)) 0))
        (⊦= 0 (cdr r))
        (⊨ (and (= 2 (length (car r))) (every category-state? (car r)))))
      (⊦= '((0 1) . 2) (run-state (sequence/monad (list tick tick)) 0))
      `(doc (code list->monad) " is " (code return) " of the mconcat of a pure list, not Haskell's " (code sequence)
            ": given a list of computations it answers that very list, unrun, and leaves the state as it is.  "
            "To run a list of computations, use " (code sequence/monad) "."))

  ((test/label _)
      (⊦= '((((0 . a) 1 . b) 2 . c) . 3) (run-state (label '((a . b) . c)) 0))
      `(doc "The usage example of the module numbers the leaves of a tree left to right: the state is the next number, "
            "and the left subtree's effects come before the right one's."))

)

(unittest/✓ category-state-suite)
