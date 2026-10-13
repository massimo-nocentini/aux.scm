(module (aux category state) *

  (import scheme (chicken base) (aux base))

  #|

  -- Control.Monad.State.Strict (mtl), the flavour Metropolis.hs imports (M:8)
  newtype State s a = State { runState :: s -> (a, s) }

  instance Monad (State s) where
    return a = State $ \s -> (a, s)
    m >>= k  = State $ \s -> case runState m s of
                               (a, s') -> runState (k a) s'
    fail msg = error msg

  get           = State $ \s -> (s, s)
  put s         = State $ \_ -> ((), s)
  modify f      = State $ \s -> ((), f s)
  gets f        = State $ \s -> (f s, s)
  state f       = State f
  withState f m = modify f >> m
  evalState m s = fst (runState m s)
  execState m s = snd (runState m s)

  |#

  ; newtype State: a computation is a record around its transition s -> (a . s), Haskell's
  ; (a, s) tuple, so that a computation is a type of its own, as the newtype makes it.  The
  ; pair is faster than (values a s) (0.65-0.9 the time over 10^5 binds, csc -O2); the record
  ; costs 1.4-2 times a bare procedure per bind, measured likewise.
  (define-record category-state run)
  (set-record-printer! category-state (λ (m port) (display "#<state>" port)))

  ; runState, evalState, execState.
  (define (run-state m s) ((category-state-run m) s))
  (define (eval-state m s) (car (run-state m s)))
  (define (exec-state m s) (cdr (run-state m s)))

  ; Monad instance.  Binding is strict, as Control.Monad.State.Strict: m runs to completion
  ; before f is applied to its result; the second run is a tail call.
  (define (return v) (make-category-state (λ (s) (cons v s))))
  (define (>>= m f)
    (make-category-state (λ (s) (let1 (r (run-state m s)) (run-state (f (car r)) (cdr r))))))
  ; upstream's `fail msg = error msg`, raised when the computation runs, as the reader's.
  (define (fail . args)
    (make-category-state
      (λ (s) (error "State monad does not support failure" `((on ,s) (args . ,args))))))

  ; MonadState.
  (define get (make-category-state (λ (s) (cons s s))))
  (define (put s) (make-category-state (λ (_) (cons (void) s))))
  (define (modify f) (make-category-state (λ (s) (cons (void) (f s)))))
  (define (gets f) (make-category-state (λ (s) (cons (f s) s))))
  (define (state f) (make-category-state f))                    ; f : s -> (a . s)
  (define (with-state f m) (make-category-state (λ (s) (run-state m (f s)))))

)

(import (aux category monad))

(module (aux category monad state) = ((aux category monad) (aux category state)))

#|

(import scheme (chicken base) (aux base) (aux category state) (aux category monad state))

(define (label tree) ; number the leaves of a tree, left to right
  (match/first tree
    ((,l . ,r) (do/monad (← l* (label l)) (← r* (label r)) ,(cons l* r*)))
    (else (do/monad (← n get) (put (add1 n)) ,(cons n tree)))))

(run-state (label '((a . b) . c)) 0) ; => ((((0 . a) 1 . b) 2 . c) . 3)

|#
