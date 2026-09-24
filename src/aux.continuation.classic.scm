
;; Classic continuation operators, in Feeley's `continuation-capture` style,
;; inspired by "The Seasoned Schemer" and by chapter 16 of Springer and
;; Friedman, "Scheme and the Art of Programming". They complement (aux
;; continuation), which provides `letcc`, `letcc*`, `callcc` and `trycc`;
;; those names are reused here, never redefined.
;;
;; Ported from the on-scheme repository, files src/continuations.scm and
;; introduction-to-continuations.scm; see
;; https://www.iro.umontreal.ca/~feeley/papers/FeeleySW01.pdf and
;; https://matt.might.net/articles/programming-with-continuations--exceptions-backtracking-search-threads-generators-coroutines/

(module (aux continuation classic) *

  (import scheme
          (chicken base)
          (chicken continuation)
          (only srfi-1 last)
          (aux base)
          (aux continuation))

  ;; Wraps a raw continuation object as a variadic procedure.
  (define continuation->λ (λ (raw) (λ args (apply continuation-return raw args))))

  ;; Binds both the raw continuation object `raw` (so that `continuation?`
  ;; holds on it) and its procedure wrapper `hop`.
  (define-syntax-rule (letcc/raw (raw hop) body ...)
    (continuation-capture (λ (raw)
                            (let1 (hop (continuation->λ raw))
                              body ...))))

  ;; `(apply/cc f a ... lst)` applies `f` to `a ...`, the elements of `lst`
  ;; and, as last argument, the current continuation as a procedure.
  (define (apply/cc f . args)
    (letcc k (apply f (append (butlast args) (last args) (list k)))))

  (define-syntax escapecc
    (syntax-rules (else =>)
      ((escapecc (out α) (else β)) (letcc out α β))
      ((escapecc (out α) (=> f)) (letcc out (let1 (result α) (f result))))))

  ;; Each clause names its own skip continuation; the first clause that does
  ;; not skip gives the result, otherwise the `else` expression does. The
  ;; `(else => f)` form applies `f` to the list of every clause's output.
  (define-syntax trycc/named
    (syntax-rules (else =>)
      ((trycc/named (skip α) ... (else β))
       (letcc success
         (letcc skip (success α)) ...
         β))
      ((trycc/named (skip α) ... (else => f))
       (letcc success
         (let1 (outputs (list (letcc skip (success α)) ...))
           (f outputs))))))

  ;; Stores the procedure wrapper of the current continuation in `v`, as the
  ;; original `(set! v (letcc (raw cont) cont))` does: the first call `(v)`
  ;; re-enters the `set!`, which then stores the unspecified value passed by
  ;; that call, so only a second call `(v)` fails as a call of non-procedure.
  (define-syntax-rule (set/cc! v) (set! v (letcc/raw (raw hop) hop)))

  ;; Known quirk, kept on purpose: `(hop v)` returns `v` from the whole
  ;; `letcc/raw`, it does not re-enter the `cond`; therefore the `(pred?
  ;; handling)` clauses are dead code and `(cond/cc (number? add1) (else (λ
  ;; (k) (+ 2 (k 3)))))` gives 3.
  (define-syntax cond/cc
    (syntax-rules (else)
      ((cond/cc (pred? handling) ... (else recv))
       (letcc/raw (cont hop)
         (cond
           ((continuation? cont) (recv hop))
           ((pred? cont) (handling cont)) ...
           (else (error "Contract violation")))))))

  (define current-continuation/cont
    (τ (continuation-capture (λ (cont) ((continuation->λ cont) cont)))))

  ;; Matt Might's version: returns a procedure `k` such that `(k k)` re-enters
  ;; the call site, returning `k` again.
  (define current-continuation/λ
    (τ (continuation-capture (compose Φ continuation->λ))))

  ;; A scoped version of the `make-escaper` of Springer and Friedman (ch.16):
  ;; `escaper` takes a procedure and returns an escape procedure, whose result
  ;; becomes the result of the whole `letcc/escaper` form, abandoning any
  ;; pending context.
  (define-syntax-rule (letcc/escaper escaper body ...)
    ((letcc top
       (let1 (escaper (λ (proc) (λ args (top (τ (apply proc args))))))
         (τ body ...)))))

  )

;; McCarthy's `amb` with a stack of failure continuations, and a SAT solver
;; built on it. As for (aux category continuation), this second module lives
;; in the extension `aux.continuation.classic`, so import (aux continuation
;; classic) before (aux continuation amb) when running from the source tree.
(module (aux continuation amb) *

  (import scheme
          (chicken base)
          (aux base)
          (aux continuation)
          (aux continuation classic))

  ;; `recv` receives `(ε ? ✗ ✓)`: choose, assert, fail and collect. When the
  ;; choices are exhausted `amb` returns the collected results, in order.
  (define amb
    (λ (recv)
      (letcc K
        (let* ((fail-stack '())
               (results '())
               (✓ (λ (v) (push! v results)))
               (✗ (λ ()
                    (cond
                      ((pair? fail-stack) (let1 (flag (car fail-stack))
                                            (set! fail-stack (cdr fail-stack))
                                            (flag flag)))
                      (else (K (reverse results))))))
               (ε (λ (choices)
                    (let1 (cc (current-continuation/λ))
                      (cond
                        ((null? choices) (✗))
                        (else (let1 (choice (car choices))
                                (set! choices (cdr choices))
                                (push! cc fail-stack)
                                choice))))))
               (? (λ (condition) (unless condition (✗)))))
          (recv ε ? ✗ ✓)))))

  ;; Exported because `sat-solve` matches it as a literal.
  (define (implies a b) (or (not a) b))

  ;; Known bug, kept on purpose: each variable's assertion is checked while
  ;; the other variables are still `(void)`, which counts as true, so answers
  ;; can be missed or duplicated. This is not the most efficient
  ;; implementation either, because a continuation is captured for each
  ;; occurrence of the same variable, instead of one for each variable.
  (define-syntax sat-solve
    (syntax-rules (and or implies not ∙ ▢)
      ((sat-solve vars formula)
       (sat-solve ∙ vars formula formula))
      ((sat-solve ∙ (var ...) formula assertion)
       (let ((var (void)) ...)
         (amb (λ (ε ? ✗ ✓)
                (sat-solve ▢ (ε ? ✗ ✓)
                  formula
                  (begin (✓ (list var ...)) (✗))
                  assertion)))))
      ((sat-solve ▢ selectors (not phi) body assertion)
       (sat-solve ▢ selectors phi body assertion))
      ((sat-solve ▢ selectors (and phi) body assertion)
       (sat-solve ▢ selectors phi body assertion))
      ((sat-solve ▢ selectors (and phi1 phi2 ...) body assertion)
       (sat-solve ▢ selectors phi1
         (sat-solve ▢ selectors (and phi2 ...) body assertion)))
      ((sat-solve ▢ selectors (or phi) body assertion)
       (sat-solve ▢ selectors phi body assertion))
      ((sat-solve ▢ selectors (or phi1 phi2 ...) body assertion)
       (sat-solve ▢ selectors phi1
         (sat-solve ▢ selectors (or phi2 ...) body assertion)))
      ((sat-solve ▢ selectors (implies phi1 phi2) body assertion)
       (sat-solve ▢ selectors phi1
         (sat-solve ▢ selectors phi2 body assertion)))
      ((sat-solve ▢ selectors #t body assertion) body)
      ((sat-solve ▢ (ε ? ✗ ✓) #f body assertion) (✗))
      ((sat-solve ▢ (ε ? ✗ ✓) v body assertion)
       (begin
         (set! v (ε (list #t #f)))
         (cond
           (assertion body)
           (else (✗)))))
      ((sat-solve ▢ selectors phi body)
       (sat-solve ▢ selectors phi body phi))))

  )
