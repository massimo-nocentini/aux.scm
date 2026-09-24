;; Exercises from "The Seasoned Schemer" (chapters 11-14), by Friedman and
;; Felleisen: collectors (CPS) and escaping continuations.
;;
;; Ported from the on-scheme repository, file src/seasoned-schemer.scm. The
;; matchable patterns are rewritten with aux `match/first`; `escape`, `try`
;; and `apply/cc` become `escapecc`, `trycc/named` and `apply/cc` of (aux
;; continuation classic); `letcc` is the one of (aux continuation). The
;; original `intersect-old` is exported as `intersect/letrec`.

(module (aux schemer seasoned) *

  (import scheme
          (chicken base)
          (aux base)
          (aux continuation)
          (aux continuation classic))

  (define multi-insert*&co
    (λ (new old_l old_r sexp coll)
      (let M ((sexp sexp)
              (coll coll))
        (cond
          ((null? sexp)
           (coll '() 0 0))
          ((atom? (car sexp))
           (cond
             ((equal? (car sexp) old_l)
              (M (cdr sexp) (λ (new_sexp L R)
                              (coll (cons new (cons old_l new_sexp)) (add1 L) R))))
             ((equal? (car sexp) old_r)
              (M (cdr sexp) (λ (new_sexp L R)
                              (coll (cons old_r (cons new new_sexp)) L (add1 R)))))
             (else
              (M (cdr sexp) (λ (new_sexp L R)
                              (coll (cons (car sexp) new_sexp) L R))))))
          (else
           (M (car sexp) (λ (new_sexp_car L_car R_car)
                           (M (cdr sexp) (λ (new_sexp_cdr L_cdr R_cdr)
                                           (coll
                                             (cons new_sexp_car new_sexp_cdr)
                                             (+ L_car L_cdr)
                                             (+ R_car R_cdr)))))))))))

  (define two-in-a-row?
    (λ (lat) ; `lat` stands for `List of AToms`
      (cond
        ((null? lat) #f)
        (else (let T ((preceding (car lat))
                      (rest (cdr lat)))
                (cond
                  ((null? rest) #f)
                  (else (or
                          (equal? preceding (car rest))
                          (T (car rest) (cdr rest))))))))))

  (define two-in-a-row?&hop
    (λ (lat)
      (cond
        ((null? lat) #f)
        (else
         (letcc hop
           (let T ((preceding (car lat))
                   (rest (cdr lat)))
             (cond
               ((null? rest) (hop #f))
               ((equal? preceding (car rest)) (hop #t))
               (else (T (car rest) (cdr rest))))))))))

  (define intersect/letrec
    (λ (this that)
      (letrec ((I (λ (set)
                    (cond
                      ((null? set) (quote ()))
                      ((member (car set) that) (cons (car set) (I (cdr set))))
                      (else (I (cdr set)))))))
        (I this))))

  (define intersect
    (λ (this that)
      (letrec ((I (λ (l)
                    (match/first l
                      (() '())
                      ((,i . ,is) (cond
                                    ((member i that) (cons i (I is)))
                                    (else (I is))))))))
        (I this))))

  (define intersect+all
    (λ (sets)
      (letcc hop
        (letrec (
                 ; 13th C.: "we can do whatever we want with the minor
                 ; version of `intersect`, nobody cares because it is protected".
                 (intersect (λ (this that)
                              (cond
                                ((null? that) (hop (quote ()))) ; 14th C.: spot it in the middle of recursion,
                                                                ; use `hop` to return '() without further delay.
                                (else
                                 (letrec ((I (λ (l)
                                               (match/first l
                                                 (() '())
                                                 ((,i . ,is) (if (member i that)
                                                               (cons i (I is))
                                                               (I is)))))))
                                   (I this))))))
                 ; the original matchable pattern `(() ...)` holds when every
                 ; element of the list is '(), the empty list included.
                 (all-null? (λ (l)
                              (cond
                                ((null? l) #t)
                                ((null? (car l)) (all-null? (cdr l)))
                                (else #f))))
                 (A (λ (sets)
                      (cond
                        ((all-null? sets) (hop (quote ()))) ; 14th C.: "this is it: the result is '()
                                                            ; and that's all there is to it",
                                                            ; spot it while reading the input.
                        ((null? (cdr sets)) (car sets))
                        (else (intersect (car sets) (A (cdr sets))))))))
          (cond
            ((null? sets) (quote ()))
            (else (A sets)))))))

  ;; A generalised `rember-upto-last`: it returns the `k`-th segment of `lat`
  ;; between occurrences of `atom`, where `k` <= 0 means the segment after the
  ;; last occurrence, that is, `rember-upto-last`. Respect to `intersect+all`,
  ;; which knows what the result is when it finds '(), this function knows
  ;; which pieces of the list are *not* in the result: when it sees `atom` it
  ;; forgets the pending computations, by `skip`, and restarts on the `cdr`.
  (define comb-upto-last
    (λ (atom k lat)
      (reverse (letcc skip ; "skip" because we discard many skipping in favor of the last one.
                 (letrec ((R (λ (prefix k lat)
                               (match/first lat
                                 (() prefix)
                                 ((,a . ,as)
                                  (cond
                                    ((equal? atom a)
                                     (cond
                                       ((zero? k) prefix)
                                       (else (skip (R (quote ()) (sub1 k) as)))))
                                    (else (R (cons a prefix) k as))))))))
                   (R (quote ()) (sub1 k) lat))))))

  ; LEFTMOST {{{

  (define leftmost/awkward
    (λ (l)
      (letrec ((L (λ (ll)
                    (match/first ll
                      (() (quote ()))
                      ((,first . ,rest)
                       (cond
                         ((symbol? first) first)
                         (else (let1 (a (L first))
                                 (cond
                                   ((symbol? a) a)
                                   (else (L rest)))))))))))
        (let1 (atom (L l))
          (cond
            ((symbol? atom) atom)
            (else l))))))

  (define leftmost/awkward+letcc
    (λ (l)
      (letrec ((L (λ (ll hop)
                    (match/first ll
                      (() (quote ()))
                      ((,first . ,rest)
                       (cond
                         ((symbol? first) (hop first))
                         (else (let1 (a (L first hop))
                                 (cond
                                   ((symbol? a) a)
                                   (else (L rest hop)))))))))))
        (let1 (atom (letcc hop (L l hop)))
          (cond
            ((symbol? atom) atom)
            (else l))))))

  (define leftmost/escape+explicit
    (λ (l)
      (letrec ((L (λ (ll out)
                    (cond
                      ((null? ll) (quote (no symbol here)))
                      ((symbol? (car ll)) (out (car ll)))
                      (else (begin
                              (L (car ll) out)
                              (L (cdr ll) out)))))))
        (escapecc
          (hop (L l hop))
          (else l)))))

  (define leftmost/escape
    (λ (l)
      (escapecc
        (hop (let L ((ll l))
               (match/first ll
                 (() (quote (no symbol here)))
                 ((,first . ,rest)
                  (cond
                    ((symbol? first) (hop first))
                    (else (begin
                            (L first)
                            (L rest))))))))
        (else l))))

  ; }}}

  (define rember1*/letcc
    (λ (atom sexp)
      (letrec ((R (λ (sexp skip)
                    (match/first sexp
                      (() (skip 'no-present))
                      ((,a . ,d)
                       (cond
                         ((symbol? a)
                          (cond
                            ((eq? a atom) d)
                            (else (cons a (R d skip)))))
                         (else (let1 (new-car (apply/cc R (list a)))
                                 (cond
                                   ((symbol? new-car) (cons a (R d skip)))
                                   (else (cons new-car d)))))))))))
        (let1 (s (apply/cc R (list sexp)))
          (cond
            ((symbol? s) sexp)
            (else s))))))

  ;; Prints its skip reasons on the current output port.
  (define rember1*/try
    (λ (atom sexp)
      (letrec ((make-skipper (λ (skip sexp)
                               (τ (let1 (reason `(,atom not present in ,sexp))
                                    (display reason)
                                    (skip reason)))))
               (R (λ (sexp keep-searching)
                    (match/first sexp
                      (() (keep-searching))
                      ((,a . ,d)
                       (cond
                         ((symbol? a)
                          (cond
                            ((eq? a atom) d)
                            (else (cons a (R d keep-searching)))))
                         (else (trycc/named
                                 (skip (cons (R a (make-skipper skip a)) d))
                                 (else (cons a (R d keep-searching)))))))))))
        (trycc/named
          (skip (R sexp (make-skipper skip sexp)))
          (else => (λ (previous)
                     (display previous)
                     sexp))))))

  )
