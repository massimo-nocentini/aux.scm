; (aux stream sicp): SICP-style lazy streams, ported from the old `on-scheme` repository (streams.scm).
;
; A stream is either '() or a promise that, once forced (possibly several times), yields '() or a
; pair (car . stream). Differences from the original:
;   - `Λ` is renamed `λ§` because (aux base) already exports a different `Λ` (a matching lambda);
;   - `stream:cons` expands to aux `cons§` and `stream:empty` is simply '();
;   - stream:null?, stream:car and stream:cdr force repeatedly, so that aux streams (whose
;     combinators may yield a promise of a promise, like `map§` or `take§`) can be consumed too;
;   - `car+cdr` (srfi-1) is replaced by `letcar&cdr` from (aux base);
;   - `stream:0s` is added (the original tests referenced it without a definition).
;   - `stream:scan` folds left in stream order with an accumulator, (op (op a₀ a₁) a₂), whereas the
;     original applied `op` to each reversed prefix, (op a₂ a₁ a₀), in quadratic time.
;   - since `stream:empty` is '(), the tree-recursive `stream:map` (`*: #t`) treats a '() element as
;     a nested empty stream (as the original did for its promise `stream:empty`), hence a '() datum
;     is no longer passed to `func` as a leaf in that mode.
; The `stream:` prefix makes clashes with aux's `§`-suffixed names impossible.

(module (aux stream sicp) *

  (import scheme
          (chicken base)
          (aux base)
          (only (aux stream) cons§)
          (only srfi-1 any every reduce-right)
          (only (aux commons) ○ fmap collect-values identity* within?))

  (define stream:force*
    (λ (s)
       (let F ((s s))
         (if (promise? s) (F (force s)) s))))

  (define stream:null? (○ null? stream:force*))

  (define stream:car
    (○ (λ (i)
          (cond
            ((pair? i) (car i))
            (else i)))
       stream:force*))

  (define stream:cdr
    (○ (λ (i)
          (cond
            ((pair? i) (cdr i))
            (else i)))
       stream:force*))

  (define stream:cadr (○ stream:car stream:cdr))
  (define stream:cddr (○ stream:cdr stream:cdr))
  (define stream:caddr (○ stream:car stream:cddr))

  (define-syntax stream:cons
    (syntax-rules ()
      ((stream:cons a d) (cons§ a d))))

  (define-syntax letdelay
    (syntax-rules ()
      ((letdelay ((bind sexp) ...) body ...)
       (letrec ((bind (delay-force sexp)) ...) body ...))))

  (define-syntax define-delay
    (syntax-rules ()
      ((define-delay (bind sexp) ...) (define-values (bind ...)
                                        (letdelay ((bind sexp) ...)
                                                  (values bind ...))))
      ((define-delay bind sexp) (define bind (letdelay ((α sexp)) α)))))

  (define-syntax stream:dest/car+cdr
    (syntax-rules (else ∅)

      ((stream:dest/car+cdr (s ∅) bind)
       (stream:dest/car+cdr s bind (else stream:empty)))

      ((stream:dest/car+cdr s ((a d) body) (else sexp))
       (cond
         ((stream:null? s) sexp)
         (else (let-values (((a d) (values (stream:car s) (stream:cdr s)))) ; inefficient because `force` will be called twice
                 body))))))

  (define-syntax λ§ ; "big-lambda", a functional abstraction that returns a *stream* of values
    (syntax-rules ()
      ((λ§ args body ...)
       (lambda args (δ! body ...)))))

  (define stream:empty '())

  (define-syntax :⁺
    (syntax-rules ()
      ((:⁺ α) α)
      ((:⁺ a b ...) (stream:cons a (:⁺ b ...)))))

  (define stream:singleton
    (λ§ (a)
        (stream:cons a stream:empty)))

  (define stream:ref
    (λ (n)
       (λ (α)
          (letrec ((R (λ (m s)
                         (cond
                           ((zero? m) ((○ car stream:force*) s)) ; `car` on purpose: referencing past the end is an error
                           (else (R (sub1 m) (stream:cdr s)))))))
            (R n α)))))

  (define stream:foldr
    (λ (func init)
       (letrec ((F (λ§ (s)
                       (stream:dest/car+cdr s
                                            ((scar scdr) (func scar (F scdr)))
                                            (else (init))))))
         (○ force F))))

  (define stream:map
    (λ (func #!key (* #f)) ; `*` in the sense of *starred* defs in 'The Little Schemer',
       ; namely to perform __tree recursion__ over streams.
       (letrec ((M (λ§ (s)
                       (stream:dest/car+cdr (s ∅)
                                            ((scar scdr) (cond
                                                           ((and * (or (promise? scar) (null? scar))) (stream:cons (M scar) (M scdr))) ; '() is stream:empty
                                                           (else (stream:cons
                                                                   (collect-values (λ () (func scar)))
                                                                   (M scdr)))))))))
         M)))

  (define stream:append-map
    (λ (f)
       (letrec ((M (λ§ (s)
                       (stream:dest/car+cdr (s ∅)
                                            ((scar scdr) (stream:append (f scar) (M scdr)))))))
         M)))

  (define stream:filter
    (λ (pred?)
       (letrec ((F (λ§ (s)
                       (stream:dest/car+cdr (s ∅)
                                            ((a d) (cond
                                                     ((pred? a) (stream:cons a (F d)))
                                                     (else (F d))))))))
         F)))

  (define stream:take
    (λ (n)
       (λ (s)
          (letrec ((T (λ§ (i r)
                          (cond
                            ((> i n) stream:empty)
                            (else (stream:dest/car+cdr (r ∅)
                                                       ((rcar rcdr) (stream:cons rcar (T (add1 i) rcdr)))))))))
            (T 1 s)))))

  (define stream:->list
    (λ (s)
       (stream:dest/car+cdr s
                            ((scar scdr) (cons scar (stream:->list scdr)))
                            (else '()))))

  (define list○take (λ (n) (○ stream:->list (stream:take n))))

  (define stream:iterator
    (λ (s)
       (let1 (α (stream:cons 'useless s))
             (λ ()
                (set! α (stream:cdr α))
                (stream:car α)))))

  (define stream:repeat
    (λ (n)
       (letdelay ((R (stream:cons n R)))
                 R)))

  (define stream:0s (stream:repeat 0))

  (define stream:zip-with
    (let ((unpadded (list 'unpadded))) ; a fresh sentinel, so that any value (even #f) can be a `pad:`
      (λ (op #!key (pad unpadded))
         ; without `pad:` zipping stops at the end of the shortest stream; with it, an exhausted
         ; stream reads as `pad` and zipping stops only when every stream has ended.
         (let* ((padded? (not (eq? pad unpadded)))
                (end? (if padded? (λ (fs) (every null? fs)) (λ (fs) (any null? fs))))
                (head (λ (f) (cond ((pair? f) (car f)) ((null? f) pad) (else f)))) ; as stream:car, but '() is `pad`
                (tail (λ (f) (if (pair? f) (cdr f) f))))                           ; as stream:cdr
           (letrec ((Z (λ§ streams
                           (let ((fs (map stream:force* streams))) ; each stream forced once per element
                             (cond
                               ((end? fs) stream:empty)
                               (else (stream:cons
                                       (apply op (map head fs))
                                       (apply Z (map tail fs)))))))))
             Z)))))

  (define stream:zip (stream:zip-with list))

  (define stream:convolution
    (λ (func scale comb)
       (letrec ((C (λ§ (s r)
                       (stream:dest/car+cdr (s ∅)
                                            ((scar scdr) (stream:dest/car+cdr (r ∅)
                                                                              ((rcar rcdr) (stream:cons
                                                                                             (func scar rcar)
                                                                                             (comb ((scale scar) rcdr)
                                                                                                   (C scdr r))))))))))
         C)))

  (define stream:tails
    (let1 (shift-first (○ stream:cdr car))
          (λ (s)
             (letrec ((C (λ§ streams
                             (stream:cons
                               streams
                               (apply C (cons (shift-first streams) streams))))))
               (C s)))))

  (define stream:prefixes
    (○ (stream:map (fmap stream:car)) stream:tails))

  (define stream:scan ; left scan: a₀, (op a₀ a₁), (op (op a₀ a₁) a₂), ...
    (λ (op)
       (letrec ((S (λ§ (acc s)
                       (stream:dest/car+cdr (s ∅)
                                            ((a d) (let ((acc (op acc a)))
                                                     (stream:cons acc (S acc d))))))))
         (λ§ (s)
             (stream:dest/car+cdr (s ∅)
                                  ((a d) (stream:cons a (S a d))))))))

  (define list->
    (λ (tail)
       (letrec ((L (λ§ (l)
                       (cond
                         ((null? l) tail)
                         (else (stream:cons (car l) (L (cdr l))))))))
         (λ (l)
            (cond
              ((null? l) tail)
              (else (L l)))))))

  (define list->stream (list-> stream:empty))

  (define stream:append
    (letrec ((S (λ§ (r s)
                    (stream:dest/car+cdr r
                                         ((rcar rcdr) (stream:cons rcar (S rcdr s)))
                                         (else s)))))
      (λ§ streams
          (cond
            ((null? streams) stream:empty)
            (else (letcar&cdr (((first rest) streams))
                              (S first (apply stream:append rest))))))))

  (define stream:merge
    (λ (pred?)
       (letrec ((M (λ§ (s r) ; binary merge strategy
                       (cond
                         ((stream:null? r) s)
                         (else (stream:dest/car+cdr s
                                                    ((scar scdr)
                                                     (stream:dest/car+cdr (r ∅)
                                                                          ((rcar rcdr) (cond
                                                                                         ((equal? (pred? scar rcar) #t)
                                                                                          (stream:cons scar (M scdr r)))
                                                                                         (else (stream:cons rcar (M s rcdr)))))))
                                                    (else r)))))))
         (λ streams
            (foldr M stream:empty streams)))))

  (define stream:tableau
    (λ (transform)
       (letrec ((T (λ§ (s)
                       (stream:cons s ((○ T transform) s)))))
         (λ (s)
            ((stream:map stream:car *: #f) (T s))))))

  (define-values (stream:enumerate-upper
                   stream:enumerate-lower
                   stream:enumerate-all)
    (letrec ((tuple (○ flatten list))
             (upper (λ§ (s r)
                        (stream:dest/car+cdr s
                                             ((scar scdr) (stream:dest/car+cdr r
                                                                               ((rcar rcdr) (stream:cons
                                                                                              (tuple scar rcar)
                                                                                              (interleave
                                                                                                ((stream:map (λ (ri)
                                                                                                                (tuple scar ri))) rcdr)
                                                                                                (upper scdr rcdr))))
                                                                               (else stream:empty)))
                                             (else stream:empty))))
             (lower (λ§ (s r)
                        (stream:dest/car+cdr s
                                             ((scar scdr) (stream:dest/car+cdr r
                                                                               ((rcar rcdr) (stream:cons
                                                                                              (tuple scar rcar)
                                                                                              (interleave
                                                                                                ((stream:map (λ (si)
                                                                                                                (tuple si rcar))) scdr)
                                                                                                (lower scdr rcdr))))
                                                                               (else stream:empty)))
                                             (else stream:empty))))
             (all (λ§ (s r)
                      (stream:dest/car+cdr s
                                           ((scar scdr) (stream:dest/car+cdr r
                                                                             ((rcar rcdr) (stream:cons
                                                                                            (tuple scar rcar)
                                                                                            (interleave
                                                                                              (interleave
                                                                                                ((stream:map (λ (ri)
                                                                                                                (tuple scar ri))) rcdr)
                                                                                                (all scdr rcdr))
                                                                                              ((stream:map (λ (si) (tuple si rcar))) scdr))))
                                                                             (else stream:empty)))
                                           (else stream:empty))))
             (interleave (λ§ (s r)
                             (stream:dest/car+cdr s
                                                  ((scar scdr) (stream:cons scar (interleave r scdr)))
                                                  (else r)))))
      (values ; a single stream enumerates itself, as the fold's rightmost operand
        (λ streams (reduce-right upper stream:empty streams))
        (λ streams (reduce-right lower stream:empty streams))
        (λ streams (reduce-right all stream:empty streams)))))

  (define stream:enumerate-weighted
    (λ (weight)
       (letrec ((make-tuple (○ flatten list))
                (B (λ§ (s r)
                       (stream:dest/car+cdr s
                                            ((scar scdr) (stream:dest/car+cdr r
                                                                              ((rcar rcdr) (stream:cons
                                                                                             (make-tuple scar rcar)
                                                                                             (interleave
                                                                                               ((stream:map (λ (ri)
                                                                                                               (make-tuple scar ri))) rcdr)
                                                                                               (B scdr rcdr))))
                                                                              (else stream:empty)))
                                            (else stream:empty))))
                (interleave (λ§ (s r)
                                (stream:dest/car+cdr s
                                                     ((scar scdr) (stream:dest/car+cdr r
                                                                                       ((rcar rcdr) (cond
                                                                                                      ((< (apply weight scar) (apply weight rcar))
                                                                                                       (stream:cons scar (interleave scdr r)))
                                                                                                      (else (stream:cons rcar (interleave s rcdr)))))
                                                                                       (else s)))
                                                     (else r)))))
         (λ streams
            (reduce-right B stream:empty streams)))))

  (define stream:take-while
    (λ (pred?)
       (letrec ((stop? (○ not pred?))
                (W (λ§ (s)
                       (stream:dest/car+cdr (s ∅)
                                            ((scar scdr) (cond
                                                           ((stop? scar) (stream:cons scar stream:empty))
                                                           (else (stream:cons scar (W scdr)))))))))
         W)))

  (define stream:map-consecutive-pairs
    (λ (func)
       (letrec ((P (λ§ (s)
                       (stream:cons
                         (func (stream:car s) (stream:cadr s))
                         (P (stream:cddr s)))))) ; disjoint consecutive pairs
         P)))

  (define stream:map-overlapping-pairs
    (λ (func)
       (letrec ((P (λ§ (s)
                       (stream:cons
                         (func (stream:car s) (stream:cadr s))
                         (P (stream:cdr s)))))) ; consecutive overlapping pairs
         P)))

  (define stream:§₂ ; binary `mplus`, for pure recursion.
    (λ§ (α β)
        (stream:dest/car+cdr α
                             ((α₀ α₊) (stream:cons α₀ (stream:§₂ β α₊)))
                             (else β))))

  (define stream:§ ; monadic `mplus`, for a *finite* number of streams.
    (λ§ streams
        (cond
          ((null? streams) stream:empty)
          (else (letcar&cdr (((α streams₊) streams))
                            (stream:dest/car+cdr α
                                                 ((α₀ α₊) (stream:cons α₀ (apply stream:§ (append streams₊ (list α₊)))))
                                                 (else (apply stream:§ streams₊))))))))

  (define stream:>>= ; monadic `bind`
    (λ§ (α β)
        (stream:dest/car+cdr (α ∅)
                             ((α₀ α₊) (stream:dest/car+cdr β
                                                           ((β₀ β₊) (stream:§₂ (β₀ α₀) (stream:>>= α₊ β₊)))
                                                           (else (error "binders stream should match the bindees one.")))))))

  (define stream:iterative-deepening
    (λ (m M depth-getter)
       (stream:filter (○ (within? m M) depth-getter))))

  )

