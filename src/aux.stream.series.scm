; (aux stream series): formal power series (McIlroy style), number sequences and Taylor
; expansions over (aux stream sicp) streams, ported from the old `on-scheme` repository (series.scm).
;
; Notes:
;   - `Λ` is written `λ§` (see (aux stream sicp));
;   - `accumulator` and `⁻¹` come from (aux commons), random numbers from `pseudo-random-integer`;
;   - many definitions are module-level streams (numbers/*, primes/*, taylor/*): they memoize
;     forever, so they are shared state among all importers;
;   - some exported names are generic and unprefixed: evens, odds, prime?, factorization,
;     divisable-by? (sic, kept for compatibility), multiples-of, not-multiples-of,
;     random-numbers, riordan-array, formalvar-series, Pythagorean-triples; beware of clashes
;     when importing this module together with other number-theoretic modules;
;   - a finite ('()-terminated) stream is a polynomial: series:+ and series:- pad it with zeros, so
;     the operations built on them (×, /, ⁻¹, exp, expt, √/∞, ...) see its trailing zeros, whereas a
;     bare stream:zip-with stops at the shortest stream; list->poly builds the infinite zero-padded
;     stream explicitly.

(module (aux stream series) *

  (import scheme
          (chicken base)
          (chicken random)
          (aux base)
          (only (aux commons) ○ accumulator ⁻¹)
          (aux stream sicp))

  (define series:0 (stream:repeat 0))

  (define list->poly (list-> series:0))

  (define series:const
    (λ§ (n)
        (stream:cons n series:0)))

  (define stream:1 (series:const 1))

  (define stream:1s (stream:repeat 1))

  (define series:range
    (λ (low high #!key (by add1))
       (letrec ((R (λ§ (low)
                       (cond
                         ((>= low high) stream:empty)
                         (else (stream:cons low (R (by low))))))))
         (R low))))

  (define series:from
    (λ§ (n)
        (stream:cons n (series:from (add1 n)))))

  (define series:+ (stream:zip-with + pad: 0)) ; a finite stream is a polynomial: zero-padded
  (define series:- (stream:zip-with - pad: 0))

  (define series:*
    (λ (a)
       (λ§ (s)
           ((stream:zip-with *) (stream:repeat a) s))))

  (define series:expt
    (λ (n)
       (λ (s)
          (letrec ((E (λ§ (n)
                          (cond
                            ((zero? n) stream:1)
                            (else (series:× s (E (sub1 n))))))))
            (E n)))))

  (define series:∫
    (λ§ (s)
        (stream:cons 0 ((stream:zip-with /) s (series:from 1)))))

  (define series:∂
    (λ§ (s)
        ((stream:zip-with *) (stream:cdr s) (series:from 1))))

  (define series:× (stream:convolution * series:* series:+))

  (define series:×*
    (λ series
       (let* ((elem* (λ (a b) ; product of two coefficients, each a number or a series in y
                        (cond
                          ((and (promise? a) (promise? b)) (series:× a b))
                          ((and (promise? a) (number? b)) ((series:* b) a))
                          ((and (number? a) (promise? b)) ((series:* a) b))
                          ((and (number? a) (number? b)) (series:const (* a b)))
                          (else (error "series:×*" "f₀ not a number" a b)))))
              (M (stream:convolution
                   elem*
                   (λ (a) (stream:map (λ (b) (elem* a b)))) ; scales every coefficient by `a`
                   (stream:zip-with series:+ pad: series:0))))
         (foldr M stream:1 series))))

  (define series:⁻¹
    (λ§ (s)
        (stream:dest/car+cdr (s ∅)
                             ((scar scdr)
                              (letdelay ((I (stream:cons (⁻¹ scar) ((series:* (/ -1 scar))
                                                                    (series:× scdr I)))))
                                        I)))))

  (define series:/&inversion
    (λ (num denum)
       (series:× num (series:⁻¹ denum))))

  (define series:/
    (λ§ (α β)
        (stream:dest/car+cdr (α ∅)
                             ((a α-cdr) (stream:dest/car+cdr (β ∅)
                                                             ((b β-cdr) (cond
                                                                          ((and (zero? a) (zero? b)) (series:/ α-cdr β-cdr))
                                                                          (else (let ((q (/ a b)))
                                                                                  (stream:cons q (series:/
                                                                                                   (series:- α-cdr ((series:* q) β-cdr))
                                                                                                   β)))))))))))

  (define series:√
    (λ (n)
       (let* ((average (λ args (/ (foldr + 0 args) (length args))))
              (improve (λ (guess) (average guess (/ n guess)))))
         (letdelay ((guesses (stream:cons 1 ((stream:map improve) guesses))))
                   guesses))))

  (define series:○
    (λ§ (α β)
        ; β(0) must vanish: β₀ is either 0 (univariate) or a series in y whose constant term is 0
        ; (bivariate). Never `equal?` a stream here: a forced one is cyclic, like series:0.
        ;
        ; The recursion C below is Horner's rule on β = β₀ + x·β′ with β₀ dropped, so it is right as
        ; it stands only when β₀ = 0. For a bivariate β₀(y) ≠ 0, α is first replaced by its Taylor
        ; expansion at β₀, α(β₀ + x·β′) = Σₙ (α⁽ⁿ⁾/n!)(β₀)·(x·β′)ⁿ, whose n-th coefficient is a
        ; univariate composition in y, well defined because β₀(0) = 0. Whether an infinite β₀ is zero
        ; is undecidable, so only series:0 itself takes the direct way; any other stream takes the
        ; (lazier, costlier) expansion, which gives the same coefficients when β₀ is zero.
        (let-values (((M B T) (let ((β₀ (stream:car β)))
                                (cond
                                  ((eqv? β₀ 0) (values series:× identity α))       ; univariate series
                                  ((not (and (promise? β₀) (eqv? (stream:car β₀) 0)))
                                   (error "series:○" "β₀ neither 0 nor a series in y with no constant term" β₀))
                                  ((eq? β₀ series:0) (values series:×* series:const α)) ; bivariate, β₀ = 0
                                  (else (letrec ((E (λ§ (n Δ) ; Δ = α⁽ⁿ⁾/n!, E yields (α⁽ⁿ⁾/n!)(β₀), ...
                                                        (stream:dest/car+cdr (Δ ∅)
                                                                             ((Δ₀ Δ′) (stream:cons
                                                                                        (series:○ Δ β₀)
                                                                                        (E (add1 n) ((series:* (/ (add1 n)))
                                                                                                     (series:∂ Δ)))))))))
                                          (values series:×* identity (E 0 α))))))))
          (letrec ((C (λ§ (α β)
                          (stream:dest/car+cdr (α ∅)
                                               ((a αs) (stream:dest/car+cdr (β ∅)
                                                                            ((b βs) (stream:cons (B a) (M (C αs β) βs)))))))))
            (C T β)))))

  (define series:√/∞
    (λ§ (α)
        ; an exhausted (finite) stream reads as its trailing zeros, as for series:+ and series:-;
        ; α₁ is read only when α₀ = 0, so that an implicit definition whose α depends on the
        ; head of its own square root, as S = √(1 + xS), does not force itself
        (stream:dest/car+cdr α
          ((α₀ α₁…)
           (cond
             ((zero? α₀)
              (stream:dest/car+cdr α₁…
                ((α₁ α₂…)
                 (if (zero? α₁)
                     (stream:cons 0 (series:√/∞ α₂…))
                     (error "series:√/∞" "α₀ zero but α₁ not zero: no power series square root")))
                (else series:0)))
             (else (letdelay ((Q (series:+
                                   (series:const (sqrt α₀)) ; Q(0)² = α₀
                                   (series:∫ (series:/
                                               (series:∂ α)
                                               ((series:* 2) Q))))))
                             Q))))
          (else series:0))))

  (define series:exp
    (λ§ (α)
        (cond
          (((○ not equal?) 0 (stream:dest/car+cdr α ((α₀ α′) α₀) (else 0))) ; the empty stream is 0
           (error "series:exp" "α₀ not zero"))
          (else (letdelay ((Y (series:+ stream:1 (series:∫
                                                   (series:× Y (series:∂ α))))))
                          Y)))))

  (define series:log₂
    (letrec ((summands (λ§ (n sign) ; carrying the sign, not a stream:map - per term, keeps it linear
                           (stream:cons (* sign (⁻¹ n)) (summands (add1 n) (- sign))))))
      ((stream:scan +) (summands 1 1))))

  (define series:Euler-transform
    (λ§ (s)
        (let* ((n-1 (stream:car s))
               (n (stream:cadr s))
               (n+1 (stream:caddr s))
               (square (λ (x) (* x x)))
               (t (- n+1 (/ (square (- n+1 n)) (+ n+1 n-1 (* -2 n))))))
          (stream:cons t (series:Euler-transform (stream:cdr s))))))

  (define series:integrator/∞
    (λ (init dt)
       (λ (s)
          (letdelay ((I (stream:cons init (series:+ ((series:* dt) s) I))))
                    I))))

  (define series:integrator
    (λ (init dt)
       (λ§ (s)
           (stream:cons init (stream:dest/car+cdr (s ∅)
                                                  ((scar scdr) ((series:integrator (+ (* dt scar) init) dt) scdr)))))))

  (define series:ode-solver-1st
    (λ (integral)
       (λ (f)
          (letdelay ((y (integral dy))
                     (dy ((stream:map f) y)))
                    y))))

  (define series:ode-solver-2nd
    (λ (integral-1st integral-2nd)
       (λ (f)
          (letdelay ((y (integral-1st dy))
                     (dy (integral-2nd ddy))
                     (ddy ((stream:zip-with f) y dy)))
                    y))))




  (define Pythagorean-triples
    (λ (n)
       (let ((F (λ (triple)
                   (equal?
                     (+ (expt (car triple) n) (expt (cadr triple) n))
                     (expt (caddr triple) n))))
             (nats (series:from 1))
             (slow-enumeration stream:enumerate-upper)
             (fast-enumeration (stream:enumerate-weighted
                                 (λ (i j #!optional (k 0))
                                    ((○ abs -)
                                     (+ (expt i n) (expt j n))
                                     (expt k n))))
                               ))
         ((stream:filter F) (fast-enumeration nats nats nats)))))

  (define random-numbers
    (let ((rand-update (λ (u) (pseudo-random-integer 1000))))
      (λ (init)
         (letdelay ((R (stream:cons init ((stream:map rand-update) R))))
                   R))))

  (define series:Montecarlo
    (λ (tosses)
       (letrec ((MC (λ§ (t s u)
                        (let ((N (λ (s u)
                                    (stream:cons (/ s (+ s u)) (MC (stream:cdr t) s u)))))
                          (cond
                            ((stream:car t) (N (add1 s) u))
                            (else (N s (add1 u))))))))
         (MC tosses 0 0))))

  (define riordan-array
    (λ§ (d h)
        ; a finite d is a polynomial, read with its trailing zeros: dcdr is zero-padded, and an
        ; exhausted d (d·h is one when h is) is series:0, so that the rows never end
        (stream:dest/car+cdr d
                             ((dcar dcdr) (stream:cons
                                            (list dcar)
                                            ((stream:zip-with cons pad: 0) dcdr (riordan-array (series:× d h) h))))
                             (else (riordan-array series:0 h)))))

  (define formalvar-series
    (λ§ (n)
        (cond
          ((zero? n) stream:1)
          (else (stream:cons 0 (formalvar-series (sub1 n)))))))

  (define series:◇
    (λ§ (α)
        (stream:dest/car+cdr (α ∅)
                             ((α₀ αs) (cond
                                        ((equal? α₀ 0) (letdelay ((R (stream:cons 0 (series:⁻¹ (series:○ αs R)))))
                                                                 R))
                                        (else (error "series:◇" "α₀ not zero" α₀)))))))

  (define divisable-by?
    (λ (y)
       (λ (x)
          (equal? (remainder x y) 0))))

  (define eratosthenes
    (λ§ (s)
        (stream:dest/car+cdr s
                             ((prime primes) (stream:cons prime (eratosthenes
                                                                  ((stream:filter
                                                                     (compose not (divisable-by? prime))) primes))))
                             (else (error "Primes are infinite.")))))

  (define fibonacci/stateful
    (λ§ (a b)
        (stream:cons a (fibonacci/stateful b (+ a b)))))

  (define powers-of/∞
    (λ (n)
       (letdelay ((α (stream:cons 1 ((stream:zip-with *) α (stream:repeat n)))))
                 α)))

  (define multiples-of (○ stream:filter divisable-by?))

  (define not-multiples-of
    (λ (p)
       (stream:filter (○ not (divisable-by? p)))))

  (define evens (stream:filter even?))
  (define odds (stream:filter odd?))

  (define series:range/0-9 (series:range 0 10))

  (define numbers/nats
    (series:from 0))

  (define numbers/nats>0
    (stream:cdr numbers/nats))

  (define numbers/nats>1
    (stream:cdr numbers/nats>0))

  (define numbers/triangular
    ((stream:map (accumulator + 0)) numbers/nats))

  (define numbers/fibonacci
    (fibonacci/stateful 0 1))

  (define numbers/fibonacci>0
    (stream:cdr numbers/fibonacci))

  (define numbers/fibonacci-triangular
    ((stream:map (accumulator + 0)) numbers/fibonacci))

  (define-delay numbers/nats/∞
    (stream:cons 0 ((stream:zip-with +) numbers/nats/∞ stream:1s)))

  (define-delay numbers/fibs/∞
    (:⁺ 0 1 ((stream:zip-with +) numbers/fibs/∞ (stream:cdr numbers/fibs/∞))))
  ;(define-delay numbers/fibs/∞ (stream:cons 0 (stream:cons 1 ((stream:zip-with +) numbers/fibs/∞ (stream:cdr numbers/fibs/∞)))))

  (define-delay numbers/lucas/∞
    (:⁺ 2 1 ((stream:zip-with +) numbers/lucas/∞ (stream:cdr numbers/lucas/∞))))

  (define numbers/fibs>0/∞
    (stream:cdr numbers/fibs/∞))

  (define numbers/triangular/+scan
    ((stream:scan +) numbers/nats/∞))

  (define numbers/fibs/+scan
    ((stream:scan +) numbers/fibs/∞))

  (define numbers/fibs/∙scan
    ((stream:scan *) (stream:cdr numbers/fibs/∞)))

  (define numbers/powers-of-2
    (powers-of/∞ 2))

  (define numbers/powers-of-3
    (powers-of/∞ 3))

  (define primes/eratosthenes
    (eratosthenes numbers/nats>1))

  (define-delay primes/∞
    (let ((prime? (λ (n)
                     (letrec ((P (λ (α)
                                    (let ((p (stream:car α)))
                                      (cond
                                        ((> p (sqrt n)) #t)
                                        (((divisable-by? p) n) #f)
                                        (else (P (stream:cdr α))))))))
                       (P primes/∞))))) ; here is the "whole meal" chunck
      (stream:cons 2 ((stream:filter prime?) (series:from 3)))))

  (define-delay numbers/factorials/∞
    (stream:cons 1 ((stream:zip-with *) numbers/factorials/∞ numbers/nats>0)))

  (define-delay numbers/central-polygonal/∞
    (stream:cons 1 ((stream:zip-with +) numbers/central-polygonal/∞ numbers/nats>0)))

  (define-delay numbers/triangular/∞
    (stream:cons 0 ((stream:zip-with +) numbers/triangular/∞ numbers/nats>0)))

  (define numbers/powers-of-2/∞
    (stream:cons 1 ((stream:zip-with +) numbers/powers-of-2/∞ numbers/powers-of-2/∞)))

  (define numbers/powers-of-3/∞
    (stream:cons 1 ((stream:zip-with +)
                    numbers/powers-of-3/∞
                    numbers/powers-of-3/∞
                    numbers/powers-of-3/∞)))

  (define max-dividing-power
    (λ (n)
       (λ (p)
          (letrec ((M (λ (n c)
                         (let-values (((q r) (quotient&remainder n p)))
                           (cond
                             ((zero? r) (M q (add1 c))) ; tail-call so no need to use `call/cc`
                             (else c))))))
            (M n 0)))))

  (define radix-expansion
    (λ§ (num den radix)
        (let-values (((q r) (quotient&remainder (* num radix) den)))
          (stream:cons q (radix-expansion r den radix)))))

  (define factorization
    (λ§ (n)
        ((stream:map (max-dividing-power n)) primes/∞)))

  (define prime?
    (λ (n)
       (letrec ((P (λ (α)
                      (stream:dest/car+cdr (α ∅)
                                           ((α₀ α⁺) (cond
                                                      ((equal? n α₀) #t)
                                                      ((< n α₀) #f)
                                                      (else (P α⁺))))))))
         (P primes/∞))))

  (define-delay ; mutually recursive definition
    (taylor/cosine (series:+ stream:1 ((series:* -1) (series:∫ taylor/sine))))
    (taylor/sine (series:∫ taylor/cosine)))

  (define-delay taylor/exponential
    (series:+ stream:1 (series:∫ taylor/exponential)))

  (define-delay taylor/catalan
    (stream:cons 1 (series:× taylor/catalan taylor/catalan)))

  (define-delay taylor/fibonacci
    (let ((t (formalvar-series 1)))
      (stream:cons 1 (series:+
                       taylor/fibonacci
                       (series:× t taylor/fibonacci)))))

  (define taylor/π
    (letrec ((summands (λ§ (n sign) ; carrying the sign, not a stream:map - per term, keeps it linear
                           (stream:cons (* sign (⁻¹ n)) (summands (+ n 2) (- sign))))))
      ((series:* 4) ((stream:scan +) (summands 1 1)))))


  )

