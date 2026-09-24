
; (aux commons): the shared prelude of the old `on-scheme` repository, ported to CHICKEN 6.
;
; Only the helpers that (aux base) does not already provide live here; the ones that
; have an (aux base) equivalent are *not* redefined (see the table below), because in
; CHICKEN 6 defining an imported name overwrites the imported binding for every importer.
;
;   commons            (aux base) replacement
;   -------            ----------------------
;   λ τ Φ Y one?       same name
;   curry₁             curry₁
;   let₁               let1
;   K                  K (one argument) or K*
;   $                  $ (one function)
;   member?            member? (two arguments)
;   push!              push!, with the arguments swapped: (push! val var)
;   undefined?         void?
;   display-on-port    display/port
;   match₁             match1/first, with `,x` patterns
;   memoize            memoize! in (aux tabling)
;   remove-duplicates  remove-duplicates/last, here (the aux one keeps another order)
;   test-fail          ⊭ in (aux unittest)

(module (aux commons) *

  (import scheme
          (chicken base)
          (chicken port)
          (chicken sort)
          (only (scheme base) open-output-string get-output-string)
          (only srfi-1 filter every)
          srfi-69
          (aux base))

  (define ○ compose)

  ; curried list helpers ---------------------------------------------------------------------------

  (define fmap    (curry₁ map))
  (define fapply  (curry₁ apply))
  (define ffilter (curry₁ filter))
  (define fsort
    (λ (key ⊂)
      (λ (s)
        (sort s (λ (p q) (⊂ (key p) (key q)))))))
  (define flist-ref (curry₁ list-ref))
  (define fvector-ref (curry₁ vector-ref))
  (define equals-to? (curry₁ equal?))
  (define =to?
    (λ (x #!key (same? equal?))
      (λ (y)
        (same? x y))))

  (define identity*
    (λ args
      (cond
        ((one? (length args)) (car args))
        (else args))))

  (define collect-values
    (λ (thunk)
      (call-with-values thunk identity*)))

  (define eternity
    (λ args
      (apply eternity args)))

  (define-syntax cond/λ
    (syntax-rules (else)
      ((cond/λ v
         (p? then) ...
         (else otherwise))
       (cond
         ((p? v) (then v)) ...
         (else (otherwise v))))))

  (define map/with-index
    (λ (f s)
      (λ (lst)
        (letrec ((M (λ (l n)
                      (cond
                        ((null? l) '())
                        (else (cons
                                ((f n) (car l))
                                (M (cdr l) (add1 n))))))))
          (M lst s)))))

  (define map/call-with-values
    (λ (producer consumer)
      (fmap (λ (a)
              (call-with-values (τ (producer a)) consumer)))))

  (define map/values
    (λ (producer)
      (map/call-with-values producer identity*)))

  (define map/tree
    (λ (f)
      (letrec ((T (λ (sexp)
                    (cond
                      ((null? sexp) '())
                      ((pair? (car sexp)) (cons (T (car sexp)) (T (cdr sexp))))
                      (else (cons (f (car sexp)) (T (cdr sexp))))))))
        T)))

  (define accumulator
    (λ (f s)
      (let1 (acc s)
        (λ (x)
          (set! acc (f x acc))
          acc))))

  (define foldl1
    (λ (f #!key (H₀ identity))
      (λ (l)
        (cond
          ((null? l) '())
          (else (foldl f (H₀ (car l)) (cdr l)))))))

  (define foldl1/lshift
    (λ (f #!key (H₀ identity))
      (λ (l)
        (cond
          ((null? l) '())
          (else (let-values (((v tail) (H₀ (car l))))
                  (foldl f v (tail l))))))))

  (define group
    (λ (key post)
      (λ (l)
        (let* ((H (make-hash-table))
               (U (λ (l₀)
                    (let1 (k (key l₀))
                      (cond
                        ((hash-table-exists? H k) (hash-table-set! H k (cons l₀ (hash-table-ref H k))))
                        (else                     (hash-table-set! H k (list l₀))))))))
          (for-each U l)
          (map post (hash-table->alist H))))))

  (define tuple/pred?
    (λ (pred?)
      (letrec ((P (λ tuples
                    (cond
                      ((apply pred? (map car tuples))
                       (let1 (cdrs (map cdr tuples))
                         (cond
                           ((every null? cdrs) #t)
                           ((every (○ not null?) cdrs) (apply P cdrs))
                           (else #f))))
                      (else #f)))))
        P)))

  (define within?
    (λ (m M)
      (λ (n)
        (<= m n M))))

  ; reflexive and transitive closure of `→`, up to an `eq?` fixed point.
  (define rtc
    (λ (→)
      (letrec ((→* (λ (s α)
                     (let1 (r (→ s))
                       (cond
                         ((eq? s r) α) ; fixed-point found
                         (else (→* r (cons r α))))))))
        (λ (s)
          (reverse (→* s (list s)))))))

  ; Keeps the *last* occurrence of each element and returns them in reverse order, that is
  ; `'(a b a a c d c e e)` gives `'(e c d a b)`; (aux base) `remove-duplicates` keeps the first
  ; occurrences in order instead.
  (define remove-duplicates/last
    (λ (lst)
      (letrec ((R (λ (lst set)
                    (cond
                      ((null? lst) set)
                      (else (let ((a (car lst)) (d (cdr lst)))
                              (cond
                                ((member a d) (R d set))
                                (else (R d (cons a set))))))))))
        (R lst '()))))

  ; numeric and string odds ------------------------------------------------------------------------

  (define sub2 (○ sub1 sub1))

  (define ≠
    (λ (v w)
      (not (equal? v w))))

  (define ⁻¹
    (λ (x) (/ 1 x)))

  (define ²
    (λ (x) (* x x)))

  (define number->symbol (○ string->symbol number->string))

  (define to-string
    (λ (v)
      (with-output-to-string (τ (display v)))))

  (define call+stdout
    (λ (thunk recv)
      (let* ((str-port (open-output-string))
             (result (with-output-to-port str-port thunk)))
        (recv result (get-output-string str-port)))))

  (define subscripts
    (let1 (H (make-hash-table))
      (hash-table-set! H #\0 "₀")
      (hash-table-set! H #\1 "₁")
      (hash-table-set! H #\2 "₂")
      (hash-table-set! H #\3 "₃")
      (hash-table-set! H #\4 "₄")
      (hash-table-set! H #\5 "₅")
      (hash-table-set! H #\6 "₆")
      (hash-table-set! H #\7 "₇")
      (hash-table-set! H #\8 "₈")
      (hash-table-set! H #\9 "₉")
      H))

  (define symbol∼
    (λ (H)
      (let* ((M (λ (n) (hash-table-ref/default H n (string n))))
             (-> (○ string->list symbol->string))
             (<- (○ string->symbol (fapply string-append)))
             (<-> (○ <- (fmap M) ->)))
        <->)))

  (define symbol∼subscripts (symbol∼ subscripts))

  )
