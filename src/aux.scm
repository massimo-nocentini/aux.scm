
(module aux *

  (import scheme 
	  (chicken base) 
	  (chicken continuation) 
	  (chicken pretty-print) 
	  (chicken fixnum) 
	  (chicken sort) 
	  (chicken port) 
    (chicken foreign) 
	  srfi-1
	  srfi-69)

  (define-syntax letport/string 
    (syntax-rules (out else) 
      ((_ (p out) body ...) (let* ((v (void))
                                   (s (call-with-output-string (λ (p) (set! v (begin body ...))))))
                              (values v s)))
      ((_ (p instring) body ...) (let* ((v (void))
                                        (s (call-with-input-string instring (λ (p) (set! v (begin body ...))))))
                                   (values v s)))
      ((_ else body ...) (let* ((v (void))
				(s (with-error-output-to-string (τ (set! v (begin body ...))))))
			   (values v s)))))

  (define-syntax push! 
    (syntax-rules () 
      ((push! val var) (begin (set! var (cons val var)) (void)))))

  (define-syntax pop! (syntax-rules () ((pop! var) (let ((a (car var))) (set! var (cdr var)) a))))
  (define-syntax append-right! 
    (syntax-rules () 
      ((append-right! lst another ... var) (begin (set! var (append var lst another ...)) (void)))))
  (define-syntax add1! (syntax-rules () ((_ var) (begin (set! var (add1 var)) (void)))))
  (define-syntax sub1! (syntax-rules () ((_ var) (begin (set! var (sub1 var)) (void)))))

  (define-syntax λ (syntax-rules () ((λ formals body ...) (lambda formals body ...))))
  (define-syntax λ/_ (syntax-rules () ((λ/_ body ...) (λ formals body ...))))
  (define-syntax τ (syntax-rules () ((τ body ...) (λ () body ...))))
  (define-syntax define-τ (syntax-rules () ((define-τ name body ...) (define name (τ body ...)))))
  (define-syntax letgensym (syntax-rules () ((letgensym (var ...) body ...) (let ((var (gensym)) ...) body ...))))

  (define-syntax letcar&cdr
    (syntax-rules ()
      ((letcar&cdr () body ...) (begin body ...))        
      ((letcar&cdr (((a d) expr) ((aa dd) eexpr) ...) body ...)
       (let* ((x expr) (a (car x)) (d (cdr x)))
         (letcar&cdr (((aa dd) eexpr) ...) body ...)))))

  (define-syntax define-let
    (syntax-rules ()
      ((define-let ((v e) ...) (name formal ...) body ...)
       (define name (let ((v e) ...) (lambda (formal ...) body ...))))))

  (define-syntax lettensor
    (syntax-rules ()
      ((lettensor f () body ...) (begin body ...))
      ((lettensor f ((x expr) (xx exprr) ...) body ...) 
       (f (lambda (x) (lettensor f ((xx exprr) ...) body ...)) expr))))

  (define-syntax letmaptensor
    (syntax-rules () ((letmaptensor ((x expr) ...) body ...) (lettensor map ((x expr) ...) body ...))))

  (define-syntax letmap
    (syntax-rules ()
      ((letmap () body ...) (list (begin body ...)))
      ((letmap ((x expr) (xx exprr) ...) body ...) 
       (apply append (map (lambda (x) (letmap ((xx exprr) ...) body ...)) expr)))))

  (define (member? v lst) (pair? (member v lst)))

  (define-syntax letassoc 
    (syntax-rules (else) 
      ((letassoc (searchexpr lstexpr) (else body ...))
       (let1 (p (assoc searchexpr lstexpr))
             (if (pair? p) (cadr p) (begin body ...))))))

  (define (mappair f lst)
    (cond
      ((or (null? lst) (null? (cdr lst))) '())
      (else (cons (f (car lst) (cadr lst)) (mappair f (cdr lst))))))

  (define (curry f g)
   (λ args (apply f (cons g args)))) 

  (define-syntax cons§ (syntax-rules () ((_ a d) (delay (cons a d)))))

  (define-syntax let1 
    (syntax-rules () 
      ((let1 (var val) body ...) (let ((var val)) body ...))
      ((let1 var body ...) (let1 (var (void)) body ...))))

  (define-syntax letcc
    (syntax-rules ()
      ((letcc hop body ...) (continuation-capture (lambda (cont) 
                                                    (let1 (hop (lambda (arg) (continuation-return cont arg)))
                                                          body ...))))))

  (define (callcc f) (letcc k (f k)))

  (define-syntax resetcc (syntax-rules () ((resetcc body ...) (callcc (delimcc-reset (τ body ...))))))
  (define-syntax letcc/shift (syntax-rules () ((letcc/shift k body ...) (callcc (delimcc-shift (lambda (k) body ...))))))
  (define (callshiftcc f) (letcc/shift k (f k)))
  (define-values (delimcc-reset delimcc-shift)
    (let1 (cont (lambda (v) v)) ;(let1 (cont (lambda (v) (error "Missing enclosing resetcc" v)))
          (define (delimcc-abort v) (cont v))
          (define (delimcc-cont-push! k) (set! cont k))
          (define (delimcc-cont-pop!) cont)
          (define ((resetk t) k)
            (let1 (s (delimcc-cont-pop!))
                  (delimcc-cont-push! (lambda (r)
                                        (delimcc-cont-push! s)
                                        (k r)))
                  (delimcc-abort (t))))
          (define ((shiftk h) k) (delimcc-abort (h (lambda (v) (resetcc (k v))))))
          (values resetk shiftk)))

  (define-syntax define-resetcc (syntax-rules () ((define-resetcc def body ...) (define def (resetcc body ...)))))
  (define (delimcc-extract) (letcc/shift k k))
  (define (delimcc-discard v) (letcc/shift _ v))
  (define-syntax delimcc-τ 
    (syntax-rules () 
      ((delimcc-τ body ...) (letcc/shift k (τ (let1 (x (begin body ...)) (k x)))))))
  (define-syntax delimcc-lambda 
    (syntax-rules () 
      ((delimcc-lambda args body ...) (letcc/shift k (lambda args (let1 (x (begin body ...)) (k x)))))))
  (define (delimcc-either lst) (letcc/shift k (map k lst)))

  (define-syntax resetcc+null (syntax-rules () ((resetcc+null body ...) (resetcc body ... '()))))
  (define (yield v) (letcc/shift k (cons v (k (void)))))
  (define (yield/extract v) (letcc/shift k (cons v k)))
  (define-syntax yield§ (syntax-rules () ((yield§ body) (letcc/shift k (cons§ body (k (void)))))))
  (define (yield§/a v) (yield§ v))

  (define (map§/yielded f t)
    (cond
      ((null? t) '())
      (else (letcar&cdr (((v k) t))
			(cons§ (f v) (map§/yielded f (k (void))))))))

  (define (foldr/yielded f t init)
    (cond
      ((null? t) init)
      (else (letcar&cdr (((v k) t))
                        (f v (foldr/yielded f (k (void)) init))))))

  (define-syntax delimcc-foldr
    (syntax-rules ()
      ((delimcc-fold bexpr ((each acc) fbody ...) body ...)
       (let* ((witness (gensym))
              (b bexpr)
              (f (lambda (each acc) fbody ...)))
         (define (L r)
           (cond
             ((eq? r witness) b)
             (else (f (car r) (L (let1 (k (cdr r)) (k (void))))))))
         (L (resetcc body ... witness))))))

  (define-syntax delimcc-foldr§
    (syntax-rules ()
      ((delimcc-fold body ...)
       (let1 (witness (gensym))
             (define (L r)
               (cond
                 ((eq? r witness) '())
                 (else (cons§ (car r) (L (let1 (k (cdr r)) (k (void))))))))
             (L (resetcc body ... witness))))))

  (define-syntax letcc*
    (syntax-rules ()
      ((letcc* next ((v exp ...) ...) body ...)
       (letcc success
              (let* ((v (letcc next (success (begin exp ...)))) ...
                     (next success))
                body ...)))))

  (define-syntax trycc
    (syntax-rules (else)
      ((trycc (next exp ...) (else body ...))
       (letcc* hop ((v (let1 (next (τ (hop (void)))) exp)) ...)
               body ...))))

  (define (§->list s)
    (cond
      ((promise? s) (§->list (force s)))
      ((pair? s) (cons (car s) (§->list (cdr s))))
      (else s)))

  (define (take§ n s)
    (cond
      ((zero? n) '())
      ((promise? s) (delay (take§ n (force s))))
      ((pair? s) (cons§ (car s) (take§ (sub1 n) (cdr s))))
      (else s)))

  (define (map§ f s)
    (cond
      ((promise? s) (delay (map§ f (force s))))
      ((pair? s) (cons§ (f (car s)) (map§ f (cdr s))))
      (else s)))

  (define (filter§ p s)
    (cond
      ((promise? s) (delay (filter§ p (force s))))
      ((pair? s) (letcar&cdr (((a d) s)) (if (p a) (cons§ a (filter§ p d)) (delay (filter§ p d)))))
      (else s)))

  (define (zip§ f r s)
    (cond
      ((and (promise? r) (promise? s)) (delay (zip§ f (force r) (force s))))
      ((promise? r) (delay (zip§ f (force r) s)))
      ((promise? s) (delay (zip§ f r (force s))))
      ((and (pair? r) (pair? s)) (cons§ (f (car r) (car s)) (zip§ f (cdr r) (cdr s))))
      (else '())))

  (define (cdr§ s)
    (cond
      ((promise? s) (cdr§ (force s)))
      ((pair? s) (cdr s))
      (else (void))))

  (define (car§ s)
    (cond
      ((promise? s) (car§ (force s)))
      ((pair? s) (car s))
      (else (void))))

  (define interleave§
    (lambda list-of-choices
      (cond
        ((null? list-of-choices) '())
        (else (letcar&cdr (((choices rest-of-choices) list-of-choices))
                          (cond
                            ((null? choices) (apply interleave§ rest-of-choices))
                            ((promise? choices) (delay (apply interleave§ (append rest-of-choices (list (force choices))))))
                            ((pair? choices) (cons§ (car choices) (apply interleave§ (append rest-of-choices (list (cdr choices))))))
                            (else '())))))))

  (define (const§ s) (rec N (cons§ s N)))
  (define (nats§ s) (rec N (cons§ s (map§ add1 N))))
  (define ℕ (nats§ 0))
  (define (gfibs§ f m n) (rec F (cons§ m (cons§ n (zip§ f F (cdr§ F))))))
  (define (fibs§ m n) (gfibs§ + m n))
  (define primes§
    (let P ((s (nats§ 2)))
      (let1 (p (car§ s)) 
            (cons§ p (P (filter§ (lambda (n) (not (zero? (modulo n p)))) (cdr§ s)))))))

  (define 1§ (const§ 1))

  (define (τ->§ t) (cons§ (t) (τ->§ t)))
  (define-syntax thunk§ (syntax-rules () ((thunk§ body ...) (τ->§ (τ body ...)))))

  (define (stop§ pred? §)
    (cond
      ((promise? §) (delay (stop§ pred? (force §))))
      ((or (null? §) (pred? (car §))) '())
      (else (cons§ (car §) (stop§ pred? (cdr§ §))))))

  (define ((nondeterministic system) cc)
    (letrec ((pool '())
             (stats (cons 0 0)) ; `car` counts accepted, `cdr` counts tried.
             (P (make-hash-table))
             (describe (τ
                         (letcar&cdr (((accepted tried) stats))
                                     `((left ,(length pool))
                                       (tried ,tried)
                                       (accepted ,accepted)
                                       (ratio ,(exact->inexact (/ accepted tried)))
                                       (distribution ,(sort (hash-table-map P (λ (value count) 
                                                                                  (list value (exact->inexact (/ count accepted)))))
                                                            (λ (a b) (> (cadr a) (cadr b)))))))))
             (fail (τ
                     (cond
                       ((null? pool) (cc (describe)))
                       (else (let1 (t (pop! pool))
                                   (cond
                                     ((procedure? t) (set-cdr! stats (add1 (cdr stats))) (t))
                                     (else (fail))))))))
             (chooseD (lambda (choices #!optional (continue? (λ (v) #t)))
                        (cond
                          ((null? choices) (fail))
                          ((promise? choices) (chooseD (force choices) continue?))
                          ((pair? choices) (letcc kk
                                                  (let1 (v (car choices))
                                                        (when (continue? v) 
							  (push! (τ (kk (chooseD (cdr choices) continue?))) pool))
                                                        (push! (τ (kk v)) pool))
                                                  (fail)))
                          (else (fail)))))
             (chooseB (lambda (choices)
                        (cond
                          ((promise? choices) (letcc kk
                                                     (append-right! (list (τ (kk (chooseB (force choices))))) pool)
                                                     (chooseB '())))
                          ((pair? choices) (letcc kk
                                                  (append-right! (list (τ (kk (car choices))) 
								       (τ (kk (chooseB (cdr choices))))) 
								 pool)
                                                  (chooseB '())))
                          (else (fail)))))
             (markD (τ (letgensym (flag) (push! flag pool) flag)))
             (cutD (lambda (flag)
                     (cond
                       ((null? pool) (void))
                       (else (let1 (a (pop! pool))
                                   (if (eq? a flag) (void) (cutD flag)))))))
             (⊦ (lambda (bool) (unless bool (fail)))))

      (set-cdr! stats (add1 (cdr stats)))
      (let1 (v (system chooseD chooseB ⊦ markD cutD))
            (set-car! stats (add1 (car stats)))
            (hash-table-update!/default P v add1 0)
            (yield§ v)
            (fail))))

  (define-syntax letnondeterministic§
    (syntax-rules ()
      ((letnondeterministic§ (chooseD chooseB asserter markD cutD) body ...)
       (resetcc+null
         (pretty-print (callcc (nondeterministic (λ (chooseD chooseB asserter markD cutD) body ...))))))))

  (define-syntax letnondeterministic
    (syntax-rules ()
      ((_ (chooseD chooseB asserter markD cutD) body ...)
       (letnondeterministic -1 (chooseD chooseB asserter markD cutD) body ...))
      ((_ nr (chooseD chooseB asserter markD cutD) body ...)
       (§->list (take§ nr (letnondeterministic§ (chooseD chooseB asserter markD cutD) body ...))))))

  (define (memoize/call f)
    (let ((called #f) (memo (void)))
      (λ args
          (unless called 
            (set! memo (apply f args))
            (set! called #t))
          memo)))

  (define (memoize/arg f)
    (let1 (memo (make-hash-table))
          (λ (arg)
              (unless (hash-table-exists? memo arg) (hash-table-set! memo arg (f arg)))
              (hash-table-ref memo arg))))

  (define-syntax λ-memo
    (syntax-rules ()
      ((_ args body ...) (let ((memo (make-hash-table))
                               (f (λ args body ...)))
                           (λ vargs
                               (unless (hash-table-exists? memo vargs) 
                                 (hash-table-set! memo vargs (apply f vargs)))
                               (hash-table-ref memo vargs))))))
  (define-syntax define-memo
    (syntax-rules ()
      ((_ (name arg ...) body ...) (define name (λ-memo (arg ...) body ...)))))

  (define ((boolean->P prob) bool) (if bool prob (- 1 prob)))

  (define boolean->01 (boolean->P 1))

  (define ((indicator set) v) (member? v set))

  (define (pairwise-different? lst)
    (cond
      ((null? lst) #t)  ; Empty list, all elements are trivially different
      ((member? (car lst) (cdr lst)) #f)  ; First element is found in the rest of the list
      (else (pairwise-different? (cdr lst)))))  ; Recur on the rest of the list

  (foreign-declare "#include \"chicken-timsort.h\"")

  (define timsort-foreign 
    (foreign-safe-lambda scheme-object "C_timsort" scheme-object size_t scheme-object scheme-object bool bool bool bool int))

  (define ((timsort/gen lt? inplace reverse use-insertion-sort be-unpredictable-on-random-data comparator_type) lst) 
    (let* ((size 0)
           (new-lst (map (λ (each) (add1! size)) lst)))
      (timsort-foreign lst size lt? new-lst inplace reverse use-insertion-sort be-unpredictable-on-random-data comparator_type)))

  (define TIMSORT_USE_COMPARATOR (foreign-value "TIMSORT_USE_COMPARATOR" int))
  (define TIMSORT_USE_LESS_THAN (foreign-value "TIMSORT_USE_LESS_THAN" int))

  (define timsort (timsort/gen < #f #f #f #t TIMSORT_USE_COMPARATOR))
  (define timsort! (timsort/gen < #t #f #f #t TIMSORT_USE_COMPARATOR))
  (define timtros (timsort/gen < #f #t #f #t TIMSORT_USE_COMPARATOR))
  (define timtros! (timsort/gen < #t #t #f #t TIMSORT_USE_COMPARATOR))

  (define timsort/primitive (timsort/gen < #f #f #f #t TIMSORT_USE_LESS_THAN))
  (define timsort/primitive! (timsort/gen < #t #f #f #t TIMSORT_USE_LESS_THAN))
  (define timtros/primitive (timsort/gen < #f #t #f #t TIMSORT_USE_LESS_THAN))
  (define timtros/primitive! (timsort/gen < #t #t #f #t TIMSORT_USE_LESS_THAN))

)
 

