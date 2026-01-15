
(module (aux base) *

  (import scheme 
          (chicken base) 
          (chicken continuation) 
          (chicken pretty-print) 
          (chicken memory representation) 
          (chicken fixnum) 
          (chicken sort) 
          (chicken port) 
          (chicken foreign)
          (chicken syntax)
          (chicken module)
          (chicken string)
          srfi-1
          srfi-69
          vector-lib)

  (reexport matchable)

  (define-syntax let1 
    (syntax-rules () 
      ((let1 (var val) body ...) (let ((var val)) body ...))
      ((let1 var body ...) (let1 (var (void)) body ...))))

  (define-syntax define-syntax-rule
    (syntax-rules (literal)
      ((define-syntax-rule (literal k ...) (name p ...) r) (define-syntax name (syntax-rules (k ...) ((_ p ...) r))))
      ((define-syntax-rule (name p ...) r) (define-syntax-rule (literal) (name p ...) r))))

  (define-syntax-rule (define-macro-ir (name expr inject compare) body ...)
    (define-syntax name (ir-macro-transformer (λ (expr inject compare) body ...))))

  (define-syntax-rule (define-macro-er (name expr rename compare) body ...)
    (define-syntax name (er-macro-transformer (λ (expr rename compare) body ...))))

  (define-syntax-rule (define-macro (name (inject (bi i) ...) (compare (bl l) ...)) ((pattern ...) body ...) ...)
    (define-macro-ir (name expr inject* compare)
      (let* ((inject (λ symbols (inject* (apply symbols->symbol/stripped-syntax symbols))))
             (bi (inject i)) ...
             (bl (λ (x) (compare x l))) ...)
        (match expr ((pattern ...) body ...) ...))))

  (define (symbols->symbol/stripped-syntax . symbols) (apply symbol-append (map strip-syntax symbols)))

  ; ------------------------------------------------------------------------------------------------
  ; dmatch: pattern matching with dajkstra's algorithm for efficient matching.
  ; ------------------------------------------------------------------------------------------------

  (define-syntax match/guarded
    (syntax-rules ()
      ((match/guarded v (e ...) ...) (match/guarded v "" (e ...) ...))
      ((match/guarded v name (e ...) ...) (dmatch-run-a-thunk (quote v) v 
                                            (string-append " for " (->string name)) 
                                            (dmatch-remexp v (e ...) ...)))))

  (define-syntax-rule (dmatch-pkg pat g e0 e ...) (cons (quote (pat g e0 e ...)) (τ e0 e ...)))
  (define dmatch-pkg-clause car)
  (define dmatch-pkg-thunk cdr)

  (define-syntax dmatch-remexp
    (syntax-rules ()
      ((dmatch-remexp (rator rand ...) cls ...) (let1 (v (rator rand ...)) (dmatch-aux v cls ...)))
      ((dmatch-remexp v cls ...) (dmatch-aux v cls ...))))

  (define-syntax dmatch-aux
    (syntax-rules (guard)
      ((dmatch-aux v) '())
      ((dmatch-aux v (pat (guard g ...) e0 e ...) cls ...)
        (let1 (fk (τ (dmatch-aux v cls ...)))
          (dmatch-ppat v pat
            (cond 
              ((not (and g ...)) (fk))
              (else (cons (dmatch-pkg pat (guard g ...) e0 e ...) (fk))))
            (fk))))
      ((dmatch-aux v (pat e0 e ...) cls ...)
        (let1 (fk (τ (dmatch-aux v cls ...)))
          (dmatch-ppat v pat
            (cons (dmatch-pkg pat (guard ) e0 e ...) (fk))
            (fk))))))

  (define-syntax dmatch-ppat
    (syntax-rules (unquote)
      ((dmatch-ppat v (unquote var) kt kf) (let1 (var v) kt))
      ((dmatch-ppat vv () kt kf) (let1 (v vv) (if (or (null? v) (and (vector? v) (zero? (vector-length v)))) kt kf)))
      ((dmatch-ppat vv (x . y) kt kf)
        (let1 (v vv)
          (cond
            ((pair? v) (let ((vx (car v)) 
                             (vy (cdr v))) 
                        (dmatch-ppat vx x (dmatch-ppat vy y kt kf) kf)))
            ((and (vector? v) (> (vector-length v) 0)) (let ((vx (vector-ref v 0))
                                                             (vy (subvector v 1)))
                                                          (dmatch-ppat vx x (dmatch-ppat vy y kt kf) kf)))
            ((record-instance? v) (let* ((r (record->vector v))
                                         (vx (vector-ref r 0))
                                         (vy (subvector r 1)))
                                    (dmatch-ppat vx x (dmatch-ppat vy y kt kf) kf)))
            (else kf))))
      ((dmatch-ppat v lit kt kf) (if (equal? v (quote lit)) kt kf))))

  (define (dmatch-run-a-thunk v-expr v name pkg∗)
    (cond
      ((null? pkg∗) (error 'match/guarded
                      (string-append "no match found" name) 
                      `((expr ,v-expr) (eval ,v))))
      ((null? (cdr pkg∗)) ((dmatch-pkg-thunk (car pkg∗))))
      (else (error 'match/guarded
              (string-append "overlapping match" name) 
              `((expr ,v-expr) (eval ,v) (ambiguities ,(map dmatch-pkg-clause pkg∗)))))))

  #|

  (define-syntax-rule (v-aux (v #(x ...)) body ...) (let1 (v (list x ...)) body ...))

  (v-aux (v #(1 2 3)) (cons 0 v))

  (define-syntax-rule (define-many (name ...) (value ...))
    (begin
      (define name value) ...
      (void)))

  (match/guarded '() (() 'empty))
  (match/guarded #() (() 'empty))
  (match/guarded '() (,r r))
  (match/guarded #() (,r r))
  (match/guarded '(p) ((,r) r))
  (match/guarded #(p) ((p) #t))
  (match/guarded #(p) ((,r) r))
  (match/guarded #(3 2) ((,r 2) r))
  (match/guarded #(3 2) ((,r . ,s) (list r s)))
  (match/guarded #(3 2) ((,r 2 ,t) r))
  (match/guarded #(3 2) ((,r ,e) r))
  (match/guarded (make-record-instance 'hello 3 2) ((hello ,r ,e) r))
  

  (define-many (a b d) (0  4 2))

  (list a b d) ; ⇒ (1 2 3)

  (import scheme (chicken base) (aux base) (chicken pretty-print) (chicken memory representation) (chicken string))

  (define h
    (lambda (x y)
      (match/guarded `(,x . ,y) "h function, example"
        ((,a . ,b) (guard (number? a) (number? b)) (* a b))
        ; ((,a . ,b) (+ a b))
        ((,a ,b ,c) (guard (number? a) (number? b) (number? c)) (+ a b c)))))

  (list (h 3 4) (apply h '(1 (3 4)))) 

  (list (h 3 'j) (apply h '(1 (3 4)))) 

  |#

  ; ------------------------------------------------------------------------------------------------

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

  (define-syntax-rule (push! val var) (begin (set! var (cons val var)) (void)))
  (define-syntax-rule (pop! var) (let ((a (car var))) (set! var (cdr var)) a))
  (define-syntax-rule (append-right! lst another ... var) (begin (set! var (append var lst another ...)) (void)))
  (define-syntax-rule (add1! var) (begin (set! var (add1 var)) (void)))
  (define-syntax-rule (sub1! var) (begin (set! var (sub1 var)) (void)))
  (define-syntax-rule (λ formals body ...) (lambda formals body ...))
  (define-syntax-rule (λ_ body ...) (λ _ body ...))
  (define-syntax-rule (τ body ...) (λ () body ...))
  (define-syntax-rule (define-τ name body ...) (define name (τ body ...)))
  (define-syntax-rule (letgensym (var ...) body ...) (let ((var (gensym)) ...) body ...))
  (define-syntax-rule (letport/output-string p body ...) (call-with-output-string (λ (p) body ...)))

  (define-syntax letcar&cdr
    (syntax-rules ()
      ((letcar&cdr () body ...) (begin body ...))        
      ((letcar&cdr (((a d) expr) ((aa dd) eexpr) ...) body ...)
       (let* ((x expr) (a (car x)) (d (cdr x)))
         (letcar&cdr (((aa dd) eexpr) ...) body ...)))))

  (define-syntax define-let
    (syntax-rules ()
      ((define-let ((v e) ...) (name formal ...) body ...)
       (define name (let ((v e) ...) (lambda (formal ...) body ...))))
      ((define-let ((v e) ...) name body ...)
       (define name (let ((v e) ...) body ...)))))

  ; ------------------------------------------------------------------------------------------------
  ; Documentation framework
  ; ------------------------------------------------------------------------------------------------

  (define-syntax-rule (define-documented (name body) (docf (tag docbody) ...))
    (begin
      (define name body)
      (set! (docf name) (list (string->symbol "name") (quote name)))
      (set! (docf name) (list (string->symbol "def") (quote body)))
      (set! (docf name) (list (quote tag) docbody)) ...
      (void)))

  (define documentation-hash-table (make-hash-table))

  (define (documentation func)
    (let1 (M (λ (f d)
              (let1 (info (cond
                            ((procedure? f) (procedure-information f))
                            (else f)))
                (list info d))))
      (M func (reverse (hash-table-ref/default documentation-hash-table func '())))))

  (set! (setter documentation)
    (λ (f d) 
      (hash-table-update!/default documentation-hash-table f (λ (lst) (cons d lst)) '())))
                      
  (define (documentation! key tag doc) (set! (documentation key) (list tag doc)))

  (documentation! documentation 'sxml `(p "This function is the fundamental documentation storage and retrieval mechanism."))

  ; ------------------------------------------------------------------------------------------------
  ; Miscellaneous utilities
  ; ------------------------------------------------------------------------------------------------

  (define-syntax lettensor
    (syntax-rules ()
      ((lettensor f () body ...) (begin body ...))
      ((lettensor f ((x expr) (xx exprr) ...) body ...) 
       (f (lambda (x) (lettensor f ((xx exprr) ...) body ...)) expr))))

  (define-syntax-rule (letmaptensor ((x expr) ...) body ...) (lettensor map ((x expr) ...) body ...))

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

  (define-syntax letassoc/cdr
    (syntax-rules (else) 
      ((letassoc (searchexpr lstexpr) (else body ...))
       (let1 (p (assoc searchexpr lstexpr))
             (if (pair? p) (cdr p) (begin body ...))))))

  (define (mappair f lst)
    (cond
      ((or (null? lst) (null? (cdr lst))) '())
      (else (cons (f (car lst) (cadr lst)) (mappair f (cdr lst))))))

  (define (curry f g) (λ args (apply f (cons g args)))) 


  (define (foldr/yielded f t init)
    (cond
      ((null? t) init)
      (else (letcar&cdr (((v k) t))
                        (f v (foldr/yielded f (k (void)) init))))))

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

  (define-syntax-rule (λ-memo args body ...) (let ((memo (make-hash-table))
                                                    (f (λ args body ...)))
                                                (λ vargs
                                                    (unless (hash-table-exists? memo vargs) 
                                                      (hash-table-set! memo vargs (apply f vargs)))
                                                    (hash-table-ref memo vargs))))

  (define-syntax-rule (define-memo (name arg ...) body ...) (define name (λ-memo (arg ...) body ...)))

  (define ((boolean->P prob) bool) (if bool prob (- 1 prob)))

  (define boolean->01 (boolean->P 1))

  (define ((indicator set) v) (member? v set))

  (define (pairwise-different? lst)
    (cond
      ((null? lst) #t)  ; Empty list, all elements are trivially different
      ((member? (car lst) (cdr lst)) #f)  ; First element is found in the rest of the list
      (else (pairwise-different? (cdr lst)))))  ; Recur on the rest of the list

  (define one? (λ (n) (equal? n 1)))
  (define void? (λ (v) (eq? v (void))))

  ; the SKI combinators.
  (define K (λ (x) (λ_ x)))
  (define K* (λ keeps (λ_ (apply values keeps))))
  (define (((S x) y) z) (x z (y z)))
  (define (((S* x) y) . zs) (apply x (append zs (list (apply y zs)))))
  (define (((S⁺ x) y) . zs) (apply x (cons (apply y zs) zs)))

  (define Φ (λ (f) (f f)))
  (define Y (λ (f) (Φ (λ (g) (f (λ args (apply (Φ g) args)))))))

  (define curry₁ (λ (f) (λ (g) (λ args (apply f (cons g args))))))

  (define-syntax λ-curry
    (syntax-rules ()
     ((λ-curry () body ...) (λ (useless) body ...))
     ((λ-curry (arg) body ...) (λ (arg) body ...))
     ((λ-curry (arg args ...) body ...) (λ (arg) (λ-curry (args ...) body ...)))))
  (define-syntax-rule (define-curry (name arg ...) body ...) (define name (λ-curry (arg ...) body ...)))

  (define (load/string str) (read (open-input-string str)))
  (define (->string/pretty-print v) (call-with-output-string (λ (p) (pretty-print v p))))
  (define (pretty-printer/port f) (λ (v port) (pretty-print (f v) port)))

  (define (string-last s) (string-ref s (sub1 (string-length s))))

  (define (foldr/add lst) (foldr + 0 lst))
  (define (foldr/times lst) (foldr * 1 lst))
  (define (foldr/avg lst) (/ (foldr/add lst) (length lst)))
  (define (foldr/var lst)
    (let1 (m (foldr/avg lst))
          (/ (foldr (λ (x acc) (let1 (d (- x m)) (+ (* d d) acc))) 0 lst)
             (sub1 (length lst)))))
  (define (foldr/stddev lst) (sqrt (foldr/var lst)))
  (define (foldr/concat lst) (foldr append '() lst))
  (define (foldr/concat-strings lst) (foldr string-append "" lst))
  (define (foldr/max lst) (foldr (λ (a b) (if (> a b) a b)) -inf.0 lst))
  (define (foldr/min lst) (foldr (λ (a b) (if (< a b) a b)) +inf.0 lst))
  (define (foldr/and lst) (foldr (λ (a b) (and a b)) #t lst))
  (define (foldr/or lst) (foldr (λ (a b) (or a b)) #f lst))

  )















