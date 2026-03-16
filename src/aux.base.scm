
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

  (define-syntax-rule (define-macro (name (inject (bi i) ...) (compare (bl l) ...)) (pattern body ...) ...)
    (define-macro-ir (name expr inject* compare)
      (let* ((inject (λ symbols (inject* (apply symbols->symbol/stripped-syntax symbols))))
             (bi (inject i)) ...
             (bl (λ (x) (compare x l))) ...)
        (match/first expr (pattern body ...) ...))))

  (define (symbols->symbol/stripped-syntax . symbols) (apply symbol-append (map strip-syntax symbols)))

  (define-syntax-rule (begin1 expr body ...) (let1 (v expr) (begin body ... v)))
  (define-syntax-rule (define-many (name ...) (value ...)) (begin (define name value) ... (void)))


  ; match/non-overlapping --------------------------------------------------------------------------

  (define-syntax-rule (match/non-overlapping v (e ...) ...) 
    (dmatch-run-a-thunk (quote v) v (dmatch-remexp v (e ...) ...)))

  (define-syntax-rule (match1/non-overlapping (pat v) body ...) (match/non-overlapping v (pat body ...)))

  (define-syntax-rule (λ/non-overlapping (e ...) ...) (λ (arg) (match/non-overlapping arg (e ...) ...)))

  (define-syntax-rule (λ1/non-overlapping pat body ...) (λ/non-overlapping (pat body ...)))

  (define-record dmatch-pkg clause thunk)

  (define-syntax dmatch-remexp
    (syntax-rules ()
      ((dmatch-remexp (rator rand ...) cls ...) (let1 (v (rator rand ...)) (dmatch-aux v cls ...)))
      ((dmatch-remexp v cls ...) (dmatch-aux v cls ...))))

  (define-syntax dmatch-aux
    (syntax-rules (⇒)
      ((dmatch-aux v) '())
      ((dmatch-aux v (pat g ⇒ e ...) cls ...)
        (let1 (fk (τ (dmatch-aux v cls ...)))
          (match-pattern v pat (if g (cons (make-dmatch-pkg (quote (pat g ⇒ e ...)) (τ e ...)) (fk)) (fk)) (fk))))
      ((dmatch-aux v (pat e ...) cls ...) (dmatch-aux v (pat #t ⇒ e ...) cls ...))))

  (define (dmatch-run-a-thunk v-expr v pkgs)
    (cond
      ((null? pkgs) (error (string-append "match/non-overlapping\n\n" 
                            (->string/pretty-print `((reason "no match found") (expr ,v-expr) (value ,v))))))
      ((null? (cdr pkgs)) (let1 (t (dmatch-pkg-thunk (car pkgs))) (t)))
      (else (error (string-append "match/non-overlapping\n\n" 
                    (->string/pretty-print `((reason "overlapping match")
                                             (expr ,v-expr) 
                                             (value ,v) 
                                             (ambiguities ,(map dmatch-pkg-clause pkgs)))))))))

  ; match/first --------------------------------------------------------------------------

  (define-syntax-rule (match/first exp clause ...) 
    (let1 (val-to-match exp) (match-case-simple* val-to-match clause ...)))

  (define-syntax-rule (match1/first (pat v) body ...) (match/first v (pat body ...)))

  (define-syntax-rule (λ-match/first e ...) (λ (arg) (match/first arg e ...)))

  (define-syntax-rule (λ-match1/first pat body ...) (λ-match/first (pat body ...)))

  (define-syntax match-case-simple*
    (syntax-rules (else ⇒)
      ((match-case-simple* val (else exp ...)) (begin exp ...))
      ((match-case-simple* val) (match-case-simple* val
                                  (else (error (string-append "match/first: uncaught value.\n\n" 
                                                              (->string/pretty-print val))))))
      ((match-case-simple* val (pattern guard ⇒ exp ...) clause ...)
        (let1 (fk (τ (match-case-simple* val clause ...)))
          (match-pattern val pattern (if guard (begin exp ...) (fk)) (fk))))
      ((match-case-simple* val (pattern exp ...) clause ...) 
        (match-case-simple* val (pattern #t ⇒ exp ...) clause ...))))

  (define-syntax match-pattern
    (syntax-rules (_ __ unquote as)
      ((match-pattern val _ kt kf) kt)
      ((match-pattern val __ kt kf) kf)
      #;((match-pattern val #() kt kf) (if (and (vector? val) (zero? (vector-length val))) kt kf))
      ((match-pattern val () kt kf) (if (or (null? val) (and (vector? val) (zero? (vector-length val)))) kt kf))
      ((match-pattern val (e as var) kt kf) (match-pattern val e (let1 (var (quasiquote e)) kt) kf))
      ((match-pattern val (unquote var) kt kf) (let1 (var val) kt))
      #;((match-pattern val #(x x* ...) kt kf)
        (cond
          ((pair? val) 
            (let ((valx (car val)) (valy (cdr val)))
              (match-pattern valx x (match-pattern valy #(x* ...) kt kf) kf)))
          ((and (vector? val) (> (vector-length val) 0))
            (let ((valx (vector-ref val 0)) (valy (subvector val 1)))
              (match-pattern valx x (match-pattern valy #(x* ...) kt kf) kf)))
          ((record-instance? val)
            (let* ((val* (record->vector val)) (valx (vector-ref val* 0)) (valy (subvector val* 1)))
              (match-pattern valx x (match-pattern valy #(x* ...) kt kf) kf)))
          (else kf)))
      ((match-pattern val (x . y) kt kf)
        (cond
          ((pair? val) 
            (let ((valx (car val)) (valy (cdr val)))
              (match-pattern valx x (match-pattern valy y kt kf) kf)))
          ((and (vector? val) (> (vector-length val) 0))
            (let ((valx (vector-ref val 0)) (valy (subvector val 1)))
              (match-pattern valx x (match-pattern valy y kt kf) kf)))
          ((record-instance? val)
            (let* ((val* (record->vector val)) (valx (vector-ref val* 0)) (valy (subvector val* 1)))
              (match-pattern valx x (match-pattern valy y kt kf) kf)))
          (else kf)))
      ((match-pattern val lit kt kf) (if (equal? val (quote lit)) kt kf))))

  ; -------------------------------------------------------------------------------------------------

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
  (define-syntax-rule (λ_ body ...) (λ useless body ...))
  (define-syntax μ
    (syntax-rules ()
      ((μ () body ...) (λ (v) body ...))
      ((μ (v) body ...) (λ (v) body ...))
      ((μ (v v* ...) body ...) (λ (v) (μ (v* ...) body ...)))
      ((μ v body ...) (μ (v) body ...))))
  (define-syntax-rule (τ body ...) (λ () body ...))
  (define-syntax-rule (define-τ name body ...) (define name (τ body ...)))
  (define-syntax-rule (letgensym (var ...) body ...) (let ((var (gensym)) ...) body ...))
  (define-syntax-rule (letport/output-string p body ...) (call-with-output-string (λ (p) body ...)))

  (define (interaction-environment/symbols syms) 
    (let1 (env0 (interaction-environment))
      (map (λ (s) (cons s (eval s env0))) syms)))

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
      ((lettensor f ((x expr) e ...) body ...) (f (μ x (lettensor f (e ...) body ...)) expr))))

  (define-syntax-rule (letmaptensor ((x expr) ...) body ...) (lettensor map ((x expr) ...) body ...))

  (define-syntax letmap
    (syntax-rules ()
      ((letmap () body ...) (list (begin body ...)))
      ((letmap ((x expr) e ...) body ...) (append-map (μ x (letmap (e ...) body ...)) expr))))

  (define (member? v lst) (pair? (member v lst)))

  (define-syntax letassoc 
    (syntax-rules (else) 
      ((letassoc (searchexpr lstexpr) (else body ...))
       (let1 (p (assoc searchexpr lstexpr)) (if (pair? p) (cadr p) (begin body ...))))))

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

  (define (map/curry f) (μ lst (map f lst)))

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















