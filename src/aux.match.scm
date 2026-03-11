
(module (aux match) *

  (import scheme 
          (chicken base)
          (chicken string)
          (chicken memory representation)
          (aux base))

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
          (dmatch-ppat v pat (if g (cons (make-dmatch-pkg (quote (pat g ⇒ e ...)) (τ e ...)) (fk)) (fk)) (fk))))
      ((dmatch-aux v (pat e ...) cls ...) (dmatch-aux v (pat #t ⇒ e ...) cls ...))))

  (define-syntax dmatch-ppat
    (syntax-rules (unquote _ __)
      ((dmatch-ppat vv () kt kf) (let1 (v vv) (if (or (null? v) (and (vector? v) (zero? (vector-length v)))) kt kf)))
      ((dmatch-ppat vv (unquote (unquote (unquote v))) kt kf) (if (eq? v vv) kf kt))
      ((dmatch-ppat vv (unquote (unquote v)) kt kf) (let ((vv* vv) (v* v))
                                                      (cond
                                                        ((procedure? v*) (if (v* vv*) kt kf))
                                                        ((eq? v* vv*) kt)
                                                        (else kf))))
      ((dmatch-ppat vv (unquote v) kt kf) (let1 (v vv) kt))
      ((dmatch-ppat vv (x . y) kt kf)
        (let1 (v vv)
          (cond
            ((pair? v) (dmatch-ppat (car v) x (dmatch-ppat (cdr v) y kt kf) kf))
            ((and (vector? v) (> (vector-length v) 0)) (dmatch-ppat (vector-ref v 0) x (dmatch-ppat (subvector v 1) y kt kf) kf))
            ((record-instance? v) (let1 (r (record->vector v))
                                    (dmatch-ppat (vector-ref r 0) x (dmatch-ppat (subvector r 1) y kt kf) kf)))
            (else kf))))
      ((dmatch-ppat vv _ kt kf) kt)
      ((dmatch-ppat vv __ kt kf) kf)
      ((dmatch-ppat v lit kt kf) (if (equal? v (quote lit)) kt kf))))

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

  ; simple --------------------------------------------------------------------------

  (define-syntax-rule (match/first exp clause ...) 
    (let1 (val-to-match exp) (match-case-simple* val-to-match clause ...)))

  (define-syntax-rule (match1/first (pat v) body ...) (match/first v (pat body ...)))

  (define-syntax-rule (λ-match/first e ...) (λ (arg) (match/first arg e ...)))

  (define-syntax-rule (λ-match1/first pat body ...) (λ-match/first (pat body ...)))

  (define-syntax match-case-simple*
    (syntax-rules (else ⇒)
      ((match-case-simple* val (else exp ...)) (begin exp ...))
      ((match-case-simple* val) (match-case-simple* val (else (error (string-append "match/first\n\n" (->string/pretty-print val))))))
      ((match-case-simple* val (pattern guard ⇒ exp ...) clause ...)
        (let1 (fk (τ (match-case-simple* val clause ...)))
          (match-pattern val pattern (if guard (begin exp ...) (fk)) (fk))))
      ((match-case-simple* val (pattern exp ...) clause ...) (match-case-simple* val (pattern #t ⇒ exp ...) clause ...))))

  (define-syntax match-pattern
    (syntax-rules (_ __ unquote let)
      ((match-pattern val _ kt kf) kt)
      ((match-pattern val __ kt kf) kf)
      ((match-pattern val () kt kf) (if (null? val) kt kf))
      ((match-pattern val #() kt kf) (if (and (vector? val) (zero? (vector-length val))) kt kf))
      ((match-pattern val (let var e) kt kf) (match-pattern val e (let1 (var (quasiquote e)) kt) kf))
      ((match-pattern val (unquote var) kt kf) (let1 (var val) kt))
      ((match-pattern val (x . y) kt kf) 
        (cond
          ((pair? val) (let ((valx (car val)) (valy (cdr val)))
                          (match-pattern valx x 
                            (match-pattern valy y kt kf) kf)))
          (else kf)))
      ((match-pattern val #(x x* ...) kt kf) 
        (cond
          ((and (vector? val) (> (vector-length val) 0))
            (match-pattern (vector-ref val 0) x
              (match-pattern (subvector val 1) #(x* ...) kt kf) kf))
          (else kf)))
      ((match-pattern val lit kt kf) (if (equal? val (quote lit)) kt kf))))

)