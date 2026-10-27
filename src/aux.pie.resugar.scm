;;; (aux pie resugar) -- Core terms back into the surface syntax that Pie prints.
;;;
;;; A port of resugar.rkt of Pie, the language of "The Little Typer" by Daniel P. Friedman and David Thrane
;;; Christiansen (https://github.com/the-little-typer/pie, commit 2c89553). Pie is distributed under the
;;; GNU AGPL-3.0, and so is this file, which is derived from it.
;;;
;;; The file is ported whole and line for line, with the upstream names. Only `resugar` is exported, as
;;; upstream provides only it (cf. resugar.rkt:6). Resugaring turns zero and add1 chains into numbers, a
;;; Π whose variable does not occur in its body into →, a Σ whose variable does not occur in its second
;;; type into Pair, nested λ, Π and Σ into one form with several binders, (TODO ...) into TODO, and the
;;; one-argument applications of Core back into applications with many arguments. To decide when a
;;; binder is needed, resugar* answers a pair whose car is the list of the variables that occur free in
;;; the term (with repetitions) and whose cdr is the resugared term.
;;;
;;; The upstream quirks are kept on purpose, and they are
;;;   - the patterns (:: hd (nil)) and (vec:: hd (vecnil)) and the four-argument replace never match a
;;;     Core term, because Core writes nil and vecnil as bare symbols and replace with three arguments;
;;;   - the clause for vec:: tests for a resugared tail headed by list, not by vec, so vec:: chains are
;;;     never turned into a single vec;
;;;   - add-?, quoted-symbol?, legal-symbol?, all-symbol-char? and symbol-char? are defined and unused.
;;; Racket's match becomes match/first of (aux base) clause by clause, in the same order. A quoted
;;; pattern `',sym is (quote ,sym), (? symbol? x) and #:when become ⊣ guards, and symbols that end in a
;;; colon are written with bars, |::| and |vec::|, because CHICKEN reads them as keywords otherwise.

(module (aux pie resugar) (resugar)

  (import scheme
          (chicken base)
          (only (scheme base) exact-integer?)
          (aux base)
          (only (aux pie basics) var-name?)
          (only (aux pie racket) racket-char-alphabetic?))

  ;;; ==================== RESUGARING ====================

  ;; resugar : Core -> surface s-expression, cf. resugar.rkt:8
  (define (resugar ll-term)
    (cdr (resugar* ll-term)))

  ;; resugar* : Core -> (Pair (Listof Symbol) surface s-expression), the free variables of the term and
  ;; the resugared term, cf. resugar.rkt:11
  (define (resugar* ll-term)
    (match/first ll-term
      (U
       (cons '() 'U))
      ((TODO . _)
       (cons '() 'TODO))
      (zero
       (cons '() 0))
      ((add1 ,n)
       (let ((resugared-n (resugar* n)))
         (cons (car resugared-n)
               (if (or (eqv? (cdr resugared-n) 0)
                       (positive-number? (cdr resugared-n)))
                   (add1 (cdr resugared-n))
                   `(add1 ,(cdr resugared-n))))))
      ((quote ,sym)
       (cons '() `',sym))
      ((,x ⊣ (symbol? x))
       (if (pie-keyword? x)
           (cons '() x)
           (cons `(,x) x)))
      ((λ (,x) ,result)
       (let ((resugared (resugar* result)))
         (cons (remv* `(,x) (car resugared))
               (add-λ x (cdr resugared)))))
      ((Π ((,x ,arg-type)) ,result-type)
       (let ((arg (resugar* arg-type))
             (res (resugar* result-type)))
         (if (memv x (car res))
             (cons (append (car arg) (remv* `(,x) (car res)))
                   (add-Π x (cdr arg) (cdr res)))
             (cons (append (car arg) (car res))
                   (add--> (cdr arg) (cdr res))))))
      ((Σ ((,x ,car-type)) ,cdr-type)
       (let ((a-t (resugar* car-type))
             (d-t (resugar* cdr-type)))
         (if (memv x (car d-t))
             (cons (append (car a-t) (remv* `(,x) (car d-t)))
                   (add-Σ x (cdr a-t) (cdr d-t)))
             (cons (append (car a-t) (car d-t))
                   `(Pair ,(cdr a-t) ,(cdr d-t))))))
      ; dead in Core, where nil is a bare symbol, cf. resugar.rkt:52
      ((|::| ,hd (nil))
       (let ((hd-out (resugar* hd)))
         (cons (car hd-out)
               `(list ,(cdr hd-out)))))
      ((|::| ,hd ,tl)
       (let ((resugared-hd (resugar* hd))
             (resugared-tl (resugar* tl)))
         (cons (append (car resugared-hd) (car resugared-tl))
               (if (and (pair? (cdr resugared-tl))
                        (eqv? (car (cdr resugared-tl))
                              'list))
                   `(list ,(cdr resugared-hd) . ,(cdr (cdr resugared-tl)))
                   `(|::| ,(cdr resugared-hd) ,(cdr resugared-tl))))))
      ; dead in Core, where vecnil is a bare symbol, cf. resugar.rkt:65
      ((|vec::| ,hd (vecnil))
       (let ((hd-out (resugar* hd)))
         (cons (car hd-out)
               `(vec ,(cdr hd-out)))))
      ; the test is for a tail headed by list, as upstream, cf. resugar.rkt:73
      ((|vec::| ,hd ,tl)
       (let ((resugared-hd (resugar* hd))
             (resugared-tl (resugar* tl)))
         (cons (append (car resugared-hd) (car resugared-tl))
               (if (and (pair? (cdr resugared-tl))
                        (eqv? (car (cdr resugared-tl))
                              'list))
                   `(vec ,(cdr resugared-hd) . ,(cdr (cdr resugared-tl)))
                   `(|vec::| ,(cdr resugared-hd) ,(cdr resugared-tl))))))
      ; dead in Core, where replace has three arguments, cf. resugar.rkt:78
      ((replace ,t ,tgt ,mot ,base)
       (let ((resugared-t (resugar* t))
             (resugared-tgt (resugar* tgt))
             (resugared-mot (resugar* mot))
             (resugared-base (resugar* base)))
         (cons (append (car resugared-t) (car resugared-tgt) (car resugared-mot) (car resugared-base))
               `(replace (the ,(cdr resugared-t) ,(cdr resugared-tgt))
                         ,(cdr resugared-mot)
                         ,(cdr resugared-base)))))
      ; the type t of the Core cong is dropped, cf. resugar.rkt:87
      ((cong ,tgt ,t ,fun)
       (let ((resugared-tgt (resugar* tgt))
             (resugared-fun (resugar* fun)))
         (cons (append (car resugared-tgt) (car resugared-fun))
               `(cong ,(cdr resugared-tgt)
                      ,(cdr resugared-fun)))))
      (((,kw . ,args) ⊣ (pie-keyword? kw))
       (let ((resugared-args (map (λ (arg) (resugar* arg)) args)))
         (cons (apply append (map car resugared-args))
               `(,kw . ,(map cdr resugared-args)))))
      ; an application whose operator is itself an application gets one more argument, cf. resugar.rkt:98
      ((,op ,arg)
       (let ((resugared-op (resugar* op))
             (resugared-arg (resugar* arg)))
         (cons (append (car resugared-op) (car resugared-arg))
               (cond ((not (pair? (cdr resugared-op)))
                      `(,(cdr resugared-op) ,(cdr resugared-arg)))
                     ((pie-keyword? (car (cdr resugared-op)))
                      `(,(cdr resugared-op) ,(cdr resugared-arg)))
                     (else
                      (append (cdr resugared-op) `(,(cdr resugared-arg))))))))
      ; Resugaring should never crash, no matter how odd the code is, cf. resugar.rkt:108
      (,any-term (cons '() any-term))))

  ;; remv* : (Listof Any) (Listof Any) -> (Listof Any), Racket's remv*, the elements of lst that are not
  ;; eqv? to any element of v-lst, in order, cf. resugar.rkt:34
  (define (remv* v-lst lst)
    (cond ((null? lst) '())
          ((memv (car lst) v-lst) (remv* v-lst (cdr lst)))
          (else (cons (car lst) (remv* v-lst (cdr lst))))))

  ;;; ==================== BINDING FORMS ====================

  ;; add-λ : Symbol Term -> Term, x in front of the arguments of a λ, or a new λ, cf. resugar.rkt:111
  (define (add-λ x term)
    (match/first term
      ((λ ,xs ,result)
       `(λ (,x . ,xs) ,result))
      (,non-λ
       `(λ (,x) ,non-λ))))

  ;; add-Π : Symbol Term Term -> Term, cf. resugar.rkt:118
  (define (add-Π x arg-type term)
    (match/first term
      ((Π ,args ,result-type)
       `(Π ((,x ,arg-type) . ,args) ,result-type))
      (,non-Π
       `(Π ((,x ,arg-type)) ,non-Π))))

  ;; add--> : Term Term -> Term, cf. resugar.rkt:125
  (define (add--> arg-type term)
    (match/first term
      ((→ . ,types)
       `(→ ,arg-type . ,types))
      (,non-->
       `(→ ,arg-type ,non-->))))

  ;; add-Σ : Symbol Term Term -> Term, cf. resugar.rkt:132
  (define (add-Σ x arg-type term)
    (match/first term
      ((Σ ,args ,result-type)
       `(Σ ((,x ,arg-type) . ,args) ,result-type))
      (,non-Σ
       `(Σ ((,x ,arg-type)) ,non-Σ))))

  ;; add-? : Symbol Term Term -> Term, unused upstream too, cf. resugar.rkt:139
  (define (add-? x hole-type term)
    (match/first term
      ((? ,args ,body)
       `(? ((,x ,hole-type) . ,args) ,body))
      (,non-?
       `(? ((,x ,hole-type)) ,non-?))))

  ;;; ==================== PREDICATES ====================

  ;; positive-number? : Any -> Boolean, Racket's exact-positive-integer?, cf. resugar.rkt:147
  (define (positive-number? x)
    (and (exact-integer? x) (positive? x)))

  ;; quoted-symbol? : Any -> Boolean, unused upstream too, cf. resugar.rkt:150
  (define (quoted-symbol? term)
    (match/first term
      ((quote ,x) (legal-symbol? x))
      (,non-x #f)))

  ;; legal-symbol? : Any -> Boolean, cf. resugar.rkt:155
  (define (legal-symbol? sym)
    (and (symbol? sym)
         (all-symbol-char? (string->list (symbol->string sym)))))

  ;; all-symbol-char? : (Listof Char) -> Boolean, with the cond of upstream that has no else,
  ;; cf. resugar.rkt:159
  (define (all-symbol-char? chars)
    (cond
      ((null? chars)
       #t)
      ((symbol-char? (car chars))
       (all-symbol-char? (cdr chars)))
      ((not (symbol-char? (car chars)))
       #f)))

  ;; symbol-char? : Char -> Boolean, a letter (racket-char-alphabetic?, Racket's char-alphabetic?) or a
  ;; dash, cf. resugar.rkt:168
  (define (symbol-char? char)
    (or (racket-char-alphabetic? char)
        (char=? char #\-)))

  ;; pie-keyword? : Any -> Boolean, a symbol that is not a variable name, cf. resugar.rkt:172
  (define (pie-keyword? x)
    (and (symbol? x) (not (var-name? x)))))
