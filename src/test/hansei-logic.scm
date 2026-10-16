; The suite of HANSEI as a declarative logic programming language, over (aux hansei) and (aux
; hansei parsec): the append relation, scheduling, the bridge puzzle, reversible type inference
; and reversible parser combinators (logic_vars.ml, daily_schedule.ml, puzzle.ml,
; type_inference.ml, parsec.ml).  Cases G1-G13 of the plan AUX-HANSEI-PLAN.md §8.5 (WP7, WP9).
; Expected values are upstream's, as exact rationals of its floats where the model's choices are
; exact, re-sorted in the Scheme order (descending mass, ties in descending pmap-compare key
; order, plan D5); probcc-reify/part answers in that order too (deviation 5), where LV:68-70's
; fold reverses explore's.

; Two CHICKEN hazards for stochastic code (plan §5.6, D11 and D12), which every model of this
; file avoids:
;   - `map` is not re-entrant: CHICKEN's `map` (and srfi-1's) builds its result destructively, so
;     resuming a continuation captured inside its procedure corrupts the list, e.g.
;     (probcc-reify/exact (map (λ (x) (probcc-coin 1/2)) '(1 2))) answers the single
;     entry (#t #t #f #f #t #f) at mass 1, the four worlds sharing one list built in place.
;     Models use probcc-map, left to right as OCaml's List.map, never map, vector-map or
;     for-each; nor srfi-1's list-tabulate and filter-map, which are re-entrant but call their
;     procedure from the last element to the first.
;   - under csi, resuming a continuation captured while the operands of a call with five or more
;     operands are evaluated accumulates arguments, even with plain call/cc, e.g.
;     (probcc-reify/exact (list 'a 'b 'c 'd (probcc-coin 1/2))) answers (a b c d #t) and
;     (a b c d #t #f); quasiquote templates and constructors (list, vector) are calls too, and
;     the position of the choice does not matter.  Models bind every choice with let* or let1
;     before using it in such a call.  Compiled code is correct.
; OCaml evaluates the components of a tuple, the arguments of a constructor (`::` too) and of a
; call, and the operands of `^`, from right to left, and `let x = e1 and y = e2 in` from left to
; right (D19); the scrutinee of `match (e1, e2) with` is not built as a tuple, and its
; components are evaluated from left to right (LV:172).  The ported models sequence them with
; let*, as their comments say.  Exact tables do not depend on that order; the bounded
; explorations and the replays may.

(import scheme
        (chicken base)
        (chicken flonum)
        (only srfi-1 fold filter make-list)
        (aux base)
        (aux unittest)
        (aux hansei)
        (aux hansei parsec))

; replays and bit-exact checks print 17 digits: with the default 15, a one-ulp failure would print
; identical expected and got values (plan §8 conventions).
(flonum-print-precision 17)

; (new) the tree pv with every unexplored branch ((C t) p) written (C p), to compare a bounded
; exploration structurally, its thunks aside.
(define (shape pv)
  (probcc-map (λ1-match/first
                (((C ,_) ,p) `(C ,p))
                (,e e))
              pv))

;; -----------------------------------------------------------------------------------------
;; Models of logic_vars.ml (LV) and of the Prolog paper (RT)
;; -----------------------------------------------------------------------------------------

; cf. LV:22-27 `bl`, `blist`, `nil`, `cons`: a boolean list with a non-deterministic spine, a
; thunk answering '() (`Nil`) or the pair (h . blist) (`Cons (h,t)`).
(define (bl-nil) '())
(define ((bl-cons h t)) (cons h t))

; cf. LV:33-37 `list_of_blist`: the list of a blist, forced from its head.
(define (blist->list bl)
  (match/first (bl)
    (() '())
    ((,h . ,t) (cons h (blist->list t)))))

; cf. LV:40-44 `append`, seemingly a function: the tail of the result is the thunk of the
; append of the rest, so l1 is forced only as far as the result is.
(define (bl-append l1 l2)
  (match/first (l1)
    (() l2)
    ((,h . ,t) (bl-cons h (τ ((bl-append t l2)))))))

; cf. LV:47-48, 168: the sample lists [t,t,t], [f,f] and [t,t,t,f,f].
(define bl-t3 (bl-cons #t (bl-cons #t (bl-cons #t bl-nil))))
(define bl-f2 (bl-cons #f (bl-cons #f bl-nil)))
(define bl-t3f2 (bl-append bl-t3 bl-f2))

; cf. LV:106 `a_boolean`: a `logic variable' over the booleans, a choice made when the variable
; is first needed, the same afterwards in its world.  LV:19 makes letlazy letlazy_nesting, for
; the soft-cut of prog3.
(define (a-boolean) (probcc-letlazy/nesting (τ (probcc-coin 1/2))))

; cf. LV:108-111 `a_blist`, over a letlazy given (LV:19 letlazy_nesting, upstream's; the plain
; probcc-letlazy for G2): a logic variable over the boolean lists.  The list `[(0.5, Nil); (0.5,
; Cons (flip 0.5, a_blist ()))]` is built before dist chooses, from the right (D19): the tail
; variable is made and the head flipped in every world, also where the list is empty.
(define (a-blist/with letlazy)
  (letlazy (τ (let* ((t (a-blist/with letlazy))
                     (h (probcc-coin 1/2)))
                (probcc-distribution `((() 1/2) ((,h . ,t) 1/2)))))))

(define (a-blist) (a-blist/with probcc-letlazy/nesting))

; cf. LV:171-177 `bl_compare`: are two blists equal, forced pairwise from the head?  The
; scrutinee (l1 (), l2 ()) of a match is evaluated from the left, l1 first (D19).
(define (bl-compare l1 l2)
  (let* ((c1 (l1))
         (c2 (l2)))
    (match/first (list c1 c2)
      ((() ()) #t)
      (((,h1 . ,t1) (,h2 . ,t2)) (and (eq? h1 h2) (bl-compare t1 t2)))
      (else #f))))

; cf. LV:222-227 `last`: last(E,L) :- append(_,[E],L); the arguments of append from the right,
; e forced first (D19).
(define (bl-last bl)
  (let* ((x (a-blist))
         (e (a-boolean))
         (l (bl-cons (e) bl-nil)))
    (unless (bl-compare (bl-append x l) bl) (probcc-impossible))
    e))

; cf. LV:278-282 `del`: l without an occurrence of b, del([H|T],H,T) and del([H|T],D,[H|R]) :-
; del(T,D,R) as a function: the first occurrence, failing on the empty list.
(define (bl-del l b)
  (match/first (l)
    (() (probcc-impossible))
    ((,h . ,t) (if (eq? h b) t (bl-cons h (bl-del t b))))))

; cf. LV:324-328 `list_haslen`: l has n elements, or the world fails.
(define (bl-haslen l n)
  (match/first (l)
    ((() ⊣ (= n 0)) (void))
    (((,_ . ,t) ⊣ (> n 0)) (bl-haslen t (sub1 n)))
    (else (probcc-impossible))))

; cf. LV:393-394 `prog3`: Prolog's committed choice, prog3(L,S) :- del(L,true,LRem) ->
; S=deleted(LRem) ; S=failed, by the soft-cut: the list of the remainder (`Some l`), or #f
; (`None`).
(define (bl-prog3 l) (probcc-soft-cut (τ (bl-del l #t)) list (τ #f)))

; (new) cf. LV:397-444, the outcome of prog3 as a value: #f, or the list of the remainder's list.
(define (bl-prog3/value l)
  (match/first (bl-prog3 l)
    (#f #f)
    ((,r) (list (blist->list r)))))

; cf. RT p.5 l.340-346 `a_list`, the cheap generator: an ordinary list of booleans, `flip 0.5 ::
; a_list ()` from the right (D19).
(define (rt-a-list)
  (if (probcc-coin 1/2)
      '()
      (let* ((t (rt-a-list))
             (h (probcc-coin 1/2)))
        (cons h t))))

;; -----------------------------------------------------------------------------------------
;; Models of daily_schedule.ml (DS)
;; -----------------------------------------------------------------------------------------

; cf. DS:35-36 `powerset`: a subset of lst, each element taken or not by a fair coin, consed, so
; in reverse; srfi-1's fold is List.fold_left, left to right, and may be re-entered.
(define (ds-powerset lst) (fold (λ (e acc) (if (probcc-coin 1/2) (cons e acc) acc)) '() lst))

; cf. DS:53 `mustbe` (= Z:49): the proposition x holds in this world, or the world fails.
(define (mustbe x) (unless x (probcc-impossible)))

; cf. DS:56-102 `schedule_model`: the actions of day d, a memoized subset of the four, are a
; logic variable; each constraint is a fact about a day chosen uniformly.  srfi-1's filter is
; left to right as List.filter and may be re-entered; `[d] = List.filter …` from the right.
(define (ds-schedule-model)
  (let* ((ndays 3)
         (actions (probcc-memo (λ (d) (ds-powerset '(Shop Walk Barber Supermarket)))))
         (action-on (λ (a d) (and (memq a (actions d)) #t)))
         (only-on (λ (a d) (equal? (list d) (filter (λ (d*) (action-on a d*)) '(0 1 2))))))
    (let1 (d (probcc-uniform ndays)) (mustbe (only-on 'Shop d)))
    (let1 (d (probcc-uniform ndays)) (mustbe (only-on 'Walk d)))
    (let1 (d (probcc-uniform ndays)) (mustbe (only-on 'Barber d)))
    (let1 (d (probcc-uniform ndays)) (mustbe (only-on 'Supermarket d)))
    (let1 (d (probcc-uniform ndays)) (mustbe (and (action-on 'Supermarket d) (action-on 'Shop d))))
    (let1 (d (probcc-uniform ndays)) (mustbe (and (action-on 'Walk d) (action-on 'Barber d))))
    (let1 (d (probcc-uniform ndays))
      (mustbe (and (action-on 'Walk d) (> d 0) (action-on 'Supermarket (sub1 d)))))
    (mustbe (action-on 'Walk 1))
    (list->vector (probcc-map actions '(0 1 2)))))

;; -----------------------------------------------------------------------------------------
;; Models of puzzle.ml (PZ): the U2 bridge puzzle
;; -----------------------------------------------------------------------------------------

; cf. PZ:94-100 the `Hansei` implementation of `SimpleNonDet`: `choose` is a uniform choice,
; failing on the empty list, as probcc-uniform/either (deviation 7); `run` reifies exactly.  The
; `Fork` implementation (PZ:66-91) is not ported.
(define pz-choose probcc-uniform/either)

; cf. PZ:114-120 `u2` and `speed`.
(define pz-u2 '(Bono Edge Adam Larry))
(define (pz-speed x) (case x ((Bono) 1) ((Edge) 2) ((Adam) 5) ((Larry) 10)))

; (new) OCaml's `x >= y` on the constructors of u2: their declaration order.
(define (pz>=? x y) (<= (length (memq x pz-u2)) (length (memq y pz-u2))))

; cf. PZ:140-144 `select_party`: one or two people to cross, the second not before the first.
(define (pz-select-party side)
  (let* ((p1 (pz-choose side))
         (p2 (pz-choose (filter (λ (x) (pz>=? x p1)) side))))
    (if (eq? p1 p2) (list p1) (list p1 p2))))

; cf. PZ:147-152 `elapsed_time` and `without`.
(define (pz-elapsed-time party) (fold (λ (x z) (max z (pz-speed x))) 0 party))
(define (pz-without l side) (filter (λ (x) (not (memq x l))) side))

; cf. PZ:168-181 `loop`: the crossings so far, newest first, as (party forward); the sides are
; (from to), swapped at each crossing.  Upstream prints the trace of a solution; here it is
; the value of the world.
(define (pz-loop trace forward time-left sides)
  (match/first sides
    (((() ,_) ⊣ forward) (reverse trace))
    (((,_ ()) ⊣ (not forward)) (reverse trace))
    ((,from ,to)
     (let* ((party (pz-select-party from))
            (elapsed (pz-elapsed-time party)))
       (when (> elapsed time-left) (probcc-impossible))
       (let* ((from* (pz-without party from))
              (to* (append to party)))
         (pz-loop (cons (list party forward) trace) (not forward) (- time-left elapsed)
                  (list to* from*)))))))

;; -----------------------------------------------------------------------------------------
;; Models of type_inference.ml (TI): type inference, forwards and backwards
;; -----------------------------------------------------------------------------------------

; cf. TI:16-28: a term is a thunk answering (I n), (V name), (L name term) or (A term term),
; so that it may be read on demand, as a stream; its constructors.
(define ((ti-var x)) (list 'V x))
(define ti-vf (ti-var "f"))
(define ti-vx (ti-var "x"))
(define ti-vz (ti-var "z"))
(define ((ti-app e1 e2)) (list 'A e1 e2))
(define ((ti-lam v t)) (list 'L v t))
(define ((ti-num n)) (list 'I n))

; cf. TI:32-42 `paren` and `string_of_term`, with no extra parentheses; `^` is right
; associative and its operands are evaluated from the right (D19), the argument first.
(define (ti-paren test str) (if test (string-append "(" str ")") str))

(define (ti-string-of-term term)
  (let loop ((prec 0)
             (tv (term)))
    (match/first tv
      ((I ,n) (number->string n))
      ((V ,name) name)
      ((L ,v ,body) (let1 (s (loop 0 (body)))
                      (ti-paren (> prec 0) (string-append "L" v ". " s))))
      ((A ,f ,x) (let* ((sx (loop 11 (x)))
                        (sf (loop 10 (f))))
                   (ti-paren (> prec 10) (string-append sf " " sx)))))))

; cf. TI:46-55 `a_term`: a random term, read on demand, each subterm a probcc-letlazy; the
; variables x0, x1, … are geometric, so the distribution is infinite.  The masses are the exact
; rationals of upstream's 0.1 and 0.4; the constructors' arguments from the right (D19).
(define (ti-a-term)
  (letrec ((var (τ (string-append "x" (number->string (probcc-geometric/count 1/10)))))
           (loop (τ ((probcc-distribution
                       `((,(τ '(I 1)) 1/10)
                         (,(τ (let1 (name (var)) (list 'V name))) 1/10)
                         (,(τ (let* ((body (probcc-letlazy loop))
                                     (name (var)))
                                (list 'L name body)))
                          2/5)
                         (,(τ (let* ((e2 (probcc-letlazy loop))
                                     (e1 (probcc-letlazy loop)))
                                (list 'A e1 e2)))
                          2/5)))))))
    (probcc-letlazy loop)))

; cf. TI:85-102 `tp_same`, `tp_v_same`, `tp_pure`: a type is a procedure of an optional type
; value, (tp #f) (`None`) answering its value, determining it if need be, and (tp (list tv))
; (`Some tv`) asserting that it is tv; a type value is (B Int) or (Arr tp tp).  Asserting the
; equality of two types determines the first and imposes it on the second.
(define (ti-tp-same x y) (y (list (x #f))) (void))

(define (ti-tp-v-same x y)
  (match/first (list x y)
    (((B Int) (B Int)) (void))
    (((Arr ,t1o ,t1a) (Arr ,t2o ,t2a)) (ti-tp-same t1o t2o) (ti-tp-same t1a t2a))
    (else (probcc-impossible))))

(define ((ti-tp-pure x) o)
  (match/first o
    (#f x)
    ((,y) (ti-tp-v-same x y) y)))

; cf. TI:105-106 `int` and `arr`.
(define ti-int (ti-tp-pure '(B Int)))
(define (ti-arr t1 t2) (ti-tp-pure (list 'Arr t1 t2)))

; cf. TI:109-116 `string_of_tp`, "..." past depth 10; the result type first (D19).
(define (ti-string-of-tp tp)
  (let loop ((depth 0)
             (prec 0)
             (tv (tp #f)))
    (match/first tv
      ((B Int) "int")
      ((Arr ,t1 ,t2) (if (> depth 10)
                         "..."
                         (let* ((s2 (loop (add1 depth) 10 (t2 #f)))
                                (s1 (loop (add1 depth) 11 (t1 #f))))
                           (ti-paren (> prec 10) (string-append s1 " -> " s2))))))))

; cf. TI:121-126 `tp_memo`: a type variable, its type tp kept in a world-local reference and
; replaced, once asked, by the pure type of its value (deviation 17: WorldLocalMem is not
; shipped upstream; probcc-wref-set! takes the reference first).
(define (ti-tp-memo tp)
  (let1 (r (probcc-wref-new tp))
    (λ (x)
      (let1 (tv ((probcc-wref-get r) x))
        (probcc-wref-set! r (ti-tp-pure tv))
        tv))))

; cf. TI:131-138 `a_tp`: the lazy generator of types, int or an arrow of two type variables
; (from the right, D19), at 1/2 each.
(define (ti-a-tp o)
  (match/first o
    (#f (let* ((t2 (ti-tp-memo ti-a-tp))
               (t1 (ti-tp-memo ti-a-tp)))
          (probcc-distribution `(((B Int) 1/2) ((Arr ,t1 ,t2) 1/2)))))
    ((,tv) tv)))

; cf. TI:212-228 `new_tvar` and `typeof`: the type of a term in the environment gamma, a list
; of (name tp) (deviation 25); an application types its argument first, the arguments of
; tp_same and arr from the right (D19).
(define (ti-new-tvar) (ti-tp-memo ti-a-tp))

(define (ti-typeof gamma exp)
  (match/first (exp)
    ((I ,_) ti-int)
    ((V ,name) (match/first (assoc name gamma)
                 (#f (probcc-impossible))
                 ((,_ ,tp) tp)))
    ((L ,v ,body) (let* ((targ (ti-new-tvar))
                         (tbody (ti-typeof (cons (list v targ) gamma) body)))
                    (ti-arr targ tbody)))
    ((A ,e1 ,e2) (let* ((tres (ti-new-tvar))
                        (t2 (ti-typeof gamma e2))
                        (t1 (ti-typeof gamma e1)))
                   (ti-tp-same t1 (ti-arr t2 tres))
                   tres))))

; cf. TI:230-232 `do_infer`: the types of term, to depth bound.
(define (ti-infer bound term) (probcc-reify/part bound (τ (ti-string-of-tp (ti-typeof '() term)))))

;; -----------------------------------------------------------------------------------------
;; Models of parsec.ml (PS)
;; -----------------------------------------------------------------------------------------

; cf. PS:203-205: `p_char 'a'`, `p_char 'b'` and `(fun _ -> ()) <$> …`, whose unit is (void)
; (deviation 22).
(define p-a (parsec-char #\a))
(define p-b (parsec-char #\b))
(define (to-unit _) (void))

; cf. PS:201-208 `pali`, the palindromes over a and b of the Curry example (PS:188-199), a
; recognizer: `empty <|> … <|> …`, nested to the left as OCaml's infix (deviation 22), and
; `p_char 'a' *> pali <* p_char 'a'`, where `*>` binds tighter than `<*`.
(define pali
  (parsec-fix (λ (pali)
                (parsec-or (parsec-or (parsec-or (parsec-or parsec-empty (parsec-fmap to-unit p-a))
                                                 (parsec-fmap to-unit p-b))
                                      (parsec-left (parsec-right p-a pali) p-a))
                           (parsec-left (parsec-right p-b pali) p-b)))))

; cf. PS:227-236, the same with the uniform alternation of the five.
(define pali/alt
  (parsec-fix (λ (pali)
                (parsec-alt (list parsec-empty
                                  (parsec-fmap to-unit p-a)
                                  (parsec-fmap to-unit p-b)
                                  (parsec-left (parsec-right p-a pali) p-a)
                                  (parsec-left (parsec-right p-b pali) p-b))))))

; cf. PS:146-151, the `many` without maximal munch, commented out upstream: `pure [] <|> (x ::
; many p)`, which keeps every prefix.
(define ((many/no-munch p) st)
  ((parsec-or (parsec-pure '())
              (parsec-ap (parsec-fmap (λ (x) (λ (xs) (cons x xs))) p) (many/no-munch p)))
   st))

; (new) `fun x y -> (x,y)` curried, for `<$> … <*>`; deviation 25: the pair is a list.
(define ((pair x) y) (list x y))

; cf. PS:430-433 `digit` and `num`, the natural numbers of the expression grammar of the Curry
; example (PS:397-428).
(define digit
  (parsec-fmap (λ (c) (- (char->integer c) (char->integer #\0)))
               (parsec-sat (λ (x) (and (char>=? x #\0) (char<=? x #\9))))))

(define num (parsec-fmap (λ (ds) (fold (λ (d acc) (+ d (* acc 10))) 0 ds)) (parsec-many1 digit)))

; cf. PS:438-444 `plus_minus` and `prod_div`; OCaml's `/` truncates, as quotient.
(define plus-minus (parsec-or (parsec-fmap (λ (_) +) (parsec-char #\+))
                              (parsec-fmap (λ (_) -) (parsec-char #\-))))

(define prod-div (parsec-or (parsec-fmap (λ (_) *) (parsec-char #\*))
                            (parsec-fmap (λ (_) quotient) (parsec-char #\/))))

; cf. PS:446-459 `expression`, `term`, `factor`: the grammar is not factored, as the Curry one;
; `(fun t1 op t2 -> op t1 t2) <$> term <*> plus_minus <*> expression` associates to the left.
(define (((binary t1) op) t2) (op t1 t2))

(define (expression st)
  ((parsec-or (parsec-ap (parsec-ap (parsec-fmap binary term) plus-minus) expression) term) st))

(define (term st)
  ((parsec-or (parsec-ap (parsec-ap (parsec-fmap binary factor) prod-div) term) factor) st))

(define (factor st)
  ((parsec-or (parsec-left (parsec-right (parsec-char #\() expression) (parsec-char #\))) num) st))

; cf. PS:473-474: the alphabet of exp_stream, a list (deviation 22).
(define expression-alphabet (string->list "0123456789+-*()"))

; cf. PS:472-476 `exp_stream`: four characters over the alphabet of the expressions; `over`
; makes the stream (parsec-stream-over, or ml-stream-over for the replays).
(define ((exp-stream over))
  (let1 (st (over expression-alphabet))
    (parsec-stream-len st 4)
    st))

; cf. PS:514-523, the more balanced `exp_stream`: four characters, an operator or a parenthesis
; with probability h (upstream's 0.7), a digit otherwise, drawn with `uniformly`
; (probcc-uniform/either, or ml-uniformly for the replays); each cell a probcc-letlazy/nesting,
; its tail made before its character (`Cons (ch (), letlazy …)` from the right, D19).
(define ((exp-stream/balanced h uniformly))
  (letrec ((ch (τ (if (probcc-coin h)
                      (uniformly (string->list "+-*()"))
                      (uniformly (string->list "0123456789")))))
           (cell (λ (n)
                   (if (zero? n)
                       (τ '())
                       (probcc-letlazy/nesting (τ (let* ((t (cell (sub1 n)))
                                                         (c (ch)))
                                                    (cons c t))))))))
    (cell 4)))

; cf. PS:496-501, the model of the sampling runs: the value of an expression parsing the whole
; stream, and the string of the stream.
(define ((expression-model stream))
  (let1 (st (stream))
    (match/first (expression st)
      ((,v . ,st*) (match/first (st*)
                     (() (list v (parsec-string-of-stream st)))
                     (else (probcc-impossible)))))))

; cf. P:156-165 `uniform` and P:168-170 `uniformly`, upstream's, with flonum masses (as in the
; suite hansei-lazy): 1. /. n, the value 0 getting 1. -. pacc.  probcc-uniform's masses are the
; exact 1/n (deviation 7), so the replays draw the characters of the stream with these, to give
; upstream's flonums bit for bit.
(define (ml-uniform n)
  (cond
    ((= n 1) 0)
    (else (let1 (p (/ 1. n))
            (let loop ((pacc 0.) (acc '()) (i (sub1 n)))
              (if (zero? i)
                  (probcc-distribution (cons (list i (- 1. pacc)) acc))
                  (loop (+ pacc p) (cons (list i p) acc) (sub1 i))))))))

(define (ml-uniformly lst) (list-ref lst (ml-uniform (length lst))))

; cf. PS:59-66 `stream_over`, as parsec-stream-over but with `flip 0.5` and ml-uniformly.
(define (ml-stream-over alphabet)
  (letrec ((loop (τ (cond
                      ((probcc-coin 0.5) (let* ((t (probcc-letlazy/nesting loop))
                                                (c (ml-uniformly alphabet)))
                                           (cons c t)))
                      (else '())))))
    (probcc-letlazy/nesting loop)))

(define-suite hansei-logic

  ((doc r)
   `((structure/section "Logic programming")
     (p "After "
        (cite/a "https://okmij.org/ftp/kakuritu/logic-programming.html" "HANSEI as a Declarative Logic Programming Language")
        ", the logic programs of the page, of its files and of the parser combinators "
        (cite/a "https://okmij.org/ftp/kakuritu/logic-programming.html#parsec" "`Reversible' parser combinators") ".")
     (structure/section "Re-thinking Prolog")
     (p "After "
        (cite/a "https://okmij.org/ftp/kakuritu/logic-programming.html#vs-prolog" "Re-thinking Prolog")
        " (LG l.33-75) and its paper (RT):"
        (cite/quote
          "Oleg Kiselyov, Yukiyoshi Kameyama"
          "One lesson is that guessing-- non-determinism-- is fundamental, but should not be the
	  default mode of execution. One should guess, but guess lazily.")
        "The library interface the paper relies on (RT Figure 1, l.285-308) is "
        (code/inline "dist") ", " (code/inline "fail") ", " (code/inline "reify0") ", "
        (code/inline "letlazy") ", " (code/inline "flip") ", " (code/inline "uniformly") ", "
        (code/inline "exact_reify") " and " (code/inline "reify_part") ", here "
        (code/inline "probcc-distribution") ", " (code/inline "probcc-impossible") ", "
        (code/inline "probcc-reify/0") ", " (code/inline "probcc-letlazy") " (and "
        (code/inline "probcc-letlazy/nesting") "), " (code/inline "probcc-coin") ", "
        (code/inline "probcc-uniform/either") ", " (code/inline "probcc-reify/exact") " and "
        (code/inline "probcc-reify/part") ". The cheap non-determinism of RT §3.1, a generator
	that guesses whole lists before testing them, finds the splits of a list only within a
	bound, and diverges past it.")
     (structure/section "No logic variables? No unification?")
     (p "After "
        (cite/a "https://okmij.org/ftp/kakuritu/logic-programming.html#logic-var" "No logic variables? No unification?")
        " (LG l.230-327): a logic variable is a generator of its domain, the sharing of the
	generator expresses its repeated occurrences, and lazy evaluation delays the
	generation as unification would:"
        (cite/quote
          "Oleg Kiselyov"
          "It appears that lazy evaluation can make up for logic variables, rendering Herbrand's
	  original approach practical."))
     (structure/section "The append relation")
     (p "After "
        (cite/a "https://okmij.org/ftp/kakuritu/logic-programming.html#append" "The append relation")
        " (LG l.328-624) and " (code/inline "logic_vars.ml") ": the Prolog relation"
        (code/scheme "append([],L,L).
append([H|T],L,[H|R]) :- append(T,L,R).")
        "becomes a function over lists with a non-deterministic spine, which runs forwards,
	backwards and sideways once its arguments are logic variables. Prolog's committed
	choice, needed in practice, breaks running backwards (PD:187-217):"
        (code/scheme "del([H|T],H,T).
del([H|T],D,[H|R]) :- del(T,D,R).
prog3(L,S) :- del(L,true,LRem) -> S=deleted(LRem); S=failed.
%% ?- prog3(L,S).
%% L = [true|_G16],
%% S = deleted(_G16).")
        "where HANSEI's soft-cut, a nested inference, keeps every answer.")
     (structure/section "Solving scheduling problems")
     (p "After "
        (cite/a "https://okmij.org/ftp/kakuritu/logic-programming.html#daily-schedule" "Solving scheduling problems")
        " (LG l.625-757): state the constraints as evidence and ask for the worlds consistent
	with them; a world is rejected as soon as one fact contradicts the evidence, so only a
	part of the 8957952 worlds is generated.")
     (structure/section "Zebra puzzle")
     (p "After "
        (cite/a "https://okmij.org/ftp/kakuritu/logic-programming.html#zebra" "Zebra puzzle")
        " (LG l.758-792): the all-different constraint of the five attributes is a lazy
	permutation of " (code/inline "(aux hansei lazy)") ", and the puzzle a naive
	generate-and-test; its unique solution, at weight 1/199065600000, is in the long suite "
        (code/inline "hansei-long.scm") ". The U2 bridge puzzle (" (code/inline "puzzle.ml")
        ") and type inference run backwards (" (code/inline "type_inference.ml")
        ") are two more programs that guess.")
     (structure/section "Reversible parser combinators")
     (p "After "
        (cite/a "https://okmij.org/ftp/kakuritu/logic-programming.html#parsec" "`Reversible' parser combinators")
        " (LG l.793-996) and " (code/inline "parsec.ml") ", the parser combinators of "
        (code/inline "(aux hansei parsec)") " work over a non-deterministic stream:"
        (cite/quote
          "Oleg Kiselyov"
          "They can emulate not only parsing of a stream (that is, parsing forwards) but also
	  determining a stream for the given parse (parsing backwards)."))
     (p "A stream is a thunk answering " (code/inline "'()") " or a pair "
        (code/inline "(char . stream)") ", and a parser maps a stream to the pair "
        (code/inline "(v . stream)") " of its value and the rest. Alternation is a uniform
	choice, so a parse comes with the probability of the choices that led to it. "
        (code/inline "parsec-run/backward") " generates streams with "
        (code/inline "parsec-stream-over") ", lazily, each cell a "
        (code/inline "probcc-letlazy/nesting") ", and keeps those the parser accepts whole.")
     (structure/section "Maximal munch, committed choice and nested inference")
     (p "After "
        (cite/a "https://okmij.org/ftp/kakuritu/logic-programming.html#many" "Maximal munch, committed choice and nested inference")
        " (LG l.997-1230): " (code/inline "parsec-many") " is Prolog's"
        (code/scheme "many(P,S,Rest) :- call(P,S,Srem) *-> many(P,Srem,Rest) ; S = Rest.")
        "with the soft-cut " (code/inline "probcc-soft-cut") ", a nested inference: "
        (code/inline "probcc-first-success") " over the reified parser. Unlike Prolog's, it
	keeps parsing backwards complete, since an inner reification lets the stream's choices
	take effect in the outer world:"
        (cite/quote
          "Oleg Kiselyov"
          "many p (s1 | s2) = many p s1 | many p s2"))))

  ((test/append _)
   (⊦ equal? '(((#t #t #t #f #f) 1))
      (probcc-reify/part #f (τ (blist->list (bl-append bl-t3 bl-f2)))))
   ; cf. LV:89-98: without letlazy, the list is a new list each time it is forced: wrong on
   ; purpose.  The tuple from the right (D19), the append first.
   (⊦ equal? '((((#t) (#t #f #f)) 1/4) (((#t) (#f #f #f)) 1/4) (((#f) (#t #f #f)) 1/4)
               (((#f) (#f #f #f)) 1/4))
      (probcc-reify/part #f (τ (let* ((l (τ (let1 (h (probcc-coin 1/2)) (cons h bl-nil))))
                                      (r (blist->list (bl-append l bl-f2)))
                                      (x (blist->list l)))
                                 (list x r)))))
   (⊦ equal? '((() 1/2) ((#t) 1/8) ((#f) 1/8) ((#t #t) 1/32) ((#t #f) 1/32) ((#f #t) 1/32)
               ((#f #f) 1/32))
      (probcc-reify/part 5 (τ (blist->list (a-blist)))))
   ; cf. LV:128-134: ?- bool(X), append([X],[f,f],R).
   (⊦ equal? '((((#t) (#t #f #f)) 1/2) (((#f) (#f #f #f)) 1/2))
      (probcc-reify/part #f (τ (let* ((l (probcc-letlazy/nesting
                                           (τ (let1 (h (probcc-coin 1/2)) (cons h bl-nil)))))
                                      (r (blist->list (bl-append l bl-f2)))
                                      (x (blist->list l)))
                                 (list x r)))))
   ; cf. LV:141-150: ?- append([t,t,t],X,R), boollist(X), boollist(R).
   (⊦ equal? '(((#t #t #t) 1/2) ((#t #t #t #t) 1/8) ((#t #t #t #f) 1/8) ((#t #t #t #t #t) 1/32)
               ((#t #t #t #t #f) 1/32) ((#t #t #t #f #t) 1/32) ((#t #t #t #f #f) 1/32))
      (probcc-reify/part 5 (τ (blist->list (bl-append bl-t3 (a-blist))))))
   ; cf. LV:155-163: ?- append(X,[f,f],R), boollist(X), boollist(R).
   (⊦ equal? '(((#f #f) 1/2) ((#t #f #f) 1/8) ((#f #f #f) 1/8) ((#t #t #f #f) 1/32)
               ((#t #f #f #f) 1/32) ((#f #t #f #f) 1/32) ((#f #f #f #f) 1/32))
      (probcc-reify/part 5 (τ (blist->list (bl-append (a-blist) bl-f2)))))
   `(doc (p "The append relation run forwards (LV:39-163, LG " (code/inline "#append")
            "): a logic variable is a generator of its domain, "
            (code/inline "a-boolean") " or " (code/inline "a-blist") ", made a variable by "
            (code/inline "probcc-letlazy/nesting") ": once its value is asked for, it stays
	    the same in its world. Without it the list "
            (code/inline "[X]") " is drawn anew each time it is forced, and "
            (code/inline "?- bool(X), append([X],[f,f],R)") " has four answers where Prolog
	    has two. Explored to depth 5 the generators give both kinds of answers that Prolog's
	    depth-first search misses, lists with an " (code/inline "f") " after the three "
            (code/inline "t") "'s among them.")))

  ((test/append/backwards _)
   (define (unappend x r)
     (unless (bl-compare r bl-t3f2) (probcc-impossible))
     (blist->list x))
   ; cf. LV:183-189: ?- append([t,t],X,[t,t,t,f,f]).
   (⊦ equal? '(((#t #f #f) 1/128))
      (probcc-reify/part #f (τ (let1 (x (a-blist))
                                 (unappend x (bl-append (bl-cons #t (bl-cons #t bl-nil)) x))))))
   ; cf. LV:193-199: ?- append(X,[f,f],[t,t,t,f,f]).
   (⊦ equal? '(((#t #t #t) 1/128))
      (probcc-reify/part #f (τ (let1 (x (a-blist))
                                 (unappend x (bl-append x (bl-cons #f (bl-cons #f bl-nil))))))))
   ; cf. LV:205-217: ?- append(X,Y,[t,t,t,f,f]), the tuple from the right (D19).
   (⊦ equal? (probcc-map (λ (e) (list e 1/4096))
                         '(((#t #t #t #f #f) ()) ((#t #t #t #f) (#f)) ((#t #t #t) (#f #f))
                           ((#t #t) (#t #f #f)) ((#t) (#t #t #f #f)) (() (#t #t #t #f #f))))
      (probcc-reify/part #f (τ (let* ((x (a-blist))
                                      (y (a-blist)))
                                 (unless (bl-compare (bl-append x y) bl-t3f2) (probcc-impossible))
                                 (let* ((ly (blist->list y))
                                        (lx (blist->list x)))
                                   (list lx ly))))))
   ; cf. LV:229-232: ?- last(E,[t,t,t,f,f]).
   (⊦ equal? '((#f 1/1024)) (probcc-reify/part #f (τ ((bl-last bl-t3f2)))))
   ; cf. LV:238-245 and LV:252-259: two variables made equal, a finite failure; compared the
   ; other way round, the search needs a bound.
   (⊦ equal? '(((#t #t #t #f #f) 1/4194304))
      (probcc-reify/part #f (τ (let* ((x (a-blist))
                                      (y (a-blist)))
                                 (unless (bl-compare x bl-t3f2) (probcc-impossible))
                                 (unless (bl-compare x y) (probcc-impossible))
                                 (blist->list y)))))
   (⊦ equal? '(((#t #t #t #f #f) 1/4194304))
      (probcc-reify/part 30 (τ (let* ((x (a-blist))
                                      (y (a-blist)))
                                 (unless (bl-compare x y) (probcc-impossible))
                                 (unless (bl-compare x bl-t3f2) (probcc-impossible))
                                 (blist->list y)))))
   `(doc (p "The append relation run backwards (LV:165-268): the prefix, the suffix, all the
	    splits of " (code/inline "[t,t,t,f,f]") " and its last element are found by
	    generate-and-test, " (code/inline "bl-compare") " forcing the variables only as far as
	    the comparison needs, so the search is finite and exhaustive; the weights estimate
	    the size of the search space (e.g. 2^-12 for each of the six splits, 2^-22 for two
	    variables equal to the five-element list). With the comparisons swapped, "
            (code/inline "x = y") " is asked first, over two unbounded lists, and the search
	    must be bounded (depth 30).")))

  ((test/committed-choice _)
   (define (with-remainder l)
     (let* ((r (blist->list (bl-del l #t)))
            (x (blist->list l)))
       (list x r)))
   ; cf. LV:287-296: deletion from a known list is deterministic.
   (⊦ equal? '(((#t #t #f #f) 1)) (probcc-reify/part #f (τ (blist->list (bl-del bl-t3f2 #t)))))
   (⊦ equal? '(((#t #t #t #f) 1)) (probcc-reify/part #f (τ (blist->list (bl-del bl-t3f2 #f)))))
   ; cf. LV:304-322: del backwards, without and with laziness, and over a-blist.
   (⊦ equal? '((((#t) ()) 1/4) (((#f) ()) 1/4))
      (probcc-reify/part #f (τ (with-remainder (τ (let1 (h (probcc-coin 1/2)) (cons h bl-nil)))))))
   (⊦ equal? '((((#t) ()) 1/2))
      (probcc-reify/part #f (τ (with-remainder (probcc-letlazy/nesting
                                                 (τ (let1 (h (probcc-coin 1/2))
                                                      (cons h bl-nil))))))))
   (⊦ equal? '((((#t) ()) 1/8) (((#t #t) (#t)) 1/32) (((#t #f) (#f)) 1/32) (((#f #t) (#f)) 1/32))
      (probcc-reify/part 5 (τ (with-remainder (a-blist)))))
   ; cf. LV:338-349: del on every list of three elements, with no bound.
   (⊦ equal? '((((#t #t #t) (#t #t)) 1/128) (((#t #t #f) (#t #f)) 1/128)
               (((#t #f #t) (#f #t)) 1/128) (((#t #f #f) (#f #f)) 1/128)
               (((#f #t #t) (#f #t)) 1/128) (((#f #t #f) (#f #f)) 1/128)
               (((#f #f #t) (#f #f)) 1/128))
      (probcc-reify/part #f (τ (let1 (l (a-blist))
                                 (bl-haslen l 3)
                                 (with-remainder l)))))
   ; cf. LV:397-401: prog3([f,t,f],S).
   (⊦ equal? '((((#f #f)) 1))
      (probcc-reify/part #f (τ (bl-prog3/value (bl-cons #f (bl-cons #t (bl-cons #f bl-nil)))))))
   ; cf. LV:404-424: prog3(L,S) to depth 9, the 16 answers, `None` (#f) among them.
   (⊦ equal? '(((() #f) 1/2) (((#t) (())) 1/8) (((#f) #f) 1/8) (((#t #t) ((#t))) 1/32)
               (((#t #f) ((#f))) 1/32) (((#f #t) ((#f))) 1/32) (((#f #f) #f) 1/32)
               (((#t #t #t) ((#t #t))) 1/128) (((#t #t #f) ((#t #f))) 1/128)
               (((#t #f #t) ((#f #t))) 1/128) (((#t #f #f) ((#f #f))) 1/128)
               (((#f #t #t) ((#f #t))) 1/128) (((#f #t #f) ((#f #f))) 1/128)
               (((#f #f #t) ((#f #f))) 1/128) (((#f #f #f) #f) 1/128) (((#f #f #f #f) #f) 1/512))
      (probcc-reify/part 9 (τ (let* ((l (a-blist))
                                     (r (bl-prog3/value l)))
                                (list (blist->list l) r)))))
   ; cf. LV:429-444: prog3 on every list of three elements, with no bound.
   (⊦ equal? '((((#t #t #t) ((#t #t))) 1/128) (((#t #t #f) ((#t #f))) 1/128)
               (((#t #f #t) ((#f #t))) 1/128) (((#t #f #f) ((#f #f))) 1/128)
               (((#f #t #t) ((#f #t))) 1/128) (((#f #t #f) ((#f #f))) 1/128)
               (((#f #f #t) ((#f #f))) 1/128) (((#f #f #f) #f) 1/128))
      (probcc-reify/part #f (τ (let1 (l (a-blist))
                                 (bl-haslen l 3)
                                 (let1 (r (bl-prog3/value l))
                                   (list (blist->list l) r))))))
   `(doc (p "Deletion and committed choice (LV:270-444): " (code/inline "bl-del")
            " deletes the first " (code/inline "#t") " of a list, failing on a list without
	    one; run backwards over " (code/inline "a-blist") " it generates the lists that have
	    one. Prolog's committed choice"
            (code/scheme "prog3(L,S) :- del(L,true,LRem) -> S=deleted(LRem) ; S=failed.")
            "loses the answers of " (code/inline "?- prog3(L,S).") " but the first (PD:202-217);
	    with the soft-cut " (code/inline "probcc-soft-cut") ", a nested inference, "
            (code/inline "bl-prog3") " keeps them all, " (code/inline "S=failed")
            " (#f) for the lists without " (code/inline "#t") ", and terminates with no bound
	    over the lists of three elements.")))

  ((test/committed-choice/letlazy _)
   ; prog3 to depth 9 over the a-blist of plain letlazy: upstream with ProbM.letlazy, run under
   ; OCaml, gives this table (17 answers) instead of LV:404-424's.
   (⊦ equal? '(((() (())) 1/16) ((() ((#f))) 1/32) (((#t) (())) 1/64) (((#f) (())) 1/64)
               ((() ((#t))) 1/64) ((() ((#f #f))) 3/256) (((#t) ((#f))) 1/128)
               (((#f) ((#f))) 1/128) ((() ((#f #t))) 1/128) (((#t #t) (())) 1/256)
               (((#t #f) (())) 1/256) (((#t) ((#t))) 1/256) (((#f #t) (())) 1/256)
               (((#f #f) (())) 1/256) (((#f) ((#t))) 1/256) ((() ((#t #t))) 1/256)
               ((() ((#t #f))) 1/256))
      (probcc-reify/part 9 (τ (let* ((l (a-blist/with probcc-letlazy))
                                     (r (bl-prog3/value l)))
                                (list (blist->list l) r)))))
   `(doc (p "Why LV:19 makes " (code/inline "letlazy") " " (code/inline "letlazy_nesting")
            ": the soft-cut of " (code/inline "bl-prog3") " reifies " (code/inline "bl-del")
            " in an inner world, which starts with an empty memory (N:75-81); with the plain "
            (code/inline "probcc-letlazy") " the list is drawn anew inside, unrelated to the
	    outer one, and the answers are wrong, e.g. the empty list has a remainder. The table
	    is upstream's under the same change.")))

  ((test/cheap-nondeterminism _)
   (define (split bound)
     (probcc-reify/part bound (τ (let* ((x (rt-a-list))
                                        (y (rt-a-list)))
                                   (unless (equal? (append x y) '(#t #t #t #f #f))
                                     (probcc-impossible))
                                   (list x y)))))
   (define splits
     (probcc-map (λ (e) (list e 1/4096))
                 '(((#t #t #t #f #f) ()) ((#t #t #t #f) (#f)) ((#t #t #t) (#f #f))
                   ((#t #t) (#t #f #f)) ((#t) (#t #t #f #f)) (() (#t #t #t #f #f)))))
   (⊦ equal? splits (split 15))
   (⊦ equal? splits (split 25))
   `(doc (p "Cheap non-determinism (RT §3.1, p.4-5 l.318-392): append run backwards by
	    generate-and-test over ordinary lists, " (code/inline "rt-a-list") " drawing a
	    whole list before " (code/inline "append") " looks at it; the six splits of "
            (code/inline "[t,t,t,f,f]") ", at 2^-12 each (the paper's rounded 0.0002), are
	    found at depth 15 and at the paper's 25, where the generator has made many more
	    candidates to reject; at depth 35 the search practically diverges (RT l.390-392).
	    Compare the lazy " (code/inline "a-blist") " above, which needs no bound.")))

  ((test/daily-schedule _)
   (⊦ equal? (probcc-map (λ (s) `((V ,s) 1/8)) '((3 2 1) (3 2) (3 1) (3) (2 1) (2) (1) ()))
      (probcc-reify/exact (ds-powerset '(1 2 3))))
   (⊦ equal? '(((V #((Supermarket Shop) (Barber Walk) ())) 1/8957952))
      (probcc-reify/exact (ds-schedule-model)))
   `(doc (p "Mikael More's scheduling problem (DS, LG " (code/inline "#daily-schedule")
            "): the actions of each day are a memoized subset of the four, a logic variable,
	    and every constraint a fact about a uniformly chosen day. The schedule is unique: on
	    Monday the supermarket and the shop, on Tuesday the walk and the barber; its weight,
	    1/8957952 (upstream " (code/inline "1.11632658893461385e-07")
            "), estimates the search space, of which only a part is visited, since a world
	    is rejected as soon as one of its facts contradicts the evidence.")))

  ((test/bridge _)
   (⊦ equal? '(((V (((Bono Edge) #t) ((Edge) #f) ((Adam Larry) #t) ((Bono) #f) ((Bono Edge) #t)))
                1/6912)
               ((V (((Bono Edge) #t) ((Bono) #f) ((Adam Larry) #t) ((Edge) #f) ((Bono Edge) #t)))
                1/13824))
      (probcc-reify/exact (pz-loop '() #t 17 (list pz-u2 '()))))
   `(doc (p "The U2 bridge puzzle (PZ:1-185): four people, one flashlight, at most two on
	    the bridge, 17 minutes. With HANSEI's " (code/inline "SimpleNonDet") " ("
            (code/inline "choose") " a uniform choice, failing on the empty list) the
	    exhaustive search finds the two known solutions, each crossing a pair "
            (code/inline "(party forward)") ": Bono and Edge cross, one of them returns, Adam and Larry cross, the other
	    returns, Bono and Edge cross. The weights, upstream's under OCaml (1/6912 and
	    1/13824), are those of the choices that led to each trace.")))

  ((test/type-inference/types _)
   ; cf. TI:158-175: a variable made equal to an arrow of two variables, the arrow's arguments
   ; from the right (D19), and the tuple, or the operands of `=`, from the right.
   (define (tp-and-arrow both?)
     (let* ((tp (ti-tp-memo ti-a-tp))
            (t2 (ti-tp-memo ti-a-tp))
            (t1 (ti-tp-memo ti-a-tp))
            (tpa (ti-arr t1 t2)))
       (ti-tp-same tp tpa)
       (when both? (ti-tp-same tpa tp))
       (let* ((b (ti-string-of-tp tpa))
              (a (ti-string-of-tp tp)))
         (list a b))))
   (⊦ equal? '(("int" 1/2) ("int -> int" 1/8)) (probcc-reify/part 3 (τ (ti-string-of-tp ti-a-tp))))
   (⊦ equal? `(((V ,(void)) 1/2))
      (probcc-explore 3 (probcc-reify/0 (τ (ti-tp-same ti-a-tp ti-int)))))
   (⊦ equal? `(((V ,(void)) 1/2))
      (probcc-explore 3 (probcc-reify/0 (τ (ti-tp-same ti-a-tp (ti-arr ti-int ti-int))))))
   ; cf. TI:152-156: a type variable is the same type whenever it is asked.
   (⊦ equal? '((#t 11/16))
      (probcc-reify/part 5 (τ (let1 (tp (ti-tp-memo ti-a-tp))
                                (let* ((b (ti-string-of-tp tp))
                                       (a (ti-string-of-tp tp)))
                                  (equal? a b))))))
   ; cf. TI:158-175.
   (⊦ equal? '((("int -> int" "int -> int") 1/8)
               (("int -> int -> int" "int -> int -> int") 1/32)
               (("(int -> int) -> int" "(int -> int) -> int") 1/32))
      (probcc-reify/part 5 (τ (tp-and-arrow #f))))
   (⊦ equal? '((#t 3/16))
      (probcc-reify/part 5 (τ (match/first (tp-and-arrow #t) ((,a ,b) (equal? a b))))))
   ; cf. TI:177-183.
   (⊦ equal? '((#t 1/2))
      (probcc-reify/part 5 (τ (let* ((tpa (ti-arr ti-int (ti-tp-memo ti-a-tp)))
                                     (int->int (ti-arr ti-int ti-int))
                                     (tpb (ti-arr (ti-tp-memo ti-a-tp) int->int)))
                                (ti-tp-same tpa tpb)
                                (let* ((b (ti-string-of-tp tpb))
                                       (a (ti-string-of-tp tpa)))
                                  (equal? a b))))))
   ; cf. TI:186-199: the infinite type a = a -> int is contractible, but not int.
   (⊦ equal? `((,(void) 1/2))
      (probcc-reify/part 5 (τ (let* ((tpa (ti-tp-memo ti-a-tp))
                                     (tpb (ti-arr tpa ti-int)))
                                (ti-tp-same tpa tpb)))))
   (⊦ equal? '()
      (probcc-reify/part #f (τ (let* ((tpa (ti-tp-memo ti-a-tp))
                                      (tpb (ti-arr tpa ti-int)))
                                 (ti-tp-same tpa tpb)
                                 (ti-tp-same ti-int tpa)))))
   `(doc (p "Types read on demand (TI:81-199): a type is a procedure asked either for its value
	    or to equal a given value, and a type variable, " (code/inline "ti-tp-memo")
            ", keeps in a world-local reference ("
            (code/inline "probcc-wref-new") ", deviation 17) the type it has become, so that it
	    is the same whenever it is asked. Equating two types determines the first only as
	    far as the second needs. " (code/inline "WorldLocalMem")
            " is not shipped upstream; these tables are TI's, and are also what TI gives under
	    OCaml with " (code/inline "wref_new") ", " (code/inline "wref_get") " and "
            (code/inline "wref_set") " written over ProbM's memory.")))

  ((test/type-inference _)
   (⊦ equal? '(("int -> int" 1/2) ("(int -> int) -> int" 1/8) ("(int -> int -> int) -> int" 1/32)
               ("((int -> int) -> int) -> int" 1/32))
      (ti-infer 4 (ti-lam "f" (ti-num 1))))
   (⊦ equal? '() (ti-infer #f (ti-lam "f" ti-vx)))
   (⊦ equal? '(("int -> int" 1/2) ("(int -> int) -> int -> int" 1/8)
               ("(int -> int -> int) -> int -> int -> int" 1/32)
               ("((int -> int) -> int) -> (int -> int) -> int" 1/32))
      (ti-infer 4 (ti-lam "f" ti-vf)))
   (⊦ equal? '(("int" 1/2)) (ti-infer #f (ti-app (ti-lam "f" ti-vf) (ti-num 1))))
   (⊦ equal? '(("(int -> int) -> int" 1/4) ("(int -> int -> int) -> int -> int" 1/16)
               ("(int -> int -> int -> int) -> int -> int -> int" 1/64)
               ("(int -> (int -> int) -> int) -> (int -> int) -> int" 1/64))
      (ti-infer 5 (ti-lam "f" (ti-app ti-vf (ti-num 1)))))
   ; cf. TI:258: no occurs check, the cyclic types are cut at depth 10 (upstream under OCaml).
   (⊦ equal? '(("((((((((((... -> int) -> int) -> int) -> int) -> int) -> int) -> int) -> int) -> int) -> int) -> int"
                1/4)
               ("((((((((((... -> ...) -> int -> int) -> int -> int) -> int -> int) -> int -> int) -> int -> int) -> int -> int) -> int -> int) -> int -> int) -> int -> int) -> int -> int"
                1/16))
      (ti-infer 3 (ti-lam "x" (ti-app ti-vx ti-vx))))
   ; cf. TI:260: the 22 candidates of depth 10 (upstream under OCaml).
   (⊦ equal? (append
               '(("(int -> int) -> (int -> int) -> int" 1/16)
                 ("(int -> int -> int) -> (int -> int) -> int -> int" 1/64)
                 ("((int -> int) -> int) -> (int -> int -> int) -> int" 1/64))
               (probcc-map (λ (s) (list s 1/256))
                           '("(int -> int -> int -> int) -> (int -> int) -> int -> int -> int"
                             "(int -> (int -> int) -> int) -> (int -> int) -> (int -> int) -> int"
                             "((int -> int) -> int -> int) -> (int -> int -> int) -> int -> int"
                             "((int -> int -> int) -> int) -> (int -> int -> int -> int) -> int"
                             "(((int -> int) -> int) -> int) -> (int -> (int -> int) -> int) -> int"))
               (probcc-map (λ (s) (list s 1/1024))
                           '("(int -> int -> int -> int -> int) -> (int -> int) -> int -> int -> int -> int"
                             "(int -> int -> (int -> int) -> int) -> (int -> int) -> int -> (int -> int) -> int"
                             "(int -> (int -> int) -> int -> int) -> (int -> int) -> (int -> int) -> int -> int"
                             "(int -> (int -> int -> int) -> int) -> (int -> int) -> (int -> int -> int) -> int"
                             "(int -> ((int -> int) -> int) -> int) -> (int -> int) -> ((int -> int) -> int) -> int"
                             "((int -> int) -> int -> int -> int) -> (int -> int -> int) -> int -> int -> int"
                             "((int -> int) -> (int -> int) -> int) -> (int -> int -> int) -> (int -> int) -> int"
                             "((int -> int -> int) -> int -> int) -> (int -> int -> int -> int) -> int -> int"
                             "((int -> int -> int -> int) -> int) -> (int -> int -> int -> int -> int) -> int"
                             "((int -> (int -> int) -> int) -> int) -> (int -> int -> (int -> int) -> int) -> int"
                             "(((int -> int) -> int) -> int -> int) -> (int -> (int -> int) -> int) -> int -> int"
                             "(((int -> int) -> int -> int) -> int) -> (int -> (int -> int) -> int -> int) -> int"
                             "(((int -> int -> int) -> int) -> int) -> (int -> (int -> int -> int) -> int) -> int"
                             "((((int -> int) -> int) -> int) -> int) -> (int -> ((int -> int) -> int) -> int) -> int")))
      (ti-infer 10 (ti-lam "f" (ti-lam "x" (ti-app ti-vf (ti-app ti-vx (ti-num 1)))))))
   (⊦ equal? '()
      (ti-infer #f (ti-lam "f" (ti-lam "x" (ti-app (ti-app ti-vf (ti-app ti-vx (ti-num 1)))
                                                   (ti-app ti-vx (ti-lam "z" ti-vz)))))))
   `(doc (p "Type inference for the simply-typed lambda calculus (TI:209-263), forwards: "
            (code/inline "ti-typeof") " gives every subterm a fresh type variable and equates
	    types as the rules ask; an ill-typed term has no type, with no bound, and a
	    polymorphic one has a stream of candidates, its instances, of which a bounded
	    search shows the first. There is no occurs check, so "
            (code/inline "Lx. x x") " gets cyclic types, printed up to depth 10.")))

  ((test/type-inference/backwards _)
   (define (generate bound)
     (probcc-reify/part bound (τ (let* ((term (ti-a-term))
                                        (tp (ti-string-of-tp (ti-typeof '() term)))
                                        (s (ti-string-of-term term)))
                                   (list s tp)))))
   ; cf. TI:66-77, upstream's comment: the terms of depth 5, mostly nonsense.
   (⊦ equal? '(("1" 1/10) ("x0" 1/100) ("x1" 9/1000) ("x2" 81/10000) ("x3" 729/100000)
               ("Lx0. 1" 1/250) ("1 1" 1/250) ("Lx1. 1" 9/2500) ("Lx2. 1" 81/25000)
               ("x0 1" 1/2500) ("Lx0. x0" 1/2500) ("1 x0" 1/2500) ("x1 1" 9/25000)
               ("1 x1" 9/25000) ("Lx0. 1 1" 1/6250) ("1 1 1" 1/6250) ("1 (Lx0. 1)" 1/6250)
               ("1 (1 1)" 1/6250) ("(Lx0. 1) 1" 1/6250))
      (probcc-reify/part 5 (τ (ti-string-of-term (ti-a-term)))))
   ; cf. TI:267-280, and TI:282 (upstream under OCaml): well-typed terms and their types.
   (⊦ equal? '((("1" "int") 1/10) (("Lx0. 1" "int -> int") 1/500)) (generate 4))
   (⊦ equal? '((("1" "int") 1/10) (("Lx0. 1" "int -> int") 1/500) (("Lx1. 1" "int -> int") 9/5000))
      (generate 5))
   (⊦ equal? '((("1" "int") 1/10) (("Lx0. 1" "int -> int") 1/500) (("Lx1. 1" "int -> int") 9/5000)
               (("Lx2. 1" "int -> int") 81/50000) (("Lx3. 1" "int -> int") 729/500000)
               (("Lx0. 1" "(int -> int) -> int") 1/2000) (("Lx1. 1" "(int -> int) -> int") 9/20000)
               (("Lx0. x0" "int -> int") 1/5000) (("(Lx0. 1) 1" "int") 1/12500)
               (("(Lx1. 1) 1" "int") 9/125000))
      (generate 7))
   ; cf. TI:292-310, upstream's comment: terms of type int -> int.
   (⊦ equal? `((("Lx0. 1" ,(void)) 1/500) (("Lx1. 1" ,(void)) 9/5000) (("Lx2. 1" ,(void)) 81/50000)
               (("Lx3. 1" ,(void)) 729/500000) (("Lx4. 1" ,(void)) 6561/5000000)
               (("Lx5. 1" ,(void)) 59049/50000000) (("Lx6. 1" ,(void)) 531441/500000000)
               (("Lx0. x0" ,(void)) 1/5000) (("Lx1. x1" ,(void)) 81/500000)
               (("Lx2. x2" ,(void)) 6561/50000000) (("Lx0. (Lx0. 1) 1" ,(void)) 1/625000)
               (("(Lx0. Lx0. 1) 1" ,(void)) 1/625000))
      (probcc-reify/part 10 (τ (let* ((term (ti-a-term))
                                      (u (ti-tp-same (ti-arr ti-int ti-int) (ti-typeof '() term)))
                                      (s (ti-string-of-term term)))
                                 (list s u)))))
   `(doc (p "Type inference backwards (TI:265-310): " (code/inline "ti-a-term")
            " generates terms read on demand, and asking for their types, or for the type "
            (code/inline "int -> int") ", keeps the well-typed ones: a type checker run as a
	    generator of programs of a given type. The masses are the exact rationals of
	    upstream's (e.g. 1/500 for " (code/inline "0.00200000000000000048") ").")))

  ((test/parsec/kit _)
   (define abc (parsec-stream-of-string "abc"))
   (⊦ equal? "abc" (parsec-string-of-stream abc))
   (⊦ equal? "" (parsec-string-of-stream (parsec-stream-of-string "")))
   (⊦ equal? #\a (car (abc)))
   (⊦ equal? "bc" (parsec-string-of-stream (cdr (abc))))
   (⊦ equal? `(((V ,(void)) 1)) (parsec-run/forward parsec-eof ""))
   (⊦ equal? '() (parsec-run/forward parsec-eof "a"))
   (⊦ equal? `(((V ,(void)) 1)) (parsec-run/forward parsec-empty ""))
   (⊦ equal? '(((V x) 1)) (parsec-run/forward (parsec-pure 'x) ""))
   (⊦ equal? '() (parsec-run/forward (parsec-pure 'x) "a"))
   (⊦ equal? '(((V #\a) 1)) (parsec-run/forward p-a "a"))
   (⊦ equal? '() (parsec-run/forward p-a "b"))
   (⊦ equal? '() (parsec-run/forward p-a ""))
   (⊦ equal? '() (parsec-run/forward p-a "ab"))
   (⊦ equal? '(((V #\7) 1)) (parsec-run/forward (parsec-sat char-numeric?) "7"))
   (⊦ equal? '(((V 7) 1)) (parsec-run/forward digit "7"))
   (⊦ equal? '(((V (#\a #\b)) 1)) (parsec-run/forward (parsec-ap (parsec-fmap pair p-a) p-b) "ab"))
   (⊦ equal? '(((V #\b) 1)) (parsec-run/forward (parsec-right p-a p-b) "ab"))
   (⊦ equal? '(((V #\a) 1)) (parsec-run/forward (parsec-left p-a p-b) "ab"))
   ; the branch of p-b fails on "ab", and its mass is lost.
   (⊦ equal? '(((V #\a) 1/2))
      (probcc-reify/exact (car ((parsec-or p-a p-b) (parsec-stream-of-string "ab")))))
   (⊦ equal? '(((V #\a) 1/4)) (parsec-run/forward (parsec-or (parsec-or p-a p-b) p-b) "a"))
   (⊦ equal? '(((V #\a) 1/3)) (parsec-run/forward (parsec-alt (list p-a p-b p-b)) "a"))
   (⊦ equal? '() (parsec-run/forward (parsec-alt '()) ""))
   ; a fixed point counting the a's: one binary choice per character and one at the end.
   (⊦ equal? '(((V 3) 1/16))
      (parsec-run/forward (parsec-fix (λ (p) (parsec-or (parsec-fmap (λ (_) 0) parsec-eof)
                                                        (parsec-right p-a (parsec-fmap add1 p)))))
                          "aaa"))
   (⊦ equal? `(((V ,(void)) 1)) (probcc-reify/exact (parsec-stream-len abc 3)))
   (⊦ equal? '() (probcc-reify/exact (parsec-stream-len abc 2)))
   (⊦ equal? '() (probcc-reify/exact (parsec-stream-len abc 4)))
   (⊦ equal? `(((V ,(void)) 1)) (probcc-reify/exact (parsec-stream-at-most-len abc 3)))
   (⊦ equal? `(((V ,(void)) 1)) (probcc-reify/exact (parsec-stream-at-most-len abc 4)))
   (⊦ equal? '() (probcc-reify/exact (parsec-stream-at-most-len abc 2)))
   ; cf. PS:272-273: the streams over a and b explored to depth 5, a coin and a character per
   ; cell, the 16 streams of three characters and more left unexplored.
   (⊦ equal? `(((V "") 1/2) ((V "b") 1/8) ((V "a") 1/8) ((V "bb") 1/32) ((V "ba") 1/32)
               ((V "ab") 1/32) ((V "aa") 1/32) ,@(make-list 16 '(C 1/128)))
      (shape (probcc-explore 5 (probcc-reify/0
                                 (τ (parsec-string-of-stream (parsec-stream-over '(#\a #\b))))))))
   `(doc (p "The streams and the primitive parsers (PS:24-144, 278-290): "
            (code/inline "parsec-or") " is a uniform choice of two parsers, nested to the left
	    for more, " (code/inline "parsec-alt") " of a list of them; a parse that leaves some
	    input, or fails, gives no world. Explored to depth 5, "
            (code/inline "parsec-stream-over") " gives upstream's table of PS:272-273, a fair
	    coin and a uniform character per cell (deviation 22: the unit value is "
            (code/inline "(void)") ", the alphabet a list).")))

  ((test/parsec/palindromes _)
   (⊦ equal? '() (parsec-run/forward pali "ab"))
   (⊦ equal? `(((V ,(void)) 1/64)) (parsec-run/forward pali "aa"))
   (⊦ equal? `(((V ,(void)) 1/256)) (parsec-run/forward pali "aaaa"))
   (⊦ equal? `(((V ,(void)) 1/32)) (parsec-run/forward pali "aba"))
   (⊦ equal? `(((V ,(void)) 1/512)) (parsec-run/forward pali "abaaba"))
   (⊦ equal? '() (parsec-run/forward pali/alt "ab"))
   (⊦ equal? `(((V ,(void)) 1/25)) (parsec-run/forward pali/alt "aa"))
   (⊦ equal? `(((V ,(void)) 1/125)) (parsec-run/forward pali/alt "aaaa"))
   (⊦ equal? `(((V ,(void)) 1/25)) (parsec-run/forward pali/alt "aba"))
   (⊦ equal? `(((V ,(void)) 1/625)) (parsec-run/forward pali/alt "abaaba"))
   `(doc (p "Recognizing palindromes forwards (PS:188-251), with the left-nested binary "
            (code/inline "parsec-or") " and with " (code/inline "parsec-alt")
            " of the five alternatives; the probabilities are the exact rationals of upstream's
	    floats, e.g. 1/25 for " (code/inline "0.0399999999999999939") " (PS:241).")))

  ((test/parsec/backward _)
   ; cf. PS:276, the palindromes generated to depth 10 (the values of upstream's table).
   (⊦ equal? `(((V (,(void) "")) 1/10) ((V (,(void) "b")) 1/40) ((V (,(void) "a")) 1/40)
               ((V (,(void) "bb")) 1/800) ((V (,(void) "aa")) 1/800)
               ((V (,(void) "bbb")) 1/3200) ((V (,(void) "bab")) 1/3200)
               ((V (,(void) "aba")) 1/3200) ((V (,(void) "aaa")) 1/3200))
      (let1 (pv (parsec-run/backward 10 pali/alt (τ (parsec-stream-over '(#\a #\b)))))
        (filter (λ1-match/first (((V ,_) ,_) #t) (else #f)) pv)))
   ; cf. PS:292-310, all the palindromes of length 5, exhaustively and uniformly.
   (⊦ equal? (probcc-map (λ (s) `((V (,(void) ,s)) 1/256000))
                         '("bbbbb" "bbabb" "babab" "baaab" "abbba" "ababa" "aabaa" "aaaaa"))
      (parsec-run/backward #f pali/alt (τ (let1 (st (parsec-stream-over '(#\a #\b)))
                                            (parsec-stream-len st 5)
                                            st))))
   `(doc (p "Parsing backwards (PS:253-310): " (code/inline "parsec-run/backward")
            " generates the streams of " (code/inline "parsec-stream-over")
            " and keeps those the parser accepts whole; bounded to depth 10 it finds the
	    palindromes of at most three characters, and with streams of exactly five
	    characters (" (code/inline "parsec-stream-len") ") the exploration is exhaustive:
	    the eight palindromes, each at 1/256000 (upstream "
            (code/inline "3.90625000000000093e-06") "). The Curry counterpart is "
            (code/inline "findall (\\[x1,x2,x3,x4,x5] -> pali [x1,x2,x3,x4,x5] =:= [])") ".")))

  ((test/parsec/many _)
   (define many-a (parsec-many p-a))
   (⊦ equal? '(((V ()) 1)) (parsec-run/forward (parsec-left many-a p-b) "b"))
   (⊦ equal? '(((V (#\a #\a #\a)) 1)) (parsec-run/forward many-a "aaa"))
   (⊦ equal? '(((V ((#\a #\a #\a) ())) 1))
      (parsec-run/forward (parsec-ap (parsec-fmap pair many-a) many-a) "aaa"))
   (⊦ equal? '() (parsec-run/forward (parsec-left many-a p-a) "aaa"))
   (⊦ equal? '(((V (#\a #\b #\a #\b #\b)) 1/32))
      (parsec-run/forward (parsec-many (parsec-or p-a p-b)) "ababb"))
   (⊦ equal? '(((V ((#\a #\a #\a))) 1)) (parsec-run/forward (parsec-many (parsec-many1 p-a)) "aaa"))
   (⊦ equal? '(((V ((#\a #\a #\a) (#\b) (#\a) (#\b))) 1/16))
      (parsec-run/forward (parsec-many (parsec-or (parsec-many1 p-a) (parsec-many1 p-b))) "aaabab"))
   (⊦ equal? '(((V ("aa" "a")) 1/4) ((V ("a" "aa")) 1/4) ((V ("a" "a" "a")) 1/8))
      (parsec-run/forward
        (parsec-left (parsec-many (parsec-or (parsec-left (parsec-left (parsec-pure "aa") p-a) p-a)
                                             (parsec-left (parsec-pure "a") p-a)))
                     p-b)
        "aaab"))
   ; cf. PS:364-371: many (p1 | p2) is not a superset of many p1 | many p2 ...
   (⊦ equal? '()
      (parsec-run/forward (parsec-left (parsec-many (parsec-or (parsec-left p-a p-a) p-a)) p-a)
                          "aaa"))
   ; ... as it is without maximal munch (PS:146-151, run under OCaml).
   (⊦ equal? '(((V (#\a)) 1/8) ((V (#\a #\a)) 1/32))
      (parsec-run/forward (parsec-left (many/no-munch (parsec-or (parsec-left p-a p-a) p-a)) p-a)
                          "aaa"))
   (⊦ equal? '(((V (#\a #\a)) 1/8))
      (parsec-run/forward (parsec-left (many/no-munch p-a) p-a) "aaa"))
   (⊦ equal? '(((V (#\a #\a #\a)) 1/16)) (parsec-run/forward (many/no-munch p-a) "aaa"))
   ; cf. PS:373-385: generating backwards loses no solution.
   (⊦ equal? '(((V ((() #\b) "b")) 1/8) ((V (((#\a) #\b) "ab")) 1/32)
               ((V (((#\a #\a) #\b) "aab")) 1/128))
      (parsec-run/backward #f (parsec-ap (parsec-fmap pair many-a) p-b)
                           (τ (let1 (st (parsec-stream-over '(#\a #\b)))
                                (parsec-stream-at-most-len st 3)
                                st))))
   ; cf. PS:387-395: the language a*, with two branches left at depth 9.
   (⊦ equal? '(((V (() "")) 1/2) ((V ((#\a) "a")) 1/8) ((V ((#\a #\a) "aa")) 1/32)
               ((V ((#\a #\a #\a) "aaa")) 1/128) (C 1/256) (C 1/256))
      (shape (parsec-run/backward 9 many-a (τ (parsec-stream-over '(#\a #\b))))))
   `(doc (p "The tests of " (code/inline "parsec-many") " (PS:316-395; the table of LG
	    l.1104-1146): it munches maximally, so it has one parse where a Kleene star has
	    several, and fails where a shorter munch would let the rest parse, e.g. "
            (code/inline "many (p_char 'a') <* p_char 'a'") " on " (code/inline "\"aaa\"")
            "; " (code/inline "many/no-munch") ", upstream's commented-out "
            (code/inline "many") " (PS:146-151), gives the parses of every munch instead. Parsed
	    backwards, " (code/inline "parsec-many") " still generates its whole language.")))

  ((test/parsec/expression _)
   (⊦ equal? '(((V 123) 1)) (parsec-run/forward num "123"))
   (⊦ equal? '(((V 10) 1/8)) (parsec-run/forward expression "10"))
   (⊦ equal? '(((V 5) 1/65536)) (parsec-run/forward expression "(10+5*2)/4"))
   ; six binary choices: term, then factor <*> prod_div <*> term, num, '/', factor, num.
   (⊦ equal? '(((V 1) 1/64)) (parsec-run/forward expression "7/4"))
   (⊦ equal? '() (parsec-run/forward expression "1+"))
   `(doc (p "The arithmetic expressions of the Curry example (PS:397-466): "
            (code/inline "num \"123\"") " is 123 for sure, " (code/inline "expression \"10\"")
            " is 10 at 1/8 (three binary alternatives on the way to " (code/inline "num")
            "), and " (code/inline "\"(10+5*2)/4\"") " is 5 at 2^-16 (upstream "
            (code/inline "1.52587890625e-05") ").")))

  ((test/parsec/factor _)
   (define pv (parsec-run/backward 14 factor (exp-stream/balanced 7/10 probcc-uniform/either)))
   (define leaves (filter (λ1-match/first (((V ,_) ,_) #t) (else #f)) pv))
   (⊦ equal? '(17768 10100) (list (length pv) (length leaves)))
   (⊦ equal? '(((V (9999 "9999")) 81/200000000) ((V (9998 "9998")) 81/200000000))
      (filter (λ1-match/first (((V (,_ ,s)) ,_) (member s '("9999" "9998"))) (else #f)) leaves))
   `(doc (p "Generating expressions (PS:514-529): " (code/inline "factor")
            " explored to depth 14 over the four-character streams of the balanced "
            (code/inline "exp_stream") " (a digit with probability 3/10) has 17768 entries, 10100
	    of them values, as under OCaml; among them " (code/inline "(9999 \"9999\")") " and "
            (code/inline "(9998 \"9998\")") " at 81/200000000 (upstream "
            (code/inline "4.05000000000000305e-07") ").")))

  ((test/parsec/nested _)
   ; cf. LG l.1212-1215: x = letlazy (s1 | s2), many over it, against the choice of the
   ; stream first.
   (define (many-over choose)
     (probcc-explore 6 (probcc-reify/0
                         (τ (let* ((s1 (parsec-stream-over '(#\a #\b)))
                                   (s2 (parsec-stream-of-string "aab"))
                                   (x (choose s1 s2)))
                              (car ((parsec-many p-a) x)))))))
   (define left (many-over (λ (s1 s2) (probcc-letlazy/nesting
                                        (τ ((probcc-uniform/either (list s1 s2))))))))
   (define right (many-over (λ (s1 s2) (probcc-uniform/either (list s1 s2)))))
   (⊦ equal? '(((V (#\a #\a)) 1/2) ((V ()) 3/8) ((V (#\a)) 3/32) (C 1/64) (C 1/64)) (shape left))
   (⊦ equal? (shape left) (shape right))
   `(doc (p "The nested-inference law of LG l.1212-1215: "
            (code/inline "many p (s1 | s2) = many p s1 | many p s2") ". With "
            (code/inline "s1") " a random stream and " (code/inline "s2") " the stream of "
            (code/inline "\"aab\"") ", a stream chosen lazily inside the soft-cut of "
            (code/inline "parsec-many") " (" (code/inline "probcc-letlazy/nesting")
            ") gives the same tree, to depth 6, as a stream chosen before parsing: the inner
	    reification lets the stream's choice take effect in the outer world, as under
	    OCaml.")))

  ((test/replay/parsec _)
   ; cf. PS:492-501, upstream's 0.000390624999999999967 (18 digits, read correctly).
   (⊦ equal? '(((V (7139 "7139")) 0.000390624999999999967)
               ((V (5164 "5164")) 0.000390624999999999967))
      (probcc-sample-importance/a (probcc-random-selector 17) 80
                                  (expression-model (exp-stream ml-stream-over))))
   (⊦ equal? '(((V (7139 "7139")) 1/2560) ((V (5164 "5164")) 1/2560))
      (probcc-sample-importance/a (probcc-random-selector 17) 80
                                  (expression-model (exp-stream parsec-stream-over))))
   ; cf. PS:531-546, 2000 samples over the balanced exp_stream, re-sorted: 55 has the larger
   ; mass, upstream's 2.50000000000000215e-05, which CHICKEN's reader reads one ulp high, as it
   ; does 2.5000000000000022e-05, so it is written 2.500000000000002e-05, the same double; the
   ; ties come in descending key order, as upstream's.
   (⊦ equal? (append '(((V (3403 "3403")) 0.0005) ((V (1396 "1396")) 0.0005)
                       ((V (55 "55+0")) 2.500000000000002e-05))
                     (probcc-map (λ (n) `((V (,n ,(string-append "55+" (number->string (- n 55)))))
                                          2.5e-05))
                                 '(64 63 62 61 60 59 58 57 56))
                     '(((V (92 "(92)")) 2.0000000000000005e-05)
                       ((V (66 "(66)")) 2.0000000000000005e-05)))
      (probcc-sample-importance/a (probcc-random-selector 17) 2000
                                  (expression-model (exp-stream/balanced 0.7 ml-uniformly))))
   `(doc (p "The sampling records of PS:492-501 and PS:531-546, importance sampling of 80
	    expressions of four characters and of 2000 over the more balanced streams, with "
            (code/inline "(probcc-random-selector 17)")
            ", replayed bit for bit: the characters are drawn with a transcription of upstream's "
            (code/inline "uniform") " (P:156-165), since " (code/inline "probcc-uniform")
            "'s masses are exact (deviation 7), and the parsers are the module's. With "
            (code/inline "parsec-stream-over") ", whose masses are exact, the draws are the same
	    and the two expressions come with the exact 1/2560. The runs of 30000 samples,
	    PS:504-512 and PS:549-557, are in the long suite.")))

  )

; a resume that fails to rewind its `dynamic-wind` frames would leave a test's string port as
; the current output port, hiding the summary; check that the port is back.
(import (only (chicken base) exit))

(define stdout (current-output-port))

(let1 (r (unittest/✓ hansei-logic))
  (unless (eq? stdout (current-output-port))
    (current-output-port stdout)
    (write (unittest/result-summary r))
    (newline)
    (display "FAIL: the suite ended with its output still captured by a test\n")
    (exit 1)))
