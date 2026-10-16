;; (aux hansei parsec) -- reversible parser combinators over a non-deterministic stream, for
;; (aux hansei), a CHICKEN port of Oleg Kiselyov and Chung-chieh Shan's HANSEI
;; (https://okmij.org/ftp/kakuritu/Hansei.html).
;;
;; Mirrors (paths relative to aux.scm/upstream/hansei/, citation keys as in the plan
;; AUX-HANSEI-PLAN.md §0):
;;   PS   parsec.ml (https://okmij.org/ftp/kakuritu/parsec.ml), its library part PS:22-290;
;;   LG   logic-programming.html
;;        (https://okmij.org/ftp/kakuritu/logic-programming.html), `#parsec` and `#many`,
;;        which calls it a "parser combinator library" (LG l.804, 993).
;; The combinators parse a stream forwards, and also find the streams that have a given parse
;; (parsing backwards); the examples of PS:200-501 and 514-529 (palindromes, arithmetic
;; expressions) are in the suite hansei-logic, the longer sampling runs of PS:504-512 and 531-557
;; in the suite hansei-long.  PS binds letlazy to letlazy_nesting "for the sake of 'many'" (PS:22),
;; and so does this module with probcc-letlazy/nesting.
;;
;; HANSEI code version: April 2009 (H, "Implementation"); web page last updated March 1, 2016.
;; Papers: O. Kiselyov, C. Shan, "Embedded Probabilistic Programming", IFIP DSL 2009, LNCS 5658,
;; pp. 360-384 (DSL = dsl-paper.txt / .pdf); O. Kiselyov, Y. Kameyama, "Re-thinking Prolog:
;; don't guess in a hurry", JSSST 2014 (RT = rethinking.txt / .pdf).
;;
;; Contents (plan §3.9):
;;   - streams: parsec-stream-of-string, parsec-string-of-stream, parsec-stream-over,
;;     parsec-stream-len, parsec-stream-at-most-len;
;;   - parsers: parsec-eof, parsec-pure, parsec-empty, parsec-sat, parsec-char, parsec-or,
;;     parsec-alt, parsec-ap, parsec-fmap, parsec-right, parsec-left, parsec-fix, parsec-many,
;;     parsec-many1;
;;   - running: parsec-run/forward, parsec-run/backward.
;; A stream is a thunk answering '() (`Eof`) or a pair (char . stream) (`Cons`, PS:25-26); a
;; parser is a procedure from a stream to the pair (v . stream') of its value and the rest of
;; the stream (PS:71).  Deviation 22 (PS:106-113): the names are prefixed parsec- (the infix
;; `<|>`, `<*>`, `<$>`, `*>`, `<*` are parsec-or, parsec-ap, parsec-fmap, parsec-right,
;; parsec-left: (aux category monad) exports `<*>` and `<$>` with other meanings), parsec-or is
;; binary (nest it to the left, as OCaml's infix associates), parsec-alt takes a list, and the
;; unit value `()` is (void).
;; Not ported (plan §2): `string_of_list` (PS:41-47), which list->string does; the `many`
;; without maximal munch, commented out at PS:146-151, is a contrast in the suite hansei-logic.

(module (aux hansei parsec) (parsec-stream-of-string parsec-string-of-stream parsec-stream-over
                             parsec-stream-len parsec-stream-at-most-len
                             parsec-eof parsec-pure parsec-empty parsec-sat parsec-char
                             parsec-or parsec-alt parsec-ap parsec-fmap parsec-right
                             parsec-left parsec-fix parsec-many parsec-many1
                             parsec-run/forward parsec-run/backward)

  (import scheme
          (chicken base)
          (aux base)
          (aux hansei))

  ;; ---------------------------------------------------------------------------------------
  ;; Streams (PS:24-66, 278-290): a thunk answering '() or (char . stream)
  ;; ---------------------------------------------------------------------------------------

  ; cf. PS:30-35 `stream_of_string`: the deterministic stream of the characters of the string s.
  (define (parsec-stream-of-string s)
    (let1 (n (string-length s))
      (let loop ((i 0))
        (τ (if (>= i n) '() (cons (string-ref s i) (loop (add1 i))))))))

  ; cf. PS:49-57 `string_of_stream`: the string of the characters of the stream st, every tail
  ; forced; a non-deterministic function, without any state effect (PS:49-50), so it may be
  ; resumed in several worlds.
  (define (parsec-string-of-stream st)
    (let loop ((acc '()) (c (st)))
      (match/first c
        (() (list->string (reverse acc)))
        ((,h . ,t) (loop (cons h acc) (t))))))

  ; cf. PS:59-66 `stream_over`: a random stream over the characters of the list alphabet, of a
  ; geometric length (it ends with probability 1/2 at every cell), each character chosen
  ; uniformly; every cell is a probcc-letlazy/nesting (PS:22, "for the sake of 'many'"), so
  ; it is chosen once per world, when first needed, also by an inner reification.  OCaml
  ; builds `Cons (uniformly ca, letlazy loop)` from the right (D19): the tail first, then the
  ; character.  Deviation 22: the alphabet is a list, not an array.
  (define (parsec-stream-over alphabet)
    (letrec ((loop (τ (cond
                        ((probcc-coin 1/2) (let* ((t (probcc-letlazy/nesting loop))
                                                  (c (probcc-uniform/either alphabet)))
                                             (cons c t)))
                        (else '())))))
      (probcc-letlazy/nesting loop)))

  ; cf. PS:278-283 `stream_len`: (void) when the stream st has exactly n elements, a failure
  ; otherwise; it forces at most n + 1 cells.
  (define (parsec-stream-len st n)
    (match/first (st)
      ((() ⊣ (zero? n)) (void))
      (((,_ . ,t) ⊣ (> n 0)) (parsec-stream-len t (sub1 n)))
      (else (probcc-impossible))))

  ; cf. PS:285-290 `stream_at_most_len`: (void) when the stream st has at most n elements, a
  ; failure otherwise.
  (define (parsec-stream-at-most-len st n)
    (match/first (st)
      (() (void))
      (((,_ . ,t) ⊣ (> n 0)) (parsec-stream-at-most-len t (sub1 n)))
      (else (probcc-impossible))))

  ;; ---------------------------------------------------------------------------------------
  ;; Primitive parsers (PS:70-100): a parser maps a stream to (v . stream')
  ;; ---------------------------------------------------------------------------------------

  ; cf. PS:76-81 `eof`: the parser of the end of the input, of value (void) (deviation 22).
  (define (parsec-eof st)
    (match/first (st)
      (() (cons (void) st))
      (else (probcc-impossible))))

  ; cf. PS:83-84 `pure`: the parser of the empty string with the value x.
  (define ((parsec-pure x) st) (cons x st))

  ; cf. PS:86-87 `empty`: the recognizer of the empty string, of value (void) (deviation 22).
  (define parsec-empty (parsec-pure (void)))

  ; cf. PS:89-97 `p_sat`: the parser of one character satisfying pred, its value.
  (define ((parsec-sat pred) st)
    (match/first (st)
      (((,c . ,t) ⊣ (pred c)) (cons c t))
      (else (probcc-impossible))))

  ; cf. PS:99-100 `p_char`: the parser of the character c.
  (define (parsec-char c) (parsec-sat (λ (x) (char=? x c))))

  ;; ---------------------------------------------------------------------------------------
  ;; Parser combinators (PS:103-177)
  ;; ---------------------------------------------------------------------------------------

  ; cf. PS:105-108 `<|>`: p1 or p2, chosen uniformly.  Deviation 22: binary and prefix, so
  ; `p1 <|> p2 <|> p3` is (parsec-or (parsec-or p1 p2) p3), as OCaml's infix associates.
  (define ((parsec-or p1 p2) st) ((probcc-uniform/either (list p1 p2)) st))

  ; cf. PS:110-113 `alt`: one of the parsers, chosen uniformly.  Deviation 22: a list, not an
  ; array.
  (define ((parsec-alt parsers) st) ((probcc-uniform/either parsers) st))

  ; cf. PS:115-120 `<*>`: p1, whose value is a procedure, then p2, whose value it is applied
  ; to.  OCaml's `(fun x y -> …) <$> p1 <*> p2` is (parsec-ap (parsec-fmap f p1) p2) with a
  ; curried f, (λ (x) (λ (y) …)).
  (define ((parsec-ap p1 p2) st)
    (match/first (p1 st)
      ((,f . ,st) (match/first (p2 st)
                    ((,v . ,st) (cons (f v) st))))))

  ; cf. PS:122-125 `<$>`: p, its value mapped by f; fmap f is pure f <*>.
  (define ((parsec-fmap f p) st)
    (match/first (p st)
      ((,v . ,st) (cons (f v) st))))

  ; cf. PS:127-133 `*>`: p1 then p2, the value of p2.  OCaml's `*>` binds tighter than `<*`, so
  ; `p1 *> p2 <* p3` is (parsec-left (parsec-right p1 p2) p3).
  (define ((parsec-right p1 p2) st)
    (match/first (p1 st)
      ((,_ . ,st) (p2 st))))

  ; cf. PS:135-139 `<*`: p1 then p2, the value of p1.
  (define ((parsec-left p1 p2) st)
    (match/first (p1 st)
      ((,v . ,st) (match/first (p2 st)
                    ((,_ . ,st) (cons v st))))))

  ; cf. PS:141-144 `p_fix`: the fixed point of f, a procedure from a parser to a parser.
  (define (parsec-fix f)
    (letrec ((p (λ (st) ((f p) st))))
      p))

  ; cf. PS:154-171 `many` (LG l.1188-1195): zero or more p, with maximal munch, Prolog's
  ; `many(P,S,Rest) :- call(P,S,Srem) *-> many(P,Srem,Rest) ; S = Rest.` (LG l.1073): when p
  ; parses the stream in some world (probcc-soft-cut), the parse goes on from where p stopped,
  ; and the empty list is the value only when p fails in every world.  The inner reification
  ; of probcc-soft-cut reifies only the choices of p; a stream cell it forces is chosen by the
  ; outer world (probcc-letlazy/nesting), so many p (s1 | s2) = many p s1 | many p s2 and
  ; parsing backwards loses no solution (LG l.1198-1222).
  (define (parsec-many p)
    (letrec ((self (λ (st)
                     (probcc-soft-cut (τ (p st))
                                      (λ1-match/first
                                        ((,v . ,st) (match/first (self st)
                                                      ((,vs . ,st) (cons (cons v vs) st)))))
                                      (τ (cons '() st))))))
      self))

  ; cf. PS:174-177 `many1`: one or more p, with maximal munch.
  (define (parsec-many1 p)
    (parsec-ap (parsec-fmap (λ (x) (λ (xs) (cons x xs))) p) (parsec-many p)))

  ;; ---------------------------------------------------------------------------------------
  ;; Running parsers (PS:179-184, 253-265)
  ;; ---------------------------------------------------------------------------------------

  ; cf. PS:179-184 `run_fwd`: the exact distribution of the values of p that parses the whole
  ; string s, forwards; a parse that leaves some input fails.
  (define (parsec-run/forward p s)
    (probcc-reify/exact
      (match/first (p (parsec-stream-of-string s))
        ((,v . ,st) (match/first (st)
                      (() v)
                      (else (probcc-impossible)))))))

  ; cf. PS:253-265 `run_bwd`: parsing backwards, the streams that p parses whole, among those
  ; of the stream generator stm (a thunk answering a stream), explored to the depth bound
  ; (a number, or #f for `None`): a pV of the pairs (list v string) of p's value and the
  ; string of the stream, whose unexplored branches are kept.  Deviation 25: the pair is a
  ; list.
  (define (parsec-run/backward bound p stm)
    (probcc-explore bound
                    (probcc-reify/0
                      (τ (let1 (st (stm))
                           (match/first (p st)
                             ((,v . ,st*) (match/first (st*)
                                            (() (list v (parsec-string-of-stream st)))
                                            (else (probcc-impossible))))))))))

  )
