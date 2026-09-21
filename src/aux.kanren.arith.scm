; https://github.com/TheReasonedSchemer2ndEd/CodeFromTheReasonedSchemer2ndEd/blob/master/trs2-arith.scm

;;; Copyright © 2018 Daniel P. Friedman, William E. Byrd, Oleg Kiselyov, and Jason Hemann
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the “Software”), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(module (aux kanren arith) *

  ;;; The arithmetic system of 'The Reasoned Schemer, Second Edition,' by
  ;;; Friedman, Byrd, Kiselyov, and Hemann (MIT Press, 2018), translated into
  ;;; the (aux kanren micro) idiom: `defrel` becomes `define-relation`, `conde`
  ;;; becomes `cond°`, `fresh` becomes `fresh°`, `==` becomes `=°`, and the
  ;;; book's trailing `o` becomes a trailing `°` throughout.

  ;;; Definitions are presented in the order in which they appear in
  ;;; Chapters 7 and 8.  The additions are a block after `<=°' -- the mirrored
  ;;; comparisons `>l°', `>=l°', `>°' and `>=°', and `multiple°' -- which the
  ;;; book never needs and so never writes; each is marked where it is defined.

  ;;; As in the book, there are three definitions of '/°'.  The first two,
  ;;; flawed definitions, are commented out using Scheme's '#;' convention.
  ;;; The final definition of '/°' is uncommented.
  ;;;
  ;;; If you wish work through the Chapter 8 one frame at a time, then
  ;;; please comment out the final definition of '/°' by adding a '#;'
  ;;; immediately before the '(define-relation (/° ...) ...)', and uncomment
  ;;; the appropriate definition of '/°' as you encounter it while reading
  ;;; this chapter.

  ;;; Nothing else has to be loaded: (aux kanren micro) is the implementation,
  ;;; and `δ°` below is what keeps a goal tree finite -- goal construction in
  ;;; this µKanren is *eager*, so a recursive call sharing a clause with other
  ;;; goals is evaluated while the tree is being built and never returns.

  (import scheme (chicken base) (aux base) (aux kanren micro))

  ;;; Goal construction in (aux kanren micro) is *eager*: `and°`/`or°`/`cond°`
  ;;; expand into calls to the procedures `andª`/`orª`, so every goal sub-expression of a
  ;;; clause is evaluated while the goal tree is being built.  A direct recursive
  ;;; call therefore has to be protected by an eta-expansion plus a `δ` (delay),
  ;;; unless it already sits inside a `fresh°` body (which is a λ, hence a
  ;;; natural delay).  `δ°` is that protection.
  (define-syntax-rule (δ° g) (μ s (δ (g s))))



  ; Helper definitions from Chapters 2 and 4.
  ;
  ; `null°` and `cons°` are already provided by (aux kanren micro):
  ;   (define (null° l) (=° l '()))
  ;   (define (cons° a d c) (=° c (cons a d)))
  ; so we do not redefine them here; only `car°`, `cdr°` and `append°` are new.

  (define-relation (car° p a)
    (fresh° (d)
      (=° (cons a d) p)))

  (define-relation (cdr° p d)
    (fresh° (a)
      (=° (cons a d) p)))

  (define-relation (append° l t out)
    (cond°
      ((null° l) (=° t out))
      ((fresh° (a d res)
         (cons° a d l)
         (cons° a res out)
         (append° d t res)))))



  ;;; Here are the key parts of Chapter 7
  (define-relation (bit-xor° x y r)
    (cond°
      ((=° 0 x) (=° 0 y) (=° 0 r))
      ((=° 0 x) (=° 1 y) (=° 1 r))
      ((=° 1 x) (=° 0 y) (=° 1 r))
      ((=° 1 x) (=° 1 y) (=° 0 r))))

  (define-relation (bit-and° x y r)
    (cond°
      ((=° 0 x) (=° 0 y) (=° 0 r))
      ((=° 1 x) (=° 0 y) (=° 0 r))
      ((=° 0 x) (=° 1 y) (=° 0 r))
      ((=° 1 x) (=° 1 y) (=° 1 r))))


  (define-relation (half-adder° x y r c)
    (bit-xor° x y r)
    (bit-and° x y c))

  ; Alternative definition of 'half-adder°' from frame 7:12 on page 87.
  #;(define-relation (half-adder° x y r c)
    (cond°
      ((=° 0 x) (=° 0 y) (=° 0 r) (=° 0 c))
      ((=° 1 x) (=° 0 y) (=° 1 r) (=° 0 c))
      ((=° 0 x) (=° 1 y) (=° 1 r) (=° 0 c))
      ((=° 1 x) (=° 1 y) (=° 0 r) (=° 1 c))))



  ; Definition of 'full-adder°' from frame 7:15 on page 87.
  #;(define-relation (full-adder° b x y r c)
    (fresh° (w xy wz)
      (half-adder° x y w xy)
      (half-adder° w b r wz)
      (bit-xor° xy wz c)))

  ; Alternative definition of 'full-adder°' from frame 7:15 on page 87.
  ;
  ; For performance reasons, we use this explicit table version of
  ; 'full-adder°' (which no longer uses 'half-adder°').
  (define-relation (full-adder° b x y r c)
    (cond°
      ((=° 0 b) (=° 0 x) (=° 0 y) (=° 0 r) (=° 0 c))
      ((=° 1 b) (=° 0 x) (=° 0 y) (=° 1 r) (=° 0 c))
      ((=° 0 b) (=° 1 x) (=° 0 y) (=° 1 r) (=° 0 c))
      ((=° 1 b) (=° 1 x) (=° 0 y) (=° 0 r) (=° 1 c))
      ((=° 0 b) (=° 0 x) (=° 1 y) (=° 1 r) (=° 0 c))
      ((=° 1 b) (=° 0 x) (=° 1 y) (=° 0 r) (=° 1 c))
      ((=° 0 b) (=° 1 x) (=° 1 y) (=° 0 r) (=° 1 c))
      ((=° 1 b) (=° 1 x) (=° 1 y) (=° 1 r) (=° 1 c))))


  (define (build-num n)
    (cond
      ((zero? n) '())
      ((even? n)
       (cons 0
         (build-num (quotient n 2))))
      ((odd? n)
       (cons 1
         (build-num (quotient (- n 1) 2))))))

  (define-relation (pos° n)
    (fresh° (a d)
      (=° `(,a . ,d) n)))

  (define-relation (>1° n)
    (fresh° (a ad dd)
      (=° `(,a ,ad . ,dd) n)))

  ; `adder°`'s body is a bare `cond°`, i.e. an ordinary call, so the three
  ; self-calls below are evaluated while `adder°` is building its own goal tree
  ; and each one must be delayed with `δ°`; without that the very first call to
  ; `adder°` would never finish.  The two calls to `gen-adder°` need no wrapper:
  ; what decides is the callee's body, not the call site, and `gen-adder°`'s body
  ; is a single `fresh°`, i.e. `(freshª (λ ...))`, so calling it merely builds a λ
  ; and returns.
  (define-relation (adder° b n m r)
    (cond°
      ((=° 0 b) (=° '() m) (=° n r))
      ((=° 0 b) (=° '() n) (=° m r)
       (pos° m))
      ((=° 1 b) (=° '() m)
       (δ° (adder° 0 n '(1) r)))
      ((=° 1 b) (=° '() n) (pos° m)
       (δ° (adder° 0 '(1) m r)))
      ((=° '(1) n) (=° '(1) m)
       (fresh° (a c)
         (=° `(,a ,c) r)
         (full-adder° b 1 1 a c)))
      ((=° '(1) n) (gen-adder° b n m r))
      ((=° '(1) m) (>1° n) (>1° r)
       (δ° (adder° b '(1) n r)))
      ((>1° n) (gen-adder° b n m r))))

  (define-relation (gen-adder° b n m r)
    (fresh° (a c d e x y z)
      (=° `(,a . ,x) n)
      (=° `(,d . ,y) m) (pos° y)
      (=° `(,c . ,z) r) (pos° z)
      (full-adder° b a d c e)
      (adder° e x y z)))

  (define-relation (plus° n m k)
    (adder° 0 n m k))

  (define-relation (minus° n m k)
    (plus° m k n))

  ;;; Here are the key parts of Chapter 8
  (define-relation (*° n m p)
    (cond°
      ((=° '() n) (=° '() p))
      ((pos° n) (=° '() m) (=° '() p))
      ((=° '(1) n) (pos° m) (=° m p))
      ((>1° n) (=° '(1) m) (=° n p))
      ((fresh° (x z)
         (=° `(0 . ,x) n) (pos° x)
         (=° `(0 . ,z) p) (pos° z)
         (>1° m)
         (*° x m z)))
      ((fresh° (x y)
         (=° `(1 . ,x) n) (pos° x)
         (=° `(0 . ,y) m) (pos° y)
         (*° m n p)))
      ((fresh° (x y)
         (=° `(1 . ,x) n) (pos° x)
         (=° `(1 . ,y) m) (pos° y)
         (odd-*° x n m p)))))

  (define-relation (odd-*° x n m p)
    (fresh° (q)
      (bound-*° q p n m)
      (*° x m q)
      (plus° `(0 . ,q) m p)))

  (define-relation (bound-*° q p n m)
    (cond°
      ((=° '() q) (pos° p))
      ((fresh° (a0 a1 a2 a3 x y z)
         (=° `(,a0 . ,x) q)
         (=° `(,a1 . ,y) p)
         (cond°
           ((=° '() n)
            (=° `(,a2 . ,z) m)
            (bound-*° x y z '()))
           ((=° `(,a3 . ,z) n)
            (bound-*° x y z m)))))))

  (define-relation (=l° n m)
    (cond°
      ((=° '() n) (=° '() m))
      ((=° '(1) n) (=° '(1) m))
      ((fresh° (a x b y)
         (=° `(,a . ,x) n) (pos° x)
         (=° `(,b . ,y) m) (pos° y)
         (=l° x y)))))

  (define-relation (<l° n m)
    (cond°
      ((=° '() n) (pos° m))
      ((=° '(1) n) (>1° m))
      ((fresh° (a x b y)
         (=° `(,a . ,x) n) (pos° x)
         (=° `(,b . ,y) m) (pos° y)
         (<l° x y)))))

  (define-relation (<=l° n m)
    (cond°
      ((=l° n m))
      ((<l° n m))))

  (define-relation (<° n m)
    (cond°
      ((<l° n m))
      ((=l° n m)
       (fresh° (x)
         (pos° x)
         (plus° n x m)))))

  (define-relation (<=° n m)
    (cond°
      ((=° n m))
      ((<° n m))))

  ; The book stops here: `/°` and `log°` only ever ask whether something is
  ; SMALLER, so `>` and `>=` are never written down.  A relation has no
  ; preferred direction, though -- the remaining four comparisons are the same
  ; goals with their arguments crossed, exactly as the book defines `minus°`
  ; as `plus°` read backwards.  Both families are mirrored: `>l°`/`>=l°`
  ; compare the length of the numeral, `>°`/`>=°` compare its value.
  (define-relation (>l° n m) (<l° m n))
  (define-relation (>=l° n m) (<=l° m n))
  (define-relation (>° n m) (<° m n))
  (define-relation (>=° n m) (<=° m n))

  ; `b` is a multiple of `a` exactly when some `k` makes a * k = b, which is
  ; the whole definition -- `*°` supplies the search and `bound-*°` supplies
  ; the termination, so a non-multiple FAILS rather than diverging.
  ;
  ; Read the other way round it enumerates divisors, and the enumeration is
  ; exhaustive: asking for more divisors of 12 than exist answers 1, 12, 2, 4,
  ; 3, 6 and closes the stream, so `(multiple° a n)` with `a` fresh is a
  ; factorisation and a primality test at once.
  ;
  ; The degenerate rows follow from a * k = b and are worth stating: every `a`
  ; divides zero (k = 0), zero divides only itself (a * k = 0 forces b = 0),
  ; and one divides everything.
  (define-relation (multiple° a b) (fresh° (k) (*° a k b)))

  ; There is deliberately no `=°` or `≠°` for numerals here.  `build-num` is
  ; canonical -- little-endian, no trailing zero -- so two numerals denote the
  ; same number exactly when their lists unify, and `(aux kanren micro)`'s own
  ; `=°` and `≠°` already decide that.  A wrapper would only hide which one is
  ; doing the work.  Note the difference in strength, though: `(=° n m)` on two
  ; fresh variables unifies them without making either a numeral, whereas
  ; `(<=° n m)` constrains both to be numerals on the way to answering.

  ; Flawed definition of '/°' from frame 8:54 on page 118.
  #;(define-relation (/° n m q r)
    (cond°
      ((=° '() q) (=° n r) (<° n m))
      ((=° '(1) q) (=° '() r) (=° n m)
       (<° r m))
      ((<° m n) (<° r m)
       (fresh° (mq)
         (<=l° mq n)
         (*° m q mq)
         (plus° mq r n)))))

  ; Flawed definition of '/°' from frame 8:64 on page 120.
  #;(define-relation (/° n m q r)
    (fresh° (mq)
      (<° r m)
      (<=l° mq n)
      (*° m q mq)
      (plus° mq r n)))

  (define-relation (split° n r l h)
    (cond°
      ((=° '() n) (=° '() h) (=° '() l))
      ((fresh° (b n^)
         (=° `(0 ,b . ,n^) n) (=° '() r)
         (=° `(,b . ,n^) h) (=° '() l)))
      ((fresh° (n^)
         (=°  `(1 . ,n^) n) (=° '() r)
         (=° n^ h) (=° '(1) l)))
      ((fresh° (b n^ a r^)
         (=° `(0 ,b . ,n^) n)
         (=° `(,a . ,r^) r) (=° '() l)
         (split° `(,b . ,n^) r^ '() h)))
      ((fresh° (n^ a r^)
         (=° `(1 . ,n^) n)
         (=° `(,a . ,r^) r) (=° '(1) l)
         (split° n^ r^ '() h)))
      ((fresh° (b n^ a r^ l^)
         (=° `(,b . ,n^) n)
         (=° `(,a . ,r^) r)
         (=° `(,b . ,l^) l)
         (pos° l^)
         (split° n^ r^ l^ h)))))

  ; Final definition of '/°' from frame 8:81 on page 124.
  (define-relation (/° n m q r)
    (cond°
      ((=° '() q) (=° r n) (<° n m))
      ((=° '(1) q) (=l° m n) (plus° r m n)
       (<° r m))
      ((pos° q) (<l° m n) (<° r m)
       (n-wider-than-m° n m q r))))

  (define-relation (n-wider-than-m° n m q r)
    (fresh° (nh nl qh ql)
      (fresh° (mql mrql rr rh)
        (split° n r nl nh)
        (split° q r ql qh)
        (cond°
          ((=° '() nh)
           (=° '() qh)
           (minus° nl r mql)
           (*° m ql mql))
          ((pos° nh)
           (*° m ql mql)
           (plus° r mql mrql)
           (minus° mrql nl rr)
           (split° rr r '() rh)
           (/° nh m qh rh))))))

  (define-relation (log° n b q r)
    (cond°
      ((=° '() q) (<=° n b)
       (plus° r '(1) n))
      ((=° '(1) q) (>1° b) (=l° n b)
       (plus° r b n))
      ((=° '(1) b) (pos° q)
       (plus° r '(1) n))
      ((=° '() b) (pos° q) (=° r n))
      ((=° '(0 1) b)
       (fresh° (a ad dd)
         (pos° dd)
         (=° `(,a ,ad . ,dd) n)
         (exp2° n '() q)
         (fresh° (s)
           (split° n dd r s))))
      ((<=° '(1 1) b) (<l° b n)
       (base-three-or-more° n b q r))))

  (define-relation (exp2° n b q)
    (cond°
      ((=° '(1) n) (=° '() q))
      ((>1° n) (=° '(1) q)
       (fresh° (s)
         (split° n b s '(1))))
      ((fresh° (q1 b2)
         (=° `(0 . ,q1) q) (pos° q1)
         (<l° b n)
         (append° b `(1 . ,b) b2)
         (exp2° n b2 q1)))
      ((fresh° (q1 nh b2 s)
         (=° `(1 . ,q1) q) (pos° q1)
         (pos° nh)
         (split° n b s nh)
         (append° b `(1 . ,b) b2)
         (exp2° nh b2 q1)))))

  (define-relation (base-three-or-more° n b q r)
    (fresh° (bw1 bw nw nw1 ql1 ql s)
      (exp2° b '() bw1)
      (plus° bw1 '(1) bw)
      (<l° q n)
      (fresh° (q1 bwq1)
        (plus° q '(1) q1)
        (*° bw q1 bwq1)
        (<° nw1 bwq1))
      (exp2° n '() nw1)
      (plus° nw1 '(1) nw)
      (/° nw bw ql1 s)
      (plus° ql '(1) ql1)
      (<=l° ql q)
      (fresh° (bql qh s qdh qd)
        (repeated-mul° b ql bql)
        (/° nw bw1 qh s)
        (plus° ql qdh qh)
        (plus° ql qd q)
        (<=° qd qdh)
        (fresh° (bqd bq1 bq)
          (repeated-mul° b qd bqd)
          (*° bql bqd bq)
          (*° b bq bq1)
          (plus° bq r n)
          (<° n bq1)))))

  (define-relation (repeated-mul° n q nq)
    (cond°
      ((pos° n) (=° '() q) (=° '(1) nq))
      ((=° '(1) q) (=° n nq))
      ((>1° q)
       (fresh° (q1 nq1)
         (plus° q1 '(1) q)
         (repeated-mul° n q1 nq1)
         (*° nq1 n nq)))))

  (define-relation (exp° b q n)
    (log° n b q '()))
)
