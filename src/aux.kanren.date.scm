(module (aux kanren date) *

  ;;; Relations over instants in time, in the idiom of (aux kanren arith): the
  ;;; book's trailing `o` is a trailing `°`, `defrel` is `define-relation`,
  ;;; `conde` is `cond°`.
  ;;;
  ;;; ==================== THE REPRESENTATION ====================
  ;;;
  ;;; An INSTANT is a POSIX time -- seconds since 1970-01-01T00:00:00Z -- held
  ;;; as a canonical little-endian bit list, exactly the numeral that
  ;;; `build-num` produces in (aux kanren arith).  Nothing else is added: an
  ;;; instant IS a numeral, so every relation of that module already applies to
  ;;; it, and this one contributes names and a calendar rather than a datatype.
  ;;;
  ;;; That choice has three consequences the reader must know before writing a
  ;;; single goal, and all three were MEASURED on this implementation, not
  ;;; guessed:
  ;;;
  ;;;   * a current instant (1_760_000_000) is a 31-BIT numeral;
  ;;;   * `plus°`, `minus°`, `<°`, `<=°` at 31 bits cost 3-11 ms -- comparison,
  ;;;     duration and shift are cheap and run in every direction;
  ;;;   * `*°` at that magnitude costs seconds, and `/°` of an instant by 86400
  ;;;     NEVER FINISHES -- killed after ten minutes.
  ;;;
  ;;; So the calendar cannot be reached by relational division.  Any design
  ;;; that tries is not slow, it is broken.
  ;;;
  ;;; ==================== THE SHAPE OF THE MODULE ====================
  ;;;
  ;;; The way out is to change magnitude before doing anything calendrical.  A
  ;;; DAY-COUNT since the epoch is a 15-bit numeral, and at 15 bits everything
  ;;; that is impossible at 31 bits becomes merely expensive: `/°` of a
  ;;; day-count by 7 takes under a second, and `numeral°` enumerates day-counts
  ;;; at a millisecond for twelve of them.  That second figure is the real
  ;;; prize, and it is worth being precise about WHY, because the obvious
  ;;; reason is the wrong one.  Striding is only about 2.5x cheaper at 15 bits
  ;;; than at 31 (measured: 21 ms a step against 53), which on its own would
  ;;; not justify a seam.  What the change of magnitude buys is that the OPEN
  ;;; MODES BECOME POSSIBLE: at 15 bits a pure generator can put a ground
  ;;; day-count in front of a guarded relation for a millisecond, so every
  ;;; `project°` in this file can have a real relation as its fallback instead
  ;;; of an error.  At 31 bits the same fallback would have to walk 2^31
  ;;; seconds.  The module is therefore built in layers around one seam, and
  ;;; the seam is the day-count -- not the instant.
  ;;;
  ;;;   LAYER 1  instant algebra, PURE.  `plus°`/`<°`/`<=°`/`between°` dressed
  ;;;            as `before°`, `elapsed°`, `within°`, `stride°`, `span°`.  It
  ;;;            names nothing calendrical, which is what lets the identical
  ;;;            relations serve instants (31 bits) and day-counts (15 bits).
  ;;;
  ;;;   LAYER 2  THE SEAM, `project°`-guarded: `day+second° t d sod`, the one
  ;;;            place in the file where 86400 is written, and the only place
  ;;;            where division would be needed.
  ;;;
  ;;;   LAYER 3  the civil calendar, over DAY-COUNTS: `civil°`, `clock°`,
  ;;;            `day-of-week°` and the seven weekday names.  `project°`-guarded.
  ;;;
  ;;;   LAYER 4  composition only, no new arithmetic: `date-time°`, `iso°`,
  ;;;            `weekday°`, `weekday-midnight°`.
  ;;;
  ;;; Each layer is announced by a banner below.  A reader scrolling the file
  ;;; should never be in doubt about whether the relation they are about to use
  ;;; is a real relation: the pure ones are in layer 1 and in the derived
  ;;; wrappers marked PURE, and every `project°` in the file is preceded by a
  ;;; comment that says which directions are sound.
  ;;;
  ;;; ==================== WHAT THE GUARD MEANS HERE ====================
  ;;;
  ;;; `prime°` in (aux kanren arith) is the precedent: one impure relation,
  ;;; clearly marked, made sound by putting a generator in front of it.  This
  ;;; module follows it, and goes one step further wherever it can.  Every
  ;;; `project°` below has an `else` branch, and that branch is not an error --
  ;;; it is the relation's own PURE definition, or a pure generator followed by
  ;;; a re-entry.  The guard is therefore an OPTIMISATION over a total
  ;;; relation, not a replacement for one: it changes the clock, never the
  ;;; answer set.  `(day+second° t d sod)` with nothing ground really does
  ;;; answer (0,0,0), (86400,1,0), (1,0,1), ...; `(civil° day y m d)` with
  ;;; nothing ground really does enumerate 1970-01-01, 1970-01-02, ...
  ;;;
  ;;; The price is that the `else` branches are only USEFUL at small
  ;;; magnitudes.  `(day+second° t d sod)` with `d` ground and `sod` fresh is
  ;;; sound and takes a day's worth of `*°` to say so.  Prefer to ground one
  ;;; side; the fallbacks are there so that forgetting is not undefined
  ;;; behaviour.
  ;;;
  ;;; ==================== WHAT IS NOT MODELLED ====================
  ;;;
  ;;;   * UTC only.  No timezones, no DST.
  ;;;   * POSIX time: every day is exactly 86400 seconds.  There are no leap
  ;;;     seconds, and `23:59:60` is rejected as a clock reading.
  ;;;   * The numeral of (aux kanren arith) is UNSIGNED, so NOTHING BEFORE
  ;;;     1970-01-01T00:00:00Z exists in this module.  A pre-epoch date fails
  ;;;     (or, at the Scheme helpers, raises) rather than wrapping round, and
  ;;;     `(elapsed° a d b)` with `b` earlier than `a` simply fails: there are
  ;;;     no negative durations in this representation.  That is a consequence
  ;;;     of the encoding, not a design choice.
  ;;;   * ISO 8601 is the fixed 20-character form "YYYY-MM-DDTHH:MM:SSZ".  A
  ;;;     year past 9999 has no spelling here and the parser rejects anything
  ;;;     that is not exactly that shape.
  ;;;   * ANSWER ORDER is the µKanren interleaving, not chronology.  The
  ;;;     `stride°` family does come out increasing; anything built on
  ;;;     `between°`/`or°` does not.  Read every expectation off a run.

  (import scheme
          (chicken base)
          (chicken format)
          srfi-1
          srfi-13
          (aux base)
          (aux kanren micro)
          (aux kanren arith))

  ;;; Goal construction in (aux kanren micro) is EAGER, exactly as
  ;;; (aux kanren arith) explains: a recursive call sharing a `cond°` clause
  ;;; with other goals is evaluated while the tree is being built and never
  ;;; returns.  `δ°` is the protection, and it is needed here for the same
  ;;; reason and in the same places -- it comes in with (aux kanren arith),
  ;;; which exports it, and is deliberately NOT redefined.


  ;;; ==============================================================
  ;;; LAYER 0 -- plain Scheme.  NOT relations.  No logic variable ever
  ;;; reaches this section; it is the counterpart of `build-num`.
  ;;; ==============================================================

  (define seconds/minute 60)
  (define seconds/hour 3600)
  (define seconds/day 86400)
  (define days/week 7)

  ;;; `quotient` truncates toward zero.  Everything in this module is
  ;;; non-negative, so the two agree -- but the calendar kernel below is the
  ;;; proleptic Gregorian one and is correct before the epoch too, and it stays
  ;;; correct only if the division is a FLOOR division.  Writing it once here
  ;;; costs nothing and removes a whole class of off-by-a-day.
  (define (floor-quotient a b)
    (let ((q (quotient a b)) (r (remainder a b)))
      (if (and (not (zero? r)) (negative? (* r b))) (- q 1) q)))

  (define (floor-remainder a b) (- a (* b (floor-quotient a b))))

  ;;; Howard Hinnant's days_from_civil / civil_from_days: exact, integer-only,
  ;;; table-free, proleptic Gregorian, with day 0 = 1970-01-01.  This is the
  ;;; whole calendar; the leap-year rule is in the `146097`-day era, not in a
  ;;; special case.
  (define (days-from-civil y m d)
    (let* ((y (if (<= m 2) (- y 1) y))
           (era (floor-quotient y 400))
           (yoe (- y (* era 400)))
           (doy (+ (quotient (+ (* 153 (+ m (if (> m 2) -3 9))) 2) 5) (- d 1)))
           (doe (+ (* yoe 365) (quotient yoe 4) (- (quotient yoe 100)) doy)))
      (+ (* era 146097) doe -719468)))

  (define (civil-from-days z)
    (let* ((z (+ z 719468))
           (era (floor-quotient z 146097))
           (doe (- z (* era 146097)))
           (yoe (quotient (- doe (quotient doe 1460) (- (quotient doe 36524)) (quotient doe 146096)) 365))
           (y (+ yoe (* era 400)))
           (doy (- doe (+ (* 365 yoe) (quotient yoe 4) (- (quotient yoe 100)))))
           (mp (quotient (+ (* 5 doy) 2) 153))
           (d (+ (- doy (quotient (+ (* 153 mp) 2) 5)) 1))
           (m (+ mp (if (< mp 10) 3 -9))))
      (list (if (<= m 2) (+ y 1) y) m d)))

  ;;; 1970-01-01 is day 0 and a THURSDAY -- verified against
  ;;; `(chicken time posix) seconds->utc-time`, not assumed -- so the weekday of
  ;;; day `z` is `(z + 4) mod 7` with Sunday at 0, which is the convention
  ;;; `seconds->utc-time` itself uses.  The first Monday of the epoch is
  ;;; therefore day 4 = 1970-01-05 = unixtime 345600.
  (define weekday-names '#(sunday monday tuesday wednesday thursday friday saturday))

  ;;; `weekday-epoch-day` is `weekday-names` read backwards: the SMALLEST
  ;;; day-count carrying each weekday, which is where a weekday stride starts.
  (define weekday-epoch-day '#(3 4 5 6 0 1 2))

  (define (weekday-of-day z) (vector-ref weekday-names (floor-remainder (+ z 4) days/week)))

  (define (weekday-index w)
    (let loop ((i 0))
      (cond ((= i days/week) #f)
            ((eq? w (vector-ref weekday-names i)) i)
            (else (loop (+ i 1))))))

  (define (weekday-name? w) (and (symbol? w) (weekday-index w) #t))

  ;;; numeral <-> integer.  `numeral->number` is the groundness test as well as
  ;;; the decoder: it returns #f unless the argument is a proper list of 0/1
  ;;; cells, so a HALF-ground numeral such as `(1 0 . β)` is correctly not
  ;;; taken for ground.  Groundness by positive typing, never by `(not (var? x))`.
  (define (numeral->number n)
    (let loop ((n n) (w 1) (a 0))
      (cond ((null? n) a)
            ((and (pair? n) (memv (car n) '(0 1))) (loop (cdr n) (* 2 w) (+ a (* w (car n)))))
            (else #f))))

  (define (numeral? n) (and (numeral->number n) #t))

  ;;; all of `ns` ground -> the list of their values; otherwise #f.
  (define (numerals->numbers ns)
    (let loop ((ns ns) (acc '()))
      (cond ((null? ns) (reverse acc))
            ((numeral->number (car ns)) => (lambda (v) (loop (cdr ns) (cons v acc))))
            (else #f))))

  ;;; `build-num` of a negative integer recurses forever.  Everything in this
  ;;; module that turns an integer into a term goes through `number->numeral`,
  ;;; which raises instead: a hang is a worse answer than an error, and the
  ;;; epoch floor is a real edge that a user WILL hit.
  (define (number->numeral n)
    (if (negative? n)
        (error 'number->numeral
               "before 1970-01-01T00:00:00Z: the numeral of (aux kanren arith) is unsigned" n)
        (build-num n)))

  ;;; Duration builders, so that a shift reads as a duration and not as a
  ;;; multiplication.  The arithmetic is done in Scheme at goal-construction
  ;;; time precisely BECAUSE `*°` at this magnitude costs seconds.
  (define (seconds n) (number->numeral n))
  (define (minutes n) (number->numeral (* seconds/minute n)))
  (define (hours n)   (number->numeral (* seconds/hour n)))
  (define (days n)    (number->numeral (* seconds/day n)))
  (define (weeks n)   (number->numeral (* seconds/day days/week n)))

  ;;; The two term builders a caller actually types.  `build-day` yields a
  ;;; DAY-COUNT numeral (layers 2 and 3 speak this); `build-instant` yields an
  ;;; INSTANT numeral (layers 1 and 4 speak this).
  (define (build-day y m d)
    (let ((z (days-from-civil y m d)))
      (if (equal? (list y m d) (civil-from-days z))
          (number->numeral z)
          (error 'build-day "not a date" y m d))))

  (define (build-instant y m d #!optional (hh 0) (mi 0) (ss 0))
    (unless (and (<= 0 hh 23) (<= 0 mi 59) (<= 0 ss 59))
      (error 'build-instant "not a clock reading" hh mi ss))
    (let ((z (days-from-civil y m d)))
      (if (equal? (list y m d) (civil-from-days z))
          (number->numeral (+ (* z seconds/day) (* hh seconds/hour) (* mi seconds/minute) ss))
          (error 'build-instant "not a date" y m d))))

  ;;; The module exports `*`, so every name here is public: `zero-pad` is
  ;;; spelled long precisely because a bare `pad` would be a collision waiting to
  ;;; happen at an importer's site.
  (define (zero-pad n w) (string-pad (number->string n) w #\0))

  (define (instant->iso t)
    (let* ((z (floor-quotient t seconds/day))
           (sod (floor-remainder t seconds/day))
           (c (civil-from-days z)))
      (sprintf "~a-~a-~aT~a:~a:~aZ"
               (zero-pad (car c) 4) (zero-pad (cadr c) 2) (zero-pad (caddr c) 2)
               (zero-pad (quotient sod seconds/hour) 2)
               (zero-pad (quotient (remainder sod seconds/hour) seconds/minute) 2)
               (zero-pad (remainder sod seconds/minute) 2))))

  ;;; The parser VALIDATES and returns #f rather than a neighbouring instant:
  ;;; "2025-02-30T00:00:00Z" is not a date and must not silently become March 2.
  ;;; The round trip through `civil-from-days` is what decides that, because the
  ;;; calendar is a bijection and a tuple the forward map does not return is not
  ;;; a date.  A pre-epoch string is rejected here too.
  (define (iso->instant s)
    (and (string? s)
         (= 20 (string-length s))
         (char=? #\T (string-ref s 10))
         (char=? #\Z (string-ref s 19))
         (let ((f (lambda (a b) (string->number (substring s a b)))))
           (let ((y (f 0 4)) (m (f 5 7)) (d (f 8 10))
                 (hh (f 11 13)) (mi (f 14 16)) (ss (f 17 19)))
             (and y m d hh mi ss
                  (<= 1 m 12) (<= 1 d 31) (<= 0 hh 23) (<= 0 mi 59) (<= 0 ss 59)
                  (let ((z (days-from-civil y m d)))
                    (and (>= z 0)
                         (equal? (list y m d) (civil-from-days z))
                         (+ (* z seconds/day) (* hh seconds/hour) (* mi seconds/minute) ss))))))))


  ;;; ==============================================================
  ;;; LAYER 1 -- INSTANT ALGEBRA.  PURE, every one of them.
  ;;;
  ;;; Nothing below calls `project°`, `*°` or `/°`.  Nothing below knows what
  ;;; a calendar is, and that is deliberate: because these relations name no
  ;;; magnitude, the SAME goals serve instants (31 bits, 3-11 ms) and
  ;;; day-counts (15 bits, well under a millisecond), and layer 3 reuses them
  ;;; verbatim.  This layer is the reason the representation is a good idea.
  ;;; ==============================================================

  ;;; Ordering.  One line each, and deliberately no body of their own: the
  ;;; point is that a date query never has to remember whether `<°` compares
  ;;; numerals or seconds, and hiding arithmetic behind the wrapper would be
  ;;; the one thing that could make ordering expensive.
  ;;;
  ;;; Both ground: a decision procedure, 15-16 ms at instant magnitude.  ONE
  ;;; ARGUMENT FRESH: an infinite generator, inherited verbatim from `<°` --
  ;;; `(before° a b)` with `b` fresh never closes, and must be bounded by
  ;;; `within°`, `span°` or `take°`, exactly as in (aux kanren arith).
  (define-relation (before° t u) (<° t u))
  (define-relation (after° t u) (<° u t))
  (define-relation (not-after° t u) (<=° t u))
  (define-relation (not-before° t u) (<=° u t))
  (define-relation (same-instant° t u) (=° t u))

  ;;; `u` is `t` moved on by the duration `d`; equivalently `d` is the time
  ;;; that passes between `t` and `u`.  ONE relation, named twice, because the
  ;;; call sites read differently -- the same gesture by which
  ;;; (aux kanren arith) defines `minus°` as `plus°` read backwards.
  ;;;
  ;;; All three directions are sound and cheap, measured on a same-day pair:
  ;;; (t,u) -> d in 11 ms, (t,d) -> u in 2 ms, (d,u) -> t by the same goal.
  ;;; Durations are non-negative, so `(elapsed° t d u)` with `u` earlier than
  ;;; `t` FAILS -- it does not answer with a negative duration, because this
  ;;; representation has none.
  (define-relation (elapsed° t d u) (plus° t d u))
  (define-relation (shift° t d u) (plus° t d u))

  ;;; `t` lies in the closed window [lo,hi].
  ;;;
  ;;; As a TEST (all three ground) this is the natural spelling and costs about
  ;;; 30 ms at instant magnitude.
  ;;; As a GENERATOR it is a trap, and the module says so rather than hiding it:
  ;;; with `t` fresh, `between°` yields the window and then KEEPS GOING past
  ;;; `hi` forever, so asking for one answer more than the window holds
  ;;; diverges -- measured, nothing after 110 seconds on a ten-wide window.
  ;;; `span°` is the closing form of the same question.
  (define-relation (within° lo t hi) (between° lo t hi))

  ;;; The arithmetic progression base, base+step, base+2·step, ...
  ;;;
  ;;; GENERATOR ONLY, and that must be stated: with `x` fresh it yields in
  ;;; strictly increasing order and is the cheapest enumeration in the module,
  ;;; but with `x` ground and NOT on the stride it steps past `x` forever.  It
  ;;; is exported because an unbounded increasing stream is exactly what a
  ;;; weekday generator wants -- an infinite set has no last element -- and
  ;;; never as a test.  For a test, use `stride/≤°`.
  (define-relation (stride° base step x)
    (cond°
      ((=° x base))
      ((fresh° (next) (plus° base step next) (δ° (stride° next step x))))))

  ;;; The same progression with a ground CEILING, and the ceiling is the whole
  ;;; reason it terminates.  The bound must be carried explicitly: a version
  ;;; that instead drops `(<° base x)` into the recursive clause diverges as a
  ;;; GENERATOR, because with `x` fresh that comparison is itself an infinite
  ;;; stream which the conjunction then maps over.  A bounded enumeration needs
  ;;; a bound; there is no way to infer one.
  ;;;
  ;;; GENERATOR (`x` fresh, `hi` ground): the stream CLOSES -- asking for 25
  ;;; answers from a 20-answer window returns 20.
  ;;; TEST (`x` ground): pass `hi = x`; it terminates on a hit AND on a miss,
  ;;; which is the property `stride°` lacks.  Cost is strictly linear in the
  ;;; number of steps, and the ceiling is what you pay for termination -- all
  ;;; four figures measured on a 52-step enumeration:
  ;;;
  ;;;                          no ceiling    with ceiling
  ;;;     day-count, 15 bits     1.3 ms/step     21 ms/step
  ;;;     instant,   31 bits     4.7 ms/step     53 ms/step
  ;;;
  ;;; The ceiling costs an order of magnitude because the `<=°` it adds is
  ;;; itself a search over the numeral.  It is still the right trade: an
  ;;; enumeration that does not close is not an enumeration.
  ;;;
  ;;; ORDER: the `cond°` here is an `or°`, and `or°` INTERLEAVES, so the answers
  ;;; are only APPROXIMATELY increasing.  A sparse stride does come out sorted
  ;;; -- the four Mondays of October 2025 arrive in order -- but a dense one
  ;;; does not: `(span° 20370 20374 x)` was measured answering 20370, 20371,
  ;;; 20372, 20374, 20373.  It is the interleaving that fixes the order, not
  ;;; the calendar, and every expectation must be read off a run.
  (define-relation (stride/≤° base step hi x)
    (cond°
      ((=° x base) (<=° base hi))
      ((fresh° (next)
         (plus° base step next)
         (<=° next hi)
         (δ° (stride/≤° next step hi x))))))

  ;;; Every numeral in [lo,hi], as a stream that CLOSES.  This exists precisely
  ;;; because `within°` does not close, and a date library must not leave that
  ;;; trap lying around unnamed.  Same answers as `within°`, and it terminates;
  ;;; the order is `stride/≤°`'s interleaving, not a sort.
  (define-relation (span° lo hi x) (stride/≤° lo '(1) hi x))


  ;;; ==============================================================
  ;;; LAYER 2 -- THE SEAM.  project°-GUARDED.
  ;;;
  ;;; The single relation that crosses from instants to day-counts, and the
  ;;; ONLY place in this file where 86400 is written.  Everything calendrical
  ;;; lives above it and never sees a 31-bit number again.
  ;;; ==============================================================

  (define DAY (build-num 86400))

  ;;; `t = d·86400 + sod`, with `0 <= sod < 86400`.
  ;;;
  ;;; The PURE definition of this relation is the `else` branch:
  ;;;   (fresh° (p) (<° sod DAY) (*° d DAY p) (plus° p sod t))
  ;;; and it is sound, complete and productive -- with nothing ground it
  ;;; answers (0,0,0), (86400,1,0), (1,0,1), (172800,2,0) in a millisecond.
  ;;; The guard adds two fast paths over that definition and CHANGES NO ANSWER
  ;;; IN ANY MODE; it only changes the clock.
  ;;;
  ;;;   `t` ground -> (d,sod): 0-1 ms.  Relationally this is `/° t 86400`,
  ;;;      which at instant magnitude NEVER FINISHES.  Here the escape is not a
  ;;;      speed-up, it is the difference between working and not working.
  ;;;   (d,sod) ground -> `t`: 2 ms, against the SECONDS that the pure `*°`
  ;;;      branch costs once the day-count is real (a prototype measured 2.4 s
  ;;;      for one 15-bit-by-17-bit product).
  ;;;   neither ground: the pure branch, productive but only useful at small
  ;;;      magnitudes.
  ;;;   `d` ground and `sod` fresh: also the pure branch -- honest, and slow
  ;;;      enough to be useless (86400 answers, each paying a `*°`).  Ground
  ;;;      `sod` too, or use `midnight°`.
  (define-relation (day+second° t d sod)
    (project° ((t* t) (d* d) (s* sod))
      (let ((T (numeral->number t*))
            (DS (numerals->numbers (list d* s*))))
        (cond
          (T (and° (=° d (number->numeral (quotient T seconds/day)))
                   (=° sod (number->numeral (remainder T seconds/day)))))
          (DS (and° (<° sod DAY)
                    (=° t (number->numeral (+ (* (car DS) seconds/day) (cadr DS))))))
          (else (fresh° (p) (<° sod DAY) (*° d DAY p) (plus° p sod t)))))))

  ;;; `t` is the instant of midnight UTC on the day-count `d`.  This is the
  ;;; relation that lifts every day-count answer of layer 3 back to an instant,
  ;;; and it has NO bad mode: whichever side is ground, the seam takes a fast
  ;;; path, because `sod` is ground at 0 by construction.  PURE composition
  ;;; over the seam -- no second `project°`.
  (define-relation (midnight° t d) (day+second° t d '()))

  ;;; `t` is an instant falling on the day-count `d` -- the filter that lifts a
  ;;; day-level answer to arbitrary instants without ever dividing.
  ;;;
  ;;;   `t` ground: the seam's fast path, 0 ms, and it computes `d`.
  ;;;   `d` ground, `t` fresh: three comparisons around the day's midnight; it
  ;;;      enumerates the day's 86400 seconds in order (140 ms for the first
  ;;;      three), which is correct, bounded, and almost never what you want --
  ;;;      put a ground `t` in front instead.
  ;;;   neither: `numeral°` generates the day-count, then re-enters.
  (define-relation (on-day° d t)
    (project° ((t* t) (d* d))
      (let ((T (numeral->number t*))
            (D (numeral->number d*)))
        (cond
          (T (=° d (number->numeral (quotient T seconds/day))))
          (D (fresh° (m e)
               (midnight° m d)
               (not-after° m t)
               (plus° m DAY e)
               (before° t e)))
          (else (and° (numeral° d) (δ° (on-day° d t))))))))


  ;;; ==============================================================
  ;;; LAYER 3 -- THE CIVIL CALENDAR, OVER DAY-COUNTS.  project°-GUARDED.
  ;;;
  ;;; Everything here speaks DAY-COUNTS, not instants.  That is the load-bearing
  ;;; decision of the module: at 15 bits a pure generator fallback costs a
  ;;; millisecond, so "neither side ground" ENUMERATES REAL DATES instead of
  ;;; being undefined.  At 31 bits nothing cheap exists and the same fallback
  ;;; would have to walk 2^31 seconds.
  ;;; ==============================================================

  ;;; `day` is the day-count of the civil date (y,m,d), UTC, proleptic
  ;;; Gregorian.  All four arguments are numerals.
  ;;;
  ;;;   `day` ground -> (y,m,d): 1 ms.
  ;;;   (y,m,d) ground -> `day`: 1 ms, exactly one answer, and a NON-EXISTENT
  ;;;      DATE FAILS rather than silently normalising.  This is not decoration:
  ;;;      `days-from-civil` happily turns 2100-02-29 into 2100-03-01 and returns
  ;;;      a day-count for it.  The check exploits the fact that the calendar is
  ;;;      a bijection -- recompute the fields and fail unless they are the ones
  ;;;      given -- so 2100-02-29, 1900-02-29, 2025-13-01 and 2025-04-31 all
  ;;;      answer with ZERO solutions.  A negative day-count (anything before
  ;;;      1970) fails for the same reason it cannot be built.
  ;;;   neither ground: the `prime°` move -- put the module's own generator in
  ;;;      front and re-enter with the argument ground.  `numeral°` enumerates
  ;;;      day-counts 0,1,2,... in increasing order, so this mode enumerates the
  ;;;      calendar itself, and partially ground fields work by generate-and-test
  ;;;      (pin the year, get that year's days).
  (define-relation (civil° day y m d)
    (project° ((day* day) (y* y) (m* m) (d* d))
      (let ((Z (numeral->number day*))
            (YMD (numerals->numbers (list y* m* d*))))
        (cond
          (Z (let ((c (civil-from-days Z)))
               (and° (=° y (number->numeral (car c)))
                     (=° m (number->numeral (cadr c)))
                     (=° d (number->numeral (caddr c))))))
          (YMD (let ((z (apply days-from-civil YMD)))
                 (if (and (>= z 0) (equal? YMD (civil-from-days z)))
                     (=° day (number->numeral z))
                     ✗°)))
          (else (and° (numeral° day) (δ° (civil° day y m d))))))))

  ;;; `sod` is the second-of-day of the clock reading (hh,mm,ss).  Kept apart
  ;;; from `civil°` so that a caller who wants only the time of day never
  ;;; touches the calendar, and vice versa.
  ;;;
  ;;;   either side ground: 0-1 ms, and out-of-range fields FAIL -- 25:00:00 and
  ;;;      23:59:60 both give zero answers.  There are no leap seconds here.
  ;;;   neither: enumerates the 86400 seconds of a day in increasing order via
  ;;;      `stride/≤°`, deliberately NOT via `within°`, whose stream would not
  ;;;      close.
  (define-relation (clock° sod hh mm ss)
    (project° ((sod* sod) (h* hh) (m* mm) (s* ss))
      (let ((S (numeral->number sod*))
            (HMS (numerals->numbers (list h* m* s*))))
        (cond
          (S (and° (=° hh (number->numeral (quotient S seconds/hour)))
                   (=° mm (number->numeral (quotient (remainder S seconds/hour) seconds/minute)))
                   (=° ss (number->numeral (remainder S seconds/minute)))))
          (HMS (let ((v (+ (* seconds/hour (car HMS))
                           (* seconds/minute (cadr HMS))
                           (caddr HMS))))
                 (if (and (<= 0 (cadr HMS) 59) (<= 0 (caddr HMS) 59) (< v seconds/day))
                     (=° sod (number->numeral v))
                     ✗°)))
          (else (and° (stride/≤° '() '(1) (number->numeral (- seconds/day 1)) sod)
                      (δ° (clock° sod hh mm ss))))))))

  ;;; `w` is the weekday of the day-count `day`, as one of the seven SYMBOLS
  ;;; sunday ... saturday.  The two directions are answered by two DIFFERENT
  ;;; mechanisms, and the measurements say that is the right call.
  ;;;
  ;;;   `day` ground (TEST): 0 ms by escape.  The relational alternatives were
  ;;;      measured and rejected: `/°` on the day-count costs 2.9 s, striding
  ;;;      from the epoch costs 15.5 s, and `multiple°` as a divisibility test
  ;;;      gave NO ANSWER after three minutes on a negative case.
  ;;;   `w` ground, `day` fresh (GENERATOR): an unbounded `stride°` from the
  ;;;      first day-count carrying that weekday, ~1 ms per answer.  No ceiling
  ;;;      is needed because the set really is infinite; for a window use
  ;;;      `day-of-week/within°`.
  ;;;   neither: `numeral°` generates the day-count and the escape names its
  ;;;      weekday -- increasing order, every day, one answer each.
  (define-relation (day-of-week° day w)
    (project° ((day* day) (w* w))
      (let ((Z (numeral->number day*)))
        (cond
          (Z (=° w (weekday-of-day Z)))
          ((weekday-name? w*)
           (stride° (number->numeral (vector-ref weekday-epoch-day (weekday-index w*)))
                    (number->numeral days/week)
                    day))
          (else (and° (numeral° day) (δ° (day-of-week° day w))))))))

  ;;; The seven weekday symbols, as a stream.  Pure, one unification per answer,
  ;;; and it exists for exactly one reason: it is the generator that grounds `w`
  ;;; in front of a relation that needs a ground weekday.  That is the `prime°`
  ;;; discipline again -- a guarded relation whose open mode would be undefined
  ;;; gets a generator put in front of it rather than an error.
  (define-relation (weekday-name° w)
    (cond°
      ((=° w 'sunday)) ((=° w 'monday)) ((=° w 'tuesday)) ((=° w 'wednesday))
      ((=° w 'thursday)) ((=° w 'friday)) ((=° w 'saturday))))

  ;;; Every day-count in the closed window [lo,hi] whose weekday is `w` -- the
  ;;; query callers actually write, and the headline feature of the module.
  ;;;
  ;;; It is a relation of its own rather than the obvious composition because
  ;;; of two measurements.  Composing `within°` with the weekday test does not
  ;;; terminate on a year-wide window; striding from the 1970 anchor to a 2025
  ;;; window costs fifteen seconds.  Anchoring at `lo` costs ONE escape --
  ;;; O(1), on a ground integer -- after which every single step is pure
  ;;; layer-1 arithmetic and the cost is the WIDTH OF THE WINDOW, not the
  ;;; distance from the epoch: measured 66 ms for the 4 Mondays of a 28-day
  ;;; window, 226 ms for the 13 of a 91-day one and 938 ms for the 52 of a
  ;;; year.  A year of Mondays is therefore about a second, which is the number
  ;;; to keep in mind when sizing a test.
  ;;;
  ;;;   lo,hi,w ground, `day` fresh: increasing order, the stream CLOSES.
  ;;;   `day` ground as well: a terminating test, on a hit and on a miss.
  ;;;   `lo` fresh: falls back to walking from the epoch anchor, which is sound
  ;;;      and slow; ground your window.
  ;;;   `w` fresh: `weekday-name°` grounds it and the relation re-enters, so the
  ;;;      window is enumerated once per weekday and the seven streams interleave.
  (define-relation (day-of-week/within° lo hi w day)
    (project° ((lo* lo) (w* w))
      (let ((L (numeral->number lo*)))
        (cond
          ((and L (weekday-name? w*))
           (stride/≤° (number->numeral
                        (+ L (modulo (- (weekday-index w*) (weekday-index (weekday-of-day L)))
                                     days/week)))
                      (number->numeral days/week)
                      hi
                      day))
          ((weekday-name? w*)
           (and° (not-before° day lo)
                 (stride/≤° (number->numeral (vector-ref weekday-epoch-day (weekday-index w*)))
                            (number->numeral days/week)
                            hi
                            day)))
          ;;; `w` is what the two fast branches need, so `w` is what the
          ;;; fallback must generate.  Generating `day` instead -- the obvious
          ;;; move, since `day` is the answer -- LOOPS: the re-entry still finds
          ;;; `w` fresh and falls back again.  Measured, as a hang.
          (else (and° (weekday-name° w) (δ° (day-of-week/within° lo hi w day))))))))

  ;;; The seven names the user asked for.  THEY TAKE A DAY-COUNT, not an
  ;;; instant -- that is what layer 3 speaks, and `midnight°` or
  ;;; `weekday-midnight°` lifts an answer back to an instant.  Thin sugar over
  ;;; `day-of-week°`, so they inherit both of its directions for free: with `d`
  ;;; ground a 0 ms test, with `d` fresh an increasing infinite generator.
  (define-relation (sunday° d) (day-of-week° d 'sunday))
  (define-relation (monday° d) (day-of-week° d 'monday))
  (define-relation (tuesday° d) (day-of-week° d 'tuesday))
  (define-relation (wednesday° d) (day-of-week° d 'wednesday))
  (define-relation (thursday° d) (day-of-week° d 'thursday))
  (define-relation (friday° d) (day-of-week° d 'friday))
  (define-relation (saturday° d) (day-of-week° d 'saturday))

  ;;; The two filters.  Note the CONJUNCTION ORDER: the disjunction that pins
  ;;; `w` comes FIRST, so that with `d` fresh `day-of-week°` still sees a ground
  ;;; weekday and takes its stride branch rather than its generate-and-test
  ;;; branch.  `workday°` spells out its five rows instead of negating the
  ;;; weekend with `≠°`, because `≠°` constrains `w` without grounding it and
  ;;; would throw the generator direction away.
  (define-relation (weekend° d)
    (fresh° (w)
      (or° (=° w 'saturday) (=° w 'sunday))
      (day-of-week° d w)))

  (define-relation (workday° d)
    (fresh° (w)
      (or° (=° w 'monday) (=° w 'tuesday) (=° w 'wednesday) (=° w 'thursday) (=° w 'friday))
      (day-of-week° d w)))

  ;;; `y` is a leap year.  No arithmetic of its own: a year is leap exactly
  ;;; when the 29th of February is a date, and `civil°` already decides that.
  ;;; With `y` ground it is a 1 ms test; with `y` fresh it enumerates leap years
  ;;; by way of `civil°`'s own generator, which is slow but total.
  (define-relation (leap-year° y)
    (fresh° (d) (civil° d y '(0 1) '(1 0 1 1 1))))

  ;;; Windows, as relations over day-counts, so that a caller writes the window
  ;;; they mean instead of computing one.  `(month° y m lo hi)` gives the first
  ;;; and last day-count of that month, `(year° y lo hi)` of that year; both
  ;;; feed straight into `day-of-week/within°` and `span°`.  PURE composition
  ;;; over `civil°` plus a twelve-row successor table, in the register of
  ;;; `full-adder°`: no division, no month-length table, and February's length
  ;;; falls out of the calendar rather than being written down.
  (define-relation (next-month° y m y* m*)
    (cond°
      ((=° m '(1))      (=° m* '(0 1))    (=° y* y))
      ((=° m '(0 1))    (=° m* '(1 1))    (=° y* y))
      ((=° m '(1 1))    (=° m* '(0 0 1))  (=° y* y))
      ((=° m '(0 0 1))  (=° m* '(1 0 1))  (=° y* y))
      ((=° m '(1 0 1))  (=° m* '(0 1 1))  (=° y* y))
      ((=° m '(0 1 1))  (=° m* '(1 1 1))  (=° y* y))
      ((=° m '(1 1 1))  (=° m* '(0 0 0 1)) (=° y* y))
      ((=° m '(0 0 0 1))(=° m* '(1 0 0 1)) (=° y* y))
      ((=° m '(1 0 0 1))(=° m* '(0 1 0 1)) (=° y* y))
      ((=° m '(0 1 0 1))(=° m* '(1 1 0 1)) (=° y* y))
      ((=° m '(1 1 0 1))(=° m* '(0 0 1 1)) (=° y* y))
      ((=° m '(0 0 1 1))(=° m* '(1))       (plus° y '(1) y*))))

  (define-relation (month° y m lo hi)
    (fresh° (y* m* next)
      (civil° lo y m '(1))
      (next-month° y m y* m*)
      (civil° next y* m* '(1))
      (plus° hi '(1) next)))

  (define-relation (year° y lo hi)
    (fresh° (y*)
      (civil° lo y '(1) '(1))
      (plus° y '(1) y*)
      (fresh° (next) (civil° next y* '(1) '(1)) (plus° hi '(1) next))))


  ;;; ==============================================================
  ;;; LAYER 4 -- COMPOSITION.  No new arithmetic anywhere below; these
  ;;; relations contain nothing but a conjunction and, where a mode has to be
  ;;; chosen, the ORDER of that conjunction.
  ;;; ==============================================================

  ;;; The composite the user asks for: instant <-> (year, month, day, hour,
  ;;; minute, second), UTC, every field a numeral.
  ;;;
  ;;; Its whole content is the conjunction order, and it chooses that by mode:
  ;;; with `t` ground, SPLIT FIRST so that both decoders see a ground input;
  ;;; with the fields ground, DECODE FIRST so that the seam sees a ground
  ;;; (day,sod) and never multiplies two logic variables.
  ;;;
  ;;;   `t` ground: 1 ms, and agrees with `(chicken time posix)
  ;;;      seconds->utc-time` wherever that oracle is defined.
  ;;;   fields ground: 0-2 ms, exactly ONE answer.
  ;;;   neither: the second ordering makes both sub-relations generate and the
  ;;;      seam only ever multiplies ground numbers, so it ENUMERATES REAL
  ;;;      INSTANTS.  Answer order is the µKanren interleaving: day and
  ;;;      second-of-day interleave, so it is not chronological.
  (define-relation (date-time° t y mo d hh mm ss)
    (project° ((t* t))
      (if (numeral? t*)
          (fresh° (day sod) (day+second° t day sod) (civil° day y mo d) (clock° sod hh mm ss))
          (fresh° (day sod) (civil° day y mo d) (clock° sod hh mm ss) (day+second° t day sod)))))

  ;;; The weekday of an INSTANT, as opposed to of a day-count.  PURE
  ;;; composition: the seam plus `day-of-week°`.  With `t` ground it is the O(1)
  ;;; weekday test and the right tool for "is this instant a Monday".  With `t`
  ;;; fresh it inherits the seam's slow mode -- for "give me the Mondays", use
  ;;; `weekday-midnight°`.
  (define-relation (weekday° t w)
    (fresh° (day sod) (day+second° t day sod) (day-of-week° day w)))

  ;;; The headline query, at instant magnitude: `t` ranges over the midnights
  ;;; UTC of the `w`-days inside the instant window [lo,hi].  Generator and
  ;;; test in one relation.
  ;;;
  ;;; The bounds cross into day-space by two O(1) escapes on ground instants;
  ;;; the enumeration itself is a pure stride at 15 BITS, measured at 21 ms a
  ;;; step against the 53 ms a step it costs to stride by 604800 over 31-bit
  ;;; instants; and each answer crosses back by a third O(1) escape, because
  ;;; `midnight°` has a ground day-count to work from.  `lo` and `hi` must be
  ;;; ground.  Measured end to end: the four Mondays of October 2025, as ISO
  ;;; strings, in 78 ms; the same four at 09:00 in 95 ms; as a test, 143 ms for
  ;;; a hit and a miss together, and the MISS TERMINATES, because the window
  ;;; bounds the stride.
  (define-relation (weekday-midnight° w lo hi t)
    (fresh° (dlo dhi slo shi day)
      (day+second° lo dlo slo)
      (day+second° hi dhi shi)
      (day-of-week/within° dlo dhi w day)
      (midnight° t day)))

  ;;; instant <-> "YYYY-MM-DDTHH:MM:SSZ", UTC.
  ;;;
  ;;; Formatting and parsing are Scheme string work, so there is no pretence of
  ;;; purity in the two fast paths -- but the OPEN mode is still defined, by
  ;;; composing with layer 4 and re-entering.
  ;;;
  ;;;   `t` ground -> the string, 0-1 ms.
  ;;;   the string ground -> `t`, 0-1 ms, and the parser VALIDATES:
  ;;;      "2025-02-30T00:00:00Z", a pre-epoch instant and plain garbage all
  ;;;      give ZERO answers rather than a wrong instant.
  ;;;   neither: `date-time°` grounds `t`, then the goal re-enters and formats.
  (define-relation (iso° t str)
    (project° ((t* t) (s* str))
      (let ((T (numeral->number t*)))
        (cond
          (T (=° str (instant->iso T)))
          ((string? s*) (let ((v (iso->instant s*)))
                          (if v (=° t (number->numeral v)) ✗°)))
          (else (fresh° (y mo d hh mm ss)
                  (date-time° t y mo d hh mm ss)
                  (δ° (iso° t str))))))))
)
