; The suite for (aux kanren date): relations over instants in time, written in
; the idiom of (aux kanren arith) and built on that module's numeral and on
; nothing else.
;
; The relations themselves live in ../aux.kanren.date.scm; this file only
; exercises them.  Every expectation below was read off a real run -- including
; the ones that assert an EMPTY answer stream, which are the only assertions
; that can tell a refusal apart from a hang, and only by being run.

(import scheme (chicken base) srfi-1 (chicken time posix)
  (aux base) (aux unittest) (aux kanren micro) (aux kanren arith) (aux kanren date))

(define-suite microkanren-date-suite

  ((doc r)
   `((structure/section "An instant is a numeral, and nothing else")
     (p "A date, in " (code/inline "(aux kanren date)") ", is a POSIX time -- the count of "
        "seconds since 1970-01-01T00:00:00Z -- held as the little-endian bit list that "
        (code/inline "build-num") " produces in " (code/inline "(aux kanren arith)")
        ". Not a record, not a tagged vector, not a new datatype: the SAME canonical numeral, "
        "so that every relation of that module already applies to an instant and this one "
        "contributes names and a calendar rather than a representation. "
        (code/inline "'()") " is 1970-01-01T00:00:00Z, and it is a FLOOR and not an origin -- "
        "the numeral is unsigned, so there is nothing underneath it and no instant before the "
        "epoch can be named at all. That single decision is what the whole module is arranged "
        "around, and the cases below are arranged around its consequences.")
     (p "The consequences were MEASURED, at the magnitudes a date query actually uses, and they "
        "are not symmetric. A current instant such as " (code/inline "1760000000")
        " is a 31-bit numeral; 86400 is 17 bits; a day-count since the epoch is 15. At 31 bits "
        (code/inline "<°") ", " (code/inline "<=°") ", " (code/inline "plus°") " and "
        (code/inline "minus°") " cost a handful of milliseconds and run in every direction, "
        "while " (code/inline "*°") " costs seconds and " (code/inline "/°")
        " of an instant by 86400 NEVER FINISHED -- it was killed after TEN MINUTES. That is not "
        "a performance note. It is the statement that the calendar cannot be reached by "
        "relational division, so a design that tries is not slow, it is broken.")
     (structure/section "Layer 1 is pure; everything calendrical is guarded")
     (p "So the module is two halves with an explicit boundary, and the cases below are grouped "
        "to make that boundary visible. Ordering, durations, shifts and bounded enumeration are "
        "PURE relations -- " (code/inline "before°") ", " (code/inline "after°") ", "
        (code/inline "same-instant°") ", " (code/inline "elapsed°") ", " (code/inline "shift°")
        ", " (code/inline "within°") ", " (code/inline "stride°") ", "
        (code/inline "stride/≤°") ", " (code/inline "span°")
        " -- with no " (code/inline "project°") " anywhere between the caller and the answer, no "
        "groundness precondition, and every mode available. " (code/inline "elapsed°")
        " and " (code/inline "shift°") " are one goal, " (code/inline "(plus° t d u)")
        ", read from different sides; running it backwards is how subtraction is spelled, "
        "because the module contains no subtraction of its own.")
     (p "Calendar conversion is the other half, and every relation in it is "
        (code/inline "project°") "-GUARDED: it reads the substitution when the goal runs, and "
        "where the relevant side is ground it escapes to Scheme -- Howard Hinnant's "
        (code/inline "days-from-civil") " / " (code/inline "civil-from-days")
        ", integer-only and table-free -- and unifies the result. This is the move "
        (code/inline "prime°") " already makes in " (code/inline "(aux kanren arith)")
        ", used for a stronger reason: there, the guard is an optimisation over a relation that "
        "would merely be slow; here, in the ground-instant direction, it is the difference "
        "between the relation existing and not existing.")
     (code/lang "scheme" ";; LAYER 1 -- pure.  No project°.  Every mode, a few milliseconds.\n(define-relation (before° t u) (<° t u))\n(define-relation (shift° t d u) (plus° t d u))\n\n;; LAYER 2 -- the seam, project°-guarded.  t = d*86400 + sod.\n;; The `else' branch IS the pure definition, and it is sound, complete and\n;; productive -- at small magnitudes.  The guard adds two fast paths over it\n;; and changes NO answer in any mode; it only changes the clock.\n(define-relation (day+second° t d sod)\n  (project° ((t* t) (d* d) (s* sod))\n    (let ((T (numeral->number t*)) (DS (numerals->numbers (list d* s*))))\n      (cond\n        (T   ...)   ; escape: /° of an instant by 86400 does not terminate\n        (DS  ...)   ; escape: *° at this magnitude costs seconds\n        (else (fresh° (p) (<° sod DAY) (*° d DAY p) (plus° p sod t)))))))")
     (p "What the guard costs the caller is a MODE OBLIGATION, and it is stated in the cases "
        "rather than assumed: because the branch is chosen by looking at instantiation at the "
        "moment the goal runs, conjunction ORDER is now part of the meaning. A goal that grounds "
        (code/inline "t") " must be written before " (code/inline "iso°") ", not after it. "
        "Groundness is decided by " (code/inline "numeral->number") ", positive typing over the "
        "cells rather than " (code/inline "(not (var? x))") ", so a half-built numeral like "
        (code/inline "(1 0 . β)") " is correctly NOT taken for ground and falls to the open "
        "branch instead of being decoded as though its tail were empty.")
     (structure/section "The seam, and why weekdays stride")
     (p "One relation crosses between the two magnitudes, " (code/inline "day+second°") ", and "
        "it is the only place in the module where 86400 is written. Everything calendrical sits "
        "above it and never sees a 31-bit number again: " (code/inline "civil°") ", "
        (code/inline "day-of-week°") ", the seven weekday names and the window relations all "
        "speak DAY-COUNTS. That is the load-bearing decision. At 15 bits the open fallback of a "
        "guarded relation costs a millisecond an answer, so \"neither side ground\" ENUMERATES "
        "REAL DATES instead of being undefined; at 31 bits the same fallback would have to walk "
        "two billion seconds.")
     (p "Weekday enumeration is the headline feature and it is a STRIDE precisely so that it "
        "never needs the modulo that would need that division. A weekday is not computed by "
        "dividing a day-count by seven -- " (code/inline "/°") " on a day-count was measured at "
        "2.9 s, and " (code/inline "multiple°") " as a divisibility test gave no answer at all "
        "after three minutes on a negative case. Instead each weekday has an ANCHOR, the "
        "smallest day-count carrying it, and the generator is repeated addition from there:")
     (code/lang "scheme" ";; 1970-01-01 is day 0 and a THURSDAY -- verified below against\n;; (chicken time posix), which is the one fact no relation can witness for itself.\n;; So the first Monday of the epoch is day 4 = 1970-01-05 = unixtime 345600.\n(define weekday-epoch-day '#(3 4 5 6 0 1 2))   ; sunday .. saturday\n\n;; GENERATOR ONLY: with x fresh, strictly increasing, ~1 ms an answer.\n;; With x ground and OFF the stride it steps past x forever -- measured.\n(define-relation (stride° base step x)\n  (cond°\n    ((=° x base))\n    ((fresh° (next) (plus° base step next) (δ° (stride° next step x))))))\n\n;; The same progression with a ground CEILING, and the ceiling is the whole\n;; reason it terminates on a MISS.  That is what a test needs.\n(define-relation (stride/≤° base step hi x)\n  (cond°\n    ((=° x base) (<=° base hi))\n    ((fresh° (next) (plus° base step next) (<=° next hi)\n             (δ° (stride/≤° next step hi x))))))")
     (p "Both relations are pure layer 1, and the cases pin the difference between them, because "
        "it is the difference between an enumeration and a decision procedure. "
        (code/inline "day-of-week°") " takes the other way out for its test direction: with the "
        "day-count ground it never strides at all, it escapes, which is why "
        (code/inline "(monday° 20370)") " and the first four Mondays cost the same nothing.")
     (structure/section "Reading the expected values")
     (p "Every expectation was read off a real run, and several of them are written in an order "
        "that is not the calendar's. " (code/inline "cond°") " is " (code/inline "or°")
        " and " (code/inline "or°") " INTERLEAVES, so a dense enumeration comes out permuted: "
        "a four-second " (code/inline "span°") " answers 400, 401, 403, 402, and "
        (code/inline "stride/≤°") " answers 4, 11, 25, 18 where " (code/inline "stride°")
        " answers 4, 11, 18, 25. Sorting those expectations would hide from a reader the one "
        "thing they must know before building a calendar on top of the module, which is that "
        "chronology is their job and not the solver's.")
     (p "Queries ask for more answers than exist wherever the stream is meant to close, so the "
        "assertion pins EXHAUSTION and not merely correctness -- a four-second window asked for "
        "nine answers hands back four. And an expectation of " (code/inline "'()")
        " is never decoration. A relation that cannot decide a negative case does not fail, it "
        "diverges, and a diverging case prints nothing at all, because the suite reports only "
        "once every case has finished. So each " (code/inline "'()")
        " below is the assertion that a miss TERMINATES: that "
        (code/inline "(monday° thursday-day-count)") " refuses instead of striding past it "
        "forever, that an inverted window is empty instead of silently swapping its bounds, and "
        "that a date which does not exist has zero solutions instead of a neighbour's.")
     (p "A handful of modes are therefore deliberately absent, each named in the case that would "
        "have carried it: " (code/inline "within°") " with a fresh middle, "
        (code/inline "stride°") " as a test, a comparison with its LATER side fresh, and "
        (code/inline "leap-year°") " on 1900. They diverge, or they answer an infinite family, "
        "or -- for 1900 -- they answer correctly for the wrong reason, the epoch floor rather "
        "than the century rule; and a suite cannot assert a hang. Where a mode is dropped the "
        "prose says what it does instead, and the closing form that answers the same question is "
        "tested in its place.")))

  ; -- instants: LAYER 1, pure, no project° between the caller and the answer -------------
  ;
  ; Dropped from this area, deliberately, and every one of them for the same reason -- the
  ; mode diverges and a suite cannot assert a hang:
  ;   * `within°` with a fresh middle: `between°` yields the window and then keeps going
  ;     past `hi` forever.  `span°` below is the closing form of that question and is
  ;     tested in its place.
  ;   * `stride°` as a TEST against a ground `x` off the stride: it steps past `x` forever.
  ;     `stride/≤°` below carries the ceiling and IS tested as a test.
  ;   * comparison with the LATER side fresh: `(after° u 3)` answers the single partially
  ;     instantiated shape `(α β γ . δ)` -- an infinite family, not an instant.  That is
  ;     `<°`'s behaviour, asserted in microkanren-arith.scm; this file asserts only the mode
  ;     a date query can use, the earlier side fresh.
  ;   * windows wider than a few seconds: `span°` costs ~50 ms a second at instant
  ;     magnitude, so the widest one below is four seconds.

  ((test/before°+after° _)
   (⊦= `(,(build-num 1760000400))
       (μkanren-run (t 2 #t) (same-instant° t (build-instant 2025 10 9 9 0 0))))
   (⊦= '(yes) (μkanren-run (q 2 #t) (=° q 'yes)
                (before° (build-instant 2025 10 9 9 0 0) (build-instant 2025 10 9 17 30 0))))
   (⊦= '()    (μkanren-run (q 2 #t) (=° q 'yes)
                (before° (build-instant 2025 10 9 17 30 0) (build-instant 2025 10 9 9 0 0))))
   (⊦= '(yes) (μkanren-run (q 2 #t) (=° q 'yes)
                (after° (build-instant 2025 10 9 17 30 0) (build-instant 2025 10 9 9 0 0))))
   (⊦= '()    (μkanren-run (q 2 #t) (=° q 'yes)
                (before° (build-instant 2025 10 9 9 0 0) (build-instant 2025 10 9 9 0 0))))
   (⊦= '(yes) (μkanren-run (q 2 #t) (=° q 'yes)
                (not-after°  (build-instant 2025 10 9 9 0 0) (build-instant 2025 10 9 9 0 0))
                (not-before° (build-instant 2025 10 9 9 0 0) (build-instant 2025 10 9 9 0 0))
                (same-instant° (build-instant 2025 10 9 9 0 0) (build-instant 2025 10 9 9 0 0))))
   (⊦= '()    (μkanren-run (q 2 #t) (=° q 'yes)
                (same-instant° (build-instant 2025 10 9 9 0 0) (build-instant 2025 10 9 17 30 0))))
   (⊦= '(yes) (μkanren-run (q 2 #t) (=° q 'yes) (before° '() (build-instant 2025 10 9 9 0 0))))
   (⊦= '()    (μkanren-run (q 2 #t) (=° q 'yes) (before° (build-instant 2025 10 9 9 0 0) '())))
   (⊦= `(,(build-num 3) ,(build-num 0) ,(build-num 1) ,(build-num 2))
       (μkanren-run (t 9 #t) (not-after° t (build-num 3))))
   `(doc (p "None of the five comparisons has a body: "
            (code/inline "before°") " is " (code/inline "<°") ", "
            (code/inline "after°") " is " (code/inline "<°") " with its arguments crossed, "
            (code/inline "same-instant°") " is " (code/inline "=°")
            ". So what these lines pin is not arithmetic -- " (code/inline "(aux kanren arith)")
            " already owns that -- but the CROSSING and the strictness, which nothing else in "
            "the module would notice if they were wrong: layers 3 and 4 never compare instants. "
            "Swap the two arguments of " (code/inline "after°") " and only the fourth line here "
            "fails. The first line is the anchor the rest of the file leans on: it reads "
            (code/inline "build-instant") " back as a plain unixtime, so every later "
            (code/inline "(build-instant 2025 10 9 ...)") " may be written for a human and still "
            "be a claim about " (code/inline "1760000400") ".")
         (p "The reflexive pair is the case worth keeping. "
            (code/inline "(before° t t)") " must FAIL and "
            (code/inline "(not-after° t t)") " must SUCCEED, and they differ by one clause: "
            (code/inline "<=°") " is " (code/inline "(or° (=° n m) (<° n m))") ". Drop that "
            (code/inline "=°") " clause and " (code/inline "not-after°") " silently becomes "
            (code/inline "before°") " -- no error, no wrong number, just every closed window in "
            "the module quietly losing its endpoints. The two boundary lines of "
            (code/inline "test/within°+span°") " and the base clause of "
            (code/inline "stride/≤°") " are exactly the things that would then break, three "
            "layers away from the edit.")
         (p "The epoch pair says that the empty numeral is 1970-01-01T00:00:00Z and that it is a "
            "FLOOR, not an origin: " (code/inline "(before° '() t)") " holds for every instant "
            "this module can name and " (code/inline "(before° t '())") " holds for none. The "
            "representation is unsigned; there is nothing underneath.")
         (p "The last line reads a comparison BACKWARDS, which is the whole reason these are "
            "relations and not predicates: with the earlier side fresh and the later side ground, "
            (code/inline "(not-after° t 3)") " enumerates 3, 0, 1, 2 and closes. Ground answers, "
            "in " (code/inline "cond°") "'s interleaving -- the " (code/inline "=°") " clause of "
            (code/inline "<=°") " comes first, so the ceiling arrives before the values below it. "
            "The numeral is tiny on purpose: the identical query at instant magnitude has 1.76 "
            "billion answers, which is why the module tells callers to bound a fresh side with "
            (code/inline "span°") " rather than with a comparison. Nothing in this clause is "
            (code/inline "project°") "-guarded -- layer 1 contains no " (code/inline "project°")
            " at all -- so the caller pays no groundness precondition and gets every mode; the "
            "15 ms a ground comparison costs at 31 bits is the whole bill.")))

  ((test/elapsed° _)
   (⊦= `(,(build-num 30600))
       (μkanren-run (d 2 #t) (elapsed° (build-instant 2025 10 9 9 0 0) d (build-instant 2025 10 9 17 30 0))))
   (⊦= `(,(build-num 1760031000))
       (μkanren-run (u 2 #t) (elapsed° (build-instant 2025 10 9 9 0 0) (seconds 30600) u)))
   (⊦= `(,(build-num 1760000400))
       (μkanren-run (t 2 #t) (elapsed° t (seconds 30600) (build-instant 2025 10 9 17 30 0))))
   (⊦= '(())
       (μkanren-run (d 2 #t) (elapsed° (build-instant 2025 10 9 9 0 0) d (build-instant 2025 10 9 9 0 0))))
   (⊦= '()
       (μkanren-run (d 2 #t) (elapsed° (build-instant 2025 10 9 17 30 0) d (build-instant 2025 10 9 9 0 0))))
   (⊦= `(,(build-num 1760000400))
       (μkanren-run (d 2 #t) (elapsed° '() d (build-instant 2025 10 9 9 0 0))))
   (⊦= '()
       (μkanren-run (d 2 #t) (elapsed° (build-instant 2025 10 9 9 0 0) d '())))
   (⊦= `((,(build-num 0) ,(build-num 60)) (,(build-num 1) ,(build-num 61))
         (,(build-num 2) ,(build-num 62)) (,(build-num 3) ,(build-num 63)))
       (μkanren-run (q 4 #t) (fresh° (t u) (=° q (list t u)) (elapsed° t (build-num 60) u))))
   `(doc (p (code/inline "elapsed°") " is " (code/inline "plus°") " with the names changed, and "
            "the first three lines are one goal read three ways: the duration between two "
            "instants (12 ms), the later instant from the earlier plus a duration (3 ms), the "
            "earlier instant from the later (7 ms). A duration relation that only ran forwards "
            "would need a subtraction, and there is none in this module -- the backward reads "
            "are not a bonus, they are how subtraction is spelled.")
         (p "Zero is an answer, not a failure. "
            (code/inline "(elapsed° t d t)") " returns the one-answer stream "
            (code/inline "(())") " -- a stream containing the empty numeral -- and the distinction "
            "between that and " (code/inline "()") " is the entire case. Put a "
            (code/inline "pos°") " on the duration, as a reader tempted to forbid "
            "\"no time passing\" might, and this line turns into an empty stream while every "
            "other line in the clause keeps passing.")
         (p "The negative case is the encoding speaking, and it is the one behaviour a caller from "
            "any other date library will guess wrong. "
            (code/inline "(elapsed° later d earlier)") " does not answer -604800, and does not "
            "raise: it is an EMPTY STREAM, because " (code/inline "plus°") " cannot prove "
            "later + d = earlier for any numeral d. Same for "
            (code/inline "(elapsed° t d '())") ". A caller who wants a distance must order the pair "
            "first with the previous clause; this relation will not tell them which way round they "
            "were. Note that a failing query is still a 13 ms search, not a type check -- the goal "
            "is run, it just proves nothing.")
         (p "Measured from the epoch, the duration IS the instant: "
            (code/inline "(elapsed° '() d t)") " answers " (code/inline "t") " itself. That line is "
            "the representation stated as a theorem, and it is what licenses every other relation "
            "in layer 1 to treat instants and durations as the same kind of numeral. The last line "
            "leaves BOTH instants fresh with only the duration ground: productive, 2 ms, "
            "(0,60) (1,61) (2,62) (3,63) -- it neither hangs nor errors, because "
            (code/inline "plus°") " has no modes. What it has is magnitudes, and that stream is "
            "useful only where the numerals are small.")))

  ((test/shift° _)
   (⊦= `(,(build-num 1760029200))
       (μkanren-run (u 2 #t) (shift° (build-instant 2025 10 9 9 0 0) (hours 8) u)))
   (⊦= `(,(build-num 1760002200))
       (μkanren-run (u 2 #t) (shift° (build-instant 2025 10 9 9 0 0) (minutes 30) u)))
   (⊦= `(,(build-num 1760605200))
       (μkanren-run (u 2 #t) (shift° (build-instant 2025 10 9 9 0 0) (weeks 1) u)))
   (⊦= `(,(build-num 1759944600))
       (μkanren-run (t 2 #t) (shift° t (days 1) (build-instant 2025 10 9 17 30 0))))
   (⊦= `(,(build-num 1760000400))
       (μkanren-run (u 2 #t) (shift° (build-instant 2025 10 9 9 0 0) (seconds 0) u)))
   (⊦= '()
       (μkanren-run (q 2 #t) (=° q 'yes)
         (shift° (build-instant 2025 10 9 9 0 0) (days 1) (build-instant 2025 10 9 17 30 0))))
   (⊦= (list (build-num 28800) (build-num 1800) (build-num 86400) (build-num 604800) (build-num 0))
       (list (hours 8) (minutes 30) (days 1) (weeks 1) (seconds 0)))
   (⊦= `(,(build-num 30600))
       (μkanren-run (d 2 #t) (shift° (build-instant 2025 10 9 9 0 0) d (build-instant 2025 10 9 17 30 0))))
   `(doc (p (code/inline "shift°") " and " (code/inline "elapsed°") " are the SAME relation under "
            "two names -- both are " (code/inline "(plus° t d u)") " -- and the last line proves "
            "it by running a " (code/inline "shift°") " query in " (code/inline "elapsed°")
            "'s mode and getting the same 30600 the previous clause got. The two names exist for "
            "the call site, not for the solver; if one of them ever grows a body of its own, that "
            "line is what notices.")
         (p "The duration builders are LAYER 0 -- plain Scheme, evaluated while the goal tree is "
            "being built -- and the seventh line asserts their numerals directly because that is a "
            "unit conversion nothing else would catch: "
            (code/inline "(hours 8)") " is 28800 and " (code/inline "(weeks 1)") " is 604800, and a "
            (code/inline "seconds/hour") " that drifted would simply move every appointment in "
            "silence. They are Scheme and not relations on purpose: "
            (code/inline "(*° d (build-num 3600))") " at instant magnitude costs SECONDS, so "
            "multiplying by a constant must never reach the relational layer. Multiplying in "
            "CHICKEN before the goal exists costs nothing.")
         (p "The backward shift is subtraction without a subtraction: "
            (code/inline "(shift° t (days 1) u)") " with " (code/inline "u") " ground recovers the "
            "instant a day earlier in 7 ms. The same goal with all three ground is a TEST and "
            "decides in 0 ms -- the false line is there so that the mode is asserted rather than "
            "assumed, because a relation that answered " (code/inline "(yes)") " to a wrong shift "
            "would be a filter that passes everything. "
            (code/inline "(seconds 0)") " shifts nothing, which is the same "
            (code/inline "()") "-is-a-numeral fact as the zero duration above, read from the other "
            "side. Everything here is pure; there is no " (code/inline "project°") " between the "
            "caller and the answer, so no line in this clause has a groundness precondition to "
            "violate.")))

  ((test/within°+span° _)
   (⊦= '(yes) (μkanren-run (q 2 #t) (=° q 'yes)
                (within° (build-instant 2025 10 9 9 0 0) (build-instant 2025 10 9 12 0 0)
                         (build-instant 2025 10 9 17 30 0))))
   (⊦= '(yes) (μkanren-run (q 2 #t) (=° q 'yes)
                (within° (build-instant 2025 10 9 9 0 0) (build-instant 2025 10 9 9 0 0)
                         (build-instant 2025 10 9 17 30 0))))
   (⊦= '(yes) (μkanren-run (q 2 #t) (=° q 'yes)
                (within° (build-instant 2025 10 9 9 0 0) (build-instant 2025 10 9 17 30 0)
                         (build-instant 2025 10 9 17 30 0))))
   (⊦= '()    (μkanren-run (q 2 #t) (=° q 'yes)
                (within° (build-instant 2025 10 9 9 0 0) (build-instant 2025 10 10 12 0 0)
                         (build-instant 2025 10 9 17 30 0))))
   (⊦= '()    (μkanren-run (q 2 #t) (=° q 'yes)
                (within° (build-instant 2025 10 9 17 30 0) (build-instant 2025 10 9 12 0 0)
                         (build-instant 2025 10 9 9 0 0))))
   (⊦= '(yes) (μkanren-run (q 2 #t) (=° q 'yes)
                (within° (build-instant 2025 10 9 9 0 0) (build-instant 2025 10 9 9 0 0)
                         (build-instant 2025 10 9 9 0 0))))
   (⊦= `(,(build-num 1760000400) ,(build-num 1760000401) ,(build-num 1760000403) ,(build-num 1760000402))
       (μkanren-run (x 9 #t) (span° (build-num 1760000400) (build-num 1760000403) x)))
   (⊦= `(,(build-num 1760000400))
       (μkanren-run (x 4 #t) (span° (build-instant 2025 10 9 9 0 0) (build-instant 2025 10 9 9 0 0) x)))
   (⊦= '()
       (μkanren-run (x 4 #t) (span° (build-instant 2025 10 9 17 30 0) (build-instant 2025 10 9 9 0 0) x)))
   (⊦= `(,(build-num 1760000400) ,(build-num 1760000401))
       (μkanren-run (x 9 #t) (span° (build-num 1760000400) (build-num 1760000404) x)
                             (before° x (build-num 1760000402))))
   `(doc (p (code/inline "within°") " is " (code/inline "between°") ", used here only as a TEST "
            "-- all three arguments ground, about 30 ms at instant magnitude. The two boundary "
            "lines are the reason the case exists: the window is CLOSED, so an instant equal to "
            (code/inline "lo") " and an instant equal to " (code/inline "hi") " are both in it. "
            (code/inline "between°") " is " (code/inline "(and° (<=° lo n) (<=° n hi))") "; make "
            "either of those strict and exactly these two lines fail while the midpoint and the "
            "outside line keep passing. An inverted window, " (code/inline "lo") " after "
            (code/inline "hi") ", is EMPTY -- it fails, in 18 ms, rather than diverging or "
            "quietly swapping the bounds for the caller -- and a point window still contains its "
            "point.")
         (p (code/inline "span°") " exists because " (code/inline "within°") " with a fresh middle "
            "does not close: " (code/inline "between°") " yields the window and then keeps counting "
            "past " (code/inline "hi") " forever. That mode is not asserted here and cannot be; "
            "asserting it means hanging the file. What is asserted is that "
            (code/inline "span°") " answers the same question and STOPS: a four-second window asked "
            "for nine answers hands back four, a point window hands back one, and an inverted "
            "window hands back none. The over-asking is the assertion -- it pins exhaustion, not "
            "merely correctness.")
         (p "The order is " (code/inline "1760000400") ", " (code/inline "401") ", "
            (code/inline "403") ", " (code/inline "402") ", and it is written down exactly that "
            "way on purpose. " (code/inline "cond°") " is " (code/inline "or°")
            " and " (code/inline "or°") " INTERLEAVES, so a dense span comes out permuted; sorting "
            "the expectation would hide from the reader the one thing they need to know before "
            "building a calendar on top of it, which is that chronology is their job and not the "
            "solver's. The last line is the composition that makes an enumeration usable: a "
            (code/inline "span°") " generating and a " (code/inline "before°")
            " filtering, 269 ms, two survivors. It is also the cost warning -- roughly 50 ms per "
            "second of window at 31 bits, which is why the widest window in this file is four "
            "seconds and why the calendar layers stride over day-counts instead of instants.")))

  ((test/stride°+stride/≤° _)
   (⊦= `(,(build-num 1760000400) ,(build-num 1760004000) ,(build-num 1760007600) ,(build-num 1760011200))
       (μkanren-run (x 4 #t) (stride° (build-instant 2025 10 9 9 0 0) (hours 1) x)))
   (⊦= `(,(build-num 1760000400) ,(build-num 1760004000) ,(build-num 1760007600) ,(build-num 1760011200)
         ,(build-num 1760014800) ,(build-num 1760018400) ,(build-num 1760022000) ,(build-num 1760025600)
         ,(build-num 1760029200))
       (μkanren-run (x 12 #t) (stride/≤° (build-instant 2025 10 9 9 0 0) (hours 1)
                                         (build-instant 2025 10 9 17 30 0) x)))
   (⊦= '(yes) (μkanren-run (q 2 #t) (=° q 'yes)
                (stride/≤° (build-instant 2025 10 9 9 0 0) (hours 1)
                           (build-instant 2025 10 9 17 0 0) (build-instant 2025 10 9 17 0 0))))
   (⊦= '()    (μkanren-run (q 2 #t) (=° q 'yes)
                (stride/≤° (build-instant 2025 10 9 9 0 0) (hours 1)
                           (build-instant 2025 10 9 17 30 0) (build-instant 2025 10 9 17 30 0))))
   (⊦= '()    (μkanren-run (x 4 #t) (stride/≤° (build-instant 2025 10 9 17 30 0) (hours 1)
                                               (build-instant 2025 10 9 9 0 0) x)))
   `(doc (p "The two relations differ by a ceiling and that difference is the whole clause. "
            (code/inline "stride°") " is the GENERATOR: four hourly instants from 09:00, strictly "
            "increasing, 9 ms. It has no last answer and therefore cannot be asked a question -- "
            "given a ground " (code/inline "x") " that is not on the stride it steps past it "
            "forever -- so that mode is documented in the module and deliberately absent here.")
         (p (code/inline "stride/≤°") " carries the bound explicitly, and the MISS is the line that "
            "matters: 17:30 is not on an hourly stride from 09:00, and the query returns "
            (code/inline "()") " in 200 ms instead of running out of memory. Delete the "
            (code/inline "(<=° next hi)") " conjunct from the recursive clause and that line does "
            "not fail -- it HANGS, and a hanging case prints nothing at all, because the suite "
            "reports only once every case has finished. The hit line pairs with it so that a "
            "relation which simply failed always would not pass for a working test. The last line "
            "pins the stricter base clause: " (code/inline "(=° x base)") " is guarded by "
            (code/inline "(<=° base hi)") ", so a base already past the ceiling emits NOTHING, "
            "where a base-only clause would emit one answer outside the window it was given.")
         (p "The nine-answer enumeration is also the price list. Four answers unbounded cost 9 ms; "
            "nine answers bounded cost 382 ms, about 53 ms a step, because the ceiling adds a "
            (code/inline "<=°") " that is itself a search over a 31-bit numeral. An order of "
            "magnitude for termination is the right trade -- an enumeration that does not close is "
            "not an enumeration -- but it is also, precisely, why every calendrical enumeration in "
            "this module strides over DAY-COUNTS at 15 bits and crosses to instants only at the "
            "seam. These five lines are the last ones in the file that are pure: from "
            (code/inline "day+second°") " upward, every relation is " (code/inline "project°")
            "-guarded and the caller starts paying attention to which side is ground.")))

  ; -- calendar: LAYERS 2 and 3, project°-guarded, over DAY-COUNTS ------------------------
  ;
  ; Dropped from this area, deliberately: 1900-02-29 cannot be put to `leap-year°` at all --
  ; the numeral is unsigned, so every pre-epoch year answers "not a leap year" for the wrong
  ; reason, and the 100-rule at 1900 is therefore pinned one layer down, on
  ; `days-from-civil`/`civil-from-days`, where no epoch floor exists.  Dropped too: the
  ; all-fresh mode of `civil°` (it is `numeral°` enumerating the calendar from 1970-01-01,
  ; which belongs with the enumeration cases, not here) and `iso°` in both directions (the
  ; human-readable surface, tested with the ISO clauses).

  ; -- civil°: the calendar as a bijection over day-counts -------------------------------

  ((test/civil° _)
   (⊦= (list (map build-num '(1970 1 1)))
       (μkanren-run (q 5 #t) (fresh° (y m d) (civil° (build-num 0) y m d) (=° q (list y m d)))))
   (⊦= (list (map build-num '(2025 10 9)))
       (μkanren-run (q 5 #t) (fresh° (y m d) (civil° (build-num 20370) y m d) (=° q (list y m d)))))
   (⊦= (list (map build-num '(2024 2 28)))
       (μkanren-run (q 5 #t) (fresh° (y m d) (civil° (build-num 19781) y m d) (=° q (list y m d)))))
   (⊦= (list (map build-num '(2024 2 29)))
       (μkanren-run (q 5 #t) (fresh° (y m d) (civil° (build-num 19782) y m d) (=° q (list y m d)))))
   (⊦= (list (map build-num '(2024 3 1)))
       (μkanren-run (q 5 #t) (fresh° (y m d) (civil° (build-num 19783) y m d) (=° q (list y m d)))))
   (⊦= (list (map build-num '(2025 1 1)))
       (μkanren-run (q 5 #t) (fresh° (y m d) (civil° (build-num 20089) y m d) (=° q (list y m d)))))
   (⊦= (list (map build-num '(2000 2 29)))
       (μkanren-run (q 5 #t) (fresh° (y m d) (civil° (build-num 11016) y m d) (=° q (list y m d)))))
   (⊦= (list (map build-num '(2000 3 1)))
       (μkanren-run (q 5 #t) (fresh° (y m d) (civil° (build-num 11017) y m d) (=° q (list y m d)))))
   (⊦= (list (map build-num '(2400 2 29)))
       (μkanren-run (q 5 #t) (fresh° (y m d) (civil° (build-num 157113) y m d) (=° q (list y m d)))))
   (⊦= `(,(build-num 0))      (μkanren-run (q 5 #t) (civil° q (build-num 1970) (build-num 1) (build-num 1))))
   (⊦= `(,(build-num 19782))  (μkanren-run (q 5 #t) (civil° q (build-num 2024) (build-num 2) (build-num 29))))
   (⊦= `(,(build-num 20089))  (μkanren-run (q 5 #t) (civil° q (build-num 2025) (build-num 1) (build-num 1))))
   (⊦= `(,(build-num 11016))  (μkanren-run (q 5 #t) (civil° q (build-num 2000) (build-num 2) (build-num 29))))
   (⊦= `(,(build-num 157113)) (μkanren-run (q 5 #t) (civil° q (build-num 2400) (build-num 2) (build-num 29))))
   (⊦= '() (μkanren-run (q 5 #t) (civil° q (build-num 2100) (build-num 2) (build-num 29))))
   (⊦= '() (μkanren-run (q 5 #t) (civil° q (build-num 2023) (build-num 2) (build-num 29))))
   (⊦= '() (μkanren-run (q 5 #t) (civil° q (build-num 2025) (build-num 13) (build-num 1))))
   (⊦= '() (μkanren-run (q 5 #t) (civil° q (build-num 2025) (build-num 4) (build-num 31))))
   (⊦= '() (μkanren-run (q 5 #t) (civil° q (build-num 1969) (build-num 12) (build-num 31))))
   `(doc
     (p "The calendar, both ways round, on the dates where an off-by-one hides: day 0, a "
        "recent day, the three days 19781-19783 that bracket a leap day, the first day of a "
        "year, and the pair 11016/11017 -- 2000-02-29 and 2000-03-01 -- which is where "
        (code/inline "civil-from-days") " rolls over a 400-year era. Hinnant's algorithm shifts "
        "the year to start in March precisely so that the leap day is the LAST day of the era, "
        "so 11016 and 11017 sit on opposite sides of the " (code/inline "146097")
        "-day division; an era boundary computed one day out would answer every other case in "
        "this file correctly. 157113 is 2400-02-29, the next such boundary, and the day-count "
        "there is 18 bits rather than 15.")
     (p "Every query asks for FIVE answers and gets one. That is the uniqueness assertion: the "
        "civil calendar is a bijection between day-counts and (y,m,d) triples, and a relation "
        "that offered a second solution -- or that failed to close its stream -- would be "
        "relating a date to something other than its day.")
     (p "The five empty answers are the case's real content. "
        (code/inline "days-from-civil") " is total: handed 2100-02-29 it returns a day-count, "
        "the one belonging to 2100-03-01, and handed 2025-04-31 it returns 2025-05-01. "
        (code/inline "civil°") " does not pass that on. It recomputes the fields from the "
        "day-count it just derived and fails unless they are the ones it was given, so a date "
        "that does not exist has ZERO solutions instead of a neighbour's. 2100 is the "
        "century rule, 2023 the plain non-leap year, 13 an impossible month, April 31 an "
        "impossible day, and 1969-12-31 the epoch floor -- the numeral of "
        (code/inline "(aux kanren arith)") " is unsigned, so the negative day-count is not "
        "representable and the same guard rejects it.")
     (p "What this costs the caller: " (code/inline "civil°") " is " (code/inline "project°")
        "-guarded, so the two fast directions here read their argument as a ground numeral and "
        "leave through Scheme. The guard is an optimisation over a total relation and not a "
        "mode declaration -- with everything fresh the " (code/inline "else") " branch puts "
        (code/inline "numeral°") " in front and enumerates the calendar -- but the price of "
        "the escape is that a PARTIALLY ground numeral is not ground: "
        (code/inline "numeral->number") " returns " (code/inline "#f") " for "
        (code/inline "(1 0 . β)") ", which is how the guard avoids answering from a term it has "
        "not fully seen, and which means a half-solved day-count falls into generate-and-test "
        "rather than into a wrong answer.")))

  ; -- day+second°: the seam, where the instant stops being 31 bits ----------------------

  ((test/day+second° _)
   (⊦= (list (map build-num '(20370 32000)))
       (μkanren-run (q 5 #t) (fresh° (d s) (day+second° (build-num 1760000000) d s) (=° q (list d s)))))
   (⊦= (list (map build-num '(19781 86399)))
       (μkanren-run (q 5 #t) (fresh° (d s) (day+second° (build-num 1709164799) d s) (=° q (list d s)))))
   (⊦= (list (map build-num '(157113 0)))
       (μkanren-run (q 5 #t) (fresh° (d s) (day+second° (build-num 13574563200) d s) (=° q (list d s)))))
   (⊦= `(,(build-num 1760000000))
       (μkanren-run (q 5 #t) (day+second° q (build-num 20370) (build-num 32000))))
   (⊦= `(,(build-num 13574563200))
       (μkanren-run (q 5 #t) (day+second° q (build-num 157113) (build-num 0))))
   (⊦= `(,(build-num 1709164800)) (μkanren-run (q 5 #t) (midnight° q (build-num 19782))))
   (⊦= `(,(build-num 13574563200)) (μkanren-run (q 5 #t) (midnight° q (build-num 157113))))
   (⊦= '() (μkanren-run (q 5 #t) (day+second° q (build-num 1) (build-num 86400))))
   (⊦= (map (lambda (row) (map build-num row)) '((0 0 0) (86400 1 0) (1 0 1) (172800 2 0)))
       (μkanren-run (q 4 #t) (fresh° (t d s) (day+second° t d s) (=° q (list t d s)))))
   (⊦= '(15 18 31 34)
       (map (lambda (n) (length (build-num n))) '(20370 157113 1760000000 13574563200)))
   `(doc
     (p "One relation, " (code/inline "t = d·86400 + sod") ", and it is the only place in the "
        "module where 86400 is written. Everything calendrical is above it and never sees a "
        "31-bit number: the last assertion is the whole argument for the seam, spelled in bits. "
        "A current instant is a 31-bit numeral and a day-count is 15, and what that buys is not "
        "speed. Relational division of an instant by 86400 was measured NOT TO TERMINATE -- "
        "killed after ten minutes -- so in the ground-instant direction the escape to Scheme is "
        "not an optimisation, it is the difference between the relation working and not "
        "existing. In the other direction the pure branch would pay a "
        (code/inline "*°") " of a 15-bit day-count by a 17-bit day, which a prototype measured "
        "at 2.4 s for one product; the guard answers in 2 ms.")
     (p "2400-02-29T00:00:00Z is carried through both directions on purpose. It is a 34-bit "
        "instant, past every plausible fixnum-sized assumption and past the era boundary, and "
        "it is decoded and reassembled exactly. " (code/inline "midnight°")
        " is asserted beside it because it is nothing but this relation with "
        (code/inline "sod") " pinned to " (code/inline "'()") " -- which is what makes it the "
        "one composition over the seam with no bad mode, since a ground zero on one side means "
        "the guard always has something to read.")
     (p "The rejected line is " (code/inline "sod") " = 86400, one second past the end of a "
        "day. It must fail: the relation is a division with a remainder, and a remainder equal "
        "to the divisor would make the encoding non-unique -- (1, 86400) and (2, 0) would name "
        "the same instant, and a day-count would no longer determine a date. The "
        (code/inline "(<° sod DAY)") " conjunct is what forbids it, and it is present in the "
        "guarded branch as well as in the pure one.")
     (p "The four-answer query is the guard's " (code/inline "else") " branch, running with "
        "nothing ground at all: the pure definition, "
        (code/inline "(fresh° (p) (<° sod DAY) (*° d DAY p) (plus° p sod t))")
        ", productive in 2 ms. Asserting it here is what makes the "
        (code/inline "project°") " above honest -- the guard changes the clock and not the "
        "answer set. The ORDER is the µKanren interleaving and not chronology, which is why "
        "86400 arrives before 1.")))

  ; -- date-time°: instant <-> (y,mo,d,hh,mm,ss), against an independent oracle -----------

  ((test/date-time° _)
   (define (leap? y) (and (zero? (modulo y 4)) (or (not (zero? (modulo y 100))) (zero? (modulo y 400)))))
   (define (month-length y m) (case m ((1 3 5 7 8 10 12) 31) ((4 6 9 11) 30) (else (if (leap? y) 29 28))))
   (define (oracle-fields t)
     (let year ((ds (quotient t 86400)) (y 1970))
       (if (>= ds (if (leap? y) 366 365))
           (year (- ds (if (leap? y) 366 365)) (+ y 1))
           (let month ((ds ds) (m 1))
             (if (>= ds (month-length y m))
                 (month (- ds (month-length y m)) (+ m 1))
                 (let ((sod (modulo t 86400)))
                   (list y m (+ ds 1)
                         (quotient sod 3600) (quotient (modulo sod 3600) 60) (modulo sod 60))))))))
   (define (oracle-instant y mo d hh mm ss)
     (let year ((yy 1970) (ds 0))
       (if (< yy y)
           (year (+ yy 1) (+ ds (if (leap? yy) 366 365)))
           (let month ((m 1) (ds ds))
             (if (< m mo)
                 (month (+ m 1) (+ ds (month-length y m)))
                 (+ (* 86400 (+ ds d -1)) (* 3600 hh) (* 60 mm) ss))))))
   (define spread
     (list 0 1760000000 1709164799 1709208000 1709251200 1735689600 951782400 951868800 13574563200))
   (define (fields t)
     (map numeral->number
          (car (μkanren-run (q 5 #t)
                 (fresh° (y mo d hh mm ss)
                   (date-time° (build-num t) y mo d hh mm ss)
                   (=° q (list y mo d hh mm ss)))))))
   (define (instant y mo d hh mm ss)
     (map numeral->number
          (μkanren-run (q 5 #t)
            (date-time° q (build-num y) (build-num mo) (build-num d)
                          (build-num hh) (build-num mm) (build-num ss)))))
   (⊦= spread
       (list (oracle-instant 1970 1 1 0 0 0)   (oracle-instant 2025 10 9 8 53 20)
             (oracle-instant 2024 2 28 23 59 59) (oracle-instant 2024 2 29 12 0 0)
             (oracle-instant 2024 3 1 0 0 0)   (oracle-instant 2025 1 1 0 0 0)
             (oracle-instant 2000 2 29 0 0 0)  (oracle-instant 2000 3 1 0 0 0)
             (oracle-instant 2400 2 29 0 0 0)))
   (for-each (lambda (t) (⊦= (oracle-fields t) (fields t))) spread)
   (for-each (lambda (t) (⊦= (list t) (apply instant (oracle-fields t)))) spread)
   (⊦= '(1970 1 1 0 0 0) (fields 0))
   (⊦= '(2024 2 29 12 0 0) (fields 1709208000))
   (⊦= '(2400 2 29 0 0 0) (fields 13574563200))
   (⊦= '(1709251200) (instant 2024 3 1 0 0 0))
   (⊦= '() (μkanren-run (q 5 #t) (date-time° q (build-num 2100) (build-num 2) (build-num 29)
                                               (build-num 0) (build-num 0) (build-num 0))))
   (⊦= '() (μkanren-run (q 5 #t) (date-time° q (build-num 2024) (build-num 2) (build-num 29)
                                               (build-num 25) (build-num 0) (build-num 0))))
   (⊦= '() (μkanren-run (q 5 #t) (date-time° q (build-num 2024) (build-num 2) (build-num 29)
                                               (build-num 23) (build-num 59) (build-num 60))))
   (⊦= (map build-num '(2024 2 29 12 0 0))
       (car (μkanren-run (q 5 #t)
              (fresh° (t y mo d hh mm ss)
                (shift° (build-instant 2024 2 28 12 0 0) (days 1) t)
                (date-time° t y mo d hh mm ss)
                (=° q (list y mo d hh mm ss))))))
   (⊦= (map build-num '(2023 3 1 12 0 0))
       (car (μkanren-run (q 5 #t)
              (fresh° (t y mo d hh mm ss)
                (shift° (build-instant 2023 2 28 12 0 0) (days 1) t)
                (date-time° t y mo d hh mm ss)
                (=° q (list y mo d hh mm ss))))))
   (⊦= (map (lambda (row) (map build-num row)) '((0 1 1 0 0 0) (86400 1 2 0 0 0) (1 1 1 0 0 1)))
       (μkanren-run (q 3 #t)
         (fresh° (t mo d hh mm ss)
           (date-time° t (build-num 1970) mo d hh mm ss)
           (=° q (list t mo d hh mm ss)))))
   `(doc
     (p "The oracle is written out inside the case, and it shares no line of code with the "
        "module: it counts years from 1970 by 365 or 366, then months off a twelve-entry "
        "length table, while " (code/inline "(aux kanren date)") " reaches the same answer "
        "through Hinnant's era arithmetic, where the leap rule appears only as the 146097-day "
        "period and February's length is never written down anywhere. Two implementations that "
        "agree on all nine instants, in both directions, are agreeing about the Gregorian "
        "calendar and not about a shared mistake. The agreement is asserted as a loop rather "
        "than as literals so that the oracle, not this file, is what the relation is measured "
        "against; the four literal lines after it are there so a reader can see actual numbers, "
        "and the first assertion pins the oracle itself to the nine timestamps.")
     (p "The spread is chosen so that each entry can fail alone: the epoch; a recent instant "
        "with a non-zero clock; 2024-02-28T23:59:59Z, the last second before a leap day; noon "
        "ON the leap day; the day after it; a year boundary at midnight, where the day-of-year "
        "resets; and the 2000-02-29 / 2000-03-01 pair that straddles a 400-year era, plus "
        "2400-02-29 at 34 bits to show the era arithmetic is periodic and not a fixed offset.")
     (p "The three empty answers say that the fields direction VALIDATES. 2100-02-29 is a "
        "date the century rule deletes and " (code/inline "civil°") " rejects; 25:00:00 is not "
        "a clock reading; 23:59:60 is a leap second, and this module is POSIX time, where every "
        "day is exactly 86400 seconds long, so it must have no instant at all. A relation that "
        "normalised any of the three would answer with a plausible instant an hour or a day "
        "away, and nothing downstream would notice.")
     (p "The two " (code/inline "shift°") " lines are the leap day tested through arithmetic "
        "rather than through the calendar: one day added to 2024-02-28T12:00:00Z lands on "
        "February 29, and the same addition in 2023 lands on March 1. The shift itself is pure "
        "layer 1 -- " (code/inline "plus°") " on a 31-bit numeral, a few milliseconds -- and it "
        "knows nothing about months; it is the decode that has to place the result. This is the "
        "composition a caller actually writes, and it would still typecheck with a calendar "
        "that had February wrong.")
     (p "What the guard costs the caller: " (code/inline "date-time°") " has no arithmetic of "
        "its own, only a " (code/inline "project°") " that chooses the ORDER of one conjunction "
        "-- with " (code/inline "t") " ground it splits first, so both decoders see a ground "
        "input; with the fields ground it decodes first, so the seam is handed a ground "
        "(day, second-of-day) and never multiplies two logic variables. The last query is the "
        "mode neither branch is optimised for, with only the year pinned, and it is included to "
        "assert that the answer set is still real dates: 1970-01-01T00:00:00Z, then "
        "1970-01-02T00:00:00Z, then 1970-01-01T00:00:01Z. That is the interleaving of the "
        "day-count and second-of-day streams, NOT chronological order, and any expectation "
        "written here by reasoning rather than by running would have had the second and third "
        "answers the other way round.")))

  ; -- leap-year°: the 4/100/400 rule, and where the epoch floor hides it -----------------

  ((test/leap-year° _)
   (define (year-length y)
     (map numeral->number
          (μkanren-run (q 5 #t)
            (fresh° (lo hi hi+1 len)
              (year° (build-num y) lo hi)
              (plus° hi '(1) hi+1) (plus° lo len hi+1) (=° q len)))))
   (define (february-length y)
     (map numeral->number
          (μkanren-run (q 5 #t)
            (fresh° (lo hi hi+1 len)
              (month° (build-num y) (build-num 2) lo hi)
              (plus° hi '(1) hi+1) (plus° lo len hi+1) (=° q len)))))
   (⊦= '(yes) (μkanren-run (r 5 #t) (leap-year° (build-num 1972)) (=° r 'yes)))
   (⊦= '(yes) (μkanren-run (r 5 #t) (leap-year° (build-num 2000)) (=° r 'yes)))
   (⊦= '(yes) (μkanren-run (r 5 #t) (leap-year° (build-num 2024)) (=° r 'yes)))
   (⊦= '(yes) (μkanren-run (r 5 #t) (leap-year° (build-num 2400)) (=° r 'yes)))
   (⊦= '() (μkanren-run (r 5 #t) (leap-year° (build-num 2025)) (=° r 'yes)))
   (⊦= '() (μkanren-run (r 5 #t) (leap-year° (build-num 2100)) (=° r 'yes)))
   (⊦= '() (μkanren-run (r 5 #t) (leap-year° (build-num 2200)) (=° r 'yes)))
   (⊦= '() (μkanren-run (r 5 #t) (leap-year° (build-num 1968)) (=° r 'yes)))
   (⊦= '() (μkanren-run (r 5 #t) (leap-year° (build-num 1900)) (=° r 'yes)))
   (⊦= '(1900 3 1)  (civil-from-days (days-from-civil 1900 2 29)))
   (⊦= '(1968 2 29) (civil-from-days (days-from-civil 1968 2 29)))
   (⊦= '(2000 2 29) (civil-from-days (days-from-civil 2000 2 29)))
   (⊦= '(2100 3 1)  (civil-from-days (days-from-civil 2100 2 29)))
   (⊦= 146097 (- (days-from-civil 2000 3 1) (days-from-civil 1600 3 1)))
   (⊦= 146097 (- (days-from-civil 2400 3 1) (days-from-civil 2000 3 1)))
   (⊦= '(366) (year-length 1972))
   (⊦= '(365) (year-length 2025))
   (⊦= '(366) (year-length 2000))
   (⊦= '(365) (year-length 2100))
   (⊦= '(366) (year-length 2400))
   (⊦= '(29) (february-length 2024))
   (⊦= '(28) (february-length 2025))
   (⊦= '(29) (february-length 2000))
   (⊦= '(28) (february-length 2100))
   (⊦= '(29) (february-length 2400))
   `(doc
     (p (code/inline "leap-year°") " has no arithmetic of its own. Its body is "
        (code/inline "(fresh° (d) (civil° d y '(0 1) '(1 0 1 1 1)))") " -- a year is a leap "
        "year exactly when the 29th of February is a date -- so this case is really asking "
        "whether " (code/inline "civil°") "'s round-trip guard implements the 4/100/400 rule. "
        "2024 is the plain rule, 2100 and 2200 are the centuries it deletes, 2000 and 2400 are "
        "the ones the 400 rule gives back. Take away the recomputation in "
        (code/inline "civil°") " and all seven answer yes, because "
        (code/inline "days-from-civil") " is happy to hand back the day-count of March 1.")
     (p "1968 answers NO although 1968 is a leap year, and that is not a bug -- it is the "
        "epoch floor, asserted so that no reader mistakes it for one. The numeral of "
        (code/inline "(aux kanren arith)") " is unsigned, 1968-02-29 has a negative day-count, "
        "and " (code/inline "civil°") " fails on it. So " (code/inline "leap-year°")
        " is a decision procedure only for years from 1970 on; before that it answers no for a "
        "reason that has nothing to do with leap years. 1900, the century rule's headline case, "
        "is therefore unreachable through the relation, and the four "
        (code/inline "civil-from-days") " lines pin it one layer down instead, on plain Scheme "
        "where negative day-counts exist: 1900-02-29 comes back as 1900-03-01 and is not a "
        "date, while 1968-02-29 and 2000-02-29 come back unchanged and are. The two 146097 "
        "lines are the same rule stated as a period -- four hundred Gregorian years are exactly "
        "146097 days, twice over, which is the identity Hinnant's era arithmetic is built on "
        "and the reason there is no leap-year branch anywhere in the kernel.")
     (p "The lengths are computed by the RELATIONS, not read off a table. "
        (code/inline "year°") " and " (code/inline "month°") " answer with the first and last "
        "day-count of the window -- " (code/inline "month°") " by way of a twelve-row "
        (code/inline "next-month°") " successor table and " (code/inline "civil°") " on the "
        "first of the following month -- and the two " (code/inline "plus°") " goals then "
        "measure it, inclusive of both endpoints. That subtraction is pure layer 1 at 15 to 18 "
        "bits, about 7 ms a query. February 2024 is 29 days and February 2100 is 28 without "
        "either number appearing in the module, which is the strongest available statement "
        "that the calendar is derived and not tabulated; and because the window is a relation "
        "over numerals rather than a pair of Scheme integers, the same goals run with the year "
        "left fresh.")))

  ; -- weekdays: the headline feature, by STRIDE and never by modulo ---------------------
  ;
  ; test/weekday°/epoch calls `seconds->utc-time`, which is why the import list above carries
  ; `(chicken time posix)` alongside `(aux kanren date)`.  That is deliberate: the epoch
  ; anchor is the one fact no relation in the module can witness for itself.
  ;
  ; Dropped from this area, and why:
  ;   * `stride°` used as a test against an off-stride ground `x` -- it diverges by
  ;     construction, so there is no assertion to write; the two `stride/≤°` rows in
  ;     test/weekday-names°/refuse stand in for it and say in prose what the unbounded one does.
  ;   * `day-of-week°` on a HALF-ground day-count such as `(1 0 . β)` -- sound (it falls to the
  ;     generate-and-test branch, which is the point of `numeral->number`), but the answer
  ;     stream is the whole calendar, so there is nothing finite to pin.  Described in prose.
  ;   * `day-of-week/within°` and `weekday-midnight°` -- the windowed enumerations; they belong
  ;     with the window cases, and only their fallback branch is referred to here.

  ((test/weekday°/epoch _)
   ; field 6 of a broken-down UTC time is the weekday, 0 = Sunday.
   (⊦= 4 (vector-ref (seconds->utc-time 0) 6))
   (⊦= 1 (vector-ref (seconds->utc-time 345600) 6))
   (⊦= 'thursday (weekday-of-day 0))
   (⊦= 'monday   (weekday-of-day 4))
   (⊦= (build-num 4)      (build-day 1970 1 5))
   (⊦= (build-num 345600) (build-instant 1970 1 5))
   (⊦= '(monday)                 (μkanren-run (w 2 #t) (weekday° (build-num 345600) w)))
   (⊦= (list (build-num 4))      (μkanren-run (d 1 #t) (monday° d)))
   (⊦= (list (build-num 345600)) (μkanren-run (t 2 #t) (midnight° t (build-num 4))))
   (⊦= '(yes) (μkanren-run (q 2 #t) (=° q 'yes) (thursday° (build-num 0))))
   (⊦= '()    (μkanren-run (q 5 #t) (=° q 'yes) (monday° (build-num 3))))
   `(doc (p "The anchor every other weekday case stands on, and the one fact in the module "
            "that nothing inside the module can check. " (code/inline "weekday-epoch-day")
            " is " (code/inline "#(3 4 5 6 0 1 2)") " -- the smallest day-count carrying each "
            "weekday -- and every generator below starts from one of those seven offsets. "
            "Rotate the vector by one and the module stays perfectly self-consistent: "
            (code/inline "monday°") " would agree with " (code/inline "day-of-week°")
            ", which would agree with " (code/inline "weekend°") ", and all three would be a "
            "day out. Only an outside witness can say so, which is why the first two rows "
            "call " (code/inline "(chicken time posix)") " rather than anything in "
            (code/inline "(aux kanren date)") ": field 6 of " (code/inline "seconds->utc-time")
            " is the weekday with 0 for Sunday, and it reads 4 for the epoch and 1 for 345600.")
         (p "The rest is the same claim made from inside, at each of the three magnitudes the "
            "module speaks. 1970-01-01 is a Thursday, so 1970-01-05 is day-count 4 and instant "
            "345600; " (code/inline "(monday° d)") " with " (code/inline "d")
            " fresh offers 4 as its very first answer, so the stride anchor and the calendar "
            "agree; and " (code/inline "midnight°") " carries day 4 back up to 345600, closing "
            "the loop between the day-count layer and the instant layer. The last row is day "
            "3, the Sunday before: five answers were asked for and none came, which is a miss "
            "that TERMINATES -- see " (code/inline "test/weekday-names°/refuse") ".")))

  ((test/day-of-week° _)
   (⊦= '(thursday) (μkanren-run (w 2 #t) (day-of-week° (build-day 2025 10 9) w)))
   (⊦= (map build-num '(4 11 18 25 32)) (μkanren-run (d 5 #t) (day-of-week° d 'monday)))
   (⊦= (map build-num '(3 10 17 24 31)) (μkanren-run (d 5 #t) (day-of-week° d 'sunday)))
   (⊦= (list (list (build-num 0) 'thursday) (list (build-num 1) 'friday)
             (list (build-num 2) 'saturday) (list (build-num 3) 'sunday))
       (μkanren-run (q 4 #t) (fresh° (d w) (=° q (list d w)) (day-of-week° d w))))
   (⊦= '() (μkanren-run (q 5 #t) (=° q 'yes) (day-of-week° (build-day 2025 10 9) 'monday)))
   `(doc (p "One relation, three modes, and three different mechanisms answering them -- which "
            "is what " (code/inline "project°") " buys and what it costs. With the day-count "
            "ground the goal ESCAPES to Scheme and unifies the answer, so the test is 0 ms and "
            "the relational alternatives are all worse: " (code/inline "/°")
            " on a day-count was measured at 2.9 s, striding from the epoch to 2025 at 15.5 s, "
            "and " (code/inline "multiple°") " as a divisibility test gave no answer at all "
            "after three minutes on a negative case. With the WEEKDAY ground instead it is a "
            "pure unbounded " (code/inline "stride°") " from that weekday's epoch anchor, "
            "about a millisecond an answer and genuinely increasing -- 4, 11, 18, 25, 32 for "
            "Monday, 3, 10, 17, 24, 31 for Sunday, each seven apart from its anchor. With "
            "neither ground " (code/inline "numeral°") " proposes a day-count and the escape "
            "names its weekday, so the relation is TOTAL: no error, no hang, just the calendar "
            "in order from the epoch.")
         (p "What the guard costs the caller is that the branch is chosen by looking at "
            "instantiation AT THE MOMENT THE GOAL RUNS, so conjunction order is now part of "
            "the meaning. The groundness test is " (code/inline "numeral->number")
            ", positive typing over the cells rather than " (code/inline "(not (var? x))")
            ", so a half-built day-count like " (code/inline "(1 0 . β)")
            " is correctly NOT taken for ground and falls to the generate-and-test branch "
            "instead of being decoded as if the tail were empty. The last row is the "
            "both-ground mode: a wrong weekday is refused by unification, in no time and with "
            "no search, which is the whole reason the next case terminates.")))

  ((test/weekday-names° _)
   (⊦= '(() () () () (yes) () ())
       (map (lambda (r) (μkanren-run (q 2 #t) (=° q 'yes) (r (build-day 2025 10 9))))
            (list sunday° monday° tuesday° wednesday° thursday° friday° saturday°)))
   (⊦= '((yes) (yes) (yes) (yes) (yes) (yes) (yes))
       (map (lambda (r d) (μkanren-run (q 2 #t) (=° q 'yes) (r (build-num d))))
            (list sunday° monday° tuesday° wednesday° thursday° friday° saturday°)
            '(20366 20367 20368 20369 20370 20371 20372)))
   (⊦= (map build-num '(1 8 15 22)) (μkanren-run (d 4 #t) (friday° d)))
   (⊦= (map build-num '(2 9 16 23)) (μkanren-run (d 4 #t) (saturday° d)))
   ; a DAY-COUNT, never an instant: 1760000000 days after the epoch is a real date
   (⊦= '(sunday) (μkanren-run (w 2 #t) (day-of-week° (build-num 1760000000) w)))
   `(doc (p "The seven names the module is for, each a line of sugar over "
            (code/inline "day-of-week°") " and inheriting both of its directions for nothing. "
            "The first row runs all seven against 2025-10-09 and exactly one answers, which is "
            "the partition property: the seven relations are mutually exclusive and jointly "
            "exhaustive over day-counts, and a table that had drifted would show up here as "
            "two hits or none. The second row is the diagonal, 20366 to 20372 -- 2025-10-05 to "
            "2025-10-11, a full week beginning on a Sunday -- and it is the row that pins WHICH "
            "name goes with which offset. Test a single name and any cyclic rotation of "
            (code/inline "weekday-names") " passes; test all seven in order against seven "
            "consecutive days and only the true assignment does.")
         (p "The generators read the anchors straight out of "
            (code/inline "weekday-epoch-day") ": Fridays are 1, 8, 15, 22 and Saturdays 2, 9, "
            "16, 23, because 1970-01-02 was a Friday and 01-03 a Saturday. The last row is the "
            "unit trap, and it is written down rather than merely warned about because nothing "
            "else in the module will catch it. These relations take a DAY-COUNT, not an "
            "instant. Handing one a unixtime is not an error and does not fail: 1760000000 "
            "days after the epoch is 4820686-05-02, and the module cheerfully reports it a "
            "Sunday. " (code/inline "weekday°") " is the relation that takes an instant; these "
            "seven are a layer below it, and " (code/inline "midnight°") " is the lift.")))

  ((test/weekday-names°/refuse _)
   (⊦= '() (μkanren-run (q 20 #t) (=° q 'yes) (monday° (build-day 2025 10 9))))
   (⊦= '() (μkanren-run (q 20 #t) (=° q 'yes) (sunday° (build-day 2025 10 9))))
   (⊦= '() (μkanren-run (q 20 #t) (=° q 'yes) (weekend° (build-day 2025 10 10))))
   (⊦= '(yes) (μkanren-run (q 3 #t) (=° q 'yes)
                (stride/≤° (build-num 4) (build-num 7) (build-num 25) (build-num 18))))
   (⊦= '()    (μkanren-run (q 3 #t) (=° q 'yes)
                (stride/≤° (build-num 4) (build-num 7) (build-num 25) (build-num 19))))
   (⊦= (map build-num '(4 11 25 18))
       (μkanren-run (x 9 #t) (stride/≤° (build-num 4) (build-num 7) (build-num 25) x)))
   (⊦= (map build-num '(4 11 18 25))
       (μkanren-run (x 4 #t) (stride° (build-num 4) (build-num 7) x)))
   `(doc (p "The case the whole design is arranged around: a weekday name used as a test on a "
            "date that is NOT that weekday must fail, and must fail in finite time. 2025-10-09 "
            "is a Thursday, so " (code/inline "monday°") " and " (code/inline "sunday°")
            " refuse it and " (code/inline "weekend°") " refuses the Friday after; each asks "
            "for twenty answers and receives none, in no measurable time. An assertion of "
            (code/inline "'()") " is the only kind that can distinguish failure from "
            "divergence, and only by being run: a goal that hangs never gets as far as "
            "disagreeing with an expectation.")
         (p "It is not a free property. The generator direction of these relations IS an "
            "unbounded stride, and what happens when that stride is asked to serve as a test "
            "was measured: " (code/inline "(stride° base step x)") " with " (code/inline "x")
            " ground and off the stride steps past it forever, because nothing in the "
            "recursive clause compares the running value to " (code/inline "x")
            ". The three " (code/inline "stride/≤°") " rows are the two ways out written side "
            "by side. A ceiling makes the stride terminate on a miss -- 18 is on 4+7k and "
            "answers, 19 is not and the stream simply ends -- and that is the relation to "
            "reach for when a stride must decide. " (code/inline "day-of-week°")
            " takes the other way out: with the day-count ground it never strides at all, it "
            "escapes, which is why 4, 11, 18, 25 and \"is 20370 a Monday\" cost the same "
            "nothing.")
         (p "The last two rows also pin the ORDER, because the two relations enumerate the "
            "same set and do not agree on sequence. " (code/inline "stride°") " walks 4, 11, "
            "18, 25; " (code/inline "stride/≤°") " answers 4, 11, 25, 18. The ceiling "
            "comparison sits inside the recursive clause, so the answers come back in the "
            "interleaving of " (code/inline "or°") " rather than in the calendar's. Anything "
            "downstream that assumed a bounded stride was sorted is wrong, and the module says "
            "so.")))

  ((test/weekend° _)
   (⊦= '(yes) (μkanren-run (q 2 #t) (=° q 'yes) (weekend° (build-num 20372))))
   (⊦= '(yes) (μkanren-run (q 2 #t) (=° q 'yes) (weekend° (build-num 20366))))
   (⊦= '()    (μkanren-run (q 3 #t) (=° q 'yes) (weekend° (build-num 20370))))
   (⊦= '(yes) (μkanren-run (q 2 #t) (=° q 'yes) (workday° (build-num 20370))))
   (⊦= '()    (μkanren-run (q 3 #t) (=° q 'yes) (workday° (build-num 20372))))
   (⊦= (map build-num '(2 3 9 10 16 17)) (μkanren-run (d 6 #t) (weekend° d)))
   (⊦= (map build-num '(4 5 6 0 1))      (μkanren-run (d 5 #t) (workday° d)))
   (⊦= (map build-num '(4 5 6 0 1 11 7 12 18 13)) (μkanren-run (d 10 #t) (workday° d)))
   `(doc (p "The two filters, over the same week as above: 20366 is a Sunday and 20372 a "
            "Saturday, 20370 a Thursday, and each relation refuses what the other accepts. "
            "The content of both definitions is CONJUNCTION ORDER. The disjunction that pins "
            (code/inline "w") " comes first, so that with the day-count fresh "
            (code/inline "day-of-week°") " still sees a ground weekday and takes its stride "
            "branch; write the two goals the other way round and the guard finds nothing "
            "ground, falls to generate-and-test, and enumerates every day-count in the "
            "calendar in order to keep two-sevenths of them. Both readings are sound and only "
            "one returns in time, which is the recurring price of a "
            (code/inline "project°") "-guarded relation: its callers have to be written for "
            "the mode they want.")
         (p (code/inline "workday°") " spells out its five rows rather than negating the "
            "weekend with " (code/inline "≠°") " for the same reason -- a disequality "
            "CONSTRAINS " (code/inline "w") " without GROUNDING it, so the guard would still "
            "see a fresh weekday and the generator direction would be lost. The last three "
            "rows are what that direction actually looks like, and they are pinned because "
            "they are not what a reader expects. Weekends come out 2, 3, 9, 10, 16, 17: two "
            "streams interleaving so cleanly they look sorted. Workdays come out 4, 5, 6, 0, "
            "1 -- the five anchors, in the order the " (code/inline "or°")
            " lists them, so Monday's 4 before Thursday's 0 -- and then 11, 7, 12, 18, 13. "
            "Five interleaved streams are visibly not a chronology, and a filter does not "
            "preserve the order of what it filters.")))

  ((test/weekday-name° _)
   (⊦= '(sunday monday tuesday wednesday thursday friday saturday)
       (μkanren-run (w 9 #t) (weekday-name° w)))
   (⊦= '(sunday monday tuesday) (μkanren-run (w 3 #t) (weekday-name° w)))
   (⊦= '(yes) (μkanren-run (q 3 #t) (=° q 'yes) (weekday-name° 'monday)))
   (⊦= '()    (μkanren-run (q 3 #t) (=° q 'yes) (weekday-name° 'funday)))
   (⊦= '(thursday)
       (μkanren-run (w 3 #t) (weekday-name° w) (day-of-week° (build-day 2025 10 9) w)))
   `(doc (p "Seven unifications in a " (code/inline "cond°") ", the only entirely pure "
            "relation in this group, and the one the guarded ones lean on. The first row asks "
            "for NINE and gets seven: the stream CLOSES, which is the property that matters "
            "and the reason a count larger than the answer set is the right thing to write "
            "here. A generator put in front of a guarded relation has to terminate, or the "
            "guarded relation's open mode terminates no better than the hang it was meant to "
            "avoid. The prefix of three fixes the order as the clause order, Sunday first, "
            "matching " (code/inline "weekday-names") " and " (code/inline "weekday-epoch-day")
            " index for index.")
         (p "Used the other way it is a membership test, and " (code/inline "funday")
            " gets zero answers rather than quietly being accepted as an eighth weekday -- the "
            "same guard as " (code/inline "weekday-name?") ", which is what every "
            (code/inline "project°") " in the calendar layer consults before it trusts a "
            "symbol. The last row is the idiom the relation exists for, and it is "
            (code/inline "prime°") "'s discipline from " (code/inline "(aux kanren arith)")
            " transplanted: where a mode would otherwise be undefined, put a GENERATOR in "
            "front of the guard instead of raising an error. Seven candidates are proposed, "
            "the ground day-count filters six away, Thursday survives. "
            (code/inline "day-of-week/within°") " does exactly this in its fallback branch, "
            "and it must generate the WEEKDAY and not the day-count: generating the answer "
            "instead leaves " (code/inline "w") " fresh on re-entry, the fallback fires again, "
            "and that version was measured hanging.")))

  ; -- iso: the human-readable surface, plain Scheme under one project° ------------------
  ;
  ; Dropped from this area, deliberately: (1) a pre-1970 instant has no formatting case at
  ; all -- `number->numeral` raises before any string is built, so the epoch floor is
  ; `build-instant`'s obligation and is asserted where that function is tested, not here;
  ; (2) `iso°` with a HALF-ground instant such as `(1 0 . β)`, because `numeral?` rightly
  ; refuses it and the goal falls to the open branch, which enumerates from the epoch and
  ; never reads the known bits -- sound, useless, and an assertion about the enumeration
  ; rather than about ISO; (3) the 2000-instant fuzz round trip (6 ms, 0 mismatches,
  ; observed in the probe) -- the named boundaries below pin the same invariant and say
  ; which boundary broke.

  ((test/instant->iso _)
   (⊦= "1970-01-01T00:00:00Z" (instant->iso 0))
   (⊦= "2025-01-02T03:04:05Z" (instant->iso 1735787045))
   (⊦= '("2025" "01" "02" "03" "04" "05")
       (let ((s (instant->iso 1735787045)))
         (list (substring s 0 4) (substring s 5 7) (substring s 8 10)
               (substring s 11 13) (substring s 14 16) (substring s 17 19))))
   (⊦= '(#\T #\Z)
       (let ((s (instant->iso 1735787045))) (list (string-ref s 10) (string-ref s 19))))
   (⊦= '("1970-01-01T00:00:00Z" "1970-01-01T00:00:01Z" "1970-01-01T00:00:59Z"
         "1970-01-01T00:01:00Z" "1970-01-01T00:59:59Z" "1970-01-01T01:00:00Z"
         "1970-01-01T23:59:59Z" "1970-01-05T00:00:00Z" "2000-02-29T00:00:00Z"
         "2025-10-09T08:53:20Z" "2038-01-19T03:14:07Z")
       (map instant->iso '(0 1 59 60 3599 3600 86399 345600 951782400 1760000000 2147483647)))
   (⊦= '(20 20 20 20)
       (map (lambda (t) (string-length (instant->iso t)))
            '(0 1735787045 1760000000 253402300799)))
   (⊦= '("00" "05" "59" "1970" "0999" "45")
       (list (zero-pad 0 2) (zero-pad 5 2) (zero-pad 59 2)
             (zero-pad 1970 4) (zero-pad 999 4) (zero-pad 12345 2)))
   (⊦= "9999-12-31T23:59:59Z" (instant->iso 253402300799))
   (⊦= "0000-01-01T00:00:00Z" (instant->iso 253402300800))
   (⊦= #f (iso->instant (instant->iso 253402300800)))
   `(doc (p "The epoch is asserted as a literal string because it is the one instant every "
            "other case in this file is measured from: " (code/inline "(instant->iso 0)")
            " is " (code/inline "\"1970-01-01T00:00:00Z\"") " exactly, and the numeral of that "
            "instant is " (code/inline "'()") ", so the encoding's zero and the calendar's "
            "origin are the same object. An epoch off by one day would leave every relation in "
            "the module self-consistent and every string in it wrong.")
         (p "1735787045 is in here for one reason: it is "
            (code/inline "2025-01-02T03:04:05Z") ", the instant whose month, day, hour, minute "
            "and second are ALL single digits at once. It is the only shape that catches the "
            "bug this kind of code always has -- a bare " (code/inline "~a")
            " on a field that happens to be 5 -- and the field-by-field "
            (code/inline "substring") " line names which field lost its zero rather than making "
            "a reader diff two twenty-character strings. The padding is not cosmetic: it is what "
            "makes lexicographic order agree with chronological order, so a sorted list of "
            "these strings is a sorted list of instants, and " (code/inline "\"2025-1-2\"")
            " would break that silently while still reading correctly to a human. The width "
            "assertion is the cheap standing guard: an ISO instant here is TWENTY characters, "
            (code/inline "T") " at index 10 and " (code/inline "Z") " at index 19, and those "
            "offsets are exactly the ones " (code/inline "iso->instant") " slices at.")
         (p "The last three lines pin the far edge honestly. "
            (code/inline "zero-pad") " is " (code/inline "string-pad")
            ", which keeps the RIGHT-hand end of a string too long for the field: "
            (code/inline "(zero-pad 12345 2)") " is " (code/inline "\"45\"") ". So the year is "
            "four digits or it is a lie, and the year 10000 -- one second past the last "
            "assertion above -- formats as " (code/inline "\"0000-01-01T00:00:00Z\"") ". That "
            "is why the line after it re-reads that string: the truncation cannot become a "
            "wrong instant, because " (code/inline "\"0000\"") " is pre-epoch and the parser "
            "answers " (code/inline "#f") ". The supported range is [1970, 9999], and it is "
            "asserted here rather than left to a reader's assumption.")))

  ((test/iso->instant _)
   (⊦= 0 (iso->instant "1970-01-01T00:00:00Z"))
   (⊦= 1735787045 (iso->instant "2025-01-02T03:04:05Z"))
   (⊦= 1760000000 (iso->instant "2025-10-09T08:53:20Z"))
   (⊦= 1709208000 (iso->instant "2024-02-29T12:00:00Z"))
   (⊦= '(0 1 59 60 86399 86400 345600 951782400 1735787045 1760000000 2147483647)
       (map (lambda (t) (iso->instant (instant->iso t)))
            '(0 1 59 60 86399 86400 345600 951782400 1735787045 1760000000 2147483647)))
   `(doc (p "The parse direction on well-formed input, and then the statement that the two "
            "string functions are inverses. Both of them are plain Scheme over plain integers "
            "-- no goal, no numeral, no " (code/inline "project°") " -- and that is the whole "
            "point of testing them apart from " (code/inline "iso°") ": the relation does "
            "nothing but choose between these two functions, so anything wrong with the format "
            "is wrong here first, where a failure names a number instead of a stream.")
         (p "The spread is chosen at the boundaries where a carry is made or dropped, because "
            "an arithmetic slip inside " (code/inline "civil-from-days")
            " or the second-of-day split shows up there and nowhere else: 0 and 1; 59 and 60, "
            "the minute; 86399 and 86400, the day, which is the seam the whole module is built "
            "around; 345600, the first Monday of the epoch; 951782400, "
            (code/inline "2000-02-29") ", a leap day in the century year that IS a leap year; "
            "1735787045, the all-single-digit instant again, now read back; and 2147483647, the "
            "signed 32-bit ceiling, which is a 31-bit numeral and the largest instant anyone is "
            "likely to hand this module by accident. Round-tripping through the string and "
            "comparing integers is what makes the assertion legible; comparing strings would "
            "pass just as well for a formatter and parser that agreed on the same wrong "
            "calendar, which is why the calendar itself is cross-checked against "
            (code/inline "(chicken time posix)") " elsewhere in this suite and not here.")))

  ((test/iso->instant/rejects _)
   (⊦= '(#f #f #f #f #f #f #f #f #f #f #f #f #f)
       (map iso->instant
            '("2025-02-30T00:00:00Z" "2100-02-29T00:00:00Z" "2025-13-01T00:00:00Z"
              "2025-04-31T00:00:00Z" "2025-10-09T25:00:00Z" "2025-10-09T00:60:00Z"
              "2025-10-09T00:00:60Z" "1969-12-31T23:59:59Z" "2025-10-09 08:53:20Z"
              "2025-10-09T08:53:20"  "2025-10-09T08:53:20+01:00" "not a date at all!!" "")))
   (⊦= #f (iso->instant 1760000000))
   `(doc (p "What happens on malformed input, said once and for all: "
            (code/inline "#f") ", never a condition and never a neighbouring instant. The "
            "thirteen strings are one row each of the three checks the parser makes, and the "
            "distinction between them is the interesting part. SHAPE is decided by three "
            "constants -- length 20, " (code/inline "T") " at 10, " (code/inline "Z")
            " at 19 -- and that is what refuses a space separator, a missing "
            (code/inline "Z") ", an offset of " (code/inline "\"+01:00\"")
            ", a truncated string and the empty one. Everything here is UTC by construction, so "
            "an offset is not converted, it is rejected; that is a design decision and this line "
            "is where it is written down. RANGE refuses hour 25, minute 60 and second 60 before "
            "any calendar arithmetic runs, so a leap second has no representation.")
         (p "The middle four are the ones that would otherwise be silent. "
            (code/inline "2025-02-30") ", " (code/inline "2025-04-31")
            " and " (code/inline "2100-02-29") " all pass the range test -- the month is in "
            "[1,12], the day in [1,31] -- and " (code/inline "days-from-civil")
            " will happily give each of them a day-count, one that belongs to March 2, May 1 and "
            "March 1. The parser catches them by mapping that day-count BACK with "
            (code/inline "civil-from-days") " and demanding the original triple: the calendar is "
            "a bijection, so a tuple the forward map never returns is not a date. Without that "
            "round trip every one of these four would parse to a wrong instant that no later "
            "assertion could distinguish from a right one -- and the century rule would be "
            "untested, 2100 being divisible by 100 and not by 400, hence not a leap year, "
            "unlike 2000 in the case above. The pre-epoch string is refused for a different "
            "reason: it is a perfectly good ISO instant with no numeral to become, because the "
            "numeral of " (code/inline "(aux kanren arith)") " is unsigned. A non-string "
            "argument is refused by the " (code/inline "string?") " guard, so a caller who "
            "passes an instant by mistake gets " (code/inline "#f")
            " rather than a type error -- which is why callers must check the result, and why "
            (code/inline "iso°") " turns exactly this " (code/inline "#f") " into "
            (code/inline "✗°") ".")))

  ((test/iso° _)
   (⊦= '("2025-10-09T08:53:20Z") (μkanren-run (s 2 #t) (iso° (build-num 1760000000) s)))
   (⊦= '("1970-01-01T00:00:00Z") (μkanren-run (s 2 #t) (iso° (build-num 0) s)))
   (⊦= '("2025-01-02T03:04:05Z") (μkanren-run (s 2 #t) (iso° (build-num 1735787045) s)))
   (⊦= (list (build-num 1760000000)) (μkanren-run (t 2 #t) (iso° t "2025-10-09T08:53:20Z")))
   (⊦= (list (build-num 0)) (μkanren-run (t 2 #t) (iso° t "1970-01-01T00:00:00Z")))
   (⊦= (list (build-num 1735787045)) (μkanren-run (t 2 #t) (iso° t "2025-01-02T03:04:05Z")))
   (⊦= '(yes) (μkanren-run (q 2 #t) (=° q 'yes) (iso° (build-num 1760000000) "2025-10-09T08:53:20Z")))
   (⊦= '() (μkanren-run (q 2 #t) (=° q 'yes) (iso° (build-num 1760000000) "2025-10-09T08:53:21Z")))
   (⊦= '() (μkanren-run (q 2 #t) (=° q 'yes) (iso° (build-num 1735787045) "2025-1-2T3:4:5Z")))
   (⊦= '(() () () () ())
       (map (lambda (s) (μkanren-run (t 1 #t) (iso° t s)))
            '("2025-02-30T00:00:00Z" "2025-13-01T00:00:00Z" "1969-12-31T23:59:59Z"
              "2025-10-09T08:53:20+01:00" "not a date at all!!")))
   `(doc (p "The relation, in the two modes a caller actually writes, and each of them asks for "
            "TWO answers so that the assertion also says the stream closes: an instant has one "
            "spelling and a spelling has one instant. Every query here is a millisecond or "
            "less, because neither direction does any relational arithmetic at all -- "
            (code/inline "iso°") " is a " (code/inline "project°") " and a "
            (code/inline "cond°") " over two string functions. Note the second group's "
            "expectations are written " (code/inline "(build-num 1760000000)")
            ", not thirty-one bits: the answer is a term, and "
            (code/inline "test/build-num") " in " (code/inline "microkanren-arith.scm")
            " has already pinned that the two constructions agree. The epoch answers "
            (code/inline "(())")
            " -- a one-answer stream whose single answer is the empty numeral.")
         (p "What the " (code/inline "project°") " costs the caller is a MODE OBLIGATION, and "
            "it is the reason these three groups are separate. The guard reads the "
            "substitution when the goal runs, so it is the conjunction ORDER that decides which "
            "branch is taken: a goal that grounds " (code/inline "t")
            " must be written before " (code/inline "iso°") ", not after it, or the relation "
            "falls through to the open branch and enumerates instead of computing. Being "
            "ground is tested by " (code/inline "numeral?") ", positively, so a partially "
            "instantiated instant counts as unknown and no bits already fixed are consulted. "
            "In exchange the caller gets something a pure relation could not give at all: "
            "formatting needs " (code/inline "/°") " on a whole unixtime, and the brief's "
            "measurement is that that division does not terminate.")
         (p "With both sides ground the goal is a TEST, and the last two lines of that group "
            "say what it tests. One second off is zero answers, which is the useful half. The "
            "other half is that " (code/inline "\"2025-1-2T3:4:5Z\"")
            " -- the same instant, spelled without the padding -- is ALSO zero answers, because "
            "the ground-" (code/inline "t") " branch wins the " (code/inline "cond°")
            " and unifies against the canonical string. Equality here is string equality on one "
            "chosen spelling, not equality of dates, and a caller comparing strings from "
            "elsewhere must normalise them first. The final line is the same refusal in "
            "relational clothing: a malformed string gives an empty stream -- the goal simply "
            "fails, it does not raise, and it does not answer with a nearby instant.")))

  ((test/iso°/round-trip _)
   (⊦= '("1970-01-01T00:00:00Z" "1970-01-01T00:00:01Z" "1970-01-05T00:00:00Z"
         "1970-01-01T23:59:59Z" "2000-02-29T00:00:00Z" "2025-01-02T03:04:05Z"
         "2025-10-09T08:53:20Z" "2038-01-19T03:14:07Z")
       (map (lambda (t) (car (μkanren-run (v 1 #t) (iso° (build-num t) v))))
            '(0 1 345600 86399 951782400 1735787045 1760000000 2147483647)))
   (⊦= '(0 1 345600 86399 951782400 1735787045 1760000000 2147483647)
       (map (lambda (t)
              (numeral->number
               (car (μkanren-run (u 1 #t) (iso° u (instant->iso t))))))
            '(0 1 345600 86399 951782400 1735787045 1760000000 2147483647)))
   (⊦= '("1970-01-01T00:00:00Z" "1970-01-02T00:00:00Z" "1970-01-01T00:00:01Z"
         "1970-01-03T00:00:00Z" "1970-01-02T00:00:01Z")
       (μkanren-run (s 5 #t) (fresh° (t) (iso° t s))))
   (⊦= '((0 "1970-01-01T00:00:00Z") (86400 "1970-01-02T00:00:00Z")
         (1 "1970-01-01T00:00:01Z") (172800 "1970-01-03T00:00:00Z"))
       (μkanren-run (q 4 #t)
         (fresh° (t s) (iso° t s)
                 (project° ((t* t)) (=° q `(,(numeral->number t*) ,s))))))
   `(doc (p "The same eight instants out through one branch of " (code/inline "iso°")
            " and back in through the other, at the numeral level rather than the integer "
            "level. " (code/inline "test/iso->instant") " already pinned that the two string "
            "functions invert each other; what this pins is that the RELATION wires them up in "
            "the right direction and builds a canonical term out of the result. A "
            (code/inline "cond°") " whose branches were swapped, or a parse branch that unified "
            (code/inline "str") " with its own input, or a numeral built by some path other "
            "than " (code/inline "number->numeral") ", would each leave the function-level "
            "assertions perfectly green and fail right here. It is eight queries a direction "
            "and still under a millisecond each.")
         (p "The last two lines are the open mode -- neither side ground -- and the reason to "
            "assert it at all is that the alternatives were a failure and a hang. Instead "
            (code/inline "date-time°") " generates an instant, the goal re-enters with "
            (code/inline "t") " now ground, and the guard's fast branch formats it: the stream "
            "is an enumeration of REAL instants, each one a string this file's parser would "
            "accept. The order is the µKanren interleaving and it is NOT chronological -- "
            "day-count and second-of-day interleave, so the answers arrive 0, 86400, 1, 172800, "
            "which is 1970-01-01, 01-02, then back to 00:00:01 on the 1st. The paired assertion "
            "is written with a " (code/inline "project°") " precisely so that the reader sees "
            "the numbers next to the strings and cannot mistake the sequence for a sorted one. "
            "This order is pinned as OBSERVED, not as promised: it is a fact about "
            (code/inline "or°") "'s interleaving, it is what a caller will actually see, and "
            "nobody should build a chronological listing on it -- " (code/inline "span°")
            " and " (code/inline "weekday-midnight°") " are the relations that bound a window.")))

  )

(unittest/✓ microkanren-date-suite)
