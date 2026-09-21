; The suite for (aux kanren arith): the arithmetic system of 'The Reasoned
; Schemer, Second Edition,' by Friedman, Byrd, Kiselyov and Hemann (MIT Press,
; 2018), chapters 7 and 8.
;
; The relations themselves live in ../aux.kanren.arith.scm, which carries the
; upstream copyright notice; this file only exercises them.  Upstream original:
; https://github.com/TheReasonedSchemer2ndEd/CodeFromTheReasonedSchemer2ndEd/blob/master/trs2-arith.scm

(import scheme (chicken base) srfi-1
  (aux base) (aux unittest) (aux kanren micro) (aux kanren arith))

(define-suite microkanren-arith-suite

  ((doc r)
   `((structure/section "Arithmetic as a relation")
     (p "Chapters 7 and 8 of "
        (cite/a "https://github.com/TheReasonedSchemer2ndEd/CodeFromTheReasonedSchemer2ndEd/blob/master/trs2-arith.scm"
                "The Reasoned Schemer, Second Edition")
        " build addition, subtraction, multiplication, division, exponentiation and the "
        "logarithm out of two truth tables and a great deal of unification. The definitions in "
        (code/inline "(aux kanren arith)") " are that development, translated line by line into the "
        (code/inline "(aux kanren micro)") " idiom and then exercised here, with four "
        "comparison relations added that the book has no use for. Nothing in the "
        "translation is clever: " (code/inline "defrel") " becomes "
        (code/inline "define-relation") ", " (code/inline "conde") " becomes "
        (code/inline "cond°") ", " (code/inline "fresh") " becomes " (code/inline "fresh°")
        ", " (code/inline "==") " becomes " (code/inline "=°") ", "
        (code/inline "(run n (q) g ...)") " becomes " (code/inline "(μkanren-run (q n #t) g ...)")
        ", and the book's trailing " (code/inline "o") " -- the mark that a name denotes a "
        "relation rather than a function -- becomes the trailing " (code/inline "°")
        " this repository uses for the same purpose. So " (code/inline "addero")
        " is " (code/inline "adder°") ", " (code/inline "*o") " is " (code/inline "*°")
        ", " (code/inline "logo") " is " (code/inline "log°") ". The two relations "
        (code/inline "null°") " and " (code/inline "cons°") " are not redefined here; they "
        "already live in " (code/inline "(aux kanren micro)") ".")
     (structure/section "The numeral")
     (p "A number is a list of the bits " (code/inline "0") " and " (code/inline "1")
        ", least significant first, with zero written as the empty list and with no trailing "
        (code/inline "0") ". Two is " (code/inline "(0 1)") ", five is " (code/inline "(1 0 1)")
        ", six is " (code/inline "(0 1 1)") " and nine is " (code/inline "(1 0 0 1)") ". "
        (code/inline "build-num") " is the only function in the module -- everything else is a "
        "relation -- and its " (code/inline "quotient") " recursion stops at "
        (code/inline "zero?") ", which is what guarantees the absence of a trailing zero. That "
        "canonicality is not cosmetic: it makes the encoding injective, so one number has one "
        "term, and a term that unifies with " (code/inline "(0 0 1)") " is four and cannot be "
        "anything else.")
     (p "Little-endian is what makes the whole system run in every direction. Addition "
        "consumes its arguments from the low bit up, so " (code/inline "adder°") " can peel one "
        "cell off each addend and recur without knowing how long either list is -- and the very "
        "same clause, read with the sum ground and an addend fresh, subtracts. There is no mode "
        "declaration anywhere in the module and no guard on which argument happens to be known. "
        (code/inline "minus°") " has no body beyond " (code/inline "(plus° m k n)") "; "
        "division by " (code/inline "*°") " is the multiplication goal with the product ground; "
        "factorisation is the same goal with both factors fresh; and "
        (code/inline "(exp° b 2 9)") " is an integer square root only because "
        (code/inline "exp°") " is " (code/inline "log°") " with the remainder pinned to "
        (code/inline "'()") ". Termination in those backward directions is bought, explicitly, "
        "by the length guards -- " (code/inline "pos°") ", " (code/inline ">1°") ", "
        (code/inline "<l°") ", " (code/inline "bound-*°") " -- and the cases below are written "
        "to pin exhaustion, not just correctness: a query that asks for more answers than exist "
        "asserts that the stream closes.")
     (structure/section "Goal construction here is eager")
     (p "This is the one fact a reader of this repository needs before changing anything in "
        (code/inline "(aux kanren arith)")
        ", and it is the single respect in which the translation could not be literal. In "
        (code/inline "(aux kanren micro)") ", " (code/inline "and°") ", " (code/inline "or°")
        " and " (code/inline "cond°") " are macros that expand into calls to the procedures "
        (code/inline "andª") " and " (code/inline "orª") " -- so every goal sub-expression of "
        "every clause is evaluated while the goal tree is being built, long before any "
        "substitution exists to search over. A recursive call that is the sole goal of its "
        "clause is fine, and so is one inside a " (code/inline "fresh°") " body, because that "
        "body is a λ and a λ is a natural delay. A recursive call sitting next to other goals "
        "in a conjunction is not: building the tree for " (code/inline "adder°") " builds the "
        "tree for " (code/inline "adder°") " builds the tree for " (code/inline "adder°")
        ", and the call never returns. Not at load time, though: " (code/inline "define-relation")
        " expands to a plain " (code/inline "define") ", so every definition in the module installs "
        "and every query that never reaches " (code/inline "adder°") " -- "
        (code/inline "append°") ", " (code/inline "bit-xor°") ", " (code/inline "<l°")
        " -- still runs and answers. The divergence is inside the first query that does reach it, "
        "directly or through " (code/inline "plus°") ", " (code/inline "*°") " or "
        (code/inline "odd-*°") ", while that query's goal tree is being built. The symptom is not "
        "a wrong answer or a long search: running the whole file prints nothing at all, because "
        "the suite reports only once every case has finished. So the question to ask is which "
        "relation the first hanging query touches, not what the load order was.")
     (p "The cure is an eta-expansion around a " (code/inline "δ") ", which is exactly what the "
        "book's " (code/inline "defrel") " does for free and what the module has to write by hand:")
     (code/lang "scheme" "(define-syntax-rule (δ° g) (μ s (δ (g s))))\n\n; hangs while the tree is built -- the call is a clause-level conjunct:\n((=° 1 b) (=° '() m) (adder° 0 n '(1) r))\n\n; terminates -- the call is now a λ awaiting a substitution:\n((=° 1 b) (=° '() m) (δ° (adder° 0 n '(1) r)))")
     (p "Three calls need it, all of them self-calls of " (code/inline "adder°") ": the two that "
        "discharge a carry against an empty addend and the one that commutes "
        (code/inline "'(1)") " into the first position. What decides is the callee's body, not "
        "the call site -- the two calls to " (code/inline "gen-adder°") " are clause-level "
        "conjuncts too, but " (code/inline "gen-adder°") " is a single " (code/inline "fresh°")
        ", that is " (code/inline "(freshª (λ ...))") ", so the call hands back a goal without "
        "evaluating its body. Everywhere else the recursion is already under a "
        (code/inline "fresh°") " -- " (code/inline "*°") ", " (code/inline "split°") ", "
        (code/inline "=l°") ", " (code/inline "exp2°") " and the rest are unchanged from the "
        "book. The wrapper is applied where eagerness would bite and nowhere else, so its "
        "presence in a clause is a statement about that clause.")
     (structure/section "Reading the expected values")
     (p "Every expectation below was read off a real run. The third argument of "
        (code/inline "μkanren-run") " is the grounding flag, and it is " (code/inline "#t")
        " throughout: a variable the search never had to instantiate is reified as a Greek "
        "letter in order of appearance, so " (code/inline "(α 1)") " is a single answer "
        "standing for both 2 and 3, and " (code/inline "(α . β)") " is " (code/inline "pos°")
        " and nothing more. Those partially ground answers are the interesting ones -- they are "
        "the relation declining to decide a bit it was never asked about, and an expectation "
        "that spelled a concrete bit there would be asserting something the code does not "
        "claim. Answer ORDER is asserted too: " (code/inline "cond°") " disjoins by "
        "INTERLEAVING its clauses, and for a truth table -- where every clause yields exactly "
        "one answer -- the interleaving coincides with textual order, so that order is "
        "observable ten definitions later in the order " (code/inline "plus°") " hands back "
        "its solutions. Where a clause yields more than one answer the two part company, and "
        "the expectations below are read off the interleaving.")
     (p "The definitions under test are a module, " (code/inline "(aux kanren arith)")
        ", rather than a preamble to this suite: the arithmetic is worth importing on its own, "
        "and keeping it out here means a case cannot quietly depend on a definition it also "
        "supplies. The "
        (code/inline "#;") "-commented alternatives -- the frame 7:12 half-adder, the frame 7:15 "
        "full-adder, and the two flawed " (code/inline "/°") "s of frames 8:54 and 8:64 -- are "
        "kept because several cases below exist precisely to say what the surviving definition "
        "does that the discarded one does not.")))

  ; -- helpers: the Chapter 2 and 4 plumbing ---------------------------------------------

  ((test/car°+cdr° _)
   (⊦= '(()) (μkanren-run (q 3 #t) (null° q)))
   (⊦= '((1 (2 3)))
       (μkanren-run (q 3 #t) (fresh° (a d) (cons° a d '(1 2 3)) (=° q (list a d)))))
   (⊦= '(a) (μkanren-run (q 3 #t) (car° '(a b c) q)))
   (⊦= '((b c)) (μkanren-run (q 3 #t) (cdr° '(a b c) q)))
   (⊦= '((1 2 3))
       (μkanren-run (q 3 #t)
         (fresh° (a d) (car° '(1 2 3) a) (cdr° '(1 2 3) d) (=° q (cons a d)))))
   (⊦= '((x . α)) (μkanren-run (q 3 #t) (car° q 'x)))
   (⊦= '((α y z)) (μkanren-run (q 3 #t) (cdr° q '(y z))))
   (⊦= '() (μkanren-run (q 3 #t) (car° '() q)))
   (⊦= '() (μkanren-run (q 3 #t) (cdr° '() q)))
   `(doc
     (p (code/inline "car°") " and " (code/inline "cdr°") " are not accessors: each is the one "
        "constraint " (code/inline "(=° (cons a d) p)") " projected onto a different variable, "
        "which is why the same relation runs backwards and " (code/inline "(car° q 'x)") " answers "
        "with the invented pair " (code/inline "(x . α)") " -- a fresh tail, reified as a Greek "
        "letter, rather than a failure for lack of input.")
     (p "Both fail on " (code/inline "'()") ", since no substitution unifies the empty list with a "
        "cons cell. That is a contract and not an accident: it is why " (code/inline "append°")
        " has to test " (code/inline "null°") " in its first clause instead of letting the "
        (code/inline "cdr°") " chain run out on its own, and it is the same reason "
        (code/inline "pos°") " and " (code/inline ">1°") " downstream can be written purely as "
        "shape unifications.")
     (p (code/inline "null°") " and " (code/inline "cons°") " come from "
        (code/inline "(aux kanren micro)") ", not from " (code/inline "(aux kanren arith)")
        ", so the first two assertions pin "
        "the library shapes everything else is built on. If " (code/inline "cons°")
        " ever stopped being " (code/inline "(=° c (cons a d))") ", every "
        (code/inline "(=° `(,a . ,d) n)") " in chapters 7 and 8 would still compile and would "
        "quietly mean something else.")))

  ((test/append° _)
   (⊦= '((a b c d)) (μkanren-run (q 3 #t) (append° '(a b) '(c d) q)))
   (⊦= '((c d)) (μkanren-run (q 3 #t) (append° '(a b) q '(a b c d))))
   (⊦= '((() (a b c)) ((a) (b c)) ((a b) (c)) ((a b c) ()))
       (μkanren-run (q 9 #t) (fresh° (x y) (append° x y '(a b c)) (=° q (list x y)))))
   (⊦= '((() (c)) ((α) (α c)) ((α β) (α β c)) ((α β γ) (α β γ c)))
       (μkanren-run (q 4 #t) (fresh° (x y) (append° x '(c) y) (=° q (list x y)))))
   (⊦= '() (μkanren-run (q 3 #t)
             (fresh° (x y) (append° x y '(a b c)) (=° x '(a b c d)) (=° q (list x y)))))
   `(doc
     (p "Three directions of one relation. Forward it concatenates; with the second argument "
        "fresh it subtracts a known prefix; with both inputs fresh and the output ground it "
        "enumerates the four ways to cut " (code/inline "(a b c)") " in two, both empty-side "
        "boundaries included. That third query asks for nine answers and gets four: the "
        "assertion is that the search is exhaustive AND terminates, which a spot check for one "
        "split would not catch.")
     (p "The fourth query runs the generator direction, output fresh, and it does not terminate "
        "on its own -- it is the count that stops it. Each answer is one cons longer and the "
        "still-fresh elements appear as the same Greek letters in " (code/inline "x") " and "
        (code/inline "y") ", showing they are shared cells and not independent unknowns. That is "
        "exactly the shape " (code/inline "exp2°") " leans on when it calls "
        (code/inline "(append° b `(1 . ,b) b2)") " to double the width of a ground "
        (code/inline "b") ".")
     (p "The last case demands a prefix longer than the whole list and gets no answers. A "
        "function would have to err or truncate; a relation just has an empty answer stream, and "
        "pinning that here is what lets the arithmetic built on it treat failure as ordinary arithmetic "
        "information.")))

  ((test/bit-xor°+bit-and° _)
   (⊦= '((0 0 0) (0 1 1) (1 0 1) (1 1 0))
       (μkanren-run (q 9 #t) (fresh° (x y r) (bit-xor° x y r) (=° q (list x y r)))))
   (⊦= '((0 0 0) (1 0 0) (0 1 0) (1 1 1))
       (μkanren-run (q 9 #t) (fresh° (x y r) (bit-and° x y r) (=° q (list x y r)))))
   (⊦= '((0 1) (1 0)) (μkanren-run (q 9 #t) (fresh° (x y) (bit-xor° x y 1) (=° q (list x y)))))
   (⊦= '((1 1)) (μkanren-run (q 9 #t) (fresh° (x y) (bit-and° x y 1) (=° q (list x y)))))
   (⊦= '() (μkanren-run (q 9 #t) (fresh° (y r) (bit-xor° 2 y r) (=° q (list y r)))))
   `(doc
     (p "Both truth tables in full, all four rows each, asked with a count of nine so that a "
        "fifth answer or a missing row would fail the case. These two relations are the entire "
        "arithmetic content of chapters 7 and 8 -- everything preceding them in the module is plumbing -- so a "
        "single transposed row would come back as a wrong sum ten definitions later, with "
        "nothing local to blame.")
     (p "The answer ORDER is asserted too, and the two tables do not share it: upstream writes "
        (code/inline "bit-and°") " as 00, 10, 01, 11 while " (code/inline "bit-xor°")
        " is 00, 01, 10, 11. Each clause of these tables yields exactly one answer, so "
        (code/inline "cond°") "'s interleaving degenerates to textual order here; that order "
        "is observable and it feeds into the order in which " (code/inline "adder°") " and "
        (code/inline "*°") " enumerate their solutions, which past the leaves is an "
        "interleaving and no longer clause order.")
     (p "Run backwards from " (code/inline "r") ", " (code/inline "bit-xor°") " has two "
        "preimages and " (code/inline "bit-and°") " has one; that asymmetry is what makes the "
        "carry bit the deterministic half of " (code/inline "half-adder°") ". The final case "
        "feeds a non-bit " (code/inline "2") " and gets nothing: the tables are closed, so these "
        "relations reject as well as generate, and a malformed bit list dies at the leaf instead "
        "of producing a plausible-looking number.")))

  ((test/half-adder° _)
   (⊦= '((0 0 0 0) (0 1 1 0) (1 0 1 0) (1 1 0 1))
       (μkanren-run (q 9 #t) (fresh° (x y r c) (half-adder° x y r c) (=° q (list x y r c)))))
   (⊦= '((1 1)) (μkanren-run (q 9 #t) (fresh° (x y) (half-adder° x y 0 1) (=° q (list x y)))))
   (⊦= '((0 1) (1 0)) (μkanren-run (q 9 #t) (fresh° (x y) (half-adder° x y 1 0) (=° q (list x y)))))
   (⊦= '() (μkanren-run (q 9 #t) (fresh° (x y) (half-adder° x y 1 1) (=° q (list x y)))))
   `(doc
     (p "The whole table, derived rather than written: " (code/inline "half-adder°")
        " is just " (code/inline "bit-xor°") " conjoined with " (code/inline "bit-and°") ", and "
        "the enumeration order it produces is " (code/inline "bit-xor°") "'s (00, 01, 10, 11), "
        "the first conjunct driving the search while the second filters. The "
        (code/inline "#;") "-commented alternative from frame 7:12 spells the same four rows out "
        "as a table; keeping this case honest is what would expose a divergence between the two.")
     (p "The last three queries run it backwards, from the outputs to the inputs, which only "
        "works because the sum and carry are related by unification and not computed: given "
        (code/inline "r") "=0 and " (code/inline "c") "=1 there is exactly one input pair that "
        "generates a carry, given " (code/inline "r") "=1 and " (code/inline "c") "=0 there are "
        "two, and " (code/inline "r") "=1 with " (code/inline "c") "=1 has none at all.")
     (p "That empty answer is the point of the case. It encodes the invariant "
        (code/inline "x + y = r + 2c") ", whose maximum is 2, so a sum bit and a carry bit can "
        "never both be set. A table with one wrong carry would still answer every forward query "
        "plausibly and would only show up as an impossible pair becoming possible here.")))

  ((test/full-adder° _)
   (⊦= '((0 0 0 0 0) (1 0 0 1 0) (0 1 0 1 0) (1 1 0 0 1)
         (0 0 1 1 0) (1 0 1 0 1) (0 1 1 0 1) (1 1 1 1 1))
       (μkanren-run (q 17 #t)
         (fresh° (b x y r c) (full-adder° b x y r c) (=° q (list b x y r c)))))
   (⊦= '((1 0 0) (0 1 0) (0 0 1))
       (μkanren-run (q 9 #t) (fresh° (b x y) (full-adder° b x y 1 0) (=° q (list b x y)))))
   (⊦= '((1 1 0) (1 0 1) (0 1 1))
       (μkanren-run (q 9 #t) (fresh° (b x y) (full-adder° b x y 0 1) (=° q (list b x y)))))
   (⊦= '((1 1 1))
       (μkanren-run (q 9 #t) (fresh° (b x y) (full-adder° b x y 1 1) (=° q (list b x y)))))
   (⊦= '((0 0 0 0) (1 0 1 0) (0 1 1 0) (1 1 0 1))
       (μkanren-run (q 9 #t) (fresh° (x y r c) (full-adder° 0 x y r c) (=° q (list x y r c)))))
   `(doc
     (p "All eight rows of the table version -- the one the module keeps live because the "
        (code/inline "half-adder°") "-based definition of frame 7:15, kept " (code/inline "#;")
        "-commented beside it, costs three sub-goals per bit. This is the leaf of every addition, "
        "subtraction, multiplication, division and logarithm in the rest of the module, so it is "
        "the one table worth asserting exhaustively, count of seventeen and all.")
     (p "The next three queries invert it on the output pair, and together they partition the "
        "eight rows by how many of " (code/inline "b") ", " (code/inline "x") ", "
        (code/inline "y") " are set: three ways to reach " (code/inline "(r c)") "=(1,0), three "
        "to reach (0,1), exactly one to reach (1,1), and the unlisted (0,0) is the all-zero row. "
        "That is " (code/inline "b + x + y = r + 2c") " read off the answer counts, and it is "
        "the invariant " (code/inline "gen-adder°") " assumes when it threads "
        (code/inline "e") " into the next bit position.")
     (p "The last case pins " (code/inline "b") "=0 to the half-adder table: the same four rows, "
        "as they must be, but in a different ORDER, because the clause order of this table is "
        (code/inline "bit-and°") "-shaped (00, 10, 01, 11) while " (code/inline "half-adder°")
        " inherits " (code/inline "bit-xor°") "'s. The two relations are extensionally equal and "
        "operationally distinct, and it is the operational half that decides the order in which "
        (code/inline "plus°") " hands back its solutions.")))

  ; -- build-num: the numeral and its two structural guards -------------------------------

  ((test/build-num _)
   (⊦= '() (build-num 0))
   (⊦= '(1) (build-num 1))
   (⊦= '(0 1) (build-num 2))
   (⊦= '(1 1) (build-num 3))
   (⊦= '(0 0 1) (build-num 4))
   (⊦= '(1 0 1) (build-num 5))
   (⊦= '(0 1 1) (build-num 6))
   (⊦= '(1 1 1) (build-num 7))
   (⊦= '(0 0 0 1) (build-num 8))
   (⊦= '(1 0 0 0 1) (build-num 17))
   (⊦= '(0 1 0 1 0 1) (build-num 42))
   (⊦= '(1 1 1 1 1 1 1 1) (build-num 255))
   (⊦= '(0 0 0 0 0 0 0 0 1) (build-num 256))
   (⊦= '(1 1 1 1 1 1 1 1) (map (lambda (n) (last (build-num n))) (iota 8 1)))
   (⊦= (build-num 8) (car (μkanren-run (r 1 #t) (plus° (build-num 3) (build-num 5) r))))
   (⊦= (build-num 15) (car (μkanren-run (r 1 #t) (*° (build-num 3) (build-num 5) r))))
   `(doc (p "The numeral, fixed once for the whole suite. Bits are little-endian -- "
            (code/inline "(build-num 2)") " is " (code/inline "(0 1)") ", not "
            (code/inline "(1 0)") " -- and zero is the empty list, not "
            (code/inline "(0)") ". The " (code/inline "quotient") " recursion stops at "
            (code/inline "zero?") ", so a numeral never carries a trailing " (code/inline "0")
            ": that is the canonicality the " (code/inline "last") " assertion pins, and it is "
            "what makes the encoding injective. One number, one term.")
         (p "The last two lines are the bridge, and they are the reason every other case in "
            "this file may write its expectation as a bare bit list. "
            (code/inline "plus°") " and " (code/inline "*°") " reconstruct their answers bit by "
            "bit through " (code/inline "gen-adder°") ", never by calling "
            (code/inline "build-num") "; the assertions say the two constructions agree, "
            "term for term. Were a relation ever to answer " (code/inline "(0 0 0 1 0)")
            " for eight -- arithmetically the same number, a different term -- it would still "
            "unify with nothing here and every literal below would be testing a different "
            "encoding than the one the relations speak.")))

  ((test/pos° _)
   (⊦= '() (μkanren-run (r 2 #t) (pos° '()) (=° r 'yes)))
   (⊦= '() (μkanren-run (r 2 #t) (pos° (build-num 0)) (=° r 'yes)))
   (⊦= '(yes) (μkanren-run (r 2 #t) (pos° (build-num 1)) (=° r 'yes)))
   (⊦= '(yes) (μkanren-run (r 2 #t) (pos° (build-num 7)) (=° r 'yes)))
   (⊦= '(yes) (μkanren-run (r 2 #t) (pos° '(0)) (=° r 'yes)))
   (⊦= '(yes) (μkanren-run (r 2 #t) (pos° '(1 . 2)) (=° r 'yes)))
   (⊦= '() (μkanren-run (r 2 #t) (pos° 5) (=° r 'yes)))
   `(doc (p (code/inline "pos°") " inspects no bits. Its body is the single unification "
            (code/inline "(=° `(,a . ,d) n)") ", so what it actually decides is \"n has a car\", "
            "and it decides it in constant time with no choice point. That it also means "
            (code/inline "n > 0") " is a property of the encoding, not of the relation: "
            (code/inline "build-num") " sends zero, and only zero, to a term without a car.")
         (p "The last three lines mark exactly where the two readings come apart. "
            (code/inline "(0)") " is a non-canonical zero -- "
            (code/inline "build-num") " never emits it -- and " (code/inline "pos°")
            " says yes; the improper pair " (code/inline "(1 . 2)")
            " is not a numeral at all and it says yes too; only an atom, with no car to take, "
            "fails. So " (code/inline "pos°") " is a structural guard and nothing more, and the "
            "obligation to keep non-canonical junk out of it belongs to its callers -- "
            (code/inline "adder°") ", " (code/inline "*°") ", " (code/inline "split°")
            " -- which only ever hand it a cdr of a numeral they built themselves.")))

  ((test/>1° _)
   (⊦= '() (μkanren-run (r 2 #t) (>1° '()) (=° r 'yes)))
   (⊦= '() (μkanren-run (r 2 #t) (>1° (build-num 1)) (=° r 'yes)))
   (⊦= '(yes) (μkanren-run (r 2 #t) (>1° (build-num 2)) (=° r 'yes)))
   (⊦= '(yes) (μkanren-run (r 2 #t) (>1° (build-num 3)) (=° r 'yes)))
   (⊦= '() (μkanren-run (r 2 #t) (>1° '(1 . 2)) (=° r 'yes)))
   (⊦= '(yes) (μkanren-run (r 2 #t) (>1° '(a b c)) (=° r 'yes)))
   `(doc (p "Same trick one bit further along: " (code/inline "(=° `(,a ,ad . ,dd) n)")
            " demands two cars, so the numerals it accepts are exactly those of length two or "
            "more, i.e. " (code/inline "n > 1") ". The two rejections are the ones that matter "
            "in " (code/inline "adder°") " and " (code/inline "*°") ": zero and one, the cases "
            "where the general bit-by-bit clauses would recur on a numeral with no bits left.")
         (p "The improper pair separates the two guards. " (code/inline "(1 . 2)")
            " satisfies " (code/inline "pos°") " but not " (code/inline ">1°")
            ", because the second pattern also forces the cdr to be a pair. "
            (code/inline "(a b c)") " passes, which says again that these relations count "
            "spine cells and never look at whether the cars are bits.")))

  ((test/pos°+>1°/fresh _)
   (⊦= '((α . β)) (μkanren-run (n 5 #t) (pos° n)))
   (⊦= '((α β . γ)) (μkanren-run (n 5 #t) (>1° n)))
   (⊦= '((α β . γ)) (μkanren-run (n 5 #t) (pos° n) (>1° n)))
   (⊦= '((λ (α β) (cons α β))) (μkanren-run (n 1 #f) (pos° n)))
   `(doc (p "Run backwards, neither relation enumerates. Asked for five answers each returns "
            "one, because each is a single unification: " (code/inline "pos°")
            " instantiates n to a pair with both fields still fresh, " (code/inline ">1°")
            " to a pair whose cdr is a pair. Conjoining the two does not add an answer, it "
            "refines the same term. This is the property the callers depend on: a guard sitting "
            "in front of a recursive clause of " (code/inline "adder°") " or " (code/inline "*°")
            " must not open a choice point, and a " (code/inline "pos°")
            " that generated all positive numerals instead of constraining one would turn every "
            "clause that guards on it into an infinite stream.")
         (p "The last line names the reifier. " (code/inline "μkanren-run") "'s third argument "
            "is the grounded flag: " (code/inline "#t") " prints a still-fresh variable as "
            (code/inline "α") ", " (code/inline "β") ", " (code/inline "γ")
            " in order of appearance, while " (code/inline "#f")
            " hands back the raw lambda form. Every partially-ground expectation in this file -- "
            "here, and the " (code/inline "α") " that " (code/inline "<°")
            " and " (code/inline "log°") " leave behind -- is written in the "
            (code/inline "#t") " language.")))

  ((test/pos°/guard-in-adder° _)
   (⊦= '((() ())) (μkanren-run (r 5 #t) (fresh° (x y) (plus° x y (build-num 0)) (=° r (list x y)))))
   (⊦= '(()) (μkanren-run (r 5 #t) (plus° '() '() r)))
   (⊦= (list (build-num 5)) (μkanren-run (r 5 #t) (plus° '() (build-num 5) r)))
   (⊦= (list (build-num 5)) (μkanren-run (r 5 #t) (plus° (build-num 5) '() r)))
   `(doc (p "Why " (code/inline "pos°") " is in " (code/inline "adder°") " at all. The second "
            "clause, " (code/inline "((=° 0 b) (=° '() n) (=° m r) (pos° m))")
            ", does not need its guard to be true -- 0 + m = m holds for m = 0 as well -- it "
            "needs it to stay disjoint from the first clause, which already answers when m is "
            (code/inline "'()") ". Measured on a copy of " (code/inline "adder°")
            " with that one conjunct deleted: " (code/inline "x + y = 0") " answers "
            (code/inline "((() ()) (() ()))") " and " (code/inline "() + () = r") " answers "
            (code/inline "(() ())") ", the same derivation twice. The overlap is confined to "
            "zero -- " (code/inline "x + y = 6") " returns the same seven answers with or "
            "without the guard -- but a duplicate is still a lie about the search: the count "
            "argument of " (code/inline "μkanren-run") " asks for n answers, not n derivations, "
            "and a relation used as a filter inside " (code/inline "*°") " and "
            (code/inline "/°") " has to be able to say \"once\".")
         (p "The two identity directions in the last lines come out of different clauses and "
            "are worth reading together: " (code/inline "5 + 0") " is decided by clause one "
            "with pure unification and no guard at all, " (code/inline "0 + 5")
            " by clause two, which is the guarded one. Asymmetric code, symmetric answers.")))

  ; -- adder: Chapter 7 addition and subtraction ------------------------------------------

  ((test/plus° _)
   (⊦= `(,(build-num 8))  (μkanren-run (r 2 #t) (plus° (build-num 3) (build-num 5) r)))
   (⊦= `(,(build-num 7))  (μkanren-run (r 2 #t) (plus° '() (build-num 7) r)))
   (⊦= `(,(build-num 7))  (μkanren-run (r 2 #t) (plus° (build-num 7) '() r)))
   (⊦= '((0 1))           (μkanren-run (r 2 #t) (plus° '(1) '(1) r)))            ; 1+1 = 2
   (⊦= `(,(build-num 16)) (μkanren-run (r 2 #t) (plus° (build-num 15) (build-num 1) r)))
   (⊦= `(,(build-num 17)) (μkanren-run (r 2 #t) (plus° (build-num 12) (build-num 5) r)))
   (⊦= `(,(build-num 7))  (μkanren-run (r 2 #t) (fresh° (x) (plus° x (build-num 5) (build-num 12)) (=° r x))))
   `(doc (p (code/inline "plus°") " is " (code/inline "adder°") " with the carry nailed to "
            (code/inline "0") ", so these are the cases where the relation happens to behave "
            "like a function.  Every query asks for two answers and is handed one: the eight "
            "clauses of " (code/inline "adder°") " are mutually exclusive by construction, and "
            "the " (code/inline "pos°") "/" (code/inline ">1°") " guards are what stops 7+0 "
            "from being proved twice, once by the empty-addend clause and once through "
            (code/inline "gen-adder°") ".  A second answer here would mean a guard was dropped, "
            "not that arithmetic broke.  15+1 is the carry-propagation case: every bit of the "
            "first addend flips and the numeral grows a digit, the only forward query that "
            "drives the full-adder chain from end to end.  The last line runs the very same "
            "goal with the first addend fresh -- nothing in " (code/inline "plus°")
            " distinguishes input from output, so subtraction is already available before "
            (code/inline "minus°") " is defined.")))

  ((test/adder° _)
   (⊦= `(,(build-num 7)) (μkanren-run (r 2 #t) (adder° 0 (build-num 3) (build-num 4) r)))
   (⊦= `(,(build-num 8)) (μkanren-run (r 2 #t) (adder° 1 (build-num 3) (build-num 4) r)))
   (⊦= '(())             (μkanren-run (r 2 #t) (adder° 0 '() '() r)))
   (⊦= '((1))            (μkanren-run (r 2 #t) (adder° 1 '() '() r)))            ; 0+0+1 = 1
   (⊦= `(,(build-num 8)) (μkanren-run (r 2 #t) (adder° 1 (build-num 7) '() r)))
   (⊦= `(,(build-num 8)) (μkanren-run (r 2 #t) (adder° 1 '() (build-num 7) r)))
   (⊦= '(1)              (μkanren-run (b 3 #t) (adder° b (build-num 3) (build-num 4) (build-num 8))))
   (⊦= '(0)              (μkanren-run (b 3 #t) (adder° b (build-num 3) (build-num 4) (build-num 7))))
   `(doc (p "The carry " (code/inline "b") " is a bit, not a numeral: "
            (code/inline "(adder° 1 n m r)") " is n+m+1, and the two clauses that consume a set "
            "carry against an empty addend discharge it by rewriting it into the literal "
            (code/inline "'(1)") " and recurring with " (code/inline "b") " back at "
            (code/inline "0") " -- which is why 7+0 with a carry is answered by the same code "
            "path as 7+1, and why " (code/inline "(adder° 1 '() '() r)") " bottoms out at "
            (code/inline "'(1)") ".  Those two self-calls, together with "
            (code/inline "(adder° b '(1) n r)") ", are exactly the calls this file wraps in "
            (code/inline "δ°") ": they are clause-level conjuncts of a " (code/inline "cond°")
            ", and goal construction here is eager, so without the wrapper the goal tree for "
            (code/inline "adder°") " never finishes being built -- not at load time, but inside "
            "the first query that calls it.  Deleting a single wrapper leaves every definition "
            "and every adder-free case working, and still yields no output whatsoever from a run "
            "of this file, not a wrong answer.  The last two lines ground the sum and leave the "
            "carry fresh: the "
            "bit is recovered, uniquely, from 3+4 against 8 rather than 7.")))

  ((test/gen-adder° _)
   (⊦= `(,(build-num 11)) (μkanren-run (r 2 #t) (gen-adder° 0 (build-num 6) (build-num 5) r)))
   (⊦= `(,(build-num 12)) (μkanren-run (r 2 #t) (gen-adder° 1 (build-num 6) (build-num 5) r)))
   (⊦= `(,(build-num 4))  (μkanren-run (r 2 #t) (gen-adder° 0 (build-num 1) (build-num 3) r)))
   (⊦= '()                (μkanren-run (r 1 #t) (gen-adder° 0 (build-num 3) (build-num 1) r)))
   (⊦= '()                (μkanren-run (r 1 #t) (gen-adder° 0 (build-num 1) (build-num 1) r)))
   (⊦= '()                (μkanren-run (r 1 #t) (gen-adder° 0 '() (build-num 5) r)))
   (⊦= `(,(build-num 4))  (μkanren-run (r 2 #t) (adder° 0 (build-num 3) (build-num 1) r)))
   `(doc (p (code/inline "gen-adder°") " is deliberately not symmetric.  Its body insists on "
            (code/inline "(=° `(,d . ,y) m)") " with " (code/inline "(pos° y)") " and on "
            (code/inline "(=° `(,c . ,z) r)") " with " (code/inline "(pos° z)") ", so the "
            "second addend and the sum must both be at least two bits wide: it adds 1+3 and "
            "refuses 3+1, 1+1 and 0+5.  Those refusals are a contract with its only caller, "
            "not a defect -- " (code/inline "adder°") " reaches it solely behind "
            (code/inline "(=° '(1) n)") " or " (code/inline "(>1° n)") ", and hands the shapes "
            "it drops to sibling clauses, notably the one that commutes "
            (code/inline "(=° '(1) m)") " into " (code/inline "(adder° b '(1) n r)") ", which "
            "is why 3+1 still answers 4 on the line below.  Calling the relation directly is "
            "the only way those guards are observable at all; every other query in this file "
            "meets " (code/inline "gen-adder°") " already filtered.")))

  ((test/minus° _)
   (⊦= `(,(build-num 7)) (μkanren-run (r 2 #t) (minus° (build-num 12) (build-num 5) r)))
   (⊦= '(())             (μkanren-run (r 2 #t) (minus° (build-num 5) (build-num 5) r)))
   (⊦= `(,(build-num 7)) (μkanren-run (r 2 #t) (minus° (build-num 7) '() r)))
   (⊦= '()               (μkanren-run (r 1 #t) (minus° (build-num 3) (build-num 5) r)))
   (⊦= '()               (μkanren-run (r 1 #t) (minus° '() (build-num 1) r)))
   (⊦= `(,(build-num 5)) (μkanren-run (r 3 #t) (fresh° (x) (minus° (build-num 12) x (build-num 7)) (=° r x))))
   `(doc (p (code/inline "minus°") " has no arithmetic of its own: its whole body is "
            (code/inline "(plus° m k n)") ", subtraction being addition read in another "
            "direction, and the final line reads it in a third one by leaving the subtrahend "
            "fresh.  The two empty answers are the point of the case.  This encoding has no "
            "negative numeral, so 3-5 is neither an error nor a distinguished value: it is the "
            "plain absence of a k with 5+k=3.  Getting " (code/inline "'()") " back means the "
            "search was exhausted, not abandoned -- it terminates only because the ground "
            "difference bounds " (code/inline "adder°") "'s recursion -- so asking for one "
            "answer and being handed none asserts more than any of the positive lines above.")))

  ((test/plus°/pairs _)
   (⊦= '(((0 0 1) ()) (() (0 0 1)) ((1) (1 1)) ((1 1) (1)) ((0 1) (0 1)))
       (μkanren-run (r 8 #t) (fresh° (x y) (plus° x y (build-num 4)) (=° r (list x y)))))
   (⊦= '(((0 1 1) ()) (() (0 1 1)) ((1) (1 0 1)))
       (μkanren-run (r 3 #t) (fresh° (x y) (plus° x y (build-num 6)) (=° r (list x y)))))
   `(doc (p "The fully relational direction, with both addends fresh, and the only case here "
            "that pins a complete answer set: eight answers are requested and five arrive, so "
            "the stream was exhausted.  4 has exactly those five ordered decompositions -- "
            "zero on either side included -- and no pair is enumerated twice.  Completeness "
            "and termination are separate properties and a query that merely checks the first "
            "few answers can see neither; widen one guard in " (code/inline "adder°")
            " and this is the assertion that notices.  The order is the interleaving of "
            (code/inline "cond°") "'s disjunction rather than magnitude, which is why 4+0 and "
            "0+4 precede 1+3; the sum-to-6 line pins that same sequence on its first three "
            "answers without paying for the full enumeration.")))

  ; -- mul: Chapter 8 multiplication ------------------------------------------------------

  ((test/*° _)
   (⊦= (list (build-num 0))  (μkanren-run (r 2 #t) (*° (build-num 0) (build-num 5) r)))
   (⊦= (list (build-num 0))  (μkanren-run (r 2 #t) (*° (build-num 5) (build-num 0) r)))
   (⊦= (list (build-num 0))  (μkanren-run (r 2 #t) (*° (build-num 0) (build-num 0) r)))
   (⊦= (list (build-num 5))  (μkanren-run (r 2 #t) (*° (build-num 1) (build-num 5) r)))
   (⊦= (list (build-num 5))  (μkanren-run (r 2 #t) (*° (build-num 5) (build-num 1) r)))
   (⊦= (list (build-num 6))  (μkanren-run (r 2 #t) (*° (build-num 2) (build-num 3) r)))
   (⊦= (list (build-num 15)) (μkanren-run (r 2 #t) (*° (build-num 3) (build-num 5) r)))
   (⊦= (list (build-num 42)) (μkanren-run (r 2 #t) (*° (build-num 6) (build-num 7) r)))
   (⊦= (list (build-num 49)) (μkanren-run (r 2 #t) (*° (build-num 7) (build-num 7) r)))
   (⊦= '(α)                  (μkanren-run (m 3 #t) (*° '() m '())))
   (⊦= '(() (α . β))         (μkanren-run (n 3 #t) (*° n '() '())))
   `(doc
     (p "Nine products, each asked for with a count of two so that the assertion also pins "
        "the number of answers: " (code/inline "*°") " must succeed exactly once on ground "
        "factors and then exhaust its stream, not leave a redundant branch alive. The two "
        "operands are not symmetric in the code even though the answers are: with "
        (code/inline "n") " odd and " (code/inline "m") " even the sixth clause recurs as "
        (code/inline "(*° m n p)") ", swapping the pair so the even operand is the one that "
        "gets halved by clause five.  Three of the nine rows reach the seventh clause, the only "
        "one that hands the work to " (code/inline "odd-*°") ": " (code/inline "3*5")
        " enters it once and is the smallest odd-times-odd product, " (code/inline "6*7")
        " once after clause five has halved the 6 into " (code/inline "(*° 3 7 z)") ", and "
        (code/inline "7*7") " eight times -- depth, not path, is what that last row buys, and "
        "deleting the clause costs exactly those three of the nine their answer.")
     (p "The last two are the zero clauses, and they are the only place "
        (code/inline "*°") " answers without grounding its arguments. "
        (code/inline "(*° '() m '())") " has a single answer that says nothing at all about "
        (code/inline "m") " -- it reifies as " (code/inline "α") " -- while "
        (code/inline "(*° n '() '())") " answers twice, once from "
        (code/inline "(=° '() n)") " and once from " (code/inline "(pos° n)") ", so a "
        "non-empty " (code/inline "n") " comes back as the bare pair "
        (code/inline "(α . β)") ". Zero times anything is the one fact this arithmetic knows "
        "without looking at the bits.")))

  ((test/division-by-*° _)
   (⊦= (list (build-num 4))  (μkanren-run (m 2 #t) (*° (build-num 3) m (build-num 12))))
   (⊦= (list (build-num 4))  (μkanren-run (m 2 #t) (*° m (build-num 3) (build-num 12))))
   (⊦= (list (build-num 3))  (μkanren-run (m 2 #t) (*° (build-num 4) m (build-num 12))))
   (⊦= (list (build-num 3))  (μkanren-run (m 2 #t) (*° (build-num 5) m (build-num 15))))
   (⊦= '()                   (μkanren-run (m 1 #t) (*° (build-num 3) m (build-num 13))))
   `(doc
     (p "The same goal run backwards: fix the product and one factor, leave the other fresh, "
        "and " (code/inline "*°") " divides. Nothing in the relation distinguishes input from "
        "output -- there is no mode declaration and no guard on which arguments are ground -- "
        "so this direction costs no extra code; what it costs is termination, and that is "
        "what these five assertions buy. Both factor positions are exercised because the "
        "clause that fires differs: with the fresh variable second the ground odd "
        (code/inline "n") " cannot match clause five at all and the swap in clause six is "
        "what fires; with it first clause six is dead and clause five halves the fresh "
        (code/inline "n") " instead.")
     (p "The last line is the one that matters. " (code/inline "(*° 3 m 13)") " has no "
        "solution, and a relational divide that cannot say so is useless: it would have to "
        "enumerate ever longer candidates for " (code/inline "m") " forever. It returns the "
        "empty stream in a couple of milliseconds because a ground product bounds every "
        "recursion -- clause five insists " (code/inline "p") " begin with a zero bit and "
        "shrinks it in step with " (code/inline "n") ", and the seventh clause's "
        (code/inline "bound-*°") " caps the rest.")))

  ((test/factorisation-by-*° _)
   (⊦= (list (list (build-num 1)  (build-num 12))
             (list (build-num 12) (build-num 1))
             (list (build-num 2)  (build-num 6))
             (list (build-num 4)  (build-num 3))
             (list (build-num 3)  (build-num 4))
             (list (build-num 6)  (build-num 2)))
       (μkanren-run (r 20 #t) (fresh° (x y) (*° x y (build-num 12)) (=° r (list x y)))))
   (⊦= (list (list (build-num 1) (build-num 6))
             (list (build-num 6) (build-num 1))
             (list (build-num 2) (build-num 3))
             (list (build-num 3) (build-num 2)))
       (μkanren-run (r 20 #t) (fresh° (x y) (*° x y (build-num 6)) (=° r (list x y)))))
   (⊦= (list (list (build-num 1) (build-num 5))
             (list (build-num 5) (build-num 1)))
       (μkanren-run (r 20 #t) (fresh° (x y) (*° x y (build-num 5)) (=° r (list x y)))))
   (⊦= (list (list (build-num 1) (build-num 1)))
       (μkanren-run (r 20 #t) (fresh° (x y) (*° x y (build-num 1)) (=° r (list x y)))))
   `(doc
     (p "Both factors fresh, the product ground: " (code/inline "*°") " factorises. The count "
        "is twenty and only six, four, two and one answers exist, which is the whole point of "
        "the case -- the run has to walk off the end of the stream and come back. A count of "
        "six would pass even if the seventh answer took forever to refute, and that seventh "
        "step is exactly where an unbounded " (code/inline "*°") " diverges.")
     (p "The order is the interleaving order of the clauses, not magnitude order: the two "
        "trivial factorisations arrive first from clauses three and four, then the pairs with "
        "an even left factor, and " (code/inline "(4 3)") " precedes "
        (code/inline "(3 4)") ". Five is prime and yields only the trivial pair; one yields "
        "only " (code/inline "(1 1)") ", which is also the statement that this arithmetic has "
        "no non-trivial divisors of one. Every answer here is fully ground -- unlike the zero "
        "clauses, factorisation of a positive numeral leaves no bit free.")))

  ((test/odd-*° _)
   (⊦= (list (build-num 15)) (μkanren-run (p 2 #t) (odd-*° (build-num 1) (build-num 3) (build-num 5) p)))
   (⊦= (list (build-num 15)) (μkanren-run (p 2 #t) (odd-*° (build-num 2) (build-num 5) (build-num 3) p)))
   (⊦= (list (build-num 21)) (μkanren-run (p 2 #t) (odd-*° (build-num 3) (build-num 7) (build-num 3) p)))
   (⊦= (list (build-num 15)) (μkanren-run (p 2 #t) (odd-*° (build-num 1) (build-num 0) (build-num 5) p)))
   (⊦= '(yes) (μkanren-run (r 2 #t) (odd-*° (build-num 1) (build-num 3) (build-num 5) (build-num 15)) (=° r 'yes)))
   (⊦= '()    (μkanren-run (r 1 #t) (odd-*° (build-num 1) (build-num 3) (build-num 5) (build-num 14)) (=° r 'yes)))
   `(doc
     (p (code/inline "odd-*°") " is not a three-way product. Its contract is "
        (code/inline "p = 2*(x*m) + m") ", the distribution of "
        (code/inline "(2x+1)*m") " that clause seven of " (code/inline "*°") " peels off "
        "when both factors are odd, and it is written as "
        (code/inline "(*° x m q)") " followed by " (code/inline "(plus° `(0 . ,q) m p)") " -- "
        "the consed zero bit being the doubling. The first three lines read "
        "2(1*5)+5 = 15, 2(2*3)+3 = 15 and 2(3*3)+3 = 21.")
     (p "The fourth line is the surprise and the reason this case exists. "
        (code/inline "n") " occurs nowhere in the body except inside "
        (code/inline "(bound-*° q p n m)") ", so its value is never read; only its length "
        "is, and even that only as part of the budget " (code/inline "|n|+|m|") ". Passing "
        (code/inline "'()") " for " (code/inline "n") " -- arithmetically absurd, since "
        (code/inline "*°") " only ever calls this with " (code/inline "n") " equal to "
        (code/inline "2x+1") " -- still answers 15, because " (code/inline "m") " alone is "
        "long enough to pay for " (code/inline "q") ". Anyone tempted to \"use\" "
        (code/inline "n") " in a rewrite is changing a different relation.")
     (p "The last two lines check the direction " (code/inline "*°") " actually uses it in: "
        "with " (code/inline "p") " ground it is a test, and it refutes 14 rather than "
        "wandering, which is what keeps clause seven from poisoning the whole stream.")))

  ((test/bound-*° _)
   (⊦= '(yes) (μkanren-run (r 2 #t) (bound-*° '(1 1) '(1 1 1) '(1) '(1))   (=° r 'yes)))
   (⊦= '(yes) (μkanren-run (r 2 #t) (bound-*° '(0 0) '(0 0 0) '(0) '(0))   (=° r 'yes)))
   (⊦= '()    (μkanren-run (r 1 #t) (bound-*° '(1 1) '(1 1)   '(1) '(1))   (=° r 'yes)))
   (⊦= '()    (μkanren-run (r 1 #t) (bound-*° '(1 1) '(1 1 1) '()  '())    (=° r 'yes)))
   (⊦= '(yes) (μkanren-run (r 2 #t) (bound-*° '(1 1) '(1 1 1) '()  '(1 1)) (=° r 'yes)))
   (⊦= '(())           (μkanren-run (q 20 #t) (bound-*° q '(1)     '(1) '(1))))
   (⊦= '(() (α))       (μkanren-run (q 20 #t) (bound-*° q '(1 1)   '(1) '(1))))
   (⊦= '(() (α) (α β)) (μkanren-run (q 20 #t) (bound-*° q '(1 1 1) '(1) '(1))))
   (⊦= '()             (μkanren-run (q 20 #t) (bound-*° q '()      '(1) '(1))))
   `(doc
     (p (code/inline "bound-*°") " computes nothing. It succeeds exactly when "
        (code/inline "|q| < |p|") " and " (code/inline "|q| <= |n|+|m|") ", and the first "
        "five lines separate those two conditions: the same " (code/inline "q") " and "
        (code/inline "p") " pass with a one-bit " (code/inline "n") " and a one-bit "
        (code/inline "m") ", fail when both are empty, and pass again when the two bits are "
        "moved into " (code/inline "m") " -- the recursion peels " (code/inline "n") " first "
        "and falls through to " (code/inline "m") " once " (code/inline "n") " runs out, so "
        "only the sum of the two lengths is visible. Line two is the same length pattern "
        "written entirely in zero bits, which is arithmetic nonsense and still succeeds: the "
        "bits are never inspected.")
     (p "The last four lines are why the relation exists at all. With "
        (code/inline "p") " ground, " (code/inline "bound-*°") " is a finite generator: it "
        "enumerates every list strictly shorter than " (code/inline "p") " and no longer than "
        (code/inline "|n|+|m|") ", leaving the bits fresh, so the answers reify as "
        (code/inline "()") ", " (code/inline "(α)") ", " (code/inline "(α β)") " and then "
        "stop; with " (code/inline "p") " empty there is nothing at all. That is the fuse "
        (code/inline "odd-*°") " lights before it calls " (code/inline "*°") ": the "
        "intermediate product " (code/inline "q") " is confined to a finite set before any "
        "multiplying happens. Remove this goal and " (code/inline "*°") " still answers every "
        "factorisation in "
        (code/inline "test/factorisation-by-*°") " -- it simply never admits there are no "
        "more.")))

  ; -- order: the two orderings, by width and by value ------------------------------------

  ((test/=l°-and-<l°-measure-the-spine _)
   (⊦= '(yes) (μkanren-run (r 1 #t) (=l° (build-num 5) (build-num 6)) (=° r 'yes)))
   (⊦= '(yes) (μkanren-run (r 1 #t) (=l° (build-num 6) (build-num 5)) (=° r 'yes)))
   (⊦= '()    (μkanren-run (r 1 #t) (=l° (build-num 5) (build-num 9)) (=° r 'yes)))
   (⊦= '()    (μkanren-run (r 1 #t) (<l° (build-num 5) (build-num 6)) (=° r 'yes)))
   (⊦= '()    (μkanren-run (r 1 #t) (<l° (build-num 6) (build-num 5)) (=° r 'yes)))
   (⊦= '(yes) (μkanren-run (r 1 #t) (<l° (build-num 5) (build-num 9)) (=° r 'yes)))
   (⊦= '()    (μkanren-run (r 1 #t) (<l° (build-num 9) (build-num 5)) (=° r 'yes)))
   `(doc (p "Five is " (code/inline "(1 0 1)") " and six is " (code/inline "(0 1 1)") ": the same "
            "number of cells, different numbers.  That pair is the whole point of this case.  "
            (code/inline "=l°") " holds in both directions and " (code/inline "<l°") " in neither, "
            "because neither relation ever looks at a bit: the base cases of " (code/inline "=l°")
            " pin " (code/inline "'()") " against " (code/inline "'()") " and " (code/inline "'(1)")
            " against " (code/inline "'(1)") ", and its recursive clause asks only that each "
            "argument be a pair with a positive tail, leaving the leading bits "
            (code/inline "a") " and " (code/inline "b") " free.  Nine is "
            (code/inline "(1 0 0 1)") ", one cell longer, and that single cell is the entire "
            "reason " (code/inline "(<l° 5 9)") " succeeds.  Since the representation is canonical "
            "-- the last bit of a positive numeral is always 1 -- a shorter numeral is always the "
            "smaller number, so " (code/inline "<l°") " never lies about value; it is merely blind "
            "to every equal-length pair, and that missing half is exactly what "
            (code/inline "<°") " restores.")))

  ((test/<°-and-<=°-measure-the-value _)
   (⊦= '(yes) (μkanren-run (r 1 #t) (<° (build-num 5) (build-num 6)) (=° r 'yes)))
   (⊦= '()    (μkanren-run (r 1 #t) (<° (build-num 6) (build-num 5)) (=° r 'yes)))
   (⊦= '()    (μkanren-run (r 1 #t) (<° (build-num 5) (build-num 5)) (=° r 'yes)))
   (⊦= '(yes) (μkanren-run (r 1 #t) (<° (build-num 4) (build-num 5)) (=° r 'yes)))
   (⊦= '()    (μkanren-run (r 1 #t) (<l° (build-num 4) (build-num 5)) (=° r 'yes)))
   (⊦= '(yes) (μkanren-run (r 1 #t) (<=° (build-num 5) (build-num 6)) (=° r 'yes)))
   (⊦= '(yes) (μkanren-run (r 4 #t) (<=° (build-num 5) (build-num 5)) (=° r 'yes)))
   (⊦= '(α)   (μkanren-run (r 1 #t) (fresh° (x) (<=° x x) (=° r x))))
   `(doc (p (code/inline "<°") " is " (code/inline "<l°") " widened: it first tries the length "
            "test, then, for equal lengths, " (code/inline "(=l° n m)") " together with "
            (code/inline "(pos° x)") " and " (code/inline "(plus° n x m)") " -- a strictly "
            "positive witness for the gap.  That " (code/inline "pos°") " is the whole of "
            "irreflexivity: " (code/inline "(<° 5 5)") " fails only because the witness would "
            "have to be " (code/inline "'()") ".  The 4-against-5 line is the one that separates "
            "the two orderings in the cheapest possible way: same length, so "
            (code/inline "<l°") " fails, yet " (code/inline "<°") " succeeds through the "
            (code/inline "plus°") " witness.  " (code/inline "<=°") " adds reflexivity with a bare "
            (code/inline "(=° n m)") ", a unification rather than a value comparison, so asking "
            "for four answers to " (code/inline "(<=° 5 5)") " still yields one: the "
            (code/inline "<°") " clause cannot also fire.  The same bare unification is why "
            (code/inline "(<=° x x)") " succeeds for a fresh " (code/inline "x") " and leaves it "
            "unbound -- the answer is the reified " (code/inline "α") ".  That query is pinned at "
            "count 1 deliberately: a second answer never arrives, because the "
            (code/inline "<°") " branch then walks " (code/inline "<l°") "'s recursive clause "
            "forever on two aliased fresh spines.")))

  ((test/<=l°-is-not-a-value-test _)
   (⊦= '(yes) (μkanren-run (r 1 #t) (<=l° (build-num 5) (build-num 6)) (=° r 'yes)))
   (⊦= '(yes) (μkanren-run (r 1 #t) (<=l° (build-num 6) (build-num 5)) (=° r 'yes)))
   (⊦= '()    (μkanren-run (r 1 #t) (<=° (build-num 6) (build-num 5)) (=° r 'yes)))
   (⊦= '(yes) (μkanren-run (r 1 #t) (<=l° (build-num 5) (build-num 9)) (=° r 'yes)))
   (⊦= '()    (μkanren-run (r 1 #t) (<=l° (build-num 9) (build-num 5)) (=° r 'yes)))
   `(doc (p "The trap.  " (code/inline "(<=l° 6 5)") " and " (code/inline "(<=l° 5 6)")
            " both succeed -- " (code/inline "=l°") " holds between them -- while "
            (code/inline "(<=° 6 5)") " correctly fails.  " (code/inline "<=l°")
            " is a preorder on widths with no antisymmetry to lose, so substituting it for "
            (code/inline "<=°") " buys a relation that happily proves six no greater than five, "
            "and no ground query on a length-differing pair would ever reveal it.  The last two "
            "lines keep the case from being vacuous: four cells against three is still refused.  "
            "This is also the only sense in which the relation is used downstream -- "
            (code/inline "base-three-or-more°") " writes " (code/inline "(<=l° ql q)")
            " and the discarded " (code/inline "/°") " of frame 8:54 writes "
            (code/inline "(<=l° mq n)") " -- as a bound on how wide a numeral the search may "
            "consider, never as an arithmetic comparison.")))

  ((test/length-orderings-enumerate-shapes _)
   (⊦= '((α β 1))               (μkanren-run (m 5 #t) (=l° (build-num 5) m)))
   (⊦= '(())                    (μkanren-run (m 3 #t) (=l° '() m)))
   (⊦= '((α β γ δ . ε))         (μkanren-run (m 3 #t) (<l° (build-num 5) m)))
   (⊦= '((α β 1) (α β γ δ . ε)) (μkanren-run (m 3 #t) (<=l° (build-num 5) m)))
   `(doc (p "Run with the second argument fresh, the length orderings answer with shapes, not "
            "numbers, and that is what makes them cheap.  " (code/inline "(=l° 5 m)")
            " has exactly one answer however many are asked for: " (code/inline "(α β 1)")
            " -- three cells, top bit forced to 1 by the " (code/inline "'(1)")
            " base case, the two low bits free -- one answer standing for 4, 5, 6 and 7 at once.  "
            (code/inline "(<l° 5 m)") " collapses just as hard, to the improper list "
            (code/inline "(α β γ δ . ε)") ": " (code/inline "<l°") " peels three cells off "
            (code/inline "(1 0 1)") " and discharges the rest with " (code/inline "(>1° m)")
            ", whose unbound tail covers every numeral of four cells or more, so an infinite "
            "family of numbers arrives as a single terminating answer.  "
            (code/inline "(=l° 0 m)") " pins the degenerate end: zero is the only numeral of "
            "length zero.  The " (code/inline "<=l°") " line pins clause order -- the "
            (code/inline "=l°") " answer comes first -- which no ground query can observe.")))

  ((test/value-orderings-enumerate-numbers _)
   (⊦= (list '() (build-num 1) '(α 1) (build-num 4))
       (μkanren-run (n 8 #t) (<° n (build-num 5))))
   (⊦= (list '() (build-num 1) '(α 1))
       (μkanren-run (n 6 #t) (<l° n (build-num 5))))
   (⊦= (list (build-num 5) '() (build-num 1) '(α 1) (build-num 4))
       (μkanren-run (n 8 #t) (<=° n (build-num 5))))
   (⊦= (list '(α β γ δ . ε) (build-num 6) (build-num 7))
       (μkanren-run (m 4 #t) (<° (build-num 5) m)))
   `(doc (p "Backwards, " (code/inline "<°") " is finite and complete where "
            (code/inline "<l°") " is finite and short.  Asking for eight answers below five "
            "returns four and stops: 0, 1, " (code/inline "(α 1)") " and 4.  The middle answer "
            "is partially ground on purpose -- " (code/inline "α") " is a reified fresh variable, "
            "so that one answer is both 2 and 3, and an assertion that expected a concrete bit "
            "there would be asserting something the relation never commits to.  The "
            (code/inline "<l°") " line says which of them were free: the first three are pure "
            "length answers, and 4 is the one only the " (code/inline "=l°") " plus "
            (code/inline "plus°") " branch can produce.  " (code/inline "<=°")
            " prepends five itself, from its " (code/inline "(=° n m)") " clause, ahead of the "
            (code/inline "<°") " answers.  The last line is the mirror direction and the reason "
            "this relation terminates upwards at all: one shape answer for everything four cells "
            "and wider, then the finitely many equal-length numerals above five, 6 and 7.")))

  ((test/mirrored-orderings _)
   ; >° and >=° are ours, not the book's: the same goals with the arguments crossed.
   (⊦= '(yes) (μkanren-run (r 3 #t) (>° (build-num 9) (build-num 5)) (=° r 'yes)))
   (⊦= '()    (μkanren-run (r 3 #t) (>° (build-num 5) (build-num 9)) (=° r 'yes)))
   (⊦= '()    (μkanren-run (r 3 #t) (>° (build-num 5) (build-num 5)) (=° r 'yes)))
   (⊦= '(yes) (μkanren-run (r 3 #t) (>=° (build-num 5) (build-num 5)) (=° r 'yes)))
   (⊦= '(yes) (μkanren-run (r 3 #t) (>=° (build-num 9) (build-num 5)) (=° r 'yes)))
   (⊦= '()    (μkanren-run (r 3 #t) (>=° (build-num 5) (build-num 9)) (=° r 'yes)))
   ; zero is the empty list, so the one pair that exercises the '() clauses
   (⊦= '(yes) (μkanren-run (r 3 #t) (>° (build-num 1) (build-num 0)) (=° r 'yes)))
   (⊦= '()    (μkanren-run (r 3 #t) (>° (build-num 0) (build-num 0)) (=° r 'yes)))
   `(doc (p "The book never writes " (code/inline ">") " or " (code/inline ">=") ": "
            (code/inline "/°") " and " (code/inline "log°") " only ever ask whether something "
            "is smaller, so chapter 8 needs one direction and stops. A relation has no "
            "preferred direction though, and these four are the existing goals with their "
            "arguments crossed -- the same move the book already makes when it defines "
            (code/inline "minus°") " as " (code/inline "plus°") " read backwards. They are "
            "the one part of " (code/inline "(aux kanren arith)") " that is not in the book, "
            "which is why they are pinned here rather than taken on trust.")))

  ((test/mirrored-orderings/length-vs-value _)
   (⊦= '(yes) (μkanren-run (r 3 #t) (>l° (build-num 9) (build-num 5)) (=° r 'yes)))
   (⊦= '()    (μkanren-run (r 3 #t) (>l° (build-num 5) (build-num 9)) (=° r 'yes)))
   ; six and five are both three bits wide, so the two families part company here
   (⊦= (list '(0 1 1) '(1 0 1)) (list (build-num 6) (build-num 5)))
   (⊦= '(yes) (μkanren-run (r 3 #t) (>° (build-num 6) (build-num 5)) (=° r 'yes)))
   (⊦= '()    (μkanren-run (r 3 #t) (>l° (build-num 6) (build-num 5)) (=° r 'yes)))
   (⊦= '(yes) (μkanren-run (r 3 #t) (>=l° (build-num 6) (build-num 5)) (=° r 'yes)))
   `(doc (p "Six is " (code/inline "(0 1 1)") " and five is " (code/inline "(1 0 1)") " -- same "
            "width, different value -- so this is the pair that tells the two mirrored families "
            "apart. " (code/inline ">°") " says yes because six is the larger number; "
            (code/inline ">l°") " says no because neither numeral is the longer one; and "
            (code/inline ">=l°") " says yes on the strength of " (code/inline "=l°") " alone. "
            "A reader reaching for " (code/inline ">l°") " when they meant " (code/inline ">°")
            " gets no error and no empty stream, just a quietly different question answered, "
            "which is why the distinction is asserted rather than described.")))

  ((test/mirrored-orderings/generate _)
   (⊦= (list '(α β γ δ . ε) (build-num 6) (build-num 7))
       (μkanren-run (n 6 #t) (>° n (build-num 5))))
   (⊦= (list (build-num 5) '(α β γ δ . ε) (build-num 6) (build-num 7))
       (μkanren-run (n 6 #t) (>=° n (build-num 5))))
   (⊦= (list '() (build-num 1) '(α 1) (build-num 4))
       (μkanren-run (m 10 #t) (>° (build-num 5) m)))
   (⊦= (list (build-num 5) '() (build-num 1) '(α 1) (build-num 4))
       (μkanren-run (m 10 #t) (>=° (build-num 5) m)))
   `(doc (p "Crossing the arguments crosses the enumeration too, and both directions stay "
            "finite. Asked for six numerals above five the relation answers three and closes "
            "the stream: " (code/inline "(α β γ δ . ε)") " for everything four cells and wider, "
            "then 6 and 7, which between them are every number greater than five. Downwards it "
            "answers four -- 0, 1, " (code/inline "(α 1)") " covering both 2 and 3, and 4. "
            (code/inline ">=°") " prepends five itself in each direction, from the "
            (code/inline "(=° n m)") " clause of " (code/inline "<=°")
            ". Counts of six and ten against streams of three and four are the point: they "
            "assert exhaustion, not merely the first few answers.")))

  ((test/numeral-equality-is-unification _)
   (⊦= '(yes) (μkanren-run (r 3 #t) (=° (build-num 5) (build-num 5)) (=° r 'yes)))
   (⊦= '(yes) (μkanren-run (r 3 #t) (≠° (build-num 5) (build-num 4)) (=° r 'yes)))
   (⊦= '()    (μkanren-run (r 3 #t) (≠° (build-num 5) (build-num 5)) (=° r 'yes)))
   `(doc (p "There is deliberately no " (code/inline "=n°") " or " (code/inline "≠n°")
            " beside the four mirrored orderings. " (code/inline "build-num") " is canonical "
            "-- little-endian, no trailing zero -- so two numerals denote the same number "
            "exactly when their lists unify, and " (code/inline "(aux kanren micro)") "'s own "
            (code/inline "=°") " and " (code/inline "≠°") " already decide that. A wrapper "
            "would add a name and hide which relation is doing the work. The strengths do "
            "differ, and that is worth knowing rather than wrapping away: "
            (code/inline "(=° n m)") " on two fresh variables unifies them without making "
            "either one a numeral, whereas " (code/inline "(<=° n m)")
            " constrains both to be numerals on the way to answering.")))

  ; -- div: split° and long division ------------------------------------------------------

  ((test/split° _)
   ; (split° n r l h)  <=>  n = h·2^(|r|+1) + l  and  l < 2^(|r|+1)
   (⊦= (list (build-num 2) (build-num 1))                    ;  6 = 1·4 + 2
       (car (μkanren-run (r 1 #t) (fresh° (l h) (split° (build-num 6) '(1) l h) (=° r (list l h))))))
   (⊦= (list (build-num 0) (build-num 3))                    ;  6 = 3·2 + 0
       (car (μkanren-run (r 1 #t) (fresh° (l h) (split° (build-num 6) '() l h) (=° r (list l h))))))
   (⊦= (list (build-num 5) (build-num 1))                    ; 13 = 1·8 + 5
       (car (μkanren-run (r 1 #t) (fresh° (l h) (split° (build-num 13) '(0 1) l h) (=° r (list l h))))))
   (⊦= (list (build-num 0) (build-num 3))                    ; 12 = 3·4 + 0, and l is '(), not '(0 0)
       (car (μkanren-run (r 1 #t) (fresh° (l h) (split° (build-num 12) '(1) l h) (=° r (list l h))))))
   (⊦= (list (build-num 0) (build-num 0))                    ;  0 = 0·4 + 0
       (car (μkanren-run (r 1 #t) (fresh° (l h) (split° '() '(1) l h) (=° r (list l h))))))
   (⊦= (build-num 6)                                         ; and backwards: 1·4 + 2 = 6
       (car (μkanren-run (r 1 #t) (split° r '(1) (build-num 2) (build-num 1)))))
   `(doc
      (p (code/inline "split°") " is not a shift on " (code/inline "n") ": the second argument is a "
         "ruler, and the relation holds exactly when " (code/inline "n = h·2^(|r|+1) + l")
         " with " (code/inline "l < 2^(|r|+1)") ". The rows read 6=1·4+2, 6=3·2+0, 13=1·8+5, "
         "12=3·4+0. Two of them pin what that equation leaves open. Splitting 12 at width 1 "
         "answers " (code/inline "l = '()") ", not " (code/inline "'(0 0)") ": the halves come "
         "back as numerals, and a trailing zero would not fail here but three frames later, when "
         (code/inline "*°") " or " (code/inline "plus°") " inside "
         (code/inline "base-three-or-more°") " refuses an unnormalised operand. "
         (code/inline "n = '()") " drives the first " (code/inline "cond°") " clause and forces "
         "both halves empty, which is the base case " (code/inline "n-wider-than-m°") " leans on "
         "when it splits a remainder it has not yet proved positive. The last row runs the same "
         "equation backwards and reassembles " (code/inline "n") " from its halves; it terminates "
         "no matter which of " (code/inline "n") ", " (code/inline "l") ", " (code/inline "h")
         " are fresh because every recursive clause eats one cell of " (code/inline "r")
         " and every base clause demands " (code/inline "r = '()") " -- the ground ruler, and "
         "nothing else, bounds the depth of the search.")))

  ((test/split°/ruler _)
   (⊦= (list (build-num 6) (build-num 0))                    ; r spelled as the numeral 2
       (car (μkanren-run (r 1 #t) (fresh° (l h) (split° (build-num 6) '(0 1) l h) (=° r (list l h))))))
   (⊦= (list (build-num 6) (build-num 0))                    ; r spelled as the numeral 3
       (car (μkanren-run (r 1 #t) (fresh° (l h) (split° (build-num 6) '(1 1) l h) (=° r (list l h))))))
   (⊦= (list (build-num 6) (build-num 0))                    ; r spelled as no numeral at all
       (car (μkanren-run (r 1 #t) (fresh° (l h) (split° (build-num 6) '(a b) l h) (=° r (list l h))))))
   (⊦= '((α))                                                ; one cell, its value never demanded
       (μkanren-run (r 3 #t) (split° (build-num 6) r (build-num 2) (build-num 1))))
   `(doc
      (p "No clause of " (code/inline "split°") " ever looks at a digit of " (code/inline "r")
         ": it is matched only against " (code/inline "'()") " or " (code/inline "`(,a . ,r^)")
         " with " (code/inline "a") " fresh, so the ruler is consumed as a unary counter. The "
         "three rows split 6 at width 2 with the ruler written as the numeral 2, as the numeral 3, "
         "and as " (code/inline "'(a b)") ", which is not a numeral at all, and all three agree. "
         "The last row is the same fact reported by the search itself: asked which ruler splits 6 "
         "into " (code/inline "l=2") ", " (code/inline "h=1") ", it answers once, with "
         (code/inline "(α)") " -- a one-cell list whose element was never constrained. That is the "
         "property " (code/inline "n-wider-than-m°") " spends: it passes " (code/inline "/°")
         "'s remainder as the ruler of both " (code/inline "(split° n r nl nh)") " and "
         (code/inline "(split° q r ql qh)") " while that remainder is still only bounded by "
         (code/inline "(<° r m)") ", not known. A tidier " (code/inline "split°") " that unified "
         (code/inline "r") " with a bit list would quietly constrain the remainder and turn long "
         "division into guessing.")))

  ((test//° _)
   (⊦= (list (build-num 0) (build-num 2))                    ; 2 = 5·0 + 2   first clause
       (car (μkanren-run (r 1 #t) (fresh° (q rem) (/° (build-num 2) (build-num 5) q rem) (=° r (list q rem))))))
   (⊦= (list (build-num 0) (build-num 0))                    ; 0 = 3·0 + 0   first clause
       (car (μkanren-run (r 1 #t) (fresh° (q rem) (/° '() (build-num 3) q rem) (=° r (list q rem))))))
   (⊦= (list (build-num 1) (build-num 0))                    ; 5 = 5·1 + 0   second clause
       (car (μkanren-run (r 1 #t) (fresh° (q rem) (/° (build-num 5) (build-num 5) q rem) (=° r (list q rem))))))
   (⊦= (list (build-num 5) (build-num 2))                    ; 17 = 3·5 + 2  third clause
       (car (μkanren-run (r 1 #t) (fresh° (q rem) (/° (build-num 17) (build-num 3) q rem) (=° r (list q rem))))))
   (⊦= (list (build-num 3) (build-num 0))                    ; 12 = 4·3 + 0  third clause, exact
       (car (μkanren-run (r 1 #t) (fresh° (q rem) (/° (build-num 12) (build-num 4) q rem) (=° r (list q rem))))))
   (⊦= (list (build-num 7) (build-num 0))                    ; 7 = 1·7 + 0   third clause, m = 1
       (car (μkanren-run (r 1 #t) (fresh° (q rem) (/° (build-num 7) (build-num 1) q rem) (=° r (list q rem))))))
   (⊦= '()                                                   ; 5 / 0: no answers, and no divergence
       (μkanren-run (r 1 #t) (fresh° (q rem) (/° (build-num 5) '() q rem) (=° r (list q rem)))))
   `(doc
      (p (code/inline "/°") " dispatches on size, and the rows cover all three of its clauses. "
         "2/5 and 0/3 take the first: a dividend narrower than the divisor is not a recursion at "
         "all, just " (code/inline "q='()") " and " (code/inline "r=n") ". 5/5 takes the second, "
         "where " (code/inline "(=l° m n)") " pins the quotient to 1 and " (code/inline "plus°")
         " recovers the remainder. 17/3, 12/4 and 7/1 take the third, the only clause that reaches "
         (code/inline "n-wider-than-m°") ". 12/4 is here because an exact division is the one case "
         "whose remainder is " (code/inline "'()") ", so it is the row that would break first if "
         (code/inline "(<° r m)") " were ever weakened to " (code/inline "<=°") ", and 7/1 is the "
         "degenerate divisor on the other side, where the quotient is the dividend itself. The "
         "last row divides by zero: no clause can hold, since two of them need "
         (code/inline "(<° r m)") " and nothing is below " (code/inline "'()") ", and the point of "
         "asserting the empty stream is that the failure is finite -- the guards reject before the "
         "recursion is entered, so 5/0 costs nothing instead of hanging. Cost note: 17/3 (~60 ms) "
         "dominates this family by an order of magnitude -- 12/4 is next at ~7 ms, 7/1 costs ~3 ms "
         "and the three narrow cases are at or under 1 ms; the price is in the bit width of the "
         "operands, so widen the numbers only with a reason.")))

  ((test//°/relational _)
   (⊦= (list (build-num 17))                                 ; n such that n = 3·5 + 2
       (μkanren-run (r 2 #t) (/° r (build-num 3) (build-num 5) (build-num 2))))
   (⊦= (list (build-num 3))                                  ; m such that 17 = m·5 + 2
       (μkanren-run (r 2 #t) (/° (build-num 17) r (build-num 5) (build-num 2))))
   (⊦= (list (build-num 5))                                  ; the quotient
       (μkanren-run (r 2 #t) (/° (build-num 17) (build-num 3) r (build-num 2))))
   (⊦= (list (build-num 2))                                  ; the remainder
       (μkanren-run (r 2 #t) (/° (build-num 17) (build-num 3) (build-num 5) r)))
   `(doc
      (p "One equation, " (code/inline "17 = 3·5 + 2") ", asked four ways: each argument in turn "
         "left fresh while the other three are ground. Every run asks for two answers and gets "
         "one, so these assertions pin exhaustion as well as value -- the stream is not merely "
         "correct at its head, it closes, and " (code/inline "/°") " is a function in all four "
         "directions on these arguments. The last two directions are the cheap ones, since "
         (code/inline "q") " and " (code/inline "r") " are what the clauses compute anyway. The "
         "first two run the long division backwards: with " (code/inline "n") " or "
         (code/inline "m") " unknown, the third clause's guards " (code/inline "(pos° q)") ", "
         (code/inline "(<l° m n)") " and " (code/inline "(<° r m)") " are the only thing keeping "
         "the width of the answer finite, and " (code/inline "split°") " inside "
         (code/inline "n-wider-than-m°") " has to assemble a dividend it was never given, which "
         "works only because that relation is driven by the ruler rather than by "
         (code/inline "n") ". Recovering " (code/inline "m") " is the slowest of the four at "
         "~140 ms, five times the forward quotient.")))

  ((test/n-wider-than-m° _)
   (⊦= (list (list (build-num 0) (build-num 17))             ; 17 = 3·0 + 17
             (list (build-num 5) (build-num 2))              ; 17 = 3·5 + 2   <- the only division
             (list (build-num 1) (build-num 14))             ; 17 = 3·1 + 14
             (list (build-num 4) (build-num 5)))             ; 17 = 3·4 + 5
       (μkanren-run (r 4 #t)
         (fresh° (q rem) (n-wider-than-m° (build-num 17) (build-num 3) q rem) (=° r (list q rem)))))
   (⊦= (list (list (build-num 5) (build-num 2)))             ; same n and m, through /°: one answer
       (μkanren-run (r 4 #t)
         (fresh° (q rem) (/° (build-num 17) (build-num 3) q rem) (=° r (list q rem)))))
   (⊦= '()                                                   ; the junk answer, offered to /°
       (μkanren-run (r 1 #t)
         (/° (build-num 17) (build-num 3) '() (build-num 17)) (=° r 'yes)))
   `(doc
      (p (code/inline "n-wider-than-m°") " is the long-division recurrence, not division. It "
         "relates " (code/inline "n") ", " (code/inline "m") ", " (code/inline "q") ", "
         (code/inline "r") " whenever " (code/inline "n = m·q + r") " and says nothing about the "
         "remainder being small or the quotient being positive, so on 17 and 3 it streams "
         "q=0 r=17, then q=5 r=2, then q=1 r=14, then q=4 r=5, and does not stop there -- all four "
         "satisfy the equation and only the second is a quotient. The discipline lives entirely in "
         "the caller: " (code/inline "/°") "'s third clause adds " (code/inline "(pos° q)") ", "
         (code/inline "(<l° m n)") " and " (code/inline "(<° r m)") " before delegating, and the "
         "second assertion is the same query through " (code/inline "/°") ", asking for four "
         "answers and receiving exactly one. The third assertion hands " (code/inline "/°")
         " the first junk answer directly and watches it be refused. Anyone who inlines "
         (code/inline "n-wider-than-m°") " into " (code/inline "/°") ", or who moves a guard down "
         "into it for symmetry, gets a " (code/inline "/°") " that answers q=0 r=n for every "
         "division and still passes any test that only looks at the head of the stream -- which is "
         "why this case reads four answers deep.")))

  ; -- log: exp2°, the logarithm and exponentiation ---------------------------------------

  ((test/exp2° _)
   (define (⌊log2⌋ n) (car (μkanren-run (q 1 #t) (exp2° (build-num n) '() q))))
   (⊦= (build-num 0) (⌊log2⌋ 1))
   (⊦= (build-num 1) (⌊log2⌋ 2))
   (⊦= (build-num 2) (⌊log2⌋ 5))
   (⊦= (build-num 3) (⌊log2⌋ 8))
   (⊦= (build-num 3) (⌊log2⌋ 15))
   (⊦= (build-num 4) (⌊log2⌋ 16))
   (⊦= '((0 0 0 1) (1 0 0 1) (0 1 0 1) (0 α 1 1) (1 1 0 1) (1 α 1 1))
       (μkanren-run (n 9 #t) (exp2° n '() (build-num 3))))
   `(doc
     (p "Despite the name " (code/inline "exp2°") " computes no power: "
        (code/inline "(exp2° n '() q)") " relates " (code/inline "n") " to "
        (code/inline "q = ⌊log₂ n⌋") ", and it does so structurally -- the yardstick "
        (code/inline "b") " is doubled by " (code/inline "(append° b `(1 . ,b) b2)")
        ", which widens a numeral instead of multiplying it. The 15/16 pair is the case that "
        "earns its place: it pins the floor, and pins that the step lands exactly on the power "
        "of two. Everything " (code/inline "log°") " does in base 2 rests on that.")
     (p "Run backwards the measure shows how coarse it is: asking which " (code/inline "n")
        " have " (code/inline "⌊log₂ n⌋ = 3") " returns the whole dyadic interval "
        (code/inline "[8,16)") " as six answers, two of them partially ground -- "
        (code/inline "(0 α 1 1)") " stands for 12 and 14, " (code/inline "(1 α 1 1)")
        " for 13 and 15. A bit the search never had to decide is left as a variable rather than "
        "split into two answers, and nine answers were requested against six that exist, so the "
        "enumeration is complete and terminating, not truncated.")))

  ((test/log°/base-two _)
   (define (q&r k n b)                   ; k answers of (q r) for n = b^q + r
     (μkanren-run (r k #t)
       (fresh° (q rem) (log° (build-num n) (build-num b) q rem) (=° r (list q rem)))))
   (⊦= `((,(build-num 0) ,(build-num 0))) (q&r 1 1 2))
   (⊦= `((,(build-num 3) ,(build-num 0))) (q&r 1 8 2))
   (⊦= `((,(build-num 3) ,(build-num 1))) (q&r 1 9 2))
   (⊦= `((,(build-num 3) ,(build-num 4))) (q&r 4 12 2))
   (⊦= `((,(build-num 4) ,(build-num 0))) (q&r 2 16 2))
   `(doc
     (p (code/inline "log°") " special-cases base two with the clause "
        (code/inline "(=° '(0 1) b)") ", which performs no arithmetic at all: it reads "
        (code/inline "q") " off " (code/inline "exp2°") " and then uses "
        (code/inline "split°") " to cut " (code/inline "n") " at that position for the "
        "remainder. Hence none of these five queries costs more than twenty milliseconds where the "
        "matching base-three query costs eighty, and the weight is carried by the two non-exact "
        "rows, 9 = 2³ + 1 and 12 = 2³ + 4: they are the assertions that would survive nothing, "
        "since " (code/inline "r") " reaches them only through " (code/inline "split°")
        ". 8 and 16 are exact powers whose remainder stays " (code/inline "'()") " however "
        (code/inline "split°") " misbehaves, and 1 never enters this clause at all -- "
        (code/inline "(pos° dd)") " demands three bits, so " (code/inline "n = 1")
        " is answered by the first clause as " (code/inline "q = 0") ", "
        (code/inline "r = n - 1") ".")
     (p "Asking for four answers of " (code/inline "log° 12 2") " and getting exactly one is "
        "the contract, not an accident. The clause commits to " (code/inline "q = ⌊log₂ n⌋")
        ", after which " (code/inline "r") " is a function of it, so the base-two path is "
        "deterministic -- unlike the small frames below, where the same query shape has two "
        "answers.")))

  ((test/log°/small-frames _)
   (define (q&r k n b)                   ; k answers of (q r) for n = b^q + r
     (μkanren-run (r k #t)
       (fresh° (q rem) (log° (build-num n) (build-num b) q rem) (=° r (list q rem)))))
   (⊦= `((,(build-num 0) ,(build-num 2))) (q&r 4 3 7))
   (⊦= `((,(build-num 1) ,(build-num 0)) (,(build-num 0) ,(build-num 4))) (q&r 4 5 5))
   (⊦= `(((α . β) ,(build-num 4))) (q&r 3 5 1))
   (⊦= `(((α . β) ,(build-num 5))) (q&r 3 5 0))
   `(doc
     (p "The first four clauses of " (code/inline "log°") " are the non-recursive ones and each "
        "fails differently. " (code/inline "n ≤ b") " (3 in base 7) answers "
        (code/inline "q = 0, r = n - 1") ", because " (code/inline "b⁰ = 1") " is subtracted, "
        "not " (code/inline "b") ". At " (code/inline "n = b") " clauses one and two overlap and "
        (code/inline "log°") " is honestly nondeterministic: 5 in base 5 answers both "
        (code/inline "5 = 5¹ + 0") " and " (code/inline "5 = 5⁰ + 4") ", and then stops. Four "
        "answers were asked for and two exist, which is the real assertion -- the overlap is "
        "exactly two-fold and does not leak into the recursive clause.")
     (p "Bases 1 and 0 are the frames that exist only to keep " (code/inline "log°")
        " total. Since " (code/inline "1^q = 1") " and " (code/inline "0^q = 0")
        " for every positive " (code/inline "q") ", the clauses constrain "
        (code/inline "q") " with nothing but " (code/inline "(pos° q)") " and it comes back "
        "reified as " (code/inline "(α . β)") " -- a pair with both halves fresh, which is "
        (code/inline "pos°") " and nothing more. Expecting the Greek letters is expecting "
        (code/inline "log°") " to decline to enumerate: a future guard that forced "
        (code/inline "q") " to a concrete numeral here would turn one answer into infinitely "
        "many.")))

  ((test/log°/base-three-or-more° _)
   (define (q&r k n b)                   ; k answers of (q r) for n = b^q + r
     (μkanren-run (r k #t)
       (fresh° (q rem) (log° (build-num n) (build-num b) q rem) (=° r (list q rem)))))
   (⊦= `((,(build-num 2) ,(build-num 0))) (q&r 1 9 3))
   (⊦= `((,(build-num 2) ,(build-num 1))) (q&r 4 10 3))
   (⊦= `((,(build-num 2) ,(build-num 0)))
       (μkanren-run (r 1 #t)
         (fresh° (q rem)
           (base-three-or-more° (build-num 9) (build-num 3) q rem)
           (=° r (list q rem)))))
   `(doc
     (p "Every base that is not 0, 1 or 2 falls through to "
        (code/inline "base-three-or-more°") ", the most expensive relation in the file: before "
        "it commits to a " (code/inline "q") " it brackets " (code/inline "b^q ≤ n < b^(q+1)")
        " by widths, calling " (code/inline "exp2°") " twice, " (code/inline "/°") " twice and "
        (code/inline "repeated-mul°") " twice. Nine in base three costs about 80 ms against 5 ms "
        "for the base-two clause; that ratio, not the two answers, is why every base-three case "
        "in this suite is a one-digit number.")
     (p "9 and 10 are chosen as a pair so that the exact and the inexact answer share a "
        (code/inline "q") ", and 10 is asked for four answers to pin that "
        (code/inline "q") " is the largest exponent with " (code/inline "b^q ≤ n")
        ": " (code/inline "10 = 3¹ + 7") " is arithmetically true and is "
        (b "not") " an answer -- exhausting that proof costs 300 ms against 85 ms for the "
        "first one. The third assertion "
        "calls " (code/inline "base-three-or-more°") " directly and gets the same answer, "
        "pinning that " (code/inline "log°") "'s last clause delegates the result whole instead "
        "of adjusting it -- so a guard added around the call may not change what comes back.")))

  ((test/repeated-mul° _)
   (define (n^q n q)
     (car (μkanren-run (nq 1 #t) (repeated-mul° (build-num n) (build-num q) nq))))
   (⊦= (build-num 1)  (n^q 3 0))
   (⊦= (build-num 3)  (n^q 3 1))
   (⊦= (build-num 27) (n^q 3 3))
   (⊦= (build-num 16) (n^q 2 4))
   (⊦= (build-num 1)  (n^q 1 5))
   (⊦= `(,(build-num 3))
       (μkanren-run (q 1 #t) (repeated-mul° (build-num 3) q (build-num 27))))
   (⊦= `(,(build-num 3))
       (μkanren-run (n 1 #t) (repeated-mul° n (build-num 3) (build-num 27))))
   `(doc
     (p (code/inline "repeated-mul°") " is the naive fold " (code/inline "n^q = n^(q-1) · n")
        " that " (code/inline "base-three-or-more°") " leans on, and its two base clauses carry "
        "all the risk. " (code/inline "q = 0") " answers 1 only for " (b "positive") " "
        (code/inline "n") ": the " (code/inline "(pos° n)") " guard is the sole reason "
        (code/inline "(repeated-mul° '() '() nq)") " does not assert "
        (code/inline "0⁰ = 1") ". And " (code/inline "1^5 = 1") " is the case where the "
        "accumulator never grows, so it fails loudly if the recursion counts down on the wrong "
        "argument.")
     (p "Both backward directions terminate on this same definition, for different reasons. "
        (code/inline "q") " from " (code/inline "3^q = 27") " runs because "
        (code/inline "(plus° q1 '(1) q)") " is a relation and subtracts as happily as it adds; "
        (code/inline "n") " from " (code/inline "n³ = 27") " runs because "
        (code/inline "*°") " factorises, and it terminates only because "
        (code/inline "bound-*°") " bounds the factors by the width of the ground product.")))

  ((test/exp° _)
   (define (b^q b q) (car (μkanren-run (n 1 #t) (exp° (build-num b) (build-num q) n))))
   (⊦= (build-num 1)  (b^q 2 0))
   (⊦= (build-num 32) (b^q 2 5))
   (⊦= (build-num 27) (b^q 3 3))
   (⊦= `(,(build-num 5)) (μkanren-run (q 1 #t) (exp° (build-num 2) q (build-num 32))))
   (⊦= `(,(build-num 3)) (μkanren-run (b 1 #t) (exp° b (build-num 2) (build-num 9))))
   `(doc
     (p (code/inline "exp°") " has no body of its own -- it is "
        (code/inline "(log° n b q '())") ", exponentiation read as a logarithm with no "
        "remainder -- so every use pays whatever " (code/inline "log°") " charges for that base. "
        (code/inline "2⁵") " is a few milliseconds down the base-two clause; "
        (code/inline "3³") " is about 0.4 s through " (code/inline "base-three-or-more°")
        " and is the slowest assertion in this file. It is also asserted a second time, via "
        (code/inline "repeated-mul°") " above, on purpose: "
        (code/inline "base-three-or-more°") " reaches 27 " (b "using")
        " " (code/inline "repeated-mul°") ", and agreement between the two readings is the "
        "cheapest available check that the bracketing is not off by one.")
     (p "The two backward runs are the interesting ones. "
        (code/inline "(exp° 2 q 32)") " is a discrete logarithm and needs no help. "
        (code/inline "(exp° b 2 9)") " is an integer square root, and it works only because "
        (code/inline "exp°") " pins the remainder to " (code/inline "'()") ": with the "
        "remainder left fresh the same query answers " (code/inline "(() (1 0 0 1))") " and "
        (code/inline "((1) (0 0 0 1))") " first -- bases 0 and 1 satisfy "
        (code/inline "log°") " for every " (code/inline "n") " -- and only then 3. Zero "
        "remainder is exactly the filter that removes the degenerate bases.")))

  )

(unittest/✓ microkanren-arith-suite)
