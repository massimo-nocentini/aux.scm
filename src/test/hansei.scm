
(import
  (only (scheme base) make-parameter parameterize)
  (only (chicken flonum) flonum-print-precision)
  (chicken sort)
  (only (chicken memory representation) record->vector)
  (only (chicken port) with-output-to-string)
  (aux base) (aux unittest) (aux hansei) (aux hansei pmap) srfi-1)

; Two CHICKEN hazards for stochastic code (plan §5.6, D11, D12), which every model of this file
; avoids:
;   - `map` builds its result destructively, so a continuation captured inside its procedure
;     must not be resumed: (probcc-reify/exact (map (λ (x) (probcc-coin 1/2)) '(1 2))) gives
;     the single entry (#t #t #f #f #t #f) at mass 1, the four worlds sharing one list built in
;     place, instead of four entries at 1/4; srfi-1's
;     `list-tabulate` and `filter-map` choose from the last element to the first.  Models use
;     probcc-map, left to right as OCaml's `List.map`;
;   - csi evaluates the operands of a call with five or more of them, a quasiquote template or a
;     constructor such as `list` or `vector` included, so that a resumed continuation
;     accumulates them: (probcc-reify/exact (list 'a 'b 'c 'd (probcc-coin 1/2))) gives
;     (a b c d #t #f) in the second world.  Models bind every choice with let* first.
; OCaml evaluates the components of a tuple and the arguments of a call from right to left, and
; `let x = e1 and y = e2 in` from left to right (D19): ported models sequence them with let*.

; The bit-exact checks print their flonums with 17 digits, so that a one-ulp failure shows (plan
; §8).
(flonum-print-precision 17)

; (new) the sources of the definitions made with define/shown, newest first, for the docs (as
; test/hakaru10-exact.scm:32-41).
(define sources '())

; (new) (define/shown name definition): make the definition, and keep its source under name.
(define-syntax-rule (define/shown name definition)
  (begin definition (push! (cons 'name 'definition) sources)))

; (new) the sources of the definitions of the names, in order, one code/scheme element each
; (spliced with ,@ into a doc).
(define (shown . names) (map (λ (name) `(code/scheme ,(alist-ref name sources))) names))

; cf. DSL p.12 l.539-546, p.15 l.653-656 and p.16 l.712-719: the lawn (grass) model, the
; probability of rain given wet grass; exact_reify gives [(0.2838, V true); (0.322, V false)],
; 0.322 being 0.3219999999999999 (as test/probcc/grass-model).
(define/shown lawn
  (define (lawn)
    (let* ((rain (probcc-coin 0.3))
           (sprinkler (probcc-coin 0.5))
           (grass-is-wet (or (and (probcc-coin 0.9) rain)
                             (and (probcc-coin 0.8) sprinkler)
                             (probcc-coin 0.1))))
      (probcc-when grass-is-wet rain))))

; cf. SM:224-233 = N:216-225: the drunk coin, a fair coin that is lost with probability 0.9, x
; drawn before lost; dcoin-and n is the conjunction of n tosses, `&&` left to right.
(define/shown drunk-coin
  (define (drunk-coin)
    (let* ((x (probcc-coin 0.5))
           (lost (probcc-coin 0.9)))
      (if lost (probcc-impossible) x))))

(define/shown dcoin-and
  (define (dcoin-and n)
    (if (= n 1) (drunk-coin) (and (drunk-coin) (dcoin-and (sub1 n))))))

; cf. IN:344-426, the some-all game with the asymmetric utility: agent A hears forms from agent
; S, who knows that two people come, and buys food for 0 .. 3 of them.  Values of formv are
; symbols, an action `A n` is n.  OCaml adds from the right, so S_b1 is forced before S_b0 in
; people-came (D19); the masses are exact, the utilities flonums, -0. included.
(define (people-came state)
  (let* ((b1 (if (state 'S_b1) 2 0))
         (b0 (if (state 'S_b0) 1 0)))
    (+ b0 b1)))

(define (utility state bought-food)
  (let1 (diff (- bought-food (people-came state)))
    (* -10. (exact->inexact (cond
                              ((= diff 0) 0)
                              ((> diff 0) diff)
                              (else (* 10 (- diff))))))))

(define (language form state)
  (and (if (> (count form '(F_all F_some F_notall F_none)) 1) (state 'S_conj) #t)
       (let1 (pc (people-came state))
         (and (if (form 'F_all) (= pc 3) #t)
              (if (form 'F_some) (> pc 0) #t)
              (if (form 'F_notall) (< pc 3) #t)
              (if (form 'F_none) (= pc 0) #t)))))

(define/shown agent-A-model
  (define ((agent-A-model form))
    (let* ((s0 (probcc-letlazy (τ (probcc-coin 1/2))))
           (s1 (probcc-letlazy (τ (probcc-coin 1/2))))
           (sc (probcc-letlazy (τ (probcc-coin 1/2))))
           (state (λ (s) (case s ((S_b0) (s0)) ((S_b1) (s1)) ((S_conj) (sc))))))
      (probcc-when (language form state)
        (let* ((action (probcc-distribution '((0 1) (1 1) (2 1) (3 1))))
               (u (utility state action)))
          (list u action))))))

; cf. IN:147-156 `argmax`: the actions of a table of (utility action) of maximal expected
; utility, as (u action) lists, several when tied.
(define (argmax plans)
  (let1 (pl (fold (λ (e pm)
                    (match/first e
                      (((V (,u ,a)) ,p)
                       (pmap-insert-with (λ (n o) (list (+ (car n) (car o)) (+ (cadr n) (cadr o))))
                                         a (list (* u p) p) pm))))
                  pmap-empty
                  plans))
    (pmap-foldi (λ (a up acc)
                  (let1 (u (/ (car up) (cadr up)))
                    (cond
                      ((null? acc) (list (list u a)))
                      ((> u (caar acc)) (list (list u a)))
                      ((= u (caar acc)) (cons (list u a) acc))
                      (else acc))))
                pl
                '())))

; cf. IN:280-290 `all_forms`, the 16 sets of forms.
(define all-forms
  (let loop ((i 0))
    (if (> i 15)
        '()
        (cons (append (if (odd? i) '(F_all) '())
                      (if (odd? (quotient i 2)) '(F_some) '())
                      (if (odd? (quotient i 4)) '(F_notall) '())
                      (if (odd? (quotient i 8)) '(F_none) '()))
              (loop (add1 i))))))

; cf. IN:389-404 `agent_S_ideal_state` and `agent_S_model`: the speaker, who knows that two
; people come, chooses the forms, and the hearer's action is inferred by `inferencer`
; (`inner_model`, IN:372-374, is argmax over it).
(define/shown agent-S-model
  (define ((agent-S-model inferencer))
    (let* ((formvs (probcc-distribution (probcc-map (λ (v) (list v 1)) all-forms)))
           (form (λ (fv) (and (memq fv formvs) #t)))
           (action-A (match/first (argmax (inferencer (agent-A-model form)))
                       (((,_ ,x)) x)
                       (() (probcc-uniform 4))
                       (,lst (let1 (p (/ 1 (length lst)))
                               (probcc-distribution (probcc-map (λ1-match/first ((,_ ,x) (list x p)))
                                                                lst))))))
           (u (utility (λ (s) (eq? s 'S_b1)) action-A)))
      (list u formvs))))

(define-suite hanseitest

  ((doc r) `((structure/section "Introduction")
             (p "Quoting from " 
                (cite/a "https://okmij.org/ftp/kakuritu/kakuritu.html" 
                        "Embedded domain-specific languages for probabilistic programming")
                ":"
                (cite/quote
                  "Oleg Kiselyov"

                  "Broadly speaking, probabilistic programming languages are to express
		  computations with degrees of uncertainty, which comes from the
		  imprecision in input data, lack of the complete knowledge or is
		  inherent in the domain. More precisely, the goal of probabilistic
		  programming languages is to represent and automate reasoning about
		  probabilistic models, which describe uncertain quantities -- random
		  variables -- and relationships among them."))
             (p
               "Here we focus on the dsl " (i "Hansei") " " 
               (cite/a "https://okmij.org/ftp/kakuritu/Hansei.html" 
                       "Embedded probabilistic domain-specific language HANSEI")
               " and the corresponding published paper "
               (cite/a "https://okmij.org/ftp/kakuritu/dsl-paper.pdf" 
                       (i "Embedded Probabilistic Programming")
                       ", In proceedings of the IFIP working conference on
		       domain-specific languages, ed. Walid Taha. LNCS 5658,
		       Springer, 2009, pp. 360-384.")
               ", by Oleg Kiselyov and Chung-chieh Shan; moreover, an application to logic can be found in "
               (cite/a "https://okmij.org/ftp/kakuritu/logic-programming.html"
                       "HANSEI as a Declarative Logic Programming Language")
               ".")

             (structure/section "Implementation")

             (p "The first implementation of our language uses the probability
		monad that represents a stochastic computation as a lazy search
		tree. That is, our implementation uses the type constructor " (code/inline "pV") 
                " defined below.  "
                (code/lang "ocaml" "type 'a vc = V of 'a | C of (unit -> 'a pV) and 'a pV = (prob * 'a vc) list")
                "Each node in a tree is a weighted list of branches. The empty
		list denotes failure, and a singleton list " (code/inline "[(p, V v)]") 
                " denotes a deterministic successful outcome" (code/inline v) "with the
		probability mass " (code/inline p) ". A branch of the form "
                (code/inline "V v") " is a leaf node
		that describes a possible successful outcome, whereas a branch
		of the form " (code/inline "C thunk") " is not yet explored.
		The intended meaning of a search tree of type " (code/inline "'a pV") 
                " is a discrete probability distribution over values of type "
                (code/inline "'a") ". ")

             (code/scheme/file "../aux.hansei.scm")

             (structure/section "Reification and reflection")

             (p "The model is ordinary Scheme code; a choice is a call to "
                (code/inline "probcc-distribution") ", which captures its continuation up to the
		nearest " (code/inline "resetcc") " and answers a list of branches, one thunk per
		value, each resuming the continuation with its value (DSL p.14 l.627-628). "
                (code/inline "probcc-reify/0") " runs a model under " (code/inline "resetcc")
                " and so turns it into a lazy search tree (DSL p.15 l.637):"
                (code/lang "ocaml" "let reify0 m = reset (fun () -> pv_unit (m ()))")
                "The paper's direct reifier is what this port keeps, on the single delimiter of "
                (code/inline "(aux continuation delimited)") ", where " (code/inline "probM.ml")
                " goes through a request type and its interpreter (deviation 2):"
                (cite/quote
                  "Oleg Kiselyov and Chung-chieh Shan, DSL p.15 l.676-679"
                  "We pay the price of embedding stochastic computations only when calling the
		  two stochastic operators dist and reify0. The deterministic parts of a stochastic
		  program run at full speed, as if no randomness were present and no inference were
		  in progress."))
             (p "The lawn model of DSL p.12 l.539-546, for example, is a function of no
		arguments: rain and a sprinkler may wet the grass, which is observed wet, and "
                (code/inline "(probcc-reify/exact/a lawn)") " answers rain at 0.2838 and no rain
		at 0.3219999999999999, before normalization (" (code/inline "test/probcc/grass-model")
                " reifies the same body):")
             ,@(shown 'lawn)
             (p (code/inline "probcc-reflect") " is the inverse of " (code/inline "probcc-reify/0")
                " (DSL p.16 l.694-700): it chooses following a tree, so that reifying a reflected
		tree gives an equivalent tree, and reflecting a reified model expresses the same
		distribution (DSL p.17 l.728-730; " (code/inline "test/reflect/inverse") "). Reifying
		a sub-model, flattening its tree with " (code/inline "probcc-explore")
                " and reflecting it is variable elimination (" (code/inline "probcc-variable-elimination")
                ", DSL p.17 l.748-771): " (code/inline "flips_xor' 10") " explores 9 trees of depth 2
		instead of the 1024 leaves of " (code/inline "flips_xor 10")
                "; memoizing the eliminated function on its arguments is bucket elimination ("
                (code/inline "λ-probcc-bucket") ", DSL p.18 l.776-783; "
                (code/inline "test/variable-elimination") ").")

             (structure/section "Exploration and its order")

             (p (code/inline "probcc-explore") " (I:11-28, DSL p.9 l.393-408) flattens a tree to a
		given depth, or to the end when the depth is " (code/inline "#f") " or "
                (code/inline "+inf.0") " (OCaml's " (code/inline "None") "): the values reached are
		collated, their masses summed, and the branches below the depth are kept as
		suspended entries. Upstream threads the answer, a persistent map, and the list of
		suspensions through its loop, and so does this port: a thunk forced by the explorer
		may choose in an enclosing model, whose every world then resumes the loop, so a
		mutable table would be shared by all of them ("
                (code/inline "test/explore/persistent") ").")
             (p "The answer is sorted by descending mass, with a stable sort of upstream's order
		(deviation 5): equal masses keep the values in descending "
                (code/inline "pmap-compare") " order, the stand-in for OCaml's "
                (code/inline "compare") " (deviation 6), then the suspended branches in reverse
		discovery order. So the order of the tables is deterministic ("
                (code/inline "test/explore/order") ", " (code/inline "test/uniform/edges") ").")

             (structure/section "World-local memory")

             (p "Every world of a model has its own memory, which the lazy variables, the memo
		tables and the world-local references live in. It is probM's first-class store
		(P:40-75):"
                (cite/quote
                  "Oleg Kiselyov, P:59-63"
                  "The first-class storage is immutable and pure functional. Only the pointer to
		  it is mutable. This is critical as different threads may have different views of
		  the same `memory' and one thread's updating its memory should not affect other
		  threads that share it.")
                (cite/quote
                  "Oleg Kiselyov and Chung-chieh Shan, ML p.1 l.82-83"
                  "We implement first-class stores as immutable maps, and associate one with each
		  delimited continuation."))
             (p (code/inline "probcc-thread-local") " is that pointer, a persistent map from
		fixnum locations to values, changed only with " (code/inline "set!")
                ": " (code/inline "probcc-reify/0") " starts the model in an empty memory and
		restores the caller's when the first segment returns (P:104-107); "
                (code/inline "probcc-distribution") " and " (code/inline "probcc-reflect")
                " read the memory before capturing the continuation, and every branch resumes in
		it (P:118-148). A parameter or a " (code/inline "dynamic-wind") " would not do:
		aux's " (code/inline "shift") " enters the " (code/inline "dynamic-wind")
                " frames of the captured continuation again on every resume, so a value
		cached in one world would show in another. A " (code/inline "parameterize")
                " inside a model is independent of the memory (" (code/inline "test/parameterize/memory")
                ").")

             (structure/section "Laziness")

             (p "Events take place in another order than they are observed, and a value drawn
		long before it is looked at duplicates the search tree in between (DSL p.19-20
		l.858-865). " (code/inline "probcc-letlazy") " (P:321-330) answers a thunk that
		evaluates its body at most once per world, when first called, keeping the value in
		the world-local memory: \"lazy let insertion at the point of reification\"
		(P:292-298). The built-in laziness cannot do it:"
                (cite/quote
                  "Oleg Kiselyov and Chung-chieh Shan, DSL p.21 l.912-915"
                  "It is incorrect to implement letlazy using OCaml's built-in laziness, because
		  the latter uses the mutable state of the underlying hardware, which does not `snap
		  back' when the inference procedure backtracks to an earlier point in the execution
		  of the probabilistic program.")
                "So " (code/inline "delay") ", " (code/inline "force") " and "
                (code/inline "δ") " are never used for model values. " (code/inline "probcc-letlazy")
                " is not nesting-safe, as upstream: an inner reification starts with an empty
		memory, so a lazy variable of the outer world is evaluated again inside, and the
		effects are reordered (N:65-81). " (code/inline "probcc-letlazy/nesting")
                " (P:342-370) stores the unevaluated body where it is created and lets an inner
		world ask its parent for it (" (code/inline "test/letlazy") ", "
                (code/inline "test/nested") ").")

             (structure/section "Memoization")

             (p (code/inline "probcc-memo") " (P:449-479) generalizes the lazy variables to
		stochastic functions:"
                (cite/quote
                  "Oleg Kiselyov, P:456-463"
                  "The function (memo f) :: a -> Prob b is then a proper function: applied to an
		  argument, it gives a sample from its output distribution. When applied to the
		  argument again, it gives the same sample. The memo facility is the
		  generalization of letlazy: indeed, letlazy is the particular case of memo when the
		  function to memoize has the argument unit.")
                "Its table lives in the world-local memory, so a memo must be made inside the
		reification that uses it (deviation 16; " (code/inline "test/memo")
                "). The table of " (code/inline "λ-probcc-bucket") " is global instead, shared by
		every inference, which is right only for a function that reads no world-local state
		(deviation 11).")

             (structure/section "Nested inference")

             (p "An inference procedure is an ordinary function, so a model may call one, and the
		choices it makes are choices of the enclosing model:"
                (cite/quote
                  "Oleg Kiselyov and Chung-chieh Shan, EPP p.4 l.356-360"
                  "Because a random choice made by an inference algorithm is expressed with dist
		  just like any other random choice, any inference procedure, such as the
		  importance-sampling algorithm in §4, can reason about itself or any other
		  inference procedure.")
                "An inner " (code/inline "probcc-reify/0") " pushes its own delimiter and an empty
		memory; the inner explorer forces its thunks in the extent of the outer model, so a
		choice directly inside them (" (code/inline "probcc-dist-selector")
                ", a lazy variable asking its parent) captures the inner explorer's loop, which
		therefore threads persistent state, as every inference loop of the core does (plan
		§5.7; " (code/inline "test/nested") ", " (code/inline "test/nested/thresholds")
                "). EPP p.4 l.374-379 memoizes the outer " (code/inline "biased")
                " \"as usual\" and uses it at the inner level; with probM's " (code/inline "memo")
                " that raises " (code/inline "Not_found") ", and only "
                (code/inline "letlazy_nesting") " works, here as upstream.")

             (structure/section "Inference about inference")

             (p "PE:69-77 asks how likely an estimate made by sampling is to be at least 0.3:"
                (cite/quote
                  "Oleg Kiselyov and Chung-chieh Shan, PE:70-75"
                  "Suppose we choose a coin as before, then estimate p by flipping the coin twice
		  and dividing the count of true by 2 (that is, by using rejection sampling with
		  count 2). What is the probability that our estimate is at least 0.3? It is 7/8,
		  because the only way for us to estimate below 0.3 is to choose a fair coin and get
		  false from it twice.")
                "With " (code/inline "probcc-dist-selector") ", the sampler's random choices are
		choices of the outer model, which exact inference enumerates: the outer table is
		exactly 7/8 and 1/8 (" (code/inline "test/nested/inference-about-inference") ").")

             (structure/section "Samplers and selectors")

             (p "A selector chooses one entry of a list of " (code/inline "(x p)")
                " entries and answers " (code/inline "(x ptotal)") " (deviation 18). "
                (code/inline "probcc-random-selector") " draws from OCaml's generator, which "
                (code/inline "(aux hansei random)") " transcribes, and its seed re-initialises the
		one global state, as " (code/inline "Random.init") " does (P:221-222), so seeded
		runs replay upstream's records bit for bit (see the suite of "
                (code/inline "(aux hansei random)") "); " (code/inline "probcc-dist-selector")
                " chooses through the enclosing model instead (P:238-241).")
             (p (code/inline "probcc-sample-dist/rejection") " (I:121-135) follows a path from the
		root to a leaf per sample. " (code/inline "probcc-sample-dist")
                " (I:137-198) is importance sampling with look-ahead:"
                (cite/quote
                  "Oleg Kiselyov and Chung-chieh Shan, DSL p.19 l.837-842"
                  "The key is to explore the tree shallowly before committing to any random choice
		  among its branches. Whenever this look-ahead comes across a branch that ends in
		  failure, that branch is eliminated from the available choices. Similarly, whenever
		  the look-ahead comes across a branch that ends in success, the successful result
		  is registered as a sample, and that branch is again eliminated from the available
		  choices.")
                "On the drunk coin of DSL p.18-19, the and of ten tosses of a coin lost with
		probability 0.9, rejection sampling never sees the ten heads, whereas importance
		sampling estimates their probability, 9.77e-14 exactly (" (code/inline "test/drunk-coin")
                "), at 7e-14 from 5000 samples with seed 1 (the replays of the random suite). EPP
		p.6 l.547-590 gives the algorithm step by step.")
             ,@(shown 'drunk-coin 'dcoin-and)

             (structure/section "Two CHICKEN hazards")

             (p "Two properties of CHICKEN break models that resume a continuation more than
		once, which every model of these suites avoids (plan §5.6):")
             (ol (li (code/inline "map") " builds its result destructively (srfi-1's too), so a
		     continuation captured inside its procedure must not be resumed: "
                     (code/inline "(probcc-reify/exact (map (λ (x) (probcc-coin 1/2)) '(1 2)))")
                     " answers the single entry " (code/inline "(#t #t #f #f #t #f)")
                     " at mass 1, the four worlds sharing one list built in place, instead of
		     four lists at 1/4. Models use " (code/inline "probcc-map")
                     ", left to right as OCaml's " (code/inline "List.map") " (deviation 20); "
                     (code/inline "list-tabulate") " and " (code/inline "filter-map")
                     " are re-entrant but call their procedure from the last element to the first
		     (" (code/inline "test/map/reentrant") ");")
                 (li "under " (code/inline "csi") ", a continuation captured while the operands of a
		     call with five or more operands are evaluated accumulates them when resumed,
		     even with plain " (code/inline "call/cc") ": "
                     (code/inline "(probcc-reify/exact (list 'a 'b 'c 'd (probcc-coin 1/2)))")
                     " answers " (code/inline "(a b c d #t)") " and " (code/inline "(a b c d #t #f)")
                     "; quasiquote templates and constructors are calls too. Models bind every
		     choice with " (code/inline "let*") " first. Compiled code is not affected."))
             (p "OCaml evaluates the components of a tuple and the arguments of a call from right
		to left, " (code/inline "let x = e1 and y = e2 in") " and the scrutinee of "
                (code/inline "match (e1, e2) with") " from left to right; the ported models
		sequence their choices with " (code/inline "let*") " in that order, which the
		depth-bounded and the replayed oracles depend on (plan D19).")

             (structure/section "Deviations")

             (p "The departures from upstream, each tagged in the source with its number as "
                (code/inline "deviation N") " (plan §9):")
             (ol (li "the representation: entries " (code/inline "((V v) p)") " and "
                     (code/inline "((C thunk) p)") " and pairs " (code/inline "(v p)")
                     " put the probability second; weights may be exact, and the operations on
		     them are the parameters " (code/inline "op/times") " .. "
                     (code/inline "op/greater") " (T:4-15);")
                 (li "the direct reifier: no " (code/inline "pReq") ", " (code/inline "interp")
                     " or " (code/inline "read_answer") ", the body of the reset answers the tree
		     already interpreted (P:24, 97-108);")
                 (li "a single untagged delimiter: a user's " (code/inline "resetcc")
                     " inside a model captures HANSEI's choices (P:26);")
                 (li (code/inline "shift") ", not " (code/inline "shift0")
                     ", which coincide since no shift body shifts again (P:120, 142, 361);")
                 (li "the output order of exploration: descending mass, stable over upstream's
		     order; " (code/inline "+inf.0") " or " (code/inline "#f") " for "
                     (code/inline "None") "; " (code/inline "probcc-reify/part")
                     " keeps the explorer's order (I:28; LV:68-70);")
                 (li (code/inline "pmap-compare") " for OCaml's " (code/inline "compare")
                     ": symbols are ordered by name, not by declaration, and unorderable values
		     raise (PM:101);")
                 (li (code/inline "probcc-uniform 1") " makes a choice node of one branch, "
                     (code/inline "n <= 0") " fails instead of raising, a count that is not an
		     integer raises " (code/inline "(exn hansei malformed)") ", and the masses
		     are the exact 1/n (P:156-165);")
                 (li (code/inline "probcc-uniform/range") " is half open, low .. high - 1
		     (P:173-174);")
                 (li (code/inline "probcc-geometric") " answers the trace of the trials; "
                     (code/inline "probcc-geometric/count") " is upstream's (P:181-185);")
                 (li "normalization turns exact weights into flonums with one rounding (I:299-301);")
                 (li "the bucket of " (code/inline "λ-probcc-bucket") " is the global "
                     (code/inline "λ-memo") ", wrong when the function reads world-local state
		     (DSL p.18 l.776-783);")
                 (li (code/inline "probcc-variable-elimination") " is variadic (P:267-268);")
                 (li (code/inline "probcc-letlazy/nesting") " prints no debugging output
		     (P:347-363);")
                 (li "the samplers print their " (code/inline "done") " lines only under "
                     (code/inline "probcc-verbose") " (I:131, 186);")
                 (li "the memory stores values directly in a map of fixnum locations, with one
		     global pointer, single-threaded (P:40-64);")
                 (li "a missing memory location or memo table raises " (code/inline "(exn hansei memory)")
                     " instead of " (code/inline "Not_found") ", and memo tables are ordered by "
                     (code/inline "pmap-compare") " (P:471);")
                 (li "the world-local references are reconstructed, since " (code/inline "WorldLocalMem")
                     " is not shipped, and " (code/inline "probcc-wref-set!")
                     " takes the reference first (TI:121-126);")
                 (li "a selector answers the list " (code/inline "(x ptotal)")
                     "; a seed re-initialises the global OCaml-compatible state, and an rng-source
		     may be given instead (P:221-233; T:20);")
                 (li "the ratios " (code/inline "p /. ptotal") " keep exact weights exact, and a zero
		     total gives NaN or an infinity as upstream (P:205, 240; I:47, 132, 160, 187);")
                 (li (code/inline "probcc-map") " must replace " (code/inline "map")
                     " around stochastic code (new);")
                 (li (code/inline "probcc-statistics") " is sorted, not in hash order (I:292);")
                 (li "the parser combinators are prefixed " (code/inline "parsec-") "; "
                     (code/inline "parsec-or") " is binary, " (code/inline "parsec-alt")
                     " and " (code/inline "parsec-stream-over") " take lists, and the unit value
		     is " (code/inline "(void)") " (PS:59-66, 106-113);")
                 (li (code/inline "probcc-prob-of") " skips branches not yet explored (N:292-293);")
                 (li "lazy lists are " (code/inline "'()") " and " (code/inline "(head-thunk . tail)")
                     ", promoted to a library, and " (code/inline "probcc-lobserve")
                     " compares heads with " (code/inline "equal?") " (SL:325-326; M2:116);")
                 (li "OCaml's tuples are lists (D18);")
                 (li "the exact " (code/inline "1") " and " (code/inline "0")
                     " stand for upstream's " (code/inline "1.0") " and " (code/inline "0.0")
                     " in deterministic branches, folds and missing values (P:100, 182, 198, 361;
		     I:108, 128, 300; N:293);")
                 (li "the standard deviation of " (code/inline "probcc-statistics")
                     " is " (code/inline "fpsqrt") " of a flonum, so a negative rounded variance
		     gives " (code/inline "+nan.0") " as OCaml (I:289-292);")
                 (li (code/inline "probcc-timeit") " prints the seconds with " (code/inline "~a")
                     ", not " (code/inline "%g") " (I:307);")
                 (li (code/inline "probcc-map-dist") " is promoted from example code, and a branch
		     not yet explored raises " (code/inline "(exn hansei malformed)") " (BL:456-460);")
                 (li "the errors of " (code/inline "(aux hansei pmap)") " are "
                     (code/inline "(exn pmap)") " conditions raised with " (code/inline "abort")
                     ", and " (code/inline "pmap-find") " takes a default thunk (PM:66, 76, 85, 141);")
                 (li "the errors of " (code/inline "(aux hansei random)") " are "
                     (code/inline "(exn hansei)") " conditions of kind " (code/inline "random")
                     " (OR:91-94);")
                 (li (code/inline "ocaml-random-float!") " draws three words, as the OCaml that
		     made HANSEI's records did, where OCaml 4.14 draws two (OR:167-172);")
                 (li "a count of " (code/inline "probcc-lazy-permutation/ins")
                     " that is not a non-negative exact integer raises "
                     (code/inline "(exn hansei malformed)") " (LP:281-286)."))
             (p "Kept as upstream, with a comment: one global RNG state, re-seeded by every "
                (code/inline "random_selector seed") "; " (code/inline "letlazy")
                " is not nesting-safe; a memo must be created inside the reification that uses
		it; " (code/inline "bounded_explore") " counts every leaf, whatever its value; "
                (code/inline "random_selector") " raises \"can't happen\" past the last entry; "
                (code/inline "first_success") " is breadth-first through " (code/inline "append") ".")
             (p "Upstream discrepancies: EPP p.4 says " (code/inline "biased")
                " can be memoized as usual and used at the inner level, which raises with P's "
                (code/inline "memo") " (only " (code/inline "letlazy_nesting")
                " works); M1:203-205 re-sums the exact " (code/inline "r")
                ", not the sample; PD:122 has a syntax typo; TI depends on the unreleased "
                (code/inline "WorldLocalMem") "; DSL p.5's normalized lawn differs in the last
		digits from what today's code computes; and the sampling records of "
                (code/inline "sorted.ml") ", " (code/inline "incite.ml") ", " (code/inline "hmm.ml")
                ", " (code/inline "colored_balls.ml") ", " (code/inline "bloodtype.ml") ", "
                (code/inline "blip_naive.ml") " and " (code/inline "blip_ailog.ml")
                " do not replay under the three-draw generator, which replays those of "
                (code/inline "samplingM.ml") ", " (code/inline "nested.ml") ", "
                (code/inline "slazy.ml") ", " (code/inline "parsec.ml") " and "
                (code/inline "music1a.ml") ": they come from earlier code or generators, and the
		suites check those models exactly, against today's upstream under OCaml, or
		statistically in the long suite.")))

  ((test/probcc/coin-model _)
   (⊦= '(((V ((x #t) (y #t))) 0.36)
           ((V ((x #t) (y #f))) 0.24)
           ((V ((x #f) (y #t))) 0.24)
           ((V ((x #f) (y #f))) 0.16))
         (probcc-normalize
           (probcc-reify/exact
             (let* ((p 0.6)
                    (x (probcc-coin p))
                    (y (probcc-coin p)))
               `((x ,x) (y ,y))))))
   `(doc (p "Joint distribution of tossing two " (i "biased") 
            " coins, where head has probability " (code/inline 0.6)
            " to appear.")))

  ((test/probcc/coin-model/when _)
   (⊦= '(((V (#t #t)) 0.428571428571429)
           ((V (#t #f)) 0.285714285714286)
           ((V (#f #t)) 0.285714285714286))
         (probcc-normalize
           (probcc-reify/exact
             (let* ((p 0.6)
                    (x (probcc-coin p))
                    (y (probcc-coin p)))
               (probcc-when (or x y) (list x y))))))
   `(doc (p "Slightly variation of the previous test, here it has been " 
            (b "observed") " that " (i "at least one head") " appeared.")))

  ((test/probcc/grass-model _)
   (define result 
     (probcc-reify/exact
       (let* ((rain (probcc-coin 0.3))
              (sprinkler (probcc-coin 0.5))
              (grass-is-wet
                (or (and (probcc-coin 0.9) rain)
                    (and (probcc-coin 0.8) sprinkler)
                    (probcc-coin 0.1))))
         (probcc-when grass-is-wet `(rain ,rain)))))
   (⊦= (list (probcc-value 0.322 '(rain #f)) 
               (probcc-value 0.2838 '(rain #t)))
         result)
   `(doc
      (cite/quote
        "Oleg Kiselyov"
        "The canonical example is the grass model, with three random variables
	representing the events of rain, of a switched-on sprinkler and wet
	grass. The (a priori) probabilities of the first two events are judged
	to be 30% and 50% correspondingly. Probabilities are non-negative real
	numbers that may be regarded as weights on non-deterministic choices.
	Rain almost certainly (90%) wets the grass. The sprinkler also makes
	the grass wet, in 80% of the cases. The grass may also be wet for some
	other reason.  The modeler gives such an unaccounted event 10% of a
	chance. This model is often depicted as a directed acyclic graph (DAG)
	-- so-called Bayesian, or belief network -- with nodes representing
	random variables and edges conditional dependencies. Associated with
	each node is a distribution (such as Bernoulli distribution: the flip
					  of a biased coin), or a function that
	computes a distribution from the node's inputs (such as the noisy
							     disjunction nor).
	The sort of reasoning we wish to perform on the model is finding out
	the probability distribution of some of its random variables. For
	example, we can work out from the model that the probability of the
	grass being wet is 60.6%. Such reasoning is called probabilistic
	inference. Often we are interested in the distribution conditioned on
	the fact that some random variables have been observed to hold a
	particular value. In our example, having observed that the grass is
	wet, we want to find out the chance it was raining on that day. ")
      (p "The solution to this problem shows the probability distribution of raining,
	   provided that " (i "has been observed a wet grass:")
         (code/scheme ,(probcc-normalize result))
         "as required. The following test defines and captures this problem.")))

  ((test/probcc/grass-model/complete _)
   (define result 
     (probcc-reify/exact
       (let* ((rain (probcc-coin 0.3))
              (sprinkler (probcc-coin 0.5))
              (grass-is-wet
                (or (and (probcc-coin 0.9) rain)
                    (and (probcc-coin 0.8) sprinkler)
                    (probcc-coin 0.1))))
         `((rain ,rain) 
           (sprinkler ,sprinkler)
           (grass-is-wet ,grass-is-wet)))))
   (⊦= '(((V ((rain #f) (sprinkler #f) (grass-is-wet #f))) 0.315)
           ((V ((rain #f) (sprinkler #t) (grass-is-wet #t))) 0.287)
           ((V ((rain #t) (sprinkler #t) (grass-is-wet #t))) 0.1473)
           ((V ((rain #t) (sprinkler #f) (grass-is-wet #t))) 0.1365)
           ((V ((rain #f) (sprinkler #t) (grass-is-wet #f))) 0.063)
           ((V ((rain #f) (sprinkler #f) (grass-is-wet #t))) 0.035)
           ((V ((rain #t) (sprinkler #f) (grass-is-wet #f))) 0.0135)
           ((V ((rain #t) (sprinkler #t) (grass-is-wet #f))) 0.0027)) 
         result)
   (⊦= (probcc-normalize result) result)
   `(doc (p "If we remove the assumption that " 
            (i "has been observed a wet grass")
            ", then we have the joint probability distribution of " 
            (i "all") " variables:")))

  ((test/uniform/range _)
   (⊦= '(((V 1) 1/7)
           ((V 2) 1/7)
           ((V 3) 1/7)
           ((V 4) 1/7)
           ((V 5) 1/7)
           ((V 6) 1/7)
           ((V 7) 1/7)) 
         (sort (probcc-reify/exact (probcc-uniform/range 1 8))
               (λ (a b) (< (cadr (car a)) (cadr (car b)))))))

  ((test/dfs _)
   (⊦= '(((V #t) 1/2) ((V #f) 1/2)) (probcc-dfs (probcc-reify/0 (τ (probcc-coin 1/2)))))
   (⊦= '(((V (#t #t)) 1/6) ((V (#t #f)) 1/3) ((V (#f #t)) 1/6) ((V (#f #f)) 1/3))
         (probcc-dfs (probcc-reify/0 (τ (let* ((a (probcc-coin 1/2)) (b (probcc-coin 1/3))) (list a b)))))))

  ((test/parameterize _)
   (define q (make-parameter 'outer))
   (⊦= '(((V (#f inner)) 2/3) ((V (#t inner)) 1/3))
         (probcc-reify/exact (parameterize ((q 'inner)) (let1 (x (probcc-coin 1/3)) (list x (q))))))
   (⊦= 'outer (q)))

  ((test/geometric _)
   (define result ((probcc-reify 5) (τ (probcc-geometric 0.85 's 'f))))
   (define t6 (cadr (car (sixth result))))
   (define t7 (cadr (car (seventh result))))
   (define t8 (cadr (car (eighth result))))
   (⊦= `(((V (s)) 0.85)
           ((V (s f)) 0.1275)
           ((V (s f f)) 0.019125)
           ((V (s f f f)) 0.00286875)
           ((V (s f f f f)) 0.0004303125)
           ((C ,t6) 6.4546875e-05)
           ((C ,t7) 9.68203125000001e-06)
           ((C ,t8) 1.70859375e-06))
         result))

  ((test/pmap _)
   ; the AVL invariant at every node of m's tree, reached through the record's slots
   ; #(pmap cmp tree) since the tree is not exported: a node #(l k v r h) stores h = 1 + the
   ; greater height of its children, whose heights differ by at most 2 (PM:40, 51).
   (define (avl? m)
     (letrec ((H (λ (t)
                   (cond
                     ((null? t) 0)
                     (else (let ((hl (H (vector-ref t 0)))
                                 (hr (H (vector-ref t 3))))
                             (and hl hr (<= (abs (- hl hr)) 2)
                                  (= (vector-ref t 4) (add1 (max hl hr)))
                                  (vector-ref t 4))))))))
       (and (pmap? m) (H (vector-ref (record->vector m) 2)) #t)))
   (define squares (map (λ (k) (cons k (* k k))) (iota 200)))
   (define m200 (let loop ((i 0) (m pmap-empty))
                  (if (= i 200)
                      m
                      (let1 (k (remainder (* 37 i) 200))
                        (loop (add1 i) (pmap-add k (* k k) m))))))
   (define m201 (pmap-add 200 'x m200))
   (define m199 (pmap-remove 100 m201))
   (define m3 (alist->pmap '((5 . b) (1 . a) (9 . c))))
   (⊦ equal? squares (pmap->alist m200))
   (⊦ equal? #t (avl? m200))
   (⊦ equal? #t (avl? m201))
   (⊦ equal? #t (avl? m199))
   (⊦ equal? 200 (length (pmap->alist m200)))
   (⊦ equal? 201 (length (pmap->alist m201)))
   (⊦ equal? 200 (length (pmap->alist m199)))
   (⊦ equal? #f (pmap-member? 100 m199))
   (⊦ equal? #t (pmap-member? 100 m201))
   (⊦ equal? #f (pmap-member? 200 m200))
   (⊦ equal? 'x (pmap-find 200 m199))
   (⊦ equal? '(n . o) (pmap-find 'k (pmap-insert-with cons 'k 'n (pmap-add 'k 'o pmap-empty))))
   (⊦ equal? 'n (pmap-find 'k (pmap-insert-with cons 'k 'n pmap-empty)))
   (⊦ equal? '((1 . a) ((5 . b) (9 . c)))
      (match/first (pmap-delete-find-min m3) ((,b ,m) (list b (pmap->alist m)))))
   (⊦ equal? '((9 . c) ((1 . a) (5 . b)))
      (match/first (pmap-delete-find-max m3) ((,b ,m) (list b (pmap->alist m)))))
   (⊦ equal? '((1 . a) (5 . b) (9 . c)) (pmap->alist m3))
   `(doc (p "The persistent AVL maps of " (code/inline "(aux hansei pmap)") ", ExtLib's "
            (code/inline "PMap") " as augmented by HANSEI with " (code/inline "insert_with")
            " (cf. PM:106-199): 200 insertions in a scrambled order list their keys in
	    ascending order, adding to or removing from a map leaves it unchanged, "
            (code/inline "pmap-insert-with") " stores " (code/inline "(f new old)")
            ", and every node keeps the AVL invariant with upstream's tolerance 2.")))

  ((test/pmap/api _)
   (define m (alist->pmap '((b . 2) (a . 1) (c . 3))))
   (define (collect-for-each m)
     (let1 (acc '())
       (pmap-for-each (λ (k v) (set! acc (cons (list k v) acc))) m)
       (reverse acc)))
   (⊦ equal? 2 (pmap-find 'b m))
   (⊦ equal? 'none (pmap-find 'z m (τ 'none)))
   (⊦raises (exn pmap) (pmap-find 'z m))
   (⊦raises (exn) (with-exception-handler (λ (e) 'x) (τ (pmap-find 'z m))))
   (⊦ equal? '(a . 1) (pmap-find-min m))
   (⊦raises (exn pmap) (pmap-find-min pmap-empty))
   (⊦ equal? #t (pmap-member? 'c m))
   (⊦ equal? #f (pmap-member? 'd m))
   (⊦ equal? '((a 1) (b 2) (c 3)) (collect-for-each m))
   (⊦ equal? '((a . 10) (b . 20) (c . 30)) (pmap->alist (pmap-map (λ (v) (* 10 v)) m)))
   (⊦ equal? '((a . (a 1)) (b . (b 2)) (c . (c 3))) (pmap->alist (pmap-map/key list m)))
   (⊦ equal? '(3 2 1) (pmap-fold cons m '()))
   (⊦ equal? '(c 3 b 2 a 1) (pmap-foldi (λ (k v acc) (cons k (cons v acc))) m '()))
   (⊦ equal? '((a . 1) (b . 2) (c . 3)) (pmap->alist (alist->pmap (pmap->alist m))))
   (⊦ equal? '((a . 9)) (pmap->alist (alist->pmap '((a . 1) (a . 9)))))
   (⊦ equal? #t (pmap-empty? pmap-empty))
   (⊦ equal? #f (pmap-empty? (pmap-add 1 1 pmap-empty)))
   (⊦ equal? #t (pmap? m))
   (⊦ equal? #f (pmap? '()))
   (⊦ equal? '(-1 0 1) (list (fx-compare 1 2) (fx-compare 2 2) (fx-compare 3 2)))
   (⊦ equal? '(-1 0 1)
      (list (number-compare 0.5 1) (number-compare 1/2 0.5) (number-compare 2 1.5)))
   (⊦ equal? '((3 . c) (2 . b) (1 . a))
      (pmap->alist (alist->pmap '((1 . a) (3 . c) (2 . b)) (λ (a b) (fx-compare b a)))))
   (⊦raises (exn pmap) (pmap-delete-find-min pmap-empty))
   (⊦raises (exn pmap) (pmap-delete-find-max pmap-empty))
   `(doc (p "The rest of the interface of " (code/inline "(aux hansei pmap)") " (cf. PMI): "
            (code/inline "pmap-find") " with and without a default thunk, the traversals in
	    ascending key order, the round trip through association lists, a map ordered by a
	    custom comparison, and the " (code/inline "(exn pmap)") " conditions raised on a
	    missing key or an empty map.")))

  ((test/pmap/compare _)
   (⊦ equal? '(#t 1 1. 2 #\c "b" a (1) #(0))
      (sort (list "b" 'a 2 #t #\c '(1) #(0) 1. 1) (λ (a b) (negative? (pmap-compare a b)))))
   (⊦ equal? 0 (pmap-compare -0. 0.))
   (⊦raises (exn pmap) (pmap-compare car cdr))
   `(doc (p (code/inline "pmap-compare") " stands in for OCaml's polymorphic "
            (code/inline "compare") ", the default order of " (code/inline "PMap.empty")
            " (cf. PM:101): types are ranked, numbers compare by value with an exact number
	    before an equal inexact one, " (code/inline "-0.") " equals " (code/inline "0.")
            ", symbols compare by name, and procedures cannot be compared, as OCaml's "
            (code/inline "compare: functional value") ".")))

  ((test/explore/persistent _)
   (⊦ equal? '(((V (((V a) 1/2) ((V #t) 1/2))) 1/2) ((V (((V a) 1/2) ((V #f) 1/2))) 1/2))
      (probcc-reify/exact
        (probcc-explore #f (list (probcc-value 1/2 'a)
                                 (probcc-τ 1/2 (probcc-unit (probcc-coin 1/2)))))))
   `(doc (p "The explorer threads its answer through its loop in a persistent map, as "
            (code/inline "explore") " threads " (code/inline "(ans, susp)") " (cf. I:16-28):
	    a thunk it forces may choose in the enclosing model, here the coin of the second
	    branch, so every world of that model resumes the same loop.  With a mutable table
	    shared by the worlds, the " (code/inline "#t") " of the first world leaked into the
	    second one, whose inner table read "
            (code/inline "(((V a) 1/2) ((V #f) 1/2) ((V #t) 1/2))") ".")))

  ((test/explore/order _)
   (⊦ equal? '(((V (#t #t)) 1/4) ((V (#t #f)) 1/4) ((V (#f #t)) 1/4) ((V (#f #f)) 1/4))
      (probcc-reify/exact (let* ((a (probcc-coin 1/2)) (b (probcc-coin 1/2))) (list a b))))
   `(doc (p "The order of the explorer's output (deviation 5): descending mass, and equal masses
	    in the order of upstream's " (code/inline "PMap.foldi") " (I:28), descending "
            (code/inline "pmap-compare") " order of the values, by a stable sort.")))

  ((test/explore/depth _)
   (define tree (probcc-reify/0 lawn))
   (define depth1 (probcc-explore 1 tree))
   (define t0 (τ '()))
   (define inner `(((C ,t0) 1/2)))
   (⊦= '(0.3 0.7) (probcc-map cadr tree))
   (⊦ equal? 8 (length depth1))
   (⊦ equal? #t (every (λ1-match/first (((C ,_) ,_) #t) (else #f)) depth1))
   (⊦= '(0.015 0.015 0.035 0.035 0.135 0.135 0.315 0.315) (sort (probcc-map cadr depth1) <))
   (⊦= '(((V #f) 0.322) ((V #t) 0.2838)) (probcc-explore 4 tree))
   (⊦= '(((V #f) 0.322) ((V #t) 0.2838)) (probcc-explore #f tree))
   (⊦ eq? t0 (car (cdr (car (car (probcc-explore 0 `(((C ,(τ inner)) 1))))))))
   `(doc (p "Exploring the lawn model to a depth (cf. DSL p.12 l.539-546, p.16 l.712-719,
	    I:16-28): its tree starts with the choice of rain; explored to depth 1, the
	    choices of rain and of the sprinkler are made, and the eight branches of the next
	    choice, the coin of 0.9, are left unexplored with their masses; from depth 4 on, as
	    with no bound (" (code/inline "#f") ", OCaml's " (code/inline "None")
            "), the tree is explored completely.  A branch left unexplored keeps its thunk ("
            (code/inline "eq?") ", I:26), and is not wrapped in a new one.")))

  ((test/explore/trees _)
   (define trees
     (list '(((V #t) 0.6) ((V #f) 0.4))
           '(((V #t) 0.4) ((V #f) 0.4) ((V #t) 0.2))
           `(((V #t) 0.4) ((C ,(τ '(((V #f) 0.4) ((V #t) 0.2)))) 1.))
           `(((V #t) 0.4) ((C ,(τ '(((V #f) 0.5) ((V #t) 0.25)))) 0.8))
           `(((V #t) 0.4) ((C ,(τ `(((V #f) 0.5) ((V #t) 0.25) ((C ,(τ '())) 0.25)))) 0.8))))
   (⊦= (make-list 5 '(((V #t) 0.6) ((V #f) 0.4)))
       (map (λ (tree) (probcc-explore #f tree)) trees))
   `(doc (p "The five trees of DSL p.8-9 l.364-374 represent the same distribution: the first
	    is a flat probability table, the others have unexplored branches, the last one a
	    failure node " (code/inline "'()") "; exploring each of them completely gives the
	    table.")))

  ((test/reflect/inverse _)
   (define table '(((V a) 0.2) ((V b) 0.5) ((V c) 0.3)))
   (define tree (probcc-reify/0 lawn))
   (⊦= (probcc-explore #f table) (probcc-reify/exact (probcc-reflect table)))
   (⊦= (probcc-explore #f tree) (probcc-reify/exact (probcc-reflect tree)))
   (⊦= '(((V b) 0.5) ((V c) 0.3) ((V a) 0.2)) (probcc-reify/exact (probcc-reflect table)))
   `(doc (p (code/inline "probcc-reflect") " is the inverse of " (code/inline "probcc-reify/0")
            " (cf. P:136-148, DSL p.16 l.694-700, p.17 l.728-733, NIPS l.65-67): reifying the
	    reflection of a table, or of the lazy tree of the lawn model, gives the
	    distribution they denote.")))

  ((test/normalize _)
   (define (grass-cloudy)
     (let* ((cloudy (probcc-coin 0.5))
            (rain (probcc-coin (if cloudy 0.8 0.2)))
            (sprinkler (probcc-coin (if cloudy 0.1 0.5)))
            (wet-roof (and (probcc-coin 0.7) rain))
            (wet-grass (or (and (probcc-coin 0.9) rain) (and (probcc-coin 0.9) sprinkler))))
       (probcc-when wet-grass rain)))
   (define posterior (probcc-reify/exact/a grass-cloudy))
   (⊦= '(((V #t) 0.4581) ((V #f) 0.189)) posterior)
   (⊦= '(0.6471 (((V #t) 0.7079276773296245) ((V #f) 0.29207232267037547)))
       (probcc-normalize/total posterior))
   (⊦= (cadr (probcc-normalize/total posterior)) (probcc-normalize posterior))
   (⊦ equal? `(3/10 (((V a) ,(exact->inexact 1/3)) ((V b) ,(exact->inexact 2/3))))
      (probcc-normalize/total '(((V a) 1/10) ((V b) 2/10))))
   (⊦ equal? #t (nan? (cadr (car (probcc-normalize '(((V a) 0)))))))
   `(doc (p (code/inline "probcc-normalize/total") " is " (code/inline "normalize")
            " (cf. I:296-301): the total mass, summed from the left, and the distribution
	    divided by it; " (code/inline "probcc-normalize") " is the list alone, as the "
            (code/inline "normalize") " of PE:22-24, here on the extended grass model of
	    PE:8-28.  Exact masses are divided exactly and rounded once, and a total of zero
	    gives NaN masses, as upstream, instead of raising.")))

  ((test/uniform/edges _)
   (⊦ equal? '(((V 2) 1/3) ((V 1) 1/3) ((V 0) 1/3)) (probcc-reify/exact (probcc-uniform 3)))
   (⊦ equal? #t (match/first (probcc-reify/0 (τ (probcc-uniform 1)))
                  ((((C ,_) 1)) #t)
                  (else #f)))
   (⊦ equal? '(((V 0) 1)) (probcc-reify/exact (probcc-uniform 1)))
   (⊦ equal? '() (probcc-reify/exact (probcc-uniform 0)))
   (⊦ equal? '() (probcc-reify/exact (probcc-uniform/either '())))
   ; a count that is not an integer raises, also through probcc-uniform/range; 2. is a count.
   (⊦raises (exn hansei) (probcc-reify/exact (probcc-uniform 5/2)))
   (⊦raises (exn hansei) (probcc-reify/exact (probcc-uniform 2.5)))
   (⊦raises (exn hansei) (probcc-reify/exact (probcc-uniform/range 0 2.5)))
   (⊦ equal? '(((V 1.) 0.5) ((V 0.) 0.5)) (probcc-reify/exact (probcc-uniform 2.)))
   `(doc (p (code/inline "probcc-uniform") " (cf. P:156-165) gives the value 0 the mass "
            (code/inline "1 - (n - 1)/n") ", the exact " (code/inline "1/n")
            " here, so the masses tie and the values come in descending order.  Deviation 7: "
            (code/inline "(probcc-uniform 1)") " is a choice of one branch, where upstream
	    answers 0 without choosing, a count of zero fails, where upstream raises, and a
	    count that is not an integer raises " (code/inline "(exn hansei)") " of kind "
            (code/inline "malformed") ", where OCaml's type rules it out.")))

  ((test/map/reentrant _)
   (⊦ equal? '(((V (#t #t)) 1/4) ((V (#t #f)) 1/4) ((V (#f #t)) 1/4) ((V (#f #f)) 1/4))
      (probcc-reify/exact (probcc-map (λ (_) (probcc-coin 1/2)) '(1 2))))
   ; (new) the hazard the doc below states, checked so that the doc cannot go stale: the four
   ; worlds of CHICKEN's `map` share one list built in place and collate into one entry.
   (⊦ equal? '(((V (#t #t #f #f #t #f)) 1))
      (probcc-reify/exact (map (λ (_) (probcc-coin 1/2)) '(1 2))))
   `(doc (p (code/inline "probcc-map") " is OCaml's " (code/inline "List.map")
            ", left to right, and a continuation captured inside its procedure may be resumed:
	    CHICKEN's " (code/inline "map") " builds its result destructively, and "
            (code/inline "(probcc-reify/exact (map (λ (x) (probcc-coin 1/2)) '(1 2)))")
            " gives the single entry " (code/inline "(#t #t #f #f #t #f)")
            " at mass 1, the four worlds sharing one list built in place (plan §5.6).")))

  ((test/geometric/count _)
   (define result (probcc-explore 4 (probcc-reify/0 (τ (probcc-geometric/count 0.85)))))
   (⊦ equal? '(((V 0) 0.85) ((V 1) 0.1275) ((V 2) 0.0191250000000000031)
               ((V 3) 0.00286875000000000107))
      (take result 4))
   (⊦ equal? #t (every (λ1-match/first (((C ,_) ,_) #t) (else #f)) (drop result 4)))
   ; X:65's 1.13906250000000092e-05 is written with 17 digits: CHICKEN's reader rounds that
   ; literal, and the shortest 1.139062500000001e-05 too, to the next double up.
   (⊦ equal? '(1.1390625000000009e-05 6.45468750000000426e-05 0.000430312500000000248)
      (sort (probcc-map cadr (drop result 4)) <))
   `(doc (p (code/inline "probcc-geometric/count") " is upstream's " (code/inline "geometric")
            " (cf. P:181-185), the number of failures before the first success; explored to
	    depth 4 (cf. X:63-66), the values 0 to 3 come with upstream's masses, bit for bit,
	    and three branches are left unexplored.")))

  ((test/geometric/bounded _)
   (⊦ equal? '(((V 0) 0.850000217845759) ((V 1) 0.127500032676863856)
               ((V 2) 0.0191250049015295812) ((V 3) 0.00286875073522943743)
               ((V 4) 0.000430312610284415691) ((V 5) 6.45468915426623699e-05)
               ((V 6) 9.68203373139935684e-06) ((V 7) 1.4523050597099037e-06))
      (probcc-reify/exact (probcc-geometric/bounded 7 0.85)))
   (⊦ equal? '(((V 0) 0.869565217391304324) ((V 1) 0.130434782608695676))
      (probcc-reify/exact (probcc-geometric/bounded 1 0.85)))
   (⊦ equal? '(((V 0) 0.850430530456043332) ((V 1) 0.127564579568406528)
               ((V 2) 0.0191346869352609812) ((V 3) 0.00287020304028914744))
      (probcc-reify/exact (probcc-geometric/bounded 3 0.85)))
   (⊦ equal? '(((V 0) 8/15) ((V 1) 4/15) ((V 2) 2/15) ((V 3) 1/15))
      (probcc-reify/exact (probcc-geometric/bounded 3 1/2)))
   `(doc (p (code/inline "probcc-geometric/bounded") " (cf. P:187-206) restricts the geometric
	    distribution to " (code/inline "0 .. n") " and normalizes it; with upstream's
	    operations in upstream's order, the masses are bit for bit those of X:72-77,
	    BN:189-190 and BL:332-334 (listed here by descending mass), and exact masses stay
	    exact.")))

  ((test/observe _)
   (define (grass-model)
     (let* ((rain (probcc-coin 0.3))
            (sprinkler (probcc-coin 0.5)))
       (probcc-observe (λ (wet) wet)
                       (τ (or (and (probcc-coin 0.9) rain)
                              (and (probcc-coin 0.8) sprinkler)
                              (probcc-coin 0.1))))
       rain))
   (⊦ equal? '(((V #f) 0.321999999999999897) ((V #t) 0.2838)) (probcc-reify/exact/a grass-model))
   (⊦ equal? '(((V 2) 1/4) ((V 0) 1/4))
      (probcc-reify/exact (probcc-observe even? (τ (probcc-uniform 4)))))
   `(doc (p (code/inline "probcc-observe") " (cf. P:213-214) asserts the evidence that the value
	    of a thunk passes a test, here the wet grass of the lawn model of X:48-56, whose
	    masses it gives bit for bit.")))

  ((test/letlazy _)
   (define calls '())
   (define (expensive x) (set! calls (cons x calls)) x)
   ; cf. SL:73-79: OCaml evaluates the tuple from the right, so the coin is flipped before x is
   ; forced (D19).
   (define (tests4)
     (let* ((u (probcc-distribution '((0 1/2) (1 1/2))))
            (x (probcc-letlazy
                 (τ (expensive (probcc-distribution `((,(+ u 10) 1/2) (,(+ u 20) 1/2))))))))
       (cond
         ((= u 0) #f)
         (else (let* ((c (probcc-coin 1/2))
                      (b (x))
                      (a (x)))
                 (list a b c))))))
   (⊦ equal? '(((V (#f #t #t)) 1/3) ((V (#f #f #f)) 1/3) ((V (#t #t #t)) 1/6) ((V (#t #f #f)) 1/6))
      (probcc-reify/exact (let* ((x (probcc-letlazy (τ (probcc-coin 1/2))))
                                 (c (probcc-coin 1/3)))
                            (list c (x) (x)))))
   (⊦ equal? '(((V #f) 1/2) ((V (21 21 #t)) 1/8) ((V (21 21 #f)) 1/8) ((V (11 11 #t)) 1/8)
               ((V (11 11 #f)) 1/8))
      (probcc-reify/exact/a tests4))
   (⊦ equal? 4 (length calls))
   (⊦ equal? #t (every (λ (x) (and (memv x '(11 21)) #t)) calls))
   `(doc (p (code/inline "probcc-letlazy") " (cf. P:321-330) evaluates its thunk at most once
	    per world, when it is first needed: a lazy coin gives the same value twice in a
	    world; in " (code/inline "tests4") " (cf. SL:73-93) the expensive function is never
	    called in the world " (code/inline "u = 0") ", and four times in the world "
            (code/inline "u = 1") ", split by the coin before " (code/inline "x")
            " is forced.")))

  ((test/memo _)
   (define (f* x) (probcc-distribution `((,x 1/2) (,(add1 x) 1/2))))
   ; cf. SL:694-709; the tuples are evaluated from the right (D19) and bound first (D12).
   (define (test2m)
     (let* ((f (probcc-memo f*))
            (c (probcc-coin 1/2)))
       (if c
           (let* ((e (f 2)) (d (f 2)) (b (f 1)) (a (f 1))) (list c a b d e))
           (let* ((e (f 1)) (d (f 1)) (b (f 2)) (a (f 2))) (list c a b d e)))))
   ; cf. SL:711-729, nested memoization: `f x` is evaluated before the choice of g's thunk.
   (define (test3m)
     (let* ((f (probcc-memo f*))
            (g (probcc-memo (λ (x)
                              (let1 (u (f x))
                                ((probcc-distribution `((,(τ u) 1/2) (,(τ (f x)) 1/2))))))))
            (c (probcc-coin 1/2)))
       (if c
           (let* ((e (g 1)) (d (f 1)) (b (g 1)) (a (f 1))) (list c a b d e))
           (let* ((e (f 1)) (d (g 1)) (b (f 1)) (a (g 1))) (list c a b d e)))))
   ; cf. PE:33-41; `()` stands for OCaml's unit argument.
   (define (grass-model-memo)
     (let* ((cloudy (probcc-memo (λ (_) (probcc-coin 0.5))))
            (rain (probcc-memo (λ (_) (probcc-coin (if (cloudy '()) 0.8 0.2)))))
            (sprinkler (probcc-memo (λ (_) (probcc-coin (if (cloudy '()) 0.1 0.5)))))
            (wet-roof (probcc-memo (λ (_) (and (probcc-coin 0.7) (rain '())))))
            (wet-grass (probcc-memo (λ (_) (or (and (probcc-coin 0.9) (rain '()))
                                                (and (probcc-coin 0.9) (sprinkler '())))))))
       (if (wet-grass '()) (rain '()) (probcc-impossible))))
   (⊦ equal? '(((V (#t 2 2 3 3)) 1/8) ((V (#t 2 2 2 2)) 1/8) ((V (#t 1 1 3 3)) 1/8)
               ((V (#t 1 1 2 2)) 1/8) ((V (#f 3 3 2 2)) 1/8) ((V (#f 3 3 1 1)) 1/8)
               ((V (#f 2 2 2 2)) 1/8) ((V (#f 2 2 1 1)) 1/8))
      (probcc-reify/exact/a test2m))
   (⊦ equal? '(((V (#t 2 2 2 2)) 1/4) ((V (#t 1 1 1 1)) 1/4) ((V (#f 2 2 2 2)) 1/4)
               ((V (#f 1 1 1 1)) 1/4))
      (probcc-reify/exact/a test3m))
   (⊦ equal? '(((V #t) 0.458100000000000063) ((V #f) 0.189000000000000029))
      (probcc-reify/exact/a grass-model-memo))
   (⊦raises (exn hansei)
            (probcc-reify/exact (let1 (f (probcc-memo f*)) (probcc-reify/exact (f 1)))))
   `(doc (p (code/inline "probcc-memo") " (cf. P:449-479) makes a stochastic function a proper
	    function in every world: " (code/inline "test2m") " and " (code/inline "test3m")
            " (cf. SL:694-729) give the same value for the same argument in a world, also
	    through another memoized function, and the grass model of PE:33-45, all of whose
	    variables are memoized, gives upstream's masses bit for bit.  The table lives in the
	    world-local memory, which a reification starts empty (P:106): a memo created outside
	    the reification it is used in raises (deviation 16).")))

  ((test/nested _)
   (⊦ equal? '(((V (((V #t) 1))) 1/2) ((V (((V #t) 1/2) ((V #f) 1/2))) 1/2))
      (probcc-reify/exact
        (let1 (coin (probcc-distribution `((,(τ (probcc-coin 1/2)) 1/2) (,(τ #t) 1/2))))
          (probcc-reify/exact/a coin))))
   (⊦ equal? '(((V (((V #t) 1))) 1/2) ((V (((V #t) 1/2) ((V #f) 1/2))) 1/2))
      (probcc-reify/exact (let1 (coin-choice (probcc-coin 1/2))
                            (probcc-reify/exact (or (probcc-coin 1/2) coin-choice)))))
   (⊦ equal? '(((V (((V #t) 1))) 1/2) ((V (((V #t) 1/2) ((V #f) 1/2))) 1/2))
      (probcc-reify/exact (let1 (coin-choice (probcc-coin 1/2))
                            (probcc-reify/exact (or coin-choice (probcc-coin 1/2))))))
   (⊦ equal? '(((V (((V #t) 3/4) ((V #f) 1/4))) 1))
      (probcc-reify/exact (let1 (coin-choice (probcc-letlazy (τ (probcc-coin 1/2))))
                            (probcc-reify/exact (or (coin-choice) (probcc-coin 1/2))))))
   (⊦ equal? '(((V (((V #t) 1))) 1/2) ((V (((V #t) 1/2) ((V #f) 1/2))) 1/2))
      (probcc-reify/exact (let1 (coin-choice (probcc-letlazy/nesting (τ (probcc-coin 1/2))))
                            (probcc-reify/exact (or (coin-choice) (probcc-coin 1/2))))))
   (⊦ equal? '(((V (((V #t) 1))) 1/2) ((V (((V #t) 1/2) ((V #f) 1/2))) 1/2))
      (probcc-reify/exact (let1 (coin-choice (probcc-letlazy/nesting (τ (probcc-coin 1/2))))
                            (probcc-reify/exact (or (probcc-coin 1/2) (coin-choice))))))
   ; cf. N:124-130; the tuples are evaluated from the right (D19).
   (⊦ equal? '(((V (#t #t (((V (#t #t #t)) 1/2) ((V (#f #t #t)) 1/2)))) 1/2)
               ((V (#f #f (((V (#t #f #f)) 1/2) ((V (#f #f #f)) 1/2)))) 1/2))
      (probcc-reify/exact
        (let* ((coin-choice (probcc-letlazy/nesting (τ (probcc-coin 1/2))))
               (v1 (probcc-reify/exact (let* ((v (probcc-coin 1/2))
                                              (c (coin-choice))
                                              (b (coin-choice)))
                                         (list v b c))))
               (b (coin-choice))
               (a (coin-choice)))
          (list a b v1))))
   ; two levels: the innermost reification asks its parent, which asks the outer world (the
   ; oracle is upstream's, run under OCaml with letlazy_nesting).
   (⊦ equal? '(((V (#t (((V (#t (((V #t) 1)) #t)) 1/2) ((V (#f (((V #t) 1)) #t)) 1/2)))) 1/2)
               ((V (#f (((V (#t (((V #f) 3/4) ((V #t) 1/4)) #f)) 1/2)
                        ((V (#f (((V #f) 3/4) ((V #t) 1/4)) #f)) 1/2)))) 1/2))
      (probcc-reify/exact
        (let* ((coin-choice (probcc-letlazy/nesting (τ (probcc-coin 1/2))))
               (v (probcc-reify/exact
                    (let* ((w (probcc-coin 1/2))
                           (inner (probcc-reify/exact (or (coin-choice) (probcc-coin 1/4))))
                           (c (coin-choice)))
                      (list w inner c))))
               (a (coin-choice)))
          (list a v))))
   `(doc (p "Nested inference (cf. N:15-130): the distribution of the distribution of a coin
	    that is fair or always true, " (code/inline "testn11") ", " (code/inline "testn21")
            " and " (code/inline "testn21'") "; with a lazy variable of the outer world, "
            (code/inline "probcc-letlazy") " is evaluated again inside, where the inner
	    reification starts with an empty memory, and the answer is wrong, as upstream's "
            (code/inline "testnl10") " (N:65-81); " (code/inline "probcc-letlazy/nesting")
            " asks the parent world instead, and " (code/inline "testnl11") ", "
            (code/inline "testnl12") " and " (code/inline "testnl13")
            " are right, also through two levels of nesting.")))

  ((test/wref _)
   (⊦ equal? '(((V (#t 1)) 1/2) ((V (#f 2)) 1/2))
      (probcc-reify/exact (let* ((r (probcc-wref-new 0))
                                 (c (probcc-coin 1/2)))
                            (probcc-wref-set! r (if c 1 2))
                            (list c (probcc-wref-get r)))))
   (⊦raises (exn hansei)
            (probcc-reify/exact (let1 (r (probcc-wref-new 0))
                                  (probcc-reify/exact (probcc-wref-get r)))))
   `(doc (p "World-local references (cf. TI:121-126, whose " (code/inline "WorldLocalMem")
            " is not shipped, deviation 17): a reference set in a world holds its value in that
	    world only.")))

  ((test/parameterize/memory _)
   (define q (make-parameter 'outer))
   (⊦ equal? '(((V (#f #t #t outer)) 1/3) ((V (#f #f #f outer)) 1/3) ((V (#t #t #t inner)) 1/6)
               ((V (#t #f #f inner)) 1/6))
      (probcc-reify/exact (let* ((x (probcc-letlazy (τ (probcc-coin 1/2))))
                                 (c (probcc-coin 1/3))
                                 (a (if c
                                        (parameterize ((q 'inner)) (list (x) (q)))
                                        (list (x) (q))))
                                 (b (x)))
                            (list c (car a) b (cadr a)))))
   (⊦ equal? '(((V (#t (((V (#t #t)) 1/2) ((V (#t #f)) 1/2)))) 1/2)
               ((V (#f (((V (#f #t)) 1/2) ((V (#f #f)) 1/2)))) 1/2))
      (probcc-reify/exact (let1 (c (probcc-coin 1/2))
                            (parameterize ((q c))
                              (list c (probcc-reify/exact (let1 (d (probcc-coin 1/2))
                                                            (list (q) d))))))))
   (⊦ equal? '(((V (#t #t outer)) 1/2) ((V (#f #f outer)) 1/2))
      (probcc-reify/exact (let* ((f (probcc-memo (λ (_) (probcc-coin 1/2))))
                                 (a (parameterize ((q 'inner)) (f 'k)))
                                 (b (f 'k)))
                            (list a b (q)))))
   (⊦ equal? 'outer (q))
   `(doc (p "The world-local memory is a variable changed with " (code/inline "set!")
            ", not a parameter, so it is not affected by the " (code/inline "dynamic-wind")
            " frames that a resume enters again: a lazy variable forced inside a "
            (code/inline "parameterize") " keeps its value outside, a nested reification inside
	    a " (code/inline "parameterize") " sees the binding, and a memo called inside a "
            (code/inline "parameterize") " answers the same value outside.")))

  ((test/map-dist _)
   (⊦ equal? '(((V 2) 0.25) ((V 1) 0.75))
      (probcc-map-dist length '(((V (a)) 0.25) ((V (b)) 0.5) ((V (a b)) 0.25))))
   (⊦raises (exn hansei) (probcc-map-dist length `(((C ,(τ '())) 1/2))))
   `(doc (p (code/inline "probcc-map-dist") " (cf. BL:456-460) maps the values of a flat table
	    and collates the equal results, in upstream's order, the results descending.")))

  ((test/prob-of _)
   (define table `(((C ,(τ '())) 1/2) ((V #f) 1/4) ((V #t) 1/4)))
   (⊦ equal? 1/4 (probcc-prob-of #t table))
   (⊦ equal? 0 (probcc-prob-of 'x table))
   ; OCaml's `=` (N:293): -0. and 0. are one value, as in the tables; NaN is never found.
   (⊦ equal? 1/2 (probcc-prob-of -0. '(((V 0.) 1/2))))
   (⊦ equal? 1 (probcc-prob-of -0. (probcc-reify/exact (if (probcc-coin 1/2) -0. 0.))))
   (⊦ equal? 0 (probcc-prob-of +nan.0 '(((V +nan.0) 1/2))))
   (⊦ equal? 0 (probcc-prob-of '(1 +nan.0) '(((V (1 +nan.0)) 1/2))))
   (⊦ equal? 1/2 (probcc-prob-of '#(a (0. b)) '(((V #(a (-0. b))) 1/2))))
   (⊦ equal? #t (probcc-at-least 1/4 #f table))
   (⊦ equal? #f (probcc-at-least 1/3 #f table))
   (⊦ equal? '(((V #t) 1))
      (probcc-reify/exact (let* ((biased (probcc-coin 1/2))
                                 (coin (τ (or (probcc-coin 1/2) biased))))
                            (probcc-at-least 0.3 #t (probcc-reify/exact/a coin)))))
   `(doc (p (code/inline "probcc-prob-of") " and " (code/inline "probcc-at-least")
            " (cf. N:292-304, PE:49-61): the mass of a value in a table, 0 when it is missing,
	    values compared as OCaml's = (-0. is 0., NaN is never found), and a branch not yet
	    explored is skipped (deviation 23); a coin that is fair or
	    always true gives true with probability at least 0.3 in every world (PE:62-66).")))

  ((test/nested/inference-about-inference _)
   (define (coin-at-least biased)
     (let1 (coin (τ (or (probcc-coin 1/2) (biased))))
       (probcc-at-least 0.3 #t (probcc-sample-rejection/a probcc-dist-selector 2 coin))))
   (⊦ equal? '(((V #t) 7/8) ((V #f) 1/8))
      (probcc-reify/exact (let1 (biased (probcc-coin 1/2))
                            (coin-at-least (τ biased)))))
   (⊦ equal? '(((V #t) 7/8) ((V #f) 1/8))
      (probcc-reify/exact (coin-at-least (probcc-letlazy/nesting (τ (probcc-coin 1/2))))))
   `(doc (p "Inference about inference (cf. PE:69-89; EPP p.4 l.345-379): a coin is fair or
	    always true; its probability of true, estimated by two rejection samples drawn with "
            (code/inline "probcc-dist-selector") ", that is through the non-determinism of the
	    outer reification, is at least 0.3 with probability 7/8, whether the bias is chosen
	    before or lazily, with " (code/inline "probcc-letlazy/nesting") "; the exact
	    estimate is in " (code/inline "test/prob-of") ".")))

  ((test/nested/thresholds _)
   (define ((test-nest p inferencer))
     (let1 (x (probcc-coin 1/2))
       (probcc-at-least p #t (inferencer (τ (or (probcc-coin 1/2) x))))))
   (define ((test-nest-lazy p inferencer))
     (let1 (x (probcc-letlazy/nesting (τ (probcc-coin 1/2))))
       (probcc-at-least p #t (inferencer (τ (or (probcc-coin 1/2) (x)))))))
   (define (sampler m) (probcc-sample-rejection/a probcc-dist-selector 2 m))
   (define (true-mass model) (probcc-prob-of #t (probcc-reify/exact/a model)))
   (define thresholds (probcc-map (λ (n) (/ n 10)) (iota 21 -5)))
   (⊦ equal?
      (probcc-map (λ (p)
                    (let ((expected-exact (cond ((<= p 1/2) 1) ((<= p 1) 1/2) (else 0)))
                          (expected-sample (cond
                                             ((<= p 0) 1)
                                             ((<= p 1/2) 7/8)
                                             ((<= p 1) 5/8)
                                             (else 0))))
                      (list p expected-exact expected-exact expected-sample expected-sample)))
                  thresholds)
      (probcc-map (λ (p)
                    (let* ((a (true-mass (test-nest p probcc-reify/exact/a)))
                           (b (true-mass (test-nest-lazy p probcc-reify/exact/a)))
                           (c (true-mass (test-nest p sampler)))
                           (d (true-mass (test-nest-lazy p sampler))))
                      (list p a b c d)))
                  thresholds))
   `(doc (p "The thresholds of N:307-345, with exact tenths from -0.5 to 1.5: the probability
	    that the estimate of a coin, fair or always true, gives true a mass of at least p, by
	    exact inference (1, 1/2, 0) and by two rejection samples (1, 7/8, 5/8, 0), the bias
	    chosen before or lazily.")))

  ((test/dist-selector _)
   (define (tflip2-noshared) (and (probcc-coin 1/2) (probcc-coin 1/2)))
   (⊦ equal? '(((V (((V #f) 1))) 3/4) ((V (((V #t) 1))) 1/4))
      (probcc-reify/exact (probcc-sample-rejection/a probcc-dist-selector 1 tflip2-noshared)))
   (⊦ equal? '(((V (((V #f) 1))) 9/16) ((V (((V #t) 1/2) ((V #f) 1/2))) 3/8)
               ((V (((V #t) 1))) 1/16))
      (probcc-reify/exact (probcc-sample-rejection/a probcc-dist-selector 2 tflip2-noshared)))
   (⊦ equal? '(((V (#t 4)) 3/4) ((V (#f 4)) 1/4))
      (probcc-reify/exact (probcc-dist-selector '((#t 3) (#f 1)))))
   `(doc (p (code/inline "probcc-dist-selector") " (cf. P:238-241) chooses through the
	    non-determinism of the enclosing reification, so a nested rejection sampler of n
	    samples answers the distribution of its estimates (N:145-160): one sample is the
	    identity, two give three tables; it answers the chosen entry and the total mass of
	    the entries, here as a distribution.")))

  ((test/first-success _)
   (⊦ equal? '(((V a) 1/2) ((V b) 1/2))
      (probcc-first-success `(((C ,(τ '(((V b) 1)))) 1/2) ((V a) 1/2))))
   (⊦ equal? '() (probcc-first-success '()))
   (⊦ equal? '() (probcc-first-success (probcc-reify/0 (τ (probcc-coin 1/2) (probcc-impossible)))))
   ; breadth first: the failing branch is dropped, the branch of 1/6 is opened before that of 1/2.
   (⊦ equal? '(((V #t) 1/6) ((V #f) 1/2))
      (probcc-first-success
        (probcc-reify/0 (τ (if (probcc-coin 1/3) (probcc-impossible) (probcc-coin 1/4))))))
   `(doc (p (code/inline "probcc-first-success") " (cf. I:55-64) opens the branches of a tree
	    breadth first, the masses multiplied along the way, until its first entry is a leaf,
	    and answers that tree; a tree without leaves gives the empty tree.")))

  ((test/soft-cut _)
   (⊦ equal? '(((V (q #t)) 1/2) ((V (q #f)) 1/2))
      (probcc-reify/exact (probcc-soft-cut (τ (probcc-coin 1/2)) (λ (x) (list 'q x)) (τ 'r))))
   (⊦ equal? '(((V r) 1))
      (probcc-reify/exact (probcc-soft-cut (τ (probcc-impossible)) (λ (x) (list 'q x)) (τ 'r))))
   `(doc (p (code/inline "probcc-soft-cut p q r") " (cf. LV:386-391, PS:156-161) is Prolog's
	    soft cut " (code/inline "p *-> q ; r") ": " (code/inline "q") " gets every value of "
            (code/inline "p") " when " (code/inline "p") " has one, " (code/inline "r")
            " runs otherwise.")))

  ((test/explore/shallow _)
   (define (dcoin-and-5) (dcoin-and 5))
   (define (geometric-7) (probcc-geometric/bounded 7 0.85))
   (define (shallow-then-explore k model)
     (probcc-explore #f (probcc-explore/shallow k (probcc-reify/0 model))))
   (⊦ equal? '((#t #t #t) (#t #t #t) (#t #t #t))
      (probcc-map (λ (model)
                    (let1 (exact (probcc-explore #f (probcc-reify/0 model)))
                      (probcc-map (λ (k)
                                    (let1 (got (shallow-then-explore k model))
                                      (and (equal? (map car exact) (map car got))
                                           (every (λ (a b) (< (abs (- (cadr a) (cadr b))) 1e-15))
                                                  exact got))))
                                  '(0 1 3))))
                  (list lawn geometric-7 dcoin-and-5)))
   ; collated values first (descending pmap-compare order), then the branches in reverse order,
   ; unsorted; the values below a branch above maxdepth are collated, and the branch answers the
   ; rest of its explored subtree, normalized, its mass times the total.
   (define t (τ '(((V b) 1/4) ((V c) 1/4))))
   (⊦ equal? `(((V b) 1/2) ((V a) 1/4) ((C ,t) 1/2))
      (probcc-explore/shallow 0 `(((V a) 1/8) ((V b) 1/2) ((C ,t) 1/2) ((V a) 1/8))))
   (define u (τ `(((V c) 1/4) ((V b) 1/4) ((C ,t) 1/2))))
   (⊦ equal? `(((C ,t) 1))
      (match/first (probcc-explore/shallow 1 `(((C ,u) 1)))
        ((((V c) 1/4) ((V b) 1/4) ((C ,t*) 1/2)) (t*))))
   `(doc (p (code/inline "probcc-explore/shallow") " (cf. I:35-53) explores a tree to a depth
	    without flattening it: exploring the result fully gives the exact distribution of
	    the lawn model, of " (code/inline "geometric_bounded 7 0.85") " and of "
            (code/inline "dcoin_and 5") ", whatever the depth, up to rounding; the entries are
	    upstream's, in its order, which the importance sampler's selections follow.")))

  ((test/explore/bounded _)
   (define (u) (probcc-reify/0 (τ (unless (dcoin-and 10) (probcc-impossible)))))
   (⊦ equal? '(0.99949400615938178 0.99949400615938178)
      (probcc-explore/bounded 1 (probcc-reify/0 (τ (when (> (probcc-geometric/bounded 7 0.85) 3)
                                                     (probcc-impossible))))))
   (⊦ equal? '(0 1.) (probcc-explore/bounded 1 (u)))
   (⊦ equal? '(0 0.049999999999999989) (probcc-explore/bounded 3 (u)))
   (⊦ equal? '(9.7656249999999776e-14 9.7656249999999776e-14) (probcc-explore/bounded 5 (u)))
   `(doc (p (code/inline "probcc-explore/bounded") " (cf. I:71-109) bounds the probability of
	    evidence, bit for bit as SM:80-82 and SM:469-480: " (code/inline "geometric_bounded")
            " is solved with a queue of one branch; for " (code/inline "dcoin_and 10")
            " the bounds close as the queue grows, and the lower bound is the exact 0 while no
	    leaf is reached (deviation 26).")))

  ((test/sample/look-ahead-exact _)
   ; cf. DSL p.20 l.866-909 and SL:640-655: lazy flips, with letlazy, observed one by one.
   (define (flips/lazy p n)
     (if (zero? n)
         '()
         (let* ((x (probcc-letlazy (τ (probcc-coin p))))
                (xs (probcc-letlazy (τ (flips/lazy p (sub1 n))))))
           (cons x xs))))
   (define (trues/lazy n xs)
     (match/first xs
       (() (zero? n))
       ((,x . ,xs*) (and (x) (trues/lazy (sub1 n) (xs*))))))
   (define (flips p n)
     (if (zero? n)
         '()
         (let* ((x (probcc-coin p))
                (xs (flips p (sub1 n))))
           (cons x xs))))
   (⊦ equal? '(((V ok) 9.5367431640625e-07))
      (probcc-sample-importance (probcc-random-selector 17) 1
        (probcc-when (trues/lazy 20 (flips/lazy 0.5 20)) 'ok)))
   (⊦ equal? '(((V ok) 1/4096))
      (probcc-reify/exact (probcc-when (every identity (flips 1/2 12)) 'ok)))
   `(doc (p "With lazy flips (DSL p.20 l.866-909), each flip is observed right after it is
	    made, so the look-ahead of importance sampling finds the probability 2^-20 of 20
	    true flips exactly, with one sample; eager flips are produced before any is observed,
	    and exact inference explores the whole tree.")))

  ((test/sample/pre-explored _)
   (define (no-selector choices) (error "a pre-explored tree draws" choices))
   (define (tflip2-shared) (let1 (v (probcc-coin 0.5)) (and v v)))
   (define (tflip2-noshared) (and (probcc-coin 0.5) (probcc-coin 0.5)))
   ; `let earthquake = flip 0.01 and burglary = flip 0.1`, left to right (D19).
   (define (talarm)
     (let* ((earthquake (probcc-coin 0.01))
            (burglary (probcc-coin 0.1)))
       (if earthquake
           (if burglary (probcc-coin 0.99) (probcc-coin 0.2))
           (if burglary (probcc-coin 0.98) (probcc-coin 0.01)))))
   (define (tiblt2)
     (let* ((x (probcc-observe (λ (r) (eq? (car r) 'a))
                               (τ (probcc-distribution '(((a b) 0.01) ((a c) 0.02) ((d e) 0.97))))))
            (y (case (cadr x)
                 ((b) (probcc-distribution '((#t 0.9) (#f 0.1))))
                 ((c) (probcc-distribution '((#t 0.6) (#f 0.4))))
                 (else (probcc-distribution '((#t 0.2) (#f 0.8)))))))
       y))
   (define (tfooibl3)
     (probcc-observe (λ (x) (eq? x #t))
                     (τ (eq? 'a (if (probcc-distribution '((#t 0.1) (#f 0.9)))
                                    (probcc-distribution '((a 0.7) (b 0.3)))
                                    (probcc-distribution '((a 0.2) (b 0.8))))))))
   (define (tmusictr42)
     (cadr (probcc-observe (λ (r) (eq? (car r) 'a))
                           (τ (if (probcc-coin 0.01)
                                  (let1 (q (probcc-coin 0.3)) (list 'a q))
                                  (list 'b #t))))))
   (⊦ equal? '(((V #t) 0.5) ((V #f) 0.5))
      (probcc-sample-importance/a no-selector 100 tflip2-shared))
   (⊦ equal? '(((V #f) 0.75) ((V #t) 0.25))
      (probcc-sample-importance/a no-selector 100 tflip2-noshared))
   (⊦ equal? '(((V #f) 0.89128000000000007) ((V #t) 0.10872000000000001))
      (probcc-sample-importance/a no-selector 100 talarm))
   (⊦ equal? '(((V #t) 0.021000000000000001) ((V #f) 0.0090000000000000011))
      (probcc-sample-importance/a no-selector 100 tiblt2))
   (⊦ equal? '(((V #t) 0.25)) (probcc-sample-importance/a no-selector 100 tfooibl3))
   (⊦ equal? '(((V #f) 0.0069999999999999993) ((V #t) 0.0030000000000000001))
      (probcc-sample-importance/a no-selector 1 tmusictr42))
   `(doc (p "The importance sampler first explores the tree to depth 3 and looks one step
	    ahead (cf. I:188-197): these models of SM:13-206 are solved before any sample is
	    drawn, so their results are exact, bit for bit as upstream's, and the selector,
	    which here raises, is never called.")))

  ((test/sample/dist-selector-importance _)
   (define (inferencer m) (probcc-sample-importance/a probcc-dist-selector 2 m))
   (⊦ equal? '((#t (F_notall)))
      (probcc-map (λ1-match/first ((,u ,a) (list (= u 0) a)))
                  (argmax (probcc-reify/exact/a (agent-S-model inferencer)))))
   `(doc (p "Bounded-rational theory of mind (cf. IN:344-426): agent S chooses what to say,
	    knowing that agent A estimates the state of the world by importance sampling of two
	    samples, through " (code/inline "probcc-dist-selector") "; saying only "
            (i "not all") " gives the maximal expected utility, -0.")
         ,@(shown 'agent-A-model 'agent-S-model)))

  ((test/statistics _)
   (⊦ equal? '((a 1/5 0.081649658092772609))
      (probcc-statistics 1 3 (λ (s) (list (probcc-value (/ s 10) 'a)))))
   (⊦ equal? #t
      (flonum? (caddr (car (probcc-statistics 1 3 (λ (s) (list (probcc-value 0.1 'a))))))))
   (⊦ equal? '((b 3/4 0.) (a 1/4 0.))
      (probcc-statistics 1 2 (λ (s) '(((V a) 1/4) ((V b) 3/4)))))
   (⊦raises (exn hansei) (probcc-statistics 1 1 (λ (s) `(((C ,(τ '())) 1)))))
   `(doc (p (code/inline "probcc-statistics") " (cf. I:279-294) gives the mean and the
	    standard deviation of the mass of each value over the samplers of a range of seeds,
	    by descending mean (deviation 21); the deviation is a flonum, NaN when rounding makes
	    the variance negative, as upstream (deviation 27).")))

  ((test/verbose _)
   (⊦ equal? "" (with-output-to-string
                  (τ (probcc-sample-rejection (probcc-random-selector 1) 10 (probcc-coin 1/2)))))
   (⊦ equal? "rejection_sample: done 10 worlds\n"
      (with-output-to-string
        (τ (parameterize ((probcc-verbose #t))
             (probcc-sample-rejection (probcc-random-selector 1) 10 (probcc-coin 1/2))))))
   (⊦ equal? "sample_importance: done 10 worlds\n"
      (with-output-to-string
        (τ (parameterize ((probcc-verbose #t))
             (probcc-sample-importance (probcc-random-selector 1) 10 (dcoin-and 10))))))
   `(doc (p "The samplers print their " (code/inline "done") " lines (I:131, 186) only when "
            (code/inline "probcc-verbose") " is true (deviation 14).")))

  ((test/variable-elimination _)
   (define (flips-xor n)
     (if (= n 1)
         (probcc-coin 1/2)
         (let* ((r (flips-xor (sub1 n)))
                (f (probcc-coin 1/2)))
           (not (eq? f r)))))
   (define (flips-xor* n)
     (if (= n 1)
         (probcc-coin 1/2)
         (let* ((r (probcc-reflect (probcc-reify/exact (flips-xor* (sub1 n)))))
                (f (probcc-coin 1/2)))
           (not (eq? f r)))))
   (define xor-bucket
     (λ-probcc-bucket (n) (if (= n 1)
                              (probcc-coin 1/2)
                              (let* ((r (xor-bucket (sub1 n)))
                                     (f (probcc-coin 1/2)))
                                (not (eq? f r))))))
   (⊦ equal? '(((V #t) 1/2) ((V #f) 1/2)) (probcc-reify/exact (flips-xor 10)))
   (⊦ equal? 1024 (probcc-leaves (probcc-reify/0 (τ (flips-xor 10)))))
   (⊦ equal? '(((V #t) 1/2) ((V #f) 1/2)) (probcc-reify/exact (flips-xor* 10)))
   (⊦ equal? 4 (probcc-leaves (probcc-reify/0 (τ (flips-xor* 10)))))
   (⊦ equal? '(((V #t) 1/2) ((V #f) 1/2)) (probcc-reify/exact (xor-bucket 10)))
   `(doc (p "Variable elimination (DSL p.17 l.751-771; X:305-329): the xor of 10 flips explores
	    the 1024 leaves of a full binary tree, while reifying and reflecting the recursive
	    call leaves 4 at the top; the bucket elimination of DSL p.18 l.776-783 memoizes the
	    reified calls.")))

  ((test/drunk-coin _)
   (⊦ equal? '(((V #f) 0.052631578947363269) ((V #t) 9.7656249999999776e-14))
      (probcc-reify/exact (dcoin-and 10)))
   `(doc (p "The drunk coin of SM:224-238 and DSL p.19 l.814-817: the exact distribution of
	    the conjunction of 10 tosses, bit for bit as upstream.")
         ,@(shown 'drunk-coin 'dcoin-and)))

  ((test/reify/part _)
   ; `flip 0.5 :: a_list ()`: the constructor's arguments from the right (D19).
   (define (a-list)
     (if (probcc-coin 1/2)
         '()
         (let* ((t (a-list))
                (h (probcc-coin 1/2)))
           (cons h t))))
   (⊦ equal? '((() 1/2) ((#t) 1/8) ((#f) 1/8)) (probcc-reify/part 3 a-list))
   (⊦ equal? '((#t 1/2) (#f 1/2)) (probcc-reify/part #f (τ (probcc-coin 1/2))))
   `(doc (p (code/inline "probcc-reify/part") " (cf. LV:68-70) runs a model to a depth and
	    keeps its values, as (v p) pairs: the boolean lists of RT p.5 l.340-365, of which
	    depth 3 reaches the empty list and the lists of one element.")))

  ((test/bind _)
   (define (f x) (probcc-reify/0 (τ (let1 (c (probcc-coin 1/3)) (list x c)))))
   (define (m1) (probcc-coin 1/2))
   (define (m2) (let* ((a (probcc-coin 1/2)) (b (probcc-coin 1/4))) (and a b)))
   (⊦ equal? (probcc-reify/exact (probcc-reflect (f (m1))))
      (probcc-explore #f (probcc-bind (probcc-reify/0 m1) f)))
   (⊦ equal? (probcc-reify/exact (probcc-reflect (f (m2))))
      (probcc-explore #f (probcc-bind (probcc-reify/0 m2) f)))
   (⊦ equal? (probcc-explore #f (f 'x)) (probcc-explore #f (probcc-bind (probcc-unit 'x) f)))
   `(doc (p (code/inline "probcc-bind") " (cf. DSL p.10 l.415-425 " (code/inline "pv_bind")
            ") with " (code/inline "probcc-unit") " is the search-tree monad, whose laws hold up
	    to the equivalence of trees explored fully.")))

  ((test/timeit-runner-memory _)
   (define v #f)
   (define out (with-output-to-string (τ (set! v (probcc-timeit (τ 42))))))
   (⊦ equal? 42 v)
   (⊦ equal? "\nTime spent: " (substring out 0 13))
   (⊦ equal? " sec\n" (substring out (- (string-length out) 5)))
   (⊦ equal? '(3 3) ((probcc-sample-runner/times 3) 0 add1))
   (⊦ equal? 'd (probcc-mem-ref (probcc-mem-new-loc) probcc-mem-empty (τ 'd)))
   (⊦raises (exn hansei) (probcc-mem-ref (probcc-mem-new-loc) probcc-mem-empty))
   `(doc (p (code/inline "probcc-timeit") " (cf. I:303-308) prints the processor time a thunk
	    takes and answers its value; " (code/inline "probcc-sample-runner/times")
            " (cf. I:142-146) applies a step n times; " (code/inline "probcc-mem-ref")
            " (cf. P:52-54) calls its default thunk on a missing location, and raises without
	    one.")))

  )

; test/parameterize resumes a choice inside a `parameterize`: when a resume did not rewind the
; `dynamic-wind` frames (a `letcc` on continuation-graft), it left the string port that captured a
; test's output as the current output port, and the run ended with rc 0 and nothing printed, the
; summary included, hiding every failure of the suite; so check that the output port is back.
(import (only (chicken base) exit))

(define stdout (current-output-port))

(let1 (r (unittest/✓ hanseitest))
  (unless (eq? stdout (current-output-port))
    (current-output-port stdout)
    (write (unittest/result-summary r))
    (newline)
    (display "FAIL: the suite ended with its output still captured by a test\n")
    (exit 1)))
