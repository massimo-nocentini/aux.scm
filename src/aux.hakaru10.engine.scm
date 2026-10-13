;; (aux hakaru10 engine) -- the incremental Metropolis-Hastings engine of (aux hakaru10), a
;; CHICKEN port of Oleg Kiselyov's Hakaru10 (https://okmij.org/ftp/kakuritu/Hakaru10/index.html).
;; Internal: (aux hakaru10) is the public interface.
;;
;; Mirrors (paths relative to aux.scm/upstream/okmij-hakaru10/, citation keys as in the docs):
;;   Mem  Memory.hs (https://okmij.org/ftp/kakuritu/Hakaru10/Memory.hs): first-class memory,
;;        folded into the chain's vector of nodes;
;;   M    Metropolis.hs (https://okmij.org/ftp/kakuritu/Hakaru10/Metropolis.hs), M:110-977:
;;        node states, MCState, node creation, dependencies, propagation, branches (ifnode,
;;        update_entry/update_exit, (de)activation), resample_trace, transit and the driver;
;;   S    Syntax.hs (https://okmij.org/ftp/kakuritu/Hakaru10/Syntax.hs), the internals of
;;        S:84-279: SExp, eval_nctor, with_node_ctor, to_node, if_.
;; Not ported: M:1174-1415 (the old Wingate-style Measure code), and the example programs that
;; Metropolis.hs writes on the raw trace API (with_node, next, ifnode), which (aux hakaru10)
;; does not export; each has a surface-syntax counterpart in test/hakaru10*.scm:
;;   M:320-361 upp1/upp1r (live upstream; its surface-syntax text M:327-333 is commented
;;        out) and M:387-397 upp2/upp2r (commented out upstream): the propagation-order note
;;        M:319-385, implemented by the update queue below (smallest address first) and
;;        checked on every step by propagate!'s "updating the already updated" assertion
;;        (M:307-308, a node updated twice; a Dirac one, whose tstamp the update advances) and
;;        doupdate's "dependency violation!" (S:240-243, a node updated before its parents);
;;        a wrong order also makes the reference evaluator of `#:check` (test/hakaru10.scm,
;;        B4) find a stale value.  Case B8 of test/hakaru10.scm runs upp1 and upp2 (a and b
;;        both feeding the non-Dirac d), transcribed into models, and upp1 as M:336-341
;;        writes it, on this module's trace API (upp1/raw, with the Dirac node ab), under
;;        `#:check`, records the updates of every step (d once, after b and c, and after ab
;;        in upp1/raw), and asserts the incrementality of P §4.2 (l.771-788, pdep:
;;        resampling x does not update z), which the reference evaluator, comparing final
;;        states, cannot see;
;;   M:529-535 ex1/ex1r: the same model as S:282-289 niceex1 (test/hakaru10.scm);
;;   M:537-545 prog_mult_conditions/exmcr/exmcr1: S:291-301 nice_prog_mult_conditions;
;;   M:547-562 ex2/prog_test1: commented out upstream;
;;   M:798-822 exgrass/exgrass': nested ifnodes over Dirac roots, the shape of E:284-296
;;        grass (commented out upstream; grass/if in test/hakaru10.scm, whose findings record
;;        the samples of exgrass's transcription), not of the noisy-or grass of E:299-304;
;;   M:825-841 exbr2/exbr3: line for line E:106-120 pcond1/pcond2 (test/hakaru10-exact.scm);
;;   M:1029-1037 diractest: test/laws/diractest (A2) in test/hakaru10.scm.
;; See P §4 (l.641-869) for the design: the trace DAG (§4.1), incremental recomputation (§4.2),
;; the Dirac distribution (§4.3) and branching (§4.4).
;;
;; Hakaru10 code version: November 2016 (W:52-53); web page last updated July 14, 2018 (W:179).
;; Paper: O. Kiselyov, "Probabilistic Programming Language and its Incremental Evaluation",
;; APLAS 2016, LNCS 10017, DOI 10.1007/978-3-319-47958-3_19 (P = design.txt / design.pdf).
;;
;; Construction (plan §15, C32-C36).  `Model a = MCMCM (SExp a)` (S:82), `MCMCM a = State
;; MCState a` (M:200): a model is a computation of the State monad of (aux category state),
;; whose state is the chain, threaded as is and mutated in place (C34; C5 for steps).  The
;; construction functions mirror S:137-279, M:233-526 and M:641-686 by name, each a State
;; action.  Nodes are created when an action runs, in bind order (C35; C18: creation order is
;; topological, P l.791-794), so building an action, in an argument position or inside `map`,
;; creates none.  Node updates and the MH step stay imperative (C5).  `condition` and `lift`
;; are pure: conditioning makes an observed distk (S:174-193), which `dist` makes a node of,
;; and lifting makes an SExp (S:99-116).
;;
;; ENGINE API (the public module (aux hakaru10) and (aux hakaru10 exact) use only what is listed
;; here).  A node is a record (the trace DAG's vertex); its address, from 1, is its creation
;; order.  `ch` is a chain, `n` a node.  A TraceM (M:202) is a State action answering a node, a
;; Model (S:82) one answering an SExp.
;;
;;   Construction (State monad, S:82-279, M:233-526, M:641-686)
;;     (model? x)              whether x is a Model, a State computation (C32).
;;     (check-model m who)     a `construction` error, located at who, unless m is a Model;
;;                             answers m.
;;     (modify! (ch) body ...) the action that runs body on the chain ch, mutated in place.
;;     (hakaru10-fail kind who msg irritant ...)   the action that raises the error when it runs.
;;     (lazy-model who thunk)  the Model that calls thunk when it runs and checks that thunk
;;                             answered a Model (C36, for the public `if_`).
;;     (new-nref n) (new-dependency parent child)  M:233-243, M:246-250 (the latter with the
;;                             branch-escape check)
;;     (register-dependencies leaves child)        S:255-261
;;     (create-node update dk [role parents fn])   M:404-419: the only draw of construction
;;     (with-node-none dk)     M:421-423
;;     (with-node-tree leaves fn [who]) (with-node ndr f [who])   S:224-228, M:451-456
;;     (with-node-ctor e df [who lazy])            S:218-228 -> TraceM
;;     (to-node e [who])       S:210-214 -> TraceM (a shared node must be in scope)
;;     (dist d arg ...)        S:137-151: d a distk (no args) or a distk constructor (a
;;                             procedure) of SExps/Vals; a Model answering the node.  d
;;                             applied to the wrong number of arguments is a `construction`
;;                             error (deviation 9).
;;     (diracN e)              S:161-169, a Model.
;;     (if_* test th el)       S:275-279, th and el Models; a Model answering the exit node.
;;     (ifnode entry th el)    M:641-686: entry a node, th and el TraceMs; answers the exit.
;;     (deactivate lo hi)      M:696-717 at construction (M:659).
;;     (in-branch entry polarity m)   m run in the scope extended with (entry . polarity).
;;     (model->trace m who)    S:197 `m >>= to_node`: the TraceM of a Model.
;;     (pair a b) (collect es) S:134-135, B1:113-114: one SExp of a pair, of a list.
;;     lazy-lifting            parameter: #t while the State action of an inactive branch runs
;;                             (in-branch); lift* then defers the errors of its function (S:100,
;;                             M:99).  build-chain starts every build with it #f.
;;     (condition/external obs d)  S:174-193, with "observation must be external": obs
;;                             neither is nor contains an SExp or a Model (the public
;;                             `condition`).
;;   SExp (S:85-93): an SExp is a node (NRef), an nt (NT) or any other value (Val)
;;     (sexp? x)  (node? x)  (nt? x) (nt-leaves e) (nt-fn e): leaves is a vector of distinct
;;                             nodes, fn (λ (values-vector) value), values aligned with leaves.
;;                             A procedure is a plain Val (S:107 `Val f`).
;;     (sexp-chain e) (sexp-leaves e) (sexp-value e)   owner, leaves (a vector), value
;;                             (eval_nctor, S:201-208).
;;     (leaf-values leaves)    the values-vector of a vector of nodes (reeval_tree, S:264-273).
;;     (deferred? x) (force-value x) (deferred-args x)   the value of a diracN node computed
;;                             while it is inactive may be a deferred value (an error, raised
;;                             again when it is forced; M:99, M:120-129): force-value answers x,
;;                             or forces it; deferred-args are the values it was computed from,
;;                             which construction-check compares.
;;     (lift* f es [checked?]) Applicative lifting (S:99-116): a Val if no e is an sexp (f
;;                             applied now), otherwise one fused nt; f's answer must hold no
;;                             SExp nor Model: applied to Vals at any depth, for an nt at any
;;                             depth on its first run and at the top on the next ones (unless
;;                             checked? is #f).
;;   The imperative core of construction
;;     (add-dependency! parent child)                 M:246-250 (with the branch-escape check)
;;     (check-owner ch e who)  e must be a Val or an sexp of ch (`construction`).
;;     (chain-scope ch) (chain-scope-set! ch s)       the construction scope, a list of
;;                                                    (entry-node . polarity), innermost first.
;;   Branches (M:641-794, S:275-279)
;;     (deactivate! ch lo hi) (activate! ch lo hi)   M:688-751, over the addresses (lo, hi]
;;     (node-branch n)         of an entry or an exit, the branch record of its if_ (else #f):
;;                             (branch-entry b) (branch-then-result b) (branch-else-result b)
;;                             nodes, (branch-then-range b) (branch-else-range b) half-open
;;                             address ranges (lo . hi), (branch-active-result b) the result
;;                             the entry's current value selects.  An exit's node-fn is #f and
;;                             its node-parents are entry, active result, inactive result,
;;                             without duplicates.
;;   Nodes (static fields are written only during construction)
;;     (chain-node ch a)       the node at address a;  (chain-count ch) the last address;
;;     (chain-fold kons knil ch [lo hi])  (kons node acc) over the nodes with addresses in
;;                             (lo, hi], ascending (all of them by default).
;;     (node-chain n) (node-addr n) (node-kind n): resampleable | observed | dirac
;;     (node-role n): root | plain | entry | exit      (node-parents n) vector of nodes
;;     (node-fn n) (node-update n): #f or (λ (n) (values nstate' extra-nodes))
;;     (node-scope n) (node-deps n) list of children (node-branch n)
;;     (node-state n) -> nstate (val ll dst tstamp)    (node-value n)
;;     (make-nstate val ll dst tstamp)   a new state, for the reference evaluator and the
;;                             kernel residual of (aux hakaru10 exact) (recomputed-state).
;;     (check-observation n obs)   deviation 9: a `conditioning` error unless obs is the
;;                             observation the observed node n was built with (doupdate,
;;                             recomputed-state).
;;     (node-inactive n) count, 0 = active  (node-inactive? n)  (node-delayed n)  (node-pending n)
;;     (node-queued n)         #t while n is in the update queue (not journaled).
;;     (set-node-state! n s) (set-inactive! n k) (set-delayed! n flag) (set-pending! n flag):
;;                             journaled setters; node-state-set! etc. are the raw ones.
;;     (chain-now ch) (chain-now-set! ch t) (chain-llcum ch) (chain-llcum-set! ch x)
;;     (chain-card-change ch) (chain-card-change-set! ch k) (chain-semantics ch) (chain-rng ch)
;;                             the setters are raw: the kernel residual of (aux hakaru10 exact)
;;                             uses them for its forced step and its load-state!.
;;     (chain-samp ch) vector of the resampleable nodes, (chain-sampleable ch) its list.
;;   MH (M:262-317, M:897-977)
;;     (modify-node! n s') -> children to enqueue (journaled store, Δ accounting)
;;     (enqueue-all! ch nodes) (propagate! ch)   propagate! ends with (settle-pending! ch):
;;                             the weights of the activated observed nodes, in 'extended mode
;;                             (see Semantics below), and the deferred values of the activated
;;                             Dirac nodes, forced (deviation 11, activate!)
;;     (resample-trace! ch) -> fwd-bwd      (apply-proposal! n v ll fb) -> fb
;;     (log-acceptance-ratio ch sampleables card-old fb)   M:929-934
;;     (rejected-ll? x)        M:853-854: whether x is infinite (a NaN is not).
;;     (transit! ch) -> accepted? (#t for an accepted step or a no-op step); after an error
;;                             that escaped a step, the next one rolls it back first
;;     (journal-begin! ch) (journal-commit! ch) (journal-rollback! ch)
;;     (chain-step-hook ch) (chain-step-hook-set! ch hook): hook is #f or
;;                             (λ (ch accepted? delta)), called after every step, once the
;;                             step is committed or rolled back: delta is the step's Δ (llcum;
;;                             -inf for a C22 rejection, 0. for a no-op step), and the chain
;;                             holds the state the next step starts from.
;;     hakaru10-debug          parameter: when true, transit! traces each step to stderr.
;;   Semantics (plan §6.8, C13, C22): (chain-semantics ch) is 'upstream (the default,
;;   bit-identical to upstream) or 'extended (deviation 8), where MH runs on the space extended
;;   with the inactive resampleable nodes as auxiliary variables with their prior, removing
;;   upstream's reactivation bias (M:1159-1165).  In 'extended mode:
;;     - no update is delayed: inactive nodes are updated too (M:298-303 skipped), and an
;;       inactive resampleable node's LL change counts in Δ, an inactive observed node's not;
;;     - (de)activation still moves only the observed weights (M:688-751), but an activated
;;       observed node's weight is added when propagation ends, with its LL then
;;       (settle-pending!; (node-pending n) while it waits);
;;     - the "parent or the node itself are inactive" assertion (M:469-473) is dropped;
;;     - C22: an (exn hakaru10) `parameter` error raised by an inactive node's update rejects
;;       the step (Δ = -inf); raised while an inactive node is built, it rejects the initial
;;       trace (build-chain answers result #f, llcum -inf; initial-step builds again).  So does
;;       any other error that an inactive diracN node would defer (deferrable?: a plain Scheme
;;       error, such as `car` of '()), whether it is raised by the node's own parameters, an
;;       NT applying a partial function, or by forcing a deferred value read through a diracN
;;       node: the two models are the same upstream (diracN only shares, S:164-169).  An (exn
;;       hakaru10) error of another kind (`branch`, `construction`, ...) propagates;
;;     - the proposal, the cardinality term (M:929-934) and the retry are unchanged.
;;     (extended? ch)          whether ch runs the 'extended semantics.
;;     (chain-extended-ll ch)  the extended target's LL from scratch: every resampleable
;;                             node's LL plus every active observed node's LL.
;;     (inactive-scope? ch)    whether a node created now is in an inactive branch.
;;   Driver (M:856-906, S:196-197); prog is a TraceM (model->trace)
;;     (build-chain prog src semantics) -> (values ch result); result is the node whose value
;;                             is sampled, #f for a build rejected in 'extended mode (C22),
;;                             where llcum is chain-extended-ll.
;;     (initial-step limit prog src semantics) -> (values ch result limit')  M:882-891
;;     (mcmc-generator limit prog #!key (seed 17) (rng #f) (semantics 'upstream)
;;                     (step-hook #f) (build-hook #f))
;;                             #f if no initial trace is found in `limit` builds, else a
;;                             generator: a thunk answering the initial trace's value, then one
;;                             value per MH step.  build-hook, if given, is called once as
;;                             (build-hook ch) when the generator is made.  A limit that is
;;                             not an exact integer (deviation 9) or is negative (deviation
;;                             10) is a `construction` error.
;;     (mcmc-fold kons knil limit prog #!key ...)   (kons sample acc) over the `limit` first
;;                             samples, in chain order (the same keys).

(module (aux hakaru10 engine) *

  (import scheme
          (only (scheme base) vector-map vector-for-each make-parameter exact-integer?)
          (chicken base)
          (chicken fixnum)
          (chicken flonum)
          (only (chicken format) fprintf)
          (only (chicken port) with-output-to-port)
          (only (chicken sort) sort!)
          (only (chicken condition) make-property-condition signal condition-case
                with-exception-handler condition-predicate condition-property-accessor)
          (only srfi-1 any every find delete-duplicates append-map list-index iota)
          (only srfi-69 make-hash-table hash-table-exists? hash-table-set!)
          (aux base)
          (only (aux category state) category-state? state run-state eval-state exec-state get
                gets modify return >>=)
          (only (aux category monad state) do/monad ▷ when/monad)
          (aux hakaru10 random)
          (aux hakaru10 distribution))

  ; No definition of this module is ever redefined, so the compiler may inline the record
  ; accessors and the small helpers into the hot paths (propagation, (de)activation, the step).
  (declare (local))

  ;; ---------------------------------------------------------------------------------------
  ;; Node states (M:119-143) and nodes (M:120-143 with the per-node maps of M:165-197)
  ;; ---------------------------------------------------------------------------------------

  ; cf. M:120-143 `data Node a`, the dynamic part: `val`, `ll`, `dst` (the current rdistr, #f
  ; for Dirac) and `tstamp`, the step at which `val` last changed ("Changes in LL do not
  ; advance the timestamp", M:127-128).  States are immutable and replaced as a whole, as
  ; `Mem.modify_ref` (Mem:36-37) does; `kind` and `update` are static fields of the node;
  ; `ident` (M:123) is the node's address.
  (define-record nstate val ll dst tstamp)

  ; cf. M:145-148 `instance Show (Node a)` (debug printer only).
  (set-record-printer! nstate
    (λ (s port)
      (fprintf port "#<nstate ~s ll ~s t ~s>" (nstate-val s) (nstate-ll s) (nstate-tstamp s))))

  ; cf. M:120-143 `data Node a` together with the per-node maps of MCState (M:168-183) and
  ; Mem:47-50 `data Memory`: a vertex of the trace DAG (P §4.1 l.721-736), which is also the
  ; NRef of an SExp (S:87, `NodeRef a`).  Static fields, written only during construction:
  ;   chain      the owner: a node of another chain (a previous mcmC, a discarded initial-retry
  ;              build, M:882-892) is rejected (deviation 10);
  ;   addr       the address, from 1: creation order, which is topological (Mem:59-63);
  ;   kind       resampleable | observed | dirac (M:125);
  ;   role       root (no update) | plain | entry | exit (plan §5.2);
  ;   parents    vector of the parent nodes, the leaves of its SExp (S:255-261);
  ;   fn         (λ (values-vector) distk) of a plain or entry node (`df . g`, S:226), the
  ;              constant distk of a root, #f for an exit;
  ;   update     #f or (λ (n) (values nstate' extra-nodes)) (M:131-142); an entry's is
  ;              replaced once, at construction (M:679-685);
  ;   scope      the construction scope, a list of (entry . polarity), innermost first;
  ;   deps       list of the children, with set semantics (M:170-172);
  ;   branch     of an entry or an exit, the branch record of its if_ (M:650-668), else #f.
  ; Dynamic fields, journaled during a step:
  ;   state      the nstate (M:168 `nodes`, Mem:48 `refs`);
  ;   inactive   the deactivation count, 0 = active (M:174-179);
  ;   delayed    #t while its update is delayed (M:181-183);
  ;   pending    #t while an observed node activated in this step waits for its weight
  ;              ('extended only, see activate!);
  ; and, not journaled, `queued`: #t while it is in the update queue (M:283-286).
  ; `refresh_nref` (M:253-256, Mem:82-84) is a field read: nodes are shared, never copied,
  ; so there is nothing to refresh, and `unsafeCoerce` (Mem:5-13) is not needed.
  (define-record node chain addr kind role parents fn update scope deps branch
                      state inactive delayed pending queued)

  ; (debug printer only) #<node addr kind>.
  (set-record-printer! node
    (λ (n port) (fprintf port "#<node ~a ~a>" (node-addr n) (node-kind n))))

  ; cf. M:161-162 `nref_val`.
  (define (node-value n) (nstate-val (node-state n)))

  ; cf. M:298 `M.member na (inactive s)` (a count of 0 is absence).
  (define (node-inactive? n) (fx> (node-inactive n) 0))

  ;; ---------------------------------------------------------------------------------------
  ;; The chain: MCState (M:165-197)
  ;; ---------------------------------------------------------------------------------------

  ; cf. M:165-197 `data MCState` and Mem:47-50 `data Memory`.  Fields:
  ;   rng          the rng source (M:166 `seed`); it is never rolled back (M:906);
  ;   semantics    'upstream or 'extended (plan §6.8);
  ;   now          the current step, from 1 (M:167, M:876);
  ;   nodes        address -> node, a vector that grows by doubling (Mem:59-63 `new`); count
  ;                is the last address (Mem:49);
  ;   llcum        total LL during construction, the LL difference during a step (M:185-187,
  ;                M:915);
  ;   sampleable   list of the resampleable nodes, prepended at creation (M:189-190, M:239);
  ;   samp         its vector, frozen after construction, samp[i] = (list-ref sampleable i);
  ;   card-change  activated minus deactivated resampleable nodes, cumulative (M:193-194);
  ;   queue        the update queue (M:273-295), made by chain-freeze!;
  ;   pending      the nodes activated during the step that wait for the end of its
  ;                propagation, newest first: the observed nodes, for their weight ('extended
  ;                only), and the Dirac nodes that hold a deferred value, to force it (see
  ;                activate!);
  ;   journal      #f, or the undo thunks of the current step (plan C5), newest first;
  ;   scope        the current construction scope, a list of (entry . polarity);
  ;   step-hook    #f or (λ (ch accepted? delta)).
  ; `debug_msg` (M:196) is dropped: see hakaru10-debug.
  (define-record chain rng semantics now nodes count llcum sampleable samp card-change
                       queue pending journal scope step-hook)

  ; (debug printer only) #<chain count nodes, now now>.
  (set-record-printer! chain
    (λ (ch port) (fprintf port "#<chain ~a nodes, now ~a>" (chain-count ch) (chain-now ch))))

  ; cf. M:872-880 `initial_step'`'s init_state: seed, now = 1, empty memory, llcum = 0,
  ; card_change = 0, no sampleable nodes.
  (define (new-chain src semantics)
    (unless (memq semantics '(upstream extended))
      (hakaru10-error 'construction 'mcmC "semantics must be 'upstream or 'extended" semantics))
    (make-chain src semantics 1 (make-vector 64 #f) 0 0. '() (vector) 0 #f '() #f '() #f))

  ; cf. Mem:67-74 `locate`: the node at address a.
  (define (chain-node ch a) (vector-ref (chain-nodes ch) a))

  ; cf. Mem:59-63 `new`: store n at the next address, growing the memory (an IntMap upstream)
  ; by doubling.
  (define (chain-store! ch n)
    (let ((a (node-addr n)) (v (chain-nodes ch)))
      (when (fx>= a (vector-length v))
        (chain-nodes-set! ch (vector-resize v (fx* 2 (vector-length v)) #f)))
      (vector-set! (chain-nodes ch) a n)
      (chain-count-set! ch a)))

  ; (helper) fold (kons node acc) over the nodes with addresses in (lo, hi], ascending: the
  ; order of upstream's IntMap/IntSet folds (M:696-751).
  (define (chain-fold kons knil ch #!optional (lo 0) (hi (chain-count ch)))
    (let loop ((a (fx+ lo 1)) (acc knil))
      (if (fx> a hi) acc (loop (fx+ a 1) (kons (chain-node ch a) acc)))))

  ; cf. M:856-870: after construction the graph is static.  Freeze `sampleable` into the
  ; vector `samp` (M:962 indexes the list with `!!`) and make the update queue (M:288), which
  ; holds each node at most once.
  (define (chain-freeze! ch)
    (chain-samp-set! ch (list->vector (chain-sampleable ch)))
    (chain-queue-set! ch (make-update-queue (make-vector (fx+ 1 (chain-count ch)) #f) 0))
    (chain-pending-set! ch '())
    (chain-scope-set! ch '()))

  ; (helper) whether ch runs the 'extended semantics (deviation 8).
  (define (extended? ch) (eq? (chain-semantics ch) 'extended))

  ;; ---------------------------------------------------------------------------------------
  ;; The undo journal (plan C5).  Upstream's MCState is persistent, and a rejected step simply
  ;; resumes from the old state with the new seed (M:904-906).  Here the state is mutable, so
  ;; every change made during a step pushes a thunk that undoes it, and a rejection runs them,
  ;; newest first.
  ;; ---------------------------------------------------------------------------------------

  ; cf. M:901, M:915: the state a step starts from; the oldest undo thunk restores `now` and
  ; `card_change`.
  ; (no upstream counterpart) a journal still open belongs to a step that an error escaped
  ; (transit!): it is rolled back first, so the new step starts from the last accepted state.
  ; Upstream's state is persistent and the failed step's sample is bottom (M:897-906); here a
  ; caller that handles the error may go on (the rng is not rolled back, M:906).  The rollback
  ; is lazy, as a handler around every step would cost a few percent of the step.
  (define (journal-begin! ch)
    (when (chain-journal ch) (journal-rollback! ch))
    (let ((now (chain-now ch)) (card (chain-card-change ch)))
      (chain-journal-set! ch (list (τ (chain-now-set! ch now) (chain-card-change-set! ch card))))))

  ; cf. M:904 `if accept then transition trace' st' ...`: keep the new state.
  (define (journal-commit! ch) (chain-journal-set! ch #f))

  ; cf. M:905-906 `transition trace st{seed=seed st'}`: back to the old state, every node
  ; state, inactive count, delayed (and pending) flag, `now` and `card_change` included, but
  ; not the seed.
  (define (journal-rollback! ch)
    (for-each (λ (undo) (undo)) (chain-journal ch))
    (chain-journal-set! ch #f)
    (chain-pending-set! ch '())
    (update-queue-clear! (chain-queue ch)))

  ; (helper) set a field of node n with `setter`, first pushing a thunk that restores its
  ; value, read with `getter`, while a step is in progress.
  (define-syntax-rule (journaled-set! (getter setter) n v)
    (let* ((n* n) (ch (node-chain n*)) (old (getter n*)) (journal (chain-journal ch)))
      (when journal (chain-journal-set! ch (cons (τ (setter n* old)) journal)))
      (setter n* v)))

  ; cf. Mem:94-98 `store` (through modify_node, M:262-271): a journaled store.
  (define (set-node-state! n s) (journaled-set! (node-state node-state-set!) n s))

  ; cf. M:705, M:733 (the inactive map updates of deactivate/activate): a journaled setter.
  (define (set-inactive! n k) (journaled-set! (node-inactive node-inactive-set!) n k))

  ; cf. M:301 and M:774 (insertion into and removal from delayed_updates): a journaled setter.
  (define (set-delayed! n flag)
    (unless (eq? flag (node-delayed n))
      (journaled-set! (node-delayed node-delayed-set!) n flag)))

  ; deviation 8 (M:724-751, plan §6.8): a journaled setter of the pending flag ('extended
  ; only, see activate!); raising it also records n in the chain's pending list.
  (define (set-pending! n flag)
    (unless (eq? flag (node-pending n))
      (journaled-set! (node-pending node-pending-set!) n flag)
      (when flag
        (let1 (ch (node-chain n)) (chain-pending-set! ch (cons n (chain-pending ch)))))))

  ;; ---------------------------------------------------------------------------------------
  ;; The update queue (M:273-295): a set of nodes, smallest address first.  "a node with a
  ;; smaller identifier does not depend on the nodes with larger identifiers" (M:274-276), so
  ;; each node is updated once, after all its updated parents (M:283-287, M:319-385).  A
  ;; binary min-heap; a node's `queued` flag gives the set semantics.
  ;; ---------------------------------------------------------------------------------------

  ; cf. M:288 `type UpdateQueue = NodeSet`.
  (define-record update-queue heap size)

  ; (helper) whether node a precedes node b in the queue.
  (define-syntax-rule (node<? a b) (fx< (node-addr a) (node-addr b)))

  ; cf. M:288 Set.insert: sift n up, unless it is already queued.
  (define (enqueue! q n)
    (unless (node-queued n)
      (node-queued-set! n #t)
      (let ((heap (update-queue-heap q)) (i (update-queue-size q)))
        (update-queue-size-set! q (fx+ i 1))
        (let up ((i i))
          (if (fx= i 0)
              (vector-set! heap 0 n)
              (let* ((p (fxshr (fx- i 1) 1)) (parent (vector-ref heap p)))
                (if (node<? n parent)
                    (begin (vector-set! heap i parent) (up p))
                    (vector-set! heap i n))))))))

  ; cf. M:295 `Set.deleteFindMin q`: remove and answer the smallest node of a non-empty queue.
  (define (dequeue! q)
    (let* ((heap (update-queue-heap q))
           (size (fx- (update-queue-size q) 1))
           (top (vector-ref heap 0))
           (last (vector-ref heap size)))
      (update-queue-size-set! q size)
      (node-queued-set! top #f)
      (let down ((i 0))
        (let* ((l (fx+ 1 (fx* 2 i))) (r (fx+ l 1)))
          (if (fx>= l size)
              (vector-set! heap i last)
              (let* ((c (if (and (fx< r size) (node<? (vector-ref heap r) (vector-ref heap l)))
                            r
                            l))
                     (child (vector-ref heap c)))
                (if (node<? child last)
                    (begin (vector-set! heap i child) (down c))
                    (vector-set! heap i last))))))
      top))

  ; (helper) M:294 `Set.null`.
  (define (update-queue-empty? q) (fx= 0 (update-queue-size q)))

  ; (helper) empty the queue, after a rejection or an aborted propagation (plan C22).
  (define (update-queue-clear! q)
    (let ((heap (update-queue-heap q)))
      (do ((i 0 (fx+ i 1))) ((fx= i (update-queue-size q)))
        (node-queued-set! (vector-ref heap i) #f))
      (update-queue-size-set! q 0)))

  ; cf. M:315-317 `Set.union tail dependents`.
  (define (enqueue-all! ch nodes)
    (let1 (q (chain-queue ch)) (for-each (λ (n) (enqueue! q n)) nodes)))

  ;; ---------------------------------------------------------------------------------------
  ;; SExp (S:84-135): Val, NRef (a node) and NT.
  ;; ---------------------------------------------------------------------------------------

  ; cf. S:89-93 `NT :: NTree a -> (a -> b) -> SExp b`, generalized from NTLeaf/NTPair to an
  ; n-ary vector of distinct leaf nodes (first occurrence order); `fn` takes the vector of the
  ; leaves' values.  The tree shape affects neither values, draws nor the tstamp check (the max
  ; over leaves, S:264-273), so the flat vector is equivalent.
  (define-record nt leaves fn)

  ; (debug printer only) #<nt (addr ...)>.
  (set-record-printer! nt
    (λ (e port) (fprintf port "#<nt ~s>" (map node-addr (vector->list (nt-leaves e))))))

  ; cf. S:85-89: whether x is an NRef or an NT; every other value is a Val (S:86), a procedure
  ; included (S:107 `Val f`): a Model is not a procedure (C32), so none needs boxing.
  (define (sexp? x) (or (node? x) (nt? x)))

  ; (helper) the owner chain of an sexp.
  (define (sexp-chain e) (node-chain (if (node? e) e (vector-ref (nt-leaves e) 0))))

  ; cf. S:206-208 `eval_ntree`'s leaves: the nodes an SExp depends on.
  (define (sexp-leaves e)
    (cond
      ((node? e) (vector e))
      ((nt? e) (nt-leaves e))
      (else (vector))))

  ; cf. S:264-273 `reeval_tree`: the values of the leaves.
  (define (leaf-values leaves) (vector-map node-value leaves))

  ;; ---------------------------------------------------------------------------------------
  ;; Deferred Dirac values.  Upstream never forces the value of a Dirac node that nothing
  ;; reads: `sampleK (Dirac x) g = (x,0,g)` (M:99) leaves x a thunk, and a Node's `val` is a
  ;; lazy field (only `ll` and `tstamp` are strict, M:120-129).  The readers of a node in an
  ;; inactive branch are in that branch (the branch-escape check), or its exit, which reads
  ;; only the active result (M:781-794), and their updates are delayed (M:298-303).  So
  ;; `if_ (lift null? xs) (return 'none) (diracN (lift car xs))` runs upstream, although the
  ;; inactive else branch applies `car` to '().  Here values are computed eagerly, so the
  ;; value of a diracN node (to-node) computed while the node is inactive, if it raises an
  ;; error, is a deferred value: the suspended application of the node's function to its
  ;; parents' values, Haskell's thunk.  It passes unforced through the inactive diracN nodes
  ;; that read it; a strict reader forces it, which raises the error again: any other node
  ;; (resampleable, observed, an entry, whose value ifnode reads, M:648-649), an active diracN
  ;; node, an active exit, and the sampled result.  Computing it draws nothing, so replays are
  ;; unchanged.
  ;; deviation 11 (M:99, M:120-129, S:100-116; strict propagation of a deferred value): the
  ;; stand-in is coarser than Haskell's laziness.  A function applied, in an inactive context,
  ;; to a deferred value is not called: its value is that deferred value (lazy-value,
  ;; lift/apply), even when the function ignores the argument (`cdr` of (pair h 1.0), `length`
  ;; of a list, a constant function).  Upstream computes the real value there, never forcing
  ;; the argument; here a strict reader of the result (a resampleable or observed node's
  ;; distribution in the same inactive branch, say) forces it and raises the original error.
  ;; With xs = '(), the inactive branch
  ;;   (do/monad (← h (diracN (lift car xs)))
  ;;             (← mu (diracN (lift cdr (pair h 1.0))))
  ;;             (dist normal mu 1))
  ;; builds upstream (mu = snd (⊥, 1.0) = 1.0) and is an error here ('upstream mode; in
  ;; 'extended mode the error, being deferrable?, rejects the build or the step instead, C22).
  ;; Likewise an active diracN or exit computes its value at once, which upstream forces only
  ;; when it is read: when it is made or updated, and when a switch activates it holding a
  ;; deferred value that no update of the step recomputes (activate!, settle-pending!).  Case
  ;; B6 of test/hakaru10.scm shows both: strict-reader and ignoring-reader the first,
  ;; strict-active the second (and strict-active/val that a lift of Vals in an active branch is
  ;; applied at once, lazy-lifting being off there); case H5 of test/hakaru10-exact.scm the
  ;; switch (guarded-head/constant, guarded-exit/constant).
  ;; Calling the function on the deferred records themselves would compute a wrong value for
  ;; one that branches on its argument's type, so the error is kept.
  ;; ---------------------------------------------------------------------------------------

  ; cf. M:99 and M:120-129 (a lazy `val`): the thunk of a diracN node's value, `fn` (λ
  ; (values-vector) value) applied to `args`, the values of the node's parents (none deferred).
  ; Compared by equal? through its fields, as the reference evaluator of `#:check` does.
  (define-record deferred fn args)

  ; (debug printer only) #<deferred>.
  (set-record-printer! deferred (λ (d port) (fprintf port "#<deferred>")))

  ; (helper) whether the error condition e, raised while an inactive diracN node's value is
  ; computed, is deferred: any error but an (exn hakaru10) one of a kind other than `parameter`
  ; (a misuse such as `dist` in a lifted function is reported at once).  In 'extended mode the
  ; same errors, raised by an inactive node, reject (C22, build-guard and guarded-update).
  (define (deferrable? e) (or (not (hakaru10-error? e)) (parameter-error? e)))

  ; (helper) force x if it is a deferred value: apply its function and answer the value, or
  ; raise the error again (deferrable?, as it was the first time: in 'extended mode an
  ; inactive node that forces it rejects, C22); any other x is answered as is.
  (define (force-value x)
    (if (deferred? x)
        ((deferred-fn x) (deferred-args x))
        x))

  ; (helper) the first deferred value of the values-vector vs, else #f.
  (define (first-deferred vs)
    (let loop ((i 0))
      (cond
        ((fx= i (vector-length vs)) #f)
        ((deferred? (vector-ref vs i)) (vector-ref vs i))
        (else (loop (fx+ i 1))))))

  ; (helper) the values-vector vs, with every deferred value forced (vs itself if none is).
  (define (forced-values vs)
    (if (first-deferred vs) (vector-map force-value vs) vs))

  ; (helper) the list counterpart of forced-values.
  (define (forced-list vs)
    (if (any deferred? vs) (map force-value vs) vs))

  ; (helper of lazy-value and lift/apply) g applied to args (the values of a node's parents, a
  ; vector, or of lift's arguments, a list; none deferred), or, if g raises a deferrable error,
  ; the deferred value of that application (deviation 11, cf. M:99).
  (define (call/deferring g args)
    (condition-case (g args)
      (e (exn) (if (deferrable? e) (make-deferred g args) (signal e)))))

  ; (helper) the value of a diracN node, g applied to its parents' values vs: if the node is
  ; inactive, the first deferred value of vs (deviation 11: g is not called, even if it would
  ; ignore it), else g's value, or a deferred value if g raises a deferrable error; if it is
  ; active, g of the forced values.
  (define (lazy-value g vs inactive?)
    (if inactive?
        (or (first-deferred vs) (call/deferring g vs))
        (g (forced-values vs))))

  ; cf. S:100, S:107 `fmap f (Val x) = Val (f x)`, a thunk too: set by in-branch while an
  ; inactive branch's State action runs, where lift* applying f to Vals defers its error as
  ; lazy-value does; build-chain starts every build with it #f, also one started from inside an
  ; inactive branch's lifted function.
  (define lazy-lifting (make-parameter #f))

  ; cf. S:201-208 `eval_nctor`: the current value of an SExp (forced, see force-value).
  (define (sexp-value e)
    (cond
      ((node? e) (force-value (node-value e)))
      ((nt? e) ((nt-fn e) (forced-values (leaf-values (nt-leaves e)))))
      (else e)))

  ; deviation 9 (S:82, S:137-151): `Model a` and `SExp a` are distinct types upstream, and a
  ; Model is a value of its own type here too (C32), so a Model in an SExp position is a
  ; run-time error; a procedure is a plain Val (S:107).
  (define (check-not-model who x)
    (when (model? x)
      (hakaru10-error 'construction who
                      "a model where an SExp is expected; bind it with ← first" x)))

  ; deviation 9 (S:85-93, S:134-135): a Scheme value that contains SExps (in pairs or
  ; vectors, e.g. a model that answers (list x y)) is not an `SExp a` upstream, where a tuple
  ; of SExps must be combined with `pair` (S:134); here it would silently be a Val whose
  ; value holds node records.  Nor is a Model inside a Scheme value: upstream's `diracN` needs
  ; `Show a`, which State has not (S:166).  The first SExp or Model found in x, x itself
  ; included, else #f.  x may be circular (e.g. srfi-1's circular-list): a value of more than
  ; 100000 pairs and vectors is walked again, recording each in an eq? table, so that a cycle
  ; ends the walk.  The values of ordinary models, which construction walks (every Val, Dirac
  ; value and first answer of a lifted function), are walked without allocating.
  (define (internal-part x)
    (let1 (r (internal-part/bounded x 100000))
      (cond
        ((not (fixnum? r)) r)
        ((fx>= r 0) #f)
        (else (internal-part/cyclic x)))))

  ; (helper of internal-part) the walk of x, with `budget` pairs and vectors left to visit: the
  ; first SExp or Model found, else the budget left, a fixnum, negative once it has run out.
  (define (internal-part/bounded x budget)
    (cond
      ((fx< budget 0) budget)
      ((or (number? x) (null? x) (boolean? x) (symbol? x)) budget)       ; the common leaves
      ((pair? x) (let1 (r (internal-part/bounded (car x) (fx- budget 1)))
                   (if (fixnum? r) (internal-part/bounded (cdr x) r) r)))
      ((vector? x) (let loop ((i 0) (b (fx- budget 1)))
                     (if (fx= i (vector-length x))
                         b
                         (let1 (r (internal-part/bounded (vector-ref x i) b))
                           (if (fixnum? r) (loop (fx+ i 1) r) r)))))
      ((or (sexp? x) (model? x)) x)
      (else budget)))

  ; (helper of internal-part) the walk of x that records every pair and vector in an eq?
  ; table and skips those already seen.
  (define (internal-part/cyclic x)
    (let1 (seen (make-hash-table eq?))
      (let walk ((x x))
        (cond
          ((or (pair? x) (vector? x))
           (and (not (hash-table-exists? seen x))
                (begin
                  (hash-table-set! seen x #t)
                  (if (pair? x)
                      (or (walk (car x)) (walk (cdr x)))
                      (any walk (vector->list x))))))
          ((or (sexp? x) (model? x)) x)
          (else #f)))))

  ; deviation 9 (S:85-93, S:134-135, S:166): a `construction` error, located at who, if the
  ; Scheme value x contains an SExp or a Model (internal-part).
  (define (check-no-sexp who x)
    (let1 (y (internal-part x))
      (cond
        ((sexp? y) (hakaru10-error 'construction who
                                   "an SExp inside a Scheme value; combine with collect, pair or lift"
                                   y))
        ((model? y) (hakaru10-error 'construction who "a model inside a Scheme value" y)))))

  ; deviation 9 (S:99-116, `fmap :: (a -> b) -> SExp a -> SExp b`): a lifted function must
  ; answer a plain value, not an SExp nor a Model, nor, if `deep?`, a Scheme value that
  ; contains one (check-no-sexp, which walks a pair or a vector).  Answers v.  It runs each
  ; time an NT's fn does, so a number or a boolean, the usual answers, is accepted first.
  (define (check-lifted-value who v #!optional deep?)
    (cond
      ((or (number? v) (boolean? v)) v)
      ((sexp? v) (hakaru10-error 'construction who "a lifted function answered an SExp" v))
      ((model? v) (hakaru10-error 'construction who "a lifted function answered a model" v))
      ((and deep? (or (pair? v) (vector? v))) (check-no-sexp who v) v)
      (else v)))

  ; cf. S:99-116 `instance Functor SExp`, `instance Applicative SExp` (fmap, <*>, liftA2,
  ; liftA3, ... in one): if no argument is an sexp, `fmap f (Val x) = Val (f x)` (S:100,
  ; S:107), f being applied now; otherwise the arguments fuse into one NT (`fmap f (NT t g) =
  ; NT t (f . g)`, S:102, and the <*> cases S:110-116).  The "cannot happen" cases (S:108,
  ; S:111, S:115) disappear: f is a Scheme procedure.  An SExp or a Model that f answers is a
  ; `construction` error (check-lifted-value, deviation 9): applied to Vals, at any depth of
  ; its answer; for an NT, each time its fn runs, when its node is built or updated, whatever
  ; the node's kind, at any depth on its first run (at the build of its first consumer, unless
  ; a deferred parent delays it) and at the top on the next ones: walking the answer at every
  ; update would cost too much (a categorical's alist, every step).  `checked?` #f skips the
  ; check, for pair and collect, whose cons and list answer their arguments' values, checked
  ; already.  Applied to Vals, f forces the deferred ones, but under lazy-lifting, where the
  ; first deferred Val is the answer (deviation 11: f is not called) and an error of f a
  ; deferred value (cf. S:100, M:99).
  (define (lift* f es #!optional (checked? #t))
    (for-each (λ (e) (check-not-model 'lift e)) es)
    (if (any sexp? es)
        (lift/fuse f es checked?)
        (let1 (v (lift/apply f es)) (if checked? (check-lifted-value 'lift v #t) v))))

  ; (helper of lift*) f applied to the values vs (see lift*).
  (define (lift/apply f vs)
    (cond
      ((not (lazy-lifting)) (apply f (forced-list vs)))
      ((find deferred? vs))
      (else (call/deferring (λ (args) (apply f args)) vs))))

  ; cf. S:110-116 (<*> on NTs, NTPair): one NT whose leaves are the union of the arguments'
  ; leaves, in first-occurrence order, and whose fn applies f to the arguments' values, each
  ; extracted from the union's values, then checked if `checked?` (check-lifted-value, at any
  ; depth on the fn's first run only, see lift*).  The leaves must belong to one chain.
  (define (lift/fuse f es checked?)
    (let* ((leaves (delete-duplicates (append-map (λ (e) (vector->list (sexp-leaves e))) es) eq?))
           (owner (node-chain (car leaves)))
           (position (λ (n) (list-index (λ (m) (eq? n m)) leaves)))
           (extractors (map (λ (e) (lift/extractor position e)) es)))
      (unless (every (λ (n) (eq? (node-chain n) owner)) leaves)
        (hakaru10-error 'construction 'lift "nodes of different model runs" es))
      (make-nt (list->vector leaves)
               (if checked?
                   (let1 (first? #t)
                     (λ (vs)
                       (let1 (v (check-lifted-value 'lift (apply f (map (λ (x) (x vs)) extractors))
                                                    first?))
                         (set! first? #f)
                         v)))
                   (λ (vs) (apply f (map (λ (x) (x vs)) extractors)))))))

  ; (helper of lift/fuse) the procedure that extracts e's value from the values of the union
  ; of leaves; `position` maps a leaf to its index there.  An NT whose leaves are a prefix of
  ; the union reads the union's vector as is.
  (define (lift/extractor position e)
    (cond
      ((node? e) (let1 (i (position e)) (λ (vs) (vector-ref vs i))))
      ((nt? e) (let* ((g (nt-fn e)) (ps (map position (vector->list (nt-leaves e)))))
                 (if (equal? ps (iota (length ps)))
                     g
                     (let1 (ps (list->vector ps))
                       (λ (vs) (g (vector-map (λ (p) (vector-ref vs p)) ps)))))))
      ((deferred? e) (λ (vs) (force-value e)))
      (else (K e))))

  ;; ---------------------------------------------------------------------------------------
  ;; Construction: the State monad (S:82-279, M:233-526, M:641-686).  `type MCMCM a = State
  ;; MCState a` (M:200), `type TraceM a = MCMCM (NodeRef a)` (M:202), `type Model a = MCMCM
  ;; (SExp a)` (S:82): a model is a computation of (aux category state) whose state is the
  ;; chain (plan §15, C32-C34).  Each function below is a State action named after its
  ;; upstream counterpart, with its argument order and decomposition (C33).  Nodes are created
  ;; when an action runs, in bind order (C35), and only by create-node.
  ;; ---------------------------------------------------------------------------------------

  ; cf. S:82 (`Model a` is a type of its own) and C32: whether x is a Model, a State
  ; computation.
  (define model? category-state?)

  ; cf. M:856-869 (`mcmc` takes a TraceM) and S:82: a `construction` error at who unless m is
  ; a Model; answers m.
  (define (check-model m who)
    (unless (model? m)
      (hakaru10-error 'construction who
                      "a model (a State computation: do/monad, dist, diracN, if_ or return) is expected"
                      m))
    m)

  ; cf. M:200 `State MCState` and upstream's `modify $ \s -> s{...}`: the action that runs body
  ; on the chain ch, mutated in place (C34), and leaves it as the state.
  (define-syntax-rule (modify! (ch) body ...) (modify (λ (ch) body ... ch)))

  ; cf. upstream's `fail msg = error msg` in MCMCM (M:475, M:647): the action that raises the
  ; (exn hakaru10) error when it runs.
  (define (hakaru10-fail kind who msg . irritants)
    (state (λ (_) (apply hakaru10-error kind who msg irritants))))

  ; deviation 11 (Haskell's non-strict arguments; C36): the Model that calls thunk when it runs
  ; and checks that thunk answered a Model.  A procedure over a thunk, not a macro, so that the
  ; public `if_` expands into plain procedure calls.
  (define (lazy-model who thunk) (state (λ (ch) (run-state (check-model (thunk) who) ch))))

  ; cf. S:174-193 `class Condition`, through the distribution module's `condition`;
  ; deviation 9 (S:38-42, S:180-183): "observation must be external to the model" is a run-time
  ; check that obs neither is nor contains an SExp or a Model (internal-part): `CondV a = a`
  ; (S:180-193), and a Model has no `Eq` nor `Show`.  This is the public `condition`.
  (define (condition/external obs d) (condition obs d internal-part))

  ; deviation 10 (no upstream counterpart): an sexp must belong to the chain being built.
  (define (check-owner ch e who)
    (when (and (sexp? e) (not (eq? (sexp-chain e) ch)))
      (hakaru10-error 'construction who
                      "a node of another model run (a stale node) is used" e)))

  ; deviation 9 (M:46-60): a distribution constructor must answer a distk.
  (define (check-distk who dk)
    (unless (distk? dk)
      (hakaru10-error 'construction who "a distribution constructor must return a distribution"
                      dk))
    dk)

  ; deviation 9 (S:38-42, S:174-176, S:251-252, M:487-488): the observation of the observed
  ; node n, obs, recomputed by its distribution function, must be the one it was built with.
  ; Upstream's types accept a constructor that observes its argument, `\xv -> condition xv
  ; (bern 0.8)` (CondV a = a, S:180-193), and its update keeps the built observation
  ; (`Observed d _ -> self{ll = logDensity d (val self), dst = d}`), so the chain silently
  ; observes the first value.  The observation must be external to the model: obs not equal?
  ; to it raises `conditioning`.  The oracles of (aux hakaru10 exact) check it too
  ; (recomputed-state).
  (define (check-observation n obs)
    (unless (equal? obs (node-value n))
      (hakaru10-error 'conditioning 'update "observation must be external to the model"
                      n (node-value n) obs)))

  ; (helper) whether the node being created now is inactive: some enclosing branch, (entry .
  ; polarity) in the construction scope, is not the one its entry's value selects.
  (define (inactive-scope? ch)
    (any (λ (p) (not (eq? (node-value (car p)) (cdr p)))) (chain-scope ch)))

  ; deviation 8 (plan C22): whether the (exn hakaru10) condition e is of kind `parameter`, the
  ; only errors that the 'extended semantics turns into a rejection.
  (define (parameter-error? e) (eq? (hakaru10-error-kind e) 'parameter))

  ; deviation 8 (M:650, M:882-891, plan C22): in 'extended mode an error that deferrable?
  ; accepts (an (exn hakaru10) error of kind `parameter`, or any non-hakaru10 error) raised
  ; while a node of an inactive branch is created (its distribution constructor, the NT that
  ; computes its parameters, a deferred value it forces, or its sampler) rejects the initial
  ; trace, as a -inf LL does: build-chain answers no result and initial-step builds again.  One
  ; criterion, deferrable?'s, so that `(dist normal (lift car xs) 1.)` and its diracN-shared
  ; form, the same model upstream (S:164-169), both reject.  Upstream (and 'upstream mode)
  ; fails, since both branches are always built (M:650-661) and the LL is strict (M:122).
  (define (build-guard ch thunk)
    (if (and (pair? (chain-scope ch)) (extended? ch) (inactive-scope? ch))
        (condition-case (thunk)
          (e (exn) (signal (if (deferrable? e)
                               (make-property-condition 'hakaru10-rejected-build)
                               e))))
        (thunk)))

  ; (helper) whether the scope s is a tail of the scope s* (eq?): the scope of s* itself or of
  ; a branch that encloses it.
  (define (scope-tail? s s*)
    (let tail? ((cs s*)) (or (eq? cs s) (and (pair? cs) (tail? (cdr cs))))))

  ; deviation 10 (no upstream counterpart; Haskell's lexical scoping guarantees it): a node
  ; may depend only on nodes of its own branch or of an enclosing one, i.e. the parent's scope
  ; must be a tail of the child's.  Exit nodes depend on both branches by design (M:670-673):
  ; each result was checked by to-node, inside its own branch.
  (define (check-branch-escape parent child)
    (unless (or (eq? (node-role child) 'exit)
                (scope-tail? (node-scope parent) (node-scope child)))
      (hakaru10-error 'branch 'if_ "node escapes its branch" parent child)))

  ; cf. M:246-250 `new_dependency`, the write: add child to the dependents of parent (IntSet
  ; union), after deviation 10's branch-escape check.
  (define (add-dependency! parent child)
    (check-branch-escape parent child)
    (unless (memq child (node-deps parent))
      (node-deps-set! parent (cons child (node-deps parent)))))

  ; cf. M:469-473 (with_node's assertion, plan C13): in 'upstream mode, "the parent or the node
  ; itself are inactive" is an error in every update; deviation 8 (M:469-473): 'extended drops
  ; it.
  (define (check-active-update n leaves)
    (vector-for-each (λ (p)
                       (when (node-inactive? p)
                         (hakaru10-error 'dependency 'update
                                         "the parent or the node itself are inactive" p n)))
                     leaves)
    (when (node-inactive? n)
      (hakaru10-error 'dependency 'update "the parent or the node itself are inactive" n)))

  ; cf. S:264-273 `reeval_tree`: the newest tstamp among the leaves.
  (define (leaves-tstamp leaves)
    (let loop ((i 0) (ts 0))
      (if (fx= i (vector-length leaves))
          ts
          (loop (fx+ i 1) (fxmax ts (nstate-tstamp (node-state (vector-ref leaves i))))))))

  ; cf. S:233-253 doupdate (and its copies M:463-489 in with_node, M:503-526 in with_node2):
  ; the update of a plain node.  It never resamples: a Dirac node gets the new value and always
  ; a new tstamp (M:482, the note on Dirac, P §4.3 l.801-843); a resampleable or observed node
  ; keeps its value and tstamp and gets the new LL "and !!! Don't forget to update the
  ; distribution" (M:483-488), an observed node also its observation (check-observation).
  ; Answers (values nstate' '()).  The node's fn forces the deferred values it reads, but that
  ; of an inactive diracN node (see lazy-value).  Imperative, as every update (C34): it runs
  ; during a step, under the journal, not as a State action.
  (define (doupdate n)
    (let ((ch (node-chain n)) (self (node-state n)) (leaves (node-parents n)))
      (unless (extended? ch) (check-active-update n leaves))
      ; cf. S:240-243: the parents must have been updated.
      (unless (fx> (leaves-tstamp leaves) (nstate-tstamp self))
        (hakaru10-error 'dependency 'update "dependency violation!" n (leaves-tstamp leaves)))
      (let* ((dk (check-distk 'update ((node-fn n) (leaf-values leaves))))
             (kind (distk-kind dk)))
        ; deviation 10 (M:46-60): the kind of a node is a static type upstream.
        (unless (eq? kind (node-kind n))
          (hakaru10-error 'construction 'update
                          "a distribution constructor changed the kind of its node"
                          n (node-kind n) kind))
        (values
          (if (eq? kind 'dirac)
              (make-nstate (check-lifted-value 'update (distk-value dk)) 0. #f (chain-now ch))
              (let ((rd (distk-rd dk)) (v (nstate-val self)))
                (when (eq? kind 'observed) (check-observation n (distk-value dk)))
                (make-nstate v ((rdistr-log-density rd) v) rd (nstate-tstamp self))))
          '()))))

  ; cf. M:233-243 `new_nref`: store n (Mem.new, M:236), record it in `sampleable` if it is
  ; resampleable, prepended (M:238-239), and add its LL to llcum (M:241); `added` is a range
  ; of chain-count here (M:240, M:650).  Answers n, its own NRef.  Upstream's `get`, then
  ; `put` of the new state, is one transition, written with `state`: it runs once per node,
  ; on the hottest path of construction.
  (define (new-nref n)
    (state (λ (ch)
             (chain-store! ch n)
             (when (eq? (node-kind n) 'resampleable)
               (chain-sampleable-set! ch (cons n (chain-sampleable ch))))
             (chain-llcum-set! ch (fp+ (chain-llcum ch) (nstate-ll (node-state n))))
             (cons n ch))))

  ; cf. M:246-250 `new_dependency`, with deviation 10's branch-escape check (add-dependency!).
  (define (new-dependency parent child) (modify! (ch) (add-dependency! parent child)))

  ; cf. S:255-261 `register_dependencies t na`: new_dependency of each leaf of t, in order (the
  ; tree's leaves are the flat vector leaves, S:89-93), as one action.
  (define (register-dependencies leaves child)
    (modify! (ch) (vector-for-each (λ (p) (add-dependency! p child)) leaves)))

  ; cf. M:404-419 `create_node update dist`: sample dk (sampleK, M:409, drawing only if it is
  ; resampleable, M:97-100; the rng source advances in place, so M:410's `modify seed` has no
  ; statement of its own), read now (M:411), make the node (M:413-419) and new_nref it.  role,
  ; parents and fn are the static fields of plan §5.2, after upstream's two arguments: role is
  ; root (no update), plain, entry or exit, parents a vector of nodes, fn the node's distk
  ; function (the constant dk of a root).  The node records the current construction scope.
  ; The only draw of construction, and the only place a node is made.  Upstream's `get`, then
  ; new_nref, is one transition, written with `state`, as new-nref is: it runs once per node,
  ; and a bind per statement made construction up to a quarter slower (plan §15.6).
  (define (create-node update dk #!optional (role 'root) (parents (vector)) (fn dk))
    (state (λ (ch)                                                                ; M:408
             (receive (v ll) (build-guard ch (τ (distk-sample dk (chain-rng ch))))  ; M:409-410
               (run-state (new-nref (make-node ch (fx+ 1 (chain-count ch)) (distk-kind dk) role
                                               parents fn update (chain-scope ch) '() #f
                                               (make-nstate v ll (distk-rd dk) (chain-now ch))
                                               0 #f #f #f))
                          ch)))))

  ; cf. M:421-423 `with_node_none`: a root node, never updated.
  (define (with-node-none dk) (create-node #f dk))

  ; cf. S:224-228, with_node_ctor's general case `create_node (Just $ doupdate t (df . g))
  ; dist >> register_dependencies t addr`, and S:255-261: a plain node whose distk is fn of the
  ; leaves' values (a vector), updated by doupdate, then a dependent of each leaf in order.
  ; The value of a Dirac node must not contain an SExp (check-no-sexp, deviation 9).
  ; with_node2 (M:492-499) is the two-leaf case of this, the leaves being a flat vector (S:89-93),
  ; so it has no definition of its own; nor has refresh_nref (M:253-256): nodes are shared,
  ; never copied.  One transition, written with `state`, as create-node: it runs once per node
  ; but a root (plan §15.6).
  (define (with-node-tree leaves fn #!optional (who 'dist))
    (state (λ (ch)
             (let1 (dk (check-distk who (build-guard ch (τ (fn (leaf-values leaves))))))  ; S:225
               (when (eq? (distk-kind dk) 'dirac)                                      ; deviation 9
                 (check-no-sexp who (distk-value dk)))
               (let* ((r (run-state (create-node doupdate dk 'plain leaves fn) ch))    ; S:226
                      (n (car r)))
                 (cons n (exec-state (register-dependencies leaves n) (cdr r))))))))   ; S:227

  ; cf. M:451-456 `with_node ndr f`: one parent; f maps its (forced) value to a distk.
  (define (with-node ndr f #!optional (who 'dist))
    (with-node-tree (vector ndr) (λ (vs) (f (force-value (vector-ref vs 0)))) who))

  ; deviation 11 (M:99, M:120-129): to_node's NT case (S:214) as a Dirac node whose value is
  ; lazy-value's, df of g of the leaves' values, deferred while the node is inactive: `self`,
  ; the node once made, answers whether it is, the construction scope before.  The cell is
  ; bound per run, after the first bind (a TraceM may run several times).
  (define (lazy-tree leaves g df who)
    (do/monad
      (← ch get)
      (let self #f)
      (← n (with-node-tree leaves
                           (λ (vs) (df (lazy-value g vs (if self
                                                            (node-inactive? self)
                                                            (inactive-scope? ch)))))
                           who))
      (let _ (set! self n))
      (return n)))

  ; cf. S:218-228 `with_node_ctor e df`, dispatching as S:219-224: a Val gives a root node
  ; (S:219 `create_node Nothing (df x)`), an NRef `with_node` (S:220), an NT the general case
  ; (S:221-228).  df maps a value to a distk, and the node's fn forces the deferred values it
  ; reads, unless `lazy` (to_node's NT, deviation 11), which makes the value lazy-value's.  A
  ; Val must not contain an SExp (check-no-sexp, deviation 9).  `who` names the caller in
  ; errors.
  (define (with-node-ctor e df #!optional (who 'dist) lazy)
    (check-not-model who e)
    (do/monad
      (gets (λ (ch) (check-owner ch e who)))                                      ; deviation 10
      (cond
        ((node? e) (if lazy
                       (lazy-tree (vector e) (λ (vs) (vector-ref vs 0)) df who)
                       (with-node e df who)))                                     ; S:220
        ((nt? e) (if lazy
                     (lazy-tree (nt-leaves e) (nt-fn e) df who)
                     (let1 (g (nt-fn e))                                          ; S:221-228
                       (with-node-tree (nt-leaves e) (λ (vs) (df (g (forced-values vs)))) who))))
        (else (check-no-sexp who e)                                               ; deviation 9
              (>>= (gets (λ (ch) (check-distk who (build-guard ch (τ (df (force-value e)))))))
                   with-node-none)))))                                            ; S:219

  ; cf. S:210-214 `to_node`, which S:166-169 `diracN` repeats: a Val becomes a Dirac root
  ; (S:212), an NRef is answered as is, shared ("if x is just a NoderRef, (diracN x) returns
  ; this reference (shares) rather than creating a copy of it!", S:164-165, S:213), an NT a
  ; lazy Dirac node (S:214, deviation 11).  A Val that contains an SExp is an error
  ; (check-no-sexp, deviation 9).
  ; deviation 10 (no upstream counterpart; Haskell's lexical scoping): a shared node must be of
  ; the current scope or an enclosing one (scope-tail?), as add-dependency! requires of a
  ; parent.  So a node captured with set! in one branch is a `branch` error as the result of
  ; another branch, which its exit would read (ifnode runs `th >>= to_node` in the branch's
  ; scope, S:279), or as the model's result (model->trace, at the top-level scope '()).
  (define (to-node e #!optional (who 'to_node))
    (check-not-model who e)
    (cond
      ((node? e) (do/monad                                                       ; S:213
                   (gets (λ (ch)
                           (check-owner ch e who)
                           (unless (scope-tail? (node-scope e) (chain-scope ch))
                             (hakaru10-error 'branch who "node escapes its branch" e))))
                   (return e)))
      ((nt? e) (with-node-ctor e dirac who #t))                                  ; S:214
      (else (check-no-sexp who e) (with-node-none (dirac e)))))                  ; S:212

  ; cf. S:134-135 `pair = liftA2 (,)`.
  (define (pair a b) (lift* cons (list a b) #f))

  ; cf. B1:113-114 and P l.503-504, `sequenceA` of a list of SExps: one SExp of the list of
  ; their values; (collect '()) is '().
  (define (collect es) (lift* list es #f))

  ; deviation 9 (S:137-151, `type family Args`): the number of `dist`'s arguments is part of
  ; the constructor's type upstream, so `dist normal 0` does not compile.  Here it is a
  ; `construction` error, located at dist, raised when the node is made, in both semantics:
  ; not CHICKEN's (exn arity) condition, which deferrable? accepts, so that build-guard would
  ; take it for a rejection in an inactive branch ('extended, C22).  `call` is the application
  ; of the constructor d to the argument values `args`, checked with check-distk; `applied?`,
  ; a variable of the caller, is #f until a first application returns: an arity cannot change
  ; from one application to the next, so the later ones, an update's, run unguarded.
  (define-syntax-rule (ctor-apply applied? d args call)
    (check-distk 'dist (if applied?
                           call
                           (let1 (dk (call/arity-check d (λ () call) args))
                             (set! applied? #t)
                             dk))))

  ; (helper of ctor-apply) call thunk, which applies d to args: an (exn arity) condition of d
  ; itself, whose `arguments` property is (d) for an interpreted and a compiled d alike, is
  ; the `construction` error; any other condition, one raised inside d's body included, is
  ; signaled again, to the handlers around the call.  A handler that does not escape, rather
  ; than condition-case's, costs half as much on this path, a node's construction.
  (define (call/arity-check d thunk args)
    (with-exception-handler
      (λ (e)
        (if (and (arity-condition? e) (memq d (exn-arguments e)))
            (hakaru10-error 'construction 'dist
                            "wrong number of arguments for the distribution constructor" d args)
            (signal e)))
      thunk))

  ; (helpers of call/arity-check) CHICKEN's (exn arity) conditions and their `arguments`.
  (define arity-condition? (condition-predicate 'arity))
  (define exn-arguments (condition-property-accessor 'exn 'arguments '()))

  ; cf. S:137-151 `type family Args`, `class StochasticPrim`: `dist d = fmap NRef $
  ; with_node_none d` (S:144-145), `dist d x = with_node_ctor x d` (S:147-148), `dist d x y =
  ; with_node_ctor (pair x y) (uncurry d)` (S:150-151).  d may answer a resampleable or an
  ; observed distk (S:143-151, k polymorphic).  deviation 9 (S:137-151): any arity (collect for
  ; n >= 3, a nullary constructor applied to '()), plain values are Vals (replacing
  ; `fromInteger`/`pure`, S:120-132), and an argument that is a Model, or a Val that holds an
  ; SExp or a Model (check-no-sexp: the `Show` of the sample type, S:144-151), raises when
  ; dist is applied, as the type check does upstream; next to an SExp argument, such a Val is
  ; only a constant of the pair or list's NT, which with-node-ctor's Val check never sees.
  ; The SExp of the arguments is made then too: pair and collect draw nothing, but raise
  ; `construction` at once for nodes of different model runs (lift/fuse), and under
  ; lazy-lifting may answer a deferred argument in place of the pair or list, whose force
  ; raises its error again when the node is made.  The owner check (check-owner) fires when
  ; the model runs (plan §15.2), and so does the arity check (ctor-apply).  It always creates
  ; a new node.
  (define (dist d . args)
    (cond
      ((distk? d) (unless (null? args)
                    (hakaru10-error 'construction 'dist "a distribution takes no arguments"
                                    d args)))
      ((procedure? d) (for-each (λ (a)
                                  (check-not-model 'dist a)
                                  (unless (sexp? a) (check-no-sexp 'dist a)))
                                args))
      (else (hakaru10-error 'construction 'dist
                            "a distribution or a distribution constructor is expected" d)))
    (let1 (applied? #f)
      (match/first args
        (() (if (distk? d)
                (with-node-none d)                                                ; S:144-145
                (with-node-ctor '() (λ (_) (ctor-apply applied? d '() (d))))))
        ((,x) (with-node-ctor x (λ (v) (ctor-apply applied? d (list v) (d v)))))  ; S:147-148
        ((,x ,y) (with-node-ctor (pair x y)                                       ; S:150-151
                                 (Λ ((,a . ,b)) (ctor-apply applied? d (list a b) (d a b)))))
        (else (with-node-ctor (collect args) (λ (vs) (ctor-apply applied? d vs (apply d vs))))))))

  ; cf. S:161-169 `diracN`: its three cases are to_node's (S:167-169 = S:212-214).
  (define (diracN e) (to-node e 'diracN))

  ; cf. S:275-279 `if_ test th el = do entry <- with_node_ctor test diracS; fmap NRef $ ifnode
  ; entry (th >>= to_node) (el >>= to_node)`: "Always make a new node, for the sake of ifnode"
  ; (a root for a constant test).  deviation 9 (S:275 `if_ :: SExp Bool -> ...`): the test
  ; must be a boolean (plan C7).  th and el are Models (the public `if_` delays them, C36).
  ; Answers the exit node.  The checks of the test (owner, a Val holding an SExp) name if_.
  (define (if_* test th el)
    (check-not-model 'if_ test)
    (do/monad
      (← entry (with-node-ctor test (λ (v) (dirac (check-test 'if_ v))) 'if_))  ; S:277-278
      (modify! (ch) (node-role-set! entry 'entry))                              ; plan §5.2
      (ifnode entry (▷ (check-model th 'if_) to-node) (▷ (check-model el 'if_) to-node))))  ; S:279

  ; cf. S:82 `type Model a = MCMCM (SExp a)` and S:196-197 `mcmC limit m = mcmc limit (m >>=
  ; to_node)`: the TraceM of a Model, whose answer, the model's result, becomes a node last,
  ; outside every branch.  Every entry point that runs a Model builds through it (build-chain
  ; takes the TraceM), checking m on behalf of who.
  (define (model->trace m who) (▷ (check-model m who) to-node))

  ;; ---------------------------------------------------------------------------------------
  ;; Branches (the note M:564-639, the code M:641-794, S:275-279; P §4.4 p.17 l.844-869):
  ;; "we mark the nodes in the inactive branch as inactive, and delay their updates until they
  ;; are activated" (M:587-588).  Both branches are instantiated, the inactive one first,
  ;; between an entry node (the test, a Dirac node, never resampleable) and an exit node (a
  ;; Dirac node whose value is the active branch's result).  A branch's nodes are the
  ;; addresses created while it is built, a half-open range (lo, hi] (replacing `added`,
  ;; M:191-192, M:650-663).  Each node carries a deactivation count (M:174-179); a switch of
  ;; the test moves the counts and the observed nodes' LL (M:688-751), and replays the updates
  ;; delayed while a node was inactive (M:181-183, M:298-303, M:767-775).
  ;; ---------------------------------------------------------------------------------------

  ; cf. M:650-668, what ifnode keeps of an if_: the entry, the results of the then and else
  ; branches (M:665-666) and their address ranges (lo . hi), half-open (lo, hi] (M:679-680,
  ; replacing `added`, M:191-192).  Shared by the entry and the exit (node-branch).
  (define-record branch entry then-result else-result then-range else-range)

  ; (debug printer only) #<branch entry then-result else-result>, as addresses.
  (set-record-printer! branch
    (λ (b port)
      (fprintf port "#<branch ~a ~a ~a>" (node-addr (branch-entry b))
               (node-addr (branch-then-result b)) (node-addr (branch-else-result b)))))

  ; cf. M:781-783 (`active = if testv then th else el`): the result node of the branch that the
  ; entry's current value selects.
  (define (branch-active-result b)
    (if (node-value (branch-entry b)) (branch-then-result b) (branch-else-result b)))

  ; deviation 9 (S:275 `if_ :: SExp Bool -> ...`): the test must be a boolean, checked when the
  ; entry is created and whenever it is updated; it is not coerced to a truth value (plan C7).
  ; Answers v.
  (define (check-test who v . irritants)
    (unless (boolean? v)
      (apply hakaru10-error 'branch who "the test of if_ must be a boolean" v irritants))
    v)

  ; cf. M:696-751, the folds `Set.foldl' deactivate1` and `Set.foldl' activate1` (M:700,
  ; M:728) that deactivate and activate share: walk the nodes of (lo, hi] in ascending address
  ; order, moving each one's deactivation count by `step` (+1 or -1, journaled).  A node whose
  ; old count is `switching` (0 for deactivate: "unless the node was already inactive", M:688;
  ; 1 for activate: IntMap's `updateLookupWithKey` answers the count *before* the update,
  ; M:733-746) changes activity: it adds its weight to llc, `(observed-weight n)` if it is
  ; observed and 0 otherwise ("We look only at Conditioned nodes", M:690-695, M:707-711,
  ; M:736-740), counts in k if it is resampleable (M:712-715, M:741-744), and is passed to
  ; `dirac-switched` if it is a Dirac node (deviation 11, see activate!).  Answers (values llc
  ; k).  A count driven below 0 is upstream's "cannot happen" (M:747-751): an internal error.
  ; A pending observed node ('extended, deviation 8) weighs 0.: adding 0. to llc changes no bit,
  ; llc starting at 0. and so never being -0.
  (define (move-activity! ch lo hi step switching observed-weight dirac-switched)
    (let loop ((a (fx+ lo 1)) (llc 0.) (k 0))
      (if (fx> a hi)
          (values llc k)
          (let* ((n (chain-node ch a)) (count (node-inactive n)) (kind (node-kind n)))
            (when (fx< (fx+ count step) 0)
              (hakaru10-error 'internal 'activate "activating an active node" n))
            (set-inactive! n (fx+ count step))
            (if (fx= count switching)
                (loop (fx+ a 1)
                      (fp+ llc (case kind
                                 ((observed) (observed-weight n))
                                 ((dirac) (dirac-switched n) 0.)
                                 (else 0.)))
                      (if (eq? kind 'resampleable) (fx+ k 1) k))
                (loop (fx+ a 1) llc k))))))

  ; cf. M:688-717 `deactivate`: "unless the node was already inactive, remove its (conditioning
  ; weight) contribution from llcum", `llcum s - llc`, and `card_change s - inactivated`
  ; (M:701-702).
  ; deviation 8 (M:707-710, plan §6.8): in 'extended mode an observed node activated earlier in
  ; the same step (pending, see activate!) has no weight in llcum yet: its flag is cleared and
  ; nothing is subtracted.
  (define (deactivate! ch lo hi)
    (receive (llc k) (move-activity! ch lo hi 1 0
                                     (λ (n)
                                       (if (node-pending n)
                                           (begin (set-pending! n #f) 0.)
                                           (nstate-ll (node-state n))))
                                     void)
      (chain-llcum-set! ch (fp- (chain-llcum ch) llc))
      (chain-card-change-set! ch (fx- (chain-card-change ch) k))))

  ; cf. M:719-751 `activate`: "if the node becomes active (its deactivation count goes to 0),
  ; remove it from the inactive map and add its conditioned LL to llcum", `llcum s + llc`, and
  ; `card_change s + activated` (M:729-730).
  ; deviation 8 (M:735-737, plan §6.8): in 'extended mode an activated observed node's weight is
  ; not added now but when the step's propagation ends (settle-pending!), with the LL the node
  ; has then.  The sum is the same, stale LL plus the change made by the node's own update in
  ; this step, but an inactive observed node may hold a -inf LL in 'extended mode (its updates
  ; run and do not count), and -inf + (ll - -inf) would be NaN, rejecting a valid proposal.
  ; deviation 11 (M:99, M:120-129): an activated Dirac node (a diracN, a root of a Val, an
  ; exit) that holds a deferred value, computed while it was inactive, joins the pending list
  ; too, in both semantics: when the propagation ends, settle-pending! forces the value, unless
  ; an update of the step has recomputed it, as an active diracN or exit computes its value at
  ; once (doupdate, update-exit) where upstream forces it only when it is read.  Not now: a
  ; parent updated later in the step (queued, or the child of a queued node) may give it
  ; another value, and a delayed update ('upstream) runs after the switch.
  (define (activate! ch lo hi)
    (receive (llc k) (move-activity! ch lo hi -1 1
                                     (if (extended? ch)
                                         (λ (n) (set-pending! n #t) 0.)
                                         (λ (n) (nstate-ll (node-state n))))
                                     (λ (n)
                                       (when (deferred? (node-value n))
                                         (chain-pending-set! ch (cons n (chain-pending ch))))))
      (chain-llcum-set! ch (fp+ (chain-llcum ch) llc))
      (chain-card-change-set! ch (fx+ (chain-card-change ch) k))))

  ; deviation 8 (M:735-737, plan §6.8): at the end of a step's propagation in 'extended mode, add
  ; to llcum the current LL of every observed node activated during the step and still active
  ; (in ascending address order), clearing their pending flags.
  ; deviation 11 (M:99, M:120-129): in both semantics, force the deferred value that a Dirac
  ; node activated during the step and still active still holds (see activate!): no update of
  ; the step recomputed it, so it is the value of the node's current parents, and forcing it
  ; raises its error again (the node's function, applied to the same values, raised it), which
  ; escapes the step as an active node's error does.  Should the function answer this time,
  ; its value is stored, with the node's tstamp.
  (define (settle-pending! ch)
    (let1 (pending (chain-pending ch))
      (when (pair? pending)
        (for-each (λ (n)
                    (cond
                      ((node-pending n)
                       (set-pending! n #f)
                       (chain-llcum-set! ch (fp+ (chain-llcum ch) (nstate-ll (node-state n)))))
                      ((and (eq? (node-kind n) 'dirac) (not (node-inactive? n)))
                       (let1 (s (node-state n))
                         (when (deferred? (nstate-val s))
                           (set-node-state! n (make-nstate (force-value (nstate-val s))
                                                           (nstate-ll s) (nstate-dst s)
                                                           (nstate-tstamp s))))))))
                  (sort! pending (λ (a b) (node<? a b))))  ; a duplicate finds its flag cleared
        (chain-pending-set! ch '()))))

  ; cf. M:767-775 `switch_branches`: deactivate the new inactive range, then activate the new
  ; active one, then take the delayed updates of the newly active range (`Set.intersection
  ; newactive (delayed_updates s)`), clearing their flags (journaled), and answer them as extra
  ; dependents of the entry (`Set.union deps newdeps`).  Ranges are pairs (lo . hi).
  ; Kept as upstream in 'upstream mode, bias included: replaying a delayed update of a
  ; *resampleable* node charges to the switching step the LL changes of the steps it was
  ; inactive in, which were accepted without them; M:1159-1165 argues they cancel, assuming
  ; reactivated nodes are fresh prior draws, but their stale values are reused (plan §6.8, the
  ; `pswitch` model: P(c) = 0.377286 instead of 1/2).
  ; deviation 8 (M:771-774, plan §6.8): in 'extended mode no update is ever delayed, so there
  ; is no newdeps and the scan is skipped.
  (define (switch-branches! ch s* extra newactive newinactive)
    (deactivate! ch (car newinactive) (cdr newinactive))
    (activate! ch (car newactive) (cdr newactive))
    (values s*
            (if (extended? ch)
                extra
                (chain-fold (λ (n deps)
                              (if (node-delayed n)
                                  (begin (set-delayed! n #f) (cons n deps))
                                  deps))
                            extra ch (car newactive) (cdr newactive)))))

  ; cf. M:760-765 `update_entry prev_update (ths,els)`: run the entry's own update (doupdate of
  ; the test, S:278); if the test's value is unchanged answer its result, else switch branches
  ; (M:765) with (newactive, newinactive) = (ths, els) when the new test is true, (els, ths)
  ; otherwise, the ranges of the branch record b.
  (define ((update-entry prev b) n)
    (let1 (old (node-value n))
      (receive (s* extra) (prev n)
        (let ((tv (nstate-val s*)) (ths (branch-then-range b)) (els (branch-else-range b)))
          (check-test 'if_ tv n)
          (cond
            ((eq? tv old) (values s* extra))
            (tv (switch-branches! (node-chain n) s* extra ths els))
            (else (switch-branches! (node-chain n) s* extra els ths)))))))

  ; cf. M:778-794 `update_exit entry (th,el)`: the exit takes the value of the active branch's
  ; result (branch-active-result of the branch record b), with a new tstamp (M:793), provided
  ; the entry or that result is newer than the exit.
  ; deviation 1 (M:788-792): otherwise upstream fails ("the parents of the exit node ... must
  ; have been updated"); here the exit answers itself unchanged, so its tstamp is not bumped
  ; and nothing propagates (plan C12).  It happens when a node shared by the inactive branch
  ; changes: `c ~ bern .5; d ~ normal 0 1; if_ c d (dist normal 5 1)` with c false enqueues the
  ; exit through inactive_nr -> exit (M:673) when d is resampled.
  ; An active exit forces a deferred value of the result (M:793 `val = nref_val ndr` is lazy,
  ; but the exit's own readers are active); an inactive one, of a nested if_, copies it.
  (define ((update-exit b) n)
    (let* ((self (node-state n))
           (ts (nstate-tstamp self))
           (e (node-state (branch-entry b)))
           (result (node-state (branch-active-result b))))
      (values (if (or (fx> (nstate-tstamp e) ts) (fx> (nstate-tstamp result) ts))
                  (make-nstate (let1 (v (nstate-val result))
                                 (if (node-inactive? n) v (force-value v)))
                               (nstate-ll self) (nstate-dst self) (chain-now (node-chain n)))
                  self)
              '())))

  ; cf. M:696-717 `deactivate`, at construction (M:659 "which rolls back llcum"): deactivate!
  ; over (lo, hi]; the journal is #f during construction, so its setters are raw writes.
  (define (deactivate lo hi) (modify! (ch) (deactivate! ch lo hi)))

  ; deviation 10 (the construction scope, no upstream counterpart: Haskell's lexical scoping)
  ; and deviation 11 (lazy lifting): run the TraceM m in the scope extended with (entry .
  ; polarity), polarity #t for the then branch, so that every node it creates records it
  ; (node-scope, the branch-escape check and inactive-scope?), and under lazy-lifting when the
  ; branch is inactive.  A State run is a direct call, so every bind of m, and the evaluation
  ; of the branch's delayed expression (C36), runs inside the parameterize.  An error leaves
  ; the scope pushed, on a chain that build-chain discards.  Answers m's answer, the branch's
  ; result node.  One transition, written with `state`, as create-node (plan §15.6).
  (define (in-branch entry polarity m)
    (state (λ (ch)
             (let1 (scope (chain-scope ch))
               (chain-scope-set! ch (cons (cons entry polarity) scope))
               (let1 (r (if (inactive-scope? ch)
                            (parameterize ((lazy-lifting #t)) (run-state m ch))
                            (run-state m ch)))
                 (chain-scope-set! (cdr r) scope)
                 r)))))

  ; cf. M:641-686 `ifnode entry th el`, line by line: th and el are TraceMs (S:279 `th >>=
  ; to_node`).  The entry must not be sampleable (M:643-647; "When a node is resampled, its
  ; update procedure is not run"); sampleable nodes are exactly the resampleable ones, so the
  ; kind is checked, and the test must be a boolean (deviation 9, as in if_*).  The inactive
  ; branch is built first (M:651-652) and deactivated at once (M:659; its range may be empty,
  ; M:655-657), then the active one (M:661); a branch's nodes are the addresses created while
  ; it runs, (added-old, added-inactive] and (added-inactive, added-active], replacing `added`
  ; (M:650-663).  The exit node, a Dirac of the active result's value updated by update-exit,
  ; depends on the entry, the active result and the inactive result, in this order
  ; (M:665-673).  The entry's update, if any, is wrapped by update-entry (M:679-685; a
  ; constant test has none, and Haskell's lazy `Just prev_upd` is then never forced).  Entry
  ; and exit share one branch record.  Answers the exit node.
  (define (ifnode entry th el)
    (do/monad
      (when/monad (eq? (node-kind entry) 'resampleable)                           ; M:643-647
        (hakaru10-fail 'internal 'if_ "Entry node for if should not be sampleable" entry))
      (let testv (check-test 'if_ (node-value entry) entry))                      ; M:648
      (let (,active ,inactive) (if testv (list th el) (list el th)))             ; M:649
      (← added-old (gets chain-count))                                            ; M:650
      (← inactive-nr (in-branch entry (not testv) inactive))                      ; M:651-652
      (← added-inactive (gets chain-count))                                       ; M:653-654
      (deactivate added-old added-inactive)                                       ; M:655-659
      (← active-nr (in-branch entry testv active))                                ; M:660-661
      (← added-active (gets chain-count))                                         ; M:662-663
      ; M:665-666 and M:679-680: the results and the ranges, in then/else order.
      (let b (if testv
                 (make-branch entry active-nr inactive-nr
                              (cons added-inactive added-active) (cons added-old added-inactive))
                 (make-branch entry inactive-nr active-nr
                              (cons added-old added-inactive) (cons added-inactive added-active))))
      (← exit (create-node (update-exit b) (dirac (node-value active-nr)) 'exit   ; M:667-669
                           (list->vector (delete-duplicates (list entry active-nr inactive-nr) eq?))
                           #f))
      (modify! (ch) (node-branch-set! exit b))
      (new-dependency entry exit)                                                 ; M:670-673
      (new-dependency active-nr exit)
      (new-dependency inactive-nr exit)
      (modify! (ch)                                                               ; M:678-685
        (let1 (prev (node-update entry))
          (when prev (node-update-set! entry (update-entry prev b))))
        (node-branch-set! entry b))
      (return exit)))                                                             ; M:686

  ;; ---------------------------------------------------------------------------------------
  ;; Propagation (M:258-317; P §4.2 p.15-16 l.765-800)
  ;; ---------------------------------------------------------------------------------------

  ; cf. M:258-271 `modify_node`: store the new state (journaled) and add its LL difference to
  ; llcum, `llcum s + ll n - ll nold`; answer the dependents if the value changed, i.e. the
  ; tstamp differs (M:269-271), else none.
  ; deviation 8 (M:268, plan §6.8): in 'extended mode the LL of an inactive observed node does
  ; not count (the extended target weighs only active observations), and neither does that of
  ; an observed node activated during this step, whose final LL settle-pending! adds; an
  ; inactive resampleable node's LL change counts (it is an auxiliary variable with its prior).
  (define (modify-node! n s*)
    (let ((ch (node-chain n)) (old (node-state n)))
      (set-node-state! n s*)
      (unless (and (extended? ch)
                   (eq? (node-kind n) 'observed)
                   (or (node-inactive? n) (node-pending n)))
        (chain-llcum-set! ch (fp- (fp+ (chain-llcum ch) (nstate-ll s*)) (nstate-ll old))))
      (if (fx= (nstate-tstamp old) (nstate-tstamp s*)) '() (node-deps n))))

  ; deviation 8 (M:298-303, plan §6.8, C22): run the update of an inactive node in 'extended
  ; mode, answering its (values nstate' extra), or (values #f '()) if it raised an error that
  ; deferrable? accepts: an (exn hakaru10) error of kind `parameter`, or any non-hakaru10 error,
  ; whether raised by the node's own function (an NT applying a partial function) or by forcing
  ; a deferred value (force-value; an inactive diracN node defers its own errors, lazy-value),
  ; the criterion of build-guard.  (exn hakaru10) errors of other kinds propagate.
  (define (guarded-update upd n)
    (condition-case (upd n)
      (e (exn) (if (deferrable? e) (values #f '()) (signal e)))))

  ; deviation 8 (plan C22): reject the step from within propagation: Δ := -inf, and the
  ; queue and the pending observed nodes are dropped (their flags are journaled, and the
  ; rejection rolls them back).
  (define (abort-propagation! ch)
    (chain-llcum-set! ch -inf.0)
    (chain-pending-set! ch '())
    (update-queue-clear! (chain-queue ch)))

  ; cf. M:290-317 `propagate_update`: process the queue smallest address first.  An inactive
  ; node's update is delayed (M:298-303), in 'upstream mode; a node already updated in this
  ; step is a "dependency violation" (M:307-308); a node with no update is skipped (M:311);
  ; otherwise its update runs and both its extra dependents and, if its value changed, its
  ; regular dependents are queued (M:312-317).
  ; deviation 8 (M:298-303, plan §6.8, C22): in 'extended mode inactive nodes are updated too
  ; (no update is delayed), and an (exn hakaru10) error of kind `parameter`, or any
  ; non-hakaru10 error (deferrable?, guarded-update), raised by an inactive node's update
  ; rejects the step: llcum := -inf and propagation stops.  The extended target is thus
  ; restricted to the states where every inactive node has valid parameters (an invalid one
  ; that only makes the LL -inf, e.g. gamma with shape <= 0, is excluded the same way).  Other
  ; (exn hakaru10) errors, and any error of an active node, propagate (upstream never runs
  ; the inactive update, M:298-303; an active node's error is upstream's too).  When
  ; the queue is empty, the weights of the observed nodes activated in this step are added
  ; (settle-pending!), which in both semantics also forces the deferred values that the Dirac
  ; nodes activated in this step still hold (deviation 11).
  (define (propagate! ch)
    (let ((q (chain-queue ch)) (extended (extended? ch)) (now (chain-now ch)))
      (let loop ()
        (if (update-queue-empty? q)
            (settle-pending! ch)
            (let1 (n (dequeue! q))
              (cond
                ((and (not extended) (node-inactive? n))
                 (set-delayed! n #t)
                 (loop))
                ((fx>= (nstate-tstamp (node-state n)) now)
                 (hakaru10-error 'dependency 'propagate
                                 "dependency violation: updating the already updated" n))
                ((node-update n)
                 => (λ (upd)
                      (receive (s* extra) (if (and extended (node-inactive? n))
                                              (guarded-update upd n)
                                              (upd n))
                        (if s*
                            (begin
                              (enqueue-all! ch extra)
                              (enqueue-all! ch (modify-node! n s*))
                              (loop))
                            (abort-propagation! ch)))))
                (else (loop))))))))

  ;; ---------------------------------------------------------------------------------------
  ;; One MH step (M:895-977)
  ;; ---------------------------------------------------------------------------------------

  ; (debug tracing) replaces `debug_msg`/debugM (M:196, M:216-217): when true, transit!
  ; writes one line per step to the current error port.
  (define hakaru10-debug (make-parameter #f))

  ; (helper) print the arguments as one line on the error port, when hakaru10-debug is true.
  (define-syntax-rule (debug-trace arg ...)
    (when (hakaru10-debug)
      (with-output-to-port (current-error-port) (τ (print arg ...)))))

  ; cf. M:976 `modify_node $ Mem.modify_ref n{val=v,ll=llv,tstamp=now s} nr`: install the
  ; proposed value of node n with a new tstamp and queue its dependents; answers fb.
  (define (apply-proposal! n v ll fb)
    (let ((ch (node-chain n)) (s (node-state n)))
      (enqueue-all! ch (modify-node! n (make-nstate v ll (nstate-dst s) (chain-now ch))))
      fb))

  ; cf. M:956-977 `resample_trace`: pick a resampleable node uniformly, `randomS (0, length
  ; resampleable - 1)` indexing the list (M:962), retrying (and drawing again) if it is
  ; inactive (M:966); resample it with its own distribution's kernel (M:972) without running its
  ; update (M:643-645); answers fwd - bwd.  There must be an active resampleable node.
  (define (resample-trace! ch)
    (let* ((samp (chain-samp ch))
           (hi (fx- (vector-length samp) 1))
           (src (chain-rng ch)))
      (let retry ()
        (let1 (n (vector-ref samp (rng-integer! src 0 hi)))
          (if (node-inactive? n)
              (retry)
              (let1 (s (node-state n))
                (receive (v ll fb)
                         ((rdistr-resample (nstate-dst s)) src (nstate-val s) (nstate-ll s))
                  (debug-trace "resample " (node-addr n) ": " (nstate-val s) " -> " v " fb " fb)
                  (apply-proposal! n v ll fb))))))))

  ; cf. M:929-934: the log acceptance ratio, `lldiff - fwd_bwd + (if carddiff == carddiff_old
  ; then 0 else log (fromIntegral (sampleables + carddiff_old) / fromIntegral (sampleables +
  ; carddiff)))`, in Haskell's association.  The cardinality term is derived at M:1039-1089.
  (define (log-acceptance-ratio ch sampleables card-old fb)
    (let1 (card-new (chain-card-change ch))
      (fp+ (fp- (chain-llcum ch) fb)
           (if (fx= card-new card-old)
               0.
               (fplog (fp/ (exact->inexact (fx+ sampleables card-old))
                           (exact->inexact (fx+ sampleables card-new))))))))

  ; cf. M:853-854 `rejectedLL = isInfinite`.
  (define (rejected-ll? x) (and (not (nan? x)) (infinite? x)))

  ; (helper) call the chain's step hook, if any (plan §6.9).
  (define (run-step-hook ch accepted? delta)
    (let1 (hook (chain-step-hook ch)) (when hook (hook ch accepted? delta))))

  ; cf. M:908-945 `transit`: advance `now` and zero llcum (M:915), resample one node
  ; (M:919), propagate (M:921), then accept or reject: an infinite LL difference rejects
  ; (M:927); a ratio >= 0 accepts without drawing; otherwise draw u and accept if u < exp ratio
  ; (M:939-944; a NaN ratio draws u and rejects).  A rejection restores the state but not the
  ; seed (M:904-906).  Answers whether the step was accepted.
  ; deviation 6 (M:962-966): with no active resampleable node upstream loops forever (or
  ; indexes []); here the step is a no-op, counted as accepted.
  ; deviation 7 (M:962, a consequence of 6): with no resampleable node at all (sampleables =
  ; 0) every step is a no-op, so the chain is constant.
  ; (no upstream counterpart) an error that escapes a step (an update, the proposal) leaves its
  ; journal open; the next step first rolls it back (journal-begin!), so it starts from the
  ; last accepted state, as after a rejection (the step hook is not called for the failed step).
  (define (transit! ch)
    (journal-begin! ch)
    (chain-now-set! ch (fx+ (chain-now ch) 1))
    (chain-llcum-set! ch 0.)
    (let ((card-old (chain-card-change ch))
          (sampleables (vector-length (chain-samp ch))))
      (if (fx= 0 (fx+ sampleables card-old))
          (begin
            (journal-commit! ch)
            (run-step-hook ch #t 0.)
            #t)
          (let* ((fb (resample-trace! ch))
                 (delta (begin (propagate! ch) (chain-llcum ch)))
                 (ratio (and (not (rejected-ll? delta))
                             (log-acceptance-ratio ch sampleables card-old fb)))
                 (accept? (and ratio
                               (or (fp>= ratio 0.)
                                   (fp< (rng-double! (chain-rng ch) 0. 1.) (fpexp ratio))))))
            (debug-trace "step " (chain-now ch) ": delta " delta " ratio " ratio
                         (if accept? " accepted" " rejected"))
            (if accept? (journal-commit! ch) (journal-rollback! ch))
            (run-step-hook ch accept? delta)
            accept?))))

  ;; ---------------------------------------------------------------------------------------
  ;; The driver (M:845-906, S:196-197)
  ;; ---------------------------------------------------------------------------------------

  ; (new, plan §6.9) the LL of the extended target ('extended semantics, plan §6.8) from
  ; scratch, in ascending address order: the LL of every resampleable node plus that of every
  ; active observed node.
  (define (chain-extended-ll ch)
    (chain-fold (λ (n acc)
                  (case (node-kind n)
                    ((resampleable) (fp+ acc (nstate-ll (node-state n))))
                    ((observed) (if (node-inactive? n) acc (fp+ acc (nstate-ll (node-state n)))))
                    (else acc)))
                0. ch))

  ; cf. M:872-880 `initial_step' g pgm = runState pgm init_state` and S:196-197: run the TraceM
  ; prog (model->trace) on a fresh chain over src and freeze the graph.  Answers (values ch
  ; result).  lazy-lifting starts #f, also for a build started from inside an inactive branch's
  ; lifted function (a lifted function calling exact-posterior or mcmC).
  ; deviation 8 (M:650, M:885, plan §6.8, C22): in 'extended mode a build rejected by
  ; build-guard answers result #f with llcum -inf (initial-step then builds again), and the
  ; llcum of a complete build is recomputed as the extended LL: the incremental sum adds an
  ; inactive observed node's LL and subtracts it at deactivation (M:659), which is NaN, not 0,
  ; when that LL is -inf.
  (define (build-chain prog src semantics)
    (check-model prog 'mcmC)
    (let* ((ch (new-chain src semantics))
           (result (condition-case (parameterize ((lazy-lifting #f)) (eval-state prog ch))
                     (e (hakaru10-rejected-build) #f))))
      (chain-freeze! ch)
      (cond
        ((not result) (chain-llcum-set! ch -inf.0))
        ((extended? ch) (chain-llcum-set! ch (chain-extended-ll ch))))
      (values ch result)))

  ; cf. M:882-891 `initial_step`: "If the rejection-sampling conditioning is present ..., the
  ; initial trace may be rejected.  If so, repeat up to limit times": a build whose llcum is
  ; infinite is retried on the same (advanced) source, at most `limit` builds in all (the
  ; build for limit 0 is never forced upstream).  Answers (values ch result limit'),
  ; limit' = 0 meaning no usable trace (ch and result are then #f).
  (define (initial-step limit prog src semantics)
    (let retry ((limit limit))
      (if (<= limit 0)
          (values #f #f 0)
          (receive (ch result) (build-chain prog src semantics)
            (if (rejected-ll? (chain-llcum ch))
                (retry (- limit 1))
                (values ch result limit))))))

  ; cf. M:856-869 `mcmc :: Integer -> TraceM a -> [a]` and M:897-906 `transition`, as a
  ; generator over the TraceM prog (model->trace): the first value is the initial trace's,
  ; then each call takes one MH step and answers the result's value, a rejected step repeating
  ; the previous one.  `rng` (an rng source) overrides `seed` (mkStdGen 17, M:858) and is used
  ; as is.  If the initial step finds no trace in `limit` builds, the answer is #f (upstream's
  ; `[]`, M:868).
  ; deviation 9 (S:196, M:856 `mcmc :: Integer -> ...`): a `limit` that is not an exact
  ; integer, such as 2.5, 1/2 or +inf.0, is a `construction` error, as the type check is
  ; upstream.
  ; deviation 10 (M:885-891, M:897-898, no upstream counterpart): a negative `limit` is a
  ; `construction` error.  Upstream stops only at limit 0 (`go 0`, `transition _ _ 0`), so it
  ; would retry the build forever or answer an infinite list.
  (define (mcmc-generator limit prog #!key (seed 17) (rng #f) (semantics 'upstream)
                          (step-hook #f) (build-hook #f))
    (unless (and (exact-integer? limit) (>= limit 0))
      (hakaru10-error 'construction 'mcmC "limit: must be a non-negative integer" limit))
    (receive (ch result limit*) (initial-step limit prog (or rng (make-stdgen-source seed))
                                              semantics)
      (and ch
           (let1 (started #f)
             (chain-step-hook-set! ch step-hook)
             (when build-hook (build-hook ch))
             (τ (if started (transit! ch) (set! started #t))
                (force-value (node-value result)))))))

  ; cf. M:856-869 `mcmc`: fold kons over the chain's first `limit` samples, counted with the
  ; original limit (M:869), so limit - 1 steps run; knil if there is no initial trace (M:868).
  ; The keys are mcmc-generator's, which rejects a limit that is not an exact integer
  ; (deviation 9) or is negative (deviation 10).
  (define (mcmc-fold kons knil limit prog . keys)
    (let1 (next (apply mcmc-generator limit prog keys))
      (if next
          (let loop ((i 0) (acc knil))
            (if (>= i limit) acc (loop (+ i 1) (kons (next) acc))))
          knil)))

  )
