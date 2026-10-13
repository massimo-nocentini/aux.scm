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
;; Metropolis.hs writes on the raw trace API (with_node, next, ifnode), which the port does not
;; expose; each has a surface-syntax counterpart in test/hakaru10*.scm:
;;   M:320-361 upp1/upp1r (live upstream; its surface-syntax text M:327-333 is commented
;;        out) and M:387-397 upp2/upp2r (commented out upstream): the propagation-order note
;;        M:319-385, implemented by the update queue below (smallest address first) and
;;        checked on every step by propagate!'s "updating the already updated" assertion
;;        (M:307-308, a node updated twice; a Dirac one, whose tstamp the update advances) and
;;        doupdate's "dependency violation!" (S:240-243, a node updated before its parents);
;;        a wrong order also makes the reference evaluator of `#:check` (test/hakaru10.scm,
;;        B4) find a stale value.  Case B8 of test/hakaru10.scm runs upp1 and upp2 (a and b
;;        both feeding the non-Dirac d), transcribed into models, under `#:check`, records
;;        the updates of every step (d once, after b and c), and asserts the incrementality
;;        of P §4.2 (l.771-788, pdep: resampling x does not update z), which the reference
;;        evaluator, comparing final states, cannot see;
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
;; Embedding (user directive of 2026-10-12; PLAN-IDIOM C25-C28).  A model is a thunk, a
;; direct-style Scheme program that *performs* three operations, `dist`, `diracN` and `if`;
;; what they mean is up to the handler that runs it.  `handle-model` runs the model under the
;; native `reset` of (chicken continuation), and `perform` is a `shift` to it: the handler gets
;; the request, a list (op arg ...), and `resume`, the model's continuation up to the `reset`,
;; which it may call once (the trace builder below), many times or lazily (the exact enumerator
;; of (aux hakaru10 exact)).  Upstream's `Model a = MCMCM (SExp a)` (S:82) is the State monad
;; threading the sampler state through construction; here the trace builder holds that state,
;; the chain.  `condition` and `lift` are not operations: conditioning makes an observed distk
;; (S:174-193), which `dist` performs, and lifting is pure (S:99-116).
;; Node creation order is load-bearing (topological order, P l.791-794): models must create
;; nodes in sequence, with `let*`, `fold`, `map-in-order` or a named `let`, never in
;; procedure-argument positions or inside `map` (plan C18, C18').  A handler that resumes many
;; times shares, between its resumptions, any state the model mutates across an operation, so
;; models must not do so (C28).
;; Runtime requirement: multi-shot resumption (exact enumeration) of a model that sets up
;; `guard`, `handle-exceptions` or `condition-case` around an operation needs a CHICKEN whose
;; call/cc escapes inside a resumed segment return to that resumption: chicken-core commits
;; f11f304b, b29737e8 and 0d398032, on the branch delimcc-multishot (developed as
;; hakaru10-support; not in main up to rev 55615603).  The module runs on both.
;; native-multishot-escapes? tests it at load; where it fails, handle-model reports a
;; misrouted escape under a lazy handler as a `construction` error instead of answering a
;; wrong posterior.  One-shot handlers (mcmC, the forward runs) do not need it.
;;
;; ENGINE API (the public module (aux hakaru10) and (aux hakaru10 exact) use only what is listed
;; here).  A node is a record (the trace DAG's vertex); its address, from 1, is its creation
;; order.  `ch` is a chain, `n` a node.
;;
;;   Effects and handlers (native shift/reset of (chicken continuation))
;;     (handle-model model handler [return])   run the thunk `model` under `handler`, a
;;                             procedure (λ (request resume) ...): `request` is (dist d args),
;;                             (diracN e) or (if test th el), `resume` (λ (value) ...) answers
;;                             what handle-model answers.  `return` (default identity) maps the
;;                             model's own value, inside the `reset`.  A handler runs outside the
;;                             model, with current-handler as outside it.
;;     (perform op arg ...)    perform the request (op arg ...) (a `construction` error outside
;;                             a model or in pure context).
;;     native-multishot-escapes?   #t if this CHICKEN returns a call/cc escape inside a
;;                             resumed segment to the resumption (see Runtime requirement).
;;     current-handler         parameter: the handler of the running model, #f elsewhere (in
;;                             particular inside pure-call and inside every handler).
;;     (pure-call thunk)       run user code (lifted functions, constructors) in pure context.
;;     (check-model model who) a `construction` error, located at who, unless model is a
;;                             procedure (a model is a thunk, S:82).
;;     lazy-lifting            parameter: #t while a branch that is inactive is built (by the
;;                             trace builder, or by a handler that runs the branch its test does
;;                             not select); lift* then defers the errors of its function (S:100,
;;                             M:99).  handle-model starts every model with it #f.
;;     (dist d arg ...)        S:137-151, performs (dist d args): d a distk (no args) or a distk
;;                             constructor (a procedure) of SExps/Vals; resumes with the choice.
;;     (diracN e)              S:161-169, performs (diracN e).
;;     (if_* test th el)       S:275-279, performs (if test th el), th and el models (thunks),
;;                             and calls the thunk it is resumed with: the handler answers the
;;                             computation that realizes the branch (C27).
;;     (condition/external obs d)  S:174-193, with "observation must be external" (the public
;;                             `condition`).
;;     For handlers under which SExps are plain values (an exact enumerator, a forward sampler:
;;     no node exists, so `lift*` evaluates at once and every argument is a Val):
;;     (effect-distk d args)   the distk of the request (dist d args), the constructor applied
;;                             in pure context to the unboxed (val-of) arguments.
;;     (select-branch test th el)  the branch thunk the boolean test selects (`branch` error
;;                             for a non-boolean test).
;;     (value-handler on-dist #!key (on-if select-branch))   the handler that answers (diracN
;;                             e) with e, a Val, and (if test th el) with (on-if test th el),
;;                             and hands (dist d args) to (on-dist distk resume); resume
;;                             boxes a procedure value with `constant`, so that it stays a Val.
;;   SExp (S:85-93): an SExp is a node (NRef), an nt (NT) or any other value (Val)
;;     (sexp? x)  (node? x)  (nt? x) (nt-leaves e) (nt-fn e): leaves is a vector of distinct
;;                             nodes, fn (λ (values-vector) value), values aligned with leaves.
;;     (constant x)            an explicit Val (needed only to pass a procedure value);
;;     (val-of x)              unboxes a `constant`, else x.
;;     (sexp-chain e) (sexp-leaves e) (sexp-value e)   owner, leaves (a vector), value
;;                             (eval_nctor, S:201-208).
;;     (leaf-values leaves)    the values-vector of a vector of nodes (reeval_tree, S:264-273).
;;     (deferred? x) (force-value x)   the value of a diracN node computed while it is
;;                             inactive may be a deferred value (an error, raised again when it is
;;                             forced; M:99, M:120-129): force-value answers x, or forces it.
;;     (lift* f es)            Applicative lifting (S:99-116): a Val if no e is an sexp (f
;;                             applied now, in pure context), otherwise one fused nt.
;;   Trace building (the trace-builder handler, M:399-526, S:137-279)
;;     (trace-builder ch)      the handler that builds the trace graph of ch.
;;     (run-model! ch model)   run model under (trace-builder ch) and turn its result into a
;;                             node (`m >>= to_node`, S:197); answers the node.
;;     (create-node! ch role parents fn dk update)   M:404-419, M:233-243 -> node
;;     (add-dependency! parent child)                 M:246-250 (with the branch-escape check)
;;     (with-node-ctor! ch e df [who lazy])           S:218-273 -> node
;;     (to-node! ch e)                                S:211-214 -> node
;;     (check-owner ch e who)  e must be a Val or an sexp of ch (`construction`).
;;     (chain-scope ch) (chain-scope-set! ch s)       the construction scope, a list of
;;                                                    (entry-node . polarity), innermost first.
;;   Branches (M:641-794, S:275-279)
;;     (ifnode! ch entry th el)  M:641-686: entry a node, th and el thunks answering the branch
;;                             results' nodes; answers the exit node.
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
;;     (enqueue-all! ch nodes) (propagate! ch)   propagate! ends, in 'extended mode, with
;;                             (settle-pending! ch) (see Semantics below)
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
;;   Driver (M:856-906, S:196-197)
;;     (build-chain model src semantics) -> (values ch result); result is the node whose value
;;                             is sampled, #f for a build rejected in 'extended mode (C22),
;;                             where llcum is chain-extended-ll.
;;     (initial-step limit model src semantics) -> (values ch result limit')  M:882-891
;;     (mcmc-generator limit model #!key (seed 17) (rng #f) (semantics 'upstream)
;;                     (step-hook #f) (build-hook #f))
;;                             #f if no initial trace is found in `limit` builds, else a
;;                             generator: a thunk answering the initial trace's value, then one
;;                             value per MH step.  build-hook, if given, is called once as
;;                             (build-hook ch) when the generator is made.  A negative limit
;;                             is a `construction` error (deviation 10).
;;     (mcmc-fold kons knil limit model #!key ...)   (kons sample acc) over the `limit` first
;;                             samples, in chain order (the same keys).

(module (aux hakaru10 engine) *

  (import scheme
          (only (scheme base) vector-map vector-for-each make-parameter)
          (chicken base)
          (chicken fixnum)
          (chicken flonum)
          (only (chicken continuation) shift reset)
          (only (chicken format) fprintf)
          (only (chicken platform) chicken-version)
          (only (chicken port) with-output-to-port)
          (only (chicken sort) sort!)
          (only (chicken condition) make-property-condition signal condition-case)
          (only srfi-1 any every find delete-duplicates append-map list-index iota)
          (only srfi-69 make-hash-table hash-table-exists? hash-table-set!)
          (aux base)
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
  ;   pending      the observed nodes activated during the step and still waiting for their
  ;                weight ('extended only, see activate!), newest first;
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

  ; cf. S:86 `Val :: a -> SExp a`, boxed: plan §5.1 makes a bare procedure an error in an SExp
  ; position (it is almost always a model passed where an SExp is expected), so a procedure Val
  ; must be boxed explicitly.
  (define-record constant value)

  ; (debug printer only) #<constant value>.
  (set-record-printer! constant
    (λ (e port) (fprintf port "#<constant ~s>" (constant-value e))))

  ; cf. S:85-89: whether x is an NRef or an NT; every other value is a Val.
  (define (sexp? x) (or (node? x) (nt? x)))

  ; cf. S:105 `pure = Val`, explicit: needed only to pass a procedure value.
  (define (constant x)
    (when (sexp? x) (hakaru10-error 'construction 'constant "already a stochastic expression" x))
    (make-constant x))

  ; (helper) the value of a Val, unboxing a `constant`.
  (define (val-of x) (if (constant? x) (constant-value x) x))

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
  ;; `if_ (lift null? xs) 'none (diracN (lift car xs))` runs upstream, although the inactive
  ;; else branch applies `car` to '().  Here values are computed eagerly, so the value of a
  ;; diracN node (to-node!) computed while the node is inactive, if it raises an error, is a
  ;; deferred value: the suspended application of the node's function to its parents' values,
  ;; Haskell's thunk.  It passes unforced through the inactive diracN nodes that read it; a
  ;; strict reader forces it, which raises the error again: any other node (resampleable,
  ;; observed, an entry, whose value ifnode reads, M:648-649), an active diracN node, an active
  ;; exit, and the sampled result.  Computing it draws nothing, so replays are unchanged.
  ;; deviation 11 (M:99, M:120-129, S:100-116; strict propagation of a deferred value): the
  ;; stand-in is coarser than Haskell's laziness.  A function applied, in an inactive context,
  ;; to a deferred value is not called: its value is that deferred value (lazy-value,
  ;; lift/apply), even when the function ignores the argument (`cdr` of (pair h 1.0), `length`
  ;; of a list, a constant function).  Upstream computes the real value there, never forcing
  ;; the argument; here a strict reader of the result (a resampleable or observed node's
  ;; distribution in the same inactive branch, say) forces it and raises the original error.
  ;; With xs = '(), the inactive branch
  ;;   (let* ((h (diracN (lift car xs))) (mu (diracN (lift cdr (pair h 1.0))))) (dist normal mu 1))
  ;; builds upstream (mu = snd (⊥, 1.0) = 1.0) and is an error here ('upstream mode; in
  ;; 'extended mode the error, being deferrable?, rejects the build or the step instead, C22).
  ;; Likewise an active diracN or exit computes its value at once, which upstream forces only
  ;; when it is read.  Case B6 of test/hakaru10.scm (strict-reader) shows both.
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

  ; (helper) force x if it is a deferred value: apply its function in pure context and answer
  ; the value, or raise the error again (deferrable?, as it was the first time: in 'extended
  ; mode an inactive node that forces it rejects, C22); any other x is answered as is.
  (define (force-value x)
    (if (deferred? x)
        (pure-call (τ ((deferred-fn x) (deferred-args x))))
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

  ; cf. S:100, S:107 `fmap f (Val x) = Val (f x)`, a thunk too: true while a branch that is
  ; inactive is built (by the trace builder, build-branch!, or by a handler that runs the
  ; branch its test does not select, as the construction-order run of `#:check` does), where
  ; lift* applying f to Vals defers its error as lazy-value does.  #f elsewhere: handle-model
  ; starts every model with it #f, also one run from inside an inactive branch.
  (define lazy-lifting (make-parameter #f))

  ; cf. S:201-208 `eval_nctor`: the current value of an SExp (forced, see force-value).
  (define (sexp-value e)
    (cond
      ((node? e) (force-value (node-value e)))
      ((nt? e) (pure-call (τ ((nt-fn e) (forced-values (leaf-values (nt-leaves e)))))))
      (else (val-of e))))

  ; deviation 9 (S:82, S:137-151): `Model a` and `SExp a` are distinct types upstream; here a
  ; bare procedure in an SExp position is rejected.
  (define (check-not-model who x)
    (when (procedure? x)
      (hakaru10-error 'construction who
                      "a model where an SExp is expected; bind it with let* first" x)))

  ; deviation 9 (S:85-93, S:134-135): a Scheme value that contains SExps (in pairs or
  ; vectors, e.g. a model that answers (list x y)) is not an `SExp a` upstream, where a tuple
  ; of SExps must be combined with `pair` (S:134); here it would silently be a Val whose
  ; value holds node records, so it is rejected.  x is a Val's value, which may be circular
  ; (e.g. srfi-1's circular-list, which the value handlers accept): once the walk has visited
  ; 100000 pairs and vectors, it records them in an eq? table and skips those already seen,
  ; so it ends on a cycle and stays allocation-free on the values of ordinary models.
  (define (check-no-sexp who x)
    (let ((budget 100000) (seen #f))
      ; whether the pair or vector x is to be walked: while the budget lasts, always; then
      ; only on its first visit since the budget ran out.
      (define (walk? x)
        (if (fx> budget 0)
            (begin (set! budget (fx- budget 1)) #t)
            (begin
              (unless seen (set! seen (make-hash-table eq?)))
              (and (not (hash-table-exists? seen x)) (begin (hash-table-set! seen x #t) #t)))))
      (let walk ((x x))
        (cond
          ((sexp? x) (hakaru10-error 'construction who
                                     "an SExp inside a Scheme value; combine with collect, pair or lift"
                                     x))
          ((pair? x) (when (walk? x) (walk (car x)) (walk (cdr x))))
          ((vector? x) (when (walk? x) (vector-for-each walk x)))))))

  ; deviation 9 (S:99-116, `fmap :: (a -> b) -> SExp a -> SExp b`): a lifted function must
  ; answer a plain value, not an SExp.
  (define (check-lifted-value who v)
    (when (sexp? v)
      (hakaru10-error 'construction who "a lifted function answered an SExp" v))
    v)

  ; cf. S:99-116 `instance Functor SExp`, `instance Applicative SExp` (fmap, <*>, liftA2,
  ; liftA3, ... in one): if no argument is an sexp, `fmap f (Val x) = Val (f x)` (S:100,
  ; S:107), f being applied now, in pure context (a procedure it answers is boxed with
  ; `constant`, so that it stays a Val); otherwise the arguments fuse into one NT
  ; (`fmap f (NT t g) = NT t (f . g)`, S:102, and the <*> cases S:110-116).  The "cannot
  ; happen" cases (S:108, S:111, S:115) disappear: f is a Scheme procedure.  An SExp that f
  ; answers is an error (check-lifted-value; for an NT, when its node is built or updated).
  ; Applied to Vals, f forces the deferred ones, but under lazy-lifting, where the first
  ; deferred Val is the answer (deviation 11: f is not called) and an error of f a deferred
  ; value (cf. S:100, M:99).
  (define (lift* f es)
    (for-each (λ (e) (check-not-model 'lift e)) es)
    (if (any sexp? es)
        (lift/fuse f es)
        (let1 (v (check-lifted-value 'lift (lift/apply f (map val-of es))))
          (if (procedure? v) (make-constant v) v))))

  ; (helper of lift*) f applied to the values vs, in pure context (see lift*).
  (define (lift/apply f vs)
    (cond
      ((not (lazy-lifting)) (pure-call (τ (apply f (forced-list vs)))))
      ((find deferred? vs))
      (else (pure-call (τ (call/deferring (λ (args) (apply f args)) vs))))))

  ; cf. S:110-116 (<*> on NTs, NTPair): one NT whose leaves are the union of the arguments'
  ; leaves, in first-occurrence order, and whose fn applies f to the arguments' values, each
  ; extracted from the union's values.  The leaves must belong to one chain.
  (define (lift/fuse f es)
    (let* ((leaves (delete-duplicates (append-map (λ (e) (vector->list (sexp-leaves e))) es) eq?))
           (owner (node-chain (car leaves)))
           (position (λ (n) (list-index (λ (m) (eq? n m)) leaves)))
           (extractors (map (λ (e) (lift/extractor position e)) es)))
      (unless (every (λ (n) (eq? (node-chain n) owner)) leaves)
        (hakaru10-error 'construction 'lift "nodes of different model runs" es))
      (make-nt (list->vector leaves) (λ (vs) (apply f (map (λ (x) (x vs)) extractors))))))

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
      (else (let1 (v (val-of e)) (if (deferred? v) (λ (vs) (force-value v)) (K v))))))

  ;; ---------------------------------------------------------------------------------------
  ;; Effects and handlers (user directive of 2026-10-12, PLAN-IDIOM §2.1): a model performs
  ;; the requests (dist d args), (diracN e) and (if test th el) by `shift`ing to the `reset` of
  ;; the handler that runs it.
  ;; ---------------------------------------------------------------------------------------

  ; deviation 9 (S:82, S:118): the handler of the model being run.  It is set inside the
  ; model's own dynamic extent only (handle-model parameterizes it inside the `reset`, so
  ; leaving the model at a `shift` restores the outer value and resuming it sets it again):
  ; handlers, user code in pure context and MH steps all see #f.
  (define current-handler (make-parameter #f))

  ; deviation 9 (S:118, P l.739-747): SExp is not a monad, so the graph is static: user code
  ; (distribution constructors, lifted functions) runs in pure context, where no effect can be
  ; performed.
  (define (pure-call thunk)
    (if (current-handler)
        (parameterize ((current-handler #f)) (thunk))
        (thunk)))

  ; cf. M:856-869 (`mcmc` takes a model): a model is a thunk (S:82), checked on behalf of who
  ; by every entry point that runs one (build-chain, and the oracles of (aux hakaru10 exact)).
  (define (check-model model who)
    (unless (procedure? model)
      (hakaru10-error 'construction who "a model (a thunk) is expected" model)))

  ; (new) what a model's `shift` hands to its handler's `reset`: the request (op arg ...) and
  ; the model's continuation up to that `reset`.  Private: no model can answer one.
  (define-record effect request k)

  ; (new) perform the request (op arg ...): capture the model's continuation up to its
  ; handler's `reset` and hand both to it; the value the handler resumes with is the answer.
  ; Outside a model, or in pure context, it is an error, as `dist` there is a type error
  ; upstream (S:82, S:118; deviation 9).
  (define (perform . request)
    (unless (current-handler)
      (hakaru10-error 'construction (car request)
                      "called outside model construction or inside a pure function"))
    (shift k (make-effect request k)))

  ; (new, no upstream counterpart: Haskell's continuations are pure) whether the native
  ; shift/reset of this CHICKEN returns a call/cc escape inside a resumed segment to that
  ; resumption.  `guard`, `handle-exceptions` and `condition-case` return (normally or on an
  ; error) through call/cc, so a model that sets one up around a `dist` relies on it under a
  ; multi-shot handler.  CHICKEN main up to rev 55615603 sends such an escape to the `reset`
  ; the capture was made under (of the first run, or of an earlier resumption) instead,
  ; dropping the rest of the enumeration: exact-posterior would answer a normalized but wrong
  ; distribution, without error, were it not for handle-model's check.  Fixed by f11f304b,
  ; b29737e8 and 0d398032 of chicken-core's branch delimcc-multishot.  A self-test run once, at
  ; load: two resumptions of one `shift` under a `condition-case` set up before it, the second
  ; raising; the answer is (1 caught) where the escape returns to its resumption.  One-shot
  ; handlers (the trace builder, the forward runs) are unaffected, their `resume` being called
  ; in tail position.
  (define native-multishot-escapes?
    (equal? (reset (condition-case (let1 (x (shift k (list (k 1) (k 2))))
                                     (if (eqv? x 2) (error "native-multishot-escapes?") x))
                     (e (exn) 'caught)))
            '(1 caught)))

  ; (helper of handle-model) the error of a misrouted escape, located at `who`, where
  ; native-multishot-escapes? is false.
  (define (multishot-unsupported who)
    (hakaru10-error 'construction who
                    "multi-shot resumption (exact enumeration) needs a CHICKEN whose call/cc escapes return to the resumption (chicken-core f11f304b, b29737e8, 0d398032)"
                    (chicken-version #t)))

  ; (new) run the thunk `model` under `handler`, (λ (request resume) ...): every request the
  ; model performs reaches the handler with `resume`, the model's continuation, which answers
  ; what handle-model answers; otherwise the answer is (return v) of the model's value v,
  ; computed inside the `reset` but outside the model's dynamic extent.  A deep handler by
  ; trampoline: `resume` runs the model up to its next request and hands that to the handler
  ; again, so a one-shot handler runs in constant stack and a multi-shot one may resume many
  ; times or lazily.  Hansei's `probcc-reify` (aux.hansei.scm) has this shape, over aux's
  ; resetcc instead of the native reset.
  ; The model starts with lazy-lifting #f: a run started from inside an inactive branch (a
  ; lifted function calling exact-posterior or mcmC) does not inherit its lazy lifting; a
  ; branch the model builds as inactive sets it again inside the model (build-branch!).
  ; Where native-multishot-escapes? is false (see there), a misrouted escape returns to the
  ; `reset` of the first run or to the `(k v)` of an earlier resumption, a second time.  Once
  ; handle-model has answered (a lazy handler such as exact enumeration's, whose resumptions
  ; run afterwards), that is a `construction` error rather than a silently wrong answer,
  ; raised where the resumption whose segment escaped was called (each such `resume` records
  ; its continuation; the latest one is the resumption running, since a segment runs as soon
  ; as it is resumed and no resumption starts inside a segment): so a tree of
  ; model->search-tree raises it from the probcc-explore that forces it, under that caller's
  ; handlers, not from the stale build call it was misrouted to.  Before handle-model answers
  ; it is let through: under a one-shot handler, which calls `resume` in tail position (the
  ; trace builder, the forward runs), it delivers what the resumption would have; an eager
  ; multi-shot handler is not protected.
  (define (handle-model model handler #!optional (return identity))
    (define (run-model)
      (if (lazy-lifting)
          (parameterize ((current-handler handler) (lazy-lifting #f)) (model))
          (parameterize ((current-handler handler)) (model))))
    (if native-multishot-escapes?
        (let run ((r (reset (return (run-model)))))
          (if (effect? r)
              (handler (effect-request r) (let1 (k (effect-k r)) (λ (v) (run (k v)))))
              r))
        (let ((answered #f) (resumption #f))
          ; (helper) continue as above with r, the value of the thunk `delimited`, a `reset` or
          ; a resumption's (k v); its second value, once handle-model has answered, is a
          ; misrouted escape, whose error is delivered to `resumption`, the continuation of the
          ; latest call of a `resume` made after handle-model answered, as a thunk it calls.
          ; The resumptions before it (all of a one-shot handler's) stay tail calls.
          (define (run delimited)
            (let* ((returned #f)
                   (r (delimited)))
              (when (and returned answered)
                (let1 (misrouted (τ (multishot-unsupported 'handle-model)))
                  (if resumption (resumption misrouted) (misrouted))))
              (set! returned #t)
              (if (effect? r)
                  (handler (effect-request r)
                           (let1 (k (effect-k r))
                             (λ (v)
                               (if answered
                                   ((call-with-current-continuation
                                      (λ (ret)
                                        (set! resumption ret)
                                        (let1 (r (run (τ (k v)))) (τ r)))))
                                   (run (τ (k v)))))))
                  r)))
          (begin1 (run (τ (reset (return (run-model))))) (set! answered #t)))))

  ; cf. S:141-151 `class StochasticPrim`: `dist d` for a distk d (S:144-145), `dist ctor x y`
  ; for a constructor (S:147-151).  deviation 9 (S:137-151): any arity, and plain values are
  ; Vals (replacing `fromInteger`/`pure`, S:120-132).  `ctor` may answer a resampleable or an
  ; observed distk (S:143-151, k polymorphic).  Performs (dist d args).
  (define (dist d . args)
    (cond
      ((distk? d)
       (unless (null? args)
         (hakaru10-error 'construction 'dist "a distribution takes no arguments" d args)))
      ((procedure? d) (for-each (λ (a) (check-not-model 'dist a)) args))
      (else (hakaru10-error 'construction 'dist
                            "a distribution or a distribution constructor is expected" d)))
    (perform 'dist d args))

  ; cf. S:161-169 `diracN`: performs (diracN e).
  (define (diracN e)
    (check-not-model 'diracN e)
    (perform 'diracN e))

  ; cf. S:275-279 `if_ test th el`: performs (if test th el), `th` and `el` being models, and
  ; calls the thunk the handler resumes with, in the model's own context and under the same
  ; `reset`: the computation that realizes the branch (PLAN-IDIOM C27).
  (define (if_* test th el)
    (unless (and (procedure? th) (procedure? el))
      (hakaru10-error 'construction 'if_ "the branches of if_ must be models (thunks)" th el))
    (check-not-model 'if_ test)
    ((perform 'if test th el)))

  ; cf. S:137-151 and S:218-219 (`with_node_ctor (Val x) df = ... df x`): the distk that the
  ; request (dist d args) denotes when every argument is a Val, as under a handler that
  ; evaluates SExps to plain values (the exact enumerator, a forward sampler); the constructor
  ; runs in pure context.
  (define (effect-distk d args)
    (if (distk? d)
        d
        (let1 (vs (map val-of args))
          (check-distk 'dist (pure-call (τ (apply d (forced-list vs))))))))

  ; cf. S:275-279 `if_`, for a handler that evaluates SExps to plain values: the branch thunk,
  ; th or el, that the test selects; resumed with it, the model runs the branch in its own
  ; context, so the branch's requests reach the same handler.
  ; deviation 9 (S:275 `if_ :: SExp Bool -> ...`): the test must be a boolean (plan C7).
  (define (select-branch test th el)
    (let1 (tv (force-value (val-of test)))
      (check-test 'if_ tv)
      (if tv th el)))

  ; (new) cf. S:137-151, S:161-169, S:275-279 read over plain values: the skeleton of every
  ; interpretation in which SExps are plain values.  (dist d args) calls (on-dist dk resume)
  ; with the request's distk; (diracN e) resumes with e, a Val; (if test th el) resumes with
  ; (on-if test th el), a thunk the model calls in its own context, by default the branch the
  ; test selects.  A procedure value stays boxed, as lift* keeps it (`constant`): `resume`
  ; boxes the value of a choice, and e is resumed as is, so that the model, which the trace
  ; builder answers a node there, may pass it to lift, dist or diracN (S:105-116 `Val f`); the
  ; model's result, lift and effect-distk unbox it (val-of).
  (define ((value-handler on-dist #!key (on-if select-branch)) request resume)
    (match/first request
      ((dist ,d ,args) (on-dist (effect-distk d args)
                                (λ (x) (resume (if (procedure? x) (make-constant x) x)))))
      ((diracN ,e) (resume e))
      ((if ,test ,th ,el) (resume (on-if test th el)))
      (else (hakaru10-error 'internal 'handle-model "unknown effect" request))))

  ; cf. S:174-193 `class Condition`, through the distribution module's `condition`;
  ; deviation 9 (S:38-42, S:180-183): "observation must be external to the model" is a run-time
  ; check that obs is not an sexp.  This is the public `condition`.
  (define (condition/external obs d) (condition obs d sexp?))

  ;; ---------------------------------------------------------------------------------------
  ;; The trace builder (M:399-526, S:137-279): the handler that builds the trace graph.
  ;; ---------------------------------------------------------------------------------------

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

  ; cf. M:404-419 `create_node` and M:233-243 `new_nref`: sample the distk (drawing only if it
  ; is resampleable, M:97-100), allocate the next address (Mem:59-63), add its LL to llcum,
  ; and record a resampleable node in `sampleable`, prepended (M:239).  `role` is root (no
  ; update), plain, entry or exit; `parents` a vector of nodes; `fn` the node's distk function
  ; (the constant distk for a root); `update` #f or the update procedure.  The node records
  ; the current construction scope.  Answers the node.
  (define (create-node! ch role parents fn dk update)
    (receive (v ll) (build-guard ch (τ (distk-sample dk (chain-rng ch))))
      (let1 (n (make-node ch (fx+ 1 (chain-count ch)) (distk-kind dk) role parents fn update
                          (chain-scope ch) '() #f
                          (make-nstate v ll (distk-rd dk) (chain-now ch)) 0 #f #f #f))
        (chain-store! ch n)
        (when (eq? (node-kind n) 'resampleable)
          (chain-sampleable-set! ch (cons n (chain-sampleable ch))))
        (chain-llcum-set! ch (fp+ (chain-llcum ch) ll))
        n)))

  ; cf. M:421-423 `with_node_none`: a root node, never updated.
  (define (create-root! ch dk) (create-node! ch 'root (vector) dk dk #f))

  ; deviation 10 (no upstream counterpart; Haskell's lexical scoping guarantees it): a node
  ; may depend only on nodes of its own branch or of an enclosing one, i.e. the parent's scope
  ; must be a tail of the child's.  Exit nodes depend on both branches by design (M:670-673).
  (define (check-branch-escape parent child)
    (unless (eq? (node-role child) 'exit)
      (let1 (ps (node-scope parent))
        (unless (let tail? ((cs (node-scope child)))
                  (or (eq? cs ps) (and (pair? cs) (tail? (cdr cs)))))
          (hakaru10-error 'branch 'if_ "node escapes its branch" parent child)))))

  ; cf. M:246-250 `new_dependency`: add child to the dependents of parent (IntSet union).
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
  ; distribution" (M:483-488).  Answers (values nstate' '()).  The node's fn forces the deferred
  ; values it reads, but that of an inactive diracN node (see lazy-value).
  (define (doupdate n)
    (let ((ch (node-chain n)) (self (node-state n)) (leaves (node-parents n)))
      (unless (extended? ch) (check-active-update n leaves))
      ; cf. S:240-243: the parents must have been updated.
      (unless (fx> (leaves-tstamp leaves) (nstate-tstamp self))
        (hakaru10-error 'dependency 'update "dependency violation!" n (leaves-tstamp leaves)))
      (let* ((dk (check-distk 'update (pure-call (τ ((node-fn n) (leaf-values leaves))))))
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
                (make-nstate v ((rdistr-log-density rd) v) rd (nstate-tstamp self))))
          '()))))

  ; cf. S:218-228 `with_node_ctor` (which dispatches to with_node, M:451-456, and with_node2,
  ; M:492-499): a Val gives a root node with no update (S:219); an NRef or an NT gives a plain
  ; node whose distk is `df` of the SExp's value, updated by doupdate and registered as a
  ; dependent of each leaf in order (S:255-261).  `df` maps a value to a distk.  The node's fn
  ; forces the deferred values it reads, unless `lazy`, which to-node! passes for a diracN
  ; node (df is then dirac): its value is lazy-value's, deferred while the node is inactive
  ; (cf. M:99, M:120-129).  The value of a Dirac node, or of a Val, must not contain an SExp
  ; (check-no-sexp, deviation 9).  `who` names the caller in errors.  Answers the node.
  (define (with-node-ctor! ch e df #!optional (who 'dist) lazy)
    (check-not-model who e)
    (check-owner ch e who)
    (if (sexp? e)
        (let* ((leaves (sexp-leaves e))
               (self #f)  ; the node, once created: lazy-value then asks it whether it is active
               (fn (cond
                     (lazy (let1 (g (if (node? e) (λ (vs) (vector-ref vs 0)) (nt-fn e)))
                             (λ (vs)
                               (df (lazy-value g vs (if self
                                                        (node-inactive? self)
                                                        (inactive-scope? ch)))))))
                     ((node? e) (λ (vs) (df (force-value (vector-ref vs 0)))))
                     (else (let1 (g (nt-fn e)) (λ (vs) (df (g (forced-values vs))))))))
               (dk (check-distk who
                                (build-guard ch (τ (pure-call (τ (fn (leaf-values leaves))))))))
               (n (begin
                    (when (eq? (distk-kind dk) 'dirac) (check-no-sexp who (distk-value dk)))
                    (create-node! ch 'plain leaves fn dk doupdate))))
          (set! self n)
          (vector-for-each (λ (p) (add-dependency! p n)) leaves)
          n)
        (let1 (v (val-of e))
          (check-no-sexp who v)
          (create-root! ch (check-distk who (build-guard ch (τ (pure-call (τ (df (force-value v)))))))))))

  ; cf. S:210-214 `to_node`, and S:161-169 `diracN`, which coincide: a Val becomes a new
  ; Dirac root (S:167); "if x is just a NoderRef, (diracN x) returns this reference (shares)
  ; rather than creating a copy of it!" (S:164-165, S:168); an NT becomes a Dirac node with an
  ; update (S:169), whose value is lazy (with-node-ctor!).  A Val that contains an SExp is an
  ; error (check-no-sexp, deviation 9).  Answers the node.
  (define (to-node! ch e #!optional (who 'to_node))
    (check-not-model who e)
    (check-owner ch e who)
    (cond
      ((node? e) e)
      ((nt? e) (with-node-ctor! ch e dirac who #t))
      (else (let1 (v (val-of e))
              (check-no-sexp who v)
              (create-root! ch (dirac v))))))

  ; cf. S:141-151: `dist d` is `with_node_none d` (S:144-145, M:421-423), `dist ctor x y` is
  ; `with_node_ctor (pair x y) (uncurry ctor)` (S:147-151), here `with_node_ctor` of the
  ; lifted application of ctor: a root node when every argument is a Val.  It always creates
  ; a new node.
  (define (build-dist! ch d args)
    (if (distk? d)
        (create-root! ch d)
        (with-node-ctor! ch (build-guard ch (τ (lift* d args))) (λ (dk) (check-distk 'dist dk)))))

  ; cf. S:275-279 `if_ test th el`: "Always make a new node, for the sake of ifnode": the entry
  ; is `with_node_ctor test diracS` (a root for a constant test), whose value must be a boolean
  ; (deviation 9); then `fmap NRef $ ifnode entry (th >>= to_node) (el >>= to_node)`.  It runs
  ; in the model's context (the trace builder resumes the model with it, C27), so the branch
  ; models run inline under the same `reset` and their requests reach the same builder.
  ; Answers the exit node.
  (define (build-if! ch test th el)
    (let1 (entry (with-node-ctor! ch test (λ (v) (check-test 'if_ v) (dirac v))))
      (node-role-set! entry 'entry)
      (ifnode! ch entry (τ (to-node! ch (th))) (τ (to-node! ch (el))))))

  ; cf. S:82 `type Model a = MCMCM (SExp a)` and S:196-197 `mcmC`'s `m >>= to_node`: the
  ; handler that interprets a model's requests by building ch's trace graph, the State monad's
  ; job upstream.  One-shot: every request resumes the model at once, in order, with the node
  ; it made, or for `if` with the thunk that builds the branches (C27), so the nodes are
  ; created and the choices drawn in upstream's order (C18).
  (define ((trace-builder ch) request resume)
    (resume (match/first request
              ((dist ,d ,args) (build-dist! ch d args))
              ((diracN ,e) (to-node! ch e 'diracN))
              ((if ,test ,th ,el) (τ (build-if! ch test th el)))
              (else (hakaru10-error 'internal 'mcmC "unknown effect" request)))))

  ; cf. S:196-197 `m >>= to_node`: run model under the trace builder of ch and turn its result
  ; into a node.  Answers the node.
  (define (run-model! ch model)
    (handle-model model (trace-builder ch) (λ (e) (to-node! ch e))))

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
  (define (check-test who v . irritants)
    (unless (boolean? v)
      (apply hakaru10-error 'branch who "the test of if_ must be a boolean" v irritants)))

  ; cf. M:696-751, the folds `Set.foldl' deactivate1` and `Set.foldl' activate1` (M:700,
  ; M:728) that deactivate and activate share: walk the nodes of (lo, hi] in ascending address
  ; order, moving each one's deactivation count by `step` (+1 or -1, journaled).  A node whose
  ; old count is `switching` (0 for deactivate: "unless the node was already inactive", M:688;
  ; 1 for activate: IntMap's `updateLookupWithKey` answers the count *before* the update,
  ; M:733-746) changes activity: it adds its weight to llc, `(observed-weight n)` if it is
  ; observed and 0 otherwise ("We look only at Conditioned nodes", M:690-695, M:707-711,
  ; M:736-740), and counts in k if it is resampleable (M:712-715, M:741-744).  Answers (values
  ; llc k).  A count driven below 0 is upstream's "cannot happen" (M:747-751): an internal error.
  ; A pending observed node ('extended, deviation 8) weighs 0.: adding 0. to llc changes no bit,
  ; llc starting at 0. and so never being -0.
  (define (move-activity! ch lo hi step switching observed-weight)
    (let loop ((a (fx+ lo 1)) (llc 0.) (k 0))
      (if (fx> a hi)
          (values llc k)
          (let* ((n (chain-node ch a)) (count (node-inactive n)) (kind (node-kind n)))
            (when (fx< (fx+ count step) 0)
              (hakaru10-error 'internal 'activate "activating an active node" n))
            (set-inactive! n (fx+ count step))
            (if (fx= count switching)
                (loop (fx+ a 1)
                      (fp+ llc (if (eq? kind 'observed) (observed-weight n) 0.))
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
                                           (nstate-ll (node-state n)))))
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
  (define (activate! ch lo hi)
    (receive (llc k) (move-activity! ch lo hi -1 1
                                     (if (extended? ch)
                                         (λ (n) (set-pending! n #t) 0.)
                                         (λ (n) (nstate-ll (node-state n)))))
      (chain-llcum-set! ch (fp+ (chain-llcum ch) llc))
      (chain-card-change-set! ch (fx+ (chain-card-change ch) k))))

  ; deviation 8 (M:735-737, plan §6.8): at the end of a step's propagation in 'extended mode, add
  ; to llcum the current LL of every observed node activated during the step and still active
  ; (in ascending address order), clearing their pending flags.
  (define (settle-pending! ch)
    (let1 (pending (chain-pending ch))
      (when (pair? pending)
        (for-each (λ (n)
                    (when (node-pending n)
                      (set-pending! n #f)
                      (chain-llcum-set! ch (fp+ (chain-llcum ch) (nstate-ll (node-state n))))))
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

  ; deviation 10 (no upstream counterpart; Haskell's lexical scoping): build a branch with
  ; `thunk`, in the construction scope extended with (entry . polarity), polarity #t for the then
  ; branch, so that every node it creates records it (node-scope, the branch-escape check and
  ; inactive-scope?).  Answers the thunk's answer, the branch's result node.
  ; The branch is built under lazy-lifting when it is inactive.
  (define (build-branch! ch entry polarity thunk)
    (let1 (scope (chain-scope ch))
      (chain-scope-set! ch (cons (cons entry polarity) scope))
      (begin1 (if (inactive-scope? ch) (parameterize ((lazy-lifting #t)) (thunk)) (thunk))
              (chain-scope-set! ch scope))))

  ; cf. M:641-686 `ifnode entry th el`: `th` and `el` are thunks that build a branch in the
  ; current chain and answer its result's node (TraceM a: `th >>= to_node`, S:279).  The
  ; entry must not be sampleable (M:643-647; "When a node is resampled, its update procedure is
  ; not run"); sampleable nodes are exactly the resampleable ones, so the kind is checked.  The
  ; inactive branch is built first (M:651-652) and deactivated at once (M:659, "which rolls
  ; back llcum"; its range may be empty, M:655-657), then the active one (M:661).  The exit
  ; node, a Dirac of the active result's value updated by update-exit, depends on the entry,
  ; the active result and the inactive result, in this order (M:665-673).  The entry's update,
  ; if any, is wrapped by update-entry (M:679-685; a constant test has none, and Haskell's lazy
  ; `Just prev_upd` is then never forced).  Entry and exit share one branch record.  Each
  ; branch is built in the scope extended with (entry . polarity), polarity #t for the then
  ; branch (deviation 10, the branch-escape check).  Answers the exit node.
  (define (ifnode! ch entry th el)
    (when (eq? (node-kind entry) 'resampleable)
      (hakaru10-error 'internal 'if_ "Entry node for if should not be sampleable" entry))
    (let1 (tv (node-value entry))
      (check-test 'if_ tv entry)
      (let* ((c0 (chain-count ch))
             (inactive-result (build-branch! ch entry (not tv) (if tv el th)))
             (c1 (chain-count ch)))
        (deactivate! ch c0 c1)
        (let* ((active-result (build-branch! ch entry tv (if tv th el)))
               (c2 (chain-count ch))
               ; M:665-666 and M:679-680: the results and the ranges, in then/else order.
               (b (if tv
                      (make-branch entry active-result inactive-result (cons c1 c2) (cons c0 c1))
                      (make-branch entry inactive-result active-result (cons c0 c1) (cons c1 c2))))
               (parents (list entry active-result inactive-result))
               (exit (create-node! ch 'exit (list->vector (delete-duplicates parents eq?)) #f
                                   (dirac (node-value active-result)) (update-exit b)))
               (prev (node-update entry)))
          (node-branch-set! exit b)
          (for-each (λ (p) (add-dependency! p exit)) parents)
          (when prev (node-update-set! entry (update-entry prev b)))
          (node-branch-set! entry b)
          exit))))

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
  ; (settle-pending!).
  (define (propagate! ch)
    (let ((q (chain-queue ch)) (extended (extended? ch)) (now (chain-now ch)))
      (let loop ()
        (if (update-queue-empty? q)
            (when extended (settle-pending! ch))
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

  ; cf. M:872-880 `initial_step'` and S:196-197 `mcmC limit m = mcmc limit (m >>= to_node)`:
  ; run the model (a thunk) once under the trace builder of a fresh chain over src, turn its
  ; result into a node, and freeze the graph.  Answers (values ch result).
  ; deviation 8 (M:650, M:885, plan §6.8, C22): in 'extended mode a build rejected by
  ; build-guard answers result #f with llcum -inf (initial-step then builds again), and the
  ; llcum of a complete build is recomputed as the extended LL: the incremental sum adds an
  ; inactive observed node's LL and subtracts it at deactivation (M:659), which is NaN, not 0,
  ; when that LL is -inf.
  (define (build-chain model src semantics)
    (check-model model 'mcmC)
    (let* ((ch (new-chain src semantics))
           (result (condition-case (run-model! ch model)
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
  (define (initial-step limit model src semantics)
    (let retry ((limit limit))
      (if (<= limit 0)
          (values #f #f 0)
          (receive (ch result) (build-chain model src semantics)
            (if (rejected-ll? (chain-llcum ch))
                (retry (- limit 1))
                (values ch result limit))))))

  ; cf. M:856-869 `mcmc` and M:897-906 `transition`, as a generator: the first value is the
  ; initial trace's, then each call takes one MH step and answers the result's value, a
  ; rejected step repeating the previous one.  `rng` (an rng source) overrides `seed`
  ; (mkStdGen 17, M:858) and is used as is.  If the initial step finds no trace in `limit`
  ; builds, the answer is #f (upstream's `[]`, M:868).
  ; deviation 10 (M:885-891, M:897-898, no upstream counterpart): a negative `limit` is a
  ; `construction` error.  Upstream stops only at limit 0 (`go 0`, `transition _ _ 0`), so it
  ; would retry the build forever or answer an infinite list.
  (define (mcmc-generator limit model #!key (seed 17) (rng #f) (semantics 'upstream)
                          (step-hook #f) (build-hook #f))
    (unless (and (real? limit) (>= limit 0))
      (hakaru10-error 'construction 'mcmC "limit: must be a non-negative number" limit))
    (receive (ch result limit*) (initial-step limit model (or rng (make-stdgen-source seed))
                                              semantics)
      (and ch
           (let1 (started #f)
             (chain-step-hook-set! ch step-hook)
             (when build-hook (build-hook ch))
             (τ (if started (transit! ch) (set! started #t))
                (force-value (node-value result)))))))

  ; cf. M:856-869 `mcmc`: fold kons over the chain's first `limit` samples, counted with the
  ; original limit (M:869), so limit - 1 steps run; knil if there is no initial trace (M:868).
  ; The keys are mcmc-generator's, which rejects a negative limit (deviation 10).
  (define (mcmc-fold kons knil limit model . keys)
    (let1 (next (apply mcmc-generator limit model keys))
      (if next
          (let loop ((i 0) (acc knil))
            (if (>= i limit) acc (loop (+ i 1) (kons (next) acc))))
          knil)))

  )
