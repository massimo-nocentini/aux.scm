# `(aux kanren lambda)`: λProlog's higher-order logic over `(aux kanren micro)`

This is the final proposal. It follows Miller & Nadathur, *Programming with Higher-Order Logic* (CUP 2012).
It brings three things into the miniKanren of aux: universal and hypothetical goals over hereditary Harrop
formulas, λ-tree syntax, and higher-order pattern unification (L_λ).

It starts from the design ranked first by the judges, **d2-hooks** (aggregate 23, against 21 for d3-native and
15.5 for d1-layered). None of the verdicts found a soundness defect in d2 that another design avoids, so d2 is
the base. The judges did find seven concrete d2 defects, and recommended about twenty grafts from d1 and d3.
Each one is either done or explicitly declined, in §10. A final adversarial review then sent thirteen reports on
this proposal itself; all were real and are fixed, each code fix with a regression case (§10.4). A second
adversarial review sent eight more; all were real (two of them are one defect), six are fixed with regression
cases, and the seventh, a cost, is measured and documented (§10.5). A third review sent twelve; all were real
(three pairs are one defect each), and all nine defects are fixed, each with a regression case (§10.6).

Everything quoted below was run. Outputs are pasted from `examples.out`, `test-results.txt` and
`bench/bench.out`.

| file | what |
|---|---|
| `aux.kanren.micro.scm`, `micro.diff` | micro with the extension protocol: +214/−41 lines against `src/aux.kanren.micro.scm`, comments included (256 changed lines with `diff -w`). The diff applies cleanly (`patch --dry-run`). |
| `aux.kanren.micro.show.scm`, `show.diff` | the show module, extended to read the new answer forms and to draw λ-terms in the cells view (+42/−12) |
| `aux.kanren.lambda.scm` | the λProlog layer, `(aux kanren lambda)`: 1255 lines, 885 of them neither blank nor comment |
| `test/microkanren-lambda.scm` | the acceptance suite A1–A9 and the three reviews' regressions R, R2 and R3: 83 cases, 320 assertions, every expected answer hand-written |
| `test/microkanren-lambda-limits.scm` | the limits of §11, each pinned by a case: 6 cases, 13 assertions |
| `test/programs.scm` | the book's programs used by the suite and by the examples |
| `micro-tests/`, `micro-tests-with-lambda/` | micro's six suites, unchanged, and the same suites with `(aux kanren lambda)` also imported |
| `examples.scm`, `examples.out` | the core book examples, each beside its λProlog original, and their actual output |
| `bench/` | `bench-first-order.scm` (the judges' benchmark), `bench-lambda.scm`, `bench-after-forall.scm`, `bench-A.scm` (§12), `run-bench.sh`, `bench.out`, `bench-r3.out`, `bench-r3-walk-D.out` |
| `build.sh`, `run-tests.sh`, `test-results.txt` | build into `repo/`, which shadows the prebuilt micro; run everything |
| `probe/` | the probes used during development, including the judges' adversarial probes ported to the final API (`adv*-d2.scm`); `probe/r3/` holds the third review's repros (`repro.scm`, `repro2.scm`) |

Run: `./build.sh && ./run-tests.sh`. The output, also in `test-results.txt`:

```
test/microkanren-lambda ((ran 83) (failed 0))
test/microkanren-lambda-limits ((ran 6) (failed 0))
micro-tests/microkanren ((ran 24) (failed 0))           micro-tests-with-lambda/microkanren ((ran 24) (failed 0))
micro-tests/microkanren-aggregation ((ran 6) (failed 0)) micro-tests-with-lambda/microkanren-aggregation ((ran 6) (failed 0))
micro-tests/microkanren-untagged ((ran 7) (failed 0))   micro-tests-with-lambda/microkanren-untagged ((ran 7) (failed 0))
micro-tests/microkanren-show ((ran 15) (failed 0))      micro-tests-with-lambda/microkanren-show ((ran 15) (failed 0))
micro-tests/microkanren-arith ((ran 52) (failed 0))     micro-tests-with-lambda/microkanren-arith ((ran 52) (failed 0))
micro-tests/microkanren-date ((ran 21) (failed 0))      micro-tests-with-lambda/microkanren-date ((ran 21) (failed 0))
```

That is 89 λ cases plus micro's 125 cases run twice, 339 cases in all, with 0 failures. The micro suites run
with unchanged expectations, answer order included. `microkanren-unionfind.scm` tests `(aux kanren unionfind)`,
a separate engine that does not import micro, so I did not run it. The user's repository was never written:
`git status` shows only the 19 untracked files it had at the start.

**The module's name.** I kept `(aux kanren lambda)`. The layer is λProlog's logic, and λKanren is the name the
literature gives this line of work, Ma & Friedman's paper included. A name such as `(aux kanren hohh)` would
describe only the goals, while the module also gives λ-tree syntax and L_λ unification. Its names follow aux:
goals end in `°` and their applicative forms in `ª`, term constructors have no suffix (`ƛ`, `@`), and
internals carry the `μkanren-` prefix.

---------------------------------------------------------------------------------------------------

## 1. The book, concept by concept

| book (page) | λProlog | here |
|---|---|---|
| λ-tree syntax, α-equivalence (§7.1, p.193) | `x\ t` | `(ƛ (x) t)`. It is locally nameless: binders are de Bruijn indices inside the body, so terms built alike are `equal?`. Unification works modulo α. |
| application, β (§4.1, §7.2) | `(F t)` | `(@ F t)`. It β-reduces at once when `F` is an abstraction, and on the fly in unification and in every deep walk. |
| η (§4.1) | built into λProlog's βη-normal forms | terms are built η-short, by `ƛ` and by the unifier; an η-redex that a later binding creates (`λx.F (G x)`, G := λz.z) is contracted by head normalisation and in deep values; η-expansion happens on demand when one side is an abstraction (§4) |
| GENERIC, `pi x\ G` (§3.3, p.98) | `pi x\ G` | `(∀° (x) G)`: a new eigenvariable one universe deeper; the universe is restored on every answer |
| AUGMENT, `D => G` (§3.2, p.93–94) | `D => G` | `(⊃° (D ...) G ...)`. Γ lives in the state, is extended for G and restored on every answer. |
| BACKCHAIN on assumed clauses | every predicate | `define-dynamic-relation`: hypotheses first, most recent first, then the static body, in one flat disjunction |
| clause with own variables | `pi X\ H :- B` | `(∀ (X) (H ← B))`, also written `(∀ (X) (← H B))`; facts are `(H)` |
| free variables of an assumed clause | shared with the context (p.102–104) | the closure's variables, shared; its own `∀` variables are renamed by a `fresh°` at each use |
| mixed prefix, universe indices (Nadathur, Jayaraman & Kwon 1995) | the WAM's universe register | a level per logic variable, kept in an sbral in `X`, checked at each binding by micro's `=°` (the `post` hook) |
| raising (§4.4.1, p.123) | | implicit in levels; lazy in variable elimination (8.K); explicit at `∀°` exit for residual constraints (discharge, §5) |
| L_λ pattern unification (§8.3, p.236–241) | Teyjus 2 | ξ, η, rigid–rigid, flex–rigid by variable elimination (pruning, lowering, raising on strict occurrences only), flex–flex same/different heads |
| dynamic L_λ (p.241–242) | Teyjus 2 delays | non-patterns are suspended in `E`, woken by bindings, reported as `(assert (≐ u v))`, closed over the eigenvariables that outlived their scope; a binding whose scope check a non-pattern decides is taken back into such an equation |
| `sigma X\ G`, `,`, `;` | | `fresh°`, `and°`, `or°` (interleaving) |
| — (λProlog has no ≠, absento, types) | | micro's `≠°`, `absent°`, `symbol°`, `number°`, made λ-aware through the protocol (§6) |

---------------------------------------------------------------------------------------------------

## 2. The edit to micro: an extension protocol

micro's state cannot gain a field from outside, and its unifier, walkers and reifier are closed (a01 §5.1).
d2 opened exactly those points with one generic record of hooks, so that the λ layer is a client module. I keep
that architecture and widen the protocol by five hooks. Each one fixes a d2 defect that the judges found. The
final review added one primitive, `μkanren-state-forget`, and a projection mode to `parts` (§10.4).

### 2.1 What changes in micro

* **State.** It gets a seventh field `X`, the extensions' slot. micro never reads it. It copies `X` at its two
  positional constructions (`μkanren-state-update`, `freshª`), and `μkanren-state-with` accepts `X:`.
  `make-μkanren-state` keeps its six-argument signature (`X` is optional), so `test/microkanren.scm`'s
  positional states still work.
* **Registry.** `(make-μkanren-extension name #!key term? atom? unify occurs post diseq tag parts value repr
  terms forms environment)` builds an extension, and `μkanren-extension-register!` registers it. Registering
  again under the same name replaces the old one.
* **Forgetting a binding.** `(μkanren-state-forget α s)` unbinds α. A `post` hook uses it to take a binding
  that `=°` just made back into a constraint of its own, and leaves that binding out of the assocs it answers.
  Since `post` runs before D, A and T are verified, those stores never see the binding.
* **absent° with a non-ground datum** (third review, generic). micro decided an atom against an absento's
  datum with `equal?` only, though a datum holding a variable, or an extension's term that reduces, may still
  become that atom: `(absent° D q) (=° q 'a) (=° D 'a)` succeeded in micro alone. An atom is now decided against
  such a datum by `μkanren-tag-unify`, as a compound already was, and a pending one is watched; an answer keeps
  it when the datum names a variable of the answer, and prints that datum as a term, `(absent? (@ α 'b) 'a)`.
  And when a type turns an absento into a prefix of D, a datum holding an extension's term stays in T instead
  (`μkanren-extension/holds-term?`), so D never holds one (§6.1).
* **Call sites.** All of them are off micro's hottest path, and each costs one `pair?` test when no extension is
  registered:

| hook | contract | called by |
|---|---|---|
| `term?` (u) | u is one of the extension's records | dispatch |
| `atom?` (u) | u holds no variable | the occurs check, which then does not look inside it (new; 18% on the λ workloads, §12) |
| `unify` (u v s assocs) → (s* as*) \| #f | a pair with one walked extension term, neither a variable | `μkanren-state-unify`: the structure–structure clause and the final `else`. The variable clauses and the pair–pair clause are untouched. |
| `occurs` (α v s assocs) → (s* as*) \| #f | micro's occurs check failed on α := v | `μkanren-state-update`, on the failure path only (new; `F = λx.G (F x)` holds with G := λz.z) |
| `post` (s* assocs) → (s** as**) \| #f | runs after the bindings of `=°`, before D/A/T are verified; may bind more | `μkanren-post-unify`, exported, called by `=°` |
| `diseq` (u v s) → #f \| state \| 'fail | may claim a disequality | `≠°`; micro's body is now `μkanren-≠` |
| `tag` (u tag s) → state \| #f | a type on its term | `symbol°`, `number°` & co. (new; a flexible term waits instead of failing) |
| `parts` (u s [projecting]) → 'stuck \| list | where `absent°` looks into its term; when `projecting` (s reified), the parts whose constraints an answer keeps | `absent°`, the re-verification of T (new; a flexible term is watched whole), and `μkanren-project-T` (final review: a λ-term is no longer split into its raw record slots) |
| `value`, `repr` (u walk s) | deep value / rebuilding expression | `μkanren-state-find/value`, `/repr` |
| `terms` (s) → terms | the terms of its residual constraints whose variables an answer names (the λ layer: its suspended equations) | `μkanren-project`, which reifies their variables after the answer's (new; residuals print α, β instead of `_2`) |
| `forms` (s s*) | the forms its constraints add to an answer | `μkanren-project` |
| `environment` () | where `°->list/ground` evaluates answers | `°->list` (new; ground answers are live λ-terms) |

Two invariants are inherited from micro. First, a hook answers the very state it was given when it changes
nothing, because `≠°` and the D store decide entailment with `eq?`. Second, a hook that binds returns the
extended `assocs`, so micro re-verifies the stores that the new bindings touch.

### 2.2 Why a protocol and not a native engine

The judges split: two ranked d2 first, and the pedagogy judge ranked d3 first. The choice rests on four facts.

* **Size and scope of the edit.** Here the edit is 255 diff lines, all of them generic: nothing in micro
  mentions λ, levels or clauses. d3 grows micro from 713 to 1217 lines and special-cases every walker. The same
  protocol could carry another store, say nominal terms or finite domains.
* **Soundness.** It is equal. d3's eager binding rule and this layer's eager `post` check enforce the same NJK
  conditions. Every scope attack by the judges fails correctly on both, and on this final version too
  (`probe/adv-d2.scm`).
* **Correctness.** It is better than either design. d3 had a crash (`μkanren-verify-D+` on a delayed prefix)
  and an η bug that made `≠` unsound. d2 had neither (§10, D1–D7). This proposal reached the same crash class
  through an incomplete D→N migration; the final review found it, and D now never holds a λ-term (§10.4).
* **Pedagogy.** d3 won this judge on three points: one vocabulary, readable answers in `ƛ`/`@`/`≐`, and
  answers usable as code. All three are now grafted (§7), so the pedagogical argument for the native design no
  longer holds.

---------------------------------------------------------------------------------------------------

## 3. Terms and state

| record | slots | meaning |
|---|---|---|
| `μkanren-lam` | `hint body` | an abstraction, locally nameless. `hint` is an *interned* symbol used only for printing, so `(equal? (ƛ (x) `(f ,x)) (ƛ (x) `(f ,x)))` is `#t` and λ-terms group in `groupby°` (A8/aggregation). |
| `μkanren-app` | `head args` | an application: flexible if the head walks to a variable, a β-redex if it walks to an abstraction, rigid otherwise |
| `μkanren-eigen` | `name level id` | GENERIC's scoped constant. `id` is an uninterned symbol, its identity. Level +∞ marks the names the unifier opens under binders. |
| `μkanren-bvar` | `index` | a de Bruijn index, only inside bodies; the first 64 are preallocated |

d2 hid levels and indices in the property lists of uninterned symbols, so that micro's record-rebuilding walks
and `absent°` could not see them. The judges called this a workaround. With the `parts`, `value` and `atom?`
hooks, micro no longer looks into the extension's records, so they are plain records with plain slots.

First-order structure stays Scheme data, as in micro. The object language is lists such as `(abs ,R)` and
`(app ,M ,N)`, with R a meta-level abstraction. `(@ 'g 'a)` (λ-tree application of a constant) and `'(g a)`
(a list) are distinct terms (limits/untyped). This is what keeps conjunction commutative: d1 conflated the two
and lost that property (the judges' t-order defect).

The extension's slot is `(define-record μkanren-hh depth top L Ln Γ E N)`, where `X = '()` reads as
everything empty:

* `depth` is the current universe level, raised by `∀°` for the extent of its body. `top` is the highest level
  reached so far (§4.3).
* `L`/`Ln` hold the level of each logic variable: an sbral indexed like S, filled lazily. A variable created
  since `L` was last extended has the current depth, so `L` is padded exactly when the depth changes, and
  whenever the unifier creates a variable at another level. Lookup is O(log n).
* `Γ` is an alist from a relation to its assumed clauses, most recent first. Assuming a clause for a relation
  already in Γ replaces its entry, so Γ has one entry per distinct assumed relation, and a lookup costs the
  number of those relations, not the number of clauses (d1's judges measured 18× slowdowns from a linear Γ).
* `E` holds the suspended constraints: `(= u v)`, `(tag tag u)`, `(absent d u)`.
* `N` holds the disequalities involving λ-terms or eigenvariables, as `(reading u . v)`. D never holds one.

---------------------------------------------------------------------------------------------------

## 4. Unification

### 4.1 Notation

ℓ(X) is the level of a variable, ℓ(c) that of an eigenvariable, with ℓ(c) = ∞ for the names the unifier opens.
A *pattern* for F is a list of arguments that hnf to distinct eigenvariables, each of level > ℓ(F). This is
Miller's L_λ condition, "quantified inside the scope of F", read off the levels (Nadathur & Linnell 2005, §4).
hnf(t) walks t, flattens nested applications and contracts head β-redexes, and an abstraction whose body has
become an η-redex through a binding (`λx.F (G x)` with G := λz.z is F). Under L_λ every contraction is a β₀
renaming (p.236). `@` on instantiated heads gives full β, which is dynamic L_λ (p.241).

### 4.2 The rules

micro unifies the first-order part: variables, pairs, vectors, foreign records and atoms. It hands any pair with
a λ-record to the `unify` hook, which applies, after hnf of both sides:

$$
\begin{array}{lll}
\text{Var} & X \doteq t & X := t \text{ by micro (occurs check); then } \mathbf{Check}(X := t)\\
\xi & \lambda.b \doteq \lambda.b' & b[c/0] \doteq b'[c/0],\ c \text{ new},\ \ell(c)=\infty;\ \mathbf{Check} \text{ the bindings made under it}\\
\eta & \lambda.b \doteq t,\ t \text{ not } \lambda & b[c/0] \doteq (t\ c) \quad\text{(untyped: only when one side is } \lambda\text{, Miller §9.3)}\\
\text{Rigid} & h\,\bar u \doteq h'\,\bar v,\ |\bar u|=|\bar v| & h \doteq h',\ \bar u \doteq \bar v;\ \text{any other clash fails}\\
\text{FF-same} & F\,\bar a \doteq F\,\bar b \text{ patterns} & \bar a=\bar b:\ \top;\ \text{else } F := \lambda\bar x.\,H\,\langle x_i \mid a_i=b_i\rangle,\ \ell(H)=\ell(F)\ \text{(p.238)}\\
\text{FR, FF-diff} & F\,\bar a \doteq t,\ \bar a \text{ pattern} & F := \lambda\bar x.\,\mathrm{Elim}(\ell(F),F,\bar a,t)\ \text{(η-short)};\ \bot \text{ or suspend as Elim says}\\
\text{FF, one pattern} & F\,\bar a \doteq G\,\bar b,\ \text{only } \bar b \text{ a pattern} & \text{FR on } G\\
\text{Delay} & \text{no side a pattern} & \text{push } \lambda L.u \doteq \lambda L.v \text{ on } E\ (L\text{: the names opened so far}),\ \text{unless } u =_\alpha v\\
\text{Occurs} & X \doteq t,\ X \in t \text{ inside a } \lambda\text{-term} & \top \text{ if } \mathrm{hnf}(t) = X;\ \text{else } \mathrm{Elim}(\ell(X),X,\langle\rangle,t) \quad\text{(the `occurs` hook)}
\end{array}
$$

**Variable elimination** `Elim(ℓ, F, ā, t)` (p.239, with levels in place of the prefix) walks t once at local
depth k and builds t′. It prunes and lowers only *strict* occurrences (Dowek, Hardin, Kirchner & Pfenning):
those not inside an argument of a flexible term that is no pattern.

$$
\begin{array}{ll}
c \text{ eigen} & c = a_i \Rightarrow \text{the bound variable of } a_i;\ \ell(c)\le\ell \Rightarrow c;\ \text{else } \bot_{\text{scope}} \text{ (8.J)}\\
\text{bvar } j<k & \text{itself}\\
Y \text{ unbound} & Y=F \Rightarrow \bot_{\text{occurs}} \text{ if } \bar a = \langle\rangle \text{ (8.G), else suspend};\ \ell(Y)\le\ell \Rightarrow Y;\ \text{else } Y := Y'\,\bar a_Y,\ \ell(Y')=\ell,\ \bar a_Y=\langle a_i \mid \ell(a_i)\le\ell(Y)\rangle \text{ (lower + delayed raising, 8.K)}\\
F\,\bar b \text{ in } t & \text{below a rigid constructor, } \bar b \text{ rigid (no variable, flexible term nor } \lambda\text{), and } |\bar b| = |\bar a| + k_{top}\\
 & \text{or the root of } t \text{ no projection of } F \text{ can make (a constant, an eigenvariable} \notin \bar a\text{, a pair)}\text{: } \bot_{\text{occurs}};\ \text{otherwise, and below binders only: suspend}\\
G\,\bar b \text{ pattern in } t & \mathit{keep}(b) \iff b \text{ local} \lor b\in\bar a \lor \ell(b)\le\ell;\ \mathit{extra}=\langle a_i \mid \ell(a_i)\le\ell(G)\rangle \text{ if } \ell(G)>\ell;\\
 & \text{if something is dropped or lowered: } G := \lambda\bar y.\,G'\,\bar y|_{keep}\,\mathit{extra},\ \ell(G')=\min(\ell,\ell(G)) \text{ (pruning, 8.H)}\\
G\,\bar b \text{ not a pattern} & \text{each } b \text{ walked on its own, binding nothing: kept as it is; } \textbf{pruned} \text{ if it holds, on a rigid}\\
 & \text{path, a forbidden eigenvariable (every instance keeps it); any other trouble in it (a variable to lower,}\\
 & \text{a flexible term to prune, an occurrence of } F\text{, a forbidden eigenvariable below a flexible head or below}\\
 & \text{the application of a variable bound inside } b\text{)} \Rightarrow \text{suspend the equation (} G \text{ may drop or apply } b\text{);}\\
 & \text{when some } b \text{ is, or may become, an abstraction (}\lambda\text{, variable, flexible term), neither pruning nor}\\
 & \text{lowering } G \text{ is strict: suspend instead}\\
\lambda.b & \lambda.\mathrm{Elim}(b) \text{ at depth } k+1\\
\text{rigid app, pair, vector, record} & \text{homomorphic}
\end{array}
$$

An occurrence F b̄ fails the occurs check only when its instance cannot be smaller than that of F ā, which
is Nipkow's size argument (8.G, 8.I): b̄ are rigid terms, which no substitution makes reduce, as many as ā
plus the k_top binders on top of t (`F ā = λȳ.t′` is `F ā ȳ = t′` by η). `F = λx.g (F x)` and Example 8.I fail.
The number of arguments does not matter when the root of t′ is a constant, an eigenvariable outside ā or a
pair (third review): no instance λx̄.M of F can then be a projection, so M has that root, and the instance of
F b̄, with rigid b̄, is at least as large as M while it sits strictly inside one of M's arguments. So the
η-short occurrence `F = λxy. g (F y)`, which is `λz. F y z` η-long, fails as Miller and Nipkow fail it (p.239),
where it left a residual. A projection root keeps the count: `∀x∀y. F x y = x (F y)` holds with F := λw.w.
Otherwise the instance may shrink, and the equation is outside L_λ, so it is suspended as Teyjus does:
`∀x. F x = x (F (λz.z))` holds with F := λa. a c, and `∀x∀y. F x y = x (F y)` with F := λw.w
(R2/occurs-non-pattern-arguments). The proposal failed both when the equation came before F's binding.

**Check** is the eager scope check of a binding X := t that micro made: `Elim(ℓ(X), #f, ⟨⟩, t)`. It fails on a
captured eigenvariable, and lowers or prunes the variables of t. That is NJK's binding rule (2)+(3), "t contains
no constant with tag > i; change to i the tags of the variables in t whose tag is > i", extended to flexible
terms by pruning. If only a later binding can decide a non-pattern in t, the binding is taken back
(`μkanren-state-forget`) into the suspended equation X ≐ t, as Teyjus delays it. So a binding never holds an
eigenvariable it may not hold, and at ∀ exit the equation is closed like any other:

```
(°->list #f (fresh° (q G H) (∀° (y) (=° q (@ G (@ H y))))))   =>  ((λ (α β γ) (assert (≐ (ƛ (y) α) (ƛ (y) (@ β (@ γ y))))) α))
;; then (=° G (ƛ (a) 'k)) gives (k), in every order of the goals
```

### 4.3 Fast paths

* **No ∀ yet.** While no `∀°` has run, `L` is empty and no check runs.
* **Plain terms.** `μkanren-plain?` is an allocation-free walk, like micro's occurs check. It accepts t when t
  holds no eigenvariable or variable of a level above ℓ(X) and no application with a variable or abstraction
  head. Only otherwise does Elim run (from d3's `μkanren-occurs/fast`).
* **Top-level variables.** A variable of level `top`, the highest level reached, can see every eigenvariable
  and variable that exists, so its bindings are not checked in `post`. The bindings made under ξ are checked
  there anyway, by `μkanren-scoped`.

### 4.4 Wake-up and termination

When `=°` binds something and E is not empty, every suspended item is retried, oldest first. New bindings are
checked, and rounds repeat while a round binds. A constraint still undecided is suspended again without binding,
so the loop ends; in particular a suspended X ≐ t with X unbound binds X only once its check passes. Every progress round binds a variable for good, and the variables it creates come from
lowering (levels strictly decrease) or pruning (arities strictly shrink).

On pattern problems this is Miller's and Nipkow's algorithm (Miller 1991, Thm 7.1 and Prop. 7.3: termination,
⊥ ⇒ no unifier, otherwise the most general one), with the prefix erased into levels (Nadathur & Linnell 2005,
Thm 2). The suite checks the book's mgus verbatim: 8.H gives `U = λx y. g (V' y)` and `V = λy w. V' y`, 8.K
gives `U = λx. f (V' x)`, and Ch. 4's `λxλy.G x = λxλy.H y` gives G, H := λw.T.

Pruning a rigidly forbidden argument of a non-pattern is sound pruning on strict occurrences, after Dowek,
Hardin, Kirchner & Pfenning, *when no argument is, or may become, an abstraction*: then the instance of G
b̄ makes no new redex, so it keeps every argument its body uses and every eigenvariable of that body. It makes
`∃q∃F∀x. q = F x x` solvable as `F := λab.F'`, `q := F'`, where d2 and d3 left the equation suspended. The
proposal pruned and lowered G also when an argument was an abstraction, and lost `∃q∃G∀x. q = G (f x) (λv.k)
∧ G = λab. b a` (the second argument drops the first) and `∃q∀x∃G. q = G (λw.k) ∧ G = λy. y x` (the argument
drops the x of G's body) in one goal order, which also made `≠°` accept these equations violated (third
review, R3/non-pattern-functional-arguments); such a check now waits. Nothing else inside such an argument is touched, since G may drop it: the
proposal first pruned and lowered there too, and lost `∃q∃G∀y∃Y. q = G Y ∧ Y = y ∧ G = λa.k` in one goal order
(R/non-strict-arguments). A variable bound inside the argument is no rigid head either: G may apply the
argument, and `q = G (λz. z y)` holds with G := λa. a (λw.k) (R/bound-head-argument). An occurrence of F itself
in an argument of G is *not* pruned: `F x = G (F x)` holds with G := λz.z for any F, so it waits
(A5/soft-occurs); nor is F at the head of its own body with only binders above it, `F a = F y` or
`F = λw. F a`, which any constant F solves (R/same-head-occurrence).

---------------------------------------------------------------------------------------------------

## 5. ∀ and ⊃: rules and justification

The book's goal-directed search for hohh consists of AND, OR, ∃, GENERIC, AUGMENT and BACKCHAIN (Ch.3 p.93,
p.98; Ch.5 p.140). Uniform proofs are complete for intuitionistic provability of hereditary Harrop formulas
(Miller, Nadathur, Pfenning & Scedrov 1991). Implementing these rules faithfully *is* the semantics.

* **GENERIC.** $\Sigma;P \vdash \forall x.G$ if $\Sigma\cup\{c\};P \vdash G[c/x]$ with $c \notin \Sigma$.
  `(∀° (x) g)` creates c at level depth+1, runs g at depth+1, and gives every answer state back its depth. The
  levels of the variables created inside are recorded first. Σ is not carried: a variable created at depth d
  stands for an ∃ after the first d universals of its branch (Miller's mixed prefix, linearised by creation),
  and Check forbids exactly the substitutions that violate that prefix. NJK Prop. 4.1 gives soundness and
  completeness of this tagging, and Check implements their binding rule. Restoring the universe on each answer
  matters: a variable created after the `∀°` cannot see c, which a creation timestamp alone would not ensure.
* **Discharge** (from d3). Leaving the scope of c at depth d, a residual equation or a generic disequality that
  mentions c stands for ∀c(u = v), respectively for its negation under GENERIC's reading of c as a new name.
  Every variable Y in it with ℓ(Y) ≥ ℓ(c) is raised, Y := Y′ c with ℓ(Y′) = d (Miller's raising, a bijection
  on solutions). The constraint is then closed into λc.u ≐ λc.v, which is equivalent by ξ. The raise bindings
  go through `μkanren-post-unify`, which wakes E, so the closed constraint is retried; with no raise, the
  closed equations are retried anyway (`F x = g F` closes into `F = λx. g F`, an occurs failure). A raise
  brings c into more constraints: an absento of T on a raised Y now watches `Y′ c`, and a prefix of D on Y
  moves to N holding c. They are discharged in turn, until no raise is left, so the absento ends as
  `(absent? 'k β)` (R2/raise-closes-constraints; the proposal printed `(@ β (eigen x))`). For n7,
  `∃F∀x∃G. F x = G x`, this gives the fully general `F := λx. α x x`:

  ```
  (°->list #f (fresh° (q) (∀° (x) (fresh° (G) (=° (@ q x) (@ G x))))))  =>  ((λ (α) (ƛ (x) (@ α x x))))
  ```
  An absento of micro whose term mentions c is closed the same way, since absence in λc.u is absence in u. An
  absento whose datum is c, on a term without c whose variables all have levels below ℓ(c), holds for good
  (no instance can ever hold c, like ∀x. q ≠ x) and is dropped, also when a later lowering makes it so. The
  constraints that cannot be closed by an abstraction (a type on `F x`, an absento whose datum is c on a term
  that may still hold c) keep c and print under `∀`: `(∀ (x) (assert (absent? x (@ α x))))`.
* **AUGMENT.** $\Sigma;P \vdash D \supset G$ if $\Sigma;P\cup\{D\} \vdash G$. Γ lives in the state, so `or°`
  branches cannot interfere, and it is restored on every answer of the body (the stack discipline of p.94 and
  p.99).
* **BACKCHAIN.** A dynamic relation disjoins its hypotheses, most recent first, then its static body, in one
  flat `or°`. A clause is `(relation . (λ (args) goal))`. Its free logic variables are those of the closure,
  shared with the context and among all its uses (p.102–104, NJK p.14). Its own `∀` variables are made by a
  `fresh°` inside, so they are renamed at each use, at the current level. That is NJK's "new tagged instance".
  With `∀z(q z ⊃ p (d z))`, the goal `∃x∀y(q y ⊃ p x)` fails as it must (A1/levels-reach-clauses).
* **Static relations are closed.** `⊃°` on a relation not made by `define-dynamic-relation` raises
  `not a dynamic relation`. All three prototypes silently ignored such a clause. Dynamic relations are tagged
  with `extend-procedure`, so the check is O(1) and needs no global registry.

---------------------------------------------------------------------------------------------------

## 6. micro's stores on λ-terms

λProlog has no disequality, absento or types. The readings adopted here extend micro's own semantics over
λProlog's term model.

### 6.1 `≠°` (the `diseq` hook, store N)

* **Claim and trial.** The layer claims a disequality when either side, walked, holds a λ-record or an
  eigenvariable. The trial unifies u = v with Check. No unifier means the disequality holds for good. A unifier
  that binds no variable older than the trial and suspends nothing means it is entailed, and `≠°` fails.
  Otherwise the original pair is kept. The variables that the trial creates (pruning, flex–flex H, lowering) are
  **existential inside the negation**: ¬∃H. F = λx.H ∧ G = λy.H. A D-prefix would wrongly store them as free.
  ```
  (≠° (ƛ (x y) (@ f x)) (ƛ (x y) (@ g y)))  =>  (λ (α β) (begin (deny (∃ (_3) (and (equal? α (ƛ (x) _3)) (equal? β (ƛ (y) _3)))))) ...)
  ```
* **Migration.** A D-prefix born first-order that comes to hold a λ-term or an eigenvariable, at any depth below
  its variables, moves to N, as the tuple disequality it stands for, before micro re-unifies it. So D never
  holds one, and micro's plain trial never decides a λ-equation, with no scope check and no reading. The check
  runs only when a new binding may hold a λ-term, which fixes d2's +45–50% cost on ≠°-heavy first-order
  code (§12): the new bindings are looked at for 64 nodes at most, and past them D and A are walked instead,
  since first-order search binds long ground data at every step (third review: one unrelated `symbol°` or
  `≠°` made append° backwards 1.65× slower, walking each ground tail). A type of micro that would turn an
  absento whose datum holds an eigenvariable into a prefix of D keeps it in T (§2.1), where ∀ exit discharges
  it like any absento: `∀x. absent° x q ∧ nonnum° q` crashed the reifier, and depended on goal order under the
  extensional reading (R3/absento-of-eigenvariable-under-type). A migrated prefix takes the reading in force when it migrates, since D records none (§11.5).
  Under the extensional reading, once levels exist, a first-order disequality is not left to D at all but
  kept in N with its reading: in D an absento of T would absorb it (micro's subsumption), and absence reads
  an eigenvariable as GENERIC does, so `Y ≠ a ∧ absent a Y ∧ Y = x` held in two orders of six
  (R2/extensional-absent-order). The
  proposal moved only a prefix whose own side was just bound, so `q ≠ (Y)` then `Y = x` stayed in D: it
  printed `(eigen x)`, broke the extensional reading, and crashed micro's D verification on a delayed trial
  (R/D-migration-deep, R/D-migration-crash, R/D-empty-prefix, R/D-eigen-leak).
* **Re-verification.** After every binding `=°`, each N entry is tried again.
* **Subsumption by A.** A trial that binds a typed variable of A to a rigid λ-term or an eigenvariable has no
  solution, since a type holds of neither, so `(symbol° q) (≠° q (ƛ (x) x))` keeps no disequality, as micro
  drops `(≠° q 5)` under `(symbol° q)`.
* **Two readings of ∀.** `μkanren-λ-reading` is a parameter. A disequality keeps the reading in force when it
  is posted.
  * `'generic` is the default, GENERIC's new name. `∀x. q ≠ x` holds, as λProlog's `pi x\ not (q = x)`
    succeeds; ∇-like, as αKanren's noms. At ∀ exit the constraint is closed: ∀x. F x ≠ x becomes F ≠ λx.x.
    This is consistent whichever comes first, the binding or the ≠ (the judges' t8/t9).
  * `'extensional` reads an eigenvariable as any term of its universe (Moiseenko; Jin et al.). The trial
    replaces each eigenvariable by a new variable of its level, after raising the variables that may depend on
    it. It counts as entailed when it binds older variables only to distinct such variables. So `∀x. q ≠ x`
    fails, `∀x∃y. y ≠ x ∧ y = a` fails, `∀x∃y. y ≠ x ∧ y = f x` succeeds, and ∀x. F x ≠ x prints
    `(∀ (x) (begin (deny (≐ (@ α x) x))))` and is violated by F := λz.c (A8/diseq-extensional). This is d1's
    reading, implemented consistently: binding before or after gives the same answer.
* **Printing.** An N entry prints as its trial mgu on the older variables, with `∃` over the trial's variables.
  When the trial only suspends, it prints as `(begin (deny (≐ u v)))`. That fixes d2's `(deny (and))`.
* **Projection.** micro drops a disequality over a variable out of the answer, which can always be chosen to
  keep the sides apart. That fails for a variable in an argument of a flexible term: `∃Y. G ≠ F Y` is
  `F ≠ λz.G`. Such an entry is kept, and its variable prints as a working one, read existentially as in
  limits/absent-projection: `(begin (deny (equal? β (@ α _3))))` (third review; the proposal printed the answer
  unconstrained, though it rejected F := λz.k, G := k, R3/diseq-unreachable-argument). A variable out of the
  answer at the head of a flexible term, or outside every argument, still drops the entry.

### 6.2 `absent°` (the `parts` hook)

The datum is looked for in the parts of a λ-term, never in binder names or indices. A flexible term is
`'stuck` and watched whole, like a variable, and decided once its head is known. Below a binder, the parts that
mention it stay closed (`λx.(a . b)` is looked into as `λx.a` and `λx.b`), so a watched term never has a loose
index. This is d1's precision, which fixes d2's and d3's over-strict rejection:

```
(absent° 'c (@ F 'c)) (=° F (ƛ (z) 'b))   =>  ((ƛ (z) b))      ; F drops its argument
(absent° 'c (@ F 'c)) (=° F (ƛ (z) z))    =>  ()
(absent° 'c (ƛ (x) `(,(@ F x) c)))        =>  ()                ; found at once outside the flexible part
```

In an answer, micro's projection of T asks the same hook, in its projection mode, instead of splitting a
λ-term into its raw record slots (which gave too strong constraints and loose `(bvar 0)`). A flexible term
whose head is out of the answer drops its constraint, since that head may be a constant function:
`(fresh° (F) (absent° 'c (@ F q)))` answers `((λ (α) α))`. One whose head is in the answer is kept whole
(R/project-absent, limits/absent-projection). The suspended types and absentos of E are projected the same
way: their terms name no variable of the answer, and one whose head is out of the answer is dropped, since
`λz̄.k` for a k of the type and none of the data satisfies it; unless two different types wait on that head,
which no constant satisfies, and then both are kept (R2/type-projection).

The datum may be a λ-term itself. Against a λ-term it is unified, as micro unifies a compound datum; against
an atom it is now unified too (§2.1), modulo βη, since `(@ G 'b)` is `a` once G := λz.a, and `λz. G z z` is
`a` by η once G := λuv. a u. The proposal compared it with `equal?` and accepted these violated absentos, in
both orders (R3/absento-datum-reduces).

### 6.3 `symbol°`, `number°` and the user's types (the `tag` hook)

A type on a flexible term waits in E as `(tag tag u)`. The post hook does the same for a variable of A that has
just been bound to one. A rigid λ-term or an eigenvariable is decided by the type's predicate, as micro decides
every other record: eigenvariables and abstractions are neither symbols nor numbers, while a type of the
user's made by `define-μkanren-tag`, say `(not (number? v))`, holds of them (third review: the proposal failed
every type on them, R3/user-types). micro's types are disjoint, so two different types on the same flexible
term fail at once, as on a variable of A; the proposal suspended both, and `(=° q (@ F 'a)) (symbol° q)
(number° q)` succeeded in two goal orders of three (R3/type-conflict-on-flexible-term). On different terms
with one head, `F q` and `F 'b`, they wait, since F may tell its arguments apart.

```
(=° q (@ F 'a)) (symbol° q) (=° F (ƛ (z) z))  =>  (a)        ; d2 and d3 failed this eagerly
```

---------------------------------------------------------------------------------------------------

## 7. Reification

* **Values.** Deep values are β-normal. The `value` hook is used by answers, D/A/T, `project°` and aggregation.
* **Abstractions** print as `(ƛ (x y) body)`, nested binders merged. A binder is named after its hint, renamed
  apart from the binders around it, from every symbol of its body (d2's printer, which pedagogy judged the
  only correct one) and from the operators of the rebuilding code (`cons`, `quote`, `vector`,
  `make-record-instance`, `ƛ`, `@`). So `λa.λa.a` prints `(ƛ (a a1) a1)`, `λx.(x 'x)` prints
  `(ƛ (x1) (x1 x))`, and `λcons.(a cons)` prints `(ƛ (cons1) (a cons1))`, which evaluates back to the same term
  (R/ground-hints).
* **Applications** print as `(@ h a ...)`, so a flexible application `(@ α x)` cannot be mistaken for the list
  `(α x)`.
* **Residuals** print as `(assert (≐ (ƛ (x) u) (ƛ (x) v)))`. The variables in residuals are named α, β after
  those of the answer, through the `terms` hook. Constraints that keep an eigenvariable print as
  `(∀ (x) form)`. No eigenvariable ever appears free in an answer. The one value that could hold one is the
  answer itself, when the variable that `°->list` reifies (variable 0) is created inside a `∀°`:
  `(°->list #f (∀° (x) (fresh° (q) (=° q x))))`. λProlog reports no binding for a variable quantified inside
  `pi x\`, and this one cannot be told without x, so reifying it raises the error `the answer holds the
  eigenvariable of a ∀° it was created in` (R2/answer-inside-forall). An answer of such a variable that does
  not hold x, `(=° q 'a)`, is reported as usual.
* **Ground answers are live terms** (from d3). `ƛ`, `@` and `≐` are bound in the λ module's environment, which
  the `environment` hook gives to `°->list/ground`, so answers can be fed back in:
  `(@ (car (°->list/ground ...)) 'b)` gives `(g b b)`. The record printer shows a term as its datum.
* **`°->list/datum`** (from d1) answers plain data, comparable with `equal?` against quoted book answers. This
  is what the suite uses.
* **The show module** (`show.diff`) reads `ƛ`, `@`, `≐`, `∃` and `∀`. Its mermaid `cells` view draws a
  `ƛ x` node with a node per binder and an `@` node, not the cons cells of the datum, where a bound `x` was
  hash-consed with the constant `x` (third review, R3/show-cells-binders). A new `°->pretty` gives the
  αKanren-style view (from d1):

```
((α : (= (ƛ (x) (@ α x x)) (ƛ (x) x))))                        ; 8.M, suspended
(((α β) : (∀ (_3) (≠ (α β) ((ƛ (x) _3) (ƛ (y) _3))))))
((α : (absento c (@ α c))))
((α : (∀ (x) (symbol? (@ α x)))))
```

---------------------------------------------------------------------------------------------------

## 8. API

```
(aux kanren lambda)
  terms    ƛ @ ≐ μkanren-abstract μkanren-apply μkanren-hnorm μkanren-α=? μkanren-datum
           μkanren-lam? μkanren-app? μkanren-eigen? μkanren-bvar? μkanren-λ-term? μkanren-lam-hint μkanren-eigen-name μkanren-eigen-level
  goals    ∀° ∀ª ⊃° ⊃ª define-dynamic-relation μkanren-dynamic μkanren-dynamic?
           (+ μkanren-hyps° μkanren-clause μkanren-clause/vars μkanren-clause*, expanded to by the macros)
  answers  °->list/datum μkanren-repr->datum
  engine   μkanren-λ-reading μkanren-λ-depth μkanren-λ-level μkanren-λ-delayed μkanren-λ-disequalities μkanren-λ-extension
  clauses  (rel t ...)  |  ((rel t ...) ← g ...)  |  (← (rel t ...) g ...)  |  (∀ (v ...) D)
(aux kanren micro show) adds  μkanren-answer->pretty °->pretty
```

Type inference (book §7.4.3) in the module's syntax:

```scheme
(define-dynamic-relation (typeof° m t)
  (cond°
    ((fresh° (M N B) (=° m `(app ,M ,N)) (typeof° M `(arr ,B ,t)) (typeof° N B)))
    ((fresh° (R A B) (=° m `(abs ,R)) (=° t `(arr ,A ,B))
       (∀° (x) (⊃° ((typeof° x A)) (typeof° (@ R x) B)))))))
```

Outputs, from `examples.out`:

```
;; typeof S, I and λx.x x
(((arr (arr α (arr β γ)) (arr (arr α β) (arr α γ)))) ((arr α α)) ())
;; clause order under augmentation (p.94)
((3 2 1) (2 3 1))
;; Example 8.H, pruning: (U V)
(((ƛ (x y) (g (@ α y))) (ƛ (y w) (@ α y))))
;; functional difference lists: palindromes (p.158), the book's first nine answers in its order
((solved) ((ƛ (x) x) (ƛ (x) (b a . x)) (ƛ (x) (a b a . x)) (ƛ (x) (α a b a . x)) (ƛ (x) (α α a b a . x)) ...))
```

`examples.scm` holds the complete curated script, every query beside its λProlog original and page.

---------------------------------------------------------------------------------------------------

## 9. Acceptance status

All 83 cases are in `test/microkanren-lambda.scm`, with exact, hand-written expected answers, raw or as data.
Six limits are in `test/microkanren-lambda-limits.scm`.

| id | status | cases (selected answers) |
|---|---|---|
| A1 | pass | ∃Y∀x.Y=x and Y=(f x) `()`; ∀x∃Y.Y=x `((λ () 'ok))`; ∀x∀y.x=y `()`; alias q=(y), y=x in both orders, two steps, var–var both ways, inside a vector `()`; alias below a binder `()`; micro's `cons°` under ∀ `()`; unrelated answer `((λ (α) (cons 'f (cons α '()))))`; NJK's ∃x∀y(q y ⊃ p x) `()`; hypothesis escapes in both directions `()` |
| A2 | pass | scope in/out `(a)`/`()`; order `(3 2 1)`, `(2 3 1)` (= p.94); branches `(a)`, `(a b)`; shared `(7)`, shared twice `()`, renamed `(ok)`, both rule syntaxes `((1 1))`; static relation → error; **courses (Fig. 3.1)**: `(210)`, `((dana 301))`, Kim as a set, `(solved)`; jars `(j)`; reverse (Fig. 3.4) both ways `((3 2 1))` |
| A3 | pass | λx.x `(arr α α)`; λxλy.x `(arr α (arr β α))`; λfλx.f(f x) `(arr (arr α α) (arr α α))`; S as on p.207; λx.x x `()` (occurs check); backwards: 4 (term, type) pairs, and the terms of type i→i |
| A4 | pass | cbn (λxλw.w) Ω `((abs (ƛ (w) w)))` (p.199), K I (λj.j j) `I`; `subst R N (R N)` with R bound before and after; β at construction; copy (p.215) `((abs (ƛ (x) (abs (ƛ (x1) (app x1 x))))))`; subst; Fig. 7.11 forwards; **p.219's four existential generalisations in the book's order** |
| A5 | pass | F x = g x x `(ƛ (x) (g x x))`; λx.F x = λx.c `(ƛ (x) c)`; F x y = G y x `(((ƛ (x y) (@ α y x)) α))`; F x y = F y x `(ƛ (x y) α)`; λxλy.G x = λxλy.H y; pruning F x = G y `(((ƛ (x) α) (ƛ (y) α)))`; **8.H**; ∃F∀x.F=x `()` vs ∃F∀x.F x=x `(ƛ (x) x)`; 8.J, 8.I, 8.G, F=λx.g(F x) `()`; **8.K**; non-pattern F (g x) = h x residual closed over x, woken to success and to failure; 8.M residual and woken; 8.A residual after raising; non-pattern pruning; n7 fully general; soft occurs F x = G (F x) suspended, then woken; η (`λx.q x = q`, F x = G x then F = G); trivial residuals dropped |
| A6 | pass (deviations documented) | λa.a=λb.b `((λ (α) α))` = paper; `equal?`, `≐`; λa.X=λb.b `()` (paper: d0); conjunction `()` (paper: d0); their Ex. 3 gives X = Y; true=false `()`; λxλy.x y = λaλa.a a `()`; shadowing printed `(ƛ (a a1) a1)`; assume on a dynamic `eq°` gives `(orange apple)` (paper: `(apple orange)`); `all A. A = F A` gives `((ƛ (a) a))` = paper |
| A7 | pass | de Bruijn §7.4.4 both directions and the p.209 trace (exact book terms); prenex (Fig. 7.8): the unique form of p.212 and **the five forms of p.213**; mappred (Figs 5.3–5.4) `((23 23 24))`, `((bob sue) (ned sue))`, also with x\y\ age y x; **palindromes (Fig. 5.11): the book's first nine answers in its order**; **Fischer CPS (Fig. 10.6): exactly the book's term of p.273** |
| A8 | pass | ≠ modulo αβη; ≠ with a flexible term woken; flex–flex ≠ with ∃; migration D→N, including a tuple; ≠ with eigenvariables (vanishes / kept / dropped after lowering); scope-exit closing consistent in both orders (t8/t9); the extensional reading (10 assertions); `absent°` on λ, names, indices, eigenvariables; precise `absent°` on flexible terms and closed at ∀ exit; `symbol°`/`number°` waiting on flexible terms; readable binders, renamed apart; live ground answers; λ-terms grouped by `groupby°` |
| A9 | pass | micro's six suites pass against the edited micro, with and without the extension loaded (125 cases each, answer order unchanged); a first-order case inside the suite; costs in §12 |
| R | pass | the final review's thirteen reports: the twelve code fixes pinned by 12 cases, 44 assertions (§10.4) |
| R2 | pass | the second review's eight reports: the six code fixes pinned by 6 cases, 31 assertions (§10.5) |
| R3 | pass | the third review's twelve reports: the nine defects pinned by 9 cases, 53 assertions (§10.6) |

**Differences from Ma & Friedman 2021 (A6)**, each decided by λProlog's semantics:

* `λa.X = λb.b` and the conjunction `… ∧ λc.X = λd.d` fail. The paper answers d0, by grafting. Under the
  quantifier reading ∃X∀a. X = a is false (p.101, Example 8.J).
* `λa.λa.X = λc.λd.c` fails, with no false "true = false". Shadowing is lexical: `ƛ` uses Scheme binders, and
  printing renames apart.
* Their Example 3, `λa.λb.X = λc.λd.Y`, gives X = Y. Neither may depend on the binders, so no "closure
  equation" is needed.
* `(assume (== apple orange) (== apple X))` cannot be written in λProlog, where `=` is not a predicate one can
  assume clauses for. With a dynamic `eq°` it gives `(orange apple)`: the assumption comes first, p.94. The
  paper gives `(apple orange)`.
* Their E12, `all A. A = (F A)`, gives F = λA.A, as in the paper.

---------------------------------------------------------------------------------------------------

## 10. Alternatives considered, defects found, and what was grafted

### 10.1 The three designs

| design | idea | why not the base |
|---|---|---|
| **d1-layered** | no edit to micro; the context in a reserved variable 0; a second unifier `=λ°` and second stores `≠λ°`/`absentλ°`; its own runner `λrun` | micro's `=°` and every relation built on it (`cons°`, user relations) leak eigenvariables under ∀ (`(∀° (x) (cons° x '() q))` gives `((x₂))`). Goals do not run under `°->list`. Conjunction is not commutative (spine notation plus η). It duplicates micro. `=λ°` costs 4.4× `=°` on search. Ranked last by all three judges. |
| **d3-native** | λ-terms native in micro: levels on the variable record, the unifier and the HH goals inside the core | A crash in `μkanren-verify-D+` on a D-prefix whose trial delays. η-incompleteness that made `≠` unsound (`≠ λx.F x λx.G x` then G := F accepted). The suite was a generated snapshot. A 931-line diff of micro, with trailing-whitespace churn. Its strengths are grafted here (§10.3). |
| **d2-hooks** | a small generic protocol in micro plus a client module | **base**: no soundness defect, the η-short invariant (λx.(q x) = q and F x = G x ⇒ F = G both hold), the soft occurs check, a hand-written suite. Its seven defects are fixed (§10.2). |

### 10.2 d2's defects, as found by the judges, and their fixes

| # | defect | fix | test |
|---|---|---|---|
| D1 | a suspended N trial printed as `(deny (and))` | the denied equation, `(deny (≐ u v))` | A8/diseq-flex |
| D2 | `absent°` over-strict on flexible terms | `parts` hook: watch stuck terms whole; closed parts below binders | A8/absent-flexible |
| D3 | trivially true residuals kept (`F x x = F x x`) | Delay drops α-equal sides | A5/trivial-residuals |
| D4 | n7 left suspended, printed with `_2` | discharge at ∀ exit (raise, close, retry); `terms` hook names residual variables | A5/non-pattern-pruning |
| D5 | registering the extension slowed ≠°-heavy first-order code by 45–50% | migrate only when a new binding holds a λ-term; `has-term?` allocation- and find-free on atoms | §12, perm6 |
| D6 | `lam°`/`app°` carried the goal suffix; `λk-` prefix; plist-hidden levels; λ-terms not `equal?` | `ƛ`, `@`, `μkanren-`; plain records thanks to `parts`/`value`/`atom?`; interned hints | A6/alpha, A8/aggregation |
| D7 | `symbol°`/`number°` failed eagerly on flexible terms; `⊃°` on static relations silently ignored | `tag` hook plus A-migration; an error for non-dynamic relations | A8/types, A2/static-relations |

One more defect turned up while grafting: `F = λx.G (F x)`, reached through η-short construction, failed
micro's occurs check before any hook could see it. The `occurs` hook now passes it to variable elimination,
which suspends it (A5/soft-occurs).

### 10.3 Grafts

| from | graft | status |
|---|---|---|
| d3 | discharge residual equations and λ-disequalities at ∀ exit by raising and closing | done (§5) |
| d3 | `≐` exported, residuals as `(assert (≐ …))`, binders merged `(ƛ (x y) …)`, a datum renderer | done (§7) |
| d3 | first-order fast path of the scope check | done: `μkanren-plain?`, `top`, and the `atom?` hook for micro's occurs check |
| d3 | var–var bindings oriented by level | **declined**. It needs a hook in micro's var–var clause, the hottest path after pair–pair. Check's lowering of the younger variable gives the same mgu up to renaming. |
| d3 | levels on the variable record | **declined**. micro's `freshª` would need the current universe, which lives in the extension's data, so micro would become λ-aware. The lazy sbral costs O(log n) per lookup, and nothing on the first-order path. |
| d3 | the 2–4× faster λ machinery | **partly done.** Instantiation shifts only open terms, decided once, which removes d2's quadratic shifting; plus the fast paths above. Church 400 typeof went from d2's 441 ms to 254 ms. d3's is 105 ms; the profile shows the rest in micro's walks (§12). |
| d3 | ground answers as live terms (pedagogy) | done: `environment` hook |
| d1 | precise `absent°` on flexible terms | done (§6.2) |
| d1 | the extensional reading of ≠ under ∀, as an option, implemented consistently | done: `μkanren-λ-reading` (§6.1) |
| d1 | pending constraints mentioning eigenvariables printed `(∀ (x) …)` | done, for the constraints that cannot be closed by ƛ |
| d1 | a data runner comparable with `equal?` | done: `°->list/datum` |
| d1 | an αKanren-style pretty view | done: `°->pretty` in the show module |
| d1 | a leak/limits table, each row pinned | done: §11 and `test/microkanren-lambda-limits.scm` |
| d1 | truly nameless λ-records, `equal?` modulo α | **partly done.** Hints are interned, so terms built alike are `equal?` and group. α-variants with *different* names are not `equal?`: a hint that `equal?` ignores cannot be stored in the record, and I chose printing with the user's binder names over that. `μkanren-α=?` and `≐` decide α-equivalence. |
| d2 | infix clauses `(H ← B)` beside d3's `(← H B)` | both accepted |
| d2 | the name `define-dynamic-relation` | kept |
| all | the show module reads `ƛ`, `@`, `≐` | done (`show.diff`) |
| all | `⊃°` on a non-dynamic relation raises | done |
| d1/d2 | hand-written cases with book page references, not a generated snapshot | done |

### 10.4 Defects found by the final review, and their fixes

Every report was reproduced first. All thirteen were real.

| report | defect | fix | test |
|---|---|---|---|
| D-migration-deep, r1-2-D-eigen-leak | a D-prefix whose *inner* variable is bound to an eigenvariable stayed in D: `(eigen x)` printed free, and the extensional reading depended on goal order | migration moves every D-prefix that holds a λ-term or an eigenvariable at any depth (§6.1) | R/D-migration-deep, R/D-eigen-leak |
| D-migration-crash, r1-2-D-empty-prefix | the same with a flexible term: micro's trial delayed and left an empty prefix `()` in D, which crashed `μkanren-project` or accepted a violated state | the same: D never holds a λ-term | R/D-migration-crash, R/D-empty-prefix |
| absent-entailed-residual | `∀x. absent° x q` printed a ∀ residual, though no instance of q can hold x | the absento is dropped when levels entail it, at ∀ exit and on retry (§5) | R/absent-entailed |
| U1 | pruning and lowering inside an argument of a non-pattern, which G may drop: answers lost, conjunction not commutative | Elim binds nothing inside such an argument; a suspended check takes the binding back into X ≐ t (§4.2) | R/non-strict-arguments |
| U2 | `z y` with z bound inside the argument was taken as rigid, and y pruned | such a head is flexible inside the argument (§4.2) | R/bound-head-argument |
| U3 | F at the head of its own body with only binders above (`F a = F y`, `F = λw.F a`) failed as an occurs error, or suspended depending on the side | suspended; an occurs failure needs a rigid constructor above (§4.2) | R/same-head-occurrence |
| U4 | the occurs hook did not β-normalise: `q = F q` with F := λz.z failed | the hook succeeds when hnf(t) is the variable itself (§4.2, Occurs) | R/occurs-modulo-beta |
| r1-2-project-T-raw-slots | micro's projection of T split a λ-term into its record slots: too strong absentos and a loose `(bvar 0)` | `μkanren-project-T` uses the `parts` hook in projection mode (§6.2) | R/project-absent |
| r1-2-ground-hint-clash | a binder hinted `cons` captured the `cons` of the rebuilding code, so `°->list/ground` crashed | binders are renamed apart from the code's operators (§7). The `quote` and `vector` variants of the report fail while the user's own Scheme code builds the term, since `(ƛ (quote) 'a)` shadows `quote` itself; built through `μkanren-abstract` they print and evaluate correctly | R/ground-hints |
| r1-2-doc-print-example | §7 claimed `λz.(z 'x)` prints `(ƛ (x1) (x1 x))` | the example is `λx.(x 'x)` (§7) | A8/reify |
| r1-2-doc-gamma-cost | Γ kept one alist entry per assumed clause, not per relation as §3 said | the code now matches §3: assuming a clause replaces its relation's entry | R/hypotheses-per-relation |

### 10.5 Defects found by the second adversarial review, and their fixes

Every report was reproduced first. All eight were real; r2-0-raise-unclosed-eigen-print and
r2-2-discharge-T-eigen-leak are one defect.

| report | defect | fix | test |
|---|---|---|---|
| r2-0-raise-unclosed-eigen-print, r2-2-discharge-T-eigen-leak | the raise at ∀ exit brought c into an absento of T and into a D prefix that moved to N, after the constraints to close were listed: `(absent? 'k (@ β (eigen x)))`, `(deny (equal? δ (@ γ (eigen x))))` | discharge repeats until no raise is left, and retries closed equations (§5) | R2/raise-closes-constraints |
| r2-0-answer-var-inside-forall | variable 0 created inside `∀°` printed `(eigen x)`, and `°->list/ground` crashed on `eigen` | reifying an answer that holds an eigenvariable raises an error that says so (§7) | R2/answer-inside-forall |
| r2-1-eta-after-instantiation | an η-redex made by a later binding, `λx.F (G x)` with G := λz.z, was never contracted: F = λx.F (G x) left a residual, its ≠ was accepted, η-expanded pattern arguments were not seen | hnf and deep values contract it (§4.1) | R2/eta-after-binding |
| r2-1-rigid-occurs-nonpattern-args | any rigid occurrence of F failed the occurs check, though F applied to a non-rigid argument or to another number of arguments may shrink: solvable problems failed, in one goal order | Nipkow's size condition, with η for the binders on top (§4.2) | R2/occurs-non-pattern-arguments |
| r2-2-ext-diseq-absent-subsumption | under the extensional reading, micro's T absorbed a D disequality, then checked only with the generic reading: two goal orders of six succeeded | such a disequality stays in N (§6.1) | R2/extensional-absent-order |
| r2-2-E-residual-unreachable-head | a type on a flexible term whose head is out of the answer was kept and named that head; a type of A did not subsume a λ-disequality | E's types and absentos are projected as T's absentos are; trials respect A (§6.1, §6.2) | R2/type-projection |
| r2-2-perf-after-forall | after any `∀°` has run, first-order search pays the scope check: +66% and +127% on the judges' searches | measured and documented (§11.8, §12); not changed, since the cheap fix gives up the guarantee of §11.6 | `bench/bench-after-forall.scm` |

### 10.6 Defects found by the third adversarial review, and their fixes

Every report was reproduced first (`probe/r3/repro.scm`, `probe/r3/repro2.scm`). All twelve were real;
r3-0-nonpattern-abstraction-arg-elim, r3-1-prune-consumed-argument and r3-1-lower-head-of-nonpattern are one
defect, as are r3-0-derived-D-eigen and r3-2-T-to-D-under-type, and r3-0-user-tag-lambda and
r3-2-user-tag-on-lambda. Each R3 case fails on the proposal before these fixes.

| report | defect | fix | test |
|---|---|---|---|
| r3-0-nonpattern-abstraction-arg-elim, r3-1-prune-consumed-argument, r3-1-lower-head-of-nonpattern | Elim pruned an argument and lowered the head of a non-pattern G b̄ whose argument is, or may become, an abstraction, which G may apply to drop that argument or its own eigenvariables: answers lost in one goal order, violated ≠° accepted | such a check suspends; pruning and lowering stay when no argument may be an abstraction (§4.2) | R3/non-pattern-functional-arguments |
| r3-0-N-unreachable-dropped | an N entry over a variable out of the answer was dropped as in micro, also inside the argument of a flexible term, where `∃Y. G ≠ F Y` is `F ≠ λz.G` | only a strict occurrence drops it (§6.1) | R3/diseq-unreachable-argument |
| r3-0-derived-D-eigen, r3-2-T-to-D-under-type | micro turned the absento of an eigenvariable or λ-term under a type into a D prefix holding it: reification crashed, a spurious ∀ residual, the extensional reading in one order | such an absento stays in T (§2.1, §6.1) | R3/absento-of-eigenvariable-under-type |
| r3-0-user-tag-lambda, r3-2-user-tag-on-lambda | every type failed on λ-terms and eigenvariables, whatever its predicate | the predicate decides (§6.3) | R3/user-types |
| r3-1-occurs-eta-short | an η-short rigid occurrence `F = λxy. g (F y)` was suspended | the count of arguments is not needed below a root that no projection makes (§4.2) | R3/eta-short-occurs |
| r3-2-absent-datum-reduces-to-atom | an atom was compared with an absento's datum by `equal?`, though a λ datum may reduce to it | atoms are unified with a non-ground datum, in micro (§2.1, §6.2) | R3/absento-datum-reduces |
| r3-2-type-conflict-on-flex | two types on one flexible term were both suspended | they fail at once (§6.3) | R3/type-conflict-on-flexible-term |
| r3-2-perf-A-D-nonempty | with D or A non-empty, every new binding was walked deep through S: 1.65× on first-order search | a bounded look at the bindings, then D and A (§6.1, §12) | `bench/bench-A.scm`, R3/first-order-cost |
| r3-2-show-cells-binders | the cells view drew a λ-term as the cons cells of its datum, sharing a bound variable with a constant | ƛ and @ nodes, binders kept apart (§7) | R3/show-cells-binders |

One more micro defect turned up while fixing these, and is left as it is, since it is micro's own and does not
involve λ-terms: under a type of the user's that holds of pairs, micro's subsumption turns `absent° '(a) q`
into `q ≠ (a)`, so `(absent° '(a) q) (nonnum° q) (=° q '((a)))` succeeds. It assumes that typed values are atoms.

### 10.7 Other alternatives

* **Raising eagerly**, as in the book's §4.4.1. It is replaced by levels, which do the same work lazily (8.K
  shows the difference in the mgu's shape, not in its meaning).
* **Huet's pre-unification**, as in Teyjus 1, enumerating 8.A's four unifiers. It is not done (§11). E is the
  natural agenda for it.
* **Rejecting non-patterns**, as ELPI's default. That would lose `subst R N (R N)` with R unknown, and the
  palindrome program.
* **A per-state hook list instead of a global registry.** `°->list` starts from `μkanren-state-empty`, so every
  initial state would have to carry the hooks. The global registry costs one `pair?` test per call site when
  empty, and the λ extension now costs little when loaded but unused (§12).

---------------------------------------------------------------------------------------------------

## 11. Limitations and deviations from λProlog

Each row is pinned by a case in `test/microkanren-lambda-limits.scm` or in the main suite.

1. **Only dynamic relations take hypotheses**, while in λProlog every predicate is dynamic. Clauses are
   closures, not formulas: a program cannot build clauses as terms, and there is no quantification over
   predicates (`pi rev\ …`, p.104). Assuming a clause for a static relation is an error.
2. **Untyped.** η applies only when one side is an abstraction. λ-tree application `(@ g a)` is not the list
   `(g a)`. Full β through `@` on instantiated heads may diverge on untyped meta-terms. There is no closed-term
   existence check (limits/untyped).
3. **Non-patterns are suspended, never enumerated** (no Huet). An answer can be conditional on jointly
   unsatisfiable residuals, which is floundering reported but not detected (limits/no-huet).
4. **Answer order is micro's interleaving.** It agrees with the book on p.94 (both orders), p.219, the
   palindromes, the reverse and course examples, and the CPS term. It differs on prenex (1 3 2 4 5), and
   wherever depth-first search loops it finds answers that λProlog misses (typeof backwards).
5. **≠, absent and types are extensions.** The default reading of ∀ under ≠ is GENERIC's
   (`∀x. q ≠ x` holds); the extensional one is a parameter. `absent°` and the types always use the generic
   reading. A first-order disequality that migrates from D to N takes the reading in force when it migrates,
   not when it was posted, since D records no reading; the two differ only when `μkanren-λ-reading` changes
   between the two goals. A first-order disequality posted under the extensional reading once a `∀°` has run
   goes to N with its reading instead (§6.1).
6. **Host escapes.** An eigenvariable smuggled out by Scheme code (`project°` and `set!`) still cannot reach an
   older variable, but a variable of a later scope at the same depth may take it. Levels police logic
   variables, not Scheme values (limits/host-escape).
7. **Binder names are hints.** α-variants with different hints are not `equal?` (§10.3).
   An absento on a flexible term whose head is in the answer is kept whole, and an argument of it that is out
   of the answer prints as a working variable: `(absent? 'c (@ α _1))`. It cannot be weakened to `absent° c α`,
   which α := λz. z c with `_1` := λv.b would violate (limits/absent-projection). Likewise a disequality with
   such an argument, `(deny (equal? β (@ α _3)))`, and an absento whose datum may become an atom of the answer,
   `(absent? (@ α 'b) 'a)` (R3/diseq-unreachable-argument, R3/absento-datum-reduces).
8. **Costs.** Every binding re-tries E and N when they are not empty, with no indexing. The scope check walks
   bound terms below the top level, and it does so from the first `∀°` on, for the rest of the search, even
   once the `∀°` has exited and nothing of a higher level is reachable: first-order search after a `∀°` costs
   1.7× to 2.3× (§12). Nested binders scale quadratically (§12).
9. **The answer variable outside every `∀°`.** The variable that `°->list` reifies must be created outside
   every `∀°` whose eigenvariable its value may hold; otherwise reifying raises an error (§7, R2/answer-inside-forall).

---------------------------------------------------------------------------------------------------

## 12. Performance

All figures are compiled `csc -O2 -d1`, best of 3 per round, two rounds (`bench/bench.out`). "before" is the
prebuilt unedited micro, "after" the edited micro with no extension, "ext" the edited micro with
`(aux kanren lambda)` loaded. The benchmark is the judges' `bench-first-order.scm`.

| benchmark (ms) | before | after | ext |
|---|---|---|---|
| append° backwards, 2000 answers (all splits of a 1999-list), full | 6068 / 5923 | 6121 / 5959 | 6151 / 5929 |
| the same, search only | 672 / 662 | 690 / 669 | 691 / 668 |
| open append°, 1000 answers, search | 1311 / 1314 | 1356 / 1326 | 1327 / 1328 |
| perm6 with 15 pairwise ≠°, 720 answers | 176 / 178 | 184 / 178 | 192 / 199 |

* The protocol costs at most 1% end to end on append° (5923 → 5959 ms in round 2). Round-to-round noise of the
  unedited binary is 2.5%.
* With the extension loaded, perm6 costs +8–12%; d2 cost +45–50%. The remainder is the `post` hook's walk of
  the new bindings, a few μs per `=°` while D is non-empty.

After the final review's fixes, a new run gave the same figures within noise: perm6 "ext" 193 / 190 ms,
append° backwards full 5905 / 5941 ms, typeof Church 400 247 ms, copy of 150 nested binders 1684 ms. The
deeper D migration walks D only when a new binding holds a λ-term. That claim was too broad: the walk of
the new bindings cost first-order search 1.65× as soon as D or A held anything, fixed below.

**With D or A non-empty** (`bench/bench-A.scm`, third review): one unrelated `(symbol° t)` or `(≠° t 1)`
before append° backwards, 2000 answers, search only. The proposal walked every new binding through S while
D or A was non-empty; it now looks at 64 nodes of them at most, then walks D and A (§6.1). From
`bench/bench-r3.out`, best of 3, two rounds:

| benchmark (ms) | after | ext, proposal (the report) | ext, now |
|---|---|---|---|
| plain | 673 / 669 | 665 | 657 / 664 |
| + `symbol° t` | 674 / 675 | 1097 | 687 / 695 |
| + `≠° t 1` | 655 / 657 | 1172 | 670 / 675 |
| + `absent° t` | 661 / 664 | — | 656 / 666 |

perm6 "ext" stays at 193 / 194 ms (after: 181 / 179). Walking D and A at every `=°`, the first fix tried,
cost perm6 +60% (288 / 302 ms, `bench/bench-r3-walk-D.out`), since its 15 prefixes are walked each time; so the
bounded look at the bindings comes first. The λ workloads are unchanged: typeof Church 400 253 ms, copy 256 ms,
typeof of 150 nested binders 1284 ms, copy 1729 ms.

**After a `∀°`** (`bench/bench-after-forall.scm`, second review). `pre` is `(∀° (z) ✓°)`, which binds nothing
and exits at once, and `typeof°` is one type inference of λx.x run before the search:

| benchmark (ms) | no `∀°` before | `pre` before | `typeof°` before |
|---|---|---|---|
| append° backwards, 2000 answers, search | 663 | 1104 (+66%) | 1092 (+65%) |
| open append°, 1000 answers, search | 1296 | 2940 (+127%) | — |

A second run (`bench/bench.out`) gave 680 / 1125 / 1154 and 1354 / 3055 ms.

Once a `∀°` has run, levels exist, so every binding of a variable below `top` gets the scope check, whose
`μkanren-plain?` walks the bound term through S beside micro's occurs check: on append° backwards the two walk
the same ground tail of the list. The check cannot be skipped at depth 0 by reachability alone: for logic
variables, an exited scope can reach a depth-0 binding only through constraints, which are checked when they
wake, but an eigenvariable smuggled out by host code (`project°` and `set!`, §11.6) would then reach an older
variable unchecked, which `limits/host-escape` pins as caught. The fix is roadmap 4(a): one walk for the
occurs and the scope check.

The second review's fixes cost nothing measurable on the λ workloads: typeof Church 400 260 ms, copy Church
400 259 ms, typeof of 150 nested binders 1292 ms, copy 1732 ms; perm6 "ext" 200 ms.

λ workloads, "ext" only (`bench/bench-lambda.scm`). The other columns are the judges' measurements of the
prototypes on the same programs:

| benchmark (ms) | final | d2 | d3 | d1 |
|---|---|---|---|---|
| typeof of the Church numeral 400 | 254 | 441 | 105 | 385 |
| copy of the Church numeral 400 | 256 | 442 | 116 | 314 |
| typeof of 150 nested binders | 1306 | — | 1409 | — |
| copy of 150 nested binders | 1745 | — | 1779 | — |
| typeof backwards, 30 (term, type) pairs | 10 | 11 | — | — |
| one pattern equation F x₁…x₅₀ = g x₅₀…x₁ under 50 ∀ | 2 | 6 | — | — |

The history of typeof Church 400: 463 ms after porting d2 (with closed-term instantiation); 330 ms with the
allocation-free `plain?`; 311 ms with the `top` skip; 254 ms with the `atom?` hook, which stops micro's occurs
check from rebuilding every eigenvariable as a slot list. A profile of the remaining time shows it in
`μkanren-state-find`/`find/SL`. These are micro's walks along the var–var chains that the type variables form
(micro binds the older variable to the newer), plus the occurs check of each binding of a subterm. Both are on
micro's hot path. d3's lead here comes from its in-core binding, which does the occurs check and the level
check in one walk; §13, stage 4.

---------------------------------------------------------------------------------------------------

## 13. Staged roadmap: what to merge first

1. **The protocol** (`micro.diff`). It is self-contained, λ-agnostic, and passes micro's 125 cases with
   unchanged answers, at about 1% cost. Merge it with a short section in micro's comments, which already
   documents each hook.
2. **`(aux kanren lambda)` with its suite** (`test/microkanren-lambda.scm`, `-limits.scm`) and
   `test/programs.scm`, as `src/aux.kanren.lambda.scm` and `src/test/microkanren-lambda*.scm`. Add the module
   to the egg's build list after micro.
3. **The show extension** (`show.diff`). It is independent of 2, since it reads forms syntactically, but only
   useful with it.
4. **Later, measured separately.**
   (a) A combined occurs-and-level walk in `μkanren-state-update`, d3's `occurs/fast`, exposed through the
   protocol as a hook that returns a verdict instead of a boolean. This is the remaining 2.4× on Church 400,
   and most of the cost of first-order search after a `∀°` (§12).
   (b) Var–var orientation by level.
   (c) An index of E and N by variable, so that a binding retries only the constraints it touches.
   (d) Huet pre-unification as an optional search over E, for 8.A-style problems.
   (e) Predicate quantification (`pi rev\`), through fresh dynamic keys allocated at run time.
   (f) Simple types, with an `atom?`/`tag` interplay for type-directed η.
