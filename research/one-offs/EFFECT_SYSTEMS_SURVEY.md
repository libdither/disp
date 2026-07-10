# Effect System Designs: A Survey for Disp

A landscape review of algebraic effect systems in modern PL research and practice,
oriented toward a concrete design decision for `disp`: a dependently-typed language
built on a hash-consed tree-calculus substrate that already performs
signature-based dispatch on wait-forms. The current closed-handler kernel
(§5–§7 of `TYPE_THEORY_NEXT.typ`) is structurally a closed Plotkin-Pretnar
algebra; §15 sketches "open it up" to user-installable effects without
committing to a specific design. This document is the comparison §15 defers.

---

## 1. Survey

### 1.1 Koka (Daan Leijen, Microsoft Research)

Koka is the canonical row-polymorphic algebraic-effect language. Effect types
are *rows of effect labels* — finite multisets with scoped labels permitting
duplicates (`<exn,exn>` ≢ `<exn>`), so unification stays principled without
side constraints (Leijen, "Koka: Programming with Row-Polymorphic Effect
Types", arXiv:1406.2061). The original compilation strategy was a *selective
type-directed CPS transform* (Leijen, "Type Directed Compilation of Row-Typed
Algebraic Effects," POPL 2017, DOI 10.1145/3009837.3009872): pure regions
stay direct, effectful regions are CPS-translated. Koka has since moved to
*evidence-passing translation* (Xie, Brachthäuser, Hillerström, Schuster,
Leijen, "Effect Handlers, Evidently," ICFP 2020, DOI 10.1145/3408981), in
which each handler is reified as a *first-class evidence record* threaded
into operations via an implicit parameter. Two optimisations follow: (a)
*tail-resumptive* ops execute in place (no stack capture); (b) handler lookup
becomes a constant-offset index instead of a runtime search. Koka also has
*named handlers* (Xie, Cong, Li, Lorenzen, Leijen, "First-Class Names for
Effect Handlers," OOPSLA 2022, DOI 10.1145/3563289) for distinguishing
multiple instances of the same effect.

### 1.2 Eff (Bauer & Pretnar)

The original full algebraic-effect implementation, accompanying Plotkin &
Pretnar's "Handlers of Algebraic Effects" (LMCS 2013). Bauer & Pretnar,
"Programming with Algebraic Effects and Handlers" (J. Log. Algebr. Methods
Program. 84(1), 2015; arXiv:1203.1539) describes the language: handlers
are *lexically scoped*, ops use *deep dynamic search* up the handler stack,
*multi-shot continuations* are unrestricted, and the type system is a simple
ML-style annotation (effects appear as type-level sets but inference is
limited). Eff implementations use delimited continuations (the prototype was
denotational, later versions compile via free-monad-like reification).

### 1.3 Effekt (Brachthäuser et al.)

Effekt (Scala library, then a standalone language; Brachthäuser, Schuster,
Ostermann, "Effekt: Capability-passing style for type- and effect-safe,
extensible effect handlers in Scala," JFP 2020, DOI 10.1017/S0956796820000027;
and OOPSLA 2020 "Effects as Capabilities," DOI 10.1145/3428194) replaces
effect rows with *capabilities*: handler-installed values passed explicitly
through the program. Capabilities are *second-class* — they cannot escape
their scope, so the type system avoids effect variables entirely and the
"effect of `f`" is exactly the set of capabilities it requires from its
context. Runtime uses multi-prompt delimited continuations. *Multi-shot* is
syntactically restricted: resumptions are scoped, ensuring the continuation
cannot outlive the handler.

### 1.4 Frank (Lindley, McBride, McLaughlin)

Frank ("Do Be Do Be Do," POPL 2017, arXiv:1611.09259) eliminates the
distinction between functions and handlers. Every function is a *multi-handler*:
it may take multiple computations as inputs and simultaneously interpret
commands from each. Effect annotations on function types are *abilities*
(sets of commands), propagated *inward* through an ambient-ability
mechanism rather than accumulated outward through union types. No source-level
effect variables are needed — abilities are inferred bidirectionally.

### 1.5 OCaml 5 (Sivaramakrishnan et al., PLDI 2021)

Multicore OCaml retrofitted effect handlers onto an industrial runtime
(arXiv:2104.00250, DOI 10.1145/3453483.3454039). Key compromises: continuations
are *one-shot* (multi-shot would require copying fiber stacks); effects are
*untyped at the source level* (a typed effect system is "future work" still
incomplete in OCaml 5.x); runtime uses *fibers* — small heap-allocated stack
chunks with kernel-bypass userland switching. Performance overhead on
non-effectful code is ~1%. The design choice was explicitly to maximise
backward compatibility and runtime speed at the cost of static safety.

### 1.6 Lean 4 (Ullrich & de Moura)

Lean 4 has no general algebraic-effect system. Effects are encoded as monads
in the standard way; the primary effect monad is `EIO`, defined as
`EStateM ε IO.RealWorld` — a specialised exception+state combination over an
opaque `RealWorld` token. `EStateM` is a custom-tuned ExceptT/StateT fusion.
The metaprogramming framework uses additional monads (`MetaM`, `TacticM`,
`TermElabM`) stacked on `EIO`. The "effect tracking" is purely *monadic
typing* — the user sees nothing more than what Haskell offers, but with full
dependent types throughout.

### 1.7 F\* (Maillard, Ahman, Atkey, Martínez, Hriţcu, Rivas, Tanter)

F\* organises effects as a *lattice* with user-defined lifts and a primitive
mechanism for *Dijkstra monads* — monads indexed by a specification monad
(typically predicate transformers). "Dijkstra Monads for All" (ICFP 2019,
arXiv:1903.01237) shows that any monad morphism from a computational monad
to a specification monad yields a Dijkstra monad, giving F\* a flexible
verification target. The framework is parameterised by a dependent type
theory and supports algebraic operations on Dijkstra monads. Effects in F\*
are *type-system features* with elaborate static checking; runtime is
extraction-driven (effects vanish or become concrete monadic code in OCaml/F#).

### 1.8 Free monads, freer, fused-effects, polysemy (Haskell/Idris)

The pure-library lineage. Kiselyov & Ishii ("Freer Monads, More Extensible
Effects," Haskell 2015, DOI 10.1145/2804302.2804319) showed that *freer
monads* — free monads without a `Functor` constraint, where the continuation
is a type-aligned sequence — give a fast, generic effect interpreter.
`fused-effects` (Wu, Hinze, et al.) uses *handler fusion* to eliminate
interpretation overhead via Haskell typeclasses, at the cost of
hand-written `Algebra` instances. `polysemy` prioritises ergonomics with
GHC plugins to drive inference. None handle higher-order/scoped effects
cleanly without the additional machinery of "Effect Handlers in Scope"
(Wu, Schrijvers, Hinze, Haskell 2014). The fundamental tradeoff: free-monad
encodings *always* pay for continuation capture even when unused.

### 1.9 Algebraic effects meets dependent types

The serious attempts:

- **Brady's Idris `Effects` library** ("Programming and Reasoning with
  Algebraic Effects and Dependent Types," ICFP 2013) and resource-dependent
  extensions (LIPIcs ECOOP 2020). Effects are embedded as a library; the
  resource type carried by an effect can depend on prior operation results.
  Compilation is to a free-monad-style interpreter.
- **Ahman's PhD work** (Edinburgh, 2017) and "Handling Fibred Algebraic
  Effects" (POPL 2018, DOI 10.1145/3158095). Handlers as user-defined algebra
  types with definitional equality on effect equations, in a dependent
  setting.
- **Maillard et al.'s Dijkstra monads** are explicitly dependent-type-native.
- **Kammar & Pretnar, "No value restriction is needed for algebraic effects
  and handlers"** (JFP 2017, arXiv:1605.06938) — important sub-result:
  unrestricted Hindley-Milner polymorphism is sound with handlers, removing
  the classical obstacle that blocks ML's combination of polymorphism + ref
  cells.

The combined field is small but established. The chief obstacles documented
are: (1) handlers in computationally-relevant positions interact awkwardly
with type-dependence (you may need to evaluate an effectful computation to
know the type of its result, breaking phase separation); (2) coherence of
multi-shot continuations with proof-relevant equality is unresolved; (3)
effect-row subtyping interacts non-trivially with definitional equality.

---

## 2. Comparison matrix

| System | Effect type rep | Runtime dispatch | Algebraic? | Multi-shot? | TC/elab extension | Compilation | Static/dynamic |
|---|---|---|---|---|---|---|---|
| **Koka** | Row of scoped labels (duplicate-permitting multiset) | Evidence-passing (handler is a record passed implicitly) | Yes (scoped resumption variant) | Yes; restricted to scoped resumption | Row unification + scoped-label algorithm | Selective CPS → evidence translation to lambda | Erased at runtime; evidence visible |
| **Eff** | Type-level sets (annotation only; weak inference) | Dynamic handler-stack search | Yes (original P-P) | Yes, unrestricted | Type-and-effect ML inference | Free-monad / delimited continuations | Mostly runtime |
| **Effekt** | Capabilities (second-class values) | Capabilities passed explicitly; multi-prompt delim. cont. | Yes | Scoped only | Path-dependent types; no effect variables | Capability-passing → delim. cont. monad | Capabilities exist at runtime; types static |
| **Frank** | Abilities (command-sets on `[..]` types) | Ambient ability + multi-handler dispatch | Yes | Yes | Bidirectional inference; no effect variables | Free-monad-style | Mostly erased; abilities are static |
| **OCaml 5** | None (untyped at source) | Fiber stack switching | Yes | **No** — one-shot only | None (effects untyped) | Native fibers + delim. cont. primitives | Fully runtime |
| **Lean 4** | Monad type (e.g. `EIO`) | Monad bind (no handler abstraction) | N/A — monads, not handlers | N/A | Standard monad typing | Specialised `EStateM` machine code | Static via monad type |
| **F\*** | Effect lattice + Dijkstra monad | N/A — verified-then-extracted | Algebraic ops over Dijkstra monads | Bounded by spec monad | Heavy: WP calculus + SMT | Extract to OCaml/F# concrete monads | Mostly static; runtime is plain monadic code |
| **freer/polysemy** | Type-level effect list (`Eff '[State Int, Reader r] a`) | Free-monad reification + handler fold | Yes | Yes (free monad) | GHC type families / typeclass resolution | Free-monad interpreter (can fuse) | Erased modulo Free-monad spine |
| **fused-effects** | Type-class constrained `Algebra sig m` | Static dispatch via typeclass instances | Yes | Typically one-shot per carrier | Typeclass resolution; manual `Algebra` impls | Carrier inlining + fusion → near-direct code | Mostly erased |

---

## 3. Synthesis: which design fits disp's substrate?

Disp has five non-negotiable properties that pre-filter the design space:

1. **Hash-consed trees give O(1) tree identity.** Any dispatch that key-compares
   on a tree constant is essentially free. This is exactly Koka's
   *evidence-passing* assumption transposed into substrate primitives — a
   wait-form's `pair_fst` *is* the evidence pointer, and `tree_eq` is
   pointer-compare.
2. **No mutation, no native continuations.** Continuations must be *tree
   values*. A handler that resumes simply applies the continuation tree.
   This rules in *deep, reified continuations* (free-monad style, CPS) and
   rules out the OCaml-5-style fiber stack switching — there are no stacks
   to switch.
3. **Pure substrate; side effects only at host.** The substrate cannot
   "perform" an effect; only a host-registered interceptor can. This
   matches Eff/Frank/Koka's *interpreter* discipline, where the language
   surface produces a tree of operation invocations and the handler
   chooses how to react.
4. **Signature-based dispatch is the existing primitive.** The kernel
   already routes `wait kernel.op meta` to a privileged interpreter via
   `pair_fst` signature comparison. User-effect dispatch is *the same
   primitive* with a different registry — exactly §15's sketch.
5. **Dependent types throughout.** Whatever effect mechanism lands, types
   may depend on effectful computations in elaboration-time positions.
   This argues for an *erasable* mechanism: effects that vanish in the
   strip pass and do not interact with type-level definitional equality.

The candidates that fit cleanly are:

- **Koka (evidence-passing).** Each handler is a record (an evidence value);
  operations look up their handler via a stable pointer. Disp's wait-forms
  already are this — `wait op meta` *is* the evidence, with `pair_fst`
  serving as the handler key. Row polymorphism maps to disp's records
  with sorted hash-cons-stable field order (a record is "almost free
  from the record infrastructure already in the library" — §15).
- **Frank (ambient ability).** Multi-handlers as the unifying abstraction
  matches disp's no-special-handle-form aesthetic. But Frank's
  bidirectional ability inference is non-trivial elaborator work; row
  polymorphism via duplicate labels (Koka) is mechanically simpler given
  disp's record infrastructure.
- **Free monads (freer-style).** A computation is a tree node tagged with
  an operation signature, holding a continuation tree. This *is* exactly
  what disp's wait-form encoding already produces. The "free monad over
  Σ" is literally the set of well-formed wait-form trees rooted in user
  ops. The fused/polysemy machinery for fast dispatch maps to disp's
  native fast-path on `param_apply`.

The candidates that fit poorly:

- **OCaml 5 fibers** — no stacks to switch; the model is structurally
  unavailable.
- **Effekt capabilities** — second-class is a real type-system addition
  on top of an already complex kernel; disp's value model is unstratified.
- **F\* Dijkstra monads** — a separate dimension of complexity; only
  worth it if disp commits to a WP-style verification methodology, which
  goes well beyond the §15 roadmap.
- **Eff dynamic-search** — wastes the O(1) hash-cons advantage.

---

## 4. Top recommendations

### Recommendation 1 (primary): Koka-style evidence-passing over wait-forms

The closest fit. The wait-form `wait op meta` already *is* the evidence
record for `op`; `pair_fst` is the dispatch key. Layer this on as follows:

```
op_sig            = pair_fst (wait user_op _)     // already hash-cons-stable
effect_sigs       = host-side Set of op_sigs registered as effects
handler_stack     = host-side Map<op_sig, handler-tree-stack>

param_apply f x:
  s = pair_fst f
  if s ∈ kernel_sigs    → run raw (existing path)
  if s ∈ effect_sigs    → look up top handler for s, apply to (meta, x)
  else                  → walker step
```

For *scoped* handlers, `with_handler sig handler body k` pushes `handler`
onto the `s`-keyed stack, evaluates `body`, pops, passes the result to `k`
(§15 already sketches this).

Static row checking maps directly to disp's existing record machinery:
an effect row is a *Record* whose fields are operation signatures with `Unit`
values, sorted by hash-cons id. Row union = record merge; row subset =
field-projection check. The `Pi` type-former generalises to
`PiE row dom cod` whose recognizer (a) accepts a function body and (b)
walks the body checking that every wait-form signature appearing in it is
contained in `row`. Body-walking machinery already exists in the parametric
walker.

**Why this wins:**

- Zero new substrate concepts. The dispatcher already does signature
  routing.
- O(1) handler lookup via hash-cons identity (better than Koka's static
  index, which itself was a tuning improvement over dynamic search).
- Static-row checking is *almost free* given disp's record library.
- Compiles to nothing on the host side — the host just maintains the
  registry; everything else is tree application.
- Multi-shot continuations come for free because continuations are tree
  values; the host handler can apply `k` any number of times.

**Sketch of integration cost:**

- ~50 lines to extend `param_apply` with the `effect_sigs` arm.
- ~100 lines of host-side `registerEffectHandler` + scoped handler stack
  management.
- ~200 lines of library for `PiE`, row-record manipulation, recognizer.
- ~100 lines of elaborator support for `<row>` syntax.

Total roughly the §15 estimate (~350-500 lines), but distributed across
the four phases above and stageable.

### Recommendation 2 (alternative): Frank-style ambient abilities

If the design priority is *minimum surface syntax*, Frank's "every function
is a handler" model is more elegant. The user never writes effect variables;
the elaborator infers abilities bidirectionally. Implementation cost is
higher (bidirectional elaboration is more invasive than row solving) but
the surface is cleaner — no `<row>` annotations, no row-polymorphism
machinery.

This is worth taking seriously if disp's surface syntax aims at being
notation-light. The runtime mechanism is identical to Recommendation 1
(signature-dispatch on wait-forms); only the static checking differs.

### Recommendation 3 (no-static-checking baseline): §15 as written

Land the *operational* §15 design (host-registered handlers, no row
checking) as-is, defer row checking entirely, gather usage data. This is
already the recommended approach in §15. The risk: users may write code
that depends on dynamic effect-handler installation in ways that make
later static checking impossible (cf. OCaml 5 — untyped effects have
proven hard to retrofit a type system onto). Recommendation 1's elaborator
support should land at the same time as the operational support, even if
the row checker is initially a no-op.

---

## 5. Open problems at the algebraic-effects-meets-dependent-types boundary

1. **Effectful functions in type positions.** If `f : Nat -> Type !{io}`
   makes a console call mid-elaboration, is `f 3` a well-defined type?
   Most systems forbid effectful types (F\*'s pure/ghost/computational
   stratification; Lean's `Prop` vs `Type` discipline). Disp's elaborator
   is currently *pure* — but as soon as user effects exist, the question
   of which positions admit which effects becomes load-bearing. The
   conservative answer: only `Pure` effects appear in elaboration-time
   evaluation; user effects are tracked but never fired.
2. **Definitional equality across effects.** If two effectful computations
   are `tree_eq` (same hash-consed tree) but their handlers are different,
   are they "the same"? Disp's H-rule and signature-based `is_*`
   recognizers may need refinement so that `pair_fst` of a user-effect
   wait-form is *not* mistaken for a type-former signature.
3. **Coherence of multi-shot continuations with proof-relevant equality.**
   Ahman (POPL 2018) treats this with handlers as algebra types; the
   equations enforce coherence. In a `tree_eq`-as-definitional-equality
   setting, multi-shot is incoherent unless the language tracks
   effect-purity in equality reasoning. This is the deepest open problem.
4. **Erasure interaction.** Disp has a strip pass (§10) — effectful code
   should be either fully retained (because it's run) or strippable along
   with its erased witnesses. Effects that produce proof-only outputs are
   strippable; effects that perform I/O are not. The strip pass needs an
   effect-aware rule.
5. **Universe polymorphism interaction.** Frank's ambient abilities and
   Koka's row polymorphism both have a "universe of effect rows" parallel
   to the universe of types. Disp has a single `Type` (no levels yet);
   when (if) it gains universe levels, the effect-row universe needs to
   be sorted out simultaneously.
6. **Scoped vs algebraic.** Wu-Schrijvers-Hinze "Effect Handlers in Scope"
   (2014) showed that some genuinely useful effects (exception-catching
   with rollback, parallel-or) are *not* algebraic in the Plotkin-Pretnar
   sense and require the more general scoped-effect machinery. Disp's
   wait-form mechanism is general enough to express scoped effects (the
   handler can inspect the body before resuming) but the equational
   reasoning gets harder.
7. **The Ahman programme.** "Handling Fibred Algebraic Effects" gives a
   dependent-type-native handler treatment, but only over a fibred
   semantics. Whether disp's parametric-walker semantics can encode
   fibred algebras is unstudied — likely the most fruitful direction for
   a research paper coming out of disp.

---

## References (key papers)

- Bauer & Pretnar (2015). "Programming with Algebraic Effects and Handlers." JLAMP 84(1). arXiv:1203.1539.
- Plotkin & Pretnar (2013). "Handlers of Algebraic Effects." LMCS 9(4).
- Leijen (2014). "Koka: Programming with Row-Polymorphic Effect Types." arXiv:1406.2061.
- Leijen (2017). "Type Directed Compilation of Row-Typed Algebraic Effects." POPL. DOI 10.1145/3009837.3009872.
- Xie, Brachthäuser, Hillerström, Schuster, Leijen (2020). "Effect Handlers, Evidently." ICFP. DOI 10.1145/3408981.
- Xie, Cong, Li, Lorenzen, Leijen (2022). "First-Class Names for Effect Handlers." OOPSLA. DOI 10.1145/3563289.
- Brachthäuser, Schuster, Ostermann (2020). "Effekt: Capability-Passing Style ..." JFP 30. DOI 10.1017/S0956796820000027.
- Brachthäuser, Schuster, Ostermann (2020). "Effects as Capabilities ..." OOPSLA. DOI 10.1145/3428194.
- Lindley, McBride, McLaughlin (2017). "Do Be Do Be Do." POPL. arXiv:1611.09259.
- Sivaramakrishnan, Dolan, White, Kelly, Jaffer, Madhavapeddy (2021). "Retrofitting Effect Handlers onto OCaml." PLDI. DOI 10.1145/3453483.3454039. arXiv:2104.00250.
- Maillard, Ahman, Atkey, Martínez, Hriţcu, Rivas, Tanter (2019). "Dijkstra Monads for All." ICFP. arXiv:1903.01237.
- Kiselyov & Ishii (2015). "Freer Monads, More Extensible Effects." Haskell. DOI 10.1145/2804302.2804319.
- Wu, Schrijvers, Hinze (2014). "Effect Handlers in Scope." Haskell.
- Kammar & Pretnar (2017). "No value restriction is needed for algebraic effects and handlers." JFP. arXiv:1605.06938.
- Brady (2013). "Programming and Reasoning with Algebraic Effects and Dependent Types." ICFP. DOI 10.1145/2500365.2500581.
- Brady (2014). "Resource-Dependent Algebraic Effects." TFP. DOI 10.1007/978-3-319-14675-1_2.
- Ahman (2018). "Handling Fibred Algebraic Effects." POPL. DOI 10.1145/3158095.
- Ahman, Ghani & Plotkin (2016). "Dependent Types and Fibred Computational Effects." FoSSaCS.
