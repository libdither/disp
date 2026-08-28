
  ## The decisive counterexample

  Current function oeq is the pointwise Pi relation in lib/std/oeq.disp:48. Pi checking runs the witness at a minted neutral, while is_neutral can observe that fact through the sanctioned readers in lib/kernel/
  engine.disp:156.

  I executed this probe:

  id    := {n} -> n
  shift := {n} -> if (is_neutral n) then n else succ n
  proof := {n} -> refl

  param_apply (oeq (Arrow Nat Nat) id shift) proof = Ok true

  id 0    = 0
  shift 0 = 1

  An actual license_guard (oeq (Arrow Nat Nat)) rebind from id to shift was also accepted.

  This is stronger than the junk-result example in ACTIVE_BUGS.md:124: both outputs remain Nats, so use-site rechecking catches nothing. Rechecking protects membership and consistency; it cannot protect program behavior.

  Therefore ACTIVE_BUGS item 5 is already a blocker for proof-certified optimization, not only for future erasure.

  ## What oeq currently means

  The equality layers are being conflated:

   Mechanism                    What it actually establishes
  ━━━━━━━━━━━━━━━━━━━━━━━━━━━  ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   tree_eq                      Structural identity of evaluated trees
  ───────────────────────────  ─────────────────────────────────────────────────────────────────────────────────────
   Eq T x y                     Intensional/convertibility equality witnessed by refl
  ───────────────────────────  ─────────────────────────────────────────────────────────────────────────────────────
   current function oeq         Agreement during a neutral-probe execution
  ───────────────────────────  ─────────────────────────────────────────────────────────────────────────────────────
   needed optimizer equality    Agreement under a specified class of observers, recursively at every type and phase

  The standard normalization properties mentioned in READING_QUESTIONS.md—soundness, completeness, and stability/round-trip—can decide definitional equality in a normalizing calculus. That is exactly what
  normalization-by-evaluation results establish, not general behavioral equality. Altenkirch and Kaposi explicitly prove normalization, completeness, stability, and decidability of definitional equality
  (https://arxiv.org/abs/1612.02462).

  That route cannot decide Disp’s intended oeq:

  - Tree calculus is Turing-complete, so normalization need not terminate (archive/live-kernel/TYPE_THEORY.typ:191).
  - The optimizer specifically wants to relate different normal forms, such as an inductive implementation and its constant-time replacement.
  - Unrestricted Disp contexts can distinguish any distinct trees with tree_eq; as already observed in research/OPTIMIZER.typ:231, unrestricted contextual equivalence collapses to structural identity.

  So the desired equality must be observer-restricted. That observer restriction is currently described but not enforced.

  ## oeq has a second, independent problem

  The current function lift is:

  ∀ a. R_B (f a) (g a)

  For setoids or logical relations it needs, in general:

  ∀ a₀ a₁. R_A a₀ a₁ → R_B (f a₀) (g a₁)

  The second form requires functions to preserve the domain relation. LinkedPi already contains the skeleton of this cross-related binder in lib/std/oeq.disp:123, but lift_setoid does not use it.

  This matters immediately for custom setoids. I confirmed an all-Tree type quotiented by “is it a leaf?” allowed a licensed replacement of a stem with a fork, even though the well-typed observer is_fork distinguished them.

  Standard setoid and PER systems avoid this:

  - NuPRL ties membership to self-relatedness and requires functions to respect equality on their inputs. Its quotient functions must prove that they preserve the quotient relation. NuPRL’s presentation states both
    requirements directly (https://nuprl-web.cs.cornell.edu/book/Introduction_Type_Theory.html).

  - In ordinary setoid models, a function comes equipped with a proof that related inputs map to related outputs. Sterling, Angiuli, and Gratzer summarize this construction (https://arxiv.org/abs/2003.01491).
  - Modern observational type theory integrates equality, casts, and computation into the calculus itself; it is not merely an optional setoid attached to otherwise unrestricted functions. TTobs
    (https://pujet.fr/pdf/OTT_now_for_good.pdf) proves normalization and decidable conversion for such an integrated design.

  Thus behavioral_specs.setoid currently supplies an equivalence relation, but not congruence, representation sealing, or respectful elimination.

  ## Why case_value → cut cannot be a general oeq

  The two operations intentionally disagree on open inputs:

  - Raw cut reads a concrete tag. It fails closed on a neutral and is wrong on shape-encoded Nat, as pinned in lib/tests/case_value.test.disp:23.
  - case_value has a concrete dispatcher face and a neutral face that pads the arms and routes through the type’s gated respond (lib/kernel/generic.disp:109).

  The valid statement is narrower:

  Given a witnessed cut-class type T,
  a closed canonical constructor c : T,
  and a well-formed case table,

  eval(case_value T P cases c)
    = eval(cut (compile_cases T cases) c).

  It is a closed-runtime/compiler theorem, not equality of the two source-language functions on neutrals.

  Current case_equiv does not prove even its claimed subset:

  - It checks one Nat instance and one fixed LicSum with arities 0, 1, and 2 (lib/std/case.disp:37). A candidate can delegate on those exact types and misbehave on a fresh arity-3 coproduct; the license accepts it.
  - More deeply, its “abstract” arms and payloads are ordinary Pi binders. I verified a candidate that delegates while the selected arm is neutral but returns 0 when that arm is concrete. Every current case_equiv obligation
    accepts refl, while real LicSum dispatch disagrees.

  - The same attack can be placed in the motive, payload, arm, type descriptor, or any nested argument.

  Therefore top-level “neutral plus concrete constructors” is not enough. Each abstract binder recursively repeats the same problem. No finite collection of at_cutN examples fixes this while candidates can recognize the proof
  environment.

  case_fast itself still appears plausibly correct by inspection and differential tests. The finding is that its current certificate does not prove that correctness.

  ## Why the walker/effect-spec bridge is not ready

  Several independent blockers stack here.

  First, pure param_walker : Tree → Tree → CheckerResult is constitutively uncheckable. Its job is to inspect the head it receives; checking it mints an opaque head. The two possible facades—walkable but forgeable, or forge-
  proof but unwalkable—are pinned in lib/tests/soundness.test.disp:126.

  Reifying inspection as an effect remains the correct direction, but today’s spec certificate is shallow:

  - walk_spec2 is annotated with EffAt, not deep Eff R X (lib/std/kernel_spec.disp:115).
  - EffAt ignores Pure result types and probes continuations with t (lib/std/effect.disp:153).
  - Consequently verification follows the first classify request into its default branch. It does not check the real ShapeR arms, later Mint/Check requests, or final CheckerResult.

  Replacing EffAt mechanically with deep Eff is insufficient. The spec branches on pair_fst response with tag comparisons, so a minted response again selects only one neutral path. Those branches must use gated rec_value
  elimination.

  There are also concrete mismatches:

  - The escaping Mint case returns Err from the spec but Ok Err from raw param_walker (lib/tests/kernel_spec_proto.test.disp:60). Exact equality is currently false.
  - hyp_reduce_spec treats MetaShape.respond : Tree as an Action, without a typed cast.
  - A deeply checked fork branch exposes raw type_meta f on a neutral.
  - Extend has a dynamic result type, but nominal neutral recognition does not widen that result to Tree.
  - Deeply following the raw recursive fix is liable to unfold forever.

  Finally, effect rows are not yet the promised parametricity modality. I verified that this passes with an empty effect row:

  covert := {n} ->
    io_pure (if is_neutral n then true else false)

  Reflection remains available through untracked pair_fst/tree_eq carve-outs. Offering an effectful Reflect operation does not help until raw reflection is unavailable in the certificate fragment.

  ## The resolution

  The fix needs observer and phase separation, not a more permissive oeq.

  ### Immediate containment

  1. Stop treating current function oeq, guard_eq, and case_equiv as proof-certified behavioral replacement.

  4. Decide whether the walker theorem concerns raw param_walker or public normalized application, and eliminate the Err/Ok Err mismatch.

  ### Build a genuine certificate fragment

  5. Make reflection exclusive to a tracked capability:
      - tree_eq, pair_fst, neutral_type, and is_neutral on abstract values must require Reflect.
      - Typed eliminators should be opaque/gated operations whose internal neutral dispatch does not expose the bit to clients.
      - apply_policed must not allow escaping from the stricter mode back into the ordinary walker.
      - Neutral application must check its argument domain; ACTIVE_BUGS item 3 is otherwise another hole in the logical-relation fundamental theorem.

  The existing stricter walker in lib/tests/ext_gate_proto.test.disp:59 is a useful prototype, but it is not yet closed under all calls.

  A modal/layered split is established practice here: intensional inspection is permitted on code at the meta layer, not on arbitrary inhabitants at the extensional layer. Layered Modal Type Theory
  (https://link.springer.com/chapter/10.1007/978-3-031-57262-3_3) is especially close to Disp’s “Tree as code” situation.

  6. Redefine the license relation as a PER or binary logical relation:

  R_(A→B)(f,g) =
    ∀ a₀ a₁.
      R_A(a₀,a₁) →
      R_B(f a₀,g a₁)

  For dependent functions, the codomain relation must be heterogeneous/displayed. Membership should be self-relatedness, or the existing recognizer must separately prove compatibility with it.

  7. Permit global replacement only in Reflect-free/respectful contexts. In reflective contexts the only valid global license remains tree_eq. Alternatively, make guarded definitions opaque or change optimization to local
     rewriting with a proof that the surrounding context is Proper.

  ### Prove the two bridges at their proper levels

  8. For case_value → cut:
      - Introduce a typed CutClass T descriptor containing variants, the identity-view law, canonicity, and the case-table compilation.
      - Prove the closed theorem by induction over the actual variant list and argspec spine.
      - Keep neutral behavior exact by delegating to case_value.
      - Certify specializations case_fast T, rather than globally rebinding the polymorphic case_value.
      - Make the certificate first-order data checked by a small total checker, not an arbitrary proof function that can inspect its arguments.

  9. For param_walker → effect spec:
      - Upgrade to Eff row CheckerResult.
      - Route every ShapeR, Action, and CheckerResult branch through gated eliminators.
      - Strengthen/cast respond, remove hidden raw metadata reads, and use carrier-specific handlers.
      - Factor a finite, nonrecursive walk_step call f x.
      - Represent recursion as a Call operation or explicit fuel.
      - Prove step-indexed simulation/bisimulation, then close the recursive knot.

  Because Tree calculus permits recursion and divergence, step-indexed logical relations are the standard tool; they are also used for row-typed algebraic effects and handlers. See Ahmed’s step-indexed PER development
  (https://people.mpi-sws.org/~dreyer/ats/papers/ahmed06-tr.pdf) and “Handle with care” (https://dl.acm.org/doi/10.1145/3158096).

  The clean long-term endpoint is to generate the fused walker from the typed effect spec using one verified fusion/partial-evaluation pass. Then the bridge is a compiler theorem established once, rather than an attempted oeq
  proof between two independently written recursive interpreters.

  In short: the missing concept is a recursively enforced, observer-indexed logical relation with a real Reflect boundary. The current minted-neutral probe is a useful checker technique, but it cannot serve as semantic
  universal quantification while programs are allowed to observe that they are being probed.