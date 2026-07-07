# The tower: structural checking of kernel text

Status: design note, 2026-07-07. Nothing in this file is built beyond stage 0 (the
inventory below). Companion piece: [`REFLECT.md`](REFLECT.md), which supplies two of
the tools stage 1 needs. The verified-optimizer connection is `OPTIMIZER.typ`.

The tower is the plan for the strongest self-checking this system can honestly
reach: a structural, syntactic checker for kernel text, written as ordinary typed
disp code, itself checked behaviorally by the running kernel, with explicit bridge
theorems connecting the two kinds of acceptance, and an explicit ledger of what
remains trusted at the bottom. The name and the shape come from Milawa, explained
below, because Milawa is the existence proof that this architecture works at scale.

## Two kinds of checking

Disp has one checker today, and it is behavioral. To check `v : T`, run `T` (a
predicate, a tree program) on `v`; to check a function, mint a fresh hypothesis,
run the body through the walker, and see what comes back. Acceptance is defined by
execution. This is the semantics of the language, not an approximation of it, which
is its great strength: there is no gap between "what the checker accepts" and "what
the system means", because the checker is the meaning.

Behavioral checking has a boundary, and we now know its exact shape. Code whose job
is to inspect representations cannot be verified by running it on an opaque
hypothesis, because the inspection is the very act the walker rejects. The
two-horns pin in `lib/tests/soundness.test.disp` shows the boundary is one line
wide and semantically forced: a typed facade for the walker needs a head guard that
is both walkable and forge-proof, and the two available guards each satisfy exactly
one. So the walker's body, the floor readers, and the substrate dispatch are
constitutively outside behavioral checking. They are pinned by tests and marked
`SEALED`, which is a comment, not a certificate.

Structural checking is the other kind: recurse over the text of a term and decide
acceptance by its shape, the way a conventional type checker works. A structural
checker never runs the term on anything, so the walker's prohibition does not apply
to its subject matter; the term is data. And here is the pleasant inversion: a
structural checker is itself a perfectly ordinary program, a fold over trees with
some bookkeeping, exactly the kind of code behavioral checking is good at. After
the tree_rec rung it can be written against typed structural recursion, and after
`REFLECT.md` its inspection of arbitrary trees (hypotheses included) can carry an
honest effect type. Nothing about a structural checker needs to be sealed.

So each kind covers the other's blind spot. Behavioral checking grounds meaning but
cannot see intensional text; structural checking sees all text but has no inherent
connection to meaning. The tower is the discipline for composing them so that the
connection is itself checked.

## Milawa, precisely

Milawa is a theorem prover built by Jared Davis (dissertation, UT Austin, 2009,
titled "A Self-Verifying Theorem Prover"). Its logic is ACL2-like: first-order,
total recursive functions over Lisp objects. What makes it relevant here is not the
logic but the architecture.

Level 1 is a small proof checker: a function, a few hundred lines, that accepts
only fully primitive proof objects, where every step is an axiom instance, a
propositional rule, an instantiation, an induction in its rawest form. It is small
enough that a person reads it and decides to trust it. That act of reading is the
system's trust anchor; nothing inside the system justifies level 1.

Level 2 is a more capable checker: it accepts proofs written with derived rules and
small automations that level 1 does not know. The crucial step is that level 2's
acceptance function is itself a function in the logic, and Davis proves, in the
logic, a faithfulness theorem: anything level 2 accepts, level 1 could also accept
(a fully primitive proof of the same formula exists). The proof of that theorem is
checked by level 1. Once it passes, running level 2 is as trustworthy as running
level 1, and level 2 is faster and more convenient.

Iterate. Each level's faithfulness relative to the previous is proven and checked
by the previous level, about a dozen levels in total, ending in a prover with
rewriting and simplification comparable in spirit to a usable fraction of ACL2. The
composed guarantee is: anything the top level accepts, level 1 would accept. The
entire chain of faithfulness proofs is ultimately one enormous proof object
checkable by the tiny level-1 function, and it was checked in full (a long
machine-scale run; that is fine, it happens once).

Two more facts complete the picture. First, later work with Magnus Myreen ran the
whole tower on Jitawa, a Lisp runtime verified in HOL4 down to x86 machine code, so
the trusted base became "the HOL4 verification of the runtime plus the level-1
checker" rather than "an unverified Lisp implementation". Second, and this is the
part to internalize: Milawa never proves its own soundness or consistency. Every
internal theorem is relative: level N+1 is faithful to level N. Soundness of level
1 is an external judgment; consistency of the logic is assumed the way every
working mathematician assumes it. Milawa is not a counterexample to Gödel. It is a
demonstration of how much room Gödel leaves.

The mapping to disp is direct. Disp's level 1 already exists and is already better
than a function you trust by reading: it is the behavioral kernel, the thing whose
acceptance is the language's semantics, and it already checks the whole library and
most of itself (the inventory below). The structural checker is disp's level 2: a
richer, syntactic acceptance function, written as ordinary code, whose faithfulness
to the behavioral kernel is the bridge obligation. Where Milawa's levels all speak
one proof language, disp's two levels speak different idioms (running versus
reading), so the faithfulness statements are per syntactic class rather than one
theorem, and they start life as differential test harnesses before they become
proofs. The shape is otherwise the same: the small semantic thing validates the big
syntactic thing once; the big thing then does work the small thing cannot.

## The Gödel boundary, and what a coherence fixpoint is

Three properties need to be kept apart, because self-checking claims live or die
on which one is meant.

Soundness: everything the checker accepts is semantically right. For any system
strong enough to encode arithmetic (disp encodes arithmetic), a general internal
soundness principle is off the table. Löb's theorem is the sharp version: if the
system proves "if `P` is provable then `P`" for a formula, it proves `P` outright,
so a system that internally trusted its own checker in general would trust
everything. This is why Milawa's theorems relate level to level and never say
"accepted implies true".

Consistency: no proof of False. Gödel's second incompleteness theorem: a
consistent, effectively axiomatized system interpreting enough arithmetic cannot
prove its own consistency. So "the kernel cannot derive False" is not an available
internal theorem either.

Coherence: the system, applied to itself, accepts. Its components agree wherever
they can be compared. Every internal check that could fail, does not. Unlike the
first two, coherence is internally establishable, executable, and regression-
checkable, and it is not vacuous, because each coherence check is falsifiable and
each failure is a real bug. What coherence does not give: a coherent system can be
wrong, consistently. A hypothetical unsound kernel could accept an unsound
structural checker that faithfully mirrors its unsoundness, and the whole package
would hum along in agreement. Coherence is trust concentration, not trust creation.

Now the fixpoint language. Loading the system computes a set of self-referential
verdicts: the kernel's fragments verify through the kernel; the prelude's
annotations verify through the barrel's checked re-import; the raw and checked
passes bind identical values per name (the barrel's dedupe is exactly this check,
and it is the reason "bootstrap fixpoint condition" already appears in the code
comments). Call the system a coherence fixpoint when the full self-referential
package returns accept, on every load: the behavioral kernel accepts the structural
checker's definition; the structural checker accepts the kernel's text, including
its own text and the behavioral kernel's; the bridge lemmas connecting the two
acceptances are themselves accepted. The word fixpoint is meant literally: checking
maps system state to verdicts, and the tower is a state that checking maps to
"yes" everywhere it points at itself.

"The strongest coherence fixpoint available this side of Gödel" therefore means:
the maximal self-referential package, where every correctness claim in the system
is reduced by machine-checked steps to a small enumerated residue that Gödel
guarantees can never be empty. The residue is the point. An unsoundness, if one
exists, must live in the residue, because everywhere else a bug breaks an
executable check. The tower does not shrink what must be trusted to zero; it
shrinks it to a list you can print, and it keeps the list honest under change.

## What exists today (stage 0)

The behavioral half of the tower is running:

  - Every kernel fragment's typed exports verify through the kernel at load
    (`verify` pins in `lib/tests/use_raw.test.disp`); the standard library and the
    prelude (fragment -1) verify the same way on every load.
  - The universe participates: `StrictType` inhabits itself; `Type` is
    `BehavioralType` (R6), so respond behavior is itself checked.
  - The walker's interface is typed through the §7A token (`param_apply`,
    `checked`, `verify` carry machine-checked annotations); its body is sealed, and
    the seal is characterized, not just asserted (the two-horns pins).
  - The barrel's open-dedupe is an executable fixpoint condition: raw and checked
    passes must agree per name.
  - A miniature structural checker already runs inside every gated inductive:
    `coh_check` derives case types from a type's declared variants and checks the
    supplied cases against them by eta-readback. That is structural checking of a
    tiny syntactic class (eliminator case records), validated behaviorally, in
    production today. The tower generalizes this seed.
  - The tools for writing bigger structural checkers as typed code landed with the
    tree_rec rung and `REFLECT.md`: typed structural recursion over trees, typed
    inspection with the hypothesis case, and `walk_spec` as the first typed
    interpreter-shaped program with a differential harness against the live one.

## The process

Each stage lands as characterization pins first, mechanism second, and nothing at
any stage may weaken the walker to make the tower's life easier (the generalization
of the optimize-the-evaluator-not-the-strictness rule). Levels are pull-driven:
build a checker level when a consumer needs it, not for architecture's sake.

Stage 1, syntax as data (tools; largely landed). Typed recursion over trees
(`tree_rec`), typed inspection (`Reflect`), and the deep `Eff R X` recognizer when
it lands (the shared critical path named in `REFLECT.md`).

Stage 2, a structural checker for one class. Write `accepts : Term -> Bool` (or a
richer verdict) for a first syntactic class, as ordinary annotated disp, verified
behaviorally at load like any other library. Candidate classes, in rough order of
consumer pull: the certificate formats of the licensing layer (the verified
optimizer's `(e', cert)` needs exactly a structural cert checker, so OPTIMIZER.typ
is the first real customer); bracket abstraction and CIR equivalence (reviving the
deleted `lib/elab`, which was this checker's ancestor and is recoverable from git);
recognizer-body wellformedness (`RecognizerShape`, the §11 deferred item).

Stage 3, the bridge. For each class, the faithfulness statement: structural
acceptance implies behavioral acceptance. First as a differential harness
(generators over the class, both checkers, agreement pins, adversarial cases from
the soundness suite), which is the tree_eq native-fast-path discipline applied at
checker scale. Then, where statable, as per-constructor lemmas checked by the
behavioral kernel, the way `coh_check` already discharges its per-constructor
obligations. Faithfulness runs one direction; completeness (behavioral implies
structural) is explicitly not claimed, since the structural checker may be honestly
partial on classes it does not cover.

Stage 4, the top of the tower. The structural checker's class grows until it
covers kernel text, including the walker's definition and the structural checker's
own. At that point the package closes: the behavioral kernel accepts the structural
checker (stage 2), the structural checker accepts the kernel's text including
itself (this stage), and the bridges connect them (stage 3). This is the coherence
fixpoint. The honest way to state the achievement is the ledger below, not a
soundness claim.

## The trust ledger at the top

What remains outside the fixpoint, permanently, by Gödel and by construction:

  1. The substrate: the five tree-calculus rewrite rules and their host
     implementation (`src/core/tree.ts`), plus the reproducibility discipline
     across evaluator backends that stands in for "the rules run as written".
  2. The floor: the sealed definitions whose bodies are the reflection the walker
     rejects (`hyp_reduce`, `make_hyp`, the support scan, the walker's body, and
     under `REFLECT.md` the one handler clause that looks). Pinned behaviorally,
     enumerated in the soundness suite, never certified from inside.
  3. One metatheorem: sealing preserves parametricity (the step-indexed logical
     relation sketched in the sealing notes). Paper mathematics first; its job is
     to justify the floor's shape, not to be replaced by an internal proof, which
     Löb forbids in the general form.

Everything else in the system is intended to sit above this line, connected to it
by machine-checked steps. That is the whole ambition, stated at its most modest:
not a system that proves itself right, which nothing can be, but a system whose
every unproved assumption fits on one page, stays on that page as the system grows,
and is guarded by executable checks the moment anything drifts.
