# Reflection as an effect

Status: prototype landed 2026-07-07 in `lib/tests/reflect_proto.test.disp` (23 pins),
building on the two-horns characterization in `lib/tests/soundness.test.disp` and the
free-monad effects layer in `lib/std/effect.disp` (TYPE_THEORY §15). This note records
the idea, what is demonstrated, what is deliberately out of scope, and the path from
here. Companion piece: [`TOWER.md`](TOWER.md), which uses this machinery as one of its
building blocks.

## The problem: looking is untypeable

The walker enforces parametricity by rejection. Checking `f : A -> B` mints a fresh
hypothesis for the argument and runs `f`'s body through `param_walker`, which refuses
any attempt to reflect on that hypothesis: triaging its shape, reading its payload.
A function that passes cannot have looked inside its abstract inputs, so it behaves
uniformly, so its type is honest. This is the soundness mechanism, not a limitation
to be engineered around.

The cost is that every function whose job is inspection is untypeable as stated.
`is_fork : Tree -> Bool` is walker-rejected, because verifying it would require
running a triage on a minted hypothesis. The same applies transitively to the
kernel's own interpreter: `param_walker` triages every head it dispatches, so it
cannot walk itself, and it carries a `SEALED(interpreter)` marker instead of a type.

The two-horns pin in `soundness.test.disp` shows this is not an artifact of how the
walker happens to be written. The natural typed facade routes the abstract head first
and delegates the rest, and its head guard must be two things at once: walkable
(runnable under the annotation check, which mints the head) and forge-proof (a stem
whose child is `hyp_sig` wears the neutral signature, since `pair_fst (t hyp_sig)`
is `hyp_sig`). The two available guards each satisfy exactly one. `is_neutral` rides
the sanctioned readers and is walkable, but routing on it manufactures the counterfeit
hyp-fork that `w_stem` exists to reject. `is_hyp_fork` adds the fork-shape guard and
is forge-proof, but the guard itself triages the minted head. The minimal pure facade
is either unsound or uncheckable. Any avenue to typing inspection has to change the
shape of the question, not the guard.

## The move: ask, don't look

Distinguish performing an inspection from requesting one.

A function that performs triage does something to its argument. Under a check, doing
that to a hypothesis is exactly the reflection the walker rejects. A function that
requests triage builds a value: a free-monad node that says "classify this tree, then
continue here", with the continuation as an ordinary function awaiting the answer.
Building a value that mentions a hypothesis is construction, and construction over
hypotheses is what the walker permits. Constructors are free; this is the same
observation that made `tree_rec` possible (dispatch is elimination, and raw triage is
its fused compiled form).

So the shape is the standard algebraic-effects shape from `std/effect.disp`:

    is_fork_eff := {v} -> eff_bind (classify v) ({s} -> io_pure (tree_eq (sh_tag s) "fork"))

`is_fork_eff` never looks at `v`. It emits one `classify` request and a pure
continuation. The one place that looks is the handler, a single clause that performs
`is_hyp_fork` and then a raw triage, at handle time, on values as data. The walker's
prohibition is about checks; the handler is the floor, and the floor was always going
to exist (see the trust ledger in `TOWER.md`). What changed is its width: one clause.

The slogan the pins encode:

    looking is untyped; asking is typed; the row is the modality.

## The Reflect signature

One operation today:

    classify : Tree -> ShapeR
    ShapeR   = leaf | stem child | fork l r | hyp stored-type

The response vocabulary is the observation interface itself. For a transparent value
the answer is its shape with the components as data. For anything wearing the
hypothesis shape the answer is `hyp` with the stored type (the already-sanctioned
`neutral_type` read), and nothing else: the payload stays unreadable, which is the
extraction-leak boundary the kernel already enforces.

The `hyp` arm is what dissolves the two horns for programs. A Reflect program never
guards on `is_neutral` (unsound at the forge boundary) or `is_hyp_fork` (unwalkable).
It asks, and the sealed handler makes the distinction where the knowledge lives. Note
the honest semantics: the handler answers `hyp` for anything wearing the hyp shape,
including a forged fork. That is not a weakness; it is exactly the answer the walker's
own stem rule needs, and `walk_spec` below uses it to reproduce stem-forge rejection
by asking about the fork it just built.

## The row is the modality

The type `Tree -> Bool` claims a pure function; `is_fork` cannot honestly carry it,
and the walker agrees. The type `Tree -> EffAt [Reflect]` says "give me a tree and I
will give you a computation that needs the classify capability to finish". `is_fork_eff`
carries it, machine-checked, today (via the `EffAt` Refinement lift; the deep `Eff R X`
recognizer is the production form). A classify request built over an abstract tree is
itself a well-typed value; both are pinned.

This is the sealing modality from `SEALING.md`, realized as a library effect row
instead of a new judgment. The `SEALED(reflection)` markers in kernel comments have
been informal types all along: "this code inspects representations". `Eff [Reflect] X`
is that sentence as a checkable type. The intended theory attaches here too: rows
without `Reflect` are the parametric fragment where free theorems live, handlers are
the mediating morphisms, and the noninterference story (the DCC framing in the sealing
notes) becomes a statement about rows rather than about the walker's body.

Relation to `tree_rec`: the two are complementary faces of the same reframe. When a
function's domain is honest structural recursion over concrete trees, write it against
`tree_rec` and it is typed as an ordinary eliminator client. When it must interrogate
arbitrary trees, hypotheses included, or when it is interpreter-shaped, it is
Reflect-shaped. `tree_rec` types the eliminative face; Reflect types the
interrogative face.

## The carve-outs become a signature

The walker sanctions exactly four readers as carve-outs (`pair_fst`, `neutral_type`,
`tree_eq`, identity), hardwired in `param_walker`'s head checks. Those four are the
current observation interface, expressed as interpreter special cases.

Under this design they are the beginning of an effect signature. Growing the
sanctioned readers stops being a walker edit with diffuse soundness obligations and
becomes adding an operation to `Reflect`: a library decision with a visible type, an
explicit handler clause, and a per-op soundness argument. The obvious next two are
already marked in the prototype: minting (`bind_hyp` is a floor capability and wants
to be an op; the prototype delegates that route verbatim) and the §7A policed route.
The trust surface becomes enumerable, and enumerated in one place.

## walk_spec: the interpreter's algorithm, typed

The prototype's centerpiece rewrites `param_walker`'s algorithm as a Reflect program.
Pure `tree_eq` head checks stay pure. Every shape decision is a classify request. The
stem rule builds `fork(a, x)` and then asks about the tree it just built; a forged
fork answers `hyp`, which the spec maps to `Err`, reproducing `w_stem`'s rejection.
The hyp route is an ordinary case. Two routes (minting, the policed token) are
delegated verbatim and marked.

Pinned results:

  - `walk_spec` agrees with the live `param_walker` across the dispatch rules (the K,
    S, and triage rules; real combinators run to completion), the carve-outs, the hyp
    route, triage-on-neutral rejection, and stem-forge rejection.
  - `walk_spec` carries `Tree -> Tree -> EffAt [Reflect]`, machine-checked. The
    checker's dispatch logic now exists as a typed object-language value.

What this is: the spec face of the interpreter, in the same two-copies discipline as
`nat_rec_fast` (a typed reference plus a fused fast form, related by pins now and by
a license where statable). What this is not: a replacement for the live walker. A
handler round per application step is not viable for the checker's inner loop, for
the same grounding reason the cut's list toolkit cannot route through `elim`. The
live walker remains the fused form; the spec is what you read, check, and test
against.

## Limits, and the shared critical path

Two limits are marked inline in the prototype.

First, the row check is shallow. `eff_check` in the effects prototype probes
continuations with a leaf, so it follows one path: branchy programs need a
junk-tolerant default arm to stay visible to it, and only a program's first
operations are row-checked (an under-approximation that can pass programs whose
later branches use other effects). Second, continuations are not yet checked under
a real minted response, so "the continuation has the right type at every answer" is
not yet part of the certificate.

Both trace to the same deferred item: the self-referential `Eff R X` recognizer,
where the continuation is recursion under a Pi (the `RecUnder` reader-functor route,
or a `make_recognizer` fix knot). That item was already the effects arc's next step;
it is now doubly load-bearing, since it upgrades `walk_spec`'s certificate from
shallow row membership to deep continuation typing. No other single piece of work
pays into both programs at once.

## Path from here

1. The `Eff R X` recognizer (k under Pi). Unlocks deep typing for both arcs.
2. `ShapeR` as a real Coproduct, so continuations eliminate the response through a
   recognized type instead of tag tests, and `classify` gets the honest type
   `Tree -> Eff [Reflect] ShapeR`.
3. Minting as a second Reflect op; the policed route as a third. The prototype's two
   delegations disappear and the signature covers the walker's whole special-case
   table.
4. The differential harness as a permanent fixture: generators over heads and
   arguments, `run_reflect (walk_spec f x)` against `param_walker f x`, including the
   adversarial cases from `soundness.test.disp`.
5. The theory note: free theorems for Reflect-free rows, the DCC reading of handlers,
   and how the row modality relates to the step-indexed sealing conjecture.

## Files

  - `lib/std/effect.disp`: the free monad, handlers, rows, `EffAt`.
  - `lib/tests/reflect_proto.test.disp`: the Reflect signature, the floor handler,
    the twins, the modality pins, `walk_spec` and its differential battery.
  - `lib/tests/soundness.test.disp`: the two-horns characterization this answers,
    and the K0 floor pins it lives next to.
  - `lib/tests/tree_rec.test.disp`: the eliminative face (typed twins over concrete
    shape recursion).
