# The embedding theorem: formal core of spatial IC

Companion to `SPATIAL_IC.md` (delivers the polarity lemma promised in its §2.2 and the
simulation theorem promised in its §14.1) and to `tc-net.typ` (whose agent table and rule
tables are the object of study). Everything here is about the Rooted TC-Net calculus as
specified in `tc-net.typ` and implemented in `evaluators/rust-ic-net/crate/src/reduce.rs`,
extended with one new agent (the forwarder ι) and placed on a grid. Status: the polarity
lemma and the created-short lemma are proven here by full case analysis over the rule table;
the simulation theorem is proven modulo the standard interaction-net metatheory it inherits;
the lower bound is sketched. The honest per-claim accounting is §9.

## 1. Port polarity

Assign every port of every agent kind one of two orientations: **out** (a value flows out of
the port, into the wire) or **in** (a value flows in from the wire, into the agent). The
assignment is forced by the calculus's own reading of each agent (`tc-net.typ` § Agents):
producers emit their value at the principal; consumers receive a value at the principal and
emit their result at `res`; operand slots receive; the duplicator emits two copies.

| Agent | principal `p` | other ports |
|---|---|---|
| `L` (leaf) | out | (none) |
| `S` (stem) | out | `x` in |
| `F` (fork) | out | `l` in, `r` in |
| `P` (suspended app) | out | `f` in, `a` in |
| `E` (demand) | in | `res` out |
| `A` (apply) | in | `arg` in, `res` out |
| `T1` (dispatch 1) | in | `b` in, `c` in, `res` out |
| `T2` (dispatch 2) | in | `w` in, `x` in, `b` in, `res` out |
| `δˢ`/`δⁿ` (duplicators) | in | `l` out, `r` out |
| `ε` (eraser) | in | (none) |
| `N` (normalizer) | in | `res` out |

`N` is the implementation's recursive normalizer (`reduce.rs`), not in `tc-net.typ`'s agent
table; it is the full-normal-form driver and is included here because the embedding must
carry it. `E` is in the spec but the implementation folds weak-head demand into `N` and the
dispatch chain; it is included for completeness.

**Polarity lemma.** In any net reachable from the encoding of a tree-calculus term (possibly
with a root consumer attached), every wire connects exactly one out-port to one in-port.

Two immediate readings. First, the active-pair invariant is a special case: an active pair is
two principals joined, and every producer principal is out while every consumer principal is
in, so an active pair is an out-in wire whose two ends both happen to be principals. Second,
a parked consumer (a δⁿ or N waiting on an auxiliary result wire, `tc-net.typ`
§ Demand-Before-Copy) is an out-in wire that is not active only because one end is auxiliary,
not principal. Orientation and activity are thus separable: orientation is a property of
every wire, activity is orientation-plus-both-ends-principal.

*Proof.* Induction on reduction. The base case is the encoding (`tc-net.typ` § Encoding): a
stem wires `S.x` (in) to its child's principal (out); a fork wires `F.l`, `F.r` (in) to two
child principals (out); an application wires `P.f`, `P.a` (in) to two child principals (out);
a root consumer wires its principal (in) to the term's root principal (out). Every wire is
out-in.

The step case checks every rule right-hand side. Notation: in a rule `C(...) ⊗ D(...)`, the
lower-case variables name the surviving far ends of the dying agents' auxiliary wires. By the
inductive hypothesis each such far end has a fixed polarity: a producer child (`S.x`,
`F.l/r`, `P.f/a` far ends) is **out**; a consumer operand (`A.arg`, `T1.b/c`, `T2.w/x/b` far
ends) is **out**; a consumer result (`E/A/T1/T2/N .res` far ends) is **in**; a duplicator
output (`δ.l/r` far ends) is **in**. It suffices to check that every wire the RHS builds
joins one out to one in. Fresh agents contribute ports at their table polarity.

Demand (`tc-net.typ` § Demand and Application):

- `E(r) ⊗ L`: `L.p→r`. out-in (leaf principal to result). ✓
- `E(r) ⊗ S(x)`: rebuild `S` on `r`; `S.p`(out)-`r`(in) ✓, `S.x`(in)-`x`(out) ✓.
- `E(r) ⊗ F(a,b)`: rebuild `F` on `r`; `F.p`(out)-`r`(in) ✓, `F.l`(in)-`a`(out) ✓, `F.r`(in)-`b`(out) ✓.
- `E(r) ⊗ P(f,a)`: fresh `A[arg:=a,res:=r]` on `f`; `A.p`(in)-`f`(out) ✓, `A.arg`(in)-`a`(out) ✓, `A.res`(out)-`r`(in) ✓.

Application (`tc-net.typ` § Demand and Application; `reduce.rs` `A` arm):

- `A(c,r) ⊗ L`: build stem `S[x:=c]` on `r`; `S.p`(out)-`r`(in) ✓, `S.x`(in)-`c`(out) ✓.
- `A(c,r) ⊗ S(x)`: build fork `F[l:=x,r:=c]` on `r`; `F.p`(out)-`r`(in) ✓, `F.l`(in)-`x`(out) ✓, `F.r`(in)-`c`(out) ✓.
- `A(c,r) ⊗ F(a,b)`: fresh `T1[b:=b,c:=c,res:=r]` on `a`; `T1.p`(in)-`a`(out) ✓, `T1.b`(in)-`b`(out) ✓, `T1.c`(in)-`c`(out) ✓, `T1.res`(out)-`r`(in) ✓.
- `A(c,r) ⊗ P(f,a)`: fresh `A1[arg:=a,res:=t]` on `f`, fresh `A2[arg:=c,res:=r]` parked on `t`. `A1.p`(in)-`f`(out) ✓, `A1.arg`(in)-`a`(out) ✓; internal `t` = `A1.res`(out)-`A2.p`(in) ✓; `A2.arg`(in)-`c`(out) ✓, `A2.res`(out)-`r`(in) ✓.

Dispatch (`tc-net.typ` § Dispatch; `reduce.rs` `T1`/`T2` arms):

- `T1(b,c,r) ⊗ P(f,a)`: fresh `A[arg:=a,res:=t]` on `f`, rebuilt `T1` parked on `t`. `A.p`(in)-`f`(out) ✓, `A.arg`(in)-`a`(out) ✓; `t` = `A.res`(out)-`T1.p`(in) ✓; `T1.b`(in)-`b`(out) ✓, `T1.c`(in)-`c`(out) ✓, `T1.res`(out)-`r`(in) ✓.
- `T1(b,c,r) ⊗ L` (K rule): `r→b`, `ε→c`. `r`(in)-`b`(out) ✓; fresh `ε.p`(in)-`c`(out) ✓.
- `T1(b,c,r) ⊗ S(x)` (S rule): a `δⁿ` demanded on `c` producing `c1`, `c2`, and three applies `A1[arg:=c1,res:=t1]` on `x`, `A2[arg:=c2,res:=t2]` on `b`, `A3[arg:=t2,res:=r]` on `t1`. `δⁿ.p`(in)-`c`(out) ✓; `δⁿ.l`(out)-`A1.arg`(in) via `c1` ✓, `δⁿ.r`(out)-`A2.arg`(in) via `c2` ✓; `A1.p`(in)-`x`(out) ✓, `A2.p`(in)-`b`(out) ✓; `t1` = `A1.res`(out)-`A3.p`(in) ✓, `t2` = `A2.res`(out)-`A3.arg`(in) ✓; `A3.res`(out)-`r`(in) ✓. Computes `(x c)(b c)` with one δ supplying the two `c`.
- `T1(b,c,r) ⊗ F(w,x)` (triage): fresh `T2[w:=w,x:=x,b:=b,res:=r]` on `c`. `T2.p`(in)-`c`(out) ✓, `T2.w`(in)-`w`(out) ✓, `T2.x`(in)-`x`(out) ✓, `T2.b`(in)-`b`(out) ✓, `T2.res`(out)-`r`(in) ✓.
- `T2(w,x,b,r) ⊗ P(f,a)`: as `T1 ⊗ P`, with `T2` parked on `t`. All wires out-in by the same accounting. ✓
- `T2(w,x,b,r) ⊗ L`: `r→w`, `ε→x`, `ε→b`. `r`(in)-`w`(out) ✓; two fresh `ε.p`(in) to `x`(out), `b`(out) ✓.
- `T2(w,x,b,r) ⊗ S(u)`: `A[arg:=u,res:=r]` on `x`, `ε→w`, `ε→b`. `A.p`(in)-`x`(out) ✓, `A.arg`(in)-`u`(out) ✓, `A.res`(out)-`r`(in) ✓; erasers ✓.
- `T2(w,x,b,r) ⊗ F(u,v)`: `A1[arg:=u,res:=t]` on `b`, `A2[arg:=v,res:=r]` parked on `t`, `ε→w`, `ε→x`. `A1.p`(in)-`b`(out) ✓, `A1.arg`(in)-`u`(out) ✓; `t` = `A1.res`(out)-`A2.p`(in) ✓; `A2.arg`(in)-`v`(out) ✓, `A2.res`(out)-`r`(in) ✓; erasers ✓.

Sharing, constructor cases shared by `δˢ` and `δⁿ` (`reduce.rs` `DS|DN` arm):

- `δ(l,r) ⊗ L`: `l→L`, `r→L`. Each `δ.l/r` far end (`l`, `r`, in) to a fresh leaf principal (out). ✓
- `δ(l,r) ⊗ S(x)`: child `δ'` demanded on `x`, two stems `S[x:=a]` on `l`, `S[x:=b]` on `r`. `δ'.p`(in)-`x`(out) ✓; `δ'.l`(out)-`S1.x`(in) via `a` ✓, `δ'.r`(out)-`S2.x`(in) via `b` ✓; `S1.p`(out)-`l`(in) ✓, `S2.p`(out)-`r`(in) ✓.
- `δ(l,r) ⊗ F(a,b)`: two child `δ` (one per child), two forks. Each child duplicator `δa.p`(in)-`a`(out), `δb.p`(in)-`b`(out) ✓; `F1[l:=ll,r:=rl]` and `F2[l:=lr,r:=rr]` take duplicator outputs (out) into fork slots (in) ✓ and emit principals (out) to `l`, `r` (in) ✓.

Sharing, the `P` cases (the species split, `tc-net.typ` § Demand-Before-Copy):

- `δˢ(l,r) ⊗ P(f,a)`: structural copy, symmetric to the `F` case with two child `δˢ` and two `P` rebuilt on `l`, `r`. All wires out-in by the same accounting. ✓
- `δⁿ(l,r) ⊗ P(f,a)`: demand-before-copy. `A[arg:=a,res:=t]` on `f`, `δⁿ[l:=l,r:=r]` parked on `t`. `A.p`(in)-`f`(out) ✓, `A.arg`(in)-`a`(out) ✓; `t` = `A.res`(out)-`δⁿ.p`(in), the parked out-in aux wire ✓; `δⁿ.l`(out)-`l`(in) ✓, `δⁿ.r`(out)-`r`(in) ✓.

Erasure (`reduce.rs` `EPS` arm):

- `ε ⊗ L`: nothing. Vacuous. ✓
- `ε ⊗ S(x)`: `ε→x`. Fresh `ε.p`(in)-`x`(out) ✓.
- `ε ⊗ F(a,b)`, `ε ⊗ P(f,a)`: `ε→` each child. Each fresh `ε.p`(in) to a child principal (out) ✓.

Normalization (`reduce.rs` `N` arm):

- `N(res) ⊗ L`: `res→L`. `res`(in)-`L.p`(out) ✓.
- `N(res) ⊗ S(x)`: child `N'[res:=vx]` on `x`, stem `S[x:=vx]` on `res`. `N'.p`(in)-`x`(out) ✓, `N'.res`(out)-`S.x`(in) via `vx` ✓, `S.p`(out)-`res`(in) ✓.
- `N(res) ⊗ F(l,r)`: two child `N` and a rebuilt fork; duplicator-free, symmetric to the `S` case. ✓
- `N(res) ⊗ P(f,a)`: demand, `A[arg:=a,res:=t]` on `f`, `N` reused parked on `t`. `A.p`(in)-`f`(out) ✓, `A.arg`(in)-`a`(out) ✓, `t` = `A.res`(out)-`N.p`(in) ✓, `N.res`(out)-`res`(in) ✓.

Every rule right-hand side builds only out-in wires, and preserves the inductive hypothesis
for the surviving far ends (their polarity is untouched: a rule reconnects a far end but
never flips it). The lemma holds. No rule was found that fails the check; the calculus is
uniformly polarizable, which is what §2.2 flagged as load-bearing (an unpolarizable wire
would leave a forwarder chain unable to orient and would deadlock).

## 2. The forwarder ι

Add one agent: `ι`, arity 1, ports `p` (principal) and `out` (auxiliary). Orientation, forced
by the requirement that ι relays a value: `ι.p` is **in** and `ι.out` is **out**. So `ι.p`
faces the producer whose value it carries (upstream, the end the value arrives from, matching
§2.2's phrasing) and `ι.out` continues downstream. ι is, in polarity terms, an interior
segment of a wire that has been given an identity so it can occupy a cell.

One rule schema, ι against any producer root `C` (`C ∈ {L, S, F, P}`):

```
ι(out) ⊗ C(children…) : C[children…].p → out
```

The producer is rebuilt with its principal now on `ι`'s downstream end. Under hash-consing
this is a pointer move; structurally it is a splice. The rule preserves polarity (the
producer principal, out, lands on `out`, whose far end is in by the polarity lemma) and is
one rule per unordered pair, one for each of the four producer kinds.

Two consequences. **Chains collapse serially, value-side first.** A forwarder chain is a run
of ι joined out-to-principal: `ι1.out - ι2.p - ι3.p - …`, each an out-in auxiliary wire, none
active. When a producer `V` reaches the chain head `ι1.p`, the pair fires and `V` re-emerges
with principal on `ι1.out = ι2.p`; now `V.p`(out)-`ι2.p`(in) is an active pair, `ι2` fires,
and so on. One elimination per element, advancing from the producer end. **ι-ι pairs are
never active.** Any two forwarders in a chain meet out-to-principal (auxiliary to principal),
never principal-to-principal, so no ι-ι active pair ever exists; the value must arrive before
the chain moves. This is the same structural fact that keeps δˢ and δⁿ from meeting
(`tc-net.typ` § The Two Species Coexist): a consumer-like agent parked on an auxiliary wire
waits.

## 3. Embeddings

Fix a substrate dimension d (2 or 3) and a lattice of cells (a finite region of Zᵈ). A
**placement** π maps each agent of a net to a distinct cell, and each wire to a path of cells
between its endpoints that respects a fixed per-cell wire-layer capacity (crossings consume
layers; `SPATIAL_IC.md` §3). The **embedded net** E_π(N) is N with every wire of path length
ℓ replaced by an oriented ι-chain of ℓ forwarders threaded along the path (a length-0 path,
adjacent cells, is the bare wire, no ι). Orientation is well-defined by the polarity lemma:
every wire is out-in, so its chain points uniformly from the out end to the in end.

π is partial and dynamic. Partial because crossing and layer capacity make some placements
infeasible (this is where Rent's rule enters, `SPATIAL_IC.md` §4.3). Dynamic because
migration re-embeds during reduction: reel-in and drift moves relocate agents and re-thread
their chains (§5). The theorem below is stated for a placement that evolves, with the moves
treated as substrate micro-steps.

## 4. Simulation theorem

Define **readback** ρ from a substrate state to an abstract net: erase every ι (splice its in
and out wires into one) and forget the placement. ρ is total (the polarity lemma guarantees
each ι has exactly one in wire and one out wire to splice) and maps E_π(N) to N.

**Theorem (simulation).** For a run of the substrate starting from E_π(N):

1. Every reachable substrate state σ satisfies: ρ(σ) is a well-formed abstract net.
2. Every substrate micro-step σ → σ' projects under ρ to either identity (a *silent* step) or
   exactly one abstract interaction (a *real* step).
3. Consequently N →\* N' in TC-Net iff E_π(N) →\* σ with ρ(σ) = N' in TC-Net+ι, where the
   extra substrate steps are ι-eliminations and agent moves; readback is a bisimulation up to
   transport (the silent steps). Normal forms correspond: σ has no active pair iff ρ(σ) has
   none.

*Proof.* The invariant is the conjunction of (1) and (2); call it I. It holds at E_π(N) since
ρ(E_π(N)) = N is well-formed and no step has yet occurred. Preservation, by micro-step kind:

- **ι-elimination** (a producer meets a chain head, §2): removes one ι and advances the
  producer one cell. ρ erases ι regardless, so ρ(σ) = ρ(σ'). Silent. Well-formedness
  preserved (splicing one fewer ι yields the same abstract wire).
- **Agent move** (reel-in or drift, `SPATIAL_IC.md` §2.3): relocates one agent to an adjacent
  cell, shortening the chain on one wire by one elimination and lengthening each other wire by
  at most one insertion (a fresh ι spliced in). ρ erases every ι at every length, so ρ(σ) =
  ρ(σ'). Silent.
- **Wire straightening** (a kinked chain cell retracts, §2.3): re-lays a chain at equal or
  shorter length. ρ unchanged. Silent.
- **Abstract interaction**: two producer/consumer principals on adjacent cells, chain between
  them already collapsed to length 0, fire the rule of `tc-net.typ`. The rewrite is local (it
  touches only the two agents plus O(1) fresh ones, created-short lemma §6), so ρ(σ) → ρ(σ')
  is exactly that one abstract interaction. Well-formedness preserved because the abstract
  rule preserves it (the polarity lemma covers the fresh wires).

An abstract active pair can fire on the substrate only once its two principals are adjacent,
which may require first collapsing the ι-chain between them (a sequence of silent steps). Thus
each abstract interaction corresponds to zero or more silent steps followed by one real step,
which is the "up to transport" clause. Since ρ commutes with reduction modulo silent steps,
reduction sequences correspond in both directions, and a state with no active pair projects to
a net with no active pair (ι are never active, §2), so normal forms match. ∎

The invariant in clause (1)-(2) is simultaneously the theorem's induction hypothesis and the
CA simulator's runtime assertion (`SPATIAL_IC.md` §13 item 6): the proof obligation and the
differential-oracle test are the same sentence.

## 5. Cost decomposition, with drift charged

Define the **transport** of a run as the number of ι-eliminations plus the number of agent
moves. By the simulation theorem the ι-eliminations are ordinary interactions on the embedded
net, so transport is an interaction count on E_π, not a second currency (`SPATIAL_IC.md`
§2.2): one ledger.

The subtlety flagged in review is that moves *insert* ι (they lengthen some wires) as well as
eliminate them, so counting only eliminations would let migration look free. Charge each move
one unit regardless of how many ι it inserts or removes. This is well-defined and bounded by
the accounting identity. Let I(t) be the total ι count at tick t. Every micro-step changes I
by a bounded amount: an ι-elimination is −1; a rule creates O(1) fresh wire length (created-
short lemma §6), so at most O(1) insertions; a move inserts at most (agent degree − 1) ≤ 4
and eliminates 1. Summed over a run,

```
I(final) = I(initial) + insertions − eliminations ≥ 0,
```

so eliminations ≤ insertions + I(initial), and insertions ≤ (rules)·O(1) + (moves)·O(1).
Hence transport = eliminations + moves = O(interactions + moves + I(initial)), where I(initial)
= Σ initial path lengths is the cost already present in the starting embedding. The ledger
closes: transport is bounded by the two quantities the machine actually counts (interactions
and moves) plus the fixed initial layout, and no term is uncharged. In `OPTIMIZER.typ` §4
terms this is the fourth grade (bit-meters) sitting beside work, space, and span, with its
coeffect face a worst-case over admissible schedules and its effect face the measured ι+move
tally.

## 6. Created-short lemma

**Lemma.** Every TC-Net rule right-hand side wires only among the two dying agents' surviving
far ends and O(1) fresh agents; it introduces no wire between two pre-existing far ends that
were not already O(1) apart in the abstract net. Consequently a rewrite never creates a long
wire; wires lengthen only by (a) splice-fusion, where a variable elimination joins two
existing chains into one, and (b) δ copying an existing long wire, where a duplicator
reproduces a wire that was already long.

*Proof.* Inspect the same rule table as §1. Each right-hand side names its wires between a
fresh agent's port and either another fresh port or one named far end; no right-hand side
connects two named far ends to each other except by routing through a fresh agent (for
example the S rule joins `x`, `b`, `c`, `r` only through the fresh `A1`, `A2`, `A3`, `δⁿ`, and
the K rule's single `r→b` connects one result far end to one operand far end, which the
abstract net had adjacent through the dying `T1`). The count of fresh agents per rule is
bounded (at most the S rule's four applies plus one duplicator). Under a placement that puts
each fresh agent adjacent to the port it principally faces, every fresh wire is short at
birth. The only lengthening events are the two named: splice-fusion at a var cell (which fuses
two chains whose lengths were already accounted) and δ over a long wire (which copies existing
length). ∎

The consequence for tension (`SPATIAL_IC.md` §2.3): geometry degrades only through
identifiable, chargeable events, so the scheduler always has a finite, local set of wires to
contract and never faces a rewrite that teleports length into the net.

## 7. Confluence preservation

ι has exactly one principal port, and the extended rule table has exactly one rule per
unordered agent pair (the four ι-against-producer cases added to the existing functional
table). The independence property (`tc-net.typ` § Interaction Nets: each agent has one
principal, so participates in at most one active pair) is unchanged, and the rule table stays
functional. Therefore the strong-confluence argument (`tc-net.typ` Theorem 2) goes through
verbatim on TC-Net+ι: distinct active pairs commute. Any firing policy computes the same
normal form, including tension scheduling (which only chooses which forwarder eliminations and
moves to perform first, all of them silent or confluent). This is the soundness argument
`SPATIAL_IC.md` §1 claims comes for free: a distance-aware machine changes when and where, not
what.

## 8. Lower bound

Transport cannot be defined away by a cleverer embedding. The argument is Thompson's cut
bound (Thompson, AT² thesis, CMU 1980; Leighton 1992 for the graph-embedding form). For a net
whose interaction DAG (`SPATIAL_IC.md` §10.1) has bisection width b(n) (the minimum number of
dependency edges whose removal splits the work into two balanced halves), any embedding into a
2D region must route those b(n) edges across a geometric bisector; if the embedding separates
the halves by distance ℓ, the crossing edges contribute Ω(b(n)·ℓ) to total wire length, which
is transport. A concrete corollary needing no cut machinery: duplicating a size-s subnet and
separating the two copies costs Ω(s^{3/2}) transport in 2D, because s nodes occupy a region of
diameter Ω(√s) and the s duplicated wires each span it, against O(s) abstract interactions for
the copy itself. So the transport grade is a genuine second axis, tight up to the mesh's
dimension: it is Θ(interactions) only for embeddings whose traffic stays within the mesh's
O(√area) bisection capacity, which is exactly the p < 1 − 1/d Rent regime (`SPATIAL_IC.md`
§4.3). Above it, transport strictly dominates work.

What this does and does not bound: it bounds total wire length (hence energy and, on a
bandwidth-limited mesh, time) from below by the communication structure; it does not bound
the number of abstract interactions (that is the work grade, fixed by the program and
schedule-invariant), and it does not by itself fix the constant, which depends on the region
shape and the realized placement.

## 9. Status ledger

| Claim | Status | Note |
|---|---|---|
| §1 Polarity assignment is consistent (every agent) | proven | table forced by the calculus's producer/consumer reading |
| §1 Polarity lemma (every wire out-in), all rules | proven | full case analysis over the entire rule table incl. `N`; base case = the encoding |
| §1 Active pair = out-in double-principal; parked = out-in aux | proven | corollary of the lemma |
| §2 ι orientation and rule schema | proven | orientation forced by the lemma; one rule per producer kind |
| §2 Chains collapse serially; ι-ι never active | proven | structural, from the polarity of `ι.p`/`ι.out` |
| §4 Simulation, clauses 1-2 (projection invariant) | proven | per-micro-step case analysis |
| §4 Simulation, clause 3 (bisimulation, NF correspondence) | proven modulo inherited metatheory | relies on ρ commuting with reduction, established by clauses 1-2; the "up to transport" quantifier is made precise but the general bisimulation-lifting lemma is cited, not re-proved |
| §5 Cost decomposition; drift charged; ledger closes | proven | accounting identity I(final) = I(initial) + insertions − eliminations ≥ 0, with per-step bounds from §6 |
| §6 Created-short lemma | proven | case analysis over the same rule table |
| §7 Confluence preservation on TC-Net+ι | proven modulo inherited metatheory | reduces to `tc-net.typ` Theorem 2, which is cited not re-proved; the reduction (ι is unary-principal, table stays functional) is checked here |
| §8 Lower bound Ω(b(n)·ℓ); s^{3/2} corollary | sketched | Thompson/Leighton cut argument cited; the s^{3/2} corollary is self-contained; the general constant and the cut-to-wire-length step are not fully reproduced |
| Well-formedness of source encodings | assumed | standard; the encoding of a closed term is a well-formed net by construction |
| Per-cell wire-layer capacity suffices for π to exist | assumed | this is the Rent-regime question (`SPATIAL_IC.md` §4.3), deliberately left to the empirics, not claimed here |

The formal floor: sections 1, 2, 5, and 6 are self-contained proofs over the rule table.
Section 4's core (the projection invariant) is proven; its lift to a full bisimulation and
section 7's confluence both reduce, by checks carried out here, to metatheory already in
`tc-net.typ`. Section 8 is a sketch with one self-contained corollary. The two assumptions are
the standard well-formedness of encodings and the existence of a feasible placement, the
latter being precisely the measured question of `SPATIAL_IC.md` §10, not something to prove a
priori.
