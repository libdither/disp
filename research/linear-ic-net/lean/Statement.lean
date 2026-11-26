import Moves
import Std.Data.HashMap
import Std.Data.HashSet

/-! # The fixed problem statement for a proof-gated search over interaction systems

Status: v0.2 draft, not frozen.
Open question for the owner: a rule (free recursion) costs far less than the same work as a program. Options:
price rules as programs (or freeze the table); score bits over a large frozen corpus; or judge designs by hand.

An interaction system here is a set of agent kinds with polarized ports and one rewrite rule per pair of
kinds that can meet (`Net.lean`). This file fixes, once, what such a system must do and what it costs. The
job: run pure λ-terms compiled by a compiler frozen here, and supply two programs that work on terms quoted
by a quoter frozen here: `reify`, which reads a quote into the term's code as neutral first-order data
(`encode t`), and `run`, which behaves as the quoted term. The cost: a vector (description bits, moves,
rounds, record bits), the last three summed over a fixed battery of jobs run by the runner here. A `Record`
is a system with checked evidence that it does the job, of some `Strength`, and proofs that its costs are
what these functions compute; `Achieved s c` says some record of strength at least s costs at most c. The
search adds a candidate only if no record already in it `Blocks` the candidate (costs at most as much
everywhere, evidence at least as strong), so it optimises exactly what is written here. Owner-frozen once
agreed.

## What is required, and what is not
- Reading is required, and exact: `reify` on the quote of any closed term (with a value or not) answers
  exactly `encode t`, compared literally. Every other reader of code (size, equality, a metered
  interpreter, any analysis) is then a plain λ-program over that data, so no list of reader jobs is needed
  and none can be bought as a dedicated primitive. A system may buy `reify` with its own rules; then it has
  bought a real code-to-data converter, and pays for it in rule bits.
- Running is required on data answers and halting only: `normNat (run (quote t))` answers exactly the
  numeral t stands for (`natWant`), and `(λ_. 0) (run (quote t))` answers 0 when t has a value. A native
  evaluator (a kind whose rules run code, even the thaw itself) is a legitimate `run`.
- Not required: an interpreter written with readers, or any reflection beyond these two programs.
- A system with no reflective agent can enter by quoting with `encode` (`QuoteMode.encoded`): `reify` is
  then λq. q, and `run` a λ-interpreter over the data. On the machines tried so far this route fails the
  fixed move budgets (third review, a Mogensen–Scott interpreter on the plain quote-net machine: succ3
  175K moves, add 0 1 531K, mul 0 1 over the 1M test budget), so it is open in name only.

## Known limits of the job
- Only whole-code reads are measured: a cheap partial read (looking at the top node only) earns nothing.
- `run` is checked on numeral answers and on halting only; a function-valued result is never compared.
  A system that discards an argument unevaluated passes the halting check by clearing `run`'s result away
  cleanly, without running it.
- The price of a rule against the price of the same behaviour as a program is a property of the code, not
  of the job: the search will keep moving work into rules wherever the job allows (a `run` or `reify`
  kind, or rules tuned to the battery's arithmetic). The third review measured the gap: the same reader
  costs about 430 bits as rules and about 1,700 as a program, since a rule re-fires on every node for free
  while a program pays for a fixed point and deep copies. This is the open question at the top.
- `run` with a native thaw is a constant (13 or 14 bits), and its jobs re-measure the plain machine.
- `records` mostly counts annihilations: on the serializer record (third review's count), 20,440 of 26,139
  come from three rules that make no agent; a copier meeting its dual is charged as if all were lost.

## Decisions for the owner to confirm
- D1. Cost is a vector `Cost` with `Dominates` (≤ everywhere, ≠); no single number ranks. `J` (bits) is
  kept as a convenience only.
- D2. Net level only: nothing about layout, lattices or wire length.
- D3. `System` bundles kinds, signature, rules and stuck pairs. `Ok` = `Admissible` (table checks, total,
  closed) + `RunOk` + `Complete` (every rule wires every end once; the older `TableOk` does not check that a
  rule forgets no port). v0's copier/eraser tags and `Honest` are gone (see "Not proved here").
- D4. `bitsOf` = one `clog2` of the product of the alternatives of every part: signature, rule count and
  rules, gadget (six kinds and the quote mode), and both kit programs (`reify`, `run`). A shape pays for
  which copies it substitutes, not only their kinds, so it can be decoded. Programs and rule right sides
  both pay a polarized wiring, (n/2)!.
- D5. One source language `Src` (pure λ, de Bruijn) with the reference `eval` (call by value, weak, closed
  terms, fuelled: `Check.lean`'s `eval` without ρ). Compiler `emitSrc` and quoter (`emitJob` on `.quote`)
  are frozen folds over a `Gadget`: the kinds playing lam, app, var, share, erase, thaw, and the quote mode
  (a one-port wrapper kind, or `encode t` compiled by the same compiler). Frozen data: Scott numerals
  (`scott`: 0 = λz s. z, n + 1 = λz s. s n), the Mogensen–Scott code `encode` (λa b c. a ⌜i⌝ | b ⌜f⌝ ⌜x⌝ |
  c ⌜e⌝: variable, application, λ, in that order; indices as Scott numerals), and the observers `normNat`
  and `constZero`. Every answer is data and is compared with the wanted term by syntactic equality, never
  up to β (a program-built `encode t` reads back literally on both existing nets). Spec: `Meets` (every
  closed term); test: `MeetsTested n`, every closed term up to size n and the fixed family `numFamily`
  (add i j, mul i j for i, j ≤ 2), with the fuel `testFuel` and move budget `testBudget` fixed here.
- D6. `work` = moves (fires + forwarder removals); `rounds` = rounds where every pair facing another at the
  round's start fires, then every new forwarder is removed; `records` = per fired rule, `recordBits` =
  ⌈log2⌉ of how many rules leave the same right side, and ⌈log2⌉ of the number of rules for a rule that
  makes no agent (nothing is left to carry a mark). A crude, lower-bound style measure. Battery: eight
  plain jobs that compute (Scott addition and multiplication through a fixed-point combinator, three
  list/unary adds of `../nets/check.py`, two terms where copies of one value cross), `reify` on three
  terms (nested binders; a 41-node term), and `normNat ∘ run` on four numeral computations. With the v0.1 programs standing in (interpreter as
  `run`, size reader for `reify`), the battery is about 1.9M moves on the older net (mul33 through `run`
  alone 1.23M) and about 90K on the quote net; assumed: real kits stay within a few times that.
- D7. The kit is `reify` and `run` (new in v0.2). No kind is banned from a program: a native
  `run` is fine, and reading is forced by `reify`, though rules may do it (the serializer record).
  `Gadget.ok` still refuses a native quote whose wrapper is the thaw, so a quote is code still to be read,
  never code already set running.
- D8. `Strength`: `full` (the spec) above every `tested n`; among tested ones, larger n is stronger.
  `Beats R R'`: R's cost dominates R''s and R is at least as strong. `Blocks R R'`: R's cost is ≤ R''s
  everywhere and R is at least as strong; the search refuses a blocked candidate, so equal records are not
  added twice. A record of equal cost and greater strength does not push the weaker one out; both stay.
- D9. `minTestSize` = 9: both v0.1 records passed 9 in under two minutes compiled. The test up to size 9
  runs 2,640 terms: 2,622 closed terms and the 18 of `numFamily`; one (`omega`) gets only its `reify` job, and
  341 stand for a number (323 small terms and the family). Measured with the v0.1 programs standing in
  (the interpreter as `run`, the size reader in `reify`'s place), the test is about 1.5x v0.1's work on the
  older net (about 28M moves, the family 8M of them). With the v0.2 kits the size-9 test runs inside
  `lake build`: about 45 s on the older net, about 60 s on the quote net.

## Attacks so far and what answered them
- A 14-bit interpreter `λx. thaw x` (v0 review): the thaw started the quoted code, so no reading was done.
  v0.1 banned the thaw from kit programs; v0.2 allows it in `run` and forces the reading through `reify`.
- An unevaluated size answer (v0 review): a size program answered a term equal to the size only up to β.
  Answered by literal comparison with frozen data (`scott`, `encode`).
- Test-only records dominating proved ones (v0 review): `Beats` and `Blocks` need the strength too.
- Self-chosen honesty tags (v0 review): copier/eraser tags the system chose checked nothing; removed.
- Idle plain jobs (v0 review): plain battery jobs that did no work let a fast interpreter alone win the
  work column; the plain jobs now compute (about 13,500 moves on both existing nets).
- Dedicated primitives for a fixed list of kit jobs (v0.1 review, record ⟨648, 14226, 1116, 6218⟩, tested
  to 8): a 12-kind plain λ-machine plus a kind `E` whose three rules copy the thaw's (interpreter
  λq. E q, 13 bits) and a kind `Sz` whose three rules build the size numeral (size program λq. Sz q 0,
  42 bits). It beat both honest records everywhere: a rule is priced far below the same behaviour written
  as a program, so any job on a fixed list gets bought with rules. Answered by the v0.2 kit: `E` is now a
  legitimate `run`, and the only reader job is `reify`, whose answer is the whole code as neutral data.
- Records driven to 0 (v0.1 review): one dummy eraser pair added to one rule of each group with the same
  right side makes every right side unique, and the pair then annihilates by a rule with an empty right
  side, which `recordBits` charged 0, for about 3% more work. Answered: a rule that makes no agent pays
  ⌈log2⌉ of the rule count. Not closed, only narrowed: re-run on the review's 23-rule cheat
  (eight plain jobs and four `run` jobs), the dummies still cut records from 29,235 to 23,940 (−18%; was
  9,792 to 0) for +3.2% moves and +129 bits, because a dummy placed on the rarely fired member of a pair
  of rules saves a bit on every fire of both.
- One copier kind (v0.1 review): a net whose compiler shares through a single copier kind passes every
  plain term up to size 9; its first failures are at size 10, e.g. (λx. x x)(λx.λy. y y). Answered: two
  size-10 crossing terms of different shapes (`crossSelf`, `crossCapture`) are plain battery jobs.
- Duplicates (v0.1 review): `Beats` needs strict dominance, so relabelled copies of a record could be added
  without end. Answered by `Blocks`.
- A serializer (v0.2 review, `RecordSerial.lean`, ⟨762, 22682, 1775, 26139⟩, tested to 9): 10 plain λ
  kinds of the quote net plus a reader kind and a numeral printer whose 8 rules write `encode t` straight
  from frozen code; `reify` λq. R q eraO, `run` the thaw. It beats both other records everywhere. Open:
  rules priced far below programs, a constant `run`, `records` counting annihilations (the dummy trick
  still saves 18%), the `.encoded` route out of budget (see "Known limits"). Held up: exact `reify`
  (`encode` is injective, a literal readback fails safe, no cheaper bounded or lookup-table reader was
  found, every rule of the record fires in the test).

## Closed cheats
- Counting the small program and timing a fast one: the kit's two `Prog`s are both counted in `bitsOf` and
  are the ones the battery and the test run.
- Programs priced by a free number: `Prog` is net data and `Prog.alts` prices it.
- Hiding evaluation in the compiler, quoter, number encoding, observers or answer check: they are frozen
  here and answers are compared literally, so the checker does no work for a program; a system only names
  six kinds and a quote mode, priced by `gadgetAlts`. Not closed: a system's rules can run or read code,
  paid for in bits and moves (see "Known limits").
- A fixed list of reader jobs bought as primitives: there is one reader job, `reify`, and its answer is the
  whole code; a primitive that passes it is a code-to-data converter.
- Quotes that run: `reify` must answer `encode t`, which tells apart terms with the same value and is asked
  of terms with no value, so a quote cannot be evaluated before it is read.
- Cheap shapes: `annAlts`, `comAlts`, `passAlts` check the exact wiring in both orientations, and charge
  the permutation, the substituted copies and their kinds; `ruleAlts` takes the cheapest of these and the
  literal.
- Two wiring codes: rules and programs both pay a polarized matching.
- Per-term rounding: one `clog2` over the product, exact to within one bit.
- Per-system benchmark: `Battery` is fixed here, in `Src`, and the plain jobs use the frozen compiler.
- Garbage, dropped pairs, stray forwarders: `Answers` and `runJob` need a clean finish (`Clean`).
- A missing rule is free in bits (stuck pairs cost nothing) but fails any job that reaches it.
- A cost in a record that is not the computed one: `Record.bits` and `Record.runs` are equations to the
  frozen functions.
- A test chosen to pass: the test's size floor (`minTestSize`), its terms, fuel and move budget are
  constants here; the terms whose value jobs it skips are those whose reference runs out of `testFuel`, a
  fixed set (up to size 9: only (λx. x x)(λx. x x), whose `reify` job still runs).
- Records driven to 0 by dummy agents that vanish: see `recordBits`.
- A tested record pushing out a proved one, or a copy re-entering: `Beats` and `Blocks` need the strength.

## Trust
`Record.runs` and `MeetsTested` are in practice provable only by `native_decide`, which trusts the Lean
compiler and every `implemented_by`, `extern` and `unsafe` definition in the dependency closure, the
record's own definitions and instances included: a record can declare `@[implemented_by]` on its rule list
and prove `False` this way, and `#print axioms` shows the same single `native_decide` axiom either way. So
the loop must accept a record only if its file is plain data and definitions and uses none of
`implemented_by`, `extern` or `unsafe` (directly or through anything it imports beyond this statement).

## Proved here, and in `StatementSound.lean`
Here: `work_well_defined` (two finished runs of a good net take the same number of moves), `Build.ok_good`
(a loaded job's net is good, so the run theory applies), `Answers.every_order` (an answer is the end of
every finished run), `MeetsTested.mono` and `Strength.holds_of_atLeast` (a stronger test gives a weaker
one), `Beats.blocks`, `asNat_eq_some` (the numeral observer's answer is a literal numeral), `clog2_spec`.
`StatementSound.lean`: the runner makes real runs of the theory, up to renaming of agents (`runJob_run`: a
real finished run whose length is the one `work_well_defined` fixes; `runJob_answers`: its readback), and
`meetsTested_spec` / `Record.tested_spec` turn a passing test into the spec's `Answers` for every job of
every tested term, from `Ok`, `Gadget.ok` and `Kit.ok` alone. No obligations remain for the test.

## Not proved here
- That `rounds` is order-independent, minimal, or anything but this runner's count.
- That `records` bounds anything: no claim that a run with fewer record bits is reversible.
- That any system meets `Meets`. The reference takes every closed term with a weak call-by-value value;
  no proof covers all terms.
- Anything about which rules copy or erase: v0's copier/eraser tags were chosen by the system and checked
  nothing, so they are removed. A tag fixed by the table (a copier is a kind some rule of which makes two
  agents of its partner's kind) fails on the quote net, whose table-reading rules copy a slot
  (`QuoteNet.lean`'s `q_reads_copy`).
- That `bitsOf` is a prefix-free code end to end (the parts are self-delimiting by construction, but this is
  not proved), or that `clog2` of the product is within one bit of the real sum (true of `clog2`, see
  `clog2_spec`, not of the Python total).
- That `encode`, `scott` and `normNat` are the right encodings and observers.
- Anything about reflection beyond the two kit programs: `Src` has no ρ.
- That `Gadget.ok` makes the gadget's kinds behave as λ, application and so on: only arities and
  distinctness are checked; behaviour is checked by the jobs.

## Records under v0.2 (all tested to 9)
| bits term | older net (`RecordOld`) | quote net (`RecordQuote`) | serializer (`RecordSerial`) |
| signature | 71.0 | 89 | 69 |
| rule count and rules | 1170.7 (33 rules) | 972 (35) | 634 (25) |
| gadget | 26.9 | 28 | 27 |
| `reify` | 1626.8 (142 agents) | 1186 (107) | 21 (3) |
| `run` | 7.7 (1 agent, identity) | 14 (2, thaw) | 13 (2, thaw) |
| cost | ⟨2904, 54580, 2961, 81730⟩ | ⟨2288, 41221, 2388, 59658⟩ | ⟨762, 22682, 1775, 26139⟩ |

## History: the v0.1 records (kit: an interpreter and a size reader; not v0.2 records)
| bits term | older net (`RecordOld.lean`) | quote net (`RecordQuote.lean`) |
| signature | 71 | 89 |
| rule count and rules | 1170.7 | 972 |
| gadget | 26.9 | 28 |
| interpreter | 2802.2 (248 agents) | 263 (29 agents) |
| size reader | 1196.0 (109 agents) | 563 (56 agents) |
| total `bitsOf` | 5267 | 1914 |
| cost, tested to 9 | ⟨5267, 334926, 6022, 256063⟩ | ⟨1914, 34594, 2123, 15747⟩ |
Each figure is a ceiling of one term; the total is one ceiling over the product.
Under v0.2's `recordBits` the same plain jobs carry about 3x the records (the quote net's eight plain jobs:
19,258 against 6,253), since every annihilation now pays for naming its rule.
-/

namespace Statement
open End

variable {K : Type} [DecidableEq K]

/-! ## Counting -/

/-- Bits to name one of n things: the least b with n ≤ 2^b. -/
def clog2 (n : Nat) : Nat := if n ≤ 1 then 0 else Nat.log2 (n - 1) + 1

/-- Length of the Elias gamma code of n ≥ 1. -/
def gammaLen (n : Nat) : Nat := 2 * Nat.log2 n + 1

def fact : Nat → Nat
  | 0 => 1
  | n + 1 => (n + 1) * fact n

def choose (n k : Nat) : Nat := if k ≤ n then fact n / (fact k * fact (n - k)) else 0

theorem clog2_spec (n : Nat) : n ≤ 2 ^ clog2 n := by
  unfold clog2
  split
  · have := Nat.one_le_two_pow (n := 0); simp; omega
  · have := Nat.lt_log2_self (n := n - 1); omega

/-! ## Systems -/

/-- An interaction system: kinds, the polarity of each kind's ports (principal first), one rule per
pair that meets, and the pairs left without a rule on purpose. -/
structure System (K : Type) where
  kinds : List K
  sig : Sig K
  rules : List (GRule K)
  stuck : List (K × K)

/-- Number of auxiliary ports. -/
def arity (S : Sig K) (k : K) : Nat := (S.ports k).length - 1

/-- Every end a rule must wire: the pair's auxiliary ports and every port of every agent it makes. -/
def expected (S : Sig K) (r : GRule K) : List End :=
  (List.range (arity S r.a)).map (fun n => End.left (n + 1)) ++
  (List.range (arity S r.b)).map (fun n => End.right (n + 1)) ++
  (List.range r.made.length).flatMap fun j =>
    (List.range (((r.made[j]?).map (arity S)).getD 0 + 1)).map fun n => End.new j n

/-- A rule wires every end it must, each exactly once: nothing dangles. -/
def Complete (S : Sig K) (r : GRule K) : Prop :=
  (ends r.wires).length = (expected S r).length ∧ ∀ e ∈ expected S r, (ends r.wires).count e = 1

instance (S : Sig K) (r : GRule K) : Decidable (Complete S r) := by unfold Complete; infer_instance

/-- Every pair that can meet has a rule, or is a declared stuck pair. -/
def Total (sys : System K) : Prop :=
  ∀ a ∈ sys.kinds, ∀ b ∈ sys.kinds, sys.sig.pol a 0 ≠ sys.sig.pol b 0 →
    (∃ r ∈ sys.rules, (r.a = a ∧ r.b = b) ∨ (r.a = b ∧ r.b = a)) ∨ (a, b) ∈ sys.stuck ∨ (b, a) ∈ sys.stuck

instance (sys : System K) : Decidable (Total sys) := by unfold Total; infer_instance

/-- The table checks of `Net.lean`, totality, and every kind a rule names is listed. -/
structure Admissible (sys : System K) : Prop where
  table : TableOk sys.sig sys.rules
  total : Total sys
  closed : ∀ r ∈ sys.rules, r.a ∈ sys.kinds ∧ r.b ∈ sys.kinds ∧ ∀ m ∈ r.made, m ∈ sys.kinds

/-- What a system must pass before anything is run: admissible, the run checks (`RunOk`, which
gives confluence and order-independent results), and every rule complete. -/
structure Ok (sys : System K) : Prop where
  admissible : Admissible sys
  run : RunOk sys.sig sys.rules
  complete : ∀ r ∈ sys.rules, Complete sys.sig r

/-- Equality of rules, for `decide` on `Deterministic`; scoped so it never clashes with a derived one. -/
scoped instance instDecEqGRule : DecidableEq (GRule K) := fun r s =>
  if h : r.a = s.a ∧ r.b = s.b ∧ r.made = s.made ∧ r.wires = s.wires then
    isTrue (by cases r; cases s; simp_all)
  else isFalse (by intro e; subst e; exact h ⟨rfl, rfl, rfl, rfl⟩)

/-! ## Description length, as a count of alternatives -/

/-- The end a rule wires to e. -/
def other (r : GRule K) (e : End) : Option End :=
  r.wires.findSome? fun w => if w.1 == e then some w.2 else if w.2 == e then some w.1 else none

/-- Annihilate: nothing made, each auxiliary port of one joined to one of the other. One flag, and the
permutation when it is not the identity. -/
def annAlts (S : Sig K) (r : GRule K) : Option Nat :=
  if r.made.isEmpty && arity S r.a == arity S r.b &&
      r.wires.all (fun w => match w with | (.left _, .right _) | (.right _, .left _) => true | _ => false) then
    let ident := r.wires.all fun w => match w with
      | (.left i, .right j) | (.right j, .left i) => i == j
      | _ => false
    some (if ident then 2 else 2 * fact (arity S r.a))
  else none

def newZero : Option End → Option Nat
  | some (.new j 0) => some j
  | _ => none

def allSome {α : Type} : List (Option α) → Option (List α)
  | [] => some []
  | some x :: xs => (allSome xs).map (x :: ·)
  | none :: _ => none

/-- Commute with A on the left: a copy of B on each auxiliary port of A, a copy of A on each of B,
wired as a grid. A copy may be another kind (an oriented copier). One flag; when some copies are
substituted, how many, which, and their kinds. -/
def comAt (S : Sig K) (k : Nat) (r : GRule K) (A B : K) (sa sb : Nat → End) : Option Nat :=
  let p := arity S A
  let q := arity S B
  if r.made.length ≠ p + q then none else
  match allSome ((List.range p).map fun i => newZero (other r (sa (i + 1)))),
        allSome ((List.range q).map fun j => newZero (other r (sb (j + 1)))) with
  | some cbi, some cai =>
    let ar := fun j => (r.made[j]?).map (arity S)
    let grid := (List.range p).all fun i => (List.range q).all fun j =>
      other r (.new (cai.getD j 0) (i + 1)) == some (.new (cbi.getD i 0) (j + 1))
    if decide (cbi ++ cai).Nodup && cbi.all (ar · == some q) && cai.all (ar · == some p) && grid then
      let subs := (cbi.filter fun j => r.made[j]? != some B).length +
        (cai.filter fun j => r.made[j]? != some A).length
      some (if subs = 0 then 2 else 2 * (p + q) * choose (p + q) subs * k ^ subs)
    else none
  | _, _ => none

def comAlts (S : Sig K) (k : Nat) (r : GRule K) : Option Nat :=
  comAt S k r r.a r.b .left .right <|> comAt S k r r.b r.a .right .left

/-- Pass on, with A on the left: a one-port A meets B, and B is made again with its principal port where
A's port led. One flag, and the kind when the remade agent is another kind. -/
def passAt (S : Sig K) (k : Nat) (r : GRule K) (A B : K) (sa sb : Nat → End) : Option Nat :=
  if arity S A = 1 ∧ r.made.length = 1 ∧ (r.made[0]?).map (arity S) = some (arity S B) then
    let want := (End.new 0 0, sa 1) :: (List.range (arity S B)).map fun n => (End.new 0 (n + 1), sb (n + 1))
    let has := fun (w : End × End) => r.wires.any fun x => x == w || x == (w.2, w.1)
    if r.wires.length = want.length ∧ want.all has = true then
      some (if r.made[0]? = some B then 2 else 2 * k)
    else none
  else none

def passAlts (S : Sig K) (k : Nat) (r : GRule K) : Option Nat :=
  passAt S k r r.a r.b .left .right <|> passAt S k r r.b r.a .right .left

/-- Written out: how many agents, their kinds, and the wiring as a bijection of out ends to in ends. -/
def literalAlts (S : Sig K) (k : Nat) (r : GRule K) : Nat :=
  let ends := arity S r.a + arity S r.b + (r.made.map fun m => arity S m + 1).sum
  2 ^ gammaLen (r.made.length + 1) * k ^ r.made.length * fact (ends / 2)

/-- A rule: the pair, a two-bit tag for its code, and the cheapest code that fits it. -/
def ruleAlts (S : Sig K) (k : Nat) (r : GRule K) : Nat :=
  let shapes := [annAlts S r, comAlts S k r, passAlts S k r].filterMap id
  k * k * 4 * shapes.foldl min (literalAlts S k r)

/-- The signature: how many kinds, a flag, and per kind its arity and the direction of each port. -/
def sigAlts (sys : System K) : Nat :=
  2 ^ (gammaLen sys.kinds.length + 1) *
    (sys.kinds.map fun k => 2 ^ (gammaLen (arity sys.sig k + 1) + (arity sys.sig k + 1))).foldl (· * ·) 1

/-- The table: how many rules, then each rule. -/
def tableAlts (sys : System K) : Nat :=
  2 ^ gammaLen (sys.rules.length + 1) *
    (sys.rules.map (ruleAlts sys.sig sys.kinds.length)).foldl (· * ·) 1

/-! ## The common source language -/

/-- Pure untyped λ-terms, de Bruijn indices. -/
inductive Src
  | var (i : Nat)
  | app (f a : Src)
  | lam (b : Src)
  deriving DecidableEq, Repr, Inhabited

namespace Src
def size : Src → Nat
  | var _ => 1
  | app f a => 1 + f.size + a.size
  | lam b => 1 + b.size

def closedAt : Nat → Src → Bool
  | d, var i => i < d
  | d, app f a => closedAt d f && closedAt d a
  | d, lam b => closedAt (d + 1) b

def closed (t : Src) : Bool := closedAt 0 t

/-- Replace variable k by the closed value v (no shifting is needed: v is closed). -/
def substV (v : Src) : Src → Nat → Src
  | var i, k => if i = k then v else var i
  | app f a, k => app (substV v f k) (substV v a k)
  | lam b, k => lam (substV v b (k + 1))

end Src

open Src in
/-- The reference evaluator: call by value, never under a binder, fuelled; `none` when the fuel runs
out (or on an open term). The fragment of `Check.lean`'s `eval` without ρ and opened functions, with
captured values written in place. -/
def eval : Nat → Src → Option Src
  | 0, _ => none
  | _ + 1, lam b => some (lam b)
  | _ + 1, var _ => none
  | n + 1, app f a => do
    let fv ← eval n f
    let av ← eval n a
    match fv with
    | lam b => eval n (substV av b 0)
    | _ => none

/-- The Scott numeral n: zero is λz s. z, and n + 1 is λz s. s n. A value, so an answer that is a
number is a finished piece of data. -/
def scott : Nat → Src
  | 0 => .lam (.lam (.var 1))
  | n + 1 => .lam (.lam (.app (.var 0) (scott n)))

/-- The number a Scott numeral writes, read literally; `none` for anything else. -/
def asNat : Src → Option Nat
  | .lam (.lam (.var 1)) => some 0
  | .lam (.lam (.app (.var 0) n)) => (asNat n).map (· + 1)
  | _ => none

/-- Mogensen–Scott code of a term: λa b c. a ⌜i⌝ | b ⌜f⌝ ⌜x⌝ | c ⌜e⌝ (a variable, an application, a
λ, in that order), indices as Scott numerals. First-order data: a value made of λs and applications of
bound variables only, so a program can build it and the runner reads it back literally. -/
def encode : Src → Src
  | .var i => .lam (.lam (.lam (.app (.var 2) (scott i))))
  | .app f a => .lam (.lam (.lam (.app (.app (.var 1) (encode f)) (encode a))))
  | .lam b => .lam (.lam (.lam (.app (.var 0) (encode b))))

namespace Battery
/-- λn z s. s n, the Scott successor. -/
def succ : Src := .lam (.lam (.lam (.app (.var 0) (.var 2))))
/-- The call-by-value fixed point λf. (λx. f (λy. x x y)) (λx. f (λy. x x y)). -/
def fix : Src :=
  let w : Src := .lam (.app (.var 1) (.lam (.app (.app (.var 1) (.var 1)) (.var 0))))
  .lam (.app w w)
end Battery

/-- The frozen numeral observer fix (λself n. n 0 (λp. succ (self p))): rebuilds a literal Scott numeral
from anything that behaves like one, so a number is compared literally even when a `run` returns its
own representation of it. -/
def normNat : Src :=
  .app Battery.fix (.lam (.lam (.app (.app (.var 0) (scott 0)) (.lam (.app Battery.succ (.app (.var 2) (.var 0)))))))

/-- The frozen halting observer λ_. 0: answers 0 once its argument is a value and is cleared away. -/
def constZero : Src := .lam (scott 0)

/-- The number a term stands for: the reference's value of `normNat t`, when that is a Scott numeral. -/
def natWant (fuel : Nat) (t : Src) : Option Nat := (eval fuel (.app normNat t)).bind asNat

/-- Every term of a given size whose free variables are below depth. -/
def terms : Nat → Nat → List Src
  | 0, _ => []
  | 1, d => (List.range d).map .var
  | n + 2, d => (terms (n + 1) (d + 1)).map .lam ++
      (List.range n).attach.flatMap fun ⟨k, hk⟩ =>
        have : k < n := List.mem_range.mp hk
        (terms (k + 1) d).flatMap fun f => (terms (n - k) d).map (.app f)
termination_by n => n

/-- Every closed term of size at most n. -/
def upTo (n : Nat) : List Src := (List.range n).flatMap fun s => terms (s + 1) 0

/-- (λx. x x)(λx. x x), the one closed term up to size 9 without a value. -/
def omega : Src := .app (.lam (.app (.var 0) (.var 0))) (.lam (.app (.var 0) (.var 0)))


/-! ## Gadgets, programs, and the frozen compiler and quoter -/

/-- How code is handed to a program: wrapped in one agent of a given kind (the wrapper's one auxiliary
port leads to the code), or as the Mogensen–Scott code of the term, compiled like any term. -/
inductive QuoteMode (K : Type)
  | native (wrap : K)
  | encoded

/-- The kinds that play each part of a λ-term. Ports are fixed: lam (p; x, b), app (p; f, a),
var (p; s), share (p; a, b), erase (p), thaw (p; o). -/
structure Gadget (K : Type) where
  lam : K
  app : K
  var : K
  share : K
  erase : K
  thaw : K
  quote : QuoteMode K

def Gadget.parts (g : Gadget K) : List K := [g.lam, g.app, g.var, g.share, g.erase, g.thaw]

/-- The gadget names six different listed kinds of the right arities; a quote's wrapper is a listed
one-port kind other than the thaw, so a quote is code still to be read, never code already set running. -/
def Gadget.ok (sys : System K) (g : Gadget K) : Bool :=
  let ar := arity sys.sig
  g.parts.all (· ∈ sys.kinds) && decide g.parts.Nodup &&
  ar g.lam == 2 && ar g.app == 2 && ar g.var == 1 && ar g.share == 2 && ar g.erase == 0 && ar g.thaw == 1 &&
  match g.quote with
  | .native w => w ∈ sys.kinds && ar w == 1 && w != g.thaw
  | .encoded => true

def gadgetAlts (sys : System K) (g : Gadget K) : Nat :=
  let k := sys.kinds.length
  k ^ 6 * match g.quote with
    | .native _ => 2 * k
    | .encoded => 2

/-- An end of a program's wiring: the one end that faces out (where its value is taken), or port n of
its j-th agent. -/
inductive PEnd
  | out
  | port (j n : Nat)
  deriving DecidableEq, Repr

/-- The order `Prog.expected` lists ends in: the out end, then by agent, then by port. -/
def PEnd.le : PEnd → PEnd → Bool
  | .out, _ => true
  | .port _ _, .out => false
  | .port j n, .port j' n' => j < j' || (j == j' && n ≤ n')

/-- Merge two lists; fuel bounds the steps, and with too little the rest is appended unsorted. -/
def mergeBy {α : Type} (le : α → α → Bool) : Nat → List α → List α → List α
  | 0, xs, ys => xs ++ ys
  | _ + 1, [], ys => ys
  | _ + 1, xs, [] => xs
  | f + 1, x :: xs, y :: ys =>
    if le x y then x :: mergeBy le f xs (y :: ys) else y :: mergeBy le f (x :: xs) ys

/-- Merge sort by structural fuel, so the kernel can run it; whatever the fuel, the result is a
permutation of the input. -/
def sortBy {α : Type} (le : α → α → Bool) : Nat → List α → List α
  | 0, xs => xs
  | f + 1, xs =>
    if xs.length ≤ 1 then xs
    else mergeBy le xs.length (sortBy le f (xs.take (xs.length / 2))) (sortBy le f (xs.drop (xs.length / 2)))

/-- A program: a finite net of the system with one free end. -/
structure Prog (K : Type) where
  agents : List K
  wires : List (PEnd × PEnd)

namespace Prog
/-- Every end of the program, in `PEnd.le` order. -/
def expected (S : Sig K) (P : Prog K) : List PEnd :=
  .out :: P.agents.zipIdx.flatMap fun (k, j) => (List.range (arity S k + 1)).map (PEnd.port j ·)

def endList (P : Prog K) : List PEnd := P.wires.flatMap fun w => [w.1, w.2]

/-- The out end is where the outside takes the value, so it counts as an in end. -/
def pol (S : Sig K) (P : Prog K) : PEnd → Pol
  | .out => .i
  | .port j n => match P.agents[j]? with
    | some k => S.pol k n
    | none => .i

/-- Listed kinds, every end wired exactly once (the wired ends, sorted, are exactly the ends), every
wire out to in. -/
def ok (sys : System K) (P : Prog K) : Bool :=
  P.agents.all (· ∈ sys.kinds) &&
  sortBy PEnd.le P.endList.length P.endList == P.expected sys.sig &&
  P.wires.all fun w => P.pol sys.sig w.1 != P.pol sys.sig w.2

/-- Ends, the out end included. -/
def numEnds (S : Sig K) (P : Prog K) : Nat := (P.agents.map fun k => arity S k + 1).sum + 1

/-- How many agents, their kinds, and the wiring as a bijection of out ends to in ends: the same
wiring code as a written-out rule. -/
def alts (sys : System K) (P : Prog K) : Nat :=
  2 ^ gammaLen (P.agents.length + 1) * sys.kinds.length ^ P.agents.length * fact (P.numEnds sys.sig / 2)
end Prog

/-- The two programs every system must supply, each applied to a quote: `reify` turns it into the
term's `encode` (reading code into neutral data), `run` behaves as the term (running code). -/
structure Kit (K : Type) where
  reify : Prog K
  run : Prog K

/-- Both programs are nets of the system, of any kinds, the thaw included; all `loadOk_of_ok` needs. -/
def Kit.ok (sys : System K) (kit : Kit K) : Bool :=
  kit.reify.ok sys && kit.run.ok sys

/-- Everything a record is charged for, as one number of alternatives. -/
def recordAlts (sys : System K) (g : Gadget K) (kit : Kit K) : Nat :=
  sigAlts sys * tableAlts sys * gadgetAlts sys g * kit.reify.alts sys * kit.run.alts sys

/-- Description length in bits: one ceiling of log2 over the whole record. -/
def bitsOf (sys : System K) (g : Gadget K) (kit : Kit K) : Nat := clog2 (recordAlts sys g kit)

/-! ### Building nets -/

/-- A net as finite lists: agents with root names, and wires. -/
structure Build (K : Type) where
  kinds : List (Ident × Ag K) := []
  wires : List (Port × Port) := []
  next : Nat := 0

def Build.add (b : Build K) (k : Ag K) : Build K × Ident :=
  let x := Ident.root b.next
  ({ b with kinds := (x, k) :: b.kinds, next := b.next + 1 }, x)

def Build.link (b : Build K) (p q : Port) : Build K := { b with wires := (p, q) :: b.wires }

def lookup (ws : List (Port × Port)) (p : Port) : Option Port :=
  match ws.find? (fun w => w.1 == p) with
  | some w => some w.2
  | none => (ws.find? (fun w => w.2 == p)).map (·.1)

def Build.net (b : Build K) : Net K := ⟨fun x => (b.kinds.find? (·.1 == x)).map (·.2), lookup b.wires⟩

/-- Every agent has a root name, and every wire is symmetric, touches agents that are there, and
joins an out port to an in port. -/
def Build.ok (S : Sig K) (b : Build K) : Bool :=
  b.kinds.all (fun e => match e.1 with | .root _ => true | .made .. => false) &&
  b.wires.all fun w =>
    lookup b.wires w.1 == some w.2 && lookup b.wires w.2 == some w.1 &&
    (b.net.kind w.1.1).isSome && (b.net.kind w.2.1).isSome && b.net.pol S w.1 != b.net.pol S w.2

def modAt {α : Type} (f : α → α) : List α → Nat → List α
  | [], _ => []
  | x :: xs, 0 => f x :: xs
  | x :: xs, n + 1 => x :: modAt f xs n

/-- Wire a binder's port to its uses: an eraser for none, a chain of sharers for several. -/
def bindUses (g : Gadget K) (b : Build K) (port : Port) : List Port → Build K
  | [] => let (b, e) := b.add (.base g.erase); b.link port (e, 0)
  | [u] => b.link port u
  | u :: us =>
    let (b, s) := b.add (.base g.share)
    bindUses g ((b.link port (s, 0)).link (s, 1) u) (s, 2) us

/-- The frozen compiler: code for a term; env collects each enclosing binder's uses. -/
def emitSrc (g : Gadget K) : Src → Build K → List (List Port) → Build K × Port × List (List Port)
  | .var i, b, env =>
    let (b, v) := b.add (.base g.var)
    (b, (v, 0), modAt (fun us => us ++ [(v, 1)]) env i)
  | .app f a, b, env =>
    let (b, x) := b.add (.base g.app)
    let (b, pf, env) := emitSrc g f b env
    let (b, pa, env) := emitSrc g a b env
    ((b.link (x, 1) pf).link (x, 2) pa, (x, 0), env)
  | .lam body, b, env =>
    let (b, l) := b.add (.base g.lam)
    let (b, pb, env') := emitSrc g body b ([] :: env)
    (bindUses g (b.link (l, 2) pb) (l, 1) (env'.headD []), (l, 0), env'.tail)

/-- Copy a program in, fresh names; the port its out end was wired to. -/
def addProg (b : Build K) (P : Prog K) : Build K × Port :=
  let base := b.next
  let b := P.agents.foldl (fun b k => (b.add (.base k)).1) b
  let port : PEnd → Port := fun
    | .out => (Ident.root base, 0)
    | .port j n => (Ident.root (base + j), n)
  let outP := (P.wires.findSome? fun w => match w with
    | (.out, e) | (e, .out) => some (port e)
    | _ => none).getD (Ident.root base, 0)
  let b := P.wires.foldl (fun b w => match w with
    | (.out, _) | (_, .out) => b
    | (e, f) => b.link (port e) (port f)) b
  (b, outP)

/-- What a job runs: a closed term, an application, a supplied program, or a quoted term. -/
inductive JobTm (K : Type)
  | src (t : Src)
  | app (f a : JobTm K)
  | prog (P : Prog K)
  | quote (t : Src)

/-- The frozen quoter is the last case: the wrapper agent over the term's code, or the code of its
Mogensen–Scott encoding. -/
def emitJob (g : Gadget K) : JobTm K → Build K → Build K × Port
  | .src t, b => let (b, p, _) := emitSrc g t b []; (b, p)
  | .app f a, b =>
    let (b, x) := b.add (.base g.app)
    let (b, pf) := emitJob g f b
    let (b, pa) := emitJob g a b
    ((b.link (x, 1) pf).link (x, 2) pa, (x, 0))
  | .prog P, b => addProg b P
  | .quote t, b => match g.quote with
    | .native w =>
      let (b, q) := b.add (.base w)
      let (b, p, _) := emitSrc g t b []
      (b.link (q, 1) p, (q, 0))
    | .encoded => let (b, p, _) := emitSrc g (encode t) b []; (b, p)

/-- A job under a thaw, its answer arriving at port 0 of a root forwarder. -/
def load (g : Gadget K) (j : JobTm K) : Build K × Ident :=
  let (b, p) := emitJob g j {}
  let (b, th) := b.add (.base g.thaw)
  let (b, root) := b.add (.fwd .i)
  ((b.link (th, 0) p).link (th, 1) (root, 0), root)

/-- The compiled term itself. -/
def plainJob (t : Src) : JobTm K := .src t
/-- `reify` on the quote; its answer must be `encode t`. -/
def reifyJob (kit : Kit K) (t : Src) : JobTm K := .app (.prog kit.reify) (.quote t)
/-- `run` on the quote; never answered on its own, only through an observer below. -/
def runQuote (kit : Kit K) (t : Src) : JobTm K := .app (.prog kit.run) (.quote t)
/-- `normNat (run (quote t))`: its answer must be the numeral t stands for. -/
def natJob (kit : Kit K) (t : Src) : JobTm K := .app (.src normNat) (runQuote kit t)
/-- `(λ_. 0) (run (quote t))`: its answer must be 0 when t has a value. -/
def haltJob (kit : Kit K) (t : Src) : JobTm K := .app (.src constZero) (runQuote kit t)

/-! ### Reading the answer back -/

/-- Follow a variable's wire back through sharers and forwarders to its binder or captured value. -/
def walk (g : Gadget K) : Nat → Net K → Port → Option Port
  | 0, _, _ => none
  | n + 1, N, s =>
    match N.kind s.1 with
    | some (.base k) => if k = g.share ∧ (s.2 = 1 ∨ s.2 = 2) then (N.mate (s.1, 0)).bind (walk g n N) else some s
    | some (.fwd _) => (N.mate (s.1, if s.2 = 0 then 1 else 0)).bind (walk g n N)
    | none => some s

/-- The term at a port; a captured value is read in place (it is closed). -/
def readback (g : Gadget K) : Nat → Net K → List Port → Port → Option Src
  | 0, _, _, _ => none
  | n + 1, N, bs, p =>
    match N.kind p.1 with
    | some (.fwd _) => (N.mate (p.1, if p.2 = 0 then 1 else 0)).bind (readback g n N bs)
    | some (.base k) =>
      if p.2 ≠ 0 then none
      else if k = g.lam then ((N.mate (p.1, 2)).bind (readback g n N ((p.1, 1) :: bs))).map .lam
      else if k = g.app then do
        let f ← (N.mate (p.1, 1)).bind (readback g n N bs)
        let a ← (N.mate (p.1, 2)).bind (readback g n N bs)
        pure (.app f a)
      else if k = g.var then do
        let src ← (N.mate (p.1, 1)).bind (walk g n N)
        match bs.findIdx? (· == src) with
        | some i => pure (.var i)
        | none => if src.2 = 0 then readback g n N [] src else none
      else none
    | none => none

/-! ## The job, as a property of runs -/

/-- x is joined to y by a path of wires. -/
inductive Linked (N : Net K) (x : Ident) : Ident → Prop
  | refl : Linked N x x
  | step {y z : Ident} {n m : Nat} : Linked N x y → N.mate (y, n) = some (z, m) → Linked N x z

/-- No two agents face each other (not even a pair without a rule), and every agent hangs off the
root: a finished run left no garbage and dropped nothing on a stuck pair. -/
def Clean (F : Net K) (root : Ident) : Prop :=
  (∀ x y (k k' : K), F.kind x = some (.base k) → F.kind y = some (.base k') → F.mate (x, 0) ≠ some (y, 0)) ∧
  ∀ x, F.kind x ≠ none → Linked F root x

/-- A job answers with a value meeting `want`: its net passes `Build.ok`, and some run (firing pairs
and removing forwarders, keeping the root) finishes clean with that value read back at the root. -/
def Answers (sys : System K) (g : Gadget K) (j : JobTm K) (want : Src → Prop) : Prop :=
  (load g j).1.ok sys.sig = true ∧
  ∃ F, Moves sys.sig sys.rules (· = (load g j).2) (load g j).1.net F ∧
    Finished sys.sig sys.rules (· = (load g j).2) F ∧ Clean F (load g j).2 ∧
    ∃ n v, (F.mate ((load g j).2, 0)).bind (readback g n F []) = some v ∧ want v

/-- **The spec.** For every closed term: `reify` on its quote answers exactly `encode t`; if the
reference gives it a value, the compiled term answers exactly that value and `run` on its quote halts
(the constant observer answers 0); and if it stands for a number (`natWant`), `normNat` of `run` on its
quote answers exactly that numeral. -/
def Meets (sys : System K) (g : Gadget K) (kit : Kit K) : Prop :=
  ∀ t : Src, t.closed = true →
    Answers sys g (reifyJob kit t) (· = encode t) ∧
    (∀ fuel v, eval fuel t = some v →
      Answers sys g (plainJob t) (· = v) ∧ Answers sys g (haltJob kit t) (· = scott 0)) ∧
    (∀ fuel k, natWant fuel t = some k → Answers sys g (natJob kit t) (· = scott k))

/-! ## The executable runner

Hash maps; only the ports a step touches are recomputed, each with the very `fire` (or `unfwd`) the
theorems are about, and made agents get short root names. It runs in rounds: every pair facing each
other when a round starts fires in that round, then every forwarder but the root is removed. -/

/-- Hash of a name; local, so it never clashes with a derived instance elsewhere. -/
def hashIdent : Ident → UInt64
  | .root n => hash n
  | .made a b k => mixHash (mixHash (hashIdent a) (hashIdent b)) (hash k)

local instance identHashable : Hashable Ident := ⟨hashIdent⟩

def agArity (S : Sig K) : Ag K → Nat
  | .base k => arity S k
  | .fwd _ => 1

structure Eng (K : Type) where
  kinds : Std.HashMap Ident (Ag K)
  mates : Std.HashMap Port Port
  fresh : Nat

namespace Eng
def ofBuild (b : Build K) : Eng K :=
  { kinds := Std.HashMap.ofList b.kinds,
    mates := b.wires.foldl (fun m (p, q) => (m.insert p q).insert q p) {},
    fresh := b.next }

def view (e : Eng K) : Net K := ⟨fun x => e.kinds[x]?, fun p => e.mates[p]?⟩

def portsOf (S : Sig K) (N : Net K) (x : Ident) : List Port :=
  (List.range ((((N.kind x).map (agArity S)).getD 0) + 1)).map fun i => (x, i)

/-- The rule for a pair of kinds, either way round, and its place in the table. -/
def findRule (x y : K) : List (GRule K) → Nat → Option (Nat × GRule K)
  | [], _ => none
  | r :: rs, i => if (r.a = x ∧ r.b = y) ∨ (r.a = y ∧ r.b = x) then some (i, r) else findRule x y rs (i + 1)

/-- The pair x is in, oriented the rule's way, if x faces an agent the table has a rule for. -/
def activeAt (T : List (GRule K)) (e : Eng K) (x : Ident) : Option (Ident × Ident × GRule K × Nat) :=
  match e.kinds[x]?, e.mates[(x, 0)]? with
  | some (.base kx), some (y, 0) =>
    match (e.kinds[y]? : Option (Ag K)) with
    | some (.base ky) => (findRule kx ky T 0).map fun (i, r) => if r.a = kx then (x, y, r, i) else (y, x, r, i)
    | _ => none
  | _, _ => none

/-- Fire one pair: the new agents, and the old ones whose wires changed. The state is taken apart
first and everything is read from it before the maps change, so they are updated in place. -/
def fire (S : Sig K) (e : Eng K) (a b : Ident) (r : GRule K) : Eng K × List Ident :=
  match e with
  | ⟨kinds, mates, fresh⟩ =>
  let N : Net K := ⟨fun x => kinds[x]?, fun p => mates[p]?⟩
  let gone := portsOf S N a ++ portsOf S N b
  let near := ((gone.filterMap N.mate).map (·.1)).filter fun z => z != a && z != b
  let made := (List.range (r.made.length + r.wires.length)).filterMap fun k =>
    (madeKind S r k).map fun g => (k, g)
  let short := fun (k : Nat) => Ident.root (fresh + k)
  let ren : Port → Port := fun p => match madeIdx a b p.1 with
    | some k => (short k, p.2)
    | none => p
  let M := _root_.fire S r a b N
  let upsNear : List (Port × Option Port) := near.flatMap fun z => (portsOf S N z).filterMap fun p =>
    match N.mate p with
    | some t => if t.1 == a || t.1 == b then some (p, (M.mate p).map ren) else none
    | none => none
  let upsMade : List (Port × Port) := made.flatMap fun (k, g) =>
    (List.range (agArity S g + 1)).filterMap fun i => (M.mate (Ident.made a b k, i)).map fun u => ((short k, i), ren u)
  let mates := gone.foldl (fun m p => m.erase p) mates
  let mates := upsNear.foldl (fun m (p, u) => match u with | some v => m.insert p v | none => m.erase p) mates
  let mates := upsMade.foldl (fun m (p, v) => m.insert p v) mates
  let kinds := made.foldl (fun m (k, g) => m.insert (short k) g) ((kinds.erase a).erase b)
  (⟨kinds, mates, fresh + r.made.length + r.wires.length⟩, made.map (fun (k, _) => short k) ++ near)

/-- Remove forwarder x: the old agents whose wires changed. -/
def cut (S : Sig K) (e : Eng K) (x : Ident) : Eng K × List Ident :=
  match e with
  | ⟨kinds, mates, fresh⟩ =>
  let N : Net K := ⟨fun x => kinds[x]?, fun p => mates[p]?⟩
  let gone := portsOf S N x
  let nbrs := (gone.filterMap N.mate).filter fun p => p.1 != x
  let M := unfwd x N
  let ups : List (Port × Option Port) := nbrs.map fun p => (p, M.mate p)
  let mates := gone.foldl (fun m p => m.erase p) mates
  let mates := ups.foldl (fun m (p, u) => match u with | some v => m.insert p v | none => m.erase p) mates
  (⟨kinds.erase x, mates, fresh⟩, nbrs.map (·.1))

def isFwd (e : Eng K) (root x : Ident) : Bool :=
  x != root && match e.kinds[x]? with | some (.fwd _) => true | _ => false
end Eng

/-- What a run did: pairs fired, forwarders removed, rounds, and fires per rule of the table. -/
structure Tally where
  fires : Nat := 0
  cuts : Nat := 0
  rounds : Nat := 0
  perRule : Array Nat
  deriving Repr

namespace Eng
/-- Rounds until no pair is left to fire; `none` past `budget` moves. -/
def rounds (S : Sig K) (T : List (GRule K)) (root : Ident) (budget : Nat) :
    Nat → Eng K → List Ident → Tally → Option (Eng K × Tally)
  | 0, _, _, _ => none
  | n + 1, e, cands, t =>
    let pairs := cands.filterMap (activeAt T e)
    if pairs.isEmpty then some (e, t) else
    let (e, touched, t) := pairs.foldl (fun (e, acc, t) (a, b, r, i) =>
      if (e.kinds[a]?).isSome && (e.kinds[b]?).isSome then
        let (e, ch) := e.fire S a b r
        (e, ch ++ acc, { t with fires := t.fires + 1, perRule := t.perRule.modify i (· + 1) })
      else (e, acc, t)) (e, [], t)
    let (e, touched, t) := (touched.filter (e.isFwd root)).foldl (fun (e, acc, t) x =>
      if e.isFwd root x then
        let (e, ch) := e.cut S x
        (e, ch ++ acc, { t with cuts := t.cuts + 1 })
      else (e, acc, t)) (e, touched, t)
    if t.fires + t.cuts > budget then none
    else rounds S T root budget n e touched { t with rounds := t.rounds + 1 }

/-- The agents joined to x by wires. -/
def reach (S : Sig K) (e : Eng K) (x : Ident) : Std.HashSet Ident :=
  go (e.mates.size + 2) [x] {}
where
  go : Nat → List Ident → Std.HashSet Ident → Std.HashSet Ident
    | 0, _, seen => seen
    | _ + 1, [], seen => seen
    | n + 1, y :: todo, seen =>
      if seen.contains y then go n todo seen
      else go n (((portsOf S e.view y).filterMap (e.mates[·]?)).map (·.1) ++ todo) (seen.insert y)

/-- `Clean`, decided: no two agents face each other, no forwarder but the root, nothing unreachable. -/
def clean (S : Sig K) (e : Eng K) (root : Ident) : Bool :=
  e.kinds.toList.all (fun (x, g) => match g with
    | .fwd _ => x == root
    | .base _ => match e.mates[(x, 0)]? with
      | some (y, 0) => match (e.kinds[y]? : Option (Ag K)) with
        | some (.base _) => false
        | _ => true
      | _ => true) &&
  (reach S e root).size == e.kinds.size
end Eng

/-- Fuel for reading an answer back. -/
def readFuel : Nat := 100000

/-- Run a job to the end: its answer and tally, or `none` if its net fails `Build.ok`, it does not
finish within `budget` moves, or it does not finish clean. -/
def runJob (sys : System K) (g : Gadget K) (budget : Nat) (j : JobTm K) : Option (Src × Tally) :=
  let (b, root) := load g j
  if b.ok sys.sig = false then none else
  match Eng.rounds sys.sig sys.rules root budget (budget + 1) (Eng.ofBuild b) (b.kinds.map (·.1))
      { perRule := Array.replicate sys.rules.length 0 } with
  | none => none
  | some (e, t) =>
    if e.clean sys.sig root then ((e.view.mate (root, 0)).bind (readback g readFuel e.view [])).map (·, t)
    else none

/-! ## The battery: fixed jobs, the same for every system -/

namespace Battery
/-- fix (λadd m n. m n (λp. succ (add p n))). -/
def add : Src :=
  .app fix (.lam (.lam (.lam (.app (.app (.var 1) (.var 0))
    (.lam (.app succ (.app (.app (.var 3) (.var 0)) (.var 1))))))))
/-- fix (λmul m n. m 0 (λp. add n (mul p n))). -/
def mul : Src :=
  .app fix (.lam (.lam (.lam (.app (.app (.var 1) (scott 0))
    (.lam (.app (.app add (.var 1)) (.app (.app (.var 3) (.var 0)) (.var 1))))))))
/-- λx y z. x z (y z): several binders, one variable used twice. -/
def sComb : Src := .lam (.lam (.lam (.app (.app (.var 2) (.var 0)) (.app (.var 1) (.var 0)))))
def add33 : Src := .app (.app add (scott 3)) (scott 3)
def mul33 : Src := .app (.app mul (scott 3)) (scott 3)
def mul44 : Src := .app (.app mul (scott 4)) (scott 4)
/-- succ (succ (succ 0)): three applications that reduce. -/
def succ3 : Src := .app succ (.app succ (.app succ (scott 0)))
/-- add (succ (succ 0)) (succ 0): a recursion through the fixed point, on numerals still to compute. -/
def add21 : Src := .app (.app add (.app succ (.app succ (scott 0)))) (.app succ (scott 0))
/-- `linear_unary_add 2u 3u` of `../nets/check.py`. -/
def linearUnaryAdd : Src :=
  (.app (.app (.lam (.lam (.app (.app (.app (.var 1) (.lam (.lam (.app (.var 1) (.app (.lam (.lam (.lam (.app (.var 1) (.app (.app (.var 2) (.var 1)) (.var 0)))))) (.var 0)))))) (.lam (.lam (.lam (.app (.app (.var 0) (.var 1)) (.var 2)))))) (.var 0)))) (.lam (.lam (.app (.var 1) (.app (.app (.lam (.lam (.app (.var 1) (.app (.app (.lam (.lam (.app (.var 0) (.var 1)))) (.var 1)) (.var 0))))) (.var 1)) (.var 0)))))) (.lam (.lam (.app (.var 1) (.app (.app (.lam (.lam (.app (.var 1) (.app (.app (.lam (.lam (.app (.var 1) (.app (.app (.lam (.lam (.app (.var 0) (.var 1)))) (.var 1)) (.var 0))))) (.var 1)) (.var 0))))) (.var 1)) (.var 0))))))
/-- `ordinary_unary_add 2s 3s` of `../nets/check.py`. -/
def ordinaryUnaryAdd : Src :=
  (.app (.app (.app (.lam (.app (.lam (.app (.var 1) (.lam (.app (.app (.var 1) (.var 1)) (.var 0))))) (.lam (.app (.var 1) (.lam (.app (.app (.var 1) (.var 1)) (.var 0))))))) (.lam (.lam (.app (.app (.var 0) (.lam (.var 0))) (.lam (.lam (.app (.app (.var 3) (.var 1)) (.lam (.lam (.app (.var 0) (.var 2))))))))))) (.lam (.lam (.app (.var 0) (.lam (.lam (.app (.var 0) (.lam (.lam (.var 1)))))))))) (.lam (.lam (.app (.var 0) (.lam (.lam (.app (.var 0) (.lam (.lam (.app (.var 0) (.lam (.lam (.var 1)))))))))))))
/-- `linear_list_add 3 1` of `../nets/check.py`. -/
def linearListAdd : Src :=
  (.app (.app (.lam (.lam (.app (.app (.app (.app (.app (.var 1) (.lam (.lam (.lam (.lam (.lam (.app (.var 2) (.lam (.lam (.app (.app (.app (.var 6) (.var 6)) (.var 1)) (.lam (.lam (.app (.app (.app (.var 0) (.var 0)) (.var 5)) (.lam (.lam (.app (.app (.app (.var 3) (.var 3)) (.var 1)) (.lam (.lam (.app (.app (.app (.var 0) (.var 2)) (.var 1)) (.lam (.lam (.app (.app (.app (.var 13) (.var 8)) (.var 0)) (.lam (.app (.var 11) (.app (.app (.lam (.lam (.lam (.lam (.app (.app (.var 1) (.var 3)) (.app (.app (.var 2) (.var 1)) (.var 0))))))) (.var 2)) (.var 0)))))))))))))))))))))))))) (.lam (.lam (.lam (.lam (.app (.app (.app (.app (.var 0) (.lam (.lam (.app (.var 0) (.var 1))))) (.var 2)) (.var 1)) (.var 3))))))) (.app (.app (.var 0) (.lam (.lam (.lam (.app (.app (.var 0) (.var 2)) (.var 1)))))) (.lam (.var 0)))) (.lam (.lam (.lam (.app (.app (.var 0) (.var 2)) (.var 1)))))) (.lam (.lam (.lam (.lam (.lam (.app (.app (.app (.app (.var 0) (.var 4)) (.var 2)) (.var 3)) (.var 1)))))))))) (.lam (.lam (.app (.app (.var 1) (.lam (.lam (.lam (.app (.app (.var 0) (.var 1)) (.var 2)))))) (.app (.app (.lam (.lam (.app (.app (.var 1) (.lam (.lam (.lam (.app (.app (.var 0) (.var 1)) (.var 2)))))) (.app (.app (.lam (.lam (.app (.app (.var 1) (.lam (.lam (.lam (.app (.app (.var 0) (.var 2)) (.var 1)))))) (.app (.app (.lam (.lam (.app (.var 0) (.var 1)))) (.var 1)) (.var 0))))) (.var 1)) (.var 0))))) (.var 1)) (.var 0)))))) (.lam (.lam (.app (.app (.var 1) (.lam (.lam (.lam (.app (.app (.var 0) (.var 1)) (.var 2)))))) (.app (.app (.lam (.lam (.app (.app (.var 1) (.lam (.lam (.lam (.app (.app (.var 0) (.var 2)) (.var 1)))))) (.app (.app (.lam (.lam (.app (.app (.var 1) (.lam (.lam (.lam (.app (.app (.var 0) (.var 2)) (.var 1)))))) (.app (.app (.lam (.lam (.app (.var 0) (.var 1)))) (.var 1)) (.var 0))))) (.var 1)) (.var 0))))) (.var 1)) (.var 0))))))

/-- (λx. x x)(λx.λy. y y): two copies of one value cross; a net with one copier kind gets it wrong. -/
def crossSelf : Src :=
  .app (.lam (.app (.var 0) (.var 0))) (.lam (.lam (.app (.var 0) (.var 0))))
/-- (λx. x x)(λx. x (λy. x)): a copy of a value is captured inside another; also wrong with one copier. -/
def crossCapture : Src :=
  .app (.lam (.app (.var 0) (.var 0))) (.lam (.app (.var 0) (.lam (.var 1))))

/-- Plain jobs: compile, run, read back. Moves on both existing nets (they share the λ part): add33 820,
mul33 3,902, mul44 6,699, linearUnaryAdd 165, ordinaryUnaryAdd 445, linearListAdd 1,395, crossSelf 35,
crossCapture 83; 13,544 in all. -/
def plain : List Src :=
  [add33, mul33, mul44, linearUnaryAdd, ordinaryUnaryAdd, linearListAdd, crossSelf, crossCapture]
/-- `reify` on the quote of these: several nested binders (sComb), applications that reduce (succ3), and
a 41-node term with a fixed point (add, a 316-node code). -/
def reifyTerms : List Src := [sComb, succ3, add]
/-- `normNat (run (quote t))` on these: numerals still to compute, up to a multiplication. -/
def runTerms : List Src := [succ3, add21, add33, mul33]
/-- Fuel for the reference, and moves allowed per job. -/
def fuel : Nat := 100000
def budget : Nat := 10000000
end Battery

/-- Each battery job with the check its answer must pass. -/
def batteryJobs (kit : Kit K) : List (JobTm K × (Src → Bool)) :=
  Battery.plain.map (fun t => (plainJob t, fun v => eval Battery.fuel t == some v)) ++
  Battery.reifyTerms.map (fun t => (reifyJob kit t, (· == encode t))) ++
  Battery.runTerms.map (fun t => (natJob kit t, fun v => eval Battery.fuel (.app normNat t) == some v))

/-! ## The test: every small closed term, and a fixed family of numbers -/

/-- The reference's fuel in the test. A term it does not finish within this fuel has only its `reify`
job tested: up to size 9 that is (λx. x x)(λx. x x) alone (`omega`). -/
def testFuel : Nat := 300

/-- Moves each test job may take. -/
def testBudget : Nat := 1000000

/-- Terms that compute a number, so `run` is tested on real work and not only on the small terms (few of
which stand for a number): add i j and mul i j for i, j ≤ 2, written with the battery's add and mul. -/
def numFamily : List Src :=
  (List.range 3).flatMap fun i => (List.range 3).flatMap fun j =>
    [.app (.app Battery.add (scott i)) (scott j), .app (.app Battery.mul (scott i)) (scott j)]

/-- One term, every job, decided by the runner: `reify` always; the plain job and the halting observer
when the reference finishes; the numeral observer when the term stands for a number. -/
def testTerm (sys : System K) (g : Gadget K) (kit : Kit K) (t : Src) : Bool :=
  (runJob sys g testBudget (reifyJob kit t)).map (·.1) == some (encode t) &&
  (match eval testFuel t with
   | none => true
   | some v => (runJob sys g testBudget (plainJob t)).map (·.1) == some v &&
       (runJob sys g testBudget (haltJob kit t)).map (·.1) == some (scott 0)) &&
  match natWant testFuel t with
  | none => true
  | some k => (runJob sys g testBudget (natJob kit t)).map (·.1) == some (scott k)

/-- The terms the test up to size n runs: every closed term up to size n, then `numFamily`. -/
def testTerms (n : Nat) : List Src := upTo n ++ numFamily

/-- **A test of the spec, not the spec**: `testTerms n`, with the fuel and budget above. Decided by the
runner, so by `native_decide`. -/
def MeetsTested (sys : System K) (g : Gadget K) (kit : Kit K) (n : Nat) : Prop :=
  (testTerms n).all (testTerm sys g kit) = true

instance (sys : System K) (g : Gadget K) (kit : Kit K) (n : Nat) :
    Decidable (MeetsTested sys g kit n) := by unfold MeetsTested; infer_instance

/-- The least test a record may claim. -/
def minTestSize : Nat := 9

/-! ## Records: what each rewrite leaves behind -/

/-- A rule's made agents in the order they are first reached from the slots (then the rest). -/
def canonOrder (S : Sig K) (r : GRule K) (slots : List End) : List Nat :=
  let arOf := fun j => ((r.made[j]?).map (arity S)).getD 0
  let visit := fun (order : List Nat) (e : End) => match other r e with
    | some (.new j _) => if order.contains j then order else order ++ [j]
    | _ => order
  let rec go : Nat → Nat → List Nat → List Nat
    | 0, _, order => order
    | fuel + 1, i, order => match order[i]? with
      | none => order
      | some j => go fuel (i + 1) ((List.range (arOf j + 1)).foldl (fun o n => visit o (.new j n)) order)
  let order := go (r.made.length + 1) 0 (slots.foldl visit [])
  order ++ (List.range r.made.length).filter (fun j => !order.contains j)

/-- A rule's right side up to renaming its made agents, with the slots in the given order: the kinds
made, and where each slot and each made port is wired. -/
def rhsForm (S : Sig K) (ks : List K) (r : GRule K) (slots : List End) :
    List (Option Nat) × List (Nat × Nat × Nat) :=
  let order := canonOrder S r slots
  let arOf := fun j => ((r.made[j]?).map (arity S)).getD 0
  let code : Option End → Nat × Nat × Nat := fun
    | some (.new j n) => (1, order.idxOf j, n)
    | some e => (0, slots.idxOf e, 0)
    | none => (2, 0, 0)
  let all := slots ++ order.flatMap fun j => (List.range (arOf j + 1)).map (End.new j ·)
  (order.map fun j => (r.made[j]?).map ks.idxOf, all.map fun e => code (other r e))

def leftSlots (S : Sig K) (r : GRule K) : List End := (List.range (arity S r.a)).map (End.left ·.succ)
def rightSlots (S : Sig K) (r : GRule K) : List End := (List.range (arity S r.b)).map (End.right ·.succ)

/-- Two rules leave the same right side, either way round. -/
def sameRhs (S : Sig K) (ks : List K) (r s : GRule K) : Bool :=
  let f := rhsForm S ks r (leftSlots S r ++ rightSlots S r)
  f == rhsForm S ks s (leftSlots S s ++ rightSlots S s) || f == rhsForm S ks s (rightSlots S s ++ leftSlots S s)

/-- Bits to tell which rule fired, given the right side it left: log2 of how many rules leave it. A
right side that makes no agent leaves nothing to carry a mark, so such a rule pays for naming any rule
of the table. A crude, lower-bound style measure, not a proved bound. -/
def recordBits (sys : System K) (r : GRule K) : Nat :=
  if r.made.isEmpty then clog2 sys.rules.length
  else clog2 (sys.rules.filter (sameRhs sys.sig sys.kinds r)).length

/-- Records of a run: the record bits of each rule, times how often it fired. -/
def recordsOf (sys : System K) (t : Tally) : Nat :=
  (sys.rules.zipIdx.map fun (r, i) => t.perRule.getD i 0 * recordBits sys r).sum

/-- The battery, run: total moves, rounds and records, or `none` if any job fails. -/
def batteryCost (sys : System K) (g : Gadget K) (kit : Kit K) : Option (Nat × Nat × Nat) :=
  (batteryJobs kit).foldlM (fun (w, rd, rc) (j, ok) =>
    match runJob sys g Battery.budget j with
    | some (v, t) => if ok v then some (w + t.fires + t.cuts, rd + t.rounds, rc + recordsOf sys t) else none
    | none => none) (0, 0, 0)

/-! ## Costs, dominance, records -/

/-- The cost vector: description bits, moves, rounds, record bits. -/
structure Cost where
  bits : Nat
  work : Nat
  rounds : Nat
  records : Nat
  deriving DecidableEq, Repr

instance : LE Cost := ⟨fun c d => c.bits ≤ d.bits ∧ c.work ≤ d.work ∧ c.rounds ≤ d.rounds ∧ c.records ≤ d.records⟩
instance (c d : Cost) : Decidable (c ≤ d) := by unfold LE.le instLECost; infer_instance

/-- c is at least as good everywhere and better somewhere. -/
def Dominates (c d : Cost) : Prop := c ≤ d ∧ c ≠ d
instance (c d : Cost) : Decidable (Dominates c d) := by unfold Dominates; infer_instance

/-- The strength of a record's evidence: the spec, or the test up to a size. -/
inductive Strength
  | full
  | tested (n : Nat)
  deriving DecidableEq, Repr

def Strength.Holds (sys : System K) (g : Gadget K) (kit : Kit K) : Strength → Prop
  | .full => Meets sys g kit
  | .tested n => minTestSize ≤ n ∧ MeetsTested sys g kit n

/-- s is at least as strong as s': the spec above every test, a larger test above a smaller one. -/
def Strength.atLeast : Strength → Strength → Bool
  | .full, _ => true
  | .tested _, .full => false
  | .tested n, .tested m => m ≤ n

/-- **A record**: a system, its gadget and kit, checked evidence that it does the job, and its cost
vector proved equal to what the frozen functions compute. -/
structure Record where
  K : Type
  [deq : DecidableEq K]
  sys : System K
  gadget : Gadget K
  kit : Kit K
  strength : Strength
  cost : Cost
  ok : Ok sys
  gadgetOk : gadget.ok sys = true
  kitOk : kit.ok sys = true
  meets : strength.Holds sys gadget kit
  bits : cost.bits = bitsOf sys gadget kit
  runs : batteryCost sys gadget kit = some (cost.work, cost.rounds, cost.records)

/-- R beats R': its cost dominates R''s and its evidence is at least as strong. -/
def Beats (R R' : Record) : Prop := Dominates R.cost R'.cost ∧ R.strength.atLeast R'.strength = true

/-- R blocks R': its cost is at most R''s everywhere and its evidence is at least as strong. The search
adds a candidate only when no record already in it blocks the candidate, so an equal record (a relabelled
copy, say) is never added again. -/
def Blocks (R R' : Record) : Prop := R.cost ≤ R'.cost ∧ R.strength.atLeast R'.strength = true

theorem Beats.blocks {R R' : Record} (h : Beats R R') : Blocks R R' := ⟨h.1.1, h.2⟩

/-- Some record of strength at least s costs at most c. -/
def Achieved (s : Strength) (c : Cost) : Prop := ∃ R : Record, R.strength.atLeast s = true ∧ R.cost ≤ c

/-- Some record with the full spec costs at most c. -/
def AchievedFull (c : Cost) : Prop := Achieved .full c

/-- The notes' single score J: the description bits alone. Nothing ranks by it alone. -/
def J (R : Record) : Nat := R.cost.bits

/-! ## Lemmas

What the run theory gives a record, and the order of tests. None of them is about the runner `Eng`. -/

section Lemmas
variable {S : Sig K} {T : List (GRule K)} {keep : Ident → Prop}

omit [DecidableEq K] in
/-- **Work is well defined**: from a good net, any two runs that finish (firing pairs and removing
forwarders in any order) take the same number of moves. From `Path.bound` and `move_diamond`. -/
theorem work_well_defined (h : RunOk S T) {N F F' : Net K} {m n : Nat} (hg : N.Good S)
    (hm : Path (Move S T keep) m N F) (hf : Finished S T keep F)
    (hn : Path (Move S T keep) n N F') (hf' : Finished S T keep F') : m = n := by
  have pres : ∀ a b, Net.Good S a → Move S T keep a b → Net.Good S b := fun _ _ hg hm => h.move_good hg hm
  have h1 := (Path.bound pres h.move_diamond hm hg hn hf').1
  have h2 := (Path.bound pres h.move_diamond hn hg hm hf).1
  omega

theorem lookup_mem {ws : List (Port × Port)} {p q : Port} (h : lookup ws p = some q) :
    (p, q) ∈ ws ∨ (q, p) ∈ ws := by
  unfold lookup at h
  cases hf : ws.find? (fun w => w.1 == p) with
  | some w =>
    rw [hf] at h; simp only [Option.some.injEq] at h; subst h
    have := List.find?_some hf; simp only [beq_iff_eq] at this
    left; rw [← this]; exact List.mem_of_find?_eq_some hf
  | none =>
    rw [hf] at h
    cases hg : ws.find? (fun w => w.2 == p) with
    | none => rw [hg] at h; cases h
    | some w =>
      rw [hg] at h; simp only [Option.map_some, Option.some.injEq] at h; subst h
      have := List.find?_some hg; simp only [beq_iff_eq] at this
      right; rw [← this]; exact List.mem_of_find?_eq_some hg

omit [DecidableEq K] in
/-- A built net passing `Build.ok` is good, so the run theorems apply to it. -/
theorem Build.ok_good {b : Build K} (h : b.ok S = true) : b.net.Good S := by
  simp only [Build.ok, Bool.and_eq_true, List.all_eq_true, beq_iff_eq, bne_iff_ne, ne_eq] at h
  obtain ⟨hroots, hw⟩ := h
  have wire : ∀ p q, b.net.mate p = some q → b.net.mate q = some p ∧ b.net.kind p.1 ≠ none ∧
      b.net.kind q.1 ≠ none ∧ b.net.pol S p ≠ b.net.pol S q := by
    intro p q hm
    rcases lookup_mem hm with hmem | hmem
    · obtain ⟨⟨⟨⟨_, h2⟩, h3⟩, h4⟩, h5⟩ := hw _ hmem
      exact ⟨h2, by simpa [Option.isSome_iff_ne_none] using h3, by simpa [Option.isSome_iff_ne_none] using h4, h5⟩
    · obtain ⟨⟨⟨⟨h1, _⟩, h3⟩, h4⟩, h5⟩ := hw _ hmem
      exact ⟨h1, by simpa [Option.isSome_iff_ne_none] using h4, by simpa [Option.isSome_iff_ne_none] using h3,
        fun e => h5 e.symm⟩
  refine ⟨fun p q hm => (wire p q hm).2.2.2, fun p q hm => ⟨(wire p q hm).2.1, (wire p q hm).2.2.1⟩,
    fun p q hm => (wire p q hm).1, tidy_of_roots ?_⟩
  intro x hx
  cases hf : b.kinds.find? (·.1 == x) with
  | none => exact absurd (by simp [Build.net, hf]) hx
  | some e =>
    have hex := List.find?_some hf; simp only [beq_iff_eq] at hex
    have hr := hroots e (List.mem_of_find?_eq_some hf)
    obtain ⟨y, g⟩ := e
    simp only at hex hr; subst hex
    cases y with
    | root n => exact ⟨n, rfl⟩
    | made _ _ _ => simp at hr

/-- **An answer is the answer of every order**: when a job answers, every run that finishes, in any
order, ends in the very net the answer was read from. -/
theorem Answers.every_order {sys : System K} {g : Gadget K} {j : JobTm K} {want : Src → Prop}
    (hok : Ok sys) (h : Answers sys g j want) :
    ∃ F, Clean F (load g j).2 ∧
      (∃ n v, (F.mate ((load g j).2, 0)).bind (readback g n F []) = some v ∧ want v) ∧
      ∀ M, Moves sys.sig sys.rules (· = (load g j).2) (load g j).1.net M →
        Finished sys.sig sys.rules (· = (load g j).2) M → M = F := by
  obtain ⟨hb, F, hrun, hfin, hcl, hans⟩ := h
  exact ⟨F, hcl, hans, fun M hM fM => hok.run.finished_unique (Build.ok_good hb) hM hrun fM hfin⟩

/-- A test up to size n passes every test up to a smaller size, so `Strength.atLeast` orders tests
soundly. -/
theorem MeetsTested.mono {sys : System K} {g : Gadget K} {kit : Kit K} {m n : Nat} (h : m ≤ n)
    (ht : MeetsTested sys g kit n) : MeetsTested sys g kit m := by
  unfold MeetsTested testTerms upTo at *
  obtain ⟨k, rfl⟩ := Nat.exists_eq_add_of_le h
  rw [List.range_add, List.flatMap_append, List.all_append, List.all_append, Bool.and_eq_true,
    Bool.and_eq_true] at ht
  rw [List.all_append, Bool.and_eq_true]
  exact ⟨ht.1.1, ht.2⟩

theorem Strength.holds_of_atLeast {sys : System K} {g : Gadget K} {kit : Kit K} {n m : Nat}
    (h : (Strength.tested n).atLeast (.tested m) = true) (hm : minTestSize ≤ m)
    (hs : (Strength.tested n).Holds sys g kit) : (Strength.tested m).Holds sys g kit := by
  simp only [Strength.atLeast, decide_eq_true_eq] at h
  exact ⟨hm, hs.2.mono h⟩

/-- `asNat` reads exactly the Scott numerals: `natWant` asks for the literal numeral `scott k`. -/
theorem asNat_eq_some {v : Src} {k : Nat} (h : asNat v = some k) : v = scott k := by
  induction v using asNat.induct generalizing k with
  | case1 => simp [asNat] at h; subst h; rfl
  | case2 n ih =>
    simp only [asNat, Option.map_eq_some_iff] at h
    obtain ⟨m, hm, rfl⟩ := h
    rw [ih hm]; rfl
  | case3 v h1 h2 => cases v <;> simp_all [asNat]

theorem asNat_scott (k : Nat) : asNat (scott k) = some k := by
  induction k with
  | zero => rfl
  | succ k ih => simp [scott, asNat, ih]

end Lemmas

/-! ## Sanity checks -/

example : clog2 1 = 0 ∧ clog2 2 = 1 ∧ clog2 3 = 2 ∧ clog2 1024 = 10 ∧ clog2 1025 = 11 := by decide
example : gammaLen 1 = 1 ∧ gammaLen 4 = 5 ∧ fact 5 = 120 ∧ choose 7 5 = 21 := by decide
example : Dominates ⟨1, 2, 3, 4⟩ ⟨1, 2, 3, 5⟩ ∧ ¬ Dominates ⟨1, 2, 3, 5⟩ ⟨1, 2, 3, 4⟩ ∧
    ¬ Dominates ⟨1, 2, 3, 4⟩ ⟨1, 2, 3, 4⟩ ∧ ¬ Dominates ⟨0, 9, 3, 4⟩ ⟨1, 2, 3, 4⟩ := by decide
/-- (λx. x) (λy. y) is λy. y; out of fuel is no answer. -/
example : eval 5 (.app (.lam (.var 0)) (.lam (.var 0))) = some (.lam (.var 0)) ∧
    eval 1 (.app (.lam (.var 0)) (.lam (.var 0))) = none := by decide
/-- Scott arithmetic answers literal numerals: the reference's value is the numeral itself. -/
example : eval 20 (.app Battery.succ (scott 0)) = some (scott 1) ∧
    eval testFuel Battery.add21 = some (scott 3) ∧ eval Battery.fuel Battery.add33 = some (scott 6) := by decide
/-- The test's one skipped term up to size 9. -/
example : eval testFuel omega = none ∧ omega.size = 9 := by decide +kernel
example : Battery.add33.size = 73 ∧ Battery.mul33.size = 112 ∧ Battery.mul44.size = 120 ∧
    Battery.succ3.size = 24 ∧ Battery.add21.size = 70 ∧ Battery.add.size = 41 ∧
    (encode Battery.add).size = 316 ∧ Battery.sComb.closed = true ∧ Battery.crossSelf.size = 10 ∧
    Battery.crossCapture.size = 10 ∧ (encode (.lam (.var 0))).closed = true := by decide
/-- The observers on numerals and on a non-number; the crossing terms' values. -/
example : asNat (scott 3) = some 3 ∧ asNat (.lam (.var 0)) = none ∧
    natWant testFuel Battery.add21 = some 3 ∧ natWant testFuel (.lam (.var 0)) = none ∧
    eval testFuel Battery.crossSelf = some (.lam (.app (.var 0) (.var 0))) := by decide +kernel
example : (Strength.full).atLeast (.tested 9) ∧ (Strength.tested 9).atLeast (.tested 8) ∧
    !(Strength.tested 8).atLeast (.tested 9) ∧ !(Strength.tested 100).atLeast .full := by decide

/-- An empty system is admissible-shaped but has no gadget: the spec cannot be met without kinds. -/
example : Gadget.ok (⟨[], ⟨fun _ => []⟩, [], []⟩ : System Bool)
    ⟨true, true, true, true, true, true, .encoded⟩ = false := by decide

end Statement
