import QuoteKit

/-! # A record for the quote-and-table net, against the fixed problem statement (`Statement.lean`, v0.2)

The quote-and-table net (14 kinds, 35 rules, `../ideal/ktable.py`) as a `Statement.System`, its gadget
(lam, app, var, the sharer `sig`, the input eraser `eraI`, the thaw `T`; quoting wraps the term's code in
one `var` agent, which is how `../ideal/knet.py` writes a quote `'t`), and its kit (`../ideal/kexport.py`,
which also writes the data module `QuoteKit.lean`):
- `reify` (107 agents), written in the net's own term language and compiled by `knet.py`:
    \q. [ \t f d T. (\T2. (\e. \a b c. c e) (t ('f T2) (\z s. s d) T2)) (\p. p ρ T)
        | \t g x d T. (\G X. \a b c. b G X) (t g d T) (t x d T)
        | \t w d T. (\i. \a b c. a i) (w d)
        | ρ | ρ ] q (\z s. z) (\n. n)
  The table reads the quoted code node by node, carrying the depth d (a Scott numeral) and a token chain
  T. A function is opened on a fresh token T2 = λp. p ρ T, so each of its variables reads back as a quote
  of T2, and its body is read at depth d + 1. At a use at depth u, T2 applied to the depth numeral strips
  one successor and hands the rest to the enclosing binder's token, down to the root's λn. n: the answer
  is u - (binder's depth) - 1, the de Bruijn index, as a tail of the depth numeral, so already a literal
  Scott numeral. Every `encode` node is built after its children are values, so it reads back literally.
- `run` (2 agents): the thaw, λq. T q. Applied to a quote, the thaw meets the wrapper `var` and the two
  annihilate, leaving the term's code under the thaw the application made: the same net the plain job runs.

Why `reify` is right on every closed term (argued, not proved): by induction on the term, reading code c
whose free variables are quotes of tokens, at depth u with chain T_u, answers `encode` of c with each such
quote of a binder at depth b < u read as index u - b - 1. Needs: the reading rules hand a slot the parts of
the node (`kref.py`); opening a λ substitutes a quote of the token for its variable without running the body;
the token chain T_b is pred^(b+1) on Scott numerals of depth ≥ b + 1; call by value builds each `encode`
node from finished children. Nothing runs the quoted term, so a term with no value is read like any other.

Proved here:
- `quote_ok : Ok qsys`: table checks, totality, closure, the run checks, every rule complete; by `decide`.
- `quote_gadget_ok`, `quote_kit_ok`: the gadget, and both programs valid nets; by `decide` (the kit by
  `decide +kernel`, which skips the elaborator's recursion limit).
- `quote_bit_terms`, `quote_bits`: each term's share, and the description length; by `decide +kernel`.
- `quote_jobs`: each battery job's moves, rounds and records; `quote_runs`: the battery's totals, every
  answer checked; by `native_decide` (the runner uses hash maps).
- `quote_tested : MeetsTested qsys qg qkit 9`: the 2,622 closed terms up to size 9 and the 18-term numeral
  family, every job; by `native_decide`.
- `quote_record : Achieved (.tested 9) ⟨2288, 41221, 2388, 59658⟩`.

`#print axioms quote_record`:
  [propext, Classical.choice, Quot.sound, QuoteRec.quote_runs._native.native_decide.ax_1_1,
   QuoteRec.quote_tested._native.native_decide.ax_1_1]
(the last two are the two `native_decide` facts, trusted to the compiler; nothing else is assumed.)

Only tested, in Python (`kexport.py`): `reify` on all 2,622 closed terms up to size 9 and 500 random closed
terms up to size 120, against the reference machine of `../ideal/kref.py`, answers compared literally with
`encode`; on the net (`knet.py`) for one in 25 and one in 5 of those.

Measured with this runner, not proved here (`QuoteKit.lean` keeps both programs):
- `run := interpProg` (v0.1's 29-agent table interpreter) instead of the thaw: bits 2537, battery
  ⟨101466 moves, 4696 rounds, 155952 records⟩; its four run jobs take 68,876 moves against the thaw's 8,631.
  It also passes the test to 9.
- `reify := reifyKProg` (122 agents, the same reader in continuation-passing style, the table handed back
  instead of shared at each application): ⟨2488, 35725, 3782, 50027⟩, also passing the test to 9. Neither
  it nor this record dominates the other.

Not proved here: `Meets` (the spec for every closed term); the two obligations `StatementSound.lean` leaves
(`LoadOk`, `CleanTransfer`).
-/

namespace QuoteRec
open Statement

def qsys : System Q := { kinds := Q.all, sig := qsig, rules := qtable, stuck := qstuck }

def qg : Gadget Q :=
  { lam := .lam, app := .app, var := .var, share := .sig, erase := .eraI, thaw := .T, quote := .native .var }

def qkit : Kit Q := { reify := reifyProg, run := runProg }

theorem q_tableOk : TableOk qsig qtable :=
  ⟨by show ∀ r ∈ qtable, ∀ w ∈ r.wires, r.wireOk qsig w = true; decide,
   by show ∀ r ∈ qtable, ∀ w ∈ r.wires, r.EndOk w.1 ∧ r.EndOk w.2; decide,
   by show ∀ r ∈ qtable, ∀ r' ∈ qtable, ((r.a = r'.a ∧ r.b = r'.b) ∨ (r.a = r'.b ∧ r.b = r'.a)) → r = r'
      decide,
   by show ∀ r ∈ qtable, qsig.pol r.a 0 ≠ qsig.pol r.b 0; decide⟩

theorem quote_ok : Ok qsys where
  admissible :=
    { table := q_tableOk
      total := by decide
      closed := by decide }
  run := ⟨q_tableOk, by decide⟩
  complete := by decide

theorem quote_gadget_ok : qg.ok qsys = true := by decide
theorem quote_kit_ok : qkit.ok qsys = true := by decide +kernel

/-- The bits, term by term: signature, table (rule count and rules), gadget, reify (107 agents), run
(2 agents); one ceiling over the product. -/
theorem quote_bit_terms :
    (clog2 (sigAlts qsys), clog2 (tableAlts qsys), clog2 (gadgetAlts qsys qg),
      clog2 (reifyProg.alts qsys), clog2 (runProg.alts qsys)) = (89, 972, 28, 1186, 14) := by decide +kernel

theorem quote_bits : bitsOf qsys qg qkit = 2288 := by decide +kernel

/-- Moves, rounds and records of each battery job, in `batteryJobs` order: the eight plain jobs, `reify`
on sComb, succ3, add, then `normNat (run ·)` on succ3, add21, add33, mul33. -/
def jobCost (j : JobTm Q) : Option (Nat × Nat × Nat) :=
  (runJob qsys qg Battery.budget j).map fun (_, t) => (t.fires + t.cuts, t.rounds, recordsOf qsys t)

theorem quote_jobs : (batteryJobs qkit).map (jobCost ·.1) =
      [some (820, 120, 1189), some (3902, 194, 5640), some (6699, 244, 9739), some (165, 33, 188),
       some (445, 90, 604), some (1395, 283, 1731), some (35, 11, 49), some (83, 21, 118),
       some (2584, 158, 3854), some (5359, 199, 7865), some (11103, 323, 16512),
       some (591, 106, 798), some (1163, 126, 1614), some (1728, 195, 2419), some (5149, 285, 7338)] := by
  native_decide

theorem quote_runs : batteryCost qsys qg qkit = some (41221, 2388, 59658) := by native_decide

theorem quote_tested : MeetsTested qsys qg qkit 9 := by native_decide

def quoteRecord : Record where
  K := Q
  sys := qsys
  gadget := qg
  kit := qkit
  strength := .tested 9
  cost := ⟨2288, 41221, 2388, 59658⟩
  ok := quote_ok
  gadgetOk := quote_gadget_ok
  kitOk := quote_kit_ok
  meets := ⟨by decide, quote_tested⟩
  bits := quote_bits.symm
  runs := quote_runs

theorem quote_record : Achieved (.tested 9) ⟨2288, 41221, 2388, 59658⟩ := ⟨quoteRecord, by decide⟩

end QuoteRec
#print axioms QuoteRec.quote_record
