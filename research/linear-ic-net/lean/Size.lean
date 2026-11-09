/-! Compiling a λ-term to a net never blows up: at most two agents per node of the term.
Terms use de Bruijn indices. The compiler gives each node one agent, an eraser to a binder whose
variable is unused, and k − 1 sharers to one whose variable is used k times. -/

namespace Size

inductive Tm | var (n : Nat) | app (f a : Tm) | lam (b : Tm)

namespace Tm
def size : Tm → Nat
  | var _ => 1 | app f a => f.size + a.size + 1 | lam b => b.size + 1

/-- uses of the variable with index d at t's top -/
def uses : Tm → Nat → Nat
  | var m, d => if m = d then 1 else 0
  | app f a, d => f.uses d + a.uses d
  | lam b, d => b.uses (d + 1)

/-- agents a binder costs beyond its own: an eraser if unused, sharers if used more than once -/
def extra (k : Nat) : Nat := if k = 0 then 1 else k - 1

/-- agents in the compiled net -/
def agents : Tm → Nat
  | var _ => 1 | app f a => f.agents + a.agents + 1 | lam b => b.agents + extra (b.uses 0) + 1

def vars : Tm → Nat | var _ => 1 | app f a => f.vars + a.vars | lam b => b.vars
def lams : Tm → Nat | var _ => 0 | app f a => f.lams + a.lams | lam b => b.lams + 1
/-- uses bound by a binder inside t -/
def bound : Tm → Nat | var _ => 0 | app f a => f.bound + a.bound | lam b => b.bound + b.uses 0
/-- uses that reach past t's top, with d binders already crossed -/
def free : Tm → Nat → Nat
  | var m, d => if d ≤ m then 1 else 0
  | app f a, d => f.free d + a.free d
  | lam b, d => b.free (d + 1)

theorem free_split (t : Tm) (d : Nat) : t.free d = t.uses d + t.free (d + 1) := by
  induction t generalizing d with
  | var m =>
    simp only [free, uses]
    by_cases h1 : m = d <;> by_cases h2 : d ≤ m <;> by_cases h3 : d + 1 ≤ m <;> simp [h1, h2, h3] <;> omega
  | app f a ihf iha => simp only [free, uses, ihf d, iha d]; omega
  | lam b ih => simp only [free, uses]; exact ih (d + 1)

/-- Every use is either bound inside t or reaches past its top. -/
theorem vars_split (t : Tm) : t.vars = t.bound + t.free 0 := by
  induction t with
  | var m => simp [vars, bound, free]
  | app f a ihf iha => simp only [vars, bound, free, ihf, iha]; omega
  | lam b ih => simp only [vars, bound, free]; rw [ih, free_split b 0]; omega

theorem agents_le_aux (t : Tm) : t.agents ≤ t.size + t.bound + t.lams := by
  induction t with
  | var m => simp [agents, size, bound, lams]
  | app f a ihf iha => simp only [agents, size, bound, lams]; omega
  | lam b ih =>
    simp only [agents, size, bound, lams, extra]
    by_cases h : b.uses 0 = 0 <;> simp [h] <;> omega

theorem vars_lams_le (t : Tm) : t.vars + t.lams ≤ t.size := by
  induction t with
  | var m => simp [vars, lams, size]
  | app f a ihf iha => simp only [vars, lams, size]; omega
  | lam b ih => simp only [vars, lams, size]; omega

/-- No blowup: a compiled net has at most twice as many agents as the term has nodes. -/
theorem agents_le_twice (t : Tm) : t.agents ≤ 2 * t.size := by
  have h1 := agents_le_aux t
  have h2 := vars_split t
  have h3 := vars_lams_le t
  omega
end Tm
end Size
