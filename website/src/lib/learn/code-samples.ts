// Every runnable code block on the Learn page, in one module so
// scripts/validate-examples.mts can push each one through the real compiler.
// `code` is what's displayed; `context` (opens, helper defs) is prepended at
// run/validation time but not shown.

export interface Sample {
  id: string
  context: string
  code: string
}

const KERNEL = 'open use "../kernel/prelude.disp"\n'
const KERNEL_OPS = KERNEL + 'open use "../std/nat/ops.disp"\n'
const RAW = 'open use raw "../prelude.disp" {}\n'

export const samples: Record<string, Sample> = {}
const s = (id: string, context: string, code: string) => {
  samples[id] = { id, context, code }
  return samples[id]
}

// ---- §3 a first look ------------------------------------------------------

s(
  'decl-basics',
  RAW,
  `// untyped definitions — plain combinators
i := {x} -> x
k := {x, y} -> x

// a test is a compile-time obligation: reduce both sides,
// demand the identical tree
test i t = t
test k t (t t) = t`
)

s(
  'decl-typed',
  KERNEL_OPS,
  `// a typed definition: the annotation is an expression too, and the
// compiler VERIFIES it through the kernel when the module loads
quadruple : Nat -> Nat := {n} -> double (double n)
test quadruple 3 = 12`
)

s(
  'match-sample',
  KERNEL_OPS,
  `// a sum-type literal auto-binds its constructors…
Wrap : Type := < box : Nat, empty >
test param_apply Wrap (box 4) = Ok true

// …and match eliminates by tag, binding payloads
test (match (box 4) { box n => double n; empty => 0 }) = 8

// booleans eliminate with if (they are shapes, not tagged sums)
test (if true then 1 else 2) = 1`
)

// ---- §4 types as predicates -----------------------------------------------

s(
  'bool-shapes',
  RAW,
  `// the two booleans are the two smallest distinguishable trees
test tree_eq true t = true
test tree_eq false (t t) = true

// elimination is a triage: the F rule reads the shape
shape_not := t (t false (t t true)) (t t (t t true))
test shape_not true = false
test shape_not false = true`
)

s(
  'central-eq',
  KERNEL,
  `// the central equation, executable: apply the type, read the verdict
test Nat 3 = Ok true
test Nat (t (t t)) = Ok false

// the verdict is a value (a CheckerResult), so failure is data too —
// no exceptions, no judgment layer`
)

// ---- §5 kernel --------------------------------------------------------------

s(
  'hyp-mint',
  KERNEL,
  `// mint a promise by hand: an opaque tree carrying a type and nothing else
let hN := make_hyp Nat 0

// a promise of a Nat counts as a Nat (the H-rule: stored type vs
// required type, compared by hash-cons identity)
test param_apply Nat hN = Ok true

// it is a neutral — recognizable, not inspectable
test is_neutral hN = true`
)

s(
  'walker-carveouts',
  KERNEL,
  `// under the dispatcher, a type that peeks at its argument's raw shape
// is refused — either answer would leak what the promise doesn't contain
test param_apply (Pi Nat ({_} -> Bool)) ({x} -> is_fork x) = Err`
)

s(
  'neutral-eval',
  KERNEL_OPS,
  `// neutral evaluation: an eliminator reaching a promise parks as a
// stuck elimination, TYPED by its motive
let hN := make_hyp Nat 0
test is_neutral (nat_rec ({_} -> Bool) true ({n, rec} -> false) hN) = true

// observations route through the stored type's respond: apply a
// function-promise and the result is stuck AT THE CODOMAIN TYPE
let hPi := make_hyp (Pi Nat ({_} -> Bool)) 0
test neutral_type (param_apply hPi zero) = Bool`
)

s(
  'derived-fields',
  KERNEL_OPS,
  `// respond has exactly two moves. Extend: stay stuck at a type.
// Reduce: compute through. A record with a derived field shows both:
let Pt := { a : Nat, b := succ a }
let hPt := make_hyp Pt 0
test neutral_type (param_apply hPt (acc "a")) = Nat
test tree_eq (param_apply hPt (acc "b")) (succ (param_apply hPt (acc "a"))) = true`
)

// ---- §6 library types -------------------------------------------------------

s(
  'telescope-literal',
  KERNEL,
  `// the surface record literal IS the library telescope former
let Point := { x : Nat, y : Nat }
let PointCells := Telescope (t (proj_cell "x" Nat) ({_x} -> t (proj_cell "y" Nat) ({_y} -> t)))
test tree_eq Point PointCells = true

// and Pi is a two-cell telescope: mint the domain, check the codomain
test param_apply (Pi Nat ({_} -> Nat)) ({n} -> n) = Ok true`
)

s(
  'record-check',
  KERNEL_OPS,
  `// a derived field's recipe runs during the check
let TDs := { a : Nat, b := double a }
test TDs { a := 2; b := 4 } = Ok true
test TDs { a := 2; b := 5 } = Ok false`
)

s(
  'coproduct-sample',
  KERNEL,
  `// a home-made Nat: two variants, one recursive position
let MyNat := Coproduct [pair "z" [], pair "s" [Rec]]
test param_apply Type MyNat = Ok true

// its values are tagged injections
let my_zero := inj "z" []
test param_apply MyNat my_zero = Ok true`
)

s(
  'eq-sample',
  KERNEL_OPS,
  `// one proof value, and it is the leaf itself
test refl = t
test (Eq Nat (double 2) 4) refl = Ok true
test (Eq Nat 3 4) refl = Ok false

// a theorem about every Nat: a Pi into a proposition
n_eq_n : {n : Nat} -> Eq Nat n n := {n} -> refl
test param_apply (Pi Nat ({n} -> Eq Nat n n)) n_eq_n = Ok true
test param_apply (Pi Nat ({n} -> Eq Nat n (succ n))) ({n} -> refl) = Ok false`
)

s(
  'universe-sample',
  KERNEL,
  `test param_apply Type Nat = Ok true
test param_apply Type zero = Ok false
test param_apply Type Type = Ok true`
)

s(
  'verify-good',
  KERNEL,
  `// the behavioral check: aim the promise machinery at a type's own
// respond, and catch lies in both directions
let MyNat := Coproduct [pair "z" [], pair "s" [Rec]]
let resp_of := {T} -> (type_meta T).respond (type_meta T).recognizer_params
test verify_good MyNat (resp_of MyNat) = Ok true
test verify_good MyNat (inductive_respond unit_witness) = Ok false
test verify_good MyNat (inert_respond unit_witness) = Ok false`
)

export const sampleList = Object.values(samples)
