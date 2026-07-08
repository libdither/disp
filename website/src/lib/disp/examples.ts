// Playground presets. Every block here must pass through the real compiler —
// they are validated by scripts/validate-examples.mts against the repo's
// elaborator (the README literate-code discipline).

export interface Example {
  id: string
  label: string
  kernel: boolean // opens the kernel (first run pays the self-verification load)
  source: string
}

export const examples: Example[] = [
  {
    id: 'welcome',
    label: 'Welcome tour',
    kernel: true,
    source: `// Welcome to the disp playground.
// This is the real compiler — the same elaborator and kernel that run in CI —
// compiled to WebAssembly, running entirely in your tab.
//
// ▶ Run file (Ctrl/⌘-Enter) runs everything. The first run loads and
//   SELF-VERIFIES the kernel (~a minute): you are watching the type system
//   check itself. After that, runs are instant.
// ▶ Run to cursor (Shift-Enter) elaborates the file up to the line you're on.
// ▶ The prompt at the bottom right evaluates expressions against this file.

open use "../kernel/prelude.disp"   // the type system (checked + cached)
open use "../std/nat/ops.disp"      // double, pred, is_zero

// A test passes when both sides reduce to the identical tree:
test double 2 = 4

// Types are predicates — to check data, apply the type:
test Nat 3 = Ok true
test Nat (t (t t)) = Ok false   // not a numeral-shaped tree

// Functions are checked by minting a hypothesis (a promise of a Nat),
// running the body on it, and watching what comes out:
quadruple : Nat -> Nat := {n} -> double (double n)
test quadruple 3 = 12

// Proving is running. An equation's only proof value is the leaf itself:
test refl = t
test (Eq Nat (double 2) 4) refl = Ok true

// The universe passes its own checker:
test param_apply Type Type = Ok true
`
  },
  {
    id: 'trees',
    label: 'Raw tree calculus (no kernel — instant)',
    kernel: false,
    source: `// Below the type system: the tree calculus itself. Five rewrite rules over
// binary trees grown from a single leaf \`t\`. No kernel load needed — this
// runs immediately.

open use raw "../prelude.disp" {}

// Three of the five rules dispatch on the SHAPE of a tree (leaf / stem /
// fork), so case analysis over arbitrary data is a rewrite rule, not
// library code:
let shape_of := t (t "leaf" ({u} -> "stem")) ({u, v} -> "fork")
test shape_of t = "leaf"
test shape_of (t t) = "stem"
test shape_of (t t t) = "fork"

// Numbers are trees too — 3 is sugar for succ (succ (succ zero)):
test tree_eq 3 (succ (succ (succ zero))) = true

// Programs can take programs apart — this is the reflection the whole
// language is built on (and the kernel exists to police):
test shape_of shape_of = "fork"
`
  },
  {
    id: 'records',
    label: 'Records & derived fields',
    kernel: true,
    source: `open use "../kernel/prelude.disp"
open use "../std/nat/ops.disp"

// A record type with a DERIVED field: b's recipe runs during the check.
let TDs := { a : Nat, b := double a }

test TDs { a := 2; b := 4 } = Ok true
test TDs { a := 2; b := 5 } = Ok false   // ran double 2, compared, rejected

// Records project by name; projection is just application of an accessor:
let p := { a := 3; b := 6 }
test p.a = 3
test tree_eq ({r} -> r.x) ({r} -> r (acc "x")) = true

// Surface record-type literals ARE the library telescope former:
let Point := { x : Nat, y : Nat }
let PointCells := Telescope (t (proj_cell "x" Nat) ({_x} -> t (proj_cell "y" Nat) ({_y} -> t)))
test tree_eq Point PointCells = true
`
  },
  {
    id: 'proofs',
    label: 'Proofs & hypotheses',
    kernel: true,
    source: `open use "../kernel/prelude.disp"
open use "../std/nat/arith.disp"

// A statement about every Nat is a Pi into a proposition. Checking it mints
// one promise (a hypothesis) and runs the body on it:
n_eq_n : {n : Nat} -> Eq Nat n n := {n} -> refl
test param_apply (Pi Nat ({n} -> Eq Nat n n)) n_eq_n = Ok true

// A false statement is rejected — the stuck ends differ:
test param_apply (Pi Nat ({n} -> Eq Nat n (succ n))) ({n} -> refl) = Ok false

// Watch a promise at work: mint one by hand and observe it.
let hN := make_hyp Nat 0
test param_apply Nat hN = Ok true            // a promise of a Nat counts as a Nat
test is_neutral (nat_rec ({_} -> Bool) true ({n, rec} -> false) hN) = true

// Reading a promise's raw shape is refused (either answer would leak):
test param_apply (Pi Nat ({_} -> Bool)) ({x} -> is_fork x) = Err
`
  },
  {
    id: 'universe',
    label: 'Who checks the types?',
    kernel: true,
    source: `open use "../kernel/prelude.disp"

// Types are values with a recognizable shape, so the universe is a
// predicate like any other — and it accepts itself:
test param_apply Type Nat = Ok true
test param_apply Type zero = Ok false
test param_apply Type Type = Ok true

// Build a type from scratch: a home-made Nat, zero and successor.
let MyNat := Coproduct [pair "z" [], pair "s" [Rec]]
test param_apply Type MyNat = Ok true

// The deeper check is behavioral: verify_good aims the promise machinery
// at a type's own respond, and catches lies in both directions:
let resp_of := {T} -> (type_meta T).respond (type_meta T).recognizer_params
test verify_good MyNat (resp_of MyNat) = Ok true
test verify_good MyNat (inductive_respond unit_witness) = Ok false   // waves junk through
test verify_good MyNat (inert_respond unit_witness) = Ok false       // refuses everything
`
  }
]
