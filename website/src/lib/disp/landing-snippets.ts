// Landing-page code fragments. Each is DISPLAYED as-is, and VALIDATED by
// scripts/validate-examples.mts as (preamble + body) through the real
// compiler — so the fragments stay truthful even when the lib moves.

export interface Snippet {
  id: string
  body: string
  preamble: string
}

const KERNEL_OPEN = 'open use "../kernel/prelude.disp"\nopen use "../std/nat.disp"\n'

export const snippets: Snippet[] = [
  {
    id: 'checking',
    preamble: KERNEL_OPEN,
    body: `// apply the type; it answers
test Nat 3 = Ok true

// structure runs too — b's recipe is
// evaluated during the check
let TDs := { a : Nat, b := double a }
test TDs { a := 2; b := 4 } = Ok true
test TDs { a := 2; b := 5 } = Ok false`
  },
  {
    id: 'kernel',
    preamble: KERNEL_OPEN + 'let NatToNat := Pi Nat ({_} -> Nat)\n',
    body: `// checking a function mints a promise
// (bind_hyp), runs the body on it, and
// watches what comes out (hyp_reduce)
quadruple : Nat -> Nat :=
  {n} -> double (double n)

test param_apply NatToNat quadruple = Ok true
test param_apply NatToNat ({n} -> "hi") = Ok false`
  },
  {
    id: 'eq',
    preamble: KERNEL_OPEN,
    body: `// deterministic elaboration + hash-consing:
// equal programs are the SAME tree, so
// conversion is a pointer comparison
test tree_eq 3 (succ (succ (succ zero))) = true

// and proofs are runs — refl is the leaf
test (Eq Nat (double 2) 4) refl = Ok true`
  },
  {
    id: 'universe',
    preamble:
      KERNEL_OPEN +
      'let MyNat := Coproduct [pair "z" [], pair "s" [Rec]]\n' +
      'let resp_of := {T} -> (type_meta T).respond (type_meta T).recognizer_params\n',
    body: `// the universe is a predicate like any
// other, and it accepts itself
test param_apply Type Nat  = Ok true
test param_apply Type Type = Ok true

// the checker checks the checkers
test verify_good MyNat (resp_of MyNat) = Ok true`
  }
]

export const snippetById = Object.fromEntries(snippets.map((s) => [s.id, s.body]))
