// The disp programs the page runs, and the results it shows without running them.
// `npm run why-check` runs all of it on the same engine the browser uses and fails on any drift.

/** Hidden above every live block. */
export const CONTEXT = `open use raw "../prelude.disp" {}
open use raw "../list.disp" {}
open use raw "../verdict.disp" {}
open use raw "../stream.disp" {}
open use raw "../claim.disp" {}
open use raw "fixtures.disp" {}
open use raw "../../IDEAS.disp" {}
by_search := honest (Trees.as_items)
`

export const DOUBLING = `doubles := {f, n} => if (NatR n) { f n == add n n } else { true }
dbl_add := {d2, n} => if (is_zero n) { zero } else { add 2 (d2 (nat_pred n)) }
by_twos := fix dbl_step
by_add2 := fix dbl_add
by_self := {n} => add n n
cheat := {n} => if (n == 0) { 0 } else if (n == 1) { 2 } else if (n == 2) { 4 } else if (n == 3) { 6 } else { 0 }
`

/** Scene 3: the programs a reader can drop in. `steps` is the recorded cost of running it on 300. */
export const GATE_BUDGET = 200
export const GATE_N = 300
export const CANDIDATES: { id: string; label: string; code: string; steps: number; fails: boolean }[] = [
  { id: 'by_twos', label: 'add two, once per number', code: 'by_twos', steps: 7725, fails: false },
  { id: 'by_add2', label: 'call add 2, once per number', code: 'by_add2', steps: 14972, fails: false },
  { id: 'by_self', label: 'add it to itself', code: 'by_self', steps: 9489, fails: false },
  { id: 'next', label: 'add one', code: 'succ', steps: 0, fails: true },
  { id: 'zero', label: 'always say 0', code: '{_} => 0', steps: 0, fails: true },
  { id: 'cheat', label: 'a short table of answers', code: 'cheat', steps: 122, fails: true }
]

/** Scene 4 */
export const SHAPE_OF = `let shape_of := t (t "leaf" ({u} => "stem")) ({u, v} => "fork")
test shape_of t = "leaf"
test shape_of (t t) = "stem"
test shape_of (t t t) = "fork"`

/** Scene 5 */
export const BELT_DEFS = `nat_fn := {g, x} => if (NatR x) { NatR (g x) } else { true }
sneaky := {n} => if (n == 3) { t t } else { n }
bool_fn := {v, x} => if (Bool x) { Bool (v x) } else { true }
`
export const BELT_CODE = `nat_fn := {g, x} => if (NatR x) { NatR (g x) } else { true }
sneaky := {n} => if (n == 3) { t t } else { n }
test by_search nat_fn ({n} => pair n n) 60 = refuted 1
test by_search nat_fn sneaky 60 = "Open"
test by_search nat_fn sneaky 80 = refuted 3
test by_search nat_fn succ 150 = "Open"

bool_fn := {v, x} => if (Bool x) { Bool (v x) } else { true }
test over bool_fn not [true, false] = "Proved"`
export const BELT_LENGTH = 150
export const BELT_PROGRAMS = [
  { id: 'succ', label: 'add one', code: 'succ' },
  { id: 'pair', label: 'pair it with itself', code: '{n} => pair n n' },
  { id: 'sneaky', label: 'wrong only on 3', code: 'sneaky' }
]

/** Scene 6: the shortcut's own promise, tested by search. */
export const COVER_CODE = `bool_fn := {v, x} => if (Bool x) { Bool (v x) } else { true }
half_bad := {x} => if (x) { true } else { pair x x }
bool_cover := lens ({w} => w) ({_w, x} => if (Bool x) { x } else { true })
bad_cover := lens ({w} => w) ({_w, _x} => true)
test by_search (lens_holds bool_fn bool_fn bool_cover) half_bad 60 = "Open"
test by_search (lens_holds bool_fn bool_fn bad_cover) half_bad 60 = refuted false`

/** Scene 6: needs the confirmer, so run on request. */
export const CONFIRMER_CONTEXT = `open use raw "../prelude.disp" {}
open use raw "../list.disp" {}
open use raw "../verdict.disp" {}
open use raw "fixtures.disp" {}
open use raw "confirmer.test.disp" {}
`
export const INDUCTION_CODE = `dbl_step := {d2, n} => if (is_zero n) { zero } else { succ (succ (d2 (nat_pred n))) }
dbl_add := {d2, n} => if (is_zero n) { zero } else { add 2 (d2 (nat_pred n)) }
wrong := {d2, n} => if (is_zero n) { zero } else { succ (d2 (nat_pred n)) }
test eq_by_induction dbl_step dbl_add = "Proved"
test eq_by_induction dbl_step wrong = refuted 1`

/** Scene 7: what each kind of "same" can see. */
export const SAME_DEFS = `dbl_add := {d2, n} => if (is_zero n) { zero } else { add 2 (d2 (nat_pred n)) }
dbl_pairs := {d2, n} => if (is_zero n) { zero } else { pair t (pair t (d2 (nat_pred n))) }
same_tree := tree_eq dbl_step dbl_pairs
diff_tree := tree_eq dbl_step dbl_add
`

/** Scene 7: doubling n, recorded in a fresh session each, imports subtracted. */
export const DOUBLING_STEPS: { n: number; by_twos: number; by_add2: number; by_self: number }[] = [
  { n: 10, by_twos: 254, by_add2: 541, by_self: 278 },
  { n: 30, by_twos: 634, by_add2: 1401, by_self: 778 },
  { n: 100, by_twos: 2325, by_add2: 4772, by_self: 2889 },
  { n: 300, by_twos: 7725, by_add2: 14972, by_self: 9489 },
  { n: 600, by_twos: 15825, by_add2: 30272, by_self: 19389 }
]

/** Scene 9, recorded (one seed, about 4 minutes a run, so why-check leaves it out):
 *  npx tsx evaluators/rust-ic-strands/programs/bench.ts --data --seeds 1 --kind bits 'added [bit_tree 3 60, bit_tree 3 50, bit_tree 3 3, bit_tree 3 63]' 'saved [bit_tree 3 60, bit_tree 3 50, bit_tree 3 3, bit_tree 3 63]' */
export const SUM_RUNS = {
  added: { steps: 2102, rewrites: 86794, clocks: 84434 },
  saved: { steps: 3412, rewrites: 121297, clocks: 71714 }
}
/** The player cannot hold the four-number pair at once, so it shows a smaller sum. */
export const PLAYER_SRC = 'added [3, 1, 3]; saved [3, 1, 3]'
