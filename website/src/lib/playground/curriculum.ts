// Guided two-track interactive curriculum for the playground:
//   Track 1 ('trees'): Tree calculus from zero -> real algorithms (raw prelude, instant)
//   Track 2 ('types'): Type theory from zero -> verified rewrites (promoted kernel + lib/std.disp)
//
// Every starter, solution, and breakIt snippet is validated against the real compiler by
// `website/scripts/validate-curriculum.mts` (and `npm run validate:examples`).

export type TrackId = 'trees' | 'types'

export interface TrackMeta {
  id: TrackId
  title: string
  short: string
  subtitle: string
  prereq: string
}

export interface LessonWatch {
  label: string
  /** Jump editor cursor to the first line containing `lineMatch`, OR pin a custom expression */
  target: { lineMatch: string } | { expr: string }
  note?: string
}

export interface LessonBreakIt {
  label: string
  prompt: string
  snippet: string
  /** What failure proves the point: type-error (kernel rejected), error (guard/parse), or test-fail */
  expectKind: 'type-error' | 'error' | 'test-fail'
  why: string
}

export interface LessonGoal {
  prompt: string
  hint: string
  /** Optional definitions that must exist in the buffer for the goal to complete */
  requireDefs?: string[]
  /** Minimum passing tests required */
  minTests?: number
}

export interface Lesson {
  id: string
  track: TrackId
  number: number
  title: string
  tagline: string
  /** Run module typechecker on `:` annotations (Track 2) */
  verifyTypes: boolean
  /** 1-3 short plain-English paragraphs (inline `code` rendered as <code>) */
  body: string[]
  /** One-line key takeaway */
  concept: string
  starter: string
  solution: string
  breakIt?: LessonBreakIt
  watches: LessonWatch[]
  goal: LessonGoal
}

export const tracks: TrackMeta[] = [
  {
    id: 'trees',
    title: 'Tree Calculus → Algorithms',
    short: '1 · Tree Calculus',
    subtitle: 'Build Booleans, numbers, recursion, and list algorithms from a single leaf `t` and five rewrite rules.',
    prereq: 'No prerequisites · runs instantly'
  },
  {
    id: 'types',
    title: 'Type Theory through Disp',
    short: '2 · Type Theory',
    subtitle: 'Learn types, functions, sums, totality, dependent types, polymorphism, proofs, and verified rewrites live.',
    prereq: 'Live kernel verification · precompiled'
  }
]

export const lessons: Lesson[] = [
  // ===========================================================================
  // TRACK 1: TREE CALCULUS -> ALGORITHMS
  // ===========================================================================
  {
    id: 't1-leaf',
    track: 'trees',
    number: 1,
    title: 'One building block: the leaf',
    tagline: 'Leaves, stems, forks — and how application grows trees',
    verifyTypes: false,
    body: [
      'Everything in disp — numbers, strings, lists, and functions — is a binary tree built from one symbol: the leaf `t`.',
      'Applying `t` to a tree `x` builds a stem `t(x)` (rule 1). Applying a stem `t(a)` to `b` builds a fork `t(a, b)` (rule 2). Put your cursor on `my_fork` and step the tree view on the right.'
    ],
    concept: 'Normal trees come in three shapes: leaf `t`, stem `t(x)`, and fork `t(a, b)`.',
    starter: `// Lesson 1: Leaves, stems, and forks
// Put your cursor on any line to see its tree on the right!
open use raw "../prelude.disp" {}

leaf := t
stem := t(t)
fork := t(t, t(t))

// Build a fork whose left child is a stem t(t) and right child is a leaf t.
// Edit the line below so the test passes:
my_fork := t

test my_fork = t(t(t), t)
`,
    solution: `// Lesson 1: Leaves, stems, and forks
// Put your cursor on any line to see its tree on the right!
open use raw "../prelude.disp" {}

leaf := t
stem := t(t)
fork := t(t, t(t))

// Build a fork whose left child is a stem t(t) and right child is a leaf t.
// Edit the line below so the test passes:
my_fork := t(t(t), t)

test my_fork = t(t(t), t)
`,
    watches: [
      { label: 'Inspect fork', target: { lineMatch: 'fork := t(t, t(t))' }, note: 'See how t builds a stem then a fork' },
      { label: 'Inspect my_fork', target: { lineMatch: 'my_fork :=' } }
    ],
    goal: {
      prompt: 'Edit `my_fork` so it equals `t(t(t), t)` and the test turns green.',
      hint: 'Pass two arguments to `t`: `my_fork := t(t(t), t)`.',
      requireDefs: ['my_fork'],
      minTests: 1
    }
  },
  {
    id: 't2-ks',
    track: 'trees',
    number: 2,
    title: 'Dropping and sharing: K and S',
    tagline: 'Two fork shapes that act as programs',
    verifyTypes: false,
    body: [
      'A fork `t(a, b)` applied to a third tree `x` does NOT grow deeper — it runs! Which rule fires depends on the shape of its left child `a`.',
      'Rule 3 (`K`): when `a` is a leaf, `t(t, b)(x)` drops `x` and returns `b`. Rule 4 (`S`): when `a` is a stem `t(c)`, `t(t(c), b)(x)` hands `x` to both sides: `c(x)(b(x))`. Toggle the `SK` button in the tree viewer to see these are just plain `t`-shapes.'
    ],
    concept: '`K` ignores an input (`t(t, b, x) = b`); `S` copies an input to two branches (`t(t(c), b, x) = c(x, b(x))`).',
    starter: `// Lesson 2: K drops an argument; S shares an argument across two branches
open use raw "../prelude.disp" {}

// K = t(t): applied to b then x, it builds t(t, b) then drops x
K := t(t)
test K(1, 2) = 1

// S(c, b) = t(t(c), b): applied to x, it computes c(x, b(x))
// Watch S share x across both branches in the tree viewer:
S_example := t(t(K), K)(42)
test S_example = 42

// Exercise: use K (without binders!) to define \`second\`, which ignores its
// first argument and returns its second argument.
// Hint: K(I) ignores its first arg and returns the identity I = t(t(t, t), t).
I := t(t(t, t), t)
second := K

test second(1, 2) = 2
test second(9, 4) = 4
`,
    solution: `// Lesson 2: K drops an argument; S shares an argument across two branches
open use raw "../prelude.disp" {}

// K = t(t): applied to b then x, it builds t(t, b) then drops x
K := t(t)
test K(1, 2) = 1

// S(c, b) = t(t(c), b): applied to x, it computes c(x, b(x))
// Watch S share x across both branches in the tree viewer:
S_example := t(t(K), K)(42)
test S_example = 42

// Exercise: use K (without binders!) to define \`second\`, which ignores its
// first argument and returns its second argument.
// Hint: K(I) ignores its first arg and returns the identity I = t(t(t, t), t).
I := t(t(t, t), t)
second := K(I)

test second(1, 2) = 2
test second(9, 4) = 4
`,
    watches: [
      { label: 'Step K(1, 2)', target: { lineMatch: 'test K(1, 2) = 1' }, note: 'Watch 2 fall off the tree' },
      { label: 'Step S_example', target: { lineMatch: 'S_example :=' }, note: 'Watch 42 get copied to both sides' }
    ],
    goal: {
      prompt: 'Fix `second` so `second(a, b)` drops `a` and returns `b`.',
      hint: '`K(I)` takes `a`, drops it, and returns `I`; then `I(b) = b`. Write `second := K(I)`.',
      requireDefs: ['second'],
      minTests: 4
    }
  },
  {
    id: 't3-triage',
    track: 'trees',
    number: 3,
    title: 'Reading shapes: F (triage)',
    tagline: 'The superpower tree calculus has over λ-calculus',
    verifyTypes: false,
    body: [
      'Rule 5 (`F`, called triage) fires when the left child of a fork is itself a fork: `t(t(on_leaf, on_stem), on_fork)(x)`. It inspects the shape of `x` and picks one of three branches — handing back `on_leaf`, `on_stem(u)`, or `on_fork(u, v)`.',
      'Booleans are just the two smallest trees: `true = t` (leaf) and `false = t(t)` (stem). That means branching and `not` are single triage steps — nothing extra built in.'
    ],
    concept: 'Triage lets any tree inspect any other tree’s shape (leaf, stem, or fork) by reduction alone.',
    starter: `// Lesson 3: Triage — inspecting tree shapes directly
open use raw "../prelude.disp" {}

// triage(on_leaf, on_stem, on_fork) is just t(t(on_leaf, on_stem), on_fork):
shape_of := triage("leaf", {_u} => "stem", {_l, _r} => "fork")
test shape_of(t) = "leaf"
test shape_of(t(t)) = "stem"
test shape_of(t(t, t)) = "fork"

// Programs are trees too, so shape_of can inspect itself!
test shape_of(shape_of) = "fork"

// Exercise: write \`flip\` on booleans (true = t, false = t(t)) using triage
// so flip(true) = false and flip(false) = true.
flip := {b} => triage(true, {_} => true, {_, _} => false, b)

test flip(true) = false
test flip(false) = true
`,
    solution: `// Lesson 3: Triage — inspecting tree shapes directly
open use raw "../prelude.disp" {}

// triage(on_leaf, on_stem, on_fork) is just t(t(on_leaf, on_stem), on_fork):
shape_of := triage("leaf", {_u} => "stem", {_l, _r} => "fork")
test shape_of(t) = "leaf"
test shape_of(t(t)) = "stem"
test shape_of(t(t, t)) = "fork"

// Programs are trees too, so shape_of can inspect itself!
test shape_of(shape_of) = "fork"

// Exercise: write \`flip\` on booleans (true = t, false = t(t)) using triage
// so flip(true) = false and flip(false) = true.
flip := {b} => triage(false, {_} => true, {_, _} => false, b)

test flip(true) = false
test flip(false) = true
`,
    watches: [
      { label: 'Step shape_of(t(t))', target: { lineMatch: 'test shape_of(t(t))' }, note: 'Watch the F rule match a stem' },
      { label: 'Step flip(true)', target: { lineMatch: 'test flip(true)' } }
    ],
    goal: {
      prompt: 'Fix `flip` so it maps `true` (leaf) to `false` and `false` (stem) to `true`.',
      hint: 'In `triage(on_leaf, on_stem, on_fork, b)`, return `false` for the leaf case and `true` for the stem case.',
      requireDefs: ['flip'],
      minTests: 6
    }
  },
  {
    id: 't4-binders',
    track: 'trees',
    number: 4,
    title: 'How lambdas become trees',
    tagline: 'Variables vanish at compile time via bracket abstraction',
    verifyTypes: false,
    body: [
      'You can write readable functions like `{x, y} => x` in disp, even though the runtime has NO variables or lambdas at all. The compiler peels every binder off inside-out, turning it into `S`, `K`, and `I` — which are themselves small `t`-trees.',
      'Click `swap` below and watch the **Build** tab peel off its binders, then switch to **Try it** in the tree viewer to watch symbols `f`, `x`, `y` route through the finished tree.'
    ],
    concept: 'Bracket abstraction compiles `{x} => ...` into pure `t`-trees; `{x, y} => x` compiles to the exact same tree as `t(t)`.',
    starter: `// Lesson 4: Binders compile away into pure trees
open use raw "../prelude.disp" {}

// Look at K_raw vs K_lam — identical trees after compilation!
K_raw := t(t)
K_lam := {x, y} => x
test tree_eq(K_raw, K_lam) = true

// Put your cursor on \`swap\` and step through Build vs Try it on the right:
swap := {f, x, y} => f(y, x)
test swap(K_lam, 1, 2) = 2

// Exercise: write \`compose\` taking (f, g, x) and returning f(g(x)).
compose := {f, g, x} => x

test compose(not, not, true) = true
test compose(succ, succ, 3) = 5
`,
    solution: `// Lesson 4: Binders compile away into pure trees
open use raw "../prelude.disp" {}

// Look at K_raw vs K_lam — identical trees after compilation!
K_raw := t(t)
K_lam := {x, y} => x
test tree_eq(K_raw, K_lam) = true

// Put your cursor on \`swap\` and step through Build vs Try it on the right:
swap := {f, x, y} => f(y, x)
test swap(K_lam, 1, 2) = 2

// Exercise: write \`compose\` taking (f, g, x) and returning f(g(x)).
compose := {f, g, x} => f(g(x))

test compose(not, not, true) = true
test compose(succ, succ, 3) = 5
`,
    watches: [
      { label: 'Watch swap compile & run', target: { lineMatch: 'swap := {f, x, y}' }, note: 'Compare Build and Try it in the tree viewer' },
      { label: 'Watch compose', target: { lineMatch: 'compose :=' } }
    ],
    goal: {
      prompt: 'Implement `compose := {f, g, x} => f(g(x))` so both tests pass.',
      hint: 'Apply `g` to `x` first, then apply `f` to that result: `{f, g, x} => f(g(x))`.',
      requireDefs: ['compose'],
      minTests: 4
    }
  },
  {
    id: 't5-data',
    track: 'trees',
    number: 5,
    title: 'Data as shapes: pairs, numbers, lists',
    tagline: 'Encoding structured data with forks and leaves',
    verifyTypes: false,
    body: [
      'Because every tree is a leaf, stem, or fork, standard data structures need zero special runtime support: a pair `(a, b)` is a fork `t(a, b)`, a number `n` is a chain of forks `0 = t`, `succ(k) = t(t, k)`, and a list is `[] = t`, `cons(h, tl) = t(h, tl)`.',
      'Because triage hands the fork branch BOTH children `(left, right)`, taking data apart is one reduction step.'
    ],
    concept: 'Numbers (`t(t, n)`), pairs (`t(a, b)`), and lists (`t(head, tail)`) are all plain tree shapes.',
    starter: `// Lesson 5: Pairs, natural numbers, and lists are just forks
open use raw "../prelude.disp" {}

// Numbers: 0 = t, 1 = t(t, t), 2 = t(t, t(t, t)), ...
test 0 = t
test 3 = t(t, t(t, t(t, t)))

// Lists: [] = t, [a, b] = t(a, t(b, t))
test [] = t
test [1, 2] = t(1, t(2, t))

// Accessing the first item of a pair via triage:
fst := {p} => triage(t, {c} => c, {l, _r} => l, p)
test fst(pair(10, 20)) = 10

// Exercise: write \`pred\` on natural numbers using triage so:
//   pred(0) = 0, and pred(succ(n)) = n.
// Remember: 0 is a leaf \`t\`, and succ(n) is a fork \`t(t, n)\`!
pred := {n} => triage(0, {_} => 0, {_tag, _prev} => 0, n)

test pred(0) = 0
test pred(1) = 0
test pred(5) = 4
`,
    solution: `// Lesson 5: Pairs, natural numbers, and lists are just forks
open use raw "../prelude.disp" {}

// Numbers: 0 = t, 1 = t(t, t), 2 = t(t, t(t, t)), ...
test 0 = t
test 3 = t(t, t(t, t(t, t)))

// Lists: [] = t, [a, b] = t(a, t(b, t))
test [] = t
test [1, 2] = t(1, t(2, t))

// Accessing the first item of a pair via triage:
fst := {p} => triage(t, {c} => c, {l, _r} => l, p)
test fst(pair(10, 20)) = 10

// Exercise: write \`pred\` on natural numbers using triage so:
//   pred(0) = 0, and pred(succ(n)) = n.
// Remember: 0 is a leaf \`t\`, and succ(n) is a fork \`t(t, n)\`!
pred := {n} => triage(0, {_} => 0, {_tag, prev} => prev, n)

test pred(0) = 0
test pred(1) = 0
test pred(5) = 4
`,
    watches: [
      { label: 'Inspect 3 as a tree', target: { lineMatch: 'test 3 = t(t, t(t, t(t, t)))' } },
      { label: 'Step pred(5)', target: { lineMatch: 'test pred(5) = 4' } }
    ],
    goal: {
      prompt: 'Fix `pred` so the fork branch returns `prev`, passing all tests.',
      hint: 'In the fork arm `{_tag, prev} => prev`, return `prev` (the right child of `t(t, prev)`).',
      requireDefs: ['pred'],
      minTests: 8
    }
  },
  {
    id: 't6-recursion',
    track: 'trees',
    number: 6,
    title: 'Recursion without built-in loops',
    tagline: 'Freezing self-application with `wait` and `fix`',
    verifyTypes: false,
    body: [
      'Disp evaluates arguments eagerly, so a naive loop like `f(f)` would spin forever before getting any input. The trick is `wait`: `wait(a, b)` freezes the call `a(b)` inside a fork until a third argument `c` arrives — then thaws `a(b)(c)`.',
      'From `wait` and self-application `omega := {x} => x(x)`, we get `fix` (which `rec` desugars to!). Every recursive call peels one layer off its input and stops at the leaf.'
    ],
    concept: '`wait(a, b)` delays `a(b)` as a passive fork until an argument arrives — giving eager recursion with zero VM loops.',
    starter: `// Lesson 6: Recursion from wait + fix
open use raw "../prelude.disp" {}

// wait(a, b) is frozen as a fork; handing it a 3rd arg c runs a(b)(c):
my_wait := {a, b, c} => t(t(a), t(t, c), b)
test my_wait(and, true, false) = false

// fix hands a function \`self\` so it can call itself when an argument arrives:
add := fix({self, a, b} =>
	triage(b, {_} => b, {_tag, p} => succ(self(p, b)), a))
test add(2, 3) = 5

// Exercise: define \`double\` using fix (or \`rec double := {n} => ...\`)
// so double(0) = 0 and double(succ(p)) = succ(succ(double(p))).
double := fix({self, n} =>
	triage(0, {_} => 0, {_tag, p} => p, n))

test double(0) = 0
test double(3) = 6
test double(5) = 10
`,
    solution: `// Lesson 6: Recursion from wait + fix
open use raw "../prelude.disp" {}

// wait(a, b) is frozen as a fork; handing it a 3rd arg c runs a(b)(c):
my_wait := {a, b, c} => t(t(a), t(t, c), b)
test my_wait(and, true, false) = false

// fix hands a function \`self\` so it can call itself when an argument arrives:
add := fix({self, a, b} =>
	triage(b, {_} => b, {_tag, p} => succ(self(p, b)), a))
test add(2, 3) = 5

// Exercise: define \`double\` using fix (or \`rec double := {n} => ...\`)
// so double(0) = 0 and double(succ(p)) = succ(succ(double(p))).
double := fix({self, n} =>
	triage(0, {_} => 0, {_tag, p} => succ(succ(self(p))), n))

test double(0) = 0
test double(3) = 6
test double(5) = 10
`,
    watches: [
      { label: 'Step add(2, 3)', target: { lineMatch: 'test add(2, 3) = 5' }, note: 'Watch fix thaw one step per successor' },
      { label: 'Step double(3)', target: { lineMatch: 'test double(3) = 6' } }
    ],
    goal: {
      prompt: 'Fix `double` so it doubles any natural number using `self`.',
      hint: 'In the fork arm, replace `p` with `succ(succ(self(p)))`.',
      requireDefs: ['double'],
      minTests: 5
    }
  },
  {
    id: 't7-algos',
    track: 'trees',
    number: 7,
    title: 'Real algorithms — and measuring their cost',
    tagline: 'List map, append, and O(n²) vs O(n) reverse in pure trees',
    verifyTypes: false,
    body: [
      'With `triage` and `fix`, you can write any algorithm — purely as tree rewrites! Below are `list_map`, `list_append`, and a slow quadratic `rev_slow`.',
      'Look at the orange cost bars in the Outline tab (or hover a test in the Outline): `rev_slow` rebuilds the whole list on every item, while an accumulator `rev_fast` walks it once. In Track 2, you’ll prove they always produce the same answer.'
    ],
    concept: 'Algorithms are just recursive triage loops; evaluator step counts show asymptotic cost directly.',
    starter: `// Lesson 7: Real list algorithms in pure tree calculus
open use raw "../prelude.disp" {}

cons := {h, tl} => t(h, tl)

list_map := {f} => fix({go, xs} =>
	triage([], {_} => [], {h, tl} => cons(f(h), go(tl)), xs))

list_append := fix({go, xs, ys} =>
	triage(ys, {_} => ys, {h, tl} => cons(h, go(tl, ys)), xs))

// Readable O(n^2) reverse: appends [h] onto the back at every step
rev_slow := fix({go, xs} =>
	triage([], {_} => [], {h, tl} => list_append(go(tl), [h]), xs))

test list_map(succ, [1, 2, 3]) = [2, 3, 4]
test rev_slow([1, 2, 3, 4]) = [4, 3, 2, 1]

// Exercise: finish the O(n) accumulator helper \`rev_acc\` so rev_fast
// reverses any list in a single pass!
rev_acc := fix({go, xs, acc} =>
	triage(acc, {_} => acc, {h, tl} => acc, xs))
rev_fast := {xs} => rev_acc(xs, [])

test rev_fast([1, 2, 3, 4]) = [4, 3, 2, 1]
test rev_fast([]) = []
`,
    solution: `// Lesson 7: Real list algorithms in pure tree calculus
open use raw "../prelude.disp" {}

cons := {h, tl} => t(h, tl)

list_map := {f} => fix({go, xs} =>
	triage([], {_} => [], {h, tl} => cons(f(h), go(tl)), xs))

list_append := fix({go, xs, ys} =>
	triage(ys, {_} => ys, {h, tl} => cons(h, go(tl, ys)), xs))

// Readable O(n^2) reverse: appends [h] onto the back at every step
rev_slow := fix({go, xs} =>
	triage([], {_} => [], {h, tl} => list_append(go(tl), [h]), xs))

test list_map(succ, [1, 2, 3]) = [2, 3, 4]
test rev_slow([1, 2, 3, 4]) = [4, 3, 2, 1]

// Exercise: finish the O(n) accumulator helper \`rev_acc\` so rev_fast
// reverses any list in a single pass!
rev_acc := fix({go, xs, acc} =>
	triage(acc, {_} => acc, {h, tl} => go(tl, cons(h, acc)), xs))
rev_fast := {xs} => rev_acc(xs, [])

test rev_fast([1, 2, 3, 4]) = [4, 3, 2, 1]
test rev_fast([]) = []
`,
    watches: [
      { label: 'Step rev_fast([1, 2, 3, 4])', target: { lineMatch: 'test rev_fast([1, 2, 3, 4])' }, note: 'Compare step counts in the Outline tab' },
      { label: 'Step list_map', target: { lineMatch: 'test list_map(succ' } }
    ],
    goal: {
      prompt: 'Complete `rev_acc` so `rev_fast` passes both tests in linear time.',
      hint: 'Push `h` onto the front of `acc` and recurse on `tl`: `go(tl, cons(h, acc))`.',
      requireDefs: ['rev_acc', 'rev_fast'],
      minTests: 4
    }
  },

  // ===========================================================================
  // TRACK 2: TYPE THEORY THROUGH DISP (PROMOTED EVALUATOR-MAP KERNEL + STD)
  // ===========================================================================
  {
    id: 'y1-types',
    track: 'types',
    number: 1,
    title: 'What is a type?',
    tagline: 'Types as executable contracts checked by the kernel',
    verifyTypes: true,
    body: [
      'In untyped trees, `true` and `0` are both `t`, and nothing stops you from adding `false` to `3`. A **type** is a contract (`name : Type := value`) that the kernel checks before your code is trusted.',
      'In disp, a base type like `Nat` or `Bool` is a `(check, assume)` pair written in disp itself: `check` tests whether a value fits the shape, and `assume` tells the evaluator how to reason about an unknown value of that type. Hit **Try breaking it** to watch the kernel reject a mismatch live!'
    ],
    concept: 'A type annotation `x : T := v` is verified by disp’s self-hosted kernel, not just a comment.',
    starter: `// Lesson 1: Types are checked contracts
// \`open use "../std.disp"\` loads the kernel + standard library (precompiled!).
open use "../std.disp"

// Base types check the shape of values:
answer : Nat := 42
flag : Bool := true
raw_tree : Tree := t(t(t), t)

// Fix the value below so \`count\` actually inhabits \`Nat\`:
count : Nat := false

test answer = 42
test count = 3
`,
    solution: `// Lesson 1: Types are checked contracts
// \`open use "../std.disp"\` loads the kernel + standard library (precompiled!).
open use "../std.disp"

// Base types check the shape of values:
answer : Nat := 42
flag : Bool := true
raw_tree : Tree := t(t(t), t)

// Fix the value below so \`count\` actually inhabits \`Nat\`:
count : Nat := 3

test answer = 42
test count = 3
`,
    breakIt: {
      label: 'Try breaking it: put a Bool in a Nat',
      prompt: 'See the kernel reject `bad : Nat := false` (because `false = t(t)` is a stem, not a numeral fork).',
      snippet: `open use "../std.disp"

// false is t(t) — a stem, never a valid Nat!
bad : Nat := false
`,
      expectKind: 'type-error',
      why: 'The kernel checked `bad` against `Nat`’s predicate (`NatP`) and rejected `t(t)`.'
    },
    watches: [
      { label: 'Inspect answer', target: { lineMatch: 'answer : Nat' } },
      { label: 'Inspect raw_tree', target: { lineMatch: 'raw_tree : Tree' } }
    ],
    goal: {
      prompt: 'Fix `count` so the module typechecks and both tests pass.',
      hint: 'Change `count : Nat := false` to `count : Nat := 3`.',
      requireDefs: ['count'],
      minTests: 2
    }
  },
  {
    id: 'y2-functions',
    track: 'types',
    number: 2,
    title: 'Functions: checking every input at once',
    tagline: 'Why `A -> B` catches bugs that unit tests miss',
    verifyTypes: true,
    body: [
      'Testing a function on a few inputs never proves it works for ALL inputs — there are infinitely many `Nat`s. To check `f : A -> B`, the kernel creates an abstract unknown of type `A`, runs `f` inside a guarded evaluator that splits cases symbolically, and verifies every branch returns a `B`.',
      'Try **Break it** below: `sneaky` passes `test sneaky(true) = 0`, because `true` and `0` share the leaf `t` — yet the kernel catches that `sneaky(false)` is NOT a `Nat`!'
    ],
    concept: '`A -> B` means: for EVERY valid `A`, `f(x)` terminates and produces a valid `B`.',
    starter: `// Lesson 2: Function types A -> B
open use "../std.disp"

// A checked function from Bool to Nat:
bool_to_nat : Bool -> Nat := {b} => if (b) { 1 } else { 0 }
test bool_to_nat(true) = 1
test bool_to_nat(false) = 0

// Higher-order function: takes a function (Nat -> Nat) and applies it twice!
apply_twice : (Nat -> Nat) -> Nat -> Nat := {f, x} => f(f(x))
test apply_twice(succ, 3) = 5

// Exercise: write \`inc_if\` of type Bool -> Nat -> Nat.
// If b is true, return succ(n); if b is false, return n.
inc_if : Bool -> Nat -> Nat := {b, n} => n

test inc_if(true, 4) = 5
test inc_if(false, 4) = 4
`,
    solution: `// Lesson 2: Function types A -> B
open use "../std.disp"

// A checked function from Bool to Nat:
bool_to_nat : Bool -> Nat := {b} => if (b) { 1 } else { 0 }
test bool_to_nat(true) = 1
test bool_to_nat(false) = 0

// Higher-order function: takes a function (Nat -> Nat) and applies it twice!
apply_twice : (Nat -> Nat) -> Nat -> Nat := {f, x} => f(f(x))
test apply_twice(succ, 3) = 5

// Exercise: write \`inc_if\` of type Bool -> Nat -> Nat.
// If b is true, return succ(n); if b is false, return n.
inc_if : Bool -> Nat -> Nat := {b, n} => if (b) { succ(n) } else { n }

test inc_if(true, 4) = 5
test inc_if(false, 4) = 4
`,
    breakIt: {
      label: 'Try breaking it: a bug unit tests miss',
      prompt: '`sneaky(true)` passes its test (`0`), yet the kernel catches `sneaky(false)`.',
      snippet: `open use "../std.disp"

// true == 0 (both are leaf t), so this test passes —
// but on false = t(t), sneaky returns a non-Nat!
sneaky : Bool -> Nat := {b} => b
test sneaky(true) = 0
`,
      expectKind: 'type-error',
      why: 'The kernel split the symbolic `Bool` into both `true` and `false` and caught `false` failing `Nat`.'
    },
    watches: [
      { label: 'Step apply_twice(succ, 3)', target: { lineMatch: 'test apply_twice(succ, 3)' } },
      { label: 'Step inc_if(true, 4)', target: { lineMatch: 'test inc_if(true, 4)' } }
    ],
    goal: {
      prompt: 'Implement `inc_if : Bool -> Nat -> Nat` so it typechecks and passes both tests.',
      hint: 'Use `if (b) { succ(n) } else { n }`.',
      requireDefs: ['inc_if'],
      minTests: 5
    }
  },
  {
    id: 'y3-sums-products',
    track: 'types',
    number: 3,
    title: 'Products & Sums: Pair, Option, Result',
    tagline: 'Combining types with AND (pairs) and OR (tagged sums)',
    verifyTypes: true,
    body: [
      'Type theory builds rich data from two dual operations: **products** (`Pair(A, B)` = an `A` AND a `B`) and **sums** (`Option(A)` = `Some(a)` OR `None`; `Result(T, E)` = `Ok(v)` OR `Err(e)`).',
      'Disp’s standard library ([lib/std.disp]) gives `Option`, `Result`, `Nat`, and `List` Rust-style dot methods (`opt.map(...)`, `opt.unwrap_or(...)`). Notice `Nat.checked_sub(2, 5)` returns `None` instead of crashing or underflowing!'
    ],
    concept: 'Products bundle values together (`Pair`); sums model explicit alternatives (`Option`, `Result`) without nulls or exceptions.',
    starter: `// Lesson 3: Products (Pair) and Sums (Option, Result)
open use "../std.disp"

// Product: a Pair(Nat, Bool) holds BOTH a Nat and a Bool
tagged : Pair(Nat, Bool) := pair(7, true)
test tagged.fst = 7

// Sum: Option(Nat) is either Some(n) or None
ok_sub : Option(Nat) := Nat.checked_sub(5, 2)
no_sub : Option(Nat) := Nat.checked_sub(2, 5)
test ok_sub = Some(3)
test no_sub = None

// Chain transformations safely with Rust-style dot methods:
bumped : Nat := ok_sub.map(succ).unwrap_or(0)
test bumped = 4

// Exercise: write \`bump_or_zero\` taking an Option(Nat), adding 2 if Some
// using \`.map({n} => succ(succ(n)))\`, and returning 0 if None via \`.unwrap_or(0)\`.
bump_or_zero : {opt : Option(Nat)} -> Nat :=> 0

test bump_or_zero(Some(3)) = 5
test bump_or_zero(None) = 0
test bump_or_zero(Nat.checked_sub(5, 2)) = 5
`,
    solution: `// Lesson 3: Products (Pair) and Sums (Option, Result)
open use "../std.disp"

// Product: a Pair(Nat, Bool) holds BOTH a Nat and a Bool
tagged : Pair(Nat, Bool) := pair(7, true)
test tagged.fst = 7

// Sum: Option(Nat) is either Some(n) or None
ok_sub : Option(Nat) := Nat.checked_sub(5, 2)
no_sub : Option(Nat) := Nat.checked_sub(2, 5)
test ok_sub = Some(3)
test no_sub = None

// Chain transformations safely with Rust-style dot methods:
bumped : Nat := ok_sub.map(succ).unwrap_or(0)
test bumped = 4

// Exercise: write \`bump_or_zero\` taking an Option(Nat), adding 2 if Some
// using \`.map({n} => succ(succ(n)))\`, and returning 0 if None via \`.unwrap_or(0)\`.
bump_or_zero : {opt : Option(Nat)} -> Nat :=>
	opt.map({n} => succ(succ(n))).unwrap_or(0)

test bump_or_zero(Some(3)) = 5
test bump_or_zero(None) = 0
test bump_or_zero(Nat.checked_sub(5, 2)) = 5
`,
    breakIt: {
      label: 'Try breaking it: return wrong branch type',
      prompt: 'Returning `false` in the `None` fallback violates `Nat`.',
      snippet: `open use "../std.disp"

// unwrap_or expects a Nat fallback, not a Bool!
bad_fallback : Option(Nat) -> Nat := {opt} =>
	if (opt.is_some) { opt.snd } else { false }
`,
      expectKind: 'type-error',
      why: 'The kernel checks BOTH branches of `Option` (`Some` and `None`) and catches `false` failing `Nat`.'
    },
    watches: [
      { label: 'Step bumped', target: { lineMatch: 'bumped : Nat' } },
      { label: 'Step bump_or_zero(Some(3))', target: { lineMatch: 'test bump_or_zero(Some(3))' } }
    ],
    goal: {
      prompt: 'Implement `bump_or_zero` using `.map({n} => succ(succ(n)))` and `.unwrap_or(0)`.',
      hint: '`opt.map({n} => succ(succ(n))).unwrap_or(0)`.',
      requireDefs: ['bump_or_zero'],
      minTests: 7
    }
  },
  {
    id: 'y4-totality',
    track: 'types',
    number: 4,
    title: 'Totality & structural recursion',
    tagline: 'Why type theory insists functions terminate',
    verifyTypes: true,
    body: [
      'In a proof-capable type system, an infinite loop is dangerous: if a function never finishes, it could claim to return ANY type (even `Empty` — a contradiction!). So the kernel requires recursive functions to be **structurally decreasing**.',
      'When you recurse on `nat_pred(n)` (a strictly smaller sub-tree of `n`), the kernel accepts it by induction! Hit **Try breaking it** to see a non-decreasing loop get rejected.'
    ],
    concept: 'Structural recursion on smaller sub-trees (`nat_pred(n)`, `xs.tail`) guarantees termination and logical soundness.',
    starter: `// Lesson 4: Termination & structural recursion
open use "../std.disp"

// Structurally decreasing on n (calls itself on nat_pred(n) < n): accepted!
rec is_even : Nat -> Bool := {n} =>
	if (is_zero(n)) { true }
	else { not(is_even(nat_pred(n))) }

test is_even(4) = true
test is_even(3) = false

// Exercise: write \`triple : Nat -> Nat\` multiplying a Nat by 3
// using structural recursion on \`nat_pred(n)\`.
rec triple : Nat -> Nat := {n} =>
	if (is_zero(n)) { 0 }
	else { n }

test triple(0) = 0
test triple(2) = 6
test triple(4) = 12
`,
    solution: `// Lesson 4: Termination & structural recursion
open use "../std.disp"

// Structurally decreasing on n (calls itself on nat_pred(n) < n): accepted!
rec is_even : Nat -> Bool := {n} =>
	if (is_zero(n)) { true }
	else { not(is_even(nat_pred(n))) }

test is_even(4) = true
test is_even(3) = false

// Exercise: write \`triple : Nat -> Nat\` multiplying a Nat by 3
// using structural recursion on \`nat_pred(n)\`.
rec triple : Nat -> Nat := {n} =>
	if (is_zero(n)) { 0 }
	else { succ(succ(succ(triple(nat_pred(n))))) }

test triple(0) = 0
test triple(2) = 6
test triple(4) = 12
`,
    breakIt: {
      label: 'Try breaking it: non-terminating recursion',
      prompt: 'Recursing on `n` instead of `nat_pred(n)` never gets smaller — watch the kernel refuse it!',
      snippet: `open use "../std.disp"

// Not structurally decreasing: calls loop(n) on the SAME n forever!
rec loop : Nat -> Nat := {n} =>
	if (is_zero(n)) { 0 } else { loop(n) }
`,
      expectKind: 'type-error',
      why: 'The kernel’s induction hypothesis only allows calls on strictly smaller sub-trees (`nat_pred(n)`), so `loop(n)` is rejected.'
    },
    watches: [
      { label: 'Step is_even(3)', target: { lineMatch: 'test is_even(3)' } },
      { label: 'Step triple(2)', target: { lineMatch: 'test triple(2)' } }
    ],
    goal: {
      prompt: 'Finish `triple : Nat -> Nat` so it typechecks and passes all tests.',
      hint: 'In the `else` branch, return `succ(succ(succ(triple(nat_pred(n)))))`.',
      requireDefs: ['triple'],
      minTests: 5
    }
  },
  {
    id: 'y5-dependent',
    track: 'types',
    number: 5,
    title: 'Dependent types: Pi, Sigma & Refine',
    tagline: 'Types that depend on runtime values',
    verifyTypes: true,
    body: [
      'Ordinary types can’t talk about values. **Dependent types** let a type mention a value directly: a dependent function `{n : Nat} -> ...` (`Pi`) can return DIFFERENT types for different values of `n`, and a dependent pair `Sigma(A, {x} => B(x))` bundles a value `x` with a second value whose type depends on `x`.',
      '`Refine(Nat, pred)` goes even further — it carves out a subtype of values satisfying any computable predicate (like positive numbers or even numbers)!'
    ],
    concept: '`Pi` lets output types depend on input values; `Sigma` & `Refine` bundle values with invariants.',
    starter: `// Lesson 5: Dependent types — Pi, Sigma, and Refine
open use "../std.disp"

// Dependent function (Pi): returns Unit when n = 0, and Nat when n > 0!
pick : {n : Nat} -> if (is_zero(n)) { Unit } else { Nat } := {n} =>
	if (is_zero(n)) { t } else { n }
test pick(0) = t
test pick(3) = 3

// Refinement type: a Nat that is guaranteed to be non-zero (> 0)
PosNat : Type := Refine(Nat, is_succ)
one_pos : PosNat := 1
test one_pos = 1

// Exercise: write \`inc_pos : Nat -> PosNat\` that turns ANY Nat into a PosNat!
inc_pos : Nat -> PosNat := {n} => n

test inc_pos(0) = 1
test inc_pos(4) = 5
`,
    solution: `// Lesson 5: Dependent types — Pi, Sigma, and Refine
open use "../std.disp"

// Dependent function (Pi): returns Unit when n = 0, and Nat when n > 0!
pick : {n : Nat} -> if (is_zero(n)) { Unit } else { Nat } := {n} =>
	if (is_zero(n)) { t } else { n }
test pick(0) = t
test pick(3) = 3

// Refinement type: a Nat that is guaranteed to be non-zero (> 0)
PosNat : Type := Refine(Nat, is_succ)
one_pos : PosNat := 1
test one_pos = 1

// Exercise: write \`inc_pos : Nat -> PosNat\` that turns ANY Nat into a PosNat!
inc_pos : Nat -> PosNat := {n} => succ(n)

test inc_pos(0) = 1
test inc_pos(4) = 5
`,
    breakIt: {
      label: 'Try breaking it: violate a refinement',
      prompt: '`0` is a valid `Nat`, but fails `Refine(Nat, is_succ)` (`PosNat`).',
      snippet: `open use "../std.disp"

PosNat : Type := Refine(Nat, is_succ)

// 0 is a leaf, so is_succ(0) is false — rejected!
bad_pos : PosNat := 0
`,
      expectKind: 'type-error',
      why: '`Refine(Nat, is_succ)` runs the predicate `is_succ` on the value; `is_succ(0)` is `false`, so the kernel rejects it.'
    },
    watches: [
      { label: 'Step pick(3)', target: { lineMatch: 'test pick(3) = 3' } },
      { label: 'Step inc_pos(4)', target: { lineMatch: 'test inc_pos(4) = 5' } }
    ],
    goal: {
      prompt: 'Fix `inc_pos` so it returns a guaranteed `PosNat` (`succ(n)`) and passes the kernel check.',
      hint: 'Change `{n} => n` to `{n} => succ(n)` — `succ(n)` is always a fork, so `is_succ` holds for every `n`.',
      requireDefs: ['inc_pos'],
      minTests: 5
    }
  },
  {
    id: 'y6-polymorphism',
    track: 'types',
    number: 6,
    title: 'Polymorphism & parametricity',
    tagline: 'One function for every type — and why it cannot cheat',
    verifyTypes: true,
    body: [
      'A **polymorphic** function works uniformly for ANY type `A : Type` — written `{A : Type} -> A -> A` (explicit) or `Implicit({A} => A -> A)` (erased at runtime, so callers don’t pass `A`).',
      'Because `A` is completely opaque to the evaluator, a polymorphic function CANNOT inspect `x`’s shape — it can only move `x` around! Hit **Try breaking it** to see the kernel enforce parametricity when a function tries to peek inside an unknown `A`.'
    ],
    concept: 'Parametric polymorphism (`{A : Type} -> ...` / `Implicit`) hides representation — functions must treat unknown types abstractly.',
    starter: `// Lesson 6: Polymorphism (Forall & Implicit) and parametricity
open use "../std.disp"

// Explicit type argument: caller passes A
id_explicit : {A : Type} -> A -> A := {_A, x} => x
test id_explicit(Nat, 7) = 7
test id_explicit(Bool, false) = false

// Implicit type argument: erased at runtime, caller only passes values!
first_of : Implicit({A} => Implicit({B} => Pair(A, B) -> A)) := {p} => p.fst
test first_of(pair(10, true)) = 10

// Standard library \`List.map\` and \`Option.map\` are polymorphic too:
test [1, 2, 3].map(succ) = [2, 3, 4]

// Exercise: write polymorphic \`swap_pair\` swapping a Pair(A, B) into Pair(B, A).
swap_pair : Implicit({A} => Implicit({B} => Pair(A, B) -> Pair(B, A))) := {p} =>
	pair(p.fst, p.snd)

test swap_pair(pair(1, true)) = pair(true, 1)
`,
    solution: `// Lesson 6: Polymorphism (Forall & Implicit) and parametricity
open use "../std.disp"

// Explicit type argument: caller passes A
id_explicit : {A : Type} -> A -> A := {_A, x} => x
test id_explicit(Nat, 7) = 7
test id_explicit(Bool, false) = false

// Implicit type argument: erased at runtime, caller only passes values!
first_of : Implicit({A} => Implicit({B} => Pair(A, B) -> A)) := {p} => p.fst
test first_of(pair(10, true)) = 10

// Standard library \`List.map\` and \`Option.map\` are polymorphic too:
test [1, 2, 3].map(succ) = [2, 3, 4]

// Exercise: write polymorphic \`swap_pair\` swapping a Pair(A, B) into Pair(B, A).
swap_pair : Implicit({A} => Implicit({B} => Pair(A, B) -> Pair(B, A))) := {p} =>
	pair(p.snd, p.fst)

test swap_pair(pair(1, true)) = pair(true, 1)
`,
    breakIt: {
      label: 'Try breaking it: peek at an abstract type',
      prompt: 'Trying to call `is_fork(x)` on an unknown `A : Type` breaks parametricity — rejected!',
      snippet: `open use "../std.disp"

// An unknown type A gives NO shape-splitting rule: peeking at x is forbidden!
peek : {A : Type} -> A -> Bool := {_A, x} => is_fork(x)
`,
      expectKind: 'type-error',
      why: 'For an abstract `A : Type`, `x` is an opaque token that cannot be triaged — so `is_fork(x)` is rejected.'
    },
    watches: [
      { label: 'Step swap_pair', target: { lineMatch: 'test swap_pair(pair(1, true))' } },
      { label: 'Step [1, 2, 3].map(succ)', target: { lineMatch: 'test [1, 2, 3].map(succ)' } }
    ],
    goal: {
      prompt: 'Fix `swap_pair` so it swaps `(p.snd, p.fst)`, typechecks, and passes the test.',
      hint: 'Return `pair(p.snd, p.fst)` — notice the kernel rejects `pair(p.fst, p.snd)` because `A` and `B` are distinct unknown types!',
      requireDefs: ['swap_pair'],
      minTests: 5
    }
  },
  {
    id: 'y7-proofs',
    track: 'types',
    number: 7,
    title: 'Propositions as types: Eq & induction',
    tagline: 'A proof is just a checked program; induction is just recursion',
    verifyTypes: true,
    body: [
      'Here is the famous Curry–Howard idea in one line: a mathematical statement is a **type**, and a proof is a **value** of that type! In disp, `Eq(a, b)` is the type of proofs that `a` and `b` reduce to the exact same tree — and its proof value is just the leaf `t`.',
      'To prove a fact for ALL `n : Nat` (like `Nat.add(n, 0) == n`), you write a recursive function `{n} => ...`: base case `0` checks by `t`, and the recursive call on `nat_pred(n)` supplies the induction hypothesis!'
    ],
    concept: 'Statements are types (`{n : Nat} -> Eq(lhs, rhs)`), reflexivity is `t`, and proof by induction is structural recursion.',
    starter: `// Lesson 7: Propositions as types — equality proofs and induction
open use "../std.disp"

// Concrete equality: both sides reduce to 4, so the leaf \`t\` proves it!
two_plus_two : Eq(Nat.add(2, 2), 4) := t

// Finite case split: Bool has only two cases, so the kernel checks both automatically!
not_not : {b : Bool} -> Eq(not(not(b)), b) := {_b} => t

// Left identity: Nat.add(0, n) reduces to n immediately by definition!
add_zero_left : {n : Nat} -> Eq(Nat.add(0, n), n) := {_n} => t

// Exercise: Right identity \`Nat.add(n, 0) == n\` needs induction on n!
// Base case (n = 0): holds by \`t\`.
// Step case (succ(p)): replace \`else { t }\` with a recursive call on \`nat_pred(n)\`
// to supply the induction hypothesis!
rec add_zero_right : {n : Nat} -> Eq(Nat.add(n, 0), n) := {n} =>
	if (is_zero(n)) { t }
	else { t }

test add_zero_right(5) = t
test not_not(false) = t
`,
    solution: `// Lesson 7: Propositions as types — equality proofs and induction
open use "../std.disp"

// Concrete equality: both sides reduce to 4, so the leaf \`t\` proves it!
two_plus_two : Eq(Nat.add(2, 2), 4) := t

// Finite case split: Bool has only two cases, so the kernel checks both automatically!
not_not : {b : Bool} -> Eq(not(not(b)), b) := {_b} => t

// Left identity: Nat.add(0, n) reduces to n immediately by definition!
add_zero_left : {n : Nat} -> Eq(Nat.add(0, n), n) := {_n} => t

// Exercise: Right identity \`Nat.add(n, 0) == n\` needs induction on n!
// Base case (n = 0): holds by \`t\`.
// Step case (succ(p)): replace \`else { t }\` with a recursive call on \`nat_pred(n)\`
// to supply the induction hypothesis!
rec add_zero_right : {n : Nat} -> Eq(Nat.add(n, 0), n) := {n} =>
	if (is_zero(n)) { t }
	else { add_zero_right(nat_pred(n)) }

test add_zero_right(5) = t
test not_not(false) = t
`,
    breakIt: {
      label: 'Try breaking it: claim a false theorem',
      prompt: 'Try proving `n == succ(n)` — the kernel catches the lie immediately!',
      snippet: `open use "../std.disp"

// False statement: no Nat equals its own successor!
bogus : {n : Nat} -> Eq(n, succ(n)) := {_n} => t
`,
      expectKind: 'type-error',
      why: 'The two sides reduce to distinct shapes (`0` vs `1`), so `Eq(n, succ(n))` rejects `t`.'
    },
    watches: [
      { label: 'Step add_zero_right(5)', target: { lineMatch: 'test add_zero_right(5)' } },
      { label: 'Step not_not(false)', target: { lineMatch: 'test not_not(false)' } }
    ],
    goal: {
      prompt: 'Complete the inductive proof `add_zero_right` so the module typechecks.',
      hint: 'In the `else` branch, call `add_zero_right(nat_pred(n))` to invoke the induction hypothesis.',
      requireDefs: ['add_zero_right'],
      minTests: 2
    }
  },
  {
    id: 'y8-verified-opt',
    track: 'types',
    number: 8,
    title: 'The payoff: verified optimizations',
    tagline: 'Replace a slow spec with a fast implementation — with a proof',
    verifyTypes: true,
    body: [
      'This is where Track 1 and Track 2 come together — and the core philosophy of disp: write **simple, readable specs** first, protect them with `guard(fn_equiv_guard(...))`, and later swap in a faster implementation by supplying a machine-checked equivalence proof.',
      'Below, `my_len` starts as a readable fold over a list. Complete the inductive proof `pf_len` to unlock swapping in the fast single-triage `len_fast` — or hit **Try breaking it** to watch the guard refuse a buggy "optimization"!'
    ],
    concept: 'Guarded rebinds (`guard(fn_equiv_guard(...))`) let you optimize code in place while the kernel proves behavior never changes.',
    starter: `// Lesson 8: Verified optimizations with \`guard\`
open use "../std.disp"

// 1. Readable specification, protected by a pointwise-equivalence guard:
let rec len_spec := {xs} =>
	if (xs.is_nil) { 0 } else { succ(len_spec(xs.tail)) }

guard(fn_equiv_guard(List(Tree))) my_len : List(Tree) -> Nat := len_spec
let before := my_len([1, 2, 3, 4])

// 2. Faster single-triage implementation (avoids extra \`is_nil\` + \`tail\` calls):
let len_fast : List(Tree) -> Nat :=
	fix({go, xs} => triage(0, {_} => 0, {_h, tl} => succ(go(tl)), xs))

// 3. Exercise: complete the inductive proof that len_fast(xs) == len_spec(xs)
//    for EVERY list xs, so the guard allows the swap!
let rec pf_len := {xs} =>
	if (xs.is_nil) { t }
	else { t }

my_len := { new := len_fast; proof := pf_len }

test before = 4
test my_len([1, 2, 3, 4]) = 4
`,
    solution: `// Lesson 8: Verified optimizations with \`guard\`
open use "../std.disp"

// 1. Readable specification, protected by a pointwise-equivalence guard:
let rec len_spec := {xs} =>
	if (xs.is_nil) { 0 } else { succ(len_spec(xs.tail)) }

guard(fn_equiv_guard(List(Tree))) my_len : List(Tree) -> Nat := len_spec
let before := my_len([1, 2, 3, 4])

// 2. Faster single-triage implementation (avoids extra \`is_nil\` + \`tail\` calls):
let len_fast : List(Tree) -> Nat :=
	fix({go, xs} => triage(0, {_} => 0, {_h, tl} => succ(go(tl)), xs))

// 3. Exercise: complete the inductive proof that len_fast(xs) == len_spec(xs)
//    for EVERY list xs, so the guard allows the swap!
let rec pf_len := {xs} =>
	if (xs.is_nil) { t }
	else { pf_len(xs.tail) }

my_len := { new := len_fast; proof := pf_len }

test before = 4
test my_len([1, 2, 3, 4]) = 4
`,
    breakIt: {
      label: 'Try breaking it: sneaky buggy optimization',
      prompt: 'Try swapping in an off-by-one `len_buggy` — the guard refuses the rebind!',
      snippet: `open use "../std.disp"

let rec len_spec := {xs} =>
	if (xs.is_nil) { 0 } else { succ(len_spec(xs.tail)) }
guard(fn_equiv_guard(List(Tree))) my_len : List(Tree) -> Nat := len_spec

// Off-by-one bug: returns 0 on every list!
let len_buggy : List(Tree) -> Nat := {_xs} => 0
let rec pf_len := {xs} => if (xs.is_nil) { t } else { pf_len(xs.tail) }

my_len := { new := len_buggy; proof := pf_len }
`,
      expectKind: 'error',
      why: '`fn_equiv_guard` checked `{xs : List(Tree)} -> Eq(len_buggy(xs), len_spec(xs))` and rejected the rebind (`proof rejected`).'
    },
    watches: [
      { label: 'Step my_len([1, 2, 3, 4])', target: { lineMatch: 'test my_len([1, 2, 3, 4]) = 4' }, note: 'Compare step counts of `before` vs `my_len` in the Outline!' }
    ],
    goal: {
      prompt: 'Complete `pf_len` (`else { pf_len(xs.tail) }`) so the guard accepts `len_fast` and all tests pass.',
      hint: 'In the `else` branch of `pf_len`, recurse on the tail: `pf_len(xs.tail)`.',
      requireDefs: ['my_len'],
      minTests: 2
    }
  }
]

export function lessonsForTrack(track: TrackId): Lesson[] {
  return lessons.filter((l) => l.track === track)
}

export function lessonById(id: string): Lesson | undefined {
  return lessons.find((l) => l.id === id)
}
