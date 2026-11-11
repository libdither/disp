# Vine / Ivy / IVM — T6 (tjjfvi), with nilscrm and enricozb

**Repo:** https://github.com/VineLang/vine (Rust, Apache-2.0 / MIT, 459★, no releases; 379 commits on `main`: 105 in 2024, 207 in 2025, 67 in 2026; `main` last moved 2026-08-24, and an unmerged compiler rewrite on the branch `revine` was pushed 2026-11-04; verified 2026-11-11)
**Site:** https://vine.dev/docs/ · its Ivy and IVM pages are stale by [the repo's own note](https://github.com/VineLang/vine/blob/cf0c325834d5bf4c8d43aa36f7fb76faf2496988/docs/ivy_ivm/ivy_ivm.typ#L5), so the runtime facts below are read from the source
**Layers:** Vine, the language (22.9k lines of Rust) → Ivy, a text format for interaction nets (2.5k) → IVM, the runtime (3.8k); the standard library `root/` is 9.4k lines of Vine
**Clone inspected:** yes, at `cf0c325834d5` — `docs/`, `ivm/src/runtime/{interact,heap,allocator,parallel,stats,graft}.rs`, `ivy/src/{engine,translate}.rs`, `vine/src/features/fn_.rs`, `vine/src/components/specializer.rs`, `root/{util,derive,Ext}.vi`, `root/ops/flex.vi`, `root/debug/Show.vi`, `cli/src/common.rs`, `tests/snaps/` (112 `stats` files), `tests/verify/`, issue #544. Not built.

## What it is

A general-purpose language that compiles to interaction nets (graphs that rewrite wherever two
nodes meet head on, so separate parts can reduce at the same time) and runs them on a parallel
virtual machine. Vine reads like Rust: structs, enums, traits, generics, pattern matching,
loops, mutable variables, references. It compiles to Ivy, a text format for nets, and IVM
reduces them on one thread or many. With Bend 2 off interaction nets ([`bend2.md`](bend2.md)),
Vine is the one general-purpose language in this survey that still runs on them.

Its types organize programs. They don't say what a program must do: no dependent types, no
contracts, no proofs. Three features come from the nets:

- **Use counts.** By default a value
  ["must be used exactly once"](https://vine.dev/docs/features/traits/fork-drop). A type opts
  into being dropped (`?`), copied (`+`) or both (`*`) by implementing the `Drop` and `Fork`
  traits, so copying is library code. `IO` implements neither, so "there is exactly one IO
  handle" and effects are ordered by passing it along.
- **The inverse, `~T`.** A wire has two ends. Where a `T` is a value, a `~T`
  ["represents an expectation of"](https://vine.dev/docs/features/inverse) one: the hole it
  will arrive in. That gives out-parameters, and variables that flow backwards ("writing to a
  variable affects accesses on earlier lines"). The page itself calls it a niche, low-level
  feature.
- **A deterministic step count.** The runtime counts interactions, and
  ["the same program will always have the exact same interaction count, no matter what machine it is run on"](https://vine.dev/docs/ivy-ivm/statistics).

An out-parameter, from the inverse page with its comments trimmed:

```vi
fn foo(~n: ~N32) {
  n = 123;
}

let x: N32;
let e = ~x; // e: ~N32
foo(e);
x // 123
```

## The mechanism that matters to disp

**1. IVM is the runtime disp's net evaluator was modelled on.**
`research/interaction-combinator/RUST_IC_NET_DESIGN.md` settles on "option (d): IVM/Vine-style"
for `evaluators/rust-ic-net`. What it copied is still in IVM at this commit:
[a node is 16 bytes, two 64-bit atomic words](https://github.com/VineLang/vine/blob/cf0c325834d5bf4c8d43aa36f7fb76faf2496988/ivm/src/runtime/heap.rs#L10)
read and written with relaxed ordering; each worker allocates by bumping a pointer and keeps a
free list; a dispatch thread hands batches of active pairs (two nodes ready to rewrite) to the
workers. IVM also keeps two lists of active pairs,
[fast ones that allocate nothing and slow ones](https://github.com/VineLang/vine/blob/cf0c325834d5bf4c8d43aa36f7fb76faf2496988/ivm/src/runtime.rs#L37),
and drains the fast list first.

**2. One copier is enough, because code stays out of the running net.** A function compiles
to named nets, one per stage of its control flow. A *graft* node stands for one of them and is
replaced by a fresh copy only when something other than a copier meets it;
[a copier that meets a graft copies the name](https://github.com/VineLang/vine/blob/cf0c325834d5bf4c8d43aa36f7fb76faf2496988/ivm/src/runtime/interact.rs#L91),
not the body. A closure is not a node a copier walks through either: it compiles to a call
stage plus, when its type allows copying,
[a fork stage](https://github.com/VineLang/vine/blob/cf0c325834d5bf4c8d43aa36f7fb76faf2496988/vine/src/features/fn_.rs#L489)
that runs code to copy what it captured. So in safe code a copier passes through data and
never through unevaluated code, and the labels HVM4 uses to pair copiers
([`hvm4.md`](hvm4.md)) are not needed. Across the 112 committed `stats` snapshots, 3.49 billion interactions in all, a copier
passing through another node (a commute) is 0.5% of the work, and 7% in the worst program.

This is the property disp's reflective-net prototype states as "function bodies stay inert
until called, so a single copy agent is enough" (`research/linear-ic-net/nets/DESIGN.md`).
Vine ships it on a parallel runtime. The prototype has the half Vine lacks: programs that read
that frozen code.

**3. A copy is a real copy.** Nothing is shared between the two results of a fork: the docs
warn that passing a list by value "clones the whole list", and references exist to avoid it.
Vine gets sound copying by giving up sharing, and has no superposition. For disp's open
question 2 (`FOUNDATIONS.md`: can sound duplication recover enough sharing to make
superposition search affordable?) it is a working language at the "sound, no sharing" corner,
where HVM4 is the "labels, sharing" corner.

## Scorecard

| Axis | Vine | Note | Clauses |
|---|---|---|---|
| G1 Substrate | ✗ 17% (types only) | Built-in traits (`Struct`, `Enum`, `Tuple`, `Object`) let library code take any *type* apart field by field; [`Show`](https://github.com/VineLang/vine/blob/cf0c325834d5bf4c8d43aa36f7fb76faf2496988/root/debug/Show.vi#L172), `Eq`, `Ord`, `Fork` and `Drop` are derived that way. Code is out of reach: a program can't read a function's net. The checker is Rust. | ½(types only) · 0 · 0 — built-in structure traits let library code take a type apart field by field (`Show`, `Eq`, `Fork` are derived that way), never code; the checker is Rust |
| G2 Specification | ✗ 0% (no specs) | Structs, enums, traits, generics and a use count per type. Nothing states a property of a value; no proofs; equality is the `Eq` trait. | 0 · 0 · 0 — structs, enums, traits and generics with a use count per type; nothing states a property of a value, no proofs; equality is the `Eq` trait |
| G3 Trust | ✗ 0% (trust the compiler) | Trust = the Rust compiler, the net optimizer's rewrite rules and IVM. `safe` on an inline net or on a `become` swap is the author's word. Output and cost snapshots, plus five scripts under `tests/verify/` that recompute library results (floats, big numbers, regex) in Rust or Python, are the only checks. | 0 · 0 · 0 — trust is the Rust compiler, the net optimizer and IVM; `become` swaps a field-by-field copy for the raw copier on the author's `safe`, unchecked; snapshots are the only check |
| G4 Execution | ◐ 35% (net VM, parallel) | A parallel virtual machine with native numbers: 63M interactions a second on one thread and 569M on 16 in the docs' own sample, self-reported, with no comparison against C. Host functions ("extrinsics") are Rust functions registered with the runtime, returning outputs only. The CLI prints interactions, heap and time beside the run. | ½(net VM) · 0 · ½(cost report) — nets run on a parallel Rust virtual machine with native numbers, 63M interactions a second on one thread and 569M on 16 in the docs' own sample; host functions return outputs only; the CLI prints interactions, heap and time beside the run |
| G5 Search | ✗ 0% (none) | No synthesis. Trait lookup finds an implementation by type; nothing searches programs. | 0 · 0 · 0 · 0 — none; trait lookup finds an implementation by type, nothing searches programs |

## What disp could steal

- **Cost snapshots in the repo.** Every test and example has a `stats` file committed beside
  its expected output: interactions by kind, heap peak, and for 72 of the 112
  [the parallel depth](https://github.com/VineLang/vine/blob/cf0c325834d5bf4c8d43aa36f7fb76faf2496988/tests/snaps/examples/fib_repl/stats#L12).
  A change to the compiler or the library that makes any program costlier shows up in the
  diff. disp measures cost per run (`--stats`, `cold_equiv`) and compares by hand.
- **Depth as a second deterministic number.** `--breadth-first` reduces the net one round at
  a time and [counts the rounds](https://github.com/VineLang/vine/blob/cf0c325834d5bf4c8d43aa36f7fb76faf2496988/ivm/src/runtime.rs#L87),
  the longest chain of rewrites that had to wait on each other. Interactions over depth says
  how parallel a program is on any machine. `research/linear-ic-net` already reports rounds
  for its adders; Vine has it as a flag and pins it in snapshots.
- **`become`: a readable definition that names its fast form.** The copy for a tuple or
  struct is written field by field and tagged
  [`#[safe become(duplicate)]`](https://github.com/VineLang/vine/blob/cf0c325834d5bf4c8d43aa36f7fb76faf2496988/root/ops/flex.vi#L34).
  When every field's copy is already the one-node raw copier, the compiler
  [swaps in the raw copier](https://github.com/VineLang/vine/blob/cf0c325834d5bf4c8d43aa36f7fb76faf2496988/vine/src/components/specializer.rs#L243)
  for the whole struct. It is disp's `.opt.disp` overlay with a rule for when it applies (all
  the parts already swapped), so the fast path composes. In Vine the swap is asserted, not
  proven; disp would want the proof.
- **Copy and drop as traits a type can refuse.** Linear by default, with `IO` ordered by
  being uncopyable, is a small typed version of what the linear tree calculus
  (`research/linear-ic-net`) does with one rule that drops and one that copies.
- **A syntax for the other end of a wire.** disp's net prototype is written as λ-terms, so a
  program can't name where a value will arrive. `~T` is that, typed.

## Where disp differs

Vine's types are for writing programs and disp's are for judging them. Vine has no way to
state what a program should do, nothing in Vine can check Vine, and a program can't read
code. The loop disp is after (a spec, a checker's verdict, a search scored by it) has no
counterpart here, by intent: Vine is a programming language, not a prover.

On cost the two agree and then part. Both count deterministic steps. Vine prints the count
and pins it in tests; nothing consumes it. disp wants the count inside the score a search
maximizes, and wants a model of the real machine behind it.

On speed Vine is ahead. Its runtime is parallel and calls native arithmetic (30% of the
interactions in the snapshots are host calls). disp's net evaluator is a research backend,
and its everyday evaluators are tree reducers that share by hashing.

## Verdict

**The working model for disp's net runtime, and a shipped case of "frozen code, one
copier".** Nothing here bears on specs, trust or search. Risk: the ground moves. Ivy was
rewritten in spring 2026 as
[a general format for nets that mix several rule systems](https://github.com/VineLang/vine/issues/544),
the site's runtime pages still describe the old one, `main` has been quiet since August, and a
compiler rewrite sits on a branch. Anything disp copies should name a commit.

**Distance from disp's goals: closest on the G4 runtime and on deterministic cost; absent on G1–G3 and G5.**
