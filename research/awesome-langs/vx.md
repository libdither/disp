# Vx — Aditya Kumar (hiraditya)

**Repo:** https://github.com/vx-lang/Vx (Rust, plus C++ for its MLIR dialect; Apache-2.0 with LLVM exceptions; 266★, v0.0.5; first commit 2026-05-14, public since 2026-09-09; 3,320 commits on `main`, 3,224 of them by its author; verified 2026-11-25)
**Site:** https://vxlang.org/docs/ · the book and the blog are also in the repo under `www/`, and the links below point there at one commit
**Clone inspected:** yes, shallow, at `1ae171cf5dcc` — `README.md`, `ROADMAP.md`, `www/book/src/{contracts,guarantees,machine-files,comptime,unsafe-and-ffi,tooling,error-index}.md`, six posts in `www/blog/`, `docs/custom_transfer_contract.md`, `docs/benchmarking_policy.md`, `formal/memory_model/README.md`, `utils/campaign/BASELINE.md`, `fleet/`. Not built.

## What it is

A Rust-like systems language for machines with more than one kind of chip. One source compiles,
through MLIR, to CPUs (LLVM), NVIDIA GPUs (PTX), Apple GPUs (Metal) and the Apple Neural Engine
(CoreML). The README's pitch: it
["puts hardware topology, memory placement, and reachability directly in the type system"](https://github.com/vx-lang/Vx).
A tensor's type names the memory it lives in, as in `Tensor<f32, [4, 4], Memory::GPU_HBM>`, and
the checker rejects a read by a device that can't see that memory, a transfer with no declared
path, and a placement too big for its memory, all before code is generated. Around that sit a
borrow checker, tasks and channels, and contracts (`requires`, `ensures`, loop and type
invariants) that the compiler proves with z3 on every build.

A contract, from
[the book](https://github.com/vx-lang/Vx/blob/1ae171cf5dccb5b67791ecbac44b9791413ee921/www/book/src/contracts.md?plain=1#L119):

```rust
fn count(n : i64) -> i64
    requires n >= 0i64
    ensures return == n
{
    let mut total = 0i64;
    for i in 0i64..n invariant total == i {
        total = total + 1i64;
    }
    return total;
}
```

It is mostly one person's work, written with AI agents: the repo carries `CLAUDE.md`, `GEMINI.md`
and an `agents/` folder, and
[each of the 51 commits](https://github.com/vx-lang/Vx/blob/1ae171cf5dccb5b67791ecbac44b9791413ee921/www/blog/let-vx-handle-lean-proofs-for-you.html#L51)
that added its 18,446 lines of Lean names an AI agent as a co-author.

## The mechanism that matters to disp

**1. The type system reads a model of the hardware.** `GOALS.md` asks for "efficient deterministic
models of the base hardware" encoded into the type system. Vx ships the memory half of that. A
machine file declares a part's memory spaces, each with a capacity and a bandwidth, which space
sits inside which, and the transfer edges between them; `fleet/` holds 16 of them (A100, H100,
H200, B200, MI300X, Apple M4, multi-GPU nodes, hosts). From one, before a binary exists, the
compiler admits or rejects each placement against capacity, finds the cheapest legal route for a
transfer, and derives its cost as a roofline over the containment tree:
["Most compilers hard-code a cost model. Vx reads one."](https://github.com/vx-lang/Vx/blob/1ae171cf5dccb5b67791ecbac44b9791413ee921/www/book/src/machine-files.md?plain=1#L3)
The files cite each figure and
[mark the ones not validated against hardware](https://github.com/vx-lang/Vx/blob/1ae171cf5dccb5b67791ecbac44b9791413ee921/www/book/src/machine-files.md?plain=1#L90).
A design note on user-written transfer code splits cost the way disp splits a step count from
measured time:
[traffic is counted from the code and can't drift from it, and only time is a model](https://github.com/vx-lang/Vx/blob/1ae171cf5dccb5b67791ecbac44b9791413ee921/docs/custom_transfer_contract.md?plain=1#L283),
scored by calibration on real parts. The model covers memory and data movement only; what
instructions compute is not in it.

**2. A field report on how a solver-backed checker fails.** The trust goal rests on a premise: a
search that maximizes a checker's verdict will find any bug in it. Vx's
[post on compiling against z3](https://github.com/vx-lang/Vx/blob/1ae171cf5dccb5b67791ecbac44b9791413ee921/www/blog/what-compiling-against-z3-taught-us-about-trusting-provers.html#L45)
is five months of a team finding those bugs without a search. For ten weeks a machine without z3
passed every proof, because a CI fix made "no solver" answer "proved". About 23,000 of the 26,000
queries in the integration tests hit a sort error that z3 skipped while still answering, so facts
went missing with no sign. Worse were wrong facts: one that outlived its variable, one that
survived a loop or a call through `&mut`, one left behind by a `comptime` block. Each let z3 prove
a false contract, and the programs printed wrong answers. The post sums it up in one line:
["A wrong fact can make a false contract pass."](https://github.com/vx-lang/Vx/blob/1ae171cf5dccb5b67791ecbac44b9791413ee921/www/blog/what-compiling-against-z3-taught-us-about-trusting-provers.html#L211)
Every bug sat in the glue between the compiler and the solver; z3 gave no wrong answer.

**3. Proofs about a model of the checker, labelled as one.** The Lean development proves the
concurrency promises: a program the checker accepts has no data race, a race-free program acts as
if its tasks took turns, `Mutex`, channels and `Arc` order what they must, and neither the
scheduler nor `select` starves a task. They are proofs about a small typed language, and
[the guarantees page](https://github.com/vx-lang/Vx/blob/1ae171cf5dccb5b67791ecbac44b9791413ee921/www/book/src/guarantees.md?plain=1#L25)
says so: "The checker is a model of the checker in `vxc`." Each promise lists where it stops and
how it is checked (Lean, herd7 litmus tests, GenMC on the LLVM IR of three compiled programs,
TLC). The project makes disp's own argument against copies: a Lean proof about a second copy of a
program
["can differ from the program in ways that the proof does not see"](https://github.com/vx-lang/Vx/blob/1ae171cf5dccb5b67791ecbac44b9791413ee921/www/blog/let-vx-handle-lean-proofs-for-you.html#L146).
Its contracts avoid that by being checked on the program itself; its largest proofs don't. That
gap, between what is proved and what runs, is the one disp's bet on no quotation layer is meant
to close.

## Scorecard

| Axis | Vx | Note | Clauses |
|---|---|---|---|
| G1 Substrate | ✗ 0% (none) | Programs are not data: no reflection on code or types, and `macro_rules` match token patterns as Rust's do. `comptime` blocks run ordinary Vx while compiling. The checker is Rust. | 0 · 0 · 0 — no reflection on code or types; `comptime` runs Vx while compiling; the checker is Rust |
| G2 Specification | ◐ 50% (z3 contracts) | `requires`, `ensures`, loop and type invariants and `raw::` bounds, proved by z3 on every build (linear integer arithmetic, nonlinear when a fact needs it). Tensor shapes in types are compile-time values. No proof objects; dependent types are [an unchecked roadmap item](https://github.com/vx-lang/Vx/blob/1ae171cf5dccb5b67791ecbac44b9791413ee921/ROADMAP.md?plain=1#L25). | ½(contracts) · ½(solver-discharged) · ½(SMT-fragment) — contracts and invariants over runtime values, z3-discharged on every build; shapes are compile-time values; dependent types are an unchecked roadmap item |
| G3 Trust | ✗ 0% (trust the compiler) | Trust = `vxc` (Rust), MLIR/LLVM and z3; an `unsat` carries no certificate. The Lean proofs cover a model of the checker. `vxc --audit` lists every fact the compiler trusts without proof and every `unsafe`, `mlir!` and `extern`. | 0 · 0 · 0 — trust is `vxc`, MLIR/LLVM and z3; z3's `unsat` carries no certificate; the Lean proofs cover a model of the checker, not `vxc` |
| G4 Execution | ◐ 70% (MLIR codegen) | Native code for CPUs and GPUs; GPU loops [within 5% of llm.c's fastest CUDA kernels](https://github.com/vx-lang/Vx/blob/1ae171cf5dccb5b67791ecbac44b9791413ee921/www/blog/plain-loops-at-the-speed-of-cuda.html#L48) on one A100, self-reported. The checker reasons about a declared model of the memory hierarchy (capacity, reachability, routes, transfer cost), not about what instructions compute. `extern "C"` calls return outputs only. | 1 · ½(memory hierarchy) · ½(FFI) — native code through MLIR; GPU loops within 5% of llm.c's CUDA kernels on one A100, self-reported; the checker reads a declared model of memory and transfers, not of instructions; `extern "C"` returns outputs only |
| G5 Search | ✗ 0% (none) | No program search. The compiler finds the cheapest route for a transfer over the declared graph: a shortest path, not synthesis. | 0 · 0 · 0 · 0 — none; the compiler picks the cheapest transfer route, nothing searches programs |

## What disp could steal

- **A proof that did not run is a failed proof.** After the post,
  [a missing solver fails the build](https://github.com/vx-lang/Vx/blob/1ae171cf5dccb5b67791ecbac44b9791413ee921/www/blog/what-compiling-against-z3-taught-us-about-trusting-provers.html#L327),
  any solver error stops the compiler and prints the query, and each query is deterministic (sorted
  names, a step limit rather than a time limit), so a program gets the same answer on every
  machine. disp's `"Proved"` is a bare string (`lib/verdict.disp`), so a check that never ran
  could print the same thing as one that passed.
- **Mutation-test the checker.** Vx
  [broke its borrow checker on purpose 108 ways](https://github.com/vx-lang/Vx/blob/1ae171cf5dccb5b67791ecbac44b9791413ee921/www/blog/breaking-the-borrow-checker-on-purpose.html#L48),
  one at a time; the tests caught 93, and 11 of the 15 misses were rules no test checked. The z3
  post makes it the rule for the prover:
  [break each part on purpose and check its test fails](https://github.com/vx-lang/Vx/blob/1ae171cf5dccb5b67791ecbac44b9791413ee921/www/blog/what-compiling-against-z3-taught-us-about-trusting-provers.html#L268).
  `lib/tests/rules.test.disp` already does this for the five rule lenses; Vx's miss rate is the
  argument for doing it to `judge` and `lib/kernel/` too.
- **The trust ledger as a compiler flag.** `vxc --audit` prints
  [each fact the compiler trusts without a proof](https://github.com/vx-lang/Vx/blob/1ae171cf5dccb5b67791ecbac44b9791413ee921/www/book/src/unsafe-and-ffi.md?plain=1#L246)
  and each use of trusted code, and `--no-trusted-code NAME` makes trusted code in a dependency an
  error, so a new version of a package can't add any unnoticed. It is the axiom ledger of Thermite
  and Verity, computed by the checker instead of kept beside it.
- **A machine-file format, and citing every number in it.** When disp reaches the hardware bullet
  of `GOALS.md`, Vx's format (spaces with capacity and bandwidth, containment, edges, and a model
  that checks its own coherence before any program) is a working start. Its habit of stating the
  vendor's figure rather than one fitted to a measurement keeps the model's error in view.
- **Rules for reporting speed.** The
  [benchmarking policy](https://github.com/vx-lang/Vx/blob/1ae171cf5dccb5b67791ecbac44b9791413ee921/docs/benchmarking_policy.md?plain=1#L20)
  requires both absolute numbers, the baseline's version, the hardware, at least three runs with
  the median, fastest and slowest kept, and the rows where Vx is slower. When disp has a benchmark,
  this is how to report it.

## Where disp differs

Vx's types describe where data lives and who may touch it; disp's are for judging what a program
does. Vx states properties as contracts a solver checks, has no proof objects, and its checker is
a Rust program that no Vx program can read or call. The loop disp is after (a spec, a checker's
verdict, a search scored by it and by cost) has no counterpart: nothing in Vx searches programs,
and its cost model ranks transfer routes, not candidates.

On hardware Vx is far ahead. disp has no target model and runs on interpreters; Vx's checker
already rejects a program that doesn't fit a declared H100 and prices its data movement before it
runs. That is half of what the hardware bullet of `GOALS.md` asks for, built for people writing
GPU kernels rather than for a search.

## Verdict

**The resource half of the hardware bullet of `GOALS.md`, shipped, and the best field report in
this survey on how a solver-backed checker goes wrong.** Vx scores like Mojo on substrate, trust and search, and above
it on specs and on reasoning about the target. Risk: it moves fast (3,320 commits in six months,
mostly with agents, one main author, v0.0.5), and its speed numbers are self-reported on one
machine each. Anything disp copies should name a commit.

**Distance from disp's goals: closest on G4's target model and on the evidence behind G3's premise;
absent on G1, G3 and G5.**
