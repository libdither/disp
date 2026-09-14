# Nock / Hoon — Urbit (Tlon, Urbit Foundation, Zorp)

**Repos:** [urbit/urbit](https://github.com/urbit/urbit) (3,618★, 366 forks, MIT, Hoon; created 2014-08-29,
pushed 2026-09-07; `pkg/arvo/sys/hoon.hoon` is 14,452 lines), [urbit/vere](https://github.com/urbit/vere)
(81★, 57 forks, MIT, C; the production runtime, split into its own repo 2022-10-24, pushed 2026-09-12),
[nockchain/nockchain](https://github.com/nockchain/nockchain) (495★, 209 forks, MIT/Apache-2.0, Rust; created
2025-04-30, pushed 2026-09-11; holds NockVM, the interpreter announced as Ares in 2023 and renamed Sword in
2024), [nockchain/jock-lang](https://github.com/nockchain/jock-lang) (55★, pushed 2026-03-12),
[urbit/benchmark](https://github.com/urbit/benchmark) (pushed 2026-09-11). Stats via the GitHub API,
2026-09-12. **Inspected:** yes, by API and raw file rather than a clone: the spec lineage
`doc/spec/nock/{13..4}.txt`, `hoon.hoon`'s `++ut`, Vere's `pkg/noun/{jets.c,jets.h,jets/tree.c,retrieve.h}`,
NockVM's design notes, the UIPs, and the benchmark suite. **Built:** yes, the prebuilt Vere 4.6 from
nixpkgs; `urbit eval` runs Nock without a ship, the 63 conformance vectors in `benchmark/norm` pass 63/63,
and the timing probes below are ours. Read: the 2016 whitepaper, the 2020 Skew proposal, and seven Urbit
Systems Technical Journal papers (free PDFs under `urbitsystems.tech/ustj/`).
**Relevance:** the only other production system built on a frozen, tiny, quotation-free reflective
combinator VM. Its jets are `.opt.disp` overlays without the license slot, its jet dashboard is disp's Q1
(intensional identity dispatching extensional claims) met in the wild, and kelvin freezing is the one
substrate bet disp rejects outright.

## What it is

**Nock 4K** is a function from a cell `[subject formula]` to a noun; a noun is a natural number or a pair
of nouns. The [definition](https://docs.urbit.org/nock/definition) fits on a page: six operators (`?` cell
test, `+` increment, `=` structural equality, `/` tree address, `#` tree edit, `*` evaluate) and twelve
opcodes. 0 is address, 1 constant, 2 eval, 3 to 5 the tests, 6 if, 7 compose, 8 push, 9 call an arm of a
core, 10 edit, 11 hint. Hints are the only side channel: a static hint is discarded, a dynamic hint `[b c]`
must be computed because it may crash, and the runtime reads the tag. The spec states what an
implementation owes: "A Nock interpreter doesn't have to use the algorithm above. It just has to get the
same result." Opcodes 6 through 11 are macros over 0 through 5
([Lindstrom-Vautrin](https://urbitsystems.tech/ustj/v02-i01/mss1.pdf): `*[a 7 b c] = *[a 2 b 1 c]`,
`*[a 8 b c] = *[a 2 [b 0 1] 1 c]`, `*[a 9 b c] = *[a 2 c 1 2 [0 1] 0 b]`, `*[a 11 [b c] d] = *[a 2 [c d] 1 0 3]`;
edit needs a 65-line formula). Versions count down in kelvins toward a freeze, and the whitepaper says
"Nock is permanently frozen and will never need updating". The
[documentary history](https://urbitsystems.tech/ustj/v02-i01/mss5.pdf) traces 13K to 4K and dates the last
change to 2018: opcode 10 was added so Vere could edit a noun in place when its refcount is one, "a change
motivated by the pragmatics of the runtime rather than theoretical or higher-level language concerns".

**Hoon** is the strict typed functional language that compiles to it. One subject serves as state, scope,
environment and argument; a core is a pair of code battery and data payload; a function call is opcode 9
on a core whose sample opcode 10 has edited. The type has seven constructors
(`%atom %cell %core %face %fork %hint %hold`), nesting is structural with four variance flavours, and wet
gates are genericity by re-checking the body at each call site. Types are erased to Nock, except that a
**vase**, a `[type noun]` pair, carries them at runtime and lets programs compile and typecheck other
programs ([Davis](https://urbitsystems.tech/ustj/v02-i01/mss4.pdf)). The checker `++ut` starts at line 8759
of `hoon.hoon`, with `mint`, `mull`, `nest` and `play` between lines 9967 and 10536; inference is forward
only (whitepaper). Two Nock interpreters written in Hoon, `mink` and `mock`, virtualize Nock inside Nock and
capture crashes and stack traces in band ([~lacnes](https://urbitsystems.tech/ustj/v02-i01/mss2.pdf)); the
runtime jets them, so "Nock proper is a special case of mock".

**Runtimes.** Vere is a C bytecode interpreter over a reference-counted single-level store, the "loom",
whose equality `u3r_sing` unifies pointers when two nouns compare equal. NockVM, in Rust, is the Ares design
(announced 2023 by the Urbit Foundation, Tlon and Zorp; renamed Sword in 2024) as it continues in Zorp's
monorepo, the `urbit/ares` repo being gone: a persistent memory arena, a two-stack allocator, and code
generation driven by subject knowledge analysis (SKA, [Afonin](https://urbitsystems.tech/ustj/v03-i01/mss1.pdf)),
a partial Nock interpreter over masked nouns that turns dynamic opcode 9 calls into direct ones. Nockchain
runs its consensus kernel on NockVM and proves one fixed Nock computation per block with a STARK; its docs
say this "does not prove every application transition". Zorp also ships Jock, a friendlier language to
Nock, `hoonc`, a shipless Hoon compiler, and `honk`, a native Rust port of the Hoon compiler held to
byte-for-byte parity with it.

**Jets are the whole trust story.** A `%fast` hint on a core registers its battery and parent chain; the
runtime keys jets on a battery hash and re-verifies the ancestry on dispatch (Vere's cold, hot and warm
state, [jetting doc](https://docs.urbit.org/nock/jetting)). The contract is one sentence: a jet "must
return the same result as evaluation according to the Nock spec, even if the Sun would die before the Nock
evaluation completed". The 2016 [whitepaper](https://media.urbit.org/whitepaper.pdf) adds that a jet "is
easily tested against the pure code. A more advanced system might use actual equivalence proofs", and that
jets "are of course the main exploit vector for both computational correctness and security intrusion".
Vere's jet record has an `ice` flag, "perfect (don't test)"; when it is off, `jets.c` runs both the jet and
the soft Nock, compares them, and bails on mismatch. All roughly 300 arm entries in `jets/tree.c` are `ice`,
explicitly or by the zero default, so that path never runs in production.

**Cost interface.** Timing is a hint that prints (`%bout`), time limits are a hint (`%jinx`), memoization
is a hint keyed on `[subject formula]` and persisted across process restarts since early 2024
([UIP-0103](https://github.com/urbit/UIPs/blob/main/UIPS/UIP-0103.md)), and detected non-termination
becomes a crash ([UIP-0123](https://github.com/urbit/UIPs/blob/main/UIPS/UIP-0123.md)). No computation can
observe its own cost: a pure function that returned it would disagree between interpreters.

**Their own retrospective** ([Blackman, 2024](https://urbitsystems.tech/ustj/v01-i01/mss0.pdf)): the 2019
bytecode interpreter let them delete "tens of thousands of lines of C code that jetted the Hoon compiler";
"the allocator and Nock function calls have been slow"; the jet dashboard "has been rewritten multiple
times"; wet cores "do not strictly guarantee parametricity"; they would redesign the types as "a more
constrained type scheme ... with de Bruijn indices"; and it was "surprising to find race conditions in a
single-threaded, purely functional operating system, but we have found and fixed plenty".

## Why disp should care

### 1. The substrate is disp's substrate, plus baked-in naturals

Nock embeds in tree calculus without tags: an atom is a chain of stems over the leaf, a cell is a fork, so
`?` is triage, `+` is one stem, `=` is `tree_eq`, and opcode 0 is a walk. A sixty-line disp interpreter
written that way passed the 63 conformance vectors (a probe, not landed). The real kernel is opcodes 0 to
5, six rules against tree calculus's three, and the difference is what is baked in: Nock fixes naturals and
pairs as data, so `+` is O(1) on machine words and bignums in every runtime, while disp encodes them and
pays for it (axis halving on unary atoms was the slow part of the probe). Nock's reflection is disp's G1
claim in production: a formula is a noun, opcode 2 is eval, and the metacircular interpreter is library
code. What Nock never did with it is build a checker. Hoon's types are compiler-internal over Hoon syntax,
and Nock code is checked only by running it. The community's own alternative-Nock papers permute or re-base
the opcodes ([Atman](https://urbitsystems.tech/ustj/v02-i01/mss6.pdf)) without citing SK, Jay, or any
combinator-calculus literature; Skew is the one text that does.

### 2. Jets are `.opt.disp` overlays, and here is how they fail

A jet is a declared extensional equivalence between a Nock core and native code. disp's `.opt.disp` overlay
is the same object with a license slot the optimizer must fill. Nock has a decade of what happens without
the slot, and every item is a check for the overlay design:

- **Registration by side effect.** Opcode 11 registers a core while returning it, so a serialized core
  loses its jet, and "if you inspect the returned jetted value inside your program, it looks like normal
  Nock" ([Skew](https://github.com/urbit/urbit/blob/skew/pkg/hs/urbit-skew/skew.md), 2020); 1,900 of the
  11,200 lines of Vere's nock directory then handled registration.
- **Intensional matching of an extensional claim.** NockVM keys jets on exact noun equality of formula and
  battery chain. Debug hints embed source positions in batteries, so a 252-line move in the compiler
  changed the identity of every battery that transits it "while staying semantically identical", and jets
  silently stopped firing. The fix defines "same Nock modulo debug/trace hints" by hand and makes it an
  opt-in dispatch mode ([design note](https://github.com/nockchain/nockchain/blob/master/docs/native-compiler/BATTERIES-MATCHES-STRUCTURAL-EQUALITY.md)).
  disp's `tree_eq` is the same kind of key.
- **Cascade loss.** One divergence anywhere in a core's ancestry fails parent matching, and "every jet
  below that core is lost, silently".
- **Mismatches that only show in traces.** The `+mink` jet does not return the stack traces its Hoon
  definition specifies ([UIP-0122](https://github.com/urbit/UIPs/blob/main/UIPS/UIP-0122.md)), a divergence
  no result comparison catches.
- **No differential check in production**, per the `ice` census above. The contrast is Blockstream's
  Simplicity, whose jets are proven equivalent to their specifications in Coq.

### 3. Cost as a value is forbidden, on purpose

GOALS.md wants a primitive that returns cost beside results. Urbit forbids exactly that, and the reason is
sound: state must replay identically on any interpreter, so a clock cannot enter a noun. The
reconciliation is the one disp already has, a deterministic cost model (`--stats` steps and `cold_equiv`)
rather than a clock. What disp lacks is Nock's runtime side of the interface: a persisted memo keyed on the
computation itself, which is what `.disp-test-cache` is, and a non-termination detector that turns a loop
into a crash.

### 4. Subject knowledge analysis is §9's partial evaluation, without the equality

SKA's lattice is a mask over the subject saying which parts are known; it propagates through every opcode,
intersects at branches, and reaches loop fixpoints by masking and re-running with a blacklist. That is "how
much of the input is fixed" as an object, the axis FOUNDATIONS §9 wants to unify with the optimizer's
search space. Two lessons. It buys about 1.7× so far, with the rest of the time in reference counting,
allocation and interpretation. And it can only *consider* a masked subject equivalent to the dynamic one;
without a semantic equality the analysis has nothing to certify, and its biggest cost is re-analysing
cycles whose cached guesses turned out wrong.

### 5. Kelvin freezing bought portability; a vector suite is the cheap half

Freezing the spec is why Nock interpreters exist in JavaScript, Forth, Go, Rust and Haskell, and why a
machine-legible suite of 63 vectors with crash cases exists (`benchmark/norm/tests.json`). disp's five
evaluators agree with each other, but no third party can run a spec-level vector file against a new
backend. The suite is the part of freezing disp can take without the freeze; GOALS.md wants to evolve the
calculus, and the history paper shows even Nock un-froze when the runtime needed an opcode.

Two probes of our own on Vere 4.6 (a loaded machine, so treat as shape, not figures): the benchmark's
hand-written decrement counts up to n in interpreted Nock at roughly 3×10⁶ to 10⁷ iterations a second; the
jetted library decrement of 10¹² returns in boot time. Jets change the complexity class, not the constant.
The Skew authors measured the 2020 runtime at 45% of Ackermann time in allocation and reference counting,
and beat it 100× on Mandelbrot and 4.5× on function dispatch with SK plus lambda lifting; the runtime has
improved since, and the core team kept 4K anyway.

## Scorecard

| Axis | Nock / Hoon | Note | Clauses |
|---|---|---|---|
| G1 Substrate | ✅ 83% (native eval) | Formulas are nouns, opcode 2 is eval, no quotation; the metacircular interpreter is library code and jetted. The only other native-reflection entry, but nothing is checked with it. | 1 · 1 · ½ — formulas are nouns and opcode 2 is eval; vases let programs typecheck programs, but nothing checks Nock with it |
| G2 Specification | ✗ 0% (structural) | Structural types with variance and wet genericity, vases for runtime typing; no dependent types, compiler-internal, and the authors would redesign it. Equality: Opcode 5 is structural; jets are asserted equivalences dispatched by intensional identity, and the dashboard has been rewritten repeatedly around that mismatch. | 0 · 0 · 0 · 0 — structural types with variance; nothing states a value property; opcode 5 is structural equality |
| G3 Trust | ◐ 50% (spec + jets) | A page of spec, a conformance suite, and a decade of independent interpreters; then an unbounded trusted surface of jets with no evidence objects and no production differential check. | 1 · 0 · ½ — a page of spec, a conformance suite, independent interpreters; jets are an unbounded trusted surface with no evidence and no production differential check |
| G4 Execution | ◐ 50% (native runtimes) | Native C and Rust runtimes ahead of disp's tree-walkers; no cost model, cost as a value forbidden by design, and Skew's dispatch critique stands. | ½ · ½ · ½ · ½ — C and Rust interpreters with jets; a page of frozen spec with a conformance suite is a modelled VM; `%bout` timing hints, cost as a value forbidden; jets are asserted equivalences dispatched on intensional identity |
| G5 Search | ✗ 0% (none) | None; `honk`'s parity policy is a differential oracle, not a search. | 0 · 0 · 0 — `honk`'s parity policy is a differential oracle, not a search |

## Where disp differs

disp evolves its calculus, keeps the checker as library code with dependent types as the target, wants an
equality that licenses rewrites and a cost that is a value; Nock freezes, keeps the checker in the
compiler, trusts jets by name, and forbids cost as a value so that state replays anywhere. Both refuse a
typed term language and both have quotation-free reflection; Nock built an operating system on that, and
disp is building a type checker on it.

Three cautions. The ecosystem is split between the Urbit Foundation's Vere line and Zorp's NockVM line, and
search results on the latter are dominated by token mining, which is not design. The Vere star count
understates its age because the runtime moved repos in 2022. The published performance comparisons are
from 2020 and our timings are from a loaded machine; the benchmark repo exists precisely because no current
cross-runtime numbers do.

## Verdict

**Substrate cousin, not competitor: the only other production system on a frozen, quotation-free,
reflective combinator VM, with disp's equality and cost problems handled socially, by freezing the spec and
trusting the jets, where disp wants proofs and measurement. Read it as the field manual for how a jet
system fails.**

**Distance from disp's goals: overlaps G1 entirely and G3's tiny-kernel discipline; the jet system is
`.opt.disp` without licenses and its failure catalogue is the useful part; zero on G5; kelvin freezing is
the one substrate bet disp rejects outright.**
