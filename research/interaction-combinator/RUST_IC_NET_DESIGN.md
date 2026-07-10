# rust-ic-net — design

The **materialized, parallel** interaction-net evaluator for disp tree calculus — the
materialized **"strategy (2)"** machine (§0), a sibling crate of the renamed `rust-eager`
(the **"strategy (1)"** hash-consed reducer). Agents and ports are
**real mutable arena nodes**, active pairs sit in a **schedulable work bag**, and `δⁿ`
is a genuine **parked agent** — the structure required to distribute reduction across
threads, which the hash-consed collapse structurally cannot give.

> Calculus spec: [`tc-net.typ`](tc-net.typ) (authoritative for agents + rules).
> Consumer: [`OPTIMIZER.typ`](../../OPTIMIZER.typ) (the optimizer this exists for).
> This doc synthesizes a 5-track deep-dive (HVM2, materialization, scheduler/GC,
> prior art, host integration). The CPU engine has since been **built through M2d**
> (see §10 for the landed milestones); read this doc for rationale, the crate for truth.

## 0. Scope & non-goals

- **rust-ic-net is NOT a checker backend.** It abandons hash-consing (mutable graph),
  so it loses O(1) conversion; the checker stays on `rust-eager` (the default backend).
- Its value is twofold and tied to the optimizer: **(a) parallel throughput** (Theorem 2),
  and **(b) provenance** — abandoning hash-consing is a *prerequisite* for the
  reverse-mode optimizer, because hash-consing merges equal-by-evaluation terms and
  destroys per-candidate blame attribution (OPTIMIZER.typ §3).
- It owes only the **batch-fold differential** vs `rust-eager` + the optimizer surface —
  not full `lib/tests` elaboration conformance (that's `rust-eager`'s gate, decision 7).

### Strategy (1) vs (2) — why this is a *separate machine*, not a flag

`tc-net.typ` fixes the *semantics* (agents, ports, active pairs); it does not mandate the
data structures. Two strategies realize it, and the choice decides which §Costs-of-`δⁿ`
obligations are even incurred:

1. **Hash-consed tree reducer — what `rust-eager` *is*.** Producers are interned nodes
   (`L`/`S`/`F`/`P`); the consumers (`A`/`T₁`/`T₂`/`δ`/`ε`) are the *control flow* of a
   reduction function, not stored agents. Then `δˢ` = returning a shared reference (it *is*
   hash-consing — free), and `δⁿ` at-most-once = a `forced` memo cell on the shared `P`.
   Sequentially this delivers `δⁿ`'s full work-sharing with **no materialized duplicators,
   no parked-agent reachability-GC, and no active-pair queue** — they collapse into
   hash-consing + memoized forcing + heap reachability, and it wins sequential reduction
   outright (§10-M2d).
2. **Materialized graph machine — what rust-ic-net *is*.** Agents and ports are real arena
   nodes; active pairs sit in a schedulable bag; `δⁿ` is a genuine parked agent on an aux
   wire. This is what **parallelism requires** (reduction can't be distributed without
   explicit, schedulable active pairs) *and* what **provenance requires** (distinct-but-equal
   subnets must stay distinct) — and it is exactly what *creates* the reachability-GC + the
   scheduler obligations (§5, §6). So this machine is justified by precisely the two things
   hash-consing structurally cannot do (§0); on raw sequential reduction it loses to
   `rust-eager`, by design.

## 1. The verdict: from-scratch Rust, reuse HVM2's *design*

HVM2 (HigherOrderCO/HVM, **Apache-2.0**) is the anchor, but **do not extend or depend on
it** — it has no abstraction seam (hardcoded `switch(rule)`, fixed 3-bit tag enum, no
trait), and its only Rust runtime is *sequential*; the parallel engines are C/CUDA. So:
**transliterate HVM2's substrate idioms into a from-scratch Rust crate** (license permits,
with attribution).

The key reframe: HVM2's hardest subsystem (optimal-λ duplication: labels/oracle) **does not
exist to skip** — HVM2 ships *one unlabeled duplicator and accepts unsoundness* on
higher-order self-cloning. Tree calculus's binderlessness (Theorem 4, no binding oracle)
means **that central HVM2 limitation simply doesn't apply to us — duplication is always
sound.** We inherit the duplicator and it becomes correct. Net simplification, not extra work.

**Study deeply, in order:** **Vine/Ivy/IVM** (`VineLang/vine`, tjjfvi — Rust, *binderless
combinators, parallel*: the closest architectural match; its dispatch-worker scheduler +
per-worker double-buffered active pairs); **HVM2** (the bump allocator, polarized atomic
linker, per-thread redex bags); **inpla** (`inpla/inpla`, MIT, C — the minimal
`{symbol_id, port[]}` + active-pair-stack representation, in-place cell reuse, and a
realistic sub-linear scaling baseline). Avoid BOHM/Lambdascope/IC machinery (labels,
delimiters, affine-var bookkeeping) — all binder bookkeeping we don't have.

## 2. Memory representation (HVM2-style)

- **Port = one tagged word.** 32-bit recommended: `[tag:4 | addr:28]`. A binary node is
  then `[u32;4]`=16 B (4/cache-line); a redex is `(u32,u32)`=`u64` (one steal-deque slot);
  every slot is an independently atomic word. `cfg`-gated `Port(u64)` is the escape hatch
  for >256M-node optimizer runs.
- **Tag = the agent kind, carried in the *referencing* port** ("kind-in-pointer"). A redex
  `(Port,Port)` names both interacting kinds with **zero memory loads** → the rule dispatch
  reads two 4-bit tags out of one `u64` and jumps. Tags: `L S F P` (producers), `E A T1 T2`
  (consumers/dispatch), `Dn Ds Eps` (sharing), `Var` (wire/substitution slot), `N`
  (recursive normalizer for full NF), + reserved (`Ref` for top-level defs).
- **Node = uniform fixed-width `[AtomicU32; k]` of auxiliary ports; the principal port is
  IMPLICIT** (it's whichever slot/redex references the node). This is HVM2's load-bearing
  trick — no per-node header/discriminant. Uniform-word beats a Rust `enum` here: stable
  word offsets for per-slot atomics, no discriminant load, in-place reuse during rewrites.
  (`rust-eager`'s `enum Node` is right for its *immutable sequential* collapse, wrong here.)
- **No global intern table.** Sharing is expressed by *wires*; duplication is an explicit
  `δ` wave. Dropping the intern table is also the single biggest *parallel-allocation* win
  (`alloc` no longer consults global shared state).

### Node width / T₁(3 aux) & T₂(4 aux) — RESOLVED: uniform atomic cells, T₁/T₂ as blocks
The deep-dive flagged this as gated by "binary nodes ⇒ GPU; T₁/T₂ are wider." A focused
verification (HVM2 + IVM/Vine + inpla source, CUDA atomics) **overturned that framing:**
- The 128-bit-atomic limit only bites a *whole-node* atomic on a >64-bit node. **No such
  op is needed.** HVM2's whole-node `node_take` is a **`relaxed` load-and-clear, not
  synchronization** — correctness rests on *ownership*, not atomicity ("the thread holding
  a redex implicitly owns both trees it contains"). The *only* cross-thread sync is a
  per-variable ≤64-bit `exchange` on a **separate** var/wire cell — which never touches the
  consumer node at all.
- So a **wide consumer (T₁/T₂) is owned-when-fired** (single-principal-port invariant ⇒
  claiming the redex claims both whole agents) and is read/cleared **per-slot**. It never
  needs a >64-bit atomic. Tree calculus does **not** hit the wall — its producers are all
  binary, and its only wide agents are owned consumers. (And on Hopper/Blackwell CC 9.x the
  128-bit wall is gone anyway; it persists only ≤ Ada / RTX 4090.)
- **The one invariant to hold: every agent has exactly ONE principal port.** Multiport
  agents would break the ownership argument — never introduce one.

**Decision — option (d): IVM/Vine-style.** Uniform **atomic cells** (one `AtomicU64` slot,
`Relaxed`; `swap` for the linker, `compare_exchange` only for the freelist head). Producers
(`L`/`S`/`F`/`P`, ≤2 aux) = **one 2-slot node** (the dense common case = the M2 bandwidth
win). Consumers **T₁/T₂ = a contiguous multi-slot block**, principal implicit, accessed
per-slot. Allocator = per-worker bump + freelist with **two size classes** (2-slot "node",
4-slot "wide"; T₁ rounds up with one spare, T₂ fits) — keeps the bump path, no inpla-style
uniform-wide waste. Per-slot ≤64-bit atomics throughout ⇒ **GPU-portable on any generation.**

This is **exactly what IVM/Vine (`VineLang/vine`, the closest existing Rust CPU-parallel IC
runtime) already does** — uniform 16-byte 2-slot atomic cells, per-slot 64-bit `Relaxed`,
bump+freelist — so option (d) is validated by a working system, not just reasoned. Rejected:
**(a) binary-chained** = HVM2's λ-encoding, which its own paper flags as a regretted *2–5×
memory overhead*, and it taxes the hottest path (dispatch); **(b) non-uniform** loses the
bump allocator + worst for coalescing; **(c) uniform-wide** (inpla) wastes 2× on the
ubiquitous binary producers and correlates with the lock-based (non-scalable) pole.
*(32-bit ports à la HVM2 — denser, 536M-node ceiling, GPU-prep — stay a later densification
of (d); start at IVM's 64-bit word for full address space + simplicity.)*

## 3. The atomic linker — exchange-based, **no CAS** (the crown jewel)

The most important reuse, and it is **agent-agnostic** (knows only `Var` substitutions),
so lift it first. HVM2/HVM3 synchronize with **one unconditional `atomic_exchange`** per
link — *never* compare-exchange. A `Var` slot doubles as a **substitution cell**:

```rust
// connect a consumer aux wire `neg` to a producer/principal `pos`
fn link(net: &Net, mut neg: Port, mut pos: Port, bag: &mut Worker<Redex>) {
    loop {
        if pos.tag() != Var {                 // two principals meet → active pair
            bag.push(pack(neg, pos)); return; // (redex DETECTED at link time, never scanned)
        }
        let prev = net.slot(pos).swap(neg.0, AcqRel);   // the ONE atomic op
        if Port(prev).tag() == EMPTY { return; }         // we won; our half waits there
        free_var(net, pos); pos = Port(prev);            // raced: take partner, relink
    }
}
```

**Why it's race-free with no lock:** every variable occurs *exactly twice*; the *second*
linker to reach a slot atomically swaps out the first's deposited port and "wins"
ownership, forming the real link/redex. Strong confluence (Theorem 2: distinct active pairs
share no agents) means we only need *memory* safety here — never result determinism, which
confluence already guarantees. The two nodes of a popped redex are **exclusively owned** by
the firing worker (each had its single principal in that redex), so it reads/overwrites/
**reuses them in place** freely; only links into *far* `Var` slots use the exchange.

## 4. Reduction kernel — the rule table

Each rule is a few `link`s + `alloc`s + `free`s; every redex is `consumer ⊗ producer`
(producers never collide). The complete rule→rewrite table for
`E A T1 T2 δˢ δⁿ ε N × {L,S,F,P}` is worked out in the materialization track (see the
deep-dive notes); the load-bearing points:

- **`E ⊗ P`** turns a suspension into an `A`-demand facing `f` (demand-then-apply).
- **Two-level dispatch = principal-port handoff:** `A⊗F` → `T₁` facing the operator's
  first child; `T₁⊗F` → `T₂` facing the third argument; `T₂⊗{L,S,F}` selects the branch.
  Level 1 picks the operation (K/S/triage), level 2 reads the data.
- **The S-rule (`T₁⊗S`) is the SOLE δ-spawner** — every other rule emits only constructors,
  `A`s, and `ε`s. So the duplicator budget ∝ S-usage, and choosing `δⁿ` *here* is the
  entire call-by-need policy (Theorem 6, at-most-once dispatch).
- **`N`** (recursive normalizer consumer) drives full NF for `dumpTernary`/`equal`
  (tc-net.typ §Full Normalization); the core is weak-head.

## 5. `δˢ` vs `δⁿ`, parked agents, and GC

`δˢ`/`δⁿ` are two tags over the same 2-aux node, sharing every constructor copy rule;
they differ only on `P`: `δˢ⊗P` copies the suspension as *syntax* (re-does work,
Prop 5); `δⁿ⊗P` **demands-before-copy** — it reuses itself with its principal facing the
demand's *result wire* (an auxiliary `A.res` slot). Because no slot holds two principals,
**no redex is enqueued — the `δⁿ` is parked**, inert, reachable only through that aux wire,
until the demand chain links a constructor into the result wire and finally fires
`δⁿ⊗value` (copying one WHNF constructor; children get fresh `δⁿ`). No oracle: duplicators
never meet principal-to-principal.

**The GC obligation this creates (and how to discharge it):** a parked `δⁿ` whose two
copies are both erased is unreachable by `ε` (ε acts at principals; the `δⁿ` is on aux
wires) → the parked demand chain strands. This is *why lazy needs GC* (and exactly the OOM
the lazy-verification experiment hit on `rust-eager`). The design:

1. **Floor:** grow-until-`dispose()` — the per-file Session drops the whole arena. Ships
   first; passes conformance with no intra-session GC.
2. **Steady state — wire reference counting (primary).** The materialized net is a **DAG**
   (object recursion/`fix` is a *tree that unfolds*, never a graph cycle), so **RC is
   complete — no cycle collector needed**. A `CachePadded<AtomicU32>` live-consumer count
   per shareable producer; inc/dec piggyback on `link`/`ε`. When `ε` erases both `δⁿ`
   outputs, the count hits 0 → free the `δⁿ` and propagate `ε` onto its parked principal
   wire. **RC gives `ε` the back-channel to reach the agent it could never reach
   principal-to-principal** — the exact, targeted fix for the leak.
3. **`crossbeam-epoch` decides *when reuse is safe*** (a worker may hold a stale `Port`
   into a just-freed slot): RC says *dead*, epoch-based reclamation says *reusable*.
4. **Tracing backstop** (mark from roots = live handles ∪ redex bag) at an allocation
   watermark — and a *validator*: in test builds assert "tracing frees ∅" (acyclic ⇒ RC
   should leave nothing), the same in-language-reference discipline as `tree_eq`.

## 6. Scheduler & allocator (crossbeam)

- **Per-worker LIFO work-stealing deque** (`crossbeam-deque`: `Worker`/`Stealer` +
  `Injector` for the seed/overflow). LIFO local push of a rule's new redexes ⇒ depth-first,
  small hot working set; idle workers steal from the *top* of victims (oldest = likely large
  independent subtrees, low contention). **rayon owns the *pool* only; never the redex
  loop** — interaction-net work is unstructured (any interaction spawns 0–4 redexes
  anywhere), so fork-join would throttle the parallelism Theorem 2 hands us free.
- **Termination:** `CachePadded<AtomicUsize>` idle count + generation handshake; done when
  all idle ∧ injector empty ∧ bags empty across a stable generation. No fairness obligation
  (confluence) — termination detection is the *only* coordination beyond linking.
- **Allocator:** per-thread bump regions + thread-local free lists (no atomics on the hot
  path; `fetch_add` a global stripe cursor only when a stripe is exhausted). **Reserve the
  node slab once** via `mmap`/overcommit (`memmap2`) — never realloc (it would invalidate
  `Port`s other workers hold). Cross-thread frees go through a per-stripe
  `crossbeam_queue::SegQueue` (or a shared pool to start).
- **Budget = interactions** (`AtomicU64`, the `tc-net.typ` unit; matches `rust-eager`),
  with the `u32::MAX` exhaustion sentinel, `panic="abort"`.

## 7. Work/span — one coeffect pair drives everything

The tension (tc-net.typ §Costs of δⁿ): `δⁿ` call-by-need is work-optimal but its demand
chain serializes (high span); `δˢ` fire-everything is low-span but wastes work and diverges
on discardable-divergent terms. Principled resolution: **speculate exactly on
definitely-demanded positions** (forced anyway ⇒ zero waste, no new divergence).

- **Mechanism:** an **"eager-safe" bit in the `Susp` tag.** STRICT ⇒ attach a `Demand` and
  seed `(E,P)` into the bag at birth ⇒ reduces in parallel. LAZY ⇒ inert until a
  `T₁/T₂/E` meets it. **The scheduler never changes — only which `(E,P)` pairs enter the
  bag.**
- **Sound floor (no types):** the reduction rules *structurally certify* some positions
  strict — `A⊗P` needs the operator's WHNF, `T₁/T₂⊗P` need the discriminant's WHNF. Seed
  STRICT on the operator spine + triage discriminant; leave the S-rule's copied arg + fork
  children LAZY; duplicators default **ω** (`δⁿ`). **This is exactly M1's policy lifted onto
  the parallel bag** — and it already extracts real parallelism ("parallelize the needed
  frontier": independent needed demands run concurrently, none wasted by Theorem 6).
- **Graded-Pi-directed (the disp advantage over HVM's untyped fire-everything):**
  strictness + usage are *type-level facts* carried on a graded Pi `(x :_{s,c} A) -> B`
  (the coeffect axes in `research/effects-and-coeffects.typ`; usage belongs on the function
  type — the bindtree-is-a-Pi insight). The elaborator lowers `s` → the STRICT/LAZY bit and
  `c` → the duplicator species (0 erase / 1 linear-no-δ / ω `δⁿ` / `δˢ` reflective-only).
  **The same usage coeffect also makes RC selective** (only ω nodes need a refcount). One
  coeffect pair drives scheduler + sharing + GC.
- **Adaptive granularity** (the lone heuristic, and Theorem 2 makes it *throughput-only,
  never correctness*): use the existing interactions counter, Optimistic-Evaluation
  speculate-measure-back-off — inline sparks that complete under a threshold.

## 8. Host integration — dual-build, differential oracle

**One crate, two targets:**
- **wasm32 (sequential)** — the **validation oracle target**: reuses the `rust-eager` seam
  verbatim (u32 handles, `(ptr<<32)|len` dump, `skipIf`-gated so toolchain-free `npm test`
  stays green). Honors decision 5 ("WASM before N-API").
- **N-API + rayon (parallel)** — the **optimizer target**: real OS threads, full crossbeam,
  `mmap` overcommit, normal `Vec`/`Arc` growth. rust-ic-net **is** decision-5's "parallelism
  is the proven need," and the optimizer calls a *fat* interface (one crossing drives
  millions of interactions), so N-API's per-call tax amortizes to nothing. Browser reach
  (WASM-threads' only edge) is irrelevant to a dev-time optimizer.

The agents + rule table are identical across targets; only the *schedule* differs (single
drain vs rayon parallel-drain). Strong confluence ⇒ a single-thread drain is *one schedule
of the same confluent net*, so **the wasm build validates the rules and the N-API build
only swaps the schedule, which cannot change the NF.**

**Minimal Session subset** (for the differential): `loadTernary`, `leaf`/`stem`/`fork`,
`apply` (= allocate `P(f,x)`, O(1), no reduction — the lazy contract the ABI already
admits), `dumpTernary` (= connect a demand, drain the bag, serialize — *the* force, where
budget is charged), `equal` (force both, structural demand-then-compare — not O(1)),
`dispose`, `canonicalHandles=false`, `stats={steps:interactions}`. Omit `recognizeNative`
(tree_eq O(1) is a checker knob — stays on rust-eager), `applyLazy` (apply is *already*
lazy), `classify` (until full conformance).

**Differential = free oracle.** `test/eval-ic-net.test.ts` is a re-point of
`eval-rust-eager.test.ts` through `sessionBatchRunner` (`src/eval/batch.ts`): same NF on the
5 named lambada programs (must match) + randomized total folds (skip-if-diverge), oracle =
disp-eager always + rust-eager net-vs-net when present. **At M2 the differential becomes a
race detector** — Theorem 2 says the NF can't change, so any disagreement is a data race.
Add net-stressing cases the existing generator misses: deep apply spines + subterms folded
into multiple positions (forces `δ`).

## 9. Optimizer interface — three tiers beyond Session

The optimizer's pattern (OPTIMIZER.typ §8) is "build one net with many superposed/independent subcomputations,
reduce all at once with maximal parallelism, read back a structured result." Three tiers on
the same rayon-drained bag, each with its own validation oracle:

1. **`foldMany(jobs) -> NF[]`** — parallel independent jobs, *pure tree calculus, no new
   agents*. Validatable vs rust-eager **immediately** (run each job sequentially, compare).
   Ships first; already exercises the parallel substrate.
2. **`sup(label,a,b)` / `collapse(root) -> SupTree`** — superposition search (adds the
   `sup_λ` agent; OPTIMIZER.typ §8). `n^k` candidate leaves *shared* in one net (Theorem 6).
   Oracle = the Step-B enumerator (`collapse(SUP) == {candidates Step B accepts}`).
   Soundness obligation: `sup_λ` must be neutral/opaque to triage (Conjectures 1/2).
3. **`reduceWithWitness(root,labels) -> {nf, conflicts}`** — reverse-mode / provenance
   (OPTIMIZER.typ §8). *This is why hash-consing is abandoned* (OPTIMIZER.typ §3: distinct-but-equal
   subterms must stay distinct to attribute blame).

## 10. Build sequencing & gates

Every gate is Theorem-2 differential vs `rust-eager` (a scheduler/GC bug changes
throughput/leaks, *never* a verdict — so build bottom-up, validate at each step).

- **M0 — sequential materialized net (wasm).** Real agents + redex queue + single-thread
  drain; minimal Session subset; grow-until-dispose. **Gate:** `build.sh`→`ic_net.wasm`;
  `eval-ic-net.test.ts` byte-identical NF to disp-eager on the 5 named + ≥15 random folds;
  `canonicalHandles===false`; **`stats` shows `agents_allocated>0 ∧ interactions>0`** (proves
  it's a real materialized net, not a recompiled `rust-eager`). *Isolates the rules +
  scheduler with zero concurrency risk — the foundation M2 stands on.*
- **M1 — `δⁿ` need-sharing** (still sequential) — *the δⁿ mechanism LANDED; wire-RC is
  the remaining piece.* The S-rule spawns `δⁿ`; δˢ/δⁿ share the constructor copy rules,
  differing only on `P` (δⁿ forces once + parks). **Gate met:** M0 differential green +
  the Prop-5/Theorem-6 micro-benchmark — a shared 12-deep suspension chain costs **δⁿ=111
  vs δˢ=327 interactions (~2.95×)**. **Key finding (refines this milestone):** δⁿ shares
  re-*reduction*, NOT *structure* — on the lambda benchmarks `δⁿ⊗P` fires **zero** times
  (`dnp=0`) because they duplicate values already reduced to *constructors* at the S-rule,
  which is a *hash-consing* matter (tc-net.typ "What Is Still Not Shared"), not δⁿ's. So
  δⁿ is a no-op there (identical counts to δˢ); its win is specifically on
  suspension-duplication.
  **M1b GC — LANDED, but it's *free-on-consume*, not refcounting.** An interaction net is
  LINEAR (each agent consumed exactly once), so the complete reclamation for the eager net
  is to free both agents of every active pair (+ resolved wire cells) to per-arity free
  lists; the demand/park rules reuse the consumer (freed later when it fires). Peak
  `nodes.len()` then tracks the LIVE working set: **silly-exp(4) 2.2M→55K, fib(8) 7.6M→64K
  (~119×), merge-sort 17.5M→38K (~460×)** — the actual usability fix. Refcounting (§5/§6)
  is *unnecessary* in eager M1: the parked-δⁿ leak it targets needs the *lazy* schedule (a
  δⁿ whose value is never demanded), but eager's forcing `A` always fires, so there is no
  unreachable garbage. **New requirement surfaced:** terms must be LINEAR — a node
  referenced twice is an ill-formed net; *sharing goes through δ, not host handle reuse*.
  `loadTernary` + the fold provide this; the optimizer must insert δ to share. **Still
  TODO (M2):** refcounting for the lazy leak + the demand schedule that makes δⁿ pay on
  more programs (the eager-safe bit).
- **M2 — parallelize. M2a/b/c LANDED + validated (commits cc4562c, 2af0d1e, 26e8e0e).**
- M2a: atomic exchange linker (vars AtomicU32, swap/AcqRel, no CAS).
- M2b: shared `Net` (atomic FIXED arena + bump cursors + atomic counters) / per-thread `Worker`, split by `Ctx{net,w}`; node cells OWNED (Relaxed), only vars cross-thread (AcqRel).
- M2c: native parallel drain — a lock-free shared crossbeam `Injector`; N workers steal/interact/flush; `in_flight`-counter termination. cfg-gated to non-wasm (crossbeam); the wasm build stays the sequential oracle. **Race-detector `parallel_matches_sequential` (1 vs 4 threads, 20×) GREEN: identical NF AND identical interaction count** — i.e. M2c is CORRECT.
- **M2c PERF — was a REGRESSION; the §6 scheduler rework LANDED (3 stages), flipping it to a speedup.** Before: the earlier "~3.1× at depth 8" claim did NOT reproduce — 2t/4t/8t were **3–30× SLOWER than the 1t sequential drain** (wide d12×c8: 1t=5ms, 8t=36ms). Cause: **per-interaction global atomics** (`budget.fetch_sub` + `interactions.fetch_add` + an **AcqRel** `in_flight` RMW, every interaction) plus the single shared `Injector` — coordination on shared cache lines ≫ the ~tens-of-ns of real work, so coherence traffic (MESI line ping-pong) serialized faster than parallelism recovered. **Rework (6-physical-core Ryzen 5 7640U, memory-bound workload):**
  - *Stage 1 — counters off the hot path:* per-worker budget **leasing** (one atomic per 64K interactions) + thread-local interaction counter (summed at join) + `in_flight` two AcqRel RMWs collapsed to one. → **~1.4–2.4× faster**, but still regressing vs 1t (injector + surviving `in_flight` dominate).
  - *Stage 2 — per-worker work-stealing deques + `active`-count quiescence (`in_flight` eliminated):* products go to the owner's LIFO deque (no atomic on the common path); idle workers steal a batch; termination is an `active` (non-idle) worker count, O(steal events) not O(interactions). → **FLIPS the sign: ~1.6–1.9× *faster* than 1t.** The injector was the dominant cost.
  - *Stage 3 — per-worker bump regions (`NODE_REGION`/`VAR_REGION`):* `alloc`/`new_var` lease a contiguous region with one atomic then bump locally — removes the per-alloc `node_top`/`var_top.fetch_add` AND the false sharing of cells across workers. → **~2.0–2.4× over 1t** (8t). `node_top` now reports cells *leased*, slightly above *used*.
  - **Net: from a ~7× regression to a ~2.4× speedup** (8t/1t), plateauing by 8 threads (6 physical cores; **memory-bandwidth bound** — the materialized net is memory-heavy and CPU bandwidth doesn't scale with logical cores; 12t ≈ 8t). Race-detector (identical NF + interaction count, 1 vs 4 threads ×20, ×5 reps) stays green throughout. **Ceiling caveat:** even at max threads ic-net still trails rust-eager-native ~1.5–1.7× on these workloads (hash-cons sharing + bandwidth), so the scheduler rework is *necessary but not sufficient* to beat rust-eager — the real parallel payoff needs either port densification (less memory traffic) or GPU (the deferred bandwidth play), and the genuine niche stays the optimizer's distinct-candidate workloads (no cross-term sharing for hash-consing to exploit).
- **M2d — native CLI binding + head-to-head benchmark. LANDED (`bench/ic-net-bench.ts`, `npm run bench:ic-net`).** napi was deferred (greenfield here; it's for M3's in-process stateful interface, not benchmarking). Instead a native `[[bin]]` per crate (`ic-net-cli`, `eager-cli`) runs each reducer as a cold subprocess, isolating the wasm-vs-native confound from parallel scaling. **Verdict: ic-net does NOT compete with rust-eager on reduction speed, by a wide margin and for a fundamental reason.** Real programs (native subprocess, best-of-3): ic-net-1t is **20–550× slower** than rust-eager-native (fib(10) 173ms vs 0.30ms; merge-sort 154ms vs 0.31ms) — the materialized net allocates per interaction and shares nothing, where rust-eager hash-conses (O(1) conversion + cross-occurrence sharing). Even on the **embarrassingly-parallel** wide workload (ic-net's best case), rust-eager *sequential* beats ic-net *sequential* ~4× (it shares the repeated `not`), and ic-net's parallel mode is ~30× worse. The wasm tax is ~1.5–2× (small vs the 100×+ design gap). **Strategic conclusion (consistent with §0): beating rust-eager on raw reduction was never ic-net's job — hash-consing wins sequential reduction decisively. ic-net's case is the OPTIMIZER substrate (§9: provenance/superposition, where hash-consing is abandoned on purpose) and parallel throughput on massively-independent workloads — and the latter needs the M2c-perf scheduler rework first.** Remaining for the optimizer path: M2c-perf rework, then napi (M3 in-process), then M3 tiers. Original sub-step text follows.

**M2 — parallelize (original plan).** *Build architecture (settled):* ONE reducer, atomics in the shared
  cells; the **wasm32 build runs it single-threaded = the oracle** (AtomicU32 compiles on
  wasm32 — confirmed; degrades to plain ops, no `+atomics` needed), the **native build runs
  it parallel** (crossbeam work-stealing + threads; napi-rs Session binding). crossbeam
  doesn't build on wasm32, so the *scheduler* is `#[cfg]`-split (wasm: sequential `Vec`
  drain; native: work-stealing) while the *rule kernel + atomic cells + linker* are shared.
  *Concurrency model:* the **node cells are owned** by the firing worker (single-principal
  invariant — no contention); the **`vars` substitution cells are the sole cross-thread
  rendezvous**, so only they need the atomic exchange (`swap`, AcqRel) — node cells publish
  via the var's release/acquire (must still be `AtomicU32`+Relaxed for Rust-soundness, but
  uncontended). *Sub-steps:* **M2a** atomic exchange linker (`vars` atomic) ← *landing now,
  single-threaded-validated* · **M2b** node cells atomic + `&self` ownership (interior
  mutability) · **M2c** native crossbeam work-stealing + N threads, validated by a cargo
  race-detector test (Theorem 2 ⇒ any NF disagreement across thread counts IS a race) ·
  **M2d** napi-rs Session binding + `bench/` thread-sweep. **Gate:** differential green on
  the parallel native Session + wall-clock scaling on a wide (independent-reduction) workload.
- **M3 — optimizer tiers** 1→3 on M2 (foldMany → sup/collapse → reduceWithWitness).
- **(deferred) GPU** — only after the CPU engine is proven *and* open-decision #1 resolves
  to binary nodes; tree-calc reduction is irregular pointer-chasing → warp divergence is a
  real risk (HVM2's own irregular workloads needed annotation to not crater).

## Fast-paths for conversion & memo (proposed): fingerprints, not merkle

*Not built — nothing in the crate references this yet; it is the plan for when ic-net needs
the two things it dropped with hash-consing:* **O(1) conversion** (`tree_eq`) and
**cross-occurrence memoization**. Recover both *approximately, with an exact fallback*, so
correctness is by construction and the approximation is a pure performance knob. (This
supersedes an earlier "NF merkle-hashing" sketch that made the hash a *verdict* — "hashes
equal ⇒ declare equal", which forces a 128-bit hash + a dense arena-sized array; the reframe
below makes it a *filter*.)

- **The principle — filter, not verdict.** A fingerprint is a function of content, so equal
  trees always produce equal fingerprints. Hence a **mismatch** is a *sound* "not equal" (no
  false negatives) and a **match** defers to an **exact** check; a collision costs one wasted
  exact check, **never a wrong answer**. Bits buy collision rate (speed), not correctness —
  32-bit, or even a node count, is sound. Same discipline as the live `tree_eq` native
  fast-path: a lossy accelerator with an exact backstop.
- **`tree_eq`, single-shot — the walk is already optimal.** `Ctx::equal` full-NFs both sides
  then walks lockstep with early-exit on the first mismatched tag (`O(min size)`, O(1) at the
  root) — it *is* a root-first cascade. A precomputed tier (size, fingerprint) must itself be
  *computed by a walk*, so it can't beat the early-exit walk one-shot; serialize-and-compare
  (`emit`→`memcmp`) is strictly *worse* (no early exit, two buffers). Leave `equal` alone
  unless profiling shows repeat conversion; the only single-shot win is interleaved WHNF
  (§12 risk-7).
- **Repeat-heavy conversion** (the checker re-comparing the same types) is the one place
  caching pays, and the cached form must be **small** and **per compared handle, never per
  arena node**: a per-handle fingerprint (O(1) reject; the exact walk still confirms a match)
  + a scoped NF intern (O(1) repeat-equal, dropped per phase). Cost scales with the type
  vocabulary compared, not arena capacity.
- **In-reduction `tree_eq` — the only *required* piece.** The kernel's conversion *is*
  in-language `tree_eq`; reducing it through the net is catastrophic (`O(size)` + budget
  blow). ic-net has no registered id, so it recognizes the operator **by content**: a
  **root-shape pre-filter** rejects ~all non-`tree_eq` operators in O(1) (the load-bearing
  step), survivors fingerprint-compare to the registered `tree_eq`, a match runs native
  `equal` and links the Scott boolean. (§8 omits `recognizeNative` for the *differential*;
  this is for when the optimizer type-checks on ic-net.)
- **memo — a bloom recurrence-gate, off by default.** Content-memo needs content-identity,
  which ic-net lacks (that *was* hash-consing) — so the key is a fingerprint of the operands,
  an intrinsic per-redex cost a bloom does *not* remove. What the bloom buys is a small,
  cache-hot memo: insert a key only on its **second** sighting, so singletons never bloat it;
  **per-worker** ⇒ parallel-healthy. **Provenance gate (non-negotiable):** memoization merges
  equal-by-evaluation terms — the blame attribution the reverse-mode optimizer needs — so it
  is **off by default**, on only for pure-reduction runs (conversion, benchmarks), validated
  by a `memo_on == memo_off` NF differential. Honest verdict: it pays a real per-redex cost
  for cross-occurrence sharing rust-eager already delivers better; its only justification is
  ic-net **self-containment**, so it ranks below `tree_eq`.
- **Storage & soundness.** **Nothing is per-arena-node** — single-shot `equal` keeps the walk
  and adds zero; the per-handle fingerprint, scoped NF intern, and per-worker bloom+memo all
  scale with *work actually compared/memoized*, so the merkle proposal's 32 MB dense array is
  avoided entirely. Every verdict still comes from pointer-eq, `memcmp`, or the structural
  walk; fingerprints only accelerate *rejection*, so no false verdict is possible at any hash
  width. Note too that neither path recovers the apply-memo / cross-occurrence *reduction*
  sharing — that **is** hash-consing, dropped on purpose (it contends under parallelism AND
  destroys provenance); this buys fast *equality*, never sequential reduction speed (rust-eager's).
- **Libraries & build order.** `rustc-hash` (FxHash — deterministic, wasm-clean, already a
  rust-eager dep) for the mixing; a hand-rolled `Vec<u64>` bloom (~30 lines, no
  `rand`/`getrandom`); the memo reuses `rust-eager/src/memo.rs`'s swap-in pattern per `Worker`.
  Order: **(1)** the `recognizeNative` root-shape pre-filter (the only *required* piece —
  unblocks kernel conversion on ic-net) → **(2)** optional interleaved-WHNF `equal` → **(3)**
  per-handle fingerprint + scoped NF intern *only if* profiled repeat-heavy → **(4)** the
  bloom-gated memo, off by default.

## 11. Open decisions (decide before / early in M0)

1. **Node width / T₁-T₂ encoding** (§2) — **RESOLVED: option (d)**, IVM/Vine-style uniform
   atomic cells with T₁/T₂ as contiguous 2-slot-class blocks, per-slot ≤64-bit atomics. The
   "binary ⇒ GPU" constraint was softer than framed: owned-when-fired consumers never need a
   whole-node atomic, so tree calculus dodges the 128-bit wall. Validated by IVM doing exactly
   this. Invariant to hold: every agent has exactly one principal port.
2. **Eager vs lazy default** — HVM2 gives eager free, lazy not at all (it punted the
   uplink scheduler). Our sound floor (§7) is structural-strict + lazy-elsewhere = M1's
   policy; confirm that's the M0/M1 target and the lazy machinery (parked `δⁿ` + RC) lands
   at M1, not M0.
3. **Linker: settled** — exchange-based, not CAS (a per-slot-CAS sketch was considered and
   rejected; HVM uses only `swap`).
4. **Threading order: settled** — wasm-sequential first (oracle), N-API/rayon at M2 (the dual
   build resolves the "wasm-first vs N-API-first" tension).

## 12. Foreseeable risks & coherency notes (post-M0 review)

Grounded in the M0 build (`evaluators/rust-ic-net/`, the differential green on the full
named lambada gate). Ordered by how much they shape the path.

1. **Observation protocol — a lazy materialized net cannot support the host's per-node
   `classify`.** This is deeper than "no hash-consing." In the net, a child's principal
   is *consumed by its parent's aux port*; to WHNF a child you must **detach** it from the
   parent, but the host's `classify(h) → {tag, child0, child1}` then `classify(child0)`
   protocol assumes `h` and its children stay independently inspectable. They don't —
   adding a second demand to a child's principal is a port conflict. M0 resolves it by
   reading back via **full-NF + structural walk** (the `N` normalizer consumes parents as
   it descends). *Consequences:* (a) the "not a checker backend" decision is forced by the
   *observation model*, not just conversion cost — the elaborator's `classify` literally
   cannot run on this substrate (confirmed: `cs.classify is not a function`); (b) the
   **optimizer must read results via `dumpTernary`/`collapse` (full readback), never lazy
   `classify`** — fold this into the tier-1–3 interface (§9).

2. **M0 is a *correctness* milestone only — δˢ + grow-until-dispose blows up.** Measured:
   `recursive-fib(8)` = 3.8M interactions / **7.6M nodes** (~30 MB); `silly-exp` is
   exponential in `n` without sharing. So **M1 (δⁿ + wire-RC) is REQUIRED for usability,
   not an optimization** — without it ic-net OOMs on any real workload (the same shape as
   the rust-eager lazy-verify OOM). It also bounds M0's **differential coverage**: rust-eager
   validates ic-net only where *both* terminate, and δˢ exhausts on heavy sharing — so full
   oracle coverage also needs δⁿ. Reframe M0's gate as "the rules + scheduler + linker are
   correct," and treat M1 as the first *usable* milestone.

3. **The atomic linker is the M2 risk — but M0 shows the port is tractable.** The exchange
   linker is only *reasoned* correct for our rule set (HVM/IVM use it, but with different
   rules). The good news from M0: **every rule already satisfies the "owned-only"
   invariant** — each `interact` touches exactly its two redex agents + freshly-allocated
   nodes + far var-cells, never a third-party node. That's precisely the property the
   lock-free argument needs, so M0's rules port to atomics structurally. Gate: the
   differential-as-race-detector under a thread sweep, plus a per-rule audit that the
   owned-only invariant holds (it does in M0 — keep it as rules are added).

4. **Wire-RC completeness rests on net acyclicity — validate, don't assume.** RC is
   complete *only* if the net is a DAG (the design's "recursion = tree-unfold, not a graph
   cycle"). This held trivially in M0 (δˢ only builds trees), but δⁿ-parking + `fix`-heavy
   programs must be checked: ship the **tracing backstop as a test-time assertion ("frees
   ∅") from M1**, and if it ever frees something, RC is leaking and tracing must become
   primary, not validator.

5. **The "sound floor extracts real parallelism" claim is workload-dependent.** A sequential
   demand spine (operator chain, bracket-abstracted binder distribution) keeps the redex
   bag at ~1 ready redex → little parallelism. The floor pays off on **independent-data**
   workloads (the optimizer's search, wide records) — which is exactly ic-net's consumer,
   so the claim is *true for the target* but should not be read as generic speedup (it
   matches the existing "distribution cost is never shared" caveat).

6. **`sup_λ` (optimizer tier 2) is the research-risky frontier.** It requires triage to
   *distribute over superposition* (a `sup_λ(a,b)` meeting a `T₁`/`T₂` must split), which
   is unproven (OPTIMIZER.typ §8/§12) and is exactly where HVM's own superposition
   carries soundness caveats. Tier 1 (`foldMany`, pure tree calculus) is safe and
   validatable vs rust-eager *now*; gate tiers 2–3 behind that conjecture.

7. **Minor coherency cleanups.** `E` (WHNF demand) is redundant with `N` for full-NF readback
   (M0 needs no `E`; demand is driven by the `A` agents that `N`/`T₁`/`T₂` spawn on `P`) —
   `E` is only for a lazy `classify`, which §risk-1 shows the net can't offer anyway, so
   drop `E` unless a WHNF-only observation appears. Conversely `N` is **essential**, not an
   "extension" (§4) — it's the readback mechanism. And M0's `equal` fully normalizes both
   sides before comparing; an interleaved WHNF-compare would short-circuit earlier (an M2
   refinement, not a correctness issue).

## Sources
HVM2 `github.com/HigherOrderCO/HVM` (Apache-2.0) + paper `paper/HVM2` (memory layout,
exchange linker, redex bags, per-thread bump alloc, linearity-not-GC, eager-only +
single-duplicator limitations). HVM3 polarized atomic linker (Taelin gist). Vine/Ivy/IVM
`VineLang/vine` (dispatch-worker, double-buffered active pairs). inpla `inpla/inpla` (MIT;
minimal agent/equation representation, in-place rewriting, parallel-scaling baseline).
crossbeam-{deque,utils,epoch,queue}; rayon; memmap2; napi-rs. Fernández–Mackie *Weak
Reduction and GC in Interaction Nets*; *Parallel needed reduction* (arXiv:1702.06092);
Ennals–Peyton Jones *Optimistic Evaluation* (ICFP 2003); Sergey–Vytiniotis–Peyton Jones
*Cardinality Analysis* (POPL 2014). M2 type-directed scheduling (the graded-Pi lever, §7):
Atkey *QTT* + McBride *I Got Plenty o' Nuttin'* (quantitative types), Petricek–Orchard–Mycroft
*coeffects*; the parallel optimal-reduction systems that hit the work/span wall — Asperti
**BOHM**, Pedicini–Quaglia **PELCR**, **Lambdascope**; Arvind–Nikhil *Id/pH* (lenient
dataflow); Blelloch–Greiner (work-span cost semantics). Internal: `tc-net.typ`,
`OPTIMIZER.typ`, `research/effects-and-coeffects.typ`,
`research/one-offs/MODAL_TYPES_INVESTIGATION.md`, `src/eval/types.ts`, the `rust-eager` crate + seam.
