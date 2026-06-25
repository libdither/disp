# rust-ic-net — design

The **materialized, parallel** interaction-net evaluator for disp tree calculus — the
"strategy (2)" machine that `TC_NET_PLAN.md` defers to M2, now a sibling crate of the
renamed `rust-eager` (the strategy-(1) hash-consed reducer). Agents and ports are
**real mutable arena nodes**, active pairs sit in a **schedulable work bag**, and `δⁿ`
is a genuine **parked agent** — the structure required to distribute reduction across
threads, which the hash-consed collapse structurally cannot give.

> Calculus spec: [`tc-net.typ`](tc-net.typ) (authoritative for agents + rules).
> Consumer: [`DISP_BACKPROP.typ`](DISP_BACKPROP.typ) (the optimizer this exists for).
> This doc synthesizes a 5-track deep-dive (HVM2, materialization, scheduler/GC,
> prior art, host integration); it is the input to the build, not yet built.

## 0. Scope & non-goals

- **rust-ic-net is NOT a checker backend.** It abandons hash-consing (mutable graph),
  so it loses O(1) conversion; the checker stays on `rust-eager` (the default backend).
- Its value is twofold and tied to the optimizer: **(a) parallel throughput** (Theorem 2),
  and **(b) provenance** — abandoning hash-consing is a *prerequisite* for the
  reverse-mode optimizer, because hash-consing merges equal-by-evaluation terms and
  destroys per-candidate blame attribution (DISP_BACKPROP Open-Q 7).
- It owes only the **batch-fold differential** vs `rust-eager` + the optimizer surface —
  not full `lib/tests` elaboration conformance (that's `rust-eager`'s gate, decision 7).

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

DISP_BACKPROP's pattern is "build one net with many superposed/independent subcomputations,
reduce all at once with maximal parallelism, read back a structured result." Three tiers on
the same rayon-drained bag, each with its own validation oracle:

1. **`foldMany(jobs) -> NF[]`** — parallel independent jobs, *pure tree calculus, no new
   agents*. Validatable vs rust-eager **immediately** (run each job sequentially, compare).
   Ships first; already exercises the parallel substrate.
2. **`sup(label,a,b)` / `collapse(root) -> SupTree`** — superposition search (adds the
   `sup_λ` agent; DISP_BACKPROP §4). `n^k` candidate leaves *shared* in one net (Theorem 6).
   Oracle = the Step-B enumerator (`collapse(SUP) == {candidates Step B accepts}`).
   Soundness obligation: `sup_λ` must be neutral/opaque to triage (Conjectures 1/2).
3. **`reduceWithWitness(root,labels) -> {nf, conflicts}`** — reverse-mode / provenance
   (DISP_BACKPROP §5–6). *This is why hash-consing is abandoned* (Open-Q 7: distinct-but-equal
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
- **M1 — `δⁿ` need-sharing** (still sequential): two duplicator species + demand-driven
  scheduling + wire-RC for parked `δⁿ`. **Gate:** M0 differential stays green + the
  Prop-5/Theorem-6 micro-benchmark (`not true` under strict binders: `δⁿ` dispatches the
  shared `P` once, `δˢ` twice).
- **M2 — parallelize: native + napi-rs + rayon.** Redex queue → concurrent work bag; slots
  → atomic; the exchange linker; crossbeam-epoch reclamation. **Gate:** same differential
  green on the N-API Session (now a race detector) + a `bench/` thread-sweep showing
  wall-clock scaling on a wide workload.
- **M3 — optimizer tiers** 1→3 on M2 (foldMany → sup/collapse → reduceWithWitness).
- **(deferred) GPU** — only after the CPU engine is proven *and* open-decision #1 resolves
  to binary nodes; tree-calc reduction is irregular pointer-chasing → warp divergence is a
  real risk (HVM2's own irregular workloads needed annotation to not crater).

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

## Sources
HVM2 `github.com/HigherOrderCO/HVM` (Apache-2.0) + paper `paper/HVM2` (memory layout,
exchange linker, redex bags, per-thread bump alloc, linearity-not-GC, eager-only +
single-duplicator limitations). HVM3 polarized atomic linker (Taelin gist). Vine/Ivy/IVM
`VineLang/vine` (dispatch-worker, double-buffered active pairs). inpla `inpla/inpla` (MIT;
minimal agent/equation representation, in-place rewriting, parallel-scaling baseline).
crossbeam-{deque,utils,epoch,queue}; rayon; memmap2; napi-rs. Fernández–Mackie *Weak
Reduction and GC in Interaction Nets*; *Parallel needed reduction* (arXiv:1702.06092);
Ennals–Peyton Jones *Optimistic Evaluation* (ICFP 2003); Sergey–Vytiniotis–Peyton Jones
*Cardinality Analysis* (POPL 2014). Internal: `tc-net.typ`, `TC_NET_PLAN.md` (M2 work/span
note), `DISP_BACKPROP.typ`, `research/effects-and-coeffects.typ`,
`research/MODAL_TYPES_INVESTIGATION.md`, `src/eval/types.ts`, the `rust-eager` crate + seam.
