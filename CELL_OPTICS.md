# CELL_OPTICS.md — addressing as optics: unifying fields, slots, and indices

**Status: design proposal (2026-06-25). Prototyped end-to-end (§1–§5); INTEGRATED & in use.** The
optic `over` + `fmap`/`bimap`/`nmap` for inductives AND records live in `lib/kernel/types.disp`; the
recognition ops are unified via `do_check` (the `(optic,role)` factoring for the recognize face). Generic
`fmap`/`fold_value` now work on EVERY coproduct — inj-tagged (Option/Either/Result + any `Coproduct_p`)
AND shape-encoded (List/Nat/Ord) — via the unifying move: each type carries its **shape↔inj iso
`(view, encode)` in the `functor` meta-field** (identity iso for inj-tagged; `fmap = encode ∘ fmap_inj ∘
view`, `fold_value` auto-reads the view). Full canonical suite green (42/42). Known limitation: this made
the kernel diverge under the *weak* naive reducer (no hash-consing) — `eval-naive-elaborate` is skipped
(reproducibility, not soundness; canonical + fast-naive-differential green). The full single-`cell_op`
collapse remains assessed as not-worth-it (see below). Companion to
[`TELESCOPE_FIXPOINT.md`](TELESCOPE_FIXPOINT.md) (the cells / `at`) and
[`NEGATIVE_TYPES.md`](NEGATIVE_TYPES.md) (the telescope).

> **Kernel integration — stage 1 landed (2026-06-25): the optic `over` half + `fmap`, additive
> (`lib/kernel/types.disp`, +98 lines, 0 deletions; full suite 41/41 + eager/naive/rust-eager green;
> `lib/tests/optic_kernel.test.disp`, 19 tests).** Added, all exported: `Param` marker; `pos_tele_p`
> (resolves `Param i` → a `pos_cell` checking `params i` at BUILD time, so recognition is unchanged and
> NO `rs` change is needed — the metacircle is structurally untouched); `Coproduct_p` (param-aware sum);
> `slot_over` (the positional `over` for the right-nested payload) + `at_map_args` + `fmap`/`bimap`/`nmap`;
> and the NAMED side `Record_p` + `key_over` + `fmap_rec`/`bimap_rec` (`key name = slot (index_of names
> name)` run backward over the §2.6 record). So the kernel now does generic `fmap` over BOTH positional
> (Coproduct, `slot.over`) and named (Record, `key.over`) containers — the over-half deduplication the
> prototype proved. **What's deferred** (the metacircle-risky core, its own gated step): collapse the
> per-kind ops (mint/apply/proj/pos/deriv/rec/gen/rec_at/qid) into ONE `cell_op` over `(optic,role)`,
> re-seat the negative cells (proj §7B / qid) preserving the StrictType metacircle, migrate existing
> types (Nat/List/Either) onto `Param` markers (needs param-aware fold/recursor), and stage-5 cleanup
> (`build_payload`/`mk_record` → `slot.over`/`key.over`). Shape-encoded (`Coproduct_viewed`) fmap and
> nested `RecUnder F` fmap also still deferred (need view-decode / F's map).

> **Full migration prototyped (2026-06-25): `lib/tests/optic_migration.test.disp` (23 tests, green
> on eager/naive/rust-eager + the vitest harness).** A cell is now `(optic, role)` driven by ONE
> generic `cell_op` (replacing mint/apply/proj/pos/deriv/rec/param); the optic is a `{get, over}` lens
> with `slot` (positional), `key` (named), `app`, `fresh`, `self`. All five §-claims hold empirically:
> (1) `proj`/`pos` differ ONLY in the optic; `rec`/`param`/`rec_at` ONLY in `role_type` (literal/self/
> params). (2) `key name = on_values names (slot (index_of names name))` — named IS positional + a
> name-table — is one executable line. (3) `at_map` rebuilds by COMPOSING the optics' `over`s, so
> `fmap`/`bimap` work over a Coproduct (slot.over) AND a Record (key.over) with **no `build_payload`
> and no `mk_record` by hand** — the deduplication step 0 only motivated. (4) the kernel `at`
> (recognize/respond) is REUSED UNCHANGED — the generic op plugs into the Step protocol; `at_map` is
> the only new walker. (5) Pi/Arrow/Intersection/Record/Coproduct/List/Either/binary-tree all re-spelled
> as `(focus -> role)` and recognized. Payload choice: uniform nil-terminated arg lists (§6 decision 4),
> making `slot` regular. Two findings worth keeping: dispatch on the role MUST be `if`/`is_role`, not a
> `match` (a match arm calling `check_step mode part X` η-exposes per CLAUDE.md and mis-evaluates);
> and the FF respond / StrictType metacircle (stage-2 gate) is the part an isolated prototype CAN'T
> exercise — it needs the real kernel wiring, so it's the integration risk to watch.

> **Step 0 prototype (2026-06-25): `lib/tests/path_b_fmap.test.disp` (16 tests).** `Param i` as a
> recognizer cell threaded through the *unmodified* kernel `at` (`rs = pair self params`); `nmap`/
> `fmap`/`bimap` derived from cells, rebuilding with `build_payload`. Validated on List/Either/tree.
> Its headline — `nmap` re-derives `build_payload` by hand — is exactly what the full migration's
> `over` deletes (confirmed above). Map-as-assoc-list (nested `RecUnder F`) stays deferred (§8 of
> TELESCOPE_FIXPOINT).

> **Step 0 landed as a prototype (2026-06-25): `lib/tests/path_b_fmap.test.disp` (16 tests, green
> on eager/naive/rust-eager).** `Param i` is a real recognizer cell — threaded through the
> *unmodified* kernel `at` by bundling `rs = pair self params` (so `rec` cells read `pair_fst rs`,
> `param` cells read `pair_snd rs`), which concretely demonstrates the "new observation = a new op,
> no walker edit" discipline. `nmap`/`fmap`/`bimap` derive generically from the cells (a cell-walk:
> `param i` → apply `fns i`; `rec` → recurse; fixed → leave; rebuild with `build_payload`), validated
> on `List` (fmap), `Either` (bimap), and a binary tree (fmap, 2 rec positions). Map-as-assoc-list
> (nested `RecUnder Pair`) is deferred with nested fmap (§8 of TELESCOPE_FIXPOINT). **The headline
> finding holds empirically:** `nmap` re-derives `build_payload` by hand at the rebuild site — the
> consumer that makes the optic's `over` half load-bearing, i.e. the signal that the §5 migration now
> earns its keep. The prototype is self-contained (no kernel change), so promotion (§5 stage 1+) is
> the next move, not a prerequisite.

A telescope cell tangles **two independent axes**: *where* it looks (a named record field, a
positional constructor slot, a context index) and *what* it does there (check a type, recurse, guard,
pin a derived value). This note factors them — **a cell = `(optic, role)`** — so that:

- named, positional, and indexed addressing become *one* thing (an **optic**: get + over);
- `fmap` and value-reconstruction fall out of the optic's `over`, instead of the ad-hoc
  `build_payload` (tuples) / `mk_record` (records) we have now;
- adding an addressing scheme **or** a role is independent (a product, not a fresh cell op each time);
- the cells land in the **container / polynomial-functor** theory: a former is a *shape* with a set of
  *positions*, each addressed by a *key*.

## 1. The two axes — current cells, decomposed

Every cell op today is `{meta, mode, source, prior, frame, rs} -> Step`. Read across, each is exactly a
**focus** (how to obtain the value this cell is about) × a **role** (the obligation on it):

| cell | focus (the optic) | role (the obligation) |
|---|---|---|
| `mint A` | `fresh A` — introduce a ∀-hyp | **bind** |
| `apply ty` | `app` — `source prior` (a function codomain) | **check** `ty` |
| `proj name ty` | `key name` — `lookup_field source name` | **check** `ty` |
| `deriv name v` | `key name` | **pin** `v` |
| `pos acc ty` | `slot acc` — `acc source` (positional) | **check** `ty` |
| `rec F acc` | `slot acc` | **check** `(F self)` |
| `rec_at i acc` | `slot acc` | **check** `(ctx i)` |
| `gen acc` | `slot acc` | **guard** `is_gen` |
| `qid ty` | `self` — `source` itself | **check** `ty` |
| `qid_meta ty` | `meta` — `type_meta source` | **check** `ty` |
| **`Param i acc`** (new) | `slot acc` | **param** `i` (≈ check `(params i)`, + mappable) |

Two clean vocabularies fall out:

- **focus** ∈ `{ fresh A, app, key name, slot acc, self, meta }` — *how to obtain the value*.
- **role** ∈ `{ bind, check T, pin v, guard g }`, where the type `T` of a `check` may come from a
  literal, the recursive `self`, a context entry `ctx i`, or a parameter `params i`.

`rec`/`rec_at`/`Param` stop being three ops: they are **one focus (`slot`)** with **check against a
threaded environment** (`self` / `ctx i` / `params i`). Likewise `proj` and `pos` differ *only* in the
optic (`key` vs `slot`).

## 2. The optic — a tree lens (get + over)

An optic focuses on a sub-tree and supports **both directions**:

```disp
// optic = { get : Whole -> Part ;  over : (Part -> Part) -> Whole -> Whole }
slot acc        // positional: get = acc ;  over = rebuild the pair-nest with that slot mapped
key  name       // named:      get = lookup_field _ name (honest, Ok/Err — establishes presence;
                //             the total `field`/`proj` is the §2.6 cut) ;  over = mk_record, that field mapped
```

The crucial fact (already true in the substrate): **a name is an index plus a key-table.** §2.6
projection is literally
```disp
proj P name = path_at (index_of (pair_fst P) name) (pair_snd P)   // cut.disp:36
```
— resolve the name against the name-list, then index positionally. So `key name` *is*
`slot (index_of names name)` over the payload, re-attaching the names; positional access is the
primitive, named access is the sugar. `RecAt i` / `Param i` are the same positional access into the
*context* rather than the value.

`get` is what we already store (the `acc` / `lookup_field`). **`over` is the missing half** — and it is
exactly what `build_payload` (positional restructure) and `mk_record` (named restructure) do today,
*per addressing scheme, by hand*. Promoting them to the optic's `over` deletes that duplication and is
what `fmap` and the recursor's `P (c args)` conclusion both need (`build_payload` is already in the
kernel from `derive_case_telescope`).

> **Container view.** A former is *shapes × positions*: `F X = Σ (s : Shape). (Pos s -> X)`. A
> *constructor* is a shape + its positions; a *field* is a position; a *key* (name or index) addresses
> one; the **cell-telescope is the position structure**; `get`/`over` are destructure/restructure.
> "Named vs indexed" is just two presentations of the position-set (labels vs `{0..k-1}`).

## 3. Two walkers over one `(optic, role)` cell

Recognize and respond share one protocol — each cell emits a `Step` (`SMint`/`SThread`/`SReject`/
`SDone`) and the **Step-based `at`** interprets it (exactly today's `at`, just reading `(optic, role)`
instead of a monolithic op):

```disp
cell_op = {meta, mode, source, prior, frame, rs} ->        // mode = recognize (TT) | respond (FF)
  let x = optic_get (meta.optic) source prior              // the focus (key/slot/self/meta/app/fresh)
  apply_role (meta.role) x mode frame rs                   // -> SThread x / SReject / SMint A / SDone (Extend/Reduce)
```

`fmap` is **not** a Step — it is a *transform* (collect parts → map per role → restructure), so it is a
**sibling walker `at_map`** over the *same* cells, using the optic's `over` instead of `get`:

```disp
at_map fns = walk cells: at a `param i` cell -> optic_over (fns i) ;  at a `rec` cell -> recurse at_map ;
             at a fixed `check`/`pin`/`guard` -> leave ;  then the position-wise `over`s compose to the
             rebuilt value (no `build_payload`/`mk_record` by hand). `fmap f = at_map (single f)`,
             `bimap f g = at_map (f, g)`, `nmap (…)` in general.
```

So **one cell representation, two walkers** — the Step-based `at` for recognize/respond (and `fold`/
`children`, which are also gets), and `at_map` for `fmap` (the only one needing `over`). Same
"one structure, many operations" discipline Path B established, now with the addressing (`get`/`over`)
factored out of the role.

## 4. Every former in the new design

Nothing about the *types* changes — only how their cells are spelled. Using `(focus → role)`:

```
Pi A B        = [ fresh A → bind ;  app → check (B x) ]
Arrow A B     = [ fresh A → bind ;  app → check B ]
Record [a:A;…]= [ key "a" → check A ; … ]                         -- the only `key` former today
Sigma A B     = [ key "fst" → check A ;  key "snd" → check (B fst) ]
Coproduct c   = per variant: [ slot j → check Tⱼ | check self | param i ]…
List A        = Coproduct (λA. [ nil [] ; cons [ slot 0 → param 0 ; slot 1 → check self ] ])
Either A B    = Coproduct (λA B. [ left [slot 0 → param 0] ; right [slot 0 → param 1] ])
Stream A      = [ slot head → param 0 ;  slot tail → guard is_gen ]   -- codata; respond delegates to Record
StrictType    = [ self → check (Tree→CheckerResult) ;  meta → check MetaShape ]
```

`Record` (and its instances — `Sigma`, `MetaShape`, `NeutralMeta`, every record-type literal) is the
only former that uses the **`key`** optic; everything else uses `slot`, `self`, `meta`, `fresh`, or
`app`. That is precisely the "indexed vs named" split you noticed — and it is now *one parameter of the
cell*, not a different cell kind.

## 5. Migration — staged and test-gated (each stage stays green)

The metacircle (StrictType self-typing) and the perf-sensitive `at` make this a careful refactor, so
introduce the new layer **underneath** the existing cells, then collapse:

1. **Add the optic interface** `{get, over}` with `slot acc` (over = `build_payload`-with-one-mapped)
   and `key name` (over = `mk_record`-with-one-mapped). Pure addition; nothing uses `over` yet.
2. **Re-seat the extractive cells on optics.** `proj`/`pos`/`rec`/`gen`/`deriv`/`rec_at`/`qid_meta`
   store an `optic` in meta and call `meta.optic.get` instead of inline `acc`/`lookup_field`. Byte-for-
   byte behavior; the `at` recognize/respond paths are unchanged except the `get` indirection.
   *Gate:* full suite green (this is the risky one — it touches every cell; validate self-typing).
3. **Split role from focus.** Replace `proj_op`/`pos_op`/… with the single `cell_op` interpreting
   `(optic, role)`. `rec`/`rec_at`/`Param` collapse into `slot` + `check (env i)`. Constructors
   (`proj_cell`, `pos_cell`, …) keep their signatures and emit `(optic, role)` pairs, so the elaborator
   and call sites don't move. *Gate:* full suite + metashape green.
4. **Add the `map` mode + `Param`.** `at` gains the map mode using `optic.over`; add the `param` role
   and `Param i` marker (the `RecAt i` analogue). `fmap`/`bimap`/`nmap` derive. *Gate:* new
   `path_b_fmap` test (List/Either/Map).
5. **Unify reconstruction.** Delete `build_payload`/`mk_record` as standalone; they are now
   `slot.over` / `key.over`. `derive_case_telescope`'s conclusion and `mk`/`mk_record` route through the
   optic. *Gate:* recursor + record tests green.
6. **(optional) Negative side.** `fresh`/`app` already fit as focuses, so `mint`/`apply` can become
   `(fresh A → bind)` / `(app → check)` for full uniformity — or stay special (they have no `over`, being
   the exponential, not a container). Recommend leaving them until a consumer needs it.

Stages 1–3 are a *behavior-preserving* refactor (the win is internal: one cell op, named/positional
unified). Stages 4–5 are where the new power (`fmap`, unified rebuild) lands. Each stage is independently
revertable.

## 6. Payoffs and risks

**Payoffs.** `fmap`/`bimap`/`nmap` for free (the `over` mode + `Param`); `build_payload`/`mk_record`
deduplicated into `over`; named/positional/indexed addressing is one parameter; the cells are now a
container description, so any future position-indexed operation (lenses over user data, a generic
`zip`/`traverse`, optic-based pattern matching) is a new role or a new optic, not a new cell kind.

**Risks.** (1) **Self-typing**: re-seating every cell on an optic touches the StrictType metacircle —
stage 2 must keep `metashape.test` green. (2) **Perf**: an extra `optic.get` indirection on the hottest
path (`at`); the generic walker is already ~2× the old specialized ones (`APPLY_BUDGET` 40M), so measure
and, if it bites, optimize the *evaluator*, not by special-casing. (3) **Compile budget**: optics carry
`over` closures; watch the `wait`/closed-redex hazards (cf. `fold_value`'s `wait fold T`). (4) The
last-arg-is-identity optimization in `pos_tele` makes positional paths slightly irregular — a uniform
`(pair … nil)`-terminated payload would make `slot j = snd^j ∘ fst` uniform at the cost of one extra
`nil` per constructor; decide before stage 1.

**Recommendation.** Do stages 1–3 only when the unification earns its keep — i.e. *together with*
`Param`/`fmap` (stage 4), which is the consumer that makes the `over` half load-bearing. Building
`fmap` on the current bare getters would re-derive `build_payload` by hand; building it on a small
`optic` is the generalization that pays for the refactor. Until then the optic is the right *mental*
model (it's already what the §2.6 cut implements).

**Where to start (step 0 — before any of the migration above).** Prototype `Param i` + `fmap`/`nmap`
directly on the **existing `slot`** accessors (reusing `build_payload` for the rebuild) — *no* optic
interface, *no* cell refactor, *no* metacircle risk. Concretely: add a `Param i` marker (the `RecAt i`
analogue) + a `param_cell`, thread the parameter context through `at` like `rs`, and write `nmap` as a
cell-walk that applies `fns i` at `param` cells, recurses at `rec` cells, and rebuilds with
`build_payload`. Validate on `List`/`Either`/`Map` in a `path_b_fmap` test. This proves the semantics
cheaply and is the consumer that later *justifies* lifting the bare getters to the full `optic` (the
stage-1–5 refactor) — at which point `build_payload` becomes `slot.over` and named/positional unify.
So: **`Param`/`fmap` on slots first; the optic migration second, only if/when it earns its keep.**
