# Telescopes: dependent n-ary records + derived fields — implementation plan

Status: **LANDED** (all phases incl. 3.5 Sigma switch, 2026-06-10; 141 host +
48 telescope tests green). Companion design doc precedent:
`WRAPPING_INVARIANT.md`. Kept for the design rationale; the deviations
discovered during implementation are listed below — read those before
trusting details in the body.

## 0. Deviations from the plan as written (what implementation taught us)

1. **δ-transparency is its own arm, `Reduce`, not `Return`.** The plan said
   "derived → Return value", but `hyp_reduce`'s `Return` arm Ok-wraps (the
   VERDICT case, load-bearing for the H-rule predicate
   `type_predicate_h_rule`). Reusing it reproduced the §7.5 wrapping bug
   (`Ok (succ a)` in operator position). Subsequently the coproduct was re-cut
   to the canonical stuck/computes dichotomy: `Action = Extend type | Reduce
   value` (both bare), with `Return v = Reduce (Ok v)` demoted to a library
   alias — a verdict is just a value that happens to be a CheckerResult.
2. **`Record` lifts its field list LAZILY.** The planned
   `Record := Telescope ∘ fields_to_tele` failed self-verification:
   `Record : Tree -> Type` mints a Tree-hyp and the eager fold triages it at
   formation. Record keeps its own wait-form (params = flat list) and runs
   `fields_to_tele` inside the recognizer/respond — formation stays parametric,
   the telescope folds are still the only checking code.
3. **Policing expectations were half-wrong, in a good way.** A derived recipe
   like `double a` is NOT rejected over a hyp — `double` is `nat_rec`-routed,
   so it goes STUCK as a clean elimination neutral (δ composes with elim §A).
   Only RAW-TRIAGING recipes/types (e.g. `pair_snd a`) hit the dead state.
4. **`mk` takes the Telescope TYPE** (reads `recognizer_params` off the meta),
   not the raw telescope — friendlier call sites (`mk T { a := 2 }`), and `mk`
   is unannotated (its fold triages, so a `Tree -> …` typing self-rejects).
5. **Dependent-tail identity is per-spelling.** A literal `{ T : Type, x : T }`
   and a helper-built telescope are extensionally equal but different trees
   (closures under binders don't normalize across constructions) — the exact
   conversion discipline Pi codomains already have. Same spelling → same tree.
6. **Sigma switch went further than planned:** `Pair` values are records too
   (`mk_pair`), and the `proj_fst`/`proj_snd` `select_lazy`-over-`is_neutral`
   hack is DELETED — `.fst`/`.snd` is uniform over concrete records and hyps.
   `walker_pair_fst`/`walker_pair_snd` are gone.
7. **Phase 0 grew the field pun** (`{ tag := 1; respond }` = `respond :=
   respond` resolving outward), with ordering coherence: a field's RHS always
   compiles before its own name binds. A pun needs a sibling `:=` field.
8. **Curried-binder precedence:** `{a : Nat, b : Nat} -> e` still parses as the
   curried binder (back-compat); record-domain functions are written
   `Arrow { a : Nat, b : Nat } R`. A braced-with-derived-member followed by
   `->` does parse as a telescope-domain arrow (binder parse fails on `:=`).

## 1. Summary

One new kernel former — `DRecord` over a **telescope** — unifies multi-input
functions with records and adds **derived fields** (fields computed from earlier
fields). No new Σ-ops, no substrate changes, no equality types (derived fields
are pinned by `tree_eq` conversion, tier-1 judgmental equality — independent of
`Eq`/cubical §13).

Key facts established by probing the current tree (2026-06-09):

- `f { a := 1; b := 2 }` **already works** for non-dependent `Record` domains
  (verified green). The gaps are: (1) `Record` is non-dependent
  (`types.disp` `record_check` applies each field type as a bare type), (2) a
  record literal's field value cannot reference an earlier field
  (`{ a := 2; b := double a }` → "unresolved free variable a", `compile.ts:410`),
  (3) no n-ary dependent former (`Sigma` is binary).
- **No function former is needed.** `Pi (DRecord tele) R` *is* the unified
  multi-input function. The load-bearing new piece is `DRecord`'s **respond**
  (projection typing on a record-hypothesis), which is `sigma_respond`
  (`types.disp:143-150`) generalized from positional `fst/snd` to `acc name`.
- The kernel's `Action` coproduct already encodes opaque/transparent:
  opaque field → `Extend type`; derived field → `Return value` (δ-unfolding).
  `hyp_reduce` handles both arms today (`engine.disp:11-14`); records just
  never used the `Return` arm before.

## 2. Design (condensed)

### 2.1 Telescope encoding

A telescope is cons-of-entry where each **tail is a binder over that entry's
value** (the dependency is a function arrow, exactly like `Sigma`'s
`B : A -> Type`). Structural well-foundedness for free: tails see only priors.

```
Tele  ::=  t                              // nil
        |  t  entry  (λx. Tele)           // fork(entry, tail-closure)
```

An entry is a §2.6 record; the solver slot is a stem-option (`t` = none,
`t e` = some — O(1) triage, no std Option dependency):

```disp
{ name := "a"; ty := Nat; def := t }             // opaque   (a : Nat)
{ name := "b"; ty := Tree; def := t (double a) } // derived  (b := double a), e in scope of priors
```

One entry kind, solver-strength axis: none = assumption (argument / stored
field), closed term = derived (evaluate), unifier = implicit (DEFERRED).
A non-dependent tail bracket-abstracts to the K-stem `t t rest` — no special
case. Hash-consing keeps equal telescopes identical, so `DRecord` conversion
stays O(1) `tree_eq` (same discipline as `Pi`).

### 2.2 The former

```disp
DRecord : Tree -> Type := {tele} -> wait (make_recognizer
  {meta, v} -> drec_check meta.recognizer_params v
) (make_meta tele drec_respond)
```

`make_recognizer` gives the neutral H-rule for free.

### 2.3 Recognizer — the concrete feed

Fold the telescope feeding each tail the **stored projection** `field v name`
(a derived prior feeds its stored value too, so later types may depend on
derived fields):

- opaque entry: `bind (entry.ty x) …` — type-check the field (same raw
  application as today's `record_check`);
- derived entry: `tree_eq x e` — **the pin**. No `Eq`, no proof objects.
  The pin subsumes the type check when the recipe is well-typed, so a derived
  entry's `ty` may be `Tree` (surface `b := double a` needs no annotation).

**Derived fields are STORED, not computed-on-read.** A record value stays a
self-contained §2.6 product (`r.b` is the cut `r (acc "b")`, no type in hand —
`cut.disp:33`). The recognizer's pin keeps stored derived fields honest;
transparency for *neutrals* lives in the respond (below). The alternative
(thin values) breaks "types are predicates over plain trees".

### 2.4 Respond — the neutral feed (the load-bearing piece)

Frame = `acc name`. Walk the telescope; feed each **prior**:

- opaque prior → its projection-neutral
  `wait hyp_reduce (extend_neutral_meta self entry.ty (acc entry.name))`
  — verbatim Sigma's pattern (`types.disp:148`) generalized to named fields;
- derived prior → **its value `e`** (δ in the feed — transparency).

Tail instantiation routes through `param_walker` (the GAP-2 discipline every
respond already follows); a tail that raw-triages a hyp → `Extend InvalidType`.
At the requested name:

- opaque → `Extend entry.ty` (instantiated by the priors);
- derived → `Return e` (instantiated) — δ-unfolding through the spine.

### 2.5 Builder (library, not kernel)

`mk tele given`: fold the telescope — opaque entries take `field given name`,
derived entries fill `e` — emit `mk_record names payload`. This is the
omission story (`mk T { a := 2 }` → `{ a := 2; b := 4 }`) with zero
elaborator/unification machinery. `mk` cannot lie: the recognizer re-pins.

### 2.6 Surface syntax (complete brace disambiguation)

| Braced form | Today | After |
|---|---|---|
| all `name := e` | record **value** | **unchanged** (`f { a := 1; b := 2 }` untouched) |
| all `name : T` | recType (binder-annotation only; throws standalone, `compile.ts:324`) | also elaborates standalone → telescope → `DRecord` (strictly new) |
| mixed `name : T` and `name (: T)? := e` | parse error | **new**: telescope literal with derived entries |
| only `let`/`test` + trailing expr | block | unchanged |
| `{…} -> e` | binder (curried Pi/lambda) | unchanged |
| `{r : {a : Nat, b : Fin a}} -> …` | `r` gets projection field-names only (`compile.ts:302-309`) | field-names **plus** `r : DRecord tele` — body projections typecheck |

Accepted limitation: an *all*-derived telescope cannot be written as a brace
literal (parses as a value) — degenerate case; use the former explicitly.

Curried multi-param binders `{a, b} -> e` stay curried. The
`Pi A (λa. …)` ↔ `Arrow (DRecord tele) R` iso is a later library lemma, not a
prerequisite.

## 3. Implementation plan (test-first ordering)

### Phase 0 — baselines + recValue scoping fix (independent value, lands alone)

1. Characterization tests pinning what already works: non-dependent
   `f { a := 1; b := 2 }`, `Record`/`Sigma` behavior, recType-annotation
   projections. (New `lib/tests/telescope.test.disp`, baseline section.)
2. **Fix recValue field scoping** (`compile.ts:410`): rebind each compiled
   field into `fieldLookup` before compiling the next (the `members` loop at
   `:334-338` already does exactly this for `let`s — mirror it).
   - Test: `{ a := 2; b := double a }` projects `b = 4`.
   - Semantics note: field names now shadow outer scope within a literal —
     document; audit kernel `.disp` for literals where a field name collides
     with an outer binding it *intends* to reference
     (e.g. `make_meta`'s `{ respond := respond; … }` self-reference pattern:
     RHS must resolve to the *parameter*, so rebind only AFTER compiling that
     field's value — order matters, pin with a test).

### Phase 1 — kernel former (adversarial tests FIRST)

3. Write the soundness tests before the former exists:
   - lying derived field rejected (`{ a := 2; b := 5 }` vs `b := double a` → `Ok FF`);
   - dependent check (`{ T : Type, x : T }` accepts `{T := Nat; x := 3}`,
     rejects `{T := Nat; x := TT}`);
   - projection typing on a hyp: `r.a : Nat`, `r.b : Fin r.a` (spine inspect);
   - derived projection on a hyp δ-unfolds: `r.b` ⇒ `Return (double r.a-neutral)`;
   - a telescope tail that triages its prior's neutral → dead state
     (`Extend InvalidType`), per the GAP-2 regime;
   - a derived recipe capturing an *enclosing* `bind_hyp`'s hyp → escape
     check fires (pin expected behavior; same regime as spine payloads);
   - unknown field → `Extend InvalidType`; H-rule on a `DRecord` neutral.
4. Implement in `lib/kernel/types.disp`: `DRecord`, `drec_check`,
   `drec_respond` (+ file-local fold helpers). Use `match`/`triage` per the
   compiler workarounds (no `select_lazy` around recursive calls).
5. `npm test` — 100% green including all pre-existing suites.

### Phase 2 — builder + derived-field surface-level tests

6. `mk` (in `lib/kernel/types.disp` alongside `elim` precedent, or
   `lib/std/` if it stays kernel-independent — decide at review).
7. Tests: `mk T { a := 2 } = { a := 2; b := 4 }`; `typecheck T (mk T given) = Ok TT`;
   `f (mk T { a := 3 })` through a `Pi (DRecord T) R`.

### Phase 3 — re-express `Record` on the telescope

8. `Record fields :=` fold the flat `[pair name T]` list into a constant-tail
   (K-stem) telescope; **keep the name and `Tree -> Type` call shape** —
   `src/compile.ts:434,723` build `Record [pair name type, …]` for module
   `typ`s (elaborator couples by NAME; see memory note + grep `src/` first).
9. Delete `record_check` + `record_respond` (`types.disp:206-220`).
   `lookup_arm` stays (Coproduct uses it).
10. Update `metashape.test.disp:51` (`record_respond` no longer exists —
    replace with the equivalent `drec_respond` expectation and reason).
11. Regression focus: module auto-verify paths (`use_raw.test.disp`,
    `module.test.disp`, `typed_mod.disp`), `NeutralMeta`/`MetaShape`/`Action`
    definitions (unchanged textually — they call `Record`), `record.test.disp`.

### Phase 4 — surface syntax + elaborator

12. `src/parse.ts`: mixed braced form (≥1 bare `name : T` ⇒ telescope literal;
    `:=` members inside are derived entries; all-`:=` stays recValue).
13. `src/compile.ts`: compile recType/telescope literals standalone — fold
    fields in order, each field's type compiled under binders for the prior
    names (the AST already walks in order; dependent scoping is positional
    per `SYNTAX.typ` shape rules), wrap in `DRecord`. Binder param recType
    annotations also produce the `DRecord` type (today they only carry field
    names) — touches the `type.tag !== "recType"` guard at `compile.ts:815`
    and `binderToPi` (`:619`).
14. Tests: dependent-domain `f { a := 1; b := 0 }` end-to-end; recType
    standalone as a Type; mixed-form parse + check + mk.

### Phase 5 — opportunistic simplifications + docs

15. Decide `Sigma` (see §4.2). Explore `Frame` structured types (§4.3) as a
    follow-up branch. Update `SYNTAX.typ` (brace table), `KERNEL_DESIGN.md`
    (telescope idiom, Extend/Return = opaque/transparent), `TYPE_THEORY.typ`
    (telescope section; mark Record/Sigma as derived forms), `CLAUDE.md`
    layout notes.

Each phase lands green (`npm test`, currently 140) before the next starts.
Phases 0-2 are purely additive; Phase 3 is the only one that rewrites
something live.

## 4. Expected simplifications

### 4.1 Mechanical deletions (high confidence)

- **`record_check` + `record_respond` deleted** (`types.disp:206-220`) —
  subsumed by `drec_check`/`drec_respond` with constant tails. `Record`
  becomes a ~3-line const-lift fold.
- **Non-dependent/dependent split disappears.** One recognizer fold, one
  respond fold; "non-dependent" is just K-tails. No parallel formers to keep
  in sync.
- **`{ a := 1; b := double a }` value scoping** (Phase 0) removes a standing
  surface wart independent of everything else.

### 4.2 `Sigma` becomes a 2-entry telescope (decision required)

`Sigma A B ≅ DRecord [(fst : A), (snd : B fst)]`, which would delete
`sigma_respond` and the ad-hoc projection selectors `walker_pair_fst` /
`walker_pair_snd` (`types.disp:132-151`). **Caveat:** Sigma *values* are bare
`pair a b`, DRecord values are named §2.6 products — re-expressing changes
value representation (breaks `pair`-based call sites/tests). Options:
keep `Sigma` as-is and note the redundancy (zero risk), or migrate values
(small breaking sweep). Recommend: keep for now, revisit after Phase 5.

### 4.3 StrictType "Part C" unblock (the big structural payoff)

`metashape.test.disp:44-51` pins the Rung-3 wall: inspecting responds
(`inductive_respond`, `eq_respond`, `record_respond`) do **not** inhabit
`RespondShape` because projecting `frame.motive` off an *abstract*
`frame : Tree` hyp routes to Tree's inert respond → `Err`. The test file
itself says the fix "needs a per-former STRUCTURED Frame type (Part C) …
(When that lands, these flip to Ok TT)."

`DRecord` is exactly that machinery: with
`Frame_inductive := DRecord [(motive : …), (cases : …)]`, a frame-*hyp*'s
`frame.motive` routes through `drec_respond` → `Extend` — a typed projection
instead of a wall. Scope honestly: this types the `frame` side; the `params`
side of each respond needs its own per-former telescope type, and `tele`
itself is triaged inside `drec_respond` (so `drec_respond` under a fully
abstract `params` still won't inhabit `RespondShape` without a `Telescope`
params type). Part C becomes *incremental* rather than blocked. This also
serves the memory note that `NeutralMeta` needs "sanctioned projections" for
responds — `drec_respond` IS the sanctioned projection mechanism.

### 4.4 Elaborator/recType plumbing audit (candidate, verify before claiming)

`compile.ts:144-148` (`fields`/`fieldTrees`/`fieldInnerFields`/… metadata) and
`:302-309` (binder recType annotations carrying field names) exist so
projections work on bound variables and statically-known records. Runtime
projection already works without metadata (the cut; `record.test.disp:19-22`).
Once recType annotations produce real `DRecord` types, some of this metadata
plumbing may reduce to the compile-time-collapse fast path only. Audit in
Phase 4 — do not pre-commit.

### 4.5 Spec/story consolidation

- `TYPE_THEORY.typ`: Pi-domain records, Record, Sigma, and eliminator-frame
  records all become instances of one telescope construction with two feeds
  (concrete projection / `bind_hyp`) and one transparency axis
  (`Extend`/`Return`). The "two-argument dispatcher over fixed Σ" story is
  untouched — no new Σ-ops.
- Multi-input functions in `std/` can migrate to named-record domains
  (`f { a := …; b := … }`) where ergonomics favor it; curried stays
  first-class. No forced migration.

## 5. Risks & mitigations

| Risk | Assessment | Mitigation |
|---|---|---|
| Compile-budget blowup on nested tail closures | Low — the known blowup (CLAUDE.md) is specific to *recursive* bodies under `select_lazy`; telescope tails are non-recursive | K-stem fast path for constant tails; `match`-style closures in the folds; budget canary test with a 6-entry dependent telescope |
| `Record` name/shape coupling in elaborator | Known (memory: elaborator consumes kernel names by string AND some tree shapes) | Keep `Record`'s name + flat-list signature; grep `src/` for every consumed name before Phase 3; module-verify regression suite |
| `Return`-unfolding escape regime | `Return e` where `e` mentions the self-hyp is the same regime as spine payloads (which already mention the hyp) | Pin with an explicit escape test in Phase 1; if `occurs` misfires, the recipe is to scope the check to `bind_hyp` boundaries (where it already lives) |
| Respond walk cost: projecting field *i* re-instantiates *i−1* tails through `param_walker` | O(n²) per full-record elaboration; n is small (≤ ~6 in kernel metas) | Accept; hash-consing dedupes repeated instantiations; revisit only if profiling says so |
| Parser ambiguity regressions | Mixed form is currently a parse error (free space); all-`:=` unchanged | Parser unit tests for every row of the §2.6 table, incl. `{…} -> e` reparse |
| Extensional telescope equality | Two extensionally-equal but differently-written tails ≠ `tree_eq` | Same limitation Pi has today for codomains — no regression, document |
| Field-name shadowing in recValue scoping fix | `{ respond := respond }` self-reference pattern in kernel metas | Rebind a field only AFTER compiling its own RHS; pin with test (Phase 0) |

## 6. Explicitly deferred

- **Implicit-by-unification entries** (`Imp`): the third solver strength.
  Requires metavariables + a unifier in the elaborator; the telescope entry
  encoding already has the slot for it (the solver axis), so it's additive.
- **Bidirectional auto-fill at the literal site** (`f { a := 1 }` with `b`
  derived, no `mk`): requires pushing expected types into record literals.
  `mk T { a := 1 }` covers the use case until then.
- **Curried↔record-domain iso** as a library lemma.
- **Sigma value-representation migration** (§4.2) pending decision.

## 7. Naming (bikeshed at review)

`DRecord` is the working name for the former; alternatives: `Telescope`
(emphasizes the unification), `Struct`. The raw telescope list needs no
exported former — it is `DRecord`'s `recognizer_params`. A `Telescope : Type`
*classifier* (for typing `drec_respond`'s params later, §4.3) is future work.
