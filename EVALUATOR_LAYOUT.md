# Evaluator subsystem layout

How the evaluator backends are organized on disk, so that multi-language,
externally-vendored evaluators stay conceptually isolated and the engine never
has to care about anyone's build system. This refines `EVALUATOR_PLAN.md` §5
(which now points here); the ABI itself is unchanged (`src/eval/types.ts`).

Status: PLAN — agreed shape, not yet migrated. The current tree is the flat
`src/eval/{types,eager,naive,registry,batch}.ts` + `src/core/tree.ts` from the
landed phases; this doc is the target it moves to.

## Principles

- **A *project* is the unit of vendoring/build; an *evaluator* is the unit of
  registration.** One foreign project (lambada) can ship *several* evaluators
  (its lazy-stacks reducer, its memoizing reducer, …). So the folder count is
  per-project, but the registered-backend count is per-evaluator.
- **Maximal isolation, minimal ceremony.** Each foreign project is a
  self-contained folder in its own ecosystem (its own build, its own vendored
  deps). No manifest, no per-backend contract files, no plugin framework — just
  a folder with a `build.sh` and a thin TS wrapper.
- **No shared *behavior*, only a shared *contract*.** The single common thing is
  the ABI (`types.ts`) — an interface. Each evaluator keeps its own reducer and
  its own ternary codec; nothing is factored across them (naive already has its
  own codec, and that's correct, not duplication to "fix").
- **The everyday loop needs zero foreign builds.** eager + naive are pure TS and
  always available; a foreign backend registers only if its artifact is built,
  else it's skipped (so `npm test` never pulls cargo/curl).

## Target layout

```
src/eval/                  # the engine-facing layer — TypeScript only
  types.ts                 #   the ABI: Session, EvalBackend, Budget,
                           #   Classification, the BatchRunner contract. The ONE
                           #   shared thing, and it's just interfaces.
  registry.ts             #   name -> backend; eager/naive always, foreign-if-built
  impl/                    #   one entry per EVALUATOR, kept apart from the shared
                           #   contract above (a folder if it has private
                           #   internals, a single file otherwise)
    eager/                 #     the TS reference backend
      tree.ts              #       hash-consed runtime (eager's private internals)
      index.ts             #       EagerSession + eagerBackend (+ re-exports Tree, prettyTree)
    naive.ts               #     the TS honesty backend
    lambada.ts             #     FFI wrapper: spawns evaluators/lambada's binary,
                           #       registers ITS several evaluators (lambada-lazy,
                           #       lambada-memo, …) — same subprocess, configured per evaluator
    tcnet.ts               #     FFI wrapper: a Session over evaluators/tc-net's wasm

evaluators/                # one self-contained folder per FOREIGN PROJECT
                           # (its own language + build; NOT in the TS build path)
  lambada/
    build.sh               #   pinned fetch/build -> artifacts/  (yields multiple evaluators)
    artifacts/             #   gitignored build output the wrapper loads/spawns
    vendor/                #   only if the upstream is multi-file; lambada is a
                           #   single main.js, so its build.sh just pins+fetches
  tc-net/                  # (future) first-party Rust -> WASM
    crate/                 #   Cargo.toml, src/
    build.sh               #   cargo build --target wasm32 -> artifacts/tcnet.wasm
    artifacts/             #   gitignored
```

`tsconfig`: `include: ["src/**/*.ts", ...]` with `evaluators/` excluded from the
TS build entirely (its only TS, if any, is loaded via built artifacts, not
compiled by the engine's tsc). The wrappers in `src/eval/impl/` reference each
project by a stable artifact path under `evaluators/<project>/artifacts/`.

(Naming note: `src/eval/` and `evaluators/` read a little alike; rename the
foreign dir to `runtimes/` or `native/` if that bugs you — purely cosmetic.)

## Conventions

**A foreign project folder** (`evaluators/<project>/`) contains exactly:
- `build.sh` — produces everything into `artifacts/` (gitignored) and nothing
  else. Pins its upstream (a commit SHA + a content hash it verifies). For
  lambada that's a fetch of one `main.js`; for tc-net it's `cargo build`.
- `artifacts/` — gitignored; a clean clone rebuilds it from the pin.
- optionally `vendor/` — a pinned checkout, only for multi-file upstreams.

**An evaluator wrapper** (`src/eval/impl/<name>.ts` or `<name>/index.ts`) exports
one `EvalBackend`. For TS backends it implements `Session` directly; for foreign
backends it's the FFI seam (spawn a subprocess / instantiate a wasm module) and
nothing more. A project that ships N evaluators is N registrations from one
wrapper file, not N folders.

**The registry** registers eager/naive unconditionally and each foreign backend
inside a `try` that skips it (with a one-line "not built; run
`evaluators/<project>/build.sh`" note) if its artifact is absent. That single
guard is the whole "graceful degradation" story.

## Adding an evaluator

1. If it's foreign: `evaluators/<project>/` with a `build.sh` that pins its
   upstream and outputs to `artifacts/`.
2. `src/eval/impl/<name>.ts` exporting an `EvalBackend` (implement `Session`, or
   wrap the artifact). One file per evaluator the project exposes.
3. One line in `registry.ts`.

That's it — no shared base to subclass, no manifest to update.

## What stays shared vs. not

- **Shared (in `src/eval/`):** `types.ts` (the ABI + BatchRunner contract) and
  `registry.ts`. That's the entire common surface. `sessionBatchRunner` (turns
  any Session into a batch contestant for bench) is a small helper that lives
  with the contract.
- **Not shared (per evaluator):** the reducer, the ternary codec, the
  hash-consing/memo strategy, the FFI mechanism. Each backend owns its own.
  `emitBlob`/export stays an engine-side helper over `Session` (not evaluator
  code).

## Migration (from the current landed tree)

Small — most files only move, no behavior change, suite stays green:

1. `src/core/tree.ts` → `src/eval/impl/eager/tree.ts`;
   `src/eval/eager.ts` → `src/eval/impl/eager/index.ts`;
   `src/eval/naive.ts` → `src/eval/impl/naive.ts`.
   `types.ts`/`registry.ts` stay put; fold the `batch.ts` BatchRunner interface
   into `types.ts`. Rewrite imports in `compile.ts`/`run.ts`/tests; update
   `tsconfig`. (Same kind of mechanical move as the core/eager split.)
2. Add the registry skip-if-unbuilt guard.
3. Then lambada: `evaluators/lambada/build.sh` (pinned fetch of `main.js`) +
   `src/eval/impl/lambada.ts` (~40-line subprocess BatchRunner registering
   `lambada-lazy` etc.; the ternary format already matches disp's) + a
   differential conformance test.
4. tc-net later slots into the identical shape.
