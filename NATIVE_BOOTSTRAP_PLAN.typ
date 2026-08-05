#set page(margin: (x: 1in, y: 0.85in))
#set text(size: 10.5pt)
#set par(justify: true, leading: 0.64em)
#set heading(numbering: "1.")
#show heading.where(level: 1): it => pagebreak(weak: true) + it
#show raw.where(block: true): it => block(
  width: 100%,
  inset: 9pt,
  radius: 3pt,
  fill: luma(246),
  stroke: luma(220),
  it,
)

#align(center)[
  #text(size: 19pt, weight: "bold")[Native Parser and Elaborator Bootstrap]

  #v(4pt)
  A current-kernel-typed, incremental design

  #v(4pt)
  #text(size: 8.5pt, fill: luma(90))[Repository audit and prototype results: 2026-07-22]
]

#v(12pt)

#block(inset: 10pt, fill: rgb("eef6ff"), stroke: rgb("b8d5ef"), radius: 3pt)[
  *Decision.* Rewrite the parser and elaborator as typed Disp programs, but do not copy the
  current TypeScript call graph. Make the canonical implementation a pull-driven state
  machine that produces one top-level item, state delta, or external request at a time.
  Keep filesystem access and optional execution fast paths outside the language. Ship the
  verified frontend as a compiled artifact, bootstrap it by exact tree equality, and add
  optimized `.opt.disp` replacements only after the readable definitions are correct.
]

#outline(depth: 3)

= What “native” means

In this plan, “native parser and elaborator” means that the semantics of tokenization,
parsing, desugaring, name resolution, module assembly, guard handling, combinator lowering,
and verification scheduling are defined by current-kernel-typed Disp code. It does *not*
mean that Disp code performs operating-system I/O, and it does not require deleting every
host implementation on the first day.

The boundary should be:

```text
host: read bytes, canonicalize path, persist opaque cache, run evaluator
                         |
                         v
typed Disp: Source -> FrontState -> FrontStep
                         |
                         v
host: satisfy NeedFile / display diagnostics / store final artifact
```

The canonical result is the tree emitted by the typed frontend. A TypeScript or Rust
implementation may temporarily execute an equivalent step faster, but it is an optimization
of that definition, not a second source of semantics. A fast path must produce the same
encoded step or module tree on the same input.

This split is useful for three reasons:

- It removes parser and elaborator behavior from an untyped host implementation while
  keeping unavoidable effects narrow and inspectable.
- It changes the evaluator interface from thousands of fine-grained construction and
  inspection calls to a small number of whole-source or whole-step calls.
- It makes parsing, elaboration, and their cache policies ordinary programs that can later
  be measured, licensed, and replaced by optimized definitions.

The parser and elaborator are not by themselves part of the kernel's logical soundness
base: the kernel still checks the produced term. They *are* part of the language's behavioral
contract, reproducible build story, diagnostics, and guard/module semantics. A bad frontend
cannot prove a false kernel judgment, but it can compile a different program, omit a required
verification request, or expose the wrong module field. Exact bootstrap comparison therefore
matters even where soundness alone would tolerate variation.

= Current system and migration surface

== Parser

The current parser is a roughly 1,400-line TypeScript tokenizer and parser-combinator
implementation. It emits the host `Expr` and declaration-member structures consumed by the
elaborator. The grammar is not merely a collection of context-free productions. It contains
several decisions that a replacement must make explicit:

- Newlines can terminate items, equations, application spines, or match arms depending on
  delimiter depth and following tokens.
- Braces can mean a binder, a record type, a record value, or a statement block. Record
  values can contain private `let`, `test`, `open`, and monadic-bind members.
- A top-level declaration can be plain, guarded/decorated, annotated, assigned, or an
  equation that expands a previously introduced function.
- Named arguments and defaults interact with application parsing and later elaboration.
- `open given` has block and line forms.
- Match arms and some declarations use forward scans to find a boundary or a top-level
  punctuation mark.

The first native parser should preserve the current encoded AST and accepted grammar exactly.
Changing syntax while also changing implementation would make differential validation much
less useful. Grammar cleanup can follow after the native parser is the oracle.

== Elaborator

The elaborator is spread across expression lowering, literals and sugar, module scanning,
state, and a large driver. Its jobs include more than `Expr -> Tree`:

- Turn literals, records, sums, recursion, matches, named arguments, and dependent binders
  into a small combinator intermediate representation.
- Eliminate lambdas with S/K/I bracket abstraction and evaluate the final application tree.
- Maintain hermetic module scopes, imports, private bindings, opens, givens, and fills.
- Scan a module's syntactic givens before instantiating it.
- Build abstract module faces using fresh hypotheses and read trees back through them.
- Recognize the pristine built-in `let` and `test` values, the current default guard, and
  installed per-name guards.
- Route every binding or reassignment through the declaration/guard protocol.
- Issue and consume collision certificates for allowed `open` collisions.
- Queue type-verification work and decide when it becomes observable.

These are one ordered semantics. Splitting the rewrite into “expression compiler” and
“driver” milestones is useful, but the final API must not let the host silently reimplement
the driver decisions.

== Evaluator boundary

The session API already exposes construction (`leaf`, `stem`, `fork`), application, optional
lazy application, classification, equality, scopes, snapshots, and statistics. It is enough
to run the current elaborator against multiple evaluators, but it is too fine-grained as the
per-node boundary for a native frontend. Crossing a foreign boundary for every token, AST
node, lookup, or combinator application would likely erase the benefit of the rewrite.

The desired steady-state operation is one source message in and a result, request, or bounded
batch of results out. In-process backends can still use the session operations internally.
Subprocess and hardware-backed evaluators become practical when the frontend exchange is
coarse.

== Existing self-hosting material

The old self-hosted elaborator stages are useful as a semantic test oracle, not as the design
to resume unchanged. The recovered implementation encoded its AST and combinator IR as raw
trees, used untyped pattern matching, and deliberately omitted annotations around functions
that inspected those trees. Its bracket, AST, and expression-compiler stages did establish a
valuable precedent: randomized and golden tests can compare the native result with the host
result by exact hash-consed tree identity.

A previous self-hosting plan recorded those experiments, but its implementation stages were
removed and its parser and module-driver stages were never started. Its proposed structure
predated current coproduct recursors, guard behavior, and module verification, so the obsolete
plan has been deleted; its useful validation results and bracket-abstraction lesson are
preserved here and in repository history.

= What current-kernel typing requires

== Honest input types

Surface `String` is presently permissive and does not give typed Disp code a structural
eliminator over its characters. Runtime strings already have a codepoint-list tree shape, but
annotating a parser as `String -> ...` would not make recursive inspection of that shape
honestly typed.

The native boundary should therefore use an explicit type such as:

```text
SourceText := List Nat
SourceChunk := { id : ChunkId, text : SourceText, digest : Digest }
```

The host can expose the existing string tree under this honest type without changing its
physical representation. A future kernel or standard-library string type may add richer
invariants, but it is not a prerequisite. File names and diagnostic labels may remain opaque
strings where the typed code compares or carries them without destructing them.

== Recursive data

`Coproduct`, `Rec`, `rec_value`, `case_value`, and `Coproduct_ctx` are sufficient to define
typed tokens, syntax, combinator IR, parser states, diagnostics, and most mutually recursive
families. Small type-specific wrappers around generic recursors are important. They give the
kernel an explicit, reusable type and avoid repeatedly asking the elaborator to infer a large
generic telescope.

There are two constraints to design around:

- Generic recursion under an arbitrary functor is not complete. Avoid a representation whose
  only natural encoding needs unsupported `RecUnder F` traversal. Use a unified syntax-node
  coproduct, sibling recursive sorts in one context, or a dedicated list/syntax recursor.
- Large mutually recursive folds can create large bracket-abstracted terms and expensive
  verification. A small explicit state machine with shallow step functions is easier for the
  current kernel than a direct transliteration of nested host visitors.

Typed code must use the recursor for the type it is inspecting. Raw `match`, raw triage of a
hypothesis, host-style `if`, and untyped boolean operators can compute but fail current-kernel
checking. Branch on typed booleans with `bool_rec`; branch on sums with `case_value`; recurse
with a type-specific wrapper over `rec_value`.

One syntax trap deserves an explicit rule: where a type is returned as a value, write
`Pi A ({_} -> B)`, not the surface `A -> B`. The latter can parse as a term-level lambda in
value position. This exact issue made a working lazy-recursion prototype appear ill-typed
until the motive used an explicit `Pi`.

== Two combinator IRs

The current host IR permits literals, variables, applications, lambdas, S, K, and I in one
recursive type. A direct typed implementation of `eliminateLams` using a general fixed point
was both difficult for the recursion gate and extremely expensive.

The cleaner design is to make the invariant visible in the type:

```text
PreCir  = Lit Tree | Var Name | App PreCir PreCir | Lam Name PreCir | S | K | I
FlatCir = Lit Tree | Var Name | App FlatCir FlatCir              | S | K | I

abstract  : Name -> FlatCir -> FlatCir
eliminate : PreCir -> FlatCir
emit      : FlatCir -> Tree
```

`eliminate` is a structural fold over `PreCir`. Its lambda case receives an already flattened
body and calls `abstract`. `abstract` is a structural fold over `FlatCir`. No general fixed
point is needed, and the type prevents a supposedly lowered tree from retaining a lambda.

The exact current S/K/I rules can also be lazy. Give the `FlatCir` fold a motive
`Pi Unit ({_} -> FlatCir)`. Its induction hypotheses become thunks. In the application case:

- Return the unmodified function for eta reduction before forcing either transformed child.
- If the variable is absent, return `K (f x)` without forcing either transformed child.
- Otherwise force the children needed for `S`, `S (K p) I -> p`, or
  `S (K p) (K q) -> K (p q)`.

This is genuine laziness inside current typed recursion. It does not depend on the evaluator's
optional `applyLazy` interface, and it keeps the termination argument structural.

== Tree inspection

Abstract module faces currently need to inspect and substitute through a compiled tree. The
typed `Tree` induction principle and `tree_rec` can replace the driver's host `classify` walk.
This path should be implemented in typed code even if a backend later recognizes and
accelerates the canonical fold. Otherwise module abstraction would remain a hidden host
semantic.

= Proposed architecture

== One pull pipeline

Use a sequence of small, persistent machines rather than one function that eagerly returns a
whole module:

```text
chunked source
    -> pull lexer
    -> annotated token stream
    -> deterministic parser machine
    -> top-level item stream
    -> elaboration transducer
    -> verification-obligation stream
    -> module artifact / diagnostic
```

Each layer asks the previous layer for only the next unit it needs. The public frontend state
contains the current layer states and persistent environment. The public step result is a
typed coproduct:

```text
FrontStep =
    NeedFile ModuleRequest FrontState
  | EmitItem ItemResult FrontState
  | NeedVerify VerificationBatch FrontState
  | Done ModuleArtifact
  | Failed Diagnostic

front_start : FrontConfig -> SourceChunk -> FrontState
front_step  : FrontState -> FrontStep
```

The continuation does not need to be an opaque host callback. `FrontState` is ordinary typed
data, so the host can serialize it, memoize it, or resume it in another evaluator session.
Requests should contain stable identifiers and content digests rather than host handles.

This API supplies three useful modes without separate implementations:

- A command-line build repeatedly steps until `Done`.
- An editor stops after the item containing the cursor and delays later work.
- A backend fast path runs many internal steps and returns a bounded batch or the first
  external request.

== Source chunks and positions

Represent a file as a persistent rope or list of hash-consed chunks. A chunk should normally
end at a line boundary, but the lexer state must also support arbitrary splits. A checkpoint
records the chunk identity and a small lexical state such as normal text, string, line comment,
or block comment.

Do not put absolute byte offsets or line numbers into every token's cache identity. A token
span should be relative to its chunk or previous token and carry only length plus newline
deltas. Absolute display positions are derived when a diagnostic is rendered. Inserting one
line at the start of a file then preserves the identities of unchanged later token subtrees.

The first implementation can use a readable list of chunks. A balanced rope belongs in an
`.opt.disp` file after edit traces demonstrate that it matters.

== Lexer

The lexer is a deterministic transition function over `(LexState, SourceCursor)`. One step
either consumes at least one codepoint, emits one token, requests another chunk, or reports a
diagnostic. The progress measure should be present in the result type or represented by an
explicit consumed-count field checked in tests; an accidental zero-width loop must not be
possible.

Tokens should include identifier, string, number, punctuation, newline, and end-of-file
variants. Preserve newlines as tokens because they are part of the grammar. Intern names and
punctuation only as an optimization; the readable version can carry their codepoint lists.

At chunk boundaries cache `(chunk digest, incoming LexState) -> token segment and outgoing
LexState`. Re-lex after an edit until both the produced segment and outgoing state equal the
old checkpoint. The rest of the token stream can then be reused.

== Token summaries

Several current parser decisions scan forward through tokens to find a top-level colon,
assignment, equality, arrow, or delimiter boundary. Repeating those scans in a lazy parser
would be quadratic. Give each hash-consed token segment a monoidal summary:

- net and minimum delimiter depth for parentheses, brackets, and braces;
- whether selected punctuation occurs at depth zero;
- first and last significant token classes;
- newline presence and indentation metadata if later required.

Combining two summaries is constant-size work. A brace classifier or item-boundary query can
skip entire segments whose summary proves that the sought token is absent. This retains the
current grammar without adopting a full packrat parser.

== Parser machine

Parser combinators are attractive as a specification but are a poor primary runtime here:
they build higher-order closures, duplicate failure paths, and make incremental checkpoints
hard to inspect. Define the native parser as a deterministic pushdown machine with explicit
frames:

```text
ParseState = { cursor, frames, delimiter_stack, best_failure, item_start }
ParseStep  = Shift ParseState
           | Reduce SyntaxNode ParseState
           | ItemDone ItemSyntax ParseState
           | ParseFailed Diagnostic
```

Frames correspond to application spines, annotations, binders, record members, match arms,
and top-level declarations. Brace classification uses token summaries plus a bounded local
lookahead, then enters one explicit submachine. Alternative parses do not retain complete
branches; only the farthest/most-specific expected-token set is kept for diagnostics.

A parser checkpoint is valid at every top-level item boundary. It contains the token-tail
identity, the small layout/delimiter state, and any compatibility state needed for equations.
After an edit, restart at the nearest preceding checkpoint and parse until a newly produced
item and outgoing checkpoint match an old pair. Unchanged suffix items can then be reused.

The initial parser should still have a total whole-file wrapper:

```text
parse_file : SourceText -> Result Diagnostic (List ItemSyntax)
```

It is a loop over `ParseStep`, not a second parser. This wrapper makes corpus comparison and
bootstrap tests straightforward.

== Given-header scan

The current module loader needs the syntactic given names before it instantiates the module.
For compatibility, add a shallow item-skeleton pass that recognizes top-level headers and
skips bodies with delimiter summaries. It can return givens without allocating complete
expression syntax. The regular parser later consumes the same token stream.

Longer term, requiring givens in an explicit module header would remove this whole-file
dependency and improve true prefix laziness. That is a syntax decision, not a bootstrap
requirement. Until then, the header scan is the one operation that may inspect the full file,
but it need only inspect summaries and declaration heads.

== Syntax representation

Preserve the existing AST distinctions for the differential phase, while making spans and
mutual recursion typed. A practical representation is one `FrontSyntax` coproduct containing
expression and member nodes, with smart constructors exposing narrower `ExprSyntax` and
`MemberSyntax` views. This avoids requiring a general nested-functor recursion feature.

Every node should include a stable structural digest or be hash-consed by the evaluator. Do
not include derived absolute positions in that identity. Diagnostics can retain the relative
span separately.

Desugaring should be explicit and staged:

```text
ItemSyntax -> CoreSyntax -> PreCir -> FlatCir -> Tree
```

Named-argument matching, binder-to-Pi conversion, records, sums, recursion, `if`, and `match`
belong in `CoreSyntax -> PreCir`. Keeping `CoreSyntax` lets tests compare desugaring separately
from bracket abstraction and makes dependency reads visible.

= Incremental elaboration

== Persistent state and item deltas

Treat elaboration as an ordered transducer:

```text
elab_item : ElabState -> ItemSyntax -> ElabOutcome

ElabOutcome =
    ItemOk ItemResult StateDelta DependencyLog ElabState
  | ItemNeeds ModuleRequest SuspendedItem
  | ItemError Diagnostic
```

`ElabState` contains bindings, exports, guards, fills, open/module metadata, pending
verification obligations, and the pristine built-in identities. For the readable version,
use persistent association lists and explicit lookup functions. They are easy to audit and
their dependency behavior is obvious. Replace hot maps with a persistent trie/HAMT only in
`.opt.disp`, after exact result tests and edit-trace benchmarks justify it.

A `StateDelta` is a replayable list of ordered effects: add private binding, add export,
install guard, record open, enqueue obligation, or update module metadata. It is not an
unordered map diff. Guarded reassignments and opens are barriers whose order is observable.

== Dependency-projected cache keys

Keying an item by the identity of the entire incoming environment would invalidate every
later item after any earlier edit. Instead, record every semantic read during elaboration:

- resolved binding name and binding identity;
- named-parameter signature inspected by an application;
- the identities of `Pi` and other syntax/former values used by desugaring;
- pristine/current identities of `let`, `test`, and `default_guard`;
- the installed guard consulted for a declaration;
- module identity, fill identities, and abstract-face identity;
- any policy/version flag that changes compilation.

The reusable entry is conceptually:

```text
{ syntax_id,
  frontend_version,
  reads : List Dependency,
  result,
  delta }
```

On replay, resolve only the logged dependencies in the new state and compare their identities.
If all match, apply the cached ordered delta without recomputing the item. The lookup structure
may additionally index by `(syntax_id, dependency_projection_digest)`, but the explicit log is
the correctness definition.

The read log must include failed lookups. For example, adding a previously absent name can
change whether a record pun, open, or decorated declaration resolves. Represent absence as a
dependency on the relevant scope/version or on an authenticated negative lookup proof.

== Invalidation algorithm

For a file edit:

1. Re-lex changed chunks until the token checkpoint converges.
2. Reparse from the previous item checkpoint until item syntax and outgoing parser state
   converge.
3. Starting with the first changed item, attempt to replay each old elaboration entry.
4. If the syntax and all logged dependencies match, apply its delta and continue.
5. Otherwise elaborate the item, replace its entry, and continue. Stop as soon as the entire
   remaining state/checkpoint identity equals the old one.

This handles the common local-edit case without pretending items are independent. A change to
the type of a widely used name correctly invalidates consumers; a change to an unused private
binding does not.

== Laziness and evaluator memoization

The pull interface determines what is demanded. Hash-consing and the evaluator's application
memo then reuse deterministic calls on identical source tails, parser states, syntax nodes,
and environment projections. No special “incremental evaluator” is required for the first
version.

Laziness must remain explicit at coarse boundaries. Current experiments show that merely
using suspended evaluator applications can retain too much graph and run out of memory when
garbage collection is not available. Prefer small data thunks or resumable states with clear
ownership. Force a bounded unit, release its scope, then continue.

= Verification, tests, and observability

Elaboration should create typed verification obligations rather than immediately forcing one
large verification record. An obligation contains the type tree, value tree, source span,
binding/module identity, and frontend/kernel version. Its result can be memoized by those
identities.

An obligation may remain lazy only while its result is not observable. It must be forced:

- before a typed binding is exported or opened into another module;
- before a module artifact is declared successfully built;
- before a guard or optimizer license based on that judgment is used;
- when a test result is requested;
- at the command-line build boundary, unless an explicitly partial/editor mode was selected.

Force obligations one at a time or in measured bounded batches. Give each batch an evaluator
scope and call the available scope cleanup afterward. Parallel verification is allowed only
after peak-memory measurements; it is not a default simply because obligations are independent.

Tests are an ordered kind of item. The pristine `test` binding can enqueue a test obligation;
a shadowed `test` is an ordinary user value and must follow ordinary application semantics.
Profiling should attach elapsed time, evaluator steps, and peak/batch memory to every forced
test. The runner should always print the slowest tests or tests over a configured threshold.

= Modules, guards, and external effects

== Pure module machine

The host remains responsible for canonical paths and file contents. Typed code owns cycle
detection, fill checking, hermetic scopes, opens, exports, guards, face construction, and the
module cache decision. The request/reply protocol is data:

```text
ModuleRequest = { requested_path, importing_module, fills, source_span }
ModuleReply   = { canonical_id, source_digest, source : SourceText }
```

The typed module cache key should include at least canonical module identity, source digest,
ordered fill identities, frontend version, and any grammar/configuration version. Caching by
path alone is insufficient when a file changes during a long-lived process.

The host must not decide that a module is “already verified” based only on a path. It can store
an opaque native cache entry, but typed code verifies the key and decides whether to reuse its
artifact.

== Guards and declaration protocol

The native driver must preserve these distinctions:

- The pristine `let` and `test` values have special declaration behavior; shadowed values do
  not.
- Module-local `let` assignments remain private and may shadow according to the current
  module rules.
- Reassignment consults the installed name guard or current default guard.
- A guarded replacement is accepted only after its evidence is checked through the same
  protocol used today.
- An `open` collision is accepted only when the guard/collision rule authorizes it.

The most readable first design replays the declaration request through typed guard functions
and records the decision in the state delta. Collision certificates can remain explicit typed
data produced by that decision. The host must not mint them. If certificate stamping later
becomes a performance bottleneck, a recognized fast path may create the same canonical value
only after exact differential validation.

This area is an ordering barrier for incremental replay. A cache hit for a later declaration
is valid only if its logged guard dependencies and preceding guard state still match.

== Abstract/functor modules

For a module with givens, construct fresh typed hypotheses, elaborate the module under them,
and use typed tree recursion to read back the resulting face and substitute supplied fills.
Keep the fresh-hypothesis scope private to the module operation. The artifact should record
the given order and type identities so that named and positional fills cannot be mixed by a
cache collision.

Face readback is a natural optimized-definition candidate because it walks arbitrary trees.
The canonical readable version should still be `tree_rec`-based and fully typed.

= Bootstrap and trust story

== Seed, stage 1, and stage 2

The first compiled native frontend necessarily comes from the current host frontend. Make
that seed explicit rather than pretending to remove it:

1. The host frontend compiles the typed native frontend sources into a canonical ternary
   artifact. This is the seed artifact.
2. The seed artifact parses and elaborates the same native frontend sources, producing stage 1.
3. Stage 1 repeats the operation, producing stage 2.
4. Require stage 1 and stage 2 to be exactly equal trees, not merely extensionally similar on
   a few examples.
5. Record source digests, kernel artifact digest, evaluator/backend identity, frontend version,
   and resulting tree digest in a small manifest.

For stronger reproducibility, run the bootstrap with two conforming evaluator backends and
compare serialized results. This does not prove the host seed correct, but it catches evaluator-
specific behavior and establishes a fixed point from which the host parser can be retired.

== Shipping

Do not re-elaborate and re-verify the entire native frontend on every invocation. Ship the
verified compiled frontend artifact alongside the kernel snapshot. Startup loads the artifact,
checks its manifest/version, and begins at `front_start`. Development and release workflows
rebuild it when its typed sources or kernel dependencies change.

The readable `.disp` sources remain canonical. The binary artifact is a cache with provenance,
not hand-maintained source.

== Optimized definitions

Follow the repository's intended split:

- Put the simplest readable, structurally recursive implementation in ordinary `.disp` files.
- Put proven-faster replacements in `.opt.disp` files.
- Validate every candidate on golden corpora, randomized generated inputs, edit traces, and
  full bootstrap equality before enabling it.

Current general observational-equivalence licensing is not yet a safe excuse to replace a
reflective frontend function globally. Until the applicable relation and guard are proven
sound for the exact observers involved, require bit-identical encoded results or a narrowly
verified relation at the replacement boundary.

Initially, the existing TypeScript parser/elaborator can remain as an oracle and optional fast
path. It must consume and emit the native encoded types, and every fast-path result should be
sampled or exhaustively compared in validation builds. Once native performance is adequate,
remove the host semantics and retain only I/O/trampoline code.

= Expected overhead

== Costs that typing does and does not add

Type annotations and proofs mainly cost verification when the frontend module is built. They
do not need to be rechecked for every source file when the verified artifact is shipped.
However, the chosen recursors, case functions, persistent data structures, and bracket-
abstracted programs are still ordinary runtime terms. Poor representations can therefore be
expensive even after verification.

Likely costs are:

- Codepoint-list lexing performs calculus reductions per character and allocates typed token
  nodes.
- Association-list environments make lookup linear in scope size.
- Recursive coproduct cases and their telescopes create large terms during frontend build.
- S/K/I abstraction can duplicate structure, especially in large mutually recursive visitors.
- Absolute source positions can destroy sharing after an edit if represented naively.
- Unbounded lazy suspensions can retain whole evaluator graphs.
- A per-node FFI design would dominate all other costs.

Likely savings are:

- Whole-source calls avoid the current construction/classification boundary chatter.
- Hash-consing shares repeated tokens, syntax tails, environments, and unchanged item results.
- Pull evaluation avoids parsing or elaborating suffixes an editor does not request.
- Dependency-projected item caching avoids recomputing unrelated later items.
- The `PreCir`/`FlatCir` split removes a costly general fixed point and encodes a useful
  invariant.
- Shipping the verified frontend removes cold verification from normal use.

== Measurements from this investigation

The following are measurements on the current repository and native eager evaluator. They are
directional probes, not a projected full-frontend benchmark.

#table(
  columns: (1.5fr, 1fr, 1fr, 1.2fr),
  inset: 5pt,
  align: (left, right, right, left),
  [*Probe*], [*Elapsed*], [*Peak RSS*], [*Result*],
  [Host TypeScript parser, one 560 kB `lib` corpus pass], [224 ms], [not isolated], [117 files; 2.38 MiB/s],
  [Typed recursive IR and simple two-phase abstraction, cold], [about 36 s], [about 1.12 GiB], [13/13 tests],
  [Same prototype after the kernel was warm], [2.90 s incremental], [not isolated], [22.54M added steps],
  [Exact lazy S/K/I abstraction, cold], [36.66 s], [1.113 GiB], [19/19; 263.67M steps],
)

The host parser corpus measurement ran 25 in-process passes after warmup; it includes all
current `lib/*.disp` files and the temporary probe used for this investigation. It measures
only tokenization and parsing, not imports or elaboration.

A direct typed bracket abstraction using a general fixed point was the negative result. One
version requested a 1.5 GiB arena allocation and failed under a 12 GiB virtual-memory cap after
roughly 43 seconds. A reordered version avoided the immediate allocation but exceeded two
minutes with several GiB resident and was stopped. This is why the two-IR structural design is
a requirement, not just aesthetic cleanup.

The cold typed-prototype number includes loading and checking the current kernel/prelude, so it
does not represent per-file runtime after shipping a verified artifact. The warm delta shows
that even a small typed frontend component remains nontrivial today. The plan should therefore
set budgets from measurements at every milestone and keep the host fast path until native
execution meets an agreed interactive target.

No honest full parser or full driver performance estimate is available yet. Character-level
native lexing, environment lookup, and module cache behavior must be prototyped before choosing
a delivery date or promising parity.

== Performance targets

Record separate budgets rather than one wall-clock number:

- frontend artifact build: elapsed time, steps, and peak RSS;
- cold artifact load: elapsed time and mapped/resident bytes;
- clean whole-corpus build: parse, elaboration, and verification time;
- one-character edit: tokens/items invalidated, evaluator steps, latency, and retained memory;
- module import hit/miss: requests, re-elaborated items, and verification obligations;
- individual tests: elapsed time and evaluator steps, with slowest tests reported.

The first acceptance target should be boundedness and correct reuse, not immediate host parity.
For example, a local edit should re-lex a bounded region and re-elaborate only dependency-
affected items even if each native step is initially slower.

= Validation workflow

== Test layers

Every stage needs both semantic and resource validation:

1. *Constructor/recursor tests.* Explicit `typecheck` or `param_apply` tests for every public
   typed function, including motives and case telescopes.
2. *Golden grammar tests.* Every current syntax form, error, newline edge, brace classification,
   decorated declaration, given, and match boundary.
3. *Differential property tests.* Generate host AST/IR values and require exact native/host
   encoded equality. Shrink failures to committed regression cases.
4. *Corpus tests.* Parse and elaborate every repository `.disp` file with both frontends and
   compare item streams, final exports, tests, and diagnostics.
5. *Module/guard tests.* Hermeticity, cycles, fills, abstract faces, opens, private lets,
   pristine versus shadowed `let`/`test`, default guards, installed guards, and allowed/denied
   reassignments.
6. *Incremental traces.* Apply edit sequences and compare every incremental result to a clean
   rebuild. Assert upper bounds on invalidated chunks/items for controlled edits.
7. *Bootstrap tests.* Stage-1 equals stage-2 exactly and agrees across selected evaluators.
8. *Resource tests.* Run under explicit maximum memory, collect per-item/per-test profiles, and
   flag long tests.

Random generators should operate at three levels: well-formed syntax trees, token streams with
layout noise, and source-preserving edits. Malformed-token fuzzing is necessary for diagnostic
and progress behavior; every parser step must either consume, reduce a bounded frame, finish,
or fail.

== Instrumentation to build first

Add one reusable frontend trace format rather than ad hoc logging. A trace event should carry:

```text
phase, module_id, item_id, action,
input_identity, output_identity,
steps_delta, elapsed, retained_nodes,
cache_hit_or_miss, invalidation_reason
```

The host supplies elapsed time and backend memory counters; typed code supplies semantic
identities, dependency logs, and invalidation reasons. A trace can then explain why an edit
re-elaborated a suffix instead of merely saying the cache missed.

The test runner should summarize total time, maximum memory, slowest tests/items, cache hit
rates, and the largest invalidation fan-out. Keep trace generation optional so normal execution
does not construct unused diagnostic data.

== Development loop

For each milestone:

1. Write the readable typed definition and focused tests.
2. Run it with a warm kernel for iteration, under a memory ceiling.
3. Run cold verification and record steps/RSS before merging the milestone.
4. Differentially compare against the current host oracle.
5. Add randomized inputs and commit minimized failures.
6. Only then benchmark or introduce an `.opt.disp` replacement.

Do not use a giant end-to-end native file as the first integration point. Large failures in
current-kernel checking are expensive and hard to localize. Compile small modules with explicit
interfaces and combine their already verified artifacts.

= Implementation sequence

== Milestone 0: freeze the contract and harness

Deliver:

- Canonical encodings for tokens, spans, current AST/items, diagnostics, dependency logs,
  module requests, and frontend steps.
- A host encoder/decoder and exact equality comparator for those values.
- Parser/elaborator corpus snapshots generated from the current host oracle.
- Random syntax and edit-trace generators.
- Per-item/test timing, evaluator-step, and memory reporting under configured memory caps.

Exit when the current host pipeline can round-trip its own encoded item stream and all grammar,
guard, and module fixtures are represented in the corpus.

== Milestone 1: typed data and combinator lowering

Deliver:

- `PreCir`, `FlatCir`, type-specific recursors, free-variable test, lazy exact abstraction,
  lowering, and tree emission.
- Typed tree readback/substitution needed by abstract module faces.
- Golden and randomized equality against the current host CIR implementation.

Exit when every definition typechecks under the current kernel, direct general recursion is
absent, and random/corpus outputs are exactly identical.

== Milestone 2: expression elaboration

Deliver:

- Typed `CoreSyntax` and staged desugaring for all current expressions.
- Explicit dependency-reader interface for every environment lookup and former used.
- Named-argument, record/sum/recursion, match, `if`, literal, and binder tests.

The host still supplies parsed items and a simple environment oracle at this milestone. Exit
when native expression results and read logs agree with instrumented host behavior.

== Milestone 3: pull lexer

Deliver:

- `SourceText = List Nat`, chunks/cursors, lexical states, tokens, relative spans, and segment
  summaries.
- Whole-source wrapper and incremental re-lex checkpoints.
- Corpus and malformed-input differential tests.

Exit when all files produce the same token stream and controlled edits converge at the expected
checkpoint under bounded memory.

== Milestone 4: parser machine

Deliver:

- Explicit parser frames, brace classifier, diagnostics, item checkpoints, shallow given-header
  scan, and whole-file wrapper.
- Exact AST and error comparison against the host parser.
- Layout/token/edit fuzzing with a progress assertion.

Exit when the full corpus and randomized suite agree, with no unbounded rescan in the tracked
lookahead cases.

== Milestone 5: declaration and module driver

Deliver:

- Persistent elaboration state and ordered deltas.
- Native pristine `let`/`test` handling, declaration requests, default/per-name guards, opens,
  collision decisions, givens/fills, cycles, abstract faces, and verification obligations.
- The `NeedFile` host oracle and content-digest module cache.

Exit when complete clean builds, exports, failures, tests, and module/guard behavior match the
host oracle on every fixture.

== Milestone 6: incremental replay

Deliver:

- Read logging, negative dependencies, projected cache validation, item-delta replay, and
  convergence checks.
- Editor-style stop-at-item operation.
- Incremental-versus-clean edit trace tests and invalidation explanations.

Exit when the controlled edit suite shows bounded lexer/parser work and only dependency-affected
elaboration, with exact clean-build equality after every edit.

== Milestone 7: bootstrap and default switch

Deliver:

- Seed/stage-1/stage-2 build workflow and manifest.
- Cross-evaluator artifact comparison.
- Shipped verified frontend artifact and version checks.
- Native default path with host parser/elaborator retained as an opt-in oracle.

Exit when stage 1 equals stage 2, all tests pass under the native default, memory caps hold, and
clean/incremental performance is reported. Remove the host semantic path only after it has no
unique coverage and the native path meets the chosen operational budgets.

== Milestone 8: measured optimization

Profile real builds and edit traces. Candidate `.opt.disp` replacements are likely to be the
source rope, name/environment map, token-summary search, parser batching, tree readback, and
whole-frontend recognized execution. Each replacement must retain the readable definition and
pass exact or appropriately licensed equivalence tests.

= Suggested source layout

Keep modules small enough to verify and profile independently:

```text
lib/frontend/
  source.disp              # honest codepoint text, chunks, spans
  token.disp               # token types and lexical states
  lex.disp                 # pull lexer
  token_summary.disp       # delimiter/lookahead summaries
  syntax.disp              # typed AST and item members
  parse_state.disp         # frames and deterministic step machine
  parse.disp               # whole-file wrapper and header scan
  core_syntax.disp         # explicit desugaring target
  cir.disp                 # PreCir / FlatCir and recursors
  bracket.disp             # readable exact abstraction
  expression.disp          # CoreSyntax -> PreCir
  dependency.disp          # reads, negative reads, validation
  state.disp               # scopes, bindings, guards, ordered deltas
  module.disp              # module machine and abstract faces
  verify.disp              # obligation scheduling
  frontend.disp            # public FrontState / FrontStep API

lib/frontend/*.opt.disp    # only measured, validated replacements
lib/tests/frontend/        # golden, random, module, edit, bootstrap tests
```

The final exact names can follow repository conventions, but the semantic boundaries should
remain. In particular, do not recombine lexer, parser, driver, and all tests into one module;
the current verification cost makes that hostile to iteration.

= Risks and decisions

#table(
  columns: (1.25fr, 1.8fr, 2.3fr),
  inset: 5pt,
  align: left,
  [*Risk*], [*Consequence*], [*Mitigation / decision*],
  [Typed character processing is too slow], [Native parser misses interactive targets], [Use honest `List Nat` first; measure lexer alone; batch whole segments; keep validated host fast path until an `.opt` lexer is proven.],
  [Large recursors explode during checking], [Frontend artifact cannot be built reliably], [Small modules, type-specific wrappers, explicit structural machines, `PreCir`/`FlatCir`, no general fixed point.],
  [Lazy graphs retain memory], [OOM despite low demanded work], [Explicit resumable states, bounded verification batches, evaluator scopes/cleanup; do not rely on `applyLazy` alone.],
  [Incremental cache reuses stale semantics], [Wrong program after an edit], [Ordered deltas, positive and negative dependency logs, clean-build comparison after every generated edit.],
  [Host keeps hidden semantics], [Rewrite is native in name only], [Host protocol limited to path/content/time/cache storage; native code owns guard, module, and verification decisions.],
  [Fast path diverges], [Backend-dependent language], [Canonical encoded step API, exact differential tests, bootstrap equality, versioned manifest.],
  [Given scan forces whole file], [Prefix parsing is not fully lazy], [Summary-only skeleton scan now; consider explicit given header as a later syntax change.],
  [Current equivalence licensing is too broad], [Unsound optimized replacement], [Require exact output identity until a narrow, observer-correct relation is established.],
  [Diagnostics regress], [Correct parser is unpleasant to use], [Carry farthest structured failure and spans; compare committed malformed-input corpus, not only successful ASTs.],
)

Three design decisions should remain firm unless a prototype disproves them:

1. The canonical frontend is typed Disp; host implementations are effects or validated
   optimizations.
2. Incrementality is item/checkpoint/dependency based, not a memo over the entire environment.
3. Structural state machines and two-phase IRs are preferred to general recursion and direct
   host-code transliteration.

Questions that can stay open until measurement:

- Exact chunk size and whether the readable rope needs balancing.
- Whether parser steps should return one transition or a bounded batch.
- The environment map selected for `.opt.disp`.
- Verification batch size and safe parallelism.
- The performance threshold for disabling the host frontend by default.
- Whether syntax is later changed to put givens in an explicit header.

= Immediate next experiment

Before writing the full frontend, implement a thin vertical slice:

1. Honest `SourceText` and a chunked lexer for identifiers, punctuation, whitespace, and
   newlines.
2. A parser machine for imports, plain declarations, applications, and lambdas.
3. `CoreSyntax -> PreCir -> FlatCir -> Tree` for that subset.
4. A two-item persistent environment with dependency logs and replay.
5. One edit trace that changes the second item and proves the first item's token, syntax,
   lowering, and verification identities are reused.

Run it both cold and from the shipped/warm kernel, under the normal maximum-memory constraints.
This slice answers the largest remaining unknown—native character/token throughput—while also
exercising the final incremental boundaries. If it performs poorly, optimize or recognize the
lexer step without discarding the typed parser/elaborator design.

= Completion criteria

The rewrite is complete when:

- Parser, elaborator, module, guard, and verification-scheduling semantics are current-kernel-
  typed Disp definitions.
- The host boundary contains only file/path services, diagnostics presentation, opaque cache
  storage, evaluator execution, and measurement.
- The native whole-file result matches the former host pipeline on the full corpus, randomized
  cases, malformed inputs, and module/guard fixtures.
- Stage 1 equals stage 2 exactly and the artifact manifest is reproducible.
- Incremental edit results equal clean rebuilds after every tested edit and invalidate only
  logged dependents.
- Tests automatically run with maximum-memory limits and report slow tests/items.
- Cold build, warm load, clean build, local edit, module hit/miss, steps, and peak memory are
  measured and kept within agreed budgets.
- Any enabled `.opt.disp` or host/backend fast path is tied to the readable definition by exact
  comparison or an adequately narrow verified license.

At that point TypeScript can stop defining the language. It becomes a small shell around a
typed, inspectable, lazily demand-driven frontend that can itself participate in Disp's longer-
term optimization loop.
