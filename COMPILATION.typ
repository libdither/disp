#set document(title: "Disp Compilation Pipeline")
#set page(margin: 2cm, numbering: "1")
#set text(font: "New Computer Modern", size: 10.5pt)
#set heading(numbering: "1.")
#show heading.where(level: 1): set text(size: 18pt, weight: "bold")
#show heading.where(level: 2): set text(size: 14pt)
#show heading.where(level: 3): set text(size: 11.5pt, style: "italic")
#show link: set text(fill: rgb("#0b63b0"))

#align(center, text(22pt, weight: "bold")[Disp Compilation Pipeline])
#v(0.5em)
#align(center)[
  How `.disp` source becomes a set of named tree-calculus terms.\
  Complementary to #raw("SYNTAX.typ") (surface grammar + AST) and
  #raw("TYPE_THEORY.typ") (type system semantics).
]
#v(1em)

= Overview

Compilation is a *single driver walk*: `src/parse.ts` tokenizes and
builds the surface AST, while `src/compile.ts` walks top-level items,
maintains scope, resolves `use` files, elaborates expressions, and
emits definitions/tests.

The elaborator is type-directed where annotations are present. It
compiles expressions to tree-calculus terms via bracket abstraction,
uses ordinary scope entries for helper discovery (`Pi`, `Type`, `Hyp`,
etc.), and invokes kernel predicates for typed bindings and
annotations.

#table(
  columns: (auto, 1fr),
  stroke: (x, y) => if y == 0 { (bottom: 0.6pt) } else { none },
  inset: (x: 6pt, y: 4pt),
  table.header[*Responsibility*][*Owner*],
  [tokenize, grammar, braced-form disambiguation],       [parser],
  [loading files for `use` expressions; cycle detection], [parser],
  [`let` / `field` / `open` scope],                      [compiler driver],
  [compile-time `test` evaluation + comparison],         [compiler driver],
  [converting expressions to backend terms],             [elaborator],
  [typed binding checks],                                [elaborator + kernel predicates],
)

There is no `trust` keyword or provenance side table. Elaborator
privilege is type-based: names like `Pi`, `Type`, and `Hyp` are found
in the ordinary scope after imports, and typed terms are accepted when
the relevant type predicate evaluates to `TT`.

= Parser

== Responsibilities

- Tokenize and apply the grammar per `SYNTAX.typ`.
- Produce `Item` / `Expr` AST nodes for `let`, `field`, `test`,
  `open`, `use`, `match`, binders, records, and applications.
- Load files for `use` expressions, holding a cycle-detection stack
  (see § Import cycles).
- Disambiguate braced forms (`recValue`, `recType`, `block`,
  `binder`) using local syntax shape.
- Reject shape errors (empty binder `{} -> body`, `:=`/`:` mixing,
  duplicate BRACED record fields and record-type fields,
  intermediate bare expressions in a Block,
  etc.). Top-level field *redefinition* is legal syntax (a guard-mediated
  rebind request, § Declarations as requests); the unguarded-duplicate
  accident check lives in the driver.

Non-responsibilities: the parser does *no* tree construction and *no*
type checking. Name resolution, file loading, projection metadata,
and annotations are handled by `src/compile.ts`.

== Scope

Every file and inline record/block introduces a scope frame. `let`s
inside braces bind only within those braces. Name lookup walks scope
frames inner-to-outer; inner bindings shadow outer.

```disp
let x := t
{
  let x := t t                 // shadows outer x within this scope
  test x = t t                 // passes against inner x
}
test x = t                     // passes against outer x
```

== `let` desugaring

Every `let` is compiled in source order. A typed `let` first compiles
the annotation as a type, then checks the body against that type.

The binding operator is `:=` only. (The legacy `let x = body` spelling
was removed when `=` became the equation operator; the parser gives a
targeted error pointing at `:=`.)

*Internal* `let x (: T)? := body; rest` (inside a Block): compile
`body`, bind `x` in the local scope, then compile `rest` with `x`
available. Equivalently:

```
App(Binder([{name: "x", type: null}], rest_as_expr), body')
```

where `body'` is checked against `T` if typed else `body`.

*Top-level* `let x (: T)? := body`: an ordinary decorated declaration —
`let` is not a keyword; the head is the library `let` decorator and the
request is private (§ Declarations as requests; the pristine fast path
is byte-identical to direct binding). Top-level lets are private
everywhere. A file that exports nothing of its own (a pure-`open`
barrel) re-exports its opens; the old fieldless mode where top-level
lets exported is gone.

Recursive `let` is not implicit. The new binding takes effect only
after its body compiles; recursive definitions use `fix` explicitly.

== `use` expression

`use "path"` is an atom (see `SYNTAX.typ`) that elaborates by loading
and running the referenced file. Steps:

1. Resolve `path` to an absolute path relative to the directory of the
   file containing the `use`.
2. If the resolved path is on the in-progress stack, emit an import
   cycle error with the full cycle (`a.disp → b.disp → a.disp`).
3. Otherwise push, parse the file to completion (discharging its tests
   and building its `Program`), pop.
4. Return a record value whose fields are the file's exported
   `:=` fields, or all definitions for legacy fieldless files.

Parsed-and-popped files may be revisited via independent `use` chains;
memoizing them is an optimization, not a correctness requirement.

== `test` discharge

`test lhs = rhs` parses both sides, invokes the elaborator on each
(inheriting the surrounding scope), and compares the resulting trees
by hash-cons identity. Hash-cons-equal ⇒ discharged; otherwise a
compile error. The program driver emits compiled test declarations for
the runner.

Tests compare the compiled left and right trees by hash-cons identity.
They are intended for closed compile-time assertions.

== Block simplification

Blocks are surface syntax only. The parser lowers block `let`s into
nested binder applications; tests are discharged by the driver.

= Elaborator

== Overview

The elaborator is integrated into the existing parser-driven
compilation pipeline. It extends `compileExpr` (the function that
converts `Expr` to `Tree` via bracket abstraction) with a single
additional parameter: *mode*, which is either `term` or `type`.

In *term mode*, a `Binder` compiles to a lambda (bracket abstraction)
--- the current behavior. In *type mode*, a `Binder` compiles to a
`Pi` type that *contains* the lambda as its codomain function. All
other expression nodes compile identically in both modes.

This design reflects the Curry-Howard correspondence at the
compilation level: `{x : A} -> B` is simultaneously lambda
abstraction and Pi formation, differentiated only by mode. The bracket
abstraction is shared; type mode wraps the result in `Pi(domain, _)`.

== Contract

```
compile(e: Expr, mode: "term" | "type"): Tree
```

- *Input* — a core expression (`Var | Leaf | Hole | App | Proj |
  Binder | RecValue | Ann`). Surface-only nodes have been processed
  away by the parser.
- *Mode* — determines whether binders compile to lambdas or Pi types.
- *Output* — a tree-calculus term. In type mode, the output is a
  kernel predicate (a function that accepts values and returns
  `TT`/`FF`).

== The binder/Pi correspondence

The central insight: for `{x : A} -> B`, bracket abstraction of `B`
over `x` produces a function `f` where `f(v) = B[x := v]`. This is
*exactly* what a Pi type's codomain function is.

```
compile({x : A} -> B, term) = abstract(x, compile(B, term))
compile({x : A} -> B, type) = Pi(compile(A, type), abstract(x, compile(B, type)))
```

The lambda IS the codomain function. Pi wraps it with the domain.

For non-dependent arrows (`A -> B`, sugar for `{_ : A} -> B`):

```
compile(A -> B, term) = K(compile(B, term))
compile(A -> B, type) = Arrow(compile(A, type), compile(B, type))
                       = Pi(compile(A, type), K(compile(B, type)))
```

Multi-param binders desugar right-associatively. Each parameter layer
wraps the remaining body in `Pi(domain, ...)`:

```
compile({x : A, y : B} -> C, type)
  = Pi(compile(A, type), abstract(x,
      Pi(compile(B, type), abstract(y, compile(C, type)))))
```

== Mode propagation

Mode determines how binders compile but does not affect other nodes.
The rules for which sub-expressions get which mode:

#table(
  columns: (auto, 1fr),
  stroke: (x, y) => if y == 0 { (bottom: 0.6pt) } else { none },
  inset: (x: 6pt, y: 4pt),
  table.header[*Position*][*Mode*],
  [`Ann.type`],                          [type],
  [`Ann.expr`],                          [term],
  [`let` type annotation],               [type],
  [`let` body],                          [term],
  [`Binder` param type (in type mode)],  [type],
  [`Binder` body (in type mode)],        [type],
  [`Binder` body (in term mode)],        [term],
  [`App` function and argument],         [inherit from parent],
  [`RecType` field types],               [type],
  [everything else],                     [inherit from parent],
)

Entry points: `let` bodies start in term mode; `let` type annotations
and `Ann.type` start in type mode. Inside a type-mode binder, both
the param types and the body stay in type mode (types can contain
types). Inside a term-mode binder, the body stays in term mode
(terms contain terms).

== Per-node handling

- `Leaf` --- returns the tree-calculus leaf (both modes).
- `Num` --- builds `succ(...zero...)` via in-scope constructors (both
  modes).
- `Var { name }` --- looks up in scope, returns compiled tree or
  emits free variable for bracket abstraction (both modes, identical).
- `App { fn, arg }` --- compile both in the current mode, apply.
- `Binder` (term mode) --- shadow params, bracket-abstract body.
  Identical to current behavior.
- `Binder` (type mode) --- shadow params, compile param type in type
  mode, bracket-abstract body (compiled in type mode), wrap in
  `Pi(domain, codFn)`. See § The binder/Pi correspondence.
- `RecType` --- compile to a kernel record type (see § Record
  encoding). Only valid in type mode.
- `RecValue` --- Church-encode fields (both modes, identical to
  current behavior).
- `Proj` --- resolve field index from compile-time metadata, emit
  selector application (both modes, identical).
- `Ann { expr, type }` --- the checking node. See § Type checking.
- `Hole` --- not supported in the initial implementation. Future:
  fresh metavariable solved by unification.
- `Use` --- resolved by the parser before reaching the elaborator.

== Type checking

At an `Ann` node (`(e : T)`) or a typed `let` (`let x : T = body`):

1. Compile `T` in type mode → `T_tree` (a kernel predicate).
2. Compile `e`/`body` in term mode → `e_tree` (a value).
3. Apply: `result = applyTree(T_tree, e_tree, budget)`.
4. Assert: `FAST_EQ(result, TT)`. If not, report a type error.
5. Return `e_tree` (the type is erased after checking).

Type checking is function application. The kernel does all the work
--- the elaborator merely calls it.

The elaborator also tries to verify that an annotation is itself a
type by checking it against the canonical `Type` when the kernel
helpers are available. Today `Type` is structural: it recognizes
guarded `predicate_frame` values. A future implementation should make
`Type` more rigorous about validating metadata, codomain functions,
and library type-former laws; for now this is documented future work.

== Kernel Helper Discovery

Certain kernel/library definitions must be in ordinary scope for the
elaborator to construct Pi types, mint hypotheses, and check
annotations. They are imported like any other definitions, typically
through:

```disp
open use "../kernel/prelude.disp"
```

The minimal helper set for typed compilation is:

#table(
  columns: (auto, 1fr),
  stroke: (x, y) => if y == 0 { (bottom: 0.6pt) } else { none },
  inset: (x: 6pt, y: 4pt),
  table.header[*Name*][*Used for*],
  [`Pi`],    [type-mode binder compilation: `{x : A} -> B` → `Pi(A, codFn)`],
  [`Arrow`], [type-mode arrow sugar: `A -> B` → `Arrow(A, B)` (optional; can use `Pi` + K)],
  [`Type`],  [universe checking: verify `T` is a valid type],
)

No provenance bit is attached to these names. The security model is
type-based: a definition is usable if it is in scope and the relevant
kernel predicates accept the terms being checked. If `Pi` or `Type`
is absent, the compiler falls back to untyped compilation for the
affected expression.

== Interaction with existing compilation

The elaborator extends the existing `compileExpr` / `exprToCir`
pipeline, not replaces it. The changes are:

1. `checkAsType` compiles type expressions and turns binders into
   `Pi(domain, codFn)` when `Pi` is in scope.
2. The `binder` case in term checking compiles lambdas and checks
   bodies against Pi codomains when the expected type is Pi-shaped.
3. The `ann` case compiles both sides and runs the check instead of
   merely erasing the annotation.
4. The driver's `runItem` for typed `let`/field items compiles the
   type and checks the body before binding the name.

`RecType` is currently compile-time metadata for projection/open
support, not a first-class dependent record type checker.

== Record encoding

For a RecValue with $n ≥ 1$ fields `f1 := e1; ...; fn := en` of
(possibly-dependent) types `X_1, ..., X_n`, the corresponding record
type is the dependent Church product

```disp
{A : Type} -> ({x1 : X_1, ..., xn : X_n} -> A) -> A
```

inhabited by `{A} -> {k} -> k e1 ... en`. Projection `.f_i` compiles
to `{p} -> p ({x_1 : X_1, ..., x_n : X_n} -> x_i)`.

The empty `{}` is the Church unit `{A : Type} -> A -> A`, inhabited by
the polymorphic identity.

Duplicate field names in a RecValue or RecType are parse errors.
This keeps projection metadata unambiguous; there is no first-wins or
last-wins shadowing rule for exported fields.

== Named / default / reorderable arguments

A function's parameters form a "shopping list" of named requirements. A
*named call* `f { name := val, … }` supplies them in any order, omits the
ones with defaults, and may supply a strict subset to get a residual
function (partial application). This is a pure elaboration-time rewrite to
the canonical *positional* call --- it introduces no kernel or runtime
representation: the `Pi` domain telescope is untouched and the parameter
names live in a host-side per-binding signature.

*Signature tracking.* When the driver compiles a binding `name := value`
(or `name : type := value`), it records `name`'s signature in scope: the
leading run of `value`'s lambda parameters (authoritative for *names* ---
bracket abstraction binds exactly those), each with its optional default
recipe. A default is the param's `:= d` suffix (`{ x : A, y : B := d } ->
…`, now allowed on a binder param) taken from the value lambda, or, failing
that, from the type annotation's `Pi`-binder at the same position (the
doc-style `f : { … , t : Nat := 30 } -> R`). Bindings whose value is not a
leading lambda (e.g. `fix (…)`) get no signature.

*Resolution.* A juxtaposition `f arg …` is a named call when `f` has a
tracked signature and `arg` is a `RecValue` whose field names are all
parameters. (A record whose fields are *not* a subset --- e.g. a
record-domain function `{r} -> r.x` applied to `{ x := … }` --- falls back
to ordinary record application, so positional calls and record-passing are
unaffected.) Resolution walks the parameters in *declared order*; each takes
its supplied field, else its default (with prior parameters' resolved
arguments substituted in, for dependent defaults like `b := double a`), else
becomes a *missing* parameter. The result is `f a0 a1 … an` wrapped in a
binder for each missing parameter.

*The load-bearing property.* Because the rewrite produces an ordinary
positional AST, conversion (O(1) hash-cons identity) makes every
reordering and default-fill compile to the *identical* tree as `f a b`.
Partial application is uniform: missing parameters that form a trailing
suffix η-collapse (the bracket abstractor's `[n] (f a n) → f a`) so a
*prefix* subset is just currying (`f { x := a }` ≡ `f a`); a *non-prefix*
subset (`f { y := b }`, awaiting `x`) yields a genuine residual lambda
`{x} -> f x b` whose later application equals the full call. A residual is
itself positional-only (it carries no tracked signature).

*Surface note.* RecValue fields may be separated by `,` (as well as `;` /
newline), so a named call reads as `f { host := "h", port := 8000 }`.

= Declarations as requests: the guard layer

*Status: implemented (2026-07-05).* Pins: `lib/tests/guards.test.disp`
(the positive lifecycle) and `test/guards.test.ts` (rejections are load
failures, asserted host-side). Guard-free files compile byte-identically
(the fast path below); nothing in this section changes the meaning of
any existing program. One v1 boundary: `Install`-only names (interface
entries) are file-local — a module's export record carries guards only
alongside values, so contract-only names travel once `Module` grows a
guards slot (the reification at the end of this section).

The layer unifies three things as one elaborator operation plus library
policy: private bindings (`let`), licensed redefinition (a name whose
owner demands an equivalence proof before it may be rebound), and
interface modules (names carrying contracts with no implementation
yet). A name's guard is its permanent owner; a declaration is a request
that the owner mediates.

== The model

The elaborator has exactly one state-writing operation:

```
Declare : Name -> Request -> ElabOp Unit
```

A `Request` packages what the declarer wants (optional fields use the
stem-option convention, `t` absent / `t x` present):

```
Request = { value : Opt Tree, ty : Opt Tree, guard : Opt Tree, private : Bool }
```

A guard is a policy function owned by the name. It receives the
incumbent value (a stem-option) and the request, and answers a
`GuardAction` through the ordinary `CheckerResult`:

```
guard       : Opt Tree -> Request -> CheckerResult
GuardAction = < Bind : Tree, Install : Tree, Both : (Tree, Tree) >
-- Ok (Bind v)      bind v as the value
-- Ok (Install g)   set the name's guard, value untouched
-- Ok (Both v g)    both
-- Err              the guard refuses; the module fails to load
```

The handler is the entire elaborator-native mechanism (the fold step of
the future state-monad elaborator):

```
step st (Declare name req):
  old = scope(st)[name]                          -- stem-option
  if present(req.value) and present(old)
     and tree_eq(old, req.value) and absent(req.guard):
       return st                                 -- idempotent redefinition: no consultation
  g = guards(st)[name] or default_guard
  case eval (g old req) of
    Err             -> fail GuardRejected(name)
    Ok (Bind v)     -> scope[name] := v
                       record[name] := v unless req.private
                       queue check (req.ty, v)   -- deferred, abort-only
    Ok (Install g') -> guards[name] := g'
    Ok (Both v g')  -> both of the above
```

Two laws the handler preserves. They are what keep elaboration a clean
state monad and keep hash-cons conversion sound:

1. Scope, record, and guard writes are pure functions of the source.
   Guard evaluation is eager (its output is the bound value) but pure.
2. Checks gate acceptance, never shape. Annotation checks stay deferred
   and abort-only; no check outcome ever selects which tree a name
   binds.

The idempotence rule (a tree-identical rebind with no guard proposal
passes without consultation) is required by the raw-then-checked
bootstrap, and it makes diamond imports of the same definition
harmless. Import collisions on a guarded name are otherwise ordinary
rebinds: a plain imported value hitting a license-guarded name is
refused, and the importing module must rebind explicitly with a
payload the guard accepts.

== Surface and desugaring

```
decl := head? NAME (":" expr)? (":=" expr)?
head := one expression, the request decorator
```

The name is the last atom before `:` or `:=`; everything before it is
the head. At least one of the annotation, the value, or a
guard-proposing decorator must be present. Decorator composition is
ordinary function composition inside the head expression. Head atoms
are line-local, and the newline-crossing expression parser refuses to
consume a line whose bracket-depth-0 tokens reach `:` or `:=`
(`isDeclStart`, the generalization of the `IDENT :=` lookahead): bare
top-level colons never occur mid-expression, so a decorated declaration
can follow a multi-line expression without being swallowed by it.

#table(
  columns: (auto, 1fr),
  stroke: (x, y) => if y == 0 { (bottom: 0.6pt) } else { none },
  inset: (x: 6pt, y: 4pt),
  table.header[*Surface*][*Desugars to*],
  [`X := v`],                        [`Declare "X" (base v)`],
  [`X : T := v`],                    [`Declare "X" (base v with ty T)`],
  [`let X := v`],                    [`Declare "X" (let (base v))` — NOT a special form: `let` is an ordinary head],
  [`guard g X : T := v`],            [`Declare "X" (guard g (base v with ty T))`],
  [`guard g X : T`],                 [`Declare "X" (guard g (sig T))` (an interface entry)],
  [`guard_eq X : T := v`],           [`Declare "X" (guard_eq (base v with ty T))` (a user-defined head; see below)],
  [`X := { new := f, proof := p }`], [`Declare "X" (base <that record>)` (payload records are guard convention, not grammar)],
)

The decorators are ordinary values, not keywords (home: `cut.disp` —
they build Request records and answer through `Ok`/`Err`, which the
dependency-free prelude deliberately lacks; record-update spelling
below is illustrative). `let` itself is one of them: the tokenizer has
no `let` keyword, so `let X := v` reaches the driver as a decorated
declaration whose head is the identifier `let`. The driver references
`default_guard`, `let`, and the `test` equation marker *by name in the
module scope*, with host fast paths of identical semantics taken while
each is unbound (the bootstrap lines before its `cut.disp`/prelude
definition) or pristine (tree-identical to the captured first
definition); this is the established helper-discovery pattern
(§ Kernel Helper Discovery) plus the `tree_eq` native-fast-path
discipline (the disp definition is normative). Shadowing any of the
three re-routes the shadowing scope through the shadowing value: a
custom `let` decorator changes what that scope's `let`s do, a custom
`test` preprocesses its marker-written equation lhs.

```disp
base    := {v}   -> { value := (t v); ty := t; guard := t; private := FF }
let     := {req} -> req with private := TT
guard   := {g}   -> {req} -> req with guard := (t g)
```

*Custom declaration heads are plain definitions.* Because the desugar
fills the request (value, then annotation) *before* applying the head,
a decorator receives the annotation and may compute from it. In
particular, guard constructors that derive their contract from the
declared type are one-liners:

```disp
guard_eq := {req} -> req with guard := (t (license_guard (oeq (stem_child (req.ty)))))
```

```disp
guard_eq sort : Arrow NatList NatList := impl
//  Declare "sort" (guard_eq (base impl with ty (Arrow NatList NatList)))
//  guard_eq reads req.ty and installs license_guard (oeq (Arrow NatList NatList))
```

There is no macro layer: a "new keyword" is a request transformer bound
to a name, definable in any library, shadowable, and (being an ordinary
name) itself guardable.

*Why block `let` stays syntactic.* In declaration position `let`
denotes the library decorator, because module scope is reified (names
are strings in a state, and the desugar quotes them). Inside an
expression block, `let x := e; rest` must bind `x` *lexically* in
`rest`, and `e` may be open in enclosing binder parameters, so `x`
cannot be a tree in any state; introducing a lexical binding into
subsequent syntax is the one thing a value cannot do, in this or any
lambda calculus. If local scopes are ever reified (the self-hosted
elaborator evaluates over an environment), block `let` can join; until
then the word means "bind in the current frame" with two
implementations chosen by frame kind — and the braced parser
recognizes the *identifier* `let` structurally (it is not a keyword
even there).

*Tests are equations.* `test lhs = rhs` is likewise not a keyword form:
the item grammar has an *equation* production `lhs = rhs` (compound lhs
required), and `test` is the prelude identity, so the conventional
marker is an ordinary value riding in the lhs. While `test` is
unbound or pristine the driver peels the marker before compiling —
identical semantics, and peeling restores the true application head so
head-keyed elaboration (named-argument calls) resolves; a shadowed
`test` stays in the compiled lhs and runs.

*Literal names.* The surface desugar only emits literal names, so
dynamic declaration (computing a name and declaring it) is unreachable
from source. This is a legibility choice, not a soundness one: the
handler mediates every `Declare` regardless of how the name was
produced, so the restriction can be dropped in the §15 endgame (module
values containing computed requests) without redesign. Note that
dynamically declared names could not be *referenced* by later source
anyway, which bounds their usefulness to generated-module tooling.

This migration is COMPLETE (2026-07-05): declaration-position `let x = e`
became `let x := e` corpus-wide, the legacy all-lets-export file mode is
retired (the pure-`open` barrel rule replaces it), and `=` is exclusively
the equation (test) operator.

== The bootstrap policy and the inaugural guards

The default guard honors the declarer verbatim. It is *defined in disp*
(`prelude.disp`) and looked up by name at each `Declare`, with a host
fallback of identical semantics for the bootstrap lines before it
exists. Because the lookup is scope-relative, shadowing `default_guard`
changes the ambient policy for *unguarded* names in that scope (a
module can open a stricter ambient, e.g. freeze-by-default); names with
installed guards are unaffected, since their own guard is consulted
instead. The normative definition:

```disp
default_guard := {old, req} ->
  if (is_stem (req.guard))
    then (if (is_stem (req.value))
      then (Ok (Both (stem_child (req.value)) (stem_child (req.guard))))
      else (Ok (Install (stem_child (req.guard)))))
    else (if (is_stem (req.value)) then (Ok (Bind (stem_child (req.value)))) else Err)
```

(A request with neither a value nor a guard proposal has nothing to do
and is refused, so a stray valueless declaration is a load error rather
than a silent bind of junk.)

`license_guard` (library, `std/oeq.disp`): rebinding an owned name
means presenting a `{ new, proof }` payload whose proof inhabits
`R old new`. The first implementation of a contract-only name needs no
license (there is nothing to be equivalent to); ownership is never
surrendered (guard proposals are refused).

```disp
license_guard := {R} -> {old, req} ->
  if (is_stem (req.guard)) then Err
  else {
    let v := stem_child (req.value)
    if (is_leaf old) then (Ok (Bind v))
    else (match (param_apply (R (stem_child old) (v.new)) (v.proof)) {
      Ok b  => (if b then (Ok (Bind (v.new))) else Err)
      Err _ => Err
    })
  }
```

Other policies are one-liners in the same shape: `freeze := {old, req}
-> Err` (an immutable name); an append-only guard merges `old` with the
payload instead of replacing it. The policy zoo is library code; the
elaborator knows none of it.

== Worked example: an owned name

The owning module states the contract once:

```disp
guard (license_guard TypeFaceEq) Tree : Type := <the full inductive>
```

An optimizations module rebinds by presenting credentials:

```disp
Tree : Type := { new := FastTree; proof := FastTree_eq_Tree }
```

The handler consults `license_guard`, which runs
`param_apply (TypeFaceEq <old Tree> FastTree) FastTree_eq_Tree`
in-language and answers `Bind FastTree`; the annotation check
(`Type FastTree`) is queued as usual. Failure modes, all load
failures: editing `FastTree` so the proof no longer matches; rebinding
with a plain value instead of a payload; proposing a replacement guard.
A contract-only variant makes an interface module:

```disp
guard (license_guard (oeq (Arrow NatList NatList))) sort : Arrow NatList NatList
```

a signature file entry: a name with an ownership contract and no
implementation, satisfiable later by any module whose first bind
type-checks and whose subsequent rebinds present licenses.

== The effects reading and migration

This layer is the elaborator instantiated as the first consumer of the
`TYPE_THEORY.typ` §15 effects design: a module is a value of the free
monad over the elaboration signature (`Declare`, plus `Test` and
`Import`), interpreted by the driver; a guard is a per-name deep
handler installed over `Declare`; the namespace is a handler stack.
Enforcement today is mediation (surface syntax can only file requests;
the handler owns the state). Once `Eff` exists as a library type,
`mod : Eff ElabSig Unit` becomes an ordinary checkable annotation and
guard-respect becomes a property of module values rather than an
elaborator promise.

Migration stages: (0) this section; (1) parser: head-spine declaration
grammar + `let x := e` dual-accept; (2) driver restructured as
request-plus-handler with no behavior change for guard-free files;
(3) `Request`/`GuardAction` as kernel structural types beside
`Action`/`Step`, `license_guard` landed in `std/oeq.disp`, the Tree
walkthrough recast as the inaugural owned name; (4) the self-hosted
handler: requests carry ASTs and the in-language elaborator compiles,
which is the planned `lib/elab` resurrection with its effect signature
fixed in advance.

= Error reporting

Current compile errors carry parser/driver messages, but source-span
plumbing is future work.

#table(
  columns: (auto, 1fr),
  stroke: (x, y) => if y == 0 { (bottom: 0.6pt) } else { none },
  inset: (x: 6pt, y: 4pt),
  table.header[*Kind*][*Source*],
  [*unresolved name*],         [compiler: a `Var` whose name matches neither a scope entry nor a binder param],
  [*type mismatch*],            [elab: type predicate returns FF on a typed `let` body or `Ann`],
  [*failed test*],              [parser: `test`'s two sides elaborate to trees that disagree by hash-cons identity],
  [*unsolved hole*],            [elab: `Hole` not yet supported; compile error],
  [*projection mismatch*],      [elab: `Proj.field` not found on target's record type, or target is not a RecValue],
  [*duplicate field*],          [parser for braced records / record types; DRIVER for top-level exported fields (an unguarded fast-path redefinition — guarded rebinds are legal)],
  [*guard rejected*],           [driver: a declaration request refused by the name's guard (or by a shadowed ambient `default_guard`) — the module fails to load],
  [*import cycle*],             [parser: a `use` transitively references an ancestor file in the parse stack],
  [*braced-form mixing*],       [parser: `:=` with a `NAME : EXPR` field, intermediate bare expressions, `->` on an incompatible shape],
)

== Future diagnostics

Better diagnostics should add source spans, recursive-let hints, and
multi-error accumulation. The current implementation fails fast on the
first parse/elaboration error for a run.

= Emit

The output of compilation is a list of declarations: exported
definitions with compiled trees and tests with compiled left/right
trees. The runner executes tests by hash-cons identity.

There is no runtime test phase: every `test` was either discharged
during parsing or raised as a compile error.

= Reference vs tree-hosted implementation

The host-side implementation (TS parser + TS backend elaborator) is a
*reference*. The long-term target is the elaborator written in disp
itself --- a tree-calculus program consuming a tagged-tree encoding of
the AST and emitting the elaborated tree. Features the TS reference
has that the disp-hosted elaborator need not replicate verbatim:

- *Span tracking* --- the tree elaborator works on span-stripped AST;
  user-facing error display maps positions back via a side table.
- *Multi-error accumulation* --- the tree elaborator may bail at the
  first error.
- *The parser and driver* (tokenizer, grammar, `use` resolution, scope
  management) stay TS-side until the disp self-hosting story matures.

Points of divergence are flagged in the TS source with `TODO(tree-host)`.
