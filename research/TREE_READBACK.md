# Reading trees back as source

A compiled disp program is one tree calculus tree, and so is every piece of it a running program
handles. This note asks how to show any such tree as what it means in the source: which definition,
which sub-expression, with which arguments filled in, in disp's own syntax, at whatever fidelity the
information at hand allows. Prototype and measurements: `research/tree-readback/` (`run.sh`
reproduces them).

## Summary

- **Today every printer names a tree only when it is a whole known definition.** There are five
  printers (CLI `src/format/print.ts`, `prettyTree` in `src/core/tree.ts`, the playground's
  `website/src/lib/disp/runner.ts`, the teaching viewer's `treecalc.ts`/`TreeVis.svelte`, and the
  strand-lattice player's details drawer). The player also marks any bigger piece of a definition as
  `‹fib›` ("somewhere in fib's code"). In a `fib 3` run at clock 4000, 368 of 957 agents read as or
  contain `‹…›`, e.g. `‹fib› x₁ (‹486› x₁) (t t x₁) (‹fib› ‹715› 1)`.
- **The compiler can say exactly where each piece came from.** Bracket abstraction
  (`src/elab/cir.ts`) builds every piece of code as `[x₁…xₖ] e` for a sub-expression `e` of the
  lowered program under its binders. Recording that per emitted tree (a 50-line patch on a scratch
  copy) gives a table that names 39% of the unnamed code pieces in that run exactly, e.g. a piece as
  `λself n. add (self (nat_pred n)) (self (nat_pred (nat_pred n)))`.
- **The rest are built at run time and read compositionally.** They are table pieces glued by S, K and
  triage shapes (a partial application, a closure). Inverting bracket abstraction around the table's
  hits, with β-reduction on symbolic variables, reads all of them with nothing left opaque.
- **Undoing the elaborator's desugarings brings back the surface.** With three presentation rules
  (`if` from its closure-wrapped `cond`, preferred names for shared trees, `wait omega x` as a delay),
  704 of the 957 agents print as source-level terms: `fib`'s body comes back as
  `λ_ self n. if is_leaf n then 0 else if is_leaf (nat_pred n) then 1 else add (self (nat_pred n)) (self (nat_pred (nat_pred n)))`,
  and a running call as `if is_leaf 2 then 0 else if is_leaf (nat_pred 2) then 1 else add …`. The
  other 236 still show recursion's plumbing (`S (λv0. S omega (λ_. v0)) fib`): the open problem.
- **Code alone is not enough.** Applying a tree to fresh variables and reducing (no tables) does
  recover λ-structure, but compiled disp is unreadable that way: nested `match`es from compiled
  pattern matching, fixpoint plumbing, and anonymous inlined helpers.
- **So the design is one reader graded by information**, with the compiler emitting debug info as a
  table keyed by tree hash, user rules for presentation, and laws that make it the inverse half of a
  parser–pretty-printer pair.

## What the compiler throws away

From source to tree (`src/parse.ts` → `src/elab/expr.ts` `exprToCir` → `src/elab/cir.ts`):
- **Names.** A name in scope becomes its tree (`lit`); a definition is inlined. Whatever name was
  written is gone, and one tree can have many names (`nat_pred` and `pair_snd` are the same tree, the
  leaf is also `b0`), so a hash→name table must guess.
- **Positions.** `Expr` nodes carry no source spans; only top-level items keep their lines.
- **Sugar.** `if c then a else b` becomes `cond c (λfvs. a) (λfvs. b) fvs`, each branch closed over
  its free variables; `match`, records and sum types lower to `prod`, `make_record`, `Coproduct`
  and pairs.
- **Abstraction.** `[x]` uses S, K, I with four rewrites: drop an unused binder (`K e`), η before and
  after recursing (`[x](f x) = f`, `S (K p) I = p`) and `S (K p) (K q) = K (p q)`. Several
  sources compile to one tree, so a reader must pick a canonical one. As Jay notes,
  ["A curious property of bracket abstractions is that they are always stable"](https://raw.githubusercontent.com/barry-jay-personal/tree-calculus/master/tree_book.pdf#page=54):
  compiled lambdas keep their shape until applied, which is what makes reading pieces of a running
  program back possible.
- **Evaluation at compile time.** Any other closed application is run while compiling, so a
  definition is stored as a finished value (`fib`'s tree is `fix` applied to its body, worked out).

## The prototype (`research/tree-readback/`)

1. `cir-origins.patch` + `probe.ts`: each abstraction result remembers its binders and the lowered
   body it abstracts; `cirToTree` records tree → origins; compiling `programs.disp` (prelude and
   list library included) gives origins for 1,054 trees and 81 names.
2. `drawer.mjs`: runs `fib 3` in the player headlessly and dumps every agent's term as the drawer has
   it (applications, shared parts, value trees).
3. `reader.py` reads a tree, first rule that applies:
   - a name, for trees of 7 nodes or more;
   - data (numbers, strings, lists) when not in function position;
   - the origin table: `λvars. body`, the body lowered terms with names, nested origins as lambdas;
   - **partial applications of named definitions**, as tree patterns derived by applying each
     definition to 1–3 holes and reducing symbolically (39 patterns: `cond c`, `wait omega`, …);
   - **bracket abstraction inverted**: `t a` (a stem), `K b` = `λ_. b`, `S p b` =
     `λv. p v (b v)` β-reduced, the triage shape as `λz. match z {…}`, `I` as `λz. z`;
   - then `resugar`: `cond c (λxs. A) (λxs. B) xs` → `if c then A else B`, names by preference
     (`nat_pred` over `pair_snd`, `succ` over `b1`, `0` over `b0`), `wait omega x` → `⌛x`.

   The symbolic evaluator (`nbe.py`) is the triage calculus of `oracle.rs` with variables: a triage on a
   variable is stuck and reads back as a `match`. This is readback with neutral terms, as strong
   reduction does with
   ["run-time representations of non-closed, non-reducible terms"](https://xavierleroy.org/publi/strong-reduction.pdf#page=4).

Results on the `fib 3` run (957 agents, 481 unnamed code pieces):

| | now (drawer) | origin table alone | prototype reader |
|---|---|---|---|
| unnamed code pieces explained | 0 (`‹fib›`, `‹size›`) | 188 (39%; 58 of 115 distinct) | 481 (100%) |
| agents with an unread part | 368 | | 17 |
| agents printed as source-level terms | | | 704 |
| agents with recursion plumbing left | | | 236 |

Samples (drawer now → prototype; λ notation, not disp syntax):

- `‹fib› (‹fib› ‹fib› ‹fib›)` → `λn. if is_leaf n then 0 else if is_leaf (nat_pred n) then 1 else add (… fib (nat_pred n)) (… fib (nat_pred (nat_pred n)))`
- `‹fib› x₁ (‹486› x₁) … where x₁ = ‹fib› (‹fib› ‹fib› ‹fib›) 2` → `if is_leaf (if is_leaf 2 then 0 else if is_leaf (nat_pred 2) then 1 else add …) then … else …`
- the 689-node piece seen 54 times → `fib`'s body under its fixpoint, `λv0 n. … (⌛v0)`

The source it reads back to, `programs.disp`:

```
fib := fix ({self, n} => if (is_leaf n) then 0 else (if (is_leaf (nat_pred n)) then 1 else (add (self (nat_pred n)) (self (nat_pred (nat_pred n))))))
```

What did not work, or only partly:
- **Recursion's plumbing.** `fix f = wait omega (λx. f (wait omega x))` leaves closures around the
  body that the table never saw: they are built by the run, along the player's lazy reduction order,
  and differ from what a symbolic run makes (a 715-node `self` against `fib`'s 713). A heuristic,
  "a tree holding a fix definition's body plus at most 64 nodes reads as that definition", names most
  of them `fib` but also names fix's inner step (which takes one more argument) `fib`, so some reads
  are wrong (`fib 1 (nat_pred 1)`). This needs the compiler's help (below), not a better heuristic.
- **Data or code.** Code that happens to be a list shape reads as data today (`[[0, 1], 0, 1]` in the
  drawer); position (function or argument) helps, origins settle it, types would.
- **Choosing among origins.** One tree can come from several places (`K add` is in `fib`, `mul`, `pow2`);
  the prototype prefers the running program's definition.

## Design: one reader, graded by information

Each level adds information and keeps what the lower ones print:

| level | needs | prints |
|---|---|---|
| 0. shape | nothing | `t`, `t a`, `t a b`; numbers, strings, lists, booleans |
| 1. names | hash → names table | whole definitions; partial applications by derived patterns |
| 2. routing | nothing more | lambdas and `match`es, by inverting S, K, I and triage and reducing on fresh variables |
| 3. origins | the compiler's origin table | which sub-expression of which definition, with its binders, arguments substituted |
| 4. surface | resugaring rules | `if`, `match`, records, sum types, recursion back as written |
| 5. source text | spans and the source file | the original text where a piece is unchanged; residual terms with their substitutions shown |

Level 0 matches the low rung decompilers sit at: without debug info
["comments, variable names, and custom variable types are technically impossible to recover"](https://arxiv.org/pdf/2108.06363#page=1),
and the lowest level can only invent names (`v0`, `x1`). Types would add one more input to level 2,
how many arguments to expand: ["given a compiled normal form and its type, we can recover its text"](https://tidsskrift.dk/daimipb/article/download/7022/5983/22798#page=1).
For terms that do not terminate, the reader prints to a depth and elides, as a normaliser that
["lazily computes the combinatory"](https://www.cse.chalmers.se/~peterd/papers/DybjerKuperberg2008.pdf#page=1)
Böhm tree would.

**Origins as debug info.** The compiler would emit, per module, a table keyed by tree hash (the
elaborator hash-conses already: `internTreeId`): for each emitted piece, the definition, the
sub-expression (span and AST path), the binders' names, the name written at each reference that
resolved to a tree, and marks for sugar (`if` wrappers, `match` lowering, `fix`). This is a source
map in the
["mapping transpiled source code back to the original sources"](https://tc39.es/ecma426/#sec-intro)
sense, the role DWARF plays when
["A source-level debugger needs to know how to associate locations in the source files"](https://dwarfstd.org/doc/DWARF5.pdf#page=166).
It sits beside the tree, not in it: Jay shows one can
["tag a function with additional, recoverable information"](https://raw.githubusercontent.com/barry-jay-personal/tree-calculus/master/tree_book.pdf#page=21)
and ["the tag has no effect on the functional behaviour"](https://raw.githubusercontent.com/barry-jay-personal/tree-calculus/master/tree_book.pdf#page=69),
but tags make trees bigger and runs longer, and would change every definition's hash.
Keyed by a 64-bit hash instead of ternary text, the prototype's 870 KB table for `programs.disp`
would be tens of kilobytes.

**Following a run.** Origin tracking in term rewriting
["establishes relations between each subterm t of a normal form and a set of subterms, the origins of t, in the initial term"](https://ir.cwi.nl/pub/5162/5162D.pdf#page=3).
Here the table does most of that for free, because code is copied whole until applied, and level 2
explains what the run builds from table pieces. A run-time tag per agent (an origin id carried
through rewrites) would cover the rest exactly, at the cost of state per agent (CPU only on the
lattice). Showing each step of a run in surface syntax is resugaring, which lifts
["evaluation sequence in the core"](https://cs.brown.edu/people/sk/Publications/Papers/Published/pk-resuarging/paper.pdf#page=1)
language back to the surface, under the guarantee that
["code originating from the original input program is never hidden by resugaring"](https://cs.brown.edu/people/sk/Publications/Papers/Published/pk-resuarging/paper.pdf#page=2).

**Recursion.** With the origin table marking `d := fix b` and `b`'s hash, the unfolding states can be
recognised by derived patterns: apply `fix`'s definition to `b` and to holes along the same (lazy)
reduction order the evaluator uses, and record each state as "`d`" or "`d`'s step". The player's
readback could also recognise `wait` suspensions directly. Either replaces the size heuristic.

**User rules.** Presentation is a list of two-way rules, each a tree or term pattern with holes and a
surface template, tried in order with fallthrough, as Lean's unexpanders
(["Unexpanders are the inverse of macros"](https://lean-lang.org/doc/reference/latest/Notations-and-Macros/Extending-Lean___s-Output/#meta-links),
["if it fails, then the next unexpander is tried"](https://lean-lang.org/doc/reference/latest/Notations-and-Macros/Extending-Lean___s-Output/#Unexpanders)).
Two-way, because a rule that prints also builds: a bidirectional pattern synonym is
["one that you can use in both patterns and expressions"](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/08/pattern-synonyms-Haskell16.pdf#page=4),
so "`K x` is `t t x`" serves the parser and the printer at once. Left-hand sides stay plain tree
patterns, as Agda's display forms do (["lambdas are not allowed in left-hand sides"](https://agda.readthedocs.io/en/latest/language/pragmas.html#display-pragma)),
which keeps matching cheap. Rules can be scoped (a module, a definition, a kind of value) and cover:
- **names in context**: which of a tree's names to use (`nat_pred` in number code, `pair_snd` in pair code);
- **routing**: show `S f g`, `K x`, `I` as combinators, or expand them to lambdas, or name them
  (`compose`, `const`), per user;
- **sugar**: `if`, `match`, records, sums, recursion;
- **data views**: how a tree reads as a value (unary numbers, the benchmarks' binary numbers, bits, strings),
  in the sense that ["A view allows any type to be viewed as a free data type"](https://www.cs.tufts.edu/~nr/cs257/archive/views/wadler.pdf#page=1).

**Layout** by a pretty-printing algebra with
["a single way to concatenate documents, which is associative and has a left and right unit"](https://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf#page=1),
emitting text for the CLI and HTML with links back to agents (player) or tree nodes (website): Lean's
delaborator does the same, ["The delaborator tracks the position in the original Expr"](https://lean-lang.org/doc/reference/latest/Notations-and-Macros/Extending-Lean___s-Output/#--tech-term-delaborators).

## The parser–pretty-printer adjoint

Compiling forgets (names, layout, sugar, positions); reading back puts back what the information at
hand allows. That is a lens with the source as the view's complement: compiling is `get`; reading
with a source map is `put`, which
["must restore all of the information discarded by the get"](https://www.cis.upenn.edu/~bcpierce/papers/boomerang.pdf#page=2);
reading with no information is `create`, which
["supplies defaults for the information discarded by get"](https://www.cis.upenn.edu/~bcpierce/papers/boomerang.pdf#page=2).
Laws to test:
- **Reading never lies** (every level): `compile(read(t)) = t`, as trees. Since compiling is
  deterministic, the reader must choose, among the many sources of a tree, one that compiles back to
  it exactly (canonical through the η and K-composition rewrites). Property test: every tree in the
  origin table, every piece a run produces.
- **Round trip with full information**: `read_full(compile(s)) = s`, up to layout, and up to nothing
  with spans and comments kept, as BiYacc prints
  ["while preserving layouts, comments, and syntactic sugar"](https://repositorium.uminho.pt/server/api/core/bitstreams/fcc768f0-a1ea-453e-bf8e-51294d043788/content#page=1)
  while ["a reflective printer can accept an AST and an empty piece of program text"](https://repositorium.uminho.pt/server/api/core/bitstreams/fcc768f0-a1ea-453e-bf8e-51294d043788/content#page=2).
- **More information refines**: whatever level 3 prints, level 2 prints a less informative term that
  compiles to the same tree.

Written once, the rules give both directions:
["both parser and pretty-printer can be described as a single program"](https://www.informatik.uni-marburg.de/%7Erendel/unparse/rendel10invertible.pdf#page=1)
from an ["algebra of partial isomorphisms"](https://www.informatik.uni-marburg.de/%7Erendel/unparse/rendel10invertible.pdf#page=1),
or only the printer is written and the parser derived, as FliPpr
["uses program inversion to produce a CFG parser from a pretty-printer"](https://kar.kent.ac.uk/47479/1/local_174795.pdf#page=2).
The nearest worked example of the whole job, decoder plus encoder with
["a correctness proof showing their consistency"](https://www.cse.psu.edu/~gxt29/papers/bigrammar.pdf#page=1),
is for machine code. For disp the payoff runs past the viewers: GOALS.md's optimizer will produce trees
no one wrote, and reading them as source is how anyone checks them.

## One module for all viewers

A shared reader in `src/format/` working over the `Session` interface (`classify`, as `print.ts`
does, so JS trees and evaluator handles both work), taking the tables (names, origins, derived
patterns) and the rule list, returning a document. Consumers:
- the CLI (`run.ts --print`, replacing `printValue` and `prettyTree`);
- the playground and the teaching viewer (`runner.ts` `#render`, `TreeVis.svelte` `nameOf`; the
  teaching viewer also builds `I` differently from the compiler, `fork(stem K, K)` against
  `t (t t t) t`, so a compiled `I` never matches its name there);
- the player's details drawer (replacing `atomOf`/`termHTML`'s naming; its readback already gives the
  term's applications and sharing).
Later, a disp version: rules as disp data and the reader as a disp program, programs as data.

## Steps, each to be agreed

1. Compiler: spans on `Expr`, the name written at each resolved reference, and an origin table emitted
   beside the tree (a flag at first); test that every piece of every compiled definition has an origin.
2. The shared reader, levels 0–3, with the round-trip property tests.
3. Resugaring rules for `if`, `match`, records, sums, and recursion by derived unfolding patterns.
4. Wire into the player's drawer, then the playground and the teaching viewer, then the CLI.
5. User rules and preferences (a file of two-way patterns per module), then a disp-native version.

## Sources not verified

- van Deursen, Klint & Tip, *Origin Tracking* (JSC 1993, doi:10.1016/S0747-7171(06)80004-0):
  the publisher blocks fetching and CWI's scan is garbled; origin tracking is cited through the CWI
  paper quoted above instead.
