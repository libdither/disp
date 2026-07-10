# Disp for VS Code

Syntax highlighting for `.disp` files (TextMate grammar, no language server).

The grammar mirrors the parser's design (SYNTAX.typ, § record members /
declaration requests): almost nothing is a keyword, so highlighting is mostly positional
rather than lexical.

- True keywords: `match`/`if`/`then`/`else` (control), `use`/`open` (modules).
- Structural modifiers, highlighted only in position: `raw` after `use`,
  `given` after `open`. Elsewhere they are plain identifiers, as in the tokenizer.
- Declaration decorators are library values, highlighted only in head position
  (member start, followed by the declared name and `:`/`:=`): `let`, `sig`,
  `base`, `given`, and guard chains `guard <policy> NAME` / `guard_eq NAME`
  with the std policies (`freeze`, `license_guard`, `guard_eq`) colored inside
  the head. Mid-expression uses, e.g. `default_guard t (guard freeze (base zero))`,
  stay plain, because there they are ordinary values. Custom decorator heads
  are possible in the language but not in this closed set, so they stay plain too.
- `test` is highlighted as the conventional equation prefix at member start
  (and the equation's applied head gets call coloring, like any call site);
  `test : Tree -> Tree := …` (the prelude defining it) is a plain declaration.
- Declared names: `entity.name.function` at top level (including after decorator
  heads), `variable.other.member` in braces (record fields, `use "f" { fills }`,
  `open given { … }` entries, named args).
- Constants: booleans `true`/`false`, the bare leaf `t` / `△`, the hole `_`,
  numbers. Plus strings, comments (`//`, `/* */`), projections (`.field`),
  operators (`:=`, `->`/`→`, `=>`, `:`, `=`), and capitalized identifiers as
  types/constructors (which also covers sum-literal tags `< Tag : T, … >`).
- Application heads get call coloring (`entity.name.function`): in disp,
  application is juxtaposition, so an identifier that starts an expression
  (line start, after an opening bracket, separator, `:=`/`:`/`=`, an arrow, or
  `if`/`then`/`else`) and is applied to at least one atom is a call site,
  like Rust's `foo(...)`. So `nat_rec ({_} -> Nat) m step n` colors `nat_rec`
  yellow under common themes.
- Lambda binder params `{n, m} ->` get `variable.parameter`; all other
  lowercase identifiers get `variable.other`. Under default themes both render
  the same light blue as Rust's variables and fields.

Known line-based limits: an annotation split across lines (`name` on one line,
`: T` on the next) loses the name highlight, and brace depth is not tracked
(a multi-line record's members on their own lines get top-level name coloring).

## Color model

The scope choices deliberately mirror Rust's distribution under any default
theme, so no `tokenColorCustomizations` are needed: yellow application heads
against a light-blue body of variables/params/fields, blue declaration
keywords and literals (`let`/`guard`/`test` like Rust's `let`, the leaf `t`
like `true`), purple control flow, teal types. Rust reads well with the same
six colors every language gets; the work is done by coloring the call sites,
and in disp the analog is the head of every application spine.
`palette-preview.html` (generated from real corpus lines plus a snippet from
the rust-ic-net crate) shows disp and Rust side by side under Dark+.

## Install (local)

```sh
ln -s "$(pwd)/editors/vscode-disp" ~/.vscode/extensions/zyansheep.disp-lang-0.3.0
```

Then reload VS Code (`Developer: Reload Window`). Grammar lives in
`syntaxes/disp.tmLanguage.json`; edit + reload to iterate.
