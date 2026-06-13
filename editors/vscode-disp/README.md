# Disp for VS Code

Syntax highlighting for `.disp` files (TextMate grammar, no language server).

Highlights: keywords (`let`, `test`, `use`, `open`, `match`, `if`/`then`/`else`),
the bare leaf `t` / `△`, the hole `_`, strings, numbers, comments (`//`, `/* */`),
exported-field and `let` definition names, projections (`.field`), operators
(`:=`, `->`, `=>`), and capitalized identifiers as types/constructors.

## Install (local)

```sh
ln -s "$(pwd)/editors/vscode-disp" ~/.vscode/extensions/zyansheep.disp-lang-0.1.0
```

Then reload VS Code (`Developer: Reload Window`). Grammar lives in
`syntaxes/disp.tmLanguage.json`; edit + reload to iterate.
