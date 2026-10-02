# Bend Tetris

Tetris in [Bend 2](https://github.com/bendlang/bend) (2.0.34), playable in a native window or a browser.

- `hello.bend`: hello world. `bend hello.bend` prints it.
- `tetris.bend`: the game. It's pure: `update` folds one frame's key presses into the next game, and `picture` draws a game as an `Image` quadtree, text and all. `main` hands both to Bend's `App.run`, which opens a 512-pixel window and calls them 60 times a second.
- `LAWS.bend` / `PROOF.bend`: claims about the game, and their proofs. Four quarter turns return every piece to its spawn shape; every piece spawns fitting on an empty board; R restarts at zero points from any game; and two worked examples of clearing and scoring. `bend PROOF.bend` prints ALL PROOFS CHECK, or fails as soon as one stops holding.
- `web/`: the browser version. `main.js` imports `../tetris.bend` (Bend compiles it to JavaScript) and stands in for `App.run`: it calls `update` every 1/60 s and paints each new picture on a canvas.
- `tetris.html`: the browser version as one self-contained page. Open it from disk. `build.sh` generates it.

## Play

In a browser, open `tetris.html`. Natively (Linux needs `libx11-dev`):

```sh
curl -fsSL https://bend-lang.com/install.sh | sh
./build.sh          # or BEND="bun path/to/bend2/main.ts" ./build.sh from a checkout
./build/tetris
```

Arrows or WASD move, rotate and soft-drop; space hard-drops; P pauses; R restarts; Esc quits.

## Notes

- Bend 2 has no `if`, and `match` only takes a parameter or a pattern-bound field, so conditions become a `Bool` parameter of a small helper def (`free.inside`, `fall.at`). Defs must sit above their callers, and recursion must shrink an argument, so the loops count down `Nat` fuel (`landing`, `nth`).
- A row is one `U32` of ten 3-bit cells, so the board is a plain copyable `List<&2, U32>`. A column left of the wall wraps to a huge `U32`, so `col < 10` checks both walls.
- Piece kinds are a datatype (`I{}` … `L{}`) rather than numbers, so a law can be proven by one case per piece.
- `--verdict` (rechecking with the Lean-proven kernel) needs Lean 4.34; it was not run.
- The first version here was Bend 1 on HVM2 (commit `747dae5`). It needed a patched runtime to be playable, because HVM2 reads output back one byte at a time and leaks what it reads. Bend 2 compiles to plain C and JavaScript, so none of that is needed.
