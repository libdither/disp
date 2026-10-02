#!/usr/bin/env bash
# Builds Tetris: build/tetris plays in a terminal, tetris.html in a browser (as WebAssembly).
# Needs bend and hvm (cargo install hvm bend-lang), a C compiler, python, and zig (or ZIG=...).
set -euo pipefail
cd "$(dirname "$0")"
mkdir -p build
bend gen-c tetris.bend | python host/patch.py > build/tetris.c
cc -O2 -w -DPROGRAM='"tetris.c"' -Ibuild host/host.c -o build/tetris -lm
${ZIG:-zig} cc -target wasm32-wasi -O2 -s -mexec-model=reactor -w -DPROGRAM='"tetris.c"' -Ibuild host/host.c -o build/tetris.wasm

# One self-contained page, with the runtime and the WebAssembly inline, so it opens from disk.
base64 -w 100 build/tetris.wasm > build/tetris.wasm.b64
{
  printf '<!doctype html>\n<meta charset="utf-8">\n<meta name="viewport" content="width=device-width, initial-scale=1">\n'
  awk '/^\/\/ @hvm\.js$/ { system("cat host/hvm.js"); next }
       /^@wasm\.b64$/ { system("cat build/tetris.wasm.b64"); next }
       { print }' host/page.html
} > tetris.html
