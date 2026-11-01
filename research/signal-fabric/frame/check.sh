#!/bin/sh
# The frame's loop: regenerates cases.bend from core.ts, checks every law in LAWS.bend (through PROOF.bend),
# builds view.bend into out/view.mjs and the viewer into ../frame.js, and prints the design's score. frame.html
# reloads itself when this finishes. `check.sh view` only rebuilds the design for the page (about a second).
# Needs a Bend checkout: BEND=path/to/bend2/main.ts (default ~/Repos/bend2/bend2/main.ts).
here=$(cd "$(dirname "$0")" && pwd)
repo=$here/../../..
bend=${BEND:-$HOME/Repos/bend2/bend2/main.ts}
out=$here/out
mkdir -p "$out"
cap() { systemd-run --user --scope -q -p MemoryMax=8G -p MemorySwapMax=1G timeout 900 "$@"; }
fail=0
if [ "$1" != view ]; then
  cap npx --prefix "$repo/website" tsx "$here/cases.mts" || exit 1
  cap bun "$bend" "$here/PROOF.bend" >"$out/proofs.new" 2>&1 || fail=1
  mv "$out/proofs.new" "$out/proofs.txt"
  cat "$out/proofs.txt"
fi
if cap bun "$bend" "$here/view.bend" -o "$out/view.new.mjs" >"$out/view.txt" 2>&1; then
  mv "$out/view.new.mjs" "$out/view.mjs"
  : >"$out/view.txt"
else
  fail=1
  cat "$out/view.txt"
fi
cap npx --prefix "$repo/website" tsx "$here/bundle.mts" >/dev/null || fail=1
if [ "$1" != view ]; then
  cap bun "$bend" "$here/score.bend" -o "$out/score.cjs" && cap node "$out/score.cjs" | tee "$out/score.txt"
fi
date -Iseconds >"$out/stamp"
exit $fail
