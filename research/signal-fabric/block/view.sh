#!/bin/sh
# Builds view.bend into out/view.mjs and the viewer into ../block.js, for block.html (about a second or two).
# check.sh runs this after the proofs; the page reloads itself when it finishes.
here=$(cd "$(dirname "$0")" && pwd)
repo=$here/../../..
bend=${BEND:-$HOME/Repos/bend2/bend2/main.ts}
out=$here/out
mkdir -p "$out"
cap() { systemd-run --user --scope -q -p MemoryMax=4G -p MemorySwapMax=1G timeout 300 "$@"; }
fail=0
if cap bun "$bend" "$here/view.bend" -o "$out/view.new.mjs" >"$out/view.txt" 2>&1; then
  mv "$out/view.new.mjs" "$out/view.mjs"
else
  fail=1
  cat "$out/view.txt"
fi
cap npx --prefix "$repo/website" tsx "$here/bundle.mts" || fail=1
date -Iseconds >"$out/stamp"
exit $fail
