#!/usr/bin/env bash
# Reproduce research/TREE_READBACK.md's measurements:
#  1. compile programs.disp with origin tracking (a scratch copy of src/ with cir-origins.patch);
#  2. run `fib 3` in the strand-lattice player (headless Chromium) to clock 4000 and dump every agent's
#     term as the details drawer shows it;
#  3. read those terms back (reader.py) and compare.
# Needs chromium on the path. Everything is memory-capped and timed out.
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
repo="$(cd "$here/../.." && pwd)"
work=$(mktemp -d /tmp/tree-readback.XXXXXX)
cp -r "$repo/src" "$work/src"
(cd "$work" && patch -s -p1 < "$here/cir-origins.patch")
ln -s "$repo/node_modules" "$work/node_modules"
ln -s "$repo/evaluators" "$work/evaluators"
cp "$here/probe.ts" "$work/"
cap() { systemd-run --user --scope -q -p MemoryMax=6G -p MemorySwapMax=512M timeout "$@"; }
(cd "$work" && cap 900 npx tsx probe.ts "$repo/evaluators/rust-ic-strands/programs/programs.disp" "$work/origins.json")
cap 400 node "$here/drawer.mjs" "file://$repo/evaluators/rust-ic-strands/player/index.html#src=fib%203&view=oblique" 4000 "$work/terms.json"
(cd "$here" && cap 900 python reader.py "$work/origins.json" "$work/terms.json" fib)
echo "scratch: $work"
