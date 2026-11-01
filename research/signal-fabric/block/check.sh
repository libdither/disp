#!/bin/sh
# The block frame's loop: checks every law in LAWS.bend (through PROOF.bend), prints how long it took, and
# builds the viewer (view.sh). Its own cap is the one to rely on: an outer timeout leaves Bend's scope running.
# Needs a Bend checkout: BEND=path/to/bend2/main.ts (default ~/Repos/bend2/bend2/main.ts).
here=$(cd "$(dirname "$0")" && pwd)
bend=${BEND:-$HOME/Repos/bend2/bend2/main.ts}
out=$here/out
mkdir -p "$out"
cap() { systemd-run --user --scope -q -p MemoryMax=8G -p MemorySwapMax=1G timeout 1800 "$@"; }
start=$(date +%s)
cap bun "$bend" "$here/PROOF.bend" >"$out/proofs.txt" 2>&1
status=$?
cat "$out/proofs.txt"
echo "proofs: $(( $(date +%s) - start ))s" | tee -a "$out/proofs.txt"
"$here/view.sh" || status=1
exit $status
