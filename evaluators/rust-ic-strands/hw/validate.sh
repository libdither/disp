#!/usr/bin/env bash
# Check that the chip in hw/rtl does exactly what the simulator does, bit for bit:
#   1. tables     regenerate rtl/strands_tables.vh from the simulator's rules and chip config
#   2. vectors    run the simulator (--chip) on workloads that exercise every move and every rule,
#                 recording turns and blocks, and whole-lattice states clock by clock
#   3. block      replay every recorded turn and block through the block unit (Verilator)
#   4. lattice    run the whole lattice (shifting, all block units, pulses) clock by clock
#   5. layout     with --layout: synthesis, place and route on IHP SG13G2, design rule check,
#                 layout versus schematic, timing (see flow/layout.sh)
# Exits non-zero on the first stage that fails. Each stage prints its wall time.
#
#   hw/validate.sh [--layout]          (memory-capped; work files in OUT, default ~/.cache/rust-ic-strands-hw)
set -euo pipefail
HW=$(cd "$(dirname "$0")" && pwd)
CRATE=$HW/../crate
OUT=${OUT:-${XDG_CACHE_HOME:-$HOME/.cache}/rust-ic-strands-hw}
mkdir -p "$OUT/vectors" "$OUT/dumps"
cap() { local mem=$1 t=$2; shift 2; systemd-run --user --scope -q -p MemoryMax="$mem" -p MemorySwapMax=1G timeout "$t" "$@"; }
stage() { echo "== $1"; STAGE_T=$(date +%s); }
done_() { echo "   $(( $(date +%s) - STAGE_T ))s"; }

stage "tables"
(cd "$CRATE" && cargo build --release -q --bin strands-run --bin strands-hw)
"$CRATE/target/release/strands-hw" "$HW/rtl/strands_tables.vh" 2>/dev/null
done_

stage "vectors"
run=$CRATE/target/release/strands-run
# term grid depth sample: the rules program and small terms record everything; the benchmarks
# record every rare move and a sample of steps, flips and idle turns.
WORKLOADS=(
  "rules:0 100 4 1" "share-tower:3 24 4 1" "k-chain:4 32 4 1" "discard-tree:4 40 4 1" "convoy:4 40 4 1"
  "full-tree:3 40 4 1" "disp-t 24 4 1" "s-rule 16 4 1" "fork 16 4 1" "k 16 4 1"
  "@(F(@(L,L),L),L) 24 4 1" "@(F(F(L,L),L),@(L,L)) 24 4 1" "@(@(S(L),L),@(L,L)) 24 4 1"
  "fib:0 232 8 500" "sort:1 490 8 500"
)
pids=()
for w in "${WORKLOADS[@]}"; do
  read -r term grid depth sample <<< "$w"
  name=$(echo "$term" | tr -c 'a-zA-Z0-9' '_')
  ( VECTORS=$OUT/vectors/$name.bin VECTOR_SAMPLE=$sample cap 3G 1800 "$run" "$term" --grid "$grid" --seed 1 --chip --depth "$depth" \
      | grep '^DONE' > /dev/null || { echo "   $term did not finish"; exit 1; } ) & pids+=($!)
done
LATTICE=("k" "fork" "s-rule" "@(F(@(L,L),L),L)")
for term in "${LATTICE[@]}"; do
  name=$(echo "$term" | tr -c 'a-zA-Z0-9' '_')
  ( DUMPS=$OUT/dumps/$name.bin cap 1G 600 "$run" "$term" --grid 8 --seed 1 --chip --depth 2 | grep '^DONE' > /dev/null \
      || { echo "   $term did not finish"; exit 1; } ) & pids+=($!)
done
for p in "${pids[@]}"; do wait "$p"; done
done_

stage "block"
cap 8G 3600 "$HW/eda.sh" verilator --cc --exe --build -O2 -j 4 -Wno-fatal -Wno-lint -Wno-style --top-module strands_block \
  -I"$HW/rtl" "$HW/rtl/strands_block.v" "$HW/rtl/strands_stages.v" "$HW/sim/tb_block.cpp" -o tb_block --Mdir "$OUT/block" > "$OUT/block.log" 2>&1
fail=0
for v in "$OUT"/vectors/*.bin; do
  r=$(cap 2G 3600 env -u LD_LIBRARY_PATH "$OUT/block/tb_block" "$v" 2>&1 | tail -1) || fail=1
  printf "   %-28s %s\n" "$(basename "$v" .bin)" "$r"
done
[ $fail = 0 ] || { echo "   block unit differs from the simulator"; exit 1; }
done_

stage "lattice"
cap 12G 3600 "$HW/eda.sh" verilator --cc --exe --build --hierarchical -O1 -j 3 -Wno-fatal -Wno-lint -Wno-style -GW=8 -GH=8 -GD=2 \
  --top-module strands_lattice -I"$HW/rtl" "$HW/rtl/strands_lattice.v" "$HW/rtl/strands_block.v" "$HW/rtl/strands_stages.v" "$HW/sim/tb_lattice.cpp" \
  -o tb_lattice --Mdir "$OUT/lattice" > "$OUT/lattice.log" 2>&1
for d in "$OUT"/dumps/*.bin; do
  r=$(cap 4G 3600 env -u LD_LIBRARY_PATH "$OUT/lattice/tb_lattice" "$d" 2>&1 | tail -1) || fail=1
  printf "   %-28s %s\n" "$(basename "$d" .bin)" "$r"
done
[ $fail = 0 ] || { echo "   lattice differs from the simulator"; exit 1; }
done_

if [ "${1:-}" = "--layout" ]; then
  stage "layout"
  "$HW/flow/layout.sh"
  done_
fi
echo "== the chip matches the simulator"
