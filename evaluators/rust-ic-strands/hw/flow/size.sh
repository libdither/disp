#!/usr/bin/env bash
# Cells per module of the block unit (Yosys generic synthesis, each module on its own and in
# parallel, the stages left out of the top's count).
#   hw/flow/size.sh [module ...]          (memory-capped; work files in OUT/size)
set -euo pipefail
HW=$(cd "$(dirname "$0")/.." && pwd)
WORK=${OUT:-${XDG_CACHE_HOME:-$HOME/.cache}/rust-ic-strands-hw}/size
mkdir -p "$WORK"
STAGES="strands_collect strands_pair strands_fire_setup strands_fire_prep strands_fire_search strands_fire_init strands_fire_link strands_move"
MODS=${*:-strands_block $STAGES}
export HW WORK STAGES MODS
systemd-run --user --scope -q -p MemoryMax=${SIZE_MEM:-12G} -p MemorySwapMax=1G timeout ${SIZE_TIMEOUT:-3600} "$HW/eda.sh" bash -c '
for m in $MODS; do
  bb=""; [ $m = strands_block ] && bb="blackbox $STAGES"
  printf "read_verilog -I%s %s %s\n%s\nhierarchy -top %s\nsynth -top %s\nstat\n" "$HW/rtl" "$HW/rtl/strands_block.v" "$HW/rtl/strands_stages.v" \
    "$bb" $m $m > "$WORK/$m.ys"
  ( start=$(date +%s); yosys -q -l "$WORK/$m.log" "$WORK/$m.ys" > /dev/null 2>&1
    echo "$(grep -m1 -E "^ +Number of cells:? " "$WORK/$m.log" | grep -oE "[0-9]+$") $(( $(date +%s) - start ))s" > "$WORK/$m.n" ) &
done
wait
total=0
for m in $MODS; do read -r n t < "$WORK/$m.n"; printf "  %-22s %8s cells  %s\n" $m "$n" "$t"; total=$((total + ${n:-0})); done
printf "  %-22s %8s cells\n" total $total
'
