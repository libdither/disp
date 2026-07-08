#!/usr/bin/env bash
# E1 summary: one row per workload from bench/e1/*.json (rent fits) + *.log (reducer).
set -uo pipefail
cd "$(dirname "$0")/.."
printf "%-14s %12s %10s %8s %10s %7s %6s\n" workload interactions span avg-par traffic/d p r2
for j in bench/e1/*.json; do
  case "$j" in *.bin.meta.json) continue ;; esac
  name=$(basename "$j" .json)
  [ -f "$j" ] || continue
  work=$(grep -o '"work":[0-9]*' "$j" | cut -d: -f2)
  span=$(grep -o '"span":[0-9]*' "$j" | cut -d: -f2)
  cut=$(grep -o '"top_cut":[0-9]*' "$j" | cut -d: -f2)
  p=$(grep -o '"p":[0-9.-]*' "$j" | cut -d: -f2)
  r2=$(grep -o '"r2":[0-9.]*' "$j" | cut -d: -f2)
  if [ -n "$work" ] && [ -n "$span" ] && [ "$span" != 0 ]; then
    par=$(awk "BEGIN{printf \"%.0f\", $work/$span}")
    tpd=$(awk "BEGIN{printf \"%.2f\", $cut/$span}")
  else
    par="-"; tpd="-"
  fi
  printf "%-14s %12s %10s %8s %10s %7s %6s\n" "$name" "${work:--}" "${span:--}" "$par" "$tpd" "${p:--}" "${r2:--}"
done
echo
echo "failed (limit/overflow):"
grep -l "FAILED\|exceeded\|overflow" bench/e1/*.log 2>/dev/null | while read -r l; do
  echo "  $(basename "$l" .log): $(tail -1 "$l" | head -c 100)"
done
