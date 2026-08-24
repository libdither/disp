#!/usr/bin/env bash
# probe.sh — typecheck a disp snippet the way the kernel actually would.
#
# Root-file annotations are never verified (only `open use`d modules are), so
# this wraps the snippet in a module, `open use`s it from a root file, runs it
# memory-capped under a named systemd scope, and classifies the outcome:
#   ACCEPTED | REJECTED | ERROR | TIMEOUT | OOM-KILLED | KILLED
# The last three mean "died without a verdict" — never acceptance.
#
# usage: scripts/probe.sh [-r] [-t SECONDS] [-m MEMORYMAX] '<disp snippet>'
#        scripts/probe.sh [-r] ... < snippet.disp
#   -r  open only the raw substrate prelude (fast; no kernel, no checking —
#       for reduction probes, not type probes)
#   -t  timeout in seconds (default 120)
#   -m  systemd MemoryMax (default 6G; swap capped at 1G)
set -u
REPO="$(cd "$(dirname "$0")/.." && pwd)"
RAW=0 TIMEOUT=120 MEM=6G
while getopts "rt:m:" o; do
  case "$o" in
    r) RAW=1 ;;
    t) TIMEOUT="$OPTARG" ;;
    m) MEM="$OPTARG" ;;
    *) exit 2 ;;
  esac
done
shift $((OPTIND - 1))

SNIPPET="${1:-$(cat)}"
DIR="$(mktemp -d /tmp/disp-probe.XXXXXX)"
trap 'rm -rf "$DIR"' EXIT
if [ "$RAW" = 1 ]; then
  printf 'open use raw "%s/lib/prelude.disp" {}\n%s\n' "$REPO" "$SNIPPET" > "$DIR/module.disp"
else
  printf 'open use "%s/lib/kernel/prelude.disp"\n%s\n' "$REPO" "$SNIPPET" > "$DIR/module.disp"
fi
printf 'open use "%s/module.disp"\n' "$DIR" > "$DIR/root.disp"

UNIT="disp-probe-$$-$RANDOM"
START=$(date +%s)
OUT="$(cd "$REPO" && python scripts/rss_run.py timeout "$TIMEOUT" systemd-run --user --scope -q --unit="$UNIT" \
  -p MemoryMax="$MEM" -p MemorySwapMax=1G \
  npx tsx src/run.ts "$DIR/root.disp" 2>&1)"
CODE=$?
SECS=$(( $(date +%s) - START ))
# `[memo] warm:` = the run adopted .disp-test-cache's reduction snapshot (src/run.ts);
# without it (or after a kernel edit, which changes every tree embedding the edited
# definition) the run is cold: ~5x slower and ~2.5x the memory.
CACHE=$(grep -q '^\[memo\] warm' <<<"$OUT" && echo warm || echo cold)
RSS=$(grep '^\[rss\]' <<<"$OUT" | sed 's/^\[rss\] //; s/ exit=.*//')

grep -v '^\[memo\]\|^\[rss\]' <<<"$OUT" | tail -6
if [ $CODE -eq 0 ]; then
  VERDICT="ACCEPTED"
elif grep -q 'failing entry' <<<"$OUT"; then
  VERDICT="REJECTED"
elif grep -q '^error:' <<<"$OUT"; then
  VERDICT="ERROR"
elif [ $CODE -eq 124 ]; then
  VERDICT="TIMEOUT (no verdict)"
elif journalctl --user -u "$UNIT.scope" --no-pager 2>/dev/null | grep -qi 'oom'; then
  VERDICT="OOM-KILLED (no verdict)"
else
  VERDICT="KILLED (no verdict, exit $CODE)"
fi
echo "== $VERDICT  (${SECS}s, exit $CODE, mem cap $MEM, cache $CACHE, $RSS)"
[ $CODE -eq 0 ] || exit $CODE
