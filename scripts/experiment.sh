#!/usr/bin/env bash
# experiment.sh — try a kernel edit against the kernel suites without landing it.
#
# Patches a definition and/or literal substrings IN PLACE, runs each suite file
# memory-capped and timed, classifies the outcomes, and restores the file on any
# exit (trap on EXIT/INT/TERM). Editing a kernel definition changes the structural
# identity of every tree that embeds it, so the persisted reduction cache stops
# hitting for most of the kernel: runs are cold by default (loading the snapshot
# anyway costs its ~0.9 GB frozen arena for little gain). A cold
# lib/kernel/kernel.test.disp needs ~4 GB / 30 s; the cap is sized from `free`
# (available − headroom) and a cap below that is reported up front, never hidden.
#
# usage: scripts/experiment.sh [opts] FILE [NAME] < replacement.disp
#        scripts/experiment.sh --restore FILE
#   NAME        column-0 definition of FILE to replace with stdin: from the line
#               `NAME :`/`NAME :=`/`let NAME` through the line before the next
#               column-0 identifier or `//` comment
#   -s OLD -r NEW  literal substring replacement (OLD must occur exactly once);
#               repeatable, for edits inside a body (e.g. one walker branch)
#   -f SUITE    suite to run (repeatable; default lib/kernel/prelude.disp, the
#               checked barrel, then lib/kernel/kernel.test.disp)
#   -w          load the reduction snapshot anyway (default: DISP_MEMO_CACHE=0)
#   -t SECONDS  per-suite timeout (default 300)
#   -m MEMORYMAX  cap override (default: min(`free` available − 1500M, 5G), floor 1G;
#               the ceiling keeps a runaway edit from taking the whole machine)
#   --restore FILE  put back the backup a killed run left behind (SIGKILL skips
#               the trap; the script refuses to start while a backup exists)
set -u
REPO="$(cd "$(dirname "$0")/.." && pwd)"
BAKDIR=/tmp/disp-experiment-backups
COLD=1 TIMEOUT=300 MEM="" SUITES=() OLDS=() NEWS=() RESTORE=""
while [ $# -gt 0 ]; do
  case "$1" in
    -s) OLDS+=("$2"); shift 2 ;;
    -r) NEWS+=("$2"); shift 2 ;;
    -f) SUITES+=("$2"); shift 2 ;;
    -w) COLD=0; shift ;;
    -t) TIMEOUT="$2"; shift 2 ;;
    -m) MEM="$2"; shift 2 ;;
    --restore) RESTORE="$2"; shift 2 ;;
    -h|--help) sed -n 2,27p "$0"; exit 0 ;;
    --) shift; break ;;
    -*) echo "unknown option $1" >&2; exit 2 ;;
    *) break ;;
  esac
done
bak_for() { echo "$BAKDIR/$(realpath -m "$1" | tr / _).orig"; }

if [ -n "$RESTORE" ]; then
  BAK="$(bak_for "$RESTORE")"
  [ -f "$BAK" ] || { echo "no backup for $RESTORE"; exit 1; }
  cp "$BAK" "$RESTORE" && rm "$BAK" && echo "restored $RESTORE"; exit 0
fi

FILE="${1:-}"; NAME="${2:-}"
[ -n "$FILE" ] && [ -f "$FILE" ] || { echo "usage: scripts/experiment.sh [opts] FILE [NAME] < replacement.disp" >&2; exit 2; }
[ ${#OLDS[@]} -eq ${#NEWS[@]} ] || { echo "-s and -r must be paired" >&2; exit 2; }
[ -n "$NAME" ] || [ ${#OLDS[@]} -gt 0 ] || { echo "nothing to patch: give NAME (stdin) and/or -s/-r pairs" >&2; exit 2; }
[ ${#SUITES[@]} -gt 0 ] || SUITES=(lib/kernel/prelude.disp lib/kernel/kernel.test.disp)
BAK="$(bak_for "$FILE")"
if [ -f "$BAK" ]; then
  echo "a previous experiment left a backup for $FILE — the file may still be patched." >&2
  echo "run: scripts/experiment.sh --restore $FILE" >&2; exit 1
fi
REPL=""
if [ -n "$NAME" ]; then
  REPL="$(cat)"
  [ -n "$REPL" ] || { echo "empty replacement on stdin for $NAME" >&2; exit 2; }
fi

# ── memory cap: what the machine can spare, minus headroom ──
AVAIL_MB=$(free -m | awk '/^Mem:/{print $7}')
if [ -z "$MEM" ]; then
  CAP_MB=$(( AVAIL_MB - 1500 )); [ "$CAP_MB" -gt 5120 ] && CAP_MB=5120; [ "$CAP_MB" -lt 1024 ] && CAP_MB=1024
  MEM="${CAP_MB}M"
fi
echo "== cap $MEM (available ${AVAIL_MB}M); cold kernel.test.disp needs ~4200M"
case "$MEM" in *G) MEM_MB=$(( ${MEM%G} * 1024 )) ;; *M) MEM_MB=${MEM%M} ;; *) MEM_MB=0 ;; esac
[ "$MEM_MB" -ge 4200 ] || echo "== WARNING: cap below a cold kernel.test.disp run; an OOM kill here means nothing about the edit"

# ── patch in place, backup first, restore on any exit ──
mkdir -p "$BAKDIR"; cp "$FILE" "$BAK"
restore() { if [ -f "$BAK" ]; then cp "$BAK" "$FILE" && rm "$BAK" && echo "== restored $FILE"; fi; }
trap restore EXIT; trap 'exit 130' INT TERM
python - "$FILE" "$NAME" "$REPL" "${#OLDS[@]}" "${OLDS[@]}" "${NEWS[@]}" <<'PY' || exit 2
import re, sys
path, name, repl, n = sys.argv[1], sys.argv[2], sys.argv[3], int(sys.argv[4])
olds, news = sys.argv[5:5 + n], sys.argv[5 + n:5 + 2 * n]
src = open(path).read()
if name:
    lines = src.split("\n")
    head = re.compile(r"^(let\s+)?" + re.escape(name) + r"\s*:")
    starts = [i for i, l in enumerate(lines) if head.match(l)]
    if len(starts) != 1:
        sys.exit(f"definition {name!r}: found {len(starts)} column-0 matches in {path}")
    s = starts[0]
    e = next((i for i in range(s + 1, len(lines)) if re.match(r"^[A-Za-z_]|^//", lines[i])), len(lines))
    lines[s:e] = repl.rstrip("\n").split("\n")
    src = "\n".join(lines)
for old, new in zip(olds, news):
    c = src.count(old)
    if c != 1:
        sys.exit(f"substring occurs {c} times (need exactly 1): {old[:80]!r}")
    src = src.replace(old, new)
open(path, "w").write(src)
PY
echo "== patch applied to $FILE:"; diff -u "$BAK" "$FILE" | tail -n +3 | grep -E '^[-+]' | cut -c1-160

# ── run each suite, classify ──
STATUS=0
for SUITE in "${SUITES[@]}"; do
  UNIT="disp-exp-$$-$RANDOM"; LOG="$(mktemp "/tmp/disp-experiment.$(basename "$SUITE").XXXXXX.log")"
  ENV=(); [ "$COLD" = 1 ] && ENV=(-E DISP_MEMO_CACHE=0)
  (cd "$REPO" && python scripts/rss_run.py timeout "$TIMEOUT" systemd-run --user --scope -q --unit="$UNIT" \
    -p MemoryMax="$MEM" -p MemorySwapMax=512M "${ENV[@]}" npx tsx src/run.ts "$SUITE") >"$LOG" 2>&1
  CODE=$?
  if [ $CODE -eq 0 ]; then VERDICT="PASSED"
  elif grep -q 'failing entries' "$LOG"; then VERDICT="REJECTED (annotation check)"
  elif grep -q 'failed: [1-9]' "$LOG"; then VERDICT="FAILED (tests)"
  elif grep -q '^error:' "$LOG"; then VERDICT="ERROR"
  elif [ $CODE -eq 124 ]; then VERDICT="TIMEOUT (no verdict)"
  elif journalctl --user -u "$UNIT.scope" --no-pager 2>/dev/null | grep -qi 'oom'; then VERDICT="OOM-KILLED (no verdict)"
  else VERDICT="KILLED (no verdict, exit $CODE)"; fi
  CACHE=$(grep -q '^\[memo\] warm' "$LOG" && echo "snapshot loaded" || echo cold)
  echo "== $SUITE: $VERDICT  [$(grep '^\[rss\]' "$LOG" | sed 's/^\[rss\] //'), $CACHE]  log: $LOG"
  grep -v '^\[memo\]\|^\[rss\]' "$LOG" | tail -3 | cut -c1-300 | sed 's/^/   /'
  grep -E '^\s*\[[^]]*:[0-9]+\] mismatch' "$LOG" | cut -c1-120 | head -12 | sed 's/^/   /'
  [ $CODE -eq 0 ] || STATUS=1
done
exit $STATUS
