#!/usr/bin/env bash
# survey-probe.sh — first-pass evidence gathering for an awesome-langs candidate.
#
# One command does the ritual that every "consider adding X" request starts with:
#   1. GitHub API metadata: stars, dates, fork parent, topics, license  (the
#      master-list convention is that activity dates are API-verified, so the
#      verification date is printed for the write-up header)
#   2. contributors with commit counts (the "authorship verified" header line)
#   3. recent commit subjects (agent-run repos are visible here immediately)
#   4. a shallow clone kept under /tmp for follow-up reading, with a top-level
#      listing, file counts by extension, and top-level docs
#   5. a proof-debt scan: sorry/admit/Admitted/assume/axiom counts in proof
#      files, so "the Lean/Coq spine is real" can be claimed or denied cheaply
#
# The output is evidence, not a verdict: scoring against _AXES.md stays manual.
#
# usage: scripts/survey-probe.sh <owner/name | github url> [branch]
#   GITHUB_TOKEN, if set, is used for the API calls (rate limit is 60/h without).
set -u

[ $# -ge 1 ] || { echo "usage: $0 <owner/name | github url> [branch]" >&2; exit 2; }
REPO=$(echo "$1" | sed -E 's#^https?://github.com/##; s#\.git$##; s#/$##')
BRANCH="${2:-}"
AUTH=()
[ -n "${GITHUB_TOKEN:-}" ] && AUTH=(-H "Authorization: Bearer $GITHUB_TOKEN")
TMP=$(mktemp -d /tmp/survey-probe.XXXXXX)
trap 'rm -rf "$TMP"' EXIT

api() { curl -fsS "${AUTH[@]}" "https://api.github.com/$1"; }

api "repos/$REPO" > "$TMP/repo.json" || { echo "API fetch failed for $REPO" >&2; exit 1; }
echo "== metadata (verified $(date +%F) via GitHub API)"
python - "$TMP/repo.json" <<'PY'
import json, sys
d = json.load(open(sys.argv[1]))
def row(k, v): print(f"  {k:<10} {v}")
row("repo", f"{d['full_name']} ({d['stargazers_count']}★, created {d['created_at'][:10]}, pushed {d['pushed_at'][:10]})")
row("about", d.get("description") or "-")
row("language", f"{d.get('language')} · {d['size']} KB · default branch {d['default_branch']}")
row("license", (d.get("license") or {}).get("spdx_id") or "-")
row("topics", ", ".join(d.get("topics") or []) or "-")
row("issues", f"{d['open_issues_count']} open · {d['forks_count']} forks")
p = d.get("parent")
if p:
    row("fork of", f"{p['full_name']} ({p['stargazers_count']}★, pushed {p['pushed_at'][:10]}) — {p.get('description') or '-'}")
PY
[ -n "$BRANCH" ] || BRANCH=$(python -c "import json,sys; print(json.load(open(sys.argv[1]))['default_branch'])" "$TMP/repo.json")

echo "== contributors"
api "repos/$REPO/contributors?per_page=10" > "$TMP/contrib.json" && python - "$TMP/contrib.json" <<'PY'
import json, sys
for c in json.load(open(sys.argv[1])):
    print(f"  {c['login']:<24} {c['contributions']}")
PY

echo "== recent commits ($BRANCH)"
api "repos/$REPO/commits?sha=$BRANCH&per_page=10" > "$TMP/commits.json" && python - "$TMP/commits.json" <<'PY'
import json, sys
for c in json.load(open(sys.argv[1])):
    m = c["commit"]
    print(f"  {m['author']['date'][:10]}  {m['message'].splitlines()[0][:90]}")
PY

DIR="/tmp/survey-$(basename "$REPO")"
echo "== clone -> $DIR"
rm -rf "$DIR"
git clone --quiet --depth 50 -b "$BRANCH" "https://github.com/$REPO" "$DIR" || exit 1
ls "$DIR" | sed 's/^/  /'

echo "== files by extension (top 12)"
find "$DIR" -type f -not -path "*/.git/*" | sed -E 's/.*\.([A-Za-z0-9]+)$/\1/; t; s/.*/none/' \
  | sort | uniq -c | sort -rn | head -12 | sed 's/^/  /'

echo "== top-level docs"
find "$DIR" -maxdepth 1 -iname "*.md" -exec wc -l {} + | sed "s#$DIR/##; s/^/  /"

echo "== proof-debt scan (.lean/.v/.agda/.thy/.dfy/.fst)"
for w in sorry admit Admitted assume axiom native_decide; do
  n=$(grep -rw "$w" "$DIR" --include='*.lean' --include='*.v' --include='*.agda' \
        --include='*.thy' --include='*.dfy' --include='*.fst' 2>/dev/null | wc -l)
  [ "$n" -gt 0 ] && echo "  $w: $n line(s) — check whether comments or real"
done
echo "done; clone kept at $DIR for follow-up reading"
