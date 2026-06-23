#!/usr/bin/env bash
# Build the lambada (lambada-llc/tree-calculus) lazy-stacks reducer into a single
# bundled artifact for disp's batch-tier peer. See EVALUATOR_LAMBADA_PLAN.md.
#
#   Output: evaluators/lambada/artifacts/main.js   (gitignored; this produces it)
#   Pin:    a fixed upstream commit, verified after checkout (loud fail on drift).
#
# The everyday loop never runs this: the disp adapter (src/eval/lambada.ts) skips
# the peer if the artifact is absent, so `npm test` stays green with no foreign
# build. Run this once to enable the lambada differential-conformance tests.
set -euo pipefail

HERE="$(cd "$(dirname "$0")" && pwd)"
PIN=b30ce83ed1eac4034b0e35e75077476ddc0e1ca2   # lambada main @ 2026-06-22
REPO=https://github.com/lambada-llc/tree-calculus.git
V="$HERE/vendor"

# 1. Pinned shallow checkout (idempotent; re-run is cheap).
if [ ! -e "$V/.git" ]; then
  rm -rf "$V"; mkdir -p "$V"
  git -C "$V" init -q
  git -C "$V" remote add origin "$REPO"
fi
git -C "$V" fetch -q --depth 1 origin "$PIN"
git -C "$V" checkout -q "$PIN"
GOT="$(git -C "$V" rev-parse HEAD)"
[ "$GOT" = "$PIN" ] || { echo "lambada pin mismatch: got $GOT, want $PIN" >&2; exit 1; }

# 2. Build the TypeScript reference (tsc compiles every evaluator .mts ⇒ .mjs).
TS="$V/implementation/typescript"
( cd "$TS" && npm install --silent && npm run build --silent )

# 3. Bundle OUR selectable-evaluator CLI (cli.mts imports all 11 built evaluators)
#    into one CommonJS artifact. .cjs, not .js: esbuild emits CommonJS (module.exports
#    + a `require.main === module` CLI guard), and disp's package.json is
#    "type": "module", under which a bare .js would be (wrongly) treated as ESM and
#    crash. esbuild runs from $TS (where it's installed) but resolves cli.mts's
#    ./vendor imports relative to cli.mts itself.
mkdir -p "$HERE/artifacts"
( cd "$TS" && npx esbuild "$HERE/cli.mts" --bundle --platform=node --outfile="$HERE/artifacts/cli.cjs" --log-level=warning )
echo "lambada: built $HERE/artifacts/cli.cjs (pin $GOT; 11 evaluators)"
