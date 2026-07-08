#!/usr/bin/env bash
# E1 driver (SPATIAL_IC.md §10): reduce every bench/e1/*.terms under the rung C tracer,
# then fit the Rent exponent of each interaction DAG. Produces per workload:
#   <name>.nf      normal form (spot-check against the eager oracle)
#   <name>.log     reducer stderr (interactions, events, peak nodes)
#   <name>.bin     the rung C event log (+ .meta.json: rung A histograms)
#   <name>.rent    the analyzer report; <name>.json the machine-readable fit
set -uo pipefail
cd "$(dirname "$0")/.."
CLI=evaluators/rust-ic-net/crate/target/release/ic-net-cli
RENT=evaluators/rust-ic-net/crate/target/release/rent
NODES=$((1 << 26))

for f in bench/e1/*.terms; do
  name=$(basename "$f" .terms)
  echo "== $name"
  # resumable: a completed trace (.bin + the meta the CLI writes on success) is not redone
  if [ ! -f "bench/e1/$name.bin.meta.json" ]; then
    if ! "$CLI" -trace "bench/e1/$name.bin" -file "$f" -nodes $NODES -vars $NODES \
        >"bench/e1/$name.nf" 2>"bench/e1/$name.log"; then
      echo "   reduce FAILED: $(tail -1 "bench/e1/$name.log")"
      rm -f "bench/e1/$name.bin"
      continue
    fi
  fi
  sed -n 's/.*\(evaluator=[^ ]*\) \(interactions=[0-9]*\) \(events=[0-9]*\).*/   \2 \3/p' "bench/e1/$name.log"
  if [ ! -f "bench/e1/$name.json" ]; then
    nice -n 10 "$RENT" "bench/e1/$name.bin" --json "bench/e1/$name.json" >"bench/e1/$name.rent" 2>&1
  fi
  grep -E "^graph|^dag|^Rent fit|traffic|^NOTE" "bench/e1/$name.rent" | sed 's/^/   /'
done
