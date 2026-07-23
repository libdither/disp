#!/bin/sh
# Regenerate lattice_cascade.js (the bundle lattice_player.html replays) and validate it.
# Run after ANY engine change: evaluators/rust-ca-lattice/crate/src/{blocklet,cascade,
# cascade_run,cascade_par,rules}.rs
set -eu
cd "$(dirname "$0")"
HERE="$(pwd)"
CRATE="$HERE/../../evaluators/rust-ca-lattice/crate"

(cd "$CRATE" && cargo run --release --bin dump-cascade -- "$HERE/lattice_cascade.js")
node --check "$HERE/lattice_cascade.js"
node "$HERE/validate_cascade_bundle.mjs"
