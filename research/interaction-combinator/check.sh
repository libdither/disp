#!/bin/sh
# The one gate for cascade engine work: crate tests (atlas + suite + lattice soak +
# abstract differential), the model-cost census against its checked-in baseline, then
# bundle regen + validation. Run before AND after any engine change; a completion
# regression in the census or a red test is a stop.
set -eu
cd "$(dirname "$0")"
CRATE="$(pwd)/../../evaluators/rust-ca-lattice/crate"

(cd "$CRATE" && cargo test --release)
(cd "$CRATE" && cargo run --release --bin census-cascade)
./regen.sh
echo "check.sh: all green"
