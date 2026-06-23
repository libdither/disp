#!/usr/bin/env bash
# Build the Rooted TC-Net evaluator (crate/) to a single wasm32 artifact for
# disp's Session backend. See TC_NET_PLAN.md.
#
#   Output: evaluators/tc-net/artifacts/tcnet.wasm   (gitignored; this produces it)
#
# The everyday loop never runs this: src/eval/tcnet.ts skips the backend if the
# artifact is absent, so `npm test` stays green with no Rust toolchain. Run this
# once TC-Net is implemented (M0) to enable it as an --evaluator backend.
#
# Requires: rustup + a Rust toolchain. The wasm32 target is added on demand.
set -euo pipefail

HERE="$(cd "$(dirname "$0")" && pwd)"
TARGET=wasm32-unknown-unknown

command -v cargo >/dev/null || { echo "cargo not found — install Rust (https://rustup.rs)" >&2; exit 1; }
if ! rustup target list --installed 2>/dev/null | grep -q "$TARGET"; then
  echo "tc-net: adding rust target $TARGET"
  rustup target add "$TARGET"
fi

( cd "$HERE/crate" && cargo build --release --target "$TARGET" )

mkdir -p "$HERE/artifacts"
cp "$HERE/crate/target/$TARGET/release/tc_net.wasm" "$HERE/artifacts/tcnet.wasm"
echo "tc-net: built $HERE/artifacts/tcnet.wasm"
