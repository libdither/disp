#!/usr/bin/env bash
# Build the Rooted TC-Net evaluator (crate/) to a single wasm32 artifact for
# disp's Session backend. See TC_NET_PLAN.md.
#
#   Output: evaluators/tc-net/artifacts/tcnet.wasm   (gitignored; this produces it)
#
# The everyday loop never runs this: src/eval/tcnet.ts skips the backend if the
# artifact is absent, so `npm test` stays green with no Rust toolchain.
#
# Requires: cargo + the wasm32 std + an LLD linker. Works with a rustup toolchain
# (adds the target + ships rust-lld) OR a standalone/Nix toolchain that already
# bundles the wasm32 std (then we discover a wasm-ld, since rust-lld may be absent).
set -euo pipefail

HERE="$(cd "$(dirname "$0")" && pwd)"
TARGET=wasm32-unknown-unknown

command -v cargo >/dev/null || { echo "cargo not found — install Rust (https://rustup.rs)" >&2; exit 1; }

# Ensure the wasm32 std is present. Prefer rustup; tolerate a toolchain that
# already ships the target (rustup absent — e.g. Nix).
SYSROOT="$(rustc --print sysroot)"
if [ ! -d "$SYSROOT/lib/rustlib/$TARGET" ]; then
  if command -v rustup >/dev/null; then
    echo "tc-net: adding rust target $TARGET"
    rustup target add "$TARGET"
  else
    echo "tc-net: $TARGET std not installed and rustup absent — add the wasm32 std to your toolchain" >&2
    exit 1
  fi
fi

# The wasm link step needs an LLD-based linker. rustup ships rust-lld; a Nix
# rustc may not, so discover a wasm-ld and point cargo at it.
if ! command -v rust-lld >/dev/null 2>&1 && ! command -v wasm-ld >/dev/null 2>&1; then
  if [ -d /nix/store ]; then
    WASMLD="$(find /nix/store -maxdepth 3 -name 'wasm-ld' -type f 2>/dev/null | head -1 || true)"
    if [ -n "${WASMLD:-}" ]; then
      echo "tc-net: using discovered wasm-ld: $WASMLD"
      export CARGO_TARGET_WASM32_UNKNOWN_UNKNOWN_LINKER="$WASMLD"
    fi
  fi
fi

( cd "$HERE/crate" && cargo build --release --target "$TARGET" )

mkdir -p "$HERE/artifacts"
cp "$HERE/crate/target/$TARGET/release/tc_net.wasm" "$HERE/artifacts/tcnet.wasm"
echo "tc-net: built $HERE/artifacts/tcnet.wasm"
