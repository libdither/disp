#!/usr/bin/env bash
# Build the rust-ic-net evaluator (crate/) to a single wasm32 artifact for disp's
# Session backend. See research/interaction-combinator/RUST_IC_NET_DESIGN.md.
#
#   Output: evaluators/rust-ic-net/artifacts/rust_ic_net.wasm   (gitignored; this produces it)
#
# The everyday loop never runs this: src/eval/ic-net.ts skips the backend if the
# artifact is absent, so `npm test` stays green with no Rust toolchain.
#
# Requires: cargo + the wasm32 std + an LLD linker (rustup ships rust-lld; a Nix
# toolchain may not, so we discover a wasm-ld).
set -euo pipefail

HERE="$(cd "$(dirname "$0")" && pwd)"
TARGET=wasm32-unknown-unknown

command -v cargo >/dev/null || { echo "cargo not found — install Rust (https://rustup.rs)" >&2; exit 1; }

SYSROOT="$(rustc --print sysroot)"
if [ ! -d "$SYSROOT/lib/rustlib/$TARGET" ]; then
  if command -v rustup >/dev/null; then
    echo "rust-ic-net: adding rust target $TARGET"
    rustup target add "$TARGET"
  else
    echo "rust-ic-net: $TARGET std not installed and rustup absent — add the wasm32 std to your toolchain" >&2
    exit 1
  fi
fi

if ! command -v rust-lld >/dev/null 2>&1 && ! command -v wasm-ld >/dev/null 2>&1; then
  if [ -d /nix/store ]; then
    WASMLD="$(find /nix/store -maxdepth 3 -name 'wasm-ld' -type f 2>/dev/null | head -1 || true)"
    if [ -n "${WASMLD:-}" ]; then
      echo "rust-ic-net: using discovered wasm-ld: $WASMLD"
      export CARGO_TARGET_WASM32_UNKNOWN_UNKNOWN_LINKER="$WASMLD"
    fi
  fi
fi

# --lib only: the native CLI bin (src/bin/ic-net-cli.rs) uses threads/crossbeam, which
# don't build on wasm32 — the wasm artifact is the sequential oracle (the lib's cdylib).
( cd "$HERE/crate" && cargo build --release --target "$TARGET" --lib )

mkdir -p "$HERE/artifacts"
cp "$HERE/crate/target/$TARGET/release/rust_ic_net.wasm" "$HERE/artifacts/rust_ic_net.wasm"
echo "rust-ic-net: built $HERE/artifacts/rust_ic_net.wasm"
