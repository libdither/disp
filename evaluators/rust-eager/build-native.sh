#!/usr/bin/env bash
# Build the rust-eager evaluator as an in-process NATIVE Node N-API addon
# (artifacts/rust_eager.node) for disp's Session backend.
#
#   Output: evaluators/rust-eager/artifacts/rust_eager.node   (gitignored; this produces it)
#
# Why native: the wasm Session is capped at the wasm32 4 GiB linear-memory ceiling (the wall
# the auto-verify check hits today); this runs in host RAM, with -O3 and no wasm sandbox tax
# on the reduce hot loop. See src/native.rs and src/eval/rust-eager-native.ts.
#
# The everyday loop never runs this: src/eval/rust-eager-native.ts skips the backend if the
# artifact is absent, so `npm test` stays green with no native addon.
#
# Requires: cargo + a C toolchain (cc + ld). Unlike the wasm build, no LLD discovery is
# needed — the host's default linker links the cdylib; the napi_* symbols stay undefined and
# resolve against the Node binary when require() loads the addon.
set -euo pipefail

HERE="$(cd "$(dirname "$0")" && pwd)"
command -v cargo >/dev/null || { echo "cargo not found — install Rust (https://rustup.rs)" >&2; exit 1; }

# --profile napi: release opt-level/LTO but panic="unwind" (napi-rs maps a Rust panic to a
# thrown JS Error via catch_unwind). --features napi pulls napi/napi-derive + compiles
# src/native.rs. --lib: skip the benchmark CLI bin (it doesn't need the addon).
( cd "$HERE/crate" && cargo build --profile napi --features napi --lib )

mkdir -p "$HERE/artifacts"
# The cdylib IS a valid Node addon: napi-derive emits napi_register_module_v1, so the .so
# loads directly via require() — no @napi-rs/cli needed (we build for this one host triple,
# not a prebuilt cross-platform matrix).
cp "$HERE/crate/target/napi/librust_eager.so" "$HERE/artifacts/rust_eager.node"
echo "rust-eager: built $HERE/artifacts/rust_eager.node"
