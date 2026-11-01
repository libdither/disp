#!/usr/bin/env bash
# Build strands-gpu (feature gpu) and run it with the Vulkan loader on the library path.
#   crate/gpu.sh --vectors FILE...  |  crate/gpu.sh TERM --grid N [--depth D] [--seed S] [--check] [--batch B] [--clocks C]
set -euo pipefail
cd "$(dirname "$(readlink -f "$0")")"
cargo build --release -q --features gpu --bin strands-gpu
loader=$(nix build --no-link --print-out-paths --impure --expr \
  '(builtins.getFlake "github:NixOS/nixpkgs/34ab99075ac4f7e40cf037eef32cb1c360bb85e9").legacyPackages.x86_64-linux.vulkan-loader')
LD_LIBRARY_PATH="$loader/lib${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" exec target/release/strands-gpu "$@"
