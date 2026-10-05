#!/usr/bin/env bash
# Run dawn.mjs with Dawn's `webgpu` package (installed once into the cache) and the Vulkan loader.
#   player/dawn.sh check 'src=fib:1'  |  player/dawn.sh bench 'src=fib:1&copies=16&clocks=512'
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
cache=${XDG_CACHE_HOME:-$HOME/.cache}/rust-ic-strands-dawn
[ -f "$cache/node_modules/webgpu/index.js" ] || (mkdir -p "$cache" && cd "$cache" && npm i -s --no-save --no-audit --no-fund webgpu@0.6.2 >&2)
loader=$(nix build --no-link --print-out-paths --impure --expr \
  '(builtins.getFlake "github:NixOS/nixpkgs/34ab99075ac4f7e40cf037eef32cb1c360bb85e9").legacyPackages.x86_64-linux.vulkan-loader')
# Dawn warns about limits it lowers; that is noise here.
WEBGPU="$cache/node_modules/webgpu/index.js" LD_LIBRARY_PATH="$loader/lib${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" \
  node "$here/dawn.mjs" "$@" 2> >(command grep -v "artificially reduced" >&2)
