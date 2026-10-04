#!/usr/bin/env bash
# Run a command with the open chip tools on PATH, fetching the pinned flow scripts and process
# design kit into ~/.cache/rust-ic-strands-eda on first use.
#   OpenROAD 2025-03-01 and Yosys 0.55 from nixos-25.11, with OpenROAD-flow-scripts pinned to the
#   commit matching that OpenROAD; KLayout, Verilator and gcc from nixpkgs unstable (IHP's design
#   rule and layout-vs-schematic decks need KLayout 0.30.5 or later).
set -euo pipefail
CACHE=${EDA_CACHE:-$HOME/.cache/rust-ic-strands-eda}
ORFS_REV=3f209494f177e0d43b0933653e9ffbc36768d13c   # 2025-02-27
IHP_REV=5e6d592e4002946a4616f798c357f0f3c06cf3b6    # IHP-Open-PDK, for its KLayout decks
mkdir -p "$CACHE"
if [ ! -d "$CACHE/orfs/flow/platforms/ihp-sg13g2" ]; then
  git clone -q --filter=blob:none --no-checkout https://github.com/The-OpenROAD-Project/OpenROAD-flow-scripts "$CACHE/orfs"
  git -C "$CACHE/orfs" sparse-checkout init --no-cone
  printf '%s\n' /flow/Makefile /flow/scripts/ /flow/util/ /flow/platforms/ihp-sg13g2/ /flow/platforms/common/ '/flow/*.mk' \
    > "$CACHE/orfs/.git/info/sparse-checkout"
  git -C "$CACHE/orfs" checkout -q "$ORFS_REV"
fi
if [ ! -d "$CACHE/ihp/ihp-sg13g2/libs.tech/klayout" ]; then
  git clone -q --filter=blob:none --no-checkout https://github.com/IHP-GmbH/IHP-Open-PDK "$CACHE/ihp"
  git -C "$CACHE/ihp" sparse-checkout init --no-cone
  printf '%s\n' /ihp-sg13g2/libs.tech/klayout/ > "$CACHE/ihp/.git/info/sparse-checkout"
  git -C "$CACHE/ihp" checkout -q "$IHP_REV"
fi
export EDA_CACHE="$CACHE"
exec env -u LD_LIBRARY_PATH nix shell --impure --expr '
  let s = (builtins.getFlake "github:NixOS/nixpkgs/nixos-25.11").legacyPackages.${builtins.currentSystem};
      u = (builtins.getFlake "nixpkgs").legacyPackages.${builtins.currentSystem};
  in s.buildEnv { name = "eda"; paths = with s; [ yosys openroad u.klayout u.verilator gnumake time bc which gawk u.gcc
    (python3.withPackages (p: [ p.pyyaml p.pandas ])) ]; }' -c "$@"
