#!/usr/bin/env bash
# Run a command with the open chip tools on PATH, fetching the pinned flow scripts and process
# design kit into ~/.cache/rust-ic-strands-eda on first use.
#   OpenROAD 2025-03-01 and Yosys 0.55 from nixos-25.11, with OpenROAD-flow-scripts pinned to the
#   commit matching that OpenROAD; KLayout, Verilator and gcc from nixos-unstable (IHP's design
#   rule and layout-vs-schematic decks need KLayout 0.30.5 or later). OpenROAD 26Q2 is in
#   nixos-unstable too, but not built by its binary cache (its or-tools is marked broken).
#   Exports ORFS_FLOW and IHP_TECH.
set -euo pipefail
CACHE=${EDA_CACHE:-$HOME/.cache/rust-ic-strands-eda}
STABLE=b6018f87da91d19d0ab4cf979885689b469cdd41    # nixos-25.11
UNSTABLE=34ab99075ac4f7e40cf037eef32cb1c360bb85e9  # nixos-unstable, 2026-08-31
ORFS_REV=3f209494f177e0d43b0933653e9ffbc36768d13c  # OpenROAD-flow-scripts, 2025-02-27
IHP_REV=5e6d592e4002946a4616f798c357f0f3c06cf3b6   # IHP-Open-PDK, for its KLayout decks
ORFS_DIR=$CACHE/orfs-$ORFS_REV
mkdir -p "$CACHE"
if [ ! -d "$ORFS_DIR/flow/platforms/ihp-sg13g2" ]; then
  git clone -q --filter=blob:none --no-checkout https://github.com/The-OpenROAD-Project/OpenROAD-flow-scripts "$ORFS_DIR"
  git -C "$ORFS_DIR" sparse-checkout init --no-cone
  printf '%s\n' /flow/Makefile /flow/scripts/ /flow/util/ /flow/platforms/ihp-sg13g2/ /flow/platforms/common/ '/flow/*.mk' \
    > "$ORFS_DIR/.git/info/sparse-checkout"
  git -C "$ORFS_DIR" checkout -q "$ORFS_REV"
fi
if [ ! -d "$CACHE/ihp/ihp-sg13g2/libs.tech/klayout" ]; then
  git clone -q --filter=blob:none --no-checkout https://github.com/IHP-GmbH/IHP-Open-PDK "$CACHE/ihp"
  git -C "$CACHE/ihp" sparse-checkout init --no-cone
  printf '%s\n' /ihp-sg13g2/libs.tech/klayout/ > "$CACHE/ihp/.git/info/sparse-checkout"
  git -C "$CACHE/ihp" checkout -q "$IHP_REV"
fi
export EDA_CACHE="$CACHE" ORFS_FLOW="$ORFS_DIR/flow" IHP_TECH="$CACHE/ihp/ihp-sg13g2/libs.tech/klayout/tech"
exec env -u LD_LIBRARY_PATH nix shell --impure --expr '
  let s = (builtins.getFlake "github:NixOS/nixpkgs/'"$STABLE"'").legacyPackages.${builtins.currentSystem};
      u = (builtins.getFlake "github:NixOS/nixpkgs/'"$UNSTABLE"'").legacyPackages.${builtins.currentSystem};
  in s.buildEnv { name = "eda"; paths = with s; [ yosys openroad u.klayout u.verilator gnumake time bc which gawk u.gcc
    (python3.withPackages (p: [ p.pyyaml p.pandas ])) ]; }' -c "$@"
