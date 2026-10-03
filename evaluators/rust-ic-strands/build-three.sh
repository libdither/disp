#!/usr/bin/env bash
# Bundle three.js (and its orbit controls) into player/three.min.js, a plain script that sets
# window.THREE, so the player's 3D view works when index.html is opened straight from disk.
set -euo pipefail
version=0.186.1
here="$(cd "$(dirname "$0")" && pwd)"
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT
cd "$tmp"
npm init -y >/dev/null
npm install --silent "three@$version" >/dev/null
cat > entry.js <<'JS'
import * as THREE from "three"
import { OrbitControls } from "three/addons/controls/OrbitControls.js"
window.THREE = { ...THREE, OrbitControls }
JS
"$here/../../node_modules/.bin/esbuild" entry.js --bundle --minify --format=iife --legal-comments=inline \
  --banner:js="/* three.js $version (MIT, https://threejs.org) with OrbitControls, bundled by build-three.sh */" \
  --outfile="$here/player/three.min.js" --log-level=warning
echo "player/three.min.js: $(stat -c %s "$here/player/three.min.js") bytes (three $version)"
