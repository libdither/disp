#!/usr/bin/env bash
# Builds Tetris: build/tetris opens a native window, tetris.html runs in a browser.
# Needs Bend 2 (curl -fsSL https://bend-lang.com/install.sh | sh), or BEND="bun path/to/bend2/main.ts".
set -euo pipefail
cd "$(dirname "$0")"
BEND=${BEND:-bend}
mkdir -p build
$BEND tetris.bend -o build/tetris
rm -rf build/web
$BEND web/index.html -o build/web

# One self-contained page: the bundled script goes inline, so it opens from disk.
python - <<'EOF'
import pathlib, re
web = pathlib.Path("build/web")
page = (web / "index.html").read_text()
tag = re.search(r'<script type="module" crossorigin src="\./([^"]+)"></script>', page)
script = (web / tag.group(1)).read_text()
assert "</script" not in script
page = page.replace(tag.group(0), '<script type="module">\n' + script + "\n</script>")
pathlib.Path("tetris.html").write_text(page)
EOF
