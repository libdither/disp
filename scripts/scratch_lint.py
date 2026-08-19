#!/usr/bin/env python
"""Lint root-level scratch .disp files: flag `open use` paths that do not resolve.

Usage: python scripts/scratch_lint.py [files...]   (default: *.disp in repo root)
Exits 1 if any open target is missing, so a stale probe fails at grep speed
instead of after a session spin-up.
"""
import re
import sys
from pathlib import Path

root = Path(__file__).resolve().parent.parent
files = [Path(a) for a in sys.argv[1:]] or sorted(root.glob("*.disp"))
bad = 0
for f in files:
    for i, line in enumerate(f.read_text().splitlines(), 1):
        m = re.search(r'open\s+use\s+(?:raw\s+)?"([^"]+)"', line)
        if m:
            target = (f.parent / m.group(1)).resolve()
            if not target.exists():
                print(f"{f}:{i}: missing open target {m.group(1)}")
                bad += 1
if bad:
    sys.exit(1)
print(f"ok: {len(files)} file(s) checked")
