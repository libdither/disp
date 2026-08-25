#!/usr/bin/env python
"""Annotation tier census for .disp files.

Buckets every top-level definition by the advancedness of its annotation:
  L0 none | L1 membership (`: Value`) | L2 sampled (`Pi (Sampled ..)`)
  L3 guard-tier tree-weak (arrows, AnyTree-only shapes)
  L4 guard-tier shape-precise (arrows with real domains/codomains)
  +dep flag: gate-derived (`arm `) or exact-output (`Point (`) content (L5/L6).

Heuristic line-based parse: a definition starts at column 0 with `name`,
`let name`, or `given name`; its annotation is the text between the first
` : ` and the first `:=`. Re-run after every annotation wave; judgment
calls (mixed slots, deliberate opt-outs) stay manual.

Usage: python scripts/annotation_census.py [file.disp ...]
"""
import re
import sys
from collections import Counter
from pathlib import Path

DEFAULT_FILES = ["lib/kernel/kernel.disp", "lib/kernel/types.disp"]
WEAK_TOKENS = {"AnyTree", "Pi", "Fn", "Sampled", "Guard", "Meta", "TwoFace", "Trials", "Enum", "t"}
DEF_RE = re.compile(r"^(let\s+|given\s+)?([A-Za-z_][A-Za-z0-9_']*)\s*(:=|:)")


def items(text: str):
    cur: list[str] = []
    for line in text.splitlines():
        starts = line and not line[0].isspace() and not line.startswith("//")
        if starts and cur:
            yield "\n".join(cur)
            cur = []
        if starts or cur:
            cur.append(line)
    if cur:
        yield "\n".join(cur)


def classify(item: str):
    m = DEF_RE.match(item)
    if not m:
        return None
    name = m.group(2)
    if name in ("test", "open"):
        return None
    assign = item.find(":=")
    colon = item.find(" : ")
    if colon == -1 or (assign != -1 and colon > assign):
        return name, "L0 none", ""
    ann = item[colon + 3 : assign if assign != -1 else len(item)].strip()
    dep = "+dep" if ("arm " in ann or "Point (" in ann) else ""
    if "Pi (Sampled" in ann:
        return name, "L2 sampled", dep
    if "->" in ann or re.search(r"\bPi\s+(Guard|Meta|TwoFace)\b", ann) or re.search(r"\bFn\b", ann):
        toks = set(re.findall(r"[A-Za-z_][A-Za-z0-9_']*", ann))
        tier = "L3 tree-weak" if toks <= WEAK_TOKENS else "L4 shape-precise"
        return name, tier, dep
    return name, "L1 membership", dep


def main() -> None:
    files = [a for a in sys.argv[1:] if not a.startswith("-")] or DEFAULT_FILES
    grand = Counter()
    for f in files:
        text = Path(f).read_text()
        rows = [r for r in (classify(i) for i in items(text)) if r]
        counts = Counter(tier for _, tier, _ in rows)
        grand.update(counts)
        print(f"\n== {f} ({len(rows)} defs)")
        for tier in sorted(counts):
            print(f"  {tier}: {counts[tier]}")
        if "-v" in sys.argv or "--verbose" in sys.argv:
            for name, tier, dep in rows:
                print(f"    {name:28s} {tier} {dep}")
        deps = [(n, t) for n, t, d in rows if d]
        if deps:
            print("  dependent/exact content:", ", ".join(n for n, _ in deps))
    print(f"\n== total ({sum(grand.values())} defs)")
    for tier in sorted(grand):
        print(f"  {tier}: {grand[tier]}")


if __name__ == "__main__":
    main()
