"""Paths named in tracked docs and code comments that no longer exist, and PDFs older than their .typ.

  python scripts/doc-links.py [PATH_PREFIX ...]

A reference is any `dir/file.ext` (md, typ, pdf, html, disp, rs, ts, js, py, sh, toml) in a tracked
doc (md, typ, html), or in a comment of a tracked source file (code itself is the compiler's to
check). It resolves against the repo root, the file's own directory, and each directory above that
file, so README-relative and repo-relative paths both count; failing those, any tracked file whose path
ends with it (`crate/src/x.rs` in a crate's README). A `.js` path also matches the `.ts` it compiles
from. References that name a commit next to
them ("in git at abc1234", "kept in abc1234") are history and are skipped. Exits 1 if anything is
missing.
"""
import os, re, subprocess, sys

root = subprocess.run(["git", "rev-parse", "--show-toplevel"], capture_output=True, text=True, check=True).stdout.strip()
os.chdir(root)
tracked = subprocess.run(["git", "ls-files"], capture_output=True, text=True, check=True).stdout.split("\n")
files = set(f for f in tracked if f)
dirs = set()
# Every tail of every tracked path: `a/b/c.rs` gives `b/c.rs` and `c.rs` too.
tails = set()
for f in files:
    parts = f.split("/")
    for i in range(1, len(parts)):
        tails.add("/".join(parts[i:]))
for f in files:
    d = os.path.dirname(f)
    while d:
        dirs.add(d)
        d = os.path.dirname(d)

TEXT = (".md", ".typ", ".rs", ".ts", ".mts", ".js", ".mjs", ".html", ".disp", ".py", ".sh", ".toml", ".wgsl", ".v", ".vh")
EXT = r"(?:md|typ|pdf|html|disp|rs|ts|mts|js|mjs|py|sh|toml)"
REF = re.compile(r"(?<![\w./-])((?:\.\./|\./)*(?:[\w.-]+/)+[\w.-]+\." + EXT + r")(?![\w/])")
HISTORY = re.compile(r"(in git at|kept in|kept at|removed in|deleted in|the deleted|recover via git|archived|at [0-9a-f]{7}\b|[0-9a-f]{7,40}\b)")
SKIP_DIRS = ("node_modules/", "vendor/", "website/static/")


def exists(path: str, src: str) -> bool:
    path = path.rstrip(".")
    alts = [path] + ([path[:-3] + ".ts", path[:-3] + ".mts"] if path.endswith(".js") else [])
    candidates = []
    for p in alts:
        candidates.append(p)
        d = os.path.dirname(src)
        while True:
            candidates.append(os.path.normpath(os.path.join(d, p)))
            if not d:
                break
            d = os.path.dirname(d)
    return any(c in files or c in dirs for c in candidates) or any(p.lstrip("./") in tails for p in alts)


DOCS = (".md", ".typ", ".html")
HASH = (".py", ".sh", ".toml")


def prose(src: str, line: str) -> str:
    """The part of a line that is prose: all of it in a doc, the comment in source."""
    if src.endswith(DOCS):
        return line
    if src.endswith(HASH):
        s = line.lstrip()
        return s if s.startswith("#") else ""
    i = line.find("//")
    while i > 0 and line[i - 1] == ":":
        i = line.find("//", i + 2)
    return line[i:] if i >= 0 else ""


prefixes = sys.argv[1:]
missing = []
for src in sorted(files):
    if not src.endswith(TEXT) or src.startswith(SKIP_DIRS) or (prefixes and not src.startswith(tuple(prefixes))):
        continue
    try:
        lines = open(src, encoding="utf-8").read().split("\n")
    except (UnicodeDecodeError, FileNotFoundError):
        continue
    for n, full in enumerate(lines, 1):
        line = prose(src, full)
        for m in REF.finditer(line):
            ref = m.group(1)
            start = max(line.rfind(c, 0, m.start()) for c in " \t(<[\"'`") + 1
            if "://" in line[start:m.end()] or line[start:].startswith("www."):
                continue
            if exists(ref, src) or HISTORY.search(line):
                continue
            missing.append(f"{src}:{n}: {ref}")

stale = []
for pdf in sorted(f for f in files if f.endswith(".pdf")):
    typ = pdf[:-4] + ".typ"
    if typ in files:
        when = lambda f: int(subprocess.run(["git", "log", "-1", "--format=%ct", "--", f], capture_output=True, text=True).stdout.strip() or 0)
        if when(pdf) < when(typ):
            stale.append(f"{pdf}: older than {typ}")

for m in missing:
    print(m)
for s in stale:
    print(s)
print(f"{len(missing)} missing references, {len(stale)} stale PDFs", file=sys.stderr)
sys.exit(1 if missing or stale else 0)
