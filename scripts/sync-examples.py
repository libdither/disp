#!/usr/bin/env python
"""Copy regions of lib/tests/by_example.test.disp into the README's example blocks.

  python scripts/sync-examples.py           rewrite the README blocks
  python scripts/sync-examples.py --check   exit 1 if any block differs from its region

A region starts at a line `//@ name` and runs to the next one. A README block is a
```rust fence right after a line `<!-- example: name -->`. The Learn page reads the
same file directly (website/src/lib/learn/code-samples.ts).
"""
import re, sys, pathlib

ROOT = pathlib.Path(__file__).resolve().parent.parent
SOURCE = ROOT / 'lib' / 'tests' / 'by_example.test.disp'
README = ROOT / 'README.md'

def regions(text):
    out, name = {}, None
    for line in text.split('\n'):
        m = re.match(r'^//@ (\S+)\s*$', line)
        if m: name = m.group(1); out[name] = []; continue
        if name: out[name].append(line)
    return {k: '\n'.join(v).strip('\n') for k, v in out.items()}

def main():
    regs = regions(SOURCE.read_text())
    readme = README.read_text()
    missing = []
    def fill(m):
        name = m.group(1)
        if name not in regs: missing.append(name); return m.group(0)
        return f'<!-- example: {name} -->\n```rust\n{regs[name]}\n```'
    new = re.sub(r'<!-- example: (\S+) -->\n```rust\n.*?\n```', fill, readme, flags=re.S)
    if missing: sys.exit(f'README names regions that {SOURCE.name} lacks: {", ".join(missing)}')
    if '--check' in sys.argv:
        if new != readme: sys.exit('README examples differ from lib/tests/by_example.test.disp: run scripts/sync-examples.py')
        print('README examples match'); return
    README.write_text(new)
    print('README examples ' + ('updated' if new != readme else 'already match'))

if __name__ == '__main__':
    main()
