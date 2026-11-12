"""The table: rewrites per read by design and program, split by kind; then the adds (non-reading) totals,
and the erases and copies started on the linear and reversible adds. Queue order (deterministic).
usage: python measure.py [designs...]"""
import sys
from harness import run, split, reads, problems
from programs import program, READING, ADDS, LINEAR
from engine import DESIGNS

designs = sys.argv[1:] or [d for d in DESIGNS if d != 'cancel-dup']
KINDS = ['copy', 'erase', 'annihilate', 'cancel', 'share', 'expand', 'read', 'rest']

print('## rewrites per read (R ⋈ X count in brackets); "agents" = agents a rule built, per read')
print(f"{'program':24} {'design':13} {'total':>8} {'/read':>7} " + ' '.join(f'{k:>6}' for k in KINDS) + f" {'agents':>7} {'fresh-erased':>12}")
for name, src in READING:
    t = program(src)
    for d in designs:
        st, res, n = run(d, t)
        r = reads(n.stats) or 1
        c = split(n.stats)
        tot = n.stats['interactions']
        print(f"{name + ' [' + str(r) + ']':24} {d:13} {tot:8} {tot / r:7.1f} " + ' '.join(f'{c[k] / r:6.1f}' for k in KINDS)
              + f" {n.alloc / r:7.1f} {n.stats['erased a fresh copy']:12}" + ('' if st == 'done' and not problems(n) else f'  {st} {problems(n)}'))
    sys.stdout.flush()

print()
print('## the adds: total rewrites (copy / erase / annihilate / rest) ; sig-started copies')
for name, src in ADDS:
    t = program(src)
    row = []
    for d in designs:
        st, res, n = run(d, t)
        c = split(n.stats)
        sigs = sum(v for k, v in n.stats.items() if k.startswith('copy') and k.endswith('by sig'))
        row.append(f"{d}:{n.stats['interactions']}(c{c['copy']} e{c['erase']} s{sigs})" + ('' if st == 'done' else st))
    print(f'{name:45}', ' '.join(row))
    sys.stdout.flush()

print()
print('## linear and reversible adds: erases (must be 0) and copies started by sig, per design')
for name, src in LINEAR:
    t = program(src)
    row = []
    for d in designs:
        st, res, n = run(d, t)
        er = sum(v for k, v in n.stats.items() if k.startswith('erase'))
        sigs = sum(v for k, v in n.stats.items() if k.startswith('copy') and k.endswith('by sig'))
        row.append(f'{d}: erase {er} sig {sigs}')
    print(f'{name:45}', ' | '.join(row))
