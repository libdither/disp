"""Every hand-written program of ../check.py (and size2) under every design, against the term interpreter,
in queue order and two random orders."""
import sys, time
from harness import run, problems
from programs import program, TESTS, READING
from check import run_ref
from engine import DESIGNS

designs = sys.argv[1:] or list(DESIGNS)
cases = TESTS + [(n, s) for n, s in READING if n.startswith('size2')]
bad = 0
for name, src in cases:
    t = program(src)
    kind, want, _ = run_ref(t, 3000000)
    line = []
    for d in designs:
        t0 = time.time()
        outs = [run(d, t, s) for s in (None, 3, 4)]
        ok = all(st == 'done' and res == want and not problems(n) for st, res, n in outs)
        if not ok:
            bad += 1
            print('BAD', d, name, [(st, problems(n), res == want) for st, res, n in outs])
        line.append(f"{d}:{outs[0][2].stats['interactions']}")
    print(f'{name:45}', ' '.join(line), flush=True)
print('bad', bad)
