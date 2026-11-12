"""Random programs from ../check.py's generators under each design, in queue order and two random orders,
against the term interpreter. usage: python difftest.py [per-family count] [designs...]"""
import sys, random, time
from collections import Counter
from harness import run, problems
from check import gen, run_ref
from engine import DESIGNS

count = int(sys.argv[1]) if len(sys.argv) > 1 else 1500
designs = sys.argv[2:] or list(DESIGNS)
rng = random.Random(7)
progs = []
for family in ('plain', 'with ρ', 'readers'):
    for i in range(count):
        t = gen(rng, rng.randint(3, 40), [], family != 'plain', family == 'readers')
        progs.append((family, t))

for d in designs:
    t0 = time.time()
    tally, bad, fired = Counter(), [], Counter()
    for family, t in progs:
        kind, want, _ = run_ref(t)
        if kind == 'fuel':
            tally[(family, 'skip')] += 1
            continue
        verdict = 'ok'
        for s in (None, 1, 2):
            st, res, n = run(d, t, s, 100000)
            fired.update(k for k in n.stats if k.startswith(('cancel', 'share', 'expand')))
            p = problems(n)
            if p: verdict = f'seed {s}: {p}'
            elif kind == 'value' and (st != 'done' or res != want): verdict = f'seed {s}: {st} {res} want {want}'
            elif kind == 'stuck' and not (st in ('stuck', 'limit')): verdict = f'seed {s}: ref stuck, net {st}'
            if verdict != 'ok': break
        tally[(family, kind, verdict == 'ok')] += 1
        if verdict != 'ok' and len(bad) < 3: bad.append((t, verdict))
    ok = sum(v for k, v in tally.items() if len(k) == 3 and k[2])
    nbad = sum(v for k, v in tally.items() if len(k) == 3 and not k[2])
    skip = sum(v for k, v in tally.items() if len(k) == 2)
    print(f'{d:13} agree {ok}  disagree {nbad}  skipped(fuel) {skip}   {time.time() - t0:.0f}s   rules seen: {sorted(fired)}')
    print('   ', dict(sorted((str(k), v) for k, v in tally.items())))
    for t, v in bad: print('   MISMATCH', t, '\n     ', v)
    sys.stdout.flush()
