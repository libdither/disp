"""Random linear programs (every bound variable used exactly once, with ρ and opened functions): every design
must agree with the term interpreter and fire no copy, erase, cancel, share or dup ⋈ dup rule.
usage: python lintest.py [count] [designs...]"""
import sys, random
from collections import Counter
from harness import run, problems
from check import run_ref
from engine import DESIGNS


def gen_linear(rng, n, vars, fresh):
    """a term of about n nodes that uses each name in vars exactly once"""
    if n <= 1 and len(vars) <= 1:
        if vars: return vars[0]
        return 'ρ' if rng.random() < 0.5 else ('λ', (x := fresh()), x)
    if len(vars) <= 1 and n >= 2 and rng.random() < 0.4:
        x = fresh()
        return ('Λ' if rng.random() < 0.15 else 'λ', x, gen_linear(rng, n - 1, vars + [x], fresh))
    vs = list(vars); rng.shuffle(vs)
    cut = rng.randint(0, len(vs))
    k = rng.randint(1, max(1, n - 2))
    return (gen_linear(rng, k, vs[:cut], fresh), gen_linear(rng, max(1, n - 1 - k), vs[cut:], fresh))


count = int(sys.argv[1]) if len(sys.argv) > 1 else 2000
designs = sys.argv[2:] or [d for d in DESIGNS if d != 'cancel-dup']
rng = random.Random(11)
ids = iter(range(10 ** 9))
progs = [gen_linear(rng, rng.randint(3, 40), [], lambda: f'v{next(ids)}') for _ in range(count)]
FORBIDDEN = ('copy', 'erase', 'cancel', 'share', 'dup⋈dup')
for d in designs:
    tally = Counter()
    bad = []
    for t in progs:
        kind, want, _ = run_ref(t)
        if kind == 'fuel': tally['skip'] += 1; continue
        verdict = 'ok'
        for s in (None, 1, 2):
            st, res, n = run(d, t, s, 100000)
            used = [k for k in n.stats if k.startswith(FORBIDDEN) and k != 'erased a fresh copy']
            if used or problems(n): verdict = f'seed {s}: fired {used} {problems(n)}'
            elif kind == 'value' and (st != 'done' or res != want): verdict = f'seed {s}: {st} {res} want {want}'
            elif kind == 'stuck' and st not in ('stuck', 'limit'): verdict = f'seed {s}: ref stuck, net {st}'
            if verdict != 'ok': break
        tally[(kind, verdict == 'ok')] += 1
        if verdict != 'ok' and len(bad) < 3: bad.append((t, verdict))
    print(f'{d:13}', dict(tally))
    for t, v in bad: print('   ', t, '\n     ', v)
