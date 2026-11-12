"""Checks for the kept table on closures.
1. programs.TESTS: the table version of each of ../check.py's reading tests gives the term the existing net
   (../net.py) gives on the original, in queue order and two random orders; the reference agrees too.
2. programs.TABLE_TESTS: reads only a table design can do, against the reference (3 orders).
3. Random programs with tables (and tables in tables) against the reference, 3 orders.
4. ../check.py's adds: the same rewrites as RefRun, and the linear ones erase-free.
usage: python check_tab.py [random count] [parts: t,x,r,a]"""
import sys, os, random, time
from collections import Counter
HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import tnet as N
import tprograms as P
sys.path.insert(0, os.path.join(HERE, '..'))
import check as old
from engine import DESIGNS

SEEDS = (None, 3, 4)


def runs(t, limit=60000000):
    out = []
    for s in SEEDS:
        st, res, n = N.run(t, s, limit)
        out.append((st, res, N.problems(n), n.stats['interactions']))
    return out


def translated():
    bad = 0
    for name, orig, src in P.TESTS:
        st, want, _ = old.run_net(old.program(orig), None, 60000000)
        assert st == 'done', (orig, st)
        t = N.parse(P.source(src))
        got = runs(t)
        same = all(g[0] == 'done' and not g[2] and g[1] == want for g in got)
        rk, rv, _ = N.run_ref(t, 3000000)
        refok = rk == 'value' and rv == got[0][1]
        bad += not (same and refok)
        lens = sorted({g[3] for g in got})
        print(f"{'ok ' if same and refok else 'BAD'} {name:44} {got[0][3]:8} rewrites{'' if len(lens) == 1 else f' (orders: {lens})'}"
              f"{'' if refok else '  ref: ' + rk}" + ('' if same else f"\n    got {[(g[0], g[2]) for g in got]} {str(got[0][1])[:150]}\n    want {str(want)[:150]}"))
    return bad


def table_only():
    bad = 0
    for name, src in P.TABLE_TESTS:
        t = N.parse(P.source(src))
        got = runs(t)
        rk, rv, _ = N.run_ref(t, 3000000)
        ok = rk == 'value' and all(g[0] == 'done' and not g[2] and g[1] == rv for g in got)
        bad += not ok
        print(f"{'ok ' if ok else 'BAD'} {name:44} {got[0][3]:8} rewrites   {str(rv)[:70]}"
              + ('' if ok else f"\n    got {[(g[0], g[2]) for g in got]} {str(got[0][1])[:150]}  ref {rk}"))
    return bad


# ---- random programs with tables ----
def gen(rng, n, env, d=0):
    """d: how many tables this term sits in (at most 3, or six arms each holding tables never stop growing)"""
    if n == 1: return rng.choice(env + ['ρ', 'ρ'])
    r = rng.random()
    if d < 3 and n >= 3 and r < 0.25: return (tab(rng, env, d), gen(rng, max(1, n // 2), env, d))      # a read
    if d < 3 and n >= 3 and r < 0.32: return tab(rng, env, d)                                         # a table as a value
    if n == 2 or rng.random() < 0.3:
        x = f'v{len(env)}'
        return ('Λ' if rng.random() < 0.15 else 'λ', x, gen(rng, n - 1, env + [x], d))
    k = rng.randint(1, n - 2)
    return (gen(rng, k, env, d), gen(rng, n - 1 - k, env, d))

def tab(rng, env, d=0):
    arms = []
    for i in range(N.NSLOT):
        xs = [f't_{len(env)}'] + [f'p{j}_{len(env)}' for j in range(N.NPARTS[i])]
        body = gen(rng, rng.randint(1, 4), env + xs, d + 1)
        for x in reversed(xs): body = ('λ', x, body)
        arms.append(body)
    return ('tab', 'keep', tuple(arms))

def tables(t, depth=0):
    """(number of tables, deepest nesting of a table in a table)"""
    if isinstance(t, str) or t[0] == '~': return 0, 0
    if N.is_tab(t):
        c = [tables(a) for a in t[2]]
        return 1 + sum(x for x, _ in c), 1 + max(d for _, d in c)
    subs = [t[2]] if len(t) == 3 else [t[0], t[1]]
    c = [tables(s) for s in subs]
    return sum(x for x, _ in c), max(d for _, d in c)

def random_tests(count, seed=11, limit=100000):
    rng = random.Random(seed)
    tally, bad, limits = Counter(), [], []
    for _ in range(count):
        t = gen(rng, rng.randint(3, 30), [])
        kind, want, _ = N.run_ref(t)
        nt, depth = tables(t)
        tag = 'nested' if depth >= 2 else 'flat' if nt else 'none'
        if kind == 'fuel': tally[(tag, 'skip')] += 1; continue
        verdict = 'ok'
        for s in SEEDS:
            st, got, n = N.run(t, s, limit)
            p = N.problems(n)
            if st == 'limit': limits.append((t, s)); verdict = 'limit'; break
            if p: verdict = f'seed {s}: {p}'
            elif kind == 'value' and (st != 'done' or got != want): verdict = f'seed {s}: {st}'
            elif kind == 'stuck' and st != 'stuck': verdict = f'seed {s}: ref stuck, net {st}'
            if verdict != 'ok': break
        tally[(tag, kind, verdict if verdict in ('ok', 'limit') else 'BAD')] += 1
        if verdict not in ('ok', 'limit') and len(bad) < 5: bad.append((t, verdict, want))
    for k in sorted(tally): print('  ', *k, tally[k])
    for b in bad: print('  MISMATCH', str(b)[:600])
    for t, s in limits:
        st, got, n = N.run(t, s, 5000000)
        kind, want, _ = N.run_ref(t)
        print(f'   limit hit (seed {s}) rerun to 5M: {st}, {n.stats["interactions"]} rewrites, agrees: {got == want}, split {dict(N.split(n.stats))}')
    return len(bad)


def adds():
    """../check.py's adds: TabRun against RefRun, rewrite for rewrite; linear ones erase-free"""
    bad = 0
    for name, src in old.TESTS:
        if 'add' not in name and 'subtract' not in name: continue
        t = old.program(src)
        a = N.run(t, None, 60000000, DESIGNS['ref-run']())
        b = N.run(t, None, 60000000)
        er = sum(v for k, v in b[2].stats.items() if k.startswith('erase'))
        same = a[0] == b[0] == 'done' and a[1] == b[1] and a[2].stats['interactions'] == b[2].stats['interactions']
        lin = name.startswith(('linear', 'reversible'))
        ok = same and (not lin or er == 0)
        bad += not ok
        print(f"{'ok ' if ok else 'BAD'} {name:45} ref-run {a[2].stats['interactions']:7}  tab-run {b[2].stats['interactions']:7}  erases {er}")
    return bad


def main():
    t0 = time.time()
    args = sys.argv[1:]
    count = int(args.pop(0)) if args and args[0].isdigit() else 400
    parts = args[0].split(',') if args else ['t', 'x', 'r', 'a']
    bad = 0
    if 't' in parts:
        print('## translated reading tests (against the existing net and the reference)'); bad += translated()
        print(f'  ({time.time() - t0:.0f}s)')
    if 'x' in parts:
        print('## reads only a table design can do (against the reference)'); bad += table_only()
        print(f'  ({time.time() - t0:.0f}s)')
    if 'r' in parts:
        print(f'## {count} random programs with tables, 3 orders, against the reference'); bad += random_tests(count)
        print(f'  ({time.time() - t0:.0f}s)')
    if 'a' in parts:
        print('## the adds'); bad += adds()
    print('ALL OK' if not bad else f'{bad} FAILURES', f'({time.time() - t0:.0f}s)')

if __name__ == '__main__':
    N.in_thread(main)
