"""Checks the case mechanisms.
1. check.py's reading tests, translated to each design, give the same term as the existing net (../net.py) gives
   on the original test, in queue order and in two random orders; the reference (casenet.Machine) agrees too.
2. Random programs whose reads go through tables of every kind: the net in three orders against the reference.
3. Every hand-written program of check.py still runs the same on the extended reducer (its rules only add).
Each run is also checked for: two copies' halves joined, a running agent copied or erased, agents left over."""
import sys, os, random, time
from collections import Counter
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import casenet as C
import programs as P
sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), '..'))
import check as old

SEEDS = (None, 3, 4)

def numeral(t):
    """the number a Church numeral term stands for, or None"""
    def ev(t, env):
        if isinstance(t, str): return env[t]
        if t[0] == '~': return ev(t[1], {})
        if t[0] == 'λ': return lambda v: ev(t[2], {**env, t[1]: v})
        if len(t) == 2: return ev(t[0], env)(ev(t[1], env))
        raise ValueError
    try:
        return ev(t, {})(lambda k: k + 1)(0)
    except Exception:
        return None

def translated(only=None):
    bad = 0
    want_cache = {}
    for d in P.DESIGNS:
        if only and d not in only: continue
        for name, orig, src in P.tests(d):
            if orig not in want_cache:
                st, want, _ = old.run_net(old.program(orig), None, 60000000)
                assert st == 'done', (orig, st)
                want_cache[orig] = want
            want = want_cache[orig]
            t = C.parse(P.source(d, src))
            got = []
            for s in SEEDS:
                status, res, stats = C.run_net(t, s, 60000000)
                got.append((status, res, C.health(stats), stats['interactions']))
            same = all(g[0] == 'done' and not g[2] and g[1] == want for g in got)
            how = 'same term' if same else ''
            if not same and all(g[0] == 'done' and not g[2] for g in got) and len({g[1] for g in got}) == 1:
                a, b = numeral(got[0][1]), numeral(want)
                if a is not None and a == b: same, how = True, f'same number ({a}), other term'
            refk, refv, _ = C.run_ref(t, 3000000)
            refok = refk == 'value' and refv == got[0][1]
            if not same or not refok: bad += 1
            print(f"{'ok ' if same and refok else 'BAD'} {d:6} {name:44} {got[0][3]:9} rewrites  {how}{'' if refok else '  ref: ' + refk}"
                  + ('' if same else f"\n    got {[(g[0], g[2]) for g in got]}\n    {str(got[0][1])[:200]}\n    want {str(want)[:200]}"))
    return bad


# ---- random programs with tables ----
PARAMS = {'once': lambda i: [], 'hand': lambda i: ['o1', 'o2', 'o3', 'o4']}
NPARTS = [1, 1, 2, 1, 1]

def gen(rng, n, env, policy):
    if n == 1:
        pool = env + ['ρ', 'ρ']
        return rng.choice(pool)
    r = rng.random()
    if n >= 3 and r < 0.25:
        return read(rng, n, env, policy)
    if n >= 3 and r < 0.32:
        return tab(rng, env, policy)            # a table as a value: copied, erased, passed around
    if n == 2 or rng.random() < 0.3:
        x = f'v{len(env)}'
        binder = 'Λ' if rng.random() < 0.15 else 'λ'
        return (binder, x, gen(rng, n - 1, env + [x], policy))
    k = rng.randint(1, n - 2)
    return (gen(rng, k, env, policy), gen(rng, n - 1 - k, env, policy))

def tab(rng, env, policy):
    arms = []
    for i in range(5):
        pre = PARAMS.get(policy, lambda i: ['t'])(i)
        xs = [f'{p}_{len(env)}' for p in pre] + [f'p{j}_{len(env)}' for j in range(NPARTS[i])]
        outer = [] if policy == 'ref' else env
        body = gen(rng, rng.randint(1, 4), outer + xs, policy)
        for x in reversed(xs): body = ('λ', x, body)
        arms.append(body)
    if policy == 'build':
        t = 'RHO'
        for a in arms: t = (t, a)
        return t
    return ('tab', policy, tuple(arms))

def read(rng, n, env, policy):
    return (tab(rng, env, policy), gen(rng, max(1, n // 2), env, policy))

def random_tests(count, seed=11):
    rng = random.Random(seed)
    tally = Counter()
    bad = []
    for policy in ('once', 'hand', 'keep', 'build', 'chain', 'ref'):
        for _ in range(count):
            t = gen(rng, rng.randint(3, 30), [], policy)
            kind, want, _ = C.run_ref(t)
            if kind == 'fuel':
                tally[(policy, 'skip')] += 1
                continue
            ok = True
            for s in SEEDS:
                status, got, stats = C.run_net(t, s)
                h = C.health(stats)
                if h or (kind == 'value' and (status != 'done' or got != want)) or (kind == 'stuck' and status not in ('stuck', 'limit')):
                    ok = False
                    if len(bad) < 5: bad.append((policy, s, t, kind, want, status, got, h))
                    break
            tally[(policy, kind, 'ok' if ok else 'BAD')] += 1
    for k in sorted(tally): print('  ', *k, tally[k])
    for b in bad: print('  MISMATCH', b)
    return len(bad)

def old_tests():
    bad = 0
    for name, src in old.TESTS:
        t = old.program(src)
        a = old.run_net(t, None, 60000000)
        b = C.run_net(t, None, 60000000)
        same = a[0] == b[0] == 'done' and a[1] == C.canon(b[1]) or a[1] == b[1]
        same = same and a[2]['interactions'] == b[2]['interactions']
        if not same: bad += 1; print('BAD old test', name)
    print(f'old tests on the extended reducer: {len(old.TESTS) - bad} of {len(old.TESTS)} the same, rewrite for rewrite')
    return bad

def main():
    t0 = time.time()
    args = sys.argv[1:]
    count = int(args.pop(0)) if args and args[0].isdigit() else 700
    only = args or None
    bad = translated(only)
    print(f'translated tests: {bad} bad ({time.time() - t0:.0f}s)')
    if not only:
        print('random programs, by table kind (3 orders each, against the reference):')
        bad += random_tests(count)
        bad += old_tests()
    print('ALL OK' if not bad else f'{bad} FAILURES', f'({time.time() - t0:.0f}s)')

if __name__ == '__main__':
    import threading, resource
    resource.setrlimit(resource.RLIMIT_AS, (3 << 30, 3 << 30))   # the reference can blow up a term: fail it, not the machine
    threading.stack_size(512 * 1024 * 1024)        # deep terms recurse deeply: without this the C stack overflows silently
    th = threading.Thread(target=main); th.start(); th.join()
