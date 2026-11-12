"""Rewrites per read, by case mechanism and program, split by kind; and how far each is from the floor.

Workloads (the same reads in every design):
  S suc        size on suc's code
  S size       size on the λ-view size's code (80 reads: the 'size size' of ../DESIGN.md)
  S code       size on the code of the λ-view size, quoted (no closure: nothing captured to drop, no Z run first)
  I quote      interp on the code of add 1 1
Run:  python measure.py            (all designs)
      python measure.py keep ref   (some)"""
import sys, os, time
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import casenet as C
import programs as P

VARIANTS = {
    'view': [('size', 'dsize'), ('size2', 'dsize2'), ('interp', 'dinterp'), ('interp2', 'dinterp2')],
    'hand': [('size', 'dsize'), ('size2', 'dsize2'), ('interp', 'dinterp'), ('interp2', 'dinterp2')],
    'once': [('size', 'dsize'), ('size2', 'dsize2'), ('interp', 'dinterp'), ('interp2', 'dinterp2')],
}
for d in ('keep', 'build', 'chain', 'ref'):
    VARIANTS[d] = [('size', 'dsize'), ('sizeK', 'dsizeK'), ('interp', 'dinterp'), ('interpK', 'dinterpK')]
WORK = {'size': ['suc', 'size', '((^d. size) ρ)'], 'interp': ['((^d. add (suc zero) (suc zero)) ρ)']}

def arm_agents(t):
    """agents in a piece of code, as the compiler lays it out (sharers and erasers included)"""
    n = C.CNet()
    env = {x: [] for x in C.free_names(t)}
    n.emit(t, env)
    return len(n.kind)

def the_table(t):
    """the first table in a term (pre-order)"""
    if C.is_tab(t): return t
    if isinstance(t, str) or t[0] == '~': return None
    for s in (t[2:] if len(t) == 3 else t):
        f = the_table(s)
        if f: return f
    return None

def measure(design, prog, arg, seeds=(None, 1)):
    t = C.parse(P.source(design, f'{prog} {arg}'))
    runs = []
    for s in seeds:
        status, res, st = C.run_net(t, s, 60000000)
        assert status == 'done' and not C.health(st), (design, prog, arg, status, C.health(st))
        runs.append(st)
    same_len = len({st['interactions'] for st in runs}) == 1
    st = runs[0]
    reads = st['reads'] + sum(v for k, v in st.items() if k.startswith('R⋈'))
    g = C.group(st)
    floor = None
    tab = the_table(C.parse(P.source(design, prog)))
    if tab is not None and design in ('keep', 'build', 'chain', 'ref'):
        sizes = [arm_agents(a) for a in tab[2]]
        floor = sum(st[f'slot {i}'] * sizes[i] for i in range(5))
    return dict(reads=reads, total=st['interactions'], same_len=same_len, inst=st['instantiated'], floor=floor, **g)

def main(designs):
    print(f"{'design':6} {'program':8} {'on':6} {'reads':>5} | {'per read':>8} {'copy':>6} {'annih':>6} {'erase':>6} {'(dup)':>6} {'rest':>6} | {'total':>8} | {'arm':>5} {'built':>6}")
    rows = []
    for d in designs:
        for label, prog in VARIANTS[d]:
            for arg in WORK['interp' if label.startswith('interp') else 'size']:
                m = measure(d, prog, arg)
                r = m['reads']
                on = 'code' if arg.startswith('((^d. size') else 'quote' if arg.startswith('((') else arg
                f = lambda k: f"{m.get(k, 0) / r:6.0f}"
                print(f"{d:6} {label:8} {on:6} {r:5} | {m['total'] / r:8.0f} {f('copy')} {f('annihilate')} {f('erase')} {f('erase dup')} {f('rest')} | {m['total']:8}"
                      f" | {(m['floor'] / r if m['floor'] is not None else float('nan')):5.1f} {(m['inst'] / r):6.1f}" + ('' if m['same_len'] else '  (length differs by order!)'))
                rows.append((d, label, on, m))
    return rows

LINEAR = {
    'hand': "hand[\\o a v p F k. k o a v p F | \\f a v p L k. k f a v p L | \\f o v p g b k. k f o v p g b | \\f o a p w k. k f o a p w | \\f o a v q k. k f o a v q] X (\\a b c d e k. k a b c d e)",
    'keep': "keep[\\t F k. k t F | \\t L k. k t L | \\t g b k. k t g b | \\t w k. k t w | \\t q k. k t q] X (\\t F k. k t F)",
    'ref': "ref[\\t F k. k t F | \\t L k. k t L | \\t g b k. k t g b | \\t w k. k t w | \\t q k. k t q] X (\\t F k. k t F)",
}

def linear():
    """a linear reader (every variable used once): what the mechanism itself copies and erases"""
    for d, src in LINEAR.items():
        for x in ('(\\x. x)', '(\\x y. y x)'):
            st, res, stats = C.run_net(C.parse(P.source(d, src.replace('X', x))), None)
            g = C.group(stats)
            print(f"  {d:5} reads {x:12} {st}: {stats['interactions']:3} rewrites, {g['copy']} copies, {g['annihilate']} annihilations, {g['erase']} erases")

if __name__ == '__main__':
    t0 = time.time()
    main(sys.argv[1:] or ['view', 'hand', 'once', 'keep', 'build', 'chain', 'ref'])
    print('linear readers:')
    linear()
    print(f"arm: agents in the taken arm, averaged over the reads (one copy of it is the floor for a table that is kept)")
    print(f"built: agents built from a ref table's code per read (ref only; a ref read is one rewrite)  ({time.time() - t0:.0f}s)")
