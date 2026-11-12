"""Problem (ii) in the combined design: the four case_table programs that hit the limit (/tmp/mm.pkl), translated
to six-slot kept tables (keep, chain and RHO-built tables all keep their table; a table read as prim with itself
as the part becomes the sixth arm  \\t a1..a6. arm5 t keep[a1|…|a6]), run against the reference.
Also: the reference answer's size as a tree and as a DAG (shared subterms counted once).
usage: python nested.py [limits...]"""
import sys, os, pickle, time
HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import tnet as N


def spine(t):
    args = []
    while isinstance(t, tuple) and len(t) == 2 and t[0] != '~':
        args.append(t[1]); t = t[0]
    return t, args[::-1]

def translate(t):
    if isinstance(t, str): return t
    if t[0] == '~': return t
    if t[0] == 'tab': return six(tuple(translate(a) for a in t[2]))
    head, args = spine(t)
    if head == 'RHO' and len(args) == 5: return six(tuple(translate(a) for a in args))
    if head == 'RHO': raise ValueError('a partial RHO')
    if len(t) == 3: return (t[0], t[1], translate(t[2]))
    return (translate(t[0]), translate(t[1]))

def six(arms):
    zs = [f'z{j}_six' for j in range(1, 7)]
    body = ((arms[4], 'tt_six'), ('tab', 'keep', tuple(zs)))
    for x in reversed(['tt_six'] + zs): body = ('λ', x, body)
    return ('tab', 'keep', arms + (body,))

def tree_dag(t):
    seen = set()
    def tree(t):
        if isinstance(t, str): return 1
        if t[0] == '~': return 1 + tree(t[1])
        if N.is_tab(t): return 1 + sum(tree(a) for a in t[2])
        if len(t) == 3: return 1 + tree(t[2])
        return 1 + tree(t[0]) + tree(t[1])
    def dag(t):
        if isinstance(t, str) or id(t) in seen: return 0
        seen.add(id(t))
        if t[0] == '~': return 1 + dag(t[1])
        if N.is_tab(t): return 1 + sum(dag(a) for a in t[2])
        if len(t) == 3: return 1 + dag(t[2])
        return 1 + dag(t[0]) + dag(t[1])
    return tree(t), dag(t)


def main():
    limits = [int(a) for a in sys.argv[1:]] or [100000, 400000, 1600000]
    progs = pickle.load(open('/tmp/mm.pkl', 'rb'))
    for k, (policy, t0) in enumerate(progs):
        try: t = translate(t0)
        except ValueError as e: print(f'#{k} (was {policy}) not translated: {e}'); continue
        m = N.Machine(10 ** 7)
        clock = time.time()
        try:
            v = m.run(t)
            tr, dg = tree_dag(v)
            ref = f'value: {m.betas} betas, answer {tr} nodes as a tree, {dg} as a DAG'
            want = N.canon(v)
        except Exception as e:
            ref, want = f'{type(e).__name__}', None
        print(f'#{k} (was {policy}) reference {ref}')
        for lim in limits:
            try: st, res, n = N.run(t, None, lim)
            except MemoryError:
                n = N.Net(N.TabRun(), None); n.load(t); st = n.run(lim) + ' (readback too large)'; res = None
            c = N.split(n.stats)
            print(f'   limit {lim:8}: {st:8} alive {len(n.kind):8} reads {N.nreads(n.stats):4} agrees {res == want if st == "done" else "-"}  '
                  + ' '.join(f'{g} {c[g]}' for g in N.GROUPS) + f'  ({time.time() - clock:.0f}s)')
            sys.stdout.flush()
            if st != 'limit': break
        if want is None: continue
        if st == 'done':
            for s in (3, 4):
                st2, res2, n2 = N.run(t, s, lim)
                print(f'   seed {s}: {st2}, {n2.stats["interactions"]} rewrites, agrees {res2 == want}')


if __name__ == '__main__':
    N.in_thread(main)
