"""Where interactions go, and the cost of applying a function to n arguments, nets against the 9-rule calculus."""
import sys
from collections import Counter
from net import Net
from check import program, run_net
from blowup import families, ltc, run_ltc, A, L, size

def group(stats):
    g = Counter()
    for k, v in stats.items():
        if k == 'interactions': continue
        g['copy' if k.startswith('copy') or k == 'dup⋈dup' else 'erase' if k.startswith('erase') else
          'run (O⋈lam)' if k == 'O⋈lam' else 'thaw' if k.startswith('T⋈') else 'reflect' if k.startswith('R⋈') or k in ('O⋈rho', 'O⋈Lam') else k] += v
    return dict(g)

for name, src in [('binders in suc', 'size suc'), ('binders in size itself', 'size size')]:
    status, _, st = run_net(program(src), None, 3000000)
    print(f'{name}: {st["interactions"]} interactions', group(st))

print('\napplying to n arguments: 9-rule steps (+ nodes copied) against net interactions (+ agents copied)')
for n in (2, 4, 8, 16, 32):
    for fam, t in families(n):
        if fam == 'nested': continue
        nt = Net()
        args = [f'a{i}' for i in range(n)]
        app = t
        for a in args: app = (app, a)
        root = nt.load(app)
        assert nt.run() == 'done'
        st = nt.stats
        lt = run_ltc(A(ltc(t), *[A(L, A(L, L)) for _ in range(n)]))[1]
        print(f'{fam:11} n={n:2}  9-rule {lt["steps"]:5} steps (+{lt["copied"]} copied)   net {st["interactions"]:4} interactions (copy {sum(v for k, v in st.items() if k.startswith("copy"))})')
