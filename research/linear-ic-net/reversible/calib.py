"""How far is bracket abstraction from the smallest tree on small routing jobs?"""
import sys
from common import comp, lam, A, L, size
import so2
from so2 import atoms, napp, fork, stem, search
names = ['a', 'b', 'c', 'd', 'k']
at = dict(zip(names, atoms(*names)))
def tgt_term(t):
    # prototype λ-free term over variable names -> so2 target (atoms applied = neutral; △ applications = nodes)
    if t == 'L': return 0
    if isinstance(t, str): return at[t]
    f, x = t
    fv, xv = tgt_term(f), tgt_term(x)
    if fv == 0: return (xv,)
    if type(fv) is tuple and len(fv) == 1: return (fv[0], xv)
    return ('@', fv, xv)
jobs = [
    (['a', 'b', 'k'], A('k', 'b', 'a')),
    (['a', 'b', 'c', 'k'], A('k', 'c', 'a', 'b')),
    (['a', 'b', 'k'], A('k', A(L, 'a', 'b'))),
    (['a', 'b', 'c', 'k'], A('k', A(L, 'a', 'b'), 'c')),
    (['a', 'b', 'k'], A('k', A(L, A(L, 'a', 'b'), 'b'))),
    (['k', 'a', 'b'], A('k', 'b', A(L, 'a'))),
    (['a', 'b', 'c', 'k'], A('k', A(L, A(L, 'c', 'a'), 'b'))),
]
for vs, body in jobs:
    t = comp(lam(*vs, body))
    target = tgt_term(body)
    args = [at[v] for v in vs]
    best = search([(args, target)], nmax=min(size(t), 19), want=1, verbose=False, limit=40)
    b = best[0] if best else None
    print(f'{" ".join(vs):10} -> {str(body):40} bracket {size(t):3}   smallest {b[0] if b else ">19":>3}' + (f' ({b[1]} steps) {so2.show(b[2])}' if b else ''), flush=True)
