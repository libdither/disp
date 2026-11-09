"""Shortest trees for S and V by size, up to the size bracket abstraction emits."""
import sys, time
from rules import L, A, size, show, normalize
from superopt import trees, R, comp, lam, steps_of
TARGETS = {'V  λx y f. f x y': (['x', 'y', 'f'], A('f', 'x', 'y')),
           'S  λf g x. f x (g x)': (['f', 'g', 'x'], A('f', 'x', A('g', 'x')))}
maxn = int(sys.argv[1])
for name, (vs, body) in TARGETS.items():
    t0 = time.time(); best = None; tried = 0
    for n in range(1, maxn + 1):
        for t in trees(n):
            tried += 1
            s = steps_of(t, vs, body)
            if s is not None and (best is None or s < best[1]): best = (n, s, t)
        if best: break
    c = comp(lam(*vs, body))
    print(f'{name}: compiled size {size(c)} steps {steps_of(c, vs, body)}; shortest '
          + (f'size {best[0]} steps {best[1]} {show(best[2])}' if best else f'none up to size {maxn}')
          + f' ({tried} trees, {time.time() - t0:.0f}s)')
