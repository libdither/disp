"""Shortest V5 trees for common routing patterns, against what bracket abstraction emits.
A candidate t implements λx1..xk. body if t x1 .. xk normalizes to body (free variables block nothing they need not)."""
import sys, time, itertools
from rules import L, A, size, show, normalize
from family import DESIGNS, rules_of, make_compiler
R = rules_of(DESIGNS['V5 I K C | W B B2']); comp = make_compiler(DESIGNS['V5 I K C | W B B2'])
def trees(n, memo={}):
    if n in memo: return memo[n]
    out = [L] if n == 1 else []
    if n >= 2: out += [A(L, t) for t in trees(n - 1)]
    if n >= 3: out += [A(L, a, b) for k in range(1, n - 1) for a in trees(k) for b in trees(n - 1 - k)]
    memo[n] = out; return out
def lam(*a):
    *xs, body = a
    for x in reversed(xs): body = ('λ', x, body)
    return body
TARGETS = {
    'T  λx f. f x': (['x', 'f'], A('f', 'x')),
    'C  λf x y. f y x': (['f', 'x', 'y'], A('f', 'y', 'x')),
    'B  λf g x. f (g x)': (['f', 'g', 'x'], A('f', A('g', 'x'))),
    'V  λx y f. f x y': (['x', 'y', 'f'], A('f', 'x', 'y')),
    'R  λx y z. y z x': (['x', 'y', 'z'], A('y', 'z', 'x')),
    'S  λf g x. f x (g x)': (['f', 'g', 'x'], A('f', 'x', A('g', 'x'))),
    'M  λx. x x': (['x'], A('x', 'x')),
    'W  λf x. f x x': (['f', 'x'], A('f', 'x', 'x')),
}
def steps_of(t, vs, body):
    nf, n = normalize(A(t, *vs), R, 60)
    return n if nf == body else None
maxn = int(sys.argv[1]) if len(sys.argv) > 1 else 12
if __name__ != "__main__": TARGETS = {}
t0 = time.time()
for name, (vs, body) in TARGETS.items():
    c = comp(lam(*vs, body)); cs = steps_of(c, vs, body)
    best = None
    for n in range(1, maxn + 1):
        for t in trees(n):
            s = steps_of(t, vs, body)
            if s is not None and (best is None or (n, s) < (best[0], best[1])): best = (n, s, t)
        if best: break
    print(f'{name:22} compiled: size {size(c):3} steps {cs} | shortest: ' +
          (f'size {best[0]} steps {best[1]}  {show(best[2])}' if best else f'none up to size {maxn}'))
print(f'{time.time() - t0:.0f}s')
