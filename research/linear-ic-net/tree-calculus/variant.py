"""What should the ninth slot (△ (△ (△ f g)) b x) be? Compare C' = f (g x) b with B' = f (g (b x)),
and check every non-K rule can be undone from its result and its name (a footprint)."""
import threading, random
from collections import Counter
from rules import *
import ltc2
from ltc2 import LTC2, I, K, B, W, C, C2, is_B, occurs, rename, fresh, fire2
LTC2B = LTC2[:5] + [('B2', A(L, A(L, A(L, 'f', 'g')), 'b', 'x'), A('f', A('g', A('b', 'x'))))] + LTC2[6:]
def B2(f, g, b): return A(L, A(L, A(L, f, g)), b)
def br_b(x, m):
    if m == x: return I
    if not occurs(x, m): return K(m)
    p, q = m
    if q == x and not occurs(x, p): return p
    if not occurs(x, p):
        r = br_b(x, q); gr = is_B(r)
        return B2(p, gr[0], gr[1]) if gr else B(p, r)
    if not occurs(x, q): return C(br_b(x, p), q)
    y = next(fresh)
    return W(br_b(x, br_b(y, (p, rename(q, x, y)))))
def compile_b(t):
    if isinstance(t, tuple) and len(t) == 3: return br_b(t[1], compile_b(t[2]))
    if isinstance(t, tuple): return (compile_b(t[0]), compile_b(t[1]))
    return t

# footprint check: from (rule name, result) rebuild the redex, for every rule but K
def unmatch(p, t, env):
    if is_var(p):
        if p in env: return env[p] == t
        env[p] = t; return True
    if p == L: return t == L
    return isinstance(t, tuple) and unmatch(p[0], t[0], env) and unmatch(p[1], t[1], env)
from check_dynamic import rtree
rng = random.Random(1); undone = 0
for _ in range(20000):
    a, b, x = (rtree(rng.randint(1, 6), rng) for _ in range(3))
    r = fire2([a, b, x], LTC2)
    if r is None or r[0] != 'done' or r[2] == 'K': continue
    _, res, name, env = r
    lhs, rhs = next((l, h) for n, l, h in LTC2 if n == name)
    e2 = {}
    assert unmatch(rhs, res, e2) and subst(lhs, e2) == A(L, a, b, x)
    undone += 1
print(f'footprints: {undone} random non-K steps, each rebuilt exactly from its result and rule name')

def main():
    import bench2
    from bench import programs, nat, tree, lst
    from lower import lower
    for label, rules, comp in (("ninth = C'", LTC2, ltc2.compile2), ("ninth = B'", LTC2B, compile_b)):
        tot = Counter()
        bench2.WAIT['ltc2'] = ltc2.wait2
        mirror, add, leaves, rev, fib = [comp(lower(p)) for p in programs('ltc2')]
        addf = A(add, add)
        for call in (A(mirror, mirror, tree(7)), A(addf, nat(30), nat(25)), A(leaves, leaves, addf, tree(7)),
                     A(rev, rev, lst(25), L), A(fib, fib, addf, nat(11))):
            v, st = bench2.make_run(rules)(call); tot += st
        print(f"{label}: {tot['steps']} rewrites over the five programs; its slot fired {tot['C2'] + tot['B2']} times")
threading.stack_size(512 * 1024 * 1024); th = threading.Thread(target=main); th.start(); th.join()
