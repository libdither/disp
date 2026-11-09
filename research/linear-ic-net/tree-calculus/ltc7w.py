"""7w: K kept (the only drop), W moved into triage: on a leaf, triage hands the fork arm over twice
(the only copy). Triage never drops. W q = C (△ (△ I q)) △: W q x -> △ (△ I q) x △ -> I q x x -> q x x."""
import itertools, random, threading
from collections import Counter
from rules import L, A, normalize, redexes, get, put, spine, size
from ltc2 import fire2
from check_rules import vars_of, rename as rn, unify, positions
from check_dynamic import rterm
from test_compile import gen, rtree, nf, arr, T
from lower import sp
from family_test import eager
from bench import programs, nat, tree, lst
import compile as compmod
R7W = [('I', A(L, L, L, 'x'), 'x'),
       ('K', A(L, L, A(L, 'q'), 'x'), 'q'),
       ('C', A(L, L, A(L, 'q', 'r'), 'x'), A('q', 'x', 'r')),
       ('B', A(L, A(L, 'p'), 'b', 'x'), A('p', A('b', 'x'))),
       ('T0', A(L, A(L, 'c', 'd'), 'b', L), A('c', 'd', 'b', 'b')),
       ('T1', A(L, A(L, 'c', 'd'), 'b', A(L, 'u')), A('d', 'c', 'b', 'u')),
       ('T2', A(L, A(L, 'c', 'd'), 'b', A(L, 'u', 'v')), A('b', 'c', 'd', 'u', 'v'))]
I = A(L, L, L); K = lambda q: A(L, L, A(L, q)); C = lambda q, r: A(L, L, A(L, q, r))
B = lambda f, g: A(L, A(L, f), g); W = lambda q: C(A(L, A(L, I, q)), L)
fresh = (f'_s{i}' for i in itertools.count())
def occ(x, m): return m == x or (isinstance(m, tuple) and len(m) == 2 and (occ(x, m[0]) or occ(x, m[1])))
def ren(m, x, y):
    if m == x: return y
    return (ren(m[0], x, y), ren(m[1], x, y)) if isinstance(m, tuple) else m
def br(x, m):
    if m == x: return I
    if not occ(x, m): return K(m)
    p, q = m
    if q == x and not occ(x, p): return p
    if not occ(x, p): return B(p, br(x, q))
    if not occ(x, q): return C(br(x, p), q)
    y = next(fresh); return W(br(x, br(y, (p, ren(q, x, y)))))
def comp(t):
    if isinstance(t, tuple) and len(t) == 3: return br(t[1], comp(t[2]))
    if isinstance(t, tuple): return (comp(t[0]), comp(t[1]))
    return t
def lower(t):   # tree calculus's triage: the leaf arm drops three things here, the others two
    if isinstance(t, tuple) and len(t) == 3: return ('λ', t[1], lower(t[2]))
    if isinstance(t, tuple):
        h, args = sp(t)
        if h == L and len(args) >= 3:
            _, (c, d) = sp(args[0]); lo = lower
            return A(L, A(L, K(K(K(lo(c)))), K(K(lo(d)))), K(K(lo(args[1]))), lo(args[2]), *[lo(a) for a in args[3:]])
        return (lower(t[0]), lower(t[1]))
    return t
def main():
    drops = [n for n, l, r in R7W if any(vars_of(r)[x] == 0 for x in vars_of(l))]
    copies = [n for n, l, r in R7W if any(vars_of(r)[x] > 1 for x in vars_of(l))]
    over = [1 for n1, l1, _ in R7W for n2, l2, _ in R7W for p, s in positions(rn(l2, '_2'))
            if not (n1 == n2 and p == ()) and unify(rn(l1, '_1'), s, {}) is not None]
    assert normalize(A(W('q'), 'x'), R7W)[0] == A('q', 'x', 'x')
    rng = random.Random(5); agree = 0
    for _ in range(2000):
        t = rterm(rng); ref, _ = normalize(t, R7W, 3000)
        if ref is None: continue
        for _ in range(2):
            u = t
            for _ in range(3000):
                ps = redexes(u); rng.shuffle(ps)
                for p in ps:
                    h, args = spine(get(u, p)); r = fire2(args, R7W)
                    if r and r[0] == 'done': u = put(u, p, r[1]); break
                else: break
            assert u == ref; agree += 1
    rng = random.Random(41); n_ok = 0
    for _ in range(1500):
        pt = rng.choice([arr(T, T, T), arr(arr(T, T), T, T), arr(arr(T, T, T), T, T, T), arr(T, T)])
        prog = gen(pt, [], rng.randint(2, 5), rng); args = []; t = pt
        while t != T:
            args.append(rtree(rng.randint(1, 9), rng) if t[1] == T else gen(t[1], [], 3, rng)); t = t[2]
        got, n = normalize(A(comp(lower(prog)), *[comp(lower(a)) for a in args]), R7W, 400000)
        assert got == nf(A(prog, *args)); n_ok += 1
    compmod.WAIT['fam'] = lambda f, x: C(B(A(L, L), A(L, f)), x)
    ps = [comp(lower(p)) for p in programs('fam')]
    mirror, add, leaves, rev, fib = ps; addf = A(add, add); tot = Counter()
    for call in (A(mirror, mirror, tree(7)), A(addf, nat(30), nat(25)), A(leaves, leaves, addf, tree(7)),
                 A(rev, rev, lst(25), L), A(fib, fib, addf, nat(11))):
        v, st = eager(R7W)(call); tot += st
    print(f'7w: {len(R7W)} rules; drops {drops}; copies {copies}; overlaps {len(over)}; {agree} random-order runs agree; '
          f'{n_ok} typed programs right; ordinary programs (5, eager): {tot["steps"]} rewrites (V5: 30,213), '
          f'{tot["copied"]} copied, {tot["dropped"]} dropped')
threading.stack_size(512 * 1024 * 1024); th = threading.Thread(target=main); th.start(); th.join()
