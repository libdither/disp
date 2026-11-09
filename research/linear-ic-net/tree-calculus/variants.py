"""Triage protocols compared (beaten arms first / children first / packed in one fork),
and what each rule allocates and frees."""
import random, threading
from collections import Counter
from rules import L, A, size, show, normalize, TC
from family import DESIGNS, rules_of, make_compiler, wait_of
from family_test import eager
from check_rules import vars_of
from lower import sp
from bench import programs, nat, tree, lst
from test_compile import gen, rtree, nf, arr, T
import compile as compmod
NAME = 'V5 I K C | W B B2'
BASE = rules_of(DESIGNS[NAME])[:6]; comp = make_compiler(DESIGNS[NAME])
K = lambda q: A(L, L, A(L, q))
def lam(*a):
    *xs, body = a
    for x in reversed(xs): body = ('λ', x, body)
    return body
TRI = {
 'beaten first': [('T0', A(L, A(L, 'c', 'd'), 'b', L), A('c', 'd', 'b')),
                  ('T1', A(L, A(L, 'c', 'd'), 'b', A(L, 'u')), A('d', 'c', 'b', 'u')),
                  ('T2', A(L, A(L, 'c', 'd'), 'b', A(L, 'u', 'v')), A('b', 'c', 'd', 'u', 'v'))],
 'children first': [('T0', A(L, A(L, 'c', 'd'), 'b', L), A('c', 'd', 'b')),
                    ('T1', A(L, A(L, 'c', 'd'), 'b', A(L, 'u')), A('d', 'u', 'c', 'b')),
                    ('T2', A(L, A(L, 'c', 'd'), 'b', A(L, 'u', 'v')), A('b', 'u', 'v', 'c', 'd'))],
 'packed': [('T0', A(L, A(L, 'c', 'd'), 'b', L), A('c', A(L, 'd', 'b'))),
            ('T1', A(L, A(L, 'c', 'd'), 'b', A(L, 'u')), A('d', A(L, 'c', 'b'), 'u')),
            ('T2', A(L, A(L, 'c', 'd'), 'b', A(L, 'u', 'v')), A('b', A(L, 'c', 'd'), 'u', 'v'))],
}
WRAP = {  # arm a of tree calculus's triage -> arm that throws away what it beat
 'beaten first': (lambda a: K(K(a)), lambda a: K(K(a)), lambda a: K(K(a))),
 'children first': (lambda a: K(K(a)), lambda a: comp(lam('u', 'o1', 'o2', A(a, 'u'))),
                    lambda a: comp(lam('u', 'v', 'o1', 'o2', A(a, 'u', 'v')))),
 'packed': (lambda a: K(a), lambda a: K(a), lambda a: K(a)),
}
def lower(t, w):
    if isinstance(t, tuple) and len(t) == 3: return ('λ', t[1], lower(t[2], w))
    if isinstance(t, tuple):
        h, args = sp(t)
        if h == L and len(args) >= 3:
            _, (c, d) = sp(args[0])
            arms = [comp(lower(z, w)) for z in (c, d, args[1])]
            return A(L, A(L, w[0](arms[0]), w[1](arms[1])), w[2](arms[2]), lower(args[2], w),
                     *[lower(a, w) for a in args[3:]])
        return (lower(t[0], w), lower(t[1], w))
    return t
def apps(t):
    return 0 if not isinstance(t, tuple) else 1 + apps(t[0]) + apps(t[1])
def main():
    print('memory per rule (application nodes and leaves, left -> right; variables excluded):')
    for n, l, r in BASE + TRI['beaten first']:
        print(f'  {n:3} apps {apps(l)} -> {apps(r)}, leaves {size(l)} -> {size(r)}' +
              (', copies x' if n == 'W' else ', drops x' if n == 'K' else ''))
    compmod.WAIT['fam'] = wait_of(NAME)
    for proto in TRI:
        R = BASE + TRI[proto]; w = WRAP[proto]
        progs = [comp(lower(p, w)) for p in programs('fam')]
        mirror, add, leaves, rev, fib = progs; addf = A(add, add)
        tot = Counter()
        for call in (A(mirror, mirror, tree(7)), A(addf, nat(30), nat(25)), A(leaves, leaves, addf, tree(7)),
                     A(rev, rev, lst(25), L), A(fib, fib, addf, nat(11))):
            v, st = eager(R)(call); tot += st
        rng = random.Random(29); nsteps = 0
        for _ in range(800):
            pt = rng.choice([arr(T, T, T), arr(arr(T, T), T, T), arr(arr(T, T, T), T, T, T)])
            prog = gen(pt, [], rng.randint(2, 5), rng); args = []; t = pt
            while t != T:
                args.append(rtree(rng.randint(1, 9), rng) if t[1] == T else gen(t[1], [], 3, rng)); t = t[2]
            got, n = normalize(A(comp(lower(prog, w)), *[comp(lower(a, w)) for a in args]), R, 400000)
            assert got == nf(A(prog, *args)), proto
            nsteps += n
        print(f"{proto:15} eager (5 programs): {tot['steps']:6} rewrites, {tot['K']:5} K, {tot['copied']:7} copied, "
              f"{tot['dropped']:7} dropped | normal order, 800 random programs: {nsteps} steps, all right")
threading.stack_size(512 * 1024 * 1024); th = threading.Thread(target=main); th.start(); th.join()
