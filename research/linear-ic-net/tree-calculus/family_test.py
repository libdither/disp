import random, threading
from collections import Counter
from rules import *
from family import DESIGNS, rules_of, make_compiler, wait_of
from check_rules import vars_of, rename as rn, unify, positions
from test_compile import gen, rtree, nf, arr, T
from test_linear import gen as gen_lin
import test_linear
from lower import sp
from ltc2 import fire2
from bench import programs, nat, tree, lst
import compile as compmod

def lower_k(t, K):
    if isinstance(t, tuple) and len(t) == 3: return ('λ', t[1], lower_k(t[2], K))
    if isinstance(t, tuple):
        h, args = sp(t)
        if h == L and len(args) >= 3:
            _, (c, d) = sp(args[0])
            w = lambda a: K(K(lower_k(a, K)))
            return A(L, A(L, w(c), w(d)), w(args[1]), lower_k(args[2], K), *[lower_k(a, K) for a in args[3:]])
        return (lower_k(t[0], K), lower_k(t[1], K))
    return t

def eager(rules):
    mult = {n: (vars_of(l), vars_of(r)) for n, l, r in rules}
    def run(t):
        st = Counter()
        def apply(f, x):
            h, args = spine(f)
            if len(args) < 2: return A(f, x)
            _, rhs, name, env = fire2([args[0], args[1], x], rules)
            st['steps'] += 1; st[name] += 1
            lv, rv = mult[name]
            for v in lv:
                if rv[v] == 0: st['dropped'] += size(env[v])
                if rv[v] > 1: st['copied'] += (rv[v] - 1) * size(env[v]); st['copies'] += 1
            return ev(rhs)
        def ev(t): return L if t == L else apply(ev(t[0]), ev(t[1]))
        return ev(t), st
    return run

def main():
    K = lambda q: A(L, L, A(L, q))
    for name, assign in [(n, a) for n, a in DESIGNS.items() if n[:2] in ('V4',)]:
        rules = rules_of(assign); comp = make_compiler(assign)
        # static: linear except one drop and one copy, 3 △ each, no overlaps
        drops = [n for n, l, r in rules if any(vars_of(r)[v] == 0 for v in vars_of(l))]
        copies = [n for n, l, r in rules if any(vars_of(r)[v] > 1 for v in vars_of(l))]
        eats = {size(l) - size(r) for n, l, r in rules}
        over = [1 for n1, l1, _ in rules for n2, l2, _ in rules for p, sub in positions(rn(l2, '_2'))
                if not (n1 == n2 and p == ()) and unify(rn(l1, '_1'), sub, {}) is not None]
        # random typed programs with triage and higher-order arguments
        rng = random.Random(29); nsteps = 0
        for _ in range(800):
            pt = rng.choice([arr(T, T, T), arr(arr(T, T), T, T), arr(arr(T, T, T), T, T, T)])
            prog = gen(pt, [], rng.randint(2, 5), rng); args = []; t = pt
            while t != T:
                args.append(rtree(rng.randint(1, 9), rng) if t[1] == T else gen(t[1], [], 3, rng)); t = t[2]
            got, n = normalize(A(comp(lower_k(prog, K)), *[comp(lower_k(a, K)) for a in args]), rules, 400000)
            assert got == nf(A(prog, *args)), name
            nsteps += n
        # random linear programs: no drop or copy fires
        rng = random.Random(31)
        for _ in range(800):
            ty = rng.choice([arr(T, T, T), arr(arr(T, T), T, T), arr(T, T, T, T)])
            prog = gen_lin(ty, [], rng.randint(1, 5), rng); args = []; t = ty
            while t != T:
                args.append(rtree(rng.randint(1, 8), rng) if t[1] == T else gen_lin(t[1], [], 3, rng)); t = t[2]
            st = Counter(); start = A(comp(prog), *[comp(a) for a in args])
            got, n = normalize(start, rules, 100000, st)
            assert got == nf(A(prog, *args)) and not any(st[c] for c in drops + copies), (name, st)
            assert 3 * n == size(start) - size(got)
        # eager recursive programs
        compmod.WAIT['fam'] = wait_of(name)
        progs = [comp(lower_k(p, K)) for p in programs('fam')]
        mirror, add, leaves, rev, fib = progs; addf = A(add, add)
        tot = Counter(); answers = []
        for call in (A(mirror, mirror, tree(7)), A(addf, nat(30), nat(25)), A(leaves, leaves, addf, tree(7)),
                     A(rev, rev, lst(25), L), A(fib, fib, addf, nat(11))):
            v, st = eager(rules)(call); tot += st; answers.append(v)
        print(f"{name}: drops {drops} copies {copies} eats {eats} overlaps {len(over)} | random {nsteps} steps ok"
              f" | linear ok | eager {tot['steps']} rewrites, {tot['copies']} copies, {tot['copied']} copied,"
              f" {tot['dropped']} dropped, code {sum(size(p) for p in progs)} △")
        globals().setdefault('ANS', answers)
        assert answers == ANS, name
    print('all designs give the same eager answers')
if __name__ == '__main__':
    threading.stack_size(512 * 1024 * 1024); th = threading.Thread(target=main); th.start(); th.join()
