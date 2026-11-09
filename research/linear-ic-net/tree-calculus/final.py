"""The final design (V5), every check in one run."""
import random, threading
from collections import Counter
from rules import *
from family import DESIGNS, rules_of, make_compiler, wait_of
from check_rules import vars_of, rename as rn, unify, positions
from check_dynamic import rtree, rterm, is_tree
from test_compile import gen, nf, arr, T
from test_linear import gen as gen_lin
from family_test import lower_k, eager
from compile import compile_, br_tc, br_ltc
import compile as compmod
from bench import programs, nat, tree, lst
from ltc2 import fire2
NAME = 'V5 I K C | W B B2'
R = rules_of(DESIGNS[NAME]); comp = make_compiler(DESIGNS[NAME]); Kq = lambda q: A(L, L, A(L, q))

def main():
    print('rules:'); [print(f'  {n:3} {show(l):24} -> {show(r)}') for n, l, r in R]
    # 1 static
    for n, l, r in R:
        lv, rv = vars_of(l), vars_of(r)
        assert all(c == 1 for c in lv.values()) and size(l) - size(r) == 3
        assert (n == 'K') == any(rv[v] == 0 for v in lv) and (n == 'W') == any(rv[v] > 1 for v in lv)
    assert not [1 for n1, l1, _ in R for n2, l2, _ in R for p, s in positions(rn(l2, '_2'))
                if not (n1 == n2 and p == ()) and unify(rn(l1, '_1'), s, {}) is not None]
    trees = [L]
    for _ in range(2): trees = list(dict.fromkeys(trees + [A(L, t) for t in trees] + [A(L, t, u) for t in trees for u in trees]))
    assert all(fire2([a, b, x], R) for a in trees for b in trees for x in trees)
    print(f'1. static: left-linear, only K drops, only W copies, each rule eats exactly 3 △, no overlaps, '
          f'all {len(trees) ** 3} redexes over small trees fire')
    # 2 random order + size law at every step + footprints
    rng = random.Random(5); agree = steps = undone = 0
    for _ in range(4000):
        t = rterm(rng); ref, _ = normalize(t, R, 3000)
        if ref is None: continue
        assert is_tree(ref)
        for _ in range(3):
            u = t
            for _ in range(3000):
                ps = redexes(u); rng.shuffle(ps)
                for p in ps:
                    s = get(u, p); h, args = spine(s); r = fire2(args, R)
                    if r is None or r[0] != 'done': continue
                    _, new, name, env = r
                    u2 = put(u, p, new); dx = size(env['x']) if name in ('K', 'W') else 0
                    assert size(u2) == size(u) - 3 - (dx if name == 'K' else 0) + (dx if name == 'W' else 0)
                    if name != 'K':
                        lhs, rhs = next((l, h) for n, l, h in R if n == name); e2 = {}
                        def um(pt, tt):
                            if is_var(pt):
                                if pt in e2: return e2[pt] == tt
                                e2[pt] = tt; return True
                            if pt == L: return tt == L
                            return isinstance(tt, tuple) and um(pt[0], tt[0]) and um(pt[1], tt[1])
                        c0 = subst(rhs, env)   # the contractum, before any extra arguments
                        assert um(rhs, c0) and subst(lhs, e2) == A(L, *args[:3])
                        undone += 1
                    u = u2; steps += 1; break
                else: break
            assert u == ref; agree += 1
    print(f'2. random rule order: {agree} runs reach the same tree; the size law held on all {steps} steps; '
          f'{undone} non-K steps rebuilt exactly from result and rule name')
    # 3 random typed programs vs λ reference and tree calculus
    rng = random.Random(41); n_ok = 0; st_tc = st_l = 0
    for _ in range(3000):
        pt = rng.choice([arr(T, T, T), arr(arr(T, T), T, T), arr(arr(T, T, T), T, T, T), arr(T, T)])
        prog = gen(pt, [], rng.randint(2, 5), rng); args = []; t = pt
        while t != T:
            args.append(rtree(rng.randint(1, 9), rng) if t[1] == T else gen(t[1], [], 3, rng)); t = t[2]
        ref = nf(A(prog, *args))
        g1, n1 = normalize(A(compile_(prog, br_tc), *[compile_(a, br_tc) for a in args]), TC, 400000)
        g2, n2 = normalize(A(comp(lower_k(prog, Kq)), *[comp(lower_k(a, Kq)) for a in args]), R, 400000)
        assert g1 == ref and g2 == ref
        st_tc += n1; st_l += n2; n_ok += 1
    print(f'3. {n_ok} random typed programs (triage, higher-order arguments): λ reference, tree calculus and '
          f'LTC agree; steps {st_tc} vs {st_l}')
    # 4 linear programs
    rng = random.Random(43); fired = Counter()
    for _ in range(3000):
        ty = rng.choice([arr(T, T, T), arr(arr(T, T), T, T), arr(arr(T, T, T), T, T, T), arr(T, T, T, T)])
        prog = gen_lin(ty, [], rng.randint(1, 5), rng); args = []; t = ty
        while t != T:
            args.append(rtree(rng.randint(1, 8), rng) if t[1] == T else gen_lin(t[1], [], 3, rng)); t = t[2]
        st = Counter(); start = A(comp(prog), *[comp(a) for a in args])
        got, n = normalize(start, R, 100000, st)
        assert got == nf(A(prog, *args)) and st['K'] == 0 and st['W'] == 0 and 3 * n == size(start) - size(got)
        fired += st
    print(f'4. 3000 random linear programs: right answers, no K or W fires, 3·steps = size drop exactly; {dict(fired)}')
    # 5 eager recursive programs vs tree calculus and the first design
    rows = []
    for calc in ('tc', 'ltc1', 'final'):
        if calc == 'final':
            compmod.WAIT['fam'] = wait_of(NAME); ps = [comp(lower_k(p, Kq)) for p in programs('fam')]; rules = R
        else:
            ps = [compile_(p, br_tc if calc == 'tc' else br_ltc) for p in programs('tc' if calc == 'tc' else 'ltc')]
            rules = TC if calc == 'tc' else LTC
        mirror, add, leaves, rev, fib = ps; addf = A(add, add)
        calls = {'mirror (depth-7 tree)': A(mirror, mirror, tree(7)), 'add 30 25': A(addf, nat(30), nat(25)),
                 'leaves (depth-7 tree)': A(leaves, leaves, addf, tree(7)), 'reverse (25 items)': A(rev, rev, lst(25), L),
                 'fib 11': A(fib, fib, addf, nat(11))}
        rows.append({k: eager(rules)(c) for k, c in calls.items()})
    print('5. eager runs (rewrites, copies, nodes copied, nodes dropped): tree calculus | first design | final')
    for k in rows[0]:
        assert rows[0][k][0] == rows[1][k][0] == rows[2][k][0]
        cells = [f"{s['steps']:>6} {s['copies']:>5} {s['copied']:>7} {s['dropped']:>7}" for _, s in (r[k] for r in rows)]
        print(f'  {k:22} ' + ' | '.join(cells))
    tot = [sum(r[k][1]['steps'] for k in r) for r in rows]
    print(f'  total rewrites {tot}, final/TC = {tot[2] / tot[0]:.2f}, final/first = {tot[2] / tot[1]:.2f}')
threading.stack_size(512 * 1024 * 1024); th = threading.Thread(target=main); th.start(); th.join()
