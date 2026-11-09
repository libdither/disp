import random, threading
from collections import Counter
from rules import L, A, normalize, redexes, get, put, spine
from ltc2 import fire2
from ltc7 import VARIANTS, compiler_7c
from ltc7_check import lower7
from check_rules import vars_of, rename as rn, unify, positions
from check_dynamic import rterm, is_tree
from test_compile import gen, rtree, nf, arr, T
from test_linear import gen as gen_lin
def main():
    R = VARIANTS['7c']; comp, K, wait = compiler_7c()
    drops = [n for n, l, r in R if any(vars_of(r)[x] == 0 for x in vars_of(l))]
    copies = [n for n, l, r in R if any(vars_of(r)[x] > 1 for x in vars_of(l))]
    over = [1 for n1, l1, _ in R for n2, l2, _ in R for p, s in positions(rn(l2, '_2'))
            if not (n1 == n2 and p == ()) and unify(rn(l1, '_1'), s, {}) is not None]
    trees = [L]
    for _ in range(2): trees = list(dict.fromkeys(trees + [A(L, t) for t in trees] + [A(L, t, u) for t in trees for u in trees]))
    stuck = sum(fire2([a, b, x], R) is None for a in trees for b in trees for x in trees)
    assert normalize(A(K('q'), 'x'), R)[0] == 'q' and normalize(A(wait('f', 'x'), 'v'), R)[0] == A('f', 'x', 'v')
    rng = random.Random(5); agree = 0
    for _ in range(3000):
        t = rterm(rng); ref, _ = normalize(t, R, 3000)
        if ref is None: continue
        assert is_tree(ref)
        for _ in range(2):
            u = t
            for _ in range(3000):
                ps = redexes(u); rng.shuffle(ps)
                for p in ps:
                    h, args = spine(get(u, p)); r = fire2(args, R)
                    if r and r[0] == 'done': u = put(u, p, r[1]); break
                else: break
            assert u == ref; agree += 1
    rng = random.Random(41)
    for _ in range(2000):
        pt = rng.choice([arr(T, T, T), arr(arr(T, T), T, T), arr(arr(T, T, T), T, T, T), arr(T, T)])
        prog = gen(pt, [], rng.randint(2, 5), rng); args = []; t = pt
        while t != T:
            args.append(rtree(rng.randint(1, 9), rng) if t[1] == T else gen(t[1], [], 3, rng)); t = t[2]
        g, n = normalize(A(comp(lower7(prog, K)), *[comp(lower7(a, K)) for a in args]), R, 400000)
        assert g == nf(A(prog, *args))
    rng = random.Random(43); bad = 0
    for _ in range(2000):
        ty = rng.choice([arr(T, T, T), arr(arr(T, T), T, T), arr(T, T, T, T)])
        prog = gen_lin(ty, [], rng.randint(1, 5), rng); args = []; t = ty
        while t != T:
            args.append(rtree(rng.randint(1, 8), rng) if t[1] == T else gen_lin(t[1], [], 3, rng)); t = t[2]
        st = Counter(); got, n = normalize(A(comp(prog), *[comp(a) for a in args]), R, 100000, st)
        assert got == nf(A(prog, *args)); bad += st['T0'] + st['S']
    print(f'7c: {len(R)} rules; drops {drops}; copies {copies}; overlaps {len(over)}; stuck {stuck} of {len(trees) ** 3}; '
          f'{agree} random-order runs agree; 2000 typed programs right; 2000 linear programs right, drops+copies fired: {bad}')
threading.stack_size(512 * 1024 * 1024); th = threading.Thread(target=main); th.start(); th.join()
