import random, threading
from collections import Counter
from rules import L, A, size, show, normalize, TC, redexes, get, put, spine
from ltc2 import fire2
from ltc7 import VARIANTS, make_compiler, wait_of, builders
from check_rules import vars_of, rename as rn, unify, positions
from check_dynamic import rterm, is_tree
from test_compile import gen, rtree, nf, arr, T
from test_linear import gen as gen_lin
from compile import compile_, br_tc
from lower import sp
def lower7(t, K):
    """tree calculus's triage: leaf -> c (free here), stem/fork arms drop the two arms they beat"""
    if isinstance(t, tuple) and len(t) == 3: return ('λ', t[1], lower7(t[2], K))
    if isinstance(t, tuple):
        h, args = sp(t)
        if h == L and len(args) >= 3:
            _, (c, d) = sp(args[0])
            lo = lambda z: lower7(z, K)
            return A(L, A(L, lo(c), K(K(lo(d)))), K(K(lo(args[1]))), lo(args[2]), *[lo(a) for a in args[3:]])
        return (lower7(t[0], K), lower7(t[1], K))
    return t
def main():
    for v, R in VARIANTS.items():
        comp = make_compiler(v); I, K, W, B, C = builders(v)
        drops = [n for n, l, r in R if any(vars_of(r)[x] == 0 for x in vars_of(l))]
        copies = [n for n, l, r in R if any(vars_of(r)[x] > 1 for x in vars_of(l))]
        over = [1 for n1, l1, _ in R for n2, l2, _ in R for p, s in positions(rn(l2, '_2'))
                if not (n1 == n2 and p == ()) and unify(rn(l1, '_1'), s, {}) is not None]
        trees = [L]
        for _ in range(2): trees = list(dict.fromkeys(trees + [A(L, t) for t in trees] + [A(L, t, u) for t in trees for u in trees]))
        stuck = sum(fire2([a, b, x], R) is None for a in trees for b in trees for x in trees)
        print(f'{v}: {len(R)} rules; drops {drops}; copies {copies}; overlaps {len(over)}; stuck redexes {stuck} of {len(trees) ** 3}')
        assert normalize(A(K('q'), 'x'), R)[0] == 'q'
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
        rng = random.Random(41); st_tc = st7 = 0
        for _ in range(2000):
            pt = rng.choice([arr(T, T, T), arr(arr(T, T), T, T), arr(arr(T, T, T), T, T, T), arr(T, T)])
            prog = gen(pt, [], rng.randint(2, 5), rng); args = []; t = pt
            while t != T:
                args.append(rtree(rng.randint(1, 9), rng) if t[1] == T else gen(t[1], [], 3, rng)); t = t[2]
            ref = nf(A(prog, *args))
            g2, n2 = normalize(A(comp(lower7(prog, K)), *[comp(lower7(a, K)) for a in args]), R, 400000)
            assert g2 == ref, v
            st7 += n2
        rng = random.Random(43); bad = 0
        for _ in range(2000):
            ty = rng.choice([arr(T, T, T), arr(arr(T, T), T, T), arr(T, T, T, T)])
            prog = gen_lin(ty, [], rng.randint(1, 5), rng); args = []; t = ty
            while t != T:
                args.append(rtree(rng.randint(1, 8), rng) if t[1] == T else gen_lin(t[1], [], 3, rng)); t = t[2]
            st = Counter(); start = A(comp(prog), *[comp(a) for a in args])
            got, n = normalize(start, R, 100000, st)
            assert got == nf(A(prog, *args))
            bad += st['T0'] + st['W']
        print(f'   {agree} random-order runs agree; 2000 typed programs right ({st7} normal-order steps; V5 took 99,330 on 3000);'
              f' 2000 linear programs right, drops+copies fired: {bad}')
if __name__ == '__main__':
    threading.stack_size(512 * 1024 * 1024); th = threading.Thread(target=main); th.start(); th.join()
