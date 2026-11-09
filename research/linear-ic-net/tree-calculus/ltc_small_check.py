import random, threading
from collections import Counter
from rules import L, A, size, normalize, redexes, get, put, spine, show
from ltc2 import fire2
from ltc_small import LTC8, LTC7R, compiler
from check_rules import vars_of, rename as rn, unify, positions
from check_dynamic import rterm
from test_compile import gen, rtree, nf, arr, T
from test_linear import gen as gen_lin
from family_test import lower_k
def uses_all(t):
    """every λ uses its variable at least once (a λI-term)"""
    def occ(x, m): return m == x or (isinstance(m, tuple) and len(m) == 2 and (occ(x, m[0]) or occ(x, m[1]))) \
        or (isinstance(m, tuple) and len(m) == 3 and m[1] != x and occ(x, m[2]))
    if isinstance(t, tuple) and len(t) == 3: return occ(t[1], t[2]) and uses_all(t[2])
    if isinstance(t, tuple): return uses_all(t[0]) and uses_all(t[1])
    return True
def has_tri(t):
    if isinstance(t, tuple) and len(t) == 3: return has_tri(t[2])
    if isinstance(t, tuple):
        h, args = t, []
        while isinstance(h, tuple) and len(h) == 2: args.append(h[1]); h = h[0]
        return (h == L and len(args) >= 3) or has_tri(t[0]) or has_tri(t[1])
    return False
def main():
    for name, R, which in (('LTC8', LTC8, '8'), ('LTC7r', LTC7R, '7r')):
        comp, K = compiler(which)
        drops = [n for n, l, r in R if any(vars_of(r)[x] == 0 for x in vars_of(l))]
        copies = [n for n, l, r in R if any(vars_of(r)[x] > 1 for x in vars_of(l))]
        over = [1 for n1, l1, _ in R for n2, l2, _ in R for p, s in positions(rn(l2, '_2'))
                if not (n1 == n2 and p == ()) and unify(rn(l1, '_1'), s, {}) is not None]
        trees = [L]
        for _ in range(2): trees = list(dict.fromkeys(trees + [A(L, t) for t in trees] + [A(L, t, u) for t in trees for u in trees]))
        stuck = [(a, b, x) for a in trees for b in trees for x in trees if fire2([a, b, x], R) is None]
        shapes = {show(A(L, a, b, 'x'))[:22] for a, b, x in stuck[:3]}
        print(f'{name}: {len(R)} rules; drops {drops}; copies {copies}; overlaps {len(over)}; '
              f'stuck {len(stuck)} of {len(trees) ** 3} (e.g. {sorted(shapes)[:1]})')
        rng = random.Random(5); agree = 0
        for _ in range(3000):
            t = rterm(rng); ref, _ = normalize(t, R, 3000)
            if ref is None: continue
            for _ in range(2):
                u = t
                for _ in range(3000):
                    ps = redexes(u); rng.shuffle(ps)
                    for p in ps:
                        h, args = spine(get(u, p)); r = fire2(args, R)
                        if r and r[0] == 'done': u = put(u, p, r[1]); break
                    else: break
                assert u == ref; agree += 1
        rng = random.Random(43); nlin = 0; bad = 0
        for _ in range(2000):
            ty = rng.choice([arr(T, T, T), arr(arr(T, T), T, T), arr(T, T, T, T)])
            prog = gen_lin(ty, [], rng.randint(1, 5), rng); args = []; t = ty
            while t != T:
                args.append(rtree(rng.randint(1, 8), rng) if t[1] == T else gen_lin(t[1], [], 3, rng)); t = t[2]
            st = Counter(); got, n = normalize(A(comp(prog), *[comp(a) for a in args]), R, 100000, st)
            assert got == nf(A(prog, *args)); bad += st['K'] + st['W']; nlin += 1
        rng = random.Random(47); nI = 0
        while nI < 1500:
            pt = rng.choice([arr(T, T, T), arr(arr(T, T), T, T), arr(arr(T, T, T), T, T, T)])
            prog = gen(pt, [], rng.randint(2, 5), rng)
            args = []; t = pt
            while t != T:
                args.append(rtree(rng.randint(1, 9), rng) if t[1] == T else gen(t[1], [], 3, rng)); t = t[2]
            if has_tri(prog) or not uses_all(prog) or not all(uses_all(a) and not has_tri(a) for a in args): continue
            got, n = normalize(A(comp(prog), *[comp(a) for a in args]), R, 400000)
            assert got == nf(A(prog, *args)); nI += 1
        extra = ''
        if which == '8':
            rng = random.Random(41); nt = 0
            for _ in range(1500):
                pt = rng.choice([arr(T, T, T), arr(arr(T, T), T, T), arr(arr(T, T, T), T, T, T), arr(T, T)])
                prog = gen(pt, [], rng.randint(2, 5), rng); args = []; t = pt
                while t != T:
                    args.append(rtree(rng.randint(1, 9), rng) if t[1] == T else gen(t[1], [], 3, rng)); t = t[2]
                got, n = normalize(A(comp(lower_k(prog, K)), *[comp(lower_k(a, K)) for a in args]), R, 400000)
                assert got == nf(A(prog, *args)); nt += 1
            extra = f'; {nt} typed programs with triage and dropping right'
        print(f'   {agree} random-order runs agree; {nlin} linear programs right (K+W fired: {bad}); '
              f'{nI} λI programs (every variable used, some twice) right{extra}')
if __name__ == '__main__':
    threading.stack_size(512 * 1024 * 1024); th = threading.Thread(target=main); th.start(); th.join()
