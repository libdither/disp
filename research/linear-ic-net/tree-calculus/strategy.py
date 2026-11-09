"""Linear programs (no K, no W): eager and normal-order take exactly the same number of steps.
Programs with K/W: the step difference comes only from what W copies and K drops."""
import random, threading
from collections import Counter
from rules import L, A, size, normalize
from radd import comp, R, eager
from test_linear import gen as gen_lin
from test_compile import gen, rtree, nf, arr, T
from family_test import lower_k
Kq = lambda q: A(L, L, A(L, q))
def main():
    rng = random.Random(7); same = 0
    for _ in range(2000):
        ty = rng.choice([arr(T, T, T), arr(arr(T, T), T, T), arr(T, T, T, T)])
        prog = gen_lin(ty, [], rng.randint(1, 5), rng); args = []; t = ty
        while t != T:
            args.append(rtree(rng.randint(1, 8), rng) if t[1] == T else gen_lin(t[1], [], 3, rng)); t = t[2]
        term = A(comp(prog), *[comp(a) for a in args])
        v1, s1 = eager(R)(term); v2, n2 = normalize(term, R, 100000)
        assert v1 == v2 and s1['steps'] == n2 == (size(term) - size(v1)) // 3
        same += 1
    print(f'{same} linear programs: eager steps = normal-order steps = (start - end) / 3, every time')
    rng = random.Random(9); diff = Counter()
    for _ in range(1000):
        pt = rng.choice([arr(T, T, T), arr(arr(T, T), T, T)])
        prog = gen(pt, [], rng.randint(2, 5), rng); args = []; t = pt
        while t != T:
            args.append(rtree(rng.randint(1, 9), rng) if t[1] == T else gen(t[1], [], 3, rng)); t = t[2]
        term = A(comp(lower_k(prog, Kq)), *[comp(lower_k(a, Kq)) for a in args])
        v1, s1 = eager(R)(term); v2, n2 = normalize(term, R, 400000)
        assert v1 == v2
        diff['eager fewer'] += s1['steps'] < n2; diff['equal'] += s1['steps'] == n2; diff['eager more'] += s1['steps'] > n2
    print(f'1000 programs with K and W: {dict(diff)}')
threading.stack_size(512 * 1024 * 1024); th = threading.Thread(target=main); th.start(); th.join()
