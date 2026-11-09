"""Random *linear* λ-programs (every variable used exactly once; tree data, higher-order arguments)
compiled to LTC2: right answer, no K and no W ever fires, and steps = (|before| - |after|) / 3 exactly."""
import random, itertools
from collections import Counter
from rules import *
from ltc2 import LTC2, compile2
from test_compile import nf, arr, T, rtree
names = (f'w{i}' for i in itertools.count())

def split(vs, rng):
    a, b = [], []
    for v in vs: (a if rng.random() < 0.5 else b).append(v)
    return a, b

def gen(ty, vs, d, rng):
    """a term of type ty using each variable in vs exactly once"""
    if ty != T:
        x = next(names)
        return ('λ', x, gen(ty[2], vs + [(x, ty[1])], d, rng))
    funs = [v for v in vs if v[1] != T]
    if funs and (d <= 0 or rng.random() < 0.4):
        f = rng.choice(funs); rest = [v for v in vs if v != f]; t = f[1]; args = []
        while t != T:
            mine, rest = split(rest, rng) if t[2] != T else (rest, [])
            args.append(gen(t[1], mine, d - 1, rng)); t = t[2]
        return A(f[0], *args)
    if len(vs) == 1 and vs[0][1] == T and (d <= 0 or rng.random() < 0.3): return vs[0][0]
    if not vs and (d <= 0 or rng.random() < 0.3): return L
    if rng.random() < 0.35: return A(L, gen(T, vs, d - 1, rng))
    a, b = split(vs, rng)
    return A(L, gen(T, a, d - 1, rng), gen(T, b, d - 1, rng))

def _main():
    rng = random.Random(3)
    types = [arr(T, T, T), arr(arr(T, T), T, T), arr(arr(T, T, T), T, T, T), arr(T, T, T, T)]
    ok = 0; fired = Counter()
    for _ in range(3000):
        ty = rng.choice(types)
        prog = gen(ty, [], rng.randint(1, 5), rng)
        args, t = [], ty
        while t != T:
            args.append(rtree(rng.randint(1, 8), rng) if t[1] == T else gen(t[1], [], 3, rng)); t = t[2]
        ref = nf(A(prog, *args))
        start = A(compile2(prog), *[compile2(a) for a in args])
        st = Counter()
        got, n = normalize(start, LTC2, 100000, st)
        assert got == ref
        assert st['K'] == 0 and st['W'] == 0, st
        assert 3 * n == size(start) - size(got), (n, size(start), size(got))
        fired += st; ok += 1
    print(f'{ok} random linear programs: right answers, no K and no W fired, 3·steps = size drop exactly every time; '
          f'rules fired: {dict(fired)}')

if __name__ == '__main__':
    _main()
