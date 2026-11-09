"""Random simply-typed programs over trees (leaf, stem, fork, triage, higher-order arguments),
run three ways: a direct λ evaluator, compiled to TC, compiled to LTC. All three must agree."""
import random, itertools
from compile import *

def sp(t):
    args = []
    while isinstance(t, tuple) and len(t) == 2: args.append(t[1]); t = t[0]
    return t, args[::-1]

def subst(m, x, n):
    if m == x: return n
    if isinstance(m, tuple) and len(m) == 3: return m if m[1] == x else ('λ', m[1], subst(m[2], x, n))
    if isinstance(m, tuple): return (subst(m[0], x, n), subst(m[1], x, n))
    return m

def whnf(t):
    while True:
        h, args = sp(t)
        if isinstance(h, tuple) and args:
            t = A(subst(h[2], h[1], args[0]), *args[1:]); continue
        if h == L and len(args) >= 3:
            _, cd = sp(whnf(args[0])); c, d = cd
            _, xs = sp(whnf(args[2]))
            r = c if not xs else A(d, *xs) if len(xs) == 1 else A(args[1], *xs)
            t = A(r, *args[3:]); continue
        return t

def nf(t):
    t = whnf(t); h, args = sp(t)
    return A(L, *[nf(a) for a in args]) if h == L else t

T = 'T'
def arr(*ts):
    t = ts[-1]
    for a in reversed(ts[:-1]): t = ('→', a, t)
    return t
names = (f'v{i}' for i in itertools.count())

def gen(ty, ctx, d, rng):
    if ty != T:
        x = next(names)
        return ('λ', x, gen(ty[2], ctx + [(x, ty[1])], d, rng))
    opts = ['leaf'] + [x for x, t in ctx if t == T]
    if d > 0:
        opts += ['stem', 'fork', 'tri', 'tri'] + ['call:' + x for x, t in ctx if t != T] * 2
    o = rng.choice(opts)
    if o == 'leaf': return L
    if o == 'stem': return A(L, gen(T, ctx, d - 1, rng))
    if o == 'fork': return A(L, gen(T, ctx, d - 1, rng), gen(T, ctx, d - 1, rng))
    if o == 'tri':
        return tri(gen(T, ctx, d - 1, rng), gen(arr(T, T), ctx, d - 1, rng),
                   gen(arr(T, T, T), ctx, d - 1, rng), gen(T, ctx, d - 1, rng))
    if o.startswith('call:'):
        f = o[5:]; t = dict(ctx)[f]; args = []
        while t != T: args.append(gen(t[1], ctx, d - 1, rng)); t = t[2]
        return A(f, *args)
    return o

def rtree(n, rng):
    if n <= 1: return L
    if rng.random() < 0.3: return A(L, rtree(n - 1, rng))
    m = rng.randint(1, max(1, n - 2))
    return A(L, rtree(m, rng), rtree(max(1, n - 1 - m), rng))

def _main():
    rng = random.Random(11)
    progtypes = [arr(T, T, T), arr(arr(T, T), T, T), arr(arr(T, T, T), T, T, T), arr(T, T)]
    ok = steps_tc = steps_ltc = 0
    for trial in range(3000):
        pt = rng.choice(progtypes)
        prog = gen(pt, [], rng.randint(2, 5), rng)
        args, t = [], pt
        while t != T:
            args.append(rtree(rng.randint(1, 9), rng) if t[1] == T else gen(t[1], [], 3, rng)); t = t[2]
        ref = nf(A(prog, *args))
        for name, br, rules in (('tc', br_tc, TC), ('ltc', br_ltc, LTC)):
            c = A(compile_(prog, br), *[compile_(a, br) for a in args])
            got, n = normalize(c, rules, 200000)
            assert got == ref, (name, prog, args, show(ref), got and show(got))
            if name == 'tc': steps_tc += n
            else: steps_ltc += n
        ok += 1
    print(f'{ok} random programs: λ evaluator, TC and LTC agree on every answer; '
          f'total steps TC {steps_tc}, LTC {steps_ltc} (normal order, no sharing)')

if __name__ == '__main__':
    _main()
