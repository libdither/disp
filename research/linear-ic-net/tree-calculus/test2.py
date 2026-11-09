"""3000 random typed programs: λ reference, TC, LTC2 and LTC2P must all agree."""
import random
from rules import *
from compile import compile_, br_tc
from ltc2 import LTC2, LTC2P, compile2
from lower import lower
from test_compile import gen, rtree, nf, arr, T
rng = random.Random(23)
progtypes = [arr(T, T, T), arr(arr(T, T), T, T), arr(arr(T, T, T), T, T, T), arr(T, T)]
steps = {'tc': 0, 'ltc2': 0, 'ltc2p': 0}; n_ok = 0
for _ in range(3000):
    pt = rng.choice(progtypes)
    prog = gen(pt, [], rng.randint(2, 5), rng)
    args, t = [], pt
    while t != T:
        args.append(rtree(rng.randint(1, 9), rng) if t[1] == T else gen(t[1], [], 3, rng)); t = t[2]
    ref = nf(A(prog, *args))
    for name, comp, rules in (('tc', lambda s: compile_(s, br_tc), TC),
                              ('ltc2', lambda s: compile2(lower(s)), LTC2),
                              ('ltc2p', lambda s: compile2(lower(s, True)), LTC2P)):
        got, n = normalize(A(comp(prog), *[comp(a) for a in args]), rules, 400000)
        assert got == ref, (name, prog, args)
        steps[name] += n
    n_ok += 1
print(f'{n_ok} random programs: λ reference, TC, LTC2 and LTC2P agree on every answer; steps {steps}')
