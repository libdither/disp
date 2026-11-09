"""7a / 7b against V5 (9 rules) and tree calculus: eager recursive programs, same random typed programs,
code size; and the per-step size law with the stem rule eating two nodes."""
import random, threading
from collections import Counter
from rules import L, A, size, normalize, TC, redexes, get, put, spine
from ltc2 import fire2
from ltc7 import VARIANTS, make_compiler, wait_of, builders, compiler_7c
from ltc7_check import lower7
from family import DESIGNS, rules_of, make_compiler as mk5, wait_of as w5
from family_test import eager, lower_k
from bench import programs, nat, tree, lst
from test_compile import gen, rtree, nf, arr, T
from compile import compile_, br_tc
from check_dynamic import rterm
import compile as compmod
V5 = 'V5 I K C | W B B2'
def setups():
    R5 = rules_of(DESIGNS[V5]); c5 = mk5(DESIGNS[V5]); K5 = lambda q: A(L, L, A(L, q))
    yield 'V5 (9 rules)', R5, c5, (lambda t: lower_k(t, K5)), w5(V5)
    for v in ('7a',):
        K = builders(v)[1]
        yield v + ' (7 rules)', VARIANTS[v], make_compiler(v), (lambda t, K=K: lower7(t, K)), wait_of(v)
    c7, K7, w7 = compiler_7c()
    yield '7c (7 rules)', VARIANTS['7c'], c7, (lambda t: lower7(t, K7)), w7
def main():
    rng = random.Random(77); progs_r = []
    for _ in range(1500):
        pt = rng.choice([arr(T, T, T), arr(arr(T, T), T, T), arr(arr(T, T, T), T, T, T), arr(T, T)])
        prog = gen(pt, [], rng.randint(2, 5), rng); args = []; t = pt
        while t != T:
            args.append(rtree(rng.randint(1, 9), rng) if t[1] == T else gen(t[1], [], 3, rng)); t = t[2]
        progs_r.append((prog, args, nf(A(prog, *args))))
    answers = None
    for name, R, comp, low, wait in setups():
        compmod.WAIT['fam'] = wait
        ps = [comp(low(p)) for p in programs('fam')]
        mirror, add, leaves, rev, fib = ps; addf = A(add, add)
        tot = Counter(); outs = []
        for call in (A(mirror, mirror, tree(7)), A(addf, nat(30), nat(25)), A(leaves, leaves, addf, tree(7)),
                     A(rev, rev, lst(25), L), A(fib, fib, addf, nat(11))):
            v, st = eager(R)(call); tot += st; outs.append(v)
        answers = answers or outs; assert outs == answers, name
        nsteps = 0
        for prog, args, ref in progs_r:
            got, n = normalize(A(comp(low(prog)), *[comp(low(a)) for a in args]), R, 400000)
            assert got == ref; nsteps += n
        drops = tot['K'] + tot['T0']
        print(f"{name:13} eager: {tot['steps']:6} rewrites, {drops:5} drop steps, {tot['W']:4} W, {tot['copied']:7} copied, "
              f"{tot['dropped']:7} dropped, code {sum(size(p) for p in ps):4} | random programs: {nsteps} steps")
    # size law for 7a: every rule eats 3 nodes except the stem rule, which eats 2
    R = VARIANTS['7a']; rng = random.Random(3); n = 0
    for _ in range(1500):
        t = rterm(rng)
        for _ in range(300):
            ps = redexes(t); rng.shuffle(ps); fired = False
            for p in ps:
                h, args = spine(get(t, p)); r = fire2(args, R)
                if not r or r[0] != 'done': continue
                _, new, nm, env = r; t2 = put(t, p, new)
                drop = size(env['d']) + size(env['b']) if nm == 'T0' else 0
                copy = size(env['x']) if nm == 'W' else 0
                assert size(t2) == size(t) - (2 if nm == 'B' else 3) - drop + copy
                t = t2; n += 1; fired = True; break
            if not fired: break
    print(f'7a size law held on {n} random steps: each step eats 3 nodes (the stem rule B eats 2), minus drops, plus copies')
threading.stack_size(512 * 1024 * 1024); th = threading.Thread(target=main); th.start(); th.join()
