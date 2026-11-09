"""Kraft sums (exhaustive rule sets sum to 1, so their size is odd), and the reversible programs
(which never drop) run on V5, LTC8 and LTC7r."""
import threading
from fractions import Fraction
from collections import Counter
from rules import L, A, size, TC
from family import DESIGNS, rules_of, make_compiler as mk5, wait_of as w5
from family_test import eager, lower_k
from ltc_small import LTC8, LTC7R, compiler
from ltc7 import VARIANTS
from bench import programs, nat, tree, lst
import compile as compmod
def lam(*a):
    *xs, body = a
    for x in reversed(xs): body = ('λ', x, body)
    return body
def tri(c, d, b, x): return A(L, A(L, c, d), b, x)
def radd_src():
    D = lam('o1', 'o2', 'u', 'a', 'n', 'dh', A(tri('o1', 'dh', 'o2', 'u'), A(L, 'a'), A(L, 'n'), 'dh'))
    C = lam('o1', 'o2', 'a', 'n', 'dh', A(L, A(L, A(L, A(L, 'o2', 'o1'), 'o2'), 'dh'), A(L, 'a', 'n')))
    return C, D
def mirror_src():
    c = lam('o1', 'o2', 'cs', 'ds', 'bs', 'k', A('k', A(L, A(L, 'cs', 'o1'), 'o2'), 'cs', 'ds', 'bs', L))
    d = lam('o1', 'o2', 'u', 'cs', 'ds', 'bs', 'k',
            A(tri('o1', 'ds', 'o2', 'u'), 'cs', 'ds', 'bs', lam('P', 'c1', 'd1', 'b1', 'r', A('k', 'P', 'c1', 'd1', 'b1', A(L, 'r')))))
    b = lam('o1', 'o2', 'u', 'v', 'cs', 'ds', 'bs', 'k',
            A(tri('o1', 'o2', 'bs', 'u'), 'cs', 'ds', 'bs',
              lam('P', 'c1', 'd1', 'b1', 'ru', A('P', 'v', 'c1', 'd1', 'b1',
                  lam('P2', 'c2', 'd2', 'b2', 'rv', A('k', 'P2', 'c2', 'd2', 'b2', A(L, 'rv', 'ru')))))))
    k0 = lam('P', 'c1', 'd1', 'b1', 'r', A(L, A(L, 'P', A(L, 'c1', A(L, 'd1', 'b1'))), 'r'))
    return c, d, b, k0
def mir(t):
    if t == L: return L
    if t[0] == L: return A(L, mir(t[1]))
    return A(L, mir(t[1]), mir(t[0][1]))
def main():
    for name, R in (('tree calculus', TC), ('V5', rules_of(DESIGNS['V5 I K C | W B B2'])), ('7a', VARIANTS['7a']),
                    ('LTC8', LTC8), ('LTC7r', LTC7R)):
        s = sum(Fraction(1, 3 ** (size(l) - 1)) for _, l, _ in R)
        print(f'Kraft sum {name:13} {len(R)} rules: {s}' + ('  (covers every shape)' if s == 1 else '  (some shape has no rule)'))
    c5 = mk5(DESIGNS['V5 I K C | W B B2'])
    rows = []
    for name, R, comp in (('V5, 9 rules', rules_of(DESIGNS['V5 I K C | W B B2']), c5),
                          ('LTC8, 8 rules', LTC8, compiler('8')[0]), ('LTC7r, 7 rules', LTC7R, compiler('7r')[0])):
        Cs, Ds = radd_src(); C, D = comp(Cs), comp(Ds); pack = A(L, A(L, C, D), C)
        v, st = eager(R)(A(pack, nat(30), L, nat(25), D))
        assert v == A(L, A(L, pack, D), A(L, nat(30), nat(55))) and st['K'] == 0
        c, d, b, k0 = [comp(x) for x in mirror_src()]; P = A(L, A(L, c, d), b)
        v2, st2 = eager(R)(A(P, tree(7), c, d, b, k0))
        assert v2 == A(L, A(L, P, A(L, c, A(L, d, b))), mir(tree(7))) and st2['K'] == 0
        print(f"{name:15} reversible add 30 25: {st['steps']:5} steps, {st['W']:3} W, 0 drops | "
              f"reversible mirror depth 7: {st2['steps']:5} steps, {st2['W']:3} W, 0 drops")
    compmod.WAIT['fam'] = w5('V5 I K C | W B B2')
    K8 = compiler('8')[1]; c8 = compiler('8')[0]
    tot5, tot8 = Counter(), Counter()
    for R, comp, K, tot in ((rules_of(DESIGNS['V5 I K C | W B B2']), c5, lambda q: A(L, L, A(L, q)), tot5), (LTC8, c8, K8, tot8)):
        ps = [comp(lower_k(p, K)) for p in programs('fam')]
        mirror, add, leaves, rev, fib = ps; addf = A(add, add)
        for call in (A(mirror, mirror, tree(7)), A(addf, nat(30), nat(25)), A(leaves, leaves, addf, tree(7)),
                     A(rev, rev, lst(25), L), A(fib, fib, addf, nat(11))):
            v, st = eager(R)(call); tot += st
    print(f"ordinary programs (5, eager): V5 {tot5['steps']} rewrites, LTC8 {tot8['steps']} rewrites "
          f"({tot8['steps'] / tot5['steps']:.2f}x)")
threading.stack_size(512 * 1024 * 1024); th = threading.Thread(target=main); th.start(); th.join()
