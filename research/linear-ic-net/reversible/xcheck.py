"""Cross-check ltcfast against the prototype's eager evaluators on the prototype's own programs."""
import threading, random
import ltcfast as F
from rules import L, A, size
from family_test import eager
from radd import R, ADD, add_call, naive_call, radd_call, radd_expect, nat
import radd2, rmirror
import bench
from compile import compile_, br_tc
import compile as compmod

def same(t):
    v1, s1 = eager(R)(t)
    v2, s2 = F.run(t)
    c2 = s2.counter()
    for k in ('steps', 'K', 'W', 'B', 'B2', 'C', 'I', 'T0', 'T1', 'T2', 'copied', 'dropped', 'copies'):
        assert s1[k] == c2[k], (k, s1[k], c2[k])
    assert F.term(v2) == v1
    return s2

def main():
    n = 0
    for m, k in ((3, 2), (10, 5), (30, 25)):
        for t in (add_call(m, k), naive_call(m, k), radd_call(m, k)): same(t); n += 1
        C, D, pack, call = radd2.build(('a', 'n', 'dh')); same(call(m, k)); n += 1
    for d in (3, 5):
        t = rmirror.tree(d); same(A(rmirror.PACK, t, rmirror.C, rmirror.D, rmirror.B, rmirror.K0)); n += 1
    # tree calculus
    progs = [compile_(p, br_tc) for p in bench.programs('tc')]
    mirror, add, leaves, rev, fib = progs; addf = A(add, add)
    for call in (A(mirror, mirror, bench.tree(6)), A(addf, bench.nat(12), bench.nat(9)), A(rev, rev, bench.lst(12), L),
                 A(leaves, leaves, addf, bench.tree(5)), A(fib, fib, addf, bench.nat(8))):
        v1, s1 = bench.run(call, 'tc')
        v2, s2 = F.run(call, 'tc'); c2 = s2.counter()
        assert F.term(v2) == v1
        for k in ('K', 'S', 'copied', 'dropped', 'copies'): assert s1[k] == c2[k], (k, s1[k], c2[k])
        assert s1['T'] == c2['T0'] + c2['T1'] + c2['T2']
        n += 1
    print(f'{n} runs: ltcfast matches the prototype evaluators exactly (value and every counter)')
threading.stack_size(512 * 1024 * 1024); th = threading.Thread(target=main); th.start(); th.join()
