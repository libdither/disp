"""Speed cases for programs that touch both parts: python speed.py"""
import sys, time; sys.path[:0] = ['..', '.']
import calculus as C, physics as P, laws, design as D
rule, reader = P.Rule(D.REACTIONS, [0] * len(D.RANGES)), laws.Reader(D)
def t(p, x, limit=3000000):
    r = laws.run(D, p, x, rule, reader, limit, watch=False)
    ok = r.still and r.x_end == C.run(p, x) and r.p_end == p and r.sealed
    return r.t if ok else ('x t=%d' % r.t)
def frontier(k): return C.SWAP if k <= 1 else C.map_(C.L, frontier(k - 1), frontier(k - 1))
def every(k): return C.SWAP if k <= 1 else C.seq(C.map_(C.L, every(k - 1), every(k - 1)), C.SWAP)
print('reactions', len(D.REACTIONS), 'of which the deep-swap sweep or wave', len(D.REACTIONS) - D.BASE)
K = range(1, int(sys.argv[1]) + 1 if sys.argv[1:] else 6)
print('k                     ', list(K))
print('leaves                ', [2 ** k for k in K])
print('frontier(k) on full k ', [t(frontier(k), C.full(k)) for k in K])
print('every(k) on full k    ', [t(every(k), C.full(k)) for k in K])
print('deep_swap on full k   ', [t(C.deep_swap(), C.full(k)) for k in K])
