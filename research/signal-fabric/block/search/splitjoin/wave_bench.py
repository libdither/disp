"""What a same-tick-only join does: SPLITJOIN=wave python wave_bench.py. Prints how each run ended and when."""
import sys; sys.path[:0] = ['..', '.']
import calculus as C, physics as P, laws, design as D
rule, reader = P.Rule(D.REACTIONS, [0] * len(D.RANGES)), laws.Reader(D)
def spine(k):
    s = C.L
    for _ in range(k): s = C.F(C.L, s)
    return s
cases = [('full %d' % k, C.full(k)) for k in range(1, int(sys.argv[1]) + 1)] + [('F(F(L,L),L)', C.F(C.F(C.L, C.L), C.L)), ('F(L,F(L,L))', C.F(C.L, C.F(C.L, C.L))), ('spine 3', spine(3))]
limit = int(sys.argv[2]) if sys.argv[2:] else 200000
print('reactions', len(D.REACTIONS), 'wave part', len(D.REACTIONS) - D.BASE)
for name, x in cases:
    r = laws.run(D, C.deep_swap(), x, rule, reader, limit, watch=False)
    want = C.run(C.deep_swap(), x)
    print('%-12s ended=%s t=%d right=%s outs=%d' % (name, r.still, r.t, r.x_end == want, len(r.outs)))
