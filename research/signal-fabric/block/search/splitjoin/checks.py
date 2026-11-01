"""Three checks behind the report: python checks.py; SPLITJOIN=wave python checks.py"""
import os, sys; sys.path[:0] = ['..', '.']
import calculus as C, physics as P, laws, design as D
rule, reader = P.Rule(D.REACTIONS, [0] * len(D.RANGES)), laws.Reader(D)
mode = os.environ.get('SPLITJOIN', 'sweep'); lay = os.environ.get('SPLITJOIN_LAY', 'free')
print('mode', mode, 'lay', lay)
def period(x, limit=400000):
    """ticks until the whole grid is back at its start, and how often the data's reading changed on the way"""
    cells, rp, ep, rx, ex, port = laws.start(D, C.deep_swap(), x)
    w = P.World(cells, rule); first = dict(w.cells)
    flips = 0; now = reader.read(w.cells, rx, ex); t0 = None
    while w.t < limit:
        w.step()
        n = reader.read(w.cells, rx, ex)
        if n != now: flips += 1; now = n
        if w.phase == 0 and w.cells == first and not w.outs: return w.t, flips
        if w.outs: return 'left at %d' % w.t, flips
    return None, flips
for name, x in (('full 1', C.full(1)), ('full 2', C.full(2)), ('full 3', C.full(3)), ('F(L,F(L,L))', C.F(C.L, C.F(C.L, C.L))), ('S then fork', C.S(C.F(C.L, C.L)))):
    print(' %-12s back at start after, flips of the reading: %s' % (name, period(x)))
bad = tot = 0
for x in C.trees(6) + [C.full(3)]:
    r = laws.run(D, C.deep_swap(), x, rule, reader, 40000)
    ok = r.still and r.x_end == C.run(C.deep_swap(), x) and r.moves and r.reads and r.p_end == C.deep_swap() and len(r.outs) == 1
    tot += 1; bad += not ok
print(' deep_swap on every tree up to 6 nodes, full 3: %d of %d run right with the watch on' % (tot - bad, tot))
