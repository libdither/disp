"""Smoke test of the harness: runs only id. A head goes down the link, bounces, and leaves."""
from physics import I, A
from laws import Lay, Link
NAME, RANGES = 'smoke', [8, 2]
V, H, C, LF, ST, FK, OUT = 1, 2, 3, 4, 5, 6, 7
E, S, W, N = (1, 0), (0, 1), (-1, 0), (0, -1)
Z = (0, 0)
def c(k, t=0): return (I(k), I(t))
void = (I(0), I(0))
REACTIONS = [
    ((void, c(V, 1), void, c(V)), (void, c(V), void, c(V, 1))),   # right column of an even block
    ((c(V), void, c(V, 1), void), (c(V, 1), void, c(V), void)),   # left column
    ((void, void, c(V, 1), void), (c(OUT), void, c(V), void)),    # leaves upward
]
def lay(t):
    cells = {}
    def go(t, x, y):
        if t == 'L': cells[(x, y)] = (LF, 0); return 1
        if t[0] == 'S':
            cells[(x, y)] = (ST, 0); return go(t[1], x, y + 1)
        cells[(x, y)] = (FK, 0)
        w = go(t[1], x, y + 1)
        for i in range(1, w): cells[(x + i, y)] = (H, 0)
        cells[(x + w, y)] = (C, 0)
        return w + go(t[2], x + w, y + 1)
    go(t, 0, 0)
    return Lay(cells, (0, 0), S)
def link(dp, dx):
    return Link((0, 2), (2 + dp.w + dp.w % 2, 2), {(1, 0): (V, 1), (1, 1): (V, 0)}, 1)
def node(cell, step):
    k = cell[0]
    if k == V: return ('pass', step) if step in (S, N) else None
    if k == H: return ('pass', step) if step in (E, W) else None
    if k == C: return ('pass', S) if step == E else None
    if k == LF: return ('leaf',)
    if k == ST: return ('stem', S)
    if k == FK: return ('fork', S, E)
    return None
