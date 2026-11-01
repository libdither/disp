"""Arc diagram on sites: one head on a route; the strand is the rail; arcs are sight lines between elbows."""
import sys, os
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from laws import Lay, Link
from kit import *
import wires, control, grow
if not os.environ.get('ARCS_NOMOVES'): import moves
NAME = 'arcs'
SHAPES = 'see TAGS: each reaction is its own shape, sites fix the placement'
E, S, W, N = (1, 0), (0, 1), (-1, 0), (0, -1)
Z = (0, 0)
REACTIONS = R.rs
RANGES = [RANGE[n] for n in FIELDS]
RANGE['m'] = max(RANGE['m'], nmodes())
RANGES = [RANGE[n] for n in FIELDS]
TAGS = R.tags

def C(**kw):
    return tuple(kw.get(n, 0) for n in FIELDS)
def depth(t):
    if t == 'L': return 0
    if t[0] == 'S': return depth(t[1])
    return max(depth(t[1]) + 1, depth(t[2]))
def lay(t):
    cells = {}
    def go(t, y):
        """rows of t from y; returns its last row"""
        if t == 'L':
            cells[(0, y)] = C(cls=NODE, k=LF)
            return y
        if t[0] == 'S':
            cells[(0, y)] = C(cls=NODE, k=ST)
            return go(t[1], y + 2)
        x = 2 * (depth(t[1]) + 1)
        cells[(0, y)] = C(cls=NODE, k=FK, c=1, h=1)
        cells[(x, y)] = C(cls=EL, w=1)
        m = go(t[1], y + 2)
        for yy in range(y + 2, m + 2, 2): cells[(x, yy)] = C(w=1)
        cells[(0, m + 2)] = C(cls=NODE, k=GAP, c=1, h=1)
        cells[(x, m + 2)] = C(cls=EL, w=0)
        return go(t[2], m + 4)
    cells[(0, go(t, 0) + 2)] = C(cls=NODE, k=GAP)
    return Lay(cells, (0, 0), S)
def link(dp, dx):
    xd = dp.w + dp.w % 2 + 2
    return Link((0, 2), (xd, 2), {(0, 0): C(cls=HUB, m=M('start'), d=1), (xd, 0): C(cls=HUB, k=1)}, 0)
FI = {n: i for i, n in enumerate(FIELDS)}
CV = M('CV'); CL = M('CL')
def node(cell, step):
    g = lambda n: cell[FI[n]]
    cl = g('cls')
    if cl == HUB: return None
    if cl == EL:
        if g('t') == 2 and g('m') == 0:
            return ('pass', S) if step in (E, S) else (('pass', E) if step == N else None)
        if g('w') == 1: return ('pass', S) if step in (E, S) else (('pass', W) if step == N else None)
        return ('pass', W) if step == S else (('pass', N) if step == E else None)
    if cl in (VOID, HB, VB, SB): return ('pass', step)
    if cl == PB:
        if g('m') == CL and step == S: return ('pass', N)
        return _kind(g('pk'), g('pf'), 0, step)
    k, f, h, a = g('k'), g('f'), g('h'), g('a')
    if g('m') == CV and step == S:
        return _kind(g('pk'), g('pf'), 0, S, Z)
    if step == N and a: return ('pass', E)
    if step == W and (a or (g('c') and h)): return ('pass', S)
    return _kind(k, f, h, step)
def _kind(k, f, h, step, on=S):
    if step not in (S, Z): return ('pass', step)
    if k == GAP: return ('pass', on)
    if k == BY: return ('pass', E)
    if k == LF: return ('leaf',)
    if k == ST: return ('stem', on)
    arm = E if h else N
    return ('fork', arm, on) if f else ('fork', on, arm)
