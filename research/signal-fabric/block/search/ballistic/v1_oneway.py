"""Ballistic: matter is nodes and mirrors on (even, even) sites, a connection is a sight line through empties.
One head: a ball flying along lines, carrying (mode h, payload v, direction d). Forks and stems hold a route field."""
from physics import I, A
from laws import Lay, Link

NAME = 'ballistic'
E, S, W, N = (1, 0), (0, 1), (-1, 0), (0, -1)
VOID, HB, VB, NODE, MIR = 0, 1, 2, 3, 4
LF, ST, FK = 0, 1, 2
ELBOW, HUB, DHUB = 0, 1, 2
SWAP, Q, NEXT, POP, RET, RS, RF = 2, 7, 8, 9, 10, 11, 12

_h = {}
def h(name):
    return _h.setdefault(name, len(_h) + 1)
def lit(x):
    return I(x) if isinstance(x, int) else A(x)
def cell(cls, k=0, f=0, r=0, ro=0, st=0, v=0, d=0):
    return tuple(lit(x) for x in (cls, k, f, r, ro, st, v, d))
O = cell(VOID)
def ball(cls, m, v='v', d='d'):
    return cell(cls, 0, 0, 0, 0, h(m), v, d)
def nd(k='k', f='f', r='r', ro='ro', st=0, v=0, d=0):
    return cell(NODE, k, f, r, ro, h(st) if isinstance(st, str) else st, v, d)
def mir(k, st=0, v=0, d=0):
    return cell(MIR, k, 0, 0, 0, st if not isinstance(st, str) or st == 'h' else h(st), v, d)

R = []
def rx(a, b):
    R.append((a, b))
def A_(nw=O, ne=O, sw=O):
    return (nw, ne, sw, O)
def B_(ne=O, se=O, sw=O):
    return (O, ne, sw, se)

# flight: heading is phase
for c in (HB, VB):
    m = cell(c, 0, 0, 0, 0, 'h', 'v', 'd')
rx((cell(HB, 0, 0, 0, 0, 'h', 'v', 'd'), O, O, O), (O, cell(HB, 0, 0, 0, 0, 'h', 'v', 'd'), O, O))
rx((O, O, cell(HB, 0, 0, 0, 0, 'h', 'v', 'd'), O), (O, O, O, cell(HB, 0, 0, 0, 0, 'h', 'v', 'd')))
rx((cell(VB, 0, 0, 0, 0, 'h', 'v', 'd'), O, O, O), (O, O, cell(VB, 0, 0, 0, 0, 'h', 'v', 'd'), O))
rx((O, cell(VB, 0, 0, 0, 0, 'h', 'v', 'd'), O, O), (O, O, O, cell(VB, 0, 0, 0, 0, 'h', 'v', 'd')))

# the elbow: E->S, S->E... any mode, held one tick since its two arms are in different blocks
H_ANY = cell(HB, 0, 0, 0, 0, 'h', 'v', 'd')
V_ANY = cell(VB, 0, 0, 0, 0, 'h', 'v', 'd')
HELD = cell(MIR, ELBOW, 1, 0, 0, 'h', 'v', 'd')
EL = cell(MIR, ELBOW)
rx(B_(sw=H_ANY, se=EL), B_(se=HELD))
rx(A_(nw=HELD), A_(nw=EL, sw=V_ANY))

# the hub above the program: the head starts in it, U turns east, D coming back turns south, R leaves
HU = cell(MIR, HUB)
rx(A_(nw=mir(HUB, 'start', 0, 1)), A_(nw=HU, sw=ball(VB, 'X', 0, 1)))
rx(A_(nw=HU, sw=ball(VB, 'U')), A_(nw=HU, ne=ball(HB, 'U')))
rx(A_(nw=HU, sw=ball(VB, 'D')), A_(nw=HU, ne=ball(HB, 'D')))
rx(A_(nw=HU, sw=ball(VB, 'R', 0, 1)), A_(nw=mir(HUB, 'end', 0, 1)))
rx(B_(se=mir(HUB, 'end', 0, 1)), B_(ne=ball(VB, 'OUT', 0, 1), se=HU))

# the hub above the data: what arrives as U goes on as D
DH = cell(MIR, DHUB)
rx(B_(sw=ball(HB, 'U'), se=DH), B_(se=mir(DHUB, 'in', 'v', 'd')))
rx(A_(nw=mir(DHUB, 'in', 'v', 'd')), A_(nw=DH, sw=ball(VB, 'D')))
rx(A_(nw=DH, sw=ball(VB, 'U')), A_(nw=mir(DHUB, 'back', 'v', 'd')))
rx(B_(se=mir(DHUB, 'back', 'v', 'd')), B_(sw=ball(HB, 'D'), se=DH))

# helpers: a node is nw of its A block (S and E arms), se of its B block (N arm)
def down_S(m, st, m2, v='v', d='d', **kw):
    rx(A_(nw=nd(st=st, v=kw.pop('v0', v), d=kw.pop('d0', d), **kw)), None)
def absorb_N(m, st, v='v', d='d', **kw):
    rx(B_(ne=ball(VB, m, v, d), se=nd(**kw)), B_(se=nd(st=st, v=v, d=d, **kw)))
def emit(st, m, arm, kw0, kw1=None, v0='v', d0='d', v1='v', d1='d'):
    b = ball(VB if arm == 'S' else HB, m, v1, d1)
    rx(A_(nw=nd(st=st, v=v0, d=d0, **kw0)), A_(nw=nd(**(kw1 or kw0)), ne=b if arm == 'E' else O, sw=b if arm == 'S' else O))
def up(st, m, v='v', d='d'):
    rx(B_(se=nd(st=st, v=v, d=d)), B_(ne=ball(VB, m, v, d), se=nd()))
def bounce(m, m2, kw0, kw1=None, v0='v', d0='d', v1='v', d1='d'):
    rx(B_(ne=ball(VB, m, v0, d0), se=nd(**kw0)), B_(ne=ball(VB, m2, v1, d1), se=nd(**(kw1 or kw0))))
def hop(m, arm, m2, arm2, kw0, kw1=None, v0='v', d0='d', v1='v', d1='d'):
    """the ball arrives at a fork on one arm and leaves on an arm with a new mode, the fork updated, in one block"""
    def side(a, mm, vv, dd, kw):
        b = ball(VB if a == 'S' else HB, mm, vv, dd)
        return A_(nw=nd(**kw), ne=b if a == 'E' else O, sw=b if a == 'S' else O)
    rx(side(arm, m, v0, d0, kw0), side(arm2, m2, v1, d1, kw1 or kw0))
def to_up(m, arm, st, kw0, kw1, v0='v', d0='d', v1='v', d1='d'):
    b = ball(VB if arm == 'S' else HB, m, v0, d0)
    rx(A_(nw=nd(**kw0), ne=b if arm == 'E' else O, sw=b if arm == 'S' else O), A_(nw=nd(st=st, v=v1, d=d1, **kw1)))

# route transit
absorb_N('D', 'dt', r=1)
absorb_N('D', 'dt', r=2)
emit('dt', 'D', 'S', dict(r=1))
emit('dt', 'D', 'E', dict(r=2))
to_up('U', 'S', 'ut', dict(), dict())
to_up('U', 'E', 'ut', dict(), dict())
up('ut', 'U')
up('rt', 'R')
up('ut2', 'U')

# exec
Xr = dict(r=0, ro=0)
bounce('X', 'R', dict(k=LF, r=0, ro=0))
absorb_N('X', 'x', k=ST, r=0, ro=0)
emit('x', 'C', 'S', dict(k=ST, r=0, ro=0), v0=0, v1=1)
absorb_N('X', 'x', k=FK, r=0, ro=0)
emit('x', 'PK', 'S', dict(k=FK, r=0, ro=0), v0=0, v1=0)
# counting stems
absorb_N('C', 'c', k=ST, r=0, ro=0)
for n in range(1, 6):
    emit('c', 'C', 'S', dict(k=ST, r=0, ro=0), v0=n, v1=n + 1)
bounce('C', 'U', dict(k=LF, r=0, ro=0))
absorb_N('C', 'c', k=FK, r=0, ro=0)
emit('c', 'PK', 'S', dict(k=FK, r=0, ro=0), v0=1, v1=1)
# peek
for k, m in ((LF, 'PKL'), (ST, 'PKS'), (FK, 'PKF')):
    bounce('PK', m, dict(k=k))

# program forks: seq and dag, decided by the left child's kind and which arm just finished
hop('R', 'S', 'PK', 'S', dict(k=FK, ro=0, r=1), v0=0, v1=0)
hop('R', 'E', 'PK', 'S', dict(k=FK, ro=0, r=2), v0=0, v1=0)
for d, order in ((1, (1, 2)), (0, (2, 1))):
    a, b = order
    for fro, to in ((0, a), (a, b)):
        m, arm = ('XS', 'S') if to == 1 else ('X', 'E')
        hop('PKS', 'S', m, arm, dict(k=FK, ro=0, r=fro), dict(k=FK, ro=0, r=to), v0=0, d0=d, v1=0, d1=d)
    to_up('PKS', 'S', 'rt', dict(k=FK, ro=0, r=b), dict(k=FK, ro=0, r=0), v0=0, d0=d, v1=0, d1=d)
    hop('PKL', 'S', 'X', 'E', dict(k=FK, ro=0, r=0), dict(k=FK, ro=0, r=2), v0=0, d0=d, v1=0, d1=1 - d)
    to_up('PKL', 'S', 'rt', dict(k=FK, ro=0, r=2), dict(k=FK, ro=0, r=0), v0=0, d0=d, v1=0, d1=1 - d)
# a seq's stem: passed through, not counted
rx(B_(ne=ball(VB, 'XS'), se=nd(k=ST, r=0, ro=0)), B_(se=nd(k=ST, r=1, ro=0, st='xs', v='v', d='d')))
emit('xs', 'X', 'S', dict(k=ST, r=1, ro=0))
to_up('R', 'S', 'rt', dict(k=ST, r=1, ro=0), dict(k=ST, r=0, ro=0))

# map: pc is the stem; c its fork; l c's left fork. pc asks the data its kind.
to_up('PKF', 'S', 'mq', dict(k=FK, r=0, ro=0), dict(k=FK, r=0, ro=0), v0=1, v1=0)
up('mq', 'MQ')
to_up('MQ', 'S', 'ut2', dict(k=ST, r=0, ro=0), dict(k=ST, r=0, ro=0), v0=0, v1=Q)
# the data's answers, at the focus (route 0): reflect, in one block
for k, kw1, a in ((LF, dict(k=LF, r=0), RET), (ST, dict(k=ST, r=1), RS)):
    bounce('D', 'U', dict(k=k, r=0), kw1, v0=Q, v1=a)
for f, r in ((0, 1), (1, 2)):
    bounce('D', 'U', dict(k=FK, f=f, r=0), dict(k=FK, f=f, r=r), v0=Q, v1=RF)
# swap at the focus
for k in (LF, ST):
    bounce('D', 'U', dict(k=k, r=0), v0=SWAP, v1=RET)
bounce('D', 'U', dict(k=FK, f=0, r=0), dict(k=FK, f=1, r=0), v0=SWAP, v1=RET)
bounce('D', 'U', dict(k=FK, f=1, r=0), dict(k=FK, f=0, r=0), v0=SWAP, v1=RET)
# pop and next: the end of the data route bounces, its parent acts
bounce('D', 'PU', dict(r=0), v0=POP, v1=0)
bounce('D', 'PN', dict(r=0), v0=NEXT, v1=0)
to_up('PU', 'S', 'ut2', dict(r=1), dict(r=0), v0=0, v1=RET)
to_up('PU', 'E', 'ut2', dict(r=2), dict(r=0), v0=0, v1=RET)
to_up('PN', 'S', 'ut2', dict(k=FK, f=0, r=1), dict(k=FK, f=0, r=2), v0=0, v1=RET)
to_up('PN', 'E', 'ut2', dict(k=FK, f=1, r=2), dict(k=FK, f=1, r=1), v0=0, v1=RET)
# back in the program: a stem at the route's end
bounce('D', 'R', dict(k=ST, r=0, ro=0), v0=RET, v1=0)
for a, g in ((RS, 1), (RF, 2)):
    absorb_N('D', 'dt', v=a, k=ST, r=0, ro=0)
    emit('dt', 'G', 'S', dict(k=ST, r=0, ro=0), dict(k=ST, r=1, ro=0), v0=a, v1=g)
absorb_N('G', 'g', k=FK, r=0, ro=0)
emit('g', 'G2', 'S', dict(k=FK, r=0, ro=0), dict(k=FK, r=1, ro=2))
absorb_N('G2', 'g2', k=FK, r=0, ro=0)
emit('g2', 'X', 'S', dict(k=FK, r=0, ro=0), dict(k=FK, r=1, ro=1), v0=1, v1=0)
emit('g2', 'X', 'E', dict(k=FK, r=0, ro=0), dict(k=FK, r=2, ro=1), v0=2, v1=0)
# returns: l says which finished; c runs its right part or finishes; pc pops
to_up('R', 'S', 'rd', dict(k=FK, r=1, ro=1), dict(k=FK, r=0, ro=0))
to_up('R', 'E', 'rn', dict(k=FK, r=2, ro=1), dict(k=FK, r=0, ro=0))
up('rd', 'RD')
up('rn', 'RN')
to_up('RD', 'S', 'rd', dict(k=FK, r=1, ro=2), dict(k=FK, r=0, ro=0))
to_up('RN', 'S', 'ut2', dict(k=FK, r=1, ro=2), dict(k=FK, r=0, ro=3), v0=0, v1=NEXT)
absorb_N('D', 'dt', v=RET, k=FK, r=0, ro=3)
emit('dt', 'X', 'E', dict(k=FK, r=0, ro=3), dict(k=FK, r=2, ro=2), v0=RET, v1=0)
to_up('R', 'E', 'rd', dict(k=FK, r=2, ro=2), dict(k=FK, r=0, ro=0))
to_up('RD', 'S', 'ut2', dict(k=ST, r=1, ro=0), dict(k=ST, r=0, ro=0), v0=0, v1=POP)

REACTIONS = R
RANGES = [5, 3, 2, 3, 4, len(_h) + 1, 13, 2]
SHAPES = '?'

# layout: left part straight down, right part east then down
COL = 4
def width(t):
    if t == 'L': return COL
    if t[0] == 'S': return width(t[1])
    return width(t[1]) + width(t[2])
def _c(*a): return tuple(a) + (0,) * (8 - len(a))
def lay(t):
    cells = {}
    def go(t, x, y):
        if t == 'L': cells[(x, y)] = _c(NODE, LF); return
        if t[0] == 'S':
            cells[(x, y)] = _c(NODE, ST); go(t[1], x, y + 2); return
        cells[(x, y)] = _c(NODE, FK)
        w = width(t[1])
        cells[(x + w, y)] = _c(MIR, ELBOW)
        go(t[1], x, y + 2)
        go(t[2], x + w, y + 2)
    go(t, 0, 0)
    return Lay(cells, (0, 0), S)
def link(dp, dx):
    xd = dp.w + dp.w % 2 + COL
    return Link((0, 2), (xd, 2), {(0, 0): _c(MIR, HUB, 0, 0, 0, h('start'), 0, 1), (xd, 0): _c(MIR, DHUB)}, 0)
def node(cell, step):
    c = cell[0]
    if c in (VOID, HB, VB): return ('pass', step)
    if c == MIR:
        if cell[1] == ELBOW: return ('pass', S) if step == E else None
        return None
    k = cell[1]
    if k == LF: return ('leaf',)
    if k == ST: return ('stem', S)
    return ('fork', S, E) if cell[2] == 0 else ('fork', E, S)
