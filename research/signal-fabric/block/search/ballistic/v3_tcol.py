"""Ballistic mirror machine. Matter: nodes and elbows on (even, even) sites; a link is a sight line through empties.
One ball (mode m, payload v, direction d, turned bit b, side s). Every walk is reversed by the same reactions: a ball that turns
retraces its path, undoing the walk, so a return needs no reactions of its own. b tells a fork a part has finished."""
from physics import I, A
from laws import Lay, Link

NAME = 'ballistic'
E, S, W, N = (1, 0), (0, 1), (-1, 0), (0, -1)
VOID, HB, VB, NODE, MIR, KB, PB = 0, 1, 2, 3, 4, 5, 6   # KB flies east-west and sees mirrors, PB does not fly
LF, ST, FK = 0, 1, 2
ELBOW, HUB, DHUB, T0, T1, AC, PS, M1 = 0, 1, 2, 3, 4, 5, 6, 7
SELF, SWAP, CROSS, SINK, STEM, UNIT = 1, 2, 3, 4, 5, 6
Q, RL, RS, RF, NEXT, NX = 7, 8, 9, 10, 11, 12

_m = {}
def m_(name):
    return _m.setdefault(name, len(_m) + 1)
def lit(x):
    return I(x) if isinstance(x, int) else A(x)
def cell(cls, k=0, f=0, r=0, ro=0, m=0, v=0, d=0, b=0, s=0):
    return tuple(lit(x) for x in (cls, k, f, r, ro, m_(m) if isinstance(m, str) and m[0] != '_' else (m if isinstance(m, int) else m[1:]), v, d, b, s))
O = cell(VOID)
def ball(arm, m, v='v', d='d', b='b', s='s'):
    return cell(HB if arm == 'E' else VB, m=m, v=v, d=d, b=b, s=s)
def nd(k='k', f='f', r='r', ro='ro', m=0, v=0, d=0, b=0, s=0):
    return cell(NODE, k, f, r, ro, m, v, d, b, s)
def held(k='k', f='f', r='r', ro='ro', m='_m', v='v', d='d', b='b', s='s'):
    return nd(k, f, r, ro, m, v, d, b, s)

R = []
def rx(a, b):
    R.append((a, b))
def A_(nw=O, ne=O, sw=O):
    return (nw, ne, sw, O)
def B_(ne=O, se=O, sw=O):
    return (O, ne, sw, se)
def arm(n, a=None, bl=None):
    """A block: node n with ball bl on arm a"""
    return A_(nw=n, ne=bl if a == 'E' else O, sw=bl if a == 'S' else O)
def st(m, v='v', d='d', b='b', s='s', **kw):
    return nd(m=m, v=v, d=d, b=b, s=s, **kw)

# flight: heading is phase
HANY, VANY = ball('E', '_m'), ball('S', '_m')
rx((HANY, O, O, O), (O, HANY, O, O))
rx((O, O, HANY, O), (O, O, O, HANY))
rx((VANY, O, O, O), (O, O, VANY, O))
rx((O, VANY, O, O), (O, O, O, VANY))

# the elbow: east to south and back, any ball, held a tick since its arms are in different blocks
EL = cell(MIR, ELBOW)
HEL = cell(MIR, ELBOW, 1, 0, 0, '_m', 'v', 'd', 'b', 's')
rx(B_(sw=HANY, se=EL), B_(se=HEL))
rx(A_(nw=HEL), A_(nw=EL, sw=VANY))

# the hub over the program: the head starts in it and leaves from it; a U turns east
HU = cell(MIR, HUB)
rx(A_(nw=cell(MIR, HUB, 0, 0, 0, 'start', 0, 1, 0, 0)), A_(nw=HU, sw=ball('S', 'C', 0, 1, 0, 0)))
rx(A_(nw=HU, sw=ball('S', 'U', s=0)), A_(nw=HU, ne=ball('E', 'U', s=0)))
rx(A_(nw=HU, sw=ball('S', 'C', 0, 1, 1, 0)), A_(nw=cell(MIR, HUB, 0, 0, 0, 'end', 0, 1, 1, 0)))
rx(B_(se=cell(MIR, HUB, 0, 0, 0, 'end', 0, 1, 1, 0)), B_(ne=ball('S', 'OUT', 0, 1, 1, 0), se=HU))
# the hub over the data: the ball changes side
DH = cell(MIR, DHUB)
HDH = cell(MIR, DHUB, 0, 0, 0, 'u', 'v', 'd', 'b', 0)
rx(B_(sw=ball('E', 'U', s=0), se=DH), B_(se=HDH))
rx(A_(nw=HDH), A_(nw=DH, sw=ball('S', 'U', s=1)))

# U: along the route, either way. In the data the route is r; in the program also a stem chain.
rx(arm(nd(r=1), 'S', ball('S', 'U')), arm(st('ut', r=1)))
rx(arm(nd(r=2), 'E', ball('E', 'U')), arm(st('ut', r=2)))
rx(arm(nd(k=ST, r=0, ro=0), 'S', ball('S', 'U', s=0)), arm(st('ut', k=ST, r=0, ro=0, s=0)))
rx(B_(ne=ball('S', 'U'), se=nd(r=1)), B_(se=st('ut', r=1)))
rx(B_(ne=ball('S', 'U'), se=nd(r=2)), B_(se=st('ut', r=2)))
rx(B_(ne=ball('S', 'U', s=0), se=nd(k=ST, r=0)), B_(se=st('ut', k=ST, r=0, s=0)))
rx(B_(ne=ball('S', 'U', s=0), se=nd(k=FK, r=0)), B_(se=st('ut', k=FK, r=0, s=0)))
# a leaf at the end of a count sends the count to the data
rx(B_(ne=ball('S', 'C', s=0), se=nd(k=LF, r=0)), B_(ne=ball('S', 'U', s=0), se=nd(k=LF, r=0)))

# C: the call. Stems count on the way down and uncount on the way back.
rx(B_(ne=ball('S', 'C'), se=nd(k=ST, r=0, ro=0)), B_(se=st('c', k=ST, r=0, ro=0)))
for n in range(6):
    rx(arm(st('c', v=n, k=ST, r=0, ro=0)), arm(nd(k=ST, r=0, ro=0), 'S', ball('S', 'C', v=n + 1)))
# a fork peeks at its left part
rx(B_(ne=ball('S', 'C'), se=nd(k=FK, r=0, ro=0)), B_(se=st('c', k=FK, r=0, ro=0)))
rx(arm(st('c', k=FK, r=0, ro=0)), arm(nd(k=FK, r=0, ro=0), 'S', ball('S', 'PK')))
for k, m in ((LF, 'PKL'), (ST, 'PKS'), (FK, 'PKF')):
    rx(B_(ne=ball('S', 'PK'), se=nd(k=k)), B_(ne=ball('S', m), se=nd(k=k)))

F0 = dict(k=FK, ro=0)
def hop(a, ba, kw0, a2, bb, kw1):
    rx(arm(nd(**kw0), a, ba), arm(nd(**kw1), a2, bb))
# seq: F(S(a), b), the stem is passed through; b says a part has finished
for d, (p, q) in ((1, (1, 2)), (0, (2, 1))):
    call = lambda t, bit: ('S', ball('S', 'XS', 0, d, bit)) if t == 1 else ('E', ball('E', 'C', 0, d, bit))
    a1, b1 = call(p, 0)
    a2, b2 = call(q, 0)
    r1, rb1 = call(p, 1)
    r2, rb2 = call(q, 1)
    hop('S', ball('S', 'PKS', 0, d, 0), dict(F0, r=0), a1, b1, dict(F0, r=p))
    hop(r1, rb1, dict(F0, r=p), a2, b2, dict(F0, r=q))
    hop(r2, rb2, dict(F0, r=q), 'S', ball('S', 'PKS', 0, d, 1), dict(F0, r=0))
rx(B_(ne=ball('S', 'XS', v=0), se=nd(k=ST, r=0, ro=0)), B_(se=st('xs', k=ST, r=0, ro=0, v=0)))
rx(arm(st('xs', k=ST, r=0, ro=0, v=0)), arm(nd(k=ST, r=0, ro=0), 'S', ball('S', 'C', v=0)))
# dag: F(L, b)
for d in (0, 1):
    hop('S', ball('S', 'PKL', 0, d, 0), dict(F0, r=0), 'E', ball('E', 'C', 0, 1 - d, 0), dict(k=FK, ro=1, r=2))
    hop('E', ball('E', 'C', 0, 1 - d, 1), dict(k=FK, ro=1, r=2), 'S', ball('S', 'PKL', 0, d, 1), dict(F0, r=0))

# map: S(F(F(a, b), c)). c is the fork under the stem; it asks the data, runs parts, then takes the question back.
def c_(ro, r=0): return dict(k=FK, ro=ro, r=r)
rx(arm(nd(**c_(0)), 'S', ball('S', 'PKF', 1, 'd', 0, 0)), arm(st('ut', k=FK, ro=2, r=0, v=Q, b=1, s=0)))
rx(arm(nd(**c_(0)), 'S', ball('S', 'PKF', 1, 'd', 1, 0)), arm(st('ut', k=FK, ro=3, r=0, v=Q, b=1, s=0)))
rx(arm(st('ut', k=FK, ro=2, r=0, v=RL, b=0, s=0)), arm(st('ut', k=FK, ro=3, r=0, v=RL, b=0, s=0)))
for a, in ((RS,), (RF,)):
    rx(arm(st('ut', k=FK, ro=2, r=0, v=a, b=0, s=0)), arm(nd(**c_(2, 1)), 'S', ball('S', 'G', a, b=0, s=0)))
rx(B_(ne=ball('S', 'G'), se=nd(k=FK, r=0, ro=0)), B_(se=st('g', k=FK, r=0, ro=0)))
rx(arm(st('g', k=FK, r=0, ro=0, v=RS, b='b', s=0)), arm(nd(k=FK, r=1, ro=6), 'S', ball('S', 'C', 0, 'd', 'b', 0)))
rx(arm(st('g', k=FK, r=0, ro=0, v=RF, b='b', s=0)), arm(nd(k=FK, r=2, ro=6), 'E', ball('E', 'C', 0, 'd', 'b', 0)))
rx(arm(nd(**c_(2, 1)), 'S', ball('S', 'G', RS, b=1, s=0)), arm(st('ut', k=FK, ro=3, r=0, v=RS, b=0, s=0)))
rx(arm(nd(**c_(2, 1)), 'S', ball('S', 'G', RF, b=1, s=0)), arm(st('ut', k=FK, ro=4, r=0, v=NEXT, b=0, s=0)))
rx(arm(st('ut', k=FK, ro=4, r=0, v=NX, b=0, s=0)), arm(nd(**c_(2, 2)), 'E', ball('E', 'C', 0, b=0, s=0)))
rx(arm(nd(**c_(2, 2)), 'E', ball('E', 'C', 0, b=1, s=0)), arm(st('ut', k=FK, ro=5, r=0, v=NX, b=0, s=0)))
rx(arm(st('ut', k=FK, ro=5, r=0, v=NEXT, b=0, s=0)), arm(st('ut', k=FK, ro=3, r=0, v=RF, b=0, s=0)))

# the data's side, s=1: what happens at the end of its route
def turn(v0, b0, v1, b1, n0, n1=None):
    rx(B_(ne=ball('S', 'U', v0, b=b0, s=1), se=n0), B_(ne=ball('S', 'U', v1, b=b1, s=1), se=n1 or n0))
turn(0, 0, 0, 1, nd(r=0))
for k in (LF, ST):
    turn(SWAP, 0, SWAP, 1, nd(k=k, r=0))
turn(SWAP, 0, SWAP, 1, nd(k=FK, f=0, r=0), nd(k=FK, f=1, r=0))
turn(SWAP, 1, SWAP, 0, nd(k=FK, f=0, r=0), nd(k=FK, f=1, r=0))
# ask: a leaf answers; a stem or fork moves the end of the route into a part, and the part answers
turn(Q, 1, RL, 0, nd(k=LF, r=0))
for k in (ST, FK):
    rx(B_(ne=ball('S', 'U', Q, b='b', s=1), se=nd(k=k, r=0)), B_(se=st('q', v=Q, s=1, k=k, r=0)))
rx(arm(st('q', v=Q, s=1, k=ST, r=0)), arm(nd(k=ST, r=1), 'S', ball('S', 'UQ', RS, b='b', s=1)))
rx(arm(st('q', v=Q, s=1, k=FK, f=0, r=0)), arm(nd(k=FK, f=0, r=1), 'S', ball('S', 'UQ', RF, b='b', s=1)))
rx(arm(st('q', v=Q, s=1, k=FK, f=1, r=0)), arm(nd(k=FK, f=1, r=2), 'E', ball('E', 'UQ', RF, b='b', s=1)))
for a in (RS, RF, NX):
    rx(B_(ne=ball('S', 'UQ', a, b=1, s=1), se=nd(r=0)), B_(ne=ball('S', 'U', a, b=0, s=1), se=nd(r=0)))
# next: the end of the route moves from the left part to the right part, through the parent
rx(B_(ne=ball('S', 'U', NEXT, b=0, s=1), se=nd(r=0)), B_(ne=ball('S', 'PN', 0, b=1, s=1), se=nd(r=0)))
rx(arm(nd(k=FK, f=0, r=1), 'S', ball('S', 'PN', 0, b=1, s=1)), arm(nd(k=FK, f=0, r=2), 'E', ball('E', 'UQ', NX, b=1, s=1)))
rx(arm(nd(k=FK, f=1, r=2), 'E', ball('E', 'PN', 0, b=1, s=1)), arm(nd(k=FK, f=1, r=1), 'S', ball('S', 'UQ', NX, b=1, s=1)))


# the kink: F's right ray is seen by a catcher in F's own column of the layout. T: out east; AC, PS, M1: out north.
def mc(k, m=0, v=0, d=0, b=0, s=0):
    return cell(MIR, k, 0, 0, 0, m, v, d, b, s)
def mh(k):
    return cell(MIR, k, 1, 0, 0, '_m', 'v', 'd', 'b', 's')
rx(B_(sw=HANY, se=mc(T0)), B_(se=mh(T0)))                  # straight through an unkinked T
rx(A_(nw=mh(T0)), A_(nw=mc(T0), ne=HANY))
rx(A_(nw=mc(T1), sw=VANY), A_(nw=mc(T1), ne=HANY))         # south arm to east arm
rx(B_(sw=HANY, se=mc(AC)), B_(ne=VANY, se=mc(AC)))         # west arm to north arm
rx(A_(nw=mh(PS)), A_(nw=mc(PS), sw=VANY))                  # a passive stack cell on an even row
rx(B_(ne=VANY, se=mc(PS)), B_(se=mh(PS)))
rx((O, mh(M1), O, O), (O, mc(M1), O, VANY))                # a stack cell on an odd row
rx((O, O, mh(M1), O), (VANY, O, mc(M1), O))
K = lambda m, d='d': cell(KB, m=m, d=d)
rx((K('KM'), O, O, O), (O, K('KM'), O, O))                 # K balls fly on their own reactions
rx((O, O, K('KM'), O), (O, O, O, K('KM')))
rx((K('KB'), O, O, O), (O, K('KB'), O, O))
rx((O, O, K('KB'), O), (O, O, O, K('KB')))

# STEM: the whole left spine under the focus moves down a site, leaf first; a stem appears in the room at the top.
pm = lambda: ball('S', 'PM', 0, 'd', 0, 0)
rx(B_(ne=ball('S', 'U', STEM, 'd', 0, 1) if False else cell(VB, m='U', v=STEM, d=1, b=0, s=1), se=nd(r=0, ro=0)), B_(se=nd(r=0, ro=1, m='pd', d=1)))
rx(B_(ne=cell(VB, m='U', v=STEM, d=0, b=1, s=1), se=nd(r=0, ro=0)), B_(se=nd(r=0, ro=1, m='pd', d=0)))
rx(B_(ne=ball('S', 'PD', 0, 'd', 0, 0), se=nd(r=0, ro=0)), B_(se=nd(r=0, ro=0, m='pd', d='d')))
rx(arm(nd(k=ST, r=0, m='pd', d='d')), arm(nd(k=ST, r=0), 'S', ball('S', 'PD', 0, 'd', 0, 0)))
rx(arm(nd(k=FK, r=0, m='pd', d='d')), arm(nd(k=FK, r=0), 'S', ball('S', 'PD', 0, 'd', 0, 0)))
rx(arm(nd(k=LF, r=0, m='pd', d='d')), A_(sw=nd(k=LF, r=0, m='m1', d='d')))
rx((O, nd(r=0, m='m1', d='d'), O, O), (O, O, O, nd(r=0, m='m2', d='d')))
rx(A_(nw=nd(r=0, m='m2', d='d')), A_(nw=nd(r=0, m='m3', d='d')))
rx(B_(se=nd(r=0, ro=0, m='m3', d='d')), B_(ne=pm(), se=nd(r=0, ro=0)))
rx(arm(nd(k=ST, r=0), 'S', pm()), A_(sw=nd(k=ST, r=0, m='m1', d='d')))
rx(B_(se=nd(r=0, ro=1, m='m3', d='d')), B_(ne=cell(PB, m='TOP', d='d'), se=nd(r=0, ro=0)))
rx(A_(sw=cell(PB, m='TOP', d='d')), A_(nw=nd(k=ST, f=0, r=0, ro=0, m='new', d='d')))
rx(B_(se=nd(k=ST, f=0, r=0, ro=0, m='new', d=1)), B_(ne=cell(VB, m='U', v=STEM, d=1, b=1, s=1), se=nd(k=ST, f=0, r=0, ro=0)))
rx(B_(se=nd(k=ST, f=0, r=0, ro=0, m='new', d=0)), B_(ne=cell(VB, m='U', v=STEM, d=0, b=0, s=1), se=nd(k=ST, f=0, r=0, ro=0)))
# a fork first builds two more catchers under its catcher, then is sent down by the last one
rx(arm(nd(k=FK, r=0), 'S', pm()), arm(nd(k=FK, r=0), 'E', K('KM')))
rx(B_(sw=K('KM'), se=mc(T0)), B_(se=mc(T1, 'kk', 0, 'd')))
rx(B_(sw=K('KM'), se=mc(AC)), B_(se=mc(PS, 'kk', 0, 'd')))
rx(A_(nw=mc('z', 'kk', 0, 'd')), A_(nw=mc('z'), sw=mc(M1, 'kk', 0, 'd')))
rx((O, mc(M1, 'kk', 0, 'd'), O, O), (O, mc(M1, 'kb', 0, 'd'), O, mc(AC)))
rx(A_(nw=mc('z'), sw=mc(M1, 'kb', 0, 'd')), A_(nw=mc('z', 'kb', 0, 'd'), sw=mc(M1)))
rx(B_(se=mc('z', 'kb', 0, 'd')), B_(sw=K('KB'), se=mc('z')))
rx(arm(nd(k=FK, r=0), 'E', K('KB')), A_(sw=nd(k=FK, r=0, m='m1', d='d')))

REACTIONS = R
RANGES = [7, 8, 2, 3, 7, len(_m) + 1, 13, 2, 2, 2]
SHAPES = '?'

# layout: left part straight down, right part east then down
COL = 4
def width(t):
    if t == 'L': return 2
    if t[0] == 'S': return width(t[1])
    return width(t[1]) + 2 + width(t[2])
def _c(*a): return tuple(a) + (0,) * (10 - len(a))
def lay(t):
    cells = {}
    def go(t, x, y):
        if t == 'L': cells[(x, y)] = _c(NODE, LF); return
        if t[0] == 'S':
            cells[(x, y)] = _c(NODE, ST); go(t[1], x, y + 2); return
        cells[(x, y)] = _c(NODE, FK)
        w = width(t[1])
        cells[(x + w, y)] = _c(MIR, T0)
        cells[(x + w + 2, y)] = _c(MIR, ELBOW)
        go(t[1], x, y + 2)
        go(t[2], x + w + 2, y + 2)
    go(t, 0, 0)
    return Lay(cells, (0, 0), S)
def link(dp, dx):
    xd = dp.w + dp.w % 2 + COL
    return Link((0, 2), (xd, 2), {(0, 0): _c(MIR, HUB, 0, 0, 0, m_('start'), 0, 1), (xd, 0): _c(MIR, DHUB)}, 0)
def node(cell, step):
    c = cell[0]
    if c in (VOID, HB, VB, KB, PB): return ('pass', step)
    if c == MIR:
        k = cell[1]
        if k == ELBOW: return ('pass', S) if step == E else None
        if k in (T0, T1): return ('pass', E) if step in (E, N) else None
        if k in (AC, PS, M1): return ('pass', N) if step in (E, N) else None
        return None
    k = cell[1]
    if k == LF: return ('leaf',)
    if k == ST: return ('stem', S)
    return ('fork', S, E) if cell[2] == 0 else ('fork', E, S)
