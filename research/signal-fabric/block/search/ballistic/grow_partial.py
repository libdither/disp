"""Ballistic mirror machine. Matter: nodes and elbows on (even, even) sites; a link is a sight line through empties.
One ball (mode m, payload v, direction d, turned bit b, side s). Every walk is reversed by the same reactions: a ball that turns
retraces its path, undoing the walk, so a return needs no reactions of its own. b tells a fork a part has finished."""
from physics import I, A
from laws import Lay, Link

NAME = 'ballistic'
E, S, W, N = (1, 0), (0, 1), (-1, 0), (0, -1)
VOID, HB, VB, NODE, MIR, KB, PB, PV_ = 0, 1, 2, 3, 4, 5, 6, 7   # KB flies east-west and sees mirrors, PB does not fly
LF, ST, FK = 0, 1, 2
ELBOW, HUB, DHUB = 0, 1, 2
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
# a stuck run has retraced itself back into the hub; it leaves by the other block, so it never restarts
rx(B_(se=cell(MIR, HUB, 0, 0, 0, 'start', 0, 1, 0, 0)), B_(ne=ball('S', 'STUCK', 0, 1, 0, 0), se=HU))
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
    rx(arm(st('c', v=n, k=ST, f=0, r=0, ro=0)), arm(nd(k=ST, f=0, r=0, ro=0), 'S', ball('S', 'C', v=n + 1)))
# a fork peeks at its left part
rx(B_(ne=ball('S', 'C'), se=nd(k=FK, r=0, ro=0)), B_(se=st('c', k=FK, r=0, ro=0)))
rx(arm(st('c', k=FK, r=0, ro=0)), arm(nd(k=FK, r=0, ro=0), 'S', ball('S', 'PK')))
for kw, m in ((dict(k=ST, f=0), 'PKS'), (dict(k=ST, f=1), 'PKF'), (dict(k=FK), 'PKF')):
    rx(B_(ne=ball('S', 'PK'), se=nd(**kw)), B_(ne=ball('S', m), se=nd(**kw)))

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
rx(B_(ne=ball('S', 'XS', v=0), se=nd(k=ST, f=0, r=0, ro=0)), B_(se=st('xs', k=ST, f=0, r=0, ro=0, v=0)))
rx(arm(st('xs', k=ST, f=0, r=0, ro=0, v=0)), arm(nd(k=ST, f=0, r=0, ro=0), 'S', ball('S', 'C', v=0)))
# dag: F(L, b) is a stem-like cell with its leaf inside; the call goes through, direction flipped
for d in (0, 1):
    rx(arm(st('c', v=0, d=d, k=ST, f=1, r=0, ro=0)), arm(nd(k=ST, f=1, r=0, ro=0), 'S', ball('S', 'C', 0, 1 - d)))

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
rx(B_(ne=ball('S', 'G', RS, b=0, s=0), se=nd(k=ST, f=1, r=0, ro=0)), B_(ne=ball('S', 'G', RS, b=1, s=0), se=nd(k=ST, f=1, r=0, ro=0)))
rx(B_(ne=ball('S', 'G', RF, b='b', s=0), se=nd(k=ST, f=1, r=0, ro=0)), B_(se=st('g', k=ST, f=1, r=0, ro=0, v=RF, b='b', s=0)))
rx(arm(st('g', k=ST, f=1, r=0, ro=0, v=RF, b='b', s=0)), arm(nd(k=ST, f=1, r=1, ro=6), 'S', ball('S', 'C', 0, 'd', 'b', 0)))
rx(arm(nd(**c_(2, 1)), 'S', ball('S', 'G', RS, b=1, s=0)), arm(st('ut', k=FK, ro=3, r=0, v=RS, b=0, s=0)))
rx(arm(nd(**c_(2, 1)), 'S', ball('S', 'G', RF, b=1, s=0)), arm(st('ut', k=FK, ro=4, r=0, v=NEXT, b=0, s=0)))
rx(arm(st('ut', k=FK, ro=4, r=0, v=NX, b=0, s=0)), arm(nd(**c_(2, 2)), 'E', ball('E', 'C', 0, b=0, s=0)))
rx(arm(nd(**c_(2, 2)), 'E', ball('E', 'C', 0, b=1, s=0)), arm(st('ut', k=FK, ro=5, r=0, v=NX, b=0, s=0)))
rx(arm(st('ut', k=FK, ro=5, r=0, v=NEXT, b=0, s=0)), arm(st('ut', k=FK, ro=3, r=0, v=RF, b=0, s=0)))

# the data's side, s=1: what happens at the end of its route
def turn(v0, b0, v1, b1, n0, n1=None):
    rx(B_(ne=ball('S', 'U', v0, b=b0, s=1), se=n0), B_(ne=ball('S', 'U', v1, b=b1, s=1), se=n1 or n0))
turn(0, 0, 0, 1, nd(r=0))
turn(SWAP, 0, SWAP, 1, nd(k=LF, r=0))
turn(SWAP, 0, SWAP, 1, nd(k=ST, f=0, r=0))
turn(SWAP, 0, SWAP, 1, nd(k=ST, f=1, r=0), nd(k=ST, f=2, r=0))
turn(SWAP, 1, SWAP, 0, nd(k=ST, f=1, r=0), nd(k=ST, f=2, r=0))
turn(SWAP, 0, SWAP, 1, nd(k=FK, f=0, r=0), nd(k=FK, f=1, r=0))
turn(SWAP, 1, SWAP, 0, nd(k=FK, f=0, r=0), nd(k=FK, f=1, r=0))
# ask: a leaf answers; a stem or fork moves the end of the route into a part, and the part answers
turn(Q, 1, RL, 0, nd(k=LF, r=0))
for k, f in ((ST, 0), (ST, 2), (FK, 'f')):
    rx(B_(ne=ball('S', 'U', Q, b='b', s=1), se=nd(k=k, f=f, r=0)), B_(se=st('q', v=Q, s=1, k=k, f=f, r=0)))
rx(arm(st('q', v=Q, s=1, k=ST, f=0, r=0)), arm(nd(k=ST, f=0, r=1), 'S', ball('S', 'UQ', RS, b='b', s=1)))
rx(arm(st('q', v=Q, s=1, k=ST, f=2, r=0)), arm(nd(k=ST, f=2, r=1), 'S', ball('S', 'UQ', RF, b='b', s=1)))
turn(Q, 1, RF, 0, nd(k=ST, f=1, r=0), nd(k=ST, f=1, r=3))
turn(Q, 1, RL, 0, nd(r=3))
turn(0, 0, 0, 1, nd(r=3))
turn(SWAP, 0, SWAP, 1, nd(r=3))
rx(B_(ne=ball('S', 'U', NX, b=0, s=1), se=nd(k=ST, f=2, r=3)), B_(se=st('ut', v=NX, b=0, s=1, k=ST, f=2, r=3)))
rx(B_(ne=ball('S', 'U', NEXT, b=0, s=1), se=nd(k=ST, f=1, r=3)), B_(se=st('nx', v=NEXT, b=0, s=1, k=ST, f=1, r=3)))
rx(arm(st('nx', v=NEXT, b=0, s=1, k=ST, f=1, r=3)), arm(nd(k=ST, f=1, r=1), 'S', ball('S', 'UQ', NX, b=1, s=1)))
rx(arm(nd(k=ST, f=2, r=1), 'S', ball('S', 'PN', 0, b=1, s=1)), arm(st('ut', v=NX, b=0, s=1, k=ST, f=2, r=3)))
rx(arm(st('q', v=Q, s=1, k=FK, f=0, r=0)), arm(nd(k=FK, f=0, r=1), 'S', ball('S', 'UQ', RF, b='b', s=1)))
rx(arm(st('q', v=Q, s=1, k=FK, f=1, r=0)), arm(nd(k=FK, f=1, r=2), 'E', ball('E', 'UQ', RF, b='b', s=1)))
for a in (RS, RF, NX):
    rx(B_(ne=ball('S', 'UQ', a, b=1, s=1), se=nd(r=0)), B_(ne=ball('S', 'U', a, b=0, s=1), se=nd(r=0)))
# next: the end of the route moves from the left part to the right part, through the parent
rx(B_(ne=ball('S', 'U', NEXT, b=0, s=1), se=nd(r=0)), B_(ne=ball('S', 'PN', 0, b=1, s=1), se=nd(r=0)))
rx(arm(nd(k=FK, f=0, r=1), 'S', ball('S', 'PN', 0, b=1, s=1)), arm(nd(k=FK, f=0, r=2), 'E', ball('E', 'UQ', NX, b=1, s=1)))
rx(arm(nd(k=FK, f=1, r=2), 'E', ball('E', 'PN', 0, b=1, s=1)), arm(nd(k=FK, f=1, r=1), 'S', ball('S', 'UQ', NX, b=1, s=1)))



# K balls and P balls fly on their own reactions, so a mirror can tell them apart from a passing call
K = lambda m, d='d': cell(KB, m=m, d=d)
KA = cell(KB, m='_m', d='d')
rx((KA, O, O, O), (O, KA, O, O))
rx((O, O, KA, O), (O, O, O, KA))
PV = cell(PV_, m='PM', d='d')
rx((PV, O, O, O), (O, O, PV, O))
rx((O, PV, O, O), (O, O, O, PV))

# STEM: the whole part under the focus moves down one site, children first; a stem appears in the room left at the top.
Us = lambda d, b, v: cell(VB, m='U', v=v, d=d, b=b, s=1)
for v, p in ((STEM, 1), (UNIT, 2)):
    for kw in (dict(k=LF, f=0), dict(k=ST), dict(k=FK)):
        rx(B_(ne=Us(1, 0, v), se=nd(r=0, ro=0, **kw)), B_(se=nd(r=0, ro=p, m='pd', d=1, **kw)))
        rx(B_(ne=Us(0, 1, v), se=nd(r=0, ro=0, **kw)), B_(se=nd(r=0, ro=p, m='pd', d=0, **kw)))
    rx(B_(ne=ball('S', 'UM', v, 'd', 0, 1), se=nd(k=LF, f=0, r=0, ro=0)), B_(se=nd(k=LF, f=2, r=0, ro=p, m='pd', d='d')))
rx(B_(ne=ball('S', 'PD', 0, 'd', 0, 0), se=nd(r=0, ro=0)), B_(se=nd(r=0, ro=0, m='pd', d='d')))
for k in (ST, FK):
    rx(arm(nd(k=k, r=0, m='pd', d='d')), arm(nd(k=k, r=0), 'S', ball('S', 'PD', 0, 'd', 0, 0)))
rx(arm(nd(k=LF, r=0, m='pd', d='d')), A_(sw=nd(k=LF, r=0, m='m1', d='d')))
rx((O, nd(r=0, m='m1', d='d'), O, O), (O, O, O, nd(r=0, m='m2', d='d')))
rx(arm(nd(k=ST, r=0), 'S', PV), A_(sw=nd(k=ST, r=0, m='m1', d='d')))
for k in (ST, LF):
    rx(A_(nw=nd(k=k, r=0, m='m2', d='d')), A_(nw=nd(k=k, r=0, m='m3', d='d')))
rx(B_(se=nd(r=0, ro=0, m='m3', d='d')), B_(ne=PV, se=nd(r=0, ro=0)))
rx(B_(se=nd(r=0, ro=2, m='m3', d='d')), B_(se=nd(r=0, ro=3, m='pd', d='d')))
for v, p in ((STEM, 1), (UNIT, 3)):
    rx(B_(se=nd(r=0, ro=p, m='m3', d='d')), B_(ne=cell(PB, m='TOP', v=v, d='d'), se=nd(r=0, ro=0)))
rx(A_(sw=cell(PB, m='TOP', v=STEM, d='d')), A_(nw=nd(k=ST, f=0, r=0, ro=0, m='new', d='d')))
rx(A_(sw=cell(PB, m='TOP', v=UNIT, d='d')), A_(nw=nd(k=ST, f=1, r=0, ro=0, m='up1', d='d')))
rx(B_(se=nd(k=ST, f=1, r=0, ro=0, m='up1', d='d')), (O, nd(k=ST, f=1, r=0, ro=0, m='up2', d='d'), O, O))
rx(A_(sw=nd(k=ST, f=1, r=0, ro=0, m='up2', d='d')), A_(nw=nd(k=ST, f=1, r=0, ro=0, m='new', d='d')))
for v, f in ((STEM, 0), (UNIT, 1)):
    rx(B_(se=nd(k=ST, f=f, r=0, ro=0, m='new', d=1)), B_(ne=Us(1, 1, v), se=nd(k=ST, f=f, r=0, ro=0)))
    rx(B_(se=nd(k=ST, f=f, r=0, ro=0, m='new', d=0)), B_(ne=Us(0, 0, v), se=nd(k=ST, f=f, r=0, ro=0)))
# a fork: left part moved, now the right part; then its elbow builds two more below itself, the fork moves, the extras go
rx(arm(nd(k=FK, r=0), 'S', PV), arm(nd(k=FK, r=0), 'E', ball('E', 'PD', 0, 'd', 0, 0)))
G = lambda m=0: cell(MIR, ELBOW, 0, 0, 0, m, 0, 'd' if m else 0, 0, 0)
rx(A_(nw=EL, sw=PV), A_(nw=EL, sw=G('kk')))
rx((O, G('kk'), O, O), (O, G('kb'), O, EL))
rx(A_(nw=EL, sw=G('kb')), A_(nw=G('kb'), sw=EL))
rx(B_(se=G('kb')), B_(sw=K('KB'), se=EL))
rx(arm(nd(k=FK, r=0), 'E', K('KB')), A_(sw=nd(k=FK, r=0, m='m1', d='d')))
rx(arm(nd(k=FK, r=0, m='m2', d='d')), arm(nd(k=FK, r=0, m='fd', d='d') if False else nd(k=FK, r=0), 'E', K('KD')))
rx((O, EL, K('KD'), EL), (O, O, O, G('dk')))
rx(A_(nw=G('dk')), A_(nw=G('dl')))
rx(B_(se=G('dl')), B_(ne=cell(PB, m='DEL', d='d'), se=EL))
rx(A_(nw=EL, sw=cell(PB, m='DEL', d='d')), A_(sw=cell(PB, m='DEL2', d='d')))
rx(B_(ne=cell(PB, m='DEL2', d='d'), se=EL), B_(se=G('dm')))
rx(A_(nw=G('dm')), A_(nw=G('dn')))
rx(B_(se=G('dn')), B_(sw=K('KR'), se=EL))
rx(arm(nd(k=FK, r=0), 'E', K('KR')), arm(nd(k=FK, r=0, m='m3', d='d')))

# a unit's own leaf is drawn out when something must happen to it: elbow and leaf appear beside the fork, then the cell turns into a fork
PBc = lambda m: cell(PB, m=m, v='v', d='d')
for v in (STEM, UNIT):
    rx(B_(ne=Us(1, 0, v), se=nd(k=ST, r=3)), B_(se=nd(k=ST, r=3, m='mt', v=v, d=1)))
rx(arm(nd(k=ST, r=3, m='mt', v='v', d='d')), A_(nw=nd(k=ST, r=3), ne=PBc('MA')))
ELM = lambda m: cell(MIR, ELBOW, 0, 0, 0, m, 'v', 'd', 0, 0)
rx(B_(sw=PBc('MA')), B_(se=ELM('mt')))
rx(A_(nw=ELM('mt')), A_(nw=EL, sw=PBc('MB')))
rx((O, PBc('MB'), O, O), (O, O, O, nd(k=LF, f=0, r=0, ro=0, m='mt', v='v', d='d')))
rx(A_(nw=nd(k=LF, f=0, r=0, ro=0, m='mt', v='v', d='d')), A_(nw=nd(k=LF, f=0, r=0, ro=0, m='mr', v='v', d='d')))
rx(B_(se=nd(k=LF, f=0, r=0, ro=0, m='mr', v='v', d='d')), B_(ne=PBc('MC'), se=nd(k=LF, f=0, r=0, ro=0)))
rx(A_(nw=EL, sw=PBc('MC')), A_(nw=ELM('mr')))
rx(B_(se=ELM('mr')), B_(sw=PBc('MD'), se=EL))
for f, f2 in ((1, 1), (2, 0)):
    rx(A_(nw=nd(k=ST, f=f, r=3), ne=PBc('MD')), A_(nw=nd(k=FK, f=f2, r=2, m='ms', v='v', d='d')))
rx(B_(se=nd(k=FK, r=2, m='ms', v='v', d='d')), B_(se=nd(k=FK, r=2, m='mu', v='v', d='d')))
rx(arm(nd(k=FK, r=2, m='mu', v='v', d='d')), arm(nd(k=FK, r=2), 'E', ball('E', 'UM', 'v', 'd', 0, 1)))


REACTIONS = R
RANGES = [8, 4, 3, 4, 7, len(_m) + 1, 13, 2, 2, 2]
SHAPES = '?'

# layout: left part straight down, right part east then down
COL = 4
Z = (0, 0)
def width(t):
    if t == 'L': return 4
    if t[0] == 'S': return width(t[1])
    return width(t[2]) if t[1] == 'L' else width(t[1]) + width(t[2])
def _c(*a): return tuple(a) + (0,) * (10 - len(a))
def lay(t):
    cells = {}
    def go(t, x, y):
        if t == 'L': cells[(x, y)] = _c(NODE, LF); return
        if t[0] == 'S':
            cells[(x, y)] = _c(NODE, ST); go(t[1], x, y + 2); return
        if t[1] == 'L':
            cells[(x, y)] = _c(NODE, ST, 1); go(t[2], x, y + 4); return
        cells[(x, y)] = _c(NODE, FK)
        w = width(t[1])
        cells[(x + w, y)] = _c(MIR, ELBOW)
        go(t[1], x, y + 2)
        go(t[2], x + w, y + 2)
    go(t, 0, 0)
    return Lay(cells, (0, 0), S)
def link(dp, dx):
    xd = dp.w + dp.w % 2 + COL
    return Link((0, 2), (xd, 2), {(0, 0): _c(MIR, HUB, 0, 0, 0, m_('start'), 0, 1), (xd, 0): _c(MIR, DHUB)}, 0)
def node(cell, step):
    c = cell[0]
    if c in (VOID, HB, VB, KB, PB, PV_): return ('pass', step)
    if c == MIR:
        if cell[1] == 3: return ('pass', E) if step == S else None
        return ('pass', S) if cell[1] == ELBOW and step in (E, S) else None
    k = cell[1]
    if k == LF: return ('leaf',)
    if k == ST:
        f = cell[2]
        if f == 0: return ('stem', S)
        if step == Z: return ('leaf',)
        return ('fork', Z, S) if f == 1 else ('fork', S, Z)
    return ('fork', S, E) if cell[2] == 0 else ('fork', E, S)
