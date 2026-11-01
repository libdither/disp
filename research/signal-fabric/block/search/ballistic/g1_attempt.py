"""Ballistic mirror machine. Matter: nodes and elbows on (even, even) sites; a link is a sight line through empties.
One ball (mode m, payload v, direction d, turned bit b, side s). Every walk is reversed by the same reactions: a ball that turns
retraces its path, undoing the walk, so a return needs no reactions of its own. b tells a fork a part has finished."""
from physics import I, A
from laws import Lay, Link

NAME = 'ballistic'
E, S, W, N = (1, 0), (0, 1), (-1, 0), (0, -1)
VOID, HB, VB, NODE, MIR, KB, PB, PV_ = 0, 1, 2, 3, 4, 5, 6, 7   # KB flies east-west and sees mirrors, PB does not fly
LF, ST, FK = 0, 1, 2
ELBOW, HUB, DHUB, CAT, ELX, CA1, TQ, SHUB, RQ = 0, 1, 2, 3, 4, 5, 6, 7, 8
SELF, SWAP, CROSS, SINK, STEM, UNIT = 1, 2, 3, 4, 5, 6
Q, RL, RS, RF, NEXT, NX, QT, AN, AL, AS, AF, LEFT, ROT, UR, GOR, SF, D1, W0, W1, E1, U1 = 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 24, 25, 26, 27, 28, 29, 30, 31

_m = {}
def m_(name):
    return _m.setdefault(name, len(_m) + 1)
def lit(x):
    return I(x) if isinstance(x, int) else A(x)
def cell(cls, k=0, f=0, r=0, ro=0, m=0, v=0, d=0, b=0, s=0, e=0, t1=0, t2=0):
    return tuple(lit(x) for x in (cls, k, f, r, ro, m_(m) if isinstance(m, str) and m[0] != '_' else (m if isinstance(m, int) else m[1:]), v, d, b, s, e, t1, t2))
O = cell(VOID)
TV = dict(t1='t1', t2='t2')
def ball(arm, m, v='v', d='d', b='b', s='s', **kw):
    return cell(HB if arm == 'E' else VB, m=m, v=v, d=d, b=b, s=s, **kw)
def nd(k='k', f='f', r='r', ro='ro', m=0, v=0, d=0, b=0, s=0, e=0, **kw):
    return cell(NODE, k, f, r, ro, m, v, d, b, s, e, **kw)
def held(k='k', f='f', r='r', ro='ro', m='_m', v='v', d='d', b='b', s='s'):
    return nd(k, f, r, ro, m, v, d, b, s)

R = []
SEC = []
def sec(n):
    SEC.append((n, len(R)))
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

sec('flight')
# flight: heading is phase
HANY, VANY = ball('E', '_m', **TV), ball('S', '_m', **TV)
rx((HANY, O, O, O), (O, HANY, O, O))
rx((O, O, HANY, O), (O, O, O, HANY))
rx((VANY, O, O, O), (O, O, VANY, O))
rx((O, VANY, O, O), (O, O, O, VANY))

sec('elbow')
# the elbow: east to south and back, any ball, held a tick since its arms are in different blocks
EL = cell(MIR, ELBOW)
HEL = cell(MIR, ELBOW, 1, 0, 0, '_m', 'v', 'd', 'b', 's', **TV)
rx(B_(sw=HANY, se=EL), B_(se=HEL))
rx(A_(nw=HEL), A_(nw=EL, sw=VANY))

sec('hubs')
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

sec('route transit')
# U: along the route, either way. In the data the route is r; in the program also a stem chain.
rx(arm(nd(r=1), 'S', ball('S', 'U', **TV)), arm(st('ut', r=1, **TV)))
rx(arm(nd(r=2), 'E', ball('E', 'U', **TV)), arm(st('ut', r=2, **TV)))
rx(arm(nd(k=ST, r=0, ro=0), 'S', ball('S', 'U', s=0)), arm(st('ut', k=ST, r=0, ro=0, s=0)))
rx(B_(ne=ball('S', 'U', **TV), se=nd(r=1)), B_(se=st('ut', r=1, **TV)))
rx(B_(ne=ball('S', 'U', **TV), se=nd(r=2)), B_(se=st('ut', r=2, **TV)))
rx(B_(ne=ball('S', 'U', s=0), se=nd(k=ST, r=0)), B_(se=st('ut', k=ST, r=0, s=0)))
rx(B_(ne=ball('S', 'U', s=0), se=nd(k=FK, r=0, e='e')), B_(se=st('ut', k=FK, r=0, s=0, e='e')))
# a leaf at the end of a count sends the count to the data
for v in (0, 2, 5, 6):
    rx(B_(ne=ball('S', 'C', v, s=0), se=nd(k=LF, r=0, ro=0)), B_(ne=ball('S', 'U', v, s=0), se=nd(k=LF, r=0, ro=0)))

sec('call and count')
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
sec('seq and dag')
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

sec('map')
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

sec('data ops: ask, next')
# the data's side, s=1: what happens at the end of its route
def turn(v0, b0, v1, b1, n0, n1=None):
    rx(B_(ne=ball('S', 'U', v0, b=b0, s=1), se=n0), B_(ne=ball('S', 'U', v1, b=b1, s=1), se=n1 or n0))
turn(0, 0, 0, 1, nd(r=0))
for k in (LF, ST):
    turn(SWAP, 0, SWAP, 1, nd(k=k, r=0))
# ask: a leaf answers; a stem or fork moves the end of the route into a part, and the part answers
turn(Q, 1, RL, 0, nd(k=LF, r=0))
for k, a, v, g in ((ST, 1, RS, Q), (FK, 1, RF, Q), (FK, 1, LEFT, LEFT), (FK, 2, GOR, GOR)):
    bv = 'b' if g == Q else 1
    rx(B_(ne=ball('S', 'U', g, 'd', bv, 1), se=nd(k=k, r=0, ro=0)), B_(se=nd(k=k, r=0, ro=0, m='q%d' % a, v=g, d='d', b=bv, s=1)))
    rx(arm(nd(k=k, r=0, ro=0, m='q%d' % a, v=g, d='d', b=bv, s=1)), arm(nd(k=k, r=a, ro=0), 'SE'[a - 1], ball('SE'[a - 1], 'UQ', v, 'd', bv, 1)))
for a in (RS, RF, NX, LEFT, GOR):
    rx(B_(ne=ball('S', 'UQ', a, b=1, s=1), se=nd(r=0)), B_(ne=ball('S', 'U', a, b=0, s=1), se=nd(r=0)))
# next: the end of the route moves from the left part to the right part, through the parent
rx(B_(ne=ball('S', 'U', NEXT, b=0, s=1), se=nd(r=0)), B_(ne=ball('S', 'PN', 0, b=1, s=1), se=nd(r=0)))
rx(arm(nd(k=FK, r=1, ro=0), 'S', ball('S', 'PN', 0, b=1, s=1)), arm(nd(k=FK, r=2, ro=0), 'E', ball('E', 'UQ', NX, b=1, s=1)))

sec('K and P flight')
# K balls and P balls fly on their own reactions, so a mirror can tell them apart from a passing call
K = lambda m, d='d', b=0: cell(KB, m=m, d=d, b=b)
KA = cell(KB, m='_m', v='v', d='d', b='b', **TV)
rx((KA, O, O, O), (O, KA, O, O))
rx((O, O, KA, O), (O, O, O, KA))
PV = cell(PV_, m='PM', d='d')
PVm = lambda m, b=0: cell(PV_, m=m, d='d', b=b)
PVA = cell(PV_, m='_m', d='d', b='b')
rx((PVA, O, O, O), (O, O, PVA, O))
rx((O, PVA, O, O), (O, O, O, PVA))

sec('stem, move down')
# STEM: the whole part under the focus moves down one site, children first; a stem appears in the room left at the top.
Us = lambda d, b, v=STEM: cell(VB, m='U', v=v, d=d, b=b, s=1)
rx(B_(ne=Us(1, 0), se=nd(r=0, ro=0)), B_(se=nd(r=0, ro=1, m='pd', d=1)))
rx(B_(ne=Us(0, 1), se=nd(r=0, ro=0)), B_(se=nd(r=0, ro=1, m='pd', d=0)))
rx(B_(ne=ball('S', 'PD', 0, 'd', 0, 0), se=nd(r=0, ro=0, **TV)), B_(se=nd(r=0, ro=0, m='pd', d='d', **TV)))
for k in (ST, FK):
    rx(arm(nd(k=k, r=0, m='pd', d='d', **TV)), arm(nd(k=k, r=0, **TV), 'S', ball('S', 'PD', 0, 'd', 0, 0)))
for ro in (0, 1, 5, 6, 24, 25):
    rx(arm(nd(k=LF, r=0, ro=ro, m='pd', d='d')), A_(sw=nd(k=LF, r=0, ro=ro, m='m1', d='d')))
rx(arm(nd(k=LF, r=0, ro=3, m='pd', d='d')), A_(nw=EL, sw=nd(k=LF, r=0, ro=3, m='m1', d='d')))
rx((O, nd(r=0, m='m1', d='d', **TV), O, O), (O, O, O, nd(r=0, m='m2', d='d', **TV)))
for ro in (0, 1, 5, 6, 24, 25):
    rx(arm(nd(k=ST, r=0, ro=ro, **TV), 'S', PV), A_(sw=nd(k=ST, r=0, ro=ro, m='m1', d='d', **TV)))
rx(arm(nd(k=ST, r=0, ro=3), 'S', PV), A_(nw=EL, sw=nd(k=ST, r=0, ro=3, m='m1', d='d')))
for k in (ST, LF):
    rx(A_(nw=nd(k=k, r=0, m='m2', d='d', **TV)), A_(nw=nd(k=k, r=0, m='m3', d='d', **TV)))
rx(B_(se=nd(r=0, ro=0, m='m3', d='d', **TV)), B_(ne=PV, se=nd(r=0, ro=0, **TV)))
rx(B_(se=nd(r=0, ro=1, m='m3', d='d')), B_(ne=cell(PB, m='TOP', d='d'), se=nd(r=0, ro=0)))
rx(A_(sw=cell(PB, m='TOP', d='d')), A_(nw=nd(k=ST, f=0, r=0, ro=0, m='new', d='d')))
rx(B_(se=nd(k=ST, f=0, r=0, ro=0, m='new', d=1)), B_(ne=Us(1, 1), se=nd(k=ST, f=0, r=0, ro=0)))
rx(B_(se=nd(k=ST, f=0, r=0, ro=0, m='new', d=0)), B_(ne=Us(0, 0), se=nd(k=ST, f=0, r=0, ro=0)))
# a fork: left part moved, now the right part; then its elbow builds two more below itself, the fork moves, the extras go
rx(arm(nd(k=FK, r=0), 'S', PV), arm(nd(k=FK, r=0), 'E', ball('E', 'PD', 0, 'd', 0, 0)))
G = lambda m=0: cell(MIR, ELBOW, 0, 0, 0, m, 0, 'd' if m else 0, 0, 0)
rx(A_(nw=EL, sw=PV), A_(nw=EL, sw=G('kk')))
rx((O, G('kk'), O, O), (O, G('kb'), O, EL))
rx(A_(nw=EL, sw=G('kb')), A_(nw=G('kb'), sw=EL))
rx(B_(se=G('kb')), B_(sw=K('KB'), se=EL))
for ro in (0, 1, 5, 6):
    rx(arm(nd(k=FK, r=0, ro=ro), 'E', K('KB')), A_(sw=nd(k=FK, r=0, ro=ro, m='m1', d='d')))
rx(arm(nd(k=FK, r=0, ro=3), 'E', K('KB')), A_(nw=EL, sw=nd(k=FK, r=0, ro=3, m='m1', d='d')))
rx(arm(nd(k=FK, r=0, m='m2', d='d')), arm(nd(k=FK, r=0, m='fd', d='d') if False else nd(k=FK, r=0), 'E', K('KD')))
rx((O, EL, K('KD'), EL), (O, O, O, G('dk')))
rx(A_(nw=G('dk')), A_(nw=G('dl')))
rx(B_(se=G('dl')), B_(ne=cell(PB, m='DEL', d='d'), se=EL))
rx(A_(nw=EL, sw=cell(PB, m='DEL', d='d')), A_(sw=cell(PB, m='DEL2', d='d')))
rx(B_(ne=cell(PB, m='DEL2', d='d'), se=EL), B_(se=G('dm')))
rx(A_(nw=G('dm')), A_(nw=G('dn')))
rx(B_(se=G('dn')), B_(sw=K('KR'), se=EL))
rx(arm(nd(k=FK, r=0), 'E', K('KR')), arm(nd(k=FK, r=0, m='m3', d='d')))

sec('unit, move east')
# UNIT: first room: down the route, each fork the route leaves by its south arm moves its east part one site east,
# outermost first. Then the focus moves east whole, a leaf and the new fork appear in its place, and it drops under a new elbow.
PE = lambda a: ball(a, 'PE', 0, 'd', 0, 0)
T = lambda a, b='b': ball(a, 'T', 0, 'd', b, 0)
rx(B_(ne=Us(1, 0, UNIT), se=nd(r=0, ro=0)), B_(ne=ball('S', 'T', 0, 1, 0, 0), se=nd(r=0, ro=0)))
rx(B_(ne=ball('S', 'T', 0, 0, 0, 0), se=nd(r=0, ro=0)), B_(ne=Us(0, 1, UNIT), se=nd(r=0, ro=0)))
rx(arm(nd(k=ST, r=1), 'S', T('S')), arm(nd(k=ST, r=1, m='tt', d='d', b='b')))
rx(arm(nd(k=FK, r=1), 'S', T('S', 0)), arm(nd(k=FK, r=1, m='tt', d='d', b=0)))
rx(arm(nd(k=FK, r=1, m='tt', d='d', b=1)), arm(nd(k=FK, r=1), 'E', PE('E')))
rx(arm(nd(k=FK, r=1), 'E', K('KE')), arm(nd(k=FK, r=1), 'S', T('S', 1)))
rx(arm(nd(r=2), 'E', T('E')), arm(nd(r=2, m='tt', d='d', b='b')))
for r in (1, 2):
    rx(B_(ne=T('S'), se=nd(r=r)), B_(se=nd(r=r, m='tt', d='d', b='b')))
rx(A_(nw=DH, sw=T('S', 0)), A_(nw=DH, sw=T('S', 1)))
rx(B_(ne=T('S', 1), se=nd(r=0, ro=0)), B_(se=nd(r=0, ro=4, m='pe', d='d')))
# moving a part east: east part first, then south part, then the node, which leaves two catchers behind it
CC = lambda m=0: cell(MIR, CAT, 0, 0, 0, m, 0, 'd' if m else 0, 0, 0)
ELs = lambda m: cell(MIR, ELBOW, 0, 0, 0, m, 0, 'd', 0, 0)
rx(B_(ne=PE('S'), se=nd(r=0, ro=0)), B_(se=nd(r=0, ro=0, m='pe', d='d')))
rx(arm(nd(k=FK, r=0, m='pe', d='d')), arm(nd(k=FK, r=0), 'E', PE('E')))
rx(arm(nd(k=ST, r=0, m='pe', d='d')), arm(nd(k=ST, r=0), 'S', PE('S')))
rx(arm(nd(k=LF, r=0, m='pe', d='d')), A_(nw=CC(), ne=nd(k=LF, r=0, m='e1', d='d')))
rx(arm(nd(k=FK, r=0), 'E', K('KE')), arm(nd(k=FK, r=0), 'S', PE('S')))
for k in (ST, FK):
    rx(arm(nd(k=k, r=0), 'S', PVm('VE')), A_(nw=CC(), ne=nd(k=k, r=0, m='e1', d='d')))
    rx(arm(nd(k=k, r=0, m='e2', d='d')), arm(nd(k=k, r=0), 'S', PVm('DL')))
    rx(arm(nd(k=k, r=0), 'S', PVm('DU')), arm(nd(k=k, r=0, m='e3', d='d')))
rx((O, O, nd(r=0, m='e1', d='d'), O), (O, O, cell(MIR, CA1), nd(r=0, m='e2', d='d')))
rx(A_(nw=nd(k=LF, r=0, m='e2', d='d')), A_(nw=nd(k=LF, r=0, m='e3', d='d')))
# the part below drops the catchers its parent no longer needs
rx((O, PVm('DL'), cell(MIR, CA1), nd(r=0)), (O, O, O, nd(r=0, m='d1', d='d')))
rx(A_(nw=nd(r=0, m='d1', d='d')), A_(nw=nd(r=0, m='d2', d='d')))
rx(B_(se=nd(r=0, m='d2', d='d')), B_(sw=K('DK'), se=nd(r=0)))
rx(A_(nw=CC(), ne=K('DK')), A_(ne=cell(PB, m='DJ', d='d')))
rx(B_(sw=cell(PB, m='DJ', d='d'), se=nd(r=0)), B_(se=nd(r=0, m='d3', d='d')))
rx(A_(nw=nd(r=0, m='d3', d='d')), A_(nw=nd(r=0, m='d4', d='d')))
rx(B_(se=nd(r=0, m='d4', d='d')), B_(ne=PVm('DU'), se=nd(r=0)))
# back to the parent through the catchers
rx((O, O, cell(MIR, CA1), nd(r=0, ro=0, m='e3', d='d')), (O, O, cell(MIR, CA1, 0, 0, 0, 'e4', 0, 'd'), nd(r=0, ro=0)))
rx((CC(), cell(MIR, CA1, 0, 0, 0, 'e4', 0, 'd'), O, O), (CC('e5'), cell(MIR, CA1), O, O))
rx(B_(se=CC('e5')), B_(ne=PVm('VE'), se=CC()))
# an elbow moves the same way, along the ray that feeds it, so no catchers of its own
rx(A_(nw=EL, sw=PVm('VE')), A_(ne=ELs('e1')))
rx((O, O, ELs('e1'), O), (O, O, O, ELs('e2')))
rx(A_(nw=ELs('e2')), A_(nw=EL, sw=PVm('DL')))
rx(A_(nw=EL, sw=PVm('DU')), A_(nw=ELs('e3')))
rx(B_(se=ELs('e3')), B_(sw=K('KE'), se=EL))
# at the top: a leaf below the catcher, then the catcher becomes the fork; the focus then drops under a new elbow
PBm = lambda m: cell(PB, m=m, d='d')
LFc = lambda m=0: nd(k=LF, f=0, r=0, ro=0, m=m, d='d' if m else 0)
FKc = lambda m=0: nd(k=FK, f=0, r=0, ro=0, m=m, d='d' if m else 0)
rx((O, O, cell(MIR, CA1), nd(r=0, ro=4, m='e3', d='d')), (O, O, cell(MIR, CA1, 0, 0, 0, 't1', 0, 'd'), nd(r=0, ro=0)))
rx((CC(), cell(MIR, CA1, 0, 0, 0, 't1', 0, 'd'), O, O), (CC('t2'), cell(MIR, CA1), O, O))
rx(B_(se=CC('t2')), B_(se=CC('t3')))
rx((CC('t3'), cell(MIR, CA1), O, O), (cell(MIR, CAT, 0, 0, 0, 't4'), cell(MIR, CA1), PBm('ML'), O))
rx((O, PBm('ML'), O, O), (O, O, O, LFc('mk')))
rx(A_(nw=LFc('mk')), A_(nw=LFc('mr')))
rx(B_(se=LFc('mr')), B_(ne=PBm('MC'), se=LFc()))
rx((cell(MIR, CAT, 0, 0, 0, 't4'), cell(MIR, CA1), PBm('MC'), O), (FKc('fa'), cell(MIR, CA1), O, O))
rx(B_(se=FKc('fa')), B_(se=FKc('fb')))
rx((FKc('fb'), cell(MIR, CA1), O, O), (FKc(), cell(MIR, CA1, 0, 0, 0, 't9', 0, 'd'), O, O))
rx((O, O, cell(MIR, CA1, 0, 0, 0, 't9', 0, 'd'), nd(r=0, ro=0)), (O, O, O, nd(r=0, ro=3, m='pd', d='d')))
rx(B_(se=nd(r=0, ro=3, m='m3', d='d')), B_(ne=ball('S', 'FU', 0, 'd', 0, 0), se=nd(r=0, ro=0)))
rx(arm(FKc(), 'E', ball('E', 'FU', 0, 'd', 0, 0)), arm(FKc('nu')))
rx(B_(se=nd(k=FK, f=0, r=0, ro=0, m='nu', d=1)), B_(ne=Us(1, 1, UNIT), se=FKc()))
rx(B_(ne=Us(0, 0, UNIT), se=FKc()), B_(se=nd(k=FK, f=0, r=0, ro=0, m='nv', d=0)))
rx(arm(nd(k=FK, f=0, r=0, ro=0, m='nv', d=0)), arm(FKc(), 'S', cell(PV_, m='UK')))
rx(B_(ne=cell(PV_, m='UK'), se=LFc()), B_(ne=cell(PV_, m='UL'), se=LFc()))
rx(arm(FKc(), 'S', cell(PV_, m='UL')), arm(nd(k=FK, f=0, r=0, ro=0, m='nw', d=0)))
rx(B_(se=nd(k=FK, f=0, r=0, ro=0, m='nw', d=0)), B_(se=nd(k=FK, f=0, r=0, ro=0, m='nu', d=0)))

sec('helpers')
def ns(m, **kw): return nd(m=m, d='d', b='b', **kw)
def ms(k, m): return cell(MIR, k, 0, 0, 0, m, 0, 'd', 'b', 0)
Kb = lambda m: K(m, 'd', 'b')
Pb = lambda m: PVm(m, 'b')
PBb = lambda m: cell(PB, m=m, d='d', b='b')
EL0 = lambda k: cell(MIR, k)
FZ = dict(k=FK, f=0, r=0, ro=0)
sec('cross and sink, by the leaf')
# the leaf at the end of a count steps through a little program, one trip to the data per step; a ball stays and heads back up
Ul = lambda v, b, d='d': ball('S', 'U', v, d, b, 0)
def step(r0, v0, b0, r1, v1, b1, d0='d', d1='d', e0=0, e1=0):
    rx(B_(ne=Ul(v0, b0, d0), se=nd(k=LF, r=0, ro=r0, e=e0)), B_(ne=Ul(v1, b1, d1), se=nd(k=LF, r=0, ro=r1, e=e1)))
for v, r0, rz in ((CROSS, 1, 9), (SINK, 11, 19)):
    rx(B_(ne=ball('S', 'C', v, 'd', 0, 0), se=nd(k=LF, r=0, ro=0)), B_(ne=Ul(QT, 1), se=nd(k=LF, r=0, ro=r0)))
    rx(B_(ne=Ul(QT, 1), se=nd(k=LF, r=0, ro=rz)), B_(ne=ball('S', 'C', v, 'd', 1, 0), se=nd(k=LF, r=0, ro=0)))
for a in (AN, AL, AS, SF, RL):
    step(1, a, 0, 9, a, 0)
step(1, AF, 0, 2, ROT, 0)
step(2, ROT, 1, 3, GOR, 1)
step(3, GOR, 0, 4, SWAP, 0)
step(4, SWAP, 1, 5, GOR, 0)
step(5, GOR, 1, 6, UR, 0)
step(6, UR, 1, 9, AF, 0)
for a in (AN, AL, AF, RL):
    step(11, a, 0, 19, a, 0)
step(11, AS, 0, 12, LEFT, 1)
step(12, LEFT, 0, 13, STEM, 0, 'd', 0, 0, 'd')
step(13, STEM, 1, 14, LEFT, 0, 0, 0, 'e', 'e')
step(14, LEFT, 1, 15, STEM, 0, 0, 1, 'e', 'e')
step(15, STEM, 1, 19, SF, 0, 1, 'e', 'e', 0)
step(11, SF, 0, 16, STEM, 0, 'd', 0, 0, 'd')
step(16, STEM, 1, 17, LEFT, 1, 0, 0, 'e', 'e')
step(17, LEFT, 0, 18, STEM, 0, 0, 1, 'e', 'e')
step(18, STEM, 1, 20, LEFT, 0, 1, 1, 'e', 'e')
step(20, LEFT, 1, 19, AS, 0, 1, 'e', 'e', 0)
sec('rotation')
# F(F(a, b), r) to F(a, F(b, r)). r drops under a second elbow; the near elbow turns into the new fork, an elbow appears above it. One
# cell commits: the fork reads through long steps. Then the old cells go, a rises, and the layout is the ordinary one. Backward is unrot.
Oe = lambda m=0, **kw: nd(k=FK, r=0, ro=0, e=1, m=m, d='d' if m else 0, b='b' if m else 0, **kw)
rx(B_(ne=ball('S', 'U', ROT, 'd', 0, 1), se=nd(k=FK, f=0, r=0, ro=0)), B_(se=nd(k=FK, f=0, r=0, ro=0, e=1, m='ra', d='d', b=0)))
rx(B_(se=nd(k=FK, f=0, r=0, ro=0, e=0, m='rz', d='d', b=0)), B_(ne=ball('S', 'U', ROT, 'd', 1, 1), se=nd(k=FK, f=0, r=0, ro=0)))
rx(B_(ne=ball('S', 'U', UR, 'd', 0, 1), se=nd(k=FK, f=0, r=0, ro=0)), B_(se=nd(k=FK, f=0, r=0, ro=0, e=0, m='rz', d='d', b=1)))
rx(B_(se=nd(k=FK, f=0, r=0, ro=0, e=1, m='ra', d='d', b=1)), B_(ne=ball('S', 'U', UR, 'd', 1, 1), se=nd(k=FK, f=0, r=0, ro=0)))
rx(arm(Oe('ra', f=0)), arm(Oe(f=0), 'E', ball('E', 'PDT', 0, 'd', 'b', 0))) if False else None
Oz = lambda: nd(k=FK, f=0, r=0, ro=0, e=1)
rx(arm(Oe('ra', f=0)), arm(Oz(), 'E', ball('E', 'PDT', 0, 'd', 'b', 0)))
for b, p in ((0, 5), (1, 6)):
    rx(B_(ne=ball('S', 'PDT', 0, 'd', b, 0), se=nd(r=0, ro=0, **TV)), B_(se=nd(r=0, ro=p, m='pd', d='d', **TV)))
    rx(B_(se=nd(r=0, ro=p, m='m3', d='d', **TV)), B_(ne=cell(PB, m='TE', d='d', b=b), se=nd(r=0, ro=0, **TV)))
rx(A_(sw=PBb('TE')), A_(nw=ms(ELBOW, 'c1')))
rx(B_(se=ms(ELBOW, 'c1')), B_(ne=ball('S', 'CU', 0, 'd', 'b', 0), se=EL))
rx(arm(Oz(), 'E', ball('E', 'CU', 0, 'd', 'b', 0)), arm(Oz(), 'S', Pb('C5')))
rx(B_(ne=Pb('C5'), se=nd(**FZ)), B_(se=ns('c2', **FZ)))
rx(arm(ns('c2', **FZ)), arm(nd(**FZ), 'E', Kb('C6')))
F4 = dict(k=FK, f=4, r=0, ro=0)
rx(B_(sw=Kb('C6'), se=EL), B_(se=ns('c3', **F4)))
rx(A_(nw=ns('c3', **F4)), A_(nw=ns('c4', **F4)))
rx(B_(se=ns('c4', **F4)), B_(ne=PBb('C7'), se=nd(**F4)))
rx(A_(sw=PBb('C7')), A_(nw=ms(ELX, 'c5')))
rx(B_(se=ms(ELX, 'c5')), B_(sw=Kb('C8'), se=EL0(ELX)))
rx(arm(Oz(), 'E', Kb('C8')), arm(Oe('c6', f=3)))
rx(B_(se=Oe('c6', f=3)), B_(se=Oe('c7', f=3)))
rx(arm(Oe('c7', f=3)), arm(nd(k=FK, f=3, r=0, ro=0, e=1), 'E', Kb('R1')))
rx(B_(sw=Kb('R1'), se=EL0(ELX)), B_(se=ms(ELBOW, 'r1')))
rx(A_(nw=ms(ELBOW, 'r1')), A_(nw=ms(ELBOW, 'r2')))
rx(B_(se=ms(ELBOW, 'r2')), B_(sw=Kb('R2'), se=EL))
O3 = lambda: nd(k=FK, f=3, r=0, ro=0, e=1)
rx(arm(O3(), 'E', Kb('R2')), arm(Oe('r3', f=3)))
rx(B_(se=Oe('r3', f=3)), B_(se=Oe('r4', f=3)))
rx(arm(Oe('r4', f=3)), arm(O3(), 'S', Pb('R3')))
F5 = dict(k=FK, f=5, r=0, ro=0)
rx(B_(ne=Pb('R3'), se=nd(**FZ)), B_(se=ns('r4', **F5)))
rx(A_(nw=ns('r4', **F5)), A_(nw=ns('r5', **F5)))
rx(B_(se=ns('r5', **F5)), B_(ne=Pb('R4'), se=nd(**F5)))
rx(arm(O3(), 'S', Pb('R4')), arm(Oe('r5', f=0)))
rx(B_(se=Oe('r5', f=0)), B_(se=Oe('r6', f=0)))
rx(arm(Oe('r6', f=0)), arm(Oz(), 'S', Pb('R5')))
rx(B_(ne=Pb('R5'), se=nd(**F5)), B_(ne=PBb('R6')))
rx(A_(nw=Oz(), sw=PBb('R6')), A_(nw=Oe('r7', f=0)))
rx(B_(se=Oe('r7', f=0)), B_(se=Oe('r8', f=0)))
OB = lambda: nd(k=FK, f=0, r=0, ro=0, e=1, b='b')
rx(arm(Oe('r8', f=0)), arm(OB(), 'S', PV))
rx(arm(OB(), 'S', ball('S', 'PD', 0, 'd', 0, 0)), arm(Oe('r9', f=0)))
rx(B_(se=Oe('r9', f=0)), B_(se=Oe('r10', f=0)))
rx(arm(Oe('r10', f=0)), arm(Oz(), 'E', ball('E', 'R6', 0, 'd', 'b', 0)))
rx(B_(ne=ball('S', 'R6', 0, 'd', 'b', 0), se=nd(**F4)), B_(se=ns('r11', **F4)))
rx(arm(ns('r11', **F4)), arm(nd(**F4), 'E', Kb('R7')))
rx(B_(sw=Kb('R7'), se=EL), B_(se=ms(ELBOW, 'r12')))
rx(A_(nw=ms(ELBOW, 'r12')), A_(nw=ms(ELBOW, 'r13')))
rx(B_(se=ms(ELBOW, 'r13')), B_(ne=PBb('R8'), se=EL))
rx(A_(nw=EL, sw=PBb('R8')), A_(sw=PBb('R9')))
rx(B_(ne=PBb('R9'), se=EL), B_(se=ms(ELBOW, 'r14')))
rx(A_(nw=ms(ELBOW, 'r14')), A_(nw=ms(ELBOW, 'r15')))
rx(B_(se=ms(ELBOW, 'r15')), B_(sw=Kb('R10'), se=EL))
rx(arm(nd(**F4), 'E', Kb('R10')), arm(ns('r16', **FZ)))
rx(B_(se=ns('r16', **FZ)), B_(ne=ball('S', 'R11', 0, 'd', 'b', 0), se=nd(**FZ)))
rx(arm(Oz(), 'E', ball('E', 'R11', 0, 'd', 'b', 0)), arm(nd(k=FK, f=0, r=0, ro=0, e=0, m='rz', d='d', b='b')))
sec('swap')
# swap moves the two parts past each other. q sinks below p, one site per node of p; a side ray in the free odd column then reads it. q goes
# west under p and p east over q, each counted by walking the other. One cell commits each change of ray; no flip bit stays.
Fx = lambda ro, m=0, f=0, **kw: nd(k=FK, f=f, r=0, ro=ro, e=1, m=m, d='d' if m else 0, **kw)
U3 = lambda v, b: ball('S', 'U', v, 'd', b, 3)
U3e = lambda v, b: ball('E', 'U', v, 'd', b, 3)
Wb = lambda a, v='v': ball(a, 'W', v, 'd', 0, 0)
WD = lambda a, v='v': ball(a, 'WD', v, 'd', 0, 0)
VB0 = lambda a, m: ball(a, m, 0, 'd', 0, 0)
RQc = lambda m=0: cell(MIR, RQ, 0, 0, 0, m, 0, 'd' if m else 0, 0, 0)
RQh = cell(MIR, RQ, 1, 0, 0, '_m', 'v', 'd', 'b', 's')
# the side ray: a ball goes fork, RQ, down the odd column, TQ, east along the row to the elbow over q
RQh = cell(MIR, RQ, 1, 0, 0, '_m', 'v', 'd', 'b', 's', **TV)
CA1c = lambda m=0: cell(MIR, CA1, 0, 0, 0, m, 0, 'd' if m else 0, 0, 0)
CA1h = cell(MIR, CA1, 1, 0, 0, '_m', 'v', 'd', 'b', 's', **TV)
TQc = lambda m=0: cell(MIR, TQ, 0, 0, 0, m, 0, 'd' if m else 0, 0, 0)
RH = lambda m, v=0, b=0, s=0: cell(MIR, RQ, 1, 0, 0, m, v, 'd', b, s)
RQs = lambda m=0: cell(MIR, RQ, 0, 0, 0, m, 0, 'd' if m else 0, 0, 0)
rx(A_(nw=RQh), A_(nw=RQs(), sw=VANY))
rx(A_(nw=RQh, sw=CA1c()), A_(nw=RQs(), sw=CA1h))
rx((O, CA1h, O, O), (O, CA1c(), O, VANY))
rx((VANY, O, TQc(), O), (O, O, TQc(), HANY))
rx((VANY, O, TQc(), EL), (O, O, TQc(), HEL))
def toR(a, ball_, ro, f, r2):
    """a ball arrives on an arm; the head goes on into RQ"""
    rx(arm(Fx(ro, f=f), a, ball_), (Fx(r2[0], f=r2[1]), O, O, r2[2]))
def frR(rq, ro, f, kw):
    rx((Fx(ro, f=f), O, O, rq), A_(nw=kw))
rx(B_(ne=ball('S', 'U', SWAP, 'd', 0, 1), se=nd(**FZ)), B_(se=Fx(30, 'ca')))
Fy = lambda m: nd(k=FK, f=0, r=0, ro=30, e=1, m=m)
rx((Fx(30, 'ca'), O, O, O), (Fy('cb'), O, O, RQs('r0')))
rx((RQs('r0'), O, O, O), (RQs(), O, TQc('r1'), O))
rx((O, TQc('r1'), O, O), (O, TQc('r2'), O, O))
rx((RQs(), O, TQc('r2'), O), (RQs('r3'), O, TQc(), O))
rx((Fy('cb'), O, O, RQs('r3')), (Fx(30, 'cc'), O, O, RQs()))
rx(B_(se=Fx(30, 'cc')), B_(se=Fx(30, 'ce')))
rx(arm(Fx(30, 'ce')), arm(Fx(30), 'S', Wb('S', D1)))
# q down one, TQ down one: for each node of p but leaves, then once more, then a last time with a new elbow left over q
def drop(ro, ro2, tail):
    rx(arm(Fx(ro), 'E', VB0('E', 'XU')), (Fx(ro + 100), O, O, RQs('td')))
    rx((Fx(ro + 100), O, O, RQs('tv')), A_(nw=Fx(ro, 'x1')))
    rx(B_(se=Fx(ro, 'x1')), B_(se=Fx(ro, 'x2')))
    rx(arm(Fx(ro, 'x2')), tail)
rx((RQs('td'), O, O, O), (RQs(), O, PVm('TD'), O))
rx((PVm('TD'), O, TQc(), O), (O, O, TQc('t1'), O))
rx((O, TQc('t1'), O, O), (O, O, O, TQc('t2')))
rx((TQc('t2'), O, O, O), (O, O, TQc('t3'), O))
rx((O, TQc('t3'), O, O), (O, TQc('t4'), O, O))
rx((O, O, TQc('t4'), O), (PVm('TV'), O, TQc(), O))
rx((RQs(), O, PVm('TV'), O), (RQs('tv'), O, O, O))
rx(arm(Fx(30), 'S', U3(D1, 0)), arm(Fx(30), 'E', VB0('E', 'PDX')))
rx(B_(ne=VB0('S', 'PDX'), se=nd(r=0, ro=0)), B_(se=nd(r=0, ro=25, m='pd', d='d')))
rx(B_(se=nd(r=0, ro=25, m='m3', d='d')), B_(ne=VB0('S', 'XU'), se=nd(r=0, ro=0)))
drop(30, 30, arm(Fx(30), 'S', U3(D1, 1)))
rx(arm(Fx(30), 'S', WD('S', D1)), arm(Fx(31), 'E', VB0('E', 'PDX')))
drop(31, 31, arm(Fx(31), 'E', ball('E', 'PDT', 0, 'd', 0, 0)))
rx(arm(Fx(31), 'E', ball('E', 'CU', 0, 'd', 0, 0)), A_(nw=Fx(44, 'ml', f=7)))
# commit: the fork reads q by the side ray. Then q west, the far elbow with it, once for each fork of p
rx(B_(se=Fx(44, 'ml', f=7)), B_(se=Fx(44, 'mm', f=7)))
rx(arm(Fx(44, 'mm', f=7)), arm(Fx(44, f=7), 'S', Wb('S', W1)))
toR('S', U3(W1, 0), 44, 7, (144, 7, RQs('wa')))
rx((RQs('wa'), O, O, O), (RQs(), O, PVm('WA'), O))
rx((PVm('WA'), O, TQc(), O), (O, O, TQc('wa'), O))
rx((O, TQc('wa'), O, O), (O, TQc('wb'), O, O))
rx((O, O, TQc('wb'), O), (O, O, TQc(), K('KE')))
frR(cell(MIR, RQ, 1, 0, 0, 'PE', 0, 'd', 0, 0), 144, 7, Fx(44, 'w1', f=7))
rx(B_(se=Fx(44, 'w1', f=7)), B_(se=Fx(44, 'w2', f=7)))
rx(arm(Fx(44, 'w2', f=7)), arm(Fx(44, f=7), 'E', K('Y4')))
rx(arm(Fx(44, f=7), 'E', K('Y0')), arm(Fx(44, f=7), 'S', U3(W1, 1)))
# the far elbow steps along its row by itself
rx(B_(sw=K('Y0'), se=EL), B_(se=ELs('y0')))
rx(A_(nw=ELs('y0')), A_(ne=ELs('y1')))
rx((O, O, ELs('y1'), O), (O, O, O, ELs('y2')))
rx(A_(nw=ELs('y2')), A_(nw=ELs('y3')))
rx(B_(se=ELs('y3')), B_(sw=K('Y4'), se=EL))
# p east once, then once for each fork of q, walked by the side ray
rx(arm(Fx(44, f=7), 'S', WD('S', W1)), arm(Fx(45, f=7), 'S', VB0('S', 'PE')))
rx(arm(Fx(45, f=7), 'S', PVm('VE')), (Fx(32, f=7), O, O, RH('W', E1)))
frR(cell(MIR, RQ, 1, 0, 0, 'U', E1, 'd', 0, 3), 32, 7, Fx(32, 'e1', f=7))
rx(B_(se=Fx(32, 'e1', f=7)), B_(se=Fx(32, 'e2', f=7)))
rx(arm(Fx(32, 'e2', f=7)), arm(Fx(32, f=7), 'S', VB0('S', 'PE')))
rx(arm(Fx(32, f=7), 'S', PVm('VE')), arm(Fx(32, f=7), 'E', K('Y0')))
rx(arm(Fx(32, f=7), 'E', K('Y4')), (Fx(32, f=7), O, O, RH('U', E1, 1, 3)))
# along a row of catchers
CH = cell(MIR, CAT, 1, 0, 0, '_m', 0, 'd', 0, 0)
CH1 = cell(MIR, CA1, 2, 0, 0, '_m', 0, 'd', 0, 0)
rx(B_(ne=cell(VB, m='_m', d='d'), se=CC()), B_(se=CH))
rx((CH, CA1c(), O, O), (CC(), CH1, O, O))
rx((O, O, CH1, CC()), (O, O, CA1c(), CH))
rx((O, O, CA1c(), CC('e5')), (O, O, CA1c('e4'), CC()))
for m, st_ in (('PE', 'pe'), ('PC', 'pc')):
    rx((O, O, cell(MIR, CA1, 2, 0, 0, m, 0, 'd', 0, 0), nd(r=0, ro=0)), (O, O, CA1c(), nd(r=0, ro=0, m=st_, d='d')))
# the far elbow is in place: it comes down onto p's column; commit: the fork reads p by its east ray
frR(cell(MIR, RQ, 1, 0, 0, 'WD', E1, 'd', 0, 0), 32, 7, Fx(46, 'q0', f=8))
rx(B_(se=Fx(46, 'q0', f=8)), B_(se=Fx(46, 'q00', f=8)))
rx(arm(Fx(46, 'q00', f=8)), arm(Fx(46, f=8), 'E', VB0('E', 'PG')))
Qn = lambda m=0: nd(r=0, ro=0, m=m, d='d' if m else 0)
# the row of catchers goes, from p's end
rx(B_(ne=VB0('S', 'PG'), se=Qn()), B_(se=Qn('l0')))
rx(A_(nw=Qn('l0')), A_(nw=Qn('l1')))
rx((O, O, CA1c(), Qn('l1')), (O, O, CA1c('l2'), Qn()))
rx((CC(), CA1c('l2'), O, O), (CC('l3'), O, O, O))
rx((O, O, CA1c(), CC('l3')), (O, O, CA1c('l2'), O))
rx(B_(se=CC('l3')), B_(ne=PBm('LU')))
rx(A_(nw=Fx(46, f=8), sw=PBm('LU')), A_(nw=Fx(46, 'q1', f=8)))
rx(B_(se=Fx(46, 'q1', f=8)), B_(se=Fx(46, 'q2', f=8)))
# two catchers beside q; then the fork reads q by its south ray; then the flip
rx((Fx(46, 'q2', f=8), O, O, O), (Fx(46, f=8), O, O, RH('PQ')))
rx(B_(ne=VB0('S', 'PQ'), se=Qn()), B_(se=Qn('q1')))
rx(A_(nw=Qn('q1')), A_(nw=Qn('q2')))
rx(B_(se=Qn('q2')), B_(sw=CA1c(), se=Qn('q3')))
rx(A_(nw=Qn('q3')), A_(nw=Qn('q4')))
rx(B_(sw=CA1c(), se=Qn('q4')), B_(sw=CA1c('q5'), se=Qn()))
rx((O, CA1c('q5'), O, O), (CC('q6'), CA1c(), O, O))
rx(B_(se=CC('q6')), B_(se=CC('q7')))
rx((CC('q7'), CA1c(), O, O), (CC(), CA1c('q8'), O, O))
rx(B_(sw=CA1c('q8'), se=Qn()), B_(sw=CA1c(), se=Qn('q9')))
rx(A_(nw=Qn('q9')), A_(nw=Qn('q10')))
rx(B_(se=Qn('q10')), B_(ne=VB0('S', 'QR'), se=Qn()))
frR(cell(MIR, RQ, 1, 0, 0, 'QR', 0, 'd', 0, 0), 46, 8, Fx(47, 'q11', f=1))
rx(B_(se=Fx(47, 'q11', f=1)), B_(se=Fx(47, 'q12', f=0)))
rx((Fx(47, 'q12', f=0), O, O, RQs()), (Fx(47, 'q13', f=0), O, O, O))
rx(B_(se=Fx(47, 'q13', f=0)), B_(se=Fx(47, 'q14', f=0)))
rx(arm(Fx(47, 'q14', f=0)), arm(Fx(47), 'S', VB0('S', 'PH')))
# the side ray's cells go
rx((O, O, cell(MIR, CA1, 2, 0, 0, 'PH', 0, 'd', 0, 0), Qn()), (O, O, CA1c(), Qn('u1')))
rx(A_(nw=Qn('u1')), A_(nw=Qn('u2')))
rx(B_(se=Qn('u2')), B_(ne=PVm('DG'), se=Qn()))
rx(A_(nw=EL, sw=PVm('DG')), A_(nw=ELs('dg')))
rx(B_(sw=TQc(), se=ELs('dg')), B_(se=ELs('dh')))
rx(A_(nw=ELs('dh')), A_(sw=PBm('DI')))
rx(B_(ne=PBm('DI'), se=Qn()), B_(se=Qn('u3')))
rx(A_(nw=Qn('u3')), A_(nw=Qn('u4')))
rx((O, O, CA1c(), Qn('u4')), (O, O, CA1c('u5'), Qn()))
rx((CC(), CA1c('u5'), O, O), (CC('u6'), CA1c(), O, O))
rx(B_(se=CC('u6')), B_(ne=PVm('DJ'), se=CC()))
rx(arm(Fx(47), 'S', PVm('DJ')), arm(Fx(48), 'S', PVm('VE')))
# q steps west onto the fork's column, then comes up
rx(arm(Fx(48), 'S', VB0('S', 'PE')), arm(Fx(49), 'E', Wb('E', U1)))
rx(arm(Fx(49), 'E', U3e(U1, 0)), arm(Fx(49), 'S', VB0('S', 'XU')))
rx(arm(Fx(49), 'S', VB0('S', 'PDX')), arm(Fx(49), 'E', U3e(U1, 1)))
rx(arm(Fx(49), 'E', WD('E', U1)), arm(Fx(50), 'S', VB0('S', 'XU')))
rx(arm(Fx(50), 'S', VB0('S', 'PDX')), arm(Fx(51), 'S', VB0('S', 'XU')))
rx(arm(Fx(51), 'S', VB0('S', 'PDX')), arm(nd(m='sx', d='d', **FZ)))
rx(B_(se=nd(m='sx', d='d', **FZ)), B_(ne=ball('S', 'U', SWAP, 'd', 1, 1), se=nd(**FZ)))
# the walk: visits every node of a part in order; at some nodes it goes back to the fork for one action, and returns
Wn = lambda m, r=0, **kw: nd(r=r, ro=0, m=m, v='v', d='d', **kw)
rx(B_(ne=Wb('S'), se=nd(r=0, ro=0)), B_(se=Wn('wa')))
rx(B_(se=Wn('wc')), B_(ne=U3('v', 0), se=nd(r=0, ro=0)))
rx(A_(nw=Wn('wa', k=FK)), A_(nw=Wn('wc', k=FK)))
rx(B_(ne=U3('v', 1), se=nd(k=FK, r=0, ro=0)), B_(se=Wn('wd', k=FK)))
rx(A_(nw=Wn('wa', k=LF)), A_(nw=Wn('we', k=LF)))
rx(B_(se=Wn('we', k=LF)), B_(se=Wn('wd', k=LF)))
for v in (D1, U1):
    rx(A_(nw=nd(k=ST, r=0, ro=0, m='wa', v=v, d='d')), A_(nw=nd(k=ST, r=0, ro=0, m='wc', v=v, d='d')))
    rx(B_(ne=U3(v, 1), se=nd(k=ST, r=0, ro=0)), B_(se=nd(k=ST, r=0, ro=0, m='wd', v=v, d='d')))
for v in (W1, E1):
    rx(A_(nw=nd(k=ST, r=0, ro=0, m='wa', v=v, d='d')), A_(nw=nd(k=ST, r=0, ro=0, m='we', v=v, d='d')))
    rx(B_(se=nd(k=ST, r=0, ro=0, m='we', v=v, d='d')), B_(se=nd(k=ST, r=0, ro=0, m='wd', v=v, d='d')))
rx(A_(nw=Wn('wd', k=LF)), A_(nw=Wn('wu', k=LF)))
rx(arm(Wn('wd', k=ST)), arm(nd(k=ST, r=1, ro=0), 'S', Wb('S')))
rx(arm(Wn('wd', k=FK)), arm(nd(k=FK, r=1, ro=0), 'S', Wb('S')))
rx(arm(nd(k=ST, r=1, ro=0), 'S', WD('S')), arm(Wn('wu', k=ST)))
rx(arm(nd(k=FK, r=1, ro=0), 'S', WD('S')), arm(nd(k=FK, r=2, ro=0), 'E', Wb('E')))
rx(arm(nd(k=FK, r=2, ro=0), 'E', WD('E')), arm(Wn('wu', k=FK)))
rx(B_(se=Wn('wu')), B_(ne=WD('S'), se=nd(r=0, ro=0)))
sec('tri')
# TRI: F(F(a, b), c). The fork asks what the data's left part is; a leaf moves the end of the route to the right part.
TN = dict(k=FK, r=0)
def ut(ro, v, b, d='d', e=0): return nd(k=FK, r=0, ro=ro, m='ut', v=v, d=d, b=b, s=0, e=e)
Gb = lambda v, b, d='d': ball('S', 'G', v, d, b, 0)
rx(arm(nd(k=FK, r=0, ro=0), 'S', ball('S', 'PKF', 0, 'd', 0, 0)), arm(ut(7, QT, 1)))
rx(arm(nd(k=FK, r=0, ro=0), 'S', ball('S', 'PKF', 0, 'd', 1, 0)), arm(ut(8, QT, 1)))
for a in (AN, SF, RL):
    rx(arm(ut(7, a, 0)), arm(ut(8, a, 0)))
# left part a leaf: run a on the right part
rx(arm(ut(7, AL, 0)), arm(nd(k=FK, r=1, ro=9), 'S', Gb(RS, 0)))
rx(arm(nd(k=FK, r=1, ro=9), 'S', Gb(RS, 1)), arm(ut(8, AL, 0)))
# left part a stem: take the stem off, run b, put a stem back (direction kept aside in e)
rx(arm(ut(7, AS, 0)), arm(ut(10, LEFT, 1)))
rx(arm(ut(10, LEFT, 0)), arm(ut(11, STEM, 0, 0, 'd')))
rx(arm(ut(11, STEM, 1, 0, 'e')), arm(ut(12, LEFT, 0, 0, 'e')))
rx(arm(ut(12, LEFT, 1, 0, 'e')), arm(nd(k=FK, r=1, ro=13), 'S', Gb(RF, 0, 'e')))
rx(arm(nd(k=FK, r=1, ro=13), 'S', Gb(RF, 1)), arm(ut(14, LEFT, 1)))
rx(arm(ut(14, LEFT, 0)), arm(ut(15, STEM, 0, 1, 'd')))
rx(arm(ut(15, STEM, 1, 1, 'e')), arm(ut(16, LEFT, 0, 1, 'e')))
rx(arm(ut(16, LEFT, 1, 1, 'e')), arm(ut(8, AS, 0, 'e', 0)))
# left part a fork: c on the rotated data. c a leaf changes nothing.
rx(arm(ut(7, AF, 0)), arm(nd(k=FK, r=0, ro=17), 'E', ball('E', 'PKE', 0, 'd', 0, 0)))
rx(B_(ne=ball('S', 'PKE', 0, 'd', 0, 0), se=nd(k=LF, r=0)), B_(ne=ball('S', 'PKEL', 0, 'd', 0, 0), se=nd(k=LF, r=0)))
for k in (ST, FK):
    rx(B_(ne=ball('S', 'PKE', 0, 'd', 0, 0), se=nd(k=k, r=0)), B_(ne=ball('S', 'PKEN', 0, 'd', 0, 0), se=nd(k=k, r=0)))
    rx(B_(ne=ball('S', 'PKEM', 0, 'd', 0, 0), se=nd(k=k, r=0)), B_(ne=ball('S', 'PKEL', 0, 'd', 0, 0), se=nd(k=k, r=0)))
rx(arm(nd(k=FK, r=0, ro=17), 'E', ball('E', 'PKEL', 0, 'd', 0, 0)), arm(ut(8, AF, 0)))
rx(arm(nd(k=FK, r=0, ro=17), 'E', ball('E', 'PKEN', 0, 'd', 0, 0)), arm(ut(18, ROT, 0)))
rx(arm(ut(18, ROT, 1)), arm(nd(k=FK, r=2, ro=19), 'E', ball('E', 'C', 0, 'd', 0, 0)))
rx(arm(nd(k=FK, r=2, ro=19), 'E', ball('E', 'C', 0, 'd', 1, 0)), arm(ut(20, UR, 0)))
rx(arm(ut(20, UR, 1)), arm(nd(k=FK, r=0, ro=17), 'E', ball('E', 'PKEM', 0, 'd', 0, 0)))
# the data's side
turn(QT, 1, AN, 0, nd(k=LF, r=0))
QKb = lambda a, v='v': ball(a, 'QK', v, 'd', 0, 1)
for k in (ST, FK):
    rx(B_(ne=ball('S', 'U', QT, 'd', 1, 1), se=nd(k=k, r=0, ro=0)), B_(se=nd(k=k, r=0, ro=0, m='ta', v=0, d='d')))
    rx(arm(nd(k=k, r=0, m='ta', v='v', d='d')), arm(nd(k=k, r=0), 'S', QKb('S')))
for k, a in ((LF, AL), (ST, AS), (FK, AF)):
    rx(B_(ne=QKb('S', 0), se=nd(k=k, r=0)), B_(ne=QKb('S', a), se=nd(k=k, r=0)))
for a in (AS, AF):
    rx(B_(se=nd(k=FK, r=0, ro=0, m='ta', v=a, d='d')), B_(ne=ball('S', 'U', a, 'd', 0, 1), se=nd(k=FK, r=0, ro=0)))
rx(B_(se=nd(k=ST, r=0, ro=0, m='ta', v=AF, d='d')), B_(ne=ball('S', 'U', SF, 'd', 0, 1), se=nd(k=ST, r=0, ro=0)))
rx(B_(se=nd(k=ST, r=0, ro=0, m='ta', v=AL, d='d')), B_(ne=ball('S', 'U', AN, 'd', 0, 1), se=nd(k=ST, r=0, ro=0)))
rx(B_(se=nd(k=ST, r=0, ro=0, m='ta', v=AS, d='d')), B_(ne=ball('S', 'U', RL, 'd', 0, 1), se=nd(k=ST, r=0, ro=0)))
rx(B_(se=nd(k=FK, r=0, m='ta', v=AL, d='d')), B_(se=nd(k=FK, r=0, m='tb', v=AL, d='d')))
rx(arm(nd(k=FK, r=0, m='tb', v=AL, d='d')), arm(nd(k=FK, r=2), 'E', ball('E', 'UQ', AL, 'd', 0, 1)))
rx(B_(ne=ball('S', 'UQ', AL, 'd', 0, 1), se=nd(r=0)), B_(ne=ball('S', 'U', AL, 'd', 0, 1), se=nd(r=0)))
sec('fix and stack')
# FIX: S(F(L, b)). The fork's route value 3 marks a fixed point. SELF walks up to it, handing each route value to a stack in free
# space, and calls b again; when that call ends the same walk runs backward and rebuilds the route.
ZO, ZOF, ZV, ZF = 20, 21, 22, 23
rx(arm(nd(k=FK, r=0, ro=0), 'S', ball('S', 'PKL', 1, 'd', 0, 0)), arm(nd(k=FK, r=3, ro=22), 'E', ball('E', 'C', 0, 'd', 0, 0)))
rx(arm(nd(k=FK, r=3, ro=22), 'E', ball('E', 'C', 0, 'd', 1, 0)), arm(nd(k=FK, r=0, ro=0), 'S', ball('S', 'PKL', 1, 'd', 1, 0)))
rx(arm(nd(k=FK, r=3), 'E', ball('E', 'U', s=0)), arm(st('ut', k=FK, r=3, s=0)))
rx(B_(ne=ball('S', 'U', s=0), se=nd(k=FK, r=3)), B_(se=st('ut', k=FK, r=3, s=0)))
Uz = lambda a, v='v', **kw: ball(a, 'U', v, 'd', 'b', 2, **kw)
# s=2: bound for the stack
rx(arm(nd(k=ST, r=0, ro=0), 'S', Uz('S', **TV)), arm(nd(k=ST, r=0, ro=0, m='ut', v='v', d='d', b='b', s=2, **TV)))
rx(B_(ne=Uz('S', **TV), se=nd(k=ST, r=0)), B_(se=nd(k=ST, r=0, m='ut', v='v', d='d', b='b', s=2, **TV)))
rx(arm(nd(k=FK, r=3), 'E', Uz('E', **TV)), arm(nd(k=FK, r=3, m='ut', v='v', d='d', b='b', s=2, **TV)))
rx(B_(ne=Uz('S', **TV), se=nd(k=FK, r=3)), B_(se=nd(k=FK, r=3, m='ut', v='v', d='d', b='b', s=2, **TV)))
# the walk
Zb = lambda a: ball(a, 'Z', 0, 'd', 'b', 0)
rx(B_(ne=ball('S', 'C', 1, 'd', 'b', 0), se=nd(k=LF, r=0)), B_(ne=Zb('S'), se=nd(k=LF, r=0)))
rx(arm(nd(k=ST, r=0, ro=0), 'S', Zb('S')), arm(nd(k=ST, r=0, ro=0, m='zs', d='d', b='b')))
rx(B_(se=nd(k=ST, r=0, m='zs', d='d', b='b')), B_(ne=Zb('S'), se=nd(k=ST, r=0)))
for r, a in ((1, 'S'), (2, 'E'), (3, 'E')):
    rx(arm(nd(k=FK, r=r), a, Zb(a)), arm(nd(k=FK, r=r, m='zc', d='d', b='b')))
    rx(B_(se=nd(k=FK, r=r, m='zc', d='d', b='b')), B_(ne=Uz('S', ZOF if r == 3 else ZO, t1=r, t2='ro'), se=nd(k=FK, r=0, ro=0)))
rx(arm(nd(k=FK, r=0, ro=0, m='ut', v=ZV, d='d', b='b', s=2)), arm(nd(k=FK, r=0, ro=0, m='zu', d='d', b='b')))
rx(B_(se=nd(k=FK, r=0, ro=0, m='zu', d='d', b='b')), B_(ne=Zb('S'), se=nd(k=FK, r=0, ro=0)))
rx(B_(ne=Uz('S', ZV), se=nd(k=FK, r=0, ro=0)), B_(se=nd(k=FK, r=0, ro=0, m='ut', v=ZV, d='d', b='b', s=2)))
rx(B_(ne=Uz('S', ZF), se=nd(k=FK, r=0, ro=0)), B_(se=nd(k=FK, r=0, ro=0, m='ut', v=ZF, d='d', b='b', s=2)))
rx(arm(nd(k=FK, r=0, ro=0, m='ut', v=ZF, d='d', b=0, s=2)), arm(nd(k=FK, r=3, ro=23), 'E', ball('E', 'C', 0, 'd', 0, 0)))
rx(arm(nd(k=FK, r=3, ro=23), 'E', ball('E', 'C', 0, 'd', 1, 0)), arm(nd(k=FK, r=0, ro=0, m='ut', v=ZF, d='d', b=1, s=2)))
# the hub sends stack-bound balls on, as K balls that a passing mirror cannot mistake
for v, m in ((ZO, 'ZK'), (ZOF, 'ZK'), (ZV, 'ZR'), (ZF, 'ZR')):
    rx(A_(nw=HU, sw=Uz('S', v, **TV)), A_(nw=HU, ne=cell(KB, m=m, v=v, d='d', b='b', **TV)))
# the stack hub: calls pass straight through
SH = cell(MIR, SHUB)
SHH = cell(MIR, SHUB, 1, 0, 0, '_m', 'v', 'd', 'b', 's', **TV)
rx(B_(sw=HANY, se=SH), B_(se=SHH))
rx(A_(nw=SHH), A_(nw=SH, ne=HANY))
ZK = lambda v='v', **kw: cell(KB, m='ZK', v=v, d='d', b='b', **kw)
SS = lambda m, v='v', d='d', **kw: cell(MIR, SHUB, 0, 0, 0, m, v, d, 'b', 0, **kw)
rx(B_(sw=ZK(**TV), se=SH), B_(se=SS('z1', **TV)))
rx(A_(nw=SS('z1', **TV)), A_(nw=SS('z2', d=0, **TV), sw=ball('S', 'PDS', 0, 'd', 0, 0)))
rx(B_(ne=ball('S', 'PDS', 0, 'd', 0, 0), se=nd(r=0, ro=0, **TV)), B_(se=nd(r=0, ro=24, m='pd', d='d', **TV)))
rx(B_(se=nd(r=0, ro=24, m='m3', d='d', **TV)), B_(ne=cell(PB, m='TOPS', d='d'), se=nd(r=0, ro=0, **TV)))
rx(A_(sw=cell(PB, m='TOPS', d='d')), A_(nw=nd(k=ST, f=0, r=0, ro=0, m='ss', d='d')))
rx(B_(se=nd(k=ST, f=0, r=0, ro=0, m='ss', d='d')), B_(ne=ball('S', 'PSU', 0, 'd', 0, 0), se=nd(k=ST, f=0, r=0, ro=0)))
rx(A_(nw=SS('z2', d=0, **TV), sw=ball('S', 'PSU', 0, 'd', 0, 0)), A_(nw=SS('z3', **TV)))
rx(B_(se=SS('z3', **TV)), B_(se=SS('z4', **TV)))
rx(A_(nw=SS('z4', **TV)), A_(nw=SS('z5', d=0), sw=ball('S', 'TK', 0, 'd', 0, 0, **TV)))
rx(B_(ne=ball('S', 'TK', 0, 'd', 0, 0, **TV), se=nd(k=ST, f=0, r=0, ro=0)), B_(ne=ball('S', 'TL', 0, 'd', 0, 0), se=nd(k=ST, f=0, r=0, ro=0, **TV)))
rx(A_(nw=SS('z5', d=0), sw=ball('S', 'TL', 0, 'd', 0, 0)), A_(nw=SS('z6')))
for v0, v1 in ((ZO, ZV), (ZOF, ZF)):
    rx(B_(se=SS('z6', v=v0)), B_(sw=cell(KB, m='ZR', v=v1, d='d', b='b'), se=SH))

REACTIONS = R
RANGES = [8, 9, 9, 4, 152, len(_m) + 1, 32, 2, 2, 4, 2, 4, 152]
def _shape(rule):
    # a reaction up to the block's symmetries, its two sides, and token names: which corners hold a ball, a node, a mirror
    cl = lambda c: '.' if all(f == ('I', 0) for f in c) else {HB: 'b', VB: 'b', KB: 'b', PB: 'b', PV_: 'b', NODE: 'n', MIR: 'm'}.get(c[0][1], '?')
    best = None
    for side in (rule, rule[::-1]):
        a, b = [tuple(cl(c) for c in q) for q in side]
        for p in ((0, 1, 2, 3), (1, 0, 3, 2), (2, 3, 0, 1), (3, 2, 1, 0), (0, 2, 1, 3), (2, 0, 3, 1), (1, 3, 0, 2), (3, 1, 2, 0)):
            k = (tuple(a[i] for i in p), tuple(b[i] for i in p))
            if best is None or k < best: best = k
    return best
SHAPES = len(set(_shape(r) for r in R))

# layout: left part straight down, right part east then down
COL = 4
E2, S2, D1 = (2, 0), (0, 2), (1, 1)
def width(t):
    if t == 'L': return 2
    if t[0] == 'S': return width(t[1])
    return width(t[1]) + width(t[2])
def _c(*a): return tuple(a) + (0,) * (13 - len(a))
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
    xs = dp.w + dp.w % 2 + COL
    xd = xs + COL
    return Link((0, 2), (xd, 2), {(0, 0): _c(MIR, HUB, 0, 0, 0, m_('start'), 0, 1), (xd, 0): _c(MIR, DHUB), (xs, 0): _c(MIR, SHUB), (xs, 2): _c(NODE, LF)}, 0)
def node(cell, step):
    c = cell[0]
    if c in (VOID, HB, VB, KB, PB, PV_): return ('pass', step)
    if c == MIR:
        k = cell[1]
        if k == CAT: return ('pass', E) if step in (E, S) else None
        if k == CA1: return ('pass', step) if step in (E, S) else None
        if k == RQ: return ('pass', S)
        if k == TQ: return ('pass', E) if step == S else None
        if k == ELBOW: return ('pass', S) if step in (E, S, E2) else None
        if k == ELX: return ('pass', E) if step == E else ('pass', S) if step == E2 else None
        return None
    k = cell[1]
    if k == LF: return ('leaf',)
    if k == ST: return ('stem', S)
    f = cell[2]
    if f == 5 or step == S2 or (f == 4 and step == E): return ('pass', S)
    return (('fork', S, E), ('fork', E, S), None, ('fork', S2, E2), ('fork', S, E), None, None, ('fork', S, D1), ('fork', E, D1))[f]
