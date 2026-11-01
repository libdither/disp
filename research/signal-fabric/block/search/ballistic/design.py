"""Ballistic mirror machine. Matter: nodes and elbows on (even, even) sites; a link is a sight line through empties.
One ball (mode m, payload v, direction d, turned bit b, side s). Every walk is reversed by the same reactions: a ball that turns
retraces its path, undoing the walk, so a return needs no reactions of its own. b tells a fork a part has finished."""
from physics import I, A
from laws import Lay, Link

NAME = 'ballistic'
E, S, W, N = (1, 0), (0, 1), (-1, 0), (0, -1)
VOID, HB, VB, NODE, MIR, KB, PB, PV_ = 0, 1, 2, 3, 4, 5, 6, 7   # KB flies east-west and sees mirrors, PB does not fly
LF, ST, FK = 0, 1, 2
ELBOW, HUB, DHUB, CAT, ELT, XC, ELX, ELB, CB, RC = 0, 1, 2, 3, 4, 5, 6, 7, 8, 9
SELF, SWAP, CROSS, SINK, STEM, UNIT = 1, 2, 3, 4, 5, 6
Q, RL, RS, RF, NEXT, NX, QT, AN, AL, AS, AF, LEFT, VR, SF = 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 24

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
for v in (0, 2, 3, 5, 6):
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

sec('data ops: swap, ask, next')
# the data's side, s=1: what happens at the end of its route
def turn(v0, b0, v1, b1, n0, n1=None):
    rx(B_(ne=ball('S', 'U', v0, b=b0, s=1), se=n0), B_(ne=ball('S', 'U', v1, b=b1, s=1), se=n1 or n0))
turn(0, 0, 0, 1, nd(r=0))
for k in (LF, ST):
    turn(SWAP, 0, SWAP, 1, nd(k=k, r=0))
# a fork of two leaves stays as it is: F(L, x) then has one form, so undoing a unit is a function of the data alone
SB = lambda a, m: ball(a, m, 0, 'd', 0, 1)
for f in (0, 1):
    rx(B_(ne=ball('S', 'U', SWAP, b=0, s=1), se=nd(k=FK, f=f, r=0, ro=0)), B_(se=nd(k=FK, f=f, r=0, ro=0, m='sw', d='d')))
    rx(B_(se=nd(k=FK, f=f, r=0, ro=0, m='sx', d='d')), B_(ne=ball('S', 'U', SWAP, 'd', 1, 1), se=nd(k=FK, f=f, r=0, ro=0)))
    rx(arm(nd(k=FK, f=f, r=0), 'S', SB('S', 'S2')), arm(nd(k=FK, f=1 - f, r=0), 'S', SB('S', 'S3')))
    rx(arm(nd(k=FK, f=f, r=0), 'E', SB('E', 'E2')), arm(nd(k=FK, f=1 - f, r=0), 'E', SB('E', 'E3')))
rx(arm(nd(k=FK, r=0, m='sw', d='d')), arm(nd(k=FK, r=0), 'S', SB('S', 'S0')))
def refl(a, b, k): rx(B_(ne=SB('S', a), se=nd(k=k, r=0)), B_(ne=SB('S', b), se=nd(k=k, r=0)))
refl('S0', 'S1', LF); refl('S4', 'S9', LF); refl('E0', 'E1', LF)
for k in (ST, FK):
    refl('S0', 'S2', k); refl('S3', 'S9', k); refl('E0', 'E2', k); refl('E3', 'E1', k)
rx(arm(nd(k=FK, r=0), 'S', SB('S', 'S1')), arm(nd(k=FK, r=0), 'E', SB('E', 'E0')))
rx(arm(nd(k=FK, r=0), 'E', SB('E', 'E1')), arm(nd(k=FK, r=0), 'S', SB('S', 'S4')))
rx(arm(nd(k=FK, r=0), 'S', SB('S', 'S9')), arm(nd(k=FK, r=0, m='sx', d='d')))
# ask: a leaf answers; a stem or fork moves the end of the route into a part, and the part answers
turn(Q, 1, RL, 0, nd(k=LF, r=0))
for k in (ST, FK):
    rx(B_(ne=ball('S', 'U', Q, b='b', s=1), se=nd(k=k, r=0, ro=0)), B_(se=st('q', v=Q, s=1, k=k, r=0, ro=0)))
rx(arm(st('q', v=Q, s=1, k=ST, r=0, ro=0)), arm(nd(k=ST, r=1, ro=0), 'S', ball('S', 'UQ', RS, b='b', s=1)))
rx(arm(st('q', v=Q, s=1, k=FK, f=0, r=0, ro=0)), arm(nd(k=FK, f=0, r=1, ro=0), 'S', ball('S', 'UQ', RF, b='b', s=1)))
rx(arm(st('q', v=Q, s=1, k=FK, f=1, r=0, ro=0)), arm(nd(k=FK, f=1, r=2, ro=0), 'E', ball('E', 'UQ', RF, b='b', s=1)))
for a in (RS, RF, NX):
    rx(B_(ne=ball('S', 'UQ', a, b=1, s=1), se=nd(r=0)), B_(ne=ball('S', 'U', a, b=0, s=1), se=nd(r=0)))
# next: the end of the route moves from the left part to the right part, through the parent
rx(B_(ne=ball('S', 'U', NEXT, b=0, s=1), se=nd(r=0)), B_(ne=ball('S', 'PN', 0, b=1, s=1), se=nd(r=0)))
rx(arm(nd(k=FK, f=0, r=1, ro=0), 'S', ball('S', 'PN', 0, b=1, s=1)), arm(nd(k=FK, f=0, r=2, ro=0), 'E', ball('E', 'UQ', NX, b=1, s=1)))
rx(arm(nd(k=FK, f=1, r=2), 'E', ball('E', 'PN', 0, b=1, s=1)), arm(nd(k=FK, f=1, r=1), 'S', ball('S', 'UQ', NX, b=1, s=1)))



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
for ro in (0, 1, 5, 6, 24):
    rx(arm(nd(k=LF, r=0, ro=ro, m='pd', d='d')), A_(sw=nd(k=LF, r=0, ro=ro, m='m1', d='d')))
rx(arm(nd(k=LF, r=0, ro=3, m='pd', d='d')), A_(nw=EL, sw=nd(k=LF, r=0, ro=3, m='m1', d='d')))
rx((O, nd(r=0, m='m1', d='d', **TV), O, O), (O, O, O, nd(r=0, m='m2', d='d', **TV)))
for ro in (0, 1, 5, 6, 24):
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
rx((O, O, nd(r=0, m='e1', d='d'), O), (O, O, CC(), nd(r=0, m='e2', d='d')))
rx(A_(nw=nd(k=LF, r=0, m='e2', d='d')), A_(nw=nd(k=LF, r=0, m='e3', d='d')))
# the part below drops the catchers its parent no longer needs
rx((O, PVm('DL'), CC(), nd(r=0)), (O, O, O, nd(r=0, m='d1', d='d')))
rx(A_(nw=nd(r=0, m='d1', d='d')), A_(nw=nd(r=0, m='d2', d='d')))
rx(B_(se=nd(r=0, m='d2', d='d')), B_(sw=K('DK'), se=nd(r=0)))
rx(A_(nw=CC(), ne=K('DK')), A_(ne=cell(PB, m='DJ', d='d')))
rx(B_(sw=cell(PB, m='DJ', d='d'), se=nd(r=0)), B_(se=nd(r=0, m='d3', d='d')))
rx(A_(nw=nd(r=0, m='d3', d='d')), A_(nw=nd(r=0, m='d4', d='d')))
rx(B_(se=nd(r=0, m='d4', d='d')), B_(ne=PVm('DU'), se=nd(r=0)))
# back to the parent through the catchers
rx((O, O, CC(), nd(r=0, ro=0, m='e3', d='d')), (O, O, CC('e4'), nd(r=0, ro=0)))
rx((CC(), CC('e4'), O, O), (CC('e5'), CC(), O, O))
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
rx((O, O, CC(), nd(r=0, ro=4, m='e3', d='d')), (O, O, CC('t1'), nd(r=0, ro=0)))
rx((CC(), CC('t1'), O, O), (CC('t2'), CC(), O, O))
rx(B_(se=CC('t2')), B_(se=CC('t3')))
rx((CC('t3'), CC(), O, O), (cell(MIR, CAT, 0, 0, 0, 't4'), CC(), PBm('ML'), O))
rx((O, PBm('ML'), O, O), (O, O, O, LFc('mk')))
rx(A_(nw=LFc('mk')), A_(nw=LFc('mr')))
rx(B_(se=LFc('mr')), B_(ne=PBm('MC'), se=LFc()))
rx((cell(MIR, CAT, 0, 0, 0, 't4'), CC(), PBm('MC'), O), (FKc('fa'), CC(), O, O))
rx(B_(se=FKc('fa')), B_(se=FKc('fb')))
rx((FKc('fb'), CC(), O, O), (FKc(), CC('t9'), O, O))
rx((O, O, CC('t9'), nd(r=0, ro=0)), (O, O, O, nd(r=0, ro=3, m='pd', d='d')))
rx(B_(se=nd(r=0, ro=3, m='m3', d='d')), B_(ne=ball('S', 'FU', 0, 'd', 0, 0), se=nd(r=0, ro=0)))
rx(arm(FKc(), 'E', ball('E', 'FU', 0, 'd', 0, 0)), arm(FKc('nu')))
rx(B_(se=nd(k=FK, f=0, r=0, ro=0, m='nu', d=1)), B_(ne=Us(1, 1, UNIT), se=FKc()))
rx(B_(ne=Us(0, 0, UNIT), se=FKc()), B_(se=nd(k=FK, f=0, r=0, ro=0, m='nv', d=0)))
rx(arm(nd(k=FK, f=0, r=0, ro=0, m='nv', d=0)), arm(FKc(), 'S', cell(PV_, m='UK')))
rx(B_(ne=cell(PV_, m='UK'), se=LFc()), B_(ne=cell(PV_, m='UL'), se=LFc()))
rx(arm(FKc(), 'S', cell(PV_, m='UL')), arm(nd(k=FK, f=0, r=0, ro=0, m='nw', d=0)))
rx(B_(se=nd(k=FK, f=0, r=0, ro=0, m='nw', d=0)), B_(se=nd(k=FK, f=0, r=0, ro=0, m='nu', d=0)))

sec('sink, by the leaf')
# helpers: a state that keeps d and b
def ns(m, **kw): return nd(m=m, d='d', b='b', **kw)
def ms(k, m): return cell(MIR, k, 0, 0, 0, m, 0, 'd', 'b', 0)
Kb = lambda m: K(m, 'd', 'b')
Pb = lambda m: PVm(m, 'b')
PBb = lambda m: cell(PB, m=m, d='d', b='b')
Ub = lambda v, b: cell(VB, m='U', v=v, d='d', b=b, s=1)
EL0 = lambda k: cell(MIR, k)
FZ = dict(k=FK, f=0, r=0, ro=0)
SZ = dict(k=ST, r=0, ro=0)

# SINK: the leaf at the end of the count runs it as a little program, one trip to the data per step: stem off here, stem on there.
Ul = lambda v, b, d='d': ball('S', 'U', v, d, b, 0)
def step(r0, v0, b0, r1, v1, b1, d0='d', d1='d', e0=0, e1=0):
    rx(B_(ne=Ul(v0, b0, d0), se=nd(k=LF, r=0, ro=r0, e=e0)), B_(ne=Ul(v1, b1, d1), se=nd(k=LF, r=0, ro=r1, e=e1)))
rx(B_(ne=ball('S', 'C', SINK, 'd', 0, 0), se=nd(k=LF, r=0, ro=0)), B_(ne=Ul(QT, 1), se=nd(k=LF, r=0, ro=11)))
rx(B_(ne=Ul(QT, 1), se=nd(k=LF, r=0, ro=19)), B_(ne=ball('S', 'C', SINK, 'd', 1, 0), se=nd(k=LF, r=0, ro=0)))
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

sec('cross')
# CROSS. Rays in place, a single commit: the fork's flag makes it read its left child through a longer step, and that step
# passes each elbow cell differently. Prep: the far part drops under a second elbow; an elbow appears over the near part.
turn(CROSS, 0, CROSS, 1, nd(k=LF, r=0))
turn(CROSS, 0, CROSS, 1, nd(k=ST, r=0))
turn(SWAP, 0, SWAP, 1, nd(k=FK, f=2, r=0), nd(k=FK, f=3, r=0))
turn(SWAP, 1, SWAP, 0, nd(k=FK, f=2, r=0), nd(k=FK, f=3, r=0))
rx(B_(ne=Ub(CROSS, 0), se=nd(**FZ)), B_(se=nd(m='cz', d='d', **FZ)))
rx(arm(nd(m='cz', d='d', **FZ)), arm(nd(**FZ), 'S', PVm('C0')))
rx(B_(ne=PVm('C0'), se=nd(**FZ)), B_(ne=PVm('C3'), se=nd(**FZ)))
rx(B_(ne=PVm('C3', 1), se=nd(**FZ)), B_(ne=PVm('C1', 1), se=nd(**FZ)))
for kw in (dict(k=LF, r=0, ro=0), dict(k=ST, r=0, ro=0)):
    rx(B_(ne=PVm('C0'), se=nd(**kw)), B_(ne=PVm('C1', 1), se=nd(**kw)))
rx(arm(nd(**FZ), 'S', PVm('C1', 1)), arm(nd(m='cx', d='d', k=FK, r=0, ro=0, f=0)))
rx(B_(se=nd(m='cx', d='d', **FZ)), B_(ne=Ub(CROSS, 1), se=nd(**FZ)))
rx(arm(nd(**FZ), 'S', Pb('C3')), arm(ns('ca', **FZ)))
# a flipped fork whose left part (east) is no fork: nothing to do
for f in (1, 3):
    rx(B_(ne=Ub(CROSS, 0), se=nd(k=FK, f=f, r=0, ro=0)), B_(se=nd(m='cy', d='d', k=FK, f=f, r=0, ro=0)))
    rx(arm(nd(m='cy', d='d', k=FK, f=f, r=0, ro=0)), arm(nd(k=FK, f=f, r=0, ro=0), 'E', ball('E', 'CY', 0, 'd', 0, 1)))
    rx(arm(nd(k=FK, f=f, r=0, ro=0), 'E', ball('E', 'CY', 0, 'd', 1, 1)), arm(nd(m='cw', d='d', k=FK, f=f, r=0, ro=0)))
    rx(B_(se=nd(m='cw', d='d', k=FK, f=f, r=0, ro=0)), B_(ne=Ub(CROSS, 1), se=nd(k=FK, f=f, r=0, ro=0)))
for k in (LF, ST):
    rx(B_(ne=ball('S', 'CY', 0, 'd', 0, 1), se=nd(k=k, r=0, ro=0)), B_(ne=ball('S', 'CY', 0, 'd', 1, 1), se=nd(k=k, r=0, ro=0)))
rx(B_(ne=VANY, se=EL0(XC)), B_(se=cell(MIR, XC, 1, 0, 0, '_m', 'v', 'd', 'b', 's', **TV)))
rx(A_(nw=cell(MIR, XC, 1, 0, 0, '_m', 'v', 'd', 'b', 's', **TV)), A_(nw=EL0(XC), sw=VANY))
rx(B_(sw=HANY, se=EL0(ELX)), B_(se=cell(MIR, ELX, 1, 0, 0, '_m', 'v', 'd', 'b', 's', **TV)))
rx(A_(nw=cell(MIR, ELX, 1, 0, 0, '_m', 'v', 'd', 'b', 's', **TV)), A_(nw=EL0(ELX), sw=VANY))
# the path
rx(B_(se=ns('ca', **FZ)), B_(se=nd(m='cb', d='d', b='b', e=0, **FZ)))
FE = dict(FZ, e='e')
rx(B_(se=ns('ca', **FZ)), B_(se=ns('cb', **FZ))) if False else None
rx(arm(ns('cb', **FE)), arm(nd(**FE), 'E', ball('E', 'PDT', 0, 'd', 'b', 0)))
for b, p in ((0, 5), (1, 6)):
    rx(B_(ne=ball('S', 'PDT', 0, 'd', b, 0), se=nd(r=0, ro=0)), B_(se=nd(r=0, ro=p, m='pd', d='d')))
    rx(B_(se=nd(r=0, ro=p, m='m3', d='d')), B_(ne=cell(PB, m='TE', d='d', b=b), se=nd(r=0, ro=0)))
rx(A_(sw=PBb('TE')), A_(nw=ms(ELBOW, 'c1')))
rx(B_(se=ms(ELBOW, 'c1')), B_(ne=ball('S', 'CU', 0, 'd', 'b', 0), se=EL))
rx(arm(nd(**FE), 'E', ball('E', 'CU', 0, 'd', 'b', 0)), arm(nd(**FE), 'S', Pb('C5')))
rx(B_(ne=Pb('C5'), se=nd(**FZ)), B_(se=ns('c2', **FZ)))
rx(arm(ns('c2', **FZ)), arm(nd(**FZ), 'E', Kb('C6')))
rx(B_(sw=Kb('C6'), se=EL), B_(se=ms(XC, 'c3')))
rx(A_(nw=ms(XC, 'c3')), A_(nw=ms(XC, 'c4')))
rx(B_(se=ms(XC, 'c4')), B_(ne=PBb('C7'), se=EL0(XC)))
rx(A_(sw=PBb('C7')), A_(nw=ms(ELX, 'c5')))
rx(B_(se=ms(ELX, 'c5')), B_(sw=Kb('C8'), se=EL0(ELX)))
rx(arm(nd(**FE), 'E', Kb('C8')), arm(ns('c6', k=FK, f=2, r=0, ro=0, e='e')))
rx(B_(se=nd(m='c6', d='d', b=0, k=FK, f=2, r=0, ro=0)), B_(ne=Ub(CROSS, 1), se=nd(k=FK, f=2, r=0, ro=0)))
rx(B_(ne=Ub(CROSS, 0), se=nd(k=FK, f=2, r=0, ro=0)), B_(se=nd(m='c6', d='d', b=1, k=FK, f=2, r=0, ro=0)))

# a flipped fork whose left part (east) is a fork: the inner fork reads back along its ray to the fork's south part,
# and the outer reads the inner's east part along the free row below it. All prep is out of sight; one cell commits.
Y0 = dict(k=FK, f=0, r=0, ro=0)
Y4 = dict(k=FK, f=4, r=0, ro=0)
Y1 = dict(k=FK, f=1, r=0, ro=0)
Y5 = dict(k=FK, f=5, r=0, ro=0)
rx(B_(ne=ball('S', 'CY', 0, 'd', 'b', 1), se=nd(**Y0)), B_(se=ns('y1', **Y4)))
rx(arm(ns('y1', **Y4)), arm(nd(**Y4), 'E', Kb('Y2')))
rx(B_(sw=Kb('Y2'), se=EL), B_(se=ms(ELBOW, 'y3')))
rx(A_(nw=ms(ELBOW, 'y3')), A_(nw=ms(ELBOW, 'y4')))
rx(B_(se=ms(ELBOW, 'y4')), B_(ne=EL0(CB), se=ms(ELBOW, 'y5')))
rx(A_(nw=ms(ELBOW, 'y5')), A_(nw=ms(ELBOW, 'y6')))
rx(B_(ne=EL0(CB), se=ms(ELBOW, 'y6')), B_(ne=EL0(CB), sw=Kb('Y7'), se=EL))
rx(arm(nd(**Y4), 'E', Kb('Y7')), arm(ns('y8', **Y4)))
rx(B_(se=ns('y8', **Y4)), B_(ne=Pb('Y9'), se=nd(**Y4)))
rx(A_(nw=EL, sw=Pb('Y9')), A_(nw=ms(ELB, 'y10')))
rx(B_(se=ms(ELB, 'y10')), B_(sw=Kb('Y11'), se=EL0(ELB)))
rx(arm(nd(**Y1), 'E', Kb('Y11')), arm(ns('y12', **Y1)))
rx(B_(se=ns('y12', **Y1)), B_(se=ns('y13', **Y1)))
rx((ns('y13', **Y1), O, O, O), (ns('y14', **Y5), O, O, EL0(RC)))
rx(B_(se=nd(m='y14', d='d', b=0, **Y5)), B_(ne=Ub(CROSS, 1), se=nd(**Y5)))
rx(B_(ne=Ub(CROSS, 0), se=nd(**Y5)), B_(se=nd(m='y14', d='d', b=1, **Y5)))

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
rx(arm(nd(k=FK, r=0, ro=17), 'E', ball('E', 'PKEL', 0, 'd', 0, 0)), arm(ut(8, AF, 0)))
rx(arm(nd(k=FK, r=0, ro=17), 'E', ball('E', 'PKEN', 0, 'd', 0, 0)), arm(ut(18, VR, 0)))
rx(arm(ut(18, VR, 1)), arm(nd(k=FK, r=2, ro=19), 'E', ball('E', 'C', 0, 'd', 0, 0)))
rx(arm(nd(k=FK, r=2, ro=19), 'E', ball('E', 'C', 0, 'd', 1, 0)), arm(ut(20, VR, 1)))
rx(arm(ut(20, VR, 0)), arm(nd(k=FK, r=0, ro=17), 'E', ball('E', 'PKEM', 0, 'd', 0, 0)))
for k in (ST, FK):
    rx(B_(ne=ball('S', 'PKEM', 0, 'd', 0, 0), se=nd(k=k, r=0)), B_(ne=ball('S', 'PKEL', 0, 'd', 0, 0), se=nd(k=k, r=0)))
# the data's side
turn(QT, 1, AN, 0, nd(k=LF, r=0))
rx(B_(ne=ball('S', 'U', QT, 'd', 1, 1), se=nd(k=ST, r=0, ro=0)), B_(se=nd(k=ST, r=0, ro=0, m='ta', v=0, d='d')))
rx(arm(nd(k=ST, r=0, m='ta', v='v', d='d')), arm(nd(k=ST, r=0), 'S', ball('S', 'QK', 'v', 'd', 0, 1)))
for a, b_ in ((AF, SF), (AL, AN), (AS, RL)):
    rx(B_(se=nd(k=ST, r=0, ro=0, m='ta', v=a, d='d')), B_(ne=ball('S', 'U', b_, 'd', 0, 1), se=nd(k=ST, r=0, ro=0)))
QKb = lambda a, v='v': ball(a, 'QK', v, 'd', 0, 1)
rx(B_(ne=ball('S', 'U', QT, 'd', 1, 1), se=nd(k=FK, r=0, ro=0)), B_(se=nd(k=FK, r=0, ro=0, m='ta', v=0, d='d')))
for f, a in ((0, 'S'), (2, 'S'), (1, 'E')):
    rx(arm(nd(k=FK, f=f, r=0, m='ta', v='v', d='d')), arm(nd(k=FK, f=f, r=0), a, QKb(a)))
for k, a in ((LF, AL), (ST, AS), (FK, AF)):
    rx(B_(ne=QKb('S', 0), se=nd(k=k, r=0)), B_(ne=QKb('S', a), se=nd(k=k, r=0)))
for a in (AS, AF):
    rx(B_(se=nd(k=FK, r=0, ro=0, m='ta', v=a, d='d')), B_(ne=ball('S', 'U', a, 'd', 0, 1), se=nd(k=FK, r=0, ro=0)))
rx(B_(se=nd(k=FK, r=0, m='ta', v=AL, d='d')), B_(se=nd(k=FK, r=0, m='tb', v=AL, d='d')))
rx(arm(nd(k=FK, f=0, r=0, m='tb', v=AL, d='d')), arm(nd(k=FK, f=0, r=2), 'E', ball('E', 'UQ', AL, 'd', 0, 1)))
rx(arm(nd(k=FK, f=1, r=0, m='tb', v=AL, d='d')), arm(nd(k=FK, f=1, r=1), 'S', ball('S', 'UQ', AL, 'd', 0, 1)))
rx(B_(ne=ball('S', 'UQ', AL, 'd', 0, 1), se=nd(r=0)), B_(ne=ball('S', 'U', AL, 'd', 0, 1), se=nd(r=0)))
# the end of the route steps into the left part and back
rx(B_(ne=ball('S', 'U', LEFT, 'd', 1, 1), se=nd(k=FK, r=0, ro=0)), B_(se=nd(k=FK, r=0, ro=0, m='lf', d='d')))
rx(arm(nd(k=FK, f=0, r=0, ro=0, m='lf', d='d')), arm(nd(k=FK, f=0, r=1, ro=0), 'S', ball('S', 'UQ', LEFT, 'd', 1, 1)))
rx(arm(nd(k=FK, f=1, r=0, ro=0, m='lf', d='d')), arm(nd(k=FK, f=1, r=2, ro=0), 'E', ball('E', 'UQ', LEFT, 'd', 1, 1)))
rx(B_(ne=ball('S', 'UQ', LEFT, 'd', 1, 1), se=nd(r=0)), B_(ne=ball('S', 'U', LEFT, 'd', 0, 1), se=nd(r=0)))
# read as rotated (mark 7): the left part is one fork further down; the right part is the fork itself (route value 3).
rx(B_(ne=ball('S', 'U', VR, 'd', 0, 1), se=nd(k=FK, r=0, ro=0)), B_(ne=ball('S', 'U', VR, 'd', 1, 1), se=nd(k=FK, r=0, ro=7)))
V7 = dict(k=FK, ro=7)
rx(B_(ne=ball('S', 'U', Q, 'd', 'b', 1), se=nd(r=0, **V7)), B_(se=nd(r=0, m='vq', v=Q, d='d', b='b', s=1, **V7)))
rx(arm(nd(r=0, m='vq', v=Q, d='d', b='b', s=1, **V7)), arm(nd(r=1, **V7), 'S', ball('S', 'VQ', RF, 'd', 'b', 1)))
rx(B_(ne=ball('S', 'VQ', RF, 'd', 'b', 1), se=nd(k=FK, f=0, r=0, ro=0)), B_(se=nd(k=FK, f=0, r=0, ro=0, m='vr', v=RF, d='d', b='b', s=1)))
rx(arm(nd(k=FK, f=0, r=0, ro=0, m='vr', v=RF, d='d', b='b', s=1)), arm(nd(k=FK, f=0, r=1, ro=7), 'S', ball('S', 'UQ', RF, 'd', 'b', 1)))
rx(arm(nd(k=FK, f=0, r=1, ro=7), 'S', ball('S', 'PN', 0, 'd', 1, 1)), arm(nd(k=FK, f=0, r=0, ro=0, m='vn', d='d')))
rx(B_(se=nd(k=FK, f=0, r=0, ro=0, m='vn', d='d')), B_(ne=ball('S', 'VN', 0, 'd', 1, 1), se=nd(k=FK, f=0, r=0, ro=0)))
rx(arm(nd(r=1, **V7), 'S', ball('S', 'VN', 0, 'd', 1, 1)), arm(nd(r=3, m='vu', v=NX, d='d', s=1, **V7)))
rx(B_(se=nd(r=3, m='vu', v=NX, d='d', s=1, **V7)), B_(ne=ball('S', 'U', NX, 'd', 0, 1), se=nd(r=3, **V7)))
# swap of that right part is a cross of the real fork
rx(B_(ne=ball('S', 'U', SWAP, 'd', 0, 1), se=nd(k=FK, f=0, r=3, ro=7)), B_(se=nd(m='cb', d='d', b=0, e=1, **FZ)))
rx(B_(se=nd(m='c6', d='d', b=0, e=1, k=FK, f=2, r=0, ro=0)), B_(ne=ball('S', 'U', SWAP, 'd', 1, 1), se=nd(k=FK, f=2, r=3, ro=7)))

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
SH = cell(MIR, 3 + 7)
SHH = cell(MIR, 3 + 7, 1, 0, 0, '_m', 'v', 'd', 'b', 's', **TV)
rx(B_(sw=HANY, se=SH), B_(se=SHH))
rx(A_(nw=SHH), A_(nw=SH, ne=HANY))
ZK = lambda v='v', **kw: cell(KB, m='ZK', v=v, d='d', b='b', **kw)
SS = lambda m, v='v', d='d', **kw: cell(MIR, 3 + 7, 0, 0, 0, m, v, d, 'b', 0, **kw)
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

# reactions no rung ever touches, and the numbers tokens can share: two names meet only in different kinds of cell (tables from pack.py)
DROP = [2, 3, 225, 283]
PM = {0: 0, 57: 1, 59: 2, 62: 3, 34: 4, 36: 5, 37: 6, 38: 7, 56: 8, 8: 9, 53: 10, 2: 0, 3: 1, 9: 11, 22: 2, 23: 3, 28: 4, 29: 5, 30: 12, 31: 6, 32: 7, 35: 8, 52: 9, 54: 10, 82: 11, 95: 12, 98: 13, 101: 14, 125: 15, 126: 16, 127: 17, 128: 18, 129: 13, 130: 19, 139: 20, 1: 1, 4: 2, 5: 21, 6: 22, 7: 1, 10: 23, 11: 24, 12: 25, 13: 26, 14: 27, 15: 14, 16: 28, 17: 14, 18: 15, 19: 16, 20: 29, 21: 30, 24: 31, 25: 32, 26: 33, 27: 34, 33: 0, 39: 0, 40: 15, 41: 4, 42: 5, 43: 0, 44: 1, 45: 6, 46: 7, 47: 1, 48: 2, 49: 8, 50: 9, 51: 2, 55: 3, 58: 1, 60: 2, 61: 3, 63: 17, 64: 18, 65: 4, 66: 3, 67: 19, 68: 20, 69: 1, 70: 2, 71: 3, 72: 4, 73: 5, 74: 6, 75: 4, 76: 11, 77: 12, 78: 5, 79: 21, 80: 22, 81: 7, 83: 23, 84: 24, 85: 4, 86: 5, 87: 25, 88: 26, 89: 6, 90: 7, 91: 8, 92: 27, 93: 28, 94: 29, 96: 30, 97: 31, 99: 6, 100: 10, 102: 9, 103: 32, 104: 5, 105: 1, 106: 2, 107: 7, 108: 1, 109: 6, 110: 33, 111: 34, 112: 7, 113: 11, 114: 12, 115: 13, 116: 14, 117: 8, 118: 35, 119: 10, 120: 1, 121: 9, 122: 36, 123: 37, 124: 38, 131: 39, 132: 40, 133: 41, 134: 35, 135: 42, 136: 43, 137: 36, 138: 44, 140: 16, 141: 45, 142: 46, 143: 10, 144: 11, 145: 1, 146: 2, 147: 37, 148: 8, 149: 21, 150: 38, 151: 3, 152: 4, 153: 5, 154: 39, 155: 40, 156: 6}
PV = {0: 0, 10: 1, 15: 2, 9: 3, 12: 4, 5: 5, 1: 1, 7: 6, 17: 7, 16: 8, 18: 9, 2: 10, 3: 11, 20: 12, 21: 13, 22: 14, 23: 15, 6: 16, 4: 2, 8: 17, 11: 18, 13: 19, 14: 20, 24: 21, 19: 22}
PR = {0: 0, 1: 1, 3: 2, 6: 3, 5: 4, 24: 5, 4: 6, 11: 7, 19: 8, 12: 9, 13: 10, 14: 11, 15: 12, 16: 13, 17: 14, 18: 15, 20: 16, 2: 17, 7: 18, 8: 19, 9: 20, 10: 21, 22: 22, 23: 23}
NM, NV, NR = 47 , 23 , 24
def _pk(c):
    c = list(c)
    if c[5][0] == 'I': c[5] = I(PM.get(c[5][1], c[5][1]))
    if c[6][0] == 'I': c[6] = I(PV.get(c[6][1], c[6][1]))
    if c[4][0] == 'I': c[4] = I(PR.get(c[4][1], c[4][1]))
    return tuple(c)
R = [(tuple(_pk(c) for c in l), tuple(_pk(c) for c in r)) for i, (l, r) in enumerate(R) if i not in DROP]
_m = {k: PM.get(v, v) for k, v in _m.items()}

REACTIONS = R
RANGES = [8, 11, 6, 4, NR, NM, NV, 2, 2, 3, 2, 4, NR]
def _shape(rule):
    """a reaction up to the block's turns and mirrors, its two sides, and token names: per corner a ball, a node or a mirror, and where each stays"""
    def cl(c):
        if all(f == ('I', 0) for f in c): return '.'
        return {HB: 'b', VB: 'b', KB: 'b', PB: 'b', PV_: 'b', NODE: 'n', MIR: 'm'}[c[0][1]]
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
E2, S2, N2, W2, D1 = (2, 0), (0, 2), (0, -2), (-2, 0), (1, 1)
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
    return Link((0, 2), (xd, 2), {(0, 0): _c(MIR, HUB, 0, 0, 0, _m['start'], 0, 1), (xd, 0): _c(MIR, DHUB), (xs, 0): _c(MIR, 10), (xs, 2): _c(NODE, LF)}, 0)
def node(cell, step):
    c = cell[0]
    if c in (VOID, HB, VB, KB, PB, PV_): return ('pass', step)
    if c == MIR:
        k = cell[1]
        if k == CAT: return ('pass', E) if step in (E, S) else None
        if k in (ELBOW, ELT): return ('pass', S) if step in (E, S, E2) else None
        if k == XC: return ('pass', E2) if step == E2 else ('pass', S) if step in (E, S) else None
        if k == ELX: return ('pass', E) if step == E else ('pass', S) if step == E2 else None
        if k == ELB:
            if step == E2: return ('pass', S2)
            if step == N2: return ('pass', W2)
            return ('pass', S) if step in (E, S) else None
        if k == CB: return ('pass', S) if step == E else None
        if k == RC: return ('pass', E)
        return None
    k = cell[1]
    if k == LF: return ('leaf',)
    if k == ST: return ('stem', S)
    f = cell[2]
    if step == W2: return ('pass', S)
    if step == S2: return ('fork', S, N2) if f == 4 else ('fork', S, E2) if f == 0 else ('fork', E2, S)
    return (('fork', S, E), ('fork', E, S), ('fork', S2, E2), ('fork', E2, S2), ('fork', S, E), ('fork', E2, D1))[f]
def head_on(c): return c[5] != 0
