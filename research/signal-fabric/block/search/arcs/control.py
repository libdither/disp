"""The walk through program and data: counts, seq, dag, map. Every walk is undone by the same reactions on the way back."""
from kit import *

SELF, SWAP, CROSS, SINK, STEM, UNIT = 1, 2, 3, 4, 5, 6
Q, RL, RS, RF, NEXT, NX = 7, 8, 9, 10, 11, 12

R.tag = 'route'
U = lambda arm='S', **kw: ball(arm, 'U', **kw)
R.even(nd(r=1), nd(m='ut', r=1), 'S', U())
R.even(nd(r=2), nd(m='ut', r=2), 'E', U('E'))
R.even(nd(k=ST, r=0, ro=0), nd(m='ut', k=ST, r=0, ro=0, s=0), 'S', U(s=0))
R.odd(nd(r=1), nd(m='ut', r=1), 'N', U())
R.odd(nd(r=2), nd(m='ut', r=2), 'N', U())
R.odd(nd(k=ST, r=0), nd(m='ut', k=ST, r=0, s=0), 'N', U(s=0))
R.odd(nd(k=FK, r=0), nd(m='ut', k=FK, r=0, s=0), 'N', U(s=0))
R.odd(nd(k=LF, r=0), nd(k=LF, r=0), 'N', ball('S', 'C', s=0), 'N', U(s=0))

R.tag = 'count, peek'
C_ = lambda **kw: ball('S', 'C', **kw)
R.odd(nd(k=ST, r=0, ro=0), nd(m='c', k=ST, r=0, ro=0), 'N', C_())
for n in range(6):
    if n == STEM - 1:
        # a growing move run backward: past here the head says "backward" in b
        R.even(nd(m='c', v=n, d=1, k=ST, r=0, ro=0), nd(k=ST, r=0, ro=0), arm2='S', bl2=C_(v=n + 1, d=1))
        for b_ in (0, 1):
            R.even(nd(m='c', v=n, d=0, b=b_, k=ST, r=0, ro=0), nd(k=ST, r=0, ro=0), arm2='S', bl2=C_(v=n + 1, d=0, b=1 - b_))
        continue
    R.even(nd(m='c', v=n, k=ST, r=0, ro=0), nd(k=ST, r=0, ro=0), arm2='S', bl2=C_(v=n + 1))
R.odd(nd(k=FK, r=0, ro=0), nd(m='c', k=FK, r=0, ro=0), 'N', C_())
R.even(nd(m='c', k=FK, r=0, ro=0), nd(k=FK, r=0, ro=0), arm2='S', bl2=ball('S', 'PK'))
for k, m in ((LF, 'PKL'), (ST, 'PKS'), (FK, 'PKF')):
    R.odd(nd(k=k), nd(k=k), 'N', ball('S', 'PK'), 'N', ball('S', m))

R.tag = 'seq, dag'
F0 = dict(k=FK, ro=0)
def hop(a, ba, kw0, a2, bb, kw1):
    R.even(nd(**kw0), nd(**kw1), a, ba, a2, bb)
for d, (p, q) in ((1, (1, 2)), (0, (2, 1))):
    call = lambda t, bit: ('S', ball('S', 'XS', v=0, d=d, b=bit)) if t == 1 else ('E', ball('E', 'C', v=0, d=d, b=bit))
    a1, b1 = call(p, 0)
    a2, b2 = call(q, 0)
    r1, rb1 = call(p, 1)
    r2, rb2 = call(q, 1)
    hop('S', ball('S', 'PKS', v=0, d=d, b=0), dict(F0, r=0), a1, b1, dict(F0, r=p))
    hop(r1, rb1, dict(F0, r=p), a2, b2, dict(F0, r=q))
    hop(r2, rb2, dict(F0, r=q), 'S', ball('S', 'PKS', v=0, d=d, b=1), dict(F0, r=0))
R.odd(nd(k=ST, r=0, ro=0), nd(m='xs', k=ST, r=0, ro=0, v=0), 'N', ball('S', 'XS', v=0))
R.even(nd(m='xs', k=ST, r=0, ro=0, v=0), nd(k=ST, r=0, ro=0), arm2='S', bl2=C_(v=0))
for d in (0, 1):
    hop('S', ball('S', 'PKL', v=0, d=d, b=0), dict(F0, r=0), 'E', ball('E', 'C', v=0, d=1 - d, b=0), dict(k=FK, ro=1, r=2))
    hop('E', ball('E', 'C', v=0, d=1 - d, b=1), dict(k=FK, ro=1, r=2), 'S', ball('S', 'PKL', v=0, d=d, b=1), dict(F0, r=0))

R.tag = 'map'
ut = lambda ro, v, **kw: nd(m='ut', k=FK, ro=ro, r=0, v=v, s=0, **kw)
def c_(ro, r=0): return dict(k=FK, ro=ro, r=r)
R.even(nd(**c_(0)), ut(2, Q, b=1), 'S', ball('S', 'PKF', v=1, b=0, s=0))
R.even(nd(**c_(0)), ut(3, Q, b=1), 'S', ball('S', 'PKF', v=1, b=1, s=0))
R.even(ut(2, RL, b=0), ut(3, RL, b=0))
for a in (RS, RF):
    R.even(ut(2, a, b=0), nd(**c_(2, 1)), arm2='S', bl2=ball('S', 'G', v=a, b=0, s=0))
R.odd(nd(k=FK, r=0, ro=0), nd(m='g', k=FK, r=0, ro=0), 'N', ball('S', 'G'))
R.even(nd(m='g', k=FK, r=0, ro=0, v=RS, s=0), nd(k=FK, r=1, ro=6), arm2='S', bl2=ball('S', 'C', v=0, s=0))
R.even(nd(m='g', k=FK, r=0, ro=0, v=RF, s=0), nd(k=FK, r=2, ro=6), arm2='E', bl2=ball('E', 'C', v=0, s=0))
R.even(nd(**c_(2, 1)), ut(3, RS, b=0), 'S', ball('S', 'G', v=RS, b=1, s=0))
R.even(nd(**c_(2, 1)), ut(4, NEXT, b=0), 'S', ball('S', 'G', v=RF, b=1, s=0))
R.even(ut(4, NX, b=0), nd(**c_(2, 2)), arm2='E', bl2=ball('E', 'C', v=0, b=0, s=0))
R.even(nd(**c_(2, 2)), ut(5, NX, b=0), 'E', ball('E', 'C', v=0, b=1, s=0))
R.even(ut(5, NEXT, b=0), ut(3, RF, b=0))

R.tag = 'data side'
Ud = lambda v, b: ball('S', 'U', v=v, b=b, s=1)
def turn(v0, b0, v1, b1, n0, n1=None):
    R.odd(n0, n1 or n0, 'N', Ud(v0, b0), 'N', Ud(v1, b1))
turn(0, 0, 0, 1, nd(r=0))
for k in (LF, ST):
    turn(SWAP, 0, SWAP, 1, nd(k=k, r=0))
turn(SWAP, 0, SWAP, 1, nd(k=FK, f=0, r=0), nd(k=FK, f=1, r=0))
turn(SWAP, 1, SWAP, 0, nd(k=FK, f=0, r=0), nd(k=FK, f=1, r=0))
turn(Q, 1, RL, 0, nd(k=LF, r=0))
qs = lambda **kw: nd(m='q', v=Q, s=1, **kw)
for k in (ST, FK):
    R.odd(nd(k=k, r=0), qs(k=k, r=0), 'N', ball('S', 'U', v=Q, s=1))
UQ = lambda arm, a: ball(arm, 'UQ', v=a, s=1)
R.even(qs(k=ST, r=0), nd(k=ST, r=1), arm2='S', bl2=UQ('S', RS))
R.even(qs(k=FK, f=0, r=0), nd(k=FK, f=0, r=1), arm2='S', bl2=UQ('S', RF))
R.even(qs(k=FK, f=1, r=0), nd(k=FK, f=1, r=2), arm2='E', bl2=UQ('E', RF))
for a in (RS, RF, NX):
    R.odd(nd(r=0), nd(r=0), 'N', ball('S', 'UQ', v=a, b=1, s=1), 'N', ball('S', 'U', v=a, b=0, s=1))
R.odd(nd(r=0), nd(r=0), 'N', ball('S', 'U', v=NEXT, b=0, s=1), 'N', ball('S', 'PN', v=0, b=1, s=1))
R.even(nd(k=FK, f=0, r=1), nd(k=FK, f=0, r=2), 'S', ball('S', 'PN', v=0, b=1, s=1), 'E', ball('E', 'UQ', v=NX, b=1, s=1))
R.even(nd(k=FK, f=1, r=2), nd(k=FK, f=1, r=1), 'E', ball('E', 'PN', v=0, b=1, s=1), 'S', ball('S', 'UQ', v=NX, b=1, s=1))
