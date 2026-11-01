"""sink and cross. sink is real row surgery: take the stem off the top and put one on the left part. cross trades the two forks' arcs:
write the lower row's new elbow, flip two elbows at once through the head between them, erase the old one."""
from kit import *
from control import SINK, CROSS
from grow import load, row, held, pb, el
SK2, SK3 = 15, 16

R.tag = 'peek'
fk = dict(k=FK, c=1, h=1, a=0, t=0, r=0, ro=0)
for k in (LF, ST):
    R.odd(nd(k=k), nd(k=k), 'N', ball('S', 'PQ', b=0, s=1), 'N', ball('S', 'PQ', b=1, s=1))
R.odd(nd(k=FK), nd(k=FK), 'N', ball('S', 'PQ', v=SINK, s=1), 'N', ball('S', 'PQF', v=SINK, s=1))
R.odd(nd(k=FK, f=0), nd(k=FK, f=0), 'N', ball('S', 'PQ', v=CROSS, s=1), 'N', ball('S', 'PQF', v=CROSS, s=1))

R.tag = 'sink'
def turn0(v, k):
    R.odd(nd(k=k, r=0), nd(k=k, r=0), 'N', ball('S', 'U', v=v, b=0, s=1), 'N', ball('S', 'U', v=v, b=1, s=1))
turn0(SINK, LF)
sk = dict(k=ST, c=0, f=0, h=0, a=0, t=0, r=0, ro=0)
sh = dict(v=SINK, s=1)
R.odd(nd(**sk), nd(m='SA', **sh, **sk), 'N', ball('S', 'U', **sh))
R.even(nd(m='SA', **sh, **sk), nd(**sk), arm2='S', bl2=ball('S', 'PQ', **sh))
R.even(nd(**sk), nd(m='SB', **sh, **sk), 'S', ball('S', 'PQF', **sh))
R.odd(nd(m='SB', **sh, **sk), nd(m='FL', z=1, **sh, **dict(sk, t=1)))
# the top stem is gone: the fork is the pointer; put a stem on its left part
R.even(held('X1', v=SINK, s=1), row(k=ST, f=0, c=0, h=0, a='h', t=1), arm2='S', bl2=load(pk='k', pf='f', pc='c', v=SINK, s=1))
xf = dict(k=FK, c=1, h=1, a=0, t=0, r=0, ro=0, z=1)
R.odd(nd(m='X1', **sh, **xf), nd(m='X2', **sh, **xf))
Us = lambda arm, v, **kw: ball(arm, 'U', v=v, s=1, **kw)
for f_, arm in ((0, 'S'), (1, 'E')):
    R.even(nd(m='X2', f=f_, **sh, **xf), nd(k=FK, f=f_, c=1, h=1, a=0, t=0, r=0, ro=0), arm2=arm, bl2=Us(arm, SK2))
    R.even(nd(k=FK, f=f_, c=1, h=1, a=0, t=0, r=0, ro=0), nd(m='Y2', f=f_, v=SK3, s=1, **fk), arm, Us(arm, SK3))
R.odd(row(), held('IN', z=0, v=SK2, s=1), 'N', Us('S', SK2))
R.even(held('IN', z=0, v=SK2, s=1), row(k=ST, f=0, c=0, h=0, a='h', t=1), arm2='S', bl2=load(pk='k', pf='f', pc='c', v=SK2, s=1))
R.odd(nd(**sk), nd(m='FL', z=1, v=SK2, s=1, **dict(sk, t=1)), 'N', Us('S', SK3))
for b_ in (0, 1):
    R.odd(nd(**fk), nd(m='Y2', v=SK3, b=1 - b_, s=1, **fk), 'N', Us('S', SINK, b=b_))
for k in (LF, FK):
    R.odd(nd(k=k, r=0), nd(k=k, r=0), 'N', Us('S', SK3, b=1), 'N', Us('S', SK3, b=0))

R.tag = 'cross'
for k in (LF, ST):
    turn0(CROSS, k)
ch = dict(v=CROSS, s=1)
R.odd(nd(**fk), nd(m='CA', **ch, **fk), 'N', Us('S', CROSS))
for f_, arm in ((0, 'S'), (1, 'E')):
    R.even(nd(m='CA', f=f_, **ch, **fk), nd(f=f_, **fk), arm2=arm, bl2=ball(arm, 'PQ', **ch))
f0 = dict(fk, f=0)
R.even(nd(**f0), nd(m='CB', b=0, **ch, **f0), 'S', ball('S', 'PQF', b=0, **ch))
R.odd(nd(m='CB', b=0, **ch, **f0), nd(m='CC', b=0, **ch, **f0))
R.even(nd(m='CC', b=0, **ch, **f0), nd(**f0), arm2='E', bl2=ball('E', 'FE', z=1, pc=0, b=0, **ch))
# the head came back along the lower row and met the lower fork's elbow: its arc is the inner one
TR = cell(cls=EL, w=1)
elh = lambda m, **kw: cell(cls=EL, w=1, m=m, z=1, d='d', b=0, **ch, **kw)
R.even(TR, elh('CK'), 'E', ball('E', 'FW', z=1, pc=0, b=0, **ch))
hp = lambda m: cell(cls=PB, m=m, z=1, d='d', b=0, **ch)
R.odd(elh('CK'), TR, arm2='N', bl2=hp('CK'))
KK = cell(cls=EL, w=1, t=2)
R.rx((void(0), O, hp('CK'), O), (KK, O, hp('CL'), O))
R.rx((O, hp('CL'), O, TR), (O, hp('CM'), O, void(1)))
R.rx((KK, O, hp('CM'), O), (elh('CN'), O, O, O))
R.odd(elh('CN'), elh('CO'))
R.even(elh('CO'), TR, arm2='E', bl2=ball('E', 'FE', z=1, pc=1, b=0, **ch))
R.even(nd(**f0), nd(m='CZ', b=0, **ch, **f0), 'S', ball('S', 'FO', z=1, b=0, **ch))
R.odd(nd(m='CZ', b=0, **ch, **f0), nd(m='CY', b=0, **ch, **f0))
R.even(nd(m='CY', b=0, **ch, **f0), nd(**f0), arm2='S', bl2=ball('S', 'PQF', b=1, **ch))
