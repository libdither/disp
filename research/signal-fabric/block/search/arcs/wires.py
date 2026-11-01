"""Flight, arc transit, the two hubs: everything a head does without touching a tree."""
from kit import *

R.tag = 'flight'
for w_, w2 in [('w1', 'w2')]:
    H = lambda w: cell(cls=HB, m='_m', z='z', pc='x', w=w, **HEAD)
    Vb = lambda w: cell(cls=VB, m='_m', z='z', pc='x', w=w, **HEAD)
    R.rx((H('w1'), O, O, O), (void('w1'), H(0), O, O))
    R.rx((O, O, H(0), void('w1')), (O, O, O, H('w1')))
    R.rx((Vb('w1'), O, O, O), (void('w1'), O, Vb(0), O))
    R.rx((O, Vb(0), O, void('w1')), (O, O, O, Vb('w1')))

R.tag = 'arc transit'
def held(c, m='_m', **kw):
    return cell(**{**c, 'm': m, **HEAD, **kw})
TR_, BR_ = dict(cls=EL, w=1), dict(cls=EL, w=0)
TR, BR = cell(**TR_), cell(**BR_)
R.odd(TR, held(TR_, t=1), 'W', ball('E', '_m'))
R.even(held(TR_, t=1), TR, arm2='S', bl2=ball('S', '_m'))
R.odd(BR, held(BR_, t=1), 'N', ball('S', '_m'))
R.even(held(BR_, t=1), held(BR_, t=2))
R.odd(held(BR_, t=2), BR, arm2='W', bl2=ball('E', '_m'))
LA = dict(k=GAP, c=1, h=1, r=0, ro=0)
R.even(nd(**LA), nd(m='_m', t=1, **LA), 'E', ball('E', '_m'))
R.odd(nd(m='_m', t=1, **LA), nd(m='_m', t=2, **LA))
R.even(nd(m='_m', t=2, **LA), nd(**LA), arm2='S', bl2=ball('S', '_m'))

R.tag = 'hubs'
HU = cell(cls=HUB)
start = dict(cls=HUB, m='start', d=1)
end = dict(cls=HUB, m='end', d=1, b=1)
R.even(cell(**start), HU, arm2='S', bl2=ball('S', 'C', v=0, d=1, b=0, s=0))
R.even(HU, HU, 'S', ball('S', 'U', s=0), 'E', ball('E', 'U', s=0))
R.even(HU, cell(**end), 'S', ball('S', 'C', v=0, d=1, b=1, s=0))
R.odd(cell(**end), HU, arm2='N', bl2=ball('S', 'OUT', v=0, d=1, b=1, s=0))
R.odd(cell(**start), HU, arm2='N', bl2=ball('S', 'STUCK', v=0, d=1, b=0, s=0))
DH = cell(cls=HUB, k=1)
HDH = cell(cls=HUB, k=1, m='hd', v='v', d='d', b='b', s=0)
R.odd(DH, HDH, 'W', ball('E', 'U', s=0))
R.even(HDH, DH, arm2='S', bl2=ball('S', 'U', s=1))
