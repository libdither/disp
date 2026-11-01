"""Row insertion. A conveyor runs down the strand swapping its load with each row (order stays right at every tick), then a
sweep comes back up, moving each arc's row down one under the strand: write the new elbow, flip the row's end, erase the old."""
from kit import *
from control import STEM, UNIT
UN2 = 13
R.tag = 'conveyor'
PL = dict(pc='pc', pk='pk', pf='pf')
pb = lambda m, **kw: cell(**{**dict(cls=PB, m=m, z=1), **HEAD, **kw})
load = lambda m='CV', **kw: pb(m, **{**PL, **kw})
row = lambda **kw: nd(**{**dict(r=0, ro=0, a=0, t=0), **kw})
held = lambda m, z=1, **kw: nd(m=m, z=z, **{**dict(r=0, ro=0, a=0, t=0), **kw})
R.odd(row(), held('CV', **PL), 'N', load())
R.even(held('CV', **PL), row(k='pk', f='pf', c='pc', h=0, a='h'), arm2='S', bl2=load(pk='k', pf='f', pc='c'))
# a gap with no arc closes the strand; the conveyor's last load is that gap
tail = dict(cls=NODE, k=GAP, f=0, c=0)
R.odd(void(0), nd(**tail, h=0, a=0, t=0, r=0, ro=0), 'N', load(pk=GAP, pf=0, pc=0), 'N', ball('S', 'FL', z=1))

R.tag = 'row sweep'
FL = lambda **kw: ball('S', 'FL', z=1, **kw)
R.even(row(h=0, t='t'), held('FL', h=0, t='t'), 'S', FL())
for k_ in (ST, LF):
    R.odd(row(k=k_, c=0, h=0), held('FL', k=k_, c=0, h=0), 'N', FL())
R.odd(row(c=1, h=0), held('FL', c=1, h=0), 'N', ball('S', 'FN', z=1, pc=0))
cor = dict(h=0, a=1, t='t', r='r', ro='ro')
R.even(nd(**cor), nd(m='FN', z=1, pc='x', **cor), 'S', ball('S', 'FN', z=1, pc='x'))
R.odd(nd(m='FN', z=1, pc='x', **cor), nd(m='FN2', z=1, pc='x', **cor))
R.even(nd(m='FN2', z=1, pc='x', **cor), nd(**cor), arm2='E', bl2=ball('E', 'FE', z=1, pc='x'))
el = lambda **kw: cell(**{**dict(cls=EL, w='w'), **HEAD, **kw})
R.odd(cell(cls=EL, w='w'), el(m='FE', z=1, pc='x'), 'W', ball('E', 'FE', z=1, pc='x'))
R.even(el(m='FE', z=1, pc=0), cell(cls=EL, w='w'), arm2='S', bl2=pb('WR', pc=0))
R.odd(void('w'), el(m='WR', z=1, pc='x'), 'N', pb('WR', pc='x'))
for x_ in (0, 2):
    R.even(el(m='WR', z=1, pc=x_), el(m='WS', z=1, pc=x_))
R.odd(el(m='WS', z=1, pc='x'), cell(cls=EL, w='w'), arm2='W', bl2=ball('E', 'FW', z=1, pc='x'))
R.even(row(c=1, h=0), held('FN', pc=1, c=1, h=1), 'E', ball('E', 'FW', z=1, pc=0))
R.odd(row(c=1, h=1), held('FN', pc=1, c=1, h=1), 'N', ball('S', 'FN', z=1, pc=1))
# the old elbow goes; a ball leaving a site it just emptied can't be an ordinary flight, so it drops onto the new elbow
for w in (0, 1):
    R.even(el(m='FE', z=1, pc=1, w=w), void(1 - w), arm2='S', bl2=pb('ER'))
R.odd(cell(cls=EL, w='w'), el(m='ER', z=1), 'N', pb('ER'))
R.even(el(m='ER', z=1), el(m='WS', z=1, pc=1))
R.even(row(c=1, h=1), held('FO', c=1, h=1), 'E', ball('E', 'FW', z=1, pc=1))
R.odd(row(c=1, h=1), held('FO', c=1, h=1), 'N', ball('S', 'FO', z=1))
clear = dict(h=0, t='t', r=0, ro=0)
R.even(nd(a=1, **clear), nd(m='FD', z=1, a=0, **clear), 'S', ball('S', 'FO', z=1))
R.odd(nd(m='FD', z=1, a=0, **clear), nd(m='FD2', z=1, a=0, **clear))
R.even(nd(m='FD2', z=1, a=0, **clear), nd(a=0, **clear), arm2='S', bl2=ball('S', 'FD', z=1))
R.odd(row(c=1, h=1), row(c=1, h=1), 'N', ball('S', 'FD', z=1), 'N', FL())

R.tag = 'stem'
Ud = lambda v, b: ball('S', 'U', v=v, b=b, s=1)
def begin(v, first, kw=None):
    """the head arrives on the pointer t; t leaves as the conveyor's first load, a marked `first` stays"""
    R.odd(row(), held('IN', z=0, v=v, b=0, s=1), 'N', Ud(v, 0))
    R.even(held('IN', z=0, v=v, b=0, s=1), row(k=first, f=0, c=0, h=0, a='h', t=1), arm2='S', bl2=load(pk='k', pf='f', pc='c', v=v, b=0, s=1))
begin(STEM, ST)
mk = lambda **kw: dict(f=0, c=0, h=0, a=0, t=1, r=0, ro=0, **kw)
R.odd(nd(k=ST, f=0, c=0, h=0, a=0, t=0, r=0, ro=0), nd(m='FL', z=1, k=ST, v=STEM, b=0, s=1, **mk()), 'N', Ud(STEM, 1))

R.tag = 'unit'
UN3 = 14
begin(UNIT, GAP)
H1 = dict(z=1, b=0, s=1)
def again(v, v2):
    """stop on the gap just made; make another under it"""
    g = dict(k=GAP, f=0, c=0, h=0, a=0, r=0, ro=0)
    R.odd(nd(m='FL', v=v, t=1, **H1, **g), nd(m='UD', v=v, t=2, **H1, **g))
    R.even(nd(m='UD', v=v, t=2, **H1, **g), nd(t=2, **g), arm2='S', bl2=ball('S', 'UD', v=v, **H1))
    R.even(held('UD', v=v, b=0, s=1), row(k=GAP, f=0, c=0, h=0, a='h', t=1), arm2='S', bl2=load(pk='k', pf='f', pc='c', v=v2, b=0, s=1))
R.odd(row(), held('UD', v='v', b=0, s=1), 'N', ball('S', 'UD', v='v', **H1))
again(UNIT, UN2)
again(UN2, UN3)
# rows now: top gap, middle gap, low gap. The low gap becomes the arc's end; the head goes up past the middle.
g3 = dict(k=GAP, f=0, h=0, a=0, r=0, ro=0)
H3 = dict(v=UN3, **H1)
R.odd(nd(m='FL', c=0, t=1, **H3, **g3), nd(c=1, t=0, **g3), arm2='N', bl2=ball('S', 'SN', pc=0, **H3))
mid = dict(k=GAP, f=0, c=0, h=0, a=0, t=2, r=0, ro=0)
R.even(nd(**mid), nd(m='SN', pc='x', **H3, **mid), 'S', ball('S', 'SN', pc='x', **H3))
R.odd(nd(m='SN', pc='x', **H3, **mid), nd(**mid), arm2='N', bl2=ball('S', 'SP', pc='x', **H3))
# from the top gap: east to the first free column, write the elbow, a wire mark, the lower elbow
top = dict(mid)
R.even(nd(**top), nd(m='SE', **H3, **top), 'S', ball('S', 'SP', pc=0, **H3))
R.odd(nd(m='SE', **H3, **top), nd(m='SF', **H3, **top))
sb = lambda w=0: cell(cls=SB, m='SC', w=w, d='d', **H3)
R.even(nd(m='SF', **H3, **top), nd(**top), arm2='E', bl2=sb())
R.odd(void('w'), sb('w'), 'W', sb())
R.rx((sb(1), O, O, O), (void(1), sb(), O, O))
EW = dict(cls=EL, w=1, d='d', **H3)
R.even(sb(0), cell(m='WA', **EW))
R.odd(cell(m='WA', **EW), cell(m='WB', **EW))
wv = lambda w=0: cell(cls=PB, m='WV', w=w, d='d', **H3)
R.even(cell(m='WB', **EW), cell(cls=EL, w=1), arm2='S', bl2=wv())
R.odd(void(0), wv(1), 'N', wv())
R.rx((wv(1), O, O, O), (void(1), O, pb('WR', pc=2, **H3), O))
# back at the low gap
low = dict(k=GAP, f=0, c=1, a=0, t=0, r=0, ro=0)
R.even(nd(h=0, **low), nd(m='YG', h=1, **H3, **low), 'E', ball('E', 'FW', z=1, pc=2, v=UN3, b=0, s=1))
R.odd(nd(h=1, **low), nd(m='YG', h=1, **H3, **low), 'N', ball('S', 'SN', pc=1, **H3))
# the bypass: the reading goes round the arc, so the leaf can appear out of its sight; then the fork reads
by = dict(top, k=BY, t=1)
R.even(nd(**top), nd(m='YD', **H3, **by), 'S', ball('S', 'SP', pc=1, **H3))
R.odd(nd(m='YD', **H3, **by), nd(m='YE', **H3, **by))
R.even(nd(m='YE', **H3, **by), nd(**by), arm2='S', bl2=ball('S', 'YD', **H3))
R.odd(nd(**mid), nd(m='YD', **H3, **mid), 'N', ball('S', 'YD', **H3))
lf = dict(mid, k=LF, t=0)
R.even(nd(m='YD', **H3, **mid), nd(m='YD', **H3, **lf))
R.odd(nd(m='YD', **H3, **lf), nd(**lf), arm2='N', bl2=ball('S', 'YU', **H3))
R.even(nd(**by), nd(m='YF', **H3, **by), 'S', ball('S', 'YU', **H3))
R.odd(nd(m='YF', **H3, **by), nd(k=FK, f=0, c=1, h=1, a=0, t=0, r=0, ro=0), arm2='N', bl2=Ud(UNIT, 1))
