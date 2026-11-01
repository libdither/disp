"""Polymer: program and data are each one Polish strand on a row; one head walks both."""
import sys, os
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from physics import I, A
from laws import Lay, Link
from kit import K, RANGE, FIELDS, cell as C, VOID, Rules
NAME = 'polymer'
RANGES = [RANGE[n] for n in FIELDS]
E, SO, W, N, SE = (1, 0), (0, 1), (-1, 0), (0, -1), (1, 1)
R = Rules(); st = R.st
kL, kS, kF, kPE, kPS, kPN, kFL, kFX, kFY, kW0, kWB, kOUT = (K[n] for n in 'L S F PE PS PN FL FX FY W0 WB OUT'.split())
ROLES = 'DP DP2 F9 FA1 F1 F2 F3 F4 F5 F6 F7 F8 BM BM2 BM3 WB RF D R1 R1b R2 R2b RC RJ R3 R3b R4 R5 RS W0 PC SEQ1 SEQ2 SEQ3 SEQ4 DAG1'.split()
ro = {n: i + 1 for i, n in enumerate(ROLES)}
def role(n, k=None, **kw):
    if k is not None: kw['k'] = k
    return dict(a=1, r=ro[n], **kw)
def go(s1, s2, c1, c2=None, f1=None, f2=None, g1=None, g2=None):
    """head on c1 moves right as c1 becomes c2; the same reaction read leftward steps from s2 onto c1"""
    c2 = c1 if c2 is None else c2
    h1 = hd(f1, g1); h2 = hd(f2, g2)
    R.up(C('x', head=st(s1), **{**c1, **h1}), C('y'), C('x', **c2), C('y', head=st(s2), **h2))
def hd(f, g):
    d = {}
    if f is not None: d['f'] = f
    if g is not None: d['g'] = g
    return d
def turnL(s1, s2, c1, c2=None, f1=None, f2=None, g1=None, g2=None):
    c2 = c1 if c2 is None else c2
    R.up(C('x', head=st(s1), **{**c1, **hd(f1, g1)}), C('y'), C('x', head=st(s2), **{**c2, **hd(f2, g2)}), C('y'))
def turnR(s1, s2, c1, c2=None, f1=None, f2=None, g1=None, g2=None):
    c2 = c1 if c2 is None else c2
    R.up(C('x', **c1), C('y', head=st(s1), **hd(f1, g1)), C('x', **c2), C('y', head=st(s2), **hd(f2, g2)))
PL = dict(m=0, e=0, a=0, r=0, u=0)
PE = C('', k=kPE, **PL); FR = C('', k=kPE, m=1, e=0, a=0, r=0, u=0); PS = C('', k=kPS, **PL)
J = C('', k=kPN, m=0, e=0, a=1, r=ro['RJ'], u=0); RS = C('', k=kPE, m=0, e=0, a=1, r=ro['RS'], u=0)
# the shared hop: every plain walker crosses a spacer-to-site gap by this one reaction
R.add((C('u'), C('v'), C('x', h=1, st='hs', f='hf', g='hg', o='ho'), C('y')), (C('u'), C('v'), C('x'), C('y', h=1, st='hs', f='hf', g='hg', o='ho')))

# SCAN: from the site after a marked cell, find where the subtree there ends. Each leaf pairs with the leftmost unpaired fork,
# so the paired forks are always a prefix and the last-paired one is the nearest paired one. The undo is the same reactions run back.
go('SC', 'SC', dict(k=kF, m=0, a=0)); go('SC', 'SC', dict(k=kS, m=0, a=0))
turnL('SC', 'BK', dict(k=kL, m=0, a=0), dict(k=kL, m=1, a=0))
go('BK', 'BK', dict(m=0, a=0)); go('BK', 'BK', dict(m=2, a=0))
turnR('BK', 'FW', dict(a=1))
go('FW', 'FW', dict(k=kF, m=2, a=0)); go('FW', 'FW', dict(k=kS, m=0, a=0)); go('FW', 'FW', dict(k=kL, m=2, a=0))
go('FW', 'RT', dict(k=kF, m=0, a=0), dict(k=kF, m=2, a=0))
go('RT', 'RT', dict(m=0, a=0)); go('RT', 'RT', dict(k=kL, m=2, a=0))
go('RT', 'SC', dict(k=kL, m=1, a=0), dict(k=kL, m=2, a=0))
turnL('FW', 'FW', dict(k=kL, m=1, e=0, a=0), dict(k=kL, m=1, e=1, a=0), f1=0, f2=1)

# SWAP at the data pointer: the left part drops to the row below, one fork flips the reading, the parts cross, it climbs back.
R.up(C('x', head=st('OP'), o=2, g=0, f=0, k=kF, a=1, r=ro['D']), C('y'), C('x', k=kF, a=1, r=ro['R1']), C('y', head=st('SC'), o=2, g=0, f=0))
R.up(C('x', head=st('OP'), o=2, k=kL, a=1, r=ro['D']), C('y'), C('x', head=st('OD'), o=2, k=kL, a=1, r=ro['D']), C('y')); R.up(C('x', head=st('OP'), o=2, k=kS, a=1, r=ro['D']), C('y'), C('x', head=st('OD'), o=2, k=kS, a=1, r=ro['D']), C('y'))
turnR('SC', 'G1', role('R1', kF), role('R1b', kF), f1=1, f2=0)
go('G1', 'G1', dict(e=0, a=0))
go('G1', 'J1', dict(k=kL, e=1, a=0, r=0), dict(k=kL, e=2, a=1, r=ro['R2']))
R.up(C('x', head=st('J1'), a=0, u=0), C('y'), C('x', head=st('J2'), a=0, u=1), C('y'), VOID, VOID, J, VOID)
turnR('J2', 'SC', role('R2', kL), role('R2b', kL), f1=0, f2=0)
turnR('SC', 'G2', role('R2b', kL, e=2), dict(k=kL, e=2, a=0, r=0), f1=1, f2=0)
go('G2', 'G2', dict(e=0, a=0))
go('G2', 'G3', dict(e=1), dict(e=3))
R.up(C('x', head=st('G3'), a=0, r=0, u=0), C('y'), C('x', head=st('W1'), a=1, r=ro['RC'], u=0), C('y'), VOID, VOID, J, VOID)
R.up(C('', head=st('G3'), k=kPN, m=0, e=0, a=1, r=ro['RJ'], u=1), C('y'), C('', head=st('W1'), k=kPN, m=0, e=0, a=1, r=ro['RC'], u=1), C('y'), VOID, VOID, J, VOID)
go('W1', 'W1', dict(e=0, a=0)); go('W1', 'W1', dict(e=3, a=0), g1=0, g2=0)
def HD(name, k, h=None, **kw):
    """a literal plain cell of kind k with the head on it"""
    return C('', head=st(name), h=h, k=k, **{**PL, **kw})
Y = C('y')
def block(a, b, c, d, a2, b2, c2, d2): R.add((a, b, c, d), (a2, b2, c2, d2))
# descent: the last cell of the left part, then the rest going left
block(C('x', k=kL, e=2, a=0), C('y', head=st('W1'), g=0), VOID, VOID, HD('DE', kPS, g=0), Y, PE, C('x', k=kL, e=2, a=0))
block(C('x', e=0, a=0), C('y', head=st('DE')), VOID, VOID, HD('DE', kPS), Y, PE, C('x', e=0, a=0))
block(C('x', k=kF, a=1, r=ro['R1b']), C('y', head=st('DE')), VOID, VOID, C('x', k=kFL, a=1, r=ro['R3']), C('y', head=st('SW')), VOID, RS)
# holes become fresh: a sweep right to the first cell of the right part, then back
R.up(HD('SW', kPS), PE, FR, C('', head=st('SW'), k=kPE, m=1, e=0, a=0, r=0, u=0))
turnL('SW', 'W2', dict(u=1))
go('W2', 'W2', dict(k=kPE, m=1, a=0, u=0))
turnR('W2', 'G4', role('R3', kFL), role('R3b', kFX))
go('G4', 'G4', dict(a=0))
turnL('G4', 'LAY', role('RC'))
# a bypass lane under the right part, then the one reaction that flips the reading
block(C('x', a=0, u=0), C('y', head=st('LAY')), VOID, VOID, C('x', head=st('LAY'), a=0, u=0), Y, FR, FR)
block(C('x', a=0, u=1, m=0), C('y', head=st('LAY')), J, VOID, C('x', head=st('W3'), a=0, u=0, m=0), Y, FR, FR)
go('W3', 'W3', dict(k=kPE, m=1, a=0, u=0))

FSP = C('', k=kPE, m=0, e=0, a=1, r=ro['RF'], u=0)
P0d = dict(k=kPE, m=0, e=0, a=0, r=0, u=0); P3d = dict(k=kPE, m=3, e=0, a=0, r=0, u=0); PF = dict(k=kPE, m=1, e=0, a=0, r=0, u=0)
FSPd = dict(k=kPE, m=0, e=0, a=1, r=ro['RF'], u=0)
FYd = dict(k=kFY, a=1, r=ro['R4']); RCd = dict(a=1, r=ro['RC']); RSd = dict(k=kPE, m=0, e=0, a=1, r=ro['RS'], u=0); JD = dict(k=kPN, m=0, e=0, a=1, r=ro['RJ'], u=0)
def hh(s, d, p='', **kw): return C(p, head=st(s), h=2, **{**d, **kw})
block(C('x', k=kFX, a=1, r=ro['R3b']), C('', head=st('W3'), g=0, **P0d), C('z'), RS, C('x', **FYd), C('', head=st('RW1'), g=1, **FSPd), C('z'), RS)
go('RW1', 'RW1', dict(a=0))
turnL('RW1', 'RW2s', role('RC'))
go('RW2s', 'RW2s', dict(a=0))
block(C('x', **FYd), C('', head=st('RW2s'), **FSPd), C('z'), C('w'), C('x', **FYd), hh('RS0', FSPd), C('z'), C('w'))
def both(a, b, a2, b2): R.both(a, b, a2, b2)
# the top row: each lap a hole next to the fork goes right through everything to the stop; the walker comes back
R.low(hh('RS0', FSPd, f=0), C('', **PF), C('', **FSPd), hh('HC', PF, f=1))
both(hh('HC', PF), C('t', a=0), C('t', a=0), hh('HC', PF))
R.low(hh('HC', PF, f=1), C('x', **RCd), hh('RW2', P3d, f=0), C('x', **RCd))
R.low(hh('HC', PF, f=0), C('x', **RCd), hh('RW2', P0d, f=0), C('x', **RCd))
both(C('t', a=0), C('y', head=st('RW2'), h=2, a=0), C('t', head=st('RW2'), h=2, a=0), C('y', a=0))
R.low(C('', **FSPd), hh('RW2', PF, f=0), C('', **FSPd), hh('HC', PF, f=0))
R.low(C('', **FSPd), C('y', head=st('RW2'), h=2, m=0, a=0), hh('RWD', FSPd), C('y', m=0, a=0))
# the lane, mirrored: a hole next to the far end goes left through everything to the lane's start
block(C('x', **FYd), hh('RWD', FSPd), C('z'), C('', **RSd), C('x', **FYd), C('', **FSPd), C('z'), hh('RW3s', RSd))
both(C('x', head=st('RW3s'), h=2), C('y', a=0), C('x'), C('y', head=st('RW3s'), h=2, a=0))
R.up(hh('RW3s', PF, f=0), C('', **JD), hh('HC', PF, f=1), C('', **JD))
R.up(C('', **RSd), hh('HC', PF, f=1), C('', **RSd), hh('RW3', P3d, f=0))
R.up(C('', **RSd), hh('HC', PF, f=0), C('', **RSd), hh('RW3', P0d, f=0))
both(C('x', head=st('RW3'), h=2, a=0), C('y', a=0), C('x', a=0), C('y', head=st('RW3'), h=2, a=0))
R.up(hh('RW3', PF, f=0), C('', **JD), hh('HC', PF, f=0), C('', **JD))
R.up(C('x', head=st('RW3'), h=2, k=kL, m=0, a=0, e=2), C('', **JD), C('x', head=st('W4'), h=2, k=kL, m=0, a=0, e=3), C('', **JD))
both(C('t', a=0, e=0), C('y', head=st('W4'), h=2, a=0), C('t', head=st('W4'), h=2, a=0, e=0), C('y', a=0))
R.up(C('', **RSd), C('y', head=st('W4'), h=2, a=0), hh('W5', RSd), C('y', a=0))
block(C('x', **FYd), C('', **FSPd), C('z'), hh('W5', RSd), C('x', **FYd), C('', head=st('W6'), **FSPd), C('z'), C('', **RSd))
# back to one strand: right part's end, holes aim down, fork reads plainly, the lane clears, the left part climbs
go('W6', 'W6', dict(e=0, a=0))
go('W6', 'W7', dict(e=3, a=0), dict(e=2, a=0))
R.up(C('', head=st('W7'), **P3d), Y, C('', k=kPS, m=3, e=0, a=0, r=0, u=0), C('y', head=st('W7b')))
R.up(C('', head=st('W7b'), **P0d), Y, PS, C('y', head=st('W7b')))
turnL('W7b', 'W8', role('RC'))
go('W8', 'W8', dict(a=0))
block(C('x', **FYd), C('', head=st('W8'), **FSPd), C('z'), RS, C('x', k=kF, a=1, r=ro['R1b']), C('', head=st('W9'), **P0d), C('z'), VOID)
LP3 = C('', **P3d)
block(C('x', head=st('W9'), e=0, a=0), Y, PE, PE, C('x', e=0, a=0), C('y', head=st('W9')), VOID, VOID)
block(C('x', head=st('W9'), e=2, a=0), Y, PE, LP3, C('x', e=2, a=0), C('y', head=st('DE1')), VOID, VOID)
block(HD('DE1', kPS, g=1, m=3), Y, PE, C('x', e=0, a=0, u=0), C('x', e=0, a=0, u=1), C('y', head=st('DE'), g=1), J, VOID)
block(HD('DE1', kPS, g=1, m=3), Y, PE, C('x', k=kL, e=3, a=0, u=0), C('x', k=kL, e=3, a=0, u=1), C('y', head=st('W1'), g=1), J, VOID)
block(HD('DE', kPS, g=1), Y, PE, C('x', k=kL, e=3, a=0), C('x', k=kL, e=3, a=0), C('y', head=st('W1'), g=1), VOID, VOID)
# after: the reverse of the marking scans runs by itself and lands here
R.up(C('x', k=kF, a=1, r=ro['R1']), C('y', head=st('SC'), f=0, g=1, o=2), C('x', head=st('OD'), k=kF, a=1, r=ro['D'], f=0, g=0, o=2), C('y'))

WBd = dict(k=kWB, a=1, r=ro['WB'])
VC = dict(k=0, m=0, e=0, a=0, r=0, u=0); JT = dict(k=kPN, m=0, e=0, a=1, r=ro['RJ'], u=1)
DPd = dict(a=1, r=ro['DP'])
def walker(n):
    go(n, n, dict(a=0)); go(n, n, WBd); go(n, n, DPd)
# The interpreter. EN enters a subprogram at a site. R is EN's mirror: it stands just left of a subprogram that has finished.
# Running time backward is EN and R trading places, so a dag is only "start at R" and "finish at EN".
go('EN', 'S1', dict(k=kS, a=0))
go('S1', 'S2', dict(k=kS, a=0))
for n in (3, 4, 5, 6): go('S%d' % (n - 1), 'S%d' % n, dict(k=kS, a=0))
for n in (2, 5, 6): R.up(C('x', head=st('S%d' % n), o=0, k=kL, a=0, r=0), C('y'), C('x', k=kL, a=1, r=ro['PC']), C('y', head=st('OP'), o=n))
walker('OP'); walker('OD')
for n in (2, 5, 6): R.up(C('x', k=kL, a=1, r=ro['PC']), C('y', head=st('OD'), o=n), C('x', head=st('C%d' % n), o=0, k=kL, a=0, r=0), C('y'))
for n in (3, 4, 5, 6): R.up(C('x', k=kS, a=0), C('y', head=st('C%d' % n)), C('x', head=st('C%d' % (n - 1)), k=kS, a=0), C('y'))
R.up(C('x', k=kS, a=0), C('y', head=st('C2')), C('x', head=st('RS'), k=kS, a=0), C('y'))
R.up(C('x', k=kS, a=0), C('y', head=st('RS')), C('x', head=st('R'), k=kS, a=0), C('y'))
turnL('EN', 'R', dict(k=kL, a=0))
go('EN', 'F1', dict(k=kF, a=0))
# seq: first part, scan it, second part, scan it again to walk back
go('F1', 'EN', dict(k=kS, a=0, r=0), role('SEQ1', kS))
turnR('R', 'SC', role('SEQ1', kS), role('SEQ2', kS), f1=0, f2=0)
turnR('SC', 'GA', role('SEQ2', kS), role('SEQ3', kS), f1=1, f2=0)
go('GA', 'GA', dict(e=0, a=0))
go('GA', 'EN', dict(k=kL, e=1, a=0), dict(k=kL, e=0, a=0))
R.up(C('x', k=kL, e=0, a=0), C('y', head=st('R')), C('x', head=st('GZ'), k=kL, e=1, a=0), C('y'))
go('GZ', 'GZ', dict(e=0, a=0))
turnR('GZ', 'SC', role('SEQ3', kS), role('SEQ4', kS), f1=0, f2=1)
R.up(C('x', k=kS, a=1, r=ro['SEQ4']), C('y', head=st('SC'), f=0), C('x', head=st('Q'), k=kS, a=0, r=0, f=0), C('y'))
R.up(C('x', k=kF, a=0), C('y', head=st('Q')), C('x', head=st('R'), k=kF, a=0), C('y'))
# map on a stem or leaf or fork: read the data's kind, go down into its parts, run the part's program, come back
go('S1', 'M1', dict(k=kF, a=0))
F9 = role('F9', kF)
go('M1', 'MP0', dict(k=kF, a=0, r=0), F9)
walker('MP0'); walker('MP')
turnL('MP0', 'MP', role('D', kL))
go('MP0', 'MC2S', role('D', kS, u=0), role('DP', kS, u=0))
go('MP0', 'MC2F', role('D', kF, u=0), role('DP', kF, u=0))
turnL('MC2S', 'TOS', dict(a=0, r=0), role('D'))
turnL('MC2F', 'TOF', dict(a=0, r=0), role('D'))
walker('TOS'); walker('TOF')
turnR('TOS', 'EN', F9, role('FA1', kF))
turnR('R', 'US', role('FA1', kF), F9)
walker('US'); walker('UF')
turnL('US', 'MC3S', role('D'), dict(a=0, r=0))
turnL('UF', 'MC3F', role('D'), dict(a=0, r=0))
R.up(C('x', k=kS, a=1, r=ro['DP'], u=0), C('y', head=st('MC3S')), C('x', head=st('MP'), k=kS, a=1, r=ro['D'], u=0), C('y'))
R.up(C('x', k=kF, a=1, r=ro['DP'], u=0), C('y', head=st('MC3F')), C('x', head=st('MP'), k=kF, a=1, r=ro['D'], u=0), C('y'))
R.up(C('x', **F9), C('y', head=st('MP')), C('x', head=st('Q2'), k=kF, a=0, r=0), C('y'))
R.up(C('x', k=kF, a=0), C('y', head=st('Q2')), C('x', head=st('RS'), k=kF, a=0), C('y'))
# fork: skip the first program, run the second on the left part
turnR('TOF', 'SC', F9, role('F1', kF), f1=0, f2=0)
turnR('SC', 'GA', role('F1', kF), role('F2', kF), f1=1, f2=0)
turnR('GZ', 'SC', role('F2', kF), role('F3', kF), f1=0, f2=1)
# the pointer steps from the left part to the right part by a scan of the left part
turnR('SC', 'DM', role('F3', kF), role('F4', kF), f1=0, f2=0)
walker('DM')
turnL('DM', 'DB', role('D'), dict(a=0, r=0))
turnR('DB', 'SC', role('DP', kF, u=0), role('DP2', kF, u=0), f1=0, f2=0)
turnR('SC', 'DG', role('DP2', kF, u=0), role('DP', kF, u=1), f1=1, f2=0)
go('DG', 'DG', dict(e=0, a=0))
go('DG', 'DG2', dict(e=1, a=0), dict(e=0, a=0))
turnL('DG2', 'DB2', dict(a=0, r=0), role('D'))
walker('DB2')
# then skip two programs and run the third on the right part
turnR('DB2', 'SC', role('F4', kF), role('F5', kF), f1=0, f2=0)
turnR('SC', 'GC', role('F5', kF), role('F6', kF), f1=1, f2=0)
go('GC', 'GC', dict(e=0, a=0))
go('GC', 'SC', dict(k=kL, e=1, a=0, r=0), role('BM', kL, e=0), f1=0, f2=0)
turnR('SC', 'GA', role('BM', kL), role('BM2', kL), f1=1, f2=0)
turnR('GZ', 'SC', role('BM2', kL), role('BM3', kL), f1=0, f2=1)
R.up(C('x', k=kL, a=1, r=ro['BM3'], e=0), C('y', head=st('SC'), f=0), C('x', head=st('YY'), k=kL, a=0, r=0, e=1, f=0), C('y'))
go('YY', 'YY', dict(e=0, a=0))
turnR('YY', 'SC', role('F6', kF), role('F7', kF), f1=0, f2=1)
turnR('SC', 'DB2', role('F7', kF), role('F8', kF), f1=0, f2=0)
turnR('DM', 'UF', role('F8', kF), F9)
# stem and unit: a pair of holes per symbol comes in from the tail, the node hops over them, and one reaction fills the gap
P2d = dict(k=kPE, m=2, e=0, a=0, r=0, u=0)
for n in (5, 6): R.up(C('x', head=st('OP'), o=n, a=1, r=ro['D']), C('y'), C('x', a=1, r=ro['D']), C('y', head=st('T'), o=n))
for k_ in (kL, kS, kF): go('T', 'T', dict(k=k_, a=0))
R.add((C('', head=st('T'), o=5, **JT), VOID, C('below1'), C('below2')), (C('', **PF), C('', head=st('T2'), o=5, **PF), C('below1'), C('below2')))
R.add((C('', head=st('T'), o=6, **JT), VOID, C('below1'), C('below2')), (C('', **PF), C('', head=st('T1'), o=6, **PF), C('below1'), C('below2')))
R.add((C('', head=st('T1'), o=6, **VC), VOID, C('below1'), C('below2')), (C('', **PF), C('', head=st('T2'), o=6, **PF), C('below1'), C('below2')))
R.add((C('', head=st('T2'), **VC), VOID, C('below1'), C('below2')), (C('', head=st('T3'), **JT), VOID, C('below1'), C('below2')))
go('T3', 'T3', dict(a=0))
turnR('T3', 'RW4', role('D'))
R.rs[-1] = ((R.rs[-1][0][0], R.rs[-1][0][1], R.rs[-1][0][2], R.rs[-1][0][3]), (R.rs[-1][1][0], C('y', head=st('RW4'), h=2), R.rs[-1][1][2], R.rs[-1][1][3]))
both(C('x', head=st('RW4'), h=2, a=0), C('y', a=0), C('x', a=0), C('y', head=st('RW4'), h=2, a=0))
R.low(hh('RW4', PF, f=0), C('', **JT), hh('HC', PF, f=1), C('', **JT))
R.up(C('x', a=1, r=ro['D']), hh('HC', PF, f=1), C('x', a=1, r=ro['D']), hh('RW5', P2d, f=0))
R.up(C('x', a=1, r=ro['D']), hh('HC', PF, f=0), C('x', a=1, r=ro['D']), hh('RW5', P3d, f=0))
both(C('x', head=st('RW5'), h=2, a=0), C('y', a=0), C('x', a=0), C('y', head=st('RW5'), h=2, a=0))
R.low(hh('RW5', PF, f=0), C('', **JT), hh('HC', PF, f=0), C('', **JT))
R.low(C('x', head=st('RW5'), h=2, m=0, a=0), C('', **JT), C('x', head=st('ZW'), h=2, m=0, a=0), C('', **JT))
both(C('t', a=0), C('y', head=st('ZW'), h=2, a=0), C('t', head=st('ZW'), h=2, a=0), C('y', a=0))
Dc = dict(a=1, r=ro['D'])
R.up(C('x', **Dc), hh('ZW', P3d), C('', **PF), C('x', head=st('X'), h=2, **Dc))
both(C('x', head=st('X'), h=2, **Dc), C('', **P3d), C('', **P0d), C('x', head=st('X'), h=2, **Dc))
R.low(C('x', head=st('X'), h=2, **Dc), C('', **P2d), C('', **P0d), C('x', head=st('Xe'), h=2, **Dc))
R.up(C('x', head=st('Xe'), h=2, **Dc), Y, C('x', head=st('Z3'), a=0, r=0), Y)
R.up(C('', **P0d), C('y', head=st('Z3'), o=6), C('', head=st('Z4'), o=6, **P0d), C('y'))
R.up(C('', **PF), C('y', head=st('Z3'), o=5), C('', head=st('OD'), o=5, k=kS, m=0, e=0, a=1, r=ro['D'], u=0), C('y'))
R.up(C('', **PF), C('', head=st('Z4'), o=6, **P0d), C('', k=kF, m=0, e=0, a=1, r=ro['D'], u=0), C('', head=st('Z5'), h=2, o=6, k=kL, m=0, e=0, a=0, r=0, u=0))
R.low(C('', head=st('Z5'), h=2, o=6, k=kL, m=0, e=0, a=0, r=0, u=0), C('', **P0d), C('', **P0d), C('', head=st('Z6'), h=2, o=6, k=kL, m=0, e=0, a=0, r=0, u=0))
R.up(C('', head=st('Z6'), h=2, o=6, k=kL, m=0, e=0, a=0, r=0, u=0), Y, C('', head=st('Z7'), o=6, k=kL, m=0, e=0, a=0, r=0, u=0), Y)
R.up(C('', k=kF, m=0, e=0, a=1, r=ro['D'], u=0), C('', head=st('Z7'), o=6, **P0d), C('', head=st('OD'), o=6, k=kF, m=0, e=0, a=1, r=ro['D'], u=0), C('', **P0d))
# dag
R.up(C('x', head=st('F1'), k=kL, a=0, r=0), C('y'), C('x', k=kL, a=1, r=ro['DAG1']), C('y', head=st('R')))
R.up(C('x', k=kL, a=1, r=ro['DAG1']), C('y', head=st('EN')), C('x', head=st('Q'), k=kL, a=0, r=0), C('y'))
# ends
W0 = dict(k=kW0, a=1, r=ro['W0'])
turnR('R', 'END', W0)
go('END', 'END', dict(a=0)); go('END', 'END', WBd)
go('END', 'TC', role('D'))
for k_ in (kL, kS, kF): go('TC', 'TC', dict(k=k_, a=0))
R.add((C('', head=st('TC'), **JT), VOID, C('below1'), C('below2')), (C('', head=st('TE'), **VC), VOID, C('below1'), C('below2')))
R.up(C('x', a=1, r=ro['D']), C('y', head=st('TE')), C('x', head=st('OG0'), a=0, r=0), C('y'))
R.up(C('x', **WBd), C('y', head=st('OG0')), C('x', head=st('OG'), **WBd), C('y'))
go('OG', 'OG', dict(a=0))
R.up(C('x', **W0), C('y', head=st('OG')), C('x', **W0), C('y', head=st('EX'), h=2))
R.add((VOID, VOID, C('y', head=st('EX'), h=2, f=0, g=0, o=0), C('z')), (C('', k=kOUT, m=0, e=0, a=0, r=0, u=0), VOID, C('y'), C('z')))
# start
go('ST', 'ST2', WBd)
go('ST2', 'TA', dict(a=0, r=0), role('D'))
for k_ in (kL, kS, kF): go('TA', 'TA', dict(k=k_, a=0)); go('TE', 'TE', dict(k=k_, a=0))
R.add((C('', head=st('TA'), **VC), VOID, C('below1'), C('below2')), (C('', head=st('TB'), **JT), VOID, C('below1'), C('below2')))
go('TB', 'TB', dict(a=0)); go('TB', 'TB', role('D'))
R.up(C('x', **WBd), C('y', head=st('TB')), C('x', head=st('TP'), **WBd), C('y'))
go('TP', 'TP', dict(a=0))
turnR('TP', 'EN', W0)
# a stuck run bounces all the way back to the start; the head then walks out over the program and leaves as a different token
go('ST', 'ST', dict(a=0))
R.up(C('x', **W0), C('y', head=st('ST')), C('x', **W0), C('y', head=st('EX2'), h=2))
R.add((VOID, VOID, C('y', head=st('EX2'), h=2, f=0, g=0, o=0), C('z')), (C('', k=kFL, m=0, e=0, a=0, r=0, u=0), VOID, C('y'), C('z')))
REACTIONS = R.rs
SHAPES = len(REACTIONS)  # no symmetry folding: every reaction is written once, with variables for the fields it carries

def _cell(k, a=0, r=0, h=0, s=0):
    return (k, 0, 0, a, r, 0, h, s, 0, 0, 0)
SP = _cell(kPE)
def lay(t):
    cells, x = {}, [0]
    def put(k):
        cells[(x[0], 0)] = _cell(k); cells[(x[0] + 1, 0)] = SP; x[0] += 2
    def go_(t):
        if t == 'L': put(kL)
        elif t[0] == 'S': put(kS); go_(t[1])
        else: put(kF); go_(t[1]); go_(t[2])
    go_(t)
    return Lay(cells, (0, 0), E)
def link(dp, dx):
    w = dp.w
    return Link((2, 0), (w + 4, 0), {(0, 0): _cell(kW0, 1, ro['W0']), (1, 0): SP, (w + 2, 0): _cell(kWB, 1, ro['WB'], 1, st('ST')), (w + 3, 0): SP}, 1)
def node(cell, step):
    k = cell[0]
    if k == kPE: return ('pass', E)
    if k == kPS: return ('pass', SE)
    if k == kPN: return ('pass', N)
    if k == kL: return ('leaf', E)
    if k == kS: return ('stem', E)
    if k == kF: return ('fork', E, None)
    if k == kFL: return ('fork', SE, None)
    if k == kFX: return ('fork', SE, E)
    if k == kFY: return ('fork', E, SE)
    return None
