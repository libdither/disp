"""Baseline for the split/join family: one walker on a route, on a site-and-edge grid.
Sites sit at (even, even), edges between them, (odd, odd) stays empty, so every block is one site with two edges:
E and S on even ticks, N and W on odd ticks. A token at a site on an even tick heads down, on an odd tick heads up.
Everything a run reads on the way in it reads again on the way out, since a block can't forget: the second half mirrors the first."""
from physics import I, A
from laws import Lay, Link
NAME = 'splitjoin-walker'
EDGE, V, H, C, LF, ST, FK, LK, OUT, RB = range(1, 11)
SEQ_S, SEQ_E, DAG_E, M1_S, M1_E, M2_S, M2_E, XL, XR, XI, XT = range(1, 12)
_n = ('OP2 ASK NX BACK KF KS KL NXR KFB KSB KLB AB '      # carriers
      'R P1 P2 PF RF RS RET RET2 OP2U ASKU DAGU SEQU TRIU MAPU MB MB2 MA MA2 ML MLB MU NXU NXRU BB B1 B2 ASKD MAPD PFB RFB DAGB SEQB MBB MAB MA2B KFU KSU KFX KSX NRX NXQ BC WV WB SW SWU W1 W2').split()
globals().update({k: i + 1 for i, k in enumerate(_n)})
RANGES = [11, 12, 2, 3, 4, len(_n) + 1, 2]
E_, S_, W_, N_ = (1, 0), (0, 1), (-1, 0), (0, -1)
_lit = lambda v: I(v) if isinstance(v, int) else A(v)
NONE = (I(0),) * 4
def car(w, a, d='d'): return (I(1), _lit(w), _lit(a), _lit(d))
def evt(a, d='d'): return (I(2), I(0), I(a), _lit(d))
def site(k, r=0, c=0, t=NONE): return (I(k), _lit(r), _lit(c)) + t
EMPTY = (I(0),) * 7
def edge(t=NONE): return site(EDGE, 0, 0, t)
def anyT(n='x'): return tuple(A(n + f) for f in 'cwad')
E0 = edge()
RX = []
def pair(l, r): RX.append((l, r))
def even(s, e=E0, so=E0): return (s, e, so, EMPTY)
def odd(s, n=E0, w=EMPTY): return (EMPTY, n, w, s)
def fk(r, c=0, t=NONE): return site(FK, r, c, t)
def st(r=0, t=NONE): return site(ST, r, 0, t)
def lf(t=NONE): return site(LF, 0, 0, t)
def rb(t=NONE): return site(RB, 0, 0, t)
def lk(t=NONE): return site(LK, 0, 0, t)
ST0 = lambda t=NONE: st(0, t)
ST1 = lambda t=NONE: st(1, t)
F0 = lambda t=NONE: fk(0, 0, t)

def thin(mk, a, b):
    """a site with only a south arm: tokens a in the site, b on the edge"""
    if mk is lf: pair((lf(a[0]), EMPTY, EMPTY, EMPTY), (lf(b[0]), EMPTY, EMPTY, EMPTY))
    else: pair((mk(a[0]), EMPTY, edge(a[1]), EMPTY), (mk(b[0]), EMPTY, edge(b[1]), EMPTY))
def wide(mk, a, b):
    """a fork: (site token, E token, S token)"""
    pair((mk(a[0]), edge(a[1]), edge(a[2]), EMPTY), (mk(b[0]), edge(b[1]), edge(b[2]), EMPTY))
ROUTES = ((SEQ_S, 'S'), (SEQ_E, 'E'), (DAG_E, 'E'), (M1_S, 'S'), (M1_E, 'E'), (M2_S, 'S'), (M2_E, 'E'))

def wires():
    x = anyT()
    pair((site(V, 0, 0, x), EMPTY, E0, EMPTY), (site(V), EMPTY, edge(x), EMPTY))
    pair((site(C, 0, 0, x), EMPTY, E0, EMPTY), (site(C), EMPTY, edge(x), EMPTY))
    pair((site(H, 0, 0, x), E0, EMPTY, EMPTY), (site(H), edge(x), EMPTY, EMPTY))
    pair(odd(site(V, 0, 0, x)), odd(site(V), edge(x)))
    pair(odd(site(H, 0, 0, x), EMPTY, E0), odd(site(H), EMPTY, edge(x)))
    pair(odd(site(C, 0, 0, x), EMPTY, E0), odd(site(C), EMPTY, edge(x)))
    for k in (LF, ST, FK):
        pair(odd(site(k, 'r', 'c', x)), odd(site(k, 'r', 'c'), edge(x)))
    pair(odd(rb(x)), odd(rb(), edge(x)))
    # routed cells pass carriers along the routed arm
    c = car('w', 'a')
    thin(ST1, (c, NONE), (NONE, c))
    for r, arm in ROUTES:
        wide(lambda t, r=r: fk(r, 0, t), (c, NONE, NONE), (NONE, NONE, c) if arm == 'S' else (NONE, c, NONE))
    for r, cc, arm in ((XL, 0, 'S'), (XL, 1, 'E'), (XR, 0, 'E'), (XR, 1, 'S')):
        wide(lambda t, r=r, cc=cc: fk(r, cc, t), (c, NONE, NONE), (NONE, NONE, c) if arm == 'S' else (NONE, c, NONE))

def ends():
    """the link: start, exit, and the crossing, which renames a carrier so landing and passing can't be confused"""
    wide(lk, (evt(R, 1), NONE, NONE), (NONE, NONE, evt(R, 1)))
    wide(lk, (evt(RET, 1), NONE, NONE), (NONE, NONE, evt(RET, 1)))
    pair((EMPTY, EMPTY, EMPTY, lk(evt(RET, 1))), (EMPTY, site(OUT), EMPTY, lk()))
    for t in (evt(R), evt(RET), car('w', 'a')):
        thin(rb, (t, NONE), (NONE, t))
    wide(lk, (NONE, NONE, car(0, 'a')), (NONE, car(2, 'a'), NONE))
    wide(lk, (NONE, car(1, 'a'), NONE), (NONE, NONE, car(3, 'a')))
    for a in (OP2, ASK, WV):
        thin(ST0, (NONE, car(0, a)), (car(0, a), NONE))

def reading():
    """forward read of a program node, and its mirror"""
    thin(ST0, (evt(R), NONE), (NONE, evt(P1)))
    thin(ST0, (evt(P1), NONE), (NONE, evt(P2)))
    thin(lf, (evt(P2), NONE), (car(0, OP2), NONE))
    thin(lf, (evt(R), NONE), (evt(RET), NONE))
    wide(F0, (evt(P1), NONE, NONE), (NONE, NONE, evt(PF)))
    wide(F0, (evt(PF), NONE, NONE), (evt(MAPU), NONE, NONE))
    wide(F0, (NONE, NONE, evt(MAPU)), (car(0, ASK), NONE, NONE))
    wide(F0, (evt(R), NONE, NONE), (NONE, NONE, evt(RF)))
    thin(lf, (evt(RF), NONE), (evt(DAGU), NONE))
    thin(ST0, (evt(RF), NONE), (evt(SEQU), NONE))
    wide(F0, (evt(RF), NONE, NONE), (evt(TRIU), NONE, NONE))
    for d in (0, 1):
        mk = lambda t: fk(DAG_E, 0, t)
        pair((F0(), E0, edge(evt(DAGU, d)), EMPTY), (mk(NONE), edge(evt(R, 1 - d)), E0, EMPTY))
        pair((mk(NONE), edge(evt(RET, d)), E0, EMPTY), (F0(), E0, edge(evt(DAGB, 1 - d)), EMPTY))
    pair((F0(), E0, edge(evt(SEQU, 1)), EMPTY), (fk(SEQ_S), E0, edge(evt(RS, 1)), EMPTY))
    pair((F0(), E0, edge(evt(SEQU, 0)), EMPTY), (fk(SEQ_E), edge(evt(R, 0)), E0, EMPTY))
    pair((st(0, evt(RS)), EMPTY, E0, EMPTY), (st(1), EMPTY, edge(evt(R)), EMPTY))
    pair((st(1), EMPTY, edge(evt(RET)), EMPTY), (st(0, evt(RET2)), EMPTY, E0, EMPTY))
    pair((fk(SEQ_S), E0, edge(evt(RET2, 1)), EMPTY), (fk(SEQ_E), edge(evt(R, 1)), E0, EMPTY))
    pair((fk(SEQ_E), edge(evt(RET, 1)), E0, EMPTY), (F0(), E0, edge(evt(SEQB, 1)), EMPTY))
    pair((fk(SEQ_E), edge(evt(RET, 0)), E0, EMPTY), (fk(SEQ_S), E0, edge(evt(RS, 0)), EMPTY))
    pair((fk(SEQ_S), E0, edge(evt(RET2, 0)), EMPTY), (F0(), E0, edge(evt(SEQB, 0)), EMPTY))
    thin(lf, (evt(DAGB), NONE), (evt(RFB), NONE))
    thin(ST0, (evt(SEQB), NONE), (evt(RFB), NONE))
    wide(F0, (NONE, NONE, evt(RFB)), (evt(RET), NONE, NONE))
    # the mirror of reading a swap: down to the leaf, back up counting
    thin(ST0, (car(3, BACK), NONE), (NONE, evt(BB)))
    thin(ST0, (evt(BB), NONE), (NONE, evt(BC)))
    thin(lf, (evt(BC), NONE), (evt(B2), NONE))
    thin(ST0, (NONE, evt(B2)), (evt(B1), NONE))
    thin(ST0, (NONE, evt(B1)), (evt(RET), NONE))

KIDS = [(lambda t: st(0, t)), lf, (lambda t: fk(0, 0, t)), (lambda t: fk(0, 1, t))]
def bounce(a, b):
    """a token lands on an unrouted cell of any kind and turns back as b"""
    thin(ST0, (a, NONE), (b, NONE)); thin(lf, (a, NONE), (b, NONE))
    for c in (0, 1): pair((fk(0, c, a), E0, E0, EMPTY), (fk(0, c, b), E0, E0, EMPTY))

def data():
    """what a walker does where it lands in the data"""
    at = lambda arm, t: (edge(t), E0) if arm == 'E' else (E0, edge(t))
    for c in (0, 1):
        lft, rgt = ('S', 'E') if c == 0 else ('E', 'S')
        pair((fk(0, c, car(2, OP2)), E0, E0, EMPTY), (fk(0, 1 - c, car(1, BACK)), E0, E0, EMPTY))
        pair((fk(0, c, car(2, ASK)), E0, E0, EMPTY), (fk(XL, c), *at(lft, evt(KFX)), EMPTY))
        pair((fk(XL, c), *at(lft, evt(NXU)), EMPTY), (fk(XR, c), *at(rgt, evt(NRX)), EMPTY))
        pair((fk(XR, c), *at(rgt, evt(KFU)), EMPTY), (fk(0, c, car(1, AB)), E0, E0, EMPTY))
    pair((st(0, car(2, ASK)), EMPTY, E0, EMPTY), (st(1), EMPTY, edge(evt(KSX)), EMPTY))
    pair((st(0, car(2, OP2)), EMPTY, E0, EMPTY), (st(0, car(1, BACK)), EMPTY, E0, EMPTY))
    thin(lf, (car(2, OP2), NONE), (car(1, BACK), NONE))
    thin(lf, (car(2, ASK), NONE), (car(1, KL), NONE))
    thin(lf, (car(2, KLB), NONE), (car(1, AB), NONE))
    bounce(evt(KFX), car(1, KF)); bounce(evt(KSX), car(1, KS)); bounce(evt(NRX), car(1, NXR))
    bounce(car(2, NX), evt(NXU)); bounce(car(2, KFB), evt(KFU)); bounce(car(2, KSB), evt(KSU))
    pair((st(1), EMPTY, edge(evt(KSU)), EMPTY), (st(0, car(1, AB)), EMPTY, E0, EMPTY))

def maps():
    """the middle of a map on the program side"""
    pair((st(0, car(3, KF)), EMPTY, E0, EMPTY), (st(1), EMPTY, edge(evt(MB)), EMPTY))
    pair((st(0, car(3, KS)), EMPTY, E0, EMPTY), (st(1), EMPTY, edge(evt(MA)), EMPTY))
    pair((st(0, car(3, KL)), EMPTY, E0, EMPTY), (st(1), EMPTY, edge(evt(ML)), EMPTY))
    pair((F0(evt(MB)), E0, E0, EMPTY), (fk(M1_S), E0, edge(evt(MB2)), EMPTY))
    pair((F0(evt(MB2)), E0, E0, EMPTY), (fk(M2_E), edge(evt(R)), E0, EMPTY))
    pair((F0(evt(MA)), E0, E0, EMPTY), (fk(M1_S), E0, edge(evt(MA2)), EMPTY))
    pair((F0(evt(MA2)), E0, E0, EMPTY), (fk(M2_S), E0, edge(evt(R)), EMPTY))
    pair((F0(evt(ML)), E0, E0, EMPTY), (F0(evt(MLB)), E0, E0, EMPTY))
    # b done: tell the data to move to its right part; the answer turns back at b's top; c starts
    pair((fk(M2_E), edge(evt(RET)), E0, EMPTY), (fk(M2_E), edge(evt(NXQ)), E0, EMPTY))
    bounce(evt(NXQ), car(0, NX)); bounce(car(3, NXR), evt(NXRU))
    pair((fk(M2_E), edge(evt(NXRU)), E0, EMPTY), (F0(evt(MU)), E0, E0, EMPTY))
    pair((fk(M1_S), E0, edge(evt(MU)), EMPTY), (fk(M1_E), edge(evt(R)), E0, EMPTY))
    # all done: unwind the map's prologue
    pair((fk(M1_E), edge(evt(RET)), E0, EMPTY), (F0(evt(MBB)), E0, E0, EMPTY))
    pair((fk(M2_S), E0, edge(evt(RET)), EMPTY), (F0(evt(MA2B)), E0, E0, EMPTY))
    pair((fk(M1_S), E0, edge(evt(MA2B)), EMPTY), (F0(evt(MAB)), E0, E0, EMPTY))
    pair((st(1), EMPTY, edge(evt(MBB)), EMPTY), (st(0, car(0, KFB)), EMPTY, E0, EMPTY))
    pair((st(1), EMPTY, edge(evt(MAB)), EMPTY), (st(0, car(0, KSB)), EMPTY, E0, EMPTY))
    pair((st(1), EMPTY, edge(evt(MLB)), EMPTY), (st(0, car(0, KLB)), EMPTY, E0, EMPTY))
    pair((st(0, car(3, AB)), EMPTY, E0, EMPTY), (st(0), EMPTY, edge(evt(ASKD)), EMPTY))
    pair((F0(evt(ASKD)), E0, E0, EMPTY), (F0(), E0, edge(evt(MAPD)), EMPTY))
    pair((F0(evt(MAPD)), E0, E0, EMPTY), (F0(evt(PFB)), E0, E0, EMPTY))
    pair((F0(), E0, edge(evt(PFB)), EMPTY), (F0(evt(B1)), E0, E0, EMPTY))

def fix_trigger():
    """any fix is read as deep swap: one trip to the data, no route, no return trips (not a real fix: see REPORT.txt)"""
    thin(lf, (evt(PF), NONE), (car(0, WV), NONE))
    wide(F0, (NONE, NONE, car(0, WV)), (car(0, WV), NONE, NONE))
    pair((st(0, car(3, WB)), EMPTY, E0, EMPTY), (st(0), EMPTY, edge(evt(W1)), EMPTY))
    pair((F0(evt(W1)), E0, E0, EMPTY), (F0(), E0, edge(evt(W2)), EMPTY))
    thin(lf, (evt(W2), NONE), (evt(PFB), NONE))
    pair((st(0, car(2, WV)), EMPTY, E0, EMPTY), (st(1), EMPTY, edge(evt(SW)), EMPTY))
    pair((st(1), EMPTY, edge(evt(SWU)), EMPTY), (st(0, car(1, WB)), EMPTY, E0, EMPTY))
    thin(lf, (car(2, WV), NONE), (car(1, WB), NONE))
    thin(lf, (evt(SW), NONE), (evt(SWU), NONE))
    thin(ST0, (evt(SW), NONE), (NONE, evt(SW)))
    thin(ST0, (NONE, evt(SWU)), (evt(SWU), NONE))

def sweep():
    """one token walks the data depth first, flipping every fork on the way in"""
    for c in (0, 1):
        pair((fk(0, c, evt(SW)), E0, E0, EMPTY), (fk(XI, 1 - c), E0, edge(evt(SW)), EMPTY))
        pair((fk(XI, c), E0, edge(evt(SWU)), EMPTY), (fk(XI, c), edge(evt(SW)), E0, EMPTY))
        pair((fk(XI, c), edge(evt(SWU)), E0, EMPTY), (fk(0, c, evt(SWU)), E0, E0, EMPTY))
        pair((fk(0, c, car(2, WV)), E0, E0, EMPTY), (fk(XT, 1 - c), E0, edge(evt(SW)), EMPTY))
        pair((fk(XT, c), E0, edge(evt(SWU)), EMPTY), (fk(XT, c), edge(evt(SW)), E0, EMPTY))
        pair((fk(XT, c), edge(evt(SWU)), E0, EMPTY), (fk(0, c, car(1, WB)), E0, E0, EMPTY))

def wave():
    """the token splits at every fork; echoes join only if both arrive on the same tick (no other join can exist)"""
    for c in (0, 1):
        pair((fk(0, c, evt(SW)), E0, E0, EMPTY), (fk(XI, 1 - c), edge(evt(SW)), edge(evt(SW)), EMPTY))
        pair((fk(XI, c), edge(evt(SWU)), edge(evt(SWU)), EMPTY), (fk(0, c, evt(SWU)), E0, E0, EMPTY))
        pair((fk(0, c, car(2, WV)), E0, E0, EMPTY), (fk(XT, 1 - c), edge(evt(SW)), edge(evt(SW)), EMPTY))
        pair((fk(XT, c), edge(evt(SWU)), edge(evt(SWU)), EMPTY), (fk(0, c, car(1, WB)), E0, E0, EMPTY))

import os
wires(); ends(); reading(); data(); maps(); fix_trigger()
BASE = len(RX)
(wave if os.environ.get('SPLITJOIN') == 'wave' else sweep)()
REACTIONS = RX
SHAPES = 'see REPORT.txt'

def lay_equal(t):
    """both arms of every fork the same length, only for the wave test"""
    cells, e = {}, (EDGE, 0, 0, 0, 0, 0, 0)
    def width(t):
        if t == 'L': return 1
        return width(t[1]) if t[0] == 'S' else width(t[1]) + width(t[2])
    def down(x, y, n):
        for i in range(n):
            cells[(x, y + 2 * i + 1)] = e
            if i < n - 1: cells[(x, y + 2 * i + 2)] = (V, 0, 0, 0, 0, 0, 0)
    def go(t, x, y):
        if t == 'L': cells[(x, y)] = (LF, 0, 0, 0, 0, 0, 0); return
        if t[0] == 'S':
            cells[(x, y)] = (ST, 0, 0, 0, 0, 0, 0); cells[(x, y + 1)] = e; go(t[1], x, y + 2); return
        wl = width(t[1]); cells[(x, y)] = (FK, 0, 0, 0, 0, 0, 0)
        L = wl + 1
        down(x, y, L); go(t[1], x, y + 2 * L)
        for i in range(1, 2 * wl): cells[(x + i, y)] = e if i % 2 else (H, 0, 0, 0, 0, 0, 0)
        rx = x + 2 * wl; cells[(rx, y)] = (C, 0, 0, 0, 0, 0, 0)
        down(rx, y, L - wl); go(t[2], rx, y + 2 * (L - wl))
    go(t, 0, 0)
    return Lay(cells, (0, 0), S_)

def lay(t):
    cells = {}
    def go(t, x, y):
        if t == 'L':
            cells[(x, y)] = (LF, 0, 0, 0, 0, 0, 0); return 1
        if t[0] == 'S':
            cells[(x, y)] = (ST, 0, 0, 0, 0, 0, 0); cells[(x, y + 1)] = (EDGE, 0, 0, 0, 0, 0, 0)
            return go(t[1], x, y + 2)
        cells[(x, y)] = (FK, 0, 0, 0, 0, 0, 0); cells[(x, y + 1)] = (EDGE, 0, 0, 0, 0, 0, 0)
        wl = go(t[1], x, y + 2)
        for i in range(1, 2 * wl):
            cells[(x + i, y)] = (EDGE if i % 2 else H, 0, 0, 0, 0, 0, 0)
        rx = x + 2 * wl
        cells[(rx, y)] = (C, 0, 0, 0, 0, 0, 0); cells[(rx, y + 1)] = (EDGE, 0, 0, 0, 0, 0, 0)
        return wl + go(t[2], rx, y + 2)
    go(t, 0, 0)
    return Lay(cells, (0, 0), S_)

if os.environ.get('SPLITJOIN_LAY') == 'equal': lay = lay_equal

def link(dp, dx):
    X = dp.w + (dp.w & 1)
    e = (EDGE, 0, 0, 0, 0, 0, 0)
    cells = {(0, 0): (LK, 0, 0, 2, 0, R, 1), (0, 1): e, (0, 2): (RB, 0, 0, 0, 0, 0, 0), (0, 3): e}
    for i in range(1, X):
        cells[(i, 0)] = e if i % 2 else (H, 0, 0, 0, 0, 0, 0)
    cells[(X, 0)] = (C, 0, 0, 0, 0, 0, 0); cells[(X, 1)] = e
    return Link((0, 4), (X, 2), cells, 0)

def node(cell, step):
    k = cell[0]
    if k == EDGE: return ('pass', step) if step in (E_, S_, W_, N_) else None
    if k == H: return ('pass', step) if step in (E_, W_) else None
    if k == V: return ('pass', step) if step in (S_, N_) else None
    if k == C: return ('pass', S_) if step == E_ else None
    if k == LF: return ('leaf',)
    if k == ST: return ('stem', S_)
    if k == FK: return ('fork', S_, E_) if cell[2] == 0 else ('fork', E_, S_)
    return None
