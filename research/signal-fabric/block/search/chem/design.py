"""The smallest piece: @ meets map(L,L,L) on a fork F(L,L), in cells, under the real harness.
One token and one wire at a time, since a split can't rejoin at a sane cost (proto_join.py). The wire from the current program
part to the current data part lies on the tree's own edges; each fork on it holds a live arm. Runs only that program:
see map_check.py. Reactions and shapes here are the counted example; nothing past it is implemented."""
from physics import I, A
from laws import Lay, Link
NAME = 'chem-map-piece'
#          kind n e s w class payload
RANGES = [5, 2, 2, 2, 2, 3, 18]
WIRE, LF, ST, FK, JN = 0, 1, 2, 3, 4
DN, EA, SO, WE = (0, -1), (1, 0), (0, 1), (-1, 0)
PORTS = {DN: 1, EA: 2, SO: 3, WE: 4}
D0, D1, D2, BGO, BGO1, BGO2, BU, BGO3, XB, XB2, CGO, CGO1, CGO2, RU, CGO3, XC, XC2, OUT = range(18)
T1, T2 = 1, 2   # class 1 crosses any live edge; class 2 acts at nodes, and only crosses wire by itself

def cell(k=WIRE, n=0, e=0, s=0, w=0, c=0, v=0): return (k, n, e, s, w, c, v)
VOID = cell()
def lit(*fs): return tuple(I(f) for f in fs)
_n = [0]
def var():
    _n[0] += 1
    return A(_n[0])
def body(**fix):
    """a cell's six non-token fields; a name given is literal, the rest are free"""
    return tuple(I(fix[f]) if f in fix else var() for f in ('k', 'n', 'e', 's', 'w'))
def idle(): return body() + (I(0), I(0))

def carry(cls):
    """a token crosses a live edge, one reaction for each place an edge can sit in a block. cls 2 keeps off nodes when leaving down or across."""
    rs = []
    nodes = cls == 2
    def side(a_fix, b_fix, place):
        a, b, v = body(**a_fix), body(**b_fix), var()
        o1, o2 = idle(), idle()
        tok = (I(cls), v); no = (I(0), I(0))
        l = {place[0]: a + tok, place[1]: b + no, place[2]: o1, place[3]: o2}
        r = {place[0]: a + no, place[1]: b + tok, place[2]: o1, place[3]: o2}
        rs.append((tuple(l[i] for i in range(4)), tuple(r[i] for i in range(4))))
    NW, NE, SW, SE = 0, 1, 2, 3
    k = {'k': WIRE} if nodes else {}
    side(dict(s=1, e=0, **k), dict(n=1, e=0), (NW, SW, NE, SE))      # left column
    side(dict(s=1, w=0), dict(n=1, w=0), (NE, SE, NW, SW))           # right column
    side(dict(e=1, s=0, **k), dict(w=1, s=0), (NW, NE, SW, SE))      # top row
    side(dict(e=1, n=0, **k), dict(w=1, n=0), (SW, SE, NW, NE))      # bottom row
    return rs

def R():
    rs = carry(T1) + carry(T2)
    free = idle
    tok = lambda c=0, v=0: (I(c), I(v))
    fx = lambda d: tuple(I(d[f]) for f in ('k', 'n', 'e', 's', 'w'))
    fk0 = dict(k=FK, n=1, e=0, s=0, w=0)
    fkS = dict(k=FK, n=1, e=0, s=1, w=0)
    fkE = dict(k=FK, n=1, e=1, s=0, w=0)
    st = dict(k=ST, n=1, e=0, s=1, w=0)
    leaf = dict(k=LF, n=1, e=0, s=0, w=0)
    armS, armE = dict(n=1, e=0), dict(w=1, s=0)
    def step(fro, to, place, t_node, t_arm):
        """node at NW goes fro -> to while a token crosses between it and an arm; either way, as all reactions"""
        arm, o1, o2 = body(**(armS if place == 'S' else armE)), free(), free()
        if place == 'S':
            rs.append(((fx(fro) + tok(*t_node), o1, arm + tok(), o2), (fx(to) + tok(), o1, arm + tok(*t_arm), o2)))
        else:
            rs.append(((fx(fro) + tok(*t_node), arm + tok(), o1, o2), (fx(to) + tok(), arm + tok(*t_arm), o1, o2)))
    def flip(d, a, b):
        """a token changes in place on a node, and its heading then takes it back up"""
        o1, o2, o3 = free(), free(), free()
        rs.append(((fx(d) + tok(*a), o1, o2, o3), (fx(d) + tok(*b), o1, o2, o3)))
    # read the program: stem, fork, fork; each fork takes a live arm; b's leaf turns the token for the trip to the data
    step(st, st, 'S', (T2, D0), (T2, D1))
    step(fk0, fkS, 'S', (T2, D1), (T2, D2))
    step(fk0, fkE, 'E', (T2, D2), (T2, BGO))
    flip(leaf, (T2, BGO), (T1, BGO1))
    # the data's fork takes its left arm; l's leaf turns the token; the data's fork lets go
    step(fk0, fkS, 'S', (T1, BGO1), (T2, BGO2))
    flip(leaf, (T2, BGO2), (T2, BU))
    step(fk0, fkS, 'S', (T1, BGO3), (T2, BU))
    # back at b: done with it; the first fork lets go, the second switches to c in one block
    flip(leaf, (T1, BGO3), (T2, XB))
    step(fk0, fkE, 'E', (T2, XB2), (T2, XB))
    arm, e1, o2 = body(**armS), body(**armE), free()
    rs.append(((fx(fkS) + tok(), e1 + tok(), arm + tok(T2, XB2), o2), (fx(fkE) + tok(), e1 + tok(T2, CGO), arm + tok(), o2)))
    # c's leaf turns the token; the data's fork takes its right arm; r's leaf turns it; the data's fork lets go
    flip(leaf, (T2, CGO), (T1, CGO1))
    step(fk0, fkE, 'E', (T1, CGO1), (T2, CGO2))
    flip(leaf, (T2, CGO2), (T2, RU))
    step(fk0, fkE, 'E', (T1, CGO3), (T2, RU))
    # back at c: done; the fork lets go; the stem sends the head home
    flip(leaf, (T1, CGO3), (T2, XC))
    step(fk0, fkE, 'E', (T2, XC2), (T2, XC))
    step(st, st, 'S', (T2, OUT), (T2, XC2))
    # the link: into the junction from the port wire, the one diagonal hop, and out at the top
    jn = dict(k=JN, n=0, e=1, s=1, w=0)
    up = dict(k=WIRE, n=1, e=0, s=1, w=0)
    v, o1, o2 = var(), free(), free()
    rs.append(((fx(up) + (I(T2), v), o1, fx(jn) + tok(), o2), (fx(up) + tok(), o1, fx(jn) + (I(T2), v), o2)))
    hop = dict(k=WIRE, n=0, e=0, s=0, w=1)
    v, root = var(), body(n=1)
    nil = lit(*VOID)
    rs.append(((fx(hop) + (I(T1), v), nil, nil, root + tok()), (fx(hop) + tok(), nil, nil, root + (I(T1), v))))
    rs.append(((nil, nil, nil, fx(up) + tok(T2, OUT)), (nil, lit(WIRE, 0, 0, 0, 0, T2, OUT), nil, fx(up) + tok())))
    return rs

REACTIONS = R()
SHAPES = 'carry 2 (x4 places); node step 10; in-place 6; switch 1; link 3'

def lay(t):
    cells = {}
    def go(t, x, y):
        if t == 'L':
            cells[(x, y)] = cell(LF, n=1); return 1
        if t[0] == 'S':
            cells[(x, y)] = cell(ST, n=1, s=1); cells[(x, y + 1)] = cell(n=1, s=1)
            return go(t[1], x, y + 2)
        cells[(x, y)] = cell(FK, n=1); cells[(x, y + 1)] = cell(n=1, s=1)
        wl = go(t[1], x, y + 2)
        for i in range(1, 2 * wl): cells[(x + i, y)] = cell(e=1, w=1)
        cx = x + 2 * wl
        cells[(cx, y)] = cell(w=1, s=1); cells[(cx, y + 1)] = cell(n=1, s=1)
        return wl + go(t[2], cx, y + 2)
    go(t, 0, 0)
    return Lay(cells, (0, 0), SO)

def link(dp, dx):
    X = dp.w + dp.w % 2 + 2
    cells = {(0, 0): cell(n=1, s=1, c=T2, v=D0), (0, 1): cell(n=1, s=1), (0, 2): cell(n=1, s=1), (0, 3): cell(JN, e=1, s=1)}
    for x in range(1, X - 1): cells[(x, 3)] = cell(e=1, w=1)
    cells[(X - 1, 3)] = cell(w=1)
    return Link((0, 4), (X, 4), cells, 0)

def node(c, step):
    k = c[0]
    if k == LF: return ('leaf',)
    if k == ST: return ('stem', SO)
    if k == FK: return ('fork', SO, EA)
    if k == WIRE:
        back_ = {EA: WE, WE: EA, SO: DN, DN: SO}[step] if step in (EA, WE, SO, DN) else None
        if back_ is None: return None
        ports = [d for d, i in PORTS.items() if c[i]]
        if back_ not in ports or len(ports) != 2: return None
        out = [d for d in ports if d != back_][0]
        return ('pass', out)
    return None
