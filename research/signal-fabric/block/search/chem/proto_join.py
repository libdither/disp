"""Can two parallel tokens rejoin? Real cells; a wire stands for any subcomputation of that many ticks.
A token can't stop: a bijection has no way into a resting state or a local loop. So a token whose partner is absent reflects and
its whole subcomputation runs backward, then forward again: waiting = undo and redo.
 ring:   split at a fork (even block), join at a separate node (odd block). The half-period H between them is always odd.
 nested: a ring inside one arm of a ring.
 tree:   each child returns to its own fork, as a tree forces. Split and join are one block, H is even, and a turn-around
         cell at each arm's end marks the token done. Run: python chem/proto_join.py from search/."""
import sys, os
from math import gcd
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
import physics as P
from physics import I
N, E, S, W = (0, -1), (1, 0), (0, 1), (-1, 0)
FORK, JOIN, OUT = 100, 101, 102
void = (0, 0)
def lit(c): return (I(c[0]), I(c[1]))
def kind(a, b): return sum(1 << i for i, d in enumerate((N, E, S, W)) if d in (a, b))

class Scene:
    def __init__(s):
        s.cells, s.edges, s.forks, s.joins, s.ends = {}, [], [], [], []
    def path(s, pts, a_end=None, b_end=None):
        """wire cells pts, joined to the cells a_end before and b_end after; kinds come from the turns"""
        full = ([a_end] if a_end else []) + pts + ([b_end] if b_end else [])
        for p, q in zip(full, full[1:]): s.edges.append((p, q))
        for i, here in enumerate(full):
            if here not in pts: continue
            nb = [full[j] for j in (i - 1, i + 1) if 0 <= j < len(full)]
            d = [(q[0] - here[0], q[1] - here[1]) for q in nb]
            assert here not in s.cells, here
            s.cells[here] = (kind(*d) if len(d) == 2 else kind(d[0], d[0]), 0)
        return len(pts) + 1
    def corner(s, p, q):
        for cx in (min(p[0], q[0]), min(p[0], q[0]) - 1):
            for cy in (min(p[1], q[1]), min(p[1], q[1]) - 1):
                if cx % 2 == cy % 2 and all(a[0] in (cx, cx + 1) and a[1] in (cy, cy + 1) for a in (p, q)): return cx, cy
        raise ValueError((p, q))
    def block(s, c, toks):
        return tuple(lit((s.cells[q][0], toks.get(q, 0)) if q in s.cells else void) for q in ((c[0], c[1]), (c[0] + 1, c[1]), (c[0], c[1] + 1), (c[0] + 1, c[1] + 1)))
    def reactions(s, tokens=(1,)):
        rs = []
        for p, q in s.edges:
            if p in s.forks and q in ((p[0] + 1, p[1]), (p[0], p[1] + 1)): continue   # arms act only through the split
            if q in s.joins and p in ((q[0], q[1] - 1), (q[0] - 1, q[1])): continue
            c = s.corner(p, q)
            for t in tokens: rs.append((s.block(c, {p: t}), s.block(c, {q: t})))
        for f in s.forks:
            arms = {(f[0] + 1, f[1]): 1, (f[0], f[1] + 1): 1}
            rs.append((s.block(f, {f: 1}), s.block(f, arms)))
        for j in s.joins:
            c = (j[0] - 1, j[1] - 1)
            rs.append((s.block(c, {j: 1}), s.block(c, {(j[0], j[1] - 1): 1, (j[0] - 1, j[1]): 1})))
        return rs

def col(x, a, b): return [(x, y) for y in range(a, b + 1)]
def row(y, a, b): return [(x, y) for x in range(a, b + 1)]
def bump(x, y): return [(x, y - 1), (x, y - 2), (x + 1, y - 2), (x + 1, y - 1)]

def start(s):
    """head on a two-cell port wire above the first fork; it leaves upward if the run undoes itself"""
    s.path(col(0, 0, 1), None, (0, 2))
    s.cells[(0, 0)] = (s.cells[(0, 0)][0], 1)

def run(s, watch, tokens=(1,), limit=3000000, done=None):
    rs = s.reactions(tokens)
    tip = s.cells[(0, 0)][0]
    rs.append(((lit(void), lit(void), lit(void), lit((tip, 1))), (lit(void), lit((OUT, 0)), lit(void), lit((tip, 0)))))
    laws = P.linear(rs) and not P.one_partner(rs) and P.void_stays(rs, void)
    w = P.World(s.cells, P.Rule(rs, void))
    w.step(); w.step()
    t0 = w.t
    seen = {}
    while w.t < limit:
        w.step()
        for name, c in watch.items():
            if name not in seen and w.cells.get(c, void)[1] == 1: seen[name] = w.t - t0
        if 'outer' in seen: return 'joins', seen, laws, len(rs)
        if w.outs: return 'undoes itself, leaves', seen, laws, len(rs)
    return 'nothing by limit', seen, laws, len(rs)

def ring(bumps, y_end=10, x_end=8):
    s = Scene(); start(s)
    fork, join = (0, 2), (x_end, y_end)
    s.cells[fork] = (FORK, 0); s.cells[join] = (JOIN, 0); s.forks.append(fork); s.joins.append(join)
    a = col(0, 3, y_end); x = 0
    for i in range(bumps):
        x = 1 + 4 * i
        a += row(y_end, a[-1][0] + 1, x) + bump(x, y_end) + [(x + 1, y_end)]
    a += row(y_end, a[-1][0] + 1, x_end - 1)
    da = s.path(a, fork, join)
    db = s.path(row(2, 1, x_end) + col(x_end, 3, y_end - 1), fork, join)
    s.path(col(x_end, y_end + 1, y_end + 2), join, None)
    return s, da, db, join

def nested(ib, y_end, x_end, xi=10):
    """arm A holds a ring with ib bumps; arm B is plain wire"""
    s = Scene(); start(s)
    fork, join, f2, j2 = (0, 2), (x_end, y_end), (0, 6), (xi, 12)
    for c, k in ((fork, FORK), (join, JOIN), (f2, FORK), (j2, JOIN)): s.cells[c] = (k, 0)
    s.forks += [fork, f2]; s.joins += [join, j2]
    s.path(col(0, 3, 5), fork, f2)
    a = col(0, 7, 12); x = 0
    for i in range(ib):
        x = 1 + 4 * i
        a += row(12, a[-1][0] + 1, x) + bump(x, 12) + [(x + 1, 12)]
    a += row(12, a[-1][0] + 1, xi - 1)
    ia = s.path(a, f2, j2)
    ibb = s.path(row(6, 1, xi) + col(xi, 7, 11), f2, j2)
    s.path(col(xi, 13, y_end) + row(y_end, xi + 1, x_end - 1), j2, join)
    db = s.path(row(2, 1, x_end) + col(x_end, 3, y_end - 1), fork, join)
    s.path(col(x_end, y_end + 1, y_end + 2), join, None)
    return s, ia, ibb, db, join, j2

def tree(a, b):
    """fork at (0,2); arm A down a cells, arm B east 2 then down; each ends in a cell that turns the token into done (2)"""
    s = Scene(); start(s)
    fork = (0, 2)
    s.cells[fork] = (FORK, 0); s.forks.append(fork)
    pa, pb = col(0, 3, 2 + a), row(2, 1, 4) + col(4, 3, 2 + b)
    s.path(pa, fork); s.path(pb, fork)
    return s, pa[-1], pb[-1]

def run_tree(a, b, limit=2000000):
    s, ea, eb = tree(a, b)
    fork = (0, 2)
    rs = s.reactions((1, 2))
    tip = s.cells[(0, 0)][0]
    rs.append(((lit(void), lit(void), lit(void), lit((tip, 1))), (lit(void), lit((OUT, 0)), lit(void), lit((tip, 0)))))
    # the end cell flips 1<->2 in the block holding it and the cell below it
    for e in (ea, eb):
        cx, cy = s.corner(e, (e[0], e[1] + 1))
        spots = ((cx, cy), (cx + 1, cy), (cx, cy + 1), (cx + 1, cy + 1))
        def b_(t): return tuple(lit((s.cells[q][0], t if q == e else 0)) if q in s.cells else lit(void) for q in spots)
        rs.append((b_(1), b_(2)))
    # joined: both arms done, in the fork's block; the fork takes a done token
    arms = {(1, 2): 2, (0, 3): 2}
    rs.append((s.block(fork, {fork: 2}), s.block(fork, arms)))
    laws = P.linear(rs) and not P.one_partner(rs) and P.void_stays(rs, void)
    w = P.World(s.cells, P.Rule(rs, void))
    w.step(); w.step()
    t0 = w.t
    while w.t < limit:
        w.step()
        if w.cells.get(fork, void)[1] == 2: return 'joins done', w.t - t0, laws
        if w.outs: return 'undoes itself, leaves', w.t - t0, laws
    return 'nothing by limit', w.t - t0, laws

def v2(n):
    k = 0
    while n % 2 == 0: n //= 2; k += 1
    return k

if __name__ == '__main__':
    print('ring: split and join in opposite-phase blocks. H = D-1')
    print('DA  DB  outcome  ticks   lcm(HA,HB)+1  laws reactions')
    for bumps, y in ((0, 10), (1, 10), (2, 10), (1, 12), (2, 12), (2, 14)):
        s, da, db, j = ring(bumps, y)
        what, seen, laws, n = run(s, {'outer': j})
        ha, hb = da - 1, db - 1
        print('%-3d %-3d %-8s %-7d %-13d %s %d' % (da, db, what, seen.get('outer', -1), ha * hb // gcd(ha, hb) + 1, laws, n))
    print('\nnested: arm A holds a ring. Each level multiplies')
    print('inner DA DB | inner joins | arm B | outer outcome, ticks')
    for ib, ye, xe in ((0, 20, 12), (1, 20, 12), (1, 22, 14), (2, 22, 14), (2, 24, 16)):
        s, ia, ibb, db, j, j2 = nested(ib, ye, xe)
        what, seen, laws, n = run(s, {'outer': j, 'inner': j2})
        print('%-3d %-7d | %-11s | %-5d | %s %s  laws %s, %d reactions' % (ia, ibb, seen.get('inner'), db, what, seen.get('outer'), laws, n))
    print('\ntree: each child comes back to its own fork, as tree layouts force. H even')
    print('cells on arm A, B | 2-adic | outcome, ticks')
    for a, b in ((2, 2), (4, 4), (2, 4), (2, 6), (4, 6), (2, 8), (4, 8), (6, 10), (4, 12), (6, 12), (8, 16), (3, 5), (3, 7), (5, 9)):
        what, t, laws = run_tree(a, b)
        print('%-3d %-13d | %d %d    | %s %d  laws %s' % (a, b + 4, v2(a), v2(b + 4), what, t, laws))
