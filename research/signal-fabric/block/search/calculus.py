"""The two-way calculus (mirror of ../calculus.bend) and the programs the laws run.
Trees: 'L' | ('S', c) | ('F', l, r)."""
from functools import lru_cache
import sys
sys.setrecursionlimit(100000)

L = 'L'
def S(c): return ('S', c)
def F(l, r): return ('F', l, r)
def is_s(t): return t != L and t[0] == 'S'
def is_f(t): return t != L and t[0] == 'F'

def swap(x): return F(x[2], x[1]) if is_f(x) else x
def cross(x): return F(F(x[1][1], x[2]), x[1][2]) if is_f(x) and is_f(x[1]) else x
def sink(x):
    if is_s(x) and is_f(x[1]): return F(S(x[1][1]), x[1][2])
    if is_f(x) and is_s(x[1]): return S(F(x[1][1], x[2]))
    return x
def stem(x, fwd):
    if fwd: return S(x)
    return x[1] if is_s(x) else None
def unit(x, fwd):
    if fwd: return F(L, x)
    return x[2] if is_f(x) and x[1] == L else None
def rot(x): return F(x[1][1], F(x[1][2], x[2])) if is_f(x) and is_f(x[1]) else x
def unrot(x): return F(F(x[1], x[2][1]), x[2][2]) if is_f(x) and is_f(x[2]) else x

class TooDeep(Exception): pass

def run(p, x, fwd=True, me=None, fuel=4000):
    """p on x: the answer, or None if stuck; raises TooDeep past fuel (a call deep)."""
    if fuel == 0: raise TooDeep()
    k = fuel - 1
    if p == L: return x
    if p[0] == 'F':
        a = p[1]
        if a == L: return run(p[2], x, not fwd, me, k)
        if a[0] == 'S':
            first, second = (a[1], p[2]) if fwd else (p[2], a[1])
            y = run(first, x, fwd, me, k)
            return None if y is None else run(second, y, fwd, me, k)
        aa, b, c = a[1], a[2], p[2]
        if not is_f(x): return x
        if x[1] == L:
            y = run(aa, x[2], fwd, me, k)
            return None if y is None else F(L, y)
        if x[1][0] == 'S':
            y = run(b, F(x[1][1], x[2]), fwd, me, k)
            return F(S(y[1]), y[2]) if y is not None and is_f(y) else None
        y = run(c, rot(x), fwd, me, k)
        return unrot(y) if y is not None and is_f(y) and is_f(y[2]) else None
    c = p[1]
    if is_f(c) and c[1] == L: return run(c[2], x, fwd, p, k)
    if is_f(c) and is_f(c[1]):
        a, b, cc = c[1][1], c[1][2], c[2]
        if is_s(x):
            y = run(a, x[1], fwd, me, k)
            return None if y is None else S(y)
        if is_f(x):
            y = run(b, x[1], fwd, me, k)
            z = run(cc, x[2], fwd, me, k)
            return None if y is None or z is None else F(y, z)
        return L
    if is_f(c): return None
    if c == L:
        return None if me is None else run(me[1][2], x, fwd, me, k)
    n, t = 1, c
    while is_s(t): n, t = n + 1, t[1]
    if t != L: return None
    if n == 2: return swap(x)
    if n == 3: return cross(x)
    if n == 4: return sink(x)
    if n == 5: return stem(x, fwd)
    if n == 6: return unit(x, fwd)
    return None

def size(t):
    if t == L: return 1
    return 1 + size(t[1]) if t[0] == 'S' else 1 + size(t[1]) + size(t[2])

def show(t):
    if t == L: return '△'
    if t[0] == 'S': return '△' + show(t[1])
    return '(' + show(t[1]) + ',' + show(t[2]) + ')'

def tops(t):
    """t, and t after one move at its top, either way. rot is here because tri rotates, and because
    cross = rot ; second(swap) ; rot⁻¹, so a design may have rotation as its physical move."""
    out = {t, swap(t), cross(t), sink(t), S(t), F(L, t), rot(t), unrot(t)}
    if is_s(t): out.add(t[1])
    if is_f(t) and t[1] == L: out.add(t[2])
    return out

@lru_cache(maxsize=200000)
def moved(u, t):
    """u is t after a step of moves: at most one at t's top, then a step in each part of that, all at once."""
    if u == t: return True
    for c in tops(t):
        if u == L and c == L: return True
        if is_s(u) and is_s(c) and moved(u[1], c[1]): return True
        if is_f(u) and is_f(c) and moved(u[1], c[1]) and moved(u[2], c[2]): return True
    return False

def trees(n):
    if n == 0: return []
    ts = trees(n - 1)
    return [L] + [S(t) for t in ts] + [F(a, b) for a in ts for b in ts if size(a) + size(b) <= n - 1]

# Programs by name
def p_stems(n, t=L):
    for _ in range(n): t = S(t)
    return t
ID = L
SELF = S(L)
SWAP, CROSS, SINK, STEM, UNIT = (p_stems(n) for n in (2, 3, 4, 5, 6))
def dag(b): return F(L, b)
def seq(*ps):
    t = ps[-1]
    for a in reversed(ps[:-1]): t = F(S(a), t)
    return t
def tri(a, b, c): return F(F(a, b), c)
def map_(a, b, c): return S(F(F(a, b), c))
def fix(b): return S(F(L, b))
def first(f): return map_(L, f, L)
def second(f): return map_(L, L, f)
def par(f, g): return map_(L, f, g)
def when1(f): return tri(L, second(f), L)
def rep(p, n): return seq(*[p] * n) if n > 1 else p

NOT = seq(UNIT, SWAP, tri(STEM, L, L), SWAP, dag(tri(STEM, L, L)), SWAP, dag(UNIT))
CNOT = when1(NOT)
_c1 = when1(when1(second(first(NOT))))
_c2 = when1(second(when1(first(NOT))))
_c3 = second(when1(when1(first(NOT))))
_sa = second(when1(first(NOT)))
_sc = when1(second(first(NOT)))
ADDB = fix(seq(_c1, _c2, _c3, second(second(second(SELF))), _c3, _c2, _c1, _sa, _sc))
def bit(b): return S(L) if b else L
def register(a, b, n):
    t = L
    for i in range(n - 1, -1, -1): t = F(L, F(bit((a >> i) & 1), F(bit((b >> i) & 1), t)))
    return t

def full(k, leaf=L):
    return leaf if k == 0 else F(full(k - 1, leaf), full(k - 1, leaf))
def deep_swap(): return fix(seq(map_(SELF, SELF, SELF), SWAP))

MARKED = F(F(S(L), L), F(L, L))
def run_trees(): return trees(4) + [MARKED]

# The ladder: each rung's programs and the trees they run on. A rung's law: every run agrees with the calculus.
def rung_swaps():
    return [L, SWAP, seq(SWAP, SWAP), dag(SWAP)], run_trees()
def rung_maps():
    return [first(SWAP), second(SWAP), map_(SWAP, SWAP, SWAP), seq(first(SWAP), SWAP), seq(second(first(SWAP)), SWAP),
            map_(map_(SWAP, L, L), SWAP, second(SWAP)), dag(seq(first(SWAP), SWAP))], run_trees()
def rung_grow():
    """Enough to rule out room kept in the layout (compact_small bounds that): chains, at the top and in parts."""
    ps = [STEM, dag(STEM), first(STEM), second(STEM), seq(STEM, STEM), seq(second(STEM), SWAP), map_(STEM, STEM, STEM), dag(second(STEM)),
          UNIT, dag(UNIT), first(UNIT), second(UNIT), seq(UNIT, SWAP), seq(UNIT, first(STEM)), dag(first(UNIT)),
          rep(STEM, 4), rep(STEM, 12), rep(first(STEM), 8), rep(second(STEM), 8), first(first(rep(STEM, 6))), second(first(rep(STEM, 6))),
          rep(UNIT, 4), rep(UNIT, 8), first(rep(UNIT, 4)), second(rep(UNIT, 4)), seq(UNIT, second(UNIT), first(STEM), second(first(STEM))),
          seq(rep(STEM, 4), dag(rep(STEM, 4))), seq(rep(UNIT, 4), dag(rep(UNIT, 4)))]
    return ps, trees(3) + [F(F(L, L), L), F(L, F(L, L)), F(F(L, L), F(L, L)), MARKED]
def rung_moves():
    return [SINK, CROSS, dag(SINK), first(SINK), second(CROSS), seq(CROSS, SWAP, CROSS), seq(SINK, SINK), map_(SINK, CROSS, SINK)], trees(5) + [F(F(F(L, L), L), L), MARKED]
def rung_tri():
    return [tri(L, L, L), tri(SWAP, L, L), tri(L, SWAP, L), tri(L, L, SWAP), tri(STEM, L, L), dag(tri(STEM, L, L)), when1(SWAP), tri(SWAP, SWAP, second(SWAP)), NOT, CNOT], \
        trees(5) + [F(S(L), F(S(L), L)), F(L, F(S(L), L)), F(F(L, L), F(L, L)), F(S(S(L)), F(L, L))]
def rung_fix():
    flip = fix(seq(first(NOT), second(SELF)))
    return [fix(map_(SELF, SELF, SWAP)), deep_swap(), fix(tri(L, seq(second(SWAP), L), L)), flip], \
        [L, S(L), F(L, L), F(F(L, L), L), F(F(L, L), F(L, L)), F(L, F(S(L), F(L, L))), full(3)]
def rung_add():
    return [ADDB], [register(a, b, n) for n, a, b in [(1, 0, 0), (1, 1, 0), (1, 1, 1), (2, 1, 1), (2, 2, 3)]]
def rung_stuck():
    """Runs the calculus calls stuck (graded, not hard): stem† on no stem, unit† on no unit, a self with no fix, a stem count past the moves."""
    return [dag(STEM), dag(UNIT), SELF, p_stems(7), S(F(S(L), L)), seq(STEM, dag(STEM), dag(STEM)), first(dag(STEM))], [L, F(L, L), F(S(L), L)]

def _rand_tree(r, n):
    if n <= 1: return L
    if r.random() < .3: return S(_rand_tree(r, n - 1))
    k = r.randint(1, n - 1)
    return F(_rand_tree(r, k), _rand_tree(r, n - k))
def _rand_prog(r, depth):
    if depth == 0 or r.random() < .35: return r.choice([L, SWAP, SWAP, CROSS, CROSS, SINK, STEM, UNIT])
    k, g = r.randint(0, 5), lambda: _rand_prog(r, depth - 1)
    return [lambda: seq(g(), g()), lambda: dag(g()), lambda: first(g()), lambda: second(g()), lambda: map_(g(), g(), g()), lambda: tri(g(), g(), g())][k]()
def rung_general():
    """Moves compose: the rungs above can be passed by a machine that only knows their exact programs. Named pairs that
    must compose, then a fixed random sample (seed fixed, so the law is one finite list). Restricts: moves and tri's
    branches work after one another, in any part, on forks already swapped. Free: all geometry and time."""
    import random
    r = random.Random(20261030)
    named = [seq(CROSS, STEM), seq(CROSS, UNIT), seq(CROSS, first(SWAP)), seq(first(SWAP), CROSS), seq(SWAP, CROSS), seq(CROSS, CROSS),
             seq(SWAP, SINK), seq(SWAP, dag(UNIT)), tri(L, L, SWAP), tri(L, L, first(SWAP)), tri(L, L, second(second(SWAP))), tri(L, L, tri(L, L, L)),
             tri(L, L, STEM), tri(L, L, CROSS), seq(tri(L, L, SWAP), CROSS)]
    trees_ = trees(5) + [F(F(F(L, L), L), L), MARKED, F(L, F(L, L)), F(F(S(L), L), S(L)), F(F(F(S(L), L), L), F(L, L))]
    ps = named + [_rand_prog(r, 3) for _ in range(120)]
    xs = trees_ + [_rand_tree(r, r.randint(3, 9)) for _ in range(20)]
    return ps, xs

RUNGS = [('swaps', rung_swaps), ('maps', rung_maps), ('grow', rung_grow), ('moves', rung_moves), ('tri', rung_tri), ('fix', rung_fix), ('add', rung_add), ('general', rung_general)]
