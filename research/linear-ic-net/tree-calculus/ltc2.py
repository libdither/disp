"""LTC2: nine rules, every one eats exactly three △ of the redex. Only K drops, only W copies.
A function is a fork; its left child plus one more node pick the rule:
  left child △   -> the right child's top node picks: I, K, B
  left child △p  -> p's top node picks: W, C, C'
  left child △cd -> the argument's top node picks: triage (hands back the arms it does not take)"""
from rules import L, A, spine, size, is_var, match, subst, Need, show
LTC2 = [
    ('I',  A(L, L, L, 'x'), 'x'),
    ('K',  A(L, L, A(L, 'q'), 'x'), 'q'),
    ('B',  A(L, L, A(L, 'q', 'r'), 'x'), A('q', A('r', 'x'))),
    ('W',  A(L, A(L, L), 'b', 'x'), A('b', 'x', 'x')),
    ('C',  A(L, A(L, A(L, 'f')), 'b', 'x'), A('f', 'x', 'b')),
    ('C2', A(L, A(L, A(L, 'f', 'g')), 'b', 'x'), A('f', A('g', 'x'), 'b')),
    ('T0', A(L, A(L, 'c', 'd'), 'b', L), A('c', 'd', 'b')),
    ('T1', A(L, A(L, 'c', 'd'), 'b', A(L, 'u')), A('d', 'c', 'b', 'u')),
    ('T2', A(L, A(L, 'c', 'd'), 'b', A(L, 'u', 'v')), A('b', 'c', 'd', 'u', 'v')),
]
# variant: triage hands back the two arms packed in one fork (it then makes one △)
LTC2P = LTC2[:6] + [
    ('T0', A(L, A(L, 'c', 'd'), 'b', L), A('c', A(L, 'd', 'b'))),
    ('T1', A(L, A(L, 'c', 'd'), 'b', A(L, 'u')), A('d', A(L, 'c', 'b'), 'u')),
    ('T2', A(L, A(L, 'c', 'd'), 'b', A(L, 'u', 'v')), A('b', A(L, 'c', 'd'), 'u', 'v')),
]

# combinators as trees
I = A(L, L, L)
def K(q): return A(L, L, A(L, q))
def B(q, r): return A(L, L, A(L, q, r))
def W(b): return A(L, A(L, L), b)
def C(f, b): return A(L, A(L, A(L, f)), b)
def C2(f, g, b): return A(L, A(L, A(L, f, g)), b)
def is_B(t):
    h, a = spine(t)
    if h != L or len(a) != 2 or a[0] != L: return None
    h2, a2 = spine(a[1])
    return (a2[0], a2[1]) if h2 == L and len(a2) == 2 else None

import itertools
fresh = (f'_y{i}' for i in itertools.count())
def occurs(x, m):
    return m == x or (isinstance(m, tuple) and len(m) == 2 and (occurs(x, m[0]) or occurs(x, m[1])))
def rename(m, x, y):
    if m == x: return y
    if isinstance(m, tuple): return (rename(m[0], x, y), rename(m[1], x, y))
    return m

def br(x, m):
    """[x]m: x itself -> I, absent -> K, right only -> B, left only -> C (C' when [x]left is a B), both -> one W."""
    if m == x: return I
    if not occurs(x, m): return K(m)
    p, q = m
    if q == x and not occurs(x, p): return p
    if not occurs(x, p): return B(p, br(x, q))
    if not occurs(x, q):
        f = br(x, p); fg = is_B(f)
        return C2(fg[0], fg[1], q) if fg else C(f, q)
    y = next(fresh)
    return W(br(x, br(y, (p, rename(q, x, y)))))

def compile2(t):
    if isinstance(t, tuple) and len(t) == 3: return br(t[1], compile2(t[2]))
    if isinstance(t, tuple): return (compile2(t[0]), compile2(t[1]))
    return t

def tri2(c, d, b, x, packed=False):
    """tree calculus's triage (leaf -> c, stem u -> d u, fork u v -> b u v) as LTC2 code: each arm drops what it beat"""
    w = (lambda a: K(a)) if packed else (lambda a: K(K(a)))
    return A(L, A(L, w(c), w(d)), w(b), x)

def wait2(f, x):
    """a value that, applied to v, runs f x v without running f x first"""
    return C(A(L, A(L, A(L, f))), x)   # C (C f) x

def fire2(args, rules):
    need = None
    for name, lhs, rhs in rules:
        _, pargs = spine(lhs)
        env, ok = {}, True
        for i, (pp, aa) in enumerate(zip(pargs, args[:3])):
            try:
                if not match(pp, aa, env): ok = False; break
            except Need:
                ok = False
                if need is None: need = i
                break
        if ok: return ('done', A(subst(rhs, env), *args[3:]), name, env)
    return ('need', need) if need is not None else None
