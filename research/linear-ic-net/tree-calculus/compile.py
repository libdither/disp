"""λ-terms → TC and LTC by bracket abstraction. λ-terms: 'L', variables (other strings),
pairs (application), ('λ', x, body)."""
import itertools
from rules import *

I = A(L, A(L, L, L), L)                      # triage that rebuilds its argument: same tree in both
def Kc(m): return A(L, L, m)
# LTC
def Wc(m): return A(L, A(L, L), m)
def Bc(f, g): return A(L, A(L, A(L, f)), g)
def Cc(f, g, n): return A(L, A(L, A(L, f, g)), n)
def is_B(t):
    h, a = spine(t)
    if h != L or len(a) != 2: return None
    h1, a1 = spine(a[0])
    if h1 != L or len(a1) != 1: return None
    h2, a2 = spine(a1[0])
    if h2 != L or len(a2) != 1: return None
    return a2[0], a[1]
# TC
def Sc(f, g): return A(L, A(L, f), g)

fresh = (f'_{i}' for i in itertools.count())

def occurs(x, m):
    if m == x: return True
    return isinstance(m, tuple) and len(m) == 2 and (occurs(x, m[0]) or occurs(x, m[1]))

def rename(m, x, y):
    if m == x: return y
    if isinstance(m, tuple): return (rename(m[0], x, y), rename(m[1], x, y))
    return m

def br_ltc(x, m):
    if m == x: return I
    if not occurs(x, m): return Kc(m)
    p, q = m
    if q == x and not occurs(x, p): return p                       # η
    if not occurs(x, p): return Bc(p, br_ltc(x, q))                # x goes right
    if not occurs(x, q):                                           # x goes left
        f = br_ltc(x, p)
        fg = is_B(f)
        return Cc(fg[0], fg[1], q) if fg else Cc(f, I, q)
    y = next(fresh)                                                # x goes both ways: W, once
    return Wc(br_ltc(x, br_ltc(y, (p, rename(q, x, y)))))

def br_tc(x, m):
    if m == x: return I
    if not occurs(x, m): return Kc(m)
    p, q = m
    if q == x and not occurs(x, p): return p
    return Sc(br_tc(x, p), br_tc(x, q))

def compile_(t, br):
    if isinstance(t, tuple) and len(t) == 3: return br(t[1], compile_(t[2], br))
    if isinstance(t, tuple): return (compile_(t[0], br), compile_(t[1], br))
    return t

# surface helpers
def lam(*a):
    *xs, body = a
    for x in reversed(xs): body = ('λ', x, body)
    return body
def tri(c, d, b, x): return A(L, A(L, c, d), b, x)
# a value that, applied to v, runs f x v, without running f x first (needed by eager evaluation)
WAIT = {'tc':  lambda f, x: Sc(Sc(Kc(f), Kc(x)), I),
        'ltc': lambda f, x: Cc(A(L, A(L, A(L, f, I))), I, x)}
