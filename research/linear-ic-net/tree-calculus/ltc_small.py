"""Two drop-free designs under nine rules (triage always hands back what it beat).
LTC8: I K C | W B + linear triage. Only K drops, only W copies. One redex shape, △ (△ (△ f g)) b x,
      has no rule (it is stuck), because eight rules cannot cover every shape.
LTC7r: I W C | B + linear triage. No rule drops at all; only W copies. Covers every shape. Without K it is
       a relevant calculus (each variable used at least once), complete for λI-terms."""
import itertools
from rules import L, A, spine
TRI = [('T0', A(L, A(L, 'c', 'd'), 'b', L), A('c', 'd', 'b')),
       ('T1', A(L, A(L, 'c', 'd'), 'b', A(L, 'u')), A('d', 'c', 'b', 'u')),
       ('T2', A(L, A(L, 'c', 'd'), 'b', A(L, 'u', 'v')), A('b', 'c', 'd', 'u', 'v'))]
LTC8 = [('I', A(L, L, L, 'x'), 'x'),
        ('K', A(L, L, A(L, 'q'), 'x'), 'q'),
        ('C', A(L, L, A(L, 'q', 'r'), 'x'), A('q', 'x', 'r')),
        ('W', A(L, A(L, L), 'b', 'x'), A('b', 'x', 'x')),
        ('B', A(L, A(L, A(L, 'f')), 'b', 'x'), A('f', A('b', 'x')))] + TRI
LTC7R = [('I', A(L, L, L, 'x'), 'x'),
         ('W', A(L, L, A(L, 'q'), 'x'), A('q', 'x', 'x')),
         ('C', A(L, L, A(L, 'q', 'r'), 'x'), A('q', 'x', 'r')),
         ('B', A(L, A(L, 'p'), 'b', 'x'), A('p', A('b', 'x')))] + TRI
def compiler(which):
    I = A(L, L, L)
    if which == '8':
        K = lambda q: A(L, L, A(L, q)); W = lambda b: A(L, A(L, L), b)
        B = lambda f, g: A(L, A(L, A(L, f)), g); C = lambda q, r: A(L, L, A(L, q, r))
    else:
        K = None; W = lambda q: A(L, L, A(L, q))
        B = lambda f, g: A(L, A(L, f), g); C = lambda q, r: A(L, L, A(L, q, r))
    fresh = (f'_r{i}' for i in itertools.count())
    def occ(x, m): return m == x or (isinstance(m, tuple) and len(m) == 2 and (occ(x, m[0]) or occ(x, m[1])))
    def ren(m, x, y):
        if m == x: return y
        return (ren(m[0], x, y), ren(m[1], x, y)) if isinstance(m, tuple) else m
    def br(x, m):
        if m == x: return I
        if not occ(x, m):
            if K is None: raise ValueError('a variable is never used: no K in LTC7r')
            return K(m)
        p, q = m
        if q == x and not occ(x, p): return p
        if not occ(x, p): return B(p, br(x, q))
        if not occ(x, q): return C(br(x, p), q)
        y = next(fresh)
        return W(br(x, br(y, (p, ren(q, x, y)))))
    def comp(t):
        if isinstance(t, tuple) and len(t) == 3: return br(t[1], comp(t[2]))
        if isinstance(t, tuple): return (comp(t[0]), comp(t[1]))
        return t
    return comp, K
