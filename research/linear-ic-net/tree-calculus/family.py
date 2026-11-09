"""Nine-slot designs: three families by the function's left child, each reading one more node.
Every design: one dropping rule (K), one copying rule (W or S), the rest linear, each rule eats 3 △."""
import itertools
from rules import L, A, spine
from ltc2 import LTC2   # triage rules are LTC2[6:]
TRI = LTC2[6:]
SLOT = {  # slot -> (lhs, operand names)
 'LL': A(L, L, L, 'x'), 'LS': A(L, L, A(L, 'q'), 'x'), 'LF': A(L, L, A(L, 'q', 'r'), 'x'),
 'SL': A(L, A(L, L), 'b', 'x'), 'SS': A(L, A(L, A(L, 'f')), 'b', 'x'), 'SF': A(L, A(L, A(L, 'f', 'g')), 'b', 'x')}
RHS = {  # combinator -> rhs, in the operand names of each slot kind
 'I': {'LL': 'x'},
 'K': {'LS': 'q', 'SL': 'b'},
 'W': {'LS': A('q', 'x', 'x'), 'SL': A('b', 'x', 'x')},
 'T': {'LS': A('x', 'q'), 'SL': A('x', 'b')},
 'S': {'LF': A('q', 'x', A('r', 'x')), 'SS': A('f', 'x', A('b', 'x'))},
 'B': {'LF': A('q', A('r', 'x')), 'SS': A('f', A('b', 'x'))},
 'C': {'LF': A('q', 'x', 'r'), 'SS': A('f', 'x', 'b')},
 'B2': {'SF': A('f', A('g', A('b', 'x')))},
 'C2': {'SF': A('f', A('g', 'x'), 'b')},
}
def rules_of(assign):
    return [(c, SLOT[s], RHS[c][s]) for s, c in assign.items()] + TRI
def maker(slot):
    """tree builder for the combinator in this slot, taking its operands in order"""
    return {'LL': lambda: A(L, L, L), 'LS': lambda q: A(L, L, A(L, q)), 'LF': lambda q, r: A(L, L, A(L, q, r)),
            'SL': lambda b: A(L, A(L, L), b), 'SS': lambda f, b: A(L, A(L, A(L, f)), b),
            'SF': lambda f, g, b: A(L, A(L, A(L, f, g)), b)}[slot]

def make_compiler(assign):
    have = {c: maker(s) for s, c in assign.items()}
    slot_of = {c: s for s, c in assign.items()}
    fresh = (f'_z{i}' for i in itertools.count())
    def occurs(x, m): return m == x or (isinstance(m, tuple) and len(m) == 2 and (occurs(x, m[0]) or occurs(x, m[1])))
    def ren(m, x, y):
        if m == x: return y
        return (ren(m[0], x, y), ren(m[1], x, y)) if isinstance(m, tuple) else m
    def shape(t, c):
        """operands if t is combinator c fully built (all its tree operands present), else None"""
        if c not in slot_of: return None
        s = slot_of[c]; h, a = spine(t)
        if h != L or len(a) != 2: return None
        if s[0] == 'L':
            if a[0] != L: return None
            h2, a2 = spine(a[1])
            return a2 if h2 == L and len(a2) == {'L': 0, 'S': 1, 'F': 2}[s[1]] else None
        h1, a1 = spine(a[0])
        if h1 != L or len(a1) != 1: return None
        h2, a2 = spine(a1[0])
        return a2 + [a[1]] if h2 == L and len(a2) == {'L': 0, 'S': 1, 'F': 2}[s[1]] else None
    I = have['I']()
    def Kc(m): return have['K'](m)
    def Bc(f, g):
        if 'B' in have: return have['B'](f, g)
        raise KeyError('B')
    def br(x, m):
        if m == x: return I
        if not occurs(x, m): return Kc(m)
        p, q = m
        if q == x and not occurs(x, p): return p
        if not occurs(x, p):                                  # x goes right
            r = br(x, q)
            if 'B2' in have:
                gb = shape(r, 'B')
                if gb: return have['B2'](p, gb[0], gb[1])
            return Bc(p, r)
        if not occurs(x, q):                                  # x goes left
            f = br(x, p)
            if f == I and 'T' in have: return have['T'](q)
            if 'C2' in have:
                fg = shape(f, 'B')
                if fg: return have['C2'](fg[0], fg[1], q)
            if 'C' in have: return have['C'](f, q)
            if 'C2' in have: return have['C2'](f, I, q)
            return Bc(have['T'](q), f)                        # f x q = T q (f x)
        if 'S' in have: return have['S'](br(x, p), br(x, q))  # x both ways
        y = next(fresh)
        return have['W'](br(x, br(y, (p, ren(q, x, y)))))
    def comp(t):
        if isinstance(t, tuple) and len(t) == 3: return br(t[1], comp(t[2]))
        if isinstance(t, tuple): return (comp(t[0]), comp(t[1]))
        return t
    return comp

DESIGNS = {
 'V1 I K B | W C C2': {'LL': 'I', 'LS': 'K', 'LF': 'B', 'SL': 'W', 'SS': 'C', 'SF': 'C2'},
 'V2 I K B | W C B2': {'LL': 'I', 'LS': 'K', 'LF': 'B', 'SL': 'W', 'SS': 'C', 'SF': 'B2'},
 'V3 I K S | T B C2': {'LL': 'I', 'LS': 'K', 'LF': 'S', 'SL': 'T', 'SS': 'B', 'SF': 'C2'},
 'V5 I K C | W B B2': {'LL': 'I', 'LS': 'K', 'LF': 'C', 'SL': 'W', 'SS': 'B', 'SF': 'B2'},
 'V4 I K S | T B B2': {'LL': 'I', 'LS': 'K', 'LF': 'S', 'SL': 'T', 'SS': 'B', 'SF': 'B2'},
 'V7 I K C | W B C2': {'LL': 'I', 'LS': 'K', 'LF': 'C', 'SL': 'W', 'SS': 'B', 'SF': 'C2'},
}
I_ = A(L, L, L)
def wait_of(name):
    """w = wait f x, a value with w v = f x v that does not run f x before v arrives"""
    Css = lambda f, b: A(L, A(L, A(L, f)), b); Css_ = lambda f: A(L, A(L, A(L, f)))      # C at SS, and C f
    C2 = lambda f, g, b: A(L, A(L, A(L, f, g)), b); C2_ = lambda f, g: A(L, A(L, A(L, f, g)))
    Clf = lambda q, r: A(L, L, A(L, q, r)); Bss = lambda f, g: A(L, A(L, A(L, f)), g)
    if name.startswith(('V1', 'V2')): return lambda f, x: Css(Css_(f), x)                 # C (C f) x
    if name.startswith('V3'): return lambda f, x: C2(C2_(f, I_), I_, x)                   # C2 (C2 f I) I x
    if name.startswith(('V5', 'V7')): return lambda f, x: Clf(Bss(A(L, L), A(L, f)), x)          # C (B (△△) (△ f)) x
    if name.startswith('V4'):                   # B (T x) [b](B (T b) f): no C here, so build the flip
        comp = make_compiler(DESIGNS[name])
        Tsl = lambda b: A(L, A(L, L), b)
        return lambda f, x: Bss(Tsl(x), comp(('λ', '_b', Bss(Tsl('_b'), f))))
    raise KeyError(name)
