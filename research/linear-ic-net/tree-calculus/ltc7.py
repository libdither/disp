"""Seven-rule linear tree calculus. The leaf family reads b (I, W, and one mover), the stem family is one
mover, the fork family is triage. Triage on a leaf drops the two arms it did not pick (as tree calculus does):
that is the only rule that drops, so K is derived: K q = C (△ (△ q △)) △, and K q x → q in two steps."""
import itertools
from rules import L, A, spine
TRI7 = [('T0', A(L, A(L, 'c', 'd'), 'b', L), 'c'),
        ('T1', A(L, A(L, 'c', 'd'), 'b', A(L, 'u')), A('d', 'c', 'b', 'u')),
        ('T2', A(L, A(L, 'c', 'd'), 'b', A(L, 'u', 'v')), A('b', 'c', 'd', 'u', 'v'))]
VARIANTS = {
 # 7a: B is the stem rule (tree calculus's S shape, made linear), C sits in the leaf family
 '7a': [('I', A(L, L, L, 'x'), 'x'),
        ('W', A(L, L, A(L, 'q'), 'x'), A('q', 'x', 'x')),
        ('C', A(L, L, A(L, 'q', 'r'), 'x'), A('q', 'x', 'r')),
        ('B', A(L, A(L, 'p'), 'b', 'x'), A('p', A('b', 'x')))] + TRI7,
 # 7b: C is the stem rule, B sits in the leaf family
 '7b': [('I', A(L, L, L, 'x'), 'x'),
        ('W', A(L, L, A(L, 'q'), 'x'), A('q', 'x', 'x')),
        ('B', A(L, L, A(L, 'q', 'r'), 'x'), A('q', A('r', 'x'))),
        ('C', A(L, A(L, 'p'), 'b', 'x'), A('p', 'x', 'b'))] + TRI7,
}
def builders(v):
    I = A(L, L, L)
    W = lambda q: A(L, L, A(L, q))
    if v == '7a':
        B = lambda f, g: A(L, A(L, f), g)
        C = lambda f, g: A(L, L, A(L, f, g))
    else:
        B = lambda f, g: A(L, L, A(L, f, g))
        C = lambda f, g: A(L, A(L, f), g)
    K = lambda q: C(A(L, A(L, q, L)), L)            # K q x -> △ (△ q △) x △ -> q
    return I, K, W, B, C
def make_compiler(v):
    I, K, W, B, C = builders(v)
    fresh = (f'_w{i}' for i in itertools.count())
    def occ(x, m): return m == x or (isinstance(m, tuple) and len(m) == 2 and (occ(x, m[0]) or occ(x, m[1])))
    def ren(m, x, y):
        if m == x: return y
        return (ren(m[0], x, y), ren(m[1], x, y)) if isinstance(m, tuple) else m
    def br(x, m):
        if m == x: return I
        if not occ(x, m): return K(m)
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
    return comp
def wait_of(v):
    """w = wait f x: w v -> f x v, without running f x first.  C (C f) x, with C f a partial tree."""
    if v == '7a':   # C at the leaf family cannot be partially applied as a tree; use B and C: C (B (△△) (△ f)) x
        return lambda f, x: A(L, L, A(L, A(L, A(L, A(L, L)), A(L, f)), x))
    return lambda f, x: A(L, A(L, A(L, A(L, f))), x)

# 7c: tree calculus's S and leaf-triage kept as they are; tree calculus's K replaced by I, T, B (read off b);
# stem and fork triage hand back the arms they beat.
VARIANTS['7c'] = [('I', A(L, L, L, 'x'), 'x'),
                  ('T', A(L, L, A(L, 'q'), 'x'), A('x', 'q')),
                  ('B', A(L, L, A(L, 'q', 'r'), 'x'), A('q', A('r', 'x'))),
                  ('S', A(L, A(L, 'p'), 'b', 'x'), A('p', 'x', A('b', 'x')))] + TRI7
def compiler_7c():
    I = A(L, L, L)
    Tq = lambda q: A(L, L, A(L, q))
    B = lambda f, g: A(L, L, A(L, f, g))
    S = lambda f, g: A(L, A(L, f), g)
    C = lambda f, n: B(Tq(n), f)                    # C f n x -> T n (f x) -> f x n
    K = lambda q: B(Tq(L), A(L, A(L, q, L)))        # K q x -> T △ (△ (△ q △) x) -> △ (△ q △) x △ -> q
    def occ(x, m): return m == x or (isinstance(m, tuple) and len(m) == 2 and (occ(x, m[0]) or occ(x, m[1])))
    def br(x, m):
        if m == x: return I
        if not occ(x, m): return K(m)
        p, q = m
        if q == x and not occ(x, p): return p
        if not occ(x, p): return B(p, br(x, q))
        if not occ(x, q):
            f = br(x, p)
            return Tq(q) if f == I else C(f, q)
        return S(br(x, p), br(x, q))
    def comp(t):
        if isinstance(t, tuple) and len(t) == 3: return br(t[1], comp(t[2]))
        if isinstance(t, tuple): return (comp(t[0]), comp(t[1]))
        return t
    def wait(f, x):                                 # B (T x) (flip f): flip f = λb y. f y b, built without running f x
        flip = comp(('λ', '_b', B(Tq('_b'), f)))
        return B(Tq(x), flip)
    return comp, K, wait
