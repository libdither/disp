"""Apply that keeps what it would have dropped (the residual), and the rules that run it back.

Trees are tuples ('L',), ('S', x), ('F', x, y). `app(f, z)` gives the result and a residual, a plain
tree whose layout says which case fired (the j's are the residuals of the applies the step starts):

    △ z         = △z         residual △
    △x z        = △xz        residual △
    △△b c       = b          residual △△c
    △(△s)b c    = s c (b c)  residual △(△j₁)(△j₂j₃)      s c, b c, then the outer apply
    △(△wx)b △   = w          residual △(△△x)b
    △(△wx)b △u  = x u        residual △(△(△j)w)b
    △(△wx)b △uv = b u v      residual △(△(△j₁j₂)w)x      b u, then (b u) v

`unapp(r, j)` gives back (f, z). FORWARD and BACKWARD are the same as interaction-net rules (in
rule_audit.py's format), sized for the strand lattice. Running back is done by the same walkers
turned round: A⁻ un-applies (A's ports in reverse order: residual, result, argument out, function
out), Sel⁻ dispatches back and Dn⁻ merges two copies; A⁻r and Dn⁻L/S/F are the second reads that
undoing a creation needs.

With shortcuts (`short`), the S rule first looks whether its function is B f g = △(△(△△f))g or
C f g = △(△f)(△△g), and if so sends the argument to the one side that uses it instead of copying it
to both and dropping one: B f g c = f (g c) leaves residual △(△j₁j₂), C f g c = f c g leaves
△(△(△j₁j₂)), the stem on top being what no other residual has. rust-ca-lattice's net makes these
shapes into agents of their own when it loads code; here they are read off the tree as the S rule
runs, so nothing has to be spelled out again when code is read as data, and running back gives the
very agents loaded.

LazyNet runs it all lazily: a copy stays a link until read, and work never done is undone by joining
the agent with its reverse.

    python residual.py [--quick]     the checks
    python residual.py --rust        the lattice's table, as rules.rs `RESID_RULES` (S looking for B and C)
"""
import itertools, re, sys, threading
from collections import Counter

sys.path.insert(0, __file__.rsplit('/', 1)[0])
import rule_audit as audit

ROOT = __file__.rsplit('/research/', 1)[0]
L = ('L',)
S = lambda x: ('S', x)
F = lambda x, y: ('F', x, y)


class OutOfFuel(Exception): pass
class Stuck(Exception): pass


class Fuel:
    def __init__(self, n): self.n = n
    def take(self):
        self.n -= 1
        if self.n < 0: raise OutOfFuel


# ---- the oracle: oracle.rs's lazy normalizer, and apply with a residual ----

def whnf(t, fuel):
    while t[0] == 'Ap':
        fuel.take()
        f = whnf(t[1], fuel)
        t = S(t[2]) if f[0] == 'L' else F(f[1], t[2]) if f[0] == 'S' else triage(f[1], f[2], t[2], fuel)
    return t


def triage(a, b, c, fuel):
    a = whnf(a, fuel)
    if a[0] == 'L': return b
    if a[0] == 'S': return ('Ap', ('Ap', a[1], c), ('Ap', b, c))
    c = whnf(c, fuel)
    return a[1] if c[0] == 'L' else ('Ap', a[2], c[1]) if c[0] == 'S' else ('Ap', ('Ap', b, c[1]), c[2])


def nf(t, fuel):
    t = whnf(t, fuel)
    return (t[0],) + tuple(nf(k, fuel) for k in t[1:])


def app(f, z, fuel, short=False):
    fuel.take()
    if f[0] == 'L': return S(z), L
    if f[0] == 'S': return F(f[1], z), L
    a, b = f[1], f[2]
    if a[0] == 'L': return b, F(L, z)
    if a[0] == 'S':
        s = a[1]
        if short and s[0] == 'F' and s[1] == L:
            r1, j1 = app(b, z, fuel, short)
            r, j2 = app(s[2], r1, fuel, short)
            return r, S(F(j1, j2))
        if short and b[0] == 'F' and b[1] == L:
            r1, j1 = app(s, z, fuel, short)
            r, j2 = app(r1, b[2], fuel, short)
            return r, S(S(F(j1, j2)))
        r1, j1 = app(s, z, fuel, short)
        r2, j2 = app(b, z, fuel, short)
        r, j3 = app(r1, r2, fuel, short)
        return r, F(S(j1), F(j2, j3))
    w, x = a[1], a[2]
    if z[0] == 'L': return w, F(F(L, x), b)
    if z[0] == 'S':
        r, j = app(x, z[1], fuel, short)
        return r, F(F(S(j), w), b)
    p, j1 = app(b, z[1], fuel, short)
    r, j2 = app(p, z[2], fuel, short)
    return r, F(F(F(j1, j2), w), x)


def unapp(r, j):
    if j == L: return (L, r[1]) if r[0] == 'S' else (S(r[1]), r[2])
    if j[0] == 'S' and j[1][0] == 'F':
        f, r1 = unapp(r, j[1][2])
        g, c = unapp(r1, j[1][1])
        return F(S(F(L, f)), g), c
    if j[0] == 'S':
        r1, g = unapp(r, j[1][1][2])
        s, c = unapp(r1, j[1][1][1])
        return F(S(s), F(L, g)), c
    l, rr = j[1], j[2]
    if l[0] == 'L': return F(L, r), rr
    if l[0] == 'S':
        p, q = unapp(r, rr[2])
        s, c1 = unapp(p, l[1])
        b, c2 = unapp(q, rr[1])
        assert c1 == c2
        return F(S(s), b), c1
    h, d = l[1], l[2]
    if h[0] == 'L': return F(F(r, d), rr), L
    if h[0] == 'S':
        x, u = unapp(r, h[1])
        return F(F(d, x), rr), S(u)
    p, v = unapp(r, h[2])
    b, u = unapp(p, h[1])
    return F(F(d, rr), b), F(u, v)


# ---- the rules, sized for the strand lattice: at most 4 ports an agent and 6 fresh agents a rule.
# Ports: A [fn, arg, out, res]; T1 [a, ⟨b,c⟩, out, res]; Sel [c, Arms3(w,x,b), out, res]; Unp [pair,
# fst, snd]; Dn [value, copy, copy]; values [p, children…]. A rule's right side: named fresh agents,
# then wires between agent ports (n.k), the consumer's other ports (c1…) and the producer's (p1…).
#
# A residual is built of five compact agents, each standing for a plain tree as the lattice's
# movement agents stand for code (and read back spelled out):
#   Rk(c) = △△c   Rs(j₁,j₂,j₃) = △(△j₁)(△j₂j₃)   Rt(h,d,r) = △(△hd)r   Rb(j₁,j₂) = △(△j₁j₂)   Rc(j₁,j₂) = △(△(△j₁j₂))
# Arms3 carries a triage's three arms. The stage walkers (SL, SS, SF, TSs, TSb, T1s, T1sf, T1sc0,
# T1sc, T1scf and their reverses) each read one more thing, so that no rule makes more than six agents.

SPELL = {'Rk': lambda c: F(L, c), 'Rs': lambda a, b, c: F(S(a), F(b, c)), 'Rt': lambda h, d, r: F(F(h, d), r),
         'Rb': lambda a, b: S(F(a, b)), 'Rc': lambda a, b: S(S(F(a, b)))}
AR = {'L': 1, 'S': 2, 'F': 3, 'Pair': 3, 'Arms3': 4, 'Rk': 2, 'Rs': 4, 'Rt': 4, 'Rb': 3, 'Rc': 3,
      'A': 4, 'T1': 4, 'Sel': 4, 'SL': 3, 'SS': 4, 'SF': 4, 'TSs': 4, 'TSb': 4,
      'T1s': 4, 'T1sf': 4, 'T1sc0': 4, 'T1sc': 4, 'T1scf': 4, 'Unp': 3, 'Dn': 3, 'Eps': 1, 'Out': 1,
      'A⁻': 4, 'A⁻r': 3, 'Sel⁻': 4, 'SL⁻': 3, 'SS⁻': 4, 'SF⁻': 4, 'Dn⁻': 3, 'Dn⁻L': 2, 'Dn⁻S': 3, 'Dn⁻F': 4}
VALUES = {'L', 'S', 'F', 'Pair', 'Arms3', *SPELL}
audit.ARITY.update(AR)


def R(c, p, fresh, wires):
    names, kinds = {}, []
    for i, item in enumerate(fresh.split()):
        n, k = item.split(':')
        names[n] = i
        kinds.append(k)
    def end(s):
        if re.fullmatch(r'[cp]\d', s): return ('C' if s[0] == 'c' else 'Px', int(s[1]), None)
        n, q = s.split('.')
        return ('X', names[n], int(q))
    return dict(name=f'{c}·{p}', c=c, p=p, fresh=kinds, wires=[tuple(map(end, w.split('='))) for w in wires.split()])


COPY = [
    R('Dn', 'L', 'x:L y:L', 'x.0=c1 y.0=c2'),
    R('Dn', 'S', 'd:Dn s:S t:S', 'd.0=p1 d.1=s.1 d.2=t.1 s.0=c1 t.0=c2'),
    R('Dn', 'F', 'd:Dn e:Dn f:F g:F', 'd.0=p1 e.0=p2 d.1=f.1 e.1=f.2 d.2=g.1 e.2=g.2 f.0=c1 g.0=c2'),
]

# Dn⁻ [copy, copy, value] reads one copy's head, then Dn⁻L/S/F [other copy, children…, value] the other's.
MERGE = [
    R('Dn⁻', 'L', 'k:Dn⁻L', 'k.0=c1 k.1=c2'),
    R('Dn⁻L', 'L', 'l:L', 'l.0=c1'),
    R('Dn⁻', 'S', 'k:Dn⁻S', 'k.0=c1 k.1=p1 k.2=c2'),
    R('Dn⁻S', 'S', 'm:Dn⁻ s:S', 'm.0=c1 m.1=p1 m.2=s.1 s.0=c2'),
    R('Dn⁻', 'F', 'k:Dn⁻F', 'k.0=c1 k.1=p1 k.2=p2 k.3=c2'),
    R('Dn⁻F', 'F', 'm:Dn⁻ n:Dn⁻ f:F', 'm.0=c1 m.1=p1 n.0=c2 n.1=p2 m.2=f.1 n.2=f.2 f.0=c3'),
]

ERASE = [R('Eps', k, ' '.join(f'e{i}:Eps' for i in range(1, AR[k])), ' '.join(f'e{i}.0=p{i}' for i in range(1, AR[k])))
         for k in ['L', 'S', 'F', 'Pair', 'Arms3', *SPELL]]

UNPAIR = R('Unp', 'Pair', '', 'c1=p1 c2=p2')


def s_body(s, b, c):
    """s c (b c), c copied, residual Rs(j₁, j₂, j₃), with s, b and c the ends they come from."""
    return ('d:Dn ab:A as:A ao:A r:Rs', f'as.0={s} ab.0={b} d.0={c} d.1=ab.1 d.2=as.1 ab.2=ao.1 as.2=ao.0 ao.2=c2 '
            'r.1=as.3 r.2=ab.3 r.3=ao.3 r.0=c3')


FORWARD_BASE = [
    R('A', 'L', 's:S j:L', 's.1=c1 s.0=c2 j.0=c3'),
    R('A', 'S', 'f:F j:L', 'f.1=p1 f.2=c1 f.0=c2 j.0=c3'),
    R('A', 'F', 't:T1 q:Pair', 't.0=p1 q.1=p2 q.2=c1 t.1=q.0 t.2=c2 t.3=c3'),
    R('T1', 'L', 'u:Unp k:Rk', 'u.0=c1 u.1=c2 u.2=k.1 k.0=c3'),
    R('T1', 'F', 'u:Unp e:Sel m:Arms3', 'u.0=c1 u.1=m.3 u.2=e.0 e.1=m.0 m.1=p1 m.2=p2 e.2=c2 e.3=c3'),
    # SL [arms, out, res], SS [arms, u, out, res], SF [arms, ⟨u,v⟩, out, res] read the arms Arms3(w,x,b).
    R('Sel', 'L', 'k:SL', 'k.0=c1 k.1=c2 k.2=c3'),
    R('SL', 'Arms3', 't:Rt l:L', 'p1=c1 t.1=l.0 t.2=p2 t.3=p3 t.0=c2'),
    R('Sel', 'S', 'k:SS', 'k.0=c1 k.1=p1 k.2=c2 k.3=c3'),
    R('SS', 'Arms3', 'a:A t:Rt s:S', 'a.0=p2 a.1=c1 a.2=c2 a.3=s.1 t.1=s.0 t.2=p1 t.3=p3 t.0=c3'),
    R('Sel', 'F', 'k:SF q:Pair', 'q.1=p1 q.2=p2 k.0=c1 k.1=q.0 k.2=c2 k.3=c3'),
    R('SF', 'Arms3', 'u:Unp a:A b:A t:Rt f:F',
      'u.0=c1 a.0=p3 a.1=u.1 a.2=b.0 b.1=u.2 b.2=c2 a.3=f.1 b.3=f.2 t.1=f.0 t.2=p1 t.3=p2 t.0=c3'),
    UNPAIR,
] + COPY

# TSs [⟨b,c⟩, s, out, res] and TSb [⟨s,c⟩, b, out, res] do the S rule once its pair is read.
PLAIN_S = [
    R('T1', 'S', 'k:TSs', 'k.0=c1 k.1=p1 k.2=c2 k.3=c3'),
    R('TSs', 'Pair', *s_body('c1', 'p1', 'p2')),
]

# T1s [s, ⟨b,c⟩, …] looks at s's head, T1sf [its left child, ⟨its right child, ⟨b,c⟩⟩, …] at that:
# a leaf there is B. Otherwise T1sc0 [⟨b,c⟩, s, …] opens the pair, T1sc [b, ⟨s,c⟩, …] and T1scf [b's
# left child, ⟨b's right child, ⟨s,c⟩⟩, …] look at b the same way: a leaf there is C. What they read
# and do not use they build again.
SHORT_S = [
    R('T1', 'S', 'k:T1s', 'k.0=p1 k.1=c1 k.2=c2 k.3=c3'),
    R('T1s', 'L', 'l:L k:T1sc0', 'k.0=c1 k.1=l.0 k.2=c2 k.3=c3'),
    R('T1s', 'S', 'l:S k:T1sc0', 'l.1=p1 k.0=c1 k.1=l.0 k.2=c2 k.3=c3'),
    R('T1s', 'F', 'k:T1sf q:Pair', 'k.0=p1 q.1=p2 q.2=c1 k.1=q.0 k.2=c2 k.3=c3'),
    # B f g c = f (g c), residual Rb(j₁, j₂).
    R('T1sf', 'L', 'u:Unp v:Unp a:A b:A k:Rb', 'u.0=c1 u.1=b.0 u.2=v.0 v.1=a.0 v.2=a.1 a.2=b.1 b.2=c2 a.3=k.1 b.3=k.2 k.0=c3'),
    R('T1sf', 'S', 'u:Unp l:S f:F k:T1sc0', 'u.0=c1 u.1=f.2 u.2=k.0 l.1=p1 f.1=l.0 k.1=f.0 k.2=c2 k.3=c3'),
    R('T1sf', 'F', 'u:Unp l:F f:F k:T1sc0', 'u.0=c1 u.1=f.2 u.2=k.0 l.1=p1 l.2=p2 f.1=l.0 k.1=f.0 k.2=c2 k.3=c3'),
    R('T1sc0', 'Pair', 'k:T1sc q:Pair', 'k.0=p1 q.1=c1 q.2=p2 k.1=q.0 k.2=c2 k.3=c3'),
    R('T1sc', 'L', 'l:L k:TSb', 'k.0=c1 k.1=l.0 k.2=c2 k.3=c3'),
    R('T1sc', 'S', 'l:S k:TSb', 'l.1=p1 k.0=c1 k.1=l.0 k.2=c2 k.3=c3'),
    R('T1sc', 'F', 'k:T1scf q:Pair', 'k.0=p1 q.1=p2 q.2=c1 k.1=q.0 k.2=c2 k.3=c3'),
    # C f g c = f c g, residual Rc(j₁, j₂).
    R('T1scf', 'L', 'u:Unp v:Unp a:A b:A k:Rc', 'u.0=c1 u.1=b.1 u.2=v.0 v.1=a.0 v.2=a.1 a.2=b.0 b.2=c2 a.3=k.1 b.3=k.2 k.0=c3'),
    R('T1scf', 'S', 'u:Unp l:S f:F k:TSb', 'u.0=c1 u.1=f.2 u.2=k.0 l.1=p1 f.1=l.0 k.1=f.0 k.2=c2 k.3=c3'),
    R('T1scf', 'F', 'u:Unp l:F f:F k:TSb', 'u.0=c1 u.1=f.2 u.2=k.0 l.1=p1 l.2=p2 f.1=l.0 k.1=f.0 k.2=c2 k.3=c3'),
    R('TSb', 'Pair', *s_body('p1', 'c1', 'p2')),
]

# A⁻ [residual, result, argument out, function out]: the residual read as the function, the result as
# the argument, and what comes out in the order apply gives its result and residual.
BACKWARD = [
    R('A⁻', 'L', 'k:A⁻r', 'k.0=c1 k.1=c2 k.2=c3'),  # A⁻r [result, argument out, function out]
    R('A⁻r', 'S', 'l:L', 'p1=c1 l.0=c2'),
    R('A⁻r', 'F', 's:S', 's.1=p1 p2=c1 s.0=c2'),
    R('A⁻', 'Rk', 'f:F l:L', 'p1=c2 f.1=l.0 f.2=c1 f.0=c3'),
    R('A⁻', 'Rs', 'o:A⁻ a:A⁻ b:A⁻ m:Dn⁻ f:F s:S',
      'o.0=p3 o.1=c1 o.2=b.1 o.3=a.1 a.0=p1 b.0=p2 a.2=m.0 b.2=m.1 m.2=c2 a.3=s.1 b.3=f.2 f.1=s.0 f.0=c3'),
    # Sel⁻ [the argument's head, Arms3(d, r, result), …], residual Rt(head, d, r).
    R('A⁻', 'Rt', 'e:Sel⁻ m:Arms3', 'e.0=p1 e.1=m.0 m.1=p2 m.2=p3 m.3=c1 e.2=c2 e.3=c3'),
    R('Sel⁻', 'L', 'k:SL⁻', 'k.0=c1 k.1=c2 k.2=c3'),
    R('SL⁻', 'Arms3', 'l:L t:F n:F', 'l.0=c1 n.1=p3 n.2=p1 t.1=n.0 t.2=p2 t.0=c2'),
    R('Sel⁻', 'S', 'k:SS⁻', 'k.0=c1 k.1=p1 k.2=c2 k.3=c3'),
    R('SS⁻', 'Arms3', 'a:A⁻ s:S t:F n:F', 'a.0=c1 a.1=p3 a.2=s.1 a.3=n.2 s.0=c2 n.1=p1 t.1=n.0 t.2=p2 t.0=c3'),
    R('Sel⁻', 'F', 'k:SF⁻ q:Pair', 'q.1=p1 q.2=p2 k.0=c1 k.1=q.0 k.2=c2 k.3=c3'),
    R('SF⁻', 'Arms3', 'u:Unp a:A⁻ b:A⁻ g:F t:F n:F',
      'u.0=c1 b.0=u.2 b.1=p3 b.2=g.2 b.3=a.1 a.0=u.1 a.2=g.1 a.3=t.2 g.0=c2 n.1=p1 n.2=p2 t.1=n.0 t.0=c3'),
    R('A⁻', 'Rb', 'b:A⁻ a:A⁻ r:F s:S k:F l:L', 'b.0=p2 b.1=c1 b.2=a.1 b.3=k.2 a.0=p1 a.2=c2 a.3=r.2 r.1=s.0 s.1=k.0 k.1=l.0 r.0=c3'),
    R('A⁻', 'Rc', 'b:A⁻ a:A⁻ r:F s:S k:F l:L', 'b.0=p2 b.1=c1 b.2=k.2 b.3=a.1 a.0=p1 a.2=c2 a.3=s.1 r.1=s.0 r.2=k.0 k.1=l.0 r.0=c3'),
    UNPAIR,
] + MERGE



def fold(j):
    """A plain residual as the compact agents the net builds it of (SPELL undone)."""
    if j == L: return L
    if j[0] == 'S':
        x = j[1]
        return ('Rb', fold(x[1]), fold(x[2])) if x[0] == 'F' else ('Rc', fold(x[1][1]), fold(x[1][2]))
    l, r = j[1], j[2]
    if l[0] == 'L': return ('Rk', r)
    if l[0] == 'S': return ('Rs', fold(l[1]), fold(r[1]), fold(r[2]))
    h = l[1]
    h = h if h[0] == 'L' else ('S', fold(h[1])) if h[0] == 'S' else ('F', fold(h[1]), fold(h[2]))
    return ('Rt', h, l[2], r)


FORWARD = FORWARD_BASE + SHORT_S
FORWARD_PLAIN = FORWARD_BASE + PLAIN_S

TWIN = {'A⁻': 'A', 'Sel⁻': 'Sel', 'SL⁻': 'SL', 'SS⁻': 'SS', 'SF⁻': 'SF', 'Dn⁻': 'Dn'}
assert all(max(AR[k] for k in [r['c'], r['p'], *r['fresh']]) <= 4 and len(r['fresh']) <= 6 for r in FORWARD + PLAIN_S + BACKWARD + ERASE)


# ---- a small interaction-net interpreter ----

class Net:
    def __init__(self, rules):
        self.rules = {(r['c'], r['p']): r for r in rules}
        self.kind, self.link, self.todo, self.fired, self.n = {}, {}, [], Counter(), 0

    def new(self, k):
        self.n += 1
        self.kind[self.n] = k
        return self.n

    def join(self, a, b):
        self.link[a], self.link[b] = b, a
        if a[1] == 0 and b[1] == 0: self.todo.append((a[0], b[0]))

    def tree(self, t):
        i = self.new(t[0])
        for q, kid in enumerate(t[1:], 1): self.join((i, q), self.tree(kid))
        return (i, 0)

    def agent(self, k, ends):
        i = self.new(k)
        for q, e in enumerate(ends):
            if e is not None: self.join((i, q), e)
        return i

    def sink(self):
        return (self.new('Out'), 0)

    def read(self, e, links=False):
        """The tree at e; with links, a ('link',) wherever no value is yet."""
        i, q = self.link[e] if self.kind.get(e[0]) == 'Out' else e
        k = self.kind.get(i)
        if q != 0 or k not in VALUES:
            if links: return ('link',)
            if (k, q) in LAZY_OUT: raise Need(i)
            raise Stuck(f'not a value: {k}.{q}')
        kids = [self.read(self.link[(i, p)], links) for p in range(1, AR[k])]
        return SPELL[k](*kids) if k in SPELL else (k, *kids)

    def count(self, e):
        """Agents in the value at e, a link counting one."""
        i, q = self.link[e] if self.kind.get(e[0]) == 'Out' else e
        k = self.kind.get(i)
        if q != 0 or k not in VALUES: return 1
        return 1 + sum(self.count(self.link[(i, p)]) for p in range(1, AR[k]))

    def run(self, fuel=10**7):
        while self.todo:
            a, b = self.todo.pop()
            if self.link.get((a, 0)) != (b, 0) or 'Out' in (self.kind[a], self.kind[b]): continue
            r = self.rules.get((self.kind[a], self.kind[b]))
            if r is None:
                a, b = b, a
                r = self.rules.get((self.kind[a], self.kind[b]))
            if r is None: raise Stuck(f'no rule for {self.kind[a]}·{self.kind[b]}')
            fuel -= 1
            if fuel < 0: raise OutOfFuel
            self.fire(a, b, r)

    def fire(self, c, p, r):
        ids = [self.new(k) for k in r['fresh']]
        def end(e):
            if e[0] == 'X': return (ids[e[1]], e[2])
            out = self.link[(c if e[0] == 'C' else p, e[1])]
            assert out[0] not in (c, p), 'a rule wired to its own pair'
            return out
        pairs = [(end(a), end(b)) for a, b in r['wires']]
        for i in (c, p):
            for q in range(AR[self.kind[i]]): self.link.pop((i, q), None)
            del self.kind[i]
        for a, b in pairs: self.join(a, b)
        self.fired[r['name']] += 1

    def live(self):
        return Counter(k for k in self.kind.values() if k != 'Out')


class Need(Exception):
    def __init__(self, i): self.i = i


LAZY = {'A', 'A⁻', 'Dn'}
MOVES = {'un-apply·apply', 'merge·copy', 'Eps·copy (un-copy)', 'dead work'}
LAZY_OUT = {('A', 2), ('A', 3), ('A⁻', 2), ('A⁻', 3), ('Dn', 1), ('Dn', 2)}
READERS = {'T1', 'Sel', 'SL', 'SS', 'SF', 'TSs', 'TSb', 'T1s', 'T1sf', 'T1sc0', 'T1sc', 'T1scf', 'Unp',
           'A⁻r', 'Sel⁻', 'SL⁻', 'SS⁻', 'SF⁻', 'Dn⁻L', 'Dn⁻S', 'Dn⁻F'}


class LazyNet(Net):
    """Applies, un-applies and copies run only when something reads what they make, so a dropped
    argument stays an apply that never ran and a dropped copy stays a link to its duplicator. Three
    moves join two agents that are not an active pair, each the undoing of work never done:
    - an un-apply whose residual and result wires both lead to one apply that never ran: both go,
      the function and argument come back out;
    - a merge whose two copies lead to one duplicator: both go, the original comes back out;
    - an eraser on a copy: the duplicator goes, the other copy reads the original (un-copy); an
      eraser on both outputs of an apply that never ran erases its function and argument instead."""

    def __init__(self, rules):
        super().__init__(rules)
        self.wanted, self.recheck = set(), set()

    def demands(self, x, y):
        """Whether port x reads what comes out of port y. A merge reads its copies from applies but
        waits on a duplicator, so that the two can cancel."""
        k = self.kind.get(x[0])
        if k == 'Dn⁻': return x[1] in (0, 1) and self.kind.get(y[0]) != 'Dn'
        if x[1] == 1 and self.facing_unrun(x[0]): return x[0] in self.wanted
        return x[1] == 0 and (k in READERS or (k in LAZY and x[0] in self.wanted))

    def facing_unrun(self, i):
        """The apply that never ran, if i is an un-apply reading its residual: i must learn what its
        result is before running it, since if the result is that apply's own output both just go."""
        e = self.link.get((i, 0))
        if self.kind.get(i) == 'A⁻' and e and e[1] == 3 and self.kind.get(e[0]) == 'A': return e[0]

    def join(self, a, b):
        super().join(a, b)
        for x, y in ((a, b), (b, a)):
            if (self.kind.get(y[0]), y[1]) in LAZY_OUT and self.demands(x, y): self.want(y[0])
        self.recheck.update((a[0], b[0]))

    def want(self, i):
        if i in self.wanted or i not in self.kind: return
        self.wanted.add(i)
        e = self.link.get((i, 0))
        if e is None: return
        j, q = e
        if q == 0 and self.kind[j] in VALUES: self.todo.append((i, j))
        elif self.facing_unrun(i): self.settle(i)
        elif (self.kind[j], q) in LAZY_OUT: self.want(j)

    def settle(self, i):
        """A wanted un-apply facing an apply that never ran: ask for its result; run the apply only
        once the result turns out to be something else."""
        j, r = self.facing_unrun(i), self.link.get((i, 1))
        if r is None or r == (j, 2): return
        if (self.kind.get(r[0]), r[1]) in LAZY_OUT: self.want(r[0])
        elif r[1] == 0 and self.kind.get(r[0]) in VALUES: self.want(j)

    def drop(self, *ids):
        for i in ids:
            for q in range(AR[self.kind[i]]): self.link.pop((i, q), None)
            del self.kind[i]
            self.wanted.discard(i)

    def move(self, name, gone, joins):
        ends = [(self.link[a], self.link[b]) for a, b in joins]
        self.drop(*gone)
        for a, b in ends: self.join(a, b)
        self.fired[name] += 1

    def check_moves(self, i):
        k, at = self.kind.get(i), lambda q: self.link.get((i, q))
        kind = lambda e: e and self.kind.get(e[0])
        if k == 'A⁻' and at(0) and at(0)[1] == 3 and kind(at(0)) == 'A' and at(1) == (at(0)[0], 2):
            j = at(0)[0]
            self.move('un-apply·apply', [i, j], [((j, 0), (i, 3)), ((j, 1), (i, 2))])
        elif k == 'A⁻' and i in self.wanted and self.facing_unrun(i):
            self.settle(i)
        elif k == 'Dn⁻' and kind(at(0)) == 'Dn' and at(0)[1] in (1, 2):
            d = at(0)[0]
            if at(1) == (d, 3 - at(0)[1]): self.move('merge·copy', [i, d], [((d, 0), (i, 2))])
            elif at(1) and at(1)[1] == 0 and kind(at(1)) in VALUES: self.want(d)
        elif k == 'Eps' and kind(at(0)) == 'Dn' and at(0)[1] in (1, 2):
            d, q = at(0)
            self.move('Eps·copy (un-copy)', [i, d], [((d, 0), (d, 3 - q))])
        elif k == 'Eps' and kind(at(0)) == 'A' and at(0)[1] in (2, 3):
            j = at(0)[0]
            e2, e3 = self.link.get((j, 2)), self.link.get((j, 3))
            if all(e and e[1] == 0 and self.kind.get(e[0]) == 'Eps' for e in (e2, e3)):
                ends = [self.link[(j, 0)], self.link[(j, 1)]]
                self.drop(j, e2[0], e3[0])
                for e in ends: self.join((self.new('Eps'), 0), e)
                self.fired['dead work'] += 1

    def run(self, fuel=10**7):
        while self.todo or self.recheck:
            if self.recheck:
                self.check_moves(self.recheck.pop())
                continue
            a, b = self.todo.pop()
            if a not in self.kind or b not in self.kind: continue
            if self.link.get((a, 0)) != (b, 0) or 'Out' in (self.kind[a], self.kind[b]): continue
            r = self.rules.get((self.kind[a], self.kind[b]))
            if r is None:
                a, b = b, a
                r = self.rules.get((self.kind[a], self.kind[b]))
            if r is None: raise Stuck(f'no rule for {self.kind[a]}·{self.kind[b]}')
            if self.kind[a] in LAZY and a not in self.wanted: continue
            fuel -= 1
            if fuel < 0: raise OutOfFuel
            self.wanted.discard(a)
            self.fire(a, b, r)

    def force(self, e):
        """Read e as a tree, running whatever it is still waiting on."""
        while True:
            try: return self.read(e)
            except Need as n:
                before = sum(self.fired.values())
                self.want(n.i)
                self.run()
                if n.i in self.kind and sum(self.fired.values()) == before: raise Stuck(f'{self.kind[n.i]} wanted, nothing ran')


def size(t): return 1 + sum(size(k) for k in t[1:])


def built(t):
    """The agents Net.tree makes for t."""
    return [t[0]] + sum((built(k) for k in t[1:]), [])


# ---- what to run: random trees applied to random trees, and the player's programs ----

class Lcg:
    def __init__(self, s): self.s = s
    def next(self):
        self.s = (self.s * 1103515245 + 12345) & 0x7fffffff
        return self.s / 0x7fffffff
    def term(self, d):
        if d == 0 or self.next() < 0.35: return L
        f = self.term(d - 1)
        return ('Ap', f, self.term(d - 1))


def random_pairs(n):
    rng, trees = Lcg(20260730), []
    while len(trees) < 2 * n:
        try: trees.append(nf(rng.term(3 + len(trees) % 5), Fuel(5000)))
        except OutOfFuel: pass
    return [(trees[2 * i], trees[2 * i + 1]) for i in range(n)]


def programs(quick=False):
    js = open(f'{ROOT}/evaluators/rust-ic-strands/player/programs.js').read()
    def tree(s, i=0):
        k = s[i]
        if k == '0': return L, i + 1
        a, i = tree(s, i + 1)
        if k == '1': return S(a), i
        b, i = tree(s, i)
        return F(a, b), i
    nat = lambda n: L if n == 0 else F(L, nat(n - 1))
    def bits(n, k):
        if k == 0: return S(L) if n & 1 else L
        w = 1 << (1 << (k - 1))
        return F(bits(n % w, k - 1), bits(n // w, k - 1))
    def lst(xs):
        out = L
        for x in reversed(xs): out = F(nat(x), out)
        return out
    calls = {'add': [2, 3], 'mul': [2, 3], 'fib': [5], 'is_even': [4], 'sum': [[1, 2, 3]], 'rev': [[1, 2, 3]],
             'ripple_add': [3, 5], 'relay_add': [3, 5]}
    out = []
    for name, args in calls.items():
        if quick and name in ('mul', 'fib'): continue
        at = js.index(f'"name": "{name}"')
        entry = js[at:at + js[at:].index('}')]
        prog = tree(re.search(r'"tree": "(\d+)"', entry).group(1))[0]
        kind = re.search(r'"kind": "(\w+)"', entry).group(1)
        if kind == 'bits':
            m = re.search(r'"width": (\d+)', entry)
            k = (int(m.group(1)).bit_length() - 1) if m else next(k for k in range(8) if max(args) < 1 << (1 << k))
            vals = [bits(a, k) for a in args]
        else:
            vals = [lst(a) if isinstance(a, list) else nat(a) for a in args]
        out.append((f'{name} {" ".join(map(str, args))}', prog, vals))
    return out


# ---- checks ----

def chain_forward(net, prog, args):
    """prog a₁ … aₙ as a chain of applies; the result's end and each apply's residual sink."""
    at, sinks = net.tree(prog), []
    for a in args:
        out, res = net.sink(), net.sink()
        net.agent('A', [at, net.tree(a), out, res])
        at = net.link[out]
        net.link.pop(out)
        net.kind.pop(out[0])
        sinks.append(res)
    return at, sinks


def chain_back(net, at, residuals):
    """An un-apply for each residual end, last first, from the result's end; the argument sinks in
    order and the function's sink."""
    got = []
    for j in reversed(residuals):
        zo, fo = net.sink(), net.sink()
        net.agent('A⁻', [j, at, zo, fo])
        got.append(zo)
        at = net.link[fo]
    fo = net.sink()
    net.join(at, fo)
    return got[::-1], fo


def check(cases, short):
    """Run eagerly: forward on the net agrees with the oracle, residuals included; backward from
    (result, residuals) gives back the program and its arguments; the two in one net, nothing read
    out in between, do too; an eraser on every residual leaves the answer alone."""
    forward, steps = FORWARD if short else FORWARD_PLAIN, Counter()
    for name, prog, args in cases:
        fuel, r, js = Fuel(10**6), prog, []
        for a in args:
            r, j = app(r, a, fuel, short)
            js.append(j)
        assert r == nf(_ap(prog, args), Fuel(10**7)), f'{name}: residual apply disagrees with the lazy oracle'
        f = r
        for j, a in zip(reversed(js), reversed(args)):
            f, z = unapp(f, j)
            assert z == a, f'{name}: unapp'
        assert f == prog, f'{name}: unapp'

        net = Net(forward)
        end, sinks = chain_forward(net, prog, args)
        out = net.sink()
        net.join(end, out)
        net.run()
        assert net.read(out) == r and [net.read(s) for s in sinks] == js, f'{name}: net forward'
        steps['forward'] += sum(net.fired.values())

        net = Net(forward + ERASE)
        end, sinks = chain_forward(net, prog, args)
        out = net.sink()
        net.join(end, out)
        for s in sinks: net.agent('Eps', [net.link[s]])
        net.run()
        assert net.read(out) == r and net.live() == Counter(built(r)), f'{name}: cut'

        net = Net(BACKWARD)
        got, fo = chain_back(net, net.tree(r), [net.tree(fold(j)) for j in js])
        net.run()
        assert net.read(fo) == prog and [net.read(z) for z in got] == args, f'{name}: run back'
        steps['backward'] += sum(net.fired.values())

        net = Net(forward + BACKWARD)
        end, sinks = chain_forward(net, prog, args)
        got, fo = chain_back(net, end, [net.link[s] for s in sinks])
        net.run()
        assert net.read(fo) == prog and [net.read(z) for z in got] == args, f'{name}: one net'
        assert net.live() == Counter(built(prog) + sum((built(a) for a in args), [])), f'{name}: leftovers {net.live()}'
    return steps


def linked(prog, args, short):
    """prog a₁ … aₙ on a LazyNet, answer read out, then run back from the live net, links and
    unrun applies intact; and the same with an eraser on every residual instead."""
    forward, fuel, r = FORWARD if short else FORWARD_PLAIN, Fuel(10**6), prog
    for a in args: r = app(r, a, fuel)[0]
    net = LazyNet(forward + BACKWARD)
    end, sinks = chain_forward(net, prog, args)
    out = net.sink()
    net.join(end, out)
    assert net.force(out) == r, 'linked answer'
    fwd = Counter(net.fired)
    residual = sum(net.count(net.link[s]) for s in sinks)
    got, fo = chain_back(net, net.link[out], [net.link[s] for s in sinks])
    assert net.force(fo) == prog and [net.force(z) for z in got] == args, 'linked run back'
    exact = net.live() == Counter(built(prog) + sum((built(a) for a in args), []))

    cut = LazyNet(forward + BACKWARD + ERASE)
    end, sinks = chain_forward(cut, prog, args)
    out = cut.sink()
    cut.join(end, out)
    for s in sinks: cut.agent('Eps', [cut.link[s]])
    assert cut.force(out) == r, 'linked cut'
    cut.run()
    return dict(answer=size(r), residual=residual, fwd=fwd, back=net.fired - fwd, cut=cut.fired, exact=exact,
                left=cut.live() - Counter(built(r)))


def canon(r, kmap):
    """A rule as wired, its agents renamed by kmap; the same for two rules means the same rule."""
    n, name = len(r['fresh']), lambda k: kmap.get(k, k)
    best = None
    for perm in itertools.permutations(range(n)):
        tags = tuple(name(r['fresh'][perm.index(p)]) for p in range(n))
        end = lambda e: ('X', perm[e[1]], e[2]) if e[0] == 'X' else e
        key = repr((name(r['c']), name(r['p']), tags, sorted(sorted((end(a), end(b)), key=repr) for a, b in r['wires'])))
        best = key if best is None else min(best, key)
    return best


def _ap(f, args):
    for a in args: f = ('Ap', f, a)
    return f


RUST_NAME = {'A': 'RA', 'T1': 'RT1', 'Sel': 'RSel', 'A⁻': 'UA', 'A⁻r': 'UAr', 'Sel⁻': 'USel', 'SL⁻': 'USL', 'SS⁻': 'USS',
             'SF⁻': 'USF', 'Dn⁻': 'Mg', 'Dn⁻L': 'MgL', 'Dn⁻S': 'MgS', 'Dn⁻F': 'MgF'}


def rust():
    """FORWARD, BACKWARD and the erasers for the new values, as rules.rs `Rule` literals; the rules the
    lattice already has (duplicators, unpairs, erasers of trees and pairs) left out."""
    have = {('Unp', 'Pair'), ('Dn', 'L'), ('Dn', 'S'), ('Dn', 'F'), ('Eps', 'L'), ('Eps', 'S'), ('Eps', 'F'), ('Eps', 'Pair')}
    name = lambda k: RUST_NAME.get(k, k)
    end = lambda e: f'X({e[1]}, {e[2]})' if e[0] == 'X' else f'C({e[1]})' if e[0] == 'C' else f'Px({e[1]})'
    for group, rules in [('forward', FORWARD), ('backward', BACKWARD), ('erasing history', ERASE)]:
        print(f'    // ---- {group} ----')
        for r in rules:
            if (r['c'], r['p']) in have: continue
            fresh, wires = ', '.join(map(name, r['fresh'])), ', '.join(f'({end(a)}, {end(b)})' for a, b in r['wires'])
            print(f'    Rule {{ consumer: {name(r["c"])}, producer: {name(r["p"])}, fresh: &[{fresh}], wires: &[{wires}], ..PLAIN }},')


def main():
    if '--rust' in sys.argv: return rust()
    quick = '--quick' in sys.argv
    cases = [(f'random {i}', f, [z]) for i, (f, z) in enumerate(random_pairs(300))]
    progs = programs(quick)
    for short in (False, True):
        steps = check(cases + progs, short)
        print(f'eager{", B and C" if short else ""}: forward (residuals included), back, both in one net and the cut agree '
              f'with the oracle on {len(cases)} random applies and {len(progs)} programs; rewrites back/forward = '
              f'{steps["backward"] / steps["forward"]:.2f}')

    print('\nlazy, copies kept as links; residual sizes in agents, eager then lazy:')
    print(f"{'':16} {'':6} {'':>16}{'without B, C':^34}{'':>3}{'with B and C':^34}")
    print(f"{'program':16} {'answer':>6}   " + 2 * f"{'residual':>9} {'lazy':>6} {'fwd':>7} {'back':>7} {'back/fwd':>8}   ")
    moves, inexact = Counter(), []
    for short in (False, True):
        for name, f, args in cases: linked(f, args, short)
    for name, prog, args in progs:
        row = ''
        for short in (False, True):
            fuel, r, js = Fuel(10**6), prog, []
            for a in args:
                r, j = app(r, a, fuel, short)
                js.append(j)
            d = linked(prog, args, short)
            assert not d['left'], f'{name}: the cut left {d["left"]}'
            if not d['exact']: inexact.append(name)
            moves += Counter({k: n for k, n in (d['fwd'] + d['back'] + d['cut']).items() if k in MOVES})
            fwd, back = sum(d['fwd'].values()), sum(d['back'].values())
            row += f"{sum(map(size, js)):9} {d['residual']:6} {fwd:7} {back:7} {back / fwd:8.2f}   "
        print(f"{name:16} {d['answer']:6}   {row}")
    print(f'moves that are not active pairs: {dict(moves)}')
    print(f'run back to the very agents loaded: {"every run" if not inexact else "not " + ", ".join(inexact)}')

    print('\nbackward rules that are a forward rule with the walker turned round (A⁻→A, Sel⁻→Sel, …, Dn⁻→Dn):')
    twin = lambda r, q: (TWIN.get(r['c'], r['c']), r['p'], sorted(TWIN.get(k, k) for k in r['fresh'])) == (q['c'], q['p'], sorted(q['fresh']))
    print('  ' + ', '.join(f"{r['name']} = {q['name']}" for r in BACKWARD for q in FORWARD if twin(r, q) and canon(r, TWIN) == canon(q, {})))
    for r in BACKWARD: audit.check(r)
    print('backward rules not connected: ' + ', '.join(f"{r['name']} ({len(audit.pieces(r))} pieces)" for r in BACKWARD if len(audit.pieces(r)) != 1))

    print('\nwhat runs each case, forward and back (with B and C):')
    show = lambda c: ' '.join(f'{k}×{n}' if n > 1 else k for k, n in sorted(c.items()))
    for case, (f, z) in {'construct △ z': (L, S(L)), 'construct △x z': (S(L), L), 'K': (F(L, S(L)), F(L, L)),
                         'S': (F(S(L), L), L), 'triage on △': (F(F(L, L), L), L), 'triage on △u': (F(F(L, L), L), S(L)),
                         'triage on △uv': (F(F(L, L), S(L)), F(L, L)), 'B': (F(S(F(L, L)), L), L),
                         'C': (F(S(S(L)), F(L, L)), L)}.items():
        net = LazyNet(FORWARD + BACKWARD)
        end, sinks = chain_forward(net, f, [z])
        out = net.sink()
        net.join(end, out)
        net.force(out)
        fwd = Counter(net.fired)
        got, fo = chain_back(net, net.link[out], [net.link[s] for s in sinks])
        net.force(fo)
        net.force(got[0])
        print(f'  {case:15} forward: {show(fwd):52} back: {show(net.fired - fwd)}')


if __name__ == '__main__':
    sys.setrecursionlimit(1_000_000)
    threading.stack_size(1 << 28)
    t = threading.Thread(target=main)
    t.start()
    t.join()
