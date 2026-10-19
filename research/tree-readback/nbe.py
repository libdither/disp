# Symbolic tree calculus (triage variant, as oracle.rs): code applied to named variables, reduced,
# and read back. Terms: ('L',) ('S',a) ('F',a,b) ('V',name) ('A',f,x) application, and the stuck
# forms ('M', scrut, w, x, b) = match scrut {L -> w; S u -> x u; F u v -> b u v}.
import sys, itertools
sys.setrecursionlimit(100000)
L = ('L',)
def S(a): return ('S', a)
def F(a, b): return ('F', a, b)
def A(f, x): return ('A', f, x)
def V(n): return ('V', n)

def parse_ternary(s):
    it = iter(s)
    def go():
        c = next(it)
        if c == '0': return L
        if c == '1': return S(go())
        a = go(); return F(a, go())
    return go()

class Fuel(Exception): pass
def whnf(t, fuel):
    while t[0] == 'A':
        fuel[0] -= 1
        if fuel[0] < 0: raise Fuel()
        f = whnf(t[1], fuel); x = t[2]
        if f[0] == 'L': t = S(x)
        elif f[0] == 'S': t = F(f[1], x)
        elif f[0] == 'F': t = triage(f[1], f[2], x, fuel)
        else: return A(f, x)            # stuck: a variable or a match applied
    return t
def triage(a, b, c, fuel):
    a = whnf(a, fuel)
    if a[0] == 'L': return b
    if a[0] == 'S': return A(A(a[1], c), A(b, c))
    if a[0] == 'F':
        c = whnf(c, fuel)
        if c[0] == 'L': return a[1]
        if c[0] == 'S': return A(a[2], c[1])
        if c[0] == 'F': return A(A(b, c[1]), c[2])
        return ('M', c, a[1], a[2], b)  # the argument is unknown: a match on it
    return A(F(a, b), c)                # the fork's left is unknown
def nf(t, fuel):
    t = whnf(t, fuel)
    if t[0] == 'S': return S(nf(t[1], fuel))
    if t[0] == 'F': return F(nf(t[1], fuel), nf(t[2], fuel))
    if t[0] == 'A': return A(nf(t[1], fuel), nf(t[2], fuel))
    if t[0] == 'M': return ('M', t[1], nf(t[2], fuel), nf(t[3], fuel), nf(t[4], fuel))
    return t

def has_var(t):
    if t[0] == 'V': return True
    return any(has_var(c) for c in t[1:] if isinstance(c, tuple))

def ternary(t):
    out = []
    def go(t):
        if t[0] == 'L': out.append('0')
        elif t[0] == 'S': out.append('1'); go(t[1])
        elif t[0] == 'F': out.append('2'); go(t[1]); go(t[2])
        else: raise ValueError('not a value')
    go(t); return ''.join(out)

def closed_value(t):
    return t[0] in ('L', 'S', 'F') and not has_var(t)

def nat_of(t):
    n = 0
    while t[0] == 'F' and t[1] == L: n += 1; t = t[2]
    return n if t == L else None

class Printer:
    """Reads a term back as disp-like text: data as literals, known code by name (exact tree match),
    applications of known code as calls, other closed code as a lambda by applying it to fresh
    variables (up to max_arity), stuck matches as match expressions."""
    def __init__(self, names=None, max_arity=3, fuel=20000):
        self.names = names or {}          # ternary -> name
        self.max_arity, self.fuel = max_arity, fuel
        self.fresh = (f"x{i}" for i in itertools.count())
    def atom(self, s): return s if (' ' not in s or s[0] in '([{"') else f"({s})"
    def show(self, t, depth=0):
        if depth > 12: return '…'
        tag = t[0]
        if tag == 'V': return t[1]
        if tag == 'M':
            # A match whose arms rebuild what it looked at is that thing (identity on trees).
            if t[2] == L and t[3] == L and t[4] == L: return self.show(t[1], depth)
            return f"match {self.atom(self.show(t[1], depth+1))} {{ △ => {self.show(t[2], depth+1)}; △u => {self.atom(self.show(t[3], depth+1))} u; △u v => {self.atom(self.show(t[4], depth+1))} u v }}"
        if tag == 'A':
            f, args = t, []
            while f[0] == 'A': args.append(f[2]); f = f[1]
            return ' '.join([self.atom(self.show(f, depth+1))] + [self.atom(self.show(a, depth+1)) for a in reversed(args)])
        if closed_value(t):
            k = ternary(t)
            if k in self.names: return self.names[k]
            if t == L: return 't'
            n = nat_of(t)
            if n is not None: return str(n)
            return self.lam(t, depth)
        # a value with variables inside: show its shape
        if tag == 'S': return f"△{self.atom(self.show(t[1], depth+1))}"
        return f"△{self.atom(self.show(t[1], depth+1))} {self.atom(self.show(t[2], depth+1))}"
    def lam(self, t, depth):
        """Closed code: the smallest arity whose body no longer looks like routing."""
        best = None
        for k in range(1, self.max_arity + 1):
            xs = [next(self.fresh) for _ in range(k)]
            body = t
            for x in xs: body = A(body, V(x))
            try: body = nf(body, [self.fuel])
            except Fuel: break
            text = self.show(body, depth + 1)
            cand = f"λ{' '.join(xs)}. {text}"
            if best is None or len(cand) < len(best): best = cand
            if not closed_value(body): break
        return best or ternary(t)
