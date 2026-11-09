"""Linear tree calculus (LTC) vs tree calculus (TC): rules as data, one engine for both.
Terms: 'L' is the leaf △; a tuple (f, x) is application. Pattern variables are other strings."""
import sys
sys.setrecursionlimit(200000)
L = 'L'

def A(*ts):
    t = ts[0]
    for x in ts[1:]:
        t = (t, x)
    return t

def spine(t):
    args = []
    while isinstance(t, tuple):
        args.append(t[1]); t = t[0]
    args.reverse()
    return t, args

def size(t):
    n, st = 0, [t]
    while st:
        u = st.pop()
        if isinstance(u, tuple): st.append(u[0]); st.append(u[1])
        elif u == L: n += 1
    return n

def is_var(p): return isinstance(p, str) and p != L

TRI = [
    ('T0', A(L, A(L, 'c', 'd'), 'b', L), 'c'),
    ('T1', A(L, A(L, 'c', 'd'), 'b', A(L, 'u')), A('d', 'u')),
    ('T2', A(L, A(L, 'c', 'd'), 'b', A(L, 'u', 'v')), A('b', 'u', 'v')),
]
# disp's tree calculus (src/eval/naive.ts): K, S, triage
TC = [('K', A(L, L, 'b', 'x'), 'b'),
      ('S', A(L, A(L, 'p'), 'b', 'x'), A('p', 'x', A('b', 'x')))] + TRI
# LTC: S is split by the shape of its first operand into copy / right / left
LTC = [('K', A(L, L, 'b', 'x'), 'b'),
       ('W', A(L, A(L, L), 'b', 'x'), A('b', 'x', 'x')),
       ('B', A(L, A(L, A(L, 'f')), 'b', 'x'), A('f', A('b', 'x'))),
       ('C', A(L, A(L, A(L, 'f', 'g')), 'b', 'x'), A('f', A('g', 'x'), 'b'))] + TRI

class Need(Exception):
    pass

def match(p, t, env):
    if is_var(p):
        env[p] = t; return True
    h, args = spine(t)
    if h == L and len(args) >= 3:
        raise Need()
    if p == L:
        return t == L
    if not isinstance(t, tuple):
        return False
    return match(p[0], t[0], env) and match(p[1], t[1], env)

def subst(p, env):
    if is_var(p): return env[p]
    if p == L: return L
    return (subst(p[0], env), subst(p[1], env))

def fire(args, rules, stats):
    """Try the rules on a redex △ a1 a2 a3 ...; return ('done', term) | ('need', i) | None."""
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
        if ok:
            if stats is not None: stats[name] = stats.get(name, 0) + 1
            return ('done', A(subst(rhs, env), *args[3:]))
    if need is not None: return ('need', need)
    return None   # a free variable sits where a shape is needed: the redex is stuck (neutral)

def step_outer(t, rules, stats=None):
    """One leftmost-outermost step, or None at normal form."""
    h, args = spine(t)
    if h == L and len(args) >= 3:
        r = fire(args, rules, stats)
        if r is not None and r[0] == 'done': return r[1]
        if r is not None:
            i = r[1]
            args = list(args); args[i] = step_outer(args[i], rules, stats)
            return A(L, *args)
    for i, a in enumerate(args):
        r = step_outer(a, rules, stats)
        if r is not None:
            args = list(args); args[i] = r
            return A(h, *args)
    return None

def redexes(t, path=()):
    """All positions where a rule fires right now (no reduction needed first)."""
    out = []
    h, args = spine(t)
    if h == L and len(args) >= 3: out.append(path)
    # subterm positions: walk the tuple tree
    if isinstance(t, tuple):
        out += redexes(t[0], path + (0,)) if isinstance(t[0], tuple) else []
        out += redexes(t[1], path + (1,))
    return list(dict.fromkeys(out))

def get(t, path):
    for k in path: t = t[k]
    return t

def put(t, path, v):
    if not path: return v
    k = path[0]
    return (put(t[0], path[1:], v), t[1]) if k == 0 else (t[0], put(t[1], path[1:], v))

def step_at(t, path, rules, stats=None):
    """Fire the redex at path if its rule applies now; None otherwise."""
    s = get(t, path)
    h, args = spine(s)
    if not (h == L and len(args) >= 3): return None
    r = fire(args, rules, stats)
    if r is None or r[0] != 'done': return None
    return put(t, path, r[1])

def normalize(t, rules, limit=100000, stats=None):
    n = 0
    while True:
        r = step_outer(t, rules, stats)
        if r is None: return t, n
        t, n = r, n + 1
        if n >= limit: return None, n

def show(t):
    if t == L: return '△'
    if not isinstance(t, tuple): return t
    h, args = spine(t)
    return '(' + ' '.join([show(h)] + [show(a) for a in args]) + ')'
