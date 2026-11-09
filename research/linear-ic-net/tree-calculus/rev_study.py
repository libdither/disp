"""Running LTC backward. Footprints (path, rule) per step; undo; is the predecessor unique without them?"""
import sys, threading
from collections import Counter
from rules import L, A, spine, size, subst, show, is_var, get, put
from ltc2 import fire2
from radd import comp, R, lam, tri, nat
from radd2 import build as build_radd2
RULE = {n: (l, r) for n, l, r in R}

def step(t, path=()):
    """one leftmost-outermost step: (t', redex path, rule) or None"""
    h, args = spine(t)
    n = len(args)
    if h == L and n >= 3:
        r = fire2(args, R)
        if r and r[0] == 'done':
            return A(r[1][0] if False else subst(RULE[r[2]][1], r[3]), *args[3:]), path + (0,) * (n - 3), r[2]
        if r and r[0] == 'need':
            i = r[1]; res = step(args[i], path + (0,) * (n - 1 - i) + (1,))
            a = list(args); a[i] = res[0]
            return A(L, *a), res[1], res[2]
    for i, a in enumerate(args):
        res = step(a, path + (0,) * (n - 1 - i) + (1,))
        if res:
            b = list(args); b[i] = res[0]
            return A(h, *b), res[1], res[2]
    return None

def unmatch(p, t, env):
    if is_var(p):
        if p in env: return env[p] == t
        env[p] = t; return True
    if p == L: return t == L
    return isinstance(t, tuple) and unmatch(p[0], t[0], env) and unmatch(p[1], t[1], env)

def undo(t, path, name):
    lhs, rhs = RULE[name]; env = {}
    assert unmatch(rhs, get(t, path), env)
    return put(t, path, subst(lhs, env))

def run(t, limit=10 ** 6):
    hist = []
    while True:
        r = step(t)
        if r is None: return t, hist
        t = r[0]; hist.append((r[1], r[2]))

def positions(t, path=()):
    yield path
    if isinstance(t, tuple):
        yield from positions(t[0], path + (0,)); yield from positions(t[1], path + (1,))

def predecessors(t):
    """every (path, rule) other than K whose undo gives a term whose next leftmost-outermost step is that one, giving t"""
    out = []
    for p in positions(t):
        sub = get(t, p)
        for name, (lhs, rhs) in RULE.items():
            if name == 'K': continue
            env = {}
            if not unmatch(rhs, sub, env): continue
            s = put(t, p, subst(lhs, env))
            r = step(s)
            if r and r[1] == p and r[2] == name and r[0] == t: out.append((p, name))
    return out

def main():
    I = A(L, L, L); B = lambda q, r: A(L, L, A(L, q, r)); C = lambda q, r: A(L, L, A(L, q, r))
    # 1. collisions with no K step: the eaten nodes carry information too
    D = A(L, A(L, I, I), I)
    for x in (L, A(L, I)):
        v, h = run(A(D, x)); print(f'peel {show(x):14} -> {show(v)}   rules {[n for _, n in h]}')
    a = A(L, L)   # any value
    Bss = lambda f, g: A(L, A(L, A(L, f)), g)
    T = A(L, L, A(L, I, a))       # C I a: x -> I x a -> x a
    for x in (I, Bss(I, I)):
        v, h = run(A(T, x)); print(f'(C I a) {show(x):28} -> {show(v)}   rules {[n for _, n in h]}')
    # 2. the reversible add, forward and back with footprints
    Cc, Dd, pack, call = build_radd2(('a', 'n', 'dh'))
    for m, n in ((5, 3), (12, 9)):
        t0 = call(m, n); t1, hist = run(t0)
        assert t1 == A(L, A(L, pack, Dd), A(L, nat(m), nat(m + n)))
        t = t1
        for p, nm in reversed(hist): t = undo(t, p, nm)
        assert t == t0
        c = Counter(nm for _, nm in hist)
        print(f'radd {m} {n}: {len(hist)} steps forward (normal order), K {c["K"]}, W {c["W"]}; '
              f'undone from {len(hist)} footprints back to the exact start')
    # 3. without footprints: how many non-K predecessors does each state have?
    for m, n in ((1, 1), (2, 1)):
        t0 = call(m, n); t1, hist = run(t0)
        counts = []; t = t1
        for p, nm in reversed(hist):
            pr = predecessors(t); counts.append(len(pr)); assert (p, nm) in pr
            t = undo(t, p, nm)
        print(f'radd {m} {n}: {len(counts)} backward steps; predecessors per step: '
              f'unique at {sum(c == 1 for c in counts)}, max {max(counts)}, mean {sum(counts) / len(counts):.1f}')
if __name__ == '__main__':
    threading.stack_size(512 * 1024 * 1024); th = threading.Thread(target=main); th.start(); th.join()
