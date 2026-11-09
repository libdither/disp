"""Superoptimizer on plain tuples (no hash-consing): leaf 0, atom negative int, stem (u,), fork (u, v),
neutral application ('@', f, x). Finds the smallest K-free tree f with f a1..an ->* target for every test vector."""
import sys, time, itertools
sys.setrecursionlimit(100000)

class Stuck(Exception): pass

def apply(f, x, limit):
    """eager f x; returns (value, steps). K raises Stuck (search is K-free)."""
    stack = []; steps = 0
    while True:
        tf = type(f)
        if tf is tuple and len(f) == 2:
            a, b = f
            steps += 1
            if steps > limit: raise Stuck
            if a == 0:
                if b == 0: v = x
                elif type(b) is tuple:
                    if len(b) == 2: stack.append(b[1]); f = b[0]; continue          # C: q x r
                    raise Stuck                                                    # K or neutral
                else: raise Stuck
            elif type(a) is tuple and len(a) == 1:
                p = a[0]
                if p == 0: stack.append(x); f = b; continue                        # W: b x x
                if type(p) is tuple:
                    if len(p) == 1: stack.append([p[0]]); f = b; continue          # B: f (b x)
                    if len(p) == 2: stack.append([p[0]]); stack.append([p[1]]); f = b; continue  # B2
                raise Stuck
            elif type(a) is tuple and len(a) == 2:
                c, d = a
                if x == 0: stack.append(b); f = c; x = d; continue                 # T0: c d b
                if type(x) is tuple:
                    if len(x) == 1: stack.append(x[0]); stack.append(b); f = d; x = c; continue   # T1: d c b u
                    if len(x) == 2: stack.append(x[1]); stack.append(x[0]); stack.append(d); f = b; x = c; continue  # T2
                raise Stuck
            else: raise Stuck
        elif f == 0: v = (x,)
        elif tf is tuple and len(f) == 1: v = (f[0], x)
        else: v = ('@', f, x)
        if not stack: return v, steps
        y = stack.pop()
        if type(y) is list: f = y[0]; x = v
        else: f = v; x = y

def run(f, args, limit):
    total = 0
    for a in args:
        f, n = apply(f, a, limit - total); total += n
    return f, total

def size(t):
    if t == 0: return 1
    if type(t) is int: return 0
    if len(t) == 3: return size(t[1]) + size(t[2])
    return 1 + sum(size(c) for c in t)

_memo = {1: [0]}
def trees(n, consts=()):
    """all trees with n nodes (each constant atom counts as one node)"""
    key = (n, consts)
    if key in _memo: return _memo[key]
    if n == 1: out = [0] + list(consts)
    else:
        out = [(t,) for t in trees(n - 1, consts)]
        for i in range(1, n - 1):
            L = trees(i, consts); R = trees(n - 1 - i, consts)
            out.extend((t, u) for t in L for u in R)
    if n <= 15: _memo[key] = out
    return out

def gen(n, consts=()):
    """generator over trees of size n, memoizing small ones only"""
    if n <= 15:
        yield from trees(n, consts); return
    for t in gen(n - 1, consts): yield (t,)
    for i in range(1, n - 1):
        for t in (trees(i, consts) if i <= 15 else list(gen(i, consts))):
            for u in gen(n - 1 - i, consts): yield (t, u)

def search(tests, nmin=1, nmax=18, consts=(), limit=40, mod=None, want=1, verbose=True):
    """tests: list of (args, target). mod: (r, 3) to try only sizes = r mod 3."""
    found = []
    t0 = time.time()
    for n in range(nmin, nmax + 1):
        if mod is not None and n % mod[1] != mod[0] % mod[1]: continue
        cnt = 0
        for f in gen(n, consts):
            cnt += 1
            ok = True; tot = 0
            for args, target in tests:
                try:
                    v, s = run(f, args, limit)
                except Stuck:
                    ok = False; break
                if v != target: ok = False; break
                tot += s
            if ok:
                found.append((n, tot, f))
                if verbose: print(f'    found size {n} steps {tot}: {show(f)}', flush=True)
        if verbose: print(f'  size {n}: {cnt} trees, {len(found)} found, {time.time() - t0:.1f}s', flush=True)
        if len(found) >= want: break
    return found

NAMES = {}
def atoms(*names):
    out = []
    for nm in names:
        if nm not in NAMES: NAMES[nm] = -(len(NAMES) + 1)
        out.append(NAMES[nm])
    return out
def show(t):
    if t == 0: return '△'
    if type(t) is int: return next(k for k, v in NAMES.items() if v == t)
    if len(t) == 1: return f'(△ {show(t[0])})'
    if len(t) == 2: return f'(△ {show(t[0])} {show(t[1])})'
    return f'[{show(t[1])} {show(t[2])}]'
def napp(*ts):
    t = ts[0]
    for u in ts[1:]: t = ('@', t, u)
    return t
def fork(u, v): return (u, v)
def stem(u): return (u,)
L = 0

def to_proto(t):
    """tuple tree -> prototype term ('L', pairs)"""
    if t == 0: return 'L'
    if type(t) is int: return show(t)
    if len(t) == 1: return ('L', to_proto(t[0]))
    if len(t) == 2: return (('L', to_proto(t[0])), to_proto(t[1]))
    return (to_proto(t[1]), to_proto(t[2]))

if __name__ == '__main__':
    c, s = atoms('c', 's')
    print('frame rebuild d c s -> △ (△ c s) s')
    search([([c, s], fork(fork(c, s), s))], nmax=14, mod=(2, 3), want=1)
