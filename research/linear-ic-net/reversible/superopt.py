"""Find the smallest LTC tree f with f a1 .. an ->* target (eager, symbolic atoms), K-free by default.
Trees are enumerated by node count; extra leaf symbols (named constants) may be allowed."""
import sys, time
import ltcfast as F

def trees_upto(n, consts=()):
    """by[k] = list of tree ids with exactly k nodes (each constant counts as 1)"""
    by = [[] for _ in range(n + 1)]
    by[1] = [F.LEAF] + [F.atom(c) for c in consts]
    for k in range(2, n + 1):
        out = [F.stem(t) for t in by[k - 1] if F.KIND[t] != 3 or True]
        for i in range(1, k - 1):
            for t in by[i]:
                for u in by[k - 1 - i]:
                    out.append(F.fork(t, u))
        by[k] = out
    return by

def check(f, args, target, limit, allow_k=False):
    st = F.Stats(); v = f
    try:
        for a in args: v = F.apply_v5(v, a, st, limit)
    except (F.Stuck, F.Limit):
        return None
    if v != target or (st.K and not allow_k): return None
    return st

def search(args, target, nmax, consts=(), limit=60, allow_k=False, want=1, verbose=False):
    """args, target: ids built with F.atom; returns list of (size, steps, tree id, stats)"""
    found = []
    by = [[] for _ in range(nmax + 1)]
    by[1] = [F.LEAF] + [F.atom(c) for c in consts]
    t0 = time.time()
    for k in range(1, nmax + 1):
        if k >= 2:
            out = [F.stem(t) for t in by[k - 1]]
            for i in range(1, k - 1):
                bi = by[i]; bj = by[k - 1 - i]
                for t in bi:
                    for u in bj: out.append(F.fork(t, u))
            by[k] = out
        for f in by[k]:
            st = check(f, args, target, limit, allow_k)
            if st is not None: found.append((k, st.steps, f, st))
        if verbose: print(f'  size {k}: {len(by[k])} trees, found {len(found)} ({time.time() - t0:.1f}s)', flush=True)
        if len(found) >= want: break
    found.sort(key=lambda r: (r[0], r[1]))
    return found

if __name__ == '__main__':
    A = F.atom
    c, s, u = A('c'), A('s'), A('u')
    # frame rebuild: d c s ->* △ (△ c s) s
    tgt = F.fork(F.fork(c, s), s)
    res = search([c, s], tgt, 15, verbose=True, want=3)
    for k, n, f, st in res[:5]: print(k, n, F.show(f), st)
