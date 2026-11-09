"""Superoptimizer with exactly one continuation constant K (an atom) among the leaves.
Usage: python so3.py JOB NMAX. Finds the smallest tree f (K-free) with f args ->* target, K appearing once."""
import sys, time
from so2 import apply, run, Stuck, atoms, napp, fork, stem, show, trees, L
_g1 = {}
def gen1(n, K):
    """trees of size n with exactly one leaf equal to K (other leaves △)"""
    if n == 1:
        yield K; return
    if n <= 13 and n in _g1:
        yield from _g1[n]; return
    out = [] if n <= 13 else None
    def emit(t):
        if out is not None: out.append(t)
        return t
    for t in gen1(n - 1, K): yield emit((t,))
    for i in range(1, n - 1):
        j = n - 1 - i
        for t in gen1(i, K):
            for u in trees(j): yield emit((t, u))
        for t in trees(i):
            for u in gen1(j, K): yield emit((t, u))
    if out is not None: _g1[n] = out

def search1(tests, K, nmax, limit=40, want=1, nmin=1):
    found = []; t0 = time.time()
    for n in range(nmin, nmax + 1):
        cnt = 0
        for f in gen1(n, K):
            cnt += 1; ok = True; tot = 0
            for args, target in tests:
                try: v, s = run(f, args, limit)
                except Stuck: ok = False; break
                if v != target: ok = False; break
                tot += s
            if ok:
                found.append((n, tot, f)); print(f'    found size {n} (real {n - 1} + K) steps {tot}: {show(f)}', flush=True)
        print(f'  size {n}: {cnt} trees, {len(found)} found, {time.time() - t0:.1f}s', flush=True)
        if len(found) >= want: break
    return found

if __name__ == '__main__':
    sp, Y, lf, sib, br, bf, ot, nxt, ar, af, acc = atoms('sp', 'Y', 'lf', 'sib', 'brest', 'bf', 'other', 'nxt', 'arest', 'af', 'acc')
    K, = atoms('K')
    job, nmax = sys.argv[1], int(sys.argv[2])
    own0 = lambda E: fork(fork(E, lf), sib)          # b = 0: △ (△ E lf) sib
    own1 = lambda E: fork(fork(E, sib), lf)          # b = 1: △ (△ E sib) lf
    jobs = {
      'P1':   ([sp, Y], napp(K, fork(fork(sp, sp), Y))),
      'G00':  ([lf, sib, br, ot], napp(K, fork(fork(br, own0(L)), ot))),
      'G01':  ([lf, sib, br, bf, ot], napp(K, fork(fork(br, own1(bf)), ot))),
      'G10':  ([lf, sib, br, ot], napp(K, fork(fork(br, ot), own0(L)))),
      'G11':  ([lf, sib, br, bf, ot], napp(K, fork(fork(br, ot), own1(bf)))),
    }
    if job in jobs:
        args, tgt = jobs[job]
        search1([(args, tgt)], K, nmax)
