"""Bennett's method on ordinary (non-injective) add, against the reversible add, in normal order."""
import threading
from collections import Counter
from rules import L, A, size, show
from radd import comp, R, lam, tri, nat, ADD, add_call
from radd2 import build as build_radd2
from family import wait_of
from rev_study import run, undo
W8 = wait_of('V5 I K C | W B B2')
c = lam('o1', 'o2', 'n', 'g', 'k', 'r', A('k', 'n', A(L, A(L, 'o1', 'o2'), A(L, 'r', 'g'))))
d = lam('o1', 'o2', 'p', 'n', 'g', 'k', 'r',
        A('r', 'p', 'n', A(L, A(L, 'o1', 'o2'), 'g'), lam('s', 'g2', A('k', A(L, 's'), 'g2'))))
b = lam('o1', 'o2', 'p', 'q', 'n', 'g', 'k', 'r',
        A('k', 'n', A(L, A(L, 'o1', 'o2'), A(L, 'p', A(L, 'q', A(L, 'r', 'g'))))))
KEEP = comp(lam('self', 'm', 'n', 'g', 'k', A(tri(c, d, b, 'm'), 'n', 'g', 'k', W8('self', 'self'))))
K0 = comp(lam('s', 'g', A(L, 's', 'g')))
def main():
    Cc, Dd, pack, rcall = build_radd2(('a', 'n', 'dh'))
    print(f"{'m n':6} {'ordinary add':>22} {'keep garbage':>30} {'Bennett on it':>22} {'reversible add':>22}")
    for m, n in ((5, 3), (12, 9), (25, 20)):
        t, h = run(add_call(m, n)); assert t == nat(m + n)
        ko = sum(nm == 'K' for _, nm in h)
        t0 = A(KEEP, KEEP, nat(m), nat(n), L, K0); t1, hk = run(t0)
        assert t1[0][1] == nat(m + n) and not any(nm == 'K' for _, nm in hk)
        garbage = size(t1) - size(nat(m + n)) - 1
        u = t1
        for p, nm in reversed(hk): u = undo(u, p, nm)
        assert u == t0
        tr, hr = run(rcall(m, n))
        print(f'{m:>2} {n:<3} {len(h):>8} steps {ko:>4} K   {len(hk):>8} steps {garbage:>7} garbage'
              f'   {2 * len(hk) + 1:>8} steps {len(hk):>5} fp   {len(hr):>8} steps   0 K')
threading.stack_size(512 * 1024 * 1024); th = threading.Thread(target=main); th.start(); th.join()
