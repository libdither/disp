"""Bits as programs that route their operands: Z k x y -> k x y (Z = I = △△△), O k x y -> k y x.
Controlled swap with a router control bit needs no triage: copy the control (W, 3 or 9 nodes), let one copy
route the operands. Program handed back: P = C M sp (P c -> M c sp ...), end term △ (△ c (△ x' y')) P."""
from common import *
import itertools
I = A(L, L, L)
O = comp(lam('k', 'x', 'y', A('k', 'y', 'x')))
def rbit(b): return O if b else I
def cswap_router():
    # M c sp x y: W-copy c, route (x, y) with one copy into a continuation that rebuilds everything
    M = comp(lam('c', 'sp', 'x', 'y', A('c', lam('p', 'q', A(L, A(L, 'c', A(L, 'p', 'q')), A(L, L, A(L, 'sp', 'sp')))), 'x', 'y')))
    return A(L, L, A(L, M, M))
def main():
    print('router O size', size(O), 'steps to run', end=' ')
    v, st = run(A(O, 'k', 'x', 'y')); print(st.steps)
    P = cswap_router()
    print(HDR)
    rng = random.Random(1)
    from gates import cellword, bitv
    for label, mk in (('router cswap, 1-bit operands', lambda: (bitv(rng.randrange(2)), bitv(rng.randrange(2)))),
                      ('router cswap, 32-bit words', lambda: (cellword(rng, 32), cellword(rng, 32)))):
        for c in (0, 1):
            s = F.Stats(); n = 10
            for _ in range(n):
                x, y = mk()
                v, st = run(A(P, rbit(c), x, y))
                want = A(L, A(L, rbit(c), A(L, y, x) if c else A(L, x, y)), P)
                assert v == V(want) and st.K == 0, (label, c); s = s + st
            a = F.Stats()
            for k in a.__slots__: setattr(a, k, round(getattr(s, k) / n, 1))
            print(row(f'{label}, control {c}', a))
big_stack(main)
