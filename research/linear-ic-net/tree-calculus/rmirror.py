"""Reversible mirror, (P, t) -> (P, mirror t): the program is threaded through the run; the second
branch of a fork reuses the program the first branch handed back. Spares cs, ds, bs: one copy of each arm."""
import threading
from rules import L, A, size
from radd import comp, R, lam, tri, eager, Kq, lower_k
from family import wait_of
from bench import programs, tree
import compile as compmod
from rev_study import run, undo
c = lam('o1', 'o2', 'cs', 'ds', 'bs', 'k', A('k', A(L, A(L, 'cs', 'o1'), 'o2'), 'cs', 'ds', 'bs', L))
d = lam('o1', 'o2', 'u', 'cs', 'ds', 'bs', 'k',
        A(tri('o1', 'ds', 'o2', 'u'), 'cs', 'ds', 'bs',
          lam('P', 'c1', 'd1', 'b1', 'r', A('k', 'P', 'c1', 'd1', 'b1', A(L, 'r')))))
b = lam('o1', 'o2', 'u', 'v', 'cs', 'ds', 'bs', 'k',
        A(tri('o1', 'o2', 'bs', 'u'), 'cs', 'ds', 'bs',
          lam('P', 'c1', 'd1', 'b1', 'ru',
              A('P', 'v', 'c1', 'd1', 'b1',
                lam('P2', 'c2', 'd2', 'b2', 'rv', A('k', 'P2', 'c2', 'd2', 'b2', A(L, 'rv', 'ru')))))))
C, D, B = comp(c), comp(d), comp(b)
PACK = A(L, A(L, C, D), B)
K0 = comp(lam('P', 'c1', 'd1', 'b1', 'r', A(L, A(L, 'P', A(L, 'c1', A(L, 'd1', 'b1'))), 'r')))
def mir(t):
    if t == L: return L
    if t[0] == L: return A(L, mir(t[1]))
    return A(L, mir(t[1]), mir(t[0][1]))
def main():
    compmod.WAIT['fam'] = wait_of('V5 I K C | W B B2')
    MIRROR = comp(lower_k(programs('fam')[0], Kq))
    print(f'code: ordinary mirror {size(MIRROR)}; reversible arms c {size(C)} d {size(D)} b {size(B)}')
    for depth in (3, 5, 7):
        t = tree(depth)
        v1, s1 = eager(R)(A(MIRROR, MIRROR, t)); assert v1 == mir(t)
        t0 = A(PACK, t, C, D, B, K0)
        v2, s2 = eager(R)(t0)
        assert v2 == A(L, A(L, PACK, A(L, C, A(L, D, B))), mir(t)) and s2['K'] == 0
        print(f"depth {depth} ({size(t)} nodes): ordinary {s1['steps']} steps {s1['K']} K {s1['W']} W "
              f"{s1['copied']} copied {s1['dropped']} dropped | reversible {s2['steps']} steps 0 K {s2['W']} W "
              f"{s2['copied']} copied, program handed back")
    t0 = A(PACK, tree(4), C, D, B, K0); t1, h = run(t0); u = t1
    for p, nm in reversed(h): u = undo(u, p, nm)
    assert u == t0
    print(f'normal order, depth 4: {len(h)} steps, undone exactly from its footprints')
if __name__ == '__main__':
    threading.stack_size(512 * 1024 * 1024); th = threading.Thread(target=main); th.start(); th.join()
