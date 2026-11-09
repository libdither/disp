"""Which order of the four context arguments (ch, dh, a, n) makes the circulating reversible add cheapest?"""
import itertools, threading
from rules import L, A, size
from radd import comp, R, lam, tri, eager, nat
def build(order):
    # d: λ o1 o2 u <order>. pack(o1, dh, o2) u <order with dh, △a, △n>
    nxt = {'ch': 'ch', 'dh': 'dh', 'a': A(L, 'a'), 'n': A(L, 'n')}
    D = comp(lam('o1', 'o2', 'u', *order, A(tri('o1', 'dh', 'o2', 'u'), *[nxt[v] for v in order])))
    C = comp(lam('o1', 'o2', *order, A(L, A(L, A(L, A(L, 'ch', 'o1'), 'o2'), A(L, 'ch', 'dh')), A(L, 'a', 'n'))))
    pack = A(L, A(L, C, D), L)
    init = {'ch': C, 'dh': D, 'a': L}
    return C, D, pack, lambda m, n: A(pack, nat(m), *[init.get(v, nat(n)) if v != 'n' else nat(n) for v in order])
def main():
    rows = []
    for order in itertools.permutations(('ch', 'dh', 'a', 'n')):
        C, D, pack, call = build(order)
        v, st = eager(R)(call(30, 25))
        assert v == A(L, A(L, pack, A(L, C, D)), A(L, nat(30), nat(55)))
        rows.append((st['steps'], size(D), size(C), st['copied'], order))
    rows.sort()
    for r in rows[:4] + rows[-2:]: print(f'steps {r[0]:5}  |d| {r[1]:4}  |c| {r[2]:4}  copied {r[3]:6}  order {r[4]}')
threading.stack_size(512 * 1024 * 1024); th = threading.Thread(target=main); th.start(); th.join()
