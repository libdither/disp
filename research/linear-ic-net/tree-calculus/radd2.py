"""Circulating reversible add, spare leaf arm kept in the unused fork-arm slot:
(PACK, d, m, n) -> (PACK, d, m, m + n) with PACK = △ (△ c d) c."""
import itertools, threading
from rules import L, A, size
from radd import comp, R, lam, tri, eager, nat
def build(order):
    nxt = {'dh': 'dh', 'a': A(L, 'a'), 'n': A(L, 'n')}
    D = comp(lam('o1', 'o2', 'u', *order, A(tri('o1', 'dh', 'o2', 'u'), *[nxt[v] for v in order])))
    C = comp(lam('o1', 'o2', *order, A(L, A(L, A(L, A(L, 'o2', 'o1'), 'o2'), 'dh'), A(L, 'a', 'n'))))
    pack = A(L, A(L, C, D), C)
    def call(m, n):
        init = {'dh': D, 'a': L, 'n': nat(n)}
        return A(pack, nat(m), *[init[v] for v in order])
    return C, D, pack, call
def main():
    best = None
    for order in itertools.permutations(('dh', 'a', 'n')):
        C, D, pack, call = build(order)
        v, st = eager(R)(call(30, 25))
        assert v == A(L, A(L, pack, D), A(L, nat(30), nat(55)))
        print(f"order {order}: steps {st['steps']}, |c| {size(C)}, |d| {size(D)}, W {st['W']}, K {st['K']}, copied {st['copied']}")
        if best is None or st['steps'] < best[0]: best = (st['steps'], order)
    print('best', best)
if __name__ == '__main__':
    threading.stack_size(512 * 1024 * 1024); th = threading.Thread(target=main); th.start(); th.join()
