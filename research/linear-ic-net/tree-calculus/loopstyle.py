"""Ordinary (irreversible) loops two ways: self-application (wait self self) vs carrying a spare arm
and reusing the arms triage hands back (drops only at the end)."""
import itertools, threading
from rules import L, A, size
from radd import comp, R, lam, tri, eager, nat, add_call
from bench import lst
def main():
    v, st = eager(R)(add_call(30, 25))
    print(f"add 30 25, self-application: {st['steps']} steps, {st['K']} K, {st['W']} W, {st['copied']} copied, {st['dropped']} dropped")
    for order in itertools.permutations(('n', 'dh')):
        nxt = {'n': A(L, 'n'), 'dh': 'dh'}
        D = comp(lam('o1', 'o2', 'u', *order, A(tri('o1', 'dh', 'o2', 'u'), *[nxt[x] for x in order])))
        C = comp(lam('o1', 'o2', *order, 'n'))
        pack = A(L, A(L, C, D), L)
        init = {'n': None, 'dh': D}
        for m, n in ((30, 25), (100, 7)):
            call = A(pack, nat(m), *[nat(n) if x == 'n' else D for x in order])
            v, st = eager(R)(call); assert v == nat(m + n)
            print(f"add {m} {n}, carried spare, order {order}: {st['steps']} steps, {st['K']} K, {st['W']} W, "
                  f"{st['copied']} copied, {st['dropped']} dropped; |d| = {size(D)}")
    v, st = eager(R)(add_call(100, 7))
    print(f"add 100 7, self-application: {st['steps']} steps, {st['K']} K, {st['W']} W, {st['copied']} copied")
threading.stack_size(512 * 1024 * 1024); th = threading.Thread(target=main); th.start(); th.join()
