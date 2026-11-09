"""Reversible add in the final LTC design (V5). Programs as λ-terms over LTC's own triage
(the chosen arm gets the two arms it beat, then x's children), compiled by family.py."""
import threading
from collections import Counter
from rules import L, A, size, spine, show
from family import DESIGNS, rules_of, make_compiler, wait_of
from family_test import eager, lower_k
from bench import programs, nat
import compile as compmod
NAME = 'V5 I K C | W B B2'
R = rules_of(DESIGNS[NAME]); comp = make_compiler(DESIGNS[NAME]); Kq = lambda q: A(L, L, A(L, q))
def lam(*a):
    *xs, body = a
    for x in reversed(xs): body = ('λ', x, body)
    return body
def tri(c, d, b, x): return A(L, A(L, c, d), b, x)

# 1. ordinary add, as in the benchmarks: self-application, arms drop what they beat (K)
compmod.WAIT['fam'] = wait_of(NAME)
ADD = comp(lower_k(programs('fam')[1], Kq))
def add_call(m, n): return A(ADD, ADD, nat(m), nat(n))

# 2. naive reversible add (m, n) -> (m, m + n): copy m, then ordinary add
NAIVE = comp(lam('add', 'm', 'n', A(L, 'm', A('add', 'add', 'm', 'n'))))
def naive_call(m, n): return A(NAIVE, ADD, nat(m), nat(n))

# 3. circulating reversible add: (pack, c, d, m, n) -> (pack, c, d, m, m + n), nothing dropped.
#    The arms it did not pick go into the next pack; only the arm it ran is copied (from a spare).
D_ = comp(lam('o1', 'o2', 'u', 'ch', 'dh', 'a', 'n',
              A(tri('o1', 'dh', 'o2', 'u'), 'ch', 'dh', A(L, 'a'), A(L, 'n'))))
C_ = comp(lam('o1', 'o2', 'ch', 'dh', 'a', 'n',
              A(L, A(L, A(L, A(L, 'ch', 'o1'), 'o2'), A(L, 'ch', 'dh')), A(L, 'a', 'n'))))
B_ = L
PACK = A(L, A(L, C_, D_), B_)
def radd_call(m, n): return A(PACK, nat(m), C_, D_, L, nat(n))
def radd_expect(m, n): return A(L, A(L, PACK, A(L, C_, D_)), A(L, nat(m), nat(m + n)))

def check_law(t, v, st):
    assert st['copied'] - st['dropped'] == 3 * st['steps'] + size(v) - size(t)

def main():
    print(f'code size: add {size(ADD)}, naive wrapper {size(NAIVE)}, circulating arms c {size(C_)} d {size(D_)}')
    print(f"{'m n':8} {'ordinary add':>34} {'naive (m, m+n)':>34} {'circulating (P, m, m+n)':>34}")
    for m, n in ((3, 2), (10, 5), (30, 25), (100, 7)):
        cells = []
        for t, want in ((add_call(m, n), nat(m + n)), (naive_call(m, n), A(L, nat(m), nat(m + n))),
                        (radd_call(m, n), radd_expect(m, n))):
            v, st = eager(R)(t)
            assert v == want, (m, n)
            check_law(t, v, st)
            cells.append(f"{st['steps']:>6} st {st['K']:>4} K {st['W']:>4} W {st['copied']:>6} cp {st['dropped']:>6} dr")
        print(f'{m:>3} {n:<4} ' + ' '.join(f'{c:>34}' for c in cells))
    print('every run: copied - dropped = 3*steps + |end| - |start|, exactly')
if __name__ == '__main__':
    threading.stack_size(512 * 1024 * 1024); th = threading.Thread(target=main); th.start(); th.join()
