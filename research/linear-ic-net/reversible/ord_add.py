"""Ordinary (K allowed) binary add, a + b mod 2^w, words LSB-first lists of bits; LTC vs tree calculus."""
from common import *

def full_adder():
    """FA ai bi c k = k s c' by triage on ai, bi, c (bits: leaf = 0, stem = 1); arms are closed terms"""
    def leaf(s, c): return lam('k', A('k', bit(s), bit(c)))
    def by_c(sc0, sc1):
        return lam('c', A(tri(leaf(*sc0), lam('_u', leaf(*sc1)), lam('_p', '_q', leaf(0, 0)), 'c')))
    def by_b(z, o):
        return lam('bi', A(tri(z, lam('_u', o), lam('_p', '_q', z), 'bi')))
    body = A(tri(by_b(by_c((0, 0), (1, 0)), by_c((1, 0), (0, 1))),
                 lam('_u', by_b(by_c((1, 0), (0, 1)), by_c((0, 1), (1, 1)))),
                 lam('_p', '_q', L), 'ai'), 'bi', 'c', 'k')
    return lam('ai', 'bi', 'c', 'k', body)

def badd(w8):
    S = 'self'
    FA = full_adder()
    bcons = lam('bi', 'rb', 'ai', 'ra', 'c', 'r',
                A(FA, 'ai', 'bi', 'c', lam('s', 'c2', A(L, 's', A('r', 'ra', 'rb', 'c2')))))
    acons = lam('ai', 'ra', 'b', 'c', 'r',
                A(tri(lam('ai', 'ra', 'c', 'r', L), lam('_u', 'ai', 'ra', 'c', 'r', L), bcons, 'b'),
                  'ai', 'ra', 'c', 'r'))
    body = A(tri(lam('b', 'c', 'r', L), lam('_u', 'b', 'c', 'r', L), acons, 'a'), 'b', 'c', w8(S, S))
    return lam(S, 'a', 'b', 'c', body)

def unword(t):
    """prototype list of bits -> int"""
    n, i = 0, 0
    while t != L:
        (_, b), t = t[0], t[1]
        n |= (1 if b != L else 0) << i; i += 1
    return n

def main():
    P_ltc = ord_ltc(badd(W8_LTC)); P_tc = ord_tc(badd(W8_TC))
    print(f'code: LTC {size(P_ltc)} △, TC {size(P_tc)} △')
    rng = random.Random(1)
    print(HDR)
    for w in (4, 8, 16, 32):
        tot = {'v5': F.Stats(), 'tc': F.Stats()}; n = 0
        cases = [(rng.randrange(1 << w), rng.randrange(1 << w)) for _ in range(20)]
        if w == 4: cases = [(x, y) for x in range(16) for y in range(16)]
        for x, y in cases:
            for calc, P in (('v5', P_ltc), ('tc', P_tc)):
                v, st = run(A(P, P, word(x, w), word(y, w), B0), calc)
                assert unword(F.term(v)) == (x + y) % (1 << w), (calc, x, y)
                tot[calc] = tot[calc] + st
            n += 1
        for calc in ('v5', 'tc'):
            s = tot[calc]
            avg = F.Stats()
            for k in avg.__slots__: setattr(avg, k, round(getattr(s, k) / n))
            print(row(f'ordinary add w={w} {calc} (avg of {n})', avg, f'{avg.steps / w:.1f} steps/bit'))
if __name__ == "__main__": big_stack(main)
