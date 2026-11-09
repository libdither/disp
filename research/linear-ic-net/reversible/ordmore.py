"""Ordinary (K allowed) rotate and Gray code (3 states, state as a trit: △, △△, △△△) and XOR of two words."""
from common import *
from chainm import cellf
from chainm2 import MORE, DirectChain

def ord_machine3(delta, fin, w8):
    S = 'self'
    sv = {0: L, 1: A(L, L), 2: A(L, L, L)}
    def push(bits, acc):
        for b in bits: acc = cellf(b, acc)
        return acc
    def on_state(f):
        return tri(f(0), lam('_u', f(1)), lam('_p', '_q', f(2)), 'q')
    def step(b):
        return lam('rest', 'q', 'acc', 'r', A(on_state(lambda q: lam('rest', 'acc', 'r',
                   A('r', 'rest', sv[delta(q, b)[1]], push(delta(q, b)[0], 'acc')))), 'rest', 'acc', 'r'))
    leaf = lam('q', 'acc', 'r', A(on_state(lambda q: lam('acc', push(fin(q), 'acc'))), 'acc'))
    fork = lam('_f', 'rest', 'q', 'acc', 'r', A(step(1), 'rest', 'q', 'acc', 'r'))
    return lam(S, 'c', 'q', 'acc', A(tri(leaf, step(0), fork, 'c'), 'q', 'acc', w8(S, S)))

def ord_xor(w8):
    """xor a b acc: walks both chains, pushes (a-cell, a^b cell) like the reversible two-word machine"""
    S = 'self'
    def bcase(a):
        def arm(bb):
            out = A(L, L, A(L, 'acc')) if (a ^ bb) else A(L, A(L, 'acc'))      # placeholder, rebuilt below
            ca = (lambda x: A(L, L, x)) if a else (lambda x: A(L, x))
            cs = (lambda x: A(L, L, x)) if (a ^ bb) else (lambda x: A(L, x))
            return cs(ca('acc'))
        return lam('b', 'arest', 'acc', 'r', A(tri(lam('arest', 'acc', 'r', 'acc'),
                                                   lam('brest', 'arest', 'acc', 'r', A('r', 'arest', 'brest', arm(0))),
                                                   lam('_f', 'brest', 'arest', 'acc', 'r', A('r', 'arest', 'brest', arm(1))), 'b'),
                                               'arest', 'acc', 'r'))
    a0 = lam('arest', 'b', 'acc', 'r', A(bcase(0), 'b', 'arest', 'acc', 'r'))
    a1 = lam('_f', 'arest', 'b', 'acc', 'r', A(bcase(1), 'b', 'arest', 'acc', 'r'))
    return lam(S, 'a', 'b', 'acc', A(tri(lam('b', 'acc', 'r', 'acc'), a0, a1, 'a'), 'b', 'acc', w8(S, S)))

def main():
    rng = random.Random(3)
    print(HDR)
    for name, (n, d, fi) in MORE.items():
        for calc, P in (('v5', ord_ltc(ord_machine3(d, fi, W8_LTC))), ('tc', ord_tc(ord_machine3(d, fi, W8_TC)))):
            tot = {}
            for w in (16, 32):
                s = F.Stats()
                for _ in range(30):
                    bits = [rng.randrange(2) for _ in range(w)]
                    t = L
                    for b in reversed(bits): t = cellf(b, t)
                    q, acc = 0, L
                    for b in bits:
                        out, q = d(q, b)
                        for o in out: acc = cellf(o, acc)
                    for o in fi(q): acc = cellf(o, acc)
                    v, st = run(A(P, P, t, L, L), calc); assert v == V(acc), (name, calc); s = s + st
                tot[w] = s
            pb = (tot[32].steps - tot[16].steps) / 30 / 16; a = F.Stats()
            for k in a.__slots__: setattr(a, k, round(getattr(tot[32], k) / 30))
            print(row(f'ordinary {name} {calc} w=32', a, f'marginal {pb:.1f} steps/bit (reversed output)'))
    for calc, P in (('v5', ord_ltc(ord_xor(W8_LTC))), ('tc', ord_tc(ord_xor(W8_TC)))):
        tot = {}
        for w in (16, 32):
            s = F.Stats()
            for _ in range(30):
                x, y = rng.randrange(1 << w), rng.randrange(1 << w)
                ca = L; cb = L
                for i in reversed(range(w)): ca = cellf((x >> i) & 1, ca); cb = cellf((y >> i) & 1, cb)
                acc = L
                for i in range(w): acc = cellf(((x ^ y) >> i) & 1, cellf((x >> i) & 1, acc))
                v, st = run(A(P, P, ca, cb, L), calc); assert v == V(acc), calc; s = s + st
            tot[w] = s
        pb = (tot[32].steps - tot[16].steps) / 30 / 16; a = F.Stats()
        for k in a.__slots__: setattr(a, k, round(getattr(tot[32], k) / 30))
        print(row(f'ordinary XOR words {calc} w=32', a, f'marginal {pb:.1f} steps/bit'))
big_stack(main)
