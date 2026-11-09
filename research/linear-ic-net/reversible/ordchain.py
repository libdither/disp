"""Ordinary (K allowed) versions of the chain programs, same encoding and same function as the reversible
machine (tail recursive, output pushed onto an accumulator, so reversed). The state (if any) is a data bit
(△ = 0, △△ = 1) triaged each step; recursion is self-application with the prototype's wait."""
from common import *
from chainm import PROGRAMS, cellf

def ord_machine(n, delta, fin, w8):
    S = 'self'
    def push(bits, acc):
        for b in bits: acc = cellf(b, acc)
        return acc
    def stateval(q): return A(L, L) if q else L
    def on_state(f):     # f(q) -> term; triage the state bit when there are two states
        if n == 1: return f(0)
        return tri(f(0), lam('_u', f(1)), lam('_p', '_q', L), 'q')
    def step(b):
        # args: rest q acc r
        return lam('rest', 'q', 'acc', 'r', A(on_state(lambda q: lam('rest', 'acc', 'r',
                   A('r', 'rest', stateval(delta(q, b)[1]), push(delta(q, b)[0], 'acc')))), 'rest', 'acc', 'r'))
    leaf = lam('q', 'acc', 'r', A(on_state(lambda q: lam('acc', push(fin(q), 'acc'))), 'acc'))
    stem = step(0)
    fork = lam('_f', 'rest', 'q', 'acc', 'r', A(step(1), 'rest', 'q', 'acc', 'r'))
    body = A(tri(leaf, stem, fork, 'c'), 'q', 'acc', w8(S, S))
    return lam(S, 'c', 'q', 'acc', body)

def main():
    rng = random.Random(3)
    print(HDR)
    for name, (n, d, fi) in PROGRAMS.items():
        for calc, P in (('v5', ord_ltc(ord_machine(n, d, fi, W8_LTC))), ('tc', ord_tc(ord_machine(n, d, fi, W8_TC)))):
            tot = {}
            for w in (16, 32):
                s = F.Stats()
                for _ in range(40):
                    bits = [rng.randrange(2) for _ in range(w)]
                    t = L
                    for b in reversed(bits): t = cellf(b, t)
                    q, acc = 0, L
                    for b in bits:
                        out, q = d(q, b)
                        for o in out: acc = cellf(o, acc)
                    for o in fi(q): acc = cellf(o, acc)
                    v, st = run(A(P, P, t, L, L), calc)
                    assert v == V(acc), (name, calc)
                    s = s + st
                tot[w] = s
            pb = (tot[32].steps - tot[16].steps) / 40 / 16
            a = F.Stats()
            for k in a.__slots__: setattr(a, k, round(getattr(tot[32], k) / 40))
            print(row(f'ordinary {name} {calc} w=32', a, f'marginal {pb:.1f} steps/bit, code {size(P)}'))
big_stack(main)
