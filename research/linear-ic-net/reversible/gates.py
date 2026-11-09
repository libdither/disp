"""Single-shot gates and word routing. Reversible versions hand the program back: end term △ OUT PROGRAM.
Bits: 0 = △, 1 = △△ (single bits) or chain cells (words). Ordinary versions: plain λ-terms, K allowed."""
import sys, itertools
from common import *
from chainm2 import DirectChain, cellf
from twochain import TwoChain

def frame(c, d, b): return A(L, A(L, c, d), b)
B0, B1 = L, A(L, L)
def bitv(b): return B1 if b else B0

# ---- controlled swap of two arbitrary trees by a bit c (Fredkin on words): O(1) ----
# P = △ (△ A0 A1) X0; A_i = △ (△ sp_i Y_i) M_i triages its sibling (a fork) so M_i gets its own spare first.
def cswap_M(i):
    if i == 0:   # c = 0: M0 sp0 Y0 A1l A1r X0 x y
        args = ['sp', 'Y', 'Sl', 'Sr', 'X', 'x', 'y']
        me = A(L, A(L, 'sp', 'Y'), 'sp'); sib = A(L, 'Sl', 'Sr')
        prog = frame(me, sib, 'X'); out = A(L, L, A(L, 'x', 'y'))
    else:        # c = 1: M1 sp1 Y1 A0l A0r X0 u x y
        args = ['sp', 'Y', 'Sl', 'Sr', 'X', 'u', 'x', 'y']
        me = A(L, A(L, 'sp', 'Y'), 'sp'); sib = A(L, 'Sl', 'Sr')
        prog = frame(sib, me, 'X'); out = A(L, A(L, 'u'), A(L, 'y', 'x'))
    return comp(lam(*args, A(L, out, prog)))
def cswap_prog():
    m0, m1 = cswap_M(0), cswap_M(1)
    return frame(A(L, A(L, m0, L), m0), A(L, A(L, m1, L), m1), L)

# ---- swap two trees: P = C M sp, P x -> M x sp ----
def swap_prog():
    M = comp(lam('x', 'sp', 'y', A(L, A(L, 'y', 'x'), A(L, L, A(L, 'sp', 'sp')))))
    return A(L, L, A(L, M, M))

def ordinary_gates():
    """λ-terms with tree-calculus triage (arms drop what they beat after lowering)"""
    on = lambda x, z, o: tri(z, lam('_u', o), lam('_p', '_q', L), x)
    NOT = lam('x', on('x', B1, B0))
    CNOT = lam('c', 't', A(on('c', lam('t', A(L, B0, 't')), lam('t', A(L, B1, on('t', B1, B0)))), 't'))
    TOF = lam('a', 'b', 'c', A(on('a', lam('b', 'c', A(L, B0, A(L, 'b', 'c'))),
                                 lam('b', 'c', A(on('b', lam('c', A(L, B1, A(L, B0, 'c'))),
                                                         lam('c', A(L, B1, A(L, B1, on('c', B1, B0))))), 'c'))), 'b', 'c'))
    FRED = lam('c', 'x', 'y', A(on('c', lam('x', 'y', A(L, B0, A(L, 'x', 'y'))), lam('x', 'y', A(L, B1, A(L, 'y', 'x')))), 'x', 'y'))
    SWAP = lam('x', 'y', A(L, 'y', 'x'))
    return dict(NOT=NOT, CNOT=CNOT, TOF=TOF, FRED=FRED, SWAP=SWAP)

def main():
    rng = random.Random(9)
    print(HDR)
    # ordinary single-shot gates
    og = ordinary_gates()
    cases = {'NOT': [(b,) for b in (0, 1)], 'CNOT': list(itertools.product((0, 1), repeat=2)),
             'TOF': list(itertools.product((0, 1), repeat=3)), 'FRED': list(itertools.product((0, 1), repeat=3)),
             'SWAP': list(itertools.product((0, 1), repeat=2))}
    def ref(g, xs):
        if g == 'NOT': return [1 - xs[0]]
        if g == 'CNOT': return [xs[0], xs[0] ^ xs[1]]
        if g == 'TOF': return [xs[0], xs[1], xs[2] ^ (xs[0] & xs[1])]
        if g == 'FRED': return [xs[0]] + ([xs[2], xs[1]] if xs[0] else [xs[1], xs[2]])
        if g == 'SWAP': return [xs[1], xs[0]]
    def tup(bs):
        t = bitv(bs[-1])
        for b in reversed(bs[:-1]): t = A(L, bitv(b), t)
        return t
    for g, t in og.items():
        for calc, P in (('v5', ord_ltc(t)), ('tc', ord_tc(t))):
            s = F.Stats()
            for xs in cases[g]:
                v, st = run(A(P, *[bitv(b) for b in xs]), calc)
                assert v == V(tup(ref(g, xs))), (g, calc, xs); s = s + st
            n = len(cases[g]); avg = F.Stats()
            for k in avg.__slots__: setattr(avg, k, round(getattr(s, k) / n, 1))
            print(row(f'ordinary {g} on bits, {calc}', avg))
    # reversible: Fredkin on words (and bits), swap
    P = cswap_prog(); Ps = swap_prog()
    for label, mk in (('rev Fredkin (controlled swap) bits', lambda: (rng.randrange(2), bitv(rng.randrange(2)), bitv(rng.randrange(2)))),
                      ('rev controlled swap of 32-bit words', lambda: (rng.randrange(2), cellword(rng, 32), cellword(rng, 32)))):
        s = F.Stats(); n = 40
        for _ in range(n):
            c, x, y = mk()
            v, st = run(A(P, bitv(c), x, y))
            want = A(L, A(L, bitv(c), A(L, y, x) if c else A(L, x, y)), P)
            assert v == V(want) and st.K == 0; s = s + st
        avg = F.Stats()
        for k in avg.__slots__: setattr(avg, k, round(getattr(s, k) / n, 1))
        print(row(label, avg))
    s = F.Stats()
    for x, y in ((B0, B1), (cellword(rng, 32), cellword(rng, 32))):
        v, st = run(A(Ps, x, y)); assert v == V(A(L, A(L, y, x), Ps)) and st.K == 0; s = st
    print(row('rev swap of two trees', s))
    # reversible gates as tiny machines over a bit word (direct style, program handed back)
    tof = DirectChain(4, lambda q, b: {0: ([b], 1 if b else 3), 1: ([b], 2 if b else 3), 2: ([1 - b], 3), 3: ([b], 3)}[q],
                      lambda q: []).build()
    notm = DirectChain(1, lambda q, b: ([1 - b], 0), lambda q: []).build()
    cnot = DirectChain(3, lambda q, b: {0: ([b], 1 if b else 2), 1: ([1 - b], 2), 2: ([b], 2)}[q], lambda q: []).build()
    fred = DirectChain(4, lambda q, b: {0: ([b], 1 if b else 3), 3: ([b], 3)}.get(q, ([b], q)), lambda q: []).build()
    for label, Mc, k in (('rev NOT, 1-bit word machine', notm, 1), ('rev CNOT, 2-bit word machine', cnot, 2),
                         ('rev Toffoli, 3-bit word machine', tof, 3)):
        s = F.Stats(); combos = list(itertools.product((0, 1), repeat=k))
        for xs in combos:
            v, st = run(Mc.start(list(xs))); assert v == V(Mc.expect(list(xs))) and st.K == 0; s = s + st
        avg = F.Stats()
        for kk in avg.__slots__: setattr(avg, kk, round(getattr(s, kk) / len(combos), 1))
        print(row(label, avg))
    tc = TwoChain(lambda a, b: (a, a ^ b)).build()
    s = F.Stats()
    for x in (0, 1):
        for y in (0, 1):
            v, st = run(tc.start(x, y, 1)); assert v == V(tc.expect(x, y, 1)) and st.K == 0; s = s + st
    avg = F.Stats()
    for kk in avg.__slots__: setattr(avg, kk, round(getattr(s, kk) / 4, 1))
    print(row('rev CNOT, two 1-bit words (XOR machine)', avg))

def cellword(rng, w):
    t = L
    for _ in range(w): t = cellf(rng.randrange(2), t)
    return t
if __name__ == '__main__':
    big_stack(main)
