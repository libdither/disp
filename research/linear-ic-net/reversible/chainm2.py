"""Direct-style (order-preserving) reversible chain machine. Same frames as chainm.py, but no accumulator:
an arm calls the next frame inside a wrap  W = △ (△ △ △) ARM  that is applied to the pair △ OUT P the rest of
the run returns; the wrap's triage hands ARM its two leaves, which become the new cell's and the pair's roots.
ARM0 = B2 △ △ △ (2 steps): c d out P -> △ (△ out) P.  End term: △ OUT PROGRAM (output first)."""
import sys
from common import *
from addx4 import best_found, subst_K, lam_P1
from chainm import frame, cellf, E, E_CHILD, PROGRAMS

ARM = {0: lam('c', 'd', 'out', A('c', A('d', 'out'))),            # △ (△ out) [then P]
       1: lam('c', 'd', 'out', A('c', A('d', L, 'out')))}         # △ (△ △ out)
def WRAP(bits):
    """a value W with W (△ out P) = △ (cells out) P; bits pushed in order (first = innermost)"""
    if len(bits) == 1:
        return A(L, A(L, L, L), comp(ARM[bits[0]]))
    # general: one wrap per bit, composed (innermost first)
    w = None
    for b in bits:
        wb = A(L, A(L, L, L), comp(ARM[b]))
        w = wb if w is None else comp(lam('r', A(wb, A(w, 'r'))))
    return w

class DirectChain:
    def __init__(self, nstates, delta, fin, use_found=True, name=''):
        self.n, self.delta, self.fin, self.use_found, self.name = nstates, delta, fin, use_found, name
        self.trail = [f'F{i}' for i in range(nstates - 1)]       # the other states' frames, canonical order
    def stage(self, job, lt):
        t = best_found(job) if self.use_found else None
        return t if t is not None else comp(lt)
    def others(self, q): return [s for s in range(self.n) if s != q]
    def M(self, q, b):
        out, q2 = self.delta(q, b)
        P1 = self.stage('P1', lam_P1())
        Eq = A('f', 'e') if b else A(L, 'e')
        own = frame(Eq, 'lf', 'sib') if b == 0 else frame(Eq, 'sib', 'lf')
        head = ['lf', 'e', 'sib'] + (['f'] if b else []) + ['rest']
        names = dict(zip(self.others(q), self.trail))            # state -> trailing name, as seen in state q
        if q2 == q:
            nxt = own; new = {s: names[s] for s in self.others(q)}
        else:
            nxt = names[q2]; new = {s: (own if s == q else names[s]) for s in self.others(q2)}
        call = A(nxt, 'rest', *[new[s] for s in self.others(q2)])
        body = call if not out else A(WRAP(out), call)
        if q2 == q and self.n == 1 and len(out) == 1:
            G = self.stage(f'G{b}', lam('lf', 'e', 'sib', *(['f'] if b else []), A('K', own)))
            D = A(L, A(L, A(L, WRAP(out))))                       # B-partial: nxt -> B W nxt
            return subst_K(P1, subst_K(G, D))
        return subst_K(P1, comp(lam(*head, *self.trail, body)))
    def end_handler(self, q):
        args = ['sp0', 'M0', 'l1', 'l2', 'd', 'PL1', 'Y1'] + self.trail
        e = A(L, A(L, 'l1', 'l2'), 'd')
        lf0 = A(L, A(L, 'sp0', 'M0'), 'Y1'); lf1 = A(L, 'PL1', 'Y1')
        Fq = frame(A(L, e), lf0, lf1)
        names = dict(zip(self.others(q), self.trail))
        Fs = [Fq if s == q else names[s] for s in range(self.n)]
        prog = Fs[0]
        for f in Fs[1:]: prog = A(L, prog, f)
        out = L
        for o in self.fin(q): out = cellf(o, out)
        return comp(lam(*args, A(L, out, prog)))
    def build(self):
        k = {(q, b): self.M(q, b) for q in range(self.n) for b in (0, 1)}
        eh = {q: self.end_handler(q) for q in range(self.n)}
        self.k = k
        self.F = {q: frame(E, A(L, A(L, k[(q, 0)], k[(q, 0)]), eh[q]), A(L, A(L, k[(q, 1)], k[(q, 1)]), eh[q]))
                  for q in range(self.n)}
        return self
    def program(self):
        p = self.F[0]
        for q in range(1, self.n): p = A(L, p, self.F[q])
        return p
    def start(self, bits, q0=0):
        t = L
        for b in reversed(bits): t = cellf(b, t)
        return A(self.F[q0], t, *[self.F[s] for s in self.others(q0)])
    def outbits(self, bits, q0=0):
        q, o = q0, []
        for b in bits:
            out, q = self.delta(q, b); o += out
        return o + self.fin(q)
    def expect(self, bits, q0=0):
        o = self.outbits(bits, q0); t = L
        for b in reversed(o): t = cellf(b, t)
        return A(L, t, self.program())
    def sizes(self): return {k: size(v) for k, v in self.k.items()}

def measure(Mc, label, ntrial=40):
    rng = random.Random(3)
    for w in range(0, 7):
        for x in range(1 << w):
            bits = [(x >> i) & 1 for i in range(w)]
            v, st = run(Mc.start(bits)); assert v == V(Mc.expect(bits)), (label, bits); assert st.K == 0
    tot = {}
    for w in (16, 32):
        s = F.Stats()
        for _ in range(ntrial):
            bits = [rng.randrange(2) for _ in range(w)]
            v, st = run(Mc.start(bits)); assert v == V(Mc.expect(bits)); assert st.K == 0
            s = s + st
        tot[w] = s
    pb = (tot[32].steps - tot[16].steps) / ntrial / 16
    s = tot[32]; n = ntrial
    print(f'{label:26} exhaustive w<=6 ok | w=32 avg {s.steps / n:6.0f} steps {s.K / n:.0f} K {s.W / n:3.0f} W '
          f'{s.copied / n:6.0f} copied {s.dropped / n:.0f} dropped | marginal {pb:5.1f} steps/bit | M {Mc.sizes()}', flush=True)
    return pb, s

MORE = {
    'rotate (first to end)': (3, lambda q, b: ([], 1 + b) if q == 0 else ([b], q), lambda q: [] if q == 0 else [q - 1]),
    'Gray code': (3, lambda q, b: ([], 1 + b) if q == 0 else ([(q - 1) ^ b], 1 + b), lambda q: [] if q == 0 else [q - 1]),
}
if __name__ == '__main__':
    def main():
        for name, (n, d, fi) in list(PROGRAMS.items()) + list(MORE.items()):
            measure(DirectChain(n, d, fi, name=name).build(), name + ' (direct)')
    big_stack(main)
