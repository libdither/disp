"""Stateless two-word machine (the adder without a carry): words a, b as chains (bit 0 = △ rest, bit 1 = △ △ rest),
a ends in △△, b in △. A-frame △ (△ Bchain Z) O triages the a-cell, the picked B-frame triages Bchain, the picked
leaf frame △ (△ M sp) Y triages E = △ and runs M. out(a, b) -> (a', b'), pushed onto one accumulator (reversed,
interleaved: a'-cell then b'-cell). End: pieces fall into the Y slot of Z's b = 1 leaf frame."""
import sys
from common import *
from addx4 import (frame, cell, chain, Cf, lam_P1, lam_B, lam_C, best_found, subst_K)

class TwoChain:
    def __init__(self, outf, use_found=True):
        self.outf, self.use_found = outf, use_found
    def stage(self, job, lt):
        t = best_found(job) if self.use_found else None
        return t if t is not None else comp(lt)
    def M(self, a, b):
        x, y = self.outf(a, b)
        P1 = self.stage('P1', lam_P1())
        Bb = self.stage('B0' if b == 0 else 'B1c', lam_B(b))
        Ca = self.stage('Ka' if a == 0 else 'C1c', lam_C(a))
        job = {(0, 0): 'D00', (0, 1): 'D01', (1, 0): 'D10', (1, 1): 'D11'}.get((x, y))
        Dl = lam('nxt', 'arest', 'acc', A('nxt', 'arest', Cf[y](Cf[x]('acc'))))
        D = self.stage(job, Dl) if x == a else comp(Dl)       # the D logs assume x = a (a kept)
        return subst_K(P1, subst_K(Bb, subst_K(Ca, D)))
    def end_handler(self):
        args = ['M1', 'sp1', 'M0', 'sp0', 'Y0', 'PO', 'lo1', 'u', 'acc']
        lf0 = A(L, A(L, 'M0', 'sp0'), 'Y0'); lf1 = A(L, A(L, 'M1', 'sp1'), 'Y0')
        Z = frame(L, lf0, lf1); O = A(L, 'PO', 'lo1')
        return comp(lam(*args, A('u', A(L, Z, O), 'acc')))
    def build(self):
        k = {(a, b): self.M(a, b) for a in (0, 1) for b in (0, 1)}
        eh = self.end_handler(); self.k = k
        lf = lambda key, Y: A(L, A(L, k[key], k[key]), Y)
        self.Z = frame(L, lf((0, 0), eh), lf((0, 1), eh))
        self.O = frame(L, lf((1, 0), L), lf((1, 1), L))
        return self
    def program(self): return A(L, self.Z, self.O)
    def start(self, x, y, w):
        return A(frame(chain(y, w, L), self.Z, self.O), chain(x, w, A(L, L)), L)
    def expect(self, x, y, w):
        acc = L
        for i in range(w):
            p, q = self.outf((x >> i) & 1, (y >> i) & 1)
            acc = cell(q, cell(p, acc))
        return A(L, self.program(), acc)
    def sizes(self): return {k: size(v) for k, v in self.k.items()}

def report(D, label):
    rng = random.Random(5)
    for w in (1, 2, 3, 4):
        for x in range(1 << w):
            for y in range(1 << w):
                v, st = run(D.start(x, y, w)); assert v == V(D.expect(x, y, w)), (label, w, x, y); assert st.K == 0
    tot = {}
    for w in (16, 32):
        s = F.Stats()
        for _ in range(30):
            x, y = rng.randrange(1 << w), rng.randrange(1 << w)
            v, st = run(D.start(x, y, w)); assert v == V(D.expect(x, y, w)); assert st.K == 0
            s = s + st
        tot[w] = s
    pb = (tot[32].steps - tot[16].steps) / 30 / 16; s = tot[32]; n = 30
    print(f'{label:28} exhaustive w<=4 ok | w=32 avg {s.steps / n:6.0f} steps {s.K / n:.0f} K {s.W / n:3.0f} W '
          f'{s.copied / n:6.0f} copied {s.dropped / n:.0f} dropped | marginal {pb:5.1f} steps/bit | M {D.sizes()}', flush=True)
if __name__ == '__main__':
    def main():
        report(TwoChain(lambda a, b: (a, a ^ b)).build(), 'XOR words (CNOT per bit)')
        report(TwoChain(lambda a, b: (a, b)).build(), 'zip copy (identity)')
    big_stack(main)
