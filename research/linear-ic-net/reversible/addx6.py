"""X6: X4 with the program as two "A-frame builders" H_c = C (B △ (C △ Z_c)) O_c (H_c x = △ (△ x Z_c) O_c).
The machine runs  A-frame arest [trailing]  where trailing holds acc and H_o (the inactive state) in a chosen
order. Start: H_0 Bchain Achain [trailing]; end: △ (△ H_0 H_1) acc."""
import sys
from common import *
from addx4 import frame, maj, cell, chain, Cf, lam_P1, lam_B, lam_C, lam_D, best_found, subst_K
def Hform(Z, O): return A(L, L, A(L, A(L, A(L, A(L, L)), A(L, L, A(L, L, Z))), O))

class Adder6:
    def __init__(self, state_first=True, use_found=True):
        self.sf, self.use_found = state_first, use_found
        self.trail = ['Ho', 'acc'] if state_first else ['acc', 'Ho']
    def stage(self, job, lt):
        t = best_found(job) if self.use_found else None
        return t if t is not None else comp(lt)
    def M(self, c, a, b):
        s = a ^ b ^ c; c2 = maj(a, b, c)
        P1 = self.stage('P1', lam_P1())
        Bb = self.stage('B0' if b == 0 else 'B1c', lam_B(b))
        if c2 == c:
            Ca = self.stage('Ka' if a == 0 else 'C1c', lam_C(a))
            if self.sf:
                D = self.stage(f'DS{a}{s}', lam('nxt', 'arest', 'Ho', 'acc', A('nxt', 'arest', 'Ho', Cf[s](Cf[a]('acc')))))
            else:
                D = self.stage({(0, 0): 'D00', (0, 1): 'D01', (1, 0): 'D10', (1, 1): 'D11'}[(a, s)], lam_D(a, s))
            return subst_K(P1, subst_K(Bb, subst_K(Ca, D)))
        head = ['own', 'brest', 'other'] + (['af'] if a else []) + ['arest']
        Zc, Oc = ('own', 'other') if a == 0 else ('other', 'own')
        accq = Cf[s](A(L, 'af', 'acc') if a else Cf[0]('acc'))
        env = {'acc': accq, 'Ho': Hform(Zc, Oc)}
        body = A('Ho', 'brest', 'arest', *[env[v] for v in self.trail])
        return subst_K(P1, subst_K(Bb, comp(lam(*head, *self.trail, body))))
    def end_handler(self, c):
        args = ['M1', 'sp1', 'M0', 'sp0', 'Y0', 'PO', 'lo1', 'u'] + self.trail
        lf0 = A(L, A(L, 'M0', 'sp0'), 'Y0'); lf1 = A(L, A(L, 'M1', 'sp1'), 'Y0')
        Hc = Hform(frame(L, lf0, lf1), A(L, 'PO', 'lo1'))
        H = {c: Hc, 1 - c: 'Ho'}
        return comp(lam(*args, A('u', A(L, H[0], H[1]), 'acc')))
    def build(self):
        k = {(c, a, b): self.M(c, a, b) for c in (0, 1) for a in (0, 1) for b in (0, 1)}
        eh = {c: self.end_handler(c) for c in (0, 1)}
        self.k, self.eh = k, eh
        lf = lambda key, Y: A(L, A(L, k[key], k[key]), Y)
        self.Z = {c: frame(L, lf((c, 0, 0), eh[c]), lf((c, 0, 1), eh[c])) for c in (0, 1)}
        self.O = {c: frame(L, lf((c, 1, 0), L), lf((c, 1, 1), L)) for c in (0, 1)}
        self.H = {c: Hform(self.Z[c], self.O[c]) for c in (0, 1)}
        return self
    def program(self): return A(L, self.H[0], self.H[1])
    def start(self, x, y, w):
        init = {'acc': L, 'Ho': self.H[1]}
        return A(self.H[0], chain(y, w, L), chain(x, w, A(L, L)), *[init[v] for v in self.trail])
    def expect(self, x, y, w):
        s = (x + y) % (1 << w); acc = L
        for i in range(w): acc = cell((s >> i) & 1, cell((x >> i) & 1, acc))
        return A(L, self.program(), acc)
    def code_size(self): return {key: size(v) for key, v in self.k.items()}

def check(D, w, cases):
    tot = F.Stats()
    for x, y in cases:
        v, st = run(D.start(x, y, w))
        assert v == V(D.expect(x, y, w)), (w, x, y)
        assert st.K == 0
        tot = tot + st
    return tot
def report(D, label):
    rng = random.Random(7)
    check(D, 4, [(x, y) for x in range(16) for y in range(16)])
    c16 = [(rng.randrange(1 << 16), rng.randrange(1 << 16)) for _ in range(30)]
    c32 = [(rng.randrange(1 << 32), rng.randrange(1 << 32)) for _ in range(30)]
    s16 = check(D, 16, c16); s32 = check(D, 32, c32); n = 30
    pb = (s32.steps - s16.steps) / n / 16
    print(f'{label:30} marginal {pb:5.1f} steps/bit | w=32 avg {s32.steps / n:6.0f} steps, {s32.W / n:.0f} W, '
          f'{s32.copied / n:.0f} copied, K {s32.K}, dropped {s32.dropped} | M {sorted(D.code_size().values())}', flush=True)
    return pb, s32
if __name__ == '__main__':
    def main():
        report(Adder6(state_first=False).build(), 'H-form, acc first')
        report(Adder6(state_first=True).build(), 'H-form, state first')
    big_stack(main)
