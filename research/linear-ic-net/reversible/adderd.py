"""In-place reversible add, direct style: (a, b) -> (a, a + b mod 2^w), both LSB-first chains in and out.
Same frames and arms as addx6 (carry as control, H-form inactive state), but no accumulator: each leaf arm
calls the next A-frame inside a wrap W that prepends this position's cells to the two output chains the rest of
the run returns. Result of a run: R = △ OA (△ OS PROGRAM). W = △ (△ △ △) ARM1 opens R (its two leaves become
material); ARM1 pushes the a-cell and applies the second wrap W2 = △ (△ △ △) ARM2 to △ OS PROGRAM."""
import sys
from common import *
from addx4 import frame, maj, cell, chain, Cf, lam_P1, lam_B, lam_C, best_found, subst_K
from addx6 import Hform

def ARM2(s):   # c d os p -> △ (Cs os) p ; c is the new root, d feeds the cell
    cs = A('d', 'os') if s == 0 else A('d', L, 'os')
    return comp(lam('c', 'd', 'os', A('c', cs)))
def W2(s): return A(L, A(L, L, L), ARM2(s))
def ARM1(a, s):  # c d oa rest -> △ (Ca oa) (W2 rest)
    ca = A('d', 'oa') if a == 0 else A('d', L, 'oa')
    return comp(lam('c', 'd', 'oa', 'rest', A('c', ca, A(W2(s), 'rest'))))
def WRAP(a, s):
    found = best_found(f'A{a}{s}')          # superoptimized one-wrap opener, if a search found one
    return A(L, A(L, L, L), found if found is not None else ARM1(a, s))

class AdderD:
    def __init__(self, use_found=True): self.use_found = use_found
    def stage(self, job, lt):
        t = best_found(job) if self.use_found else None
        return t if t is not None else comp(lt)
    def M(self, c, a, b):
        s = a ^ b ^ c; c2 = maj(a, b, c)
        P1 = self.stage('P1', lam_P1())
        Bb = self.stage('B0' if b == 0 else 'B1c', lam_B(b))
        W = WRAP(a, s)
        if c2 == c:
            Ca = self.stage('Ka' if a == 0 else 'C1c', lam_C(a))
            D = A(L, A(L, A(L, A(L, A(L, A(L, W))))))           # nxt arest Ho -> W (nxt arest Ho)
            return subst_K(P1, subst_K(Bb, subst_K(Ca, D)))
        head = ['own', 'brest', 'other'] + (['af'] if a else []) + ['arest', 'Ho']
        Zc, Oc = ('own', 'other') if a == 0 else ('other', 'own')
        H = Hform(Zc, Oc)
        if a:   # absorb the a-cell filler as the root of the H-form
            H = A('af', L, A(L, A(L, A(L, A(L, L)), A(L, L, A(L, L, Zc))), Oc))
        body = A(W, A('Ho', 'brest', 'arest', H))
        return subst_K(P1, subst_K(Bb, comp(lam(*head, body))))
    def end_handler(self, c):
        args = ['M1', 'sp1', 'M0', 'sp0', 'Y0', 'PO', 'lo1', 'u', 'Ho']
        lf0 = A(L, A(L, 'M0', 'sp0'), 'Y0'); lf1 = A(L, A(L, 'M1', 'sp1'), 'Y0')
        Hc = Hform(frame(L, lf0, lf1), A(L, 'PO', 'lo1'))
        Hs = {c: Hc, 1 - c: 'Ho'}
        return comp(lam(*args, A(L, A(L, 'u'), A(L, L, A(L, Hs[0], Hs[1])))))
    def build(self):
        k = {(c, a, b): self.M(c, a, b) for c in (0, 1) for a in (0, 1) for b in (0, 1)}
        eh = {c: self.end_handler(c) for c in (0, 1)}
        self.k = k
        lf = lambda key, Y: A(L, A(L, k[key], k[key]), Y)
        Z = {c: frame(L, lf((c, 0, 0), eh[c]), lf((c, 0, 1), eh[c])) for c in (0, 1)}
        O = {c: frame(L, lf((c, 1, 0), L), lf((c, 1, 1), L)) for c in (0, 1)}
        self.H = {c: Hform(Z[c], O[c]) for c in (0, 1)}
        return self
    def program(self): return A(L, self.H[0], self.H[1])
    def start(self, x, y, w): return A(self.H[0], chain(y, w, L), chain(x, w, A(L, L)), self.H[1])
    def expect(self, x, y, w):
        s = (x + y) % (1 << w)
        return A(L, chain(x, w, A(L, L)), A(L, chain(s, w, L), self.program()))
    def sizes(self): return {k: size(v) for k, v in self.k.items()}

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
    print(f'{label:30} w=4 exhaustive ok | w=32 avg {s32.steps / n:6.0f} steps, {s32.K / n:.0f} K, {s32.W / n:.0f} W, '
          f'{s32.copied / n:.0f} copied, {s32.dropped / n:.0f} dropped | marginal {pb:5.1f} steps/bit | M {sorted(D.sizes().values())}', flush=True)
    return pb, s32
if __name__ == '__main__':
    big_stack(lambda: report(AdderD().build(), 'in-place adder (direct style)'))
