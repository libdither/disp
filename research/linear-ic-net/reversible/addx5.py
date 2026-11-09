"""X5: X4 with the inactive state packed as ONE trailing value and a choice of trailing order.
pair : the inactive state rides as two values Zo, Oo.
hole : the inactive state rides as H = C (B △ (C △ Z)) O, so H x = △ (△ x Z) O is its A-frame around x.
order: 'acc first' or 'state first'. The end term is △ PROGRAM acc with PROGRAM fixed by the start term."""
import sys
from common import *
from addx4 import frame, maj, cell, chain, Cf, lam_P1, lam_B, lam_C, lam_D, best_found, subst_K, check
def Hform(Z, O): return A(L, L, A(L, A(L, A(L, A(L, L)), A(L, L, A(L, L, Z))), O))   # C (B △ (C △ Z)) O

class Adder5:
    def __init__(self, state='pair', acc_first=True, use_found=True):
        self.state, self.acc_first, self.use_found = state, acc_first, use_found
        st = ['Zo', 'Oo'] if state == 'pair' else ['Ho']
        self.trail = (['acc'] + st) if acc_first else (st + ['acc'])
    def stage(self, job, lt):
        t = best_found(job) if self.use_found else None
        return t if t is not None else comp(lt)
    def M(self, c, a, b):
        s = a ^ b ^ c; c2 = maj(a, b, c)
        P1 = self.stage('P1', lam_P1())
        Bb = self.stage('B0' if b == 0 else 'B1c', lam_B(b))
        head = ['own', 'brest', 'other'] + (['af'] if a else []) + ['arest']
        accq = Cf[s](Cf[a]('acc'))
        Zc, Oc = ('own', 'other') if a == 0 else ('other', 'own')
        root = 'af' if a else L
        if c2 == c:
            env = {'acc': accq}
            if self.acc_first:   # cheap tail: the inactive state is never touched
                Ca = self.stage('Ka' if a == 0 else 'C1c', lam_C(a))
                D = self.stage({(0, 0): 'D00', (0, 1): 'D01', (1, 0): 'D10', (1, 1): 'D11'}[(a, s)], lam_D(a, s))
                return subst_K(P1, subst_K(Bb, subst_K(Ca, D)))
            body = A(A(root, A(L, 'brest', Zc), Oc), 'arest', *[env.get(v, v) for v in self.trail])
        else:
            if self.state == 'pair':
                env = {'acc': accq, 'Zo': Zc, 'Oo': Oc}
                nxt = A(root, A(L, 'brest', 'Zo'), 'Oo')
            else:
                env = {'acc': accq, 'Ho': Hform(Zc, Oc)}
                nxt = A('Ho', 'brest')
                if a: env['acc'] = Cf[s](A(L, 'af', 'acc'))    # af becomes the a-cell filler
            body = A(nxt, 'arest', *[env.get(v, v) for v in self.trail])
        if a and not (c2 != c and self.state == 'hole'): pass
        args = head + self.trail
        used_af = (c2 == c) or self.state == 'pair'
        if a and not used_af: pass
        tail = comp(lam(*args, body))
        return subst_K(P1, subst_K(Bb, tail))
    def end_handler(self, c):
        T = self.trail
        args = ['M1', 'sp1', 'M0', 'sp0', 'Y0', 'PO', 'lo1', 'u'] + T
        lf0 = A(L, A(L, 'M0', 'sp0'), 'Y0'); lf1 = A(L, A(L, 'M1', 'sp1'), 'Y0')
        Zc = frame(L, lf0, lf1); Oc = A(L, 'PO', 'lo1')
        if self.state == 'pair':
            Z = {c: Zc, 1 - c: 'Zo'}; O = {c: Oc, 1 - c: 'Oo'}
            prog = A(L, A(L, Z[0], O[0]), A(L, Z[1], O[1]))
            return comp(lam(*args, A('u', prog, 'acc')))
        # hole form: PROGRAM = △ (△ Z0 O0) H1. In state 0 we hold Z0, O0 and H1 directly.
        if c == 0:
            return comp(lam(*args, A('u', A(L, A(L, Zc, Oc), 'Ho'), 'acc')))
        # state 1: we hold Z1, O1 and H0 = △ △ (△ (B △ (C △ Z0)) O0); open H0 with triage frames whose
        # spare arms are leaves, then rebuild: the opener arms are part of this handler's own code.
        # H0 is a fork △ △ X with X = △ P O0 and P = △ (△ (△ △)) (△ △ (△ △ Z0)).
        # Simplest correct route: let a λ-level triage pick H0 apart (the lowering hands beaten arms back).
        k3 = lam('l1', 'l2', 'q', 'r', A('q', 'r'))
        raise NotImplementedError
    def build(self):
        k = {(c, a, b): self.M(c, a, b) for c in (0, 1) for a in (0, 1) for b in (0, 1)}
        eh = {c: self.end_handler(c) for c in (0, 1)}
        self.k, self.eh = k, eh
        lf = lambda key, Y: A(L, A(L, k[key], k[key]), Y)
        self.Z = {c: frame(L, lf((c, 0, 0), eh[c]), lf((c, 0, 1), eh[c])) for c in (0, 1)}
        self.O = {c: frame(L, lf((c, 1, 0), L), lf((c, 1, 1), L)) for c in (0, 1)}
        return self
    def program(self):
        if self.state == 'pair': return A(L, A(L, self.Z[0], self.O[0]), A(L, self.Z[1], self.O[1]))
        return A(L, A(L, self.Z[0], self.O[0]), Hform(self.Z[1], self.O[1]))
    def start(self, x, y, w):
        init = {'acc': L, 'Zo': self.Z[1], 'Oo': self.O[1], 'Ho': Hform(self.Z[1], self.O[1])}
        return A(frame(chain(y, w, L), self.Z[0], self.O[0]), chain(x, w, A(L, L)), *[init[v] for v in self.trail])
    def expect(self, x, y, w):
        s = (x + y) % (1 << w); acc = L
        for i in range(w): acc = cell((s >> i) & 1, cell((x >> i) & 1, acc))
        return A(L, self.program(), acc)
    def code_size(self): return {key: size(v) for key, v in self.k.items()}

def report(D, label):
    rng = random.Random(7)
    check(D, 4, [(x, y) for x in range(16) for y in range(16)])
    c16 = [(rng.randrange(1 << 16), rng.randrange(1 << 16)) for _ in range(30)]
    c32 = [(rng.randrange(1 << 32), rng.randrange(1 << 32)) for _ in range(30)]
    s16 = check(D, 16, c16); s32 = check(D, 32, c32); n = 30
    pb = (s32.steps - s16.steps) / n / 16
    print(f'{label:34} marginal {pb:5.1f} steps/bit | w=32 avg {s32.steps / n:6.0f} steps, {s32.W / n:.0f} W, '
          f'{s32.copied / n:.0f} copied, K {s32.K}, dropped {s32.dropped} | M {sorted(D.code_size().values())}', flush=True)

if __name__ == '__main__':
    def main():
        for acc_first in (True, False):
            report(Adder5('pair', acc_first).build(), f'pair, {"acc first" if acc_first else "state first"}')
    big_stack(main)
