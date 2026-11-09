"""Design X4 = X3 with bit-1 cells that carry their filler on the LEFT (bit 0 = △ rest, bit 1 = △ △ rest), so
every output cell is a constant applied to the rest (C0 = △, C1 = △△), and leaf arms built from superoptimized
stages: P1 (rebuild the leaf frame) -> B_b (rebuild the B-frame) -> C_a (rebuild the A-frame) -> D (push cells, go).
Each stage hands its result to the next stage as that stage's first argument."""
import sys, glob, re
from common import *
from treeparse import parse

def frame(c, d, b): return A(L, A(L, c, d), b)
def maj(a, b, c): return (a & b) | (a & c) | (b & c)
def cell(bit, rest): return A(L, L, rest) if bit else A(L, rest)
def chain(n, w, term):
    t = term
    for i in reversed(range(w)): t = cell((n >> i) & 1, t)
    return t
TRAIL = ['acc', 'Zo', 'Oo']
Cf = {0: lambda x: A(L, x), 1: lambda x: A(L, L, x)}

# --- λ versions of every stage (fallback when no superoptimized tree is known) ---
def lam_P1():  return lam('sp', 'Y', A('K', A(L, A(L, 'sp', 'sp'), 'Y')))
def lam_B(b):
    return lam('lf', 'sib', A('K', frame(L, 'lf', 'sib'))) if b == 0 else lam('lf', 'sib', 'bf', A('K', frame('bf', 'sib', 'lf')))
def lam_C(a):
    return lam('own', 'brest', 'other', A('K', frame('brest', 'own', 'other'))) if a == 0 else \
           lam('own', 'brest', 'other', 'af', A('K', A('af', A(L, 'brest', 'other'), 'own')))
def lam_D(a, s):
    return lam('nxt', 'arest', 'acc', A('nxt', 'arest', Cf[s](Cf[a]('acc'))))
def lam_SW(c, a, b):
    s = a ^ b ^ c
    args = ['own', 'brest', 'other'] + (['af'] if a else []) + ['arest', 'acc', 'Zo', 'Oo']
    Zc, Oc = ('own', 'other') if a == 0 else ('other', 'own')
    root = 'af' if a else L
    return lam(*args, A(A(root, A(L, 'brest', 'Zo'), 'Oo'), 'arest', Cf[s](Cf[a]('acc')), Zc, Oc))

def subst_K(t, k):
    if t == 'K': return k
    if isinstance(t, tuple): return (subst_K(t[0], k), subst_K(t[1], k))
    return t
def compile_stage(lt):
    """bracket-abstract a stage whose body mentions the free constant K; returns a term with 'K' inside"""
    return comp(lt)

def best_found(job):
    """smallest tree a superoptimizer log recorded for this stage, or None"""
    best = None
    for f in glob.glob(f'so*_{job}.log'):
        for line in open(f):
            m = re.match(r'\s+found size (\d+).*?: (\(.*\)|△)\s*$', line)
            if m and (best is None or int(m.group(1)) < best[0]): best = (int(m.group(1)), m.group(2))
    return parse(best[1]) if best else None

class Adder4:
    def __init__(self, use_found=True):
        self.use_found = use_found
    def stage(self, job, lt):
        t = best_found(job) if self.use_found else None
        return t if t is not None else compile_stage(lt)
    def M(self, c, a, b):
        s = a ^ b ^ c; c2 = maj(a, b, c)
        P1 = self.stage('P1', lam_P1())
        Bb = self.stage('B0' if b == 0 else 'B1c', lam_B(b))
        if c2 != c:
            tail = compile_stage(lam_SW(c, a, b))
        else:
            Ca = self.stage('Ka' if a == 0 else 'C1c', lam_C(a))
            D = self.stage({(0, 0): 'D00', (0, 1): 'D01', (1, 0): 'D10', (1, 1): 'D11'}[(a, s)], lam_D(a, s))
            tail = subst_K(Ca, D)
        return subst_K(P1, subst_K(Bb, tail))
    def end_handler(self, c):
        args = ['M1', 'sp1', 'M0', 'sp0', 'Y0', 'PO', 'lo1', 'u'] + TRAIL
        lf0 = A(L, A(L, 'M0', 'sp0'), 'Y0'); lf1 = A(L, A(L, 'M1', 'sp1'), 'Y0')
        Zc = frame(L, lf0, lf1); Oc = A(L, 'PO', 'lo1')
        Z = {c: Zc, 1 - c: 'Zo'}; O = {c: Oc, 1 - c: 'Oo'}
        return comp(lam(*args, A('u', A(L, A(L, Z[0], O[0]), A(L, Z[1], O[1])), 'acc')))
    def build(self):
        k = {(c, a, b): self.M(c, a, b) for c in (0, 1) for a in (0, 1) for b in (0, 1)}
        eh = {c: self.end_handler(c) for c in (0, 1)}
        self.k, self.eh = k, eh
        lf = lambda key, Y: A(L, A(L, k[key], k[key]), Y)
        self.Z = {c: frame(L, lf((c, 0, 0), eh[c]), lf((c, 0, 1), eh[c])) for c in (0, 1)}
        self.O = {c: frame(L, lf((c, 1, 0), L), lf((c, 1, 1), L)) for c in (0, 1)}
        return self
    def program(self): return A(L, A(L, self.Z[0], self.O[0]), A(L, self.Z[1], self.O[1]))
    def start(self, x, y, w):
        return A(frame(chain(y, w, L), self.Z[0], self.O[0]), chain(x, w, A(L, L)), L, self.Z[1], self.O[1])
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

def report(D, label, rng=None):
    rng = rng or random.Random(7)
    check(D, 4, [(x, y) for x in range(16) for y in range(16)])
    c16 = [(rng.randrange(1 << 16), rng.randrange(1 << 16)) for _ in range(30)]
    c32 = [(rng.randrange(1 << 32), rng.randrange(1 << 32)) for _ in range(30)]
    s16 = check(D, 16, c16); s32 = check(D, 32, c32); n = 30
    pb = (s32.steps - s16.steps) / n / 16
    print(f'{label:30} w=4 exhaustive ok | w=32 avg: {s32.steps / n:6.0f} steps {s32.K / n:2.0f} K {s32.W / n:3.0f} W '
          f'{s32.copied / n:7.0f} copied {s32.dropped / n:2.0f} dropped | marginal {pb:5.1f} steps/bit')
    print(f'   M sizes {D.code_size()}')
    return pb, s32

if __name__ == '__main__':
    def main():
        report(Adder4(use_found=False).build(), 'X4 staged, bracket stages')
        report(Adder4(use_found=True).build(), 'X4 staged, superopt stages')
    big_stack(main)
