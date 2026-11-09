"""Reversible binary add, design X3.
Words a, b: separate chains, bit 0 = △ rest, bit 1 = △ rest △; a ends in TA = △△, b ends in TB = △.
Carry = control: state c uses B-frames Z_c (a = 0) and O_c (a = 1); the other state's pair rides at the end.
Chain of three triages per bit, no arm code in between:
  A-frame △ (△ Bchain Z_c) O_c triages the a-cell; the picked B-frame triages Bchain (its leaf slot);
  the picked leaf frame △ (△ M sp) Y triages the B-frame's leaf slot E = △ and runs M with its own spare sp first.
M rebuilds all three frames, pushes the a-cell and the sum cell onto one accumulator (reversed, interleaved),
and calls the next A-frame. At the end the leftover pieces fall through to the Y slot of Z_c's b = 1 leaf
frame, which holds the end handler for state c (its spare sits in the b = 0 leaf frame's Y slot).
Output: △ PROGRAM acc, acc = s_{w-1} a_{w-1} ... s_0 a_0 △ (MSB first, each position: a-cell then s-cell)."""
import sys
from common import *

def frame(c, d, b): return A(L, A(L, c, d), b)
def maj(a, b, c): return (a & b) | (a & c) | (b & c)
def cell(bit, rest): return A(L, rest, L) if bit else A(L, rest)
def chain(n, w, term):
    t = term
    for i in reversed(range(w)): t = cell((n >> i) & 1, t)
    return t
TRAIL = ['acc', 'Zo', 'Oo']

def M_args(a, b):
    return ['sp', 'Y', 'sib', 'brest'] + (['bf'] if b else []) + ['other', 'arest'] + (['af'] if a else [])

def M_body(c, a, b):
    """the λ-body of leaf arm M for state c, a-bit a, b-bit b"""
    s = a ^ b ^ c; c2 = maj(a, b, c)
    lf = A(L, A(L, 'sp', 'sp'), 'Y')
    E = 'bf' if (b and s == 0) else L                  # a spare filler becomes the B-frame's leaf slot
    own = frame(E, lf, 'sib') if b == 0 else frame(E, 'sib', lf)
    Zc, Oc = (own, 'other') if a == 0 else ('other', own)
    acellf = (lambda x: A(L, x)) if a == 0 else (lambda x: A(L, x, 'af'))
    scellf = (lambda x: A(L, x)) if s == 0 else (lambda x: A(L, x, 'bf' if b else L))
    acc2 = scellf(acellf('acc'))
    if c2 == c: return A(frame('brest', Zc, Oc), 'arest', acc2, 'Zo', 'Oo')
    return A(frame('brest', 'Zo', 'Oo'), 'arest', acc2, Zc, Oc)

def end_args(): return ['M1', 'sp1', 'M0', 'sp0', 'Y0', 'PO', 'lo1', 'u'] + TRAIL
def end_body(c):
    lf0 = A(L, A(L, 'M0', 'sp0'), 'Y0'); lf1 = A(L, A(L, 'M1', 'sp1'), 'Y0')   # Y0 (the handler's spare) twice
    Zc = frame(L, lf0, lf1); Oc = A(L, 'PO', 'lo1')
    Z = {c: Zc, 1 - c: 'Zo'}; O = {c: Oc, 1 - c: 'Oo'}
    return A('u', A(L, A(L, Z[0], O[0]), A(L, Z[1], O[1])), 'acc')

class Adder:
    def __init__(self, Mcode=None, Ecode=None):
        """Mcode(c, a, b) -> tree; Ecode(c) -> tree. Defaults: bracket abstraction of the λ-terms."""
        self.Mcode = Mcode or (lambda c, a, b: comp(lam(*M_args(a, b), *TRAIL, M_body(c, a, b))))
        self.Ecode = Ecode or (lambda c: comp(lam(*end_args(), end_body(c))))
    def build(self):
        k = {(c, a, b): self.Mcode(c, a, b) for c in (0, 1) for a in (0, 1) for b in (0, 1)}
        eh = {c: self.Ecode(c) for c in (0, 1)}
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
    def code_size(self):
        return {k: size(v) for k, v in self.k.items()}

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
    t4 = check(D, 4, [(x, y) for x in range(16) for y in range(16)])
    c16 = [(rng.randrange(1 << 16), rng.randrange(1 << 16)) for _ in range(30)]
    c32 = [(rng.randrange(1 << 32), rng.randrange(1 << 32)) for _ in range(30)]
    s16 = check(D, 16, c16); s32 = check(D, 32, c32)
    pb = (s32.steps - s16.steps) / 30 / 16
    n = 30
    print(f'{label:34} w=4 exhaustive ok | w=32: {s32.steps / n:7.0f} steps {s32.K / n:3.0f} K {s32.W / n:4.0f} W '
          f'{s32.copied / n:8.0f} copied {s32.dropped / n:3.0f} dropped | marginal {pb:5.1f} steps/bit | '
          f'M sizes {sorted(D.code_size().values())}')
    return pb, s32

if __name__ == '__main__':
    def main():
        D = Adder().build()
        report(D, 'X3 compiled M')
    big_stack(main)
