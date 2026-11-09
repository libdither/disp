"""Before/after for the chain machines: one bracket-compiled arm, bracket-compiled stages, superoptimized stages."""
from common import *
import chainm, chainm2
from chainm import ChainMachine, PROGRAMS, measure, frame, cellf
from chainm2 import DirectChain, measure as measure2, MORE

class Unstaged(ChainMachine):
    def M(self, q, b):
        out, q2 = self.delta(q, b)
        Eq = A('f', 'e') if b else A(L, 'e')
        lf = A(L, A(L, 'sp', 'sp'), 'Y')
        own = frame(Eq, lf, 'sib') if b == 0 else frame(Eq, 'sib', lf)
        acc2 = self.push(out, 'acc')
        head = ['sp', 'Y', 'e', 'sib'] + (['f'] if b else []) + ['rest']
        body = A(own, 'rest', acc2, *self.trail[1:]) if q2 == q else A('Fo', 'rest', acc2, own)
        return comp(lam(*head, *self.trail, body))

def main():
    for name in ('reverse', 'increment'):
        n, d, fi = PROGRAMS[name]
        measure(Unstaged(n, d, fi).build(), f'{name}: one bracket arm')
        measure(ChainMachine(n, d, fi, use_found=False).build(), f'{name}: bracket stages')
        measure(ChainMachine(n, d, fi, use_found=True).build(), f'{name}: superopt stages')
    n, d, fi = PROGRAMS['map NOT']
    measure2(DirectChain(n, d, fi, use_found=False).build(), 'map NOT direct: bracket stages')
    measure2(DirectChain(n, d, fi, use_found=True).build(), 'map NOT direct: superopt stages')
big_stack(main)
