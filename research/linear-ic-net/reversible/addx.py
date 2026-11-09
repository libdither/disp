"""Reversible binary add, design X: words a, b as separate chains (bit 0 = △ rest, bit 1 = △ rest △),
carry as control (two states), the B-chain sits in the A-frame's leaf slot so the arm the A-triage picks
(itself a frame) triages the B-chain at once: per bit two triages and ONE leaf arm runs.
Output: reversed chains (MSB first). Leaf arms are λ-terms compiled by the prototype's bracket abstraction."""
import itertools
from common import *

def frame(c, d, b): return A(L, A(L, c, d), b)
def maj(a, b, c): return (a & b) | (a & c) | (b & c)
SP = [f'S{c}{a}{b}' for c in (0, 1) for a in (0, 1) for b in (0, 1)]

class Design:
    def __init__(self, trail_order=None):
        # trailing arguments after the A-chain's children, in this order
        self.trail = trail_order or (['Zo', 'Oo', 'accA', 'accS'] + SP + ['SE0', 'SE1'])
    def head_args(self, a, b):
        return ['E', 'sib', 'brest'] + (['bf'] if b else []) + ['other', 'arest'] + (['af'] if a else [])
    def leaf(self, c, a, b):
        s = a ^ b ^ c; c2 = maj(a, b, c); me = f'S{c}{a}{b}'
        args = self.head_args(a, b) + self.trail
        own = frame('E', me, 'sib') if b == 0 else frame('E', 'sib', me)
        Zc, Oc = (own, 'other') if a == 0 else ('other', own)
        cellA = A(L, 'accA') if a == 0 else A(L, 'accA', 'af')
        if s == 0: cellS = A(L, 'accS')
        else: cellS = A(L, 'accS', 'bf' if b else L)
        root = 'bf' if (b and s == 0) else L          # a spare filler leaf becomes the A-frame's root
        env = {'accA': cellA, 'accS': cellS}
        if c2 == c:
            nxt = A(root, A(L, 'brest', Zc), Oc); env['Zo'] = 'Zo'; env['Oo'] = 'Oo'
        else:
            nxt = A(root, A(L, 'brest', 'Zo'), 'Oo'); env['Zo'] = Zc; env['Oo'] = Oc
        body = A(nxt, 'arest', *[env.get(v, v) for v in self.trail])
        return lam(*args, body)
    def end(self, c):
        """EZ_c: Z_c triaged the B-terminator (leaf). args: z0 z1 O_c u(=△ from the A-terminator) trailing"""
        args = ['z0', 'z1', 'Oc', 'u'] + self.trail
        Zc = frame(f'SE{c}', 'z0', 'z1')
        Z = {c: Zc, 1 - c: 'Zo'}; O = {c: 'Oc', 1 - c: 'Oo'}
        prog = A(L, A(L, Z[0], O[0]), A(L, A(L, Z[1], O[1]), lst_vars(SP + ['SE0', 'SE1'])))
        return lam(*args, A('u', prog, A(L, 'accA', 'accS')))
    def build(self):
        self.code = {}
        for c in (0, 1):
            for a in (0, 1):
                for b in (0, 1):
                    self.code[(c, a, b)] = comp(self.leaf(c, a, b))
            self.code[('E', c)] = comp(self.end(c))
        k = self.code
        self.Z = {c: frame(k[('E', c)], k[(c, 0, 0)], k[(c, 0, 1)]) for c in (0, 1)}
        self.O = {c: frame(L, k[(c, 1, 0)], k[(c, 1, 1)]) for c in (0, 1)}
        self.spares = {f'S{c}{a}{b}': k[(c, a, b)] for c in (0, 1) for a in (0, 1) for b in (0, 1)}
        self.spares['SE0'] = k[('E', 0)]; self.spares['SE1'] = k[('E', 1)]
        return self
    def program(self):
        return A(L, A(L, self.Z[0], self.O[0]), A(L, A(L, self.Z[1], self.O[1]),
                 lst([self.spares[n] for n in SP + ['SE0', 'SE1']])))
    def start(self, x, y, w):
        TA, TB = A(L, L), L
        init = {'Zo': self.Z[1], 'Oo': self.O[1], 'accA': TA, 'accS': TB}
        init.update(self.spares)
        return A(frame(chain(y, w, TB), self.Z[0], self.O[0]), chain(x, w, TA), *[init[v] for v in self.trail])
    def expect(self, x, y, w):
        s = (x + y) % (1 << w)
        return A(L, self.program(), A(L, chain(rev(x, w), w, A(L, L)), chain(rev(s, w), w, L)))

def lst_vars(vs):
    t = L
    for v in reversed(vs): t = A(L, v, t)
    return t
def cell(bit, rest): return A(L, rest, L) if bit else A(L, rest)
def chain(n, w, term):
    t = term
    for i in reversed(range(w)): t = cell((n >> i) & 1, t)
    return t
def rev(n, w): return int(format(n, f'0{w}b')[::-1], 2)

def measure(D, w, cases):
    tot = F.Stats(); P = V(D.program())
    for x, y in cases:
        v, st = run(D.start(x, y, w))
        assert v == V(D.expect(x, y, w)), (x, y)
        assert st.K == 0
        tot = tot + st
    return tot

def main():
    D = Design().build()
    print('leaf arm sizes:', {k: size(v) for k, v in D.code.items()})
    rng = random.Random(7)
    print(HDR)
    for w in (4, 8, 16):
        cases = [(x, y) for x in range(16) for y in range(16)] if w == 4 else \
                [(rng.randrange(1 << w), rng.randrange(1 << w)) for _ in range(40)]
        tot = measure(D, w, cases); n = len(cases)
        avg = F.Stats()
        for k in avg.__slots__: setattr(avg, k, round(getattr(tot, k) / n))
        print(row(f'rev add X (compiled) w={w} avg/{n}', avg, f'{tot.steps / n / w:.1f} steps/bit'))
if __name__ == '__main__':
    big_stack(main)
