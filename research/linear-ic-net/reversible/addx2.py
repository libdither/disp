"""Design X with self-spared leaf arms: every leaf arm is a frame △ (△ spare Y) M that triages the B-frame's
leaf slot (always a fork), so its body M gets its own spare as the FIRST argument (no deep routing).
Z_c's leaf slot is the end handler for state c; O_c's leaf slot is a constant fork."""
import itertools, sys
from common import *
from addx import frame, maj, cell, chain, rev, lst_vars

class Design2:
    def __init__(self, trail=('accA', 'accS', 'Zo', 'Oo', 'SE0', 'SE1'), y_leaf=True):
        self.trail = list(trail)
    def head(self, a, b):
        # M's args: own spare, Y slot, the two children of the B-frame's leaf-slot fork, sibling leaf frame, ...
        return ['sp', 'Y', 'e1', 'e2', 'sib', 'brest'] + (['bf'] if b else []) + ['other', 'arest'] + (['af'] if a else [])
    def M(self, c, a, b):
        s = a ^ b ^ c; c2 = maj(a, b, c)
        me = frame('sp', 'Y', 'sp')                      # the leaf frame rebuilt: spare stays in the leaf slot
        E = A(L, 'e1', 'e2')
        own = frame(E, me, 'sib') if b == 0 else frame(E, 'sib', me)
        Zc, Oc = (own, 'other') if a == 0 else ('other', own)
        cellA = A(L, 'accA') if a == 0 else A(L, 'accA', 'af')
        cellS = A(L, 'accS') if s == 0 else A(L, 'accS', 'bf' if b else L)
        root = 'bf' if (b and s == 0) else L
        env = {'accA': cellA, 'accS': cellS}
        if c2 == c: nxt = A(root, A(L, 'brest', Zc), Oc)
        else:
            nxt = A(root, A(L, 'brest', 'Zo'), 'Oo'); env['Zo'] = Zc; env['Oo'] = Oc
        return lam(*self.head(a, b), *self.trail, A(nxt, 'arest', *[env.get(v, v) for v in self.trail]))
    def end(self, c):
        # Z_c triaged the B-terminator: E_c z0 z1 O_c u trailing ; E_c = C q r so this runs q z0 r z1 ...
        # we compile the handler as a λ-term over (z0, z1, Oc, u, trailing) and wrap it as E_c = C (B? ..)
        args = ['z0', 'z1', 'Oc', 'u'] + self.trail
        Zc = frame(f'SE{c}', 'z0', 'z1')
        Z = {c: Zc, 1 - c: 'Zo'}; O = {c: 'Oc', 1 - c: 'Oo'}
        prog = A(L, A(L, A(L, Z[0], O[0]), A(L, Z[1], O[1])), A(L, 'SE0', 'SE1'))
        return lam(*args, A('u', prog, A(L, 'accA', 'accS')))
    def build(self):
        k = {}
        for c in (0, 1):
            for a in (0, 1):
                for b in (0, 1):
                    k[(c, a, b)] = comp(self.M(c, a, b))
            k[('E', c)] = comp(self.end(c))
            sh = F.KIND[V(k[('E', c)])]
            assert sh == 2, 'end handler must be a fork'
        self.k = k
        Y = L
        leaf = {key: frame(k[key], Y, k[key]) for key in k if key[0] != 'E'}
        self.Z = {c: frame(k[('E', c)], leaf[(c, 0, 0)], leaf[(c, 0, 1)]) for c in (0, 1)}
        self.O = {c: frame(A(L, L, L), leaf[(c, 1, 0)], leaf[(c, 1, 1)]) for c in (0, 1)}
        self.SE = {f'SE{c}': k[('E', c)] for c in (0, 1)}
        return self
    def program(self):
        return A(L, A(L, A(L, self.Z[0], self.O[0]), A(L, self.Z[1], self.O[1])), A(L, self.SE['SE0'], self.SE['SE1']))
    def start(self, x, y, w):
        TA, TB = A(L, L), L
        init = {'Zo': self.Z[1], 'Oo': self.O[1], 'accA': TA, 'accS': TB}; init.update(self.SE)
        return A(frame(chain(y, w, TB), self.Z[0], self.O[0]), chain(x, w, TA), *[init[v] for v in self.trail])
    def expect(self, x, y, w):
        s = (x + y) % (1 << w)
        return A(L, self.program(), A(L, chain(rev(x, w), w, A(L, L)), chain(rev(s, w), w, L)))

def measure(D, w, cases):
    tot = F.Stats()
    for x, y in cases:
        v, st = run(D.start(x, y, w))
        assert v == V(D.expect(x, y, w)), (x, y)
        assert st.K == 0
        tot = tot + st
    return tot

def per_bit(D, rng):
    c16 = [(rng.randrange(1 << 16), rng.randrange(1 << 16)) for _ in range(30)]
    c32 = [(rng.randrange(1 << 32), rng.randrange(1 << 32)) for _ in range(30)]
    s16 = measure(D, 16, c16); s32 = measure(D, 32, c32)
    return (s32.steps - s16.steps) / 30 / 16, s16, s32

def main():
    D = Design2().build()
    print('M sizes:', {k: size(v) for k, v in D.k.items()})
    rng = random.Random(7)
    tot = measure(D, 4, [(x, y) for x in range(16) for y in range(16)])
    print('w=4 exhaustive ok, K =', tot.K)
    pb, s16, s32 = per_bit(D, rng)
    print(f'marginal cost {pb:.1f} steps/bit; w=32 avg steps {s32.steps / 30:.0f}, W {s32.W / 30:.0f}, copied {s32.copied / 30:.0f}')
if __name__ == '__main__':
    big_stack(main)
