"""History-free reversible binary add (a, b) -> (a, a+b mod 2^n) by exchange rules: a carry walker
and a column change kind in place (each keeps its back port, so no footprint), the walker reversing
the list's pointers as it goes down (MAJ-like) and restoring them on the way up (UMA-like), the up
sweep using up the carries the down sweep left in the columns. Ports: W[p, t], X[p, n], End[p], Start[p]."""
import random, itertools

def maj(c, a, b):   # (c, a, b) -> (carry out, a^c, b^c)
    return ((a & b) | (a & c) | (b & c), a ^ c, b ^ c)
def uma(c1, a, b):  # (c_{i+1}, a', b') -> (c_i, a_i, s_i)
    c = c1 ^ (a & b); return (c, a ^ c, a ^ b ^ c)
MAJ = {(c, a, b): maj(c, a, b) for c, a, b in itertools.product((0, 1), repeat=3)}
UMA = {(c, a, b): uma(c, a, b) for c, a, b in itertools.product((0, 1), repeat=3)}
assert len(set(MAJ.values())) == 8 and len(set(UMA.values())) == 8      # both bijections
MAJi = {v: k for k, v in MAJ.items()}; UMAi = {v: k for k, v in UMA.items()}

class Lst:
    def __init__(self, a_bits, b_bits):
        self.kind, self.link, n = {}, {}, 0
        def new(k):
            nonlocal n; n += 1; self.kind[n] = k; return n
        self.start = new(('Start',)); self.w = new(('Wd', 0)); cols = [new(('X', a, b)) for a, b in zip(a_bits, b_bits)]
        self.end = new(('End',))
        def con(x, y): self.link[x] = y; self.link[y] = x
        con((self.w, 't'), (self.start, 'p')); con((self.w, 'p'), (cols[0], 'p'))
        for x, y in zip(cols, cols[1:] + [self.end]): con((x, 'n'), (y, 'p'))
    def snap(self): return dict(self.kind), dict(self.link)
    def con(self, x, y): self.link[x] = y; self.link[y] = x
    def fwd(self):
        w = self.w; kw = self.kind[w]; y, pt = self.link[(w, 'p')]; ky = self.kind[y]
        if pt != 'p': return False
        if ky[0] == 'X':
            table = MAJ if kw[0] == 'Wd' else UMA
            c2, a2, b2 = table[(kw[1], ky[1], ky[2])]
            tp, np_ = self.link[(w, 't')], self.link[(y, 'n')]
            for e in [(w, 'p'), (y, 'p'), (w, 't'), tp, (y, 'n'), np_]: self.link.pop(e)
            self.con((w, 'p'), np_); self.con((w, 't'), (y, 'p')); self.con((y, 'n'), tp)
            self.kind[w] = (kw[0], c2); self.kind[y] = ('X', a2, b2); return True
        if ky[0] == 'End' and kw[0] == 'Wd':                        # turn round
            tp = self.link[(w, 't')]
            for e in [(w, 'p'), (y, 'p'), (w, 't'), tp]: self.link.pop(e)
            self.con((w, 'p'), tp); self.con((w, 't'), (y, 'p')); self.kind[w] = ('Wu', kw[1]); return True
        return False                                                # Wu facing Start: done
    def back(self):
        w = self.w; kw = self.kind[w]; y, pt = self.link[(w, 't')]; ky = self.kind[y]
        if pt != 'p': return False
        if ky[0] == 'X':
            table = MAJi if kw[0] == 'Wd' else UMAi
            c0, a0, b0 = table[(kw[1], ky[1], ky[2])]
            np_, tp = self.link[(w, 'p')], self.link[(y, 'n')]
            for e in [(w, 't'), (y, 'p'), (w, 'p'), np_, (y, 'n'), tp]: self.link.pop(e)
            self.con((w, 'p'), (y, 'p')); self.con((w, 't'), tp); self.con((y, 'n'), np_)
            self.kind[w] = (kw[0], c0); self.kind[y] = ('X', a0, b0); return True
        if ky[0] == 'End' and kw[0] == 'Wu':
            pp = self.link[(w, 'p')]
            for e in [(w, 't'), (y, 'p'), (w, 'p'), pp]: self.link.pop(e)
            self.con((w, 'p'), (y, 'p')); self.con((w, 't'), pp); self.kind[w] = ('Wd', kw[1]); return True
        return False
    def read(self):
        out = []; x = self.link[(self.w, 't')][0]
        while self.kind[x][0] == 'X': out.append(self.kind[x][1:]); x = self.link[(x, 'n')][0]
        return out

rng = random.Random(3); ok = exact = brown = 0; N = 0
for trial in range(3000):
    n = rng.randint(1, 12); a = rng.randrange(1 << n); b = rng.randrange(1 << n)
    ab = [(a >> i) & 1 for i in range(n)]; bb = [(b >> i) & 1 for i in range(n)]
    L = Lst(ab, bb); s0 = L.snap(); steps = 0
    while L.fwd(): steps += 1
    cols = L.read(); s = (a + b) % (1 << n)
    ok += cols == [(ab[i], (s >> i) & 1) for i in range(n)] and L.kind[L.w] == ('Wu', 0) and steps == 2 * n + 1
    while L.back(): pass
    exact += L.snap() == s0
    # Brownian: random forward/back moves with a forward bias, then finish
    L2 = Lst(ab, bb)
    for _ in range(20 * n):
        (L2.fwd() if rng.random() < 0.6 else L2.back())
    while L2.fwd(): pass
    brown += L2.read() == cols
    N += 1
print(f"{N} adds up to 12 bits: right sums with the walker done and carry 0 {ok}, back to the input exactly {exact}, "
      f"random back-and-forth then on to the end gives the same {brown}; left over: no agents, no footprints")
