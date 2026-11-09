"""Generic reversible single-chain machine (tail recursive, output reversed onto an accumulator).
Bits: 0 = △ rest, 1 = △ △ rest; the chain ends in △. States q: frame F_q = △ (△ E lf_q0) lf_q1, E = △ e,
e = △ (△ △ △) △. Leaf frame lf = △ (△ sp M) Y: triaging E (a stem) runs M with its own spare sp first.
At the end of the chain the pieces fall through into the Y slot of lf_q0 (the end handler of state q);
the handler's spare sits in lf_q1's Y slot.  Machine: F_q chain acc [other state frame].
spec: delta(q, b) -> (bits to push, q'); fin(q) -> bits to push at the end."""
import sys
from common import *
from addx4 import best_found, subst_K, lam_P1
from treeparse import parse
def frame(c, d, b): return A(L, A(L, c, d), b)
def cellf(bit, x): return A(L, L, x) if bit else A(L, x)
E_CHILD = A(L, A(L, L, L), L)          # e = △ (△ △ △) △
E = A(L, E_CHILD)

class ChainMachine:
    def __init__(self, nstates, delta, fin, use_found=True, name=''):
        self.n, self.delta, self.fin, self.use_found, self.name = nstates, delta, fin, use_found, name
        assert nstates in (1, 2)
        self.trail = ['acc'] + (['Fo'] if nstates == 2 else [])
    def stage(self, job, lt):
        t = best_found(job) if self.use_found else None
        return t if t is not None else comp(lt)
    def push(self, bits, acc):
        for b in bits: acc = cellf(b, acc)
        return acc
    def M(self, q, b):
        out, q2 = self.delta(q, b)
        P1 = self.stage('P1', lam_P1())
        Eq = A('f', 'e') if b else A(L, 'e')              # a bit-1 filler becomes E's root
        head = ['lf', 'e', 'sib'] + (['f'] if b else []) + ['rest']
        own = frame(Eq, 'lf', 'sib') if b == 0 else frame(Eq, 'sib', 'lf')
        acc2 = self.push(out, 'acc')
        if q2 == q:
            # staged: rebuild the frame (G_b), then push and go (D)
            Gl = lam('lf', 'e', 'sib', *(['f'] if b else []), A('K', own))
            G = self.stage(f'G{b}', Gl)
            if len(out) == 1:
                D = self.stage(f'DD{out[0]}', lam('nxt', 'rest', 'acc', A('nxt', 'rest', acc2)))
            elif len(out) == 2 and self.n == 1:
                D = self.stage(f'D{out[0]}{out[1]}', lam('nxt', 'rest', 'acc', A('nxt', 'rest', acc2)))
            else:
                D = comp(lam('nxt', 'rest', *self.trail, A('nxt', 'rest', acc2, *self.trail[1:])))
            tail = subst_K(G, D)
        else:
            body = A('Fo', 'rest', acc2, own)
            tail = comp(lam(*head, *self.trail, body))
        return subst_K(P1, tail)
    def end_handler(self, q):
        args = ['sp0', 'M0', 'l1', 'l2', 'd', 'PL1', 'Y1'] + self.trail
        e = A(L, A('l1', 'l2'), 'd')                       # c' = △ l1 l2 rebuilt as l1 applied... see below
        e = A(L, A(L, 'l1', 'l2'), 'd')                    # wrong arity guard (overwritten next line)
        # rebuild e = △ (△ △ △) △ from the leaves l1, l2, d: c' = △ l1 l2, e = △ c' d
        e = A(L, A(L, 'l1', 'l2'), 'd')
        lf0 = A(L, A(L, 'sp0', 'M0'), 'Y1'); lf1 = A(L, 'PL1', 'Y1')
        Fq = frame(A(L, e), lf0, lf1)
        acc2 = self.push(self.fin(q), 'acc')
        if self.n == 1: prog = Fq
        else:
            F = {q: Fq, 1 - q: 'Fo'}; prog = A(L, F[0], F[1])
        return comp(lam(*args, A(L, prog, acc2)))
    def build(self):
        k = {(q, b): self.M(q, b) for q in range(self.n) for b in (0, 1)}
        eh = {q: self.end_handler(q) for q in range(self.n)}
        self.k, self.eh = k, eh
        self.F = {q: frame(E, A(L, A(L, k[(q, 0)], k[(q, 0)]), eh[q]), A(L, A(L, k[(q, 1)], k[(q, 1)]), eh[q]))
                  for q in range(self.n)}
        return self
    def program(self):
        return self.F[0] if self.n == 1 else A(L, self.F[0], self.F[1])
    def start(self, bits, q0=0):
        t = L
        for b in reversed(bits): t = cellf(b, t)
        args = [L] + ([self.F[1 - q0]] if self.n == 2 else [])
        return A(self.F[q0], t, *args)
    def expect(self, bits, q0=0):
        q, acc = q0, L
        for b in bits:
            out, q = self.delta(q, b)
            acc = self.push(out, acc)
        acc = self.push(self.fin(q), acc)
        return A(L, self.program(), acc)
    def sizes(self): return {k: size(v) for k, v in self.k.items()}

def measure(Mc, label, wmax=16, ntrial=40, rng=None):
    rng = rng or random.Random(3)
    for w in range(0, 7):
        for x in range(1 << w):
            bits = [(x >> i) & 1 for i in range(w)]
            v, st = run(Mc.start(bits)); assert v == V(Mc.expect(bits)), (label, bits); assert st.K == 0
    tot = {}
    for w in (16, 32):
        s = F.Stats()
        for _ in range(ntrial):
            bits = [rng.randrange(2) for _ in range(w)]
            v, st = run(Mc.start(bits)); assert v == V(Mc.expect(bits)); assert st.K == 0
            s = s + st
        tot[w] = s
    pb = (tot[32].steps - tot[16].steps) / ntrial / 16
    s = tot[32]; n = ntrial
    print(f'{label:26} exhaustive w<=6 ok | w=32 avg {s.steps / n:6.0f} steps {s.K / n:.0f} K {s.W / n:3.0f} W '
          f'{s.copied / n:6.0f} copied {s.dropped / n:.0f} dropped | marginal {pb:5.1f} steps/bit | M {Mc.sizes()}', flush=True)
    return pb, s

PROGRAMS = {
    'reverse':   (1, lambda q, b: ([b], 0), lambda q: []),
    'map NOT':   (1, lambda q, b: ([1 - b], 0), lambda q: []),
    'increment': (2, lambda q, b: (([0], 0) if b else ([1], 1)) if q == 0 else ([b], 1), lambda q: []),
    'decrement': (2, lambda q, b: (([1], 0) if b == 0 else ([0], 1)) if q == 0 else ([b], 1), lambda q: []),
}
if __name__ == '__main__':
    def main():
        for name, (n, d, fi) in PROGRAMS.items():
            measure(ChainMachine(n, d, fi, name=name).build(), name)
    big_stack(main)
