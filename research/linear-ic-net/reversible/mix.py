"""Rule mix of the final reversible adder vs the ordinary one (w = 32, 30 random adds)."""
from common import *
from addx6 import Adder6
from adderd import AdderD
from ord_add import badd, unword
from addx4 import chain
rng = random.Random(11)
cases = [(rng.randrange(1 << 32), rng.randrange(1 << 32)) for _ in range(30)]
def mixline(label, s):
    moves = s.B + s.B2 + s.C + s.I; tri_ = s.T0 + s.T1 + s.T2
    print(f'{label:28} steps {s.steps:7} | moves (B,B2,C,I) {moves:6} = {100 * moves / s.steps:4.1f}% | triage {tri_:5} | W {s.W:4} | K {s.K:4} | per add {s.steps / 30:.0f}')
def main():
    for label, D in (('reversible add (reversed out)', Adder6(state_first=False).build()), ('reversible add (in place)', AdderD().build())):
        s = F.Stats()
        for x, y in cases:
            v, st = run(D.start(x, y, 32)); assert v == V(D.expect(x, y, 32)); s = s + st
        mixline(label, s)
    P = ord_ltc(badd(W8_LTC)); s = F.Stats()
    from common import word
    for x, y in cases:
        v, st = run(A(P, P, word(x, 32), word(y, 32), L)); s = s + st
    mixline('ordinary add LTC', s)
big_stack(main)
