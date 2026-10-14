"""Reversible unary add (a, b) -> (a, a+b) with one extra mediator, the tail copier G."""
from fpnet import FpNet, FP_PORTS, FP_OUT, R_, index_rules, to_tern
from collections import Counter
FP_PORTS['G'] = ['p', 'o1', 'o2', 't', 'b']
FP_OUT['G'] = (1, 2)
G_RULES = [
    R_('G', 'L', ['L', 'R'], "0.p-C.o1 1.p-C.t 1.o-C.o2"),
    R_('G', 'S', ['S', 'S', 'G'], "0.p-C.o1 1.p-C.o2 2.p-P.c 2.o1-0.c 2.o2-1.c 2.t-C.t"),
    R_('G', 'F', ['F', 'F', 'D', 'G'], "0.p-C.o1 1.p-C.o2 2.p-P.l 2.o1-0.l 2.o2-1.l 3.p-P.r 3.o1-0.r 3.o2-1.r 3.t-C.t"),
]
def nat(n):
    t = ('L',)
    for _ in range(n): t = ('F', ('L',), t)
    return t
def size(t): return 1 + sum(size(c) for c in t[1:])
def run(a, b, readers=True):
    f = FpNet(dismantle=True)
    f.rules = f.rules + G_RULES
    f.ix = index_rules(f.rules)
    g = f.new('G')
    f.connect((g, 0), f.build(nat(a)))
    f.connect((g, 3), f.build(nat(b)))
    outs = []
    for port in (1, 2):
        n = f.new('N'); f.connect((n, 0), (g, port))
        o = f.new('Out'); f.connect((n, 1), (o, 0)); outs.append(o)
    k0, l0 = f.snapshot()
    r, w = f.forward()
    res = []
    for o in outs:
        y, pt = f.link[(o, 0)]; res.append(f.read(y, pt))
    ok = res[0] == nat(a) and res[1] == nat(a + b)
    by = Counter(f.rules[v[0]].name for v in f.fps.values())
    br, bw = f.backward('one')
    k1, l1 = f.snapshot()
    return dict(a=a, b=b, ok=ok, rounds=r, fires=sum(w), footprints=sum(by.values()),
                by=dict(by), back_rounds=br, exact=(k0 == k1 and l0 == l1))
for a, b in [(0, 0), (0, 3), (1, 0), (2, 3), (5, 4), (10, 1)]:
    print(run(a, b))
