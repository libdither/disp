"""Pattern kit for the polymer family: cells, head moves, reaction list."""
from physics import I, A
FIELDS = 'k m e a r u h st f g o'.split()
K = dict(zip('void PE PS PN L S F FL FX FY W0 WB OUT'.split(), range(13)))
RANGE = dict(k=13, m=4, e=4, a=2, r=40, u=2, h=3, st=128, f=2, g=2, o=7)
def lit(v, name): return v if isinstance(v, tuple) else (I(v) if isinstance(v, int) else A(v))
def cell(p='', head=None, h=None, **kw):
    """fields not given are variables named p+field; no head unless head= a state (h=1 hops by the shared hop, h=2 has its own moves)"""
    out = {}
    for n in FIELDS:
        if n in kw: out[n] = lit(kw[n], n)
        elif n in ('h', 'st', 'f', 'g', 'o'): out[n] = I(0)
        else: out[n] = A(p + n)
    if head is None and h is not None: out['h'] = lit(h, 'h')
    if head is not None:
        out['h'], out['st'] = I(h if h else 1), I(head)
        out['f'], out['g'], out['o'] = lit(kw.get('f', 'hf'), 'f'), lit(kw.get('g', 'hg'), 'g'), lit(kw.get('o', 'ho'), 'o')
    return tuple(out[n] for n in FIELDS)
VOID = tuple(I(0) for _ in FIELDS)
class Rules:
    def __init__(s): s.rs, s.names, s.n = [], {}, 0
    def st(s, name):
        if name not in s.names:
            s.n += 1; s.names[name] = s.n
        return s.names[name]
    def add(s, l, r): s.rs.append((tuple(l), tuple(r)))
    # upper pair, below free: site left, spacer right
    def up(s, a, b, a2, b2, sw=None, se=None, sw2=None, se2=None):
        sw = sw or cell('below1'); se = se or cell('below2')
        s.add((a, b, sw, se), (a2, b2, sw2 or sw, se2 or se))
    def low(s, a, b, a2, b2):
        u, v = cell('above1'), cell('above2')
        s.add((u, v, a, b), (u, v, a2, b2))
    def both(s, a, b, a2, b2):
        s.up(a, b, a2, b2); s.low(a, b, a2, b2)
