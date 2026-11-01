"""Pattern kit: cells, reaction helpers. Matter sits only on (even, even) sites, so a block holds one site:
the even block has its E and S arms, the odd block its W and N arms."""
from physics import I, A

FIELDS = 'cls k f c h a t r ro w z m v d b s pc pk pf'.split()
VOID, HB, VB, PB, SB, NODE, EL, HUB = range(8)
GAP, LF, ST, FK, BY = range(5)  # node kinds; GAP reads as a wire, BY sends the reading east
_modes = {}
def M(name):
    return _modes.setdefault(name, len(_modes) + 1)
def nmodes():
    return len(_modes) + 1
RANGE = dict(cls=8, k=5, f=2, c=3, h=2, a=2, t=3, r=3, ro=7, w=2, z=2, m=64, v=17, d=2, b=2, s=2, pc=3, pk=5, pf=2)

def _lit(x):
    return I(x) if isinstance(x, int) else A(x)
def cell(**kw):
    if isinstance(kw.get('m'), str) and not kw['m'].startswith('_'): kw['m'] = M(kw['m'])
    elif isinstance(kw.get('m'), str): kw['m'] = kw['m'][1:]
    return tuple(_lit(kw.get(n, 0)) for n in FIELDS)
O = cell()
def void(w='w2'):
    return cell(w=w)
HEAD = dict(v='v', d='d', b='b', s='s')
def ball(arm, m, z=0, **kw):
    return cell(cls=HB if arm == 'E' else VB, m=m, z=z, **{**HEAD, **kw})
def nd(m=0, **kw):
    """a node; unnamed node fields are variables; give m for a held head"""
    base = dict(cls=NODE, k='k', f='f', c='c', h='h', a='a', t=0, r='r', ro='ro')
    base.update(kw)
    if m != 0:
        for n, x in HEAD.items(): base.setdefault(n, x)
        base['m'] = m
    return cell(**base)

class Rules:
    def __init__(s): s.rs, s.tags = [], {}
    def rx(s, a, b, tag=None):
        s.rs.append((a, b))
        s.tags[tag or s.tag] = s.tags.get(tag or s.tag, 0) + 1
    tag = 'misc'
    def even(s, n, n2, arm=None, bl=None, arm2=None, bl2=None, e=O, e2=O):
        """even block: site nw, with a ball on its E or S arm on either side"""
        def side(n, arm, bl, e):
            return (n, bl if arm == 'E' else e, bl if arm == 'S' else e, O)
        s.rx(side(n, arm, bl, e), side(n2, arm2, bl2, e))
    def odd(s, n, n2, arm=None, bl=None, arm2=None, bl2=None):
        """odd block: site se, with a ball on its W or N arm on either side"""
        def side(n, arm, bl):
            return (O, bl if arm == 'N' else O, bl if arm == 'W' else O, n)
        s.rx(side(n, arm, bl), side(n2, arm2, bl2))
R = Rules()
