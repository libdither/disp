"""dbg.py: print one_partner failures by reaction with field names"""
import sys, os
sys.path.insert(0, os.path.abspath('.')); sys.path.insert(0, os.path.abspath('arcs'))
import physics as P, design as D
from kit import FIELDS
def show(c):
    return '{' + ' '.join('%s=%s' % (n, f[1]) for n, f in zip(FIELDS, c) if f != ('I', 0)) + '}'
def side(s): return ' | '.join(show(c) for c in s)
for i, j in P.one_partner(D.REACTIONS)[:6]:
    for q in (i, j):
        l, r = D.REACTIONS[q // 2]
        print(q // 2, q % 2, side(l), '<->', side(r))
    print()
