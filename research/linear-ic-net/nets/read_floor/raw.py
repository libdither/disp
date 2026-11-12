"""Every charge key of a run, largest first: python raw.py '<program>' '<input>'"""
import sys
from tagnet import parse_defs, term, label, run
from defs import SRC

defs = parse_defs(SRC)
prog, inp = sys.argv[1], sys.argv[2]
t = (term(defs, prog), label(term(defs, inp), 'IN:'))
n, root = run(t)
reads = sum(v for k, v in n.acct.items() if k[0].startswith('R⋈'))
print('rewrites', n.stats['interactions'], 'reads', reads)
for k, v in n.acct.most_common():
    print(f'{v:7} {v / reads:8.1f}  {k}')
