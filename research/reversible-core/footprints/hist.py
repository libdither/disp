"""What the footprints at the answer record, by kind of rule."""
import sys
from fpnet import FpNet
from run import calls
from collections import Counter
CAT = {'A': 'apply', 'T': 'choose', 'Q': 'choose', 'N': 'read out', 'D': 'copy', 'R': 'relay', 'H': 'take apart'}
for name, head, args, ans in calls():
    if sys.argv[1:] and name not in sys.argv[1:]:
        continue
    f = FpNet(dismantle=True)
    f.load_call(head, args)
    loaded = len(f.kind)
    r, w = f.forward()
    cat = Counter()
    erased_copies = 0
    uncopy = 0
    for fid, (ri, out, prod, cid, pid) in f.fps.items():
        rule = f.rules[ri]
        cat[CAT[rule.c]] += 1
        if rule.c == 'H' and rule.p in ('L', 'S', 'F'):
            # did this holder take apart a value a copy made?
            e = f.link.get((fid, ('Pb',)))
            if e is not None and e[0] in f.fps and f.rules[f.fps[e[0]][0]].c == 'D':
                erased_copies += 1
        if rule.c == 'H' and rule.p == 'D':
            uncopy += 1
    tot = sum(cat.values())
    print(f"{name[:40]:40s} loaded {loaded:5d} footprints {tot:6d} ({tot/loaded:4.1f}x) "
          + ' '.join(f"{k} {100*v/tot:3.0f}%" for k, v in sorted(cat.items(), key=lambda x: -x[1]))
          + f" | takes apart a copy {erased_copies} | un-copies before copying {uncopy}")
