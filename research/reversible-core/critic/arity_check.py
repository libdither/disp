"""Critic probe: wire ends a footprint holds, over every rule and every set of products out."""
import os
import sys, os, itertools
sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', 'footprints'))
os.chdir(os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', 'footprints'))
from fpnet import FP_RULES, FP_DISMANTLE
worst = {}
for r in FP_RULES + FP_DISMANTLE:
    n = len(r.fresh)
    best = (0, None)
    for m in range(1 << n):
        out = {k for k in range(n) if m >> k & 1}
        ends = 2 + len(out)                     # two back wires, a child port per product out
        for sa, sb in r.pairs:
            def inside(s): return s[0] == 'X' and s[1] not in out
            def outside(s): return s[0] in ('C', 'P')
            def outp(s): return s[0] == 'X' and s[1] in out
            a, b = sa, sb
            if (outside(a) and inside(b)) or (outside(b) and inside(a)): ends += 1      # outside wire held for a product inside
            elif (outp(a) and inside(b)) or (outp(b) and inside(a)): ends += 1          # wire from a product out to one inside
            elif outside(a) and outside(b): ends += 2
        if ends > best[0]: best = (ends, sorted(out))
    worst[r.name] = best
for k, v in sorted(worst.items(), key=lambda x: -x[1][0]):
    print(f"{k:10s} max wire ends {v[0]:2d} with products out {v[1]}")
