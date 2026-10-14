"""Critic probe: forward-forward clashes, an agent wanted by its front pair and by a collector on an output."""
import os
import sys, os
sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', 'footprints')); sys.setrecursionlimit(100000)
os.chdir(os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', 'footprints'))
from fpnet import FpNet, FP_VALUES, FP_OUT
from run import calls
from collections import Counter

class Count(FpNet):
    def moves(self):
        W = self.wanted()
        # front pairs that are ready (demand-wanted or not)
        front_ready = set()
        for m, k in self.kind.items():
            if k in FP_OUT:
                y, pt = self.link[(m, 0)]
                if self.kind.get(y) in FP_VALUES and pt == 0:
                    front_ready.add(m)
        hit = Counter()
        for h, k in self.kind.items():
            if k != 'H': continue
            y, pt = self.link[(h, 0)]
            ky = self.kind.get(y)
            if ky in FP_OUT and pt in FP_OUT[ky]:
                hit[y] += 1
        for y, n in hit.items():
            if y in front_ready:
                self.stats['clash front+collector' + (' (wanted)' if y in W else '')] += 1
            if n > 1:
                self.stats['clash two collectors on one agent'] += 1
        return super().moves()

tot = Counter()
for name, head, args, ans in calls():
    f = Count(dismantle=True); f.load_call(head, args); f.forward()
    c = {k: v for k, v in f.stats.items() if k.startswith('clash')}
    tot.update(c)
    print(f"{name[:30]:30s} {c}")
print('total', dict(tot))
