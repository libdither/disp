"""Critic probe: clashes settled at random (as asynchronous local moves would) instead of collectors first."""
import os
import sys, os, random
sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', 'footprints')); sys.setrecursionlimit(100000)
os.chdir(os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', 'footprints'))
from fpnet import FpNet, FP_VALUES
from run import calls

class Random(FpNet):
    rng = random.Random(7)
    def moves(self):
        W = self.wanted()
        cand = []
        for h, k in self.kind.items():
            if k != 'H': continue
            y, pt = self.link[(h, 0)]
            ky = self.kind.get(y)
            if ky is None: continue
            ri = self.ix.get(('H', ky, pt))
            if ri is not None and (ky != 'H' or pt == 0):
                cand.append((h, y, ri))
        for m in W:
            y, pt = self.link[(m, 0)]
            ky = self.kind.get(y)
            if ky in FP_VALUES and pt == 0:
                cand.append((m, y, self.ix[(self.kind[m], ky, 0)]))
        self.rng.shuffle(cand)
        used, mv = set(), []
        for c, p, ri in cand:
            if c in used or p in used: continue
            used.add(c); used.add(p); mv.append((c, p, ri))
        return mv

pick = sys.argv[1:]
for name, head, args, ans in calls():
    if pick and not any(name.startswith(p) for p in pick): continue
    a = FpNet(dismantle=True); a.load_call(head, args); ar, aw = a.forward()
    b = Random(dismantle=True); b.load_call(head, args); br, bw = b.forward()
    print(f"{name[:28]:28s} collectors first: {ar} rounds {len(a.fps)} footprints | random clashes: {br} rounds {len(b.fps)} footprints ({len(b.fps)/len(a.fps):.2f}x) ok={b.readback() is not None}", flush=True)
