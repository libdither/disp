"""Critic probe: no demand controller (eager), then run back every branch that missed the answer."""
import os
import sys, os
sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', 'footprints')); sys.setrecursionlimit(100000)
os.chdir(os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', 'footprints'))
from fpnet import FpNet, FP_OUT
from run import calls
from uncomp import uncompute_garbage, answer_agents

class Eager(FpNet):
    def wanted(self):
        return {x for x, k in self.kind.items() if k in FP_OUT}

pick = sys.argv[1:]
for name, head, args, ans in calls():
    if pick and not any(name.startswith(p) for p in pick):
        continue
    out = []
    for cls in (FpNet, Eager):
        f = cls(dismantle=True); f.load_call(head, args)
        r, w = f.forward()
        fp0 = len(f.fps)
        br = uncompute_garbage(f)
        ok = f.readback() is not None
        out.append((r, fp0, len(f.fps), br, ok))
    (lr, l0, l1, lb, lok), (er, e0, e1, eb, eok) = out
    print(f"{name[:28]:28s} lazy: {lr} rounds, footprints {l0} -> {l1} after GC ({lb} back rounds) | "
          f"eager: {er} rounds, footprints {e0} -> {e1} after GC ({eb} back rounds) ok={lok and eok}", flush=True)
