"""Critic probe: the footprint net with no demand controller (every ready pair fires)."""
import os
import sys, os
sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', 'footprints')); sys.setrecursionlimit(100000)
os.chdir(os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', 'footprints'))
from fpnet import FpNet, FP_OUT
from run import calls

class Eager(FpNet):
    def wanted(self):
        return {x for x, k in self.kind.items() if k in FP_OUT}

pick = sys.argv[1:]
for name, head, args, ans in calls():
    if pick and not any(name.startswith(p) for p in pick):
        continue
    lazy = FpNet(dismantle=True); lazy.load_call(head, args)
    lr, lw = lazy.forward()
    e = Eager(dismantle=True); e.load_call(head, args)
    cap = 20000
    er, ew = e.forward(max_rounds=cap)
    done = not e.moves() and not e.partial()
    got = e.readback()
    from fpnet import to_tern
    print(f"{name[:28]:28s} lazy: {lr} rounds {sum(lw)} fires | eager: {er} rounds {sum(ew)} fires "
          f"({sum(ew)/sum(lw):.1f}x) finished={done} answer_ok={got is not None and to_tern(got)==ans} footprints={len(e.fps)} live={len(e.kind)}", flush=True)
