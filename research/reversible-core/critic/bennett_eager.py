"""Critic probe: no demand controller at all (every ready pair fires), then Bennett's clean-up with readers."""
import os
import sys, os
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__))); sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', 'footprints')); sys.setrecursionlimit(100000)
os.chdir(os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', 'footprints'))
from bennett_lib import Bennett, to_tern, calls
from fpnet import FP_OUT

class EagerBennett(Bennett):
    def wanted(self):
        return {x for x, k in self.kind.items() if k in FP_OUT}

pick = sys.argv[1:]
for name, head, args, ans in calls():
    if pick and not any(name.startswith(p) for p in pick): continue
    f = EagerBennett(dismantle=True); f.load_call(head, args)
    k0, l0 = f.snapshot()
    fr, fw = f.forward(); nfp = len(f.fps)
    f.insert((f.out, 0)); rounds = 0
    while True:
        mv = f.reader_moves()
        if not mv: break
        rounds += 1
        for m in mv: f.do(m)
    root = f.link[(f.out, 0)][0]
    copy = f.twin((root, 2))
    br, bw = f.backward('one')
    print(f"{name[:24]:24s} eager forward {fr} rounds, {nfp} footprints -> {len(f.fps)} after reading ({rounds} rounds) and {br} rounds back; copy right {to_tern(copy) == ans}", flush=True)
