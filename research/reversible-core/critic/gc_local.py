"""Critic probe: is running back the dead branches local? Replace the global keep-set with 'readers' footprints never take back'."""
import os
import sys, os
sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', 'footprints')); sys.setrecursionlimit(100000)
os.chdir(os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', 'footprints'))
from fpnet import FpNet
from run import calls
from uncomp import uncompute_garbage

def uncompute_local(f):
    rounds = 0
    while True:
        act = []
        for fid, (ri, out, prod, _, _) in f.fps.items():
            if f.rules[ri].c == 'N':            # the root reader's footprints hold the answer: frozen
                continue
            if not out:
                act.append(('u', fid, None)); continue
            for k in out:
                if f.fresh(fid, k):
                    act.append(('a', fid, k)); break
        if not act: break
        rounds += 1
        for kind, fid, k in act:
            if fid not in f.fps: continue
            if kind == 'u':
                if not f.fps[fid][1]: f.unfire(fid)
            elif f.fresh(fid, k): f.absorb(fid, k)
    return rounds

for name, head, args, ans in calls():
    a = FpNet(dismantle=True); a.load_call(head, args); a.forward(); uncompute_garbage(a)
    b = FpNet(dismantle=True); b.load_call(head, args); b.forward(); uncompute_local(b)
    same = a.snapshot() == b.snapshot() and set(a.fps) == set(b.fps)
    print(f"{name[:30]:30s} global keep-set leaves {len(a.fps)} footprints, frozen readers leave {len(b.fps)}; identical state: {same}; answer intact: {b.readback() is not None}")
