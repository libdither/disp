"""Critic probe: at the answer, unbiased emit/absorb (the design biases only meet/part) makes the answer flicker."""
import os
import sys, os, random
sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', 'footprints')); sys.setrecursionlimit(100000)
os.chdir(os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', 'footprints'))
from fpnet import FpNet
from run import calls
rng = random.Random(1)
for name, head, args, ans in calls():
    if name not in ('add 2 3', 'size [5, 6, 7]', 'is_even 3'): continue
    f = FpNet(dismantle=True); f.load_call(head, args); f.forward()
    # the answer's agents are fresh products; collect footprints that have fresh products or products inside
    def toggles():
        out = []
        for fid, (ri, o, prod, _, _) in f.fps.items():
            n = len(f.rules[ri].fresh)
            for k in range(n):
                if k in o and f.fresh(fid, k): out.append(('a', fid, k))
                elif k not in o and o: out.append(('e', fid, k))   # emit back (never from an empty footprint: that would be a part's neighbour)
        return out
    whole = 0; samples = 0
    for step in range(200000):
        t = toggles() if step % 50 == 0 else t
        kind, fid, k = rng.choice(t)
        if fid not in f.fps: continue
        o = f.fps[fid][1]
        if kind == 'a' and k in o and f.fresh(fid, k) and len(o) > 1: f.absorb(fid, k)
        elif kind == 'e' and k not in o: f.emit(fid, k)
        if step % 50 == 49:
            t = toggles()
            samples += 1; whole += f.readback() is not None
    print(f"{name}: answer fully out in {whole}/{samples} samples of unbiased emit/absorb at the end", flush=True)
