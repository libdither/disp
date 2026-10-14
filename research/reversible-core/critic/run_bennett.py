"""Forward, read the answer out with a non-consuming reader, run every footprint back (critic's reader rules)."""
import os
import sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__))); sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', 'footprints')); sys.setrecursionlimit(100000)
from bennett_lib import Bennett
from fpnet import tern, to_tern
path = sys.argv[1]
for line in open(path):
    line = line.rstrip('\n')
    if not line: continue
    name, head, args, ans = line.split('|')
    head = tern(head); args = [tern(a) for a in args.split(',') if a]
    f = Bennett(dismantle=True); f.load_call(head, args)
    k0, l0 = f.snapshot(); loaded = len(k0)
    fr, fw = f.forward()
    nfp = len(f.fps)
    f.insert((f.out, 0))
    rr = 0
    while True:
        mv = f.reader_moves()
        if not mv: break
        rr += 1
        for m in mv: f.do(m)
    root = f.link[(f.out, 0)][0]
    copy = f.twin((root, 2))
    br, bw = f.backward('one')
    k1, l1 = f.snapshot()
    a, b = l1[(root, 0)], l1[(root, 1)]
    twins = {x for x, k in k1.items() if k in ('tL', 'tS', 'tF')}
    k1 = {x: k for x, k in k1.items() if x != root and x not in twins}
    l1 = {e: v for e, v in l1.items() if e[0] != root and v[0] != root and e[0] not in twins and v[0] not in twins}
    l1[a] = b; l1[b] = a
    print(f"{name:22s} loaded {loaded} | forward {fr} rounds, {sum(fw)} fires, widest {max(fw)} | footprints {nfp} -> {len(f.fps)} | "
          f"reader {rr} rounds | back {br} rounds | copy right {to_tern(copy) == ans} | rest = loaded net {k1 == k0 and l1 == l0} | twin size {len(twins)}", flush=True)
