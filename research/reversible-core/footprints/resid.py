import sys
from fpnet import FpNet, FP_PORTS
from run import calls
from uncomp import uncompute_garbage, answer_agents
from collections import Counter
for name, head, args, ans in calls():
    if sys.argv[1:] and name not in sys.argv[1:]: continue
    f = FpNet(dismantle=True); f.load_call(head, args); f.forward(); uncompute_garbage(f)
    keep = answer_agents(f)
    rooted = made = 0; kinds = Counter()
    for a, k in f.kind.items():
        if a in keep or k == 'Out': continue
        kinds[k] += 1
        if (a, len(FP_PORTS[k]) - 1) in f.link: made += 1
        else: rooted += 1
    print(f"{name[:40]:40s} beside the answer: loaded originals {rooted}, made by the run {made}, kinds {dict(kinds)}")
