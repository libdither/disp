from fpnet import FpNet
from run import calls
from collections import Counter
tot = Counter()
for name, head, args, ans in calls():
    f = FpNet(dismantle=True); f.load_call(head, args); f.forward()
    tot.update(f.stats)
for r in FpNet(dismantle=True).rules:
    print(f"{r.name:10s} {tot[r.name]:8d}")
