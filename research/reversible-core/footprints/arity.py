from fpnet import FpNet
from run import calls
from collections import Counter, defaultdict
mx_all = Counter(); mx_any = defaultdict(int)
for name, head, args, ans in calls():
    if name not in ("add 2 3", "fib 3", "relay_add (bit_tree 3 255) (bit_tree 3 1)", "isort [2, 1]"): continue
    f = FpNet(dismantle=True); f.load_call(head, args); f.forward()
    ends = Counter(k[0] for k in f.link if k[0] in f.fps)
    for fid, n in ends.items():
        rn = f.rules[f.fps[fid][0]].name
        mx_all[rn] = max(mx_all[rn], n)
    # during the way back, one product at a time
    while f.fps:
        ends = Counter(k[0] for k in f.link if k[0] in f.fps)
        for fid, n in ends.items():
            rn = f.rules[f.fps[fid][0]].name
            mx_any[rn] = max(mx_any[rn], n)
        act = []
        for fid, (ri, out, prod, _, _) in f.fps.items():
            if not out: act.append((fid, None)); continue
            fr = [k for k in out if f.fresh(fid, k)]
            if fr: act.append((fid, max(fr)))
        for fid, k in act:
            if k is None: f.unfire(fid)
            else: f.absorb(fid, k)
print("wire ends a footprint holds: with everything out | at most, on the way back")
for rn in sorted(mx_any, key=lambda r: -mx_any[r]):
    print(f"  {rn:9s} {mx_all[rn]:2d} | {mx_any[rn]:2d}")
