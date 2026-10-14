import sys
from fpnet import FpNet, CurNet
from run import calls
def peak(net, fp):
    best = bestlive = 0
    rounds = 0
    while True:
        if fp:
            mv = net.moves(); em = net.partial()
            if not mv and not em: break
            for f, k in em: net.emit(f, k)
            for c, p, ri in mv: net.fire(c, p, ri)
            live = len(net.kind); tot = live + len(net.fps)
        else:
            before = len(net.kind)
            r, w = net.forward(max_rounds=1)
            if r == 0: break
            live = tot = len(net.kind)
        best = max(best, tot); bestlive = max(bestlive, live); rounds += 1
    return best, bestlive
for name, head, args, ans in calls():
    if sys.argv[1:] and name not in sys.argv[1:]: continue
    c = CurNet(); c.load_call(head, args); cp, _ = peak(c, False)
    f = FpNet(dismantle=True); f.load_call(head, args); ft, fl = peak(f, True)
    print(f"{name[:40]:40s} current peak agents {cp:6d} | footprint net: peak live {fl:6d} ({fl/cp:4.2f}x), with footprints {ft:6d} ({ft/cp:5.1f}x)")
