"""Critic probe: the reader's moves run backward (unjoin, unleaf, unsplit) undo a whole read exactly."""
import os
import sys, os
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__))); sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', 'footprints')); sys.setrecursionlimit(100000)
os.chdir(os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', 'footprints'))
from bennett_lib import Bennett, calls, FP_PORTS
from fpnet import FP_VALUES
BASE = {'tL': 'L', 'tS': 'S', 'tF': 'F'}

class Inv(Bennett):
    def inverse_moves(self):
        mv, used = [], set()
        for r, k in list(self.kind.items()):
            if r in used: continue
            if k == 'Rdd':
                x, pt = self.link[(r, 1)]; t, tp = self.link[(r, 2)]
                kx, kt = self.kind.get(x), self.kind.get(t)
                if pt == 0 and tp == 0 and kt in BASE and BASE[kt] == kx and x not in used and t not in used:
                    used |= {r, x, t}; mv.append(('unleaf' if kx == 'L' else 'unjoin', r, x, t))
        for x, kx in list(self.kind.items()):
            if kx not in ('S', 'F') or x in used: continue
            rs = []
            for q in range(1, len(FP_PORTS[kx]) - 1):
                y, pt = self.link[(x, q)]
                if self.kind.get(y) == 'Rd' and pt == 0 and y not in used: rs.append(y)
            if len(rs) == len(FP_PORTS[kx]) - 2:
                used |= {x, *rs}; mv.append(('unsplit', x, rs))
        return mv

    def undo(self, m):
        if m[0] == 'unleaf':
            _, r, x, t = m
            del self.link[(r, 2)], self.link[(t, 0)]; del self.kind[t]
            self.kind[r] = 'Rd'
        elif m[0] == 'unjoin':
            _, r, x, t = m
            kx = self.kind[x]
            up = self.link.pop((r, 0)); del self.link[up]
            del self.link[(r, 1)], self.link[(x, 0)], self.link[(r, 2)], self.link[(t, 0)]
            del self.kind[r]
            self.connect(up, (x, 0))
            for q in range(1, len(FP_PORTS[kx]) - 1):
                tw = self.link.pop((t, q)); del self.link[tw]
                rr = self.insert((x, q)); self.kind[rr] = 'Rdd'
                self.connect((rr, 2), tw)
            del self.kind[t]
        else:
            _, x, rs = m
            for q, rr in enumerate(rs, start=1):
                bl = self.link.pop((rr, 1)); del self.link[bl]
                del self.link[(rr, 0)], self.link[(x, q)]
                del self.kind[rr]
                self.connect((x, q), bl)
            self.insert(self.link[(x, 0)])

for name, head, args, ans in calls():
    f = Inv(dismantle=True); f.load_call(head, args); f.forward()
    s0 = f.snapshot()
    r0 = f.insert((f.out, 0))
    s1 = f.snapshot()
    fwd = 0
    while True:
        mv = f.reader_moves()
        if not mv: break
        fwd += 1
        for m in mv: f.do(m)
    back = 0
    while True:
        mv = f.inverse_moves()
        if not mv: break
        back += 1
        for m in mv: f.undo(m)
    k1, l1 = f.snapshot()
    # one Rd left on the root wire, maybe under another id: compare up to that id
    rd = [x for x, k in k1.items() if k == 'Rd']
    ok = len(rd) == 1 and len(k1) == len(s1[0]) and {k for x, k in k1.items() if x not in rd} == {k for x, k in s1[0].items() if x != r0}
    # splice the reader out and compare exactly with the net before the read
    if len(rd) == 1:
        r = rd[0]; a, b = l1[(r, 0)], l1[(r, 1)]
        kk = {x: k for x, k in k1.items() if x != r}
        ll = {e: v for e, v in l1.items() if e[0] != r and v[0] != r}; ll[a] = b; ll[b] = a
        ok = (kk, ll) == s0
    print(f"{name[:30]:30s} read in {fwd} rounds, unread in {back} rounds, back exactly to the net before reading: {ok}", flush=True)
