"""Critic probe: Bennett's clean-up inside the footprint net, with a reader that does not consume.

Readers sit on wires and leave every value agent (and its back port) alone. Rules, each a bijection with
no footprint (the twin it makes is its own record, checked going back):
  split   Rd on the wire into F (or S): one Rd on each child wire
  leaf    Rd on the wire into L: Rdd there, holding a new twin L'
  join    Rdd on each child wire of F (or S): one Rdd on the wire into it, holding a new twin F' (S')
Run forward, read the answer out with readers (in parallel rounds), then run every footprint back.
"""
import os
import sys, os
sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', 'footprints')); sys.setrecursionlimit(100000)
os.chdir(os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', 'footprints'))
from fpnet import FpNet, FP_PORTS, FP_VALUES, to_tern
from run import calls

FP_PORTS['Rd'] = ['u', 'p']          # u: toward the consumer, p: the value's front port
FP_PORTS['Rdd'] = ['u', 'p', 'o']    # o: the twin of the subtree below
TW = {'L': 'tL', 'S': 'tS', 'F': 'tF'}
FP_PORTS['tL'] = ['p']; FP_PORTS['tS'] = ['p', 'c']; FP_PORTS['tF'] = ['p', 'l', 'r']   # twins: no back port

class Bennett(FpNet):
    def insert(self, end):
        """put a reader on the wire at endpoint `end` (a port facing a value's front)"""
        other = self.link.pop(end); del self.link[other]
        r = self.new('Rd')
        self.connect((r, 0), end); self.connect((r, 1), other)
        return r

    def reader_moves(self):
        mv, used = [], set()
        for r, k in list(self.kind.items()):
            if k == 'Rd':
                x, pt = self.link[(r, 1)]
                kx = self.kind.get(x)
                if kx in FP_VALUES and pt == 0 and x not in used and r not in used:
                    used |= {r, x}; mv.append(('split' if kx != 'L' else 'leaf', r, x))
        for x, kx in list(self.kind.items()):
            if kx not in ('S', 'F') or x in used: continue
            rs = []
            for q in range(1, len(FP_PORTS[kx]) - 1):
                y, pt = self.link[(x, q)]
                if self.kind.get(y) == 'Rdd' and pt == 0 and y not in used: rs.append(y)
            if len(rs) == len(FP_PORTS[kx]) - 2:
                used |= {x, *rs}; mv.append(('join', x, rs))
        return mv

    def do(self, m):
        if m[0] == 'split':
            _, r, x = m
            up = self.link.pop((r, 0)); del self.link[up]
            del self.link[(r, 1)], self.link[(x, 0)]
            del self.kind[r]
            self.connect(up, (x, 0))
            for q in range(1, len(FP_PORTS[self.kind[x]]) - 1):
                self.insert((x, q))
        elif m[0] == 'leaf':
            _, r, x = m
            self.kind[r] = 'Rdd'
            t = self.new('tL'); self.connect((r, 2), (t, 0))
        else:
            _, x, rs = m
            kx = self.kind[x]
            t = self.new(TW[kx])
            for q, rr in enumerate(rs, start=1):
                below, _ = self.link[(rr, 1)]
                tw = self.link.pop((rr, 2)); del self.link[tw]
                self.connect((t, q), tw)
                del self.link[(rr, 0)], self.link[(x, q)]
                bl = self.link.pop((rr, 1)); del self.link[bl]
                self.connect((x, q), bl)
                del self.kind[rr]
            up = self.link.pop((x, 0)); del self.link[up]
            r = self.new('Rdd')
            self.connect((r, 0), up); self.connect((r, 1), (x, 0)); self.connect((r, 2), (t, 0))

    def twin(self, end):
        y, pt = self.link[end]
        k = self.kind[y]
        base = {'tL': 'L', 'tS': 'S', 'tF': 'F'}[k]
        return (base, *[self.twin((y, q)) for q in range(1, len(FP_PORTS[k]))])

