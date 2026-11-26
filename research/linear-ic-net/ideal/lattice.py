"""How far apart things are when a net is laid out in a 3D grid of cells: a crude, stated model.

  - one agent per cell of Z^3; a wire may run anywhere and cross anything (3D), and its length is the
    Manhattan distance between its two cells
  - agents never move. A pair of agents joined principal to principal fires over the wire between them:
    the distance at that moment is how far a signal has to travel for that rewrite
  - the agents a rewrite makes go into the nearest free cells around the middle of the pair, the two cells
    the pair leaves among them. A cell further than the pair's own two is a blank fetched from that far
  - the starting net fills a cube, in the order the compiler emits it, along a path that snakes through
    the cube (so agents emitted one after the other sit side by side)

Reported for a run: rewrites; the travel distance of a rewrite (mean, 95th percentile, largest); how far
the blanks came from; the most agents alive at once and the side of the cube that would just hold them;
the bounding box the run really used; wire length per agent (mean over samples).

python lattice.py            the reflective jobs on both nets, then adds of growing size
"""
import sys, os, math
HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', 'nets'))
sys.setrecursionlimit(1000000)

R = 14
OFFSETS = sorted(((abs(x) + abs(y) + abs(z), x, y, z) for x in range(-R, R + 1) for y in range(-R, R + 1) for z in range(-R, R + 1)))
OFFSETS = [(x, y, z) for d, x, y, z in OFFSETS]


def placed(base):
    """the reducer class `base` (../nets/net.py's Net or one built on it), with every agent given a cell"""
    class Placed(base):
        def __init__(self, *a, **k):
            self.pos, self.cell = {}, {}
            self.anchor = (0, 0, 0)
            self.travel, self.fetch = [], []
            self.peak = 0
            self.box = [0, 0, 0, 0, 0, 0]
            self.wire_samples = []
            self.loading = True
            super().__init__(*a, **k)

        def free_near(self, c):
            cx, cy, cz = c
            for dx, dy, dz in OFFSETS:
                p = (cx + dx, cy + dy, cz + dz)
                if p not in self.cell: return p
            r = R + 1                                    # a full neighbourhood: walk outward along one axis
            while (cx + r, cy, cz) in self.cell: r += 1
            return (cx + r, cy, cz)

        def new(self, kind, *a, **k):
            g = super().new(kind, *a, **k)
            if kind not in ('ph', 'root'):
                p = self.snake() if self.loading else self.free_near(self.anchor)
                self.pos[g] = p
                self.cell[p] = g
                b = self.box
                for i in range(3):
                    if p[i] < b[2 * i]: b[2 * i] = p[i]
                    if p[i] > b[2 * i + 1]: b[2 * i + 1] = p[i]
                if not self.loading: self.fetch.append(sum(abs(p[i] - self.anchor[i]) for i in range(3)))
                if len(self.pos) > self.peak: self.peak = len(self.pos)
            return g

        side = 1
        def snake(self):
            """the next cell of a path that fills a cube of this side, turning back at every wall"""
            i, s = len(self.pos), self.side
            z, y, x = i // (s * s), (i // s) % s, i % s
            if z % 2: y = s - 1 - y
            if (y + z * (s % 2)) % 2: x = s - 1 - x
            return (x, y, z)

        def remove(self, a):
            p = self.pos.pop(a, None)
            if p is not None: del self.cell[p]
            super().remove(a)

        def fire(self, a, b):
            self.loading = False
            pa, pb = self.pos[a], self.pos[b]
            self.travel.append(abs(pa[0] - pb[0]) + abs(pa[1] - pb[1]) + abs(pa[2] - pb[2]))
            self.anchor = ((pa[0] + pb[0]) // 2, (pa[1] + pb[1]) // 2, (pa[2] + pb[2]) // 2)
            if len(self.travel) % self.sample_every == 0: self.wire_samples.append(self.wire_per_agent())
            super().fire(a, b)

        sample_every = 500

        def wire_per_agent(self):
            tot = 0
            for (g, i), (h, j) in self.mate.items():
                pg, ph = self.pos.get(g), self.pos.get(h)
                if pg is None or ph is None: continue
                tot += abs(pg[0] - ph[0]) + abs(pg[1] - ph[1]) + abs(pg[2] - ph[2])
            return tot / 2 / max(1, len(self.pos))

    return Placed


def pct(xs, q):
    if not xs: return 0
    s = sorted(xs)
    return s[min(len(s) - 1, int(q * len(s)))]


def measure(base, term, limit=100000000, seed=None):
    count = base()
    count.load(term)
    n = placed(base)(seed)
    n.side = max(1, math.ceil((len(count.kind) - 1) ** (1 / 3)))
    n.load(term)
    start_wire = n.wire_per_agent()
    status = n.run(limit)
    t, f = n.travel, n.fetch
    b = n.box
    return dict(status=status, rewrites=len(t), travel_mean=sum(t) / max(1, len(t)), travel_p95=pct(t, 0.95), travel_max=max(t, default=0),
                travel_sum=sum(t), fetch_mean=sum(f) / max(1, len(f)), fetch_max=max(f, default=0),
                start=len(count.kind) - 1, peak=n.peak, cube=math.ceil(n.peak ** (1 / 3)), box=(b[1] - b[0] + 1, b[3] - b[2] + 1, b[5] - b[4] + 1),
                wire0=start_wire, wire=sum(n.wire_samples) / max(1, len(n.wire_samples)))


def show(label, m):
    print(f"  {label:<46} {m['status']:<5} {m['rewrites']:>8} rewrites | travel mean {m['travel_mean']:5.1f} p95 {m['travel_p95']:>3} max {m['travel_max']:>3}"
          f" | blanks from {m['fetch_mean']:4.1f} (max {m['fetch_max']:>2}) | {m['start']:>5} agents at the start, peak {m['peak']:>6}, cube side {m['cube']:>2}, box {m['box']}"
          f" | wire per agent {m['wire0']:4.1f} at the start, {m['wire']:4.1f} running")


def main():
    import net as oldnet, check as oldcheck, ref
    import knet, kcheck
    q = oldcheck.quote
    print('reflective jobs:')
    show('old: size suc', measure(oldnet.Net, oldcheck.program('size suc')))
    show('candidate: sizeK suc', measure(knet.KNet, kcheck.program('sizeK suc')))
    show('old: interpret 1 + 1', measure(oldnet.Net, oldcheck.program('interp ' + q('add (suc zero) (suc zero)'))))
    show('candidate: interpT 1 + 1', measure(knet.KNet, kcheck.program("interpT '(add (suc zero) (suc zero))")))
    show('candidate: sizeK on the old size', measure(knet.KNet, kcheck.program("sizeK 'oldsize", {'oldsize': kcheck.old_code('size')})))
    show('candidate: interpT on itself', measure(knet.KNet, kcheck.program("interpT '(interpT '((\\x. x) (\\y. y)))")))
    print('the same λ-programs on either net (they use no reader): adds of growing size, queue order then a random order')
    for bits in (4, 16, 64, 256):
        a, b = (1 << bits) - 1, (1 << (bits - 1)) + 1
        for name in ('linear_tree_add', 'linear_list_add', 'ordinary_list_add'):
            form = 't' if 'tree' in name else 'c' if 'ordinary' in name else ''
            t = oldcheck.program(f'{name} {a}{form} {b}{form}')
            show(f'{name}, {bits} bits', measure(knet.KNet, t))
            if bits == 64: show(f'{name}, {bits} bits, random order', measure(knet.KNet, t, seed=1))


if __name__ == '__main__':
    main()
