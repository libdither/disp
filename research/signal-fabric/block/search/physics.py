"""Sparse block physics (mirror of ../blocks.bend, on an unbounded plane). A cell is a tuple of small Nats;
a pattern field is ('I', n) or ('A', v); a reaction is (l, r) with l, r = (nw, ne, sw, se) of cell patterns.
Blocks have corners at even (x, y) on even ticks, at odd (x, y) on odd ticks. The plane is unbounded to the right
and down, so matter can always grow there; a non-empty cell at x < 0 or y < 0 has left, and is logged."""

def I(n): return ('I', n)
def A(v): return ('A', v)

def _fit(pat, block):
    binds = {}
    for pc, c in zip(pat, block):
        if len(pc) != len(c): return None
        for f, n in zip(pc, c):
            if f[0] == 'I':
                if f[1] != n: return None
            else:
                binds[f[1]] = n
    return binds

def _fill(pat, binds):
    return tuple(tuple(f[1] if f[0] == 'I' else binds[f[1]] for f in pc) for pc in pat)

class Rule:
    def __init__(self, reactions, void):
        self.rs, self.void, self.cache = reactions, tuple(void), {}
        self.used = {}
    def __call__(self, block):
        got = self.cache.get(block)
        if got is None:
            got = block
            for i, (l, r) in enumerate(self.rs):
                b = _fit(l, block)
                if b is not None:
                    got = _fill(r, b); self.used[i] = True; break
                b = _fit(r, block)
                if b is not None:
                    got = _fill(l, b); self.used[i] = True; break
            self.cache[block] = got
        return got

class World:
    """cells: {(x, y): cell}, empties absent. step() runs one tick."""
    def __init__(self, cells, rule):
        self.cells, self.rule, self.void = dict(cells), rule, rule.void
        self.phase, self.t, self.outs = 0, 0, []
        self.dirty = [set(), set()]
        for (x, y) in self.cells: self._mark(x, y)
    def _mark(self, x, y):
        self.dirty[0].add((x & ~1, y & ~1))
        self.dirty[1].add((((x - 1) & ~1) + 1, ((y - 1) & ~1) + 1))
    def still(self):
        return not self.dirty[0] and not self.dirty[1]
    def step(self):
        """one tick; the cells whose value changed, as a list of (x, y)"""
        ph, cells, void, rule = self.phase, self.cells, self.void, self.rule
        todo, self.dirty[ph] = self.dirty[ph], set()
        changed = []
        get = cells.get
        for (x, y) in todo:
            spots = ((x, y), (x + 1, y), (x, y + 1), (x + 1, y + 1))
            block = tuple(get(s, void) for s in spots)
            new = rule(block)
            if new is block or new == block: continue
            for s, a, b in zip(spots, block, new):
                if a != b:
                    if b == void: del cells[s]
                    else: cells[s] = b
                    changed.append(s)
        for (x, y) in changed:
            self._mark(x, y)
            if x < 0 or y < 0:
                if (x, y) in cells:
                    self.outs.append((self.t, x, y, cells.pop((x, y))))
        self.phase, self.t = 1 - ph, self.t + 1
        return changed

# Laws on reactions: these hold for every block, whatever a run does.
def fits_fields(rs, ranges):
    """every cell of every side has the design's fields, values in range; a variable moves only between equal ranges"""
    k = len(ranges)
    for l, r in rs:
        at = {}
        for side in (l, r):
            for c in side:
                if len(c) != k: return False
                for f, rg in zip(c, ranges):
                    if f[0] == 'I' and not 0 <= f[1] < rg: return False
                    if f[0] == 'A':
                        if at.setdefault(f[1], rg) != rg: return False
    return True

def _vars(side):
    return [f[1] for c in side for f in c if f[0] == 'A']

def linear(rs):
    for l, r in rs:
        a, b = _vars(l), _vars(r)
        if len(set(a)) != len(a) or set(a) != set(b) or len(a) != len(b): return False
    return True

def void_stays(rs, void):
    q = tuple(tuple(void) for _ in range(4))
    return all(_fit(l, q) is None and _fit(r, q) is None for l, r in rs)

def one_partner(rs):
    """two sides that fit a block in common turn it into the same block; a list of the pairs that break this"""
    sides = []
    for i, (l, r) in enumerate(rs):
        sides.append(([f for c in l for f in c], [f for c in r for f in c], 2 * i))
        sides.append(([f for c in r for f in c], [f for c in l for f in c], 2 * i + 1))
    # index on the first literal fields to skip most pairs
    bad = []
    n = len(sides)
    for ia in range(n):
        s1, t1, i = sides[ia]
        for ib in range(ia + 1, n):
            s2, t2, j = sides[ib]
            ok = True
            for x, y in zip(s1, s2):
                if x[0] == 'I' and y[0] == 'I' and x[1] != y[1]:
                    ok = False; break
            if not ok: continue
            # the shared block: a literal where either has one, else a free value named by place
            shared = [x if x[0] == 'I' else (y if y[0] == 'I' else ('F', p)) for p, (x, y) in enumerate(zip(s1, s2))]
            v1 = {f[1]: p for p, f in enumerate(s1) if f[0] == 'A'}
            v2 = {f[1]: p for p, f in enumerate(s2) if f[0] == 'A'}
            o1 = [f if f[0] == 'I' else shared[v1[f[1]]] for f in t1]
            o2 = [f if f[0] == 'I' else shared[v2[f[1]]] for f in t2]
            if o1 != o2: bad.append((i, j))
    return bad

def bad_self_fits(rs):
    """a reaction whose own two sides overlap must also agree: included in one_partner via the pair (l, r)"""
    return [b for b in one_partner(rs) if b[0] // 2 == b[1] // 2]
