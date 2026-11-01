"""The laws a block design keeps, independent of how it works. A design is a module with:

  NAME, RANGES (each field's range), REACTIONS (physics.py's format)
  lay(tree) -> Lay(cells={(x, y): cell}, root=(x, y), enter=(dx, dy))
      the one layout, used for program and data alike
  link(dp, dx) -> Link(off_p, off_x, cells, port)
      how a run starts, given only each layout's Dims (w, h, root, enter), never its contents.
      off_p, off_x are even offsets; cells are whatever joins the two (the head among them); port is the column things leave by
  node(cell, step) -> what a cell reads as when the reading steps into it by step (dx, dy):
      ('pass', d) | ('leaf',) | ('stem', d) | ('fork', dl, dr) | None. dr may be None: the right part starts
      where the left part's reading ended (for a tree written along a strand). ('leaf', d) gives where a strand goes on.
  optional: rot(cell), mir(cell): the cell seen after turning the plane 90° clockwise / mirroring left-right, for the graded symmetry check
  optional: SHAPES: how many reactions up to symmetry and token names, if the generator knows

Each law says what it restricts and what it leaves free.

HARD, on the rule (every block):
  fits_fields  Restricts: finitely many cells; nothing holds a growing counter. Free: how many fields, how large (a score).
  linear       Restricts: no value copied or dropped unseen. Free: where values move.
  undoes_itself (one_partner) Restricts: a tick is undone by the same tick; no information lost. Free: what reacts, how many reactions.
  void_stays   Restricts: empty space does nothing alone. Free: matter acting on the empty cells next to it.

HARD, on matter:
  same_matter  lay is one function and link never sees the contents, so a program is on the grid as a tree and there is no compile step.
               Restricts: no tape made from the program, no layout chosen for a program. Free: geometry, and any fixed head or joining cells.
  local_reading  The harness reads, walking the table node(cell, step): a cell's meaning depends on that cell and the way in, nothing else.
               Restricts: the tree cannot be hidden in far-away state or computed by the reading. Free: cell format, wires or none, directions.
  lays_small   every tree of at most 6 nodes reads back, and rests under both ticks. Free: the layout.
  compact_small  a tree of at most 8 nodes has at most 6 cells of matter per node. Restricts: room kept in the layout for later growth.
               Free: any layout within that; bigger trees are only scored (some layouts are not linear).

HARD, on every run of a rung:
  reads        the data reads as x at the start and as some tree at every tick. Restricts: the tree is always physically there. Free: how many ticks a move takes.
  moves_only   the reading changes only by swap, cross, sink, stem, unit, rot (either way), any number at once at different nodes.
               Restricts: nothing but local tree edits shows. Free: which, when, how many in a tick. rot is allowed since tri itself rotates.
  program_kept the program reads as p at the start and at the end. Restricts: a program is not used up. Free: it may move or change in the middle.
  agrees       the run goes still within the limit and the data reads as the calculus's answer. (A reversible grid goes still from motion only
               if what moved leaves, so this also says every moving thing leaves or settles.) Restricts: meaning. Free: time, space.
  sealed       things leave only upward from the port column. Restricts: no dumping leftovers out the sides. Free: what leaves.
  clean        everything that leaves is the same on every successful run of the design, whatever p and x. Restricts: no garbage: no
               history comes out, so the only output of a run is the answer. Free: how the head looks, and geometry left in the grid (scored).

GRADED, reported, not required; each could turn hard once a design shows what it costs:
  refuses      on a stuck run: goes still, data and program as they were, and what left differs from a success. Why not hard: a design might
               legitimately report stuck in other ways; but this is the cleanest answer and reversible walkers may get it for one reaction.
  undo_exact   p ; p† gives back the data's exact cells. Why not hard: it rules out slack that settles wherever it likes.
  mirror_trace the data's readings over a run of p† on y are those of p on x, reversed. Why not hard: parallel designs may interleave differently.
  never_moves  the program's reading never changes during a run.
  symmetric    rule commutes with turning and mirroring blocks. Why not hard: a favoured direction may be worth its savings.
  matter_counted  every reaction keeps the number of non-empty cells. Why not hard: forces slack to travel from somewhere; test what it costs.
  scores       rung reached; reactions; shapes; cell states; matter per node; ticks on a ladder of sizes.
"""
from collections import namedtuple
import importlib, json, sys, time
import calculus as C
import physics as P

Lay = namedtuple('Lay', 'cells root enter')
Dims = namedtuple('Dims', 'w h root enter')
Link = namedtuple('Link', 'off_p off_x cells port')

def dims(l):
    xs = [x for x, _ in l.cells] or [0]
    ys = [y for _, y in l.cells] or [0]
    return Dims(max(xs) + 1, max(ys) + 1, l.root, l.enter)

class Reader:
    """The harness's reading: a walk over the table, nothing else."""
    def __init__(self, D):
        self.node, self.void = D.node, tuple(0 for _ in D.RANGES)
        self.sigs = {}
    def sig(self, cell):
        s = self.sigs.get(cell)
        if s is None:
            dirs = [(a, b) for a in (-2, -1, 0, 1, 2) for b in (-2, -1, 0, 1, 2)]  # long steps too: a table may answer by the step it arrives with
            s = self.sigs[cell] = tuple(self.node(cell, d) for d in dirs)
        return s
    def read(self, cells, at, step, fuel=None):
        fuel = [fuel if fuel is not None else 400 + 8 * len(cells)]
        try:
            t, _ = self._walk(cells, at, step, fuel)
        except _NoRead:
            return None
        return t
    def _walk(self, cells, at, step, fuel):
        while True:
            fuel[0] -= 1
            if fuel[0] < 0: raise _NoRead()
            n = self.node(cells.get(at, self.void), step)
            if n is None: raise _NoRead()
            k = n[0]
            if k == 'pass':
                step = n[1]; at = (at[0] + step[0], at[1] + step[1]); continue
            if k == 'leaf':
                return C.L, (at, n[1] if len(n) > 1 else None)
            if k == 'stem':
                d = n[1]
                c, e = self._walk(cells, (at[0] + d[0], at[1] + d[1]), d, fuel)
                return ('S', c), e
            if k == 'fork':
                dl, dr = n[1], n[2]
                a, e = self._walk(cells, (at[0] + dl[0], at[1] + dl[1]), dl, fuel)
                if dr is None:
                    (e_at, e_d) = e
                    if e_d is None: raise _NoRead()
                    b, e2 = self._walk(cells, (e_at[0] + e_d[0], e_at[1] + e_d[1]), e_d, fuel)
                else:
                    b, e2 = self._walk(cells, (at[0] + dr[0], at[1] + dr[1]), dr, fuel)
                return ('F', a, b), e2
            raise _NoRead()

class _NoRead(Exception): pass

def matter(cells, void):
    return sum(1 for c in cells.values() if c != void)

def start(D, p, x):
    lp, lx = D.lay(p), D.lay(x)
    lk = D.link(dims(lp), dims(lx))
    assert all(v % 2 == 0 for v in lk.off_p + lk.off_x), 'link offsets must be even'
    cells = {}
    for (l, off) in ((lp, lk.off_p), (lx, lk.off_x)):
        for (a, b), c in l.cells.items():
            cells[(a + off[0], b + off[1])] = c
    for s, c in lk.cells.items():
        assert s not in cells, 'link cell on a layout'
        cells[s] = c
    rp = (lp.root[0] + lk.off_p[0], lp.root[1] + lk.off_p[1])
    rx = (lx.root[0] + lk.off_x[0], lx.root[1] + lk.off_x[1])
    return cells, rp, lp.enter, rx, lx.enter, lk.port

class Result(dict):
    __getattr__ = dict.get

def run(D, p, x, rule=None, reader=None, limit=200000, frames=None, watch=True):
    """One run. Reads only when a changed cell's reading changed, so tokens cost nothing."""
    rule = rule or P.Rule(D.REACTIONS, [0] * len(D.RANGES))
    reader = reader or Reader(D)
    cells, rp, ep, rx, ex, port = start(D, p, x)
    w = P.World(cells, rule)
    first_x, first_p = reader.read(w.cells, rx, ex), reader.read(w.cells, rp, ep)
    now_x, now_p = first_x, first_p
    res = Result(first_x=first_x, first_p=first_p, reads=first_x is not None, moves=True, never_moves=True, trace=[first_x], port=port)
    if frames is not None: frames.append(dict(w.cells))
    sig, void = reader.sig, w.void
    before = {}
    while not w.still() and w.t < limit:
        snap = w.cells
        ph = w.phase
        prev = {}
        # watch cells' reading: compare signature before and after by reading the changed cells' old values
        old = _peek(w, ph)
        changed = w.step()
        if frames is not None: frames.append(dict(w.cells))
        if watch and any(sig(old.get(s, void)) != sig(w.cells.get(s, void)) for s in changed):
            nx, np_ = reader.read(w.cells, rx, ex), reader.read(w.cells, rp, ep)
            if nx is None: res['reads'] = False
            if nx is not None and now_x is not None and not C.moved(nx, now_x):
                res['moves'] = False; res.setdefault('bad_move', (w.t, C.show(now_x), C.show(nx)))
            if np_ != now_p: res['never_moves'] = False
            if nx != now_x and nx is not None: res['trace'].append(nx)
            now_x, now_p = nx, np_
    res.update(t=w.t, still=w.still(), outs=list(w.outs), cells=w.cells, world=w)
    res['x_end'] = reader.read(w.cells, rx, ex)
    res['p_end'] = reader.read(w.cells, rp, ep)
    res['sealed'] = all(oy == -1 and ox == port for _, ox, oy, _ in w.outs)
    res['out_sig'] = tuple(c for _, _, _, c in w.outs)
    return res

def _peek(w, ph):
    """the values of every cell the next tick may change"""
    out, get = {}, w.cells.get
    for (x, y) in w.dirty[ph]:
        for s in ((x, y), (x + 1, y), (x, y + 1), (x + 1, y + 1)):
            c = get(s)
            if c is not None: out[s] = c
    return out

# Laws
def rule_laws(D):
    rs, ranges = D.REACTIONS, D.RANGES
    bad = P.one_partner(rs)
    return dict(fits_fields=P.fits_fields(rs, ranges), linear=P.linear(rs), undoes_itself=not bad, void_stays=P.void_stays(rs, tuple(0 for _ in ranges))), bad

def matter_laws(D):
    rule, reader = P.Rule(D.REACTIONS, [0] * len(D.RANGES)), Reader(D)
    fails = []
    for t in C.trees(6):
        l = D.lay(t)
        got = reader.read(l.cells, l.root, l.enter)
        if got != t: fails.append(('lays: reads', C.show(t), got and C.show(got))); continue
        w = P.World(l.cells, rule)
        for _ in range(4): w.step()
        if w.cells != l.cells or w.outs: fails.append(('lays: rests', C.show(t)))
    worst = 0
    for t in C.trees(8):
        worst = max(worst, matter(D.lay(t).cells, ()) / C.size(t))
    if worst > 6: fails.append(('compact_small', worst))
    return fails, worst

def check_rung(D, name, ps, xs, limit=200000, stop_after=8):
    rule, reader = P.Rule(D.REACTIONS, [0] * len(D.RANGES)), Reader(D)
    fails, n, tmax, tsum, sigs = [], 0, 0, 0, set()
    for p in ps:
        for x in xs:
            try:
                want = C.run(p, x)
            except C.TooDeep:
                continue
            if want is None: continue
            n += 1
            r = run(D, p, x, rule, reader, limit)
            why = []
            if r.first_x != x: why.append('reads at start')
            if r.first_p != p: why.append('program at start')
            if not r.reads: why.append('reads')
            if not r.moves: why.append('moves %s' % (r.bad_move,))
            if not r.still: why.append('never still (t=%d)' % r.t)
            elif r.x_end != want: why.append('answer %s want %s' % (r.x_end and C.show(r.x_end), C.show(want)))
            if r.p_end != p: why.append('program changed')
            if not r.sealed: why.append('not sealed')
            sigs.add(r.out_sig)
            tmax, tsum = max(tmax, r.t), tsum + r.t
            if why:
                fails.append((C.show(p), C.show(x), why))
                if len(fails) >= stop_after: return dict(rung=name, runs=n, ok=False, fails=fails, tmax=tmax, tsum=tsum, sigs=sigs)
    return dict(rung=name, runs=n, ok=not fails, fails=fails, tmax=tmax, tsum=tsum, sigs=sigs)

def graded(D, limit=200000):
    out = {}
    rule, reader = P.Rule(D.REACTIONS, [0] * len(D.RANGES)), Reader(D)
    # refuses
    ps, xs = C.rung_stuck()
    ok = tot = 0
    for p in ps:
        for x in xs:
            try:
                if C.run(p, x) is not None: continue
            except C.TooDeep:
                continue
            tot += 1
            r = run(D, p, x, rule, reader, limit)
            fine = run(D, C.L, x, rule, reader, limit)
            ok += bool(r.still and r.x_end == x and r.p_end == p and r.out_sig != fine.out_sig)
    out['refuses'] = '%d/%d' % (ok, tot)
    # undo_exact, mirror_trace, never_moves over the first rungs a design passes
    undo = mirror = tot = 0
    for p in (C.SWAP, C.first(C.SWAP), C.STEM, C.UNIT, C.rep(C.STEM, 3), C.first(C.STEM), C.second(C.UNIT), C.SINK, C.CROSS):
        for x in (C.L, C.F(C.L, C.L), C.F(C.F(C.L, C.L), C.L), C.MARKED, C.F(C.S(C.L), C.L)):
            try:
                y = C.run(p, x)
            except C.TooDeep:
                continue
            if y is None: continue
            tot += 1
            a = run(D, C.seq(p, C.dag(p)), x, rule, reader, limit)
            lx = D.lay(x); lk = D.link(dims(D.lay(C.seq(p, C.dag(p)))), dims(lx))
            want = {(u + lk.off_x[0], v + lk.off_x[1]): c for (u, v), c in lx.cells.items()}
            have = {s: c for s, c in a.cells.items() if s in want} if a.cells else {}
            extra = sum(1 for s in (a.cells or {}) if s[0] >= lk.off_x[0] and s[1] >= lk.off_x[1] and s not in want)
            undo += have == want and extra == 0
            f, b = run(D, p, x, rule, reader, limit), run(D, C.dag(p), y, rule, reader, limit)
            mirror += f.trace == list(reversed(b.trace or []))
    out['undo_exact'] = '%d/%d' % (undo, tot)
    out['mirror_trace'] = '%d/%d' % (mirror, tot)
    # matter_counted
    cnt = 0
    for l, r in D.REACTIONS:
        ne = lambda side: sum(1 for c in side if not all(f == ('I', 0) for f in c))
        cnt += ne(l) == ne(r)
    out['matter_counted'] = '%d/%d' % (cnt, len(D.REACTIONS))
    # symmetric
    if hasattr(D, 'rot') and hasattr(D, 'mir'):
        bad = seen = 0
        for block, new in list(rule.cache.items())[:20000]:
            # turning a block: nw ne / sw se -> sw nw / se ne
            turn = lambda q: tuple(D.rot(c) for c in (q[2], q[0], q[3], q[1]))
            mirr = lambda q: tuple(D.mir(c) for c in (q[1], q[0], q[3], q[2]))
            seen += 1
            bad += rule(turn(block)) != turn(new) or rule(mirr(block)) != mirr(new)
        out['symmetric'] = '%d/%d blocks' % (seen - bad, seen)
    else:
        out['symmetric'] = 'n/a'
    return out

def speed(D, limit=2000000):
    """ticks on a ladder of sizes: how a design scales"""
    rule, reader = P.Rule(D.REACTIONS, [0] * len(D.RANGES)), Reader(D)
    table = {}
    def t(p, x):
        try:
            r = run(D, p, x, rule, reader, limit, watch=False)
        except Exception as e:
            return 'err:%s' % type(e).__name__
        return r.t if r.still and r.x_end == C.run(p, x) else 'x'
    table['map(swap,swap,swap) on full k=1..4'] = [t(C.map_(C.SWAP, C.SWAP, C.SWAP), C.full(k)) for k in (1, 2, 3, 4)]
    def path(k, f):
        for _ in range(k): f = C.second(f)
        return f
    def spine(k):
        s = C.L
        for _ in range(k): s = C.F(C.L, s)
        return s
    table['swap at depth k=1,2,4,8 of a list'] = [t(path(k, C.SWAP), spine(k + 1)) for k in (1, 2, 4, 8)]
    table['stem^k on a leaf k=1,4,16'] = [t(C.rep(C.STEM, k), C.L) for k in (1, 4, 16)]
    table['stem on a full k=1,2,3 tree'] = [t(C.STEM, C.full(k)) for k in (1, 2, 3)]
    table['first(stem) on a full k=1,2,3 tree'] = [t(C.first(C.STEM), C.full(k)) for k in (1, 2, 3)]
    table['deep swap on full k=1,2,3'] = [t(C.deep_swap(), C.full(k)) for k in (1, 2, 3)]
    table['NOT on a bit'] = t(C.NOT, C.L)
    table['ADDB 1+1 in 2 bits'] = t(C.ADDB, C.register(1, 1, 2))
    return table

def report(D, want_graded=True, want_speed=True, upto=None, limit=200000):
    t0 = time.time()
    rl, bad = rule_laws(D)
    print('%s: %d reactions, shapes %s, cell states %d, fields %s' % (D.NAME, len(D.REACTIONS), getattr(D, 'SHAPES', '?'), _prod(D.RANGES), D.RANGES))
    print(' rule laws:', rl, ('one_partner fails e.g. %s' % bad[:6]) if bad else '')
    mf, worst = matter_laws(D)
    print(' matter: lays/compact fails %d %s; worst cells/node for size<=8: %.2f' % (len(mf), mf[:4], worst))
    reached, sigs = None, set()
    results = []
    for name, f in C.RUNGS:
        ps, xs = f()
        t1 = time.time()
        r = check_rung(D, name, ps, xs, limit=limit if name != 'add' else 20000000)
        sigs |= r['sigs']
        print(' rung %-6s %s runs %d, ticks max %d sum %d, %.1fs %s' % (name, 'OK  ' if r['ok'] else 'FAIL', r['runs'], r['tmax'], r['tsum'], time.time() - t1, r['fails'][:3] if r['fails'] else ''))
        results.append(r)
        if r['ok'] and len(sigs) <= 1: reached = name
        if not r['ok'] or name == upto: break
    print(' clean: %s' % ('one output on every run' if len(sigs) <= 1 else '%d different outputs, e.g. %s' % (len(sigs), list(sigs)[:3])))
    print(' reached:', reached)
    if want_graded:
        print(' graded:', graded(D, limit))
    if want_speed:
        for k, v in speed(D).items(): print('  %-42s %s' % (k, v))
    print(' total %.1fs' % (time.time() - t0))
    return reached

def _prod(xs):
    n = 1
    for x in xs: n *= x
    return n

if __name__ == '__main__':
    # python laws.py path/to/design.py [--upto RUNG] [--quick]
    import importlib.util as u, os
    path = sys.argv[1]
    sys.path.insert(0, os.path.dirname(os.path.abspath(path)))
    s = u.spec_from_file_location('design', path); D = u.module_from_spec(s); s.loader.exec_module(D)
    upto = sys.argv[sys.argv.index('--upto') + 1] if '--upto' in sys.argv else None
    quick = '--quick' in sys.argv
    report(D, want_graded=not quick, want_speed=not quick, upto=upto)
