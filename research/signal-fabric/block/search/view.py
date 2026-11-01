"""Runs a few programs on a design and writes out/view.json for view.html. Any design works: what each cell
looks like comes from its reading table. python view.py ballistic/design.py"""
import json, os, sys
import importlib.util as u
import calculus as C, laws as Lw, physics as P

path = sys.argv[1]
sys.path.insert(0, os.path.dirname(os.path.abspath(path)))
s = u.spec_from_file_location('design', path); D = u.module_from_spec(s); s.loader.exec_module(D)
E, SS = (1, 0), (0, 1)
VOID = tuple(0 for _ in D.RANGES)
rest = set(c for t in C.trees(5) for c in D.lay(t).cells.values())
head_on = getattr(D, 'head_on', lambda c: c not in rest)

def glyph(c):
    a, b = D.node(c, SS), D.node(c, E)
    n = a or b
    k = 'other'
    if n and n[0] in ('leaf', 'stem', 'fork'): k = n[0]
    elif n and n[0] == 'pass':
        k = 'elbow' if (a and a[1] != SS) or (b and b[1] != E) else 'ball'
    return dict(k=k, on=bool(k != 'ball' and head_on(c)), d=' '.join(str(v) for v in c), arms=[list(n[1]), list(n[2])] if n and n[0] == 'fork' and n[2] else None)

def links(cells, at, step, reader):
    """the reading's path as segments: [x0, y0, x1, y1, side], side 0 plain, 1 left part, 2 right part"""
    out, fuel = [], [4000]
    def walk(at, step, side):
        while fuel[0] > 0:
            fuel[0] -= 1
            n = D.node(cells.get(at, VOID), step)
            if n is None or n[0] == 'leaf': return
            if n[0] in ('pass', 'stem'):
                to = (at[0] + n[1][0], at[1] + n[1][1])
                out.append([*at, *to, side]); at, step = to, n[1]; continue
            for d, sd in ((n[1], 1), (n[2], 2)):
                if d is None: return
                to = (at[0] + d[0], at[1] + d[1])
                out.append([*at, *to, sd]); walk(to, d, sd)
            return
    walk(at, step, 0)
    return out

CASES = [('swap', C.SWAP, C.F(C.S(C.L), C.L)), ('first(swap)', C.first(C.SWAP), C.F(C.F(C.S(C.L), C.L), C.L)),
         ('stem', C.STEM, C.F(C.L, C.L)), ('first(stem)', C.first(C.STEM), C.F(C.F(C.L, C.L), C.L)), ('unit', C.UNIT, C.F(C.L, C.L)),
         ('unit ; first(unit): east parts move', C.seq(C.UNIT, C.first(C.UNIT)), C.F(C.L, C.L)), ('sink', C.SINK, C.S(C.F(C.L, C.L))), ('cross', C.CROSS, C.F(C.F(C.S(C.L), C.L), C.L)),
         ('dag(stem): stuck, undoes itself', C.seq(C.STEM, C.dag(C.STEM), C.dag(C.STEM)), C.L),
         ('tri, stem branch', C.when1(C.SWAP), C.F(C.S(C.L), C.F(C.S(C.L), C.L))), ('not 0→1', C.NOT, C.L), ('not 1→0', C.NOT, C.S(C.L)),
         ('deep swap (fix)', C.deep_swap(), C.F(C.F(C.S(C.L), C.L), C.L))]

glyphs, gid = [], {}
def g_of(c):
    if c not in gid:
        gid[c] = len(glyphs); glyphs.append(glyph(c))
    return gid[c]

rule, reader = P.Rule(D.REACTIONS, [0] * len(D.RANGES)), Lw.Reader(D)
out = []
for name, p, x in CASES:
    want = C.run(p, x)
    cells, rp, ep, rx, ex, port = Lw.start(D, p, x)
    w = P.World(cells, rule)
    first = [[a, b, g_of(c)] for (a, b), c in w.cells.items()]
    ticks, lks, reads, was = [], [[0, links(w.cells, rp, ep, reader) + links(w.cells, rx, ex, reader)]], [[0, C.show(reader.read(w.cells, rx, ex))]], reader.read(w.cells, rx, ex)
    while not w.still() and w.t < 100000:
        old = Lw._peek(w, w.phase)
        ch = w.step()
        ticks.append([v for s in ch for v in (s[0], s[1], g_of(w.cells[s]) if s in w.cells else -1)])
        if any(reader.sig(old.get(s, VOID)) != reader.sig(w.cells.get(s, VOID)) for s in ch):
            lks.append([w.t, links(w.cells, rp, ep, reader) + links(w.cells, rx, ex, reader)])
            now = reader.read(w.cells, rx, ex)
            if now != was: reads.append([w.t, C.show(now) if now else '?']); was = now
    out.append(dict(name=name, prog=C.show(p), data=C.show(x), want=C.show(want) if want else 'stuck', ok=reader.read(w.cells, rx, ex) == (want if want else x) and w.still(), first=first, ticks=ticks, links=lks, reads=reads))
    print('%-34s %7d ticks  %s' % (name, w.t, 'ok' if out[-1]['ok'] else 'WRONG'))
os.makedirs('out', exist_ok=True)
json.dump(dict(design=D.NAME, reactions=len(D.REACTIONS), glyphs=glyphs, cases=out), open('out/view.json', 'w'), separators=(',', ':'))
print('out/view.json', os.path.getsize('out/view.json') // 1024, 'KB')
