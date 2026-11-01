"""Reads the tree at every tick, not only when a cell's reading changes, to check laws.py's shortcut hides nothing."""
import sys, os, time
sys.path[:0] = ['.', os.path.dirname(os.path.abspath(sys.argv[1]))]
import calculus as C, laws as Lw, physics as P
import importlib.util as u
s = u.spec_from_file_location('design', sys.argv[1]); D = u.module_from_spec(s); s.loader.exec_module(D)
rule, rd = P.Rule(D.REACTIONS, [0] * len(D.RANGES)), Lw.Reader(D)
for name, f in C.RUNGS[:6]:
    ps, xs = f(); runs = ticks = bad = 0; t0 = time.time()
    for p in ps:
        for x in xs:
            try: want = C.run(p, x)
            except C.TooDeep: continue
            if want is None: continue
            cells, rp, ep, rx, ex, port = Lw.start(D, p, x)
            w = P.World(cells, rule); was = rd.read(w.cells, rx, ex); runs += 1
            pk = rd.read(w.cells, rp, ep)
            while not w.still() and w.t < 300000:
                w.step(); ticks += 1
                now = rd.read(w.cells, rx, ex)
                if now is None or not C.moved(now, was): bad += 1; print('BAD', name, C.show(p), C.show(x), w.t, now and C.show(now)); break
                was = now
            if rd.read(w.cells, rx, ex) != want or rd.read(w.cells, rp, ep) != p or not w.still(): bad += 1; print('END', name, C.show(p), C.show(x))
    print(name, 'runs', runs, 'ticks', ticks, 'bad', bad, '%.0fs' % (time.time() - t0), flush=True)
