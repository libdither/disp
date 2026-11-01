"""trace.py 'prog-expr' 'data-expr' [from] [to]: print the grid ticks where something interesting changes; expressions over calculus.py names"""
import sys, os
sys.path.insert(0, os.path.abspath('.')); sys.path.insert(0, os.path.abspath('arcs'))
import laws as L, calculus as C, physics as P, design as D
from kit import FIELDS, _modes, VOID, HB, VB, PB, SB, NODE, EL, HUB
env = {k: getattr(C, k) for k in dir(C)}
p, x = eval(sys.argv[1], env), eval(sys.argv[2], env)
a = int(sys.argv[3]) if len(sys.argv) > 3 else 0
b = int(sys.argv[4]) if len(sys.argv) > 4 else 10**9
names = {v: k for k, v in _modes.items()}
FI = {n: i for i, n in enumerate(FIELDS)}
def sym(c):
    g = lambda n: c[FI[n]]
    cl = g('cls')
    head = names.get(g('m'), '') if g('m') else ''
    if cl == VOID: return '|' if g('w') else '.'
    if cl in (HB, VB, PB, SB): return {HB: '-', VB: '!', PB: 'P', SB: 's'}[cl] + head + ('' if not g('w') else '|') + (':%d' % g('v') if g('v') else '') + (('p%d' % g('pk')) if cl == PB else '')
    if cl == EL: return ('T' if g('w') else 'B') + head
    if cl == HUB: return 'H' + head
    return '.LSFY'[g('k')].replace('.', '_') + ('*' if g('f') else '') + ('c' if g('c') else '') + ('h' if g('h') else '') + ('a' if g('a') else '') + (str(g('r')) if g('r') else '') + ('t' if g('t') else '') + head
frames = []
r = L.run(D, p, x, frames=frames)
print('t', r.t, 'still', r.still, 'reads', r.reads, 'moves', r.moves, r.bad_move, 'end', r.x_end and C.show(r.x_end), 'want', C.show(C.run(p, x) or 'L'))
rd = L.Reader(D)
_, rp, ep, rx, ex, _ = L.start(D, p, x)
for t, f in enumerate(frames):
    q = rd.read(f, rx, ex)
    bad = q is None
    if a <= t <= b or (bad and a == 0 and b == 10**9):
        xs = [s[0] for s in f]; ys = [s[1] for s in f]
        print('--- t', t, 'x=', q and C.show(q))
        x0 = rx[0]
        for y in range(0, max(ys) + 1):
            row = [(sym(f[(xx, y)]) if (xx, y) in f else '.') for xx in range(x0, max(xs) + 1)]
            print('%3d ' % y + ' '.join('%-6s' % c for c in row))
        if bad: break
