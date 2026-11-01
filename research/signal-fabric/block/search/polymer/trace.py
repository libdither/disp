import sys, os
here = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, here); sys.path.insert(0, os.path.dirname(here))
import design as D, physics as P, laws as Lw, calculus as C
names = {v: k for k, v in D.R.names.items()}
KN = {v: k for k, v in D.K.items()}
def head(w):
    for s, c in w.cells.items():
        if c[6]: return s, names.get(c[7], c[7]), c
    return None, None, None
def show(w, y=0):
    xs = [x for (x, yy) in w.cells if yy == y]
    if not xs: return ''
    out = []
    for x in range(0, max(xs) + 1):
        c = w.cells.get((x, y))
        if c is None: out.append('.'); continue
        t = KN[c[0]]
        t = {'PE':'-','PS':'\\','PN':'^','void':'_'}.get(t, t)
        if c[3]: t += '*'
        if c[1]: t += "'"
        if c[2]: t += 'e%d' % c[2]
        if c[6]: t = '[' + t + ']'
        out.append(t)
    return ' '.join(out)
def trace(p, x, n=4000, every=0, tail=60):
    rule = P.Rule(D.REACTIONS, [0] * len(D.RANGES)); cells, *_ = Lw.start(D, p, x)
    w = P.World(cells, rule); seq = []; last = None
    for _ in range(n):
        if w.still(): break
        w.step(); s, st, c = head(w)
        if st != last:
            seq.append((w.t, st, s, (c[8], c[9]) if c else None, show(w, 0), show(w, 1))); last = st
    for r in seq[-tail:]: print(r[0], r[1], r[2], 'fg', r[3], '|', r[4], '//', r[5])
    print('t', w.t, 'still', w.still(), 'outs', w.outs[:2])
if __name__ == '__main__':
    p = eval(sys.argv[1], vars(C)); x = eval(sys.argv[2], vars(C))
    trace(p, x, int(sys.argv[3]) if len(sys.argv) > 3 else 4000, tail=int(sys.argv[4]) if len(sys.argv) > 4 else 60)
