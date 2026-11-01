"""Watch one run token by token: python trace.py"""
import sys; sys.path[:0] = ['..', '.']
import calculus as C, physics as P, laws, design as D
names = {i + 1: n for i, n in enumerate(D._n)}
def watch(p, x, n=60):
    cells, *_ = laws.start(D, p, x)
    w = P.World(cells, P.Rule(D.REACTIONS, [0] * len(D.RANGES)))
    for _ in range(n):
        toks = [(s, c[3], c[4], names.get(c[5]), c[6], c[0], c[1]) for s, c in sorted(w.cells.items()) if c[3]]
        print(w.t, toks, w.outs)
        if w.still(): break
        w.step()
if __name__ == '__main__':
    a = sys.argv[1:] or ['SWAP', 'F(L,L)', '60']
    env = dict(vars(C)); watch(eval(a[0], env), eval(a[1], env), int(a[2]))
