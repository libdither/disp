from so2 import *
nil, s, h, t, acc, x, y = atoms('nil', 's', 'h', 't', 'acc', 'x', 'y')
def tests(order):
    out = []
    for tv, res in ((L, lambda tail: napp(nil, s, s, tail)), (fork(x, y), lambda tail: napp(s, nil, s, x, y, tail))):
        env = {'nil': nil, 's': s, 'h': h, 't': tv, 'acc': acc}
        out.append(([env[v] for v in order], res(fork(h, acc))))
    return out
import sys
order = sys.argv[1].split(',')
print('order', order)
search(tests(order), nmin=12, nmax=int(sys.argv[2]), mod=(0, 3), want=1, limit=30)
