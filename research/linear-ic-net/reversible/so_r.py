from so2 import *
import sys
Fa, h, t, acc = atoms('F', 'h', 't', 'acc')
order = sys.argv[1].split(',')
env = {'F': Fa, 'h': h, 't': t, 'acc': acc}
tgt = napp(Fa, t, fork(h, acc))
print('R', order, '-> F t (△ h acc)')
search([([env[v] for v in order], tgt)], nmin=1, nmax=int(sys.argv[2]), mod=(1, 3), want=3, limit=30)
