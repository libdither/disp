import sys
from so2 import *
nxt, ar, acc = atoms('nxt', 'arest', 'acc')
job, nmax = sys.argv[1], int(sys.argv[2])
jobs = {'DD0': ([nxt, ar, acc], napp(nxt, ar, stem(acc))), 'DD1': ([nxt, ar, acc], napp(nxt, ar, fork(L, acc)))}
args, tgt = jobs[job]; search([(args, tgt)], nmax=nmax, want=1)
