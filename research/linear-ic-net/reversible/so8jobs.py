"""ARM of the direct-style wrap: the wrap frame △ (△ △ △) ARM opens the returned pair △ P out and hands ARM
two leaves; ARM must give △ P (cell out)."""
import sys
from so2 import *
P, out = atoms('P', 'out')
job, nmax = sys.argv[1], int(sys.argv[2])
jobs = {'ARM0': ([L, L, P, out], fork(P, stem(out))),
        'ARM1': ([L, L, P, out], fork(P, fork(L, out)))}
args, tgt = jobs[job]; search([(args, tgt)], nmax=nmax, want=1)
