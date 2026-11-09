import sys
from so3 import *
from so2 import search
sp, Y, lf, sib, br, bf, ot, nxt, ar, af, acc, z, x, y = atoms('sp', 'Y', 'lf', 'sib', 'brest', 'bf', 'other', 'nxt', 'arest', 'af', 'acc', 'z', 'x', 'y')
K, = atoms('K')
job, nmax = sys.argv[1], int(sys.argv[2])
withK = {
  'Ka':   ([z, br, ot], napp(K, fork(fork(br, z), ot))),          # a = 0: frame(brest, own, other)
  'Kb':   ([z, br, ot], napp(K, fork(fork(br, ot), z))),          # a = 1: frame(brest, other, own)
  'Kc':   ([z, br, bf, ot], napp(K, (fork(br, z), ot))),          # bf as the A-frame root (bf = △ in reality)
  'own1': ([x, y], napp(K, fork(fork(L, y), x))),                 # b = 1: △ (△ △ sib) lf from (lf, sib)
}
noK = {   # H: nxt arest [af] acc -> nxt arest acc'
  'H00': ([nxt, ar, acc], napp(nxt, ar, stem(stem(acc)))),
  'H01': ([nxt, ar, acc], napp(nxt, ar, fork(stem(acc), L))),
  'H10': ([nxt, ar, af, acc], napp(nxt, ar, stem(fork(acc, af)))),
  'H11': ([nxt, ar, af, acc], napp(nxt, ar, fork(fork(acc, af), L))),
}
if job in withK:
    args, tgt = withK[job]; search1([(args, tgt)], K, nmax)
else:
    args, tgt = noK[job]; search([(args, tgt)], nmax=nmax, want=1)
