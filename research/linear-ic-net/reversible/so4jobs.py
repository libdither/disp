"""Stage searches for the adder leaf arms, cells with the filler on the LEFT (bit 1 = △ △ rest)."""
import sys
from so3 import *
from so2 import search
sp, Y, lf, sib, br, bf, ot, nxt, ar, af, acc, own = atoms('sp', 'Y', 'lf', 'sib', 'brest', 'bf', 'other', 'nxt', 'arest', 'af', 'acc', 'own')
K, = atoms('K')
C0 = lambda x: stem(x)            # 0-cell
C1 = lambda x: fork(L, x)         # 1-cell, filler on the left
job, nmax = sys.argv[1], int(sys.argv[2])
withK = {
  'B0':  ([lf, sib], napp(K, fork(fork(L, lf), sib))),
  'B1':  ([lf, sib, bf], napp(K, fork(fork(bf, sib), lf))),
  'C1r': ([own, br, ot, af], napp(K, (fork(br, ot), own) if False else napp(af, fork(br, ot), own))),
}
noK = {
  'D00': ([nxt, ar, acc], napp(nxt, ar, C0(C0(acc)))),
  'D01': ([nxt, ar, acc], napp(nxt, ar, C1(C0(acc)))),
  'D10f': ([nxt, af, ar, acc], napp(nxt, ar, C0(fork(af, acc)))),   # af reused as the a-cell filler
  'D11f': ([nxt, af, ar, acc], napp(nxt, ar, C1(fork(af, acc)))),
}
withK.update({
  'B1c': ([lf, sib, L], napp(K, fork(fork(L, sib), lf))),              # bf is a leaf: any leaf will do
  'C1c': ([own, br, ot, L], napp(K, fork(fork(br, ot), own))),         # af is a leaf
})
noK.update({
  'D10': ([nxt, ar, acc], napp(nxt, ar, C0(C1(acc)))),
  'D11': ([nxt, ar, acc], napp(nxt, ar, C1(C1(acc)))),
})
if job in withK:
    args, tgt = withK[job]; search1([(args, tgt)], K, nmax)
else:
    args, tgt = noK[job]; search([(args, tgt)], nmax=nmax, want=1)
