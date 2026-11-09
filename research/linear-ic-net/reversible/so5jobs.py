"""Searches for the H-form adder with the inactive state Ho riding before acc."""
import sys
from so3 import *
from so2 import search
nxt, ar, Ho, acc, b, r, h, k, own, ot, br = atoms('nxt', 'arest', 'Ho', 'acc', 'b', 'r', 'h', 'k', 'own', 'other', 'brest')
K, = atoms('K')
C0 = lambda x: stem(x); C1 = lambda x: fork(L, x)
job, nmax = sys.argv[1], int(sys.argv[2])
noK = {
  'DS00': ([nxt, ar, Ho, acc], napp(nxt, ar, Ho, C0(C0(acc)))),
  'DS01': ([nxt, ar, Ho, acc], napp(nxt, ar, Ho, C1(C0(acc)))),
  'DS10': ([nxt, ar, Ho, acc], napp(nxt, ar, Ho, C0(C1(acc)))),
  'DS11': ([nxt, ar, Ho, acc], napp(nxt, ar, Ho, C1(C1(acc)))),
  'SWAP3': ([b, r, h, k], napp(k, b, r, h)),
}
args, tgt = noK[job]; search([(args, tgt)], nmax=nmax, want=1)
