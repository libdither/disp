"""Stages for the single-chain transducer: rebuild the main frame △ (△ (△ e) lf) sib after the leaf frame."""
import sys
from so3 import *
lf, e, sib = atoms('lf', 'e', 'sib'); K, = atoms('K')
job, nmax = sys.argv[1], int(sys.argv[2])
jobs = {
  'G0': ([lf, e, sib], napp(K, fork(fork(stem(e), lf), sib))),
  'G1': ([lf, e, sib, L], napp(K, fork(fork(stem(e), sib), lf))),
}
args, tgt = jobs[job]; search1([(args, tgt)], K, nmax)
