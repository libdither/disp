import sys
from so2 import *
s, y, nxt, ar, acc, z, br, ot, lf, e1, e2, sib = atoms('s', 'y', 'nxt', 'arest', 'acc', 'z', 'brest', 'other', 'lf', 'e1', 'e2', 'sib')
S3, S2 = atoms('S3', 'S2')
job = sys.argv[1]; nmax = int(sys.argv[2])
if job == 'LFB':
    search([([s, y], fork(fork(s, y), s))], nmax=nmax, want=2)
elif job == 'S3':
    search([([nxt, ar, acc], napp(nxt, ar, stem(stem(acc))))], nmax=nmax, want=2)
elif job == 'S2':
    search([([z, br, ot], napp(S3, fork(fork(br, z), ot)))], nmax=nmax, consts=(S3,), want=2)
elif job == 'S1':
    search([([lf, e1, e2, sib], napp(S2, fork(fork(fork(e1, e2), lf), sib)))], nmax=nmax, consts=(S2,), want=2)
