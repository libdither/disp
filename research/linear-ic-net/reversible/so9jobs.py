import sys
from so2 import *
from so3 import search1
fr, br, bacc, p, iend, isp = atoms('fr', 'brest', 'bacc', 'p', 'iend', 'isp'); K, = atoms('K')
job, nmax = sys.argv[1], int(sys.argv[2])
if job == 'PUSH2':   # fr brest bacc p -> fr brest (△ bacc) (△ p)
    search([([fr, br, bacc, p], napp(fr, br, stem(bacc), stem(p)))], nmax=nmax, want=1)
elif job == 'FRK':   # iend isp -> K (△ (△ iend isp) isp)
    search1([([iend, isp], napp(K, fork(fork(iend, isp), isp)))], K, nmax)
