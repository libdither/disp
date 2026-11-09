"""One-wrap opener for the in-place adder: ARM gets two leaves, OA, and the pair △ OS P; it must give
△ (Ca OA) (△ (Cs OS) P)."""
import sys
from so2 import *
OA, OS, P = atoms('OA', 'OS', 'P')
C = {0: lambda x: stem(x), 1: lambda x: fork(L, x)}
job, nmax = sys.argv[1], int(sys.argv[2])
a, s = int(job[-2]), int(job[-1])
search([([L, L, OA, fork(OS, P)], fork(C[a](OA), fork(C[s](OS), P)))], nmax=nmax, want=1, limit=40)
