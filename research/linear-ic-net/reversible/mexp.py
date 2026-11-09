"""Formulations of one leaf arm M (c=0,a=0,b=0) of the self-spared adder, compiled by bracket abstraction."""
from common import *
import so2
F1 = lam('sp', 'Y', 'e1', 'e2', 'sib', 'brest', 'other', 'arest', 'acc',
         A(A(L, A(L, 'brest', A(L, A(L, A(L, 'e1', 'e2'), A(L, A(L, 'sp', 'Y'), 'sp')), 'sib')), 'other'), 'arest', A(L, A(L, 'acc'))))
FC = comp(lam('c', 'd', 'b', A(L, A(L, 'c', 'd'), 'b')))
LFB = comp(lam('s', 'y', A(L, A(L, 's', 'y'), 's')))
F2 = lam('sp', 'Y', 'e1', 'e2', 'sib', 'brest', 'other', 'arest', 'acc',
         A(FC, 'brest', A(FC, A(L, 'e1', 'e2'), A(LFB, 'sp', 'Y'), 'sib'), 'other', 'arest', A(L, A(L, 'acc'))))
S3 = comp(lam('nxt', 'arest', 'acc', A('nxt', 'arest', A(L, A(L, 'acc')))))
S2 = comp(lam('z', 'brest', 'other', A(S3, A(FC, 'brest', 'z', 'other'))))
S1 = comp(lam('lf', 'e1', 'e2', 'sib', A(S2, A(FC, A(L, 'e1', 'e2'), 'lf', 'sib'))))
F3 = lam('sp', 'Y', A(S1, A(LFB, 'sp', 'Y')))
print('FC', size(FC), 'LFB', size(LFB), 'S1', size(S1), 'S2', size(S2), 'S3', size(S3))
for nm, t in (('F1 direct', F1), ('F2 builders', F2), ('F3 staged', F3)):
    c = comp(t)
    # run on symbolic args to count steps (stuck when the frame meets atom arest: give arest a leaf)
    args = ['sp', 'Y', 'e1', 'e2', 'sib', 'brest', 'other', 'L', 'acc']
    v, st = F.run(A(c, *args))
    print(f'{nm:12} size {size(c):4}  steps to the next triage+ {st.steps}  {st}')
