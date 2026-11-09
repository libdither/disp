from common import *
from addx6 import Hform
Cf = {0: lambda x: A(L, x), 1: lambda x: A(L, L, x)}
# switching tail (acc first, H-form), case a=0 b=0 s=1: own brest other arest acc Ho -> Ho brest arest (C1 (C0 acc)) (Hform own other)
direct = lam('own', 'brest', 'other', 'arest', 'acc', 'Ho', A('Ho', 'brest', 'arest', Cf[1](Cf[0]('acc')), Hform('own', 'other')))
S2 = comp(lam('brest', 'hc', 'arest', 'acc', 'Ho', A('Ho', 'brest', 'arest', Cf[1](Cf[0]('acc')), 'hc')))
S1 = comp(lam('own', 'brest', 'other', A(S2, 'brest', Hform('own', 'other'))))
S2b = comp(lam('hc', 'brest', 'arest', 'acc', 'Ho', A('Ho', 'brest', 'arest', Cf[1](Cf[0]('acc')), 'hc')))
S1b = comp(lam('own', 'brest', 'other', A(S2b, Hform('own', 'other'), 'brest')))
HF1 = comp(lam('own', A(L, A(L, A(L, L)), A(L, L, A(L, L, 'own')))))   # own -> B △ (C △ own)
S2c = comp(lam('hf', 'brest', 'other', 'arest', 'acc', 'Ho', A('Ho', 'brest', 'arest', Cf[1](Cf[0]('acc')), A(L, L, A(L, 'hf', 'other')))))
print('direct', size(comp(direct)), '| staged S1+S2', size(S1), '(S2', size(S2), ') | S1b', size(S1b), '(S2b', size(S2b), ')',
      '| HF1-then', size(HF1) + size(S2c))
