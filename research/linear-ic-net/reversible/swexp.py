from common import *
def Hform(Z, O): return A(L, L, A(L, A(L, A(L, A(L, L)), A(L, L, A(L, L, Z))), O))
Cf = {0: lambda x: A(L, x), 1: lambda x: A(L, L, x)}
SWAPIN = comp(lam('b', 'r', 'a', 'h', 'k', A('k', 'b', 'r', 'a', 'h')))
SWAP3 = comp(lam('b', 'r', 'h', 'k', A('k', 'b', 'r', 'h')))
print('SWAPIN', size(SWAPIN), 'SWAP3', size(SWAP3))
for nm, t in [
  ('pair acc-first a0', lam('own', 'brest', 'other', 'arest', 'acc', 'Zo', 'Oo', A(A(L, A(L, 'brest', 'Zo'), 'Oo'), 'arest', Cf[1](Cf[0]('acc')), 'own', 'other'))),
  ('hole acc-first a0', lam('own', 'brest', 'other', 'arest', 'acc', 'Ho', A('Ho', 'brest', 'arest', Cf[1](Cf[0]('acc')), Hform('own', 'other')))),
  ('hole acc-first a0 via SWAPIN', lam('own', 'brest', 'other', 'arest', 'acc', A(SWAPIN, 'brest', 'arest', Cf[1](Cf[0]('acc')), Hform('own', 'other')))),
  ('hole state-first a0', lam('own', 'brest', 'other', 'arest', 'Ho', 'acc', A('Ho', 'brest', 'arest', Hform('own', 'other'), Cf[1](Cf[0]('acc'))))),
  ('hole state-first a0 via SWAP3', lam('own', 'brest', 'other', 'arest', A(SWAP3, 'brest', 'arest', Hform('own', 'other')))),
  ('nonsw hole state-first D', lam('nxt', 'arest', 'Ho', 'acc', A('nxt', 'arest', 'Ho', Cf[0](Cf[0]('acc'))))),
]:
    print(f'{nm:32} {size(comp(t)):4}')
