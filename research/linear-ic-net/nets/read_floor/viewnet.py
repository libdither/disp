"""Task 3, the smaller change measured: the view hands the taken arm all five arms, itself included.

  R ⋈ X  gives  λk1 … k5. ki k1 k2 k3 k4 k5 parts      (ki used twice: the view copies the taken arm)

An arm then needs no `me me`: it passes on the five arms it is handed. The copy of the taken arm is still made
agent by agent (the view's sharer on ki), so this only saves the self-application around it.

python viewnet.py
"""
import sys
sys.path.insert(0, '..')
import net
import programs
from programs import measure, DEFS
from tagnet import parse_defs
from defs import SRC, TO_NUMBER

_four_others = net.view_term
FIVE = False

def view_term(kind):
    if not FIVE: return _four_others(kind)
    ks = [f'k{i + 1}' for i in range(5)]
    body = ks[net.arms().index(kind)]
    for k in ks: body = (body, k)
    for p in net.PARTS[kind]: body = (body, ('~', p))
    for k in reversed(ks): body = ('λ', k, body)
    return body
net.view_term = view_term

VIEW5 = r'''
vfun := \f o a v p F n j k. ρ (F ρ) f o a v p (\s z. s (n s z)) j k
vopn := \f o a v p L n j k. ρ L f o a v p n j k
vapp := \f o a v p g b n j k. ρ g f o a v p n j (\r. r (ρ b) k)
vvar := \f o a v p w n j k. k (\vb k. vb f o a v p n (\r. r w j) k)
vprm := \f o a v p n j k. k (\vb k. vb f o a v p n j k)
vcount := \c. ρ c vfun vopn vapp vvar vprm lzero (\x. x) dstop
'''
DEFS.update(parse_defs(SRC + VIEW5))
TO_NUMBER['vcount'] = '\\r. r (\\n j f o a v p k. n)'

if __name__ == '__main__':
    want = {i: programs.answer('size', i) for i in programs.INPUTS}
    programs.answer = (lambda old: lambda prog, inp: want[inp] if prog == 'size' else old(prog, inp))(programs.answer)
    FIVE = True
    globals()['FIVE'] = True
    for i in programs.INPUTS:
        measure('vcount', i, rows=(i == 'size'))
