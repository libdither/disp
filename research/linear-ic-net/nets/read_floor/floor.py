"""Task 3: the floor, measured at the margin. What one more read costs on code made only of one kind of node:
  a chain of binders  λx1 … xn. xn                    (one more read: a function)
  a chain of applications  λx. x x … x   (n uses; x is copied as ρ when opened: data cost, 1 a use)
                                                      (one more pair of reads: an application and a variable)
for each program, the difference between n = 32 and n = 16, by kind.

python floor.py
"""
import sys
from collections import Counter
sys.path.insert(0, '..')
import net
from tagnet import TagNet, term, label
from account import kind_of, KINDS
import programs, refnet

MINI = r'''
-- the least a read can do in this net: issue the next read on the body of a function, passing the four
-- arms on and a new copy of itself. Any other kind ends the walk and gives back what it holds.
mfun := \me o a v p F. ρ (F ρ) (me me) o a v p
mend := \me x y z u w. w
mini := \c. ρ c (mfun mfun) (mend mend) (mend mend) (mend mend) (\me f o a v. me)
'''
programs.DEFS.update(programs.parse_defs(programs.SRC + MINI))
refnet.DEFS.update(refnet.parse_with_refs(refnet.REFS + r'''
#Mfun := \o a v p F. ρ (F ρ) Mfun o a v p
Mini := \c. ρ c Mfun (\f a v p w. w) (\f o v p g b. g) (\f o a p w. w) (\f o a v. f)
'''))


def binders(n):
    body = f'x{n - 1}'
    for i in reversed(range(n)): body = ('λ', f'x{i}', body)
    return body

def applications(n):
    body = 'x'
    for _ in range(n): body = (body, 'x')
    return ('λ', 'x', body)


def cost(defs, netclass, prog, data):
    nt = netclass()
    nt.load((programs.term(defs, prog), data))
    assert nt.run(50_000_000) == 'done'
    by = Counter()
    for k, v in nt.acct.items(): by[kind_of(k)] += v
    by['rewrites'] = nt.stats['interactions']
    by['reads'] = sum(v for k, v in nt.acct.items() if k[0].startswith('R⋈') and k[0] != 'R⋈ref')
    by['laid out'] = getattr(nt, 'laid_out', 0)
    return by


def margin(defs, netclass, prog, shape):
    a, b = cost(defs, netclass, prog, shape(16)), cost(defs, netclass, prog, shape(32))
    return {k: (b[k] - a[k]) / 16 for k in ('rewrites', 'reads', 'laid out') + KINDS}


ROWS = [
    ('mini (stateless descent, me me)', programs.DEFS, TagNet, 'mini', [binders]),
    ('walk (count_d, nothing counted)', programs.DEFS, TagNet, 'walk', [binders, applications]),
    ('count_d (linear, best here)', programs.DEFS, TagNet, 'count_d', [binders, applications]),
    ('size2', programs.DEFS, TagNet, 'size2', [binders, applications]),
    ('size', programs.DEFS, TagNet, 'size', [binders, applications]),
    ('Mini (refs, stateless descent)', refnet.DEFS, refnet.RefNet, 'Mini', [binders]),
    ('ecount (refs, count_d)', refnet.DEFS, refnet.RefNet, 'ecount', [binders, applications]),
    ('rsize (refs, size as written)', refnet.DEFS, refnet.RefNet, 'rsize', [binders, applications]),
]

if __name__ == '__main__':
    print(f'{"program":34} {"one more":24} {"reads":>5} {"rewrites":>8} ' + ' '.join(f'{k:>9}' for k in KINDS) + f' {"laid out":>9}')
    for name, defs, cls, prog, shapes in ROWS:
        for shape in shapes:
            m = margin(defs, cls, prog, shape)
            what = 'function' if shape is binders else 'application + variable'
            print(f'{name:34} {what:24} {m["reads"]:5.1f} {m["rewrites"]:8.1f} ' + ' '.join(f'{m[k]:9.1f}' for k in KINDS) + f' {m["laid out"]:9.1f}')
