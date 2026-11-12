"""Task 2: the counting programs on the same inputs, rewrites per read by kind and by cause, and their answers.

python programs.py [program ...]  (default: every program in defs.TO_NUMBER)
Inputs: suc, size, interp (the evaluated values, as `size size` reads them).
"""
import sys
from collections import Counter, defaultdict
sys.path.insert(0, '..')
from net import Net, readback
from ref import canon, number_of
from tagnet import parse_defs, term, label, run, strip
from account import kind_of, KINDS
from defs import SRC, TO_NUMBER

INPUTS = ('suc', 'size', 'interp')
DEFS = parse_defs(SRC)


def answer(prog, inp):
    """the count the program gives, as a number, from a separate plain run"""
    t = (term(DEFS, f'({TO_NUMBER[prog]}) ({prog} {inp})'))
    n = Net()
    root = n.load(strip(t))
    assert n.run(50_000_000) == 'done'
    got = number_of(canon(readback(n, n.mate[(root, 0)])))
    assert got and got[1] == 'u', got
    return got[0]


def owner(tag):
    return tag.split(':')[0].split('.')[0]


def cause(key):
    k = key[0]
    if k in ('copy', 'cancel', 'erase-dup'):
        why = key[1] if k != 'erase-dup' else key[2]
        if why.startswith('IN:'):
            return 'data: ρ meets the read code\'s binders' if k == 'copy' and not key[3].startswith('IN:') else 'setup: evaluating the input'
        if why.endswith(':me'): return 'copy of the taken arm (me me)'
        return f'copy because {why} is used twice'
    if k == 'erase':
        who, subject = key[1], key[3]
        if who.startswith('IN:'):
            return 'setup: evaluating the input' if subject.startswith('IN:') else 'data: ρ meets the read code\'s binders'
        return f'erase by {who}'
    tag = key[1]
    if k.startswith('R⋈') or k == 'O⋈rho' or tag == 'view': return 'view: made and applied to the arms'
    if k == 'O⋈Lam': return 'opening F on ρ'
    if tag.startswith('IN:'): return 'setup: evaluating the input'
    return f'thaw/call of {owner(tag)}'


def measure(prog, inp, out=print, rows=True):
    t = (term(DEFS, prog), label(term(DEFS, inp), 'IN:'))
    n, root = run(t)
    total = n.stats['interactions']
    reads = sum(v for k, v in n.acct.items() if k[0].startswith('R⋈'))
    by_kind, by_cause = Counter(), defaultdict(Counter)
    for key, v in n.acct.items():
        by_kind[kind_of(key)] += v
        by_cause[cause(key)][kind_of(key)] += v
    setup = sum(by_cause['setup: evaluating the input'].values())
    want = answer('size', inp)
    got = answer(prog, inp) if TO_NUMBER[prog] else None
    verdict = 'no answer to check' if got is None else '= size' if got == want else f'!= size ({want})'
    out(f'{prog:9} {inp:7} {total:8} rewrites {reads:4} reads  {total / reads:7.1f} a read ({(total - setup) / reads:6.1f} without evaluating the input)'
        f'  answer {got} {verdict}  '
        + ' '.join(f'{k} {by_kind[k] / reads:.1f}' for k in KINDS))
    if rows:
        for c, kinds in sorted(by_cause.items(), key=lambda kv: -sum(kv[1].values())):
            s = sum(kinds.values())
            out(f'      {c:50} {s / reads:7.1f}  ' + ' '.join(f'{k} {kinds[k] / reads:.1f}' for k in KINDS if kinds[k]))
    return total, reads, got == want


if __name__ == '__main__':
    progs = sys.argv[1:] or list(TO_NUMBER)
    for p in progs:
        for i in INPUTS:
            measure(p, i, rows=(i == 'size'))
        print()
