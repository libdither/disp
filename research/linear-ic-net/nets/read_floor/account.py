"""Task 1: where every rewrite of a counting program goes, per read, by cause.

python account.py [program input]...   (default: size size, size2 size, size suc, size2 suc)
"""
import sys
from collections import Counter, defaultdict
from tagnet import parse_defs, term, label, run
from defs import SRC

ARM_PARAMS = set('foavp')                  # the handed-back arms, in every program here
PART_PARAMS = {'F', 'L', 'g', 'b', 'w'}    # the parts of what was read
CHURCH = ('suc', 'add', 'zero')

KINDS = ('copy', 'cancel', 'erase', 'erase-dup', 'thaw', 'call', 'read')


def kind_of(key):
    k = key[0]
    if k in ('copy', 'cancel', 'erase', 'erase-dup'): return k
    if k.startswith('T⋈'): return 'thaw'
    if k.startswith('R⋈'): return 'read'
    return 'call'


def var_of(tag):
    return tag.rsplit(':', 1)[1] if ':' in tag else None


def owner(tag):
    return tag.rsplit(':', 1)[0]


def cause(key):
    k = key[0]
    if k in ('copy', 'cancel', 'erase-dup'):
        why = key[1] if k != 'erase-dup' else key[2]
        if why.startswith('IN:'):
            subject = key[3] if k == 'copy' else None
            return 'data: ρ meets the read code\'s binders' if subject and not subject.startswith('IN:') else 'setup: evaluating the input'
        if why.startswith('Z:'):
            if k == 'copy':
                deep, subject = key[2], key[3]
                if subject == 'Z' and not deep: return 'fixpoint: copies of Z\'s own λh'
                return 'fixpoint: copies of the function it holds'
            return 'fixpoint: copies of the function it holds'
        if var_of(why) == 'self': return 'self copied (used twice)'
        if owner(why) in CHURCH: return 'Church arithmetic'
        return f'other copy ({why})'
    if k == 'erase':
        who, deep, subject = key[1], key[2], key[3]
        if who.startswith('IN:'):
            return 'setup: evaluating the input' if subject.startswith('IN:') or subject == 'era' and False else 'data: ρ meets the read code\'s binders'
        v = var_of(who)
        if owner(who) in CHURCH: return 'Church arithmetic'
        if v in ARM_PARAMS: return 'untaken arms erased: what they captured' if deep else 'untaken arms erased: their own code'
        if v == 'self': return 'self dropped (leaf arms)'
        if v in PART_PARAMS: return 'parts of the data dropped'
        return f'other erase ({who})'
    tag = key[1]
    if k.startswith('R⋈') or tag == 'view': return 'view: made and applied to the arms'
    if k == 'O⋈rho': return 'view: made and applied to the arms'
    if k == 'O⋈Lam': return 'opening F on ρ'
    if tag.startswith('IN:'): return 'setup: evaluating the input'
    if tag in CHURCH: return 'Church arithmetic'
    if tag == 'Z': return 'fixpoint: thaws and calls of Z'
    return 'thaws and calls of the program (body, arms)'


def table(prog, inp, out=print):
    defs = parse_defs(SRC)
    t = (term(defs, prog), label(term(defs, inp), 'IN:'))
    n, root = run(t)
    total = n.stats['interactions']
    assert sum(n.acct.values()) == total
    reads = n.acct.get(('O⋈rho',), 0) or sum(v for k, v in n.acct.items() if k[0].startswith('R⋈'))
    rows = defaultdict(Counter)
    for key, v in n.acct.items():
        rows[cause(key)][kind_of(key)] += v
    out(f'\n{prog} {inp}: {total} rewrites, {reads} reads, {total / reads:.1f} a read')
    out(f'  {"cause":46} {"a read":>7} {"share":>6} ' + ' '.join(f'{k:>9}' for k in KINDS))
    for c, kinds in sorted(rows.items(), key=lambda kv: -sum(kv[1].values())):
        s = sum(kinds.values())
        out(f'  {c:46} {s / reads:7.1f} {100 * s / total:5.1f}% ' + ' '.join(f'{kinds[k] / reads:9.1f}' for k in KINDS))
    return n, rows, reads


def detail(n, reads, pred, out=print):
    for key, v in sorted(n.acct.items(), key=lambda kv: -kv[1]):
        if pred(key): out(f'      {v / reads:8.2f}  {key}')


if __name__ == '__main__':
    args = sys.argv[1:] or ['size', 'size', 'size2', 'size', 'size', 'suc', 'size2', 'suc']
    for prog, inp in zip(args[::2], args[1::2]):
        table(prog, inp)
