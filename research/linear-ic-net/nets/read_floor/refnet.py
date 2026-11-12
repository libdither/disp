"""Task 3 prototype: one new agent, `ref`, a reference to a closed definition by name.

  ref(p)          a closed definition, not yet laid out
  T ⋈ ref         the ref is a value already: it comes back as it is (as T ⋈ lam does)
  O ⋈ ref         lay the definition out (one rewrite, as R ⋈ X lays out a view), then O meets it
  R ⋈ ref         the same, then R meets it
  sig|dup ⋈ ref   two refs: copying a closed definition is one rewrite, whatever its size
  era ⋈ ref       gone: one rewrite

Code that is run is no longer copied out first: the rule lays out a fresh copy of the definition each time
it is called, as the reflection rule lays out a fresh view. A program names a definition with `#name := …`;
every use of the name (its own body included) is a ref. Inputs are loaded as before, with no refs in them.

python refnet.py   (the programs below on suc, size, interp)
"""
import sys, re
from collections import Counter, defaultdict
sys.path.insert(0, '..')
import net
from net import Net, readback, ARITY
from ref import canon, number_of, expr, FORMS
from tagnet import TagNet, parse_defs, term, label, strip, TOKENS
from account import kind_of, KINDS
from defs import SRC
import programs

ARITY['ref'] = 0


class RefNet(TagNet):
    templates = {}

    def __init__(self, seed=None):
        super().__init__(seed)
        self.refname = {}
        self.laid_out = 0          # agents laid out by expanding refs (allocation a rewrite does not count)

    def emit_ref(self, name):
        saved = self.cur
        self.cur = 'ref ' + name
        r = self.new('ref')
        self.cur = saved
        self.refname[r] = name
        return (r, 0)

    def expand(self, name):
        before = next(self.ids)
        saved = self.cur
        self.cur = name
        port = self.emit(self.templates[name], {})
        self.cur = saved
        self.laid_out += next(self.ids) - before - 1
        return port


def r_thaw_ref(n, t, r):
    name = n.refname[r]
    def build(slot):
        w = n.new('ref')
        n.refname[w] = name
        n.link((w, 0), slot(t, 1))
    n.rewrite(t, r, build)

def lay_out(kind):
    def rule(n, x, r):
        name = n.refname[r]
        def build(slot):
            y = n.new(kind)
            for i in range(1, ARITY[kind] + 1):
                n.link((y, i), slot(x, i))
            n.link((y, 0), n.expand(name))
        n.rewrite(x, r, build)
    return rule

def r_copy_ref(n, c, r):
    name = n.refname[r]
    if n.kind[c] == 'sig': next(n.ops)
    def build(slot):
        for i in (1, 2):
            w = n.new('ref')
            n.refname[w] = name
            n.link((w, 0), slot(c, i))
    n.rewrite(c, r, build)
r_copy_ref.copy_like = True

net.RULES[('T', 'ref')] = r_thaw_ref
net.RULES[('O', 'ref')] = lay_out('O')
net.RULES[('R', 'ref')] = lay_out('R')
net.RULES[('sig', 'ref')] = r_copy_ref
net.RULES[('dup', 'ref')] = r_copy_ref


REFS = r'''
-- size with each arm a definition of its own, calling size by name: no fixpoint, no self. A read hands
-- back four refs, and the arm drops them (one erase each).
#rsize := \c. ρ c rfun ropn rapp rvar rprm
#rfun := \o a v p F. suc (rsize (F ρ))
#ropn := \f a v p L. rsize L
#rapp := \f o v p g b. add (rsize g) (rsize b)
#rvar := \f o a p w. zero
#rprm := \f o a v. zero

-- count_d with refs: an arm hands on a ref to itself in place of me me. Linear as count_d is.
#efun := \o a v p F n j k. ρ (F ρ) efun o a v p (\s z. s (n s z)) j k
#eopn := \f a v p L n j k. ρ L f eopn a v p n j k
#eapp := \f o v p g b n j k. ρ g f o eapp v p n j (\r. r (ρ b) k)
#evar := \f o a p w n j k. k (\vb k. vb f o a evar p n (\r. r w j) k)
#eprm := \f o a v n j k. k (\vb k. vb f o a v eprm n j k)
estop := \r. r (\f o a v p n j k z. z n j f o a v p k) (\x. x)
ecount := \c. ρ c efun eopn eapp evar eprm lzero (\x. x) estop

-- the same, but every read names all five arms afresh and the arm drops the four it is handed
-- (one erase of a ref each) in place of passing them on; leaves drop w.
#gfun := \o a v p F n k. ρ (F ρ) gfun gopn gapp gvar gprm (\s z. s (n s z)) k
#gopn := \f a v p L n k. ρ L gfun gopn gapp gvar gprm n k
#gapp := \f o v p g b n k. ρ g gfun gopn gapp gvar gprm n (\r. r (ρ b) k)
#gvar := \f o a p w n k. k (\vb k. vb gfun gopn gapp gvar gprm n k)
#gprm := \f o a v n k. k (\vb k. vb gfun gopn gapp gvar gprm n k)
gstop := \r. r (\f o a v p n k. n) (\x. x)
gcount := \c. ρ c gfun gopn gapp gvar gprm lzero gstop
'''

TO_NUMBER = {'rsize': '\\r. r lsuc lzero', 'ecount': '\\r. r (\\n j f o a v p k. n)', 'gcount': '\\r. r'}


def parse_with_refs(src):
    """check.PROGRAMS and the programs above; '#name := …' makes name a ref, its body a template"""
    names = re.findall(r'^#(\w+)\s*:=', src, re.M)
    defs = parse_defs(SRC)
    for nm in names: defs[nm] = ('#', nm)
    plain = re.sub(r'^#', '', src, flags=re.M)
    lines = [l.split('--')[0] for l in plain.strip().split('\n')]
    chunks = []
    for l in lines:
        if not l.strip(): continue
        if ':=' in l or not chunks or not l[0].isspace(): chunks.append(l)
        else: chunks[-1] += ' ' + l
    for ch in chunks:
        name, body = [s.strip() for s in ch.split(':=', 1)]
        t, i = expr(re.findall(TOKENS, body), 0, defs, ())
        assert i == len(re.findall(TOKENS, body))
        if name in names: RefNet.templates[name] = ('@', name, t)
        else: defs[name] = ('@', name, t)
    return defs

DEFS = parse_with_refs(REFS)


def answer(prog, inp):
    t = term(DEFS, f'({TO_NUMBER[prog]}) ({prog} {inp})')
    n = RefNet()
    root = n.load(strip(t))
    assert n.run(50_000_000) == 'done'
    got = number_of(canon(readback(n, n.mate[(root, 0)])))
    assert got and got[1] == 'u', got
    return got[0]


def cause(key):
    k = key[0]
    if k == 'copy' and key[3].startswith('ref '): return 'copy of a ref'
    if k == 'erase' and key[3].startswith('ref '): return 'erase of a ref (an arm handed back)'
    if k in ('O⋈ref', 'R⋈ref'): return 'laying out a definition'
    if k == 'T⋈ref': return 'thaw of a ref'
    return programs.cause(key)


def measure(prog, inp, out=print, rows=True):
    t = (term(DEFS, prog), label(term(DEFS, inp), 'IN:'))
    n = RefNet()
    root = n.load(t)
    assert n.run(50_000_000) == 'done'
    assert not n.stats['annihilated across two copies']
    total = n.stats['interactions']
    reads = sum(v for k, v in n.acct.items() if k[0].startswith('R⋈') and k[0] != 'R⋈ref')
    by_kind, by_cause = Counter(), defaultdict(Counter)
    for key, v in n.acct.items():
        by_kind[kind_of(key)] += v
        by_cause[cause(key)][kind_of(key)] += v
    setup = sum(by_cause['setup: evaluating the input'].values())
    got, want = answer(prog, inp), programs.answer('size', inp)
    out(f'{prog:9} {inp:7} {total:8} rewrites {reads:4} reads  {total / reads:7.1f} a read ({(total - setup) / reads:6.1f} without evaluating the input)'
        f'  answer {got} {"= size" if got == want else f"!= size ({want})"}  '
        + ' '.join(f'{k} {by_kind[k] / reads:.1f}' for k in KINDS) + f'  | agents laid out by refs {n.laid_out / reads:.1f} a read')
    if rows:
        for c, kinds in sorted(by_cause.items(), key=lambda kv: -sum(kv[1].values())):
            s = sum(kinds.values())
            out(f'      {c:50} {s / reads:7.1f}  ' + ' '.join(f'{k} {kinds[k] / reads:.1f}' for k in KINDS if kinds[k]))
    return total, reads, got == want


if __name__ == '__main__':
    for p in sys.argv[1:] or list(TO_NUMBER):
        for i in programs.INPUTS:
            measure(p, i, rows=(i == 'size'))
        print()
