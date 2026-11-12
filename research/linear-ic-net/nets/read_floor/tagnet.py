"""net.Net with every agent tagged by where it came from, and every rewrite charged to a cause.

Tags:
  - an agent emitted from source carries the label of the innermost named definition around it
    ('size', 'Z', 'suc', ...); the arms of a read `ρ c A1 … A5` get '<label>.arm<i>'; an input
    (the code being read) is loaded with every label prefixed 'IN:'.
  - a sharer or an eraser made for a binder carries '<label>:<variable>' (the variable it splits or drops).
  - an agent a rule makes inherits the tag of the code agent of the pair (a copy keeps the tag of what it
    copies; a T or O the tag of the app or lam it came from; the agents of a view are tagged 'view';
    an eraser made by erasing keeps the tag of the eraser that made it, so an eraser's tag always names
    the binder that first dropped something).
  - every copy operation (one per sharer that fires) remembers the sharer's tag: the variable whose second
    use caused it. Each dup carries its operation, so copy, cancel and erase-dup rewrites are charged to it.
  - a copy or an erase that has gone through a var's s port (into a value a closure captured) is 'deep'.

`acct` counts rewrites by key:
  ('copy', why, deep, what)       why = the sharer that started the copy, what = tag of the agent copied
  ('cancel', why)                 dup ⋈ dup of that copy
  ('erase', who, deep, what)      who = the binder that dropped it, what = tag of the agent erased
  ('erase-dup', who, why)         an eraser meets a copy in flight (copying code that holds an eraser, or
                                  erasing something still being copied)
  (rule, tag)                     every other rewrite, charged to the code agent of the pair
"""
import sys, re
from collections import Counter
sys.path.insert(0, '..')
import net
from net import Net, ARITY, copy_rule, erase_rule, reflect_with
from ref import FORMS, expr

sys.setrecursionlimit(1000000)


class Ops:
    def __init__(self): self.i = -1
    def __iter__(self): return self
    def __next__(self):
        self.i += 1
        return self.i


class TagNet(Net):
    def __init__(self, seed=None):
        super().__init__(seed)
        self.tag = {}
        self.cur = '?'
        self.deep_new = False
        self.deep = set()
        self.static = set()      # erasers that sit on an unused binder (emitted, or copied along with code)
        self.ops = Ops()
        self.why = {}            # copy operation -> tag of the sharer that started it
        self.acct = Counter()
        self.read_tag = None
        self.static_new = False

    def new(self, kind):
        a = super().new(kind)
        self.tag[a] = self.cur
        if self.deep_new and kind in ('dup', 'era'): self.deep.add(a)
        if self.static_new and kind == 'era': self.static.add(a)
        return a

    # ---- emit with labels ----
    def emit(self, t, env):
        if isinstance(t, tuple) and len(t) == 2 and t[0] == '#':
            return self.emit_ref(t[1])
        if isinstance(t, tuple) and len(t) == 3 and t[0] == '@':
            saved = self.cur
            self.cur = t[1]
            try: return self.emit(t[2], env)
            finally: self.cur = saved
        if t == 'ρ':
            return (self.new('rho'), 0)
        if isinstance(t, tuple) and len(t) == 2 and t[0] == '~':
            v = self.new('var')
            self.link((v, 1), t[1])
            return (v, 0)
        if isinstance(t, tuple) and len(t) == 3:
            binder, x, body = t
            lam = self.new('Lam' if binder == 'Λ' else 'lam')
            occ = []
            self.link((lam, 2), self.emit(body, {**env, x: occ}))
            saved = self.cur
            self.cur = f'{saved}:{x}'
            self.bind((lam, 1), occ)
            if not occ: self.static.add(self.mate[(lam, 1)][0])
            self.cur = saved
            return (lam, 0)
        if isinstance(t, tuple):
            head, args = t, []
            while isinstance(head, tuple) and len(head) == 2 and head[0] != '~':
                args.append(head[1]); head = head[0]
            args.reverse()
            if head == 'ρ' and len(args) >= 6 and not (isinstance(args[1], tuple) and args[1][0] == '@' and '.arm' in args[1][1]):
                for i in range(1, 6): args[i] = ('@', f'{self.cur}.arm{i}', args[i])
                t = head
                for a in args: t = (t, a)
            a = self.new('app')
            self.link((a, 1), self.emit(t[0], env))
            self.link((a, 2), self.emit(t[1], env))
            return (a, 0)
        if t not in env: raise NameError(f'nothing binds {t}')
        v = self.new('var')
        env[t].append((v, 1))
        return (v, 0)

    # ---- charge each rewrite ----
    def fire(self, a, b):
        for (x, y) in ((a, b), (b, a)):
            kx, ky = self.kind[x], self.kind[y]
            f = net.RULES.get((kx, ky))
            if f is not None: break
            if kx in ('sig', 'dup') and ky not in ('dup', 'era'): f = copy_rule; break
            if kx == 'era': f = erase_rule; break
        else:
            return super().fire(a, b)          # raises Stuck
        tx, ty = self.tag[x], self.tag[y]
        self.deep_new = self.static_new = False
        if f is copy_rule or getattr(f, 'copy_like', False):
            if kx == 'sig':
                op = self.ops.i + 1
                self.why[op] = tx
            else:
                op = self.op[x]
            d = x in self.deep
            self.acct[('copy', self.why[op], d, ty)] += 1
            self.cur = ty
            self.deep_new = d or ky == 'var'
        elif f is erase_rule:
            if ky == 'era' and x in self.static and y not in self.static:
                x, y, tx, ty = y, x, ty, tx               # the eraser that moves is the one doing the erasing
            d = x in self.deep
            if ky == 'dup':
                self.acct[('erase-dup', tx, self.why[self.op[y]])] += 1
                if x in self.static: self.static_new = True
            else:
                self.acct[('erase', tx, d, ty if ky != 'era' else 'era')] += 1
            self.cur = tx
            self.deep_new = d or ky == 'var'
        elif kx == 'dup' and ky == 'dup':
            self.acct[('cancel', self.why[self.op[x]])] += 1
        else:
            code = y if kx in ('O', 'T', 'R') else x
            other = x if code == y else y
            self.acct[(f'{self.kind[other]}⋈{self.kind[code]}', self.tag[code])] += 1
            self.cur = 'view' if 'R' in (kx, ky) else self.tag[code]
            self.read_tag = self.tag[code]
        try:
            return super().fire(a, b)
        finally:
            self.deep_new = self.static_new = False


# R ⋈ lam / Lam: the opened (closed) function keeps the tag of the function read, not 'view'
def _function_as(kind):
    def parts(n, v, slot):
        if isinstance(n, TagNet):
            saved = n.cur
            n.cur = n.read_tag
            f = n.new(kind())
            n.cur = saved
        else:
            f = n.new(kind())
        n.link((f, 1), slot(v, 1))
        n.link((f, 2), slot(v, 2))
        return {'F': (f, 0)}
    return parts
net.RULES[('R', 'lam')] = reflect_with('fun', _function_as(lambda: 'Lam'))
net.RULES[('R', 'Lam')] = reflect_with('opened', _function_as(lambda: 'lam'))


# ---- source with labels ----
def label(t, prefix):
    """t with every '@' label prefixed"""
    if isinstance(t, str) or t is None: return t
    if t[0] == '@': return ('@', prefix + t[1], label(t[2], prefix))
    if t[0] == '~': return t
    if len(t) == 3: return (t[0], t[1], label(t[2], prefix))
    return (label(t[0], prefix), label(t[1], prefix))


def strip(t):
    if isinstance(t, str): return t
    if t[0] == '@': return strip(t[2])
    if t[0] == '~': return t
    if len(t) == 3: return (t[0], t[1], strip(t[2]))
    return (strip(t[0]), strip(t[1]))


TOKENS = r"\\|\^|\.|\(|\)|[A-Za-z_][A-Za-z0-9_']*|\d+[utscap]?|ρ"

def parse_defs(src):
    """name := term lines, as ref.parse reads them; each definition is wrapped in ('@', name, term).
    Returns the dict of definitions."""
    defs = {}
    lines = [l.split('--')[0] for l in src.strip().split('\n')]
    chunks = []
    for l in lines:
        if not l.strip(): continue
        if ':=' in l or not chunks or not l[0].isspace(): chunks.append(l)
        else: chunks[-1] += ' ' + l
    chunks = [(ch.split(':=', 1) if ':=' in ch else (None, ch)) for ch in chunks]
    for name, body in chunks:
        toks = [FORMS[tk.lstrip('0123456789')](int(tk.rstrip('utscap')), 4, 1) if tk[0].isdigit() else tk
                for tk in re.findall(TOKENS, body)]
        t, i = expr(toks, 0, defs, ())
        assert i == len(toks), f'unparsed: {toks[i:]}'
        if name: defs[name.strip()] = ('@', name.strip(), t)
    return defs


def term(defs, src):
    t, i = expr(re.findall(TOKENS, src), 0, defs, ())
    return t


def run(t, limit=50_000_000):
    n = TagNet()
    root = n.load(t)
    status = n.run(limit)
    assert status == 'done', status
    assert not n.stats['annihilated across two copies']
    return n, root
