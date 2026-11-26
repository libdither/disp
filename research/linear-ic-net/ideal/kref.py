"""The quote-and-table calculus as plain term rewriting: the reference the candidate net (knet.py) is tested against.

Code (four kinds of node, plus the table):
  x            a bound name
  (f, a)       an application
  ('λ', x, b)  a function
  ('~', t)     a quote: when run it hands back the code t as a value, unrun. Written 'M. A bound name inside t
               is still bound outside, so a quote is also what a variable becomes once it has a value (~v).
  'ρ'          the atom
  ('tab', k1..k5)   a table, one slot per kind of value: function, application, quote, atom, table.
                    Written [k1 | k2 | k3 | k4 | k5].
Values: a function, ρ, a table whose slots are values, and closed code (an application or a quote).
Running is call by value and never goes under a binder.
  f v        f a function: its body runs with x := ~v
  t v        t a table: the code  ~k ~t ~p1 … ~pn  runs, where k is the slot for v's kind and p1 … pn are
             v's parts (a function: itself; an application: its two sides; a quote: what it quotes;
             ρ: nothing; a table: its five slots). So the slot is applied to the table, then to the parts.
  ~g v       opening: the code that g v would run, handed back unrun. g a function: its body with x := ~v.
             g a table: the code ~k ~g ~p1 … ~pn. Anything else: stuck.
Applying ρ or an application (code) is stuck.
"""
import re, sys, os
sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', 'nets'))
from ref import FORMS, LISTS

NSLOT = 5
KINDS = ('fun', 'app', 'quote', 'atom', 'table')


class Stuck(Exception):
    pass


class Fuel(Exception):
    pass


def is_tab(t): return isinstance(t, tuple) and len(t) == NSLOT + 1 and t[0] == 'tab'
def is_quote(t): return isinstance(t, tuple) and len(t) == 2 and t[0] == '~'
def is_lam(t): return isinstance(t, tuple) and len(t) == 3 and t[0] == 'λ'
def is_app(t): return isinstance(t, tuple) and len(t) == 2 and t[0] != '~'


def subst(c, x, s):
    if c == x: return s
    if isinstance(c, str): return c
    if is_quote(c): return ('~', subst(c[1], x, s))
    if is_tab(c): return ('tab',) + tuple(subst(k, x, s) for k in c[1:])
    if is_lam(c): return c if c[1] == x else (c[0], c[1], subst(c[2], x, s))
    return (subst(c[0], x, s), subst(c[1], x, s))


def kind(v):
    """(index, parts) of a value, as a table sees it"""
    if is_lam(v): return 0, [v]
    if is_quote(v): return 2, [v[1]]
    if v == 'ρ': return 3, []
    if is_tab(v): return 4, list(v[1:])
    if is_app(v): return 1, [v[0], v[1]]
    raise AssertionError(f'not a value: {v}')


class Machine:
    def __init__(self, fuel):
        self.fuel = fuel
        self.betas = self.reads = self.opens = 0

    def run(self, c):
        if isinstance(c, str):
            if c == 'ρ': return 'ρ'
            raise AssertionError(f'free variable {c} while running')
        if is_quote(c): return c[1]
        if is_lam(c): return c
        if is_tab(c): return ('tab',) + tuple(self.run(k) for k in c[1:])
        return self.apply(self.run(c[0]), self.run(c[1]))

    def apply(self, f, v):
        self.fuel -= 1
        if self.fuel < 0: raise Fuel
        if is_lam(f):
            self.betas += 1
            return self.run(subst(f[2], f[1], ('~', v)))
        if is_quote(f):
            g = f[1]
            self.opens += 1
            if is_lam(g): return subst(g[2], g[1], ('~', v))
            if is_tab(g): return self.read(g, v)
            raise Stuck('opening what is neither a function nor a table')
        if is_tab(f):
            return self.run(self.read(f, v))
        raise Stuck('applying ρ' if f == 'ρ' else 'applying code')

    def read(self, tab, v):
        """the code a table runs on v: the slot for v's kind applied to the table, then to v's parts"""
        self.reads += 1
        i, parts = kind(v)
        c = (('~', tab[1 + i]), ('~', tab))
        for p in parts: c = (c, ('~', p))
        return c


def canon(t):
    """rename binders x0, x1, … in the order a pre-order walk meets them"""
    count = [0]
    def go(t, env):
        if isinstance(t, str): return env.get(t, t)
        if is_quote(t): return ('~', go(t[1], env))
        if is_tab(t): return ('tab',) + tuple(go(k, env) for k in t[1:])
        if is_lam(t):
            x = f'x{count[0]}'
            count[0] += 1
            return (t[0], x, go(t[2], {**env, t[1]: x}))
        return (go(t[0], env), go(t[1], env))
    return go(t, {})


def size(t):
    """nodes of a term"""
    if isinstance(t, str): return 1
    if is_quote(t): return 1 + size(t[1])
    if is_tab(t): return 1 + sum(size(k) for k in t[1:])
    if is_lam(t): return 1 + size(t[2])
    return 1 + size(t[0]) + size(t[1])


TOKEN = r"\\|\.|\(|\)|\[|\]|\||'|[A-Za-z_][A-Za-z0-9_]*|\d+[utscap]?|ρ"

def parse(src, given=None):
    """\\x y. body, juxtaposition, parentheses, 'M for a quote, [a | b | c | d | e] for a table, ρ (or rho);
    name := term lines define names for the lines after them. Numbers are written as in ../nets/ref.py.
    `given` maps names to terms built elsewhere (closed code of the older calculus, to be read)."""
    defs = dict(given or {})
    lines = [l.split('--')[0] for l in src.strip().split('\n')]
    chunks = []
    for l in lines:
        if not l.strip(): continue
        if ':=' in l or not chunks or not l[0].isspace(): chunks.append(l)
        else: chunks[-1] += ' ' + l
    chunks = [(ch.split(':=', 1) if ':=' in ch else (None, ch)) for ch in chunks]
    chunks = [(name, re.findall(TOKEN, body)) for name, body in chunks]
    written = [(int(t.rstrip('utscap')), t.lstrip('0123456789')) for _, toks in chunks for t in toks if t[0].isdigit()]
    largest = lambda forms: max([n.bit_length() for n, form in written if form in forms], default=0)
    width, depth = largest(LISTS) + 1, max(1, largest(('t',)).bit_length())
    def number(tok):
        return FORMS[tok.lstrip('0123456789')](int(tok.rstrip('utscap')), width, depth)
    out = None
    for name, toks in chunks:
        toks = [number(t) if t[0].isdigit() else t for t in toks]
        t, i = expr(toks, 0, defs, ())
        assert i == len(toks), f'unparsed: {toks[i:]}'
        if name: defs[name.strip()] = t
        else: out = t
    return out


def expr(toks, i, defs, bound):
    if toks[i] == '\\':
        xs = []
        i += 1
        while toks[i] != '.':
            xs.append(toks[i]); i += 1
        body, i = expr(toks, i + 1, defs, bound + tuple(xs))
        for x in reversed(xs): body = ('λ', x, body)
        return body, i
    head = None
    while i < len(toks) and toks[i] not in (')', ']', '|'):
        if toks[i] == '\\': a, i = expr(toks, i, defs, bound)
        else: a, i = atom(toks, i, defs, bound)
        head = a if head is None else (head, a)
    return head, i


def atom(toks, i, defs, bound):
    t = toks[i]
    if isinstance(t, tuple): return t, i + 1          # a number, already a term
    if t == "'":
        a, i = atom(toks, i + 1, defs, bound)
        return ('~', a), i
    if t == '(':
        a, i = expr(toks, i + 1, defs, bound)
        assert toks[i] == ')', f'expected ) at {toks[i:i + 5]}'
        return a, i + 1
    if t == '[':
        slots = []
        i += 1
        while True:
            a, i = expr(toks, i, defs, bound)
            slots.append(a)
            if toks[i] == ']': break
            assert toks[i] == '|', f'expected | at {toks[i:i + 5]}'
            i += 1
        assert len(slots) == NSLOT, f'a table has {NSLOT} slots, got {len(slots)}'
        return ('tab',) + tuple(slots), i + 1
    if t in ('ρ', 'rho'): return 'ρ', i + 1
    if t in bound: return t, i + 1
    if t in defs: return defs[t], i + 1
    raise NameError(f'nothing binds {t}')
