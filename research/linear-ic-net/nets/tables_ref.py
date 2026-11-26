"""The calculus of tables.py as plain term rewriting, written independently of the net, to test it against;
and the parser, ref.py's with keep[e1 | … | e6] added.

Code: bound names, (f, a), ('λ', x, body), ('Λ', x, body), 'ρ', ('keep', s1, …, s6) a table, ('~', value) a
splice of a closed value. Values: ('λ', x, code), ('Λ', x, code), 'ρ', a table whose slots are values, and
code values: (c1, c2) or ('~', value), closed code that nothing runs. Running is call by value and never goes
under a binder; a table runs its slots left to right. Applying a table to a value v runs the slot for v's kind
(function, opened function, application, spliced value, ρ, table) on the table itself and then v's parts: a
function opened, an opened function closed, f and a, the value, ρ, the six slots. Applying ρ, or code that is
not a function, is stuck."""
import re
from ref import FORMS, LISTS, Stuck, Fuel

NSLOT = 6


def is_tab(t):
    return isinstance(t, tuple) and len(t) == NSLOT + 1 and t[0] == 'keep'


def subst(c, x, s):
    if c == x: return s
    if isinstance(c, str): return c
    if c[0] == '~': return c
    if is_tab(c): return ('keep',) + tuple(subst(a, x, s) for a in c[1:])
    if len(c) == 3: return c if c[1] == x else (c[0], c[1], subst(c[2], x, s))
    return (subst(c[0], x, s), subst(c[1], x, s))


def kind_parts(v):
    """the slot a table runs for v, and the parts it hands that slot after the table"""
    if v == 'ρ': return 4, ['ρ']
    if is_tab(v): return 5, list(v[1:])
    if v[0] == 'λ': return 0, [('Λ', v[1], v[2])]
    if v[0] == 'Λ': return 1, [('λ', v[1], v[2])]
    if v[0] == '~': return 3, [v[1]]
    return 2, [v[0], v[1]]


class Machine:
    def __init__(self, fuel):
        self.fuel = fuel
        self.betas = 0

    def run(self, c):
        if isinstance(c, str):
            if c == 'ρ': return 'ρ'
            raise AssertionError(f'free variable {c} while running')
        if c[0] == '~': return c[1]
        if is_tab(c): return ('keep',) + tuple(self.run(a) for a in c[1:])
        if c[0] in ('λ', 'Λ'): return c
        return self.apply(self.run(c[0]), self.run(c[1]))

    def apply(self, f, v):
        self.fuel -= 1
        if self.fuel < 0: raise Fuel
        if isinstance(f, tuple) and f[0] == 'λ':
            self.betas += 1
            return self.run(subst(f[2], f[1], ('~', v)))
        if isinstance(f, tuple) and f[0] == 'Λ':
            return subst(f[2], f[1], ('~', v))
        if is_tab(f):
            i, parts = kind_parts(v)
            g = f[1 + i]
            for a in [f] + parts: g = self.apply(g, a)
            return g
        raise Stuck('applying ρ' if f == 'ρ' else 'applying code')


def canon(t):
    """rename binders x0, x1, ... in the order a pre-order walk meets them (the order readback names them in)"""
    names = {}
    def go(t, env):
        if isinstance(t, str): return env.get(t, t)
        if t[0] == '~': return ('~', go(t[1], {}))
        if is_tab(t): return ('keep',) + tuple(go(a, env) for a in t[1:])
        if len(t) == 3:
            x = f'x{len(names)}'
            names[x] = 1
            return (t[0], x, go(t[2], {**env, t[1]: x}))
        return (go(t[0], env), go(t[1], env))
    return go(t, {})


def run_ref(t, fuel=4000):
    m = Machine(fuel)
    try:
        return 'value', canon(m.run(t)), m.betas
    except Stuck:
        return 'stuck', None, m.betas
    except (Fuel, RecursionError):
        return 'fuel', None, m.betas


def binders(t):
    """how many λ and Λ a term has, splices and tables included"""
    if isinstance(t, str): return 0
    if t[0] == '~': return binders(t[1])
    if is_tab(t): return sum(binders(a) for a in t[1:])
    if len(t) == 3: return 1 + binders(t[2])
    return binders(t[0]) + binders(t[1])


def church(t):
    """the number a Church numeral value stands for, or None: the term run with Python functions"""
    def ev(t, env):
        if isinstance(t, str): return env[t]
        if t[0] == '~': return ev(t[1], {})
        if len(t) == 3 and t[0] == 'λ': return lambda v: ev(t[2], {**env, t[1]: v})
        if len(t) == 2: return ev(t[0], env)(ev(t[1], env))
        raise TypeError
    try:
        n = ev(t, {})(lambda k: k + 1)(0)
        return n if isinstance(n, int) else None
    except (TypeError, KeyError, RecursionError):
        return None


# ---- parsing: ref.parse, and keep[e1 | … | e6] a table ----
TOKENS = r"keep\[|\]|\||\\|\^|\.|\(|\)|[A-Za-z_][A-Za-z0-9_']*|\d+[utscap]?|ρ"
STOP = (')', '|', ']')

def parse(src):
    """ref.parse's language (\\x y. body, ^x y. body, juxtaposition, ρ, name := term lines, numbers by form), and
    keep[e1 | e2 | e3 | e4 | e5 | e6] a table, its slots for a function, an opened function, an application, a
    spliced value, ρ, and a table"""
    defs = {}
    lines = [l.split('--')[0] for l in src.strip().split('\n')]
    chunks = []
    for l in lines:
        if not l.strip(): continue
        if ':=' in l or not chunks or not l[0].isspace(): chunks.append(l)
        else: chunks[-1] += ' ' + l
    chunks = [(ch.split(':=', 1) if ':=' in ch else (None, ch)) for ch in chunks]
    chunks = [(name, re.findall(TOKENS, body)) for name, body in chunks]
    written = [(int(t.rstrip('utscap')), t.lstrip('0123456789')) for _, toks in chunks for t in toks if t[0].isdigit()]
    largest = lambda forms: max([n.bit_length() for n, form in written if form in forms], default=0)
    width, depth = largest(LISTS) + 1, max(1, largest(('t',)).bit_length())
    number = lambda tok: FORMS[tok.lstrip('0123456789')](int(tok.rstrip('utscap')), width, depth)
    out = None
    for name, toks in chunks:
        toks = [number(t) if t[0].isdigit() else t for t in toks]
        t, i = expr(toks, 0, defs, ())
        assert i == len(toks), f'unparsed: {toks[i:]}'
        if name: defs[name.strip()] = t
        else: out = t
    return out


def expr(toks, i, defs, bound):
    if toks[i] in ('\\', '^'):
        binder = 'λ' if toks[i] == '\\' else 'Λ'
        xs = []
        i += 1
        while toks[i] != '.':
            xs.append(toks[i]); i += 1
        body, i = expr(toks, i + 1, defs, bound + tuple(xs))
        for x in reversed(xs): body = (binder, x, body)
        return body, i
    head = None
    while i < len(toks) and not (isinstance(toks[i], str) and toks[i] in STOP):
        tok = toks[i]
        if isinstance(tok, str) and tok in ('\\', '^'):
            a, i = expr(toks, i, defs, bound)
        elif tok == '(':
            a, i = expr(toks, i + 1, defs, bound)
            i += 1
        elif tok == 'keep[':
            slots = []
            i += 1
            while True:
                s, i = expr(toks, i, defs, bound)
                slots.append(s)
                if toks[i] == ']': i += 1; break
                assert toks[i] == '|', f'expected | or ], got {toks[i]}'
                i += 1
            assert len(slots) == NSLOT, f'a table has {NSLOT} slots, not {len(slots)}'
            a = ('keep',) + tuple(slots)
        else:
            if isinstance(tok, tuple): a = tok           # a number, already a term
            elif tok in ('ρ', 'rho'): a = 'ρ'
            elif tok in bound: a = tok
            elif tok in defs: a = defs[tok]
            else: raise NameError(f'nothing binds {tok}')
            i += 1
        head = a if head is None else (head, a)
    return head, i


def source(t):
    """a term as program text"""
    if isinstance(t, str): return t
    if is_tab(t): return 'keep[' + ' | '.join(source(s) for s in t[1:]) + ']'
    if len(t) == 3: return f"({'^' if t[0] == 'Λ' else chr(92)}{t[1]}. {source(t[2])})"
    return f'({source(t[0])} {source(t[1])})'
