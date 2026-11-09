"""The same calculus as plain term rewriting, written independently of the nets, to test them against.

Code: variable names, (f, a), ('λ', x, body), 'ρ', ('~', value) a splice of a closed value.
Values: ('λ', x, code), ('Λ', x, code), 'ρ', ('sym', name), and code values: (c1, c2) or ('~', value),
which are closed code that nothing runs.
Running is call by value and never goes under a binder."""
import re
from net import view_term


class Stuck(Exception):
    pass


class Fuel(Exception):
    pass


def close(t, bound=()):
    """free names become splices of symbols"""
    if isinstance(t, str):
        if t == 'ρ' or t in bound: return t
        return ('~', ('sym', t))
    if t[0] == '~': return t
    if len(t) == 3: return (t[0], t[1], close(t[2], bound + (t[1],)))
    return (close(t[0], bound), close(t[1], bound))


def subst(c, x, s):
    if c == x: return s
    if isinstance(c, str): return c
    if c[0] == '~': return c
    if len(c) == 3: return c if c[1] == x else (c[0], c[1], subst(c[2], x, s))
    return (subst(c[0], x, s), subst(c[1], x, s))


def fill(t, parts):
    if isinstance(t, str): return t
    if t[0] == '~': return ('~', parts[t[1]])
    if len(t) == 3: return (t[0], t[1], fill(t[2], parts))
    return (fill(t[0], parts), fill(t[1], parts))


class Machine:
    def __init__(self, fuel):
        self.fuel = fuel
        self.betas = 0

    def run(self, c):
        if isinstance(c, str):
            if c == 'ρ': return 'ρ'
            raise AssertionError(f'free variable {c} while running')
        if c[0] == '~': return c[1]
        if c[0] == 'λ': return c
        return self.apply(self.run(c[0]), self.run(c[1]))

    def apply(self, f, v):
        self.fuel -= 1
        if self.fuel < 0: raise Fuel
        if isinstance(f, tuple) and f[0] == 'λ':
            self.betas += 1
            return self.run(subst(f[2], f[1], ('~', v)))
        if isinstance(f, tuple) and f[0] == 'Λ':
            return subst(f[2], f[1], ('~', v))
        if f == 'ρ':
            return self.view(v)
        raise Stuck(f'applying {kind(f)}')

    def view(self, v):
        if isinstance(v, tuple) and v[0] in ('λ', 'Λ'):
            return fill(view_term('fun'), {'F': ('Λ', v[1], v[2])})
        if v == 'ρ':
            return fill(view_term('prim'), {})
        if isinstance(v, tuple) and v[0] == '~':
            return fill(view_term('var'), {'w': v[1]})
        if isinstance(v, tuple) and len(v) == 2 and v[0] != 'sym':
            return fill(view_term('app'), {'f': v[0], 'a': v[1]})
        raise Stuck(f'reflecting {kind(v)}')


def kind(v):
    if v == 'ρ': return 'ρ'
    return v[0] if v[0] in ('λ', 'Λ', '~', 'sym') else 'app'


def canon(t, names=None):
    """rename binders x0, x1, ... in the order a pre-order walk meets them (the order readback names them in)"""
    names = {} if names is None else names
    def go(t, env):
        if isinstance(t, str):
            return env.get(t, t)
        if t[0] == 'sym': return t
        if t[0] == '~': return ('~', go(t[1], {}))
        if len(t) == 3:
            x = f'x{len(names)}'
            names[x] = 1
            return (t[0], x, go(t[2], {**env, t[1]: x}))
        return (go(t[0], env), go(t[1], env))
    return go(t, {})


def parse(src):
    """\\x y. body, juxtaposition, parentheses; ρ (or rho) is the reflection primitive; name := term lines define."""
    defs = {}
    lines = [l.split('--')[0] for l in src.strip().split('\n')]
    chunks = []
    for l in lines:
        if not l.strip(): continue
        if ':=' in l or not chunks or not l[0].isspace(): chunks.append(l)
        else: chunks[-1] += ' ' + l
    out = None
    for ch in chunks:
        name, body = (ch.split(':=', 1) if ':=' in ch else (None, ch))
        toks = re.findall(r"\\|\.|\(|\)|[A-Za-z_][A-Za-z0-9_']*|ρ", body)
        t, i = expr(toks, 0, defs)
        assert i == len(toks), f'unparsed: {toks[i:]}'
        if name: defs[name.strip()] = t
        else: out = t
    return out


def expr(toks, i, defs):
    if toks[i] == '\\':
        xs = []
        i += 1
        while toks[i] != '.':
            xs.append(toks[i]); i += 1
        body, i = expr(toks, i + 1, defs)
        for x in reversed(xs): body = ('λ', x, body)
        return body, i
    head = None
    while i < len(toks) and toks[i] != ')':
        if toks[i] == '\\':
            a, i = expr(toks, i, defs)
        elif toks[i] == '(':
            a, i = expr(toks, i + 1, defs)
            i += 1
        else:
            a = 'ρ' if toks[i] in ('ρ', 'rho') else defs.get(toks[i], toks[i])
            i += 1
        head = a if head is None else (head, a)
    return head, i
