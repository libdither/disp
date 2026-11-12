"""The same calculus as plain term rewriting, written independently of the nets, to test them against.

Code: bound names, (f, a), ('λ', x, body), ('Λ', x, body) an opened function written out (applying it
hands back its body as code, so it works as a quote), 'ρ', ('~', value) a splice of a closed value.
Values: ('λ', x, code), ('Λ', x, code), 'ρ', and code values: (c1, c2) or ('~', value), which are closed
code that nothing runs. Every program is closed: a name nothing binds is an error.
Running is call by value and never goes under a binder."""
import re
import net
from net import view_term


class Stuck(Exception):
    pass


class Fuel(Exception):
    pass


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
        if f == 'ρ':
            return self.view(v)
        raise Stuck(f'applying {kind(f)}')

    def view(self, v):
        if isinstance(v, tuple) and v[0] == 'λ':
            return fill(view_term('fun'), {'F': ('Λ', v[1], v[2])})
        if isinstance(v, tuple) and v[0] == 'Λ':
            return fill(view_term('opened'), {'F': ('λ' if net.VARIANT == 'toggle' else 'Λ', v[1], v[2])})
        if v == 'ρ':
            return fill(view_term('prim'), {})
        if isinstance(v, tuple) and v[0] == '~':
            return fill(view_term('var'), {'w': v[1]})
        if isinstance(v, tuple) and len(v) == 2:
            return fill(view_term('app'), {'f': v[0], 'a': v[1]})
        raise Stuck(f'reflecting {kind(v)}')


def kind(v):
    if v == 'ρ': return 'ρ'
    return v[0] if v[0] in ('λ', 'Λ', '~') else 'app'


def canon(t, names=None):
    """rename binders x0, x1, ... in the order a pre-order walk meets them (the order readback names them in)"""
    names = {} if names is None else names
    def go(t, env):
        if isinstance(t, str):
            return env.get(t, t)
        if t[0] == '~': return ('~', go(t[1], {}))
        if len(t) == 3:
            x = f'x{len(names)}'
            names[x] = 1
            return (t[0], x, go(t[2], {**env, t[1]: x}))
        return (go(t[0], env), go(t[1], env))
    return go(t, {})


BIT = (('λ', 'p', ('λ', 'q', ('λ', 'k', (('k', 'p'), 'q')))), ('λ', 'p', ('λ', 'q', ('λ', 'k', (('k', 'q'), 'p')))))


def unary(n):
    """the linear unary number n written out: λs z. s (rest s z) per successor, around λs z. z s"""
    t = ('λ', 's', ('λ', 'z', ('z', 's')))
    for _ in range(n): t = ('λ', 's', ('λ', 'z', ('s', ((t, 's'), 'z'))))
    return t


def binary(n, width):
    """the linear binary number n in `width` bits written out, lowest bit outermost: λf n. f bit (rest f n)
    per bit, around λf n. n f"""
    t = ('λ', 'f', ('λ', 'n', ('n', 'f')))
    for i in reversed(range(width)): t = ('λ', 'f', ('λ', 'n', (('f', BIT[(n >> i) & 1]), ((t, 'f'), 'n'))))
    return t


def tree(n, depth):
    """the linear tree number n written out: λk. k bits loop. The bits are a tree of pairs λk. k low high
    `depth` levels deep with a bit at each leaf, the lowest bit leftmost; the loop is the depth,
    λs z. s (… (s z)) with one s for each level"""
    def bits(lo, d):
        if not d: return BIT[(n >> lo) & 1]
        return ('λ', 'k', (('k', bits(lo, d - 1)), bits(lo + (1 << (d - 1)), d - 1)))
    loop = 'z'
    for _ in range(depth): loop = ('s', loop)
    return ('λ', 'k', (('k', bits(0, depth)), ('λ', 's', ('λ', 'z', loop))))


# ---- the numbers of the ordinary programs, and of the add whose two numbers have different shapes ----
TRUE, FALSE = ('λ', 't', ('λ', 'e', 't')), ('λ', 't', ('λ', 'e', 'e'))
END = ('λ', 'k', 'k')

def bits(v, w): return [(v >> i) & 1 for i in range(w)]

def scott(n):
    """the unary number n the usual way: λz s. z for zero, λz s. s rest for a successor"""
    t = ('λ', 'z', ('λ', 's', 'z'))
    for _ in range(n): t = ('λ', 'z', ('λ', 's', ('s', t)))
    return t

def bool_list(bits):
    """a list of bits the usual way, lowest first: λn c. n for the end, λn c. c bit rest for a cell, with
    true and false for the bits"""
    t = ('λ', 'n', ('λ', 'c', 'n'))
    for b in reversed(bits): t = ('λ', 'n', ('λ', 'c', (('c', TRUE if b else FALSE), t)))
    return t

def arm_word(bits):
    """a number whose cells pick an arm by their bit: λo i n. o (rest o i n), or i"""
    t = ('λ', 'o', ('λ', 'i', ('λ', 'n', (('n', 'o'), 'i'))))
    for b in reversed(bits): t = ('λ', 'o', ('λ', 'i', ('λ', 'n', ('i' if b else 'o', (((t, 'o'), 'i'), 'n')))))
    return t

def chain(bits):
    """a chain of pairs, lowest bit first: λk. k bit rest, ended by λk. k"""
    t = END
    for b in reversed(bits): t = ('λ', 'k', (('k', BIT[b]), t))
    return t

# What a number is written with after its digits, and its term in a program whose list numbers have
# `width` bits and whose tree numbers are `depth` levels deep.
FORMS = {
    'u': lambda n, width, depth: unary(n),
    '': lambda n, width, depth: binary(n, width),
    't': lambda n, width, depth: tree(n, depth),
    's': lambda n, width, depth: scott(n),
    'c': lambda n, width, depth: bool_list(bits(n, width)),
    'a': lambda n, width, depth: arm_word(bits(n, width)),
    'p': lambda n, width, depth: chain(bits(n, width)),
}
LISTS = ('', 'c', 'a', 'p')         # the forms that are lists of bits: every one in a program has the same width


def value(t):
    """a term without the splices round it"""
    while isinstance(t, tuple) and len(t) == 2 and t[0] == '~': t = t[1]
    return t


def binders(t, n):
    """the n binders a value opens with and the body under them, or None"""
    xs = []
    for _ in range(n):
        t = value(t)
        if not (isinstance(t, tuple) and len(t) == 3 and t[0] == 'λ'): return None
        xs.append(t[1]); t = t[2]
    return xs, t


def spine(t):
    """a term as its head and the arguments it is applied to"""
    args = []
    while isinstance(t, tuple) and len(t) == 2 and t[0] != '~':
        args.append(t[1]); t = t[0]
    return t, args[::-1]


def bit_of(t):
    """0 or 1 for a value that is b0 or b1, else None"""
    got = binders(t, 3)
    if not got: return None
    (p, q, k), body = got
    return 0 if body == ((k, p), q) else 1 if body == ((k, q), p) else None


def tree_of(t):
    """(n, depth) for a value that is a tree of bits: a bit, or a pair λk. k low high of two trees of one
    depth; else None"""
    b = bit_of(t)
    if b is not None: return b, 0
    got = binders(t, 1)
    if not got: return None
    (k,), body = got
    head, args = spine(body)
    if head != k or len(args) != 2: return None
    lo, hi = tree_of(args[0]), tree_of(args[1])
    if not lo or not hi or lo[1] != hi[1]: return None
    return lo[0] | hi[0] << (1 << lo[1]), lo[1] + 1


def loop_of(t):
    """how many times a value λs z. s (… (s z)) runs its step, if it is one and runs it at all; else None"""
    got = binders(t, 2)
    if not got: return None
    (s, z), body = got
    times = 0
    while isinstance(body, tuple) and len(body) == 2 and body[0] == s: times, body = times + 1, body[1]
    return times if times and body == z else None


def number_of(t):
    """What number a value is: (n, 'u') for a linear unary one, (n, width) for a linear binary one, (n, 't')
    for a linear tree one, else None. The number with no cells reads as (0, 'u')."""
    got = binders(t, 1)
    if got:
        head, args = spine(got[1])
        if head == got[0][0] and len(args) == 2:
            bits = tree_of(args[0])
            if bits and bits[1] and loop_of(args[1]) == bits[1]: return bits[0], 't'
    n, width, successors = 0, 0, 0
    while True:
        got = binders(t, 2)
        if not got: return None
        (f, z), body = got
        if body == (z, f): break
        head, args = spine(body)
        if head != f or not args: return None
        rest, handed = spine(args[-1])
        if handed != [f, z]: return None
        if len(args) == 1: successors += 1
        elif len(args) == 2 and bit_of(args[0]) is not None:
            n |= bit_of(args[0]) << width; width += 1
        else: return None
        t = rest
    if successors and width: return None
    return (n, width) if width else (successors, 'u')


def parse(src):
    """\\x y. body, ^x y. body for opened functions, juxtaposition, parentheses; ρ (or rho) is the reflection
    primitive; name := term lines define names for the lines after them. A name nothing binds or defines
    is an error. A number is written with its form after its digits (see FORMS). The linear ones: 5u is
    unary, 5 a list of bits and 5t a tree of bits. The ordinary ones: 5s is unary and 5c a list of bits.
    And for the add whose two numbers have different shapes: 5a is a number whose cells pick an arm, 5p a
    chain of pairs. Every list of bits in a program is written with one bit more than the largest needs, so
    a sum of two fits, and every tree number with the fewest levels that hold as much."""
    defs = {}
    lines = [l.split('--')[0] for l in src.strip().split('\n')]
    chunks = []
    for l in lines:
        if not l.strip(): continue
        if ':=' in l or not chunks or not l[0].isspace(): chunks.append(l)
        else: chunks[-1] += ' ' + l
    chunks = [(ch.split(':=', 1) if ':=' in ch else (None, ch)) for ch in chunks]
    chunks = [(name, re.findall(r"\\|\^|\.|\(|\)|[A-Za-z_][A-Za-z0-9_']*|\d+[utscap]?|ρ", body)) for name, body in chunks]
    written = [(int(t.rstrip('utscap')), t.lstrip('0123456789')) for _, toks in chunks for t in toks if t[0].isdigit()]
    largest = lambda forms: max([n.bit_length() for n, form in written if form in forms], default=0)
    width, depth = largest(LISTS) + 1, max(1, largest(('t',)).bit_length())   # 2 ** depth leaves: one more than the largest needs, at least
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
    while i < len(toks) and toks[i] != ')':
        if toks[i] in ('\\', '^'):
            a, i = expr(toks, i, defs, bound)
        elif toks[i] == '(':
            a, i = expr(toks, i + 1, defs, bound)
            i += 1
        else:
            name = toks[i]
            if isinstance(name, tuple): a = name   # a number, already a term
            elif name in ('ρ', 'rho'): a = 'ρ'
            elif name in bound: a = name
            elif name in defs: a = defs[name]
            else: raise NameError(f'nothing binds {name}')
            i += 1
        head = a if head is None else (head, a)
    return head, i
