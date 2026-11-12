"""The kept table on closure agents: ../lazy_copy/engine.py's RefRun (every λ is clo_k(p; e1..en) naming a
static definition) plus one table agent and one reading agent.

  tab(p; a1..a6)        a table: six slots. In code its slots are code; laid out running or thawed, they are
                        values (usually closure agents). Written keep[e1 | … | e6].
  Ctab(p; a1..a6, r)    a table waiting for the value it reads.

Rules (on top of RefRun's):
  O ⋈ tab      Ctab(p → O's argument; the six slots; r → O's result)                              (1 rewrite)
  Ctab ⋈ X     X has kind i (fun clo, opened cloL, app, var, prim rho, table tab): a dup on slot i;
               one copy goes back into a rebuilt tab, the other is applied to (the rebuilt tab, then X's
               parts): fun → X as cloL, opened → X as clo, app → f a, var → w, prim → ρ, table → its six slots
  T ⋈ tab      the same tab with a thaw on each slot (a table in code is a constructor; slots evaluated)
  copy / erase of tab: the generic rules (1 rewrite, then one per slot)

So a table is a sixth kind of value for reading: its parts are its six slots, which makes the binders inside
its arms countable and lets an interpreter rebuild a table from interpreted slots.
The old λ-view ρ keeps its rules; ρ has no reading of a table (stuck, as in the reference).
"""
import sys, os, re, itertools
from collections import Counter
HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', 'lazy_copy'))
sys.path.insert(0, os.path.join(HERE, '..'))
import engine as E
from engine import Net, RefRun, Defs, bind, resolve, copy_rule
import ref as R
from ref import FORMS, LISTS

sys.setrecursionlimit(1000000)
NSLOT = 6
E.ARITY.update({'tab': NSLOT, 'Ctab': NSLOT + 1})
KIND = {'clo': 0, 'cloL': 1, 'app': 2, 'var': 3, 'rho': 4, 'tab': 5}
NPARTS = [1, 1, 2, 1, 1, NSLOT]
LIVE = ('O', 'T', 'R', 'Ctab', 'root', 'ph')


def is_tab(t):
    return isinstance(t, tuple) and len(t) == 3 and t[0] == 'tab'


class TDefs(Defs):
    """Defs, with tables inside λ bodies"""
    def intern(self, t):
        got = self.memo.get(id(t))
        if got is not None and got[0] is t: return got[1], got[2]
        fvs, names, has_port = [], [], [False]
        def go(t, bound):
            if t == 'ρ': return t
            if isinstance(t, str):
                if t not in bound and t not in fvs: fvs.append(t); names.append(t)
                return t
            if t[0] == '~':
                has_port[0] = True
                fvs.append(t); nm = f'#{len(names)}'; names.append(nm)
                return nm
            if is_tab(t): return ('tab', t[1], tuple(go(a, bound) for a in t[2]))
            if len(t) == 3: return (t[0], t[1], go(t[2], bound | {t[1]}))
            return (go(t[0], bound), go(t[1], bound))
        norm = go(t, frozenset())
        key = (norm, tuple(names))
        k = self.key.get(key)
        if k is None:
            k = self.key[key] = len(self.table)
            self.table.append((norm[0], norm[1], norm[2], tuple(names)))
        if not has_port[0]: self.memo[id(t)] = (t, k, fvs)
        return k, fvs


def rb_term(body, env, fresh):
    if body == 'ρ': return body
    if isinstance(body, str): return env[body]
    if is_tab(body): return ('tab', body[1], tuple(rb_term(a, env, fresh) for a in body[2]))
    if len(body) == 3:
        x = fresh()
        return (body[0], x, rb_term(body[2], {**env, body[1]: x}, fresh))
    return (rb_term(body[0], env, fresh), rb_term(body[1], env, fresh))


def new_dup(n):
    d = n.new('dup'); n.op[d] = next(n.ops)
    return d


class TabRun(RefRun):
    """RefRun + the kept table (see the module docstring). policy 'keep' is the design; 'hand' (the taken slot
    is consumed and handed the other five slots, then the parts; no copy) is kept only to compare linear readers."""
    name = 'tab-run'
    def __init__(self, policy='keep'):
        super().__init__()
        self.policy = policy
        self.defs = TDefs()
        self.R.update({('O', 'tab'): self.start, ('T', 'tab'): self.thaw_tab})
        for k in KIND: self.R[('Ctab', k)] = self.read

    def label(self, kx, ky, f):
        if kx == 'O' and ky == 'tab': return 'read O⋈tab'
        if kx == 'Ctab': return f'read Ctab⋈{ky}'
        return super().label(kx, ky, f)

    @staticmethod
    def start(n, p, q):
        o, tb = p[0], q[0]
        def build(slot):
            c = n.new('Ctab')
            n.link((c, 0), slot(o, 1))
            for j in range(1, NSLOT + 1): n.link((c, j), slot(tb, j))
            n.link((c, NSLOT + 1), slot(o, 2))
        n.rewrite(p, q, build)

    @staticmethod
    def thaw_tab(n, p, q):
        t, v = p[0], q[0]
        def build(slot):
            w = n.new('tab')
            n.link((w, 0), slot(t, 1))
            for j in range(1, NSLOT + 1):
                th = n.new('T'); n.link((th, 0), slot(v, j)); n.link((th, 1), (w, j))
        n.rewrite(p, q, build)

    def read(self, n, p, q):
        c, x = p[0], q[0]
        kx = n.kind[x]
        i = KIND[kx]
        ar, tag = n.ar[x], n.tag.get(x)
        n.stats['reads'] += 1
        def build(slot):
            if i in (0, 1):
                f = n.new('cloL' if i == 0 else 'clo', ar, tag)
                for j in range(1, ar + 1): n.link((f, j), slot(x, j))
                parts = [(f, 0)]
            elif i == 4: parts = [(n.new('rho'), 0)]
            else: parts = [slot(x, j) for j in range(1, ar + 1)]
            arms = [slot(c, j) for j in range(1, NSLOT + 1)]
            if self.policy == 'keep':
                d = new_dup(n)
                n.link((d, 0), arms[i])
                t = n.new('tab')
                for j in range(NSLOT): n.link((t, j + 1), (d, 1) if j == i else arms[j])
                head, args = (d, 2), [(t, 0)] + parts
            else:
                head, args = arms[i], [arms[j] for j in range(NSLOT) if j != i] + parts
            cur = head
            for a in args:
                o = n.new('O'); n.link((o, 0), cur); n.link((o, 1), a); cur = (o, 2)
            n.link(cur, slot(c, NSLOT + 1))
        n.rewrite(p, q, build)

    def emit(self, n, t, env):
        if is_tab(t):
            a = n.new('tab')
            for j, s in enumerate(t[2]): n.link((a, j + 1), self.emit(n, s, env))
            return (a, 0)
        return super().emit(n, t, env)

    def emit_running(self, n, t, env, dest):
        if is_tab(t):
            a = n.new('tab')
            n.link(dest, (a, 0))
            for j, s in enumerate(t[2]): self.emit_running(n, s, env, (a, j + 1))
            return
        super().emit_running(n, t, env, dest)

    def readback(self, n, port, names=None):
        names = {} if names is None else names
        a = port[0]
        k = n.kind[a]
        if port[1] != 0: raise ValueError(f'not a value: port {port} of {k}')
        if k == 'tab':
            return ('tab', 'keep', tuple(self.readback(n, n.mate[(a, j)], names) for j in range(1, NSLOT + 1)))
        if k in ('clo', 'cloL'):
            _, x, body, nms = self.defs.table[n.tag[a]]
            env = {nm: resolve(n, (a, i + 1), names, self.readback) for i, nm in enumerate(nms)}
            fresh = lambda: f'y{next(n.ids)}'
            return rb_term(('Λ' if k == 'cloL' else 'λ', x, body), env, fresh)
        r = self.rb_common(n, port, names)
        if r is None: raise ValueError(f'not finished: {k} at the result')
        return r


# ---------------------------------------------------------------- the reference: term rewriting with tables
def subst(c, x, s):
    if c == x: return s
    if isinstance(c, str): return c
    if c[0] == '~': return c
    if is_tab(c): return ('tab', c[1], tuple(subst(a, x, s) for a in c[2]))
    if len(c) == 3: return c if c[1] == x else (c[0], c[1], subst(c[2], x, s))
    return (subst(c[0], x, s), subst(c[1], x, s))


def kind_parts(v):
    if v == 'ρ': return 4, ['ρ']
    if is_tab(v): return 5, list(v[2])
    if v[0] == 'λ': return 0, [('Λ', v[1], v[2])]
    if v[0] == 'Λ': return 1, [('λ', v[1], v[2])]
    if v[0] == '~': return 3, [v[1]]
    return 2, [v[0], v[1]]


class Machine(R.Machine):
    """call by value; a table term evaluates its six slots left to right (a constructor). A table value holds
    its slot values in place (they are never run again). Applying a table to v: slot kind(v) applied to the
    table, then v's parts. A table in code applied (it is a value, as a λ in code is) uses its slots as they are."""
    def run(self, c):
        if isinstance(c, str):
            if c == 'ρ': return 'ρ'
            raise AssertionError(f'free variable {c} while running')
        if c[0] == '~': return c[1]
        if is_tab(c): return ('tab', c[1], tuple(self.run(a) for a in c[2]))
        if c[0] in ('λ', 'Λ'): return c
        return self.apply(self.run(c[0]), self.run(c[1]))

    def apply(self, f, v):
        self.fuel -= 1
        if self.fuel < 0: raise R.Fuel
        if isinstance(f, tuple) and f[0] == 'λ':
            self.betas += 1
            return self.run(subst(f[2], f[1], ('~', v)))
        if isinstance(f, tuple) and f[0] == 'Λ': return subst(f[2], f[1], ('~', v))
        if f == 'ρ':
            if is_tab(v): raise R.Stuck('the λ-view has no reading of a table')
            return self.view(v)
        if is_tab(f):
            i, parts = kind_parts(v)
            g = f[2][i]
            for a in [f] + parts: g = self.apply(g, a)
            return g
        raise R.Stuck('applying code')


def canon(t):
    names = {}
    def go(t, env):
        if isinstance(t, str): return env.get(t, t)
        if t[0] == '~': return ('~', go(t[1], {}))
        if is_tab(t): return ('tab', 'keep', tuple(go(a, env) for a in t[2]))
        if len(t) == 3:
            x = f'x{len(names)}'; names[x] = 1
            return (t[0], x, go(t[2], {**env, t[1]: x}))
        return (go(t[0], env), go(t[1], env))
    return go(t, {})


def run_ref(t, fuel=4000):
    m = Machine(fuel)
    try:
        return 'value', canon(m.run(t)), m.betas
    except R.Stuck:
        return 'stuck', None, m.betas
    except (R.Fuel, MemoryError, RecursionError):
        return 'fuel', None, m.betas


# ---------------------------------------------------------------- running the net
def reachable(n, root):
    seen, todo = set(), [root]
    while todo:
        a = todo.pop()
        if a in seen: continue
        seen.add(a)
        for i in range(n.ar[a] + 1):
            m = n.mate.get((a, i))
            if m is not None: todo.append(m[0])
    return len(seen)


def run(t, seed=None, limit=10 ** 7, design=None):
    D = design or TabRun()
    n = Net(D, seed)
    root = n.load(t)
    status = n.run(limit)
    res = None
    if status == 'done':
        try:
            res = canon(D.readback(n, n.mate[(root, 0)]))
        except ValueError as e:
            return f'readback: {e}', None, n
        garbage = len(n.kind) - reachable(n, root)
        if garbage: status = f'garbage {garbage}'
    return status, res, n


def problems(n):
    out = []
    if n.stats['annihilated across two copies']: out.append('annihilated across copies')
    for k in n.stats:
        w = k.split()
        if w[0] in ('copy', 'erase') and len(w) > 1 and w[1] in LIVE: out.append(k)
    return out


GROUPS = ['copy', 'erase', 'annihilate', 'layout', 'read', 'rest']
def split(stats):
    """copy (a copier meets an agent), erase, annihilate (dup ⋈ dup), layout (O ⋈ clo/cloL: a body laid out),
    read (O ⋈ tab, Ctab ⋈ X, O ⋈ rho, R ⋈ X), rest (thaws, O ⋈ code that is not a closure)"""
    c = Counter()
    for k, v in stats.items():
        if k in ('interactions', 'annihilated across two copies', 'erased a fresh copy', 'expanded agents', 'reads'): continue
        w = k.split()
        if w[0] == 'copy': c['copy'] += v
        elif w[0] == 'erase': c['erase'] += v
        elif k == 'dup⋈dup': c['annihilate'] += v
        elif k == 'expand': c['layout'] += v
        elif w[0] == 'read' or k.startswith('R⋈') or k == 'O⋈rho': c['read'] += v
        else: c['rest'] += v
    assert sum(c.values()) == stats['interactions'], (c, stats['interactions'])
    return c


def nreads(stats):
    return stats['reads'] + sum(v for k, v in stats.items() if k.startswith('R⋈'))


# ---------------------------------------------------------------- parsing: ref.parse plus keep[e1 | … | e6]
TOKENS = r"keep\[|\]|\||\\|\^|\.|\(|\)|[A-Za-z_][A-Za-z0-9_']*|\d+[utscap]?|ρ"

def parse(src):
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

STOP = (')', '|', ']')
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
            a, i = expr(toks, i + 1, defs, bound); i += 1
        elif tok == 'keep[':
            arms = []; i += 1
            while True:
                arm, i = expr(toks, i, defs, bound)
                arms.append(arm)
                if toks[i] == ']': i += 1; break
                assert toks[i] == '|'; i += 1
            assert len(arms) == NSLOT, f'a table has {NSLOT} slots, not {len(arms)}'
            a = ('tab', 'keep', tuple(arms))
        else:
            if isinstance(tok, tuple): a = tok
            elif tok in ('ρ', 'rho'): a = 'ρ'
            elif tok in bound: a = tok
            elif tok in defs: a = defs[tok]
            else: raise NameError(f'nothing binds {tok}')
            i += 1
        head = a if head is None else (head, a)
    return head, i


def in_thread(main):
    import threading, resource
    resource.setrlimit(resource.RLIMIT_AS, (3 << 30, 3 << 30))
    threading.stack_size(512 * 1024 * 1024)
    th = threading.Thread(target=main); th.start(); th.join()
