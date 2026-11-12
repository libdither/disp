"""One reducer, several copy disciplines, so their costs can be compared on the same programs.

The reducer is ../net.py generalised in three ways: agents may have any arity (closure agents carry their
captured values), an agent kind may have more than one principal port (the sharer `sh` of design SHARE
has two), and active pairs are pairs of ports, not of agents.

Designs (see the docstring of each class):
  Base     ../net.py exactly: copies walk through inert code agent by agent
  Cancel   Base + an eraser that reaches a copier's auxiliary port turns the copier into a wire
  Ref      closure conversion: every λ is a closure agent naming a closed definition in a static table
  Share    the same closures, but the closed code lives in the net once, shared through `sh` agents
           with two principal ports (no table)
Ref and Share also come with the Cancel rule (RefCancel, ShareCancel).
"""
import sys, os, itertools, random
from collections import Counter, deque
sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), '..'))
import net as basenet
from net import view_term

ARITY = dict(basenet.ARITY)
ARITY.update({'sh': 2, 'dupE': 2})
COPIERS = ('sig', 'dup', 'dupE')


class Stuck(Exception):
    pass


class Net:
    def __init__(self, design, seed=None):
        self.D = design
        self.kind, self.ar, self.tag, self.mate = {}, {}, {}, {}
        self.ids = itertools.count()
        self.queue = deque()
        self.rng = random.Random(seed) if seed is not None else None
        self.stats = Counter()
        self.op, self.ops = {}, itertools.count()
        self.stuck = []
        self.copied = set()        # agents made by a copy rule and not yet touched otherwise
        self.alloc = 0             # agents made by rules (placeholders excluded)

    def new(self, kind, arity=None, tag=None):
        a = next(self.ids)
        self.kind[a] = kind
        self.ar[a] = ARITY[kind] if arity is None else arity
        if tag is not None: self.tag[a] = tag
        if kind != 'ph': self.alloc += 1
        return a

    def principal(self, p):
        a, i = p
        k = self.kind[a]
        if k in ('root', 'ph'): return False
        return i == 0 or (k == 'sh' and i == 1)

    def active(self, p, q):
        if self.principal(p) and self.principal(q): return True
        if self.D.cancel:
            for x, y in ((p, q), (q, p)):
                if self.kind[x[0]] == 'era' and x[1] == 0 and self.kind[y[0]] in self.D.cancel and y[1] in (1, 2):
                    return True
        return False

    def link(self, p, q):
        self.mate[p] = q
        self.mate[q] = p
        if self.active(p, q):
            self.queue.append((p, q))

    def remove(self, a):
        for i in range(self.ar[a] + 1):
            self.mate.pop((a, i), None)
        del self.kind[a], self.ar[a]
        self.tag.pop(a, None)
        self.op.pop(a, None)
        self.copied.discard(a)

    def rewrite(self, p, q, build):
        """Replace the agents of the active pair p, q. build(slot) wires the replacement; slot(agent, i) is where
        that agent's port i was connected (every port but the two active ones)."""
        a, b = p[0], q[0]
        assert a != b
        ports = [(x, i) for x in (a, b) for i in range(self.ar[x] + 1) if (x, i) != p and (x, i) != q]
        ph = {pt: self.new('ph') for pt in ports}
        for pt in ports:
            e = self.mate[pt]
            if e in ph:
                if pt < e: self.link((ph[pt], 0), (ph[e], 0))
            elif e == p or e == q:
                raise AssertionError('an auxiliary port wired to the active pair')
            else:
                self.link((ph[pt], 0), e)
        self.remove(a)
        self.remove(b)
        build(lambda x, i: (ph[(x, i)], 1))
        for h in ph.values():
            x, y = self.mate.get((h, 0)), self.mate.get((h, 1))
            if x is None or y is None:
                raise AssertionError(f'placeholder left unconnected: {x} {y}')
            if x != (h, 1):
                self.link(x, y)
            self.remove(h)

    def fire(self, p, q):
        for x, y in ((p, q), (q, p)):
            got = self.D.lookup(self, x, y)
            if got is not None:
                label, f = got
                self.stats[label] += 1
                self.stats['interactions'] += 1
                f(self, x, y)
                return
        self.stuck.append((self.kind[p[0]], self.kind[q[0]]))
        raise Stuck(f'no rule for {self.kind[p[0]]}.{p[1]} ⋈ {self.kind[q[0]]}.{q[1]}')

    def run(self, limit=200000):
        n = 0
        while self.queue:
            if self.rng is not None and len(self.queue) > 1:
                self.queue.rotate(-self.rng.randrange(len(self.queue)))
            p, q = self.queue.popleft()
            if p[0] not in self.kind or q[0] not in self.kind or self.mate.get(p) != q:
                continue
            n += 1
            if n > limit:
                return 'limit'
            try:
                self.fire(p, q)
            except Stuck:
                continue
        return 'stuck' if self.stuck else 'done'

    def load(self, t):
        root = self.new('root')
        th = self.new('T')
        self.link((th, 0), self.D.emit(self, t, {}))
        self.link((th, 1), (root, 0))
        return root


# ---------------------------------------------------------------- rules shared by every design
def copy_rule(n, p, q):
    """a copier meets x: two copies of x; each auxiliary port of x gets a copy in flight"""
    c, x = p[0], q[0]
    k, ar, tag = n.kind[x], n.ar[x], n.tag.get(x)
    op = n.op.get(c) if n.kind[c] == 'dup' else next(n.ops)
    def build(slot):
        x1, x2 = n.new(k, ar, tag), n.new(k, ar, tag)
        n.copied.update((x1, x2))
        if k == 'dup': n.op[x1] = n.op[x2] = n.op[x]
        n.link((x1, 0), slot(c, 1))
        n.link((x2, 0), slot(c, 2))
        for i in range(1, ar + 1):
            d = n.new('dup')
            n.op[d] = op
            n.link((d, 0), slot(x, i))
            n.link((d, 1), (x1, i))
            n.link((d, 2), (x2, i))
    n.rewrite(p, q, build)

def erase_rule(n, p, q):
    e, x = p[0], q[0]
    if x in n.copied: n.stats['erased a fresh copy'] += 1
    ports = [i for i in range(n.ar[x] + 1) if i != q[1]]
    n.rewrite(p, q, lambda slot: [n.link((n.new('era'), 0), slot(x, i)) for i in ports])

def cancel_rule(n, p, q):
    """an eraser on auxiliary port i of a copier: nothing goes that way, so the copier is a wire"""
    c, i = q
    n.rewrite(p, q, lambda slot: n.link(slot(c, 0), slot(c, 3 - i)))

def annihilate(n, p, q):
    d1, d2 = p[0], q[0]
    if n.op.get(d1) != n.op.get(d2):
        n.stats['annihilated across two copies'] += 1
    n.rewrite(p, q, lambda slot: (n.link(slot(d1, 1), slot(d2, 1)), n.link(slot(d1, 2), slot(d2, 2))))

def thaw_app(n, p, q):
    t, ap = p[0], q[0]
    def build(slot):
        o, tf, ta = n.new('O'), n.new('T'), n.new('T')
        n.link((tf, 0), slot(ap, 1)); n.link((tf, 1), (o, 0))
        n.link((ta, 0), slot(ap, 2)); n.link((ta, 1), (o, 1))
        n.link((o, 2), slot(t, 1))
    n.rewrite(p, q, build)

def thaw_var(n, p, q):
    t, v = p[0], q[0]
    n.rewrite(p, q, lambda slot: n.link(slot(t, 1), slot(v, 1)))

def thaw_value(n, p, q):
    """T meets a value: the value itself (rebuilt, as in ../net.py)"""
    t, v = p[0], q[0]
    k, ar, tag = n.kind[v], n.ar[v], n.tag.get(v)
    def build(slot):
        w = n.new(k, ar, tag)
        n.link((w, 0), slot(t, 1))
        for i in range(1, ar + 1): n.link((w, i), slot(v, i))
    n.rewrite(p, q, build)

def reflect_start(n, p, q):
    o, rho = p[0], q[0]
    def build(slot):
        r = n.new('R')
        n.link((r, 0), slot(o, 1)); n.link((r, 1), slot(o, 2))
    n.rewrite(p, q, build)

def reflect_with(view, parts):
    def rule(n, p, q):
        r, v = p[0], q[0]
        info = (n.ar[v], n.tag.get(v))
        def build(slot):
            ports = parts(n, v, slot, info)
            t = view_term(view)
            if ports: t = basenet.subst_parts(t, ports)
            n.link(n.D.emit(n, t, {}), slot(r, 1))
        n.rewrite(p, q, build)
    return rule

def function_as(kind):
    """the function goes back to the reader as an agent of this kind, on the same ports"""
    def parts(n, v, slot, info):
        f = n.new(kind, *info)
        for i in range(1, info[0] + 1): n.link((f, i), slot(v, i))
        return {'F': (f, 0)}
    return parts

READ_PARTS = {
    'app': reflect_with('app', lambda n, v, slot, info: {'f': slot(v, 1), 'a': slot(v, 2)}),
    'var': reflect_with('var', lambda n, v, slot, info: {'w': slot(v, 1)}),
    'rho': reflect_with('prim', lambda n, v, slot, info: {}),
}


def bind(n, port, occ):
    """wire a binder port to its occurrences: an eraser for none, a chain of sharers for several"""
    if not occ:
        n.link(port, (n.new('era'), 0))
        return
    occ = list(occ)
    while len(occ) > 1:
        s = n.new('sig')
        n.link(port, (s, 0))
        n.link((s, 1), occ.pop(0))
        port = (s, 2)
    n.link(port, occ[0])


def resolve(n, port, names, rb):
    """what an occurrence port leads to: a bound name, or a splice of a value"""
    src = n.mate[port]
    while True:
        k = n.kind[src[0]]
        if k == 'sig' and src[1] in (1, 2): src = n.mate[(src[0], 0)]
        elif k == 'sh' and src[1] in (0, 1): src = n.mate[(src[0], 2)]
        else: break
    if src in names: return names[src]
    return ('~', rb(n, src, names))


class Design:
    cancel = ()          # the copier kinds an eraser on an auxiliary port turns into a wire
    name = '?'
    def __init__(self):
        self.R = {('O', 'rho'): reflect_start, ('T', 'app'): thaw_app, ('T', 'var'): thaw_var, ('T', 'rho'): thaw_value,
                  ('R', 'app'): READ_PARTS['app'], ('R', 'var'): READ_PARTS['var'], ('R', 'rho'): READ_PARTS['rho'],
                  ('dup', 'dup'): annihilate}

    def lookup(self, n, p, q):
        (x, i), (y, j) = p, q
        kx, ky = n.kind[x], n.kind[y]
        if n.principal(p) and n.principal(q):
            f = self.R.get((kx, ky))
            if f is not None: return self.label(kx, ky, f), f
            if kx in COPIERS and ky not in ('dup', 'era'): return f'copy {ky} by {kx}', copy_rule
            if kx == 'era': return f'erase {ky}', erase_rule
            return None
        if self.cancel and kx == 'era' and i == 0 and ky in self.cancel and j in (1, 2):
            return f'cancel {ky}', cancel_rule
        return None

    def label(self, kx, ky, f):
        return f'{kx}⋈{ky}'

    def emit_common(self, n, t, env, lam):
        if t == 'ρ':
            return (n.new('rho'), 0)
        if isinstance(t, tuple) and len(t) == 2 and t[0] == '~':
            v = n.new('var')
            n.link((v, 1), t[1])
            return (v, 0)
        if isinstance(t, tuple) and len(t) == 3:
            return lam(t)
        if isinstance(t, tuple):
            a = n.new('app')
            n.link((a, 1), self.emit(n, t[0], env))
            n.link((a, 2), self.emit(n, t[1], env))
            return (a, 0)
        if t not in env: raise NameError(f'nothing binds {t}')
        v = n.new('var')
        env[t].append((v, 1))
        return (v, 0)

    def rb_common(self, n, port, names):
        a, i = port
        if i != 0: raise ValueError(f'not a value: port {port} of {n.kind.get(a)}')
        k = n.kind[a]
        if k == 'app': return (self.readback(n, n.mate[(a, 1)], names), self.readback(n, n.mate[(a, 2)], names))
        if k == 'rho': return 'ρ'
        if k == 'var': return resolve(n, (a, 1), names, self.readback)
        return None


# ---------------------------------------------------------------- the designs
class Base(Design):
    """../net.py: lam and Lam carry their body inline; copying and erasing walk it agent by agent"""
    name = 'base'
    def __init__(self):
        super().__init__()
        def run(n, p, q):
            o, l = p[0], q[0]
            def build(slot):
                t = n.new('T')
                n.link(slot(o, 1), slot(l, 1)); n.link((t, 0), slot(l, 2)); n.link((t, 1), slot(o, 2))
            n.rewrite(p, q, build)
        def opn(n, p, q):
            o, l = p[0], q[0]
            n.rewrite(p, q, lambda slot: (n.link(slot(o, 1), slot(l, 1)), n.link(slot(o, 2), slot(l, 2))))
        self.R.update({('O', 'lam'): run, ('O', 'Lam'): opn, ('T', 'lam'): thaw_value, ('T', 'Lam'): thaw_value,
                       ('R', 'lam'): reflect_with('fun', function_as('Lam')),
                       ('R', 'Lam'): reflect_with('opened', function_as('lam'))})

    def emit(self, n, t, env):
        def lam(t):
            binder, x, body = t
            l = n.new('Lam' if binder == 'Λ' else 'lam')
            occ = []
            n.link((l, 2), self.emit(n, body, {**env, x: occ}))
            bind(n, (l, 1), occ)
            return (l, 0)
        return self.emit_common(n, t, env, lam)

    def readback(self, n, port, names=None):
        names = {} if names is None else names
        a = port[0]
        if n.kind[a] in ('lam', 'Lam'):
            x = f'x{len(names)}'
            names[(a, 1)] = x
            return ('λ' if n.kind[a] == 'lam' else 'Λ', x, self.readback(n, n.mate[(a, 2)], names))
        r = self.rb_common(n, port, names)
        if r is None: raise ValueError(f'not finished: {n.kind[a]} at the result')
        return r


class Cancel(Base):
    """Base, and an eraser that meets a copier's auxiliary port turns the copier into a wire (the copier
    then has three ports an eraser can meet: a multi-port agent, see the report)"""
    name = 'cancel'
    cancel = ('sig',)


class CancelDup(Base):
    """UNSOUND, kept to show it: the cancel rule on dup as well. A copy of a function that is applied and
    whose result is then erased cancels the body's copier while the binder's copier waits for its other
    half forever (see the report)"""
    name = 'cancel-dup'
    cancel = ('sig', 'dup')


# ---- closure conversion: a λ in code is an agent naming a closed definition, with one port per free variable
class Defs:
    """The static table: each λ or Λ of the program (and of the five views) with its free variables made
    explicit. A definition is (binder, x, body, names): body is closed over x and names."""
    def __init__(self):
        self.table, self.key, self.memo = [], {}, {}

    def intern(self, t):
        """(k, fvs) for the λ-term t: fvs are its free variables in order, each a name of the enclosing code
        or ('~', port) for a splice"""
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
    """a definition's body as a term, its free names replaced by env (names or splices)"""
    if body == 'ρ': return body
    if isinstance(body, str): return env[body]
    if len(body) == 3:
        x = fresh()
        return (body[0], x, rb_term(body[2], {**env, body[1]: x}, fresh))
    return (rb_term(body[0], env, fresh), rb_term(body[1], env, fresh))


class Ref(Design):
    """Reference agents. clo_k(p; e1..en) is the function of definition k with its n captured values,
    cloL_k the same function opened. Copy and erase: one rewrite plus whatever the captured values cost.
      O ⋈ clo_k    expand: definition k's body, its binder on the argument and its free names on e1..en,
                   under a thaw, to the result (the body's own λs come out as closure agents again)
      O ⋈ cloL_k   expand, the body as code to the result
      R ⋈ clo_k    view 'fun', the function handed back as cloL_k on the same e1..en (nothing expanded)
      R ⋈ cloL_k   view 'opened', handed back as clo_k
      T ⋈ clo | cloL   the value itself"""
    name = 'ref'
    def __init__(self):
        super().__init__()
        self.defs = Defs()
        self.R.update({('O', 'clo'): self.expand(True), ('O', 'cloL'): self.expand(False),
                       ('T', 'clo'): thaw_value, ('T', 'cloL'): thaw_value,
                       ('R', 'clo'): reflect_with('fun', function_as('cloL')),
                       ('R', 'cloL'): reflect_with('opened', function_as('clo'))})

    def label(self, kx, ky, f):
        return 'expand' if kx == 'O' and ky in ('clo', 'cloL') else f'{kx}⋈{ky}'

    def expand(self, run):
        def rule(n, p, q):
            o, c = p[0], q[0]
            binder, x, body, names = self.defs.table[n.tag[c]]
            def build(slot):
                before = n.alloc
                env = {x: [], **{nm: [] for nm in names}}
                top = self.emit(n, body, env)
                if run:
                    t = n.new('T'); n.link((t, 0), top); n.link((t, 1), slot(o, 2))
                else:
                    n.link(top, slot(o, 2))
                bind(n, slot(o, 1), env[x])
                for i, nm in enumerate(names): bind(n, slot(c, i + 1), env[nm])
                n.stats['expanded agents'] += n.alloc - before
            n.rewrite(p, q, build)
        return rule

    def emit(self, n, t, env):
        def lam(t):
            k, fvs = self.defs.intern(t)
            c = n.new('cloL' if t[0] == 'Λ' else 'clo', len(fvs), k)
            for i, fv in enumerate(fvs):
                if isinstance(fv, tuple): n.link((c, i + 1), fv[1])
                else: env[fv].append((c, i + 1))
            return (c, 0)
        return self.emit_common(n, t, env, lam)

    def readback(self, n, port, names=None):
        names = {} if names is None else names
        a = port[0]
        if n.kind[a] in ('clo', 'cloL'):
            _, x, body, nms = self.defs.table[n.tag[a]]
            env = {nm: resolve(n, (a, i + 1), names, self.readback) for i, nm in enumerate(nms)}
            fresh = lambda: f'y{next(n.ids)}'
            return rb_term(('Λ' if n.kind[a] == 'cloL' else 'λ', x, body), env, fresh)
        r = self.rb_common(n, port, names)
        if r is None: raise ValueError(f'not finished: {n.kind[a]} at the result')
        return r


class RefCancel(Ref):
    name = 'ref+cancel'
    cancel = ('sig',)


class RefRun(Ref):
    """Ref, but O ⋈ clo builds the body already running: an application is an O, a variable is a wire, a λ a
    closure agent. No thaw is left to do. O ⋈ cloL (opening) still builds code, which reading needs."""
    name = 'ref-run'
    def expand(self, run):
        if not run: return super().expand(run)
        def rule(n, p, q):
            o, c = p[0], q[0]
            binder, x, body, names = self.defs.table[n.tag[c]]
            def build(slot):
                before = n.alloc
                env = {x: [], **{nm: [] for nm in names}}
                ph = n.new('ph')                       # where the body's value goes
                self.emit_running(n, body, env, (ph, 0))
                bind(n, slot(o, 1), env[x])
                for i, nm in enumerate(names): bind(n, slot(c, i + 1), env[nm])
                n.link((ph, 1), slot(o, 2))
                y, z = n.mate[(ph, 0)], n.mate[(ph, 1)]
                n.remove(ph); n.link(y, z)
                n.stats['expanded agents'] += n.alloc - before
            n.rewrite(p, q, build)
        return rule

    def emit_running(self, n, t, env, dest):
        if t == 'ρ':
            n.link(dest, (n.new('rho'), 0))
        elif isinstance(t, str):
            env[t].append(dest)
        elif len(t) == 3:
            n.link(dest, self.emit(n, t, env))
        else:
            o = n.new('O')
            n.link(dest, (o, 2))
            self.emit_running(n, t[0], env, (o, 0))
            self.emit_running(n, t[1], env, (o, 1))


class Share(Ref):
    """The closures of Ref with the table moved into the net. A λ of the program is a closure
    cl(p; c, e1..en) and its closed code cc(p; x, b, f1..fn): x and f1..fn hand the argument and the
    captured values to the body's occurrences, as a binder does. Shared code hangs below a tree of
    sh(p1, p2; s) agents, which have TWO principal ports. Rules (k, l are the two principal ports of sh):
      O ⋈ cl | clL     get(p→c; a, r, e1..en): ask for a private copy of the code, to run or to open
      get ⋈ cc         instantiate: x–a, fi–ei, body under a thaw (or as code) to r
      get ⋈ sh@k       dupE(p→s; a→ the get again, b→ what was at l): a private copy for this side only
      dupE ⋈ cc        copy cc one level: both copies get plain dup on x, b, f1..fn (body copied down to
                       its own closures, whose code is then shared)
      dupE ⋈ sh@k      dupE(p→s; a→ dupE's a, b→ sh'.s), sh'(dupE's b, what was at l; s)
      dup ⋈ cc         share: sh(dup's a, dup's b; s→cc)                     (no code is copied)
      dup ⋈ sh@k       sh'(dup's a, dup's b; s→ sh''.p1), sh''(p1, what was at l; s)
      era ⋈ sh@k       what was at l – s                                       (O(1))
      R ⋈ cl | clL     the view, handed back as clL | cl on the same c, e1..en
      sig | dup ⋈ cl   ordinary copy of the closure agent: dup on c reaches cc or sh and shares
      era ⋈ cl, era ⋈ cc   ordinary erase; erasing cc happens only when the last closure on it goes"""
    name = 'share'
    def __init__(self):
        Design.__init__(self)
        self.defs = Defs()
        self.R.update({('O', 'cl'): self.ask(True), ('O', 'clL'): self.ask(False),
                       ('get', 'cc'): self.instantiate, ('get', 'sh'): self.get_sh,
                       ('dupE', 'cc'): self.copy_cc, ('dupE', 'sh'): self.dupE_sh,
                       ('dup', 'cc'): self.share_cc, ('dup', 'sh'): self.dup_sh, ('era', 'sh'): self.era_sh,
                       ('T', 'cl'): thaw_value, ('T', 'clL'): thaw_value,
                       ('R', 'cl'): reflect_with('fun', function_as('clL')),
                       ('R', 'clL'): reflect_with('opened', function_as('cl'))})

    def label(self, kx, ky, f):
        if (kx, ky) == ('get', 'cc'): return 'expand'
        if (kx, ky) == ('dupE', 'cc'): return 'copy cc by dupE'
        if 'sh' in (kx, ky) or (kx, ky) == ('dup', 'cc'): return f'share {kx}⋈{ky}'
        return f'{kx}⋈{ky}'

    def ask(self, run):
        def rule(n, p, q):
            o, c = p[0], q[0]
            m = n.ar[c] - 1
            def build(slot):
                g = n.new('get', 2 + m, 'run' if run else 'open')
                n.link((g, 0), slot(c, 1)); n.link((g, 1), slot(o, 1)); n.link((g, 2), slot(o, 2))
                for i in range(m): n.link((g, 3 + i), slot(c, 2 + i))
            n.rewrite(p, q, build)
        return rule

    def instantiate(self, n, p, q):
        g, c = p[0], q[0]
        m, mode = n.ar[c] - 2, n.tag[g]
        def build(slot):
            n.link(slot(g, 1), slot(c, 1))
            for i in range(m): n.link(slot(g, 3 + i), slot(c, 3 + i))
            if mode == 'run':
                t = n.new('T'); n.link((t, 0), slot(c, 2)); n.link((t, 1), slot(g, 2))
            else:
                n.link(slot(c, 2), slot(g, 2))
        n.rewrite(p, q, build)

    def contested(self, n, s, k):
        """count the times a sharer fires on one principal port while its other one is in an active pair too"""
        m = n.mate.get((s, 1 - k))
        if m is not None and n.principal(m): n.stats['sh contested'] += 1

    def get_sh(self, n, p, q):
        g, (s, k) = p[0], q
        self.contested(n, s, k)
        ar, tag = n.ar[g], n.tag[g]
        def build(slot):
            d = n.new('dupE')
            g2 = n.new('get', ar, tag)
            for i in range(1, ar + 1): n.link((g2, i), slot(g, i))
            n.link((d, 0), slot(s, 2)); n.link((d, 1), (g2, 0)); n.link((d, 2), slot(s, 1 - k))
        n.rewrite(p, q, build)

    def copy_cc(self, n, p, q):
        copy_rule(n, p, q)

    def dupE_sh(self, n, p, q):
        e, (s, k) = p[0], q
        self.contested(n, s, k)
        def build(slot):
            d = n.new('dupE'); h = n.new('sh')
            n.link((d, 0), slot(s, 2)); n.link((d, 1), slot(e, 1)); n.link((d, 2), (h, 2))
            n.link((h, 0), slot(e, 2)); n.link((h, 1), slot(s, 1 - k))
        n.rewrite(p, q, build)

    def share_cc(self, n, p, q):
        d, c = p[0], q[0]
        ar, tag = n.ar[c], n.tag[c]
        def build(slot):
            c2 = n.new('cc', ar, tag)
            for i in range(1, ar + 1): n.link((c2, i), slot(c, i))
            h = n.new('sh')
            n.link((h, 0), slot(d, 1)); n.link((h, 1), slot(d, 2)); n.link((h, 2), (c2, 0))
        n.rewrite(p, q, build)

    def dup_sh(self, n, p, q):
        d, (s, k) = p[0], q
        self.contested(n, s, k)
        def build(slot):
            h1, h2 = n.new('sh'), n.new('sh')
            n.link((h1, 0), slot(d, 1)); n.link((h1, 1), slot(d, 2)); n.link((h1, 2), (h2, 0))
            n.link((h2, 1), slot(s, 1 - k)); n.link((h2, 2), slot(s, 2))
        n.rewrite(p, q, build)

    def era_sh(self, n, p, q):
        s, k = q
        self.contested(n, s, k)
        n.rewrite(p, q, lambda slot: n.link(slot(s, 1 - k), slot(s, 2)))

    def emit(self, n, t, env):
        def lam(t):
            k, fvs = self.defs.intern(t)
            binder, x, body, names = self.defs.table[k]
            m = len(names)
            cc = n.new('cc', 2 + m, k)
            inner = {x: [], **{nm: [] for nm in names}}
            n.link((cc, 2), self.emit(n, body, inner))
            bind(n, (cc, 1), inner[x])
            for i, nm in enumerate(names): bind(n, (cc, 3 + i), inner[nm])
            c = n.new('clL' if binder == 'Λ' else 'cl', 1 + m)
            n.link((c, 1), (cc, 0))
            for i, fv in enumerate(fvs):
                if isinstance(fv, tuple): n.link((c, 2 + i), fv[1])
                else: env[fv].append((c, 2 + i))
            return (c, 0)
        return self.emit_common(n, t, env, lam)

    def readback(self, n, port, names=None):
        names = {} if names is None else names
        a = port[0]
        if n.kind[a] == 'sh':
            return self.readback(n, n.mate[(a, 2)], names)
        if n.kind[a] in ('cl', 'clL', 'clv', 'clLv'):
            src = n.mate[(a, 1)]
            while n.kind[src[0]] == 'sh':
                src = n.mate[(src[0], 2)]
            cc = src[0]
            assert n.kind[cc] == 'cc' and src[1] == 0, n.kind[cc]
            x = f'y{next(n.ids)}'
            inner = {**names, (cc, 1): x}
            for i in range(n.ar[cc] - 2):
                inner[(cc, 3 + i)] = resolve(n, (a, 2 + i), names, self.readback)
            return ('λ' if n.kind[a] in ('cl', 'clv') else 'Λ', x, self.readback(n, n.mate[(cc, 2)], inner))
        r = self.rb_common(n, port, names)
        if r is None: raise ValueError(f'not finished: {n.kind[a]} at the result')
        return r


class ShareCancel(Share):
    name = 'share+cancel'
    cancel = ('sig', 'dupE')


class Lazy(Share):
    """Share, but a function stays inline until it is first copied. An inline function is
    lc(p; x, b, f1..fn, e1..en): x and f1..fn hand the argument and the captured values to the body, e1..en
    are the captured values from outside (fi is wired to ei when the function runs). New rules:
      O ⋈ lc         run:  x–a, fi–ei, b → T → r        (one rewrite, as O ⋈ lam)
      O ⋈ lcL        open: x–a, fi–ei, b → r
      R ⋈ lc | lcL   the view, handed back as lcL | lc on the same ports
      sig | dup ⋈ lc   close and share: cc(x, b, f1..fn) under sh, two closures cl(c; e1..en) on its two
                     sides, dup on each ei                (one rewrite, the body is not touched)
      era ⋈ lc       ordinary erase (it is the only copy of its code)
    and every rule of Share for the closed form."""
    name = 'lazy'
    def __init__(self):
        super().__init__()
        self.run_lc = run = self._run_lc
        self.R.update({('O', 'lc'): run(False), ('O', 'lcL'): run(True),
                       ('T', 'lc'): thaw_value, ('T', 'lcL'): thaw_value,
                       ('R', 'lc'): reflect_with('fun', function_as('lcL')),
                       ('R', 'lcL'): reflect_with('opened', function_as('lc'))})
        for c in ('sig', 'dup'):
            self.R[(c, 'lc')] = self.close
            self.R[(c, 'lcL')] = self.close

    @staticmethod
    def _run_lc(open_):
        def rule(n, p, q):
            o, l = p[0], q[0]
            m = (n.ar[l] - 2) // 2
            def build(slot):
                n.link(slot(o, 1), slot(l, 1))
                for i in range(m): n.link(slot(l, 3 + i), slot(l, 3 + m + i))
                if open_: n.link(slot(l, 2), slot(o, 2))
                else:
                    t = n.new('T'); n.link((t, 0), slot(l, 2)); n.link((t, 1), slot(o, 2))
            n.rewrite(p, q, build)
        return rule

    def label(self, kx, ky, f):
        if ky in ('lc', 'lcL') and kx in ('sig', 'dup'): return f'share close {ky} by {kx}'
        if kx == 'O' and ky in ('lc', 'lcL'): return f'{kx}⋈{ky}'
        return super().label(kx, ky, f)

    def close(self, n, p, q, kinds=('cl', 'clL')):
        c, l = p[0], q[0]
        m, tag, opened = (n.ar[l] - 2) // 2, n.tag[l], n.kind[l] in ('lcL', 'lcLv')
        op = n.op.get(c) if n.kind[c] == 'dup' else next(n.ops)
        def build(slot):
            cc = n.new('cc', 2 + m, tag)
            for i in range(1, 3 + m): n.link((cc, i), slot(l, i))
            h = n.new('sh')
            n.link((h, 2), (cc, 0))
            cls = []
            for j in (1, 2):
                k = n.new(kinds[1] if opened else kinds[0], 1 + m, tag)
                n.link((k, 0), slot(c, j)); n.link((k, 1), (h, j - 1))
                cls.append(k)
            for i in range(m):
                d = n.new('dup'); n.op[d] = op
                n.link((d, 0), slot(l, 3 + m + i)); n.link((d, 1), (cls[0], 2 + i)); n.link((d, 2), (cls[1], 2 + i))
        n.rewrite(p, q, build)

    def emit(self, n, t, env):
        def lam(t):
            k, fvs = self.defs.intern(t)
            binder, x, body, names = self.defs.table[k]
            m = len(names)
            l = n.new('lcL' if binder == 'Λ' else 'lc', 2 + 2 * m, k)
            inner = {x: [], **{nm: [] for nm in names}}
            n.link((l, 2), self.emit(n, body, inner))
            bind(n, (l, 1), inner[x])
            for i, nm in enumerate(names): bind(n, (l, 3 + i), inner[nm])
            for i, fv in enumerate(fvs):
                if isinstance(fv, tuple): n.link((l, 3 + m + i), fv[1])
                else: env[fv].append((l, 3 + m + i))
            return (l, 0)
        return self.emit_common(n, t, env, lam)

    def readback(self, n, port, names=None):
        names = {} if names is None else names
        a = port[0]
        if n.kind[a] in ('lc', 'lcL', 'lcv', 'lcLv'):
            m = (n.ar[a] - 2) // 2
            x = f'y{next(n.ids)}'
            inner = {**names, (a, 1): x}
            for i in range(m): inner[(a, 3 + i)] = resolve(n, (a, 3 + m + i), names, self.readback)
            return ('λ' if n.kind[a] in ('lc', 'lcv') else 'Λ', x, self.readback(n, n.mate[(a, 2)], inner))
        return super().readback(n, port, names)


class LazyCancel(Lazy):
    name = 'lazy+cancel'
    cancel = ('sig', 'dupE')


def thaw_to(kind):
    """T meets a function in code: the same function as a value, of this kind"""
    def rule(n, p, q):
        t, v = p[0], q[0]
        ar, tag = n.ar[v], n.tag.get(v)
        def build(slot):
            w = n.new(kind, ar, tag)
            n.link((w, 0), slot(t, 1))
            for i in range(1, ar + 1): n.link((w, i), slot(v, i))
        n.rewrite(p, q, build)
    return rule


class LazyV(Lazy):
    """Lazy, and function VALUES are shared too, not just code. A thaw turns a function in code into a value
    agent (lc→lcv, lcL→lcLv, cl→clv, clL→clLv; same ports, same rules as the code kind otherwise). Rules added:
      sig | dup ⋈ lcv | lcLv | clv | clLv   share: sh(copier's a, copier's b; s→ the value)   (O(1), whatever
                                            the value captured)
      O ⋈ sh@k, R ⋈ sh@k    a demand: dupE(p→s; a→ the O or R again, b→ what was at l)
      dupE ⋈ lcv | lcLv     close it (as sig ⋈ lc), the two closures are values
      dupE ⋈ clv | clLv     copy the closure agent one level; its captured values are met by dup, so shared
      sig ⋈ sh@k            split the share three ways (as dup ⋈ sh)
    A copy is opened one level per demand; erasing a copy that was never demanded is two rewrites."""
    name = 'lazy-v'
    VAL = {'lc': 'lcv', 'lcL': 'lcLv', 'cl': 'clv', 'clL': 'clLv'}
    def __init__(self):
        super().__init__()
        V = self.VAL
        for code, val in V.items():
            self.R[('T', code)] = thaw_to(val)
        self.R.update({('O', 'lcv'): self._run_lc(False), ('O', 'lcLv'): self._run_lc(True),
                       ('O', 'clv'): self.ask(True), ('O', 'clLv'): self.ask(False),
                       ('R', 'lc'): reflect_with('fun', function_as('lcLv')), ('R', 'lcL'): reflect_with('opened', function_as('lcv')),
                       ('R', 'cl'): reflect_with('fun', function_as('clLv')), ('R', 'clL'): reflect_with('opened', function_as('clv')),
                       ('R', 'lcv'): reflect_with('fun', function_as('lcLv')), ('R', 'lcLv'): reflect_with('opened', function_as('lcv')),
                       ('R', 'clv'): reflect_with('fun', function_as('clLv')), ('R', 'clLv'): reflect_with('opened', function_as('clv')),
                       ('O', 'sh'): self.demand, ('R', 'sh'): self.demand, ('sig', 'sh'): self.dup_sh,
                       ('dupE', 'lcv'): lambda n, p, q: self.close(n, p, q, ('clv', 'clLv')),
                       ('dupE', 'lcLv'): lambda n, p, q: self.close(n, p, q, ('clv', 'clLv'))})
        for c in ('sig', 'dup'):
            for val in V.values(): self.R[(c, val)] = self.share_value

    def label(self, kx, ky, f):
        if kx in ('sig', 'dup') and ky in self.VAL.values(): return f'share value {ky} by {kx}'
        if kx in ('O', 'R') and ky == 'sh': return f'share demand {kx}⋈sh'
        if kx == 'dupE' and ky in ('lcv', 'lcLv'): return f'share close {ky} by dupE'
        if kx == 'dupE' and ky in ('clv', 'clLv'): return f'copy {ky} by dupE'
        if kx == 'O' and ky in ('lcv', 'lcLv'): return f'{kx}⋈{ky}'
        if kx == 'O' and ky in ('clv', 'clLv'): return f'{kx}⋈{ky}'
        return super().label(kx, ky, f)

    def share_value(self, n, p, q):
        d, v = p[0], q[0]
        k, ar, tag = n.kind[v], n.ar[v], n.tag.get(v)
        def build(slot):
            w = n.new(k, ar, tag)
            for i in range(1, ar + 1): n.link((w, i), slot(v, i))
            h = n.new('sh')
            n.link((h, 0), slot(d, 1)); n.link((h, 1), slot(d, 2)); n.link((h, 2), (w, 0))
        n.rewrite(p, q, build)

    def demand(self, n, p, q):
        x, (s, k) = p[0], q
        self.contested(n, s, k)
        kx, ar = n.kind[x], n.ar[x]
        def build(slot):
            d = n.new('dupE')
            x2 = n.new(kx, ar)
            for i in range(1, ar + 1): n.link((x2, i), slot(x, i))
            n.link((d, 0), slot(s, 2)); n.link((d, 1), (x2, 0)); n.link((d, 2), slot(s, 1 - k))
        n.rewrite(p, q, build)


DESIGNS = {d.name: d for d in (Base, Cancel, CancelDup, Ref, RefCancel, RefRun, Share, ShareCancel, Lazy, LazyCancel, LazyV)}
