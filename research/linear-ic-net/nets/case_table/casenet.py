"""Case tables: the reflective net of ../net.py with reading done by a table agent instead of a λ-encoded view.

Every rule of ../net.py stays (so the old `ρ c k1 … k5` view still runs, as the baseline). Added:

  a table written in code, five slots of code (like a lam's body, the slots stay inert until taken):
    once[k1|…|k5]   once(p; k1..k5)   applied to a value: the slot for its kind runs on the parts, the other four are erased
    hand[k1|…|k5]   hand(p; k1..k5)   the slot runs on the other four slots, then the parts (the λ-view's protocol, as one agent)
    keep[k1|…|k5]   keep(p; k1..k5)   the slot is copied: one copy goes back, the other runs on the table (rebuilt) and the parts
    chain[k1|…|k5]  B1(p; k1, B2(k2, B3(k3, B4(k4, k5))))   the same as keep, as a chain of binary tests
    ref[k1|…|k5]    ref_n(p)          the same as keep, with closed slots held once outside the net: a nullary agent per table
  a table built at run time from values, so no new code former (reading code still sees the arms as lams):
    RHO k1 k2 k3 k4 k5   rhok, P1..P4 (partial tables), then a keep agent

Reading agents (principal port on the value being read):
    Conce / Chand / Ckeep (p; k1..k5, r)   Q1..Q4 (p; arm, rest, r, t, h)   Fw (p; X, r, t, h)   Cref_n (p; r)

Kinds read: fun (lam, part: it opened), opened (Lam, part: it closed), app (parts f, a), var (part: the spliced
value), prim (ρ, any table or partial table; part: the value itself, handed back unchanged).
"""
import sys, os, re, random
from collections import Counter
HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..'))
import net
from net import Net, ARITY, Stuck, r_thaw_value
import ref
from ref import FORMS, LISTS, Fuel

sys.setrecursionlimit(1000000)

POLICIES = ('once', 'hand', 'keep')
for _p in POLICIES:
    ARITY[_p] = 5
    ARITY['C' + _p] = 6
for _k in range(1, 5):
    ARITY[f'B{_k}'] = 2
    ARITY[f'Q{_k}'] = 5
    ARITY[f'P{_k}'] = _k
ARITY['Fw'] = 4
ARITY['rhok'] = 0
PRIMS = {'rho', 'once', 'hand', 'keep', 'B1', 'rhok', 'P1', 'P2', 'P3', 'P4'}
LIVE = {'O', 'T', 'R', 'root', 'ph', 'Conce', 'Chand', 'Ckeep', 'Q1', 'Q2', 'Q3', 'Q4', 'Fw'}

REFS = []          # the five slots of each ref table, as closed terms
REF_INDEX = {}

def is_ref(k): return k.startswith('ref') and k[3:].isdigit()
def is_cref(k): return k.startswith('Cref') and k[4:].isdigit()
def is_live(k): return k in LIVE or is_cref(k)

def kind_index(k):
    if k == 'lam': return 0
    if k == 'Lam': return 1
    if k == 'app': return 2
    if k == 'var': return 3
    if k in PRIMS or is_ref(k): return 4
    return None


# ---- pieces shared by the rules ----
def new_dup(n):
    d = n.new('dup')
    n.op[d] = next(n.ops)
    return d

def run_arm(n, code, args, out):
    """T on the arm, then the arm applied to args one by one, the result at out"""
    t = n.new('T')
    n.link((t, 0), code)
    cur = (t, 1)
    for a in args:
        o = n.new('O')
        n.link((o, 0), cur)
        n.link((o, 1), a)
        cur = (o, 2)
    n.link(cur, out)

def rebuild(n, k, x, slot):
    w = n.new(k)
    for j in range(1, ARITY[k] + 1):
        n.link((w, j), slot(x, j))
    return (w, 0)

def parts_of(n, k, x, slot):
    i = kind_index(k)
    if i in (0, 1):
        f = n.new('Lam' if i == 0 else 'lam')
        n.link((f, 1), slot(x, 1))
        n.link((f, 2), slot(x, 2))
        return [(f, 0)]
    if i == 2: return [slot(x, 1), slot(x, 2)]
    if i == 3: return [slot(x, 1)]
    return [rebuild(n, k, x, slot)]


# ---- a, b, d: one table agent ----
def r_start(n, o, tb):                      # O ⋈ once|hand|keep
    k = n.kind[tb]
    def build(slot):
        c = n.new('C' + k)
        n.link((c, 0), slot(o, 1))
        for j in range(5): n.link((c, j + 1), slot(tb, j + 1))
        n.link((c, 6), slot(o, 2))
    n.rewrite(o, tb, build)

def r_read(n, c, x):                        # Conce|Chand|Ckeep ⋈ value
    policy = n.kind[c][1:]
    kx = n.kind[x]
    i = kind_index(kx)
    n.stats['reads'] += 1
    n.stats[f'slot {i}'] += 1
    def build(slot):
        ps = parts_of(n, kx, x, slot)
        arms = [slot(c, j + 1) for j in range(5)]
        out = slot(c, 6)
        if policy == 'once':
            for j in range(5):
                if j != i: n.link((n.new('era'), 0), arms[j])
            run_arm(n, arms[i], ps, out)
        elif policy == 'hand':
            run_arm(n, arms[i], [arms[j] for j in range(5) if j != i] + ps, out)
        else:
            d = new_dup(n)
            n.link((d, 0), arms[i])
            t = n.new('keep')
            for j in range(5): n.link((t, j + 1), (d, 1) if j == i else arms[j])
            run_arm(n, (d, 2), [(t, 0)] + ps, out)
    n.rewrite(c, x, build)


# ---- c: a chain of binary tests, persistent ----
def r_chain_start(n, o, b):                 # O ⋈ B1
    n.stats['reads'] += 1
    def build(slot):
        q = n.new('Q1')
        n.link((q, 0), slot(o, 1))
        n.link((q, 1), slot(b, 1))
        n.link((q, 2), slot(b, 2))
        n.link((q, 3), slot(o, 2))
        n.link((q, 4), (q, 5))              # level 1: the node rebuilt here is the whole table
    n.rewrite(o, b, build)

def r_test(n, q, x):                        # Qk ⋈ value
    k = int(n.kind[q][1])
    kx = n.kind[x]
    i = kind_index(kx)
    def build(slot):
        if i == k - 1 or (k == 4 and i == 4):
            n.stats[f'slot {i}'] += 1
            take = 1 if i == k - 1 else 2
            d = new_dup(n)
            n.link((d, 0), slot(q, take))
            b = n.new(f'B{k}')
            n.link((b, 1), (d, 1) if take == 1 else slot(q, 1))
            n.link((b, 2), (d, 1) if take == 2 else slot(q, 2))
            n.link((b, 0), slot(q, 5))
            run_arm(n, (d, 2), [slot(q, 4)] + parts_of(n, kx, x, slot), slot(q, 3))
        else:
            b = n.new(f'B{k}')
            n.link((b, 1), slot(q, 1))
            n.link((b, 0), slot(q, 5))
            fw = n.new('Fw')
            n.link((fw, 0), slot(q, 2))
            n.link((fw, 1), rebuild(n, kx, x, slot))
            n.link((fw, 2), slot(q, 3))
            n.link((fw, 3), slot(q, 4))
            n.link((fw, 4), (b, 2))
    n.rewrite(q, x, build)

def r_forward(n, fw, b):                    # Fw ⋈ B2|B3|B4
    k = n.kind[b][1]
    def build(slot):
        q = n.new('Q' + k)
        n.link((q, 0), slot(fw, 1))
        n.link((q, 1), slot(b, 1))
        n.link((q, 2), slot(b, 2))
        n.link((q, 3), slot(fw, 2))
        n.link((q, 4), slot(fw, 3))
        n.link((q, 5), slot(fw, 4))
    n.rewrite(fw, b, build)


# ---- d: closed tables held outside the net (one nullary agent per table) ----
def r_ref_start(n, o, rf):                  # O ⋈ ref_n
    k = n.kind[rf]
    def build(slot):
        c = n.new('C' + k)
        n.link((c, 0), slot(o, 1))
        n.link((c, 1), slot(o, 2))
    n.rewrite(o, rf, build)

def r_ref_read(n, c, x):                    # Cref_n ⋈ value: the taken slot is built fresh from the table's code
    k = int(n.kind[c][4:])
    kx = n.kind[x]
    i = kind_index(kx)
    n.stats['reads'] += 1
    n.stats[f'slot {i}'] += 1
    def build(slot):
        ps = parts_of(n, kx, x, slot)
        before = len(n.kind)
        code = n.emit(REFS[k][i], {})
        n.stats['instantiated'] += len(n.kind) - before
        t = n.new(f'ref{k}')
        run_arm(n, code, [(t, 0)] + ps, slot(c, 1))
    n.rewrite(c, x, build)


# ---- a table built at run time: RHO k1 … k5 ----
def r_build(n, o, p):                       # O ⋈ rhok | P1..P4
    k = n.kind[p]
    have = 0 if k == 'rhok' else int(k[1])
    def build(slot):
        w = n.new('keep' if have == 4 else f'P{have + 1}')
        for j in range(have): n.link((w, j + 1), slot(p, j + 1))
        n.link((w, have + 1), slot(o, 1))
        n.link((w, 0), slot(o, 2))
    n.rewrite(o, p, build)


def case_rule(kx, ky):
    if kx == 'O':
        if ky in POLICIES: return r_start
        if ky == 'B1': return r_chain_start
        if is_ref(ky): return r_ref_start
        if ky in ('rhok', 'P1', 'P2', 'P3', 'P4'): return r_build
    if kx == 'T' and (ky in POLICIES or ky in ('B1', 'rhok') or is_ref(ky)): return r_thaw_value
    if kx in ('Conce', 'Chand', 'Ckeep') and kind_index(ky) is not None: return r_read
    if kx in ('Q1', 'Q2', 'Q3', 'Q4') and kind_index(ky) is not None: return r_test
    if kx == 'Fw' and ky in ('B2', 'B3', 'B4'): return r_forward
    if is_cref(kx) and kind_index(ky) is not None: return r_ref_read
    return None


def free_names(t, bound=()):
    if isinstance(t, str): return set() if t in bound or t in ('ρ', 'RHO') else {t}
    if t[0] == '~': return set()
    if is_tab(t): return set().union(*(free_names(a, bound) for a in t[2]))
    if len(t) == 3: return free_names(t[2], bound + (t[1],))
    return free_names(t[0], bound) | free_names(t[1], bound)

def is_tab(t):
    return isinstance(t, tuple) and len(t) == 3 and t[0] == 'tab'


class CNet(Net):
    def emit(self, t, env):
        if t == 'RHO':
            return (self.new('rhok'), 0)
        if is_tab(t):
            _, policy, arms = t
            if policy in POLICIES:
                a = self.new(policy)
                for j, arm in enumerate(arms): self.link((a, j + 1), self.emit(arm, env))
                return (a, 0)
            if policy == 'chain':
                ports = [self.emit(arm, env) for arm in arms]
                port = ports[4]
                for k in (4, 3, 2, 1):
                    b = self.new(f'B{k}')
                    self.link((b, 1), ports[k - 1])
                    self.link((b, 2), port)
                    port = (b, 0)
                return port
            if policy == 'ref':
                if free_names(t): raise NameError(f'a ref table must be closed: {free_names(t)}')
                key = repr(arms)
                if key not in REF_INDEX:
                    REF_INDEX[key] = len(REFS)
                    REFS.append(arms)
                    ARITY[f'ref{REF_INDEX[key]}'] = 0
                    ARITY[f'Cref{REF_INDEX[key]}'] = 1
                return (self.new(f'ref{REF_INDEX[key]}'), 0)
            raise ValueError(policy)
        return super().emit(t, env)

    def fire(self, a, b):
        for x, y in ((a, b), (b, a)):
            kx, ky = self.kind[x], self.kind[y]
            f = case_rule(kx, ky)
            if f is not None:
                self.stats[f'{"Cref" if is_cref(kx) else kx}⋈{"ref" if is_ref(ky) else ky}'] += 1
                self.stats['interactions'] += 1
                f(self, x, y)
                return
        return super().fire(a, b)


# ---- reading a finished net back as a term ----
def readback(n, port, names=None):
    names = {} if names is None else names
    a, i = port
    if i != 0: raise ValueError(f'not a value: port {port} of {n.kind.get(a)}')
    k = n.kind[a]
    rb = lambda p: readback(n, n.mate[p], names)
    if k in ('lam', 'Lam'):
        x = f'x{len(names)}'
        names[(a, 1)] = x
        return ('λ' if k == 'lam' else 'Λ', x, rb((a, 2)))
    if k == 'app': return (rb((a, 1)), rb((a, 2)))
    if k == 'rho': return 'ρ'
    if k == 'rhok': return 'RHO'
    if k in ('P1', 'P2', 'P3', 'P4'): return ('part', tuple(rb((a, j)) for j in range(1, int(k[1]) + 1)))
    if k in POLICIES: return ('tab', k, tuple(rb((a, j)) for j in range(1, 6)))
    if k == 'B1':
        arms, b = [], a
        for lvl in range(1, 5):
            arms.append(rb((b, 1)))
            nxt = n.mate[(b, 2)]
            if lvl < 4: b = nxt[0]
            else: arms.append(readback(n, nxt, names))
        return ('tab', 'chain', tuple(arms))
    if is_ref(k): return ('tab', 'ref', REFS[int(k[3:])])
    if k == 'var':
        src = n.mate[(a, 1)]
        while n.kind[src[0]] == 'sig' and src[1] in (1, 2):
            src = n.mate[(src[0], 0)]
        if src in names: return names[src]
        return ('~', readback(n, src, names))
    raise ValueError(f'not finished: {k} at the result')


def canon(t):
    names = {}
    def go(t, env):
        if isinstance(t, str): return env.get(t, t)
        if t[0] == '~': return ('~', go(t[1], {}))
        if t[0] == 'part': return ('part', tuple(go(a, {}) for a in t[1]))
        if is_tab(t): return ('tab', 'keep' if t[1] == 'keep' else t[1], tuple(go(a, env) for a in t[2]))
        if len(t) == 3:
            x = f'x{len(names)}'
            names[x] = 1
            return (t[0], x, go(t[2], {**env, t[1]: x}))
        return (go(t[0], env), go(t[1], env))
    return go(t, {})


# ---- the reference: term rewriting with tables, written apart from the net ----
def subst(c, x, s):
    if c == x: return s
    if isinstance(c, str): return c
    if c[0] in ('~', 'part'): return c
    if is_tab(c): return ('tab', c[1], tuple(subst(a, x, s) for a in c[2]))
    if len(c) == 3: return c if c[1] == x else (c[0], c[1], subst(c[2], x, s))
    return (subst(c[0], x, s), subst(c[1], x, s))

def kind_parts(v):
    if v in ('ρ', 'RHO') or is_tab(v) or v[0] == 'part': return 4, [v]
    if v[0] == 'λ': return 0, [('Λ', v[1], v[2])]
    if v[0] == 'Λ': return 1, [('λ', v[1], v[2])]
    if v[0] == '~': return 3, [v[1]]
    return 2, [v[0], v[1]]

class Machine(ref.Machine):
    def run(self, c):
        if isinstance(c, str):
            if c in ('ρ', 'RHO'): return c
            raise AssertionError(f'free variable {c} while running')
        if c[0] == '~': return c[1]
        if c[0] == 'part' or is_tab(c) or c[0] in ('λ', 'Λ'): return c
        return self.apply(self.run(c[0]), self.run(c[1]))

    def apply(self, f, v):
        self.fuel -= 1
        if self.fuel < 0: raise Fuel
        if isinstance(f, tuple) and f[0] == 'λ':
            self.betas += 1
            return self.run(subst(f[2], f[1], ('~', v)))
        if isinstance(f, tuple) and f[0] == 'Λ': return subst(f[2], f[1], ('~', v))
        if f == 'ρ':
            if is_tab(v) or v == 'RHO' or v[0] == 'part': raise ref.Stuck('the old view has no reading of a table')
            return self.view(v)
        if f == 'RHO': return ('part', (v,))
        if isinstance(f, tuple) and f[0] == 'part':
            return ('tab', 'keep', f[1] + (v,)) if len(f[1]) == 4 else ('part', f[1] + (v,))
        if is_tab(f):
            _, policy, arms = f
            i, parts = kind_parts(v)
            g = self.run(arms[i])
            if policy == 'once': args = parts
            elif policy == 'hand': args = [arms[j] for j in range(5) if j != i] + parts
            else: args = [f] + parts
            for a in args: g = self.apply(g, a)
            return g
        raise ref.Stuck('applying code')


# ---- parsing: ref.parse plus  P[a | b | c | d | e]  and RHO ----
TOKENS = r"(?:once|hand|keep|chain|ref)\[|\]|\||\\|\^|\.|\(|\)|[A-Za-z_][A-Za-z0-9_']*|\d+[utscap]?|ρ"

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
            a, i = expr(toks, i + 1, defs, bound)
            i += 1
        elif isinstance(tok, str) and tok.endswith('['):
            arms = []
            i += 1
            while True:
                arm, i = expr(toks, i, defs, bound)
                arms.append(arm)
                if toks[i] == ']': i += 1; break
                assert toks[i] == '|'; i += 1
            assert len(arms) == 5, f'a table has five slots, not {len(arms)}'
            a = ('tab', tok[:-1], tuple(arms))
        else:
            if isinstance(tok, tuple): a = tok
            elif tok in ('ρ', 'rho'): a = 'ρ'
            elif tok == 'RHO': a = 'RHO'
            elif tok in bound: a = tok
            elif tok in defs: a = defs[tok]
            else: raise NameError(f'nothing binds {tok}')
            i += 1
        head = a if head is None else (head, a)
    return head, i


# ---- running ----
def reachable(n, root):
    seen, todo = set(), [root]
    while todo:
        a = todo.pop()
        if a in seen: continue
        seen.add(a)
        for i in range(ARITY[n.kind[a]] + 1):
            m = n.mate.get((a, i))
            if m is not None: todo.append(m[0])
    return len(seen)

def run_net(t, seed=None, limit=100000):
    n = CNet(seed)
    root = n.load(t)
    status = n.run(limit)
    res = None
    if status == 'done':
        res = canon(readback(n, n.mate[(root, 0)]))
        garbage = len(n.kind) - reachable(n, root)
        if garbage: status = f'garbage {garbage}'
    return status, res, n.stats

def run_ref(t, fuel=4000):
    m = Machine(fuel)
    try:
        return 'value', canon(m.run(t)), m.betas
    except ref.Stuck:
        return 'stuck', None, m.betas
    except (Fuel, MemoryError, RecursionError):        # a term that grows past memory counts as out of fuel
        return 'fuel', None, m.betas

def health(stats):
    """what must never happen: two copies' halves joined, a running agent copied or erased"""
    bad = []
    if stats['annihilated across two copies']: bad.append('annihilated across two copies')
    for k in stats:
        w = k.split()
        if w[0] in ('copy', 'erase') and is_live(w[1]): bad.append(k)
    return bad

def group(stats):
    """rewrites by kind: copy (sig/dup ⋈ X), annihilate (dup ⋈ dup), erase (era ⋈ X; erase-dup is a part of it), rest"""
    g = Counter()
    for k, v in stats.items():
        if k in ('interactions', 'reads', 'instantiated', 'annihilated across two copies') or k.startswith('slot '): continue
        if k.startswith('copy'): g['copy'] += v
        elif k == 'dup⋈dup': g['annihilate'] += v
        elif k.startswith('erase'):
            g['erase'] += v
            if k == 'erase dup': g['erase dup'] += v
        else: g['rest'] += v
    return g
