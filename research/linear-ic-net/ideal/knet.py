"""The candidate net ("quote-and-table net"): the reducer.

It is ../nets/net.py with the reader rebuilt. Kept: variables are wires, a function body stays inert until
called, one copier, one eraser. Changed:
  - four kinds of code, not five: lam, app, var, rho. `var` is both a variable use and a quote (its one
    auxiliary port leads to a binder, or to a piece of code). There is no opened-function agent.
  - a function is opened by applying a quote of it: O ⋈ var turns the application into A ("open-apply"),
    and A ⋈ lam is a plain annihilation (argument to the binder, body to the result, nothing thawed).
  - reading is done by a table agent that holds one slot per kind and hands itself back: the slot taken is
    copied, and the read leaves a piece of code: the copy applied to the table and then to the parts of the
    value read. Applied, a table thaws that code; opened (a quote of it applied), it hands the code back.

Agents (principal port first):
  code     lam(p; x, b)  app(p; f, a)  var(p; s)  rho(p)  tab(p; k1..k5)
  running  O(p; a, r) apply   A(p; a, r) open-apply   T(p; o) thaw   C(p; k1..k5, r) a table reading
  resources  sig(p; a, b)  dup(p; a, b)  era(p)

Rules:
  O ⋈ lam   x–a, b → T → r                      (run)
  O ⋈ var   A(p → what the quote holds; a, r)   (a quote applied: open what it quotes)
  A ⋈ lam   x–a, b–r                            (annihilate: the body comes back as code)
  O ⋈ tab   C(p → the argument; the five slots; r → T → result)      (read, then run what the read leaves)
  A ⋈ tab   C(p → the argument; the five slots; r → result)          (read, and hand back what it leaves)
  C ⋈ X     X of kind i (lam 0, app 1, var 2, rho 3, tab 4): a dup on slot i; one copy goes back into a
            rebuilt tab; the result is the code  ~copy ~tab ~p1 … ~pn  (app and var agents), p1 … pn the
            parts of X (lam: the function itself; app: f, a; var: s; rho: none; tab: its five slots)
  T ⋈ app   O with T on f and on a, r → o
  T ⋈ var   s–o                                  (annihilate)
  T ⋈ tab   the tab with a T on each slot        (Lafont's commutation)
  T ⋈ lam | rho   the value itself
  sig | dup ⋈ X   copy X (code);  dup ⋈ dup annihilate;  era ⋈ X erase X;  era ⋈ era vanish
No rule (stuck): O ⋈ app, O ⋈ rho, A ⋈ app | var | rho.
"""
import sys, os
HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', 'nets'))
import net
from net import Net, ARITY, Stuck
from kref import NSLOT, is_tab, is_quote, is_lam

ARITY.update({'tab': NSLOT, 'C': NSLOT + 1, 'A': 2})
CODE = ('lam', 'app', 'var', 'rho', 'tab')
KIND = {'lam': 0, 'app': 1, 'var': 2, 'rho': 3, 'tab': 4}
LIVE = ('O', 'A', 'T', 'C', 'root', 'ph')


class KNet(Net):
    def emit(self, t, env):
        """Code for term t; returns its principal port. env maps a bound name to the list of its uses."""
        if t == 'ρ':
            return (self.new('rho'), 0)
        if is_quote(t):
            v = self.new('var')
            self.link((v, 1), self.emit(t[1], env))
            return (v, 0)
        if is_tab(t):
            tb = self.new('tab')
            for j, k in enumerate(t[1:]): self.link((tb, j + 1), self.emit(k, env))
            return (tb, 0)
        if is_lam(t):
            _, x, body = t
            lam = self.new('lam')
            occ = []
            self.link((lam, 2), self.emit(body, {**env, x: occ}))
            self.bind((lam, 1), occ)
            return (lam, 0)
        if isinstance(t, tuple):
            a = self.new('app')
            self.link((a, 1), self.emit(t[0], env))
            self.link((a, 2), self.emit(t[1], env))
            return (a, 0)
        if t not in env: raise NameError(f'nothing binds {t}')
        v = self.new('var')
        env[t].append((v, 1))
        return (v, 0)

    def fire(self, a, b):
        ka, kb = self.kind[a], self.kind[b]
        for (x, y) in ((a, b), (b, a)):
            kx, ky = self.kind[x], self.kind[y]
            f = RULES.get((kx, ky))
            label = f'{kx}⋈{ky}'
            if f is None and ((kx in ('sig', 'dup') and ky in CODE) or (kx == 'dup' and ky == 'sig')):
                f, label = net.copy_rule, f'copy {ky} by {kx}'
            if f is None and kx == 'era' and (ky in CODE or ky in ('sig', 'dup', 'era')):
                f, label = net.erase_rule, f'erase {ky}'
            if f is not None:
                self.stats[label] += 1
                self.stats['interactions'] += 1
                made = len(self.kind)
                f(self, x, y)
                self.stats['made'] += len(self.kind) - made + 2
                return
        self.stuck.append((ka, kb))
        raise Stuck(f'no rule for {ka} ⋈ {kb}')


def r_quote_apply(n, o, v):          # O ⋈ var
    def build(slot):
        a = n.new('A')
        n.link((a, 0), slot(v, 1))
        n.link((a, 1), slot(o, 1))
        n.link((a, 2), slot(o, 2))
    n.rewrite(o, v, build)

def r_open(n, a, l):                 # A ⋈ lam
    n.rewrite(a, l, lambda slot: (n.link(slot(a, 1), slot(l, 1)), n.link(slot(a, 2), slot(l, 2))))

def r_read_start(n, o, tb):          # O ⋈ tab (the code read is thawed), A ⋈ tab (it is handed back)
    run = n.kind[o] == 'O'
    def build(slot):
        c = n.new('C')
        n.link((c, 0), slot(o, 1))
        for j in range(1, NSLOT + 1): n.link((c, j), slot(tb, j))
        if run:
            t = n.new('T')
            n.link((t, 0), (c, NSLOT + 1)); n.link((t, 1), slot(o, 2))
        else:
            n.link((c, NSLOT + 1), slot(o, 2))
    n.rewrite(o, tb, build)

def r_read(n, c, x):                 # C ⋈ X
    kx = n.kind[x]
    i = KIND[kx]
    n.stats[f'read {kx}'] += 1
    def build(slot):
        if kx == 'lam':
            f = n.new('lam')
            n.link((f, 1), slot(x, 1)); n.link((f, 2), slot(x, 2))
            parts = [(f, 0)]
        else:
            parts = [slot(x, j) for j in range(1, ARITY[kx] + 1)]
        d = n.new('dup')
        n.op[d] = next(n.ops)
        n.link((d, 0), slot(c, i + 1))
        t = n.new('tab')
        for j in range(NSLOT): n.link((t, j + 1), (d, 1) if j == i else slot(c, j + 1))
        def quoted(p):
            v = n.new('var')
            n.link((v, 1), p)
            return (v, 0)
        cur = quoted((d, 2))
        for p in [(t, 0)] + parts:
            a = n.new('app')
            n.link((a, 1), cur); n.link((a, 2), quoted(p))
            cur = (a, 0)
        n.link(cur, slot(c, NSLOT + 1))
    n.rewrite(c, x, build)

def r_thaw_tab(n, t, tb):            # T ⋈ tab
    def build(slot):
        w = n.new('tab')
        n.link((w, 0), slot(t, 1))
        for j in range(1, NSLOT + 1):
            th = n.new('T')
            n.link((th, 0), slot(tb, j)); n.link((th, 1), (w, j))
    n.rewrite(t, tb, build)

RULES = {
    ('O', 'lam'): net.r_run,
    ('O', 'var'): r_quote_apply,
    ('A', 'lam'): r_open,
    ('O', 'tab'): r_read_start,
    ('A', 'tab'): r_read_start,
    **{('C', k): r_read for k in CODE},
    ('T', 'app'): net.r_thaw_app,
    ('T', 'var'): net.r_thaw_var,
    ('T', 'lam'): net.r_thaw_value,
    ('T', 'rho'): net.r_thaw_value,
    ('T', 'tab'): r_thaw_tab,
    ('dup', 'dup'): net.r_annihilate,
}


def readback(n, port, names=None):
    """The term at a principal port."""
    names = {} if names is None else names
    a, i = port
    if i != 0: raise ValueError(f'not a value: port {port} of {n.kind.get(a)}')
    k = n.kind[a]
    if k == 'lam':
        x = f'x{len(names)}'
        names[(a, 1)] = x
        return ('λ', x, readback(n, n.mate[(a, 2)], names))
    if k == 'app':
        return (readback(n, n.mate[(a, 1)], names), readback(n, n.mate[(a, 2)], names))
    if k == 'rho':
        return 'ρ'
    if k == 'tab':
        return ('tab',) + tuple(readback(n, n.mate[(a, j)], names) for j in range(1, NSLOT + 1))
    if k == 'var':
        src = n.mate[(a, 1)]
        while n.kind[src[0]] == 'sig' and src[1] in (1, 2):
            src = n.mate[(src[0], 0)]
        if src in names: return names[src]
        return ('~', readback(n, src, names))
    raise ValueError(f'not finished: {k} at the result')
