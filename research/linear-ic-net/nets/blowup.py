"""How big bracket abstraction makes programs, against the agent count of the same λ-term as a net.
λ-terms: variables (strings), pairs (application), ('λ', x, body)."""
import os, sys, random
sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', 'tree-calculus'))
sys.setrecursionlimit(100000)
from rules import A, L, size
from family import DESIGNS, rules_of, make_compiler
from family_test import eager
from compile import compile_, br_tc

V5 = DESIGNS['V5 I K C | W B B2']
ltc = make_compiler(V5)
run_ltc = eager(rules_of(V5))
def tc(t): return compile_(t, br_tc)

def lam(xs, body):
    for x in reversed(xs): body = ('λ', x, body)
    return body
def apps(f, *xs):
    for x in xs: f = (f, x)
    return f

def term_size(t):
    if isinstance(t, tuple) and len(t) == 3: return 1 + term_size(t[2])
    if isinstance(t, tuple): return 1 + term_size(t[0]) + term_size(t[1])
    return 1

def net_size(t):
    """agents of the compiled net: one per λ, application and occurrence, plus a sharer per extra use and an eraser per unused binder"""
    def uses(x, m):
        if m == x: return 1
        if isinstance(m, tuple) and len(m) == 3: return 0 if m[1] == x else uses(x, m[2])
        if isinstance(m, tuple): return uses(x, m[0]) + uses(x, m[1])
        return 0
    def go(m):
        if isinstance(m, tuple) and len(m) == 3:
            k = uses(m[1], m[2])
            return 1 + (k - 1 if k >= 2 else 1 if k == 0 else 0) + go(m[2])
        if isinstance(m, tuple): return 1 + go(m[0]) + go(m[1])
        return 1
    return go(t)

def families(n):
    xs = [f'x{i}' for i in range(1, n + 1)]
    yield 'reverse', lam(xs + ['k'], apps('k', *reversed(xs)))
    nest = xs[-1]
    for x in reversed(xs[:-1]): nest = (x, nest)
    yield 'nested', lam(xs, nest)
    yield 'each twice', lam(xs + ['k'], apps('k', *xs, *xs))

def rterm(rng, n, env):
    """a random term with exactly n nodes whose free variables come from env (closed when env is the binders above)"""
    if n == 1:
        return rng.choice(env)
    if n == 2 or rng.random() < 0.3:
        x = f'v{len(env)}'
        return ('λ', x, rterm(rng, n - 1, env + [x]))
    k = rng.randint(1, n - 2)
    return (rterm(rng, k, env), rterm(rng, n - 1 - k, env))

if __name__ == '__main__':
    print('family        n  term  net  TC tree  9-rule tree   9-rule steps to apply')
    for n in (2, 4, 8, 12, 16):
        for name, t in families(n):
            args = [A(L, A(L, L)) for _ in range(n)]   # any closed trees will do
            tree = ltc(t)
            steps = run_ltc(A(tree, *args))[1]['steps'] if name != 'nested' else '-'
            print(f'{name:11} {n:3} {term_size(t):5} {net_size(t):4} {size(tc(t)):8} {size(tree):12}   {steps}')
    rng = random.Random(1)
    print('\nrandom closed terms (40 per size): average sizes')
    print('term   net   TC tree   9-rule tree')
    for m in (10, 20, 40, 80, 160, 320):
        tot = [0, 0, 0, 0]
        for _ in range(40):
            t = ('λ', 'v0', rterm(rng, m - 1, ['v0']))
            assert term_size(t) == m
            for i, v in enumerate((term_size(t), net_size(t), size(tc(t)), size(ltc(t)))): tot[i] += v
        print('  '.join(f'{v / 40:7.1f}' for v in tot))
