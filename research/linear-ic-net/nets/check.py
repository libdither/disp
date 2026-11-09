"""Differential tests: the nets against the term interpreter, on random programs and on hand-written reflective ones."""
import sys, random
from collections import Counter
from net import Net, readback, ARITY
from ref import Machine, Stuck, Fuel, close, canon, parse

sys.setrecursionlimit(100000)


def gen(rng, n, env, rho):
    """a random term with n nodes whose names come from env; with rho, ρ and free symbols appear too"""
    if n == 1:
        pool = env + (['ρ', 'ρ', 'u', 'w'] if rho else [])
        return rng.choice(pool) if pool else 'ρ' if rho else ('λ', 'z', 'z')
    if n == 2 or rng.random() < 0.3:
        x = f'v{len(env)}'
        return ('λ', x, gen(rng, n - 1, env + [x], rho))
    k = rng.randint(1, n - 2)
    return (gen(rng, k, env, rho), gen(rng, n - 1 - k, env, rho))


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
    n = Net(seed)
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
        return 'value', canon(m.run(close(t))), m.betas
    except Stuck:
        return 'stuck', None, m.betas
    except Fuel:
        return 'fuel', None, m.betas


def compare(t, seeds=(None, 1, 2)):
    """None when both agree, else a description"""
    kind, want, _ = run_ref(t)
    if kind == 'fuel': return 'skip'
    for s in seeds:
        status, got, stats = run_net(t, s)
        if stats['annihilated across two copies']: return f'annihilated across copies (seed {s})'
        live = [k for k in stats if k.split()[0] in ('copy', 'erase') and k.split()[1] in ('O', 'T', 'R', 'root', 'ph')]
        if live: return f'live agents copied or erased: {live}'
        if kind == 'value' and (status != 'done' or got != want):
            return f'seed {s}: net {status} {got}\n   ref {want}'
        if kind == 'stuck' and status not in ('stuck', 'limit'):
            return f'seed {s}: ref stuck, net {status} {got}'
    return None


PROGRAMS = r'''
-- Church numerals and booleans, a fixpoint, and reflective programs reading code
zero := \f x. x
suc := \n f x. f (n f x)
add := \m n f x. m f (n f x)
true := \t e. t
false := \t e. e
Z := \g. (\h. g (\v. h h v)) (\h. g (\v. h h v))
-- kind of a value: each arm drops the arms it is handed and the parts
kindOf := \c. ρ c (\a v p F. true) (\l v p f a. false) (\l a p w. false) (\l a v. false)
-- count binders in code: open each binder on a symbol, recurse into both sides of an application
size := Z (\self c. ρ c
   (\a v p F. suc (self (F mark)))
   (\l v p f a. add (self f) (self a))
   (\l a p w. zero)
   (\l a v. zero))
'''

def program(body):
    return parse(PROGRAMS + '\n' + body)

TESTS = [
    ('kind of a function', 'kindOf (\\x. x)'),
    ('kind of ρ', 'kindOf ρ'),
    ('binders in λx. x', 'size (\\x. x)'),
    ('binders in λx y. y x', 'size (\\x y. y x)'),
    ('binders in a closure', 'size ((\\x y. x) (\\a b c. c))'),
    ('binders in suc', 'size suc'),
    ('binders in add 2 2', 'size (add (suc (suc zero)) (suc (suc zero)))'),
    ('binders in size itself', 'size size'),
    ('run after copying a closure', '(\\f. f (f zero)) (add (suc zero))'),
    ('open then read the body', 'ρ (\\x. x x) (\\a v p F. F u) l l l'),
    ('read an application part', 'ρ ((\\x. \\y. x y) w) (\\a v p F. F u) l l l'),
    ('share a piece of code', '(\\c k. k c c) (ρ (\\x. x x) (\\a v p F. F u) l l l)'),
    ('share a spliced value', '(\\c k. k c c) (ρ (\\x. x) (\\a v p F. F u) l l l)'),
    ('share an opened binder', '(\\g k. k g g) (ρ (\\x y. y x) (\\a v p F. F) l l l)'),
    ('reflect an opened binder', 'kindOf (ρ (\\x. x) (\\a v p F. F) l l l)'),
    ('open a shared copy twice', '(\\g. g u (g w)) (ρ (\\x y. x) (\\a v p F. F) l l l)'),
]


def main():
    rng = random.Random(7)
    tally = Counter()
    bad = []
    for rho in (False, True):
        for i in range(4000):
            size = rng.randint(3, 40)
            t = gen(rng, size, [], rho)
            r = compare(t)
            outcome = run_ref(t)[0]
            tally[(rho, outcome, 'skip' if r == 'skip' else 'ok' if r is None else 'BAD')] += 1
            if r not in (None, 'skip') and len(bad) < 5: bad.append((t, r))
    print('random programs (without ρ / with ρ):', dict(tally))
    for t, r in bad: print('  MISMATCH', t, '\n  ', r)
    for name, src in TESTS:
        t = program(src)
        kind, want, betas = run_ref(t, 200000)
        status, got, stats = run_net(t, None, 2000000)
        ok = kind == 'value' and status == 'done' and got == want and not stats['annihilated across two copies']
        others = [run_net(t, s, 2000000)[1] == got for s in (3, 4)]
        print(f"{'ok ' if ok and all(others) else 'BAD'} {name:28} ref {kind} {betas:5} β | net {status} {stats['interactions']:6} interactions"
              + ('' if ok else f'\n   net {got}\n   ref {want}'))


if __name__ == '__main__':
    main()
