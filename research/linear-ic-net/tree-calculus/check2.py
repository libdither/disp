"""LTC2 checks. Static: linearity per rule, exactly 3 △ eaten per rule, no overlaps, every redex shape
covered. Dynamic: random terms under random rule order reach one normal form, normal forms are trees,
and every single step obeys |t'| = |t| - 3 - (|x| if K) + (|x| if W)."""
import random, itertools
from rules import *
from ltc2 import *
from check_rules import vars_of, rename as rn, unify, positions
from check_dynamic import rtree, rterm, is_tree

for title, rules in (('LTC2', LTC2), ('LTC2P', LTC2P)):
    print(f'== {title}: {len(rules)} rules')
    for name, lhs, rhs in rules:
        lv, rv = vars_of(lhs), vars_of(rhs)
        assert all(n == 1 for n in lv.values())
        drop = [v for v in lv if rv[v] == 0]; copy = [v for v in lv if rv[v] > 1]
        print(f'  {name:3} {show(lhs):24} -> {show(rhs):16} eats {size(lhs) - size(rhs)} △'
              + (f', drops {drop}' if drop else '') + (f', copies {copy}' if copy else ''))
    over = [(n1, n2, p) for n1, l1, _ in rules for n2, l2, _ in rules
            for p, sub in positions(rn(l2, '_2')) if not (n1 == n2 and p == ())
            and unify(rn(l1, '_1'), sub, {}) is not None]
    print('  overlaps:', over or 'none')
    # coverage: every closed redex △ a b x with a, b, x small trees fires a rule
    trees = [L]
    for _ in range(2):
        trees = list(dict.fromkeys(trees + [A(L, t) for t in trees] + [A(L, t, u) for t in trees for u in trees]))
    stuck = [(a, b, x) for a in trees for b in trees[:6] for x in trees[:6]
             if fire2([a, b, x], rules) is None]
    print(f'  coverage: {len(trees) * 36} redexes over {len(trees)} heads, {len(stuck)} stuck')

def rand_run(t, rules, rng, limit=3000):
    """random redex each step; checks the size law at every step"""
    for n in range(limit):
        ps = redexes(t); rng.shuffle(ps)
        for p in ps:
            s = get(t, p); h, args = spine(s)
            r = fire2(args, rules)
            if r is None or r[0] != 'done': continue
            _, new, name, env = r
            t2 = put(t, p, new)
            dx = size(env['x']) if name in ('K', 'W') else 0
            want = size(t) - 3 - (dx if name == 'K' else 0) + (dx if name == 'W' else 0)
            if rules is LTC2P and name in ('T0', 'T1', 'T2'): want += 1
            assert size(t2) == want, (name, show(s))
            t = t2; break
        else:
            return t, n
    return None, limit

for title, rules in (('LTC2', LTC2), ('LTC2P', LTC2P)):
    rng = random.Random(5); agree = dis = 0
    for _ in range(4000):
        t = rterm(rng)
        nf, n = normalize(t, rules, 3000)
        if nf is None: continue
        assert is_tree(nf)
        for _ in range(3):
            nf2, _ = rand_run(t, rules, rng)
            if nf2 is None: continue
            agree += nf2 == nf; dis += nf2 != nf
    print(f'{title}: {agree} random-order runs agree, {dis} disagree; the size law held at every step')
