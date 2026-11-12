"""Writes expected.json: what ../nets/net.py makes of the hand-written programs and of random ones, for
check.mjs to hold the player's engine to. Run from this folder: python expected.py"""
import sys, os, json, random
here = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(here, '..', 'nets'))
sys.setrecursionlimit(1000000)
from check import PROGRAMS, TESTS, program, run_net, gen

# two runs of several million interactions are left out: they add minutes and no new rule
LONG = ('interpret: binders in suc', 'interpret the interpreter, reading')

def source(t):
    """a term as program text"""
    if isinstance(t, str): return t
    if len(t) == 3: return f"({'^' if t[0] == 'Λ' else chr(92)}{t[1]}. {source(t[2])})"
    return f'({source(t[0])} {source(t[1])})'

def lists(t):
    return t if isinstance(t, str) or t is None else [lists(x) for x in t]

tests = []
for name, src in TESTS:
    if name in LONG: continue
    status, got, stats = run_net(program(src), None, 60000000)
    tests.append(dict(name=name, src=src, status=status, interactions=stats['interactions'], answer=lists(got),
                      crossed=stats['annihilated across two copies']))
# written numbers, large enough to show the engine builds one as net.py does
for src in ('linear_unary_add 40u 3u', 'linear_list_add 1000 2000', 'linear_list_add 0 0', '(\\x y. y x) 7 0u', '(\\f k. k (f 1u) (f 2u)) (linear_unary_add 3u)',
            'linear_tree_add 0t 0t', 'linear_tree_add 100000t 200000t', 'ordinary_tree_add 5t 6t', '(\\x y. y x) 5t 300t',
            'linear_tree_add_parallel 0t 0t', 'linear_tree_add_parallel 100000t 200000t', 'ordinary_tree_add_parallel 5t 6t',
            '(\\x y k. k y x) 3s 0s', '(\\x y z w k. k w z y x) 9c 300a 7p 2', 'reversible_tree_sub 100000t 200000t',
            # the adds the page lists (engine.js ADDS)
            'linear_unary_add 40u 2u', 'ordinary_unary_add 40s 2s', 'linear_list_add 1014 2413', 'linear_list_add_two_shapes 1014a 2413p',
            'ordinary_list_add 1014c 2413c', 'linear_tree_add 1014t 2413t', 'ordinary_tree_add 1014t 2413t', 'reversible_tree_add 1014t 2413t',
            'reversible_tree_sub 1014t 3427t', 'reversible_tree_add 1014t 2413t reversible_tree_sub', 'linear_tree_add_parallel 1014t 2413t',
            'ordinary_tree_add_parallel 1014t 2413t', 'reversible_tree_add_parallel 1014t 2413t', 'reversible_tree_sub_parallel 1014t 3427t',
            'reversible_tree_add_parallel 1014t 2413t reversible_tree_sub_parallel'):
    status, got, stats = run_net(program(src), None, 60000000)
    tests.append(dict(name=src, src=src, status=status, interactions=stats['interactions'], answer=lists(got),
                      crossed=stats['annihilated across two copies']))
rng = random.Random(3)
for i in range(600):
    t = gen(rng, rng.randint(3, 30), [], i % 3 > 0, i % 3 == 2)
    status, got, stats = run_net(t, None, 20000)
    if status == 'limit': continue
    tests.append(dict(name=f'random {i}', src=source(t), status=status, interactions=stats['interactions'], answer=lists(got),
                      crossed=stats['annihilated across two copies']))
json.dump(dict(prelude=PROGRAMS, tests=tests), open(os.path.join(here, 'expected.json'), 'w'), ensure_ascii=False, separators=(',', ':'))
print(len(tests), 'programs written')
