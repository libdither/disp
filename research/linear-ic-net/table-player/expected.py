"""Writes expected.json: what ../nets/tables.py makes of the programs of ../nets/tables_check.py, of the page's
adds and examples (read from engine.js), of written numbers, and of random programs, for check.mjs to hold the
engine to. Run from this folder: python expected.py"""
import sys, os, re, json, random, hashlib
here = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(here, '..', 'nets'))
from tables_check import PROGRAMS, TESTS, TABLE_TESTS, program, run_net, gen, in_thread
from tables_ref import source

FAMILIES = ['call', 'open', 'read', 'thaw', 'copy', 'erase']

def family(label):
    if label == 'O⋈clo': return 'call'
    if label == 'O⋈cloL': return 'open'
    if label == 'O⋈tab' or label.startswith('Ctab⋈'): return 'read'
    if label.startswith('T⋈'): return 'thaw'
    return label.split()[0]

def lists(t):
    return t if isinstance(t, (str, int)) or t is None else [lists(x) for x in t]

def entry(name, src, limit=5000000):
    st, got, stats, n = run_net(program(src), None, limit)
    fams = [0] * len(FAMILIES)
    for k, v in stats.items():
        if k != 'interactions': fams[FAMILIES.index(family(k))] += v
    defs = json.dumps([[b, x, lists(body), list(names)] for b, x, body, names in n.defs], ensure_ascii=False, separators=(',', ':'))
    return dict(name=name, src=src, status=st, limit=limit, interactions=stats['interactions'], families=fams, made=n.made,
                answer=lists(got), defs=hashlib.sha1(defs.encode()).hexdigest(), ndefs=len(n.defs))

def page_list(name):
    """the programs of a list in engine.js (ADDS or EXAMPLES): the first string of each row, group headers aside"""
    text = open(os.path.join(here, 'engine.js'), encoding='utf8').read()
    body = text[text.index(f'const {name} = ['):]
    body = body[:body.index('\n];')]
    return [json.loads(f'"{s}"') for s in re.findall(r'^\s*\["((?:[^"\\]|\\.)*)",(?!\s*\[)', body, re.M)]

def main():
    tests = [entry(name, src) for name, src in TESTS + TABLE_TESTS]
    srcs = ['linear_unary_add 40u 3u', 'linear_list_add 1000 2000', 'linear_list_add 0 0', '(\\x y. y x) 7 0u', '(\\f k. k (f 1u) (f 2u)) (linear_unary_add 3u)',
            'linear_tree_add 0t 0t', 'linear_tree_add 100000t 200000t', 'ordinary_tree_add 5t 6t', '(\\x y. y x) 5t 300t',
            'linear_tree_add_parallel 0t 0t', 'linear_tree_add_parallel 100000t 200000t', 'ordinary_tree_add_parallel 5t 6t',
            '(\\x y k. k y x) 3s 0s', '(\\x y z w k. k w z y x) 9c 300a 7p 2', 'reversible_tree_sub 100000t 200000t']
    srcs += [s for s in page_list('ADDS') + page_list('EXAMPLES') if s not in srcs]
    tests += [entry(src, src, 200000) for src in srcs]
    rng = random.Random(3)
    for i in range(600):
        t = gen(rng, rng.randint(3, 30), [], i % 3 > 0, i % 3 == 2)
        st, got, stats, n = run_net(t, None, 20000)
        if st == 'limit': continue
        tests.append(entry(f'random {i}', source(t), 20000))
    json.dump(dict(prelude=PROGRAMS, tests=tests), open(os.path.join(here, 'expected.json'), 'w'), ensure_ascii=False, separators=(',', ':'))
    print(len(tests), 'programs written')

in_thread(main)
