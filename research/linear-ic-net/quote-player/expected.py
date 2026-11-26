"""Writes expected.json: what ../ideal/knet.py makes of the programs of ../ideal/kcheck.py, of the page's
examples (read from engine.js), and of random programs, each also run on kref.py's machine, for check.mjs to
hold the engine to. Run from this folder: python expected.py"""
import sys, os, re, json, random, threading
here = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(here, '..', 'ideal'))
from kcheck import PROGRAMS, TESTS, program, run_net, run_ref, gen
from kref import canon

FAMILIES = ['call', 'open', 'read', 'thaw', 'copy', 'cancel', 'erase']
SKIP = ('equal: size and size',)        # over three million rewrites

def family(label):
    if label == 'O⋈lam': return 'call'
    if label in ('O⋈var', 'A⋈lam'): return 'open'
    if label in ('O⋈tab', 'A⋈tab') or label.startswith('C⋈'): return 'read'
    if label.startswith('T⋈'): return 'thaw'
    if label == 'dup⋈dup': return 'cancel'
    return label.split()[0]

def lists(t):
    return t if isinstance(t, str) or t is None else [lists(x) for x in t]

def entry(name, src, t, limit):
    st, got, stats, n = run_net(t, None, limit)
    fams = [0] * len(FAMILIES)
    for k, v in stats.items():
        if k in ('interactions', 'made', 'annihilated across two copies') or k.startswith('read '): continue
        fams[FAMILIES.index(family(k))] += v
    kind, want, _ = run_ref(t, 3000000) if st != 'limit' else ('not run', None, None)
    if st == 'done': assert kind == 'value' and want == got, (name, kind, want, got)
    if st == 'stuck': assert kind == 'stuck', (name, kind)
    return dict(name=name, src=src, status=st, limit=limit, interactions=stats['interactions'], families=fams,
                made=stats['made'], answer=lists(got), ref=kind)

def page_examples():
    """the programs of EXAMPLES in engine.js: the first string of each row, group headers aside"""
    text = open(os.path.join(here, 'engine.js'), encoding='utf8').read()
    body = text[text.index('const EXAMPLES = ['):]
    body = body[:body.index('\n];')]
    return [json.loads(f'"{s}"') for s in re.findall(r'^\s*\["((?:[^"\\]|\\.)*)",(?!\s*\[)', body, re.M)]

def main():
    tests = [entry(name, src, program(src), 3000000) for name, src in TESTS if name not in SKIP]
    tests += [entry(src, src, program(src.replace(';', '\n')), 400000) for src in page_examples()]
    rng = random.Random(3)
    for i in range(600):
        t = gen(rng, rng.randint(3, 30), [], i % 3 > 0, i % 3 == 2)
        if run_net(t, None, 20000)[0] == 'limit': continue
        tests.append(entry(f'random {i}', None, t, 20000))
        tests[-1]['term'] = lists(t)
    json.dump(dict(prelude=PROGRAMS, families=FAMILIES, tests=tests), open(os.path.join(here, 'expected.json'), 'w'), ensure_ascii=False, separators=(',', ':'))
    print(len(tests), 'programs written')

sys.setrecursionlimit(1000000)
threading.stack_size(512 * 1024 * 1024)
th = threading.Thread(target=main)
th.start()
th.join()
