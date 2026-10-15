"""Which of the tree-calculus net's rules could run backward with no memory beyond the net itself.

Salikhmetov (arXiv 1806.07275): an interaction system runs backward deterministically iff every rule
is connected (its right side cannot be split into two parts sharing no wire) and no two active pairs
clash (no net is the result of two different active pairs); in particular no two rules may have the
same arity (the left side's auxiliary ports, counted together).

Reads the rule table from evaluators/rust-ca-lattice/crate/src/rules.rs (RULES, and SUP_RULES with
--sup) plus the lattice's three collection moves, and prints for each rule: its arity, whether its
right side is connected (and the pieces if not), and which other rules' right sides look the same to
a reader of the net (same agents, wired the same way, the free wires unnamed).

    python rule_audit.py [--sup]
"""
import itertools, re, sys
from collections import defaultdict

ROOT = __file__.rsplit('/research/', 1)[0]
SRC = open(f'{ROOT}/evaluators/rust-ca-lattice/crate/src/rules.rs').read()
ARITY = {t: 1 for t in ['L', 'Eps', 'Out', 'I']} | {t: 2 for t in ['S', 'Nrm', 'K', 'W', 'Seam']}
ar = lambda t: ARITY.get(t, 3)
VALUES = {'L', 'S', 'F', 'P', 'Pair', 'Sup'}


def table(name):
    body = SRC[SRC.index(f'pub static {name}'):]
    body = body[:body.index('\n];') + 1]
    out = []
    for m in re.finditer(r'Rule \{ consumer: (\w+), producer: (\w+),(.*?)\},\s*(?://[^\n]*)?\n', body, re.S):
        c, p, rest = m.groups()
        fresh = re.search(r'fresh: &\[([^\]]*)\]', rest).group(1)
        wires = re.search(r'wires: &\[(.*?)\](?:\s*,|\s*$)', rest, re.S).group(1)
        when = re.search(r'when: When::(\w+)', rest)
        ends = lambda s: [('X', int(a), int(b)) if k == 'X' else (k, int(a), None)
                          for k, a, b in re.findall(r'(X|C|Px)\((\d+)(?:, (\d+))?\)', s)]
        pairs = re.findall(r'\((\w+\(\d+(?:, \d+)?\)), (\w+\(\d+(?:, \d+)?\))\)', wires)
        out.append(dict(name=f'{c}·{p}' + (f' ({when.group(1).lower()} labels)' if when and when.group(1) != 'Any' else ''),
                        c=c, p=p, fresh=[t.strip() for t in fresh.split(',') if t.strip()],
                        wires=[(ends(a)[0], ends(b)[0]) for a, b in pairs]))
    return out


def collection():
    """The lattice's collection moves (lattice.rs `collect`), as rules on the eraser and what it reads."""
    X = lambda f, q: ('X', f, q)
    return [
        dict(name='un-copy: Eps on Dn copy 1', c='Eps', p='Dn', fresh=[], wires=[(('Px', 0, None), ('Px', 2, None))], note='Dn port 0 (its input) joined to its copy 2'),
        dict(name='dead work: Eps on A result', c='Eps', p='A', fresh=['Eps', 'Eps'], wires=[(X(0, 0), ('Px', 0, None)), (X(1, 0), ('Px', 1, None))], note='erasers on its two inputs'),
        dict(name='unpaired: Eps, Eps on Unp', c='Eps', p='Unp', fresh=['Eps'], wires=[(X(0, 0), ('Px', 0, None))], note='one eraser on its input'),
    ]


def check(r):
    """Every port used once: the free ports are the left side's auxiliary ports."""
    seen = [e for w in r['wires'] for e in w]
    want = [('X', f, q) for f, t in enumerate(r['fresh']) for q in range(ar(t))]
    if r['name'].startswith(('un-copy', 'dead', 'unpaired')): return
    want += [('C', i, None) for i in range(1, ar(r['c']))] + [('Px', j, None) for j in range(1, ar(r['p']))]
    assert sorted(seen) == sorted(want), (r['name'], seen, want)


def pieces(r):
    """The right side's connected pieces, each as the fresh agents and free wires in it."""
    nodes = [('F', f) for f in range(len(r['fresh']))]
    node = lambda e: ('F', e[1]) if e[0] == 'X' else (e[0], e[1])
    free = sorted({node(e) for w in r['wires'] for e in w if e[0] != 'X'})
    nodes += free
    parent = {n: n for n in nodes}
    def find(n):
        while parent[n] != n: n = parent[n]
        return n
    for a, b in r['wires']: parent[find(node(a))] = find(node(b))
    groups = defaultdict(list)
    for n in nodes: groups[find(n)].append(n)
    show = lambda n: r['fresh'][n[1]] if n[0] == 'F' else {'C': 'c', 'Px': 'p'}[n[0]] + str(n[1])
    return [' '.join(show(n) for n in g) for g in groups.values()]


def shape(r):
    """The right side as a reader of the net sees it: agents and how they are wired to each other and
    to the outside, with the free wires unnamed; the same for two rules means neither can be undone
    without knowing which fired. Canonical over orderings of the fresh agents."""
    n, best = len(r['fresh']), None
    for perm in itertools.permutations(range(n)):
        pos = {f: perm[f] for f in range(n)}
        def end(e): return ('X', pos[e[1]], e[2]) if e[0] == 'X' else ('out',)
        tags = tuple(r['fresh'][perm.index(i)] for i in range(n))
        ws = tuple(sorted(tuple(sorted([end(a), end(b)], key=repr)) for a, b in r['wires']))
        key = repr((tags, ws))
        if best is None or key < best: best = key
    return best


def inside(r, q):
    """Whether r's right side occurs inside q's: r's agents matched one to one to q's agents of the same
    kind, every wire between two of r's agents also there in q (r's free wires may meet anything)."""
    n, m = len(r['fresh']), len(q['fresh'])
    if n == 0 or n > m or r is q: return False
    inner = [(a, b) for a, b in r['wires'] if a[0] == 'X' and b[0] == 'X']
    qw = {frozenset([(a[1], a[2]), (b[1], b[2])]) for a, b in q['wires'] if a[0] == 'X' and b[0] == 'X'}
    for img in itertools.permutations(range(m), n):
        if any(r['fresh'][f] != q['fresh'][img[f]] for f in range(n)): continue
        if all(frozenset([(img[a[1]], a[2]), (img[b[1]], b[2])]) in qw for a, b in inner): return True
    return False


def main():
    rules = table('RULES') + (table('SUP_RULES') if '--sup' in sys.argv else []) + collection()
    for r in rules: check(r)
    by_arity, by_shape = defaultdict(list), defaultdict(list)
    for r in rules:
        r['arity'] = ar(r['c']) - 1 + ar(r['p']) - 1
        by_arity[r['arity']].append(r['name'])
        by_shape[shape(r)].append(r['name'])
    print(f"{'rule':34} {'arity':>5}  {'right side':38} connected?")
    for r in rules:
        ps = pieces(r)
        rhs = ' '.join(r['fresh']) or ('wires only' if r['wires'] else 'nothing')
        conn = 'vanishes' if not ps else 'yes' if len(ps) == 1 else f"no: {len(ps)} pieces [{' | '.join(ps)}]"
        print(f"{r['name']:34} {r['arity']:>5}  {rhs:38} {conn}")
    print('\nrules sharing an arity (Salikhmetov: these clash):')
    for a, ns in sorted(by_arity.items()):
        if len(ns) > 1: print(f'  arity {a}: {", ".join(ns)}')
    print('\nright sides a reader cannot tell apart:')
    for ns in by_shape.values():
        if len(ns) > 1: print(f'  {", ".join(ns)}')
    print('\nright sides that also occur inside another rule\'s right side (ignoring single plain values):')
    for r in rules:
        if len(r['fresh']) == 1 and r['fresh'][0] in VALUES: continue
        hosts = [q['name'] for q in rules if shape(q) != shape(r) and inside(r, q)]
        if hosts: print(f"  {r['name']} ({' '.join(r['fresh'])}) inside {', '.join(hosts)}")
    print('\nright sides that are one plain value (indistinguishable from loaded matter):')
    print('  ' + ', '.join(r['name'] for r in rules if len(r['fresh']) == 1 and r['fresh'][0] in VALUES))


if __name__ == '__main__':
    main()
