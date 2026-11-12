"""Every add written for the nets so far, measured the same way, on numbers of 2 to 1,024 bits, and beside
them the reversible add and its subtract, on the same two numbers.

For each run: the work (rewrites, and what kind), the time in rounds (every pair that waits when a round
starts fires at once, so this is the time with no limit on how many rewrites can happen together), the
widest round, and the room (the most agents there ever were). Each size is run on six pairs of numbers:
0 + 0, all ones + 1 (the longest carry), all ones + all ones, and three random pairs. At 256 bits each
add is also run with a limit on how many pairs may fire in a round, from 1 (one rewrite after another)
up. The unary adds are run on m + m for m of 1 to 10 bits, since their cost goes with the number itself.

Prints the tables and writes the numbers into adders.html, which draws them. With --quick it stops at 64
bits (about two minutes) and only prints."""
import os, sys, json, random, threading
here = os.path.dirname(os.path.abspath(__file__))
from ref import unary, binary, tree, number_of
from linear import run, fields, scott, scott_of, bool_list, bools_of, arm_word, chain, chain_of, bits, total

SIZES = [2, 4, 8, 16, 32, 64, 128, 256, 512, 1024]
WIDTHS = [1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, None]
WIDTH_BITS = 256
QUICK = '--quick' in sys.argv
if QUICK: SIZES, WIDTH_BITS = SIZES[:6], 64
PARTS = ('copy', 'cancel', 'erase', 'call', 'thaw')

trees = lambda a, b, n: (tree(a, n.bit_length() - 1), tree(b, n.bit_length() - 1))
tree_sum = lambda got: number_of(fields(got)[0])[0]
both = lambda got: tuple(number_of(f)[0] for f in fields(got))
plus = lambda a, b, n: (a + b) % (1 << n)
# the program, its name in the tables, the shape of its numbers, its kind, how it carries, its call on two numbers of
# n bits, how to read its answer, and what the answer should be
BINARY = [
    ('linear_list_add', 'linear list', 'list', 'linear', 'ripple', lambda a, b, n: ('linear_list_add', binary(a, n), binary(b, n)), tree_sum, plus),
    ('linear_list_add_two_shapes', 'linear list, two shapes', 'list', 'linear', 'ripple',
     lambda a, b, n: ('linear_list_add_two_shapes', arm_word(bits(a, n)), chain(bits(b, n))), lambda got: total(chain_of(fields(got)[1])), plus),
    ('ordinary_list_add', 'ordinary list', 'list', 'ordinary', 'ripple',
     lambda a, b, n: ('ordinary_list_add', bool_list(bits(a, n)), bool_list(bits(b, n))), lambda got: total(bools_of(got)), plus),
    ('linear_tree_add', 'linear tree', 'tree', 'linear', 'ripple', lambda a, b, n: ('linear_tree_add', *trees(a, b, n)), tree_sum, plus),
    ('ordinary_tree_add', 'ordinary tree', 'tree', 'ordinary', 'ripple', lambda a, b, n: ('ordinary_tree_add', *trees(a, b, n)), tree_sum, plus),
    ('linear_tree_add_parallel', 'linear tree, parallel', 'tree', 'linear', 'parallel', lambda a, b, n: ('linear_tree_add_parallel', *trees(a, b, n)), tree_sum, plus),
    ('ordinary_tree_add_parallel', 'ordinary tree, parallel', 'tree', 'ordinary', 'parallel',
     lambda a, b, n: ('ordinary_tree_add_parallel', *trees(a, b, n)), tree_sum, plus),
    # the reversible pair: the add hands back a and a + b; the subtract, run here on the same two numbers, a and b - a
    ('reversible_tree_add', 'reversible tree', 'tree', 'reversible', 'ripple', lambda a, b, n: ('reversible_tree_add', *trees(a, b, n)), both,
     lambda a, b, n: (a, plus(a, b, n))),
    ('reversible_tree_sub', 'reversible tree, subtract', 'tree', 'reversible', 'ripple', lambda a, b, n: ('reversible_tree_sub', *trees(a, b, n)), both,
     lambda a, b, n: (a, (b - a) % (1 << n))),
    ('reversible_tree_add_parallel', 'reversible tree, parallel', 'tree', 'reversible', 'parallel',
     lambda a, b, n: ('reversible_tree_add_parallel', *trees(a, b, n)), both, lambda a, b, n: (a, plus(a, b, n))),
    ('reversible_tree_sub_parallel', 'reversible tree, parallel, subtract', 'tree', 'reversible', 'parallel',
     lambda a, b, n: ('reversible_tree_sub_parallel', *trees(a, b, n)), both, lambda a, b, n: (a, (b - a) % (1 << n))),
]
UNARY = [
    ('linear_unary_add', 'linear unary', 'linear', lambda m: ('linear_unary_add', unary(m), unary(m)), lambda got: number_of(fields(got)[0])[0]),
    ('ordinary_unary_add', 'ordinary unary', 'ordinary', lambda m: ('ordinary_unary_add', scott(m), scott(m)), scott_of),
]

def inputs(n, rng):
    top = (1 << n) - 1
    return [('0 + 0', 0, 0), ('longest carry', top, 1), ('all ones', top, top)] + \
           [('random', rng.randrange(1 << n), rng.randrange(1 << n)) for _ in range(3)]


def one(call, read, want, width=None):
    """one run: the cost, with the rounds, the widest round, the most agents and the agents at the start"""
    fn, *args = call
    got, cost, span = run(fn, *args, limit=10 ** 9, width=width, spans=True)
    assert read(got) == want, (fn, want)
    return {**cost, **span}


def measure():
    rng = random.Random(21)
    cases = {n: inputs(n, rng) for n in SIZES}
    out = []
    for key, name, shape, kind, way, call, read, want in BINARY:
        rows = {n: [one(call(a, b, n), read, want(a, b, n)) for _, a, b in cases[n]] for n in SIZES}
        mean = lambda n, k: sum(r.get(k, 0) for r in rows[n][3:]) / 3     # over the random pairs
        spread = lambda n, k: [min(r[k] for r in rows[n]), max(r[k] for r in rows[n])]
        out.append(dict(key=key, name=name, shape=shape, kind=kind, way=way,
                        **{k: [mean(n, k) for n in SIZES] for k in ('rewrites', 'rounds', 'widest', 'peak', 'start', 'copies_started', 'erasers') + PARTS},
                        spread={k: [spread(n, k) for n in SIZES] for k in ('rewrites', 'rounds', 'peak')}))
        print(f'  measured {name}', file=sys.stderr, flush=True)
    return out


def unary_adds():
    out = []
    for key, name, kind, call, read in UNARY:
        rows = [one(call(m), read, 2 * m) for m in [(1 << k) - 1 for k in range(1, 11)]]
        out.append(dict(key=key, name=name, kind=kind, **{k: [r.get(k, 0) for r in rows] for k in ('rewrites', 'rounds', 'widest', 'peak', 'start') + PARTS}))
    return out


def limited():
    """the rounds of each add of 256-bit numbers when at most w pairs may fire in a round"""
    rng = random.Random(22)
    a, b = rng.randrange(1 << WIDTH_BITS), rng.randrange(1 << WIDTH_BITS)
    return {key: [one(call(a, b, WIDTH_BITS), read, want(a, b, WIDTH_BITS), width=w)['rounds'] for w in WIDTHS]
            for key, _, _, _, _, call, read, want in BINARY}


def table(title, head, rows):
    print(f'\n{title}')
    print('  ' + f'{"":36}' + ''.join(f'{h:>13}' for h in head))
    for name, cells in rows:
        print('  ' + f'{name:36}' + ''.join(f'{c:>13}' for c in cells))


def show(data):
    at = lambda n: SIZES.index(n)
    num = lambda v: f'{v:,.0f}'
    per = lambda a, k: (a[k][-1] - a[k][-2]) / (SIZES[-1] - SIZES[-2])
    A = data['binary']
    table('a + b on 64-bit numbers (three random pairs)',
          ['rewrites', 'rounds', 'per round', 'widest round', 'most agents', 'at the start', 'copies', 'erases'],
          [(a['name'], [num(a['rewrites'][at(64)]), num(a['rounds'][at(64)]), f"{a['rewrites'][at(64)] / a['rounds'][at(64)]:.1f}", num(a['widest'][at(64)]),
                        num(a['peak'][at(64)]), num(a['start'][at(64)]), num(a['copies_started'][at(64)]), num(a['erase'][at(64)])]) for a in A])
    table(f'how they grow: for each bit more, from {SIZES[-2]:,} to {SIZES[-1]:,} bits; and the rounds that doubling adds',
          ['rewrites', 'copy+cancel', 'erase', 'call', 'thaw', 'rounds', 'agents', 'rounds added'],
          [(a['name'], [f"{per(a, 'rewrites'):.1f}", f"{per(a, 'copy') + per(a, 'cancel'):.1f}", f"{per(a, 'erase'):.1f}", f"{per(a, 'call'):.1f}",
                        f"{per(a, 'thaw'):.1f}", f"{per(a, 'rounds'):.2f}", f"{per(a, 'peak'):.1f}", f"{a['rounds'][-1] - a['rounds'][-2]:,.0f}"]) for a in A])
    table('what the numbers change, at 64 bits: least and most over 0 + 0, the longest carry, all ones and three random pairs',
          ['rewrites', '', 'rounds', '', 'most agents', ''],
          [(a['name'], [num(a['spread']['rewrites'][at(64)][0]), num(a['spread']['rewrites'][at(64)][1]), num(a['spread']['rounds'][at(64)][0]),
                        num(a['spread']['rounds'][at(64)][1]), num(a['spread']['peak'][at(64)][0]), num(a['spread']['peak'][at(64)][1])]) for a in A])
    table(f'rounds for {WIDTH_BITS}-bit numbers when at most this many pairs may fire in a round',
          [str(w) if w else 'any' for w in WIDTHS if w in (1, 4, 16, 64, 256, 1024, None)],
          [(a['name'], [num(r) for w, r in zip(WIDTHS, data['limited'][a['key']]) if w in (1, 4, 16, 64, 256, 1024, None)]) for a in A])
    table('unary, m + m: for m of this many bits',
          [f'{k} bits' for k in (2, 4, 6, 8, 10)],
          [(f"{u['name']}, {what}", [num(u[what][k - 1]) for k in (2, 4, 6, 8, 10)]) for u in data['unary']
           for what in ('rewrites', 'rounds', 'peak')])


def main():
    data = dict(sizes=SIZES, parts=PARTS, widths=[w or 0 for w in WIDTHS], width_bits=WIDTH_BITS,
                binary=measure(), unary=unary_adds(), limited=limited())
    show(data)
    page = os.path.join(here, 'adders.html')
    if QUICK: return
    if os.path.exists(page):
        html = open(page).read()
        start, end = '<script id="data" type="application/json">', '</script>'
        at = html.index(start) + len(start)
        open(page, 'w').write(html[:at] + json.dumps(data) + html[html.index(end, at):])
        print('\nwritten into adders.html')
    else:
        json.dump(data, open(os.path.join(here, 'adders.json'), 'w'))


if __name__ == '__main__':
    threading.stack_size(1 << 29)        # reading a long number back goes deep
    t = threading.Thread(target=main)
    t.start(); t.join()
