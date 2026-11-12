"""How the adds scale: what m + m costs in unary, and a + b in binary, as the numbers grow. The nets' linear and
ordinary adds are counted in rewrites; beside them, the same sums in tree calculus and in the 9-rule linear
tree calculus (`../tree-calculus`, `../reversible`), counted in steps, with the nodes tree calculus copies
when nothing is shared. Prints the table and writes the numbers into scaling.html, which draws them."""
import os, sys, json, random
here = os.path.dirname(os.path.abspath(__file__))
for d in ('reversible', 'tree-calculus'): sys.path.insert(0, os.path.join(here, '..', d))
from ref import unary, binary, number_of
from linear import run, fields, scott, scott_of, bool_list, bools_of, arm_word, chain, chain_of, bits, total
os.chdir(os.path.join(here, '..', 'reversible'))   # the reversible adders read their stage trees from logs there
import common, ord_add, radd, adderd
import ltcfast as F
from rules import L, A
from compile import compile_, br_tc
from bench import programs, nat

UNARY = [1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024]
BINARY = [1, 2, 4, 8, 16, 32, 64, 128, 256]
PAIRS = 4   # random pairs of numbers at each width; the ordinary adds' cost depends a little on the bits


def unary_rows():
    """m + m for each m: (name, unit, [cost at each m]) per system"""
    add_tc = compile_(programs('tc')[1], br_tc)
    def nets(fn, number, answer):
        out = []
        for m in UNARY:
            got, cost = run(fn, number(m), number(m))
            assert answer(got) == 2 * m, (fn, m)
            out.append(cost['rewrites'])
        return out
    def trees(call, calc, expect):
        steps, copied = [], []
        for m in UNARY:
            v, st = F.run(call(m), calc)
            assert v == F.val(expect(m)), (calc, m)
            steps.append(st.steps); copied.append(st.copied)
        return steps, copied
    tc, tc_copied = trees(lambda m: A(add_tc, add_tc, nat(m), nat(m)), 'tc', lambda m: nat(2 * m))
    ltc, _ = trees(lambda m: A(radd.ADD, radd.ADD, nat(m), nat(m)), 'v5', lambda m: nat(2 * m))
    rev, _ = trees(lambda m: radd.radd_call(m, m), 'v5', lambda m: radd.radd_expect(m, m))
    return [('nets, linear', 'rewrites', nets('linear_unary_add', unary, lambda t: number_of(fields(t)[0])[0])),
            ('nets, ordinary', 'rewrites', nets('ordinary_unary_add', scott, scott_of)),
            ('tree calculus', 'steps', tc),
            ('linear tree calculus', 'steps', ltc),
            ('linear tree calculus, reversible', 'steps', rev),
            ('tree calculus, nodes copied', 'nodes', tc_copied)]


def binary_rows():
    """a + b for random a, b of each width, averaged over a few pairs: (name, unit, [cost at each width])"""
    rng = random.Random(8)
    pairs = {w: [(rng.randrange(1 << w), rng.randrange(1 << w)) for _ in range(PAIRS)] for w in BINARY}
    mean = lambda xs: sum(xs) / len(xs)
    def nets(fn, number, answer):
        out = []
        for w in BINARY:
            costs = []
            for a, b in pairs[w]:
                got, cost = run(fn, *number(a, b, w))
                assert answer(got) == (a + b) % (1 << w), (fn, w, a, b)
                costs.append(cost['rewrites'])
            out.append(mean(costs))
        return out
    P_tc, P_ltc = common.ord_tc(ord_add.badd(common.W8_TC)), common.ord_ltc(ord_add.badd(common.W8_LTC))
    D = adderd.AdderD().build()
    def trees(start, calc, ok):
        steps, copied = [], []
        for w in BINARY:
            got = []
            for a, b in pairs[w]:
                v, st = common.run(start(a, b, w), calc)
                assert ok(v, a, b, w), (calc, w, a, b)
                got.append(st)
            steps.append(mean([st.steps for st in got])); copied.append(mean([st.copied for st in got]))
        return steps, copied
    summed = lambda v, a, b, w: ord_add.unword(F.term(v)) == (a + b) % (1 << w)
    word = lambda P: lambda a, b, w: A(P, P, common.word(a, w), common.word(b, w), common.B0)
    tc, tc_copied = trees(word(P_tc), 'tc', summed)
    ltc, _ = trees(word(P_ltc), 'v5', summed)
    rev, _ = trees(D.start, 'v5', lambda v, a, b, w: v == common.V(D.expect(a, b, w)))
    return [('nets, linear', 'rewrites', nets('linear_list_add', lambda a, b, w: (binary(a, w), binary(b, w)), lambda t: number_of(fields(t)[0])[0])),
            ('nets, ordinary', 'rewrites', nets('ordinary_list_add', lambda a, b, w: (bool_list(bits(a, w)), bool_list(bits(b, w))), lambda t: total(bools_of(t)))),
            ('tree calculus', 'steps', tc),
            ('linear tree calculus', 'steps', ltc),
            ('linear tree calculus, reversible', 'steps', rev),
            ('tree calculus, nodes copied', 'nodes', tc_copied),
            ('nets, linear, two shapes', 'rewrites', nets('linear_list_add_two_shapes', lambda a, b, w: (arm_word(bits(a, w)), chain(bits(b, w))), lambda t: total(chain_of(fields(t)[1]))))]


def show(title, sizes, rows):
    print(f'\n{title}')
    print(f'  {"":34}' + ''.join(f'{s:>10}' for s in sizes) + f'{"each more":>11}')
    for name, unit, costs in rows:
        each = (costs[-1] - costs[-2]) / (sizes[-1] - sizes[-2])
        print(f'  {name + " (" + unit + ")":34}' + ''.join(f'{c:10.0f}' for c in costs) + f'{each:11.1f}')


def main():
    u, b = unary_rows(), binary_rows()
    show('m + m in unary, by m', UNARY, u)
    show('a + b in binary, by the number of bits', BINARY, b)
    data = {'unary': {'sizes': UNARY, 'rows': [dict(name=n, unit=un, cost=c) for n, un, c in u]},
            'binary': {'sizes': BINARY, 'rows': [dict(name=n, unit=un, cost=c) for n, un, c in b]}}
    page = os.path.join(here, 'scaling.html')
    html = open(page).read()
    start, end = '<script id="data" type="application/json">', '</script>'
    at = html.index(start) + len(start)
    open(page, 'w').write(html[:at] + json.dumps(data) + html[html.index(end, at):])
    print('\nwritten into scaling.html')


common.big_stack(main)
