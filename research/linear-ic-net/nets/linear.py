"""The adds in the nets, held to plain arithmetic: linear ones, which erase nothing and copy only what the
program asks for, on unary numbers, lists of bits and trees of bits; reversible ones, which also hand
the first number back and have a subtract that undoes them; and the ordinary ones beside them.

The programs are in check.PROGRAMS (lzero … reversible_tree_sub_parallel). Here they are run on many inputs
and held to plain arithmetic, to the reference interpreter, and to what "linear" means:
  - the compiled program has no eraser, and no run fires an erase rule;
  - copies start only where a variable is used twice: one per successor for linear_unary_add, five per bit
    for linear_list_add (the step, the pair that turns b into a chain, and three bits), and for
    linear_tree_add three per leaf (the bits) and two per level less one (the step of a's loop and the adder
    each doubling copies), and for linear_tree_add_parallel eight per leaf less six and two per level;
  - the number of rewrites depends on the size of the input, not on its bits.
Two adds are also run in a row, the second on the first one's sum, and a cheaper adder whose two numbers
have different shapes. The ordinary versions (a fixpoint, arms that drop what they don't use) are run
beside them for the cost. What "reversible" means is checked too: the add and the subtract each undo the
other, and every pair of numbers gives its own answer, where a linear add gives one answer for many
pairs. Every number is written out as a term, so no run pays for building its input."""
import sys, random
from collections import Counter
from net import Net, readback
from ref import (Machine, canon, parse, unary, binary, tree, scott, bool_list, arm_word, chain, bits, number_of, tree_of,
                 bit_of, binders, spine, BIT)
from check import PROGRAMS, reachable

sys.setrecursionlimit(1000000)

DEFS = PROGRAMS

# ---- reading answers ----
def fields(t):
    """the parts of a tuple λk. k a b …"""
    (k,), body = binders(t, 1)
    head, args = spine(body)
    assert head == k
    return args

def scott_of(t):
    n = 0
    while True:
        (z, s), body = binders(t, 2)
        if body == z: return n
        assert body[0] == s
        n, t = n + 1, body[1]

def bools_of(t):
    out = []
    while True:
        (n, c), body = binders(t, 2)
        if body == n: return out
        (_, x), t = body
        (a, b), picked = binders(x, 2)
        out.append(1 if picked == a else 0)

def arm_word_of(t):
    out = []
    while True:
        (o, i, n), body = binders(t, 3)
        if body == ((n, o), i): return out
        out.append(1 if body[0] == i else 0)
        t = spine(body[1])[0]

def chain_of(t):
    out = []
    while True:
        (k,), body = binders(t, 1)
        if body == k: return out
        (_, x), t = body
        out.append(bit_of(x))

def total(bs): return sum(b << i for i, b in enumerate(bs))


def run(fn, *args, seed=None, limit=40000000, width=None, spans=False):
    """the answer of the function (as source) applied to numbers (as terms), and what the run cost. With
    spans, also how the run went in rounds of at most `width` rewrites at once (any number, if None):
    the rounds, the widest round, the most agents there were, and the agents at the start."""
    t = parse(DEFS + '\n' + fn)
    for a in args: t = (t, a)
    n = Net(seed)
    root = n.load(t)
    erasers, start = sum(1 for k in n.kind.values() if k == 'era'), n.size()
    assert n.run(limit, width) == 'done', fn
    got = canon(readback(n, n.mate[(root, 0)]))
    assert len(n.kind) == reachable(n, root), 'agents left over'
    assert not n.stats['annihilated across two copies']
    cost = Counter(erasers=erasers, copies_started=next(n.ops))
    for k, v in n.stats.items():
        cost['rewrites' if k == 'interactions' else 'copy' if k.startswith('copy') else 'cancel' if k == 'dup⋈dup' else
             'erase' if k.startswith('erase') else 'call' if k.startswith('O⋈') else 'thaw'] += v
    return (got, cost, dict(rounds=n.rounds, widest=n.widest, peak=n.peak, start=start)) if spans else (got, cost)


def reference(fn, *args):
    t = parse(DEFS + '\n' + fn)
    for a in args: t = (t, a)
    return canon(Machine(10000000).run(t))


def linear(cost, copies):
    return cost['erasers'] == 0 and cost['erase'] == 0 and cost['copies_started'] == copies


def unary_adds():
    """linear_unary_add m n against m + n, for every m, n up to 12 and a few larger"""
    cases = [(m, n) for m in range(13) for n in range(13)] + [(40, 3), (3, 40), (64, 64), (500, 500)]
    for m, n in cases:
        got, cost = run('linear_unary_add', unary(m), unary(n))
        assert number_of(fields(got)[0]) == (m + n, 'u'), f'linear_unary_add {m} {n}: wrong sum'
        assert linear(cost, m), f'linear_unary_add {m} {n}: not linear: {dict(cost)}'
        if m <= 64:
            assert got == reference('linear_unary_add', unary(m), unary(n)), f'linear_unary_add {m} {n}: differs from the reference'
            for seed in (1, 2):
                assert run('linear_unary_add', unary(m), unary(n), seed=seed) == (got, cost), f'linear_unary_add {m} {n}: order {seed} differs'
        assert scott_of(run('ordinary_unary_add', scott(m), scott(n))[0]) == m + n, f'ordinary_unary_add {m} {n}: wrong sum'
    return len(cases)


def words(rng, w):
    """every pair of w-bit numbers for a small w, a dozen random ones and three edges above"""
    if w <= 3: return [(a, b) for a in range(1 << w) for b in range(1 << w)]
    top = (1 << w) - 1
    return [(rng.randrange(1 << w), rng.randrange(1 << w)) for _ in range(12)] + [(top, 1), (top, top), (0, 0)]


def binary_adds(rng):
    """linear_list_add a b against a + b on numbers of 1 to 64 bits"""
    count = 0
    for w in (1, 2, 3, 4, 8, 16, 32, 64):
        rewrites = set()
        for a, b in words(rng, w):
            got, cost = run('linear_list_add', binary(a, w), binary(b, w))
            s, c = fields(got)[:2]
            assert number_of(s) == ((a + b) % (1 << w), w) and bit_of(c) == (a + b) >> w, f'linear_list_add {a} {b} in {w} bits: wrong sum'
            assert linear(cost, 5 * w), f'linear_list_add {a} {b} in {w} bits: not linear: {dict(cost)}'
            rewrites.add(cost['rewrites'])
            if w <= 4:
                assert got == reference('linear_list_add', binary(a, w), binary(b, w)), f'linear_list_add {a} {b}: differs from the reference'
                assert run('linear_list_add', binary(a, w), binary(b, w), seed=1) == (got, cost), f'linear_list_add {a} {b}: another order differs'
            got, _ = run('ordinary_list_add', bool_list(bits(a, w)), bool_list(bits(b, w)))
            assert total(bools_of(got)) == (a + b) % (1 << w), f'ordinary_list_add {a} {b}: wrong sum'
            count += 1
        assert len(rewrites) == 1, f'{w} bits: the cost depends on the bits: {rewrites}'
    return count


def twice(rng):
    """a sum is a number like any other: a + b, then a + that; the program copies a to use it twice"""
    count = 0
    for w in (1, 2, 3, 8):
        for a, b in words(rng, w)[:40]:
            got, cost = run('(\\a b. linear_list_add a b (\\s c e f. linear_list_add a s (\\s2 c2 e2 f2 k. k s2 c c2 e f e2 f2)))', binary(a, w), binary(b, w))
            assert number_of(fields(got)[0]) == ((2 * a + b) % (1 << w), w), f'two adds, {a} {b} in {w} bits: wrong'
            assert linear(cost, 10 * w + 1), f'two adds, {a} {b} in {w} bits: not linear: {dict(cost)}'
            count += 1
    return count


def two_shapes(rng):
    """the cheaper adder, whose numbers have different shapes"""
    count = 0
    for w in (1, 2, 3, 16):
        for a, b in words(rng, w):
            got, cost = run('linear_list_add_two_shapes', arm_word(bits(a, w)), chain(bits(b, w)))
            a2, s, c = fields(got)[:3]
            assert (total(arm_word_of(a2)), total(chain_of(s)), bit_of(c)) == (a, (a + b) % (1 << w), (a + b) >> w), f'linear_list_add_two_shapes {a} {b}: wrong'
            assert linear(cost, 2 * w), f'linear_list_add_two_shapes {a} {b} in {w} bits: not linear: {dict(cost)}'
            count += 1
    return count


def full_adder():
    for x in (0, 1):
        for y in (0, 1):
            for c in (0, 1):
                got, cost = run('(\\x y c. add3 x y c (\\s cn k. k s cn))', BIT[x], BIT[y], BIT[c])
                assert [bit_of(f) for f in fields(got)] == [(x + y + c) & 1, (x + y + c) >> 1], f'add3 {x} {y} {c}: wrong'
                assert linear(cost, 3), f'add3 {x} {y} {c}: not linear: {dict(cost)}'
                got, _ = run('oadd3', BIT[x], BIT[y], BIT[c])
                assert [bit_of(f) for f in fields(got)] == [(x + y + c) & 1, (x + y + c) >> 1], f'oadd3 {x} {y} {c}: wrong'
    return 8


def tree_adds(rng):
    """linear_tree_add a b against a + b on trees of 2 to 64 leaves, the ordinary ordinary_tree_add beside it, and two adds in a
    row: a + (a + b), where the program copies a to use it twice"""
    count = 0
    for depth in (1, 2, 3, 4, 5, 6):
        w, rewrites = 1 << depth, set()
        for a, b in words(rng, w):
            got, cost = run('linear_tree_add', tree(a, depth), tree(b, depth))
            s, c = fields(got)
            assert number_of(s) == ((a + b) % (1 << w), 't') and tree_of(fields(s)[0])[1] == depth and bit_of(c) == (a + b) >> w, \
                f'linear_tree_add {a} {b} in {w} leaves: wrong sum'
            assert linear(cost, 3 * w + 2 * depth - 1), f'linear_tree_add {a} {b} in {w} leaves: not linear: {dict(cost)}'
            rewrites.add(cost['rewrites'])
            if w <= 4:
                assert got == reference('linear_tree_add', tree(a, depth), tree(b, depth)), f'linear_tree_add {a} {b}: differs from the reference'
                assert run('linear_tree_add', tree(a, depth), tree(b, depth), seed=1) == (got, cost), f'linear_tree_add {a} {b}: another order differs'
            other, _ = run('ordinary_tree_add', tree(a, depth), tree(b, depth))
            assert other == got, f'ordinary_tree_add {a} {b} in {w} leaves: not the answer of linear_tree_add'
            got, cost = run('(\\a b. linear_tree_add a b (\\s c. linear_tree_add a s (\\s2 c2 k. k s2 c c2)))', tree(a, depth), tree(b, depth))
            assert number_of(fields(got)[0]) == ((2 * a + b) % (1 << w), 't'), f'two tree adds, {a} {b} in {w} leaves: wrong'
            assert linear(cost, 2 * (3 * w + 2 * depth - 1) + 1), f'two tree adds, {a} {b} in {w} leaves: not linear: {dict(cost)}'
            count += 1
        assert len(rewrites) == 1, f'{w} leaves: the cost depends on the bits: {rewrites}'
    return count


def parallel_adds(rng):
    """linear_tree_add_parallel a b against a + b on trees of 2 to 64 leaves, the ordinary ordinary_tree_add_parallel beside it. The rounds grow by a
    fixed number for each level, where linear_tree_add's double with the leaves."""
    count, rounds = 0, {}
    for depth in (1, 2, 3, 4, 5, 6):
        w, rewrites = 1 << depth, set()
        for a, b in words(rng, w):
            got, cost, span = run('linear_tree_add_parallel', tree(a, depth), tree(b, depth), spans=True)
            s, c = fields(got)
            assert number_of(s) == ((a + b) % (1 << w), 't') and tree_of(fields(s)[0])[1] == depth and bit_of(c) == (a + b) >> w, \
                f'linear_tree_add_parallel {a} {b} in {w} leaves: wrong sum'
            assert linear(cost, 8 * w + 2 * depth - 6), f'linear_tree_add_parallel {a} {b} in {w} leaves: not linear: {dict(cost)}'
            rewrites.add(cost['rewrites'])
            rounds.setdefault(depth, set()).add(span['rounds'])
            assert got == run('linear_tree_add', tree(a, depth), tree(b, depth))[0], f'linear_tree_add_parallel {a} {b} in {w} leaves: not the answer of linear_tree_add'
            if w <= 4:
                assert got == reference('linear_tree_add_parallel', tree(a, depth), tree(b, depth)), f'linear_tree_add_parallel {a} {b}: differs from the reference'
                assert run('linear_tree_add_parallel', tree(a, depth), tree(b, depth), seed=1) == (got, cost), f'linear_tree_add_parallel {a} {b}: another order differs'
            assert run('ordinary_tree_add_parallel', tree(a, depth), tree(b, depth))[0] == got, f'ordinary_tree_add_parallel {a} {b} in {w} leaves: not the answer of linear_tree_add_parallel'
            count += 1
        assert len(rewrites) == 1, f'{w} leaves: the cost depends on the bits: {rewrites}'
    assert all(len(r) == 1 for r in rounds.values()), f'the rounds depend on the bits: {rounds}'
    steps = {max(rounds[d + 1]) - max(rounds[d]) for d in (4, 5)}
    assert len(steps) == 1, f'the rounds do not grow by a fixed number for each level: {rounds}'
    return count, steps.pop()


def reversible_adds(rng):
    """reversible_tree_add a b against (a, a + b) and reversible_tree_sub a s against (a, s - a), in as many bits
    as a tree has leaves, one bit after another and in parallel. Each undoes the other. On trees of four
    leaves every pair of numbers is run: each gives its own answer, where linear_tree_add gives 31 answers
    for the 256 pairs. The subtract copies one bit less at each leaf: it reads a's bit with a swap, which
    hands the bit back, where the add copies it."""
    both = lambda got: tuple(number_of(f)[0] for f in fields(got))
    count = 0
    for add, sub, copies in (('reversible_tree_add', 'reversible_tree_sub', lambda w, d: (3 * w + 2 * d, 2 * w + 2 * d)),
                             ('reversible_tree_add_parallel', 'reversible_tree_sub_parallel', lambda w, d: (8 * w + 2 * d - 5, 7 * w + 2 * d - 5))):
        for depth in (1, 2, 3, 4, 5, 6):
            w = 1 << depth
            rewrites, answers = {add: set(), sub: set()}, {add: set(), sub: set()}
            pairs = [(a, b) for a in range(16) for b in range(16)] if depth == 2 else words(rng, w)
            for i, (a, b) in enumerate(pairs):
                for fn, want, n in ((add, (a + b) % (1 << w), copies(w, depth)[0]), (sub, (b - a) % (1 << w), copies(w, depth)[1])):
                    got, cost = run(fn, tree(a, depth), tree(b, depth))
                    assert both(got) == (a, want) and all(tree_of(fields(f)[0])[1] == depth for f in fields(got)), f'{fn} {a} {b} in {w} leaves: wrong'
                    assert linear(cost, n), f'{fn} {a} {b} in {w} leaves: not linear: {dict(cost)}'
                    rewrites[fn].add(cost['rewrites']); answers[fn].add(both(got))
                    if w <= 4 and i < 16:
                        assert got == reference(fn, tree(a, depth), tree(b, depth)), f'{fn} {a} {b}: differs from the reference'
                        assert run(fn, tree(a, depth), tree(b, depth), seed=1) == (got, cost), f'{fn} {a} {b}: another order differs'
                for first, then in ((add, sub), (sub, add)):
                    got, cost = run(f'(\\a b. {first} a b {then})', tree(a, depth), tree(b, depth))
                    assert both(got) == (a, b), f'{first} then {then}, {a} {b} in {w} leaves: not the two numbers again'
                    assert linear(cost, sum(copies(w, depth))), f'{first} then {then}, {a} {b} in {w} leaves: not linear: {dict(cost)}'
                count += 1
            assert all(len(r) == 1 for r in rewrites.values()), f'{w} leaves: the cost depends on the bits: {rewrites}'
            if depth == 2:
                assert len(answers[add]) == len(answers[sub]) == 256, f'{add}, {sub}: two pairs of numbers give one answer'
                sums = {run('linear_tree_add', tree(a, depth), tree(b, depth))[0] for a, b in pairs}
                assert len(sums) == 31, f'linear_tree_add gives {len(sums)} answers for the 256 pairs'
    return count


def written():
    """a number written in a program is the term its builder makes: the same answer for the same cost"""
    cases = [('linear_unary_add 2u 3u', 'linear_unary_add', unary(2), unary(3)),
             ('ordinary_unary_add 2s 3s', 'ordinary_unary_add', scott(2), scott(3)),
             ('linear_list_add 5 6', 'linear_list_add', binary(5, 4), binary(6, 4)),
             ('ordinary_list_add 5c 6c', 'ordinary_list_add', bool_list(bits(5, 4)), bool_list(bits(6, 4))),
             ('linear_list_add_two_shapes 5a 6p', 'linear_list_add_two_shapes', arm_word(bits(5, 4)), chain(bits(6, 4))),
             ('linear_tree_add 5t 6t', 'linear_tree_add', tree(5, 2), tree(6, 2))]
    for src, fn, a, b in cases:
        assert run(src) == run(fn, a, b), f'{src}: not as the numbers built by hand'
    assert scott_of(run('ordinary_unary_add 2s 3s')[0]) == 5 and total(bools_of(run('ordinary_list_add 5c 6c')[0])) == 11
    a, s, c = fields(run('linear_list_add_two_shapes 5a 6p')[0])[:3]
    assert (total(arm_word_of(a)), total(chain_of(s)), bit_of(c)) == (5, 11, 0)
    return len(cases)


KINDS = ('rewrites', 'copy', 'cancel', 'erase', 'call', 'thaw')

def per(lo, hi, cost_at):
    """what one more unit costs: the difference between sizes hi and lo, per unit"""
    a, b = cost_at(lo), cost_at(hi)
    return {k: (b[k] - a[k]) / (hi - lo) for k in KINDS}


def costs(rng):
    a, b = rng.randrange(1 << 32), rng.randrange(1 << 32)
    low = lambda w: (a % (1 << w), b % (1 << w))
    return [
        ('linear unary add', per(16, 32, lambda m: run('linear_unary_add', unary(m), unary(3))[1])),
        ('ordinary unary add', per(16, 32, lambda m: run('ordinary_unary_add', scott(m), scott(3))[1])),
        ('linear list add', per(16, 32, lambda w: run('linear_list_add', *[binary(v, w) for v in low(w)])[1])),
        ('linear list add, two shapes', per(16, 32, lambda w: run('linear_list_add_two_shapes', arm_word(bits(low(w)[0], w)), chain(bits(low(w)[1], w)))[1])),
        ('ordinary list add', per(16, 32, lambda w: run('ordinary_list_add', *[bool_list(bits(v, w)) for v in low(w)])[1])),
        ('linear tree add', per(16, 32, lambda w: run('linear_tree_add', *[tree(v, w.bit_length() - 1) for v in low(w)])[1])),
        ('ordinary tree add', per(16, 32, lambda w: run('ordinary_tree_add', *[tree(v, w.bit_length() - 1) for v in low(w)])[1])),
        ('linear parallel tree add', per(16, 32, lambda w: run('linear_tree_add_parallel', *[tree(v, w.bit_length() - 1) for v in low(w)])[1])),
        ('ordinary parallel tree add', per(16, 32, lambda w: run('ordinary_tree_add_parallel', *[tree(v, w.bit_length() - 1) for v in low(w)])[1])),
        ('reversible tree add', per(16, 32, lambda w: run('reversible_tree_add', *[tree(v, w.bit_length() - 1) for v in low(w)])[1])),
        ('reversible tree subtract', per(16, 32, lambda w: run('reversible_tree_sub', *[tree(v, w.bit_length() - 1) for v in low(w)])[1])),
        ('reversible parallel tree add', per(16, 32, lambda w: run('reversible_tree_add_parallel', *[tree(v, w.bit_length() - 1) for v in low(w)])[1])),
        ('reversible parallel tree subtract', per(16, 32, lambda w: run('reversible_tree_sub_parallel', *[tree(v, w.bit_length() - 1) for v in low(w)])[1])),
    ]


def main():
    rng = random.Random(4)
    print(f'unary: {unary_adds()} sums right, as the reference, the same in three orders; no eraser, one copy per successor')
    print(f'lists: {binary_adds(rng)} sums right; no eraser, five copies per bit, the same number of rewrites whatever the bits')
    print(f'two adds in a row: {twice(rng)} right; the sum of one is the number the next takes, and still no eraser')
    print(f'two shapes: {two_shapes(rng)} sums right; no eraser, two copies per bit')
    print(f'three bits added: {full_adder()} of 8 inputs right; no eraser, three bits copied, no arm picked')
    print(f'trees: {tree_adds(rng)} sums right, and two adds in a row; no eraser, three copies per leaf and two per level less one, the same number of rewrites whatever the bits; the ordinary adder gives the same answers')
    count, step = parallel_adds(rng)
    print(f'parallel trees: {count} sums right, as linear_tree_add and ordinary_tree_add_parallel give them; no eraser, eight copies per leaf less six and two per level, the same number of rewrites whatever the bits; {step} more rounds for each level')
    print(f'reversible trees: {reversible_adds(rng)} pairs of numbers, added and subtracted, one bit after another and in parallel: right, each undoes the other, and every pair of numbers gives its own answer; no eraser, and the first number copied once')
    print(f'written numbers: {written()} programs give the same answer for the same cost as with numbers built by hand')
    print('\nrewrites for one more unit (unary) or one more bit (lists and trees, random numbers):')
    print(f'  {"":34} {"rewrites":>9} {"copy":>7} {"cancel":>7} {"erase":>7} {"call":>6} {"thaw":>6}')
    for name, c in costs(rng):
        print(f'  {name:34} {c["rewrites"]:9.1f} {c["copy"]:7.1f} {c["cancel"]:7.1f} {c["erase"]:7.1f} {c["call"]:6.1f} {c["thaw"]:6.1f}')


if __name__ == '__main__':
    main()
