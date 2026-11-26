"""How big is the space of interaction systems? Exact counts at small bounds.

A system: k agent kinds, each with an arity (auxiliary ports) and, if polarized, a polarity per port; for
each unordered pair of kinds at most one rule; a rule's right-hand side is a list of made agents (at most M)
and a perfect matching of the free ends (the pair's auxiliary ports and every port of every made agent).

Counts are of labeled right-hand sides (made agents in a fixed order, no quotient by renaming), so they
are upper bounds on the number of distinct rules; the quotient is at most M! smaller per rule.
usage: python count.py
"""
import itertools, math
from functools import lru_cache


def dfact(n):
    """(n-1)!!: perfect matchings of n ends, 0 if n is odd"""
    if n % 2: return 0
    r = 1
    for i in range(n - 1, 0, -2): r *= i
    return r


def multisets(k, m):
    """made-agent lists as multisets of kind indices, size 0..m"""
    for size in range(m + 1):
        yield from itertools.combinations_with_replacement(range(k), size)


def mult_orders(ms):
    """how many ordered lists give this multiset"""
    c = math.factorial(len(ms))
    for _, g in itertools.groupby(ms): c //= math.factorial(len(list(g)))
    return c


def rhs_count_unpol(ar, a, b, M, ordered=True):
    """number of right-hand sides for pair (a, b): sum over made lists of matchings"""
    tot = 0
    for ms in multisets(len(ar), M):
        ends = ar[a] + ar[b] + sum(ar[x] + 1 for x in ms)
        tot += dfact(ends) * (mult_orders(ms) if ordered else 1)
    return tot


def rhs_count_pol(pol, a, b, M, ordered=True):
    """pol[x] = tuple of port polarities (principal first), 1 = out. Slot ends take the flipped polarity
    of the pair's port (the made port stands in for it), so every wire joins an out end to an in end."""
    tot = 0
    for ms in multisets(len(pol), M):
        outs = ins = 0
        for x in (a, b):
            for p in pol[x][1:]:
                if p: ins += 1        # an out aux port of the pair: its outside neighbour is an in end
                else: outs += 1
        for x in ms:
            for p in pol[x]:
                if p: outs += 1
                else: ins += 1
        if outs == ins: tot += math.factorial(outs) * (mult_orders(ms) if ordered else 1)
    return tot


def system_count_unpol(ar, M):
    """systems over a fixed signature: each pair has no rule or one of its right-hand sides"""
    k, tot = len(ar), 1
    for a in range(k):
        for b in range(a, k):
            tot *= 1 + rhs_count_unpol(ar, a, b, M)
    return tot


def system_count_pol(pol, M):
    k, tot = len(pol), 1
    for a in range(k):
        for b in range(a, k):
            if pol[a][0] != pol[b][0]: tot *= 1 + rhs_count_pol(pol, a, b, M)
    return tot


def lg(n): return math.log10(n) if n > 0 else float('-inf')


def table1():
    print('Unpolarized systems, all signatures with k kinds of aux arity <= A, right-hand sides <= M agents')
    print(f'{"k":>2} {"A":>2} {"M":>2} {"signatures":>10} {"log10 systems":>14} {"log10 rules (one pair, max)":>28}')
    for k in (1, 2, 3, 4):
        for A in (2, 3):
            for M in (2, 3, 4):
                if k == 4 and M == 4: continue
                tot, best, nsig = 0, 0, 0
                for ar in itertools.combinations_with_replacement(range(A + 1), k):   # up to kind renaming
                    nsig += 1
                    tot += system_count_unpol(list(ar), M)
                    best = max(best, max(rhs_count_unpol(list(ar), a, b, M) for a in range(k) for b in range(a, k)))
                print(f'{k:>2} {A:>2} {M:>2} {nsig:>10} {lg(tot):>14.1f} {lg(best):>28.1f}')


def table2():
    print('\nThe interaction-combinator signature (eps:0, gamma:2, delta:2), unpolarized')
    ar = [0, 2, 2]
    names = ['eps', 'gam', 'del']
    for M in (2, 4):
        print(f' M={M}: log10 systems = {lg(system_count_unpol(ar, M)):.1f}')
        for a in range(3):
            for b in range(a, 3):
                print(f'   {names[a]}x{names[b]}: {rhs_count_unpol(ar, a, b, M):,} right-hand sides')


def table3():
    """The reflective-nets signature (lean/Tables.lean), polarized: per-pair counts with <= M made agents."""
    import json, os
    here = os.path.dirname(os.path.abspath(__file__))
    kinds, rules, stuck = [], [], []
    for line in open(os.path.join(here, 'old_table.jsonl')):
        d = json.loads(line)
        if 'kind' in d: kinds.append(d)
        elif 'stuck' in d: stuck.append(d['stuck'])
        else: rules.append(d)
    idx = {d['kind']: i for i, d in enumerate(kinds)}
    pol = [tuple(1 if p == 'o' else 0 for p in d['ports']) for d in kinds]
    print('\nThe reflective-nets signature (13 kinds, polarized), right-hand sides per pair')
    for M in (1, 2, 3):
        per = []
        for a in range(13):
            for b in range(a, 13):
                if pol[a][0] != pol[b][0]: per.append(rhs_count_pol(pol, a, b, M))
        prod = 1
        for n in per: prod *= 1 + n
        print(f' M={M}: {len(per)} pairs that can meet; per-pair right-hand sides: min {min(per):,}, '
              f'max {max(per):,}, sum {sum(per):,}; whole tables: 10^{lg(prod):.0f}')
    sizes = sorted(len(r['made']) for r in rules)
    print(f' the table itself: made agents per rule {sizes}')


if __name__ == '__main__':
    table1(); table2(); table3()
