"""Which small systems built only from the two generic rule shapes (annihilate, commute) survive the necessary
conditions for universality that Lafont lists for his combinators (section 2.8 of "Interaction Combinators"),
and where do the interaction combinators rank among them under the default-plus-exceptions code of score.py?

Space: k kinds (k <= 3), aux arity <= A; each pair of kinds has no rule, ANN with any wiring permutation (an
involution when the two kinds are equal, so the rule reads the same from either side), or COM.
Necessary conditions (Lafont's remarks, read as general ones):
  grow   some rule makes more agents than it consumes (else no net can grow)
  grow+  stronger: some rule makes more agents of positive arity (erasers alone growing does not count)
  empty  some rule's right side is empty: no agents, no wires
  wire   some rule's right side is a nonempty pure wiring
  total  every pair has a rule (not necessary for universality; Lafont drops del-eps and stays universal)
usage: python minimal.py
"""
import itertools, math, sys
from score import System, default_bits, sig_bits, lg


def ann(ar, a, b, perm):
    return (a, b, [], [[['L', i + 1], ['R', perm[i] + 1]] for i in range(ar[a])])


def com(ar, a, b):
    p, q = ar[a], ar[b]
    made = [a] * q + [b] * p
    w = [[['R', j + 1], ['N', j, 0]] for j in range(q)] + [[['L', i + 1], ['N', q + i, 0]] for i in range(p)]
    w += [[['N', j, i + 1], ['N', q + i, j + 1]] for i in range(p) for j in range(q)]
    return (a, b, made, w)


def options(ar, a, b):
    yield ('none', None)
    if ar[a] or ar[b]: yield ('COM', com(ar, a, b))     # for two 0-ary kinds COM and ANN are the same empty rule
    if ar[a] == ar[b]:
        for perm in itertools.permutations(range(ar[a])):
            if a == b and any(perm[perm[i]] != i for i in range(len(perm))): continue
            yield (f'ANN{"" if list(perm) == sorted(perm) else "x"}', ann(ar, a, b, perm))


def conditions(ar, rules):
    grow = any(len(r[2]) > 2 for r in rules)
    pos = lambda r: sum(1 for m in r[2] if ar[m] > 0) > sum(1 for x in r[:2] if ar[x] > 0)
    grow2 = any(pos(r) for r in rules)
    empty = any(not r[2] and not r[3] for r in rules)
    wire = any(not r[2] and r[3] for r in rules)
    return grow, grow2, empty, wire


def main():
    rows = []
    total_sys = 0
    for k, A in ((1, 3), (2, 3), (3, 2)):
        for arities in itertools.combinations_with_replacement(range(A + 1), k):
            names = [f'{chr(97 + i)}{arities[i]}' for i in range(k)]
            ar = dict(zip(names, arities))
            pairs = [(names[i], names[j]) for i in range(k) for j in range(i, k)]
            for choice in itertools.product(*[list(options(ar, a, b)) for a, b in pairs]):
                total_sys += 1
                rules = [r for _, r in choice if r is not None]
                if not rules: continue
                g, g2, e, w = conditions(ar, rules)
                if not (g and e and w): continue
                S = System('', ar, False, rules)
                bits = sig_bits(S) + default_bits(S)
                tot = all(r is not None for _, r in choice)
                desc = ' '.join(f'{a}{b}:{c}' for (a, b), (c, _) in zip(pairs, choice))
                rows.append((bits, g2, tot, desc, k))
    rows.sort()
    print(f'{total_sys} schema-only systems; {len(rows)} pass grow+empty+wire; '
          f'{sum(1 for r in rows if r[1])} also grow+; {sum(1 for r in rows if r[1] and r[2])} also total')
    print('cheapest that pass grow+empty+wire+grow+ (bits, total?, rules):')
    for kk in (1, 2, 3):
        print(f'  with {kk} kinds: {sum(1 for r in rows if r[4] == kk and r[1])} survive grow+empty+wire+grow+')
    for bits, g2, tot, desc, _ in [r for r in rows if r[1]][:12]:
        print(f'  {bits:6.1f} {"total" if tot else "     "} {desc}')
    ic = [r for r in rows if r[3] == 'a0a0:ANN a0b2:COM a0c2:COM b2b2:ANN b2c2:COM c2c2:ANN']
    print('interaction combinators:', ic, ' rank among grow+ survivors:',
          1 + sum(1 for r in rows if r[1] and r[0] < ic[0][0]) if ic else None,
          ' ties:', sum(1 for r in rows if r[1] and abs(r[0] - ic[0][0]) < 1e-9) if ic else None)


if __name__ == '__main__':
    main()
