"""Random terms: every strategy reaches the same normal form (confluence, empirically), normal forms
are binary trees, and a run with no W step is at most (|t|-1)/3 steps long."""
import random
from rules import *

def rtree(n, rng):
    if n <= 1: return L
    k = rng.random()
    if k < 0.3: return A(L, rtree(n - 1, rng))
    m = rng.randint(1, n - 2) if n > 2 else 1
    return A(L, rtree(m, rng), rtree(max(1, n - 1 - m), rng))

def rterm(rng):
    return A(*[rtree(rng.randint(1, 7), rng) for _ in range(rng.randint(2, 5))])

def run_random(t, rules, rng, limit=3000, stats=None):
    for n in range(limit):
        ps = redexes(t)
        rng.shuffle(ps)
        for p in ps:
            r = step_at(t, p, rules, stats)
            if r is not None: t = r; break
        else:
            return t, n
    return None, limit

def is_tree(t):
    h, args = spine(t)
    return h == L and len(args) <= 2 and all(is_tree(a) for a in args)

def _main():
    for title, rules in (('TC', TC), ('LTC', LTC)):
        rng = random.Random(7)
        agree = disagree = skipped = bound_checked = 0
        for _ in range(4000):
            t = rterm(rng)
            st = {}
            nf, n = normalize(t, rules, 3000, st)
            if nf is None: skipped += 1; continue
            assert is_tree(nf)
            if title == 'LTC' and 'W' not in st:
                assert n <= (size(t) - 1) // 3, (show(t), n)
                bound_checked += 1
            for _ in range(3):
                st2 = {}
                nf2, n2 = run_random(t, rules, rng, 3000, st2)
                if nf2 is None: continue
                if nf2 == nf: agree += 1
                else: disagree += 1; print('DISAGREE', show(t), show(nf), show(nf2))
                if title == 'LTC' and 'W' not in st2:
                    assert n2 <= (size(t) - 1) // 3
        print(f'{title}: {agree} random-order runs agree with leftmost-outermost, {disagree} disagree, '
              f'{skipped} terms skipped (no normal form within 3000 steps)' +
              (f'; W-free bound held on {bound_checked} terms' if title == 'LTC' else ''))

if __name__ == '__main__':
    _main()
