"""Static checks: linearity of each rule, orthogonality (no overlaps), how many △ each rule removes."""
from rules import *
from collections import Counter

def vars_of(p, c=None):
    c = Counter() if c is None else c
    if is_var(p): c[p] += 1
    elif isinstance(p, tuple): vars_of(p[0], c); vars_of(p[1], c)
    return c

def rename(p, s):
    if is_var(p): return p + s
    if isinstance(p, tuple): return (rename(p[0], s), rename(p[1], s))
    return p

def walk(t, sub):
    while is_var(t) and t in sub: t = sub[t]
    return t

def unify(a, b, sub):
    a, b = walk(a, sub), walk(b, sub)
    if a == b: return sub
    if is_var(a): return {**sub, a: b}
    if is_var(b): return {**sub, b: a}
    if isinstance(a, tuple) and isinstance(b, tuple):
        s = unify(a[0], b[0], sub)
        return None if s is None else unify(a[1], b[1], s)
    return None

def positions(p, path=()):
    if is_var(p): return
    yield path, p
    if isinstance(p, tuple):
        yield from positions(p[0], path + (0,))
        yield from positions(p[1], path + (1,))

def _main():
    for title, rules in (('TC', TC), ('LTC', LTC)):
        print(f'== {title}: {len(rules)} rules')
        for name, lhs, rhs in rules:
            lv, rv = vars_of(lhs), vars_of(rhs)
            assert all(n == 1 for n in lv.values()), (name, 'not left-linear')
            dropped = [v for v in lv if rv[v] == 0]
            copied = [v for v in lv if rv[v] > 1]
            removed = size(lhs) - size(rhs)   # constants consumed (variables counted as 0 here)
            kind = 'copies ' + ','.join(copied) if copied else ('drops ' + ','.join(dropped) if dropped else 'linear')
            bound = removed + len(dropped) - sum(rv[v] - 1 for v in copied)
            print(f'  {name:3} {show(lhs):28} -> {show(rhs):14} {kind:12} removes >= {bound} △' + ('' if not copied else ' (less |x| per copy)'))
        # overlaps: an LHS unifying with a non-variable subterm of another (or the same, below the root) LHS
        over = []
        for n1, l1, _ in rules:
            for n2, l2, _ in rules:
                for path, sub in positions(rename(l2, '_2')):
                    if n1 == n2 and path == (): continue
                    if path and spine(sub)[0] == L and len(spine(sub)[1]) < 3 and len(spine(rename(l1,'_1'))[1]) >= 3:
                        pass  # still try: unification decides
                    if unify(rename(l1, '_1'), sub, {}) is not None:
                        over.append((n1, n2, path))
        print('  overlaps:', over if over else 'none (left-linear and non-overlapping: orthogonal)')

if __name__ == '__main__':
    _main()
