"""Run a term under a design, check it, and split its rewrites by kind."""
import sys, os
from collections import Counter
sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), '..'))
sys.setrecursionlimit(200000)
from engine import Net, DESIGNS
from ref import canon

LIVE = ('O', 'T', 'R', 'get', 'dupE', 'root', 'ph')


def reachable(n, root):
    seen, todo = set(), [root]
    while todo:
        a = todo.pop()
        if a in seen: continue
        seen.add(a)
        for i in range(n.ar[a] + 1):
            m = n.mate.get((a, i))
            if m is not None: todo.append(m[0])
    return len(seen)


def run(design, t, seed=None, limit=10 ** 8):
    D = DESIGNS[design]() if isinstance(design, str) else design
    n = Net(D, seed)
    root = n.load(t)
    status = n.run(limit)
    res = None
    if status == 'done':
        try:
            res = canon(D.readback(n, n.mate[(root, 0)]))
        except ValueError as e:
            return f'readback: {e}', None, n
        garbage = len(n.kind) - reachable(n, root)
        if garbage: status = f'garbage {garbage}'
    return status, res, n


def problems(n):
    """bookkeeping violations: annihilations across two copies, live agents copied or erased"""
    out = []
    if n.stats['annihilated across two copies']: out.append('annihilated across copies')
    for k in n.stats:
        w = k.split()
        if w[0] in ('copy', 'erase') and len(w) > 1 and w[1] in LIVE: out.append(k)
    return out


def split(stats):
    """rewrites by kind: copy (a copier meets an agent), erase, annihilate (dup ⋈ dup), cancel (an eraser
    on a copier's auxiliary port), share (sh bookkeeping, and dup ⋈ cc), expand, read (R ⋈ X and O ⋈ ρ),
    rest (calls and thaws)"""
    c = Counter()
    for k, v in stats.items():
        if k in ('interactions', 'annihilated across two copies', 'erased a fresh copy', 'expanded agents', 'sh contested'): continue
        w = k.split()
        if w[0] == 'copy': c['copy'] += v
        elif w[0] == 'erase': c['erase'] += v
        elif w[0] == 'cancel': c['cancel'] += v
        elif w[0] == 'share': c['share'] += v
        elif k == 'dup⋈dup': c['annihilate'] += v
        elif k == 'expand': c['expand'] += v
        elif k.startswith('R⋈') or k == 'O⋈rho': c['read'] += v
        else: c['rest'] += v
    assert sum(c.values()) == stats['interactions'], (c, stats['interactions'])
    return c


def reads(stats):
    return sum(v for k, v in stats.items() if k.startswith('R⋈'))
