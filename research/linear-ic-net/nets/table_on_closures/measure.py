"""Rewrites and agents allocated per read, split copy / erase / annihilate / layout / read / rest, for the kept
table on closures, next to RefRun running the λ-view programs; then linear readers (keep and hand).
Queue order, plus a check that seed 1 fires the same number of rewrites.
usage: python measure.py"""
import sys, os, time
HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import tnet as N
import tprograms as P
from engine import DESIGNS

Q11 = '((^d. add (suc zero) (suc zero)) ρ)'
ROWS = [
    ('size of suc', [('ref-run', 'size suc'), ('tab', 'tsize suc'), ('tab', 'tsizeK suc')]),
    ('size of λ-view size (closure)', [('ref-run', 'size size'), ('tab', 'tsize size'), ('tab', 'tsizeK size')]),
    ('size of own code (quoted)', [('ref-run', 'size ((^d. size) ρ)'), ('tab', 'tsize ((^d. tsize) ρ)'), ('tab', 'tsizeK ((^d. tsizeK) ρ)')]),
    ('size of own evaluated value', [('ref-run', 'size size'), ('tab', 'tsize tsize'), ('tab', 'tsizeK tsizeK')]),
    ('interp of quote add 1 1', [('ref-run', 'interp ' + Q11), ('tab', 'tinterp ' + Q11)]),
]


def one(design, src, seed=None):
    D = DESIGNS['ref-run']() if design == 'ref-run' else N.TabRun()
    t = N.parse(P.source(src))
    st, res, n = N.run(t, seed, 60000000, D)
    assert st == 'done' and not N.problems(n), (src, st, N.problems(n))
    return res, n


def main():
    t0 = time.time()
    print(f"{'workload':32} {'design':8} {'program':24} {'reads':>5} {'/read':>6} " + ' '.join(f'{k[:6]:>6}' for k in N.GROUPS)
          + f" {'agents':>7} {'arm cp':>6}")
    for label, cells in ROWS:
        for design, src in cells:
            res, n = one(design, src)
            _, n2 = one(design, src, 1)
            r = N.nreads(n.stats)
            c = N.split(n.stats)
            prog = src.split(' ')[0]
            armcopy = sum(v for k, v in n.stats.items() if k in ('copy clo by dup', 'copy rho by dup', 'copy cloL by dup'))
            print(f"{label:32} {design:8} {prog:24} {r:5} {n.stats['interactions'] / r:6.1f} " + ' '.join(f'{c[k] / r:6.1f}' for k in N.GROUPS)
                  + f" {n.alloc / r:7.1f} {armcopy / r:6.1f}"
                  + ('' if n.stats['interactions'] == n2.stats['interactions'] else f"  (seed 1: {n2.stats['interactions']})"))
            sys.stdout.flush()
    print(f'({time.time() - t0:.0f}s)')
    print()
    print('## linear readers: what the read mechanism itself copies and erases (whole run)')
    LIN = {
        'keep': r"keep[\t F k. k t F | \t L k. k t L | \t g b k. k t g b | \t w k. k t w | \t q k. k t q | \t a1 a2 a3 a4 a5 a6 k. k t a1 a2 a3 a4 a5 a6] X (\t F k. k t F)",
        'hand': r"keep[\o1 o2 o3 o4 o5 F k. k o1 o2 o3 o4 o5 F | \o1 o2 o3 o4 o5 L k. k o1 o2 o3 o4 o5 L | \o1 o2 o3 o4 o5 g b k. k o1 o2 o3 o4 o5 g b | \o1 o2 o3 o4 o5 w k. k o1 o2 o3 o4 o5 w | \o1 o2 o3 o4 o5 q k. k o1 o2 o3 o4 o5 q | \o1 o2 o3 o4 o5 a1 a2 a3 a4 a5 a6 k. k o1 o2 o3 o4 o5 a1 a2 a3 a4 a5 a6] X (\a b c d e F k. k a b c d e F)",
    }
    for pol, src in LIN.items():
        for x in ('(\\x. x)', '(\\x y. y x)'):
            t = N.parse(src.replace('X', x))
            st, res, n = N.run(t, None, 10 ** 6, N.TabRun(pol))
            c = N.split(n.stats)
            copies = {k: v for k, v in n.stats.items() if k.startswith('copy')}
            print(f"  {pol:5} reads {x:12} {st}: {n.stats['interactions']:3} rewrites, {n.alloc} agents, copy {c['copy']} {copies}, "
                  f"annihilate {c['annihilate']}, erase {c['erase']}, layout {c['layout']}")


if __name__ == '__main__':
    N.in_thread(main)
