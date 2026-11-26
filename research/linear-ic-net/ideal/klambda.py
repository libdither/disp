"""The first translation, λ-terms into the candidate net, in numbers: agents per term node, and rewrites per
step of the reference machine (a β-step, a table read, an opening).

python klambda.py
"""
import sys, os, random
HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', 'nets'))
sys.setrecursionlimit(1000000)
import kcheck, knet, kref, kscore
import check as oc


def main():
    rng = random.Random(3)
    print('compiled size, over 2000 random terms of 3 to 60 nodes:')
    for fam, full, readers in (('plain λ-terms', False, False), ('with quotes', True, False), ('with tables', True, True)):
        tot_a = tot_n = worst = 0
        for i in range(2000):
            t = kcheck.gen(rng, rng.randint(3, 60), [], full, readers)
            n = knet.KNet()
            n.emit(t, {})
            a, s = len(n.kind), kref.size(t)
            tot_a += a; tot_n += s; worst = max(worst, a / s)
        print(f'  {fam:<14} {tot_a / tot_n:.2f} agents per term node (worst {worst:.2f})')
    print('rewrites per step of the reference machine:')
    jobs = [('linear_tree_add 1014t 2413t', oc.program('linear_tree_add 1014t 2413t')),
            ('linear_list_add 5 6', oc.program('linear_list_add 5 6')),
            ('ordinary_list_add 5c 6c', oc.program('ordinary_list_add 5c 6c')),
            ('ordinary_tree_add 1014t 2413t', oc.program('ordinary_tree_add 1014t 2413t')),
            ('sizeK size', kcheck.program('sizeK size')),
            ("interpT '(1 + 1)", kcheck.program("interpT '(add (suc zero) (suc zero))")),
            ('interpT on itself', kcheck.program("interpT '(interpT '((\\x. x) (\\y. y)))")),
            ('equal suc suc', kcheck.program('equal suc suc'))]
    for name, t in jobs:
        m = kref.Machine(10 ** 7)
        m.run(t)
        st, got, stats, n = kcheck.run_net(t, None, 10 ** 8)
        steps = m.betas + m.reads + m.opens
        print(f'  {name:<30} {m.betas:>5} β {m.reads:>4} reads {m.opens:>3} openings | {stats["interactions"]:>6} rewrites,'
              f' {stats["interactions"] / steps:5.1f} a step | {stats["made"]:>6} agents made')
    C = kscore.candidate()
    rows = kscore.accounting(C)
    reads = {k: v for k, v in rows.items() if k[0] == 'C'}
    print('the five reading rules, right side:', ', '.join(f'{a}⋈{b}: {v[1]}' for (a, b), v in reads.items()))
    print('rules whose right side has no agent:', ', '.join(f'{a}⋈{b}' for (a, b), v in rows.items() if v[1] == 'no agent left'))
    print('rules that leave the value alone:', ', '.join(f'{a}⋈{b}' for (a, b), v in rows.items() if v[1] == 'the value alone'))
    print('rules that share a right side:', ', '.join(f'{a}⋈{b}' for (a, b), v in rows.items() if v[1] == 'same as another rule'))


if __name__ == '__main__':
    main()
