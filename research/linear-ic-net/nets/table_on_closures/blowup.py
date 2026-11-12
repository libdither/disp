"""The smallest shape of the nested-table limit hits: an answer that is small as a DAG but exponential as a tree.
D y = (λa b k. k a b) y y (y is copied when D runs), applied n times to ρ. The reference shares y (Python tuples), the nets build every copy.
Rewrites and live agents for n = 2..12, in the existing net (../net.py) and the kept table on closures."""
import sys, os
HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import tnet as N
sys.path.insert(0, os.path.join(HERE, '..'))
import check as old


def main():
    for n in range(2, 13, 2):
        src = 'D := \\y. (\\a b k. k a b) y y\n' + 'D (' * n + 'ρ' + ')' * n
        t = N.parse(src)
        st, res, a = old.run_net(t, None, 10 ** 7)
        st2, res2, net = N.run(t, None, 10 ** 7)
        print(f'n {n:2}: existing net {a["interactions"]:7} rewrites ({st})   tab-run {net.stats["interactions"]:7} rewrites, '
              f'{len(net.kind):6} agents left ({st2}), same answer {res == res2}')


if __name__ == '__main__':
    N.in_thread(main)
