"""Eager runs (as disp runs) of five recursive programs on TC, LTC (first design), LTC2 and LTC2P,
with a generic evaluator driven by the rule tables: rewrites, copies, nodes copied, nodes dropped."""
import threading
from collections import Counter
from rules import *
from check_rules import vars_of
from compile import compile_, br_tc, br_ltc, WAIT
from ltc2 import LTC2, LTC2P, compile2, wait2, fire2
from lower import lower
from bench import programs, nat, tree, lst

def make_run(rules):
    mult = {name: (vars_of(lhs), vars_of(rhs)) for name, lhs, rhs in rules}
    def run(t):
        st = Counter()
        def apply(f, x):
            h, args = spine(f)
            if len(args) < 2: return A(f, x)
            r = fire2([args[0], args[1], x], rules)
            _, rhs, name, env = r
            st[name] += 1; st['steps'] += 1
            lv, rv = mult[name]
            for v in lv:
                if rv[v] == 0: st['dropped'] += size(env[v])
                if rv[v] > 1: st['copied'] += (rv[v] - 1) * size(env[v]); st['copies'] += 1
            return ev(rhs)
        def ev(t):
            if t == L: return L
            return apply(ev(t[0]), ev(t[1]))
        return ev(t), st
    return run

def build(calc):
    if calc in ('tc', 'ltc'):
        br = br_tc if calc == 'tc' else br_ltc
        return [compile_(p, br) for p in programs(calc)]
    WAIT['ltc2'] = wait2
    return [compile2(lower(p, calc == 'ltc2p')) for p in programs('ltc2')]

def main():
    calcs = [('tc', TC), ('ltc', LTC), ('ltc2', LTC2), ('ltc2p', LTC2P)]
    print(f"{'call':7} " + ' '.join(f'{c:>34}' for c, _ in calcs))
    print(f"{'':7} " + ' '.join(f"{'rewrites copies copied dropped':>34}" for _ in calcs))
    for name in ('mirror', 'add', 'leaves', 'rev', 'fib'):
        outs, cells = set(), []
        for calc, rules in calcs:
            mirror, add, leaves, rev, fib = build(calc)
            addf = A(add, add)
            call = {'mirror': A(mirror, mirror, tree(7)), 'add': A(addf, nat(30), nat(25)),
                    'leaves': A(leaves, leaves, addf, tree(7)), 'rev': A(rev, rev, lst(25), L),
                    'fib': A(fib, fib, addf, nat(11))}[name]
            v, st = make_run(rules)(call)
            outs.add(v)
            cells.append(f"{st['steps']:>8} {st['copies']:>6} {st['copied']:>8} {st['dropped']:>8}" + ' ' * 2)
        assert len(outs) == 1, name
        print(f'{name:7} ' + ' '.join(f'{c:>34}' for c in cells))
    print('same answers on all four')
threading.stack_size(512 * 1024 * 1024)
th = threading.Thread(target=main); th.start(); th.join()
